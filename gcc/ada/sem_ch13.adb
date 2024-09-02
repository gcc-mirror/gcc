------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Accessibility;    use Accessibility;
with Aspects;          use Aspects;
with Atree;            use Atree;
with Checks;           use Checks;
with Contracts;        use Contracts;
with Debug;            use Debug;
with Diagnostics.Constructors; use Diagnostics.Constructors;
with Einfo;            use Einfo;
with Einfo.Entities;   use Einfo.Entities;
with Einfo.Utils;      use Einfo.Utils;
with Elists;           use Elists;
with Errout;           use Errout;
with Exp_Ch3;          use Exp_Ch3;
with Exp_Disp;         use Exp_Disp;
with Exp_Tss;          use Exp_Tss;
with Exp_Util;         use Exp_Util;
with Expander;         use Expander;
with Freeze;           use Freeze;
with Ghost;            use Ghost;
with Lib;              use Lib;
with Lib.Xref;         use Lib.Xref;
with Mutably_Tagged;   use Mutably_Tagged;
with Namet;            use Namet;
with Nlists;           use Nlists;
with Nmake;            use Nmake;
with Opt;              use Opt;
with Par_SCO;          use Par_SCO;
with Restrict;         use Restrict;
with Rident;           use Rident;
with Rtsfind;          use Rtsfind;
with Sem;              use Sem;
with Sem_Aux;          use Sem_Aux;
with Sem_Case;         use Sem_Case;
with Sem_Cat;          use Sem_Cat;
with Sem_Ch3;          use Sem_Ch3;
with Sem_Ch6;          use Sem_Ch6;
with Sem_Ch7;          use Sem_Ch7;
with Sem_Ch8;          use Sem_Ch8;
with Sem_Dim;          use Sem_Dim;
with Sem_Eval;         use Sem_Eval;
with Sem_Prag;         use Sem_Prag;
with Sem_Res;          use Sem_Res;
with Sem_Type;         use Sem_Type;
with Sem_Util;         use Sem_Util;
with Sem_Warn;         use Sem_Warn;
with Sinfo;            use Sinfo;
with Sinfo.Nodes;      use Sinfo.Nodes;
with Sinfo.Utils;      use Sinfo.Utils;
with Sinput;           use Sinput;
with Snames;           use Snames;
with Stand;            use Stand;
with System.Case_Util; use System.Case_Util;
with Table;
with Targparm;         use Targparm;
with Ttypes;           use Ttypes;
with Tbuild;           use Tbuild;
with Urealp;           use Urealp;
with Warnsw;           use Warnsw;

with GNAT.Heap_Sort_G;

package body Sem_Ch13 is

   SSU : constant Pos := System_Storage_Unit;
   --  Convenient short hand for commonly used constant

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Adjust_Record_For_Reverse_Bit_Order_Ada_95 (R : Entity_Id);
   --  Helper routine providing the original (pre-AI95-0133) behavior for
   --  Adjust_Record_For_Reverse_Bit_Order.

   procedure Alignment_Check_For_Size_Change (Typ : Entity_Id; Size : Uint);
   --  This routine is called after setting one of the sizes of type entity
   --  Typ to Size. The purpose is to deal with the situation of a derived
   --  type whose inherited alignment is no longer appropriate for the new
   --  size value. In this case, we reset the Alignment to unknown.

   function All_Static_Choices (L : List_Id) return Boolean;
   --  Returns true if all elements of the list are OK static choices
   --  as defined below for Is_Static_Choice. Used for case expression
   --  alternatives and for the right operand of a membership test. An
   --  others_choice is static if the corresponding expression is static.
   --  The staticness of the bounds is checked separately.

   procedure Analyze_User_Aspect_Aspect_Specification (N : Node_Id);
   --  Analyze a User_Aspect aspect specification. Called from outside
   --  this package (in addition to locally), but the call from aspect.adb
   --  is via an access-to-subprogram value.

   procedure Build_Discrete_Static_Predicate
     (Typ  : Entity_Id;
      Expr : Node_Id;
      Nam  : Name_Id);
   --  Given a predicated type Typ, where Typ is a discrete static subtype,
   --  whose predicate expression is Expr, tests if Expr is a static predicate,
   --  and if so, builds the predicate range list. Nam is the name of the one
   --  argument to the predicate function. Occurrences of the type name in the
   --  predicate expression have been replaced by identifier references to this
   --  name, which is unique, so any identifier with Chars matching Nam must be
   --  a reference to the type. If the predicate is non-static, this procedure
   --  returns doing nothing. If the predicate is static, then the predicate
   --  list is stored in Static_Discrete_Predicate (Typ), and the Expr is
   --  rewritten as a canonicalized membership operation.

   function Build_Export_Import_Pragma
     (Asp : Node_Id;
      Id  : Entity_Id) return Node_Id;
   --  Create the corresponding pragma for aspect Export or Import denoted by
   --  Asp. Id is the related entity subject to the aspect. Return Empty when
   --  the expression of aspect Asp evaluates to False or is erroneous.

   function Build_Predicate_Function_Declaration
      (Typ : Entity_Id) return Node_Id;
   --  Build the declaration for a predicate function. The declaration is built
   --  at the same time as the body but inserted before, as explained below.

   procedure Build_Predicate_Function (Typ : Entity_Id; N : Node_Id);
   --  If Typ has predicates (indicated by Has_Predicates being set for Typ),
   --  then either there are pragma Predicate entries on the rep chain for the
   --  type (note that Predicate aspects are converted to pragma Predicate), or
   --  there are inherited aspects from a parent type, or ancestor subtypes.
   --  This procedure builds body for the Predicate function that tests these
   --  predicates. N is the freeze node for the type. The spec of the function
   --  is inserted before the freeze node, and the body of the function is
   --  inserted after the freeze node.

   procedure Check_Aspect_At_End_Of_Declarations (ASN : Node_Id);
   --  Performs the processing of an aspect at the freeze all point and issues
   --  appropriate error messages if the visibility has indeed changed. ASN is
   --  the N_Aspect_Specification node for the aspect.

   procedure Check_Aspect_At_Freeze_Point (ASN : Node_Id);
   --  Performs the processing of an aspect at the freeze point. ASN is the
   --  N_Aspect_Specification node for the aspect.

   procedure Check_Aspect_Too_Late (N : Node_Id);
   --  This procedure is similar to Rep_Item_Too_Late for representation
   --  aspects that apply to type and that do not have a corresponding pragma.
   --
   --  Used to check in particular that the expression associated with aspect
   --  node N for the given type (entity) of the aspect does not appear too
   --  late according to the rules in RM 13.1(9) and 13.1(10).

   procedure Check_Pool_Size_Clash (Ent : Entity_Id; SP, SS : Node_Id);
   --  Called if both Storage_Pool and Storage_Size attribute definition
   --  clauses (SP and SS) are present for entity Ent. Issue error message.

   procedure Freeze_Entity_Checks (N : Node_Id);
   --  Called from Analyze_Freeze_Entity and Analyze_Generic_Freeze Entity
   --  to generate appropriate semantic checks that are delayed until this
   --  point (they had to be delayed this long for cases of delayed aspects,
   --  e.g. analysis of statically predicated subtypes in choices, for which
   --  we have to be sure the subtypes in question are frozen before checking).

   function Get_Alignment_Value (Expr : Node_Id) return Uint;
   --  Given the expression for an alignment value, returns the corresponding
   --  Uint value. If the value is inappropriate, then error messages are
   --  posted as required, and a value of No_Uint is returned.

   function Is_Operational_Item (N : Node_Id) return Boolean;
   --  A specification for a stream attribute is allowed before the full type
   --  is declared, as explained in AI-00137 and the corrigendum. Attributes
   --  that do not specify a representation characteristic are operational
   --  attributes.

   function Is_Static_Choice (N : Node_Id) return Boolean;
   --  Returns True if N represents a static choice (static subtype, or
   --  static subtype indication, or static expression, or static range).
   --
   --  Note that this is a bit more inclusive than we actually need
   --  (in particular membership tests do not allow the use of subtype
   --  indications). But that doesn't matter, we have already checked
   --  that the construct is legal to get this far.

   function Is_Type_Related_Rep_Item (N : Node_Id) return Boolean;
   --  Returns True for a representation clause/pragma that specifies a
   --  type-related representation (as opposed to operational) aspect.

   function Is_Predicate_Static
     (Expr : Node_Id;
      Nam  : Name_Id;
      Warn : Boolean := True) return Boolean;
   --  Given predicate expression Expr, tests if Expr is predicate-static in
   --  the sense of the rules in (RM 3.2.4 (15-24)). Occurrences of the type
   --  name in the predicate expression have been replaced by references to
   --  an identifier whose Chars field is Nam. This name is unique, so any
   --  identifier with Chars matching Nam must be a reference to the type.
   --  Returns True if the expression is predicate-static and False otherwise,
   --  but is not in the business of setting flags or issuing error messages.
   --
   --  Only scalar types can have static predicates, so False is always
   --  returned for non-scalar types.
   --
   --  Note: the RM seems to suggest that string types can also have static
   --  predicates. But that really makes little sense as very few useful
   --  predicates can be constructed for strings. Remember that:
   --
   --     "ABC" < "DEF"
   --
   --  is not a static expression. So even though the clearly faulty RM wording
   --  allows the following:
   --
   --     subtype S is String with Static_Predicate => S < "DEF"
   --
   --  We can't allow this, otherwise we have predicate-static applying to a
   --  larger class than static expressions, which was never intended.
   --
   --  The Warn parameter is True iff this is not a recursive call. This
   --  parameter is used to avoid generating warnings for subexpressions and
   --  for cases where the predicate expression (as originally written by
   --  the user, before any transformations) is a Boolean literal.

   procedure New_Put_Image_Subprogram
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id);
   --  Similar to New_Stream_Subprogram, but for the Put_Image attribute

   procedure New_Stream_Subprogram
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : TSS_Name_Type);
   --  Create a subprogram renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as a
   --  primitive operation, or in the untagged case make an appropriate TSS
   --  entry. This is more properly an expansion activity than just semantics,
   --  but the presence of user-defined stream functions for limited types
   --  is a legality check, which is why this takes place here rather than in
   --  exp_ch13, where it was previously. Nam indicates the name of the TSS
   --  function to be generated.
   --
   --  To avoid elaboration anomalies with freeze nodes, for untagged types
   --  we generate both a subprogram declaration and a subprogram renaming
   --  declaration, so that the attribute specification is handled as a
   --  renaming_as_body. For tagged types, the specification is one of the
   --  primitive specs.

   procedure No_Type_Rep_Item (N : Node_Id);
   --  Output message indicating that no type-related aspects can be
   --  specified due to some property of the parent type.

   procedure Register_Address_Clause_Check
     (N   : Node_Id;
      X   : Entity_Id;
      A   : Uint;
      Y   : Entity_Id;
      Off : Boolean);
   --  Register a check for the address clause N. The rest of the parameters
   --  are in keeping with the components of Address_Clause_Check_Record below.

   procedure Validate_Aspect_Aggregate (N : Node_Id);
   --  Check legality of operations given in the Ada 2022 Aggregate aspect for
   --  containers.

   procedure Resolve_Aspect_Aggregate
     (Typ  : Entity_Id;
      Expr : Node_Id);
   --  Resolve each one of the operations specified in the specification of
   --  Aspect_Aggregate.

   procedure Validate_Aspect_Local_Restrictions (E : Entity_Id; N : Node_Id);
   --  Check legality of a Local_Restrictions aspect specification

   procedure Validate_Aspect_Stable_Properties
     (E : Entity_Id; N : Node_Id; Class_Present : Boolean);
   --  Check legality of functions given in the Ada 2022 Stable_Properties
   --  (or Stable_Properties'Class) aspect.

   procedure Validate_Storage_Model_Type_Aspect
     (Typ : Entity_Id; ASN : Node_Id);
   --  Check legality and completeness of the aggregate associations given in
   --  the Storage_Model_Type aspect associated with Typ.

   procedure Validate_Finalizable_Aspect (Typ : Entity_Id; ASN : Node_Id);
   --  Check legality and completeness of the aggregate associations given in
   --  the Finalizable aspect associated with Typ.

   procedure Resolve_Storage_Model_Type_Argument
     (N         : Node_Id;
      Typ       : Entity_Id;
      Addr_Type : in out Entity_Id;
      Nam       : Name_Id);
   --  Resolve argument N to be of the proper kind (when a type or constant)
   --  or to have the proper profile (when a subprogram).

   procedure Resolve_Aspect_Stable_Properties
    (Typ_Or_Subp   : Entity_Id;
     Expr          : Node_Id;
     Class_Present : Boolean);
   --  Resolve each one of the functions specified in the specification of
   --  aspect Stable_Properties (or Stable_Properties'Class).

   procedure Resolve_Finalizable_Argument
     (N   : Node_Id;
      Typ : Entity_Id;
      Nam : Name_Id);
   --  Resolve each one of the arguments specified in the specification of
   --  aspect Finalizable.

   procedure Resolve_Iterable_Operation
     (N      : Node_Id;
      Cursor : Entity_Id;
      Typ    : Entity_Id;
      Nam    : Name_Id);
   --  If the name of a primitive operation for an Iterable aspect is
   --  overloaded, resolve according to required signature.

   procedure Set_Biased
     (E      : Entity_Id;
      N      : Node_Id;
      Msg    : String;
      Biased : Boolean := True);
   --  If Biased is True, sets Has_Biased_Representation flag for E, and
   --  outputs a warning message at node N if Warn_On_Biased_Representation is
   --  is True. This warning inserts the string Msg to describe the construct
   --  causing biasing.

   -----------------------------------------------------------
   --  Visibility of Discriminants in Aspect Specifications --
   -----------------------------------------------------------

   --  The discriminants of a type are visible when analyzing the aspect
   --  specifications of a type declaration or protected type declaration,
   --  but not when analyzing those of a subtype declaration. The following
   --  routines enforce this distinction.

   procedure Push_Type (E : Entity_Id);
   --  Push scope E and make visible the discriminants of type entity E if E
   --  has discriminants and is not a subtype.

   procedure Pop_Type (E : Entity_Id);
   --  Remove visibility to the discriminants of type entity E and pop the
   --  scope stack if E has discriminants and is not a subtype.

   ----------------------------------------------
   -- Table for Validate_Unchecked_Conversions --
   ----------------------------------------------

   --  The following table collects unchecked conversions for validation.
   --  Entries are made by Validate_Unchecked_Conversion and then the call
   --  to Validate_Unchecked_Conversions does the actual error checking and
   --  posting of warnings. The reason for this delayed processing is to take
   --  advantage of back-annotations of size and alignment values performed by
   --  the back end.

   --  Note: the reason we store a Source_Ptr value instead of a Node_Id is
   --  that by the time Validate_Unchecked_Conversions is called, Sprint will
   --  already have modified all Sloc values if the -gnatD option is set.

   type UC_Entry is record
      Eloc     : Source_Ptr; -- node used for posting warnings
      Source   : Entity_Id;  -- source type for unchecked conversion
      Target   : Entity_Id;  -- target type for unchecked conversion
      Act_Unit : Entity_Id;  -- actual function instantiated
   end record;

   package Unchecked_Conversions is new Table.Table (
     Table_Component_Type => UC_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 200,
     Table_Name           => "Unchecked_Conversions");

   ----------------------------------------
   -- Table for Validate_Address_Clauses --
   ----------------------------------------

   --  If an address clause has the form

   --    for X'Address use Expr

   --  where Expr has a value known at compile time or is of the form Y'Address
   --  or recursively is a reference to a constant initialized with either of
   --  these forms, and the value of Expr is not a multiple of X's alignment,
   --  or if Y has a smaller alignment than X, then that merits a warning about
   --  possible bad alignment. The following table collects address clauses of
   --  this kind. We put these in a table so that they can be checked after the
   --  back end has completed annotation of the alignments of objects, since we
   --  can catch more cases that way.

   type Address_Clause_Check_Record is record
      N : Node_Id;
      --  The address clause

      X : Entity_Id;
      --  The entity of the object subject to the address clause

      A : Uint;
      --  The value of the address in the first case

      Y : Entity_Id;
      --  The entity of the object being overlaid in the second case

      Off : Boolean;
      --  Whether the address is offset within Y in the second case

      Alignment_Checks_Suppressed : Boolean;
      --  Whether alignment checks are suppressed by an active scope suppress
      --  setting. We need to save the value in order to be able to reuse it
      --  after the back end has been run.
   end record;

   package Address_Clause_Checks is new Table.Table (
     Table_Component_Type => Address_Clause_Check_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "Address_Clause_Checks");

   function Alignment_Checks_Suppressed
     (ACCR : Address_Clause_Check_Record) return Boolean;
   --  Return whether the alignment check generated for the address clause
   --  is suppressed.

   ---------------------------------
   -- Alignment_Checks_Suppressed --
   ---------------------------------

   function Alignment_Checks_Suppressed
     (ACCR : Address_Clause_Check_Record) return Boolean
   is
   begin
      if Checks_May_Be_Suppressed (ACCR.X) then
         return Is_Check_Suppressed (ACCR.X, Alignment_Check);
      else
         return ACCR.Alignment_Checks_Suppressed;
      end if;
   end Alignment_Checks_Suppressed;

   -----------------------------------------
   -- Adjust_Record_For_Reverse_Bit_Order --
   -----------------------------------------

   procedure Adjust_Record_For_Reverse_Bit_Order (R : Entity_Id) is
      Max_Machine_Scalar_Size : constant Uint :=
        UI_From_Int (if Reverse_Bit_Order_Threshold >= 0
                     then Reverse_Bit_Order_Threshold
                     else System_Max_Integer_Size);
      --  We use this as the maximum machine scalar size

      SSU : constant Uint := UI_From_Int (System_Storage_Unit);

      CC     : Node_Id;
      Comp   : Node_Id;
      Num_CC : Natural;

   begin
      --  The processing done here used to depend on the Ada version, but the
      --  behavior has been changed by AI95-0133. However this AI is a Binding
      --  Interpretation, so we now implement it even in Ada 95 mode. But the
      --  original behavior from unamended Ada 95 is available for the sake of
      --  compatibility under the debugging switch -gnatd.p in Ada 95 mode.

      if Ada_Version < Ada_2005 and then Debug_Flag_Dot_P then
         Adjust_Record_For_Reverse_Bit_Order_Ada_95 (R);
         return;
      end if;

      --  For Ada 2005, we do machine scalar processing, as fully described In
      --  AI-133. This involves gathering all components which start at the
      --  same byte offset and processing them together. Same approach is still
      --  valid in later versions including Ada 2012.

      --  Note that component clauses found on record types may be inherited,
      --  in which case the layout of the component with such a clause still
      --  has to be done at this point. Therefore, the processing done here
      --  must exclusively rely on the Component_Clause of the component.

      --  This first loop through components does two things. First it deals
      --  with the case of components with component clauses whose length is
      --  greater than the maximum machine scalar size (either accepting them
      --  or rejecting as needed). Second, it counts the number of components
      --  with component clauses whose length does not exceed this maximum for
      --  later processing.

      Num_CC := 0;
      Comp   := First_Component_Or_Discriminant (R);
      while Present (Comp) loop
         CC := Component_Clause (Comp);

         if Present (CC) then
            declare
               Fbit : constant Uint := Static_Integer (First_Bit (CC));
               Lbit : constant Uint := Static_Integer (Last_Bit (CC));

            begin
               --  Case of component with last bit >= max machine scalar

               if Lbit >= Max_Machine_Scalar_Size then

                  --  This is allowed only if first bit is zero, and last bit
                  --  + 1 is a multiple of storage unit size.

                  if Fbit = 0 and then (Lbit + 1) mod SSU = 0 then

                     --  This is the case to give a warning if enabled

                     if Warn_On_Reverse_Bit_Order then
                        Error_Msg_N
                          ("multi-byte field specified with "
                           & "non-standard Bit_Order?.v?", CC);

                        if Bytes_Big_Endian then
                           Error_Msg_N
                             ("\bytes are not reversed "
                              & "(component is big-endian)?.v?", CC);
                        else
                           Error_Msg_N
                             ("\bytes are not reversed "
                              & "(component is little-endian)?.v?", CC);
                        end if;
                     end if;

                  --  Give error message for RM 13.5.1(10) violation

                  else
                     Error_Msg_FE
                       ("machine scalar rules not followed for&",
                        First_Bit (CC), Comp);

                     Error_Msg_Uint_1 := Lbit + 1;
                     Error_Msg_Uint_2 := Max_Machine_Scalar_Size;
                     Error_Msg_F
                       ("\last bit + 1 (^) exceeds maximum machine scalar "
                        & "size (^)", First_Bit (CC));

                     if (Lbit + 1) mod SSU /= 0 then
                        Error_Msg_Uint_1 := SSU;
                        Error_Msg_F
                          ("\and is not a multiple of Storage_Unit (^) "
                           & "(RM 13.5.1(10))", First_Bit (CC));

                     else
                        Error_Msg_Uint_1 := Fbit;
                        Error_Msg_F
                          ("\and first bit (^) is non-zero "
                           & "(RM 13.4.1(10))", First_Bit (CC));
                     end if;
                  end if;

               --  OK case of machine scalar related component clause. For now,
               --  just count them.

               else
                  Num_CC := Num_CC + 1;
               end if;
            end;
         end if;

         Next_Component_Or_Discriminant (Comp);
      end loop;

      --  We need to sort the component clauses on the basis of the Position
      --  values in the clause, so we can group clauses with the same Position
      --  together to determine the relevant machine scalar size.

      Sort_CC : declare
         Comps : array (0 .. Num_CC) of Entity_Id;
         --  Array to collect component and discriminant entities. The data
         --  starts at index 1, the 0'th entry is for the sort routine.

         function CP_Lt (Op1, Op2 : Natural) return Boolean;
         --  Compare routine for Sort

         procedure CP_Move (From : Natural; To : Natural);
         --  Move routine for Sort

         package Sorting is new GNAT.Heap_Sort_G (CP_Move, CP_Lt);

         MaxL : Uint;
         --  Maximum last bit value of any component in this set

         MSS : Uint;
         --  Corresponding machine scalar size

         Start : Natural;
         Stop  : Natural;
         --  Start and stop positions in the component list of the set of
         --  components with the same starting position (that constitute
         --  components in a single machine scalar).

         -----------
         -- CP_Lt --
         -----------

         function CP_Lt (Op1, Op2 : Natural) return Boolean is
         begin
            return
              Position (Component_Clause (Comps (Op1))) <
              Position (Component_Clause (Comps (Op2)));
         end CP_Lt;

         -------------
         -- CP_Move --
         -------------

         procedure CP_Move (From : Natural; To : Natural) is
         begin
            Comps (To) := Comps (From);
         end CP_Move;

      --  Start of processing for Sort_CC

      begin
         --  Collect the machine scalar relevant component clauses

         Num_CC := 0;
         Comp   := First_Component_Or_Discriminant (R);
         while Present (Comp) loop
            declare
               CC : constant Node_Id := Component_Clause (Comp);

            begin
               --  Collect only component clauses whose last bit is less than
               --  machine scalar size. Any component clause whose last bit
               --  exceeds this value does not take part in machine scalar
               --  layout considerations. The test for Error_Posted makes sure
               --  we exclude component clauses for which we already posted an
               --  error.

               if Present (CC)
                 and then not Error_Posted (Last_Bit (CC))
                 and then Static_Integer (Last_Bit (CC)) <
                                              Max_Machine_Scalar_Size
               then
                  Num_CC := Num_CC + 1;
                  Comps (Num_CC) := Comp;
               end if;
            end;

            Next_Component_Or_Discriminant (Comp);
         end loop;

         --  Sort by ascending position number

         Sorting.Sort (Num_CC);

         --  We now have all the components whose size does not exceed the max
         --  machine scalar value, sorted by starting position. In this loop we
         --  gather groups of clauses starting at the same position, to process
         --  them in accordance with AI-133.

         Stop := 0;
         while Stop < Num_CC loop
            Start := Stop + 1;
            Stop  := Start;
            MaxL  :=
              Static_Integer
                (Last_Bit (Component_Clause (Comps (Start))));
            while Stop < Num_CC loop
               if Static_Integer
                    (Position (Component_Clause (Comps (Stop + 1)))) =
                  Static_Integer
                    (Position (Component_Clause (Comps (Stop))))
               then
                  Stop := Stop + 1;
                  MaxL :=
                    UI_Max
                      (MaxL,
                       Static_Integer
                         (Last_Bit
                            (Component_Clause (Comps (Stop)))));
               else
                  exit;
               end if;
            end loop;

            --  Now we have a group of component clauses from Start to Stop
            --  whose positions are identical, and MaxL is the maximum last
            --  bit value of any of these components.

            --  We need to determine the corresponding machine scalar size.
            --  This loop assumes that machine scalar sizes are even, and that
            --  each possible machine scalar has twice as many bits as the next
            --  smaller one.

            MSS := Max_Machine_Scalar_Size;
            while MSS mod 2 = 0
              and then (MSS / 2) >= SSU
              and then (MSS / 2) > MaxL
            loop
               MSS := MSS / 2;
            end loop;

            --  Here is where we fix up the Component_Bit_Offset value to
            --  account for the reverse bit order. Some examples of what needs
            --  to be done for the case of a machine scalar size of 8 are:

            --    First_Bit .. Last_Bit     Component_Bit_Offset
            --      old          new          old       new

            --     0 .. 0       7 .. 7         0         7
            --     0 .. 1       6 .. 7         0         6
            --     0 .. 2       5 .. 7         0         5
            --     0 .. 7       0 .. 7         0         4

            --     1 .. 1       6 .. 6         1         6
            --     1 .. 4       3 .. 6         1         3
            --     4 .. 7       0 .. 3         4         0

            --  The rule is that the first bit is obtained by subtracting the
            --  old ending bit from machine scalar size - 1.

            for C in Start .. Stop loop
               declare
                  Comp : constant Entity_Id := Comps (C);
                  CC   : constant Node_Id   := Component_Clause (Comp);

                  FB   : constant Uint := Static_Integer (First_Bit (CC));
                  LB   : constant Uint := Static_Integer (Last_Bit (CC));
                  NFB  : constant Uint := MSS - 1 - LB;
                  NLB  : constant Uint := NFB + LB - FB;
                  Pos  : constant Uint := Static_Integer (Position (CC));

               begin
                  --  Do not warn for the artificial clause built for the tag
                  --  in Check_Record_Representation_Clause if it is inherited.

                  if Warn_On_Reverse_Bit_Order
                    and then Chars (Comp) /= Name_uTag
                  then
                     Error_Msg_Uint_1 := MSS;
                     Error_Msg_N
                       ("reverse bit order in machine scalar of "
                        & "length^?.v?", First_Bit (CC));
                     Error_Msg_Uint_1 := NFB;
                     Error_Msg_Uint_2 := NLB;

                     if Bytes_Big_Endian then
                        Error_Msg_NE
                          ("\big-endian range for component & is ^ .. ^?.v?",
                           First_Bit (CC), Comp);
                     else
                        Error_Msg_NE
                          ("\little-endian range for component " &
                           "& is ^ .. ^?.v?",
                           First_Bit (CC), Comp);
                     end if;
                  end if;

                  Set_Component_Bit_Offset (Comp, Pos * SSU + NFB);
                  Set_Esize                (Comp, 1 + (NLB - NFB));
                  Set_Normalized_First_Bit (Comp, NFB mod SSU);
                  Set_Normalized_Position  (Comp, Pos + NFB / SSU);
               end;
            end loop;
         end loop;
      end Sort_CC;
   end Adjust_Record_For_Reverse_Bit_Order;

   ------------------------------------------------
   -- Adjust_Record_For_Reverse_Bit_Order_Ada_95 --
   ------------------------------------------------

   procedure Adjust_Record_For_Reverse_Bit_Order_Ada_95 (R : Entity_Id) is
      CC   : Node_Id;
      Comp : Node_Id;

   begin
      --  For Ada 95, we just renumber bits within a storage unit. We do the
      --  same for Ada 83 mode, since we recognize the Bit_Order attribute in
      --  Ada 83, and are free to add this extension.

      Comp := First_Component_Or_Discriminant (R);
      while Present (Comp) loop
         CC := Component_Clause (Comp);

         --  If component clause is present, then deal with the non-default
         --  bit order case for Ada 95 mode.

         --  We only do this processing for the base type, and in fact that
         --  is important, since otherwise if there are record subtypes, we
         --  could reverse the bits once for each subtype, which is wrong.

         if Present (CC) and then Ekind (R) = E_Record_Type then
            declare
               CFB : constant Uint    := Component_Bit_Offset (Comp);
               CSZ : constant Uint    := Esize (Comp);
               CLC : constant Node_Id := Component_Clause (Comp);
               Pos : constant Node_Id := Position (CLC);
               FB  : constant Node_Id := First_Bit (CLC);

               Storage_Unit_Offset : constant Uint :=
                                       CFB / System_Storage_Unit;

               Start_Bit : constant Uint :=
                             CFB mod System_Storage_Unit;

            begin
               --  Cases where field goes over storage unit boundary

               if Start_Bit + CSZ > System_Storage_Unit then

                  --  Allow multi-byte field but generate warning

                  if Start_Bit mod System_Storage_Unit = 0
                    and then CSZ mod System_Storage_Unit = 0
                  then
                     Error_Msg_N
                       ("multi-byte field specified with non-standard "
                        & "Bit_Order?.v?", CLC);

                     if Bytes_Big_Endian then
                        Error_Msg_N
                          ("\bytes are not reversed "
                           & "(component is big-endian)?.v?", CLC);
                     else
                        Error_Msg_N
                          ("\bytes are not reversed "
                           & "(component is little-endian)?.v?", CLC);
                     end if;

                  --  Do not allow non-contiguous field

                  else
                     Error_Msg_N
                       ("attempt to specify non-contiguous field not "
                        & "permitted", CLC);
                     Error_Msg_N
                       ("\caused by non-standard Bit_Order specified in "
                        & "legacy Ada 95 mode", CLC);
                  end if;

               --  Case where field fits in one storage unit

               else
                  --  Give warning if suspicious component clause

                  if Intval (FB) >= System_Storage_Unit
                    and then Warn_On_Reverse_Bit_Order
                  then
                     Error_Msg_N
                       ("Bit_Order clause does not affect byte "
                        & "ordering?.v?", Pos);
                     Error_Msg_Uint_1 :=
                       Intval (Pos) + Intval (FB) /
                       System_Storage_Unit;
                     Error_Msg_N
                       ("position normalized to ^ before bit order "
                        & "interpreted?.v?", Pos);
                  end if;

                  --  Here is where we fix up the Component_Bit_Offset value
                  --  to account for the reverse bit order. Some examples of
                  --  what needs to be done are:

                  --    First_Bit .. Last_Bit     Component_Bit_Offset
                  --      old          new          old       new

                  --     0 .. 0       7 .. 7         0         7
                  --     0 .. 1       6 .. 7         0         6
                  --     0 .. 2       5 .. 7         0         5
                  --     0 .. 7       0 .. 7         0         4

                  --     1 .. 1       6 .. 6         1         6
                  --     1 .. 4       3 .. 6         1         3
                  --     4 .. 7       0 .. 3         4         0

                  --  The rule is that the first bit is obtained by subtracting
                  --  the old ending bit from storage_unit - 1.

                  Set_Component_Bit_Offset (Comp,
                    (Storage_Unit_Offset * System_Storage_Unit) +
                      (System_Storage_Unit - 1) -
                      (Start_Bit + CSZ - 1));

                  Set_Normalized_Position (Comp,
                    Component_Bit_Offset (Comp) / System_Storage_Unit);

                  Set_Normalized_First_Bit (Comp,
                    Component_Bit_Offset (Comp) mod System_Storage_Unit);
               end if;
            end;
         end if;

         Next_Component_Or_Discriminant (Comp);
      end loop;
   end Adjust_Record_For_Reverse_Bit_Order_Ada_95;

   -------------------------------------
   -- Alignment_Check_For_Size_Change --
   -------------------------------------

   procedure Alignment_Check_For_Size_Change (Typ : Entity_Id; Size : Uint) is
   begin
      --  If the alignment is known, and not set by a rep clause, and is
      --  inconsistent with the size being set, then reset it to unknown,
      --  we assume in this case that the size overrides the inherited
      --  alignment, and that the alignment must be recomputed.

      if Known_Alignment (Typ)
        and then not Has_Alignment_Clause (Typ)
        and then Present (Size)
        and then Size mod (Alignment (Typ) * SSU) /= 0
      then
         Reinit_Alignment (Typ);
      end if;
   end Alignment_Check_For_Size_Change;

   -----------------------------------
   -- All_Membership_Choices_Static --
   -----------------------------------

   function All_Membership_Choices_Static (Expr : Node_Id) return Boolean is
      pragma Assert (Nkind (Expr) in N_Membership_Test);
   begin
      pragma Assert
        (Present (Right_Opnd (Expr))
           xor
         Present (Alternatives (Expr)));

      if Present (Right_Opnd (Expr)) then
         return Is_Static_Choice (Right_Opnd (Expr));
      else
         return All_Static_Choices (Alternatives (Expr));
      end if;
   end All_Membership_Choices_Static;

   ------------------------
   -- All_Static_Choices --
   ------------------------

   function All_Static_Choices (L : List_Id) return Boolean is
      N : Node_Id;

   begin
      N := First (L);
      while Present (N) loop
         if not Is_Static_Choice (N) then
            return False;
         end if;

         Next (N);
      end loop;

      return True;
   end All_Static_Choices;

   -------------------------------------
   -- Analyze_Aspects_At_Freeze_Point --
   -------------------------------------

   procedure Analyze_Aspects_At_Freeze_Point (E : Entity_Id) is
      procedure Analyze_Aspect_Default_Value (ASN : Node_Id);
      --  This routine analyzes an Aspect_Default_[Component_]Value denoted by
      --  the aspect specification node ASN.

      procedure Make_Pragma_From_Boolean_Aspect (ASN : Node_Id);
      --  Given an aspect specification node ASN whose expression is an
      --  optional Boolean, this routines creates the corresponding pragma
      --  at the freezing point.

      ----------------------------------
      -- Analyze_Aspect_Default_Value --
      ----------------------------------

      procedure Analyze_Aspect_Default_Value (ASN : Node_Id) is
         Ent  : constant Entity_Id := Entity (ASN);
         Expr : constant Node_Id   := Expression (ASN);

      begin
         Set_Has_Default_Aspect (Base_Type (Ent));

         if Is_Scalar_Type (Ent) then
            Set_Default_Aspect_Value (Base_Type (Ent), Expr);
         else
            Set_Default_Aspect_Component_Value (Base_Type (Ent), Expr);
         end if;

         Check_Aspect_Too_Late (ASN);
      end Analyze_Aspect_Default_Value;

      -------------------------------------
      -- Make_Pragma_From_Boolean_Aspect --
      -------------------------------------

      procedure Make_Pragma_From_Boolean_Aspect (ASN : Node_Id) is
         Ident  : constant Node_Id    := Identifier (ASN);
         A_Name : constant Name_Id    := Chars (Ident);
         A_Id   : constant Aspect_Id  := Get_Aspect_Id (A_Name);
         Ent    : constant Entity_Id  := Entity (ASN);
         Expr   : constant Node_Id    := Expression (ASN);
         Loc    : constant Source_Ptr := Sloc (ASN);

         procedure Check_False_Aspect_For_Derived_Type;
         --  This procedure checks for the case of a false aspect for a derived
         --  type, which improperly tries to cancel an aspect inherited from
         --  the parent.

         -----------------------------------------
         -- Check_False_Aspect_For_Derived_Type --
         -----------------------------------------

         procedure Check_False_Aspect_For_Derived_Type is
            Par : Node_Id;

         begin
            --  We are only checking derived types

            if not Is_Derived_Type (E) then
               return;
            end if;

            Par := Nearest_Ancestor (E);

            case A_Id is
               when Aspect_Atomic
                  | Aspect_Shared
               =>
                  if not Is_Atomic (Par) then
                     return;
                  end if;

               when Aspect_Atomic_Components =>
                  if not Has_Atomic_Components (Par) then
                     return;
                  end if;

               when Aspect_Discard_Names =>
                  if not Discard_Names (Par) then
                     return;
                  end if;

               when Aspect_Pack =>
                  if not Is_Packed (Par) then
                     return;
                  end if;

               when Aspect_Unchecked_Union =>
                  if not Is_Unchecked_Union (Par) then
                     return;
                  end if;

               when Aspect_Volatile =>
                  if not Is_Volatile (Par) then
                     return;
                  end if;

               when Aspect_Volatile_Components =>
                  if not Has_Volatile_Components (Par) then
                     return;
                  end if;

               when Aspect_Volatile_Full_Access
                  | Aspect_Full_Access_Only
               =>
                  if not Is_Volatile_Full_Access (Par) then
                     return;
                  end if;

               when others =>
                  return;
            end case;

            --  Fall through means we are canceling an inherited aspect

            Error_Msg_Name_1 := A_Name;
            Error_Msg_NE
              ("derived type& inherits aspect%, cannot cancel", Expr, E);
         end Check_False_Aspect_For_Derived_Type;

         --  Local variables

         Prag   : Node_Id;
         P_Name : Name_Id;

      --  Start of processing for Make_Pragma_From_Boolean_Aspect

      begin
         if Present (Expr) and then Is_False (Static_Boolean (Expr)) then
            Check_False_Aspect_For_Derived_Type;

         else
            --  There is no Full_Access_Only pragma so use VFA instead

            if A_Name = Name_Full_Access_Only then
               P_Name := Name_Volatile_Full_Access;
            else
               P_Name := A_Name;
            end if;

            Prag :=
              Make_Pragma (Loc,
                Pragma_Identifier            =>
                  Make_Identifier (Sloc (Ident), P_Name),
                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Sloc (Ident),
                    Expression => New_Occurrence_Of (Ent, Sloc (Ident)))));

            Set_From_Aspect_Specification (Prag, True);
            Set_Corresponding_Aspect (Prag, ASN);
            Set_Aspect_Rep_Item (ASN, Prag);
            Set_Is_Delayed_Aspect (Prag);
            Set_Parent (Prag, ASN);
         end if;
      end Make_Pragma_From_Boolean_Aspect;

      --  Local variables

      A_Id  : Aspect_Id;
      ASN   : Node_Id;
      Ritem : Node_Id;

   --  Start of processing for Analyze_Aspects_At_Freeze_Point

   begin
      --  Must be visible in current scope, but if this is a type from a nested
      --  package it may be frozen from an object declaration in the enclosing
      --  scope, so install the package declarations to complete the analysis
      --  of the aspects, if any. If the package itself is frozen the type will
      --  have been frozen as well.

      if not Scope_Within_Or_Same (Current_Scope, Scope (E)) then
         if Is_Type (E) and then From_Nested_Package (E) then
            declare
               Pack : constant Entity_Id := Scope (E);

            begin
               Push_Scope (Pack);
               Install_Visible_Declarations (Pack);
               Install_Private_Declarations (Pack);
               Analyze_Aspects_At_Freeze_Point (E);

               if Is_Private_Type (E)
                 and then Present (Full_View (E))
               then
                  Analyze_Aspects_At_Freeze_Point (Full_View (E));
               end if;

               End_Package_Scope (Pack);
               return;
            end;

         --  Aspects from other entities in different contexts are analyzed
         --  elsewhere.

         else
            return;
         end if;
      end if;

      --  Look for aspect specification entries for this entity

      ASN := First_Rep_Item (E);
      while Present (ASN) loop
         if Nkind (ASN) = N_Aspect_Specification then
            exit when Entity (ASN) /= E;

            if Is_Delayed_Aspect (ASN) then
               A_Id := Get_Aspect_Id (ASN);

               case A_Id is

                  --  For aspects whose expression is an optional Boolean, make
                  --  the corresponding pragma at the freeze point.

                  when Boolean_Aspects
                     | Library_Unit_Aspects
                  =>
                     --  Aspects Export and Import require special handling.
                     --  Both are by definition Boolean and may benefit from
                     --  forward references, however their expressions are
                     --  treated as static. In addition, the syntax of their
                     --  corresponding pragmas requires extra "pieces" which
                     --  may also contain forward references. To account for
                     --  all of this, the corresponding pragma is created by
                     --  Analyze_Aspect_Export_Import, but is not analyzed as
                     --  the complete analysis must happen now.

                     --  Aspect Full_Access_Only must be analyzed last so that
                     --  aspects Volatile and Atomic, if any, are analyzed.

                     --  Skip creation of pragma Preelaborable_Initialization
                     --  in the case where the aspect has an expression,
                     --  because the pragma is only needed for setting flag
                     --  Known_To_Have_Preelab_Init, which is set by other
                     --  means following resolution of the aspect expression.

                     if A_Id not in Aspect_Export
                                  | Aspect_Full_Access_Only
                                  | Aspect_Import
                       and then (A_Id /= Aspect_Preelaborable_Initialization
                                  or else No (Expression (ASN)))
                     then
                        Make_Pragma_From_Boolean_Aspect (ASN);
                     end if;

                  --  Special handling for aspects that don't correspond to
                  --  pragmas/attributes.

                  when Aspect_Default_Value
                     | Aspect_Default_Component_Value
                  =>
                     --  Do not inherit aspect for anonymous base type of a
                     --  scalar or array type, because they apply to the first
                     --  subtype of the type, and will be processed when that
                     --  first subtype is frozen.

                     if Is_Derived_Type (E)
                       and then not Comes_From_Source (E)
                       and then E /= First_Subtype (E)
                     then
                        null;
                     else
                        Analyze_Aspect_Default_Value (ASN);
                     end if;

                  --  Ditto for iterator aspects, because the corresponding
                  --  attributes may not have been analyzed yet.

                  when Aspect_Constant_Indexing
                     | Aspect_Default_Iterator
                     | Aspect_Iterator_Element
                     | Aspect_Variable_Indexing
                  =>
                     Analyze (Expression (ASN));

                     if Etype (Expression (ASN)) = Any_Type then
                        Error_Msg_NE
                          ("aspect must be fully defined before & is frozen",
                           ASN, E);
                     end if;

                  when Aspect_Integer_Literal
                     | Aspect_Real_Literal
                     | Aspect_String_Literal
                  =>
                     Validate_Literal_Aspect (E, ASN);

                  when Aspect_Iterable =>
                     Validate_Iterable_Aspect (E, ASN);

                  when Aspect_Designated_Storage_Model =>
                     Analyze_And_Resolve (Expression (ASN));

                     if not Is_Entity_Name (Expression (ASN))
                       or else not Is_Object (Entity (Expression (ASN)))
                       or else
                         No (Find_Aspect (Etype (Expression (ASN)),
                                                   Aspect_Storage_Model_Type))
                     then
                        Error_Msg_N
                          ("must specify name of stand-alone object of type "
                            & "with aspect Storage_Model_Type",
                           Expression (ASN));

                     --  Set access type's Associated_Storage_Pool to denote
                     --  the Storage_Model_Type object given for the aspect
                     --  (even though that isn't actually an Ada storage pool).

                     else
                        Set_Associated_Storage_Pool
                          (E, Entity (Expression (ASN)));
                     end if;

                  when Aspect_Storage_Model_Type =>
                     Validate_Storage_Model_Type_Aspect (E, ASN);

                  when Aspect_Aggregate =>
                     if Is_Array_Type (E) then
                        Error_Msg_N
                          ("aspect Aggregate may not be applied to array type",
                           ASN);
                     end if;

                  when Aspect_Finalizable =>
                     Validate_Finalizable_Aspect (E, ASN);

                  when others =>
                     null;
               end case;

               Ritem := Aspect_Rep_Item (ASN);

               if Present (Ritem) then
                  Analyze (Ritem);
               end if;
            end if;
         end if;

         Next_Rep_Item (ASN);
      end loop;

      --  Make a second pass for a Full_Access_Only entry, see above why

      ASN := First_Rep_Item (E);
      while Present (ASN) loop
         if Nkind (ASN) = N_Aspect_Specification then
            exit when Entity (ASN) /= E;

            if Get_Aspect_Id (ASN) = Aspect_Full_Access_Only then
               Make_Pragma_From_Boolean_Aspect (ASN);
               Ritem := Aspect_Rep_Item (ASN);
               if Present (Ritem) then
                  Analyze (Ritem);
               end if;
            end if;
         end if;

         Next_Rep_Item (ASN);
      end loop;

      if In_Instance
        and then E /= Base_Type (E)
        and then Is_First_Subtype (E)
      then
         Inherit_Rep_Item_Chain (Base_Type (E), E);
      end if;
   end Analyze_Aspects_At_Freeze_Point;

   -----------------------------------
   -- Analyze_Aspect_Specifications --
   -----------------------------------

   procedure Analyze_Aspect_Specifications (N : Node_Id; E : Entity_Id) is
      pragma Assert (Present (E));

      procedure Decorate (Asp : Node_Id; Prag : Node_Id);
      --  Establish linkages between an aspect and its corresponding pragma

      procedure Insert_Pragma
        (Prag        : Node_Id;
         Is_Instance : Boolean := False);
      --  Subsidiary to the analysis of aspects
      --    Abstract_State
      --    Always_Terminates
      --    Attach_Handler
      --    Async_Readers
      --    Async_Writers
      --    Constant_After_Elaboration
      --    Contract_Cases
      --    Convention
      --    Default_Initial_Condition
      --    Default_Storage_Pool
      --    Depends
      --    Effective_Reads
      --    Effective_Writes
      --    Exceptional_Cases
      --    Extensions_Visible
      --    Ghost
      --    Global
      --    Initial_Condition
      --    Initializes
      --    Max_Entry_Queue_Length
      --    Max_Queue_Length
      --    No_Caching
      --    Part_Of
      --    Post
      --    Pre
      --    Refined_Depends
      --    Refined_Global
      --    Refined_Post
      --    Refined_State
      --    Side_Effects
      --    SPARK_Mode
      --    Secondary_Stack_Size
      --    Subprogram_Variant
      --    Volatile_Function
      --    Warnings
      --  Insert pragma Prag such that it mimics the placement of a source
      --  pragma of the same kind. Flag Is_Generic should be set when the
      --  context denotes a generic instance.

      function Relocate_Expression (Source : Node_Id) return Node_Id;
      --  Outside of a generic this function is equivalent to Relocate_Node.
      --  Inside a generic it is an identity function, because Relocate_Node
      --  would create a new node that is not associated with the generic
      --  template. This association is needed to save references to entities
      --  that are global to the generic (and might be not visible from where
      --  the generic is instantiated).
      --
      --  Inside a generic the original tree is shared between aspect and
      --  a corresponding pragma (or an attribute definition clause). This
      --  parallels what is done in sem_prag.adb (see Get_Argument).

      --------------
      -- Decorate --
      --------------

      procedure Decorate (Asp : Node_Id; Prag : Node_Id) is
      begin
         Set_Aspect_Rep_Item           (Asp, Prag);
         Set_Corresponding_Aspect      (Prag, Asp);
         Set_From_Aspect_Specification (Prag);
         Set_Parent                    (Prag, Asp);
      end Decorate;

      -------------------
      -- Insert_Pragma --
      -------------------

      procedure Insert_Pragma
        (Prag        : Node_Id;
         Is_Instance : Boolean := False)
      is
         Aux      : Node_Id;
         Decl     : Node_Id;
         Decls    : List_Id;
         Def      : Node_Id;
         Inserted : Boolean := False;

      begin
         --  When the aspect appears on an entry, package, protected unit,
         --  subprogram, or task unit body, insert the generated pragma at the
         --  top of the body declarations to emulate the behavior of a source
         --  pragma.

         --    package body Pack with Aspect is

         --    package body Pack is
         --       pragma Prag;

         if Nkind (N) in N_Entry_Body
                       | N_Package_Body
                       | N_Protected_Body
                       | N_Subprogram_Body
                       | N_Task_Body
         then
            Decls := Declarations (N);

            if No (Decls) then
               Decls := New_List;
               Set_Declarations (N, Decls);
            end if;

            Prepend_To (Decls, Prag);

         --  When the aspect is associated with a [generic] package declaration
         --  insert the generated pragma at the top of the visible declarations
         --  to emulate the behavior of a source pragma.

         --    package Pack with Aspect is

         --    package Pack is
         --       pragma Prag;

         elsif Nkind (N) in N_Generic_Package_Declaration
                          | N_Package_Declaration
         then
            Decls := Visible_Declarations (Specification (N));

            if No (Decls) then
               Decls := New_List;
               Set_Visible_Declarations (Specification (N), Decls);
            end if;

            --  The visible declarations of a generic instance have the
            --  following structure:

            --    <renamings of generic formals>
            --    <renamings of internally-generated spec and body>
            --    <first source declaration>

            --  Insert the pragma before the first source declaration by
            --  skipping the instance "header" to ensure proper visibility of
            --  all formals.

            if Is_Instance then
               Decl := First (Decls);
               while Present (Decl) loop
                  if Comes_From_Source (Decl) then
                     Insert_Before (Decl, Prag);
                     Inserted := True;
                     exit;
                  else
                     Next (Decl);
                  end if;
               end loop;

               --  The pragma is placed after the instance "header"

               if not Inserted then
                  Append_To (Decls, Prag);
               end if;

            --  Otherwise this is not a generic instance

            else
               Prepend_To (Decls, Prag);
            end if;

         --  When the aspect is associated with a protected unit declaration,
         --  insert the generated pragma at the top of the visible declarations
         --  the emulate the behavior of a source pragma.

         --    protected [type] Prot with Aspect is

         --    protected [type] Prot is
         --       pragma Prag;

         elsif Nkind (N) = N_Protected_Type_Declaration then
            Def := Protected_Definition (N);

            if No (Def) then
               Def :=
                 Make_Protected_Definition (Sloc (N),
                   Visible_Declarations => New_List,
                   End_Label            => Empty);

               Set_Protected_Definition (N, Def);
            end if;

            Decls := Visible_Declarations (Def);

            if No (Decls) then
               Decls := New_List;
               Set_Visible_Declarations (Def, Decls);
            end if;

            Prepend_To (Decls, Prag);

         --  When the aspect is associated with a task unit declaration, insert
         --  insert the generated pragma at the top of the visible declarations
         --  the emulate the behavior of a source pragma.

         --    task [type] Prot with Aspect is

         --    task [type] Prot is
         --       pragma Prag;

         elsif Nkind (N) = N_Task_Type_Declaration then
            Def := Task_Definition (N);

            if No (Def) then
               Def :=
                 Make_Task_Definition (Sloc (N),
                   Visible_Declarations => New_List,
                   End_Label            => Empty);

               Set_Task_Definition (N, Def);
            end if;

            Decls := Visible_Declarations (Def);

            if No (Decls) then
               Decls := New_List;
               Set_Visible_Declarations (Def, Decls);
            end if;

            Prepend_To (Decls, Prag);

         --  When the context is a library unit, the pragma is added to the
         --  Pragmas_After list.

         elsif Nkind (Parent (N)) = N_Compilation_Unit then
            Aux := Aux_Decls_Node (Parent (N));

            if No (Pragmas_After (Aux)) then
               Set_Pragmas_After (Aux, New_List);
            end if;

            Prepend (Prag, Pragmas_After (Aux));

         --  Default, the pragma is inserted after the context

         else
            Insert_After (N, Prag);
         end if;
      end Insert_Pragma;

      -------------------------
      -- Relocate_Expression --
      -------------------------

      function Relocate_Expression (Source : Node_Id) return Node_Id is
      begin
         if Inside_A_Generic then
            return Source;
         else
            return Atree.Relocate_Node (Source);
         end if;
      end Relocate_Expression;

      --  Local variables

      Aspect : Node_Id;
      Ent    : Node_Id;

      L : constant List_Id := Aspect_Specifications (N);

      Ins_Node : Node_Id := N;
      --  Insert pragmas/attribute definition clause after this node when no
      --  delayed analysis is required.

   --  Start of processing for Analyze_Aspect_Specifications

   begin
      --  The general processing involves building an attribute definition
      --  clause or a pragma node that corresponds to the aspect. Then in order
      --  to delay the evaluation of this aspect to the freeze point, we attach
      --  the corresponding pragma/attribute definition clause to the aspect
      --  specification node, which is then placed in the Rep Item chain. In
      --  this case we mark the entity by setting the flag Has_Delayed_Aspects
      --  and we evaluate the rep item at the freeze point. When the aspect
      --  doesn't have a corresponding pragma/attribute definition clause, then
      --  its analysis is simply delayed at the freeze point.

      --  Some special cases don't require delay analysis, thus the aspect is
      --  analyzed right now.

      --  Note that there is a special handling for Pre, Post, Test_Case,
      --  Contract_Cases, Always_Terminates, Exceptional_Cases and
      --  Subprogram_Variant aspects. In these cases, we do not have to worry
      --  about delay issues, since the pragmas themselves deal with delay of
      --  visibility for the expression analysis. Thus, we just insert the
      --  pragma after the node N.

      if No (L) then
         return;
      end if;

      --  Loop through aspects

      Aspect := First (L);
      Aspect_Loop : while Present (Aspect) loop
         Analyze_One_Aspect : declare

            Aspect_Exit : exception;
            --  This exception is used to exit aspect processing completely. It
            --  is used when an error is detected, and no further processing is
            --  required. It is also used if an earlier error has left the tree
            --  in a state where the aspect should not be processed.

            Expr : constant Node_Id    := Expression (Aspect);
            Id   : constant Node_Id    := Identifier (Aspect);
            Loc  : constant Source_Ptr := Sloc (Aspect);
            Nam  : constant Name_Id    := Chars (Id);
            A_Id : constant Aspect_Id  := Get_Aspect_Id (Nam);

            Aitem : Node_Id := Empty;
            --  The associated N_Pragma or N_Attribute_Definition_Clause

            Anod : Node_Id;
            --  An auxiliary node

            Delay_Required : Boolean;
            --  Set False if delay is not required

            Eloc : Source_Ptr := No_Location;
            --  Source location of expression, modified when we split PPC's. It
            --  is set below when Expr is present.

            procedure Analyze_Aspect_Convention;
            --  Perform analysis of aspect Convention

            procedure Analyze_Aspect_Disable_Controlled;
            --  Perform analysis of aspect Disable_Controlled

            procedure Analyze_Aspect_Export_Import;
            --  Perform analysis of aspects Export or Import

            procedure Analyze_Aspect_External_Link_Name;
            --  Perform analysis of aspects External_Name or Link_Name

            procedure Analyze_Aspect_Implicit_Dereference;
            --  Perform analysis of the Implicit_Dereference aspects

            procedure Analyze_Aspect_Relaxed_Initialization;
            --  Perform analysis of aspect Relaxed_Initialization

            procedure Analyze_Aspect_Yield;
            --  Perform analysis of aspect Yield

            procedure Analyze_Aspect_Static;
            --  Ada 2022 (AI12-0075): Perform analysis of aspect Static

            procedure Check_Expr_Is_OK_Static_Expression
              (Expr : Node_Id;
               Typ  : Entity_Id := Empty);
            --  Check the specified expression Expr to make sure that it is a
            --  static expression of the given type (i.e. it will be analyzed
            --  and resolved using this type, which can be any valid argument
            --  to Resolve, e.g. Any_Integer is OK). If not, give an error
            --  and raise Aspect_Exit. If Typ is left Empty, then any static
            --  expression is allowed. Includes checking that the expression
            --  does not raise Constraint_Error.

            function Directly_Specified
              (Id : Entity_Id; A : Aspect_Id) return Boolean;
            --  Returns True if the given aspect is directly (as opposed to
            --  via any form of inheritance) specified for the given entity.

            function Make_Aitem_Pragma
              (Pragma_Argument_Associations : List_Id;
               Pragma_Name                  : Name_Id) return Node_Id;
            --  This is a wrapper for Make_Pragma used for converting aspects
            --  to pragmas. It takes care of Sloc (set from Loc) and building
            --  the pragma identifier from the given name. In addition the flag
            --  Class_Present is set from the aspect node, as well as
            --  Is_Ignored. This routine also sets the
            --  From_Aspect_Specification in the resulting pragma node to True,
            --  and sets Corresponding_Aspect to point to the aspect. The
            --  resulting pragma is assigned to Aitem.

            -------------------------------
            -- Analyze_Aspect_Convention --
            -------------------------------

            procedure Analyze_Aspect_Convention is
               Conv    : Node_Id;
               Dummy_1 : Node_Id;
               Dummy_2 : Node_Id;
               Dummy_3 : Node_Id;
               Expo    : Node_Id;
               Imp     : Node_Id;

            begin
               --  Obtain all interfacing aspects that apply to the related
               --  entity.

               Get_Interfacing_Aspects
                 (Iface_Asp => Aspect,
                  Conv_Asp  => Dummy_1,
                  EN_Asp    => Dummy_2,
                  Expo_Asp  => Expo,
                  Imp_Asp   => Imp,
                  LN_Asp    => Dummy_3,
                  Do_Checks => True);

               --  The related entity is subject to aspect Export or Import.
               --  Do not process Convention now because it must be analysed
               --  as part of Export or Import.

               if Present (Expo) or else Present (Imp) then
                  return;

               --  Otherwise Convention appears by itself

               else
                  --  The aspect specifies a particular convention

                  if Present (Expr) then
                     Conv := New_Copy_Tree (Expr);

                  --  Otherwise assume convention Ada

                  else
                     Conv := Make_Identifier (Loc, Name_Ada);
                  end if;

                  --  Generate:
                  --    pragma Convention (<Conv>, <E>);

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Name => Name_Convention,
                     Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Conv),
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Ent)));

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
               end if;
            end Analyze_Aspect_Convention;

            ---------------------------------------
            -- Analyze_Aspect_Disable_Controlled --
            ---------------------------------------

            procedure Analyze_Aspect_Disable_Controlled is
            begin
               Error_Msg_Name_1 := Nam;

               --  The aspect applies only to controlled records

               if not (Ekind (E) = E_Record_Type
                        and then Is_Controlled_Active (E))
               then
                  Error_Msg_N
                    ("aspect % requires controlled record type", Aspect);
                  return;
               end if;

               --  Preanalyze the expression (if any) when the aspect resides
               --  in a generic unit.

               if Inside_A_Generic then
                  if Present (Expr) then
                     Preanalyze_And_Resolve (Expr, Any_Boolean);
                  end if;

               --  Otherwise the aspect resides in a nongeneric context

               else
                  --  A controlled record type loses its controlled semantics
                  --  when the expression statically evaluates to True.

                  if Present (Expr) then
                     Analyze_And_Resolve (Expr, Any_Boolean);

                     if Is_OK_Static_Expression (Expr) then
                        if Is_True (Static_Boolean (Expr)) then
                           Set_Disable_Controlled (E);
                        end if;

                     --  Otherwise the expression is not static

                     else
                        Flag_Non_Static_Expr
                          ("expression of aspect % must be static!", Aspect);
                     end if;

                  --  Otherwise the aspect appears without an expression and
                  --  defaults to True.

                  else
                     Set_Disable_Controlled (E);
                  end if;
               end if;
            end Analyze_Aspect_Disable_Controlled;

            ----------------------------------
            -- Analyze_Aspect_Export_Import --
            ----------------------------------

            procedure Analyze_Aspect_Export_Import is
               Dummy_1 : Node_Id;
               Dummy_2 : Node_Id;
               Dummy_3 : Node_Id;
               Expo    : Node_Id;
               Imp     : Node_Id;

            begin
               --  Obtain all interfacing aspects that apply to the related
               --  entity.

               Get_Interfacing_Aspects
                 (Iface_Asp => Aspect,
                  Conv_Asp  => Dummy_1,
                  EN_Asp    => Dummy_2,
                  Expo_Asp  => Expo,
                  Imp_Asp   => Imp,
                  LN_Asp    => Dummy_3,
                  Do_Checks => True);

               --  The related entity cannot be subject to both aspects Export
               --  and Import.

               if Present (Expo) and then Present (Imp) then
                  Error_Msg_N
                    ("incompatible interfacing aspects given for &", E);
                  Error_Msg_Sloc := Sloc (Expo);
                  Error_Msg_N ("\aspect Export #", E);
                  Error_Msg_Sloc := Sloc (Imp);
                  Error_Msg_N ("\aspect Import #", E);
               end if;

               --  A variable is most likely modified from the outside. Take
               --  the optimistic approach to avoid spurious errors.

               if Ekind (E) = E_Variable then
                  Set_Never_Set_In_Source (E, False);
               end if;

               --  Resolve the expression of an Import or Export here, and
               --  require it to be of type Boolean and static. This is not
               --  quite right, because in general this should be delayed,
               --  but that seems tricky for these, because normally Boolean
               --  aspects are replaced with pragmas at the freeze point in
               --  Make_Pragma_From_Boolean_Aspect.

               if No (Expr)
                 or else Is_True (Static_Boolean (Expr))
               then
                  if A_Id = Aspect_Import then
                     Set_Has_Completion (E);
                     Set_Is_Imported (E);

                     --  An imported object cannot be explicitly initialized

                     if Nkind (N) = N_Object_Declaration
                       and then Present (Expression (N))
                     then
                        Error_Msg_N
                          ("imported entities cannot be initialized "
                           & "(RM B.1(24))", Expression (N));
                     end if;

                  else
                     pragma Assert (A_Id = Aspect_Export);
                     Set_Is_Exported (E);
                  end if;

                  --  Create the proper form of pragma Export or Import taking
                  --  into account Conversion, External_Name, and Link_Name.

                  Aitem := Build_Export_Import_Pragma (Aspect, E);

               --  Otherwise the expression is either False or erroneous. There
               --  is no corresponding pragma.

               else
                  Aitem := Empty;
               end if;
            end Analyze_Aspect_Export_Import;

            ---------------------------------------
            -- Analyze_Aspect_External_Link_Name --
            ---------------------------------------

            procedure Analyze_Aspect_External_Link_Name is
               Dummy_1 : Node_Id;
               Dummy_2 : Node_Id;
               Dummy_3 : Node_Id;
               Expo    : Node_Id;
               Imp     : Node_Id;

            begin
               --  Obtain all interfacing aspects that apply to the related
               --  entity.

               Get_Interfacing_Aspects
                 (Iface_Asp => Aspect,
                  Conv_Asp  => Dummy_1,
                  EN_Asp    => Dummy_2,
                  Expo_Asp  => Expo,
                  Imp_Asp   => Imp,
                  LN_Asp    => Dummy_3,
                  Do_Checks => True);

               --  Ensure that aspect External_Name applies to aspect Export or
               --  Import.

               if A_Id = Aspect_External_Name then
                  if No (Expo) and then No (Imp) then
                     Error_Msg_N
                       ("aspect External_Name requires aspect Import or "
                        & "Export", Aspect);
                  end if;

               --  Otherwise ensure that aspect Link_Name applies to aspect
               --  Export or Import.

               else
                  pragma Assert (A_Id = Aspect_Link_Name);
                  if No (Expo) and then No (Imp) then
                     Error_Msg_N
                       ("aspect Link_Name requires aspect Import or Export",
                        Aspect);
                  end if;
               end if;
            end Analyze_Aspect_External_Link_Name;

            -----------------------------------------
            -- Analyze_Aspect_Implicit_Dereference --
            -----------------------------------------

            procedure Analyze_Aspect_Implicit_Dereference is
            begin
               if not Is_Type (E) or else not Has_Discriminants (E) then
                  Error_Msg_N
                    ("aspect must apply to a type with discriminants", Expr);

               elsif not Is_First_Subtype (E) then
                  Error_Msg_N
                    ("aspect not specifiable in a subtype declaration",
                     Aspect);

               elsif not Is_Entity_Name (Expr) then
                  Error_Msg_N
                    ("aspect must name a discriminant of current type", Expr);

               else
                  --  Discriminant type be an anonymous access type or an
                  --  anonymous access to subprogram.

                  --  Missing synchronized types???

                  declare
                     Disc : Entity_Id := First_Discriminant (E);
                  begin
                     while Present (Disc) loop
                        if Chars (Expr) = Chars (Disc)
                          and then Ekind (Etype (Disc)) in
                            E_Anonymous_Access_Subprogram_Type |
                            E_Anonymous_Access_Type
                        then
                           Set_Has_Implicit_Dereference (E);
                           Set_Has_Implicit_Dereference (Disc);
                           exit;
                        end if;

                        Next_Discriminant (Disc);
                     end loop;

                     --  Error if no proper access discriminant

                     if Present (Disc) then
                        --  For a type extension, check whether parent has
                        --  a reference discriminant, to verify that use is
                        --  proper.

                        if Is_Derived_Type (E)
                          and then Has_Discriminants (Etype (E))
                        then
                           declare
                              Parent_Disc : constant Entity_Id :=
                                Get_Reference_Discriminant (Etype (E));
                           begin
                              if Present (Parent_Disc)
                                and then Corresponding_Discriminant (Disc) /=
                                           Parent_Disc
                              then
                                 Error_Msg_N
                                   ("reference discriminant does not match "
                                      & "discriminant of parent type", Expr);
                              end if;
                           end;
                        end if;

                     else
                        Error_Msg_NE
                          ("not an access discriminant of&", Expr, E);
                     end if;
                  end;
               end if;

            end Analyze_Aspect_Implicit_Dereference;

            -------------------------------------------
            -- Analyze_Aspect_Relaxed_Initialization --
            -------------------------------------------

            procedure Analyze_Aspect_Relaxed_Initialization is
               procedure Analyze_Relaxed_Parameter
                 (Subp_Id : Entity_Id;
                  Param   : Node_Id;
                  Seen    : in out Elist_Id);
               --  Analyze parameter that appears in the expression of the
               --  aspect Relaxed_Initialization.

               -------------------------------
               -- Analyze_Relaxed_Parameter --
               -------------------------------

               procedure Analyze_Relaxed_Parameter
                 (Subp_Id : Entity_Id;
                  Param   : Node_Id;
                  Seen    : in out Elist_Id)
               is
               begin
                  --  Set name of the aspect for error messages
                  Error_Msg_Name_1 := Nam;

                  --  The relaxed parameter is a formal parameter

                  if Nkind (Param) in N_Identifier | N_Expanded_Name then
                     Analyze (Param);

                     declare
                        Item : constant Entity_Id := Entity (Param);
                     begin
                        --  It must be a formal of the analyzed subprogram

                        if Scope (Item) = Subp_Id then

                           pragma Assert (Is_Formal (Item));

                           --  It must not have scalar or access type

                           if Is_Elementary_Type (Etype (Item)) then
                              Error_Msg_N ("illegal aspect % item", Param);
                              Error_Msg_N
                                ("\item must not have elementary type", Param);
                           end if;

                           --  Detect duplicated items

                           if Contains (Seen, Item) then
                              Error_Msg_N ("duplicate aspect % item", Param);
                           else
                              Append_New_Elmt (Item, Seen);
                           end if;
                        else
                           Error_Msg_N ("illegal aspect % item", Param);
                        end if;
                     end;

                  --  The relaxed parameter is the function's Result attribute

                  elsif Is_Attribute_Result (Param) then
                     Analyze (Param);

                     declare
                        Pref : constant Node_Id := Prefix (Param);
                     begin
                        if Present (Pref)
                          and then
                            Nkind (Pref) in N_Identifier | N_Expanded_Name
                          and then
                            Entity (Pref) = Subp_Id
                        then
                           --  Function result must not have scalar or access
                           --  type.

                           if Is_Elementary_Type (Etype (Pref)) then
                              Error_Msg_N ("illegal aspect % item", Param);
                              Error_Msg_N
                                ("\function result must not have elementary"
                                 & " type", Param);
                           end if;

                           --  Detect duplicated items

                           if Contains (Seen, Subp_Id) then
                              Error_Msg_N ("duplicate aspect % item", Param);
                           else
                              Append_New_Elmt (Entity (Pref), Seen);
                           end if;

                        else
                           Error_Msg_N ("illegal aspect % item", Param);
                        end if;
                     end;
                  else
                     Error_Msg_N ("illegal aspect % item", Param);
                  end if;
               end Analyze_Relaxed_Parameter;

               --  Local variables

               Seen : Elist_Id := No_Elist;
               --  Items that appear in the relaxed initialization aspect
               --  expression of a subprogram; for detecting duplicates.

               Restore_Scope : Boolean;
               --  Will be set to True if we need to restore the scope table
               --  after analyzing the aspect expression.

               Prev_Id : Entity_Id;

            --  Start of processing for Analyze_Aspect_Relaxed_Initialization

            begin
               --  Set name of the aspect for error messages
               Error_Msg_Name_1 := Nam;

               --  Annotation of a type; no aspect expression is allowed.
               --  For a private type, the aspect must be attached to the
               --  partial view.
               --
               --  ??? Once the exact rule for this aspect is ready, we will
               --  likely reject concurrent types, etc., so let's keep the code
               --  for types and variable separate.

               if Is_First_Subtype (E) then
                  Prev_Id := Incomplete_Or_Partial_View (E);
                  if Present (Prev_Id) then

                     --  Aspect may appear on the full view of an incomplete
                     --  type because the incomplete declaration cannot have
                     --  any aspects.

                     if Ekind (Prev_Id) = E_Incomplete_Type then
                        null;
                     else
                        Error_Msg_N ("aspect % must apply to partial view", N);
                     end if;

                  elsif Present (Expr) then
                     Error_Msg_N ("illegal aspect % expression", Expr);
                  end if;

               --  Annotation of a variable; no aspect expression is allowed

               elsif Ekind (E) = E_Variable then
                  if Present (Expr) then
                     Error_Msg_N ("illegal aspect % expression", Expr);
                  end if;

               --  Annotation of a constant; no aspect expression is allowed.
               --  For a deferred constant, the aspect must be attached to the
               --  partial view.

               elsif Ekind (E) = E_Constant then
                  if Present (Incomplete_Or_Partial_View (E)) then
                     Error_Msg_N
                       ("aspect % must apply to deferred constant", N);

                  elsif Present (Expr) then
                     Error_Msg_N ("illegal aspect % expression", Expr);
                  end if;

               --  Annotation of a subprogram; aspect expression is required

               elsif Is_Subprogram_Or_Entry (E)
                 or else Is_Generic_Subprogram (E)
               then
                  if Present (Expr) then

                     --  If we analyze subprogram body that acts as its own
                     --  spec, then the subprogram itself and its formals are
                     --  already installed; otherwise, we need to install them,
                     --  as they must be visible when analyzing the aspect
                     --  expression.

                     if In_Open_Scopes (E) then
                        Restore_Scope := False;
                     else
                        Restore_Scope := True;
                        Push_Scope (E);

                        --  Only formals of the subprogram itself can appear
                        --  in Relaxed_Initialization aspect expression, not
                        --  formals of the enclosing generic unit. (This is
                        --  different than in Precondition or Depends aspects,
                        --  where both kinds of formals are allowed.)

                        Install_Formals (E);
                     end if;

                     --  Aspect expression is either an aggregate with list of
                     --  parameters (and possibly the Result attribute for a
                     --  function).

                     if Nkind (Expr) = N_Aggregate then

                        --  Component associations in the aggregate must be a
                        --  parameter name followed by a static boolean
                        --  expression.

                        if Present (Component_Associations (Expr)) then
                           declare
                              Assoc : Node_Id :=
                                First (Component_Associations (Expr));
                           begin
                              while Present (Assoc) loop
                                 if List_Length (Choices (Assoc)) = 1 then
                                    Analyze_Relaxed_Parameter
                                      (E, First (Choices (Assoc)), Seen);

                                    if Inside_A_Generic then
                                       Preanalyze_And_Resolve
                                         (Expression (Assoc), Any_Boolean);
                                    else
                                       Analyze_And_Resolve
                                         (Expression (Assoc), Any_Boolean);
                                    end if;

                                    if not Is_OK_Static_Expression
                                      (Expression (Assoc))
                                    then
                                       Error_Msg_Name_1 := Nam;
                                       Flag_Non_Static_Expr
                                         ("expression of aspect % " &
                                          "must be static!", Aspect);
                                    end if;

                                 else
                                    Error_Msg_Name_1 := Nam;
                                    Error_Msg_N
                                      ("illegal aspect % expression", Expr);
                                 end if;
                                 Next (Assoc);
                              end loop;
                           end;
                        end if;

                        --  Expressions of the aggregate are parameter names

                        if Present (Expressions (Expr)) then
                           declare
                              Param : Node_Id := First (Expressions (Expr));

                           begin
                              while Present (Param) loop
                                 Analyze_Relaxed_Parameter (E, Param, Seen);
                                 Next (Param);
                              end loop;
                           end;
                        end if;

                        --  Mark the aggregate expression itself as analyzed;
                        --  its subexpressions were marked when they themselves
                        --  were analyzed.

                        Set_Analyzed (Expr);

                     --  Otherwise, it is a single name of a subprogram
                     --  parameter (or possibly the Result attribute for
                     --  a function).

                     else
                        Analyze_Relaxed_Parameter (E, Expr, Seen);
                     end if;

                     if Restore_Scope then
                        End_Scope;
                     end if;
                  else
                     Error_Msg_N ("missing expression for aspect %", N);
                  end if;

               else
                  Error_Msg_N ("inappropriate entity for aspect %", E);
               end if;
            end Analyze_Aspect_Relaxed_Initialization;

            ---------------------------
            -- Analyze_Aspect_Static --
            ---------------------------

            procedure Analyze_Aspect_Static is
               function Has_Convention_Intrinsic (L : List_Id) return Boolean;
               --  Return True if L contains a pragma argument association
               --  node representing a convention Intrinsic.

               ------------------------------
               -- Has_Convention_Intrinsic --
               ------------------------------

               function Has_Convention_Intrinsic
                 (L : List_Id) return Boolean
               is
                  Arg : Node_Id := First (L);
               begin
                  while Present (Arg) loop
                     if Nkind (Arg) = N_Pragma_Argument_Association
                       and then Chars (Arg) = Name_Convention
                       and then Chars (Expression (Arg)) = Name_Intrinsic
                     then
                        return True;
                     end if;

                     Next (Arg);
                  end loop;

                  return False;
               end Has_Convention_Intrinsic;

               Is_Imported_Intrinsic : Boolean;

            begin
               if Ada_Version < Ada_2022 then
                  Error_Msg_Ada_2022_Feature ("aspect %", Loc);
                  return;
               end if;

               Is_Imported_Intrinsic := Is_Imported (E)
                 and then
                   Has_Convention_Intrinsic
                     (Pragma_Argument_Associations (Import_Pragma (E)));

               --  The aspect applies only to expression functions that
               --  statisfy the requirements for a static expression function
               --  (such as having an expression that is predicate-static) as
               --  well as Intrinsic imported functions as a -gnatX extension.

               if not Is_Expression_Function (E)
                 and then
                   not (All_Extensions_Allowed and then Is_Imported_Intrinsic)
               then
                  if All_Extensions_Allowed then
                     Error_Msg_N
                       ("aspect % requires intrinsic or expression function",
                        Aspect);

                  elsif Is_Imported_Intrinsic then
                     Error_Msg_GNAT_Extension
                       ("aspect % on intrinsic function", Loc,
                        Is_Core_Extension => True);

                  else
                     Error_Msg_N
                       ("aspect % requires expression function", Aspect);
                  end if;

                  return;

               --  Ada 2022 (AI12-0075): Check that the function satisfies
               --  several requirements of static functions as specified in
               --  RM 6.8(5.1-5.8). Note that some of the requirements given
               --  there are checked elsewhere.

               else
                  --  The expression of the expression function must be a
                  --  potentially static expression (RM 2022 6.8(3.2-3.4)).
                  --  That's checked in Sem_Ch6.Analyze_Expression_Function.

                  --  The function must not contain any calls to itself, which
                  --  is checked in Sem_Res.Resolve_Call.

                  --  Each formal must be of mode in and have a static subtype

                  declare
                     Formal : Entity_Id := First_Formal (E);
                  begin
                     while Present (Formal) loop
                        if Ekind (Formal) /= E_In_Parameter then
                           Error_Msg_N
                             ("aspect % requires formals of mode IN",
                              Aspect);

                           return;
                        end if;

                        if not Is_Static_Subtype (Etype (Formal)) then
                           Error_Msg_N
                             ("aspect % requires formals with static subtypes",
                              Aspect);

                           return;
                        end if;

                        Next_Formal (Formal);
                     end loop;
                  end;

                  --  The function's result subtype must be a static subtype

                  if not Is_Static_Subtype (Etype (E)) then
                     Error_Msg_N
                       ("aspect % requires function with result of "
                        & "a static subtype",
                        Aspect);

                     return;
                  end if;

                  --  Check that the function does not have any applicable
                  --  precondition or postcondition expression.

                  for Asp in Pre_Post_Aspects loop
                     if Has_Aspect (E, Asp) then
                        Error_Msg_Name_1 := Aspect_Names (Asp);
                        Error_Msg_N
                          ("aspect % is not allowed for a static "
                           & "expression function",
                           Find_Aspect (E, Asp));

                        return;
                     end if;
                  end loop;

                  --  ??? Must check that "for result type R, if the
                  --  function is a boundary entity for type R (see 7.3.2),
                  --  no type invariant applies to type R; if R has a
                  --  component type C, a similar rule applies to C."
               end if;

               --  When the expression is present, it must be static. If it
               --  evaluates to True, the expression function is treated as
               --  a static function. Otherwise the aspect appears without
               --  an expression and defaults to True.

               if Present (Expr) then
                  --  Preanalyze the expression when the aspect resides in a
                  --  generic unit. (Is this generic-related code necessary
                  --  for this aspect? It's modeled on what's done for aspect
                  --  Disable_Controlled. ???)

                  if Inside_A_Generic then
                     Preanalyze_And_Resolve (Expr, Any_Boolean);

                  --  Otherwise the aspect resides in a nongeneric context

                  else
                     Analyze_And_Resolve (Expr, Any_Boolean);

                     --  Error if the boolean expression is not static

                     if not Is_OK_Static_Expression (Expr) then
                        Flag_Non_Static_Expr
                          ("expression of aspect % must be static!", Aspect);
                     end if;
                  end if;
               end if;
            end Analyze_Aspect_Static;

            --------------------------
            -- Analyze_Aspect_Yield --
            --------------------------

            procedure Analyze_Aspect_Yield is
               Expr_Value : Boolean := False;

            begin
               --  Check valid entity for 'Yield

               if (Is_Subprogram (E)
                     or else Is_Generic_Subprogram (E)
                     or else Is_Entry (E))
                 and then not Within_Protected_Type (E)
               then
                  null;

               elsif Within_Protected_Type (E) then
                  Error_Msg_N
                    ("aspect% not applicable to protected operation", Id);
                  return;

               else
                  Error_Msg_N
                    ("aspect% only applicable to subprogram and entry "
                     & "declarations", Id);
                  return;
               end if;

               --  Evaluate its static expression (if available); otherwise it
               --  defaults to True.

               if No (Expr) then
                  Expr_Value := True;

               --  Otherwise it must have a static boolean expression

               else
                  if Inside_A_Generic then
                     Preanalyze_And_Resolve (Expr, Any_Boolean);
                  else
                     Analyze_And_Resolve (Expr, Any_Boolean);
                  end if;

                  if Is_OK_Static_Expression (Expr) then
                     if Is_True (Static_Boolean (Expr)) then
                        Expr_Value := True;
                     end if;
                  else
                     Flag_Non_Static_Expr
                       ("expression of aspect % must be static!", Aspect);
                  end if;
               end if;

               if Expr_Value then
                  Set_Has_Yield_Aspect (E);
               end if;

               --  If the Yield aspect is specified for a dispatching
               --  subprogram that inherits the aspect, the specified
               --  value shall be confirming.

               if Present (Expr)
                 and then Is_Dispatching_Operation (E)
                 and then Present (Overridden_Operation (E))
                 and then Has_Yield_Aspect (Overridden_Operation (E))
                            /= Is_True (Static_Boolean (Expr))
               then
                  Error_Msg_N ("specification of inherited aspect% can only " &
                               "confirm parent value", Id);
               end if;
            end Analyze_Aspect_Yield;

            ----------------------------------------
            -- Check_Expr_Is_OK_Static_Expression --
            ----------------------------------------

            procedure Check_Expr_Is_OK_Static_Expression
              (Expr : Node_Id;
               Typ  : Entity_Id := Empty)
            is
            begin
               if Present (Typ) then
                  Analyze_And_Resolve (Expr, Typ);
               else
                  Analyze_And_Resolve (Expr);
               end if;

               --  An expression cannot be considered static if its resolution
               --  failed or if it's erroneous. Stop the analysis of the
               --  related aspect.

               if Etype (Expr) = Any_Type or else Error_Posted (Expr) then
                  raise Aspect_Exit;

               elsif Is_OK_Static_Expression (Expr) then
                  return;

               --  Finally, we have a real error

               else
                  Error_Msg_Name_1 := Nam;
                  Flag_Non_Static_Expr
                    ("entity for aspect% must be a static expression!",
                     Expr);
                  raise Aspect_Exit;
               end if;
            end Check_Expr_Is_OK_Static_Expression;

            ------------------------
            -- Directly_Specified --
            ------------------------

            function Directly_Specified
              (Id : Entity_Id; A : Aspect_Id) return Boolean
            is
               Aspect_Spec : constant Node_Id := Find_Aspect (Id, A);
            begin
               return Present (Aspect_Spec) and then Entity (Aspect_Spec) = Id;
            end Directly_Specified;

            -----------------------
            -- Make_Aitem_Pragma --
            -----------------------

            function Make_Aitem_Pragma
              (Pragma_Argument_Associations : List_Id;
               Pragma_Name                  : Name_Id) return Node_Id
            is
               Args  : List_Id := Pragma_Argument_Associations;
               Aitem : Node_Id;

            begin
               --  We should never get here if aspect was disabled

               pragma Assert (not Is_Disabled (Aspect));

               --  Certain aspects allow for an optional name or expression. Do
               --  not generate a pragma with empty argument association list.

               if No (Args) or else No (Expression (First (Args))) then
                  Args := No_List;
               end if;

               --  Build the pragma

               Aitem :=
                 Make_Pragma (Loc,
                   Pragma_Argument_Associations => Args,
                   Pragma_Identifier =>
                     Make_Identifier (Sloc (Id), Pragma_Name),
                   Class_Present     => Class_Present (Aspect));

               --  Set additional semantic fields

               if Is_Ignored (Aspect) then
                  Set_Is_Ignored (Aitem);
               elsif Is_Checked (Aspect) then
                  Set_Is_Checked (Aitem);
               end if;

               Set_Corresponding_Aspect (Aitem, Aspect);
               Set_From_Aspect_Specification (Aitem);

               return Aitem;
            end Make_Aitem_Pragma;

         --  Start of processing for Analyze_One_Aspect

         begin
            --  Skip aspect if already analyzed, to avoid looping in some cases

            if Analyzed (Aspect) then
               goto Continue;
            end if;

            --  Skip looking at aspect if it is totally disabled. Just mark it
            --  as such for later reference in the tree. This also sets the
            --  Is_Ignored and Is_Checked flags appropriately.

            Check_Applicable_Policy (Aspect);

            if Is_Disabled (Aspect) then
               goto Continue;
            end if;

            --  Set the source location of expression, used in the case of
            --  a failed precondition/postcondition or invariant. Note that
            --  the source location of the expression is not usually the best
            --  choice here. For example, it gets located on the last AND
            --  keyword in a chain of boolean expressiond AND'ed together.
            --  It is best to put the message on the first character of the
            --  assertion, which is the effect of the First_Node call here.

            if Present (Expr) then
               Eloc := Sloc (First_Node (Expr));
            end if;

            --  Check restriction No_Implementation_Aspect_Specifications

            if Implementation_Defined_Aspect (A_Id) then
               Check_Restriction
                 (No_Implementation_Aspect_Specifications, Aspect);
            end if;

            --  Check restriction No_Specification_Of_Aspect

            Check_Restriction_No_Specification_Of_Aspect (Aspect);

            --  Mark aspect analyzed (actual analysis is delayed till later)

            if A_Id /= Aspect_User_Aspect then
               --  Analyzed flag is handled differently for a User_Aspect
               --  aspect specification because it can also be analyzed
               --  "on demand" from Aspects.Find_Aspect. So that analysis
               --  tests for the case where the aspect specification has
               --  already been analyzed (in which case it just returns)
               --  and takes care of calling Set_Analyzed.

               Set_Analyzed (Aspect);
            end if;

            Set_Entity (Aspect, E);

            --  Build the reference to E that will be used in the built pragmas

            Ent := New_Occurrence_Of (E, Sloc (Id));

            if A_Id in Aspect_Attach_Handler | Aspect_Interrupt_Handler then

               --  Treat the specification as a reference to the protected
               --  operation, which might otherwise appear unreferenced and
               --  generate spurious warnings.

               Generate_Reference (E, Id);
            end if;

            --  Check for duplicate aspect. Note that the Comes_From_Source
            --  test allows duplicate Pre/Post's that we generate internally
            --  to escape being flagged here.

            if No_Duplicates_Allowed (A_Id) then
               Anod := First (L);
               while Anod /= Aspect loop
                  if Comes_From_Source (Aspect)
                    and then Same_Aspect (A_Id, Get_Aspect_Id (Anod))
                  then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_Sloc := Sloc (Anod);

                     --  Case of same aspect specified twice

                     if Class_Present (Anod) = Class_Present (Aspect) then
                        if not Class_Present (Anod) then
                           Error_Msg_NE
                             ("aspect% for & previously given#",
                              Id, E);
                        else
                           Error_Msg_NE
                             ("aspect `%''Class` for & previously given#",
                              Id, E);
                        end if;
                     end if;
                  end if;

                  Next (Anod);
               end loop;
            end if;

            --  Check some general restrictions on language defined aspects

            if not Implementation_Defined_Aspect (A_Id)
              or else A_Id in Aspect_Async_Readers
                            | Aspect_Async_Writers
                            | Aspect_Effective_Reads
                            | Aspect_Effective_Writes
                            | Aspect_Preelaborable_Initialization
            then
               Error_Msg_Name_1 := Nam;

               --  Not allowed for renaming declarations. Examine the original
               --  node because a subprogram renaming may have been rewritten
               --  as a body.

               if Nkind (Original_Node (N)) in N_Renaming_Declaration then
                  Error_Msg_N
                    ("aspect % not allowed for renaming declaration",
                     Aspect);
               end if;

               --  Not allowed for formal type declarations in previous
               --  versions of the language. Allowed for them only for
               --  shared variable control aspects.

               --  Original node is used in case expansion rewrote the node -
               --  as is the case with generic derived types.

               if Nkind (Original_Node (N)) = N_Formal_Type_Declaration then
                  if Ada_Version < Ada_2022 then
                     Error_Msg_N
                       ("aspect % not allowed for formal type declaration",
                        Aspect);

                  elsif A_Id not in Aspect_Atomic
                                  | Aspect_Volatile
                                  | Aspect_Independent
                                  | Aspect_Atomic_Components
                                  | Aspect_Independent_Components
                                  | Aspect_Volatile_Components
                                  | Aspect_Async_Readers
                                  | Aspect_Async_Writers
                                  | Aspect_Effective_Reads
                                  | Aspect_Effective_Writes
                                  | Aspect_Preelaborable_Initialization
                  then
                     Error_Msg_N
                       ("aspect % not allowed for formal type declaration",
                        Aspect);
                  end if;
               end if;
            end if;

            --  Copy expression for later processing by the procedures
            --  Check_Aspect_At_[Freeze_Point | End_Of_Declarations]

            --  The expression may be a subprogram name, and can
            --  be an operator name that appears as a string, but
            --  requires its own analysis procedure (see sem_ch6).

            if Nkind (Expr) = N_Operator_Symbol then
               Set_Expression_Copy (Aspect, Expr);
            else
               Set_Expression_Copy (Aspect, New_Copy_Tree (Expr));
            end if;

            --  Set Delay_Required as appropriate to aspect

            case Aspect_Delay (A_Id) is
               when Always_Delay =>
                  --  For Boolean aspects, do not delay if no expression

                  if A_Id in Boolean_Aspects | Library_Unit_Aspects then
                     Delay_Required := Present (Expr);
                  else
                     Delay_Required := True;
                  end if;

               when Never_Delay =>
                  Delay_Required := False;

               when Rep_Aspect =>

                  --  For Boolean aspects, do not delay if no expression except
                  --  for Full_Access_Only because we need to process it after
                  --  Volatile and Atomic, which can be independently delayed.

                  if A_Id in Boolean_Aspects
                    and then A_Id /= Aspect_Full_Access_Only
                    and then No (Expr)
                  then
                     Delay_Required := False;

                  --  For non-Boolean aspects, if the expression has the form
                  --  of an integer literal, then do not delay, since we know
                  --  the value cannot change. This optimization catches most
                  --  rep clause cases.

                  elsif A_Id not in Boolean_Aspects
                    and then Present (Expr)
                    and then Nkind (Expr) = N_Integer_Literal
                  then
                     Delay_Required := False;

                  --  For Alignment and various Size aspects, do not delay for
                  --  an attribute reference whose prefix is Standard, for
                  --  example Standard'Maximum_Alignment or Standard'Word_Size.

                  elsif A_Id in Aspect_Alignment
                              | Aspect_Component_Size
                              | Aspect_Object_Size
                              | Aspect_Size
                              | Aspect_Value_Size
                    and then Present (Expr)
                    and then Nkind (Expr) = N_Attribute_Reference
                    and then Nkind (Prefix (Expr)) = N_Identifier
                    and then Chars (Prefix (Expr)) = Name_Standard
                  then
                     Delay_Required := False;

                  --  All other cases are delayed

                  else
                     Delay_Required := True;
                     Set_Has_Delayed_Rep_Aspects (E);
                  end if;
            end case;

            --  Check 13.1(9.2/5): A representation aspect of a subtype or type
            --  shall not be specified (whether by a representation item or an
            --  aspect_specification) before the type is completely defined
            --  (see 3.11.1).

            if Is_Representation_Aspect (A_Id)
              and then Rep_Item_Too_Early (E, N)
            then
               goto Continue;
            end if;

            --  Processing based on specific aspect

            case A_Id is
               when Aspect_Unimplemented =>
                  null; -- ??? temp for now

               --  No_Aspect should be impossible

               when No_Aspect =>
                  raise Program_Error;

               --  Case 1: Aspects corresponding to attribute definition
               --  clauses.

               when Aspect_Address
                  | Aspect_Alignment
                  | Aspect_Bit_Order
                  | Aspect_Component_Size
                  | Aspect_Constant_Indexing
                  | Aspect_Default_Iterator
                  | Aspect_Dispatching_Domain
                  | Aspect_External_Tag
                  | Aspect_Input
                  | Aspect_Iterable
                  | Aspect_Iterator_Element
                  | Aspect_Machine_Radix
                  | Aspect_Object_Size
                  | Aspect_Output
                  | Aspect_Put_Image
                  | Aspect_Read
                  | Aspect_Scalar_Storage_Order
                  | Aspect_Simple_Storage_Pool
                  | Aspect_Size
                  | Aspect_Small
                  | Aspect_Storage_Pool
                  | Aspect_Stream_Size
                  | Aspect_Value_Size
                  | Aspect_Variable_Indexing
                  | Aspect_Write
               =>
                  --  Indexing aspects apply only to tagged type

                  if A_Id in Aspect_Constant_Indexing
                           | Aspect_Variable_Indexing
                    and then not (Is_Type (E)
                                   and then Is_Tagged_Type (E))
                  then
                     Error_Msg_N
                       ("indexing aspect can only apply to a tagged type",
                        Aspect);
                     goto Continue;
                  end if;

                  --  For the case of aspect Address, we don't consider that we
                  --  know the entity is never set in the source, since it is
                  --  is likely aliasing is occurring.

                  --  Note: one might think that the analysis of the resulting
                  --  attribute definition clause would take care of that, but
                  --  that's not the case since it won't be from source.

                  if A_Id = Aspect_Address then
                     Set_Never_Set_In_Source (E, False);
                  end if;

                  --  Correctness of the profile of a stream operation is
                  --  verified at the freeze point, but we must detect the
                  --  illegal specification of this aspect for a subtype now,
                  --  to prevent malformed rep_item chains.

                  if A_Id in Aspect_Input
                           | Aspect_Output
                           | Aspect_Read
                           | Aspect_Write
                  then
                     if not Is_First_Subtype (E) then
                        Error_Msg_N
                          ("local name must be a first subtype", Aspect);
                        goto Continue;

                     --  If stream aspect applies to the class-wide type,
                     --  the generated attribute definition applies to the
                     --  class-wide type as well.

                     elsif Class_Present (Aspect) then
                        Ent :=
                          Make_Attribute_Reference (Loc,
                            Prefix         => Ent,
                            Attribute_Name => Name_Class);
                     end if;
                  end if;

                  --  Propagate the 'Size'Class aspect to the class-wide type

                  if A_Id = Aspect_Size and then Class_Present (Aspect) then
                     Ent :=
                       Make_Attribute_Reference (Loc,
                         Prefix         => Ent,
                         Attribute_Name => Name_Class);
                  end if;

                  --  Construct the attribute_definition_clause. The expression
                  --  in the aspect specification is simply shared with the
                  --  constructed attribute, because it will be fully analyzed
                  --  when the attribute is processed.

                  Aitem :=
                    Make_Attribute_Definition_Clause (Loc,
                      Name       => Ent,
                      Chars      => Nam,
                      Expression => Relocate_Expression (Expr));

                  --  If the address is specified, then we treat the entity as
                  --  referenced, to avoid spurious warnings. This is analogous
                  --  to what is done with an attribute definition clause, but
                  --  here we don't want to generate a reference because this
                  --  is the point of definition of the entity.

                  if A_Id = Aspect_Address then
                     Set_Referenced (E);
                  end if;

               --  Case 2: Aspects corresponding to pragmas

               --  Case 2a: Aspects corresponding to pragmas with two
               --  arguments, where the first argument is a local name
               --  referring to the entity, and the second argument is the
               --  aspect definition expression.

               --  Linker_Section

               when Aspect_Linker_Section =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Linker_Section);

                  --  No need to delay the processing if the entity is already
                  --  frozen. This should only happen for subprogram bodies.

                  if Is_Frozen (E) then
                     pragma Assert (Nkind (N) = N_Subprogram_Body);
                     Delay_Required := False;
                  end if;

               --  Synchronization

               --  Corresponds to pragma Implemented, construct the pragma

               when Aspect_Synchronization =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Implemented);

               --  Attach_Handler

               when Aspect_Attach_Handler =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Expression (Expr))),
                     Pragma_Name                  => Name_Attach_Handler);

                  --  We need to insert this pragma into the tree to get proper
                  --  processing and to look valid from a placement viewpoint.

                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Dynamic_Predicate, Predicate, Static_Predicate

               when Aspect_Dynamic_Predicate
                  | Aspect_Ghost_Predicate
                  | Aspect_Predicate
                  | Aspect_Static_Predicate
               =>
                  --  These aspects apply only to subtypes

                  if not Is_Type (E) then
                     Error_Msg_N
                       ("predicate can only be specified for a subtype",
                        Aspect);
                     goto Continue;

                  elsif Is_Incomplete_Type (E) then
                     Error_Msg_N
                       ("predicate cannot apply to incomplete view", Aspect);

                  elsif Is_Generic_Type (E) then
                     Error_Msg_N
                       ("predicate cannot apply to formal type", Aspect);
                     goto Continue;
                  end if;

                  --  Construct the pragma (always a pragma Predicate, with
                  --  flags recording whether it is static/dynamic). We also
                  --  set flags recording this in the type itself.

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Expression (Expr))),
                     Pragma_Name => Name_Predicate);

                  --  Mark type has predicates, and remember what kind of
                  --  aspect lead to this predicate (we need this to access
                  --  the right set of check policies later on).

                  Set_Has_Predicates (E);

                  if A_Id = Aspect_Dynamic_Predicate then
                     Set_Has_Dynamic_Predicate_Aspect (E);

                     --  If the entity has a dynamic predicate, any inherited
                     --  static predicate becomes dynamic as well, and the
                     --  predicate function includes the conjunction of both.

                     Set_Has_Static_Predicate_Aspect (E, False);

                     --  Query the applicable policy since it must rely on the
                     --  policy applicable in the context of the declaration of
                     --  entity E; it cannot be done when the built pragma is
                     --  analyzed because it will be analyzed when E is frozen,
                     --  and at that point the applicable policy may differ.
                     --  For example:

                     --  pragma Assertion_Policy (Dynamic_Predicate => Check);
                     --  type T is ... with Dynamic_Predicate => ...
                     --  pragma Assertion_Policy (Dynamic_Predicate => Ignore);
                     --  X : T; --  freezes T

                     Set_Predicates_Ignored (E,
                       Policy_In_Effect (Name_Dynamic_Predicate)
                         = Name_Ignore);

                  elsif A_Id = Aspect_Static_Predicate then
                     Set_Has_Static_Predicate_Aspect (E);
                  elsif A_Id = Aspect_Ghost_Predicate then
                     Set_Has_Ghost_Predicate_Aspect (E);
                  end if;

                  --  If the type is private, indicate that its completion
                  --  has a freeze node, because that is the one that will
                  --  be visible at freeze time.

                  if Is_Private_Type (E) and then Present (Full_View (E)) then
                     Set_Has_Predicates (Full_View (E));

                     if A_Id = Aspect_Dynamic_Predicate then
                        Set_Has_Dynamic_Predicate_Aspect (Full_View (E));
                     elsif A_Id = Aspect_Static_Predicate then
                        Set_Has_Static_Predicate_Aspect (Full_View (E));
                     elsif A_Id = Aspect_Ghost_Predicate then
                        Set_Has_Ghost_Predicate_Aspect (Full_View (E));
                     end if;

                     Set_Has_Delayed_Aspects (Full_View (E));
                     Ensure_Freeze_Node (Full_View (E));

                     --  If there is an Underlying_Full_View, also create a
                     --  freeze node for that one.

                     if Is_Private_Type (Full_View (E)) then
                        declare
                           U_Full : constant Entity_Id :=
                             Underlying_Full_View (Full_View (E));
                        begin
                           if Present (U_Full) then
                              Set_Has_Delayed_Aspects (U_Full);
                              Ensure_Freeze_Node (U_Full);
                           end if;
                        end;
                     end if;
                  end if;

               --  Predicate_Failure

               when Aspect_Predicate_Failure =>

                  --  This aspect applies only to subtypes

                  if not Is_Type (E) then
                     Error_Msg_N
                       ("predicate can only be specified for a subtype",
                        Aspect);
                     goto Continue;

                  elsif Is_Incomplete_Type (E) then
                     Error_Msg_N
                       ("predicate cannot apply to incomplete view", Aspect);
                     goto Continue;

                  elsif not Has_Predicates (E) then
                     Error_Msg_N
                       ("Predicate_Failure requires previous predicate" &
                        " specification", Aspect);
                     goto Continue;

                  elsif not (Directly_Specified (E, Aspect_Dynamic_Predicate)
                    or else Directly_Specified (E, Aspect_Predicate)
                    or else Directly_Specified (E, Aspect_Ghost_Predicate)
                    or else Directly_Specified (E, Aspect_Static_Predicate))
                  then
                     Error_Msg_N
                       ("Predicate_Failure requires accompanying" &
                        " noninherited predicate specification", Aspect);
                     goto Continue;
                  end if;

                  --  Construct the pragma

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name => Name_Predicate_Failure);

               --  Case 2b: Aspects corresponding to pragmas with two
               --  arguments, where the second argument is a local name
               --  referring to the entity, and the first argument is the
               --  aspect definition expression.

               --  Convention

               when Aspect_Convention =>
                  Analyze_Aspect_Convention;
                  goto Continue;

               --  External_Name, Link_Name

               --  Only the legality checks are done during the analysis, thus
               --  no delay is required.

               when Aspect_External_Name
                  | Aspect_Link_Name
               =>
                  Analyze_Aspect_External_Link_Name;
                  goto Continue;

               --  CPU, Interrupt_Priority, Priority

               --  These three aspects can be specified for a subprogram spec
               --  or body, in which case we analyze the expression and export
               --  the value of the aspect.

               --  Previously, we generated an equivalent pragma for bodies
               --  (note that the specs cannot contain these pragmas). The
               --  pragma was inserted ahead of local declarations, rather than
               --  after the body. This leads to a certain duplication between
               --  the processing performed for the aspect and the pragma, but
               --  given the straightforward handling required it is simpler
               --  to duplicate than to translate the aspect in the spec into
               --  a pragma in the declarative part of the body.

               when Aspect_CPU
                  | Aspect_Interrupt_Priority
                  | Aspect_Priority
               =>
                  --  Verify the expression is static when Static_Priorities is
                  --  enabled.

                  if not Is_OK_Static_Expression (Expr) then
                     Check_Restriction (Static_Priorities, Expr);
                  end if;

                  if Nkind (N) in N_Subprogram_Body | N_Subprogram_Declaration
                  then
                     --  Analyze the aspect expression

                     Analyze_And_Resolve (Expr, Standard_Integer);

                     --  Interrupt_Priority aspect not allowed for main
                     --  subprograms. RM D.1 does not forbid this explicitly,
                     --  but RM J.15.11(6/3) does not permit pragma
                     --  Interrupt_Priority for subprograms.

                     if A_Id = Aspect_Interrupt_Priority then
                        Error_Msg_N
                          ("Interrupt_Priority aspect cannot apply to "
                           & "subprogram", Expr);

                     --  The expression must be static

                     elsif not Is_OK_Static_Expression (Expr) then
                        Flag_Non_Static_Expr
                          ("aspect requires static expression!", Expr);

                     --  Check whether this is the main subprogram. Issue a
                     --  warning only if it is obviously not a main program
                     --  (when it has parameters or when the subprogram is
                     --  within a package).

                     elsif Present (Parameter_Specifications
                                      (Specification (N)))
                       or else not Is_Compilation_Unit (Defining_Entity (N))
                     then
                        --  See RM D.1(14/3) and D.16(12/3)

                        Error_Msg_N
                          ("aspect applied to subprogram other than the "
                           & "main subprogram has no effect??", Expr);

                     --  Otherwise check in range and export the value

                     --  For the CPU aspect

                     elsif A_Id = Aspect_CPU then
                        if Is_In_Range (Expr, RTE (RE_CPU_Range)) then

                           --  Value is correct so we export the value to make
                           --  it available at execution time.

                           Set_Main_CPU
                             (Main_Unit, UI_To_Int (Expr_Value (Expr)));

                        else
                           Error_Msg_N
                             ("main subprogram 'C'P'U is out of range", Expr);
                        end if;

                     --  For the Priority aspect

                     elsif A_Id = Aspect_Priority then
                        if Is_In_Range (Expr, RTE (RE_Priority)) then

                           --  Value is correct so we export the value to make
                           --  it available at execution time.

                           Set_Main_Priority
                             (Main_Unit, UI_To_Int (Expr_Value (Expr)));

                        --  Ignore pragma if Relaxed_RM_Semantics to support
                        --  other targets/non GNAT compilers.

                        elsif not Relaxed_RM_Semantics then
                           Error_Msg_N
                             ("main subprogram priority is out of range",
                              Expr);
                        end if;
                     end if;

                     --  Load an arbitrary entity from System.Tasking.Stages
                     --  or System.Tasking.Restricted.Stages (depending on
                     --  the supported profile) to make sure that one of these
                     --  packages is implicitly with'ed, since we need to have
                     --  the tasking run time active for the pragma Priority to
                     --  have any effect. Previously we with'ed the package
                     --  System.Tasking, but this package does not trigger the
                     --  required initialization of the run-time library.

                     if Restricted_Profile then
                        Discard_Node (RTE (RE_Activate_Restricted_Tasks));
                     else
                        Discard_Node (RTE (RE_Activate_Tasks));
                     end if;

                     --  Handling for these aspects in subprograms is complete

                     goto Continue;

                  --  For task and protected types pass the aspect as an
                  --  attribute.

                  else
                     Aitem :=
                       Make_Attribute_Definition_Clause (Loc,
                         Name       => Ent,
                         Chars      => Nam,
                         Expression => Relocate_Expression (Expr));
                  end if;

               --  Suppress/Unsuppress

               when Aspect_Suppress
                  | Aspect_Unsuppress
               =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr)),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Ent)),
                     Pragma_Name                  => Nam);

                  Delay_Required := False;

               --  Warnings

               when Aspect_Warnings =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr)),
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Ent)),
                     Pragma_Name                  => Name_Warnings);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Case 2c: Aspects corresponding to pragmas with three
               --  arguments.

               --  Invariant aspects have a first argument that references the
               --  entity, a second argument that is the expression and a third
               --  argument that is an appropriate message.

               --  Invariant, Type_Invariant

               when Aspect_Invariant
                  | Aspect_Type_Invariant
               =>
                  --  Analysis of the pragma will verify placement legality:
                  --  an invariant must apply to a private type, or appear in
                  --  the private part of a spec and apply to a completion.

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Invariant);

                  --  Add message unless exception messages are suppressed

                  if not Opt.Exception_Locations_Suppressed then
                     Append_To (Pragma_Argument_Associations (Aitem),
                       Make_Pragma_Argument_Association (Eloc,
                         Chars      => Name_Message,
                         Expression =>
                           Make_String_Literal (Eloc,
                             Strval => "failed invariant from "
                                       & Build_Location_String (Eloc))));
                  end if;

                  --  For Invariant case, insert immediately after the entity
                  --  declaration. We do not have to worry about delay issues
                  --  since the pragma processing takes care of this.

                  Delay_Required := False;

               --  Case 2d : Aspects that correspond to a pragma with one
               --  argument.

               --  Abstract_State

               --  Aspect Abstract_State introduces implicit declarations for
               --  all state abstraction entities it defines. To emulate this
               --  behavior, insert the pragma at the beginning of the visible
               --  declarations of the related package so that it is analyzed
               --  immediately.

               when Aspect_Abstract_State => Abstract_State : declare
                  Context : Node_Id := N;

               begin
                  --  When aspect Abstract_State appears on a generic package,
                  --  it is propagated to the package instance. The context in
                  --  this case is the instance spec.

                  if Nkind (Context) = N_Package_Instantiation then
                     Context := Instance_Spec (Context);
                  end if;

                  if Nkind (Context) in N_Generic_Package_Declaration
                                      | N_Package_Declaration
                  then
                     Aitem := Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Abstract_State);

                     Decorate (Aspect, Aitem);
                     Insert_Pragma
                       (Prag        => Aitem,
                        Is_Instance =>
                          Is_Generic_Instance (Defining_Entity (Context)));

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package declaration",
                        Aspect, Id);
                  end if;

                  goto Continue;
               end Abstract_State;

               --  Aspect Default_Internal_Condition is never delayed because
               --  it is equivalent to a source pragma which appears after the
               --  related private type. To deal with forward references, the
               --  generated pragma is stored in the rep chain of the related
               --  private type as types do not carry contracts. The pragma is
               --  wrapped inside of a procedure at the freeze point of the
               --  private type's full view.

               --  A type entity argument is appended to facilitate inheriting
               --  the aspect from parent types (see Build_DIC_Procedure_Body),
               --  though that extra argument isn't documented for the pragma.

               when Aspect_Default_Initial_Condition =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr)),
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent)),
                     Pragma_Name                  =>
                       Name_Default_Initial_Condition);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Default_Storage_Pool

               when Aspect_Default_Storage_Pool =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  =>
                       Name_Default_Storage_Pool);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Depends

               --  Aspect Depends is never delayed because it is equivalent to
               --  a source pragma which appears after the related subprogram.
               --  To deal with forward references, the generated pragma is
               --  stored in the contract of the related subprogram and later
               --  analyzed at the end of the declarative region. See routine
               --  Analyze_Depends_In_Decl_Part for details.

               when Aspect_Depends =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Depends);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Global

               --  Aspect Global is never delayed because it is equivalent to
               --  a source pragma which appears after the related subprogram.
               --  To deal with forward references, the generated pragma is
               --  stored in the contract of the related subprogram and later
               --  analyzed at the end of the declarative region. See routine
               --  Analyze_Global_In_Decl_Part for details.

               when Aspect_Global =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Global);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Initial_Condition

               --  Aspect Initial_Condition is never delayed because it is
               --  equivalent to a source pragma which appears after the
               --  related package. To deal with forward references, the
               --  generated pragma is stored in the contract of the related
               --  package and later analyzed at the end of the declarative
               --  region. See routine Analyze_Initial_Condition_In_Decl_Part
               --  for details.

               when Aspect_Initial_Condition => Initial_Condition : declare
                  Context : Node_Id := N;

               begin
                  --  When aspect Initial_Condition appears on a generic
                  --  package, it is propagated to the package instance. The
                  --  context in this case is the instance spec.

                  if Nkind (Context) = N_Package_Instantiation then
                     Context := Instance_Spec (Context);
                  end if;

                  if Nkind (Context) in N_Generic_Package_Declaration
                                      | N_Package_Declaration
                  then
                     Aitem := Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  =>
                          Name_Initial_Condition);

                     Decorate (Aspect, Aitem);
                     Insert_Pragma
                       (Prag        => Aitem,
                        Is_Instance =>
                          Is_Generic_Instance (Defining_Entity (Context)));

                  --  Otherwise the context is illegal

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package declaration",
                        Aspect, Id);
                  end if;

                  goto Continue;
               end Initial_Condition;

               --  Initializes

               --  Aspect Initializes is never delayed because it is equivalent
               --  to a source pragma appearing after the related package. To
               --  deal with forward references, the generated pragma is stored
               --  in the contract of the related package and later analyzed at
               --  the end of the declarative region. For details, see routine
               --  Analyze_Initializes_In_Decl_Part.

               when Aspect_Initializes => Initializes : declare
                  Context : Node_Id := N;

               begin
                  --  When aspect Initializes appears on a generic package,
                  --  it is propagated to the package instance. The context
                  --  in this case is the instance spec.

                  if Nkind (Context) = N_Package_Instantiation then
                     Context := Instance_Spec (Context);
                  end if;

                  if Nkind (Context) in N_Generic_Package_Declaration
                                      | N_Package_Declaration
                  then
                     Aitem := Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Initializes);

                     Decorate (Aspect, Aitem);
                     Insert_Pragma
                       (Prag        => Aitem,
                        Is_Instance =>
                          Is_Generic_Instance (Defining_Entity (Context)));

                  --  Otherwise the context is illegal

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package declaration",
                        Aspect, Id);
                  end if;

                  goto Continue;
               end Initializes;

               --  Max_Entry_Queue_Length

               when Aspect_Max_Entry_Queue_Length =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name => Name_Max_Entry_Queue_Length);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Max_Queue_Length

               when Aspect_Max_Queue_Length =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Max_Queue_Length);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Obsolescent

               when Aspect_Obsolescent => declare
                  Args : List_Id;

               begin
                  if No (Expr) then
                     Args := No_List;
                  else
                     Args := New_List (
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr)));
                  end if;

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => Args,
                     Pragma_Name                  => Name_Obsolescent);
               end;

               --  Part_Of

               when Aspect_Part_Of =>
                  if Nkind (N) in N_Object_Declaration
                                | N_Package_Instantiation
                    or else Is_Single_Concurrent_Type_Declaration (N)
                  then
                     Aitem := Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Part_Of);

                     Decorate (Aspect, Aitem);
                     Insert_Pragma (Aitem);

                  else
                     Error_Msg_NE
                       ("aspect & must apply to package instantiation, "
                        & "object, single protected type or single task type",
                        Aspect, Id);
                  end if;

                  goto Continue;

               --  SPARK_Mode

               when Aspect_SPARK_Mode =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_SPARK_Mode);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Refined_Depends

               --  Aspect Refined_Depends is never delayed because it is
               --  equivalent to a source pragma which appears in the
               --  declarations of the related subprogram body. To deal with
               --  forward references, the generated pragma is stored in the
               --  contract of the related subprogram body and later analyzed
               --  at the end of the declarative region. For details, see
               --  routine Analyze_Refined_Depends_In_Decl_Part.

               when Aspect_Refined_Depends =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Refined_Depends);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Refined_Global

               --  Aspect Refined_Global is never delayed because it is
               --  equivalent to a source pragma which appears in the
               --  declarations of the related subprogram body. To deal with
               --  forward references, the generated pragma is stored in the
               --  contract of the related subprogram body and later analyzed
               --  at the end of the declarative region. For details, see
               --  routine Analyze_Refined_Global_In_Decl_Part.

               when Aspect_Refined_Global =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Refined_Global);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Refined_Post

               when Aspect_Refined_Post =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Refined_Post);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Refined_State

               when Aspect_Refined_State =>

                  --  The corresponding pragma for Refined_State is inserted in
                  --  the declarations of the related package body. This action
                  --  synchronizes both the source and from-aspect versions of
                  --  the pragma.

                  if Nkind (N) = N_Package_Body then
                     Aitem := Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Refined_State);

                     Decorate (Aspect, Aitem);
                     Insert_Pragma (Aitem);

                  --  Otherwise the context is illegal

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package body", Aspect, Id);
                  end if;

                  goto Continue;

               --  Relative_Deadline

               when Aspect_Relative_Deadline =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                      Pragma_Name                 => Name_Relative_Deadline);

                  --  If the aspect applies to a task, the corresponding pragma
                  --  must appear within its declarations, not after.

                  if Nkind (N) = N_Task_Type_Declaration then
                     declare
                        Def : Node_Id;
                        V   : List_Id;

                     begin
                        if No (Task_Definition (N)) then
                           Set_Task_Definition (N,
                             Make_Task_Definition (Loc,
                                Visible_Declarations => New_List,
                                End_Label => Empty));
                        end if;

                        Def := Task_Definition (N);
                        V  := Visible_Declarations (Def);
                        if not Is_Empty_List (V) then
                           Insert_Before (First (V), Aitem);

                        else
                           Set_Visible_Declarations (Def, New_List (Aitem));
                        end if;

                        goto Continue;
                     end;
                  end if;

               --  Relaxed_Initialization

               when Aspect_Relaxed_Initialization =>
                  Analyze_Aspect_Relaxed_Initialization;
                  goto Continue;

               --  Secondary_Stack_Size

               --  Aspect Secondary_Stack_Size needs to be converted into a
               --  pragma for two reasons: the attribute is not analyzed until
               --  after the expansion of the task type declaration and the
               --  attribute does not have visibility on the discriminant.

               when Aspect_Secondary_Stack_Size =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  =>
                       Name_Secondary_Stack_Size);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  User_Aspect

               when Aspect_User_Aspect =>
                  Analyze_User_Aspect_Aspect_Specification (Aspect);
                  goto Continue;

               --  Case 2e: Annotate aspect

               when Aspect_Annotate | Aspect_GNAT_Annotate =>
                  declare
                     Args  : List_Id;
                     Pargs : List_Id;
                     Arg   : Node_Id;

                  begin
                     --  The argument can be a single identifier

                     if Nkind (Expr) = N_Identifier then

                        --  One level of parens is allowed

                        if Paren_Count (Expr) > 1 then
                           Error_Msg_F ("extra parentheses ignored", Expr);
                        end if;

                        Set_Paren_Count (Expr, 0);

                        --  Add the single item to the list

                        Args := New_List (Expr);

                     --  Otherwise we must have an aggregate

                     elsif Nkind (Expr) = N_Aggregate then

                        --  Must be positional

                        if Present (Component_Associations (Expr)) then
                           Error_Msg_F
                             ("purely positional aggregate required", Expr);
                           goto Continue;
                        end if;

                        --  Must not be parenthesized

                        if Paren_Count (Expr) /= 0 then
                           Error_Msg_F -- CODEFIX
                             ("redundant parentheses", Expr);
                        end if;

                        --  List of arguments is list of aggregate expressions

                        Args := Expressions (Expr);

                     --  Anything else is illegal

                     else
                        Error_Msg_F ("wrong form for Annotate aspect", Expr);
                        goto Continue;
                     end if;

                     --  Prepare pragma arguments

                     Pargs := New_List;
                     Arg := First (Args);
                     while Present (Arg) loop
                        Append_To (Pargs,
                          Make_Pragma_Argument_Association (Sloc (Arg),
                            Expression => Relocate_Node (Arg)));
                        Next (Arg);
                     end loop;

                     Append_To (Pargs,
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Chars      => Name_Entity,
                         Expression => Ent));

                     Aitem := Make_Aitem_Pragma
                       (Pragma_Argument_Associations => Pargs,
                        Pragma_Name                  => Name_Annotate);
                  end;

               --  Case 3 : Aspects that don't correspond to pragma/attribute
               --  definition clause.

               --  Case 3a: The aspects listed below don't correspond to
               --  pragmas/attributes but do require delayed analysis.

               when Aspect_Default_Value | Aspect_Default_Component_Value =>
                  Error_Msg_Name_1 := Nam;

                  if not Is_Type (E) then
                     Error_Msg_N ("aspect% can only apply to a type", Id);
                     goto Continue;

                  elsif not Is_First_Subtype (E) then
                     Error_Msg_N ("aspect% cannot apply to subtype", Id);
                     goto Continue;

                  elsif A_Id = Aspect_Default_Value then
                     if not Is_Scalar_Type (E) then
                        Error_Msg_N
                          ("aspect% can only be applied to scalar type", Id);
                        goto Continue;
                     end if;

                  elsif A_Id = Aspect_Default_Component_Value then
                     if not Is_Array_Type (E) then
                        Error_Msg_N
                          ("aspect% can only be applied to array type", Id);
                        goto Continue;

                     elsif not Is_Scalar_Type (Component_Type (E)) then
                        Error_Msg_N ("aspect% requires scalar components", Id);
                        goto Continue;
                     end if;
                  end if;

               when Aspect_Aggregate =>
                  --  We will be checking that the aspect is not specified on
                  --  an array type in Analyze_Aspects_At_Freeze_Point.

                  Validate_Aspect_Aggregate (Expr);

               when Aspect_Stable_Properties =>
                  Validate_Aspect_Stable_Properties
                    (E, Expr, Class_Present => Class_Present (Aspect));

               when Aspect_Designated_Storage_Model =>
                  if not All_Extensions_Allowed then
                     Error_Msg_GNAT_Extension ("aspect %", Loc);
                     goto Continue;

                  elsif not Is_Type (E)
                    or else Ekind (E) /= E_Access_Type
                  then
                     Error_Msg_N
                       ("can only be specified for pool-specific access type",
                        Aspect);
                     goto Continue;
                  end if;

               when Aspect_Storage_Model_Type =>
                  if not All_Extensions_Allowed then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_GNAT_Extension ("aspect %", Loc);
                     goto Continue;

                  elsif not Is_Type (E)
                    or else not Is_Immutably_Limited_Type (E)
                  then
                     Error_Msg_N
                       ("can only be specified for immutably limited type",
                        Aspect);
                     goto Continue;
                  end if;

               when Aspect_Finalizable =>
                  if not All_Extensions_Allowed then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_GNAT_Extension ("aspect %", Loc);
                     goto Continue;

                  elsif not Is_Type (E) then
                     Error_Msg_N ("can only be specified for a type", Aspect);
                     goto Continue;
                  end if;

               when Aspect_Integer_Literal
                  | Aspect_Real_Literal
                  | Aspect_String_Literal
               =>

                  if not Is_First_Subtype (E) then
                     Error_Msg_N
                       ("may only be specified for a first subtype", Aspect);
                     goto Continue;
                  end if;

                  if Ada_Version < Ada_2022 then
                     Check_Restriction
                       (No_Implementation_Aspect_Specifications, N);
                  end if;

               --  Case 3b: The aspects listed below don't correspond to
               --  pragmas/attributes and don't need delayed analysis.

               --  Implicit_Dereference

               --  Only the legality checks are done during the analysis, thus
               --  no delay is required.

               when Aspect_Implicit_Dereference =>
                  Analyze_Aspect_Implicit_Dereference;
                  goto Continue;

               --  Dimension

               when Aspect_Dimension =>
                  Analyze_Aspect_Dimension (N, Id, Expr);
                  goto Continue;

               --  Dimension_System

               when Aspect_Dimension_System =>
                  Analyze_Aspect_Dimension_System (N, Id, Expr);
                  goto Continue;

               when Aspect_Local_Restrictions =>
                  Validate_Aspect_Local_Restrictions (E, Expr);
                  Record_Rep_Item (E, Aspect);
                  goto Continue;

               --  Case 4: Aspects requiring special handling

               --  Pre/Post/Test_Case/Contract_Cases/Always_Terminates/
               --  Exceptional_Cases and Subprogram_Variant whose corresponding
               --  pragmas take care of the delay.

               --  Pre/Post

               --  Aspects Pre/Post generate Precondition/Postcondition pragmas
               --  with a first argument that is the expression, and a second
               --  argument that is an informative message if the test fails.
               --  This is inserted right after the declaration, to get the
               --  required pragma placement. The processing for the pragmas
               --  takes care of the required delay.

               when Pre_Post_Aspects => Pre_Post : declare
                  Pname : Name_Id;

               begin
                  if A_Id in Aspect_Pre | Aspect_Precondition then
                     Pname := Name_Precondition;
                  else
                     Pname := Name_Postcondition;
                  end if;

                  --  Check that the class-wide predicate cannot be applied to
                  --  an operation of a synchronized type. AI12-0182 forbids
                  --  these altogether, while earlier language semantics made
                  --  them legal on tagged synchronized types.

                  --  Other legality checks are performed when analyzing the
                  --  contract of the operation.

                  if Class_Present (Aspect)
                    and then Is_Concurrent_Type (Current_Scope)
                    and then Ekind (E) in E_Entry | E_Function | E_Procedure
                  then
                     Error_Msg_Name_1 := Original_Aspect_Pragma_Name (Aspect);
                     Error_Msg_N
                       ("aspect % can only be specified for a primitive "
                        & "operation of a tagged type", Aspect);

                     goto Continue;
                  end if;

                  --  Remember class-wide conditions; they will be merged
                  --  with inherited conditions.

                  if Class_Present (Aspect)
                    and then A_Id in Aspect_Pre | Aspect_Post
                    and then Is_Subprogram (E)
                    and then not Is_Ignored_Ghost_Entity (E)
                  then
                     if A_Id = Aspect_Pre then
                        if Is_Ignored (Aspect) then
                           Set_Ignored_Class_Preconditions (E,
                             New_Copy_Tree (Expr));
                        else
                           Set_Class_Preconditions (E, New_Copy_Tree (Expr));
                        end if;

                     --  Postconditions may split into separate aspects, and we
                     --  remember the expression before such split (i.e. when
                     --  the first postcondition is processed).

                     elsif No (Class_Postconditions (E))
                       and then No (Ignored_Class_Postconditions (E))
                     then
                        if Is_Ignored (Aspect) then
                           Set_Ignored_Class_Postconditions (E,
                             New_Copy_Tree (Expr));
                        else
                           Set_Class_Postconditions (E, New_Copy_Tree (Expr));
                        end if;
                     end if;
                  end if;

                  --  Build the precondition/postcondition pragma

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Eloc,
                         Chars      => Name_Check,
                         Expression => Relocate_Expression (Expr))),
                       Pragma_Name                => Pname);

                  Set_Is_Delayed_Aspect (Aspect);

                  --  For Pre/Post cases, insert immediately after the entity
                  --  declaration, since that is the required pragma placement.
                  --  Note that for these aspects, we do not have to worry
                  --  about delay issues, since the pragmas themselves deal
                  --  with delay of visibility for the expression analysis.

                  Insert_Pragma (Aitem);

                  goto Continue;
               end Pre_Post;

               --  Test_Case

               when Aspect_Test_Case => Test_Case : declare
                  Args      : List_Id;
                  Comp_Expr : Node_Id;
                  Comp_Assn : Node_Id;

               begin
                  Args := New_List;

                  if Nkind (Parent (N)) = N_Compilation_Unit then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_N ("incorrect placement of aspect %", E);
                     goto Continue;
                  end if;

                  if Nkind (Expr) /= N_Aggregate
                    or else Null_Record_Present (Expr)
                  then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_NE
                       ("wrong syntax for aspect % for &", Id, E);
                     goto Continue;
                  end if;

                  --  Check that the expression is a proper aggregate (no
                  --  parentheses).

                  if Paren_Count (Expr) /= 0 then
                     Error_Msg_F -- CODEFIX
                       ("redundant parentheses", Expr);
                     goto Continue;
                  end if;

                  --  Create the list of arguments for building the Test_Case
                  --  pragma.

                  Comp_Expr := First (Expressions (Expr));
                  while Present (Comp_Expr) loop
                     Append_To (Args,
                       Make_Pragma_Argument_Association (Sloc (Comp_Expr),
                         Expression => Relocate_Node (Comp_Expr)));
                     Next (Comp_Expr);
                  end loop;

                  Comp_Assn := First (Component_Associations (Expr));
                  while Present (Comp_Assn) loop
                     if List_Length (Choices (Comp_Assn)) /= 1
                       or else
                         Nkind (First (Choices (Comp_Assn))) /= N_Identifier
                     then
                        Error_Msg_Name_1 := Nam;
                        Error_Msg_NE
                          ("wrong syntax for aspect % for &", Id, E);
                        goto Continue;
                     end if;

                     Append_To (Args,
                       Make_Pragma_Argument_Association (Sloc (Comp_Assn),
                         Chars      => Chars (First (Choices (Comp_Assn))),
                         Expression =>
                           Relocate_Node (Expression (Comp_Assn))));
                     Next (Comp_Assn);
                  end loop;

                  --  Build the test-case pragma

                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => Args,
                     Pragma_Name                  => Name_Test_Case);
               end Test_Case;

               --  Contract_Cases

               when Aspect_Contract_Cases =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Contract_Cases);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Exceptional_Cases

               when Aspect_Exceptional_Cases =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Exceptional_Cases);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Subprogram_Variant

               when Aspect_Subprogram_Variant =>
                  Aitem := Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Subprogram_Variant);

                  Decorate (Aspect, Aitem);
                  Insert_Pragma (Aitem);
                  goto Continue;

               --  Case 5: Special handling for aspects with an optional
               --  boolean argument.

               --  In the delayed case, the corresponding pragma cannot be
               --  generated yet because the evaluation of the boolean needs
               --  to be delayed till the freeze point.

               when Boolean_Aspects
                  | Library_Unit_Aspects
               =>
                  Set_Is_Boolean_Aspect (Aspect);

                  --  Lock_Free aspect only apply to protected objects

                  if A_Id = Aspect_Lock_Free then
                     if Ekind (E) /= E_Protected_Type then
                        Error_Msg_Name_1 := Nam;
                        Error_Msg_N
                          ("aspect % only applies to a protected type " &
                           "or object",
                           Aspect);

                     else
                        --  Set the Uses_Lock_Free flag to True if there is no
                        --  expression or if the expression is True. The
                        --  evaluation of this aspect should be delayed to the
                        --  freeze point if we wanted to handle the corner case
                        --  of "true" or "false" being redefined.

                        if No (Expr)
                          or else Is_True (Static_Boolean (Expr))
                        then
                           Set_Uses_Lock_Free (E);
                        end if;

                        Record_Rep_Item (E, Aspect);
                     end if;

                     goto Continue;

                  elsif A_Id in Aspect_Export | Aspect_Import then
                     Analyze_Aspect_Export_Import;

                  --  Disable_Controlled

                  elsif A_Id = Aspect_Disable_Controlled then
                     Analyze_Aspect_Disable_Controlled;
                     goto Continue;

                  --  Ada 2022 (AI12-0129): Exclusive_Functions

                  elsif A_Id = Aspect_Exclusive_Functions then
                     if Ekind (E) /= E_Protected_Type then
                        Error_Msg_Name_1 := Nam;
                        Error_Msg_N
                          ("aspect % only applies to a protected type " &
                           "or object",
                           Aspect);
                     end if;

                     goto Continue;

                  --  Ada 2022 (AI12-0363): Full_Access_Only

                  elsif A_Id = Aspect_Full_Access_Only then
                     Error_Msg_Ada_2022_Feature ("aspect %", Loc);

                  --  No_Controlled_Parts, No_Task_Parts

                  elsif A_Id in Aspect_No_Controlled_Parts
                              | Aspect_No_Task_Parts
                  then
                     Error_Msg_Name_1 := Nam;

                     --  Disallow formal types

                     if Nkind (Original_Node (N)) = N_Formal_Type_Declaration
                     then
                        Error_Msg_N
                          ("aspect % not allowed for formal type declaration",
                           Aspect);

                     --  Disallow subtypes

                     elsif Nkind (Original_Node (N)) = N_Subtype_Declaration
                     then
                        Error_Msg_N
                          ("aspect % not allowed for subtype declaration",
                           Aspect);

                     --  Accept all other types

                     elsif not Is_Type (E) then
                        Error_Msg_N
                          ("aspect % can only be specified for a type",
                           Aspect);
                     end if;

                     --  Resolve the expression to a boolean

                     if Present (Expr) then
                        Check_Expr_Is_OK_Static_Expression (Expr, Any_Boolean);
                     end if;

                     goto Continue;

                  --  Ada 2022 (AI12-0075): static expression functions

                  elsif A_Id = Aspect_Static then
                     Analyze_Aspect_Static;
                     goto Continue;

                  --  Ada 2022 (AI12-0279)

                  elsif A_Id = Aspect_Yield then
                     Analyze_Aspect_Yield;
                     goto Continue;

                  --  Handle Boolean aspects equivalent to source pragmas which
                  --  appears after the related object declaration.

                  elsif A_Id in Aspect_Always_Terminates
                              | Aspect_Async_Readers
                              | Aspect_Async_Writers
                              | Aspect_Constant_After_Elaboration
                              | Aspect_Effective_Reads
                              | Aspect_Effective_Writes
                              | Aspect_Extensions_Visible
                              | Aspect_Ghost
                              | Aspect_No_Caching
                              | Aspect_Side_Effects
                              | Aspect_Volatile_Function
                  then
                     Aitem :=
                       Make_Aitem_Pragma
                         (Pragma_Argument_Associations => New_List (
                            Make_Pragma_Argument_Association (Loc,
                              Expression => Relocate_Node (Expr))),
                          Pragma_Name                  => Nam);
                     Decorate (Aspect, Aitem);
                     Insert_Pragma (Aitem);
                     goto Continue;
                  end if;

                  --  Library unit aspects require special handling in the case
                  --  of a package declaration, the pragma needs to be inserted
                  --  in the list of declarations for the associated package.
                  --  There is no issue of visibility delay for these aspects.

                  if A_Id in Library_Unit_Aspects
                    and then
                      Nkind (N) in N_Package_Declaration
                                 | N_Generic_Package_Declaration
                    and then Nkind (Parent (N)) /= N_Compilation_Unit

                    --  Aspect is legal on a local instantiation of a library-
                    --  level generic unit.

                    and then not Is_Generic_Instance (Defining_Entity (N))
                  then
                     Error_Msg_N
                       ("incorrect context for library unit aspect&", Id);
                     goto Continue;
                  end if;

                  --  Cases where we do not delay

                  if not Delay_Required then

                     --  Exclude aspects Export and Import because their pragma
                     --  syntax does not map directly to a Boolean aspect.

                     if A_Id not in Aspect_Export | Aspect_Import then
                        Aitem := Make_Aitem_Pragma
                          (Pragma_Argument_Associations => New_List (
                             Make_Pragma_Argument_Association (Sloc (Ent),
                               Expression => Ent)),
                           Pragma_Name                  => Nam);
                     end if;

                     --  Minimum check of First_Controlling_Parameter aspect;
                     --  the checks shared by the aspect and its corresponding
                     --  pragma are performed when the pragma is analyzed.

                     if A_Id = Aspect_First_Controlling_Parameter then
                        if Present (Expr) then
                           Analyze (Expr);
                        end if;

                        if (No (Expr) or else Entity (Expr) = Standard_True)
                          and then not Core_Extensions_Allowed
                        then
                           Error_Msg_GNAT_Extension
                             ("'First_'Controlling_'Parameter", Sloc (Aspect),
                              Is_Core_Extension => True);
                           goto Continue;
                        end if;

                        if not (Is_Type (E)
                                  and then
                                    (Is_Tagged_Type (E)
                                       or else Is_Concurrent_Type (E)))
                        then
                           Error_Msg_N
                             ("aspect 'First_'Controlling_'Parameter can only "
                              & "apply to tagged type or concurrent type",
                              Aspect);
                           goto Continue;
                        end if;

                        if Present (Expr)
                          and then Entity (Expr) = Standard_False
                        then
                           --  If the aspect is specified for a derived type,
                           --  the specified value shall be confirming.

                           if Is_Derived_Type (E)
                             and then Has_First_Controlling_Parameter_Aspect
                                        (Etype (E))
                           then
                              Error_Msg_Name_1 := Nam;
                              Error_Msg_N
                                ("specification of inherited True value for "
                                   & "aspect% can only confirm parent value",
                                 Id);
                           end if;

                           goto Continue;
                        end if;

                        --  Given that the aspect has been explicitly given,
                        --  we take note to avoid checking for its implicit
                        --  inheritance (see Analyze_Full_Type_Declaration).

                        Set_Has_First_Controlling_Parameter_Aspect (E);
                     end if;

                  --  In general cases, the corresponding pragma/attribute
                  --  definition clause will be inserted later at the freezing
                  --  point, and we do not need to build it now.

                  else
                     Aitem := Empty;
                  end if;

               --  Storage_Size

               --  This is special because for access types we need to generate
               --  an attribute definition clause. This also works for single
               --  task declarations, but it does not work for task type
               --  declarations, because we have the case where the expression
               --  references a discriminant of the task type. That can't use
               --  an attribute definition clause because we would not have
               --  visibility on the discriminant. For that case we must
               --  generate a pragma in the task definition.

               when Aspect_Storage_Size =>

                  --  Task type case

                  if Ekind (E) = E_Task_Type then
                     declare
                        Decl : constant Node_Id := Declaration_Node (E);

                     begin
                        pragma Assert (Nkind (Decl) = N_Task_Type_Declaration);

                        --  If no task definition, create one

                        if No (Task_Definition (Decl)) then
                           Set_Task_Definition (Decl,
                             Make_Task_Definition (Loc,
                               Visible_Declarations => Empty_List,
                               End_Label            => Empty));
                        end if;

                        --  Create a pragma and put it at the start of the task
                        --  definition for the task type declaration.

                        Aitem := Make_Aitem_Pragma
                          (Pragma_Argument_Associations => New_List (
                             Make_Pragma_Argument_Association (Loc,
                               Expression => Relocate_Node (Expr))),
                           Pragma_Name                  => Name_Storage_Size);

                        Prepend
                          (Aitem,
                           Visible_Declarations (Task_Definition (Decl)));
                        goto Continue;
                     end;

                  --  All other cases, generate attribute definition

                  else
                     Aitem :=
                       Make_Attribute_Definition_Clause (Loc,
                         Name       => Ent,
                         Chars      => Name_Storage_Size,
                         Expression => Relocate_Node (Expr));
                  end if;

               when Aspect_External_Initialization =>
                  Error_Msg_GNAT_Extension
                    ("External_Initialization aspect", Sloc (Aspect));

                  --  The External_Initialization aspect specifications that
                  --  are attached to object declarations were already
                  --  processed and detached from the list at an earlier stage,
                  --  so we can only get here if the specification is not in an
                  --  appropriate place.

                  Error_Msg_N
                    ("External_Initialization aspect can only be specified " &
                     "for object declarations", Aspect);
            end case;

            --  Attach the corresponding pragma/attribute definition clause to
            --  the aspect specification node.

            if Present (Aitem) then
               Set_From_Aspect_Specification (Aitem);
            end if;

            --  For an aspect that applies to a type, indicate whether it
            --  appears on a partial view of the type.

            if Is_Type (E) and then Is_Private_Type (E) then
               Set_Aspect_On_Partial_View (Aspect);
            end if;

            --  In the context of a compilation unit, we directly put the
            --  pragma in the Pragmas_After list of the N_Compilation_Unit_Aux
            --  node (no delay is required here) except for aspects on a
            --  subprogram body (see below) and a generic package, for which we
            --  need to introduce the pragma before building the generic copy
            --  (see sem_ch12), and for package instantiations, where the
            --  library unit pragmas are better handled early.

            if Nkind (Parent (N)) = N_Compilation_Unit
              and then (Present (Aitem) or else Is_Boolean_Aspect (Aspect))
            then
               declare
                  Aux : constant Node_Id := Aux_Decls_Node (Parent (N));

               begin
                  pragma Assert (Nkind (Aux) = N_Compilation_Unit_Aux);

                  --  For a Boolean aspect, create the corresponding pragma if
                  --  no expression or if the value is True.

                  if Is_Boolean_Aspect (Aspect) and then No (Aitem) then
                     if Is_True (Static_Boolean (Expr)) then
                        Aitem := Make_Aitem_Pragma
                          (Pragma_Argument_Associations => New_List (
                             Make_Pragma_Argument_Association (Sloc (Ent),
                               Expression => Ent)),
                           Pragma_Name                  => Nam);

                        Set_From_Aspect_Specification (Aitem, True);
                        Set_Corresponding_Aspect (Aitem, Aspect);

                     else
                        goto Continue;
                     end if;
                  end if;

                  --  If the aspect is on a subprogram body (relevant aspect
                  --  is Inline), add the pragma in front of the declarations.

                  if Nkind (N) = N_Subprogram_Body then
                     if No (Declarations (N)) then
                        Set_Declarations (N, New_List);
                     end if;

                     Prepend (Aitem, Declarations (N));

                  elsif Nkind (N) = N_Generic_Package_Declaration then
                     if No (Visible_Declarations (Specification (N))) then
                        Set_Visible_Declarations (Specification (N), New_List);
                     end if;

                     Prepend (Aitem,
                       Visible_Declarations (Specification (N)));

                  elsif Nkind (N) = N_Package_Instantiation then
                     declare
                        Spec : constant Node_Id :=
                                 Specification (Instance_Spec (N));
                     begin
                        if No (Visible_Declarations (Spec)) then
                           Set_Visible_Declarations (Spec, New_List);
                        end if;

                        Prepend (Aitem, Visible_Declarations (Spec));
                     end;

                  else
                     if No (Pragmas_After (Aux)) then
                        Set_Pragmas_After (Aux, New_List);
                     end if;

                     Append (Aitem, Pragmas_After (Aux));
                  end if;

                  goto Continue;
               end;
            end if;

            --  The evaluation of the aspect is delayed to the freezing point.
            --  The pragma or attribute clause if there is one is then attached
            --  to the aspect specification which is put in the rep item list.

            if Delay_Required then
               if Present (Aitem) then
                  Set_Is_Delayed_Aspect (Aitem);
                  Set_Aspect_Rep_Item (Aspect, Aitem);
                  Set_Parent (Aitem, Aspect);
               end if;

               Set_Is_Delayed_Aspect (Aspect);

               --  In the case of Default_Value, link the aspect to base type
               --  as well, even though it appears on a first subtype. This is
               --  mandated by the semantics of the aspect. Do not establish
               --  the link when processing the base type itself as this leads
               --  to a rep item circularity.

               if A_Id = Aspect_Default_Value and then Base_Type (E) /= E then
                  Set_Has_Delayed_Aspects (Base_Type (E));
                  Record_Rep_Item (Base_Type (E), Aspect);
               end if;

               Set_Has_Delayed_Aspects (E);
               Record_Rep_Item (E, Aspect);

            --  When delay is not required and the context is a package or a
            --  subprogram body, insert the pragma in the body declarations.

            elsif Nkind (N) in N_Package_Body | N_Subprogram_Body then
               if No (Declarations (N)) then
                  Set_Declarations (N, New_List);
               end if;

               --  The pragma is added before source declarations

               Prepend_To (Declarations (N), Aitem);

            --  When delay is not required and the context is not a compilation
            --  unit, we simply insert the pragma/attribute definition clause
            --  in sequence.

            elsif Present (Aitem) then
               Insert_After (Ins_Node, Aitem);
               Ins_Node := Aitem;
            end if;

            <<Continue>>

            --  If a nonoverridable aspect is explicitly specified for a
            --  derived type, then check consistency with the parent type.

            if A_Id in Nonoverridable_Aspect_Id
              and then Nkind (N) = N_Full_Type_Declaration
              and then Nkind (Type_Definition (N)) = N_Derived_Type_Definition
              and then not In_Instance_Body
            then
               --  In order to locate the parent type we must go first to its
               --  base type because the frontend introduces an implicit base
               --  type even if there is no constraint attached to it, since
               --  this is closer to the Ada semantics.

               declare
                  Parent_Type      : constant Entity_Id :=
                    Etype (Base_Type (E));
                  Inherited_Aspect : constant Node_Id :=
                    Find_Aspect (Parent_Type, A_Id);
               begin
                  if Present (Inherited_Aspect)
                    and then not Is_Confirming
                                   (A_Id, Inherited_Aspect, Aspect)
                  then
                     Error_Msg_Name_1 := Aspect_Names (A_Id);
                     Error_Msg_Sloc := Sloc (Inherited_Aspect);

                     Error_Msg_N
                       ("overriding aspect specification for "
                          & "nonoverridable aspect % does not confirm "
                          & "aspect specification inherited from #",
                        Aspect);
                  end if;
               end;
            end if;

         exception
            when Aspect_Exit => null;
         end Analyze_One_Aspect;

         Next (Aspect);
      end loop Aspect_Loop;

      if Has_Delayed_Aspects (E) then
         Ensure_Freeze_Node (E);
      end if;
   end Analyze_Aspect_Specifications;

   ------------------------------------------------
   -- Analyze_Aspects_On_Subprogram_Body_Or_Stub --
   ------------------------------------------------

   procedure Analyze_Aspects_On_Subprogram_Body_Or_Stub (N : Node_Id) is
      Body_Id : constant Entity_Id := Defining_Entity (N);

      procedure Diagnose_Misplaced_Aspects (Spec_Id : Entity_Id);
      --  Body [stub] N has aspects, but they are not properly placed. Emit an
      --  error message depending on the aspects involved. Spec_Id denotes the
      --  entity of the corresponding spec.

      --------------------------------
      -- Diagnose_Misplaced_Aspects --
      --------------------------------

      procedure Diagnose_Misplaced_Aspects (Spec_Id : Entity_Id) is
         procedure Misplaced_Aspect_Error
           (Asp     : Node_Id;
            Ref_Nam : Name_Id);
         --  Emit an error message concerning misplaced aspect Asp. Ref_Nam is
         --  the name of the refined version of the aspect.

         ----------------------------
         -- Misplaced_Aspect_Error --
         ----------------------------

         procedure Misplaced_Aspect_Error
           (Asp     : Node_Id;
            Ref_Nam : Name_Id)
         is
            Asp_Nam : constant Name_Id   := Chars (Identifier (Asp));
            Asp_Id  : constant Aspect_Id := Get_Aspect_Id (Asp_Nam);

         begin
            --  The corresponding spec already contains the aspect in question
            --  and the one appearing on the body must be the refined form:

            --    procedure P with Global ...;
            --    procedure P with Global ... is ... end P;
            --                     ^
            --                     Refined_Global

            if Has_Aspect (Spec_Id, Asp_Id) then
               Error_Msg_Name_1 := Asp_Nam;

               --  Subunits cannot carry aspects that apply to a subprogram
               --  declaration.

               if Nkind (Parent (N)) = N_Subunit then
                  Error_Msg_N ("aspect % cannot apply to a subunit", Asp);

               --  Otherwise suggest the refined form

               else
                  Error_Msg_Name_2 := Ref_Nam;
                  Error_Msg_N ("aspect % should be %", Asp);
               end if;

            --  Otherwise the aspect must appear on the spec, not on the body

            --    procedure P;
            --    procedure P with Global ... is ... end P;

            else
               Error_Msg_N
                 ("aspect specification must appear on initial declaration",
                  Asp);
            end if;
         end Misplaced_Aspect_Error;

         --  Local variables

         Asp     : Node_Id;
         Asp_Nam : Name_Id;

      --  Start of processing for Diagnose_Misplaced_Aspects

      begin
         --  Iterate over the aspect specifications and emit specific errors
         --  where applicable.

         Asp := First (Aspect_Specifications (N));
         while Present (Asp) loop
            Asp_Nam := Chars (Identifier (Asp));

            --  Do not emit errors on aspects that can appear on a subprogram
            --  body. This scenario occurs when the aspect specification list
            --  contains both misplaced and properly placed aspects.

            if Aspect_On_Body_Or_Stub_OK (Get_Aspect_Id (Asp_Nam)) then
               null;

            --  Special diagnostics for SPARK aspects

            elsif Asp_Nam = Name_Depends then
               Misplaced_Aspect_Error (Asp, Name_Refined_Depends);

            elsif Asp_Nam = Name_Global then
               Misplaced_Aspect_Error (Asp, Name_Refined_Global);

            elsif Asp_Nam = Name_Post then
               Misplaced_Aspect_Error (Asp, Name_Refined_Post);

            --  Otherwise a language-defined aspect is misplaced

            else
               Error_Msg_N
                 ("aspect specification must appear on initial declaration",
                  Asp);
            end if;

            Next (Asp);
         end loop;
      end Diagnose_Misplaced_Aspects;

      --  Local variables

      Spec_Id : constant Entity_Id := Unique_Defining_Entity (N);

   --  Start of processing for Analyze_Aspects_On_Subprogram_Body_Or_Stub

   begin
      --  Language-defined aspects cannot be associated with a subprogram body
      --  [stub] if the subprogram has a spec. Certain implementation defined
      --  aspects are allowed to break this rule (for all applicable cases, see
      --  table Aspects.Aspect_On_Body_Or_Stub_OK).

      if  Spec_Id /= Body_Id
         and then Has_Aspects (N)
         and then not Aspects_On_Body_Or_Stub_OK (N)
      then
         Diagnose_Misplaced_Aspects (Spec_Id);
      else
         Analyze_Aspect_Specifications (N, Body_Id);
      end if;
   end Analyze_Aspects_On_Subprogram_Body_Or_Stub;

   -----------------------
   -- Analyze_At_Clause --
   -----------------------

   --  An at clause is replaced by the corresponding Address attribute
   --  definition clause that is the preferred approach in Ada 95.

   procedure Analyze_At_Clause (N : Node_Id) is
      CS : constant Boolean := Comes_From_Source (N);

   begin
      --  This is an obsolescent feature

      Check_Restriction (No_Obsolescent_Features, N);

      if Warn_On_Obsolescent_Feature then
         Error_Msg_N
           ("?j?at clause is an obsolescent feature (RM J.7(2))", N);
         Error_Msg_N
           ("\?j?use address attribute definition clause instead", N);
      end if;

      --  Rewrite as address clause

      Rewrite (N,
        Make_Attribute_Definition_Clause (Sloc (N),
          Name       => Identifier (N),
          Chars      => Name_Address,
          Expression => Expression (N)));

      --  We preserve Comes_From_Source, since logically the clause still comes
      --  from the source program even though it is changed in form.

      Set_Comes_From_Source (N, CS);

      --  Analyze rewritten clause

      Analyze_Attribute_Definition_Clause (N);
   end Analyze_At_Clause;

   -----------------------------------------
   -- Analyze_Attribute_Definition_Clause --
   -----------------------------------------

   procedure Analyze_Attribute_Definition_Clause (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Nam   : constant Node_Id      := Name (N);
      Attr  : constant Name_Id      := Chars (N);
      Expr  : constant Node_Id      := Expression (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Attr);

      Ent : Entity_Id;
      --  The entity of Nam after it is analyzed. In the case of an incomplete
      --  type, this is the underlying type.

      U_Ent : Entity_Id;
      --  The underlying entity to which the attribute applies. Generally this
      --  is the Underlying_Type of Ent, except in the case where the clause
      --  applies to the full view of an incomplete or private type, in which
      --  case U_Ent is just a copy of Ent.

      FOnly : Boolean := False;
      --  Reset to True for subtype specific attribute (Alignment, Size)
      --  and for stream attributes, i.e. those cases where in the call to
      --  Rep_Item_Too_Late, FOnly is set True so that only the freezing rules
      --  are checked. Note that the case of stream attributes is not clear
      --  from the RM, but see AI95-00137. Also, the RM seems to disallow
      --  Storage_Size for derived task types, but that is also clearly
      --  unintentional.

      procedure Analyze_Put_Image_TSS_Definition;

      procedure Analyze_Stream_TSS_Definition (TSS_Nam : TSS_Name_Type);
      --  Common processing for 'Read, 'Write, 'Input and 'Output attribute
      --  definition clauses.

      function Duplicate_Clause return Boolean;
      --  This routine checks if the aspect for U_Ent being given by attribute
      --  definition clause N is for an aspect that has already been specified,
      --  and if so gives an error message. If there is a duplicate, True is
      --  returned, otherwise there is no error, and False is returned. Size
      --  and Value_Size are considered to conflict, but for compatibility,
      --  this is merely a warning.

      procedure Check_Indexing_Functions;
      --  Check that the function in Constant_Indexing or Variable_Indexing
      --  attribute has the proper type structure. If the name is overloaded,
      --  check that some interpretation is legal.

      procedure Check_Iterator_Functions;
      --  Check that there is a single function in Default_Iterator attribute
      --  that has the proper type structure.

      function Check_Primitive_Function (Subp : Entity_Id) return Boolean;
      --  Common legality check for the previous two

      -----------------------------------
      -- Analyze_Put_Image_TSS_Definition --
      -----------------------------------

      procedure Analyze_Put_Image_TSS_Definition is
         Subp : Entity_Id := Empty;
         I    : Interp_Index;
         It   : Interp;
         Pnam : Entity_Id;

         function Has_Good_Profile
           (Subp   : Entity_Id;
            Report : Boolean := False) return Boolean;
         --  Return true if the entity is a subprogram with an appropriate
         --  profile for the attribute being defined. If result is False and
         --  Report is True, function emits appropriate error.

         ----------------------
         -- Has_Good_Profile --
         ----------------------

         function Has_Good_Profile
           (Subp   : Entity_Id;
            Report : Boolean := False) return Boolean
         is
            F              : Entity_Id;
            Typ            : Entity_Id;

         begin
            if Ekind (Subp) /= E_Procedure then
               return False;
            end if;

            F := First_Formal (Subp);

            if No (F) then
               return False;
            end if;

            if Base_Type (Etype (F))
              /= Class_Wide_Type (RTE (RE_Root_Buffer_Type))
            then
               if Report then
                  Error_Msg_N
                    ("wrong type for Put_Image procedure''s first parameter",
                     Parameter_Type (Parent (F)));
               end if;

               return False;
            end if;

            if Parameter_Mode (F) /= E_In_Out_Parameter then
               if Report then
                  Error_Msg_N
                    ("wrong mode for Put_Image procedure''s first parameter",
                     Parent (F));
               end if;

               return False;
            end if;

            Next_Formal (F);

            Typ := Etype (F);

            --  Verify that the prefix of the attribute and the local name for
            --  the type of the formal match.

            if Base_Type (Typ) /= Base_Type (Ent) then
               if Report then
                  Error_Msg_N
                    ("wrong type for Put_Image procedure''s second parameter",
                     Parameter_Type (Parent (F)));
               end if;

               return False;
            end if;

            if Parameter_Mode (F) /= E_In_Parameter then
               if Report then
                  Error_Msg_N
                    ("wrong mode for Put_Image procedure''s second parameter",
                     Parent (F));
               end if;

               return False;
            end if;

            if Present (Next_Formal (F)) then
               return False;
            end if;

            return True;
         end Has_Good_Profile;

      --  Start of processing for Analyze_Put_Image_TSS_Definition

      begin
         if not Is_Type (U_Ent) then
            Error_Msg_N ("local name must be a subtype", Nam);
            return;

         elsif not Is_First_Subtype (U_Ent) then
            Error_Msg_N ("local name must be a first subtype", Nam);
            return;
         end if;

         Pnam := TSS (Base_Type (U_Ent), TSS_Put_Image);

         --  If Pnam is present, it can be either inherited from an ancestor
         --  type (in which case it is legal to redefine it for this type), or
         --  be a previous definition of the attribute for the same type (in
         --  which case it is illegal).

         --  In the first case, it will have been analyzed already, and we can
         --  check that its profile does not match the expected profile for the
         --  Put_Image attribute of U_Ent. In the second case, either Pnam has
         --  been analyzed (and has the expected profile), or it has not been
         --  analyzed yet (case of a type that has not been frozen yet and for
         --  which Put_Image has been set using Set_TSS).

         if Present (Pnam)
           and then (No (First_Entity (Pnam)) or else Has_Good_Profile (Pnam))
         then
            Error_Msg_Sloc := Sloc (Pnam);
            Error_Msg_Name_1 := Attr;
            Error_Msg_N ("% attribute already defined #", Nam);
            return;
         end if;

         Analyze (Expr);

         if Is_Entity_Name (Expr) then
            if not Is_Overloaded (Expr) then
               if Has_Good_Profile (Entity (Expr), Report => True) then
                  Subp := Entity (Expr);
               end if;

            else
               Get_First_Interp (Expr, I, It);
               while Present (It.Nam) loop
                  if Has_Good_Profile (It.Nam) then
                     Subp := It.Nam;
                     exit;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end if;
         end if;

         if Present (Subp) then
            if Is_Abstract_Subprogram (Subp) then
               Error_Msg_N ("Put_Image subprogram must not be abstract", Expr);
               return;
            end if;

            Set_Entity (Expr, Subp);
            Set_Etype (Expr, Etype (Subp));

            New_Put_Image_Subprogram (N, U_Ent, Subp);

         else
            Error_Msg_Name_1 := Attr;
            Error_Msg_N ("incorrect expression for% attribute", Expr);
         end if;
      end Analyze_Put_Image_TSS_Definition;

      -----------------------------------
      -- Analyze_Stream_TSS_Definition --
      -----------------------------------

      procedure Analyze_Stream_TSS_Definition (TSS_Nam : TSS_Name_Type) is
         Subp : Entity_Id := Empty;
         I    : Interp_Index;
         It   : Interp;
         Pnam : Entity_Id;

         Is_Read : constant Boolean := (TSS_Nam = TSS_Stream_Read);
         --  True for Read attribute, False for other attributes

         function Has_Good_Profile
           (Subp   : Entity_Id;
            Report : Boolean := False) return Boolean;
         --  Return true if the entity is a subprogram with an appropriate
         --  profile for the attribute being defined. If result is False and
         --  Report is True, function emits appropriate error.

         ----------------------
         -- Has_Good_Profile --
         ----------------------

         function Has_Good_Profile
           (Subp   : Entity_Id;
            Report : Boolean := False) return Boolean
         is
            Expected_Ekind : constant array (Boolean) of Entity_Kind :=
                               (False => E_Procedure, True => E_Function);
            Is_Function    : constant Boolean := (TSS_Nam = TSS_Stream_Input);
            F              : Entity_Id;
            Typ            : Entity_Id;

         begin
            if Ekind (Subp) /= Expected_Ekind (Is_Function) then
               return False;
            end if;

            F := First_Formal (Subp);

            if No (F)
              or else Ekind (Etype (F)) /= E_Anonymous_Access_Type
              or else Base_Type (Designated_Type (Etype (F))) /=
                        Class_Wide_Type (RTE (RE_Root_Stream_Type))
            then
               return False;
            end if;

            if not Is_Function then
               Next_Formal (F);

               declare
                  Expected_Mode : constant array (Boolean) of Entity_Kind :=
                                    (False => E_In_Parameter,
                                     True  => E_Out_Parameter);
               begin
                  if Parameter_Mode (F) /= Expected_Mode (Is_Read) then
                     return False;
                  end if;
               end;

               Typ := Etype (F);

            else
               Typ := Etype (Subp);
            end if;

            --  Verify that the prefix of the attribute and the local name for
            --  the type of the formal match.

            if Base_Type (Typ) /= Base_Type (Ent) then
               return False;
            end if;

            if Present (Next_Formal (F)) then
               return False;

            elsif not Is_Scalar_Type (Typ)
              and then not Is_First_Subtype (Typ)
              and then not Is_Class_Wide_Type (Typ)
            then
               if Report and not Is_First_Subtype (Typ) then
                  Error_Msg_N
                    ("subtype of formal in stream operation must be a first "
                     & "subtype", Parameter_Type (Parent (F)));
               end if;

               return False;

            else
               return True;
            end if;
         end Has_Good_Profile;

      --  Start of processing for Analyze_Stream_TSS_Definition

      begin
         FOnly := True;

         if not Is_Type (U_Ent) then
            Error_Msg_N ("local name must be a subtype", Nam);
            return;

         elsif not Is_First_Subtype (U_Ent) then
            Error_Msg_N ("local name must be a first subtype", Nam);
            return;
         end if;

         Pnam := TSS (Base_Type (U_Ent), TSS_Nam);

         --  If Pnam is present, it can be either inherited from an ancestor
         --  type (in which case it is legal to redefine it for this type), or
         --  be a previous definition of the attribute for the same type (in
         --  which case it is illegal).

         --  In the first case, it will have been analyzed already, and we
         --  can check that its profile does not match the expected profile
         --  for a stream attribute of U_Ent. In the second case, either Pnam
         --  has been analyzed (and has the expected profile), or it has not
         --  been analyzed yet (case of a type that has not been frozen yet
         --  and for which the stream attribute has been set using Set_TSS).

         if Present (Pnam)
           and then (No (First_Entity (Pnam)) or else Has_Good_Profile (Pnam))
         then
            Error_Msg_Sloc := Sloc (Pnam);
            Error_Msg_Name_1 := Attr;
            Error_Msg_N ("% attribute already defined #", Nam);
            return;
         end if;

         Analyze (Expr);

         if Is_Entity_Name (Expr) then
            if not Is_Overloaded (Expr) then
               if Has_Good_Profile (Entity (Expr), Report => True) then
                  Subp := Entity (Expr);
               end if;

            else
               Get_First_Interp (Expr, I, It);
               while Present (It.Nam) loop
                  if Has_Good_Profile (It.Nam) then
                     Subp := It.Nam;
                     exit;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end if;
         end if;

         if Present (Subp) then
            if Is_Abstract_Subprogram (Subp) then
               Error_Msg_N ("stream subprogram must not be abstract", Expr);
               return;

            --  A stream subprogram for an interface type must be a null
            --  procedure (RM 13.13.2 (38/3)). Note that the class-wide type
            --  of an interface is not an interface type (3.9.4 (6.b/2)).

            elsif Is_Interface (U_Ent)
              and then not Is_Class_Wide_Type (U_Ent)
              and then not Inside_A_Generic
              and then
                (Ekind (Subp) = E_Function
                  or else
                    not Null_Present
                          (Specification
                             (Unit_Declaration_Node (Ultimate_Alias (Subp)))))
            then
               Error_Msg_N
                 ("stream subprogram for interface type must be null "
                  & "procedure", Expr);
            end if;

            Set_Entity (Expr, Subp);
            Set_Etype (Expr, Etype (Subp));

            New_Stream_Subprogram (N, U_Ent, Subp, TSS_Nam);

         else
            Error_Msg_Name_1 := Attr;

            if Is_Class_Wide_Type (Base_Type (Ent)) then
               Error_Msg_N
                 ("incorrect expression for class-wide% attribute", Expr);
            else
               Error_Msg_N ("incorrect expression for% attribute", Expr);
            end if;
         end if;
      end Analyze_Stream_TSS_Definition;

      ------------------------------
      -- Check_Indexing_Functions --
      ------------------------------

      procedure Check_Indexing_Functions is
         Indexing_Found : Boolean := False;

         procedure Check_Inherited_Indexing;
         --  For a derived type, check that for a derived type, a specification
         --  of an indexing aspect can only be confirming, i.e. uses the same
         --  name as in the parent type.
         --  AI12-0160: Verify that an indexing cannot be specified for
         --  a derived type unless it is specified for the parent.

         procedure Check_One_Function (Subp : Entity_Id);
         --  Check one possible interpretation. Sets Indexing_Found True if a
         --  legal indexing function is found.

         procedure Illegal_Indexing (Msg : String);
         --  Diagnose illegal indexing function if not overloaded. In the
         --  overloaded case indicate that no legal interpretation  exists.

         ------------------------------
         -- Check_Inherited_Indexing --
         ------------------------------

         procedure Check_Inherited_Indexing is
            Inherited      : Node_Id;
            Other_Indexing : Node_Id;

         begin
            if Attr = Name_Constant_Indexing then
               Inherited :=
                 Find_Aspect (Etype (Ent), Aspect_Constant_Indexing);
               Other_Indexing :=
                 Find_Aspect (Etype (Ent), Aspect_Variable_Indexing);

            else pragma Assert (Attr = Name_Variable_Indexing);
               Inherited :=
                  Find_Aspect (Etype (Ent), Aspect_Variable_Indexing);
               Other_Indexing :=
                 Find_Aspect (Etype (Ent), Aspect_Constant_Indexing);
            end if;

            if Present (Inherited) then
               if Debug_Flag_Dot_XX then
                  null;

               --  OK if current attribute_definition_clause is expansion of
               --  inherited aspect.

               elsif Aspect_Rep_Item (Inherited) = N then
                  null;

               --  Check if this is a confirming specification. The name
               --  may be overloaded between the parent operation and the
               --  inherited one, so we check that the Chars fields match.

               elsif Is_Entity_Name (Expression (Inherited))
                 and then Chars (Entity (Expression (Inherited))) =
                    Chars (Entity (Expression (N)))
               then
                  Indexing_Found := True;

               --  Indicate the operation that must be overridden, rather than
               --  redefining the indexing aspect.

               else
                  Illegal_Indexing
                    ("indexing function already inherited from parent type");
                  Error_Msg_NE
                    ("!override & instead",
                     N, Entity (Expression (Inherited)));
               end if;

            --  If not inherited and the parent has another indexing function
            --  this is illegal, because it leads to inconsistent results in
            --  class-wide calls.

            elsif Present (Other_Indexing) then
               Error_Msg_N
                 ("cannot specify indexing operation on derived type"
                   & " if not specified for parent", N);
            end if;
         end Check_Inherited_Indexing;

         ------------------------
         -- Check_One_Function --
         ------------------------

         procedure Check_One_Function (Subp : Entity_Id) is
            Default_Element : Node_Id;
            Ret_Type        : constant Entity_Id := Etype (Subp);

         begin
            if not Is_Overloadable (Subp) then
               Illegal_Indexing ("illegal indexing function for type&");
               return;

            elsif Scope (Subp) /= Scope (Ent) then
               if Nkind (Expr) = N_Expanded_Name then

                  --  Indexing function can't be declared elsewhere

                  Illegal_Indexing
                    ("indexing function must be declared"
                      & " in scope of type&");
               end if;

               if Is_Derived_Type (Ent) then
                  Check_Inherited_Indexing;
               end if;

               return;

            elsif No (First_Formal (Subp)) then
               Illegal_Indexing
                 ("Indexing requires a function that applies to type&");
               return;

            elsif No (Next_Formal (First_Formal (Subp))) then
               Illegal_Indexing
                 ("indexing function must have at least two parameters");
               return;

            elsif Is_Derived_Type (Ent) then
               Check_Inherited_Indexing;
            end if;

            if not Check_Primitive_Function (Subp) then
               Illegal_Indexing
                 ("Indexing aspect requires a function that applies to type&");
               return;
            end if;

            --  If partial declaration exists, verify that it is not tagged.

            if Ekind (Current_Scope) = E_Package
              and then Has_Private_Declaration (Ent)
              and then From_Aspect_Specification (N)
              and then
                List_Containing (Parent (Ent)) =
                  Private_Declarations
                    (Specification (Unit_Declaration_Node (Current_Scope)))
              and then Nkind (N) = N_Attribute_Definition_Clause
            then
               declare
                  Decl : Node_Id;

               begin
                  Decl :=
                     First (Visible_Declarations
                              (Specification
                                 (Unit_Declaration_Node (Current_Scope))));

                  while Present (Decl) loop
                     if Nkind (Decl) = N_Private_Type_Declaration
                       and then Ent = Full_View (Defining_Identifier (Decl))
                       and then Tagged_Present (Decl)
                       and then No (Aspect_Specifications (Decl))
                     then
                        Illegal_Indexing
                          ("Indexing aspect cannot be specified on full view "
                           & "if partial view is tagged");
                        return;
                     end if;

                     Next (Decl);
                  end loop;
               end;
            end if;

            --  An indexing function must return either the default element of
            --  the container, or a reference type. For variable indexing it
            --  must be the latter.

            Default_Element :=
              Find_Value_Of_Aspect
               (Etype (First_Formal (Subp)), Aspect_Iterator_Element);

            if Present (Default_Element) then
               Analyze (Default_Element);
            end if;

            --  For variable_indexing the return type must be a reference type

            if Attr = Name_Variable_Indexing then
               if not Has_Implicit_Dereference (Ret_Type) then
                  Illegal_Indexing
                     ("variable indexing must return a reference type");
                  return;

               elsif Is_Access_Constant
                       (Etype (First_Discriminant (Ret_Type)))
               then
                  Illegal_Indexing
                    ("variable indexing must return an access to variable");
                  return;
               end if;

            else
               if Has_Implicit_Dereference (Ret_Type)
                 and then not
                   Is_Access_Constant
                     (Etype (Get_Reference_Discriminant (Ret_Type)))
               then
                  Illegal_Indexing
                    ("constant indexing must return an access to constant");
                  return;

               elsif Is_Access_Type (Etype (First_Formal (Subp)))
                 and then not Is_Access_Constant (Etype (First_Formal (Subp)))
               then
                  Illegal_Indexing
                    ("constant indexing must apply to an access to constant");
                  return;
               end if;
            end if;

            --  All checks succeeded

            Indexing_Found := True;
         end Check_One_Function;

         -----------------------
         --  Illegal_Indexing --
         -----------------------

         procedure Illegal_Indexing (Msg : String) is
         begin
            Error_Msg_NE (Msg, N, Ent);
         end Illegal_Indexing;

      --  Start of processing for Check_Indexing_Functions

      begin
         if In_Instance then
            Check_Inherited_Indexing;
         end if;

         Analyze (Expr);

         if not Is_Overloaded (Expr) then
            Check_One_Function (Entity (Expr));

         else
            declare
               I  : Interp_Index;
               It : Interp;

            begin
               Indexing_Found := False;
               Get_First_Interp (Expr, I, It);
               while Present (It.Nam) loop

                  --  Note that analysis will have added the interpretation
                  --  that corresponds to the dereference. We only check the
                  --  subprogram itself. Ignore homonyms that may come from
                  --  derived types in the context.

                  if Is_Overloadable (It.Nam)
                    and then Comes_From_Source (It.Nam)
                  then
                     Check_One_Function (It.Nam);
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end;
         end if;

         if not Indexing_Found and then not Error_Posted (N) then
            Error_Msg_NE
              ("aspect Indexing requires a local function that applies to "
               & "type&", Expr, Ent);
         end if;
      end Check_Indexing_Functions;

      ------------------------------
      -- Check_Iterator_Functions --
      ------------------------------

      procedure Check_Iterator_Functions is
         function Valid_Default_Iterator (Subp     : Entity_Id;
                                          Ref_Node : Node_Id := Empty)
                                          return Boolean;
         --  Check one possible interpretation for validity. If
         --  Ref_Node is present report errors on violations.

         ----------------------------
         -- Valid_Default_Iterator --
         ----------------------------

         function Valid_Default_Iterator (Subp     : Entity_Id;
                                          Ref_Node : Node_Id := Empty)
                                          return Boolean
         is
            Return_Type : constant Entity_Id := Etype (Etype (Subp));
            Return_Node : Node_Id;
            Root_T      : constant Entity_Id := Root_Type (Return_Type);
            Formal      : Entity_Id;

            function Valid_Iterator_Name (E : Entity_Id) return Boolean
            is (Chars (E) in Name_Forward_Iterator | Name_Reversible_Iterator);

            function Valid_Iterator_Name (L : Elist_Id) return Boolean;

            -------------------------
            -- Valid_Iterator_Name --
            -------------------------

            function Valid_Iterator_Name (L : Elist_Id) return Boolean
            is
               Iface_Elmt : Elmt_Id := First_Elmt (L);
            begin
               while Present (Iface_Elmt) loop
                  if Valid_Iterator_Name (Node (Iface_Elmt)) then
                     return True;
                  end if;
                  Next_Elmt (Iface_Elmt);
               end loop;

               return False;
            end Valid_Iterator_Name;

         begin
            if Subp = Any_Id then
               if Present (Ref_Node) then

                  --  Subp is not resolved and an error will be posted about
                  --  it later

                  Error_Msg_N ("improper function for default iterator!",
                     Ref_Node);
               end if;

               return False;
            end if;

            if not Check_Primitive_Function (Subp) then
               if Present (Ref_Node) then
                  if Debug_Flag_Underscore_DD then
                     Record_Default_Iterator_Not_Primitive_Error
                       (Ref_Node, Subp);
                  else
                     Error_Msg_N ("improper function for default iterator!",
                        Ref_Node);
                     Error_Msg_Sloc := Sloc (Subp);
                     Error_Msg_NE
                        ("\\default iterator defined # "
                        & "must be a primitive function",
                        Ref_Node, Subp);
                  end if;
               end if;

               return False;
            end if;

            --  The return type must be derived from a type in an instance
            --  of Iterator.Interfaces, and thus its root type must have a
            --  predefined name.

            if not Valid_Iterator_Name (Root_T)
               and then not (Has_Interfaces (Return_Type) and then
                  Valid_Iterator_Name (Interfaces (Return_Type)))
            then
               if Present (Ref_Node) then

                  Return_Node := Result_Definition (Parent (Subp));

                  Error_Msg_N ("improper function for default iterator!",
                     Ref_Node);
                  Error_Msg_Sloc := Sloc (Return_Node);
                  Error_Msg_NE ("\\return type & # "
                     & "must inherit from either "
                     & "Forward_Iterator or Reversible_Iterator",
                     Ref_Node, Return_Node);
               end if;

               return False;
            end if;

            Formal := First_Formal (Subp);

            --  False if any subsequent formal has no default expression

            Next_Formal (Formal);
            while Present (Formal) loop
               if No (Expression (Parent (Formal))) then
                  if Present (Ref_Node) then
                     Error_Msg_N ("improper function for default iterator!",
                        Ref_Node);
                     Error_Msg_Sloc := Sloc (Formal);
                     Error_Msg_NE ("\\formal parameter & # "
                        & "must have a default expression",
                        Ref_Node, Formal);
                  end if;

                  return False;
               end if;

               Next_Formal (Formal);
            end loop;

            --  True if all subsequent formals have default expressions

            return True;
         end Valid_Default_Iterator;

         Ignore : Boolean;

      --  Start of processing for Check_Iterator_Functions

      begin
         Analyze (Expr);

         if not Is_Entity_Name (Expr) then
            Error_Msg_N ("aspect Iterator must be a function name", Expr);
         end if;

         if not Is_Overloaded (Expr) then
            if Entity (Expr) /= Any_Id
              and then not Check_Primitive_Function (Entity (Expr))
            then
               Error_Msg_NE
                 ("aspect Indexing requires a function that applies to type&",
                  Entity (Expr), Ent);
            end if;

            --  Flag the default_iterator as well as the denoted function.

            Ignore := Valid_Default_Iterator (Entity (Expr), Expr);

         else
            declare
               Default : Entity_Id := Empty;
               I       : Interp_Index;
               It      : Interp;

            begin
               Get_First_Interp (Expr, I, It);
               while Present (It.Nam) loop
                  if not Check_Primitive_Function (It.Nam)
                    or else not Valid_Default_Iterator (It.Nam)
                  then
                     Remove_Interp (I);

                  elsif Present (Default) then

                     --  An explicit one should override an implicit one

                     if Comes_From_Source (Default) =
                          Comes_From_Source (It.Nam)
                     then
                        Error_Msg_N ("default iterator must be unique", Expr);
                        Error_Msg_Sloc := Sloc (Default);
                        Error_Msg_N ("\\possible interpretation#", Expr);
                        Error_Msg_Sloc := Sloc (It.Nam);
                        Error_Msg_N ("\\possible interpretation#", Expr);

                     elsif Comes_From_Source (It.Nam) then
                        Default := It.Nam;
                     end if;
                  else
                     Default := It.Nam;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;

               if Present (Default) then
                  Set_Entity (Expr, Default);
                  Set_Is_Overloaded (Expr, False);
               else
                  Error_Msg_N
                    ("no interpretation is a valid default iterator!", Expr);
               end if;
            end;
         end if;
      end Check_Iterator_Functions;

      -------------------------------
      -- Check_Primitive_Function  --
      -------------------------------

      function Check_Primitive_Function (Subp : Entity_Id) return Boolean is
         Ctrl : Entity_Id;

      begin
         if Ekind (Subp) /= E_Function then
            return False;
         end if;

         if No (First_Formal (Subp)) then
            return False;
         else
            Ctrl := Etype (First_Formal (Subp));
         end if;

         --  To be a primitive operation subprogram has to be in same scope.

         if Scope (Ctrl) /= Scope (Subp) then
            return False;
         end if;

         --  Type of formal may be the class-wide type, an access to such,
         --  or an incomplete view.

         if Ctrl = Ent
           or else Ctrl = Class_Wide_Type (Ent)
           or else
             (Ekind (Ctrl) = E_Anonymous_Access_Type
               and then (Designated_Type (Ctrl) = Ent
                           or else
                         Designated_Type (Ctrl) = Class_Wide_Type (Ent)))
           or else
             (Ekind (Ctrl) = E_Incomplete_Type
               and then Full_View (Ctrl) = Ent)
         then
            null;
         else
            return False;
         end if;

         return True;
      end Check_Primitive_Function;

      ----------------------
      -- Duplicate_Clause --
      ----------------------

      function Duplicate_Clause return Boolean is

         function Check_One_Attr (Attr_1, Attr_2 : Name_Id) return Boolean;
         --  Check for one attribute; Attr_1 is the attribute_designator we are
         --  looking for. Attr_2 is the attribute_designator of the current
         --  node. Normally, this is called just once by Duplicate_Clause, with
         --  Attr_1 = Attr_2. However, it needs to be called twice for Size and
         --  Value_Size, because these mean the same thing. For compatibility,
         --  we allow specifying both Size and Value_Size, but only if the two
         --  sizes are equal.

         --------------------
         -- Check_One_Attr --
         --------------------

         function Check_One_Attr (Attr_1, Attr_2 : Name_Id) return Boolean is
            A : constant Node_Id :=
              Get_Rep_Item (U_Ent, Attr_1, Check_Parents => False);
         begin
            if Present (A) then
               if Attr_1 = Attr_2 then
                  Error_Msg_Name_1 := Attr_1;
                  Error_Msg_Sloc := Sloc (A);
                  Error_Msg_NE ("aspect% for & previously given#", N, U_Ent);

               else
                  pragma Assert (Attr_1 in Name_Size | Name_Value_Size);
                  pragma Assert (Attr_2 in Name_Size | Name_Value_Size);

                  Error_Msg_Name_1 := Attr_2;
                  Error_Msg_Name_2 := Attr_1;
                  Error_Msg_Sloc := Sloc (A);
                  Error_Msg_NE ("?% for & conflicts with % #", N, U_Ent);
               end if;

               return True;
            end if;

            return False;
         end Check_One_Attr;

      --  Start of processing for Duplicate_Clause

      begin
         --  Nothing to do if this attribute definition clause comes from
         --  an aspect specification, since we could not be duplicating an
         --  explicit clause, and we dealt with the case of duplicated aspects
         --  in Analyze_Aspect_Specifications.

         if From_Aspect_Specification (N) then
            return False;
         end if;

         --  Special cases for Size and Value_Size

         if (Chars (N) = Name_Size
               and then Check_One_Attr (Name_Value_Size, Name_Size))
           or else
            (Chars (N) = Name_Value_Size
               and then Check_One_Attr (Name_Size, Name_Value_Size))
         then
            return True;
         end if;

         --  Normal case (including Size and Value_Size)

         return Check_One_Attr (Chars (N), Chars (N));
      end Duplicate_Clause;

   --  Start of processing for Analyze_Attribute_Definition_Clause

   begin
      --  The following code is a defense against recursion. Not clear that
      --  this can happen legitimately, but perhaps some error situations can
      --  cause it, and we did see this recursion during testing.

      if Analyzed (N) then
         return;
      else
         Set_Analyzed (N, True);
      end if;

      Check_Restriction_No_Use_Of_Attribute (N);

      if Is_Aspect_Id (Chars (N)) then
         --  6.1/3 No_Specification_of_Aspect: Identifies an aspect for which
         --    no aspect_specification, attribute_definition_clause, or pragma
         --    is given.
         Check_Restriction_No_Specification_Of_Aspect (N);
      end if;

      --  Process Ignore_Rep_Clauses option

      if Ignore_Rep_Clauses then
         case Id is

            --  The following should be ignored. They do not affect legality
            --  and may be target dependent. The basic idea of -gnatI is to
            --  ignore any rep clauses that may be target dependent but do not
            --  affect legality (except possibly to be rejected because they
            --  are incompatible with the compilation target).

            when Attribute_Alignment
               | Attribute_Bit_Order
               | Attribute_Component_Size
               | Attribute_Default_Scalar_Storage_Order
               | Attribute_Machine_Radix
               | Attribute_Object_Size
               | Attribute_Scalar_Storage_Order
               | Attribute_Size
               | Attribute_Small
               | Attribute_Stream_Size
               | Attribute_Value_Size
            =>
               Kill_Rep_Clause (N);
               return;

            --  The following should not be ignored, because in the first place
            --  they are reasonably portable, and should not cause problems
            --  in compiling code from another target, and also they do affect
            --  legality, e.g. failing to provide a stream attribute for a type
            --  may make a program illegal.

            when Attribute_External_Tag
               | Attribute_Input
               | Attribute_Output
               | Attribute_Put_Image
               | Attribute_Read
               | Attribute_Simple_Storage_Pool
               | Attribute_Storage_Pool
               | Attribute_Storage_Size
               | Attribute_Write
            =>
               null;

            --  We do not do anything here with address clauses, they will be
            --  removed by Freeze later on, but for now, it works better to
            --  keep them in the tree.

            when Attribute_Address =>
               null;

            --  Other cases are errors ("attribute& cannot be set with
            --  definition clause"), which will be caught below.

            when others =>
               null;
         end case;
      end if;

      Analyze (Nam);
      Ent := Entity (Nam);

      if Rep_Item_Too_Early (Ent, N) then
         return;
      end if;

      --  Rep clause applies to (underlying) full view of private or incomplete
      --  type if we have one (if not, this is a premature use of the type).
      --  However, some semantic checks need to be done on the specified entity
      --  i.e. the private view, so we save it in Ent.

      if Is_Private_Type (Ent)
        and then Is_Derived_Type (Ent)
        and then not Is_Tagged_Type (Ent)
        and then No (Full_View (Ent))
        and then No (Underlying_Full_View (Ent))
      then
         U_Ent := Ent;

      elsif Ekind (Ent) = E_Incomplete_Type then

         --  The attribute applies to the full view, set the entity of the
         --  attribute definition accordingly.

         Ent := Underlying_Type (Ent);
         U_Ent := Ent;
         Set_Entity (Nam, Ent);

      else
         U_Ent := Underlying_Type (Ent);
      end if;

      --  Avoid cascaded error

      if Etype (Nam) = Any_Type then
         return;

      --  Must be declared in current scope or in case of an aspect
      --  specification, must be visible in current scope.

      elsif Scope (Ent) /= Current_Scope
        and then
          not (From_Aspect_Specification (N)
                and then Scope_Within_Or_Same (Current_Scope, Scope (Ent)))
      then
         Error_Msg_N ("entity must be declared in this scope", Nam);
         return;

      --  Must not be a source renaming (we do have some cases where the
      --  expander generates a renaming, and those cases are OK, in such
      --  cases any attribute applies to the renamed object as well).

      elsif Is_Object (Ent)
        and then Present (Renamed_Object (Ent))
      then
         --  In the case of a renamed object from source, this is an error
         --  unless the object is an aggregate and the renaming is created
         --  for an object declaration.

         if Comes_From_Source (Renamed_Object (Ent))
           and then Nkind (Renamed_Object (Ent)) /= N_Aggregate
         then
            Get_Name_String (Chars (N));
            Error_Msg_Strlen := Name_Len;
            Error_Msg_String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
            Error_Msg_N
              ("~ clause not allowed for a renaming declaration "
               & "(RM 13.1(6))", Nam);
            return;

         --  For the case of a compiler generated renaming, the attribute
         --  definition clause applies to the renamed object created by the
         --  expander. The easiest general way to handle this is to create a
         --  copy of the attribute definition clause for this object.

         elsif Is_Entity_Name (Renamed_Object (Ent)) then
            Insert_Action (N,
              Make_Attribute_Definition_Clause (Loc,
                Name       =>
                  New_Occurrence_Of (Entity (Renamed_Object (Ent)), Loc),
                Chars      => Chars (N),
                Expression => Duplicate_Subexpr (Expression (N))));

         --  If the renamed object is not an entity, it must be a dereference
         --  of an unconstrained function call, and we must introduce a new
         --  declaration to capture the expression. This is needed in the case
         --  of 'Alignment, where the original declaration must be rewritten.

         else
            pragma Assert
              (Nkind (Renamed_Object (Ent)) = N_Explicit_Dereference);
            null;
         end if;

      --  If no underlying entity, use entity itself, applies to some
      --  previously detected error cases ???

      elsif No (U_Ent) then
         U_Ent := Ent;

      --  Cannot specify for a subtype (exception Object/Value_Size)

      elsif Is_Type (U_Ent)
        and then not Is_First_Subtype (U_Ent)
        and then Id /= Attribute_Object_Size
        and then Id /= Attribute_Value_Size
        and then not From_At_Mod (N)
      then
         Error_Msg_N ("cannot specify attribute for subtype", Nam);
         return;
      end if;

      Set_Entity (N, U_Ent);

      --  Switch on particular attribute

      case Id is

         -------------
         -- Address --
         -------------

         --  Address attribute definition clause

         when Attribute_Address => Address : begin

            --  A little error check, catch for X'Address use X'Address;

            if Nkind (Nam) = N_Identifier
              and then Nkind (Expr) = N_Attribute_Reference
              and then Attribute_Name (Expr) = Name_Address
              and then Nkind (Prefix (Expr)) = N_Identifier
              and then Chars (Nam) = Chars (Prefix (Expr))
            then
               Error_Msg_NE
                 ("address for & is self-referencing", Prefix (Expr), Ent);
               return;
            end if;

            --  Not that special case, carry on with analysis of expression

            Analyze_And_Resolve (Expr, RTE (RE_Address));

            --  Even when ignoring rep clauses we need to indicate that the
            --  entity has an address clause and thus it is legal to declare
            --  it imported. Freeze will get rid of the address clause later.
            --  Also call Set_Address_Taken to indicate that an address clause
            --  was present, even if we are about to remove it.

            if Ignore_Rep_Clauses then
               Set_Address_Taken (U_Ent);

               if Ekind (U_Ent) in E_Variable | E_Constant then
                  Record_Rep_Item (U_Ent, N);
               end if;

               return;
            end if;

            if Duplicate_Clause then
               null;

            --  Case of address clause for subprogram

            elsif Is_Subprogram (U_Ent) then
               if Has_Homonym (U_Ent) then
                  Error_Msg_N
                    ("address clause cannot be given for overloaded "
                     & "subprogram", Nam);
                  return;
               end if;

               --  For subprograms, all address clauses are permitted, and we
               --  mark the subprogram as having a deferred freeze so that Gigi
               --  will not elaborate it too soon.

               --  Above needs more comments, what is too soon about???

               Set_Has_Delayed_Freeze (U_Ent);

            --  Case of address clause for entry

            elsif Ekind (U_Ent) = E_Entry then
               if Nkind (Parent (N)) = N_Task_Body then
                  Error_Msg_N
                    ("entry address must be specified in task spec", Nam);
                  return;
               end if;

               --  For entries, we require a constant address

               Check_Constant_Address_Clause (Expr, U_Ent);

               --  Special checks for task types

               if Is_Task_Type (Scope (U_Ent))
                 and then Comes_From_Source (Scope (U_Ent))
               then
                  Error_Msg_N
                    ("??entry address declared for entry in task type", N);
                  Error_Msg_N
                    ("\??only one task can be declared of this type", N);
               end if;

               --  Entry address clauses are obsolescent

               Check_Restriction (No_Obsolescent_Features, N);

               if Warn_On_Obsolescent_Feature then
                  Error_Msg_N
                    ("?j?attaching interrupt to task entry is an obsolescent "
                     & "feature (RM J.7.1)", N);
                  Error_Msg_N
                    ("\?j?use interrupt procedure instead", N);
               end if;

            --  Case of address clause for an object

            elsif Ekind (U_Ent) in E_Constant | E_Variable then

               --  Disallow case of an address clause for an object of an
               --  indefinite subtype which takes its bounds/discriminant/tag
               --  from its initial value. Without this, we get a Gigi
               --  assertion failure for things like
               --    X : String := Some_Function (...) with Address => ...;
               --  where the result subtype of the function is unconstrained.
               --
               --  We want to reject two cases: the class-wide case, and the
               --  case where the FE conjures up a renaming declaration and
               --  would then otherwise generate an address specification for
               --  that renaming (which is a malformed tree, which is why Gigi
               --  complains).

               if Is_Class_Wide_Type (Etype (U_Ent)) then
                  Error_Msg_N
                    ("address specification not supported for class-wide " &
                     "object declaration", Nam);
                  return;
               elsif Is_Constr_Subt_For_U_Nominal (Etype (U_Ent))
                 and then
                   Nkind (Parent (U_Ent)) = N_Object_Renaming_Declaration
               then
                  --  Confirm accuracy of " and dynamic size" message text
                  --  before including it. We want to include that text when
                  --  it is correct because it may be useful to the reader.
                  --  The case where we omit that part of the message text
                  --  might be dead code, but let's not rely on that.

                  Error_Msg_N
                    ("address specification not supported for object " &
                     "declaration with indefinite nominal subtype" &
                     (if Size_Known_At_Compile_Time (Etype (U_Ent))
                      then ""
                      else " and dynamic size"), Nam);
                  return;
               end if;

               declare
                  Expr  : constant Node_Id := Expression (N);
                  O_Ent : Entity_Id;
                  Off   : Boolean;

               begin
                  --  Exported variables cannot have an address clause, because
                  --  this cancels the effect of the pragma Export.

                  if Is_Exported (U_Ent) then
                     Error_Msg_N
                       ("cannot export object with address clause", Nam);
                     return;
                  end if;

                  Find_Overlaid_Entity (N, O_Ent, Off);

                  if Present (O_Ent) then

                     --  If the object overlays a constant object, mark it so

                     if Is_Constant_Object (O_Ent) then
                        Set_Overlays_Constant (U_Ent);
                     end if;

                     --  If the address clause is of the form:

                     --    for X'Address use Y'Address;

                     --  or

                     --    C : constant Address := Y'Address;
                     --    ...
                     --    for X'Address use C;

                     --  then we make an entry in the table to check the size
                     --  and alignment of the overlaying variable. But we defer
                     --  this check till after code generation to take full
                     --  advantage of the annotation done by the back end.

                     --  If the entity has a generic type, the check will be
                     --  performed in the instance if the actual type justifies
                     --  it, and we do not insert the clause in the table to
                     --  prevent spurious warnings.

                     --  Note: we used to test Comes_From_Source and only give
                     --  this warning for source entities, but we have removed
                     --  this test. It really seems bogus to generate overlays
                     --  that would trigger this warning in generated code.
                     --  Furthermore, by removing the test, we handle the
                     --  aspect case properly.

                     if Is_Object (O_Ent)
                       and then not Is_Generic_Formal (O_Ent)
                       and then not Is_Generic_Type (Etype (U_Ent))
                       and then Address_Clause_Overlay_Warnings
                     then
                        Register_Address_Clause_Check
                          (N, U_Ent, No_Uint, O_Ent, Off);
                     end if;

                     --  If the overlay changes the storage order, warn since
                     --  the construct is not really supported by the back end.
                     --  Also mark the entity as being volatile to block the
                     --  optimizer, even if there is no warranty on the result.

                     if (Is_Record_Type (Etype (U_Ent))
                          or else Is_Array_Type (Etype (U_Ent)))
                       and then (Is_Record_Type (Etype (O_Ent))
                                  or else Is_Array_Type (Etype (O_Ent)))
                       and then Reverse_Storage_Order (Etype (U_Ent)) /=
                                Reverse_Storage_Order (Etype (O_Ent))
                     then
                        Error_Msg_N
                          ("??overlay changes scalar storage order", Expr);
                        Set_Treat_As_Volatile (U_Ent);
                     end if;

                  else
                     --  If this is not an overlay, mark a variable as being
                     --  volatile to prevent unwanted optimizations. It's a
                     --  conservative interpretation of RM 13.3(19) for the
                     --  cases where the compiler cannot detect potential
                     --  aliasing issues easily and it also covers the case
                     --  of an absolute address where the volatile aspect is
                     --  kind of implicit.

                     if Ekind (U_Ent) = E_Variable then
                        Set_Treat_As_Volatile (U_Ent);
                     end if;

                     --  Make an entry in the table for an absolute address as
                     --  above to check that the value is compatible with the
                     --  alignment of the object.

                     declare
                        Addr : constant Node_Id := Address_Value (Expr);
                     begin
                        if Compile_Time_Known_Value (Addr)
                          and then Address_Clause_Overlay_Warnings
                        then
                           Register_Address_Clause_Check
                             (N, U_Ent, Expr_Value (Addr), Empty, False);
                        end if;
                     end;
                  end if;

                  --  Issue an unconditional warning for a constant overlaying
                  --  a variable. For the reverse case, we will issue it only
                  --  if the variable is modified.
                  --  Within a generic unit an In_Parameter is a constant.
                  --  It can be instantiated with a variable, in which case
                  --  there will be a warning on the instance.

                  if Ekind (U_Ent) = E_Constant
                    and then Present (O_Ent)
                    and then Ekind (O_Ent) /= E_Generic_In_Parameter
                    and then not Overlays_Constant (U_Ent)
                    and then Address_Clause_Overlay_Warnings
                  then
                     Error_Msg_N ("?o?constant overlays a variable", Expr);

                  --  Imported variables can have an address clause, but then
                  --  the import is pretty meaningless except to suppress
                  --  initializations, so we do not need such variables to
                  --  be statically allocated (and in fact it causes trouble
                  --  if the address clause is a local value).

                  elsif Is_Imported (U_Ent) then
                     Set_Is_Statically_Allocated (U_Ent, False);
                  end if;

                  --  We mark a possible modification of a variable with an
                  --  address clause, since it is likely aliasing is occurring.

                  Note_Possible_Modification (Nam, Sure => False);

                  --  Legality checks on the address clause for initialized
                  --  objects is deferred until the freeze point, because
                  --  a subsequent pragma might indicate that the object
                  --  is imported and thus not initialized. Also, the address
                  --  clause might involve entities that have yet to be
                  --  elaborated.

                  Set_Has_Delayed_Freeze (U_Ent);

                  --  If an initialization call has been generated for this
                  --  object, it needs to be deferred to after the freeze node
                  --  we have just now added, otherwise GIGI will see a
                  --  reference to the variable (as actual to the IP call)
                  --  before its definition.

                  declare
                     Init_Call : constant Node_Id :=
                                   Remove_Init_Call (U_Ent, N);

                  begin
                     if Present (Init_Call) then
                        Append_Freeze_Action (U_Ent, Init_Call);

                        --  Reset Initialization_Statements pointer so that
                        --  if there is a pragma Import further down, it can
                        --  clear any default initialization.

                        Set_Initialization_Statements (U_Ent, Init_Call);
                     end if;
                  end;

                  --  Entity has delayed freeze, so we will generate an
                  --  alignment check at the freeze point unless suppressed.

                  if not Range_Checks_Suppressed (U_Ent)
                    and then not Alignment_Checks_Suppressed (U_Ent)
                  then
                     Set_Check_Address_Alignment (N);
                  end if;

                  --  Kill the size check code, since we are not allocating
                  --  the variable, it is somewhere else.

                  Kill_Size_Check_Code (U_Ent);
               end;

            --  Not a valid entity for an address clause

            else
               Error_Msg_N ("address cannot be given for &", Nam);
            end if;
         end Address;

         ---------------
         -- Alignment --
         ---------------

         --  Alignment attribute definition clause

         when Attribute_Alignment => Alignment : declare
            Align     : constant Uint := Get_Alignment_Value (Expr);
            Max_Align : constant Uint := UI_From_Int (Maximum_Alignment);

         begin
            FOnly := True;

            if not Is_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Variable
              and then Ekind (U_Ent) /= E_Constant
            then
               Error_Msg_N ("alignment cannot be given for &", Nam);

            elsif Duplicate_Clause then
               null;

            elsif Present (Align) then
               Set_Has_Alignment_Clause (U_Ent);

               --  Tagged type case, check for attempt to set alignment to a
               --  value greater than Max_Align, and reset if so.

               if Is_Tagged_Type (U_Ent) and then Align > Max_Align then
                  Error_Msg_N
                    ("alignment for & set to Maximum_Aligment??", Nam);
                  Set_Alignment (U_Ent, Max_Align);

               --  Because Object_Size must be multiple of Alignment (in bits),
               --  System_Max_Integer_Size limit for discrete and fixed point
               --  types implies a limit on alignment for such types.

               elsif (Is_Discrete_Type (U_Ent)
                        or else Is_Fixed_Point_Type (U_Ent))
                 and then Align > System_Max_Integer_Size / System_Storage_Unit
               then
                  Error_Msg_N
                    ("specified alignment too large for discrete or fixed " &
                     "point type", Expr);
                  Set_Alignment
                    (U_Ent, UI_From_Int (System_Max_Integer_Size /
                                         System_Storage_Unit));

               --  All other cases

               else
                  Set_Alignment (U_Ent, Align);
               end if;

               --  For an array type, U_Ent is the first subtype. In that case,
               --  also set the alignment of the anonymous base type so that
               --  other subtypes (such as the itypes for aggregates of the
               --  type) also receive the expected alignment.

               if Is_Array_Type (U_Ent) then
                  Set_Alignment (Base_Type (U_Ent), Align);
               end if;
            end if;
         end Alignment;

         ---------------
         -- Bit_Order --
         ---------------

         --  Bit_Order attribute definition clause

         when Attribute_Bit_Order =>
            if not Is_Record_Type (U_Ent) then
               Error_Msg_N
                 ("Bit_Order can only be defined for record type", Nam);

            elsif Is_Tagged_Type (U_Ent) and then Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("Bit_Order cannot be defined for record extensions", Nam);

            elsif Duplicate_Clause then
               null;

            else
               Analyze_And_Resolve (Expr, RTE (RE_Bit_Order));

               if Etype (Expr) = Any_Type then
                  return;

               elsif not Is_OK_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("Bit_Order requires static expression!", Expr);

               elsif (Expr_Value (Expr) = 0) /= Bytes_Big_Endian then
                  Set_Reverse_Bit_Order (Base_Type (U_Ent), True);
               end if;
            end if;

         --------------------
         -- Component_Size --
         --------------------

         --  Component_Size attribute definition clause

         when Attribute_Component_Size => Component_Size_Case : declare
            Csize    : constant Uint := Static_Integer (Expr);
            Ctyp     : Entity_Id;
            Btype    : Entity_Id;
            Biased   : Boolean;
            New_Ctyp : Entity_Id;
            Decl     : Node_Id;

         begin
            if not Is_Array_Type (U_Ent) then
               Error_Msg_N ("component size requires array type", Nam);
               return;
            end if;

            Btype := Base_Type (U_Ent);
            Ctyp  := Component_Type (Btype);

            if Duplicate_Clause then
               null;

            elsif Rep_Item_Too_Early (Btype, N) then
               null;

            elsif Present (Csize) then
               Check_Size (Expr, Ctyp, Csize, Biased);

               --  For the biased case, build a declaration for a subtype that
               --  will be used to represent the biased subtype that reflects
               --  the biased representation of components. We need the subtype
               --  to get proper conversions on referencing elements of the
               --  array.

               if Biased then
                  New_Ctyp :=
                    Make_Defining_Identifier (Loc,
                      Chars =>
                        New_External_Name (Chars (U_Ent), 'C', 0, 'T'));

                  Decl :=
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => New_Ctyp,
                      Subtype_Indication  =>
                        New_Occurrence_Of (Component_Type (Btype), Loc));

                  Set_Parent (Decl, N);
                  Analyze (Decl, Suppress => All_Checks);

                  Set_Has_Delayed_Freeze        (New_Ctyp, False);
                  Reinit_Esize                  (New_Ctyp);
                  Set_RM_Size                   (New_Ctyp, Csize);
                  Reinit_Alignment              (New_Ctyp);
                  Set_Is_Itype                  (New_Ctyp, True);
                  Set_Associated_Node_For_Itype (New_Ctyp, U_Ent);

                  Set_Component_Type (Btype, New_Ctyp);
                  Set_Biased (New_Ctyp, N, "component size clause");
               end if;

               Set_Component_Size (Btype, Csize);

               --  Deal with warning on overridden size

               if Warn_On_Overridden_Size
                 and then Has_Size_Clause (Ctyp)
                 and then RM_Size (Ctyp) /= Csize
               then
                  Error_Msg_NE
                    ("component size overrides size clause for&?.s?", N, Ctyp);
               end if;

               Set_Has_Component_Size_Clause (Btype, True);
               Set_Has_Non_Standard_Rep (Btype, True);
            end if;
         end Component_Size_Case;

         -----------------------
         -- Constant_Indexing --
         -----------------------

         when Attribute_Constant_Indexing =>
            Check_Indexing_Functions;

         ---------
         -- CPU --
         ---------

         when Attribute_CPU =>
            pragma Assert (From_Aspect_Specification (N));
            --  The parser forbids this clause in source code, so it must have
            --  come from an aspect specification.

            if not Is_Task_Type (U_Ent) then
               Error_Msg_N ("'C'P'U can only be defined for task", Nam);

            elsif Duplicate_Clause then
               null;

            else
               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               --  The visibility to the components must be established
               --  and restored before and after analysis.

               Push_Type (U_Ent);
               Preanalyze_Spec_Expression (Expr, RTE (RE_CPU_Range));
               Pop_Type (U_Ent);

               --  AI12-0117-1, "Restriction No_Tasks_Unassigned_To_CPU":
               --  If the expression is static, and its value is
               --  System.Multiprocessors.Not_A_Specific_CPU (i.e. zero) then
               --  that's a violation of No_Tasks_Unassigned_To_CPU. It might
               --  seem better to refer to Not_A_Specific_CPU here, but that
               --  involves a lot of horsing around with Rtsfind, and this
               --  value is not going to change, so it's better to hardwire
               --  Uint_0.
               --
               --  AI12-0055-1, "All properties of a usage profile are defined
               --  by pragmas": If the expression is nonstatic, that's a
               --  violation of No_Dynamic_CPU_Assignment.

               if Is_OK_Static_Expression (Expr) then
                  if Expr_Value (Expr) = Uint_0 then
                     Check_Restriction (No_Tasks_Unassigned_To_CPU, Expr);
                  end if;
               else
                  Check_Restriction (No_Dynamic_CPU_Assignment, Expr);
               end if;
            end if;

         ----------------------
         -- Default_Iterator --
         ----------------------

         when Attribute_Default_Iterator => Default_Iterator : declare
            Func : Entity_Id;
            Typ  : Entity_Id;

         begin
            --  If target type is untagged, further checks are irrelevant

            if not Is_Tagged_Type (U_Ent) then
               Error_Msg_N
                 ("aspect Default_Iterator applies to tagged type", Nam);
               return;
            end if;

            Check_Iterator_Functions;

            Analyze (Expr);

            if not Is_Entity_Name (Expr)
              or else Ekind (Entity (Expr)) /= E_Function
            then
               Error_Msg_N ("aspect Iterator must be a function", Expr);
               return;
            else
               Func := Entity (Expr);
            end if;

            --  The type of the first parameter must be T, T'class, or a
            --  corresponding access type (5.5.1 (8/3). If function is
            --  parameterless label type accordingly.

            if No (First_Formal (Func)) then
               Typ := Any_Type;
            else
               Typ := Etype (First_Formal (Func));
            end if;

            if Typ = U_Ent
              or else Typ = Class_Wide_Type (U_Ent)
              or else (Is_Access_Type (Typ)
                        and then Designated_Type (Typ) = U_Ent)
              or else (Is_Access_Type (Typ)
                        and then Designated_Type (Typ) =
                                          Class_Wide_Type (U_Ent))
            then
               null;

            else
               Error_Msg_NE
                 ("Default_Iterator must be a primitive of&", Func, U_Ent);
            end if;
         end Default_Iterator;

         ------------------------
         -- Dispatching_Domain --
         ------------------------

         when Attribute_Dispatching_Domain =>
            pragma Assert (From_Aspect_Specification (N));
            --  The parser forbids this clause in source code, so it must have
            --  come from an aspect specification.

            if not Is_Task_Type (U_Ent) then
               Error_Msg_N
                 ("Dispatching_Domain can only be defined for task", Nam);

            elsif Duplicate_Clause then
               null;

            else
               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               --  The visibility to the components must be restored

               Push_Type (U_Ent);

               Preanalyze_Spec_Expression
                 (Expr, RTE (RE_Dispatching_Domain));

               Pop_Type (U_Ent);
            end if;

         ------------------
         -- External_Tag --
         ------------------

         when Attribute_External_Tag =>
            if not Is_Tagged_Type (U_Ent) then
               Error_Msg_N ("should be a tagged type", Nam);
            end if;

            if Duplicate_Clause then
               null;

            else
               Analyze_And_Resolve (Expr, Standard_String);

               if not Is_OK_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("static string required for tag name!", Nam);
               end if;

               if not Is_Library_Level_Entity (U_Ent) then
                  Error_Msg_NE
                    ("??non-unique external tag supplied for &", N, U_Ent);
                  Error_Msg_N
                    ("\??same external tag applies to all subprogram calls",
                     N);
                  Error_Msg_N
                    ("\??corresponding internal tag cannot be obtained", N);
               end if;
            end if;

         --------------------------
         -- Implicit_Dereference --
         --------------------------

         when Attribute_Implicit_Dereference =>

            --  Legality checks already performed at the point of the type
            --  declaration, aspect is not delayed.

            null;

         -----------
         -- Input --
         -----------

         when Attribute_Input =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Input);
            Set_Has_Specified_Stream_Input (Ent);

         ------------------------
         -- Interrupt_Priority --
         ------------------------

         when Attribute_Interrupt_Priority =>
            pragma Assert (From_Aspect_Specification (N));
            --  The parser forbids this clause in source code, so it must have
            --  come from an aspect specification.

            if not Is_Concurrent_Type (U_Ent) then
               Error_Msg_N
                 ("Interrupt_Priority can only be defined for task and "
                  & "protected object", Nam);

            elsif Duplicate_Clause then
               null;

            else
               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               --  The visibility to the components must be restored

               Push_Type (U_Ent);

               Preanalyze_Spec_Expression
                 (Expr, RTE (RE_Interrupt_Priority));

               Pop_Type (U_Ent);

               --  Check the No_Task_At_Interrupt_Priority restriction

               if Is_Task_Type (U_Ent) then
                  Check_Restriction (No_Task_At_Interrupt_Priority, N);
               end if;
            end if;

         --------------
         -- Iterable --
         --------------

         when Attribute_Iterable =>
            Analyze (Expr);

            if Nkind (Expr) /= N_Aggregate then
               Error_Msg_N ("aspect Iterable must be an aggregate", Expr);
               return;
            end if;

            declare
               Assoc : Node_Id;

            begin
               Assoc := First (Component_Associations (Expr));
               while Present (Assoc) loop
                  Analyze (Expression (Assoc));

                  if not Is_Entity_Name (Expression (Assoc))
                    or else Ekind (Entity (Expression (Assoc))) /= E_Function
                  then
                     Error_Msg_N ("value must be a function", Assoc);
                  end if;

                  Next (Assoc);
               end loop;
            end;

         ----------------------
         -- Iterator_Element --
         ----------------------

         when Attribute_Iterator_Element =>
            Analyze (Expr);

            if not Is_Entity_Name (Expr)
              or else not Is_Type (Entity (Expr))
            then
               Error_Msg_N ("aspect Iterator_Element must be a type", Expr);
               return;
            end if;

         -------------------
         -- Machine_Radix --
         -------------------

         --  Machine radix attribute definition clause

         when Attribute_Machine_Radix => Machine_Radix : declare
            Radix : constant Uint := Static_Integer (Expr);

         begin
            if not Is_Decimal_Fixed_Point_Type (U_Ent) then
               Error_Msg_N ("decimal fixed-point type expected for &", Nam);

            elsif Duplicate_Clause then
               null;

            elsif Present (Radix) then
               Set_Has_Machine_Radix_Clause (U_Ent);
               Set_Has_Non_Standard_Rep (Base_Type (U_Ent));

               if Radix = 2 then
                  null;

               elsif Radix = 10 then
                  Set_Machine_Radix_10 (U_Ent);

               else
                  Error_Msg_N ("machine radix value must be 2 or 10", Expr);
               end if;
            end if;
         end Machine_Radix;

         -----------------
         -- Object_Size --
         -----------------

         --  Object_Size attribute definition clause

         when Attribute_Object_Size => Object_Size : declare
            Size : constant Uint := Static_Integer (Expr);

            Biased : Boolean;
            pragma Warnings (Off, Biased);

         begin
            if not Is_Type (U_Ent) then
               Error_Msg_N ("Object_Size cannot be given for &", Nam);

            elsif Duplicate_Clause then
               null;

            else
               Check_Size (Expr, U_Ent, Size, Biased);

               if No (Size) or else Size <= 0 then
                  Error_Msg_N ("Object_Size must be positive", Expr);

               elsif Is_Scalar_Type (U_Ent) then
                  if Size /= 8 and then Size /= 16 and then Size /= 32
                    and then UI_Mod (Size, 64) /= 0
                  then
                     Error_Msg_N
                       ("Object_Size must be 8, 16, 32, or multiple of 64",
                        Expr);
                  end if;

               elsif Size mod 8 /= 0 then
                  Error_Msg_N ("Object_Size must be a multiple of 8", Expr);
               end if;

               Set_Esize (U_Ent, Size);
               Set_Has_Object_Size_Clause (U_Ent);
               Alignment_Check_For_Size_Change (U_Ent, Size);
            end if;
         end Object_Size;

         ------------
         -- Output --
         ------------

         when Attribute_Output =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Output);
            Set_Has_Specified_Stream_Output (Ent);

         --------------
         -- Priority --
         --------------

         when Attribute_Priority =>

            --  Priority attribute definition clause not allowed except from
            --  aspect specification.

            if From_Aspect_Specification (N) then
               if not (Is_Concurrent_Type (U_Ent)
                        or else Ekind (U_Ent) = E_Procedure)
               then
                  Error_Msg_N
                    ("Priority can only be defined for task and protected "
                     & "object", Nam);

               elsif Duplicate_Clause then
                  null;

               else
                  --  The expression must be analyzed in the special manner
                  --  described in "Handling of Default and Per-Object
                  --  Expressions" in sem.ads.

                  --  The visibility to the components must be restored

                  Push_Type (U_Ent);
                  Preanalyze_Spec_Expression (Expr, Standard_Integer);
                  Pop_Type (U_Ent);

                  if not Is_OK_Static_Expression (Expr) then
                     Check_Restriction (Static_Priorities, Expr);
                  end if;
               end if;

            else
               Error_Msg_N
                 ("attribute& cannot be set with definition clause", N);
            end if;

         ---------------
         -- Put_Image --
         ---------------

         when Attribute_Put_Image =>
            Analyze_Put_Image_TSS_Definition;

         ----------
         -- Read --
         ----------

         when Attribute_Read =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Read);
            Set_Has_Specified_Stream_Read (Ent);

         --------------------------
         -- Scalar_Storage_Order --
         --------------------------

         --  Scalar_Storage_Order attribute definition clause

         when Attribute_Scalar_Storage_Order =>
            if not (Is_Record_Type (U_Ent) or else Is_Array_Type (U_Ent)) then
               Error_Msg_N
                 ("Scalar_Storage_Order can only be defined for record or "
                  & "array type", Nam);

            elsif Duplicate_Clause then
               null;

            else
               Analyze_And_Resolve (Expr, RTE (RE_Bit_Order));

               if Etype (Expr) = Any_Type then
                  return;

               elsif not Is_OK_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("Scalar_Storage_Order requires static expression!", Expr);

               elsif (Expr_Value (Expr) = 0) /= Bytes_Big_Endian then

                  --  Here for the case of a non-default (i.e. non-confirming)
                  --  Scalar_Storage_Order attribute definition.

                  if Support_Nondefault_SSO_On_Target then
                     Set_Reverse_Storage_Order (Base_Type (U_Ent), True);
                  else
                     Error_Msg_N
                       ("non-default Scalar_Storage_Order not supported on "
                        & "target", Expr);
                  end if;
               end if;

               --  Clear SSO default indications since explicit setting of the
               --  order overrides the defaults.

               Set_SSO_Set_Low_By_Default  (Base_Type (U_Ent), False);
               Set_SSO_Set_High_By_Default (Base_Type (U_Ent), False);
            end if;

         ------------------------
         -- Size or Value_Size --
         ------------------------

         --  Size or Value_Size attribute definition clause. These are treated
         --  the same, except that Size is allowed on objects, and Value_Size
         --  is allowed on nonfirst subtypes. First subtypes allow both Size
         --  and Value_Size; the treatment is the same for both.

         when Attribute_Size | Attribute_Value_Size => Size : declare
            Size   : constant Uint := Static_Integer (Expr);

            Attr_Name : constant String :=
              (if Id = Attribute_Size then "size"
               elsif Id = Attribute_Value_Size then "value size"
               else ""); -- can't happen
            --  Name of the attribute for printing in messages

            OK_Prefix : constant Boolean :=
              (if Id = Attribute_Size then
                Ekind (U_Ent) in Type_Kind | Constant_Or_Variable_Kind
               elsif Id = Attribute_Value_Size then
                Ekind (U_Ent) in Type_Kind
               else False); -- can't happen
            --  For X'Size, X can be a type or object; for X'Value_Size,
            --  X can be a type. Note that we already checked that 'Size
            --  can be specified only for a first subtype.

         begin
            FOnly := True;

            if not OK_Prefix then
               Error_Msg_N (Attr_Name & " cannot be given for &", Nam);

            elsif Duplicate_Clause then
               null;

            elsif Is_Array_Type (U_Ent)
              and then not Is_Constrained (U_Ent)
            then
               Error_Msg_N
                 (Attr_Name & " cannot be given for unconstrained array", Nam);

            elsif Present (Size) then
               declare
                  Etyp : constant Entity_Id :=
                    (if Is_Type (U_Ent) then U_Ent else Etype (U_Ent));

               begin
                  --  Check size, note that Gigi is in charge of checking that
                  --  the size of an array or record type is OK. Also we do not
                  --  check the size in the ordinary fixed-point case, since
                  --  it is too early to do so (there may be subsequent small
                  --  clause that affects the size). We can check the size if
                  --  a small clause has already been given.

                  if not Is_Ordinary_Fixed_Point_Type (U_Ent)
                    or else Has_Small_Clause (U_Ent)
                  then
                     declare
                        Biased : Boolean;
                     begin
                        Check_Size (Expr, Etyp, Size, Biased);
                        Set_Biased (U_Ent, N, Attr_Name & " clause", Biased);
                     end;
                  end if;

                  --  For types, set RM_Size and Esize if appropriate

                  if Is_Type (U_Ent) then
                     Set_RM_Size (U_Ent, Size);

                     --  If we are specifying the Size or Value_Size of a
                     --  first subtype, then for elementary types, increase
                     --  Object_Size to power of 2, but not less than a storage
                     --  unit in any case (normally this means it will be byte
                     --  addressable).

                     --  For all other types, nothing else to do, we leave
                     --  Esize (object size) unset; the back end will set it
                     --  from the size and alignment in an appropriate manner.

                     --  In both cases, we check whether the alignment must be
                     --  reset in the wake of the size change.

                     --  For nonfirst subtypes ('Value_Size only), we do
                     --  nothing here.

                     if Is_First_Subtype (U_Ent) then
                        if Is_Elementary_Type (U_Ent) then
                           if Size <= System_Storage_Unit then
                              Set_Esize
                                (U_Ent, UI_From_Int (System_Storage_Unit));
                           elsif Size <= 16 then
                              Set_Esize (U_Ent, Uint_16);
                           elsif Size <= 32 then
                              Set_Esize (U_Ent, Uint_32);
                           else
                              Set_Esize (U_Ent, (Size + 63) / 64 * 64);
                           end if;

                           Alignment_Check_For_Size_Change
                             (U_Ent, Esize (U_Ent));
                        else
                           Alignment_Check_For_Size_Change (U_Ent, Size);
                        end if;
                     end if;

                  --  For Object'Size, set Esize only

                  else
                     if Is_Elementary_Type (Etyp)
                       and then Size /= System_Storage_Unit
                       and then Size /= 16
                       and then Size /= 32
                       and then Size /= 64
                       and then Size /= System_Max_Integer_Size
                     then
                        Error_Msg_Uint_1 := UI_From_Int (System_Storage_Unit);
                        Error_Msg_Uint_2 :=
                          UI_From_Int (System_Max_Integer_Size);
                        Error_Msg_N
                          ("size for primitive object must be a power of 2 in "
                           & "the range ^-^", N);
                     end if;

                     Set_Esize (U_Ent, Size);
                  end if;

                  --  As of RM 13.1, only confirming size
                  --  (i.e. (Size = Esize (Etyp))) for aliased object of
                  --  elementary type must be supported.
                  --  GNAT rejects nonconfirming size for such object.

                  if Is_Aliased (U_Ent)
                    and then Is_Elementary_Type (Etyp)
                    and then Known_Esize (U_Ent)
                    and then Size /= Esize (Etyp)
                  then
                     Error_Msg_N
                       ("nonconfirming Size for aliased object is not "
                        & "supported", N);
                  end if;

                  --  Handle extension aspect 'Size'Class which allows for
                  --  "mutably tagged" types.

                  if Ekind (Etyp) = E_Class_Wide_Type then
                     Error_Msg_GNAT_Extension
                       ("attribute size class", Sloc (N));

                     --  Check for various restrictions applied to mutably
                     --  tagged types.

                     if Is_Derived_Type (Etype (Etyp)) then
                        Error_Msg_N
                          ("cannot be specified on derived types", Nam);

                     elsif Ekind (Etype (Prefix (Nam))) = E_Record_Subtype then
                        Error_Msg_N
                          ("cannot be specified on a subtype", Nam);

                     elsif Is_Interface (Etype (Etyp)) then
                        Error_Msg_N
                          ("cannot be specified on interface types", Nam);

                     elsif Has_Discriminants (Etype (Etyp)) then
                        Error_Msg_N
                          ("cannot be specified on discriminated type", Nam);

                     elsif Present (Incomplete_Or_Partial_View (Etype (Etyp)))
                       and then Is_Tagged_Type
                                  (Incomplete_Or_Partial_View (Etype (Etyp)))
                     then
                        Error_Msg_N
                          ("cannot be specified on a type whose partial view"
                           & " is tagged", Nam);

                     --  Otherwise, the declaration is valid

                     else
                        declare
                           Actions : List_Id;
                        begin
                           --  Generate our class-wide equivalent type which
                           --  is sized according to the value specified by
                           --  'Size'Class.

                           Set_Class_Wide_Equivalent_Type (Etyp,
                             Make_CW_Equivalent_Type (Etyp, Empty, Actions));

                           --  Add a Compile_Time_Error sizing check as a hint
                           --  to the backend.

                           Append_To (Actions,
                             Make_CW_Size_Compile_Check
                               (Etype (Etyp), U_Ent));

                           --  Set the expansion to occur during freezing when
                           --  everything is analyzed

                           Append_Freeze_Actions (Etyp, Actions);

                           Set_Is_Mutably_Tagged_Type (Etyp);
                        end;
                     end if;
                  end if;

                  Set_Has_Size_Clause (U_Ent);
               end;
            end if;
         end Size;

         -----------
         -- Small --
         -----------

         --  Small attribute definition clause

         when Attribute_Small => Small : declare
            Implicit_Base : constant Entity_Id := Base_Type (U_Ent);
            Small         : Ureal;

         begin
            Analyze_And_Resolve (Expr, Any_Real);

            if Etype (Expr) = Any_Type then
               return;

            elsif not Is_OK_Static_Expression (Expr) then
               Flag_Non_Static_Expr
                 ("small requires static expression!", Expr);
               return;

            else
               Small := Expr_Value_R (Expr);

               if Small <= Ureal_0 then
                  Error_Msg_N ("small value must be greater than zero", Expr);
                  return;
               end if;

            end if;

            if not Is_Ordinary_Fixed_Point_Type (U_Ent) then
               Error_Msg_N
                 ("small requires an ordinary fixed point type", Nam);

            elsif Has_Small_Clause (U_Ent) then
               Error_Msg_N ("small already given for &", Nam);

            elsif Small > Delta_Value (U_Ent) then
               Error_Msg_N
                 ("small value must not be greater than delta value", Nam);

            else
               Set_Small_Value (U_Ent, Small);
               Set_Small_Value (Implicit_Base, Small);
               Set_Has_Small_Clause (U_Ent);
               Set_Has_Small_Clause (Implicit_Base);
               Set_Has_Non_Standard_Rep (Implicit_Base);
            end if;
         end Small;

         ------------------
         -- Storage_Pool --
         ------------------

         --  Storage_Pool attribute definition clause

         when Attribute_Simple_Storage_Pool
            | Attribute_Storage_Pool
         =>
         Storage_Pool : declare
            Pool : Entity_Id;
            T    : Entity_Id;

            procedure Associate_Storage_Pool
              (Ent : Entity_Id; Pool : Entity_Id);
            --  Associate Pool to Ent and perform legality checks on subpools

            ----------------------------
            -- Associate_Storage_Pool --
            ----------------------------

            procedure Associate_Storage_Pool
              (Ent : Entity_Id; Pool : Entity_Id)
            is
               function Object_From (Pool : Entity_Id) return Entity_Id;
               --  Return the entity of which Pool is a part of

               -----------------
               -- Object_From --
               -----------------

               function Object_From
                 (Pool : Entity_Id) return Entity_Id
               is
                  N : Node_Id := Pool;
               begin
                  if Present (Renamed_Object (Pool)) then
                     N := Renamed_Object (Pool);
                  end if;

                  while Present (N) loop
                     case Nkind (N) is
                        when N_Defining_Identifier =>
                           return N;

                        when N_Identifier | N_Expanded_Name =>
                           return Entity (N);

                        when N_Indexed_Component | N_Selected_Component |
                             N_Explicit_Dereference
                        =>
                           N := Prefix (N);

                        when N_Type_Conversion =>
                           N := Expression (N);

                        when others =>
                           --  ??? we probably should handle more cases but
                           --  this is good enough in practice for this check
                           --  on a corner case.

                           return Empty;
                     end case;
                  end loop;

                  return Empty;
               end Object_From;

               Obj : Entity_Id;

            begin
               Set_Associated_Storage_Pool (Ent, Pool);

               --  Check RM 13.11.4(22-23/3): a specification of a storage pool
               --  is illegal if the storage pool supports subpools and:
               --  (A) The access type is a general access type.
               --  (B) The access type is statically deeper than the storage
               --      pool object;
               --  (C) The storage pool object is a part of a formal parameter;
               --  (D) The storage pool object is a part of the dereference of
               --      a non-library level general access type;

               if Ada_Version >= Ada_2012
                 and then RTU_Loaded (System_Storage_Pools_Subpools)
                 and then
                   Is_Ancestor (RTE (RE_Root_Storage_Pool_With_Subpools),
                                Etype (Pool))
               then
                  --  check (A)

                  if Ekind (Etype (Ent)) = E_General_Access_Type then
                     Error_Msg_N
                       ("subpool cannot be used on general access type", Ent);
                  end if;

                  --  check (B)

                  if Type_Access_Level (Ent)
                       > Static_Accessibility_Level
                           (Pool, Object_Decl_Level)
                  then
                     Error_Msg_N
                       ("subpool access type has deeper accessibility "
                        & "level than pool", Ent);
                     return;
                  end if;

                  Obj := Object_From (Pool);

                  --  check (C)

                  if Present (Obj) and then Is_Formal (Obj) then
                     Error_Msg_N
                       ("subpool cannot be part of a parameter", Ent);
                     return;
                  end if;

                  --  check (D)

                  if Present (Obj)
                    and then Ekind (Etype (Obj)) = E_General_Access_Type
                    and then not Is_Library_Level_Entity (Etype (Obj))
                  then
                     Error_Msg_N
                       ("subpool cannot be part of the dereference of a " &
                        "nested general access type", Ent);
                     return;
                  end if;
               end if;
            end Associate_Storage_Pool;

         begin
            if Ekind (U_Ent) = E_Access_Subprogram_Type then
               Error_Msg_N
                 ("storage pool cannot be given for access-to-subprogram type",
                  Nam);
               return;

            elsif Ekind (U_Ent) not in E_Access_Type | E_General_Access_Type
            then
               Error_Msg_N
                 ("storage pool can only be given for access types", Nam);
               return;

            elsif Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("storage pool cannot be given for a derived access type",
                  Nam);

            elsif Duplicate_Clause then
               return;

            elsif Present (Associated_Storage_Pool (U_Ent)) then
               Error_Msg_N ("storage pool already given for &", Nam);
               return;
            end if;

            --  Check for Storage_Size previously given

            declare
               SS : constant Node_Id :=
                      Get_Attribute_Definition_Clause
                        (U_Ent, Attribute_Storage_Size);
            begin
               if Present (SS) then
                  Check_Pool_Size_Clash (U_Ent, N, SS);
               end if;
            end;

            --  Storage_Pool case

            if Id = Attribute_Storage_Pool then
               Analyze_And_Resolve
                 (Expr, Class_Wide_Type (RTE (RE_Root_Storage_Pool)));

            --  In the Simple_Storage_Pool case, we allow a variable of any
            --  simple storage pool type, so we Resolve without imposing an
            --  expected type.

            else
               Analyze_And_Resolve (Expr);

               if No (Get_Rep_Pragma
                                 (Etype (Expr), Name_Simple_Storage_Pool_Type))
               then
                  Error_Msg_N
                    ("expression must be of a simple storage pool type", Expr);
               end if;
            end if;

            if not Denotes_Variable (Expr) then
               Error_Msg_N ("storage pool must be a variable", Expr);
               return;
            end if;

            if Nkind (Expr) = N_Type_Conversion then
               T := Etype (Expression (Expr));
            else
               T := Etype (Expr);
            end if;

            --  The Stack_Bounded_Pool is used internally for implementing
            --  access types with a Storage_Size. Since it only work properly
            --  when used on one specific type, we need to check that it is not
            --  hijacked improperly:

            --    type T is access Integer;
            --    for T'Storage_Size use n;
            --    type Q is access Float;
            --    for Q'Storage_Size use T'Storage_Size; -- incorrect

            if Is_RTE (Base_Type (T), RE_Stack_Bounded_Pool) then
               Error_Msg_N ("non-shareable internal Pool", Expr);
               return;
            end if;

            --  Validate_Remote_Access_To_Class_Wide_Type for attribute
            --  Storage_Pool since this attribute cannot be defined for such
            --  types (RM E.2.2(17)).

            Validate_Remote_Access_To_Class_Wide_Type (N);

            --  If the argument is a name that is not an entity name, then
            --  we construct a renaming operation to define an entity of
            --  type storage pool.

            if not Is_Entity_Name (Expr)
              and then Is_Object_Reference (Expr)
            then
               Pool := Make_Temporary (Loc, 'P', Expr);

               declare
                  Rnode : constant Node_Id :=
                            Make_Object_Renaming_Declaration (Loc,
                              Defining_Identifier => Pool,
                              Subtype_Mark        =>
                                New_Occurrence_Of (Etype (Expr), Loc),
                              Name                => Expr);

               begin
                  --  If the attribute definition clause comes from an aspect
                  --  clause, then insert the renaming before the associated
                  --  entity's declaration, since the attribute clause has
                  --  not yet been appended to the declaration list.

                  if From_Aspect_Specification (N) then
                     Insert_Before (Parent (Entity (N)), Rnode);
                  else
                     Insert_Before (N, Rnode);
                  end if;

                  Analyze (Rnode);
                  Associate_Storage_Pool (U_Ent, Pool);
               end;

            elsif Is_Entity_Name (Expr) then
               Pool := Entity (Expr);

               --  If pool is a renamed object, get original one. This can
               --  happen with an explicit renaming, and within instances.

               while Present (Renamed_Object (Pool))
                 and then Is_Entity_Name (Renamed_Object (Pool))
               loop
                  Pool := Entity (Renamed_Object (Pool));
               end loop;

               if Present (Renamed_Object (Pool))
                 and then Nkind (Renamed_Object (Pool)) = N_Type_Conversion
                 and then Is_Entity_Name (Expression (Renamed_Object (Pool)))
               then
                  Pool := Entity (Expression (Renamed_Object (Pool)));
               end if;

               Associate_Storage_Pool (U_Ent, Pool);

            elsif Nkind (Expr) = N_Type_Conversion
              and then Is_Entity_Name (Expression (Expr))
              and then Nkind (Original_Node (Expr)) = N_Attribute_Reference
            then
               Pool := Entity (Expression (Expr));
               Associate_Storage_Pool (U_Ent, Pool);

            else
               Error_Msg_N ("incorrect reference to a Storage Pool", Expr);
               return;
            end if;
         end Storage_Pool;

         ------------------
         -- Storage_Size --
         ------------------

         --  Storage_Size attribute definition clause

         when Attribute_Storage_Size => Storage_Size : declare
            Btype : constant Entity_Id := Base_Type (U_Ent);

         begin
            if Is_Task_Type (U_Ent) then

               --  Check obsolescent (but never obsolescent if from aspect)

               if not From_Aspect_Specification (N) then
                  Check_Restriction (No_Obsolescent_Features, N);

                  if Warn_On_Obsolescent_Feature then
                     Error_Msg_N
                       ("?j?storage size clause for task is an obsolescent "
                        & "feature (RM J.9)", N);
                     Error_Msg_N ("\?j?use Storage_Size pragma instead", N);
                  end if;
               end if;

               FOnly := True;
            end if;

            if not Is_Access_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Task_Type
            then
               Error_Msg_N ("storage size cannot be given for &", Nam);

            elsif Is_Access_Type (U_Ent) and Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("storage size cannot be given for a derived access type",
                  Nam);

            elsif Duplicate_Clause then
               null;

            else
               --  Validate_Remote_Access_To_Class_Wide_Type for attribute
               --  Storage_Size since this attribute cannot be defined for such
               --  types (RM E.2.2(17)).

               Validate_Remote_Access_To_Class_Wide_Type (N);

               Analyze_And_Resolve (Expr, Any_Integer);

               if Is_Access_Type (U_Ent) then

                  --  Check for Storage_Pool previously given

                  declare
                     SP : constant Node_Id :=
                            Get_Attribute_Definition_Clause
                              (U_Ent, Attribute_Storage_Pool);

                  begin
                     if Present (SP) then
                        Check_Pool_Size_Clash (U_Ent, SP, N);
                     end if;
                  end;

                  --  Special case of for x'Storage_Size use 0

                  if Is_OK_Static_Expression (Expr)
                    and then Expr_Value (Expr) = 0
                  then
                     Set_No_Pool_Assigned (Btype);
                  end if;
               end if;

               Set_Has_Storage_Size_Clause (Btype);
            end if;
         end Storage_Size;

         -----------------
         -- Stream_Size --
         -----------------

         when Attribute_Stream_Size => Stream_Size : declare
            Size : constant Uint := Static_Integer (Expr);

         begin
            if Ada_Version <= Ada_95 then
               Check_Restriction (No_Implementation_Attributes, N);
            end if;

            if Duplicate_Clause then
               null;

            elsif Is_Elementary_Type (U_Ent) then
               --  Size will be empty if we already detected an error
               --  (e.g. Expr is of the wrong type); we might as well
               --  give the useful hint below even in that case.

               if No (Size) or else
                 (Size /= System_Storage_Unit
                  and then Size /= System_Storage_Unit * 2
                  and then Size /= System_Storage_Unit * 3
                  and then Size /= System_Storage_Unit * 4
                  and then Size /= System_Storage_Unit * 8)
               then
                  Error_Msg_N
                    ("stream size for elementary type must be 8, 16, 24, " &
                     "32 or 64", N);

               elsif Known_RM_Size (U_Ent) and then RM_Size (U_Ent) > Size then
                  Error_Msg_Uint_1 := RM_Size (U_Ent);
                  Error_Msg_N
                    ("stream size for elementary type must be 8, 16, 24, " &
                     "32 or 64 and at least ^", N);
               end if;

               Set_Has_Stream_Size_Clause (U_Ent);

            else
               Error_Msg_N ("Stream_Size cannot be given for &", Nam);
            end if;
         end Stream_Size;

         -----------------------
         -- Variable_Indexing --
         -----------------------

         when Attribute_Variable_Indexing =>
            Check_Indexing_Functions;

         -----------
         -- Write --
         -----------

         when Attribute_Write =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Write);
            Set_Has_Specified_Stream_Write (Ent);

         --  All other attributes cannot be set

         when others =>
            Error_Msg_N
              ("attribute& cannot be set with definition clause", N);
      end case;

      --  The test for the type being frozen must be performed after any
      --  expression the clause has been analyzed since the expression itself
      --  might cause freezing that makes the clause illegal.

      if Rep_Item_Too_Late (U_Ent, N, FOnly) then
         return;
      end if;
   end Analyze_Attribute_Definition_Clause;

   ----------------------------
   -- Analyze_Code_Statement --
   ----------------------------

   procedure Analyze_Code_Statement (N : Node_Id) is
      HSS   : constant Node_Id   := Parent (N);
      SBody : constant Node_Id   := Parent (HSS);
      Subp  : constant Entity_Id := Current_Scope;
      Stmt  : Node_Id;
      Decl  : Node_Id;
      StmtO : Node_Id;
      DeclO : Node_Id;

   begin
      --  Accept foreign code statements for CodePeer. The analysis is skipped
      --  to avoid rejecting unrecognized constructs.

      if CodePeer_Mode then
         Set_Analyzed (N);
         return;
      end if;

      --  Analyze and check we get right type, note that this implements the
      --  requirement (RM 13.8(1)) that Machine_Code be with'ed, since that is
      --  the only way that Asm_Insn could possibly be visible.

      Analyze_And_Resolve (Expression (N));

      if Etype (Expression (N)) = Any_Type then
         return;
      elsif not Is_RTE (Etype (Expression (N)), RE_Asm_Insn) then
         Error_Msg_N ("incorrect type for code statement", N);
         return;
      end if;

      Check_Code_Statement (N);

      --  Make sure we appear in the handled statement sequence of a subprogram
      --  (RM 13.8(3)).

      if Nkind (HSS) /= N_Handled_Sequence_Of_Statements
        or else Nkind (SBody) /= N_Subprogram_Body
      then
         Error_Msg_N
           ("code statement can only appear in body of subprogram", N);
         return;
      end if;

      --  Do remaining checks (RM 13.8(3)) if not already done

      if not Is_Machine_Code_Subprogram (Subp) then
         Set_Is_Machine_Code_Subprogram (Subp);

         --  No exception handlers allowed

         if Present (Exception_Handlers (HSS)) then
            Error_Msg_N
              ("exception handlers not permitted in machine code subprogram",
               First (Exception_Handlers (HSS)));
         end if;

         --  No declarations other than use clauses and pragmas (we allow
         --  certain internally generated declarations as well).

         Decl := First (Declarations (SBody));
         while Present (Decl) loop
            DeclO := Original_Node (Decl);
            if Comes_From_Source (DeclO)
              and Nkind (DeclO) not in N_Pragma
                                     | N_Use_Package_Clause
                                     | N_Use_Type_Clause
                                     | N_Implicit_Label_Declaration
            then
               Error_Msg_N
                 ("this declaration is not allowed in machine code subprogram",
                  DeclO);
            end if;

            Next (Decl);
         end loop;

         --  No statements other than code statements, pragmas, and labels.
         --  Again we allow certain internally generated statements.

         --  In Ada 2012, qualified expressions are names, and the code
         --  statement is initially parsed as a procedure call.

         Stmt := First (Statements (HSS));
         while Present (Stmt) loop
            StmtO := Original_Node (Stmt);

            --  A procedure call transformed into a code statement is OK

            if Ada_Version >= Ada_2012
              and then Nkind (StmtO) = N_Procedure_Call_Statement
              and then Nkind (Name (StmtO)) = N_Qualified_Expression
            then
               null;

            elsif Comes_From_Source (StmtO)
              and then Nkind (StmtO) not in
                         N_Pragma | N_Label | N_Code_Statement
            then
               Error_Msg_N
                 ("this statement is not allowed in machine code subprogram",
                  StmtO);
            end if;

            Next (Stmt);
         end loop;
      end if;
   end Analyze_Code_Statement;

   -----------------------------------------------
   -- Analyze_Enumeration_Representation_Clause --
   -----------------------------------------------

   procedure Analyze_Enumeration_Representation_Clause (N : Node_Id) is
      Ident    : constant Node_Id := Identifier (N);
      Aggr     : constant Node_Id := Array_Aggregate (N);
      Enumtype : Entity_Id;
      Elit     : Entity_Id;
      Expr     : Node_Id;
      Assoc    : Node_Id;
      Choice   : Node_Id;
      Val      : Uint;

      Err : Boolean := False;
      --  Set True to avoid cascade errors and crashes on incorrect source code

      Lo : constant Uint := Expr_Value (Type_Low_Bound (Universal_Integer));
      Hi : constant Uint := Expr_Value (Type_High_Bound (Universal_Integer));
      --  Allowed range of universal integer (= allowed range of enum lit vals)

      Min : Uint;
      Max : Uint;
      --  Minimum and maximum values of entries

      Max_Node : Node_Id := Empty; -- init to avoid warning
      --  Pointer to node for literal providing max value

   begin
      if Ignore_Rep_Clauses then
         Kill_Rep_Clause (N);
         return;
      end if;

      --  Ignore enumeration rep clauses by default in CodePeer mode,
      --  unless -gnatd.I is specified, as a work around for potential false
      --  positive messages.

      if CodePeer_Mode and not Debug_Flag_Dot_II then
         return;
      end if;

      --  First some basic error checks

      Find_Type (Ident);
      Enumtype := Entity (Ident);

      if Enumtype = Any_Type
        or else Rep_Item_Too_Early (Enumtype, N)
      then
         return;
      else
         Enumtype := Underlying_Type (Enumtype);
      end if;

      if not Is_Enumeration_Type (Enumtype) then
         Error_Msg_NE
           ("enumeration type required, found}",
            Ident, First_Subtype (Enumtype));
         return;
      end if;

      --  Ignore rep clause on generic actual type. This will already have
      --  been flagged on the template as an error, and this is the safest
      --  way to ensure we don't get a junk cascaded message in the instance.

      if Is_Generic_Actual_Type (Enumtype) then
         return;

      --  Type must be in current scope

      elsif Scope (Enumtype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", Ident);
         return;

      --  Type must be a first subtype

      elsif not Is_First_Subtype (Enumtype) then
         Error_Msg_N ("cannot give enumeration rep clause for subtype", N);
         return;

      --  Ignore duplicate rep clause

      elsif Has_Enumeration_Rep_Clause (Enumtype) then
         Error_Msg_N ("duplicate enumeration rep clause ignored", N);
         return;

      --  Don't allow rep clause for standard [wide_[wide_]]character

      elsif Is_Standard_Character_Type (Enumtype) then
         Error_Msg_N ("enumeration rep clause not allowed for this type", N);
         return;

      --  Check that the expression is a proper aggregate (no parentheses)

      elsif Paren_Count (Aggr) /= 0 then
         Error_Msg_F
           ("extra parentheses surrounding aggregate not allowed", Aggr);
         return;

      --  Reject the mixing of named and positional entries in the aggregate

      elsif Present (Expressions (Aggr))
        and then Present (Component_Associations (Aggr))
      then
         Error_Msg_N ("cannot mix positional and named entries in "
                       & "enumeration rep clause", N);
         return;

      --  All tests passed, so set rep clause in place

      else
         Set_Has_Enumeration_Rep_Clause (Enumtype);
         Set_Has_Enumeration_Rep_Clause (Base_Type (Enumtype));
      end if;

      --  Now we process the aggregate. Note that we don't use the normal
      --  aggregate code for this purpose, because we don't want any of the
      --  normal expansion activities, and a number of special semantic
      --  rules apply (including the component type being any integer type)

      Elit := First_Literal (Enumtype);

      --  Process positional entries

      if Present (Expressions (Aggr)) then
         Expr := First (Expressions (Aggr));
         while Present (Expr) loop
            if No (Elit) then
               Error_Msg_N ("too many entries in aggregate", Expr);
               return;
            end if;

            Val := Static_Integer (Expr);

            --  Err signals that we found some incorrect entries processing
            --  the list. The final checks for completeness and ordering are
            --  skipped in this case.

            if No (Val) then
               Err := True;

            elsif Val < Lo or else Hi < Val then
               Error_Msg_N ("value outside permitted range", Expr);
               Err := True;

            else
               Set_Enumeration_Rep (Elit, Val);
               Set_Enumeration_Rep_Expr (Elit, Expr);
            end if;

            Next (Expr);
            Next (Elit);
         end loop;

      --  Process named entries

      elsif Present (Component_Associations (Aggr)) then
         Assoc := First (Component_Associations (Aggr));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));

            if Present (Next (Choice)) then
               Error_Msg_N
                 ("multiple choice not allowed here", Next (Choice));
               Err := True;
            end if;

            if Nkind (Choice) = N_Others_Choice then
               Error_Msg_N ("OTHERS choice not allowed here", Choice);
               Err := True;

            elsif Nkind (Choice) = N_Range then

               --  ??? should allow zero/one element range here

               Error_Msg_N ("range not allowed here", Choice);
               Err := True;

            else
               Analyze_And_Resolve (Choice, Enumtype);

               if Error_Posted (Choice) then
                  Err := True;
               end if;

               if not Err then
                  if Is_Entity_Name (Choice)
                    and then Is_Type (Entity (Choice))
                  then
                     Error_Msg_N ("subtype name not allowed here", Choice);
                     Err := True;

                     --  ??? should allow static subtype with zero/one entry

                  elsif Etype (Choice) = Base_Type (Enumtype) then
                     if not Is_OK_Static_Expression (Choice) then
                        Flag_Non_Static_Expr
                          ("non-static expression used for choice!", Choice);
                        Err := True;

                     else
                        Elit := Expr_Value_E (Choice);

                        if Present (Enumeration_Rep_Expr (Elit)) then
                           Error_Msg_Sloc :=
                             Sloc (Enumeration_Rep_Expr (Elit));
                           Error_Msg_NE
                             ("representation for& previously given#",
                              Choice, Elit);
                           Err := True;
                        end if;

                        Set_Enumeration_Rep_Expr (Elit, Expression (Assoc));

                        Expr := Expression (Assoc);
                        Val := Static_Integer (Expr);

                        if No (Val) then
                           Err := True;

                        elsif Val < Lo or else Hi < Val then
                           Error_Msg_N ("value outside permitted range", Expr);
                           Err := True;

                        else
                           Set_Enumeration_Rep (Elit, Val);
                        end if;
                     end if;
                  end if;
               end if;
            end if;

            Next (Assoc);
         end loop;
      end if;

      --  Aggregate is fully processed. Now we check that a full set of
      --  representations was given, and that they are in range and in order.
      --  These checks are only done if no other errors occurred.

      if not Err then
         Min  := No_Uint;
         Max  := No_Uint;

         Elit := First_Literal (Enumtype);
         while Present (Elit) loop
            if No (Enumeration_Rep_Expr (Elit)) then
               Error_Msg_NE ("missing representation for&!", N, Elit);

            else
               Val := Enumeration_Rep (Elit);

               if No (Min) then
                  Min := Val;
               end if;

               if Present (Val) then
                  if Present (Max) and then Val <= Max then
                     Error_Msg_NE
                       ("enumeration value for& not ordered!",
                        Enumeration_Rep_Expr (Elit), Elit);
                  end if;

                  Max_Node := Enumeration_Rep_Expr (Elit);
                  Max := Val;
               end if;

               --  If there is at least one literal whose representation is not
               --  equal to the Pos value, then note that this enumeration type
               --  has a non-standard representation.

               if Val /= Enumeration_Pos (Elit) then
                  Set_Has_Non_Standard_Rep (Base_Type (Enumtype));
               end if;
            end if;

            Next (Elit);
         end loop;

         --  Now set proper size information

         declare
            Minsize : Uint := UI_From_Int (Minimum_Size (Enumtype));

         begin
            if Has_Size_Clause (Enumtype) then

               --  All OK, if size is OK now

               if RM_Size (Enumtype) >= Minsize then
                  null;

               else
                  --  Try if we can get by with biasing

                  Minsize :=
                    UI_From_Int (Minimum_Size (Enumtype, Biased => True));

                  --  Error message if even biasing does not work

                  if RM_Size (Enumtype) < Minsize then
                     Error_Msg_Uint_1 := RM_Size (Enumtype);
                     Error_Msg_Uint_2 := Max;
                     Error_Msg_N
                       ("previously given size (^) is too small "
                        & "for this value (^)", Max_Node);

                  --  If biasing worked, indicate that we now have biased rep

                  else
                     Set_Biased
                       (Enumtype, Size_Clause (Enumtype), "size clause");
                  end if;
               end if;

            else
               Set_RM_Size    (Enumtype, Minsize);
               Set_Enum_Esize (Enumtype);
            end if;

            Set_RM_Size (Base_Type (Enumtype), RM_Size   (Enumtype));
            Set_Esize   (Base_Type (Enumtype), Esize     (Enumtype));

            Copy_Alignment (To => Base_Type (Enumtype), From => Enumtype);
         end;
      end if;

      --  We repeat the too late test in case it froze itself

      if Rep_Item_Too_Late (Enumtype, N) then
         null;
      end if;
   end Analyze_Enumeration_Representation_Clause;

   ----------------------------
   -- Analyze_Free_Statement --
   ----------------------------

   procedure Analyze_Free_Statement (N : Node_Id) is
   begin
      Analyze (Expression (N));
   end Analyze_Free_Statement;

   ---------------------------
   -- Analyze_Freeze_Entity --
   ---------------------------

   procedure Analyze_Freeze_Entity (N : Node_Id) is
   begin
      Freeze_Entity_Checks (N);
   end Analyze_Freeze_Entity;

   -----------------------------------
   -- Analyze_Freeze_Generic_Entity --
   -----------------------------------

   procedure Analyze_Freeze_Generic_Entity (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      if not Is_Frozen (E) and then Has_Delayed_Aspects (E) then
         Analyze_Aspects_At_Freeze_Point (E);
      end if;

      Freeze_Entity_Checks (N);
   end Analyze_Freeze_Generic_Entity;

   ------------------------------------------
   -- Analyze_Record_Representation_Clause --
   ------------------------------------------

   --  Note: we check as much as we can here, but we can't do any checks
   --  based on the position values (e.g. overlap checks) until freeze time
   --  because especially in Ada 2005 (machine scalar mode), the processing
   --  for non-standard bit order can substantially change the positions.
   --  See procedure Check_Record_Representation_Clause (called from Freeze)
   --  for the remainder of this processing.

   procedure Analyze_Record_Representation_Clause (N : Node_Id) is
      Ident   : constant Node_Id := Identifier (N);
      Biased  : Boolean;
      CC      : Node_Id;
      Comp    : Entity_Id;
      Fbit    : Uint;
      Lbit    : Uint;
      Ocomp   : Entity_Id;
      Posit   : Uint;
      Rectype : Entity_Id;
      Recdef  : Node_Id;

      function Is_Inherited (Comp : Entity_Id) return Boolean;
      --  True if Comp is an inherited component in a record extension

      ------------------
      -- Is_Inherited --
      ------------------

      function Is_Inherited (Comp : Entity_Id) return Boolean is
         Comp_Base : Entity_Id;

      begin
         if Ekind (Rectype) = E_Record_Subtype then
            Comp_Base := Original_Record_Component (Comp);
         else
            Comp_Base := Comp;
         end if;

         return Comp_Base /= Original_Record_Component (Comp_Base);
      end Is_Inherited;

      --  Local variables

      Is_Record_Extension : Boolean;
      --  True if Rectype is a record extension

      CR_Pragma : Node_Id := Empty;
      --  Points to N_Pragma node if Complete_Representation pragma present

   --  Start of processing for Analyze_Record_Representation_Clause

   begin
      if Ignore_Rep_Clauses then
         Kill_Rep_Clause (N);
         return;
      end if;

      Find_Type (Ident);
      Rectype := Entity (Ident);

      if Rectype = Any_Type or else Rep_Item_Too_Early (Rectype, N) then
         return;
      else
         Rectype := Underlying_Type (Rectype);
      end if;

      --  First some basic error checks

      if not Is_Record_Type (Rectype) then
         Error_Msg_NE
           ("record type required, found}", Ident, First_Subtype (Rectype));
         return;

      elsif Scope (Rectype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", N);
         return;

      elsif not Is_First_Subtype (Rectype) then
         Error_Msg_N ("cannot give record rep clause for subtype", N);
         return;

      elsif Has_Record_Rep_Clause (Rectype) then
         Error_Msg_N ("duplicate record rep clause ignored", N);
         return;

      elsif Rep_Item_Too_Late (Rectype, N) then
         return;
      end if;

      --  We know we have a first subtype, now possibly go to the anonymous
      --  base type to determine whether Rectype is a record extension.

      Recdef := Type_Definition (Declaration_Node (Base_Type (Rectype)));
      Is_Record_Extension :=
        Nkind (Recdef) = N_Derived_Type_Definition
          and then Present (Record_Extension_Part (Recdef));

      if Present (Mod_Clause (N)) then
         declare
            M      : constant Node_Id := Mod_Clause (N);
            P      : constant List_Id := Pragmas_Before (M);
            Ignore : Uint;

         begin
            Check_Restriction (No_Obsolescent_Features, Mod_Clause (N));

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("?j?mod clause is an obsolescent feature (RM J.8)", N);
               Error_Msg_N
                 ("\?j?use alignment attribute definition clause instead", N);
            end if;

            if Present (P) then
               Analyze_List (P);
            end if;

            --  Get the alignment value to perform error checking

            Ignore := Get_Alignment_Value (Expression (M));
         end;
      end if;

      --  For untagged types, clear any existing component clauses for the
      --  type. If the type is derived, this is what allows us to override
      --  a rep clause for the parent. For type extensions, the representation
      --  of the inherited components is inherited, so we want to keep previous
      --  component clauses for completeness.

      if not Is_Tagged_Type (Rectype) then
         Comp := First_Component_Or_Discriminant (Rectype);
         while Present (Comp) loop
            Set_Component_Clause (Comp, Empty);
            Next_Component_Or_Discriminant (Comp);
         end loop;
      end if;

      --  All done if no component clauses

      CC := First (Component_Clauses (N));

      if No (CC) then
         return;
      end if;

      --  A representation like this applies to the base type

      Set_Has_Record_Rep_Clause (Base_Type (Rectype));
      Set_Has_Non_Standard_Rep  (Base_Type (Rectype));
      Set_Has_Specified_Layout  (Base_Type (Rectype));

      --  Process the component clauses

      while Present (CC) loop

         --  Pragma

         if Nkind (CC) = N_Pragma then
            Analyze (CC);

            --  The only pragma of interest is Complete_Representation

            if Pragma_Name (CC) = Name_Complete_Representation then
               CR_Pragma := CC;
            end if;

         --  Processing for real component clause

         else
            Posit := Static_Integer (Position  (CC));
            Fbit  := Static_Integer (First_Bit (CC));
            Lbit  := Static_Integer (Last_Bit  (CC));

            if Present (Posit)
              and then Present (Fbit)
              and then Present (Lbit)
            then
               if Posit < 0 then
                  Error_Msg_N ("position cannot be negative", Position (CC));

               elsif Fbit < 0 then
                  Error_Msg_N ("first bit cannot be negative", First_Bit (CC));

               --  The Last_Bit specified in a component clause must not be
               --  less than the First_Bit minus one (RM-13.5.1(10)).

               elsif Lbit < Fbit - 1 then
                  Error_Msg_N
                    ("last bit cannot be less than first bit minus one",
                     Last_Bit (CC));

               --  Values look OK, so find the corresponding record component
               --  Even though the syntax allows an attribute reference for
               --  implementation-defined components, GNAT does not allow the
               --  tag to get an explicit position.

               elsif Nkind (Component_Name (CC)) = N_Attribute_Reference then
                  if Attribute_Name (Component_Name (CC)) = Name_Tag then
                     Error_Msg_N ("position of tag cannot be specified", CC);
                  else
                     Error_Msg_N ("illegal component name", CC);
                  end if;

               else
                  Comp := First_Entity (Rectype);
                  while Present (Comp) loop
                     exit when Chars (Comp) = Chars (Component_Name (CC));
                     Next_Entity (Comp);
                  end loop;

                  if No (Comp) then

                     --  Maybe component of base type that is absent from
                     --  statically constrained first subtype.

                     Comp := First_Entity (Base_Type (Rectype));
                     while Present (Comp) loop
                        exit when Chars (Comp) = Chars (Component_Name (CC));
                        Next_Entity (Comp);
                     end loop;
                  end if;

                  if No (Comp) then
                     Error_Msg_N
                       ("component clause is for non-existent field", CC);

                  --  Ada 2012 (AI05-0026): Any name that denotes a
                  --  discriminant of an object of an unchecked union type
                  --  shall not occur within a record_representation_clause.

                  --  The general restriction of using record rep clauses on
                  --  Unchecked_Union types has now been lifted. Since it is
                  --  possible to introduce a record rep clause which mentions
                  --  the discriminant of an Unchecked_Union in non-Ada 2012
                  --  code, this check is applied to all versions of the
                  --  language.

                  elsif Ekind (Comp) = E_Discriminant
                    and then Is_Unchecked_Union (Rectype)
                  then
                     Error_Msg_N
                       ("cannot reference discriminant of unchecked union",
                        Component_Name (CC));

                  elsif Is_Record_Extension and then Is_Inherited (Comp) then
                     Error_Msg_NE
                       ("component clause not allowed for inherited "
                        & "component&", CC, Comp);

                  elsif Present (Component_Clause (Comp)) then

                     --  Diagnose duplicate rep clause, or check consistency
                     --  if this is an inherited component. In a double fault,
                     --  there may be a duplicate inconsistent clause for an
                     --  inherited component.

                     if Scope (Original_Record_Component (Comp)) = Rectype
                       or else Parent (Component_Clause (Comp)) = N
                     then
                        Error_Msg_Sloc := Sloc (Component_Clause (Comp));
                        Error_Msg_N ("component clause previously given#", CC);

                     else
                        declare
                           Rep1 : constant Node_Id := Component_Clause (Comp);
                        begin
                           if Intval (Position (Rep1)) /=
                                                   Intval (Position (CC))
                             or else Intval (First_Bit (Rep1)) /=
                                                   Intval (First_Bit (CC))
                             or else Intval (Last_Bit (Rep1)) /=
                                                   Intval (Last_Bit (CC))
                           then
                              Error_Msg_N
                                ("component clause inconsistent with "
                                 & "representation of ancestor", CC);

                           elsif Warn_On_Redundant_Constructs then
                              Error_Msg_N
                                ("?r?redundant confirming component clause "
                                 & "for component!", CC);
                           end if;
                        end;
                     end if;

                  --  Normal case where this is the first component clause we
                  --  have seen for this entity, so set it up properly.

                  else
                     --  Make reference for field in record rep clause and set
                     --  appropriate entity field in the field identifier.

                     Generate_Reference
                       (Comp, Component_Name (CC), Set_Ref => False);
                     Set_Entity_With_Checks (Component_Name (CC), Comp);

                     --  Update Fbit and Lbit to the actual bit number

                     Fbit := Fbit + UI_From_Int (SSU) * Posit;
                     Lbit := Lbit + UI_From_Int (SSU) * Posit;

                     if Has_Size_Clause (Rectype)
                       and then RM_Size (Rectype) <= Lbit
                     then
                        Error_Msg_Uint_1 := RM_Size (Rectype);
                        Error_Msg_Uint_2 := Lbit + 1;
                        Error_Msg_N ("bit number out of range of specified "
                           & "size (expected ^, got ^)",
                           Last_Bit (CC));
                     else
                        Set_Component_Clause     (Comp, CC);
                        Set_Component_Bit_Offset (Comp, Fbit);
                        Set_Esize                (Comp, 1 + (Lbit - Fbit));
                        Set_Normalized_First_Bit (Comp, Fbit mod SSU);
                        Set_Normalized_Position  (Comp, Fbit / SSU);

                        if Warn_On_Overridden_Size
                          and then Has_Size_Clause (Etype (Comp))
                          and then RM_Size (Etype (Comp)) /= Esize (Comp)
                        then
                           Error_Msg_NE
                             ("?.s?component size overrides size clause for&",
                              Component_Name (CC), Etype (Comp));
                        end if;

                        Check_Size
                          (Component_Name (CC),
                           Etype (Comp),
                           Esize (Comp),
                           Biased);

                        Set_Biased
                          (Comp, First_Node (CC), "component clause", Biased);

                        --  This information is also set in the corresponding
                        --  component of the base type, found by accessing the
                        --  Original_Record_Component link if it is present.

                        Ocomp := Original_Record_Component (Comp);

                        if Present (Ocomp) and then Ocomp /= Comp then
                           Set_Component_Clause     (Ocomp, CC);
                           Set_Component_Bit_Offset (Ocomp, Fbit);
                           Set_Esize                (Ocomp, 1 + (Lbit - Fbit));
                           Set_Normalized_First_Bit (Ocomp, Fbit mod SSU);
                           Set_Normalized_Position  (Ocomp, Fbit / SSU);

                           --  Note: we don't use Set_Biased here, because we
                           --  already gave a warning above if needed, and we
                           --  would get a duplicate for the same name here.

                           Set_Has_Biased_Representation
                             (Ocomp, Has_Biased_Representation (Comp));
                        end if;

                        if Esize (Comp) < 0 then
                           Error_Msg_N ("component size is negative", CC);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;

         Next (CC);
      end loop;

      --  Check missing components if Complete_Representation pragma appeared

      if Present (CR_Pragma) then
         Comp := First_Component_Or_Discriminant (Rectype);
         while Present (Comp) loop
            if No (Component_Clause (Comp)) then
               Error_Msg_NE
                 ("missing component clause for &", CR_Pragma, Comp);
            end if;

            Next_Component_Or_Discriminant (Comp);
         end loop;

      --  Give missing components warning if required

      elsif Warn_On_Unrepped_Components then
         declare
            Num_Repped_Components   : Nat := 0;
            Num_Unrepped_Components : Nat := 0;

         begin
            --  First count number of repped and unrepped components

            Comp := First_Component_Or_Discriminant (Rectype);
            while Present (Comp) loop
               if Present (Component_Clause (Comp)) then
                  Num_Repped_Components := Num_Repped_Components + 1;
               else
                  Num_Unrepped_Components := Num_Unrepped_Components + 1;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;

            --  We are only interested in the case where there is at least one
            --  unrepped component, and at least half the components have rep
            --  clauses. We figure that if less than half have them, then the
            --  partial rep clause is really intentional. If the component
            --  type has no underlying type set at this point (as for a generic
            --  formal type), we don't know enough to give a warning on the
            --  component.

            if Num_Unrepped_Components > 0
              and then Num_Unrepped_Components < Num_Repped_Components
            then
               Comp := First_Component_Or_Discriminant (Rectype);
               while Present (Comp) loop
                  if No (Component_Clause (Comp))
                    and then Comes_From_Source (Comp)
                    and then Present (Underlying_Type (Etype (Comp)))
                    and then (Is_Scalar_Type (Underlying_Type (Etype (Comp)))
                               or else Size_Known_At_Compile_Time
                                         (Underlying_Type (Etype (Comp))))
                    and then not Has_Warnings_Off (Rectype)

                    --  Ignore discriminant in unchecked union, since it is
                    --  not there, and cannot have a component clause.

                    and then (not Is_Unchecked_Union (Rectype)
                               or else Ekind (Comp) /= E_Discriminant)
                  then
                     Error_Msg_Sloc := Sloc (Comp);
                     Error_Msg_NE
                       ("?.c?no component clause given for & declared #",
                        N, Comp);
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;
            end if;
         end;
      end if;
   end Analyze_Record_Representation_Clause;

   ----------------------------------------------
   -- Analyze_User_Aspect_Aspect_Specification --
   ----------------------------------------------

   procedure Analyze_User_Aspect_Aspect_Specification (N : Node_Id) is
      OK : Boolean := True;

      procedure Analyze_One_User_Aspect (Id : Node_Id);
      --  A User_Aspect aspect specification may specify multiple
      --  user-defined aspects. This procedure is called for each one.

      -----------------------------
      -- Analyze_One_User_Aspect --
      -----------------------------

      procedure Analyze_One_User_Aspect (Id : Node_Id) is
         UAD_Pragma : constant Node_Id :=
           User_Aspect_Support.Registered_UAD_Pragma (Chars (Id));

         Arg : Node_Id;
      begin
         if No (UAD_Pragma) then
            Error_Msg_N ("no definition for user-defined aspect", Id);
            return;
         end if;

         --  Process args in reverse order so that inserted
         --  aspect specs end up in "right" order (although
         --  order shouldn't matter).
         Arg := Last (Pragma_Argument_Associations (UAD_Pragma));

         --  Skip first argument, which is the name of the
         --  user-defined aspect.
         while Present (Prev (Arg)) loop
            declare
               Exp             : constant Node_Id := Expression (Arg);
               New_Sloc        : constant Source_Ptr := Sloc (N);
               New_Aspect_Spec : Node_Id;
               New_Exp         : Node_Id;
               New_Exp_List    : List_Id;
            begin
               case Nkind (Exp) is
                  when N_Identifier =>
                     New_Aspect_Spec :=
                       Make_Aspect_Specification
                         (New_Sloc,
                          Identifier =>
                            New_Copy_Tree (Exp, New_Sloc => New_Sloc));

                  when N_Indexed_Component =>
                     New_Exp_List := New_List;

                     declare
                        Index_Exp : Node_Id := First (Expressions (Exp));
                     begin
                        while Present (Index_Exp) loop
                           Append (New_Copy_Tree
                                     (Index_Exp, New_Sloc => New_Sloc),
                                   To => New_Exp_List);
                           Next (Index_Exp);
                        end loop;
                     end;

                     New_Exp := Make_Aggregate
                       (Sloc => New_Sloc,
                        Expressions => New_Exp_List,
                        Is_Parenthesis_Aggregate => True);

                     New_Aspect_Spec :=
                       Make_Aspect_Specification
                         (New_Sloc,
                          Identifier =>
                            New_Copy_Tree (Prefix (Exp), New_Sloc => New_Sloc),
                          Expression => New_Exp);

                  when others =>
                     raise Program_Error;
               end case;

               Insert_After (After => N, Node => New_Aspect_Spec);
            end;
            Arg := Prev (Arg);
         end loop;
      end Analyze_One_User_Aspect;
   begin
      if Analyzed (N) then
         return;
      end if;

      --  This aspect can be specified for any entity whose
      --  syntax allows an aspect specification.
      --  The analysis code below constructs new aspect
      --  specifications for the given entity; each might
      --  turn out to be legal or illegal. That is determined
      --  when each of these new aspect_specs is analyzed.

      case Nkind (Expression (N)) is
         when N_Identifier =>
            Analyze_One_User_Aspect (Expression (N));
         when N_Aggregate =>
            OK := Is_Parenthesis_Aggregate (Expression (N));
            declare
               Id : Node_Id := First (Expressions (Expression (N)));
            begin
               while Present (Id) loop
                  if Nkind (Id) = N_Identifier then
                     Analyze_One_User_Aspect (Id);
                  else
                     OK := False;
                  end if;
                  Next (Id);
               end loop;
            end;
         when others =>
            OK := False;
      end case;

      if not OK then
         Error_Msg_N
           ("Bad argument for User_Aspect aspect specification", N);
      end if;

      Set_Analyzed (N);
   end Analyze_User_Aspect_Aspect_Specification;

   -------------------------------------
   -- Build_Discrete_Static_Predicate --
   -------------------------------------

   procedure Build_Discrete_Static_Predicate
     (Typ  : Entity_Id;
      Expr : Node_Id;
      Nam  : Name_Id)
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      Btyp : constant Entity_Id := Base_Type (Typ);

      BLo : constant Uint := Expr_Value (Type_Low_Bound  (Btyp));
      BHi : constant Uint := Expr_Value (Type_High_Bound (Btyp));
      --  Low bound and high bound value of base type of Typ

      TLo : Uint;
      THi : Uint;
      --  Bounds for constructing the static predicate. We use the bound of the
      --  subtype if it is static, otherwise the corresponding base type bound.
      --  Note: a non-static subtype can have a static predicate.

      type REnt is record
         Lo, Hi : Uint;
      end record;
      --  One entry in a Rlist value, a single REnt (range entry) value denotes
      --  one range from Lo to Hi. To represent a single value range Lo = Hi =
      --  value.

      type RList is array (Nat range <>) of REnt;
      --  A list of ranges. The ranges are sorted in increasing order, and are
      --  disjoint (there is a gap of at least one value between each range in
      --  the table). A value is in the set of ranges in Rlist if it lies
      --  within one of these ranges.

      False_Range : constant RList :=
        RList'(1 .. 0 => REnt'(No_Uint, No_Uint));
      --  An empty set of ranges represents a range list that can never be
      --  satisfied, since there are no ranges in which the value could lie,
      --  so it does not lie in any of them. False_Range is a canonical value
      --  for this empty set, but general processing should test for an Rlist
      --  with length zero (see Is_False predicate), since other null ranges
      --  may appear which must be treated as False.

      True_Range : constant RList := RList'(1 => REnt'(BLo, BHi));
      --  Range representing True, value must be in the base range

      function "and" (Left : RList; Right : RList) return RList;
      --  And's together two range lists, returning a range list. This is a set
      --  intersection operation.

      function "or" (Left : RList; Right : RList) return RList;
      --  Or's together two range lists, returning a range list. This is a set
      --  union operation.

      function "not" (Right : RList) return RList;
      --  Returns complement of a given range list, i.e. a range list
      --  representing all the values in TLo .. THi that are not in the input
      --  operand Right.

      function Build_Val (V : Uint) return Node_Id;
      --  Return an analyzed N_Identifier node referencing this value, suitable
      --  for use as an entry in the Static_Discrete_Predicate list. This node
      --  is typed with the base type.

      function Build_Range (Lo : Uint; Hi : Uint) return Node_Id;
      --  Return an analyzed N_Range node referencing this range, suitable for
      --  use as an entry in the Static_Discrete_Predicate list. This node is
      --  typed with the base type.

      function Get_RList
        (Exp    : Node_Id;
         Static : access Boolean) return RList;
      --  This is a recursive routine that converts the given expression into a
      --  list of ranges, suitable for use in building the static predicate.
      --  Static.all will be set to False if the expression is found to be non
      --  static. Note that Static.all should be set to True by the caller.

      function Is_False (R : RList) return Boolean;
      pragma Inline (Is_False);
      --  Returns True if the given range list is empty, and thus represents a
      --  False list of ranges that can never be satisfied.

      function Is_True (R : RList) return Boolean;
      --  Returns True if R trivially represents the True predicate by having a
      --  single range from BLo to BHi.

      function Is_Type_Ref (N : Node_Id) return Boolean;
      pragma Inline (Is_Type_Ref);
      --  Returns if True if N is a reference to the type for the predicate in
      --  the expression (i.e. if it is an identifier whose Chars field matches
      --  the Nam given in the call). N must not be parenthesized, if the type
      --  name appears in parens, this routine will return False.

      function Lo_Val (N : Node_Id) return Uint;
      --  Given an entry from a Static_Discrete_Predicate list that is either
      --  a static expression or static range, gets either the expression value
      --  or the low bound of the range.

      function Hi_Val (N : Node_Id) return Uint;
      --  Given an entry from a Static_Discrete_Predicate list that is either
      --  a static expression or static range, gets either the expression value
      --  or the high bound of the range.

      function Membership_Entry
        (N : Node_Id; Static : access Boolean) return RList;
      --  Given a single membership entry (range, value, or subtype), returns
      --  the corresponding range list. Set Static.all to False if not static.

      function Membership_Entries
        (N : Node_Id; Static : access Boolean) return RList;
      --  Given an element on an alternatives list of a membership operation,
      --  returns the range list corresponding to this entry and all following
      --  entries (i.e. returns the "or" of this list of values).
      --  Set Static.all to False if not static.

      function Stat_Pred
        (Typ    : Entity_Id;
         Static : access Boolean) return RList;
      --  Given a type, if it has a static predicate, then set Result to the
      --  predicate as a range list, otherwise set Static.all to False.

      procedure Warn_If_Test_Ineffective (REntry : REnt; N : Node_Id);
      --  Issue a warning if REntry includes only values that are
      --  outside the range TLo .. THi.

      -----------
      -- "and" --
      -----------

      function "and" (Left : RList; Right : RList) return RList is
         FEnt : REnt;
         --  First range of result

         SLeft : Nat := Left'First;
         --  Start of rest of left entries

         SRight : Nat := Right'First;
         --  Start of rest of right entries

      begin
         --  If either range is True, return the other

         if Is_True (Left) then
            return Right;
         elsif Is_True (Right) then
            return Left;
         end if;

         --  If either range is False, return False

         if Is_False (Left) or else Is_False (Right) then
            return False_Range;
         end if;

         --  Loop to remove entries at start that are disjoint, and thus just
         --  get discarded from the result entirely.

         loop
            --  If no operands left in either operand, result is false

            if SLeft > Left'Last or else SRight > Right'Last then
               return False_Range;

            --  Discard first left operand entry if disjoint with right

            elsif Left (SLeft).Hi < Right (SRight).Lo then
               SLeft := SLeft + 1;

            --  Discard first right operand entry if disjoint with left

            elsif Right (SRight).Hi < Left (SLeft).Lo then
               SRight := SRight + 1;

            --  Otherwise we have an overlapping entry

            else
               exit;
            end if;
         end loop;

         --  Now we have two non-null operands, and first entries overlap. The
         --  first entry in the result will be the overlapping part of these
         --  two entries.

         FEnt := REnt'(Lo => UI_Max (Left (SLeft).Lo, Right (SRight).Lo),
                       Hi => UI_Min (Left (SLeft).Hi, Right (SRight).Hi));

         --  Now we can remove the entry that ended at a lower value, since its
         --  contribution is entirely contained in Fent.

         if Left (SLeft).Hi <= Right (SRight).Hi then
            SLeft := SLeft + 1;
         else
            SRight := SRight + 1;
         end if;

         --  Compute result by concatenating this first entry with the "and" of
         --  the remaining parts of the left and right operands. Note that if
         --  either of these is empty, "and" will yield empty, so that we will
         --  end up with just Fent, which is what we want in that case.

         return
           FEnt & (Left (SLeft .. Left'Last) and Right (SRight .. Right'Last));
      end "and";

      -----------
      -- "not" --
      -----------

      function "not" (Right : RList) return RList is
      begin
         --  Return True if False range

         if Is_False (Right) then
            return True_Range;
         end if;

         --  Return False if True range

         if Is_True (Right) then
            return False_Range;
         end if;

         --  Here if not trivial case

         declare
            Result : RList (1 .. Right'Length + 1);
            --  May need one more entry for gap at beginning and end

            Count : Nat := 0;
            --  Number of entries stored in Result

         begin
            --  Gap at start

            if Right (Right'First).Lo > TLo then
               Count := Count + 1;
               Result (Count) := REnt'(TLo, Right (Right'First).Lo - 1);
            end if;

            --  Gaps between ranges

            for J in Right'First .. Right'Last - 1 loop
               Count := Count + 1;
               Result (Count) := REnt'(Right (J).Hi + 1, Right (J + 1).Lo - 1);
            end loop;

            --  Gap at end

            if Right (Right'Last).Hi < THi then
               Count := Count + 1;
               Result (Count) := REnt'(Right (Right'Last).Hi + 1, THi);
            end if;

            return Result (1 .. Count);
         end;
      end "not";

      ----------
      -- "or" --
      ----------

      function "or" (Left : RList; Right : RList) return RList is
         FEnt : REnt;
         --  First range of result

         SLeft : Nat := Left'First;
         --  Start of rest of left entries

         SRight : Nat := Right'First;
         --  Start of rest of right entries

      begin
         --  If either range is True, return True

         if Is_True (Left) or else Is_True (Right) then
            return True_Range;
         end if;

         --  If either range is False (empty), return the other

         if Is_False (Left) then
            return Right;
         elsif Is_False (Right) then
            return Left;
         end if;

         --  Initialize result first entry from left or right operand depending
         --  on which starts with the lower range.

         if Left (SLeft).Lo < Right (SRight).Lo then
            FEnt := Left (SLeft);
            SLeft := SLeft + 1;
         else
            FEnt := Right (SRight);
            SRight := SRight + 1;
         end if;

         --  This loop eats ranges from left and right operands that are
         --  contiguous with the first range we are gathering.

         loop
            --  Eat first entry in left operand if contiguous or overlapped by
            --  gathered first operand of result.

            if SLeft <= Left'Last
              and then Left (SLeft).Lo <= FEnt.Hi + 1
            then
               FEnt.Hi := UI_Max (FEnt.Hi, Left (SLeft).Hi);
               SLeft := SLeft + 1;

            --  Eat first entry in right operand if contiguous or overlapped by
            --  gathered right operand of result.

            elsif SRight <= Right'Last
              and then Right (SRight).Lo <= FEnt.Hi + 1
            then
               FEnt.Hi := UI_Max (FEnt.Hi, Right (SRight).Hi);
               SRight := SRight + 1;

            --  All done if no more entries to eat

            else
               exit;
            end if;
         end loop;

         --  Obtain result as the first entry we just computed, concatenated
         --  to the "or" of the remaining results (if one operand is empty,
         --  this will just concatenate with the other

         return
           FEnt & (Left (SLeft .. Left'Last) or Right (SRight .. Right'Last));
      end "or";

      -----------------
      -- Build_Range --
      -----------------

      function Build_Range (Lo : Uint; Hi : Uint) return Node_Id is
         Result : Node_Id;
      begin
         Result :=
           Make_Range (Loc,
              Low_Bound  => Build_Val (Lo),
              High_Bound => Build_Val (Hi));
         Set_Etype (Result, Btyp);
         Set_Analyzed (Result);
         return Result;
      end Build_Range;

      ---------------
      -- Build_Val --
      ---------------

      function Build_Val (V : Uint) return Node_Id is
         Result : Node_Id;

      begin
         if Is_Enumeration_Type (Typ) then
            Result := Get_Enum_Lit_From_Pos (Typ, V, Loc);
         else
            Result := Make_Integer_Literal (Loc, V);
         end if;

         Set_Etype (Result, Btyp);
         Set_Is_Static_Expression (Result);
         Set_Analyzed (Result);
         return Result;
      end Build_Val;

      ---------------
      -- Get_RList --
      ---------------

      function Get_RList
        (Exp    : Node_Id;
         Static : access Boolean) return RList
      is
         Op         : Node_Kind;
         Val        : Uint;
         Val_Bearer : Node_Id;

      begin
         --  Static expression can only be true or false

         if Is_OK_Static_Expression (Exp) then
            if Expr_Value (Exp) = 0 then
               return False_Range;
            else
               return True_Range;
            end if;
         end if;

         --  Otherwise test node type

         Op := Nkind (Exp);

         case Op is

            --  And

            when N_And_Then
               | N_Op_And
            =>
               return Get_RList (Left_Opnd (Exp), Static)
                        and
                      Get_RList (Right_Opnd (Exp), Static);

            --  Or

            when N_Op_Or
               | N_Or_Else
            =>
               return Get_RList (Left_Opnd (Exp), Static)
                        or
                      Get_RList (Right_Opnd (Exp), Static);

            --  Not

            when N_Op_Not =>
               return not Get_RList (Right_Opnd (Exp), Static);

               --  Comparisons of type with static value

            when N_Op_Compare =>

               --  Type is left operand

               if Is_Type_Ref (Left_Opnd (Exp))
                 and then Is_OK_Static_Expression (Right_Opnd (Exp))
               then
                  Val_Bearer := Right_Opnd (Exp);

               --  Typ is right operand

               elsif Is_Type_Ref (Right_Opnd (Exp))
                 and then Is_OK_Static_Expression (Left_Opnd (Exp))
               then
                  Val_Bearer := Left_Opnd (Exp);

                  --  Invert sense of comparison

                  case Op is
                     when N_Op_Gt => Op := N_Op_Lt;
                     when N_Op_Lt => Op := N_Op_Gt;
                     when N_Op_Ge => Op := N_Op_Le;
                     when N_Op_Le => Op := N_Op_Ge;
                     when others  => null;
                  end case;

               --  Other cases are non-static

               else
                  Static.all := False;
                  return False_Range;
               end if;

               Val := Expr_Value (Val_Bearer);

               --  Construct range according to comparison operation

               declare
                  REntry : REnt;
               begin
                  case Op is
                     when N_Op_Eq =>
                        REntry := (Val, Val);

                     when N_Op_Ge =>
                        REntry := (Val, THi);

                     when N_Op_Gt =>
                        REntry := (Val + 1, THi);

                     when N_Op_Le =>
                        REntry := (TLo, Val);

                     when N_Op_Lt =>
                        REntry := (TLo, Val - 1);

                     when N_Op_Ne =>
                        Warn_If_Test_Ineffective ((Val, Val), Val_Bearer);
                        return RList'(REnt'(TLo, Val - 1),
                                      REnt'(Val + 1, THi));

                     when others  =>
                        raise Program_Error;
                  end case;

                  Warn_If_Test_Ineffective (REntry, Val_Bearer);
                  return RList'(1 => REntry);
               end;

            --  Membership (IN)

            when N_In =>
               if not Is_Type_Ref (Left_Opnd (Exp)) then
                  Static.all := False;
                  return False_Range;
               end if;

               if Present (Right_Opnd (Exp)) then
                  return Membership_Entry (Right_Opnd (Exp), Static);
               else
                  return Membership_Entries
                           (First (Alternatives (Exp)), Static);
               end if;

            --  Negative membership (NOT IN)

            when N_Not_In =>
               if not Is_Type_Ref (Left_Opnd (Exp)) then
                  Static.all := False;
                  return False_Range;
               end if;

               if Present (Right_Opnd (Exp)) then
                  return not Membership_Entry (Right_Opnd (Exp), Static);
               else
                  return not Membership_Entries
                               (First (Alternatives (Exp)), Static);
               end if;

            --  Function call, may be call to static predicate

            when N_Function_Call =>
               if Is_Entity_Name (Name (Exp)) then
                  declare
                     Ent : constant Entity_Id := Entity (Name (Exp));
                  begin
                     if Is_Predicate_Function (Ent) then
                        return Stat_Pred (Etype (First_Formal (Ent)), Static);
                     end if;
                  end;
               end if;

               --  Other function call cases are non-static

               Static.all := False;
               return False_Range;

            --  Qualified expression, dig out the expression

            when N_Qualified_Expression =>
               return Get_RList (Expression (Exp), Static);

            when N_Case_Expression =>
               declare
                  Alt     : Node_Id;
                  Choices : List_Id;
                  Dep     : Node_Id;

               begin
                  if not Is_Entity_Name (Expression (Expr))
                    or else Etype (Expression (Expr)) /= Typ
                  then
                     Error_Msg_N
                       ("expression must denote subtype", Expression (Expr));
                     return False_Range;
                  end if;

                  --  Collect discrete choices in all True alternatives

                  Choices := New_List;
                  Alt := First (Alternatives (Exp));
                  while Present (Alt) loop
                     Dep := Expression (Alt);

                     if not Is_OK_Static_Expression (Dep) then
                        Static.all := False;
                        return False_Range;

                     elsif Is_True (Expr_Value (Dep)) then
                        Append_List_To (Choices,
                          New_Copy_List (Discrete_Choices (Alt)));
                     end if;

                     Next (Alt);
                  end loop;

                  return Membership_Entries (First (Choices), Static);
               end;

            --  Expression with actions: if no actions, dig out expression

            when N_Expression_With_Actions =>
               if Is_Empty_List (Actions (Exp)) then
                  return Get_RList (Expression (Exp), Static);
               else
                  Static.all := False;
                  return False_Range;
               end if;

            --  Xor operator

            when N_Op_Xor =>
               return (Get_RList (Left_Opnd (Exp), Static)
                        and not Get_RList (Right_Opnd (Exp), Static))
                 or   (Get_RList (Right_Opnd (Exp), Static)
                        and not Get_RList (Left_Opnd (Exp), Static));

            --  Any other node type is non-static

            when others =>
               Static.all := False;
               return False_Range;
         end case;
      end Get_RList;

      ------------
      -- Hi_Val --
      ------------

      function Hi_Val (N : Node_Id) return Uint is
      begin
         if Is_OK_Static_Expression (N) then
            return Expr_Value (N);
         else
            pragma Assert (Nkind (N) = N_Range);
            return Expr_Value (High_Bound (N));
         end if;
      end Hi_Val;

      --------------
      -- Is_False --
      --------------

      function Is_False (R : RList) return Boolean is
      begin
         return R'Length = 0;
      end Is_False;

      -------------
      -- Is_True --
      -------------

      function Is_True (R : RList) return Boolean is
      begin
         return R'Length = 1
           and then R (R'First).Lo = BLo
           and then R (R'First).Hi = BHi;
      end Is_True;

      -----------------
      -- Is_Type_Ref --
      -----------------

      function Is_Type_Ref (N : Node_Id) return Boolean is
      begin
         return Nkind (N) = N_Identifier
           and then Chars (N) = Nam
           and then Paren_Count (N) = 0;
      end Is_Type_Ref;

      ------------
      -- Lo_Val --
      ------------

      function Lo_Val (N : Node_Id) return Uint is
      begin
         if Is_OK_Static_Expression (N) then
            return Expr_Value (N);
         else
            pragma Assert (Nkind (N) = N_Range);
            return Expr_Value (Low_Bound (N));
         end if;
      end Lo_Val;

      ------------------------
      -- Membership_Entries --
      ------------------------

      function Membership_Entries
        (N : Node_Id; Static : access Boolean) return RList is
      begin
         if No (Next (N)) then
            return Membership_Entry (N, Static);
         else
            return Membership_Entry (N, Static)
              or Membership_Entries (Next (N), Static);
         end if;
      end Membership_Entries;

      ----------------------
      -- Membership_Entry --
      ----------------------

      function Membership_Entry
        (N : Node_Id; Static : access Boolean) return RList
      is
         Val : Uint;
         SLo : Uint;
         SHi : Uint;

      begin
         --  Range case

         if Nkind (N) = N_Range then
            if not Is_OK_Static_Expression (Low_Bound  (N))
                 or else
               not Is_OK_Static_Expression (High_Bound (N))
            then
               Static.all := False;
               return False_Range;
            else
               SLo := Expr_Value (Low_Bound  (N));
               SHi := Expr_Value (High_Bound (N));
               declare
                  REntry : constant REnt := (SLo, SHi);
               begin
                  Warn_If_Test_Ineffective (REntry, N);
                  return RList'(1 => REntry);
               end;
            end if;

         --  Others case

         elsif Nkind (N) = N_Others_Choice then
            declare
               Choices    : constant List_Id := Others_Discrete_Choices (N);
               Choice     : Node_Id;
               Range_List : RList (1 .. List_Length (Choices));

            begin
               Choice := First (Choices);

               for J in Range_List'Range loop
                  Range_List (J) := REnt'(Lo_Val (Choice), Hi_Val (Choice));
                  Next (Choice);
               end loop;

               return Range_List;
            end;

         --  Static expression case

         elsif Is_OK_Static_Expression (N) then
            Val := Expr_Value (N);
            declare
               REntry : constant REnt := (Val, Val);
            begin
               Warn_If_Test_Ineffective (REntry, N);
               return RList'(1 => REntry);
            end;

         --  Identifier (other than static expression) case

         else pragma Assert (Nkind (N) in N_Expanded_Name | N_Identifier);

            --  Type case

            if Is_Type (Entity (N)) then

               --  If type has predicates, process them

               if Has_Predicates (Entity (N)) then
                  return Stat_Pred (Entity (N), Static);

               --  For static subtype without predicates, get range

               elsif Is_OK_Static_Subtype (Entity (N)) then
                  SLo := Expr_Value (Type_Low_Bound  (Entity (N)));
                  SHi := Expr_Value (Type_High_Bound (Entity (N)));
                  return RList'(1 => REnt'(SLo, SHi));

               --  Any other type makes us non-static

               else
                  Static.all := False;
                  return False_Range;
               end if;

            --  Any other kind of identifier in predicate (e.g. a non-static
            --  expression value) means this is not a static predicate.

            else
               Static.all := False;
               return False_Range;
            end if;
         end if;
      end Membership_Entry;

      ---------------
      -- Stat_Pred --
      ---------------

      function Stat_Pred
        (Typ    : Entity_Id;
         Static : access Boolean) return RList is
      begin
         --  Not static if type does not have static predicates

         if not Has_Static_Predicate (Typ) then
            Static.all := False;
            return False_Range;
         end if;

         --  Otherwise we convert the predicate list to a range list

         declare
            Spred  : constant List_Id := Static_Discrete_Predicate (Typ);
            Result : RList (1 .. List_Length (Spred));
            P      : Node_Id;

         begin
            P := First (Static_Discrete_Predicate (Typ));
            for J in Result'Range loop
               Result (J) := REnt'(Lo_Val (P), Hi_Val (P));
               Next (P);
            end loop;

            return Result;
         end;
      end Stat_Pred;

      procedure Warn_If_Test_Ineffective (REntry : REnt; N : Node_Id) is

         procedure IPT_Warning (Msg : String);
         --  Emit warning

         -----------------
         -- IPT_Warning --
         -----------------
         procedure IPT_Warning (Msg : String) is
         begin
            Error_Msg_N ("ineffective predicate test " & Msg & "?_s?", N);
         end IPT_Warning;

      --  Start of processing for Warn_If_Test_Ineffective

      begin
         --  Do nothing if warning disabled

         if not Warn_On_Ineffective_Predicate_Test then
            null;

         --  skip null-range corner cases

         elsif REntry.Lo > REntry.Hi or else TLo > THi then
            null;

         --  warn if no overlap between subtype bounds and the given range

         elsif REntry.Lo > THi or else REntry.Hi < TLo then
            Error_Msg_Uint_1 := REntry.Lo;
            if REntry.Lo /= REntry.Hi then
               Error_Msg_Uint_2 := REntry.Hi;
               IPT_Warning ("range: ^ .. ^");
            elsif Is_Enumeration_Type (Typ) and then
               Nkind (N) in N_Identifier | N_Expanded_Name
            then
               IPT_Warning ("value: &");
            else
               IPT_Warning ("value: ^");
            end if;
         end if;
      end Warn_If_Test_Ineffective;

   --  Start of processing for Build_Discrete_Static_Predicate

   begin
      --  Establish bounds for the predicate

      if Compile_Time_Known_Value (Type_Low_Bound (Typ)) then
         TLo := Expr_Value (Type_Low_Bound (Typ));
      else
         TLo := BLo;
      end if;

      if Compile_Time_Known_Value (Type_High_Bound (Typ)) then
         THi := Expr_Value (Type_High_Bound (Typ));
      else
         THi := BHi;
      end if;

      --  Analyze the expression to see if it is a static predicate

      declare
         Static : aliased Boolean := True;
         Ranges : constant RList := Get_RList (Expr, Static'Access);
         --  Range list from expression if it is static

         Plist : List_Id;

      begin
         --  If non-static, return doing nothing

         if not Static then
            return;
         end if;

         --  Convert range list into a form for the static predicate. In the
         --  Ranges array, we just have raw ranges, these must be converted
         --  to properly typed and analyzed static expressions or range nodes.

         --  Note: here we limit ranges to the ranges of the subtype, so that
         --  a predicate is always false for values outside the subtype. That
         --  seems fine, such values are invalid anyway, and considering them
         --  to fail the predicate seems allowed and friendly, and furthermore
         --  simplifies processing for case statements and loops.

         Plist := New_List;

         for J in Ranges'Range loop
            declare
               Lo : Uint := Ranges (J).Lo;
               Hi : Uint := Ranges (J).Hi;

            begin
               --  Ignore completely out of range entry

               if Hi < TLo or else Lo > THi then
                  null;

               --  Otherwise process entry

               else
                  --  Adjust out of range value to subtype range

                  if Lo < TLo then
                     Lo := TLo;
                  end if;

                  if Hi > THi then
                     Hi := THi;
                  end if;

                  --  Convert range into required form

                  Append_To (Plist, Build_Range (Lo, Hi));
               end if;
            end;
         end loop;

         --  Processing was successful and all entries were static, so now we
         --  can store the result as the predicate list.

         Set_Static_Discrete_Predicate (Typ, Plist);

         --  Within a generic the predicate functions themselves need not
         --  be constructed.

         if Inside_A_Generic then
            return;
         end if;

         --  The processing for static predicates put the expression into
         --  canonical form as a series of ranges. It also eliminated
         --  duplicates and collapsed and combined ranges. We might as well
         --  replace the alternatives list of the right operand of the
         --  membership test with the static predicate list, which will
         --  usually be more efficient.

         declare
            New_Alts : constant List_Id := New_List;
            Old_Node : Node_Id;
            New_Node : Node_Id;

         begin
            Old_Node := First (Plist);
            while Present (Old_Node) loop
               New_Node := New_Copy (Old_Node);

               if Nkind (New_Node) = N_Range then
                  Set_Low_Bound  (New_Node, New_Copy (Low_Bound  (Old_Node)));
                  Set_High_Bound (New_Node, New_Copy (High_Bound (Old_Node)));
               end if;

               Append_To (New_Alts, New_Node);
               Next (Old_Node);
            end loop;

            --  If empty list, replace by False

            if Is_Empty_List (New_Alts) then
               Rewrite (Expr, New_Occurrence_Of (Standard_False, Loc));

               --  Else replace by set membership test

            else
               Rewrite (Expr,
                 Make_In (Loc,
                   Left_Opnd    => Make_Identifier (Loc, Nam),
                   Right_Opnd   => Empty,
                   Alternatives => New_Alts));

               --  Resolve new expression in function context

               Push_Scope (Predicate_Function (Typ));
               Install_Formals (Predicate_Function (Typ));
               Analyze_And_Resolve (Expr, Standard_Boolean);
               End_Scope;
            end if;
         end;
      end;
   end Build_Discrete_Static_Predicate;

   --------------------------------
   -- Build_Export_Import_Pragma --
   --------------------------------

   function Build_Export_Import_Pragma
     (Asp : Node_Id;
      Id  : Entity_Id) return Node_Id
   is
      Asp_Id : constant Aspect_Id  := Get_Aspect_Id (Asp);
      Expr   : constant Node_Id    := Expression (Asp);
      Loc    : constant Source_Ptr := Sloc (Asp);

      Args     : List_Id;
      Conv     : Node_Id;
      Conv_Arg : Node_Id;
      Dummy_1  : Node_Id;
      Dummy_2  : Node_Id;
      EN       : Node_Id;
      LN       : Node_Id;
      Prag     : Node_Id;

      Create_Pragma : Boolean := False;
      --  This flag is set when the aspect form is such that it warrants the
      --  creation of a corresponding pragma.

   begin
      if Present (Expr) then
         if Error_Posted (Expr) then
            null;

         elsif Is_True (Expr_Value (Expr)) then
            Create_Pragma := True;
         end if;

      --  Otherwise the aspect defaults to True

      else
         Create_Pragma := True;
      end if;

      --  Nothing to do when the expression is False or is erroneous

      if not Create_Pragma then
         return Empty;
      end if;

      --  Obtain all interfacing aspects that apply to the related entity

      Get_Interfacing_Aspects
        (Iface_Asp => Asp,
         Conv_Asp  => Conv,
         EN_Asp    => EN,
         Expo_Asp  => Dummy_1,
         Imp_Asp   => Dummy_2,
         LN_Asp    => LN);

      Args := New_List;

      --  Handle the convention argument

      if Present (Conv) then
         Conv_Arg := New_Copy_Tree (Expression (Conv));

      --  Assume convention "Ada' when aspect Convention is missing

      else
         Conv_Arg := Make_Identifier (Loc, Name_Ada);
      end if;

      Append_To (Args,
        Make_Pragma_Argument_Association (Loc,
          Chars      => Name_Convention,
          Expression => Conv_Arg));

      --  Handle the entity argument

      Append_To (Args,
        Make_Pragma_Argument_Association (Loc,
          Chars      => Name_Entity,
          Expression => New_Occurrence_Of (Id, Loc)));

      --  Handle the External_Name argument

      if Present (EN) then
         Append_To (Args,
           Make_Pragma_Argument_Association (Loc,
             Chars      => Name_External_Name,
             Expression => New_Copy_Tree (Expression (EN))));
      end if;

      --  Handle the Link_Name argument

      if Present (LN) then
         Append_To (Args,
           Make_Pragma_Argument_Association (Loc,
             Chars      => Name_Link_Name,
             Expression => New_Copy_Tree (Expression (LN))));
      end if;

      --  Generate:
      --    pragma Export/Import
      --      (Convention    => <Conv>/Ada,
      --       Entity        => <Id>,
      --      [External_Name => <EN>,]
      --      [Link_Name     => <LN>]);

      Prag :=
        Make_Pragma (Loc,
          Pragma_Identifier            =>
            Make_Identifier (Loc, Chars (Identifier (Asp))),
          Pragma_Argument_Associations => Args);

      --  Decorate the relevant aspect and the pragma

      Set_Aspect_Rep_Item (Asp, Prag);

      Set_Corresponding_Aspect      (Prag, Asp);
      Set_From_Aspect_Specification (Prag);
      Set_Parent                    (Prag, Asp);

      if Asp_Id = Aspect_Import and then Is_Subprogram (Id) then
         Set_Import_Pragma (Id, Prag);
      end if;

      return Prag;
   end Build_Export_Import_Pragma;

   ------------------------------
   -- Build_Predicate_Function --
   ------------------------------

   --  The function constructed here has the form:

   --    function typPredicate (Ixxx : typ) return Boolean is
   --    begin
   --       return
   --          typ1Predicate (typ1 (Ixxx))
   --          and then typ2Predicate (typ2 (Ixxx))
   --          and then ...
   --          and then exp1 and then exp2 and then ...;
   --    end typPredicate;

   --  If Predicate_Function_Needs_Membership_Parameter is true, then this
   --  function takes an additional boolean parameter; the parameter
   --  indicates whether the predicate evaluation is part of a membership
   --  test. This parameter is used in two cases: 1) It is passed along
   --  if another predicate function is called and that predicate function
   --  expects to be passed a boolean parameter. 2) If the Predicate_Failure
   --  aspect is directly specified for typ, then we replace the return
   --  expression described above with
   --      (if <expression described above> then True
   --       elsif For_Membership_Test then False
   --       else (raise Assertion_Error
   --                     with <Predicate_Failure expression>))
   --  Here exp1, and exp2 are expressions from Predicate pragmas. Note that
   --  this is the point at which these expressions get analyzed, providing the
   --  required delay, and typ1, typ2, are entities from which predicates are
   --  inherited. Note that we do NOT generate Check pragmas, that's because we
   --  use this function even if checks are off, e.g. for membership tests.

   --  Note that the inherited predicates are evaluated first, as required by
   --  AI12-0071-1.

   --  Note that Sem_Eval.Real_Or_String_Static_Predicate_Matches depends on
   --  the form of this return expression.

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Build_Predicate_Function (Typ : Entity_Id; N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      Expr : Node_Id;
      --  This is the expression for the result of the function. It is
      --  built by connecting the component predicates with AND THEN.

      Object_Name : Name_Id;
      --  Name for argument of Predicate procedure. Note that we use the same
      --  name for both predicate functions. That way the reference within the
      --  predicate expression is the same in both functions.

      Object_Entity : Entity_Id;
      --  Entity for argument of Predicate procedure

      FDecl : Node_Id;
      --  The function declaration

      SId : Entity_Id;
      --  Its entity

      Restore_Scope : Boolean;
      --  True if the current scope must be restored on exit

      Ancestor_Predicate_Function_Called : Boolean := False;
      --  Does this predicate function include a call to the
      --  predication function of an ancestor subtype?

      procedure Add_Condition (Cond : Node_Id);
      --  Append Cond to Expr using "and then" (or just copy Cond to Expr if
      --  Expr is empty).

      procedure Add_Predicates;
      --  Appends expressions for any Predicate pragmas in the rep item chain
      --  Typ to Expr. Note that we look only at items for this exact entity.
      --  Inheritance of predicates for the parent type is done by calling the
      --  Predicate_Function of the parent type, using Add_Call above.

      procedure Add_Call (T : Entity_Id);
      --  Includes a call to the predicate function for type T in Expr if
      --  Predicate_Function (T) is non-empty.

      procedure Replace_Current_Instance_References
         (N : Node_Id; Typ, New_Entity : Entity_Id);
      --  Replace all references to Typ in the tree rooted at N with
      --  references to Param. [New_Entity will be a formal parameter of a
      --  predicate function.]

      --------------
      -- Add_Call --
      --------------

      procedure Add_Call (T : Entity_Id) is
         Exp : Node_Id;

      begin
         if Present (Predicate_Function (T)) then
            pragma Assert (Has_Predicates (Typ));

            --  Build the call to the predicate function of T. The type may be
            --  derived, so use an unchecked conversion for the actual.

            declare
               Dynamic_Mem : Node_Id := Empty;
               Second_Formal : constant Entity_Id :=
                 Next_Entity (Object_Entity);
            begin
               --  Some predicate functions require a second parameter;
               --  If one predicate function calls another and the second
               --  requires two parameters, then the first should also
               --  take two parameters (so that the first function has
               --  something to pass to the second function).
               if Predicate_Function_Needs_Membership_Parameter (T) then
                  pragma Assert (Present (Second_Formal));
                  Dynamic_Mem := New_Occurrence_Of (Second_Formal, Loc);
               end if;

               Exp :=
                 Make_Predicate_Call
                   (Typ  => T,
                    Expr =>
                      Unchecked_Convert_To (T,
                        Make_Identifier (Loc, Object_Name)),
                    Dynamic_Mem => Dynamic_Mem);
            end;

            --  "and"-in the call to evolving expression

            Add_Condition (Exp);
            Ancestor_Predicate_Function_Called := True;

            --  Output info message on inheritance if required. Note we do not
            --  give this information for generic actual types, since it is
            --  unwelcome noise in that case in instantiations. We also
            --  generally suppress the message in instantiations, and also
            --  if it involves internal names.

            if List_Inherited_Aspects
              and then not Is_Generic_Actual_Type (Typ)
              and then Instantiation_Location (Sloc (Typ)) = No_Location
              and then not Is_Internal_Name (Chars (T))
              and then not Is_Internal_Name (Chars (Typ))
            then
               Error_Msg_Sloc := Sloc (Predicate_Function (T));
               Error_Msg_Node_2 := T;
               Error_Msg_N ("info: & inherits predicate from & #?.l?", Typ);
            end if;
         end if;
      end Add_Call;

      -------------------
      -- Add_Condition --
      -------------------

      procedure Add_Condition (Cond : Node_Id) is
      begin
         --  This is the first predicate expression

         if No (Expr) then
            Expr := Cond;

         --  Otherwise concatenate to the existing predicate expressions by
         --  using "and then".

         else
            Expr :=
              Make_And_Then (Loc,
                Left_Opnd  => Relocate_Node (Expr),
                Right_Opnd => Cond);
         end if;
      end Add_Condition;

      --------------------
      -- Add_Predicates --
      --------------------

      procedure Add_Predicates is
         procedure Add_Predicate (Prag : Node_Id);
         --  Concatenate the expression of predicate pragma Prag to Expr by
         --  using a short circuit "and then" operator.

         -------------------
         -- Add_Predicate --
         -------------------

         procedure Add_Predicate (Prag : Node_Id) is
            --  Local variables

            Asp  : constant Node_Id := Corresponding_Aspect (Prag);
            Arg1 : Node_Id;
            Arg2 : Node_Id;

         --  Start of processing for Add_Predicate

         begin
            --  A ghost predicate is checked only when Ghost mode is enabled.
            --  Add a condition for the presence of a predicate to be recorded,
            --  which is needed to generate the corresponding predicate
            --  function.

            if Is_Ignored_Ghost_Pragma (Prag) then
               Add_Condition (New_Occurrence_Of (Standard_True, Sloc (Prag)));

            else
               --  Mark corresponding SCO as enabled

               Set_SCO_Pragma_Enabled (Sloc (Prag));
            end if;

            --  Extract the arguments of the pragma

            Arg1 := First (Pragma_Argument_Associations (Prag));
            Arg2 := Next (Arg1);

            Arg1 := Get_Pragma_Arg (Arg1);
            Arg2 := Get_Pragma_Arg (Arg2);

            --  When the predicate pragma applies to the current type or its
            --  full view, replace all occurrences of the subtype name with
            --  references to the formal parameter of the predicate function.

            if Entity (Arg1) = Typ
              or else Full_View (Entity (Arg1)) = Typ
            then
               declare
                  Arg2_Copy : constant Node_Id := New_Copy_Tree (Arg2);
               begin
                  Replace_Current_Instance_References
                   (Arg2_Copy, Typ => Typ, New_Entity => Object_Entity);

                  --  If the predicate pragma comes from an aspect, replace the
                  --  saved expression because we need the subtype references
                  --  replaced for the calls to Preanalyze_Spec_Expression in
                  --  Check_Aspect_At_xxx routines.

                  if Present (Asp) then
                     Set_Expression_Copy (Asp, New_Copy_Tree (Arg2_Copy));
                  end if;

                  --  "and"-in the Arg2 condition to evolving expression

                  if not Is_Ignored_Ghost_Pragma (Prag) then
                     Add_Condition (Arg2_Copy);
                  end if;
               end;
            end if;
         end Add_Predicate;

         --  Local variables

         Ritem : Node_Id;

      --  Start of processing for Add_Predicates

      begin
         Ritem := First_Rep_Item (Typ);

         --  If the type is private, check whether full view has inherited
         --  predicates.

         if Is_Private_Type (Typ)
           and then No (Ritem)
           and then Present (Full_View (Typ))
         then
            Ritem := First_Rep_Item (Full_View (Typ));
         end if;

         while Present (Ritem) loop
            if Nkind (Ritem) = N_Pragma
              and then Pragma_Name (Ritem) = Name_Predicate
            then
               Add_Predicate (Ritem);

            --  If the type is declared in an inner package it may be frozen
            --  outside of the package, and the generated pragma has not been
            --  analyzed yet, so capture the expression for the predicate
            --  function at this point.

            elsif Nkind (Ritem) = N_Aspect_Specification
              and then Present (Aspect_Rep_Item (Ritem))
              and then Scope_Depth (Scope (Typ)) > Scope_Depth (Current_Scope)
            then
               declare
                  Prag : constant Node_Id := Aspect_Rep_Item (Ritem);

               begin
                  if Nkind (Prag) = N_Pragma
                    and then Pragma_Name (Prag) = Name_Predicate
                  then
                     Add_Predicate (Prag);
                  end if;
               end;
            end if;

            Next_Rep_Item (Ritem);
         end loop;
      end Add_Predicates;

      -----------------------------------------
      -- Replace_Current_Instance_References --
      -----------------------------------------

      procedure Replace_Current_Instance_References
         (N : Node_Id; Typ, New_Entity : Entity_Id)
      is
         Root : Node_Id renames N;

         procedure Replace_One_Reference (N : Node_Id);
         --  Actual parameter for Replace_Type_References_Generic instance

         ---------------------------
         -- Replace_One_Reference --
         ---------------------------

         procedure Replace_One_Reference (N : Node_Id) is
            pragma Assert (In_Subtree (N, Root => Root));
         begin
            Rewrite (N, New_Occurrence_Of (New_Entity, Sloc (N)));
            --  Use the Sloc of the usage name, not the defining name
         end Replace_One_Reference;

         procedure Replace_Type_References is
           new Replace_Type_References_Generic (Replace_One_Reference);
      begin
         Replace_Type_References (N, Typ);
      end Replace_Current_Instance_References;

   --  Start of processing for Build_Predicate_Function

   begin
      --  Return if already built, if type does not have predicates,
      --  or if type is a constructed subtype that will inherit a
      --  predicate function from its ancestor. In a generic context
      --  the predicated parent may not have a predicate function yet
      --  but we don't want to build a new one for the subtype. This can
      --  happen in an instance body which is nested within a generic
      --  unit, in which case Within_A_Generic may be false, SId is
      --  Empty, but uses of Typ will receive a predicate check in a
      --  context where expansion and tests are enabled.

      SId := Predicate_Function (Typ);
      if not Has_Predicates (Typ)
        or else (Present (SId) and then Has_Completion (SId))
        or else
          (Is_Itype (Typ)
           and then not Comes_From_Source (Typ)
           and then Ekind (Typ) in E_Array_Subtype
                                 | E_Record_Subtype
                                 | E_Record_Subtype_With_Private
           and then Present (Predicated_Parent (Typ)))
      then
         return;

      --  Do not generate predicate bodies within a generic unit. The
      --  expressions have been analyzed already, and the bodies play no role
      --  if not within an executable unit. However, if a static predicate is
      --  present it must be processed for legality checks such as case
      --  coverage in an expression.

      elsif Inside_A_Generic
        and then not Has_Static_Predicate_Aspect (Typ)
      then
         return;
      end if;

      --  Ensure that the declarations are added to the scope of the type

      if Scope (Typ) /= Current_Scope then
         Push_Scope (Scope (Typ));
         Restore_Scope := True;
      else
         Restore_Scope := False;
      end if;

      --  The related type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the predicate functions are properly marked as Ghost.

      Set_Ghost_Mode (Typ);

      --  Prepare to construct predicate expression

      Expr := Empty;

      if Present (SId) then
         FDecl := Unit_Declaration_Node (SId);

      else
         FDecl := Build_Predicate_Function_Declaration (Typ);
         SId   := Defining_Entity (FDecl);
      end if;

      --  Recover name of formal parameter of function that replaces references
      --  to the type in predicate expressions.

      Object_Entity :=
         Defining_Identifier
           (First (Parameter_Specifications (Specification (FDecl))));

      Object_Name   := Chars (Object_Entity);

      --  Add predicates for ancestor if present. These must come before the
      --  ones for the current type, as required by AI12-0071-1.

      --  Looks like predicates aren't added for case of inheriting from
      --  multiple progenitors???

      declare
         Atyp : Entity_Id;
      begin
         Atyp := Nearest_Ancestor (Typ);

         --  The type may be private but the full view may inherit predicates

         if No (Atyp) and then Is_Private_Type (Typ) then
            Atyp := Nearest_Ancestor (Full_View (Typ));
         end if;

         if Present (Atyp) then
            Add_Call (Atyp);
         end if;
      end;

      --  Add Predicates for the current type

      Add_Predicates;

      --  Case where predicates are present

      if Present (Expr) then

         --  Build the main predicate function

         declare
            SIdB : constant Entity_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Typ), "Predicate"));
            --  The entity for the function body

            Spec  : Node_Id;
            FBody : Node_Id;

         begin
            Mutate_Ekind (SIdB, E_Function);
            Set_Is_Predicate_Function (SIdB);

            --  Build function body

            declare
               Param_Specs : constant List_Id := New_List (
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Object_Name),
                   Parameter_Type =>
                     New_Occurrence_Of (Typ, Loc)));
            begin
               --  if Spec has 2 parameters, then body should too
               if Present (Next_Entity (Object_Entity)) then
                  Append (Make_Parameter_Specification (Loc,
                            Defining_Identifier =>
                              Make_Defining_Identifier
                                (Loc, Chars (Next_Entity (Object_Entity))),
                            Parameter_Type      =>
                              New_Occurrence_Of (Standard_Boolean, Loc)),
                          Param_Specs);
               end if;

               Spec :=
                 Make_Function_Specification (Loc,
                   Defining_Unit_Name       => SIdB,
                   Parameter_Specifications => Param_Specs,
                   Result_Definition        =>
                     New_Occurrence_Of (Standard_Boolean, Loc));
            end;

            --  The Predicate_Expression attribute is used by SPARK.
            --
            --  If Ancestor_Predicate_Function_Called is True, then
            --  we try to exclude that call to the ancestor's
            --  predicate function by calling Right_Opnd.
            --  The call is not excluded in the case where
            --  it is not "and"ed with anything else (so we don't have
            --  an N_And_Then node). This exclusion is required if the
            --  Predicate_Failure aspect is specified for Typ because
            --  in that case we are going to drop the N_And_Then node
            --  on the floor. Otherwise, it is a question of what is
            --  most convenient for SPARK.

            Set_Predicate_Expression
              (SId, (if Ancestor_Predicate_Function_Called
                       and then Nkind (Expr) = N_And_Then
                     then Right_Opnd (Expr)
                     else Expr));

            declare
               Result_Expr   : Node_Id := Expr;
               PF_Expr       : Node_Id := Predicate_Failure_Expression
                                            (Typ, Inherited_OK => False);
               PF_Expr_Copy  : Node_Id;
               Second_Formal : constant Entity_Id :=
                 Next_Entity (Object_Entity);
            begin
               --  In GNATprove mode we are only interested in the predicate
               --  expression itself and don't want a raise expression that
               --  comes from the Predicate_Failure. Ditto for CodePeer.
               --  And an illegal Predicate_Failure aspect can lead to cases
               --  we want to avoid.

               if Present (PF_Expr)
                 and then not GNATprove_Mode
                 and then not CodePeer_Mode
                 and then Serious_Errors_Detected = 0
               then
                  pragma Assert (Present (Second_Formal));

                  --  This is an ugly hack to cope with an ugly situation.
                  --  PF_Expr may have children whose Parent attribute
                  --  does not point back to PF_Expr. If we pass such a
                  --  tree to New_Copy_Tree, then it does not make a deep
                  --  copy. But we need a deep copy. So we need to find a
                  --  tree for which New_Copy_Tree *will* make a deep copy.

                  declare
                     function Check_Node_Parent (Parent_Node, Node : Node_Id)
                       return Traverse_Result;
                     function Check_Node_Parent (Parent_Node, Node : Node_Id)
                       return Traverse_Result is
                     begin
                        if Parent_Node = PF_Expr
                          and then not Is_List_Member (Node)
                        then
                           pragma Assert
                             (Nkind (PF_Expr) = Nkind (Parent (Node)));

                           --  We need PF_Expr to be a node for which
                           --  New_Copy_Tree will make a deep copy.
                           PF_Expr := Parent (Node);
                           return Abandon;
                        end if;
                        return OK;
                     end Check_Node_Parent;
                     procedure Check_Parentage is
                       new Traverse_Proc_With_Parent (Check_Node_Parent);
                  begin
                     Check_Parentage (PF_Expr);
                     PF_Expr_Copy := New_Copy_Tree (PF_Expr);
                  end;

                  --  Current instance uses need to have their Entity
                  --  fields set so that Replace_Current_Instance_References
                  --  can find them. So we preanalyze. Just for purposes of
                  --  calls to Is_Current_Instance during this preanalysis,
                  --  we set the Parent field.
                  Set_Parent (PF_Expr_Copy, Parent (PF_Expr));
                  Preanalyze (PF_Expr_Copy);
                  Set_Parent (PF_Expr_Copy, Empty);

                  Replace_Current_Instance_References
                    (PF_Expr_Copy, Typ => Typ, New_Entity => Object_Entity);

                  if Ancestor_Predicate_Function_Called then
                     --  If the call to an ancestor predicate function
                     --  returns False, we do not want to raise an
                     --  exception here. Our Predicate_Failure aspect does
                     --  not apply in that case. So we have to build a
                     --  more complicated result expression:
                     --   (if not Ancestor_Predicate_Function (...) then False
                     --    elsif Noninherited_Predicates (...) then True
                     --    elsif Is_Membership_Test then False
                     --    else (raise Assertion_Error with PF text))

                     declare
                        Ancestor_Call : constant Node_Id :=
                          Left_Opnd (Result_Expr);
                        Local_Preds   : constant Node_Id :=
                          Right_Opnd (Result_Expr);
                     begin
                        Result_Expr :=
                          Make_If_Expression (Loc,
                            Expressions => New_List (
                              Make_Op_Not (Loc, Ancestor_Call),
                              New_Occurrence_Of (Standard_False, Loc),
                              Make_If_Expression (Loc,
                                Is_Elsif => True,
                                Expressions => New_List (
                                  Local_Preds,
                                  New_Occurrence_Of (Standard_True, Loc),
                                  Make_If_Expression (Loc,
                                    Is_Elsif => True,
                                    Expressions => New_List (
                                      New_Occurrence_Of (Second_Formal, Loc),
                                      New_Occurrence_Of (Standard_False, Loc),
                                      Make_Raise_Expression (Loc,
                                        New_Occurrence_Of (RTE
                                          (RE_Assert_Failure), Loc),
                                        PF_Expr_Copy)))))));
                     end;

                  else
                     --  Build a conditional expression:
                     --   (if <predicate evaluates to True> then True
                     --    elsif Is_Membership_Test then False
                     --    else (raise Assertion_Error with PF text))

                     Result_Expr :=
                       Make_If_Expression (Loc,
                         Expressions => New_List (
                           Result_Expr,
                           New_Occurrence_Of (Standard_True, Loc),
                           Make_If_Expression (Loc,
                             Is_Elsif => True,
                             Expressions => New_List (
                               New_Occurrence_Of (Second_Formal, Loc),
                               New_Occurrence_Of (Standard_False, Loc),
                               Make_Raise_Expression (Loc,
                                 New_Occurrence_Of (RTE
                                   (RE_Assert_Failure), Loc),
                                 PF_Expr_Copy)))));
                  end if;
               end if;

               FBody :=
                 Make_Subprogram_Body (Loc,
                   Specification              => Spec,
                   Declarations               => Empty_List,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Simple_Return_Statement (Loc,
                           Expression => Result_Expr))));
            end;

            --  The declaration has been analyzed when created, and placed
            --  after type declaration. Insert body itself after freeze node,
            --  unless subprogram declaration is already there, in which case
            --  body better be placed afterwards.

            if FDecl = Next (N) then
               Insert_After_And_Analyze (FDecl, FBody);
            else
               Insert_After_And_Analyze (N, FBody);
            end if;

            --  The defining identifier of a quantified expression carries the
            --  scope in which the type appears, but when unnesting we need
            --  to indicate that its proper scope is the constructed predicate
            --  function. The quantified expressions have been converted into
            --  loops during analysis and expansion.

            declare
               function Reset_Quantified_Variable_Scope
                 (N : Node_Id) return Traverse_Result;

               procedure Reset_Quantified_Variables_Scope is
                 new Traverse_Proc (Reset_Quantified_Variable_Scope);

               -------------------------------------
               -- Reset_Quantified_Variable_Scope --
               -------------------------------------

               function Reset_Quantified_Variable_Scope
                 (N : Node_Id) return Traverse_Result is
               begin
                  if Nkind (N) in N_Iterator_Specification
                                | N_Loop_Parameter_Specification
                  then
                     Set_Scope (Defining_Identifier (N),
                       Predicate_Function (Typ));
                  end if;

                  return OK;
               end Reset_Quantified_Variable_Scope;

            begin
               if Unnest_Subprogram_Mode then
                  Reset_Quantified_Variables_Scope (Expr);
               end if;
            end;

            --  Within a generic unit, prevent a double analysis of the body
            --  which will not be marked analyzed yet. This will happen when
            --  the freeze node is created during the preanalysis of an
            --  expression function.

            if Inside_A_Generic then
               Set_Analyzed (FBody);
            end if;

            --  Static predicate functions are always side-effect-free, and
            --  in most cases dynamic predicate functions are as well. Mark
            --  them as such whenever possible, so redundant predicate checks
            --  can be optimized. If there is a variable reference within the
            --  expression, the function is not pure.

            if Expander_Active then
               Set_Is_Pure (SId,
                 Side_Effect_Free (Expr, Variable_Ref => True));
               Set_Is_Inlined (SId);
            end if;
         end;

         --  See if we have a static predicate. Note that the answer may be
         --  yes even if we have an explicit Dynamic_Predicate present.

         declare
            PS : Boolean;
            EN : Node_Id;

         begin
            if not Is_Scalar_Type (Typ) and then not Is_String_Type (Typ) then
               PS := False;
            else
               PS := Is_Predicate_Static (Expr, Object_Name);
            end if;

            --  Case where we have a predicate-static aspect

            if PS then

               --  We don't set Has_Static_Predicate_Aspect, since we can have
               --  any of the three cases (Predicate, Dynamic_Predicate, or
               --  Static_Predicate) generating a predicate with an expression
               --  that is predicate-static. We just indicate that we have a
               --  predicate that can be treated as static.

               Set_Has_Static_Predicate (Typ);

               --  For discrete subtype, build the static predicate list

               if Is_Discrete_Type (Typ) then
                  Build_Discrete_Static_Predicate (Typ, Expr, Object_Name);

                  --  If we don't get a static predicate list, it means that we
                  --  have a case where this is not possible, most typically in
                  --  the case where we inherit a dynamic predicate. We do not
                  --  consider this an error, we just leave the predicate as
                  --  dynamic. But if we do succeed in building the list, then
                  --  we mark the predicate as static.

                  if No (Static_Discrete_Predicate (Typ)) then
                     Set_Has_Static_Predicate (Typ, False);
                  end if;

               --  For real or string subtype, save predicate expression

               elsif Is_Real_Type (Typ) or else Is_String_Type (Typ) then
                  Set_Static_Real_Or_String_Predicate (Typ, Expr);
               end if;

            --  Case of dynamic predicate (expression is not predicate-static)

            else
               --  Again, we don't set Has_Dynamic_Predicate_Aspect, since that
               --  is only set if we have an explicit Dynamic_Predicate aspect
               --  given. Here we may simply have a Predicate aspect where the
               --  expression happens not to be predicate-static.

               --  Emit an error when the predicate is categorized as static
               --  but its expression is not predicate-static.

               --  First a little fiddling to get a nice location for the
               --  message. If the expression is of the form (A and then B),
               --  where A is an inherited predicate, then use the right
               --  operand for the Sloc. This avoids getting confused by a call
               --  to an inherited predicate with a less convenient source
               --  location.

               EN := Expr;
               while Nkind (EN) = N_And_Then
                 and then Nkind (Left_Opnd (EN)) = N_Function_Call
                 and then Is_Predicate_Function
                            (Entity (Name (Left_Opnd (EN))))
               loop
                  EN := Right_Opnd (EN);
               end loop;

               --  Now post appropriate message

               if Has_Static_Predicate_Aspect (Typ) then
                  if Is_Scalar_Type (Typ) or else Is_String_Type (Typ) then
                     Error_Msg_F
                       ("expression is not predicate-static (RM 3.2.4(16-22))",
                        EN);
                  else
                     Error_Msg_F
                       ("static predicate requires scalar or string type", EN);
                  end if;
               end if;
            end if;
         end;
      end if;

      Restore_Ghost_Region (Saved_GM, Saved_IGR);

      if Restore_Scope then
         Pop_Scope;
      end if;
   end Build_Predicate_Function;

   ------------------------------------------
   -- Build_Predicate_Function_Declaration --
   ------------------------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   function Build_Predicate_Function_Declaration
     (Typ : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      Func_Decl : Node_Id;
      Func_Id   : Entity_Id;
      Spec      : Node_Id;

      CRec_Typ : Entity_Id;
      --  The corresponding record type of Full_Typ

      Full_Typ : Entity_Id;
      --  The full view of Typ

      Priv_Typ : Entity_Id;
      --  The partial view of Typ

      UFull_Typ : Entity_Id;
      --  The underlying full view of Full_Typ

   begin
      --  The related type may be subject to pragma Ghost. Set the mode now to
      --  ensure that the predicate functions are properly marked as Ghost.

      Set_Ghost_Mode (Typ);

      Func_Id :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (Typ), "Predicate"));

      Mutate_Ekind (Func_Id, E_Function);
      Set_Etype (Func_Id, Standard_Boolean);
      Set_Is_Internal (Func_Id);
      Set_Is_Predicate_Function (Func_Id);
      Set_Predicate_Function (Typ, Func_Id);

      --  The predicate function requires debug info when the predicates are
      --  subject to Source Coverage Obligations.

      if Opt.Generate_SCO then
         Set_Debug_Info_Needed (Func_Id);
      end if;

      --  Obtain all views of the input type

      Get_Views (Typ, Priv_Typ, Full_Typ, UFull_Typ, CRec_Typ);

      --  Associate the predicate function and various flags with all views

      Propagate_Predicate_Attributes (Priv_Typ,  From_Typ => Typ);
      Propagate_Predicate_Attributes (Full_Typ,  From_Typ => Typ);
      Propagate_Predicate_Attributes (UFull_Typ, From_Typ => Typ);
      Propagate_Predicate_Attributes (CRec_Typ,  From_Typ => Typ);

      declare
         Param_Specs : constant List_Id := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Temporary (Loc, 'I'),
             Parameter_Type      => New_Occurrence_Of (Typ, Loc)));
      begin
         if Predicate_Function_Needs_Membership_Parameter (Typ) then
            --  Add Boolean-valued For_Membership_Test param
            Append (Make_Parameter_Specification (Loc,
                      Defining_Identifier => Make_Temporary (Loc, 'M'),
                      Parameter_Type      =>
                        New_Occurrence_Of (Standard_Boolean, Loc)),
                    Param_Specs);
         end if;

         Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Func_Id,
             Parameter_Specifications => Param_Specs,
             Result_Definition        =>
               New_Occurrence_Of (Standard_Boolean, Loc));
      end;

      Func_Decl := Make_Subprogram_Declaration (Loc, Specification => Spec);

      Insert_After (Parent (Typ), Func_Decl);
      Analyze (Func_Decl);

      Restore_Ghost_Region (Saved_GM, Saved_IGR);

      return Func_Decl;
   end Build_Predicate_Function_Declaration;

   -----------------------------------------
   -- Check_Aspect_At_End_Of_Declarations --
   -----------------------------------------

   procedure Check_Aspect_At_End_Of_Declarations (ASN : Node_Id) is
      Ent   : constant Entity_Id := Entity     (ASN);
      Ident : constant Node_Id   := Identifier (ASN);
      A_Id  : constant Aspect_Id := Get_Aspect_Id (Chars (Ident));

      End_Decl_Expr : constant Node_Id := Expression_Copy (ASN);
      --  Expression to be analyzed at end of declarations

      Freeze_Expr : constant Node_Id := Expression (ASN);
      --  Expression from call to Check_Aspect_At_Freeze_Point

      T : constant Entity_Id :=
            (if Present (Freeze_Expr) and then A_Id /= Aspect_Stable_Properties
             then Etype (Original_Node (Freeze_Expr))
             else Empty);
      --  Type required for preanalyze call. We use the original expression to
      --  get the proper type, to prevent cascaded errors when the expression
      --  is constant-folded. For Stable_Properties, the aspect value is
      --  not semantically an expression (although it is syntactically);
      --  in particular, it has no type.

      Err : Boolean;
      --  Set True if error

      --  On entry to this procedure, Entity (Ident) contains a copy of the
      --  original expression from the aspect, saved for this purpose, and
      --  but Expression (Ident) is a preanalyzed copy of the expression,
      --  preanalyzed just after the freeze point.

      procedure Check_Overloaded_Name;
      --  For aspects whose expression is simply a name, this routine checks if
      --  the name is overloaded or not. If so, it verifies there is an
      --  interpretation that matches the entity obtained at the freeze point,
      --  otherwise the compiler complains.

      ---------------------------
      -- Check_Overloaded_Name --
      ---------------------------

      procedure Check_Overloaded_Name is
      begin
         if not Is_Overloaded (End_Decl_Expr) then
            Err := not Is_Entity_Name (End_Decl_Expr)
                     or else Entity (End_Decl_Expr) /= Entity (Freeze_Expr);

         else
            Err := True;

            declare
               Index : Interp_Index;
               It    : Interp;

            begin
               Get_First_Interp (End_Decl_Expr, Index, It);
               while Present (It.Typ) loop
                  if It.Nam = Entity (Freeze_Expr) then
                     Err := False;
                     exit;
                  end if;

                  Get_Next_Interp (Index, It);
               end loop;
            end;
         end if;
      end Check_Overloaded_Name;

   --  Start of processing for Check_Aspect_At_End_Of_Declarations

   begin
      --  In an instance we do not perform the consistency check between freeze
      --  point and end of declarations, because it was done already in the
      --  analysis of the generic. Furthermore, the delayed analysis of an
      --  aspect of the instance may produce spurious errors when the generic
      --  is a child unit that references entities in the parent (which might
      --  not be in scope at the freeze point of the instance).

      if In_Instance then
         return;

      --  The enclosing scope may have been rewritten during expansion (e.g. a
      --  task body is rewritten as a procedure) after this conformance check
      --  has been performed, so do not perform it again (it may not easily be
      --  done if full visibility of local entities is not available).

      elsif not Comes_From_Source (Current_Scope) then
         return;

      --  Case of aspects Dimension, Dimension_System and Synchronization

      elsif A_Id = Aspect_Synchronization then
         return;

      --  Case of stream attributes and Put_Image, just have to compare
      --  entities. However, the expression is just a possibly-overloaded
      --  name, so we need to verify that one of these interpretations is
      --  the one available at at the freeze point.

      elsif A_Id in Aspect_Input
                  | Aspect_Output
                  | Aspect_Read
                  | Aspect_Write
                  | Aspect_Put_Image
      then
         Analyze (End_Decl_Expr);
         Check_Overloaded_Name;

      elsif A_Id in Aspect_Variable_Indexing
                  | Aspect_Constant_Indexing
                  | Aspect_Default_Iterator
                  | Aspect_Iterator_Element
                  | Aspect_Integer_Literal
                  | Aspect_Real_Literal
                  | Aspect_String_Literal
      then
         --  Make type unfrozen before analysis, to prevent spurious errors
         --  about late attributes.

         Set_Is_Frozen (Ent, False);
         Analyze (End_Decl_Expr);
         Set_Is_Frozen (Ent, True);

         --  If the end of declarations comes before any other freeze point,
         --  the Freeze_Expr is not analyzed: no check needed.

         if Analyzed (Freeze_Expr) then
            Check_Overloaded_Name;
         else
            Err := False;
         end if;

      --  All other cases

      else
         --  In a generic context freeze nodes are not always generated, so
         --  analyze the expression now. If the aspect is for a type, we must
         --  also make its potential components accessible.

         if not Analyzed (Freeze_Expr) and then Inside_A_Generic then
            if A_Id in Aspect_Dynamic_Predicate
                     | Aspect_Ghost_Predicate
                     | Aspect_Predicate
                     | Aspect_Static_Predicate
            then
               Push_Type (Ent);
               Preanalyze_Spec_Expression (Freeze_Expr, Standard_Boolean);
               Pop_Type (Ent);

            elsif A_Id = Aspect_Priority then
               Push_Type (Ent);
               Preanalyze_Spec_Expression (Freeze_Expr, Any_Integer);
               Pop_Type (Ent);

            else
               Preanalyze (Freeze_Expr);
            end if;
         end if;

         --  Indicate that the expression comes from an aspect specification,
         --  which is used in subsequent analysis even if expansion is off.

         if Present (End_Decl_Expr) then
            Set_Parent (End_Decl_Expr, ASN);
         end if;

         --  In a generic context the original aspect expressions have not
         --  been preanalyzed, so do it now. There are no conformance checks
         --  to perform in this case. As before, we have to make components
         --  visible for aspects that may reference them.

         if Present (Freeze_Expr) and then No (T) then
            if A_Id in Aspect_Dynamic_Predicate
                     | Aspect_Ghost_Predicate
                     | Aspect_Predicate
                     | Aspect_Priority
                     | Aspect_Static_Predicate
            then
               Push_Type (Ent);
               Check_Aspect_At_Freeze_Point (ASN);
               Pop_Type (Ent);

            else
               Check_Aspect_At_Freeze_Point (ASN);
            end if;
            return;

         --  The default values attributes may be defined in the private part,
         --  and the analysis of the expression may take place when only the
         --  partial view is visible. The expression must be scalar, so use
         --  the full view to resolve.

         elsif A_Id in Aspect_Default_Component_Value | Aspect_Default_Value
            and then Is_Private_Type (T)
         then
            Preanalyze_Spec_Expression (End_Decl_Expr, Full_View (T));

         --  The following aspect expressions may contain references to
         --  components and discriminants of the type.

         elsif A_Id in Aspect_CPU
                     | Aspect_Dynamic_Predicate
                     | Aspect_Ghost_Predicate
                     | Aspect_Interrupt_Priority
                     | Aspect_Predicate
                     | Aspect_Priority
                     | Aspect_Static_Predicate
         then
            Push_Type (Ent);
            Preanalyze_Spec_Expression (End_Decl_Expr, T);
            Pop_Type (Ent);

         elsif A_Id = Aspect_Predicate_Failure then
            Preanalyze_Spec_Expression (End_Decl_Expr, Standard_String);
         elsif Present (End_Decl_Expr) then
            Preanalyze_Spec_Expression (End_Decl_Expr, T);
         end if;

         Err :=
           not Fully_Conformant_Expressions
                 (End_Decl_Expr, Freeze_Expr, Report => True);
      end if;

      --  Output error message if error. Force error on aspect specification
      --  even if there is an error on the expression itself.

      if Err then
         Error_Msg_NE
           ("!visibility of aspect for& changes after freeze point",
            ASN, Ent);
         Error_Msg_Sloc := Sloc (Freeze_Node (Ent));
         Error_Msg_NE
           ("\& is frozen #, (RM 13.1.1 (13/3))",
            ASN, Ent);
      end if;
   end Check_Aspect_At_End_Of_Declarations;

   ------------------------------------------
   -- Check_Aspects_At_End_Of_Declarations --
   ------------------------------------------

   procedure Check_Aspects_At_End_Of_Declarations (E : Entity_Id) is
      ASN : Node_Id;

   begin
      ASN := First_Rep_Item (E);

      while Present (ASN) loop
         if Nkind (ASN) = N_Aspect_Specification
           and then Entity (ASN) = E
           and then Is_Delayed_Aspect (ASN)
         then
            Check_Aspect_At_End_Of_Declarations (ASN);
         end if;

         Next_Rep_Item (ASN);
      end loop;
   end Check_Aspects_At_End_Of_Declarations;

   ----------------------------------
   -- Check_Aspect_At_Freeze_Point --
   ----------------------------------

   procedure Check_Aspect_At_Freeze_Point (ASN : Node_Id) is
      Ident : constant Node_Id := Identifier (ASN);
      --  Identifier (use Entity field to save expression)

      Expr : constant Node_Id := Expression (ASN);
      --  For cases where using Entity (Identifier) doesn't work

      A_Id : constant Aspect_Id := Get_Aspect_Id (Chars (Ident));

      T : Entity_Id := Empty;
      --  Type required for preanalyze call

   begin
      --  On entry to this procedure, Entity (Ident) contains a copy of the
      --  original expression from the aspect, saved for this purpose.

      --  On exit from this procedure Entity (Ident) is unchanged, still
      --  containing that copy, but Expression (Ident) is a preanalyzed copy
      --  of the expression, preanalyzed just after the freeze point.

      --  Make a copy of the expression to be preanalyzed

      Set_Expression (ASN, New_Copy_Tree (Expression_Copy (ASN)));

      --  Find type for preanalyze call

      case A_Id is

         --  No_Aspect should be impossible

         when No_Aspect =>
            raise Program_Error;

         --  Aspects taking an optional boolean argument

         when Boolean_Aspects
            | Library_Unit_Aspects
         =>
            T := Standard_Boolean;

         --  Aspects corresponding to attribute definition clauses

         when Aspect_Address =>
            T := RTE (RE_Address);

         when Aspect_Attach_Handler =>
            T := RTE (RE_Interrupt_ID);

         when Aspect_Bit_Order
            | Aspect_Scalar_Storage_Order
         =>
            T := RTE (RE_Bit_Order);

         when Aspect_Convention =>
            return;

         when Aspect_CPU =>
            T := RTE (RE_CPU_Range);

         --  Default_Component_Value is resolved with the component type

         when Aspect_Default_Component_Value =>
            T := Component_Type (Entity (ASN));

         when Aspect_Default_Storage_Pool =>
            T := Class_Wide_Type (RTE (RE_Root_Storage_Pool));

         --  Default_Value is resolved with the type entity in question

         when Aspect_Default_Value =>
            T := Entity (ASN);

         when Aspect_Dispatching_Domain =>
            T := RTE (RE_Dispatching_Domain);

         when Aspect_External_Tag =>
            T := Standard_String;

         when Aspect_External_Name =>
            T := Standard_String;

         when Aspect_Link_Name =>
            T := Standard_String;

         when Aspect_Interrupt_Priority
            | Aspect_Priority
         =>
            T := Standard_Integer;

         when Aspect_Relative_Deadline =>
            T := RTE (RE_Time_Span);

         when Aspect_Secondary_Stack_Size =>
            T := Standard_Integer;

         when Aspect_Small =>

            --  Note that the expression can be of any real type (not just a
            --  real universal literal) as long as it is a static constant.

            T := Any_Real;

         --  For a simple storage pool, we have to retrieve the type of the
         --  pool object associated with the aspect's corresponding attribute
         --  definition clause.

         when Aspect_Simple_Storage_Pool =>
            T := Etype (Expression (Aspect_Rep_Item (ASN)));

         when Aspect_Storage_Pool =>
            T := Class_Wide_Type (RTE (RE_Root_Storage_Pool));

         when Aspect_Alignment
            | Aspect_Component_Size
            | Aspect_Machine_Radix
            | Aspect_Object_Size
            | Aspect_Size
            | Aspect_Storage_Size
            | Aspect_Stream_Size
            | Aspect_Value_Size
         =>
            T := Any_Integer;

         when Aspect_Linker_Section =>
            T := Standard_String;

         when Aspect_Local_Restrictions =>
            return;

         when Aspect_Synchronization =>
            return;

         --  Special case, the expression of these aspects is just an entity
         --  that does not need any resolution, so just analyze.

         when Aspect_Input
            | Aspect_Output
            | Aspect_Put_Image
            | Aspect_Read
            | Aspect_Warnings
            | Aspect_Write
         =>
            Analyze (Expression (ASN));
            return;

         --  Same for Iterator aspects, where the expression is a function
         --  name. Legality rules are checked separately.

         when Aspect_Constant_Indexing
            | Aspect_Default_Iterator
            | Aspect_Iterator_Element
            | Aspect_Variable_Indexing
         =>
            Analyze (Expression (ASN));
            return;

         --  Same for Literal aspects, where the expression is a function
         --  name. Legality rules are checked separately. Use Expr to avoid
         --  losing track of the previous resolution of Expression.

         when Aspect_Integer_Literal
            | Aspect_Real_Literal
            | Aspect_String_Literal
         =>
            Set_Entity (Expression (ASN), Entity (Expr));
            Set_Etype (Expression (ASN), Etype (Expr));
            Set_Is_Overloaded (Expression (ASN), False);
            Analyze (Expression (ASN));
            return;

         --  Finalizable, legality checks in Validate_Finalizable_Aspect

         when Aspect_Finalizable =>
            T := Entity (ASN);

            if Nkind (Expression (ASN)) /= N_Aggregate then
               pragma Assert (Serious_Errors_Detected > 0);
               return;
            end if;

            declare
               Assoc : Node_Id;
               Exp   : Node_Id;
               Nam   : Node_Id;

            begin
               Assoc := First (Component_Associations (Expression (ASN)));
               while Present (Assoc) loop
                  Nam := First (Choices (Assoc));
                  Exp := Expression (Assoc);

                  if Chars (Nam) = Name_Relaxed_Finalization
                    and then Inside_A_Generic
                  then
                     Preanalyze_And_Resolve (Exp, Any_Boolean);

                  else
                     Analyze (Exp);
                     Resolve_Finalizable_Argument (Exp, T, Chars (Nam));
                  end if;

                  Next (Assoc);
               end loop;
            end;

            return;

         --  Iterable, legality checks in Validate_Iterable_Aspect

         when Aspect_Iterable =>
            T := Entity (ASN);

            if Nkind (Expression (ASN)) /= N_Aggregate then
               pragma Assert (Serious_Errors_Detected > 0);
               return;
            end if;

            declare
               Cursor : constant Entity_Id := Get_Cursor_Type (ASN, T);
               Assoc  : Node_Id;
               Expr   : Node_Id;

            begin
               if Cursor = Any_Type then
                  return;
               end if;

               Assoc := First (Component_Associations (Expression (ASN)));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Analyze (Expr);

                  if not Error_Posted (Expr) then
                     Resolve_Iterable_Operation
                       (Expr, Cursor, T, Chars (First (Choices (Assoc))));
                  end if;

                  Next (Assoc);
               end loop;
            end;

            return;

         when Aspect_Aggregate =>
            Resolve_Aspect_Aggregate (Entity (ASN), Expression (ASN));
            return;

         when Aspect_Stable_Properties =>
            Resolve_Aspect_Stable_Properties
              (Entity (ASN), Expression (ASN),
               Class_Present => Class_Present (ASN));
            return;

         --  Invariant/Predicate take boolean expressions

         when Aspect_Dynamic_Predicate
            | Aspect_Invariant
            | Aspect_Ghost_Predicate
            | Aspect_Predicate
            | Aspect_Static_Predicate
            | Aspect_Type_Invariant
         =>
            T := Standard_Boolean;

         when Aspect_Predicate_Failure =>
            T := Standard_String;

         --  As for some other aspects above, the expression of this aspect is
         --  just an entity that does not need any resolution, so just analyze.

         when Aspect_Designated_Storage_Model =>
            Analyze (Expression (ASN));
            return;

         when Aspect_Storage_Model_Type =>

            --  The aggregate argument of Storage_Model_Type is optional, and
            --  when not present the aspect defaults to the native storage
            --  model (where the address type is System.Address, and other
            --  arguments default to corresponding native storage operations).

            if No (Expression (ASN)) then
               return;
            end if;

            T := Entity (ASN);

            declare
               Assoc     : Node_Id;
               Expr      : Node_Id;
               Addr_Type : Entity_Id := Empty;

            begin
               Assoc := First (Component_Associations (Expression (ASN)));
               while Present (Assoc) loop
                  Expr := Expression (Assoc);
                  Analyze (Expr);

                  if not Error_Posted (Expr) then
                     Resolve_Storage_Model_Type_Argument
                       (Expr, T, Addr_Type, Chars (First (Choices (Assoc))));
                  end if;

                  Next (Assoc);
               end loop;
            end;

            return;

         --  Here is the list of aspects that don't require delay analysis

         when Aspect_Abstract_State
            | Aspect_Annotate
            | Aspect_Contract_Cases
            | Aspect_Default_Initial_Condition
            | Aspect_Depends
            | Aspect_Dimension
            | Aspect_Dimension_System
            | Aspect_Exceptional_Cases
            | Aspect_External_Initialization
            | Aspect_Global
            | Aspect_GNAT_Annotate
            | Aspect_Implicit_Dereference
            | Aspect_Initial_Condition
            | Aspect_Initializes
            | Aspect_Max_Entry_Queue_Length
            | Aspect_Max_Queue_Length
            | Aspect_Obsolescent
            | Aspect_Part_Of
            | Aspect_Post
            | Aspect_Postcondition
            | Aspect_Pre
            | Aspect_Precondition
            | Aspect_Refined_Depends
            | Aspect_Refined_Global
            | Aspect_Refined_Post
            | Aspect_Refined_State
            | Aspect_Relaxed_Initialization
            | Aspect_SPARK_Mode
            | Aspect_Subprogram_Variant
            | Aspect_Suppress
            | Aspect_Test_Case
            | Aspect_Unimplemented
            | Aspect_Unsuppress
            | Aspect_User_Aspect
         =>
            raise Program_Error;

      end case;

      --  Do the preanalyze call

      if Present (Expression (ASN)) then
         Preanalyze_Spec_Expression (Expression (ASN), T);
      end if;
   end Check_Aspect_At_Freeze_Point;

   ---------------------------
   -- Check_Aspect_Too_Late --
   ---------------------------

   procedure Check_Aspect_Too_Late (N : Node_Id) is
      Typ  : constant Entity_Id := Entity (N);
      Expr : constant Node_Id   := Expression (N);

      function Find_Type_Reference
        (Typ : Entity_Id; Expr : Node_Id) return Boolean;
      --  Return True if a reference to type Typ is found in the expression
      --  Expr.

      -------------------------
      -- Find_Type_Reference --
      -------------------------

      function Find_Type_Reference
        (Typ : Entity_Id; Expr : Node_Id) return Boolean
      is
         function Find_Type (N : Node_Id) return Traverse_Result;
         --  Set Found to True if N refers to Typ

         ---------------
         -- Find_Type --
         ---------------

         function Find_Type (N : Node_Id) return Traverse_Result is
         begin
            if N = Typ
              or else (Nkind (N) in N_Identifier | N_Expanded_Name
                        and then Present (Entity (N))
                        and then Entity (N) = Typ)
            then
               return Abandon;
            else
               return OK;
            end if;
         end Find_Type;

         function Search_Type_Reference is new Traverse_Func (Find_Type);

      begin
         return Search_Type_Reference (Expr) = Abandon;
      end Find_Type_Reference;

      Parent_Type : Entity_Id;

      Save_In_Spec_Expression : constant Boolean := In_Spec_Expression;
      Save_Must_Not_Freeze    : constant Boolean := Must_Not_Freeze (Expr);

   begin
      --  Ensure Expr is analyzed so that e.g. all types are properly
      --  resolved for Find_Type_Reference. We preanalyze this expression
      --  (to avoid expansion), handle it as a spec expression (like default
      --  expression), disable freezing and skip resolution (to not fold
      --  type self-references, e.g. T'Last).

      In_Spec_Expression := True;
      Set_Must_Not_Freeze (Expr);

      Preanalyze (Expr);

      Set_Must_Not_Freeze (Expr, Save_Must_Not_Freeze);
      In_Spec_Expression := Save_In_Spec_Expression;

      --  A self-referential aspect is illegal if it forces freezing the
      --  entity before the corresponding aspect has been analyzed.

      if Find_Type_Reference (Typ, Expr) then
         Error_Msg_NE
           ("aspect specification causes premature freezing of&", N, Typ);
      end if;

      --  For representation aspects, check for case of untagged derived
      --  type whose parent either has primitive operations (pre Ada 2022),
      --  or is a by-reference type (RM 13.1(10)).
      --  Strictly speaking the check also applies to Ada 2012 but it is
      --  really too constraining for existing code already, so relax it.
      --  ??? Confirming aspects should be allowed here.

      if Is_Representation_Aspect (Get_Aspect_Id (N))
        and then Is_Derived_Type (Typ)
        and then not Is_Tagged_Type (Typ)
      then
         Parent_Type := Etype (Base_Type (Typ));

         if Ada_Version <= Ada_2012
           and then Has_Primitive_Operations (Parent_Type)
         then
            Error_Msg_N
              ("|representation aspect not permitted before Ada 2022: " &
               "use -gnat2022!", N);
            Error_Msg_NE
              ("\parent type & has primitive operations!", N, Parent_Type);

         elsif Is_By_Reference_Type (Parent_Type) then
            No_Type_Rep_Item (N);
            Error_Msg_NE
              ("\parent type & is a by-reference type!", N, Parent_Type);
         end if;
      end if;
   end Check_Aspect_Too_Late;

   -----------------------------------
   -- Check_Constant_Address_Clause --
   -----------------------------------

   procedure Check_Constant_Address_Clause
     (Expr  : Node_Id;
      U_Ent : Entity_Id)
   is
      procedure Check_At_Constant_Address (Nod : Node_Id);
      --  Checks that the given node N represents a name whose 'Address is
      --  constant (in the same sense as OK_Constant_Address_Clause, i.e. the
      --  address value is the same at the point of declaration of U_Ent and at
      --  the time of elaboration of the address clause.

      procedure Check_Expr_Constants (Nod : Node_Id);
      --  Checks that Nod meets the requirements for a constant address clause
      --  in the sense of the enclosing procedure.

      procedure Check_List_Constants (Lst : List_Id);
      --  Check that all elements of list Lst meet the requirements for a
      --  constant address clause in the sense of the enclosing procedure.

      -------------------------------
      -- Check_At_Constant_Address --
      -------------------------------

      procedure Check_At_Constant_Address (Nod : Node_Id) is
      begin
         if Is_Entity_Name (Nod) then
            if Present (Address_Clause (Entity ((Nod)))) then
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_NE
                 ("address for& cannot depend on another address clause! "
                  & "(RM 13.1(22))!", Nod, U_Ent);

            elsif In_Same_Source_Unit (Entity (Nod), U_Ent)
              and then Sloc (U_Ent) < Sloc (Entity (Nod))
            then
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_Node_2 := U_Ent;
               Error_Msg_NE
                 ("\& must be defined before & (RM 13.1(22))!",
                  Nod, Entity (Nod));
            end if;

         elsif Nkind (Nod) = N_Selected_Component then
            declare
               T : constant Entity_Id := Etype (Prefix (Nod));

            begin
               if (Is_Record_Type (T)
                    and then Has_Discriminants (T))
                 or else
                  (Is_Access_Type (T)
                    and then Is_Record_Type (Designated_Type (T))
                    and then Has_Discriminants (Designated_Type (T)))
               then
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);
                  Error_Msg_N
                    ("\address cannot depend on component of discriminated "
                     & "record (RM 13.1(22))!", Nod);
               else
                  Check_At_Constant_Address (Prefix (Nod));
               end if;
            end;

         elsif Nkind (Nod) = N_Indexed_Component then
            Check_At_Constant_Address (Prefix (Nod));
            Check_List_Constants (Expressions (Nod));

         else
            Check_Expr_Constants (Nod);
         end if;
      end Check_At_Constant_Address;

      --------------------------
      -- Check_Expr_Constants --
      --------------------------

      procedure Check_Expr_Constants (Nod : Node_Id) is
         Loc_U_Ent : constant Source_Ptr := Sloc (U_Ent);
         Ent       : Entity_Id           := Empty;

      begin
         if Nkind (Nod) in N_Has_Etype
           and then Etype (Nod) = Any_Type
         then
            return;
         end if;

         case Nkind (Nod) is
            when N_Empty
               | N_Error
            =>
               return;

            when N_Expanded_Name
               | N_Identifier
            =>
               Ent := Entity (Nod);

               --  We need to look at the original node if it is different
               --  from the node, since we may have rewritten things and
               --  substituted an identifier representing the rewrite.

               if Is_Rewrite_Substitution (Nod) then
                  Check_Expr_Constants (Original_Node (Nod));

                  --  If the node is an object declaration without initial
                  --  value, some code has been expanded, and the expression
                  --  is not constant, even if the constituents might be
                  --  acceptable, as in A'Address + offset.

                  if Ekind (Ent) = E_Variable
                    and then
                      Nkind (Declaration_Node (Ent)) = N_Object_Declaration
                    and then
                      No (Expression (Declaration_Node (Ent)))
                  then
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);

                  --  If entity is constant, it may be the result of expanding
                  --  a check. We must verify that its declaration appears
                  --  before the object in question, else we also reject the
                  --  address clause.

                  elsif Ekind (Ent) = E_Constant
                    and then In_Same_Source_Unit (Ent, U_Ent)
                    and then Sloc (Ent) > Loc_U_Ent
                  then
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);
                  end if;

                  return;
               end if;

               --  Otherwise look at the identifier and see if it is OK

               if Is_Named_Number (Ent) or else Is_Type (Ent) then
                  return;

               elsif Ekind (Ent) in E_Constant | E_In_Parameter then

                  --  This is the case where we must have Ent defined before
                  --  U_Ent. Clearly if they are in different units this
                  --  requirement is met since the unit containing Ent is
                  --  already processed.

                  if not In_Same_Source_Unit (Ent, U_Ent) then
                     return;

                  --  Otherwise location of Ent must be before the location
                  --  of U_Ent, that's what prior defined means.

                  elsif Sloc (Ent) < Loc_U_Ent then
                     return;

                  else
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);
                     Error_Msg_Node_2 := U_Ent;
                     Error_Msg_NE
                       ("\& must be defined before & (RM 13.1(22))!",
                        Nod, Ent);
                  end if;

               elsif Nkind (Original_Node (Nod)) = N_Function_Call then
                  Check_Expr_Constants (Original_Node (Nod));

               else
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);

                  if Comes_From_Source (Ent) then
                     Error_Msg_NE
                       ("\reference to variable& not allowed"
                          & " (RM 13.1(22))!", Nod, Ent);
                  else
                     Error_Msg_N
                       ("non-static expression not allowed"
                          & " (RM 13.1(22))!", Nod);
                  end if;
               end if;

            when N_Integer_Literal   =>

               --  If this is a rewritten unchecked conversion, in a system
               --  where Address is an integer type, always use the base type
               --  for a literal value. This is user-friendly and prevents
               --  order-of-elaboration issues with instances of unchecked
               --  conversion.

               if Nkind (Original_Node (Nod)) = N_Function_Call then
                  Set_Etype (Nod, Base_Type (Etype (Nod)));
               end if;

            when N_Character_Literal
               | N_Real_Literal
               | N_String_Literal
            =>
               return;

            when N_Range =>
               Check_Expr_Constants (Low_Bound (Nod));
               Check_Expr_Constants (High_Bound (Nod));

            when N_Explicit_Dereference =>
               Check_Expr_Constants (Prefix (Nod));

            when N_Indexed_Component =>
               Check_Expr_Constants (Prefix (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Slice =>
               Check_Expr_Constants (Prefix (Nod));
               Check_Expr_Constants (Discrete_Range (Nod));

            when N_Selected_Component =>
               Check_Expr_Constants (Prefix (Nod));

            when N_Attribute_Reference =>
               if Attribute_Name (Nod) in Name_Address
                                        | Name_Access
                                        | Name_Unchecked_Access
                                        | Name_Unrestricted_Access
               then
                  Check_At_Constant_Address (Prefix (Nod));

               --  Normally, System'To_Address will have been transformed into
               --  an Unchecked_Conversion, but in -gnatc mode, it will not,
               --  and we don't want to give an error, because the whole point
               --  of 'To_Address is that it is static.

               elsif Attribute_Name (Nod) = Name_To_Address then
                  pragma Assert (Operating_Mode = Check_Semantics);
                  null;

               else
                  Check_Expr_Constants (Prefix (Nod));
                  Check_List_Constants (Expressions (Nod));
               end if;

            when N_Aggregate =>
               Check_List_Constants (Component_Associations (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Component_Association =>
               Check_Expr_Constants (Expression (Nod));

            when N_Extension_Aggregate =>
               Check_Expr_Constants (Ancestor_Part (Nod));
               Check_List_Constants (Component_Associations (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Null =>
               return;

            when N_Binary_Op
               | N_Membership_Test
               | N_Short_Circuit
            =>
               Check_Expr_Constants (Left_Opnd (Nod));
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Unary_Op =>
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Allocator
               | N_Qualified_Expression
               | N_Type_Conversion
               | N_Unchecked_Type_Conversion
            =>
               Check_Expr_Constants (Expression (Nod));

            when N_Function_Call =>
               if not Is_Pure (Entity (Name (Nod))) then
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);

                  Error_Msg_NE
                    ("\function & is not pure (RM 13.1(22))!",
                     Nod, Entity (Name (Nod)));

               else
                  Check_List_Constants (Parameter_Associations (Nod));
               end if;

            when N_Parameter_Association =>
               Check_Expr_Constants (Explicit_Actual_Parameter (Nod));

            when others =>
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_NE
                 ("\must be constant defined before& (RM 13.1(22))!",
                  Nod, U_Ent);
         end case;
      end Check_Expr_Constants;

      --------------------------
      -- Check_List_Constants --
      --------------------------

      procedure Check_List_Constants (Lst : List_Id) is
         Nod1 : Node_Id;

      begin
         Nod1 := First (Lst);
         while Present (Nod1) loop
            Check_Expr_Constants (Nod1);
            Next (Nod1);
         end loop;
      end Check_List_Constants;

   --  Start of processing for Check_Constant_Address_Clause

   begin
      --  If rep_clauses are to be ignored, no need for legality checks. In
      --  particular, no need to pester user about rep clauses that violate the
      --  rule on constant addresses, given that these clauses will be removed
      --  by Freeze before they reach the back end. Similarly in CodePeer mode,
      --  we want to relax these checks.

      if not Ignore_Rep_Clauses and not CodePeer_Mode then
         Check_Expr_Constants (Expr);
      end if;
   end Check_Constant_Address_Clause;

   ---------------------------
   -- Check_Pool_Size_Clash --
   ---------------------------

   procedure Check_Pool_Size_Clash (Ent : Entity_Id; SP, SS : Node_Id) is
      Post : Node_Id;

   begin
      --  We need to find out which one came first. Note that in the case of
      --  aspects mixed with pragmas there are cases where the processing order
      --  is reversed, which is why we do the check here.

      if Sloc (SP) < Sloc (SS) then
         Error_Msg_Sloc := Sloc (SP);
         Post := SS;
         Error_Msg_NE ("Storage_Pool previously given for&#", Post, Ent);

      else
         Error_Msg_Sloc := Sloc (SS);
         Post := SP;
         Error_Msg_NE ("Storage_Size previously given for&#", Post, Ent);
      end if;

      Error_Msg_N
        ("\cannot have Storage_Size and Storage_Pool (RM 13.11(3))", Post);
   end Check_Pool_Size_Clash;

   ----------------------------------------
   -- Check_Record_Representation_Clause --
   ----------------------------------------

   procedure Check_Record_Representation_Clause (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Ident   : constant Node_Id    := Identifier (N);
      Rectype : Entity_Id;
      Fent    : Entity_Id;
      CC      : Node_Id;
      Fbit    : Uint := No_Uint;
      Lbit    : Uint := No_Uint;
      Hbit    : Uint := Uint_0;
      Comp    : Entity_Id;
      Pcomp   : Entity_Id;

      Max_Bit_So_Far : Uint;
      --  Records the maximum bit position so far. If all field positions
      --  are monotonically increasing, then we can skip the circuit for
      --  checking for overlap, since no overlap is possible.

      Tagged_Parent : Entity_Id := Empty;
      --  This is set in the case of an extension for which we have either a
      --  size clause or Is_Fully_Repped_Tagged_Type True (indicating that all
      --  components are positioned by record representation clauses) on the
      --  parent type. In this case we check for overlap between components of
      --  this tagged type and the parent component. Tagged_Parent will point
      --  to this parent type. For all other cases, Tagged_Parent is Empty.

      Parent_Last_Bit : Uint := No_Uint; -- init to avoid warning
      --  Relevant only if Tagged_Parent is set, Parent_Last_Bit indicates the
      --  last bit position for any field in the parent type. We only need to
      --  check overlap for fields starting below this point.

      Overlap_Check_Required : Boolean;
      --  Used to keep track of whether or not an overlap check is required

      Overlap_Detected : Boolean := False;
      --  Set True if an overlap is detected

      Ccount : Natural := 0;
      --  Number of component clauses in record rep clause

      procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id);
      --  Given two entities for record components or discriminants, checks
      --  if they have overlapping component clauses and issues errors if so.

      procedure Find_Component;
      --  Finds component entity corresponding to current component clause (in
      --  CC), and sets Comp to the entity, and Fbit/Lbit to the zero origin
      --  start/stop bits for the field. If there is no matching component or
      --  if the matching component does not have a component clause, then
      --  that's an error and Comp is set to Empty, but no error message is
      --  issued, since the message was already given. Comp is also set to
      --  Empty if the current "component clause" is in fact a pragma.

      procedure Record_Hole_Check
        (Rectype : Entity_Id; After_Last : out Uint; Warn : Boolean);
      --  Checks for gaps in the given Rectype. Compute After_Last, the bit
      --  number after the last component. Warn is True on the initial call,
      --  and warnings are given for gaps. For a type extension, this is called
      --  recursively to compute After_Last for the parent type; in this case
      --  Warn is False and the warnings are suppressed.

      procedure Component_Order_Check (Rectype : Entity_Id);
      --  Check that the order of component clauses agrees with the order of
      --  component declarations, and that the component clauses are given in
      --  increasing order of bit offset.

      -----------------------------
      -- Check_Component_Overlap --
      -----------------------------

      procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id) is
         CC1 : constant Node_Id := Component_Clause (C1_Ent);
         CC2 : constant Node_Id := Component_Clause (C2_Ent);

      begin
         if Present (CC1) and then Present (CC2) then

            --  Exclude odd case where we have two tag components in the same
            --  record, both at location zero. This seems a bit strange, but
            --  it seems to happen in some circumstances, perhaps on an error.

            if Chars (C1_Ent) = Name_uTag then
               return;
            end if;

            --  Here we check if the two fields overlap

            declare
               S1 : constant Uint := Component_Bit_Offset (C1_Ent);
               S2 : constant Uint := Component_Bit_Offset (C2_Ent);
               E1 : constant Uint := S1 + Esize (C1_Ent);
               E2 : constant Uint := S2 + Esize (C2_Ent);

            begin
               if E2 <= S1 or else E1 <= S2 then
                  null;
               else
                  Error_Msg_Node_2 := Component_Name (CC2);
                  Error_Msg_Sloc := Sloc (Error_Msg_Node_2);
                  Error_Msg_Node_1 := Component_Name (CC1);
                  Error_Msg_N
                    ("component& overlaps & #", Component_Name (CC1));
                  Overlap_Detected := True;
               end if;
            end;
         end if;
      end Check_Component_Overlap;

      ---------------------------
      -- Component_Order_Check --
      ---------------------------

      procedure Component_Order_Check (Rectype : Entity_Id) is
         Comp : Entity_Id := First_Component (Rectype);
         Clause : Node_Id := First (Component_Clauses (N));
         Prev_Bit_Offset : Uint := Uint_0;
         OOO : constant String :=
           "?_r?component clause out of order with respect to declaration";

      begin
         --  Step Comp through components and Clause through component clauses,
         --  skipping pragmas. We ignore discriminants and variant parts,
         --  because we get most of the benefit from the plain vanilla
         --  component cases, without the extra complexity. If we find a Comp
         --  and Clause that don't match, give a warning on both and quit. If
         --  we find two subsequent clauses out of order by bit layout, give
         --  warning and quit. On each iteration, Prev_Bit_Offset is the one
         --  from the previous iteration (or 0 to start).

         while Present (Comp) and then Present (Clause) loop
            if Nkind (Clause) = N_Component_Clause
              and then Ekind (Entity (Component_Name (Clause))) = E_Component
            then
               if Entity (Component_Name (Clause)) /= Comp then
                  Error_Msg_N (OOO, Comp);
                  Error_Msg_N (OOO, Clause);
                  exit;
               end if;

               if not Reverse_Bit_Order (Rectype)
                 and then not Reverse_Storage_Order (Rectype)
                 and then Component_Bit_Offset (Comp) < Prev_Bit_Offset
               then
                  Error_Msg_N ("?_r?memory layout out of order", Clause);
                  exit;
               end if;

               Prev_Bit_Offset := Component_Bit_Offset (Comp);
               Next_Component (Comp);
            end if;

            Next (Clause);
         end loop;
      end Component_Order_Check;

      --------------------
      -- Find_Component --
      --------------------

      procedure Find_Component is

         procedure Search_Component (R : Entity_Id);
         --  Search components of R for a match. If found, Comp is set

         ----------------------
         -- Search_Component --
         ----------------------

         procedure Search_Component (R : Entity_Id) is
         begin
            Comp := First_Component_Or_Discriminant (R);
            while Present (Comp) loop

               --  Ignore error of attribute name for component name (we
               --  already gave an error message for this, so no need to
               --  complain here)

               if Nkind (Component_Name (CC)) = N_Attribute_Reference then
                  null;
               else
                  exit when Chars (Comp) = Chars (Component_Name (CC));
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;
         end Search_Component;

      --  Start of processing for Find_Component

      begin
         --  Return with Comp set to Empty if we have a pragma

         if Nkind (CC) = N_Pragma then
            Comp := Empty;
            return;
         end if;

         --  Search current record for matching component

         Search_Component (Rectype);

         --  If not found, maybe component of base type discriminant that is
         --  absent from statically constrained first subtype.

         if No (Comp) then
            Search_Component (Base_Type (Rectype));
         end if;

         --  If no component, or the component does not reference the component
         --  clause in question, then there was some previous error for which
         --  we already gave a message, so just return with Comp Empty.

         if No (Comp) or else Component_Clause (Comp) /= CC then
            Check_Error_Detected;
            Comp := Empty;

         --  Normal case where we have a component clause

         else
            Fbit := Component_Bit_Offset (Comp);
            Lbit := Fbit + Esize (Comp) - 1;
         end if;
      end Find_Component;

      -----------------------
      -- Record_Hole_Check --
      -----------------------

      procedure Record_Hole_Check
        (Rectype : Entity_Id; After_Last : out Uint; Warn : Boolean)
      is
         Decl : constant Node_Id := Declaration_Node (Base_Type (Rectype));
         --  Full declaration of record type

         procedure Check_Component_List
           (DS   : List_Id;
            CL   : Node_Id;
            Sbit : Uint;
            Abit : out Uint);
         --  Check component list CL for holes. DS is a list of discriminant
         --  specifications to be included in the consideration of components.
         --  Sbit is the starting bit, which is zero if there are no preceding
         --  components (before a variant part, or a parent type, or a tag
         --  field). If there are preceding components, Sbit is the bit just
         --  after the last such component. Abit is set to the bit just after
         --  the last component of DS and CL.

         --------------------------
         -- Check_Component_List --
         --------------------------

         procedure Check_Component_List
           (DS   : List_Id;
            CL   : Node_Id;
            Sbit : Uint;
            Abit : out Uint)
         is
            Compl : constant Natural :=
              Natural (List_Length (Component_Items (CL)) + List_Length (DS));

            Comps : array (Natural range 0 .. Compl) of Entity_Id;
            --  Gather components (zero entry is for sort routine)

            Ncomps : Natural := 0;
            --  Number of entries stored in Comps (starting at Comps (1))

            Citem : Node_Id;
            --  One component item or discriminant specification

            Nbit  : Uint;
            --  Starting bit for next component

            CEnt  : Entity_Id;
            --  Component entity

            Variant : Node_Id;
            --  One variant

            function Lt (Op1, Op2 : Natural) return Boolean;
            --  Compare routine for Sort

            procedure Move (From : Natural; To : Natural);
            --  Move routine for Sort

            package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

            --------
            -- Lt --
            --------

            function Lt (Op1, Op2 : Natural) return Boolean is
               K1 : constant Boolean :=
                 Known_Component_Bit_Offset (Comps (Op1));
               K2 : constant Boolean :=
                 Known_Component_Bit_Offset (Comps (Op2));
               --  Record representation clauses can be incomplete, so the
               --  Component_Bit_Offsets can be unknown.
            begin
               if K1 then
                  if K2 then
                     return Component_Bit_Offset (Comps (Op1))
                          < Component_Bit_Offset (Comps (Op2));
                  else
                     return True;
                  end if;
               else
                  return K2;
               end if;
            end Lt;

            ----------
            -- Move --
            ----------

            procedure Move (From : Natural; To : Natural) is
            begin
               Comps (To) := Comps (From);
            end Move;

         --  Start of processing for Check_Component_List

         begin
            --  Gather discriminants into Comp

            Citem := First (DS);
            while Present (Citem) loop
               if Nkind (Citem) = N_Discriminant_Specification then
                  declare
                     Ent : constant Entity_Id :=
                             Defining_Identifier (Citem);
                  begin
                     if Ekind (Ent) = E_Discriminant then
                        Ncomps := Ncomps + 1;
                        Comps (Ncomps) := Ent;
                     end if;
                  end;
               end if;

               Next (Citem);
            end loop;

            --  Gather component entities into Comp

            Citem := First (Component_Items (CL));
            while Present (Citem) loop
               if Nkind (Citem) = N_Component_Declaration then
                  Ncomps := Ncomps + 1;
                  Comps (Ncomps) := Defining_Identifier (Citem);
               end if;

               Next (Citem);
            end loop;

            --  Now sort the component entities based on the first bit.
            --  Note we already know there are no overlapping components.

            Sorting.Sort (Ncomps);

            --  Loop through entries checking for holes

            Nbit := Sbit;
            for J in 1 .. Ncomps loop
               CEnt := Comps (J);
               pragma Annotate (CodePeer, Modified, CEnt);

               declare
                  CBO : constant Uint := Component_Bit_Offset (CEnt);

               begin
                  --  Skip components with unknown offsets

                  if Present (CBO) and then CBO >= 0 then
                     Error_Msg_Uint_1 := CBO - Nbit;

                     if Warn and then Error_Msg_Uint_1 > 0 then
                        Error_Msg_NE
                          ("?.h?^-bit gap before component&",
                           Component_Name (Component_Clause (CEnt)),
                           CEnt);
                     end if;

                     Nbit := CBO + Esize (CEnt);
                  end if;
               end;
            end loop;

            --  Set Abit to just after the last nonvariant component

            Abit := Nbit;

            --  Process variant parts recursively if present. Set Abit to the
            --  maximum for all variant parts.

            if Present (Variant_Part (CL)) then
               declare
                  Var_Start : constant Uint := Nbit;
               begin
                  Variant := First (Variants (Variant_Part (CL)));
                  while Present (Variant) loop
                     Check_Component_List
                       (No_List, Component_List (Variant), Var_Start, Nbit);
                     Next (Variant);
                     if Nbit > Abit then
                        Abit := Nbit;
                     end if;
                  end loop;
               end;
            end if;
         end Check_Component_List;

         --  Local variables

         Sbit : Uint;
         --  Starting bit for call to Check_Component_List. Zero for an
         --  untagged type. The size of the Tag for a nonderived tagged
         --  type. Parent size for a type extension.

         Record_Definition : Node_Id;
         --  Record_Definition containing Component_List to pass to
         --  Check_Component_List.

      --  Start of processing for Record_Hole_Check

      begin
         if Is_Tagged_Type (Rectype) then
            Sbit := UI_From_Int (System_Address_Size);
         else
            Sbit := Uint_0;
         end if;

         After_Last := Uint_0;

         if Nkind (Decl) = N_Full_Type_Declaration then
            Record_Definition := Type_Definition (Decl);

            --  If we have a record extension, set Sbit to point after the last
            --  component of the parent type, by calling Record_Hole_Check
            --  recursively.

            if Nkind (Record_Definition) = N_Derived_Type_Definition then
               Record_Definition := Record_Extension_Part (Record_Definition);
               Record_Hole_Check (Underlying_Type (Parent_Subtype (Rectype)),
                                  After_Last => Sbit, Warn => False);
            end if;

            if Nkind (Record_Definition) = N_Record_Definition then
               Check_Component_List
                 (Discriminant_Specifications (Decl),
                  Component_List (Record_Definition),
                  Sbit, After_Last);
            end if;
         end if;
      end Record_Hole_Check;

   --  Start of processing for Check_Record_Representation_Clause

   begin
      Find_Type (Ident);
      Rectype := Entity (Ident);

      if Rectype = Any_Type then
         return;
      end if;

      Rectype := Underlying_Type (Rectype);

      --  See if we have a fully repped derived tagged type

      declare
         PS : constant Entity_Id := Parent_Subtype (Rectype);

      begin
         if Present (PS) and then Known_Static_RM_Size (PS) then
            Tagged_Parent := PS;
            Parent_Last_Bit := RM_Size (PS) - 1;

         elsif Present (PS) and then Is_Fully_Repped_Tagged_Type (PS) then
            Tagged_Parent := PS;

            --  Find maximum bit of any component of the parent type

            Parent_Last_Bit := UI_From_Int (System_Address_Size - 1);
            Pcomp := First_Component_Or_Discriminant (Tagged_Parent);
            while Present (Pcomp) loop
               if Present (Component_Bit_Offset (Pcomp))
                 and then Known_Static_Esize (Pcomp)
               then
                  Parent_Last_Bit :=
                    UI_Max
                      (Parent_Last_Bit,
                       Component_Bit_Offset (Pcomp) + Esize (Pcomp) - 1);
               end if;

               Next_Component_Or_Discriminant (Pcomp);
            end loop;
         end if;
      end;

      --  All done if no component clauses

      CC := First (Component_Clauses (N));

      if No (CC) then
         return;
      end if;

      --  If a tag is present, then create a component clause that places it
      --  at the start of the record (otherwise gigi may place it after other
      --  fields that have rep clauses).

      Fent := First_Entity (Rectype);

      if Nkind (Fent) = N_Defining_Identifier
        and then Chars (Fent) = Name_uTag
      then
         Set_Component_Bit_Offset    (Fent, Uint_0);
         Set_Normalized_Position     (Fent, Uint_0);
         Set_Normalized_First_Bit    (Fent, Uint_0);
         Set_Esize                   (Fent, UI_From_Int (System_Address_Size));

         Set_Component_Clause (Fent,
           Make_Component_Clause (Loc,
             Component_Name => Make_Identifier (Loc, Name_uTag),

             Position  => Make_Integer_Literal (Loc, Uint_0),
             First_Bit => Make_Integer_Literal (Loc, Uint_0),
             Last_Bit  =>
               Make_Integer_Literal (Loc,
                 UI_From_Int (System_Address_Size - 1))));

         Ccount := Ccount + 1;
      end if;

      Max_Bit_So_Far := Uint_Minus_1;
      Overlap_Check_Required := False;

      --  Process the component clauses

      while Present (CC) loop
         Find_Component;

         if Present (Comp) then
            Ccount := Ccount + 1;

            --  We need a full overlap check if record positions non-monotonic

            if Fbit <= Max_Bit_So_Far then
               Overlap_Check_Required := True;
            end if;

            Max_Bit_So_Far := Lbit;

            --  Check bit position out of range of specified size

            if Has_Size_Clause (Rectype)
              and then RM_Size (Rectype) <= Lbit
            then
               Error_Msg_Uint_1 := RM_Size (Rectype);
               Error_Msg_Uint_2 := Lbit + 1;
               Error_Msg_N ("bit number out of range of specified "
                  & "size (expected ^, got ^)",
                  Last_Bit (CC));

               --  Check for overlap with tag or parent component

            else
               if Is_Tagged_Type (Rectype)
                 and then Fbit < System_Address_Size
               then
                  Error_Msg_NE
                    ("component overlaps tag field of&",
                     Component_Name (CC), Rectype);
                  Overlap_Detected := True;

               elsif Present (Tagged_Parent)
                 and then Fbit <= Parent_Last_Bit
               then
                  Error_Msg_NE
                    ("component overlaps parent field of&",
                     Component_Name (CC), Rectype);
                  Overlap_Detected := True;
               end if;

               if Hbit < Lbit then
                  Hbit := Lbit;
               end if;
            end if;
         end if;

         Next (CC);
      end loop;

      --  Now that we have processed all the component clauses, check for
      --  overlap. We have to leave this till last, since the components can
      --  appear in any arbitrary order in the representation clause.

      --  We do not need this check if all specified ranges were monotonic,
      --  as recorded by Overlap_Check_Required being False at this stage.

      --  This first section checks if there are any overlapping entries at
      --  all. It does this by sorting all entries and then seeing if there are
      --  any overlaps. If there are none, then that is decisive, but if there
      --  are overlaps, they may still be OK (they may result from fields in
      --  different variants).

      if Overlap_Check_Required then
         Overlap_Check1 : declare

            OC_Fbit : array (0 .. Ccount) of Uint;
            --  First-bit values for component clauses, the value is the offset
            --  of the first bit of the field from start of record. The zero
            --  entry is for use in sorting.

            OC_Lbit : array (0 .. Ccount) of Uint;
            --  Last-bit values for component clauses, the value is the offset
            --  of the last bit of the field from start of record. The zero
            --  entry is for use in sorting.

            OC_Count : Natural := 0;
            --  Count of entries in OC_Fbit and OC_Lbit

            function OC_Lt (Op1, Op2 : Natural) return Boolean;
            --  Compare routine for Sort

            procedure OC_Move (From : Natural; To : Natural);
            --  Move routine for Sort

            package Sorting is new GNAT.Heap_Sort_G (OC_Move, OC_Lt);

            -----------
            -- OC_Lt --
            -----------

            function OC_Lt (Op1, Op2 : Natural) return Boolean is
            begin
               return OC_Fbit (Op1) < OC_Fbit (Op2);
            end OC_Lt;

            -------------
            -- OC_Move --
            -------------

            procedure OC_Move (From : Natural; To : Natural) is
            begin
               OC_Fbit (To) := OC_Fbit (From);
               OC_Lbit (To) := OC_Lbit (From);
            end OC_Move;

         --  Start of processing for Overlap_Check

         begin
            CC := First (Component_Clauses (N));
            while Present (CC) loop

               --  Exclude component clause already marked in error

               if not Error_Posted (CC) then
                  Find_Component;

                  if Present (Comp) then
                     OC_Count := OC_Count + 1;
                     OC_Fbit (OC_Count) := Fbit;
                     OC_Lbit (OC_Count) := Lbit;
                  end if;
               end if;

               Next (CC);
            end loop;

            Sorting.Sort (OC_Count);

            Overlap_Check_Required := False;
            for J in 1 .. OC_Count - 1 loop
               if OC_Lbit (J) >= OC_Fbit (J + 1) then
                  Overlap_Check_Required := True;
                  exit;
               end if;
            end loop;
         end Overlap_Check1;
      end if;

      --  If Overlap_Check_Required is still True, then we have to do the full
      --  scale overlap check, since we have at least two fields that do
      --  overlap, and we need to know if that is OK since they are in
      --  different variant, or whether we have a definite problem.

      if Overlap_Check_Required then
         Overlap_Check2 : declare
            C1_Ent, C2_Ent : Entity_Id;
            --  Entities of components being checked for overlap

            Clist : Node_Id;
            --  Component_List node whose Component_Items are being checked

            Citem : Node_Id;
            --  Component declaration for component being checked

         begin
            C1_Ent := First_Entity (Base_Type (Rectype));

            --  Loop through all components in record. For each component check
            --  for overlap with any of the preceding elements on the component
            --  list containing the component and also, if the component is in
            --  a variant, check against components outside the case structure.
            --  This latter test is repeated recursively up the variant tree.

            Main_Component_Loop : while Present (C1_Ent) loop
               if Ekind (C1_Ent) not in E_Component | E_Discriminant then
                  goto Continue_Main_Component_Loop;
               end if;

               --  Skip overlap check if entity has no declaration node. This
               --  happens with discriminants in constrained derived types.
               --  Possibly we are missing some checks as a result, but that
               --  does not seem terribly serious.

               if No (Declaration_Node (C1_Ent)) then
                  goto Continue_Main_Component_Loop;
               end if;

               Clist := Parent (List_Containing (Declaration_Node (C1_Ent)));

               --  Loop through component lists that need checking. Check the
               --  current component list and all lists in variants above us.

               Component_List_Loop : loop

                  --  If derived type definition, go to full declaration
                  --  If at outer level, check discriminants if there are any.

                  if Nkind (Clist) = N_Derived_Type_Definition then
                     Clist := Parent (Clist);
                  end if;

                  --  Outer level of record definition, check discriminants
                  --  but be careful not to flag a non-stored discriminant
                  --  and the stored discriminant it renames as overlapping.

                  if Nkind (Clist) in N_Full_Type_Declaration
                                    | N_Private_Type_Declaration
                  then
                     if Has_Discriminants (Defining_Identifier (Clist)) then
                        C2_Ent :=
                          First_Discriminant (Defining_Identifier (Clist));
                        while Present (C2_Ent) loop
                           exit when
                             Original_Record_Component (C1_Ent) =
                               Original_Record_Component (C2_Ent);
                           Check_Component_Overlap (C1_Ent, C2_Ent);
                           Next_Discriminant (C2_Ent);
                        end loop;
                     end if;

                     --  Record extension case

                  elsif Nkind (Clist) = N_Derived_Type_Definition then
                     Clist := Empty;

                     --  Otherwise check one component list

                  else
                     Citem := First (Component_Items (Clist));
                     while Present (Citem) loop
                        if Nkind (Citem) = N_Component_Declaration then
                           C2_Ent := Defining_Identifier (Citem);
                           exit when C1_Ent = C2_Ent;
                           Check_Component_Overlap (C1_Ent, C2_Ent);
                        end if;

                        Next (Citem);
                     end loop;
                  end if;

                  --  Check for variants above us (the parent of the Clist can
                  --  be a variant, in which case its parent is a variant part,
                  --  and the parent of the variant part is a component list
                  --  whose components must all be checked against the current
                  --  component for overlap).

                  if Nkind (Parent (Clist)) = N_Variant then
                     Clist := Parent (Parent (Parent (Clist)));

                     --  Check for possible discriminant part in record, this
                     --  is treated essentially as another level in the
                     --  recursion. For this case the parent of the component
                     --  list is the record definition, and its parent is the
                     --  full type declaration containing the discriminant
                     --  specifications.

                  elsif Nkind (Parent (Clist)) = N_Record_Definition then
                     Clist := Parent (Parent ((Clist)));

                     --  If neither of these two cases, we are at the top of
                     --  the tree.

                  else
                     exit Component_List_Loop;
                  end if;
               end loop Component_List_Loop;

               <<Continue_Main_Component_Loop>>
               Next_Entity (C1_Ent);

            end loop Main_Component_Loop;
         end Overlap_Check2;
      end if;

      --  Skip the following warnings if overlap was detected; programmer
      --  should fix the errors first. Also skip the warnings for types in
      --  generics, because their representation information is not fully
      --  computed.

      if not Overlap_Detected and then not In_Generic_Scope (Rectype) then
         --  Check for record holes (gaps)

         if Warn_On_Record_Holes then
            declare
               Ignore : Uint;
            begin
               Record_Hole_Check (Rectype, After_Last => Ignore, Warn => True);
            end;
         end if;

         --  Check for out-of-order component clauses

         if Warn_On_Component_Order then
            Component_Order_Check (Rectype);
         end if;
      end if;

      --  For records that have component clauses for all components, and whose
      --  size is less than or equal to 32, and which can be fully packed, we
      --  need to know the size in the front end to activate possible packed
      --  array processing where the component type is a record.

      --  At this stage Hbit + 1 represents the first unused bit from all the
      --  component clauses processed, so if the component clauses are
      --  complete, then this is the length of the record.

      --  For records longer than System.Storage_Unit, and for those where not
      --  all components have component clauses, the back end determines the
      --  length (it may for example be appropriate to round up the size
      --  to some convenient boundary, based on alignment considerations, etc).

      if not Known_RM_Size (Rectype)
        and then Hbit + 1 <= 32
        and then not Strict_Alignment (Rectype)
      then

         --  Nothing to do if at least one component has no component clause

         Comp := First_Component_Or_Discriminant (Rectype);
         while Present (Comp) loop
            exit when No (Component_Clause (Comp));
            Next_Component_Or_Discriminant (Comp);
         end loop;

         --  If we fall out of loop, all components have component clauses
         --  and so we can set the size to the maximum value.

         if No (Comp) then
            Set_RM_Size (Rectype, Hbit + 1);
         end if;
      end if;
   end Check_Record_Representation_Clause;

   ----------------
   -- Check_Size --
   ----------------

   procedure Check_Size
     (N      : Node_Id;
      T      : Entity_Id;
      Siz    : Uint;
      Biased : out Boolean)
   is
      procedure Size_Too_Small_Error (Min_Siz : Uint);
      --  Emit an error concerning illegal size Siz. Min_Siz denotes the
      --  minimum size.

      --------------------------
      -- Size_Too_Small_Error --
      --------------------------

      procedure Size_Too_Small_Error (Min_Siz : Uint) is
      begin
         Error_Msg_Uint_1 := Min_Siz;
         Error_Msg_NE (Size_Too_Small_Message, N, T);
      end Size_Too_Small_Error;

      --  Local variables

      UT : constant Entity_Id := Underlying_Type (T);
      M  : Uint;

   --  Start of processing for Check_Size

   begin
      Biased := False;

      --  Reject patently improper size values

      if Is_Elementary_Type (T)
        and then Siz > Int'Last
      then
         Error_Msg_N ("Size value too large for elementary type", N);

         if Nkind (Original_Node (N)) = N_Op_Expon then
            Error_Msg_N
              ("\maybe '* was meant, rather than '*'*", Original_Node (N));
         end if;
      end if;

      --  Dismiss generic types

      if Is_Generic_Type (T)
           or else
         Is_Generic_Type (UT)
           or else
         Is_Generic_Type (Root_Type (UT))
      then
         return;

      --  Guard against previous errors

      elsif No (UT) or else UT = Any_Type then
         Check_Error_Detected;
         return;

      --  Check case of bit packed array

      elsif Is_Array_Type (UT)
        and then Known_Static_Component_Size (UT)
        and then Is_Bit_Packed_Array (UT)
      then
         declare
            Asiz : Uint;
            Indx : Node_Id;
            Ityp : Entity_Id;

         begin
            Asiz := Component_Size (UT);
            Indx := First_Index (UT);
            loop
               Ityp := Etype (Indx);

               --  If non-static bound, then we are not in the business of
               --  trying to check the length, and indeed an error will be
               --  issued elsewhere, since sizes of non-static array types
               --  cannot be set implicitly or explicitly.

               if not Is_OK_Static_Subtype (Ityp) then
                  return;
               end if;

               --  Otherwise accumulate next dimension

               Asiz := Asiz * (Expr_Value (Type_High_Bound (Ityp)) -
                               Expr_Value (Type_Low_Bound  (Ityp)) +
                               Uint_1);

               Next_Index (Indx);
               exit when No (Indx);
            end loop;

            if Asiz <= Siz then
               return;

            else
               Size_Too_Small_Error (Asiz);
            end if;
         end;

      --  All other composite types are ignored

      elsif Is_Composite_Type (UT) then
         return;

      --  For fixed-point types, don't check minimum if type is not frozen,
      --  since we don't know all the characteristics of the type that can
      --  affect the size (e.g. a specified small) till freeze time.

      elsif Is_Fixed_Point_Type (UT) and then not Is_Frozen (UT) then
         null;

      --  Cases for which a minimum check is required

      else
         --  Ignore if specified size is correct for the type

         if Known_Esize (UT) and then Siz = Esize (UT) then
            return;
         end if;

         --  Otherwise get minimum size

         M := UI_From_Int (Minimum_Size (UT));

         if Siz < M then

            --  Size is less than minimum size, but one possibility remains
            --  that we can manage with the new size if we bias the type.

            M := UI_From_Int (Minimum_Size (UT, Biased => True));

            if Siz < M then
               Size_Too_Small_Error (M);
            else
               Biased := True;
            end if;
         end if;
      end if;
   end Check_Size;

   --------------------------
   -- Freeze_Entity_Checks --
   --------------------------

   procedure Freeze_Entity_Checks (N : Node_Id) is
      procedure Hide_Non_Overridden_Subprograms (Typ : Entity_Id);
      --  Inspect the primitive operations of type Typ and hide all pairs of
      --  implicitly declared non-overridden non-fully conformant homographs
      --  (RM 8.3(12.3/2)).

      -------------------------------------
      -- Hide_Non_Overridden_Subprograms --
      -------------------------------------

      procedure Hide_Non_Overridden_Subprograms (Typ : Entity_Id) is
         procedure Hide_Matching_Homographs
           (Subp_Id    : Entity_Id;
            Start_Elmt : Elmt_Id);
         --  Inspect a list of primitive operations starting with Start_Elmt
         --  and find matching implicitly declared non-overridden non-fully
         --  conformant homographs of Subp_Id. If found, all matches along
         --  with Subp_Id are hidden from all visibility.

         function Is_Non_Overridden_Or_Null_Procedure
           (Subp_Id : Entity_Id) return Boolean;
         --  Determine whether subprogram Subp_Id is implicitly declared non-
         --  overridden subprogram or an implicitly declared null procedure.

         ------------------------------
         -- Hide_Matching_Homographs --
         ------------------------------

         procedure Hide_Matching_Homographs
           (Subp_Id    : Entity_Id;
            Start_Elmt : Elmt_Id)
         is
            Prim      : Entity_Id;
            Prim_Elmt : Elmt_Id;

         begin
            Prim_Elmt := Start_Elmt;
            while Present (Prim_Elmt) loop
               Prim := Node (Prim_Elmt);

               --  The current primitive is implicitly declared non-overridden
               --  non-fully conformant homograph of Subp_Id. Both subprograms
               --  must be hidden from visibility.

               if Chars (Prim) = Chars (Subp_Id)
                 and then Is_Non_Overridden_Or_Null_Procedure (Prim)
                 and then not Fully_Conformant (Prim, Subp_Id)
               then
                  Set_Is_Hidden_Non_Overridden_Subpgm (Prim);
                  Set_Is_Immediately_Visible          (Prim, False);
                  Set_Is_Potentially_Use_Visible      (Prim, False);

                  Set_Is_Hidden_Non_Overridden_Subpgm (Subp_Id);
                  Set_Is_Immediately_Visible          (Subp_Id, False);
                  Set_Is_Potentially_Use_Visible      (Subp_Id, False);
               end if;

               Next_Elmt (Prim_Elmt);
            end loop;
         end Hide_Matching_Homographs;

         -----------------------------------------
         -- Is_Non_Overridden_Or_Null_Procedure --
         -----------------------------------------

         function Is_Non_Overridden_Or_Null_Procedure
           (Subp_Id : Entity_Id) return Boolean
         is
            Alias_Id : Entity_Id;

         begin
            --  The subprogram is inherited (implicitly declared), it does not
            --  override and does not cover a primitive of an interface.

            if Ekind (Subp_Id) in E_Function | E_Procedure
              and then Present (Alias (Subp_Id))
              and then No (Interface_Alias (Subp_Id))
              and then No (Overridden_Operation (Subp_Id))
            then
               Alias_Id := Alias (Subp_Id);

               if Requires_Overriding (Alias_Id) then
                  return True;

               elsif Nkind (Parent (Alias_Id)) = N_Procedure_Specification
                 and then Null_Present (Parent (Alias_Id))
               then
                  return True;
               end if;
            end if;

            return False;
         end Is_Non_Overridden_Or_Null_Procedure;

         --  Local variables

         Prim_Ops  : constant Elist_Id := Direct_Primitive_Operations (Typ);
         Prim      : Entity_Id;
         Prim_Elmt : Elmt_Id;

      --  Start of processing for Hide_Non_Overridden_Subprograms

      begin
         --  Inspect the list of primitives looking for non-overridden
         --  subprograms.

         if Present (Prim_Ops) then
            Prim_Elmt := First_Elmt (Prim_Ops);
            while Present (Prim_Elmt) loop
               Prim := Node (Prim_Elmt);
               Next_Elmt (Prim_Elmt);

               if Is_Non_Overridden_Or_Null_Procedure (Prim) then
                  Hide_Matching_Homographs
                    (Subp_Id    => Prim,
                     Start_Elmt => Prim_Elmt);
               end if;
            end loop;
         end if;
      end Hide_Non_Overridden_Subprograms;

      --  Local variables

      E : constant Entity_Id := Entity (N);

      Nongeneric_Case : constant Boolean := Nkind (N) = N_Freeze_Entity;
      --  True in nongeneric case. Some of the processing here is skipped
      --  for the generic case since it is not needed. Basically in the
      --  generic case, we only need to do stuff that might generate error
      --  messages or warnings.

   --  Start of processing for Freeze_Entity_Checks

   begin
      --  Remember that we are processing a freezing entity. Required to
      --  ensure correct decoration of internal entities associated with
      --  interfaces (see New_Overloaded_Entity).

      Inside_Freezing_Actions := Inside_Freezing_Actions + 1;

      --  For tagged types covering interfaces add internal entities that link
      --  the primitives of the interfaces with the primitives that cover them.
      --  Note: These entities were originally generated only when generating
      --  code because their main purpose was to provide support to initialize
      --  the secondary dispatch tables. They are also used to locate
      --  primitives covering interfaces when processing generics (see
      --  Derive_Subprograms).

      --  This is not needed in the generic case

      if Ada_Version >= Ada_2005
        and then Nongeneric_Case
        and then Ekind (E) = E_Record_Type
        and then Is_Tagged_Type (E)
      then
         --  This would be a good common place to call the routine that checks
         --  overriding of interface primitives (and thus factorize calls to
         --  Check_Abstract_Overriding located at different contexts in the
         --  compiler). However, this is not possible because it causes
         --  spurious errors in case of late overriding.

         if Has_Interfaces (E)
           and then not Is_Interface (E)
         then
            Add_Internal_Interface_Entities (E);
         end if;

         --  For a derived tagged type, check strub mode compatibility of
         --  its primitives and whether inherited primitives might require
         --  a wrapper to handle class-wide conditions. For derived interface
         --  check strub mode compatibility of its primitives.

         if Is_Derived_Type (E)
           and then not In_Generic_Scope (E)
         then
            Check_Inherited_Conditions (E);
         end if;
      end if;

      --  After all forms of overriding have been resolved, a tagged type may
      --  be left with a set of implicitly declared and possibly erroneous
      --  abstract subprograms, null procedures and subprograms that require
      --  overriding. If this set contains fully conformant homographs, then
      --  one is chosen arbitrarily (already done during resolution), otherwise
      --  all remaining non-fully conformant homographs are hidden from
      --  visibility (RM 8.3(12.3/2)).

      if Is_Tagged_Type (E) then
         Hide_Non_Overridden_Subprograms (E);
      end if;

      --  Check CPP types

      if Ekind (E) = E_Record_Type
        and then Is_CPP_Class (E)
        and then Is_Tagged_Type (E)
        and then Tagged_Type_Expansion
      then
         if CPP_Num_Prims (E) = 0 then

            --  If the CPP type has user defined components then it must import
            --  primitives from C++. This is required because if the C++ class
            --  has no primitives then the C++ compiler does not added the _tag
            --  component to the type.

            if First_Entity (E) /= Last_Entity (E) then
               Error_Msg_N
                 ("'C'P'P type must import at least one primitive from C++??",
                  E);
            end if;
         end if;

         --  Check that all its primitives are abstract or imported from C++.
         --  Check also availability of the C++ constructor.

         declare
            Has_Constructors : constant Boolean := Has_CPP_Constructors (E);
            Elmt             : Elmt_Id;
            Error_Reported   : Boolean := False;
            Prim             : Node_Id;

         begin
            Elmt := First_Elmt (Primitive_Operations (E));
            while Present (Elmt) loop
               Prim := Node (Elmt);

               if Comes_From_Source (Prim) then
                  if Is_Abstract_Subprogram (Prim) then
                     null;

                  elsif not Is_Imported (Prim)
                    or else Convention (Prim) /= Convention_CPP
                  then
                     Error_Msg_N
                       ("primitives of 'C'P'P types must be imported from C++ "
                        & "or abstract??", Prim);

                  elsif not Has_Constructors
                     and then not Error_Reported
                  then
                     Error_Msg_Name_1 := Chars (E);
                     Error_Msg_N
                       ("??'C'P'P constructor required for type %", Prim);
                     Error_Reported := True;
                  end if;
               end if;

               Next_Elmt (Elmt);
            end loop;
         end;
      end if;

      --  Check Ada derivation of CPP type

      if Expander_Active              -- why? losing errors in -gnatc mode???
        and then Present (Etype (E))  -- defend against errors
        and then Tagged_Type_Expansion
        and then Ekind (E) = E_Record_Type
        and then Etype (E) /= E
        and then Is_CPP_Class (Etype (E))
        and then CPP_Num_Prims (Etype (E)) > 0
        and then not Is_CPP_Class (E)
        and then not Has_CPP_Constructors (Etype (E))
      then
         --  If the parent has C++ primitives but it has no constructor then
         --  check that all the primitives are overridden in this derivation;
         --  otherwise the constructor of the parent is needed to build the
         --  dispatch table.

         declare
            Elmt : Elmt_Id;
            Prim : Node_Id;

         begin
            Elmt := First_Elmt (Primitive_Operations (E));
            while Present (Elmt) loop
               Prim := Node (Elmt);

               if not Is_Abstract_Subprogram (Prim)
                 and then No (Interface_Alias (Prim))
                 and then Find_Dispatching_Type (Ultimate_Alias (Prim)) /= E
               then
                  Error_Msg_Name_1 := Chars (Etype (E));
                  Error_Msg_N
                    ("'C'P'P constructor required for parent type %", E);
                  exit;
               end if;

               Next_Elmt (Elmt);
            end loop;
         end;
      end if;

      Inside_Freezing_Actions := Inside_Freezing_Actions - 1;

      --  For a record type, deal with variant parts. This has to be delayed to
      --  this point, because of the issue of statically predicated subtypes,
      --  which we have to ensure are frozen before checking choices, since we
      --  need to have the static choice list set.

      if Is_Record_Type (E) then
         Check_Variant_Part : declare
            D  : constant Node_Id := Declaration_Node (E);
            T  : Node_Id;
            C  : Node_Id;
            VP : Node_Id;

            Others_Present : Boolean;
            pragma Warnings (Off, Others_Present);
            --  Indicates others present, not used in this case

            procedure Non_Static_Choice_Error (Choice : Node_Id);
            --  Error routine invoked by the generic instantiation below when
            --  the variant part has a non static choice.

            procedure Process_Declarations (Variant : Node_Id);
            --  Processes declarations associated with a variant. We analyzed
            --  the declarations earlier (in Sem_Ch3.Analyze_Variant_Part),
            --  but we still need the recursive call to Check_Choices for any
            --  nested variant to get its choices properly processed. This is
            --  also where we expand out the choices if expansion is active.

            package Variant_Choices_Processing is new
              Generic_Check_Choices
                (Process_Empty_Choice      => No_OP,
                 Process_Non_Static_Choice => Non_Static_Choice_Error,
                 Process_Associated_Node   => Process_Declarations);
            use Variant_Choices_Processing;

            -----------------------------
            -- Non_Static_Choice_Error --
            -----------------------------

            procedure Non_Static_Choice_Error (Choice : Node_Id) is
            begin
               Flag_Non_Static_Expr
                 ("choice given in variant part is not static!", Choice);
            end Non_Static_Choice_Error;

            --------------------------
            -- Process_Declarations --
            --------------------------

            procedure Process_Declarations (Variant : Node_Id) is
               CL : constant Node_Id := Component_List (Variant);
               VP : Node_Id;

            begin
               --  Check for static predicate present in this variant

               if Has_SP_Choice (Variant) then

                  --  Here we expand. You might expect to find this call in
                  --  Expand_N_Variant_Part, but that is called when we first
                  --  see the variant part, and we cannot do this expansion
                  --  earlier than the freeze point, since for statically
                  --  predicated subtypes, the predicate is not known till
                  --  the freeze point.

                  --  Furthermore, we do this expansion even if the expander
                  --  is not active, because other semantic processing, e.g.
                  --  for aggregates, requires the expanded list of choices.

                  --  If the expander is not active, then we can't just clobber
                  --  the list since it would invalidate the tree.
                  --  So we have to rewrite the variant part with a Rewrite
                  --  call that replaces it with a copy and clobber the copy.

                  if not Expander_Active then
                     declare
                        NewV : constant Node_Id := New_Copy (Variant);
                     begin
                        Set_Discrete_Choices
                          (NewV, New_Copy_List (Discrete_Choices (Variant)));
                        Rewrite (Variant, NewV);
                     end;
                  end if;

                  Expand_Static_Predicates_In_Choices (Variant);
               end if;

               --  We don't need to worry about the declarations in the variant
               --  (since they were analyzed by Analyze_Choices when we first
               --  encountered the variant), but we do need to take care of
               --  expansion of any nested variants.

               if not Null_Present (CL) then
                  VP := Variant_Part (CL);

                  if Present (VP) then
                     Check_Choices
                       (VP, Variants (VP), Etype (Name (VP)), Others_Present);
                  end if;
               end if;
            end Process_Declarations;

         --  Start of processing for Check_Variant_Part

         begin
            --  Find component list

            C := Empty;

            if Nkind (D) = N_Full_Type_Declaration then
               T := Type_Definition (D);

               if Nkind (T) = N_Record_Definition then
                  C := Component_List (T);

               elsif Nkind (T) = N_Derived_Type_Definition
                 and then Present (Record_Extension_Part (T))
               then
                  C := Component_List (Record_Extension_Part (T));
               end if;
            end if;

            --  Case of variant part present

            if Present (C) and then Present (Variant_Part (C)) then
               VP := Variant_Part (C);

               --  Check choices

               Check_Choices
                 (VP, Variants (VP), Etype (Name (VP)), Others_Present);

               --  If the last variant does not contain the Others choice,
               --  replace it with an N_Others_Choice node since Gigi always
               --  wants an Others. Note that we do not bother to call Analyze
               --  on the modified variant part, since its only effect would be
               --  to compute the Others_Discrete_Choices node laboriously, and
               --  of course we already know the list of choices corresponding
               --  to the others choice (it's the list we're replacing).

               --  We only want to do this if the expander is active, since
               --  we do not want to clobber the tree.

               if Expander_Active then
                  declare
                     Last_Var : constant Node_Id :=
                                     Last_Non_Pragma (Variants (VP));

                     Others_Node : Node_Id;

                  begin
                     if Nkind (First (Discrete_Choices (Last_Var))) /=
                                                            N_Others_Choice
                     then
                        Others_Node := Make_Others_Choice (Sloc (Last_Var));
                        Set_Others_Discrete_Choices
                          (Others_Node, Discrete_Choices (Last_Var));
                        Set_Discrete_Choices
                          (Last_Var, New_List (Others_Node));
                     end if;
                  end;
               end if;
            end if;
         end Check_Variant_Part;
      end if;

      --  If we have a type with predicates, build predicate function. This is
      --  not needed in the generic case, nor within e.g. TSS subprograms and
      --  other predefined primitives. For a derived type, ensure that the
      --  parent type is already frozen so that its predicate function has been
      --  constructed already. This is necessary if the parent is declared
      --  in a nested package and its own freeze point has not been reached.

      if Is_Type (E)
        and then Nongeneric_Case
        and then Has_Predicates (E)
        and then Predicate_Check_In_Scope (N)
      then
         declare
            Atyp : constant Entity_Id := Nearest_Ancestor (E);

         begin
            if Present (Atyp)
              and then Has_Predicates (Atyp)
              and then not Is_Frozen (Atyp)
            then
               Freeze_Before (N, Atyp);
            end if;
         end;

         --  Before we build a predicate function, ensure that discriminant
         --  checking functions are available. The predicate function might
         --  need to call these functions if the predicate references any
         --  components declared in a variant part.

         if Ekind (E) = E_Record_Type and then Has_Discriminants (E) then
            Build_Or_Copy_Discr_Checking_Funcs (Parent (E));
         end if;

         Build_Predicate_Function (E, N);
      end if;

      --  If type has delayed aspects, this is where we do the preanalysis at
      --  the freeze point, as part of the consistent visibility check. Note
      --  that this must be done after calling Build_Predicate_Function or
      --  Build_Invariant_Procedure since these subprograms fix occurrences of
      --  the subtype name in the saved expression so that they will not cause
      --  trouble in the preanalysis.

      --  This is also not needed in the generic case

      if Nongeneric_Case
        and then Has_Delayed_Aspects (E)
        and then Scope (E) = Current_Scope
      then
         declare
            Ritem : Node_Id;

         begin
            --  Look for aspect specification entries for this entity

            Ritem := First_Rep_Item (E);
            while Present (Ritem) loop
               if Nkind (Ritem) = N_Aspect_Specification
                 and then Entity (Ritem) = E
                 and then Is_Delayed_Aspect (Ritem)
               then
                  if Get_Aspect_Id (Ritem) in Aspect_CPU
                                            | Aspect_Dynamic_Predicate
                                            | Aspect_Ghost_Predicate
                                            | Aspect_Interrupt_Priority
                                            | Aspect_Predicate
                                            | Aspect_Static_Predicate
                                            | Aspect_Priority
                  then
                    --  Retrieve the visibility to components and discriminants
                    --  in order to properly analyze the aspects.

                     Push_Type (E);
                     Check_Aspect_At_Freeze_Point (Ritem);

                     --  In the case of predicate aspects, there will be
                     --  a corresponding Predicate pragma associated with
                     --  the aspect, and the expression of the pragma also
                     --  needs to be analyzed at this point, to ensure that
                     --  Save_Global_References will capture global refs in
                     --  expressions that occur in generic bodies, for proper
                     --  later resolution of the pragma in instantiations.

                     if Is_Type (E)
                       and then Inside_A_Generic
                       and then Has_Predicates (E)
                       and then Present (Aspect_Rep_Item (Ritem))
                     then
                        declare
                           Pragma_Args : constant List_Id :=
                             Pragma_Argument_Associations
                               (Aspect_Rep_Item (Ritem));
                           Pragma_Expr : constant Node_Id :=
                             Expression (Next (First (Pragma_Args)));
                        begin
                           if Present (Pragma_Expr) then
                              Analyze_And_Resolve
                                (Pragma_Expr, Standard_Boolean);
                           end if;
                        end;
                     end if;

                     Pop_Type (E);

                  else
                     Check_Aspect_At_Freeze_Point (Ritem);
                  end if;

               --  A pragma Predicate should be checked like one of the
               --  corresponding aspects, wrt possible misuse of ghost
               --  entities.

               elsif Nkind (Ritem) = N_Pragma
                 and then No (Corresponding_Aspect (Ritem))
                 and then
                   Get_Pragma_Id (Pragma_Name (Ritem)) = Pragma_Predicate
               then
                  --  Retrieve the visibility to components and discriminants
                  --  in order to properly analyze the pragma.

                  declare
                     Arg : constant Node_Id :=
                        Next (First (Pragma_Argument_Associations (Ritem)));
                  begin
                     Push_Type (E);
                     Preanalyze_Spec_Expression
                       (Expression (Arg), Standard_Boolean);
                     Pop_Type (E);
                  end;
               end if;

               Next_Rep_Item (Ritem);
            end loop;
         end;
      end if;

      if not In_Generic_Scope (E)
        and then Ekind (E) = E_Record_Type
        and then Is_Tagged_Type (E)
      then
         Process_Class_Conditions_At_Freeze_Point (E);
      end if;
   end Freeze_Entity_Checks;

   -------------------------
   -- Get_Alignment_Value --
   -------------------------

   function Get_Alignment_Value (Expr : Node_Id) return Uint is
      Align : constant Uint := Static_Integer (Expr);

   begin
      if No (Align) then
         return No_Uint;

      elsif Align < 0 then
         Error_Msg_N ("alignment value must be positive", Expr);
         return No_Uint;

      --  If Alignment is specified to be 0, we treat it the same as 1

      elsif Align = 0 then
         return Uint_1;

      else
         for J in Int range 0 .. 64 loop
            declare
               M : constant Uint := Uint_2 ** J;

            begin
               exit when M = Align;

               if M > Align then
                  Error_Msg_N ("alignment value must be power of 2", Expr);
                  return No_Uint;
               end if;
            end;
         end loop;

         return Align;
      end if;
   end Get_Alignment_Value;

   -----------------------------------
   -- Has_Compatible_Representation --
   -----------------------------------

   function Has_Compatible_Representation
     (Target_Typ, Operand_Typ : Entity_Id) return Boolean
   is
      --  The subtype-specific representation attributes (Size and Alignment)
      --  do not affect representation from the point of view of this function.

      T1 : constant Entity_Id := Implementation_Base_Type (Target_Typ);
      T2 : constant Entity_Id := Implementation_Base_Type (Operand_Typ);

   begin
      --  Return true immediately for the same base type

      if T1 = T2 then
         return True;

      --  Tagged types always have the same representation, because it is not
      --  possible to specify different representations for common fields.

      elsif Is_Tagged_Type (T1) then
         return True;

      --  Representations are definitely different if conventions differ

      elsif Convention (T1) /= Convention (T2) then
         return False;

      --  Representations are different if component alignments or scalar
      --  storage orders differ.

      elsif (Is_Record_Type (T1) or else Is_Array_Type (T1))
              and then
            (Is_Record_Type (T2) or else Is_Array_Type (T2))
        and then (Component_Alignment (T1) /= Component_Alignment (T2)
                   or else
                  Reverse_Storage_Order (T1) /= Reverse_Storage_Order (T2))
      then
         return False;
      end if;

      --  For arrays, the only real issue is component size. If we know the
      --  component size for both arrays, and it is the same, then that's
      --  good enough to know we don't have a change of representation.

      if Is_Array_Type (T1) then

         --  In a view conversion, if the target type is an array type having
         --  aliased components and the operand type is an array type having
         --  unaliased components, then a new object is created (4.6(58.3/4)).

         if Has_Aliased_Components (T1)
           and then not Has_Aliased_Components (T2)
         then
            return False;
         end if;

         if Known_Component_Size (T1)
           and then Known_Component_Size (T2)
           and then Component_Size (T1) = Component_Size (T2)
         then
            return True;
         end if;

      --  For records, representations are different if reordering differs

      elsif Is_Record_Type (T1)
        and then Is_Record_Type (T2)
        and then No_Reordering (T1) /= No_Reordering (T2)
      then
         return False;
      end if;

      --  Types definitely have same representation if neither has non-standard
      --  representation since default representations are always consistent.
      --  If only one has non-standard representation, and the other does not,
      --  then we consider that they do not have the same representation. They
      --  might, but there is no way of telling early enough.

      if Has_Non_Standard_Rep (T1) then
         if not Has_Non_Standard_Rep (T2) then
            return False;
         end if;
      else
         return not Has_Non_Standard_Rep (T2);
      end if;

      --  Here the two types both have non-standard representation, and we need
      --  to determine if they have the same non-standard representation.

      --  For arrays, we simply need to test if the component sizes are the
      --  same. Pragma Pack is reflected in modified component sizes, so this
      --  check also deals with pragma Pack.

      if Is_Array_Type (T1) then
         return Component_Size (T1) = Component_Size (T2);

      --  Case of record types

      elsif Is_Record_Type (T1) then

         --  Packed status must conform

         if Is_Packed (T1) /= Is_Packed (T2) then
            return False;

         --  If the operand type is derived from the target type and no clause
         --  has been given after the derivation, then the representations are
         --  the same since the derived type inherits that of the parent type.

         elsif Is_Derived_Type (T2)
           and then Etype (T2) = T1
           and then not Has_Record_Rep_Clause (T2)
         then
            return True;

         --  Otherwise we must check components. Typ2 maybe a constrained
         --  subtype with fewer components, so we compare the components
         --  of the base types.

         else
            Record_Case : declare
               CD1, CD2 : Entity_Id;

               function Same_Rep return Boolean;
               --  CD1 and CD2 are either components or discriminants. This
               --  function tests whether they have the same representation.

               --------------
               -- Same_Rep --
               --------------

               function Same_Rep return Boolean is
               begin
                  if No (Component_Clause (CD1)) then
                     return No (Component_Clause (CD2));
                  else
                     --  Note: at this point, component clauses have been
                     --  normalized to the default bit order, so that the
                     --  comparison of Component_Bit_Offsets is meaningful.

                     return
                        Present (Component_Clause (CD2))
                          and then
                        Component_Bit_Offset (CD1) = Component_Bit_Offset (CD2)
                          and then
                        Esize (CD1) = Esize (CD2);
                  end if;
               end Same_Rep;

            --  Start of processing for Record_Case

            begin
               if Has_Discriminants (T1) then

                  --  The number of discriminants may be different if the
                  --  derived type has fewer (constrained by values). The
                  --  invisible discriminants retain the representation of
                  --  the original, so the discrepancy does not per se
                  --  indicate a different representation.

                  CD1 := First_Discriminant (T1);
                  CD2 := First_Discriminant (T2);
                  while Present (CD1) and then Present (CD2) loop
                     if not Same_Rep then
                        return False;
                     else
                        Next_Discriminant (CD1);
                        Next_Discriminant (CD2);
                     end if;
                  end loop;
               end if;

               CD1 := First_Component (Underlying_Type (Base_Type (T1)));
               CD2 := First_Component (Underlying_Type (Base_Type (T2)));
               while Present (CD1) loop
                  if not Same_Rep then
                     return False;
                  else
                     Next_Component (CD1);
                     Next_Component (CD2);
                  end if;
               end loop;

               return True;
            end Record_Case;
         end if;

      --  For enumeration types, we must check each literal to see if the
      --  representation is the same. Note that we do not permit enumeration
      --  representation clauses for Character and Wide_Character, so these
      --  cases were already dealt with.

      elsif Is_Enumeration_Type (T1) then
         Enumeration_Case : declare
            L1, L2 : Entity_Id;

         begin
            L1 := First_Literal (T1);
            L2 := First_Literal (T2);
            while Present (L1) loop
               if Enumeration_Rep (L1) /= Enumeration_Rep (L2) then
                  return False;
               else
                  Next_Literal (L1);
                  Next_Literal (L2);
               end if;
            end loop;

            return True;
         end Enumeration_Case;

      --  Any other types have the same representation for these purposes

      else
         return True;
      end if;
   end Has_Compatible_Representation;

   -------------------------------------
   -- Inherit_Aspects_At_Freeze_Point --
   -------------------------------------

   procedure Inherit_Aspects_At_Freeze_Point (Typ : Entity_Id) is
      function Get_Inherited_Rep_Item
        (E   : Entity_Id;
         Nam : Name_Id) return Node_Id;
      --  Search the Rep_Item chain of entity E for an instance of a rep item
      --  (pragma, attribute definition clause, or aspect specification) whose
      --  name matches the given name Nam, and that has been inherited from its
      --  parent, i.e. that has not been directly specified for E . If one is
      --  found, it is returned, otherwise Empty is returned.

      function Get_Inherited_Rep_Item
        (E    : Entity_Id;
         Nam1 : Name_Id;
         Nam2 : Name_Id) return Node_Id;
      --  Search the Rep_Item chain of entity E for an instance of a rep item
      --  (pragma, attribute definition clause, or aspect specification) whose
      --  name matches one of the given names Nam1 or Nam2, and that has been
      --  inherited from its parent, i.e. that has not been directly specified
      --  for E . If one is found, it is returned, otherwise Empty is returned.

      function Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
        (Rep_Item : Node_Id) return Boolean;
      --  This routine checks if Rep_Item is either a pragma or an aspect
      --  specification node whose corresponding pragma (if any) is present in
      --  the Rep Item chain of the entity it has been specified to.

      ----------------------------
      -- Get_Inherited_Rep_Item --
      ----------------------------

      function Get_Inherited_Rep_Item
        (E   : Entity_Id;
         Nam : Name_Id) return Node_Id
      is
         Rep : constant Node_Id :=
           Get_Rep_Item (E, Nam, Check_Parents => True);
      begin
         if Present (Rep)
           and then not Has_Rep_Item (E, Nam, Check_Parents => False)
         then
            return Rep;
         else
            return Empty;
         end if;
      end Get_Inherited_Rep_Item;

      function Get_Inherited_Rep_Item
        (E    : Entity_Id;
         Nam1 : Name_Id;
         Nam2 : Name_Id) return Node_Id
      is
         Rep : constant Node_Id :=
           Get_Rep_Item (E, Nam1, Nam2, Check_Parents => True);
      begin
         if Present (Rep)
           and then not Has_Rep_Item (E, Nam1, Nam2, Check_Parents => False)
         then
            return Rep;
         else
            return Empty;
         end if;
      end Get_Inherited_Rep_Item;

      --------------------------------------------------
      -- Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item --
      --------------------------------------------------

      function Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
        (Rep_Item : Node_Id) return Boolean
      is
      begin
         return
           Nkind (Rep_Item) = N_Pragma
             or else
           Present_In_Rep_Item (Entity (Rep_Item), Aspect_Rep_Item (Rep_Item));
      end Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item;

      Rep : Node_Id;

   --  Start of processing for Inherit_Aspects_At_Freeze_Point

   begin
      --  A representation item is either subtype-specific (Size and Alignment
      --  clauses) or type-related (all others). Subtype-specific aspects may
      --  differ for different subtypes of the same type (RM 13.1.8).

      --  A derived type inherits each type-related representation aspect of
      --  its parent type that was directly specified before the declaration of
      --  the derived type (RM 13.1.15).

      --  A derived subtype inherits each subtype-specific representation
      --  aspect of its parent subtype that was directly specified before the
      --  declaration of the derived type (RM 13.1.15).

      --  The general processing involves inheriting a representation aspect
      --  from a parent type whenever the first rep item (aspect specification,
      --  attribute definition clause, pragma) corresponding to the given
      --  representation aspect in the rep item chain of Typ, if any, isn't
      --  directly specified to Typ but to one of its parents.

      --  In addition, Convention must be propagated from base type to subtype,
      --  because the subtype may have been declared on an incomplete view.

      if Nkind (Parent (Typ)) = N_Private_Extension_Declaration then
         return;
      end if;

      --  Ada_05/Ada_2005

      Rep := Get_Inherited_Rep_Item (Typ, Name_Ada_05, Name_Ada_2005);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Is_Ada_2005_Only (Typ);
      end if;

      --  Ada_12/Ada_2012

      Rep := Get_Inherited_Rep_Item (Typ, Name_Ada_12, Name_Ada_2012);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Is_Ada_2012_Only (Typ);
      end if;

      --  Ada_2022

      Rep := Get_Inherited_Rep_Item (Typ, Name_Ada_2022);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Is_Ada_2022_Only (Typ);
      end if;

      --  Atomic/Shared

      Rep := Get_Inherited_Rep_Item (Typ,  Name_Atomic, Name_Shared);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Is_Atomic (Typ);
         Set_Is_Volatile (Typ);
         Set_Treat_As_Volatile (Typ);
      end if;

      --  Convention

      if Is_Record_Type (Typ)
        and then Typ /= Base_Type (Typ) and then Is_Frozen (Base_Type (Typ))
      then
         Set_Convention (Typ, Convention (Base_Type (Typ)));
      end if;

      --  Default_Component_Value (for base types only)

      --  Note that we need to look into the first subtype because the base
      --  type may be the implicit base type built by the compiler for the
      --  declaration of a constrained subtype with the aspect.

      if Is_Array_Type (Typ) and then Is_Base_Type (Typ) then
         declare
            F_Typ : constant Entity_Id := First_Subtype (Typ);

            E : Entity_Id;

         begin
            Rep :=
              Get_Inherited_Rep_Item (F_Typ, Name_Default_Component_Value);
            if Present (Rep) then
               E := Entity (Rep);

               --  Deal with private types

               if Is_Private_Type (E) then
                  E := Full_View (E);
               end if;

               Set_Default_Aspect_Component_Value
                 (Typ, Default_Aspect_Component_Value (E));
               Set_Has_Default_Aspect (Typ);
            end if;
         end;
      end if;

      --  Default_Value (for base types only)

      --  Note that we need to look into the first subtype because the base
      --  type may be the implicit base type built by the compiler for the
      --  declaration of a constrained subtype with the aspect.

      if Is_Scalar_Type (Typ) and then Is_Base_Type (Typ) then
         declare
            F_Typ : constant Entity_Id := First_Subtype (Typ);

            E : Entity_Id;

         begin
            Rep := Get_Inherited_Rep_Item (F_Typ, Name_Default_Value);
            if Present (Rep) then
               E := Entity (Rep);

               --  Deal with private types

               if Is_Private_Type (E) then
                  E := Full_View (E);
               end if;

               Set_Default_Aspect_Value (Typ, Default_Aspect_Value (E));
               Set_Has_Default_Aspect (Typ);
            end if;
         end;
      end if;

      --  Discard_Names

      Rep := Get_Inherited_Rep_Item (Typ, Name_Discard_Names);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Discard_Names (Typ);
      end if;

      --  Volatile

      Rep := Get_Inherited_Rep_Item (Typ, Name_Volatile);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Is_Volatile (Typ);
         Set_Treat_As_Volatile (Typ);
      end if;

      --  Volatile_Full_Access and Full_Access_Only

      Rep := Get_Inherited_Rep_Item
               (Typ, Name_Volatile_Full_Access, Name_Full_Access_Only);
      if Present (Rep)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
      then
         Set_Is_Volatile_Full_Access (Typ);
         Set_Is_Volatile (Typ);
         Set_Treat_As_Volatile (Typ);
      end if;

      --  Inheritance for derived types only

      if Is_Derived_Type (Typ) then
         declare
            Bas_Typ     : constant Entity_Id := Base_Type (Typ);
            Imp_Bas_Typ : constant Entity_Id := Implementation_Base_Type (Typ);

         begin
            --  Atomic_Components

            Rep := Get_Inherited_Rep_Item (Typ, Name_Atomic_Components);
            if Present (Rep)
              and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
            then
               Set_Has_Atomic_Components (Imp_Bas_Typ);
            end if;

            --  Volatile_Components

            Rep := Get_Inherited_Rep_Item (Typ, Name_Volatile_Components);
            if Present (Rep)
              and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
            then
               Set_Has_Volatile_Components (Imp_Bas_Typ);
            end if;

            --  Universal_Aliasing

            Rep := Get_Inherited_Rep_Item (Typ, Name_Universal_Aliasing);
            if Present (Rep)
              and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item (Rep)
            then
               Set_Universal_Aliasing (Imp_Bas_Typ);
            end if;

            --  Bit_Order

            if Is_Record_Type (Typ) and then Typ = Bas_Typ then
               Rep := Get_Inherited_Rep_Item (Typ, Name_Bit_Order);
               if Present (Rep) then
                  Set_Reverse_Bit_Order (Bas_Typ,
                    Reverse_Bit_Order
                      (Implementation_Base_Type (Etype (Bas_Typ))));
               end if;
            end if;

            --  Scalar_Storage_Order

            if (Is_Record_Type (Typ) or else Is_Array_Type (Typ))
              and then Typ = Bas_Typ
            then
               --  For a type extension, always inherit from parent; otherwise
               --  inherit if no default applies. Note: we do not check for
               --  an explicit rep item on the parent type when inheriting,
               --  because the parent SSO may itself have been set by default.

               if not Has_Rep_Item (First_Subtype (Typ),
                                    Name_Scalar_Storage_Order, False)
                 and then (Is_Tagged_Type (Bas_Typ)
                            or else not (SSO_Set_Low_By_Default  (Bas_Typ)
                                           or else
                                         SSO_Set_High_By_Default (Bas_Typ)))
               then
                  Set_Reverse_Storage_Order (Bas_Typ,
                    Reverse_Storage_Order
                      (Implementation_Base_Type (Etype (Bas_Typ))));

                  --  Clear default SSO indications, since the inherited aspect
                  --  which was set explicitly overrides the default.

                  Set_SSO_Set_Low_By_Default  (Bas_Typ, False);
                  Set_SSO_Set_High_By_Default (Bas_Typ, False);
               end if;
            end if;

            --  Finalizable

            if Is_Record_Type (Typ) and then Typ = Bas_Typ then
               Rep := Get_Inherited_Rep_Item (Typ, Name_Finalizable);
               if Present (Rep) then
                  Propagate_Controlled_Flags (Typ, Etype (Bas_Typ));
               end if;
            end if;
         end;
      end if;
   end Inherit_Aspects_At_Freeze_Point;

   ---------------------------------
   -- Inherit_Delayed_Rep_Aspects --
   ---------------------------------

   procedure Inherit_Delayed_Rep_Aspects (Typ : Entity_Id) is
      A : Aspect_Id;
      N : Node_Id;
      P : Entity_Id;

   begin
      --  Find the first aspect that has been inherited

      N := First_Rep_Item (Typ);
      while Present (N) loop
         if Nkind (N) = N_Aspect_Specification then
            exit when Entity (N) /= Typ;
         end if;

         Next_Rep_Item (N);
      end loop;

      --  There must be one if we reach here

      pragma Assert (Present (N));
      P := Entity (N);

      --  Loop through delayed aspects for the parent type

      while Present (N) loop
         if Nkind (N) = N_Aspect_Specification then
            exit when Entity (N) /= P;

            if Is_Delayed_Aspect (N) then
               A := Get_Aspect_Id (N);

               --  Process delayed rep aspect. For Boolean attributes it is
               --  not possible to cancel an attribute once set (the attempt
               --  to use an aspect with xxx => False is an error) for a
               --  derived type. So for those cases, we do not have to check
               --  if a clause has been given for the derived type, since it
               --  is harmless to set it again if it is already set.

               case A is

                  --  Alignment

                  when Aspect_Alignment =>
                     if not Has_Alignment_Clause (Typ) then
                        Set_Alignment (Typ, Alignment (P));
                     end if;

                  --  Atomic

                  when Aspect_Atomic =>
                     if Is_Atomic (P) then
                        Set_Is_Atomic (Typ);
                     end if;

                  --  Atomic_Components

                  when Aspect_Atomic_Components =>
                     if Has_Atomic_Components (P) then
                        Set_Has_Atomic_Components (Base_Type (Typ));
                     end if;

                  --  Bit_Order

                  when Aspect_Bit_Order =>
                     if Is_Record_Type (Typ)
                       and then No (Get_Attribute_Definition_Clause
                                      (Typ, Attribute_Bit_Order))
                       and then Reverse_Bit_Order (P)
                     then
                        Set_Reverse_Bit_Order (Base_Type (Typ));
                     end if;

                  --  Component_Size

                  when Aspect_Component_Size =>
                     if Is_Array_Type (Typ)
                       and then not Has_Component_Size_Clause (Typ)
                     then
                        Set_Component_Size
                          (Base_Type (Typ), Component_Size (P));
                     end if;

                  --  Machine_Radix

                  when Aspect_Machine_Radix =>
                     if Is_Decimal_Fixed_Point_Type (Typ)
                       and then not Has_Machine_Radix_Clause (Typ)
                     then
                        Set_Machine_Radix_10 (Typ, Machine_Radix_10 (P));
                     end if;

                  --  Object_Size (also Size which also sets Object_Size)

                  when Aspect_Object_Size
                     | Aspect_Size
                  =>
                     if not Has_Size_Clause (Typ)
                       and then No (Object_Size_Clause (Typ))
                     then
                        Set_Esize (Typ, Esize (P));
                     end if;

                  --  Pack

                  when Aspect_Pack =>
                     if not Is_Packed (Typ) then
                        Set_Is_Packed (Base_Type (Typ));

                        if Is_Bit_Packed_Array (P) then
                           Set_Is_Bit_Packed_Array (Base_Type (Typ));
                           Set_Packed_Array_Impl_Type
                             (Typ, Packed_Array_Impl_Type (P));
                        end if;
                     end if;

                  --  Scalar_Storage_Order

                  when Aspect_Scalar_Storage_Order =>
                     if (Is_Record_Type (Typ) or else Is_Array_Type (Typ))
                       and then No (Get_Attribute_Definition_Clause
                                      (Typ, Attribute_Scalar_Storage_Order))
                       and then Reverse_Storage_Order (P)
                     then
                        Set_Reverse_Storage_Order (Base_Type (Typ));

                        --  Clear default SSO indications, since the aspect
                        --  overrides the default.

                        Set_SSO_Set_Low_By_Default  (Base_Type (Typ), False);
                        Set_SSO_Set_High_By_Default (Base_Type (Typ), False);
                     end if;

                  --  Small

                  when Aspect_Small =>
                     if Is_Fixed_Point_Type (Typ)
                       and then not Has_Small_Clause (Typ)
                     then
                        Set_Small_Value (Typ, Small_Value (P));
                     end if;

                  --  Storage_Size

                  when Aspect_Storage_Size =>
                     if (Is_Access_Type (Typ) or else Is_Task_Type (Typ))
                       and then not Has_Storage_Size_Clause (Typ)
                     then
                        Set_Storage_Size_Variable
                          (Base_Type (Typ), Storage_Size_Variable (P));
                     end if;

                  --  Value_Size

                  when Aspect_Value_Size =>

                     --  Value_Size is never inherited, it is either set by
                     --  default, or it is explicitly set for the derived
                     --  type. So nothing to do here.

                     null;

                  --  Volatile

                  when Aspect_Volatile =>
                     if Is_Volatile (P) then
                        Set_Is_Volatile (Typ);
                     end if;

                  --  Volatile_Full_Access (also Full_Access_Only)

                  when Aspect_Volatile_Full_Access
                     | Aspect_Full_Access_Only
                  =>
                     if Is_Volatile_Full_Access (P) then
                        Set_Is_Volatile_Full_Access (Typ);
                     end if;

                  --  Volatile_Components

                  when Aspect_Volatile_Components =>
                     if Has_Volatile_Components (P) then
                        Set_Has_Volatile_Components (Base_Type (Typ));
                     end if;

                  --  That should be all the Rep Aspects

                  when others =>
                     pragma Assert (Aspect_Delay (A) /= Rep_Aspect);
                     null;
               end case;
            end if;
         end if;

         Next_Rep_Item (N);
      end loop;
   end Inherit_Delayed_Rep_Aspects;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Address_Clause_Checks.Init;
      Unchecked_Conversions.Init;

      --  The following might be needed in the future for some non-GCC back
      --  ends:
      --  if AAMP_On_Target then
      --     Independence_Checks.Init;
      --  end if;
   end Initialize;

   ---------------------------
   -- Install_Discriminants --
   ---------------------------

   procedure Install_Discriminants (E : Entity_Id) is
      Disc : Entity_Id;
      Prev : Entity_Id;
   begin
      Disc := First_Discriminant (E);
      while Present (Disc) loop
         Prev := Current_Entity (Disc);
         Set_Current_Entity (Disc);
         Set_Is_Immediately_Visible (Disc);
         Set_Homonym (Disc, Prev);
         Next_Discriminant (Disc);
      end loop;
   end Install_Discriminants;

   -------------------------
   -- Is_Operational_Item --
   -------------------------

   function Is_Operational_Item (N : Node_Id) return Boolean is
   begin
      --  List of operational items is given in AARM 13.1(8.mm/1). It is
      --  clearly incomplete, as it does not include iterator aspects, among
      --  others.

      return Nkind (N) = N_Attribute_Definition_Clause
          and then
        Get_Attribute_Id (Chars (N)) in Attribute_Constant_Indexing
                                      | Attribute_External_Tag
                                      | Attribute_Default_Iterator
                                      | Attribute_Implicit_Dereference
                                      | Attribute_Input
                                      | Attribute_Iterable
                                      | Attribute_Iterator_Element
                                      | Attribute_Output
                                      | Attribute_Put_Image
                                      | Attribute_Read
                                      | Attribute_Variable_Indexing
                                      | Attribute_Write;
   end Is_Operational_Item;

   -------------------------
   -- Is_Predicate_Static --
   -------------------------

   --  Note: the basic legality of the expression has already been checked, so
   --  we don't need to worry about cases or ranges on strings for example.

   function Is_Predicate_Static
     (Expr : Node_Id;
      Nam  : Name_Id;
      Warn : Boolean := True) return Boolean
   is
      function All_Static_Case_Alternatives (L : List_Id) return Boolean;
      --  Given a list of case expression alternatives, returns True if all
      --  the alternatives are static (have all static choices, and a static
      --  expression).

      function Is_Type_Ref (N : Node_Id) return Boolean;
      pragma Inline (Is_Type_Ref);
      --  Returns True if N is a reference to the type for the predicate in the
      --  expression (i.e. if it is an identifier whose Chars field matches the
      --  Nam given in the call). N must not be parenthesized, if the type name
      --  appears in parens, this routine will return False.
      --
      --  The routine also returns True for function calls generated during the
      --  expansion of comparison operators on strings, which are intended to
      --  be legal in static predicates, and are converted into calls to array
      --  comparison routines in the body of the corresponding predicate
      --  function.

      ----------------------------------
      -- All_Static_Case_Alternatives --
      ----------------------------------

      function All_Static_Case_Alternatives (L : List_Id) return Boolean is
         N : Node_Id;

      begin
         N := First (L);
         while Present (N) loop
            if not (All_Static_Choices (Discrete_Choices (N))
                     and then Is_OK_Static_Expression (Expression (N)))
            then
               return False;
            end if;

            Next (N);
         end loop;

         return True;
      end All_Static_Case_Alternatives;

      -----------------
      -- Is_Type_Ref --
      -----------------

      function Is_Type_Ref (N : Node_Id) return Boolean is
      begin
         return (Nkind (N) = N_Identifier
                  and then Chars (N) = Nam
                  and then Paren_Count (N) = 0);
      end Is_Type_Ref;

      --  helper function for recursive calls
      function Is_Predicate_Static_Aux (Expr : Node_Id) return Boolean is
        (Is_Predicate_Static (Expr, Nam, Warn => False));

   --  Start of processing for Is_Predicate_Static

   begin
      --   Handle cases like
      --     subtype S is Integer with Static_Predicate =>
      --       (Some_Integer_Variable in Integer) and then (S /= 0);
      --   where the predicate (which should be rejected) might have been
      --   transformed into just "(S /= 0)", which would appear to be
      --   a predicate-static expression (and therefore legal).

      if Is_Rewrite_Substitution (Expr) then

         --  Emit warnings for predicates that are always True or always False
         --  and were not originally expressed as Boolean literals.

         return Result : constant Boolean :=
           Is_Predicate_Static_Aux (Original_Node (Expr))
         do
            if Result and then Warn and then Is_Entity_Name (Expr) then
               if Entity (Expr) = Standard_True then
                  Error_Msg_N ("predicate is redundant (always True)?", Expr);
               elsif Entity (Expr) = Standard_False then
                  Error_Msg_N
                    ("predicate is unsatisfiable (always False)?", Expr);
               end if;
            end if;
         end return;
      end if;

      --  Predicate_Static means one of the following holds. Numbers are the
      --  corresponding paragraph numbers in (RM 3.2.4(16-22)).

      --  16: A static expression

      if Is_OK_Static_Expression (Expr) then
         return True;

      --  17: A membership test whose simple_expression is the current
      --  instance, and whose membership_choice_list meets the requirements
      --  for a static membership test.

      elsif Nkind (Expr) in N_Membership_Test
        and then Is_Type_Ref (Left_Opnd (Expr))
        and then All_Membership_Choices_Static (Expr)
      then
         return True;

      --  18. A case_expression whose selecting_expression is the current
      --  instance, and whose dependent expressions are static expressions.

      elsif Nkind (Expr) = N_Case_Expression
        and then Is_Type_Ref (Expression (Expr))
        and then All_Static_Case_Alternatives (Alternatives (Expr))
      then
         return True;

      --  19. A call to a predefined equality or ordering operator, where one
      --  operand is the current instance, and the other is a static
      --  expression.

      --  Note: the RM is clearly wrong here in not excluding string types.
      --  Without this exclusion, we would allow expressions like X > "ABC"
      --  to be considered as predicate-static, which is clearly not intended,
      --  since the idea is for predicate-static to be a subset of normal
      --  static expressions (and "DEF" > "ABC" is not a static expression).

      --  However, we do allow internally generated (not from source) equality
      --  and inequality operations to be valid on strings (this helps deal
      --  with cases where we transform A in "ABC" to A = "ABC).

      --  In fact, it appears that the intent of the ARG is to extend static
      --  predicates to strings, and that the extension should probably apply
      --  to static expressions themselves. The code below accepts comparison
      --  operators that apply to static strings.

      elsif Nkind (Expr) in N_Op_Compare
        and then ((Is_Type_Ref (Left_Opnd (Expr))
                    and then Is_OK_Static_Expression (Right_Opnd (Expr)))
                  or else
                    (Is_Type_Ref (Right_Opnd (Expr))
                      and then Is_OK_Static_Expression (Left_Opnd (Expr))))
      then
         return True;

      --  20. A call to a predefined boolean logical operator, where each
      --  operand is predicate-static.

      elsif (Nkind (Expr) in N_Op_And | N_Op_Or | N_Op_Xor
              and then Is_Predicate_Static_Aux (Left_Opnd (Expr))
              and then Is_Predicate_Static_Aux (Right_Opnd (Expr)))
        or else
            (Nkind (Expr) = N_Op_Not
              and then Is_Predicate_Static_Aux (Right_Opnd (Expr)))
      then
         return True;

      --  21. A short-circuit control form where both operands are
      --  predicate-static.

      elsif Nkind (Expr) in N_Short_Circuit
        and then Is_Predicate_Static_Aux (Left_Opnd (Expr))
        and then Is_Predicate_Static_Aux (Right_Opnd (Expr))
      then
         return True;

      --  22. A parenthesized predicate-static expression. This does not
      --  require any special test, since we just ignore paren levels in
      --  all the cases above.

      --  One more test that is an implementation artifact caused by the fact
      --  that we are analyzing not the original expression, but the generated
      --  expression in the body of the predicate function. This can include
      --  references to inherited predicates, so that the expression we are
      --  processing looks like:

      --    xxPredicate (typ (Inns)) and then expression

      --  Where the call is to a Predicate function for an inherited predicate.
      --  We simply ignore such a call, which could be to either a dynamic or
      --  a static predicate. Note that if the parent predicate is dynamic then
      --  eventually this type will be marked as dynamic, but you are allowed
      --  to specify a static predicate for a subtype which is inheriting a
      --  dynamic predicate, so the static predicate validation here ignores
      --  the inherited predicate even if it is dynamic.
      --  In all cases, a static predicate can only apply to a scalar type.

      elsif Nkind (Expr) = N_Function_Call
        and then Is_Predicate_Function (Entity (Name (Expr)))
        and then Is_Scalar_Type (Etype (First_Entity (Entity (Name (Expr)))))
      then
         return True;

      --  That's an exhaustive list of tests, all other cases are not
      --  predicate-static, so we return False.

      else
         return False;
      end if;
   end Is_Predicate_Static;

   ----------------------
   -- Is_Static_Choice --
   ----------------------

   function Is_Static_Choice (N : Node_Id) return Boolean is
   begin
      return Nkind (N) = N_Others_Choice
        or else Is_OK_Static_Expression (N)
        or else (Is_Entity_Name (N) and then Is_Type (Entity (N))
                  and then Is_OK_Static_Subtype (Entity (N)))
        or else (Nkind (N) = N_Subtype_Indication
                  and then Is_OK_Static_Subtype (Entity (N)))
        or else (Nkind (N) = N_Range and then Is_OK_Static_Range (N));
   end Is_Static_Choice;

   ------------------------------
   -- Is_Type_Related_Rep_Item --
   ------------------------------

   function Is_Type_Related_Rep_Item (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Attribute_Definition_Clause =>
            --  See AARM 13.1(8.f-8.x) list items that end in "clause"
            --  ???: include any GNAT-defined attributes here?
            return Get_Attribute_Id (Chars (N)) in Attribute_Bit_Order
                                                 | Attribute_Component_Size
                                                 | Attribute_Machine_Radix
                                                 | Attribute_Storage_Pool
                                                 | Attribute_Stream_Size;

         when N_Pragma =>
            case Get_Pragma_Id (N) is
               --  See AARM 13.1(8.f-8.x) list items that start with "pragma"
               --  ???: include any GNAT-defined pragmas here?
               when Pragma_Pack
                  | Pragma_Import
                  | Pragma_Export
                  | Pragma_Convention
                  | Pragma_Atomic
                  | Pragma_Independent
                  | Pragma_Volatile
                  | Pragma_Atomic_Components
                  | Pragma_Independent_Components
                  | Pragma_Volatile_Components
                  | Pragma_Discard_Names
               =>
                  return True;
               when others =>
                  null;
            end case;

         when N_Enumeration_Representation_Clause
            | N_Record_Representation_Clause
         =>
            return True;

         when others =>
            null;
      end case;

      return False;
   end Is_Type_Related_Rep_Item;

   ---------------------
   -- Kill_Rep_Clause --
   ---------------------

   procedure Kill_Rep_Clause (N : Node_Id) is
   begin
      pragma Assert (Ignore_Rep_Clauses);

      --  Note: we use Replace rather than Rewrite, because we don't want
      --  tools to be able to use Original_Node to dig out the (undecorated)
      --  rep clause that is being replaced.

      Replace (N, Make_Null_Statement (Sloc (N)));

      --  The null statement must be marked as not coming from source. This is
      --  so that tools ignore it, and also the back end does not expect bogus
      --  "from source" null statements in weird places (e.g. in declarative
      --  regions where such null statements are not allowed).

      Set_Comes_From_Source (N, False);
   end Kill_Rep_Clause;

   ------------------
   -- Minimum_Size --
   ------------------

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False) return Int
   is
      Lo     : Uint    := No_Uint;
      Hi     : Uint    := No_Uint;
      LoR    : Ureal   := No_Ureal;
      HiR    : Ureal   := No_Ureal;
      LoSet  : Boolean := False;
      HiSet  : Boolean := False;
      B      : Uint;
      S      : Nat;
      Ancest : Entity_Id;
      R_Typ  : constant Entity_Id := Root_Type (T);

   begin
      --  Bad type

      if T = Any_Type then
         return Unknown_Minimum_Size;

      --  For generic types, just return unknown. There cannot be any
      --  legitimate need to know such a size, but this routine may be
      --  called with a generic type as part of normal processing.

      elsif Is_Generic_Type (R_Typ) or else R_Typ = Any_Type then
         return Unknown_Minimum_Size;

         --  Access types (cannot have size smaller than System.Address)

      elsif Is_Access_Type (T) then
         return System_Address_Size;

      --  Floating-point types

      elsif Is_Floating_Point_Type (T) then
         return UI_To_Int (Esize (R_Typ));

      --  Discrete types

      elsif Is_Discrete_Type (T) then

         --  The following loop is looking for the nearest compile time known
         --  bounds following the ancestor subtype chain. The idea is to find
         --  the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return Unknown_Minimum_Size;
            end if;

            if not LoSet then
               if Compile_Time_Known_Value (Type_Low_Bound (Ancest)) then
                  Lo := Expr_Rep_Value (Type_Low_Bound (Ancest));
                  LoSet := True;
                  exit when HiSet;
               end if;
            end if;

            if not HiSet then
               if Compile_Time_Known_Value (Type_High_Bound (Ancest)) then
                  Hi := Expr_Rep_Value (Type_High_Bound (Ancest));
                  HiSet := True;
                  exit when LoSet;
               end if;
            end if;

            Ancest := Ancestor_Subtype (Ancest);

            if No (Ancest) then
               Ancest := Base_Type (T);

               if Is_Generic_Type (Ancest) then
                  return Unknown_Minimum_Size;
               end if;
            end if;
         end loop;

      --  Fixed-point types. We can't simply use Expr_Value to get the
      --  Corresponding_Integer_Value values of the bounds, since these do not
      --  get set till the type is frozen, and this routine can be called
      --  before the type is frozen. Similarly the test for bounds being static
      --  needs to include the case where we have unanalyzed real literals for
      --  the same reason.

      elsif Is_Fixed_Point_Type (T) then

         --  The following loop is looking for the nearest compile time known
         --  bounds following the ancestor subtype chain. The idea is to find
         --  the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return Unknown_Minimum_Size;
            end if;

            --  Note: In the following two tests for LoSet and HiSet, it may
            --  seem redundant to test for N_Real_Literal here since normally
            --  one would assume that the test for the value being known at
            --  compile time includes this case. However, there is a glitch.
            --  If the real literal comes from folding a non-static expression,
            --  then we don't consider any non- static expression to be known
            --  at compile time if we are in configurable run time mode (needed
            --  in some cases to give a clearer definition of what is and what
            --  is not accepted). So the test is indeed needed. Without it, we
            --  would set neither Lo_Set nor Hi_Set and get an infinite loop.

            if not LoSet then
               if Nkind (Type_Low_Bound (Ancest)) = N_Real_Literal
                 or else Compile_Time_Known_Value (Type_Low_Bound (Ancest))
               then
                  LoR := Expr_Value_R (Type_Low_Bound (Ancest));
                  LoSet := True;
                  exit when HiSet;
               end if;
            end if;

            if not HiSet then
               if Nkind (Type_High_Bound (Ancest)) = N_Real_Literal
                 or else Compile_Time_Known_Value (Type_High_Bound (Ancest))
               then
                  HiR := Expr_Value_R (Type_High_Bound (Ancest));
                  HiSet := True;
                  exit when LoSet;
               end if;
            end if;

            Ancest := Ancestor_Subtype (Ancest);

            if No (Ancest) then
               Ancest := Base_Type (T);

               if Is_Generic_Type (Ancest) then
                  return Unknown_Minimum_Size;
               end if;
            end if;
         end loop;

         Lo := UR_To_Uint (LoR / Small_Value (T));
         Hi := UR_To_Uint (HiR / Small_Value (T));

      --  No other types allowed

      else
         raise Program_Error;
      end if;

      --  Fall through with Hi and Lo set. Deal with biased case

      if (Biased
           and then not Is_Fixed_Point_Type (T)
           and then not (Is_Enumeration_Type (T)
                          and then Has_Non_Standard_Rep (T)))
        or else Has_Biased_Representation (T)
      then
         Hi := Hi - Lo;
         Lo := Uint_0;
      end if;

      --  Null range case, size is always zero. We only do this in the discrete
      --  type case, since that's the odd case that came up. Probably we should
      --  also do this in the fixed-point case, but doing so causes peculiar
      --  gigi failures, and it is not worth worrying about this incredibly
      --  marginal case (explicit null-range fixed-point type declarations).

      if Lo > Hi and then Is_Discrete_Type (T) then
         S := 0;

      --  Signed case. Note that we consider types like range 1 .. -1 to be
      --  signed for the purpose of computing the size, since the bounds have
      --  to be accommodated in the base type.

      elsif Lo < 0 or else Hi < 0 then
         S := 1;
         B := Uint_1;

         --  S = size, B = 2 ** (size - 1) (can accommodate -B .. +(B - 1))
         --  Note that we accommodate the case where the bounds cross. This
         --  can happen either because of the way the bounds are declared
         --  or because of the algorithm in Freeze_Fixed_Point_Type.

         while Lo < -B
           or else Hi < -B
           or else Lo >= B
           or else Hi >= B
         loop
            B := Uint_2 ** S;
            S := S + 1;
         end loop;

      --  Unsigned case

      else
         --  If both bounds are positive, make sure that both are represen-
         --  table in the case where the bounds are crossed. This can happen
         --  either because of the way the bounds are declared, or because of
         --  the algorithm in Freeze_Fixed_Point_Type.

         if Lo > Hi then
            Hi := Lo;
         end if;

         --  S = size, (can accommodate 0 .. (2**size - 1))

         S := 0;
         while Hi >= Uint_2 ** S loop
            S := S + 1;
         end loop;
      end if;

      return S;
   end Minimum_Size;

   ------------------------------
   -- New_Put_Image_Subprogram --
   ------------------------------

   procedure New_Put_Image_Subprogram
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Sname     : constant Name_Id    :=
        Make_TSS_Name (Base_Type (Ent), TSS_Put_Image);
      Subp_Id   : Entity_Id;
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

      Defer_Declaration : constant Boolean :=
                            Is_Tagged_Type (Ent) or else Is_Private_Type (Ent);
      --  For a tagged type, there is a declaration at the freeze point, and
      --  we must generate only a completion of this declaration. We do the
      --  same for private types, because the full view might be tagged.
      --  Otherwise we generate a declaration at the point of the attribute
      --  definition clause. If the attribute definition comes from an aspect
      --  specification the declaration is part of the freeze actions of the
      --  type.

      function Build_Spec return Node_Id;
      --  Used for declaration and renaming declaration, so that this is
      --  treated as a renaming_as_body.

      ----------------
      -- Build_Spec --
      ----------------

      function Build_Spec return Node_Id is
         Formals : List_Id;
         Spec    : Node_Id;
         T_Ref   : constant Node_Id := New_Occurrence_Of (Etyp, Loc);

      begin
         Subp_Id := Make_Defining_Identifier (Loc, Sname);

         --  S : Root_Buffer_Type'Class

         Formals := New_List (
                      Make_Parameter_Specification (Loc,
                        Defining_Identifier =>
                          Make_Defining_Identifier (Loc, Name_S),
                        In_Present          => True,
                        Out_Present         => True,
                        Parameter_Type      =>
                          New_Occurrence_Of (Etype (F), Loc)));

         --  V : T

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Parameter_Type      => T_Ref));

         Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Subp_Id,
             Parameter_Specifications => Formals);

         return Spec;
      end Build_Spec;

   --  Start of processing for New_Put_Image_Subprogram

   begin
      F := First_Formal (Subp);

      Etyp := Etype (Next_Formal (F));

      --  Prepare subprogram declaration and insert it as an action on the
      --  clause node. The visibility for this entity is used to test for
      --  visibility of the attribute definition clause (in the sense of
      --  8.3(23) as amended by AI-195).

      if not Defer_Declaration then
         Subp_Decl :=
           Make_Subprogram_Declaration (Loc,
             Specification => Build_Spec);

      --  For a tagged type, there is always a visible declaration for the
      --  Put_Image TSS (it is a predefined primitive operation), and the
      --  completion of this declaration occurs at the freeze point, which is
      --  not always visible at places where the attribute definition clause is
      --  visible. So, we create a dummy entity here for the purpose of
      --  tracking the visibility of the attribute definition clause itself.

      else
         Subp_Id :=
           Make_Defining_Identifier (Loc, New_External_Name (Sname, 'V'));
         Subp_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Id,
             Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      end if;

      if not Defer_Declaration
        and then From_Aspect_Specification (N)
        and then Has_Delayed_Freeze (Ent)
      then
         Append_Freeze_Action (Ent, Subp_Decl);

         --  We may freeze Subp_Id immediately since Ent has just been frozen.
         --  This will help to shield us from potential late freezing issues.

         Set_Is_Frozen (Subp_Id);

      else
         Insert_Action (N, Subp_Decl);
         Set_Entity (N, Subp_Id);
      end if;

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => Build_Spec,
          Name          => New_Occurrence_Of (Subp, Loc));

      if Defer_Declaration then
         Set_TSS (Base_Type (Ent), Subp_Id);

      else
         if From_Aspect_Specification (N) then
            Append_Freeze_Action (Ent, Subp_Decl);
         else
            Insert_Action (N, Subp_Decl);
         end if;

         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;
   end New_Put_Image_Subprogram;

   ---------------------------
   -- New_Stream_Subprogram --
   ---------------------------

   procedure New_Stream_Subprogram
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id;
      Nam   : TSS_Name_Type)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Sname     : constant Name_Id    := Make_TSS_Name (Base_Type (Ent), Nam);
      Subp_Id   : Entity_Id;
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

      Defer_Declaration : constant Boolean :=
                            Is_Tagged_Type (Ent) or else Is_Private_Type (Ent);
      --  For a tagged type, there is a declaration for each stream attribute
      --  at the freeze point, and we must generate only a completion of this
      --  declaration. We do the same for private types, because the full view
      --  might be tagged. Otherwise we generate a declaration at the point of
      --  the attribute definition clause. If the attribute definition comes
      --  from an aspect specification the declaration is part of the freeze
      --  actions of the type.

      function Build_Spec return Node_Id;
      --  Used for declaration and renaming declaration, so that this is
      --  treated as a renaming_as_body.

      ----------------
      -- Build_Spec --
      ----------------

      function Build_Spec return Node_Id is
         Out_P   : constant Boolean := (Nam = TSS_Stream_Read);
         Formals : List_Id;
         Spec    : Node_Id;
         T_Ref   : constant Node_Id := New_Occurrence_Of (Etyp, Loc);

      begin
         Subp_Id := Make_Defining_Identifier (Loc, Sname);

         --  S : access Root_Stream_Type'Class

         Formals := New_List (
                      Make_Parameter_Specification (Loc,
                        Defining_Identifier =>
                          Make_Defining_Identifier (Loc, Name_S),
                        Parameter_Type =>
                          Make_Access_Definition (Loc,
                            Subtype_Mark =>
                              New_Occurrence_Of (
                                Designated_Type (Etype (F)), Loc))));

         if Nam = TSS_Stream_Input then
            Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name       => Subp_Id,
                Parameter_Specifications => Formals,
                Result_Definition        => T_Ref);
         else
            --  V : [out] T

            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
                Out_Present         => Out_P,
                Parameter_Type      => T_Ref));

            Spec :=
              Make_Procedure_Specification (Loc,
                Defining_Unit_Name       => Subp_Id,
                Parameter_Specifications => Formals);
         end if;

         return Spec;
      end Build_Spec;

   --  Start of processing for New_Stream_Subprogram

   begin
      F := First_Formal (Subp);

      if Ekind (Subp) = E_Procedure then
         Etyp := Etype (Next_Formal (F));
      else
         Etyp := Etype (Subp);
      end if;

      --  Prepare subprogram declaration and insert it as an action on the
      --  clause node. The visibility for this entity is used to test for
      --  visibility of the attribute definition clause (in the sense of
      --  8.3(23) as amended by AI-195).

      if not Defer_Declaration then
         Subp_Decl :=
           Make_Subprogram_Declaration (Loc,
             Specification => Build_Spec);

      --  For a tagged type, there is always a visible declaration for each
      --  stream TSS (it is a predefined primitive operation), and the
      --  completion of this declaration occurs at the freeze point, which is
      --  not always visible at places where the attribute definition clause is
      --  visible. So, we create a dummy entity here for the purpose of
      --  tracking the visibility of the attribute definition clause itself.

      else
         Subp_Id :=
           Make_Defining_Identifier (Loc, New_External_Name (Sname, 'V'));
         Subp_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Id,
             Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      end if;

      if not Defer_Declaration
        and then From_Aspect_Specification (N)
        and then Has_Delayed_Freeze (Ent)
      then
         Append_Freeze_Action (Ent, Subp_Decl);

      else
         Insert_Action (N, Subp_Decl);
         Set_Entity (N, Subp_Id);
      end if;

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => Build_Spec,
          Name          => New_Occurrence_Of (Subp, Loc));

      if Defer_Declaration then
         Set_TSS (Base_Type (Ent), Subp_Id);

      else
         if From_Aspect_Specification (N) then
            Append_Freeze_Action (Ent, Subp_Decl);
         else
            Insert_Action (N, Subp_Decl);
         end if;

         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;
   end New_Stream_Subprogram;

   ----------------------
   -- No_Type_Rep_Item --
   ----------------------

   procedure No_Type_Rep_Item (N : Node_Id) is
   begin
      Error_Msg_N ("|type-related representation item not permitted!", N);
   end No_Type_Rep_Item;

   --------------
   -- Pop_Type --
   --------------

   procedure Pop_Type (E : Entity_Id) is
   begin
      if Ekind (E) = E_Record_Type and then E = Current_Scope then
         End_Scope;

      elsif Is_Type (E)
        and then Has_Discriminants (E)
        and then Nkind (Parent (E)) /= N_Subtype_Declaration
      then
         Uninstall_Discriminants (E);
         Pop_Scope;
      end if;
   end Pop_Type;

   ---------------
   -- Push_Type --
   ---------------

   procedure Push_Type (E : Entity_Id) is
      Comp : Entity_Id;

   begin
      if Ekind (E) = E_Record_Type then
         Push_Scope (E);

         Comp := First_Component (E);
         while Present (Comp) loop
            Install_Entity (Comp);
            Next_Component (Comp);
         end loop;

         if Has_Discriminants (E) then
            Install_Discriminants (E);
         end if;

      elsif Is_Type (E)
        and then Has_Discriminants (E)
        and then Nkind (Parent (E)) /= N_Subtype_Declaration
      then
         Push_Scope (E);
         Install_Discriminants (E);
      end if;
   end Push_Type;

   -----------------------------------
   -- Register_Address_Clause_Check --
   -----------------------------------

   procedure Register_Address_Clause_Check
     (N   : Node_Id;
      X   : Entity_Id;
      A   : Uint;
      Y   : Entity_Id;
      Off : Boolean)
   is
      ACS : constant Boolean := Scope_Suppress.Suppress (Alignment_Check);
   begin
      Address_Clause_Checks.Append ((N, X, A, Y, Off, ACS));
   end Register_Address_Clause_Check;

   ------------------------
   -- Rep_Item_Too_Early --
   ------------------------

   function Rep_Item_Too_Early (T : Entity_Id; N : Node_Id) return Boolean is
      function Has_Generic_Parent (E : Entity_Id) return Boolean;
      --  Return True if R or any ancestor is a generic type

      ------------------------
      -- Has_Generic_Parent --
      ------------------------

      function Has_Generic_Parent (E : Entity_Id) return Boolean is
         Ancestor_Type : Entity_Id := Etype (E);

      begin
         if Is_Generic_Type (E) then
            return True;
         end if;

         while Present (Ancestor_Type)
           and then not Is_Generic_Type (Ancestor_Type)
           and then Etype (Ancestor_Type) /= Ancestor_Type
         loop
            Ancestor_Type := Etype (Ancestor_Type);
         end loop;

         return
           Present (Ancestor_Type) and then Is_Generic_Type (Ancestor_Type);
      end Has_Generic_Parent;

   --  Start of processing for Rep_Item_Too_Early

   begin
      --  Cannot apply non-operational rep items to generic types

      if Is_Operational_Item (N) then
         return False;

      elsif Is_Type (T)
        and then Has_Generic_Parent (T)
        and then (Nkind (N) /= N_Pragma
                   or else Get_Pragma_Id (N) /= Pragma_Convention)
      then
         if Ada_Version < Ada_2022 then
            Error_Msg_N
              ("representation item not allowed for generic type", N);
            return True;
         else
            return False;
         end if;
      end if;

      --  Otherwise check for incomplete type

      if Is_Incomplete_Or_Private_Type (T)
        and then No (Underlying_Type (T))
        and then
          (Nkind (N) /= N_Pragma
            or else Get_Pragma_Id (N) /= Pragma_Import)
      then
         Error_Msg_N
           ("representation item must be after full type declaration", N);
         return True;

      --  If the type has incomplete components, a representation clause is
      --  illegal but stream attributes and Convention pragmas are correct.

      elsif Has_Private_Component (T) then
         if Nkind (N) = N_Pragma then
            return False;

         else
            Error_Msg_N
              ("representation item must appear after type is fully defined",
                N);
            return True;
         end if;
      else
         return False;
      end if;
   end Rep_Item_Too_Early;

   -----------------------
   -- Rep_Item_Too_Late --
   -----------------------

   function Rep_Item_Too_Late
     (T     : Entity_Id;
      N     : Node_Id;
      FOnly : Boolean := False) return Boolean
   is
      procedure Too_Late;
      --  Output message for an aspect being specified too late

      --  Note that neither of the above errors is considered a serious one,
      --  since the effect is simply that we ignore the representation clause
      --  in these cases.
      --  Is this really true? In any case if we make this change we must
      --  document the requirement in the spec of Rep_Item_Too_Late that
      --  if True is returned, then the rep item must be completely ignored???

      --------------
      -- Too_Late --
      --------------

      procedure Too_Late is
         S : Entity_Id;
      begin
         --  Other compilers seem more relaxed about rep items appearing too
         --  late. Since analysis tools typically don't care about rep items
         --  anyway, no reason to be too strict about this.

         if not Relaxed_RM_Semantics then
            if Debug_Flag_Underscore_DD then

               S := First_Subtype (T);
               if Present (Freeze_Node (S)) then
                  Record_Representation_Too_Late_Error
                    (Rep    => N,
                     Freeze => Freeze_Node (S),
                     Def    => S);
               else
                  Error_Msg_N ("|representation item appears too late!", N);
               end if;

            else
               Error_Msg_N ("|representation item appears too late!", N);

               S := First_Subtype (T);
               if Present (Freeze_Node (S)) then
                  Error_Msg_NE
                    ("??no more representation items for }",
                     Freeze_Node (S), S);
               end if;
            end if;
         end if;
      end Too_Late;

      --  Local variables

      Parent_Type : Entity_Id;

   --  Start of processing for Rep_Item_Too_Late

   begin
      --  First make sure entity is not frozen (RM 13.1(9))

      if Is_Frozen (T)

        --  Exclude imported types, which may be frozen if they appear in a
        --  representation clause for a local type.

        and then not From_Limited_With (T)

        --  Exclude generated entities (not coming from source). The common
        --  case is when we generate a renaming which prematurely freezes the
        --  renamed internal entity, but we still want to be able to set copies
        --  of attribute values such as Size/Alignment.

        and then Comes_From_Source (T)
      then
         --  A self-referential aspect is illegal if it forces freezing the
         --  entity before the corresponding pragma has been analyzed.

         if Nkind (N) in N_Attribute_Definition_Clause | N_Pragma
           and then From_Aspect_Specification (N)
         then
            Error_Msg_NE
              ("aspect specification causes premature freezing of&", N, T);
            Set_Has_Delayed_Freeze (T, False);
            return True;
         end if;

         Too_Late;

         return True;

      --  Check for case of untagged derived type whose parent either has
      --  primitive operations (pre Ada 2022), or is a by-reference type (RM
      --  13.1(10)). In this case we do not output a Too_Late message, since
      --  there is no earlier point where the rep item could be placed to make
      --  it legal.
      --  ??? Confirming representation clauses should be allowed here.

      elsif Is_Type (T)
        and then not FOnly
        and then Is_Derived_Type (T)
        and then not Is_Tagged_Type (T)
      then
         Parent_Type := Etype (Base_Type (T));

         if Relaxed_RM_Semantics then
            null;

         elsif Ada_Version <= Ada_2012
           and then Has_Primitive_Operations (Parent_Type)
         then
            Error_Msg_N
              ("|representation item not permitted before Ada 2022!", N);
            Error_Msg_NE
              ("\parent type & has primitive operations!", N, Parent_Type);
            return True;

         elsif Is_By_Reference_Type (Parent_Type) then
            No_Type_Rep_Item (N);
            Error_Msg_NE
              ("\parent type & is a by-reference type!", N, Parent_Type);
            return True;
         end if;
      end if;

      --  No error, but one more warning to consider. The RM (surprisingly)
      --  allows this pattern in some cases:

      --    type S is ...
      --    primitive operations for S
      --    type R is new S;
      --    rep clause for S

      --  Meaning that calls on the primitive operations of S for values of
      --  type R may require possibly expensive implicit conversion operations.
      --  So even when this is not an error, it is still worth a warning.

      if not Relaxed_RM_Semantics and then Is_Type (T) then
         declare
            DTL : constant Entity_Id := Derived_Type_Link (Base_Type (T));

         begin
            if Present (DTL)

              --  For now, do not generate this warning for the case of
              --  aspect specification using Ada 2012 syntax, since we get
              --  wrong messages we do not understand. The whole business
              --  of derived types and rep items seems a bit confused when
              --  aspects are used, since the aspects are not evaluated
              --  till freeze time. However, AI12-0109 confirms (in an AARM
              --  ramification) that inheritance in this case is required
              --  to work.

              and then not From_Aspect_Specification (N)
            then
               if Is_By_Reference_Type (T)
                 and then not Is_Tagged_Type (T)
                 and then Is_Type_Related_Rep_Item (N)
                 and then (Ada_Version >= Ada_2012
                            or else Has_Primitive_Operations (Base_Type (T)))
               then
                  --  Treat as hard error (AI12-0109, binding interpretation).
                  --  Implementing a change of representation is not really
                  --  an option in the case of a by-reference type, so we
                  --  take this path for all Ada dialects if primitive
                  --  operations are present.
                  Error_Msg_Sloc := Sloc (DTL);
                  Error_Msg_N
                    ("representation item for& appears after derived type "
                     & "declaration#", N);

               elsif Has_Primitive_Operations (Base_Type (T)) then
                  Error_Msg_Sloc := Sloc (DTL);

                  Error_Msg_N
                    ("representation item for& appears after derived type "
                     & "declaration#??", N);
                  Error_Msg_NE
                    ("\may result in implicit conversions for primitive "
                     & "operations of&??", N, T);
                  Error_Msg_NE
                    ("\to change representations when called with arguments "
                     & "of type&??", N, DTL);
               end if;
            end if;
         end;
      end if;

      --  No error, link item into head of chain of rep items for the entity,
      --  but avoid chaining if we have an overloadable entity, and the pragma
      --  is one that can apply to multiple overloaded entities.

      if Is_Overloadable (T) and then Nkind (N) = N_Pragma then
         declare
            Pname : constant Name_Id := Pragma_Name (N);
         begin
            if Pname in Name_Convention | Name_Import | Name_Export
                      | Name_External   | Name_Interface
            then
               return False;
            end if;
         end;
      end if;

      Record_Rep_Item (T, N);
      return False;
   end Rep_Item_Too_Late;

   -------------------------------------
   -- Replace_Type_References_Generic --
   -------------------------------------

   procedure Replace_Type_References_Generic (N : Node_Id; T : Entity_Id) is
      TName : constant Name_Id := Chars (T);

      function Replace_Type_Ref (N : Node_Id) return Traverse_Result;
      --  Processes a single node in the traversal procedure below, checking
      --  if node N should be replaced, and if so, doing the replacement.

      function Visible_Component (Comp : Name_Id) return Entity_Id;
      --  Given an identifier in the expression, check whether there is a
      --  discriminant, component, protected procedure, or entry of the type
      --  that is directy visible, and rewrite it as the corresponding selected
      --  component of the formal of the subprogram.

      ----------------------
      -- Replace_Type_Ref --
      ----------------------

      function Replace_Type_Ref (N : Node_Id) return Traverse_Result is
         Loc : constant Source_Ptr := Sloc (N);

         procedure Add_Prefix (Ref : Node_Id; Comp : Entity_Id);
         --  Add the proper prefix to a reference to a component of the type
         --  when it is not already a selected component.

         ----------------
         -- Add_Prefix --
         ----------------

         procedure Add_Prefix (Ref : Node_Id; Comp : Entity_Id) is
         begin
            Rewrite (Ref,
              Make_Selected_Component (Loc,
                Prefix        => New_Occurrence_Of (T, Loc),
                Selector_Name => New_Occurrence_Of (Comp, Loc)));
            Replace_Type_Reference (Prefix (Ref));
         end Add_Prefix;

         --  Local variables

         Comp : Entity_Id;
         Pref : Node_Id;
         Scop : Entity_Id;

      --  Start of processing for Replace_Type_Ref

      begin
         if Nkind (N) = N_Identifier then

            --  If not the type name, check whether it is a reference to some
            --  other type, which must be frozen before the predicate function
            --  is analyzed, i.e. before the freeze node of the type to which
            --  the predicate applies.

            if Chars (N) /= TName then
               if Present (Current_Entity (N))
                 and then Is_Type (Current_Entity (N))
               then
                  Freeze_Before (Freeze_Node (T), Current_Entity (N));
               end if;

               --  The components of the type are directly visible and can
               --  be referenced in the source code without a prefix.
               --  If a name denoting a component doesn't already have a
               --  prefix, then normalize it by adding a reference to the
               --  current instance of the type as a prefix.
               --
               --  This isn't right in the pathological corner case of an
               --  object-declaring expression (e.g., a quantified expression
               --  or a declare expression) that declares an object with the
               --  same name as a visible component declaration, thereby hiding
               --  the component within that expression. For example, given a
               --  record with a Boolean component "C" and a dynamic predicate
               --  "C = (for some C in Character => Some_Function (C))", only
               --  the first of the two uses of C should have a prefix added
               --  here; instead, both will get prefixes.

               if Nkind (Parent (N)) /= N_Selected_Component
                 or else N /= Selector_Name (Parent (N))
               then
                  Comp := Visible_Component (Chars (N));

                  if Present (Comp) then
                     Add_Prefix (N, Comp);
                  end if;
               end if;

               return Skip;

            --  Otherwise do the replacement if this is not a qualified
            --  reference to a homograph of the type itself. Note that the
            --  current instance could not appear in such a context, e.g.
            --  the prefix of a type conversion.

            else
               if Nkind (Parent (N)) /= N_Selected_Component
                 or else N /= Selector_Name (Parent (N))
               then
                  Replace_Type_Reference (N);
               end if;

               return Skip;
            end if;

         --  Case of selected component, which may be a subcomponent of the
         --  current instance, or an expanded name which is still unanalyzed.

         elsif Nkind (N) = N_Selected_Component then

            --  If selector name is not our type, keep going (we might still
            --  have an occurrence of the type in the prefix). If it is a
            --  subcomponent of the current entity, add prefix.

            if Nkind (Selector_Name (N)) /= N_Identifier
              or else Chars (Selector_Name (N)) /= TName
            then
               if Nkind (Prefix (N)) = N_Identifier then
                  Comp := Visible_Component (Chars (Prefix (N)));

                  if Present (Comp) then
                     Add_Prefix (Prefix (N), Comp);
                  end if;
               end if;

               return OK;

            --  Selector name is our type, check qualification

            else
               --  Loop through scopes and prefixes, doing comparison

               Scop := Current_Scope;
               Pref := Prefix (N);
               loop
                  --  Continue if no more scopes or scope with no name

                  if No (Scop) or else Nkind (Scop) not in N_Has_Chars then
                     return OK;
                  end if;

                  --  Do replace if prefix is an identifier matching the scope
                  --  that we are currently looking at.

                  if Nkind (Pref) = N_Identifier
                    and then Chars (Pref) = Chars (Scop)
                  then
                     Replace_Type_Reference (N);
                     return Skip;
                  end if;

                  --  Go check scope above us if prefix is itself of the form
                  --  of a selected component, whose selector matches the scope
                  --  we are currently looking at.

                  if Nkind (Pref) = N_Selected_Component
                    and then Nkind (Selector_Name (Pref)) = N_Identifier
                    and then Chars (Selector_Name (Pref)) = Chars (Scop)
                  then
                     Scop := Scope (Scop);
                     Pref := Prefix (Pref);

                  --  For anything else, we don't have a match, so keep on
                  --  going, there are still some weird cases where we may
                  --  still have a replacement within the prefix.

                  else
                     return OK;
                  end if;
               end loop;
            end if;

         --  Continue for any other node kind

         else
            return OK;
         end if;
      end Replace_Type_Ref;

      procedure Replace_Type_Refs is new Traverse_Proc (Replace_Type_Ref);

      -----------------------
      -- Visible_Component --
      -----------------------

      function Visible_Component (Comp : Name_Id) return Entity_Id is
         E : Entity_Id;

      begin
         --  Types with nameable components are record, task, protected types

         if Ekind (T) in E_Record_Type | E_Task_Type | E_Protected_Type then
            --  This is a sequential search, which seems acceptable
            --  efficiency-wise, given the typical size of component
            --  lists, protected operation lists, task item lists, and
            --  check expressions.

            E := First_Entity (T);
            while Present (E) loop
               if Comes_From_Source (E) and then Chars (E) = Comp then
                  return E;
               end if;

               Next_Entity (E);
            end loop;

         --  Private discriminated types may have visible discriminants

         elsif Is_Private_Type (T) and then Has_Discriminants (T) then
            declare
               Decl : constant Node_Id := Declaration_Node (T);

               Discr : Node_Id;

            begin
               --  Loop over the discriminants listed in the discriminant part
               --  of the private type declaration to find one with a matching
               --  name; then, if it exists, return the discriminant entity of
               --  the same name in the type, which is that of its full view.

               if Nkind (Decl) in N_Private_Extension_Declaration
                                | N_Private_Type_Declaration
                 and then Present (Discriminant_Specifications (Decl))
               then
                  Discr := First (Discriminant_Specifications (Decl));

                  while Present (Discr) loop
                     if Chars (Defining_Identifier (Discr)) = Comp then
                        Discr := First_Discriminant (T);

                        while Present (Discr) loop
                           if Chars (Discr) = Comp then
                              return Discr;
                           end if;

                           Next_Discriminant (Discr);
                        end loop;

                        pragma Assert (False);
                     end if;

                     Next (Discr);
                  end loop;
               end if;
            end;
         end if;

         --  Nothing by that name

         return Empty;
      end Visible_Component;

   --  Start of processing for Replace_Type_References_Generic

   begin
      Replace_Type_Refs (N);
   end Replace_Type_References_Generic;

   --------------------------------
   -- Resolve_Aspect_Expressions --
   --------------------------------

   procedure Resolve_Aspect_Expressions (E : Entity_Id) is
      function Resolve_Name (N : Node_Id) return Traverse_Result;
      --  Verify that all identifiers in the expression, with the exception
      --  of references to the current entity, denote visible entities. This
      --  is done only to detect visibility errors, as the expression will be
      --  properly analyzed/expanded during analysis of the predicate function
      --  body. We omit quantified expressions from this test, given that they
      --  introduce a local identifier that would require proper expansion to
      --  handle properly.

      ------------------
      -- Resolve_Name --
      ------------------

      function Resolve_Name (N : Node_Id) return Traverse_Result is
         Dummy : Traverse_Result;

      begin
         if Nkind (N) = N_Selected_Component then
            if Nkind (Prefix (N)) = N_Identifier
              and then Chars (Prefix (N)) /= Chars (E)
            then
               Find_Selected_Component (N);

               --  Reset the Entity if N is overloaded since the entity might
               --  not be the correct one; allow later resolution to set it
               --  properly.

               if Is_Overloaded (N) then
                  Set_Entity (N, Empty);
               end if;
            end if;

            return Skip;

         --  Resolve identifiers, but not selectors in parameter associations;
         --  such selectors are never resolved by visibility.

         elsif Nkind (N) = N_Identifier
           and then Chars (N) /= Chars (E)
           and then (Nkind (Parent (N)) /= N_Parameter_Association
                      or else N /= Selector_Name (Parent (N)))
         then
            Find_Direct_Name (N);

            --  Reset the Entity as above for selected_components

            if Is_Overloaded (N) then
               Set_Entity (N, Empty);
            end if;

         --  The name in a component association needs no resolution

         elsif Nkind (N) = N_Component_Association then
            Dummy := Resolve_Name (Expression (N));
            return Skip;

         elsif Nkind (N) = N_Quantified_Expression then
            return Skip;
         end if;

         return OK;
      end Resolve_Name;

      procedure Resolve_Aspect_Expression is new Traverse_Proc (Resolve_Name);

      --  Local variables

      ASN : Node_Id := First_Rep_Item (E);

   --  Start of processing for Resolve_Aspect_Expressions

   begin
      while Present (ASN) loop
         if Nkind (ASN) = N_Aspect_Specification and then Entity (ASN) = E then
            declare
               A_Id : constant Aspect_Id := Get_Aspect_Id (ASN);
               Expr : constant Node_Id   := Expression (ASN);

            begin
               case A_Id is

                  when Aspect_Aggregate =>
                     Resolve_Aspect_Aggregate (Entity (ASN), Expr);

                  when Aspect_Stable_Properties =>
                     Resolve_Aspect_Stable_Properties
                       (Entity (ASN), Expr, Class_Present (ASN));

                  when Aspect_Local_Restrictions =>
                     --  Expression is an aggregate, but only syntactically
                     null;

                  --  For now we only deal with aspects that do not generate
                  --  subprograms, or that may mention current instances of
                  --  types. These will require special handling???.

                  when Aspect_Invariant
                     | Aspect_Predicate_Failure
                  =>
                     null;

                  when Aspect_Dynamic_Predicate
                     | Aspect_Ghost_Predicate
                     | Aspect_Predicate
                     | Aspect_Static_Predicate
                  =>
                     --  Preanalyze expression after type replacement to catch
                     --  name resolution errors if the predicate function has
                     --  not been built yet.

                     --  Note that we cannot use Preanalyze_Spec_Expression
                     --  directly because of the special handling required for
                     --  quantifiers (see comments on Resolve_Aspect_Expression
                     --  above) but we need to emulate it properly.

                     if No (Predicate_Function (E)) then
                        declare
                           Save_In_Spec_Expression : constant Boolean :=
                                                       In_Spec_Expression;
                           Save_Full_Analysis : constant Boolean :=
                                                  Full_Analysis;
                        begin
                           In_Spec_Expression := True;
                           Full_Analysis := False;
                           Expander_Mode_Save_And_Set (False);
                           Push_Type (E);
                           Resolve_Aspect_Expression (Expr);
                           Pop_Type (E);
                           Expander_Mode_Restore;
                           Full_Analysis := Save_Full_Analysis;
                           In_Spec_Expression := Save_In_Spec_Expression;
                        end;
                     end if;

                  when Pre_Post_Aspects =>
                     null;

                  when Aspect_Finalizable | Aspect_Iterable =>
                     if Nkind (Expr) = N_Aggregate then
                        declare
                           Assoc : Node_Id;

                        begin
                           Assoc := First (Component_Associations (Expr));
                           while Present (Assoc) loop
                              if Nkind (Expression (Assoc)) in N_Has_Entity
                              then
                                 Find_Direct_Name (Expression (Assoc));
                              end if;

                              Next (Assoc);
                           end loop;
                        end;
                     end if;

                  --  The expression for Default_Value is a static expression
                  --  of the type, but this expression does not freeze the
                  --  type, so it can still appear in a representation clause
                  --  before the actual freeze point.

                  when Aspect_Default_Value =>
                     Check_Aspect_Too_Late (ASN);
                     Preanalyze_Spec_Expression (Expr, E);

                  when Aspect_Default_Component_Value =>
                     Check_Aspect_Too_Late (ASN);
                     Preanalyze_Spec_Expression (Expr, Component_Type (E));

                  when Aspect_CPU
                     | Aspect_Interrupt_Priority
                     | Aspect_Priority
                  =>
                     Push_Type (E);
                     Preanalyze_Spec_Expression (Expr, Any_Integer);
                     Pop_Type (E);

                  --  Ditto for Storage_Size. Any other aspects that carry
                  --  expressions that should not freeze ??? This is only
                  --  relevant to the misuse of deferred constants.

                  when Aspect_Storage_Size =>
                     Preanalyze_Spec_Expression (Expr, Any_Integer);

                  when others =>
                     if Present (Expr) then
                        case Aspect_Argument (A_Id) is
                           when Expression
                              | Optional_Expression
                           =>
                              Analyze_And_Resolve (Expr);

                           when Name
                              | Optional_Name
                           =>
                              if Nkind (Expr) = N_Identifier then
                                 Find_Direct_Name (Expr);

                              elsif Nkind (Expr) = N_Selected_Component then
                                 Find_Selected_Component (Expr);
                              end if;
                        end case;
                     end if;
               end case;
            end;
         end if;

         Next_Rep_Item (ASN);
      end loop;
   end Resolve_Aspect_Expressions;

   ----------------------------
   -- Parse_Aspect_Aggregate --
   ----------------------------

   procedure Parse_Aspect_Aggregate
     (N                   : Node_Id;
      Empty_Subp          : in out Node_Id;
      Add_Named_Subp      : in out Node_Id;
      Add_Unnamed_Subp    : in out Node_Id;
      New_Indexed_Subp    : in out Node_Id;
      Assign_Indexed_Subp : in out Node_Id)
   is
      Assoc   : Node_Id := First (Component_Associations (N));
      Op_Name : Name_Id;
      Subp    : Node_Id;

   begin
      while Present (Assoc) loop
         Subp := Expression (Assoc);
         Op_Name := Chars (First (Choices (Assoc)));
         if Op_Name = Name_Empty then
            Empty_Subp := Subp;

         elsif Op_Name = Name_Add_Named then
            Add_Named_Subp := Subp;

         elsif Op_Name = Name_Add_Unnamed then
            Add_Unnamed_Subp := Subp;

         elsif Op_Name = Name_New_Indexed then
            New_Indexed_Subp := Subp;

         elsif Op_Name = Name_Assign_Indexed then
            Assign_Indexed_Subp := Subp;
         end if;

         Next (Assoc);
      end loop;
   end Parse_Aspect_Aggregate;

   -------------------------------------
   -- Parse_Aspect_Local_Restrictions --
   -------------------------------------

   function Parse_Aspect_Local_Restrictions (Aspect_Spec : Node_Id)
     return Local_Restrict.Local_Restriction_Set
   is
      use Local_Restrict;

      Result : Local_Restriction_Set := (others => False);
      Id     : Node_Id := Expression (Aspect_Spec);
      Is_Agg : constant Boolean := Nkind (Id) = N_Aggregate
        and then not Is_Empty_List (Expressions (Id));
   begin
      if Is_Agg then
         Id := First (Expressions (Id));
      end if;

      while Present (Id) loop
         if Nkind (Id) /= N_Identifier then
            Error_Msg_N ("local restriction name not an identifier", Id);
            exit;
         end if;

         declare
            Found : Boolean := False;
            Nam : constant Name_Id := Chars (Id);
         begin
            for L_R in Local_Restriction loop
               declare
                  S : String := L_R'Img;
               begin
                  --  Note that the instance of System.Case_Util.To_Lower that
                  --  has signature
                  --
                  --     function To_Lower (A : String) return String
                  --
                  --  cannot be used here because it is not present in the
                  --  run-time library used by the bootstrap compiler at the
                  --  time of writing.
                  To_Lower (S);
                  if Length_Of_Name (Nam) = S'Length
                    and then Get_Name_String (Nam) = S
                  then
                     if Result (L_R) then
                        Error_Msg_N ("local restriction duplicated", Id);
                        exit;
                     end if;
                     Found := True;
                     Result (L_R) := True;
                     exit;
                  end if;
               end;
            end loop;

            if not Found then
               Error_Msg_N ("invalid local restriction name", Id);
               exit;
            end if;
         end;

         exit when not Is_Agg;
         Next (Id);
      end loop;

      return Result;
   end Parse_Aspect_Local_Restrictions;

   ------------------------------------
   -- Parse_Aspect_Stable_Properties --
   ------------------------------------

   function Parse_Aspect_Stable_Properties
     (Aspect_Spec : Node_Id; Negated : out Boolean) return Subprogram_List
   is
      function Extract_Entity (Expr : Node_Id) return Entity_Id;
      --  Given an element of a Stable_Properties aspect spec, return the
      --  associated entity.
      --  This function updates the Negated flag as a side effect.

      --------------------
      -- Extract_Entity --
      --------------------

      function Extract_Entity (Expr : Node_Id) return Entity_Id is
         Name : Node_Id;
      begin
         if Nkind (Expr) = N_Op_Not then
            Negated := True;
            Name := Right_Opnd (Expr);
         else
            Name := Expr;
         end if;

         if Nkind (Name) in N_Has_Entity then
            return Entity (Name);
         else
            return Empty;
         end if;
      end Extract_Entity;

      --  Local variables

      L  : List_Id;
      Id : Node_Id;

   --  Start of processing for Parse_Aspect_Stable_Properties

   begin
      Negated := False;

      if Nkind (Aspect_Spec) /= N_Aggregate then
         return (1 => Extract_Entity (Aspect_Spec));
      else
         L := Expressions (Aspect_Spec);
         Id := First (L);

         return Result : Subprogram_List (1 .. List_Length (L)) do
            for I in Result'Range loop
               Result (I) := Extract_Entity (Id);

               if No (Result (I)) then
                  pragma Assert (Serious_Errors_Detected > 0);
                  goto Ignore_Aspect;
               end if;

               Next (Id);
            end loop;
         end return;
      end if;

      <<Ignore_Aspect>> return (1 .. 0 => <>);
   end Parse_Aspect_Stable_Properties;

   -------------------------------
   -- Validate_Aspect_Aggregate --
   -------------------------------

   procedure Validate_Aspect_Aggregate (N : Node_Id) is
      Empty_Subp          : Node_Id := Empty;
      Add_Named_Subp      : Node_Id := Empty;
      Add_Unnamed_Subp    : Node_Id := Empty;
      New_Indexed_Subp    : Node_Id := Empty;
      Assign_Indexed_Subp : Node_Id := Empty;

   begin
      Error_Msg_Ada_2022_Feature ("aspect Aggregate", Sloc (N));

      if Nkind (N) /= N_Aggregate
        or else Present (Expressions (N))
        or else No (Component_Associations (N))
      then
         Error_Msg_N ("aspect Aggregate requires an aggregate "
                        & "with component associations", N);
         return;
      end if;

      Parse_Aspect_Aggregate (N,
        Empty_Subp, Add_Named_Subp, Add_Unnamed_Subp,
        New_Indexed_Subp, Assign_Indexed_Subp);

      if No (Empty_Subp) then
         Error_Msg_N ("missing specification for Empty in aggregate", N);
      end if;

      if Present (Add_Named_Subp) then
         if Present (Add_Unnamed_Subp)
           or else Present (Assign_Indexed_Subp)
         then
            Error_Msg_N
             ("conflicting operations for aggregate (RM 4.3.5)", N);
            return;
         end if;

      elsif No (Add_Named_Subp)
        and then No (Add_Unnamed_Subp)
        and then No (Assign_Indexed_Subp)
      then
         Error_Msg_N ("incomplete specification for aggregate", N);

      elsif Present (New_Indexed_Subp) /= Present (Assign_Indexed_Subp) then
         Error_Msg_N ("incomplete specification for indexed aggregate", N);
      end if;
   end Validate_Aspect_Aggregate;

   -----------------------------------------
   --  Validate_Aspect_Local_Restrictions --
   -----------------------------------------

   procedure Validate_Aspect_Local_Restrictions (E : Entity_Id; N : Node_Id) is
      use Local_Restrict;
   begin
      --  Do not check Is_Parenthesis_Aggregate. We don't want to
      --  disallow the more familiar parens, but we also don't
      --  want to require parens for a homogeneous list.

      if Nkind (N) = N_Identifier and then Paren_Count (N) = 1 then
         --  a positional aggregate with one element (in effect) is ok
         null;
      elsif Nkind (N) /= N_Aggregate
        or else No (Expressions (N))
        or else Present (Component_Associations (N))
      then
         Error_Msg_N
           ("aspect Local_Restrictions requires a parenthesized list", N);
         return;
      end if;

      declare
         Set : constant Local_Restriction_Set :=
           Parse_Aspect_Local_Restrictions (Parent (N));
         pragma Unreferenced (Set);
      begin
         null;
      end;

      --  This will be relaxed later, e.g. for generic subprograms or
      --  for packages.

      if Ekind (E) in Subprogram_Kind | E_Package then
         if Get_Renamed_Entity (E) /= E then
            Error_Msg_N
              ("aspect Local_Restrictions cannot be specified for "
                 & "a renaming", N);
         end if;
      else
         Error_Msg_N
           ("aspect Local_Restrictions can only be specified for "
              & "a subprogram or package spec", N);
      end if;
   end Validate_Aspect_Local_Restrictions;

   ---------------------------------------
   -- Validate_Aspect_Stable_Properties --
   ---------------------------------------

   procedure Validate_Aspect_Stable_Properties
     (E : Entity_Id; N : Node_Id; Class_Present : Boolean)
   is
      Is_Aspect_Of_Type : constant Boolean := Is_Type (E);

      type Permission is (Forbidden, Optional, Required);
      Modifier_Permission : Permission :=
       (if Is_Aspect_Of_Type then Forbidden else Optional);
      Modifier_Error_Called : Boolean := False;

      procedure Check_Property_Function_Arg (PF_Arg : Node_Id);
      --  Check syntax of a property function argument

      ----------------------------------
      -- Check_Property_Function_Arg --
      ----------------------------------

      procedure Check_Property_Function_Arg (PF_Arg : Node_Id) is
         procedure Modifier_Error;
         --  Generate message about bad "not" modifier if no message already
         --  generated. Errors include specifying "not" for an aspect of
         --  of a type and specifying "not" for some but not all of the
         --  names in a list.

         --------------------
         -- Modifier_Error --
         --------------------

         procedure Modifier_Error is
         begin
            if Modifier_Error_Called then
               return; -- error message already generated
            end if;

            Modifier_Error_Called := True;

            if Is_Aspect_Of_Type then
               Error_Msg_N
                 ("NOT modifier not allowed for Stable_Properties aspect"
                  & " of a type", PF_Arg);
            else
               Error_Msg_N ("mixed use of NOT modifiers", PF_Arg);
            end if;
         end Modifier_Error;

         PF_Name : Node_Id := PF_Arg;

      --  Start of processing for Check_Property_Function_Arg

      begin
         if Nkind (PF_Arg) = N_Op_Not then
            PF_Name := Right_Opnd (PF_Arg);

            case Modifier_Permission is
               when Forbidden =>
                  Modifier_Error;
               when Optional =>
                  Modifier_Permission := Required;
               when Required =>
                  null;
            end case;
         else
            case Modifier_Permission is
               when Forbidden =>
                  null;
               when Optional =>
                  Modifier_Permission := Forbidden;
               when Required =>
                  Modifier_Error;
            end case;
         end if;

         if Nkind (PF_Name) not in
           N_Identifier | N_Operator_Symbol | N_Selected_Component
         then
            Error_Msg_N ("bad property function name", PF_Name);
         end if;
      end Check_Property_Function_Arg;

   --  Start of processing for Validate_Aspect_Stable_Properties

   begin
      Error_Msg_Ada_2022_Feature ("aspect Stable_Properties", Sloc (N));

      if not Is_Aspect_Of_Type and then not Is_Subprogram (E) then
         Error_Msg_N ("Stable_Properties aspect can only be specified for "
                      & "a type or a subprogram", N);
      elsif Class_Present then
         if Is_Aspect_Of_Type then
            if not Is_Tagged_Type (E) then
               Error_Msg_N
                 ("Stable_Properties''Class aspect cannot be specified for "
                  & "an untagged type", N);
            end if;
         else
            if not Is_Dispatching_Operation (E) then
               Error_Msg_N
                 ("Stable_Properties''Class aspect cannot be specified for "
                  & "a subprogram that is not a primitive subprogram "
                  & "of a tagged type", N);
            end if;
         end if;
      end if;

      if Nkind (N) = N_Aggregate then
         if Present (Component_Associations (N))
            or else Null_Record_Present (N)
            or else No (Expressions (N))
         then
            Error_Msg_N ("bad Stable_Properties aspect specification", N);
            return;
         end if;

         declare
            PF_Arg : Node_Id := First (Expressions (N));
         begin
            while Present (PF_Arg) loop
               Check_Property_Function_Arg (PF_Arg);
               Next (PF_Arg);
            end loop;
         end;
      else
         Check_Property_Function_Arg (N);
      end if;
   end Validate_Aspect_Stable_Properties;

   ----------------------------------
   -- Resolve_Finalizable_Argument --
   ----------------------------------

   procedure Resolve_Finalizable_Argument
     (N   : Node_Id;
      Typ : Entity_Id;
      Nam : Name_Id)
   is
      function Is_Finalizable_Primitive (E : Entity_Id) return Boolean;
      --  Check whether E is a finalizable primitive for Typ

      ------------------------------
      -- Is_Finalizable_Primitive --
      ------------------------------

      function Is_Finalizable_Primitive (E : Entity_Id) return Boolean is
      begin
         return Ekind (E) = E_Procedure
           and then Scope (E) = Scope (Typ)
           and then Present (First_Formal (E))
           and then Ekind (First_Formal (E)) = E_In_Out_Parameter
           and then Etype (First_Formal (E)) = Typ
           and then No (Next_Formal (First_Formal (E)));
      end Is_Finalizable_Primitive;

   --  Start of processing for Resolve_Finalizable_Argument

   begin
      if Nam = Name_Relaxed_Finalization then
         Resolve (N, Any_Boolean);

         if Is_OK_Static_Expression (N) then
            Set_Has_Relaxed_Finalization (Typ, Is_True (Static_Boolean (N)));

         else
            Flag_Non_Static_Expr
              ("expression of aspect Finalizable must be static!", N);
         end if;

         return;
      end if;

      if not Is_Entity_Name (N) then
         null;

      elsif not Is_Overloaded (N) then
         if Is_Finalizable_Primitive (Entity (N)) then
            return;
         end if;

      else
         --  Overloaded case: find subprogram with proper signature

         declare
            I  : Interp_Index;
            It : Interp;

         begin
            Get_First_Interp (N, I, It);

            while Present (It.Typ) loop
               if Is_Finalizable_Primitive (It.Nam) then
                  Set_Entity (N, It.Nam);
                  return;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      Error_Msg_N
        ("finalizable primitive must be local procedure whose only formal " &
         "parameter has mode `IN OUT` and is of the finalizable type", N);
   end Resolve_Finalizable_Argument;

   --------------------------------
   -- Resolve_Iterable_Operation --
   --------------------------------

   procedure Resolve_Iterable_Operation
     (N      : Node_Id;
      Cursor : Entity_Id;
      Typ    : Entity_Id;
      Nam    : Name_Id)
   is
      Ent : Entity_Id;
      F1  : Entity_Id;
      F2  : Entity_Id;

   begin
      if not Is_Overloaded (N) then
         if not Is_Entity_Name (N)
           or else Ekind (Entity (N)) /= E_Function
           or else Scope (Entity (N)) /= Scope (Typ)
           or else No (First_Formal (Entity (N)))
           or else Etype (First_Formal (Entity (N))) /= Typ
         then
            Error_Msg_N
              ("iterable primitive must be local function name whose first "
               & "formal is an iterable type", N);
            return;
         end if;

         Ent := Entity (N);
         F1  := First_Formal (Ent);
         F2  := Next_Formal (F1);

         if Nam = Name_First then

            --  First (Container) => Cursor

            if Etype (Ent) /= Cursor then
               Error_Msg_N ("primitive for First must yield a cursor", N);
            elsif Present (F2) then
               Error_Msg_N ("no match for First iterable primitive", N);
            end if;

         elsif Nam = Name_Last then

            --  Last (Container) => Cursor

            if Etype (Ent) /= Cursor then
               Error_Msg_N ("primitive for Last must yield a cursor", N);
            elsif Present (F2) then
               Error_Msg_N ("no match for Last iterable primitive", N);
            end if;

         elsif Nam = Name_Next then

            --  Next (Container, Cursor) => Cursor

            if No (F2)
              or else Etype (F2) /= Cursor
              or else Etype (Ent) /= Cursor
              or else Present (Next_Formal (F2))
            then
               Error_Msg_N ("no match for Next iterable primitive", N);
            end if;

         elsif Nam = Name_Previous then

            --  Previous (Container, Cursor) => Cursor

            if No (F2)
              or else Etype (F2) /= Cursor
              or else Etype (Ent) /= Cursor
              or else Present (Next_Formal (F2))
            then
               Error_Msg_N ("no match for Previous iterable primitive", N);
            end if;

         elsif Nam = Name_Has_Element then

            --  Has_Element (Container, Cursor) => Boolean

            if No (F2)
              or else Etype (F2) /= Cursor
              or else Etype (Ent) /= Standard_Boolean
              or else Present (Next_Formal (F2))
            then
               Error_Msg_N ("no match for Has_Element iterable primitive", N);
            end if;

         elsif Nam = Name_Element then

            --  Element (Container, Cursor) => Element_Type;

            if No (F2)
              or else Etype (F2) /= Cursor
              or else Present (Next_Formal (F2))
            then
               Error_Msg_N ("no match for Element iterable primitive", N);
            end if;

         else
            raise Program_Error;
         end if;

      else
         --  Overloaded case: find subprogram with proper signature. Caller
         --  will report error if no match is found.

         declare
            I  : Interp_Index;
            It : Interp;

         begin
            Get_First_Interp (N, I, It);
            while Present (It.Typ) loop
               if Ekind (It.Nam) = E_Function
                  and then Scope (It.Nam) = Scope (Typ)
                  and then Present (First_Formal (It.Nam))
                  and then Etype (First_Formal (It.Nam)) = Typ
               then
                  F1 := First_Formal (It.Nam);

                  if Nam = Name_First then
                     if Etype (It.Nam) = Cursor
                       and then No (Next_Formal (F1))
                     then
                        Set_Entity (N, It.Nam);
                        exit;
                     end if;

                  elsif Nam = Name_Next then
                     F2 := Next_Formal (F1);

                     if Present (F2)
                       and then No (Next_Formal (F2))
                       and then Etype (F2) = Cursor
                       and then Etype (It.Nam) = Cursor
                     then
                        Set_Entity (N, It.Nam);
                        exit;
                     end if;

                  elsif Nam = Name_Has_Element then
                     F2 := Next_Formal (F1);

                     if Present (F2)
                       and then No (Next_Formal (F2))
                       and then Etype (F2) = Cursor
                       and then Etype (It.Nam) = Standard_Boolean
                     then
                        Set_Entity (N, It.Nam);
                        F2 := Next_Formal (F1);
                        exit;
                     end if;

                  elsif Nam = Name_Element then
                     F2 := Next_Formal (F1);

                     if Present (F2)
                       and then No (Next_Formal (F2))
                       and then Etype (F2) = Cursor
                     then
                        Set_Entity (N, It.Nam);
                        exit;
                     end if;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;
   end Resolve_Iterable_Operation;

   ------------------------------
   -- Resolve_Aspect_Aggregate --
   ------------------------------

   procedure Resolve_Aspect_Aggregate
    (Typ  : Entity_Id;
     Expr : Node_Id)
   is
      function Valid_Empty          (E : Entity_Id) return Boolean;
      function Valid_Add_Named      (E : Entity_Id) return Boolean;
      function Valid_Add_Unnamed    (E : Entity_Id) return Boolean;
      function Valid_New_Indexed    (E : Entity_Id) return Boolean;
      function Valid_Assign_Indexed (E : Entity_Id) return Boolean;
      --  Predicates that establish the legality of each possible operation in
      --  an Aggregate aspect.

      generic
        with function Pred (Id : Node_Id) return Boolean;
      procedure Resolve_Operation (Subp_Id : Node_Id);
      --  Common processing to resolve each aggregate operation.

      ------------------------
      -- Valid_Assign_Index --
      ------------------------

      function Valid_Assign_Indexed (E : Entity_Id) return Boolean is
      begin
         --  The profile must be the same as for Add_Named, with the added
         --  requirement that the key_type be a discrete type.

         if Valid_Add_Named (E) then
            return Is_Discrete_Type (Etype (Next_Formal (First_Formal (E))));
         else
            return False;
         end if;
      end Valid_Assign_Indexed;

      -----------------
      -- Valid_Empty --
      -----------------

      function Valid_Empty (E :  Entity_Id) return Boolean is
      begin
         if Etype (E) /= Typ or else Scope (E) /= Scope (Typ) then
            return False;

         elsif Ekind (E) = E_Function then
            return No (First_Formal (E))
              or else
                (Is_Signed_Integer_Type (Etype (First_Formal (E)))
                  and then No (Next_Formal (First_Formal (E))));
         else
            return False;
         end if;
      end Valid_Empty;

      ---------------------
      -- Valid_Add_Named --
      ---------------------

      function Valid_Add_Named (E : Entity_Id) return Boolean is
         F2, F3 : Entity_Id;
      begin
         if Ekind (E) = E_Procedure
           and then Scope (E) = Scope (Typ)
           and then Number_Formals (E) = 3
           and then Etype (First_Formal (E)) = Typ
           and then Ekind (First_Formal (E)) = E_In_Out_Parameter
         then
            F2 := Next_Formal (First_Formal (E));
            F3 := Next_Formal (F2);
            return Ekind (F2) = E_In_Parameter
              and then Ekind (F3) = E_In_Parameter
              and then not Is_Limited_Type (Etype (F2))
              and then not Is_Limited_Type (Etype (F3));
         else
            return False;
         end if;
      end Valid_Add_Named;

      -----------------------
      -- Valid_Add_Unnamed --
      -----------------------

      function Valid_Add_Unnamed (E : Entity_Id) return Boolean is
      begin
         return Ekind (E) = E_Procedure
           and then Scope (E) = Scope (Typ)
           and then Number_Formals (E) = 2
           and then Etype (First_Formal (E)) = Typ
           and then Ekind (First_Formal (E)) = E_In_Out_Parameter
           and then
             not Is_Limited_Type (Etype (Next_Formal (First_Formal (E))));
      end Valid_Add_Unnamed;

      -----------------------
      -- Valid_Nmw_Indexed --
      -----------------------

      function Valid_New_Indexed (E : Entity_Id) return Boolean is
      begin
         return Ekind (E) = E_Function
           and then Scope (E) = Scope (Typ)
           and then Etype (E) = Typ
           and then Number_Formals (E) = 2
           and then Is_Discrete_Type (Etype (First_Formal (E)))
           and then Etype (First_Formal (E)) =
             Etype (Next_Formal (First_Formal (E)));
      end Valid_New_Indexed;

      -----------------------
      -- Resolve_Operation --
      -----------------------

      procedure Resolve_Operation (Subp_Id : Node_Id) is
         Subp : Entity_Id;

         I  : Interp_Index;
         It : Interp;

      begin
         if not Is_Overloaded (Subp_Id) then
            Subp := Entity (Subp_Id);
            if not Pred (Subp) then
               Error_Msg_NE
                 ("improper aggregate operation for&", Subp_Id, Typ);
            end if;

         else
            Set_Entity (Subp_Id, Empty);
            Get_First_Interp (Subp_Id, I, It);
            while Present (It.Nam) loop
               if Pred (It.Nam) then
                  Set_Is_Overloaded (Subp_Id, False);
                  Set_Entity (Subp_Id, It.Nam);
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;

            if No (Entity (Subp_Id)) then
               Error_Msg_NE
                 ("improper aggregate operation for&", Subp_Id, Typ);
            end if;
         end if;
      end Resolve_Operation;

      Assoc   : Node_Id;
      Op_Name : Name_Id;
      Subp_Id : Node_Id;

      procedure Resolve_Empty   is new Resolve_Operation (Valid_Empty);
      procedure Resolve_Unnamed is new Resolve_Operation (Valid_Add_Unnamed);
      procedure Resolve_Named   is new Resolve_Operation (Valid_Add_Named);
      procedure Resolve_Indexed is new Resolve_Operation (Valid_New_Indexed);
      procedure Resolve_Assign_Indexed
                                is new Resolve_Operation
                                                      (Valid_Assign_Indexed);

   --  Start of processing for Resolve_Aspect_Aggregate

   begin
      Assoc := First (Component_Associations (Expr));

      while Present (Assoc) loop
         Op_Name := Chars (First (Choices (Assoc)));

         --  When verifying the consistency of aspects between the freeze point
         --  and the end of declarations, we use a copy which is not analyzed
         --  yet, so do it now.

         Subp_Id := Expression (Assoc);
         if No (Etype (Subp_Id)) then
            Analyze (Subp_Id);
         end if;

         if Op_Name = Name_Empty then
            Resolve_Empty (Subp_Id);

         elsif Op_Name = Name_Add_Named then
            Resolve_Named (Subp_Id);

         elsif Op_Name = Name_Add_Unnamed then
            Resolve_Unnamed (Subp_Id);

         elsif Op_Name = Name_New_Indexed then
            Resolve_Indexed (Subp_Id);

         elsif Op_Name = Name_Assign_Indexed then
            Resolve_Assign_Indexed (Subp_Id);
         end if;

         Next (Assoc);
      end loop;
   end Resolve_Aspect_Aggregate;

   --------------------------------------
   -- Resolve_Aspect_Stable_Properties --
   --------------------------------------

   procedure Resolve_Aspect_Stable_Properties
    (Typ_Or_Subp : Entity_Id; Expr : Node_Id; Class_Present : Boolean)
   is
      Is_Aspect_Of_Type : constant Boolean := Is_Type (Typ_Or_Subp);

      Singleton : constant Boolean := Nkind (Expr) /= N_Aggregate;
      Subp_Name : Node_Id := (if Singleton
                              then Expr
                              else First (Expressions (Expr)));
      Has_Not   : Boolean;
   begin
      if Is_Aspect_Of_Type
         and then Has_Private_Declaration (Typ_Or_Subp)
         and then not Is_Private_Type (Typ_Or_Subp)
      then
         Error_Msg_N
           ("Stable_Properties aspect cannot be specified " &
             "for the completion of a private type", Typ_Or_Subp);
      end if;

      --  Analogous checks that the aspect is not specified for a completion
      --  in the subprogram case are not performed here because they are not
      --  specific to this particular aspect. Right ???

      loop
         Has_Not := Nkind (Subp_Name) = N_Op_Not;
         if Has_Not then
            Set_Analyzed (Subp_Name); -- ???
            Subp_Name := Right_Opnd (Subp_Name);
         end if;

         if No (Etype (Subp_Name)) then
            Analyze (Subp_Name);
         end if;

         declare
            Subp : Entity_Id := Empty;

            I  : Interp_Index;
            It : Interp;

            function Is_Property_Function (E : Entity_Id) return Boolean;
            --  Implements RM 7.3.4 definition of "property function"

            --------------------------
            -- Is_Property_Function --
            --------------------------

            function Is_Property_Function (E : Entity_Id) return Boolean is
            begin
               if Ekind (E) not in E_Function | E_Operator
                 or else Number_Formals (E) /= 1
               then
                  return False;
               end if;

               declare
                  Param_Type : constant Entity_Id :=
                     Base_Type (Etype (First_Formal (E)));

                  function Matches_Param_Type (Typ : Entity_Id)
                    return Boolean is
                    (Base_Type (Typ) = Param_Type
                     or else
                     (Is_Class_Wide_Type (Param_Type)
                      and then Is_Ancestor (Root_Type (Param_Type),
                                            Base_Type (Typ))));
               begin
                  if Is_Aspect_Of_Type then
                     if Matches_Param_Type (Typ_Or_Subp) then
                        return True;
                     end if;
                  elsif Is_Primitive (Typ_Or_Subp) then
                     declare
                        Formal : Entity_Id := First_Formal (Typ_Or_Subp);
                     begin
                        while Present (Formal) loop
                           if Matches_Param_Type (Etype (Formal)) then

                              --  Test whether Typ_Or_Subp (which is a subp
                              --  in this case) is primitive op of the type
                              --  of this parameter.
                              if Scope (Typ_Or_Subp) = Scope (Param_Type) then
                                 return True;
                              end if;
                           end if;
                           Next_Formal (Formal);
                        end loop;
                     end;
                  end if;
               end;

               return False;
            end Is_Property_Function;
         begin
            if not Is_Overloaded (Subp_Name) then
               Subp := Entity (Subp_Name);
               if not Is_Property_Function (Subp) then
                  Error_Msg_NE ("improper property function for&",
                    Subp_Name, Typ_Or_Subp);
                  return;
               end if;
            else
               Set_Entity (Subp_Name, Empty);
               Get_First_Interp (Subp_Name, I, It);
               while Present (It.Nam) loop
                  if Is_Property_Function (It.Nam) then
                     if Present (Subp) then
                        Error_Msg_NE
                          ("ambiguous property function name for&",
                           Subp_Name, Typ_Or_Subp);
                        return;
                     end if;

                     Subp := It.Nam;
                     Set_Is_Overloaded (Subp_Name, False);
                     Set_Entity (Subp_Name, Subp);
                  end if;

                  Get_Next_Interp (I, It);
               end loop;

               if No (Subp) then
                  Error_Msg_NE ("improper property function for&",
                    Subp_Name, Typ_Or_Subp);
                  return;
               end if;
            end if;

            --  perform legality (as opposed to name resolution) Subp checks

            if Is_Limited_Type (Etype (Subp)) then
               Error_Msg_NE
                 ("result type of property function for& is limited",
                  Subp_Name, Typ_Or_Subp);
            end if;

            if Ekind (First_Formal (Subp)) /= E_In_Parameter then
               Error_Msg_NE
                 ("mode of parameter of property function for& is not IN",
                  Subp_Name, Typ_Or_Subp);
            end if;

            if Is_Class_Wide_Type (Etype (First_Formal (Subp))) then
               if not Covers (Etype (First_Formal (Subp)), Typ_Or_Subp) then
                  Error_Msg_NE
                    ("class-wide parameter type of property function " &
                     "for& does not cover the type",
                     Subp_Name, Typ_Or_Subp);

               --  ??? This test is slightly stricter than 7.3.4(12/5);
               --  some legal corner cases may be incorrectly rejected.
               elsif Scope (Subp) /= Scope (Etype (First_Formal (Subp)))
               then
                  Error_Msg_NE
                    ("property function for& not declared in same scope " &
                     "as parameter type",
                     Subp_Name, Typ_Or_Subp);
               end if;
            elsif Is_Aspect_Of_Type and then
              Scope (Subp) /= Scope (Typ_Or_Subp) and then
              Scope (Subp) /= Standard_Standard --  e.g., derived type's "abs"
            then
               Error_Msg_NE
                 ("property function for& " &
                  "not a primitive function of the type",
                  Subp_Name, Typ_Or_Subp);
            end if;

            if Has_Not then
               --  check that Subp was mentioned in param type's aspect spec
               declare
                  Param_Type : constant Entity_Id :=
                    Base_Type (Etype (First_Formal (Subp)));
                  Aspect_Spec : constant Node_Id :=
                    Find_Value_Of_Aspect
                      (Param_Type, Aspect_Stable_Properties,
                       Class_Present => Class_Present);
                  Found : Boolean := False;
               begin
                  if Present (Aspect_Spec) then
                     declare
                        Ignored : Boolean;
                        SPF_List : constant Subprogram_List :=
                          Parse_Aspect_Stable_Properties
                            (Aspect_Spec, Negated => Ignored);
                     begin
                        Found := (for some E of SPF_List => E = Subp);
                        --  look through renamings ???
                     end;
                  end if;
                  if not Found then
                     declare
                        CW_Modifier : constant String :=
                          (if Class_Present then "class-wide " else "");
                     begin
                        Error_Msg_NE
                       (CW_Modifier
                         & "property function for& mentioned after NOT "
                         & "but not a "
                         & CW_Modifier
                         & "stable property function of its parameter type",
                        Subp_Name, Typ_Or_Subp);
                     end;
                  end if;
               end;
            end if;
         end;

         exit when Singleton;
         Subp_Name :=
           Next ((if Has_Not then Parent (Subp_Name) else Subp_Name));
         exit when No (Subp_Name);
      end loop;

      Set_Analyzed (Expr);
   end Resolve_Aspect_Stable_Properties;

   -----------------------------------------
   -- Resolve_Storage_Model_Type_Argument --
   -----------------------------------------

   procedure Resolve_Storage_Model_Type_Argument
     (N         : Node_Id;
      Typ       : Entity_Id;
      Addr_Type : in out Entity_Id;
      Nam       : Name_Id)
   is

      type Formal_Profile is record
         Subt : Entity_Id;
         Mode : Formal_Kind;
      end record;

      type Formal_Profiles is array (Positive range <>) of Formal_Profile;

      function Aspect_Argument_Profile_Matches
        (Subp            : Entity_Id;
         Profiles        : Formal_Profiles;
         Result_Subt     : Entity_Id;
         Err_On_Mismatch : Boolean) return Boolean;
      --  Checks that the formal parameters of subprogram Subp conform to the
      --  subtypes and modes specified by Profiles, as well as to the result
      --  subtype Result_Subt when that is nonempty.

      function Aspect_Argument_Profile_Matches
        (Subp            : Entity_Id;
         Profiles        : Formal_Profiles;
         Result_Subt     : Entity_Id;
         Err_On_Mismatch : Boolean) return Boolean
      is

         procedure Report_Argument_Error
           (Msg    : String;
            Formal : Entity_Id := Empty;
            Subt   : Entity_Id := Empty);
         --  If Err_On_Mismatch is True, reports an argument error given by Msg
         --  associated with Formal and/or Subt.

         procedure Report_Argument_Error
           (Msg    : String;
            Formal : Entity_Id := Empty;
            Subt   : Entity_Id := Empty)
         is
         begin
            if Err_On_Mismatch then
               if Present (Formal) then
                  if Present (Subt) then
                     Error_Msg_Node_2 := Subt;
                  end if;
                  Error_Msg_NE (Msg, N, Formal);

               elsif Present (Subt) then
                  Error_Msg_NE (Msg, N, Subt);

               else
                  Error_Msg_N (Msg, N);
               end if;
            end if;
         end Report_Argument_Error;

         --  Local variables

         Formal    : Entity_Id := First_Formal (Subp);
         Is_Error  : Boolean   := False;

      --  Start of processing for Aspect_Argument_Profile_Matches

      begin
         for FP of Profiles loop
            if No (Formal) then
               Is_Error := True;
               Report_Argument_Error ("missing formal of }", Subt => FP.Subt);
               exit;

            elsif not Subtypes_Statically_Match
                        (Etype (Formal), FP.Subt)
            then
               Is_Error := True;
               Report_Argument_Error
                 ("formal& must be of subtype&",
                  Formal => Formal, Subt => FP.Subt);
               exit;

            elsif Ekind (Formal) /= FP.Mode then
               Is_Error := True;
               Report_Argument_Error
                 ("formal& has wrong mode", Formal => Formal);
               exit;
            end if;

            Formal := Next_Formal (Formal);
         end loop;

         if not Is_Error
           and then Present (Formal)
         then
            Is_Error := True;
            Report_Argument_Error
              ("too many formals for subprogram in aspect");
         end if;

         if not Is_Error
           and then Present (Result_Subt)
           and then not Subtypes_Statically_Match (Etype (Subp), Result_Subt)
         then
            Is_Error := True;
            Report_Argument_Error
              ("subprogram must have result}", Subt => Result_Subt);
         end if;

         return not Is_Error;
      end Aspect_Argument_Profile_Matches;

      --  Local variables

      Ent : Entity_Id;

      Storage_Count_Type  : constant Entity_Id := RTE (RE_Storage_Count);
      System_Address_Type : constant Entity_Id := RTE (RE_Address);

   --  Start of processing for Resolve_Storage_Model_Type_Argument

   begin
      if Nam = Name_Address_Type then
         if not Is_Entity_Name (N)
           or else not Is_Type (Entity (N))
           or else (Root_Type (Entity (N)) /= System_Address_Type
                     and then not Is_Integer_Type (Entity (N)))
         then
            Error_Msg_N ("named entity must be a descendant of System.Address "
                         & "or an integer type", N);
         end if;

         Addr_Type := Entity (N);

         return;

      --  If Addr_Type is not present as the first association, then we default
      --  it to System.Address.

      elsif No (Addr_Type) then
         Addr_Type := RTE (RE_Address);
      end if;

      if Nam = Name_Null_Address then
         if not Is_Entity_Name (N)
           or else not Is_Constant_Object (Entity (N))
           or else
             not Subtypes_Statically_Match (Etype (Entity (N)), Addr_Type)
         then
            Error_Msg_NE
              ("named entity must be constant of subtype}", N, Addr_Type);
         end if;

         return;

      elsif not Is_Overloaded (N) then
         if not Is_Entity_Name (N)
           or else Ekind (Entity (N)) not in E_Function | E_Procedure
           or else Scope (Entity (N)) /= Scope (Typ)
         then
            Error_Msg_N ("argument must be local subprogram name", N);
            return;
         end if;

         Ent := Entity (N);

         if Nam = Name_Allocate then
            if not Aspect_Argument_Profile_Matches
                     (Ent,
                      Profiles        =>
                        ((Typ,                E_In_Out_Parameter),
                         (Addr_Type,          E_Out_Parameter),
                         (Storage_Count_Type, E_In_Parameter),
                         (Storage_Count_Type, E_In_Parameter)),
                      Result_Subt     => Empty,
                      Err_On_Mismatch => True)
            then
               Error_Msg_N ("no match for Allocate operation", N);
            end if;

         elsif Nam = Name_Deallocate then
            if not Aspect_Argument_Profile_Matches
                     (Ent,
                      Profiles        =>
                        ((Typ,                E_In_Out_Parameter),
                         (Addr_Type,          E_In_Parameter),
                         (Storage_Count_Type, E_In_Parameter),
                         (Storage_Count_Type, E_In_Parameter)),
                      Result_Subt     => Empty,
                      Err_On_Mismatch => True)
            then
               Error_Msg_N ("no match for Deallocate operation", N);
            end if;

         elsif Nam = Name_Copy_From then
            if not Aspect_Argument_Profile_Matches
                     (Ent,
                      Profiles        =>
                        ((Typ,                 E_In_Out_Parameter),
                         (System_Address_Type, E_In_Parameter),
                         (Addr_Type,           E_In_Parameter),
                         (Storage_Count_Type,  E_In_Parameter)),
                      Result_Subt     => Empty,
                      Err_On_Mismatch => True)
            then
               Error_Msg_N ("no match for Copy_From operation", N);
            end if;

         elsif Nam = Name_Copy_To then
            if not Aspect_Argument_Profile_Matches
                     (Ent,
                      Profiles        =>
                        ((Typ,                 E_In_Out_Parameter),
                         (Addr_Type,           E_In_Parameter),
                         (System_Address_Type, E_In_Parameter),
                         (Storage_Count_Type,  E_In_Parameter)),
                      Result_Subt     => Empty,
                      Err_On_Mismatch => True)
            then
               Error_Msg_N ("no match for Copy_To operation", N);
            end if;

         elsif Nam = Name_Storage_Size then
            if not Aspect_Argument_Profile_Matches
                     (Ent,
                      Profiles        => (1 => (Typ, E_In_Parameter)),
                      Result_Subt     => Storage_Count_Type,
                      Err_On_Mismatch => True)
            then
               Error_Msg_N ("no match for Storage_Size operation", N);
            end if;

         else
            null; -- Error will be caught in Validate_Storage_Model_Type_Aspect
         end if;

      else
         --  Overloaded case: find subprogram with proper signature

         declare
            I  : Interp_Index;
            It : Interp;
            Found_Match : Boolean := False;

         begin
            Get_First_Interp (N, I, It);
            while Present (It.Typ) loop
               if Ekind (It.Nam) in E_Function | E_Procedure
                  and then Scope (It.Nam) = Scope (Typ)
               then
                  if Nam = Name_Allocate then
                     Found_Match :=
                       Aspect_Argument_Profile_Matches
                         (It.Nam,
                          Profiles        =>
                            ((Typ,                E_In_Out_Parameter),
                             (Addr_Type,          E_Out_Parameter),
                             (Storage_Count_Type, E_In_Parameter),
                             (Storage_Count_Type, E_In_Parameter)),
                          Result_Subt     => Empty,
                          Err_On_Mismatch => False);

                  elsif Nam = Name_Deallocate then
                     Found_Match :=
                       Aspect_Argument_Profile_Matches
                         (It.Nam,
                          Profiles        =>
                            ((Typ,                E_In_Out_Parameter),
                             (Addr_Type,          E_In_Parameter),
                             (Storage_Count_Type, E_In_Parameter),
                             (Storage_Count_Type, E_In_Parameter)),
                          Result_Subt     => Empty,
                          Err_On_Mismatch => False);

                  elsif Nam = Name_Copy_From then
                     Found_Match :=
                       Aspect_Argument_Profile_Matches
                         (It.Nam,
                          Profiles        =>
                            ((Typ,                 E_In_Out_Parameter),
                             (System_Address_Type, E_In_Parameter),
                             (Addr_Type,           E_In_Parameter),
                             (Storage_Count_Type,  E_In_Parameter),
                             (Storage_Count_Type,  E_In_Parameter)),
                          Result_Subt     => Empty,
                          Err_On_Mismatch => False);

                  elsif Nam = Name_Copy_To then
                     Found_Match :=
                       Aspect_Argument_Profile_Matches
                         (It.Nam,
                          Profiles        =>
                            ((Typ,                 E_In_Out_Parameter),
                             (Addr_Type,           E_In_Parameter),
                             (Storage_Count_Type,  E_In_Parameter),
                             (System_Address_Type, E_In_Parameter),
                             (Storage_Count_Type,  E_In_Parameter)),
                          Result_Subt     => Empty,
                          Err_On_Mismatch => False);

                  elsif Nam = Name_Storage_Size then
                     Found_Match :=
                       Aspect_Argument_Profile_Matches
                         (It.Nam,
                          Profiles        => (1 => (Typ, E_In_Parameter)),
                          Result_Subt     => Storage_Count_Type,
                          Err_On_Mismatch => False);
                  end if;

                  if Found_Match then
                     Set_Entity (N, It.Nam);
                     exit;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;

            if not Found_Match then
               Error_Msg_N
                 ("no match found for Storage_Model_Type operation", N);
            end if;
         end;
      end if;
   end Resolve_Storage_Model_Type_Argument;

   ----------------
   -- Set_Biased --
   ----------------

   procedure Set_Biased
     (E      : Entity_Id;
      N      : Node_Id;
      Msg    : String;
      Biased : Boolean := True)
   is
   begin
      if Biased then
         Set_Has_Biased_Representation (E);

         if Warn_On_Biased_Representation then
            Error_Msg_NE
              ("?.b?" & Msg & " forces biased representation for&", N, E);
         end if;
      end if;
   end Set_Biased;

   --------------------
   -- Set_Enum_Esize --
   --------------------

   procedure Set_Enum_Esize (T : Entity_Id) is
      Lo : Uint;
      Hi : Uint;
      Sz : Unat;

   begin
      --  Find the minimum standard size (8,16,32,64,128) that fits

      Lo := Enumeration_Rep (Entity (Type_Low_Bound (T)));
      Hi := Enumeration_Rep (Entity (Type_High_Bound (T)));

      if Lo < 0 then
         if Lo >= -Uint_2**7 and then Hi < Uint_2**7 then
            Sz := UI_From_Int (Standard_Character_Size);
            --  Might be > 8 on some targets

         elsif Lo >= -Uint_2**15 and then Hi < Uint_2**15 then
            Sz := Uint_16;

         elsif Lo >= -Uint_2**31 and then Hi < Uint_2**31 then
            Sz := Uint_32;

         elsif Lo >= -Uint_2**63 and then Hi < Uint_2**63 then
            Sz := Uint_64;

         else pragma Assert (Lo >= -Uint_2**127 and then Hi < Uint_2**127);
            Sz := Uint_128;
         end if;

      else
         if Hi < Uint_2**8 then
            Sz := UI_From_Int (Standard_Character_Size);

         elsif Hi < Uint_2**16 then
            Sz := Uint_16;

         elsif Hi < Uint_2**32 then
            Sz := Uint_32;

         elsif Hi < Uint_2**64 then
            Sz := Uint_64;

         else pragma Assert (Hi < Uint_2**128);
            Sz := Uint_128;
         end if;
      end if;

      --  That minimum is the proper size unless we have a foreign convention
      --  and the size required is 32 or less, in which case we bump the size
      --  up to 32. This is required for C and C++ and seems reasonable for
      --  all other foreign conventions.

      if Has_Foreign_Convention (T)
        and then Esize (T) < Standard_Integer_Size

        --  Don't do this if Short_Enums on target

        and then not Target_Short_Enums
      then
         Set_Esize (T, UI_From_Int (Standard_Integer_Size));
      else
         Set_Esize (T, Sz);
      end if;
   end Set_Enum_Esize;

   -----------------------------
   -- Uninstall_Discriminants --
   -----------------------------

   procedure Uninstall_Discriminants (E : Entity_Id) is
      Disc  : Entity_Id;
      Prev  : Entity_Id;
      Outer : Entity_Id;

   begin
      --  Discriminants have been made visible for type declarations and
      --  protected type declarations, not for subtype declarations.

      if Nkind (Parent (E)) /= N_Subtype_Declaration then
         Disc := First_Discriminant (E);
         while Present (Disc) loop
            if Disc /= Current_Entity (Disc) then
               Prev := Current_Entity (Disc);
               while Present (Prev)
                 and then Present (Homonym (Prev))
                 and then Homonym (Prev) /= Disc
               loop
                  Prev := Homonym (Prev);
               end loop;
            else
               Prev := Empty;
            end if;

            Set_Is_Immediately_Visible (Disc, False);

            Outer := Homonym (Disc);
            while Present (Outer) and then Scope (Outer) = E loop
               Outer := Homonym (Outer);
            end loop;

            --  Reset homonym link of other entities, but do not modify link
            --  between entities in current scope, so that the back end can
            --  have a proper count of local overloadings.

            if No (Prev) then
               Set_Name_Entity_Id (Chars (Disc), Outer);

            elsif Scope (Prev) /= Scope (Disc) then
               Set_Homonym (Prev,  Outer);
            end if;

            Next_Discriminant (Disc);
         end loop;
      end if;
   end Uninstall_Discriminants;

   ------------------------------
   -- Validate_Address_Clauses --
   ------------------------------

   procedure Validate_Address_Clauses is
      function Offset_Value (Expr : Node_Id) return Uint;
      --  Given an Address attribute reference, return the value in bits of its
      --  offset from the first bit of the underlying entity, or 0 if it is not
      --  known at compile time.

      ------------------
      -- Offset_Value --
      ------------------

      function Offset_Value (Expr : Node_Id) return Uint is
         N   : Node_Id := Prefix (Expr);
         Off : Uint;
         Val : Uint := Uint_0;

      begin
         --  Climb the prefix chain and compute the cumulative offset

         loop
            if Is_Entity_Name (N) then
               return Val;

            elsif Nkind (N) = N_Selected_Component then
               Off := Component_Bit_Offset (Entity (Selector_Name (N)));
               if Present (Off) and then Off >= Uint_0 then
                  Val := Val + Off;
                  N   := Prefix (N);
               else
                  return Uint_0;
               end if;

            elsif Nkind (N) = N_Indexed_Component then
               Off := Indexed_Component_Bit_Offset (N);
               if Present (Off) then
                  Val := Val + Off;
                  N   := Prefix (N);
               else
                  return Uint_0;
               end if;

            else
               return Uint_0;
            end if;
         end loop;
      end Offset_Value;

   --  Start of processing for Validate_Address_Clauses

   begin
      for J in Address_Clause_Checks.First .. Address_Clause_Checks.Last loop
         declare
            ACCR : Address_Clause_Check_Record
                     renames Address_Clause_Checks.Table (J);

            Expr : Node_Id;

            X_Alignment : Uint;
            Y_Alignment : Uint := Uint_0;

            X_Size : Uint;
            Y_Size : Uint := Uint_0;

            X_Offs : Uint;

         begin
            --  Skip processing of this entry if warning already posted, or if
            --  alignments are not set.

            if not Address_Warning_Posted (ACCR.N)
              and then Known_Alignment (ACCR.X)
              and then Known_Alignment (ACCR.Y)
            then
               Expr := Original_Node (Expression (ACCR.N));

               --  Get alignments, sizes and offset, if any

               X_Alignment := Alignment (ACCR.X);
               X_Size      := Esize (ACCR.X);

               if Present (ACCR.Y) then
                  Y_Alignment := Alignment (ACCR.Y);
                  Y_Size :=
                    (if Known_Esize (ACCR.Y) then Esize (ACCR.Y) else Uint_0);
               end if;

               if ACCR.Off
                 and then Nkind (Expr) = N_Attribute_Reference
                 and then Attribute_Name (Expr) = Name_Address
               then
                  X_Offs := Offset_Value (Expr);
               else
                  X_Offs := Uint_0;
               end if;

               --  Check for known value not multiple of alignment

               if No (ACCR.Y) then
                  if not Alignment_Checks_Suppressed (ACCR)
                    and then X_Alignment /= 0
                    and then ACCR.A mod X_Alignment /= 0
                  then
                     Error_Msg_NE
                       ("??specified address for& is inconsistent with "
                        & "alignment", ACCR.N, ACCR.X);
                     Error_Msg_N
                       ("\??program execution may be erroneous (RM 13.3(27))",
                        ACCR.N);

                     Error_Msg_Uint_1 := X_Alignment;
                     Error_Msg_NE ("\??alignment of & is ^", ACCR.N, ACCR.X);
                  end if;

               --  Check for large object overlaying smaller one

               elsif Y_Size > Uint_0
                 and then X_Size > Uint_0
                 and then X_Offs + X_Size > Y_Size
               then
                  Error_Msg_NE ("??& overlays smaller object", ACCR.N, ACCR.X);
                  Error_Msg_N
                    ("\??program execution may be erroneous", ACCR.N);

                  Error_Msg_Uint_1 := X_Size;
                  Error_Msg_NE ("\??size of & is ^", ACCR.N, ACCR.X);

                  Error_Msg_Uint_1 := Y_Size;
                  Error_Msg_NE ("\??size of & is ^", ACCR.N, ACCR.Y);

                  if Y_Size >= X_Size then
                     Error_Msg_Uint_1 := X_Offs;
                     Error_Msg_NE ("\??but offset of & is ^", ACCR.N, ACCR.X);
                  end if;

               --  Check for inadequate alignment, both of the base object
               --  and of the offset, if any. We only do this check if the
               --  run-time Alignment_Check is active. No point in warning
               --  if this check has been suppressed (or is suppressed by
               --  default in the non-strict alignment machine case).

               --  Note: we do not check the alignment if we gave a size
               --  warning, since it would likely be redundant.

               elsif not Alignment_Checks_Suppressed (ACCR)
                 and then Y_Alignment /= Uint_0
                 and then
                   (Y_Alignment < X_Alignment
                     or else
                       (ACCR.Off
                         and then Nkind (Expr) = N_Attribute_Reference
                         and then Attribute_Name (Expr) = Name_Address
                         and then Has_Compatible_Alignment
                                    (ACCR.X, Prefix (Expr), True) /=
                                      Known_Compatible))
               then
                  Error_Msg_NE
                    ("??specified address for& may be inconsistent with "
                     & "alignment", ACCR.N, ACCR.X);
                  Error_Msg_N
                    ("\??program execution may be erroneous (RM 13.3(27))",
                     ACCR.N);

                  Error_Msg_Uint_1 := X_Alignment;
                  Error_Msg_NE ("\??alignment of & is ^", ACCR.N, ACCR.X);

                  Error_Msg_Uint_1 := Y_Alignment;
                  Error_Msg_NE ("\??alignment of & is ^", ACCR.N, ACCR.Y);

                  if Y_Alignment >= X_Alignment then
                     Error_Msg_N
                       ("\??but offset is not multiple of alignment", ACCR.N);
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Validate_Address_Clauses;

   ---------------------------------
   -- Validate_Finalizable_Aspect --
   ---------------------------------

   procedure Validate_Finalizable_Aspect (Typ : Entity_Id; ASN : Node_Id) is
      Aggr : constant Node_Id := Expression (ASN);

      Assoc : Node_Id;
      Exp   : Node_Id;
      Nam   : Node_Id;

   begin
      if not Is_Record_Type (Typ) then
         Error_Msg_N
           ("aspect Finalizable can only be specified for a record type", ASN);
         return;

      elsif Is_Derived_Type (Typ) then
         Error_Msg_N
           ("aspect Finalizable cannot be specified for a derived type", ASN);
         return;

      elsif Nkind (Aggr) /= N_Aggregate then
         Error_Msg_N ("aspect Finalizable must be an aggregate", Aggr);
         return;

      elsif not Is_Empty_List (Expressions (Aggr)) then
         Error_Msg_N
           ("illegal positional association", First (Expressions (Aggr)));
         return;
      end if;

      Set_Is_Controlled_Active (Typ);

      --  Relaxed_Finalization is optional and set True if not specified

      Set_Has_Relaxed_Finalization (Typ);

      Assoc := First (Component_Associations (Aggr));
      while Present (Assoc) loop
         Nam := First (Choices (Assoc));
         Exp := Expression (Assoc);

         if Nkind (Nam) /= N_Identifier or else Present (Next (Nam)) then
            Error_Msg_N ("illegal name in association", Nam);

         elsif Chars (Nam) in Name_Initialize | Name_Adjust | Name_Finalize
         then
            Analyze (Exp);
            Resolve_Finalizable_Argument (Exp, Typ, Chars (Nam));

         elsif Chars (Nam) = Name_Relaxed_Finalization then
            if Inside_A_Generic then
               Preanalyze_And_Resolve (Exp, Any_Boolean);
            else
               Analyze (Exp);
               Resolve_Finalizable_Argument (Exp, Typ, Chars (Nam));
            end if;

         else
            Error_Msg_N ("invalid argument for Finalizable aspect", Nam);
         end if;

         Next (Assoc);
      end loop;

      --  If Relaxed_Finalization is set, the Finalize and Adjust procedures
      --  are considered as having the No_Raise aspect specified.

      if Serious_Errors_Detected > 0 then
         null;

      elsif Has_Relaxed_Finalization (Typ) then
         Assoc := First (Component_Associations (Aggr));
         while Present (Assoc) loop
            Nam := First (Choices (Assoc));
            Exp := Expression (Assoc);

            if Chars (Nam) in Name_Adjust | Name_Finalize then
               pragma Assert (Is_Entity_Name (Exp));
               Set_No_Raise (Entity (Exp));
            end if;

            Next (Assoc);
         end loop;

      --  If Relaxed_Finalization is not set, then check that the support for
      --  strict finalization is available in the runtime library.

      elsif not In_Predefined_Unit (Cunit (Get_Source_Unit (Typ)))
        and then not RTE_Available (RE_Finalization_Master)
      then
         Error_Msg_N
           ("only Relaxed Finalization is supported in this configuration",
            ASN);
      end if;
   end Validate_Finalizable_Aspect;

   ------------------------------
   -- Validate_Iterable_Aspect --
   ------------------------------

   procedure Validate_Iterable_Aspect (Typ : Entity_Id; ASN : Node_Id) is
      Aggr  : constant Node_Id := Expression (ASN);
      Assoc : Node_Id;
      Expr  : Node_Id;

      Prim   : Node_Id;
      Cursor : Entity_Id;

      First_Id       : Entity_Id := Empty;
      Last_Id        : Entity_Id := Empty;
      Next_Id        : Entity_Id := Empty;
      Has_Element_Id : Entity_Id := Empty;
      Element_Id     : Entity_Id := Empty;

   begin
      if Nkind (Aggr) /= N_Aggregate then
         Error_Msg_N ("aspect Iterable must be an aggregate", Aggr);
         return;
      end if;

      Cursor := Get_Cursor_Type (ASN, Typ);

      --  If previous error aspect is unusable

      if Cursor = Any_Type then
         return;
      end if;

      if not Is_Empty_List (Expressions (Aggr)) then
         Error_Msg_N
           ("illegal positional association", First (Expressions (Aggr)));
      end if;

      --  Each expression must resolve to a function with the proper signature

      Assoc := First (Component_Associations (Aggr));
      while Present (Assoc) loop
         Expr := Expression (Assoc);
         Analyze (Expr);

         Prim := First (Choices (Assoc));

         if Nkind (Prim) /= N_Identifier or else Present (Next (Prim)) then
            Error_Msg_N ("illegal name in association", Prim);

         elsif Chars (Prim) = Name_First then
            Resolve_Iterable_Operation (Expr, Cursor, Typ, Name_First);
            First_Id := Entity (Expr);

         elsif Chars (Prim) = Name_Last then
            Resolve_Iterable_Operation (Expr, Cursor, Typ, Name_Last);
            Last_Id := Entity (Expr);

         elsif Chars (Prim) = Name_Previous then
            Resolve_Iterable_Operation (Expr, Cursor, Typ, Name_Previous);
            Last_Id := Entity (Expr);

         elsif Chars (Prim) = Name_Next then
            Resolve_Iterable_Operation (Expr, Cursor, Typ, Name_Next);
            Next_Id := Entity (Expr);

         elsif Chars (Prim) = Name_Has_Element then
            Resolve_Iterable_Operation (Expr, Cursor, Typ, Name_Has_Element);
            Has_Element_Id := Entity (Expr);

         elsif Chars (Prim) = Name_Element then
            Resolve_Iterable_Operation (Expr, Cursor, Typ, Name_Element);
            Element_Id := Entity (Expr);

         else
            Error_Msg_N ("invalid name for iterable function", Prim);
         end if;

         Next (Assoc);
      end loop;

      if No (First_Id) then
         Error_Msg_N ("match for First primitive not found", ASN);

      elsif No (Next_Id) then
         Error_Msg_N ("match for Next primitive not found", ASN);

      elsif No (Has_Element_Id) then
         Error_Msg_N ("match for Has_Element primitive not found", ASN);

      elsif No (Element_Id) or else No (Last_Id) then
         null;  --  optional
      end if;
   end Validate_Iterable_Aspect;

   ------------------------------
   -- Validate_Literal_Aspect --
   ------------------------------

   procedure Validate_Literal_Aspect (Typ : Entity_Id; ASN : Node_Id) is
      A_Id        : constant Aspect_Id := Get_Aspect_Id (ASN);
      pragma Assert (A_Id in Aspect_Integer_Literal |
                             Aspect_Real_Literal | Aspect_String_Literal);
      Func_Name   : constant Node_Id := Expression (ASN);
      Overloaded  : Boolean := Is_Overloaded (Func_Name);

      I            : Interp_Index := 0;
      It           : Interp;
      Param_Type   : Entity_Id;
      Match_Found  : Boolean := False;
      Match2_Found : Boolean := False;
      Is_Match     : Boolean;
      Match        : Interp;
      Match2       : Entity_Id := Empty;

      function Matching
        (Param_Id : Entity_Id; Param_Type : Entity_Id) return Boolean;
      --  Return True if Param_Id is a non aliased in parameter whose base type
      --  is Param_Type.

      --------------
      -- Matching --
      --------------

      function Matching
        (Param_Id : Entity_Id; Param_Type : Entity_Id) return Boolean is
      begin
         return Base_Type (Etype (Param_Id)) = Param_Type
           and then Ekind (Param_Id) = E_In_Parameter
           and then not Is_Aliased (Param_Id);
      end Matching;

   begin
      if not Is_Type (Typ) then
         Error_Msg_N ("aspect can only be specified for a type", ASN);
         return;

      elsif not Is_First_Subtype (Typ) then
         Error_Msg_N ("aspect cannot be specified for a subtype", ASN);
         return;
      end if;

      if A_Id = Aspect_String_Literal then
         if Is_String_Type (Typ) then
            Error_Msg_N ("aspect cannot be specified for a string type", ASN);
            return;
         end if;

         Param_Type := Standard_Wide_Wide_String;

      else
         if Is_Numeric_Type (Typ) then
            Error_Msg_N ("aspect cannot be specified for a numeric type", ASN);
            return;
         end if;

         Param_Type := Standard_String;
      end if;

      if not Overloaded and then No (Entity (Func_Name)) then
         --  The aspect is specified by a subprogram name, which
         --  may be an operator name given originally by a string.

         if Is_Operator_Name (Chars (Func_Name)) then
            Analyze_Operator_Symbol (Func_Name);
         else
            Analyze (Func_Name);
         end if;

         Overloaded := Is_Overloaded (Func_Name);
      end if;

      if Overloaded then
         Get_First_Interp (Func_Name, I => I, It => It);
      else
         --  only one possible interpretation
         It.Nam := Entity (Func_Name);
         pragma Assert (Present (It.Nam));
      end if;

      while It.Nam /= Empty loop
         Is_Match := False;

         if Ekind (It.Nam) = E_Function
           and then Base_Type (Etype (It.Nam)) = Base_Type (Typ)
         then
            declare
               Params     : constant List_Id :=
                 Parameter_Specifications (Parent (It.Nam));
               Param_Spec : Node_Id;

            begin
               if List_Length (Params) = 1 then
                  Param_Spec := First (Params);
                  Is_Match :=
                    Matching (Defining_Identifier (Param_Spec), Param_Type);

               --  Look for the optional overloaded 2-param Real_Literal

               elsif List_Length (Params) = 2
                 and then A_Id = Aspect_Real_Literal
               then
                  Param_Spec := First (Params);

                  if Matching (Defining_Identifier (Param_Spec), Param_Type)
                  then
                     Param_Spec := Next (Param_Spec);

                     if Matching (Defining_Identifier (Param_Spec), Param_Type)
                     then
                        if No (Match2) then
                           Match2 := It.Nam;
                           Match2_Found := True;
                        else
                           --  If we find more than one possible match then
                           --  do not take any into account here: since the
                           --  2-parameter version of Real_Literal is optional
                           --  we cannot generate an error here, so let
                           --  standard resolution fail later if we do need to
                           --  call this variant.

                           Match2_Found := False;
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end if;

         if Is_Match then
            if Match_Found then
               Error_Msg_N ("aspect specification is ambiguous", ASN);
               return;
            end if;

            Match_Found := True;
            Match := It;
         end if;

         exit when not Overloaded;

         if not Is_Match then
            Remove_Interp (I => I);
         end if;

         Get_Next_Interp (I => I, It => It);
      end loop;

      if not Match_Found then
         Error_Msg_N
           ("function name in aspect specification cannot be resolved", ASN);
         return;
      end if;

      Set_Entity (Func_Name, Match.Nam);
      Set_Etype (Func_Name, Etype (Match.Nam));
      Set_Is_Overloaded (Func_Name, False);

      --  Record the match for 2-parameter function if found

      if Match2_Found then
         Set_Related_Expression (Match.Nam, Match2);
      end if;
   end Validate_Literal_Aspect;

   ----------------------------------------
   -- Validate_Storage_Model_Type_Aspect --
   ----------------------------------------

   procedure Validate_Storage_Model_Type_Aspect
     (Typ : Entity_Id; ASN : Node_Id)
   is
      Assoc       : Node_Id;
      Choice      : Entity_Id;
      Choice_Name : Name_Id;
      Expr        : Node_Id;

      Address_Type_Id : Entity_Id := Empty;
      Null_Address_Id : Entity_Id := Empty;
      Allocate_Id     : Entity_Id := Empty;
      Deallocate_Id   : Entity_Id := Empty;
      Copy_From_Id    : Entity_Id := Empty;
      Copy_To_Id      : Entity_Id := Empty;
      Storage_Size_Id : Entity_Id := Empty;

      procedure Check_And_Resolve_Storage_Model_Type_Argument
        (Expr        : Node_Id;
         Typ         : Entity_Id;
         Argument_Id : in out Entity_Id;
         Nam         : Name_Id);
      --  Checks that the subaspect for Nam has not already been specified for
      --  Typ's Storage_Model_Type aspect (i.e., checks Argument_Id = Empty),
      --  resolves Expr, and sets Argument_Id to the entity resolved for Expr.

      procedure Check_And_Resolve_Storage_Model_Type_Argument
        (Expr        : Node_Id;
         Typ         : Entity_Id;
         Argument_Id : in out Entity_Id;
         Nam         : Name_Id)
      is
         Name_String : String := Get_Name_String (Nam);

      begin
         To_Mixed (Name_String);

         if Present (Argument_Id) then
            Error_Msg_String (1 .. Name_String'Length) := Name_String;
            Error_Msg_Strlen := Name_String'Length;

            Error_Msg_N ("~ already specified", Expr);
         end if;

         Resolve_Storage_Model_Type_Argument (Expr, Typ, Address_Type_Id, Nam);
         Argument_Id := Entity (Expr);
      end Check_And_Resolve_Storage_Model_Type_Argument;

   --  Start of processing for Validate_Storage_Model_Type_Aspect

   begin
      --  The aggregate argument of Storage_Model_Type is optional, and when
      --  not present the aspect defaults to the native storage model (where
      --  the address type is System.Address, and other arguments default to
      --  the corresponding native storage operations).

      if No (Expression (ASN)) then
         return;
      end if;

      --  Each expression must resolve to an entity of the right kind or proper
      --  profile.

      Assoc := First (Component_Associations (Expression (ASN)));
      while Present (Assoc) loop
         Expr := Expression (Assoc);
         Analyze (Expr);

         Choice := First (Choices (Assoc));

         Choice_Name := Chars (Choice);

         if Nkind (Choice) /= N_Identifier or else Present (Next (Choice)) then
            Error_Msg_N ("illegal name in association", Choice);

         elsif Choice_Name = Name_Address_Type then
            if Assoc /= First (Component_Associations (Expression (ASN))) then
               Error_Msg_N ("Address_Type must be first association", Choice);
            end if;

            Check_And_Resolve_Storage_Model_Type_Argument
              (Expr, Typ, Address_Type_Id, Name_Address_Type);

         else
            --  It's allowed to leave out the Address_Type argument, in which
            --  case the address type is defined to default to System.Address.

            if No (Address_Type_Id) then
               Address_Type_Id := RTE (RE_Address);
            end if;

            if Choice_Name = Name_Null_Address then
               Check_And_Resolve_Storage_Model_Type_Argument
                 (Expr, Typ, Null_Address_Id, Name_Null_Address);

            elsif Choice_Name = Name_Allocate then
               Check_And_Resolve_Storage_Model_Type_Argument
                 (Expr, Typ, Allocate_Id, Name_Allocate);

            elsif Choice_Name = Name_Deallocate then
               Check_And_Resolve_Storage_Model_Type_Argument
                 (Expr, Typ, Deallocate_Id, Name_Deallocate);

            elsif Choice_Name = Name_Copy_From then
               Check_And_Resolve_Storage_Model_Type_Argument
                 (Expr, Typ, Copy_From_Id, Name_Copy_From);

            elsif Choice_Name = Name_Copy_To then
               Check_And_Resolve_Storage_Model_Type_Argument
                 (Expr, Typ, Copy_To_Id, Name_Copy_To);

            elsif Choice_Name = Name_Storage_Size then
               Check_And_Resolve_Storage_Model_Type_Argument
                 (Expr, Typ, Storage_Size_Id, Name_Storage_Size);

            else
               Error_Msg_N
                 ("invalid name for Storage_Model_Type argument", Choice);
            end if;
         end if;

         Next (Assoc);
      end loop;

      --  If Address_Type has been specified as or defaults to System.Address,
      --  then other "subaspect" arguments can be specified, but are optional.
      --  Otherwise, all other arguments are required and an error is flagged
      --  about any that are missing.

      if Address_Type_Id = RTE (RE_Address) then
         return;

      elsif No (Null_Address_Id) then
         Error_Msg_N ("match for Null_Address primitive not found", ASN);

      elsif No (Allocate_Id) then
         Error_Msg_N ("match for Allocate primitive not found", ASN);

      elsif No (Deallocate_Id) then
         Error_Msg_N ("match for Deallocate primitive not found", ASN);

      elsif No (Copy_From_Id) then
         Error_Msg_N ("match for Copy_From primitive not found", ASN);

      elsif No (Copy_To_Id) then
         Error_Msg_N ("match for Copy_To primitive not found", ASN);

      elsif No (Storage_Size_Id) then
         Error_Msg_N ("match for Storage_Size primitive not found", ASN);
      end if;
   end Validate_Storage_Model_Type_Aspect;

   -----------------------------------
   -- Validate_Unchecked_Conversion --
   -----------------------------------

   procedure Validate_Unchecked_Conversion
     (N        : Node_Id;
      Act_Unit : Entity_Id)
   is
      Source : Entity_Id;
      Target : Entity_Id;

      procedure Warn_Nonportable (RE : RE_Id);
      --  Warn if either source or target of the conversion is a predefined
      --  private type, whose representation might differ between releases and
      --  targets of the compiler.

      ----------------------
      -- Warn_Nonportable --
      ----------------------

      procedure Warn_Nonportable (RE : RE_Id) is
      begin
         if Is_RTE (Source, RE) or else Is_RTE (Target, RE) then
            pragma Assert (Is_Private_Type (RTE (RE)));
            Error_Msg_NE
              ("?z?representation of & values may change between "
               & "'G'N'A'T versions", N, RTE (RE));
         end if;
      end Warn_Nonportable;

      --  Local variables

      Vnode  : Node_Id;

   --  Start of processing for Validate_Unchecked_Conversion

   begin
      --  Obtain source and target types. Note that we call Ancestor_Subtype
      --  here because the processing for generic instantiation always makes
      --  subtypes, and we want the original frozen actual types.

      Source := Ancestor_Subtype (Etype (First_Formal (Act_Unit)));
      Target := Ancestor_Subtype (Etype (Act_Unit));

      --  If either type is generic, the instantiation happens within a generic
      --  unit, and there is nothing to check. The proper check will happen
      --  when the enclosing generic is instantiated.

      if Is_Generic_Type (Source) or else Is_Generic_Type (Target) then
         return;
      end if;

      --  Warn if one of the operands is a private type declared in
      --  Ada.Calendar or Ada.Real_Time. Do not emit a warning when compiling
      --  GNAT-related sources.

      if Warn_On_Unchecked_Conversion
        and then not In_Predefined_Unit (N)
      then
         Warn_Nonportable (RO_CA_Time);
         Warn_Nonportable (RO_RT_Time);
         Warn_Nonportable (RE_Time_Span);
      end if;

      --  If we are dealing with private types, then do the check on their
      --  fully declared counterparts if the full declarations have been
      --  encountered (they don't have to be visible, but they must exist).

      if Is_Private_Type (Source)
        and then Present (Underlying_Type (Source))
      then
         Source := Underlying_Type (Source);
      end if;

      if Is_Private_Type (Target)
        and then Present (Underlying_Type (Target))
      then
         Target := Underlying_Type (Target);
      end if;

      --  Source may be unconstrained array, but not target, except in relaxed
      --  semantics mode.

      if Is_Array_Type (Target)
        and then not Is_Constrained (Target)
        and then not Relaxed_RM_Semantics
      then
         Error_Msg_N
           ("unchecked conversion to unconstrained array not allowed", N);
         return;
      end if;

      --  Warn if conversion between two different convention pointers

      if Is_Access_Type (Target)
        and then Is_Access_Type (Source)
        and then Convention (Target) /= Convention (Source)
        and then Warn_On_Unchecked_Conversion
      then
         --  Give warnings for subprogram pointers only on most targets

         if Is_Access_Subprogram_Type (Target)
           or else Is_Access_Subprogram_Type (Source)
         then
            Error_Msg_N
              ("?z?conversion between pointers with different conventions!",
               N);
         end if;
      end if;

      --  Make entry in unchecked conversion table for later processing by
      --  Validate_Unchecked_Conversions, which will check sizes and alignments
      --  (using values set by the back end where possible). This is only done
      --  if the appropriate warning is active.

      if Warn_On_Unchecked_Conversion then
         Unchecked_Conversions.Append
           (New_Val => UC_Entry'(Eloc     => Sloc (N),
                                 Source   => Source,
                                 Target   => Target,
                                 Act_Unit => Act_Unit));

         --  If both sizes are known statically now, then back-end annotation
         --  is not required to do a proper check but if either size is not
         --  known statically, then we need the annotation.

         if Known_Static_RM_Size (Source)
              and then
            Known_Static_RM_Size (Target)
         then
            null;
         else
            Back_Annotate_Rep_Info := True;
         end if;
      end if;

      --  If unchecked conversion to access type, and access type is declared
      --  in the same unit as the unchecked conversion, then set the flag
      --  No_Strict_Aliasing (no strict aliasing is implicit here)

      if Is_Access_Type (Target)
        and then In_Same_Source_Unit (Target, N)
      then
         Set_No_Strict_Aliasing (Implementation_Base_Type (Target));
      end if;

      --  For code generators that do not support nested subprograms, if the
      --  unchecked conversion is between Address and an access subprogram
      --  type, show that we shouldn't use an internal representation for the
      --  access subprogram type.

      if Unnest_Subprogram_Mode then
         if Is_Access_Subprogram_Type (Target)
           and then Is_Descendant_Of_Address (Source)
           and then In_Same_Source_Unit (Target, N)
         then
            Set_Can_Use_Internal_Rep (Base_Type (Target), False);
         elsif Is_Access_Subprogram_Type (Source)
           and then Is_Descendant_Of_Address (Target)
           and then In_Same_Source_Unit (Source, N)
         then
            Set_Can_Use_Internal_Rep (Base_Type (Source), False);
         end if;
      end if;

      --  Generate N_Validate_Unchecked_Conversion node for back end in case
      --  the back end needs to perform special validation checks.

      --  Shouldn't this be in Exp_Ch13, since the check only gets done if we
      --  have full expansion and the back end is called ???

      Vnode :=
        Make_Validate_Unchecked_Conversion (Sloc (N));
      Set_Source_Type (Vnode, Source);
      Set_Target_Type (Vnode, Target);

      --  If the unchecked conversion node is in a list, just insert before it.
      --  If not we have some strange case, not worth bothering about.

      if Is_List_Member (N) then
         Insert_After (N, Vnode);
      end if;
   end Validate_Unchecked_Conversion;

   ------------------------------------
   -- Validate_Unchecked_Conversions --
   ------------------------------------

   procedure Validate_Unchecked_Conversions is
      function Is_Null_Array (T : Entity_Id) return Boolean;
      --  We want to warn in the case of converting to a wrong-sized array of
      --  bytes, including the zero-size case. This returns True in that case,
      --  which is necessary because a size of 0 is used to indicate both an
      --  unknown size and a size of 0. It's OK for this to return True in
      --  other zero-size cases, but we don't go out of our way; for example,
      --  we don't bother with multidimensional arrays.

      function Is_Null_Array (T : Entity_Id) return Boolean is
      begin
         if Is_Array_Type (T) and then Is_Constrained (T) then
            declare
               Index : constant Node_Id := First_Index (T);
               R : Node_Id; -- N_Range
            begin
               case Nkind (Index) is
                  when N_Range =>
                     R := Index;
                  when N_Subtype_Indication =>
                     R := Range_Expression (Constraint (Index));
                  when N_Identifier | N_Expanded_Name =>
                     R := Scalar_Range (Entity (Index));
                  when others =>
                     raise Program_Error;
               end case;

               return Is_Null_Range (Low_Bound (R), High_Bound (R));
            end;
         end if;

         return False;
      end Is_Null_Array;

   begin
      for N in Unchecked_Conversions.First .. Unchecked_Conversions.Last loop
         declare
            T : UC_Entry renames Unchecked_Conversions.Table (N);

            Act_Unit : constant Entity_Id  := T.Act_Unit;
            Eloc     : constant Source_Ptr := T.Eloc;
            Source   : constant Entity_Id  := T.Source;
            Target   : constant Entity_Id  := T.Target;

            Source_Siz : Uint;
            Target_Siz : Uint;

         begin
            --  Skip if function marked as warnings off

            if Has_Warnings_Off (Act_Unit)
              or else Serious_Errors_Detected > 0
            then
               goto Continue;
            end if;

           --  Don't do the check if warnings off for either type, note the
           --  deliberate use of OR here instead of OR ELSE to get the flag
           --  Warnings_Off_Used set for both types if appropriate.

            if Has_Warnings_Off (Source) or Has_Warnings_Off (Target) then
               goto Continue;
            end if;

            if (Known_Static_RM_Size (Source)
                  and then Known_Static_RM_Size (Target))
              or else Is_Null_Array (Target)
            then
               --  This validation check, which warns if we have unequal sizes
               --  for unchecked conversion, and thus implementation dependent
               --  semantics, is one of the few occasions on which we use the
               --  official RM size instead of Esize. See description in Einfo
               --  "Handling of Type'Size Values" for details.

               Source_Siz := RM_Size (Source);
               Target_Siz := RM_Size (Target);

               if Present (Source_Siz) and then Present (Target_Siz)
                 and then Source_Siz /= Target_Siz
               then
                  Error_Msg
                    ("?z?types for unchecked conversion have different sizes!",
                     Eloc, Act_Unit);

                  if All_Errors_Mode then
                     Error_Msg_Name_1 := Chars (Source);
                     Error_Msg_Uint_1 := Source_Siz;
                     Error_Msg_Name_2 := Chars (Target);
                     Error_Msg_Uint_2 := Target_Siz;
                     Error_Msg
                       ("\size of % is ^, size of % is ^?z?", Eloc, Act_Unit);

                     Error_Msg_Uint_1 := UI_Abs (Source_Siz - Target_Siz);

                     if Is_Discrete_Type (Source)
                          and then
                        Is_Discrete_Type (Target)
                     then
                        if Source_Siz > Target_Siz then
                           Error_Msg
                             ("\?z?^ high order bits of source will "
                              & "be ignored!", Eloc, Act_Unit);

                        elsif Is_Unsigned_Type (Source) then
                           Error_Msg
                             ("\?z?source will be extended with ^ high order "
                              & "zero bits!", Eloc, Act_Unit);

                        else
                           Error_Msg
                             ("\?z?source will be extended with ^ high order "
                              & "sign bits!", Eloc, Act_Unit);
                        end if;

                     elsif Source_Siz < Target_Siz then
                        if Is_Discrete_Type (Target) then
                           if Bytes_Big_Endian then
                              Error_Msg
                                ("\?z?target value will include ^ undefined "
                                 & "low order bits!", Eloc, Act_Unit);
                           else
                              Error_Msg
                                ("\?z?target value will include ^ undefined "
                                 & "high order bits!", Eloc, Act_Unit);
                           end if;

                        else
                           Error_Msg
                             ("\?z?^ trailing bits of target value will be "
                              & "undefined!", Eloc, Act_Unit);
                        end if;

                     else pragma Assert (Source_Siz > Target_Siz);
                        if Is_Discrete_Type (Source) then
                           if Bytes_Big_Endian then
                              Error_Msg
                                ("\?z?^ low order bits of source will be "
                                 & "ignored!", Eloc, Act_Unit);
                           else
                              Error_Msg
                                ("\?z?^ high order bits of source will be "
                                 & "ignored!", Eloc, Act_Unit);
                           end if;

                        else
                           Error_Msg
                             ("\?z?^ trailing bits of source will be "
                              & "ignored!", Eloc, Act_Unit);
                        end if;
                     end if;
                  end if;
               end if;
            end if;

            --  If both types are access types, we need to check the alignment.
            --  If the alignment of both is specified, we can do it here.

            if Serious_Errors_Detected = 0
              and then Is_Access_Type (Source)
              and then Is_Access_Type (Target)
              and then Target_Strict_Alignment
              and then Present (Designated_Type (Source))
              and then Present (Designated_Type (Target))
            then
               declare
                  D_Source : constant Entity_Id := Designated_Type (Source);
                  D_Target : constant Entity_Id := Designated_Type (Target);

               begin
                  if Known_Alignment (D_Source)
                       and then
                     Known_Alignment (D_Target)
                  then
                     declare
                        Source_Align : constant Uint := Alignment (D_Source);
                        Target_Align : constant Uint := Alignment (D_Target);

                     begin
                        if Source_Align < Target_Align
                          and then not Is_Tagged_Type (D_Source)

                          --  Suppress warning if warnings suppressed on either
                          --  type or either designated type. Note the use of
                          --  OR here instead of OR ELSE. That is intentional,
                          --  we would like to set flag Warnings_Off_Used in
                          --  all types for which warnings are suppressed.

                          and then not (Has_Warnings_Off (D_Source)
                                          or
                                        Has_Warnings_Off (D_Target)
                                          or
                                        Has_Warnings_Off (Source)
                                          or
                                        Has_Warnings_Off (Target))
                        then
                           Error_Msg_Uint_1 := Target_Align;
                           Error_Msg_Uint_2 := Source_Align;
                           Error_Msg_Node_1 := D_Target;
                           Error_Msg_Node_2 := D_Source;
                           Error_Msg
                             ("?z?alignment of & (^) is stricter than "
                              & "alignment of & (^)!", Eloc, Act_Unit);
                           Error_Msg
                             ("\?z?resulting access value may have invalid "
                              & "alignment!", Eloc, Act_Unit);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;

      <<Continue>>
         null;
      end loop;
   end Validate_Unchecked_Conversions;

begin
   User_Aspect_Support.Analyze_User_Aspect_Aspect_Specification_Hook :=
     Analyze_User_Aspect_Aspect_Specification'Access;
end Sem_Ch13;
