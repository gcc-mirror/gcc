------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with Aspects;  use Aspects;
with Atree;    use Atree;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Disp; use Exp_Disp;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Case; use Sem_Case;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch9;  use Sem_Ch9;
with Sem_Dim;  use Sem_Dim;
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Ttypes;   use Ttypes;
with Tbuild;   use Tbuild;
with Urealp;   use Urealp;
with Warnsw;   use Warnsw;

with GNAT.Heap_Sort_G;

package body Sem_Ch13 is

   SSU : constant Pos := System_Storage_Unit;
   --  Convenient short hand for commonly used constant

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Alignment_Check_For_Size_Change (Typ : Entity_Id; Size : Uint);
   --  This routine is called after setting one of the sizes of type entity
   --  Typ to Size. The purpose is to deal with the situation of a derived
   --  type whose inherited alignment is no longer appropriate for the new
   --  size value. In this case, we reset the Alignment to unknown.

   procedure Build_Predicate_Functions (Typ : Entity_Id; N : Node_Id);
   --  If Typ has predicates (indicated by Has_Predicates being set for Typ,
   --  then either there are pragma Predicate entries on the rep chain for the
   --  type (note that Predicate aspects are converted to pragma Predicate), or
   --  there are inherited aspects from a parent type, or ancestor subtypes.
   --  This procedure builds the spec and body for the Predicate function that
   --  tests these predicates. N is the freeze node for the type. The spec of
   --  the function is inserted before the freeze node, and the body of the
   --  function is inserted after the freeze node. If the predicate expression
   --  has at least one Raise_Expression, then this procedure also builds the
   --  M version of the predicate function for use in membership tests.

   procedure Build_Static_Predicate
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
   --  list is stored in Static_Predicate (Typ), and the Expr is rewritten as
   --  a canonicalized membership operation.

   procedure Check_Pool_Size_Clash (Ent : Entity_Id; SP, SS : Node_Id);
   --  Called if both Storage_Pool and Storage_Size attribute definition
   --  clauses (SP and SS) are present for entity Ent. Issue error message.

   procedure Freeze_Entity_Checks (N : Node_Id);
   --  Called from Analyze_Freeze_Entity and Analyze_Generic_Freeze Entity
   --  to generate appropriate semantic checks that are delayed until this
   --  point (they had to be delayed this long for cases of delayed aspects,
   --  e.g. analysis of statically predicated subtypes in choices, for which
   --  we have to be sure the subtypes in question are frozen before checking.

   function Get_Alignment_Value (Expr : Node_Id) return Uint;
   --  Given the expression for an alignment value, returns the corresponding
   --  Uint value. If the value is inappropriate, then error messages are
   --  posted as required, and a value of No_Uint is returned.

   function Is_Operational_Item (N : Node_Id) return Boolean;
   --  A specification for a stream attribute is allowed before the full type
   --  is declared, as explained in AI-00137 and the corrigendum. Attributes
   --  that do not specify a representation characteristic are operational
   --  attributes.

   procedure New_Stream_Subprogram
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : TSS_Name_Type);
   --  Create a subprogram renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as a
   --  primitive operation, or in the non-tagged case make an appropriate TSS
   --  entry. This is more properly an expansion activity than just semantics,
   --  but the presence of user-defined stream functions for limited types is a
   --  legality check, which is why this takes place here rather than in
   --  exp_ch13, where it was previously. Nam indicates the name of the TSS
   --  function to be generated.
   --
   --  To avoid elaboration anomalies with freeze nodes, for untagged types
   --  we generate both a subprogram declaration and a subprogram renaming
   --  declaration, so that the attribute specification is handled as a
   --  renaming_as_body. For tagged types, the specification is one of the
   --  primitive specs.

   generic
      with procedure Replace_Type_Reference (N : Node_Id);
   procedure Replace_Type_References_Generic (N : Node_Id; TName : Name_Id);
   --  This is used to scan an expression for a predicate or invariant aspect
   --  replacing occurrences of the name TName (the name of the subtype to
   --  which the aspect applies) with appropriate references to the parameter
   --  of the predicate function or invariant procedure. The procedure passed
   --  as a generic parameter does the actual replacement of node N, which is
   --  either a simple direct reference to TName, or a selected component that
   --  represents an appropriately qualified occurrence of TName.

   procedure Set_Biased
     (E      : Entity_Id;
      N      : Node_Id;
      Msg    : String;
      Biased : Boolean := True);
   --  If Biased is True, sets Has_Biased_Representation flag for E, and
   --  outputs a warning message at node N if Warn_On_Biased_Representation is
   --  is True. This warning inserts the string Msg to describe the construct
   --  causing biasing.

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
      Eloc   : Source_Ptr; -- node used for posting warnings
      Source : Entity_Id;  -- source type for unchecked conversion
      Target : Entity_Id;  -- target type for unchecked conversion
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

   --  where Expr is of the form Y'Address or recursively is a reference to a
   --  constant of either of these forms, and X and Y are entities of objects,
   --  then if Y has a smaller alignment than X, that merits a warning about
   --  possible bad alignment. The following table collects address clauses of
   --  this kind. We put these in a table so that they can be checked after the
   --  back end has completed annotation of the alignments of objects, since we
   --  can catch more cases that way.

   type Address_Clause_Check_Record is record
      N : Node_Id;
      --  The address clause

      X : Entity_Id;
      --  The entity of the object overlaying Y

      Y : Entity_Id;
      --  The entity of the object being overlaid

      Off : Boolean;
      --  Whether the address is offset within Y
   end record;

   package Address_Clause_Checks is new Table.Table (
     Table_Component_Type => Address_Clause_Check_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "Address_Clause_Checks");

   -----------------------------------------
   -- Adjust_Record_For_Reverse_Bit_Order --
   -----------------------------------------

   procedure Adjust_Record_For_Reverse_Bit_Order (R : Entity_Id) is
      Comp : Node_Id;
      CC   : Node_Id;

   begin
      --  Processing depends on version of Ada

      --  For Ada 95, we just renumber bits within a storage unit. We do the
      --  same for Ada 83 mode, since we recognize the Bit_Order attribute in
      --  Ada 83, and are free to add this extension.

      if Ada_Version < Ada_2005 then
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
                          ("multi-byte field specified with non-standard"
                           & " Bit_Order??", CLC);

                        if Bytes_Big_Endian then
                           Error_Msg_N
                             ("bytes are not reversed "
                              & "(component is big-endian)??", CLC);
                        else
                           Error_Msg_N
                             ("bytes are not reversed "
                              & "(component is little-endian)??", CLC);
                        end if;

                        --  Do not allow non-contiguous field

                     else
                        Error_Msg_N
                          ("attempt to specify non-contiguous field "
                           & "not permitted", CLC);
                        Error_Msg_N
                          ("\caused by non-standard Bit_Order "
                           & "specified", CLC);
                        Error_Msg_N
                          ("\consider possibility of using "
                           & "Ada 2005 mode here", CLC);
                     end if;

                  --  Case where field fits in one storage unit

                  else
                     --  Give warning if suspicious component clause

                     if Intval (FB) >= System_Storage_Unit
                       and then Warn_On_Reverse_Bit_Order
                     then
                        Error_Msg_N
                          ("Bit_Order clause does not affect " &
                           "byte ordering?V?", Pos);
                        Error_Msg_Uint_1 :=
                          Intval (Pos) + Intval (FB) /
                          System_Storage_Unit;
                        Error_Msg_N
                          ("position normalized to ^ before bit " &
                           "order interpreted?V?", Pos);
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

                     --  The rule is that the first bit is is obtained by
                     --  subtracting the old ending bit from storage_unit - 1.

                     Set_Component_Bit_Offset
                       (Comp,
                        (Storage_Unit_Offset * System_Storage_Unit) +
                          (System_Storage_Unit - 1) -
                          (Start_Bit + CSZ - 1));

                     Set_Normalized_First_Bit
                       (Comp,
                        Component_Bit_Offset (Comp) mod
                          System_Storage_Unit);
                  end if;
               end;
            end if;

            Next_Component_Or_Discriminant (Comp);
         end loop;

      --  For Ada 2005, we do machine scalar processing, as fully described In
      --  AI-133. This involves gathering all components which start at the
      --  same byte offset and processing them together. Same approach is still
      --  valid in later versions including Ada 2012.

      else
         declare
            Max_Machine_Scalar_Size : constant Uint :=
                                        UI_From_Int
                                          (Standard_Long_Long_Integer_Size);
            --  We use this as the maximum machine scalar size

            Num_CC : Natural;
            SSU    : constant Uint := UI_From_Int (System_Storage_Unit);

         begin
            --  This first loop through components does two things. First it
            --  deals with the case of components with component clauses whose
            --  length is greater than the maximum machine scalar size (either
            --  accepting them or rejecting as needed). Second, it counts the
            --  number of components with component clauses whose length does
            --  not exceed this maximum for later processing.

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

                        --  This is allowed only if first bit is zero, and
                        --  last bit + 1 is a multiple of storage unit size.

                        if Fbit = 0 and then (Lbit + 1) mod SSU = 0 then

                           --  This is the case to give a warning if enabled

                           if Warn_On_Reverse_Bit_Order then
                              Error_Msg_N
                                ("multi-byte field specified with "
                                 & "  non-standard Bit_Order?V?", CC);

                              if Bytes_Big_Endian then
                                 Error_Msg_N
                                   ("\bytes are not reversed "
                                    & "(component is big-endian)?V?", CC);
                              else
                                 Error_Msg_N
                                   ("\bytes are not reversed "
                                    & "(component is little-endian)?V?", CC);
                              end if;
                           end if;

                        --  Give error message for RM 13.5.1(10) violation

                        else
                           Error_Msg_FE
                             ("machine scalar rules not followed for&",
                              First_Bit (CC), Comp);

                           Error_Msg_Uint_1 := Lbit;
                           Error_Msg_Uint_2 := Max_Machine_Scalar_Size;
                           Error_Msg_F
                             ("\last bit (^) exceeds maximum machine "
                              & "scalar size (^)",
                              First_Bit (CC));

                           if (Lbit + 1) mod SSU /= 0 then
                              Error_Msg_Uint_1 := SSU;
                              Error_Msg_F
                                ("\and is not a multiple of Storage_Unit (^) "
                                 & "(RM 13.4.1(10))",
                                 First_Bit (CC));

                           else
                              Error_Msg_Uint_1 := Fbit;
                              Error_Msg_F
                                ("\and first bit (^) is non-zero "
                                 & "(RM 13.4.1(10))",
                                 First_Bit (CC));
                           end if;
                        end if;

                     --  OK case of machine scalar related component clause,
                     --  For now, just count them.

                     else
                        Num_CC := Num_CC + 1;
                     end if;
                  end;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;

            --  We need to sort the component clauses on the basis of the
            --  Position values in the clause, so we can group clauses with
            --  the same Position. together to determine the relevant machine
            --  scalar size.

            Sort_CC : declare
               Comps : array (0 .. Num_CC) of Entity_Id;
               --  Array to collect component and discriminant entities. The
               --  data starts at index 1, the 0'th entry is for the sort
               --  routine.

               function CP_Lt (Op1, Op2 : Natural) return Boolean;
               --  Compare routine for Sort

               procedure CP_Move (From : Natural; To : Natural);
               --  Move routine for Sort

               package Sorting is new GNAT.Heap_Sort_G (CP_Move, CP_Lt);

               Start : Natural;
               Stop  : Natural;
               --  Start and stop positions in the component list of the set of
               --  components with the same starting position (that constitute
               --  components in a single machine scalar).

               MaxL  : Uint;
               --  Maximum last bit value of any component in this set

               MSS   : Uint;
               --  Corresponding machine scalar size

               -----------
               -- CP_Lt --
               -----------

               function CP_Lt (Op1, Op2 : Natural) return Boolean is
               begin
                  return Position (Component_Clause (Comps (Op1))) <
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
                     CC   : constant Node_Id := Component_Clause (Comp);

                  begin
                     --  Collect only component clauses whose last bit is less
                     --  than machine scalar size. Any component clause whose
                     --  last bit exceeds this value does not take part in
                     --  machine scalar layout considerations. The test for
                     --  Error_Posted makes sure we exclude component clauses
                     --  for which we already posted an error.

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

               --  We now have all the components whose size does not exceed
               --  the max machine scalar value, sorted by starting position.
               --  In this loop we gather groups of clauses starting at the
               --  same position, to process them in accordance with AI-133.

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

                  --  Now we have a group of component clauses from Start to
                  --  Stop whose positions are identical, and MaxL is the
                  --  maximum last bit value of any of these components.

                  --  We need to determine the corresponding machine scalar
                  --  size. This loop assumes that machine scalar sizes are
                  --  even, and that each possible machine scalar has twice
                  --  as many bits as the next smaller one.

                  MSS := Max_Machine_Scalar_Size;
                  while MSS mod 2 = 0
                    and then (MSS / 2) >= SSU
                    and then (MSS / 2) > MaxL
                  loop
                     MSS := MSS / 2;
                  end loop;

                  --  Here is where we fix up the Component_Bit_Offset value
                  --  to account for the reverse bit order. Some examples of
                  --  what needs to be done for the case of a machine scalar
                  --  size of 8 are:

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
                  --  the old ending bit from machine scalar size - 1.

                  for C in Start .. Stop loop
                     declare
                        Comp : constant Entity_Id := Comps (C);
                        CC   : constant Node_Id   := Component_Clause (Comp);

                        LB   : constant Uint := Static_Integer (Last_Bit (CC));
                        NFB  : constant Uint := MSS - Uint_1 - LB;
                        NLB  : constant Uint := NFB + Esize (Comp) - 1;
                        Pos  : constant Uint := Static_Integer (Position (CC));

                     begin
                        if Warn_On_Reverse_Bit_Order then
                           Error_Msg_Uint_1 := MSS;
                           Error_Msg_N
                             ("info: reverse bit order in machine " &
                              "scalar of length^?V?", First_Bit (CC));
                           Error_Msg_Uint_1 := NFB;
                           Error_Msg_Uint_2 := NLB;

                           if Bytes_Big_Endian then
                              Error_Msg_NE
                                ("\info: big-endian range for "
                                 & "component & is ^ .. ^?V?",
                                 First_Bit (CC), Comp);
                           else
                              Error_Msg_NE
                                ("\info: little-endian range "
                                 & "for component & is ^ .. ^?V?",
                                 First_Bit (CC), Comp);
                           end if;
                        end if;

                        Set_Component_Bit_Offset (Comp, Pos * SSU + NFB);
                        Set_Normalized_First_Bit (Comp, NFB mod SSU);
                     end;
                  end loop;
               end loop;
            end Sort_CC;
         end;
      end if;
   end Adjust_Record_For_Reverse_Bit_Order;

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
        and then Size mod (Alignment (Typ) * SSU) /= 0
      then
         Init_Alignment (Typ);
      end if;
   end Alignment_Check_For_Size_Change;

   -------------------------------------
   -- Analyze_Aspects_At_Freeze_Point --
   -------------------------------------

   procedure Analyze_Aspects_At_Freeze_Point (E : Entity_Id) is
      ASN   : Node_Id;
      A_Id  : Aspect_Id;
      Ritem : Node_Id;

      procedure Analyze_Aspect_Default_Value (ASN : Node_Id);
      --  This routine analyzes an Aspect_Default_[Component_]Value denoted by
      --  the aspect specification node ASN.

      procedure Inherit_Delayed_Rep_Aspects (ASN : Node_Id);
      --  As discussed in the spec of Aspects (see Aspect_Delay declaration),
      --  a derived type can inherit aspects from its parent which have been
      --  specified at the time of the derivation using an aspect, as in:
      --
      --    type A is range 1 .. 10
      --      with Size => Not_Defined_Yet;
      --    ..
      --    type B is new A;
      --    ..
      --    Not_Defined_Yet : constant := 64;
      --
      --  In this example, the Size of A is considered to be specified prior
      --  to the derivation, and thus inherited, even though the value is not
      --  known at the time of derivation. To deal with this, we use two entity
      --  flags. The flag Has_Derived_Rep_Aspects is set in the parent type (A
      --  here), and then the flag May_Inherit_Delayed_Rep_Aspects is set in
      --  the derived type (B here). If this flag is set when the derived type
      --  is frozen, then this procedure is called to ensure proper inheritance
      --  of all delayed aspects from the parent type. The derived type is E,
      --  the argument to Analyze_Aspects_At_Freeze_Point. ASN is the first
      --  aspect specification node in the Rep_Item chain for the parent type.

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
         Id   : constant Node_Id   := Identifier (ASN);

      begin
         Error_Msg_Name_1 := Chars (Id);

         if not Is_Type (Ent) then
            Error_Msg_N ("aspect% can only apply to a type", Id);
            return;

         elsif not Is_First_Subtype (Ent) then
            Error_Msg_N ("aspect% cannot apply to subtype", Id);
            return;

         elsif A_Id = Aspect_Default_Value
           and then not Is_Scalar_Type (Ent)
         then
            Error_Msg_N ("aspect% can only be applied to scalar type", Id);
            return;

         elsif A_Id = Aspect_Default_Component_Value then
            if not Is_Array_Type (Ent) then
               Error_Msg_N ("aspect% can only be applied to array type", Id);
               return;

            elsif not Is_Scalar_Type (Component_Type (Ent)) then
               Error_Msg_N ("aspect% requires scalar components", Id);
               return;
            end if;
         end if;

         Set_Has_Default_Aspect (Base_Type (Ent));

         if Is_Scalar_Type (Ent) then
            Set_Default_Aspect_Value (Base_Type (Ent), Expr);
         else
            Set_Default_Aspect_Component_Value (Base_Type (Ent), Expr);
         end if;
      end Analyze_Aspect_Default_Value;

      ---------------------------------
      -- Inherit_Delayed_Rep_Aspects --
      ---------------------------------

      procedure Inherit_Delayed_Rep_Aspects (ASN : Node_Id) is
         P : constant Entity_Id := Entity (ASN);
         --  Entithy for parent type

         N : Node_Id;
         --  Item from Rep_Item chain

         A : Aspect_Id;

      begin
         --  Loop through delayed aspects for the parent type

         N := ASN;
         while Present (N) loop
            if Nkind (N) = N_Aspect_Specification then
               exit when Entity (N) /= P;

               if Is_Delayed_Aspect (N) then
                  A := Get_Aspect_Id (Chars (Identifier (N)));

                  --  Process delayed rep aspect. For Boolean attributes it is
                  --  not possible to cancel an attribute once set (the attempt
                  --  to use an aspect with xxx => False is an error) for a
                  --  derived type. So for those cases, we do not have to check
                  --  if a clause has been given for the derived type, since it
                  --  is harmless to set it again if it is already set.

                  case A is

                     --  Alignment

                     when Aspect_Alignment =>
                        if not Has_Alignment_Clause (E) then
                           Set_Alignment (E, Alignment (P));
                        end if;

                     --  Atomic

                     when Aspect_Atomic =>
                        if Is_Atomic (P) then
                           Set_Is_Atomic (E);
                        end if;

                     --  Atomic_Components

                     when Aspect_Atomic_Components =>
                        if Has_Atomic_Components (P) then
                           Set_Has_Atomic_Components (Base_Type (E));
                        end if;

                     --  Bit_Order

                     when Aspect_Bit_Order =>
                        if Is_Record_Type (E)
                          and then No (Get_Attribute_Definition_Clause
                                         (E, Attribute_Bit_Order))
                          and then Reverse_Bit_Order (P)
                        then
                           Set_Reverse_Bit_Order (Base_Type (E));
                        end if;

                     --  Component_Size

                     when Aspect_Component_Size =>
                        if Is_Array_Type (E)
                          and then not Has_Component_Size_Clause (E)
                        then
                           Set_Component_Size
                             (Base_Type (E), Component_Size (P));
                        end if;

                     --  Machine_Radix

                     when Aspect_Machine_Radix =>
                        if Is_Decimal_Fixed_Point_Type (E)
                          and then not Has_Machine_Radix_Clause (E)
                        then
                           Set_Machine_Radix_10 (E, Machine_Radix_10 (P));
                        end if;

                     --  Object_Size (also Size which also sets Object_Size)

                     when Aspect_Object_Size | Aspect_Size =>
                        if not Has_Size_Clause (E)
                          and then
                            No (Get_Attribute_Definition_Clause
                                  (E, Attribute_Object_Size))
                        then
                           Set_Esize (E, Esize (P));
                        end if;

                     --  Pack

                     when Aspect_Pack =>
                        if not Is_Packed (E) then
                           Set_Is_Packed (Base_Type (E));

                           if Is_Bit_Packed_Array (P) then
                              Set_Is_Bit_Packed_Array (Base_Type (E));
                              Set_Packed_Array_Type (E, Packed_Array_Type (P));
                           end if;
                        end if;

                     --  Scalar_Storage_Order

                     when Aspect_Scalar_Storage_Order =>
                        if (Is_Record_Type (E) or else Is_Array_Type (E))
                          and then No (Get_Attribute_Definition_Clause
                                         (E, Attribute_Scalar_Storage_Order))
                          and then Reverse_Storage_Order (P)
                        then
                           Set_Reverse_Storage_Order (Base_Type (E));
                        end if;

                     --  Small

                     when Aspect_Small =>
                        if Is_Fixed_Point_Type (E)
                          and then not Has_Small_Clause (E)
                        then
                           Set_Small_Value (E, Small_Value (P));
                        end if;

                     --  Storage_Size

                     when Aspect_Storage_Size =>
                        if (Is_Access_Type (E) or else Is_Task_Type (E))
                          and then not Has_Storage_Size_Clause (E)
                        then
                           Set_Storage_Size_Variable
                             (Base_Type (E), Storage_Size_Variable (P));
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
                           Set_Is_Volatile (E);
                        end if;

                     --  Volatile_Components

                     when Aspect_Volatile_Components =>
                        if Has_Volatile_Components (P) then
                           Set_Has_Volatile_Components (Base_Type (E));
                        end if;

                     --  That should be all the Rep Aspects

                     when others =>
                        pragma Assert (Aspect_Delay (A_Id) /= Rep_Aspect);
                        null;

                  end case;
               end if;
            end if;

            N := Next_Rep_Item (N);
         end loop;
      end Inherit_Delayed_Rep_Aspects;

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

         Prag : Node_Id;

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
               when Aspect_Atomic | Aspect_Shared =>
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

               when others =>
                  return;
            end case;

            --  Fall through means we are canceling an inherited aspect

            Error_Msg_Name_1 := A_Name;
            Error_Msg_NE
              ("derived type& inherits aspect%, cannot cancel", Expr, E);

         end Check_False_Aspect_For_Derived_Type;

      --  Start of processing for Make_Pragma_From_Boolean_Aspect

      begin
         --  Note that we know Expr is present, because for a missing Expr
         --  argument, we knew it was True and did not need to delay the
         --  evaluation to the freeze point.

         if Is_False (Static_Boolean (Expr)) then
            Check_False_Aspect_For_Derived_Type;

         else
            Prag :=
              Make_Pragma (Loc,
                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Sloc (Ident),
                    Expression => New_Occurrence_Of (Ent, Sloc (Ident)))),

                Pragma_Identifier            =>
                  Make_Identifier (Sloc (Ident), Chars (Ident)));

            Set_From_Aspect_Specification (Prag, True);
            Set_Corresponding_Aspect (Prag, ASN);
            Set_Aspect_Rep_Item (ASN, Prag);
            Set_Is_Delayed_Aspect (Prag);
            Set_Parent (Prag, ASN);
         end if;
      end Make_Pragma_From_Boolean_Aspect;

   --  Start of processing for Analyze_Aspects_At_Freeze_Point

   begin
      --  Must be visible in current scope

      if not Scope_Within_Or_Same (Current_Scope, Scope (E)) then
         return;
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
                  --  the corresponding pragma at the freezing point.

               when Boolean_Aspects      |
                    Library_Unit_Aspects =>
                  Make_Pragma_From_Boolean_Aspect (ASN);

                  --  Special handling for aspects that don't correspond to
                  --  pragmas/attributes.

               when Aspect_Default_Value           |
                    Aspect_Default_Component_Value =>
                  Analyze_Aspect_Default_Value (ASN);

                  --  Ditto for iterator aspects, because the corresponding
                  --  attributes may not have been analyzed yet.

               when Aspect_Constant_Indexing |
                    Aspect_Variable_Indexing |
                    Aspect_Default_Iterator  |
                    Aspect_Iterator_Element  =>
                  Analyze (Expression (ASN));

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

      --  This is where we inherit delayed rep aspects from our parent. Note
      --  that if we fell out of the above loop with ASN non-empty, it means
      --  we hit an aspect for an entity other than E, and it must be the
      --  type from which we were derived.

      if May_Inherit_Delayed_Rep_Aspects (E) then
         Inherit_Delayed_Rep_Aspects (ASN);
      end if;
   end Analyze_Aspects_At_Freeze_Point;

   -----------------------------------
   -- Analyze_Aspect_Specifications --
   -----------------------------------

   procedure Analyze_Aspect_Specifications (N : Node_Id; E : Entity_Id) is
      procedure Decorate_Delayed_Aspect_And_Pragma
        (Asp  : Node_Id;
         Prag : Node_Id);
      --  Establish the linkages between a delayed aspect and its corresponding
      --  pragma. Set all delay-related flags on both constructs.

      procedure Insert_Delayed_Pragma (Prag : Node_Id);
      --  Insert a postcondition-like pragma into the tree depending on the
      --  context. Prag must denote one of the following: Pre, Post, Depends,
      --  Global or Contract_Cases.

      ----------------------------------------
      -- Decorate_Delayed_Aspect_And_Pragma --
      ----------------------------------------

      procedure Decorate_Delayed_Aspect_And_Pragma
        (Asp  : Node_Id;
         Prag : Node_Id)
      is
      begin
         Set_Aspect_Rep_Item           (Asp, Prag);
         Set_Corresponding_Aspect      (Prag, Asp);
         Set_From_Aspect_Specification (Prag);
         Set_Is_Delayed_Aspect         (Prag);
         Set_Is_Delayed_Aspect         (Asp);
         Set_Parent                    (Prag, Asp);
      end Decorate_Delayed_Aspect_And_Pragma;

      ---------------------------
      -- Insert_Delayed_Pragma --
      ---------------------------

      procedure Insert_Delayed_Pragma (Prag : Node_Id) is
         Aux : Node_Id;

      begin
         --  When the context is a library unit, the pragma is added to the
         --  Pragmas_After list.

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Aux := Aux_Decls_Node (Parent (N));

            if No (Pragmas_After (Aux)) then
               Set_Pragmas_After (Aux, New_List);
            end if;

            Prepend (Prag, Pragmas_After (Aux));

         --  Pragmas associated with subprogram bodies are inserted in the
         --  declarative part.

         elsif Nkind (N) = N_Subprogram_Body then
            if No (Declarations (N)) then
               Set_Declarations (N, New_List (Prag));
            else
               declare
                  D : Node_Id;
               begin

                  --  There may be several aspects associated with the body;
                  --  preserve the ordering of the corresponding pragmas.

                  D := First (Declarations (N));
                  while Present (D) loop
                     exit when Nkind (D) /= N_Pragma
                       or else not From_Aspect_Specification (D);
                     Next (D);
                  end loop;

                  if No (D) then
                     Append (Prag, Declarations (N));
                  else
                     Insert_Before (D, Prag);
                  end if;
               end;
            end if;

         --  Default

         else
            Insert_After (N, Prag);

            --  Analyze the pragma before analyzing the proper body of a stub.
            --  This ensures that the pragma will appear on the proper contract
            --  list (see N_Contract).

            if Nkind (N) = N_Subprogram_Body_Stub then
               Analyze (Prag);
            end if;
         end if;
      end Insert_Delayed_Pragma;

      --  Local variables

      Aspect : Node_Id;
      Aitem  : Node_Id;
      Ent    : Node_Id;

      L : constant List_Id := Aspect_Specifications (N);

      Ins_Node : Node_Id := N;
      --  Insert pragmas/attribute definition clause after this node when no
      --  delayed analysis is required.

      --  Start of processing for Analyze_Aspect_Specifications

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
      --  Contract_Cases aspects. In these cases, we do not have to worry
      --  about delay issues, since the pragmas themselves deal with delay
      --  of visibility for the expression analysis. Thus, we just insert
      --  the pragma after the node N.

   begin
      pragma Assert (Present (L));

      --  Loop through aspects

      Aspect := First (L);
      Aspect_Loop : while Present (Aspect) loop
         Analyze_One_Aspect : declare
            Expr : constant Node_Id    := Expression (Aspect);
            Id   : constant Node_Id    := Identifier (Aspect);
            Loc  : constant Source_Ptr := Sloc (Aspect);
            Nam  : constant Name_Id    := Chars (Id);
            A_Id : constant Aspect_Id  := Get_Aspect_Id (Nam);
            Anod : Node_Id;

            Delay_Required : Boolean;
            --  Set False if delay is not required

            Eloc : Source_Ptr := No_Location;
            --  Source location of expression, modified when we split PPC's. It
            --  is set below when Expr is present.

            procedure Analyze_Aspect_External_Or_Link_Name;
            --  Perform analysis of the External_Name or Link_Name aspects

            procedure Analyze_Aspect_Implicit_Dereference;
            --  Perform analysis of the Implicit_Dereference aspects

            procedure Make_Aitem_Pragma
              (Pragma_Argument_Associations : List_Id;
               Pragma_Name                  : Name_Id);
            --  This is a wrapper for Make_Pragma used for converting aspects
            --  to pragmas. It takes care of Sloc (set from Loc) and building
            --  the pragma identifier from the given name. In addition the
            --  flags Class_Present and Split_PPC are set from the aspect
            --  node, as well as Is_Ignored. This routine also sets the
            --  From_Aspect_Specification in the resulting pragma node to
            --  True, and sets Corresponding_Aspect to point to the aspect.
            --  The resulting pragma is assigned to Aitem.

            ------------------------------------------
            -- Analyze_Aspect_External_Or_Link_Name --
            ------------------------------------------

            procedure Analyze_Aspect_External_Or_Link_Name is
            begin
               --  Verify that there is an Import/Export aspect defined for the
               --  entity. The processing of that aspect in turn checks that
               --  there is a Convention aspect declared. The pragma is
               --  constructed when processing the Convention aspect.

               declare
                  A : Node_Id;

               begin
                  A := First (L);
                  while Present (A) loop
                     exit when Nam_In (Chars (Identifier (A)), Name_Export,
                                                               Name_Import);
                     Next (A);
                  end loop;

                  if No (A) then
                     Error_Msg_N
                       ("missing Import/Export for Link/External name",
                         Aspect);
                  end if;
               end;
            end Analyze_Aspect_External_Or_Link_Name;

            -----------------------------------------
            -- Analyze_Aspect_Implicit_Dereference --
            -----------------------------------------

            procedure Analyze_Aspect_Implicit_Dereference is
            begin
               if not Is_Type (E) or else not Has_Discriminants (E) then
                  Error_Msg_N
                    ("aspect must apply to a type with discriminants", N);

               else
                  declare
                     Disc : Entity_Id;

                  begin
                     Disc := First_Discriminant (E);
                     while Present (Disc) loop
                        if Chars (Expr) = Chars (Disc)
                          and then Ekind (Etype (Disc)) =
                                     E_Anonymous_Access_Type
                        then
                           Set_Has_Implicit_Dereference (E);
                           Set_Has_Implicit_Dereference (Disc);
                           return;
                        end if;

                        Next_Discriminant (Disc);
                     end loop;

                     --  Error if no proper access discriminant.

                     Error_Msg_NE
                      ("not an access discriminant of&", Expr, E);
                  end;
               end if;
            end Analyze_Aspect_Implicit_Dereference;

            -----------------------
            -- Make_Aitem_Pragma --
            -----------------------

            procedure Make_Aitem_Pragma
              (Pragma_Argument_Associations : List_Id;
               Pragma_Name                  : Name_Id)
            is
               Args : List_Id := Pragma_Argument_Associations;

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
                   Class_Present     => Class_Present (Aspect),
                   Split_PPC         => Split_PPC (Aspect));

               --  Set additional semantic fields

               if Is_Ignored (Aspect) then
                  Set_Is_Ignored (Aitem);
               elsif Is_Checked (Aspect) then
                  Set_Is_Checked (Aitem);
               end if;

               Set_Corresponding_Aspect (Aitem, Aspect);
               Set_From_Aspect_Specification (Aitem, True);
            end Make_Aitem_Pragma;

         --  Start of processing for Analyze_One_Aspect

         begin
            --  Skip aspect if already analyzed (not clear if this is needed)

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

            --  Analyze this aspect (actual analysis is delayed till later)

            Set_Analyzed (Aspect);
            Set_Entity (Aspect, E);
            Ent := New_Occurrence_Of (E, Sloc (Id));

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

            if not Implementation_Defined_Aspect (A_Id) then
               Error_Msg_Name_1 := Nam;

               --  Not allowed for renaming declarations

               if Nkind (N) in N_Renaming_Declaration then
                  Error_Msg_N
                    ("aspect % not allowed for renaming declaration",
                     Aspect);
               end if;

               --  Not allowed for formal type declarations

               if Nkind (N) = N_Formal_Type_Declaration then
                  Error_Msg_N
                    ("aspect % not allowed for formal type declaration",
                     Aspect);
               end if;
            end if;

            --  Copy expression for later processing by the procedures
            --  Check_Aspect_At_[Freeze_Point | End_Of_Declarations]

            Set_Entity (Id, New_Copy_Tree (Expr));

            --  Set Delay_Required as appropriate to aspect

            case Aspect_Delay (A_Id) is
               when Always_Delay =>
                  Delay_Required := True;

               when Never_Delay =>
                  Delay_Required := False;

               when Rep_Aspect =>

                  --  If expression has the form of an integer literal, then
                  --  do not delay, since we know the value cannot change.
                  --  This optimization catches most rep clause cases.

               if (Present (Expr) and then Nkind (Expr) = N_Integer_Literal)
                 or else (A_Id in Boolean_Aspects and then No (Expr))
               then
                  Delay_Required := False;
               else
                  Delay_Required := True;
                  Set_Has_Delayed_Rep_Aspects (E);
               end if;
            end case;

            --  Processing based on specific aspect

            case A_Id is

               --  No_Aspect should be impossible

               when No_Aspect =>
                  raise Program_Error;

               --  Case 1: Aspects corresponding to attribute definition
               --  clauses.

               when Aspect_Address              |
                    Aspect_Alignment            |
                    Aspect_Bit_Order            |
                    Aspect_Component_Size       |
                    Aspect_Constant_Indexing    |
                    Aspect_Default_Iterator     |
                    Aspect_Dispatching_Domain   |
                    Aspect_External_Tag         |
                    Aspect_Input                |
                    Aspect_Iterator_Element     |
                    Aspect_Machine_Radix        |
                    Aspect_Object_Size          |
                    Aspect_Output               |
                    Aspect_Read                 |
                    Aspect_Scalar_Storage_Order |
                    Aspect_Size                 |
                    Aspect_Small                |
                    Aspect_Simple_Storage_Pool  |
                    Aspect_Storage_Pool         |
                    Aspect_Stream_Size          |
                    Aspect_Value_Size           |
                    Aspect_Variable_Indexing    |
                    Aspect_Write                =>

                  --  Indexing aspects apply only to tagged type

                  if (A_Id = Aspect_Constant_Indexing
                        or else
                      A_Id = Aspect_Variable_Indexing)
                    and then not (Is_Type (E)
                                   and then Is_Tagged_Type (E))
                  then
                     Error_Msg_N ("indexing applies to a tagged type", N);
                     goto Continue;
                  end if;

                  --  For case of address aspect, we don't consider that we
                  --  know the entity is never set in the source, since it is
                  --  is likely aliasing is occurring.

                  --  Note: one might think that the analysis of the resulting
                  --  attribute definition clause would take care of that, but
                  --  that's not the case since it won't be from source.

                  if A_Id = Aspect_Address then
                     Set_Never_Set_In_Source (E, False);
                  end if;

                  --  Construct the attribute definition clause

                  Aitem :=
                    Make_Attribute_Definition_Clause (Loc,
                      Name       => Ent,
                      Chars      => Chars (Id),
                      Expression => Relocate_Node (Expr));

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

               --  Linker_Section/Suppress/Unsuppress

               when Aspect_Linker_Section |
                    Aspect_Suppress       |
                    Aspect_Unsuppress     =>

                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => New_Occurrence_Of (E, Loc)),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Chars (Id));

               --  Synchronization

               --  Corresponds to pragma Implemented, construct the pragma

               when Aspect_Synchronization =>

                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => New_Occurrence_Of (E, Loc)),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Implemented);

               --  Attach Handler

               when Aspect_Attach_Handler =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Attach_Handler);

               --  Dynamic_Predicate, Predicate, Static_Predicate

               when Aspect_Dynamic_Predicate |
                    Aspect_Predicate         |
                    Aspect_Static_Predicate  =>

                  --  Construct the pragma (always a pragma Predicate, with
                  --  flags recording whether it is static/dynamic). We also
                  --  set flags recording this in the type itself.

                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent),
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Predicate);

                  --  Mark type has predicates, and remember what kind of
                  --  aspect lead to this predicate (we need this to access
                  --  the right set of check policies later on).

                  Set_Has_Predicates (E);

                  if A_Id = Aspect_Dynamic_Predicate then
                     Set_Has_Dynamic_Predicate_Aspect (E);
                  elsif A_Id = Aspect_Static_Predicate then
                     Set_Has_Static_Predicate_Aspect (E);
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
                     end if;

                     Set_Has_Delayed_Aspects (Full_View (E));
                     Ensure_Freeze_Node (Full_View (E));
                  end if;

               --  Case 2b: Aspects corresponding to pragmas with two
               --  arguments, where the second argument is a local name
               --  referring to the entity, and the first argument is the
               --  aspect definition expression.

               --  Convention

               when Aspect_Convention  =>

                  --  The aspect may be part of the specification of an import
                  --  or export pragma. Scan the aspect list to gather the
                  --  other components, if any. The name of the generated
                  --  pragma is one of Convention/Import/Export.

                  declare
                     P_Name   : Name_Id;
                     A_Name   : Name_Id;
                     A        : Node_Id;
                     Arg_List : List_Id;
                     Found    : Boolean;
                     L_Assoc  : Node_Id;
                     E_Assoc  : Node_Id;

                  begin
                     P_Name   := Chars (Id);
                     Found    := False;
                     Arg_List := New_List;
                     L_Assoc  := Empty;
                     E_Assoc  := Empty;

                     A := First (L);
                     while Present (A) loop
                        A_Name := Chars (Identifier (A));

                        if Nam_In (A_Name, Name_Import, Name_Export) then
                           if Found then
                              Error_Msg_N ("conflicting", A);
                           else
                              Found := True;
                           end if;

                           P_Name := A_Name;

                        elsif A_Name = Name_Link_Name then
                           L_Assoc :=
                             Make_Pragma_Argument_Association (Loc,
                               Chars      => A_Name,
                               Expression => Relocate_Node (Expression (A)));

                        elsif A_Name = Name_External_Name then
                           E_Assoc :=
                             Make_Pragma_Argument_Association (Loc,
                               Chars      => A_Name,
                               Expression => Relocate_Node (Expression (A)));
                        end if;

                        Next (A);
                     end loop;

                     Arg_List := New_List (
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr)),
                       Make_Pragma_Argument_Association (Sloc (Ent),
                         Expression => Ent));

                     if Present (L_Assoc) then
                        Append_To (Arg_List, L_Assoc);
                     end if;

                     if Present (E_Assoc) then
                        Append_To (Arg_List, E_Assoc);
                     end if;

                     Make_Aitem_Pragma
                       (Pragma_Argument_Associations => Arg_List,
                        Pragma_Name                  => P_Name);
                  end;

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

               when Aspect_CPU                |
                    Aspect_Interrupt_Priority |
                    Aspect_Priority           =>

                  if Nkind_In (N, N_Subprogram_Body,
                                  N_Subprogram_Declaration)
                  then
                     --  Analyze the aspect expression

                     Analyze_And_Resolve (Expr, Standard_Integer);

                     --  Interrupt_Priority aspect not allowed for main
                     --  subprograms. ARM D.1 does not forbid this explicitly,
                     --  but ARM J.15.11 (6/3) does not permit pragma
                     --  Interrupt_Priority for subprograms.

                     if A_Id = Aspect_Interrupt_Priority then
                        Error_Msg_N
                          ("Interrupt_Priority aspect cannot apply to "
                           & "subprogram", Expr);

                     --  The expression must be static

                     elsif not Is_Static_Expression (Expr) then
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
                        --  See ARM D.1 (14/3) and D.16 (12/3)

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
                             ("main subprogram CPU is out of range", Expr);
                        end if;

                     --  For the Priority aspect

                     elsif A_Id = Aspect_Priority then
                        if Is_In_Range (Expr, RTE (RE_Priority)) then

                           --  Value is correct so we export the value to make
                           --  it available at execution time.

                           Set_Main_Priority
                             (Main_Unit, UI_To_Int (Expr_Value (Expr)));

                        else
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
                     --  have any effect. Previously with with'ed the package
                     --  System.Tasking, but this package does not trigger the
                     --  required initialization of the run-time library.

                     declare
                        Discard : Entity_Id;
                        pragma Warnings (Off, Discard);
                     begin
                        if Restricted_Profile then
                           Discard := RTE (RE_Activate_Restricted_Tasks);
                        else
                           Discard := RTE (RE_Activate_Tasks);
                        end if;
                     end;

                     --  Handling for these Aspects in subprograms is complete

                     goto Continue;

                  --  For tasks

                  else
                     --  Pass the aspect as an attribute

                     Aitem :=
                       Make_Attribute_Definition_Clause (Loc,
                         Name       => Ent,
                         Chars      => Chars (Id),
                         Expression => Relocate_Node (Expr));
                  end if;

               --  Warnings

               when Aspect_Warnings =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Sloc (Expr),
                         Expression => Relocate_Node (Expr)),
                       Make_Pragma_Argument_Association (Loc,
                         Expression => New_Occurrence_Of (E, Loc))),
                     Pragma_Name                  => Chars (Id));

               --  Case 2c: Aspects corresponding to pragmas with three
               --  arguments.

               --  Invariant aspects have a first argument that references the
               --  entity, a second argument that is the expression and a third
               --  argument that is an appropriate message.

               --  Invariant, Type_Invariant

               when Aspect_Invariant      |
                    Aspect_Type_Invariant =>

                  --  Analysis of the pragma will verify placement legality:
                  --  an invariant must apply to a private type, or appear in
                  --  the private part of a spec and apply to a completion.

                  Make_Aitem_Pragma
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
                  Decls : List_Id;

               begin
                  if Nkind_In (N, N_Generic_Package_Declaration,
                                  N_Package_Declaration)
                  then
                     Decls := Visible_Declarations (Specification (N));

                     Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Abstract_State);
                     Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);

                     if No (Decls) then
                        Decls := New_List;
                        Set_Visible_Declarations (N, Decls);
                     end if;

                     Prepend_To (Decls, Aitem);

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package declaration",
                        Aspect, Id);
                  end if;

                  goto Continue;
               end Abstract_State;

               --  Depends

               --  Aspect Depends must be delayed because it mentions names
               --  of inputs and output that are classified by aspect Global.
               --  The aspect and pragma are treated the same way as a post
               --  condition.

               when Aspect_Depends =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Depends);

                  Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                  Insert_Delayed_Pragma (Aitem);
                  goto Continue;

               --  Global

               --  Aspect Global must be delayed because it can mention names
               --  and benefit from the forward visibility rules applicable to
               --  aspects of subprograms. The aspect and pragma are treated
               --  the same way as a post condition.

               when Aspect_Global =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Global);

                  Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                  Insert_Delayed_Pragma (Aitem);
                  goto Continue;

               --  Initial_Condition

               --  Aspect Initial_Condition covers the visible declarations of
               --  a package and all hidden states through functions. As such,
               --  it must be evaluated at the end of the said declarations.

               when Aspect_Initial_Condition => Initial_Condition : declare
                  Decls : List_Id;

               begin
                  if Nkind_In (N, N_Generic_Package_Declaration,
                                  N_Package_Declaration)
                  then
                     Decls := Visible_Declarations (Specification (N));

                     Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  =>
                          Name_Initial_Condition);
                     Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);

                     if No (Decls) then
                        Decls := New_List;
                        Set_Visible_Declarations (N, Decls);
                     end if;

                     Prepend_To (Decls, Aitem);

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package declaration",
                        Aspect, Id);
                  end if;

                  goto Continue;
               end Initial_Condition;

               --  Initializes

               --  Aspect Initializes coverts the visible declarations of a
               --  package. As such, it must be evaluated at the end of the
               --  said declarations.

               when Aspect_Initializes => Initializes : declare
                  Decls : List_Id;

               begin
                  if Nkind_In (N, N_Generic_Package_Declaration,
                                  N_Package_Declaration)
                  then
                     Decls := Visible_Declarations (Specification (N));

                     Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Initializes);
                     Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);

                     if No (Decls) then
                        Decls := New_List;
                        Set_Visible_Declarations (N, Decls);
                     end if;

                     Prepend_To (Decls, Aitem);

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package declaration",
                        Aspect, Id);
                  end if;

                  goto Continue;
               end Initializes;

               --  SPARK_Mode

               when Aspect_SPARK_Mode => SPARK_Mode : declare
                  Decls : List_Id;

               begin
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_SPARK_Mode);

                  --  When the aspect appears on a package body, insert the
                  --  generated pragma at the top of the body declarations to
                  --  emulate the behavior of a source pragma.

                  if Nkind (N) = N_Package_Body then
                     Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                     Decls := Declarations (N);

                     if No (Decls) then
                        Decls := New_List;
                        Set_Declarations (N, Decls);
                     end if;

                     Prepend_To (Decls, Aitem);
                     goto Continue;

                  --  When the aspect is associated with package declaration,
                  --  insert the generated pragma at the top of the visible
                  --  declarations to emulate the behavior of a source pragma.

                  elsif Nkind (N) = N_Package_Declaration then
                     Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                     Decls := Visible_Declarations (Specification (N));

                     if No (Decls) then
                        Decls := New_List;
                        Set_Visible_Declarations (Specification (N), Decls);
                     end if;

                     Prepend_To (Decls, Aitem);
                     goto Continue;
                  end if;
               end SPARK_Mode;

               --  Refined_Depends

               --  Aspect Refined_Depends must be delayed because it can
               --  mention state refinements introduced by aspect Refined_State
               --  and further classified by aspect Refined_Global. Since both
               --  those aspects are delayed, so is Refined_Depends.

               when Aspect_Refined_Depends =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Refined_Depends);

                  Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                  Insert_Delayed_Pragma (Aitem);
                  goto Continue;

               --  Refined_Global

               --  Aspect Refined_Global must be delayed because it can mention
               --  state refinements introduced by aspect Refined_State. Since
               --  Refined_State is already delayed due to forward references,
               --  so is Refined_Global.

               when Aspect_Refined_Global =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Refined_Global);

                  Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                  Insert_Delayed_Pragma (Aitem);
                  goto Continue;

               --  Refined_Post

               when Aspect_Refined_Post =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Name_Refined_Post);

               --  Refined_State

               when Aspect_Refined_State => Refined_State : declare
                  Decls : List_Id;

               begin
                  --  The corresponding pragma for Refined_State is inserted in
                  --  the declarations of the related package body. This action
                  --  synchronizes both the source and from-aspect versions of
                  --  the pragma.

                  if Nkind (N) = N_Package_Body then
                     Decls := Declarations (N);

                     Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Loc,
                            Expression => Relocate_Node (Expr))),
                        Pragma_Name                  => Name_Refined_State);
                     Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);

                     if No (Decls) then
                        Decls := New_List;
                        Set_Declarations (N, Decls);
                     end if;

                     Prepend_To (Decls, Aitem);

                  else
                     Error_Msg_NE
                       ("aspect & must apply to a package body", Aspect, Id);
                  end if;

                  goto Continue;
               end Refined_State;

               --  Relative_Deadline

               when Aspect_Relative_Deadline =>
                  Make_Aitem_Pragma
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

               --  Case 3 : Aspects that don't correspond to pragma/attribute
               --  definition clause.

               --  Case 3a: The aspects listed below don't correspond to
               --  pragmas/attributes but do require delayed analysis.

               --  Default_Value, Default_Component_Value

               when Aspect_Default_Value           |
                    Aspect_Default_Component_Value =>
                  Aitem := Empty;

               --  Case 3b: The aspects listed below don't correspond to
               --  pragmas/attributes and don't need delayed analysis.

               --  Implicit_Dereference

               --  For Implicit_Dereference, External_Name and Link_Name, only
               --  the legality checks are done during the analysis, thus no
               --  delay is required.

               when Aspect_Implicit_Dereference =>
                  Analyze_Aspect_Implicit_Dereference;
                  goto Continue;

               --  External_Name, Link_Name

               when Aspect_External_Name |
                    Aspect_Link_Name     =>
                  Analyze_Aspect_External_Or_Link_Name;
                  goto Continue;

               --  Dimension

               when Aspect_Dimension =>
                  Analyze_Aspect_Dimension (N, Id, Expr);
                  goto Continue;

               --  Dimension_System

               when Aspect_Dimension_System =>
                  Analyze_Aspect_Dimension_System (N, Id, Expr);
                  goto Continue;

               --  Case 4: Aspects requiring special handling

               --  Pre/Post/Test_Case/Contract_Cases whose corresponding
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
                  if A_Id = Aspect_Pre or else A_Id = Aspect_Precondition then
                     Pname := Name_Precondition;
                  else
                     Pname := Name_Postcondition;
                  end if;

                  --  If the expressions is of the form A and then B, then
                  --  we generate separate Pre/Post aspects for the separate
                  --  clauses. Since we allow multiple pragmas, there is no
                  --  problem in allowing multiple Pre/Post aspects internally.
                  --  These should be treated in reverse order (B first and
                  --  A second) since they are later inserted just after N in
                  --  the order they are treated. This way, the pragma for A
                  --  ends up preceding the pragma for B, which may have an
                  --  importance for the error raised (either constraint error
                  --  or precondition error).

                  --  We do not do this for Pre'Class, since we have to put
                  --  these conditions together in a complex OR expression

                  --  We do not do this in ASIS mode, as ASIS relies on the
                  --  original node representing the complete expression, when
                  --  retrieving it through the source aspect table.

                  if not ASIS_Mode
                    and then (Pname = Name_Postcondition
                               or else not Class_Present (Aspect))
                  then
                     while Nkind (Expr) = N_And_Then loop
                        Insert_After (Aspect,
                          Make_Aspect_Specification (Sloc (Left_Opnd (Expr)),
                            Identifier    => Identifier (Aspect),
                            Expression    => Relocate_Node (Left_Opnd (Expr)),
                            Class_Present => Class_Present (Aspect),
                            Split_PPC     => True));
                        Rewrite (Expr, Relocate_Node (Right_Opnd (Expr)));
                        Eloc := Sloc (Expr);
                     end loop;
                  end if;

                  --  Build the precondition/postcondition pragma

                  --  Add note about why we do NOT need Copy_Tree here ???

                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Eloc,
                         Chars      => Name_Check,
                         Expression => Relocate_Node (Expr))),
                       Pragma_Name                => Pname);

                  --  Add message unless exception messages are suppressed

                  if not Opt.Exception_Locations_Suppressed then
                     Append_To (Pragma_Argument_Associations (Aitem),
                       Make_Pragma_Argument_Association (Eloc,
                         Chars     => Name_Message,
                         Expression =>
                           Make_String_Literal (Eloc,
                             Strval => "failed "
                                       & Get_Name_String (Pname)
                                       & " from "
                                       & Build_Location_String (Eloc))));
                  end if;

                  Set_Is_Delayed_Aspect (Aspect);

                  --  For Pre/Post cases, insert immediately after the entity
                  --  declaration, since that is the required pragma placement.
                  --  Note that for these aspects, we do not have to worry
                  --  about delay issues, since the pragmas themselves deal
                  --  with delay of visibility for the expression analysis.

                  Insert_Delayed_Pragma (Aitem);
                  goto Continue;
               end Pre_Post;

               --  Test_Case

               when Aspect_Test_Case => Test_Case : declare
                  Args      : List_Id;
                  Comp_Expr : Node_Id;
                  Comp_Assn : Node_Id;
                  New_Expr  : Node_Id;

               begin
                  Args := New_List;

                  if Nkind (Parent (N)) = N_Compilation_Unit then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_N ("incorrect placement of aspect `%`", E);
                     goto Continue;
                  end if;

                  if Nkind (Expr) /= N_Aggregate then
                     Error_Msg_Name_1 := Nam;
                     Error_Msg_NE
                       ("wrong syntax for aspect `%` for &", Id, E);
                     goto Continue;
                  end if;

                  --  Make pragma expressions refer to the original aspect
                  --  expressions through the Original_Node link. This is
                  --  used in semantic analysis for ASIS mode, so that the
                  --  original expression also gets analyzed.

                  Comp_Expr := First (Expressions (Expr));
                  while Present (Comp_Expr) loop
                     New_Expr := Relocate_Node (Comp_Expr);
                     Set_Original_Node (New_Expr, Comp_Expr);
                     Append_To (Args,
                       Make_Pragma_Argument_Association (Sloc (Comp_Expr),
                         Expression => New_Expr));
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
                          ("wrong syntax for aspect `%` for &", Id, E);
                        goto Continue;
                     end if;

                     New_Expr := Relocate_Node (Expression (Comp_Assn));
                     Set_Original_Node (New_Expr, Expression (Comp_Assn));
                     Append_To (Args,
                       Make_Pragma_Argument_Association (Sloc (Comp_Assn),
                       Chars      => Chars (First (Choices (Comp_Assn))),
                       Expression => New_Expr));
                     Next (Comp_Assn);
                  end loop;

                  --  Build the test-case pragma

                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => Args,
                     Pragma_Name                  => Nam);
               end Test_Case;

               --  Contract_Cases

               when Aspect_Contract_Cases =>
                  Make_Aitem_Pragma
                    (Pragma_Argument_Associations => New_List (
                       Make_Pragma_Argument_Association (Loc,
                         Expression => Relocate_Node (Expr))),
                     Pragma_Name                  => Nam);

                  Decorate_Delayed_Aspect_And_Pragma (Aspect, Aitem);
                  Insert_Delayed_Pragma (Aitem);
                  goto Continue;

               --  Case 5: Special handling for aspects with an optional
               --  boolean argument.

               --  In the general case, the corresponding pragma cannot be
               --  generated yet because the evaluation of the boolean needs
               --  to be delayed till the freeze point.

               when Boolean_Aspects      |
                    Library_Unit_Aspects =>

                  Set_Is_Boolean_Aspect (Aspect);

                  --  Lock_Free aspect only apply to protected objects

                  if A_Id = Aspect_Lock_Free then
                     if Ekind (E) /= E_Protected_Type then
                        Error_Msg_Name_1 := Nam;
                        Error_Msg_N
                          ("aspect % only applies to a protected object",
                           Aspect);

                     else
                        --  Set the Uses_Lock_Free flag to True if there is no
                        --  expression or if the expression is True. The
                        --  evaluation of this aspect should be delayed to the
                        --  freeze point (why???)

                        if No (Expr)
                          or else Is_True (Static_Boolean (Expr))
                        then
                           Set_Uses_Lock_Free (E);
                        end if;

                        Record_Rep_Item (E, Aspect);
                     end if;

                     goto Continue;

                  elsif A_Id = Aspect_Import or else A_Id = Aspect_Export then

                     --  Verify that there is an aspect Convention that will
                     --  incorporate the Import/Export aspect, and eventual
                     --  Link/External names.

                     declare
                        A : Node_Id;

                     begin
                        A := First (L);
                        while Present (A) loop
                           exit when Chars (Identifier (A)) = Name_Convention;
                           Next (A);
                        end loop;

                        --  It is legal to specify Import for a variable, in
                        --  order to suppress initialization for it, without
                        --  specifying explicitly its convention. However this
                        --  is only legal if the convention of the object type
                        --  is Ada or similar.

                        if No (A) then
                           if Ekind (E) = E_Variable
                             and then A_Id = Aspect_Import
                           then
                              declare
                                 C : constant Convention_Id :=
                                       Convention (Etype (E));
                              begin
                                 if C = Convention_Ada              or else
                                    C = Convention_Ada_Pass_By_Copy or else
                                    C = Convention_Ada_Pass_By_Reference
                                 then
                                    goto Continue;
                                 end if;
                              end;
                           end if;

                           --  Otherwise, Convention must be specified

                           Error_Msg_N
                             ("missing Convention aspect for Export/Import",
                              Aspect);
                        end if;
                     end;

                     goto Continue;
                  end if;

                  --  Library unit aspects require special handling in the case
                  --  of a package declaration, the pragma needs to be inserted
                  --  in the list of declarations for the associated package.
                  --  There is no issue of visibility delay for these aspects.

                  if A_Id in Library_Unit_Aspects
                    and then
                      Nkind_In (N, N_Package_Declaration,
                                   N_Generic_Package_Declaration)
                    and then Nkind (Parent (N)) /= N_Compilation_Unit
                  then
                     Error_Msg_N
                        ("incorrect context for library unit aspect&", Id);
                     goto Continue;
                  end if;

                  --  Cases where we do not delay, includes all cases where
                  --  the expression is missing other than the above cases.

                  if not Delay_Required or else No (Expr) then
                     Make_Aitem_Pragma
                       (Pragma_Argument_Associations => New_List (
                          Make_Pragma_Argument_Association (Sloc (Ent),
                            Expression => Ent)),
                        Pragma_Name                  => Chars (Id));
                     Delay_Required := False;

                  --  In general cases, the corresponding pragma/attribute
                  --  definition clause will be inserted later at the freezing
                  --  point, and we do not need to build it now

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

                        --  Create a pragma and put it at the start of the
                        --  task definition for the task type declaration.

                        Make_Aitem_Pragma
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
                         Chars      => Chars (Id),
                         Expression => Relocate_Node (Expr));
                  end if;
            end case;

            --  Attach the corresponding pragma/attribute definition clause to
            --  the aspect specification node.

            if Present (Aitem) then
               Set_From_Aspect_Specification (Aitem, True);
            end if;

            --  In the context of a compilation unit, we directly put the
            --  pragma in the Pragmas_After list of the N_Compilation_Unit_Aux
            --  node (no delay is required here) except for aspects on a
            --  subprogram body (see below) and a generic package, for which
            --  we need to introduce the pragma before building the generic
            --  copy (see sem_ch12), and for package instantiations, where
            --  the library unit pragmas are better handled early.

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
                        Make_Aitem_Pragma
                          (Pragma_Argument_Associations => New_List (
                             Make_Pragma_Argument_Association (Sloc (Ent),
                               Expression => Ent)),
                           Pragma_Name                  => Chars (Id));

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
               --  to a rep item circularity. Verify that we are dealing with
               --  a scalar type to prevent cascaded errors.

               if A_Id = Aspect_Default_Value
                 and then Is_Scalar_Type (E)
                 and then Base_Type (E) /= E
               then
                  Set_Has_Delayed_Aspects (Base_Type (E));
                  Record_Rep_Item (Base_Type (E), Aspect);
               end if;

               Set_Has_Delayed_Aspects (E);
               Record_Rep_Item (E, Aspect);

            --  When delay is not required and the context is a package or a
            --  subprogram body, insert the pragma in the body declarations.

            elsif Nkind_In (N, N_Package_Body, N_Subprogram_Body) then
               if No (Declarations (N)) then
                  Set_Declarations (N, New_List);
               end if;

               --  The pragma is added before source declarations

               Prepend_To (Declarations (N), Aitem);

            --  When delay is not required and the context is not a compilation
            --  unit, we simply insert the pragma/attribute definition clause
            --  in sequence.

            else
               Insert_After (Ins_Node, Aitem);
               Ins_Node := Aitem;
            end if;
         end Analyze_One_Aspect;

      <<Continue>>
         Next (Aspect);
      end loop Aspect_Loop;

      if Has_Delayed_Aspects (E) then
         Ensure_Freeze_Node (E);
      end if;
   end Analyze_Aspect_Specifications;

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
      --  applies to full view of incomplete type or private type in which case
      --  U_Ent is just a copy of Ent.

      FOnly : Boolean := False;
      --  Reset to True for subtype specific attribute (Alignment, Size)
      --  and for stream attributes, i.e. those cases where in the call
      --  to Rep_Item_Too_Late, FOnly is set True so that only the freezing
      --  rules are checked. Note that the case of stream attributes is not
      --  clear from the RM, but see AI95-00137. Also, the RM seems to
      --  disallow Storage_Size for derived task types, but that is also
      --  clearly unintentional.

      procedure Analyze_Stream_TSS_Definition (TSS_Nam : TSS_Name_Type);
      --  Common processing for 'Read, 'Write, 'Input and 'Output attribute
      --  definition clauses.

      function Duplicate_Clause return Boolean;
      --  This routine checks if the aspect for U_Ent being given by attribute
      --  definition clause N is for an aspect that has already been specified,
      --  and if so gives an error message. If there is a duplicate, True is
      --  returned, otherwise if there is no error, False is returned.

      procedure Check_Indexing_Functions;
      --  Check that the function in Constant_Indexing or Variable_Indexing
      --  attribute has the proper type structure. If the name is overloaded,
      --  check that some interpretation is legal.

      procedure Check_Iterator_Functions;
      --  Check that there is a single function in Default_Iterator attribute
      --  has the proper type structure.

      function Check_Primitive_Function (Subp : Entity_Id) return Boolean;
      --  Common legality check for the previous two

      -----------------------------------
      -- Analyze_Stream_TSS_Definition --
      -----------------------------------

      procedure Analyze_Stream_TSS_Definition (TSS_Nam : TSS_Name_Type) is
         Subp : Entity_Id := Empty;
         I    : Interp_Index;
         It   : Interp;
         Pnam : Entity_Id;

         Is_Read : constant Boolean := (TSS_Nam = TSS_Stream_Read);
         --  True for Read attribute, false for other attributes

         function Has_Good_Profile (Subp : Entity_Id) return Boolean;
         --  Return true if the entity is a subprogram with an appropriate
         --  profile for the attribute being defined.

         ----------------------
         -- Has_Good_Profile --
         ----------------------

         function Has_Good_Profile (Subp : Entity_Id) return Boolean is
            F              : Entity_Id;
            Is_Function    : constant Boolean := (TSS_Nam = TSS_Stream_Input);
            Expected_Ekind : constant array (Boolean) of Entity_Kind :=
                               (False => E_Procedure, True => E_Function);
            Typ            : Entity_Id;

         begin
            if Ekind (Subp) /= Expected_Ekind (Is_Function) then
               return False;
            end if;

            F := First_Formal (Subp);

            if No (F)
              or else Ekind (Etype (F)) /= E_Anonymous_Access_Type
              or else Designated_Type (Etype (F)) /=
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

            return Base_Type (Typ) = Base_Type (Ent)
              and then No (Next_Formal (F));
         end Has_Good_Profile;

      --  Start of processing for Analyze_Stream_TSS_Definition

      begin
         FOnly := True;

         if not Is_Type (U_Ent) then
            Error_Msg_N ("local name must be a subtype", Nam);
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
               if Has_Good_Profile (Entity (Expr)) then
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
            end if;

            Set_Entity (Expr, Subp);
            Set_Etype (Expr, Etype (Subp));

            New_Stream_Subprogram (N, U_Ent, Subp, TSS_Nam);

         else
            Error_Msg_Name_1 := Attr;
            Error_Msg_N ("incorrect expression for% attribute", Expr);
         end if;
      end Analyze_Stream_TSS_Definition;

      ------------------------------
      -- Check_Indexing_Functions --
      ------------------------------

      procedure Check_Indexing_Functions is
         Indexing_Found : Boolean;

         procedure Check_One_Function (Subp : Entity_Id);
         --  Check one possible interpretation. Sets Indexing_Found True if an
         --  indexing function is found.

         ------------------------
         -- Check_One_Function --
         ------------------------

         procedure Check_One_Function (Subp : Entity_Id) is
            Default_Element : constant Node_Id :=
                                Find_Value_Of_Aspect
                                  (Etype (First_Formal (Subp)),
                                   Aspect_Iterator_Element);

         begin
            if not Check_Primitive_Function (Subp)
              and then not Is_Overloaded (Expr)
            then
               Error_Msg_NE
                 ("aspect Indexing requires a function that applies to type&",
                    Subp, Ent);
            end if;

            --  An indexing function must return either the default element of
            --  the container, or a reference type. For variable indexing it
            --  must be the latter.

            if Present (Default_Element) then
               Analyze (Default_Element);

               if Is_Entity_Name (Default_Element)
                 and then Covers (Entity (Default_Element), Etype (Subp))
               then
                  Indexing_Found := True;
                  return;
               end if;
            end if;

            --  For variable_indexing the return type must be a reference type

            if Attr = Name_Variable_Indexing
              and then not Has_Implicit_Dereference (Etype (Subp))
            then
               Error_Msg_N
                 ("function for indexing must return a reference type", Subp);

            else
               Indexing_Found := True;
            end if;
         end Check_One_Function;

      --  Start of processing for Check_Indexing_Functions

      begin
         if In_Instance then
            return;
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
                  --  subprogram itself.

                  if Is_Overloadable (It.Nam) then
                     Check_One_Function (It.Nam);
                  end if;

                  Get_Next_Interp (I, It);
               end loop;

               if not Indexing_Found then
                  Error_Msg_NE
                    ("aspect Indexing requires a function that "
                     & "applies to type&", Expr, Ent);
               end if;
            end;
         end if;
      end Check_Indexing_Functions;

      ------------------------------
      -- Check_Iterator_Functions --
      ------------------------------

      procedure Check_Iterator_Functions is
         Default : Entity_Id;

         function Valid_Default_Iterator (Subp : Entity_Id) return Boolean;
         --  Check one possible interpretation for validity

         ----------------------------
         -- Valid_Default_Iterator --
         ----------------------------

         function Valid_Default_Iterator (Subp : Entity_Id) return Boolean is
            Formal : Entity_Id;

         begin
            if not Check_Primitive_Function (Subp) then
               return False;
            else
               Formal := First_Formal (Subp);
            end if;

            --  False if any subsequent formal has no default expression

            Formal := Next_Formal (Formal);
            while Present (Formal) loop
               if No (Expression (Parent (Formal))) then
                  return False;
               end if;

               Next_Formal (Formal);
            end loop;

            --  True if all subsequent formals have default expressions

            return True;
         end Valid_Default_Iterator;

      --  Start of processing for Check_Iterator_Functions

      begin
         Analyze (Expr);

         if not Is_Entity_Name (Expr) then
            Error_Msg_N ("aspect Iterator must be a function name", Expr);
         end if;

         if not Is_Overloaded (Expr) then
            if not Check_Primitive_Function (Entity (Expr)) then
               Error_Msg_NE
                 ("aspect Indexing requires a function that applies to type&",
                   Entity (Expr), Ent);
            end if;

            if not Valid_Default_Iterator (Entity (Expr)) then
               Error_Msg_N ("improper function for default iterator", Expr);
            end if;

         else
            Default := Empty;
            declare
               I : Interp_Index;
               It : Interp;

            begin
               Get_First_Interp (Expr, I, It);
               while Present (It.Nam) loop
                  if not Check_Primitive_Function (It.Nam)
                    or else not Valid_Default_Iterator (It.Nam)
                  then
                     Remove_Interp (I);

                  elsif Present (Default) then
                     Error_Msg_N ("default iterator must be unique", Expr);

                  else
                     Default := It.Nam;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end;

            if Present (Default) then
               Set_Entity (Expr, Default);
               Set_Is_Overloaded (Expr, False);
            end if;
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

         if Ctrl = Ent
           or else Ctrl = Class_Wide_Type (Ent)
           or else
             (Ekind (Ctrl) = E_Anonymous_Access_Type
               and then
                 (Designated_Type (Ctrl) = Ent
                   or else Designated_Type (Ctrl) = Class_Wide_Type (Ent)))
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
         A : Node_Id;

      begin
         --  Nothing to do if this attribute definition clause comes from
         --  an aspect specification, since we could not be duplicating an
         --  explicit clause, and we dealt with the case of duplicated aspects
         --  in Analyze_Aspect_Specifications.

         if From_Aspect_Specification (N) then
            return False;
         end if;

         --  Otherwise current clause may duplicate previous clause, or a
         --  previously given pragma or aspect specification for the same
         --  aspect.

         A := Get_Rep_Item (U_Ent, Chars (N), Check_Parents => False);

         if Present (A) then
            Error_Msg_Name_1 := Chars (N);
            Error_Msg_Sloc := Sloc (A);

            Error_Msg_NE ("aspect% for & previously given#", N, U_Ent);
            return True;
         end if;

         return False;
      end Duplicate_Clause;

   --  Start of processing for Analyze_Attribute_Definition_Clause

   begin
      --  The following code is a defense against recursion. Not clear that
      --  this can happen legitimately, but perhaps some error situations
      --  can cause it, and we did see this recursion during testing.

      if Analyzed (N) then
         return;
      else
         Set_Analyzed (N, True);
      end if;

      --  Ignore some selected attributes in CodePeer mode since they are not
      --  relevant in this context.

      if CodePeer_Mode then
         case Id is

            --  Ignore Component_Size in CodePeer mode, to avoid changing the
            --  internal representation of types by implicitly packing them.

            when Attribute_Component_Size =>
               Rewrite (N, Make_Null_Statement (Sloc (N)));
               return;

            when others =>
               null;
         end case;
      end if;

      --  Process Ignore_Rep_Clauses option

      if Ignore_Rep_Clauses then
         case Id is

            --  The following should be ignored. They do not affect legality
            --  and may be target dependent. The basic idea of -gnatI is to
            --  ignore any rep clauses that may be target dependent but do not
            --  affect legality (except possibly to be rejected because they
            --  are incompatible with the compilation target).

            when Attribute_Alignment      |
                 Attribute_Bit_Order      |
                 Attribute_Component_Size |
                 Attribute_Machine_Radix  |
                 Attribute_Object_Size    |
                 Attribute_Size           |
                 Attribute_Stream_Size    |
                 Attribute_Value_Size     =>
               Rewrite (N, Make_Null_Statement (Sloc (N)));
               return;

            --  Perhaps 'Small should not be ignored by Ignore_Rep_Clauses ???

            when Attribute_Small =>
               if Ignore_Rep_Clauses then
                  Rewrite (N, Make_Null_Statement (Sloc (N)));
                  return;
               end if;

            --  The following should not be ignored, because in the first place
            --  they are reasonably portable, and should not cause problems in
            --  compiling code from another target, and also they do affect
            --  legality, e.g. failing to provide a stream attribute for a
            --  type may make a program illegal.

            when Attribute_External_Tag        |
                 Attribute_Input               |
                 Attribute_Output              |
                 Attribute_Read                |
                 Attribute_Simple_Storage_Pool |
                 Attribute_Storage_Pool        |
                 Attribute_Storage_Size        |
                 Attribute_Write               =>
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

      --  Rep clause applies to full view of incomplete type or private type if
      --  we have one (if not, this is a premature use of the type). However,
      --  certain semantic checks need to be done on the specified entity (i.e.
      --  the private view), so we save it in Ent.

      if Is_Private_Type (Ent)
        and then Is_Derived_Type (Ent)
        and then not Is_Tagged_Type (Ent)
        and then No (Full_View (Ent))
      then
         --  If this is a private type whose completion is a derivation from
         --  another private type, there is no full view, and the attribute
         --  belongs to the type itself, not its underlying parent.

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
         --  Case of renamed object from source, this is an error

         if Comes_From_Source (Renamed_Object (Ent)) then
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

         else
            Insert_Action (N,
              Make_Attribute_Definition_Clause (Loc,
                Name       =>
                  New_Occurrence_Of (Entity (Renamed_Object (Ent)), Loc),
                Chars      => Chars (N),
                Expression => Duplicate_Subexpr (Expression (N))));
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
      Check_Restriction_No_Use_Of_Attribute (N);

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
            --  it imported.

            if Ignore_Rep_Clauses then
               if Ekind_In (U_Ent, E_Variable, E_Constant) then
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
                    ("address clause cannot be given " &
                     "for overloaded subprogram",
                     Nam);
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
                    ("?j?attaching interrupt to task entry is an " &
                     "obsolescent feature (RM J.7.1)", N);
                  Error_Msg_N
                    ("\?j?use interrupt procedure instead", N);
               end if;

            --  Case of an address clause for a controlled object which we
            --  consider to be erroneous.

            elsif Is_Controlled (Etype (U_Ent))
              or else Has_Controlled_Component (Etype (U_Ent))
            then
               Error_Msg_NE
                 ("??controlled object& must not be overlaid", Nam, U_Ent);
               Error_Msg_N
                 ("\??Program_Error will be raised at run time", Nam);
               Insert_Action (Declaration_Node (U_Ent),
                 Make_Raise_Program_Error (Loc,
                   Reason => PE_Overlaid_Controlled_Object));
               return;

            --  Case of address clause for a (non-controlled) object

            elsif
              Ekind (U_Ent) = E_Variable
                or else
              Ekind (U_Ent) = E_Constant
            then
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

                  --  Overlaying controlled objects is erroneous

                  if Present (O_Ent)
                    and then (Has_Controlled_Component (Etype (O_Ent))
                                or else Is_Controlled (Etype (O_Ent)))
                  then
                     Error_Msg_N
                       ("??cannot overlay with controlled object", Expr);
                     Error_Msg_N
                       ("\??Program_Error will be raised at run time", Expr);
                     Insert_Action (Declaration_Node (U_Ent),
                       Make_Raise_Program_Error (Loc,
                         Reason => PE_Overlaid_Controlled_Object));
                     return;

                  elsif Present (O_Ent)
                    and then Ekind (U_Ent) = E_Constant
                    and then not Is_Constant_Object (O_Ent)
                  then
                     Error_Msg_N ("??constant overlays a variable", Expr);

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

                  --  Here we are checking for explicit overlap of one variable
                  --  by another, and if we find this then mark the overlapped
                  --  variable as also being volatile to prevent unwanted
                  --  optimizations. This is a significant pessimization so
                  --  avoid it when there is an offset, i.e. when the object
                  --  is composite; they cannot be optimized easily anyway.

                  if Present (O_Ent)
                    and then Is_Object (O_Ent)
                    and then not Off

                    --  The following test is an expedient solution to what
                    --  is really a problem in CodePeer. Suppressing the
                    --  Set_Treat_As_Volatile call here prevents later
                    --  generation (in some cases) of trees that CodePeer
                    --  should, but currently does not, handle correctly.
                    --  This test should probably be removed when CodePeer
                    --  is improved, just because we want the tree CodePeer
                    --  analyzes to match the tree for which we generate code
                    --  as closely as is practical. ???

                    and then not CodePeer_Mode
                  then
                     --  ??? O_Ent might not be in current unit

                     Set_Treat_As_Volatile (O_Ent);
                  end if;

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

                        --  If the init call is an expression with actions with
                        --  null expression, just extract the actions.

                        if Nkind (Init_Call) = N_Expression_With_Actions
                          and then
                            Nkind (Expression (Init_Call)) = N_Null_Statement
                        then
                           Append_Freeze_Actions (U_Ent, Actions (Init_Call));

                        --  General case: move Init_Call to freeze actions

                        else
                           Append_Freeze_Action (U_Ent, Init_Call);
                        end if;
                     end if;
                  end;

                  if Is_Exported (U_Ent) then
                     Error_Msg_N
                       ("& cannot be exported if an address clause is given",
                        Nam);
                     Error_Msg_N
                       ("\define and export a variable "
                        & "that holds its address instead", Nam);
                  end if;

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

                  --  If the address clause is of the form:

                  --    for Y'Address use X'Address

                  --  or

                  --    Const : constant Address := X'Address;
                  --    ...
                  --    for Y'Address use Const;

                  --  then we make an entry in the table for checking the size
                  --  and alignment of the overlaying variable. We defer this
                  --  check till after code generation to take full advantage
                  --  of the annotation done by the back end.

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

                  if Address_Clause_Overlay_Warnings
                    and then Present (O_Ent)
                    and then Is_Object (O_Ent)
                  then
                     if not Is_Generic_Type (Etype (U_Ent)) then
                        Address_Clause_Checks.Append ((N, U_Ent, O_Ent, Off));
                     end if;

                     --  If variable overlays a constant view, and we are
                     --  warning on overlays, then mark the variable as
                     --  overlaying a constant (we will give warnings later
                     --  if this variable is assigned).

                     if Is_Constant_Object (O_Ent)
                       and then Ekind (U_Ent) = E_Variable
                     then
                        Set_Overlays_Constant (U_Ent);
                     end if;
                  end if;
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

            elsif Align /= No_Uint then
               Set_Has_Alignment_Clause (U_Ent);

               --  Tagged type case, check for attempt to set alignment to a
               --  value greater than Max_Align, and reset if so.

               if Is_Tagged_Type (U_Ent) and then Align > Max_Align then
                  Error_Msg_N
                    ("alignment for & set to Maximum_Aligment??", Nam);
                     Set_Alignment (U_Ent, Max_Align);

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

         when Attribute_Bit_Order => Bit_Order : declare
         begin
            if not Is_Record_Type (U_Ent) then
               Error_Msg_N
                 ("Bit_Order can only be defined for record type", Nam);

            elsif Duplicate_Clause then
               null;

            else
               Analyze_And_Resolve (Expr, RTE (RE_Bit_Order));

               if Etype (Expr) = Any_Type then
                  return;

               elsif not Is_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("Bit_Order requires static expression!", Expr);

               else
                  if (Expr_Value (Expr) = 0) /= Bytes_Big_Endian then
                     Set_Reverse_Bit_Order (U_Ent, True);
                  end if;
               end if;
            end if;
         end Bit_Order;

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
            Ctyp := Component_Type (Btype);

            if Duplicate_Clause then
               null;

            elsif Rep_Item_Too_Early (Btype, N) then
               null;

            elsif Csize /= No_Uint then
               Check_Size (Expr, Ctyp, Csize, Biased);

               --  For the biased case, build a declaration for a subtype that
               --  will be used to represent the biased subtype that reflects
               --  the biased representation of components. We need the subtype
               --  to get proper conversions on referencing elements of the
               --  array. Note: component size clauses are ignored in VM mode.

               if VM_Target = No_VM then
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
                     Set_Esize                     (New_Ctyp, Csize);
                     Set_RM_Size                   (New_Ctyp, Csize);
                     Init_Alignment                (New_Ctyp);
                     Set_Is_Itype                  (New_Ctyp, True);
                     Set_Associated_Node_For_Itype (New_Ctyp, U_Ent);

                     Set_Component_Type (Btype, New_Ctyp);
                     Set_Biased (New_Ctyp, N, "component size clause");
                  end if;

                  Set_Component_Size (Btype, Csize);

               --  For VM case, we ignore component size clauses

               else
                  --  Give a warning unless we are in GNAT mode, in which case
                  --  the warning is suppressed since it is not useful.

                  if not GNAT_Mode then
                     Error_Msg_N
                       ("component size ignored in this configuration??", N);
                  end if;
               end if;

               --  Deal with warning on overridden size

               if Warn_On_Overridden_Size
                 and then Has_Size_Clause (Ctyp)
                 and then RM_Size (Ctyp) /= Csize
               then
                  Error_Msg_NE
                    ("component size overrides size clause for&?S?", N, Ctyp);
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

         when Attribute_CPU => CPU :
         begin
            --  CPU attribute definition clause not allowed except from aspect
            --  specification.

            if From_Aspect_Specification (N) then
               if not Is_Task_Type (U_Ent) then
                  Error_Msg_N ("CPU can only be defined for task", Nam);

               elsif Duplicate_Clause then
                  null;

               else
                  --  The expression must be analyzed in the special manner
                  --  described in "Handling of Default and Per-Object
                  --  Expressions" in sem.ads.

                  --  The visibility to the discriminants must be restored

                  Push_Scope_And_Install_Discriminants (U_Ent);
                  Preanalyze_Spec_Expression (Expr, RTE (RE_CPU_Range));
                  Uninstall_Discriminants_And_Pop_Scope (U_Ent);

                  if not Is_Static_Expression (Expr) then
                     Check_Restriction (Static_Priorities, Expr);
                  end if;
               end if;

            else
               Error_Msg_N
                 ("attribute& cannot be set with definition clause", N);
            end if;
         end CPU;

         ----------------------
         -- Default_Iterator --
         ----------------------

         when Attribute_Default_Iterator =>  Default_Iterator : declare
            Func : Entity_Id;

         begin
            if not Is_Tagged_Type (U_Ent) then
               Error_Msg_N
                 ("aspect Default_Iterator applies to  tagged type", Nam);
            end if;

            Check_Iterator_Functions;

            Analyze (Expr);

            if not Is_Entity_Name (Expr)
              or else Ekind (Entity (Expr)) /= E_Function
            then
               Error_Msg_N ("aspect Iterator must be a function", Expr);
            else
               Func := Entity (Expr);
            end if;

            if No (First_Formal (Func))
              or else Etype (First_Formal (Func)) /= U_Ent
            then
               Error_Msg_NE
                 ("Default Iterator must be a primitive of&", Func, U_Ent);
            end if;
         end Default_Iterator;

         ------------------------
         -- Dispatching_Domain --
         ------------------------

         when Attribute_Dispatching_Domain => Dispatching_Domain :
         begin
            --  Dispatching_Domain attribute definition clause not allowed
            --  except from aspect specification.

            if From_Aspect_Specification (N) then
               if not Is_Task_Type (U_Ent) then
                  Error_Msg_N ("Dispatching_Domain can only be defined" &
                               "for task",
                               Nam);

               elsif Duplicate_Clause then
                  null;

               else
                  --  The expression must be analyzed in the special manner
                  --  described in "Handling of Default and Per-Object
                  --  Expressions" in sem.ads.

                  --  The visibility to the discriminants must be restored

                  Push_Scope_And_Install_Discriminants (U_Ent);

                  Preanalyze_Spec_Expression
                    (Expr, RTE (RE_Dispatching_Domain));

                  Uninstall_Discriminants_And_Pop_Scope (U_Ent);
               end if;

            else
               Error_Msg_N
                 ("attribute& cannot be set with definition clause", N);
            end if;
         end Dispatching_Domain;

         ------------------
         -- External_Tag --
         ------------------

         when Attribute_External_Tag => External_Tag :
         begin
            if not Is_Tagged_Type (U_Ent) then
               Error_Msg_N ("should be a tagged type", Nam);
            end if;

            if Duplicate_Clause then
               null;

            else
               Analyze_And_Resolve (Expr, Standard_String);

               if not Is_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("static string required for tag name!", Nam);
               end if;

               if VM_Target = No_VM then
                  Set_Has_External_Tag_Rep_Clause (U_Ent);
               else
                  Error_Msg_Name_1 := Attr;
                  Error_Msg_N
                    ("% attribute unsupported in this configuration", Nam);
               end if;

               if not Is_Library_Level_Entity (U_Ent) then
                  Error_Msg_NE
                    ("??non-unique external tag supplied for &", N, U_Ent);
                  Error_Msg_N
                       ("\??same external tag applies to all "
                        & "subprogram calls", N);
                  Error_Msg_N
                    ("\??corresponding internal tag cannot be obtained", N);
               end if;
            end if;
         end External_Tag;

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

         when Attribute_Interrupt_Priority => Interrupt_Priority :
         begin
            --  Interrupt_Priority attribute definition clause not allowed
            --  except from aspect specification.

            if From_Aspect_Specification (N) then
               if not (Is_Protected_Type (U_Ent)
                        or else Is_Task_Type (U_Ent))
               then
                  Error_Msg_N
                    ("Interrupt_Priority can only be defined for task" &
                     "and protected object",
                     Nam);

               elsif Duplicate_Clause then
                  null;

               else
                  --  The expression must be analyzed in the special manner
                  --  described in "Handling of Default and Per-Object
                  --  Expressions" in sem.ads.

                  --  The visibility to the discriminants must be restored

                  Push_Scope_And_Install_Discriminants (U_Ent);

                  Preanalyze_Spec_Expression
                    (Expr, RTE (RE_Interrupt_Priority));

                  Uninstall_Discriminants_And_Pop_Scope (U_Ent);
               end if;

            else
               Error_Msg_N
                 ("attribute& cannot be set with definition clause", N);
            end if;
         end Interrupt_Priority;

         ----------------------
         -- Iterator_Element --
         ----------------------

         when Attribute_Iterator_Element =>
            Analyze (Expr);

            if not Is_Entity_Name (Expr)
              or else not Is_Type (Entity (Expr))
            then
               Error_Msg_N ("aspect Iterator_Element must be a type", Expr);
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

            elsif Radix /= No_Uint then
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

               if Size /= 8
                    and then
                  Size /= 16
                    and then
                  Size /= 32
                    and then
                  UI_Mod (Size, 64) /= 0
               then
                  Error_Msg_N
                    ("Object_Size must be 8, 16, 32, or multiple of 64",
                     Expr);
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

         when Attribute_Priority => Priority :
         begin
            --  Priority attribute definition clause not allowed except from
            --  aspect specification.

            if From_Aspect_Specification (N) then
               if not (Is_Protected_Type (U_Ent)
                        or else Is_Task_Type (U_Ent)
                        or else Ekind (U_Ent) = E_Procedure)
               then
                  Error_Msg_N
                    ("Priority can only be defined for task and protected " &
                     "object",
                     Nam);

               elsif Duplicate_Clause then
                  null;

               else
                  --  The expression must be analyzed in the special manner
                  --  described in "Handling of Default and Per-Object
                  --  Expressions" in sem.ads.

                  --  The visibility to the discriminants must be restored

                  Push_Scope_And_Install_Discriminants (U_Ent);
                  Preanalyze_Spec_Expression (Expr, Standard_Integer);
                  Uninstall_Discriminants_And_Pop_Scope (U_Ent);

                  if not Is_Static_Expression (Expr) then
                     Check_Restriction (Static_Priorities, Expr);
                  end if;
               end if;

            else
               Error_Msg_N
                 ("attribute& cannot be set with definition clause", N);
            end if;
         end Priority;

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

         when Attribute_Scalar_Storage_Order => Scalar_Storage_Order : declare
         begin
            if not (Is_Record_Type (U_Ent) or else Is_Array_Type (U_Ent)) then
               Error_Msg_N
                 ("Scalar_Storage_Order can only be defined for "
                  & "record or array type", Nam);

            elsif Duplicate_Clause then
               null;

            else
               Analyze_And_Resolve (Expr, RTE (RE_Bit_Order));

               if Etype (Expr) = Any_Type then
                  return;

               elsif not Is_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("Scalar_Storage_Order requires static expression!", Expr);

               elsif (Expr_Value (Expr) = 0) /= Bytes_Big_Endian then

                  --  Here for the case of a non-default (i.e. non-confirming)
                  --  Scalar_Storage_Order attribute definition.

                  if Support_Nondefault_SSO_On_Target then
                     Set_Reverse_Storage_Order (Base_Type (U_Ent), True);
                  else
                     Error_Msg_N
                       ("non-default Scalar_Storage_Order "
                        & "not supported on target", Expr);
                  end if;
               end if;
            end if;
         end Scalar_Storage_Order;

         ----------
         -- Size --
         ----------

         --  Size attribute definition clause

         when Attribute_Size => Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Etyp   : Entity_Id;
            Biased : Boolean;

         begin
            FOnly := True;

            if Duplicate_Clause then
               null;

            elsif not Is_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Variable
              and then Ekind (U_Ent) /= E_Constant
            then
               Error_Msg_N ("size cannot be given for &", Nam);

            elsif Is_Array_Type (U_Ent)
              and then not Is_Constrained (U_Ent)
            then
               Error_Msg_N
                 ("size cannot be given for unconstrained array", Nam);

            elsif Size /= No_Uint then
               if VM_Target /= No_VM and then not GNAT_Mode then

                  --  Size clause is not handled properly on VM targets.
                  --  Display a warning unless we are in GNAT mode, in which
                  --  case this is useless.

                  Error_Msg_N
                    ("size clauses are ignored in this configuration??", N);
               end if;

               if Is_Type (U_Ent) then
                  Etyp := U_Ent;
               else
                  Etyp := Etype (U_Ent);
               end if;

               --  Check size, note that Gigi is in charge of checking that the
               --  size of an array or record type is OK. Also we do not check
               --  the size in the ordinary fixed-point case, since it is too
               --  early to do so (there may be subsequent small clause that
               --  affects the size). We can check the size if a small clause
               --  has already been given.

               if not Is_Ordinary_Fixed_Point_Type (U_Ent)
                 or else Has_Small_Clause (U_Ent)
               then
                  Check_Size (Expr, Etyp, Size, Biased);
                  Set_Biased (U_Ent, N, "size clause", Biased);
               end if;

               --  For types set RM_Size and Esize if possible

               if Is_Type (U_Ent) then
                  Set_RM_Size (U_Ent, Size);

                  --  For elementary types, increase Object_Size to power of 2,
                  --  but not less than a storage unit in any case (normally
                  --  this means it will be byte addressable).

                  --  For all other types, nothing else to do, we leave Esize
                  --  (object size) unset, the back end will set it from the
                  --  size and alignment in an appropriate manner.

                  --  In both cases, we check whether the alignment must be
                  --  reset in the wake of the size change.

                  if Is_Elementary_Type (U_Ent) then
                     if Size <= System_Storage_Unit then
                        Init_Esize (U_Ent, System_Storage_Unit);
                     elsif Size <= 16 then
                        Init_Esize (U_Ent, 16);
                     elsif Size <= 32 then
                        Init_Esize (U_Ent, 32);
                     else
                        Set_Esize  (U_Ent, (Size + 63) / 64 * 64);
                     end if;

                     Alignment_Check_For_Size_Change (U_Ent, Esize (U_Ent));
                  else
                     Alignment_Check_For_Size_Change (U_Ent, Size);
                  end if;

               --  For objects, set Esize only

               else
                  if Is_Elementary_Type (Etyp) then
                     if Size /= System_Storage_Unit
                          and then
                        Size /= System_Storage_Unit * 2
                          and then
                        Size /= System_Storage_Unit * 4
                           and then
                        Size /= System_Storage_Unit * 8
                     then
                        Error_Msg_Uint_1 := UI_From_Int (System_Storage_Unit);
                        Error_Msg_Uint_2 := Error_Msg_Uint_1 * 8;
                        Error_Msg_N
                          ("size for primitive object must be a power of 2"
                            & " in the range ^-^", N);
                     end if;
                  end if;

                  Set_Esize (U_Ent, Size);
               end if;

               Set_Has_Size_Clause (U_Ent);
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

            elsif not Is_Static_Expression (Expr) then
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

         when Attribute_Storage_Pool | Attribute_Simple_Storage_Pool => declare
            Pool : Entity_Id;
            T    : Entity_Id;

         begin
            if Ekind (U_Ent) = E_Access_Subprogram_Type then
               Error_Msg_N
                 ("storage pool cannot be given for access-to-subprogram type",
                  Nam);
               return;

            elsif not
              Ekind_In (U_Ent, E_Access_Type, E_General_Access_Type)
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

               if not Present (Get_Rep_Pragma
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

            if RTE_Available (RE_Stack_Bounded_Pool)
              and then Base_Type (T) = RTE (RE_Stack_Bounded_Pool)
            then
               Error_Msg_N ("non-shareable internal Pool", Expr);
               return;
            end if;

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
                  Set_Associated_Storage_Pool (U_Ent, Pool);
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

               Set_Associated_Storage_Pool (U_Ent, Pool);

            elsif Nkind (Expr) = N_Type_Conversion
              and then Is_Entity_Name (Expression (Expr))
              and then Nkind (Original_Node (Expr)) = N_Attribute_Reference
            then
               Pool := Entity (Expression (Expr));
               Set_Associated_Storage_Pool (U_Ent, Pool);

            else
               Error_Msg_N ("incorrect reference to a Storage Pool", Expr);
               return;
            end if;
         end;

         ------------------
         -- Storage_Size --
         ------------------

         --  Storage_Size attribute definition clause

         when Attribute_Storage_Size => Storage_Size : declare
            Btype : constant Entity_Id := Base_Type (U_Ent);

         begin
            if Is_Task_Type (U_Ent) then

               --  Check obsolescent (but never obsolescent if from aspect!)

               if not From_Aspect_Specification (N) then
                  Check_Restriction (No_Obsolescent_Features, N);

                  if Warn_On_Obsolescent_Feature then
                     Error_Msg_N
                       ("?j?storage size clause for task is an " &
                        "obsolescent feature (RM J.9)", N);
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
               if Size /= System_Storage_Unit
                    and then
                  Size /= System_Storage_Unit * 2
                    and then
                  Size /= System_Storage_Unit * 4
                     and then
                  Size /= System_Storage_Unit * 8
               then
                  Error_Msg_Uint_1 := UI_From_Int (System_Storage_Unit);
                  Error_Msg_N
                    ("stream size for elementary type must be a"
                       & " power of 2 and at least ^", N);

               elsif RM_Size (U_Ent) > Size then
                  Error_Msg_Uint_1 := RM_Size (U_Ent);
                  Error_Msg_N
                    ("stream size for elementary type must be a"
                       & " power of 2 and at least ^", N);
               end if;

               Set_Has_Stream_Size_Clause (U_Ent);

            else
               Error_Msg_N ("Stream_Size cannot be given for &", Nam);
            end if;
         end Stream_Size;

         ----------------
         -- Value_Size --
         ----------------

         --  Value_Size attribute definition clause

         when Attribute_Value_Size => Value_Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Biased : Boolean;

         begin
            if not Is_Type (U_Ent) then
               Error_Msg_N ("Value_Size cannot be given for &", Nam);

            elsif Duplicate_Clause then
               null;

            elsif Is_Array_Type (U_Ent)
              and then not Is_Constrained (U_Ent)
            then
               Error_Msg_N
                 ("Value_Size cannot be given for unconstrained array", Nam);

            else
               if Is_Elementary_Type (U_Ent) then
                  Check_Size (Expr, U_Ent, Size, Biased);
                  Set_Biased (U_Ent, N, "value size clause", Biased);
               end if;

               Set_RM_Size (U_Ent, Size);
            end if;
         end Value_Size;

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
      --  Analyze and check we get right type, note that this implements the
      --  requirement (RM 13.8(1)) that Machine_Code be with'ed, since that
      --  is the only way that Asm_Insn could possibly be visible.

      Analyze_And_Resolve (Expression (N));

      if Etype (Expression (N)) = Any_Type then
         return;
      elsif Etype (Expression (N)) /= RTE (RE_Asm_Insn) then
         Error_Msg_N ("incorrect type for code statement", N);
         return;
      end if;

      Check_Code_Statement (N);

      --  Make sure we appear in the handled statement sequence of a
      --  subprogram (RM 13.8(3)).

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
              and not Nkind_In (DeclO, N_Pragma,
                                       N_Use_Package_Clause,
                                       N_Use_Type_Clause,
                                       N_Implicit_Label_Declaration)
            then
               Error_Msg_N
                 ("this declaration not allowed in machine code subprogram",
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

            --  A procedure call transformed into a code statement is OK.

            if Ada_Version >= Ada_2012
              and then Nkind (StmtO) = N_Procedure_Call_Statement
              and then Nkind (Name (StmtO)) = N_Qualified_Expression
            then
               null;

            elsif Comes_From_Source (StmtO)
              and then not Nkind_In (StmtO, N_Pragma,
                                            N_Label,
                                            N_Code_Statement)
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
      Ident    : constant Node_Id    := Identifier (N);
      Aggr     : constant Node_Id    := Array_Aggregate (N);
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

      Max_Node : Node_Id;
      --  Pointer to node for literal providing max value

   begin
      if Ignore_Rep_Clauses then
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
         Error_Msg
           ("extra parentheses surrounding aggregate not allowed",
            First_Sloc (Aggr));
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

      --  First the positional entries if any

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

            if Val = No_Uint then
               Err := True;
            elsif Val < Lo or else Hi < Val then
               Error_Msg_N ("value outside permitted range", Expr);
               Err := True;
            end if;

            Set_Enumeration_Rep (Elit, Val);
            Set_Enumeration_Rep_Expr (Elit, Expr);
            Next (Expr);
            Next (Elit);
         end loop;
      end if;

      --  Now process the named entries if present

      if Present (Component_Associations (Aggr)) then
         Assoc := First (Component_Associations (Aggr));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));

            if Present (Next (Choice)) then
               Error_Msg_N
                 ("multiple choice not allowed here", Next (Choice));
               Err := True;
            end if;

            if Nkind (Choice) = N_Others_Choice then
               Error_Msg_N ("others choice not allowed here", Choice);
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
                     if not Is_Static_Expression (Choice) then
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

                        if Val = No_Uint then
                           Err := True;

                        elsif Val < Lo or else Hi < Val then
                           Error_Msg_N ("value outside permitted range", Expr);
                           Err := True;
                        end if;

                        Set_Enumeration_Rep (Elit, Val);
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

               if Min = No_Uint then
                  Min := Val;
               end if;

               if Val /= No_Uint then
                  if Max /= No_Uint and then Val <= Max then
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

            Set_RM_Size   (Base_Type (Enumtype), RM_Size   (Enumtype));
            Set_Esize     (Base_Type (Enumtype), Esize     (Enumtype));
            Set_Alignment (Base_Type (Enumtype), Alignment (Enumtype));
         end;
      end if;

      --  We repeat the too late test in case it froze itself!

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
   begin
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
      Hbit    : Uint := Uint_0;
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

      --  We know we have a first subtype, now possibly go the the anonymous
      --  base type to determine whether Rectype is a record extension.

      Recdef := Type_Definition (Declaration_Node (Base_Type (Rectype)));
      Is_Record_Extension :=
        Nkind (Recdef) = N_Derived_Type_Definition
          and then Present (Record_Extension_Part (Recdef));

      if Present (Mod_Clause (N)) then
         declare
            Loc     : constant Source_Ptr := Sloc (N);
            M       : constant Node_Id := Mod_Clause (N);
            P       : constant List_Id := Pragmas_Before (M);
            AtM_Nod : Node_Id;

            Mod_Val : Uint;
            pragma Warnings (Off, Mod_Val);

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

            --  In ASIS_Mode mode, expansion is disabled, but we must convert
            --  the Mod clause into an alignment clause anyway, so that the
            --  back-end can compute and back-annotate properly the size and
            --  alignment of types that may include this record.

            --  This seems dubious, this destroys the source tree in a manner
            --  not detectable by ASIS ???

            if Operating_Mode = Check_Semantics and then ASIS_Mode then
               AtM_Nod :=
                 Make_Attribute_Definition_Clause (Loc,
                   Name       => New_Reference_To (Base_Type (Rectype), Loc),
                   Chars      => Name_Alignment,
                   Expression => Relocate_Node (Expression (M)));

               Set_From_At_Mod (AtM_Nod);
               Insert_After (N, AtM_Nod);
               Mod_Val := Get_Alignment_Value (Expression (AtM_Nod));
               Set_Mod_Clause (N, Empty);

            else
               --  Get the alignment value to perform error checking

               Mod_Val := Get_Alignment_Value (Expression (M));
            end if;
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

            if Posit /= No_Uint
              and then Fbit /= No_Uint
              and then Lbit /= No_Uint
            then
               if Posit < 0 then
                  Error_Msg_N
                    ("position cannot be negative", Position (CC));

               elsif Fbit < 0 then
                  Error_Msg_N
                    ("first bit cannot be negative", First_Bit (CC));

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
                                ("component clause inconsistent "
                                 & "with representation of ancestor", CC);

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
                     Set_Entity (Component_Name (CC), Comp);

                     --  Update Fbit and Lbit to the actual bit number

                     Fbit := Fbit + UI_From_Int (SSU) * Posit;
                     Lbit := Lbit + UI_From_Int (SSU) * Posit;

                     if Has_Size_Clause (Rectype)
                       and then RM_Size (Rectype) <= Lbit
                     then
                        Error_Msg_N
                          ("bit number out of range of specified size",
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
                             ("?S?component size overrides size clause for&",
                              Component_Name (CC), Etype (Comp));
                        end if;

                        --  This information is also set in the corresponding
                        --  component of the base type, found by accessing the
                        --  Original_Record_Component link if it is present.

                        Ocomp := Original_Record_Component (Comp);

                        if Hbit < Lbit then
                           Hbit := Lbit;
                        end if;

                        Check_Size
                          (Component_Name (CC),
                           Etype (Comp),
                           Esize (Comp),
                           Biased);

                        Set_Biased
                          (Comp, First_Node (CC), "component clause", Biased);

                        if Present (Ocomp) then
                           Set_Component_Clause     (Ocomp, CC);
                           Set_Component_Bit_Offset (Ocomp, Fbit);
                           Set_Normalized_First_Bit (Ocomp, Fbit mod SSU);
                           Set_Normalized_Position  (Ocomp, Fbit / SSU);
                           Set_Esize                (Ocomp, 1 + (Lbit - Fbit));

                           Set_Normalized_Position_Max
                             (Ocomp, Normalized_Position (Ocomp));

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
                  then
                     Error_Msg_Sloc := Sloc (Comp);
                     Error_Msg_NE
                       ("?C?no component clause given for & declared #",
                        N, Comp);
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;
            end if;
         end;
      end if;
   end Analyze_Record_Representation_Clause;

   -------------------------------------------
   -- Build_Invariant_Procedure_Declaration --
   -------------------------------------------

   function Build_Invariant_Procedure_Declaration
     (Typ : Entity_Id) return Node_Id
   is
      Loc           : constant Source_Ptr := Sloc (Typ);
      Object_Entity : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('I'));
      Spec          : Node_Id;
      SId           : Entity_Id;

   begin
      Set_Etype (Object_Entity, Typ);

      --  Check for duplicate definiations.

      if Has_Invariants (Typ) and then Present (Invariant_Procedure (Typ)) then
         return Empty;
      end if;

      SId :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (Typ), "Invariant"));
      Set_Has_Invariants (Typ);
      Set_Ekind (SId, E_Procedure);
      Set_Is_Invariant_Procedure (SId);
      Set_Invariant_Procedure (Typ, SId);

      Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => SId,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Object_Entity,
              Parameter_Type      => New_Occurrence_Of (Typ, Loc))));

      return Make_Subprogram_Declaration (Loc, Specification => Spec);
   end Build_Invariant_Procedure_Declaration;

   -------------------------------
   -- Build_Invariant_Procedure --
   -------------------------------

   --  The procedure that is constructed here has the form

   --  procedure typInvariant (Ixxx : typ) is
   --  begin
   --     pragma Check (Invariant, exp, "failed invariant from xxx");
   --     pragma Check (Invariant, exp, "failed invariant from xxx");
   --     ...
   --     pragma Check (Invariant, exp, "failed inherited invariant from xxx");
   --     ...
   --  end typInvariant;

   procedure Build_Invariant_Procedure (Typ : Entity_Id; N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Stmts : List_Id;
      Spec  : Node_Id;
      SId   : Entity_Id;
      PDecl : Node_Id;
      PBody : Node_Id;

      Visible_Decls : constant List_Id := Visible_Declarations (N);
      Private_Decls : constant List_Id := Private_Declarations (N);

      procedure Add_Invariants (T : Entity_Id; Inherit : Boolean);
      --  Appends statements to Stmts for any invariants in the rep item chain
      --  of the given type. If Inherit is False, then we only process entries
      --  on the chain for the type Typ. If Inherit is True, then we ignore any
      --  Invariant aspects, but we process all Invariant'Class aspects, adding
      --  "inherited" to the exception message and generating an informational
      --  message about the inheritance of an invariant.

      Object_Name : Name_Id;
      --  Name for argument of invariant procedure

      Object_Entity : Node_Id;
      --  The entity of the formal for the procedure

      --------------------
      -- Add_Invariants --
      --------------------

      procedure Add_Invariants (T : Entity_Id; Inherit : Boolean) is
         Ritem : Node_Id;
         Arg1  : Node_Id;
         Arg2  : Node_Id;
         Arg3  : Node_Id;
         Exp   : Node_Id;
         Loc   : Source_Ptr;
         Assoc : List_Id;
         Str   : String_Id;

         procedure Replace_Type_Reference (N : Node_Id);
         --  Replace a single occurrence N of the subtype name with a reference
         --  to the formal of the predicate function. N can be an identifier
         --  referencing the subtype, or a selected component, representing an
         --  appropriately qualified occurrence of the subtype name.

         procedure Replace_Type_References is
           new Replace_Type_References_Generic (Replace_Type_Reference);
         --  Traverse an expression replacing all occurrences of the subtype
         --  name with appropriate references to the object that is the formal
         --  parameter of the predicate function. Note that we must ensure
         --  that the type and entity information is properly set in the
         --  replacement node, since we will do a Preanalyze call of this
         --  expression without proper visibility of the procedure argument.

         ----------------------------
         -- Replace_Type_Reference --
         ----------------------------

         --  Note: See comments in Add_Predicates.Replace_Type_Reference
         --  regarding handling of Sloc and Comes_From_Source.

         procedure Replace_Type_Reference (N : Node_Id) is
         begin

            --  Add semantic information to node to be rewritten, for ASIS
            --  navigation needs.

            if Nkind (N) = N_Identifier then
               Set_Entity (N, T);
               Set_Etype  (N, T);

            elsif Nkind (N) = N_Selected_Component then
               Analyze (Prefix (N));
               Set_Entity (Selector_Name (N), T);
               Set_Etype  (Selector_Name (N), T);
            end if;

            --  Invariant'Class, replace with T'Class (obj)

            if Class_Present (Ritem) then
               Rewrite (N,
                 Make_Type_Conversion (Sloc (N),
                   Subtype_Mark =>
                     Make_Attribute_Reference (Sloc (N),
                       Prefix         => New_Occurrence_Of (T, Sloc (N)),
                       Attribute_Name => Name_Class),
                   Expression   => Make_Identifier (Sloc (N), Object_Name)));

               Set_Entity (Expression (N), Object_Entity);
               Set_Etype  (Expression (N), Typ);

            --  Invariant, replace with obj

            else
               Rewrite (N, Make_Identifier (Sloc (N), Object_Name));
               Set_Entity (N, Object_Entity);
               Set_Etype  (N, Typ);
            end if;

            Set_Comes_From_Source (N, True);
         end Replace_Type_Reference;

      --  Start of processing for Add_Invariants

      begin
         Ritem := First_Rep_Item (T);
         while Present (Ritem) loop
            if Nkind (Ritem) = N_Pragma
              and then Pragma_Name (Ritem) = Name_Invariant
            then
               Arg1 := First (Pragma_Argument_Associations (Ritem));
               Arg2 := Next (Arg1);
               Arg3 := Next (Arg2);

               Arg1 := Get_Pragma_Arg (Arg1);
               Arg2 := Get_Pragma_Arg (Arg2);

               --  For Inherit case, ignore Invariant, process only Class case

               if Inherit then
                  if not Class_Present (Ritem) then
                     goto Continue;
                  end if;

               --  For Inherit false, process only item for right type

               else
                  if Entity (Arg1) /= Typ then
                     goto Continue;
                  end if;
               end if;

               if No (Stmts) then
                  Stmts := Empty_List;
               end if;

               Exp := New_Copy_Tree (Arg2);

               --  Preserve sloc of original pragma Invariant

               Loc := Sloc (Ritem);

               --  We need to replace any occurrences of the name of the type
               --  with references to the object, converted to type'Class in
               --  the case of Invariant'Class aspects.

               Replace_Type_References (Exp, Chars (T));

               --  If this invariant comes from an aspect, find the aspect
               --  specification, and replace the saved expression because
               --  we need the subtype references replaced for the calls to
               --  Preanalyze_Spec_Expressin in Check_Aspect_At_Freeze_Point
               --  and Check_Aspect_At_End_Of_Declarations.

               if From_Aspect_Specification (Ritem) then
                  declare
                     Aitem : Node_Id;

                  begin
                     --  Loop to find corresponding aspect, note that this
                     --  must be present given the pragma is marked delayed.

                     Aitem := Next_Rep_Item (Ritem);
                     while Present (Aitem) loop
                        if Nkind (Aitem) = N_Aspect_Specification
                          and then Aspect_Rep_Item (Aitem) = Ritem
                        then
                           Set_Entity
                             (Identifier (Aitem), New_Copy_Tree (Exp));
                           exit;
                        end if;

                        Aitem := Next_Rep_Item (Aitem);
                     end loop;
                  end;
               end if;

               --  Now we need to preanalyze the expression to properly capture
               --  the visibility in the visible part. The expression will not
               --  be analyzed for real until the body is analyzed, but that is
               --  at the end of the private part and has the wrong visibility.

               Set_Parent (Exp, N);
               Preanalyze_Assert_Expression (Exp, Standard_Boolean);

               --  In ASIS mode, even if assertions are not enabled, we must
               --  analyze the original expression in the aspect specification
               --  because it is part of the original tree.

               if ASIS_Mode then
                  declare
                     Inv : constant Node_Id :=
                             Expression (Corresponding_Aspect (Ritem));
                  begin
                     Replace_Type_References (Inv, Chars (T));
                     Preanalyze_Assert_Expression (Inv, Standard_Boolean);
                  end;
               end if;

               --  Build first two arguments for Check pragma

               Assoc := New_List (
                 Make_Pragma_Argument_Association (Loc,
                   Expression => Make_Identifier (Loc, Name_Invariant)),
                 Make_Pragma_Argument_Association (Loc,
                   Expression => Exp));

               --  Add message if present in Invariant pragma

               if Present (Arg3) then
                  Str := Strval (Get_Pragma_Arg (Arg3));

                  --  If inherited case, and message starts "failed invariant",
                  --  change it to be "failed inherited invariant".

                  if Inherit then
                     String_To_Name_Buffer (Str);

                     if Name_Buffer (1 .. 16) = "failed invariant" then
                        Insert_Str_In_Name_Buffer ("inherited ", 8);
                        Str := String_From_Name_Buffer;
                     end if;
                  end if;

                  Append_To (Assoc,
                    Make_Pragma_Argument_Association (Loc,
                      Expression => Make_String_Literal (Loc, Str)));
               end if;

               --  Add Check pragma to list of statements

               Append_To (Stmts,
                 Make_Pragma (Loc,
                   Pragma_Identifier            =>
                     Make_Identifier (Loc, Name_Check),
                   Pragma_Argument_Associations => Assoc));

               --  If Inherited case and option enabled, output info msg. Note
               --  that we know this is a case of Invariant'Class.

               if Inherit and Opt.List_Inherited_Aspects then
                  Error_Msg_Sloc := Sloc (Ritem);
                  Error_Msg_N
                    ("?L?info: & inherits `Invariant''Class` aspect from #",
                     Typ);
               end if;
            end if;

         <<Continue>>
            Next_Rep_Item (Ritem);
         end loop;
      end Add_Invariants;

   --  Start of processing for Build_Invariant_Procedure

   begin
      Stmts := No_List;
      PDecl := Empty;
      PBody := Empty;
      SId   := Empty;

      --  If the aspect specification exists for some view of the type, the
      --  declaration for the procedure has been created.

      if Has_Invariants (Typ) then
         SId := Invariant_Procedure (Typ);
      end if;

      if Present (SId) then
         PDecl := Unit_Declaration_Node (SId);
      else
         PDecl := Build_Invariant_Procedure_Declaration (Typ);
      end if;

      --  Recover formal of procedure, for use in the calls to invariant
      --  functions (including inherited ones).

      Object_Entity :=
        Defining_Identifier
          (First (Parameter_Specifications (Specification (PDecl))));
      Object_Name := Chars (Object_Entity);

      --  Add invariants for the current type

      Add_Invariants (Typ, Inherit => False);

      --  Add invariants for parent types

      declare
         Current_Typ : Entity_Id;
         Parent_Typ  : Entity_Id;

      begin
         Current_Typ := Typ;
         loop
            Parent_Typ := Etype (Current_Typ);

            if Is_Private_Type (Parent_Typ)
              and then Present (Full_View (Base_Type (Parent_Typ)))
            then
               Parent_Typ := Full_View (Base_Type (Parent_Typ));
            end if;

            exit when Parent_Typ = Current_Typ;

            Current_Typ := Parent_Typ;
            Add_Invariants (Current_Typ, Inherit => True);
         end loop;
      end;

      --  Build the procedure if we generated at least one Check pragma

      if Stmts /= No_List then
         Spec  := Copy_Separate_Tree (Specification (PDecl));

         PBody :=
           Make_Subprogram_Body (Loc,
             Specification              => Spec,
             Declarations               => Empty_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts));

         --  Insert procedure declaration and spec at the appropriate points.
         --  If declaration is already analyzed, it was processed by the
         --  generated pragma.

         if Present (Private_Decls) then

            --  The spec goes at the end of visible declarations, but they have
            --  already been analyzed, so we need to explicitly do the analyze.

            if not Analyzed (PDecl) then
               Append_To (Visible_Decls, PDecl);
               Analyze (PDecl);
            end if;

            --  The body goes at the end of the private declarations, which we
            --  have not analyzed yet, so we do not need to perform an explicit
            --  analyze call. We skip this if there are no private declarations
            --  (this is an error that will be caught elsewhere);

            Append_To (Private_Decls, PBody);

            --  If the invariant appears on the full view of a type, the
            --  analysis of the private part is complete, and we must
            --  analyze the new body explicitly.

            if In_Private_Part (Current_Scope) then
               Analyze (PBody);
            end if;

         --  If there are no private declarations this may be an error that
         --  will be diagnosed elsewhere. However, if this is a non-private
         --  type that inherits invariants, it needs no completion and there
         --  may be no private part. In this case insert invariant procedure
         --  at end of current declarative list, and analyze at once, given
         --  that the type is about to be frozen.

         elsif not Is_Private_Type (Typ) then
            Append_To (Visible_Decls, PDecl);
            Append_To (Visible_Decls, PBody);
            Analyze (PDecl);
            Analyze (PBody);
         end if;
      end if;
   end Build_Invariant_Procedure;

   -------------------------------
   -- Build_Predicate_Functions --
   -------------------------------

   --  The procedures that are constructed here have the form:

   --    function typPredicate (Ixxx : typ) return Boolean is
   --    begin
   --       return
   --          exp1 and then exp2 and then ...
   --          and then typ1Predicate (typ1 (Ixxx))
   --          and then typ2Predicate (typ2 (Ixxx))
   --          and then ...;
   --    end typPredicate;

   --  Here exp1, and exp2 are expressions from Predicate pragmas. Note that
   --  this is the point at which these expressions get analyzed, providing the
   --  required delay, and typ1, typ2, are entities from which predicates are
   --  inherited. Note that we do NOT generate Check pragmas, that's because we
   --  use this function even if checks are off, e.g. for membership tests.

   --  If the expression has at least one Raise_Expression, then we also build
   --  the typPredicateM version of the function, in which any occurrence of a
   --  Raise_Expression is converted to "return False".

   procedure Build_Predicate_Functions (Typ : Entity_Id; N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Typ);

      Expr : Node_Id;
      --  This is the expression for the result of the function. It is
      --  is build by connecting the component predicates with AND THEN.

      Expr_M : Node_Id;
      --  This is the corresponding return expression for the Predicate_M
      --  function. It differs in that raise expressions are marked for
      --  special expansion (see Process_REs).

      Object_Name : constant Name_Id := New_Internal_Name ('I');
      --  Name for argument of Predicate procedure. Note that we use the same
      --  name for both predicate procedure. That way the reference within the
      --  predicate expression is the same in both functions.

      Object_Entity : constant Entity_Id :=
                        Make_Defining_Identifier (Loc, Chars => Object_Name);
      --  Entity for argument of Predicate procedure

      Object_Entity_M : constant Entity_Id :=
                         Make_Defining_Identifier (Loc, Chars => Object_Name);
      --  Entity for argument of Predicate_M procedure

      Raise_Expression_Present : Boolean := False;
      --  Set True if Expr has at least one Raise_Expression

      Static_Predic : Node_Id := Empty;
      --  Set to N_Pragma node for a static predicate if one is encountered

      procedure Add_Call (T : Entity_Id);
      --  Includes a call to the predicate function for type T in Expr if T
      --  has predicates and Predicate_Function (T) is non-empty.

      procedure Add_Predicates;
      --  Appends expressions for any Predicate pragmas in the rep item chain
      --  Typ to Expr. Note that we look only at items for this exact entity.
      --  Inheritance of predicates for the parent type is done by calling the
      --  Predicate_Function of the parent type, using Add_Call above.

      function Test_RE (N : Node_Id) return Traverse_Result;
      --  Used in Test_REs, tests one node for being a raise expression, and if
      --  so sets Raise_Expression_Present True.

      procedure Test_REs is new Traverse_Proc (Test_RE);
      --  Tests to see if Expr contains any raise expressions

      function Process_RE (N : Node_Id) return Traverse_Result;
      --  Used in Process REs, tests if node N is a raise expression, and if
      --  so, marks it to be converted to return False.

      procedure Process_REs is new Traverse_Proc (Process_RE);
      --  Marks any raise expressions in Expr_M to return False

      --------------
      -- Add_Call --
      --------------

      procedure Add_Call (T : Entity_Id) is
         Exp : Node_Id;

      begin
         if Present (T) and then Present (Predicate_Function (T)) then
            Set_Has_Predicates (Typ);

            --  Build the call to the predicate function of T

            Exp :=
              Make_Predicate_Call
                (T, Convert_To (T, Make_Identifier (Loc, Object_Name)));

            --  Add call to evolving expression, using AND THEN if needed

            if No (Expr) then
               Expr := Exp;
            else
               Expr :=
                 Make_And_Then (Loc,
                   Left_Opnd  => Relocate_Node (Expr),
                   Right_Opnd => Exp);
            end if;

            --  Output info message on inheritance if required. Note we do not
            --  give this information for generic actual types, since it is
            --  unwelcome noise in that case in instantiations. We also
            --  generally suppress the message in instantiations, and also
            --  if it involves internal names.

            if Opt.List_Inherited_Aspects
              and then not Is_Generic_Actual_Type (Typ)
              and then Instantiation_Depth (Sloc (Typ)) = 0
              and then not Is_Internal_Name (Chars (T))
              and then not Is_Internal_Name (Chars (Typ))
            then
               Error_Msg_Sloc := Sloc (Predicate_Function (T));
               Error_Msg_Node_2 := T;
               Error_Msg_N ("info: & inherits predicate from & #?L?", Typ);
            end if;
         end if;
      end Add_Call;

      --------------------
      -- Add_Predicates --
      --------------------

      procedure Add_Predicates is
         Ritem : Node_Id;
         Arg1  : Node_Id;
         Arg2  : Node_Id;

         procedure Replace_Type_Reference (N : Node_Id);
         --  Replace a single occurrence N of the subtype name with a reference
         --  to the formal of the predicate function. N can be an identifier
         --  referencing the subtype, or a selected component, representing an
         --  appropriately qualified occurrence of the subtype name.

         procedure Replace_Type_References is
           new Replace_Type_References_Generic (Replace_Type_Reference);
         --  Traverse an expression changing every occurrence of an identifier
         --  whose name matches the name of the subtype with a reference to
         --  the formal parameter of the predicate function.

         ----------------------------
         -- Replace_Type_Reference --
         ----------------------------

         procedure Replace_Type_Reference (N : Node_Id) is
         begin
            Rewrite (N, Make_Identifier (Sloc (N), Object_Name));
            --  Use the Sloc of the usage name, not the defining name

            Set_Etype (N, Typ);
            Set_Entity (N, Object_Entity);

            --  We want to treat the node as if it comes from source, so that
            --  ASIS will not ignore it

            Set_Comes_From_Source (N, True);
         end Replace_Type_Reference;

      --  Start of processing for Add_Predicates

      begin
         Ritem := First_Rep_Item (Typ);
         while Present (Ritem) loop
            if Nkind (Ritem) = N_Pragma
              and then Pragma_Name (Ritem) = Name_Predicate
            then
               --  Save the static predicate of the type for diagnostics and
               --  error reporting purposes.

               if Present (Corresponding_Aspect (Ritem))
                 and then Chars (Identifier (Corresponding_Aspect (Ritem))) =
                            Name_Static_Predicate
               then
                  Static_Predic := Ritem;
               end if;

               --  Acquire arguments

               Arg1 := First (Pragma_Argument_Associations (Ritem));
               Arg2 := Next (Arg1);

               Arg1 := Get_Pragma_Arg (Arg1);
               Arg2 := Get_Pragma_Arg (Arg2);

               --  See if this predicate pragma is for the current type or for
               --  its full view. A predicate on a private completion is placed
               --  on the partial view beause this is the visible entity that
               --  is frozen.

               if Entity (Arg1) = Typ
                 or else Full_View (Entity (Arg1)) = Typ
               then
                  --  We have a match, this entry is for our subtype

                  --  We need to replace any occurrences of the name of the
                  --  type with references to the object.

                  Replace_Type_References (Arg2, Chars (Typ));

                  --  If this predicate comes from an aspect, find the aspect
                  --  specification, and replace the saved expression because
                  --  we need the subtype references replaced for the calls to
                  --  Preanalyze_Spec_Expressin in Check_Aspect_At_Freeze_Point
                  --  and Check_Aspect_At_End_Of_Declarations.

                  if From_Aspect_Specification (Ritem) then
                     declare
                        Aitem : Node_Id;

                     begin
                        --  Loop to find corresponding aspect, note that this
                        --  must be present given the pragma is marked delayed.

                        Aitem := Next_Rep_Item (Ritem);
                        loop
                           if Nkind (Aitem) = N_Aspect_Specification
                             and then Aspect_Rep_Item (Aitem) = Ritem
                           then
                              Set_Entity
                                (Identifier (Aitem), New_Copy_Tree (Arg2));
                              exit;
                           end if;

                           Aitem := Next_Rep_Item (Aitem);
                        end loop;
                     end;
                  end if;

                  --  Now we can add the expression

                  if No (Expr) then
                     Expr := Relocate_Node (Arg2);

                  --  There already was a predicate, so add to it

                  else
                     Expr :=
                       Make_And_Then (Loc,
                         Left_Opnd  => Relocate_Node (Expr),
                         Right_Opnd => Relocate_Node (Arg2));
                  end if;
               end if;
            end if;

            Next_Rep_Item (Ritem);
         end loop;
      end Add_Predicates;

      ----------------
      -- Process_RE --
      ----------------

      function Process_RE (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Raise_Expression then
            Set_Convert_To_Return_False (N);
            return Skip;
         else
            return OK;
         end if;
      end Process_RE;

      -------------
      -- Test_RE --
      -------------

      function Test_RE (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Raise_Expression then
            Raise_Expression_Present := True;
            return Abandon;
         else
            return OK;
         end if;
      end Test_RE;

   --  Start of processing for Build_Predicate_Functions

   begin
      --  Return if already built or if type does not have predicates

      if not Has_Predicates (Typ)
        or else Present (Predicate_Function (Typ))
      then
         return;
      end if;

      --  Prepare to construct predicate expression

      Expr := Empty;

      --  Add Predicates for the current type

      Add_Predicates;

      --  Add predicates for ancestor if present

      declare
         Atyp : constant Entity_Id := Nearest_Ancestor (Typ);
      begin
         if Present (Atyp) then
            Add_Call (Atyp);
         end if;
      end;

      --  Case where predicates are present

      if Present (Expr) then

         --  Test for raise expression present

         Test_REs (Expr);

         --  If raise expression is present, capture a copy of Expr for use
         --  in building the predicateM function version later on. For this
         --  copy we replace references to Object_Entity by Object_Entity_M.

         if Raise_Expression_Present then
            declare
               Map : constant Elist_Id := New_Elmt_List;
            begin
               Append_Elmt (Object_Entity, Map);
               Append_Elmt (Object_Entity_M, Map);
               Expr_M := New_Copy_Tree (Expr, Map => Map);
            end;
         end if;

         --  Build the main predicate function

         declare
            SId : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (Typ), "Predicate"));
            --  The entity for the the function spec

            SIdB : constant Entity_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Typ), "Predicate"));
            --  The entity for the function body

            Spec  : Node_Id;
            FDecl : Node_Id;
            FBody : Node_Id;

         begin
            --  Build function declaration

            Set_Ekind (SId, E_Function);
            Set_Is_Predicate_Function (SId);
            Set_Predicate_Function (Typ, SId);

            --  The predicate function is shared between views of a type

            if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
               Set_Predicate_Function (Full_View (Typ), SId);
            end if;

            Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name       => SId,
                Parameter_Specifications => New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier => Object_Entity,
                    Parameter_Type      => New_Occurrence_Of (Typ, Loc))),
                Result_Definition        =>
                  New_Occurrence_Of (Standard_Boolean, Loc));

            FDecl :=
              Make_Subprogram_Declaration (Loc,
                Specification => Spec);

            --  Build function body

            Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name       => SIdB,
                Parameter_Specifications => New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Object_Name),
                    Parameter_Type =>
                      New_Occurrence_Of (Typ, Loc))),
                Result_Definition        =>
                  New_Occurrence_Of (Standard_Boolean, Loc));

            FBody :=
              Make_Subprogram_Body (Loc,
                Specification              => Spec,
                Declarations               => Empty_List,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (
                      Make_Simple_Return_Statement (Loc,
                        Expression => Expr))));

            --  Insert declaration before freeze node and body after

            Insert_Before_And_Analyze (N, FDecl);
            Insert_After_And_Analyze  (N, FBody);
         end;

         --  Test for raise expressions present and if so build M version

         if Raise_Expression_Present then
            declare
               SId : constant Entity_Id :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (Typ), "PredicateM"));
               --  The entity for the the function spec

               SIdB : constant Entity_Id :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (Typ), "PredicateM"));
               --  The entity for the function body

               Spec  : Node_Id;
               FDecl : Node_Id;
               FBody : Node_Id;
               BTemp : Entity_Id;

            begin
               --  Mark any raise expressions for special expansion

               Process_REs (Expr_M);

               --  Build function declaration

               Set_Ekind (SId, E_Function);
               Set_Is_Predicate_Function_M (SId);
               Set_Predicate_Function_M (Typ, SId);

               --  The predicate function is shared between views of a type

               if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
                  Set_Predicate_Function_M (Full_View (Typ), SId);
               end if;

               Spec :=
                 Make_Function_Specification (Loc,
                   Defining_Unit_Name       => SId,
                   Parameter_Specifications => New_List (
                     Make_Parameter_Specification (Loc,
                       Defining_Identifier => Object_Entity_M,
                       Parameter_Type      => New_Occurrence_Of (Typ, Loc))),
                   Result_Definition        =>
                     New_Occurrence_Of (Standard_Boolean, Loc));

               FDecl :=
                 Make_Subprogram_Declaration (Loc,
                   Specification => Spec);

               --  Build function body

               Spec :=
                 Make_Function_Specification (Loc,
                   Defining_Unit_Name       => SIdB,
                   Parameter_Specifications => New_List (
                     Make_Parameter_Specification (Loc,
                       Defining_Identifier =>
                         Make_Defining_Identifier (Loc, Object_Name),
                       Parameter_Type =>
                         New_Occurrence_Of (Typ, Loc))),
                   Result_Definition        =>
                     New_Occurrence_Of (Standard_Boolean, Loc));

               --  Build the body, we declare the boolean expression before
               --  doing the return, because we are not really confident of
               --  what happens if a return appears within a return!

               BTemp :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_Internal_Name ('B'));

               FBody :=
                 Make_Subprogram_Body (Loc,
                   Specification              => Spec,

                   Declarations               => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => BTemp,
                       Constant_Present    => True,
                         Object_Definition =>
                           New_Reference_To (Standard_Boolean, Loc),
                         Expression        => Expr_M)),

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Simple_Return_Statement (Loc,
                           Expression => New_Reference_To (BTemp, Loc)))));

               --  Insert declaration before freeze node and body after

               Insert_Before_And_Analyze (N, FDecl);
               Insert_After_And_Analyze  (N, FBody);
            end;
         end if;

         if Is_Scalar_Type (Typ) then

            --  Attempt to build a static predicate for a discrete or a real
            --  subtype. This action may fail because the actual expression may
            --  not be static. Note that the presence of an inherited or
            --  explicitly declared dynamic predicate is orthogonal to this
            --  check because we are only interested in the static predicate.

            if Ekind_In (Typ, E_Decimal_Fixed_Point_Subtype,
                              E_Enumeration_Subtype,
                              E_Floating_Point_Subtype,
                              E_Modular_Integer_Subtype,
                              E_Ordinary_Fixed_Point_Subtype,
                              E_Signed_Integer_Subtype)
            then
               Build_Static_Predicate (Typ, Expr, Object_Name);

               --  Emit an error when the predicate is categorized as static
               --  but its expression is dynamic.

               if Present (Static_Predic)
                 and then No (Static_Predicate (Typ))
               then
                  Error_Msg_F
                    ("expression does not have required form for "
                     & "static predicate",
                     Next (First (Pragma_Argument_Associations
                                   (Static_Predic))));
               end if;
            end if;

         --  If a static predicate applies on other types, that's an error:
         --  either the type is scalar but non-static, or it's not even a
         --  scalar type. We do not issue an error on generated types, as
         --  these may be duplicates of the same error on a source type.

         elsif Present (Static_Predic) and then Comes_From_Source (Typ) then
            if Is_Scalar_Type (Typ) then
               Error_Msg_FE
                 ("static predicate not allowed for non-static type&",
                  Typ, Typ);
            else
               Error_Msg_FE
                 ("static predicate not allowed for non-scalar type&",
                  Typ, Typ);
            end if;
         end if;
      end if;
   end Build_Predicate_Functions;

   ----------------------------
   -- Build_Static_Predicate --
   ----------------------------

   procedure Build_Static_Predicate
     (Typ  : Entity_Id;
      Expr : Node_Id;
      Nam  : Name_Id)
   is
      Loc : constant Source_Ptr := Sloc (Expr);

      Non_Static : exception;
      --  Raised if something non-static is found

      Btyp : constant Entity_Id := Base_Type (Typ);

      BLo : constant Uint := Expr_Value (Type_Low_Bound  (Btyp));
      BHi : constant Uint := Expr_Value (Type_High_Bound (Btyp));
      --  Low bound and high bound value of base type of Typ

      TLo : constant Uint := Expr_Value (Type_Low_Bound  (Typ));
      THi : constant Uint := Expr_Value (Type_High_Bound (Typ));
      --  Low bound and high bound values of static subtype Typ

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
      --  for use as an entry in the Static_Predicate list. This node is typed
      --  with the base type.

      function Build_Range (Lo : Uint; Hi : Uint) return Node_Id;
      --  Return an analyzed N_Range node referencing this range, suitable for
      --  use as an entry in the Static_Predicate list. This node is typed with
      --  the base type.

      function Get_RList (Exp : Node_Id) return RList;
      --  This is a recursive routine that converts the given expression into a
      --  list of ranges, suitable for use in building the static predicate.

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
      --  the Nam given in the call).

      function Lo_Val (N : Node_Id) return Uint;
      --  Given static expression or static range from a Static_Predicate list,
      --  gets expression value or low bound of range.

      function Hi_Val (N : Node_Id) return Uint;
      --  Given static expression or static range from a Static_Predicate list,
      --  gets expression value of high bound of range.

      function Membership_Entry (N : Node_Id) return RList;
      --  Given a single membership entry (range, value, or subtype), returns
      --  the corresponding range list. Raises Static_Error if not static.

      function Membership_Entries (N : Node_Id) return RList;
      --  Given an element on an alternatives list of a membership operation,
      --  returns the range list corresponding to this entry and all following
      --  entries (i.e. returns the "or" of this list of values).

      function Stat_Pred (Typ : Entity_Id) return RList;
      --  Given a type, if it has a static predicate, then return the predicate
      --  as a range list, otherwise raise Non_Static.

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
               Result (Count) :=
                 REnt'(Right (J).Hi + 1, Right (J + 1).Lo - 1);
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

      function Get_RList (Exp : Node_Id) return RList is
         Op  : Node_Kind;
         Val : Uint;

      begin
         --  Static expression can only be true or false

         if Is_OK_Static_Expression (Exp) then

            --  For False

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

            when N_Op_And | N_And_Then =>
               return Get_RList (Left_Opnd (Exp))
                        and
                      Get_RList (Right_Opnd (Exp));

            --  Or

            when N_Op_Or | N_Or_Else =>
               return Get_RList (Left_Opnd (Exp))
                        or
                      Get_RList (Right_Opnd (Exp));

            --  Not

            when N_Op_Not =>
               return not Get_RList (Right_Opnd (Exp));

            --  Comparisons of type with static value

            when N_Op_Compare =>

               --  Type is left operand

               if Is_Type_Ref (Left_Opnd (Exp))
                 and then Is_OK_Static_Expression (Right_Opnd (Exp))
               then
                  Val := Expr_Value (Right_Opnd (Exp));

                  --  Typ is right operand

               elsif Is_Type_Ref (Right_Opnd (Exp))
                 and then Is_OK_Static_Expression (Left_Opnd (Exp))
               then
                  Val := Expr_Value (Left_Opnd (Exp));

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
                  raise Non_Static;
               end if;

               --  Construct range according to comparison operation

               case Op is
                  when N_Op_Eq =>
                     return RList'(1 => REnt'(Val, Val));

                  when N_Op_Ge =>
                     return RList'(1 => REnt'(Val, BHi));

                  when N_Op_Gt =>
                     return RList'(1 => REnt'(Val + 1, BHi));

                  when N_Op_Le =>
                     return RList'(1 => REnt'(BLo, Val));

                  when N_Op_Lt =>
                     return RList'(1 => REnt'(BLo, Val - 1));

                  when N_Op_Ne =>
                     return RList'(REnt'(BLo, Val - 1),
                                   REnt'(Val + 1, BHi));

                  when others  =>
                     raise Program_Error;
               end case;

            --  Membership (IN)

            when N_In =>
               if not Is_Type_Ref (Left_Opnd (Exp)) then
                  raise Non_Static;
               end if;

               if Present (Right_Opnd (Exp)) then
                  return Membership_Entry (Right_Opnd (Exp));
               else
                  return Membership_Entries (First (Alternatives (Exp)));
               end if;

            --  Negative membership (NOT IN)

            when N_Not_In =>
               if not Is_Type_Ref (Left_Opnd (Exp)) then
                  raise Non_Static;
               end if;

               if Present (Right_Opnd (Exp)) then
                  return not Membership_Entry (Right_Opnd (Exp));
               else
                  return not Membership_Entries (First (Alternatives (Exp)));
               end if;

            --  Function call, may be call to static predicate

            when N_Function_Call =>
               if Is_Entity_Name (Name (Exp)) then
                  declare
                     Ent : constant Entity_Id := Entity (Name (Exp));
                  begin
                     if Is_Predicate_Function (Ent)
                          or else
                        Is_Predicate_Function_M (Ent)
                     then
                        return Stat_Pred (Etype (First_Formal (Ent)));
                     end if;
                  end;
               end if;

               --  Other function call cases are non-static

               raise Non_Static;

            --  Qualified expression, dig out the expression

            when N_Qualified_Expression =>
               return Get_RList (Expression (Exp));

            --  Expression with actions: if no actions, dig out expression

            when N_Expression_With_Actions =>
               if Is_Empty_List (Actions (Exp)) then
                  return Get_RList (Expression (Exp));

               else
                  raise Non_Static;
               end if;

            --  Xor operator

            when N_Op_Xor =>
               return (Get_RList (Left_Opnd (Exp))
                        and not Get_RList (Right_Opnd (Exp)))
                 or   (Get_RList (Right_Opnd (Exp))
                        and not Get_RList (Left_Opnd (Exp)));

            --  Any other node type is non-static

            when others =>
               raise Non_Static;
         end case;
      end Get_RList;

      ------------
      -- Hi_Val --
      ------------

      function Hi_Val (N : Node_Id) return Uint is
      begin
         if Is_Static_Expression (N) then
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
         return Nkind (N) = N_Identifier and then Chars (N) = Nam;
      end Is_Type_Ref;

      ------------
      -- Lo_Val --
      ------------

      function Lo_Val (N : Node_Id) return Uint is
      begin
         if Is_Static_Expression (N) then
            return Expr_Value (N);
         else
            pragma Assert (Nkind (N) = N_Range);
            return Expr_Value (Low_Bound (N));
         end if;
      end Lo_Val;

      ------------------------
      -- Membership_Entries --
      ------------------------

      function Membership_Entries (N : Node_Id) return RList is
      begin
         if No (Next (N)) then
            return Membership_Entry (N);
         else
            return Membership_Entry (N) or Membership_Entries (Next (N));
         end if;
      end Membership_Entries;

      ----------------------
      -- Membership_Entry --
      ----------------------

      function Membership_Entry (N : Node_Id) return RList is
         Val : Uint;
         SLo : Uint;
         SHi : Uint;

      begin
         --  Range case

         if Nkind (N) = N_Range then
            if not Is_Static_Expression (Low_Bound (N))
                 or else
               not Is_Static_Expression (High_Bound (N))
            then
               raise Non_Static;
            else
               SLo := Expr_Value (Low_Bound  (N));
               SHi := Expr_Value (High_Bound (N));
               return RList'(1 => REnt'(SLo, SHi));
            end if;

         --  Static expression case

         elsif Is_Static_Expression (N) then
            Val := Expr_Value (N);
            return RList'(1 => REnt'(Val, Val));

         --  Identifier (other than static expression) case

         else pragma Assert (Nkind (N) = N_Identifier);

            --  Type case

            if Is_Type (Entity (N)) then

               --  If type has predicates, process them

               if Has_Predicates (Entity (N)) then
                  return Stat_Pred (Entity (N));

               --  For static subtype without predicates, get range

               elsif Is_Static_Subtype (Entity (N)) then
                  SLo := Expr_Value (Type_Low_Bound  (Entity (N)));
                  SHi := Expr_Value (Type_High_Bound (Entity (N)));
                  return RList'(1 => REnt'(SLo, SHi));

               --  Any other type makes us non-static

               else
                  raise Non_Static;
               end if;

            --  Any other kind of identifier in predicate (e.g. a non-static
            --  expression value) means this is not a static predicate.

            else
               raise Non_Static;
            end if;
         end if;
      end Membership_Entry;

      ---------------
      -- Stat_Pred --
      ---------------

      function Stat_Pred (Typ : Entity_Id) return RList is
      begin
         --  Not static if type does not have static predicates

         if not Has_Predicates (Typ) or else No (Static_Predicate (Typ)) then
            raise Non_Static;
         end if;

         --  Otherwise we convert the predicate list to a range list

         declare
            Result : RList (1 .. List_Length (Static_Predicate (Typ)));
            P      : Node_Id;

         begin
            P := First (Static_Predicate (Typ));
            for J in Result'Range loop
               Result (J) := REnt'(Lo_Val (P), Hi_Val (P));
               Next (P);
            end loop;

            return Result;
         end;
      end Stat_Pred;

   --  Start of processing for Build_Static_Predicate

   begin
      --  Now analyze the expression to see if it is a static predicate

      declare
         Ranges : constant RList := Get_RList (Expr);
         --  Range list from expression if it is static

         Plist : List_Id;

      begin
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

         Set_Static_Predicate (Typ, Plist);

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

               Install_Formals (Predicate_Function (Typ));
               Push_Scope (Predicate_Function (Typ));
               Analyze_And_Resolve (Expr, Standard_Boolean);
               Pop_Scope;
            end if;
         end;
      end;

   --  If non-static, return doing nothing

   exception
      when Non_Static =>
         return;
   end Build_Static_Predicate;

   -----------------------------------------
   -- Check_Aspect_At_End_Of_Declarations --
   -----------------------------------------

   procedure Check_Aspect_At_End_Of_Declarations (ASN : Node_Id) is
      Ent   : constant Entity_Id := Entity     (ASN);
      Ident : constant Node_Id   := Identifier (ASN);
      A_Id  : constant Aspect_Id := Get_Aspect_Id (Chars (Ident));

      End_Decl_Expr : constant Node_Id := Entity (Ident);
      --  Expression to be analyzed at end of declarations

      Freeze_Expr : constant Node_Id := Expression (ASN);
      --  Expression from call to Check_Aspect_At_Freeze_Point

      T : constant Entity_Id := Etype (Freeze_Expr);
      --  Type required for preanalyze call

      Err : Boolean;
      --  Set False if error

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
            Err := Entity (End_Decl_Expr) /= Entity (Freeze_Expr);

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
      --  Case of aspects Dimension, Dimension_System and Synchronization

      if A_Id = Aspect_Synchronization then
         return;

      --  Case of stream attributes, just have to compare entities. However,
      --  the expression is just a name (possibly overloaded), and there may
      --  be stream operations declared for unrelated types, so we just need
      --  to verify that one of these interpretations is the one available at
      --  at the freeze point.

      elsif A_Id = Aspect_Input  or else
         A_Id = Aspect_Output    or else
         A_Id = Aspect_Read      or else
         A_Id = Aspect_Write
      then
         Analyze (End_Decl_Expr);
         Check_Overloaded_Name;

      elsif A_Id = Aspect_Variable_Indexing or else
            A_Id = Aspect_Constant_Indexing or else
            A_Id = Aspect_Default_Iterator  or else
            A_Id = Aspect_Iterator_Element
      then
         --  Make type unfrozen before analysis, to prevent spurious errors
         --  about late attributes.

         Set_Is_Frozen (Ent, False);
         Analyze (End_Decl_Expr);
         Set_Is_Frozen (Ent, True);

         --  If the end of declarations comes before any other freeze
         --  point, the Freeze_Expr is not analyzed: no check needed.

         if Analyzed (Freeze_Expr) and then not In_Instance then
            Check_Overloaded_Name;
         else
            Err := False;
         end if;

      --  All other cases

      else
         --  In a generic context the aspect expressions have not been
         --  preanalyzed, so do it now. There are no conformance checks
         --  to perform in this case.

         if No (T) then
            Check_Aspect_At_Freeze_Point (ASN);
            return;

         --  The default values attributes may be defined in the private part,
         --  and the analysis of the expression may take place when only the
         --  partial view is visible. The expression must be scalar, so use
         --  the full view to resolve.

         elsif (A_Id = Aspect_Default_Value
                  or else
                A_Id = Aspect_Default_Component_Value)
            and then Is_Private_Type (T)
         then
            Preanalyze_Spec_Expression (End_Decl_Expr, Full_View (T));
         else
            Preanalyze_Spec_Expression (End_Decl_Expr, T);
         end if;

         Err := not Fully_Conformant_Expressions (End_Decl_Expr, Freeze_Expr);
      end if;

      --  Output error message if error

      if Err then
         Error_Msg_NE
           ("visibility of aspect for& changes after freeze point",
            ASN, Ent);
         Error_Msg_NE
           ("info: & is frozen here, aspects evaluated at this point??",
            Freeze_Node (Ent), Ent);
      end if;
   end Check_Aspect_At_End_Of_Declarations;

   ----------------------------------
   -- Check_Aspect_At_Freeze_Point --
   ----------------------------------

   procedure Check_Aspect_At_Freeze_Point (ASN : Node_Id) is
      Ident : constant Node_Id := Identifier (ASN);
      --  Identifier (use Entity field to save expression)

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

      Set_Expression (ASN, New_Copy_Tree (Entity (Ident)));

      --  Find type for preanalyze call

      case A_Id is

         --  No_Aspect should be impossible

         when No_Aspect =>
            raise Program_Error;

         --  Aspects taking an optional boolean argument

         when Boolean_Aspects      |
              Library_Unit_Aspects =>

            T := Standard_Boolean;

         --  Aspects corresponding to attribute definition clauses

         when Aspect_Address =>
            T := RTE (RE_Address);

         when Aspect_Attach_Handler =>
            T := RTE (RE_Interrupt_ID);

         when Aspect_Bit_Order | Aspect_Scalar_Storage_Order =>
            T := RTE (RE_Bit_Order);

         when Aspect_Convention =>
            return;

         when Aspect_CPU =>
            T := RTE (RE_CPU_Range);

         --  Default_Component_Value is resolved with the component type

         when Aspect_Default_Component_Value =>
            T := Component_Type (Entity (ASN));

         --  Default_Value is resolved with the type entity in question

         when Aspect_Default_Value =>
            T := Entity (ASN);

         --  Depends is a delayed aspect because it mentiones names first
         --  introduced by aspect Global which is already delayed. There is
         --  no action to be taken with respect to the aspect itself as the
         --  analysis is done by the corresponding pragma.

         when Aspect_Depends =>
            return;

         when Aspect_Dispatching_Domain =>
            T := RTE (RE_Dispatching_Domain);

         when Aspect_External_Tag =>
            T := Standard_String;

         when Aspect_External_Name =>
            T := Standard_String;

         --  Global is a delayed aspect because it may reference names that
         --  have not been declared yet. There is no action to be taken with
         --  respect to the aspect itself as the reference checking is done
         --  on the corresponding pragma.

         when Aspect_Global =>
            return;

         when Aspect_Link_Name =>
            T := Standard_String;

         when Aspect_Priority | Aspect_Interrupt_Priority =>
            T := Standard_Integer;

         when Aspect_Relative_Deadline =>
            T := RTE (RE_Time_Span);

         when Aspect_Small =>
            T := Universal_Real;

         --  For a simple storage pool, we have to retrieve the type of the
         --  pool object associated with the aspect's corresponding attribute
         --  definition clause.

         when Aspect_Simple_Storage_Pool =>
            T := Etype (Expression (Aspect_Rep_Item (ASN)));

         when Aspect_Storage_Pool =>
            T := Class_Wide_Type (RTE (RE_Root_Storage_Pool));

         when Aspect_Alignment      |
              Aspect_Component_Size |
              Aspect_Machine_Radix  |
              Aspect_Object_Size    |
              Aspect_Size           |
              Aspect_Storage_Size   |
              Aspect_Stream_Size    |
              Aspect_Value_Size     =>
            T := Any_Integer;

         when Aspect_Linker_Section =>
            T := Standard_String;

         when Aspect_Synchronization =>
            return;

         --  Special case, the expression of these aspects is just an entity
         --  that does not need any resolution, so just analyze.

         when Aspect_Input      |
              Aspect_Output     |
              Aspect_Read       |
              Aspect_Suppress   |
              Aspect_Unsuppress |
              Aspect_Warnings   |
              Aspect_Write      =>
            Analyze (Expression (ASN));
            return;

         --  Same for Iterator aspects, where the expression is a function
         --  name. Legality rules are checked separately.

         when Aspect_Constant_Indexing |
              Aspect_Default_Iterator  |
              Aspect_Iterator_Element  |
              Aspect_Variable_Indexing =>
            Analyze (Expression (ASN));
            return;

         --  Invariant/Predicate take boolean expressions

         when Aspect_Dynamic_Predicate |
              Aspect_Invariant         |
              Aspect_Predicate         |
              Aspect_Static_Predicate  |
              Aspect_Type_Invariant    =>
            T := Standard_Boolean;

         --  Here is the list of aspects that don't require delay analysis

         when Aspect_Abstract_State       |
              Aspect_Contract_Cases       |
              Aspect_Dimension            |
              Aspect_Dimension_System     |
              Aspect_Implicit_Dereference |
              Aspect_Initial_Condition    |
              Aspect_Initializes          |
              Aspect_Post                 |
              Aspect_Postcondition        |
              Aspect_Pre                  |
              Aspect_Precondition         |
              Aspect_Refined_Depends      |
              Aspect_Refined_Global       |
              Aspect_Refined_Post         |
              Aspect_Refined_State        |
              Aspect_SPARK_Mode           |
              Aspect_Test_Case            =>
            raise Program_Error;

      end case;

      --  Do the preanalyze call

      Preanalyze_Spec_Expression (Expression (ASN), T);
   end Check_Aspect_At_Freeze_Point;

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
                 ("address for& cannot" &
                    " depend on another address clause! (RM 13.1(22))!",
                  Nod, U_Ent);

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
                    ("\address cannot depend on component" &
                     " of discriminated record (RM 13.1(22))!",
                     Nod);
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
            when N_Empty | N_Error =>
               return;

            when N_Identifier | N_Expanded_Name =>
               Ent := Entity (Nod);

               --  We need to look at the original node if it is different
               --  from the node, since we may have rewritten things and
               --  substituted an identifier representing the rewrite.

               if Original_Node (Nod) /= Nod then
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

               if Ekind_In (Ent, E_Named_Integer, E_Named_Real)
                 or else Is_Type (Ent)
               then
                  return;

               elsif
                  Ekind (Ent) = E_Constant
                    or else
                  Ekind (Ent) = E_In_Parameter
               then
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

            when N_Real_Literal      |
                 N_String_Literal    |
                 N_Character_Literal =>
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
               if Nam_In (Attribute_Name (Nod), Name_Address,
                                                Name_Access,
                                                Name_Unchecked_Access,
                                                Name_Unrestricted_Access)
               then
                  Check_At_Constant_Address (Prefix (Nod));

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

            when N_Binary_Op | N_Short_Circuit | N_Membership_Test =>
               Check_Expr_Constants (Left_Opnd (Nod));
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Unary_Op =>
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Type_Conversion           |
                 N_Qualified_Expression      |
                 N_Allocator                 |
                 N_Unchecked_Type_Conversion =>
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
         if Present (Lst) then
            Nod1 := First (Lst);
            while Present (Nod1) loop
               Check_Expr_Constants (Nod1);
               Next (Nod1);
            end loop;
         end if;
      end Check_List_Constants;

   --  Start of processing for Check_Constant_Address_Clause

   begin
      --  If rep_clauses are to be ignored, no need for legality checks. In
      --  particular, no need to pester user about rep clauses that violate
      --  the rule on constant addresses, given that these clauses will be
      --  removed by Freeze before they reach the back end.

      if not Ignore_Rep_Clauses then
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
      Fbit    : Uint;
      Lbit    : Uint;
      Hbit    : Uint := Uint_0;
      Comp    : Entity_Id;
      Pcomp   : Entity_Id;

      Max_Bit_So_Far : Uint;
      --  Records the maximum bit position so far. If all field positions
      --  are monotonically increasing, then we can skip the circuit for
      --  checking for overlap, since no overlap is possible.

      Tagged_Parent : Entity_Id := Empty;
      --  This is set in the case of a derived tagged type for which we have
      --  Is_Fully_Repped_Tagged_Type True (indicating that all components are
      --  positioned by record representation clauses). In this case we must
      --  check for overlap between components of this tagged type, and the
      --  components of its parent. Tagged_Parent will point to this parent
      --  type. For all other cases Tagged_Parent is left set to Empty.

      Parent_Last_Bit : Uint;
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

            if Nam_In (Chars (C1_Ent), Name_uTag, Name_uTag) then
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

   --  Start of processing for Check_Record_Representation_Clause

   begin
      Find_Type (Ident);
      Rectype := Entity (Ident);

      if Rectype = Any_Type then
         return;
      else
         Rectype := Underlying_Type (Rectype);
      end if;

      --  See if we have a fully repped derived tagged type

      declare
         PS : constant Entity_Id := Parent_Subtype (Rectype);

      begin
         if Present (PS) and then Is_Fully_Repped_Tagged_Type (PS) then
            Tagged_Parent := PS;

            --  Find maximum bit of any component of the parent type

            Parent_Last_Bit := UI_From_Int (System_Address_Size - 1);
            Pcomp := First_Entity (Tagged_Parent);
            while Present (Pcomp) loop
               if Ekind_In (Pcomp, E_Discriminant, E_Component) then
                  if Component_Bit_Offset (Pcomp) /= No_Uint
                    and then Known_Static_Esize (Pcomp)
                  then
                     Parent_Last_Bit :=
                       UI_Max
                         (Parent_Last_Bit,
                          Component_Bit_Offset (Pcomp) + Esize (Pcomp) - 1);
                  end if;

                  Next_Entity (Pcomp);
               end if;
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
         Set_Normalized_Position_Max (Fent, Uint_0);
         Init_Esize                  (Fent, System_Address_Size);

         Set_Component_Clause (Fent,
           Make_Component_Clause (Loc,
             Component_Name => Make_Identifier (Loc, Name_uTag),

             Position  => Make_Integer_Literal (Loc, Uint_0),
             First_Bit => Make_Integer_Literal (Loc, Uint_0),
             Last_Bit  =>
               Make_Integer_Literal (Loc,
                 UI_From_Int (System_Address_Size))));

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
               Error_Msg_N
                 ("bit number out of range of specified size",
                  Last_Bit (CC));

               --  Check for overlap with tag component

            else
               if Is_Tagged_Type (Rectype)
                 and then Fbit < System_Address_Size
               then
                  Error_Msg_NE
                    ("component overlaps tag field of&",
                     Component_Name (CC), Rectype);
                  Overlap_Detected := True;
               end if;

               if Hbit < Lbit then
                  Hbit := Lbit;
               end if;
            end if;

            --  Check parent overlap if component might overlap parent field

            if Present (Tagged_Parent) and then Fbit <= Parent_Last_Bit then
               Pcomp := First_Component_Or_Discriminant (Tagged_Parent);
               while Present (Pcomp) loop
                  if not Is_Tag (Pcomp)
                    and then Chars (Pcomp) /= Name_uParent
                  then
                     Check_Component_Overlap (Comp, Pcomp);
                  end if;

                  Next_Component_Or_Discriminant (Pcomp);
               end loop;
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
               if not Ekind_In (C1_Ent, E_Component, E_Discriminant) then
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

                  if Nkind_In (Clist, N_Full_Type_Declaration,
                                      N_Private_Type_Declaration)
                  then
                     if Has_Discriminants (Defining_Identifier (Clist)) then
                        C2_Ent :=
                          First_Discriminant (Defining_Identifier (Clist));
                        while Present (C2_Ent) loop
                           exit when C1_Ent = C2_Ent;
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

      --  The following circuit deals with warning on record holes (gaps). We
      --  skip this check if overlap was detected, since it makes sense for the
      --  programmer to fix this illegality before worrying about warnings.

      if not Overlap_Detected and Warn_On_Record_Holes then
         Record_Hole_Check : declare
            Decl : constant Node_Id := Declaration_Node (Base_Type (Rectype));
            --  Full declaration of record type

            procedure Check_Component_List
              (CL   : Node_Id;
               Sbit : Uint;
               DS   : List_Id);
            --  Check component list CL for holes. The starting bit should be
            --  Sbit. which is zero for the main record component list and set
            --  appropriately for recursive calls for variants. DS is set to
            --  a list of discriminant specifications to be included in the
            --  consideration of components. It is No_List if none to consider.

            --------------------------
            -- Check_Component_List --
            --------------------------

            procedure Check_Component_List
              (CL   : Node_Id;
               Sbit : Uint;
               DS   : List_Id)
            is
               Compl : Integer;

            begin
               Compl := Integer (List_Length (Component_Items (CL)));

               if DS /= No_List then
                  Compl := Compl + Integer (List_Length (DS));
               end if;

               declare
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
                  begin
                     return Component_Bit_Offset (Comps (Op1))
                       <
                       Component_Bit_Offset (Comps (Op2));
                  end Lt;

                  ----------
                  -- Move --
                  ----------

                  procedure Move (From : Natural; To : Natural) is
                  begin
                     Comps (To) := Comps (From);
                  end Move;

               begin
                  --  Gather discriminants into Comp

                  if DS /= No_List then
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
                  end if;

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
                     Error_Msg_Uint_1 := Component_Bit_Offset (CEnt) - Nbit;

                     if Error_Msg_Uint_1 > 0 then
                        Error_Msg_NE
                          ("?H?^-bit gap before component&",
                           Component_Name (Component_Clause (CEnt)), CEnt);
                     end if;

                     Nbit := Component_Bit_Offset (CEnt) + Esize (CEnt);
                  end loop;

                  --  Process variant parts recursively if present

                  if Present (Variant_Part (CL)) then
                     Variant := First (Variants (Variant_Part (CL)));
                     while Present (Variant) loop
                        Check_Component_List
                          (Component_List (Variant), Nbit, No_List);
                        Next (Variant);
                     end loop;
                  end if;
               end;
            end Check_Component_List;

         --  Start of processing for Record_Hole_Check

         begin
            declare
               Sbit : Uint;

            begin
               if Is_Tagged_Type (Rectype) then
                  Sbit := UI_From_Int (System_Address_Size);
               else
                  Sbit := Uint_0;
               end if;

               if Nkind (Decl) = N_Full_Type_Declaration
                 and then Nkind (Type_Definition (Decl)) = N_Record_Definition
               then
                  Check_Component_List
                    (Component_List (Type_Definition (Decl)),
                     Sbit,
                     Discriminant_Specifications (Decl));
               end if;
            end;
         end Record_Hole_Check;
      end if;

      --  For records that have component clauses for all components, and whose
      --  size is less than or equal to 32, we need to know the size in the
      --  front end to activate possible packed array processing where the
      --  component type is a record.

      --  At this stage Hbit + 1 represents the first unused bit from all the
      --  component clauses processed, so if the component clauses are
      --  complete, then this is the length of the record.

      --  For records longer than System.Storage_Unit, and for those where not
      --  all components have component clauses, the back end determines the
      --  length (it may for example be appropriate to round up the size
      --  to some convenient boundary, based on alignment considerations, etc).

      if Unknown_RM_Size (Rectype) and then Hbit + 1 <= 32 then

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
      UT : constant Entity_Id := Underlying_Type (T);
      M  : Uint;

   begin
      Biased := False;

      --  Reject patently improper size values.

      if Is_Elementary_Type (T)
        and then Siz > UI_From_Int (Int'Last)
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

               if not Is_Static_Subtype (Ityp) then
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
               Error_Msg_Uint_1 := Asiz;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^", N, T);
               Set_Esize   (T, Asiz);
               Set_RM_Size (T, Asiz);
            end if;
         end;

      --  All other composite types are ignored

      elsif Is_Composite_Type (UT) then
         return;

      --  For fixed-point types, don't check minimum if type is not frozen,
      --  since we don't know all the characteristics of the type that can
      --  affect the size (e.g. a specified small) till freeze time.

      elsif Is_Fixed_Point_Type (UT)
        and then not Is_Frozen (UT)
      then
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
               Error_Msg_Uint_1 := M;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^", N, T);
               Set_Esize (T, M);
               Set_RM_Size (T, M);
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
      E : constant Entity_Id := Entity (N);

      Non_Generic_Case : constant Boolean := Nkind (N) = N_Freeze_Entity;
      --  True in non-generic case. Some of the processing here is skipped
      --  for the generic case since it is not needed. Basically in the
      --  generic case, we only need to do stuff that might generate error
      --  messages or warnings.
   begin
      --  Remember that we are processing a freezing entity. Required to
      --  ensure correct decoration of internal entities associated with
      --  interfaces (see New_Overloaded_Entity).

      Inside_Freezing_Actions := Inside_Freezing_Actions + 1;

      --  For tagged types covering interfaces add internal entities that link
      --  the primitives of the interfaces with the primitives that cover them.
      --  Note: These entities were originally generated only when generating
      --  code because their main purpose was to provide support to initialize
      --  the secondary dispatch tables. They are now generated also when
      --  compiling with no code generation to provide ASIS the relationship
      --  between interface primitives and tagged type primitives. They are
      --  also used to locate primitives covering interfaces when processing
      --  generics (see Derive_Subprograms).

      --  This is not needed in the generic case

      if Ada_Version >= Ada_2005
        and then Non_Generic_Case
        and then Ekind (E) = E_Record_Type
        and then Is_Tagged_Type (E)
        and then not Is_Interface (E)
        and then Has_Interfaces (E)
      then
         --  This would be a good common place to call the routine that checks
         --  overriding of interface primitives (and thus factorize calls to
         --  Check_Abstract_Overriding located at different contexts in the
         --  compiler). However, this is not possible because it causes
         --  spurious errors in case of late overriding.

         Add_Internal_Interface_Entities (E);
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

      if Expander_Active    -- why? losing errors in -gnatc mode???
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

      --  If we have a type with predicates, build predicate function. This
      --  is not needed in the generic casee

      if Non_Generic_Case and then Is_Type (E) and then Has_Predicates (E) then
         Build_Predicate_Functions (E, N);
      end if;

      --  If type has delayed aspects, this is where we do the preanalysis at
      --  the freeze point, as part of the consistent visibility check. Note
      --  that this must be done after calling Build_Predicate_Functions or
      --  Build_Invariant_Procedure since these subprograms fix occurrences of
      --  the subtype name in the saved expression so that they will not cause
      --  trouble in the preanalysis.

      --  This is also not needed in the generic case

      if Non_Generic_Case
        and then Has_Delayed_Aspects (E)
        and then Scope (E) = Current_Scope
      then
         --  Retrieve the visibility to the discriminants in order to properly
         --  analyze the aspects.

         Push_Scope_And_Install_Discriminants (E);

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
                  Check_Aspect_At_Freeze_Point (Ritem);
               end if;

               Next_Rep_Item (Ritem);
            end loop;
         end;

         Uninstall_Discriminants_And_Pop_Scope (E);
      end if;

      --  For a record type, deal with variant parts. This has to be delayed
      --  to this point, because of the issue of statically precicated
      --  subtypes, which we have to ensure are frozen before checking
      --  choices, since we need to have the static choice list set.

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
                  --  the list since it would invalidate the ASIS -gnatct tree.
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
               --  to the others choice (it's the list we're replacing!)

               --  We only want to do this if the expander is active, since
               --  we do not want to clobber the ASIS tree!

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
   end Freeze_Entity_Checks;

   -------------------------
   -- Get_Alignment_Value --
   -------------------------

   function Get_Alignment_Value (Expr : Node_Id) return Uint is
      Align : constant Uint := Static_Integer (Expr);

   begin
      if Align = No_Uint then
         return No_Uint;

      elsif Align <= 0 then
         Error_Msg_N ("alignment value must be positive", Expr);
         return No_Uint;

      else
         for J in Int range 0 .. 64 loop
            declare
               M : constant Uint := Uint_2 ** J;

            begin
               exit when M = Align;

               if M > Align then
                  Error_Msg_N
                    ("alignment value must be power of 2", Expr);
                  return No_Uint;
               end if;
            end;
         end loop;

         return Align;
      end if;
   end Get_Alignment_Value;

   -------------------------------------
   -- Inherit_Aspects_At_Freeze_Point --
   -------------------------------------

   procedure Inherit_Aspects_At_Freeze_Point (Typ : Entity_Id) is
      function Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
        (Rep_Item : Node_Id) return Boolean;
      --  This routine checks if Rep_Item is either a pragma or an aspect
      --  specification node whose correponding pragma (if any) is present in
      --  the Rep Item chain of the entity it has been specified to.

      --------------------------------------------------
      -- Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item --
      --------------------------------------------------

      function Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
        (Rep_Item : Node_Id) return Boolean
      is
      begin
         return Nkind (Rep_Item) = N_Pragma
           or else Present_In_Rep_Item
                     (Entity (Rep_Item), Aspect_Rep_Item (Rep_Item));
      end Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item;

   --  Start of processing for Inherit_Aspects_At_Freeze_Point

   begin
      --  A representation item is either subtype-specific (Size and Alignment
      --  clauses) or type-related (all others).  Subtype-specific aspects may
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

      --  ??? Note that, for now, just a limited number of representation
      --  aspects have been inherited here so far. Many of them are
      --  still inherited in Sem_Ch3. This will be fixed soon. Here is
      --  a non- exhaustive list of aspects that likely also need to
      --  be moved to this routine: Alignment, Component_Alignment,
      --  Component_Size, Machine_Radix, Object_Size, Pack, Predicates,
      --  Preelaborable_Initialization, RM_Size and Small.

      if Nkind (Parent (Typ)) = N_Private_Extension_Declaration then
         return;
      end if;

      --  Ada_05/Ada_2005

      if not Has_Rep_Item (Typ, Name_Ada_05, Name_Ada_2005, False)
        and then Has_Rep_Item (Typ, Name_Ada_05, Name_Ada_2005)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Ada_05, Name_Ada_2005))
      then
         Set_Is_Ada_2005_Only (Typ);
      end if;

      --  Ada_12/Ada_2012

      if not Has_Rep_Item (Typ, Name_Ada_12, Name_Ada_2012, False)
        and then Has_Rep_Item (Typ, Name_Ada_12, Name_Ada_2012)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Ada_12, Name_Ada_2012))
      then
         Set_Is_Ada_2012_Only (Typ);
      end if;

      --  Atomic/Shared

      if not Has_Rep_Item (Typ, Name_Atomic, Name_Shared, False)
        and then Has_Rep_Pragma (Typ, Name_Atomic, Name_Shared)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Atomic, Name_Shared))
      then
         Set_Is_Atomic (Typ);
         Set_Treat_As_Volatile (Typ);
         Set_Is_Volatile (Typ);
      end if;

      --  Default_Component_Value

      if Is_Array_Type (Typ)
        and then Is_Base_Type (Typ)
        and then Has_Rep_Item (Typ, Name_Default_Component_Value, False)
        and then Has_Rep_Item (Typ, Name_Default_Component_Value)
      then
         Set_Default_Aspect_Component_Value (Typ,
           Default_Aspect_Component_Value
             (Entity (Get_Rep_Item (Typ, Name_Default_Component_Value))));
      end if;

      --  Default_Value

      if Is_Scalar_Type (Typ)
        and then Is_Base_Type (Typ)
        and then Has_Rep_Item (Typ, Name_Default_Value, False)
        and then Has_Rep_Item (Typ, Name_Default_Value)
      then
         Set_Default_Aspect_Value (Typ,
           Default_Aspect_Value
             (Entity (Get_Rep_Item (Typ, Name_Default_Value))));
      end if;

      --  Discard_Names

      if not Has_Rep_Item (Typ, Name_Discard_Names, False)
        and then Has_Rep_Item (Typ, Name_Discard_Names)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Discard_Names))
      then
         Set_Discard_Names (Typ);
      end if;

      --  Invariants

      if not Has_Rep_Item (Typ, Name_Invariant, False)
        and then Has_Rep_Item (Typ, Name_Invariant)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Invariant))
      then
         Set_Has_Invariants (Typ);

         if Class_Present (Get_Rep_Item (Typ, Name_Invariant)) then
            Set_Has_Inheritable_Invariants (Typ);
         end if;
      end if;

      --  Volatile

      if not Has_Rep_Item (Typ, Name_Volatile, False)
        and then Has_Rep_Item (Typ, Name_Volatile)
        and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Volatile))
      then
         Set_Treat_As_Volatile (Typ);
         Set_Is_Volatile (Typ);
      end if;

      --  Inheritance for derived types only

      if Is_Derived_Type (Typ) then
         declare
            Bas_Typ     : constant Entity_Id := Base_Type (Typ);
            Imp_Bas_Typ : constant Entity_Id := Implementation_Base_Type (Typ);

         begin
            --  Atomic_Components

            if not Has_Rep_Item (Typ, Name_Atomic_Components, False)
              and then Has_Rep_Item (Typ, Name_Atomic_Components)
              and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Atomic_Components))
            then
               Set_Has_Atomic_Components (Imp_Bas_Typ);
            end if;

            --  Volatile_Components

            if not Has_Rep_Item (Typ, Name_Volatile_Components, False)
              and then Has_Rep_Item (Typ, Name_Volatile_Components)
              and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Volatile_Components))
            then
               Set_Has_Volatile_Components (Imp_Bas_Typ);
            end if;

            --  Finalize_Storage_Only.

            if not Has_Rep_Pragma (Typ, Name_Finalize_Storage_Only, False)
              and then Has_Rep_Pragma (Typ, Name_Finalize_Storage_Only)
            then
               Set_Finalize_Storage_Only (Bas_Typ);
            end if;

            --  Universal_Aliasing

            if not Has_Rep_Item (Typ, Name_Universal_Aliasing, False)
              and then Has_Rep_Item (Typ, Name_Universal_Aliasing)
              and then Is_Pragma_Or_Corr_Pragma_Present_In_Rep_Item
                   (Get_Rep_Item (Typ, Name_Universal_Aliasing))
            then
               Set_Universal_Aliasing (Imp_Bas_Typ);
            end if;

            --  Record type specific aspects

            if Is_Record_Type (Typ) then

               --  Bit_Order

               if not Has_Rep_Item (Typ, Name_Bit_Order, False)
                 and then Has_Rep_Item (Typ, Name_Bit_Order)
               then
                  Set_Reverse_Bit_Order (Bas_Typ,
                    Reverse_Bit_Order (Entity (Name
                      (Get_Rep_Item (Typ, Name_Bit_Order)))));
               end if;

               --  Scalar_Storage_Order

               if not Has_Rep_Item (Typ, Name_Scalar_Storage_Order, False)
                 and then Has_Rep_Item (Typ, Name_Scalar_Storage_Order)
               then
                  Set_Reverse_Storage_Order (Bas_Typ,
                    Reverse_Storage_Order (Entity (Name
                      (Get_Rep_Item (Typ, Name_Scalar_Storage_Order)))));
               end if;
            end if;
         end;
      end if;
   end Inherit_Aspects_At_Freeze_Point;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Address_Clause_Checks.Init;
      Independence_Checks.Init;
      Unchecked_Conversions.Init;
   end Initialize;

   -------------------------
   -- Is_Operational_Item --
   -------------------------

   function Is_Operational_Item (N : Node_Id) return Boolean is
   begin
      if Nkind (N) /= N_Attribute_Definition_Clause then
         return False;

      else
         declare
            Id : constant Attribute_Id := Get_Attribute_Id (Chars (N));
         begin
            return    Id = Attribute_Input
              or else Id = Attribute_Output
              or else Id = Attribute_Read
              or else Id = Attribute_Write
              or else Id = Attribute_External_Tag;
         end;
      end if;
   end Is_Operational_Item;

   ------------------
   -- Minimum_Size --
   ------------------

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False) return Nat
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
      --  If bad type, return 0

      if T = Any_Type then
         return 0;

      --  For generic types, just return zero. There cannot be any legitimate
      --  need to know such a size, but this routine may be called with a
      --  generic type as part of normal processing.

      elsif Is_Generic_Type (R_Typ)
        or else R_Typ = Any_Type
      then
         return 0;

         --  Access types. Normally an access type cannot have a size smaller
         --  than the size of System.Address. The exception is on VMS, where
         --  we have short and long addresses, and it is possible for an access
         --  type to have a short address size (and thus be less than the size
         --  of System.Address itself). We simply skip the check for VMS, and
         --  leave it to the back end to do the check.

      elsif Is_Access_Type (T) then
         if OpenVMS_On_Target then
            return 0;
         else
            return System_Address_Size;
         end if;

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
               return 0;
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
                  return 0;
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
               return 0;
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
                  return 0;
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

      --  Signed case. Note that we consider types like range 1 .. -1 to be
      --  signed for the purpose of computing the size, since the bounds have
      --  to be accommodated in the base type.

      if Lo < 0 or else Hi < 0 then
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
      --  the attribute definition clause.

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
         T_Ref   : constant Node_Id := New_Reference_To (Etyp, Loc);

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
                              New_Reference_To (
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

      Insert_Action (N, Subp_Decl);
      Set_Entity (N, Subp_Id);

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => Build_Spec,
          Name => New_Reference_To (Subp, Loc));

      if Defer_Declaration then
         Set_TSS (Base_Type (Ent), Subp_Id);
      else
         Insert_Action (N, Subp_Decl);
         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;
   end New_Stream_Subprogram;

   ------------------------
   -- Rep_Item_Too_Early --
   ------------------------

   function Rep_Item_Too_Early (T : Entity_Id; N : Node_Id) return Boolean is
   begin
      --  Cannot apply non-operational rep items to generic types

      if Is_Operational_Item (N) then
         return False;

      elsif Is_Type (T)
        and then Is_Generic_Type (Root_Type (T))
      then
         Error_Msg_N ("representation item not allowed for generic type", N);
         return True;
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
      S           : Entity_Id;
      Parent_Type : Entity_Id;

      procedure Too_Late;
      --  Output the too late message. Note that this is not considered a
      --  serious error, since the effect is simply that we ignore the
      --  representation clause in this case.

      --------------
      -- Too_Late --
      --------------

      procedure Too_Late is
      begin
         --  Other compilers seem more relaxed about rep items appearing too
         --  late. Since analysis tools typically don't care about rep items
         --  anyway, no reason to be too strict about this.

         if not Relaxed_RM_Semantics then
            Error_Msg_N ("|representation item appears too late!", N);
         end if;
      end Too_Late;

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
         Too_Late;
         S := First_Subtype (T);

         if Present (Freeze_Node (S)) then
            Error_Msg_NE
              ("??no more representation items for }", Freeze_Node (S), S);
         end if;

         return True;

      --  Check for case of non-tagged derived type whose parent either has
      --  primitive operations, or is a by reference type (RM 13.1(10)).

      elsif Is_Type (T)
        and then not FOnly
        and then Is_Derived_Type (T)
        and then not Is_Tagged_Type (T)
      then
         Parent_Type := Etype (Base_Type (T));

         if Has_Primitive_Operations (Parent_Type) then
            Too_Late;
            Error_Msg_NE
              ("primitive operations already defined for&!", N, Parent_Type);
            return True;

         elsif Is_By_Reference_Type (Parent_Type) then
            Too_Late;
            Error_Msg_NE
              ("parent type & is a by reference type!", N, Parent_Type);
            return True;
         end if;
      end if;

      --  No error, link item into head of chain of rep items for the entity,
      --  but avoid chaining if we have an overloadable entity, and the pragma
      --  is one that can apply to multiple overloaded entities.

      if Is_Overloadable (T) and then Nkind (N) = N_Pragma then
         declare
            Pname : constant Name_Id := Pragma_Name (N);
         begin
            if Nam_In (Pname, Name_Convention, Name_Import,   Name_Export,
                              Name_External,   Name_Interface)
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

   procedure Replace_Type_References_Generic (N : Node_Id; TName : Name_Id) is

      function Replace_Node (N : Node_Id) return Traverse_Result;
      --  Processes a single node in the traversal procedure below, checking
      --  if node N should be replaced, and if so, doing the replacement.

      procedure Replace_Type_Refs is new Traverse_Proc (Replace_Node);
      --  This instantiation provides the body of Replace_Type_References

      ------------------
      -- Replace_Node --
      ------------------

      function Replace_Node (N : Node_Id) return Traverse_Result is
         S : Entity_Id;
         P : Node_Id;

      begin
         --  Case of identifier

         if Nkind (N) = N_Identifier then

            --  If not the type name, all done with this node

            if Chars (N) /= TName then
               return Skip;

            --  Otherwise do the replacement and we are done with this node

            else
               Replace_Type_Reference (N);
               return Skip;
            end if;

         --  Case of selected component (which is what a qualification
         --  looks like in the unanalyzed tree, which is what we have.

         elsif Nkind (N) = N_Selected_Component then

            --  If selector name is not our type, keeping going (we might
            --  still have an occurrence of the type in the prefix).

            if Nkind (Selector_Name (N)) /= N_Identifier
              or else Chars (Selector_Name (N)) /= TName
            then
               return OK;

            --  Selector name is our type, check qualification

            else
               --  Loop through scopes and prefixes, doing comparison

               S := Current_Scope;
               P := Prefix (N);
               loop
                  --  Continue if no more scopes or scope with no name

                  if No (S) or else Nkind (S) not in N_Has_Chars then
                     return OK;
                  end if;

                  --  Do replace if prefix is an identifier matching the
                  --  scope that we are currently looking at.

                  if Nkind (P) = N_Identifier
                    and then Chars (P) = Chars (S)
                  then
                     Replace_Type_Reference (N);
                     return Skip;
                  end if;

                  --  Go check scope above us if prefix is itself of the
                  --  form of a selected component, whose selector matches
                  --  the scope we are currently looking at.

                  if Nkind (P) = N_Selected_Component
                    and then Nkind (Selector_Name (P)) = N_Identifier
                    and then Chars (Selector_Name (P)) = Chars (S)
                  then
                     S := Scope (S);
                     P := Prefix (P);

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
      end Replace_Node;

   begin
      Replace_Type_Refs (N);
   end Replace_Type_References_Generic;

   -------------------------
   -- Same_Representation --
   -------------------------

   function Same_Representation (Typ1, Typ2 : Entity_Id) return Boolean is
      T1 : constant Entity_Id := Underlying_Type (Typ1);
      T2 : constant Entity_Id := Underlying_Type (Typ2);

   begin
      --  A quick check, if base types are the same, then we definitely have
      --  the same representation, because the subtype specific representation
      --  attributes (Size and Alignment) do not affect representation from
      --  the point of view of this test.

      if Base_Type (T1) = Base_Type (T2) then
         return True;

      elsif Is_Private_Type (Base_Type (T2))
        and then Base_Type (T1) = Full_View (Base_Type (T2))
      then
         return True;
      end if;

      --  Tagged types never have differing representations

      if Is_Tagged_Type (T1) then
         return True;
      end if;

      --  Representations are definitely different if conventions differ

      if Convention (T1) /= Convention (T2) then
         return False;
      end if;

      --  Representations are different if component alignments or scalar
      --  storage orders differ.

      if (Is_Record_Type (T1) or else Is_Array_Type (T1))
            and then
         (Is_Record_Type (T2) or else Is_Array_Type (T2))
        and then
         (Component_Alignment (T1) /= Component_Alignment (T2)
            or else
              Reverse_Storage_Order (T1) /= Reverse_Storage_Order (T2))
      then
         return False;
      end if;

      --  For arrays, the only real issue is component size. If we know the
      --  component size for both arrays, and it is the same, then that's
      --  good enough to know we don't have a change of representation.

      if Is_Array_Type (T1) then
         if Known_Component_Size (T1)
           and then Known_Component_Size (T2)
           and then Component_Size (T1) = Component_Size (T2)
         then
            if VM_Target = No_VM then
               return True;

            --  In VM targets the representation of arrays with aliased
            --  components differs from arrays with non-aliased components

            else
               return Has_Aliased_Components (Base_Type (T1))
                        =
                      Has_Aliased_Components (Base_Type (T2));
            end if;
         end if;
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

      --  Tagged types always have the same representation, because it is not
      --  possible to specify different representations for common fields.

      elsif Is_Tagged_Type (T1) then
         return True;

      --  Case of record types

      elsif Is_Record_Type (T1) then

         --  Packed status must conform

         if Is_Packed (T1) /= Is_Packed (T2) then
            return False;

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
   end Same_Representation;

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
              ("?B?" & Msg & " forces biased representation for&", N, E);
         end if;
      end if;
   end Set_Biased;

   --------------------
   -- Set_Enum_Esize --
   --------------------

   procedure Set_Enum_Esize (T : Entity_Id) is
      Lo : Uint;
      Hi : Uint;
      Sz : Nat;

   begin
      Init_Alignment (T);

      --  Find the minimum standard size (8,16,32,64) that fits

      Lo := Enumeration_Rep (Entity (Type_Low_Bound (T)));
      Hi := Enumeration_Rep (Entity (Type_High_Bound (T)));

      if Lo < 0 then
         if Lo >= -Uint_2**07 and then Hi < Uint_2**07 then
            Sz := Standard_Character_Size;  -- May be > 8 on some targets

         elsif Lo >= -Uint_2**15 and then Hi < Uint_2**15 then
            Sz := 16;

         elsif Lo >= -Uint_2**31 and then Hi < Uint_2**31 then
            Sz := 32;

         else pragma Assert (Lo >= -Uint_2**63 and then Hi < Uint_2**63);
            Sz := 64;
         end if;

      else
         if Hi < Uint_2**08 then
            Sz := Standard_Character_Size;  -- May be > 8 on some targets

         elsif Hi < Uint_2**16 then
            Sz := 16;

         elsif Hi < Uint_2**32 then
            Sz := 32;

         else pragma Assert (Hi < Uint_2**63);
            Sz := 64;
         end if;
      end if;

      --  That minimum is the proper size unless we have a foreign convention
      --  and the size required is 32 or less, in which case we bump the size
      --  up to 32. This is required for C and C++ and seems reasonable for
      --  all other foreign conventions.

      if Has_Foreign_Convention (T)
        and then Esize (T) < Standard_Integer_Size
      then
         Init_Esize (T, Standard_Integer_Size);
      else
         Init_Esize (T, Sz);
      end if;
   end Set_Enum_Esize;

   ------------------------------
   -- Validate_Address_Clauses --
   ------------------------------

   procedure Validate_Address_Clauses is
   begin
      for J in Address_Clause_Checks.First .. Address_Clause_Checks.Last loop
         declare
            ACCR : Address_Clause_Check_Record
                     renames Address_Clause_Checks.Table (J);

            Expr : Node_Id;

            X_Alignment : Uint;
            Y_Alignment : Uint;

            X_Size : Uint;
            Y_Size : Uint;

         begin
            --  Skip processing of this entry if warning already posted

            if not Address_Warning_Posted (ACCR.N) then
               Expr := Original_Node (Expression (ACCR.N));

               --  Get alignments

               X_Alignment := Alignment (ACCR.X);
               Y_Alignment := Alignment (ACCR.Y);

               --  Similarly obtain sizes

               X_Size := Esize (ACCR.X);
               Y_Size := Esize (ACCR.Y);

               --  Check for large object overlaying smaller one

               if Y_Size > Uint_0
                 and then X_Size > Uint_0
                 and then X_Size > Y_Size
               then
                  Error_Msg_NE
                    ("?& overlays smaller object", ACCR.N, ACCR.X);
                  Error_Msg_N
                    ("\??program execution may be erroneous", ACCR.N);
                  Error_Msg_Uint_1 := X_Size;
                  Error_Msg_NE
                    ("\??size of & is ^", ACCR.N, ACCR.X);
                  Error_Msg_Uint_1 := Y_Size;
                  Error_Msg_NE
                    ("\??size of & is ^", ACCR.N, ACCR.Y);

               --  Check for inadequate alignment, both of the base object
               --  and of the offset, if any.

               --  Note: we do not check the alignment if we gave a size
               --  warning, since it would likely be redundant.

               elsif Y_Alignment /= Uint_0
                 and then (Y_Alignment < X_Alignment
                             or else (ACCR.Off
                                        and then
                                          Nkind (Expr) = N_Attribute_Reference
                                        and then
                                          Attribute_Name (Expr) = Name_Address
                                        and then
                                          Has_Compatible_Alignment
                                            (ACCR.X, Prefix (Expr))
                                             /= Known_Compatible))
               then
                  Error_Msg_NE
                    ("??specified address for& may be inconsistent "
                       & "with alignment", ACCR.N, ACCR.X);
                  Error_Msg_N
                    ("\??program execution may be erroneous (RM 13.3(27))",
                     ACCR.N);
                  Error_Msg_Uint_1 := X_Alignment;
                  Error_Msg_NE
                    ("\??alignment of & is ^", ACCR.N, ACCR.X);
                  Error_Msg_Uint_1 := Y_Alignment;
                  Error_Msg_NE
                    ("\??alignment of & is ^", ACCR.N, ACCR.Y);
                  if Y_Alignment >= X_Alignment then
                     Error_Msg_N
                      ("\??but offset is not multiple of alignment", ACCR.N);
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Validate_Address_Clauses;

   ---------------------------
   -- Validate_Independence --
   ---------------------------

   procedure Validate_Independence is
      SU   : constant Uint := UI_From_Int (System_Storage_Unit);
      N    : Node_Id;
      E    : Entity_Id;
      IC   : Boolean;
      Comp : Entity_Id;
      Addr : Node_Id;
      P    : Node_Id;

      procedure Check_Array_Type (Atyp : Entity_Id);
      --  Checks if the array type Atyp has independent components, and
      --  if not, outputs an appropriate set of error messages.

      procedure No_Independence;
      --  Output message that independence cannot be guaranteed

      function OK_Component (C : Entity_Id) return Boolean;
      --  Checks one component to see if it is independently accessible, and
      --  if so yields True, otherwise yields False if independent access
      --  cannot be guaranteed. This is a conservative routine, it only
      --  returns True if it knows for sure, it returns False if it knows
      --  there is a problem, or it cannot be sure there is no problem.

      procedure Reason_Bad_Component (C : Entity_Id);
      --  Outputs continuation message if a reason can be determined for
      --  the component C being bad.

      ----------------------
      -- Check_Array_Type --
      ----------------------

      procedure Check_Array_Type (Atyp : Entity_Id) is
         Ctyp : constant Entity_Id := Component_Type (Atyp);

      begin
         --  OK if no alignment clause, no pack, and no component size

         if not Has_Component_Size_Clause (Atyp)
           and then not Has_Alignment_Clause (Atyp)
           and then not Is_Packed (Atyp)
         then
            return;
         end if;

         --  Check actual component size

         if not Known_Component_Size (Atyp)
           or else not (Addressable (Component_Size (Atyp))
                          and then Component_Size (Atyp) < 64)
           or else Component_Size (Atyp) mod Esize (Ctyp) /= 0
         then
            No_Independence;

            --  Bad component size, check reason

            if Has_Component_Size_Clause (Atyp) then
               P := Get_Attribute_Definition_Clause
                      (Atyp, Attribute_Component_Size);

               if Present (P) then
                  Error_Msg_Sloc := Sloc (P);
                  Error_Msg_N ("\because of Component_Size clause#", N);
                  return;
               end if;
            end if;

            if Is_Packed (Atyp) then
               P := Get_Rep_Pragma (Atyp, Name_Pack);

               if Present (P) then
                  Error_Msg_Sloc := Sloc (P);
                  Error_Msg_N ("\because of pragma Pack#", N);
                  return;
               end if;
            end if;

            --  No reason found, just return

            return;
         end if;

         --  Array type is OK independence-wise

         return;
      end Check_Array_Type;

      ---------------------
      -- No_Independence --
      ---------------------

      procedure No_Independence is
      begin
         if Pragma_Name (N) = Name_Independent then
            Error_Msg_NE ("independence cannot be guaranteed for&", N, E);
         else
            Error_Msg_NE
              ("independent components cannot be guaranteed for&", N, E);
         end if;
      end No_Independence;

      ------------------
      -- OK_Component --
      ------------------

      function OK_Component (C : Entity_Id) return Boolean is
         Rec  : constant Entity_Id := Scope (C);
         Ctyp : constant Entity_Id := Etype (C);

      begin
         --  OK if no component clause, no Pack, and no alignment clause

         if No (Component_Clause (C))
           and then not Is_Packed (Rec)
           and then not Has_Alignment_Clause (Rec)
         then
            return True;
         end if;

         --  Here we look at the actual component layout. A component is
         --  addressable if its size is a multiple of the Esize of the
         --  component type, and its starting position in the record has
         --  appropriate alignment, and the record itself has appropriate
         --  alignment to guarantee the component alignment.

         --  Make sure sizes are static, always assume the worst for any
         --  cases where we cannot check static values.

         if not (Known_Static_Esize (C)
                  and then
                 Known_Static_Esize (Ctyp))
         then
            return False;
         end if;

         --  Size of component must be addressable or greater than 64 bits
         --  and a multiple of bytes.

         if not Addressable (Esize (C)) and then Esize (C) < Uint_64 then
            return False;
         end if;

         --  Check size is proper multiple

         if Esize (C) mod Esize (Ctyp) /= 0 then
            return False;
         end if;

         --  Check alignment of component is OK

         if not Known_Component_Bit_Offset (C)
           or else Component_Bit_Offset (C) < Uint_0
           or else Component_Bit_Offset (C) mod Esize (Ctyp) /= 0
         then
            return False;
         end if;

         --  Check alignment of record type is OK

         if not Known_Alignment (Rec)
           or else (Alignment (Rec) * SU) mod Esize (Ctyp) /= 0
         then
            return False;
         end if;

         --  All tests passed, component is addressable

         return True;
      end OK_Component;

      --------------------------
      -- Reason_Bad_Component --
      --------------------------

      procedure Reason_Bad_Component (C : Entity_Id) is
         Rec  : constant Entity_Id := Scope (C);
         Ctyp : constant Entity_Id := Etype (C);

      begin
         --  If component clause present assume that's the problem

         if Present (Component_Clause (C)) then
            Error_Msg_Sloc := Sloc (Component_Clause (C));
            Error_Msg_N ("\because of Component_Clause#", N);
            return;
         end if;

         --  If pragma Pack clause present, assume that's the problem

         if Is_Packed (Rec) then
            P := Get_Rep_Pragma (Rec, Name_Pack);

            if Present (P) then
               Error_Msg_Sloc := Sloc (P);
               Error_Msg_N ("\because of pragma Pack#", N);
               return;
            end if;
         end if;

         --  See if record has bad alignment clause

         if Has_Alignment_Clause (Rec)
           and then Known_Alignment (Rec)
           and then (Alignment (Rec) * SU) mod Esize (Ctyp) /= 0
         then
            P := Get_Attribute_Definition_Clause (Rec, Attribute_Alignment);

            if Present (P) then
               Error_Msg_Sloc := Sloc (P);
               Error_Msg_N ("\because of Alignment clause#", N);
            end if;
         end if;

         --  Couldn't find a reason, so return without a message

         return;
      end Reason_Bad_Component;

   --  Start of processing for Validate_Independence

   begin
      for J in Independence_Checks.First .. Independence_Checks.Last loop
         N  := Independence_Checks.Table (J).N;
         E  := Independence_Checks.Table (J).E;
         IC := Pragma_Name (N) = Name_Independent_Components;

         --  Deal with component case

         if Ekind (E) = E_Discriminant or else Ekind (E) = E_Component then
            if not OK_Component (E) then
               No_Independence;
               Reason_Bad_Component (E);
               goto Continue;
            end if;
         end if;

         --  Deal with record with Independent_Components

         if IC and then Is_Record_Type (E) then
            Comp := First_Component_Or_Discriminant (E);
            while Present (Comp) loop
               if not OK_Component (Comp) then
                  No_Independence;
                  Reason_Bad_Component (Comp);
                  goto Continue;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;
         end if;

         --  Deal with address clause case

         if Is_Object (E) then
            Addr := Address_Clause (E);

            if Present (Addr) then
               No_Independence;
               Error_Msg_Sloc := Sloc (Addr);
               Error_Msg_N ("\because of Address clause#", N);
               goto Continue;
            end if;
         end if;

         --  Deal with independent components for array type

         if IC and then Is_Array_Type (E) then
            Check_Array_Type (E);
         end if;

         --  Deal with independent components for array object

         if IC and then Is_Object (E) and then Is_Array_Type (Etype (E)) then
            Check_Array_Type (Etype (E));
         end if;

      <<Continue>> null;
      end loop;
   end Validate_Independence;

   -----------------------------------
   -- Validate_Unchecked_Conversion --
   -----------------------------------

   procedure Validate_Unchecked_Conversion
     (N        : Node_Id;
      Act_Unit : Entity_Id)
   is
      Source : Entity_Id;
      Target : Entity_Id;
      Vnode  : Node_Id;

   begin
      --  Obtain source and target types. Note that we call Ancestor_Subtype
      --  here because the processing for generic instantiation always makes
      --  subtypes, and we want the original frozen actual types.

      --  If we are dealing with private types, then do the check on their
      --  fully declared counterparts if the full declarations have been
      --  encountered (they don't have to be visible, but they must exist!)

      Source := Ancestor_Subtype (Etype (First_Formal (Act_Unit)));

      if Is_Private_Type (Source)
        and then Present (Underlying_Type (Source))
      then
         Source := Underlying_Type (Source);
      end if;

      Target := Ancestor_Subtype (Etype (Act_Unit));

      --  If either type is generic, the instantiation happens within a generic
      --  unit, and there is nothing to check. The proper check will happen
      --  when the enclosing generic is instantiated.

      if Is_Generic_Type (Source) or else Is_Generic_Type (Target) then
         return;
      end if;

      if Is_Private_Type (Target)
        and then Present (Underlying_Type (Target))
      then
         Target := Underlying_Type (Target);
      end if;

      --  Source may be unconstrained array, but not target

      if Is_Array_Type (Target) and then not Is_Constrained (Target) then
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
         --  Give warnings for subprogram pointers only on most targets. The
         --  exception is VMS, where data pointers can have different lengths
         --  depending on the pointer convention.

         if Is_Access_Subprogram_Type (Target)
           or else Is_Access_Subprogram_Type (Source)
           or else OpenVMS_On_Target
         then
            Error_Msg_N
              ("?z?conversion between pointers with different conventions!",
               N);
         end if;
      end if;

      --  Warn if one of the operands is Ada.Calendar.Time. Do not emit a
      --  warning when compiling GNAT-related sources.

      if Warn_On_Unchecked_Conversion
        and then not In_Predefined_Unit (N)
        and then RTU_Loaded (Ada_Calendar)
        and then
          (Chars (Source) = Name_Time
             or else
           Chars (Target) = Name_Time)
      then
         --  If Ada.Calendar is loaded and the name of one of the operands is
         --  Time, there is a good chance that this is Ada.Calendar.Time.

         declare
            Calendar_Time : constant Entity_Id :=
                              Full_View (RTE (RO_CA_Time));
         begin
            pragma Assert (Present (Calendar_Time));

            if Source = Calendar_Time or else Target = Calendar_Time then
               Error_Msg_N
                 ("?z?representation of 'Time values may change between " &
                  "'G'N'A'T versions", N);
            end if;
         end;
      end if;

      --  Make entry in unchecked conversion table for later processing by
      --  Validate_Unchecked_Conversions, which will check sizes and alignments
      --  (using values set by the back-end where possible). This is only done
      --  if the appropriate warning is active.

      if Warn_On_Unchecked_Conversion then
         Unchecked_Conversions.Append
           (New_Val => UC_Entry'(Eloc   => Sloc (N),
                                 Source => Source,
                                 Target => Target));

         --  If both sizes are known statically now, then back end annotation
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

      if Is_Access_Type (Target) and then
        In_Same_Source_Unit (Target, N)
      then
         Set_No_Strict_Aliasing (Implementation_Base_Type (Target));
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
   begin
      for N in Unchecked_Conversions.First .. Unchecked_Conversions.Last loop
         declare
            T : UC_Entry renames Unchecked_Conversions.Table (N);

            Eloc   : constant Source_Ptr := T.Eloc;
            Source : constant Entity_Id  := T.Source;
            Target : constant Entity_Id  := T.Target;

            Source_Siz : Uint;
            Target_Siz : Uint;

         begin
            --  This validation check, which warns if we have unequal sizes for
            --  unchecked conversion, and thus potentially implementation
            --  dependent semantics, is one of the few occasions on which we
            --  use the official RM size instead of Esize. See description in
            --  Einfo "Handling of Type'Size Values" for details.

            if Serious_Errors_Detected = 0
              and then Known_Static_RM_Size (Source)
              and then Known_Static_RM_Size (Target)

              --  Don't do the check if warnings off for either type, note the
              --  deliberate use of OR here instead of OR ELSE to get the flag
              --  Warnings_Off_Used set for both types if appropriate.

              and then not (Has_Warnings_Off (Source)
                              or
                            Has_Warnings_Off (Target))
            then
               Source_Siz := RM_Size (Source);
               Target_Siz := RM_Size (Target);

               if Source_Siz /= Target_Siz then
                  Error_Msg
                    ("?z?types for unchecked conversion have different sizes!",
                     Eloc);

                  if All_Errors_Mode then
                     Error_Msg_Name_1 := Chars (Source);
                     Error_Msg_Uint_1 := Source_Siz;
                     Error_Msg_Name_2 := Chars (Target);
                     Error_Msg_Uint_2 := Target_Siz;
                     Error_Msg ("\size of % is ^, size of % is ^?z?", Eloc);

                     Error_Msg_Uint_1 := UI_Abs (Source_Siz - Target_Siz);

                     if Is_Discrete_Type (Source)
                          and then
                        Is_Discrete_Type (Target)
                     then
                        if Source_Siz > Target_Siz then
                           Error_Msg
                             ("\?z?^ high order bits of source will "
                              & "be ignored!", Eloc);

                        elsif Is_Unsigned_Type (Source) then
                           Error_Msg
                             ("\?z?source will be extended with ^ high order "
                              & "zero bits?!", Eloc);

                        else
                           Error_Msg
                             ("\?z?source will be extended with ^ high order "
                              & "sign bits!", Eloc);
                        end if;

                     elsif Source_Siz < Target_Siz then
                        if Is_Discrete_Type (Target) then
                           if Bytes_Big_Endian then
                              Error_Msg
                                ("\?z?target value will include ^ undefined "
                                 & "low order bits!", Eloc);
                           else
                              Error_Msg
                                ("\?z?target value will include ^ undefined "
                                 & "high order bits!", Eloc);
                           end if;

                        else
                           Error_Msg
                             ("\?z?^ trailing bits of target value will be "
                              & "undefined!", Eloc);
                        end if;

                     else pragma Assert (Source_Siz > Target_Siz);
                        Error_Msg
                          ("\?z?^ trailing bits of source will be ignored!",
                           Eloc);
                     end if;
                  end if;
               end if;
            end if;

            --  If both types are access types, we need to check the alignment.
            --  If the alignment of both is specified, we can do it here.

            if Serious_Errors_Detected = 0
              and then Ekind (Source) in Access_Kind
              and then Ekind (Target) in Access_Kind
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
                              & "alignment of & (^)!", Eloc);
                           Error_Msg
                             ("\?z?resulting access value may have invalid "
                              & "alignment!", Eloc);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Validate_Unchecked_Conversions;

end Sem_Ch13;
