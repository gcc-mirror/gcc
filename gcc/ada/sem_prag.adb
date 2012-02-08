------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

--  This unit contains the semantic processing for all pragmas, both language
--  and implementation defined. For most pragmas, the parser only does the
--  most basic job of checking the syntax, so Sem_Prag also contains the code
--  to complete the syntax checks. Certain pragmas are handled partially or
--  completely by the parser (see Par.Prag for further details).

with Aspects;  use Aspects;
with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Dist; use Exp_Dist;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Lib.Writ; use Lib.Writ;
with Lib.Xref; use Lib.Xref;
with Namet.Sp; use Namet.Sp;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Par_SCO;  use Par_SCO;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Intr; use Sem_Intr;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_VFpt; use Sem_VFpt;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stylesw;  use Stylesw;
with Table;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;   use Urealp;
with Validsw;  use Validsw;
with Warnsw;   use Warnsw;

package body Sem_Prag is

   ----------------------------------------------
   -- Common Handling of Import-Export Pragmas --
   ----------------------------------------------

   --  In the following section, a number of Import_xxx and Export_xxx pragmas
   --  are defined by GNAT. These are compatible with the DEC pragmas of the
   --  same name, and all have the following common form and processing:

   --  pragma Export_xxx
   --        [Internal                 =>] LOCAL_NAME
   --     [, [External                 =>] EXTERNAL_SYMBOL]
   --     [, other optional parameters   ]);

   --  pragma Import_xxx
   --        [Internal                 =>] LOCAL_NAME
   --     [, [External                 =>] EXTERNAL_SYMBOL]
   --     [, other optional parameters   ]);

   --   EXTERNAL_SYMBOL ::=
   --     IDENTIFIER
   --   | static_string_EXPRESSION

   --  The internal LOCAL_NAME designates the entity that is imported or
   --  exported, and must refer to an entity in the current declarative
   --  part (as required by the rules for LOCAL_NAME).

   --  The external linker name is designated by the External parameter if
   --  given, or the Internal parameter if not (if there is no External
   --  parameter, the External parameter is a copy of the Internal name).

   --  If the External parameter is given as a string, then this string is
   --  treated as an external name (exactly as though it had been given as an
   --  External_Name parameter for a normal Import pragma).

   --  If the External parameter is given as an identifier (or there is no
   --  External parameter, so that the Internal identifier is used), then
   --  the external name is the characters of the identifier, translated
   --  to all upper case letters for OpenVMS versions of GNAT, and to all
   --  lower case letters for all other versions

   --  Note: the external name specified or implied by any of these special
   --  Import_xxx or Export_xxx pragmas override an external or link name
   --  specified in a previous Import or Export pragma.

   --  Note: these and all other DEC-compatible GNAT pragmas allow full use of
   --  named notation, following the standard rules for subprogram calls, i.e.
   --  parameters can be given in any order if named notation is used, and
   --  positional and named notation can be mixed, subject to the rule that all
   --  positional parameters must appear first.

   --  Note: All these pragmas are implemented exactly following the DEC design
   --  and implementation and are intended to be fully compatible with the use
   --  of these pragmas in the DEC Ada compiler.

   --------------------------------------------
   -- Checking for Duplicated External Names --
   --------------------------------------------

   --  It is suspicious if two separate Export pragmas use the same external
   --  name. The following table is used to diagnose this situation so that
   --  an appropriate warning can be issued.

   --  The Node_Id stored is for the N_String_Literal node created to hold
   --  the value of the external name. The Sloc of this node is used to
   --  cross-reference the location of the duplication.

   package Externals is new Table.Table (
     Table_Component_Type => Node_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Name_Externals");

   -------------------------------------
   -- Local Subprograms and Variables --
   -------------------------------------

   function Adjust_External_Name_Case (N : Node_Id) return Node_Id;
   --  This routine is used for possible casing adjustment of an explicit
   --  external name supplied as a string literal (the node N), according to
   --  the casing requirement of Opt.External_Name_Casing. If this is set to
   --  As_Is, then the string literal is returned unchanged, but if it is set
   --  to Uppercase or Lowercase, then a new string literal with appropriate
   --  casing is constructed.

   function Get_Base_Subprogram (Def_Id : Entity_Id) return Entity_Id;
   --  If Def_Id refers to a renamed subprogram, then the base subprogram (the
   --  original one, following the renaming chain) is returned. Otherwise the
   --  entity is returned unchanged. Should be in Einfo???

   procedure Preanalyze_TC_Args (N, Arg_Req, Arg_Ens : Node_Id);
   --  Preanalyze the boolean expressions in the Requires and Ensures arguments
   --  of a Test_Case pragma if present (possibly Empty). We treat these as
   --  spec expressions (i.e. similar to a default expression).

   procedure rv;
   --  This is a dummy function called by the processing for pragma Reviewable.
   --  It is there for assisting front end debugging. By placing a Reviewable
   --  pragma in the source program, a breakpoint on rv catches this place in
   --  the source, allowing convenient stepping to the point of interest.

   procedure Set_Unit_Name (N : Node_Id; With_Item : Node_Id);
   --  Place semantic information on the argument of an Elaborate/Elaborate_All
   --  pragma. Entity name for unit and its parents is taken from item in
   --  previous with_clause that mentions the unit.

   -------------------------------
   -- Adjust_External_Name_Case --
   -------------------------------

   function Adjust_External_Name_Case (N : Node_Id) return Node_Id is
      CC : Char_Code;

   begin
      --  Adjust case of literal if required

      if Opt.External_Name_Exp_Casing = As_Is then
         return N;

      else
         --  Copy existing string

         Start_String;

         --  Set proper casing

         for J in 1 .. String_Length (Strval (N)) loop
            CC := Get_String_Char (Strval (N), J);

            if Opt.External_Name_Exp_Casing = Uppercase
              and then CC >= Get_Char_Code ('a')
              and then CC <= Get_Char_Code ('z')
            then
               Store_String_Char (CC - 32);

            elsif Opt.External_Name_Exp_Casing = Lowercase
              and then CC >= Get_Char_Code ('A')
              and then CC <= Get_Char_Code ('Z')
            then
               Store_String_Char (CC + 32);

            else
               Store_String_Char (CC);
            end if;
         end loop;

         return
           Make_String_Literal (Sloc (N),
             Strval => End_String);
      end if;
   end Adjust_External_Name_Case;

   ------------------------------
   -- Analyze_PPC_In_Decl_Part --
   ------------------------------

   procedure Analyze_PPC_In_Decl_Part (N : Node_Id; S : Entity_Id) is
      Arg1 : constant Node_Id := First (Pragma_Argument_Associations (N));

   begin
      --  Install formals and push subprogram spec onto scope stack so that we
      --  can see the formals from the pragma.

      Install_Formals (S);
      Push_Scope (S);

      --  Preanalyze the boolean expression, we treat this as a spec expression
      --  (i.e. similar to a default expression).

      Preanalyze_Spec_Expression (Get_Pragma_Arg (Arg1), Standard_Boolean);

      --  In ASIS mode, for a pragma generated from a source aspect, also
      --  analyze the original aspect expression.

      if ASIS_Mode
        and then Present (Corresponding_Aspect (N))
      then
         Preanalyze_Spec_Expression
           (Expression (Corresponding_Aspect (N)), Standard_Boolean);
      end if;

      --  For a class-wide condition, a reference to a controlling formal must
      --  be interpreted as having the class-wide type (or an access to such)
      --  so that the inherited condition can be properly applied to any
      --  overriding operation (see ARM12 6.6.1 (7)).

      if Class_Present (N) then
         declare
            T   : constant Entity_Id := Find_Dispatching_Type (S);

            ACW : Entity_Id := Empty;
            --  Access to T'class, created if there is a controlling formal
            --  that is an access parameter.

            function Get_ACW return Entity_Id;
            --  If the expression has a reference to an controlling access
            --  parameter, create an access to T'class for the necessary
            --  conversions if one does not exist.

            function Process (N : Node_Id) return Traverse_Result;
            --  ARM 6.1.1: Within the expression for a Pre'Class or Post'Class
            --  aspect for a primitive subprogram of a tagged type T, a name
            --  that denotes a formal parameter of type T is interpreted as
            --  having type T'Class. Similarly, a name that denotes a formal
            --  accessparameter of type access-to-T is interpreted as having
            --  type access-to-T'Class. This ensures the expression is well-
            --  defined for a primitive subprogram of a type descended from T.

            -------------
            -- Get_ACW --
            -------------

            function Get_ACW return Entity_Id is
               Loc  : constant Source_Ptr := Sloc (N);
               Decl : Node_Id;

            begin
               if No (ACW) then
                  Decl := Make_Full_Type_Declaration (Loc,
                    Defining_Identifier => Make_Temporary (Loc, 'T'),
                    Type_Definition =>
                       Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Occurrence_Of (Class_Wide_Type (T), Loc),
                       All_Present => True));

                  Insert_Before (Unit_Declaration_Node (S), Decl);
                  Analyze (Decl);
                  ACW := Defining_Identifier (Decl);
                  Freeze_Before (Unit_Declaration_Node (S), ACW);
               end if;

               return ACW;
            end Get_ACW;

            -------------
            -- Process --
            -------------

            function Process (N : Node_Id) return Traverse_Result is
               Loc : constant Source_Ptr := Sloc (N);
               Typ : Entity_Id;

            begin
               if Is_Entity_Name (N)
                 and then Is_Formal (Entity (N))
                 and then Nkind (Parent (N)) /= N_Type_Conversion
               then
                  if Etype (Entity (N)) = T then
                     Typ := Class_Wide_Type (T);

                  elsif Is_Access_Type (Etype (Entity (N)))
                    and then Designated_Type (Etype (Entity (N))) = T
                  then
                     Typ := Get_ACW;
                  else
                     Typ := Empty;
                  end if;

                  if Present (Typ) then
                     Rewrite (N,
                       Make_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of (Typ, Loc),
                         Expression  => New_Occurrence_Of (Entity (N), Loc)));
                     Set_Etype (N, Typ);
                  end if;
               end if;

               return OK;
            end Process;

            procedure Replace_Type is new Traverse_Proc (Process);

         begin
            Replace_Type (Get_Pragma_Arg (Arg1));
         end;
      end if;

      --  Remove the subprogram from the scope stack now that the pre-analysis
      --  of the precondition/postcondition is done.

      End_Scope;
   end Analyze_PPC_In_Decl_Part;

   --------------------
   -- Analyze_Pragma --
   --------------------

   procedure Analyze_Pragma (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Prag_Id : Pragma_Id;

      Pname : Name_Id;
      --  Name of the source pragma, or name of the corresponding aspect for
      --  pragmas which originate in a source aspect. In the latter case, the
      --  name may be different from the pragma name.

      Pragma_Exit : exception;
      --  This exception is used to exit pragma processing completely. It is
      --  used when an error is detected, and no further processing is
      --  required. It is also used if an earlier error has left the tree in
      --  a state where the pragma should not be processed.

      Arg_Count : Nat;
      --  Number of pragma argument associations

      Arg1 : Node_Id;
      Arg2 : Node_Id;
      Arg3 : Node_Id;
      Arg4 : Node_Id;
      --  First four pragma arguments (pragma argument association nodes, or
      --  Empty if the corresponding argument does not exist).

      type Name_List is array (Natural range <>) of Name_Id;
      type Args_List is array (Natural range <>) of Node_Id;
      --  Types used for arguments to Check_Arg_Order and Gather_Associations

      procedure Ada_2005_Pragma;
      --  Called for pragmas defined in Ada 2005, that are not in Ada 95. In
      --  Ada 95 mode, these are implementation defined pragmas, so should be
      --  caught by the No_Implementation_Pragmas restriction.

      procedure Ada_2012_Pragma;
      --  Called for pragmas defined in Ada 2012, that are not in Ada 95 or 05.
      --  In Ada 95 or 05 mode, these are implementation defined pragmas, so
      --  should be caught by the No_Implementation_Pragmas restriction.

      procedure Check_Ada_83_Warning;
      --  Issues a warning message for the current pragma if operating in Ada
      --  83 mode (used for language pragmas that are not a standard part of
      --  Ada 83). This procedure does not raise Error_Pragma. Also notes use
      --  of 95 pragma.

      procedure Check_Arg_Count (Required : Nat);
      --  Check argument count for pragma is equal to given parameter. If not,
      --  then issue an error message and raise Pragma_Exit.

      --  Note: all routines whose name is Check_Arg_Is_xxx take an argument
      --  Arg which can either be a pragma argument association, in which case
      --  the check is applied to the expression of the association or an
      --  expression directly.

      procedure Check_Arg_Is_External_Name (Arg : Node_Id);
      --  Check that an argument has the right form for an EXTERNAL_NAME
      --  parameter of an extended import/export pragma. The rule is that the
      --  name must be an identifier or string literal (in Ada 83 mode) or a
      --  static string expression (in Ada 95 mode).

      procedure Check_Arg_Is_Identifier (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  identifier. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Integer_Literal (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is an integer
      --  literal. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Library_Level_Local_Name (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it has the proper
      --  syntactic form for a local name and meets the semantic requirements
      --  for a local name. The local name is analyzed as part of the
      --  processing for this call. In addition, the local name is required
      --  to represent an entity at the library level.

      procedure Check_Arg_Is_Local_Name (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it has the proper
      --  syntactic form for a local name and meets the semantic requirements
      --  for a local name. The local name is analyzed as part of the
      --  processing for this call.

      procedure Check_Arg_Is_Locking_Policy (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a valid
      --  locking policy name. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_One_Of
        (Arg                : Node_Id;
         N1, N2             : Name_Id);
      procedure Check_Arg_Is_One_Of
        (Arg                : Node_Id;
         N1, N2, N3         : Name_Id);
      procedure Check_Arg_Is_One_Of
        (Arg                : Node_Id;
         N1, N2, N3, N4     : Name_Id);
      procedure Check_Arg_Is_One_Of
        (Arg                : Node_Id;
         N1, N2, N3, N4, N5 : Name_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  identifier whose name matches either N1 or N2 (or N3, N4, N5 if
      --  present). If not then give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Queuing_Policy (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a valid
      --  queuing policy name. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Static_Expression
        (Arg : Node_Id;
         Typ : Entity_Id := Empty);
      --  Check the specified argument Arg to make sure that it is a static
      --  expression of the given type (i.e. it will be analyzed and resolved
      --  using this type, which can be any valid argument to Resolve, e.g.
      --  Any_Integer is OK). If not, given error and raise Pragma_Exit. If
      --  Typ is left Empty, then any static expression is allowed.

      procedure Check_Arg_Is_Task_Dispatching_Policy (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a valid task
      --  dispatching policy name. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Order (Names : Name_List);
      --  Checks for an instance of two arguments with identifiers for the
      --  current pragma which are not in the sequence indicated by Names,
      --  and if so, generates a fatal message about bad order of arguments.

      procedure Check_At_Least_N_Arguments (N : Nat);
      --  Check there are at least N arguments present

      procedure Check_At_Most_N_Arguments (N : Nat);
      --  Check there are no more than N arguments present

      procedure Check_Component
        (Comp            : Node_Id;
         UU_Typ          : Entity_Id;
         In_Variant_Part : Boolean := False);
      --  Examine an Unchecked_Union component for correct use of per-object
      --  constrained subtypes, and for restrictions on finalizable components.
      --  UU_Typ is the related Unchecked_Union type. Flag In_Variant_Part
      --  should be set when Comp comes from a record variant.

      procedure Check_Duplicate_Pragma (E : Entity_Id);
      --  Check if a pragma of the same name as the current pragma is already
      --  chained as a rep pragma to the given entity. If so give a message
      --  about the duplicate, and then raise Pragma_Exit so does not return.
      --  Also checks for delayed aspect specification node in the chain.

      procedure Check_Duplicated_Export_Name (Nam : Node_Id);
      --  Nam is an N_String_Literal node containing the external name set by
      --  an Import or Export pragma (or extended Import or Export pragma).
      --  This procedure checks for possible duplications if this is the export
      --  case, and if found, issues an appropriate error message.

      procedure Check_Expr_Is_Static_Expression
        (Expr : Node_Id;
         Typ  : Entity_Id := Empty);
      --  Check the specified expression Expr to make sure that it is a static
      --  expression of the given type (i.e. it will be analyzed and resolved
      --  using this type, which can be any valid argument to Resolve, e.g.
      --  Any_Integer is OK). If not, given error and raise Pragma_Exit. If
      --  Typ is left Empty, then any static expression is allowed.

      procedure Check_First_Subtype (Arg : Node_Id);
      --  Checks that Arg, whose expression is an entity name, references a
      --  first subtype.

      procedure Check_Identifier (Arg : Node_Id; Id : Name_Id);
      --  Checks that the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is no identifier, or
      --  a non-matching identifier, then an error message is given and
      --  Pragma_Exit is raised.

      procedure Check_Identifier_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id);
      --  Checks that the given argument has an identifier, and if so, requires
      --  it to match one of the given identifier names. If there is no
      --  identifier, or a non-matching identifier, then an error message is
      --  given and Pragma_Exit is raised.

      procedure Check_In_Main_Program;
      --  Common checks for pragmas that appear within a main program
      --  (Priority, Main_Storage, Time_Slice, Relative_Deadline, CPU).

      procedure Check_Interrupt_Or_Attach_Handler;
      --  Common processing for first argument of pragma Interrupt_Handler or
      --  pragma Attach_Handler.

      procedure Check_Is_In_Decl_Part_Or_Package_Spec;
      --  Check that pragma appears in a declarative part, or in a package
      --  specification, i.e. that it does not occur in a statement sequence
      --  in a body.

      procedure Check_No_Identifier (Arg : Node_Id);
      --  Checks that the given argument does not have an identifier. If
      --  an identifier is present, then an error message is issued, and
      --  Pragma_Exit is raised.

      procedure Check_No_Identifiers;
      --  Checks that none of the arguments to the pragma has an identifier.
      --  If any argument has an identifier, then an error message is issued,
      --  and Pragma_Exit is raised.

      procedure Check_No_Link_Name;
      --  Checks that no link name is specified

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id);
      --  Checks if the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is a non-matching
      --  identifier, then an error message is given and Pragma_Exit is raised.

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : String);
      --  Checks if the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is a non-matching
      --  identifier, then an error message is given and Pragma_Exit is raised.
      --  In this version of the procedure, the identifier name is given as
      --  a string with lower case letters.

      procedure Check_Precondition_Postcondition (In_Body : out Boolean);
      --  Called to process a precondition or postcondition pragma. There are
      --  three cases:
      --
      --    The pragma appears after a subprogram spec
      --
      --      If the corresponding check is not enabled, the pragma is analyzed
      --      but otherwise ignored and control returns with In_Body set False.
      --
      --      If the check is enabled, then the first step is to analyze the
      --      pragma, but this is skipped if the subprogram spec appears within
      --      a package specification (because this is the case where we delay
      --      analysis till the end of the spec). Then (whether or not it was
      --      analyzed), the pragma is chained to the subprogram in question
      --      (using Spec_PPC_List and Next_Pragma) and control returns to the
      --      caller with In_Body set False.
      --
      --    The pragma appears at the start of subprogram body declarations
      --
      --      In this case an immediate return to the caller is made with
      --      In_Body set True, and the pragma is NOT analyzed.
      --
      --    In all other cases, an error message for bad placement is given

      procedure Check_Static_Constraint (Constr : Node_Id);
      --  Constr is a constraint from an N_Subtype_Indication node from a
      --  component constraint in an Unchecked_Union type. This routine checks
      --  that the constraint is static as required by the restrictions for
      --  Unchecked_Union.

      procedure Check_Test_Case;
      --  Called to process a test-case pragma. The treatment is similar to the
      --  one for pre- and postcondition in Check_Precondition_Postcondition,
      --  except the placement rules for the test-case pragma are stricter.
      --  This pragma may only occur after a subprogram spec declared directly
      --  in a package spec unit. In this case, the pragma is chained to the
      --  subprogram in question (using Spec_TC_List and Next_Pragma) and
      --  analysis of the pragma is delayed till the end of the spec. In
      --  all other cases, an error message for bad placement is given.

      procedure Check_Valid_Configuration_Pragma;
      --  Legality checks for placement of a configuration pragma

      procedure Check_Valid_Library_Unit_Pragma;
      --  Legality checks for library unit pragmas. A special case arises for
      --  pragmas in generic instances that come from copies of the original
      --  library unit pragmas in the generic templates. In the case of other
      --  than library level instantiations these can appear in contexts which
      --  would normally be invalid (they only apply to the original template
      --  and to library level instantiations), and they are simply ignored,
      --  which is implemented by rewriting them as null statements.

      procedure Check_Variant (Variant : Node_Id; UU_Typ : Entity_Id);
      --  Check an Unchecked_Union variant for lack of nested variants and
      --  presence of at least one component. UU_Typ is the related Unchecked_
      --  Union type.

      procedure Error_Pragma (Msg : String);
      pragma No_Return (Error_Pragma);
      --  Outputs error message for current pragma. The message contains a %
      --  that will be replaced with the pragma name, and the flag is placed
      --  on the pragma itself. Pragma_Exit is then raised.

      procedure Error_Pragma_Arg (Msg : String; Arg : Node_Id);
      pragma No_Return (Error_Pragma_Arg);
      --  Outputs error message for current pragma. The message may contain
      --  a % that will be replaced with the pragma name. The parameter Arg
      --  may either be a pragma argument association, in which case the flag
      --  is placed on the expression of this association, or an expression,
      --  in which case the flag is placed directly on the expression. The
      --  message is placed using Error_Msg_N, so the message may also contain
      --  an & insertion character which will reference the given Arg value.
      --  After placing the message, Pragma_Exit is raised.

      procedure Error_Pragma_Arg (Msg1, Msg2 : String; Arg : Node_Id);
      pragma No_Return (Error_Pragma_Arg);
      --  Similar to above form of Error_Pragma_Arg except that two messages
      --  are provided, the second is a continuation comment starting with \.

      procedure Error_Pragma_Arg_Ident (Msg : String; Arg : Node_Id);
      pragma No_Return (Error_Pragma_Arg_Ident);
      --  Outputs error message for current pragma. The message may contain
      --  a % that will be replaced with the pragma name. The parameter Arg
      --  must be a pragma argument association with a non-empty identifier
      --  (i.e. its Chars field must be set), and the error message is placed
      --  on the identifier. The message is placed using Error_Msg_N so
      --  the message may also contain an & insertion character which will
      --  reference the identifier. After placing the message, Pragma_Exit
      --  is raised.

      procedure Error_Pragma_Ref (Msg : String; Ref : Entity_Id);
      pragma No_Return (Error_Pragma_Ref);
      --  Outputs error message for current pragma. The message may contain
      --  a % that will be replaced with the pragma name. The parameter Ref
      --  must be an entity whose name can be referenced by & and sloc by #.
      --  After placing the message, Pragma_Exit is raised.

      function Find_Lib_Unit_Name return Entity_Id;
      --  Used for a library unit pragma to find the entity to which the
      --  library unit pragma applies, returns the entity found.

      procedure Find_Program_Unit_Name (Id : Node_Id);
      --  If the pragma is a compilation unit pragma, the id must denote the
      --  compilation unit in the same compilation, and the pragma must appear
      --  in the list of preceding or trailing pragmas. If it is a program
      --  unit pragma that is not a compilation unit pragma, then the
      --  identifier must be visible.

      function Find_Unique_Parameterless_Procedure
        (Name : Entity_Id;
         Arg  : Node_Id) return Entity_Id;
      --  Used for a procedure pragma to find the unique parameterless
      --  procedure identified by Name, returns it if it exists, otherwise
      --  errors out and uses Arg as the pragma argument for the message.

      procedure Fix_Error (Msg : in out String);
      --  This is called prior to issuing an error message. Msg is a string
      --  that typically contains the substring "pragma". If the current pragma
      --  comes from an aspect, each such "pragma" substring is replaced with
      --  the characters "aspect", and if Error_Msg_Name_1 is Name_Precondition
      --  (resp Name_Postcondition) it is changed to Name_Pre (resp Name_Post).

      procedure Gather_Associations
        (Names : Name_List;
         Args  : out Args_List);
      --  This procedure is used to gather the arguments for a pragma that
      --  permits arbitrary ordering of parameters using the normal rules
      --  for named and positional parameters. The Names argument is a list
      --  of Name_Id values that corresponds to the allowed pragma argument
      --  association identifiers in order. The result returned in Args is
      --  a list of corresponding expressions that are the pragma arguments.
      --  Note that this is a list of expressions, not of pragma argument
      --  associations (Gather_Associations has completely checked all the
      --  optional identifiers when it returns). An entry in Args is Empty
      --  on return if the corresponding argument is not present.

      procedure GNAT_Pragma;
      --  Called for all GNAT defined pragmas to check the relevant restriction
      --  (No_Implementation_Pragmas).

      function Is_Before_First_Decl
        (Pragma_Node : Node_Id;
         Decls       : List_Id) return Boolean;
      --  Return True if Pragma_Node is before the first declarative item in
      --  Decls where Decls is the list of declarative items.

      function Is_Configuration_Pragma return Boolean;
      --  Determines if the placement of the current pragma is appropriate
      --  for a configuration pragma.

      function Is_In_Context_Clause return Boolean;
      --  Returns True if pragma appears within the context clause of a unit,
      --  and False for any other placement (does not generate any messages).

      function Is_Static_String_Expression (Arg : Node_Id) return Boolean;
      --  Analyzes the argument, and determines if it is a static string
      --  expression, returns True if so, False if non-static or not String.

      procedure Pragma_Misplaced;
      pragma No_Return (Pragma_Misplaced);
      --  Issue fatal error message for misplaced pragma

      procedure Process_Atomic_Shared_Volatile;
      --  Common processing for pragmas Atomic, Shared, Volatile. Note that
      --  Shared is an obsolete Ada 83 pragma, treated as being identical
      --  in effect to pragma Atomic.

      procedure Process_Compile_Time_Warning_Or_Error;
      --  Common processing for Compile_Time_Error and Compile_Time_Warning

      procedure Process_Convention
        (C   : out Convention_Id;
         Ent : out Entity_Id);
      --  Common processing for Convention, Interface, Import and Export.
      --  Checks first two arguments of pragma, and sets the appropriate
      --  convention value in the specified entity or entities. On return
      --  C is the convention, Ent is the referenced entity.

      procedure Process_Disable_Enable_Atomic_Sync (Nam : Name_Id);
      --  Common processing for Disable/Enable_Atomic_Synchronization. Nam is
      --  Name_Suppress for Disable and Name_Unsuppress for Enable.

      procedure Process_Extended_Import_Export_Exception_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Form     : Node_Id;
         Arg_Code     : Node_Id);
      --  Common processing for the pragmas Import/Export_Exception. The three
      --  arguments correspond to the three named parameters of the pragma. An
      --  argument is empty if the corresponding parameter is not present in
      --  the pragma.

      procedure Process_Extended_Import_Export_Object_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Size     : Node_Id);
      --  Common processing for the pragmas Import/Export_Object. The three
      --  arguments correspond to the three named parameters of the pragmas. An
      --  argument is empty if the corresponding parameter is not present in
      --  the pragma.

      procedure Process_Extended_Import_Export_Internal_Arg
        (Arg_Internal : Node_Id := Empty);
      --  Common processing for all extended Import and Export pragmas. The
      --  argument is the pragma parameter for the Internal argument. If
      --  Arg_Internal is empty or inappropriate, an error message is posted.
      --  Otherwise, on normal return, the Entity_Field of Arg_Internal is
      --  set to identify the referenced entity.

      procedure Process_Extended_Import_Export_Subprogram_Pragma
        (Arg_Internal                 : Node_Id;
         Arg_External                 : Node_Id;
         Arg_Parameter_Types          : Node_Id;
         Arg_Result_Type              : Node_Id := Empty;
         Arg_Mechanism                : Node_Id;
         Arg_Result_Mechanism         : Node_Id := Empty;
         Arg_First_Optional_Parameter : Node_Id := Empty);
      --  Common processing for all extended Import and Export pragmas applying
      --  to subprograms. The caller omits any arguments that do not apply to
      --  the pragma in question (for example, Arg_Result_Type can be non-Empty
      --  only in the Import_Function and Export_Function cases). The argument
      --  names correspond to the allowed pragma association identifiers.

      procedure Process_Generic_List;
      --  Common processing for Share_Generic and Inline_Generic

      procedure Process_Import_Or_Interface;
      --  Common processing for Import of Interface

      procedure Process_Import_Predefined_Type;
      --  Processing for completing a type with pragma Import. This is used
      --  to declare types that match predefined C types, especially for cases
      --  without corresponding Ada predefined type.

      procedure Process_Inline (Active : Boolean);
      --  Common processing for Inline and Inline_Always. The parameter
      --  indicates if the inline pragma is active, i.e. if it should actually
      --  cause inlining to occur.

      procedure Process_Interface_Name
        (Subprogram_Def : Entity_Id;
         Ext_Arg        : Node_Id;
         Link_Arg       : Node_Id);
      --  Given the last two arguments of pragma Import, pragma Export, or
      --  pragma Interface_Name, performs validity checks and sets the
      --  Interface_Name field of the given subprogram entity to the
      --  appropriate external or link name, depending on the arguments given.
      --  Ext_Arg is always present, but Link_Arg may be missing. Note that
      --  Ext_Arg may represent the Link_Name if Link_Arg is missing, and
      --  appropriate named notation is used for Ext_Arg. If neither Ext_Arg
      --  nor Link_Arg is present, the interface name is set to the default
      --  from the subprogram name.

      procedure Process_Interrupt_Or_Attach_Handler;
      --  Common processing for Interrupt and Attach_Handler pragmas

      procedure Process_Restrictions_Or_Restriction_Warnings (Warn : Boolean);
      --  Common processing for Restrictions and Restriction_Warnings pragmas.
      --  Warn is True for Restriction_Warnings, or for Restrictions if the
      --  flag Treat_Restrictions_As_Warnings is set, and False if this flag
      --  is not set in the Restrictions case.

      procedure Process_Suppress_Unsuppress (Suppress_Case : Boolean);
      --  Common processing for Suppress and Unsuppress. The boolean parameter
      --  Suppress_Case is True for the Suppress case, and False for the
      --  Unsuppress case.

      procedure Set_Exported (E : Entity_Id; Arg : Node_Id);
      --  This procedure sets the Is_Exported flag for the given entity,
      --  checking that the entity was not previously imported. Arg is
      --  the argument that specified the entity. A check is also made
      --  for exporting inappropriate entities.

      procedure Set_Extended_Import_Export_External_Name
        (Internal_Ent : Entity_Id;
         Arg_External : Node_Id);
      --  Common processing for all extended import export pragmas. The first
      --  argument, Internal_Ent, is the internal entity, which has already
      --  been checked for validity by the caller. Arg_External is from the
      --  Import or Export pragma, and may be null if no External parameter
      --  was present. If Arg_External is present and is a non-null string
      --  (a null string is treated as the default), then the Interface_Name
      --  field of Internal_Ent is set appropriately.

      procedure Set_Imported (E : Entity_Id);
      --  This procedure sets the Is_Imported flag for the given entity,
      --  checking that it is not previously exported or imported.

      procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id);
      --  Mech is a parameter passing mechanism (see Import_Function syntax
      --  for MECHANISM_NAME). This routine checks that the mechanism argument
      --  has the right form, and if not issues an error message. If the
      --  argument has the right form then the Mechanism field of Ent is
      --  set appropriately.

      procedure Set_Ravenscar_Profile (N : Node_Id);
      --  Activate the set of configuration pragmas and restrictions that make
      --  up the Ravenscar Profile. N is the corresponding pragma node, which
      --  is used for error messages on any constructs that violate the
      --  profile.

      ---------------------
      -- Ada_2005_Pragma --
      ---------------------

      procedure Ada_2005_Pragma is
      begin
         if Ada_Version <= Ada_95 then
            Check_Restriction (No_Implementation_Pragmas, N);
         end if;
      end Ada_2005_Pragma;

      ---------------------
      -- Ada_2012_Pragma --
      ---------------------

      procedure Ada_2012_Pragma is
      begin
         if Ada_Version <= Ada_2005 then
            Check_Restriction (No_Implementation_Pragmas, N);
         end if;
      end Ada_2012_Pragma;

      --------------------------
      -- Check_Ada_83_Warning --
      --------------------------

      procedure Check_Ada_83_Warning is
      begin
         if Ada_Version = Ada_83 and then Comes_From_Source (N) then
            Error_Msg_N ("(Ada 83) pragma& is non-standard?", N);
         end if;
      end Check_Ada_83_Warning;

      ---------------------
      -- Check_Arg_Count --
      ---------------------

      procedure Check_Arg_Count (Required : Nat) is
      begin
         if Arg_Count /= Required then
            Error_Pragma ("wrong number of arguments for pragma%");
         end if;
      end Check_Arg_Count;

      --------------------------------
      -- Check_Arg_Is_External_Name --
      --------------------------------

      procedure Check_Arg_Is_External_Name (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) = N_Identifier then
            return;

         else
            Analyze_And_Resolve (Argx, Standard_String);

            if Is_OK_Static_Expression (Argx) then
               return;

            elsif Etype (Argx) = Any_Type then
               raise Pragma_Exit;

            --  An interesting special case, if we have a string literal and
            --  we are in Ada 83 mode, then we allow it even though it will
            --  not be flagged as static. This allows expected Ada 83 mode
            --  use of external names which are string literals, even though
            --  technically these are not static in Ada 83.

            elsif Ada_Version = Ada_83
              and then Nkind (Argx) = N_String_Literal
            then
               return;

            --  Static expression that raises Constraint_Error. This has
            --  already been flagged, so just exit from pragma processing.

            elsif Is_Static_Expression (Argx) then
               raise Pragma_Exit;

            --  Here we have a real error (non-static expression)

            else
               Error_Msg_Name_1 := Pname;

               declare
                  Msg : String :=
                          "argument for pragma% must be a identifier or "
                          & "static string expression!";
               begin
                  Fix_Error (Msg);
                  Flag_Non_Static_Expr (Msg, Argx);
                  raise Pragma_Exit;
               end;
            end if;
         end if;
      end Check_Arg_Is_External_Name;

      -----------------------------
      -- Check_Arg_Is_Identifier --
      -----------------------------

      procedure Check_Arg_Is_Identifier (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);
      begin
         if Nkind (Argx) /= N_Identifier then
            Error_Pragma_Arg
              ("argument for pragma% must be identifier", Argx);
         end if;
      end Check_Arg_Is_Identifier;

      ----------------------------------
      -- Check_Arg_Is_Integer_Literal --
      ----------------------------------

      procedure Check_Arg_Is_Integer_Literal (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);
      begin
         if Nkind (Argx) /= N_Integer_Literal then
            Error_Pragma_Arg
              ("argument for pragma% must be integer literal", Argx);
         end if;
      end Check_Arg_Is_Integer_Literal;

      -------------------------------------------
      -- Check_Arg_Is_Library_Level_Local_Name --
      -------------------------------------------

      --  LOCAL_NAME ::=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      procedure Check_Arg_Is_Library_Level_Local_Name (Arg : Node_Id) is
      begin
         Check_Arg_Is_Local_Name (Arg);

         if not Is_Library_Level_Entity (Entity (Get_Pragma_Arg (Arg)))
           and then Comes_From_Source (N)
         then
            Error_Pragma_Arg
              ("argument for pragma% must be library level entity", Arg);
         end if;
      end Check_Arg_Is_Library_Level_Local_Name;

      -----------------------------
      -- Check_Arg_Is_Local_Name --
      -----------------------------

      --  LOCAL_NAME ::=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      procedure Check_Arg_Is_Local_Name (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Analyze (Argx);

         if Nkind (Argx) not in N_Direct_Name
           and then (Nkind (Argx) /= N_Attribute_Reference
                      or else Present (Expressions (Argx))
                      or else Nkind (Prefix (Argx)) /= N_Identifier)
           and then (not Is_Entity_Name (Argx)
                      or else not Is_Compilation_Unit (Entity (Argx)))
         then
            Error_Pragma_Arg ("argument for pragma% must be local name", Argx);
         end if;

         --  No further check required if not an entity name

         if not Is_Entity_Name (Argx) then
            null;

         else
            declare
               OK   : Boolean;
               Ent  : constant Entity_Id := Entity (Argx);
               Scop : constant Entity_Id := Scope (Ent);
            begin
               --  Case of a pragma applied to a compilation unit: pragma must
               --  occur immediately after the program unit in the compilation.

               if Is_Compilation_Unit (Ent) then
                  declare
                     Decl : constant Node_Id := Unit_Declaration_Node (Ent);

                  begin
                     --  Case of pragma placed immediately after spec

                     if Parent (N) = Aux_Decls_Node (Parent (Decl)) then
                        OK := True;

                     --  Case of pragma placed immediately after body

                     elsif Nkind (Decl) = N_Subprogram_Declaration
                             and then Present (Corresponding_Body (Decl))
                     then
                        OK := Parent (N) =
                                Aux_Decls_Node
                                  (Parent (Unit_Declaration_Node
                                             (Corresponding_Body (Decl))));

                     --  All other cases are illegal

                     else
                        OK := False;
                     end if;
                  end;

               --  Special restricted placement rule from 10.2.1(11.8/2)

               elsif Is_Generic_Formal (Ent)
                       and then Prag_Id = Pragma_Preelaborable_Initialization
               then
                  OK := List_Containing (N) =
                          Generic_Formal_Declarations
                            (Unit_Declaration_Node (Scop));

               --  Default case, just check that the pragma occurs in the scope
               --  of the entity denoted by the name.

               else
                  OK := Current_Scope = Scop;
               end if;

               if not OK then
                  Error_Pragma_Arg
                    ("pragma% argument must be in same declarative part", Arg);
               end if;
            end;
         end if;
      end Check_Arg_Is_Local_Name;

      ---------------------------------
      -- Check_Arg_Is_Locking_Policy --
      ---------------------------------

      procedure Check_Arg_Is_Locking_Policy (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if not Is_Locking_Policy_Name (Chars (Argx)) then
            Error_Pragma_Arg ("& is not a valid locking policy name", Argx);
         end if;
      end Check_Arg_Is_Locking_Policy;

      -------------------------
      -- Check_Arg_Is_One_Of --
      -------------------------

      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1 and then Chars (Argx) /= N2 then
            Error_Msg_Name_2 := N1;
            Error_Msg_Name_3 := N2;
            Error_Pragma_Arg ("argument for pragma% must be% or%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      procedure Check_Arg_Is_One_Of
        (Arg        : Node_Id;
         N1, N2, N3 : Name_Id)
      is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1
           and then Chars (Argx) /= N2
           and then Chars (Argx) /= N3
         then
            Error_Pragma_Arg ("invalid argument for pragma%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      procedure Check_Arg_Is_One_Of
        (Arg                : Node_Id;
         N1, N2, N3, N4     : Name_Id)
      is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1
           and then Chars (Argx) /= N2
           and then Chars (Argx) /= N3
           and then Chars (Argx) /= N4
         then
            Error_Pragma_Arg ("invalid argument for pragma%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      procedure Check_Arg_Is_One_Of
        (Arg                : Node_Id;
         N1, N2, N3, N4, N5 : Name_Id)
      is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1
           and then Chars (Argx) /= N2
           and then Chars (Argx) /= N3
           and then Chars (Argx) /= N4
           and then Chars (Argx) /= N5
         then
            Error_Pragma_Arg ("invalid argument for pragma%", Argx);
         end if;
      end Check_Arg_Is_One_Of;
      ---------------------------------
      -- Check_Arg_Is_Queuing_Policy --
      ---------------------------------

      procedure Check_Arg_Is_Queuing_Policy (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if not Is_Queuing_Policy_Name (Chars (Argx)) then
            Error_Pragma_Arg ("& is not a valid queuing policy name", Argx);
         end if;
      end Check_Arg_Is_Queuing_Policy;

      ------------------------------------
      -- Check_Arg_Is_Static_Expression --
      ------------------------------------

      procedure Check_Arg_Is_Static_Expression
        (Arg : Node_Id;
         Typ : Entity_Id := Empty)
      is
      begin
         Check_Expr_Is_Static_Expression (Get_Pragma_Arg (Arg), Typ);
      end Check_Arg_Is_Static_Expression;

      ------------------------------------------
      -- Check_Arg_Is_Task_Dispatching_Policy --
      ------------------------------------------

      procedure Check_Arg_Is_Task_Dispatching_Policy (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if not Is_Task_Dispatching_Policy_Name (Chars (Argx)) then
            Error_Pragma_Arg
              ("& is not a valid task dispatching policy name", Argx);
         end if;
      end Check_Arg_Is_Task_Dispatching_Policy;

      ---------------------
      -- Check_Arg_Order --
      ---------------------

      procedure Check_Arg_Order (Names : Name_List) is
         Arg : Node_Id;

         Highest_So_Far : Natural := 0;
         --  Highest index in Names seen do far

      begin
         Arg := Arg1;
         for J in 1 .. Arg_Count loop
            if Chars (Arg) /= No_Name then
               for K in Names'Range loop
                  if Chars (Arg) = Names (K) then
                     if K < Highest_So_Far then
                        Error_Msg_Name_1 := Pname;
                        Error_Msg_N
                          ("parameters out of order for pragma%", Arg);
                        Error_Msg_Name_1 := Names (K);
                        Error_Msg_Name_2 := Names (Highest_So_Far);
                        Error_Msg_N ("\% must appear before %", Arg);
                        raise Pragma_Exit;

                     else
                        Highest_So_Far := K;
                     end if;
                  end if;
               end loop;
            end if;

            Arg := Next (Arg);
         end loop;
      end Check_Arg_Order;

      --------------------------------
      -- Check_At_Least_N_Arguments --
      --------------------------------

      procedure Check_At_Least_N_Arguments (N : Nat) is
      begin
         if Arg_Count < N then
            Error_Pragma ("too few arguments for pragma%");
         end if;
      end Check_At_Least_N_Arguments;

      -------------------------------
      -- Check_At_Most_N_Arguments --
      -------------------------------

      procedure Check_At_Most_N_Arguments (N : Nat) is
         Arg : Node_Id;
      begin
         if Arg_Count > N then
            Arg := Arg1;
            for J in 1 .. N loop
               Next (Arg);
               Error_Pragma_Arg ("too many arguments for pragma%", Arg);
            end loop;
         end if;
      end Check_At_Most_N_Arguments;

      ---------------------
      -- Check_Component --
      ---------------------

      procedure Check_Component
        (Comp            : Node_Id;
         UU_Typ          : Entity_Id;
         In_Variant_Part : Boolean := False)
      is
         Comp_Id : constant Entity_Id := Defining_Identifier (Comp);
         Sindic  : constant Node_Id :=
                     Subtype_Indication (Component_Definition (Comp));
         Typ     : constant Entity_Id := Etype (Comp_Id);

      begin
         --  Ada 2005 (AI-216): If a component subtype is subject to a per-
         --  object constraint, then the component type shall be an Unchecked_
         --  Union.

         if Nkind (Sindic) = N_Subtype_Indication
           and then Has_Per_Object_Constraint (Comp_Id)
           and then not Is_Unchecked_Union (Etype (Subtype_Mark (Sindic)))
         then
            Error_Msg_N
              ("component subtype subject to per-object constraint " &
               "must be an Unchecked_Union", Comp);

         --  Ada 2012 (AI05-0026): For an unchecked union type declared within
         --  the body of a generic unit, or within the body of any of its
         --  descendant library units, no part of the type of a component
         --  declared in a variant_part of the unchecked union type shall be of
         --  a formal private type or formal private extension declared within
         --  the formal part of the generic unit.

         elsif Ada_Version >= Ada_2012
           and then In_Generic_Body (UU_Typ)
           and then In_Variant_Part
           and then Is_Private_Type (Typ)
           and then Is_Generic_Type (Typ)
         then
            Error_Msg_N
              ("component of Unchecked_Union cannot be of generic type", Comp);

         elsif Needs_Finalization (Typ) then
            Error_Msg_N
              ("component of Unchecked_Union cannot be controlled", Comp);

         elsif Has_Task (Typ) then
            Error_Msg_N
              ("component of Unchecked_Union cannot have tasks", Comp);
         end if;
      end Check_Component;

      ----------------------------
      -- Check_Duplicate_Pragma --
      ----------------------------

      procedure Check_Duplicate_Pragma (E : Entity_Id) is
         P : Node_Id;

      begin
         --  Nothing to do if this pragma comes from an aspect specification,
         --  since we could not be duplicating a pragma, and we dealt with the
         --  case of duplicated aspects in Analyze_Aspect_Specifications.

         if From_Aspect_Specification (N) then
            return;
         end if;

         --  Otherwise current pragma may duplicate previous pragma or a
         --  previously given aspect specification for the same pragma.

         P := Get_Rep_Item_For_Entity (E, Pragma_Name (N));

         if Present (P) then
            Error_Msg_Name_1 := Pragma_Name (N);
            Error_Msg_Sloc := Sloc (P);

            if Nkind (P) = N_Aspect_Specification
              or else From_Aspect_Specification (P)
            then
               Error_Msg_NE ("aspect% for & previously given#", N, E);
            else
               Error_Msg_NE ("pragma% for & duplicates pragma#", N, E);
            end if;

            raise Pragma_Exit;
         end if;
      end Check_Duplicate_Pragma;

      ----------------------------------
      -- Check_Duplicated_Export_Name --
      ----------------------------------

      procedure Check_Duplicated_Export_Name (Nam : Node_Id) is
         String_Val : constant String_Id := Strval (Nam);

      begin
         --  We are only interested in the export case, and in the case of
         --  generics, it is the instance, not the template, that is the
         --  problem (the template will generate a warning in any case).

         if not Inside_A_Generic
           and then (Prag_Id = Pragma_Export
                       or else
                     Prag_Id = Pragma_Export_Procedure
                       or else
                     Prag_Id = Pragma_Export_Valued_Procedure
                       or else
                     Prag_Id = Pragma_Export_Function)
         then
            for J in Externals.First .. Externals.Last loop
               if String_Equal (String_Val, Strval (Externals.Table (J))) then
                  Error_Msg_Sloc := Sloc (Externals.Table (J));
                  Error_Msg_N ("external name duplicates name given#", Nam);
                  exit;
               end if;
            end loop;

            Externals.Append (Nam);
         end if;
      end Check_Duplicated_Export_Name;

      -------------------------------------
      -- Check_Expr_Is_Static_Expression --
      -------------------------------------

      procedure Check_Expr_Is_Static_Expression
        (Expr : Node_Id;
         Typ  : Entity_Id := Empty)
      is
      begin
         if Present (Typ) then
            Analyze_And_Resolve (Expr, Typ);
         else
            Analyze_And_Resolve (Expr);
         end if;

         if Is_OK_Static_Expression (Expr) then
            return;

         elsif Etype (Expr) = Any_Type then
            raise Pragma_Exit;

         --  An interesting special case, if we have a string literal and we
         --  are in Ada 83 mode, then we allow it even though it will not be
         --  flagged as static. This allows the use of Ada 95 pragmas like
         --  Import in Ada 83 mode. They will of course be flagged with
         --  warnings as usual, but will not cause errors.

         elsif Ada_Version = Ada_83
           and then Nkind (Expr) = N_String_Literal
         then
            return;

         --  Static expression that raises Constraint_Error. This has already
         --  been flagged, so just exit from pragma processing.

         elsif Is_Static_Expression (Expr) then
            raise Pragma_Exit;

         --  Finally, we have a real error

         else
            Error_Msg_Name_1 := Pname;

            declare
               Msg : String :=
                       "argument for pragma% must be a static expression!";
            begin
               Fix_Error (Msg);
               Flag_Non_Static_Expr (Msg, Expr);
            end;

            raise Pragma_Exit;
         end if;
      end Check_Expr_Is_Static_Expression;

      -------------------------
      -- Check_First_Subtype --
      -------------------------

      procedure Check_First_Subtype (Arg : Node_Id) is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);
         Ent  : constant Entity_Id := Entity (Argx);

      begin
         if Is_First_Subtype (Ent) then
            null;

         elsif Is_Type (Ent) then
            Error_Pragma_Arg
              ("pragma% cannot apply to subtype", Argx);

         elsif Is_Object (Ent) then
            Error_Pragma_Arg
              ("pragma% cannot apply to object, requires a type", Argx);

         else
            Error_Pragma_Arg
              ("pragma% cannot apply to&, requires a type", Argx);
         end if;
      end Check_First_Subtype;

      ----------------------
      -- Check_Identifier --
      ----------------------

      procedure Check_Identifier (Arg : Node_Id; Id : Name_Id) is
      begin
         if Present (Arg)
           and then Nkind (Arg) = N_Pragma_Argument_Association
         then
            if Chars (Arg) = No_Name or else Chars (Arg) /= Id then
               Error_Msg_Name_1 := Pname;
               Error_Msg_Name_2 := Id;
               Error_Msg_N ("pragma% argument expects identifier%", Arg);
               raise Pragma_Exit;
            end if;
         end if;
      end Check_Identifier;

      --------------------------------
      -- Check_Identifier_Is_One_Of --
      --------------------------------

      procedure Check_Identifier_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id) is
      begin
         if Present (Arg)
           and then Nkind (Arg) = N_Pragma_Argument_Association
         then
            if Chars (Arg) = No_Name then
               Error_Msg_Name_1 := Pname;
               Error_Msg_N ("pragma% argument expects an identifier", Arg);
               raise Pragma_Exit;

            elsif Chars (Arg) /= N1
              and then Chars (Arg) /= N2
            then
               Error_Msg_Name_1 := Pname;
               Error_Msg_N ("invalid identifier for pragma% argument", Arg);
               raise Pragma_Exit;
            end if;
         end if;
      end Check_Identifier_Is_One_Of;

      ---------------------------
      -- Check_In_Main_Program --
      ---------------------------

      procedure Check_In_Main_Program is
         P : constant Node_Id := Parent (N);

      begin
         --  Must be at in subprogram body

         if Nkind (P) /= N_Subprogram_Body then
            Error_Pragma ("% pragma allowed only in subprogram");

         --  Otherwise warn if obviously not main program

         elsif Present (Parameter_Specifications (Specification (P)))
           or else not Is_Compilation_Unit (Defining_Entity (P))
         then
            Error_Msg_Name_1 := Pname;
            Error_Msg_N
              ("?pragma% is only effective in main program", N);
         end if;
      end Check_In_Main_Program;

      ---------------------------------------
      -- Check_Interrupt_Or_Attach_Handler --
      ---------------------------------------

      procedure Check_Interrupt_Or_Attach_Handler is
         Arg1_X : constant Node_Id := Get_Pragma_Arg (Arg1);
         Handler_Proc, Proc_Scope : Entity_Id;

      begin
         Analyze (Arg1_X);

         if Prag_Id = Pragma_Interrupt_Handler then
            Check_Restriction (No_Dynamic_Attachment, N);
         end if;

         Handler_Proc := Find_Unique_Parameterless_Procedure (Arg1_X, Arg1);
         Proc_Scope := Scope (Handler_Proc);

         --  On AAMP only, a pragma Interrupt_Handler is supported for
         --  nonprotected parameterless procedures.

         if not AAMP_On_Target
           or else Prag_Id = Pragma_Attach_Handler
         then
            if Ekind (Proc_Scope) /= E_Protected_Type then
               Error_Pragma_Arg
                 ("argument of pragma% must be protected procedure", Arg1);
            end if;

            if Parent (N) /= Protected_Definition (Parent (Proc_Scope)) then
               Error_Pragma ("pragma% must be in protected definition");
            end if;
         end if;

         if not Is_Library_Level_Entity (Proc_Scope)
           or else (AAMP_On_Target
                     and then not Is_Library_Level_Entity (Handler_Proc))
         then
            Error_Pragma_Arg
              ("argument for pragma% must be library level entity", Arg1);
         end if;

         --  AI05-0033: A pragma cannot appear within a generic body, because
         --  instance can be in a nested scope. The check that protected type
         --  is itself a library-level declaration is done elsewhere.

         --  Note: we omit this check in Codepeer mode to properly handle code
         --  prior to AI-0033 (pragmas don't matter to codepeer in any case).

         if Inside_A_Generic then
            if Ekind (Scope (Current_Scope)) = E_Generic_Package
              and then In_Package_Body (Scope (Current_Scope))
              and then not CodePeer_Mode
            then
               Error_Pragma ("pragma% cannot be used inside a generic");
            end if;
         end if;
      end Check_Interrupt_Or_Attach_Handler;

      -------------------------------------------
      -- Check_Is_In_Decl_Part_Or_Package_Spec --
      -------------------------------------------

      procedure Check_Is_In_Decl_Part_Or_Package_Spec is
         P : Node_Id;

      begin
         P := Parent (N);
         loop
            if No (P) then
               exit;

            elsif Nkind (P) = N_Handled_Sequence_Of_Statements then
               exit;

            elsif Nkind_In (P, N_Package_Specification,
                               N_Block_Statement)
            then
               return;

            --  Note: the following tests seem a little peculiar, because
            --  they test for bodies, but if we were in the statement part
            --  of the body, we would already have hit the handled statement
            --  sequence, so the only way we get here is by being in the
            --  declarative part of the body.

            elsif Nkind_In (P, N_Subprogram_Body,
                               N_Package_Body,
                               N_Task_Body,
                               N_Entry_Body)
            then
               return;
            end if;

            P := Parent (P);
         end loop;

         Error_Pragma ("pragma% is not in declarative part or package spec");
      end Check_Is_In_Decl_Part_Or_Package_Spec;

      -------------------------
      -- Check_No_Identifier --
      -------------------------

      procedure Check_No_Identifier (Arg : Node_Id) is
      begin
         if Nkind (Arg) = N_Pragma_Argument_Association
           and then Chars (Arg) /= No_Name
         then
            Error_Pragma_Arg_Ident
              ("pragma% does not permit identifier& here", Arg);
         end if;
      end Check_No_Identifier;

      --------------------------
      -- Check_No_Identifiers --
      --------------------------

      procedure Check_No_Identifiers is
         Arg_Node : Node_Id;
      begin
         if Arg_Count > 0 then
            Arg_Node := Arg1;
            while Present (Arg_Node) loop
               Check_No_Identifier (Arg_Node);
               Next (Arg_Node);
            end loop;
         end if;
      end Check_No_Identifiers;

      ------------------------
      -- Check_No_Link_Name --
      ------------------------

      procedure Check_No_Link_Name is
      begin
         if Present (Arg3)
           and then Chars (Arg3) = Name_Link_Name
         then
            Arg4 := Arg3;
         end if;

         if Present (Arg4) then
            Error_Pragma_Arg
              ("Link_Name argument not allowed for Import Intrinsic", Arg4);
         end if;
      end Check_No_Link_Name;

      -------------------------------
      -- Check_Optional_Identifier --
      -------------------------------

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id) is
      begin
         if Present (Arg)
           and then Nkind (Arg) = N_Pragma_Argument_Association
           and then Chars (Arg) /= No_Name
         then
            if Chars (Arg) /= Id then
               Error_Msg_Name_1 := Pname;
               Error_Msg_Name_2 := Id;
               Error_Msg_N ("pragma% argument expects identifier%", Arg);
               raise Pragma_Exit;
            end if;
         end if;
      end Check_Optional_Identifier;

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : String) is
      begin
         Name_Buffer (1 .. Id'Length) := Id;
         Name_Len := Id'Length;
         Check_Optional_Identifier (Arg, Name_Find);
      end Check_Optional_Identifier;

      --------------------------------------
      -- Check_Precondition_Postcondition --
      --------------------------------------

      procedure Check_Precondition_Postcondition (In_Body : out Boolean) is
         P  : Node_Id;
         PO : Node_Id;

         procedure Chain_PPC (PO : Node_Id);
         --  If PO is an entry or a [generic] subprogram declaration node, then
         --  the precondition/postcondition applies to this subprogram and the
         --  processing for the pragma is completed. Otherwise the pragma is
         --  misplaced.

         ---------------
         -- Chain_PPC --
         ---------------

         procedure Chain_PPC (PO : Node_Id) is
            S   : Entity_Id;
            P   : Node_Id;

         begin
            if Nkind (PO) = N_Abstract_Subprogram_Declaration then
               if not From_Aspect_Specification (N) then
                  Error_Pragma
                    ("pragma% cannot be applied to abstract subprogram");

               elsif Class_Present (N) then
                  null;

               else
                  Error_Pragma
                    ("aspect % requires ''Class for abstract subprogram");
               end if;

            --  AI05-0230: The same restriction applies to null procedures. For
            --  compatibility with earlier uses of the Ada pragma, apply this
            --  rule only to aspect specifications.

            --  The above discrpency needs documentation. Robert is dubious
            --  about whether it is a good idea ???

            elsif Nkind (PO) = N_Subprogram_Declaration
              and then Nkind (Specification (PO)) = N_Procedure_Specification
              and then Null_Present (Specification (PO))
              and then From_Aspect_Specification (N)
              and then not Class_Present (N)
            then
               Error_Pragma
                 ("aspect % requires ''Class for null procedure");

            elsif not Nkind_In (PO, N_Subprogram_Declaration,
                                    N_Generic_Subprogram_Declaration,
                                    N_Entry_Declaration)
            then
               Pragma_Misplaced;
            end if;

            --  Here if we have [generic] subprogram or entry declaration

            if Nkind (PO) = N_Entry_Declaration then
               S := Defining_Entity (PO);
            else
               S := Defining_Unit_Name (Specification (PO));
            end if;

            --  Make sure we do not have the case of a precondition pragma when
            --  the Pre'Class aspect is present.

            --  We do this by looking at pragmas already chained to the entity
            --  since the aspect derived pragma will be put on this list first.

            if Pragma_Name (N) = Name_Precondition then
               if not From_Aspect_Specification (N) then
                  P := Spec_PPC_List (Contract (S));
                  while Present (P) loop
                     if Pragma_Name (P) = Name_Precondition
                       and then From_Aspect_Specification (P)
                       and then Class_Present (P)
                     then
                        Error_Msg_Sloc := Sloc (P);
                        Error_Pragma
                          ("pragma% not allowed, `Pre''Class` aspect given#");
                     end if;

                     P := Next_Pragma (P);
                  end loop;
               end if;
            end if;

            --  Similarly check for Pre with inherited Pre'Class. Note that
            --  we cover the aspect case as well here.

            if Pragma_Name (N) = Name_Precondition
              and then not Class_Present (N)
            then
               declare
                  Inherited : constant Subprogram_List :=
                                Inherited_Subprograms (S);
                  P         : Node_Id;

               begin
                  for J in Inherited'Range loop
                     P := Spec_PPC_List (Contract (Inherited (J)));
                     while Present (P) loop
                        if Pragma_Name (P) = Name_Precondition
                          and then Class_Present (P)
                        then
                           Error_Msg_Sloc := Sloc (P);
                           Error_Pragma
                             ("pragma% not allowed, `Pre''Class` "
                              & "aspect inherited from#");
                        end if;

                        P := Next_Pragma (P);
                     end loop;
                  end loop;
               end;
            end if;

            --  Note: we do not analyze the pragma at this point. Instead we
            --  delay this analysis until the end of the declarative part in
            --  which the pragma appears. This implements the required delay
            --  in this analysis, allowing forward references. The analysis
            --  happens at the end of Analyze_Declarations.

            --  Chain spec PPC pragma to list for subprogram

            Set_Next_Pragma (N, Spec_PPC_List (Contract (S)));
            Set_Spec_PPC_List (Contract (S), N);

            --  Return indicating spec case

            In_Body := False;
            return;
         end Chain_PPC;

      --  Start of processing for Check_Precondition_Postcondition

      begin
         if not Is_List_Member (N) then
            Pragma_Misplaced;
         end if;

         --  Preanalyze message argument if present. Visibility in this
         --  argument is established at the point of pragma occurrence.

         if Arg_Count = 2 then
            Check_Optional_Identifier (Arg2, Name_Message);
            Preanalyze_Spec_Expression
              (Get_Pragma_Arg (Arg2), Standard_String);
         end if;

         --  Record if pragma is disabled

         if Check_Enabled (Pname) then
            Set_SCO_Pragma_Enabled (Loc);
         end if;

         --  If we are within an inlined body, the legality of the pragma
         --  has been checked already.

         if In_Inlined_Body then
            In_Body := True;
            return;
         end if;

         --  Search prior declarations

         P := N;
         while Present (Prev (P)) loop
            P := Prev (P);

            --  If the previous node is a generic subprogram, do not go to to
            --  the original node, which is the unanalyzed tree: we need to
            --  attach the pre/postconditions to the analyzed version at this
            --  point. They get propagated to the original tree when analyzing
            --  the corresponding body.

            if Nkind (P) not in N_Generic_Declaration then
               PO := Original_Node (P);
            else
               PO := P;
            end if;

            --  Skip past prior pragma

            if Nkind (PO) = N_Pragma then
               null;

            --  Skip stuff not coming from source

            elsif not Comes_From_Source (PO) then

               --  The condition may apply to a subprogram instantiation

               if Nkind (PO) = N_Subprogram_Declaration
                 and then Present (Generic_Parent (Specification (PO)))
               then
                  Chain_PPC (PO);
                  return;

               elsif Nkind (PO) = N_Subprogram_Declaration
                 and then In_Instance
               then
                  Chain_PPC (PO);
                  return;

               --  For all other cases of non source code, do nothing

               else
                  null;
               end if;

            --  Only remaining possibility is subprogram declaration

            else
               Chain_PPC (PO);
               return;
            end if;
         end loop;

         --  If we fall through loop, pragma is at start of list, so see if it
         --  is at the start of declarations of a subprogram body.

         if Nkind (Parent (N)) = N_Subprogram_Body
           and then List_Containing (N) = Declarations (Parent (N))
         then
            if Operating_Mode /= Generate_Code
              or else Inside_A_Generic
            then
               --  Analyze pragma expression for correctness and for ASIS use

               Preanalyze_Spec_Expression
                 (Get_Pragma_Arg (Arg1), Standard_Boolean);

               --  In ASIS mode, for a pragma generated from a source aspect,
               --  also analyze the original aspect expression.

               if ASIS_Mode
                 and then Present (Corresponding_Aspect (N))
               then
                  Preanalyze_Spec_Expression
                    (Expression (Corresponding_Aspect (N)), Standard_Boolean);
               end if;
            end if;

            In_Body := True;
            return;

         --  See if it is in the pragmas after a library level subprogram

         elsif Nkind (Parent (N)) = N_Compilation_Unit_Aux then

            --  In formal verification mode, analyze pragma expression for
            --  correctness, as it is not expanded later.

            if Alfa_Mode then
               Analyze_PPC_In_Decl_Part
                 (N, Defining_Entity (Unit (Parent (Parent (N)))));
            end if;

            Chain_PPC (Unit (Parent (Parent (N))));
            return;
         end if;

         --  If we fall through, pragma was misplaced

         Pragma_Misplaced;
      end Check_Precondition_Postcondition;

      -----------------------------
      -- Check_Static_Constraint --
      -----------------------------

      --  Note: for convenience in writing this procedure, in addition to
      --  the officially (i.e. by spec) allowed argument which is always a
      --  constraint, it also allows ranges and discriminant associations.
      --  Above is not clear ???

      procedure Check_Static_Constraint (Constr : Node_Id) is

         procedure Require_Static (E : Node_Id);
         --  Require given expression to be static expression

         --------------------
         -- Require_Static --
         --------------------

         procedure Require_Static (E : Node_Id) is
         begin
            if not Is_OK_Static_Expression (E) then
               Flag_Non_Static_Expr
                 ("non-static constraint not allowed in Unchecked_Union!", E);
               raise Pragma_Exit;
            end if;
         end Require_Static;

      --  Start of processing for Check_Static_Constraint

      begin
         case Nkind (Constr) is
            when N_Discriminant_Association =>
               Require_Static (Expression (Constr));

            when N_Range =>
               Require_Static (Low_Bound (Constr));
               Require_Static (High_Bound (Constr));

            when N_Attribute_Reference =>
               Require_Static (Type_Low_Bound  (Etype (Prefix (Constr))));
               Require_Static (Type_High_Bound (Etype (Prefix (Constr))));

            when N_Range_Constraint =>
               Check_Static_Constraint (Range_Expression (Constr));

            when N_Index_Or_Discriminant_Constraint =>
               declare
                  IDC : Entity_Id;
               begin
                  IDC := First (Constraints (Constr));
                  while Present (IDC) loop
                     Check_Static_Constraint (IDC);
                     Next (IDC);
                  end loop;
               end;

            when others =>
               null;
         end case;
      end Check_Static_Constraint;

      ---------------------
      -- Check_Test_Case --
      ---------------------

      procedure Check_Test_Case is
         P  : Node_Id;
         PO : Node_Id;

         procedure Chain_TC (PO : Node_Id);
         --  If PO is a [generic] subprogram declaration node, then the
         --  test-case applies to this subprogram and the processing for the
         --  pragma is completed. Otherwise the pragma is misplaced.

         --------------
         -- Chain_TC --
         --------------

         procedure Chain_TC (PO : Node_Id) is
            S   : Entity_Id;

         begin
            if Nkind (PO) = N_Abstract_Subprogram_Declaration then
               if From_Aspect_Specification (N) then
                  Error_Pragma
                    ("aspect% cannot be applied to abstract subprogram");
               else
                  Error_Pragma
                    ("pragma% cannot be applied to abstract subprogram");
               end if;

            elsif Nkind (PO) = N_Entry_Declaration then
               if From_Aspect_Specification (N) then
                  Error_Pragma ("aspect% cannot be applied to entry");
               else
                  Error_Pragma ("pragma% cannot be applied to entry");
               end if;

            elsif not Nkind_In (PO, N_Subprogram_Declaration,
                                    N_Generic_Subprogram_Declaration)
            then
               Pragma_Misplaced;
            end if;

            --  Here if we have [generic] subprogram declaration

            S := Defining_Unit_Name (Specification (PO));

            --  Note: we do not analyze the pragma at this point. Instead we
            --  delay this analysis until the end of the declarative part in
            --  which the pragma appears. This implements the required delay
            --  in this analysis, allowing forward references. The analysis
            --  happens at the end of Analyze_Declarations.

            --  There should not be another test case with the same name
            --  associated to this subprogram.

            declare
               Name : constant String_Id := Get_Name_From_Test_Case_Pragma (N);
               TC   : Node_Id;

            begin
               TC := Spec_TC_List (Contract (S));
               while Present (TC) loop

                  if String_Equal
                    (Name, Get_Name_From_Test_Case_Pragma (TC))
                  then
                     Error_Msg_Sloc := Sloc (TC);

                     if From_Aspect_Specification (N) then
                        Error_Pragma ("name for aspect% is already used#");
                     else
                        Error_Pragma ("name for pragma% is already used#");
                     end if;
                  end if;

                  TC := Next_Pragma (TC);
               end loop;
            end;

            --  Chain spec TC pragma to list for subprogram

            Set_Next_Pragma (N, Spec_TC_List (Contract (S)));
            Set_Spec_TC_List (Contract (S), N);
         end Chain_TC;

      --  Start of processing for Check_Test_Case

      begin
         if not Is_List_Member (N) then
            Pragma_Misplaced;
         end if;

         --  Test cases should only appear in package spec unit

         if Get_Source_Unit (N) = No_Unit
           or else not Nkind_In (Sinfo.Unit (Cunit (Get_Source_Unit (N))),
                                 N_Package_Declaration,
                                 N_Generic_Package_Declaration)
         then
            Pragma_Misplaced;
         end if;

         --  Search prior declarations

         P := N;
         while Present (Prev (P)) loop
            P := Prev (P);

            --  If the previous node is a generic subprogram, do not go to to
            --  the original node, which is the unanalyzed tree: we need to
            --  attach the test-case to the analyzed version at this point.
            --  They get propagated to the original tree when analyzing the
            --  corresponding body.

            if Nkind (P) not in N_Generic_Declaration then
               PO := Original_Node (P);
            else
               PO := P;
            end if;

            --  Skip past prior pragma

            if Nkind (PO) = N_Pragma then
               null;

            --  Skip stuff not coming from source

            elsif not Comes_From_Source (PO) then
               null;

            --  Only remaining possibility is subprogram declaration. First
            --  check that it is declared directly in a package declaration.
            --  This may be either the package declaration for the current unit
            --  being defined or a local package declaration.

            elsif not Present (Parent (Parent (PO)))
              or else not Present (Parent (Parent (Parent (PO))))
              or else not Nkind_In (Parent (Parent (PO)),
                                    N_Package_Declaration,
                                    N_Generic_Package_Declaration)
            then
               Pragma_Misplaced;

            else
               Chain_TC (PO);
               return;
            end if;
         end loop;

         --  If we fall through, pragma was misplaced

         Pragma_Misplaced;
      end Check_Test_Case;

      --------------------------------------
      -- Check_Valid_Configuration_Pragma --
      --------------------------------------

      --  A configuration pragma must appear in the context clause of a
      --  compilation unit, and only other pragmas may precede it. Note that
      --  the test also allows use in a configuration pragma file.

      procedure Check_Valid_Configuration_Pragma is
      begin
         if not Is_Configuration_Pragma then
            Error_Pragma ("incorrect placement for configuration pragma%");
         end if;
      end Check_Valid_Configuration_Pragma;

      -------------------------------------
      -- Check_Valid_Library_Unit_Pragma --
      -------------------------------------

      procedure Check_Valid_Library_Unit_Pragma is
         Plist       : List_Id;
         Parent_Node : Node_Id;
         Unit_Name   : Entity_Id;
         Unit_Kind   : Node_Kind;
         Unit_Node   : Node_Id;
         Sindex      : Source_File_Index;

      begin
         if not Is_List_Member (N) then
            Pragma_Misplaced;

         else
            Plist := List_Containing (N);
            Parent_Node := Parent (Plist);

            if Parent_Node = Empty then
               Pragma_Misplaced;

            --  Case of pragma appearing after a compilation unit. In this case
            --  it must have an argument with the corresponding name and must
            --  be part of the following pragmas of its parent.

            elsif Nkind (Parent_Node) = N_Compilation_Unit_Aux then
               if Plist /= Pragmas_After (Parent_Node) then
                  Pragma_Misplaced;

               elsif Arg_Count = 0 then
                  Error_Pragma
                    ("argument required if outside compilation unit");

               else
                  Check_No_Identifiers;
                  Check_Arg_Count (1);
                  Unit_Node := Unit (Parent (Parent_Node));
                  Unit_Kind := Nkind (Unit_Node);

                  Analyze (Get_Pragma_Arg (Arg1));

                  if Unit_Kind = N_Generic_Subprogram_Declaration
                    or else Unit_Kind = N_Subprogram_Declaration
                  then
                     Unit_Name := Defining_Entity (Unit_Node);

                  elsif Unit_Kind in N_Generic_Instantiation then
                     Unit_Name := Defining_Entity (Unit_Node);

                  else
                     Unit_Name := Cunit_Entity (Current_Sem_Unit);
                  end if;

                  if Chars (Unit_Name) /=
                     Chars (Entity (Get_Pragma_Arg (Arg1)))
                  then
                     Error_Pragma_Arg
                       ("pragma% argument is not current unit name", Arg1);
                  end if;

                  if Ekind (Unit_Name) = E_Package
                    and then Present (Renamed_Entity (Unit_Name))
                  then
                     Error_Pragma ("pragma% not allowed for renamed package");
                  end if;
               end if;

            --  Pragma appears other than after a compilation unit

            else
               --  Here we check for the generic instantiation case and also
               --  for the case of processing a generic formal package. We
               --  detect these cases by noting that the Sloc on the node
               --  does not belong to the current compilation unit.

               Sindex := Source_Index (Current_Sem_Unit);

               if Loc not in Source_First (Sindex) .. Source_Last (Sindex) then
                  Rewrite (N, Make_Null_Statement (Loc));
                  return;

               --  If before first declaration, the pragma applies to the
               --  enclosing unit, and the name if present must be this name.

               elsif Is_Before_First_Decl (N, Plist) then
                  Unit_Node := Unit_Declaration_Node (Current_Scope);
                  Unit_Kind := Nkind (Unit_Node);

                  if Nkind (Parent (Unit_Node)) /= N_Compilation_Unit then
                     Pragma_Misplaced;

                  elsif Unit_Kind = N_Subprogram_Body
                    and then not Acts_As_Spec (Unit_Node)
                  then
                     Pragma_Misplaced;

                  elsif Nkind (Parent_Node) = N_Package_Body then
                     Pragma_Misplaced;

                  elsif Nkind (Parent_Node) = N_Package_Specification
                    and then Plist = Private_Declarations (Parent_Node)
                  then
                     Pragma_Misplaced;

                  elsif (Nkind (Parent_Node) = N_Generic_Package_Declaration
                           or else Nkind (Parent_Node) =
                                             N_Generic_Subprogram_Declaration)
                    and then Plist = Generic_Formal_Declarations (Parent_Node)
                  then
                     Pragma_Misplaced;

                  elsif Arg_Count > 0 then
                     Analyze (Get_Pragma_Arg (Arg1));

                     if Entity (Get_Pragma_Arg (Arg1)) /= Current_Scope then
                        Error_Pragma_Arg
                          ("name in pragma% must be enclosing unit", Arg1);
                     end if;

                  --  It is legal to have no argument in this context

                  else
                     return;
                  end if;

               --  Error if not before first declaration. This is because a
               --  library unit pragma argument must be the name of a library
               --  unit (RM 10.1.5(7)), but the only names permitted in this
               --  context are (RM 10.1.5(6)) names of subprogram declarations,
               --  generic subprogram declarations or generic instantiations.

               else
                  Error_Pragma
                    ("pragma% misplaced, must be before first declaration");
               end if;
            end if;
         end if;
      end Check_Valid_Library_Unit_Pragma;

      -------------------
      -- Check_Variant --
      -------------------

      procedure Check_Variant (Variant : Node_Id; UU_Typ : Entity_Id) is
         Clist : constant Node_Id := Component_List (Variant);
         Comp  : Node_Id;

      begin
         if not Is_Non_Empty_List (Component_Items (Clist)) then
            Error_Msg_N
              ("Unchecked_Union may not have empty component list",
               Variant);
            return;
         end if;

         Comp := First (Component_Items (Clist));
         while Present (Comp) loop
            Check_Component (Comp, UU_Typ, In_Variant_Part => True);
            Next (Comp);
         end loop;
      end Check_Variant;

      ------------------
      -- Error_Pragma --
      ------------------

      procedure Error_Pragma (Msg : String) is
         MsgF : String := Msg;
      begin
         Error_Msg_Name_1 := Pname;
         Fix_Error (MsgF);
         Error_Msg_N (MsgF, N);
         raise Pragma_Exit;
      end Error_Pragma;

      ----------------------
      -- Error_Pragma_Arg --
      ----------------------

      procedure Error_Pragma_Arg (Msg : String; Arg : Node_Id) is
         MsgF : String := Msg;
      begin
         Error_Msg_Name_1 := Pname;
         Fix_Error (MsgF);
         Error_Msg_N (MsgF, Get_Pragma_Arg (Arg));
         raise Pragma_Exit;
      end Error_Pragma_Arg;

      procedure Error_Pragma_Arg (Msg1, Msg2 : String; Arg : Node_Id) is
         MsgF : String := Msg1;
      begin
         Error_Msg_Name_1 := Pname;
         Fix_Error (MsgF);
         Error_Msg_N (MsgF, Get_Pragma_Arg (Arg));
         Error_Pragma_Arg (Msg2, Arg);
      end Error_Pragma_Arg;

      ----------------------------
      -- Error_Pragma_Arg_Ident --
      ----------------------------

      procedure Error_Pragma_Arg_Ident (Msg : String; Arg : Node_Id) is
         MsgF : String := Msg;
      begin
         Error_Msg_Name_1 := Pname;
         Fix_Error (MsgF);
         Error_Msg_N (MsgF, Arg);
         raise Pragma_Exit;
      end Error_Pragma_Arg_Ident;

      ----------------------
      -- Error_Pragma_Ref --
      ----------------------

      procedure Error_Pragma_Ref (Msg : String; Ref : Entity_Id) is
         MsgF : String := Msg;
      begin
         Error_Msg_Name_1 := Pname;
         Fix_Error (MsgF);
         Error_Msg_Sloc   := Sloc (Ref);
         Error_Msg_NE (MsgF, N, Ref);
         raise Pragma_Exit;
      end Error_Pragma_Ref;

      ------------------------
      -- Find_Lib_Unit_Name --
      ------------------------

      function Find_Lib_Unit_Name return Entity_Id is
      begin
         --  Return inner compilation unit entity, for case of nested
         --  categorization pragmas. This happens in generic unit.

         if Nkind (Parent (N)) = N_Package_Specification
           and then Defining_Entity (Parent (N)) /= Current_Scope
         then
            return Defining_Entity (Parent (N));
         else
            return Current_Scope;
         end if;
      end Find_Lib_Unit_Name;

      ----------------------------
      -- Find_Program_Unit_Name --
      ----------------------------

      procedure Find_Program_Unit_Name (Id : Node_Id) is
         Unit_Name : Entity_Id;
         Unit_Kind : Node_Kind;
         P         : constant Node_Id := Parent (N);

      begin
         if Nkind (P) = N_Compilation_Unit then
            Unit_Kind := Nkind (Unit (P));

            if Unit_Kind = N_Subprogram_Declaration
              or else Unit_Kind = N_Package_Declaration
              or else Unit_Kind in N_Generic_Declaration
            then
               Unit_Name := Defining_Entity (Unit (P));

               if Chars (Id) = Chars (Unit_Name) then
                  Set_Entity (Id, Unit_Name);
                  Set_Etype (Id, Etype (Unit_Name));
               else
                  Set_Etype (Id, Any_Type);
                  Error_Pragma
                    ("cannot find program unit referenced by pragma%");
               end if;

            else
               Set_Etype (Id, Any_Type);
               Error_Pragma ("pragma% inapplicable to this unit");
            end if;

         else
            Analyze (Id);
         end if;
      end Find_Program_Unit_Name;

      -----------------------------------------
      -- Find_Unique_Parameterless_Procedure --
      -----------------------------------------

      function Find_Unique_Parameterless_Procedure
        (Name : Entity_Id;
         Arg  : Node_Id) return Entity_Id
      is
         Proc : Entity_Id := Empty;

      begin
         --  The body of this procedure needs some comments ???

         if not Is_Entity_Name (Name) then
            Error_Pragma_Arg
              ("argument of pragma% must be entity name", Arg);

         elsif not Is_Overloaded (Name) then
            Proc := Entity (Name);

            if Ekind (Proc) /= E_Procedure
              or else Present (First_Formal (Proc))
            then
               Error_Pragma_Arg
                 ("argument of pragma% must be parameterless procedure", Arg);
            end if;

         else
            declare
               Found : Boolean := False;
               It    : Interp;
               Index : Interp_Index;

            begin
               Get_First_Interp (Name, Index, It);
               while Present (It.Nam) loop
                  Proc := It.Nam;

                  if Ekind (Proc) = E_Procedure
                    and then No (First_Formal (Proc))
                  then
                     if not Found then
                        Found := True;
                        Set_Entity (Name, Proc);
                        Set_Is_Overloaded (Name, False);
                     else
                        Error_Pragma_Arg
                          ("ambiguous handler name for pragma% ", Arg);
                     end if;
                  end if;

                  Get_Next_Interp (Index, It);
               end loop;

               if not Found then
                  Error_Pragma_Arg
                    ("argument of pragma% must be parameterless procedure",
                     Arg);
               else
                  Proc := Entity (Name);
               end if;
            end;
         end if;

         return Proc;
      end Find_Unique_Parameterless_Procedure;

      ---------------
      -- Fix_Error --
      ---------------

      procedure Fix_Error (Msg : in out String) is
      begin
         if From_Aspect_Specification (N) then
            for J in Msg'First .. Msg'Last - 5 loop
               if Msg (J .. J + 5) = "pragma" then
                  Msg (J .. J + 5) := "aspect";
               end if;
            end loop;

            if Error_Msg_Name_1 = Name_Precondition then
               Error_Msg_Name_1 := Name_Pre;
            elsif Error_Msg_Name_1 = Name_Postcondition then
               Error_Msg_Name_1 := Name_Post;
            end if;
         end if;
      end Fix_Error;

      -------------------------
      -- Gather_Associations --
      -------------------------

      procedure Gather_Associations
        (Names : Name_List;
         Args  : out Args_List)
      is
         Arg : Node_Id;

      begin
         --  Initialize all parameters to Empty

         for J in Args'Range loop
            Args (J) := Empty;
         end loop;

         --  That's all we have to do if there are no argument associations

         if No (Pragma_Argument_Associations (N)) then
            return;
         end if;

         --  Otherwise first deal with any positional parameters present

         Arg := First (Pragma_Argument_Associations (N));
         for Index in Args'Range loop
            exit when No (Arg) or else Chars (Arg) /= No_Name;
            Args (Index) := Get_Pragma_Arg (Arg);
            Next (Arg);
         end loop;

         --  Positional parameters all processed, if any left, then we
         --  have too many positional parameters.

         if Present (Arg) and then Chars (Arg) = No_Name then
            Error_Pragma_Arg
              ("too many positional associations for pragma%", Arg);
         end if;

         --  Process named parameters if any are present

         while Present (Arg) loop
            if Chars (Arg) = No_Name then
               Error_Pragma_Arg
                 ("positional association cannot follow named association",
                  Arg);

            else
               for Index in Names'Range loop
                  if Names (Index) = Chars (Arg) then
                     if Present (Args (Index)) then
                        Error_Pragma_Arg
                          ("duplicate argument association for pragma%", Arg);
                     else
                        Args (Index) := Get_Pragma_Arg (Arg);
                        exit;
                     end if;
                  end if;

                  if Index = Names'Last then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N ("pragma% does not allow & argument", Arg);

                     --  Check for possible misspelling

                     for Index1 in Names'Range loop
                        if Is_Bad_Spelling_Of
                             (Chars (Arg), Names (Index1))
                        then
                           Error_Msg_Name_1 := Names (Index1);
                           Error_Msg_N -- CODEFIX
                             ("\possible misspelling of%", Arg);
                           exit;
                        end if;
                     end loop;

                     raise Pragma_Exit;
                  end if;
               end loop;
            end if;

            Next (Arg);
         end loop;
      end Gather_Associations;

      -----------------
      -- GNAT_Pragma --
      -----------------

      procedure GNAT_Pragma is
      begin
         --  We need to check the No_Implementation_Pragmas restriction for
         --  the case of a pragma from source. Note that the case of aspects
         --  generating corresponding pragmas marks these pragmas as not being
         --  from source, so this test also catches that case.

         if Comes_From_Source (N) then
            Check_Restriction (No_Implementation_Pragmas, N);
         end if;
      end GNAT_Pragma;

      --------------------------
      -- Is_Before_First_Decl --
      --------------------------

      function Is_Before_First_Decl
        (Pragma_Node : Node_Id;
         Decls       : List_Id) return Boolean
      is
         Item : Node_Id := First (Decls);

      begin
         --  Only other pragmas can come before this pragma

         loop
            if No (Item) or else Nkind (Item) /= N_Pragma then
               return False;

            elsif Item = Pragma_Node then
               return True;
            end if;

            Next (Item);
         end loop;
      end Is_Before_First_Decl;

      -----------------------------
      -- Is_Configuration_Pragma --
      -----------------------------

      --  A configuration pragma must appear in the context clause of a
      --  compilation unit, and only other pragmas may precede it. Note that
      --  the test below also permits use in a configuration pragma file.

      function Is_Configuration_Pragma return Boolean is
         Lis : constant List_Id := List_Containing (N);
         Par : constant Node_Id := Parent (N);
         Prg : Node_Id;

      begin
         --  If no parent, then we are in the configuration pragma file,
         --  so the placement is definitely appropriate.

         if No (Par) then
            return True;

         --  Otherwise we must be in the context clause of a compilation unit
         --  and the only thing allowed before us in the context list is more
         --  configuration pragmas.

         elsif Nkind (Par) = N_Compilation_Unit
           and then Context_Items (Par) = Lis
         then
            Prg := First (Lis);

            loop
               if Prg = N then
                  return True;
               elsif Nkind (Prg) /= N_Pragma then
                  return False;
               end if;

               Next (Prg);
            end loop;

         else
            return False;
         end if;
      end Is_Configuration_Pragma;

      --------------------------
      -- Is_In_Context_Clause --
      --------------------------

      function Is_In_Context_Clause return Boolean is
         Plist       : List_Id;
         Parent_Node : Node_Id;

      begin
         if not Is_List_Member (N) then
            return False;

         else
            Plist := List_Containing (N);
            Parent_Node := Parent (Plist);

            if Parent_Node = Empty
              or else Nkind (Parent_Node) /= N_Compilation_Unit
              or else Context_Items (Parent_Node) /= Plist
            then
               return False;
            end if;
         end if;

         return True;
      end Is_In_Context_Clause;

      ---------------------------------
      -- Is_Static_String_Expression --
      ---------------------------------

      function Is_Static_String_Expression (Arg : Node_Id) return Boolean is
         Argx : constant Node_Id := Get_Pragma_Arg (Arg);

      begin
         Analyze_And_Resolve (Argx);
         return Is_OK_Static_Expression (Argx)
           and then Nkind (Argx) = N_String_Literal;
      end Is_Static_String_Expression;

      ----------------------
      -- Pragma_Misplaced --
      ----------------------

      procedure Pragma_Misplaced is
      begin
         Error_Pragma ("incorrect placement of pragma%");
      end Pragma_Misplaced;

      ------------------------------------
      -- Process Atomic_Shared_Volatile --
      ------------------------------------

      procedure Process_Atomic_Shared_Volatile is
         E_Id : Node_Id;
         E    : Entity_Id;
         D    : Node_Id;
         K    : Node_Kind;
         Utyp : Entity_Id;

         procedure Set_Atomic (E : Entity_Id);
         --  Set given type as atomic, and if no explicit alignment was given,
         --  set alignment to unknown, since back end knows what the alignment
         --  requirements are for atomic arrays. Note: this step is necessary
         --  for derived types.

         ----------------
         -- Set_Atomic --
         ----------------

         procedure Set_Atomic (E : Entity_Id) is
         begin
            Set_Is_Atomic (E);

            if not Has_Alignment_Clause (E) then
               Set_Alignment (E, Uint_0);
            end if;
         end Set_Atomic;

      --  Start of processing for Process_Atomic_Shared_Volatile

      begin
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Arg_Is_Local_Name (Arg1);
         E_Id := Get_Pragma_Arg (Arg1);

         if Etype (E_Id) = Any_Type then
            return;
         end if;

         E := Entity (E_Id);
         D := Declaration_Node (E);
         K := Nkind (D);

         --  Check duplicate before we chain ourselves!

         Check_Duplicate_Pragma (E);

         --  Now check appropriateness of the entity

         if Is_Type (E) then
            if Rep_Item_Too_Early (E, N)
                 or else
               Rep_Item_Too_Late (E, N)
            then
               return;
            else
               Check_First_Subtype (Arg1);
            end if;

            if Prag_Id /= Pragma_Volatile then
               Set_Atomic (E);
               Set_Atomic (Underlying_Type (E));
               Set_Atomic (Base_Type (E));
            end if;

            --  Attribute belongs on the base type. If the view of the type is
            --  currently private, it also belongs on the underlying type.

            Set_Is_Volatile (Base_Type (E));
            Set_Is_Volatile (Underlying_Type (E));

            Set_Treat_As_Volatile (E);
            Set_Treat_As_Volatile (Underlying_Type (E));

         elsif K = N_Object_Declaration
           or else (K = N_Component_Declaration
                     and then Original_Record_Component (E) = E)
         then
            if Rep_Item_Too_Late (E, N) then
               return;
            end if;

            if Prag_Id /= Pragma_Volatile then
               Set_Is_Atomic (E);

               --  If the object declaration has an explicit initialization, a
               --  temporary may have to be created to hold the expression, to
               --  ensure that access to the object remain atomic.

               if Nkind (Parent (E)) = N_Object_Declaration
                 and then Present (Expression (Parent (E)))
               then
                  Set_Has_Delayed_Freeze (E);
               end if;

               --  An interesting improvement here. If an object of type X is
               --  declared atomic, and the type X is not atomic, that's a
               --  pity, since it may not have appropriate alignment etc. We
               --  can rescue this in the special case where the object and
               --  type are in the same unit by just setting the type as
               --  atomic, so that the back end will process it as atomic.

               Utyp := Underlying_Type (Etype (E));

               if Present (Utyp)
                 and then Sloc (E) > No_Location
                 and then Sloc (Utyp) > No_Location
                 and then
                   Get_Source_File_Index (Sloc (E)) =
                   Get_Source_File_Index (Sloc (Underlying_Type (Etype (E))))
               then
                  Set_Is_Atomic (Underlying_Type (Etype (E)));
               end if;
            end if;

            Set_Is_Volatile (E);
            Set_Treat_As_Volatile (E);

         else
            Error_Pragma_Arg
              ("inappropriate entity for pragma%", Arg1);
         end if;
      end Process_Atomic_Shared_Volatile;

      -------------------------------------------
      -- Process_Compile_Time_Warning_Or_Error --
      -------------------------------------------

      procedure Process_Compile_Time_Warning_Or_Error is
         Arg1x : constant Node_Id := Get_Pragma_Arg (Arg1);

      begin
         Check_Arg_Count (2);
         Check_No_Identifiers;
         Check_Arg_Is_Static_Expression (Arg2, Standard_String);
         Analyze_And_Resolve (Arg1x, Standard_Boolean);

         if Compile_Time_Known_Value (Arg1x) then
            if Is_True (Expr_Value (Get_Pragma_Arg (Arg1))) then
               declare
                  Str   : constant String_Id :=
                            Strval (Get_Pragma_Arg (Arg2));
                  Len   : constant Int := String_Length (Str);
                  Cont  : Boolean;
                  Ptr   : Nat;
                  CC    : Char_Code;
                  C     : Character;
                  Cent  : constant Entity_Id :=
                            Cunit_Entity (Current_Sem_Unit);

                  Force : constant Boolean :=
                            Prag_Id = Pragma_Compile_Time_Warning
                              and then
                                Is_Spec_Name (Unit_Name (Current_Sem_Unit))
                              and then (Ekind (Cent) /= E_Package
                                          or else not In_Private_Part (Cent));
                  --  Set True if this is the warning case, and we are in the
                  --  visible part of a package spec, or in a subprogram spec,
                  --  in which case we want to force the client to see the
                  --  warning, even though it is not in the main unit.

               begin
                  --  Loop through segments of message separated by line feeds.
                  --  We output these segments as separate messages with
                  --  continuation marks for all but the first.

                  Cont := False;
                  Ptr := 1;
                  loop
                     Error_Msg_Strlen := 0;

                     --  Loop to copy characters from argument to error message
                     --  string buffer.

                     loop
                        exit when Ptr > Len;
                        CC := Get_String_Char (Str, Ptr);
                        Ptr := Ptr + 1;

                        --  Ignore wide chars ??? else store character

                        if In_Character_Range (CC) then
                           C := Get_Character (CC);
                           exit when C = ASCII.LF;
                           Error_Msg_Strlen := Error_Msg_Strlen + 1;
                           Error_Msg_String (Error_Msg_Strlen) := C;
                        end if;
                     end loop;

                     --  Here with one line ready to go

                     Error_Msg_Warn := Prag_Id = Pragma_Compile_Time_Warning;

                     --  If this is a warning in a spec, then we want clients
                     --  to see the warning, so mark the message with the
                     --  special sequence !! to force the warning. In the case
                     --  of a package spec, we do not force this if we are in
                     --  the private part of the spec.

                     if Force then
                        if Cont = False then
                           Error_Msg_N ("<~!!", Arg1);
                           Cont := True;
                        else
                           Error_Msg_N ("\<~!!", Arg1);
                        end if;

                     --  Error, rather than warning, or in a body, so we do not
                     --  need to force visibility for client (error will be
                     --  output in any case, and this is the situation in which
                     --  we do not want a client to get a warning, since the
                     --  warning is in the body or the spec private part).

                     else
                        if Cont = False then
                           Error_Msg_N ("<~", Arg1);
                           Cont := True;
                        else
                           Error_Msg_N ("\<~", Arg1);
                        end if;
                     end if;

                     exit when Ptr > Len;
                  end loop;
               end;
            end if;
         end if;
      end Process_Compile_Time_Warning_Or_Error;

      ------------------------
      -- Process_Convention --
      ------------------------

      procedure Process_Convention
        (C   : out Convention_Id;
         Ent : out Entity_Id)
      is
         Id        : Node_Id;
         E         : Entity_Id;
         E1        : Entity_Id;
         Cname     : Name_Id;
         Comp_Unit : Unit_Number_Type;

         procedure Diagnose_Multiple_Pragmas (S : Entity_Id);
         --  Called if we have more than one Export/Import/Convention pragma.
         --  This is generally illegal, but we have a special case of allowing
         --  Import and Interface to coexist if they specify the convention in
         --  a consistent manner. We are allowed to do this, since Interface is
         --  an implementation defined pragma, and we choose to do it since we
         --  know Rational allows this combination. S is the entity id of the
         --  subprogram in question. This procedure also sets the special flag
         --  Import_Interface_Present in both pragmas in the case where we do
         --  have matching Import and Interface pragmas.

         procedure Set_Convention_From_Pragma (E : Entity_Id);
         --  Set convention in entity E, and also flag that the entity has a
         --  convention pragma. If entity is for a private or incomplete type,
         --  also set convention and flag on underlying type. This procedure
         --  also deals with the special case of C_Pass_By_Copy convention.

         -------------------------------
         -- Diagnose_Multiple_Pragmas --
         -------------------------------

         procedure Diagnose_Multiple_Pragmas (S : Entity_Id) is
            Pdec : constant Node_Id := Declaration_Node (S);
            Decl : Node_Id;
            Err  : Boolean;

            function Same_Convention (Decl : Node_Id) return Boolean;
            --  Decl is a pragma node. This function returns True if this
            --  pragma has a first argument that is an identifier with a
            --  Chars field corresponding to the Convention_Id C.

            function Same_Name (Decl : Node_Id) return Boolean;
            --  Decl is a pragma node. This function returns True if this
            --  pragma has a second argument that is an identifier with a
            --  Chars field that matches the Chars of the current subprogram.

            ---------------------
            -- Same_Convention --
            ---------------------

            function Same_Convention (Decl : Node_Id) return Boolean is
               Arg1 : constant Node_Id :=
                        First (Pragma_Argument_Associations (Decl));

            begin
               if Present (Arg1) then
                  declare
                     Arg : constant Node_Id := Get_Pragma_Arg (Arg1);
                  begin
                     if Nkind (Arg) = N_Identifier
                       and then Is_Convention_Name (Chars (Arg))
                       and then Get_Convention_Id (Chars (Arg)) = C
                     then
                        return True;
                     end if;
                  end;
               end if;

               return False;
            end Same_Convention;

            ---------------
            -- Same_Name --
            ---------------

            function Same_Name (Decl : Node_Id) return Boolean is
               Arg1 : constant Node_Id :=
                        First (Pragma_Argument_Associations (Decl));
               Arg2 : Node_Id;

            begin
               if No (Arg1) then
                  return False;
               end if;

               Arg2 := Next (Arg1);

               if No (Arg2) then
                  return False;
               end if;

               declare
                  Arg : constant Node_Id := Get_Pragma_Arg (Arg2);
               begin
                  if Nkind (Arg) = N_Identifier
                    and then Chars (Arg) = Chars (S)
                  then
                     return True;
                  end if;
               end;

               return False;
            end Same_Name;

         --  Start of processing for Diagnose_Multiple_Pragmas

         begin
            Err := True;

            --  Definitely give message if we have Convention/Export here

            if Prag_Id = Pragma_Convention or else Prag_Id = Pragma_Export then
               null;

               --  If we have an Import or Export, scan back from pragma to
               --  find any previous pragma applying to the same procedure.
               --  The scan will be terminated by the start of the list, or
               --  hitting the subprogram declaration. This won't allow one
               --  pragma to appear in the public part and one in the private
               --  part, but that seems very unlikely in practice.

            else
               Decl := Prev (N);
               while Present (Decl) and then Decl /= Pdec loop

                  --  Look for pragma with same name as us

                  if Nkind (Decl) = N_Pragma
                    and then Same_Name (Decl)
                  then
                     --  Give error if same as our pragma or Export/Convention

                     if Pragma_Name (Decl) = Name_Export
                          or else
                        Pragma_Name (Decl) = Name_Convention
                          or else
                        Pragma_Name (Decl) = Pragma_Name (N)
                     then
                        exit;

                     --  Case of Import/Interface or the other way round

                     elsif Pragma_Name (Decl) = Name_Interface
                             or else
                           Pragma_Name (Decl) = Name_Import
                     then
                        --  Here we know that we have Import and Interface. It
                        --  doesn't matter which way round they are. See if
                        --  they specify the same convention. If so, all OK,
                        --  and set special flags to stop other messages

                        if Same_Convention (Decl) then
                           Set_Import_Interface_Present (N);
                           Set_Import_Interface_Present (Decl);
                           Err := False;

                        --  If different conventions, special message

                        else
                           Error_Msg_Sloc := Sloc (Decl);
                           Error_Pragma_Arg
                             ("convention differs from that given#", Arg1);
                           return;
                        end if;
                     end if;
                  end if;

                  Next (Decl);
               end loop;
            end if;

            --  Give message if needed if we fall through those tests

            if Err then
               Error_Pragma_Arg
                 ("at most one Convention/Export/Import pragma is allowed",
                  Arg2);
            end if;
         end Diagnose_Multiple_Pragmas;

         --------------------------------
         -- Set_Convention_From_Pragma --
         --------------------------------

         procedure Set_Convention_From_Pragma (E : Entity_Id) is
         begin
            --  Ada 2005 (AI-430): Check invalid attempt to change convention
            --  for an overridden dispatching operation. Technically this is
            --  an amendment and should only be done in Ada 2005 mode. However,
            --  this is clearly a mistake, since the problem that is addressed
            --  by this AI is that there is a clear gap in the RM!

            if Is_Dispatching_Operation (E)
              and then Present (Overridden_Operation (E))
              and then C /= Convention (Overridden_Operation (E))
            then
               Error_Pragma_Arg
                 ("cannot change convention for " &
                  "overridden dispatching operation",
                  Arg1);
            end if;

            --  Set the convention

            Set_Convention (E, C);
            Set_Has_Convention_Pragma (E);

            if Is_Incomplete_Or_Private_Type (E)
              and then Present (Underlying_Type (E))
            then
               Set_Convention            (Underlying_Type (E), C);
               Set_Has_Convention_Pragma (Underlying_Type (E), True);
            end if;

            --  A class-wide type should inherit the convention of the specific
            --  root type (although this isn't specified clearly by the RM).

            if Is_Type (E) and then Present (Class_Wide_Type (E)) then
               Set_Convention (Class_Wide_Type (E), C);
            end if;

            --  If the entity is a record type, then check for special case of
            --  C_Pass_By_Copy, which is treated the same as C except that the
            --  special record flag is set. This convention is only permitted
            --  on record types (see AI95-00131).

            if Cname = Name_C_Pass_By_Copy then
               if Is_Record_Type (E) then
                  Set_C_Pass_By_Copy (Base_Type (E));
               elsif Is_Incomplete_Or_Private_Type (E)
                 and then Is_Record_Type (Underlying_Type (E))
               then
                  Set_C_Pass_By_Copy (Base_Type (Underlying_Type (E)));
               else
                  Error_Pragma_Arg
                    ("C_Pass_By_Copy convention allowed only for record type",
                     Arg2);
               end if;
            end if;

            --  If the entity is a derived boolean type, check for the special
            --  case of convention C, C++, or Fortran, where we consider any
            --  nonzero value to represent true.

            if Is_Discrete_Type (E)
              and then Root_Type (Etype (E)) = Standard_Boolean
              and then
                (C = Convention_C
                   or else
                 C = Convention_CPP
                   or else
                 C = Convention_Fortran)
            then
               Set_Nonzero_Is_True (Base_Type (E));
            end if;
         end Set_Convention_From_Pragma;

      --  Start of processing for Process_Convention

      begin
         Check_At_Least_N_Arguments (2);
         Check_Optional_Identifier (Arg1, Name_Convention);
         Check_Arg_Is_Identifier (Arg1);
         Cname := Chars (Get_Pragma_Arg (Arg1));

         --  C_Pass_By_Copy is treated as a synonym for convention C (this is
         --  tested again below to set the critical flag).

         if Cname = Name_C_Pass_By_Copy then
            C := Convention_C;

         --  Otherwise we must have something in the standard convention list

         elsif Is_Convention_Name (Cname) then
            C := Get_Convention_Id (Chars (Get_Pragma_Arg (Arg1)));

         --  In DEC VMS, it seems that there is an undocumented feature that
         --  any unrecognized convention is treated as the default, which for
         --  us is convention C. It does not seem so terrible to do this
         --  unconditionally, silently in the VMS case, and with a warning
         --  in the non-VMS case.

         else
            if Warn_On_Export_Import and not OpenVMS_On_Target then
               Error_Msg_N
                 ("?unrecognized convention name, C assumed",
                  Get_Pragma_Arg (Arg1));
            end if;

            C := Convention_C;
         end if;

         Check_Optional_Identifier (Arg2, Name_Entity);
         Check_Arg_Is_Local_Name (Arg2);

         Id := Get_Pragma_Arg (Arg2);
         Analyze (Id);

         if not Is_Entity_Name (Id) then
            Error_Pragma_Arg ("entity name required", Arg2);
         end if;

         E := Entity (Id);

         --  Set entity to return

         Ent := E;

         --  Ada_Pass_By_Copy special checking

         if C = Convention_Ada_Pass_By_Copy then
            if not Is_First_Subtype (E) then
               Error_Pragma_Arg
                 ("convention `Ada_Pass_By_Copy` only "
                  & "allowed for types", Arg2);
            end if;

            if Is_By_Reference_Type (E) then
               Error_Pragma_Arg
                 ("convention `Ada_Pass_By_Copy` not allowed for "
                  & "by-reference type", Arg1);
            end if;
         end if;

         --  Ada_Pass_By_Reference special checking

         if C = Convention_Ada_Pass_By_Reference then
            if not Is_First_Subtype (E) then
               Error_Pragma_Arg
                 ("convention `Ada_Pass_By_Reference` only "
                  & "allowed for types", Arg2);
            end if;

            if Is_By_Copy_Type (E) then
               Error_Pragma_Arg
                 ("convention `Ada_Pass_By_Reference` not allowed for "
                  & "by-copy type", Arg1);
            end if;
         end if;

         --  Go to renamed subprogram if present, since convention applies to
         --  the actual renamed entity, not to the renaming entity. If the
         --  subprogram is inherited, go to parent subprogram.

         if Is_Subprogram (E)
           and then Present (Alias (E))
         then
            if Nkind (Parent (Declaration_Node (E))) =
                                       N_Subprogram_Renaming_Declaration
            then
               if Scope (E) /= Scope (Alias (E)) then
                  Error_Pragma_Ref
                    ("cannot apply pragma% to non-local entity&#", E);
               end if;

               E := Alias (E);

            elsif Nkind_In (Parent (E), N_Full_Type_Declaration,
                                        N_Private_Extension_Declaration)
              and then Scope (E) = Scope (Alias (E))
            then
               E := Alias (E);

               --  Return the parent subprogram the entity was inherited from

               Ent := E;
            end if;
         end if;

         --  Check that we are not applying this to a specless body

         if Is_Subprogram (E)
           and then Nkind (Parent (Declaration_Node (E))) = N_Subprogram_Body
         then
            Error_Pragma
              ("pragma% requires separate spec and must come before body");
         end if;

         --  Check that we are not applying this to a named constant

         if Ekind_In (E, E_Named_Integer, E_Named_Real) then
            Error_Msg_Name_1 := Pname;
            Error_Msg_N
              ("cannot apply pragma% to named constant!",
               Get_Pragma_Arg (Arg2));
            Error_Pragma_Arg
              ("\supply appropriate type for&!", Arg2);
         end if;

         if Ekind (E) = E_Enumeration_Literal then
            Error_Pragma ("enumeration literal not allowed for pragma%");
         end if;

         --  Check for rep item appearing too early or too late

         if Etype (E) = Any_Type
           or else Rep_Item_Too_Early (E, N)
         then
            raise Pragma_Exit;

         elsif Present (Underlying_Type (E)) then
            E := Underlying_Type (E);
         end if;

         if Rep_Item_Too_Late (E, N) then
            raise Pragma_Exit;
         end if;

         if Has_Convention_Pragma (E) then
            Diagnose_Multiple_Pragmas (E);

         elsif Convention (E) = Convention_Protected
           or else Ekind (Scope (E)) = E_Protected_Type
         then
            Error_Pragma_Arg
              ("a protected operation cannot be given a different convention",
                Arg2);
         end if;

         --  For Intrinsic, a subprogram is required

         if C = Convention_Intrinsic
           and then not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
         then
            Error_Pragma_Arg
              ("second argument of pragma% must be a subprogram", Arg2);
         end if;

         --  Stdcall case

         if C = Convention_Stdcall then

            --  A dispatching call is not allowed. A dispatching subprogram
            --  cannot be used to interface to the Win32 API, so in fact this
            --  check does not impose any effective restriction.

            if Is_Dispatching_Operation (E) then

               Error_Pragma
                 ("dispatching subprograms cannot use Stdcall convention");

            --  Subprogram is allowed, but not a generic subprogram, and not a
            --  dispatching operation.

            elsif not Is_Subprogram (E)
              and then not Is_Generic_Subprogram (E)

              --  A variable is OK

              and then Ekind (E) /= E_Variable

              --  An access to subprogram is also allowed

              and then not
                (Is_Access_Type (E)
                  and then Ekind (Designated_Type (E)) = E_Subprogram_Type)
            then
               Error_Pragma_Arg
                 ("second argument of pragma% must be subprogram (type)",
                  Arg2);
            end if;
         end if;

         if not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
         then
            Set_Convention_From_Pragma (E);

            if Is_Type (E) then
               Check_First_Subtype (Arg2);
               Set_Convention_From_Pragma (Base_Type (E));

               --  For subprograms, we must set the convention on the
               --  internally generated directly designated type as well.

               if Ekind (E) = E_Access_Subprogram_Type then
                  Set_Convention_From_Pragma (Directly_Designated_Type (E));
               end if;
            end if;

         --  For the subprogram case, set proper convention for all homonyms
         --  in same scope and the same declarative part, i.e. the same
         --  compilation unit.

         else
            Comp_Unit := Get_Source_Unit (E);
            Set_Convention_From_Pragma (E);

            --  Treat a pragma Import as an implicit body, for GPS use

            if Prag_Id = Pragma_Import then
               Generate_Reference (E, Id, 'b');
            end if;

            --  Loop through the homonyms of the pragma argument's entity

            E1 := Ent;
            loop
               E1 := Homonym (E1);
               exit when No (E1) or else Scope (E1) /= Current_Scope;

               --  Do not set the pragma on inherited operations or on formal
               --  subprograms.

               if Comes_From_Source (E1)
                 and then Comp_Unit = Get_Source_Unit (E1)
                 and then not Is_Formal_Subprogram (E1)
                 and then Nkind (Original_Node (Parent (E1))) /=
                                                    N_Full_Type_Declaration
               then
                  if Present (Alias (E1))
                    and then Scope (E1) /= Scope (Alias (E1))
                  then
                     Error_Pragma_Ref
                       ("cannot apply pragma% to non-local entity& declared#",
                        E1);
                  end if;

                  Set_Convention_From_Pragma (E1);

                  if Prag_Id = Pragma_Import then
                     Generate_Reference (E1, Id, 'b');
                  end if;
               end if;

               --  For aspect case, do NOT apply to homonyms

               exit when From_Aspect_Specification (N);
            end loop;
         end if;
      end Process_Convention;

      ----------------------------------------
      -- Process_Disable_Enable_Atomic_Sync --
      ----------------------------------------

      procedure Process_Disable_Enable_Atomic_Sync (Nam : Name_Id) is
      begin
         GNAT_Pragma;
         Check_No_Identifiers;
         Check_At_Most_N_Arguments (1);

         --  Modeled internally as
         --    pragma Unsuppress (Atomic_Synchronization [,Entity])

         Rewrite (N,
           Make_Pragma (Loc,
             Pragma_Identifier            =>
               Make_Identifier (Loc, Nam),
             Pragma_Argument_Associations => New_List (
               Make_Pragma_Argument_Association (Loc,
                 Expression =>
                   Make_Identifier (Loc, Name_Atomic_Synchronization)))));

         if Present (Arg1) then
            Append_To (Pragma_Argument_Associations (N), New_Copy (Arg1));
         end if;

         Analyze (N);
      end Process_Disable_Enable_Atomic_Sync;

      -----------------------------------------------------
      -- Process_Extended_Import_Export_Exception_Pragma --
      -----------------------------------------------------

      procedure Process_Extended_Import_Export_Exception_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Form     : Node_Id;
         Arg_Code     : Node_Id)
      is
         Def_Id   : Entity_Id;
         Code_Val : Uint;

      begin
         if not OpenVMS_On_Target then
            Error_Pragma
              ("?pragma% ignored (applies only to Open'V'M'S)");
         end if;

         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Def_Id := Entity (Arg_Internal);

         if Ekind (Def_Id) /= E_Exception then
            Error_Pragma_Arg
              ("pragma% must refer to declared exception", Arg_Internal);
         end if;

         Set_Extended_Import_Export_External_Name (Def_Id, Arg_External);

         if Present (Arg_Form) then
            Check_Arg_Is_One_Of (Arg_Form, Name_Ada, Name_VMS);
         end if;

         if Present (Arg_Form)
           and then Chars (Arg_Form) = Name_Ada
         then
            null;
         else
            Set_Is_VMS_Exception (Def_Id);
            Set_Exception_Code (Def_Id, No_Uint);
         end if;

         if Present (Arg_Code) then
            if not Is_VMS_Exception (Def_Id) then
               Error_Pragma_Arg
                 ("Code option for pragma% not allowed for Ada case",
                  Arg_Code);
            end if;

            Check_Arg_Is_Static_Expression (Arg_Code, Any_Integer);
            Code_Val := Expr_Value (Arg_Code);

            if not UI_Is_In_Int_Range (Code_Val) then
               Error_Pragma_Arg
                 ("Code option for pragma% must be in 32-bit range",
                  Arg_Code);

            else
               Set_Exception_Code (Def_Id, Code_Val);
            end if;
         end if;
      end Process_Extended_Import_Export_Exception_Pragma;

      -------------------------------------------------
      -- Process_Extended_Import_Export_Internal_Arg --
      -------------------------------------------------

      procedure Process_Extended_Import_Export_Internal_Arg
        (Arg_Internal : Node_Id := Empty)
      is
      begin
         if No (Arg_Internal) then
            Error_Pragma ("Internal parameter required for pragma%");
         end if;

         if Nkind (Arg_Internal) = N_Identifier then
            null;

         elsif Nkind (Arg_Internal) = N_Operator_Symbol
           and then (Prag_Id = Pragma_Import_Function
                       or else
                     Prag_Id = Pragma_Export_Function)
         then
            null;

         else
            Error_Pragma_Arg
              ("wrong form for Internal parameter for pragma%", Arg_Internal);
         end if;

         Check_Arg_Is_Local_Name (Arg_Internal);
      end Process_Extended_Import_Export_Internal_Arg;

      --------------------------------------------------
      -- Process_Extended_Import_Export_Object_Pragma --
      --------------------------------------------------

      procedure Process_Extended_Import_Export_Object_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Size     : Node_Id)
      is
         Def_Id : Entity_Id;

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Def_Id := Entity (Arg_Internal);

         if not Ekind_In (Def_Id, E_Constant, E_Variable) then
            Error_Pragma_Arg
              ("pragma% must designate an object", Arg_Internal);
         end if;

         if Has_Rep_Pragma (Def_Id, Name_Common_Object)
              or else
            Has_Rep_Pragma (Def_Id, Name_Psect_Object)
         then
            Error_Pragma_Arg
              ("previous Common/Psect_Object applies, pragma % not permitted",
               Arg_Internal);
         end if;

         if Rep_Item_Too_Late (Def_Id, N) then
            raise Pragma_Exit;
         end if;

         Set_Extended_Import_Export_External_Name (Def_Id, Arg_External);

         if Present (Arg_Size) then
            Check_Arg_Is_External_Name (Arg_Size);
         end if;

         --  Export_Object case

         if Prag_Id = Pragma_Export_Object then
            if not Is_Library_Level_Entity (Def_Id) then
               Error_Pragma_Arg
                 ("argument for pragma% must be library level entity",
                  Arg_Internal);
            end if;

            if Ekind (Current_Scope) = E_Generic_Package then
               Error_Pragma ("pragma& cannot appear in a generic unit");
            end if;

            if not Size_Known_At_Compile_Time (Etype (Def_Id)) then
               Error_Pragma_Arg
                 ("exported object must have compile time known size",
                  Arg_Internal);
            end if;

            if Warn_On_Export_Import and then Is_Exported (Def_Id) then
               Error_Msg_N ("?duplicate Export_Object pragma", N);
            else
               Set_Exported (Def_Id, Arg_Internal);
            end if;

         --  Import_Object case

         else
            if Is_Concurrent_Type (Etype (Def_Id)) then
               Error_Pragma_Arg
                 ("cannot use pragma% for task/protected object",
                  Arg_Internal);
            end if;

            if Ekind (Def_Id) = E_Constant then
               Error_Pragma_Arg
                 ("cannot import a constant", Arg_Internal);
            end if;

            if Warn_On_Export_Import
              and then Has_Discriminants (Etype (Def_Id))
            then
               Error_Msg_N
                 ("imported value must be initialized?", Arg_Internal);
            end if;

            if Warn_On_Export_Import
              and then Is_Access_Type (Etype (Def_Id))
            then
               Error_Pragma_Arg
                 ("cannot import object of an access type?", Arg_Internal);
            end if;

            if Warn_On_Export_Import
              and then Is_Imported (Def_Id)
            then
               Error_Msg_N
                 ("?duplicate Import_Object pragma", N);

            --  Check for explicit initialization present. Note that an
            --  initialization generated by the code generator, e.g. for an
            --  access type, does not count here.

            elsif Present (Expression (Parent (Def_Id)))
               and then
                 Comes_From_Source
                   (Original_Node (Expression (Parent (Def_Id))))
            then
               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Pragma_Arg
                 ("imported entities cannot be initialized (RM B.1(24))",
                  "\no initialization allowed for & declared#", Arg1);
            else
               Set_Imported (Def_Id);
               Note_Possible_Modification (Arg_Internal, Sure => False);
            end if;
         end if;
      end Process_Extended_Import_Export_Object_Pragma;

      ------------------------------------------------------
      -- Process_Extended_Import_Export_Subprogram_Pragma --
      ------------------------------------------------------

      procedure Process_Extended_Import_Export_Subprogram_Pragma
        (Arg_Internal                 : Node_Id;
         Arg_External                 : Node_Id;
         Arg_Parameter_Types          : Node_Id;
         Arg_Result_Type              : Node_Id := Empty;
         Arg_Mechanism                : Node_Id;
         Arg_Result_Mechanism         : Node_Id := Empty;
         Arg_First_Optional_Parameter : Node_Id := Empty)
      is
         Ent       : Entity_Id;
         Def_Id    : Entity_Id;
         Hom_Id    : Entity_Id;
         Formal    : Entity_Id;
         Ambiguous : Boolean;
         Match     : Boolean;
         Dval      : Node_Id;

         function Same_Base_Type
          (Ptype  : Node_Id;
           Formal : Entity_Id) return Boolean;
         --  Determines if Ptype references the type of Formal. Note that only
         --  the base types need to match according to the spec. Ptype here is
         --  the argument from the pragma, which is either a type name, or an
         --  access attribute.

         --------------------
         -- Same_Base_Type --
         --------------------

         function Same_Base_Type
           (Ptype  : Node_Id;
            Formal : Entity_Id) return Boolean
         is
            Ftyp : constant Entity_Id := Base_Type (Etype (Formal));
            Pref : Node_Id;

         begin
            --  Case where pragma argument is typ'Access

            if Nkind (Ptype) = N_Attribute_Reference
              and then Attribute_Name (Ptype) = Name_Access
            then
               Pref := Prefix (Ptype);
               Find_Type (Pref);

               if not Is_Entity_Name (Pref)
                 or else Entity (Pref) = Any_Type
               then
                  raise Pragma_Exit;
               end if;

               --  We have a match if the corresponding argument is of an
               --  anonymous access type, and its designated type matches the
               --  type of the prefix of the access attribute

               return Ekind (Ftyp) = E_Anonymous_Access_Type
                 and then Base_Type (Entity (Pref)) =
                            Base_Type (Etype (Designated_Type (Ftyp)));

            --  Case where pragma argument is a type name

            else
               Find_Type (Ptype);

               if not Is_Entity_Name (Ptype)
                 or else Entity (Ptype) = Any_Type
               then
                  raise Pragma_Exit;
               end if;

               --  We have a match if the corresponding argument is of the type
               --  given in the pragma (comparing base types)

               return Base_Type (Entity (Ptype)) = Ftyp;
            end if;
         end Same_Base_Type;

      --  Start of processing for
      --  Process_Extended_Import_Export_Subprogram_Pragma

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Ent := Empty;
         Ambiguous := False;

         --  Loop through homonyms (overloadings) of the entity

         Hom_Id := Entity (Arg_Internal);
         while Present (Hom_Id) loop
            Def_Id := Get_Base_Subprogram (Hom_Id);

            --  We need a subprogram in the current scope

            if not Is_Subprogram (Def_Id)
              or else Scope (Def_Id) /= Current_Scope
            then
               null;

            else
               Match := True;

               --  Pragma cannot apply to subprogram body

               if Is_Subprogram (Def_Id)
                 and then Nkind (Parent (Declaration_Node (Def_Id))) =
                                                             N_Subprogram_Body
               then
                  Error_Pragma
                    ("pragma% requires separate spec"
                      & " and must come before body");
               end if;

               --  Test result type if given, note that the result type
               --  parameter can only be present for the function cases.

               if Present (Arg_Result_Type)
                 and then not Same_Base_Type (Arg_Result_Type, Def_Id)
               then
                  Match := False;

               elsif Etype (Def_Id) /= Standard_Void_Type
                 and then
                   (Pname = Name_Export_Procedure
                      or else
                    Pname = Name_Import_Procedure)
               then
                  Match := False;

               --  Test parameter types if given. Note that this parameter
               --  has not been analyzed (and must not be, since it is
               --  semantic nonsense), so we get it as the parser left it.

               elsif Present (Arg_Parameter_Types) then
                  Check_Matching_Types : declare
                     Formal : Entity_Id;
                     Ptype  : Node_Id;

                  begin
                     Formal := First_Formal (Def_Id);

                     if Nkind (Arg_Parameter_Types) = N_Null then
                        if Present (Formal) then
                           Match := False;
                        end if;

                     --  A list of one type, e.g. (List) is parsed as
                     --  a parenthesized expression.

                     elsif Nkind (Arg_Parameter_Types) /= N_Aggregate
                       and then Paren_Count (Arg_Parameter_Types) = 1
                     then
                        if No (Formal)
                          or else Present (Next_Formal (Formal))
                        then
                           Match := False;
                        else
                           Match :=
                             Same_Base_Type (Arg_Parameter_Types, Formal);
                        end if;

                     --  A list of more than one type is parsed as a aggregate

                     elsif Nkind (Arg_Parameter_Types) = N_Aggregate
                       and then Paren_Count (Arg_Parameter_Types) = 0
                     then
                        Ptype := First (Expressions (Arg_Parameter_Types));
                        while Present (Ptype) or else Present (Formal) loop
                           if No (Ptype)
                             or else No (Formal)
                             or else not Same_Base_Type (Ptype, Formal)
                           then
                              Match := False;
                              exit;
                           else
                              Next_Formal (Formal);
                              Next (Ptype);
                           end if;
                        end loop;

                     --  Anything else is of the wrong form

                     else
                        Error_Pragma_Arg
                          ("wrong form for Parameter_Types parameter",
                           Arg_Parameter_Types);
                     end if;
                  end Check_Matching_Types;
               end if;

               --  Match is now False if the entry we found did not match
               --  either a supplied Parameter_Types or Result_Types argument

               if Match then
                  if No (Ent) then
                     Ent := Def_Id;

                  --  Ambiguous case, the flag Ambiguous shows if we already
                  --  detected this and output the initial messages.

                  else
                     if not Ambiguous then
                        Ambiguous := True;
                        Error_Msg_Name_1 := Pname;
                        Error_Msg_N
                          ("pragma% does not uniquely identify subprogram!",
                           N);
                        Error_Msg_Sloc := Sloc (Ent);
                        Error_Msg_N ("matching subprogram #!", N);
                        Ent := Empty;
                     end if;

                     Error_Msg_Sloc := Sloc (Def_Id);
                     Error_Msg_N ("matching subprogram #!", N);
                  end if;
               end if;
            end if;

            Hom_Id := Homonym (Hom_Id);
         end loop;

         --  See if we found an entry

         if No (Ent) then
            if not Ambiguous then
               if Is_Generic_Subprogram (Entity (Arg_Internal)) then
                  Error_Pragma
                    ("pragma% cannot be given for generic subprogram");
               else
                  Error_Pragma
                    ("pragma% does not identify local subprogram");
               end if;
            end if;

            return;
         end if;

         --  Import pragmas must be for imported entities

         if Prag_Id = Pragma_Import_Function
              or else
            Prag_Id = Pragma_Import_Procedure
              or else
            Prag_Id = Pragma_Import_Valued_Procedure
         then
            if not Is_Imported (Ent) then
               Error_Pragma
                 ("pragma Import or Interface must precede pragma%");
            end if;

         --  Here we have the Export case which can set the entity as exported

         --  But does not do so if the specified external name is null, since
         --  that is taken as a signal in DEC Ada 83 (with which we want to be
         --  compatible) to request no external name.

         elsif Nkind (Arg_External) = N_String_Literal
           and then String_Length (Strval (Arg_External)) = 0
         then
            null;

         --  In all other cases, set entity as exported

         else
            Set_Exported (Ent, Arg_Internal);
         end if;

         --  Special processing for Valued_Procedure cases

         if Prag_Id = Pragma_Import_Valued_Procedure
           or else
            Prag_Id = Pragma_Export_Valued_Procedure
         then
            Formal := First_Formal (Ent);

            if No (Formal) then
               Error_Pragma ("at least one parameter required for pragma%");

            elsif Ekind (Formal) /= E_Out_Parameter then
               Error_Pragma ("first parameter must have mode out for pragma%");

            else
               Set_Is_Valued_Procedure (Ent);
            end if;
         end if;

         Set_Extended_Import_Export_External_Name (Ent, Arg_External);

         --  Process Result_Mechanism argument if present. We have already
         --  checked that this is only allowed for the function case.

         if Present (Arg_Result_Mechanism) then
            Set_Mechanism_Value (Ent, Arg_Result_Mechanism);
         end if;

         --  Process Mechanism parameter if present. Note that this parameter
         --  is not analyzed, and must not be analyzed since it is semantic
         --  nonsense, so we get it in exactly as the parser left it.

         if Present (Arg_Mechanism) then
            declare
               Formal : Entity_Id;
               Massoc : Node_Id;
               Mname  : Node_Id;
               Choice : Node_Id;

            begin
               --  A single mechanism association without a formal parameter
               --  name is parsed as a parenthesized expression. All other
               --  cases are parsed as aggregates, so we rewrite the single
               --  parameter case as an aggregate for consistency.

               if Nkind (Arg_Mechanism) /= N_Aggregate
                 and then Paren_Count (Arg_Mechanism) = 1
               then
                  Rewrite (Arg_Mechanism,
                    Make_Aggregate (Sloc (Arg_Mechanism),
                      Expressions => New_List (
                        Relocate_Node (Arg_Mechanism))));
               end if;

               --  Case of only mechanism name given, applies to all formals

               if Nkind (Arg_Mechanism) /= N_Aggregate then
                  Formal := First_Formal (Ent);
                  while Present (Formal) loop
                     Set_Mechanism_Value (Formal, Arg_Mechanism);
                     Next_Formal (Formal);
                  end loop;

               --  Case of list of mechanism associations given

               else
                  if Null_Record_Present (Arg_Mechanism) then
                     Error_Pragma_Arg
                       ("inappropriate form for Mechanism parameter",
                        Arg_Mechanism);
                  end if;

                  --  Deal with positional ones first

                  Formal := First_Formal (Ent);

                  if Present (Expressions (Arg_Mechanism)) then
                     Mname := First (Expressions (Arg_Mechanism));
                     while Present (Mname) loop
                        if No (Formal) then
                           Error_Pragma_Arg
                             ("too many mechanism associations", Mname);
                        end if;

                        Set_Mechanism_Value (Formal, Mname);
                        Next_Formal (Formal);
                        Next (Mname);
                     end loop;
                  end if;

                  --  Deal with named entries

                  if Present (Component_Associations (Arg_Mechanism)) then
                     Massoc := First (Component_Associations (Arg_Mechanism));
                     while Present (Massoc) loop
                        Choice := First (Choices (Massoc));

                        if Nkind (Choice) /= N_Identifier
                          or else Present (Next (Choice))
                        then
                           Error_Pragma_Arg
                             ("incorrect form for mechanism association",
                              Massoc);
                        end if;

                        Formal := First_Formal (Ent);
                        loop
                           if No (Formal) then
                              Error_Pragma_Arg
                                ("parameter name & not present", Choice);
                           end if;

                           if Chars (Choice) = Chars (Formal) then
                              Set_Mechanism_Value
                                (Formal, Expression (Massoc));

                              --  Set entity on identifier (needed by ASIS)

                              Set_Entity (Choice, Formal);

                              exit;
                           end if;

                           Next_Formal (Formal);
                        end loop;

                        Next (Massoc);
                     end loop;
                  end if;
               end if;
            end;
         end if;

         --  Process First_Optional_Parameter argument if present. We have
         --  already checked that this is only allowed for the Import case.

         if Present (Arg_First_Optional_Parameter) then
            if Nkind (Arg_First_Optional_Parameter) /= N_Identifier then
               Error_Pragma_Arg
                 ("first optional parameter must be formal parameter name",
                  Arg_First_Optional_Parameter);
            end if;

            Formal := First_Formal (Ent);
            loop
               if No (Formal) then
                  Error_Pragma_Arg
                    ("specified formal parameter& not found",
                     Arg_First_Optional_Parameter);
               end if;

               exit when Chars (Formal) =
                         Chars (Arg_First_Optional_Parameter);

               Next_Formal (Formal);
            end loop;

            Set_First_Optional_Parameter (Ent, Formal);

            --  Check specified and all remaining formals have right form

            while Present (Formal) loop
               if Ekind (Formal) /= E_In_Parameter then
                  Error_Msg_NE
                    ("optional formal& is not of mode in!",
                     Arg_First_Optional_Parameter, Formal);

               else
                  Dval := Default_Value (Formal);

                  if No (Dval) then
                     Error_Msg_NE
                       ("optional formal& does not have default value!",
                        Arg_First_Optional_Parameter, Formal);

                  elsif Compile_Time_Known_Value_Or_Aggr (Dval) then
                     null;

                  else
                     Error_Msg_FE
                       ("default value for optional formal& is non-static!",
                        Arg_First_Optional_Parameter, Formal);
                  end if;
               end if;

               Set_Is_Optional_Parameter (Formal);
               Next_Formal (Formal);
            end loop;
         end if;
      end Process_Extended_Import_Export_Subprogram_Pragma;

      --------------------------
      -- Process_Generic_List --
      --------------------------

      procedure Process_Generic_List is
         Arg : Node_Id;
         Exp : Node_Id;

      begin
         Check_No_Identifiers;
         Check_At_Least_N_Arguments (1);

         Arg := Arg1;
         while Present (Arg) loop
            Exp := Get_Pragma_Arg (Arg);
            Analyze (Exp);

            if not Is_Entity_Name (Exp)
              or else
                (not Is_Generic_Instance (Entity (Exp))
                  and then
                 not Is_Generic_Unit (Entity (Exp)))
            then
               Error_Pragma_Arg
                 ("pragma% argument must be name of generic unit/instance",
                  Arg);
            end if;

            Next (Arg);
         end loop;
      end Process_Generic_List;

      ------------------------------------
      -- Process_Import_Predefined_Type --
      ------------------------------------

      procedure Process_Import_Predefined_Type is
         Loc  : constant Source_Ptr := Sloc (N);
         Elmt : Elmt_Id;
         Ftyp : Node_Id := Empty;
         Decl : Node_Id;
         Def  : Node_Id;
         Nam  : Name_Id;

      begin
         String_To_Name_Buffer (Strval (Expression (Arg3)));
         Nam := Name_Find;

         Elmt := First_Elmt (Predefined_Float_Types);
         while Present (Elmt) and then Chars (Node (Elmt)) /= Nam loop
            Next_Elmt (Elmt);
         end loop;

         Ftyp := Node (Elmt);

         if Present (Ftyp) then

            --  Don't build a derived type declaration, because predefined C
            --  types have no declaration anywhere, so cannot really be named.
            --  Instead build a full type declaration, starting with an
            --  appropriate type definition is built

            if Is_Floating_Point_Type (Ftyp) then
               Def := Make_Floating_Point_Definition (Loc,
                 Make_Integer_Literal (Loc, Digits_Value (Ftyp)),
                 Make_Real_Range_Specification (Loc,
                   Make_Real_Literal (Loc, Realval (Type_Low_Bound (Ftyp))),
                   Make_Real_Literal (Loc, Realval (Type_High_Bound (Ftyp)))));

            --  Should never have a predefined type we cannot handle

            else
               raise Program_Error;
            end if;

            --  Build and insert a Full_Type_Declaration, which will be
            --  analyzed as soon as this list entry has been analyzed.

            Decl := Make_Full_Type_Declaration (Loc,
              Make_Defining_Identifier (Loc, Chars (Expression (Arg2))),
              Type_Definition => Def);

            Insert_After (N, Decl);
            Mark_Rewrite_Insertion (Decl);

         else
            Error_Pragma_Arg ("no matching type found for pragma%",
            Arg2);
         end if;
      end Process_Import_Predefined_Type;

      ---------------------------------
      -- Process_Import_Or_Interface --
      ---------------------------------

      procedure Process_Import_Or_Interface is
         C      : Convention_Id;
         Def_Id : Entity_Id;
         Hom_Id : Entity_Id;

      begin
         Process_Convention (C, Def_Id);
         Kill_Size_Check_Code (Def_Id);
         Note_Possible_Modification (Get_Pragma_Arg (Arg2), Sure => False);

         if Ekind_In (Def_Id, E_Variable, E_Constant) then

            --  We do not permit Import to apply to a renaming declaration

            if Present (Renamed_Object (Def_Id)) then
               Error_Pragma_Arg
                 ("pragma% not allowed for object renaming", Arg2);

            --  User initialization is not allowed for imported object, but
            --  the object declaration may contain a default initialization,
            --  that will be discarded. Note that an explicit initialization
            --  only counts if it comes from source, otherwise it is simply
            --  the code generator making an implicit initialization explicit.

            elsif Present (Expression (Parent (Def_Id)))
              and then Comes_From_Source (Expression (Parent (Def_Id)))
            then
               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Pragma_Arg
                 ("no initialization allowed for declaration of& #",
                  "\imported entities cannot be initialized (RM B.1(24))",
                  Arg2);

            else
               Set_Imported (Def_Id);
               Process_Interface_Name (Def_Id, Arg3, Arg4);

               --  Note that we do not set Is_Public here. That's because we
               --  only want to set it if there is no address clause, and we
               --  don't know that yet, so we delay that processing till
               --  freeze time.

               --  pragma Import completes deferred constants

               if Ekind (Def_Id) = E_Constant then
                  Set_Has_Completion (Def_Id);
               end if;

               --  It is not possible to import a constant of an unconstrained
               --  array type (e.g. string) because there is no simple way to
               --  write a meaningful subtype for it.

               if Is_Array_Type (Etype (Def_Id))
                 and then not Is_Constrained (Etype (Def_Id))
               then
                  Error_Msg_NE
                    ("imported constant& must have a constrained subtype",
                      N, Def_Id);
               end if;
            end if;

         elsif Is_Subprogram (Def_Id)
           or else Is_Generic_Subprogram (Def_Id)
         then
            --  If the name is overloaded, pragma applies to all of the denoted
            --  entities in the same declarative part.

            Hom_Id := Def_Id;
            while Present (Hom_Id) loop
               Def_Id := Get_Base_Subprogram (Hom_Id);

               --  Ignore inherited subprograms because the pragma will apply
               --  to the parent operation, which is the one called.

               if Is_Overloadable (Def_Id)
                 and then Present (Alias (Def_Id))
               then
                  null;

               --  If it is not a subprogram, it must be in an outer scope and
               --  pragma does not apply.

               elsif not Is_Subprogram (Def_Id)
                 and then not Is_Generic_Subprogram (Def_Id)
               then
                  null;

               --  The pragma does not apply to primitives of interfaces

               elsif Is_Dispatching_Operation (Def_Id)
                 and then Present (Find_Dispatching_Type (Def_Id))
                 and then Is_Interface (Find_Dispatching_Type (Def_Id))
               then
                  null;

               --  Verify that the homonym is in the same declarative part (not
               --  just the same scope).

               elsif Parent (Unit_Declaration_Node (Def_Id)) /= Parent (N)
                 and then Nkind (Parent (N)) /= N_Compilation_Unit_Aux
               then
                  exit;

               else
                  Set_Imported (Def_Id);

                  --  Reject an Import applied to an abstract subprogram

                  if Is_Subprogram (Def_Id)
                    and then Is_Abstract_Subprogram (Def_Id)
                  then
                     Error_Msg_Sloc := Sloc (Def_Id);
                     Error_Msg_NE
                       ("cannot import abstract subprogram& declared#",
                        Arg2, Def_Id);
                  end if;

                  --  Special processing for Convention_Intrinsic

                  if C = Convention_Intrinsic then

                     --  Link_Name argument not allowed for intrinsic

                     Check_No_Link_Name;

                     Set_Is_Intrinsic_Subprogram (Def_Id);

                     --  If no external name is present, then check that this
                     --  is a valid intrinsic subprogram. If an external name
                     --  is present, then this is handled by the back end.

                     if No (Arg3) then
                        Check_Intrinsic_Subprogram
                          (Def_Id, Get_Pragma_Arg (Arg2));
                     end if;
                  end if;

                  --  All interfaced procedures need an external symbol created
                  --  for them since they are always referenced from another
                  --  object file.

                  Set_Is_Public (Def_Id);

                  --  Verify that the subprogram does not have a completion
                  --  through a renaming declaration. For other completions the
                  --  pragma appears as a too late representation.

                  declare
                     Decl : constant Node_Id := Unit_Declaration_Node (Def_Id);

                  begin
                     if Present (Decl)
                       and then Nkind (Decl) = N_Subprogram_Declaration
                       and then Present (Corresponding_Body (Decl))
                       and then Nkind (Unit_Declaration_Node
                                        (Corresponding_Body (Decl))) =
                                             N_Subprogram_Renaming_Declaration
                     then
                        Error_Msg_Sloc := Sloc (Def_Id);
                        Error_Msg_NE
                          ("cannot import&, renaming already provided for " &
                           "declaration #", N, Def_Id);
                     end if;
                  end;

                  Set_Has_Completion (Def_Id);
                  Process_Interface_Name (Def_Id, Arg3, Arg4);
               end if;

               if Is_Compilation_Unit (Hom_Id) then

                  --  Its possible homonyms are not affected by the pragma.
                  --  Such homonyms might be present in the context of other
                  --  units being compiled.

                  exit;

               else
                  Hom_Id := Homonym (Hom_Id);
               end if;
            end loop;

         --  When the convention is Java or CIL, we also allow Import to be
         --  given for packages, generic packages, exceptions, record
         --  components, and access to subprograms.

         elsif (C = Convention_Java or else C = Convention_CIL)
           and then
             (Is_Package_Or_Generic_Package (Def_Id)
               or else Ekind (Def_Id) = E_Exception
               or else Ekind (Def_Id) = E_Access_Subprogram_Type
               or else Nkind (Parent (Def_Id)) = N_Component_Declaration)
         then
            Set_Imported (Def_Id);
            Set_Is_Public (Def_Id);
            Process_Interface_Name (Def_Id, Arg3, Arg4);

         --  Import a CPP class

         elsif C = Convention_CPP
           and then (Is_Record_Type (Def_Id)
                      or else Ekind (Def_Id) = E_Incomplete_Type)
         then
            if Ekind (Def_Id) = E_Incomplete_Type then
               if Present (Full_View (Def_Id)) then
                  Def_Id := Full_View (Def_Id);

               else
                  Error_Msg_N
                    ("cannot import 'C'P'P type before full declaration seen",
                     Get_Pragma_Arg (Arg2));

                  --  Although we have reported the error we decorate it as
                  --  CPP_Class to avoid reporting spurious errors

                  Set_Is_CPP_Class (Def_Id);
                  return;
               end if;
            end if;

            --  Types treated as CPP classes must be declared limited (note:
            --  this used to be a warning but there is no real benefit to it
            --  since we did effectively intend to treat the type as limited
            --  anyway).

            if not Is_Limited_Type (Def_Id) then
               Error_Msg_N
                 ("imported 'C'P'P type must be limited",
                  Get_Pragma_Arg (Arg2));
            end if;

            Set_Is_CPP_Class (Def_Id);

            --  Imported CPP types must not have discriminants (because C++
            --  classes do not have discriminants).

            if Has_Discriminants (Def_Id) then
               Error_Msg_N
                 ("imported 'C'P'P type cannot have discriminants",
                  First (Discriminant_Specifications
                          (Declaration_Node (Def_Id))));
            end if;

            --  Check that components of imported CPP types do not have default
            --  expressions. For private types this check is performed when the
            --  full view is analyzed (see Process_Full_View).

            if not Is_Private_Type (Def_Id) then
               Check_CPP_Type_Has_No_Defaults (Def_Id);
            end if;

         elsif Nkind (Parent (Def_Id)) = N_Incomplete_Type_Declaration then
            Check_No_Link_Name;
            Check_Arg_Count (3);
            Check_Arg_Is_Static_Expression (Arg3, Standard_String);

            Process_Import_Predefined_Type;

         else
            Error_Pragma_Arg
              ("second argument of pragma% must be object, subprogram "
               & "or incomplete type",
               Arg2);
         end if;

         --  If this pragma applies to a compilation unit, then the unit, which
         --  is a subprogram, does not require (or allow) a body. We also do
         --  not need to elaborate imported procedures.

         if Nkind (Parent (N)) = N_Compilation_Unit_Aux then
            declare
               Cunit : constant Node_Id := Parent (Parent (N));
            begin
               Set_Body_Required (Cunit, False);
            end;
         end if;
      end Process_Import_Or_Interface;

      --------------------
      -- Process_Inline --
      --------------------

      procedure Process_Inline (Active : Boolean) is
         Assoc     : Node_Id;
         Decl      : Node_Id;
         Subp_Id   : Node_Id;
         Subp      : Entity_Id;
         Applies   : Boolean;

         Effective : Boolean := False;
         --  Set True if inline has some effect, i.e. if there is at least one
         --  subprogram set as inlined as a result of the use of the pragma.

         procedure Make_Inline (Subp : Entity_Id);
         --  Subp is the defining unit name of the subprogram declaration. Set
         --  the flag, as well as the flag in the corresponding body, if there
         --  is one present.

         procedure Set_Inline_Flags (Subp : Entity_Id);
         --  Sets Is_Inlined and Has_Pragma_Inline flags for Subp and also
         --  Has_Pragma_Inline_Always for the Inline_Always case.

         function Inlining_Not_Possible (Subp : Entity_Id) return Boolean;
         --  Returns True if it can be determined at this stage that inlining
         --  is not possible, for example if the body is available and contains
         --  exception handlers, we prevent inlining, since otherwise we can
         --  get undefined symbols at link time. This function also emits a
         --  warning if front-end inlining is enabled and the pragma appears
         --  too late.
         --
         --  ??? is business with link symbols still valid, or does it relate
         --  to front end ZCX which is being phased out ???

         ---------------------------
         -- Inlining_Not_Possible --
         ---------------------------

         function Inlining_Not_Possible (Subp : Entity_Id) return Boolean is
            Decl  : constant Node_Id := Unit_Declaration_Node (Subp);
            Stats : Node_Id;

         begin
            if Nkind (Decl) = N_Subprogram_Body then
               Stats := Handled_Statement_Sequence (Decl);
               return Present (Exception_Handlers (Stats))
                 or else Present (At_End_Proc (Stats));

            elsif Nkind (Decl) = N_Subprogram_Declaration
              and then Present (Corresponding_Body (Decl))
            then
               if Front_End_Inlining
                 and then Analyzed (Corresponding_Body (Decl))
               then
                  Error_Msg_N ("pragma appears too late, ignored?", N);
                  return True;

               --  If the subprogram is a renaming as body, the body is just a
               --  call to the renamed subprogram, and inlining is trivially
               --  possible.

               elsif
                 Nkind (Unit_Declaration_Node (Corresponding_Body (Decl))) =
                                             N_Subprogram_Renaming_Declaration
               then
                  return False;

               else
                  Stats :=
                    Handled_Statement_Sequence
                        (Unit_Declaration_Node (Corresponding_Body (Decl)));

                  return
                    Present (Exception_Handlers (Stats))
                      or else Present (At_End_Proc (Stats));
               end if;

            else
               --  If body is not available, assume the best, the check is
               --  performed again when compiling enclosing package bodies.

               return False;
            end if;
         end Inlining_Not_Possible;

         -----------------
         -- Make_Inline --
         -----------------

         procedure Make_Inline (Subp : Entity_Id) is
            Kind       : constant Entity_Kind := Ekind (Subp);
            Inner_Subp : Entity_Id   := Subp;

         begin
            --  Ignore if bad type, avoid cascaded error

            if Etype (Subp) = Any_Type then
               Applies := True;
               return;

            --  Ignore if all inlining is suppressed

            elsif Suppress_All_Inlining then
               Applies := True;
               return;

            --  If inlining is not possible, for now do not treat as an error

            elsif Inlining_Not_Possible (Subp) then
               Applies := True;
               return;

            --  Here we have a candidate for inlining, but we must exclude
            --  derived operations. Otherwise we would end up trying to inline
            --  a phantom declaration, and the result would be to drag in a
            --  body which has no direct inlining associated with it. That
            --  would not only be inefficient but would also result in the
            --  backend doing cross-unit inlining in cases where it was
            --  definitely inappropriate to do so.

            --  However, a simple Comes_From_Source test is insufficient, since
            --  we do want to allow inlining of generic instances which also do
            --  not come from source. We also need to recognize specs generated
            --  by the front-end for bodies that carry the pragma. Finally,
            --  predefined operators do not come from source but are not
            --  inlineable either.

            elsif Is_Generic_Instance (Subp)
              or else Nkind (Parent (Parent (Subp))) = N_Subprogram_Declaration
            then
               null;

            elsif not Comes_From_Source (Subp)
              and then Scope (Subp) /= Standard_Standard
            then
               Applies := True;
               return;
            end if;

            --  The referenced entity must either be the enclosing entity, or
            --  an entity declared within the current open scope.

            if Present (Scope (Subp))
              and then Scope (Subp) /= Current_Scope
              and then Subp /= Current_Scope
            then
               Error_Pragma_Arg
                 ("argument of% must be entity in current scope", Assoc);
               return;
            end if;

            --  Processing for procedure, operator or function. If subprogram
            --  is aliased (as for an instance) indicate that the renamed
            --  entity (if declared in the same unit) is inlined.

            if Is_Subprogram (Subp) then
               Inner_Subp := Ultimate_Alias (Inner_Subp);

               if In_Same_Source_Unit (Subp, Inner_Subp) then
                  Set_Inline_Flags (Inner_Subp);

                  Decl := Parent (Parent (Inner_Subp));

                  if Nkind (Decl) = N_Subprogram_Declaration
                    and then Present (Corresponding_Body (Decl))
                  then
                     Set_Inline_Flags (Corresponding_Body (Decl));

                  elsif Is_Generic_Instance (Subp) then

                     --  Indicate that the body needs to be created for
                     --  inlining subsequent calls. The instantiation node
                     --  follows the declaration of the wrapper package
                     --  created for it.

                     if Scope (Subp) /= Standard_Standard
                       and then
                         Need_Subprogram_Instance_Body
                          (Next (Unit_Declaration_Node (Scope (Alias (Subp)))),
                              Subp)
                     then
                        null;
                     end if;

                  --  Inline is a program unit pragma (RM 10.1.5) and cannot
                  --  appear in a formal part to apply to a formal subprogram.
                  --  Do not apply check within an instance or a formal package
                  --  the test will have been applied to the original generic.

                  elsif Nkind (Decl) in N_Formal_Subprogram_Declaration
                    and then List_Containing (Decl) = List_Containing (N)
                    and then not In_Instance
                  then
                     Error_Msg_N
                       ("Inline cannot apply to a formal subprogram", N);
                  end if;
               end if;

               Applies := True;

            --  For a generic subprogram set flag as well, for use at the point
            --  of instantiation, to determine whether the body should be
            --  generated.

            elsif Is_Generic_Subprogram (Subp) then
               Set_Inline_Flags (Subp);
               Applies := True;

            --  Literals are by definition inlined

            elsif Kind = E_Enumeration_Literal then
               null;

            --  Anything else is an error

            else
               Error_Pragma_Arg
                 ("expect subprogram name for pragma%", Assoc);
            end if;
         end Make_Inline;

         ----------------------
         -- Set_Inline_Flags --
         ----------------------

         procedure Set_Inline_Flags (Subp : Entity_Id) is
         begin
            if Active then
               Set_Is_Inlined (Subp);
            end if;

            if not Has_Pragma_Inline (Subp) then
               Set_Has_Pragma_Inline (Subp);
               Effective := True;
            end if;

            if Prag_Id = Pragma_Inline_Always then
               Set_Has_Pragma_Inline_Always (Subp);
            end if;
         end Set_Inline_Flags;

      --  Start of processing for Process_Inline

      begin
         Check_No_Identifiers;
         Check_At_Least_N_Arguments (1);

         if Active then
            Inline_Processing_Required := True;
         end if;

         Assoc := Arg1;
         while Present (Assoc) loop
            Subp_Id := Get_Pragma_Arg (Assoc);
            Analyze (Subp_Id);
            Applies := False;

            if Is_Entity_Name (Subp_Id) then
               Subp := Entity (Subp_Id);

               if Subp = Any_Id then

                  --  If previous error, avoid cascaded errors

                  Applies := True;
                  Effective := True;

               else
                  Make_Inline (Subp);

                  --  For the pragma case, climb homonym chain. This is
                  --  what implements allowing the pragma in the renaming
                  --  case, with the result applying to the ancestors, and
                  --  also allows Inline to apply to all previous homonyms.

                  if not From_Aspect_Specification (N) then
                     while Present (Homonym (Subp))
                       and then Scope (Homonym (Subp)) = Current_Scope
                     loop
                        Make_Inline (Homonym (Subp));
                        Subp := Homonym (Subp);
                     end loop;
                  end if;
               end if;
            end if;

            if not Applies then
               Error_Pragma_Arg
                 ("inappropriate argument for pragma%", Assoc);

            elsif not Effective
              and then Warn_On_Redundant_Constructs
              and then not Suppress_All_Inlining
            then
               if Inlining_Not_Possible (Subp) then
                  Error_Msg_NE
                    ("pragma Inline for& is ignored?", N, Entity (Subp_Id));
               else
                  Error_Msg_NE
                    ("pragma Inline for& is redundant?", N, Entity (Subp_Id));
               end if;
            end if;

            Next (Assoc);
         end loop;
      end Process_Inline;

      ----------------------------
      -- Process_Interface_Name --
      ----------------------------

      procedure Process_Interface_Name
        (Subprogram_Def : Entity_Id;
         Ext_Arg        : Node_Id;
         Link_Arg       : Node_Id)
      is
         Ext_Nam    : Node_Id;
         Link_Nam   : Node_Id;
         String_Val : String_Id;

         procedure Check_Form_Of_Interface_Name
           (SN            : Node_Id;
            Ext_Name_Case : Boolean);
         --  SN is a string literal node for an interface name. This routine
         --  performs some minimal checks that the name is reasonable. In
         --  particular that no spaces or other obviously incorrect characters
         --  appear. This is only a warning, since any characters are allowed.
         --  Ext_Name_Case is True for an External_Name, False for a Link_Name.

         ----------------------------------
         -- Check_Form_Of_Interface_Name --
         ----------------------------------

         procedure Check_Form_Of_Interface_Name
           (SN            : Node_Id;
            Ext_Name_Case : Boolean)
         is
            S  : constant String_Id := Strval (Expr_Value_S (SN));
            SL : constant Nat       := String_Length (S);
            C  : Char_Code;

         begin
            if SL = 0 then
               Error_Msg_N ("interface name cannot be null string", SN);
            end if;

            for J in 1 .. SL loop
               C := Get_String_Char (S, J);

               --  Look for dubious character and issue unconditional warning.
               --  Definitely dubious if not in character range.

               if not In_Character_Range (C)

                  --  For all cases except CLI target,
                  --  commas, spaces and slashes are dubious (in CLI, we use
                  --  commas and backslashes in external names to specify
                  --  assembly version and public key, while slashes and spaces
                  --  can be used in names to mark nested classes and
                  --  valuetypes).

                  or else ((not Ext_Name_Case or else VM_Target /= CLI_Target)
                             and then (Get_Character (C) = ','
                                         or else
                                       Get_Character (C) = '\'))
                 or else (VM_Target /= CLI_Target
                            and then (Get_Character (C) = ' '
                                        or else
                                      Get_Character (C) = '/'))
               then
                  Error_Msg
                    ("?interface name contains illegal character",
                     Sloc (SN) + Source_Ptr (J));
               end if;
            end loop;
         end Check_Form_Of_Interface_Name;

      --  Start of processing for Process_Interface_Name

      begin
         if No (Link_Arg) then
            if No (Ext_Arg) then
               if VM_Target = CLI_Target
                 and then Ekind (Subprogram_Def) = E_Package
                 and then Nkind (Parent (Subprogram_Def)) =
                                                 N_Package_Specification
                 and then Present (Generic_Parent (Parent (Subprogram_Def)))
               then
                  Set_Interface_Name
                     (Subprogram_Def,
                      Interface_Name
                        (Generic_Parent (Parent (Subprogram_Def))));
               end if;

               return;

            elsif Chars (Ext_Arg) = Name_Link_Name then
               Ext_Nam  := Empty;
               Link_Nam := Expression (Ext_Arg);

            else
               Check_Optional_Identifier (Ext_Arg, Name_External_Name);
               Ext_Nam  := Expression (Ext_Arg);
               Link_Nam := Empty;
            end if;

         else
            Check_Optional_Identifier (Ext_Arg,  Name_External_Name);
            Check_Optional_Identifier (Link_Arg, Name_Link_Name);
            Ext_Nam  := Expression (Ext_Arg);
            Link_Nam := Expression (Link_Arg);
         end if;

         --  Check expressions for external name and link name are static

         if Present (Ext_Nam) then
            Check_Arg_Is_Static_Expression (Ext_Nam, Standard_String);
            Check_Form_Of_Interface_Name (Ext_Nam, Ext_Name_Case => True);

            --  Verify that external name is not the name of a local entity,
            --  which would hide the imported one and could lead to run-time
            --  surprises. The problem can only arise for entities declared in
            --  a package body (otherwise the external name is fully qualified
            --  and will not conflict).

            declare
               Nam : Name_Id;
               E   : Entity_Id;
               Par : Node_Id;

            begin
               if Prag_Id = Pragma_Import then
                  String_To_Name_Buffer (Strval (Expr_Value_S (Ext_Nam)));
                  Nam := Name_Find;
                  E   := Entity_Id (Get_Name_Table_Info (Nam));

                  if Nam /= Chars (Subprogram_Def)
                    and then Present (E)
                    and then not Is_Overloadable (E)
                    and then Is_Immediately_Visible (E)
                    and then not Is_Imported (E)
                    and then Ekind (Scope (E)) = E_Package
                  then
                     Par := Parent (E);
                     while Present (Par) loop
                        if Nkind (Par) = N_Package_Body then
                           Error_Msg_Sloc := Sloc (E);
                           Error_Msg_NE
                             ("imported entity is hidden by & declared#",
                              Ext_Arg, E);
                           exit;
                        end if;

                        Par := Parent (Par);
                     end loop;
                  end if;
               end if;
            end;
         end if;

         if Present (Link_Nam) then
            Check_Arg_Is_Static_Expression (Link_Nam, Standard_String);
            Check_Form_Of_Interface_Name (Link_Nam, Ext_Name_Case => False);
         end if;

         --  If there is no link name, just set the external name

         if No (Link_Nam) then
            Link_Nam := Adjust_External_Name_Case (Expr_Value_S (Ext_Nam));

         --  For the Link_Name case, the given literal is preceded by an
         --  asterisk, which indicates to GCC that the given name should be
         --  taken literally, and in particular that no prepending of
         --  underlines should occur, even in systems where this is the
         --  normal default.

         else
            Start_String;

            if VM_Target = No_VM then
               Store_String_Char (Get_Char_Code ('*'));
            end if;

            String_Val := Strval (Expr_Value_S (Link_Nam));
            Store_String_Chars (String_Val);
            Link_Nam :=
              Make_String_Literal (Sloc (Link_Nam),
                Strval => End_String);
         end if;

         --  Set the interface name. If the entity is a generic instance, use
         --  its alias, which is the callable entity.

         if Is_Generic_Instance (Subprogram_Def) then
            Set_Encoded_Interface_Name
              (Alias (Get_Base_Subprogram (Subprogram_Def)), Link_Nam);
         else
            Set_Encoded_Interface_Name
              (Get_Base_Subprogram (Subprogram_Def), Link_Nam);
         end if;

         --  We allow duplicated export names in CIL/Java, as they are always
         --  enclosed in a namespace that differentiates them, and overloaded
         --  entities are supported by the VM.

         if Convention (Subprogram_Def) /= Convention_CIL
              and then
            Convention (Subprogram_Def) /= Convention_Java
         then
            Check_Duplicated_Export_Name (Link_Nam);
         end if;
      end Process_Interface_Name;

      -----------------------------------------
      -- Process_Interrupt_Or_Attach_Handler --
      -----------------------------------------

      procedure Process_Interrupt_Or_Attach_Handler is
         Arg1_X       : constant Node_Id   := Get_Pragma_Arg (Arg1);
         Handler_Proc : constant Entity_Id := Entity (Arg1_X);
         Proc_Scope   : constant Entity_Id := Scope (Handler_Proc);

      begin
         Set_Is_Interrupt_Handler (Handler_Proc);

         --  If the pragma is not associated with a handler procedure within a
         --  protected type, then it must be for a nonprotected procedure for
         --  the AAMP target, in which case we don't associate a representation
         --  item with the procedure's scope.

         if Ekind (Proc_Scope) = E_Protected_Type then
            if Prag_Id = Pragma_Interrupt_Handler
                 or else
               Prag_Id = Pragma_Attach_Handler
            then
               Record_Rep_Item (Proc_Scope, N);
            end if;
         end if;
      end Process_Interrupt_Or_Attach_Handler;

      --------------------------------------------------
      -- Process_Restrictions_Or_Restriction_Warnings --
      --------------------------------------------------

      --  Note: some of the simple identifier cases were handled in par-prag,
      --  but it is harmless (and more straightforward) to simply handle all
      --  cases here, even if it means we repeat a bit of work in some cases.

      procedure Process_Restrictions_Or_Restriction_Warnings
        (Warn : Boolean)
      is
         Arg   : Node_Id;
         R_Id  : Restriction_Id;
         Id    : Name_Id;
         Expr  : Node_Id;
         Val   : Uint;

         procedure Check_Unit_Name (N : Node_Id);
         --  Checks unit name parameter for No_Dependence. Returns if it has
         --  an appropriate form, otherwise raises pragma argument error.

         ---------------------
         -- Check_Unit_Name --
         ---------------------

         procedure Check_Unit_Name (N : Node_Id) is
         begin
            if Nkind (N) = N_Selected_Component then
               Check_Unit_Name (Prefix (N));
               Check_Unit_Name (Selector_Name (N));

            elsif Nkind (N) = N_Identifier then
               return;

            else
               Error_Pragma_Arg
                 ("wrong form for unit name for No_Dependence", N);
            end if;
         end Check_Unit_Name;

      --  Start of processing for Process_Restrictions_Or_Restriction_Warnings

      begin
         --  Ignore all Restrictions pragma in CodePeer mode

         if CodePeer_Mode then
            return;
         end if;

         Check_Ada_83_Warning;
         Check_At_Least_N_Arguments (1);
         Check_Valid_Configuration_Pragma;

         Arg := Arg1;
         while Present (Arg) loop
            Id := Chars (Arg);
            Expr := Get_Pragma_Arg (Arg);

            --  Case of no restriction identifier present

            if Id = No_Name then
               if Nkind (Expr) /= N_Identifier then
                  Error_Pragma_Arg
                    ("invalid form for restriction", Arg);
               end if;

               R_Id :=
                 Get_Restriction_Id
                   (Process_Restriction_Synonyms (Expr));

               if R_Id not in All_Boolean_Restrictions then
                  Error_Msg_Name_1 := Pname;
                  Error_Msg_N
                    ("invalid restriction identifier&", Get_Pragma_Arg (Arg));

                  --  Check for possible misspelling

                  for J in Restriction_Id loop
                     declare
                        Rnm : constant String := Restriction_Id'Image (J);

                     begin
                        Name_Buffer (1 .. Rnm'Length) := Rnm;
                        Name_Len := Rnm'Length;
                        Set_Casing (All_Lower_Case);

                        if Is_Bad_Spelling_Of (Chars (Expr), Name_Enter) then
                           Set_Casing
                             (Identifier_Casing (Current_Source_File));
                           Error_Msg_String (1 .. Rnm'Length) :=
                             Name_Buffer (1 .. Name_Len);
                           Error_Msg_Strlen := Rnm'Length;
                           Error_Msg_N -- CODEFIX
                             ("\possible misspelling of ""~""",
                              Get_Pragma_Arg (Arg));
                           exit;
                        end if;
                     end;
                  end loop;

                  raise Pragma_Exit;
               end if;

               if Implementation_Restriction (R_Id) then
                  Check_Restriction (No_Implementation_Restrictions, Arg);
               end if;

               --  Special processing for No_Elaboration_Code restriction

               if R_Id = No_Elaboration_Code then

                  --  Restriction is only recognized within a configuration
                  --  pragma file, or within a unit of the main extended
                  --  program. Note: the test for Main_Unit is needed to
                  --  properly include the case of configuration pragma files.

                  if not (Current_Sem_Unit = Main_Unit
                           or else In_Extended_Main_Source_Unit (N))
                  then
                     return;

                  --  Don't allow in a subunit unless already specified in
                  --  body or spec.

                  elsif Nkind (Parent (N)) = N_Compilation_Unit
                    and then Nkind (Unit (Parent (N))) = N_Subunit
                    and then not Restriction_Active (No_Elaboration_Code)
                  then
                     Error_Msg_N
                       ("invalid specification of ""No_Elaboration_Code""",
                        N);
                     Error_Msg_N
                       ("\restriction cannot be specified in a subunit", N);
                     Error_Msg_N
                       ("\unless also specified in body or spec", N);
                     return;

                  --  If we have a No_Elaboration_Code pragma that we
                  --  accept, then it needs to be added to the configuration
                  --  restrcition set so that we get proper application to
                  --  other units in the main extended source as required.

                  else
                     Add_To_Config_Boolean_Restrictions (No_Elaboration_Code);
                  end if;
               end if;

               --  If this is a warning, then set the warning unless we already
               --  have a real restriction active (we never want a warning to
               --  override a real restriction).

               if Warn then
                  if not Restriction_Active (R_Id) then
                     Set_Restriction (R_Id, N);
                     Restriction_Warnings (R_Id) := True;
                  end if;

               --  If real restriction case, then set it and make sure that the
               --  restriction warning flag is off, since a real restriction
               --  always overrides a warning.

               else
                  Set_Restriction (R_Id, N);
                  Restriction_Warnings (R_Id) := False;
               end if;

               --  Check for obsolescent restrictions in Ada 2005 mode

               if not Warn
                 and then Ada_Version >= Ada_2005
                 and then (R_Id = No_Asynchronous_Control
                            or else
                           R_Id = No_Unchecked_Deallocation
                            or else
                           R_Id = No_Unchecked_Conversion)
               then
                  Check_Restriction (No_Obsolescent_Features, N);
               end if;

               --  A very special case that must be processed here: pragma
               --  Restrictions (No_Exceptions) turns off all run-time
               --  checking. This is a bit dubious in terms of the formal
               --  language definition, but it is what is intended by RM
               --  H.4(12). Restriction_Warnings never affects generated code
               --  so this is done only in the real restriction case.

               --  Atomic_Synchronization is not a real check, so it is not
               --  affected by this processing).

               if R_Id = No_Exceptions and then not Warn then
                  for J in Scope_Suppress'Range loop
                     if J /= Atomic_Synchronization then
                        Scope_Suppress (J) := True;
                     end if;
                  end loop;
               end if;

            --  Case of No_Dependence => unit-name. Note that the parser
            --  already made the necessary entry in the No_Dependence table.

            elsif Id = Name_No_Dependence then
               Check_Unit_Name (Expr);

            --  Case of No_Specification_Of_Aspect => Identifier.

            elsif Id = Name_No_Specification_Of_Aspect then
               declare
                  A_Id : Aspect_Id;

               begin
                  if Nkind (Expr) /= N_Identifier then
                     A_Id := No_Aspect;
                  else
                     A_Id := Get_Aspect_Id (Chars (Expr));
                  end if;

                  if A_Id = No_Aspect then
                     Error_Pragma_Arg ("invalid restriction name", Arg);
                  else
                     Set_Restriction_No_Specification_Of_Aspect (Expr, Warn);
                  end if;
               end;

            --  All other cases of restriction identifier present

            else
               R_Id := Get_Restriction_Id (Process_Restriction_Synonyms (Arg));
               Analyze_And_Resolve (Expr, Any_Integer);

               if R_Id not in All_Parameter_Restrictions then
                  Error_Pragma_Arg
                    ("invalid restriction parameter identifier", Arg);

               elsif not Is_OK_Static_Expression (Expr) then
                  Flag_Non_Static_Expr
                    ("value must be static expression!", Expr);
                  raise Pragma_Exit;

               elsif not Is_Integer_Type (Etype (Expr))
                 or else Expr_Value (Expr) < 0
               then
                  Error_Pragma_Arg
                    ("value must be non-negative integer", Arg);
               end if;

               --  Restriction pragma is active

               Val := Expr_Value (Expr);

               if not UI_Is_In_Int_Range (Val) then
                  Error_Pragma_Arg
                    ("pragma ignored, value too large?", Arg);
               end if;

               --  Warning case. If the real restriction is active, then we
               --  ignore the request, since warning never overrides a real
               --  restriction. Otherwise we set the proper warning. Note that
               --  this circuit sets the warning again if it is already set,
               --  which is what we want, since the constant may have changed.

               if Warn then
                  if not Restriction_Active (R_Id) then
                     Set_Restriction
                       (R_Id, N, Integer (UI_To_Int (Val)));
                     Restriction_Warnings (R_Id) := True;
                  end if;

               --  Real restriction case, set restriction and make sure warning
               --  flag is off since real restriction always overrides warning.

               else
                  Set_Restriction (R_Id, N, Integer (UI_To_Int (Val)));
                  Restriction_Warnings (R_Id) := False;
               end if;
            end if;

            Next (Arg);
         end loop;
      end Process_Restrictions_Or_Restriction_Warnings;

      ---------------------------------
      -- Process_Suppress_Unsuppress --
      ---------------------------------

      --  Note: this procedure makes entries in the check suppress data
      --  structures managed by Sem. See spec of package Sem for full
      --  details on how we handle recording of check suppression.

      procedure Process_Suppress_Unsuppress (Suppress_Case : Boolean) is
         C    : Check_Id;
         E_Id : Node_Id;
         E    : Entity_Id;

         In_Package_Spec : constant Boolean :=
                             Is_Package_Or_Generic_Package (Current_Scope)
                               and then not In_Package_Body (Current_Scope);

         procedure Suppress_Unsuppress_Echeck (E : Entity_Id; C : Check_Id);
         --  Used to suppress a single check on the given entity

         --------------------------------
         -- Suppress_Unsuppress_Echeck --
         --------------------------------

         procedure Suppress_Unsuppress_Echeck (E : Entity_Id; C : Check_Id) is
         begin
            --  Check for error of trying to set atomic synchronization for
            --  a non-atomic variable.

            if C = Atomic_Synchronization
              and then not (Is_Atomic (E) or else Has_Atomic_Components (E))
            then
               Error_Msg_N
                 ("pragma & requires atomic type or variable",
                  Pragma_Identifier (Original_Node (N)));
            end if;

            Set_Checks_May_Be_Suppressed (E);

            if In_Package_Spec then
               Push_Global_Suppress_Stack_Entry
                 (Entity   => E,
                  Check    => C,
                  Suppress => Suppress_Case);
            else
               Push_Local_Suppress_Stack_Entry
                 (Entity   => E,
                  Check    => C,
                  Suppress => Suppress_Case);
            end if;

            --  If this is a first subtype, and the base type is distinct,
            --  then also set the suppress flags on the base type.

            if Is_First_Subtype (E)
              and then Etype (E) /= E
            then
               Suppress_Unsuppress_Echeck (Etype (E), C);
            end if;
         end Suppress_Unsuppress_Echeck;

      --  Start of processing for Process_Suppress_Unsuppress

      begin
         --  Ignore pragma Suppress/Unsuppress in CodePeer and Alfa modes on
         --  user code: we want to generate checks for analysis purposes, as
         --  set respectively by -gnatC and -gnatd.F

         if (CodePeer_Mode or Alfa_Mode)
           and then Comes_From_Source (N)
         then
            return;
         end if;

         --  Suppress/Unsuppress can appear as a configuration pragma, or in a
         --  declarative part or a package spec (RM 11.5(5)).

         if not Is_Configuration_Pragma then
            Check_Is_In_Decl_Part_Or_Package_Spec;
         end if;

         Check_At_Least_N_Arguments (1);
         Check_At_Most_N_Arguments (2);
         Check_No_Identifier (Arg1);
         Check_Arg_Is_Identifier (Arg1);

         C := Get_Check_Id (Chars (Get_Pragma_Arg (Arg1)));

         if C = No_Check_Id then
            Error_Pragma_Arg
              ("argument of pragma% is not valid check name", Arg1);
         end if;

         if not Suppress_Case
           and then (C = All_Checks or else C = Overflow_Check)
         then
            Opt.Overflow_Checks_Unsuppressed := True;
         end if;

         if Arg_Count = 1 then

            --  Make an entry in the local scope suppress table. This is the
            --  table that directly shows the current value of the scope
            --  suppress check for any check id value.

            if C = All_Checks then

               --  For All_Checks, we set all specific predefined checks with
               --  the exception of Elaboration_Check, which is handled
               --  specially because of not wanting All_Checks to have the
               --  effect of deactivating static elaboration order processing.
               --  Atomic_Synchronization is also not affected, since this is
               --  not a real check.

               for J in Scope_Suppress'Range loop
                  if J /= Elaboration_Check
                    and then J /= Atomic_Synchronization
                  then
                     Scope_Suppress (J) := Suppress_Case;
                  end if;
               end loop;

            --  If not All_Checks, and predefined check, then set appropriate
            --  scope entry. Note that we will set Elaboration_Check if this
            --  is explicitly specified. Atomic_Synchronization is allowed
            --  only if internally generated and entity is atomic.

            elsif C in Predefined_Check_Id
              and then (not Comes_From_Source (N)
                         or else C /= Atomic_Synchronization)
            then
               Scope_Suppress (C) := Suppress_Case;
            end if;

            --  Also make an entry in the Local_Entity_Suppress table

            Push_Local_Suppress_Stack_Entry
              (Entity   => Empty,
               Check    => C,
               Suppress => Suppress_Case);

         --  Case of two arguments present, where the check is suppressed for
         --  a specified entity (given as the second argument of the pragma)

         else
            --  This is obsolescent in Ada 2005 mode

            if Ada_Version >= Ada_2005 then
               Check_Restriction (No_Obsolescent_Features, Arg2);
            end if;

            Check_Optional_Identifier (Arg2, Name_On);
            E_Id := Get_Pragma_Arg (Arg2);
            Analyze (E_Id);

            if not Is_Entity_Name (E_Id) then
               Error_Pragma_Arg
                 ("second argument of pragma% must be entity name", Arg2);
            end if;

            E := Entity (E_Id);

            if E = Any_Id then
               return;
            end if;

            --  Enforce RM 11.5(7) which requires that for a pragma that
            --  appears within a package spec, the named entity must be
            --  within the package spec. We allow the package name itself
            --  to be mentioned since that makes sense, although it is not
            --  strictly allowed by 11.5(7).

            if In_Package_Spec
              and then E /= Current_Scope
              and then Scope (E) /= Current_Scope
            then
               Error_Pragma_Arg
                 ("entity in pragma% is not in package spec (RM 11.5(7))",
                  Arg2);
            end if;

            --  Loop through homonyms. As noted below, in the case of a package
            --  spec, only homonyms within the package spec are considered.

            loop
               Suppress_Unsuppress_Echeck (E, C);

               if Is_Generic_Instance (E)
                 and then Is_Subprogram (E)
                 and then Present (Alias (E))
               then
                  Suppress_Unsuppress_Echeck (Alias (E), C);
               end if;

               --  Move to next homonym if not aspect spec case

               exit when From_Aspect_Specification (N);
               E := Homonym (E);
               exit when No (E);

               --  If we are within a package specification, the pragma only
               --  applies to homonyms in the same scope.

               exit when In_Package_Spec
                 and then Scope (E) /= Current_Scope;
            end loop;
         end if;
      end Process_Suppress_Unsuppress;

      ------------------
      -- Set_Exported --
      ------------------

      procedure Set_Exported (E : Entity_Id; Arg : Node_Id) is
      begin
         if Is_Imported (E) then
            Error_Pragma_Arg
              ("cannot export entity& that was previously imported", Arg);

         elsif Present (Address_Clause (E)) and then not CodePeer_Mode then
            Error_Pragma_Arg
              ("cannot export entity& that has an address clause", Arg);
         end if;

         Set_Is_Exported (E);

         --  Generate a reference for entity explicitly, because the
         --  identifier may be overloaded and name resolution will not
         --  generate one.

         Generate_Reference (E, Arg);

         --  Deal with exporting non-library level entity

         if not Is_Library_Level_Entity (E) then

            --  Not allowed at all for subprograms

            if Is_Subprogram (E) then
               Error_Pragma_Arg ("local subprogram& cannot be exported", Arg);

            --  Otherwise set public and statically allocated

            else
               Set_Is_Public (E);
               Set_Is_Statically_Allocated (E);

               --  Warn if the corresponding W flag is set and the pragma comes
               --  from source. The latter may not be true e.g. on VMS where we
               --  expand export pragmas for exception codes associated with
               --  imported or exported exceptions. We do not want to generate
               --  a warning for something that the user did not write.

               if Warn_On_Export_Import
                 and then Comes_From_Source (Arg)
               then
                  Error_Msg_NE
                    ("?& has been made static as a result of Export", Arg, E);
                  Error_Msg_N
                    ("\this usage is non-standard and non-portable", Arg);
               end if;
            end if;
         end if;

         if Warn_On_Export_Import and then Is_Type (E) then
            Error_Msg_NE ("exporting a type has no effect?", Arg, E);
         end if;

         if Warn_On_Export_Import and Inside_A_Generic then
            Error_Msg_NE
              ("all instances of& will have the same external name?", Arg, E);
         end if;
      end Set_Exported;

      ----------------------------------------------
      -- Set_Extended_Import_Export_External_Name --
      ----------------------------------------------

      procedure Set_Extended_Import_Export_External_Name
        (Internal_Ent : Entity_Id;
         Arg_External : Node_Id)
      is
         Old_Name : constant Node_Id := Interface_Name (Internal_Ent);
         New_Name : Node_Id;

      begin
         if No (Arg_External) then
            return;
         end if;

         Check_Arg_Is_External_Name (Arg_External);

         if Nkind (Arg_External) = N_String_Literal then
            if String_Length (Strval (Arg_External)) = 0 then
               return;
            else
               New_Name := Adjust_External_Name_Case (Arg_External);
            end if;

         elsif Nkind (Arg_External) = N_Identifier then
            New_Name := Get_Default_External_Name (Arg_External);

         --  Check_Arg_Is_External_Name should let through only identifiers and
         --  string literals or static string expressions (which are folded to
         --  string literals).

         else
            raise Program_Error;
         end if;

         --  If we already have an external name set (by a prior normal Import
         --  or Export pragma), then the external names must match

         if Present (Interface_Name (Internal_Ent)) then
            Check_Matching_Internal_Names : declare
               S1 : constant String_Id := Strval (Old_Name);
               S2 : constant String_Id := Strval (New_Name);

               procedure Mismatch;
               --  Called if names do not match

               --------------
               -- Mismatch --
               --------------

               procedure Mismatch is
               begin
                  Error_Msg_Sloc := Sloc (Old_Name);
                  Error_Pragma_Arg
                    ("external name does not match that given #",
                     Arg_External);
               end Mismatch;

            --  Start of processing for Check_Matching_Internal_Names

            begin
               if String_Length (S1) /= String_Length (S2) then
                  Mismatch;

               else
                  for J in 1 .. String_Length (S1) loop
                     if Get_String_Char (S1, J) /= Get_String_Char (S2, J) then
                        Mismatch;
                     end if;
                  end loop;
               end if;
            end Check_Matching_Internal_Names;

         --  Otherwise set the given name

         else
            Set_Encoded_Interface_Name (Internal_Ent, New_Name);
            Check_Duplicated_Export_Name (New_Name);
         end if;
      end Set_Extended_Import_Export_External_Name;

      ------------------
      -- Set_Imported --
      ------------------

      procedure Set_Imported (E : Entity_Id) is
      begin
         --  Error message if already imported or exported

         if Is_Exported (E) or else Is_Imported (E) then

            --  Error if being set Exported twice

            if Is_Exported (E) then
               Error_Msg_NE ("entity& was previously exported", N, E);

            --  OK if Import/Interface case

            elsif Import_Interface_Present (N) then
               goto OK;

            --  Error if being set Imported twice

            else
               Error_Msg_NE ("entity& was previously imported", N, E);
            end if;

            Error_Msg_Name_1 := Pname;
            Error_Msg_N
              ("\(pragma% applies to all previous entities)", N);

            Error_Msg_Sloc  := Sloc (E);
            Error_Msg_NE ("\import not allowed for& declared#", N, E);

         --  Here if not previously imported or exported, OK to import

         else
            Set_Is_Imported (E);

            --  If the entity is an object that is not at the library level,
            --  then it is statically allocated. We do not worry about objects
            --  with address clauses in this context since they are not really
            --  imported in the linker sense.

            if Is_Object (E)
              and then not Is_Library_Level_Entity (E)
              and then No (Address_Clause (E))
            then
               Set_Is_Statically_Allocated (E);
            end if;
         end if;

         <<OK>> null;
      end Set_Imported;

      -------------------------
      -- Set_Mechanism_Value --
      -------------------------

      --  Note: the mechanism name has not been analyzed (and cannot indeed be
      --  analyzed, since it is semantic nonsense), so we get it in the exact
      --  form created by the parser.

      procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id) is
         Class        : Node_Id;
         Param        : Node_Id;
         Mech_Name_Id : Name_Id;

         procedure Bad_Class;
         --  Signal bad descriptor class name

         procedure Bad_Mechanism;
         --  Signal bad mechanism name

         ---------------
         -- Bad_Class --
         ---------------

         procedure Bad_Class is
         begin
            Error_Pragma_Arg ("unrecognized descriptor class name", Class);
         end Bad_Class;

         -------------------------
         -- Bad_Mechanism_Value --
         -------------------------

         procedure Bad_Mechanism is
         begin
            Error_Pragma_Arg ("unrecognized mechanism name", Mech_Name);
         end Bad_Mechanism;

      --  Start of processing for Set_Mechanism_Value

      begin
         if Mechanism (Ent) /= Default_Mechanism then
            Error_Msg_NE
              ("mechanism for & has already been set", Mech_Name, Ent);
         end if;

         --  MECHANISM_NAME ::= value | reference | descriptor |
         --                     short_descriptor

         if Nkind (Mech_Name) = N_Identifier then
            if Chars (Mech_Name) = Name_Value then
               Set_Mechanism (Ent, By_Copy);
               return;

            elsif Chars (Mech_Name) = Name_Reference then
               Set_Mechanism (Ent, By_Reference);
               return;

            elsif Chars (Mech_Name) = Name_Descriptor then
               Check_VMS (Mech_Name);

               --  Descriptor => Short_Descriptor if pragma was given

               if Short_Descriptors then
                  Set_Mechanism (Ent, By_Short_Descriptor);
               else
                  Set_Mechanism (Ent, By_Descriptor);
               end if;

               return;

            elsif Chars (Mech_Name) = Name_Short_Descriptor then
               Check_VMS (Mech_Name);
               Set_Mechanism (Ent, By_Short_Descriptor);
               return;

            elsif Chars (Mech_Name) = Name_Copy then
               Error_Pragma_Arg
                 ("bad mechanism name, Value assumed", Mech_Name);

            else
               Bad_Mechanism;
            end if;

         --  MECHANISM_NAME ::= descriptor (CLASS_NAME) |
         --                     short_descriptor (CLASS_NAME)
         --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

         --  Note: this form is parsed as an indexed component

         elsif Nkind (Mech_Name) = N_Indexed_Component then
            Class := First (Expressions (Mech_Name));

            if Nkind (Prefix (Mech_Name)) /= N_Identifier
             or else not (Chars (Prefix (Mech_Name)) = Name_Descriptor or else
                          Chars (Prefix (Mech_Name)) = Name_Short_Descriptor)
             or else Present (Next (Class))
            then
               Bad_Mechanism;
            else
               Mech_Name_Id := Chars (Prefix (Mech_Name));

               --  Change Descriptor => Short_Descriptor if pragma was given

               if Mech_Name_Id = Name_Descriptor
                 and then Short_Descriptors
               then
                  Mech_Name_Id := Name_Short_Descriptor;
               end if;
            end if;

         --  MECHANISM_NAME ::= descriptor (Class => CLASS_NAME) |
         --                     short_descriptor (Class => CLASS_NAME)
         --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

         --  Note: this form is parsed as a function call

         elsif Nkind (Mech_Name) = N_Function_Call then
            Param := First (Parameter_Associations (Mech_Name));

            if Nkind (Name (Mech_Name)) /= N_Identifier
              or else not (Chars (Name (Mech_Name)) = Name_Descriptor or else
                           Chars (Name (Mech_Name)) = Name_Short_Descriptor)
              or else Present (Next (Param))
              or else No (Selector_Name (Param))
              or else Chars (Selector_Name (Param)) /= Name_Class
            then
               Bad_Mechanism;
            else
               Class := Explicit_Actual_Parameter (Param);
               Mech_Name_Id := Chars (Name (Mech_Name));
            end if;

         else
            Bad_Mechanism;
         end if;

         --  Fall through here with Class set to descriptor class name

         Check_VMS (Mech_Name);

         if Nkind (Class) /= N_Identifier then
            Bad_Class;

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_UBS
         then
            Set_Mechanism (Ent, By_Descriptor_UBS);

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_UBSB
         then
            Set_Mechanism (Ent, By_Descriptor_UBSB);

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_UBA
         then
            Set_Mechanism (Ent, By_Descriptor_UBA);

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_S
         then
            Set_Mechanism (Ent, By_Descriptor_S);

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_SB
         then
            Set_Mechanism (Ent, By_Descriptor_SB);

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_A
         then
            Set_Mechanism (Ent, By_Descriptor_A);

         elsif Mech_Name_Id = Name_Descriptor
           and then Chars (Class) = Name_NCA
         then
            Set_Mechanism (Ent, By_Descriptor_NCA);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_UBS
         then
            Set_Mechanism (Ent, By_Short_Descriptor_UBS);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_UBSB
         then
            Set_Mechanism (Ent, By_Short_Descriptor_UBSB);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_UBA
         then
            Set_Mechanism (Ent, By_Short_Descriptor_UBA);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_S
         then
            Set_Mechanism (Ent, By_Short_Descriptor_S);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_SB
         then
            Set_Mechanism (Ent, By_Short_Descriptor_SB);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_A
         then
            Set_Mechanism (Ent, By_Short_Descriptor_A);

         elsif Mech_Name_Id = Name_Short_Descriptor
           and then Chars (Class) = Name_NCA
         then
            Set_Mechanism (Ent, By_Short_Descriptor_NCA);

         else
            Bad_Class;
         end if;
      end Set_Mechanism_Value;

      ---------------------------
      -- Set_Ravenscar_Profile --
      ---------------------------

      --  The tasks to be done here are

      --    Set required policies

      --      pragma Task_Dispatching_Policy (FIFO_Within_Priorities)
      --      pragma Locking_Policy (Ceiling_Locking)

      --    Set Detect_Blocking mode

      --    Set required restrictions (see System.Rident for detailed list)

      --    Set the No_Dependence rules
      --      No_Dependence => Ada.Asynchronous_Task_Control
      --      No_Dependence => Ada.Calendar
      --      No_Dependence => Ada.Execution_Time.Group_Budget
      --      No_Dependence => Ada.Execution_Time.Timers
      --      No_Dependence => Ada.Task_Attributes
      --      No_Dependence => System.Multiprocessors.Dispatching_Domains

      procedure Set_Ravenscar_Profile (N : Node_Id) is
         Prefix_Entity   : Entity_Id;
         Selector_Entity : Entity_Id;
         Prefix_Node     : Node_Id;
         Node            : Node_Id;

      begin
         --  pragma Task_Dispatching_Policy (FIFO_Within_Priorities)

         if Task_Dispatching_Policy /= ' '
           and then Task_Dispatching_Policy /= 'F'
         then
            Error_Msg_Sloc := Task_Dispatching_Policy_Sloc;
            Error_Pragma ("Profile (Ravenscar) incompatible with policy#");

         --  Set the FIFO_Within_Priorities policy, but always preserve
         --  System_Location since we like the error message with the run time
         --  name.

         else
            Task_Dispatching_Policy := 'F';

            if Task_Dispatching_Policy_Sloc /= System_Location then
               Task_Dispatching_Policy_Sloc := Loc;
            end if;
         end if;

         --  pragma Locking_Policy (Ceiling_Locking)

         if Locking_Policy /= ' '
           and then Locking_Policy /= 'C'
         then
            Error_Msg_Sloc := Locking_Policy_Sloc;
            Error_Pragma ("Profile (Ravenscar) incompatible with policy#");

         --  Set the Ceiling_Locking policy, but preserve System_Location since
         --  we like the error message with the run time name.

         else
            Locking_Policy := 'C';

            if Locking_Policy_Sloc /= System_Location then
               Locking_Policy_Sloc := Loc;
            end if;
         end if;

         --  pragma Detect_Blocking

         Detect_Blocking := True;

         --  Set the corresponding restrictions

         Set_Profile_Restrictions
           (Ravenscar, N, Warn => Treat_Restrictions_As_Warnings);

         --  Set the No_Dependence restrictions

         --  The following No_Dependence restrictions:
         --    No_Dependence => Ada.Asynchronous_Task_Control
         --    No_Dependence => Ada.Calendar
         --    No_Dependence => Ada.Task_Attributes
         --  are already set by previous call to Set_Profile_Restrictions.

         --  Set the following restrictions which were added to Ada 2005:
         --    No_Dependence => Ada.Execution_Time.Group_Budget
         --    No_Dependence => Ada.Execution_Time.Timers

         if Ada_Version >= Ada_2005 then
            Name_Buffer (1 .. 3) := "ada";
            Name_Len := 3;

            Prefix_Entity := Make_Identifier (Loc, Name_Find);

            Name_Buffer (1 .. 14) := "execution_time";
            Name_Len := 14;

            Selector_Entity := Make_Identifier (Loc, Name_Find);

            Prefix_Node :=
              Make_Selected_Component
                (Sloc          => Loc,
                 Prefix        => Prefix_Entity,
                 Selector_Name => Selector_Entity);

            Name_Buffer (1 .. 13) := "group_budgets";
            Name_Len := 13;

            Selector_Entity := Make_Identifier (Loc, Name_Find);

            Node :=
              Make_Selected_Component
                (Sloc          => Loc,
                 Prefix        => Prefix_Node,
                 Selector_Name => Selector_Entity);

            Set_Restriction_No_Dependence
              (Unit    => Node,
               Warn    => Treat_Restrictions_As_Warnings,
               Profile => Ravenscar);

            Name_Buffer (1 .. 6) := "timers";
            Name_Len := 6;

            Selector_Entity := Make_Identifier (Loc, Name_Find);

            Node :=
              Make_Selected_Component
                (Sloc          => Loc,
                 Prefix        => Prefix_Node,
                 Selector_Name => Selector_Entity);

            Set_Restriction_No_Dependence
              (Unit    => Node,
               Warn    => Treat_Restrictions_As_Warnings,
               Profile => Ravenscar);
         end if;

         --  Set the following restrictions which was added to Ada 2012 (see
         --  AI-0171):
         --    No_Dependence => System.Multiprocessors.Dispatching_Domains

         if Ada_Version >= Ada_2012 then
            Name_Buffer (1 .. 6) := "system";
            Name_Len := 6;

            Prefix_Entity := Make_Identifier (Loc, Name_Find);

            Name_Buffer (1 .. 15) := "multiprocessors";
            Name_Len := 15;

            Selector_Entity := Make_Identifier (Loc, Name_Find);

            Prefix_Node :=
              Make_Selected_Component
                (Sloc          => Loc,
                 Prefix        => Prefix_Entity,
                 Selector_Name => Selector_Entity);

            Name_Buffer (1 .. 19) := "dispatching_domains";
            Name_Len := 19;

            Selector_Entity := Make_Identifier (Loc, Name_Find);

            Node :=
              Make_Selected_Component
                (Sloc          => Loc,
                 Prefix        => Prefix_Node,
                 Selector_Name => Selector_Entity);

            Set_Restriction_No_Dependence
              (Unit    => Node,
               Warn    => Treat_Restrictions_As_Warnings,
               Profile => Ravenscar);
         end if;
      end Set_Ravenscar_Profile;

   --  Start of processing for Analyze_Pragma

   begin
      --  The following code is a defense against recursion. Not clear that
      --  this can happen legitimately, but perhaps some error situations
      --  can cause it, and we did see this recursion during testing.

      if Analyzed (N) then
         return;
      else
         Set_Analyzed (N, True);
      end if;

      --  Deal with unrecognized pragma

      Pname := Pragma_Name (N);

      if not Is_Pragma_Name (Pname) then
         if Warn_On_Unrecognized_Pragma then
            Error_Msg_Name_1 := Pname;
            Error_Msg_N ("?unrecognized pragma%!", Pragma_Identifier (N));

            for PN in First_Pragma_Name .. Last_Pragma_Name loop
               if Is_Bad_Spelling_Of (Pname, PN) then
                  Error_Msg_Name_1 := PN;
                  Error_Msg_N -- CODEFIX
                    ("\?possible misspelling of %!", Pragma_Identifier (N));
                  exit;
               end if;
            end loop;
         end if;

         return;
      end if;

      --  Here to start processing for recognized pragma

      Prag_Id := Get_Pragma_Id (Pname);

      if Present (Corresponding_Aspect (N)) then
         Pname := Chars (Identifier (Corresponding_Aspect (N)));
      end if;

      --  Preset arguments

      Arg_Count := 0;
      Arg1      := Empty;
      Arg2      := Empty;
      Arg3      := Empty;
      Arg4      := Empty;

      if Present (Pragma_Argument_Associations (N)) then
         Arg_Count := List_Length (Pragma_Argument_Associations (N));
         Arg1 := First (Pragma_Argument_Associations (N));

         if Present (Arg1) then
            Arg2 := Next (Arg1);

            if Present (Arg2) then
               Arg3 := Next (Arg2);

               if Present (Arg3) then
                  Arg4 := Next (Arg3);
               end if;
            end if;
         end if;
      end if;

      --  An enumeration type defines the pragmas that are supported by the
      --  implementation. Get_Pragma_Id (in package Prag) transforms a name
      --  into the corresponding enumeration value for the following case.

      case Prag_Id is

         -----------------
         -- Abort_Defer --
         -----------------

         --  pragma Abort_Defer;

         when Pragma_Abort_Defer =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            --  The only required semantic processing is to check the
            --  placement. This pragma must appear at the start of the
            --  statement sequence of a handled sequence of statements.

            if Nkind (Parent (N)) /= N_Handled_Sequence_Of_Statements
              or else N /= First (Statements (Parent (N)))
            then
               Pragma_Misplaced;
            end if;

         ------------
         -- Ada_83 --
         ------------

         --  pragma Ada_83;

         --  Note: this pragma also has some specific processing in Par.Prag
         --  because we want to set the Ada version mode during parsing.

         when Pragma_Ada_83 =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            --  We really should check unconditionally for proper configuration
            --  pragma placement, since we really don't want mixed Ada modes
            --  within a single unit, and the GNAT reference manual has always
            --  said this was a configuration pragma, but we did not check and
            --  are hesitant to add the check now.

            --  However, we really cannot tolerate mixing Ada 2005 or Ada 2012
            --  with Ada 83 or Ada 95, so we must check if we are in Ada 2005
            --  or Ada 2012 mode.

            if Ada_Version >= Ada_2005 then
               Check_Valid_Configuration_Pragma;
            end if;

            --  Now set Ada 83 mode

            Ada_Version := Ada_83;
            Ada_Version_Explicit := Ada_Version;

         ------------
         -- Ada_95 --
         ------------

         --  pragma Ada_95;

         --  Note: this pragma also has some specific processing in Par.Prag
         --  because we want to set the Ada 83 version mode during parsing.

         when Pragma_Ada_95 =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            --  We really should check unconditionally for proper configuration
            --  pragma placement, since we really don't want mixed Ada modes
            --  within a single unit, and the GNAT reference manual has always
            --  said this was a configuration pragma, but we did not check and
            --  are hesitant to add the check now.

            --  However, we really cannot tolerate mixing Ada 2005 with Ada 83
            --  or Ada 95, so we must check if we are in Ada 2005 mode.

            if Ada_Version >= Ada_2005 then
               Check_Valid_Configuration_Pragma;
            end if;

            --  Now set Ada 95 mode

            Ada_Version := Ada_95;
            Ada_Version_Explicit := Ada_Version;

         ---------------------
         -- Ada_05/Ada_2005 --
         ---------------------

         --  pragma Ada_05;
         --  pragma Ada_05 (LOCAL_NAME);

         --  pragma Ada_2005;
         --  pragma Ada_2005 (LOCAL_NAME):

         --  Note: these pragmas also have some specific processing in Par.Prag
         --  because we want to set the Ada 2005 version mode during parsing.

         when Pragma_Ada_05 | Pragma_Ada_2005 => declare
            E_Id : Node_Id;

         begin
            GNAT_Pragma;

            if Arg_Count = 1 then
               Check_Arg_Is_Local_Name (Arg1);
               E_Id := Get_Pragma_Arg (Arg1);

               if Etype (E_Id) = Any_Type then
                  return;
               end if;

               Set_Is_Ada_2005_Only (Entity (E_Id));

            else
               Check_Arg_Count (0);

               --  For Ada_2005 we unconditionally enforce the documented
               --  configuration pragma placement, since we do not want to
               --  tolerate mixed modes in a unit involving Ada 2005. That
               --  would cause real difficulties for those cases where there
               --  are incompatibilities between Ada 95 and Ada 2005.

               Check_Valid_Configuration_Pragma;

               --  Now set appropriate Ada mode

               Ada_Version          := Ada_2005;
               Ada_Version_Explicit := Ada_2005;
            end if;
         end;

         ---------------------
         -- Ada_12/Ada_2012 --
         ---------------------

         --  pragma Ada_12;
         --  pragma Ada_12 (LOCAL_NAME);

         --  pragma Ada_2012;
         --  pragma Ada_2012 (LOCAL_NAME):

         --  Note: these pragmas also have some specific processing in Par.Prag
         --  because we want to set the Ada 2012 version mode during parsing.

         when Pragma_Ada_12 | Pragma_Ada_2012 => declare
            E_Id : Node_Id;

         begin
            GNAT_Pragma;

            if Arg_Count = 1 then
               Check_Arg_Is_Local_Name (Arg1);
               E_Id := Get_Pragma_Arg (Arg1);

               if Etype (E_Id) = Any_Type then
                  return;
               end if;

               Set_Is_Ada_2012_Only (Entity (E_Id));

            else
               Check_Arg_Count (0);

               --  For Ada_2012 we unconditionally enforce the documented
               --  configuration pragma placement, since we do not want to
               --  tolerate mixed modes in a unit involving Ada 2012. That
               --  would cause real difficulties for those cases where there
               --  are incompatibilities between Ada 95 and Ada 2012. We could
               --  allow mixing of Ada 2005 and Ada 2012 but it's not worth it.

               Check_Valid_Configuration_Pragma;

               --  Now set appropriate Ada mode

               Ada_Version          := Ada_2012;
               Ada_Version_Explicit := Ada_2012;
            end if;
         end;

         ----------------------
         -- All_Calls_Remote --
         ----------------------

         --  pragma All_Calls_Remote [(library_package_NAME)];

         when Pragma_All_Calls_Remote => All_Calls_Remote : declare
            Lib_Entity : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Lib_Entity := Find_Lib_Unit_Name;

            --  This pragma should only apply to a RCI unit (RM E.2.3(23))

            if Present (Lib_Entity)
              and then not Debug_Flag_U
            then
               if not Is_Remote_Call_Interface (Lib_Entity) then
                  Error_Pragma ("pragma% only apply to rci unit");

               --  Set flag for entity of the library unit

               else
                  Set_Has_All_Calls_Remote (Lib_Entity);
               end if;

            end if;
         end All_Calls_Remote;

         --------------
         -- Annotate --
         --------------

         --  pragma Annotate (IDENTIFIER [, IDENTIFIER {, ARG}]);
         --  ARG ::= NAME | EXPRESSION

         --  The first two arguments are by convention intended to refer to an
         --  external tool and a tool-specific function. These arguments are
         --  not analyzed.

         when Pragma_Annotate => Annotate : declare
            Arg : Node_Id;
            Exp : Node_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_Arg_Is_Identifier (Arg1);
            Check_No_Identifiers;
            Store_Note (N);

            --  Second parameter is optional, it is never analyzed

            if No (Arg2) then
               null;

            --  Here if we have a second parameter

            else
               --  Second parameter must be identifier

               Check_Arg_Is_Identifier (Arg2);

               --  Process remaining parameters if any

               Arg := Next (Arg2);
               while Present (Arg) loop
                  Exp := Get_Pragma_Arg (Arg);
                  Analyze (Exp);

                  if Is_Entity_Name (Exp) then
                     null;

                  --  For string literals, we assume Standard_String as the
                  --  type, unless the string contains wide or wide_wide
                  --  characters.

                  elsif Nkind (Exp) = N_String_Literal then
                     if Has_Wide_Wide_Character (Exp) then
                        Resolve (Exp, Standard_Wide_Wide_String);
                     elsif Has_Wide_Character (Exp) then
                        Resolve (Exp, Standard_Wide_String);
                     else
                        Resolve (Exp, Standard_String);
                     end if;

                  elsif Is_Overloaded (Exp) then
                        Error_Pragma_Arg
                          ("ambiguous argument for pragma%", Exp);

                  else
                     Resolve (Exp);
                  end if;

                  Next (Arg);
               end loop;
            end if;
         end Annotate;

         ------------
         -- Assert --
         ------------

         --  pragma Assert ([Check =>] Boolean_EXPRESSION
         --                 [, [Message =>] Static_String_EXPRESSION]);

         when Pragma_Assert => Assert : declare
            Expr : Node_Id;
            Newa : List_Id;

         begin
            Ada_2005_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_At_Most_N_Arguments (2);
            Check_Arg_Order ((Name_Check, Name_Message));
            Check_Optional_Identifier (Arg1, Name_Check);

            --  We treat pragma Assert as equivalent to:

            --    pragma Check (Assertion, condition [, msg]);

            --  So rewrite pragma in this manner, and analyze the result

            Expr := Get_Pragma_Arg (Arg1);
            Newa := New_List (
              Make_Pragma_Argument_Association (Loc,
                Expression => Make_Identifier (Loc, Name_Assertion)),

              Make_Pragma_Argument_Association (Sloc (Expr),
                Expression => Expr));

            if Arg_Count > 1 then
               Check_Optional_Identifier (Arg2, Name_Message);
               Analyze_And_Resolve (Get_Pragma_Arg (Arg2), Standard_String);
               Append_To (Newa, Relocate_Node (Arg2));
            end if;

            Rewrite (N,
              Make_Pragma (Loc,
                Chars                        => Name_Check,
                Pragma_Argument_Associations => Newa));
            Analyze (N);
         end Assert;

         ----------------------
         -- Assertion_Policy --
         ----------------------

         --  pragma Assertion_Policy (Check | Disable |Ignore)

         when Pragma_Assertion_Policy => Assertion_Policy : declare
            Policy : Node_Id;

         begin
            Ada_2005_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_One_Of (Arg1, Name_Check, Name_Disable, Name_Ignore);

            --  We treat pragma Assertion_Policy as equivalent to:

            --    pragma Check_Policy (Assertion, policy)

            --  So rewrite the pragma in that manner and link on to the chain
            --  of Check_Policy pragmas, marking the pragma as analyzed.

            Policy := Get_Pragma_Arg (Arg1);

            Rewrite (N,
              Make_Pragma (Loc,
                Chars => Name_Check_Policy,

                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Loc,
                    Expression => Make_Identifier (Loc, Name_Assertion)),

                  Make_Pragma_Argument_Association (Loc,
                    Expression =>
                      Make_Identifier (Sloc (Policy), Chars (Policy))))));

            Set_Analyzed (N);
            Set_Next_Pragma (N, Opt.Check_Policy_List);
            Opt.Check_Policy_List := N;
         end Assertion_Policy;

         ------------------------------
         -- Assume_No_Invalid_Values --
         ------------------------------

         --  pragma Assume_No_Invalid_Values (On | Off);

         when Pragma_Assume_No_Invalid_Values =>
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);

            if Chars (Get_Pragma_Arg (Arg1)) = Name_On then
               Assume_No_Invalid_Values := True;
            else
               Assume_No_Invalid_Values := False;
            end if;

         ---------------
         -- AST_Entry --
         ---------------

         --  pragma AST_Entry (entry_IDENTIFIER);

         when Pragma_AST_Entry => AST_Entry : declare
            Ent : Node_Id;

         begin
            GNAT_Pragma;
            Check_VMS (N);
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Local_Name (Arg1);
            Ent := Entity (Get_Pragma_Arg (Arg1));

            --  Note: the implementation of the AST_Entry pragma could handle
            --  the entry family case fine, but for now we are consistent with
            --  the DEC rules, and do not allow the pragma, which of course
            --  has the effect of also forbidding the attribute.

            if Ekind (Ent) /= E_Entry then
               Error_Pragma_Arg
                 ("pragma% argument must be simple entry name", Arg1);

            elsif Is_AST_Entry (Ent) then
               Error_Pragma_Arg
                 ("duplicate % pragma for entry", Arg1);

            elsif Has_Homonym (Ent) then
               Error_Pragma_Arg
                 ("pragma% argument cannot specify overloaded entry", Arg1);

            else
               declare
                  FF : constant Entity_Id := First_Formal (Ent);

               begin
                  if Present (FF) then
                     if Present (Next_Formal (FF)) then
                        Error_Pragma_Arg
                          ("entry for pragma% can have only one argument",
                           Arg1);

                     elsif Parameter_Mode (FF) /= E_In_Parameter then
                        Error_Pragma_Arg
                          ("entry parameter for pragma% must have mode IN",
                           Arg1);
                     end if;
                  end if;
               end;

               Set_Is_AST_Entry (Ent);
            end if;
         end AST_Entry;

         ------------------
         -- Asynchronous --
         ------------------

         --  pragma Asynchronous (LOCAL_NAME);

         when Pragma_Asynchronous => Asynchronous : declare
            Nm     : Entity_Id;
            C_Ent  : Entity_Id;
            L      : List_Id;
            S      : Node_Id;
            N      : Node_Id;
            Formal : Entity_Id;

            procedure Process_Async_Pragma;
            --  Common processing for procedure and access-to-procedure case

            --------------------------
            -- Process_Async_Pragma --
            --------------------------

            procedure Process_Async_Pragma is
            begin
               if No (L) then
                  Set_Is_Asynchronous (Nm);
                  return;
               end if;

               --  The formals should be of mode IN (RM E.4.1(6))

               S := First (L);
               while Present (S) loop
                  Formal := Defining_Identifier (S);

                  if Nkind (Formal) = N_Defining_Identifier
                    and then Ekind (Formal) /= E_In_Parameter
                  then
                     Error_Pragma_Arg
                       ("pragma% procedure can only have IN parameter",
                        Arg1);
                  end if;

                  Next (S);
               end loop;

               Set_Is_Asynchronous (Nm);
            end Process_Async_Pragma;

         --  Start of processing for pragma Asynchronous

         begin
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            if Debug_Flag_U then
               return;
            end if;

            C_Ent := Cunit_Entity (Current_Sem_Unit);
            Analyze (Get_Pragma_Arg (Arg1));
            Nm := Entity (Get_Pragma_Arg (Arg1));

            if not Is_Remote_Call_Interface (C_Ent)
              and then not Is_Remote_Types (C_Ent)
            then
               --  This pragma should only appear in an RCI or Remote Types
               --  unit (RM E.4.1(4)).

               Error_Pragma
                 ("pragma% not in Remote_Call_Interface or " &
                  "Remote_Types unit");
            end if;

            if Ekind (Nm) = E_Procedure
              and then Nkind (Parent (Nm)) = N_Procedure_Specification
            then
               if not Is_Remote_Call_Interface (Nm) then
                  Error_Pragma_Arg
                    ("pragma% cannot be applied on non-remote procedure",
                     Arg1);
               end if;

               L := Parameter_Specifications (Parent (Nm));
               Process_Async_Pragma;
               return;

            elsif Ekind (Nm) = E_Function then
               Error_Pragma_Arg
                 ("pragma% cannot be applied to function", Arg1);

            elsif Is_Remote_Access_To_Subprogram_Type (Nm) then
                  if Is_Record_Type (Nm) then

                  --  A record type that is the Equivalent_Type for a remote
                  --  access-to-subprogram type.

                     N := Declaration_Node (Corresponding_Remote_Type (Nm));

                  else
                     --  A non-expanded RAS type (distribution is not enabled)

                     N := Declaration_Node (Nm);
                  end if;

               if Nkind (N) = N_Full_Type_Declaration
                 and then Nkind (Type_Definition (N)) =
                                     N_Access_Procedure_Definition
               then
                  L := Parameter_Specifications (Type_Definition (N));
                  Process_Async_Pragma;

                  if Is_Asynchronous (Nm)
                    and then Expander_Active
                    and then Get_PCS_Name /= Name_No_DSA
                  then
                     RACW_Type_Is_Asynchronous (Underlying_RACW_Type (Nm));
                  end if;

               else
                  Error_Pragma_Arg
                    ("pragma% cannot reference access-to-function type",
                    Arg1);
               end if;

            --  Only other possibility is Access-to-class-wide type

            elsif Is_Access_Type (Nm)
              and then Is_Class_Wide_Type (Designated_Type (Nm))
            then
               Check_First_Subtype (Arg1);
               Set_Is_Asynchronous (Nm);
               if Expander_Active then
                  RACW_Type_Is_Asynchronous (Nm);
               end if;

            else
               Error_Pragma_Arg ("inappropriate argument for pragma%", Arg1);
            end if;
         end Asynchronous;

         ------------
         -- Atomic --
         ------------

         --  pragma Atomic (LOCAL_NAME);

         when Pragma_Atomic =>
            Process_Atomic_Shared_Volatile;

         -----------------------
         -- Atomic_Components --
         -----------------------

         --  pragma Atomic_Components (array_LOCAL_NAME);

         --  This processing is shared by Volatile_Components

         when Pragma_Atomic_Components   |
              Pragma_Volatile_Components =>

         Atomic_Components : declare
            E_Id : Node_Id;
            E    : Entity_Id;
            D    : Node_Id;
            K    : Node_Kind;

         begin
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Get_Pragma_Arg (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            Check_Duplicate_Pragma (E);

            if Rep_Item_Too_Early (E, N)
                 or else
               Rep_Item_Too_Late (E, N)
            then
               return;
            end if;

            D := Declaration_Node (E);
            K := Nkind (D);

            if (K = N_Full_Type_Declaration and then Is_Array_Type (E))
              or else
                ((Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
                   and then Nkind (D) = N_Object_Declaration
                   and then Nkind (Object_Definition (D)) =
                                       N_Constrained_Array_Definition)
            then
               --  The flag is set on the object, or on the base type

               if Nkind (D) /= N_Object_Declaration then
                  E := Base_Type (E);
               end if;

               Set_Has_Volatile_Components (E);

               if Prag_Id = Pragma_Atomic_Components then
                  Set_Has_Atomic_Components (E);
               end if;

            else
               Error_Pragma_Arg ("inappropriate entity for pragma%", Arg1);
            end if;
         end Atomic_Components;
         --------------------
         -- Attach_Handler --
         --------------------

         --  pragma Attach_Handler (handler_NAME, EXPRESSION);

         when Pragma_Attach_Handler =>
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (2);

            if No_Run_Time_Mode then
               Error_Msg_CRT ("Attach_Handler pragma", N);
            else
               Check_Interrupt_Or_Attach_Handler;

               --  The expression that designates the attribute may depend on a
               --  discriminant, and is therefore a per-object expression, to
               --  be expanded in the init proc. If expansion is enabled, then
               --  perform semantic checks on a copy only.

               if Expander_Active then
                  declare
                     Temp : constant Node_Id :=
                              New_Copy_Tree (Get_Pragma_Arg (Arg2));
                  begin
                     Set_Parent (Temp, N);
                     Preanalyze_And_Resolve (Temp, RTE (RE_Interrupt_ID));
                  end;

               else
                  Analyze (Get_Pragma_Arg (Arg2));
                  Resolve (Get_Pragma_Arg (Arg2), RTE (RE_Interrupt_ID));
               end if;

               Process_Interrupt_Or_Attach_Handler;
            end if;

         --------------------
         -- C_Pass_By_Copy --
         --------------------

         --  pragma C_Pass_By_Copy ([Max_Size =>] static_integer_EXPRESSION);

         when Pragma_C_Pass_By_Copy => C_Pass_By_Copy : declare
            Arg : Node_Id;
            Val : Uint;

         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, "max_size");

            Arg := Get_Pragma_Arg (Arg1);
            Check_Arg_Is_Static_Expression (Arg, Any_Integer);

            Val := Expr_Value (Arg);

            if Val <= 0 then
               Error_Pragma_Arg
                 ("maximum size for pragma% must be positive", Arg1);

            elsif UI_Is_In_Int_Range (Val) then
               Default_C_Record_Mechanism := UI_To_Int (Val);

            --  If a giant value is given, Int'Last will do well enough.
            --  If sometime someone complains that a record larger than
            --  two gigabytes is not copied, we will worry about it then!

            else
               Default_C_Record_Mechanism := Mechanism_Type'Last;
            end if;
         end C_Pass_By_Copy;

         -----------
         -- Check --
         -----------

         --  pragma Check ([Name    =>] IDENTIFIER,
         --                [Check   =>] Boolean_EXPRESSION
         --              [,[Message =>] String_EXPRESSION]);

         when Pragma_Check => Check : declare
            Expr : Node_Id;
            Eloc : Source_Ptr;

            Check_On : Boolean;
            --  Set True if category of assertions referenced by Name enabled

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments (3);
            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Optional_Identifier (Arg2, Name_Check);

            if Arg_Count = 3 then
               Check_Optional_Identifier (Arg3, Name_Message);
               Analyze_And_Resolve (Get_Pragma_Arg (Arg3), Standard_String);
            end if;

            Check_Arg_Is_Identifier (Arg1);

            --  Completely ignore if disabled

            if Check_Disabled (Chars (Get_Pragma_Arg (Arg1))) then
               Rewrite (N, Make_Null_Statement (Loc));
               Analyze (N);
               return;
            end if;

            --  Indicate if pragma is enabled. The Original_Node reference here
            --  is to deal with pragma Assert rewritten as a Check pragma.

            Check_On := Check_Enabled (Chars (Get_Pragma_Arg (Arg1)));

            if Check_On then
               Set_SCO_Pragma_Enabled (Loc);
            end if;

            --  If expansion is active and the check is not enabled then we
            --  rewrite the Check as:

            --    if False and then condition then
            --       null;
            --    end if;

            --  The reason we do this rewriting during semantic analysis rather
            --  than as part of normal expansion is that we cannot analyze and
            --  expand the code for the boolean expression directly, or it may
            --  cause insertion of actions that would escape the attempt to
            --  suppress the check code.

            --  Note that the Sloc for the if statement corresponds to the
            --  argument condition, not the pragma itself. The reason for this
            --  is that we may generate a warning if the condition is False at
            --  compile time, and we do not want to delete this warning when we
            --  delete the if statement.

            Expr := Get_Pragma_Arg (Arg2);

            if Expander_Active and then not Check_On then
               Eloc := Sloc (Expr);

               Rewrite (N,
                 Make_If_Statement (Eloc,
                   Condition =>
                     Make_And_Then (Eloc,
                       Left_Opnd  => New_Occurrence_Of (Standard_False, Eloc),
                       Right_Opnd => Expr),
                   Then_Statements => New_List (
                     Make_Null_Statement (Eloc))));

               Analyze (N);

            --  Check is active

            else
               Analyze_And_Resolve (Expr, Any_Boolean);
            end if;
         end Check;

         ----------------
         -- Check_Name --
         ----------------

         --  pragma Check_Name (check_IDENTIFIER);

         when Pragma_Check_Name =>
            Check_No_Identifiers;
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_Arg_Is_Identifier (Arg1);

            declare
               Nam : constant Name_Id := Chars (Get_Pragma_Arg (Arg1));

            begin
               for J in Check_Names.First .. Check_Names.Last loop
                  if Check_Names.Table (J) = Nam then
                     return;
                  end if;
               end loop;

               Check_Names.Append (Nam);
            end;

         ------------------
         -- Check_Policy --
         ------------------

         --  pragma Check_Policy (
         --    [Name   =>] IDENTIFIER,
         --    [Policy =>] POLICY_IDENTIFIER);

         --  POLICY_IDENTIFIER ::= ON | OFF | CHECK | DISABLE | IGNORE

         --  Note: this is a configuration pragma, but it is allowed to appear
         --  anywhere else.

         when Pragma_Check_Policy =>
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Optional_Identifier (Arg2, Name_Policy);
            Check_Arg_Is_One_Of
              (Arg2, Name_On, Name_Off, Name_Check, Name_Disable, Name_Ignore);

            --  A Check_Policy pragma can appear either as a configuration
            --  pragma, or in a declarative part or a package spec (see RM
            --  11.5(5) for rules for Suppress/Unsuppress which are also
            --  followed for Check_Policy).

            if not Is_Configuration_Pragma then
               Check_Is_In_Decl_Part_Or_Package_Spec;
            end if;

            Set_Next_Pragma (N, Opt.Check_Policy_List);
            Opt.Check_Policy_List := N;

         ---------------------
         -- CIL_Constructor --
         ---------------------

         --  pragma CIL_Constructor ([Entity =>] LOCAL_NAME);

         --  Processing for this pragma is shared with Java_Constructor

         -------------
         -- Comment --
         -------------

         --  pragma Comment (static_string_EXPRESSION)

         --  Processing for pragma Comment shares the circuitry for pragma
         --  Ident. The only differences are that Ident enforces a limit of 31
         --  characters on its argument, and also enforces limitations on
         --  placement for DEC compatibility. Pragma Comment shares neither of
         --  these restrictions.

         -------------------
         -- Common_Object --
         -------------------

         --  pragma Common_Object (
         --        [Internal =>] LOCAL_NAME
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         --  Processing for this pragma is shared with Psect_Object

         ------------------------
         -- Compile_Time_Error --
         ------------------------

         --  pragma Compile_Time_Error
         --    (boolean_EXPRESSION, static_string_EXPRESSION);

         when Pragma_Compile_Time_Error =>
            GNAT_Pragma;
            Process_Compile_Time_Warning_Or_Error;

         --------------------------
         -- Compile_Time_Warning --
         --------------------------

         --  pragma Compile_Time_Warning
         --    (boolean_EXPRESSION, static_string_EXPRESSION);

         when Pragma_Compile_Time_Warning =>
            GNAT_Pragma;
            Process_Compile_Time_Warning_Or_Error;

         -------------------
         -- Compiler_Unit --
         -------------------

         when Pragma_Compiler_Unit =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Set_Is_Compiler_Unit (Get_Source_Unit (N));

         -----------------------------
         -- Complete_Representation --
         -----------------------------

         --  pragma Complete_Representation;

         when Pragma_Complete_Representation =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            if Nkind (Parent (N)) /= N_Record_Representation_Clause then
               Error_Pragma
                 ("pragma & must appear within record representation clause");
            end if;

         ----------------------------
         -- Complex_Representation --
         ----------------------------

         --  pragma Complex_Representation ([Entity =>] LOCAL_NAME);

         when Pragma_Complex_Representation => Complex_Representation : declare
            E_Id : Entity_Id;
            E    : Entity_Id;
            Ent  : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Get_Pragma_Arg (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            if not Is_Record_Type (E) then
               Error_Pragma_Arg
                 ("argument for pragma% must be record type", Arg1);
            end if;

            Ent := First_Entity (E);

            if No (Ent)
              or else No (Next_Entity (Ent))
              or else Present (Next_Entity (Next_Entity (Ent)))
              or else not Is_Floating_Point_Type (Etype (Ent))
              or else Etype (Ent) /= Etype (Next_Entity (Ent))
            then
               Error_Pragma_Arg
                 ("record for pragma% must have two fields of the same "
                  & "floating-point type", Arg1);

            else
               Set_Has_Complex_Representation (Base_Type (E));

               --  We need to treat the type has having a non-standard
               --  representation, for back-end purposes, even though in
               --  general a complex will have the default representation
               --  of a record with two real components.

               Set_Has_Non_Standard_Rep (Base_Type (E));
            end if;
         end Complex_Representation;

         -------------------------
         -- Component_Alignment --
         -------------------------

         --  pragma Component_Alignment (
         --        [Form =>] ALIGNMENT_CHOICE
         --     [, [Name =>] type_LOCAL_NAME]);
         --
         --   ALIGNMENT_CHOICE ::=
         --     Component_Size
         --   | Component_Size_4
         --   | Storage_Unit
         --   | Default

         when Pragma_Component_Alignment => Component_AlignmentP : declare
            Args  : Args_List (1 .. 2);
            Names : constant Name_List (1 .. 2) := (
                      Name_Form,
                      Name_Name);

            Form  : Node_Id renames Args (1);
            Name  : Node_Id renames Args (2);

            Atype : Component_Alignment_Kind;
            Typ   : Entity_Id;

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);

            if No (Form) then
               Error_Pragma ("missing Form argument for pragma%");
            end if;

            Check_Arg_Is_Identifier (Form);

            --  Get proper alignment, note that Default = Component_Size on all
            --  machines we have so far, and we want to set this value rather
            --  than the default value to indicate that it has been explicitly
            --  set (and thus will not get overridden by the default component
            --  alignment for the current scope)

            if Chars (Form) = Name_Component_Size then
               Atype := Calign_Component_Size;

            elsif Chars (Form) = Name_Component_Size_4 then
               Atype := Calign_Component_Size_4;

            elsif Chars (Form) = Name_Default then
               Atype := Calign_Component_Size;

            elsif Chars (Form) = Name_Storage_Unit then
               Atype := Calign_Storage_Unit;

            else
               Error_Pragma_Arg
                 ("invalid Form parameter for pragma%", Form);
            end if;

            --  Case with no name, supplied, affects scope table entry

            if No (Name) then
               Scope_Stack.Table
                 (Scope_Stack.Last).Component_Alignment_Default := Atype;

            --  Case of name supplied

            else
               Check_Arg_Is_Local_Name (Name);
               Find_Type (Name);
               Typ := Entity (Name);

               if Typ = Any_Type
                 or else Rep_Item_Too_Early (Typ, N)
               then
                  return;
               else
                  Typ := Underlying_Type (Typ);
               end if;

               if not Is_Record_Type (Typ)
                 and then not Is_Array_Type (Typ)
               then
                  Error_Pragma_Arg
                    ("Name parameter of pragma% must identify record or " &
                     "array type", Name);
               end if;

               --  An explicit Component_Alignment pragma overrides an
               --  implicit pragma Pack, but not an explicit one.

               if not Has_Pragma_Pack (Base_Type (Typ)) then
                  Set_Is_Packed (Base_Type (Typ), False);
                  Set_Component_Alignment (Base_Type (Typ), Atype);
               end if;
            end if;
         end Component_AlignmentP;

         ----------------
         -- Controlled --
         ----------------

         --  pragma Controlled (first_subtype_LOCAL_NAME);

         when Pragma_Controlled => Controlled : declare
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            Arg := Get_Pragma_Arg (Arg1);

            if not Is_Entity_Name (Arg)
              or else not Is_Access_Type (Entity (Arg))
            then
               Error_Pragma_Arg ("pragma% requires access type", Arg1);
            else
               Set_Has_Pragma_Controlled (Base_Type (Entity (Arg)));
            end if;
         end Controlled;

         ----------------
         -- Convention --
         ----------------

         --  pragma Convention ([Convention =>] convention_IDENTIFIER,
         --    [Entity =>] LOCAL_NAME);

         when Pragma_Convention => Convention : declare
            C : Convention_Id;
            E : Entity_Id;
            pragma Warnings (Off, C);
            pragma Warnings (Off, E);
         begin
            Check_Arg_Order ((Name_Convention, Name_Entity));
            Check_Ada_83_Warning;
            Check_Arg_Count (2);
            Process_Convention (C, E);
         end Convention;

         ---------------------------
         -- Convention_Identifier --
         ---------------------------

         --  pragma Convention_Identifier ([Name =>] IDENTIFIER,
         --    [Convention =>] convention_IDENTIFIER);

         when Pragma_Convention_Identifier => Convention_Identifier : declare
            Idnam : Name_Id;
            Cname : Name_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Order ((Name_Name, Name_Convention));
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Optional_Identifier (Arg2, Name_Convention);
            Check_Arg_Is_Identifier (Arg1);
            Check_Arg_Is_Identifier (Arg2);
            Idnam := Chars (Get_Pragma_Arg (Arg1));
            Cname := Chars (Get_Pragma_Arg (Arg2));

            if Is_Convention_Name (Cname) then
               Record_Convention_Identifier
                 (Idnam, Get_Convention_Id (Cname));
            else
               Error_Pragma_Arg
                 ("second arg for % pragma must be convention", Arg2);
            end if;
         end Convention_Identifier;

         ---------------
         -- CPP_Class --
         ---------------

         --  pragma CPP_Class ([Entity =>] local_NAME)

         when Pragma_CPP_Class => CPP_Class : declare
            Arg : Node_Id;
            Typ : Entity_Id;

         begin
            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("'G'N'A'T pragma cpp'_class is now obsolete; replace it" &
                  " by pragma import?", N);
            end if;

            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            Arg := Get_Pragma_Arg (Arg1);
            Analyze (Arg);

            if Etype (Arg) = Any_Type then
               return;
            end if;

            if not Is_Entity_Name (Arg)
              or else not Is_Type (Entity (Arg))
            then
               Error_Pragma_Arg ("pragma% requires a type mark", Arg1);
            end if;

            Typ := Entity (Arg);

            if not Is_Tagged_Type (Typ) then
               Error_Pragma_Arg ("pragma% applicable to tagged types ", Arg1);
            end if;

            --  Types treated as CPP classes must be declared limited (note:
            --  this used to be a warning but there is no real benefit to it
            --  since we did effectively intend to treat the type as limited
            --  anyway).

            if not Is_Limited_Type (Typ) then
               Error_Msg_N
                 ("imported 'C'P'P type must be limited",
                  Get_Pragma_Arg (Arg1));
            end if;

            Set_Is_CPP_Class (Typ);
            Set_Convention (Typ, Convention_CPP);

            --  Imported CPP types must not have discriminants (because C++
            --  classes do not have discriminants).

            if Has_Discriminants (Typ) then
               Error_Msg_N
                 ("imported 'C'P'P type cannot have discriminants",
                  First (Discriminant_Specifications
                          (Declaration_Node (Typ))));
            end if;

            --  Components of imported CPP types must not have default
            --  expressions because the constructor (if any) is in the
            --  C++ side.

            if Is_Incomplete_Or_Private_Type (Typ)
              and then No (Underlying_Type (Typ))
            then
               --  It should be an error to apply pragma CPP to a private
               --  type if the underlying type is not visible (as it is
               --  for any representation item). For now, for backward
               --  compatibility we do nothing but we cannot check components
               --  because they are not available at this stage. All this code
               --  will be removed when we cleanup this obsolete GNAT pragma???

               null;

            else
               declare
                  Tdef  : constant Node_Id :=
                            Type_Definition (Declaration_Node (Typ));
                  Clist : Node_Id;
                  Comp  : Node_Id;

               begin
                  if Nkind (Tdef) = N_Record_Definition then
                     Clist := Component_List (Tdef);
                  else
                     pragma Assert (Nkind (Tdef) = N_Derived_Type_Definition);
                     Clist := Component_List (Record_Extension_Part (Tdef));
                  end if;

                  if Present (Clist) then
                     Comp := First (Component_Items (Clist));
                     while Present (Comp) loop
                        if Present (Expression (Comp)) then
                           Error_Msg_N
                             ("component of imported 'C'P'P type cannot have" &
                              " default expression", Expression (Comp));
                        end if;

                        Next (Comp);
                     end loop;
                  end if;
               end;
            end if;
         end CPP_Class;

         ---------------------
         -- CPP_Constructor --
         ---------------------

         --  pragma CPP_Constructor ([Entity =>] LOCAL_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_CPP_Constructor => CPP_Constructor : declare
            Elmt    : Elmt_Id;
            Id      : Entity_Id;
            Def_Id  : Entity_Id;
            Tag_Typ : Entity_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_At_Most_N_Arguments (3);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            Id := Get_Pragma_Arg (Arg1);
            Find_Program_Unit_Name (Id);

            --  If we did not find the name, we are done

            if Etype (Id) = Any_Type then
               return;
            end if;

            Def_Id := Entity (Id);

            --  Check if already defined as constructor

            if Is_Constructor (Def_Id) then
               Error_Msg_N
                 ("?duplicate argument for pragma 'C'P'P_Constructor", Arg1);
               return;
            end if;

            if Ekind (Def_Id) = E_Function
              and then (Is_CPP_Class (Etype (Def_Id))
                         or else (Is_Class_Wide_Type (Etype (Def_Id))
                                   and then
                                  Is_CPP_Class (Root_Type (Etype (Def_Id)))))
            then
               if Arg_Count >= 2 then
                  Set_Imported (Def_Id);
                  Set_Is_Public (Def_Id);
                  Process_Interface_Name (Def_Id, Arg2, Arg3);
               end if;

               Set_Has_Completion (Def_Id);
               Set_Is_Constructor (Def_Id);

               --  Imported C++ constructors are not dispatching primitives
               --  because in C++ they don't have a dispatch table slot.
               --  However, in Ada the constructor has the profile of a
               --  function that returns a tagged type and therefore it has
               --  been treated as a primitive operation during semantic
               --  analysis. We now remove it from the list of primitive
               --  operations of the type.

               if Is_Tagged_Type (Etype (Def_Id))
                 and then not Is_Class_Wide_Type (Etype (Def_Id))
               then
                  pragma Assert (Is_Dispatching_Operation (Def_Id));
                  Tag_Typ := Etype (Def_Id);

                  Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
                  while Present (Elmt) and then Node (Elmt) /= Def_Id loop
                     Next_Elmt (Elmt);
                  end loop;

                  Remove_Elmt (Primitive_Operations (Tag_Typ), Elmt);
                  Set_Is_Dispatching_Operation (Def_Id, False);
               end if;

               --  For backward compatibility, if the constructor returns a
               --  class wide type, and we internally change the return type to
               --  the corresponding root type.

               if Is_Class_Wide_Type (Etype (Def_Id)) then
                  Set_Etype (Def_Id, Root_Type (Etype (Def_Id)));
               end if;
            else
               Error_Pragma_Arg
                 ("pragma% requires function returning a 'C'P'P_Class type",
                   Arg1);
            end if;
         end CPP_Constructor;

         -----------------
         -- CPP_Virtual --
         -----------------

         when Pragma_CPP_Virtual => CPP_Virtual : declare
         begin
            GNAT_Pragma;

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("'G'N'A'T pragma cpp'_virtual is now obsolete and has " &
                  "no effect?", N);
            end if;
         end CPP_Virtual;

         ----------------
         -- CPP_Vtable --
         ----------------

         when Pragma_CPP_Vtable => CPP_Vtable : declare
         begin
            GNAT_Pragma;

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("'G'N'A'T pragma cpp'_vtable is now obsolete and has " &
                  "no effect?", N);
            end if;
         end CPP_Vtable;

         ---------
         -- CPU --
         ---------

         --  pragma CPU (EXPRESSION);

         when Pragma_CPU => CPU : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Ada_2012_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);

            --  Subprogram case

            if Nkind (P) = N_Subprogram_Body then
               Check_In_Main_Program;

               Arg := Get_Pragma_Arg (Arg1);
               Analyze_And_Resolve (Arg, Any_Integer);

               --  Must be static

               if not Is_Static_Expression (Arg) then
                  Flag_Non_Static_Expr
                    ("main subprogram affinity is not static!", Arg);
                  raise Pragma_Exit;

               --  If constraint error, then we already signalled an error

               elsif Raises_Constraint_Error (Arg) then
                  null;

               --  Otherwise check in range

               else
                  declare
                     CPU_Id : constant Entity_Id := RTE (RE_CPU_Range);
                     --  This is the entity System.Multiprocessors.CPU_Range;

                     Val : constant Uint := Expr_Value (Arg);

                  begin
                     if Val < Expr_Value (Type_Low_Bound (CPU_Id))
                          or else
                        Val > Expr_Value (Type_High_Bound (CPU_Id))
                     then
                        Error_Pragma_Arg
                          ("main subprogram CPU is out of range", Arg1);
                     end if;
                  end;
               end if;

               Set_Main_CPU
                    (Current_Sem_Unit, UI_To_Int (Expr_Value (Arg)));

            --  Task case

            elsif Nkind (P) = N_Task_Definition then
               Arg := Get_Pragma_Arg (Arg1);

               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               Preanalyze_Spec_Expression (Arg, RTE (RE_CPU_Range));

            --  Anything else is incorrect

            else
               Pragma_Misplaced;
            end if;

            if Has_Pragma_CPU (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Pragma_CPU (P, True);

               if Nkind (P) = N_Task_Definition then
                  Record_Rep_Item (Defining_Identifier (Parent (P)), N);
               end if;
            end if;
         end CPU;

         -----------
         -- Debug --
         -----------

         --  pragma Debug ([boolean_EXPRESSION,] PROCEDURE_CALL_STATEMENT);

         when Pragma_Debug => Debug : declare
            Cond : Node_Id;
            Call : Node_Id;

         begin
            GNAT_Pragma;

            --  Skip analysis if disabled

            if Debug_Pragmas_Disabled then
               Rewrite (N, Make_Null_Statement (Loc));
               Analyze (N);
               return;
            end if;

            Cond :=
              New_Occurrence_Of
                (Boolean_Literals (Debug_Pragmas_Enabled and Expander_Active),
                 Loc);

            if Debug_Pragmas_Enabled then
               Set_SCO_Pragma_Enabled (Loc);
            end if;

            if Arg_Count = 2 then
               Cond :=
                 Make_And_Then (Loc,
                   Left_Opnd  => Relocate_Node (Cond),
                   Right_Opnd => Get_Pragma_Arg (Arg1));
               Call := Get_Pragma_Arg (Arg2);
            else
               Call := Get_Pragma_Arg (Arg1);
            end if;

            if Nkind_In (Call,
                 N_Indexed_Component,
                 N_Function_Call,
                 N_Identifier,
                 N_Expanded_Name,
                 N_Selected_Component)
            then
               --  If this pragma Debug comes from source, its argument was
               --  parsed as a name form (which is syntactically identical).
               --  In a generic context a parameterless call will be left as
               --  an expanded name (if global) or selected_component if local.
               --  Change it to a procedure call statement now.

               Change_Name_To_Procedure_Call_Statement (Call);

            elsif Nkind (Call) = N_Procedure_Call_Statement then

               --  Already in the form of a procedure call statement: nothing
               --  to do (could happen in case of an internally generated
               --  pragma Debug).

               null;

            else
               --  All other cases: diagnose error

               Error_Msg
                 ("argument of pragma ""Debug"" is not procedure call",
                  Sloc (Call));
               return;
            end if;

            --  Rewrite into a conditional with an appropriate condition. We
            --  wrap the procedure call in a block so that overhead from e.g.
            --  use of the secondary stack does not generate execution overhead
            --  for suppressed conditions.

            --  Normally the analysis that follows will freeze the subprogram
            --  being called. However, if the call is to a null procedure,
            --  we want to freeze it before creating the block, because the
            --  analysis that follows may be done with expansion disabled, in
            --  which case the body will not be generated, leading to spurious
            --  errors.

            if Nkind (Call) = N_Procedure_Call_Statement
              and then Is_Entity_Name (Name (Call))
            then
               Analyze (Name (Call));
               Freeze_Before (N, Entity (Name (Call)));
            end if;

            Rewrite (N, Make_Implicit_If_Statement (N,
              Condition => Cond,
                 Then_Statements => New_List (
                   Make_Block_Statement (Loc,
                     Handled_Statement_Sequence =>
                       Make_Handled_Sequence_Of_Statements (Loc,
                         Statements => New_List (Relocate_Node (Call)))))));
            Analyze (N);
         end Debug;

         ------------------
         -- Debug_Policy --
         ------------------

         --  pragma Debug_Policy (Check | Ignore)

         when Pragma_Debug_Policy =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Check, Name_Disable, Name_Ignore);
            Debug_Pragmas_Enabled :=
              Chars (Get_Pragma_Arg (Arg1)) = Name_Check;
            Debug_Pragmas_Disabled :=
              Chars (Get_Pragma_Arg (Arg1)) = Name_Disable;

         ---------------------
         -- Detect_Blocking --
         ---------------------

         --  pragma Detect_Blocking;

         when Pragma_Detect_Blocking =>
            Ada_2005_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Detect_Blocking := True;

         --------------------------
         -- Default_Storage_Pool --
         --------------------------

         --  pragma Default_Storage_Pool (storage_pool_NAME | null);

         when Pragma_Default_Storage_Pool =>
            Ada_2012_Pragma;
            Check_Arg_Count (1);

            --  Default_Storage_Pool can appear as a configuration pragma, or
            --  in a declarative part or a package spec.

            if not Is_Configuration_Pragma then
               Check_Is_In_Decl_Part_Or_Package_Spec;
            end if;

            --  Case of Default_Storage_Pool (null);

            if Nkind (Expression (Arg1)) = N_Null then
               Analyze (Expression (Arg1));

               --  This is an odd case, this is not really an expression, so
               --  we don't have a type for it. So just set the type to Empty.

               Set_Etype (Expression (Arg1), Empty);

            --  Case of Default_Storage_Pool (storage_pool_NAME);

            else
               --  If it's a configuration pragma, then the only allowed
               --  argument is "null".

               if Is_Configuration_Pragma then
                  Error_Pragma_Arg ("NULL expected", Arg1);
               end if;

               --  The expected type for a non-"null" argument is
               --  Root_Storage_Pool'Class.

               Analyze_And_Resolve
                 (Get_Pragma_Arg (Arg1),
                  Typ => Class_Wide_Type (RTE (RE_Root_Storage_Pool)));
            end if;

            --  Finally, record the pool name (or null). Freeze.Freeze_Entity
            --  for an access type will use this information to set the
            --  appropriate attributes of the access type.

            Default_Pool := Expression (Arg1);

         ------------------------------------
         -- Disable_Atomic_Synchronization --
         ------------------------------------

         --  pragma Disable_Atomic_Synchronization [(Entity)];

         when Pragma_Disable_Atomic_Synchronization =>
            Process_Disable_Enable_Atomic_Sync (Name_Suppress);

         -------------------
         -- Discard_Names --
         -------------------

         --  pragma Discard_Names [([On =>] LOCAL_NAME)];

         when Pragma_Discard_Names => Discard_Names : declare
            E    : Entity_Id;
            E_Id : Entity_Id;

         begin
            Check_Ada_83_Warning;

            --  Deal with configuration pragma case

            if Arg_Count = 0 and then Is_Configuration_Pragma then
               Global_Discard_Names := True;
               return;

            --  Otherwise, check correct appropriate context

            else
               Check_Is_In_Decl_Part_Or_Package_Spec;

               if Arg_Count = 0 then

                  --  If there is no parameter, then from now on this pragma
                  --  applies to any enumeration, exception or tagged type
                  --  defined in the current declarative part, and recursively
                  --  to any nested scope.

                  Set_Discard_Names (Current_Scope);
                  return;

               else
                  Check_Arg_Count (1);
                  Check_Optional_Identifier (Arg1, Name_On);
                  Check_Arg_Is_Local_Name (Arg1);

                  E_Id := Get_Pragma_Arg (Arg1);

                  if Etype (E_Id) = Any_Type then
                     return;
                  else
                     E := Entity (E_Id);
                  end if;

                  if (Is_First_Subtype (E)
                      and then
                        (Is_Enumeration_Type (E) or else Is_Tagged_Type (E)))
                    or else Ekind (E) = E_Exception
                  then
                     Set_Discard_Names (E);
                  else
                     Error_Pragma_Arg
                       ("inappropriate entity for pragma%", Arg1);
                  end if;

               end if;
            end if;
         end Discard_Names;

         ------------------------
         -- Dispatching_Domain --
         ------------------------

         --  pragma Dispatching_Domain (EXPRESSION);

         when Pragma_Dispatching_Domain => Dispatching_Domain : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Ada_2012_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);

            --  This pragma is born obsolete, but not the aspect

            if not From_Aspect_Specification (N) then
               Check_Restriction
                 (No_Obsolescent_Features, Pragma_Identifier (N));
            end if;

            if Nkind (P) = N_Task_Definition then
               Arg := Get_Pragma_Arg (Arg1);

               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               Preanalyze_Spec_Expression (Arg, RTE (RE_Dispatching_Domain));

            --  Anything else is incorrect

            else
               Pragma_Misplaced;
            end if;

            if Has_Pragma_Dispatching_Domain (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Pragma_Dispatching_Domain (P, True);

               if Nkind (P) = N_Task_Definition then
                  Record_Rep_Item (Defining_Identifier (Parent (P)), N);
               end if;
            end if;
         end Dispatching_Domain;

         ---------------
         -- Elaborate --
         ---------------

         --  pragma Elaborate (library_unit_NAME {, library_unit_NAME});

         when Pragma_Elaborate => Elaborate : declare
            Arg   : Node_Id;
            Citem : Node_Id;

         begin
            --  Pragma must be in context items list of a compilation unit

            if not Is_In_Context_Clause then
               Pragma_Misplaced;
            end if;

            --  Must be at least one argument

            if Arg_Count = 0 then
               Error_Pragma ("pragma% requires at least one argument");
            end if;

            --  In Ada 83 mode, there can be no items following it in the
            --  context list except other pragmas and implicit with clauses
            --  (e.g. those added by use of Rtsfind). In Ada 95 mode, this
            --  placement rule does not apply.

            if Ada_Version = Ada_83 and then Comes_From_Source (N) then
               Citem := Next (N);
               while Present (Citem) loop
                  if Nkind (Citem) = N_Pragma
                    or else (Nkind (Citem) = N_With_Clause
                              and then Implicit_With (Citem))
                  then
                     null;
                  else
                     Error_Pragma
                       ("(Ada 83) pragma% must be at end of context clause");
                  end if;

                  Next (Citem);
               end loop;
            end if;

            --  Finally, the arguments must all be units mentioned in a with
            --  clause in the same context clause. Note we already checked (in
            --  Par.Prag) that the arguments are all identifiers or selected
            --  components.

            Arg := Arg1;
            Outer : while Present (Arg) loop
               Citem := First (List_Containing (N));
               Inner : while Citem /= N loop
                  if Nkind (Citem) = N_With_Clause
                    and then Same_Name (Name (Citem), Get_Pragma_Arg (Arg))
                  then
                     Set_Elaborate_Present (Citem, True);
                     Set_Unit_Name (Get_Pragma_Arg (Arg), Name (Citem));
                     Generate_Reference (Entity (Name (Citem)), Citem);

                     --  With the pragma present, elaboration calls on
                     --  subprograms from the named unit need no further
                     --  checks, as long as the pragma appears in the current
                     --  compilation unit. If the pragma appears in some unit
                     --  in the context, there might still be a need for an
                     --  Elaborate_All_Desirable from the current compilation
                     --  to the named unit, so we keep the check enabled.

                     if In_Extended_Main_Source_Unit (N) then
                        Set_Suppress_Elaboration_Warnings
                          (Entity (Name (Citem)));
                     end if;

                     exit Inner;
                  end if;

                  Next (Citem);
               end loop Inner;

               if Citem = N then
                  Error_Pragma_Arg
                    ("argument of pragma% is not withed unit", Arg);
               end if;

               Next (Arg);
            end loop Outer;

            --  Give a warning if operating in static mode with -gnatwl
            --  (elaboration warnings enabled) switch set.

            if Elab_Warnings and not Dynamic_Elaboration_Checks then
               Error_Msg_N
                 ("?use of pragma Elaborate may not be safe", N);
               Error_Msg_N
                 ("?use pragma Elaborate_All instead if possible", N);
            end if;
         end Elaborate;

         -------------------
         -- Elaborate_All --
         -------------------

         --  pragma Elaborate_All (library_unit_NAME {, library_unit_NAME});

         when Pragma_Elaborate_All => Elaborate_All : declare
            Arg   : Node_Id;
            Citem : Node_Id;

         begin
            Check_Ada_83_Warning;

            --  Pragma must be in context items list of a compilation unit

            if not Is_In_Context_Clause then
               Pragma_Misplaced;
            end if;

            --  Must be at least one argument

            if Arg_Count = 0 then
               Error_Pragma ("pragma% requires at least one argument");
            end if;

            --  Note: unlike pragma Elaborate, pragma Elaborate_All does not
            --  have to appear at the end of the context clause, but may
            --  appear mixed in with other items, even in Ada 83 mode.

            --  Final check: the arguments must all be units mentioned in
            --  a with clause in the same context clause. Note that we
            --  already checked (in Par.Prag) that all the arguments are
            --  either identifiers or selected components.

            Arg := Arg1;
            Outr : while Present (Arg) loop
               Citem := First (List_Containing (N));
               Innr : while Citem /= N loop
                  if Nkind (Citem) = N_With_Clause
                    and then Same_Name (Name (Citem), Get_Pragma_Arg (Arg))
                  then
                     Set_Elaborate_All_Present (Citem, True);
                     Set_Unit_Name (Get_Pragma_Arg (Arg), Name (Citem));

                     --  Suppress warnings and elaboration checks on the named
                     --  unit if the pragma is in the current compilation, as
                     --  for pragma Elaborate.

                     if In_Extended_Main_Source_Unit (N) then
                        Set_Suppress_Elaboration_Warnings
                          (Entity (Name (Citem)));
                     end if;
                     exit Innr;
                  end if;

                  Next (Citem);
               end loop Innr;

               if Citem = N then
                  Set_Error_Posted (N);
                  Error_Pragma_Arg
                    ("argument of pragma% is not withed unit", Arg);
               end if;

               Next (Arg);
            end loop Outr;
         end Elaborate_All;

         --------------------
         -- Elaborate_Body --
         --------------------

         --  pragma Elaborate_Body [( library_unit_NAME )];

         when Pragma_Elaborate_Body => Elaborate_Body : declare
            Cunit_Node : Node_Id;
            Cunit_Ent  : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Cunit_Node := Cunit (Current_Sem_Unit);
            Cunit_Ent  := Cunit_Entity (Current_Sem_Unit);

            if Nkind_In (Unit (Cunit_Node), N_Package_Body,
                                            N_Subprogram_Body)
            then
               Error_Pragma ("pragma% must refer to a spec, not a body");
            else
               Set_Body_Required (Cunit_Node, True);
               Set_Has_Pragma_Elaborate_Body (Cunit_Ent);

               --  If we are in dynamic elaboration mode, then we suppress
               --  elaboration warnings for the unit, since it is definitely
               --  fine NOT to do dynamic checks at the first level (and such
               --  checks will be suppressed because no elaboration boolean
               --  is created for Elaborate_Body packages).

               --  But in the static model of elaboration, Elaborate_Body is
               --  definitely NOT good enough to ensure elaboration safety on
               --  its own, since the body may WITH other units that are not
               --  safe from an elaboration point of view, so a client must
               --  still do an Elaborate_All on such units.

               --  Debug flag -gnatdD restores the old behavior of 3.13, where
               --  Elaborate_Body always suppressed elab warnings.

               if Dynamic_Elaboration_Checks or Debug_Flag_DD then
                  Set_Suppress_Elaboration_Warnings (Cunit_Ent);
               end if;
            end if;
         end Elaborate_Body;

         ------------------------
         -- Elaboration_Checks --
         ------------------------

         --  pragma Elaboration_Checks (Static | Dynamic);

         when Pragma_Elaboration_Checks =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Static, Name_Dynamic);
            Dynamic_Elaboration_Checks :=
              (Chars (Get_Pragma_Arg (Arg1)) = Name_Dynamic);

         ---------------
         -- Eliminate --
         ---------------

         --  pragma Eliminate (
         --      [Unit_Name  =>] IDENTIFIER | SELECTED_COMPONENT,
         --    [,[Entity     =>] IDENTIFIER |
         --                      SELECTED_COMPONENT |
         --                      STRING_LITERAL]
         --    [,                OVERLOADING_RESOLUTION]);

         --  OVERLOADING_RESOLUTION ::= PARAMETER_AND_RESULT_TYPE_PROFILE |
         --                             SOURCE_LOCATION

         --  PARAMETER_AND_RESULT_TYPE_PROFILE ::= PROCEDURE_PROFILE |
         --                                        FUNCTION_PROFILE

         --  PROCEDURE_PROFILE ::= Parameter_Types => PARAMETER_TYPES

         --  FUNCTION_PROFILE ::= [Parameter_Types => PARAMETER_TYPES,]
         --                       Result_Type => result_SUBTYPE_NAME]

         --  PARAMETER_TYPES ::= (SUBTYPE_NAME {, SUBTYPE_NAME})
         --  SUBTYPE_NAME    ::= STRING_LITERAL

         --  SOURCE_LOCATION ::= Source_Location => SOURCE_TRACE
         --  SOURCE_TRACE    ::= STRING_LITERAL

         when Pragma_Eliminate => Eliminate : declare
            Args  : Args_List (1 .. 5);
            Names : constant Name_List (1 .. 5) := (
                      Name_Unit_Name,
                      Name_Entity,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Source_Location);

            Unit_Name       : Node_Id renames Args (1);
            Entity          : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Result_Type     : Node_Id renames Args (4);
            Source_Location : Node_Id renames Args (5);

         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Gather_Associations (Names, Args);

            if No (Unit_Name) then
               Error_Pragma ("missing Unit_Name argument for pragma%");
            end if;

            if No (Entity)
              and then (Present (Parameter_Types)
                          or else
                        Present (Result_Type)
                          or else
                        Present (Source_Location))
            then
               Error_Pragma ("missing Entity argument for pragma%");
            end if;

            if (Present (Parameter_Types)
                  or else
                Present (Result_Type))
              and then
                Present (Source_Location)
            then
               Error_Pragma
                 ("parameter profile and source location cannot " &
                  "be used together in pragma%");
            end if;

            Process_Eliminate_Pragma
              (N,
               Unit_Name,
               Entity,
               Parameter_Types,
               Result_Type,
               Source_Location);
         end Eliminate;

         -----------------------------------
         -- Enable_Atomic_Synchronization --
         -----------------------------------

         --  pragma Enable_Atomic_Synchronization [(Entity)];

         when Pragma_Enable_Atomic_Synchronization =>
            Process_Disable_Enable_Atomic_Sync (Name_Unsuppress);

         ------------
         -- Export --
         ------------

         --  pragma Export (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Export => Export : declare
            C      : Convention_Id;
            Def_Id : Entity_Id;

            pragma Warnings (Off, C);

         begin
            Check_Ada_83_Warning;
            Check_Arg_Order
              ((Name_Convention,
                Name_Entity,
                Name_External_Name,
                Name_Link_Name));
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Convention (C, Def_Id);

            if Ekind (Def_Id) /= E_Constant then
               Note_Possible_Modification
                 (Get_Pragma_Arg (Arg2), Sure => False);
            end if;

            Process_Interface_Name (Def_Id, Arg3, Arg4);
            Set_Exported (Def_Id, Arg2);

            --  If the entity is a deferred constant, propagate the information
            --  to the full view, because gigi elaborates the full view only.

            if Ekind (Def_Id) = E_Constant
              and then Present (Full_View (Def_Id))
            then
               declare
                  Id2 : constant Entity_Id := Full_View (Def_Id);
               begin
                  Set_Is_Exported    (Id2, Is_Exported          (Def_Id));
                  Set_First_Rep_Item (Id2, First_Rep_Item       (Def_Id));
                  Set_Interface_Name (Id2, Einfo.Interface_Name (Def_Id));
               end;
            end if;
         end Export;

         ----------------------
         -- Export_Exception --
         ----------------------

         --  pragma Export_Exception (
         --        [Internal         =>] LOCAL_NAME
         --     [, [External         =>] EXTERNAL_SYMBOL]
         --     [, [Form     =>] Ada | VMS]
         --     [, [Code     =>] static_integer_EXPRESSION]);

         when Pragma_Export_Exception => Export_Exception : declare
            Args  : Args_List (1 .. 4);
            Names : constant Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Form,
                      Name_Code);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Form     : Node_Id renames Args (3);
            Code     : Node_Id renames Args (4);

         begin
            GNAT_Pragma;

            if Inside_A_Generic then
               Error_Pragma ("pragma% cannot be used for generic entities");
            end if;

            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Exception_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Form     => Form,
              Arg_Code     => Code);

            if not Is_VMS_Exception (Entity (Internal)) then
               Set_Exported (Entity (Internal), Internal);
            end if;
         end Export_Exception;

         ---------------------
         -- Export_Function --
         ---------------------

         --  pragma Export_Function (
         --        [Internal         =>] LOCAL_NAME
         --     [, [External         =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Result_Type      =>] TYPE_DESIGNATOR]
         --     [, [Mechanism        =>] MECHANISM]
         --     [, [Result_Mechanism =>] MECHANISM_NAME]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Function => Export_Function : declare
            Args  : Args_List (1 .. 6);
            Names : constant Name_List (1 .. 6) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Mechanism,
                      Name_Result_Mechanism);

            Internal         : Node_Id renames Args (1);
            External         : Node_Id renames Args (2);
            Parameter_Types  : Node_Id renames Args (3);
            Result_Type      : Node_Id renames Args (4);
            Mechanism        : Node_Id renames Args (5);
            Result_Mechanism : Node_Id renames Args (6);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal         => Internal,
              Arg_External         => External,
              Arg_Parameter_Types  => Parameter_Types,
              Arg_Result_Type      => Result_Type,
              Arg_Mechanism        => Mechanism,
              Arg_Result_Mechanism => Result_Mechanism);
         end Export_Function;

         -------------------
         -- Export_Object --
         -------------------

         --  pragma Export_Object (
         --        [Internal =>] LOCAL_NAME
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Object => Export_Object : declare
            Args  : Args_List (1 .. 3);
            Names : constant Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Export_Object;

         ----------------------
         -- Export_Procedure --
         ----------------------

         --  pragma Export_Procedure (
         --        [Internal         =>] LOCAL_NAME
         --     [, [External         =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Mechanism        =>] MECHANISM]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Procedure => Export_Procedure : declare
            Args  : Args_List (1 .. 4);
            Names : constant Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism);

            Internal        : Node_Id renames Args (1);
            External        : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Mechanism       : Node_Id renames Args (4);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal        => Internal,
              Arg_External        => External,
              Arg_Parameter_Types => Parameter_Types,
              Arg_Mechanism       => Mechanism);
         end Export_Procedure;

         ------------------
         -- Export_Value --
         ------------------

         --  pragma Export_Value (
         --     [Value     =>] static_integer_EXPRESSION,
         --     [Link_Name =>] static_string_EXPRESSION);

         when Pragma_Export_Value =>
            GNAT_Pragma;
            Check_Arg_Order ((Name_Value, Name_Link_Name));
            Check_Arg_Count (2);

            Check_Optional_Identifier (Arg1, Name_Value);
            Check_Arg_Is_Static_Expression (Arg1, Any_Integer);

            Check_Optional_Identifier (Arg2, Name_Link_Name);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);

         -----------------------------
         -- Export_Valued_Procedure --
         -----------------------------

         --  pragma Export_Valued_Procedure (
         --        [Internal         =>] LOCAL_NAME
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Mechanism        =>] MECHANISM]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Export_Valued_Procedure =>
         Export_Valued_Procedure : declare
            Args  : Args_List (1 .. 4);
            Names : constant Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism);

            Internal        : Node_Id renames Args (1);
            External        : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Mechanism       : Node_Id renames Args (4);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal        => Internal,
              Arg_External        => External,
              Arg_Parameter_Types => Parameter_Types,
              Arg_Mechanism       => Mechanism);
         end Export_Valued_Procedure;

         -------------------
         -- Extend_System --
         -------------------

         --  pragma Extend_System ([Name =>] Identifier);

         when Pragma_Extend_System => Extend_System : declare
         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Arg_Is_Identifier (Arg1);

            Get_Name_String (Chars (Get_Pragma_Arg (Arg1)));

            if Name_Len > 4
              and then Name_Buffer (1 .. 4) = "aux_"
            then
               if Present (System_Extend_Pragma_Arg) then
                  if Chars (Get_Pragma_Arg (Arg1)) =
                     Chars (Expression (System_Extend_Pragma_Arg))
                  then
                     null;
                  else
                     Error_Msg_Sloc := Sloc (System_Extend_Pragma_Arg);
                     Error_Pragma ("pragma% conflicts with that #");
                  end if;

               else
                  System_Extend_Pragma_Arg := Arg1;

                  if not GNAT_Mode then
                     System_Extend_Unit := Arg1;
                  end if;
               end if;
            else
               Error_Pragma ("incorrect name for pragma%, must be Aux_xxx");
            end if;
         end Extend_System;

         ------------------------
         -- Extensions_Allowed --
         ------------------------

         --  pragma Extensions_Allowed (ON | OFF);

         when Pragma_Extensions_Allowed =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);

            if Chars (Get_Pragma_Arg (Arg1)) = Name_On then
               Extensions_Allowed := True;
               Ada_Version := Ada_Version_Type'Last;

            else
               Extensions_Allowed := False;
               Ada_Version := Ada_Version_Explicit;
            end if;

         --------------
         -- External --
         --------------

         --  pragma External (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_External => External : declare
               Def_Id : Entity_Id;

               C : Convention_Id;
               pragma Warnings (Off, C);

         begin
            GNAT_Pragma;
            Check_Arg_Order
              ((Name_Convention,
                Name_Entity,
                Name_External_Name,
                Name_Link_Name));
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Convention (C, Def_Id);
            Note_Possible_Modification
              (Get_Pragma_Arg (Arg2), Sure => False);
            Process_Interface_Name (Def_Id, Arg3, Arg4);
            Set_Exported (Def_Id, Arg2);
         end External;

         --------------------------
         -- External_Name_Casing --
         --------------------------

         --  pragma External_Name_Casing (
         --    UPPERCASE | LOWERCASE
         --    [, AS_IS | UPPERCASE | LOWERCASE]);

         when Pragma_External_Name_Casing => External_Name_Casing : declare
         begin
            GNAT_Pragma;
            Check_No_Identifiers;

            if Arg_Count = 2 then
               Check_Arg_Is_One_Of
                 (Arg2, Name_As_Is, Name_Uppercase, Name_Lowercase);

               case Chars (Get_Pragma_Arg (Arg2)) is
                  when Name_As_Is     =>
                     Opt.External_Name_Exp_Casing := As_Is;

                  when Name_Uppercase =>
                     Opt.External_Name_Exp_Casing := Uppercase;

                  when Name_Lowercase =>
                     Opt.External_Name_Exp_Casing := Lowercase;

                  when others =>
                     null;
               end case;

            else
               Check_Arg_Count (1);
            end if;

            Check_Arg_Is_One_Of (Arg1, Name_Uppercase, Name_Lowercase);

            case Chars (Get_Pragma_Arg (Arg1)) is
               when Name_Uppercase =>
                  Opt.External_Name_Imp_Casing := Uppercase;

               when Name_Lowercase =>
                  Opt.External_Name_Imp_Casing := Lowercase;

               when others =>
                  null;
            end case;
         end External_Name_Casing;

         --------------------------
         -- Favor_Top_Level --
         --------------------------

         --  pragma Favor_Top_Level (type_NAME);

         when Pragma_Favor_Top_Level => Favor_Top_Level : declare
               Named_Entity : Entity_Id;

         begin
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            Named_Entity := Entity (Get_Pragma_Arg (Arg1));

            --  If it's an access-to-subprogram type (in particular, not a
            --  subtype), set the flag on that type.

            if Is_Access_Subprogram_Type (Named_Entity) then
               Set_Can_Use_Internal_Rep (Named_Entity, False);

            --  Otherwise it's an error (name denotes the wrong sort of entity)

            else
               Error_Pragma_Arg
                 ("access-to-subprogram type expected",
                  Get_Pragma_Arg (Arg1));
            end if;
         end Favor_Top_Level;

         ---------------
         -- Fast_Math --
         ---------------

         --  pragma Fast_Math;

         when Pragma_Fast_Math =>
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Valid_Configuration_Pragma;
            Fast_Math := True;

         ---------------------------
         -- Finalize_Storage_Only --
         ---------------------------

         --  pragma Finalize_Storage_Only (first_subtype_LOCAL_NAME);

         when Pragma_Finalize_Storage_Only => Finalize_Storage : declare
            Assoc   : constant Node_Id := Arg1;
            Type_Id : constant Node_Id := Get_Pragma_Arg (Assoc);
            Typ     : Entity_Id;

         begin
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type
              or else Rep_Item_Too_Early (Typ, N)
            then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if not Is_Controlled (Typ) then
               Error_Pragma ("pragma% must specify controlled type");
            end if;

            Check_First_Subtype (Arg1);

            if Finalize_Storage_Only (Typ) then
               Error_Pragma ("duplicate pragma%, only one allowed");

            elsif not Rep_Item_Too_Late (Typ, N) then
               Set_Finalize_Storage_Only (Base_Type (Typ), True);
            end if;
         end Finalize_Storage;

         --------------------------
         -- Float_Representation --
         --------------------------

         --  pragma Float_Representation (FLOAT_REP[, float_type_LOCAL_NAME]);

         --  FLOAT_REP ::= VAX_Float | IEEE_Float

         when Pragma_Float_Representation => Float_Representation : declare
            Argx : Node_Id;
            Digs : Nat;
            Ent  : Entity_Id;

         begin
            GNAT_Pragma;

            if Arg_Count = 1 then
               Check_Valid_Configuration_Pragma;
            else
               Check_Arg_Count (2);
               Check_Optional_Identifier (Arg2, Name_Entity);
               Check_Arg_Is_Local_Name (Arg2);
            end if;

            Check_No_Identifier (Arg1);
            Check_Arg_Is_One_Of (Arg1, Name_VAX_Float, Name_IEEE_Float);

            if not OpenVMS_On_Target then
               if Chars (Get_Pragma_Arg (Arg1)) = Name_VAX_Float then
                  Error_Pragma
                    ("?pragma% ignored (applies only to Open'V'M'S)");
               end if;

               return;
            end if;

            --  One argument case

            if Arg_Count = 1 then
               if Chars (Get_Pragma_Arg (Arg1)) = Name_VAX_Float then
                  if Opt.Float_Format = 'I' then
                     Error_Pragma ("'I'E'E'E format previously specified");
                  end if;

                  Opt.Float_Format := 'V';

               else
                  if Opt.Float_Format = 'V' then
                     Error_Pragma ("'V'A'X format previously specified");
                  end if;

                  Opt.Float_Format := 'I';
               end if;

               Set_Standard_Fpt_Formats;

            --  Two argument case

            else
               Argx := Get_Pragma_Arg (Arg2);

               if not Is_Entity_Name (Argx)
                 or else not Is_Floating_Point_Type (Entity (Argx))
               then
                  Error_Pragma_Arg
                    ("second argument of% pragma must be floating-point type",
                     Arg2);
               end if;

               Ent  := Entity (Argx);
               Digs := UI_To_Int (Digits_Value (Ent));

               --  Two arguments, VAX_Float case

               if Chars (Get_Pragma_Arg (Arg1)) = Name_VAX_Float then
                  case Digs is
                     when  6 => Set_F_Float (Ent);
                     when  9 => Set_D_Float (Ent);
                     when 15 => Set_G_Float (Ent);

                     when others =>
                        Error_Pragma_Arg
                          ("wrong digits value, must be 6,9 or 15", Arg2);
                  end case;

               --  Two arguments, IEEE_Float case

               else
                  case Digs is
                     when  6 => Set_IEEE_Short (Ent);
                     when 15 => Set_IEEE_Long  (Ent);

                     when others =>
                        Error_Pragma_Arg
                          ("wrong digits value, must be 6 or 15", Arg2);
                  end case;
               end if;
            end if;
         end Float_Representation;

         -----------
         -- Ident --
         -----------

         --  pragma Ident (static_string_EXPRESSION)

         --  Note: pragma Comment shares this processing. Pragma Comment is
         --  identical to Ident, except that the restriction of the argument to
         --  31 characters and the placement restrictions are not enforced for
         --  pragma Comment.

         when Pragma_Ident | Pragma_Comment => Ident : declare
            Str : Node_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Static_Expression (Arg1, Standard_String);
            Store_Note (N);

            --  For pragma Ident, preserve DEC compatibility by requiring the
            --  pragma to appear in a declarative part or package spec.

            if Prag_Id = Pragma_Ident then
               Check_Is_In_Decl_Part_Or_Package_Spec;
            end if;

            Str := Expr_Value_S (Get_Pragma_Arg (Arg1));

            declare
               CS : Node_Id;
               GP : Node_Id;

            begin
               GP := Parent (Parent (N));

               if Nkind_In (GP, N_Package_Declaration,
                                N_Generic_Package_Declaration)
               then
                  GP := Parent (GP);
               end if;

               --  If we have a compilation unit, then record the ident value,
               --  checking for improper duplication.

               if Nkind (GP) = N_Compilation_Unit then
                  CS := Ident_String (Current_Sem_Unit);

                  if Present (CS) then

                     --  For Ident, we do not permit multiple instances

                     if Prag_Id = Pragma_Ident then
                        Error_Pragma ("duplicate% pragma not permitted");

                     --  For Comment, we concatenate the string, unless we want
                     --  to preserve the tree structure for ASIS.

                     elsif not ASIS_Mode then
                        Start_String (Strval (CS));
                        Store_String_Char (' ');
                        Store_String_Chars (Strval (Str));
                        Set_Strval (CS, End_String);
                     end if;

                  else
                     --  In VMS, the effect of IDENT is achieved by passing
                     --  --identification=name as a --for-linker switch.

                     if OpenVMS_On_Target then
                        Start_String;
                        Store_String_Chars
                          ("--for-linker=--identification=");
                        String_To_Name_Buffer (Strval (Str));
                        Store_String_Chars (Name_Buffer (1 .. Name_Len));

                        --  Only the last processed IDENT is saved. The main
                        --  purpose is so an IDENT associated with a main
                        --  procedure will be used in preference to an IDENT
                        --  associated with a with'd package.

                        Replace_Linker_Option_String
                          (End_String, "--for-linker=--identification=");
                     end if;

                     Set_Ident_String (Current_Sem_Unit, Str);
                  end if;

               --  For subunits, we just ignore the Ident, since in GNAT these
               --  are not separate object files, and hence not separate units
               --  in the unit table.

               elsif Nkind (GP) = N_Subunit then
                  null;

               --  Otherwise we have a misplaced pragma Ident, but we ignore
               --  this if we are in an instantiation, since it comes from
               --  a generic, and has no relevance to the instantiation.

               elsif Prag_Id = Pragma_Ident then
                  if Instantiation_Location (Loc) = No_Location then
                     Error_Pragma ("pragma% only allowed at outer level");
                  end if;
               end if;
            end;
         end Ident;

         ----------------------------
         -- Implementation_Defined --
         ----------------------------

         --  pragma Implementation_Defined (local_NAME);

         --  Marks previously declared entity as implementation defined. For
         --  an overloaded entity, applies to the most recent homonym.

         --  pragma Implementation_Defined;

         --  The form with no arguments appears anywhere within a scope, most
         --  typically a package spec, and indicates that all entities that are
         --  defined within the package spec are Implementation_Defined.

         when Pragma_Implementation_Defined => Implementation_Defined : declare
            Ent : Entity_Id;

         begin
            Check_No_Identifiers;

            --  Form with no arguments

            if Arg_Count = 0 then
               Set_Is_Implementation_Defined (Current_Scope);

            --  Form with one argument

            else
               Check_Arg_Count (1);
               Check_Arg_Is_Local_Name (Arg1);
               Ent := Entity (Get_Pragma_Arg (Arg1));
               Set_Is_Implementation_Defined (Ent);
            end if;
         end Implementation_Defined;

         -----------------
         -- Implemented --
         -----------------

         --  pragma Implemented (procedure_LOCAL_NAME, implementation_kind);
         --  implementation_kind ::=
         --    By_Entry | By_Protected_Procedure | By_Any | Optional

         --  "By_Any" and "Optional" are treated as synonyms in order to
         --  support Ada 2012 aspect Synchronization.

         when Pragma_Implemented => Implemented : declare
            Proc_Id : Entity_Id;
            Typ     : Entity_Id;

         begin
            Ada_2012_Pragma;
            Check_Arg_Count (2);
            Check_No_Identifiers;
            Check_Arg_Is_Identifier (Arg1);
            Check_Arg_Is_Local_Name (Arg1);
            Check_Arg_Is_One_Of (Arg2,
              Name_By_Any,
              Name_By_Entry,
              Name_By_Protected_Procedure,
              Name_Optional);

            --  Extract the name of the local procedure

            Proc_Id := Entity (Get_Pragma_Arg (Arg1));

            --  Ada 2012 (AI05-0030): The procedure_LOCAL_NAME must denote a
            --  primitive procedure of a synchronized tagged type.

            if Ekind (Proc_Id) = E_Procedure
              and then Is_Primitive (Proc_Id)
              and then Present (First_Formal (Proc_Id))
            then
               Typ := Etype (First_Formal (Proc_Id));

               if Is_Tagged_Type (Typ)
                 and then

                  --  Check for a protected, a synchronized or a task interface

                   ((Is_Interface (Typ)
                       and then Is_Synchronized_Interface (Typ))

                  --  Check for a protected type or a task type that implements
                  --  an interface.

                   or else
                    (Is_Concurrent_Record_Type (Typ)
                       and then Present (Interfaces (Typ)))

                  --  Check for a private record extension with keyword
                  --  "synchronized".

                   or else
                    (Ekind_In (Typ, E_Record_Type_With_Private,
                                    E_Record_Subtype_With_Private)
                       and then Synchronized_Present (Parent (Typ))))
               then
                  null;
               else
                  Error_Pragma_Arg
                    ("controlling formal must be of synchronized " &
                     "tagged type", Arg1);
                  return;
               end if;

            --  Procedures declared inside a protected type must be accepted

            elsif Ekind (Proc_Id) = E_Procedure
              and then Is_Protected_Type (Scope (Proc_Id))
            then
               null;

            --  The first argument is not a primitive procedure

            else
               Error_Pragma_Arg
                 ("pragma % must be applied to a primitive procedure", Arg1);
               return;
            end if;

            --  Ada 2012 (AI05-0030): Cannot apply the implementation_kind
            --  By_Protected_Procedure to the primitive procedure of a task
            --  interface.

            if Chars (Arg2) = Name_By_Protected_Procedure
              and then Is_Interface (Typ)
              and then Is_Task_Interface (Typ)
            then
               Error_Pragma_Arg
                 ("implementation kind By_Protected_Procedure cannot be " &
                  "applied to a task interface primitive", Arg2);
               return;
            end if;

            Record_Rep_Item (Proc_Id, N);
         end Implemented;

         ----------------------
         -- Implicit_Packing --
         ----------------------

         --  pragma Implicit_Packing;

         when Pragma_Implicit_Packing =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Implicit_Packing := True;

         ------------
         -- Import --
         ------------

         --  pragma Import (
         --       [Convention    =>] convention_IDENTIFIER,
         --       [Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Import =>
            Check_Ada_83_Warning;
            Check_Arg_Order
              ((Name_Convention,
                Name_Entity,
                Name_External_Name,
                Name_Link_Name));
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Import_Or_Interface;

         ----------------------
         -- Import_Exception --
         ----------------------

         --  pragma Import_Exception (
         --        [Internal         =>] LOCAL_NAME
         --     [, [External         =>] EXTERNAL_SYMBOL]
         --     [, [Form     =>] Ada | VMS]
         --     [, [Code     =>] static_integer_EXPRESSION]);

         when Pragma_Import_Exception => Import_Exception : declare
            Args  : Args_List (1 .. 4);
            Names : constant Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Form,
                      Name_Code);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Form     : Node_Id renames Args (3);
            Code     : Node_Id renames Args (4);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);

            if Present (External) and then Present (Code) then
               Error_Pragma
                 ("cannot give both External and Code options for pragma%");
            end if;

            Process_Extended_Import_Export_Exception_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Form     => Form,
              Arg_Code     => Code);

            if not Is_VMS_Exception (Entity (Internal)) then
               Set_Imported (Entity (Internal));
            end if;
         end Import_Exception;

         ---------------------
         -- Import_Function --
         ---------------------

         --  pragma Import_Function (
         --        [Internal                 =>] LOCAL_NAME,
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Result_Type              =>] SUBTYPE_MARK]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [Result_Mechanism         =>] MECHANISM_NAME]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Import_Function => Import_Function : declare
            Args  : Args_List (1 .. 7);
            Names : constant Name_List (1 .. 7) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Mechanism,
                      Name_Result_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Result_Type              : Node_Id renames Args (4);
            Mechanism                : Node_Id renames Args (5);
            Result_Mechanism         : Node_Id renames Args (6);
            First_Optional_Parameter : Node_Id renames Args (7);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Result_Type              => Result_Type,
              Arg_Mechanism                => Mechanism,
              Arg_Result_Mechanism         => Result_Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Function;

         -------------------
         -- Import_Object --
         -------------------

         --  pragma Import_Object (
         --        [Internal =>] LOCAL_NAME
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         when Pragma_Import_Object => Import_Object : declare
            Args  : Args_List (1 .. 3);
            Names : constant Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Import_Object;

         ----------------------
         -- Import_Procedure --
         ----------------------

         --  pragma Import_Procedure (
         --        [Internal                 =>] LOCAL_NAME
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Import_Procedure => Import_Procedure : declare
            Args  : Args_List (1 .. 5);
            Names : constant Name_List (1 .. 5) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Mechanism                : Node_Id renames Args (4);
            First_Optional_Parameter : Node_Id renames Args (5);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Mechanism                => Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Procedure;

         -----------------------------
         -- Import_Valued_Procedure --
         -----------------------------

         --  pragma Import_Valued_Procedure (
         --        [Internal                 =>] LOCAL_NAME
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         --  EXTERNAL_SYMBOL ::=
         --    IDENTIFIER
         --  | static_string_EXPRESSION

         --  PARAMETER_TYPES ::=
         --    null
         --  | TYPE_DESIGNATOR @{, TYPE_DESIGNATOR@}

         --  TYPE_DESIGNATOR ::=
         --    subtype_NAME
         --  | subtype_Name ' Access

         --  MECHANISM ::=
         --    MECHANISM_NAME
         --  | (MECHANISM_ASSOCIATION @{, MECHANISM_ASSOCIATION@})

         --  MECHANISM_ASSOCIATION ::=
         --    [formal_parameter_NAME =>] MECHANISM_NAME

         --  MECHANISM_NAME ::=
         --    Value
         --  | Reference
         --  | Descriptor [([Class =>] CLASS_NAME)]

         --  CLASS_NAME ::= ubs | ubsb | uba | s | sb | a | nca

         when Pragma_Import_Valued_Procedure =>
         Import_Valued_Procedure : declare
            Args  : Args_List (1 .. 5);
            Names : constant Name_List (1 .. 5) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Mechanism                : Node_Id renames Args (4);
            First_Optional_Parameter : Node_Id renames Args (5);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Mechanism                => Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Valued_Procedure;

         -----------------
         -- Independent --
         -----------------

         --  pragma Independent (LOCAL_NAME);

         when Pragma_Independent => Independent : declare
            E_Id : Node_Id;
            E    : Entity_Id;
            D    : Node_Id;
            K    : Node_Kind;

         begin
            Check_Ada_83_Warning;
            Ada_2012_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Get_Pragma_Arg (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);
            D := Declaration_Node (E);
            K := Nkind (D);

            --  Check duplicate before we chain ourselves!

            Check_Duplicate_Pragma (E);

            --  Check appropriate entity

            if Is_Type (E) then
               if Rep_Item_Too_Early (E, N)
                    or else
                  Rep_Item_Too_Late (E, N)
               then
                  return;
               else
                  Check_First_Subtype (Arg1);
               end if;

            elsif K = N_Object_Declaration
              or else (K = N_Component_Declaration
                       and then Original_Record_Component (E) = E)
            then
               if Rep_Item_Too_Late (E, N) then
                  return;
               end if;

            else
               Error_Pragma_Arg
                 ("inappropriate entity for pragma%", Arg1);
            end if;

            Independence_Checks.Append ((N, E));
         end Independent;

         ----------------------------
         -- Independent_Components --
         ----------------------------

         --  pragma Atomic_Components (array_LOCAL_NAME);

         --  This processing is shared by Volatile_Components

         when Pragma_Independent_Components => Independent_Components : declare
            E_Id : Node_Id;
            E    : Entity_Id;
            D    : Node_Id;
            K    : Node_Kind;

         begin
            Check_Ada_83_Warning;
            Ada_2012_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Get_Pragma_Arg (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            --  Check duplicate before we chain ourselves!

            Check_Duplicate_Pragma (E);

            --  Check appropriate entity

            if Rep_Item_Too_Early (E, N)
                 or else
               Rep_Item_Too_Late (E, N)
            then
               return;
            end if;

            D := Declaration_Node (E);
            K := Nkind (D);

            if (K = N_Full_Type_Declaration
                 and then (Is_Array_Type (E) or else Is_Record_Type (E)))
              or else
                ((Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
                   and then Nkind (D) = N_Object_Declaration
                   and then Nkind (Object_Definition (D)) =
                                       N_Constrained_Array_Definition)
            then
               Independence_Checks.Append ((N, E));

            else
               Error_Pragma_Arg ("inappropriate entity for pragma%", Arg1);
            end if;
         end Independent_Components;

         ------------------------
         -- Initialize_Scalars --
         ------------------------

         --  pragma Initialize_Scalars;

         when Pragma_Initialize_Scalars =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Check_Restriction (No_Initialize_Scalars, N);

            --  Initialize_Scalars creates false positives in CodePeer, and
            --  incorrect negative results in Alfa mode, so ignore this pragma
            --  in these modes.

            if not Restriction_Active (No_Initialize_Scalars)
              and then not (CodePeer_Mode or Alfa_Mode)
            then
               Init_Or_Norm_Scalars := True;
               Initialize_Scalars := True;
            end if;

         ------------
         -- Inline --
         ------------

         --  pragma Inline ( NAME {, NAME} );

         when Pragma_Inline =>

            --  Pragma is active if inlining option is active

            Process_Inline (Inline_Active);

         -------------------
         -- Inline_Always --
         -------------------

         --  pragma Inline_Always ( NAME {, NAME} );

         when Pragma_Inline_Always =>
            GNAT_Pragma;

            --  Pragma always active unless in CodePeer or Alfa mode, since
            --  this causes walk order issues.

            if not (CodePeer_Mode or Alfa_Mode) then
               Process_Inline (True);
            end if;

         --------------------
         -- Inline_Generic --
         --------------------

         --  pragma Inline_Generic (NAME {, NAME});

         when Pragma_Inline_Generic =>
            GNAT_Pragma;
            Process_Generic_List;

         ----------------------
         -- Inspection_Point --
         ----------------------

         --  pragma Inspection_Point [(object_NAME {, object_NAME})];

         when Pragma_Inspection_Point => Inspection_Point : declare
            Arg : Node_Id;
            Exp : Node_Id;

         begin
            if Arg_Count > 0 then
               Arg := Arg1;
               loop
                  Exp := Get_Pragma_Arg (Arg);
                  Analyze (Exp);

                  if not Is_Entity_Name (Exp)
                    or else not Is_Object (Entity (Exp))
                  then
                     Error_Pragma_Arg ("object name required", Arg);
                  end if;

                  Next (Arg);
                  exit when No (Arg);
               end loop;
            end if;
         end Inspection_Point;

         ---------------
         -- Interface --
         ---------------

         --  pragma Interface (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Interface =>
            GNAT_Pragma;
            Check_Arg_Order
              ((Name_Convention,
                Name_Entity,
                Name_External_Name,
                Name_Link_Name));
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Import_Or_Interface;

            --  In Ada 2005, the permission to use Interface (a reserved word)
            --  as a pragma name is considered an obsolescent feature.

            if Ada_Version >= Ada_2005 then
               Check_Restriction
                 (No_Obsolescent_Features, Pragma_Identifier (N));
            end if;

         --------------------
         -- Interface_Name --
         --------------------

         --  pragma Interface_Name (
         --    [  Entity        =>] local_NAME
         --    [,[External_Name =>] static_string_EXPRESSION ]
         --    [,[Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Interface_Name => Interface_Name : declare
            Id     : Node_Id;
            Def_Id : Entity_Id;
            Hom_Id : Entity_Id;
            Found  : Boolean;

         begin
            GNAT_Pragma;
            Check_Arg_Order
              ((Name_Entity, Name_External_Name, Name_Link_Name));
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (3);
            Id := Get_Pragma_Arg (Arg1);
            Analyze (Id);

            if not Is_Entity_Name (Id) then
               Error_Pragma_Arg
                 ("first argument for pragma% must be entity name", Arg1);
            elsif Etype (Id) = Any_Type then
               return;
            else
               Def_Id := Entity (Id);
            end if;

            --  Special DEC-compatible processing for the object case, forces
            --  object to be imported.

            if Ekind (Def_Id) = E_Variable then
               Kill_Size_Check_Code (Def_Id);
               Note_Possible_Modification (Id, Sure => False);

               --  Initialization is not allowed for imported variable

               if Present (Expression (Parent (Def_Id)))
                 and then Comes_From_Source (Expression (Parent (Def_Id)))
               then
                  Error_Msg_Sloc := Sloc (Def_Id);
                  Error_Pragma_Arg
                    ("no initialization allowed for declaration of& #",
                     Arg2);

               else
                  --  For compatibility, support VADS usage of providing both
                  --  pragmas Interface and Interface_Name to obtain the effect
                  --  of a single Import pragma.

                  if Is_Imported (Def_Id)
                    and then Present (First_Rep_Item (Def_Id))
                    and then Nkind (First_Rep_Item (Def_Id)) = N_Pragma
                    and then
                      Pragma_Name (First_Rep_Item (Def_Id)) = Name_Interface
                  then
                     null;
                  else
                     Set_Imported (Def_Id);
                  end if;

                  Set_Is_Public (Def_Id);
                  Process_Interface_Name (Def_Id, Arg2, Arg3);
               end if;

            --  Otherwise must be subprogram

            elsif not Is_Subprogram (Def_Id) then
               Error_Pragma_Arg
                 ("argument of pragma% is not subprogram", Arg1);

            else
               Check_At_Most_N_Arguments (3);
               Hom_Id := Def_Id;
               Found := False;

               --  Loop through homonyms

               loop
                  Def_Id := Get_Base_Subprogram (Hom_Id);

                  if Is_Imported (Def_Id) then
                     Process_Interface_Name (Def_Id, Arg2, Arg3);
                     Found := True;
                  end if;

                  exit when From_Aspect_Specification (N);
                  Hom_Id := Homonym (Hom_Id);

                  exit when No (Hom_Id)
                    or else Scope (Hom_Id) /= Current_Scope;
               end loop;

               if not Found then
                  Error_Pragma_Arg
                    ("argument of pragma% is not imported subprogram",
                     Arg1);
               end if;
            end if;
         end Interface_Name;

         -----------------------
         -- Interrupt_Handler --
         -----------------------

         --  pragma Interrupt_Handler (handler_NAME);

         when Pragma_Interrupt_Handler =>
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;

            if No_Run_Time_Mode then
               Error_Msg_CRT ("Interrupt_Handler pragma", N);
            else
               Check_Interrupt_Or_Attach_Handler;
               Process_Interrupt_Or_Attach_Handler;
            end if;

         ------------------------
         -- Interrupt_Priority --
         ------------------------

         --  pragma Interrupt_Priority [(EXPRESSION)];

         when Pragma_Interrupt_Priority => Interrupt_Priority : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Check_Ada_83_Warning;

            if Arg_Count /= 0 then
               Arg := Get_Pragma_Arg (Arg1);
               Check_Arg_Count (1);
               Check_No_Identifiers;

               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               Preanalyze_Spec_Expression (Arg, RTE (RE_Interrupt_Priority));
            end if;

            if not Nkind_In (P, N_Task_Definition, N_Protected_Definition) then
               Pragma_Misplaced;
               return;

            elsif Has_Pragma_Priority (P) then
               Error_Pragma ("duplicate pragma% not allowed");

            else
               Set_Has_Pragma_Priority (P, True);
               Record_Rep_Item (Defining_Identifier (Parent (P)), N);
            end if;
         end Interrupt_Priority;

         ---------------------
         -- Interrupt_State --
         ---------------------

         --  pragma Interrupt_State (
         --    [Name  =>] INTERRUPT_ID,
         --    [State =>] INTERRUPT_STATE);

         --  INTERRUPT_ID => IDENTIFIER | static_integer_EXPRESSION
         --  INTERRUPT_STATE => System | Runtime | User

         --  Note: if the interrupt id is given as an identifier, then it must
         --  be one of the identifiers in Ada.Interrupts.Names. Otherwise it is
         --  given as a static integer expression which must be in the range of
         --  Ada.Interrupts.Interrupt_ID.

         when Pragma_Interrupt_State => Interrupt_State : declare

            Int_Id : constant Entity_Id := RTE (RE_Interrupt_ID);
            --  This is the entity Ada.Interrupts.Interrupt_ID;

            State_Type : Character;
            --  Set to 's'/'r'/'u' for System/Runtime/User

            IST_Num : Pos;
            --  Index to entry in Interrupt_States table

            Int_Val : Uint;
            --  Value of interrupt

            Arg1X : constant Node_Id := Get_Pragma_Arg (Arg1);
            --  The first argument to the pragma

            Int_Ent : Entity_Id;
            --  Interrupt entity in Ada.Interrupts.Names

         begin
            GNAT_Pragma;
            Check_Arg_Order ((Name_Name, Name_State));
            Check_Arg_Count (2);

            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Optional_Identifier (Arg2, Name_State);
            Check_Arg_Is_Identifier (Arg2);

            --  First argument is identifier

            if Nkind (Arg1X) = N_Identifier then

               --  Search list of names in Ada.Interrupts.Names

               Int_Ent := First_Entity (RTE (RE_Names));
               loop
                  if No (Int_Ent) then
                     Error_Pragma_Arg ("invalid interrupt name", Arg1);

                  elsif Chars (Int_Ent) = Chars (Arg1X) then
                     Int_Val := Expr_Value (Constant_Value (Int_Ent));
                     exit;
                  end if;

                  Next_Entity (Int_Ent);
               end loop;

            --  First argument is not an identifier, so it must be a static
            --  expression of type Ada.Interrupts.Interrupt_ID.

            else
               Check_Arg_Is_Static_Expression (Arg1, Any_Integer);
               Int_Val := Expr_Value (Arg1X);

               if Int_Val < Expr_Value (Type_Low_Bound (Int_Id))
                    or else
                  Int_Val > Expr_Value (Type_High_Bound (Int_Id))
               then
                  Error_Pragma_Arg
                    ("value not in range of type " &
                     """Ada.Interrupts.Interrupt_'I'D""", Arg1);
               end if;
            end if;

            --  Check OK state

            case Chars (Get_Pragma_Arg (Arg2)) is
               when Name_Runtime => State_Type := 'r';
               when Name_System  => State_Type := 's';
               when Name_User    => State_Type := 'u';

               when others =>
                  Error_Pragma_Arg ("invalid interrupt state", Arg2);
            end case;

            --  Check if entry is already stored

            IST_Num := Interrupt_States.First;
            loop
               --  If entry not found, add it

               if IST_Num > Interrupt_States.Last then
                  Interrupt_States.Append
                    ((Interrupt_Number => UI_To_Int (Int_Val),
                      Interrupt_State  => State_Type,
                      Pragma_Loc       => Loc));
                  exit;

               --  Case of entry for the same entry

               elsif Int_Val = Interrupt_States.Table (IST_Num).
                                                           Interrupt_Number
               then
                  --  If state matches, done, no need to make redundant entry

                  exit when
                    State_Type = Interrupt_States.Table (IST_Num).
                                                           Interrupt_State;

                  --  Otherwise if state does not match, error

                  Error_Msg_Sloc :=
                    Interrupt_States.Table (IST_Num).Pragma_Loc;
                  Error_Pragma_Arg
                    ("state conflicts with that given #", Arg2);
                  exit;
               end if;

               IST_Num := IST_Num + 1;
            end loop;
         end Interrupt_State;

         ---------------
         -- Invariant --
         ---------------

         --  pragma Invariant
         --    ([Entity =>]    type_LOCAL_NAME,
         --     [Check  =>]    EXPRESSION
         --     [,[Message =>] String_Expression]);

         when Pragma_Invariant => Invariant : declare
            Type_Id : Node_Id;
            Typ     : Entity_Id;

            Discard : Boolean;
            pragma Unreferenced (Discard);

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments (3);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Check);

            if Arg_Count = 3 then
               Check_Optional_Identifier (Arg3, Name_Message);
               Check_Arg_Is_Static_Expression (Arg3, Standard_String);
            end if;

            Check_Arg_Is_Local_Name (Arg1);

            Type_Id := Get_Pragma_Arg (Arg1);
            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type then
               return;

            --  An invariant must apply to a private type, or appear in the
            --  private part of a package spec and apply to a completion.

            elsif Ekind_In (Typ, E_Private_Type,
                                 E_Record_Type_With_Private,
                                 E_Limited_Private_Type)
            then
               null;

            elsif In_Private_Part (Current_Scope)
              and then Has_Private_Declaration (Typ)
            then
               null;

            elsif In_Private_Part (Current_Scope) then
               Error_Pragma_Arg
                 ("pragma% only allowed for private type " &
                  "declared in visible part", Arg1);

            else
               Error_Pragma_Arg
                 ("pragma% only allowed for private type", Arg1);
            end if;

            --  Note that the type has at least one invariant, and also that
            --  it has inheritable invariants if we have Invariant'Class.

            Set_Has_Invariants (Typ);

            if Class_Present (N) then
               Set_Has_Inheritable_Invariants (Typ);
            end if;

            --  The remaining processing is simply to link the pragma on to
            --  the rep item chain, for processing when the type is frozen.
            --  This is accomplished by a call to Rep_Item_Too_Late.

            Discard := Rep_Item_Too_Late (Typ, N, FOnly => True);
         end Invariant;

         ----------------------
         -- Java_Constructor --
         ----------------------

         --  pragma Java_Constructor ([Entity =>] LOCAL_NAME);

         --  Also handles pragma CIL_Constructor

         when Pragma_CIL_Constructor | Pragma_Java_Constructor =>
         Java_Constructor : declare
            Convention  : Convention_Id;
            Def_Id      : Entity_Id;
            Hom_Id      : Entity_Id;
            Id          : Entity_Id;
            This_Formal : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            Id := Get_Pragma_Arg (Arg1);
            Find_Program_Unit_Name (Id);

            --  If we did not find the name, we are done

            if Etype (Id) = Any_Type then
               return;
            end if;

            --  Check wrong use of pragma in wrong VM target

            if VM_Target = No_VM then
               return;

            elsif VM_Target = CLI_Target
              and then Prag_Id = Pragma_Java_Constructor
            then
               Error_Pragma ("must use pragma 'C'I'L_'Constructor");

            elsif VM_Target = JVM_Target
              and then Prag_Id = Pragma_CIL_Constructor
            then
               Error_Pragma ("must use pragma 'Java_'Constructor");
            end if;

            case Prag_Id is
               when Pragma_CIL_Constructor  => Convention := Convention_CIL;
               when Pragma_Java_Constructor => Convention := Convention_Java;
               when others                  => null;
            end case;

            Hom_Id := Entity (Id);

            --  Loop through homonyms

            loop
               Def_Id := Get_Base_Subprogram (Hom_Id);

               --  The constructor is required to be a function

               if Ekind (Def_Id) /= E_Function then
                  if VM_Target = JVM_Target then
                     Error_Pragma_Arg
                       ("pragma% requires function returning a " &
                        "'Java access type", Def_Id);
                  else
                     Error_Pragma_Arg
                       ("pragma% requires function returning a " &
                        "'C'I'L access type", Def_Id);
                  end if;
               end if;

               --  Check arguments: For tagged type the first formal must be
               --  named "this" and its type must be a named access type
               --  designating a class-wide tagged type that has convention
               --  CIL/Java. The first formal must also have a null default
               --  value. For example:

               --      type Typ is tagged ...
               --      type Ref is access all Typ;
               --      pragma Convention (CIL, Typ);

               --      function New_Typ (This : Ref) return Ref;
               --      function New_Typ (This : Ref; I : Integer) return Ref;
               --      pragma Cil_Constructor (New_Typ);

               --  Reason: The first formal must NOT be a primitive of the
               --  tagged type.

               --  This rule also applies to constructors of delegates used
               --  to interface with standard target libraries. For example:

               --      type Delegate is access procedure ...
               --      pragma Import (CIL, Delegate, ...);

               --      function new_Delegate
               --        (This : Delegate := null; ... ) return Delegate;

               --  For value-types this rule does not apply.

               if not Is_Value_Type (Etype (Def_Id)) then
                  if No (First_Formal (Def_Id)) then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N ("% function must have parameters", Def_Id);
                     return;
                  end if;

                  --  In the JRE library we have several occurrences in which
                  --  the "this" parameter is not the first formal.

                  This_Formal := First_Formal (Def_Id);

                  --  In the JRE library we have several occurrences in which
                  --  the "this" parameter is not the first formal. Search for
                  --  it.

                  if VM_Target = JVM_Target then
                     while Present (This_Formal)
                       and then Get_Name_String (Chars (This_Formal)) /= "this"
                     loop
                        Next_Formal (This_Formal);
                     end loop;

                     if No (This_Formal) then
                        This_Formal := First_Formal (Def_Id);
                     end if;
                  end if;

                  --  Warning: The first parameter should be named "this".
                  --  We temporarily allow it because we have the following
                  --  case in the Java runtime (file s-osinte.ads) ???

                  --    function new_Thread
                  --      (Self_Id : System.Address) return Thread_Id;
                  --    pragma Java_Constructor (new_Thread);

                  if VM_Target = JVM_Target
                    and then Get_Name_String (Chars (First_Formal (Def_Id)))
                               = "self_id"
                    and then Etype (First_Formal (Def_Id)) = RTE (RE_Address)
                  then
                     null;

                  elsif Get_Name_String (Chars (This_Formal)) /= "this" then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N
                       ("first formal of % function must be named `this`",
                        Parent (This_Formal));

                  elsif not Is_Access_Type (Etype (This_Formal)) then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N
                       ("first formal of % function must be an access type",
                        Parameter_Type (Parent (This_Formal)));

                  --  For delegates the type of the first formal must be a
                  --  named access-to-subprogram type (see previous example)

                  elsif Ekind (Etype (Def_Id)) = E_Access_Subprogram_Type
                    and then Ekind (Etype (This_Formal))
                               /= E_Access_Subprogram_Type
                  then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N
                       ("first formal of % function must be a named access" &
                        " to subprogram type",
                        Parameter_Type (Parent (This_Formal)));

                  --  Warning: We should reject anonymous access types because
                  --  the constructor must not be handled as a primitive of the
                  --  tagged type. We temporarily allow it because this profile
                  --  is currently generated by cil2ada???

                  elsif Ekind (Etype (Def_Id)) /= E_Access_Subprogram_Type
                    and then not Ekind_In (Etype (This_Formal),
                                             E_Access_Type,
                                             E_General_Access_Type,
                                             E_Anonymous_Access_Type)
                  then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N
                       ("first formal of % function must be a named access" &
                        " type",
                        Parameter_Type (Parent (This_Formal)));

                  elsif Atree.Convention
                         (Designated_Type (Etype (This_Formal))) /= Convention
                  then
                     Error_Msg_Name_1 := Pname;

                     if Convention = Convention_Java then
                        Error_Msg_N
                          ("pragma% requires convention 'Cil in designated" &
                           " type",
                           Parameter_Type (Parent (This_Formal)));
                     else
                        Error_Msg_N
                          ("pragma% requires convention 'Java in designated" &
                           " type",
                           Parameter_Type (Parent (This_Formal)));
                     end if;

                  elsif No (Expression (Parent (This_Formal)))
                    or else Nkind (Expression (Parent (This_Formal))) /= N_Null
                  then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N
                       ("pragma% requires first formal with default `null`",
                        Parameter_Type (Parent (This_Formal)));
                  end if;
               end if;

               --  Check result type: the constructor must be a function
               --  returning:
               --   * a value type (only allowed in the CIL compiler)
               --   * an access-to-subprogram type with convention Java/CIL
               --   * an access-type designating a type that has convention
               --     Java/CIL.

               if Is_Value_Type (Etype (Def_Id)) then
                  null;

               --  Access-to-subprogram type with convention Java/CIL

               elsif Ekind (Etype (Def_Id)) = E_Access_Subprogram_Type then
                  if Atree.Convention (Etype (Def_Id)) /= Convention then
                     if Convention = Convention_Java then
                        Error_Pragma_Arg
                          ("pragma% requires function returning a " &
                           "'Java access type", Arg1);
                     else
                        pragma Assert (Convention = Convention_CIL);
                        Error_Pragma_Arg
                          ("pragma% requires function returning a " &
                           "'C'I'L access type", Arg1);
                     end if;
                  end if;

               elsif Ekind (Etype (Def_Id)) in Access_Kind then
                  if not Ekind_In (Etype (Def_Id), E_Access_Type,
                                                   E_General_Access_Type)
                    or else
                      Atree.Convention
                        (Designated_Type (Etype (Def_Id))) /= Convention
                  then
                     Error_Msg_Name_1 := Pname;

                     if Convention = Convention_Java then
                        Error_Pragma_Arg
                          ("pragma% requires function returning a named" &
                           "'Java access type", Arg1);
                     else
                        Error_Pragma_Arg
                          ("pragma% requires function returning a named" &
                           "'C'I'L access type", Arg1);
                     end if;
                  end if;
               end if;

               Set_Is_Constructor (Def_Id);
               Set_Convention     (Def_Id, Convention);
               Set_Is_Imported    (Def_Id);

               exit when From_Aspect_Specification (N);
               Hom_Id := Homonym (Hom_Id);

               exit when No (Hom_Id) or else Scope (Hom_Id) /= Current_Scope;
            end loop;
         end Java_Constructor;

         ----------------------
         -- Java_Interface --
         ----------------------

         --  pragma Java_Interface ([Entity =>] LOCAL_NAME);

         when Pragma_Java_Interface => Java_Interface : declare
            Arg : Node_Id;
            Typ : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            Arg := Get_Pragma_Arg (Arg1);
            Analyze (Arg);

            if Etype (Arg) = Any_Type then
               return;
            end if;

            if not Is_Entity_Name (Arg)
              or else not Is_Type (Entity (Arg))
            then
               Error_Pragma_Arg ("pragma% requires a type mark", Arg1);
            end if;

            Typ := Underlying_Type (Entity (Arg));

            --  For now simply check some of the semantic constraints on the
            --  type. This currently leaves out some restrictions on interface
            --  types, namely that the parent type must be java.lang.Object.Typ
            --  and that all primitives of the type should be declared
            --  abstract. ???

            if not Is_Tagged_Type (Typ) or else not Is_Abstract_Type (Typ) then
               Error_Pragma_Arg ("pragma% requires an abstract "
                 & "tagged type", Arg1);

            elsif not Has_Discriminants (Typ)
              or else Ekind (Etype (First_Discriminant (Typ)))
                        /= E_Anonymous_Access_Type
              or else
                not Is_Class_Wide_Type
                      (Designated_Type (Etype (First_Discriminant (Typ))))
            then
               Error_Pragma_Arg
                 ("type must have a class-wide access discriminant", Arg1);
            end if;
         end Java_Interface;

         ----------------
         -- Keep_Names --
         ----------------

         --  pragma Keep_Names ([On => ] local_NAME);

         when Pragma_Keep_Names => Keep_Names : declare
            Arg : Node_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_On);
            Check_Arg_Is_Local_Name (Arg1);

            Arg := Get_Pragma_Arg (Arg1);
            Analyze (Arg);

            if Etype (Arg) = Any_Type then
               return;
            end if;

            if not Is_Entity_Name (Arg)
              or else Ekind (Entity (Arg)) /= E_Enumeration_Type
            then
               Error_Pragma_Arg
                 ("pragma% requires a local enumeration type", Arg1);
            end if;

            Set_Discard_Names (Entity (Arg), False);
         end Keep_Names;

         -------------
         -- License --
         -------------

         --  pragma License (RESTRICTED | UNRESTRICTED | GPL | MODIFIED_GPL);

         when Pragma_License =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Is_Identifier (Arg1);

            declare
               Sind : constant Source_File_Index :=
                        Source_Index (Current_Sem_Unit);

            begin
               case Chars (Get_Pragma_Arg (Arg1)) is
                  when Name_GPL =>
                     Set_License (Sind, GPL);

                  when Name_Modified_GPL =>
                     Set_License (Sind, Modified_GPL);

                  when Name_Restricted =>
                     Set_License (Sind, Restricted);

                  when Name_Unrestricted =>
                     Set_License (Sind, Unrestricted);

                  when others =>
                     Error_Pragma_Arg ("invalid license name", Arg1);
               end case;
            end;

         ---------------
         -- Link_With --
         ---------------

         --  pragma Link_With (string_EXPRESSION {, string_EXPRESSION});

         when Pragma_Link_With => Link_With : declare
            Arg : Node_Id;

         begin
            GNAT_Pragma;

            if Operating_Mode = Generate_Code
              and then In_Extended_Main_Source_Unit (N)
            then
               Check_At_Least_N_Arguments (1);
               Check_No_Identifiers;
               Check_Is_In_Decl_Part_Or_Package_Spec;
               Check_Arg_Is_Static_Expression (Arg1, Standard_String);
               Start_String;

               Arg := Arg1;
               while Present (Arg) loop
                  Check_Arg_Is_Static_Expression (Arg, Standard_String);

                  --  Store argument, converting sequences of spaces to a
                  --  single null character (this is one of the differences
                  --  in processing between Link_With and Linker_Options).

                  Arg_Store : declare
                     C : constant Char_Code := Get_Char_Code (' ');
                     S : constant String_Id :=
                           Strval (Expr_Value_S (Get_Pragma_Arg (Arg)));
                     L : constant Nat := String_Length (S);
                     F : Nat := 1;

                     procedure Skip_Spaces;
                     --  Advance F past any spaces

                     -----------------
                     -- Skip_Spaces --
                     -----------------

                     procedure Skip_Spaces is
                     begin
                        while F <= L and then Get_String_Char (S, F) = C loop
                           F := F + 1;
                        end loop;
                     end Skip_Spaces;

                  --  Start of processing for Arg_Store

                  begin
                     Skip_Spaces; -- skip leading spaces

                     --  Loop through characters, changing any embedded
                     --  sequence of spaces to a single null character (this
                     --  is how Link_With/Linker_Options differ)

                     while F <= L loop
                        if Get_String_Char (S, F) = C then
                           Skip_Spaces;
                           exit when F > L;
                           Store_String_Char (ASCII.NUL);

                        else
                           Store_String_Char (Get_String_Char (S, F));
                           F := F + 1;
                        end if;
                     end loop;
                  end Arg_Store;

                  Arg := Next (Arg);

                  if Present (Arg) then
                     Store_String_Char (ASCII.NUL);
                  end if;
               end loop;

               Store_Linker_Option_String (End_String);
            end if;
         end Link_With;

         ------------------
         -- Linker_Alias --
         ------------------

         --  pragma Linker_Alias (
         --      [Entity =>]  LOCAL_NAME
         --      [Target =>]  static_string_EXPRESSION);

         when Pragma_Linker_Alias =>
            GNAT_Pragma;
            Check_Arg_Order ((Name_Entity, Name_Target));
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Target);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Entity (Get_Pragma_Arg (Arg1)), N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Get_Pragma_Arg (Arg1)));
            end if;

         ------------------------
         -- Linker_Constructor --
         ------------------------

         --  pragma Linker_Constructor (procedure_LOCAL_NAME);

         --  Code is shared with Linker_Destructor

         -----------------------
         -- Linker_Destructor --
         -----------------------

         --  pragma Linker_Destructor (procedure_LOCAL_NAME);

         when Pragma_Linker_Constructor |
              Pragma_Linker_Destructor =>
         Linker_Constructor : declare
            Arg1_X : Node_Id;
            Proc   : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Local_Name (Arg1);
            Arg1_X := Get_Pragma_Arg (Arg1);
            Analyze (Arg1_X);
            Proc := Find_Unique_Parameterless_Procedure (Arg1_X, Arg1);

            if not Is_Library_Level_Entity (Proc) then
               Error_Pragma_Arg
                ("argument for pragma% must be library level entity", Arg1);
            end if;

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Proc, N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Proc);
            end if;
         end Linker_Constructor;

         --------------------
         -- Linker_Options --
         --------------------

         --  pragma Linker_Options (string_EXPRESSION {, string_EXPRESSION});

         when Pragma_Linker_Options => Linker_Options : declare
            Arg : Node_Id;

         begin
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Is_In_Decl_Part_Or_Package_Spec;
            Check_Arg_Is_Static_Expression (Arg1, Standard_String);
            Start_String (Strval (Expr_Value_S (Get_Pragma_Arg (Arg1))));

            Arg := Arg2;
            while Present (Arg) loop
               Check_Arg_Is_Static_Expression (Arg, Standard_String);
               Store_String_Char (ASCII.NUL);
               Store_String_Chars
                 (Strval (Expr_Value_S (Get_Pragma_Arg (Arg))));
               Arg := Next (Arg);
            end loop;

            if Operating_Mode = Generate_Code
              and then In_Extended_Main_Source_Unit (N)
            then
               Store_Linker_Option_String (End_String);
            end if;
         end Linker_Options;

         --------------------
         -- Linker_Section --
         --------------------

         --  pragma Linker_Section (
         --      [Entity  =>]  LOCAL_NAME
         --      [Section =>]  static_string_EXPRESSION);

         when Pragma_Linker_Section =>
            GNAT_Pragma;
            Check_Arg_Order ((Name_Entity, Name_Section));
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Section);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);

            --  This pragma applies only to objects

            if not Is_Object (Entity (Get_Pragma_Arg (Arg1))) then
               Error_Pragma_Arg ("pragma% applies only to objects", Arg1);
            end if;

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Entity (Get_Pragma_Arg (Arg1)), N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Get_Pragma_Arg (Arg1)));
            end if;

         ----------
         -- List --
         ----------

         --  pragma List (On | Off)

         --  There is nothing to do here, since we did all the processing for
         --  this pragma in Par.Prag (so that it works properly even in syntax
         --  only mode).

         when Pragma_List =>
            null;

         --------------------
         -- Locking_Policy --
         --------------------

         --  pragma Locking_Policy (policy_IDENTIFIER);

         when Pragma_Locking_Policy => declare
            subtype LP_Range is Name_Id
              range First_Locking_Policy_Name .. Last_Locking_Policy_Name;
            LP_Val : LP_Range;
            LP     : Character;
         begin
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Locking_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            LP_Val := Chars (Get_Pragma_Arg (Arg1));

            case LP_Val is
               when Name_Ceiling_Locking            => LP := 'C';
               when Name_Inheritance_Locking        => LP := 'I';
               when Name_Concurrent_Readers_Locking => LP := 'R';
            end case;

            if Locking_Policy /= ' '
              and then Locking_Policy /= LP
            then
               Error_Msg_Sloc := Locking_Policy_Sloc;
               Error_Pragma ("locking policy incompatible with policy#");

            --  Set new policy, but always preserve System_Location since we
            --  like the error message with the run time name.

            else
               Locking_Policy := LP;

               if Locking_Policy_Sloc /= System_Location then
                  Locking_Policy_Sloc := Loc;
               end if;
            end if;
         end;

         ----------------
         -- Long_Float --
         ----------------

         --  pragma Long_Float (D_Float | G_Float);

         when Pragma_Long_Float => Long_Float : declare
         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifier (Arg1);
            Check_Arg_Is_One_Of (Arg1, Name_D_Float, Name_G_Float);

            if not OpenVMS_On_Target then
               Error_Pragma ("?pragma% ignored (applies only to Open'V'M'S)");
            end if;

            --  D_Float case

            if Chars (Get_Pragma_Arg (Arg1)) = Name_D_Float then
               if Opt.Float_Format_Long = 'G' then
                  Error_Pragma_Arg
                    ("G_Float previously specified", Arg1);

               elsif Current_Sem_Unit /= Main_Unit
                 and then Opt.Float_Format_Long /= 'D'
               then
                  Error_Pragma_Arg
                    ("main unit not compiled with pragma Long_Float (D_Float)",
                     "\pragma% must be used consistently for whole partition",
                     Arg1);

               else
                  Opt.Float_Format_Long := 'D';
               end if;

            --  G_Float case (this is the default, does not need overriding)

            else
               if Opt.Float_Format_Long = 'D' then
                  Error_Pragma ("D_Float previously specified");

               elsif Current_Sem_Unit /= Main_Unit
                 and then Opt.Float_Format_Long /= 'G'
               then
                  Error_Pragma_Arg
                    ("main unit not compiled with pragma Long_Float (G_Float)",
                     "\pragma% must be used consistently for whole partition",
                     Arg1);

               else
                  Opt.Float_Format_Long := 'G';
               end if;
            end if;

            Set_Standard_Fpt_Formats;
         end Long_Float;

         -----------------------
         -- Machine_Attribute --
         -----------------------

         --  pragma Machine_Attribute (
         --       [Entity         =>] LOCAL_NAME,
         --       [Attribute_Name =>] static_string_EXPRESSION
         --    [, [Info           =>] static_EXPRESSION] );

         when Pragma_Machine_Attribute => Machine_Attribute : declare
            Def_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Order ((Name_Entity, Name_Attribute_Name, Name_Info));

            if Arg_Count = 3 then
               Check_Optional_Identifier (Arg3, Name_Info);
               Check_Arg_Is_Static_Expression (Arg3);
            else
               Check_Arg_Count (2);
            end if;

            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Attribute_Name);
            Check_Arg_Is_Local_Name (Arg1);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);
            Def_Id := Entity (Get_Pragma_Arg (Arg1));

            if Is_Access_Type (Def_Id) then
               Def_Id := Designated_Type (Def_Id);
            end if;

            if Rep_Item_Too_Early (Def_Id, N) then
               return;
            end if;

            Def_Id := Underlying_Type (Def_Id);

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Def_Id, N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Get_Pragma_Arg (Arg1)));
            end if;
         end Machine_Attribute;

         ----------
         -- Main --
         ----------

         --  pragma Main
         --   (MAIN_OPTION [, MAIN_OPTION]);

         --  MAIN_OPTION ::=
         --    [STACK_SIZE              =>] static_integer_EXPRESSION
         --  | [TASK_STACK_SIZE_DEFAULT =>] static_integer_EXPRESSION
         --  | [TIME_SLICING_ENABLED    =>] static_boolean_EXPRESSION

         when Pragma_Main => Main : declare
            Args  : Args_List (1 .. 3);
            Names : constant Name_List (1 .. 3) := (
                      Name_Stack_Size,
                      Name_Task_Stack_Size_Default,
                      Name_Time_Slicing_Enabled);

            Nod : Node_Id;

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);

            for J in 1 .. 2 loop
               if Present (Args (J)) then
                  Check_Arg_Is_Static_Expression (Args (J), Any_Integer);
               end if;
            end loop;

            if Present (Args (3)) then
               Check_Arg_Is_Static_Expression (Args (3), Standard_Boolean);
            end if;

            Nod := Next (N);
            while Present (Nod) loop
               if Nkind (Nod) = N_Pragma
                 and then Pragma_Name (Nod) = Name_Main
               then
                  Error_Msg_Name_1 := Pname;
                  Error_Msg_N ("duplicate pragma% not permitted", Nod);
               end if;

               Next (Nod);
            end loop;
         end Main;

         ------------------
         -- Main_Storage --
         ------------------

         --  pragma Main_Storage
         --   (MAIN_STORAGE_OPTION [, MAIN_STORAGE_OPTION]);

         --  MAIN_STORAGE_OPTION ::=
         --    [WORKING_STORAGE =>] static_SIMPLE_EXPRESSION
         --  | [TOP_GUARD =>] static_SIMPLE_EXPRESSION

         when Pragma_Main_Storage => Main_Storage : declare
            Args  : Args_List (1 .. 2);
            Names : constant Name_List (1 .. 2) := (
                      Name_Working_Storage,
                      Name_Top_Guard);

            Nod : Node_Id;

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);

            for J in 1 .. 2 loop
               if Present (Args (J)) then
                  Check_Arg_Is_Static_Expression (Args (J), Any_Integer);
               end if;
            end loop;

            Check_In_Main_Program;

            Nod := Next (N);
            while Present (Nod) loop
               if Nkind (Nod) = N_Pragma
                 and then Pragma_Name (Nod) = Name_Main_Storage
               then
                  Error_Msg_Name_1 := Pname;
                  Error_Msg_N ("duplicate pragma% not permitted", Nod);
               end if;

               Next (Nod);
            end loop;
         end Main_Storage;

         -----------------
         -- Memory_Size --
         -----------------

         --  pragma Memory_Size (NUMERIC_LITERAL)

         when Pragma_Memory_Size =>
            GNAT_Pragma;

            --  Memory size is simply ignored

            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Integer_Literal (Arg1);

         -------------
         -- No_Body --
         -------------

         --  pragma No_Body;

         --  The only correct use of this pragma is on its own in a file, in
         --  which case it is specially processed (see Gnat1drv.Check_Bad_Body
         --  and Frontend, which use Sinput.L.Source_File_Is_Pragma_No_Body to
         --  check for a file containing nothing but a No_Body pragma). If we
         --  attempt to process it during normal semantics processing, it means
         --  it was misplaced.

         when Pragma_No_Body =>
            GNAT_Pragma;
            Pragma_Misplaced;

         ---------------
         -- No_Return --
         ---------------

         --  pragma No_Return (procedure_LOCAL_NAME {, procedure_Local_Name});

         when Pragma_No_Return => No_Return : declare
            Id    : Node_Id;
            E     : Entity_Id;
            Found : Boolean;
            Arg   : Node_Id;

         begin
            Ada_2005_Pragma;
            Check_At_Least_N_Arguments (1);

            --  Loop through arguments of pragma

            Arg := Arg1;
            while Present (Arg) loop
               Check_Arg_Is_Local_Name (Arg);
               Id := Get_Pragma_Arg (Arg);
               Analyze (Id);

               if not Is_Entity_Name (Id) then
                  Error_Pragma_Arg ("entity name required", Arg);
               end if;

               if Etype (Id) = Any_Type then
                  raise Pragma_Exit;
               end if;

               --  Loop to find matching procedures

               E := Entity (Id);
               Found := False;
               while Present (E)
                 and then Scope (E) = Current_Scope
               loop
                  if Ekind_In (E, E_Procedure, E_Generic_Procedure) then
                     Set_No_Return (E);

                     --  Set flag on any alias as well

                     if Is_Overloadable (E) and then Present (Alias (E)) then
                        Set_No_Return (Alias (E));
                     end if;

                     Found := True;
                  end if;

                  exit when From_Aspect_Specification (N);
                  E := Homonym (E);
               end loop;

               if not Found then
                  Error_Pragma_Arg ("no procedure & found for pragma%", Arg);
               end if;

               Next (Arg);
            end loop;
         end No_Return;

         -----------------
         -- No_Run_Time --
         -----------------

         --  pragma No_Run_Time;

         --  Note: this pragma is retained for backwards compatibility. See
         --  body of Rtsfind for full details on its handling.

         when Pragma_No_Run_Time =>
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (0);

            No_Run_Time_Mode           := True;
            Configurable_Run_Time_Mode := True;

            --  Set Duration to 32 bits if word size is 32

            if Ttypes.System_Word_Size = 32 then
               Duration_32_Bits_On_Target := True;
            end if;

            --  Set appropriate restrictions

            Set_Restriction (No_Finalization, N);
            Set_Restriction (No_Exception_Handlers, N);
            Set_Restriction (Max_Tasks, N, 0);
            Set_Restriction (No_Tasking, N);

         ------------------------
         -- No_Strict_Aliasing --
         ------------------------

         --  pragma No_Strict_Aliasing [([Entity =>] type_LOCAL_NAME)];

         when Pragma_No_Strict_Aliasing => No_Strict_Aliasing : declare
            E_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_At_Most_N_Arguments (1);

            if Arg_Count = 0 then
               Check_Valid_Configuration_Pragma;
               Opt.No_Strict_Aliasing := True;

            else
               Check_Optional_Identifier (Arg2, Name_Entity);
               Check_Arg_Is_Local_Name (Arg1);
               E_Id := Entity (Get_Pragma_Arg (Arg1));

               if E_Id = Any_Type then
                  return;
               elsif No (E_Id) or else not Is_Access_Type (E_Id) then
                  Error_Pragma_Arg ("pragma% requires access type", Arg1);
               end if;

               Set_No_Strict_Aliasing (Implementation_Base_Type (E_Id));
            end if;
         end No_Strict_Aliasing;

         -----------------------
         -- Normalize_Scalars --
         -----------------------

         --  pragma Normalize_Scalars;

         when Pragma_Normalize_Scalars =>
            Check_Ada_83_Warning;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;

            --  Normalize_Scalars creates false positives in CodePeer, and
            --  incorrect negative results in Alfa mode, so ignore this pragma
            --  in these modes.

            if not (CodePeer_Mode or Alfa_Mode) then
               Normalize_Scalars := True;
               Init_Or_Norm_Scalars := True;
            end if;

         -----------------
         -- Obsolescent --
         -----------------

         --  pragma Obsolescent;

         --  pragma Obsolescent (
         --    [Message =>] static_string_EXPRESSION
         --  [,[Version =>] Ada_05]]);

         --  pragma Obsolescent (
         --    [Entity  =>] NAME
         --  [,[Message =>] static_string_EXPRESSION
         --  [,[Version =>] Ada_05]] );

         when Pragma_Obsolescent => Obsolescent : declare
            Ename : Node_Id;
            Decl  : Node_Id;

            procedure Set_Obsolescent (E : Entity_Id);
            --  Given an entity Ent, mark it as obsolescent if appropriate

            ---------------------
            -- Set_Obsolescent --
            ---------------------

            procedure Set_Obsolescent (E : Entity_Id) is
               Active : Boolean;
               Ent    : Entity_Id;
               S      : String_Id;

            begin
               Active := True;
               Ent    := E;

               --  Entity name was given

               if Present (Ename) then

                  --  If entity name matches, we are fine. Save entity in
                  --  pragma argument, for ASIS use.

                  if Chars (Ename) = Chars (Ent) then
                     Set_Entity (Ename, Ent);
                     Generate_Reference (Ent, Ename);

                  --  If entity name does not match, only possibility is an
                  --  enumeration literal from an enumeration type declaration.

                  elsif Ekind (Ent) /= E_Enumeration_Type then
                     Error_Pragma
                       ("pragma % entity name does not match declaration");

                  else
                     Ent := First_Literal (E);
                     loop
                        if No (Ent) then
                           Error_Pragma
                             ("pragma % entity name does not match any " &
                              "enumeration literal");

                        elsif Chars (Ent) = Chars (Ename) then
                           Set_Entity (Ename, Ent);
                           Generate_Reference (Ent, Ename);
                           exit;

                        else
                           Ent := Next_Literal (Ent);
                        end if;
                     end loop;
                  end if;
               end if;

               --  Ent points to entity to be marked

               if Arg_Count >= 1 then

                  --  Deal with static string argument

                  Check_Arg_Is_Static_Expression (Arg1, Standard_String);
                  S := Strval (Get_Pragma_Arg (Arg1));

                  for J in 1 .. String_Length (S) loop
                     if not In_Character_Range (Get_String_Char (S, J)) then
                        Error_Pragma_Arg
                          ("pragma% argument does not allow wide characters",
                           Arg1);
                     end if;
                  end loop;

                  Obsolescent_Warnings.Append
                    ((Ent => Ent, Msg => Strval (Get_Pragma_Arg (Arg1))));

                  --  Check for Ada_05 parameter

                  if Arg_Count /= 1 then
                     Check_Arg_Count (2);

                     declare
                        Argx : constant Node_Id := Get_Pragma_Arg (Arg2);

                     begin
                        Check_Arg_Is_Identifier (Argx);

                        if Chars (Argx) /= Name_Ada_05 then
                           Error_Msg_Name_2 := Name_Ada_05;
                           Error_Pragma_Arg
                             ("only allowed argument for pragma% is %", Argx);
                        end if;

                        if Ada_Version_Explicit < Ada_2005
                          or else not Warn_On_Ada_2005_Compatibility
                        then
                           Active := False;
                        end if;
                     end;
                  end if;
               end if;

               --  Set flag if pragma active

               if Active then
                  Set_Is_Obsolescent (Ent);
               end if;

               return;
            end Set_Obsolescent;

         --  Start of processing for pragma Obsolescent

         begin
            GNAT_Pragma;

            Check_At_Most_N_Arguments (3);

            --  See if first argument specifies an entity name

            if Arg_Count >= 1
              and then
                (Chars (Arg1) = Name_Entity
                   or else
                     Nkind_In (Get_Pragma_Arg (Arg1), N_Character_Literal,
                                                      N_Identifier,
                                                      N_Operator_Symbol))
            then
               Ename := Get_Pragma_Arg (Arg1);

               --  Eliminate first argument, so we can share processing

               Arg1 := Arg2;
               Arg2 := Arg3;
               Arg_Count := Arg_Count - 1;

            --  No Entity name argument given

            else
               Ename := Empty;
            end if;

            if Arg_Count >= 1 then
               Check_Optional_Identifier (Arg1, Name_Message);

               if Arg_Count = 2 then
                  Check_Optional_Identifier (Arg2, Name_Version);
               end if;
            end if;

            --  Get immediately preceding declaration

            Decl := Prev (N);
            while Present (Decl) and then Nkind (Decl) = N_Pragma loop
               Prev (Decl);
            end loop;

            --  Cases where we do not follow anything other than another pragma

            if No (Decl) then

               --  First case: library level compilation unit declaration with
               --  the pragma immediately following the declaration.

               if Nkind (Parent (N)) = N_Compilation_Unit_Aux then
                  Set_Obsolescent
                    (Defining_Entity (Unit (Parent (Parent (N)))));
                  return;

               --  Case 2: library unit placement for package

               else
                  declare
                     Ent : constant Entity_Id := Find_Lib_Unit_Name;
                  begin
                     if Is_Package_Or_Generic_Package (Ent) then
                        Set_Obsolescent (Ent);
                        return;
                     end if;
                  end;
               end if;

            --  Cases where we must follow a declaration

            else
               if         Nkind (Decl) not in N_Declaration
                 and then Nkind (Decl) not in N_Later_Decl_Item
                 and then Nkind (Decl) not in N_Generic_Declaration
                 and then Nkind (Decl) not in N_Renaming_Declaration
               then
                  Error_Pragma
                    ("pragma% misplaced, "
                     & "must immediately follow a declaration");

               else
                  Set_Obsolescent (Defining_Entity (Decl));
                  return;
               end if;
            end if;
         end Obsolescent;

         --------------
         -- Optimize --
         --------------

         --  pragma Optimize (Time | Space | Off);

         --  The actual check for optimize is done in Gigi. Note that this
         --  pragma does not actually change the optimization setting, it
         --  simply checks that it is consistent with the pragma.

         when Pragma_Optimize =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Time, Name_Space, Name_Off);

         ------------------------
         -- Optimize_Alignment --
         ------------------------

         --  pragma Optimize_Alignment (Time | Space | Off);

         when Pragma_Optimize_Alignment => Optimize_Alignment : begin
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Valid_Configuration_Pragma;

            declare
               Nam : constant Name_Id := Chars (Get_Pragma_Arg (Arg1));
            begin
               case Nam is
                  when Name_Time =>
                     Opt.Optimize_Alignment := 'T';
                  when Name_Space =>
                     Opt.Optimize_Alignment := 'S';
                  when Name_Off =>
                     Opt.Optimize_Alignment := 'O';
                  when others =>
                     Error_Pragma_Arg ("invalid argument for pragma%", Arg1);
               end case;
            end;

            --  Set indication that mode is set locally. If we are in fact in a
            --  configuration pragma file, this setting is harmless since the
            --  switch will get reset anyway at the start of each unit.

            Optimize_Alignment_Local := True;
         end Optimize_Alignment;

         -------------
         -- Ordered --
         -------------

         --  pragma Ordered (first_enumeration_subtype_LOCAL_NAME);

         when Pragma_Ordered => Ordered : declare
            Assoc   : constant Node_Id := Arg1;
            Type_Id : Node_Id;
            Typ     : Entity_Id;

         begin
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Type_Id := Get_Pragma_Arg (Assoc);
            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if not Is_Enumeration_Type (Typ) then
               Error_Pragma ("pragma% must specify enumeration type");
            end if;

            Check_First_Subtype (Arg1);
            Set_Has_Pragma_Ordered (Base_Type (Typ));
         end Ordered;

         ----------
         -- Pack --
         ----------

         --  pragma Pack (first_subtype_LOCAL_NAME);

         when Pragma_Pack => Pack : declare
            Assoc   : constant Node_Id := Arg1;
            Type_Id : Node_Id;
            Typ     : Entity_Id;
            Ctyp    : Entity_Id;
            Ignore  : Boolean := False;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Type_Id := Get_Pragma_Arg (Assoc);
            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type
              or else Rep_Item_Too_Early (Typ, N)
            then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if not Is_Array_Type (Typ) and then not Is_Record_Type (Typ) then
               Error_Pragma ("pragma% must specify array or record type");
            end if;

            Check_First_Subtype (Arg1);
            Check_Duplicate_Pragma (Typ);

            --  Array type

            if Is_Array_Type (Typ) then
               Ctyp := Component_Type (Typ);

               --  Ignore pack that does nothing

               if Known_Static_Esize (Ctyp)
                 and then Known_Static_RM_Size (Ctyp)
                 and then Esize (Ctyp) = RM_Size (Ctyp)
                 and then Addressable (Esize (Ctyp))
               then
                  Ignore := True;
               end if;

               --  Process OK pragma Pack. Note that if there is a separate
               --  component clause present, the Pack will be cancelled. This
               --  processing is in Freeze.

               if not Rep_Item_Too_Late (Typ, N) then

                  --  In the context of static code analysis, we do not need
                  --  complex front-end expansions related to pragma Pack,
                  --  so disable handling of pragma Pack in these cases.

                  if CodePeer_Mode or Alfa_Mode then
                     null;

                  --  Don't attempt any packing for VM targets. We possibly
                  --  could deal with some cases of array bit-packing, but we
                  --  don't bother, since this is not a typical kind of
                  --  representation in the VM context anyway (and would not
                  --  for example work nicely with the debugger).

                  elsif VM_Target /= No_VM then
                     if not GNAT_Mode then
                        Error_Pragma
                          ("?pragma% ignored in this configuration");
                     end if;

                  --  Normal case where we do the pack action

                  else
                     if not Ignore then
                        Set_Is_Packed            (Base_Type (Typ));
                        Set_Has_Non_Standard_Rep (Base_Type (Typ));
                     end if;

                     Set_Has_Pragma_Pack (Base_Type (Typ));
                  end if;
               end if;

            --  For record types, the pack is always effective

            else pragma Assert (Is_Record_Type (Typ));
               if not Rep_Item_Too_Late (Typ, N) then

                  --  Ignore pack request with warning in VM mode (skip warning
                  --  if we are compiling GNAT run time library).

                  if VM_Target /= No_VM then
                     if not GNAT_Mode then
                        Error_Pragma
                          ("?pragma% ignored in this configuration");
                     end if;

                  --  Normal case of pack request active

                  else
                     Set_Is_Packed            (Base_Type (Typ));
                     Set_Has_Pragma_Pack      (Base_Type (Typ));
                     Set_Has_Non_Standard_Rep (Base_Type (Typ));
                  end if;
               end if;
            end if;
         end Pack;

         ----------
         -- Page --
         ----------

         --  pragma Page;

         --  There is nothing to do here, since we did all the processing for
         --  this pragma in Par.Prag (so that it works properly even in syntax
         --  only mode).

         when Pragma_Page =>
            null;

         -------------
         -- Passive --
         -------------

         --  pragma Passive [(PASSIVE_FORM)];

         --  PASSIVE_FORM ::= Semaphore | No

         when Pragma_Passive =>
            GNAT_Pragma;

            if Nkind (Parent (N)) /= N_Task_Definition then
               Error_Pragma ("pragma% must be within task definition");
            end if;

            if Arg_Count /= 0 then
               Check_Arg_Count (1);
               Check_Arg_Is_One_Of (Arg1, Name_Semaphore, Name_No);
            end if;

         ----------------------------------
         -- Preelaborable_Initialization --
         ----------------------------------

         --  pragma Preelaborable_Initialization (DIRECT_NAME);

         when Pragma_Preelaborable_Initialization => Preelab_Init : declare
            Ent : Entity_Id;

         begin
            Ada_2005_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Identifier (Arg1);
            Check_Arg_Is_Local_Name (Arg1);
            Check_First_Subtype (Arg1);
            Ent := Entity (Get_Pragma_Arg (Arg1));

            if not (Is_Private_Type (Ent)
                      or else
                    Is_Protected_Type (Ent)
                      or else
                    (Is_Generic_Type (Ent) and then Is_Derived_Type (Ent)))
            then
               Error_Pragma_Arg
                 ("pragma % can only be applied to private, formal derived or "
                  & "protected type",
                  Arg1);
            end if;

            --  Give an error if the pragma is applied to a protected type that
            --  does not qualify (due to having entries, or due to components
            --  that do not qualify).

            if Is_Protected_Type (Ent)
              and then not Has_Preelaborable_Initialization (Ent)
            then
               Error_Msg_N
                 ("protected type & does not have preelaborable " &
                  "initialization", Ent);

            --  Otherwise mark the type as definitely having preelaborable
            --  initialization.

            else
               Set_Known_To_Have_Preelab_Init (Ent);
            end if;

            if Has_Pragma_Preelab_Init (Ent)
              and then Warn_On_Redundant_Constructs
            then
               Error_Pragma ("?duplicate pragma%!");
            else
               Set_Has_Pragma_Preelab_Init (Ent);
            end if;
         end Preelab_Init;

         --------------------
         -- Persistent_BSS --
         --------------------

         --  pragma Persistent_BSS [(object_NAME)];

         when Pragma_Persistent_BSS => Persistent_BSS :  declare
            Decl : Node_Id;
            Ent  : Entity_Id;
            Prag : Node_Id;

         begin
            GNAT_Pragma;
            Check_At_Most_N_Arguments (1);

            --  Case of application to specific object (one argument)

            if Arg_Count = 1 then
               Check_Arg_Is_Library_Level_Local_Name (Arg1);

               if not Is_Entity_Name (Get_Pragma_Arg (Arg1))
                 or else not
                  Ekind_In (Entity (Get_Pragma_Arg (Arg1)), E_Variable,
                                                            E_Constant)
               then
                  Error_Pragma_Arg ("pragma% only applies to objects", Arg1);
               end if;

               Ent := Entity (Get_Pragma_Arg (Arg1));
               Decl := Parent (Ent);

               if Rep_Item_Too_Late (Ent, N) then
                  return;
               end if;

               if Present (Expression (Decl)) then
                  Error_Pragma_Arg
                    ("object for pragma% cannot have initialization", Arg1);
               end if;

               if not Is_Potentially_Persistent_Type (Etype (Ent)) then
                  Error_Pragma_Arg
                    ("object type for pragma% is not potentially persistent",
                     Arg1);
               end if;

               Check_Duplicate_Pragma (Ent);

               Prag :=
                 Make_Linker_Section_Pragma
                   (Ent, Sloc (N), ".persistent.bss");
               Insert_After (N, Prag);
               Analyze (Prag);

            --  Case of use as configuration pragma with no arguments

            else
               Check_Valid_Configuration_Pragma;
               Persistent_BSS_Mode := True;
            end if;
         end Persistent_BSS;

         -------------
         -- Polling --
         -------------

         --  pragma Polling (ON | OFF);

         when Pragma_Polling =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);
            Polling_Required := (Chars (Get_Pragma_Arg (Arg1)) = Name_On);

         -------------------
         -- Postcondition --
         -------------------

         --  pragma Postcondition ([Check   =>] Boolean_EXPRESSION
         --                      [,[Message =>] String_EXPRESSION]);

         when Pragma_Postcondition => Postcondition : declare
            In_Body : Boolean;
            pragma Warnings (Off, In_Body);

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_At_Most_N_Arguments (2);
            Check_Optional_Identifier (Arg1, Name_Check);

            --  All we need to do here is call the common check procedure,
            --  the remainder of the processing is found in Sem_Ch6/Sem_Ch7.

            Check_Precondition_Postcondition (In_Body);
         end Postcondition;

         ------------------
         -- Precondition --
         ------------------

         --  pragma Precondition ([Check   =>] Boolean_EXPRESSION
         --                     [,[Message =>] String_EXPRESSION]);

         when Pragma_Precondition => Precondition : declare
            In_Body : Boolean;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_At_Most_N_Arguments (2);
            Check_Optional_Identifier (Arg1, Name_Check);
            Check_Precondition_Postcondition (In_Body);

            --  If in spec, nothing more to do. If in body, then we convert the
            --  pragma to pragma Check (Precondition, cond [, msg]). Note we do
            --  this whether or not precondition checks are enabled. That works
            --  fine since pragma Check will do this check, and will also
            --  analyze the condition itself in the proper context.

            if In_Body then
               Rewrite (N,
                 Make_Pragma (Loc,
                   Chars => Name_Check,
                   Pragma_Argument_Associations => New_List (
                     Make_Pragma_Argument_Association (Loc,
                       Expression => Make_Identifier (Loc, Name_Precondition)),

                     Make_Pragma_Argument_Association (Sloc (Arg1),
                       Expression => Relocate_Node (Get_Pragma_Arg (Arg1))))));

               if Arg_Count = 2 then
                  Append_To (Pragma_Argument_Associations (N),
                    Make_Pragma_Argument_Association (Sloc (Arg2),
                      Expression => Relocate_Node (Get_Pragma_Arg (Arg2))));
               end if;

               Analyze (N);
            end if;
         end Precondition;

         ---------------
         -- Predicate --
         ---------------

         --  pragma Predicate
         --    ([Entity =>] type_LOCAL_NAME,
         --     [Check  =>] EXPRESSION);

         when Pragma_Predicate => Predicate : declare
            Type_Id : Node_Id;
            Typ     : Entity_Id;

            Discard : Boolean;
            pragma Unreferenced (Discard);

         begin
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Check);

            Check_Arg_Is_Local_Name (Arg1);

            Type_Id := Get_Pragma_Arg (Arg1);
            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type then
               return;
            end if;

            --  The remaining processing is simply to link the pragma on to
            --  the rep item chain, for processing when the type is frozen.
            --  This is accomplished by a call to Rep_Item_Too_Late. We also
            --  mark the type as having predicates.

            Set_Has_Predicates (Typ);
            Discard := Rep_Item_Too_Late (Typ, N, FOnly => True);
         end Predicate;

         ------------------
         -- Preelaborate --
         ------------------

         --  pragma Preelaborate [(library_unit_NAME)];

         --  Set the flag Is_Preelaborated of program unit name entity

         when Pragma_Preelaborate => Preelaborate : declare
            Pa  : constant Node_Id   := Parent (N);
            Pk  : constant Node_Kind := Nkind (Pa);
            Ent : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Ent := Find_Lib_Unit_Name;
            Check_Duplicate_Pragma (Ent);

            --  This filters out pragmas inside generic parent then
            --  show up inside instantiation

            if Present (Ent)
              and then not (Pk = N_Package_Specification
                             and then Present (Generic_Parent (Pa)))
            then
               if not Debug_Flag_U then
                  Set_Is_Preelaborated (Ent);
                  Set_Suppress_Elaboration_Warnings (Ent);
               end if;
            end if;
         end Preelaborate;

         ---------------------
         -- Preelaborate_05 --
         ---------------------

         --  pragma Preelaborate_05 [(library_unit_NAME)];

         --  This pragma is useable only in GNAT_Mode, where it is used like
         --  pragma Preelaborate but it is only effective in Ada 2005 mode
         --  (otherwise it is ignored). This is used to implement AI-362 which
         --  recategorizes some run-time packages in Ada 2005 mode.

         when Pragma_Preelaborate_05 => Preelaborate_05 : declare
            Ent : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Valid_Library_Unit_Pragma;

            if not GNAT_Mode then
               Error_Pragma ("pragma% only available in GNAT mode");
            end if;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            --  This is one of the few cases where we need to test the value of
            --  Ada_Version_Explicit rather than Ada_Version (which is always
            --  set to Ada_2012 in a predefined unit), we need to know the
            --  explicit version set to know if this pragma is active.

            if Ada_Version_Explicit >= Ada_2005 then
               Ent := Find_Lib_Unit_Name;
               Set_Is_Preelaborated (Ent);
               Set_Suppress_Elaboration_Warnings (Ent);
            end if;
         end Preelaborate_05;

         --------------
         -- Priority --
         --------------

         --  pragma Priority (EXPRESSION);

         when Pragma_Priority => Priority : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);

            --  Subprogram case

            if Nkind (P) = N_Subprogram_Body then
               Check_In_Main_Program;

               Arg := Get_Pragma_Arg (Arg1);
               Analyze_And_Resolve (Arg, Standard_Integer);

               --  Must be static

               if not Is_Static_Expression (Arg) then
                  Flag_Non_Static_Expr
                    ("main subprogram priority is not static!", Arg);
                  raise Pragma_Exit;

               --  If constraint error, then we already signalled an error

               elsif Raises_Constraint_Error (Arg) then
                  null;

               --  Otherwise check in range

               else
                  declare
                     Val : constant Uint := Expr_Value (Arg);

                  begin
                     if Val < 0
                       or else Val > Expr_Value (Expression
                                       (Parent (RTE (RE_Max_Priority))))
                     then
                        Error_Pragma_Arg
                          ("main subprogram priority is out of range", Arg1);
                     end if;
                  end;
               end if;

               Set_Main_Priority
                    (Current_Sem_Unit, UI_To_Int (Expr_Value (Arg)));

               --  Load an arbitrary entity from System.Tasking to make sure
               --  this package is implicitly with'ed, since we need to have
               --  the tasking run-time active for the pragma Priority to have
               --  any effect.

               declare
                  Discard : Entity_Id;
                  pragma Warnings (Off, Discard);
               begin
                  Discard := RTE (RE_Task_List);
               end;

            --  Task or Protected, must be of type Integer

            elsif Nkind_In (P, N_Protected_Definition, N_Task_Definition) then
               Arg := Get_Pragma_Arg (Arg1);

               --  The expression must be analyzed in the special manner
               --  described in "Handling of Default and Per-Object
               --  Expressions" in sem.ads.

               Preanalyze_Spec_Expression (Arg, Standard_Integer);

               if not Is_Static_Expression (Arg) then
                  Check_Restriction (Static_Priorities, Arg);
               end if;

            --  Anything else is incorrect

            else
               Pragma_Misplaced;
            end if;

            if Has_Pragma_Priority (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Pragma_Priority (P, True);

               if Nkind_In (P, N_Protected_Definition, N_Task_Definition) then
                  Record_Rep_Item (Defining_Identifier (Parent (P)), N);
                  --  exp_ch9 should use this ???
               end if;
            end if;
         end Priority;

         -----------------------------------
         -- Priority_Specific_Dispatching --
         -----------------------------------

         --  pragma Priority_Specific_Dispatching (
         --    policy_IDENTIFIER,
         --    first_priority_EXPRESSION,
         --    last_priority_EXPRESSION);

         when Pragma_Priority_Specific_Dispatching =>
         Priority_Specific_Dispatching : declare
            Prio_Id : constant Entity_Id := RTE (RE_Any_Priority);
            --  This is the entity System.Any_Priority;

            DP          : Character;
            Lower_Bound : Node_Id;
            Upper_Bound : Node_Id;
            Lower_Val   : Uint;
            Upper_Val   : Uint;

         begin
            Ada_2005_Pragma;
            Check_Arg_Count (3);
            Check_No_Identifiers;
            Check_Arg_Is_Task_Dispatching_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            Get_Name_String (Chars (Get_Pragma_Arg (Arg1)));
            DP := Fold_Upper (Name_Buffer (1));

            Lower_Bound := Get_Pragma_Arg (Arg2);
            Check_Arg_Is_Static_Expression (Lower_Bound, Standard_Integer);
            Lower_Val := Expr_Value (Lower_Bound);

            Upper_Bound := Get_Pragma_Arg (Arg3);
            Check_Arg_Is_Static_Expression (Upper_Bound, Standard_Integer);
            Upper_Val := Expr_Value (Upper_Bound);

            --  It is not allowed to use Task_Dispatching_Policy and
            --  Priority_Specific_Dispatching in the same partition.

            if Task_Dispatching_Policy /= ' ' then
               Error_Msg_Sloc := Task_Dispatching_Policy_Sloc;
               Error_Pragma
                 ("pragma% incompatible with Task_Dispatching_Policy#");

            --  Check lower bound in range

            elsif Lower_Val < Expr_Value (Type_Low_Bound (Prio_Id))
                    or else
                  Lower_Val > Expr_Value (Type_High_Bound (Prio_Id))
            then
               Error_Pragma_Arg
                 ("first_priority is out of range", Arg2);

            --  Check upper bound in range

            elsif Upper_Val < Expr_Value (Type_Low_Bound (Prio_Id))
                    or else
                  Upper_Val > Expr_Value (Type_High_Bound (Prio_Id))
            then
               Error_Pragma_Arg
                 ("last_priority is out of range", Arg3);

            --  Check that the priority range is valid

            elsif Lower_Val > Upper_Val then
               Error_Pragma
                 ("last_priority_expression must be greater than" &
                  " or equal to first_priority_expression");

            --  Store the new policy, but always preserve System_Location since
            --  we like the error message with the run-time name.

            else
               --  Check overlapping in the priority ranges specified in other
               --  Priority_Specific_Dispatching pragmas within the same
               --  partition. We can only check those we know about!

               for J in
                  Specific_Dispatching.First .. Specific_Dispatching.Last
               loop
                  if Specific_Dispatching.Table (J).First_Priority in
                    UI_To_Int (Lower_Val) .. UI_To_Int (Upper_Val)
                  or else Specific_Dispatching.Table (J).Last_Priority in
                    UI_To_Int (Lower_Val) .. UI_To_Int (Upper_Val)
                  then
                     Error_Msg_Sloc :=
                       Specific_Dispatching.Table (J).Pragma_Loc;
                        Error_Pragma
                          ("priority range overlaps with "
                           & "Priority_Specific_Dispatching#");
                  end if;
               end loop;

               --  The use of Priority_Specific_Dispatching is incompatible
               --  with Task_Dispatching_Policy.

               if Task_Dispatching_Policy /= ' ' then
                  Error_Msg_Sloc := Task_Dispatching_Policy_Sloc;
                     Error_Pragma
                       ("Priority_Specific_Dispatching incompatible "
                        & "with Task_Dispatching_Policy#");
               end if;

               --  The use of Priority_Specific_Dispatching forces ceiling
               --  locking policy.

               if Locking_Policy /= ' ' and then Locking_Policy /= 'C' then
                  Error_Msg_Sloc := Locking_Policy_Sloc;
                     Error_Pragma
                       ("Priority_Specific_Dispatching incompatible "
                        & "with Locking_Policy#");

               --  Set the Ceiling_Locking policy, but preserve System_Location
               --  since we like the error message with the run time name.

               else
                  Locking_Policy := 'C';

                  if Locking_Policy_Sloc /= System_Location then
                     Locking_Policy_Sloc := Loc;
                  end if;
               end if;

               --  Add entry in the table

               Specific_Dispatching.Append
                    ((Dispatching_Policy => DP,
                      First_Priority     => UI_To_Int (Lower_Val),
                      Last_Priority      => UI_To_Int (Upper_Val),
                      Pragma_Loc         => Loc));
            end if;
         end Priority_Specific_Dispatching;

         -------------
         -- Profile --
         -------------

         --  pragma Profile (profile_IDENTIFIER);

         --  profile_IDENTIFIER => Restricted | Ravenscar

         when Pragma_Profile =>
            Ada_2005_Pragma;
            Check_Arg_Count (1);
            Check_Valid_Configuration_Pragma;
            Check_No_Identifiers;

            declare
               Argx : constant Node_Id := Get_Pragma_Arg (Arg1);

            begin
               if Chars (Argx) = Name_Ravenscar then
                  Set_Ravenscar_Profile (N);

               elsif Chars (Argx) = Name_Restricted then
                  Set_Profile_Restrictions
                    (Restricted,
                     N, Warn => Treat_Restrictions_As_Warnings);

               elsif Chars (Argx) = Name_No_Implementation_Extensions then
                  Set_Profile_Restrictions
                    (No_Implementation_Extensions,
                     N, Warn => Treat_Restrictions_As_Warnings);

               else
                  Error_Pragma_Arg ("& is not a valid profile", Argx);
               end if;
            end;

         ----------------------
         -- Profile_Warnings --
         ----------------------

         --  pragma Profile_Warnings (profile_IDENTIFIER);

         --  profile_IDENTIFIER => Restricted | Ravenscar

         when Pragma_Profile_Warnings =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Valid_Configuration_Pragma;
            Check_No_Identifiers;

            declare
               Argx : constant Node_Id := Get_Pragma_Arg (Arg1);

            begin
               if Chars (Argx) = Name_Ravenscar then
                  Set_Profile_Restrictions (Ravenscar, N, Warn => True);

               elsif Chars (Argx) = Name_Restricted then
                  Set_Profile_Restrictions (Restricted, N, Warn => True);

               elsif Chars (Argx) = Name_No_Implementation_Extensions then
                  Set_Profile_Restrictions
                    (No_Implementation_Extensions, N, Warn => True);

               else
                  Error_Pragma_Arg ("& is not a valid profile", Argx);
               end if;
            end;

         --------------------------
         -- Propagate_Exceptions --
         --------------------------

         --  pragma Propagate_Exceptions;

         --  Note: this pragma is obsolete and has no effect

         when Pragma_Propagate_Exceptions =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            if In_Extended_Main_Source_Unit (N) then
               Propagate_Exceptions := True;
            end if;

         ------------------
         -- Psect_Object --
         ------------------

         --  pragma Psect_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         when Pragma_Psect_Object | Pragma_Common_Object =>
         Psect_Object : declare
            Args  : Args_List (1 .. 3);
            Names : constant Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

            Def_Id : Entity_Id;

            procedure Check_Too_Long (Arg : Node_Id);
            --  Posts message if the argument is an identifier with more
            --  than 31 characters, or a string literal with more than
            --  31 characters, and we are operating under VMS

            --------------------
            -- Check_Too_Long --
            --------------------

            procedure Check_Too_Long (Arg : Node_Id) is
               X : constant Node_Id := Original_Node (Arg);

            begin
               if not Nkind_In (X, N_String_Literal, N_Identifier) then
                  Error_Pragma_Arg
                    ("inappropriate argument for pragma %", Arg);
               end if;

               if OpenVMS_On_Target then
                  if (Nkind (X) = N_String_Literal
                       and then String_Length (Strval (X)) > 31)
                    or else
                     (Nkind (X) = N_Identifier
                       and then Length_Of_Name (Chars (X)) > 31)
                  then
                     Error_Pragma_Arg
                       ("argument for pragma % is longer than 31 characters",
                        Arg);
                  end if;
               end if;
            end Check_Too_Long;

         --  Start of processing for Common_Object/Psect_Object

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Internal_Arg (Internal);

            Def_Id := Entity (Internal);

            if not Ekind_In (Def_Id, E_Constant, E_Variable) then
               Error_Pragma_Arg
                 ("pragma% must designate an object", Internal);
            end if;

            Check_Too_Long (Internal);

            if Is_Imported (Def_Id) or else Is_Exported (Def_Id) then
               Error_Pragma_Arg
                 ("cannot use pragma% for imported/exported object",
                  Internal);
            end if;

            if Is_Concurrent_Type (Etype (Internal)) then
               Error_Pragma_Arg
                 ("cannot specify pragma % for task/protected object",
                  Internal);
            end if;

            if Has_Rep_Pragma (Def_Id, Name_Common_Object)
                 or else
               Has_Rep_Pragma (Def_Id, Name_Psect_Object)
            then
               Error_Msg_N ("?duplicate Common/Psect_Object pragma", N);
            end if;

            if Ekind (Def_Id) = E_Constant then
               Error_Pragma_Arg
                 ("cannot specify pragma % for a constant", Internal);
            end if;

            if Is_Record_Type (Etype (Internal)) then
               declare
                  Ent  : Entity_Id;
                  Decl : Entity_Id;

               begin
                  Ent := First_Entity (Etype (Internal));
                  while Present (Ent) loop
                     Decl := Declaration_Node (Ent);

                     if Ekind (Ent) = E_Component
                       and then Nkind (Decl) = N_Component_Declaration
                       and then Present (Expression (Decl))
                       and then Warn_On_Export_Import
                     then
                        Error_Msg_N
                          ("?object for pragma % has defaults", Internal);
                        exit;

                     else
                        Next_Entity (Ent);
                     end if;
                  end loop;
               end;
            end if;

            if Present (Size) then
               Check_Too_Long (Size);
            end if;

            if Present (External) then
               Check_Arg_Is_External_Name (External);
               Check_Too_Long (External);
            end if;

            --  If all error tests pass, link pragma on to the rep item chain

            Record_Rep_Item (Def_Id, N);
         end Psect_Object;

         ----------
         -- Pure --
         ----------

         --  pragma Pure [(library_unit_NAME)];

         when Pragma_Pure => Pure : declare
            Ent : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Ent := Find_Lib_Unit_Name;
            Set_Is_Pure (Ent);
            Set_Has_Pragma_Pure (Ent);
            Set_Suppress_Elaboration_Warnings (Ent);
         end Pure;

         -------------
         -- Pure_05 --
         -------------

         --  pragma Pure_05 [(library_unit_NAME)];

         --  This pragma is useable only in GNAT_Mode, where it is used like
         --  pragma Pure but it is only effective in Ada 2005 mode (otherwise
         --  it is ignored). It may be used after a pragma Preelaborate, in
         --  which case it overrides the effect of the pragma Preelaborate.
         --  This is used to implement AI-362 which recategorizes some run-time
         --  packages in Ada 2005 mode.

         when Pragma_Pure_05 => Pure_05 : declare
            Ent : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Valid_Library_Unit_Pragma;

            if not GNAT_Mode then
               Error_Pragma ("pragma% only available in GNAT mode");
            end if;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            --  This is one of the few cases where we need to test the value of
            --  Ada_Version_Explicit rather than Ada_Version (which is always
            --  set to Ada_2012 in a predefined unit), we need to know the
            --  explicit version set to know if this pragma is active.

            if Ada_Version_Explicit >= Ada_2005 then
               Ent := Find_Lib_Unit_Name;
               Set_Is_Preelaborated (Ent, False);
               Set_Is_Pure (Ent);
               Set_Suppress_Elaboration_Warnings (Ent);
            end if;
         end Pure_05;

         -------------
         -- Pure_12 --
         -------------

         --  pragma Pure_12 [(library_unit_NAME)];

         --  This pragma is useable only in GNAT_Mode, where it is used like
         --  pragma Pure but it is only effective in Ada 2012 mode (otherwise
         --  it is ignored). It may be used after a pragma Preelaborate, in
         --  which case it overrides the effect of the pragma Preelaborate.
         --  This is used to implement AI05-0212 which recategorizes some
         --  run-time packages in Ada 2012 mode.

         when Pragma_Pure_12 => Pure_12 : declare
            Ent : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Valid_Library_Unit_Pragma;

            if not GNAT_Mode then
               Error_Pragma ("pragma% only available in GNAT mode");
            end if;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            --  This is one of the few cases where we need to test the value of
            --  Ada_Version_Explicit rather than Ada_Version (which is always
            --  set to Ada_2012 in a predefined unit), we need to know the
            --  explicit version set to know if this pragma is active.

            if Ada_Version_Explicit >= Ada_2012 then
               Ent := Find_Lib_Unit_Name;
               Set_Is_Preelaborated (Ent, False);
               Set_Is_Pure (Ent);
               Set_Suppress_Elaboration_Warnings (Ent);
            end if;
         end Pure_12;

         -------------------
         -- Pure_Function --
         -------------------

         --  pragma Pure_Function ([Entity =>] function_LOCAL_NAME);

         when Pragma_Pure_Function => Pure_Function : declare
            E_Id      : Node_Id;
            E         : Entity_Id;
            Def_Id    : Entity_Id;
            Effective : Boolean := False;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Get_Pragma_Arg (Arg1);

            if Error_Posted (E_Id) then
               return;
            end if;

            --  Loop through homonyms (overloadings) of referenced entity

            E := Entity (E_Id);

            if Present (E) then
               loop
                  Def_Id := Get_Base_Subprogram (E);

                  if not Ekind_In (Def_Id, E_Function,
                                           E_Generic_Function,
                                           E_Operator)
                  then
                     Error_Pragma_Arg
                       ("pragma% requires a function name", Arg1);
                  end if;

                  Set_Is_Pure (Def_Id);

                  if not Has_Pragma_Pure_Function (Def_Id) then
                     Set_Has_Pragma_Pure_Function (Def_Id);
                     Effective := True;
                  end if;

                  exit when From_Aspect_Specification (N);
                  E := Homonym (E);
                  exit when No (E) or else Scope (E) /= Current_Scope;
               end loop;

               if not Effective
                 and then Warn_On_Redundant_Constructs
               then
                  Error_Msg_NE
                    ("pragma Pure_Function on& is redundant?",
                     N, Entity (E_Id));
               end if;
            end if;
         end Pure_Function;

         --------------------
         -- Queuing_Policy --
         --------------------

         --  pragma Queuing_Policy (policy_IDENTIFIER);

         when Pragma_Queuing_Policy => declare
            QP : Character;

         begin
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Queuing_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            Get_Name_String (Chars (Get_Pragma_Arg (Arg1)));
            QP := Fold_Upper (Name_Buffer (1));

            if Queuing_Policy /= ' '
              and then Queuing_Policy /= QP
            then
               Error_Msg_Sloc := Queuing_Policy_Sloc;
               Error_Pragma ("queuing policy incompatible with policy#");

            --  Set new policy, but always preserve System_Location since we
            --  like the error message with the run time name.

            else
               Queuing_Policy := QP;

               if Queuing_Policy_Sloc /= System_Location then
                  Queuing_Policy_Sloc := Loc;
               end if;
            end if;
         end;

         -----------------------
         -- Relative_Deadline --
         -----------------------

         --  pragma Relative_Deadline (time_span_EXPRESSION);

         when Pragma_Relative_Deadline => Relative_Deadline : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Ada_2005_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);

            Arg := Get_Pragma_Arg (Arg1);

            --  The expression must be analyzed in the special manner described
            --  in "Handling of Default and Per-Object Expressions" in sem.ads.

            Preanalyze_Spec_Expression (Arg, RTE (RE_Time_Span));

            --  Subprogram case

            if Nkind (P) = N_Subprogram_Body then
               Check_In_Main_Program;

            --  Tasks

            elsif Nkind (P) = N_Task_Definition then
               null;

            --  Anything else is incorrect

            else
               Pragma_Misplaced;
            end if;

            if Has_Relative_Deadline_Pragma (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Relative_Deadline_Pragma (P, True);

               if Nkind (P) = N_Task_Definition then
                  Record_Rep_Item (Defining_Identifier (Parent (P)), N);
               end if;
            end if;
         end Relative_Deadline;

         ------------------------
         -- Remote_Access_Type --
         ------------------------

         --  pragma Remote_Access_Type ([Entity =>] formal_type_LOCAL_NAME);

         when Pragma_Remote_Access_Type => Remote_Access_Type : declare
            E : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            E := Entity (Get_Pragma_Arg (Arg1));

            if Nkind (Parent (E)) = N_Formal_Type_Declaration
              and then Ekind (E) = E_General_Access_Type
              and then Is_Class_Wide_Type (Directly_Designated_Type (E))
              and then Scope (Root_Type (Directly_Designated_Type (E)))
                         = Scope (E)
              and then Is_Valid_Remote_Object_Type
                         (Root_Type (Directly_Designated_Type (E)))
            then
               Set_Is_Remote_Types (E);

            else
               Error_Pragma_Arg
                 ("pragma% applies only to formal access to classwide types",
                  Arg1);
            end if;
         end Remote_Access_Type;

         ---------------------------
         -- Remote_Call_Interface --
         ---------------------------

         --  pragma Remote_Call_Interface [(library_unit_NAME)];

         when Pragma_Remote_Call_Interface => Remote_Call_Interface : declare
            Cunit_Node : Node_Id;
            Cunit_Ent  : Entity_Id;
            K          : Node_Kind;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Cunit_Node := Cunit (Current_Sem_Unit);
            K          := Nkind (Unit (Cunit_Node));
            Cunit_Ent  := Cunit_Entity (Current_Sem_Unit);

            if K = N_Package_Declaration
              or else K = N_Generic_Package_Declaration
              or else K = N_Subprogram_Declaration
              or else K = N_Generic_Subprogram_Declaration
              or else (K = N_Subprogram_Body
                         and then Acts_As_Spec (Unit (Cunit_Node)))
            then
               null;
            else
               Error_Pragma (
                 "pragma% must apply to package or subprogram declaration");
            end if;

            Set_Is_Remote_Call_Interface (Cunit_Ent);
         end Remote_Call_Interface;

         ------------------
         -- Remote_Types --
         ------------------

         --  pragma Remote_Types [(library_unit_NAME)];

         when Pragma_Remote_Types => Remote_Types : declare
            Cunit_Node : Node_Id;
            Cunit_Ent  : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Cunit_Node := Cunit (Current_Sem_Unit);
            Cunit_Ent  := Cunit_Entity (Current_Sem_Unit);

            if not Nkind_In (Unit (Cunit_Node), N_Package_Declaration,
                                                N_Generic_Package_Declaration)
            then
               Error_Pragma
                 ("pragma% can only apply to a package declaration");
            end if;

            Set_Is_Remote_Types (Cunit_Ent);
         end Remote_Types;

         ---------------
         -- Ravenscar --
         ---------------

         --  pragma Ravenscar;

         when Pragma_Ravenscar =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Set_Ravenscar_Profile (N);

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N ("pragma Ravenscar is an obsolescent feature?", N);
               Error_Msg_N ("|use pragma Profile (Ravenscar) instead", N);
            end if;

         -------------------------
         -- Restricted_Run_Time --
         -------------------------

         --  pragma Restricted_Run_Time;

         when Pragma_Restricted_Run_Time =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Set_Profile_Restrictions
              (Restricted, N, Warn => Treat_Restrictions_As_Warnings);

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("pragma Restricted_Run_Time is an obsolescent feature?", N);
               Error_Msg_N ("|use pragma Profile (Restricted) instead", N);
            end if;

         ------------------
         -- Restrictions --
         ------------------

         --  pragma Restrictions (RESTRICTION {, RESTRICTION});

         --  RESTRICTION ::=
         --    restriction_IDENTIFIER
         --  | restriction_parameter_IDENTIFIER => EXPRESSION

         when Pragma_Restrictions =>
            Process_Restrictions_Or_Restriction_Warnings
              (Warn => Treat_Restrictions_As_Warnings);

         --------------------------
         -- Restriction_Warnings --
         --------------------------

         --  pragma Restriction_Warnings (RESTRICTION {, RESTRICTION});

         --  RESTRICTION ::=
         --    restriction_IDENTIFIER
         --  | restriction_parameter_IDENTIFIER => EXPRESSION

         when Pragma_Restriction_Warnings =>
            GNAT_Pragma;
            Process_Restrictions_Or_Restriction_Warnings (Warn => True);

         ----------------
         -- Reviewable --
         ----------------

         --  pragma Reviewable;

         when Pragma_Reviewable =>
            Check_Ada_83_Warning;
            Check_Arg_Count (0);

            --  Call dummy debugging function rv. This is done to assist front
            --  end debugging. By placing a Reviewable pragma in the source
            --  program, a breakpoint on rv catches this place in the source,
            --  allowing convenient stepping to the point of interest.

            rv;

         --------------------------
         -- Short_Circuit_And_Or --
         --------------------------

         when Pragma_Short_Circuit_And_Or =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Short_Circuit_And_Or := True;

         -------------------
         -- Share_Generic --
         -------------------

         --  pragma Share_Generic (NAME {, NAME});

         when Pragma_Share_Generic =>
            GNAT_Pragma;
            Process_Generic_List;

         ------------
         -- Shared --
         ------------

         --  pragma Shared (LOCAL_NAME);

         when Pragma_Shared =>
            GNAT_Pragma;
            Process_Atomic_Shared_Volatile;

         --------------------
         -- Shared_Passive --
         --------------------

         --  pragma Shared_Passive [(library_unit_NAME)];

         --  Set the flag Is_Shared_Passive of program unit name entity

         when Pragma_Shared_Passive => Shared_Passive : declare
            Cunit_Node : Node_Id;
            Cunit_Ent  : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;

            if Nkind (N) = N_Null_Statement then
               return;
            end if;

            Cunit_Node := Cunit (Current_Sem_Unit);
            Cunit_Ent  := Cunit_Entity (Current_Sem_Unit);

            if not Nkind_In (Unit (Cunit_Node), N_Package_Declaration,
                                                N_Generic_Package_Declaration)
            then
               Error_Pragma
                 ("pragma% can only apply to a package declaration");
            end if;

            Set_Is_Shared_Passive (Cunit_Ent);
         end Shared_Passive;

         -----------------------
         -- Short_Descriptors --
         -----------------------

         --  pragma Short_Descriptors;

         when Pragma_Short_Descriptors =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Short_Descriptors := True;

         -------------------------
         -- Simple_Storage_Pool --
         -------------------------

         --  pragma Simple_Storage_Pool (type_LOCAL_NAME);

         when Pragma_Simple_Storage_Pool => Simple_Storage_Pool : declare
               Type_Id : Node_Id;
               Typ     : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);

            Type_Id := Get_Pragma_Arg (Arg1);
            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type then
               return;
            end if;

            --  We require the pragma to apply to a type declared in a package
            --  declaration, but not (immediately) within a package body.

            if Ekind (Current_Scope) /= E_Package
              or else In_Package_Body (Current_Scope)
            then
               Error_Pragma
                 ("pragma% can only apply to type declared immediately " &
                  "within a package declaration");
            end if;

            --  A simple storage pool type must be an immutably limited record
            --  or private type. If the pragma is given for a private type,
            --  the full type is similarly restricted (which is checked later
            --  in Freeze_Entity).

            if Is_Record_Type (Typ)
              and then not Is_Immutably_Limited_Type (Typ)
            then
               Error_Pragma
                 ("pragma% can only apply to explicitly limited record type");

            elsif Is_Private_Type (Typ) and then not Is_Limited_Type (Typ) then
               Error_Pragma
                 ("pragma% can only apply to a private type that is limited");

            elsif not Is_Record_Type (Typ)
              and then not Is_Private_Type (Typ)
            then
               Error_Pragma
                 ("pragma% can only apply to limited record or private type");
            end if;

            Record_Rep_Item (Typ, N);
         end Simple_Storage_Pool;

         ----------------------
         -- Source_File_Name --
         ----------------------

         --  There are five forms for this pragma:

         --  pragma Source_File_Name (
         --    [UNIT_NAME      =>] unit_NAME,
         --     BODY_FILE_NAME =>  STRING_LITERAL
         --    [, [INDEX =>] INTEGER_LITERAL]);

         --  pragma Source_File_Name (
         --    [UNIT_NAME      =>] unit_NAME,
         --     SPEC_FILE_NAME =>  STRING_LITERAL
         --    [, [INDEX =>] INTEGER_LITERAL]);

         --  pragma Source_File_Name (
         --     BODY_FILE_NAME  => STRING_LITERAL
         --  [, DOT_REPLACEMENT => STRING_LITERAL]
         --  [, CASING          => CASING_SPEC]);

         --  pragma Source_File_Name (
         --     SPEC_FILE_NAME  => STRING_LITERAL
         --  [, DOT_REPLACEMENT => STRING_LITERAL]
         --  [, CASING          => CASING_SPEC]);

         --  pragma Source_File_Name (
         --     SUBUNIT_FILE_NAME  => STRING_LITERAL
         --  [, DOT_REPLACEMENT    => STRING_LITERAL]
         --  [, CASING             => CASING_SPEC]);

         --  CASING_SPEC ::= Uppercase | Lowercase | Mixedcase

         --  Pragma Source_File_Name_Project (SFNP) is equivalent to pragma
         --  Source_File_Name (SFN), however their usage is exclusive: SFN can
         --  only be used when no project file is used, while SFNP can only be
         --  used when a project file is used.

         --  No processing here. Processing was completed during parsing, since
         --  we need to have file names set as early as possible. Units are
         --  loaded well before semantic processing starts.

         --  The only processing we defer to this point is the check for
         --  correct placement.

         when Pragma_Source_File_Name =>
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;

         ------------------------------
         -- Source_File_Name_Project --
         ------------------------------

         --  See Source_File_Name for syntax

         --  No processing here. Processing was completed during parsing, since
         --  we need to have file names set as early as possible. Units are
         --  loaded well before semantic processing starts.

         --  The only processing we defer to this point is the check for
         --  correct placement.

         when Pragma_Source_File_Name_Project =>
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;

            --  Check that a pragma Source_File_Name_Project is used only in a
            --  configuration pragmas file.

            --  Pragmas Source_File_Name_Project should only be generated by
            --  the Project Manager in configuration pragmas files.

            --  This is really an ugly test. It seems to depend on some
            --  accidental and undocumented property. At the very least it
            --  needs to be documented, but it would be better to have a
            --  clean way of testing if we are in a configuration file???

            if Present (Parent (N)) then
               Error_Pragma
                 ("pragma% can only appear in a configuration pragmas file");
            end if;

         ----------------------
         -- Source_Reference --
         ----------------------

         --  pragma Source_Reference (INTEGER_LITERAL [, STRING_LITERAL]);

         --  Nothing to do, all processing completed in Par.Prag, since we need
         --  the information for possible parser messages that are output.

         when Pragma_Source_Reference =>
            GNAT_Pragma;

         --------------------------------
         -- Static_Elaboration_Desired --
         --------------------------------

         --  pragma Static_Elaboration_Desired (DIRECT_NAME);

         when Pragma_Static_Elaboration_Desired =>
            GNAT_Pragma;
            Check_At_Most_N_Arguments (1);

            if Is_Compilation_Unit (Current_Scope)
              and then Ekind (Current_Scope) = E_Package
            then
               Set_Static_Elaboration_Desired (Current_Scope, True);
            else
               Error_Pragma ("pragma% must apply to a library-level package");
            end if;

         ------------------
         -- Storage_Size --
         ------------------

         --  pragma Storage_Size (EXPRESSION);

         when Pragma_Storage_Size => Storage_Size : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);

            --  The expression must be analyzed in the special manner described
            --  in "Handling of Default Expressions" in sem.ads.

            Arg := Get_Pragma_Arg (Arg1);
            Preanalyze_Spec_Expression (Arg, Any_Integer);

            if not Is_Static_Expression (Arg) then
               Check_Restriction (Static_Storage_Size, Arg);
            end if;

            if Nkind (P) /= N_Task_Definition then
               Pragma_Misplaced;
               return;

            else
               if Has_Storage_Size_Pragma (P) then
                  Error_Pragma ("duplicate pragma% not allowed");
               else
                  Set_Has_Storage_Size_Pragma (P, True);
               end if;

               Record_Rep_Item (Defining_Identifier (Parent (P)), N);
               --  ???  exp_ch9 should use this!
            end if;
         end Storage_Size;

         ------------------
         -- Storage_Unit --
         ------------------

         --  pragma Storage_Unit (NUMERIC_LITERAL);

         --  Only permitted argument is System'Storage_Unit value

         when Pragma_Storage_Unit =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Integer_Literal (Arg1);

            if Intval (Get_Pragma_Arg (Arg1)) /=
              UI_From_Int (Ttypes.System_Storage_Unit)
            then
               Error_Msg_Uint_1 := UI_From_Int (Ttypes.System_Storage_Unit);
               Error_Pragma_Arg
                 ("the only allowed argument for pragma% is ^", Arg1);
            end if;

         --------------------
         -- Stream_Convert --
         --------------------

         --  pragma Stream_Convert (
         --    [Entity =>] type_LOCAL_NAME,
         --    [Read   =>] function_NAME,
         --    [Write  =>] function NAME);

         when Pragma_Stream_Convert => Stream_Convert : declare

            procedure Check_OK_Stream_Convert_Function (Arg : Node_Id);
            --  Check that the given argument is the name of a local function
            --  of one argument that is not overloaded earlier in the current
            --  local scope. A check is also made that the argument is a
            --  function with one parameter.

            --------------------------------------
            -- Check_OK_Stream_Convert_Function --
            --------------------------------------

            procedure Check_OK_Stream_Convert_Function (Arg : Node_Id) is
               Ent : Entity_Id;

            begin
               Check_Arg_Is_Local_Name (Arg);
               Ent := Entity (Get_Pragma_Arg (Arg));

               if Has_Homonym (Ent) then
                  Error_Pragma_Arg
                    ("argument for pragma% may not be overloaded", Arg);
               end if;

               if Ekind (Ent) /= E_Function
                 or else No (First_Formal (Ent))
                 or else Present (Next_Formal (First_Formal (Ent)))
               then
                  Error_Pragma_Arg
                    ("argument for pragma% must be" &
                     " function of one argument", Arg);
               end if;
            end Check_OK_Stream_Convert_Function;

         --  Start of processing for Stream_Convert

         begin
            GNAT_Pragma;
            Check_Arg_Order ((Name_Entity, Name_Read, Name_Write));
            Check_Arg_Count (3);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Read);
            Check_Optional_Identifier (Arg3, Name_Write);
            Check_Arg_Is_Local_Name (Arg1);
            Check_OK_Stream_Convert_Function (Arg2);
            Check_OK_Stream_Convert_Function (Arg3);

            declare
               Typ   : constant Entity_Id :=
                         Underlying_Type (Entity (Get_Pragma_Arg (Arg1)));
               Read  : constant Entity_Id := Entity (Get_Pragma_Arg (Arg2));
               Write : constant Entity_Id := Entity (Get_Pragma_Arg (Arg3));

            begin
               Check_First_Subtype (Arg1);

               --  Check for too early or too late. Note that we don't enforce
               --  the rule about primitive operations in this case, since, as
               --  is the case for explicit stream attributes themselves, these
               --  restrictions are not appropriate. Note that the chaining of
               --  the pragma by Rep_Item_Too_Late is actually the critical
               --  processing done for this pragma.

               if Rep_Item_Too_Early (Typ, N)
                    or else
                  Rep_Item_Too_Late (Typ, N, FOnly => True)
               then
                  return;
               end if;

               --  Return if previous error

               if Etype (Typ) = Any_Type
                    or else
                  Etype (Read) = Any_Type
                    or else
                  Etype (Write) = Any_Type
               then
                  return;
               end if;

               --  Error checks

               if Underlying_Type (Etype (Read)) /= Typ then
                  Error_Pragma_Arg
                    ("incorrect return type for function&", Arg2);
               end if;

               if Underlying_Type (Etype (First_Formal (Write))) /= Typ then
                  Error_Pragma_Arg
                    ("incorrect parameter type for function&", Arg3);
               end if;

               if Underlying_Type (Etype (First_Formal (Read))) /=
                  Underlying_Type (Etype (Write))
               then
                  Error_Pragma_Arg
                    ("result type of & does not match Read parameter type",
                     Arg3);
               end if;
            end;
         end Stream_Convert;

         -------------------------
         -- Style_Checks (GNAT) --
         -------------------------

         --  pragma Style_Checks (On | Off | ALL_CHECKS | STRING_LITERAL);

         --  This is processed by the parser since some of the style checks
         --  take place during source scanning and parsing. This means that
         --  we don't need to issue error messages here.

         when Pragma_Style_Checks => Style_Checks : declare
            A  : constant Node_Id   := Get_Pragma_Arg (Arg1);
            S  : String_Id;
            C  : Char_Code;

         begin
            GNAT_Pragma;
            Check_No_Identifiers;

            --  Two argument form

            if Arg_Count = 2 then
               Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);

               declare
                  E_Id : Node_Id;
                  E    : Entity_Id;

               begin
                  E_Id := Get_Pragma_Arg (Arg2);
                  Analyze (E_Id);

                  if not Is_Entity_Name (E_Id) then
                     Error_Pragma_Arg
                       ("second argument of pragma% must be entity name",
                        Arg2);
                  end if;

                  E := Entity (E_Id);

                  if E = Any_Id then
                     return;
                  else
                     loop
                        Set_Suppress_Style_Checks (E,
                          (Chars (Get_Pragma_Arg (Arg1)) = Name_Off));
                        exit when No (Homonym (E));
                        E := Homonym (E);
                     end loop;
                  end if;
               end;

            --  One argument form

            else
               Check_Arg_Count (1);

               if Nkind (A) = N_String_Literal then
                  S   := Strval (A);

                  declare
                     Slen    : constant Natural := Natural (String_Length (S));
                     Options : String (1 .. Slen);
                     J       : Natural;

                  begin
                     J := 1;
                     loop
                        C := Get_String_Char (S, Int (J));
                        exit when not In_Character_Range (C);
                        Options (J) := Get_Character (C);

                        --  If at end of string, set options. As per discussion
                        --  above, no need to check for errors, since we issued
                        --  them in the parser.

                        if J = Slen then
                           Set_Style_Check_Options (Options);
                           exit;
                        end if;

                        J := J + 1;
                     end loop;
                  end;

               elsif Nkind (A) = N_Identifier then
                  if Chars (A) = Name_All_Checks then
                     if GNAT_Mode then
                        Set_GNAT_Style_Check_Options;
                     else
                        Set_Default_Style_Check_Options;
                     end if;

                  elsif Chars (A) = Name_On then
                     Style_Check := True;

                  elsif Chars (A) = Name_Off then
                     Style_Check := False;
                  end if;
               end if;
            end if;
         end Style_Checks;

         --------------
         -- Subtitle --
         --------------

         --  pragma Subtitle ([Subtitle =>] STRING_LITERAL);

         when Pragma_Subtitle =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Subtitle);
            Check_Arg_Is_Static_Expression (Arg1, Standard_String);
            Store_Note (N);

         --------------
         -- Suppress --
         --------------

         --  pragma Suppress (IDENTIFIER [, [On =>] NAME]);

         when Pragma_Suppress =>
            Process_Suppress_Unsuppress (True);

         ------------------
         -- Suppress_All --
         ------------------

         --  pragma Suppress_All;

         --  The only check made here is that the pragma has no arguments.
         --  There are no placement rules, and the processing required (setting
         --  the Has_Pragma_Suppress_All flag in the compilation unit node was
         --  taken care of by the parser). Process_Compilation_Unit_Pragmas
         --  then creates and inserts a pragma Suppress (All_Checks).

         when Pragma_Suppress_All =>
            GNAT_Pragma;
            Check_Arg_Count (0);

         -------------------------
         -- Suppress_Debug_Info --
         -------------------------

         --  pragma Suppress_Debug_Info ([Entity =>] LOCAL_NAME);

         when Pragma_Suppress_Debug_Info =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            Set_Debug_Info_Off (Entity (Get_Pragma_Arg (Arg1)));

         ----------------------------------
         -- Suppress_Exception_Locations --
         ----------------------------------

         --  pragma Suppress_Exception_Locations;

         when Pragma_Suppress_Exception_Locations =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Exception_Locations_Suppressed := True;

         -----------------------------
         -- Suppress_Initialization --
         -----------------------------

         --  pragma Suppress_Initialization ([Entity =>] type_Name);

         when Pragma_Suppress_Initialization => Suppress_Init : declare
            E_Id : Node_Id;
            E    : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            E_Id := Get_Pragma_Arg (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            if not Is_Type (E) then
               Error_Pragma_Arg ("pragma% requires type or subtype", Arg1);
            end if;

            if Rep_Item_Too_Early (E, N)
                 or else
               Rep_Item_Too_Late (E, N, FOnly => True)
            then
               return;
            end if;

            --  For incomplete/private type, set flag on full view

            if Is_Incomplete_Or_Private_Type (E) then
               if No (Full_View (Base_Type (E))) then
                  Error_Pragma_Arg
                    ("argument of pragma% cannot be an incomplete type", Arg1);
               else
                  Set_Suppress_Initialization (Full_View (Base_Type (E)));
               end if;

            --  For first subtype, set flag on base type

            elsif Is_First_Subtype (E) then
               Set_Suppress_Initialization (Base_Type (E));

            --  For other than first subtype, set flag on subtype itself

            else
               Set_Suppress_Initialization (E);
            end if;
         end Suppress_Init;

         -----------------
         -- System_Name --
         -----------------

         --  pragma System_Name (DIRECT_NAME);

         --  Syntax check: one argument, which must be the identifier GNAT or
         --  the identifier GCC, no other identifiers are acceptable.

         when Pragma_System_Name =>
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Gcc, Name_Gnat);

         -----------------------------
         -- Task_Dispatching_Policy --
         -----------------------------

         --  pragma Task_Dispatching_Policy (policy_IDENTIFIER);

         when Pragma_Task_Dispatching_Policy => declare
            DP : Character;

         begin
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Task_Dispatching_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            Get_Name_String (Chars (Get_Pragma_Arg (Arg1)));
            DP := Fold_Upper (Name_Buffer (1));

            if Task_Dispatching_Policy /= ' '
              and then Task_Dispatching_Policy /= DP
            then
               Error_Msg_Sloc := Task_Dispatching_Policy_Sloc;
               Error_Pragma
                 ("task dispatching policy incompatible with policy#");

            --  Set new policy, but always preserve System_Location since we
            --  like the error message with the run time name.

            else
               Task_Dispatching_Policy := DP;

               if Task_Dispatching_Policy_Sloc /= System_Location then
                  Task_Dispatching_Policy_Sloc := Loc;
               end if;
            end if;
         end;

         ---------------
         -- Task_Info --
         ---------------

         --  pragma Task_Info (EXPRESSION);

         when Pragma_Task_Info => Task_Info : declare
            P : constant Node_Id := Parent (N);

         begin
            GNAT_Pragma;

            if Nkind (P) /= N_Task_Definition then
               Error_Pragma ("pragma% must appear in task definition");
            end if;

            Check_No_Identifiers;
            Check_Arg_Count (1);

            Analyze_And_Resolve
              (Get_Pragma_Arg (Arg1), RTE (RE_Task_Info_Type));

            if Etype (Get_Pragma_Arg (Arg1)) = Any_Type then
               return;
            end if;

            if Has_Task_Info_Pragma (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Task_Info_Pragma (P, True);
            end if;
         end Task_Info;

         ---------------
         -- Task_Name --
         ---------------

         --  pragma Task_Name (string_EXPRESSION);

         when Pragma_Task_Name => Task_Name : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);

            Arg := Get_Pragma_Arg (Arg1);

            --  The expression is used in the call to Create_Task, and must be
            --  expanded there, not in the context of the current spec. It must
            --  however be analyzed to capture global references, in case it
            --  appears in a generic context.

            Preanalyze_And_Resolve (Arg, Standard_String);

            if Nkind (P) /= N_Task_Definition then
               Pragma_Misplaced;
            end if;

            if Has_Task_Name_Pragma (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Task_Name_Pragma (P, True);
               Record_Rep_Item (Defining_Identifier (Parent (P)), N);
            end if;
         end Task_Name;

         ------------------
         -- Task_Storage --
         ------------------

         --  pragma Task_Storage (
         --     [Task_Type =>] LOCAL_NAME,
         --     [Top_Guard =>] static_integer_EXPRESSION);

         when Pragma_Task_Storage => Task_Storage : declare
            Args  : Args_List (1 .. 2);
            Names : constant Name_List (1 .. 2) := (
                      Name_Task_Type,
                      Name_Top_Guard);

            Task_Type : Node_Id renames Args (1);
            Top_Guard : Node_Id renames Args (2);

            Ent : Entity_Id;

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);

            if No (Task_Type) then
               Error_Pragma
                 ("missing task_type argument for pragma%");
            end if;

            Check_Arg_Is_Local_Name (Task_Type);

            Ent := Entity (Task_Type);

            if not Is_Task_Type (Ent) then
               Error_Pragma_Arg
                 ("argument for pragma% must be task type", Task_Type);
            end if;

            if No (Top_Guard) then
               Error_Pragma_Arg
                 ("pragma% takes two arguments", Task_Type);
            else
               Check_Arg_Is_Static_Expression (Top_Guard, Any_Integer);
            end if;

            Check_First_Subtype (Task_Type);

            if Rep_Item_Too_Late (Ent, N) then
               raise Pragma_Exit;
            end if;
         end Task_Storage;

         ---------------
         -- Test_Case --
         ---------------

         --  pragma Test_Case ([Name     =>] Static_String_EXPRESSION
         --                   ,[Mode     =>] MODE_TYPE
         --                  [, Requires =>  Boolean_EXPRESSION]
         --                  [, Ensures  =>  Boolean_EXPRESSION]);

         --  MODE_TYPE ::= Nominal | Robustness

         when Pragma_Test_Case => Test_Case : declare
         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments (4);
            Check_Arg_Order
                 ((Name_Name, Name_Mode, Name_Requires, Name_Ensures));

            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Arg_Is_Static_Expression (Arg1, Standard_String);

            --  In ASIS mode, for a pragma generated from a source aspect, also
            --  analyze the original aspect expression.

            if ASIS_Mode
              and then Present (Corresponding_Aspect (N))
            then
               Check_Expr_Is_Static_Expression
                 (Original_Node (Get_Pragma_Arg (Arg1)), Standard_String);
            end if;

            Check_Optional_Identifier (Arg2, Name_Mode);
            Check_Arg_Is_One_Of (Arg2, Name_Nominal, Name_Robustness);

            if Arg_Count = 4 then
               Check_Identifier (Arg3, Name_Requires);
               Check_Identifier (Arg4, Name_Ensures);

            elsif Arg_Count = 3 then
               Check_Identifier_Is_One_Of (Arg3, Name_Requires, Name_Ensures);
            end if;

            Check_Test_Case;
         end Test_Case;

         --------------------------
         -- Thread_Local_Storage --
         --------------------------

         --  pragma Thread_Local_Storage ([Entity =>] LOCAL_NAME);

         when Pragma_Thread_Local_Storage => Thread_Local_Storage : declare
            Id : Node_Id;
            E  : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);

            Id := Get_Pragma_Arg (Arg1);
            Analyze (Id);

            if not Is_Entity_Name (Id)
              or else Ekind (Entity (Id)) /= E_Variable
            then
               Error_Pragma_Arg ("local variable name required", Arg1);
            end if;

            E := Entity (Id);

            if Rep_Item_Too_Early (E, N)
              or else Rep_Item_Too_Late (E, N)
            then
               raise Pragma_Exit;
            end if;

            Set_Has_Pragma_Thread_Local_Storage (E);
            Set_Has_Gigi_Rep_Item (E);
         end Thread_Local_Storage;

         ----------------
         -- Time_Slice --
         ----------------

         --  pragma Time_Slice (static_duration_EXPRESSION);

         when Pragma_Time_Slice => Time_Slice : declare
            Val : Ureal;
            Nod : Node_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_In_Main_Program;
            Check_Arg_Is_Static_Expression (Arg1, Standard_Duration);

            if not Error_Posted (Arg1) then
               Nod := Next (N);
               while Present (Nod) loop
                  if Nkind (Nod) = N_Pragma
                    and then Pragma_Name (Nod) = Name_Time_Slice
                  then
                     Error_Msg_Name_1 := Pname;
                     Error_Msg_N ("duplicate pragma% not permitted", Nod);
                  end if;

                  Next (Nod);
               end loop;
            end if;

            --  Process only if in main unit

            if Get_Source_Unit (Loc) = Main_Unit then
               Opt.Time_Slice_Set := True;
               Val := Expr_Value_R (Get_Pragma_Arg (Arg1));

               if Val <= Ureal_0 then
                  Opt.Time_Slice_Value := 0;

               elsif Val > UR_From_Uint (UI_From_Int (1000)) then
                  Opt.Time_Slice_Value := 1_000_000_000;

               else
                  Opt.Time_Slice_Value :=
                    UI_To_Int (UR_To_Uint (Val * UI_From_Int (1_000_000)));
               end if;
            end if;
         end Time_Slice;

         -----------
         -- Title --
         -----------

         --  pragma Title (TITLING_OPTION [, TITLING OPTION]);

         --   TITLING_OPTION ::=
         --     [Title =>] STRING_LITERAL
         --   | [Subtitle =>] STRING_LITERAL

         when Pragma_Title => Title : declare
            Args  : Args_List (1 .. 2);
            Names : constant Name_List (1 .. 2) := (
                      Name_Title,
                      Name_Subtitle);

         begin
            GNAT_Pragma;
            Gather_Associations (Names, Args);
            Store_Note (N);

            for J in 1 .. 2 loop
               if Present (Args (J)) then
                  Check_Arg_Is_Static_Expression (Args (J), Standard_String);
               end if;
            end loop;
         end Title;

         ---------------------
         -- Unchecked_Union --
         ---------------------

         --  pragma Unchecked_Union (first_subtype_LOCAL_NAME)

         when Pragma_Unchecked_Union => Unchecked_Union : declare
            Assoc   : constant Node_Id := Arg1;
            Type_Id : constant Node_Id := Get_Pragma_Arg (Assoc);
            Typ     : Entity_Id;
            Discr   : Entity_Id;
            Tdef    : Node_Id;
            Clist   : Node_Id;
            Vpart   : Node_Id;
            Comp    : Node_Id;
            Variant : Node_Id;

         begin
            Ada_2005_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type
              or else Rep_Item_Too_Early (Typ, N)
            then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if Rep_Item_Too_Late (Typ, N) then
               return;
            end if;

            Check_First_Subtype (Arg1);

            --  Note remaining cases are references to a type in the current
            --  declarative part. If we find an error, we post the error on
            --  the relevant type declaration at an appropriate point.

            if not Is_Record_Type (Typ) then
               Error_Msg_N ("Unchecked_Union must be record type", Typ);
               return;

            elsif Is_Tagged_Type (Typ) then
               Error_Msg_N ("Unchecked_Union must not be tagged", Typ);
               return;

            elsif not Has_Discriminants (Typ) then
               Error_Msg_N
                ("Unchecked_Union must have one discriminant", Typ);
               return;

            --  Note: in previous versions of GNAT we used to check for limited
            --  types and give an error, but in fact the standard does allow
            --  Unchecked_Union on limited types, so this check was removed.

            --  Proceed with basic error checks completed

            else
               Discr := First_Discriminant (Typ);
               while Present (Discr) loop
                  if No (Discriminant_Default_Value (Discr)) then
                     Error_Msg_N
                       ("Unchecked_Union discriminant must have default value",
                        Discr);
                  end if;

                  Next_Discriminant (Discr);
               end loop;

               Tdef  := Type_Definition (Declaration_Node (Typ));
               Clist := Component_List (Tdef);

               Comp := First (Component_Items (Clist));
               while Present (Comp) loop
                  Check_Component (Comp, Typ);
                  Next (Comp);
               end loop;

               if No (Clist) or else No (Variant_Part (Clist)) then
                  Error_Msg_N
                    ("Unchecked_Union must have variant part",
                     Tdef);
                  return;
               end if;

               Vpart := Variant_Part (Clist);

               Variant := First (Variants (Vpart));
               while Present (Variant) loop
                  Check_Variant (Variant, Typ);
                  Next (Variant);
               end loop;
            end if;

            Set_Is_Unchecked_Union  (Typ);
            Set_Convention (Typ, Convention_C);
            Set_Has_Unchecked_Union (Base_Type (Typ));
            Set_Is_Unchecked_Union  (Base_Type (Typ));
         end Unchecked_Union;

         ------------------------
         -- Unimplemented_Unit --
         ------------------------

         --  pragma Unimplemented_Unit;

         --  Note: this only gives an error if we are generating code, or if
         --  we are in a generic library unit (where the pragma appears in the
         --  body, not in the spec).

         when Pragma_Unimplemented_Unit => Unimplemented_Unit : declare
            Cunitent : constant Entity_Id :=
                         Cunit_Entity (Get_Source_Unit (Loc));
            Ent_Kind : constant Entity_Kind :=
                         Ekind (Cunitent);

         begin
            GNAT_Pragma;
            Check_Arg_Count (0);

            if Operating_Mode = Generate_Code
              or else Ent_Kind = E_Generic_Function
              or else Ent_Kind = E_Generic_Procedure
              or else Ent_Kind = E_Generic_Package
            then
               Get_Name_String (Chars (Cunitent));
               Set_Casing (Mixed_Case);
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Str (" is not supported in this configuration");
               Write_Eol;
               raise Unrecoverable_Error;
            end if;
         end Unimplemented_Unit;

         ------------------------
         -- Universal_Aliasing --
         ------------------------

         --  pragma Universal_Aliasing [([Entity =>] type_LOCAL_NAME)];

         when Pragma_Universal_Aliasing => Universal_Alias : declare
            E_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg2, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Entity (Get_Pragma_Arg (Arg1));

            if E_Id = Any_Type then
               return;
            elsif No (E_Id) or else not Is_Type (E_Id) then
               Error_Pragma_Arg ("pragma% requires type", Arg1);
            end if;

            Set_Universal_Aliasing (Implementation_Base_Type (E_Id));
         end Universal_Alias;

         --------------------
         -- Universal_Data --
         --------------------

         --  pragma Universal_Data [(library_unit_NAME)];

         when Pragma_Universal_Data =>
            GNAT_Pragma;

            --  If this is a configuration pragma, then set the universal
            --  addressing option, otherwise confirm that the pragma satisfies
            --  the requirements of library unit pragma placement and leave it
            --  to the GNAAMP back end to detect the pragma (avoids transitive
            --  setting of the option due to withed units).

            if Is_Configuration_Pragma then
               Universal_Addressing_On_AAMP := True;
            else
               Check_Valid_Library_Unit_Pragma;
            end if;

            if not AAMP_On_Target then
               Error_Pragma ("?pragma% ignored (applies only to AAMP)");
            end if;

         ----------------
         -- Unmodified --
         ----------------

         --  pragma Unmodified (local_Name {, local_Name});

         when Pragma_Unmodified => Unmodified : declare
            Arg_Node : Node_Id;
            Arg_Expr : Node_Id;
            Arg_Ent  : Entity_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);

            --  Loop through arguments

            Arg_Node := Arg1;
            while Present (Arg_Node) loop
               Check_No_Identifier (Arg_Node);

               --  Note: the analyze call done by Check_Arg_Is_Local_Name will
               --  in fact generate reference, so that the entity will have a
               --  reference, which will inhibit any warnings about it not
               --  being referenced, and also properly show up in the ali file
               --  as a reference. But this reference is recorded before the
               --  Has_Pragma_Unreferenced flag is set, so that no warning is
               --  generated for this reference.

               Check_Arg_Is_Local_Name (Arg_Node);
               Arg_Expr := Get_Pragma_Arg (Arg_Node);

               if Is_Entity_Name (Arg_Expr) then
                  Arg_Ent := Entity (Arg_Expr);

                  if not Is_Assignable (Arg_Ent) then
                     Error_Pragma_Arg
                       ("pragma% can only be applied to a variable",
                        Arg_Expr);
                  else
                     Set_Has_Pragma_Unmodified (Arg_Ent);
                  end if;
               end if;

               Next (Arg_Node);
            end loop;
         end Unmodified;

         ------------------
         -- Unreferenced --
         ------------------

         --  pragma Unreferenced (local_Name {, local_Name});

         --    or when used in a context clause:

         --  pragma Unreferenced (library_unit_NAME {, library_unit_NAME}

         when Pragma_Unreferenced => Unreferenced : declare
            Arg_Node : Node_Id;
            Arg_Expr : Node_Id;
            Arg_Ent  : Entity_Id;
            Citem    : Node_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);

            --  Check case of appearing within context clause

            if Is_In_Context_Clause then

               --  The arguments must all be units mentioned in a with clause
               --  in the same context clause. Note we already checked (in
               --  Par.Prag) that the arguments are either identifiers or
               --  selected components.

               Arg_Node := Arg1;
               while Present (Arg_Node) loop
                  Citem := First (List_Containing (N));
                  while Citem /= N loop
                     if Nkind (Citem) = N_With_Clause
                       and then
                         Same_Name (Name (Citem), Get_Pragma_Arg (Arg_Node))
                     then
                        Set_Has_Pragma_Unreferenced
                          (Cunit_Entity
                             (Get_Source_Unit
                                (Library_Unit (Citem))));
                        Set_Unit_Name
                          (Get_Pragma_Arg (Arg_Node), Name (Citem));
                        exit;
                     end if;

                     Next (Citem);
                  end loop;

                  if Citem = N then
                     Error_Pragma_Arg
                       ("argument of pragma% is not withed unit", Arg_Node);
                  end if;

                  Next (Arg_Node);
               end loop;

            --  Case of not in list of context items

            else
               Arg_Node := Arg1;
               while Present (Arg_Node) loop
                  Check_No_Identifier (Arg_Node);

                  --  Note: the analyze call done by Check_Arg_Is_Local_Name
                  --  will in fact generate reference, so that the entity will
                  --  have a reference, which will inhibit any warnings about
                  --  it not being referenced, and also properly show up in the
                  --  ali file as a reference. But this reference is recorded
                  --  before the Has_Pragma_Unreferenced flag is set, so that
                  --  no warning is generated for this reference.

                  Check_Arg_Is_Local_Name (Arg_Node);
                  Arg_Expr := Get_Pragma_Arg (Arg_Node);

                  if Is_Entity_Name (Arg_Expr) then
                     Arg_Ent := Entity (Arg_Expr);

                     --  If the entity is overloaded, the pragma applies to the
                     --  most recent overloading, as documented. In this case,
                     --  name resolution does not generate a reference, so it
                     --  must be done here explicitly.

                     if Is_Overloaded (Arg_Expr) then
                        Generate_Reference (Arg_Ent, N);
                     end if;

                     Set_Has_Pragma_Unreferenced (Arg_Ent);
                  end if;

                  Next (Arg_Node);
               end loop;
            end if;
         end Unreferenced;

         --------------------------
         -- Unreferenced_Objects --
         --------------------------

         --  pragma Unreferenced_Objects (local_Name {, local_Name});

         when Pragma_Unreferenced_Objects => Unreferenced_Objects : declare
            Arg_Node : Node_Id;
            Arg_Expr : Node_Id;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);

            Arg_Node := Arg1;
            while Present (Arg_Node) loop
               Check_No_Identifier (Arg_Node);
               Check_Arg_Is_Local_Name (Arg_Node);
               Arg_Expr := Get_Pragma_Arg (Arg_Node);

               if not Is_Entity_Name (Arg_Expr)
                 or else not Is_Type (Entity (Arg_Expr))
               then
                  Error_Pragma_Arg
                    ("argument for pragma% must be type or subtype", Arg_Node);
               end if;

               Set_Has_Pragma_Unreferenced_Objects (Entity (Arg_Expr));
               Next (Arg_Node);
            end loop;
         end Unreferenced_Objects;

         ------------------------------
         -- Unreserve_All_Interrupts --
         ------------------------------

         --  pragma Unreserve_All_Interrupts;

         when Pragma_Unreserve_All_Interrupts =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            if In_Extended_Main_Code_Unit (Main_Unit_Entity) then
               Unreserve_All_Interrupts := True;
            end if;

         ----------------
         -- Unsuppress --
         ----------------

         --  pragma Unsuppress (IDENTIFIER [, [On =>] NAME]);

         when Pragma_Unsuppress =>
            Ada_2005_Pragma;
            Process_Suppress_Unsuppress (False);

         -------------------
         -- Use_VADS_Size --
         -------------------

         --  pragma Use_VADS_Size;

         when Pragma_Use_VADS_Size =>
            GNAT_Pragma;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Use_VADS_Size := True;

         ---------------------
         -- Validity_Checks --
         ---------------------

         --  pragma Validity_Checks (On | Off | ALL_CHECKS | STRING_LITERAL);

         when Pragma_Validity_Checks => Validity_Checks : declare
            A  : constant Node_Id   := Get_Pragma_Arg (Arg1);
            S  : String_Id;
            C  : Char_Code;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;

            if Nkind (A) = N_String_Literal then
               S   := Strval (A);

               declare
                  Slen    : constant Natural := Natural (String_Length (S));
                  Options : String (1 .. Slen);
                  J       : Natural;

               begin
                  J := 1;
                  loop
                     C := Get_String_Char (S, Int (J));
                     exit when not In_Character_Range (C);
                     Options (J) := Get_Character (C);

                     if J = Slen then
                        Set_Validity_Check_Options (Options);
                        exit;
                     else
                        J := J + 1;
                     end if;
                  end loop;
               end;

            elsif Nkind (A) = N_Identifier then
               if Chars (A) = Name_All_Checks then
                  Set_Validity_Check_Options ("a");
               elsif Chars (A) = Name_On then
                  Validity_Checks_On := True;
               elsif Chars (A) = Name_Off then
                  Validity_Checks_On := False;
               end if;
            end if;
         end Validity_Checks;

         --------------
         -- Volatile --
         --------------

         --  pragma Volatile (LOCAL_NAME);

         when Pragma_Volatile =>
            Process_Atomic_Shared_Volatile;

         -------------------------
         -- Volatile_Components --
         -------------------------

         --  pragma Volatile_Components (array_LOCAL_NAME);

         --  Volatile is handled by the same circuit as Atomic_Components

         --------------
         -- Warnings --
         --------------

         --  pragma Warnings (On | Off);
         --  pragma Warnings (On | Off, LOCAL_NAME);
         --  pragma Warnings (static_string_EXPRESSION);
         --  pragma Warnings (On | Off, STRING_LITERAL);

         when Pragma_Warnings => Warnings : begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_No_Identifiers;

            --  If debug flag -gnatd.i is set, pragma is ignored

            if Debug_Flag_Dot_I then
               return;
            end if;

            --  Process various forms of the pragma

            declare
               Argx : constant Node_Id := Get_Pragma_Arg (Arg1);

            begin
               --  One argument case

               if Arg_Count = 1 then

                  --  On/Off one argument case was processed by parser

                  if Nkind (Argx) = N_Identifier
                    and then
                      (Chars (Argx) = Name_On
                         or else
                       Chars (Argx) = Name_Off)
                  then
                     null;

                  --  One argument case must be ON/OFF or static string expr

                  elsif not Is_Static_String_Expression (Arg1) then
                     Error_Pragma_Arg
                       ("argument of pragma% must be On/Off or " &
                        "static string expression", Arg1);

                  --  One argument string expression case

                  else
                     declare
                        Lit : constant Node_Id   := Expr_Value_S (Argx);
                        Str : constant String_Id := Strval (Lit);
                        Len : constant Nat       := String_Length (Str);
                        C   : Char_Code;
                        J   : Nat;
                        OK  : Boolean;
                        Chr : Character;

                     begin
                        J := 1;
                        while J <= Len loop
                           C := Get_String_Char (Str, J);
                           OK := In_Character_Range (C);

                           if OK then
                              Chr := Get_Character (C);

                              --  Dot case

                              if J < Len and then Chr = '.' then
                                 J := J + 1;
                                 C := Get_String_Char (Str, J);
                                 Chr := Get_Character (C);

                                 if not Set_Dot_Warning_Switch (Chr) then
                                    Error_Pragma_Arg
                                      ("invalid warning switch character " &
                                       '.' & Chr, Arg1);
                                 end if;

                              --  Non-Dot case

                              else
                                 OK := Set_Warning_Switch (Chr);
                              end if;
                           end if;

                           if not OK then
                              Error_Pragma_Arg
                                ("invalid warning switch character " & Chr,
                                 Arg1);
                           end if;

                           J := J + 1;
                        end loop;
                     end;
                  end if;

               --  Two or more arguments (must be two)

               else
                  Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);
                  Check_At_Most_N_Arguments (2);

                  declare
                     E_Id : Node_Id;
                     E    : Entity_Id;
                     Err  : Boolean;

                  begin
                     E_Id := Get_Pragma_Arg (Arg2);
                     Analyze (E_Id);

                     --  In the expansion of an inlined body, a reference to
                     --  the formal may be wrapped in a conversion if the
                     --  actual is a conversion. Retrieve the real entity name.

                     if (In_Instance_Body or In_Inlined_Body)
                       and then Nkind (E_Id) = N_Unchecked_Type_Conversion
                     then
                        E_Id := Expression (E_Id);
                     end if;

                     --  Entity name case

                     if Is_Entity_Name (E_Id) then
                        E := Entity (E_Id);

                        if E = Any_Id then
                           return;
                        else
                           loop
                              Set_Warnings_Off
                                (E, (Chars (Get_Pragma_Arg (Arg1)) =
                                                              Name_Off));

                              if Chars (Get_Pragma_Arg (Arg1)) = Name_Off
                                and then Warn_On_Warnings_Off
                              then
                                 Warnings_Off_Pragmas.Append ((N, E));
                              end if;

                              if Is_Enumeration_Type (E) then
                                 declare
                                    Lit : Entity_Id;
                                 begin
                                    Lit := First_Literal (E);
                                    while Present (Lit) loop
                                       Set_Warnings_Off (Lit);
                                       Next_Literal (Lit);
                                    end loop;
                                 end;
                              end if;

                              exit when No (Homonym (E));
                              E := Homonym (E);
                           end loop;
                        end if;

                     --  Error if not entity or static string literal case

                     elsif not Is_Static_String_Expression (Arg2) then
                        Error_Pragma_Arg
                          ("second argument of pragma% must be entity " &
                           "name or static string expression", Arg2);

                     --  String literal case

                     else
                        String_To_Name_Buffer
                          (Strval (Expr_Value_S (Get_Pragma_Arg (Arg2))));

                        --  Note on configuration pragma case: If this is a
                        --  configuration pragma, then for an OFF pragma, we
                        --  just set Config True in the call, which is all
                        --  that needs to be done. For the case of ON, this
                        --  is normally an error, unless it is canceling the
                        --  effect of a previous OFF pragma in the same file.
                        --  In any other case, an error will be signalled (ON
                        --  with no matching OFF).

                        --  Note: We set Used if we are inside a generic to
                        --  disable the test that the non-config case actually
                        --  cancels a warning. That's because we can't be sure
                        --  there isn't an instantiation in some other unit
                        --  where a warning is suppressed.

                        --  We could do a little better here by checking if the
                        --  generic unit we are inside is public, but for now
                        --  we don't bother with that refinement.

                        if Chars (Argx) = Name_Off then
                           Set_Specific_Warning_Off
                             (Loc, Name_Buffer (1 .. Name_Len),
                              Config => Is_Configuration_Pragma,
                              Used   => Inside_A_Generic or else In_Instance);

                        elsif Chars (Argx) = Name_On then
                           Set_Specific_Warning_On
                             (Loc, Name_Buffer (1 .. Name_Len), Err);

                           if Err then
                              Error_Msg
                                ("?pragma Warnings On with no " &
                                 "matching Warnings Off",
                                 Loc);
                           end if;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end Warnings;

         -------------------
         -- Weak_External --
         -------------------

         --  pragma Weak_External ([Entity =>] LOCAL_NAME);

         when Pragma_Weak_External => Weak_External : declare
            Ent : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Ent := Entity (Get_Pragma_Arg (Arg1));

            if Rep_Item_Too_Early (Ent, N) then
               return;
            else
               Ent := Underlying_Type (Ent);
            end if;

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Ent, N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Ent);
            end if;
         end Weak_External;

         -----------------------------
         -- Wide_Character_Encoding --
         -----------------------------

         --  pragma Wide_Character_Encoding (IDENTIFIER);

         when Pragma_Wide_Character_Encoding =>
            GNAT_Pragma;

            --  Nothing to do, handled in parser. Note that we do not enforce
            --  configuration pragma placement, this pragma can appear at any
            --  place in the source, allowing mixed encodings within a single
            --  source program.

            null;

         --------------------
         -- Unknown_Pragma --
         --------------------

         --  Should be impossible, since the case of an unknown pragma is
         --  separately processed before the case statement is entered.

         when Unknown_Pragma =>
            raise Program_Error;
      end case;

      --  AI05-0144: detect dangerous order dependence. Disabled for now,
      --  until AI is formally approved.

      --  Check_Order_Dependence;

   exception
      when Pragma_Exit => null;
   end Analyze_Pragma;

   -----------------------------
   -- Analyze_TC_In_Decl_Part --
   -----------------------------

   procedure Analyze_TC_In_Decl_Part (N : Node_Id; S : Entity_Id) is
   begin
      --  Install formals and push subprogram spec onto scope stack so that we
      --  can see the formals from the pragma.

      Install_Formals (S);
      Push_Scope (S);

      --  Preanalyze the boolean expressions, we treat these as spec
      --  expressions (i.e. similar to a default expression).

      Preanalyze_TC_Args (N,
                          Get_Requires_From_Test_Case_Pragma (N),
                          Get_Ensures_From_Test_Case_Pragma (N));

      --  Remove the subprogram from the scope stack now that the pre-analysis
      --  of the expressions in the test-case is done.

      End_Scope;
   end Analyze_TC_In_Decl_Part;

   --------------------
   -- Check_Disabled --
   --------------------

   function Check_Disabled (Nam : Name_Id) return Boolean is
      PP : Node_Id;

   begin
      --  Loop through entries in check policy list

      PP := Opt.Check_Policy_List;
      loop
         --  If there are no specific entries that matched, then nothing is
         --  disabled, so return False.

         if No (PP) then
            return False;

         --  Here we have an entry see if it matches

         else
            declare
               PPA : constant List_Id := Pragma_Argument_Associations (PP);
            begin
               if Nam = Chars (Get_Pragma_Arg (First (PPA))) then
                  return Chars (Get_Pragma_Arg (Last (PPA))) = Name_Disable;
               else
                  PP := Next_Pragma (PP);
               end if;
            end;
         end if;
      end loop;
   end Check_Disabled;

   -------------------
   -- Check_Enabled --
   -------------------

   function Check_Enabled (Nam : Name_Id) return Boolean is
      PP : Node_Id;

   begin
      --  Loop through entries in check policy list

      PP := Opt.Check_Policy_List;
      loop
         --  If there are no specific entries that matched, then we let the
         --  setting of assertions govern. Note that this provides the needed
         --  compatibility with the RM for the cases of assertion, invariant,
         --  precondition, predicate, and postcondition.

         if No (PP) then
            return Assertions_Enabled;

         --  Here we have an entry see if it matches

         else
            declare
               PPA : constant List_Id := Pragma_Argument_Associations (PP);

            begin
               if Nam = Chars (Get_Pragma_Arg (First (PPA))) then
                  case (Chars (Get_Pragma_Arg (Last (PPA)))) is
                     when Name_On | Name_Check =>
                        return True;
                     when Name_Off | Name_Ignore =>
                        return False;
                     when others =>
                        raise Program_Error;
                  end case;

               else
                  PP := Next_Pragma (PP);
               end if;
            end;
         end if;
      end loop;
   end Check_Enabled;

   ---------------------------------
   -- Delay_Config_Pragma_Analyze --
   ---------------------------------

   function Delay_Config_Pragma_Analyze (N : Node_Id) return Boolean is
   begin
      return Pragma_Name (N) = Name_Interrupt_State
               or else
             Pragma_Name (N) = Name_Priority_Specific_Dispatching;
   end Delay_Config_Pragma_Analyze;

   -------------------------
   -- Get_Base_Subprogram --
   -------------------------

   function Get_Base_Subprogram (Def_Id : Entity_Id) return Entity_Id is
      Result : Entity_Id;

   begin
      --  Follow subprogram renaming chain

      Result := Def_Id;
      while Is_Subprogram (Result)
        and then
          Nkind (Parent (Declaration_Node (Result))) =
                                         N_Subprogram_Renaming_Declaration
        and then Present (Alias (Result))
      loop
         Result := Alias (Result);
      end loop;

      return Result;
   end Get_Base_Subprogram;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Externals.Init;
   end Initialize;

   -----------------------------
   -- Is_Config_Static_String --
   -----------------------------

   function Is_Config_Static_String (Arg : Node_Id) return Boolean is

      function Add_Config_Static_String (Arg : Node_Id) return Boolean;
      --  This is an internal recursive function that is just like the outer
      --  function except that it adds the string to the name buffer rather
      --  than placing the string in the name buffer.

      ------------------------------
      -- Add_Config_Static_String --
      ------------------------------

      function Add_Config_Static_String (Arg : Node_Id) return Boolean is
         N : Node_Id;
         C : Char_Code;

      begin
         N := Arg;

         if Nkind (N) = N_Op_Concat then
            if Add_Config_Static_String (Left_Opnd (N)) then
               N := Right_Opnd (N);
            else
               return False;
            end if;
         end if;

         if Nkind (N) /= N_String_Literal then
            Error_Msg_N ("string literal expected for pragma argument", N);
            return False;

         else
            for J in 1 .. String_Length (Strval (N)) loop
               C := Get_String_Char (Strval (N), J);

               if not In_Character_Range (C) then
                  Error_Msg
                    ("string literal contains invalid wide character",
                     Sloc (N) + 1 + Source_Ptr (J));
                  return False;
               end if;

               Add_Char_To_Name_Buffer (Get_Character (C));
            end loop;
         end if;

         return True;
      end Add_Config_Static_String;

   --  Start of processing for Is_Config_Static_String

   begin

      Name_Len := 0;
      return Add_Config_Static_String (Arg);
   end Is_Config_Static_String;

   -----------------------------------------
   -- Is_Non_Significant_Pragma_Reference --
   -----------------------------------------

   --  This function makes use of the following static table which indicates
   --  whether appearance of some name in a given pragma is to be considered
   --  as a reference for the purposes of warnings about unreferenced objects.

   --  -1  indicates that references in any argument position are significant
   --  0   indicates that appearance in any argument is not significant
   --  +n  indicates that appearance as argument n is significant, but all
   --      other arguments are not significant
   --  99  special processing required (e.g. for pragma Check)

   Sig_Flags : constant array (Pragma_Id) of Int :=
     (Pragma_AST_Entry                      => -1,
      Pragma_Abort_Defer                    => -1,
      Pragma_Ada_83                         => -1,
      Pragma_Ada_95                         => -1,
      Pragma_Ada_05                         => -1,
      Pragma_Ada_2005                       => -1,
      Pragma_Ada_12                         => -1,
      Pragma_Ada_2012                       => -1,
      Pragma_All_Calls_Remote               => -1,
      Pragma_Annotate                       => -1,
      Pragma_Assert                         => -1,
      Pragma_Assertion_Policy               =>  0,
      Pragma_Assume_No_Invalid_Values       =>  0,
      Pragma_Asynchronous                   => -1,
      Pragma_Atomic                         =>  0,
      Pragma_Atomic_Components              =>  0,
      Pragma_Attach_Handler                 => -1,
      Pragma_Check                          => 99,
      Pragma_Check_Name                     =>  0,
      Pragma_Check_Policy                   =>  0,
      Pragma_CIL_Constructor                => -1,
      Pragma_CPP_Class                      =>  0,
      Pragma_CPP_Constructor                =>  0,
      Pragma_CPP_Virtual                    =>  0,
      Pragma_CPP_Vtable                     =>  0,
      Pragma_CPU                            => -1,
      Pragma_C_Pass_By_Copy                 =>  0,
      Pragma_Comment                        =>  0,
      Pragma_Common_Object                  => -1,
      Pragma_Compile_Time_Error             => -1,
      Pragma_Compile_Time_Warning           => -1,
      Pragma_Compiler_Unit                  =>  0,
      Pragma_Complete_Representation        =>  0,
      Pragma_Complex_Representation         =>  0,
      Pragma_Component_Alignment            => -1,
      Pragma_Controlled                     =>  0,
      Pragma_Convention                     =>  0,
      Pragma_Convention_Identifier          =>  0,
      Pragma_Debug                          => -1,
      Pragma_Debug_Policy                   =>  0,
      Pragma_Detect_Blocking                => -1,
      Pragma_Default_Storage_Pool           => -1,
      Pragma_Disable_Atomic_Synchronization => -1,
      Pragma_Discard_Names                  =>  0,
      Pragma_Dispatching_Domain             => -1,
      Pragma_Elaborate                      => -1,
      Pragma_Elaborate_All                  => -1,
      Pragma_Elaborate_Body                 => -1,
      Pragma_Elaboration_Checks             => -1,
      Pragma_Eliminate                      => -1,
      Pragma_Enable_Atomic_Synchronization  => -1,
      Pragma_Export                         => -1,
      Pragma_Export_Exception               => -1,
      Pragma_Export_Function                => -1,
      Pragma_Export_Object                  => -1,
      Pragma_Export_Procedure               => -1,
      Pragma_Export_Value                   => -1,
      Pragma_Export_Valued_Procedure        => -1,
      Pragma_Extend_System                  => -1,
      Pragma_Extensions_Allowed             => -1,
      Pragma_External                       => -1,
      Pragma_Favor_Top_Level                => -1,
      Pragma_External_Name_Casing           => -1,
      Pragma_Fast_Math                      => -1,
      Pragma_Finalize_Storage_Only          =>  0,
      Pragma_Float_Representation           =>  0,
      Pragma_Ident                          => -1,
      Pragma_Implementation_Defined         => -1,
      Pragma_Implemented                    => -1,
      Pragma_Implicit_Packing               =>  0,
      Pragma_Import                         => +2,
      Pragma_Import_Exception               =>  0,
      Pragma_Import_Function                =>  0,
      Pragma_Import_Object                  =>  0,
      Pragma_Import_Procedure               =>  0,
      Pragma_Import_Valued_Procedure        =>  0,
      Pragma_Independent                    =>  0,
      Pragma_Independent_Components         =>  0,
      Pragma_Initialize_Scalars             => -1,
      Pragma_Inline                         =>  0,
      Pragma_Inline_Always                  =>  0,
      Pragma_Inline_Generic                 =>  0,
      Pragma_Inspection_Point               => -1,
      Pragma_Interface                      => +2,
      Pragma_Interface_Name                 => +2,
      Pragma_Interrupt_Handler              => -1,
      Pragma_Interrupt_Priority             => -1,
      Pragma_Interrupt_State                => -1,
      Pragma_Invariant                      => -1,
      Pragma_Java_Constructor               => -1,
      Pragma_Java_Interface                 => -1,
      Pragma_Keep_Names                     =>  0,
      Pragma_License                        => -1,
      Pragma_Link_With                      => -1,
      Pragma_Linker_Alias                   => -1,
      Pragma_Linker_Constructor             => -1,
      Pragma_Linker_Destructor              => -1,
      Pragma_Linker_Options                 => -1,
      Pragma_Linker_Section                 => -1,
      Pragma_List                           => -1,
      Pragma_Locking_Policy                 => -1,
      Pragma_Long_Float                     => -1,
      Pragma_Machine_Attribute              => -1,
      Pragma_Main                           => -1,
      Pragma_Main_Storage                   => -1,
      Pragma_Memory_Size                    => -1,
      Pragma_No_Return                      =>  0,
      Pragma_No_Body                        =>  0,
      Pragma_No_Run_Time                    => -1,
      Pragma_No_Strict_Aliasing             => -1,
      Pragma_Normalize_Scalars              => -1,
      Pragma_Obsolescent                    =>  0,
      Pragma_Optimize                       => -1,
      Pragma_Optimize_Alignment             => -1,
      Pragma_Ordered                        =>  0,
      Pragma_Pack                           =>  0,
      Pragma_Page                           => -1,
      Pragma_Passive                        => -1,
      Pragma_Preelaborable_Initialization   => -1,
      Pragma_Polling                        => -1,
      Pragma_Persistent_BSS                 =>  0,
      Pragma_Postcondition                  => -1,
      Pragma_Precondition                   => -1,
      Pragma_Predicate                      => -1,
      Pragma_Preelaborate                   => -1,
      Pragma_Preelaborate_05                => -1,
      Pragma_Priority                       => -1,
      Pragma_Priority_Specific_Dispatching  => -1,
      Pragma_Profile                        =>  0,
      Pragma_Profile_Warnings               =>  0,
      Pragma_Propagate_Exceptions           => -1,
      Pragma_Psect_Object                   => -1,
      Pragma_Pure                           => -1,
      Pragma_Pure_05                        => -1,
      Pragma_Pure_12                        => -1,
      Pragma_Pure_Function                  => -1,
      Pragma_Queuing_Policy                 => -1,
      Pragma_Ravenscar                      => -1,
      Pragma_Relative_Deadline              => -1,
      Pragma_Remote_Access_Type             => -1,
      Pragma_Remote_Call_Interface          => -1,
      Pragma_Remote_Types                   => -1,
      Pragma_Restricted_Run_Time            => -1,
      Pragma_Restriction_Warnings           => -1,
      Pragma_Restrictions                   => -1,
      Pragma_Reviewable                     => -1,
      Pragma_Short_Circuit_And_Or           => -1,
      Pragma_Share_Generic                  => -1,
      Pragma_Shared                         => -1,
      Pragma_Shared_Passive                 => -1,
      Pragma_Short_Descriptors              =>  0,
      Pragma_Simple_Storage_Pool            =>  0,
      Pragma_Source_File_Name               => -1,
      Pragma_Source_File_Name_Project       => -1,
      Pragma_Source_Reference               => -1,
      Pragma_Storage_Size                   => -1,
      Pragma_Storage_Unit                   => -1,
      Pragma_Static_Elaboration_Desired     => -1,
      Pragma_Stream_Convert                 => -1,
      Pragma_Style_Checks                   => -1,
      Pragma_Subtitle                       => -1,
      Pragma_Suppress                       =>  0,
      Pragma_Suppress_Exception_Locations   =>  0,
      Pragma_Suppress_All                   => -1,
      Pragma_Suppress_Debug_Info            =>  0,
      Pragma_Suppress_Initialization        =>  0,
      Pragma_System_Name                    => -1,
      Pragma_Task_Dispatching_Policy        => -1,
      Pragma_Task_Info                      => -1,
      Pragma_Task_Name                      => -1,
      Pragma_Task_Storage                   =>  0,
      Pragma_Test_Case                      => -1,
      Pragma_Thread_Local_Storage           =>  0,
      Pragma_Time_Slice                     => -1,
      Pragma_Title                          => -1,
      Pragma_Unchecked_Union                =>  0,
      Pragma_Unimplemented_Unit             => -1,
      Pragma_Universal_Aliasing             => -1,
      Pragma_Universal_Data                 => -1,
      Pragma_Unmodified                     => -1,
      Pragma_Unreferenced                   => -1,
      Pragma_Unreferenced_Objects           => -1,
      Pragma_Unreserve_All_Interrupts       => -1,
      Pragma_Unsuppress                     =>  0,
      Pragma_Use_VADS_Size                  => -1,
      Pragma_Validity_Checks                => -1,
      Pragma_Volatile                       =>  0,
      Pragma_Volatile_Components            =>  0,
      Pragma_Warnings                       => -1,
      Pragma_Weak_External                  => -1,
      Pragma_Wide_Character_Encoding        =>  0,
      Unknown_Pragma                        =>  0);

   function Is_Non_Significant_Pragma_Reference (N : Node_Id) return Boolean is
      Id : Pragma_Id;
      P  : Node_Id;
      C  : Int;
      A  : Node_Id;

   begin
      P := Parent (N);

      if Nkind (P) /= N_Pragma_Argument_Association then
         return False;

      else
         Id := Get_Pragma_Id (Parent (P));
         C := Sig_Flags (Id);

         case C is
            when -1 =>
               return False;

            when 0 =>
               return True;

            when 99 =>
               case Id is

                  --  For pragma Check, the first argument is not significant,
                  --  the second and the third (if present) arguments are
                  --  significant.

                  when Pragma_Check =>
                     return
                       P = First (Pragma_Argument_Associations (Parent (P)));

                  when others =>
                     raise Program_Error;
               end case;

            when others =>
               A := First (Pragma_Argument_Associations (Parent (P)));
               for J in 1 .. C - 1 loop
                  if No (A) then
                     return False;
                  end if;

                  Next (A);
               end loop;

               return A = P; -- is this wrong way round ???
         end case;
      end if;
   end Is_Non_Significant_Pragma_Reference;

   ------------------------------
   -- Is_Pragma_String_Literal --
   ------------------------------

   --  This function returns true if the corresponding pragma argument is a
   --  static string expression. These are the only cases in which string
   --  literals can appear as pragma arguments. We also allow a string literal
   --  as the first argument to pragma Assert (although it will of course
   --  always generate a type error).

   function Is_Pragma_String_Literal (Par : Node_Id) return Boolean is
      Pragn : constant Node_Id := Parent (Par);
      Assoc : constant List_Id := Pragma_Argument_Associations (Pragn);
      Pname : constant Name_Id := Pragma_Name (Pragn);
      Argn  : Natural;
      N     : Node_Id;

   begin
      Argn := 1;
      N := First (Assoc);
      loop
         exit when N = Par;
         Argn := Argn + 1;
         Next (N);
      end loop;

      if Pname = Name_Assert then
         return True;

      elsif Pname = Name_Export then
         return Argn > 2;

      elsif Pname = Name_Ident then
         return Argn = 1;

      elsif Pname = Name_Import then
         return Argn > 2;

      elsif Pname = Name_Interface_Name then
         return Argn > 1;

      elsif Pname = Name_Linker_Alias then
         return Argn = 2;

      elsif Pname = Name_Linker_Section then
         return Argn = 2;

      elsif Pname = Name_Machine_Attribute then
         return Argn = 2;

      elsif Pname = Name_Source_File_Name then
         return True;

      elsif Pname = Name_Source_Reference then
         return Argn = 2;

      elsif Pname = Name_Title then
         return True;

      elsif Pname = Name_Subtitle then
         return True;

      else
         return False;
      end if;
   end Is_Pragma_String_Literal;

   -----------------------------------------
   -- Make_Aspect_For_PPC_In_Gen_Sub_Decl --
   -----------------------------------------

   procedure Make_Aspect_For_PPC_In_Gen_Sub_Decl (Decl : Node_Id) is
      Aspects : constant List_Id := New_List;
      Loc     : constant Source_Ptr := Sloc (Decl);
      Or_Decl : constant Node_Id := Original_Node (Decl);

      Original_Aspects : List_Id;
      --  To capture global references, a copy of the created aspects must be
      --  inserted in the original tree.

      Prag         : Node_Id;
      Prag_Arg_Ass : Node_Id;
      Prag_Id      : Pragma_Id;

   begin
      --  Check for any PPC pragmas that appear within Decl

      Prag := Next (Decl);
      while Nkind (Prag) = N_Pragma loop
         Prag_Id := Get_Pragma_Id (Chars (Pragma_Identifier (Prag)));

         case Prag_Id is
            when Pragma_Postcondition | Pragma_Precondition =>
               Prag_Arg_Ass := First (Pragma_Argument_Associations (Prag));

               --  Make an aspect from any PPC pragma

               Append_To (Aspects,
                 Make_Aspect_Specification (Loc,
                   Identifier =>
                     Make_Identifier (Loc, Chars (Pragma_Identifier (Prag))),
                   Expression =>
                     Copy_Separate_Tree (Expression (Prag_Arg_Ass))));

               --  Generate the analysis information in the pragma expression
               --  and then set the pragma node analyzed to avoid any further
               --  analysis.

               Analyze (Expression (Prag_Arg_Ass));
               Set_Analyzed (Prag, True);

            when others => null;
         end case;

         Next (Prag);
      end loop;

      --  Set all new aspects into the generic declaration node

      if Is_Non_Empty_List (Aspects) then

         --  Create the list of aspects to be inserted in the original tree

         Original_Aspects := Copy_Separate_List (Aspects);

         --  Check if Decl already has aspects

         --  Attach the new lists of aspects to both the generic copy and the
         --  original tree.

         if Has_Aspects (Decl) then
            Append_List (Aspects, Aspect_Specifications (Decl));
            Append_List (Original_Aspects, Aspect_Specifications (Or_Decl));

         else
            Set_Parent (Aspects, Decl);
            Set_Aspect_Specifications (Decl, Aspects);
            Set_Parent (Original_Aspects, Or_Decl);
            Set_Aspect_Specifications (Or_Decl, Original_Aspects);
         end if;
      end if;
   end Make_Aspect_For_PPC_In_Gen_Sub_Decl;

   ------------------------
   -- Preanalyze_TC_Args --
   ------------------------

   procedure Preanalyze_TC_Args (N, Arg_Req, Arg_Ens : Node_Id) is
   begin
      --  Preanalyze the boolean expressions, we treat these as spec
      --  expressions (i.e. similar to a default expression).

      if Present (Arg_Req) then
         Preanalyze_Spec_Expression
           (Get_Pragma_Arg (Arg_Req), Standard_Boolean);

         --  In ASIS mode, for a pragma generated from a source aspect, also
         --  analyze the original aspect expression.

         if ASIS_Mode and then Present (Corresponding_Aspect (N)) then
            Preanalyze_Spec_Expression
              (Original_Node (Get_Pragma_Arg (Arg_Req)), Standard_Boolean);
         end if;
      end if;

      if Present (Arg_Ens) then
         Preanalyze_Spec_Expression
           (Get_Pragma_Arg (Arg_Ens), Standard_Boolean);

         --  In ASIS mode, for a pragma generated from a source aspect, also
         --  analyze the original aspect expression.

         if ASIS_Mode and then Present (Corresponding_Aspect (N)) then
            Preanalyze_Spec_Expression
              (Original_Node (Get_Pragma_Arg (Arg_Ens)), Standard_Boolean);
         end if;
      end if;
   end Preanalyze_TC_Args;

   --------------------------------------
   -- Process_Compilation_Unit_Pragmas --
   --------------------------------------

   procedure Process_Compilation_Unit_Pragmas (N : Node_Id) is
   begin
      --  A special check for pragma Suppress_All, a very strange DEC pragma,
      --  strange because it comes at the end of the unit. Rational has the
      --  same name for a pragma, but treats it as a program unit pragma, In
      --  GNAT we just decide to allow it anywhere at all. If it appeared then
      --  the flag Has_Pragma_Suppress_All was set on the compilation unit
      --  node, and we insert a pragma Suppress (All_Checks) at the start of
      --  the context clause to ensure the correct processing.

      if Has_Pragma_Suppress_All (N) then
         Prepend_To (Context_Items (N),
           Make_Pragma (Sloc (N),
             Chars                        => Name_Suppress,
             Pragma_Argument_Associations => New_List (
               Make_Pragma_Argument_Association (Sloc (N),
                 Expression => Make_Identifier (Sloc (N), Name_All_Checks)))));
      end if;

      --  Nothing else to do at the current time!

   end Process_Compilation_Unit_Pragmas;

   --------
   -- rv --
   --------

   procedure rv is
   begin
      null;
   end rv;

   --------------------------------
   -- Set_Encoded_Interface_Name --
   --------------------------------

   procedure Set_Encoded_Interface_Name (E : Entity_Id; S : Node_Id) is
      Str : constant String_Id := Strval (S);
      Len : constant Int       := String_Length (Str);
      CC  : Char_Code;
      C   : Character;
      J   : Int;

      Hex : constant array (0 .. 15) of Character := "0123456789abcdef";

      procedure Encode;
      --  Stores encoded value of character code CC. The encoding we use an
      --  underscore followed by four lower case hex digits.

      ------------
      -- Encode --
      ------------

      procedure Encode is
      begin
         Store_String_Char (Get_Char_Code ('_'));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC / 2 ** 12))));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC / 2 ** 8 and 16#0F#))));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC / 2 ** 4 and 16#0F#))));
         Store_String_Char
           (Get_Char_Code (Hex (Integer (CC and 16#0F#))));
      end Encode;

   --  Start of processing for Set_Encoded_Interface_Name

   begin
      --  If first character is asterisk, this is a link name, and we leave it
      --  completely unmodified. We also ignore null strings (the latter case
      --  happens only in error cases) and no encoding should occur for Java or
      --  AAMP interface names.

      if Len = 0
        or else Get_String_Char (Str, 1) = Get_Char_Code ('*')
        or else VM_Target /= No_VM
        or else AAMP_On_Target
      then
         Set_Interface_Name (E, S);

      else
         J := 1;
         loop
            CC := Get_String_Char (Str, J);

            exit when not In_Character_Range (CC);

            C := Get_Character (CC);

            exit when C /= '_' and then C /= '$'
              and then C not in '0' .. '9'
              and then C not in 'a' .. 'z'
              and then C not in 'A' .. 'Z';

            if J = Len then
               Set_Interface_Name (E, S);
               return;

            else
               J := J + 1;
            end if;
         end loop;

         --  Here we need to encode. The encoding we use as follows:
         --     three underscores  + four hex digits (lower case)

         Start_String;

         for J in 1 .. String_Length (Str) loop
            CC := Get_String_Char (Str, J);

            if not In_Character_Range (CC) then
               Encode;
            else
               C := Get_Character (CC);

               if C = '_' or else C = '$'
                 or else C in '0' .. '9'
                 or else C in 'a' .. 'z'
                 or else C in 'A' .. 'Z'
               then
                  Store_String_Char (CC);
               else
                  Encode;
               end if;
            end if;
         end loop;

         Set_Interface_Name (E,
           Make_String_Literal (Sloc (S),
             Strval => End_String));
      end if;
   end Set_Encoded_Interface_Name;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name (N : Node_Id; With_Item : Node_Id) is
      Pref : Node_Id;
      Scop : Entity_Id;

   begin
      if Nkind (N) = N_Identifier
        and then Nkind (With_Item) = N_Identifier
      then
         Set_Entity (N, Entity (With_Item));

      elsif Nkind (N) = N_Selected_Component then
         Change_Selected_Component_To_Expanded_Name (N);
         Set_Entity (N, Entity (With_Item));
         Set_Entity (Selector_Name (N), Entity (N));

         Pref := Prefix (N);
         Scop := Scope (Entity (N));
         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);
            Set_Entity (Selector_Name (Pref), Scop);
            Set_Entity (Pref, Scop);
            Pref := Prefix (Pref);
            Scop := Scope (Scop);
         end loop;

         Set_Entity (Pref, Scop);
      end if;
   end Set_Unit_Name;

end Sem_Prag;
