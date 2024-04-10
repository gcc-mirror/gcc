------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            C O N T R A C T S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2015-2024, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Prag;       use Exp_Prag;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Lib;            use Lib;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch12;       use Sem_Ch12;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Disp;       use Sem_Disp;
with Sem_Prag;       use Sem_Prag;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Warnsw;         use Warnsw;

package body Contracts is

   Contract_Error : exception;
   --  This exception is raised by Add_Contract_Item when it is invoked on an
   --  invalid pragma. Note that clients of the package must filter them out
   --  before invoking Add_Contract_Item, so it should not escape the package.

   procedure Analyze_Package_Instantiation_Contract (Inst_Id : Entity_Id);
   --  Analyze all delayed pragmas chained on the contract of package
   --  instantiation Inst_Id as if they appear at the end of a declarative
   --  region. The pragmas in question are:
   --
   --    Part_Of

   procedure Build_Subprogram_Contract_Wrapper
     (Body_Id : Entity_Id;
      Stmts   : List_Id;
      Decls   : List_Id;
      Result  : Entity_Id);
   --  Generate a wrapper for a given subprogram body when the expansion of
   --  postconditions require it by moving its declarations and statements
   --  into a locally declared subprogram _Wrapped_Statements.

   --  Postcondition and precondition checks then get inserted in place of
   --  the original statements and declarations along with a call to
   --  _Wrapped_Statements.

   procedure Check_Class_Condition
     (Cond            : Node_Id;
      Subp            : Entity_Id;
      Par_Subp        : Entity_Id;
      Is_Precondition : Boolean);
   --  Perform checking of class-wide pre/postcondition Cond inherited by Subp
   --  from Par_Subp. Is_Precondition enables check specific for preconditions.
   --  In SPARK_Mode, an inherited operation that is not overridden but has
   --  inherited modified conditions pre/postconditions is illegal.

   function Is_Prologue_Renaming (Decl : Node_Id) return Boolean;
   --  Determine whether arbitrary declaration Decl denotes a renaming of
   --  a discriminant or protection field _object.

   procedure Check_Type_Or_Object_External_Properties
     (Type_Or_Obj_Id : Entity_Id);
   --  Perform checking of external properties pragmas that is common to both
   --  type declarations and object declarations.

   procedure Expand_Subprogram_Contract (Body_Id : Entity_Id);
   --  Expand the contracts of a subprogram body and its correspoding spec (if
   --  any). This routine processes all [refined] pre- and postconditions as
   --  well as Always_Terminates, Contract_Cases, Exceptional_Cases,
   --  Subprogram_Variant, invariants and predicates. Body_Id denotes the
   --  entity of the subprogram body.

   procedure Preanalyze_Condition
     (Subp : Entity_Id;
      Expr : Node_Id);
   --  Preanalyze the class-wide condition Expr of Subp

   procedure Set_Class_Condition
     (Kind : Condition_Kind;
      Subp : Entity_Id;
      Cond : Node_Id);
   --  Set the class-wide Kind condition of Subp

   -----------------------
   -- Add_Contract_Item --
   -----------------------

   procedure Add_Contract_Item (Prag : Node_Id; Id : Entity_Id) is
      Items : Node_Id := Contract (Id);

      procedure Add_Classification;
      --  Prepend Prag to the list of classifications

      procedure Add_Contract_Test_Case;
      --  Prepend Prag to the list of contract and test cases

      procedure Add_Pre_Post_Condition;
      --  Prepend Prag to the list of pre- and postconditions

      ------------------------
      -- Add_Classification --
      ------------------------

      procedure Add_Classification is
      begin
         Set_Next_Pragma (Prag, Classifications (Items));
         Set_Classifications (Items, Prag);
      end Add_Classification;

      ----------------------------
      -- Add_Contract_Test_Case --
      ----------------------------

      procedure Add_Contract_Test_Case is
      begin
         Set_Next_Pragma (Prag, Contract_Test_Cases (Items));
         Set_Contract_Test_Cases (Items, Prag);
      end Add_Contract_Test_Case;

      ----------------------------
      -- Add_Pre_Post_Condition --
      ----------------------------

      procedure Add_Pre_Post_Condition is
      begin
         Set_Next_Pragma (Prag, Pre_Post_Conditions (Items));
         Set_Pre_Post_Conditions (Items, Prag);
      end Add_Pre_Post_Condition;

      --  Local variables

      --  A contract must contain only pragmas

      pragma Assert (Nkind (Prag) = N_Pragma);
      Prag_Nam : constant Name_Id := Pragma_Name (Prag);

   --  Start of processing for Add_Contract_Item

   begin
      --  Create a new contract when adding the first item

      if No (Items) then
         Items := Make_Contract (Sloc (Id));
         Set_Contract (Id, Items);
      end if;

      --  Constants, the applicable pragmas are:
      --    Part_Of

      if Ekind (Id) = E_Constant then
         if Prag_Nam in Name_Async_Readers
                      | Name_Async_Writers
                      | Name_Effective_Reads
                      | Name_Effective_Writes
                      | Name_No_Caching
                      | Name_Part_Of
         then
            Add_Classification;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  Entry bodies, the applicable pragmas are:
      --    Refined_Depends
      --    Refined_Global
      --    Refined_Post

      elsif Is_Entry_Body (Id) then
         if Prag_Nam in Name_Refined_Depends | Name_Refined_Global then
            Add_Classification;

         elsif Prag_Nam = Name_Refined_Post then
            Add_Pre_Post_Condition;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  Entry or subprogram declarations, the applicable pragmas are:
      --    Always_Terminates
      --    Attach_Handler
      --    Contract_Cases
      --    Depends
      --    Exceptional_Cases
      --    Extensions_Visible
      --    Global
      --    Interrupt_Handler
      --    Postcondition
      --    Precondition
      --    Side_Effects
      --    Subprogram_Variant
      --    Test_Case
      --    Volatile_Function

      elsif Is_Entry_Declaration (Id)
        or else Ekind (Id) in E_Function
                            | E_Generic_Function
                            | E_Generic_Procedure
                            | E_Procedure
      then
         if Prag_Nam in Name_Attach_Handler | Name_Interrupt_Handler
           and then Ekind (Id) in E_Generic_Procedure | E_Procedure
         then
            Add_Classification;

         elsif Prag_Nam in Name_Depends
                         | Name_Extensions_Visible
                         | Name_Global
                         | Name_Side_Effects
         then
            Add_Classification;

         elsif Prag_Nam = Name_Volatile_Function
           and then Ekind (Id) in E_Function | E_Generic_Function
         then
            Add_Classification;

         elsif Prag_Nam in Name_Always_Terminates
                         | Name_Contract_Cases
                         | Name_Exceptional_Cases
                         | Name_Subprogram_Variant
                         | Name_Test_Case
         then
            Add_Contract_Test_Case;

         elsif Prag_Nam in Name_Postcondition | Name_Precondition then
            Add_Pre_Post_Condition;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  Packages or instantiations, the applicable pragmas are:
      --    Abstract_States
      --    Initial_Condition
      --    Initializes
      --    Part_Of (instantiation only)

      elsif Is_Package_Or_Generic_Package (Id) then
         if Prag_Nam in Name_Abstract_State
                      | Name_Initial_Condition
                      | Name_Initializes
         then
            Add_Classification;

         --  Indicator Part_Of must be associated with a package instantiation

         elsif Prag_Nam = Name_Part_Of and then Is_Generic_Instance (Id) then
            Add_Classification;

         elsif Prag_Nam = Name_Always_Terminates then
            Add_Contract_Test_Case;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  Package bodies, the applicable pragmas are:
      --    Refined_States

      elsif Ekind (Id) = E_Package_Body then
         if Prag_Nam = Name_Refined_State then
            Add_Classification;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  The four volatility refinement pragmas are ok for all types.
      --  Part_Of is ok for task types and protected types.
      --  Depends and Global are ok for task types.
      --
      --  Precondition and Postcondition are added separately; they are allowed
      --  for access-to-subprogram types.

      elsif Is_Type (Id) then
         declare
            Is_OK_Classification : constant Boolean :=
              Prag_Nam in Name_Async_Readers
                        | Name_Async_Writers
                        | Name_Effective_Reads
                        | Name_Effective_Writes
                        | Name_No_Caching
              or else (Ekind (Id) = E_Task_Type
                         and Prag_Nam in Name_Part_Of
                                       | Name_Depends
                                       | Name_Global)
              or else (Ekind (Id) = E_Protected_Type
                         and Prag_Nam = Name_Part_Of);

         begin
            if Is_OK_Classification then
               Add_Classification;

            elsif Ekind (Id) = E_Subprogram_Type
                and then Prag_Nam in Name_Precondition
                                   | Name_Postcondition
            then
               Add_Pre_Post_Condition;
            else

               --  The pragma is not a proper contract item

               raise Contract_Error;
            end if;
         end;

      --  Subprogram bodies, the applicable pragmas are:
      --    Postcondition
      --    Precondition
      --    Refined_Depends
      --    Refined_Global
      --    Refined_Post

      elsif Ekind (Id) = E_Subprogram_Body then
         if Prag_Nam in Name_Refined_Depends | Name_Refined_Global then
            Add_Classification;

         elsif Prag_Nam in Name_Postcondition
                         | Name_Precondition
                         | Name_Refined_Post
         then
            Add_Pre_Post_Condition;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  Task bodies, the applicable pragmas are:
      --    Refined_Depends
      --    Refined_Global

      elsif Ekind (Id) = E_Task_Body then
         if Prag_Nam in Name_Refined_Depends | Name_Refined_Global then
            Add_Classification;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      --  Task units, the applicable pragmas are:
      --    Depends
      --    Global
      --    Part_Of

      --  Variables, the applicable pragmas are:
      --    Async_Readers
      --    Async_Writers
      --    Constant_After_Elaboration
      --    Depends
      --    Effective_Reads
      --    Effective_Writes
      --    Global
      --    No_Caching
      --    Part_Of

      elsif Ekind (Id) = E_Variable then
         if Prag_Nam in Name_Async_Readers
                      | Name_Async_Writers
                      | Name_Constant_After_Elaboration
                      | Name_Depends
                      | Name_Effective_Reads
                      | Name_Effective_Writes
                      | Name_Global
                      | Name_No_Caching
                      | Name_Part_Of
         then
            Add_Classification;

         --  The pragma is not a proper contract item

         else
            raise Contract_Error;
         end if;

      else
         raise Contract_Error;
      end if;
   end Add_Contract_Item;

   -----------------------
   -- Analyze_Contracts --
   -----------------------

   procedure Analyze_Contracts (L : List_Id) is
      Decl : Node_Id;

   begin
      Decl := First (L);
      while Present (Decl) loop

         --  Entry or subprogram declarations

         if Nkind (Decl) in N_Abstract_Subprogram_Declaration
                          | N_Entry_Declaration
                          | N_Generic_Subprogram_Declaration
                          | N_Subprogram_Declaration
         then
            Analyze_Entry_Or_Subprogram_Contract (Defining_Entity (Decl));

         --  Entry or subprogram bodies

         elsif Nkind (Decl) in N_Entry_Body | N_Subprogram_Body then
            Analyze_Entry_Or_Subprogram_Body_Contract (Defining_Entity (Decl));

         --  Objects

         elsif Nkind (Decl) = N_Object_Declaration then
            Analyze_Object_Contract (Defining_Entity (Decl));

         --  Package instantiation

         elsif Nkind (Decl) = N_Package_Instantiation then
            Analyze_Package_Instantiation_Contract (Defining_Entity (Decl));

         --  Protected units

         elsif Nkind (Decl) in N_Protected_Type_Declaration
                             | N_Single_Protected_Declaration
         then
            Analyze_Protected_Contract (Defining_Entity (Decl));

         --  Subprogram body stubs

         elsif Nkind (Decl) = N_Subprogram_Body_Stub then
            Analyze_Subprogram_Body_Stub_Contract (Defining_Entity (Decl));

         --  Task units

         elsif Nkind (Decl) in N_Single_Task_Declaration
                             | N_Task_Type_Declaration
         then
            Analyze_Task_Contract (Defining_Entity (Decl));

         --  For type declarations, we need to do the preanalysis of Iterable
         --  and the 3 Xxx_Literal aspect specifications.

         --  Other type aspects need to be resolved here???

         elsif Nkind (Decl) = N_Private_Type_Declaration
           and then Present (Aspect_Specifications (Decl))
         then
            declare
               E  : constant Entity_Id  := Defining_Identifier (Decl);
               It : constant Node_Id    := Find_Aspect (E, Aspect_Iterable);
               I_Lit : constant Node_Id :=
                 Find_Aspect (E, Aspect_Integer_Literal);
               R_Lit : constant Node_Id :=
                 Find_Aspect (E, Aspect_Real_Literal);
               S_Lit : constant Node_Id :=
                 Find_Aspect (E, Aspect_String_Literal);

            begin
               if Present (It) then
                  Validate_Iterable_Aspect (E, It);
               end if;

               if Present (I_Lit) then
                  Validate_Literal_Aspect (E, I_Lit);
               end if;
               if Present (R_Lit) then
                  Validate_Literal_Aspect (E, R_Lit);
               end if;
               if Present (S_Lit) then
                  Validate_Literal_Aspect (E, S_Lit);
               end if;
            end;
         end if;

         if Nkind (Decl) in N_Full_Type_Declaration
                          | N_Private_Type_Declaration
                          | N_Task_Type_Declaration
                          | N_Protected_Type_Declaration
                          | N_Formal_Type_Declaration
         then
            Analyze_Type_Contract (Defining_Identifier (Decl));
         end if;

         Next (Decl);
      end loop;
   end Analyze_Contracts;

   -------------------------------------
   -- Analyze_Pragmas_In_Declarations --
   -------------------------------------

   procedure Analyze_Pragmas_In_Declarations (Body_Id : Entity_Id) is
      Curr_Decl : Node_Id;

   begin
      --  Move through the body's declarations analyzing all pragmas which
      --  appear at the top of the declarations.

      Curr_Decl := First (Declarations (Unit_Declaration_Node (Body_Id)));
      while Present (Curr_Decl) loop

         if Nkind (Curr_Decl) = N_Pragma then

            if Pragma_Significant_To_Subprograms
                 (Get_Pragma_Id (Curr_Decl))
            then
               Analyze (Curr_Decl);
            end if;

         --  Skip the renamings of discriminants and protection fields

         elsif Is_Prologue_Renaming (Curr_Decl) then
            null;

         --  We have reached something which is not a pragma so we can be sure
         --  there are no more contracts or pragmas which need to be taken into
         --  account.

         else
            exit;
         end if;

         Next (Curr_Decl);
      end loop;
   end Analyze_Pragmas_In_Declarations;

   -----------------------------------------------
   -- Analyze_Entry_Or_Subprogram_Body_Contract --
   -----------------------------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Entry_Or_Subprogram_Body_Contract (Body_Id : Entity_Id) is
      Body_Decl : constant Node_Id   := Unit_Declaration_Node (Body_Id);
      Items     : constant Node_Id   := Contract (Body_Id);
      Spec_Id   : constant Entity_Id := Unique_Defining_Entity (Body_Decl);

   begin
      --  When a subprogram body declaration is illegal, its defining entity is
      --  left unanalyzed. There is nothing left to do in this case because the
      --  body lacks a contract, or even a proper Ekind.

      if Ekind (Body_Id) = E_Void then
         return;

      --  Do not analyze a contract multiple times

      elsif Present (Items) then
         if Analyzed (Items) then
            return;
         else
            Set_Analyzed (Items);
         end if;

      --  When this is a subprogram body not coming from source, for example an
      --  expression function, it does not cause freezing of previous contracts
      --  (see Analyze_Subprogram_Body_Helper), in particular not of those on
      --  its spec if it exists. In this case make sure they have been properly
      --  analyzed before being expanded below, as we may be invoked during the
      --  freezing of the subprogram in the middle of its enclosing declarative
      --  part because the declarative part contains e.g. the declaration of a
      --  variable initialized by means of a call to the subprogram.

      elsif Nkind (Body_Decl) = N_Subprogram_Body
        and then not Comes_From_Source (Original_Node (Body_Decl))
        and then Present (Corresponding_Spec (Body_Decl))
        and then Present (Contract (Corresponding_Spec (Body_Decl)))
      then
         Analyze_Entry_Or_Subprogram_Contract (Corresponding_Spec (Body_Decl));
      end if;

      --  Ensure that the contract cases or postconditions mention 'Result or
      --  define a post-state.

      Check_Result_And_Post_State (Body_Id);

      --  Capture all global references in a generic subprogram body now that
      --  the contract has been analyzed.

      if Is_Generic_Declaration_Or_Body (Body_Decl) then
         Save_Global_References_In_Contract
           (Templ  => Original_Node (Body_Decl),
            Gen_Id => Spec_Id);
      end if;

      --  Deal with preconditions, [refined] postconditions, Always_Terminates,
      --  Contract_Cases, Exceptional_Cases, Subprogram_Variant, invariants and
      --  predicates associated with body and its spec. Do not expand the
      --  contract of subprogram body stubs.

      if Nkind (Body_Decl) = N_Subprogram_Body then
         Expand_Subprogram_Contract (Body_Id);
      end if;
   end Analyze_Entry_Or_Subprogram_Body_Contract;

   ------------------------------------------
   -- Analyze_Entry_Or_Subprogram_Contract --
   ------------------------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Entry_Or_Subprogram_Contract
     (Subp_Id   : Entity_Id;
      Freeze_Id : Entity_Id := Empty)
   is
      Items     : constant Node_Id := Contract (Subp_Id);
      Subp_Decl : constant Node_Id :=
        (if Ekind (Subp_Id) = E_Subprogram_Type
         then Associated_Node_For_Itype (Subp_Id)
         else Unit_Declaration_Node (Subp_Id));

      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK_Mode-related data to restore on exit

      Skip_Assert_Exprs : constant Boolean :=
                            Is_Entry (Subp_Id) and then not GNATprove_Mode;

      Depends  : Node_Id := Empty;
      Global   : Node_Id := Empty;
      Prag     : Node_Id;
      Prag_Nam : Name_Id;

   begin
      --  Do not analyze a contract multiple times

      if Present (Items) then
         if Analyzed (Items) then
            return;
         else
            Set_Analyzed (Items);
         end if;
      end if;

      --  Due to the timing of contract analysis, delayed pragmas may be
      --  subject to the wrong SPARK_Mode, usually that of the enclosing
      --  context. To remedy this, restore the original SPARK_Mode of the
      --  related subprogram body.

      Set_SPARK_Mode (Subp_Id);

      --  All subprograms carry a contract, but for some it is not significant
      --  and should not be processed.

      if not Has_Significant_Contract (Subp_Id) then
         null;

      elsif Present (Items) then

         --  Do not analyze the pre/postconditions of an entry declaration
         --  unless annotating the original tree for GNATprove. The
         --  real analysis occurs when the pre/postconditons are relocated to
         --  the contract wrapper procedure (see Build_Contract_Wrapper).

         if Skip_Assert_Exprs then
            null;

         --  Otherwise analyze the pre/postconditions.
         --  If these come from an aspect specification, their expressions
         --  might include references to types that are not frozen yet, in the
         --  case where the body is a rewritten expression function that is a
         --  completion, so freeze all types within before constructing the
         --  contract code.

         else
            declare
               Bod          : Node_Id := Empty;
               Freeze_Types : Boolean := False;

            begin
               if Present (Freeze_Id) then
                  Bod := Unit_Declaration_Node (Freeze_Id);

                  if Nkind (Bod) = N_Subprogram_Body
                    and then Was_Expression_Function (Bod)
                    and then Ekind (Subp_Id) = E_Function
                    and then Chars (Subp_Id) = Chars (Freeze_Id)
                    and then Subp_Id /= Freeze_Id
                  then
                     Freeze_Types := True;
                  end if;
               end if;

               Prag := Pre_Post_Conditions (Items);
               while Present (Prag) loop
                  if Freeze_Types
                    and then Present (Corresponding_Aspect (Prag))
                  then
                     Freeze_Expr_Types
                       (Def_Id => Subp_Id,
                        Typ    => Standard_Boolean,
                        Expr   =>
                          Expression
                            (First (Pragma_Argument_Associations (Prag))),
                        N      => Bod);
                  end if;

                  Analyze_Pre_Post_Condition_In_Decl_Part (Prag, Freeze_Id);
                  Prag := Next_Pragma (Prag);
               end loop;
            end;
         end if;

         --  Analyze contract-cases, subprogram-variant and test-cases

         Prag := Contract_Test_Cases (Items);
         while Present (Prag) loop
            Prag_Nam := Pragma_Name (Prag);

            if Prag_Nam = Name_Always_Terminates then
               Analyze_Always_Terminates_In_Decl_Part (Prag);

            elsif Prag_Nam = Name_Contract_Cases then

               --  Do not analyze the contract cases of an entry declaration
               --  unless annotating the original tree for GNATprove.
               --  The real analysis occurs when the contract cases are moved
               --  to the contract wrapper procedure (Build_Contract_Wrapper).

               if Skip_Assert_Exprs then
                  null;

               --  Otherwise analyze the contract cases

               else
                  Analyze_Contract_Cases_In_Decl_Part (Prag, Freeze_Id);
               end if;

            elsif Prag_Nam = Name_Exceptional_Cases then
               Analyze_Exceptional_Cases_In_Decl_Part (Prag);

            elsif Prag_Nam = Name_Subprogram_Variant then
               Analyze_Subprogram_Variant_In_Decl_Part (Prag);

            else
               pragma Assert (Prag_Nam = Name_Test_Case);
               Analyze_Test_Case_In_Decl_Part (Prag);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         --  Analyze classification pragmas

         Prag := Classifications (Items);
         while Present (Prag) loop
            Prag_Nam := Pragma_Name (Prag);

            if Prag_Nam = Name_Depends then
               Depends := Prag;

            elsif Prag_Nam = Name_Global then
               Global := Prag;
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         --  Analyze Global first, as Depends may mention items classified in
         --  the global categorization.

         if Present (Global) then
            Analyze_Global_In_Decl_Part (Global);
         end if;

         --  Depends must be analyzed after Global in order to see the modes of
         --  all global items.

         if Present (Depends) then
            Analyze_Depends_In_Decl_Part (Depends);
         end if;

         --  Ensure that the contract cases or postconditions mention 'Result
         --  or define a post-state.

         Check_Result_And_Post_State (Subp_Id);
      end if;

      --  Restore the SPARK_Mode of the enclosing context after all delayed
      --  pragmas have been analyzed.

      Restore_SPARK_Mode (Saved_SM, Saved_SMP);

      --  Capture all global references in a generic subprogram now that the
      --  contract has been analyzed.

      if Is_Generic_Declaration_Or_Body (Subp_Decl) then
         Save_Global_References_In_Contract
           (Templ  => Original_Node (Subp_Decl),
            Gen_Id => Subp_Id);
      end if;
   end Analyze_Entry_Or_Subprogram_Contract;

   ----------------------------------------------
   -- Check_Type_Or_Object_External_Properties --
   ----------------------------------------------

   procedure Check_Type_Or_Object_External_Properties
     (Type_Or_Obj_Id : Entity_Id)
   is
      Is_Type_Id : constant Boolean := Is_Type (Type_Or_Obj_Id);

      --  Local variables

      AR_Val : Boolean := False;
      AW_Val : Boolean := False;
      ER_Val : Boolean := False;
      EW_Val : Boolean := False;
      NC_Val : Boolean;
      Seen   : Boolean := False;
      Prag   : Node_Id;

   --  Start of processing for Check_Type_Or_Object_External_Properties

   begin
      --  Analyze all external properties

      if Is_Type_Id then
         --  If the parent type of a derived type is volatile
         --  then the derived type inherits volatility-related flags.

         if Is_Derived_Type (Type_Or_Obj_Id) then
            declare
               Parent_Type : constant Entity_Id :=
                 Etype (Base_Type (Type_Or_Obj_Id));
            begin
               if Is_Effectively_Volatile (Parent_Type) then
                  AR_Val := Async_Readers_Enabled (Parent_Type);
                  AW_Val := Async_Writers_Enabled (Parent_Type);
                  ER_Val := Effective_Reads_Enabled (Parent_Type);
                  EW_Val := Effective_Writes_Enabled (Parent_Type);
               end if;
            end;
         end if;
      end if;

      Prag := Get_Pragma (Type_Or_Obj_Id, Pragma_Async_Readers);

      if Present (Prag) then
         declare
            Saved_AR_Val : constant Boolean := AR_Val;
         begin
            Analyze_External_Property_In_Decl_Part (Prag, AR_Val);
            Seen := True;
            if Saved_AR_Val and not AR_Val then
               Error_Msg_N
                 ("illegal non-confirming Async_Readers specification",
                  Prag);
            end if;
         end;
      end if;

      Prag := Get_Pragma (Type_Or_Obj_Id, Pragma_Async_Writers);

      if Present (Prag) then
         declare
            Saved_AW_Val : constant Boolean := AW_Val;
         begin
            Analyze_External_Property_In_Decl_Part (Prag, AW_Val);
            Seen := True;
            if Saved_AW_Val and not AW_Val then
               Error_Msg_N
                 ("illegal non-confirming Async_Writers specification",
                  Prag);
            end if;
         end;
      end if;

      Prag := Get_Pragma (Type_Or_Obj_Id, Pragma_Effective_Reads);

      if Present (Prag) then
         declare
            Saved_ER_Val : constant Boolean := ER_Val;
         begin
            Analyze_External_Property_In_Decl_Part (Prag, ER_Val);
            Seen := True;
            if Saved_ER_Val and not ER_Val then
               Error_Msg_N
                 ("illegal non-confirming Effective_Reads specification",
                  Prag);
            end if;
         end;
      end if;

      Prag := Get_Pragma (Type_Or_Obj_Id, Pragma_Effective_Writes);

      if Present (Prag) then
         declare
            Saved_EW_Val : constant Boolean := EW_Val;
         begin
            Analyze_External_Property_In_Decl_Part (Prag, EW_Val);
            Seen := True;
            if Saved_EW_Val and not EW_Val then
               Error_Msg_N
                 ("illegal non-confirming Effective_Writes specification",
                  Prag);
            end if;
         end;
      end if;

      --  Verify the mutual interaction of the various external properties.
      --  For types and variables for which No_Caching is enabled, it has been
      --  checked already that only False values for other external properties
      --  are allowed.

      if Seen
        and then not No_Caching_Enabled (Type_Or_Obj_Id)
      then
         Check_External_Properties
           (Type_Or_Obj_Id, AR_Val, AW_Val, ER_Val, EW_Val);
      end if;

      --  Analyze the non-external volatility property No_Caching

      Prag := Get_Pragma (Type_Or_Obj_Id, Pragma_No_Caching);

      if Present (Prag) then
         Analyze_External_Property_In_Decl_Part (Prag, NC_Val);
      end if;
   end Check_Type_Or_Object_External_Properties;

   -----------------------------
   -- Analyze_Object_Contract --
   -----------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Object_Contract
     (Obj_Id    : Entity_Id;
      Freeze_Id : Entity_Id := Empty)
   is
      Obj_Typ : constant Entity_Id := Etype (Obj_Id);

      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK_Mode-related data to restore on exit

      Items    : Node_Id;
      Prag     : Node_Id;
      Ref_Elmt : Elmt_Id;

   begin
      --  The loop parameter in an element iterator over a formal container
      --  is declared with an object declaration, but no contracts apply.

      if Ekind (Obj_Id) = E_Loop_Parameter then
         return;
      end if;

      --  Do not analyze a contract multiple times

      Items := Contract (Obj_Id);

      if Present (Items) then
         if Analyzed (Items) then
            return;
         else
            Set_Analyzed (Items);
         end if;
      end if;

      --  The anonymous object created for a single concurrent type inherits
      --  the SPARK_Mode from the type. Due to the timing of contract analysis,
      --  delayed pragmas may be subject to the wrong SPARK_Mode, usually that
      --  of the enclosing context. To remedy this, restore the original mode
      --  of the related anonymous object.

      if Is_Single_Concurrent_Object (Obj_Id)
        and then Present (SPARK_Pragma (Obj_Id))
      then
         Set_SPARK_Mode (Obj_Id);
      end if;

      --  Checks related to external properties, same for constants and
      --  variables.

      Check_Type_Or_Object_External_Properties (Type_Or_Obj_Id => Obj_Id);

      --  Constant-related checks

      if Ekind (Obj_Id) = E_Constant then

         --  Analyze indicator Part_Of

         Prag := Get_Pragma (Obj_Id, Pragma_Part_Of);

         --  Check whether the lack of indicator Part_Of agrees with the
         --  placement of the constant with respect to the state space.

         if No (Prag) then
            Check_Missing_Part_Of (Obj_Id);
         end if;

      --  Variable-related checks

      else pragma Assert (Ekind (Obj_Id) = E_Variable);

         --  The anonymous object created for a single task type carries
         --  pragmas Depends and Global of the type.

         if Is_Single_Task_Object (Obj_Id) then

            --  Analyze Global first, as Depends may mention items classified
            --  in the global categorization.

            Prag := Get_Pragma (Obj_Id, Pragma_Global);

            if Present (Prag) then
               Analyze_Global_In_Decl_Part (Prag);
            end if;

            --  Depends must be analyzed after Global in order to see the modes
            --  of all global items.

            Prag := Get_Pragma (Obj_Id, Pragma_Depends);

            if Present (Prag) then
               Analyze_Depends_In_Decl_Part (Prag);
            end if;
         end if;

         Prag := Get_Pragma (Obj_Id, Pragma_Part_Of);

         --  Analyze indicator Part_Of

         if Present (Prag) then
            Analyze_Part_Of_In_Decl_Part (Prag, Freeze_Id);

            --  The variable is a constituent of a single protected/task type
            --  and behaves as a component of the type. Verify that references
            --  to the variable occur within the definition or body of the type
            --  (SPARK RM 9.3).

            if Present (Encapsulating_State (Obj_Id))
              and then Is_Single_Concurrent_Object
                         (Encapsulating_State (Obj_Id))
              and then Present (Part_Of_References (Obj_Id))
            then
               Ref_Elmt := First_Elmt (Part_Of_References (Obj_Id));
               while Present (Ref_Elmt) loop
                  Check_Part_Of_Reference (Obj_Id, Node (Ref_Elmt));
                  Next_Elmt (Ref_Elmt);
               end loop;
            end if;

         --  Otherwise check whether the lack of indicator Part_Of agrees with
         --  the placement of the variable with respect to the state space.

         else
            Check_Missing_Part_Of (Obj_Id);
         end if;
      end if;

      --  Common checks

      if Comes_From_Source (Obj_Id) and then Is_Ghost_Entity (Obj_Id) then

         --  A Ghost object cannot be of a type that yields a synchronized
         --  object (SPARK RM 6.9(19)).

         if Yields_Synchronized_Object (Obj_Typ) then
            Error_Msg_N ("ghost object & cannot be synchronized", Obj_Id);

         --  A Ghost object cannot be imported or exported (SPARK RM 6.9(7)).
         --  One exception to this is the object that represents the dispatch
         --  table of a Ghost tagged type, as the symbol needs to be exported.

         elsif Is_Exported (Obj_Id) then
            Error_Msg_N ("ghost object & cannot be exported", Obj_Id);

         elsif Is_Imported (Obj_Id) then
            Error_Msg_N ("ghost object & cannot be imported", Obj_Id);
         end if;
      end if;

      --  Restore the SPARK_Mode of the enclosing context after all delayed
      --  pragmas have been analyzed.

      Restore_SPARK_Mode (Saved_SM, Saved_SMP);
   end Analyze_Object_Contract;

   -----------------------------------
   -- Analyze_Package_Body_Contract --
   -----------------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Package_Body_Contract
     (Body_Id   : Entity_Id;
      Freeze_Id : Entity_Id := Empty)
   is
      Body_Decl : constant Node_Id   := Unit_Declaration_Node (Body_Id);
      Items     : constant Node_Id   := Contract (Body_Id);
      Spec_Id   : constant Entity_Id := Spec_Entity (Body_Id);

      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK_Mode-related data to restore on exit

      Ref_State : Node_Id;

   begin
      --  Do not analyze a contract multiple times

      if Present (Items) then
         if Analyzed (Items) then
            return;
         else
            Set_Analyzed (Items);
         end if;
      end if;

      --  Due to the timing of contract analysis, delayed pragmas may be
      --  subject to the wrong SPARK_Mode, usually that of the enclosing
      --  context. To remedy this, restore the original SPARK_Mode of the
      --  related package body.

      Set_SPARK_Mode (Body_Id);

      Ref_State := Get_Pragma (Body_Id, Pragma_Refined_State);

      --  The analysis of pragma Refined_State detects whether the spec has
      --  abstract states available for refinement.

      if Present (Ref_State) then
         Analyze_Refined_State_In_Decl_Part (Ref_State, Freeze_Id);
      end if;

      --  Restore the SPARK_Mode of the enclosing context after all delayed
      --  pragmas have been analyzed.

      Restore_SPARK_Mode (Saved_SM, Saved_SMP);

      --  Capture all global references in a generic package body now that the
      --  contract has been analyzed.

      if Is_Generic_Declaration_Or_Body (Body_Decl) then
         Save_Global_References_In_Contract
           (Templ  => Original_Node (Body_Decl),
            Gen_Id => Spec_Id);
      end if;
   end Analyze_Package_Body_Contract;

   ------------------------------
   -- Analyze_Package_Contract --
   ------------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Package_Contract (Pack_Id : Entity_Id) is
      Items     : constant Node_Id := Contract (Pack_Id);
      Pack_Decl : constant Node_Id := Unit_Declaration_Node (Pack_Id);

      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK_Mode-related data to restore on exit

      Init      : Node_Id := Empty;
      Init_Cond : Node_Id := Empty;
      Prag      : Node_Id;
      Prag_Nam  : Name_Id;

   begin
      --  Do not analyze a contract multiple times

      if Present (Items) then
         if Analyzed (Items) then
            return;

         --  Do not analyze the contract of the internal package
         --  created to check conformance of an actual package.
         --  Such an internal package is removed from the tree after
         --  legality checks are completed, and it does not contain
         --  the declarations of all local entities of the generic.

         elsif Is_Internal (Pack_Id)
           and then Is_Generic_Instance (Pack_Id)
         then
            return;

         else
            Set_Analyzed (Items);
         end if;
      end if;

      --  Due to the timing of contract analysis, delayed pragmas may be
      --  subject to the wrong SPARK_Mode, usually that of the enclosing
      --  context. To remedy this, restore the original SPARK_Mode of the
      --  related package.

      Set_SPARK_Mode (Pack_Id);

      if Present (Items) then

         --  Locate and store pragmas Initial_Condition and Initializes, since
         --  their order of analysis matters.

         Prag := Classifications (Items);
         while Present (Prag) loop
            Prag_Nam := Pragma_Name (Prag);

            if Prag_Nam = Name_Initial_Condition then
               Init_Cond := Prag;

            elsif Prag_Nam = Name_Initializes then
               Init := Prag;
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         --  Analyze the initialization-related pragmas. Initializes must come
         --  before Initial_Condition due to item dependencies.

         if Present (Init) then
            Analyze_Initializes_In_Decl_Part (Init);
         end if;

         if Present (Init_Cond) then
            Analyze_Initial_Condition_In_Decl_Part (Init_Cond);
         end if;
      end if;

      --  Restore the SPARK_Mode of the enclosing context after all delayed
      --  pragmas have been analyzed.

      Restore_SPARK_Mode (Saved_SM, Saved_SMP);

      --  Capture all global references in a generic package now that the
      --  contract has been analyzed.

      if Is_Generic_Declaration_Or_Body (Pack_Decl) then
         Save_Global_References_In_Contract
           (Templ  => Original_Node (Pack_Decl),
            Gen_Id => Pack_Id);
      end if;
   end Analyze_Package_Contract;

   --------------------------------------------
   -- Analyze_Package_Instantiation_Contract --
   --------------------------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Package_Instantiation_Contract (Inst_Id : Entity_Id) is
      Inst_Spec : constant Node_Id :=
                    Instance_Spec (Unit_Declaration_Node (Inst_Id));

      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK_Mode-related data to restore on exit

      Pack_Id : Entity_Id;
      Prag    : Node_Id;

   begin
      --  Nothing to do when the package instantiation is erroneous or left
      --  partially decorated.

      if No (Inst_Spec) then
         return;
      end if;

      Pack_Id := Defining_Entity (Inst_Spec);
      Prag    := Get_Pragma (Pack_Id, Pragma_Part_Of);

      --  Due to the timing of contract analysis, delayed pragmas may be
      --  subject to the wrong SPARK_Mode, usually that of the enclosing
      --  context. To remedy this, restore the original SPARK_Mode of the
      --  related package.

      Set_SPARK_Mode (Pack_Id);

      --  Check whether the lack of indicator Part_Of agrees with the placement
      --  of the package instantiation with respect to the state space. Nested
      --  package instantiations do not need to be checked because they inherit
      --  Part_Of indicator of the outermost package instantiation (see routine
      --  Propagate_Part_Of in Sem_Prag).

      if In_Instance then
         null;

      elsif No (Prag) then
         Check_Missing_Part_Of (Pack_Id);
      end if;

      --  Restore the SPARK_Mode of the enclosing context after all delayed
      --  pragmas have been analyzed.

      Restore_SPARK_Mode (Saved_SM, Saved_SMP);
   end Analyze_Package_Instantiation_Contract;

   --------------------------------
   -- Analyze_Protected_Contract --
   --------------------------------

   procedure Analyze_Protected_Contract (Prot_Id : Entity_Id) is
      Items : constant Node_Id := Contract (Prot_Id);

   begin
      --  Do not analyze a contract multiple times

      if Present (Items) then
         if Analyzed (Items) then
            return;
         else
            Set_Analyzed (Items);
         end if;
      end if;
   end Analyze_Protected_Contract;

   -------------------------------------------
   -- Analyze_Subprogram_Body_Stub_Contract --
   -------------------------------------------

   procedure Analyze_Subprogram_Body_Stub_Contract (Stub_Id : Entity_Id) is
      Stub_Decl : constant Node_Id   := Parent (Parent (Stub_Id));
      Spec_Id   : constant Entity_Id := Corresponding_Spec_Of_Stub (Stub_Decl);

   begin
      --  A subprogram body stub may act as its own spec or as the completion
      --  of a previous declaration. Depending on the context, the contract of
      --  the stub may contain two sets of pragmas.

      --  The stub is a completion, the applicable pragmas are:
      --    Refined_Depends
      --    Refined_Global

      if Present (Spec_Id) then
         Analyze_Entry_Or_Subprogram_Body_Contract (Stub_Id);

      --  The stub acts as its own spec, the applicable pragmas are:
      --    Always_Terminates
      --    Contract_Cases
      --    Depends
      --    Exceptional_Cases
      --    Global
      --    Postcondition
      --    Precondition
      --    Subprogram_Variant
      --    Test_Case

      else
         Analyze_Entry_Or_Subprogram_Contract (Stub_Id);
      end if;
   end Analyze_Subprogram_Body_Stub_Contract;

   ---------------------------
   -- Analyze_Task_Contract --
   ---------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Task_Contract (Task_Id : Entity_Id) is
      Items : constant Node_Id := Contract (Task_Id);

      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK_Mode-related data to restore on exit

      Prag : Node_Id;

   begin
      --  Do not analyze a contract multiple times

      if Present (Items) then
         if Analyzed (Items) then
            return;
         else
            Set_Analyzed (Items);
         end if;
      end if;

      --  Due to the timing of contract analysis, delayed pragmas may be
      --  subject to the wrong SPARK_Mode, usually that of the enclosing
      --  context. To remedy this, restore the original SPARK_Mode of the
      --  related task unit.

      Set_SPARK_Mode (Task_Id);

      --  Analyze Global first, as Depends may mention items classified in the
      --  global categorization.

      Prag := Get_Pragma (Task_Id, Pragma_Global);

      if Present (Prag) then
         Analyze_Global_In_Decl_Part (Prag);
      end if;

      --  Depends must be analyzed after Global in order to see the modes of
      --  all global items.

      Prag := Get_Pragma (Task_Id, Pragma_Depends);

      if Present (Prag) then
         Analyze_Depends_In_Decl_Part (Prag);
      end if;

      --  Restore the SPARK_Mode of the enclosing context after all delayed
      --  pragmas have been analyzed.

      Restore_SPARK_Mode (Saved_SM, Saved_SMP);
   end Analyze_Task_Contract;

   ---------------------------
   -- Analyze_Type_Contract --
   ---------------------------

   procedure Analyze_Type_Contract (Type_Id : Entity_Id) is
   begin
      Check_Type_Or_Object_External_Properties
        (Type_Or_Obj_Id => Type_Id);

      --  Analyze Pre/Post on access-to-subprogram type

      if Ekind (Type_Id) in Access_Subprogram_Kind then
         Analyze_Entry_Or_Subprogram_Contract
           (Directly_Designated_Type (Type_Id));
      end if;
   end Analyze_Type_Contract;

   ---------------------------------------
   -- Build_Subprogram_Contract_Wrapper --
   ---------------------------------------

   procedure Build_Subprogram_Contract_Wrapper
     (Body_Id : Entity_Id;
      Stmts   : List_Id;
      Decls   : List_Id;
      Result  : Entity_Id)
   is
      Body_Decl : constant Entity_Id  := Unit_Declaration_Node (Body_Id);
      Loc       : constant Source_Ptr := Sloc (Body_Decl);
      Spec_Id   : constant Entity_Id  := Corresponding_Spec (Body_Decl);
      Subp_Id   : Entity_Id;
      Ret_Type  : Entity_Id;

      Wrapper_Id   : Entity_Id;
      Wrapper_Body : Node_Id;
      Wrapper_Spec : Node_Id;

   begin
      --  When there are no postcondition statements we do not need to
      --  generate a wrapper.

      if No (Stmts) then
         return;
      end if;

      --  Obtain the related subprogram id from the body id.

      if Present (Spec_Id) then
         Subp_Id := Spec_Id;
      else
         Subp_Id := Body_Id;
      end if;
      Ret_Type := Etype (Subp_Id);

      --  Generate the contracts wrapper by moving the original declarations
      --  and statements within a local subprogram, calling it and possibly
      --  preserving the result for the purpose of evaluating postconditions,
      --  contracts, type invariants, etc.

      --  In the case of a function, generate:
      --
      --  function Original_Func (X : in out Integer) return Typ is
      --     <prologue renamings>
      --     <preconditions>
      --
      --     function _Wrapped_Statements return Typ is
      --        <original declarations>
      --     begin
      --        <original statements>
      --     end;
      --
      --  begin
      --     return
      --        Result_Obj : constant Typ := _Wrapped_Statements
      --     do
      --        <postconditions statements>
      --     end return;
      --  end;

      --  Or else, in the case of a procedure, generate:
      --
      --  procedure Original_Proc (X : in out Integer) is
      --     <prologue renamings>
      --     <preconditions>
      --
      --     procedure _Wrapped_Statements is
      --        <original declarations>
      --     begin
      --        <original statements>
      --     end;
      --
      --  begin
      --     _Wrapped_Statements;
      --     <postconditions statements>
      --  end;

      --  Create Identifier

      Wrapper_Id := Make_Defining_Identifier (Loc, Name_uWrapped_Statements);
      Set_Debug_Info_Needed  (Wrapper_Id);
      Set_Wrapped_Statements (Subp_Id, Wrapper_Id);

      Set_Has_Pragma_Inline (Wrapper_Id, Has_Pragma_Inline (Subp_Id));
      Set_Has_Pragma_Inline_Always
        (Wrapper_Id, Has_Pragma_Inline_Always (Subp_Id));

      --  Create specification and declaration for the wrapper

      if No (Ret_Type) or else Ret_Type = Standard_Void_Type then
         Wrapper_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name => Wrapper_Id);
      else
         Wrapper_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name => Wrapper_Id,
             Result_Definition  => New_Occurrence_Of (Ret_Type, Loc));
      end if;

      --  Create the wrapper body using Body_Id's statements and declarations

      Wrapper_Body :=
        Make_Subprogram_Body (Loc,
          Specification              => Wrapper_Spec,
          Declarations               => Declarations (Body_Decl),
          Handled_Statement_Sequence =>
            Relocate_Node (Handled_Statement_Sequence (Body_Decl)));

      Append_To (Decls, Wrapper_Body);
      Set_Declarations (Body_Decl, Decls);
      Set_Handled_Statement_Sequence (Body_Decl,
        Make_Handled_Sequence_Of_Statements (Loc,
          End_Label  => Make_Identifier (Loc, Chars (Wrapper_Id))));

      --  Prepend a call to the wrapper when the subprogram is a procedure

      if No (Ret_Type) or else Ret_Type = Standard_Void_Type then
         Prepend_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Wrapper_Id, Loc)));
         Set_Statements
           (Handled_Statement_Sequence (Body_Decl), Stmts);

      --  Generate the post-execution statements and the extended return
      --  when the subprogram being wrapped is a function.

      else
         Set_Statements (Handled_Statement_Sequence (Body_Decl), New_List (
           Make_Extended_Return_Statement (Loc,
             Return_Object_Declarations => New_List (
                Make_Object_Declaration (Loc,
                  Defining_Identifier => Result,
                  Constant_Present    => True,
                  Object_Definition   =>
                    New_Occurrence_Of (Ret_Type, Loc),
                  Expression          =>
                    Make_Function_Call (Loc,
                      Name                   =>
                        New_Occurrence_Of (Wrapper_Id, Loc)))),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts))));
      end if;
   end Build_Subprogram_Contract_Wrapper;

   ----------------------------------
   -- Build_Entry_Contract_Wrapper --
   ----------------------------------

   procedure Build_Entry_Contract_Wrapper (E : Entity_Id; Decl : Node_Id) is
      Conc_Typ : constant Entity_Id  := Scope (E);
      Loc      : constant Source_Ptr := Sloc (E);

      procedure Add_Discriminant_Renamings
        (Obj_Id : Entity_Id;
         Decls  : List_Id);
      --  Add renaming declarations for all discriminants of concurrent type
      --  Conc_Typ. Obj_Id is the entity of the wrapper formal parameter which
      --  represents the concurrent object.

      procedure Add_Matching_Formals
        (Formals : List_Id;
         Actuals : in out List_Id);
      --  Add formal parameters that match those of entry E to list Formals.
      --  The routine also adds matching actuals for the new formals to list
      --  Actuals.

      procedure Transfer_Pragma (Prag : Node_Id; To : in out List_Id);
      --  Relocate pragma Prag to list To. The routine creates a new list if
      --  To does not exist.

      --------------------------------
      -- Add_Discriminant_Renamings --
      --------------------------------

      procedure Add_Discriminant_Renamings
        (Obj_Id : Entity_Id;
         Decls  : List_Id)
      is
         Discr         : Entity_Id;
         Renaming_Decl : Node_Id;

      begin
         --  Inspect the discriminants of the concurrent type and generate a
         --  renaming for each one.

         if Has_Discriminants (Conc_Typ) then
            Discr := First_Discriminant (Conc_Typ);
            while Present (Discr) loop
               Renaming_Decl :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Chars (Discr)),
                   Subtype_Mark        =>
                     New_Occurrence_Of (Etype (Discr), Loc),
                   Name                =>
                     Make_Selected_Component (Loc,
                       Prefix        => New_Occurrence_Of (Obj_Id, Loc),
                       Selector_Name =>
                         Make_Identifier (Loc, Chars (Discr))));

               Prepend_To (Decls, Renaming_Decl);

               Next_Discriminant (Discr);
            end loop;
         end if;
      end Add_Discriminant_Renamings;

      --------------------------
      -- Add_Matching_Formals --
      --------------------------

      procedure Add_Matching_Formals
        (Formals : List_Id;
         Actuals : in out List_Id)
      is
         Formal     : Entity_Id;
         New_Formal : Entity_Id;

      begin
         --  Inspect the formal parameters of the entry and generate a new
         --  matching formal with the same name for the wrapper. A reference
         --  to the new formal becomes an actual in the entry call.

         Formal := First_Formal (E);
         while Present (Formal) loop
            New_Formal := Make_Defining_Identifier (Loc, Chars (Formal));
            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => New_Formal,
                In_Present          => In_Present  (Parent (Formal)),
                Out_Present         => Out_Present (Parent (Formal)),
                Parameter_Type      =>
                  New_Occurrence_Of (Etype (Formal), Loc)));

            if No (Actuals) then
               Actuals := New_List;
            end if;

            Append_To (Actuals, New_Occurrence_Of (New_Formal, Loc));
            Next_Formal (Formal);
         end loop;
      end Add_Matching_Formals;

      ---------------------
      -- Transfer_Pragma --
      ---------------------

      procedure Transfer_Pragma (Prag : Node_Id; To : in out List_Id) is
         New_Prag : Node_Id;

      begin
         if No (To) then
            To := New_List;
         end if;

         New_Prag := Relocate_Node (Prag);

         Set_Analyzed (New_Prag, False);
         Append       (New_Prag, To);
      end Transfer_Pragma;

      --  Local variables

      Items      : constant Node_Id := Contract (E);
      Actuals    : List_Id := No_List;
      Call       : Node_Id;
      Call_Nam   : Node_Id;
      Decls      : List_Id := No_List;
      Formals    : List_Id;
      Has_Pragma : Boolean := False;
      Index_Id   : Entity_Id;
      Obj_Id     : Entity_Id;
      Prag       : Node_Id;
      Wrapper_Id : Entity_Id;

   --  Start of processing for Build_Entry_Contract_Wrapper

   begin
      --  This routine generates a specialized wrapper for a protected or task
      --  entry [family] which implements precondition/postcondition semantics.
      --  Preconditions and case guards of contract cases are checked before
      --  the protected action or rendezvous takes place.

      --    procedure Wrapper
      --      (Obj_Id    : Conc_Typ;    --  concurrent object
      --       [Index    : Index_Typ;]  --  index of entry family
      --       [Formal_1 : ...;         --  parameters of original entry
      --        Formal_N : ...])
      --    is
      --       [Discr_1 : ... renames Obj_Id.Discr_1;   --  discriminant
      --        Discr_N : ... renames Obj_Id.Discr_N;]  --  renamings

      --       <contracts pragmas>
      --       <case guard checks>

      --    begin
      --       Entry_Call (Obj_Id, [Index,] [Formal_1, Formal_N]);
      --    end Wrapper;

      --  Create the wrapper only when the entry has at least one executable
      --  contract item such as contract cases, precondition or postcondition.

      if Present (Items) then

         --  Inspect the list of pre/postconditions and transfer all available
         --  pragmas to the declarative list of the wrapper.

         Prag := Pre_Post_Conditions (Items);
         while Present (Prag) loop
            if Pragma_Name_Unmapped (Prag) in Name_Postcondition
                                            | Name_Precondition
              and then Is_Checked (Prag)
            then
               Has_Pragma := True;
               Transfer_Pragma (Prag, To => Decls);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         --  Inspect the list of test/contract cases and transfer only contract
         --  cases pragmas to the declarative part of the wrapper.

         Prag := Contract_Test_Cases (Items);
         while Present (Prag) loop
            if Pragma_Name (Prag) = Name_Contract_Cases
              and then Is_Checked (Prag)
            then
               Has_Pragma := True;
               Transfer_Pragma (Prag, To => Decls);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;
      end if;

      --  The entry lacks executable contract items and a wrapper is not needed

      if not Has_Pragma then
         return;
      end if;

      --  Create the profile of the wrapper. The first formal parameter is the
      --  concurrent object.

      Obj_Id :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (Conc_Typ), 'A'));

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Obj_Id,
          Out_Present         => True,
          In_Present          => True,
          Parameter_Type      => New_Occurrence_Of (Conc_Typ, Loc)));

      --  Construct the call to the original entry. The call will be gradually
      --  augmented with an optional entry index and extra parameters.

      Call_Nam :=
        Make_Selected_Component (Loc,
          Prefix        => New_Occurrence_Of (Obj_Id, Loc),
          Selector_Name => New_Occurrence_Of (E, Loc));

      --  When creating a wrapper for an entry family, the second formal is the
      --  entry index.

      if Ekind (E) = E_Entry_Family then
         Index_Id := Make_Defining_Identifier (Loc, Name_I);

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Index_Id,
             Parameter_Type      =>
               New_Occurrence_Of (Entry_Index_Type (E), Loc)));

         --  The call to the original entry becomes an indexed component to
         --  accommodate the entry index.

         Call_Nam :=
           Make_Indexed_Component (Loc,
             Prefix      => Call_Nam,
             Expressions => New_List (New_Occurrence_Of (Index_Id, Loc)));
      end if;

      --  Add formal parameters to match those of the entry and build actuals
      --  for the entry call.

      Add_Matching_Formals (Formals, Actuals);

      Call :=
        Make_Procedure_Call_Statement (Loc,
          Name                   => Call_Nam,
          Parameter_Associations => Actuals);

      --  Add renaming declarations for the discriminants of the enclosing type
      --  as the various contract items may reference them.

      Add_Discriminant_Renamings (Obj_Id, Decls);

      Wrapper_Id :=
        Make_Defining_Identifier (Loc, New_External_Name (Chars (E), 'E'));
      Set_Contract_Wrapper (E, Wrapper_Id);
      Set_Is_Entry_Wrapper (Wrapper_Id);

      --  The wrapper body is analyzed when the enclosing type is frozen

      Append_Freeze_Action (Defining_Entity (Decl),
        Make_Subprogram_Body (Loc,
          Specification              =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Wrapper_Id,
              Parameter_Specifications => Formals),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (Call))));
   end Build_Entry_Contract_Wrapper;

   ---------------------------
   -- Check_Class_Condition --
   ---------------------------

   procedure Check_Class_Condition
     (Cond            : Node_Id;
      Subp            : Entity_Id;
      Par_Subp        : Entity_Id;
      Is_Precondition : Boolean)
   is
      function Check_Entity (N : Node_Id) return Traverse_Result;
      --  Check reference to formal of inherited operation or to primitive
      --  operation of root type.

      ------------------
      -- Check_Entity --
      ------------------

      function Check_Entity (N : Node_Id) return Traverse_Result is
         New_E  : Entity_Id;
         Orig_E : Entity_Id;

      begin
         if Nkind (N) = N_Identifier
           and then Present (Entity (N))
           and then
             (Is_Formal (Entity (N)) or else Is_Subprogram (Entity (N)))
           and then
             (Nkind (Parent (N)) /= N_Attribute_Reference
               or else Attribute_Name (Parent (N)) /= Name_Class)
         then
            --  These checks do not apply to dispatching calls within the
            --  condition, but only to calls whose static tag is that of
            --  the parent type.

            if Is_Subprogram (Entity (N))
              and then Nkind (Parent (N)) = N_Function_Call
              and then Present (Controlling_Argument (Parent (N)))
            then
               return OK;
            end if;

            --  Determine whether entity has a renaming

            Orig_E := Entity (N);
            New_E  := Get_Mapped_Entity (Orig_E);

            if Present (New_E) then

               --  AI12-0166: A precondition for a protected operation
               --  cannot include an internal call to a protected function
               --  of the type. In the case of an inherited condition for an
               --  overriding operation, both the operation and the function
               --  are given by primitive wrappers.

               if Is_Precondition
                 and then Ekind (New_E) = E_Function
                 and then Is_Primitive_Wrapper (New_E)
                 and then Is_Primitive_Wrapper (Subp)
                 and then Scope (Subp) = Scope (New_E)
               then
                  Error_Msg_Node_2 := Wrapped_Entity (Subp);
                  Error_Msg_NE
                    ("internal call to& cannot appear in inherited "
                     & "precondition of protected operation&",
                     Subp, Wrapped_Entity (New_E));
               end if;
            end if;

            --  Check that there are no calls left to abstract operations if
            --  the current subprogram is not abstract.

            if Present (New_E)
              and then Nkind (Parent (N)) = N_Function_Call
              and then N = Name (Parent (N))
            then
               if not Is_Abstract_Subprogram (Subp)
                 and then Is_Abstract_Subprogram (New_E)
               then
                  Error_Msg_Sloc   := Sloc (Current_Scope);
                  Error_Msg_Node_2 := Subp;

                  if Comes_From_Source (Subp) then
                     Error_Msg_NE
                       ("cannot call abstract subprogram & in inherited "
                        & "condition for&#", Subp, New_E);
                  else
                     Error_Msg_NE
                       ("cannot call abstract subprogram & in inherited "
                        & "condition for inherited&#", Subp, New_E);
                  end if;

               --  In SPARK mode, report error on inherited condition for an
               --  inherited operation if it contains a call to an overriding
               --  operation, because this implies that the pre/postconditions
               --  of the inherited operation have changed silently.

               elsif SPARK_Mode = On
                 and then Warn_On_Suspicious_Contract
                 and then Present (Alias (Subp))
                 and then Present (New_E)
                 and then Comes_From_Source (New_E)
               then
                  Error_Msg_N
                    ("cannot modify inherited condition (SPARK RM 6.1.1(1))",
                     Parent (Subp));
                  Error_Msg_Sloc   := Sloc (New_E);
                  Error_Msg_Node_2 := Subp;
                  Error_Msg_NE
                    ("\overriding of&# forces overriding of&",
                     Parent (Subp), New_E);
               end if;
            end if;
         end if;

         return OK;
      end Check_Entity;

      procedure Check_Condition_Entities is
        new Traverse_Proc (Check_Entity);

   --  Start of processing for Check_Class_Condition

   begin
      --  No check required if the subprograms match

      if Par_Subp = Subp then
         return;
      end if;

      Update_Primitives_Mapping (Par_Subp, Subp);
      Map_Formals (Par_Subp, Subp);
      Check_Condition_Entities (Cond);
   end Check_Class_Condition;

   -----------------------------
   -- Create_Generic_Contract --
   -----------------------------

   procedure Create_Generic_Contract (Unit : Node_Id) is
      Templ    : constant Node_Id   := Original_Node (Unit);
      Templ_Id : constant Entity_Id := Defining_Entity (Templ);

      procedure Add_Generic_Contract_Pragma (Prag : Node_Id);
      --  Add a single contract-related source pragma Prag to the contract of
      --  generic template Templ_Id.

      ---------------------------------
      -- Add_Generic_Contract_Pragma --
      ---------------------------------

      procedure Add_Generic_Contract_Pragma (Prag : Node_Id) is
         Prag_Templ : Node_Id;

      begin
         --  Mark the pragma to prevent the premature capture of global
         --  references when capturing global references of the context
         --  (see Save_References_In_Pragma).

         Set_Is_Generic_Contract_Pragma (Prag);

         --  Pragmas that apply to a generic subprogram declaration are not
         --  part of the semantic structure of the generic template:

         --    generic
         --    procedure Example (Formal : Integer);
         --    pragma Precondition (Formal > 0);

         --  Create a generic template for such pragmas and link the template
         --  of the pragma with the generic template.

         if Nkind (Templ) = N_Generic_Subprogram_Declaration then
            Rewrite
              (Prag, Copy_Generic_Node (Prag, Empty, Instantiating => False));
            Prag_Templ := Original_Node (Prag);

            Set_Is_Generic_Contract_Pragma (Prag_Templ);
            Add_Contract_Item (Prag_Templ, Templ_Id);

         --  Otherwise link the pragma with the generic template

         else
            Add_Contract_Item (Prag, Templ_Id);
         end if;

      exception
         --  We do not stop the compilation at this point in the case of an
         --  invalid pragma because it will be properly diagnosed afterward.

         when Contract_Error => null;
      end Add_Generic_Contract_Pragma;

      --  Local variables

      Context : constant Node_Id   := Parent (Unit);
      Decl    : Node_Id := Empty;

   --  Start of processing for Create_Generic_Contract

   begin
      --  A generic package declaration carries contract-related source pragmas
      --  in its visible declarations.

      if Nkind (Templ) = N_Generic_Package_Declaration then
         Mutate_Ekind (Templ_Id, E_Generic_Package);

         if Present (Visible_Declarations (Specification (Templ))) then
            Decl := First (Visible_Declarations (Specification (Templ)));
         end if;

      --  A generic package body carries contract-related source pragmas in its
      --  declarations.

      elsif Nkind (Templ) = N_Package_Body then
         Mutate_Ekind (Templ_Id, E_Package_Body);

         if Present (Declarations (Templ)) then
            Decl := First (Declarations (Templ));
         end if;

      --  Generic subprogram declaration

      elsif Nkind (Templ) = N_Generic_Subprogram_Declaration then
         if Nkind (Specification (Templ)) = N_Function_Specification then
            Mutate_Ekind (Templ_Id, E_Generic_Function);
         else
            Mutate_Ekind (Templ_Id, E_Generic_Procedure);
         end if;

         --  When the generic subprogram acts as a compilation unit, inspect
         --  the Pragmas_After list for contract-related source pragmas.

         if Nkind (Context) = N_Compilation_Unit then
            if Present (Aux_Decls_Node (Context))
              and then Present (Pragmas_After (Aux_Decls_Node (Context)))
            then
               Decl := First (Pragmas_After (Aux_Decls_Node (Context)));
            end if;

         --  Otherwise inspect the successive declarations for contract-related
         --  source pragmas.

         else
            Decl := Next (Unit);
         end if;

      --  A generic subprogram body carries contract-related source pragmas in
      --  its declarations.

      elsif Nkind (Templ) = N_Subprogram_Body then
         Mutate_Ekind (Templ_Id, E_Subprogram_Body);

         if Present (Declarations (Templ)) then
            Decl := First (Declarations (Templ));
         end if;
      end if;

      --  Inspect the relevant declarations looking for contract-related source
      --  pragmas and add them to the contract of the generic unit.

      while Present (Decl) loop
         if Comes_From_Source (Decl) then
            if Nkind (Decl) = N_Pragma then

               --  The source pragma is a contract annotation

               if Is_Contract_Annotation (Decl) then
                  Add_Generic_Contract_Pragma (Decl);
               end if;

            --  The region where a contract-related source pragma may appear
            --  ends with the first source non-pragma declaration or statement.

            else
               exit;
            end if;
         end if;

         Next (Decl);
      end loop;
   end Create_Generic_Contract;

   --------------------------------
   -- Expand_Subprogram_Contract --
   --------------------------------

   procedure Expand_Subprogram_Contract (Body_Id : Entity_Id) is
      Body_Decl : constant Node_Id   := Unit_Declaration_Node (Body_Id);
      Spec_Id   : constant Entity_Id := Corresponding_Spec (Body_Decl);

      procedure Add_Invariant_And_Predicate_Checks
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id;
         Result  : out Node_Id);
      --  Process the result of function Subp_Id (if applicable) and all its
      --  formals. Add invariant and predicate checks where applicable. The
      --  routine appends all the checks to list Stmts. If Subp_Id denotes a
      --  function, Result contains the entity of parameter _Result, to be
      --  used in the creation of procedure _Postconditions.

      procedure Add_Stable_Property_Contracts
        (Subp_Id : Entity_Id; Class_Present : Boolean);
      --  Augment postcondition contracts to reflect Stable_Property aspect
      --  (if Class_Present = False) or Stable_Property'Class aspect (if
      --  Class_Present = True).

      procedure Append_Enabled_Item (Item : Node_Id; List : in out List_Id);
      --  Append a node to a list. If there is no list, create a new one. When
      --  the item denotes a pragma, it is added to the list only when it is
      --  enabled.

      procedure Process_Contract_Cases
        (Stmts : in out List_Id;
         Decls : List_Id);
      --  Process pragma Contract_Cases. This routine prepends items to the
      --  body declarations and appends items to list Stmts.

      procedure Process_Postconditions (Stmts : in out List_Id);
      --  Collect all [inherited] spec and body postconditions and accumulate
      --  their pragma Check equivalents in list Stmts.

      procedure Process_Preconditions (Decls : in out List_Id);
      --  Collect all [inherited] spec and body preconditions and prepend their
      --  pragma Check equivalents to the declarations of the body.

      ----------------------------------------
      -- Add_Invariant_And_Predicate_Checks --
      ----------------------------------------

      procedure Add_Invariant_And_Predicate_Checks
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id;
         Result  : out Node_Id)
      is
         procedure Add_Invariant_Access_Checks (Id : Entity_Id);
         --  Id denotes the return value of a function or a formal parameter.
         --  Add an invariant check if the type of Id is access to a type with
         --  invariants. The routine appends the generated code to Stmts.

         function Invariant_Checks_OK (Typ : Entity_Id) return Boolean;
         --  Determine whether type Typ can benefit from invariant checks. To
         --  qualify, the type must have a non-null invariant procedure and
         --  subprogram Subp_Id must appear visible from the point of view of
         --  the type.

         ---------------------------------
         -- Add_Invariant_Access_Checks --
         ---------------------------------

         procedure Add_Invariant_Access_Checks (Id : Entity_Id) is
            Loc : constant Source_Ptr := Sloc (Body_Decl);
            Ref : Node_Id;
            Typ : Entity_Id;

         begin
            Typ := Etype (Id);

            if Is_Access_Type (Typ) and then not Is_Access_Constant (Typ) then
               Typ := Designated_Type (Typ);

               if Invariant_Checks_OK (Typ) then
                  Ref :=
                    Make_Explicit_Dereference (Loc,
                      Prefix => New_Occurrence_Of (Id, Loc));
                  Set_Etype (Ref, Typ);

                  --  Generate:
                  --    if <Id> /= null then
                  --       <invariant_call (<Ref>)>
                  --    end if;

                  Append_Enabled_Item
                    (Item =>
                       Make_If_Statement (Loc,
                         Condition =>
                           Make_Op_Ne (Loc,
                             Left_Opnd  => New_Occurrence_Of (Id, Loc),
                             Right_Opnd => Make_Null (Loc)),
                         Then_Statements => New_List (
                           Make_Invariant_Call (Ref))),
                     List => Stmts);
               end if;
            end if;
         end Add_Invariant_Access_Checks;

         -------------------------
         -- Invariant_Checks_OK --
         -------------------------

         function Invariant_Checks_OK (Typ : Entity_Id) return Boolean is
            function Has_Public_Visibility_Of_Subprogram return Boolean;
            --  Determine whether type Typ has public visibility of subprogram
            --  Subp_Id.

            -----------------------------------------
            -- Has_Public_Visibility_Of_Subprogram --
            -----------------------------------------

            function Has_Public_Visibility_Of_Subprogram return Boolean is
               Subp_Decl : constant Node_Id := Unit_Declaration_Node (Subp_Id);

            begin
               --  An Initialization procedure must be considered visible even
               --  though it is internally generated.

               if Is_Init_Proc (Defining_Entity (Subp_Decl)) then
                  return True;

               elsif Ekind (Scope (Typ)) /= E_Package then
                  return False;

               --  Internally generated code is never publicly visible except
               --  for a subprogram that is the implementation of an expression
               --  function. In that case the visibility is determined by the
               --  last check.

               elsif not Comes_From_Source (Subp_Decl)
                 and then
                   (Nkind (Original_Node (Subp_Decl)) /= N_Expression_Function
                      or else not
                        Comes_From_Source (Defining_Entity (Subp_Decl)))
               then
                  return False;

               --  Determine whether the subprogram is declared in the visible
               --  declarations of the package containing the type, or in the
               --  visible declaration of a child unit of that package.

               elsif Is_List_Member (Subp_Decl) then
                  declare
                     Decls      : constant List_Id   :=
                                    List_Containing (Subp_Decl);
                     Subp_Scope : constant Entity_Id :=
                                    Scope (Defining_Entity (Subp_Decl));
                     Typ_Scope  : constant Entity_Id := Scope (Typ);

                  begin
                     return
                       Decls = Visible_Declarations
                           (Specification (Unit_Declaration_Node (Typ_Scope)))

                         or else
                           (Ekind (Subp_Scope) = E_Package
                             and then Typ_Scope /= Subp_Scope
                             and then Is_Child_Unit (Subp_Scope)
                             and then
                               Is_Ancestor_Package (Typ_Scope, Subp_Scope)
                             and then
                               Decls = Visible_Declarations
                                 (Specification
                                   (Unit_Declaration_Node (Subp_Scope))));
                  end;

               --  Determine whether the subprogram is a child subprogram of
               --  of the package containing the type.

               else
                  pragma Assert
                    (Nkind (Parent (Subp_Decl)) = N_Compilation_Unit);

                  declare
                     Subp_Scope : constant Entity_Id :=
                                    Scope (Defining_Entity (Subp_Decl));
                     Typ_Scope  : constant Entity_Id := Scope (Typ);

                  begin
                     return
                       Ekind (Subp_Scope) = E_Package
                         and then
                           (Typ_Scope = Subp_Scope
                              or else
                                (Is_Child_Unit (Subp_Scope)
                                   and then Is_Ancestor_Package
                                              (Typ_Scope, Subp_Scope)));
                  end;
               end if;
            end Has_Public_Visibility_Of_Subprogram;

         --  Start of processing for Invariant_Checks_OK

         begin
            return
              Has_Invariants (Typ)
                and then Present (Invariant_Procedure (Typ))
                and then not Has_Null_Body (Invariant_Procedure (Typ))
                and then Has_Public_Visibility_Of_Subprogram;
         end Invariant_Checks_OK;

         --  Local variables

         Loc : constant Source_Ptr := Sloc (Body_Decl);
         --  Source location of subprogram body contract

         Formal : Entity_Id;
         Typ    : Entity_Id;

      --  Start of processing for Add_Invariant_And_Predicate_Checks

      begin
         Result := Empty;

         --  Process the result of a function

         if Ekind (Subp_Id) = E_Function then
            Typ := Etype (Subp_Id);

            --  Generate _Result which is used in procedure _Postconditions to
            --  verify the return value.

            Result := Make_Defining_Identifier (Loc, Name_uResult);
            Set_Etype (Result, Typ);

            --  Add an invariant check when the return type has invariants and
            --  the related function is visible to the outside.

            if Invariant_Checks_OK (Typ) then
               Append_Enabled_Item
                 (Item =>
                    Make_Invariant_Call (New_Occurrence_Of (Result, Loc)),
                  List => Stmts);
            end if;

            --  Add an invariant check when the return type is an access to a
            --  type with invariants.

            Add_Invariant_Access_Checks (Result);
         end if;

         --  Add invariant checks for all formals that qualify (see AI05-0289
         --  and AI12-0044).

         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Typ := Etype (Formal);

            if Ekind (Formal) /= E_In_Parameter
              or else Ekind (Subp_Id) = E_Procedure
              or else Is_Access_Type (Typ)
            then
               if Invariant_Checks_OK (Typ) then
                  Append_Enabled_Item
                    (Item =>
                       Make_Invariant_Call (New_Occurrence_Of (Formal, Loc)),
                     List => Stmts);
               end if;

               Add_Invariant_Access_Checks (Formal);

               --  Note: we used to add predicate checks for OUT and IN OUT
               --  formals here, but that was misguided, since such checks are
               --  performed on the caller side, based on the predicate of the
               --  actual, rather than the predicate of the formal.

            end if;

            Next_Formal (Formal);
         end loop;
      end Add_Invariant_And_Predicate_Checks;

      -----------------------------------
      -- Add_Stable_Property_Contracts --
      -----------------------------------

      procedure Add_Stable_Property_Contracts
        (Subp_Id : Entity_Id; Class_Present : Boolean)
      is
         Loc : constant Source_Ptr := Sloc (Subp_Id);

         procedure Insert_Stable_Property_Check
           (Formal : Entity_Id; Property_Function : Entity_Id);
         --  Build the pragma for one check and insert it in the tree.

         function Make_Stable_Property_Condition
           (Formal : Entity_Id; Property_Function : Entity_Id) return Node_Id;
         --  Builds tree for "Func (Formal) = Func (Formal)'Old" expression.

         function Stable_Properties
           (Aspect_Bearer : Entity_Id; Negated : out Boolean)
           return Subprogram_List;
         --  If no aspect specified, then returns length-zero result.
         --  Negated indicates that reserved word NOT was specified.

         ----------------------------------
         -- Insert_Stable_Property_Check --
         ----------------------------------

         procedure Insert_Stable_Property_Check
           (Formal : Entity_Id; Property_Function : Entity_Id) is

            Args : constant List_Id :=
              New_List
                (Make_Pragma_Argument_Association
                   (Sloc => Loc,
                    Expression =>
                      Make_Stable_Property_Condition
                         (Formal            => Formal,
                          Property_Function => Property_Function)),
                 Make_Pragma_Argument_Association
                   (Sloc => Loc,
                    Expression =>
                      Make_String_Literal
                        (Sloc => Loc,
                         Strval =>
                           "failed stable property check at "
                           & Build_Location_String (Loc)
                           & " for parameter "
                           & To_String (Fully_Qualified_Name_String
                               (Formal, Append_NUL => False))
                           & " and property function "
                           & To_String (Fully_Qualified_Name_String
                               (Property_Function, Append_NUL => False))
                        )));

            Prag : constant Node_Id :=
              Make_Pragma (Loc,
                Pragma_Identifier            =>
                  Make_Identifier (Loc, Name_Postcondition),
                Pragma_Argument_Associations => Args,
                Class_Present                => Class_Present);

            Subp_Decl : constant Node_Id := Enclosing_Declaration (Subp_Id);
            pragma Assert (Is_Declaration (Subp_Decl));
         begin
            Insert_After_And_Analyze (Subp_Decl, Prag);
         end Insert_Stable_Property_Check;

         ------------------------------------
         -- Make_Stable_Property_Condition --
         ------------------------------------

         function Make_Stable_Property_Condition
           (Formal : Entity_Id; Property_Function : Entity_Id) return Node_Id
         is
            function Call_Property_Function return Node_Id is
              (Make_Function_Call
                 (Loc,
                  Name                   =>
                    New_Occurrence_Of (Property_Function, Loc),
                  Parameter_Associations =>
                    New_List (New_Occurrence_Of (Formal, Loc))));
         begin
            return Make_Op_Eq
              (Loc,
               Call_Property_Function,
               Make_Attribute_Reference
                 (Loc,
                  Prefix         => Call_Property_Function,
                  Attribute_Name => Name_Old));
         end Make_Stable_Property_Condition;

         -----------------------
         -- Stable_Properties --
         -----------------------

         function Stable_Properties
           (Aspect_Bearer : Entity_Id; Negated : out Boolean)
           return Subprogram_List
         is
            Aspect_Spec : Node_Id :=
              Find_Value_Of_Aspect
                (Aspect_Bearer, Aspect_Stable_Properties,
                 Class_Present => Class_Present);
         begin
            --  ??? For a derived type, we wish Find_Value_Of_Aspect
            --  somehow knew that this aspect is not inherited.
            --  But it doesn't, so we cope with that here.
            --
            --  There are probably issues here with inheritance from
            --  interface types, where just looking for the one parent type
            --  isn't enough. But this is far from the only work needed for
            --  Stable_Properties'Class for interface types.

            if Is_Derived_Type (Aspect_Bearer) then
               declare
                  Parent_Type : constant Entity_Id :=
                    Etype (Base_Type (Aspect_Bearer));
               begin
                  if Aspect_Spec =
                     Find_Value_Of_Aspect
                       (Parent_Type, Aspect_Stable_Properties,
                        Class_Present => Class_Present)
                  then
                     --  prevent inheritance
                     Aspect_Spec := Empty;
                  end if;
               end;
            end if;

            if No (Aspect_Spec) then
               Negated := Aspect_Bearer = Subp_Id;
               --  This is a little bit subtle.
               --  We need to assign True in the Subp_Id case in order to
               --  distinguish between no aspect spec at all vs. an
               --  explicitly specified "with S_P => []" empty list.
               --  In both cases Stable_Properties will return a length-0
               --  array, but the two cases are not equivalent.
               --  Very roughly speaking the lack of an S_P aspect spec for
               --  a subprogram would be equivalent to something like
               --  "with S_P => [not]", where we apply the "not" modifier to
               --  an empty set of subprograms, if such a construct existed.
               --  We could just assign True here, but it seems untidy to
               --  return True in the case of an aspect spec for a type
               --  (since negation is only allowed for subp S_P aspects).

               return (1 .. 0 => <>);
            else
               return Parse_Aspect_Stable_Properties
                        (Aspect_Spec, Negated => Negated);
            end if;
         end Stable_Properties;

         Formal                  : Entity_Id := First_Formal (Subp_Id);
         Type_Of_Formal          : Entity_Id;

         Subp_Properties_Negated : Boolean;
         Subp_Properties         : constant Subprogram_List :=
           Stable_Properties (Subp_Id, Subp_Properties_Negated);

         --  start of processing for Add_Stable_Property_Contracts

      begin
         if not (Is_Primitive (Subp_Id) and then Comes_From_Source (Subp_Id))
         then
            return;
         end if;

         while Present (Formal) loop
            Type_Of_Formal := Base_Type (Etype (Formal));

            if not Subp_Properties_Negated then

               for SPF_Id of Subp_Properties loop
                  if Type_Of_Formal = Base_Type (Etype (First_Formal (SPF_Id)))
                     and then Scope (Type_Of_Formal) = Scope (Subp_Id)
                  then
                     --  ??? Need to filter out checks for SPFs that are
                     --  mentioned explicitly in the postcondition of
                     --  Subp_Id.

                     Insert_Stable_Property_Check
                       (Formal => Formal, Property_Function => SPF_Id);
                  end if;
               end loop;

            elsif Scope (Type_Of_Formal) = Scope (Subp_Id) then
               declare
                  Ignored : Boolean range False .. False;

                  Typ_Property_Funcs : constant Subprogram_List :=
                     Stable_Properties (Type_Of_Formal, Negated => Ignored);

                  function Excluded_By_Aspect_Spec_Of_Subp
                    (SPF_Id : Entity_Id) return Boolean;
                  --  Examine Subp_Properties to determine whether SPF should
                  --  be excluded.

                  -------------------------------------
                  -- Excluded_By_Aspect_Spec_Of_Subp --
                  -------------------------------------

                  function Excluded_By_Aspect_Spec_Of_Subp
                    (SPF_Id : Entity_Id) return Boolean is
                  begin
                     pragma Assert (Subp_Properties_Negated);
                     --  Look through renames for equality test here ???
                     return  (for some F of Subp_Properties => F = SPF_Id);
                  end Excluded_By_Aspect_Spec_Of_Subp;

                  --  Look through renames for equality test here ???
                  Subp_Is_Stable_Property_Function : constant Boolean :=
                    (for some F of Typ_Property_Funcs => F = Subp_Id);
               begin
                  if not Subp_Is_Stable_Property_Function then
                     for SPF_Id of Typ_Property_Funcs loop
                        if not Excluded_By_Aspect_Spec_Of_Subp (SPF_Id) then
                           --  ??? Need to filter out checks for SPFs that are
                           --  mentioned explicitly in the postcondition of
                           --  Subp_Id.
                           Insert_Stable_Property_Check
                             (Formal => Formal, Property_Function => SPF_Id);
                        end if;
                     end loop;
                  end if;
               end;
            end if;
            Next_Formal (Formal);
         end loop;
      end Add_Stable_Property_Contracts;

      -------------------------
      -- Append_Enabled_Item --
      -------------------------

      procedure Append_Enabled_Item (Item : Node_Id; List : in out List_Id) is
      begin
         --  Do not chain ignored or disabled pragmas

         if Nkind (Item) = N_Pragma
           and then (Is_Ignored (Item) or else Is_Disabled (Item))
         then
            null;

         --  Otherwise, add the item

         else
            if No (List) then
               List := New_List;
            end if;

            --  If the pragma is a conjunct in a composite postcondition, it
            --  has been processed in reverse order. In the postcondition body
            --  it must appear before the others.

            if Nkind (Item) = N_Pragma
              and then From_Aspect_Specification (Item)
              and then Split_PPC (Item)
            then
               Prepend (Item, List);
            else
               Append (Item, List);
            end if;
         end if;
      end Append_Enabled_Item;

      ----------------------------
      -- Process_Contract_Cases --
      ----------------------------

      procedure Process_Contract_Cases
        (Stmts : in out List_Id;
         Decls : List_Id)
      is
         procedure Process_Contract_Cases_For (Subp_Id : Entity_Id);
         --  Process pragma Contract_Cases for subprogram Subp_Id

         --------------------------------
         -- Process_Contract_Cases_For --
         --------------------------------

         procedure Process_Contract_Cases_For (Subp_Id : Entity_Id) is
            Items : constant Node_Id := Contract (Subp_Id);
            Prag  : Node_Id;

         begin
            if Present (Items) then
               Prag := Contract_Test_Cases (Items);
               while Present (Prag) loop
                  if Is_Checked (Prag) then
                     if Pragma_Name (Prag) = Name_Always_Terminates then
                        Expand_Pragma_Always_Terminates (Prag);

                     elsif Pragma_Name (Prag) = Name_Contract_Cases then
                        Expand_Pragma_Contract_Cases
                          (CCs     => Prag,
                           Subp_Id => Subp_Id,
                           Decls   => Decls,
                           Stmts   => Stmts);

                     elsif Pragma_Name (Prag) = Name_Exceptional_Cases then
                        Expand_Pragma_Exceptional_Cases (Prag);

                     elsif Pragma_Name (Prag) = Name_Subprogram_Variant then
                        Expand_Pragma_Subprogram_Variant
                          (Prag       => Prag,
                           Subp_Id    => Subp_Id,
                           Body_Decls => Decls);
                     end if;
                  end if;

                  Prag := Next_Pragma (Prag);
               end loop;
            end if;
         end Process_Contract_Cases_For;

      --  Start of processing for Process_Contract_Cases

      begin
         Process_Contract_Cases_For (Body_Id);

         if Present (Spec_Id) then
            Process_Contract_Cases_For (Spec_Id);
         end if;
      end Process_Contract_Cases;

      ----------------------------
      -- Process_Postconditions --
      ----------------------------

      procedure Process_Postconditions (Stmts : in out List_Id) is
         procedure Process_Body_Postconditions (Post_Nam : Name_Id);
         --  Collect all [refined] postconditions of a specific kind denoted
         --  by Post_Nam that belong to the body, and generate pragma Check
         --  equivalents in list Stmts.

         procedure Process_Spec_Postconditions;
         --  Collect all [inherited] postconditions of the spec, and generate
         --  pragma Check equivalents in list Stmts.

         ---------------------------------
         -- Process_Body_Postconditions --
         ---------------------------------

         procedure Process_Body_Postconditions (Post_Nam : Name_Id) is
            Items     : constant Node_Id := Contract (Body_Id);
            Unit_Decl : constant Node_Id := Parent (Body_Decl);
            Decl      : Node_Id;
            Prag      : Node_Id;

         begin
            --  Process the contract

            if Present (Items) then
               Prag := Pre_Post_Conditions (Items);
               while Present (Prag) loop
                  if Pragma_Name (Prag) = Post_Nam
                    and then Is_Checked (Prag)
                  then
                     Append_Enabled_Item
                       (Item => Build_Pragma_Check_Equivalent (Prag),
                        List => Stmts);
                  end if;

                  Prag := Next_Pragma (Prag);
               end loop;
            end if;

            --  The subprogram body being processed is actually the proper body
            --  of a stub with a corresponding spec. The subprogram stub may
            --  carry a postcondition pragma, in which case it must be taken
            --  into account. The pragma appears after the stub.

            if Present (Spec_Id) and then Nkind (Unit_Decl) = N_Subunit then
               Decl := Next (Corresponding_Stub (Unit_Decl));
               while Present (Decl) loop

                  --  Note that non-matching pragmas are skipped

                  if Nkind (Decl) = N_Pragma then
                     if Pragma_Name (Decl) = Post_Nam
                       and then Is_Checked (Decl)
                     then
                        Append_Enabled_Item
                          (Item => Build_Pragma_Check_Equivalent (Decl),
                           List => Stmts);
                     end if;

                  --  Skip internally generated code

                  elsif not Comes_From_Source (Decl) then
                     null;

                  --  Postcondition pragmas are usually grouped together. There
                  --  is no need to inspect the whole declarative list.

                  else
                     exit;
                  end if;

                  Next (Decl);
               end loop;
            end if;
         end Process_Body_Postconditions;

         ---------------------------------
         -- Process_Spec_Postconditions --
         ---------------------------------

         procedure Process_Spec_Postconditions is
            Subps : constant Subprogram_List :=
                      Inherited_Subprograms (Spec_Id);
            Seen  : Subprogram_List (Subps'Range) := (others => Empty);

            function Seen_Subp (Subp_Id : Entity_Id) return Boolean;
            --  Return True if the contract of subprogram Subp_Id has been
            --  processed.

            ---------------
            -- Seen_Subp --
            ---------------

            function Seen_Subp (Subp_Id : Entity_Id) return Boolean is
            begin
               for Index in Seen'Range loop
                  if Seen (Index) = Subp_Id then
                     return True;
                  end if;
               end loop;

               return False;
            end Seen_Subp;

            --  Local variables

            Item    : Node_Id;
            Items   : Node_Id;
            Prag    : Node_Id;
            Subp_Id : Entity_Id;

         --  Start of processing for Process_Spec_Postconditions

         begin
            --  Process the contract

            Items := Contract (Spec_Id);

            if Present (Items) then
               Prag := Pre_Post_Conditions (Items);
               while Present (Prag) loop
                  if Pragma_Name (Prag) = Name_Postcondition
                    and then Is_Checked (Prag)
                  then
                     Append_Enabled_Item
                       (Item => Build_Pragma_Check_Equivalent (Prag),
                        List => Stmts);
                  end if;

                  Prag := Next_Pragma (Prag);
               end loop;
            end if;

            --  Process the contracts of all inherited subprograms, looking for
            --  class-wide postconditions.

            for Index in Subps'Range loop
               Subp_Id := Subps (Index);

               if Present (Alias (Subp_Id)) then
                  Subp_Id := Ultimate_Alias (Subp_Id);
               end if;

               --  Wrappers of class-wide pre/postconditions reference the
               --  parent primitive that has the inherited contract.

               if Is_Wrapper (Subp_Id)
                 and then Present (LSP_Subprogram (Subp_Id))
               then
                  Subp_Id := LSP_Subprogram (Subp_Id);
               end if;

               Items := Contract (Subp_Id);

               if not Seen_Subp (Subp_Id) and then Present (Items) then
                  Seen (Index) := Subp_Id;

                  Prag := Pre_Post_Conditions (Items);
                  while Present (Prag) loop
                     if Pragma_Name (Prag) = Name_Postcondition
                       and then Class_Present (Prag)
                     then
                        Item :=
                          Build_Pragma_Check_Equivalent
                            (Prag     => Prag,
                             Subp_Id  => Spec_Id,
                             Inher_Id => Subp_Id);

                        --  The pragma Check equivalent of the class-wide
                        --  postcondition is still created even though the
                        --  pragma may be ignored because the equivalent
                        --  performs semantic checks.

                        if Is_Checked (Prag) then
                           Append_Enabled_Item (Item, Stmts);
                        end if;
                     end if;

                     Prag := Next_Pragma (Prag);
                  end loop;
               end if;
            end loop;
         end Process_Spec_Postconditions;

         pragma Unmodified (Stmts);
         --  Stmts is passed as IN OUT to signal that the list can be updated,
         --  even if the corresponding integer value representing the list does
         --  not change.

      --  Start of processing for Process_Postconditions

      begin
         --  The processing of postconditions is done in reverse order (body
         --  first) to ensure the following arrangement:

         --    <refined postconditions from body>
         --    <postconditions from body>
         --    <postconditions from spec>
         --    <inherited postconditions>

         Process_Body_Postconditions (Name_Refined_Post);
         Process_Body_Postconditions (Name_Postcondition);

         if Present (Spec_Id) then
            Process_Spec_Postconditions;
         end if;
      end Process_Postconditions;

      ---------------------------
      -- Process_Preconditions --
      ---------------------------

      procedure Process_Preconditions (Decls : in out List_Id) is
         Insert_Node : Node_Id := Empty;
         --  The insertion node after which all pragma Check equivalents are
         --  inserted.

         procedure Prepend_To_Decls (Item : Node_Id);
         --  Prepend a single item to the declarations of the subprogram body

         procedure Prepend_Pragma_To_Decls (Prag : Node_Id);
         --  Prepend a normal precondition to the declarations of the body and
         --  analyze it.

         procedure Process_Preconditions_For (Subp_Id : Entity_Id);
         --  Collect all preconditions of subprogram Subp_Id and prepend their
         --  pragma Check equivalents to the declarations of the body.

         ----------------------
         -- Prepend_To_Decls --
         ----------------------

         procedure Prepend_To_Decls (Item : Node_Id) is
         begin
            --  Ensure that the body has a declarative list

            if No (Decls) then
               Decls := New_List;
               Set_Declarations (Body_Decl, Decls);
            end if;

            Prepend_To (Decls, Item);
         end Prepend_To_Decls;

         -----------------------------
         -- Prepend_Pragma_To_Decls --
         -----------------------------

         procedure Prepend_Pragma_To_Decls (Prag : Node_Id) is
            Check_Prag : Node_Id;

         begin
            --  Skip the sole class-wide precondition (if any) since it is
            --  processed by Merge_Class_Conditions.

            if Class_Present (Prag) then
               null;

            --  Accumulate the corresponding Check pragmas at the top of the
            --  declarations. Prepending the items ensures that they will be
            --  evaluated in their original order.

            else
               Check_Prag := Build_Pragma_Check_Equivalent (Prag);
               Prepend_To_Decls (Check_Prag);

            end if;
         end Prepend_Pragma_To_Decls;

         -------------------------------
         -- Process_Preconditions_For --
         -------------------------------

         procedure Process_Preconditions_For (Subp_Id : Entity_Id) is
            Items     : constant Node_Id := Contract (Subp_Id);
            Subp_Decl : constant Node_Id := Unit_Declaration_Node (Subp_Id);
            Decl      : Node_Id;
            Freeze_T  : Boolean;
            Prag      : Node_Id;

         begin
            --  Process the contract. If the body is an expression function
            --  that is a completion, freeze types within, because this may
            --  not have been done yet, when the subprogram declaration and
            --  its completion by an expression function appear in distinct
            --  declarative lists of the same unit (visible and private).

            Freeze_T :=
              Was_Expression_Function (Body_Decl)
                and then Sloc (Body_Id) /= Sloc (Subp_Id)
                and then In_Same_Source_Unit (Body_Id, Subp_Id)
                and then not In_Same_List (Body_Decl, Subp_Decl);

            if Present (Items) then
               Prag := Pre_Post_Conditions (Items);
               while Present (Prag) loop
                  if Pragma_Name (Prag) = Name_Precondition
                    and then Is_Checked (Prag)
                  then
                     if Freeze_T
                       and then Present (Corresponding_Aspect (Prag))
                     then
                        Freeze_Expr_Types
                          (Def_Id => Subp_Id,
                           Typ    => Standard_Boolean,
                           Expr   =>
                             Expression
                               (First (Pragma_Argument_Associations (Prag))),
                           N      => Body_Decl);
                     end if;

                     Prepend_Pragma_To_Decls (Prag);
                  end if;

                  Prag := Next_Pragma (Prag);
               end loop;
            end if;

            --  The subprogram declaration being processed is actually a body
            --  stub. The stub may carry a precondition pragma, in which case
            --  it must be taken into account. The pragma appears after the
            --  stub.

            if Nkind (Subp_Decl) = N_Subprogram_Body_Stub then

               --  Inspect the declarations following the body stub

               Decl := Next (Subp_Decl);
               while Present (Decl) loop

                  --  Note that non-matching pragmas are skipped

                  if Nkind (Decl) = N_Pragma then
                     if Pragma_Name (Decl) = Name_Precondition
                       and then Is_Checked (Decl)
                     then
                        Prepend_Pragma_To_Decls (Decl);
                     end if;

                  --  Skip internally generated code

                  elsif not Comes_From_Source (Decl) then
                     null;

                  --  Preconditions are usually grouped together. There is no
                  --  need to inspect the whole declarative list.

                  else
                     exit;
                  end if;

                  Next (Decl);
               end loop;
            end if;
         end Process_Preconditions_For;

         --  Local variables

         Body_Decls : constant List_Id := Declarations (Body_Decl);
         Decl       : Node_Id;
         Next_Decl  : Node_Id;

      --  Start of processing for Process_Preconditions

      begin
         --  Find the proper insertion point for all pragma Check equivalents

         if Present (Body_Decls) then
            Decl := First (Body_Decls);
            while Present (Decl) loop

               --  First source declaration terminates the search, because all
               --  preconditions must be evaluated prior to it, by definition.

               if Comes_From_Source (Decl) then
                  exit;

               --  Certain internally generated object renamings such as those
               --  for discriminants and protection fields must be elaborated
               --  before the preconditions are evaluated, as their expressions
               --  may mention the discriminants. The renamings include those
               --  for private components so we need to find the last such.

               elsif Is_Prologue_Renaming (Decl) then
                  while Present (Next (Decl))
                    and then Is_Prologue_Renaming (Next (Decl))
                  loop
                     Next (Decl);
                  end loop;

                  Insert_Node := Decl;

               --  Otherwise the declaration does not come from source. This
               --  also terminates the search, because internal code may raise
               --  exceptions which should not preempt the preconditions.

               else
                  exit;
               end if;

               Next (Decl);
            end loop;

            --  The processing of preconditions is done in reverse order (body
            --  first), because each pragma Check equivalent is inserted at the
            --  top of the declarations. This ensures that the final order is
            --  consistent with following diagram:

            --    <inherited preconditions>
            --    <preconditions from spec>
            --    <preconditions from body>

            Process_Preconditions_For (Body_Id);

            --  Move the generated entry-call prologue renamings into the
            --  outer declarations for use in the preconditions.

            Decl := First (Body_Decls);
            while Present (Decl) and then Present (Insert_Node) loop
               Next_Decl := Next (Decl);
               Remove (Decl);
               Prepend_To_Decls (Decl);

               exit when Decl = Insert_Node;
               Decl := Next_Decl;
            end loop;
         end if;

         if Present (Spec_Id) then
            Process_Preconditions_For (Spec_Id);
         end if;
      end Process_Preconditions;

      --  Local variables

      Restore_Scope : Boolean := False;
      Result        : Entity_Id;
      Stmts         : List_Id := No_List;
      Decls         : List_Id := New_List;
      Subp_Id       : Entity_Id;

   --  Start of processing for Expand_Subprogram_Contract

   begin
      --  Obtain the entity of the initial declaration

      if Present (Spec_Id) then
         Subp_Id := Spec_Id;
      else
         Subp_Id := Body_Id;
      end if;

      --  Do not perform expansion activity when it is not needed

      if not Expander_Active then
         return;

      --  GNATprove does not need the executable semantics of a contract

      elsif GNATprove_Mode then
         return;

      --  The contract of a generic subprogram or one declared in a generic
      --  context is not expanded, as the corresponding instance will provide
      --  the executable semantics of the contract.

      elsif Is_Generic_Subprogram (Subp_Id) or else Inside_A_Generic then
         return;

      --  All subprograms carry a contract, but for some it is not significant
      --  and should not be processed. This is a small optimization.

      elsif not Has_Significant_Contract (Subp_Id) then
         return;

      --  The contract of an ignored Ghost subprogram does not need expansion,
      --  because the subprogram and all calls to it will be removed.

      elsif Is_Ignored_Ghost_Entity (Subp_Id) then
         return;

      --  No action needed for helpers and indirect-call wrapper built to
      --  support class-wide preconditions.

      elsif Present (Class_Preconditions_Subprogram (Subp_Id)) then
         return;

      --  Do not re-expand the same contract. This scenario occurs when a
      --  construct is rewritten into something else during its analysis
      --  (expression functions for instance).

      elsif Has_Expanded_Contract (Subp_Id) then
         return;
      end if;

      --  Prevent multiple expansion attempts of the same contract

      Set_Has_Expanded_Contract (Subp_Id);

      --  Ensure that the formal parameters are visible when expanding all
      --  contract items.

      if not In_Open_Scopes (Subp_Id) then
         Restore_Scope := True;
         Push_Scope (Subp_Id);

         if Is_Generic_Subprogram (Subp_Id) then
            Install_Generic_Formals (Subp_Id);
         else
            Install_Formals (Subp_Id);
         end if;
      end if;

      --  The expansion of a subprogram contract involves the creation of Check
      --  pragmas to verify the contract assertions of the spec and body in a
      --  particular order. The order is as follows:

      --    function Original_Code (...) return ... is
      --       <prologue renamings>
      --       <inherited preconditions>
      --       <preconditions from spec>
      --       <preconditions from body>
      --       <contract case conditions>

      --       function _Wrapped_Statements (...) return ... is
      --          <source declarations>
      --       begin
      --          <source statements>
      --       end _Wrapped_Statements;

      --    begin
      --       return Result : constant ... := _Wrapped_Statements do
      --          <refined postconditions from body>
      --          <postconditions from body>
      --          <postconditions from spec>
      --          <inherited postconditions>
      --          <contract case consequences>
      --          <invariant check of function result>
      --          <invariant and predicate checks of parameters
      --       end return;
      --    end Original_Code;

      --  Step 1: augment contracts list with postconditions associated with
      --  Stable_Properties and Stable_Properties'Class aspects. This must
      --  precede Process_Postconditions.

      for Class_Present in Boolean loop
         Add_Stable_Property_Contracts
           (Subp_Id, Class_Present => Class_Present);
      end loop;

      --  Step 2: Handle all preconditions. This action must come before the
      --  processing of pragma Contract_Cases because the pragma prepends items
      --  to the body declarations.

      Process_Preconditions (Decls);

      --  Step 3: Handle all postconditions. This action must come before the
      --  processing of pragma Contract_Cases because the pragma appends items
      --  to list Stmts.

      Process_Postconditions (Stmts);

      --  Step 4: Handle pragma Contract_Cases. This action must come before
      --  the processing of invariants and predicates because those append
      --  items to list Stmts.

      Process_Contract_Cases (Stmts, Decls);

      --  Step 5: Apply invariant and predicate checks on a function result and
      --  all formals. The resulting checks are accumulated in list Stmts.

      Add_Invariant_And_Predicate_Checks (Subp_Id, Stmts, Result);

      --  Step 6: Construct subprogram _wrapped_statements

      --  When no statements are present we still need to insert contract
      --  related declarations.

      if No (Stmts) then
         Prepend_List_To (Declarations (Body_Decl), Decls);

      --  Otherwise, we need a wrapper

      else
         Build_Subprogram_Contract_Wrapper (Body_Id, Stmts, Decls, Result);
      end if;

      if Restore_Scope then
         End_Scope;
      end if;
   end Expand_Subprogram_Contract;

   -------------------------------
   -- Freeze_Previous_Contracts --
   -------------------------------

   procedure Freeze_Previous_Contracts (Body_Decl : Node_Id) is
      function Causes_Contract_Freezing (N : Node_Id) return Boolean;
      pragma Inline (Causes_Contract_Freezing);
      --  Determine whether arbitrary node N causes contract freezing. This is
      --  used as an assertion for the current body declaration that caused
      --  contract freezing, and as a condition to detect body declaration that
      --  already caused contract freezing before.

      procedure Freeze_Contracts;
      pragma Inline (Freeze_Contracts);
      --  Freeze the contracts of all eligible constructs which precede body
      --  Body_Decl.

      procedure Freeze_Enclosing_Package_Body;
      pragma Inline (Freeze_Enclosing_Package_Body);
      --  Freeze the contract of the nearest package body (if any) which
      --  encloses body Body_Decl.

      ------------------------------
      -- Causes_Contract_Freezing --
      ------------------------------

      function Causes_Contract_Freezing (N : Node_Id) return Boolean is
      begin
         --  The following condition matches guards for calls to
         --  Freeze_Previous_Contracts from routines that analyze various body
         --  declarations. In particular, it detects expression functions, as
         --  described in the call from Analyze_Subprogram_Body_Helper.

         return
           Comes_From_Source (Original_Node (N))
             and then
           Nkind (N) in
             N_Entry_Body      | N_Package_Body         | N_Protected_Body |
             N_Subprogram_Body | N_Subprogram_Body_Stub | N_Task_Body;
      end Causes_Contract_Freezing;

      ----------------------
      -- Freeze_Contracts --
      ----------------------

      procedure Freeze_Contracts is
         Body_Id : constant Entity_Id := Defining_Entity (Body_Decl);
         Decl    : Node_Id;

      begin
         --  Nothing to do when the body which causes freezing does not appear
         --  in a declarative list because there cannot possibly be constructs
         --  with contracts.

         if not Is_List_Member (Body_Decl) then
            return;
         end if;

         --  Inspect the declarations preceding the body, and freeze individual
         --  contracts of eligible constructs.

         Decl := Prev (Body_Decl);
         while Present (Decl) loop

            --  Stop the traversal when a preceding construct that causes
            --  freezing is encountered as there is no point in refreezing
            --  the already frozen constructs.

            if Causes_Contract_Freezing (Decl) then
               exit;

            --  Entry or subprogram declarations

            elsif Nkind (Decl) in N_Abstract_Subprogram_Declaration
                                | N_Entry_Declaration
                                | N_Generic_Subprogram_Declaration
                                | N_Subprogram_Declaration
            then
               Analyze_Entry_Or_Subprogram_Contract
                 (Subp_Id   => Defining_Entity (Decl),
                  Freeze_Id => Body_Id);

            --  Objects

            elsif Nkind (Decl) = N_Object_Declaration then
               Analyze_Object_Contract
                 (Obj_Id    => Defining_Entity (Decl),
                  Freeze_Id => Body_Id);

            --  Protected units

            elsif Nkind (Decl) in N_Protected_Type_Declaration
                                | N_Single_Protected_Declaration
            then
               Analyze_Protected_Contract (Defining_Entity (Decl));

            --  Subprogram body stubs

            elsif Nkind (Decl) = N_Subprogram_Body_Stub then
               Analyze_Subprogram_Body_Stub_Contract (Defining_Entity (Decl));

            --  Task units

            elsif Nkind (Decl) in N_Single_Task_Declaration
                                | N_Task_Type_Declaration
            then
               Analyze_Task_Contract (Defining_Entity (Decl));
            end if;

            if Nkind (Decl) in N_Full_Type_Declaration
                             | N_Private_Type_Declaration
                             | N_Task_Type_Declaration
                             | N_Protected_Type_Declaration
                             | N_Formal_Type_Declaration
            then
               Analyze_Type_Contract (Defining_Identifier (Decl));
            end if;

            Prev (Decl);
         end loop;
      end Freeze_Contracts;

      -----------------------------------
      -- Freeze_Enclosing_Package_Body --
      -----------------------------------

      procedure Freeze_Enclosing_Package_Body is
         Orig_Decl : constant Node_Id := Original_Node (Body_Decl);
         Par       : Node_Id;

      begin
         --  Climb the parent chain looking for an enclosing package body. Do
         --  not use the scope stack, because a body utilizes the entity of its
         --  corresponding spec.

         Par := Parent (Body_Decl);
         while Present (Par) loop
            if Nkind (Par) = N_Package_Body then
               Analyze_Package_Body_Contract
                 (Body_Id   => Defining_Entity (Par),
                  Freeze_Id => Defining_Entity (Body_Decl));

               exit;

            --  Do not look for an enclosing package body when the construct
            --  which causes freezing is a body generated for an expression
            --  function and it appears within a package spec. This ensures
            --  that the traversal will not reach too far up the parent chain
            --  and attempt to freeze a package body which must not be frozen.

            --    package body Enclosing_Body
            --      with Refined_State => (State => Var)
            --    is
            --       package Nested is
            --          type Some_Type is ...;
            --          function Cause_Freezing return ...;
            --       private
            --          function Cause_Freezing is (...);
            --       end Nested;
            --
            --       Var : Nested.Some_Type;

            elsif Nkind (Par) = N_Package_Declaration
              and then Nkind (Orig_Decl) = N_Expression_Function
            then
               exit;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;
      end Freeze_Enclosing_Package_Body;

      --  Local variables

      Body_Id : constant Entity_Id := Defining_Entity (Body_Decl);

   --  Start of processing for Freeze_Previous_Contracts

   begin
      pragma Assert (Causes_Contract_Freezing (Body_Decl));

      --  A body that is in the process of being inlined appears from source,
      --  but carries name _parent. Such a body does not cause freezing of
      --  contracts.

      if Chars (Body_Id) = Name_uParent then
         return;
      end if;

      Freeze_Enclosing_Package_Body;
      Freeze_Contracts;
   end Freeze_Previous_Contracts;

   ---------------------------------
   -- Inherit_Subprogram_Contract --
   ---------------------------------

   procedure Inherit_Subprogram_Contract
     (Subp      : Entity_Id;
      From_Subp : Entity_Id)
   is
      procedure Inherit_Pragma (Prag_Id : Pragma_Id);
      --  Propagate a pragma denoted by Prag_Id from From_Subp's contract to
      --  Subp's contract.

      --------------------
      -- Inherit_Pragma --
      --------------------

      procedure Inherit_Pragma (Prag_Id : Pragma_Id) is
         Prag     : constant Node_Id := Get_Pragma (From_Subp, Prag_Id);
         New_Prag : Node_Id;

      begin
         --  A pragma cannot be part of more than one First_Pragma/Next_Pragma
         --  chains, therefore the node must be replicated. The new pragma is
         --  flagged as inherited for distinction purposes.

         if Present (Prag) then
            New_Prag := New_Copy_Tree (Prag);
            Set_Is_Inherited_Pragma (New_Prag);

            Add_Contract_Item (New_Prag, Subp);
         end if;
      end Inherit_Pragma;

   --   Start of processing for Inherit_Subprogram_Contract

   begin
      --  Inheritance is carried out only when both entities are subprograms
      --  with contracts.

      if Is_Subprogram_Or_Generic_Subprogram (Subp)
        and then Is_Subprogram_Or_Generic_Subprogram (From_Subp)
        and then Present (Contract (From_Subp))
      then
         Inherit_Pragma (Pragma_Extensions_Visible);
         Inherit_Pragma (Pragma_Side_Effects);
      end if;
   end Inherit_Subprogram_Contract;

   -------------------------------------
   -- Instantiate_Subprogram_Contract --
   -------------------------------------

   procedure Instantiate_Subprogram_Contract (Templ : Node_Id; L : List_Id) is
      procedure Instantiate_Pragmas (First_Prag : Node_Id);
      --  Instantiate all contract-related source pragmas found in the list,
      --  starting with pragma First_Prag. Each instantiated pragma is added
      --  to list L.

      -------------------------
      -- Instantiate_Pragmas --
      -------------------------

      procedure Instantiate_Pragmas (First_Prag : Node_Id) is
         Inst_Prag : Node_Id;
         Prag      : Node_Id;

      begin
         Prag := First_Prag;
         while Present (Prag) loop
            if Is_Generic_Contract_Pragma (Prag) then
               Inst_Prag :=
                 Copy_Generic_Node (Prag, Empty, Instantiating => True);

               Set_Analyzed (Inst_Prag, False);
               Append_To (L, Inst_Prag);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;
      end Instantiate_Pragmas;

      --  Local variables

      Items : constant Node_Id := Contract (Defining_Entity (Templ));

   --  Start of processing for Instantiate_Subprogram_Contract

   begin
      if Present (Items) then
         Instantiate_Pragmas (Pre_Post_Conditions (Items));
         Instantiate_Pragmas (Contract_Test_Cases (Items));
         Instantiate_Pragmas (Classifications     (Items));
      end if;
   end Instantiate_Subprogram_Contract;

   --------------------------
   -- Is_Prologue_Renaming --
   --------------------------

   --  This should be turned into a flag and set during the expansion of
   --  task and protected types when the renamings get generated ???

   function Is_Prologue_Renaming (Decl : Node_Id) return Boolean is
      Nam  : Node_Id;
      Obj  : Entity_Id;
      Pref : Node_Id;
      Sel  : Node_Id;

   begin
      if Nkind (Decl) = N_Object_Renaming_Declaration
        and then not Comes_From_Source (Decl)
      then
         Obj := Defining_Entity (Decl);
         Nam := Name (Decl);

         if Nkind (Nam) = N_Selected_Component then
            --  Analyze the renaming declaration so we can further examine it

            if not Analyzed (Decl) then
               Analyze (Decl);
            end if;

            Pref := Prefix (Nam);
            Sel  := Selector_Name (Nam);

            --  A discriminant renaming appears as
            --    Discr : constant ... := Prefix.Discr;

            if Ekind (Obj) = E_Constant
              and then Is_Entity_Name (Sel)
              and then Present (Entity (Sel))
              and then Ekind (Entity (Sel)) = E_Discriminant
            then
               return True;

            --  A protection field renaming appears as
            --    Prot : ... := _object._object;

            --  A renamed private component is just a component of
            --  _object, with an arbitrary name.

            elsif Ekind (Obj) in E_Variable | E_Constant
              and then Nkind (Pref) = N_Identifier
              and then Chars (Pref) = Name_uObject
              and then Nkind (Sel) = N_Identifier
            then
               return True;
            end if;
         end if;
      end if;

      return False;
   end Is_Prologue_Renaming;

   -----------------------------------
   -- Make_Class_Precondition_Subps --
   -----------------------------------

   procedure Make_Class_Precondition_Subps
     (Subp_Id         : Entity_Id;
      Late_Overriding : Boolean := False)
   is
      Loc         : constant Source_Ptr := Sloc (Subp_Id);
      Tagged_Type : constant Entity_Id := Find_Dispatching_Type (Subp_Id);

      procedure Add_Indirect_Call_Wrapper;
      --  Build the indirect-call wrapper and append it to the freezing actions
      --  of Tagged_Type.

      procedure Add_Call_Helper
        (Helper_Id  : Entity_Id;
         Is_Dynamic : Boolean);
      --  Factorizes code for building a call helper with the given identifier
      --  and append it to the freezing actions of Tagged_Type. Is_Dynamic
      --  controls building the static or dynamic version of the helper.

      function Build_Unique_Name (Suffix : String) return Name_Id;
      --  Build an unique new name adding suffix to Subp_Id name (plus its
      --  homonym number for values bigger than 1).

      -------------------------------
      -- Add_Indirect_Call_Wrapper --
      -------------------------------

      procedure Add_Indirect_Call_Wrapper is

         function Build_ICW_Body return Node_Id;
         --  Build the body of the indirect call wrapper

         function Build_ICW_Decl return Node_Id;
         --  Build the declaration of the indirect call wrapper

         --------------------
         -- Build_ICW_Body --
         --------------------

         function Build_ICW_Body return Node_Id is
            ICW_Id    : constant Entity_Id := Indirect_Call_Wrapper (Subp_Id);
            Spec      : constant Node_Id   := Parent (ICW_Id);
            Body_Spec : Node_Id;
            Call      : Node_Id;
            ICW_Body  : Node_Id;

         begin
            Body_Spec := Copy_Subprogram_Spec (Spec);

            --  Build call to wrapped subprogram

            declare
               Actuals     : constant List_Id := Empty_List;
               Formal_Spec : Entity_Id :=
                               First (Parameter_Specifications (Spec));
            begin
               --  Build parameter association & call

               while Present (Formal_Spec) loop
                  Append_To (Actuals,
                    New_Occurrence_Of
                      (Defining_Identifier (Formal_Spec), Loc));
                  Next (Formal_Spec);
               end loop;

               if Ekind (ICW_Id) = E_Procedure then
                  Call :=
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Occurrence_Of (Subp_Id, Loc),
                      Parameter_Associations => Actuals);
               else
                  Call :=
                    Make_Simple_Return_Statement (Loc,
                      Expression =>
                        Make_Function_Call (Loc,
                          Name => New_Occurrence_Of (Subp_Id, Loc),
                          Parameter_Associations => Actuals));
               end if;
            end;

            ICW_Body :=
              Make_Subprogram_Body (Loc,
                Specification              => Body_Spec,
                Declarations               => New_List,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Call)));

            --  The new operation is internal and overriding indicators do not
            --  apply.

            Set_Must_Override (Body_Spec, False);

            return ICW_Body;
         end Build_ICW_Body;

         --------------------
         -- Build_ICW_Decl --
         --------------------

         function Build_ICW_Decl return Node_Id is
            ICW_Id : constant Entity_Id  :=
                       Make_Defining_Identifier (Loc,
                         Build_Unique_Name (Suffix => "ICW"));
            Decl   : Node_Id;
            Spec   : Node_Id;

         begin
            Spec := Copy_Subprogram_Spec (Parent (Subp_Id));
            Set_Must_Override      (Spec, False);
            Set_Must_Not_Override  (Spec, False);
            Set_Defining_Unit_Name (Spec, ICW_Id);
            Mutate_Ekind  (ICW_Id, Ekind (Subp_Id));
            Set_Is_Public (ICW_Id);

            --  The indirect call wrapper is commonly used for indirect calls
            --  but inlined for direct calls performed from the DTW.

            Set_Is_Inlined (ICW_Id);

            if Nkind (Spec) = N_Procedure_Specification then
               Set_Null_Present (Spec, False);
            end if;

            Decl := Make_Subprogram_Declaration (Loc, Spec);

            --  Link original subprogram to indirect wrapper and vice versa

            Set_Indirect_Call_Wrapper (Subp_Id, ICW_Id);
            Set_Class_Preconditions_Subprogram (ICW_Id, Subp_Id);

            --  Inherit debug info flag to allow debugging the wrapper

            if Needs_Debug_Info (Subp_Id) then
               Set_Debug_Info_Needed (ICW_Id);
            end if;

            return Decl;
         end Build_ICW_Decl;

         --  Local Variables

         ICW_Body : Node_Id;
         ICW_Decl : Node_Id;

      --  Start of processing for Add_Indirect_Call_Wrapper

      begin
         pragma Assert (No (Indirect_Call_Wrapper (Subp_Id)));

         ICW_Decl := Build_ICW_Decl;

         Append_Freeze_Action (Tagged_Type, ICW_Decl);
         Analyze (ICW_Decl);

         ICW_Body := Build_ICW_Body;
         Append_Freeze_Action (Tagged_Type, ICW_Body);

         --  We cannot defer the analysis of this ICW wrapper when it is
         --  built as a consequence of building its partner DTW wrapper
         --  at the freezing point of the tagged type.

         if Is_Dispatch_Table_Wrapper (Subp_Id) then
            Analyze (ICW_Body);
         end if;
      end Add_Indirect_Call_Wrapper;

      ---------------------
      -- Add_Call_Helper --
      ---------------------

      procedure Add_Call_Helper
        (Helper_Id  : Entity_Id;
         Is_Dynamic : Boolean)
      is
         function Build_Call_Helper_Body return Node_Id;
         --  Build the body of a call helper

         function Build_Call_Helper_Decl return Node_Id;
         --  Build the declaration of a call helper

         function Build_Call_Helper_Spec (Spec_Id : Entity_Id) return Node_Id;
         --  Build the specification of the helper

         ----------------------------
         -- Build_Call_Helper_Body --
         ----------------------------

         function Build_Call_Helper_Body return Node_Id is

            function Copy_And_Update_References
              (Expr : Node_Id) return Node_Id;
            --  Copy Expr updating references to formals of Helper_Id; update
            --  also references to loop identifiers of quantified expressions.

            --------------------------------
            -- Copy_And_Update_References --
            --------------------------------

            function Copy_And_Update_References
              (Expr : Node_Id) return Node_Id
            is
               Assoc_List : constant Elist_Id := New_Elmt_List;

               procedure Map_Quantified_Expression_Loop_Identifiers;
               --  Traverse Expr and append to Assoc_List the mapping of loop
               --  identifers of quantified expressions with its new copy.

               ------------------------------------------------
               -- Map_Quantified_Expression_Loop_Identifiers --
               ------------------------------------------------

               procedure Map_Quantified_Expression_Loop_Identifiers is
                  function Map_Loop_Param (N : Node_Id) return Traverse_Result;
                  --  Append to Assoc_List the mapping of loop identifers of
                  --  quantified expressions with its new copy.

                  --------------------
                  -- Map_Loop_Param --
                  --------------------

                  function Map_Loop_Param (N : Node_Id) return Traverse_Result
                  is
                  begin
                     if Nkind (N) = N_Loop_Parameter_Specification
                       and then Nkind (Parent (N)) = N_Quantified_Expression
                     then
                        declare
                           Def_Id : constant Entity_Id :=
                                      Defining_Identifier (N);
                        begin
                           Append_Elmt (Def_Id, Assoc_List);
                           Append_Elmt (New_Copy (Def_Id), Assoc_List);
                        end;
                     end if;

                     return OK;
                  end Map_Loop_Param;

                  procedure Map_Quantified_Expressions is
                     new Traverse_Proc (Map_Loop_Param);

               begin
                  Map_Quantified_Expressions (Expr);
               end Map_Quantified_Expression_Loop_Identifiers;

               --  Local variables

               Subp_Formal_Id   : Entity_Id := First_Formal (Subp_Id);
               Helper_Formal_Id : Entity_Id := First_Formal (Helper_Id);

            --  Start of processing for Copy_And_Update_References

            begin
               while Present (Subp_Formal_Id) loop
                  Append_Elmt (Subp_Formal_Id,   Assoc_List);
                  Append_Elmt (Helper_Formal_Id, Assoc_List);

                  Next_Formal (Subp_Formal_Id);
                  Next_Formal (Helper_Formal_Id);
               end loop;

               Map_Quantified_Expression_Loop_Identifiers;

               return New_Copy_Tree (Expr, Map => Assoc_List);
            end Copy_And_Update_References;

            --  Local variables

            Helper_Decl : constant Node_Id := Parent (Parent (Helper_Id));
            Body_Id     : Entity_Id;
            Body_Spec   : Node_Id;
            Body_Stmts  : Node_Id;
            Helper_Body : Node_Id;
            Return_Expr : Node_Id;

         --  Start of processing for Build_Call_Helper_Body

         begin
            pragma Assert (Analyzed (Unit_Declaration_Node (Helper_Id)));
            pragma Assert (No (Corresponding_Body (Helper_Decl)));

            Body_Id   := Make_Defining_Identifier (Loc, Chars (Helper_Id));
            Body_Spec := Build_Call_Helper_Spec (Body_Id);

            Set_Corresponding_Body (Helper_Decl, Body_Id);
            Set_Must_Override (Body_Spec, False);

            if Present (Class_Preconditions (Subp_Id))
            --  Evaluate the expression if we are building a dynamic helper
            --  or we are building a static helper for a non-abstract tagged
            --  type; for abstract tagged types the helper just returns True
            --  since it is called by the indirect call wrapper (ICW).
              and then
                (Is_Dynamic
                   or else
                      not Is_Abstract_Type (Find_Dispatching_Type (Subp_Id)))
            then
               Return_Expr :=
                 Copy_And_Update_References (Class_Preconditions (Subp_Id));

            --  When the subprogram is compiled with assertions disabled the
            --  helper just returns True; done to avoid reporting errors at
            --  link time since a unit may be compiled with assertions disabled
            --  and another (which depends on it) compiled with assertions
            --  enabled.

            else
               pragma Assert (Present (Ignored_Class_Preconditions (Subp_Id))
                 or else Is_Abstract_Type (Find_Dispatching_Type (Subp_Id)));
               Return_Expr := New_Occurrence_Of (Standard_True, Loc);
            end if;

            Body_Stmts :=
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Simple_Return_Statement (Loc, Return_Expr)));

            Helper_Body :=
              Make_Subprogram_Body (Loc,
                Specification              => Body_Spec,
                Declarations               => New_List,
                Handled_Statement_Sequence => Body_Stmts);

            return Helper_Body;
         end Build_Call_Helper_Body;

         ----------------------------
         -- Build_Call_Helper_Decl --
         ----------------------------

         function Build_Call_Helper_Decl return Node_Id is
            Decl : Node_Id;
            Spec : Node_Id;

         begin
            Spec := Build_Call_Helper_Spec (Helper_Id);
            Set_Must_Override      (Spec, False);
            Set_Must_Not_Override  (Spec, False);
            Set_Is_Inlined (Helper_Id);
            Set_Is_Public  (Helper_Id);

            Decl := Make_Subprogram_Declaration (Loc, Spec);

            --  Inherit debug info flag from Subp_Id to Helper_Id to allow
            --  debugging of the helper subprogram.

            if Needs_Debug_Info (Subp_Id) then
               Set_Debug_Info_Needed (Helper_Id);
            end if;

            return Decl;
         end Build_Call_Helper_Decl;

         ----------------------------
         -- Build_Call_Helper_Spec --
         ----------------------------

         function Build_Call_Helper_Spec (Spec_Id : Entity_Id) return Node_Id
         is
            Spec         : constant Node_Id := Parent (Subp_Id);
            Def_Id       : constant Node_Id := Defining_Unit_Name (Spec);
            Formal       : Entity_Id;
            Func_Formals : constant List_Id := New_List;
            P_Spec       : constant List_Id := Parameter_Specifications (Spec);
            Par_Formal   : Node_Id;
            Param        : Node_Id;
            Param_Type   : Node_Id;

         begin
            --  Create a list of formal parameters with the same types as the
            --  original subprogram but changing the controlling formal.

            Param  := First (P_Spec);
            Formal := First_Formal (Def_Id);
            while Present (Formal) loop
               Par_Formal := Parent (Formal);

               if Is_Dynamic and then Is_Controlling_Formal (Formal) then
                  if Nkind (Parameter_Type (Par_Formal))
                    = N_Access_Definition
                  then
                     Param_Type :=
                       Copy_Separate_Tree (Parameter_Type (Par_Formal));
                     Rewrite (Subtype_Mark (Param_Type),
                       Make_Attribute_Reference (Loc,
                         Prefix => Relocate_Node (Subtype_Mark (Param_Type)),
                         Attribute_Name => Name_Class));

                  else
                     Param_Type :=
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Occurrence_Of (Etype (Formal), Loc),
                         Attribute_Name => Name_Class);
                  end if;
               else
                  Param_Type := New_Occurrence_Of (Etype (Formal), Loc);
               end if;

               Append_To (Func_Formals,
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier    =>
                     Make_Defining_Identifier (Loc, Chars (Formal)),
                   In_Present             => In_Present (Par_Formal),
                   Out_Present            => Out_Present (Par_Formal),
                   Null_Exclusion_Present => Null_Exclusion_Present
                                               (Par_Formal),
                   Parameter_Type         => Param_Type));

               Next (Param);
               Next_Formal (Formal);
            end loop;

            return
              Make_Function_Specification (Loc,
                Defining_Unit_Name       => Spec_Id,
                Parameter_Specifications => Func_Formals,
                Result_Definition        =>
                  New_Occurrence_Of (Standard_Boolean, Loc));
         end Build_Call_Helper_Spec;

         --  Local variables

         Helper_Body : Node_Id;
         Helper_Decl : Node_Id;

      --  Start of processing for Add_Call_Helper

      begin
         Helper_Decl := Build_Call_Helper_Decl;
         Mutate_Ekind (Helper_Id, Ekind (Subp_Id));

         --  Add the helper to the freezing actions of the tagged type

         Append_Freeze_Action (Tagged_Type, Helper_Decl);
         Analyze (Helper_Decl);

         Helper_Body := Build_Call_Helper_Body;
         Append_Freeze_Action (Tagged_Type, Helper_Body);

         --  If this helper is built as part of building the DTW at the
         --  freezing point of its tagged type then we cannot defer
         --  its analysis.

         if Late_Overriding then
            pragma Assert (Is_Dispatch_Table_Wrapper (Subp_Id));
            Analyze (Helper_Body);
         end if;
      end Add_Call_Helper;

      -----------------------
      -- Build_Unique_Name --
      -----------------------

      function Build_Unique_Name (Suffix : String) return Name_Id is
      begin
         --  Append the homonym number. Strip the leading space character in
         --  the image of natural numbers. Also do not add the homonym value
         --  of 1.

         if Has_Homonym (Subp_Id) and then Homonym_Number (Subp_Id) > 1 then
            declare
               S : constant String := Homonym_Number (Subp_Id)'Img;

            begin
               return New_External_Name (Chars (Subp_Id),
                        Suffix => Suffix & "_" & S (2 .. S'Last));
            end;
         end if;

         return New_External_Name (Chars (Subp_Id), Suffix);
      end Build_Unique_Name;

      --  Local variables

      Helper_Id : Entity_Id;

   --  Start of processing for Make_Class_Precondition_Subps

   begin
      if Present (Class_Preconditions (Subp_Id))
        or Present (Ignored_Class_Preconditions (Subp_Id))
      then
         pragma Assert
           (Comes_From_Source (Subp_Id)
              or else Is_Dispatch_Table_Wrapper (Subp_Id));

         if No (Dynamic_Call_Helper (Subp_Id)) then

            --  Build and add to the freezing actions of Tagged_Type its
            --  dynamic-call helper.

            Helper_Id :=
              Make_Defining_Identifier (Loc,
                Build_Unique_Name (Suffix => "DP"));
            Add_Call_Helper (Helper_Id, Is_Dynamic => True);

            --  Link original subprogram to helper and vice versa

            Set_Dynamic_Call_Helper (Subp_Id, Helper_Id);
            Set_Class_Preconditions_Subprogram (Helper_Id, Subp_Id);
         end if;

         if not Is_Abstract_Subprogram (Subp_Id)
           and then No (Static_Call_Helper (Subp_Id))
         then
            --  Build and add to the freezing actions of Tagged_Type its
            --  static-call helper.

            Helper_Id :=
              Make_Defining_Identifier (Loc,
                Build_Unique_Name (Suffix => "SP"));

            Add_Call_Helper (Helper_Id, Is_Dynamic => False);

            --  Link original subprogram to helper and vice versa

            Set_Static_Call_Helper (Subp_Id, Helper_Id);
            Set_Class_Preconditions_Subprogram (Helper_Id, Subp_Id);

            --  Build and add to the freezing actions of Tagged_Type the
            --  indirect-call wrapper.

            Add_Indirect_Call_Wrapper;
         end if;
      end if;
   end Make_Class_Precondition_Subps;

   ----------------------------------------------
   -- Process_Class_Conditions_At_Freeze_Point --
   ----------------------------------------------

   procedure Process_Class_Conditions_At_Freeze_Point (Typ : Entity_Id) is

      procedure Check_Class_Conditions (Spec_Id : Entity_Id);
      --  Check class-wide pre/postconditions of Spec_Id

      function Has_Class_Postconditions_Subprogram
        (Spec_Id : Entity_Id) return Boolean;
      --  Return True if Spec_Id has (or inherits) a postconditions subprogram.

      function Has_Class_Preconditions_Subprogram
        (Spec_Id : Entity_Id) return Boolean;
      --  Return True if Spec_Id has (or inherits) a preconditions subprogram.

      ----------------------------
      -- Check_Class_Conditions --
      ----------------------------

      procedure Check_Class_Conditions (Spec_Id : Entity_Id) is
         Par_Subp : Entity_Id;

      begin
         for Kind in Condition_Kind loop
            Par_Subp := Nearest_Class_Condition_Subprogram (Kind, Spec_Id);

            if Present (Par_Subp) then
               Check_Class_Condition
                 (Cond            => Class_Condition (Kind, Par_Subp),
                  Subp            => Spec_Id,
                  Par_Subp        => Par_Subp,
                  Is_Precondition => Kind in Ignored_Class_Precondition
                                           | Class_Precondition);
            end if;
         end loop;
      end Check_Class_Conditions;

      -----------------------------------------
      -- Has_Class_Postconditions_Subprogram --
      -----------------------------------------

      function Has_Class_Postconditions_Subprogram
        (Spec_Id : Entity_Id) return Boolean is
      begin
         return
           Present (Nearest_Class_Condition_Subprogram
                     (Spec_Id => Spec_Id,
                      Kind    => Class_Postcondition))
             or else
           Present (Nearest_Class_Condition_Subprogram
                     (Spec_Id => Spec_Id,
                      Kind    => Ignored_Class_Postcondition));
      end Has_Class_Postconditions_Subprogram;

      ----------------------------------------
      -- Has_Class_Preconditions_Subprogram --
      ----------------------------------------

      function Has_Class_Preconditions_Subprogram
        (Spec_Id : Entity_Id) return Boolean is
      begin
         return
           Present (Nearest_Class_Condition_Subprogram
                     (Spec_Id => Spec_Id,
                      Kind    => Class_Precondition))
             or else
           Present (Nearest_Class_Condition_Subprogram
                     (Spec_Id => Spec_Id,
                      Kind    => Ignored_Class_Precondition));
      end Has_Class_Preconditions_Subprogram;

      --  Local variables

      Prim_Elmt : Elmt_Id := First_Elmt (Primitive_Operations (Typ));
      Prim      : Entity_Id;

   --  Start of processing for Process_Class_Conditions_At_Freeze_Point

   begin
      while Present (Prim_Elmt) loop
         Prim := Node (Prim_Elmt);

         if Has_Class_Preconditions_Subprogram (Prim)
           or else Has_Class_Postconditions_Subprogram (Prim)
         then
            if Comes_From_Source (Prim) then
               if Has_Significant_Contract (Prim) then
                  Merge_Class_Conditions (Prim);
               end if;

            --  Handle wrapper of protected operation

            elsif Is_Primitive_Wrapper (Prim) then
               Merge_Class_Conditions (Prim);

            --  Check inherited class-wide conditions, excluding internal
            --  entities built for mapping of interface primitives.

            elsif Is_Derived_Type (Typ)
              and then Present (Alias (Prim))
              and then No (Interface_Alias (Prim))
            then
               Check_Class_Conditions (Prim);
            end if;
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;
   end Process_Class_Conditions_At_Freeze_Point;

   ----------------------------
   -- Merge_Class_Conditions --
   ----------------------------

   procedure Merge_Class_Conditions (Spec_Id : Entity_Id) is

      procedure Process_Inherited_Conditions (Kind : Condition_Kind);
      --  Collect all inherited class-wide conditions of Spec_Id and merge
      --  them into one big condition.

      ----------------------------------
      -- Process_Inherited_Conditions --
      ----------------------------------

      procedure Process_Inherited_Conditions (Kind : Condition_Kind) is
         Tag_Typ : constant Entity_Id       := Find_Dispatching_Type (Spec_Id);
         Subps   : constant Subprogram_List := Inherited_Subprograms (Spec_Id);
         Seen    : Subprogram_List (Subps'Range) := (others => Empty);

         function Inherit_Condition
           (Par_Subp : Entity_Id;
            Subp     : Entity_Id) return Node_Id;
         --  Inherit the class-wide condition from Par_Subp to Subp and adjust
         --  all the references to formals in the inherited condition.

         procedure Merge_Conditions (From : Node_Id; Into : Node_Id);
         --  Merge two class-wide preconditions or postconditions (the former
         --  are merged using "or else", and the latter are merged using "and-
         --  then"). The changes are accumulated in parameter Into.

         function Seen_Subp (Id : Entity_Id) return Boolean;
         --  Return True if the contract of subprogram Id has been processed

         -----------------------
         -- Inherit_Condition --
         -----------------------

         function Inherit_Condition
           (Par_Subp : Entity_Id;
            Subp     : Entity_Id) return Node_Id
         is
            function Check_Condition (Expr : Node_Id) return Boolean;
            --  Used in assertion to check that Expr has no reference to the
            --  formals of Par_Subp.

            ---------------------
            -- Check_Condition --
            ---------------------

            function Check_Condition (Expr : Node_Id) return Boolean is
               Par_Formal_Id : Entity_Id;

               function Check_Entity (N : Node_Id) return Traverse_Result;
               --  Check occurrence of Par_Formal_Id

               ------------------
               -- Check_Entity --
               ------------------

               function Check_Entity (N : Node_Id) return Traverse_Result is
               begin
                  if Nkind (N) = N_Identifier
                    and then Present (Entity (N))
                    and then Entity (N) = Par_Formal_Id
                  then
                     return Abandon;
                  end if;

                  return OK;
               end Check_Entity;

               function Check_Expression is new Traverse_Func (Check_Entity);

            --  Start of processing for Check_Condition

            begin
               Par_Formal_Id := First_Formal (Par_Subp);

               while Present (Par_Formal_Id) loop
                  if Check_Expression (Expr) = Abandon then
                     return False;
                  end if;

                  Next_Formal (Par_Formal_Id);
               end loop;

               return True;
            end Check_Condition;

            --  Local variables

            Assoc_List     : constant Elist_Id := New_Elmt_List;
            Par_Formal_Id  : Entity_Id := First_Formal (Par_Subp);
            Subp_Formal_Id : Entity_Id := First_Formal (Subp);
            New_Condition  : Node_Id;

         begin
            while Present (Par_Formal_Id) loop
               Append_Elmt (Par_Formal_Id,  Assoc_List);
               Append_Elmt (Subp_Formal_Id, Assoc_List);

               Next_Formal (Par_Formal_Id);
               Next_Formal (Subp_Formal_Id);
            end loop;

            --  Check that Parent field of all the nodes have their correct
            --  decoration; required because otherwise mapped nodes with
            --  wrong Parent field are left unmodified in the copied tree
            --  and cause reporting wrong errors at later stages.

            pragma Assert
              (Check_Parents (Class_Condition (Kind, Par_Subp), Assoc_List));

            New_Condition :=
              New_Copy_Tree
                (Source => Class_Condition (Kind, Par_Subp),
                 Map    => Assoc_List);

            --  Ensure that the inherited condition has no reference to the
            --  formals of the parent subprogram.

            pragma Assert (Check_Condition (New_Condition));

            return New_Condition;
         end Inherit_Condition;

         ----------------------
         -- Merge_Conditions --
         ----------------------

         procedure Merge_Conditions (From : Node_Id; Into : Node_Id) is
            function Expression_Arg (Expr : Node_Id) return Node_Id;
            --  Return the boolean expression argument of a condition while
            --  updating its parentheses count for the subsequent merge.

            --------------------
            -- Expression_Arg --
            --------------------

            function Expression_Arg (Expr : Node_Id) return Node_Id is
            begin
               if Paren_Count (Expr) = 0 then
                  Set_Paren_Count (Expr, 1);
               end if;

               return Expr;
            end Expression_Arg;

            --  Local variables

            From_Expr : constant Node_Id := Expression_Arg (From);
            Into_Expr : constant Node_Id := Expression_Arg (Into);
            Loc       : constant Source_Ptr := Sloc (Into);

         --  Start of processing for Merge_Conditions

         begin
            case Kind is

               --  Merge the two preconditions by "or else"-ing them

               when Ignored_Class_Precondition
                  | Class_Precondition
               =>
                  Rewrite (Into_Expr,
                    Make_Or_Else (Loc,
                      Right_Opnd => Relocate_Node (Into_Expr),
                      Left_Opnd  => From_Expr));

               --  Merge the two postconditions by "and then"-ing them

               when Ignored_Class_Postcondition
                  | Class_Postcondition
               =>
                  Rewrite (Into_Expr,
                    Make_And_Then (Loc,
                      Right_Opnd => Relocate_Node (Into_Expr),
                      Left_Opnd  => From_Expr));
            end case;
         end Merge_Conditions;

         ---------------
         -- Seen_Subp --
         ---------------

         function Seen_Subp (Id : Entity_Id) return Boolean is
         begin
            for Index in Seen'Range loop
               if Seen (Index) = Id then
                  return True;
               end if;
            end loop;

            return False;
         end Seen_Subp;

         --  Local variables

         Class_Cond      : Node_Id;
         Cond            : Node_Id;
         Subp_Id         : Entity_Id;
         Par_Prim        : Entity_Id := Empty;
         Par_Iface_Prims : Elist_Id  := No_Elist;

      --  Start of processing for Process_Inherited_Conditions

      begin
         Class_Cond := Class_Condition (Kind, Spec_Id);

         --  Process parent primitives looking for nearest ancestor with
         --  class-wide conditions.

         for Index in Subps'Range loop
            Subp_Id := Subps (Index);

            if No (Par_Prim)
              and then Is_Ancestor (Find_Dispatching_Type (Subp_Id), Tag_Typ)
            then
               if Present (Alias (Subp_Id)) then
                  Subp_Id := Ultimate_Alias (Subp_Id);
               end if;

               --  Wrappers of class-wide pre/postconditions reference the
               --  parent primitive that has the inherited contract and help
               --  us to climb fast.

               if Is_Wrapper (Subp_Id)
                 and then Present (LSP_Subprogram (Subp_Id))
               then
                  Subp_Id := LSP_Subprogram (Subp_Id);
               end if;

               if not Seen_Subp (Subp_Id)
                 and then Present (Class_Condition (Kind, Subp_Id))
               then
                  Seen (Index)    := Subp_Id;
                  Par_Prim        := Subp_Id;
                  Par_Iface_Prims := Covered_Interface_Primitives (Par_Prim);

                  Cond := Inherit_Condition
                            (Subp     => Spec_Id,
                             Par_Subp => Subp_Id);

                  if Present (Class_Cond) then
                     Merge_Conditions (Cond, Class_Cond);
                  else
                     Class_Cond := Cond;
                  end if;

                  Check_Class_Condition
                    (Cond            => Class_Cond,
                     Subp            => Spec_Id,
                     Par_Subp        => Subp_Id,
                     Is_Precondition => Kind in Ignored_Class_Precondition
                                              | Class_Precondition);
                  Build_Class_Wide_Expression
                    (Pragma_Or_Expr  => Class_Cond,
                     Subp            => Spec_Id,
                     Par_Subp        => Subp_Id,
                     Adjust_Sloc     => False);

                  --  We are done as soon as we process the nearest ancestor

                  exit;
               end if;
            end if;
         end loop;

         --  Process the contract of interface primitives not covered by
         --  the nearest ancestor.

         for Index in Subps'Range loop
            Subp_Id := Subps (Index);

            if Is_Interface (Find_Dispatching_Type (Subp_Id)) then
               if Present (Alias (Subp_Id)) then
                  Subp_Id := Ultimate_Alias (Subp_Id);
               end if;

               if not Seen_Subp (Subp_Id)
                 and then Present (Class_Condition (Kind, Subp_Id))
                 and then not Contains (Par_Iface_Prims, Subp_Id)
               then
                  Seen (Index) := Subp_Id;

                  Cond := Inherit_Condition
                            (Subp     => Spec_Id,
                             Par_Subp => Subp_Id);

                  Check_Class_Condition
                    (Cond            => Cond,
                     Subp            => Spec_Id,
                     Par_Subp        => Subp_Id,
                     Is_Precondition => Kind in Ignored_Class_Precondition
                                              | Class_Precondition);
                  Build_Class_Wide_Expression
                    (Pragma_Or_Expr  => Cond,
                     Subp            => Spec_Id,
                     Par_Subp        => Subp_Id,
                     Adjust_Sloc     => False);

                  if Present (Class_Cond) then
                     Merge_Conditions (Cond, Class_Cond);
                  else
                     Class_Cond := Cond;
                  end if;
               end if;
            end if;
         end loop;

         Set_Class_Condition (Kind, Spec_Id, Class_Cond);
      end Process_Inherited_Conditions;

      --  Local variables

      Cond : Node_Id;

   --  Start of processing for Merge_Class_Conditions

   begin
      for Kind in Condition_Kind loop
         Cond := Class_Condition (Kind, Spec_Id);

         --  If this subprogram has class-wide conditions then preanalyze
         --  them before processing inherited conditions since conditions
         --  are checked and merged from right to left.

         if Present (Cond) then
            Preanalyze_Condition (Spec_Id, Cond);
         end if;

         Process_Inherited_Conditions (Kind);

         --  Preanalyze merged inherited conditions

         if Cond /= Class_Condition (Kind, Spec_Id) then
            Preanalyze_Condition (Spec_Id,
              Class_Condition (Kind, Spec_Id));
         end if;
      end loop;
   end Merge_Class_Conditions;

   ---------------------------------
   -- Preanalyze_Class_Conditions --
   ---------------------------------

   procedure Preanalyze_Class_Conditions (Spec_Id : Entity_Id) is
      Cond : Node_Id;

   begin
      for Kind in Condition_Kind loop
         Cond := Class_Condition (Kind, Spec_Id);

         if Present (Cond) then
            Preanalyze_Condition (Spec_Id, Cond);
         end if;
      end loop;
   end Preanalyze_Class_Conditions;

   --------------------------
   -- Preanalyze_Condition --
   --------------------------

   procedure Preanalyze_Condition
     (Subp : Entity_Id;
      Expr : Node_Id)
   is
      procedure Clear_Unset_References;
      --  Clear unset references on formals of Subp since preanalysis
      --  occurs in a place unrelated to the actual code.

      procedure Remove_Controlling_Arguments;
      --  Traverse Expr and clear the Controlling_Argument of calls to
      --  nonabstract functions.

      procedure Restore_Original_Selected_Component;
      --  Traverse Expr searching for dispatching calls to functions whose
      --  original node was a selected component, and replace them with
      --  their original node.

      ----------------------------
      -- Clear_Unset_References --
      ----------------------------

      procedure Clear_Unset_References is
         F : Entity_Id := First_Formal (Subp);

      begin
         while Present (F) loop
            Set_Unset_Reference (F, Empty);
            Next_Formal (F);
         end loop;
      end Clear_Unset_References;

      ----------------------------------
      -- Remove_Controlling_Arguments --
      ----------------------------------

      procedure Remove_Controlling_Arguments is
         function Remove_Ctrl_Arg (N : Node_Id) return Traverse_Result;
         --  Reset the Controlling_Argument of calls to nonabstract
         --  function calls.

         ---------------------
         -- Remove_Ctrl_Arg --
         ---------------------

         function Remove_Ctrl_Arg (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Function_Call
              and then Present (Controlling_Argument (N))
              and then not Is_Abstract_Subprogram (Entity (Name (N)))
            then
               Set_Controlling_Argument (N, Empty);
            end if;

            return OK;
         end Remove_Ctrl_Arg;

         procedure Remove_Ctrl_Args is new Traverse_Proc (Remove_Ctrl_Arg);
      begin
         Remove_Ctrl_Args (Expr);
      end Remove_Controlling_Arguments;

      -----------------------------------------
      -- Restore_Original_Selected_Component --
      -----------------------------------------

      procedure Restore_Original_Selected_Component is
         Restored_Nodes_List : Elist_Id := No_Elist;

         procedure Fix_Parents (N : Node_Id);
         --  Traverse the subtree of N fixing the Parent field of all the
         --  nodes.

         function Restore_Node (N : Node_Id) return Traverse_Result;
         --  Process dispatching calls to functions whose original node was
         --  a selected component, and replace them with their original
         --  node. Restored nodes are stored in the Restored_Nodes_List
         --  to fix the parent fields of their subtrees in a separate
         --  tree traversal.

         -----------------
         -- Fix_Parents --
         -----------------

         procedure Fix_Parents (N : Node_Id) is

            function Fix_Parent
              (Parent_Node : Node_Id;
               Node        : Node_Id) return Traverse_Result;
            --  Process a single node

            ----------------
            -- Fix_Parent --
            ----------------

            function Fix_Parent
              (Parent_Node : Node_Id;
               Node        : Node_Id) return Traverse_Result
            is
               Par : constant Node_Id := Parent (Node);

            begin
               if Par /= Parent_Node then
                  if Is_List_Member (Node) then
                     Set_List_Parent (List_Containing (Node), Parent_Node);
                  else
                     Set_Parent (Node, Parent_Node);
                  end if;
               end if;

               return OK;
            end Fix_Parent;

            procedure Fix_Parents is
               new Traverse_Proc_With_Parent (Fix_Parent);

         begin
            Fix_Parents (N);
         end Fix_Parents;

         ------------------
         -- Restore_Node --
         ------------------

         function Restore_Node (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Function_Call
              and then Nkind (Original_Node (N)) = N_Selected_Component
              and then Is_Dispatching_Operation (Entity (Name (N)))
            then
               Rewrite (N, Original_Node (N));
               Set_Original_Node (N, N);

               --  Save the restored node in the Restored_Nodes_List to fix
               --  the parent fields of their subtrees in a separate tree
               --  traversal.

               Append_New_Elmt (N, Restored_Nodes_List);
            end if;

            return OK;
         end Restore_Node;

         procedure Restore_Nodes is new Traverse_Proc (Restore_Node);

      --  Start of processing for Restore_Original_Selected_Component

      begin
         Restore_Nodes (Expr);

         --  After restoring the original node we must fix the decoration
         --  of the Parent attribute to ensure tree consistency; required
         --  because when the class-wide condition is inherited, calls to
         --  New_Copy_Tree will perform copies of this subtree, and formal
         --  occurrences with wrong Parent field cannot be mapped to the
         --  new formals.

         if Present (Restored_Nodes_List) then
            declare
               Elmt : Elmt_Id := First_Elmt (Restored_Nodes_List);

            begin
               while Present (Elmt) loop
                  Fix_Parents (Node (Elmt));
                  Next_Elmt (Elmt);
               end loop;
            end;
         end if;
      end Restore_Original_Selected_Component;

   --  Start of processing for Preanalyze_Condition

   begin
      pragma Assert (Present (Expr));
      pragma Assert (Inside_Class_Condition_Preanalysis = False);

      Push_Scope (Subp);
      Install_Formals (Subp);
      Inside_Class_Condition_Preanalysis := True;

      Preanalyze_Spec_Expression (Expr, Standard_Boolean);

      Inside_Class_Condition_Preanalysis := False;
      End_Scope;

      --  If this preanalyzed condition has occurrences of dispatching calls
      --  using the Object.Operation notation, during preanalysis such calls
      --  are rewritten as dispatching function calls; if at later stages
      --  this condition is inherited we must have restored the original
      --  selected-component node to ensure that the preanalysis of the
      --  inherited condition rewrites these dispatching calls in the
      --  correct context to avoid reporting spurious errors.

      Restore_Original_Selected_Component;

      --  Traverse Expr and clear the Controlling_Argument of calls to
      --  nonabstract functions. Required since the preanalyzed condition
      --  is not yet installed on its definite context and will be cloned
      --  and extended in derivations with additional conditions.

      Remove_Controlling_Arguments;

      --  Clear also attribute Unset_Reference; again because preanalysis
      --  occurs in a place unrelated to the actual code.

      Clear_Unset_References;
   end Preanalyze_Condition;

   ----------------------------------------
   -- Save_Global_References_In_Contract --
   ----------------------------------------

   procedure Save_Global_References_In_Contract
     (Templ  : Node_Id;
      Gen_Id : Entity_Id)
   is
      procedure Save_Global_References_In_List (First_Prag : Node_Id);
      --  Save all global references in contract-related source pragmas found
      --  in the list, starting with pragma First_Prag.

      ------------------------------------
      -- Save_Global_References_In_List --
      ------------------------------------

      procedure Save_Global_References_In_List (First_Prag : Node_Id) is
         Prag : Node_Id := First_Prag;

      begin
         while Present (Prag) loop
            if Is_Generic_Contract_Pragma (Prag) then
               Save_Global_References (Prag);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;
      end Save_Global_References_In_List;

      --  Local variables

      Items : constant Node_Id := Contract (Defining_Entity (Templ));

   --  Start of processing for Save_Global_References_In_Contract

   begin
      --  The entity of the analyzed generic copy must be on the scope stack
      --  to ensure proper detection of global references.

      Push_Scope (Gen_Id);

      if Permits_Aspect_Specifications (Templ)
        and then Has_Aspects (Templ)
      then
         Save_Global_References_In_Aspects (Templ);
      end if;

      if Present (Items) then
         Save_Global_References_In_List (Pre_Post_Conditions (Items));
         Save_Global_References_In_List (Contract_Test_Cases (Items));
         Save_Global_References_In_List (Classifications     (Items));
      end if;

      Pop_Scope;
   end Save_Global_References_In_Contract;

   -------------------------
   -- Set_Class_Condition --
   -------------------------

   procedure Set_Class_Condition
     (Kind : Condition_Kind;
      Subp : Entity_Id;
      Cond : Node_Id)
   is
   begin
      case Kind is
         when Class_Postcondition =>
            Set_Class_Postconditions (Subp, Cond);

         when Class_Precondition =>
            Set_Class_Preconditions (Subp, Cond);

         when Ignored_Class_Postcondition =>
            Set_Ignored_Class_Postconditions (Subp, Cond);

         when Ignored_Class_Precondition =>
            Set_Ignored_Class_Preconditions (Subp, Cond);
      end case;
   end Set_Class_Condition;

end Contracts;
