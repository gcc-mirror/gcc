------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C A T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Attr; use Sem_Attr;
with Sem_Aux;  use Sem_Aux;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;

package body Sem_Cat is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id;
      Is_Subunit      : Boolean);
   --  This procedure checks that the categorization of a lib unit and that
   --  of the depended unit satisfy dependency restrictions.
   --  The depended_entity can be the entity in a with_clause item, in which
   --  case Info_Node denotes that item. The depended_entity can also be the
   --  parent unit of a child unit, in which case Info_Node is the declaration
   --  of the child unit.  The error message is posted on Info_Node, and is
   --  specialized if Is_Subunit is true.

   procedure Check_Non_Static_Default_Expr
     (Type_Def : Node_Id;
      Obj_Decl : Node_Id);
   --  Iterate through the component list of a record definition, check
   --  that no component is declared with a nonstatic default value.
   --  If a nonstatic default exists, report an error on Obj_Decl.

   function Has_Read_Write_Attributes (E : Entity_Id) return Boolean;
   --  Return True if entity has attribute definition clauses for Read and
   --  Write attributes that are visible at some place.

   function Is_Non_Remote_Access_Type (E : Entity_Id) return Boolean;
   --  Returns true if the entity is a type whose full view is a non-remote
   --  access type, for the purpose of enforcing E.2.2(8) rules.

   function Has_Non_Remote_Access (Typ : Entity_Id) return Boolean;
   --  Return true if Typ or the type of any of its subcomponents is a non
   --  remote access type and doesn't have user-defined stream attributes.

   function No_External_Streaming (E : Entity_Id) return Boolean;
   --  Return True if the entity or one of its subcomponents does not support
   --  external streaming.

   function In_RCI_Declaration return Boolean;
   function In_RT_Declaration return Boolean;
   --  Determine if current scope is within the declaration of a Remote Call
   --  Interface or Remote Types unit, for semantic checking purposes.

   function In_Package_Declaration return Boolean;
   --  Shared supporting routine for In_RCI_Declaration and In_RT_Declaration

   function In_Shared_Passive_Unit return Boolean;
   --  Determines if current scope is within a Shared Passive compilation unit

   function Static_Discriminant_Expr (L : List_Id) return Boolean;
   --  Iterate through the list of discriminants to check if any of them
   --  contains non-static default expression, which is a violation in
   --  a preelaborated library unit.

   procedure Validate_Remote_Access_Object_Type_Declaration (T : Entity_Id);
   --  Check validity of declaration if RCI or RT unit. It should not contain
   --  the declaration of an access-to-object type unless it is a general
   --  access type that designates a class-wide limited private type. There are
   --  also constraints about the primitive subprograms of the class-wide type.
   --  RM E.2 (9, 13, 14)

   procedure Validate_RACW_Primitive
     (Subp : Entity_Id;
      RACW : Entity_Id);
   --  Check legality of the declaration of primitive Subp of the designated
   --  type of the given RACW type.

   ---------------------------------------
   -- Check_Categorization_Dependencies --
   ---------------------------------------

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id;
      Is_Subunit      : Boolean)
   is
      N   : constant Node_Id := Info_Node;
      Err : Boolean;

      --  Here we define an enumeration type to represent categorization types,
      --  ordered so that a unit with a given categorization can only WITH
      --  units with lower or equal categorization type.

      type Categorization is
        (Pure,
         Shared_Passive,
         Remote_Types,
         Remote_Call_Interface,
         Normal);

      function Get_Categorization (E : Entity_Id) return Categorization;
      --  Check categorization flags from entity, and return in the form
      --  of the lowest value of the Categorization type that applies to E.

      ------------------------
      -- Get_Categorization --
      ------------------------

      function Get_Categorization (E : Entity_Id) return Categorization is
      begin
         --  Get the lowest categorization that corresponds to E. Note that
         --  nothing prevents several (different) categorization pragmas
         --  to apply to the same library unit, in which case the unit has
         --  all associated categories, so we need to be careful here to
         --  check pragmas in proper Categorization order in order to
         --  return the lowest applicable value.

         --  Ignore Pure specification if set by pragma Pure_Function

         if Is_Pure (E)
           and then not
            (Has_Pragma_Pure_Function (E) and not Has_Pragma_Pure (E))
         then
            return Pure;

         elsif Is_Shared_Passive (E) then
            return Shared_Passive;

         elsif Is_Remote_Types (E) then
            return Remote_Types;

         elsif Is_Remote_Call_Interface (E) then
            return Remote_Call_Interface;

         else
            return Normal;
         end if;
      end Get_Categorization;

      Unit_Category : Categorization;
      With_Category : Categorization;

   --  Start of processing for Check_Categorization_Dependencies

   begin
      --  Intrinsic subprograms are preelaborated, so do not impose any
      --  categorization dependencies. Also, ignore categorization
      --  dependencies when compilation switch -gnatdu is used.

      if Is_Intrinsic_Subprogram (Depended_Entity) or else Debug_Flag_U then
         return;
      end if;

      --  First check 10.2.1 (11/1) rules on preelaborate packages

      if Is_Preelaborated (Unit_Entity)
        and then not Is_Preelaborated (Depended_Entity)
        and then not Is_Pure (Depended_Entity)
      then
         Err := True;
      else
         Err := False;
      end if;

      --  Check categorization rules of RM E.2(5)

      Unit_Category := Get_Categorization (Unit_Entity);
      With_Category := Get_Categorization (Depended_Entity);

      if With_Category > Unit_Category then

         --  Special case: Remote_Types and Remote_Call_Interface are allowed
         --  to WITH anything in the package body, per (RM E.2(5)).

         if (Unit_Category = Remote_Types
              or else Unit_Category = Remote_Call_Interface)
           and then In_Package_Body (Unit_Entity)
         then
            null;

         --  Special case: Remote_Types and Remote_Call_Interface declarations
         --  can depend on a preelaborated unit via a private with_clause, per
         --  AI05-0206.

         elsif (Unit_Category = Remote_Types
                  or else
                Unit_Category = Remote_Call_Interface)
           and then Nkind (N) = N_With_Clause
           and then Private_Present (N)
           and then Is_Preelaborated (Depended_Entity)
         then
            null;

         --  All other cases, we do have an error

         else
            Err := True;
         end if;
      end if;

      --  Here if we have an error

      if Err then

         --  These messages are warnings in GNAT mode or if the -gnateP switch
         --  was set. Otherwise these are real errors for real illegalities.

         --  The reason we suppress these errors in GNAT mode is that the run-
         --  time has several instances of violations of the categorization
         --  errors (e.g. Pure units withing Preelaborate units. All these
         --  violations are harmless in the cases where we intend them, and
         --  we suppress the warnings with Warnings (Off). In cases where we
         --  do not intend the violation, warnings are errors in GNAT mode
         --  anyway, so we will still get an error.

         Error_Msg_Warn :=
           Treat_Categorization_Errors_As_Warnings or GNAT_Mode;

         --  Don't give error if main unit is not an internal unit, and the
         --  unit generating the message is an internal unit. This is the
         --  situation in which such messages would be ignored in any case,
         --  so it is convenient not to generate them (since it causes
         --  annoying interference with debugging).

         if Is_Internal_Unit (Current_Sem_Unit)
           and then not Is_Internal_Unit (Main_Unit)
         then
            return;

         --  Dependence of Remote_Types or Remote_Call_Interface declaration
         --  on a preelaborated unit with a normal with_clause.

         elsif (Unit_Category = Remote_Types
                  or else
                Unit_Category = Remote_Call_Interface)
           and then Is_Preelaborated (Depended_Entity)
         then
            Error_Msg_NE
              ("<<must use private with clause for preelaborated unit& ",
               N, Depended_Entity);

         --  Subunit case

         elsif Is_Subunit then
            Error_Msg_NE
              ("<subunit cannot depend on& " &
               "(parent has wrong categorization)", N, Depended_Entity);

         --  Normal unit, not subunit

         else
            Error_Msg_NE
              ("<<cannot depend on& " &
               "(wrong categorization)", N, Depended_Entity);
         end if;

         --  Add further explanation for Pure/Preelaborate common cases

         if Unit_Category = Pure then
            Error_Msg_NE
              ("\<<pure unit cannot depend on non-pure unit",
               N, Depended_Entity);

         elsif Is_Preelaborated (Unit_Entity)
           and then not Is_Preelaborated (Depended_Entity)
           and then not Is_Pure (Depended_Entity)
         then
            Error_Msg_NE
              ("\<<preelaborated unit cannot depend on "
               & "non-preelaborated unit",
               N, Depended_Entity);
         end if;
      end if;
   end Check_Categorization_Dependencies;

   -----------------------------------
   -- Check_Non_Static_Default_Expr --
   -----------------------------------

   procedure Check_Non_Static_Default_Expr
     (Type_Def : Node_Id;
      Obj_Decl : Node_Id)
   is
      Recdef         : Node_Id;
      Component_Decl : Node_Id;

   begin
      if Nkind (Type_Def) = N_Derived_Type_Definition then
         Recdef := Record_Extension_Part (Type_Def);

         if No (Recdef) then
            return;
         end if;

      else
         Recdef := Type_Def;
      end if;

      --  Check that component declarations do not involve:

      --    a. a non-static default expression, where the object is
      --       declared to be default initialized.

      --    b. a dynamic Itype (discriminants and constraints)

      if Null_Present (Recdef) then
         return;
      else
         Component_Decl := First (Component_Items (Component_List (Recdef)));
      end if;

      while Present (Component_Decl)
        and then Nkind (Component_Decl) = N_Component_Declaration
      loop
         if Present (Expression (Component_Decl))
           and then Nkind (Expression (Component_Decl)) /= N_Null
           and then not Is_OK_Static_Expression (Expression (Component_Decl))
         then
            Error_Msg_Sloc := Sloc (Component_Decl);
            Error_Msg_F
              ("object in preelaborated unit has non-static default#",
               Obj_Decl);

         --  Fix this later ???

         --  elsif Has_Dynamic_Itype (Component_Decl) then
         --     Error_Msg_N
         --       ("dynamic type discriminant," &
         --        " constraint in preelaborated unit",
         --        Component_Decl);
         end if;

         Next (Component_Decl);
      end loop;
   end Check_Non_Static_Default_Expr;

   ---------------------------
   -- Has_Non_Remote_Access --
   ---------------------------

   function Has_Non_Remote_Access (Typ : Entity_Id) return Boolean is
      Component : Entity_Id;
      Comp_Type : Entity_Id;
      U_Typ     : constant Entity_Id := Underlying_Type (Typ);

   begin
      if No (U_Typ) then
         return False;

      elsif Has_Read_Write_Attributes (Typ)
        or else Has_Read_Write_Attributes (U_Typ)
      then
         return False;

      elsif Is_Non_Remote_Access_Type (U_Typ) then
         return True;
      end if;

      if Is_Record_Type (U_Typ) then
         Component := First_Entity (U_Typ);
         while Present (Component) loop
            if not Is_Tag (Component) then
               Comp_Type := Etype (Component);

               if Has_Non_Remote_Access (Comp_Type) then
                  return True;
               end if;
            end if;

            Next_Entity (Component);
         end loop;

      elsif Is_Array_Type (U_Typ) then
         return Has_Non_Remote_Access (Component_Type (U_Typ));

      end if;

      return False;
   end Has_Non_Remote_Access;

   -------------------------------
   -- Has_Read_Write_Attributes --
   -------------------------------

   function Has_Read_Write_Attributes (E : Entity_Id) return Boolean is
   begin
      return True
        and then Has_Stream_Attribute_Definition
                   (E, TSS_Stream_Read,  At_Any_Place => True)
        and then Has_Stream_Attribute_Definition
                   (E, TSS_Stream_Write, At_Any_Place => True);
   end Has_Read_Write_Attributes;

   -------------------------------------
   -- Has_Stream_Attribute_Definition --
   -------------------------------------

   function Has_Stream_Attribute_Definition
     (Typ          : Entity_Id;
      Nam          : TSS_Name_Type;
      At_Any_Place : Boolean := False) return Boolean
   is
      Rep_Item : Node_Id;

      Real_Rep : Node_Id;
      --  The stream operation may be specified by an attribute definition
      --  clause in the source, or by an aspect that generates such an
      --  attribute definition. For an aspect, the generated attribute
      --  definition may be placed at the freeze point of the full view of
      --  the type, but the aspect specification makes the operation visible
      --  to a client wherever the partial view is visible.

   begin
      --  We start from the declaration node and then loop until the end of
      --  the list until we find the requested attribute definition clause.
      --  In Ada 2005 mode, clauses are ignored if they are not currently
      --  visible (this is tested using the corresponding Entity, which is
      --  inserted by the expander at the point where the clause occurs),
      --  unless At_Any_Place is true.

      Rep_Item := First_Rep_Item (Typ);
      while Present (Rep_Item) loop
         Real_Rep := Rep_Item;

         --  If the representation item is an aspect specification, retrieve
         --  the corresponding pragma or attribute definition.

         if Nkind (Rep_Item) = N_Aspect_Specification then
            Real_Rep := Aspect_Rep_Item (Rep_Item);
         end if;

         if Nkind (Real_Rep) = N_Attribute_Definition_Clause then
            case Chars (Real_Rep) is
               when Name_Read =>
                  exit when Nam = TSS_Stream_Read;

               when Name_Write =>
                  exit when Nam = TSS_Stream_Write;

               when Name_Input =>
                  exit when Nam = TSS_Stream_Input;

               when Name_Output =>
                  exit when Nam = TSS_Stream_Output;

               when others =>
                  null;
            end case;
         end if;

         Next_Rep_Item (Rep_Item);
      end loop;

      --  If not found, and the type is derived from a private view, check
      --  for a stream attribute inherited from parent. Any specified stream
      --  attributes will be attached to the derived type's underlying type
      --  rather the derived type entity itself (which is itself private).

      if No (Rep_Item)
        and then Is_Private_Type (Typ)
        and then Is_Derived_Type (Typ)
        and then Present (Full_View (Typ))
      then
         return Has_Stream_Attribute_Definition
            (Underlying_Type (Typ), Nam, At_Any_Place);

      --  Otherwise, if At_Any_Place is true, return True if the attribute is
      --  available at any place; if it is false, return True only if the
      --  attribute is currently visible.

      else
         return Present (Rep_Item)
           and then (Ada_Version < Ada_2005
                      or else At_Any_Place
                      or else not Is_Hidden (Entity (Rep_Item)));
      end if;
   end Has_Stream_Attribute_Definition;

   ----------------------------
   -- In_Package_Declaration --
   ----------------------------

   function In_Package_Declaration return Boolean is
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the body of an RCI or RT unit

      return Is_Package_Or_Generic_Package (Current_Scope)
        and then Unit_Kind /= N_Package_Body
        and then not In_Package_Body (Current_Scope)
        and then not In_Instance;
   end In_Package_Declaration;

   ---------------------------
   -- In_Preelaborated_Unit --
   ---------------------------

   function In_Preelaborated_Unit return Boolean is
      Unit_Entity : Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  If evaluating actuals for a child unit instantiation, then ignore
      --  the preelaboration status of the parent; use the child instead.

      if Is_Compilation_Unit (Unit_Entity)
        and then Unit_Kind in N_Generic_Instantiation
        and then not In_Same_Source_Unit (Unit_Entity,
                                          Cunit (Current_Sem_Unit))
      then
         Unit_Entity := Cunit_Entity (Current_Sem_Unit);
      end if;

      --  There are no constraints on the body of Remote_Call_Interface or
      --  Remote_Types packages.

      return (Unit_Entity /= Standard_Standard)
        and then (Is_Preelaborated (Unit_Entity)
                    or else Is_Pure (Unit_Entity)
                    or else Is_Shared_Passive (Unit_Entity)
                    or else
                      ((Is_Remote_Types (Unit_Entity)
                          or else Is_Remote_Call_Interface (Unit_Entity))
                         and then Ekind (Unit_Entity) = E_Package
                         and then Unit_Kind /= N_Package_Body
                         and then not In_Package_Body (Unit_Entity)
                         and then not In_Instance));
   end In_Preelaborated_Unit;

   ------------------
   -- In_Pure_Unit --
   ------------------

   function In_Pure_Unit return Boolean is
   begin
      return Is_Pure (Current_Scope);
   end In_Pure_Unit;

   ------------------------
   -- In_RCI_Declaration --
   ------------------------

   function In_RCI_Declaration return Boolean is
   begin
      return Is_Remote_Call_Interface (Current_Scope)
        and then In_Package_Declaration;
   end In_RCI_Declaration;

   -----------------------
   -- In_RT_Declaration --
   -----------------------

   function In_RT_Declaration return Boolean is
   begin
      return Is_Remote_Types (Current_Scope) and then In_Package_Declaration;
   end In_RT_Declaration;

   ----------------------------
   -- In_Shared_Passive_Unit --
   ----------------------------

   function In_Shared_Passive_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;

   begin
      return Is_Shared_Passive (Unit_Entity);
   end In_Shared_Passive_Unit;

   ---------------------------------------
   -- In_Subprogram_Task_Protected_Unit --
   ---------------------------------------

   function In_Subprogram_Task_Protected_Unit return Boolean is
      E : Entity_Id;

   begin
      --  The following is to verify that a declaration is inside
      --  subprogram, generic subprogram, task unit, protected unit.
      --  Used to validate if a lib. unit is Pure. RM 10.2.1(16).

      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         if Is_Subprogram_Or_Generic_Subprogram (E)
              or else
            Is_Concurrent_Type (E)
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;
   end In_Subprogram_Task_Protected_Unit;

   -------------------------------
   -- Is_Non_Remote_Access_Type --
   -------------------------------

   function Is_Non_Remote_Access_Type (E : Entity_Id) return Boolean is
      U_E : constant Entity_Id := Underlying_Type (Base_Type (E));
      --  Use full view of base type to handle subtypes properly.

   begin
      if No (U_E) then

         --  This case arises for the case of a generic formal type, in which
         --  case E.2.2(8) rules will be enforced at instantiation time.

         return False;
      end if;

      return Is_Access_Type (U_E)
        and then not Is_Remote_Access_To_Class_Wide_Type (U_E)
        and then not Is_Remote_Access_To_Subprogram_Type (U_E);
   end Is_Non_Remote_Access_Type;

   ---------------------------
   -- No_External_Streaming --
   ---------------------------

   function No_External_Streaming (E : Entity_Id) return Boolean is
      U_E : constant Entity_Id := Underlying_Type (E);

   begin
      if No (U_E) then
         return False;

      elsif Has_Read_Write_Attributes (E) then

         --  Note: availability of stream attributes is tested on E, not U_E.
         --  There may be stream attributes defined on U_E that are not visible
         --  at the place where support of external streaming is tested.

         return False;

      elsif Has_Non_Remote_Access (U_E) then
         return True;
      end if;

      return Is_Limited_Type (E);
   end No_External_Streaming;

   -------------------------------------
   -- Set_Categorization_From_Pragmas --
   -------------------------------------

   procedure Set_Categorization_From_Pragmas (N : Node_Id) is
      P   : constant Node_Id := Parent (N);
      S   : constant Entity_Id := Current_Scope;

      procedure Set_Parents (Visibility : Boolean);
         --  If this is a child instance, the parents are not immediately
         --  visible during analysis. Make them momentarily visible so that
         --  the argument of the pragma can be resolved properly, and reset
         --  afterwards.

      -----------------
      -- Set_Parents --
      -----------------

      procedure Set_Parents (Visibility : Boolean) is
         Par : Entity_Id;
      begin
         Par := Scope (S);
         while Present (Par) and then Par /= Standard_Standard loop
            Set_Is_Immediately_Visible (Par, Visibility);
            Par := Scope (Par);
         end loop;
      end Set_Parents;

   --  Start of processing for Set_Categorization_From_Pragmas

   begin
      --  Deal with categorization pragmas in Pragmas of Compilation_Unit.
      --  The purpose is to set categorization flags before analyzing the
      --  unit itself, so as to diagnose violations of categorization as
      --  we process each declaration, even though the pragma appears after
      --  the unit.

      if Nkind (P) /= N_Compilation_Unit then
         return;
      end if;

      declare
         PN : Node_Id;

      begin
         if Is_Child_Unit (S) and then Is_Generic_Instance (S) then
            Set_Parents (True);
         end if;

         PN := First (Pragmas_After (Aux_Decls_Node (P)));
         while Present (PN) loop

            --  Skip implicit types that may have been introduced by
            --  previous analysis.

            if Nkind (PN) = N_Pragma then
               case Get_Pragma_Id (PN) is
                  when Pragma_All_Calls_Remote
                     | Pragma_Preelaborate
                     | Pragma_Pure
                     | Pragma_Remote_Call_Interface
                     | Pragma_Remote_Types
                     | Pragma_Shared_Passive
                  =>
                     Analyze (PN);

                  when others =>
                     null;
               end case;
            end if;

            Next (PN);
         end loop;

         if Is_Child_Unit (S) and then Is_Generic_Instance (S) then
            Set_Parents (False);
         end if;
      end;
   end Set_Categorization_From_Pragmas;

   -----------------------------------
   -- Set_Categorization_From_Scope --
   -----------------------------------

   procedure Set_Categorization_From_Scope (E : Entity_Id; Scop : Entity_Id) is
      Declaration   : Node_Id := Empty;
      Specification : Node_Id := Empty;

   begin
      --  Do not modify the purity of an internally generated entity if it has
      --  been explicitly marked as pure for optimization purposes.

      if not Has_Pragma_Pure_Function (E) then
         Set_Is_Pure
           (E, Is_Pure (Scop) and then Is_Library_Level_Entity (E));
      end if;

      if not Is_Remote_Call_Interface (E) then
         if Ekind (E) in Subprogram_Kind then
            Declaration := Unit_Declaration_Node (E);

            if Nkind_In (Declaration, N_Subprogram_Body,
                                      N_Subprogram_Renaming_Declaration)
            then
               Specification := Corresponding_Spec (Declaration);
            end if;
         end if;

         --  A subprogram body or renaming-as-body is a remote call interface
         --  if it serves as the completion of a subprogram declaration that
         --  is a remote call interface.

         if Nkind (Specification) in N_Entity then
            Set_Is_Remote_Call_Interface
              (E, Is_Remote_Call_Interface (Specification));

         --  A subprogram declaration is a remote call interface when it is
         --  declared within the visible part of, or declared by, a library
         --  unit declaration that is a remote call interface.

         else
            Set_Is_Remote_Call_Interface
              (E, Is_Remote_Call_Interface (Scop)
                    and then not (In_Private_Part (Scop)
                                   or else In_Package_Body (Scop)));
         end if;
      end if;

      Set_Is_Remote_Types
        (E, Is_Remote_Types (Scop)
              and then not (In_Private_Part (Scop)
                             or else In_Package_Body (Scop)));
   end Set_Categorization_From_Scope;

   ------------------------------
   -- Static_Discriminant_Expr --
   ------------------------------

   --  We need to accommodate a Why_Not_Static call somehow here ???

   function Static_Discriminant_Expr (L : List_Id) return Boolean is
      Discriminant_Spec : Node_Id;

   begin
      Discriminant_Spec := First (L);
      while Present (Discriminant_Spec) loop
         if Present (Expression (Discriminant_Spec))
           and then
             not Is_OK_Static_Expression (Expression (Discriminant_Spec))
         then
            return False;
         end if;

         Next (Discriminant_Spec);
      end loop;

      return True;
   end Static_Discriminant_Expr;

   --------------------------------------
   -- Validate_Access_Type_Declaration --
   --------------------------------------

   procedure Validate_Access_Type_Declaration (T : Entity_Id; N : Node_Id) is
      Def : constant Node_Id := Type_Definition (N);

   begin
      case Nkind (Def) is

         --  Access to subprogram case

         when N_Access_To_Subprogram_Definition =>

            --  A pure library_item must not contain the declaration of a
            --  named access type, except within a subprogram, generic
            --  subprogram, task unit, or protected unit (RM 10.2.1(16)).

            --  This test is skipped in Ada 2005 (see AI-366)

            if Ada_Version < Ada_2005
              and then Comes_From_Source (T)
              and then In_Pure_Unit
              and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N ("named access type not allowed in pure unit", T);
            end if;

         --  Access to object case

         when N_Access_To_Object_Definition =>
            if Comes_From_Source (T)
              and then In_Pure_Unit
              and then not In_Subprogram_Task_Protected_Unit
            then
               --  We can't give the message yet, since the type is not frozen
               --  and in Ada 2005 mode, access types are allowed in pure units
               --  if the type has no storage pool (see AI-366). So we set a
               --  flag which will be checked at freeze time.

               Set_Is_Pure_Unit_Access_Type (T);
            end if;

            --  Check for RCI or RT unit type declaration: declaration of an
            --  access-to-object type is illegal unless it is a general access
            --  type that designates a class-wide limited private type.
            --  Note that constraints on the primitive subprograms of the
            --  designated tagged type are not enforced here but in
            --  Validate_RACW_Primitives, which is done separately because the
            --  designated type might not be frozen (and therefore its
            --  primitive operations might not be completely known) at the
            --  point of the RACW declaration.

            Validate_Remote_Access_Object_Type_Declaration (T);

            --  Check for shared passive unit type declaration. It should
            --  not contain the declaration of access to class wide type,
            --  access to task type and access to protected type with entry.

            Validate_SP_Access_Object_Type_Decl (T);

         when others =>
            null;
      end case;

      --  Set categorization flag from package on entity as well, to allow
      --  easy checks later on for required validations of RCI or RT units.
      --  This is only done for entities that are in the original source.

      if Comes_From_Source (T)
        and then not (In_Package_Body (Scope (T))
                       or else In_Private_Part (Scope (T)))
      then
         Set_Is_Remote_Call_Interface
           (T, Is_Remote_Call_Interface (Scope (T)));
         Set_Is_Remote_Types
           (T, Is_Remote_Types (Scope (T)));
      end if;
   end Validate_Access_Type_Declaration;

   ----------------------------
   -- Validate_Ancestor_Part --
   ----------------------------

   procedure Validate_Ancestor_Part (N : Node_Id) is
      A : constant Node_Id   := Ancestor_Part (N);
      T : constant Entity_Id := Entity (A);

   begin
      if In_Preelaborated_Unit
        and then not In_Subprogram_Or_Concurrent_Unit
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (N)))
      then
         --  If the type is private, it must have the Ada 2005 pragma
         --  Has_Preelaborable_Initialization.

         --  The check is omitted within predefined units. This is probably
         --  obsolete code to fix the Ada 95 weakness in this area ???

         if Is_Private_Type (T)
           and then not Has_Pragma_Preelab_Init (T)
           and then not In_Internal_Unit (N)
         then
            Error_Msg_N
              ("private ancestor type not allowed in preelaborated unit", A);

         elsif Is_Record_Type (T) then
            if Nkind (Parent (T)) = N_Full_Type_Declaration then
               Check_Non_Static_Default_Expr
                 (Type_Definition (Parent (T)), A);
            end if;
         end if;
      end if;
   end Validate_Ancestor_Part;

   ----------------------------------------
   -- Validate_Categorization_Dependency --
   ----------------------------------------

   procedure Validate_Categorization_Dependency
     (N : Node_Id;
      E : Entity_Id)
   is
      K          : constant Node_Kind := Nkind (N);
      P          : Node_Id            := Parent (N);
      U          : Entity_Id := E;
      Is_Subunit : constant Boolean := Nkind (P) = N_Subunit;

   begin
      --  Only validate library units and subunits. For subunits, checks
      --  concerning withed units apply to the parent compilation unit.

      if Is_Subunit then
         P := Parent (P);
         U := Scope (E);

         while Present (U)
           and then not Is_Compilation_Unit (U)
           and then not Is_Child_Unit (U)
         loop
            U := Scope (U);
         end loop;
      end if;

      if Nkind (P) /= N_Compilation_Unit then
         return;
      end if;

      --  Body of RCI unit does not need validation

      if Is_Remote_Call_Interface (E)
        and then Nkind_In (N, N_Package_Body, N_Subprogram_Body)
      then
         return;
      end if;

      --  Ada 2005 (AI-50217): Process explicit non-limited with_clauses

      declare
         Item             : Node_Id;
         Entity_Of_Withed : Entity_Id;

      begin
         Item := First (Context_Items (P));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then
                not (Implicit_With (Item)
                      or else Limited_Present (Item)

                      --  Skip if error already posted on the WITH clause (in
                      --  which case the Name attribute may be invalid). In
                      --  particular, this fixes the problem of hanging in the
                      --  presence of a WITH clause on a child that is an
                      --  illegal generic instantiation.

                      or else Error_Posted (Item))
              and then
                not (Try_Semantics

                      --  Skip processing malformed trees

                      and then Nkind (Name (Item)) not in N_Has_Entity)
            then
               Entity_Of_Withed := Entity (Name (Item));
               Check_Categorization_Dependencies
                 (U, Entity_Of_Withed, Item, Is_Subunit);
            end if;

            Next (Item);
         end loop;
      end;

      --  Child depends on parent; therefore parent should also be categorized
      --  and satisfy the dependency hierarchy.

      --  Check if N is a child spec

      if (K in N_Generic_Declaration              or else
          K in N_Generic_Instantiation            or else
          K in N_Generic_Renaming_Declaration     or else
          K =  N_Package_Declaration              or else
          K =  N_Package_Renaming_Declaration     or else
          K =  N_Subprogram_Declaration           or else
          K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (N))
      then
         Check_Categorization_Dependencies (E, Scope (E), N, False);

         --  Verify that public child of an RCI library unit must also be an
         --  RCI library unit (RM E.2.3(15)).

         if Is_Remote_Call_Interface (Scope (E))
           and then not Private_Present (P)
           and then not Is_Remote_Call_Interface (E)
         then
            Error_Msg_N ("public child of rci unit must also be rci unit", N);
         end if;
      end if;
   end Validate_Categorization_Dependency;

   --------------------------------
   -- Validate_Controlled_Object --
   --------------------------------

   procedure Validate_Controlled_Object (E : Entity_Id) is
   begin
      --  Don't need this check in Ada 2005 mode, where this is all taken
      --  care of by the mechanism for Preelaborable Initialization.

      if Ada_Version >= Ada_2005 then
         return;
      end if;

      --  For now, never apply this check for internal GNAT units, since we
      --  have a number of cases in the library where we are stuck with objects
      --  of this type, and the RM requires Preelaborate.

      --  For similar reasons, we only do this check for source entities, since
      --  we generate entities of this type in some situations.

      --  Note that the 10.2.1(9) restrictions are not relevant to us anyway.
      --  We have to enforce them for RM compatibility, but we have no trouble
      --  accepting these objects and doing the right thing. Note that there is
      --  no requirement that Preelaborate not actually generate any code.

      if In_Preelaborated_Unit
        and then not Debug_Flag_PP
        and then Comes_From_Source (E)
        and then not In_Internal_Unit (E)
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (E)))
        and then not Is_Protected_Type (Etype (E))
      then
         Error_Msg_N
           ("library level controlled object not allowed in " &
            "preelaborated unit", E);
      end if;
   end Validate_Controlled_Object;

   --------------------------------------
   -- Validate_Null_Statement_Sequence --
   --------------------------------------

   procedure Validate_Null_Statement_Sequence (N : Node_Id) is
      Item : Node_Id;

   begin
      if In_Preelaborated_Unit then
         Item := First (Statements (Handled_Statement_Sequence (N)));
         while Present (Item) loop
            if Nkind (Item) /= N_Label
              and then Nkind (Item) /= N_Null_Statement
            then
               --  In GNAT mode, this is a warning, allowing the run-time
               --  to judiciously bypass this error condition.

               Error_Msg_Warn := GNAT_Mode;
               Error_Msg_N
                 ("<<statements not allowed in preelaborated unit", Item);

               exit;
            end if;

            Next (Item);
         end loop;
      end if;
   end Validate_Null_Statement_Sequence;

   ---------------------------------
   -- Validate_Object_Declaration --
   ---------------------------------

   procedure Validate_Object_Declaration (N : Node_Id) is
      Id  : constant Entity_Id  := Defining_Identifier (N);
      E   : constant Node_Id    := Expression (N);
      Odf : constant Node_Id    := Object_Definition (N);
      T   : constant Entity_Id  := Etype (Id);

   begin
      --  Verify that any access to subprogram object does not have in its
      --  subprogram profile access type parameters or limited parameters
      --  without Read and Write attributes (E.2.3(13)).

      Validate_RCI_Subprogram_Declaration (N);

      --  Check that if we are in preelaborated elaboration code, then we
      --  do not have an instance of a default initialized private, task or
      --  protected object declaration which would violate (RM 10.2.1(9)).
      --  Note that constants are never default initialized (and the test
      --  below also filters out deferred constants). A variable is default
      --  initialized if it does *not* have an initialization expression.

      --  Filter out cases that are not declaration of a variable from source

      if Nkind (N) /= N_Object_Declaration
        or else Constant_Present (N)
        or else not Comes_From_Source (Id)
      then
         return;
      end if;

      --  Exclude generic specs from the checks (this will get rechecked
      --  on instantiations).

      if Inside_A_Generic and then No (Enclosing_Generic_Body (Id)) then
         return;
      end if;

      --  Required checks for declaration that is in a preelaborated package
      --  and is not within some subprogram.

      if In_Preelaborated_Unit
        and then not In_Subprogram_Or_Concurrent_Unit
      then
         --  Check for default initialized variable case. Note that in
         --  accordance with (RM B.1(24)) imported objects are not subject to
         --  default initialization.
         --  If the initialization does not come from source and is an
         --  aggregate, it is a static initialization that replaces an
         --  implicit call, and must be treated as such.

         if Present (E)
           and then (Comes_From_Source (E) or else Nkind (E) /= N_Aggregate)
         then
            null;

         elsif Is_Imported (Id) then
            null;

         else
            declare
               Ent : Entity_Id := T;

            begin
               --  An array whose component type is a record with nonstatic
               --  default expressions is a violation, so we get the array's
               --  component type.

               if Is_Array_Type (Ent) then
                  declare
                     Comp_Type : Entity_Id;

                  begin
                     Comp_Type := Component_Type (Ent);
                     while Is_Array_Type (Comp_Type) loop
                        Comp_Type := Component_Type (Comp_Type);
                     end loop;

                     Ent := Comp_Type;
                  end;
               end if;

               --  Object decl. that is of record type and has no default expr.
               --  should check if there is any non-static default expression
               --  in component decl. of the record type decl.

               if Is_Record_Type (Ent) then
                  if Nkind (Parent (Ent)) = N_Full_Type_Declaration then
                     Check_Non_Static_Default_Expr
                       (Type_Definition (Parent (Ent)), N);

                  elsif Nkind (Odf) = N_Subtype_Indication
                    and then not Is_Array_Type (T)
                    and then not Is_Private_Type (T)
                  then
                     Check_Non_Static_Default_Expr (Type_Definition
                       (Parent (Entity (Subtype_Mark (Odf)))), N);
                  end if;
               end if;

               --  Check for invalid use of private object. Note that Ada 2005
               --  AI-161 modifies the rules for Ada 2005, including the use of
               --  the new pragma Preelaborable_Initialization.

               if Is_Private_Type (Ent)
                 or else Depends_On_Private (Ent)
               then
                  --  Case where type has preelaborable initialization which
                  --  means that a pragma Preelaborable_Initialization was
                  --  given for the private type.

                  if Relaxed_RM_Semantics then

                     --  In relaxed mode, do not issue these messages, this
                     --  is basically similar to the GNAT_Mode test below.

                     null;

                  elsif Has_Preelaborable_Initialization (Ent) then

                     --  But for the predefined units, we will ignore this
                     --  status unless we are in Ada 2005 mode since we want
                     --  Ada 95 compatible behavior, in which the entities
                     --  marked with this pragma in the predefined library are
                     --  not treated specially.

                     if Ada_Version < Ada_2005 then
                        Error_Msg_N
                          ("private object not allowed in preelaborated unit",
                           N);
                        Error_Msg_N ("\(would be legal in Ada 2005 mode)", N);
                     end if;

                  --  Type does not have preelaborable initialization

                  else
                     --  We allow this when compiling in GNAT mode to make life
                     --  easier for some cases where it would otherwise be hard
                     --  to be exactly valid Ada.

                     if not GNAT_Mode then
                        Error_Msg_N
                          ("private object not allowed in preelaborated unit",
                           N);

                        --  Add a message if it would help to provide a pragma
                        --  Preelaborable_Initialization on the type of the
                        --  object (which would make it legal in Ada 2005).

                        --  If the type has no full view (generic type, or
                        --  previous error), the warning does not apply.

                        if Is_Private_Type (Ent)
                          and then Present (Full_View (Ent))
                          and then
                            Has_Preelaborable_Initialization (Full_View (Ent))
                        then
                           Error_Msg_Sloc := Sloc (Ent);

                           if Ada_Version >= Ada_2005 then
                              Error_Msg_NE
                                ("\would be legal if pragma Preelaborable_" &
                                 "Initialization given for & #", N, Ent);
                           else
                              Error_Msg_NE
                                ("\would be legal in Ada 2005 if pragma " &
                                 "Preelaborable_Initialization given for & #",
                                 N, Ent);
                           end if;
                        end if;
                     end if;
                  end if;

               --  Access to Task or Protected type

               elsif Is_Entity_Name (Odf)
                 and then Present (Etype (Odf))
                 and then Is_Access_Type (Etype (Odf))
               then
                  Ent := Designated_Type (Etype (Odf));

               elsif Is_Entity_Name (Odf) then
                  Ent := Entity (Odf);

               elsif Nkind (Odf) = N_Subtype_Indication then
                  Ent := Etype (Subtype_Mark (Odf));

               elsif Nkind (Odf) = N_Constrained_Array_Definition then
                  Ent := Component_Type (T);
               end if;

               if Is_Task_Type (Ent)
                 or else (Is_Protected_Type (Ent) and then Has_Entries (Ent))
               then
                  Error_Msg_N
                    ("concurrent object not allowed in preelaborated unit",
                     N);
                  return;
               end if;
            end;
         end if;

         --  Non-static discriminants not allowed in preelaborated unit.
         --  Objects of a controlled type with a user-defined Initialize
         --  are forbidden as well.

         if Is_Record_Type (Etype (Id)) then
            declare
               ET  : constant Entity_Id := Etype (Id);
               EE  : constant Entity_Id := Etype (Etype (Id));
               PEE : Node_Id;

            begin
               if Has_Discriminants (ET) and then Present (EE) then
                  PEE := Parent (EE);

                  if Nkind (PEE) = N_Full_Type_Declaration
                    and then not Static_Discriminant_Expr
                                   (Discriminant_Specifications (PEE))
                  then
                     Error_Msg_N
                       ("non-static discriminant in preelaborated unit",
                        PEE);
                  end if;
               end if;

               --  For controlled type or type with controlled component, check
               --  preelaboration flag, as there may be a non-null Initialize
               --  primitive. For language versions earlier than Ada 2005,
               --  there is no notion of preelaborable initialization, and
               --  Validate_Controlled_Object is used to enforce rules for
               --  controlled objects.

               if (Is_Controlled (ET) or else Has_Controlled_Component (ET))
                    and then Ada_Version >= Ada_2005
                    and then not Has_Preelaborable_Initialization (ET)
               then
                  Error_Msg_NE
                    ("controlled type& does not have"
                      & " preelaborable initialization", N, ET);
               end if;
            end;

         end if;
      end if;

      --  A pure library_item must not contain the declaration of any variable
      --  except within a subprogram, generic subprogram, task unit, or
      --  protected unit (RM 10.2.1(16)).

      if In_Pure_Unit and then not In_Subprogram_Task_Protected_Unit then
         Error_Msg_N ("declaration of variable not allowed in pure unit", N);

      elsif not In_Private_Part (Id) then

         --  The visible part of an RCI library unit must not contain the
         --  declaration of a variable (RM E.1.3(9)).

         if In_RCI_Declaration then
            Error_Msg_N ("visible variable not allowed in 'R'C'I unit", N);

         --  The visible part of a Shared Passive library unit must not contain
         --  the declaration of a variable (RM E.2.2(7)).

         elsif In_RT_Declaration then
            Error_Msg_N
              ("visible variable not allowed in remote types unit", N);
         end if;
      end if;
   end Validate_Object_Declaration;

   -----------------------------
   -- Validate_RACW_Primitive --
   -----------------------------

   procedure Validate_RACW_Primitive
     (Subp : Entity_Id;
      RACW : Entity_Id)
   is
      procedure Illegal_Remote_Subp (Msg : String; N : Node_Id);
      --  Diagnose illegality on N. If RACW is present, report the error on it
      --  rather than on N.

      -------------------------
      -- Illegal_Remote_Subp --
      -------------------------

      procedure Illegal_Remote_Subp (Msg : String; N : Node_Id) is
      begin
         if Present (RACW) then
            if not Error_Posted (RACW) then
               Error_Msg_N
                 ("illegal remote access to class-wide type&", RACW);
            end if;

            Error_Msg_Sloc := Sloc (N);
            Error_Msg_NE ("\\" & Msg & " in primitive& #", RACW, Subp);

         else
            Error_Msg_NE (Msg & " in remote subprogram&", N, Subp);
         end if;
      end Illegal_Remote_Subp;

      Rtyp       : Entity_Id;
      Param      : Node_Id;
      Param_Spec : Node_Id;
      Param_Type : Entity_Id;

   --  Start of processing for Validate_RACW_Primitive

   begin
      --  Check return type

      if Ekind (Subp) = E_Function then
         Rtyp := Etype (Subp);

         --  AI05-0101 (Binding Interpretation): The result type of a remote
         --  function must either support external streaming or be a
         --  controlling access result type.

         if Has_Controlling_Result (Subp) then
            null;

         elsif Ekind (Rtyp) = E_Anonymous_Access_Type then
            Illegal_Remote_Subp ("anonymous access result", Rtyp);

         elsif Is_Limited_Type (Rtyp) then
            if No (TSS (Rtyp, TSS_Stream_Read))
                 or else
               No (TSS (Rtyp, TSS_Stream_Write))
            then
               Illegal_Remote_Subp
                 ("limited return type must have Read and Write attributes",
                     Parent (Subp));
               Explain_Limited_Type (Rtyp, Parent (Subp));
            end if;

         --  Check that the return type supports external streaming

         elsif No_External_Streaming (Rtyp)
           and then not Error_Posted (Rtyp)
         then
            Illegal_Remote_Subp ("return type containing non-remote access "
              & "must have Read and Write attributes",
              Parent (Subp));
         end if;
      end if;

      Param := First_Formal (Subp);
      while Present (Param) loop

         --  Now find out if this parameter is a controlling parameter

         Param_Spec := Parent (Param);
         Param_Type := Etype (Param);

         if Is_Controlling_Formal (Param) then

            --  It is a controlling parameter, so specific checks below do not
            --  apply.

            null;

         elsif Ekind_In (Param_Type, E_Anonymous_Access_Type,
                                     E_Anonymous_Access_Subprogram_Type)
         then
            --  From RM E.2.2(14), no anonymous access parameter other than
            --  controlling ones may be used (because an anonymous access
            --  type never supports external streaming).

            Illegal_Remote_Subp
              ("non-controlling access parameter", Param_Spec);

         elsif No_External_Streaming (Param_Type)
            and then not Error_Posted (Param_Type)
         then
            Illegal_Remote_Subp ("formal parameter in remote subprogram must "
              & "support external streaming", Param_Spec);
         end if;

         --  Check next parameter in this subprogram

         Next_Formal (Param);
      end loop;
   end Validate_RACW_Primitive;

   ------------------------------
   -- Validate_RACW_Primitives --
   ------------------------------

   procedure Validate_RACW_Primitives (T : Entity_Id) is
      Desig_Type             : Entity_Id;
      Primitive_Subprograms  : Elist_Id;
      Subprogram_Elmt        : Elmt_Id;
      Subprogram             : Entity_Id;

   begin
      Desig_Type := Etype (Designated_Type (T));

      --  No action needed for concurrent types

      if Is_Concurrent_Type (Desig_Type) then
         return;
      end if;

      Primitive_Subprograms := Primitive_Operations (Desig_Type);

      Subprogram_Elmt := First_Elmt (Primitive_Subprograms);
      while Subprogram_Elmt /= No_Elmt loop
         Subprogram := Node (Subprogram_Elmt);

         if Is_Predefined_Dispatching_Operation (Subprogram)
           or else Is_Hidden (Subprogram)
         then
            goto Next_Subprogram;
         end if;

         Validate_RACW_Primitive (Subp => Subprogram, RACW => T);

      <<Next_Subprogram>>
         Next_Elmt (Subprogram_Elmt);
      end loop;
   end Validate_RACW_Primitives;

   -------------------------------
   -- Validate_RCI_Declarations --
   -------------------------------

   procedure Validate_RCI_Declarations (P : Entity_Id) is
      E : Entity_Id;

   begin
      E := First_Entity (P);
      while Present (E) loop
         if Comes_From_Source (E) then
            if Is_Limited_Type (E) then
               Error_Msg_N
                 ("limited type not allowed in rci unit", Parent (E));
               Explain_Limited_Type (E, Parent (E));

            elsif Ekind_In (E, E_Generic_Function,
                               E_Generic_Package,
                               E_Generic_Procedure)
            then
               Error_Msg_N ("generic declaration not allowed in rci unit",
                 Parent (E));

            elsif (Ekind (E) = E_Function or else Ekind (E) = E_Procedure)
              and then Has_Pragma_Inline (E)
            then
               Error_Msg_N
                 ("inlined subprogram not allowed in rci unit", Parent (E));

            --  Inner packages that are renamings need not be checked. Generic
            --  RCI packages are subject to the checks, but entities that come
            --  from formal packages are not part of the visible declarations
            --  of the package and are not checked.

            elsif Ekind (E) = E_Package then
               if Present (Renamed_Entity (E)) then
                  null;

               elsif Ekind (P) /= E_Generic_Package
                 or else List_Containing (Unit_Declaration_Node (E)) /=
                           Generic_Formal_Declarations
                             (Unit_Declaration_Node (P))
               then
                  Validate_RCI_Declarations (E);
               end if;
            end if;
         end if;

         Next_Entity (E);
      end loop;
   end Validate_RCI_Declarations;

   -----------------------------------------
   -- Validate_RCI_Subprogram_Declaration --
   -----------------------------------------

   procedure Validate_RCI_Subprogram_Declaration (N : Node_Id) is
      K               : constant Node_Kind := Nkind (N);
      Profile         : List_Id;
      Id              : constant Entity_Id := Defining_Entity (N);
      Param_Spec      : Node_Id;
      Param_Type      : Entity_Id;
      Error_Node      : Node_Id := N;

   begin
      --  This procedure enforces rules on subprogram and access to subprogram
      --  declarations in RCI units. These rules do not apply to expander
      --  generated routines, which are not remote subprograms. It is called:

      --    1. from Analyze_Subprogram_Declaration.
      --    2. from Validate_Object_Declaration (access to subprogram).

      if not (Comes_From_Source (N)
                and then In_RCI_Declaration
                and then not In_Private_Part (Scope (Id)))
      then
         return;
      end if;

      if K = N_Subprogram_Declaration then
         Profile := Parameter_Specifications (Specification (N));

      else
         pragma Assert (K = N_Object_Declaration);

         --  The above assertion is dubious, the visible declarations of an
         --  RCI unit never contain an object declaration, this should be an
         --  ACCESS-to-object declaration???

         if Nkind (Id) = N_Defining_Identifier
           and then Nkind (Parent (Etype (Id))) = N_Full_Type_Declaration
           and then Ekind (Etype (Id)) = E_Access_Subprogram_Type
         then
            Profile :=
              Parameter_Specifications (Type_Definition (Parent (Etype (Id))));
         else
            return;
         end if;
      end if;

      --  Iterate through the parameter specification list, checking that
      --  no access parameter and no limited type parameter in the list.
      --  RM E.2.3(14).

      if Present (Profile) then
         Param_Spec := First (Profile);
         while Present (Param_Spec) loop
            Param_Type := Etype (Defining_Identifier (Param_Spec));

            if Ekind (Param_Type) = E_Anonymous_Access_Type then
               if K = N_Subprogram_Declaration then
                  Error_Node := Param_Spec;
               end if;

               --  Report error only if declaration is in source program

               if Comes_From_Source (Id) then
                  Error_Msg_N
                    ("subprogram in 'R'C'I unit cannot have access parameter",
                     Error_Node);
               end if;

            --  For a limited private type parameter, we check only the private
            --  declaration and ignore full type declaration, unless this is
            --  the only declaration for the type, e.g., as a limited record.

            elsif No_External_Streaming (Param_Type) then
               if K = N_Subprogram_Declaration then
                  Error_Node := Param_Spec;
               end if;

               Error_Msg_NE
                 ("formal of remote subprogram& "
                  & "must support external streaming",
                  Error_Node, Id);
               if Is_Limited_Type (Param_Type) then
                  Explain_Limited_Type (Param_Type, Error_Node);
               end if;
            end if;

            Next (Param_Spec);
         end loop;
      end if;

      if Ekind (Id) = E_Function
        and then Ekind (Etype (Id)) = E_Anonymous_Access_Type
        and then Comes_From_Source (Id)
      then
         Error_Msg_N
           ("function in 'R'C'I unit cannot have access result",
             Error_Node);
      end if;
   end Validate_RCI_Subprogram_Declaration;

   ----------------------------------------------------
   -- Validate_Remote_Access_Object_Type_Declaration --
   ----------------------------------------------------

   procedure Validate_Remote_Access_Object_Type_Declaration (T : Entity_Id) is
      Direct_Designated_Type : Entity_Id;
      Desig_Type             : Entity_Id;

   begin
      --  We are called from Analyze_Full_Type_Declaration, and the Nkind of
      --  the given node is N_Access_To_Object_Definition.

      if not Comes_From_Source (T)
        or else (not In_RCI_Declaration and then not In_RT_Declaration)
      then
         return;
      end if;

      --  An access definition in the private part of a package is not a
      --  remote access type. Restrictions related to external streaming
      --  support for non-remote access types are enforced elsewhere. Note
      --  that In_Private_Part is never set on type entities: check flag
      --  on enclosing scope.

      if In_Private_Part (Scope (T)) then
         return;
      end if;

      --  Check RCI or RT unit type declaration. It may not contain the
      --  declaration of an access-to-object type unless it is a general access
      --  type that designates a class-wide limited private type or subtype.
      --  There are also constraints on the primitive subprograms of the
      --  class-wide type (RM E.2.2(14), see Validate_RACW_Primitives).

      if Ekind (T) /= E_General_Access_Type
        or else not Is_Class_Wide_Type (Designated_Type (T))
      then
         if In_RCI_Declaration then
            Error_Msg_N
              ("error in access type in Remote_Call_Interface unit", T);
         else
            Error_Msg_N
              ("error in access type in Remote_Types unit", T);
         end if;

         Error_Msg_N ("\must be general access to class-wide type", T);
         return;
      end if;

      Direct_Designated_Type := Designated_Type (T);
      Desig_Type := Etype (Direct_Designated_Type);

      --  Why is this check not in Validate_Remote_Access_To_Class_Wide_Type???

      if not Is_Valid_Remote_Object_Type (Desig_Type) then
         Error_Msg_N
           ("error in designated type of remote access to class-wide type", T);
         Error_Msg_N
           ("\must be tagged limited private or private extension", T);
         return;
      end if;
   end Validate_Remote_Access_Object_Type_Declaration;

   -----------------------------------------------
   -- Validate_Remote_Access_To_Class_Wide_Type --
   -----------------------------------------------

   procedure Validate_Remote_Access_To_Class_Wide_Type (N : Node_Id) is
      K  : constant Node_Kind := Nkind (N);
      PK : constant Node_Kind := Nkind (Parent (N));
      E  : Entity_Id;

   begin
      --  This subprogram enforces the checks in (RM E.2.2(8)) for certain uses
      --  of class-wide limited private types.

      --    Storage_Pool and Storage_Size are not defined for such types
      --
      --    The expected type of allocator must not be such a type.

      --    The actual parameter of generic instantiation must not be such a
      --    type if the formal parameter is of an access type.

      --  On entry, there are several cases:

      --    1. called from sem_attr Analyze_Attribute where attribute name is
      --       either Storage_Pool or Storage_Size.

      --    2. called from exp_ch4 Expand_N_Allocator

      --    3. called from sem_ch4 Analyze_Explicit_Dereference

      --    4. called from sem_res Resolve_Actuals

      if K = N_Attribute_Reference then
         E := Etype (Prefix (N));

         if Is_Remote_Access_To_Class_Wide_Type (E) then
            Error_Msg_N ("incorrect attribute of remote operand", N);
            return;
         end if;

      elsif K = N_Allocator then
         E := Etype (N);

         if Is_Remote_Access_To_Class_Wide_Type (E) then
            Error_Msg_N ("incorrect expected remote type of allocator", N);
            return;
         end if;

      --  This subprogram also enforces the checks in E.2.2(13). A value of
      --  such type must not be dereferenced unless as controlling operand of
      --  a dispatching call. Explicit dereferences not coming from source are
      --  exempted from this checking because the expander produces them in
      --  some cases (such as for tag checks on dispatching calls with multiple
      --  controlling operands). However we do check in the case of an implicit
      --  dereference that is expanded to an explicit dereference (hence the
      --  test of whether Original_Node (N) comes from source).

      elsif K = N_Explicit_Dereference
        and then Comes_From_Source (Original_Node (N))
      then
         E := Etype (Prefix (N));

         --  If the class-wide type is not a remote one, the restrictions
         --  do not apply.

         if not Is_Remote_Access_To_Class_Wide_Type (E) then
            return;
         end if;

         --  If we have a true dereference that comes from source and that
         --  is a controlling argument for a dispatching call, accept it.

         if Is_Actual_Parameter (N) and then Is_Controlling_Actual (N) then
            return;
         end if;

         --  If we are just within a procedure or function call and the
         --  dereference has not been analyzed, return because this procedure
         --  will be called again from sem_res Resolve_Actuals. The same can
         --  apply in the case of dereference that is the prefix of a selected
         --  component, which can be a call given in prefixed form.

         if (Is_Actual_Parameter (N) or else PK = N_Selected_Component)
           and then not Analyzed (N)
         then
            return;
         end if;

         --  We must allow expanded code to generate a reference to the tag of
         --  the designated object (may be either the actual tag, or the stub
         --  tag in the case of a remote object).

         if PK = N_Selected_Component
           and then Is_Tag (Entity (Selector_Name (Parent (N))))
         then
            return;
         end if;

         Error_Msg_N
           ("invalid dereference of a remote access-to-class-wide value", N);
      end if;
   end Validate_Remote_Access_To_Class_Wide_Type;

   ------------------------------------------
   -- Validate_Remote_Type_Type_Conversion --
   ------------------------------------------

   procedure Validate_Remote_Type_Type_Conversion (N : Node_Id) is
      S : constant Entity_Id := Etype (N);
      E : constant Entity_Id := Etype (Expression (N));

   begin
      --  This test is required in the case where a conversion appears inside a
      --  normal package, it does not necessarily have to be inside an RCI,
      --  Remote_Types unit (RM E.2.2(9,12)).

      if Is_Remote_Access_To_Subprogram_Type (E)
        and then not Is_Remote_Access_To_Subprogram_Type (S)
      then
         Error_Msg_N
           ("incorrect conversion of remote operand to local type", N);
         return;

      elsif not Is_Remote_Access_To_Subprogram_Type (E)
        and then Is_Remote_Access_To_Subprogram_Type (S)
      then
         Error_Msg_N
           ("incorrect conversion of local operand to remote type", N);
         return;

      elsif Is_Remote_Access_To_Class_Wide_Type (E)
        and then not Is_Remote_Access_To_Class_Wide_Type (S)
      then
         Error_Msg_N
           ("incorrect conversion of remote operand to local type", N);
         return;
      end if;

      --  If a local access type is converted into a RACW type, then the
      --  current unit has a pointer that may now be exported to another
      --  partition.

      if Is_Remote_Access_To_Class_Wide_Type (S)
        and then not Is_Remote_Access_To_Class_Wide_Type (E)
      then
         Set_Has_RACW (Current_Sem_Unit);
      end if;
   end Validate_Remote_Type_Type_Conversion;

   -------------------------------
   -- Validate_RT_RAT_Component --
   -------------------------------

   procedure Validate_RT_RAT_Component (N : Node_Id) is
      Spec           : constant Node_Id   := Specification (N);
      Name_U         : constant Entity_Id := Defining_Entity (Spec);
      Typ            : Entity_Id;
      U_Typ          : Entity_Id;
      First_Priv_Ent : constant Entity_Id := First_Private_Entity (Name_U);

      function Stream_Attributes_Available (Typ : Entity_Id) return Boolean;
      --  True if any stream attribute is available for Typ

      ---------------------------------
      -- Stream_Attributes_Available --
      ---------------------------------

      function Stream_Attributes_Available (Typ : Entity_Id) return Boolean
      is
      begin
         return Stream_Attribute_Available (Typ, TSS_Stream_Read)
                  or else
                Stream_Attribute_Available (Typ, TSS_Stream_Write)
                  or else
                Stream_Attribute_Available (Typ, TSS_Stream_Input)
                  or else
                Stream_Attribute_Available (Typ, TSS_Stream_Output);
      end Stream_Attributes_Available;

   --  Start of processing for Validate_RT_RAT_Component

   begin
      if not Is_Remote_Types (Name_U) then
         return;
      end if;

      Typ := First_Entity (Name_U);
      while Present (Typ) and then Typ /= First_Priv_Ent loop
         U_Typ := Underlying_Type (Base_Type (Typ));

         if No (U_Typ) then
            U_Typ := Typ;
         end if;

         if Comes_From_Source (Typ) and then Is_Type (Typ)
           and then Ekind (Typ) /= E_Incomplete_Type
         then
            --  Check that the type can be meaningfully transmitted to another
            --  partition (E.2.2(8)).

            if (Ada_Version < Ada_2005 and then Has_Non_Remote_Access (U_Typ))
                 or else (Stream_Attributes_Available (Typ)
                           and then No_External_Streaming (U_Typ))
            then
               if Is_Non_Remote_Access_Type (Typ) then
                  Error_Msg_N ("error in non-remote access type", U_Typ);
               else
                  Error_Msg_N
                    ("error in record type containing a component of a " &
                     "non-remote access type", U_Typ);
               end if;

               if Ada_Version >= Ada_2005 then
                  Error_Msg_N
                    ("\must have visible Read and Write attribute " &
                     "definition clauses (RM E.2.2(8))", U_Typ);
               else
                  Error_Msg_N
                    ("\must have Read and Write attribute " &
                     "definition clauses (RM E.2.2(8))", U_Typ);
               end if;
            end if;
         end if;

         Next_Entity (Typ);
      end loop;
   end Validate_RT_RAT_Component;

   -----------------------------------------
   -- Validate_SP_Access_Object_Type_Decl --
   -----------------------------------------

   procedure Validate_SP_Access_Object_Type_Decl (T : Entity_Id) is
      Direct_Designated_Type : Entity_Id;

      function Has_Entry_Declarations (E : Entity_Id) return Boolean;
      --  Return true if the protected type designated by T has entry
      --  declarations.

      ----------------------------
      -- Has_Entry_Declarations --
      ----------------------------

      function Has_Entry_Declarations (E : Entity_Id) return Boolean is
         Ety : Entity_Id;

      begin
         if Nkind (Parent (E)) = N_Protected_Type_Declaration then
            Ety := First_Entity (E);
            while Present (Ety) loop
               if Ekind (Ety) = E_Entry then
                  return True;
               end if;

               Next_Entity (Ety);
            end loop;
         end if;

         return False;
      end Has_Entry_Declarations;

   --  Start of processing for Validate_SP_Access_Object_Type_Decl

   begin
      --  We are called from Sem_Ch3.Analyze_Full_Type_Declaration, and the
      --  Nkind of the given entity is N_Access_To_Object_Definition.

      if not Comes_From_Source (T)
        or else not In_Shared_Passive_Unit
        or else In_Subprogram_Task_Protected_Unit
      then
         return;
      end if;

      --  Check Shared Passive unit. It should not contain the declaration
      --  of an access-to-object type whose designated type is a class-wide
      --  type, task type or protected type with entry (RM E.2.1(7)).

      Direct_Designated_Type := Designated_Type (T);

      if Ekind (Direct_Designated_Type) = E_Class_Wide_Type then
         Error_Msg_N
           ("invalid access-to-class-wide type in shared passive unit", T);
         return;

      elsif Ekind (Direct_Designated_Type) in Task_Kind then
         Error_Msg_N
           ("invalid access-to-task type in shared passive unit", T);
         return;

      elsif Ekind (Direct_Designated_Type) in Protected_Kind
        and then Has_Entry_Declarations (Direct_Designated_Type)
      then
         Error_Msg_N
           ("invalid access-to-protected type in shared passive unit", T);
         return;
      end if;
   end Validate_SP_Access_Object_Type_Decl;

   ---------------------------------
   -- Validate_Static_Object_Name --
   ---------------------------------

   procedure Validate_Static_Object_Name (N : Node_Id) is
      E   : Entity_Id;
      Val : Node_Id;

      function Is_Primary (N : Node_Id) return Boolean;
      --  Determine whether node is syntactically a primary in an expression
      --  This function should probably be somewhere else ???
      --
      --  Also it does not do what it says, e.g if N is a binary operator
      --  whose parent is a binary operator, Is_Primary returns True ???

      ----------------
      -- Is_Primary --
      ----------------

      function Is_Primary (N : Node_Id) return Boolean is
         K : constant Node_Kind := Nkind (Parent (N));

      begin
         case K is
            when N_Aggregate
               | N_Component_Association
               | N_Index_Or_Discriminant_Constraint
               | N_Membership_Test
               | N_Op
            =>
               return True;

            when N_Attribute_Reference =>
               declare
                  Attr : constant Name_Id := Attribute_Name (Parent (N));

               begin
                  return     Attr /= Name_Address
                    and then Attr /= Name_Access
                    and then Attr /= Name_Unchecked_Access
                    and then Attr /= Name_Unrestricted_Access;
               end;

            when N_Indexed_Component =>
               return N /= Prefix (Parent (N)) or else Is_Primary (Parent (N));

            when N_Qualified_Expression
               | N_Type_Conversion
            =>
               return Is_Primary (Parent (N));

            when N_Assignment_Statement
               | N_Object_Declaration
            =>
               return N = Expression (Parent (N));

            when N_Selected_Component =>
               return Is_Primary (Parent (N));

            when others =>
               return False;
         end case;
      end Is_Primary;

   --  Start of processing for Validate_Static_Object_Name

   begin
      if not In_Preelaborated_Unit
        or else not Comes_From_Source (N)
        or else In_Subprogram_Or_Concurrent_Unit
        or else Ekind (Current_Scope) = E_Block
      then
         return;

      --  Filter out cases where primary is default in a component declaration,
      --  discriminant specification, or actual in a record type initialization
      --  call.

      --  Initialization call of internal types

      elsif Nkind (Parent (N)) = N_Procedure_Call_Statement then

         if Present (Parent (Parent (N)))
           and then Nkind (Parent (Parent (N))) = N_Freeze_Entity
         then
            return;
         end if;

         if Nkind (Name (Parent (N))) = N_Identifier
           and then not Comes_From_Source (Entity (Name (Parent (N))))
         then
            return;
         end if;
      end if;

      --  Error if the name is a primary in an expression. The parent must not
      --  be an operator, or a selected component or an indexed component that
      --  is itself a primary. Entities that are actuals do not need to be
      --  checked, because the call itself will be diagnosed. Entities in a
      --  generic unit or within a preanalyzed expression are not checked:
      --  only their use in executable code matters.

      if Is_Primary (N)
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (N)))
        and then not In_Spec_Expression
      then
         if Ekind (Entity (N)) = E_Variable
           or else Ekind (Entity (N)) in Formal_Object_Kind
         then
            Flag_Non_Static_Expr
              ("non-static object name in preelaborated unit", N);

         --  Give an error for a reference to a nonstatic constant, unless the
         --  constant is in another GNAT library unit that is preelaborable.

         elsif Ekind (Entity (N)) = E_Constant
           and then not Is_Static_Expression (N)
         then
            E   := Entity (N);
            Val := Constant_Value (E);

            if In_Internal_Unit (N)
              and then
                Enclosing_Comp_Unit_Node (N) /= Enclosing_Comp_Unit_Node (E)
              and then (Is_Preelaborated (Scope (E))
                         or else Is_Pure (Scope (E))
                         or else (Present (Renamed_Object (E))
                                   and then Is_Entity_Name (Renamed_Object (E))
                                   and then
                                     (Is_Preelaborated
                                        (Scope (Renamed_Object (E)))
                                       or else
                                         Is_Pure
                                           (Scope (Renamed_Object (E))))))
            then
               null;

            --  If the value of the constant is a local variable that renames
            --  an aggregate, this is in itself legal. The aggregate may be
            --  expanded into a loop, but this does not affect preelaborability
            --  in itself. If some aggregate components are non-static, that is
            --  to say if they involve non static primaries, they will be
            --  flagged when analyzed.

            elsif Present (Val)
              and then Is_Entity_Name (Val)
              and then Is_Array_Type (Etype (Val))
              and then not Comes_From_Source (Val)
              and then Nkind (Original_Node (Val)) = N_Aggregate
            then
               null;

            --  This is the error case

            else
               --  In GNAT mode or Relaxed RM Semantic mode, this is just a
               --  warning, to allow it to be judiciously turned off.
               --  Otherwise it is a real error.

               if GNAT_Mode or Relaxed_RM_Semantics then
                  Error_Msg_N
                    ("??non-static constant in preelaborated unit", N);
               else
                  Flag_Non_Static_Expr
                    ("non-static constant in preelaborated unit", N);
               end if;
            end if;
         end if;
      end if;
   end Validate_Static_Object_Name;

end Sem_Cat;
