------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C A T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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
with Exp_Tss;  use Exp_Tss;
with Fname;    use Fname;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Sem;      use Sem;
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

   --  Iterate through the component list of a record definition, check
   --  that no component is declared with a non-static default value.

   function Missing_Read_Write_Attributes (E : Entity_Id) return Boolean;
   --  Return True if the entity or one of its subcomponent is an access
   --  type which does not have user-defined Read and Write attribute.

   function In_RCI_Declaration (N : Node_Id) return Boolean;
   --  Determines if a declaration is  within the visible part of  a Remote
   --  Call Interface compilation unit, for semantic checking purposes only,
   --  (returns false within an instance and within the package body).

   function In_RT_Declaration return Boolean;
   --  Determines if current scope is within a Remote Types compilation unit,
   --  for semantic checking purposes.

   function Is_Non_Remote_Access_Type (E : Entity_Id) return Boolean;
   --  Returns true if the entity is a non-remote access type

   function In_Shared_Passive_Unit return Boolean;
   --  Determines if current scope is within a Shared Passive compilation unit

   function Static_Discriminant_Expr (L : List_Id) return Boolean;
   --  Iterate through the list of discriminants to check if any of them
   --  contains non-static default expression, which is a violation in
   --  a preelaborated library unit.

   procedure Validate_Remote_Access_Object_Type_Declaration (T : Entity_Id);
   --  Check validity of declaration if RCI or RT unit. It should not contain
   --  the declaration of an access-to-object type unless it is a
   --  general access type that designates a class-wide limited
   --  private type. There are also constraints about the primitive
   --  subprograms of the class-wide type. RM E.2 (9, 13, 14)

   function Is_Recursively_Limited_Private (E : Entity_Id) return Boolean;
   --  Return True if E is a limited private type, or if E is a private
   --  extension of a type whose parent verifies this property (hence the
   --  recursive keyword).

   ---------------------------------------
   -- Check_Categorization_Dependencies --
   ---------------------------------------

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id;
      Is_Subunit      : Boolean)
   is
      N : constant Node_Id := Info_Node;

      type Categorization is
         (Pure, Shared_Passive, Remote_Types,
           Remote_Call_Interface, Pre_Elaborated, Normal);

      Unit_Category : Categorization;
      With_Category : Categorization;

      function Get_Categorization (E : Entity_Id) return Categorization;
      --  Check categorization flags from entity, and return in the form
      --  of a corresponding enumeration value.

      ------------------------
      -- Get_Categorization --
      ------------------------

      function Get_Categorization (E : Entity_Id) return Categorization is
      begin
         if Is_Preelaborated (E) then
            return Pre_Elaborated;
         elsif Is_Pure (E) then
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

   --  Start of processing for Check_Categorization_Dependencies

   begin
      --  Intrinsic subprograms are preelaborated, so do not impose any
      --  categorization dependencies.

      if Is_Intrinsic_Subprogram (Depended_Entity) then
         return;
      end if;

      Unit_Category := Get_Categorization (Unit_Entity);
      With_Category := Get_Categorization (Depended_Entity);

      if With_Category > Unit_Category then

         if (Unit_Category = Remote_Types
                 or else Unit_Category = Remote_Call_Interface)
           and then In_Package_Body (Unit_Entity)
         then
            null;

         elsif Is_Subunit then
            Error_Msg_NE ("subunit cannot depend on&"
              & " (parent has wrong categorization)", N, Depended_Entity);
         else
            Error_Msg_NE ("current unit cannot depend on&"
              & " (wrong categorization)", N, Depended_Entity);
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
           and then not Is_Static_Expression (Expression (Component_Decl))
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
   -- In_Preelaborated_Unit --
   ---------------------------

   function In_Preelaborated_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no constraints on body of remote_call_interface or
      --  remote_types packages..

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

   function In_RCI_Declaration (N : Node_Id) return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the private part or body
      --  of an RCI unit.

      return Is_Remote_Call_Interface (Unit_Entity)
        and then (Ekind (Unit_Entity) = E_Package
                  or else Ekind (Unit_Entity) = E_Generic_Package)
        and then Unit_Kind /= N_Package_Body
        and then List_Containing (N) =
                  Visible_Declarations
                    (Specification (Unit_Declaration_Node (Unit_Entity)))
        and then not In_Package_Body (Unit_Entity)
        and then not In_Instance;
   end In_RCI_Declaration;

   -----------------------
   -- In_RT_Declaration --
   -----------------------

   function In_RT_Declaration return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the body of a Remote Types unit.

      return Is_Remote_Types (Unit_Entity)
        and then (Ekind (Unit_Entity) = E_Package
                   or else Ekind (Unit_Entity) = E_Generic_Package)
        and then Unit_Kind /= N_Package_Body
        and then not In_Package_Body (Unit_Entity)
        and then not In_Instance;
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
         if Is_Subprogram (E)
              or else
            Is_Generic_Subprogram (E)
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
   begin
      return Is_Access_Type (E)
        and then not Is_Remote_Access_To_Class_Wide_Type (E)
        and then not Is_Remote_Access_To_Subprogram_Type (E);
   end Is_Non_Remote_Access_Type;

   ------------------------------------
   -- Is_Recursively_Limited_Private --
   ------------------------------------

   function Is_Recursively_Limited_Private (E : Entity_Id) return Boolean is
      P : constant Node_Id := Parent (E);

   begin
      if Nkind (P) = N_Private_Type_Declaration
        and then Is_Limited_Record (E)
      then
         return True;
      elsif Nkind (P) = N_Private_Extension_Declaration then
         return Is_Recursively_Limited_Private (Etype (E));
      elsif Nkind (P) = N_Formal_Type_Declaration
        and then Ekind (E) = E_Record_Type_With_Private
        and then Is_Generic_Type (E)
        and then Is_Limited_Record (E)
      then
         return True;
      else
         return False;
      end if;
   end Is_Recursively_Limited_Private;

   ----------------------------------
   -- Missing_Read_Write_Attribute --
   ----------------------------------

   function Missing_Read_Write_Attributes (E : Entity_Id) return Boolean is
      Component      : Entity_Id;
      Component_Type : Entity_Id;

      function Has_Read_Write_Attributes (E : Entity_Id) return Boolean;
      --  Return True if entity has Read and Write attributes

      -------------------------------
      -- Has_Read_Write_Attributes --
      -------------------------------

      function Has_Read_Write_Attributes (E : Entity_Id) return Boolean is
         Rep_Item        : Node_Id := First_Rep_Item (E);
         Read_Attribute  : Boolean := False;
         Write_Attribute : Boolean := False;

      begin
         --  We start from the declaration node and then loop until the end
         --  of the list until we find those two attribute definition clauses.

         while Present (Rep_Item) loop
            if Chars (Rep_Item) = Name_Read then
               Read_Attribute := True;
            elsif Chars (Rep_Item) = Name_Write then
               Write_Attribute := True;
            end if;

            if Read_Attribute and Write_Attribute then
               return True;
            end if;

            Next_Rep_Item (Rep_Item);
         end loop;

         return False;
      end Has_Read_Write_Attributes;

   --  Start of processing for Missing_Read_Write_Attributes

   begin
      if Has_Read_Write_Attributes (E) then
         return False;
      elsif Is_Non_Remote_Access_Type (E) then
         return True;
      end if;

      if Is_Record_Type (E) then
         Component := First_Entity (E);
         while Present (Component) loop
            Component_Type := Etype (Component);

            if (Is_Non_Remote_Access_Type (Component_Type)
                or else Is_Record_Type (Component_Type))
              and then Missing_Read_Write_Attributes (Component_Type)
            then
               return True;
            end if;

            Next_Entity (Component);
         end loop;
      end if;

      return False;
   end Missing_Read_Write_Attributes;

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

      procedure Set_Parents (Visibility : Boolean) is
         Par : Entity_Id := Scope (S);

      begin
         while Present (Par) and then Par /= Standard_Standard loop
            Set_Is_Immediately_Visible (Par, Visibility);
            Par := Scope (Par);
         end loop;
      end Set_Parents;

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
         PN : Node_Id := First (Pragmas_After (Aux_Decls_Node (P)));

      begin

         if Is_Child_Unit (S)
           and then Is_Generic_Instance (S)
         then
            Set_Parents (True);
         end if;

         while Present (PN) loop

            --  Skip implicit types that may have been introduced by
            --  previous analysis.

            if Nkind (PN) = N_Pragma then

               case Get_Pragma_Id (Chars (PN)) is
                  when Pragma_All_Calls_Remote   |
                    Pragma_Preelaborate          |
                    Pragma_Pure                  |
                    Pragma_Remote_Call_Interface |
                    Pragma_Remote_Types          |
                    Pragma_Shared_Passive        => Analyze (PN);
                  when others                    => null;
               end case;
            end if;

            Next (PN);
         end loop;
         if Is_Child_Unit (S)
           and then Is_Generic_Instance (S)
         then
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
      Set_Is_Pure (E,
        Is_Pure (Scop) and then Is_Library_Level_Entity (E));

      if not Is_Remote_Call_Interface (E) then
         if Ekind (E) in Subprogram_Kind then
            Declaration := Unit_Declaration_Node (E);

            if False
              or else Nkind (Declaration) = N_Subprogram_Body
              or else Nkind (Declaration) = N_Subprogram_Renaming_Declaration
            then
               Specification := Corresponding_Spec (Declaration);
            end if;
         end if;

         --  A subprogram body or renaming-as-body is a remote call
         --  interface if it serves as the completion of a subprogram
         --  declaration that is a remote call interface.

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

      Set_Is_Remote_Types (E, Is_Remote_Types (Scop));
   end Set_Categorization_From_Scope;

   ------------------------------
   -- Static_Discriminant_Expr --
   ------------------------------

   --  We need to accomodate a Why_Not_Static call somehow here ???

   function Static_Discriminant_Expr (L : List_Id) return Boolean is
      Discriminant_Spec : Node_Id;

   begin
      Discriminant_Spec := First (L);
      while Present (Discriminant_Spec) loop
         if Present (Expression (Discriminant_Spec))
           and then not Is_Static_Expression (Expression (Discriminant_Spec))
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
         when N_Access_To_Subprogram_Definition =>

            --  A pure library_item must not contain the declaration of a
            --  named access type, except within a subprogram, generic
            --  subprogram, task unit, or protected unit (RM 10.2.1(16)).

            if Comes_From_Source (T)
               and then In_Pure_Unit
               and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N ("named access type not allowed in pure unit", T);
            end if;

         when N_Access_To_Object_Definition =>

            if Comes_From_Source (T)
              and then In_Pure_Unit
              and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N
                 ("named access type not allowed in pure unit", T);
            end if;

            --  Check for RCI or RT unit type declaration. It should not
            --  contain the declaration of an access-to-object type unless it
            --  is a general access type that designates a class-wide limited
            --  private type. There are also constraints about the primitive
            --  subprograms of the class-wide type.

            Validate_Remote_Access_Object_Type_Declaration (T);

            --  Check for shared passive unit type declaration. It should
            --  not contain the declaration of access to class wide type,
            --  access to task type and access to protected type with entry.

            Validate_SP_Access_Object_Type_Decl (T);

         when others => null;
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
         --  We relax the restriction of 10.2.1(9) within GNAT
         --  units to allow packages such as Ada.Strings.Unbounded
         --  to be implemented (i.p., Null_Unbounded_String).
         --  (There are ACVC tests that check that the restriction
         --  is enforced, but note that AI-161, once approved,
         --  will relax the restriction prohibiting default-
         --  initialized objects of private and controlled
         --  types.)

         if Is_Private_Type (T)
           and then not Is_Internal_File_Name
                          (Unit_File_Name (Get_Source_Unit (N)))
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

      --  Body of RCI unit does not need validation.

      if Is_Remote_Call_Interface (E)
        and then (Nkind (N) = N_Package_Body
                   or else Nkind (N) = N_Subprogram_Body)
      then
         return;
      end if;

      --  Ada0Y (AI-50217): Process explicit with_clauses that are not limited

      declare
         Item             : Node_Id;
         Entity_Of_Withed : Entity_Id;

      begin
         Item := First (Context_Items (P));

         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then not (Implicit_With (Item)
                              or else Limited_Present (Item))
            then
               Entity_Of_Withed := Entity (Name (Item));
               Check_Categorization_Dependencies
                (U, Entity_Of_Withed, Item, Is_Subunit);
            end if;

            Next (Item);
         end loop;
      end;

      --  Child depends on parent; therefore parent should also
      --  be categorized and satify the dependency hierarchy.

      --  Check if N is a child spec.

      if (K in N_Generic_Declaration              or else
          K in N_Generic_Instantiation            or else
          K in N_Generic_Renaming_Declaration     or else
          K =  N_Package_Declaration              or else
          K =  N_Package_Renaming_Declaration     or else
          K =  N_Subprogram_Declaration           or else
          K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (N))
      then
         declare
            Parent_Lib_U  : constant Node_Id   := Parent_Spec (N);
            Parent_Kind   : constant Node_Kind :=
                              Nkind (Unit (Parent_Lib_U));
            Parent_Entity : Entity_Id;

         begin
            if        Parent_Kind =  N_Package_Instantiation
              or else Parent_Kind =  N_Procedure_Instantiation
              or else Parent_Kind =  N_Function_Instantiation
              or else Parent_Kind =  N_Package_Renaming_Declaration
              or else Parent_Kind in N_Generic_Renaming_Declaration
            then
               Parent_Entity := Defining_Entity (Unit (Parent_Lib_U));

            else
               Parent_Entity :=
                 Defining_Entity (Specification (Unit (Parent_Lib_U)));
            end if;

            Check_Categorization_Dependencies (E, Parent_Entity, N, False);

            --  Verify that public child of an RCI library unit
            --  must also be an RCI library unit (RM E.2.3(15)).

            if Is_Remote_Call_Interface (Parent_Entity)
              and then not Private_Present (P)
              and then not Is_Remote_Call_Interface (E)
            then
               Error_Msg_N
                 ("public child of rci unit must also be rci unit", N);
               return;
            end if;
         end;
      end if;

   end Validate_Categorization_Dependency;

   --------------------------------
   -- Validate_Controlled_Object --
   --------------------------------

   procedure Validate_Controlled_Object (E : Entity_Id) is
   begin
      --  For now, never apply this check for internal GNAT units, since we
      --  have a number of cases in the library where we are stuck with objects
      --  of this type, and the RM requires Preelaborate.

      --  For similar reasons, we only do this check for source entities, since
      --  we generate entities of this type in some situations.

      --  Note that the 10.2.1(9) restrictions are not relevant to us anyway.
      --  We have to enforce them for RM compatibility, but we have no trouble
      --  accepting these objects and doing the right thing. Note that there is
      --  no requirement that Preelaborate not actually generate any code!

      if In_Preelaborated_Unit
        and then not Debug_Flag_PP
        and then Comes_From_Source (E)
        and then not
          Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (E)))
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
               Error_Msg_N
                 ("statements not allowed in preelaborated unit", Item);
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

      if Inside_A_Generic
        and then not Present (Enclosing_Generic_Body (Id))
      then
         return;
      end if;

      --  Required checks for declaration that is in a preelaborated
      --  package and is not within some subprogram.

      if In_Preelaborated_Unit
        and then not In_Subprogram_Or_Concurrent_Unit
      then
         --  Check for default initialized variable case. Note that in
         --  accordance with (RM B.1(24)) imported objects are not
         --  subject to default initialization.

         if No (E) and then not Is_Imported (Id) then
            declare
               Ent : Entity_Id := T;

            begin
               --  An array whose component type is a record with nonstatic
               --  default expressions is a violation, so we get the array's
               --  component type.

               if Is_Array_Type (Ent) then
                  declare
                     Comp_Type : Entity_Id := Component_Type (Ent);

                  begin
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

               --  We relax the restriction of 10.2.1(9) within GNAT
               --  units. (There are ACVC tests that check that the
               --  restriction is enforced, but note that AI-161,
               --  once approved, will relax the restriction prohibiting
               --  default-initialized objects of private types, and
               --  will recommend a pragma for marking private types.)

               if (Is_Private_Type (Ent)
                    or else Depends_On_Private (Ent))
                 and then not Is_Internal_File_Name
                                (Unit_File_Name (Get_Source_Unit (N)))
               then
                  Error_Msg_N
                    ("private object not allowed in preelaborated unit", N);
                  return;

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

               elsif
                  Nkind (Odf) = N_Constrained_Array_Definition
               then
                  Ent := Component_Type (T);

               --  else
               --     return;
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

         --  Non-static discriminant not allowed in preelaborayted unit

         if Is_Record_Type (Etype (Id)) then
            declare
               ET  : constant Entity_Id := Etype (Id);
               EE  : constant Entity_Id := Etype (Etype (Id));
               PEE : Node_Id;

            begin
               if Has_Discriminants (ET)
                 and then Present (EE)
               then
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
            end;
         end if;
      end if;

      --  A pure library_item must not contain the declaration of any
      --  variable except within  a subprogram, generic subprogram, task
      --  unit or protected unit (RM 10.2.1(16)).

      if In_Pure_Unit
        and then not In_Subprogram_Task_Protected_Unit
      then
         Error_Msg_N ("declaration of variable not allowed in pure unit", N);

      --  The visible part of an RCI library unit must not contain the
      --  declaration of a variable (RM E.1.3(9))

      elsif In_RCI_Declaration (N) then
         Error_Msg_N ("declaration of variable not allowed in rci unit", N);

      --  The visible part of a Shared Passive library unit must not contain
      --  the declaration of a variable (RM E.2.2(7))

      elsif In_RT_Declaration then
         Error_Msg_N
           ("variable declaration not allowed in remote types unit", N);
      end if;

   end Validate_Object_Declaration;

   --------------------------------
   --  Validate_RCI_Declarations --
   --------------------------------

   procedure Validate_RCI_Declarations (P : Entity_Id) is
      E : Entity_Id;

   begin
      E := First_Entity (P);
      while Present (E) loop
         if Comes_From_Source (E) then
            if Is_Limited_Type (E) then
               Error_Msg_N
                 ("Limited type not allowed in rci unit", Parent (E));
               Explain_Limited_Type (E, Parent (E));

            elsif Ekind (E) = E_Generic_Function
              or else Ekind (E) = E_Generic_Package
              or else Ekind (E) = E_Generic_Procedure
            then
               Error_Msg_N ("generic declaration not allowed in rci unit",
                 Parent (E));

            elsif (Ekind (E) = E_Function
                    or else Ekind (E) = E_Procedure)
              and then Has_Pragma_Inline (E)
            then
               Error_Msg_N
                 ("inlined subprogram not allowed in rci unit", Parent (E));

            --  Inner packages that are renamings need not be checked.
            --  Generic RCI packages are subject to the checks, but
            --  entities that come from formal packages are not part of the
            --  visible declarations of the package and are not checked.

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
      Id              : Node_Id;
      Param_Spec      : Node_Id;
      Param_Type      : Entity_Id;
      Base_Param_Type : Entity_Id;
      Type_Decl       : Node_Id;
      Error_Node      : Node_Id := N;

   begin
      --  There are two possible cases in which this procedure is called:

      --    1. called from Analyze_Subprogram_Declaration.
      --    2. called from Validate_Object_Declaration (access to subprogram).

      if not In_RCI_Declaration (N) then
         return;
      end if;

      if K = N_Subprogram_Declaration then
         Profile := Parameter_Specifications (Specification (N));

      else pragma Assert (K = N_Object_Declaration);
         Id := Defining_Identifier (N);

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
      --  RM E.2.3 (14)

      if Present (Profile) then
         Param_Spec := First (Profile);

         while Present (Param_Spec) loop
            Param_Type := Etype (Defining_Identifier (Param_Spec));
            Type_Decl  := Parent (Param_Type);

            if Ekind (Param_Type) = E_Anonymous_Access_Type then

               if K = N_Subprogram_Declaration then
                  Error_Node := Param_Spec;
               end if;

               --  Report error only if declaration is in source program.

               if Comes_From_Source
                 (Defining_Entity (Specification (N)))
               then
                  Error_Msg_N
                    ("subprogram in rci unit cannot have access parameter",
                      Error_Node);
               end if;

            --  For limited private type parameter, we check only the
            --  private declaration and ignore full type declaration,
            --  unless this is the only declaration for the type, eg.
            --  as a limited record.

            elsif Is_Limited_Type (Param_Type)
              and then (Nkind (Type_Decl) = N_Private_Type_Declaration
                         or else
                        (Nkind (Type_Decl) = N_Full_Type_Declaration
                          and then not (Has_Private_Declaration (Param_Type))
                          and then Comes_From_Source (N)))
            then
               --  A limited parameter is legal only if user-specified
               --  Read and Write attributes exist for it.
               --  second part of RM E.2.3 (14)

               if No (Full_View (Param_Type))
                 and then Ekind (Param_Type) /= E_Record_Type
               then
                  --  Type does not have completion yet, so if declared in
                  --  in the current RCI scope it is illegal, and will be
                  --  flagged subsequently.
                  return;
               end if;

               Base_Param_Type := Base_Type (Underlying_Type (Param_Type));

               if No (TSS (Base_Param_Type, TSS_Stream_Read))
                    or else
                  No (TSS (Base_Param_Type, TSS_Stream_Write))
               then
                  if K = N_Subprogram_Declaration then
                     Error_Node := Param_Spec;
                  end if;

                  Error_Msg_N
                    ("limited parameter in rci unit "
                       & "must have read/write attributes ", Error_Node);
                  Explain_Limited_Type (Param_Type, Error_Node);
               end if;
            end if;

            Next (Param_Spec);
         end loop;
      end if;
   end Validate_RCI_Subprogram_Declaration;

   ----------------------------------------------------
   -- Validate_Remote_Access_Object_Type_Declaration --
   ----------------------------------------------------

   procedure Validate_Remote_Access_Object_Type_Declaration (T : Entity_Id) is
      Direct_Designated_Type : Entity_Id;
      Desig_Type             : Entity_Id;
      Primitive_Subprograms  : Elist_Id;
      Subprogram             : Elmt_Id;
      Subprogram_Node        : Node_Id;
      Profile                : List_Id;
      Param_Spec             : Node_Id;
      Param_Type             : Entity_Id;

   begin
      --  We are called from Analyze_Type_Declaration, and the Nkind
      --  of the given node is N_Access_To_Object_Definition.

      if not Comes_From_Source (T)
        or else (not In_RCI_Declaration (Parent (T))
                   and then not In_RT_Declaration)
      then
         return;
      end if;

      --  An access definition in the private part of a Remote Types package
      --  may be legal if it has user-defined Read and Write attributes. This
      --  will be checked at the end of the package spec processing.

      if In_RT_Declaration and then In_Private_Part (Scope (T)) then
         return;
      end if;

      --  Check RCI or RT unit type declaration. It may not contain
      --  the declaration of an access-to-object type unless it is a
      --  general access type that designates a class-wide limited
      --  private type. There are also constraints about the primitive
      --  subprograms of the class-wide type (RM E.2.3(14)).

      if Ekind (T) /= E_General_Access_Type
        or else Ekind (Designated_Type (T)) /= E_Class_Wide_Type
      then
         if In_RCI_Declaration (Parent (T)) then
            Error_Msg_N
              ("access type in Remote_Call_Interface unit must be " &
               "general access", T);
         else
            Error_Msg_N ("access type in Remote_Types unit must be " &
              "general access", T);
         end if;
         Error_Msg_N ("\to class-wide type", T);
         return;
      end if;

      Direct_Designated_Type := Designated_Type (T);
      Desig_Type := Etype (Direct_Designated_Type);

      if not Is_Recursively_Limited_Private (Desig_Type) then
         Error_Msg_N
           ("error in designated type of remote access to class-wide type", T);
         Error_Msg_N
           ("\must be tagged limited private or private extension of type", T);
         return;
      end if;

      Primitive_Subprograms := Primitive_Operations (Desig_Type);
      Subprogram            := First_Elmt (Primitive_Subprograms);

      while Subprogram /= No_Elmt loop
         Subprogram_Node := Node (Subprogram);

         if not Comes_From_Source (Subprogram_Node) then
            goto Next_Subprogram;
         end if;

         Profile := Parameter_Specifications (Parent (Subprogram_Node));

         --  Profile must exist, otherwise not primitive operation

         Param_Spec := First (Profile);

         while Present (Param_Spec) loop

            --  Now find out if this parameter is a controlling parameter

            Param_Type := Parameter_Type (Param_Spec);

            if (Nkind (Param_Type) = N_Access_Definition
                  and then Etype (Subtype_Mark (Param_Type)) = Desig_Type)
              or else (Nkind (Param_Type) /= N_Access_Definition
                        and then Etype (Param_Type) = Desig_Type)
            then
               --  It is a controlling parameter, so specific checks below
               --  do not apply.

               null;

            elsif
              Nkind (Param_Type) = N_Access_Definition
            then
               --  From RM E.2.2(14), no access parameter other than
               --  controlling ones may be used.

               Error_Msg_N
                 ("non-controlling access parameter", Param_Spec);

            elsif
              Is_Limited_Type (Etype (Defining_Identifier (Param_Spec)))
            then
               --  Not a controlling parameter, so type must have Read
               --  and Write attributes.

               if Nkind (Param_Type) in N_Has_Etype
                 and then Nkind (Parent (Etype (Param_Type))) =
                          N_Private_Type_Declaration
               then
                  Param_Type := Etype (Param_Type);

                  if No (TSS (Param_Type, TSS_Stream_Read))
                       or else
                     No (TSS (Param_Type, TSS_Stream_Write))
                  then
                     Error_Msg_N
                       ("limited formal must have Read and Write attributes",
                         Param_Spec);
                     Explain_Limited_Type
                       (Etype (Defining_Identifier (Param_Spec)), Param_Spec);
                  end if;
               end if;
            end if;

            --  Check next parameter in this subprogram

            Next (Param_Spec);
         end loop;

         <<Next_Subprogram>>
            Next_Elmt (Subprogram);
      end loop;

      --  Now this is an RCI unit access-to-class-wide-limited-private type
      --  declaration. Set the type entity to be Is_Remote_Call_Interface to
      --  optimize later checks by avoiding tree traversal to find out if this
      --  entity is inside an RCI unit.

      Set_Is_Remote_Call_Interface (T);

   end Validate_Remote_Access_Object_Type_Declaration;

   -----------------------------------------------
   -- Validate_Remote_Access_To_Class_Wide_Type --
   -----------------------------------------------

   procedure Validate_Remote_Access_To_Class_Wide_Type (N : Node_Id) is
      K  : constant Node_Kind := Nkind (N);
      PK : constant Node_Kind := Nkind (Parent (N));
      E  : Entity_Id;

   begin
      --  This subprogram enforces the checks in (RM E.2.2(8)) for
      --  certain uses of class-wide limited private types.

      --    Storage_Pool and Storage_Size are not defined for such types
      --
      --    The expected type of allocator must not not be such a type.

      --    The actual parameter of generic instantiation must not
      --    be such a type if the formal parameter is of an access type.

      --  On entry, there are five cases

      --    1. called from sem_attr Analyze_Attribute where attribute
      --       name is either Storage_Pool or Storage_Size.

      --    2. called from exp_ch4 Expand_N_Allocator

      --    3. called from sem_ch12 Analyze_Associations

      --    4. called from sem_ch4 Analyze_Explicit_Dereference

      --    5. called from sem_res Resolve_Actuals

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

      elsif K in N_Has_Entity then
         E := Entity (N);

         if Is_Remote_Access_To_Class_Wide_Type (E) then
            Error_Msg_N ("incorrect remote type generic actual", N);
            return;
         end if;

      --  This subprogram also enforces the checks in E.2.2(13).
      --  A value of such type must not be dereferenced unless as a
      --  controlling operand of a dispatching call.

      elsif K = N_Explicit_Dereference
        and then (Comes_From_Source (N)
                    or else (Nkind (Original_Node (N)) = N_Selected_Component
                               and then Comes_From_Source (Original_Node (N))))
      then
         E := Etype (Prefix (N));

         --  If the class-wide type is not a remote one, the restrictions
         --  do not apply.

         if not Is_Remote_Access_To_Class_Wide_Type (E) then
            return;
         end if;

         --  If we have a true dereference that comes from source and that
         --  is a controlling argument for a dispatching call, accept it.

         if K = N_Explicit_Dereference
           and then Is_Actual_Parameter (N)
           and then Is_Controlling_Actual (N)
         then
            return;
         end if;

         --  If we are just within a procedure or function call and the
         --  dereference has not been analyzed, return because this
         --  procedure will be called again from sem_res Resolve_Actuals.

         if Is_Actual_Parameter (N)
           and then not Analyzed (N)
         then
            return;
         end if;

         --  The following is to let the compiler generated tags check
         --  pass through without error message. This is a bit kludgy
         --  isn't there some better way of making this exclusion ???

         if (PK = N_Selected_Component
              and then Present (Parent (Parent (N)))
              and then Nkind (Parent (Parent (N))) = N_Op_Ne)
           or else (PK = N_Unchecked_Type_Conversion
                    and then Present (Parent (Parent (N)))
                    and then
                      Nkind (Parent (Parent (N))) = N_Selected_Component)
         then
            return;
         end if;

         --  The following code is needed for expansion of RACW Write
         --  attribute, since such expressions can appear in the expanded
         --  code.

         if not Comes_From_Source (N)
           and then
           (PK = N_In
            or else PK = N_Attribute_Reference
            or else
              (PK = N_Type_Conversion
               and then Present (Parent (N))
               and then Present (Parent (Parent (N)))
               and then
                 Nkind (Parent (Parent (N))) = N_Selected_Component))
         then
            return;
         end if;

         Error_Msg_N ("incorrect remote type dereference", N);
      end if;
   end Validate_Remote_Access_To_Class_Wide_Type;

   ------------------------------------------
   -- Validate_Remote_Type_Type_Conversion --
   ------------------------------------------

   procedure Validate_Remote_Type_Type_Conversion (N : Node_Id) is
      S : constant Entity_Id := Etype (N);
      E : constant Entity_Id := Etype (Expression (N));

   begin
      --  This test is required in the case where a conversion appears
      --  inside a normal package, it does not necessarily have to be
      --  inside an RCI, Remote_Types unit (RM E.2.2(9,12)).

      if Is_Remote_Access_To_Subprogram_Type (E)
        and then not Is_Remote_Access_To_Subprogram_Type (S)
      then
         Error_Msg_N ("incorrect conversion of remote operand", N);
         return;

      elsif Is_Remote_Access_To_Class_Wide_Type (E)
        and then not Is_Remote_Access_To_Class_Wide_Type (S)
      then
         Error_Msg_N ("incorrect conversion of remote operand", N);
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
      Spec            : constant Node_Id   := Specification (N);
      Name_U          : constant Entity_Id := Defining_Entity (Spec);
      Typ             : Entity_Id;
      First_Priv_Ent  : constant Entity_Id := First_Private_Entity (Name_U);
      In_Visible_Part : Boolean            := True;

   begin
      if not Is_Remote_Types (Name_U) then
         return;
      end if;

      Typ := First_Entity (Name_U);
      while Present (Typ) loop
         if In_Visible_Part and then Typ = First_Priv_Ent then
            In_Visible_Part := False;
         end if;

         if Comes_From_Source (Typ)
           and then Is_Type (Typ)
           and then (In_Visible_Part or else Has_Private_Declaration (Typ))
         then
            if Missing_Read_Write_Attributes (Typ) then
               if Is_Non_Remote_Access_Type (Typ) then
                  Error_Msg_N
                    ("non-remote access type without user-defined Read " &
                     "and Write attributes", Typ);
               else
                  Error_Msg_N
                    ("record type containing a component of a " &
                     "non-remote access", Typ);
                  Error_Msg_N
                    ("\type without Read and Write attributes " &
                     "('R'M E.2.2(8))", Typ);
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
      --  Return true if the protected type designated by T has
      --  entry declarations.

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
      --  We are called from Sem_Ch3.Analyze_Type_Declaration, and the
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
      E : Entity_Id;

      function Is_Primary (N : Node_Id) return Boolean;
      --  Determine whether node is syntactically a primary in an expression.

      function Is_Primary (N : Node_Id) return Boolean is
         K : constant Node_Kind := Nkind (Parent (N));

      begin
         case K is

            when N_Op | N_In | N_Not_In =>
               return True;

            when N_Aggregate
               | N_Component_Association
               | N_Index_Or_Discriminant_Constraint =>
               return True;

            when N_Attribute_Reference =>
               return Attribute_Name (Parent (N)) /= Name_Address
                 and then Attribute_Name (Parent (N)) /= Name_Access
                 and then Attribute_Name (Parent (N)) /= Name_Unchecked_Access
                 and then
                   Attribute_Name (Parent (N)) /= Name_Unrestricted_Access;

            when N_Indexed_Component =>
               return (N /= Prefix (Parent (N))
                 or else Is_Primary (Parent (N)));

            when N_Qualified_Expression | N_Type_Conversion =>
               return Is_Primary (Parent (N));

            when N_Assignment_Statement | N_Object_Declaration =>
               return (N = Expression (Parent (N)));

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

      --  Filter out cases where primary is default in a component
      --  declaration, discriminant specification, or actual in a record
      --  type initialization call.

      --  Initialization call of internal types.

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
      --  checked, because the call itself will be diagnosed.

      if Is_Primary (N)
        and then (not Inside_A_Generic
                   or else Present (Enclosing_Generic_Body (N)))
      then
         if Ekind (Entity (N)) = E_Variable then
            Flag_Non_Static_Expr
              ("non-static object name in preelaborated unit", N);

         --  We take the view that a constant defined in another preelaborated
         --  unit is preelaborable, even though it may have a private type and
         --  thus appear non-static in a client. This must be the intent of
         --  the language, but currently is an RM gap.

         elsif Ekind (Entity (N)) = E_Constant
           and then not Is_Static_Expression (N)
         then
            E := Entity (N);

            if Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (N)))
              and then
                Enclosing_Lib_Unit_Node (N) /= Enclosing_Lib_Unit_Node (E)
              and then (Is_Preelaborated (Scope (E))
                          or else Is_Pure (Scope (E))
                          or else (Present (Renamed_Object (E))
                                     and then
                                       Is_Entity_Name (Renamed_Object (E))
                                     and then
                                       (Is_Preelaborated
                                         (Scope (Renamed_Object (E)))
                                            or else
                                              Is_Pure (Scope
                                                (Renamed_Object (E))))))
            then
               null;
            else
               Flag_Non_Static_Expr
                 ("non-static constant in preelaborated unit", N);
            end if;
         end if;
      end if;
   end Validate_Static_Object_Name;

end Sem_Cat;
