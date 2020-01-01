------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Einfo;    use Einfo;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;
with Exp_Imgv; use Exp_Imgv;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Validsw;  use Validsw;

package body Exp_Ch13 is

   ------------------------------------------
   -- Expand_N_Attribute_Definition_Clause --
   ------------------------------------------

   --  Expansion action depends on attribute involved

   procedure Expand_N_Attribute_Definition_Clause (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Exp : constant Node_Id    := Expression (N);
      Ent : Entity_Id;
      V   : Node_Id;

   begin
      Ent := Entity (Name (N));

      if Is_Type (Ent) then
         Ent := Underlying_Type (Ent);
      end if;

      case Get_Attribute_Id (Chars (N)) is

         -------------
         -- Address --
         -------------

         when Attribute_Address =>

            --  If there is an initialization which did not come from the
            --  source program, then it is an artifact of our expansion, and we
            --  suppress it. The case we are most concerned about here is the
            --  initialization of a packed array to all false, which seems
            --  inappropriate for variable to which an address clause is
            --  applied. The expression may itself have been rewritten if the
            --  type is packed array, so we need to examine whether the
            --  original node is in the source. An exception though is the case
            --  of an access variable which is default initialized to null, and
            --  such initialization is retained.

            --  Furthermore, if the initialization is the equivalent aggregate
            --  of the type initialization procedure, it replaces an implicit
            --  call to the init proc, and must be respected. Note that for
            --  packed types we do not build equivalent aggregates.

            --  Also, if Init_Or_Norm_Scalars applies, then we need to retain
            --  any default initialization for objects of scalar types and
            --  types with scalar components. Normally a composite type will
            --  have an init_proc in the presence of Init_Or_Norm_Scalars,
            --  so when that flag is set we have just have to do a test for
            --  scalar and string types (the predefined string types such as
            --  String and Wide_String don't have an init_proc).

            declare
               Decl : constant Node_Id := Declaration_Node (Ent);
               Typ  : constant Entity_Id := Etype (Ent);

            begin
               if Nkind (Decl) = N_Object_Declaration
                  and then Present (Expression (Decl))
                  and then Nkind (Expression (Decl)) /= N_Null
                  and then
                    not Comes_From_Source (Original_Node (Expression (Decl)))
               then
                  if Present (Base_Init_Proc (Typ))
                    and then
                      Present (Static_Initialization (Base_Init_Proc (Typ)))
                  then
                     null;

                  elsif Init_Or_Norm_Scalars
                    and then (Is_Scalar_Type (Typ)
                               or else Is_String_Type (Typ))
                  then
                     null;

                  else
                     Set_Expression (Decl, Empty);
                  end if;

               --  An object declaration to which an address clause applies
               --  has a delayed freeze, but the address expression itself
               --  must be elaborated at the point it appears. If the object
               --  is controlled, additional checks apply elsewhere.
               --  If the attribute comes from an aspect specification it
               --  is being elaborated at the freeze point and side effects
               --  need not be removed (and shouldn't, if the expression
               --  depends on other entities that have delayed freeze).
               --  This is another consequence of the delayed analysis of
               --  aspects, and a real semantic difference.

               elsif Nkind (Decl) = N_Object_Declaration
                 and then not Needs_Constant_Address (Decl, Typ)
                 and then not From_Aspect_Specification (N)
               then
                  Remove_Side_Effects (Exp);
               end if;
            end;

         ---------------
         -- Alignment --
         ---------------

         when Attribute_Alignment =>

            --  As required by Gigi, we guarantee that the operand is an
            --  integer literal (this simplifies things in Gigi).

            if Nkind (Exp) /= N_Integer_Literal then
               Rewrite (Exp, Make_Integer_Literal (Loc, Expr_Value (Exp)));
            end if;

            --  A complex case arises if the alignment clause applies to an
            --  unconstrained object initialized with a function call. The
            --  result of the call is placed on the secondary stack, and the
            --  declaration is rewritten as a renaming of a dereference, which
            --  fails expansion. We must introduce a temporary and assign its
            --  value to the existing entity.

            if Nkind (Parent (Ent)) = N_Object_Renaming_Declaration
              and then not Is_Entity_Name (Renamed_Object (Ent))
            then
               declare
                  Decl : constant Node_Id    := Parent (Ent);
                  Loc  : constant Source_Ptr := Sloc (N);
                  Temp : constant Entity_Id  := Make_Temporary (Loc, 'T');

                  New_Decl : Node_Id;

               begin
                  --  Replace entity with temporary and reanalyze

                  Set_Defining_Identifier (Decl, Temp);
                  Set_Analyzed (Decl, False);
                  Analyze (Decl);

                  --  Introduce new declaration for entity but do not reanalyze
                  --  because entity is already in scope. Type and expression
                  --  are already resolved.

                  New_Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Ent,
                      Object_Definition   =>
                        New_Occurrence_Of (Etype (Ent), Loc),
                      Expression          => New_Occurrence_Of (Temp, Loc));

                  Set_Renamed_Object (Ent, Empty);
                  Insert_After (Decl, New_Decl);
                  Set_Analyzed (Decl);
               end;
            end if;

         ------------------
         -- Storage_Size --
         ------------------

         when Attribute_Storage_Size =>

            --  If the type is a task type, then assign the value of the
            --  storage size to the Size variable associated with the task.
            --  Insert the assignment right after the declaration of the Size
            --  variable.

            --  Generate:

            --  task_typeZ := expression

            if Ekind (Ent) = E_Task_Type then

               declare
                  Assign : Node_Id;
               begin
                  Assign :=
                    Make_Assignment_Statement (Loc,
                      Name       =>
                        New_Occurrence_Of (Storage_Size_Variable (Ent), Loc),
                      Expression =>
                        Convert_To (RTE (RE_Size_Type), Expression (N)));

                  --  If the clause is not generated by an aspect, insert
                  --  the assignment here. Freezing rules ensure that this
                  --  is safe, or clause will have been rejected already.

                  if Is_List_Member (N) then
                     Insert_After (N, Assign);

                  --  Otherwise, insert assignment after task declaration.

                  else
                     Insert_After
                       (Parent (Storage_Size_Variable (Entity (N))), Assign);
                  end if;

                  Analyze (Assign);
               end;

            --  For Storage_Size for an access type, create a variable to hold
            --  the value of the specified size with name typeV and expand an
            --  assignment statement to initialize this value.

            elsif Is_Access_Type (Ent) then

               --  We don't need the variable for a storage size of zero

               if not No_Pool_Assigned (Ent) then
                  V :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (Ent), 'V'));

                  --  Insert the declaration of the object. If the expression
                  --  is not static it may depend on some other type that is
                  --  not frozen yet, so attach the declaration that captures
                  --  the value of the expression to the actions of the freeze
                  --  node of the current type.

                  declare
                     Decl : constant Node_Id :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => V,
                         Object_Definition   =>
                           New_Occurrence_Of (RTE (RE_Storage_Offset), Loc),
                         Expression          =>
                           Convert_To
                             (RTE (RE_Storage_Offset), Expression (N)));
                  begin
                     if not Is_OK_Static_Expression (Expression (N))
                       and then Present (Freeze_Node (Ent))
                     then
                        if No (Actions (Freeze_Node (Ent))) then
                           Set_Actions (Freeze_Node (Ent), New_List (Decl));
                        else
                           Append (Decl, Actions (Freeze_Node (Ent)));
                        end if;

                     else
                        Insert_Action (N, Decl);
                     end if;
                  end;

                  Set_Storage_Size_Variable (Ent, Entity_Id (V));
               end if;
            end if;

         --  Other attributes require no expansion

         when others =>
            null;
      end case;
   end Expand_N_Attribute_Definition_Clause;

   -----------------------------
   -- Expand_N_Free_Statement --
   -----------------------------

   procedure Expand_N_Free_Statement (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
      Typ  : Entity_Id;

   begin
      --  Certain run-time configurations and targets do not provide support
      --  for controlled types.

      if Restriction_Active (No_Finalization) then
         return;
      end if;

      --  Use the base type to perform the check for finalization master

      Typ := Etype (Expr);

      if Ekind (Typ) = E_Access_Subtype then
         Typ := Etype (Typ);
      end if;

      --  Handle private access types

      if Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
      then
         Typ := Full_View (Typ);
      end if;

      --  Do not create a custom Deallocate when freeing an object with
      --  suppressed finalization. In such cases the object is never attached
      --  to a master, so it does not need to be detached. Use a regular free
      --  statement instead.

      if No (Finalization_Master (Typ)) then
         return;
      end if;

      --  Use a temporary to store the result of a complex expression. Perform
      --  the following transformation:
      --
      --     Free (Complex_Expression);
      --
      --     Temp : constant Type_Of_Expression := Complex_Expression;
      --     Free (Temp);

      if Nkind (Expr) /= N_Identifier then
         declare
            Expr_Typ : constant Entity_Id  := Etype (Expr);
            Loc      : constant Source_Ptr := Sloc (N);
            New_Expr : Node_Id;
            Temp_Id  : Entity_Id;

         begin
            Temp_Id := Make_Temporary (Loc, 'T');
            Insert_Action (N,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp_Id,
                Object_Definition   => New_Occurrence_Of (Expr_Typ, Loc),
                Expression          => Relocate_Node (Expr)));

            New_Expr := New_Occurrence_Of (Temp_Id, Loc);
            Set_Etype (New_Expr, Expr_Typ);

            Set_Expression (N, New_Expr);
         end;
      end if;

      --  Create a custom Deallocate for a controlled object. This routine
      --  ensures that the hidden list header will be deallocated along with
      --  the actual object.

      Build_Allocate_Deallocate_Proc (N, Is_Allocate => False);
   end Expand_N_Free_Statement;

   ----------------------------
   -- Expand_N_Freeze_Entity --
   ----------------------------

   procedure Expand_N_Freeze_Entity (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

      Decl           : Node_Id;
      Delete         : Boolean := False;
      E_Scope        : Entity_Id;
      In_Other_Scope : Boolean;
      In_Outer_Scope : Boolean;

   begin
      --  If there are delayed aspect specifications, we insert them just
      --  before the freeze node. They are already analyzed so we don't need
      --  to reanalyze them (they were analyzed before the type was frozen),
      --  but we want them in the tree for the back end, and so that the
      --  listing from sprint is clearer on where these occur logically.

      if Has_Delayed_Aspects (E) then
         declare
            Aitem : Node_Id;
            Ritem : Node_Id;

         begin
            --  Look for aspect specs for this entity

            Ritem := First_Rep_Item (E);
            while Present (Ritem) loop
               if Nkind (Ritem) = N_Aspect_Specification
                 and then Entity (Ritem) = E
               then
                  Aitem := Aspect_Rep_Item (Ritem);

                  --  Skip this for aspects (e.g. Current_Value) for which
                  --  there is no corresponding pragma or attribute.

                  if Present (Aitem)

                    --  Also skip if we have a null statement rather than a
                    --  delayed aspect (this happens when we are ignoring rep
                    --  items from use of the -gnatI switch).

                    and then Nkind (Aitem) /= N_Null_Statement
                  then
                     pragma Assert (Is_Delayed_Aspect (Aitem));
                     Insert_Before (N, Aitem);
                  end if;
               end if;

               Next_Rep_Item (Ritem);
            end loop;
         end;
      end if;

      --  Processing for objects

      if Is_Object (E) then
         if Present (Address_Clause (E)) then
            Apply_Address_Clause_Check (E, N);
         end if;

         --  Analyze actions in freeze node, if any

         if Present (Actions (N)) then
            declare
               Act : Node_Id;
            begin
               Act := First (Actions (N));
               while Present (Act) loop
                  Analyze (Act);
                  Next (Act);
               end loop;
            end;
         end if;

         --  If initialization statements have been captured in a compound
         --  statement, insert them back into the tree now.

         Explode_Initialization_Compound_Statement (E);
         return;

      --  Only other items requiring any front end action are types and
      --  subprograms.

      elsif not Is_Type (E) and then not Is_Subprogram (E) then
         return;
      end if;

      --  Here E is a type or a subprogram

      E_Scope := Scope (E);

      --  This is an error protection against previous errors

      if No (E_Scope) then
         Check_Error_Detected;
         return;
      end if;

      --  The entity may be a subtype declared for a constrained record
      --  component, in which case the relevant scope is the scope of
      --  the record. This happens for class-wide subtypes created for
      --  a constrained type extension with inherited discriminants.

      if Is_Type (E_Scope)
        and then Ekind (E_Scope) not in Concurrent_Kind
      then
         E_Scope := Scope (E_Scope);

      --  The entity may be a subtype declared for an iterator

      elsif Ekind (E_Scope) = E_Loop then
         E_Scope := Scope (E_Scope);
      end if;

      --  Remember that we are processing a freezing entity and its freezing
      --  nodes. This flag (non-zero = set) is used to avoid the need of
      --  climbing through the tree while processing the freezing actions (ie.
      --  to avoid generating spurious warnings or to avoid killing constant
      --  indications while processing the code associated with freezing
      --  actions). We use a counter to deal with nesting.

      Inside_Freezing_Actions := Inside_Freezing_Actions + 1;

      --  If we are freezing entities defined in protected types, they belong
      --  in the enclosing scope, given that the original type has been
      --  expanded away. The same is true for entities in task types, in
      --  particular the parameter records of entries (Entities in bodies are
      --  all frozen within the body). If we are in the task body, this is a
      --  proper scope. If we are within a subprogram body, the proper scope
      --  is the corresponding spec. This may happen for itypes generated in
      --  the bodies of protected operations.

      if Ekind (E_Scope) = E_Protected_Type
        or else (Ekind (E_Scope) = E_Task_Type
                  and then not Has_Completion (E_Scope))
      then
         E_Scope := Scope (E_Scope);

      elsif Ekind (E_Scope) = E_Subprogram_Body then
         E_Scope := Corresponding_Spec (Unit_Declaration_Node (E_Scope));
      end if;

      --  If the scope of the entity is in open scopes, it is the current one
      --  or an enclosing one, including a loop, a block, or a subprogram.

      if In_Open_Scopes (E_Scope) then
         In_Other_Scope := False;
         In_Outer_Scope := E_Scope /= Current_Scope;

      --  Otherwise it is a local package or a different compilation unit

      else
         In_Other_Scope := True;
         In_Outer_Scope := False;
      end if;

      --  If the entity being frozen is defined in a scope that is not
      --  currently on the scope stack, we must establish the proper
      --  visibility before freezing the entity and related subprograms.

      if In_Other_Scope then
         Push_Scope (E_Scope);

         --  Finalizers are little odd in terms of freezing. The spec of the
         --  procedure appears in the declarations while the body appears in
         --  the statement part of a single construct. Since the finalizer must
         --  be called by the At_End handler of the construct, the spec is
         --  manually frozen right after its declaration. The only side effect
         --  of this action appears in contexts where the construct is not in
         --  its final resting place. These contexts are:

         --    * Entry bodies - The declarations and statements are moved to
         --      the procedure equivalen of the entry.
         --    * Protected subprograms - The declarations and statements are
         --      moved to the non-protected version of the subprogram.
         --    * Task bodies - The declarations and statements are moved to the
         --      task body procedure.
         --    * Blocks that will be rewritten as subprograms when unnesting
         --      is in effect.

         --  Visible declarations do not need to be installed in these three
         --  cases since it does not make semantic sense to do so. All entities
         --  referenced by a finalizer are visible and already resolved, plus
         --  the enclosing scope may not have visible declarations at all.

         if Ekind (E) = E_Procedure
           and then Is_Finalizer (E)
           and then
             (Is_Entry (E_Scope)
                or else (Is_Subprogram (E_Scope)
                          and then Is_Protected_Type (Scope (E_Scope)))
                or else Is_Task_Type (E_Scope)
                or else Ekind (E_Scope) = E_Block)
         then
            null;
         else
            Install_Visible_Declarations (E_Scope);
         end if;

         if Is_Package_Or_Generic_Package (E_Scope) or else
            Is_Protected_Type (E_Scope)             or else
            Is_Task_Type (E_Scope)
         then
            Install_Private_Declarations (E_Scope);
         end if;

      --  If the entity is in an outer scope, then that scope needs to
      --  temporarily become the current scope so that operations created
      --  during type freezing will be declared in the right scope and
      --  can properly override any corresponding inherited operations.

      elsif In_Outer_Scope then
         Push_Scope (E_Scope);
      end if;

      --  If type, freeze the type

      if Is_Type (E) then
         Delete := Freeze_Type (N);

         --  And for enumeration type, build the enumeration tables

         if Is_Enumeration_Type (E) then
            Build_Enumeration_Image_Tables (E, N);
         end if;

      --  If subprogram, freeze the subprogram

      elsif Is_Subprogram (E) then
         Exp_Ch6.Freeze_Subprogram (N);

         --  Ada 2005 (AI-251): Remove the freezing node associated with the
         --  entities internally used by the frontend to register primitives
         --  covering abstract interfaces. The call to Freeze_Subprogram has
         --  already expanded the code that fills the corresponding entry in
         --  its secondary dispatch table and therefore the code generator
         --  has nothing else to do with this freezing node.

         Delete := Present (Interface_Alias (E));
      end if;

      --  Analyze actions generated by freezing. The init_proc contains source
      --  expressions that may raise Constraint_Error, and the assignment
      --  procedure for complex types needs checks on individual component
      --  assignments, but all other freezing actions should be compiled with
      --  all checks off.

      if Present (Actions (N)) then
         Decl := First (Actions (N));
         while Present (Decl) loop
            if Nkind (Decl) = N_Subprogram_Body
              and then (Is_Init_Proc (Defining_Entity (Decl))
                          or else
                            Chars (Defining_Entity (Decl)) = Name_uAssign)
            then
               Analyze (Decl);

            --  A subprogram body created for a renaming_as_body completes
            --  a previous declaration, which may be in a different scope.
            --  Establish the proper scope before analysis.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then Present (Corresponding_Spec (Decl))
              and then Scope (Corresponding_Spec (Decl)) /= Current_Scope
            then
               Push_Scope (Scope (Corresponding_Spec (Decl)));
               Analyze (Decl, Suppress => All_Checks);
               Pop_Scope;

            --  We treat generated equality specially, if validity checks are
            --  enabled, in order to detect components default-initialized
            --  with invalid values.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then Chars (Defining_Entity (Decl)) = Name_Op_Eq
              and then Validity_Checks_On
              and then Initialize_Scalars
            then
               declare
                  Save_Force : constant Boolean := Force_Validity_Checks;
               begin
                  Force_Validity_Checks := True;
                  Analyze (Decl);
                  Force_Validity_Checks := Save_Force;
               end;

            --  All other freezing actions

            else
               Analyze (Decl, Suppress => All_Checks);
            end if;

            Next (Decl);
         end loop;
      end if;

      --  If we are to delete this N_Freeze_Entity, do so by rewriting so that
      --  a loop on all nodes being inserted will work propertly.

      if Delete then
         Rewrite (N, Make_Null_Statement (Sloc (N)));
      end if;

      --  Pop scope if we installed one for the analysis

      if In_Other_Scope then
         if Ekind (Current_Scope) = E_Package then
            End_Package_Scope (E_Scope);
         else
            End_Scope;
         end if;

      elsif In_Outer_Scope then
         Pop_Scope;
      end if;

      --  Restore previous value of the nesting-level counter that records
      --  whether we are inside a (possibly nested) call to this procedure.

      Inside_Freezing_Actions := Inside_Freezing_Actions - 1;
   end Expand_N_Freeze_Entity;

   -------------------------------------------
   -- Expand_N_Record_Representation_Clause --
   -------------------------------------------

   --  The only expansion required is for the case of a mod clause present,
   --  which is removed, and translated into an alignment representation
   --  clause inserted immediately after the record rep clause with any
   --  initial pragmas inserted at the start of the component clause list.

   procedure Expand_N_Record_Representation_Clause (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Rectype : constant Entity_Id  := Entity (Identifier (N));
      Mod_Val : Uint;
      Citems  : List_Id;
      Repitem : Node_Id;
      AtM_Nod : Node_Id;

   begin
      if Present (Mod_Clause (N)) and then not Ignore_Rep_Clauses then
         Mod_Val := Expr_Value (Expression (Mod_Clause (N)));
         Citems  := Pragmas_Before (Mod_Clause (N));

         if Present (Citems) then
            Append_List_To (Citems, Component_Clauses (N));
            Set_Component_Clauses (N, Citems);
         end if;

         AtM_Nod :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Occurrence_Of (Base_Type (Rectype), Loc),
             Chars      => Name_Alignment,
             Expression => Make_Integer_Literal (Loc, Mod_Val));

         Set_From_At_Mod (AtM_Nod);
         Insert_After (N, AtM_Nod);
         Set_Mod_Clause (N, Empty);
      end if;

      --  If the record representation clause has no components, then
      --  completely remove it. Note that we also have to remove
      --  ourself from the Rep Item list.

      if Is_Empty_List (Component_Clauses (N)) then
         if First_Rep_Item (Rectype) = N then
            Set_First_Rep_Item (Rectype, Next_Rep_Item (N));
         else
            Repitem := First_Rep_Item (Rectype);
            while Present (Next_Rep_Item (Repitem)) loop
               if Next_Rep_Item (Repitem) = N then
                  Set_Next_Rep_Item (Repitem, Next_Rep_Item (N));
                  exit;
               end if;

               Next_Rep_Item (Repitem);
            end loop;
         end if;

         Rewrite (N,
           Make_Null_Statement (Loc));
      end if;
   end Expand_N_Record_Representation_Clause;

end Exp_Ch13;
