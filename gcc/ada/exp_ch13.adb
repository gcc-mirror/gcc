------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Einfo;    use Einfo;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Imgv; use Exp_Imgv;
with Exp_Util; use Exp_Util;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

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

            --  If there is an initialization which did not come from
            --  the source program, then it is an artifact of our
            --  expansion, and we suppress it. The case we are most
            --  concerned about here is the initialization of a packed
            --  array to all false, which seems inappropriate for a
            --  variable to which an address clause is applied. The
            --  expression may itself have been rewritten if the type is a
            --  packed array, so we need to examine whether the original
            --  node is in the source.

            declare
               Decl : constant Node_Id := Declaration_Node (Ent);

            begin
               if Nkind (Decl) = N_Object_Declaration
                  and then Present (Expression (Decl))
                  and then
                   not Comes_From_Source (Original_Node (Expression (Decl)))
               then
                  Set_Expression (Decl, Empty);
               end if;
            end;

         ---------------
         -- Alignment --
         ---------------

         when Attribute_Alignment =>

            --  As required by Gigi, we guarantee that the operand is an
            --  integer literal (this simplifies things in Gigi).

            if Nkind (Exp) /= N_Integer_Literal then
               Rewrite
                 (Exp, Make_Integer_Literal (Loc, Expr_Value (Exp)));
            end if;

         ------------------
         -- External_Tag --
         ------------------

         --  For the rep clause "for x'external_tag use y" generate:

         --     xV : constant string := y;
         --     Set_External_Tag (x'tag, xV'Address);
         --     Register_Tag (x'tag);

         --  note that register_tag has been delayed up to now because
         --  the external_tag must be set before resistering.

         when Attribute_External_Tag => External_Tag : declare
            E       : Entity_Id;
            Old_Val : String_Id := Strval (Expr_Value_S (Exp));
            New_Val : String_Id;

         begin
            --  Create a new nul terminated string if it is not already

            if String_Length (Old_Val) > 0
              and then Get_String_Char (Old_Val, String_Length (Old_Val)) = 0
            then
               New_Val := Old_Val;
            else
               Start_String (Old_Val);
               Store_String_Char (Get_Char_Code (ASCII.NUL));
               New_Val := End_String;
            end if;

            E :=
              Make_Defining_Identifier (Loc,
                New_External_Name (Chars (Ent), 'A'));

            Insert_Action (N,
              Make_Object_Declaration (Loc,
                Defining_Identifier => E,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Reference_To (Standard_String, Loc),
                Expression          =>
                  Make_String_Literal (Loc, Strval => New_Val)));

            Insert_Actions (N, New_List (
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Set_External_Tag), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Tag,
                    Prefix         => New_Occurrence_Of (Ent, Loc)),

                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Address,
                    Prefix         => New_Occurrence_Of (E, Loc)))),

              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Register_Tag), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Tag,
                    Prefix         => New_Occurrence_Of (Ent, Loc))))));
         end External_Tag;

         ------------------
         -- Storage_Size --
         ------------------

         when Attribute_Storage_Size =>

            --  If the type is a task type, then assign the value of the
            --  storage size to the Size variable associated with the task.
            --    task_typeZ := expression

            if Ekind (Ent) = E_Task_Type then
               Insert_Action (N,
                 Make_Assignment_Statement (Loc,
                   Name => New_Reference_To (Storage_Size_Variable (Ent), Loc),
                   Expression =>
                     Convert_To (RTE (RE_Size_Type), Expression (N))));

            --  For Storage_Size for an access type, create a variable to hold
            --  the value of the specified size with name typeV and expand an
            --  assignment statement to initialze this value.

            elsif Is_Access_Type (Ent) then

               V := Make_Defining_Identifier (Loc,
                      New_External_Name (Chars (Ent), 'V'));

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => V,
                   Object_Definition  =>
                     New_Reference_To (RTE (RE_Storage_Offset), Loc),
                   Expression =>
                     Convert_To (RTE (RE_Storage_Offset), Expression (N))));

               Set_Storage_Size_Variable (Ent, Entity_Id (V));
            end if;

         --  Other attributes require no expansion

         when others =>
            null;

      end case;

   end Expand_N_Attribute_Definition_Clause;

   ----------------------------
   -- Expand_N_Freeze_Entity --
   ----------------------------

   procedure Expand_N_Freeze_Entity (N : Node_Id) is
      E              : constant Entity_Id := Entity (N);
      E_Scope        : Entity_Id;
      S              : Entity_Id;
      In_Other_Scope : Boolean;
      In_Outer_Scope : Boolean;
      Decl           : Node_Id;

   begin
      --  For object, with address clause, check alignment is OK

      if Is_Object (E) then
         Apply_Alignment_Check (E, N);

      --  Only other items requiring any front end action are
      --  types and subprograms.

      elsif not Is_Type (E) and then not Is_Subprogram (E) then
         return;
      end if;

      --  Here E is a type or a subprogram

      E_Scope := Scope (E);

      --  If we are freezing entities defined in protected types, they
      --  belong in the enclosing scope, given that the original type
      --  has been expanded away. The same is true for entities in task types,
      --  in particular the parameter records of entries (Entities in bodies
      --  are all frozen within the body). If we are in the task body, this
      --  is a proper scope.

      if Ekind (E_Scope) = E_Protected_Type
        or else (Ekind (E_Scope) = E_Task_Type
                   and then not Has_Completion (E_Scope))
      then
         E_Scope := Scope (E_Scope);
      end if;

      S := Current_Scope;
      while S /= Standard_Standard and then S /= E_Scope loop
         S := Scope (S);
      end loop;

      In_Other_Scope := not (S = E_Scope);
      In_Outer_Scope := (not In_Other_Scope) and then (S /= Current_Scope);

      --  If the entity being frozen is defined in a scope that is not
      --  currently on the scope stack, we must establish the proper
      --  visibility before freezing the entity and related subprograms.

      if In_Other_Scope then
         New_Scope (E_Scope);
         Install_Visible_Declarations (E_Scope);

         if Ekind (E_Scope) = E_Package         or else
            Ekind (E_Scope) = E_Generic_Package or else
            Is_Protected_Type (E_Scope)         or else
            Is_Task_Type (E_Scope)
         then
            Install_Private_Declarations (E_Scope);
         end if;

      --  If the entity is in an outer scope, then that scope needs to
      --  temporarily become the current scope so that operations created
      --  during type freezing will be declared in the right scope and
      --  can properly override any corresponding inherited operations.

      elsif In_Outer_Scope then
         New_Scope (E_Scope);
      end if;

      --  If type, freeze the type

      if Is_Type (E) then
         Freeze_Type (N);

         --  And for enumeration type, build the enumeration tables

         if Is_Enumeration_Type (E) then
            Build_Enumeration_Image_Tables (E, N);
         end if;

      --  If subprogram, freeze the subprogram

      elsif Is_Subprogram (E) then
         Freeze_Subprogram (N);
      end if;

      --  Analyze actions generated by freezing. The init_proc contains
      --  source expressions that may raise constraint_error, and the
      --  assignment procedure for complex types needs checks on individual
      --  component assignments, but all other freezing actions should be
      --  compiled with all checks off.

      if Present (Actions (N)) then
         Decl := First (Actions (N));

         while Present (Decl) loop

            if Nkind (Decl) = N_Subprogram_Body
              and then (Chars (Defining_Entity (Decl)) = Name_uInit_Proc
                 or else Chars (Defining_Entity (Decl)) = Name_uAssign)
            then
               Analyze (Decl);

            --  A subprogram body created for a renaming_as_body completes
            --  a previous declaration, which may be in a different scope.
            --  Establish the proper scope before analysis.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then Present (Corresponding_Spec (Decl))
              and then Scope (Corresponding_Spec (Decl)) /= Current_Scope
            then
               New_Scope (Scope (Corresponding_Spec (Decl)));
               Analyze (Decl, Suppress => All_Checks);
               Pop_Scope;

            else
               Analyze (Decl, Suppress => All_Checks);
            end if;

            Next (Decl);
         end loop;
      end if;

      if In_Other_Scope then
         if Ekind (Current_Scope) = E_Package then
            End_Package_Scope (E_Scope);
         else
            End_Scope;
         end if;

      elsif In_Outer_Scope then
         Pop_Scope;
      end if;
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
      if Present (Mod_Clause (N)) then
         Mod_Val := Expr_Value (Expression (Mod_Clause (N)));
         Citems  := Pragmas_Before (Mod_Clause (N));

         if Present (Citems) then
            Append_List_To (Citems, Component_Clauses (N));
            Set_Component_Clauses (N, Citems);
         end if;

         AtM_Nod :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Reference_To (Base_Type (Rectype), Loc),
             Chars      => Name_Alignment,
             Expression => Make_Integer_Literal (Loc, Mod_Val));

         Set_From_At_Mod (AtM_Nod);
         Insert_After (N, AtM_Nod);
         Set_Mod_Clause (N, Empty);
      end if;

      --  If the record representation clause has no components, then
      --  completely remove it.  Note that we also have to remove
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
