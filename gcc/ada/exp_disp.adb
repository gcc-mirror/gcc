------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Atag; use Exp_Atag;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Dbug; use Exp_Dbug;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Layout;   use Layout;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Disp is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Default_Prim_Op_Position (E : Entity_Id) return Uint;
   --  Ada 2005 (AI-251): Returns the fixed position in the dispatch table
   --  of the default primitive operations.

   function Has_DT (Typ : Entity_Id) return Boolean;
   pragma Inline (Has_DT);
   --  Returns true if we generate a dispatch table for tagged type Typ

   function Is_Predefined_Dispatching_Alias (Prim : Entity_Id) return Boolean;
   --  Returns true if Prim is not a predefined dispatching primitive but it is
   --  an alias of a predefined dispatching primitive (i.e. through a renaming)

   function New_Value (From : Node_Id) return Node_Id;
   --  From is the original Expression. New_Value is equivalent to a call
   --  to Duplicate_Subexpr with an explicit dereference when From is an
   --  access parameter.

   function Original_View_In_Visible_Part (Typ : Entity_Id) return Boolean;
   --  Check if the type has a private view or if the public view appears
   --  in the visible part of a package spec.

   function Prim_Op_Kind
     (Prim : Entity_Id;
      Typ  : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Determine the primitive operation kind of Prim
   --  according to its type Typ. Return a reference to an RE_Prim_Op_Kind
   --  enumeration value.

   function Tagged_Kind (T : Entity_Id) return Node_Id;
   --  Ada 2005 (AI-345): Determine the tagged kind of T and return a reference
   --  to an RE_Tagged_Kind enumeration value.

   ----------------------
   -- Apply_Tag_Checks --
   ----------------------

   procedure Apply_Tag_Checks (Call_Node : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (Call_Node);
      Ctrl_Arg   : constant Node_Id   := Controlling_Argument (Call_Node);
      Ctrl_Typ   : constant Entity_Id := Base_Type (Etype (Ctrl_Arg));
      Param_List : constant List_Id   := Parameter_Associations (Call_Node);

      Subp            : Entity_Id;
      CW_Typ          : Entity_Id;
      Param           : Node_Id;
      Typ             : Entity_Id;
      Eq_Prim_Op      : Entity_Id := Empty;

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("tagged types", Call_Node);
         return;
      end if;

      --  Apply_Tag_Checks is called directly from the semantics, so we need
      --  a check to see whether expansion is active before proceeding. In
      --  addition, there is no need to expand the call when compiling under
      --  restriction No_Dispatching_Calls; the semantic analyzer has
      --  previously notified the violation of this restriction.

      if not Expander_Active
        or else Restriction_Active (No_Dispatching_Calls)
      then
         return;
      end if;

      --  Set subprogram. If this is an inherited operation that was
      --  overridden, the body that is being called is its alias.

      Subp := Entity (Name (Call_Node));

      if Present (Alias (Subp))
        and then Is_Inherited_Operation (Subp)
        and then No (DTC_Entity (Subp))
      then
         Subp := Alias (Subp);
      end if;

      --  Definition of the class-wide type and the tagged type

      --  If the controlling argument is itself a tag rather than a tagged
      --  object, then use the class-wide type associated with the subprogram's
      --  controlling type. This case can occur when a call to an inherited
      --  primitive has an actual that originated from a default parameter
      --  given by a tag-indeterminate call and when there is no other
      --  controlling argument providing the tag (AI-239 requires dispatching).
      --  This capability of dispatching directly by tag is also needed by the
      --  implementation of AI-260 (for the generic dispatching constructors).

      if Ctrl_Typ = RTE (RE_Tag)
        or else (RTE_Available (RE_Interface_Tag)
                  and then Ctrl_Typ = RTE (RE_Interface_Tag))
      then
         CW_Typ := Class_Wide_Type (Find_Dispatching_Type (Subp));

      --  Class_Wide_Type is applied to the expressions used to initialize
      --  CW_Typ, to ensure that CW_Typ always denotes a class-wide type, since
      --  there are cases where the controlling type is resolved to a specific
      --  type (such as for designated types of arguments such as CW'Access).

      elsif Is_Access_Type (Ctrl_Typ) then
         CW_Typ := Class_Wide_Type (Designated_Type (Ctrl_Typ));

      else
         CW_Typ := Class_Wide_Type (Ctrl_Typ);
      end if;

      Typ := Root_Type (CW_Typ);

      if Ekind (Typ) = E_Incomplete_Type then
         Typ := Non_Limited_View (Typ);
      end if;

      if not Is_Limited_Type (Typ) then
         Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
      end if;

      --  Dispatching call to C++ primitive

      if Is_CPP_Class (Typ) then
         null;

      --  Dispatching call to Ada primitive

      elsif Present (Param_List) then

         --  Generate the Tag checks when appropriate

         Param := First_Actual (Call_Node);
         while Present (Param) loop

            --  No tag check with itself

            if Param = Ctrl_Arg then
               null;

            --  No tag check for parameter whose type is neither tagged nor
            --  access to tagged (for access parameters)

            elsif No (Find_Controlling_Arg (Param)) then
               null;

            --  No tag check for function dispatching on result if the
            --  Tag given by the context is this one

            elsif Find_Controlling_Arg (Param) = Ctrl_Arg then
               null;

            --  "=" is the only dispatching operation allowed to get
            --  operands with incompatible tags (it just returns false).
            --  We use Duplicate_Subexpr_Move_Checks instead of calling
            --  Relocate_Node because the value will be duplicated to
            --  check the tags.

            elsif Subp = Eq_Prim_Op then
               null;

            --  No check in presence of suppress flags

            elsif Tag_Checks_Suppressed (Etype (Param))
              or else (Is_Access_Type (Etype (Param))
                         and then Tag_Checks_Suppressed
                                    (Designated_Type (Etype (Param))))
            then
               null;

            --  Optimization: no tag checks if the parameters are identical

            elsif Is_Entity_Name (Param)
              and then Is_Entity_Name (Ctrl_Arg)
              and then Entity (Param) = Entity (Ctrl_Arg)
            then
               null;

            --  Now we need to generate the Tag check

            else
               --  Generate code for tag equality check
               --  Perhaps should have Checks.Apply_Tag_Equality_Check???

               Insert_Action (Ctrl_Arg,
                 Make_Implicit_If_Statement (Call_Node,
                   Condition =>
                     Make_Op_Ne (Loc,
                       Left_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix => New_Value (Ctrl_Arg),
                           Selector_Name =>
                             New_Reference_To
                               (First_Tag_Component (Typ), Loc)),

                       Right_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Unchecked_Convert_To (Typ, New_Value (Param)),
                           Selector_Name =>
                             New_Reference_To
                               (First_Tag_Component (Typ), Loc))),

                   Then_Statements =>
                     New_List (New_Constraint_Error (Loc))));
            end if;

            Next_Actual (Param);
         end loop;
      end if;
   end Apply_Tag_Checks;

   ------------------------
   -- Building_Static_DT --
   ------------------------

   function Building_Static_DT (Typ : Entity_Id) return Boolean is
      Root_Typ : Entity_Id := Root_Type (Typ);

   begin
      --  Handle private types

      if Present (Full_View (Root_Typ)) then
         Root_Typ := Full_View (Root_Typ);
      end if;

      return Static_Dispatch_Tables
        and then Is_Library_Level_Tagged_Type (Typ)

         --  If the type is derived from a CPP class we cannot statically
         --  build the dispatch tables because we must inherit primitives
         --  from the CPP side.

        and then not Is_CPP_Class (Root_Typ);
   end Building_Static_DT;

   ----------------------------------
   -- Build_Static_Dispatch_Tables --
   ----------------------------------

   procedure Build_Static_Dispatch_Tables (N : Entity_Id) is
      Target_List : List_Id;

      procedure Build_Dispatch_Tables (List : List_Id);
      --  Build the static dispatch table of tagged types found in the list of
      --  declarations. The generated nodes are added at the end of Target_List

      procedure Build_Package_Dispatch_Tables (N : Node_Id);
      --  Build static dispatch tables associated with package declaration N

      ---------------------------
      -- Build_Dispatch_Tables --
      ---------------------------

      procedure Build_Dispatch_Tables (List : List_Id) is
         D : Node_Id;

      begin
         D := First (List);
         while Present (D) loop

            --  Handle nested packages and package bodies recursively. The
            --  generated code is placed on the Target_List established for
            --  the enclosing compilation unit.

            if Nkind (D) = N_Package_Declaration then
               Build_Package_Dispatch_Tables (D);

            elsif Nkind (D) = N_Package_Body then
               Build_Dispatch_Tables (Declarations (D));

            elsif Nkind (D) = N_Package_Body_Stub
              and then Present (Library_Unit (D))
            then
               Build_Dispatch_Tables
                 (Declarations (Proper_Body (Unit (Library_Unit (D)))));

            --  Handle full type declarations and derivations of library
            --  level tagged types

            elsif Nkind_In (D, N_Full_Type_Declaration,
                               N_Derived_Type_Definition)
              and then Is_Library_Level_Tagged_Type (Defining_Entity (D))
              and then Ekind (Defining_Entity (D)) /= E_Record_Subtype
              and then not Is_Private_Type (Defining_Entity (D))
            then
               --  We do not generate dispatch tables for the internal types
               --  created for a type extension with unknown discriminants
               --  The needed information is shared with the source type,
               --  See Expand_N_Record_Extension.

               if Is_Underlying_Record_View (Defining_Entity (D))
                 or else
                  (not Comes_From_Source (Defining_Entity (D))
                     and then
                       Has_Unknown_Discriminants (Etype (Defining_Entity (D)))
                     and then
                       not Comes_From_Source
                             (First_Subtype (Defining_Entity (D))))
               then
                  null;
               else
                  Insert_List_After_And_Analyze (Last (Target_List),
                    Make_DT (Defining_Entity (D)));
               end if;

            --  Handle private types of library level tagged types. We must
            --  exchange the private and full-view to ensure the correct
            --  expansion. If the full view is a synchronized type ignore
            --  the type because the table will be built for the corresponding
            --  record type, that has its own declaration.

            elsif (Nkind (D) = N_Private_Type_Declaration
                     or else Nkind (D) = N_Private_Extension_Declaration)
               and then Present (Full_View (Defining_Entity (D)))
            then
               declare
                  E1 : constant Entity_Id := Defining_Entity (D);
                  E2 : constant Entity_Id := Full_View (E1);

               begin
                  if Is_Library_Level_Tagged_Type (E2)
                    and then Ekind (E2) /= E_Record_Subtype
                    and then not Is_Concurrent_Type (E2)
                  then
                     Exchange_Declarations (E1);
                     Insert_List_After_And_Analyze (Last (Target_List),
                       Make_DT (E1));
                     Exchange_Declarations (E2);
                  end if;
               end;
            end if;

            Next (D);
         end loop;
      end Build_Dispatch_Tables;

      -----------------------------------
      -- Build_Package_Dispatch_Tables --
      -----------------------------------

      procedure Build_Package_Dispatch_Tables (N : Node_Id) is
         Spec       : constant Node_Id   := Specification (N);
         Id         : constant Entity_Id := Defining_Entity (N);
         Vis_Decls  : constant List_Id   := Visible_Declarations (Spec);
         Priv_Decls : constant List_Id   := Private_Declarations (Spec);

      begin
         Push_Scope (Id);

         if Present (Priv_Decls) then
            Build_Dispatch_Tables (Vis_Decls);
            Build_Dispatch_Tables (Priv_Decls);

         elsif Present (Vis_Decls) then
            Build_Dispatch_Tables (Vis_Decls);
         end if;

         Pop_Scope;
      end Build_Package_Dispatch_Tables;

   --  Start of processing for Build_Static_Dispatch_Tables

   begin
      if not Expander_Active
        or else not Tagged_Type_Expansion
      then
         return;
      end if;

      if Nkind (N) = N_Package_Declaration then
         declare
            Spec       : constant Node_Id := Specification (N);
            Vis_Decls  : constant List_Id := Visible_Declarations (Spec);
            Priv_Decls : constant List_Id := Private_Declarations (Spec);

         begin
            if Present (Priv_Decls)
              and then Is_Non_Empty_List (Priv_Decls)
            then
               Target_List := Priv_Decls;

            elsif not Present (Vis_Decls) then
               Target_List := New_List;
               Set_Private_Declarations (Spec, Target_List);
            else
               Target_List := Vis_Decls;
            end if;

            Build_Package_Dispatch_Tables (N);
         end;

      else pragma Assert (Nkind (N) = N_Package_Body);
         Target_List := Declarations (N);
         Build_Dispatch_Tables (Target_List);
      end if;
   end Build_Static_Dispatch_Tables;

   ------------------------------
   -- Default_Prim_Op_Position --
   ------------------------------

   function Default_Prim_Op_Position (E : Entity_Id) return Uint is
      TSS_Name : TSS_Name_Type;

   begin
      Get_Name_String (Chars (E));
      TSS_Name :=
        TSS_Name_Type
          (Name_Buffer (Name_Len - TSS_Name'Length + 1 .. Name_Len));

      if Chars (E) = Name_uSize then
         return Uint_1;

      elsif Chars (E) = Name_uAlignment then
         return Uint_2;

      elsif TSS_Name = TSS_Stream_Read then
         return Uint_3;

      elsif TSS_Name = TSS_Stream_Write then
         return Uint_4;

      elsif TSS_Name = TSS_Stream_Input then
         return Uint_5;

      elsif TSS_Name = TSS_Stream_Output then
         return Uint_6;

      elsif Chars (E) = Name_Op_Eq then
         return Uint_7;

      elsif Chars (E) = Name_uAssign then
         return Uint_8;

      elsif TSS_Name = TSS_Deep_Adjust then
         return Uint_9;

      elsif TSS_Name = TSS_Deep_Finalize then
         return Uint_10;

      elsif Ada_Version >= Ada_05 then
         if Chars (E) = Name_uDisp_Asynchronous_Select then
            return Uint_11;

         elsif Chars (E) = Name_uDisp_Conditional_Select then
            return Uint_12;

         elsif Chars (E) = Name_uDisp_Get_Prim_Op_Kind then
            return Uint_13;

         elsif Chars (E) = Name_uDisp_Get_Task_Id then
            return Uint_14;

         elsif Chars (E) = Name_uDisp_Requeue then
            return Uint_15;

         elsif Chars (E) = Name_uDisp_Timed_Select then
            return Uint_16;
         end if;
      end if;

      raise Program_Error;
   end Default_Prim_Op_Position;

   -----------------------------
   -- Expand_Dispatching_Call --
   -----------------------------

   procedure Expand_Dispatching_Call (Call_Node : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (Call_Node);
      Call_Typ : constant Entity_Id  := Etype (Call_Node);

      Ctrl_Arg   : constant Node_Id   := Controlling_Argument (Call_Node);
      Ctrl_Typ   : constant Entity_Id := Base_Type (Etype (Ctrl_Arg));
      Param_List : constant List_Id   := Parameter_Associations (Call_Node);

      Subp            : Entity_Id;
      CW_Typ          : Entity_Id;
      New_Call        : Node_Id;
      New_Call_Name   : Node_Id;
      New_Params      : List_Id := No_List;
      Param           : Node_Id;
      Res_Typ         : Entity_Id;
      Subp_Ptr_Typ    : Entity_Id;
      Subp_Typ        : Entity_Id;
      Typ             : Entity_Id;
      Eq_Prim_Op      : Entity_Id := Empty;
      Controlling_Tag : Node_Id;

      function New_Value (From : Node_Id) return Node_Id;
      --  From is the original Expression. New_Value is equivalent to a call
      --  to Duplicate_Subexpr with an explicit dereference when From is an
      --  access parameter.

      ---------------
      -- New_Value --
      ---------------

      function New_Value (From : Node_Id) return Node_Id is
         Res : constant Node_Id := Duplicate_Subexpr (From);
      begin
         if Is_Access_Type (Etype (From)) then
            return
              Make_Explicit_Dereference (Sloc (From),
                Prefix => Res);
         else
            return Res;
         end if;
      end New_Value;

      --  Local variables

      New_Node  : Node_Id;
      SCIL_Node : Node_Id;

   --  Start of processing for Expand_Dispatching_Call

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("tagged types", Call_Node);
         return;
      end if;

      --  Expand_Dispatching_Call is called directly from the semantics,
      --  so we need a check to see whether expansion is active before
      --  proceeding. In addition, there is no need to expand the call
      --  if we are compiling under restriction No_Dispatching_Calls;
      --  the semantic analyzer has previously notified the violation
      --  of this restriction.

      if not Expander_Active
        or else Restriction_Active (No_Dispatching_Calls)
      then
         return;
      end if;

      --  Set subprogram. If this is an inherited operation that was
      --  overridden, the body that is being called is its alias.

      Subp := Entity (Name (Call_Node));

      if Present (Alias (Subp))
        and then Is_Inherited_Operation (Subp)
        and then No (DTC_Entity (Subp))
      then
         Subp := Alias (Subp);
      end if;

      --  Definition of the class-wide type and the tagged type

      --  If the controlling argument is itself a tag rather than a tagged
      --  object, then use the class-wide type associated with the subprogram's
      --  controlling type. This case can occur when a call to an inherited
      --  primitive has an actual that originated from a default parameter
      --  given by a tag-indeterminate call and when there is no other
      --  controlling argument providing the tag (AI-239 requires dispatching).
      --  This capability of dispatching directly by tag is also needed by the
      --  implementation of AI-260 (for the generic dispatching constructors).

      if Ctrl_Typ = RTE (RE_Tag)
        or else (RTE_Available (RE_Interface_Tag)
                  and then Ctrl_Typ = RTE (RE_Interface_Tag))
      then
         CW_Typ := Class_Wide_Type (Find_Dispatching_Type (Subp));

      --  Class_Wide_Type is applied to the expressions used to initialize
      --  CW_Typ, to ensure that CW_Typ always denotes a class-wide type, since
      --  there are cases where the controlling type is resolved to a specific
      --  type (such as for designated types of arguments such as CW'Access).

      elsif Is_Access_Type (Ctrl_Typ) then
         CW_Typ := Class_Wide_Type (Designated_Type (Ctrl_Typ));

      else
         CW_Typ := Class_Wide_Type (Ctrl_Typ);
      end if;

      Typ := Root_Type (CW_Typ);

      if Ekind (Typ) = E_Incomplete_Type then
         Typ := Non_Limited_View (Typ);
      end if;

      --  Generate the SCIL node for this dispatching call. The SCIL node for a
      --  dispatching call is inserted in the tree before the call is rewriten
      --  and expanded because the SCIL node must be found by the SCIL backend
      --  BEFORE the expanded nodes associated with the call node are found.

      if Generate_SCIL then
         SCIL_Node := Make_SCIL_Dispatching_Call (Sloc (Call_Node));
         Set_SCIL_Related_Node (SCIL_Node, Call_Node);
         Set_SCIL_Entity       (SCIL_Node, Typ);
         Set_SCIL_Target_Prim  (SCIL_Node, Subp);
         Insert_Action (Call_Node, SCIL_Node);
      end if;

      if not Is_Limited_Type (Typ) then
         Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
      end if;

      --  Dispatching call to C++ primitive. Create a new parameter list
      --  with no tag checks.

      New_Params := New_List;

      if Is_CPP_Class (Typ) then
         Param := First_Actual (Call_Node);
         while Present (Param) loop
            Append_To (New_Params, Relocate_Node (Param));
            Next_Actual (Param);
         end loop;

      --  Dispatching call to Ada primitive

      elsif Present (Param_List) then
         Apply_Tag_Checks (Call_Node);

         Param := First_Actual (Call_Node);
         while Present (Param) loop
            --  Cases in which we may have generated runtime checks

            if Param = Ctrl_Arg
              or else Subp = Eq_Prim_Op
            then
               Append_To (New_Params,
                 Duplicate_Subexpr_Move_Checks (Param));

            elsif Nkind (Parent (Param)) /= N_Parameter_Association
              or else not Is_Accessibility_Actual (Parent (Param))
            then
               Append_To (New_Params, Relocate_Node (Param));
            end if;

            Next_Actual (Param);
         end loop;
      end if;

      --  Generate the appropriate subprogram pointer type

      if Etype (Subp) = Typ then
         Res_Typ := CW_Typ;
      else
         Res_Typ := Etype (Subp);
      end if;

      Subp_Typ     := Create_Itype (E_Subprogram_Type, Call_Node);
      Subp_Ptr_Typ := Create_Itype (E_Access_Subprogram_Type, Call_Node);
      Set_Etype          (Subp_Typ, Res_Typ);
      Set_Returns_By_Ref (Subp_Typ, Returns_By_Ref (Subp));

      --  Create a new list of parameters which is a copy of the old formal
      --  list including the creation of a new set of matching entities.

      declare
         Old_Formal : Entity_Id := First_Formal (Subp);
         New_Formal : Entity_Id;
         Extra      : Entity_Id := Empty;

      begin
         if Present (Old_Formal) then
            New_Formal := New_Copy (Old_Formal);
            Set_First_Entity (Subp_Typ, New_Formal);
            Param := First_Actual (Call_Node);

            loop
               Set_Scope (New_Formal, Subp_Typ);

               --  Change all the controlling argument types to be class-wide
               --  to avoid a recursion in dispatching.

               if Is_Controlling_Formal (New_Formal) then
                  Set_Etype (New_Formal, Etype (Param));
               end if;

               --  If the type of the formal is an itype, there was code here
               --  introduced in 1998 in revision 1.46, to create a new itype
               --  by copy. This seems useless, and in fact leads to semantic
               --  errors when the itype is the completion of a type derived
               --  from a private type.

               Extra := New_Formal;
               Next_Formal (Old_Formal);
               exit when No (Old_Formal);

               Set_Next_Entity (New_Formal, New_Copy (Old_Formal));
               Next_Entity (New_Formal);
               Next_Actual (Param);
            end loop;

            Set_Next_Entity (New_Formal, Empty);
            Set_Last_Entity (Subp_Typ, Extra);
         end if;

         --  Now that the explicit formals have been duplicated, any extra
         --  formals needed by the subprogram must be created.

         if Present (Extra) then
            Set_Extra_Formal (Extra, Empty);
         end if;

         Create_Extra_Formals (Subp_Typ);
      end;

      --  Complete description of pointer type, including size information, as
      --  must be done with itypes to prevent order-of-elaboration anomalies
      --  in gigi.

      Set_Etype (Subp_Ptr_Typ, Subp_Ptr_Typ);
      Set_Directly_Designated_Type (Subp_Ptr_Typ, Subp_Typ);
      Set_Convention (Subp_Ptr_Typ, Convention (Subp_Typ));
      Layout_Type    (Subp_Ptr_Typ);

      --  If the controlling argument is a value of type Ada.Tag or an abstract
      --  interface class-wide type then use it directly. Otherwise, the tag
      --  must be extracted from the controlling object.

      if Ctrl_Typ = RTE (RE_Tag)
        or else (RTE_Available (RE_Interface_Tag)
                  and then Ctrl_Typ = RTE (RE_Interface_Tag))
      then
         Controlling_Tag := Duplicate_Subexpr (Ctrl_Arg);

      --  Extract the tag from an unchecked type conversion. Done to avoid
      --  the expansion of additional code just to obtain the value of such
      --  tag because the current management of interface type conversions
      --  generates in some cases this unchecked type conversion with the
      --  tag of the object (see Expand_Interface_Conversion).

      elsif Nkind (Ctrl_Arg) = N_Unchecked_Type_Conversion
        and then
          (Etype (Expression (Ctrl_Arg)) = RTE (RE_Tag)
            or else
              (RTE_Available (RE_Interface_Tag)
                and then
                  Etype (Expression (Ctrl_Arg)) = RTE (RE_Interface_Tag)))
      then
         Controlling_Tag := Duplicate_Subexpr (Expression (Ctrl_Arg));

      --  Ada 2005 (AI-251): Abstract interface class-wide type

      elsif Is_Interface (Ctrl_Typ)
        and then Is_Class_Wide_Type (Ctrl_Typ)
      then
         Controlling_Tag := Duplicate_Subexpr (Ctrl_Arg);

      else
         Controlling_Tag :=
           Make_Selected_Component (Loc,
             Prefix        => Duplicate_Subexpr_Move_Checks (Ctrl_Arg),
             Selector_Name => New_Reference_To (DTC_Entity (Subp), Loc));
      end if;

      --  Handle dispatching calls to predefined primitives

      if Is_Predefined_Dispatching_Operation (Subp)
        or else Is_Predefined_Dispatching_Alias (Subp)
      then
         Build_Get_Predefined_Prim_Op_Address (Loc,
           Tag_Node => Controlling_Tag,
           Position => DT_Position (Subp),
           New_Node => New_Node);

      --  Handle dispatching calls to user-defined primitives

      else
         Build_Get_Prim_Op_Address (Loc,
           Typ      => Find_Dispatching_Type (Subp),
           Tag_Node => Controlling_Tag,
           Position => DT_Position (Subp),
           New_Node => New_Node);
      end if;

      New_Call_Name :=
        Unchecked_Convert_To (Subp_Ptr_Typ, New_Node);

      --  Complete decoration of SCIL dispatching node. It must be done after
      --  the new call name is built to reference the nodes that will see the
      --  SCIL backend (because Build_Get_Prim_Op_Address generates an
      --  unchecked type conversion which relocates the controlling tag node).

      if Generate_SCIL then

         --  Common case: the controlling tag is the tag of an object
         --  (for example, obj.tag)

         if Nkind (Controlling_Tag) = N_Selected_Component then
            Set_SCIL_Controlling_Tag (SCIL_Node, Controlling_Tag);

         --  Handle renaming of selected component

         elsif Nkind (Controlling_Tag) = N_Identifier
           and then Nkind (Parent (Entity (Controlling_Tag))) =
                                             N_Object_Renaming_Declaration
           and then Nkind (Name (Parent (Entity (Controlling_Tag)))) =
                                             N_Selected_Component
         then
            Set_SCIL_Controlling_Tag (SCIL_Node,
              Name (Parent (Entity (Controlling_Tag))));

         --  If the controlling tag is an identifier, the SCIL node references
         --  the corresponding object or parameter declaration

         elsif Nkind (Controlling_Tag) = N_Identifier
           and then Nkind_In (Parent (Entity (Controlling_Tag)),
                              N_Object_Declaration,
                              N_Parameter_Specification)
         then
            Set_SCIL_Controlling_Tag (SCIL_Node,
              Parent (Entity (Controlling_Tag)));

         --  If the controlling tag is a dereference, the SCIL node references
         --  the corresponding object or parameter declaration

         elsif Nkind (Controlling_Tag) = N_Explicit_Dereference
            and then Nkind (Prefix (Controlling_Tag)) = N_Identifier
            and then Nkind_In (Parent (Entity (Prefix (Controlling_Tag))),
                               N_Object_Declaration,
                               N_Parameter_Specification)
         then
            Set_SCIL_Controlling_Tag (SCIL_Node,
              Parent (Entity (Prefix (Controlling_Tag))));

         --  For a direct reference of the tag of the type the SCIL node
         --  references the the internal object declaration containing the tag
         --  of the type.

         elsif Nkind (Controlling_Tag) = N_Attribute_Reference
            and then Attribute_Name (Controlling_Tag) = Name_Tag
         then
            Set_SCIL_Controlling_Tag (SCIL_Node,
              Parent
                (Node
                  (First_Elmt
                    (Access_Disp_Table (Entity (Prefix (Controlling_Tag)))))));

         --  Interfaces are not supported. For now we leave the SCIL node
         --  decorated with the Controlling_Tag. More work needed here???

         elsif Is_Interface (Etype (Controlling_Tag)) then
            Set_SCIL_Controlling_Tag (SCIL_Node, Controlling_Tag);

         else
            pragma Assert (False);
            null;
         end if;
      end if;

      if Nkind (Call_Node) = N_Function_Call then
         New_Call :=
           Make_Function_Call (Loc,
             Name                   => New_Call_Name,
             Parameter_Associations => New_Params);

         --  If this is a dispatching "=", we must first compare the tags so
         --  we generate: x.tag = y.tag and then x = y

         if Subp = Eq_Prim_Op then
            Param := First_Actual (Call_Node);
            New_Call :=
              Make_And_Then (Loc,
                Left_Opnd =>
                     Make_Op_Eq (Loc,
                       Left_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix        => New_Value (Param),
                           Selector_Name =>
                             New_Reference_To (First_Tag_Component (Typ),
                                               Loc)),

                       Right_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix        =>
                             Unchecked_Convert_To (Typ,
                               New_Value (Next_Actual (Param))),
                           Selector_Name =>
                             New_Reference_To
                               (First_Tag_Component (Typ), Loc))),
                Right_Opnd => New_Call);
         end if;

      else
         New_Call :=
           Make_Procedure_Call_Statement (Loc,
             Name                   => New_Call_Name,
             Parameter_Associations => New_Params);
      end if;

      Rewrite (Call_Node, New_Call);

      --  Suppress all checks during the analysis of the expanded code
      --  to avoid the generation of spurious warnings under ZFP run-time.

      Analyze_And_Resolve (Call_Node, Call_Typ, Suppress => All_Checks);
   end Expand_Dispatching_Call;

   ---------------------------------
   -- Expand_Interface_Conversion --
   ---------------------------------

   procedure Expand_Interface_Conversion
     (N         : Node_Id;
      Is_Static : Boolean := True)
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Etyp        : constant Entity_Id  := Etype (N);
      Operand     : constant Node_Id    := Expression (N);
      Operand_Typ : Entity_Id           := Etype (Operand);
      Func        : Node_Id;
      Iface_Typ   : Entity_Id           := Etype (N);
      Iface_Tag   : Entity_Id;

   begin
      --  Ada 2005 (AI-345): Handle synchronized interface type derivations

      if Is_Concurrent_Type (Operand_Typ) then
         Operand_Typ := Base_Type (Corresponding_Record_Type (Operand_Typ));
      end if;

      --  Handle access to class-wide interface types

      if Is_Access_Type (Iface_Typ) then
         Iface_Typ := Etype (Directly_Designated_Type (Iface_Typ));
      end if;

      --  Handle class-wide interface types. This conversion can appear
      --  explicitly in the source code. Example: I'Class (Obj)

      if Is_Class_Wide_Type (Iface_Typ) then
         Iface_Typ := Root_Type (Iface_Typ);
      end if;

      --  If the target type is a tagged synchronized type, the dispatch table
      --  info is in the corresponding record type.

      if Is_Concurrent_Type (Iface_Typ) then
         Iface_Typ := Corresponding_Record_Type (Iface_Typ);
      end if;

      --  Freeze the entity associated with the target interface to have
      --  available the attribute Access_Disp_Table.

      Freeze_Before (N, Iface_Typ);

      pragma Assert (not Is_Static
        or else (not Is_Class_Wide_Type (Iface_Typ)
                  and then Is_Interface (Iface_Typ)));

      if not Tagged_Type_Expansion then

         --  For VM, just do a conversion ???

         Rewrite (N, Unchecked_Convert_To (Etype (N), N));
         Analyze (N);
         return;
      end if;

      if not Is_Static then

         --  Give error if configurable run time and Displace not available

         if not RTE_Available (RE_Displace) then
            Error_Msg_CRT ("dynamic interface conversion", N);
            return;
         end if;

         --  Handle conversion of access-to-class-wide interface types. Target
         --  can be an access to an object or an access to another class-wide
         --  interface (see -1- and -2- in the following example):

         --     type Iface1_Ref is access all Iface1'Class;
         --     type Iface2_Ref is access all Iface1'Class;

         --     Acc1 : Iface1_Ref := new ...
         --     Obj  : Obj_Ref    := Obj_Ref (Acc);    -- 1
         --     Acc2 : Iface2_Ref := Iface2_Ref (Acc); -- 2

         if Is_Access_Type (Operand_Typ) then
            Rewrite (N,
              Unchecked_Convert_To (Etype (N),
                Make_Function_Call (Loc,
                  Name => New_Reference_To (RTE (RE_Displace), Loc),
                  Parameter_Associations => New_List (

                    Unchecked_Convert_To (RTE (RE_Address),
                      Relocate_Node (Expression (N))),

                    New_Occurrence_Of
                      (Node (First_Elmt (Access_Disp_Table (Iface_Typ))),
                       Loc)))));

            Analyze (N);
            return;
         end if;

         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (RTE (RE_Displace), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => Relocate_Node (Expression (N)),
                 Attribute_Name => Name_Address),

               New_Occurrence_Of
                 (Node (First_Elmt (Access_Disp_Table (Iface_Typ))),
                  Loc))));

         Analyze (N);

         --  If the target is a class-wide interface we change the type of the
         --  data returned by IW_Convert to indicate that this is a dispatching
         --  call.

         declare
            New_Itype : Entity_Id;

         begin
            New_Itype := Create_Itype (E_Anonymous_Access_Type, N);
            Set_Etype (New_Itype, New_Itype);
            Set_Directly_Designated_Type (New_Itype, Etyp);

            Rewrite (N,
              Make_Explicit_Dereference (Loc,
                Prefix =>
                  Unchecked_Convert_To (New_Itype, Relocate_Node (N))));
            Analyze (N);
            Freeze_Itype (New_Itype, N);

            return;
         end;
      end if;

      Iface_Tag := Find_Interface_Tag (Operand_Typ, Iface_Typ);
      pragma Assert (Iface_Tag /= Empty);

      --  Keep separate access types to interfaces because one internal
      --  function is used to handle the null value (see following comment)

      if not Is_Access_Type (Etype (N)) then
         Rewrite (N,
           Unchecked_Convert_To (Etype (N),
             Make_Selected_Component (Loc,
               Prefix => Relocate_Node (Expression (N)),
               Selector_Name =>
                 New_Occurrence_Of (Iface_Tag, Loc))));

      else
         --  Build internal function to handle the case in which the
         --  actual is null. If the actual is null returns null because
         --  no displacement is required; otherwise performs a type
         --  conversion that will be expanded in the code that returns
         --  the value of the displaced actual. That is:

         --     function Func (O : Address) return Iface_Typ is
         --        type Op_Typ is access all Operand_Typ;
         --        Aux : Op_Typ := To_Op_Typ (O);
         --     begin
         --        if O = Null_Address then
         --           return null;
         --        else
         --           return Iface_Typ!(Aux.Iface_Tag'Address);
         --        end if;
         --     end Func;

         declare
            Desig_Typ    : Entity_Id;
            Fent         : Entity_Id;
            New_Typ_Decl : Node_Id;
            Stats        : List_Id;

         begin
            Desig_Typ := Etype (Expression (N));

            if Is_Access_Type (Desig_Typ) then
               Desig_Typ :=
                 Available_View (Directly_Designated_Type (Desig_Typ));
            end if;

            if Is_Concurrent_Type (Desig_Typ) then
               Desig_Typ := Base_Type (Corresponding_Record_Type (Desig_Typ));
            end if;

            New_Typ_Decl :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, New_Internal_Name ('T')),
                Type_Definition =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present            => True,
                    Null_Exclusion_Present => False,
                    Constant_Present       => False,
                    Subtype_Indication     =>
                      New_Reference_To (Desig_Typ, Loc)));

            Stats := New_List (
              Make_Simple_Return_Statement (Loc,
                Unchecked_Convert_To (Etype (N),
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix =>
                          Unchecked_Convert_To
                            (Defining_Identifier (New_Typ_Decl),
                             Make_Identifier (Loc, Name_uO)),
                        Selector_Name =>
                          New_Occurrence_Of (Iface_Tag, Loc)),
                    Attribute_Name => Name_Address))));

            --  If the type is null-excluding, no need for the null branch.
            --  Otherwise we need to check for it and return null.

            if not Can_Never_Be_Null (Etype (N)) then
               Stats := New_List (
                 Make_If_Statement (Loc,
                  Condition       =>
                    Make_Op_Eq (Loc,
                       Left_Opnd  => Make_Identifier (Loc, Name_uO),
                       Right_Opnd => New_Reference_To
                                       (RTE (RE_Null_Address), Loc)),

                 Then_Statements => New_List (
                   Make_Simple_Return_Statement (Loc,
                     Make_Null (Loc))),
                 Else_Statements => Stats));
            end if;

            Fent :=
              Make_Defining_Identifier (Loc,
                New_Internal_Name ('F'));

            Func :=
              Make_Subprogram_Body (Loc,
                Specification =>
                  Make_Function_Specification (Loc,
                    Defining_Unit_Name => Fent,

                    Parameter_Specifications => New_List (
                      Make_Parameter_Specification (Loc,
                        Defining_Identifier =>
                          Make_Defining_Identifier (Loc, Name_uO),
                        Parameter_Type =>
                          New_Reference_To (RTE (RE_Address), Loc))),

                    Result_Definition =>
                      New_Reference_To (Etype (N), Loc)),

                Declarations => New_List (New_Typ_Decl),

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Stats));

            --  Place function body before the expression containing the
            --  conversion. We suppress all checks because the body of the
            --  internally generated function already takes care of the case
            --  in which the actual is null; therefore there is no need to
            --  double check that the pointer is not null when the program
            --  executes the alternative that performs the type conversion).

            Insert_Action (N, Func, Suppress => All_Checks);

            if Is_Access_Type (Etype (Expression (N))) then

               --  Generate: Func (Address!(Expression))

               Rewrite (N,
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (Fent, Loc),
                   Parameter_Associations => New_List (
                     Unchecked_Convert_To (RTE (RE_Address),
                       Relocate_Node (Expression (N))))));

            else
               --  Generate: Func (Operand_Typ!(Expression)'Address)

               Rewrite (N,
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (Fent, Loc),
                   Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix  => Unchecked_Convert_To (Operand_Typ,
                                    Relocate_Node (Expression (N))),
                       Attribute_Name => Name_Address))));
            end if;
         end;
      end if;

      Analyze (N);
   end Expand_Interface_Conversion;

   ------------------------------
   -- Expand_Interface_Actuals --
   ------------------------------

   procedure Expand_Interface_Actuals (Call_Node : Node_Id) is
      Actual     : Node_Id;
      Actual_Dup : Node_Id;
      Actual_Typ : Entity_Id;
      Anon       : Entity_Id;
      Conversion : Node_Id;
      Formal     : Entity_Id;
      Formal_Typ : Entity_Id;
      Subp       : Entity_Id;
      Formal_DDT : Entity_Id;
      Actual_DDT : Entity_Id;

   begin
      --  This subprogram is called directly from the semantics, so we need a
      --  check to see whether expansion is active before proceeding.

      if not Expander_Active then
         return;
      end if;

      --  Call using access to subprogram with explicit dereference

      if Nkind (Name (Call_Node)) = N_Explicit_Dereference then
         Subp := Etype (Name (Call_Node));

      --  Call using selected component

      elsif Nkind (Name (Call_Node)) = N_Selected_Component then
         Subp := Entity (Selector_Name (Name (Call_Node)));

      --  Call using direct name

      else
         Subp := Entity (Name (Call_Node));
      end if;

      --  Ada 2005 (AI-251): Look for interface type formals to force "this"
      --  displacement

      Formal := First_Formal (Subp);
      Actual := First_Actual (Call_Node);
      while Present (Formal) loop
         Formal_Typ := Etype (Formal);

         if Ekind (Formal_Typ) = E_Record_Type_With_Private then
            Formal_Typ := Full_View (Formal_Typ);
         end if;

         if Is_Access_Type (Formal_Typ) then
            Formal_DDT := Directly_Designated_Type (Formal_Typ);
         end if;

         Actual_Typ := Etype (Actual);

         if Is_Access_Type (Actual_Typ) then
            Actual_DDT := Directly_Designated_Type (Actual_Typ);
         end if;

         if Is_Interface (Formal_Typ)
           and then Is_Class_Wide_Type (Formal_Typ)
         then
            --  No need to displace the pointer if the type of the actual
            --  coindices with the type of the formal.

            if Actual_Typ = Formal_Typ then
               null;

            --  No need to displace the pointer if the interface type is
            --  a parent of the type of the actual because in this case the
            --  interface primitives are located in the primary dispatch table.

            elsif Is_Ancestor (Formal_Typ, Actual_Typ) then
               null;

            --  Implicit conversion to the class-wide formal type to force
            --  the displacement of the pointer.

            else
               Conversion := Convert_To (Formal_Typ, Relocate_Node (Actual));
               Rewrite (Actual, Conversion);
               Analyze_And_Resolve (Actual, Formal_Typ);
            end if;

         --  Access to class-wide interface type

         elsif Is_Access_Type (Formal_Typ)
           and then Is_Interface (Formal_DDT)
           and then Is_Class_Wide_Type (Formal_DDT)
           and then Interface_Present_In_Ancestor
                      (Typ   => Actual_DDT,
                       Iface => Etype (Formal_DDT))
         then
            --  Handle attributes 'Access and 'Unchecked_Access

            if Nkind (Actual) = N_Attribute_Reference
              and then
               (Attribute_Name (Actual) = Name_Access
                 or else Attribute_Name (Actual) = Name_Unchecked_Access)
            then
               --  This case must have been handled by the analysis and
               --  expansion of 'Access. The only exception is when types
               --  match and no further expansion is required.

               pragma Assert (Base_Type (Etype (Prefix (Actual)))
                               = Base_Type (Formal_DDT));
               null;

            --  No need to displace the pointer if the type of the actual
            --  coincides with the type of the formal.

            elsif Actual_DDT = Formal_DDT then
               null;

            --  No need to displace the pointer if the interface type is
            --  a parent of the type of the actual because in this case the
            --  interface primitives are located in the primary dispatch table.

            elsif Is_Ancestor (Formal_DDT, Actual_DDT) then
               null;

            else
               Actual_Dup := Relocate_Node (Actual);

               if From_With_Type (Actual_Typ) then

                  --  If the type of the actual parameter comes from a limited
                  --  with-clause and the non-limited view is already available
                  --  we replace the anonymous access type by a duplicate
                  --  declaration whose designated type is the non-limited view

                  if Ekind (Actual_DDT) = E_Incomplete_Type
                    and then Present (Non_Limited_View (Actual_DDT))
                  then
                     Anon := New_Copy (Actual_Typ);

                     if Is_Itype (Anon) then
                        Set_Scope (Anon, Current_Scope);
                     end if;

                     Set_Directly_Designated_Type (Anon,
                       Non_Limited_View (Actual_DDT));
                     Set_Etype (Actual_Dup, Anon);

                  elsif Is_Class_Wide_Type (Actual_DDT)
                    and then Ekind (Etype (Actual_DDT)) = E_Incomplete_Type
                    and then Present (Non_Limited_View (Etype (Actual_DDT)))
                  then
                     Anon := New_Copy (Actual_Typ);

                     if Is_Itype (Anon) then
                        Set_Scope (Anon, Current_Scope);
                     end if;

                     Set_Directly_Designated_Type (Anon,
                       New_Copy (Actual_DDT));
                     Set_Class_Wide_Type (Directly_Designated_Type (Anon),
                       New_Copy (Class_Wide_Type (Actual_DDT)));
                     Set_Etype (Directly_Designated_Type (Anon),
                       Non_Limited_View (Etype (Actual_DDT)));
                     Set_Etype (
                       Class_Wide_Type (Directly_Designated_Type (Anon)),
                       Non_Limited_View (Etype (Actual_DDT)));
                     Set_Etype (Actual_Dup, Anon);
                  end if;
               end if;

               Conversion := Convert_To (Formal_Typ, Actual_Dup);
               Rewrite (Actual, Conversion);
               Analyze_And_Resolve (Actual, Formal_Typ);
            end if;
         end if;

         Next_Actual (Actual);
         Next_Formal (Formal);
      end loop;
   end Expand_Interface_Actuals;

   ----------------------------
   -- Expand_Interface_Thunk --
   ----------------------------

   procedure Expand_Interface_Thunk
     (Prim       : Node_Id;
      Thunk_Id   : out Entity_Id;
      Thunk_Code : out Node_Id)
   is
      Loc             : constant Source_Ptr := Sloc (Prim);
      Actuals         : constant List_Id    := New_List;
      Decl            : constant List_Id    := New_List;
      Formals         : constant List_Id    := New_List;

      Controlling_Typ : Entity_Id;
      Decl_1          : Node_Id;
      Decl_2          : Node_Id;
      Formal          : Node_Id;
      New_Arg         : Node_Id;
      Offset_To_Top   : Node_Id;
      Target          : Entity_Id;
      Target_Formal   : Entity_Id;

   begin
      Thunk_Id   := Empty;
      Thunk_Code := Empty;

      --  Traverse the list of alias to find the final target

      Target := Prim;
      while Present (Alias (Target)) loop
         Target := Alias (Target);
      end loop;

      --  In case of primitives that are functions without formals and
      --  a controlling result there is no need to build the thunk.

      if not Present (First_Formal (Target)) then
         pragma Assert (Ekind (Target) = E_Function
           and then Has_Controlling_Result (Target));
         return;
      end if;

      --  Duplicate the formals

      Formal := First_Formal (Target);
      while Present (Formal) loop
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Sloc (Formal),
                 Chars => Chars (Formal)),
             In_Present => In_Present (Parent (Formal)),
             Out_Present => Out_Present (Parent (Formal)),
             Parameter_Type =>
               New_Reference_To (Etype (Formal), Loc),
             Expression => New_Copy_Tree (Expression (Parent (Formal)))));

         Next_Formal (Formal);
      end loop;

      Controlling_Typ := Find_Dispatching_Type (Target);

      Target_Formal := First_Formal (Target);
      Formal        := First (Formals);
      while Present (Formal) loop
         if Ekind (Target_Formal) = E_In_Parameter
           and then Ekind (Etype (Target_Formal)) = E_Anonymous_Access_Type
           and then Directly_Designated_Type (Etype (Target_Formal))
                     = Controlling_Typ
         then
            --  Generate:

            --     type T is access all <<type of the target formal>>
            --     S : Storage_Offset := Storage_Offset!(Formal)
            --                            - Offset_To_Top (address!(Formal))

            Decl_2 :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc,
                    New_Internal_Name ('T')),
                Type_Definition =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present            => True,
                    Null_Exclusion_Present => False,
                    Constant_Present       => False,
                    Subtype_Indication     =>
                      New_Reference_To
                        (Directly_Designated_Type
                          (Etype (Target_Formal)), Loc)));

            New_Arg :=
              Unchecked_Convert_To (RTE (RE_Address),
                New_Reference_To (Defining_Identifier (Formal), Loc));

            if not RTE_Available (RE_Offset_To_Top) then
               Offset_To_Top :=
                 Build_Offset_To_Top (Loc, New_Arg);
            else
               Offset_To_Top :=
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (RTE (RE_Offset_To_Top), Loc),
                   Parameter_Associations => New_List (New_Arg));
            end if;

            Decl_1 :=
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc,
                    New_Internal_Name ('S')),
                Constant_Present    => True,
                Object_Definition   =>
                  New_Reference_To (RTE (RE_Storage_Offset), Loc),
                Expression          =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd  =>
                      Unchecked_Convert_To
                        (RTE (RE_Storage_Offset),
                         New_Reference_To (Defining_Identifier (Formal), Loc)),
                     Right_Opnd =>
                       Offset_To_Top));

            Append_To (Decl, Decl_2);
            Append_To (Decl, Decl_1);

            --  Reference the new actual. Generate:
            --    T!(S)

            Append_To (Actuals,
              Unchecked_Convert_To
                (Defining_Identifier (Decl_2),
                 New_Reference_To (Defining_Identifier (Decl_1), Loc)));

         elsif Etype (Target_Formal) = Controlling_Typ then
            --  Generate:

            --     S1 : Storage_Offset := Storage_Offset!(Formal'Address)
            --                             - Offset_To_Top (Formal'Address)
            --     S2 : Addr_Ptr := Addr_Ptr!(S1)

            New_Arg :=
              Make_Attribute_Reference (Loc,
                Prefix =>
                  New_Reference_To (Defining_Identifier (Formal), Loc),
                Attribute_Name =>
                  Name_Address);

            if not RTE_Available (RE_Offset_To_Top) then
               Offset_To_Top :=
                 Build_Offset_To_Top (Loc, New_Arg);
            else
               Offset_To_Top :=
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (RTE (RE_Offset_To_Top), Loc),
                   Parameter_Associations => New_List (New_Arg));
            end if;

            Decl_1 :=
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, New_Internal_Name ('S')),
                Constant_Present    => True,
                Object_Definition   =>
                  New_Reference_To (RTE (RE_Storage_Offset), Loc),
                Expression          =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Unchecked_Convert_To
                        (RTE (RE_Storage_Offset),
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Reference_To
                               (Defining_Identifier (Formal), Loc),
                           Attribute_Name => Name_Address)),
                    Right_Opnd =>
                      Offset_To_Top));

            Decl_2 :=
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, New_Internal_Name ('S')),
                Constant_Present  => True,
                Object_Definition => New_Reference_To (RTE (RE_Addr_Ptr), Loc),
                Expression        =>
                  Unchecked_Convert_To
                    (RTE (RE_Addr_Ptr),
                     New_Reference_To (Defining_Identifier (Decl_1), Loc)));

            Append_To (Decl, Decl_1);
            Append_To (Decl, Decl_2);

            --  Reference the new actual. Generate:
            --    Target_Formal (S2.all)

            Append_To (Actuals,
              Unchecked_Convert_To
                (Etype (Target_Formal),
                 Make_Explicit_Dereference (Loc,
                   New_Reference_To (Defining_Identifier (Decl_2), Loc))));

         --  No special management required for this actual

         else
            Append_To (Actuals,
               New_Reference_To (Defining_Identifier (Formal), Loc));
         end if;

         Next_Formal (Target_Formal);
         Next (Formal);
      end loop;

      Thunk_Id :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('T'));

      Set_Is_Thunk (Thunk_Id);

      if Ekind (Target) = E_Procedure then
         Thunk_Code :=
           Make_Subprogram_Body (Loc,
              Specification =>
                Make_Procedure_Specification (Loc,
                  Defining_Unit_Name       => Thunk_Id,
                  Parameter_Specifications => Formals),
              Declarations => Decl,
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => New_List (
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Occurrence_Of (Target, Loc),
                      Parameter_Associations => Actuals))));

      else pragma Assert (Ekind (Target) = E_Function);

         Thunk_Code :=
           Make_Subprogram_Body (Loc,
              Specification =>
                Make_Function_Specification (Loc,
                  Defining_Unit_Name       => Thunk_Id,
                  Parameter_Specifications => Formals,
                  Result_Definition =>
                    New_Copy (Result_Definition (Parent (Target)))),
              Declarations => Decl,
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => New_List (
                    Make_Simple_Return_Statement (Loc,
                      Make_Function_Call (Loc,
                        Name => New_Occurrence_Of (Target, Loc),
                        Parameter_Associations => Actuals)))));
      end if;
   end Expand_Interface_Thunk;

   ------------
   -- Has_DT --
   ------------

   function Has_DT (Typ : Entity_Id) return Boolean is
   begin
      return not Is_Interface (Typ)
               and then not Restriction_Active (No_Dispatching_Calls);
   end Has_DT;

   -----------------------------------------
   -- Is_Predefined_Dispatching_Operation --
   -----------------------------------------

   function Is_Predefined_Dispatching_Operation
     (E : Entity_Id) return Boolean
   is
      TSS_Name : TSS_Name_Type;

   begin
      if not Is_Dispatching_Operation (E) then
         return False;
      end if;

      Get_Name_String (Chars (E));

      --  Most predefined primitives have internally generated names. Equality
      --  must be treated differently; the predefined operation is recognized
      --  as a homogeneous binary operator that returns Boolean.

      if Name_Len > TSS_Name_Type'Last then
         TSS_Name := TSS_Name_Type (Name_Buffer (Name_Len - TSS_Name'Length + 1
                                     .. Name_Len));
         if        Chars (E) = Name_uSize
           or else Chars (E) = Name_uAlignment
           or else TSS_Name  = TSS_Stream_Read
           or else TSS_Name  = TSS_Stream_Write
           or else TSS_Name  = TSS_Stream_Input
           or else TSS_Name  = TSS_Stream_Output
           or else
             (Chars (E) = Name_Op_Eq
                and then Etype (First_Entity (E)) = Etype (Last_Entity (E)))
           or else Chars (E) = Name_uAssign
           or else TSS_Name  = TSS_Deep_Adjust
           or else TSS_Name  = TSS_Deep_Finalize
           or else Is_Predefined_Interface_Primitive (E)
         then
            return True;
         end if;
      end if;

      return False;
   end Is_Predefined_Dispatching_Operation;

   ---------------------------------------
   -- Is_Predefined_Internal_Operation  --
   ---------------------------------------

   function Is_Predefined_Internal_Operation
     (E : Entity_Id) return Boolean
   is
      TSS_Name : TSS_Name_Type;

   begin
      if not Is_Dispatching_Operation (E) then
         return False;
      end if;

      Get_Name_String (Chars (E));

      --  Most predefined primitives have internally generated names. Equality
      --  must be treated differently; the predefined operation is recognized
      --  as a homogeneous binary operator that returns Boolean.

      if Name_Len > TSS_Name_Type'Last then
         TSS_Name :=
           TSS_Name_Type
             (Name_Buffer (Name_Len - TSS_Name'Length + 1 .. Name_Len));

         if        Chars (E) = Name_uSize
           or else Chars (E) = Name_uAlignment
           or else
             (Chars (E) = Name_Op_Eq
                and then Etype (First_Entity (E)) = Etype (Last_Entity (E)))
           or else Chars (E) = Name_uAssign
           or else TSS_Name  = TSS_Deep_Adjust
           or else TSS_Name  = TSS_Deep_Finalize
           or else Is_Predefined_Interface_Primitive (E)
         then
            return True;
         end if;
      end if;

      return False;
   end Is_Predefined_Internal_Operation;

   -------------------------------------
   -- Is_Predefined_Dispatching_Alias --
   -------------------------------------

   function Is_Predefined_Dispatching_Alias (Prim : Entity_Id) return Boolean
   is
      E : Entity_Id;

   begin
      if not Is_Predefined_Dispatching_Operation (Prim)
        and then Present (Alias (Prim))
      then
         E := Prim;
         while Present (Alias (E)) loop
            E := Alias (E);
         end loop;

         if Is_Predefined_Dispatching_Operation (E) then
            return True;
         end if;
      end if;

      return False;
   end Is_Predefined_Dispatching_Alias;

   ---------------------------------------
   -- Is_Predefined_Interface_Primitive --
   ---------------------------------------

   function Is_Predefined_Interface_Primitive (E : Entity_Id) return Boolean is
   begin
      return Ada_Version >= Ada_05
        and then (Chars (E) = Name_uDisp_Asynchronous_Select or else
                  Chars (E) = Name_uDisp_Conditional_Select  or else
                  Chars (E) = Name_uDisp_Get_Prim_Op_Kind    or else
                  Chars (E) = Name_uDisp_Get_Task_Id         or else
                  Chars (E) = Name_uDisp_Requeue             or else
                  Chars (E) = Name_uDisp_Timed_Select);
   end Is_Predefined_Interface_Primitive;

   ----------------------------------------
   -- Make_Disp_Asynchronous_Select_Body --
   ----------------------------------------

   --  For interface types, generate:

   --     procedure _Disp_Asynchronous_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        B : out System.Storage_Elements.Dummy_Communication_Block;
   --        F : out Boolean)
   --     is
   --     begin
   --        null;
   --     end _Disp_Asynchronous_Select;

   --  For protected types, generate:

   --     procedure _Disp_Asynchronous_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        B : out System.Storage_Elements.Dummy_Communication_Block;
   --        F : out Boolean)
   --     is
   --        I   : Integer :=
   --                Ada.Tags.Get_Entry_Index (Ada.Tags.Tag (<Typ>VP, S));
   --        Bnn : System.Tasking.Protected_Objects.Operations.
   --                Communication_Block;
   --     begin
   --        System.Tasking.Protected_Objects.Operations.Protected_Entry_Call
   --          (T._object'Access,
   --           System.Tasking.Protected_Objects.Protected_Entry_Index (I),
   --           P,
   --           System.Tasking.Asynchronous_Call,
   --           Bnn);
   --        B := System.Storage_Elements.Dummy_Communication_Block (Bnn);
   --     end _Disp_Asynchronous_Select;

   --  For task types, generate:

   --     procedure _Disp_Asynchronous_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        B : out System.Storage_Elements.Dummy_Communication_Block;
   --        F : out Boolean)
   --     is
   --        I   : Integer :=
   --                Ada.Tags.Get_Entry_Index (Ada.Tags.Tag (<Typ>VP, S));
   --     begin
   --        System.Tasking.Rendezvous.Task_Entry_Call
   --          (T._task_id,
   --           System.Tasking.Task_Entry_Index (I),
   --           P,
   --           System.Tasking.Asynchronous_Call,
   --           F);
   --     end _Disp_Asynchronous_Select;

   function Make_Disp_Asynchronous_Select_Body
     (Typ : Entity_Id) return Node_Id
   is
      Com_Block : Entity_Id;
      Conc_Typ  : Entity_Id           := Empty;
      Decls     : constant List_Id    := New_List;
      DT_Ptr    : Entity_Id;
      Loc       : constant Source_Ptr := Sloc (Typ);
      Obj_Ref   : Node_Id;
      Stmts     : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  Null body is generated for interface types

      if Is_Interface (Typ) then
         return
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Disp_Asynchronous_Select_Spec (Typ),
             Declarations =>
               New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 New_List (Make_Null_Statement (Loc))));
      end if;

      DT_Ptr := Node (First_Elmt (Access_Disp_Table (Typ)));

      if Is_Concurrent_Record_Type (Typ) then
         Conc_Typ := Corresponding_Concurrent_Type (Typ);

         --  Generate:
         --    I : Integer :=
         --          Ada.Tags.Get_Entry_Index (Ada.Tags.Tag! (<type>VP), S);

         --  where I will be used to capture the entry index of the primitive
         --  wrapper at position S.

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uI),
             Object_Definition =>
               New_Reference_To (Standard_Integer, Loc),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Get_Entry_Index), Loc),
                 Parameter_Associations =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         if Ekind (Conc_Typ) = E_Protected_Type then

            --  Generate:
            --    Bnn : Communication_Block;

            Com_Block :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('B'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Com_Block,
                Object_Definition =>
                  New_Reference_To (RTE (RE_Communication_Block), Loc)));

            --  Build T._object'Access for calls below

            Obj_Ref :=
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Unchecked_Access,
                 Prefix         =>
                   Make_Selected_Component (Loc,
                     Prefix        => Make_Identifier (Loc, Name_uT),
                     Selector_Name => Make_Identifier (Loc, Name_uObject)));

            case Corresponding_Runtime_Package (Conc_Typ) is
               when System_Tasking_Protected_Objects_Entries =>

                  --  Generate:
                  --    Protected_Entry_Call
                  --      (T._object'Access,            --  Object
                  --       Protected_Entry_Index! (I),  --  E
                  --       P,                           --  Uninterpreted_Data
                  --       Asynchronous_Call,           --  Mode
                  --       Bnn);                        --  Communication_Block

                  --  where T is the protected object, I is the entry index, P
                  --  is the wrapped parameters and B is the name of the
                  --  communication block.

                  Append_To (Stmts,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (RTE (RE_Protected_Entry_Call), Loc),
                      Parameter_Associations =>
                        New_List (
                          Obj_Ref,

                          Make_Unchecked_Type_Conversion (Loc,  --  entry index
                            Subtype_Mark =>
                              New_Reference_To
                                 (RTE (RE_Protected_Entry_Index), Loc),
                            Expression => Make_Identifier (Loc, Name_uI)),

                          Make_Identifier (Loc, Name_uP), --  parameter block
                          New_Reference_To (              --  Asynchronous_Call
                            RTE (RE_Asynchronous_Call), Loc),

                          New_Reference_To (Com_Block, Loc)))); -- comm block

               when System_Tasking_Protected_Objects_Single_Entry =>

                  --  Generate:
                  --    procedure Protected_Single_Entry_Call
                  --      (Object              : Protection_Entry_Access;
                  --       Uninterpreted_Data  : System.Address;
                  --       Mode                : Call_Modes);

                  Append_To (Stmts,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To
                          (RTE (RE_Protected_Single_Entry_Call), Loc),
                      Parameter_Associations =>
                        New_List (
                          Obj_Ref,

                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uP),
                            Attribute_Name => Name_Address),

                            New_Reference_To
                             (RTE (RE_Asynchronous_Call), Loc))));

               when others =>
                  raise Program_Error;
            end case;

            --  Generate:
            --    B := Dummy_Communication_Block (Bnn);

            Append_To (Stmts,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Identifier (Loc, Name_uB),
                Expression =>
                  Make_Unchecked_Type_Conversion (Loc,
                    Subtype_Mark =>
                      New_Reference_To (
                        RTE (RE_Dummy_Communication_Block), Loc),
                    Expression =>
                      New_Reference_To (Com_Block, Loc))));

         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);

            --  Generate:
            --    Task_Entry_Call
            --      (T._task_id,             --  Acceptor
            --       Task_Entry_Index! (I),  --  E
            --       P,                      --  Uninterpreted_Data
            --       Asynchronous_Call,      --  Mode
            --       F);                     --  Rendezvous_Successful

            --  where T is the task object, I is the entry index, P is the
            --  wrapped parameters and F is the status flag.

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Task_Entry_Call), Loc),
                Parameter_Associations =>
                  New_List (
                    Make_Selected_Component (Loc,         -- T._task_id
                      Prefix =>
                        Make_Identifier (Loc, Name_uT),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_uTask_Id)),

                    Make_Unchecked_Type_Conversion (Loc,  --  entry index
                      Subtype_Mark =>
                        New_Reference_To (RTE (RE_Task_Entry_Index), Loc),
                      Expression =>
                        Make_Identifier (Loc, Name_uI)),

                    Make_Identifier (Loc, Name_uP),       --  parameter block
                    New_Reference_To (                    --  Asynchronous_Call
                      RTE (RE_Asynchronous_Call), Loc),
                    Make_Identifier (Loc, Name_uF))));    --  status flag
         end if;

      else
         --  Ensure that the statements list is non-empty

         Append_To (Stmts, Make_Null_Statement (Loc));
      end if;

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Disp_Asynchronous_Select_Spec (Typ),
          Declarations =>
            Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Stmts));
   end Make_Disp_Asynchronous_Select_Body;

   ----------------------------------------
   -- Make_Disp_Asynchronous_Select_Spec --
   ----------------------------------------

   function Make_Disp_Asynchronous_Select_Spec
     (Typ : Entity_Id) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Typ);
      Def_Id : constant Node_Id    :=
                 Make_Defining_Identifier (Loc,
                   Name_uDisp_Asynchronous_Select);
      Params : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  T : in out Typ;                     --  Object parameter
      --  S : Integer;                        --  Primitive operation slot
      --  P : Address;                        --  Wrapped parameters
      --  B : out Dummy_Communication_Block;  --  Communication block dummy
      --  F : out Boolean;                    --  Status flag

      Append_List_To (Params, New_List (

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uT),
          Parameter_Type =>
            New_Reference_To (Typ, Loc),
          In_Present  => True,
          Out_Present => True),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uS),
          Parameter_Type =>
            New_Reference_To (Standard_Integer, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uP),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Address), Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uB),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Dummy_Communication_Block), Loc),
          Out_Present => True),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uF),
          Parameter_Type =>
            New_Reference_To (Standard_Boolean, Loc),
          Out_Present => True)));

      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => Def_Id,
          Parameter_Specifications => Params);
   end Make_Disp_Asynchronous_Select_Spec;

   ---------------------------------------
   -- Make_Disp_Conditional_Select_Body --
   ---------------------------------------

   --  For interface types, generate:

   --     procedure _Disp_Conditional_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        C : out Ada.Tags.Prim_Op_Kind;
   --        F : out Boolean)
   --     is
   --     begin
   --        null;
   --     end _Disp_Conditional_Select;

   --  For protected types, generate:

   --     procedure _Disp_Conditional_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        C : out Ada.Tags.Prim_Op_Kind;
   --        F : out Boolean)
   --     is
   --        I   : Integer;
   --        Bnn : System.Tasking.Protected_Objects.Operations.
   --                Communication_Block;

   --     begin
   --        C := Ada.Tags.Get_Prim_Op_Kind (Ada.Tags.Tag (<Typ>VP, S));

   --        if C = Ada.Tags.POK_Procedure
   --          or else C = Ada.Tags.POK_Protected_Procedure
   --          or else C = Ada.Tags.POK_Task_Procedure
   --        then
   --           F := True;
   --           return;
   --        end if;

   --        I := Ada.Tags.Get_Entry_Index (Ada.Tags.Tag (<Typ>VP, S));
   --        System.Tasking.Protected_Objects.Operations.Protected_Entry_Call
   --          (T.object'Access,
   --           System.Tasking.Protected_Objects.Protected_Entry_Index (I),
   --           P,
   --           System.Tasking.Conditional_Call,
   --           Bnn);
   --        F := not Cancelled (Bnn);
   --     end _Disp_Conditional_Select;

   --  For task types, generate:

   --     procedure _Disp_Conditional_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        C : out Ada.Tags.Prim_Op_Kind;
   --        F : out Boolean)
   --     is
   --        I : Integer;

   --     begin
   --        I := Ada.Tags.Get_Entry_Index (Ada.Tags.Tag (<Typ>VP, S));
   --        System.Tasking.Rendezvous.Task_Entry_Call
   --          (T._task_id,
   --           System.Tasking.Task_Entry_Index (I),
   --           P,
   --           System.Tasking.Conditional_Call,
   --           F);
   --     end _Disp_Conditional_Select;

   function Make_Disp_Conditional_Select_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Blk_Nam  : Entity_Id;
      Conc_Typ : Entity_Id           := Empty;
      Decls    : constant List_Id    := New_List;
      DT_Ptr   : Entity_Id;
      Obj_Ref  : Node_Id;
      Stmts    : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  Null body is generated for interface types

      if Is_Interface (Typ) then
         return
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Disp_Conditional_Select_Spec (Typ),
             Declarations =>
               No_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 New_List (Make_Null_Statement (Loc))));
      end if;

      DT_Ptr := Node (First_Elmt (Access_Disp_Table (Typ)));

      if Is_Concurrent_Record_Type (Typ) then
         Conc_Typ := Corresponding_Concurrent_Type (Typ);

         --  Generate:
         --    I : Integer;

         --  where I will be used to capture the entry index of the primitive
         --  wrapper at position S.

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uI),
             Object_Definition =>
               New_Reference_To (Standard_Integer, Loc)));

         --  Generate:
         --    C := Ada.Tags.Get_Prim_Op_Kind (Ada.Tags.Tag! (<type>VP), S);

         --    if C = POK_Procedure
         --      or else C = POK_Protected_Procedure
         --      or else C = POK_Task_Procedure;
         --    then
         --       F := True;
         --       return;
         --    end if;

         Build_Common_Dispatching_Select_Statements (Loc, DT_Ptr, Stmts);

         --  Generate:
         --    Bnn : Communication_Block;

         --  where Bnn is the name of the communication block used in the
         --  call to Protected_Entry_Call.

         Blk_Nam := Make_Defining_Identifier (Loc, New_Internal_Name ('B'));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Blk_Nam,
             Object_Definition =>
               New_Reference_To (RTE (RE_Communication_Block), Loc)));

         --  Generate:
         --    I := Ada.Tags.Get_Entry_Index (Ada.Tags.Tag! (<type>VP), S);

         --  I is the entry index and S is the dispatch table slot

         Append_To (Stmts,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Identifier (Loc, Name_uI),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Get_Entry_Index), Loc),
                 Parameter_Associations =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         if Ekind (Conc_Typ) = E_Protected_Type then

            Obj_Ref :=                                  -- T._object'Access
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Unchecked_Access,
                 Prefix         =>
                   Make_Selected_Component (Loc,
                     Prefix        => Make_Identifier (Loc, Name_uT),
                     Selector_Name => Make_Identifier (Loc, Name_uObject)));

            case Corresponding_Runtime_Package (Conc_Typ) is
               when System_Tasking_Protected_Objects_Entries =>
                  --  Generate:

                  --    Protected_Entry_Call
                  --      (T._object'Access,            --  Object
                  --       Protected_Entry_Index! (I),  --  E
                  --       P,                           --  Uninterpreted_Data
                  --       Conditional_Call,            --  Mode
                  --       Bnn);                        --  Block

                  --  where T is the protected object, I is the entry index, P
                  --  are the wrapped parameters and Bnn is the name of the
                  --  communication block.

                  Append_To (Stmts,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (RTE (RE_Protected_Entry_Call), Loc),
                      Parameter_Associations =>
                        New_List (
                          Obj_Ref,

                          Make_Unchecked_Type_Conversion (Loc,  --  entry index
                            Subtype_Mark =>
                              New_Reference_To
                                 (RTE (RE_Protected_Entry_Index), Loc),
                            Expression => Make_Identifier (Loc, Name_uI)),

                          Make_Identifier (Loc, Name_uP),  --  parameter block

                          New_Reference_To (               --  Conditional_Call
                            RTE (RE_Conditional_Call), Loc),
                          New_Reference_To (               --  Bnn
                            Blk_Nam, Loc))));

               when System_Tasking_Protected_Objects_Single_Entry =>

                  --    If we are compiling for a restricted run-time, the call
                  --    uses the simpler form.

                  Append_To (Stmts,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To
                          (RTE (RE_Protected_Single_Entry_Call), Loc),
                      Parameter_Associations =>
                        New_List (
                          Obj_Ref,

                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uP),
                            Attribute_Name => Name_Address),

                            New_Reference_To
                             (RTE (RE_Conditional_Call), Loc))));
               when others =>
                  raise Program_Error;
            end case;

            --  Generate:
            --    F := not Cancelled (Bnn);

            --  where F is the success flag. The status of Cancelled is negated
            --  in order to match the behaviour of the version for task types.

            Append_To (Stmts,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Identifier (Loc, Name_uF),
                Expression =>
                  Make_Op_Not (Loc,
                    Right_Opnd =>
                      Make_Function_Call (Loc,
                        Name =>
                          New_Reference_To (RTE (RE_Cancelled), Loc),
                        Parameter_Associations =>
                          New_List (
                            New_Reference_To (Blk_Nam, Loc))))));
         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);

            --  Generate:
            --    Task_Entry_Call
            --      (T._task_id,             --  Acceptor
            --       Task_Entry_Index! (I),  --  E
            --       P,                      --  Uninterpreted_Data
            --       Conditional_Call,       --  Mode
            --       F);                     --  Rendezvous_Successful

            --  where T is the task object, I is the entry index, P are the
            --  wrapped parameters and F is the status flag.

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Task_Entry_Call), Loc),
                Parameter_Associations =>
                  New_List (

                    Make_Selected_Component (Loc,         -- T._task_id
                      Prefix =>
                        Make_Identifier (Loc, Name_uT),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_uTask_Id)),

                    Make_Unchecked_Type_Conversion (Loc,  --  entry index
                      Subtype_Mark =>
                        New_Reference_To (RTE (RE_Task_Entry_Index), Loc),
                      Expression =>
                        Make_Identifier (Loc, Name_uI)),

                    Make_Identifier (Loc, Name_uP),       --  parameter block
                    New_Reference_To (                    --  Conditional_Call
                      RTE (RE_Conditional_Call), Loc),
                    Make_Identifier (Loc, Name_uF))));    --  status flag
         end if;

      else
         --  Ensure that the statements list is non-empty

         Append_To (Stmts, Make_Null_Statement (Loc));
      end if;

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Disp_Conditional_Select_Spec (Typ),
          Declarations =>
            Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Stmts));
   end Make_Disp_Conditional_Select_Body;

   ---------------------------------------
   -- Make_Disp_Conditional_Select_Spec --
   ---------------------------------------

   function Make_Disp_Conditional_Select_Spec
     (Typ : Entity_Id) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Typ);
      Def_Id : constant Node_Id    :=
                 Make_Defining_Identifier (Loc,
                   Name_uDisp_Conditional_Select);
      Params : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  T : in out Typ;        --  Object parameter
      --  S : Integer;           --  Primitive operation slot
      --  P : Address;           --  Wrapped parameters
      --  C : out Prim_Op_Kind;  --  Call kind
      --  F : out Boolean;       --  Status flag

      Append_List_To (Params, New_List (

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uT),
          Parameter_Type =>
            New_Reference_To (Typ, Loc),
          In_Present  => True,
          Out_Present => True),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uS),
          Parameter_Type =>
            New_Reference_To (Standard_Integer, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uP),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Address), Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uC),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Prim_Op_Kind), Loc),
          Out_Present => True),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uF),
          Parameter_Type =>
            New_Reference_To (Standard_Boolean, Loc),
          Out_Present => True)));

      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => Def_Id,
          Parameter_Specifications => Params);
   end Make_Disp_Conditional_Select_Spec;

   -------------------------------------
   -- Make_Disp_Get_Prim_Op_Kind_Body --
   -------------------------------------

   function Make_Disp_Get_Prim_Op_Kind_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Typ);
      DT_Ptr : Entity_Id;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      if Is_Interface (Typ) then
         return
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Disp_Get_Prim_Op_Kind_Spec (Typ),
             Declarations =>
               New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 New_List (Make_Null_Statement (Loc))));
      end if;

      DT_Ptr := Node (First_Elmt (Access_Disp_Table (Typ)));

      --  Generate:
      --    C := get_prim_op_kind (tag! (<type>VP), S);

      --  where C is the out parameter capturing the call kind and S is the
      --  dispatch table slot number.

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Disp_Get_Prim_Op_Kind_Spec (Typ),
          Declarations =>
            New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              New_List (
                Make_Assignment_Statement (Loc,
                  Name =>
                    Make_Identifier (Loc, Name_uC),
                  Expression =>
                    Make_Function_Call (Loc,
                      Name =>
                        New_Reference_To (RTE (RE_Get_Prim_Op_Kind), Loc),
                      Parameter_Associations => New_List (
                        Unchecked_Convert_To (RTE (RE_Tag),
                          New_Reference_To (DT_Ptr, Loc)),
                          Make_Identifier (Loc, Name_uS)))))));
   end Make_Disp_Get_Prim_Op_Kind_Body;

   -------------------------------------
   -- Make_Disp_Get_Prim_Op_Kind_Spec --
   -------------------------------------

   function Make_Disp_Get_Prim_Op_Kind_Spec
     (Typ : Entity_Id) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Typ);
      Def_Id : constant Node_Id    :=
                 Make_Defining_Identifier (Loc,
                   Name_uDisp_Get_Prim_Op_Kind);
      Params : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  T : in out Typ;       --  Object parameter
      --  S : Integer;          --  Primitive operation slot
      --  C : out Prim_Op_Kind; --  Call kind

      Append_List_To (Params, New_List (

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uT),
          Parameter_Type =>
            New_Reference_To (Typ, Loc),
          In_Present  => True,
          Out_Present => True),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uS),
          Parameter_Type =>
            New_Reference_To (Standard_Integer, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uC),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Prim_Op_Kind), Loc),
          Out_Present => True)));

      return
        Make_Procedure_Specification (Loc,
           Defining_Unit_Name       => Def_Id,
           Parameter_Specifications => Params);
   end Make_Disp_Get_Prim_Op_Kind_Spec;

   --------------------------------
   -- Make_Disp_Get_Task_Id_Body --
   --------------------------------

   function Make_Disp_Get_Task_Id_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);
      Ret : Node_Id;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      if Is_Concurrent_Record_Type (Typ)
        and then Ekind (Corresponding_Concurrent_Type (Typ)) = E_Task_Type
      then
         --  Generate:
         --    return To_Address (_T._task_id);

         Ret :=
           Make_Simple_Return_Statement (Loc,
             Expression =>
               Make_Unchecked_Type_Conversion (Loc,
                 Subtype_Mark =>
                   New_Reference_To (RTE (RE_Address), Loc),
                 Expression =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       Make_Identifier (Loc, Name_uT),
                     Selector_Name =>
                       Make_Identifier (Loc, Name_uTask_Id))));

      --  A null body is constructed for non-task types

      else
         --  Generate:
         --    return Null_Address;

         Ret :=
           Make_Simple_Return_Statement (Loc,
             Expression =>
               New_Reference_To (RTE (RE_Null_Address), Loc));
      end if;

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Disp_Get_Task_Id_Spec (Typ),
          Declarations =>
            New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              New_List (Ret)));
   end Make_Disp_Get_Task_Id_Body;

   --------------------------------
   -- Make_Disp_Get_Task_Id_Spec --
   --------------------------------

   function Make_Disp_Get_Task_Id_Spec
     (Typ : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      return
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Make_Defining_Identifier (Loc, Name_uDisp_Get_Task_Id),
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_uT),
              Parameter_Type =>
                New_Reference_To (Typ, Loc))),
          Result_Definition =>
            New_Reference_To (RTE (RE_Address), Loc));
   end Make_Disp_Get_Task_Id_Spec;

   ----------------------------
   -- Make_Disp_Requeue_Body --
   ----------------------------

   function Make_Disp_Requeue_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Conc_Typ : Entity_Id           := Empty;
      Stmts    : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  Null body is generated for interface types and non-concurrent
      --  tagged types.

      if Is_Interface (Typ)
        or else not Is_Concurrent_Record_Type (Typ)
      then
         return
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Disp_Requeue_Spec (Typ),
             Declarations =>
               No_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 New_List (Make_Null_Statement (Loc))));
      end if;

      Conc_Typ := Corresponding_Concurrent_Type (Typ);

      if Ekind (Conc_Typ) = E_Protected_Type then

         --  Generate statements:
         --    if F then
         --       System.Tasking.Protected_Objects.Operations.
         --         Requeue_Protected_Entry
         --           (Protection_Entries_Access (P),
         --            O._object'Unchecked_Access,
         --            Protected_Entry_Index (I),
         --            A);
         --    else
         --       System.Tasking.Protected_Objects.Operations.
         --         Requeue_Task_To_Protected_Entry
         --           (O._object'Unchecked_Access,
         --            Protected_Entry_Index (I),
         --            A);
         --    end if;

         if Restriction_Active (No_Entry_Queue) then
            Append_To (Stmts, Make_Null_Statement (Loc));
         else
            Append_To (Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Identifier (Loc, Name_uF),

                Then_Statements =>
                  New_List (

                     --  Call to Requeue_Protected_Entry

                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (
                          RTE (RE_Requeue_Protected_Entry), Loc),
                      Parameter_Associations =>
                        New_List (

                          Make_Unchecked_Type_Conversion (Loc,  -- PEA (P)
                            Subtype_Mark =>
                              New_Reference_To (
                                RTE (RE_Protection_Entries_Access), Loc),
                            Expression =>
                              Make_Identifier (Loc, Name_uP)),

                          Make_Attribute_Reference (Loc,      -- O._object'Acc
                            Attribute_Name =>
                              Name_Unchecked_Access,
                            Prefix =>
                              Make_Selected_Component (Loc,
                                Prefix =>
                                  Make_Identifier (Loc, Name_uO),
                                Selector_Name =>
                                  Make_Identifier (Loc, Name_uObject))),

                          Make_Unchecked_Type_Conversion (Loc,  -- entry index
                            Subtype_Mark =>
                              New_Reference_To (
                                RTE (RE_Protected_Entry_Index), Loc),
                            Expression =>
                              Make_Identifier (Loc, Name_uI)),

                          Make_Identifier (Loc, Name_uA)))),   -- abort status

                Else_Statements =>
                  New_List (

                     --  Call to Requeue_Task_To_Protected_Entry

                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (
                          RTE (RE_Requeue_Task_To_Protected_Entry), Loc),
                      Parameter_Associations =>
                        New_List (

                          Make_Attribute_Reference (Loc,     -- O._object'Acc
                            Attribute_Name =>
                              Name_Unchecked_Access,
                            Prefix =>
                              Make_Selected_Component (Loc,
                                Prefix =>
                                  Make_Identifier (Loc, Name_uO),
                                Selector_Name =>
                                  Make_Identifier (Loc, Name_uObject))),

                          Make_Unchecked_Type_Conversion (Loc, -- entry index
                            Subtype_Mark =>
                              New_Reference_To (
                                RTE (RE_Protected_Entry_Index), Loc),
                            Expression =>
                              Make_Identifier (Loc, Name_uI)),

                          Make_Identifier (Loc, Name_uA)))))); -- abort status
         end if;
      else
         pragma Assert (Is_Task_Type (Conc_Typ));

         --  Generate:
         --    if F then
         --       System.Tasking.Rendezvous.Requeue_Protected_To_Task_Entry
         --         (Protection_Entries_Access (P),
         --          O._task_id,
         --          Task_Entry_Index (I),
         --          A);
         --    else
         --       System.Tasking.Rendezvous.Requeue_Task_Entry
         --         (O._task_id,
         --          Task_Entry_Index (I),
         --          A);
         --    end if;

         Append_To (Stmts,
           Make_If_Statement (Loc,
             Condition =>
               Make_Identifier (Loc, Name_uF),

             Then_Statements =>
               New_List (

                  --  Call to Requeue_Protected_To_Task_Entry

                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Reference_To (
                       RTE (RE_Requeue_Protected_To_Task_Entry), Loc),

                   Parameter_Associations =>
                     New_List (

                       Make_Unchecked_Type_Conversion (Loc,  -- PEA (P)
                         Subtype_Mark =>
                           New_Reference_To (
                             RTE (RE_Protection_Entries_Access), Loc),
                         Expression =>
                           Make_Identifier (Loc, Name_uP)),

                       Make_Selected_Component (Loc,         -- O._task_id
                         Prefix =>
                           Make_Identifier (Loc, Name_uO),
                         Selector_Name =>
                           Make_Identifier (Loc, Name_uTask_Id)),

                       Make_Unchecked_Type_Conversion (Loc,  -- entry index
                         Subtype_Mark =>
                           New_Reference_To (
                             RTE (RE_Task_Entry_Index), Loc),
                         Expression =>
                           Make_Identifier (Loc, Name_uI)),

                       Make_Identifier (Loc, Name_uA)))),    -- abort status

             Else_Statements =>
               New_List (

                  --  Call to Requeue_Task_Entry

                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Reference_To (RTE (RE_Requeue_Task_Entry), Loc),

                   Parameter_Associations =>
                     New_List (

                       Make_Selected_Component (Loc,         -- O._task_id
                         Prefix =>
                           Make_Identifier (Loc, Name_uO),
                         Selector_Name =>
                           Make_Identifier (Loc, Name_uTask_Id)),

                       Make_Unchecked_Type_Conversion (Loc,  -- entry index
                         Subtype_Mark =>
                           New_Reference_To (
                             RTE (RE_Task_Entry_Index), Loc),
                         Expression =>
                           Make_Identifier (Loc, Name_uI)),

                       Make_Identifier (Loc, Name_uA))))));  -- abort status
      end if;

      --  Even though no declarations are needed in both cases, we allocate
      --  a list for entities added by Freeze.

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Disp_Requeue_Spec (Typ),
          Declarations =>
            New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Stmts));
   end Make_Disp_Requeue_Body;

   ----------------------------
   -- Make_Disp_Requeue_Spec --
   ----------------------------

   function Make_Disp_Requeue_Spec
     (Typ : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  O : in out Typ;   -  Object parameter
      --  F : Boolean;      -  Protected (True) / task (False) flag
      --  P : Address;      -  Protection_Entries_Access value
      --  I : Entry_Index   -  Index of entry call
      --  A : Boolean       -  Abort flag

      --  Note that the Protection_Entries_Access value is represented as a
      --  System.Address in order to avoid dragging in the tasking runtime
      --  when compiling sources without tasking constructs.

      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name =>
            Make_Defining_Identifier (Loc, Name_uDisp_Requeue),

          Parameter_Specifications =>
            New_List (

              Make_Parameter_Specification (Loc,             --  O
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uO),
                Parameter_Type =>
                  New_Reference_To (Typ, Loc),
                In_Present  => True,
                Out_Present => True),

              Make_Parameter_Specification (Loc,             --  F
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uF),
                Parameter_Type =>
                  New_Reference_To (Standard_Boolean, Loc)),

              Make_Parameter_Specification (Loc,             --  P
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uP),
                Parameter_Type =>
                  New_Reference_To (RTE (RE_Address), Loc)),

              Make_Parameter_Specification (Loc,             --  I
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uI),
                Parameter_Type =>
                  New_Reference_To (Standard_Integer, Loc)),

              Make_Parameter_Specification (Loc,             --  A
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uA),
                Parameter_Type =>
                  New_Reference_To (Standard_Boolean, Loc))));
   end Make_Disp_Requeue_Spec;

   ---------------------------------
   -- Make_Disp_Timed_Select_Body --
   ---------------------------------

   --  For interface types, generate:

   --     procedure _Disp_Timed_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        D : Duration;
   --        M : Integer;
   --        C : out Ada.Tags.Prim_Op_Kind;
   --        F : out Boolean)
   --     is
   --     begin
   --        null;
   --     end _Disp_Timed_Select;

   --  For protected types, generate:

   --     procedure _Disp_Timed_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        D : Duration;
   --        M : Integer;
   --        C : out Ada.Tags.Prim_Op_Kind;
   --        F : out Boolean)
   --     is
   --        I : Integer;

   --     begin
   --        C := Ada.Tags.Get_Prim_Op_Kind (Ada.Tags.Tag (<Typ>VP), S);

   --        if C = Ada.Tags.POK_Procedure
   --          or else C = Ada.Tags.POK_Protected_Procedure
   --          or else C = Ada.Tags.POK_Task_Procedure
   --        then
   --           F := True;
   --           return;
   --        end if;

   --        I := Ada.Tags.Get_Entry_Index (Ada.Tags.Tag (<Typ>VP), S);
   --        System.Tasking.Protected_Objects.Operations.
   --          Timed_Protected_Entry_Call
   --            (T._object'Access,
   --             System.Tasking.Protected_Objects.Protected_Entry_Index (I),
   --             P,
   --             D,
   --             M,
   --             F);
   --     end _Disp_Timed_Select;

   --  For task types, generate:

   --     procedure _Disp_Timed_Select
   --       (T : in out <Typ>;
   --        S : Integer;
   --        P : System.Address;
   --        D : Duration;
   --        M : Integer;
   --        C : out Ada.Tags.Prim_Op_Kind;
   --        F : out Boolean)
   --     is
   --        I : Integer;

   --     begin
   --        I := Ada.Tags.Get_Entry_Index (Ada.Tags.Tag (<Typ>VP), S);
   --        System.Tasking.Rendezvous.Timed_Task_Entry_Call
   --          (T._task_id,
   --           System.Tasking.Task_Entry_Index (I),
   --           P,
   --           D,
   --           M,
   --           D);
   --     end _Disp_Time_Select;

   function Make_Disp_Timed_Select_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Conc_Typ : Entity_Id           := Empty;
      Decls    : constant List_Id    := New_List;
      DT_Ptr   : Entity_Id;
      Obj_Ref  : Node_Id;
      Stmts    : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  Null body is generated for interface types

      if Is_Interface (Typ) then
         return
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Disp_Timed_Select_Spec (Typ),
             Declarations =>
               New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 New_List (Make_Null_Statement (Loc))));
      end if;

      DT_Ptr := Node (First_Elmt (Access_Disp_Table (Typ)));

      if Is_Concurrent_Record_Type (Typ) then
         Conc_Typ := Corresponding_Concurrent_Type (Typ);

         --  Generate:
         --    I : Integer;

         --  where I will be used to capture the entry index of the primitive
         --  wrapper at position S.

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uI),
             Object_Definition =>
               New_Reference_To (Standard_Integer, Loc)));

         --  Generate:
         --    C := Get_Prim_Op_Kind (tag! (<type>VP), S);

         --    if C = POK_Procedure
         --      or else C = POK_Protected_Procedure
         --      or else C = POK_Task_Procedure;
         --    then
         --       F := True;
         --       return;
         --    end if;

         Build_Common_Dispatching_Select_Statements (Loc, DT_Ptr, Stmts);

         --  Generate:
         --    I := Get_Entry_Index (tag! (<type>VP), S);

         --  I is the entry index and S is the dispatch table slot

         Append_To (Stmts,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Identifier (Loc, Name_uI),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Get_Entry_Index), Loc),
                 Parameter_Associations =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         --  Protected case

         if Ekind (Conc_Typ) = E_Protected_Type then

            --  Build T._object'Access

            Obj_Ref :=
               Make_Attribute_Reference (Loc,
                  Attribute_Name => Name_Unchecked_Access,
                  Prefix         =>
                    Make_Selected_Component (Loc,
                      Prefix        => Make_Identifier (Loc, Name_uT),
                      Selector_Name => Make_Identifier (Loc, Name_uObject)));

            --  Normal case, No_Entry_Queue restriction not active. In this
            --  case we generate:

            --   Timed_Protected_Entry_Call
            --     (T._object'access,
            --      Protected_Entry_Index! (I),
            --      P, D, M, F);

            --  where T is the protected object, I is the entry index, P are
            --  the wrapped parameters, D is the delay amount, M is the delay
            --  mode and F is the status flag.

            case Corresponding_Runtime_Package (Conc_Typ) is
               when System_Tasking_Protected_Objects_Entries =>
                  Append_To (Stmts,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To
                          (RTE (RE_Timed_Protected_Entry_Call), Loc),
                      Parameter_Associations =>
                        New_List (
                          Obj_Ref,

                          Make_Unchecked_Type_Conversion (Loc,  --  entry index
                            Subtype_Mark =>
                              New_Reference_To
                                (RTE (RE_Protected_Entry_Index), Loc),
                            Expression =>
                              Make_Identifier (Loc, Name_uI)),

                          Make_Identifier (Loc, Name_uP),   --  parameter block
                          Make_Identifier (Loc, Name_uD),   --  delay
                          Make_Identifier (Loc, Name_uM),   --  delay mode
                          Make_Identifier (Loc, Name_uF)))); --  status flag

               when System_Tasking_Protected_Objects_Single_Entry =>
                  --  Generate:

                  --   Timed_Protected_Single_Entry_Call
                  --     (T._object'access, P, D, M, F);

                  --  where T is the protected object, P is the wrapped
                  --  parameters, D is the delay amount, M is the delay mode, F
                  --  is the status flag.

                  Append_To (Stmts,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To
                          (RTE (RE_Timed_Protected_Single_Entry_Call), Loc),
                      Parameter_Associations =>
                        New_List (
                          Obj_Ref,
                          Make_Identifier (Loc, Name_uP),   --  parameter block
                          Make_Identifier (Loc, Name_uD),   --  delay
                          Make_Identifier (Loc, Name_uM),   --  delay mode
                          Make_Identifier (Loc, Name_uF)))); --  status flag

               when others =>
                  raise Program_Error;
            end case;

         --  Task case

         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);

            --  Generate:
            --    Timed_Task_Entry_Call (
            --      T._task_id,
            --      Task_Entry_Index! (I),
            --      P,
            --      D,
            --      M,
            --      F);

            --  where T is the task object, I is the entry index, P are the
            --  wrapped parameters, D is the delay amount, M is the delay
            --  mode and F is the status flag.

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Timed_Task_Entry_Call), Loc),
                Parameter_Associations =>
                  New_List (

                    Make_Selected_Component (Loc,         --  T._task_id
                      Prefix =>
                        Make_Identifier (Loc, Name_uT),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_uTask_Id)),

                    Make_Unchecked_Type_Conversion (Loc,  --  entry index
                      Subtype_Mark =>
                        New_Reference_To (RTE (RE_Task_Entry_Index), Loc),
                      Expression =>
                        Make_Identifier (Loc, Name_uI)),

                    Make_Identifier (Loc, Name_uP),       --  parameter block
                    Make_Identifier (Loc, Name_uD),       --  delay
                    Make_Identifier (Loc, Name_uM),       --  delay mode
                    Make_Identifier (Loc, Name_uF))));    --  status flag
         end if;

      else
         --  Ensure that the statements list is non-empty

         Append_To (Stmts, Make_Null_Statement (Loc));
      end if;

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Disp_Timed_Select_Spec (Typ),
          Declarations =>
            Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Stmts));
   end Make_Disp_Timed_Select_Body;

   ---------------------------------
   -- Make_Disp_Timed_Select_Spec --
   ---------------------------------

   function Make_Disp_Timed_Select_Spec
     (Typ : Entity_Id) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Typ);
      Def_Id : constant Node_Id    :=
                 Make_Defining_Identifier (Loc,
                   Name_uDisp_Timed_Select);
      Params : constant List_Id    := New_List;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      --  T : in out Typ;        --  Object parameter
      --  S : Integer;           --  Primitive operation slot
      --  P : Address;           --  Wrapped parameters
      --  D : Duration;          --  Delay
      --  M : Integer;           --  Delay Mode
      --  C : out Prim_Op_Kind;  --  Call kind
      --  F : out Boolean;       --  Status flag

      Append_List_To (Params, New_List (

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uT),
          Parameter_Type =>
            New_Reference_To (Typ, Loc),
          In_Present  => True,
          Out_Present => True),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uS),
          Parameter_Type =>
            New_Reference_To (Standard_Integer, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uP),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Address), Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uD),
          Parameter_Type =>
            New_Reference_To (Standard_Duration, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uM),
          Parameter_Type =>
            New_Reference_To (Standard_Integer, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uC),
          Parameter_Type =>
            New_Reference_To (RTE (RE_Prim_Op_Kind), Loc),
          Out_Present => True)));

      Append_To (Params,
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uF),
          Parameter_Type =>
            New_Reference_To (Standard_Boolean, Loc),
          Out_Present => True));

      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => Def_Id,
          Parameter_Specifications => Params);
   end Make_Disp_Timed_Select_Spec;

   -------------
   -- Make_DT --
   -------------

   --  The frontend supports two models for expanding dispatch tables
   --  associated with library-level defined tagged types: statically
   --  and non-statically allocated dispatch tables. In the former case
   --  the object containing the dispatch table is constant and it is
   --  initialized by means of a positional aggregate. In the latter case,
   --  the object containing the dispatch table is a variable which is
   --  initialized by means of assignments.

   --  In case of locally defined tagged types, the object containing the
   --  object containing the dispatch table is always a variable (instead
   --  of a constant). This is currently required to give support to late
   --  overriding of primitives. For example:

   --     procedure Example is
   --        package Pkg is
   --           type T1 is tagged null record;
   --           procedure Prim (O : T1);
   --        end Pkg;

   --        type T2 is new Pkg.T1 with null record;
   --        procedure Prim (X : T2) is    -- late overriding
   --        begin
   --           ...
   --     ...
   --     end;

   function Make_DT (Typ : Entity_Id; N : Node_Id := Empty) return List_Id is
      Loc : constant Source_Ptr := Sloc (Typ);

      Max_Predef_Prims : constant Int :=
                           UI_To_Int
                             (Intval
                               (Expression
                                 (Parent (RTE (RE_Max_Predef_Prims)))));

      DT_Decl : constant Elist_Id := New_Elmt_List;
      DT_Aggr : constant Elist_Id := New_Elmt_List;
      --  Entities marked with attribute Is_Dispatch_Table_Entity

      procedure Check_Premature_Freezing (Subp : Entity_Id; Typ : Entity_Id);
      --  Verify that all non-tagged types in the profile of a subprogram
      --  are frozen at the point the subprogram is frozen. This enforces
      --  the rule on RM 13.14 (14) as modified by AI05-019. At the point a
      --  subprogram is frozen, enough must be known about it to build the
      --  activation record for it, which requires at least that the size of
      --  all parameters be known. Controlling arguments are by-reference,
      --  and therefore the rule only applies to non-tagged types.
      --  Typical violation of the rule involves an object declaration that
      --  freezes a tagged type, when one of its primitive operations has a
      --  type in its profile whose full view has not been analyzed yet.

      procedure Export_DT (Typ : Entity_Id; DT : Entity_Id; Index : Nat := 0);
      --  Export the dispatch table DT of tagged type Typ. Required to generate
      --  forward references and statically allocate the table. For primary
      --  dispatch tables Index is 0; for secondary dispatch tables the value
      --  of index must match the Suffix_Index value assigned to the table by
      --  Make_Tags when generating its unique external name, and it is used to
      --  retrieve from the Dispatch_Table_Wrappers list associated with Typ
      --  the external name generated by Import_DT.

      procedure Make_Secondary_DT
        (Typ              : Entity_Id;
         Iface            : Entity_Id;
         Suffix_Index     : Int;
         Num_Iface_Prims  : Nat;
         Iface_DT_Ptr     : Entity_Id;
         Predef_Prims_Ptr : Entity_Id;
         Build_Thunks     : Boolean;
         Result           : List_Id);
      --  Ada 2005 (AI-251): Expand the declarations for a Secondary Dispatch
      --  Table of Typ associated with Iface. Each abstract interface of Typ
      --  has two secondary dispatch tables: one containing pointers to thunks
      --  and another containing pointers to the primitives covering the
      --  interface primitives. The former secondary table is generated when
      --  Build_Thunks is True, and provides common support for dispatching
      --  calls through interface types; the latter secondary table is
      --  generated when Build_Thunks is False, and provides support for
      --  Generic Dispatching Constructors that dispatch calls through
      --  interface types. When constructing this latter table the value
      --  of Suffix_Index is -1 to indicate that there is no need to export
      --  such table when building statically allocated dispatch tables; a
      --  positive value of Suffix_Index must match the Suffix_Index value
      --  assigned to this secondary dispatch table by Make_Tags when its
      --  unique external name was generated.

      ------------------------------
      -- Check_Premature_Freezing --
      ------------------------------

      procedure Check_Premature_Freezing (Subp : Entity_Id; Typ : Entity_Id) is
      begin
         if Present (N)
           and then  Is_Private_Type (Typ)
           and then No (Full_View (Typ))
           and then not Is_Generic_Type (Typ)
           and then not Is_Tagged_Type (Typ)
           and then not Is_Frozen (Typ)
         then
            Error_Msg_Sloc := Sloc (Subp);
            Error_Msg_NE
              ("declaration must appear after completion of type &", N, Typ);
            Error_Msg_NE
              ("\which is an untagged type in the profile of"
               & " primitive operation & declared#",
               N, Subp);
         end if;
      end Check_Premature_Freezing;

      ---------------
      -- Export_DT --
      ---------------

      procedure Export_DT (Typ : Entity_Id; DT : Entity_Id; Index : Nat := 0)
      is
         Count : Nat;
         Elmt  : Elmt_Id;

      begin
         Set_Is_Statically_Allocated (DT);
         Set_Is_True_Constant (DT);
         Set_Is_Exported (DT);

         Count := 0;
         Elmt  := First_Elmt (Dispatch_Table_Wrappers (Typ));
         while Count /= Index loop
            Next_Elmt (Elmt);
            Count := Count + 1;
         end loop;

         pragma Assert (Related_Type (Node (Elmt)) = Typ);

         Get_External_Name
           (Entity     => Node (Elmt),
            Has_Suffix => True);

         Set_Interface_Name (DT,
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));

         --  Ensure proper Sprint output of this implicit importation

         Set_Is_Internal (DT);
         Set_Is_Public (DT);
      end Export_DT;

      -----------------------
      -- Make_Secondary_DT --
      -----------------------

      procedure Make_Secondary_DT
        (Typ              : Entity_Id;
         Iface            : Entity_Id;
         Suffix_Index     : Int;
         Num_Iface_Prims  : Nat;
         Iface_DT_Ptr     : Entity_Id;
         Predef_Prims_Ptr : Entity_Id;
         Build_Thunks     : Boolean;
         Result           : List_Id)
      is
         Loc                : constant Source_Ptr := Sloc (Typ);
         Exporting_Table    : constant Boolean :=
                                Building_Static_DT (Typ)
                                  and then Suffix_Index > 0;
         Iface_DT           : constant Entity_Id :=
                                Make_Defining_Identifier (Loc,
                                  Chars => New_Internal_Name ('T'));
         Name_Predef_Prims  : constant Name_Id := New_Internal_Name ('R');
         Predef_Prims       : constant Entity_Id :=
                                Make_Defining_Identifier (Loc,
                                  Chars => Name_Predef_Prims);
         DT_Constr_List     : List_Id;
         DT_Aggr_List       : List_Id;
         Empty_DT           : Boolean := False;
         Nb_Predef_Prims    : Nat := 0;
         Nb_Prim            : Nat;
         New_Node           : Node_Id;
         OSD                : Entity_Id;
         OSD_Aggr_List      : List_Id;
         Pos                : Nat;
         Prim               : Entity_Id;
         Prim_Elmt          : Elmt_Id;
         Prim_Ops_Aggr_List : List_Id;

      begin
         --  Handle cases in which we do not generate statically allocated
         --  dispatch tables.

         if not Building_Static_DT (Typ) then
            Set_Ekind (Predef_Prims, E_Variable);
            Set_Ekind (Iface_DT, E_Variable);

         --  Statically allocated dispatch tables and related entities are
         --  constants.

         else
            Set_Ekind (Predef_Prims, E_Constant);
            Set_Is_Statically_Allocated (Predef_Prims);
            Set_Is_True_Constant (Predef_Prims);

            Set_Ekind (Iface_DT, E_Constant);
            Set_Is_Statically_Allocated (Iface_DT);
            Set_Is_True_Constant (Iface_DT);
         end if;

         --  Calculate the number of slots of the dispatch table. If the number
         --  of primitives of Typ is 0 we reserve a dummy single entry for its
         --  DT because at run-time the pointer to this dummy entry will be
         --  used as the tag.

         if Num_Iface_Prims = 0 then
            Empty_DT := True;
            Nb_Prim  := 1;
         else
            Nb_Prim  := Num_Iface_Prims;
         end if;

         --  Generate:

         --   Predef_Prims : Address_Array (1 .. Default_Prim_Ops_Count) :=
         --                    (predef-prim-op-thunk-1'address,
         --                     predef-prim-op-thunk-2'address,
         --                     ...
         --                     predef-prim-op-thunk-n'address);
         --   for Predef_Prims'Alignment use Address'Alignment

         --  Stage 1: Calculate the number of predefined primitives

         if not Building_Static_DT (Typ) then
            Nb_Predef_Prims := Max_Predef_Prims;
         else
            Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
            while Present (Prim_Elmt) loop
               Prim := Node (Prim_Elmt);

               if Is_Predefined_Dispatching_Operation (Prim)
                 and then not Is_Abstract_Subprogram (Prim)
               then
                  Pos := UI_To_Int (DT_Position (Prim));

                  if Pos > Nb_Predef_Prims then
                     Nb_Predef_Prims := Pos;
                  end if;
               end if;

               Next_Elmt (Prim_Elmt);
            end loop;
         end if;

         --  Stage 2: Create the thunks associated with the predefined
         --  primitives and save their entity to fill the aggregate.

         declare
            Prim_Table : array (Nat range 1 .. Nb_Predef_Prims) of Entity_Id;
            Decl       : Node_Id;
            Thunk_Id   : Entity_Id;
            Thunk_Code : Node_Id;

         begin
            Prim_Ops_Aggr_List := New_List;
            Prim_Table := (others => Empty);

            if Building_Static_DT (Typ) then
               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if Is_Predefined_Dispatching_Operation (Prim)
                    and then not Is_Abstract_Subprogram (Prim)
                    and then not Present (Prim_Table
                                           (UI_To_Int (DT_Position (Prim))))
                  then
                     if not Build_Thunks then
                        Prim_Table (UI_To_Int (DT_Position (Prim))) :=
                          Alias (Prim);

                     else
                        while Present (Alias (Prim)) loop
                           Prim := Alias (Prim);
                        end loop;

                        Expand_Interface_Thunk (Prim, Thunk_Id, Thunk_Code);

                        if Present (Thunk_Id) then
                           Append_To (Result, Thunk_Code);
                           Prim_Table (UI_To_Int (DT_Position (Prim)))
                             := Thunk_Id;
                        end if;
                     end if;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
            end if;

            for J in Prim_Table'Range loop
               if Present (Prim_Table (J)) then
                  New_Node :=
                    Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                      Make_Attribute_Reference (Loc,
                        Prefix => New_Reference_To (Prim_Table (J), Loc),
                        Attribute_Name => Name_Unrestricted_Access));
               else
                  New_Node := Make_Null (Loc);
               end if;

               Append_To (Prim_Ops_Aggr_List, New_Node);
            end loop;

            New_Node :=
              Make_Aggregate (Loc,
                Expressions => Prim_Ops_Aggr_List);

            --  Remember aggregates initializing dispatch tables

            Append_Elmt (New_Node, DT_Aggr);

            Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc,
                    New_Internal_Name ('S')),
                Subtype_Indication =>
                  New_Reference_To (RTE (RE_Address_Array), Loc));

            Append_To (Result, Decl);

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Predef_Prims,
                Constant_Present    => Building_Static_DT (Typ),
                Aliased_Present     => True,
                Object_Definition   => New_Reference_To
                                         (Defining_Identifier (Decl), Loc),
                Expression => New_Node));

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (Predef_Prims, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));
         end;

         --  Generate

         --   OSD : Ada.Tags.Object_Specific_Data (Nb_Prims) :=
         --          (OSD_Table => (1 => <value>,
         --                           ...
         --                         N => <value>));

         --   Iface_DT : Dispatch_Table (Nb_Prims) :=
         --               ([ Signature   => <sig-value> ],
         --                Tag_Kind      => <tag_kind-value>,
         --                Predef_Prims  => Predef_Prims'Address,
         --                Offset_To_Top => 0,
         --                OSD           => OSD'Address,
         --                Prims_Ptr     => (prim-op-1'address,
         --                                  prim-op-2'address,
         --                                  ...
         --                                  prim-op-n'address));
         --   for Iface_DT'Alignment use Address'Alignment;

         --  Stage 3: Initialize the discriminant and the record components

         DT_Constr_List := New_List;
         DT_Aggr_List   := New_List;

         --  Nb_Prim. If the tagged type has no primitives we add a dummy
         --  slot whose address will be the tag of this type.

         if Nb_Prim = 0 then
            New_Node := Make_Integer_Literal (Loc, 1);
         else
            New_Node := Make_Integer_Literal (Loc, Nb_Prim);
         end if;

         Append_To (DT_Constr_List, New_Node);
         Append_To (DT_Aggr_List, New_Copy (New_Node));

         --  Signature

         if RTE_Record_Component_Available (RE_Signature) then
            Append_To (DT_Aggr_List,
              New_Reference_To (RTE (RE_Secondary_DT), Loc));
         end if;

         --  Tag_Kind

         if RTE_Record_Component_Available (RE_Tag_Kind) then
            Append_To (DT_Aggr_List, Tagged_Kind (Typ));
         end if;

         --  Predef_Prims

         Append_To (DT_Aggr_List,
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Predef_Prims, Loc),
             Attribute_Name => Name_Address));

         --  Note: The correct value of Offset_To_Top will be set by the init
         --  subprogram

         Append_To (DT_Aggr_List, Make_Integer_Literal (Loc, 0));

         --  Generate the Object Specific Data table required to dispatch calls
         --  through synchronized interfaces.

         if Empty_DT
           or else Is_Abstract_Type (Typ)
           or else Is_Controlled (Typ)
           or else Restriction_Active (No_Dispatching_Calls)
           or else not Is_Limited_Type (Typ)
           or else not Has_Interfaces (Typ)
           or else not Build_Thunks
           or else not RTE_Record_Component_Available (RE_OSD_Table)
         then
            --  No OSD table required

            Append_To (DT_Aggr_List,
              New_Reference_To (RTE (RE_Null_Address), Loc));

         else
            OSD_Aggr_List := New_List;

            declare
               Prim_Table : array (Nat range 1 .. Nb_Prim) of Entity_Id;
               Prim       : Entity_Id;
               Prim_Alias : Entity_Id;
               Prim_Elmt  : Elmt_Id;
               E          : Entity_Id;
               Count      : Nat := 0;
               Pos        : Nat;

            begin
               Prim_Table := (others => Empty);
               Prim_Alias := Empty;

               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if Present (Interface_Alias (Prim))
                    and then Find_Dispatching_Type
                               (Interface_Alias (Prim)) = Iface
                  then
                     Prim_Alias := Interface_Alias (Prim);

                     E := Prim;
                     while Present (Alias (E)) loop
                        E := Alias (E);
                     end loop;

                     Pos := UI_To_Int (DT_Position (Prim_Alias));

                     if Present (Prim_Table (Pos)) then
                        pragma Assert (Prim_Table (Pos) = E);
                        null;

                     else
                        Prim_Table (Pos) := E;

                        Append_To (OSD_Aggr_List,
                          Make_Component_Association (Loc,
                            Choices => New_List (
                              Make_Integer_Literal (Loc,
                                DT_Position (Prim_Alias))),
                            Expression =>
                              Make_Integer_Literal (Loc,
                                DT_Position (Alias (Prim)))));

                        Count := Count + 1;
                     end if;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
               pragma Assert (Count = Nb_Prim);
            end;

            OSD := Make_Defining_Identifier (Loc, New_Internal_Name ('I'));

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => OSD,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RE_Object_Specific_Data), Loc),
                    Constraint =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => New_List (
                          Make_Integer_Literal (Loc, Nb_Prim)))),
                Expression => Make_Aggregate (Loc,
                  Component_Associations => New_List (
                    Make_Component_Association (Loc,
                      Choices => New_List (
                        New_Occurrence_Of
                          (RTE_Record_Component (RE_OSD_Num_Prims), Loc)),
                      Expression =>
                        Make_Integer_Literal (Loc, Nb_Prim)),

                    Make_Component_Association (Loc,
                      Choices => New_List (
                        New_Occurrence_Of
                          (RTE_Record_Component (RE_OSD_Table), Loc)),
                      Expression => Make_Aggregate (Loc,
                        Component_Associations => OSD_Aggr_List))))));

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (OSD, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));

            --  In secondary dispatch tables the Typeinfo component contains
            --  the address of the Object Specific Data (see a-tags.ads)

            Append_To (DT_Aggr_List,
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (OSD, Loc),
                Attribute_Name => Name_Address));
         end if;

         --  Initialize the table of primitive operations

         Prim_Ops_Aggr_List := New_List;

         if Empty_DT then
            Append_To (Prim_Ops_Aggr_List, Make_Null (Loc));

         elsif Is_Abstract_Type (Typ)
           or else not Building_Static_DT (Typ)
         then
            for J in 1 .. Nb_Prim loop
               Append_To (Prim_Ops_Aggr_List, Make_Null (Loc));
            end loop;

         else
            declare
               Prim_Table : array (Nat range 1 .. Nb_Prim) of Entity_Id;
               Pos        : Nat;
               Thunk_Code : Node_Id;
               Thunk_Id   : Entity_Id;

            begin
               Prim_Table := (others => Empty);

               Prim_Elmt  := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if not Is_Predefined_Dispatching_Operation (Prim)
                    and then Present (Interface_Alias (Prim))
                    and then not Is_Abstract_Subprogram (Alias (Prim))
                    and then not Is_Imported (Alias (Prim))
                    and then Find_Dispatching_Type
                               (Interface_Alias (Prim)) = Iface

                     --  Generate the code of the thunk only if the abstract
                     --  interface type is not an immediate ancestor of
                     --  Tagged_Type; otherwise the DT associated with the
                     --  interface is the primary DT.

                    and then not Is_Ancestor (Iface, Typ)
                  then
                     if not Build_Thunks then
                        Pos :=
                          UI_To_Int (DT_Position (Interface_Alias (Prim)));
                        Prim_Table (Pos) := Alias (Prim);
                     else
                        Expand_Interface_Thunk (Prim, Thunk_Id, Thunk_Code);

                        if Present (Thunk_Id) then
                           Pos :=
                             UI_To_Int (DT_Position (Interface_Alias (Prim)));

                           Prim_Table (Pos) := Thunk_Id;
                           Append_To (Result, Thunk_Code);
                        end if;
                     end if;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;

               for J in Prim_Table'Range loop
                  if Present (Prim_Table (J)) then
                     New_Node :=
                       Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (Prim_Table (J), Loc),
                           Attribute_Name => Name_Unrestricted_Access));
                  else
                     New_Node := Make_Null (Loc);
                  end if;

                  Append_To (Prim_Ops_Aggr_List, New_Node);
               end loop;
            end;
         end if;

         New_Node :=
           Make_Aggregate (Loc,
             Expressions => Prim_Ops_Aggr_List);

         Append_To (DT_Aggr_List, New_Node);

         --  Remember aggregates initializing dispatch tables

         Append_Elmt (New_Node, DT_Aggr);

         --  Note: Secondary dispatch tables cannot be declared constant
         --  because the component Offset_To_Top is currently initialized
         --  by the IP routine.

         Append_To (Result,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Iface_DT,
             Aliased_Present     => True,
             Constant_Present    => False,

             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => New_Reference_To
                                   (RTE (RE_Dispatch_Table_Wrapper), Loc),
                 Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                                   Constraints => DT_Constr_List)),

             Expression          =>
               Make_Aggregate (Loc,
                 Expressions => DT_Aggr_List)));

         Append_To (Result,
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Reference_To (Iface_DT, Loc),
             Chars      => Name_Alignment,

             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Reference_To (RTE (RE_Integer_Address), Loc),
                 Attribute_Name => Name_Alignment)));

         if Exporting_Table then
            Export_DT (Typ, Iface_DT, Suffix_Index);

         --  Generate code to create the pointer to the dispatch table

         --    Iface_DT_Ptr : Tag := Tag!(DT.Prims_Ptr'Address);

         --  Note: This declaration is not added here if the table is exported
         --  because in such case Make_Tags has already added this declaration.

         else
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Iface_DT_Ptr,
                Constant_Present    => True,

                Object_Definition   =>
                  New_Reference_To (RTE (RE_Interface_Tag), Loc),

                Expression          =>
                  Unchecked_Convert_To (RTE (RE_Interface_Tag),
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        Make_Selected_Component (Loc,
                          Prefix        => New_Reference_To (Iface_DT, Loc),
                          Selector_Name =>
                            New_Occurrence_Of
                              (RTE_Record_Component (RE_Prims_Ptr), Loc)),
                      Attribute_Name => Name_Address))));
         end if;

         Append_To (Result,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Predef_Prims_Ptr,
             Constant_Present    => True,

             Object_Definition   =>
               New_Reference_To (RTE (RE_Address), Loc),

             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   Make_Selected_Component (Loc,
                     Prefix        => New_Reference_To (Iface_DT, Loc),
                     Selector_Name =>
                       New_Occurrence_Of
                         (RTE_Record_Component (RE_Predef_Prims), Loc)),
                 Attribute_Name => Name_Address)));

         --  Remember entities containing dispatch tables

         Append_Elmt (Predef_Prims, DT_Decl);
         Append_Elmt (Iface_DT, DT_Decl);
      end Make_Secondary_DT;

      --  Local variables

      Elab_Code          : constant List_Id := New_List;
      Result             : constant List_Id := New_List;
      Tname              : constant Name_Id := Chars (Typ);
      AI                 : Elmt_Id;
      AI_Tag_Elmt        : Elmt_Id;
      AI_Tag_Comp        : Elmt_Id;
      DT_Aggr_List       : List_Id;
      DT_Constr_List     : List_Id;
      DT_Ptr             : Entity_Id;
      ITable             : Node_Id;
      I_Depth            : Nat := 0;
      Iface_Table_Node   : Node_Id;
      Name_ITable        : Name_Id;
      Nb_Predef_Prims    : Nat := 0;
      Nb_Prim            : Nat := 0;
      New_Node           : Node_Id;
      Num_Ifaces         : Nat := 0;
      Parent_Typ         : Entity_Id;
      Prim               : Entity_Id;
      Prim_Elmt          : Elmt_Id;
      Prim_Ops_Aggr_List : List_Id;
      Suffix_Index       : Int;
      Typ_Comps          : Elist_Id;
      Typ_Ifaces         : Elist_Id;
      TSD_Aggr_List      : List_Id;
      TSD_Tags_List      : List_Id;

      --  The following name entries are used by Make_DT to generate a number
      --  of entities related to a tagged type. These entities may be generated
      --  in a scope other than that of the tagged type declaration, and if
      --  the entities for two tagged types with the same name happen to be
      --  generated in the same scope, we have to take care to use different
      --  names. This is achieved by means of a unique serial number appended
      --  to each generated entity name.

      Name_DT           : constant Name_Id :=
                            New_External_Name (Tname, 'T', Suffix_Index => -1);
      Name_Exname       : constant Name_Id :=
                            New_External_Name (Tname, 'E', Suffix_Index => -1);
      Name_HT_Link      : constant Name_Id :=
                            New_External_Name (Tname, 'H', Suffix_Index => -1);
      Name_Predef_Prims : constant Name_Id :=
                            New_External_Name (Tname, 'R', Suffix_Index => -1);
      Name_SSD          : constant Name_Id :=
                            New_External_Name (Tname, 'S', Suffix_Index => -1);
      Name_TSD          : constant Name_Id :=
                            New_External_Name (Tname, 'B', Suffix_Index => -1);

      --  Entities built with above names

      DT           : constant Entity_Id :=
                       Make_Defining_Identifier (Loc, Name_DT);
      Exname       : constant Entity_Id :=
                       Make_Defining_Identifier (Loc, Name_Exname);
      HT_Link      : constant Entity_Id :=
                       Make_Defining_Identifier (Loc, Name_HT_Link);
      Predef_Prims : constant Entity_Id :=
                       Make_Defining_Identifier (Loc, Name_Predef_Prims);
      SSD          : constant Entity_Id :=
                       Make_Defining_Identifier (Loc, Name_SSD);
      TSD          : constant Entity_Id :=
                       Make_Defining_Identifier (Loc, Name_TSD);

   --  Start of processing for Make_DT

   begin
      pragma Assert (Is_Frozen (Typ));

      --  Handle cases in which there is no need to build the dispatch table

      if Has_Dispatch_Table (Typ)
        or else No (Access_Disp_Table (Typ))
        or else Is_CPP_Class (Typ)
      then
         return Result;

      elsif No_Run_Time_Mode then
         Error_Msg_CRT ("tagged types", Typ);
         return Result;

      elsif not RTE_Available (RE_Tag) then
         Append_To (Result,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Node (First_Elmt
                                           (Access_Disp_Table (Typ))),
             Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc),
             Constant_Present    => True,
             Expression =>
               Unchecked_Convert_To (RTE (RE_Tag),
                 New_Reference_To (RTE (RE_Null_Address), Loc))));

         Analyze_List (Result, Suppress => All_Checks);
         Error_Msg_CRT ("tagged types", Typ);
         return Result;
      end if;

      --  Ensure that the value of Max_Predef_Prims defined in a-tags is
      --  correct. Valid values are 10 under configurable runtime or 16
      --  with full runtime.

      if RTE_Available (RE_Interface_Data) then
         if Max_Predef_Prims /= 16 then
            Error_Msg_N ("run-time library configuration error", Typ);
            return Result;
         end if;
      else
         if Max_Predef_Prims /= 10 then
            Error_Msg_N ("run-time library configuration error", Typ);
            Error_Msg_CRT ("tagged types", Typ);
            return Result;
         end if;
      end if;

      --  Initialize Parent_Typ handling private types

      Parent_Typ := Etype (Typ);

      if Present (Full_View (Parent_Typ)) then
         Parent_Typ := Full_View (Parent_Typ);
      end if;

      --  Ensure that all the primitives are frozen. This is only required when
      --  building static dispatch tables --- the primitives must be frozen to
      --  be referenced (otherwise we have problems with the backend). It is
      --  not a requirement with nonstatic dispatch tables because in this case
      --  we generate now an empty dispatch table; the extra code required to
      --  register the primitives in the slots will be generated later --- when
      --  each primitive is frozen (see Freeze_Subprogram).

      if Building_Static_DT (Typ)
        and then not Is_CPP_Class (Typ)
      then
         declare
            Save      : constant Boolean := Freezing_Library_Level_Tagged_Type;
            Prim      : Entity_Id;
            Prim_Elmt : Elmt_Id;
            Frnodes   : List_Id;

         begin
            Freezing_Library_Level_Tagged_Type := True;

            Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
            while Present (Prim_Elmt) loop
               Prim    := Node (Prim_Elmt);
               Frnodes := Freeze_Entity (Prim, Loc);

               declare
                  F : Entity_Id;

               begin
                  F := First_Formal (Prim);
                  while Present (F) loop
                     Check_Premature_Freezing (Prim, Etype (F));
                     Next_Formal (F);
                  end loop;

                  Check_Premature_Freezing (Prim, Etype (Prim));
               end;

               if Present (Frnodes) then
                  Append_List_To (Result, Frnodes);
               end if;

               Next_Elmt (Prim_Elmt);
            end loop;

            Freezing_Library_Level_Tagged_Type := Save;
         end;
      end if;

      --  Ada 2005 (AI-251): Build the secondary dispatch tables

      if Has_Interfaces (Typ) then
         Collect_Interface_Components (Typ, Typ_Comps);

         --  Each secondary dispatch table is assigned an unique positive
         --  suffix index; such value also corresponds with the location of
         --  its entity in the Dispatch_Table_Wrappers list (see Make_Tags).

         --  Note: This value must be kept sync with the Suffix_Index values
         --  generated by Make_Tags

         Suffix_Index := 1;
         AI_Tag_Elmt  :=
           Next_Elmt (Next_Elmt (First_Elmt (Access_Disp_Table (Typ))));

         AI_Tag_Comp := First_Elmt (Typ_Comps);
         while Present (AI_Tag_Comp) loop

            --  Build the secondary table containing pointers to thunks

            Make_Secondary_DT
             (Typ             => Typ,
              Iface           => Base_Type (Related_Type (Node (AI_Tag_Comp))),
              Suffix_Index    => Suffix_Index,
              Num_Iface_Prims => UI_To_Int
                                   (DT_Entry_Count (Node (AI_Tag_Comp))),
              Iface_DT_Ptr    => Node (AI_Tag_Elmt),
              Predef_Prims_Ptr => Node (Next_Elmt (AI_Tag_Elmt)),
              Build_Thunks    => True,
              Result          => Result);

            --  Skip secondary dispatch table and secondary dispatch table of
            --  predefined primitives

            Next_Elmt (AI_Tag_Elmt);
            Next_Elmt (AI_Tag_Elmt);

            --  Build the secondary table containing pointers to primitives
            --  (used to give support to Generic Dispatching Constructors).

            Make_Secondary_DT
             (Typ             => Typ,
              Iface           => Base_Type (Related_Type (Node (AI_Tag_Comp))),
              Suffix_Index    => -1,
              Num_Iface_Prims =>  UI_To_Int
                                   (DT_Entry_Count (Node (AI_Tag_Comp))),
              Iface_DT_Ptr    => Node (AI_Tag_Elmt),
              Predef_Prims_Ptr => Node (Next_Elmt (AI_Tag_Elmt)),
              Build_Thunks    => False,
              Result          => Result);

            --  Skip secondary dispatch table and secondary dispatch table of
            --  predefined primitives

            Next_Elmt (AI_Tag_Elmt);
            Next_Elmt (AI_Tag_Elmt);

            Suffix_Index := Suffix_Index + 1;
            Next_Elmt (AI_Tag_Comp);
         end loop;
      end if;

      --  Get the _tag entity and the number of primitives of its dispatch
      --  table.

      DT_Ptr  := Node (First_Elmt (Access_Disp_Table (Typ)));
      Nb_Prim := UI_To_Int (DT_Entry_Count (First_Tag_Component (Typ)));

      Set_Is_Statically_Allocated (DT,  Is_Library_Level_Tagged_Type (Typ));
      Set_Is_Statically_Allocated (SSD, Is_Library_Level_Tagged_Type (Typ));
      Set_Is_Statically_Allocated (TSD, Is_Library_Level_Tagged_Type (Typ));
      Set_Is_Statically_Allocated (Predef_Prims,
        Is_Library_Level_Tagged_Type (Typ));

      --  In case of locally defined tagged type we declare the object
      --  containing the dispatch table by means of a variable. Its
      --  initialization is done later by means of an assignment. This is
      --  required to generate its External_Tag.

      if not Building_Static_DT (Typ) then

         --  Generate:
         --    DT     : No_Dispatch_Table_Wrapper;
         --    for DT'Alignment use Address'Alignment;
         --    DT_Ptr : Tag := !Tag (DT.NDT_Prims_Ptr'Address);

         if not Has_DT (Typ) then
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT,
                Aliased_Present     => True,
                Constant_Present    => False,
                Object_Definition   =>
                  New_Reference_To
                    (RTE (RE_No_Dispatch_Table_Wrapper), Loc)));

            --  Generate a SCIL node for the previous object declaration
            --  because it has a null dispatch table.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Object_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (DT, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT_Ptr,
                Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc),
                Constant_Present    => True,
                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => New_Reference_To (DT, Loc),
                        Selector_Name =>
                          New_Occurrence_Of
                            (RTE_Record_Component (RE_NDT_Prims_Ptr), Loc)),
                      Attribute_Name => Name_Address))));

            --  Generate the SCIL node for the previous object declaration
            --  because it has a tag initialization.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Tag_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

         --  Generate:
         --    DT : Dispatch_Table_Wrapper (Nb_Prim);
         --    for DT'Alignment use Address'Alignment;
         --    DT_Ptr : Tag := !Tag (DT.Prims_Ptr'Address);

         else
            --  If the tagged type has no primitives we add a dummy slot
            --  whose address will be the tag of this type.

            if Nb_Prim = 0 then
               DT_Constr_List :=
                 New_List (Make_Integer_Literal (Loc, 1));
            else
               DT_Constr_List :=
                 New_List (Make_Integer_Literal (Loc, Nb_Prim));
            end if;

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT,
                Aliased_Present     => True,
                Constant_Present    => False,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RE_Dispatch_Table_Wrapper), Loc),
                    Constraint => Make_Index_Or_Discriminant_Constraint (Loc,
                                    Constraints => DT_Constr_List))));

            --  Generate the SCIL node for the previous object declaration
            --  because it contains a dispatch table.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Object_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (DT, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT_Ptr,
                Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc),
                Constant_Present    => True,
                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => New_Reference_To (DT, Loc),
                        Selector_Name =>
                          New_Occurrence_Of
                            (RTE_Record_Component (RE_Prims_Ptr), Loc)),
                      Attribute_Name => Name_Address))));

            --  Generate the SCIL node for the previous object declaration
            --  because it has a tag initialization.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Tag_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Node (Next_Elmt (First_Elmt (Access_Disp_Table (Typ)))),
                Constant_Present    => True,
                Object_Definition   => New_Reference_To
                                            (RTE (RE_Address), Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Reference_To (DT, Loc),
                      Selector_Name =>
                        New_Occurrence_Of
                          (RTE_Record_Component (RE_Predef_Prims), Loc)),
                    Attribute_Name => Name_Address)));
         end if;
      end if;

      --  Generate: Exname : constant String := full_qualified_name (typ);
      --  The type itself may be an anonymous parent type, so use the first
      --  subtype to have a user-recognizable name.

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Exname,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Standard_String, Loc),
          Expression =>
            Make_String_Literal (Loc,
              Full_Qualified_Name (First_Subtype (Typ)))));

      Set_Is_Statically_Allocated (Exname);
      Set_Is_True_Constant (Exname);

      --  Declare the object used by Ada.Tags.Register_Tag

      if RTE_Available (RE_Register_Tag) then
         Append_To (Result,
           Make_Object_Declaration (Loc,
             Defining_Identifier => HT_Link,
             Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc)));
      end if;

      --  Generate code to create the storage for the type specific data object
      --  with enough space to store the tags of the ancestors plus the tags
      --  of all the implemented interfaces (as described in a-tags.adb).

      --   TSD : Type_Specific_Data (I_Depth) :=
      --           (Idepth             => I_Depth,
      --            Access_Level       => Type_Access_Level (Typ),
      --            Expanded_Name      => Cstring_Ptr!(Exname'Address))
      --            External_Tag       => Cstring_Ptr!(Exname'Address))
      --            HT_Link            => HT_Link'Address,
      --            Transportable      => <<boolean-value>>,
      --            RC_Offset          => <<integer-value>>,
      --            [ Size_Func         => Size_Prim'Access ]
      --            [ Interfaces_Table  => <<access-value>> ]
      --            [ SSD               => SSD_Table'Address ]
      --            Tags_Table         => (0 => null,
      --                                   1 => Parent'Tag
      --                                   ...);
      --   for TSD'Alignment use Address'Alignment

      TSD_Aggr_List := New_List;

      --  Idepth: Count ancestors to compute the inheritance depth. For private
      --  extensions, always go to the full view in order to compute the real
      --  inheritance depth.

      declare
         Current_Typ : Entity_Id;
         Parent_Typ  : Entity_Id;

      begin
         I_Depth     := 0;
         Current_Typ := Typ;
         loop
            Parent_Typ := Etype (Current_Typ);

            if Is_Private_Type (Parent_Typ) then
               Parent_Typ := Full_View (Base_Type (Parent_Typ));
            end if;

            exit when Parent_Typ = Current_Typ;

            I_Depth := I_Depth + 1;
            Current_Typ := Parent_Typ;
         end loop;
      end;

      Append_To (TSD_Aggr_List,
        Make_Integer_Literal (Loc, I_Depth));

      --  Access_Level

      Append_To (TSD_Aggr_List,
        Make_Integer_Literal (Loc, Type_Access_Level (Typ)));

      --  Expanded_Name

      Append_To (TSD_Aggr_List,
        Unchecked_Convert_To (RTE (RE_Cstring_Ptr),
          Make_Attribute_Reference (Loc,
            Prefix => New_Reference_To (Exname, Loc),
            Attribute_Name => Name_Address)));

      --  External_Tag of a local tagged type

      --     <typ>A : constant String :=
      --                "Internal tag at 16#tag-addr#: <full-name-of-typ>";

      --  The reason we generate this strange name is that we do not want to
      --  enter local tagged types in the global hash table used to compute
      --  the Internal_Tag attribute for two reasons:

      --    1. It is hard to avoid a tasking race condition for entering the
      --    entry into the hash table.

      --    2. It would cause a storage leak, unless we rig up considerable
      --    mechanism to remove the entry from the hash table on exit.

      --  So what we do is to generate the above external tag name, where the
      --  hex address is the address of the local dispatch table (i.e. exactly
      --  the value we want if Internal_Tag is computed from this string).

      --  Of course this value will only be valid if the tagged type is still
      --  in scope, but it clearly must be erroneous to compute the internal
      --  tag of a tagged type that is out of scope!

      --  We don't do this processing if an explicit external tag has been
      --  specified. That's an odd case for which we have already issued a
      --  warning, where we will not be able to compute the internal tag.

      if not Is_Library_Level_Entity (Typ)
        and then not Has_External_Tag_Rep_Clause (Typ)
      then
         declare
            Exname      : constant Entity_Id :=
                            Make_Defining_Identifier (Loc,
                              New_External_Name (Tname, 'A'));

            Full_Name   : constant String_Id :=
                            Full_Qualified_Name (First_Subtype (Typ));
            Str1_Id     : String_Id;
            Str2_Id     : String_Id;

         begin
            --  Generate:
            --    Str1 = "Internal tag at 16#";

            Start_String;
            Store_String_Chars ("Internal tag at 16#");
            Str1_Id := End_String;

            --  Generate:
            --    Str2 = "#: <type-full-name>";

            Start_String;
            Store_String_Chars ("#: ");
            Store_String_Chars (Full_Name);
            Str2_Id := End_String;

            --  Generate:
            --    Exname : constant String :=
            --               Str1 & Address_Image (Tag) & Str2;

            if RTE_Available (RE_Address_Image) then
               Append_To (Result,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Exname,
                   Constant_Present    => True,
                   Object_Definition   => New_Reference_To
                                            (Standard_String, Loc),
                   Expression =>
                     Make_Op_Concat (Loc,
                       Left_Opnd =>
                         Make_String_Literal (Loc, Str1_Id),
                       Right_Opnd =>
                         Make_Op_Concat (Loc,
                           Left_Opnd =>
                             Make_Function_Call (Loc,
                               Name =>
                                 New_Reference_To
                                   (RTE (RE_Address_Image), Loc),
                               Parameter_Associations => New_List (
                                 Unchecked_Convert_To (RTE (RE_Address),
                                   New_Reference_To (DT_Ptr, Loc)))),
                           Right_Opnd =>
                             Make_String_Literal (Loc, Str2_Id)))));

            else
               Append_To (Result,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Exname,
                   Constant_Present    => True,
                   Object_Definition   => New_Reference_To
                                            (Standard_String, Loc),
                   Expression =>
                     Make_Op_Concat (Loc,
                       Left_Opnd =>
                         Make_String_Literal (Loc, Str1_Id),
                       Right_Opnd =>
                         Make_String_Literal (Loc, Str2_Id))));
            end if;

            New_Node :=
              Unchecked_Convert_To (RTE (RE_Cstring_Ptr),
                Make_Attribute_Reference (Loc,
                  Prefix => New_Reference_To (Exname, Loc),
                  Attribute_Name => Name_Address));
         end;

      --  External tag of a library-level tagged type: Check for a definition
      --  of External_Tag. The clause is considered only if it applies to this
      --  specific tagged type, as opposed to one of its ancestors.
      --  If the type is an unconstrained type extension, we are building the
      --  dispatch table of its anonymous base type, so the external tag, if
      --  any was specified, must be retrieved from the first subtype. Go to
      --  the full view in case the clause is in the private part.

      else
         declare
            Def : constant Node_Id := Get_Attribute_Definition_Clause
                                        (Underlying_Type (First_Subtype (Typ)),
                                         Attribute_External_Tag);

            Old_Val : String_Id;
            New_Val : String_Id;
            E       : Entity_Id;

         begin
            if not Present (Def)
              or else Entity (Name (Def)) /= First_Subtype (Typ)
            then
               New_Node :=
                 Unchecked_Convert_To (RTE (RE_Cstring_Ptr),
                   Make_Attribute_Reference (Loc,
                     Prefix         => New_Reference_To (Exname, Loc),
                     Attribute_Name => Name_Address));
            else
               Old_Val := Strval (Expr_Value_S (Expression (Def)));

               --  For the rep clause "for <typ>'external_tag use y" generate:

               --     <typ>A : constant string := y;
               --
               --  <typ>A'Address is used to set the External_Tag component
               --  of the TSD

               --  Create a new nul terminated string if it is not already

               if String_Length (Old_Val) > 0
                 and then
                  Get_String_Char (Old_Val, String_Length (Old_Val)) = 0
               then
                  New_Val := Old_Val;
               else
                  Start_String (Old_Val);
                  Store_String_Char (Get_Char_Code (ASCII.NUL));
                  New_Val := End_String;
               end if;

               E := Make_Defining_Identifier (Loc,
                      New_External_Name (Chars (Typ), 'A'));

               Append_To (Result,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => E,
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Reference_To (Standard_String, Loc),
                   Expression          =>
                     Make_String_Literal (Loc, New_Val)));

               New_Node :=
                 Unchecked_Convert_To (RTE (RE_Cstring_Ptr),
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (E, Loc),
                     Attribute_Name => Name_Address));
            end if;
         end;
      end if;

      Append_To (TSD_Aggr_List, New_Node);

      --  HT_Link

      if RTE_Available (RE_Register_Tag) then
         Append_To (TSD_Aggr_List,
           Unchecked_Convert_To (RTE (RE_Tag_Ptr),
             Make_Attribute_Reference (Loc,
               Prefix => New_Reference_To (HT_Link, Loc),
               Attribute_Name => Name_Address)));
      else
         Append_To (TSD_Aggr_List,
           Unchecked_Convert_To (RTE (RE_Tag_Ptr),
             New_Reference_To (RTE (RE_Null_Address), Loc)));
      end if;

      --  Transportable: Set for types that can be used in remote calls
      --  with respect to E.4(18) legality rules.

      declare
         Transportable : Entity_Id;

      begin
         Transportable :=
           Boolean_Literals
             (Is_Pure (Typ)
                or else Is_Shared_Passive (Typ)
                or else
                  ((Is_Remote_Types (Typ)
                      or else Is_Remote_Call_Interface (Typ))
                   and then Original_View_In_Visible_Part (Typ))
                or else not Comes_From_Source (Typ));

         Append_To (TSD_Aggr_List,
            New_Occurrence_Of (Transportable, Loc));
      end;

      --  RC_Offset: These are the valid values and their meaning:

      --   >0: For simple types with controlled components is
      --         type._record_controller'position

      --    0: For types with no controlled components

      --   -1: For complex types with controlled components where the position
      --       of the record controller is not statically computable but there
      --       are controlled components at this level. The _Controller field
      --       is available right after the _parent.

      --   -2: There are no controlled components at this level. We need to
      --       get the position from the parent.

      declare
         RC_Offset_Node : Node_Id;

      begin
         if not Has_Controlled_Component (Typ) then
            RC_Offset_Node := Make_Integer_Literal (Loc, 0);

         elsif Etype (Typ) /= Typ
           and then Has_Discriminants (Parent_Typ)
         then
            if Has_New_Controlled_Component (Typ) then
               RC_Offset_Node := Make_Integer_Literal (Loc, -1);
            else
               RC_Offset_Node := Make_Integer_Literal (Loc, -2);
            end if;
         else
            RC_Offset_Node :=
              Make_Attribute_Reference (Loc,
                Prefix =>
                  Make_Selected_Component (Loc,
                    Prefix => New_Reference_To (Typ, Loc),
                    Selector_Name =>
                      New_Reference_To (Controller_Component (Typ), Loc)),
                Attribute_Name => Name_Position);

            --  This is not proper Ada code to use the attribute 'Position
            --  on something else than an object but this is supported by
            --  the back end (see comment on the Bit_Component attribute in
            --  sem_attr). So we avoid semantic checking here.

            --  Is this documented in sinfo.ads??? it should be!

            Set_Analyzed (RC_Offset_Node);
            Set_Etype (Prefix (RC_Offset_Node), RTE (RE_Record_Controller));
            Set_Etype (Prefix (Prefix (RC_Offset_Node)), Typ);
            Set_Etype (Selector_Name (Prefix (RC_Offset_Node)),
              RTE (RE_Record_Controller));
            Set_Etype (RC_Offset_Node, RTE (RE_Storage_Offset));
         end if;

         Append_To (TSD_Aggr_List, RC_Offset_Node);
      end;

      --  Size_Func

      if RTE_Record_Component_Available (RE_Size_Func) then
         if not Building_Static_DT (Typ)
           or else Is_Interface (Typ)
         then
            Append_To (TSD_Aggr_List,
              Unchecked_Convert_To (RTE (RE_Size_Ptr),
                New_Reference_To (RTE (RE_Null_Address), Loc)));

         else
            declare
               Prim_Elmt : Elmt_Id;
               Prim      : Entity_Id;

            begin
               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if Chars (Prim) = Name_uSize then
                     while Present (Alias (Prim)) loop
                        Prim := Alias (Prim);
                     end loop;

                     if Is_Abstract_Subprogram (Prim) then
                        Append_To (TSD_Aggr_List,
                          Unchecked_Convert_To (RTE (RE_Size_Ptr),
                            New_Reference_To (RTE (RE_Null_Address), Loc)));
                     else
                        Append_To (TSD_Aggr_List,
                          Unchecked_Convert_To (RTE (RE_Size_Ptr),
                            Make_Attribute_Reference (Loc,
                              Prefix => New_Reference_To (Prim, Loc),
                              Attribute_Name => Name_Unrestricted_Access)));
                     end if;

                     exit;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
            end;
         end if;
      end if;

      --  Interfaces_Table (required for AI-405)

      if RTE_Record_Component_Available (RE_Interfaces_Table) then

         --  Count the number of interface types implemented by Typ

         Collect_Interfaces (Typ, Typ_Ifaces);

         AI := First_Elmt (Typ_Ifaces);
         while Present (AI) loop
            Num_Ifaces := Num_Ifaces + 1;
            Next_Elmt (AI);
         end loop;

         if Num_Ifaces = 0 then
            Iface_Table_Node := Make_Null (Loc);

         --  Generate the Interface_Table object

         else
            declare
               TSD_Ifaces_List : constant List_Id := New_List;
               Elmt       : Elmt_Id;
               Sec_DT_Tag : Node_Id;

            begin
               AI := First_Elmt (Typ_Ifaces);
               while Present (AI) loop
                  if Is_Ancestor (Node (AI), Typ) then
                     Sec_DT_Tag :=
                       New_Reference_To (DT_Ptr, Loc);
                  else
                     Elmt :=
                       Next_Elmt
                        (Next_Elmt (First_Elmt (Access_Disp_Table (Typ))));
                     pragma Assert (Has_Thunks (Node (Elmt)));

                     while Ekind (Node (Elmt)) = E_Constant
                        and then not
                          Is_Ancestor (Node (AI), Related_Type (Node (Elmt)))
                     loop
                        pragma Assert (Has_Thunks (Node (Elmt)));
                        Next_Elmt (Elmt);
                        pragma Assert (Has_Thunks (Node (Elmt)));
                        Next_Elmt (Elmt);
                        pragma Assert (not Has_Thunks (Node (Elmt)));
                        Next_Elmt (Elmt);
                        pragma Assert (not Has_Thunks (Node (Elmt)));
                        Next_Elmt (Elmt);
                     end loop;

                     pragma Assert (Ekind (Node (Elmt)) = E_Constant
                       and then not
                         Has_Thunks (Node (Next_Elmt (Next_Elmt (Elmt)))));
                     Sec_DT_Tag :=
                       New_Reference_To (Node (Next_Elmt (Next_Elmt (Elmt))),
                                         Loc);
                  end if;

                  Append_To (TSD_Ifaces_List,
                     Make_Aggregate (Loc,
                       Expressions => New_List (

                        --  Iface_Tag

                        Unchecked_Convert_To (RTE (RE_Tag),
                          New_Reference_To
                            (Node (First_Elmt (Access_Disp_Table (Node (AI)))),
                             Loc)),

                        --  Static_Offset_To_Top

                        New_Reference_To (Standard_True, Loc),

                        --  Offset_To_Top_Value

                        Make_Integer_Literal (Loc, 0),

                        --  Offset_To_Top_Func

                        Make_Null (Loc),

                        --  Secondary_DT

                        Unchecked_Convert_To (RTE (RE_Tag), Sec_DT_Tag)

                        )));

                  Next_Elmt (AI);
               end loop;

               Name_ITable := New_External_Name (Tname, 'I');
               ITable      := Make_Defining_Identifier (Loc, Name_ITable);
               Set_Is_Statically_Allocated (ITable,
                 Is_Library_Level_Tagged_Type (Typ));

               --  The table of interfaces is not constant; its slots are
               --  filled at run-time by the IP routine using attribute
               --  'Position to know the location of the tag components
               --  (and this attribute cannot be safely used before the
               --  object is initialized).

               Append_To (Result,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => ITable,
                   Aliased_Present     => True,
                   Constant_Present    => False,
                   Object_Definition   =>
                     Make_Subtype_Indication (Loc,
                       Subtype_Mark =>
                         New_Reference_To (RTE (RE_Interface_Data), Loc),
                       Constraint => Make_Index_Or_Discriminant_Constraint
                         (Loc,
                          Constraints => New_List (
                            Make_Integer_Literal (Loc, Num_Ifaces)))),

                   Expression => Make_Aggregate (Loc,
                     Expressions => New_List (
                       Make_Integer_Literal (Loc, Num_Ifaces),
                       Make_Aggregate (Loc,
                         Expressions => TSD_Ifaces_List)))));

               Append_To (Result,
                 Make_Attribute_Definition_Clause (Loc,
                   Name       => New_Reference_To (ITable, Loc),
                   Chars      => Name_Alignment,
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix =>
                         New_Reference_To (RTE (RE_Integer_Address), Loc),
                       Attribute_Name => Name_Alignment)));

               Iface_Table_Node :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Reference_To (ITable, Loc),
                   Attribute_Name => Name_Unchecked_Access);
            end;
         end if;

         Append_To (TSD_Aggr_List, Iface_Table_Node);
      end if;

      --  Generate the Select Specific Data table for synchronized types that
      --  implement synchronized interfaces. The size of the table is
      --  constrained by the number of non-predefined primitive operations.

      if RTE_Record_Component_Available (RE_SSD) then
         if Ada_Version >= Ada_05
           and then Has_DT (Typ)
           and then Is_Concurrent_Record_Type (Typ)
           and then Has_Interfaces (Typ)
           and then Nb_Prim > 0
           and then not Is_Abstract_Type (Typ)
           and then not Is_Controlled (Typ)
           and then not Restriction_Active (No_Dispatching_Calls)
           and then not Restriction_Active (No_Select_Statements)
         then
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => SSD,
                Aliased_Present     => True,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Reference_To (
                      RTE (RE_Select_Specific_Data), Loc),
                    Constraint   =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => New_List (
                          Make_Integer_Literal (Loc, Nb_Prim))))));

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (SSD, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));

            --  This table is initialized by Make_Select_Specific_Data_Table,
            --  which calls Set_Entry_Index and Set_Prim_Op_Kind.

            Append_To (TSD_Aggr_List,
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (SSD, Loc),
                Attribute_Name => Name_Unchecked_Access));
         else
            Append_To (TSD_Aggr_List, Make_Null (Loc));
         end if;
      end if;

      --  Initialize the table of ancestor tags. In case of interface types
      --  this table is not needed.

      TSD_Tags_List := New_List;

      --  If we are not statically allocating the dispatch table then we must
      --  fill position 0 with null because we still have not generated the
      --  tag of Typ.

      if not Building_Static_DT (Typ)
        or else Is_Interface (Typ)
      then
         Append_To (TSD_Tags_List,
           Unchecked_Convert_To (RTE (RE_Tag),
             New_Reference_To (RTE (RE_Null_Address), Loc)));

      --  Otherwise we can safely reference the tag

      else
         Append_To (TSD_Tags_List,
           New_Reference_To (DT_Ptr, Loc));
      end if;

      --  Fill the rest of the table with the tags of the ancestors

      declare
         Current_Typ : Entity_Id;
         Parent_Typ  : Entity_Id;
         Pos         : Nat;

      begin
         Pos := 1;
         Current_Typ := Typ;

         loop
            Parent_Typ := Etype (Current_Typ);

            if Is_Private_Type (Parent_Typ) then
               Parent_Typ := Full_View (Base_Type (Parent_Typ));
            end if;

            exit when Parent_Typ = Current_Typ;

            if Is_CPP_Class (Parent_Typ) then

               --  The tags defined in the C++ side will be inherited when
               --  the object is constructed (Exp_Ch3.Build_Init_Procedure)

               Append_To (TSD_Tags_List,
                 Unchecked_Convert_To (RTE (RE_Tag),
                   New_Reference_To (RTE (RE_Null_Address), Loc)));
            else
               Append_To (TSD_Tags_List,
                 New_Reference_To
                   (Node (First_Elmt (Access_Disp_Table (Parent_Typ))),
                    Loc));
            end if;

            Pos := Pos + 1;
            Current_Typ := Parent_Typ;
         end loop;

         pragma Assert (Pos = I_Depth + 1);
      end;

      Append_To (TSD_Aggr_List,
        Make_Aggregate (Loc,
          Expressions => TSD_Tags_List));

      --  Build the TSD object

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => TSD,
          Aliased_Present     => True,
          Constant_Present    => Building_Static_DT (Typ),
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (
                RTE (RE_Type_Specific_Data), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => New_List (
                    Make_Integer_Literal (Loc, I_Depth)))),

          Expression => Make_Aggregate (Loc,
            Expressions => TSD_Aggr_List)));

      Set_Is_True_Constant (TSD, Building_Static_DT (Typ));

      Append_To (Result,
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Reference_To (TSD, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Initialize or declare the dispatch table object

      if not Has_DT (Typ) then
         DT_Constr_List := New_List;
         DT_Aggr_List   := New_List;

         --  Typeinfo

         New_Node :=
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (TSD, Loc),
             Attribute_Name => Name_Address);

         Append_To (DT_Constr_List, New_Node);
         Append_To (DT_Aggr_List,   New_Copy (New_Node));
         Append_To (DT_Aggr_List,   Make_Integer_Literal (Loc, 0));

         --  In case of locally defined tagged types we have already declared
         --  and uninitialized object for the dispatch table, which is now
         --  initialized by means of the following assignment:

         --    DT := (TSD'Address, 0);

         if not Building_Static_DT (Typ) then
            Append_To (Result,
              Make_Assignment_Statement (Loc,
                Name => New_Reference_To (DT, Loc),
                Expression => Make_Aggregate (Loc,
                  Expressions => DT_Aggr_List)));

         --  In case of library level tagged types we declare and export now
         --  the constant object containing the dummy dispatch table. There
         --  is no need to declare the tag here because it has been previously
         --  declared by Make_Tags

         --   DT : aliased constant No_Dispatch_Table :=
         --          (NDT_TSD       => TSD'Address;
         --           NDT_Prims_Ptr => 0);
         --   for DT'Alignment use Address'Alignment;

         else
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT,
                Aliased_Present     => True,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Reference_To (RTE (RE_No_Dispatch_Table_Wrapper), Loc),
                Expression => Make_Aggregate (Loc,
                  Expressions => DT_Aggr_List)));

            --  Generate the SCIL node for the previous object declaration
            --  because it has a null dispatch table.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Object_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (DT, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));

            Export_DT (Typ, DT);
         end if;

      --  Common case: Typ has a dispatch table

      --  Generate:

      --   Predef_Prims : Address_Array (1 .. Default_Prim_Ops_Count) :=
      --                    (predef-prim-op-1'address,
      --                     predef-prim-op-2'address,
      --                     ...
      --                     predef-prim-op-n'address);
      --   for Predef_Prims'Alignment use Address'Alignment

      --   DT : Dispatch_Table (Nb_Prims) :=
      --          (Signature => <sig-value>,
      --           Tag_Kind  => <tag_kind-value>,
      --           Predef_Prims => Predef_Prims'First'Address,
      --           Offset_To_Top => 0,
      --           TSD           => TSD'Address;
      --           Prims_Ptr     => (prim-op-1'address,
      --                             prim-op-2'address,
      --                             ...
      --                             prim-op-n'address));
      --   for DT'Alignment use Address'Alignment

      else
         declare
            Pos : Nat;

         begin
            if not Building_Static_DT (Typ) then
               Nb_Predef_Prims := Max_Predef_Prims;

            else
               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if Is_Predefined_Dispatching_Operation (Prim)
                    and then not Is_Abstract_Subprogram (Prim)
                  then
                     Pos := UI_To_Int (DT_Position (Prim));

                     if Pos > Nb_Predef_Prims then
                        Nb_Predef_Prims := Pos;
                     end if;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
            end if;

            declare
               Prim_Table : array
                              (Nat range 1 .. Nb_Predef_Prims) of Entity_Id;
               Decl       : Node_Id;
               E          : Entity_Id;

            begin
               Prim_Ops_Aggr_List := New_List;

               Prim_Table := (others => Empty);

               if Building_Static_DT (Typ) then
                  Prim_Elmt  := First_Elmt (Primitive_Operations (Typ));
                  while Present (Prim_Elmt) loop
                     Prim := Node (Prim_Elmt);

                     if Is_Predefined_Dispatching_Operation (Prim)
                       and then not Is_Abstract_Subprogram (Prim)
                       and then not Present (Prim_Table
                                              (UI_To_Int (DT_Position (Prim))))
                     then
                        E := Prim;
                        while Present (Alias (E)) loop
                           E := Alias (E);
                        end loop;

                        pragma Assert (not Is_Abstract_Subprogram (E));
                        Prim_Table (UI_To_Int (DT_Position (Prim))) := E;
                     end if;

                     Next_Elmt (Prim_Elmt);
                  end loop;
               end if;

               for J in Prim_Table'Range loop
                  if Present (Prim_Table (J)) then
                     New_Node :=
                       Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (Prim_Table (J), Loc),
                           Attribute_Name => Name_Unrestricted_Access));
                  else
                     New_Node := Make_Null (Loc);
                  end if;

                  Append_To (Prim_Ops_Aggr_List, New_Node);
               end loop;

               New_Node :=
                 Make_Aggregate (Loc,
                   Expressions => Prim_Ops_Aggr_List);

               Decl :=
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc,
                       New_Internal_Name ('S')),
                   Subtype_Indication =>
                     New_Reference_To (RTE (RE_Address_Array), Loc));

               Append_To (Result, Decl);

               Append_To (Result,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Predef_Prims,
                   Aliased_Present     => True,
                   Constant_Present    => Building_Static_DT (Typ),
                   Object_Definition   => New_Reference_To
                                           (Defining_Identifier (Decl), Loc),
                   Expression => New_Node));

               --  Remember aggregates initializing dispatch tables

               Append_Elmt (New_Node, DT_Aggr);

               Append_To (Result,
                 Make_Attribute_Definition_Clause (Loc,
                   Name       => New_Reference_To (Predef_Prims, Loc),
                   Chars      => Name_Alignment,
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix =>
                         New_Reference_To (RTE (RE_Integer_Address), Loc),
                       Attribute_Name => Name_Alignment)));
            end;
         end;

         --  Stage 1: Initialize the discriminant and the record components

         DT_Constr_List := New_List;
         DT_Aggr_List   := New_List;

         --  Num_Prims. If the tagged type has no primitives we add a dummy
         --  slot whose address will be the tag of this type.

         if Nb_Prim = 0 then
            New_Node := Make_Integer_Literal (Loc, 1);
         else
            New_Node := Make_Integer_Literal (Loc, Nb_Prim);
         end if;

         Append_To (DT_Constr_List, New_Node);
         Append_To (DT_Aggr_List,   New_Copy (New_Node));

         --  Signature

         if RTE_Record_Component_Available (RE_Signature) then
            Append_To (DT_Aggr_List,
              New_Reference_To (RTE (RE_Primary_DT), Loc));
         end if;

         --  Tag_Kind

         if RTE_Record_Component_Available (RE_Tag_Kind) then
            Append_To (DT_Aggr_List, Tagged_Kind (Typ));
         end if;

         --  Predef_Prims

         Append_To (DT_Aggr_List,
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Predef_Prims, Loc),
             Attribute_Name => Name_Address));

         --  Offset_To_Top

         Append_To (DT_Aggr_List, Make_Integer_Literal (Loc, 0));

         --  Typeinfo

         Append_To (DT_Aggr_List,
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (TSD, Loc),
             Attribute_Name => Name_Address));

         --  Stage 2: Initialize the table of primitive operations

         Prim_Ops_Aggr_List := New_List;

         if Nb_Prim = 0 then
            Append_To (Prim_Ops_Aggr_List, Make_Null (Loc));

         elsif not Building_Static_DT (Typ) then
            for J in 1 .. Nb_Prim loop
               Append_To (Prim_Ops_Aggr_List, Make_Null (Loc));
            end loop;

         else
            declare
               Prim_Table : array (Nat range 1 .. Nb_Prim) of Entity_Id;
               E          : Entity_Id;
               Prim       : Entity_Id;
               Prim_Elmt  : Elmt_Id;

            begin
               Prim_Table := (others => Empty);

               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  --  Retrieve the ultimate alias of the primitive for proper
                  --  handling of renamings and eliminated primitives.

                  E := Ultimate_Alias (Prim);

                  if Is_Imported (Prim)
                    or else Present (Interface_Alias (Prim))
                    or else Is_Predefined_Dispatching_Operation (Prim)
                    or else Is_Eliminated (E)
                  then
                     null;

                  else
                     if not Is_Predefined_Dispatching_Operation (E)
                       and then not Is_Abstract_Subprogram (E)
                       and then not Present (Interface_Alias (E))
                     then
                        pragma Assert
                          (UI_To_Int (DT_Position (Prim)) <= Nb_Prim);

                        Prim_Table (UI_To_Int (DT_Position (Prim))) := E;
                     end if;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;

               for J in Prim_Table'Range loop
                  if Present (Prim_Table (J)) then
                     New_Node :=
                       Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (Prim_Table (J), Loc),
                           Attribute_Name => Name_Unrestricted_Access));
                  else
                     New_Node := Make_Null (Loc);
                  end if;

                  Append_To (Prim_Ops_Aggr_List, New_Node);
               end loop;
            end;
         end if;

         New_Node :=
           Make_Aggregate (Loc,
             Expressions => Prim_Ops_Aggr_List);

         Append_To (DT_Aggr_List, New_Node);

         --  Remember aggregates initializing dispatch tables

         Append_Elmt (New_Node, DT_Aggr);

         --  In case of locally defined tagged types we have already declared
         --  and uninitialized object for the dispatch table, which is now
         --  initialized by means of an assignment.

         if not Building_Static_DT (Typ) then
            Append_To (Result,
              Make_Assignment_Statement (Loc,
                Name => New_Reference_To (DT, Loc),
                Expression => Make_Aggregate (Loc,
                  Expressions => DT_Aggr_List)));

         --  In case of library level tagged types we declare now and export
         --  the constant object containing the dispatch table.

         else
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT,
                Aliased_Present     => True,
                Constant_Present    => True,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Reference_To
                                      (RTE (RE_Dispatch_Table_Wrapper), Loc),
                    Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                                      Constraints => DT_Constr_List)),
                Expression => Make_Aggregate (Loc,
                  Expressions => DT_Aggr_List)));

            --  Generate the SCIL node for the previous object declaration
            --  because it contains a dispatch table.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Object_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

            Append_To (Result,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Reference_To (DT, Loc),
                Chars      => Name_Alignment,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (RTE (RE_Integer_Address), Loc),
                    Attribute_Name => Name_Alignment)));

            Export_DT (Typ, DT);
         end if;
      end if;

      --  Initialize the table of ancestor tags if not building static
      --  dispatch table

      if not Building_Static_DT (Typ)
        and then not Is_Interface (Typ)
        and then not Is_CPP_Class (Typ)
      then
         Append_To (Result,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Indexed_Component (Loc,
                 Prefix =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       New_Reference_To (TSD, Loc),
                     Selector_Name =>
                       New_Reference_To
                         (RTE_Record_Component (RE_Tags_Table), Loc)),
                 Expressions =>
                    New_List (Make_Integer_Literal (Loc, 0))),

             Expression =>
               New_Reference_To
                 (Node (First_Elmt (Access_Disp_Table (Typ))), Loc)));
      end if;

      --  Inherit the dispatch tables of the parent. There is no need to
      --  inherit anything from the parent when building static dispatch tables
      --  because the whole dispatch table (including inherited primitives) has
      --  been already built.

      if Building_Static_DT (Typ) then
         null;

      --  If the ancestor is a CPP_Class type we inherit the dispatch tables
      --  in the init proc, and we don't need to fill them in here.

      elsif Is_CPP_Class (Parent_Typ) then
         null;

      --  Otherwise we fill in the dispatch tables here

      else
         if Typ /= Parent_Typ
           and then not Is_Interface (Typ)
           and then not Restriction_Active (No_Dispatching_Calls)
         then
            --  Inherit the dispatch table

            if not Is_Interface (Typ)
              and then not Is_Interface (Parent_Typ)
              and then not Is_CPP_Class (Parent_Typ)
            then
               declare
                  Nb_Prims : constant Int :=
                               UI_To_Int (DT_Entry_Count
                                 (First_Tag_Component (Parent_Typ)));

               begin
                  Append_To (Elab_Code,
                    Build_Inherit_Predefined_Prims (Loc,
                      Old_Tag_Node =>
                        New_Reference_To
                          (Node
                           (Next_Elmt
                            (First_Elmt
                             (Access_Disp_Table (Parent_Typ)))), Loc),
                      New_Tag_Node =>
                        New_Reference_To
                          (Node
                           (Next_Elmt
                            (First_Elmt
                             (Access_Disp_Table (Typ)))), Loc)));

                  if Nb_Prims /= 0 then
                     Append_To (Elab_Code,
                       Build_Inherit_Prims (Loc,
                         Typ          => Typ,
                         Old_Tag_Node =>
                           New_Reference_To
                             (Node
                              (First_Elmt
                               (Access_Disp_Table (Parent_Typ))), Loc),
                         New_Tag_Node => New_Reference_To (DT_Ptr, Loc),
                         Num_Prims    => Nb_Prims));
                  end if;
               end;
            end if;

            --  Inherit the secondary dispatch tables of the ancestor

            if not Is_CPP_Class (Parent_Typ) then
               declare
                  Sec_DT_Ancestor : Elmt_Id :=
                                      Next_Elmt
                                       (Next_Elmt
                                        (First_Elmt
                                          (Access_Disp_Table (Parent_Typ))));
                  Sec_DT_Typ      : Elmt_Id :=
                                      Next_Elmt
                                       (Next_Elmt
                                         (First_Elmt
                                           (Access_Disp_Table (Typ))));

                  procedure Copy_Secondary_DTs (Typ : Entity_Id);
                  --  Local procedure required to climb through the ancestors
                  --  and copy the contents of all their secondary dispatch
                  --  tables.

                  ------------------------
                  -- Copy_Secondary_DTs --
                  ------------------------

                  procedure Copy_Secondary_DTs (Typ : Entity_Id) is
                     E     : Entity_Id;
                     Iface : Elmt_Id;

                  begin
                     --  Climb to the ancestor (if any) handling private types

                     if Present (Full_View (Etype (Typ))) then
                        if Full_View (Etype (Typ)) /= Typ then
                           Copy_Secondary_DTs (Full_View (Etype (Typ)));
                        end if;

                     elsif Etype (Typ) /= Typ then
                        Copy_Secondary_DTs (Etype (Typ));
                     end if;

                     if Present (Interfaces (Typ))
                       and then not Is_Empty_Elmt_List (Interfaces (Typ))
                     then
                        Iface := First_Elmt (Interfaces (Typ));
                        E     := First_Entity (Typ);
                        while Present (E)
                          and then Present (Node (Sec_DT_Ancestor))
                          and then Ekind (Node (Sec_DT_Ancestor)) = E_Constant
                        loop
                           if Is_Tag (E) and then Chars (E) /= Name_uTag then
                              declare
                                 Num_Prims : constant Int :=
                                               UI_To_Int (DT_Entry_Count (E));

                              begin
                                 if not Is_Interface (Etype (Typ)) then

                                    --  Inherit first secondary dispatch table

                                    Append_To (Elab_Code,
                                      Build_Inherit_Predefined_Prims (Loc,
                                        Old_Tag_Node =>
                                          Unchecked_Convert_To (RTE (RE_Tag),
                                            New_Reference_To
                                              (Node
                                                (Next_Elmt (Sec_DT_Ancestor)),
                                               Loc)),
                                        New_Tag_Node =>
                                          Unchecked_Convert_To (RTE (RE_Tag),
                                            New_Reference_To
                                              (Node (Next_Elmt (Sec_DT_Typ)),
                                               Loc))));

                                    if Num_Prims /= 0 then
                                       Append_To (Elab_Code,
                                         Build_Inherit_Prims (Loc,
                                           Typ          => Node (Iface),
                                           Old_Tag_Node =>
                                             Unchecked_Convert_To
                                               (RTE (RE_Tag),
                                                New_Reference_To
                                                  (Node (Sec_DT_Ancestor),
                                                   Loc)),
                                           New_Tag_Node =>
                                             Unchecked_Convert_To
                                              (RTE (RE_Tag),
                                               New_Reference_To
                                                 (Node (Sec_DT_Typ), Loc)),
                                           Num_Prims    => Num_Prims));
                                    end if;
                                 end if;

                                 Next_Elmt (Sec_DT_Ancestor);
                                 Next_Elmt (Sec_DT_Typ);

                                 --  Skip the secondary dispatch table of
                                 --  predefined primitives

                                 Next_Elmt (Sec_DT_Ancestor);
                                 Next_Elmt (Sec_DT_Typ);

                                 if not Is_Interface (Etype (Typ)) then

                                    --  Inherit second secondary dispatch table

                                    Append_To (Elab_Code,
                                      Build_Inherit_Predefined_Prims (Loc,
                                        Old_Tag_Node =>
                                          Unchecked_Convert_To (RTE (RE_Tag),
                                             New_Reference_To
                                               (Node
                                                 (Next_Elmt (Sec_DT_Ancestor)),
                                                Loc)),
                                        New_Tag_Node =>
                                          Unchecked_Convert_To (RTE (RE_Tag),
                                            New_Reference_To
                                              (Node (Next_Elmt (Sec_DT_Typ)),
                                               Loc))));

                                    if Num_Prims /= 0 then
                                       Append_To (Elab_Code,
                                         Build_Inherit_Prims (Loc,
                                           Typ          => Node (Iface),
                                           Old_Tag_Node =>
                                             Unchecked_Convert_To
                                               (RTE (RE_Tag),
                                                New_Reference_To
                                                  (Node (Sec_DT_Ancestor),
                                                   Loc)),
                                           New_Tag_Node =>
                                             Unchecked_Convert_To
                                              (RTE (RE_Tag),
                                               New_Reference_To
                                                 (Node (Sec_DT_Typ), Loc)),
                                           Num_Prims    => Num_Prims));
                                    end if;
                                 end if;
                              end;

                              Next_Elmt (Sec_DT_Ancestor);
                              Next_Elmt (Sec_DT_Typ);

                              --  Skip the secondary dispatch table of
                              --  predefined primitives

                              Next_Elmt (Sec_DT_Ancestor);
                              Next_Elmt (Sec_DT_Typ);

                              Next_Elmt (Iface);
                           end if;

                           Next_Entity (E);
                        end loop;
                     end if;
                  end Copy_Secondary_DTs;

               begin
                  if Present (Node (Sec_DT_Ancestor))
                    and then Ekind (Node (Sec_DT_Ancestor)) = E_Constant
                  then
                     --  Handle private types

                     if Present (Full_View (Typ)) then
                        Copy_Secondary_DTs (Full_View (Typ));
                     else
                        Copy_Secondary_DTs (Typ);
                     end if;
                  end if;
               end;
            end if;
         end if;
      end if;

      --  Generate code to register the Tag in the External_Tag hash table for
      --  the pure Ada type only.

      --        Register_Tag (Dt_Ptr);

      --  Skip this action in the following cases:
      --    1) if Register_Tag is not available.
      --    2) in No_Run_Time mode.
      --    3) if Typ is not defined at the library level (this is required
      --       to avoid adding concurrency control to the hash table used
      --       by the run-time to register the tags).

      if not No_Run_Time_Mode
        and then Is_Library_Level_Entity (Typ)
        and then RTE_Available (RE_Register_Tag)
      then
         Append_To (Elab_Code,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Register_Tag), Loc),
             Parameter_Associations =>
               New_List (New_Reference_To (DT_Ptr, Loc))));
      end if;

      if not Is_Empty_List (Elab_Code) then
         Append_List_To (Result, Elab_Code);
      end if;

      --  Populate the two auxiliary tables used for dispatching asynchronous,
      --  conditional and timed selects for synchronized types that implement
      --  a limited interface. Skip this step in Ravenscar profile or when
      --  general dispatching is forbidden.

      if Ada_Version >= Ada_05
        and then Is_Concurrent_Record_Type (Typ)
        and then Has_Interfaces (Typ)
        and then not Restriction_Active (No_Dispatching_Calls)
        and then not Restriction_Active (No_Select_Statements)
      then
         Append_List_To (Result,
           Make_Select_Specific_Data_Table (Typ));
      end if;

      --  Remember entities containing dispatch tables

      Append_Elmt (Predef_Prims, DT_Decl);
      Append_Elmt (DT, DT_Decl);

      Analyze_List (Result, Suppress => All_Checks);
      Set_Has_Dispatch_Table (Typ);

      --  Mark entities containing dispatch tables. Required by the backend to
      --  handle them properly.

      if not Is_Interface (Typ) then
         declare
            Elmt : Elmt_Id;

         begin
            --  Ensure that entities Prim_Ptr and Predef_Prims_Table_Ptr have
            --  the decoration required by the backend

            Set_Is_Dispatch_Table_Entity (RTE (RE_Prim_Ptr));
            Set_Is_Dispatch_Table_Entity (RTE (RE_Predef_Prims_Table_Ptr));

            --  Object declarations

            Elmt := First_Elmt (DT_Decl);
            while Present (Elmt) loop
               Set_Is_Dispatch_Table_Entity (Node (Elmt));
               pragma Assert (Ekind (Etype (Node (Elmt))) = E_Array_Subtype
                 or else Ekind (Etype (Node (Elmt))) = E_Record_Subtype);
               Set_Is_Dispatch_Table_Entity (Etype (Node (Elmt)));
               Next_Elmt (Elmt);
            end loop;

            --  Aggregates initializing dispatch tables

            Elmt := First_Elmt (DT_Aggr);
            while Present (Elmt) loop
               Set_Is_Dispatch_Table_Entity (Etype (Node (Elmt)));
               Next_Elmt (Elmt);
            end loop;
         end;
      end if;

      return Result;
   end Make_DT;

   -------------------------------------
   -- Make_Select_Specific_Data_Table --
   -------------------------------------

   function Make_Select_Specific_Data_Table
     (Typ : Entity_Id) return List_Id
   is
      Assignments : constant List_Id    := New_List;
      Loc         : constant Source_Ptr := Sloc (Typ);

      Conc_Typ  : Entity_Id;
      Decls     : List_Id;
      DT_Ptr    : Entity_Id;
      Prim      : Entity_Id;
      Prim_Als  : Entity_Id;
      Prim_Elmt : Elmt_Id;
      Prim_Pos  : Uint;
      Nb_Prim   : Nat := 0;

      type Examined_Array is array (Int range <>) of Boolean;

      function Find_Entry_Index (E : Entity_Id) return Uint;
      --  Given an entry, find its index in the visible declarations of the
      --  corresponding concurrent type of Typ.

      ----------------------
      -- Find_Entry_Index --
      ----------------------

      function Find_Entry_Index (E : Entity_Id) return Uint is
         Index     : Uint := Uint_1;
         Subp_Decl : Entity_Id;

      begin
         if Present (Decls)
           and then not Is_Empty_List (Decls)
         then
            Subp_Decl := First (Decls);
            while Present (Subp_Decl) loop
               if Nkind (Subp_Decl) = N_Entry_Declaration then
                  if Defining_Identifier (Subp_Decl) = E then
                     return Index;
                  end if;

                  Index := Index + 1;
               end if;

               Next (Subp_Decl);
            end loop;
         end if;

         return Uint_0;
      end Find_Entry_Index;

   --  Start of processing for Make_Select_Specific_Data_Table

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      DT_Ptr := Node (First_Elmt (Access_Disp_Table (Typ)));

      if Present (Corresponding_Concurrent_Type (Typ)) then
         Conc_Typ := Corresponding_Concurrent_Type (Typ);

         if Present (Full_View (Conc_Typ)) then
            Conc_Typ := Full_View (Conc_Typ);
         end if;

         if Ekind (Conc_Typ) = E_Protected_Type then
            Decls := Visible_Declarations (Protected_Definition (
                       Parent (Conc_Typ)));
         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);
            Decls := Visible_Declarations (Task_Definition (
                       Parent (Conc_Typ)));
         end if;
      end if;

      --  Count the non-predefined primitive operations

      Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
      while Present (Prim_Elmt) loop
         Prim := Node (Prim_Elmt);

         if not (Is_Predefined_Dispatching_Operation (Prim)
                   or else Is_Predefined_Dispatching_Alias (Prim))
         then
            Nb_Prim := Nb_Prim + 1;
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;

      declare
         Examined : Examined_Array (1 .. Nb_Prim) := (others => False);

      begin
         Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);

            --  Look for primitive overriding an abstract interface subprogram

            if Present (Interface_Alias (Prim))
              and then not Examined (UI_To_Int (DT_Position (Alias (Prim))))
            then
               Prim_Pos := DT_Position (Alias (Prim));
               pragma Assert (UI_To_Int (Prim_Pos) <= Nb_Prim);
               Examined (UI_To_Int (Prim_Pos)) := True;

               --  Set the primitive operation kind regardless of subprogram
               --  type. Generate:
               --    Ada.Tags.Set_Prim_Op_Kind (DT_Ptr, <position>, <kind>);

               Append_To (Assignments,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (RTE (RE_Set_Prim_Op_Kind), Loc),
                   Parameter_Associations => New_List (
                     New_Reference_To (DT_Ptr, Loc),
                     Make_Integer_Literal (Loc, Prim_Pos),
                     Prim_Op_Kind (Alias (Prim), Typ))));

               --  Retrieve the root of the alias chain

               Prim_Als := Prim;
               while Present (Alias (Prim_Als)) loop
                  Prim_Als := Alias (Prim_Als);
               end loop;

               --  In the case of an entry wrapper, set the entry index

               if Ekind (Prim) = E_Procedure
                 and then Is_Primitive_Wrapper (Prim_Als)
                 and then Ekind (Wrapped_Entity (Prim_Als)) = E_Entry
               then
                  --  Generate:
                  --    Ada.Tags.Set_Entry_Index
                  --      (DT_Ptr, <position>, <index>);

                  Append_To (Assignments,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (RTE (RE_Set_Entry_Index), Loc),
                      Parameter_Associations => New_List (
                        New_Reference_To (DT_Ptr, Loc),
                        Make_Integer_Literal (Loc, Prim_Pos),
                        Make_Integer_Literal (Loc,
                          Find_Entry_Index (Wrapped_Entity (Prim_Als))))));
               end if;
            end if;

            Next_Elmt (Prim_Elmt);
         end loop;
      end;

      return Assignments;
   end Make_Select_Specific_Data_Table;

   ---------------
   -- Make_Tags --
   ---------------

   function Make_Tags (Typ : Entity_Id) return List_Id is
      Loc    : constant Source_Ptr := Sloc (Typ);
      Result : constant List_Id    := New_List;

      procedure Import_DT
        (Tag_Typ         : Entity_Id;
         DT              : Entity_Id;
         Is_Secondary_DT : Boolean);
      --  Import the dispatch table DT of tagged type Tag_Typ. Required to
      --  generate forward references and statically allocate the table. For
      --  primary dispatch tables that require no dispatch table generate:
      --     DT : static aliased constant Non_Dispatch_Table_Wrapper;
      --     $pragma import (ada, DT);
      --  Otherwise generate:
      --     DT : static aliased constant Dispatch_Table_Wrapper (Nb_Prim);
      --     $pragma import (ada, DT);

      ---------------
      -- Import_DT --
      ---------------

      procedure Import_DT
        (Tag_Typ         : Entity_Id;
         DT              : Entity_Id;
         Is_Secondary_DT : Boolean)
      is
         DT_Constr_List : List_Id;
         Nb_Prim        : Nat;

      begin
         Set_Is_Imported  (DT);
         Set_Ekind        (DT, E_Constant);
         Set_Related_Type (DT, Typ);

         --  The scope must be set now to call Get_External_Name

         Set_Scope (DT, Current_Scope);

         Get_External_Name (DT, True);
         Set_Interface_Name (DT,
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));

         --  Ensure proper Sprint output of this implicit importation

         Set_Is_Internal (DT);

         --  Save this entity to allow Make_DT to generate its exportation

         Append_Elmt (DT, Dispatch_Table_Wrappers (Typ));

         --  No dispatch table required

         if not Is_Secondary_DT
           and then not Has_DT (Tag_Typ)
         then
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT,
                Aliased_Present     => True,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Reference_To (RTE (RE_No_Dispatch_Table_Wrapper), Loc)));

         else
            --  Calculate the number of primitives of the dispatch table and
            --  the size of the Type_Specific_Data record.

            Nb_Prim :=
              UI_To_Int (DT_Entry_Count (First_Tag_Component (Tag_Typ)));

            --  If the tagged type has no primitives we add a dummy slot
            --  whose address will be the tag of this type.

            if Nb_Prim = 0 then
               DT_Constr_List :=
                 New_List (Make_Integer_Literal (Loc, 1));
            else
               DT_Constr_List :=
                 New_List (Make_Integer_Literal (Loc, Nb_Prim));
            end if;

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT,
                Aliased_Present     => True,
                Constant_Present    => True,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RE_Dispatch_Table_Wrapper), Loc),
                    Constraint => Make_Index_Or_Discriminant_Constraint (Loc,
                                    Constraints => DT_Constr_List))));
         end if;
      end Import_DT;

      --  Local variables

      Tname            : constant Name_Id := Chars (Typ);
      AI_Tag_Comp      : Elmt_Id;
      DT               : Node_Id;
      DT_Ptr           : Node_Id;
      Predef_Prims_Ptr : Node_Id;
      Iface_DT         : Node_Id;
      Iface_DT_Ptr     : Node_Id;
      New_Node         : Node_Id;
      Suffix_Index     : Int;
      Typ_Name         : Name_Id;
      Typ_Comps        : Elist_Id;

   --  Start of processing for Make_Tags

   begin
      --  1) Generate the primary and secondary tag entities

      --  Collect the components associated with secondary dispatch tables

      if Has_Interfaces (Typ) then
         Collect_Interface_Components (Typ, Typ_Comps);
      end if;

      --  1) Generate the primary tag entities

      --  Primary dispatch table containing user-defined primitives

      DT_Ptr := Make_Defining_Identifier (Loc,
                  New_External_Name (Tname, 'P'));
      Set_Etype (DT_Ptr, RTE (RE_Tag));

      --  Primary dispatch table containing predefined primitives

      Predef_Prims_Ptr :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Tname, 'Y'));
      Set_Etype (Predef_Prims_Ptr, RTE (RE_Address));

      --  Import the forward declaration of the Dispatch Table wrapper record
      --  (Make_DT will take care of its exportation)

      if Building_Static_DT (Typ) then
         Set_Dispatch_Table_Wrappers (Typ, New_Elmt_List);

         DT :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Tname, 'T'));

         Import_DT (Typ, DT, Is_Secondary_DT => False);

         if Has_DT (Typ) then
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT_Ptr,
                Constant_Present    => True,
                Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc),
                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => New_Reference_To (DT, Loc),
                        Selector_Name =>
                          New_Occurrence_Of
                            (RTE_Record_Component (RE_Prims_Ptr), Loc)),
                      Attribute_Name => Name_Address))));

            --  Generate the SCIL node for the previous object declaration
            --  because it has a tag initialization.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Tag_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;

            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Predef_Prims_Ptr,
                Constant_Present    => True,
                Object_Definition   => New_Reference_To
                                            (RTE (RE_Address), Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Reference_To (DT, Loc),
                      Selector_Name =>
                        New_Occurrence_Of
                          (RTE_Record_Component (RE_Predef_Prims), Loc)),
                    Attribute_Name => Name_Address)));

         --  No dispatch table required

         else
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => DT_Ptr,
                Constant_Present    => True,
                Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc),
                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => New_Reference_To (DT, Loc),
                        Selector_Name =>
                          New_Occurrence_Of
                            (RTE_Record_Component (RE_NDT_Prims_Ptr), Loc)),
                      Attribute_Name => Name_Address))));

            --  Generate the SCIL node for the previous object declaration
            --  because it has a tag initialization.

            if Generate_SCIL then
               New_Node :=
                 Make_SCIL_Dispatch_Table_Object_Init (Sloc (Last (Result)));
               Set_SCIL_Related_Node (New_Node, Last (Result));
               Set_SCIL_Entity (New_Node, Typ);
               Insert_Before (Last (Result), New_Node);
            end if;
         end if;

         Set_Is_True_Constant (DT_Ptr);
         Set_Is_Statically_Allocated (DT_Ptr);
      end if;

      pragma Assert (No (Access_Disp_Table (Typ)));
      Set_Access_Disp_Table (Typ, New_Elmt_List);
      Append_Elmt (DT_Ptr, Access_Disp_Table (Typ));
      Append_Elmt (Predef_Prims_Ptr, Access_Disp_Table (Typ));

      --  2) Generate the secondary tag entities

      if Has_Interfaces (Typ) then

         --  Note: The following value of Suffix_Index must be in sync with
         --  the Suffix_Index values of secondary dispatch tables generated
         --  by Make_DT.

         Suffix_Index := 1;

         --  For each interface type we build an unique external name
         --  associated with its corresponding secondary dispatch table.
         --  This external name will be used to declare an object that
         --  references this secondary dispatch table, value that will be
         --  used for the elaboration of Typ's objects and also for the
         --  elaboration of objects of derivations of Typ that do not
         --  override the primitive operation of this interface type.

         AI_Tag_Comp := First_Elmt (Typ_Comps);
         while Present (AI_Tag_Comp) loop
            Get_Secondary_DT_External_Name
              (Typ, Related_Type (Node (AI_Tag_Comp)), Suffix_Index);
            Typ_Name := Name_Find;

            if Building_Static_DT (Typ) then
               Iface_DT :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name
                              (Typ_Name, 'T', Suffix_Index => -1));
               Import_DT
                 (Tag_Typ => Related_Type (Node (AI_Tag_Comp)),
                  DT      => Iface_DT,
                  Is_Secondary_DT => True);
            end if;

            --  Secondary dispatch table referencing thunks to user-defined
            --  primitives covered by this interface.

            Iface_DT_Ptr :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Typ_Name, 'P'));
            Set_Etype (Iface_DT_Ptr, RTE (RE_Interface_Tag));
            Set_Ekind (Iface_DT_Ptr, E_Constant);
            Set_Is_Tag (Iface_DT_Ptr);
            Set_Has_Thunks (Iface_DT_Ptr);
            Set_Is_Statically_Allocated (Iface_DT_Ptr,
              Is_Library_Level_Tagged_Type (Typ));
            Set_Is_True_Constant (Iface_DT_Ptr);
            Set_Related_Type
              (Iface_DT_Ptr, Related_Type (Node (AI_Tag_Comp)));
            Append_Elmt (Iface_DT_Ptr, Access_Disp_Table (Typ));

            if Building_Static_DT (Typ) then
               Append_To (Result,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Iface_DT_Ptr,
                   Constant_Present    => True,
                   Object_Definition   => New_Reference_To
                                            (RTE (RE_Interface_Tag), Loc),
                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Interface_Tag),
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           Make_Selected_Component (Loc,
                             Prefix => New_Reference_To (Iface_DT, Loc),
                           Selector_Name =>
                             New_Occurrence_Of
                               (RTE_Record_Component (RE_Prims_Ptr), Loc)),
                         Attribute_Name => Name_Address))));
            end if;

            --  Secondary dispatch table referencing thunks to predefined
            --  primitives.

            Iface_DT_Ptr :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Typ_Name, 'Y'));
            Set_Etype (Iface_DT_Ptr, RTE (RE_Address));
            Set_Ekind (Iface_DT_Ptr, E_Constant);
            Set_Is_Tag (Iface_DT_Ptr);
            Set_Has_Thunks (Iface_DT_Ptr);
            Set_Is_Statically_Allocated (Iface_DT_Ptr,
              Is_Library_Level_Tagged_Type (Typ));
            Set_Is_True_Constant (Iface_DT_Ptr);
            Set_Related_Type
              (Iface_DT_Ptr, Related_Type (Node (AI_Tag_Comp)));
            Append_Elmt (Iface_DT_Ptr, Access_Disp_Table (Typ));

            --  Secondary dispatch table referencing user-defined primitives
            --  covered by this interface.

            Iface_DT_Ptr :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Typ_Name, 'D'));
            Set_Etype (Iface_DT_Ptr, RTE (RE_Interface_Tag));
            Set_Ekind (Iface_DT_Ptr, E_Constant);
            Set_Is_Tag (Iface_DT_Ptr);
            Set_Is_Statically_Allocated (Iface_DT_Ptr,
              Is_Library_Level_Tagged_Type (Typ));
            Set_Is_True_Constant (Iface_DT_Ptr);
            Set_Related_Type
              (Iface_DT_Ptr, Related_Type (Node (AI_Tag_Comp)));
            Append_Elmt (Iface_DT_Ptr, Access_Disp_Table (Typ));

            --  Secondary dispatch table referencing predefined primitives

            Iface_DT_Ptr :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Typ_Name, 'Z'));
            Set_Etype (Iface_DT_Ptr, RTE (RE_Address));
            Set_Ekind (Iface_DT_Ptr, E_Constant);
            Set_Is_Tag (Iface_DT_Ptr);
            Set_Is_Statically_Allocated (Iface_DT_Ptr,
              Is_Library_Level_Tagged_Type (Typ));
            Set_Is_True_Constant (Iface_DT_Ptr);
            Set_Related_Type
              (Iface_DT_Ptr, Related_Type (Node (AI_Tag_Comp)));
            Append_Elmt (Iface_DT_Ptr, Access_Disp_Table (Typ));

            Next_Elmt (AI_Tag_Comp);
         end loop;
      end if;

      --  3) At the end of Access_Disp_Table, if the type has user-defined
      --     primitives, we add the entity of an access type declaration that
      --     is used by Build_Get_Prim_Op_Address to expand dispatching calls
      --     through the primary dispatch table.

      if UI_To_Int (DT_Entry_Count (First_Tag_Component (Typ))) = 0 then
         Analyze_List (Result);

      --     Generate:
      --       type Typ_DT is array (1 .. Nb_Prims) of Prim_Ptr;
      --       type Typ_DT_Acc is access Typ_DT;

      else
         declare
            Name_DT_Prims     : constant Name_Id :=
                                  New_External_Name (Tname, 'G');
            Name_DT_Prims_Acc : constant Name_Id :=
                                  New_External_Name (Tname, 'H');
            DT_Prims          : constant Entity_Id :=
                                  Make_Defining_Identifier (Loc,
                                    Name_DT_Prims);
            DT_Prims_Acc      : constant Entity_Id :=
                                  Make_Defining_Identifier (Loc,
                                    Name_DT_Prims_Acc);
         begin
            Append_To (Result,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => DT_Prims,
                Type_Definition =>
                  Make_Constrained_Array_Definition (Loc,
                    Discrete_Subtype_Definitions => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound => Make_Integer_Literal (Loc,
                                       DT_Entry_Count
                                         (First_Tag_Component (Typ))))),
                    Component_Definition =>
                      Make_Component_Definition (Loc,
                        Subtype_Indication =>
                          New_Reference_To (RTE (RE_Prim_Ptr), Loc)))));

            Append_To (Result,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => DT_Prims_Acc,
                Type_Definition =>
                   Make_Access_To_Object_Definition (Loc,
                     Subtype_Indication =>
                       New_Occurrence_Of (DT_Prims, Loc))));

            Append_Elmt (DT_Prims_Acc, Access_Disp_Table (Typ));

            --  Analyze the resulting list and suppress the generation of the
            --  Init_Proc associated with the above array declaration because
            --  this type is never used in object declarations. It is only used
            --  to simplify the expansion associated with dispatching calls.

            Analyze_List (Result);
            Set_Suppress_Init_Proc (Base_Type (DT_Prims));

            --  Mark entity of dispatch table. Required by the back end to
            --  handle them properly.

            Set_Is_Dispatch_Table_Entity (DT_Prims);
         end;
      end if;

      Set_Ekind        (DT_Ptr, E_Constant);
      Set_Is_Tag       (DT_Ptr);
      Set_Related_Type (DT_Ptr, Typ);

      return Result;
   end Make_Tags;

   ---------------
   -- New_Value --
   ---------------

   function New_Value (From : Node_Id) return Node_Id is
      Res : constant Node_Id := Duplicate_Subexpr (From);
   begin
      if Is_Access_Type (Etype (From)) then
         return
           Make_Explicit_Dereference (Sloc (From),
             Prefix => Res);
      else
         return Res;
      end if;
   end New_Value;

   -----------------------------------
   -- Original_View_In_Visible_Part --
   -----------------------------------

   function Original_View_In_Visible_Part (Typ : Entity_Id) return Boolean is
      Scop : constant Entity_Id := Scope (Typ);

   begin
      --  The scope must be a package

      if not Is_Package_Or_Generic_Package (Scop) then
         return False;
      end if;

      --  A type with a private declaration has a private view declared in
      --  the visible part.

      if Has_Private_Declaration (Typ) then
         return True;
      end if;

      return List_Containing (Parent (Typ)) =
        Visible_Declarations (Specification (Unit_Declaration_Node (Scop)));
   end Original_View_In_Visible_Part;

   ------------------
   -- Prim_Op_Kind --
   ------------------

   function Prim_Op_Kind
     (Prim : Entity_Id;
      Typ  : Entity_Id) return Node_Id
   is
      Full_Typ : Entity_Id := Typ;
      Loc      : constant Source_Ptr := Sloc (Prim);
      Prim_Op  : Entity_Id;

   begin
      --  Retrieve the original primitive operation

      Prim_Op := Prim;
      while Present (Alias (Prim_Op)) loop
         Prim_Op := Alias (Prim_Op);
      end loop;

      if Ekind (Typ) = E_Record_Type
        and then Present (Corresponding_Concurrent_Type (Typ))
      then
         Full_Typ := Corresponding_Concurrent_Type (Typ);
      end if;

      --  When a private tagged type is completed by a concurrent type,
      --  retrieve the full view.

      if Is_Private_Type (Full_Typ) then
         Full_Typ := Full_View (Full_Typ);
      end if;

      if Ekind (Prim_Op) = E_Function then

         --  Protected function

         if Ekind (Full_Typ) = E_Protected_Type then
            return New_Reference_To (RTE (RE_POK_Protected_Function), Loc);

         --  Task function

         elsif Ekind (Full_Typ) = E_Task_Type then
            return New_Reference_To (RTE (RE_POK_Task_Function), Loc);

         --  Regular function

         else
            return New_Reference_To (RTE (RE_POK_Function), Loc);
         end if;

      else
         pragma Assert (Ekind (Prim_Op) = E_Procedure);

         if Ekind (Full_Typ) = E_Protected_Type then

            --  Protected entry

            if Is_Primitive_Wrapper (Prim_Op)
              and then Ekind (Wrapped_Entity (Prim_Op)) = E_Entry
            then
               return New_Reference_To (RTE (RE_POK_Protected_Entry), Loc);

            --  Protected procedure

            else
               return New_Reference_To (RTE (RE_POK_Protected_Procedure), Loc);
            end if;

         elsif Ekind (Full_Typ) = E_Task_Type then

            --  Task entry

            if Is_Primitive_Wrapper (Prim_Op)
              and then Ekind (Wrapped_Entity (Prim_Op)) = E_Entry
            then
               return New_Reference_To (RTE (RE_POK_Task_Entry), Loc);

            --  Task "procedure". These are the internally Expander-generated
            --  procedures (task body for instance).

            else
               return New_Reference_To (RTE (RE_POK_Task_Procedure), Loc);
            end if;

         --  Regular procedure

         else
            return New_Reference_To (RTE (RE_POK_Procedure), Loc);
         end if;
      end if;
   end Prim_Op_Kind;

   ------------------------
   -- Register_Primitive --
   ------------------------

   function Register_Primitive
     (Loc     : Source_Ptr;
      Prim    : Entity_Id) return List_Id
   is
      DT_Ptr        : Entity_Id;
      Iface_Prim    : Entity_Id;
      Iface_Typ     : Entity_Id;
      Iface_DT_Ptr  : Entity_Id;
      Iface_DT_Elmt : Elmt_Id;
      L             : constant List_Id := New_List;
      Pos           : Uint;
      Tag           : Entity_Id;
      Tag_Typ       : Entity_Id;
      Thunk_Id      : Entity_Id;
      Thunk_Code    : Node_Id;

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      if not RTE_Available (RE_Tag) then
         return L;
      end if;

      if not Present (Interface_Alias (Prim)) then
         Tag_Typ := Scope (DTC_Entity (Prim));
         Pos := DT_Position (Prim);
         Tag := First_Tag_Component (Tag_Typ);

         if Is_Predefined_Dispatching_Operation (Prim)
           or else Is_Predefined_Dispatching_Alias (Prim)
         then
            DT_Ptr :=
              Node (Next_Elmt (First_Elmt (Access_Disp_Table (Tag_Typ))));

            Append_To (L,
              Build_Set_Predefined_Prim_Op_Address (Loc,
                Tag_Node     => New_Reference_To (DT_Ptr, Loc),
                Position     => Pos,
                Address_Node =>
                  Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Prim, Loc),
                      Attribute_Name => Name_Unrestricted_Access))));

            --  Register copy of the pointer to the 'size primitive in the TSD

            if Chars (Prim) = Name_uSize
              and then RTE_Record_Component_Available (RE_Size_Func)
            then
               DT_Ptr := Node (First_Elmt (Access_Disp_Table (Tag_Typ)));
               Append_To (L,
                 Build_Set_Size_Function (Loc,
                   Tag_Node  => New_Reference_To (DT_Ptr, Loc),
                   Size_Func => Prim));
            end if;

         else
            pragma Assert (Pos /= Uint_0 and then Pos <= DT_Entry_Count (Tag));

            DT_Ptr := Node (First_Elmt (Access_Disp_Table (Tag_Typ)));
            Append_To (L,
              Build_Set_Prim_Op_Address (Loc,
                Typ          => Tag_Typ,
                Tag_Node     => New_Reference_To (DT_Ptr, Loc),
                Position     => Pos,
                Address_Node =>
                  Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Prim, Loc),
                      Attribute_Name => Name_Unrestricted_Access))));
         end if;

      --  Ada 2005 (AI-251): Primitive associated with an interface type
      --  Generate the code of the thunk only if the interface type is not an
      --  immediate ancestor of Typ; otherwise the dispatch table associated
      --  with the interface is the primary dispatch table and we have nothing
      --  else to do here.

      else
         Tag_Typ   := Find_Dispatching_Type (Alias (Prim));
         Iface_Typ := Find_Dispatching_Type (Interface_Alias (Prim));

         pragma Assert (Is_Interface (Iface_Typ));

         Expand_Interface_Thunk (Prim, Thunk_Id, Thunk_Code);

         if not Is_Ancestor (Iface_Typ, Tag_Typ)
           and then Present (Thunk_Code)
         then
            --  Generate the code necessary to fill the appropriate entry of
            --  the secondary dispatch table of Prim's controlling type with
            --  Thunk_Id's address.

            Iface_DT_Elmt := Find_Interface_ADT (Tag_Typ, Iface_Typ);
            Iface_DT_Ptr  := Node (Iface_DT_Elmt);
            pragma Assert (Has_Thunks (Iface_DT_Ptr));

            Iface_Prim := Interface_Alias (Prim);
            Pos        := DT_Position (Iface_Prim);
            Tag        := First_Tag_Component (Iface_Typ);

            Prepend_To (L, Thunk_Code);

            if Is_Predefined_Dispatching_Operation (Prim)
              or else Is_Predefined_Dispatching_Alias (Prim)
            then
               Append_To (L,
                 Build_Set_Predefined_Prim_Op_Address (Loc,
                   Tag_Node =>
                     New_Reference_To (Node (Next_Elmt (Iface_DT_Elmt)), Loc),
                   Position => Pos,
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix          => New_Reference_To (Thunk_Id, Loc),
                         Attribute_Name  => Name_Unrestricted_Access))));

               Next_Elmt (Iface_DT_Elmt);
               Next_Elmt (Iface_DT_Elmt);
               Iface_DT_Ptr := Node (Iface_DT_Elmt);
               pragma Assert (not Has_Thunks (Iface_DT_Ptr));

               Append_To (L,
                 Build_Set_Predefined_Prim_Op_Address (Loc,
                   Tag_Node =>
                     New_Reference_To (Node (Next_Elmt (Iface_DT_Elmt)), Loc),
                   Position => Pos,
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Alias (Prim), Loc),
                         Attribute_Name  => Name_Unrestricted_Access))));

            else
               pragma Assert (Pos /= Uint_0
                 and then Pos <= DT_Entry_Count (Tag));

               Append_To (L,
                 Build_Set_Prim_Op_Address (Loc,
                   Typ          => Iface_Typ,
                   Tag_Node     => New_Reference_To (Iface_DT_Ptr, Loc),
                   Position     => Pos,
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Thunk_Id, Loc),
                         Attribute_Name => Name_Unrestricted_Access))));

               Next_Elmt (Iface_DT_Elmt);
               Next_Elmt (Iface_DT_Elmt);
               Iface_DT_Ptr := Node (Iface_DT_Elmt);
               pragma Assert (not Has_Thunks (Iface_DT_Ptr));

               Append_To (L,
                 Build_Set_Prim_Op_Address (Loc,
                   Typ          => Iface_Typ,
                   Tag_Node     => New_Reference_To (Iface_DT_Ptr, Loc),
                   Position     => Pos,
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Alias (Prim), Loc),
                         Attribute_Name => Name_Unrestricted_Access))));

            end if;
         end if;
      end if;

      return L;
   end Register_Primitive;

   -------------------------
   -- Set_All_DT_Position --
   -------------------------

   procedure Set_All_DT_Position (Typ : Entity_Id) is

      procedure Validate_Position (Prim : Entity_Id);
      --  Check that the position assigned to Prim is completely safe
      --  (it has not been assigned to a previously defined primitive
      --   operation of Typ)

      -----------------------
      -- Validate_Position --
      -----------------------

      procedure Validate_Position (Prim : Entity_Id) is
         Op_Elmt : Elmt_Id;
         Op      : Entity_Id;

      begin
         --  Aliased primitives are safe

         if Present (Alias (Prim)) then
            return;
         end if;

         Op_Elmt := First_Elmt (Primitive_Operations (Typ));
         while Present (Op_Elmt) loop
            Op := Node (Op_Elmt);

            --  No need to check against itself

            if Op = Prim then
               null;

            --  Primitive operations covering abstract interfaces are
            --  allocated later

            elsif Present (Interface_Alias (Op)) then
               null;

            --  Predefined dispatching operations are completely safe. They
            --  are allocated at fixed positions in a separate table.

            elsif Is_Predefined_Dispatching_Operation (Op)
               or else Is_Predefined_Dispatching_Alias (Op)
            then
               null;

            --  Aliased subprograms are safe

            elsif Present (Alias (Op)) then
               null;

            elsif DT_Position (Op) = DT_Position (Prim)
               and then not Is_Predefined_Dispatching_Operation (Op)
               and then not Is_Predefined_Dispatching_Operation (Prim)
               and then not Is_Predefined_Dispatching_Alias (Op)
               and then not Is_Predefined_Dispatching_Alias (Prim)
            then

               --  Handle aliased subprograms

               declare
                  Op_1 : Entity_Id;
                  Op_2 : Entity_Id;

               begin
                  Op_1 := Op;
                  loop
                     if Present (Overridden_Operation (Op_1)) then
                        Op_1 := Overridden_Operation (Op_1);
                     elsif Present (Alias (Op_1)) then
                        Op_1 := Alias (Op_1);
                     else
                        exit;
                     end if;
                  end loop;

                  Op_2 := Prim;
                  loop
                     if Present (Overridden_Operation (Op_2)) then
                        Op_2 := Overridden_Operation (Op_2);
                     elsif Present (Alias (Op_2)) then
                        Op_2 := Alias (Op_2);
                     else
                        exit;
                     end if;
                  end loop;

                  if Op_1 /= Op_2 then
                     raise Program_Error;
                  end if;
               end;
            end if;

            Next_Elmt (Op_Elmt);
         end loop;
      end Validate_Position;

      --  Local variables

      Parent_Typ : constant Entity_Id := Etype (Typ);
      First_Prim : constant Elmt_Id := First_Elmt (Primitive_Operations (Typ));
      The_Tag    : constant Entity_Id := First_Tag_Component (Typ);

      Adjusted   : Boolean := False;
      Finalized  : Boolean := False;

      Count_Prim : Nat;
      DT_Length  : Nat;
      Nb_Prim    : Nat;
      Prim       : Entity_Id;
      Prim_Elmt  : Elmt_Id;

   --  Start of processing for Set_All_DT_Position

   begin
      pragma Assert (Present (First_Tag_Component (Typ)));

      --  Set the DT_Position for each primitive operation. Perform some sanity
      --  checks to avoid building inconsistent dispatch tables.

      --  First stage: Set the DTC entity of all the primitive operations. This
      --  is required to properly read the DT_Position attribute in the latter
      --  stages.

      Prim_Elmt  := First_Prim;
      Count_Prim := 0;
      while Present (Prim_Elmt) loop
         Prim := Node (Prim_Elmt);

         --  Predefined primitives have a separate dispatch table

         if not (Is_Predefined_Dispatching_Operation (Prim)
                   or else
                 Is_Predefined_Dispatching_Alias (Prim))
         then
            Count_Prim := Count_Prim + 1;
         end if;

         Set_DTC_Entity_Value (Typ, Prim);

         --  Clear any previous value of the DT_Position attribute. In this
         --  way we ensure that the final position of all the primitives is
         --  established by the following stages of this algorithm.

         Set_DT_Position (Prim, No_Uint);

         Next_Elmt (Prim_Elmt);
      end loop;

      declare
         Fixed_Prim : array (Int range 0 .. Count_Prim) of Boolean :=
                        (others => False);

         E : Entity_Id;

         procedure Handle_Inherited_Private_Subprograms (Typ : Entity_Id);
         --  Called if Typ is declared in a nested package or a public child
         --  package to handle inherited primitives that were inherited by Typ
         --  in  the visible part, but whose declaration was deferred because
         --  the parent operation was private and not visible at that point.

         procedure Set_Fixed_Prim (Pos : Nat);
         --  Sets to true an element of the Fixed_Prim table to indicate
         --  that this entry of the dispatch table of Typ is occupied.

         ------------------------------------------
         -- Handle_Inherited_Private_Subprograms --
         ------------------------------------------

         procedure Handle_Inherited_Private_Subprograms (Typ : Entity_Id) is
            Op_List     : Elist_Id;
            Op_Elmt     : Elmt_Id;
            Op_Elmt_2   : Elmt_Id;
            Prim_Op     : Entity_Id;
            Parent_Subp : Entity_Id;

         begin
            Op_List := Primitive_Operations (Typ);

            Op_Elmt := First_Elmt (Op_List);
            while Present (Op_Elmt) loop
               Prim_Op := Node (Op_Elmt);

               --  Search primitives that are implicit operations with an
               --  internal name whose parent operation has a normal name.

               if Present (Alias (Prim_Op))
                 and then Find_Dispatching_Type (Alias (Prim_Op)) /= Typ
                 and then not Comes_From_Source (Prim_Op)
                 and then Is_Internal_Name (Chars (Prim_Op))
                 and then not Is_Internal_Name (Chars (Alias (Prim_Op)))
               then
                  Parent_Subp := Alias (Prim_Op);

                  --  Check if the type has an explicit overriding for this
                  --  primitive.

                  Op_Elmt_2 := Next_Elmt (Op_Elmt);
                  while Present (Op_Elmt_2) loop
                     if Chars (Node (Op_Elmt_2)) = Chars (Parent_Subp)
                       and then Type_Conformant (Prim_Op, Node (Op_Elmt_2))
                     then
                        Set_DT_Position (Prim_Op, DT_Position (Parent_Subp));
                        Set_DT_Position (Node (Op_Elmt_2),
                          DT_Position (Parent_Subp));
                        Set_Fixed_Prim (UI_To_Int (DT_Position (Prim_Op)));

                        goto Next_Primitive;
                     end if;

                     Next_Elmt (Op_Elmt_2);
                  end loop;
               end if;

               <<Next_Primitive>>
               Next_Elmt (Op_Elmt);
            end loop;
         end Handle_Inherited_Private_Subprograms;

         --------------------
         -- Set_Fixed_Prim --
         --------------------

         procedure Set_Fixed_Prim (Pos : Nat) is
         begin
            pragma Assert (Pos <= Count_Prim);
            Fixed_Prim (Pos) := True;
         exception
            when Constraint_Error =>
               raise Program_Error;
         end Set_Fixed_Prim;

      begin
         --  In case of nested packages and public child package it may be
         --  necessary a special management on inherited subprograms so that
         --  the dispatch table is properly filled.

         if Ekind (Scope (Scope (Typ))) = E_Package
           and then Scope (Scope (Typ)) /= Standard_Standard
           and then ((Is_Derived_Type (Typ) and then not Is_Private_Type (Typ))
                       or else
                        (Nkind (Parent (Typ)) = N_Private_Extension_Declaration
                          and then Is_Generic_Type (Typ)))
           and then In_Open_Scopes (Scope (Etype (Typ)))
           and then Typ = Base_Type (Typ)
         then
            Handle_Inherited_Private_Subprograms (Typ);
         end if;

         --  Second stage: Register fixed entries

         Nb_Prim   := 0;
         Prim_Elmt := First_Prim;
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);

            --  Predefined primitives have a separate table and all its
            --  entries are at predefined fixed positions.

            if Is_Predefined_Dispatching_Operation (Prim) then
               Set_DT_Position (Prim, Default_Prim_Op_Position (Prim));

            elsif Is_Predefined_Dispatching_Alias (Prim) then
               E := Alias (Prim);
               while Present (Alias (E)) loop
                  E := Alias (E);
               end loop;

               Set_DT_Position (Prim, Default_Prim_Op_Position (E));

            --  Overriding primitives of ancestor abstract interfaces

            elsif Present (Interface_Alias (Prim))
              and then Is_Ancestor
                         (Find_Dispatching_Type (Interface_Alias (Prim)), Typ)
            then
               pragma Assert (DT_Position (Prim) = No_Uint
                 and then Present (DTC_Entity (Interface_Alias (Prim))));

               E := Interface_Alias (Prim);
               Set_DT_Position (Prim, DT_Position (E));

               pragma Assert
                 (DT_Position (Alias (Prim)) = No_Uint
                    or else DT_Position (Alias (Prim)) = DT_Position (E));
               Set_DT_Position (Alias (Prim), DT_Position (E));
               Set_Fixed_Prim (UI_To_Int (DT_Position (Prim)));

            --  Overriding primitives must use the same entry as the
            --  overridden primitive.

            elsif not Present (Interface_Alias (Prim))
              and then Present (Alias (Prim))
              and then Chars (Prim) = Chars (Alias (Prim))
              and then Find_Dispatching_Type (Alias (Prim)) /= Typ
              and then Is_Ancestor
                         (Find_Dispatching_Type (Alias (Prim)), Typ)
              and then Present (DTC_Entity (Alias (Prim)))
            then
               E := Alias (Prim);
               Set_DT_Position (Prim, DT_Position (E));

               if not Is_Predefined_Dispatching_Alias (E) then
                  Set_Fixed_Prim (UI_To_Int (DT_Position (E)));
               end if;
            end if;

            Next_Elmt (Prim_Elmt);
         end loop;

         --  Third stage: Fix the position of all the new primitives
         --  Entries associated with primitives covering interfaces
         --  are handled in a latter round.

         Prim_Elmt := First_Prim;
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);

            --  Skip primitives previously set entries

            if DT_Position (Prim) /= No_Uint then
               null;

            --  Primitives covering interface primitives are handled later

            elsif Present (Interface_Alias (Prim)) then
               null;

            else
               --  Take the next available position in the DT

               loop
                  Nb_Prim := Nb_Prim + 1;
                  pragma Assert (Nb_Prim <= Count_Prim);
                  exit when not Fixed_Prim (Nb_Prim);
               end loop;

               Set_DT_Position (Prim, UI_From_Int (Nb_Prim));
               Set_Fixed_Prim (Nb_Prim);
            end if;

            Next_Elmt (Prim_Elmt);
         end loop;
      end;

      --  Fourth stage: Complete the decoration of primitives covering
      --  interfaces (that is, propagate the DT_Position attribute
      --  from the aliased primitive)

      Prim_Elmt := First_Prim;
      while Present (Prim_Elmt) loop
         Prim := Node (Prim_Elmt);

         if DT_Position (Prim) = No_Uint
           and then Present (Interface_Alias (Prim))
         then
            pragma Assert (Present (Alias (Prim))
              and then Find_Dispatching_Type (Alias (Prim)) = Typ);

            --  Check if this entry will be placed in the primary DT

            if Is_Ancestor
                (Find_Dispatching_Type (Interface_Alias (Prim)), Typ)
            then
               pragma Assert (DT_Position (Alias (Prim)) /= No_Uint);
               Set_DT_Position (Prim, DT_Position (Alias (Prim)));

            --  Otherwise it will be placed in the secondary DT

            else
               pragma Assert
                 (DT_Position (Interface_Alias (Prim)) /= No_Uint);
               Set_DT_Position (Prim,
                 DT_Position (Interface_Alias (Prim)));
            end if;
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;

      --  Generate listing showing the contents of the dispatch tables.
      --  This action is done before some further static checks because
      --  in case of critical errors caused by a wrong dispatch table
      --  we need to see the contents of such table.

      if Debug_Flag_ZZ then
         Write_DT (Typ);
      end if;

      --  Final stage: Ensure that the table is correct plus some further
      --  verifications concerning the primitives.

      Prim_Elmt := First_Prim;
      DT_Length := 0;
      while Present (Prim_Elmt) loop
         Prim := Node (Prim_Elmt);

         --  At this point all the primitives MUST have a position
         --  in the dispatch table.

         if DT_Position (Prim) = No_Uint then
            raise Program_Error;
         end if;

         --  Calculate real size of the dispatch table

         if not (Is_Predefined_Dispatching_Operation (Prim)
                   or else Is_Predefined_Dispatching_Alias (Prim))
           and then UI_To_Int (DT_Position (Prim)) > DT_Length
         then
            DT_Length := UI_To_Int (DT_Position (Prim));
         end if;

         --  Ensure that the assigned position to non-predefined
         --  dispatching operations in the dispatch table is correct.

         if not (Is_Predefined_Dispatching_Operation (Prim)
                   or else Is_Predefined_Dispatching_Alias (Prim))
         then
            Validate_Position (Prim);
         end if;

         if Chars (Prim) = Name_Finalize then
            Finalized := True;
         end if;

         if Chars (Prim) = Name_Adjust then
            Adjusted := True;
         end if;

         --  An abstract operation cannot be declared in the private part
         --  for a visible abstract type, because it could never be over-
         --  ridden. For explicit declarations this is checked at the
         --  point of declaration, but for inherited operations it must
         --  be done when building the dispatch table.

         --  Ada 2005 (AI-251): Primitives associated with interfaces are
         --  excluded from this check because interfaces must be visible in
         --  the public and private part (RM 7.3 (7.3/2))

         if Is_Abstract_Type (Typ)
           and then Is_Abstract_Subprogram (Prim)
           and then Present (Alias (Prim))
           and then not Is_Interface
                          (Find_Dispatching_Type (Ultimate_Alias (Prim)))
           and then not Present (Interface_Alias (Prim))
           and then Is_Derived_Type (Typ)
           and then In_Private_Part (Current_Scope)
           and then
             List_Containing (Parent (Prim)) =
               Private_Declarations
                (Specification (Unit_Declaration_Node (Current_Scope)))
           and then Original_View_In_Visible_Part (Typ)
         then
            --  We exclude Input and Output stream operations because
            --  Limited_Controlled inherits useless Input and Output
            --  stream operations from Root_Controlled, which can
            --  never be overridden.

            if not Is_TSS (Prim, TSS_Stream_Input)
                 and then
               not Is_TSS (Prim, TSS_Stream_Output)
            then
               Error_Msg_NE
                 ("abstract inherited private operation&" &
                  " must be overridden (RM 3.9.3(10))",
                 Parent (Typ), Prim);
            end if;
         end if;

         Next_Elmt (Prim_Elmt);
      end loop;

      --  Additional check

      if Is_Controlled (Typ) then
         if not Finalized then
            Error_Msg_N
              ("controlled type has no explicit Finalize method?", Typ);

         elsif not Adjusted then
            Error_Msg_N
              ("controlled type has no explicit Adjust method?", Typ);
         end if;
      end if;

      --  Set the final size of the Dispatch Table

      Set_DT_Entry_Count (The_Tag, UI_From_Int (DT_Length));

      --  The derived type must have at least as many components as its parent
      --  (for root types Etype points to itself and the test cannot fail).

      if DT_Entry_Count (The_Tag) <
           DT_Entry_Count (First_Tag_Component (Parent_Typ))
      then
         raise Program_Error;
      end if;
   end Set_All_DT_Position;

   --------------------------
   -- Set_CPP_Constructors --
   --------------------------

   procedure Set_CPP_Constructors (Typ : Entity_Id) is
      Loc   : Source_Ptr;
      Init  : Entity_Id;
      E     : Entity_Id;
      Found : Boolean := False;
      P     : Node_Id;
      Parms : List_Id;

   begin
      --  Look for the constructor entities

      E := Next_Entity (Typ);
      while Present (E) loop
         if Ekind (E) = E_Function
           and then Is_Constructor (E)
         then
            --  Create the init procedure

            Found := True;
            Loc   := Sloc (E);
            Init  := Make_Defining_Identifier (Loc, Make_Init_Proc_Name (Typ));
            Parms :=
              New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_X),
                  Parameter_Type =>
                    New_Reference_To (Typ, Loc)));

            if Present (Parameter_Specifications (Parent (E))) then
               P := First (Parameter_Specifications (Parent (E)));
               while Present (P) loop
                  Append_To (Parms,
                    Make_Parameter_Specification (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars (Defining_Identifier (P))),
                      Parameter_Type => New_Copy_Tree (Parameter_Type (P))));
                  Next (P);
               end loop;
            end if;

            Discard_Node (
              Make_Subprogram_Declaration (Loc,
                Make_Procedure_Specification (Loc,
                  Defining_Unit_Name => Init,
                  Parameter_Specifications => Parms)));

            Set_Init_Proc (Typ, Init);
            Set_Is_Imported    (Init);
            Set_Interface_Name (Init, Interface_Name (E));
            Set_Convention     (Init, Convention_C);
            Set_Is_Public      (Init);
            Set_Has_Completion (Init);
         end if;

         Next_Entity (E);
      end loop;

      --  If there are no constructors, mark the type as abstract since we
      --  won't be able to declare objects of that type.

      if not Found then
         Set_Is_Abstract_Type (Typ);
      end if;
   end Set_CPP_Constructors;

   --------------------------
   -- Set_DTC_Entity_Value --
   --------------------------

   procedure Set_DTC_Entity_Value
     (Tagged_Type : Entity_Id;
      Prim        : Entity_Id)
   is
   begin
      if Present (Interface_Alias (Prim))
        and then Is_Interface
                   (Find_Dispatching_Type (Interface_Alias (Prim)))
      then
         Set_DTC_Entity (Prim,
           Find_Interface_Tag
             (T     => Tagged_Type,
              Iface => Find_Dispatching_Type (Interface_Alias (Prim))));
      else
         Set_DTC_Entity (Prim,
           First_Tag_Component (Tagged_Type));
      end if;
   end Set_DTC_Entity_Value;

   -----------------
   -- Tagged_Kind --
   -----------------

   function Tagged_Kind (T : Entity_Id) return Node_Id is
      Conc_Typ : Entity_Id;
      Loc      : constant Source_Ptr := Sloc (T);

   begin
      pragma Assert
        (Is_Tagged_Type (T) and then RTE_Available (RE_Tagged_Kind));

      --  Abstract kinds

      if Is_Abstract_Type (T) then
         if Is_Limited_Record (T) then
            return New_Reference_To (RTE (RE_TK_Abstract_Limited_Tagged), Loc);
         else
            return New_Reference_To (RTE (RE_TK_Abstract_Tagged), Loc);
         end if;

      --  Concurrent kinds

      elsif Is_Concurrent_Record_Type (T) then
         Conc_Typ := Corresponding_Concurrent_Type (T);

         if Present (Full_View (Conc_Typ)) then
            Conc_Typ := Full_View (Conc_Typ);
         end if;

         if Ekind (Conc_Typ) = E_Protected_Type then
            return New_Reference_To (RTE (RE_TK_Protected), Loc);
         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);
            return New_Reference_To (RTE (RE_TK_Task), Loc);
         end if;

      --  Regular tagged kinds

      else
         if Is_Limited_Record (T) then
            return New_Reference_To (RTE (RE_TK_Limited_Tagged), Loc);
         else
            return New_Reference_To (RTE (RE_TK_Tagged), Loc);
         end if;
      end if;
   end Tagged_Kind;

   --------------
   -- Write_DT --
   --------------

   procedure Write_DT (Typ : Entity_Id) is
      Elmt : Elmt_Id;
      Prim : Node_Id;

   begin
      --  Protect this procedure against wrong usage. Required because it will
      --  be used directly from GDB

      if not (Typ <= Last_Node_Id)
        or else not Is_Tagged_Type (Typ)
      then
         Write_Str ("wrong usage: Write_DT must be used with tagged types");
         Write_Eol;
         return;
      end if;

      Write_Int (Int (Typ));
      Write_Str (": ");
      Write_Name (Chars (Typ));

      if Is_Interface (Typ) then
         Write_Str (" is interface");
      end if;

      Write_Eol;

      Elmt := First_Elmt (Primitive_Operations (Typ));
      while Present (Elmt) loop
         Prim := Node (Elmt);
         Write_Str  (" - ");

         --  Indicate if this primitive will be allocated in the primary
         --  dispatch table or in a secondary dispatch table associated
         --  with an abstract interface type

         if Present (DTC_Entity (Prim)) then
            if Etype (DTC_Entity (Prim)) = RTE (RE_Tag) then
               Write_Str ("[P] ");
            else
               Write_Str ("[s] ");
            end if;
         end if;

         --  Output the node of this primitive operation and its name

         Write_Int  (Int (Prim));
         Write_Str  (": ");

         if Is_Predefined_Dispatching_Operation (Prim) then
            Write_Str ("(predefined) ");
         end if;

         Write_Name (Chars (Prim));

         --  Indicate if this primitive has an aliased primitive

         if Present (Alias (Prim)) then
            Write_Str (" (alias = ");
            Write_Int (Int (Alias (Prim)));

            --  If the DTC_Entity attribute is already set we can also output
            --  the name of the interface covered by this primitive (if any)

            if Present (DTC_Entity (Alias (Prim)))
              and then Is_Interface (Scope (DTC_Entity (Alias (Prim))))
            then
               Write_Str  (" from interface ");
               Write_Name (Chars (Scope (DTC_Entity (Alias (Prim)))));
            end if;

            if Present (Interface_Alias (Prim)) then
               Write_Str  (", AI_Alias of ");
               Write_Name
                 (Chars (Find_Dispatching_Type (Interface_Alias (Prim))));
               Write_Char (':');
               Write_Int  (Int (Interface_Alias (Prim)));
            end if;

            Write_Str (")");
         end if;

         --  Display the final position of this primitive in its associated
         --  (primary or secondary) dispatch table

         if Present (DTC_Entity (Prim))
           and then DT_Position (Prim) /= No_Uint
         then
            Write_Str (" at #");
            Write_Int (UI_To_Int (DT_Position (Prim)));
         end if;

         if Is_Abstract_Subprogram (Prim) then
            Write_Str (" is abstract;");

         --  Check if this is a null primitive

         elsif Comes_From_Source (Prim)
           and then Ekind (Prim) = E_Procedure
           and then Null_Present (Parent (Prim))
         then
            Write_Str (" is null;");
         end if;

         if Is_Eliminated (Ultimate_Alias (Prim)) then
            Write_Str (" (eliminated)");
         end if;

         Write_Eol;

         Next_Elmt (Elmt);
      end loop;
   end Write_DT;

end Exp_Disp;
