------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem_Disp; use Sem_Disp;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Disp is

   Ada_Actions : constant array (DT_Access_Action) of RE_Id :=
      (CW_Membership           => RE_CW_Membership,
       DT_Entry_Size           => RE_DT_Entry_Size,
       DT_Prologue_Size        => RE_DT_Prologue_Size,
       Get_Expanded_Name       => RE_Get_Expanded_Name,
       Get_External_Tag        => RE_Get_External_Tag,
       Get_Prim_Op_Address     => RE_Get_Prim_Op_Address,
       Get_RC_Offset           => RE_Get_RC_Offset,
       Get_Remotely_Callable   => RE_Get_Remotely_Callable,
       Get_TSD                 => RE_Get_TSD,
       Inherit_DT              => RE_Inherit_DT,
       Inherit_TSD             => RE_Inherit_TSD,
       Register_Tag            => RE_Register_Tag,
       Set_Expanded_Name       => RE_Set_Expanded_Name,
       Set_External_Tag        => RE_Set_External_Tag,
       Set_Prim_Op_Address     => RE_Set_Prim_Op_Address,
       Set_RC_Offset           => RE_Set_RC_Offset,
       Set_Remotely_Callable   => RE_Set_Remotely_Callable,
       Set_TSD                 => RE_Set_TSD,
       TSD_Entry_Size          => RE_TSD_Entry_Size,
       TSD_Prologue_Size       => RE_TSD_Prologue_Size);

   CPP_Actions : constant array (DT_Access_Action) of RE_Id :=
      (CW_Membership           => RE_CPP_CW_Membership,
       DT_Entry_Size           => RE_CPP_DT_Entry_Size,
       DT_Prologue_Size        => RE_CPP_DT_Prologue_Size,
       Get_Expanded_Name       => RE_CPP_Get_Expanded_Name,
       Get_External_Tag        => RE_CPP_Get_External_Tag,
       Get_Prim_Op_Address     => RE_CPP_Get_Prim_Op_Address,
       Get_RC_Offset           => RE_CPP_Get_RC_Offset,
       Get_Remotely_Callable   => RE_CPP_Get_Remotely_Callable,
       Get_TSD                 => RE_CPP_Get_TSD,
       Inherit_DT              => RE_CPP_Inherit_DT,
       Inherit_TSD             => RE_CPP_Inherit_TSD,
       Register_Tag            => RE_CPP_Register_Tag,
       Set_Expanded_Name       => RE_CPP_Set_Expanded_Name,
       Set_External_Tag        => RE_CPP_Set_External_Tag,
       Set_Prim_Op_Address     => RE_CPP_Set_Prim_Op_Address,
       Set_RC_Offset           => RE_CPP_Set_RC_Offset,
       Set_Remotely_Callable   => RE_CPP_Set_Remotely_Callable,
       Set_TSD                 => RE_CPP_Set_TSD,
       TSD_Entry_Size          => RE_CPP_TSD_Entry_Size,
       TSD_Prologue_Size       => RE_CPP_TSD_Prologue_Size);

   Action_Is_Proc : constant array (DT_Access_Action) of Boolean :=
      (CW_Membership           => False,
       DT_Entry_Size           => False,
       DT_Prologue_Size        => False,
       Get_Expanded_Name       => False,
       Get_External_Tag        => False,
       Get_Prim_Op_Address     => False,
       Get_Remotely_Callable   => False,
       Get_RC_Offset           => False,
       Get_TSD                 => False,
       Inherit_DT              => True,
       Inherit_TSD             => True,
       Register_Tag            => True,
       Set_Expanded_Name       => True,
       Set_External_Tag        => True,
       Set_Prim_Op_Address     => True,
       Set_RC_Offset           => True,
       Set_Remotely_Callable   => True,
       Set_TSD                 => True,
       TSD_Entry_Size          => False,
       TSD_Prologue_Size       => False);

   Action_Nb_Arg : constant array (DT_Access_Action) of Int :=
      (CW_Membership           => 2,
       DT_Entry_Size           => 0,
       DT_Prologue_Size        => 0,
       Get_Expanded_Name       => 1,
       Get_External_Tag        => 1,
       Get_Prim_Op_Address     => 2,
       Get_RC_Offset           => 1,
       Get_Remotely_Callable   => 1,
       Get_TSD                 => 1,
       Inherit_DT              => 3,
       Inherit_TSD             => 2,
       Register_Tag            => 1,
       Set_Expanded_Name       => 2,
       Set_External_Tag        => 2,
       Set_Prim_Op_Address     => 3,
       Set_RC_Offset           => 2,
       Set_Remotely_Callable   => 2,
       Set_TSD                 => 2,
       TSD_Entry_Size          => 0,
       TSD_Prologue_Size       => 0);

   function Original_View_In_Visible_Part (Typ : Entity_Id) return Boolean;
   --  Check if the type has a private view or if the public view appears
   --  in the visible part of a package spec.

   --------------------------
   -- Expand_Dispatch_Call --
   --------------------------

   procedure Expand_Dispatch_Call (Call_Node : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (Call_Node);
      Call_Typ : constant Entity_Id  := Etype (Call_Node);

      Ctrl_Arg   : constant Node_Id := Controlling_Argument (Call_Node);
      Param_List : constant List_Id := Parameter_Associations (Call_Node);
      Subp       : Entity_Id        := Entity (Name (Call_Node));

      CW_Typ        : Entity_Id;
      New_Call      : Node_Id;
      New_Call_Name : Node_Id;
      New_Params    : List_Id := No_List;
      Param         : Node_Id;
      Res_Typ       : Entity_Id;
      Subp_Ptr_Typ  : Entity_Id;
      Subp_Typ      : Entity_Id;
      Typ           : Entity_Id;
      Eq_Prim_Op    : Entity_Id := Empty;

      function New_Value (From : Node_Id) return Node_Id;
      --  From is the original Expression. New_Value is equivalent to a call
      --  to Duplicate_Subexpr with an explicit dereference when From is an
      --  access parameter

      ---------------
      -- New_Value --
      ---------------

      function New_Value (From : Node_Id) return Node_Id is
         Res : constant Node_Id := Duplicate_Subexpr (From);

      begin
         if Is_Access_Type (Etype (From)) then
            return Make_Explicit_Dereference (Sloc (From), Res);
         else
            return Res;
         end if;
      end New_Value;

   --  Start of processing for Expand_Dispatch_Call

   begin
      --  If this is an inherited operation that was overriden, the body
      --  that is being called is its alias.

      if Present (Alias (Subp))
        and then Is_Inherited_Operation (Subp)
        and then No (DTC_Entity (Subp))
      then
         Subp := Alias (Subp);
      end if;

      --  Expand_Dispatch is called directly from the semantics, so we need
      --  a check to see whether expansion is active before proceeding

      if not Expander_Active then
         return;
      end if;

      --  Definition of the ClassWide Type and the Tagged type

      if Is_Access_Type (Etype (Ctrl_Arg)) then
         CW_Typ := Designated_Type (Etype (Ctrl_Arg));
      else
         CW_Typ := Etype (Ctrl_Arg);
      end if;

      Typ := Root_Type (CW_Typ);

      if not Is_Limited_Type (Typ) then
         Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
      end if;

      if Is_CPP_Class (Root_Type (Typ)) then

         --  Create a new parameter list with the displaced 'this'

         New_Params := New_List;
         Param := First_Actual (Call_Node);
         while Present (Param) loop

            --  We assume that dispatching through the main dispatch table
            --  (referenced by Tag_Component) doesn't require a displacement
            --  so the expansion below is only done when dispatching on
            --  another vtable pointer, in which case the first argument
            --  is expanded into :

            --     typ!(Displaced_This (Address!(Param)))

            if Param = Ctrl_Arg
              and then DTC_Entity (Subp) /= Tag_Component (Typ)
            then
               Append_To (New_Params,

                 Unchecked_Convert_To (Etype (Param),
                   Make_Function_Call (Loc,
                     Name => New_Reference_To (RTE (RE_Displaced_This), Loc),
                     Parameter_Associations => New_List (

                     --  Current_This

                       Make_Unchecked_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Reference_To (RTE (RE_Address), Loc),
                         Expression   => Relocate_Node (Param)),

                     --  Vptr

                       Make_Selected_Component (Loc,
                          Prefix => Duplicate_Subexpr (Ctrl_Arg),
                          Selector_Name =>
                            New_Reference_To (DTC_Entity (Subp), Loc)),

                     --  Position

                       Make_Integer_Literal (Loc, DT_Position (Subp))))));

            else
               Append_To (New_Params, Relocate_Node (Param));
            end if;

            Next_Actual (Param);
         end loop;

      elsif Present (Param_List) then

         --  Generate the Tag checks when appropriate

         New_Params := New_List;

         Param := First_Actual (Call_Node);
         while Present (Param) loop

            --  No tag check with itself

            if Param = Ctrl_Arg then
               Append_To (New_Params,
                 Duplicate_Subexpr_Move_Checks (Param));

            --  No tag check for parameter whose type is neither tagged nor
            --  access to tagged (for access parameters)

            elsif No (Find_Controlling_Arg (Param)) then
               Append_To (New_Params, Relocate_Node (Param));

            --  No tag check for function dispatching on result it the
            --  Tag given by the context is this one

            elsif Find_Controlling_Arg (Param) = Ctrl_Arg then
               Append_To (New_Params, Relocate_Node (Param));

            --  "=" is the only dispatching operation allowed to get
            --  operands with incompatible tags (it just returns false).
            --  We use Duplicate_Subexpr_Move_Checks instead of calling
            --  Relocate_Node because the value will be duplicated to
            --  check the tags.

            elsif Subp = Eq_Prim_Op then
               Append_To (New_Params,
                 Duplicate_Subexpr_Move_Checks (Param));

            --  No check in presence of suppress flags

            elsif Tag_Checks_Suppressed (Etype (Param))
              or else (Is_Access_Type (Etype (Param))
                         and then Tag_Checks_Suppressed
                                    (Designated_Type (Etype (Param))))
            then
               Append_To (New_Params, Relocate_Node (Param));

            --  Optimization: no tag checks if the parameters are identical

            elsif Is_Entity_Name (Param)
              and then Is_Entity_Name (Ctrl_Arg)
              and then Entity (Param) = Entity (Ctrl_Arg)
            then
               Append_To (New_Params, Relocate_Node (Param));

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
                             New_Reference_To (Tag_Component (Typ), Loc)),

                       Right_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Unchecked_Convert_To (Typ, New_Value (Param)),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc))),

                   Then_Statements =>
                     New_List (New_Constraint_Error (Loc))));

               Append_To (New_Params, Relocate_Node (Param));
            end if;

            Next_Actual (Param);
         end loop;
      end if;

      --  Generate the appropriate subprogram pointer type

      if  Etype (Subp) = Typ then
         Res_Typ := CW_Typ;
      else
         Res_Typ :=  Etype (Subp);
      end if;

      Subp_Typ := Create_Itype (E_Subprogram_Type, Call_Node);
      Subp_Ptr_Typ := Create_Itype (E_Access_Subprogram_Type, Call_Node);
      Set_Etype          (Subp_Typ, Res_Typ);
      Init_Size_Align    (Subp_Ptr_Typ);
      Set_Returns_By_Ref (Subp_Typ, Returns_By_Ref (Subp));

      --  Create a new list of parameters which is a copy of the old formal
      --  list including the creation of a new set of matching entities.

      declare
         Old_Formal : Entity_Id := First_Formal (Subp);
         New_Formal : Entity_Id;
         Extra      : Entity_Id;

      begin
         if Present (Old_Formal) then
            New_Formal := New_Copy (Old_Formal);
            Set_First_Entity (Subp_Typ, New_Formal);
            Param := First_Actual (Call_Node);

            loop
               Set_Scope (New_Formal, Subp_Typ);

               --  Change all the controlling argument types to be class-wide
               --  to avoid a recursion in dispatching

               if Is_Controlling_Actual (Param) then
                  Set_Etype (New_Formal, Etype (Param));
               end if;

               if Is_Itype (Etype (New_Formal)) then
                  Extra := New_Copy (Etype (New_Formal));

                  if Ekind (Extra) = E_Record_Subtype
                    or else Ekind (Extra) = E_Class_Wide_Subtype
                  then
                     Set_Cloned_Subtype (Extra, Etype (New_Formal));
                  end if;

                  Set_Etype (New_Formal, Extra);
                  Set_Scope (Etype (New_Formal), Subp_Typ);
               end if;

               Extra := New_Formal;
               Next_Formal (Old_Formal);
               exit when No (Old_Formal);

               Set_Next_Entity (New_Formal, New_Copy (Old_Formal));
               Next_Entity (New_Formal);
               Next_Actual (Param);
            end loop;
            Set_Last_Entity (Subp_Typ, Extra);

            --  Copy extra formals

            New_Formal := First_Entity (Subp_Typ);
            while Present (New_Formal) loop
               if Present (Extra_Constrained (New_Formal)) then
                  Set_Extra_Formal (Extra,
                    New_Copy (Extra_Constrained (New_Formal)));
                  Extra := Extra_Formal (Extra);
                  Set_Extra_Constrained (New_Formal, Extra);

               elsif Present (Extra_Accessibility (New_Formal)) then
                  Set_Extra_Formal (Extra,
                    New_Copy (Extra_Accessibility (New_Formal)));
                  Extra := Extra_Formal (Extra);
                  Set_Extra_Accessibility (New_Formal, Extra);
               end if;

               Next_Formal (New_Formal);
            end loop;
         end if;
      end;

      Set_Etype (Subp_Ptr_Typ, Subp_Ptr_Typ);
      Set_Directly_Designated_Type (Subp_Ptr_Typ, Subp_Typ);

      --  Generate:
      --   Subp_Ptr_Typ!(Get_Prim_Op_Address (Ctrl._Tag, pos));

      New_Call_Name :=
        Unchecked_Convert_To (Subp_Ptr_Typ,
          Make_DT_Access_Action (Typ,
            Action => Get_Prim_Op_Address,
            Args => New_List (

            --  Vptr

              Make_Selected_Component (Loc,
                Prefix => Duplicate_Subexpr_Move_Checks (Ctrl_Arg),
                Selector_Name => New_Reference_To (DTC_Entity (Subp), Loc)),

            --  Position

              Make_Integer_Literal (Loc, DT_Position (Subp)))));

      if Nkind (Call_Node) = N_Function_Call then
         New_Call :=
           Make_Function_Call (Loc,
             Name => New_Call_Name,
             Parameter_Associations => New_Params);

         --  if this is a dispatching "=", we must first compare the tags so
         --  we generate: x.tag = y.tag and then x = y

         if Subp = Eq_Prim_Op then

            Param := First_Actual (Call_Node);
            New_Call :=
              Make_And_Then (Loc,
                Left_Opnd =>
                     Make_Op_Eq (Loc,
                       Left_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix => New_Value (Param),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc)),

                       Right_Opnd =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Unchecked_Convert_To (Typ,
                               New_Value (Next_Actual (Param))),
                           Selector_Name =>
                             New_Reference_To (Tag_Component (Typ), Loc))),

                Right_Opnd => New_Call);
         end if;

      else
         New_Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Call_Name,
             Parameter_Associations => New_Params);
      end if;

      Rewrite (Call_Node, New_Call);
      Analyze_And_Resolve (Call_Node, Call_Typ);
   end Expand_Dispatch_Call;

   -------------
   -- Fill_DT --
   -------------

   function Fill_DT_Entry
     (Loc  : Source_Ptr;
      Prim : Entity_Id)
      return Node_Id
   is
      Typ    : constant Entity_Id := Scope (DTC_Entity (Prim));
      DT_Ptr : constant Entity_Id := Access_Disp_Table (Typ);

   begin
      return
        Make_DT_Access_Action (Typ,
          Action => Set_Prim_Op_Address,
          Args   => New_List (
            New_Reference_To (DT_Ptr, Loc),                     -- DTptr

            Make_Integer_Literal (Loc, DT_Position (Prim)),     -- Position

            Make_Attribute_Reference (Loc,                      -- Value
              Prefix          => New_Reference_To (Prim, Loc),
              Attribute_Name  => Name_Address)));
   end Fill_DT_Entry;

   ---------------------------
   -- Get_Remotely_Callable --
   ---------------------------

   function Get_Remotely_Callable (Obj : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (Obj);

   begin
      return Make_DT_Access_Action
        (Typ    => Etype (Obj),
         Action => Get_Remotely_Callable,
         Args   => New_List (
           Make_Selected_Component (Loc,
             Prefix        => Obj,
             Selector_Name => Make_Identifier (Loc, Name_uTag))));
   end Get_Remotely_Callable;

   -------------
   -- Make_DT --
   -------------

   function Make_DT (Typ : Entity_Id) return List_Id is
      Loc : constant Source_Ptr := Sloc (Typ);

      Result    : constant List_Id := New_List;
      Elab_Code : constant List_Id := New_List;

      Tname       : constant Name_Id := Chars (Typ);
      Name_DT     : constant Name_Id := New_External_Name (Tname, 'T');
      Name_DT_Ptr : constant Name_Id := New_External_Name (Tname, 'P');
      Name_TSD    : constant Name_Id := New_External_Name (Tname, 'B');
      Name_Exname : constant Name_Id := New_External_Name (Tname, 'E');
      Name_No_Reg : constant Name_Id := New_External_Name (Tname, 'F');

      DT     : constant Node_Id := Make_Defining_Identifier (Loc, Name_DT);
      DT_Ptr : constant Node_Id := Make_Defining_Identifier (Loc, Name_DT_Ptr);
      TSD    : constant Node_Id := Make_Defining_Identifier (Loc, Name_TSD);
      Exname : constant Node_Id := Make_Defining_Identifier (Loc, Name_Exname);
      No_Reg : constant Node_Id := Make_Defining_Identifier (Loc, Name_No_Reg);

      I_Depth         : Int;
      Generalized_Tag : Entity_Id;
      Size_Expr_Node  : Node_Id;
      Old_Tag         : Node_Id;
      Old_TSD         : Node_Id;

   begin
      if not RTE_Available (RE_Tag) then
         Error_Msg_CRT ("tagged types", Typ);
         return New_List;
      end if;

      if Is_CPP_Class (Root_Type (Typ)) then
         Generalized_Tag := RTE (RE_Vtable_Ptr);
      else
         Generalized_Tag := RTE (RE_Tag);
      end if;

      --  Dispatch table and related entities are allocated statically

      Set_Ekind (DT, E_Variable);
      Set_Is_Statically_Allocated (DT);

      Set_Ekind (DT_Ptr, E_Variable);
      Set_Is_Statically_Allocated (DT_Ptr);

      Set_Ekind (TSD, E_Variable);
      Set_Is_Statically_Allocated (TSD);

      Set_Ekind (Exname, E_Variable);
      Set_Is_Statically_Allocated (Exname);

      Set_Ekind (No_Reg, E_Variable);
      Set_Is_Statically_Allocated (No_Reg);

      --  Generate code to create the storage for the Dispatch_Table object:

      --   DT : Storage_Array (1..DT_Prologue_Size+nb_prim*DT_Entry_Size);
      --   for DT'Alignment use Address'Alignment

      Size_Expr_Node :=
        Make_Op_Add (Loc,
          Left_Opnd  => Make_DT_Access_Action (Typ, DT_Prologue_Size, No_List),
          Right_Opnd =>
            Make_Op_Multiply (Loc,
              Left_Opnd  =>
                Make_DT_Access_Action (Typ, DT_Entry_Size, No_List),
              Right_Opnd =>
                Make_Integer_Literal (Loc,
                  DT_Entry_Count (Tag_Component (Typ)))));

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => DT,
          Aliased_Present     => True,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                Constraints => New_List (
                  Make_Range (Loc,
                    Low_Bound  => Make_Integer_Literal (Loc, 1),
                    High_Bound => Size_Expr_Node))))));

      Append_To (Result,
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Reference_To (DT, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Generate code to create the pointer to the dispatch table

      --    DT_Ptr : Tag := Tag!(DT'Address);                 Ada case
      --  or
      --    DT_Ptr : Vtable_Ptr := Vtable_Ptr!(DT'Address);   CPP case

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => DT_Ptr,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Generalized_Tag, Loc),
          Expression          =>
            Unchecked_Convert_To (Generalized_Tag,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Reference_To (DT, Loc),
                Attribute_Name => Name_Address))));

      --  Generate code to define the boolean that controls registration, in
      --  order to avoid multiple registrations for tagged types defined in
      --  multiple-called scopes

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => No_Reg,
          Object_Definition   => New_Reference_To (Standard_Boolean, Loc),
          Expression          => New_Reference_To (Standard_True, Loc)));

      --  Set Access_Disp_Table field to be the dispatch table pointer

      Set_Access_Disp_Table (Typ, DT_Ptr);

      --  Count ancestors to compute the inheritance depth. For private
      --  extensions, always go to the full view in order to compute the real
      --  inheritance depth.

      declare
         Parent_Type : Entity_Id := Typ;
         P           : Entity_Id;

      begin
         I_Depth := 0;

         loop
            P := Etype (Parent_Type);

            if Is_Private_Type (P) then
               P := Full_View (Base_Type (P));
            end if;

            exit when P = Parent_Type;

            I_Depth := I_Depth + 1;
            Parent_Type := P;
         end loop;
      end;

      --  Generate code to create the storage for the type specific data object

      --   TSD: Storage_Array (1..TSD_Prologue_Size+(1+Idepth)*TSD_Entry_Size);
      --   for TSD'Alignment use Address'Alignment

      Size_Expr_Node :=
        Make_Op_Add (Loc,
          Left_Opnd  =>
            Make_DT_Access_Action (Typ, TSD_Prologue_Size, No_List),
          Right_Opnd =>
            Make_Op_Multiply (Loc,
              Left_Opnd  =>
                Make_DT_Access_Action (Typ, TSD_Entry_Size, No_List),
              Right_Opnd =>
                Make_Op_Add (Loc,
                  Left_Opnd  => Make_Integer_Literal (Loc, 1),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, I_Depth))));

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => TSD,
          Aliased_Present     => True,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                Constraints => New_List (
                  Make_Range (Loc,
                    Low_Bound  => Make_Integer_Literal (Loc, 1),
                    High_Bound => Size_Expr_Node))))));

      Append_To (Result,
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Reference_To (TSD, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Generate code to put the Address of the TSD in the dispatch table
      --    Set_TSD (DT_Ptr, TSD);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Set_TSD,
          Args   => New_List (
            New_Reference_To (DT_Ptr, Loc),                  -- DTptr
              Make_Attribute_Reference (Loc,                 -- Value
              Prefix          => New_Reference_To (TSD, Loc),
              Attribute_Name  => Name_Address))));

      if Typ = Etype (Typ)
        or else Is_CPP_Class (Etype (Typ))
      then
         Old_Tag :=
           Unchecked_Convert_To (Generalized_Tag,
             Make_Integer_Literal (Loc, 0));

         Old_TSD :=
           Unchecked_Convert_To (RTE (RE_Address),
             Make_Integer_Literal (Loc, 0));

      else
         Old_Tag := New_Reference_To (Access_Disp_Table (Etype (Typ)), Loc);
         Old_TSD :=
           Make_DT_Access_Action (Typ,
             Action => Get_TSD,
             Args   => New_List (
               New_Reference_To (Access_Disp_Table (Etype (Typ)), Loc)));
      end if;

      --  Generate: Inherit_DT (parent'tag, DT_Ptr, nb_prim of parent);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Inherit_DT,
          Args   => New_List (
            Node1 => Old_Tag,
            Node2 => New_Reference_To (DT_Ptr, Loc),
            Node3 => Make_Integer_Literal (Loc,
                       DT_Entry_Count (Tag_Component (Etype (Typ)))))));

      --  Generate: Inherit_TSD (Get_TSD (parent), DT_Ptr);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Inherit_TSD,
          Args   => New_List (
            Node1 => Old_TSD,
            Node2 => New_Reference_To (DT_Ptr, Loc))));

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

      --  Generate: Set_Expanded_Name (DT_Ptr, exname'Address);

      Append_To (Elab_Code,
        Make_DT_Access_Action (Typ,
          Action => Set_Expanded_Name,
          Args   => New_List (
            Node1 => New_Reference_To (DT_Ptr, Loc),
            Node2 =>
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (Exname, Loc),
                Attribute_Name => Name_Address))));

      --  for types with no controlled components
      --    Generate: Set_RC_Offset (DT_Ptr, 0);
      --  for simple types with controlled components
      --    Generate: Set_RC_Offset (DT_Ptr, type._record_controller'position);
      --  for complex types with controlled components where the position
      --  of the record controller is not statically computable, if there are
      --  controlled components at this level
      --    Generate: Set_RC_Offset (DT_Ptr, -1);
      --  to indicate that the _controller field is right after the _parent or
      --  if there are no controlled components at this level,
      --    Generate: Set_RC_Offset (DT_Ptr, -2);
      --  to indicate that we need to get the position from the parent.

      declare
         Position : Node_Id;

      begin
         if not Has_Controlled_Component (Typ) then
            Position := Make_Integer_Literal (Loc, 0);

         elsif Etype (Typ) /= Typ and then Has_Discriminants (Etype (Typ)) then
            if Has_New_Controlled_Component (Typ) then
               Position := Make_Integer_Literal (Loc, -1);
            else
               Position := Make_Integer_Literal (Loc, -2);
            end if;
         else
            Position :=
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

            Set_Analyzed (Position);
            Set_Etype (Prefix (Position), RTE (RE_Record_Controller));
            Set_Etype (Prefix (Prefix (Position)), Typ);
            Set_Etype (Selector_Name (Prefix (Position)),
              RTE (RE_Record_Controller));
            Set_Etype (Position, RTE (RE_Storage_Offset));
         end if;

         Append_To (Elab_Code,
           Make_DT_Access_Action (Typ,
             Action => Set_RC_Offset,
             Args   => New_List (
               Node1 => New_Reference_To (DT_Ptr, Loc),
               Node2 => Position)));
      end;

      --  Generate: Set_Remotely_Callable (DT_Ptr, status);
      --  where status is described in E.4 (18)

      declare
         Status : Entity_Id;

      begin
         if Is_Pure (Typ)
           or else Is_Shared_Passive (Typ)
           or else
             ((Is_Remote_Types (Typ) or else Is_Remote_Call_Interface (Typ))
                 and then Original_View_In_Visible_Part (Typ))
           or else not Comes_From_Source (Typ)
         then
            Status := Standard_True;
         else
            Status := Standard_False;
         end if;

         Append_To (Elab_Code,
           Make_DT_Access_Action (Typ,
             Action => Set_Remotely_Callable,
             Args   => New_List (
               New_Occurrence_Of (DT_Ptr, Loc),
               New_Occurrence_Of (Status, Loc))));
      end;

      --  Generate: Set_External_Tag (DT_Ptr, exname'Address);
      --  Should be the external name not the qualified name???

      if not Has_External_Tag_Rep_Clause (Typ) then
         Append_To (Elab_Code,
           Make_DT_Access_Action (Typ,
             Action => Set_External_Tag,
             Args   => New_List (
               Node1 => New_Reference_To (DT_Ptr, Loc),
               Node2 =>
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Exname, Loc),
                   Attribute_Name => Name_Address))));

      --  Generate code to register the Tag in the External_Tag hash
      --  table for the pure Ada type only.

      --        Register_Tag (Dt_Ptr);

      --  Skip this if routine not available, or in No_Run_Time mode

         if RTE_Available (RE_Register_Tag)
           and then Is_RTE (Generalized_Tag, RE_Tag)
           and then not No_Run_Time_Mode
         then
            Append_To (Elab_Code,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Register_Tag), Loc),
                Parameter_Associations =>
                  New_List (New_Reference_To (DT_Ptr, Loc))));
         end if;
      end if;

      --  Generate:
      --     if No_Reg then
      --        <elab_code>
      --        No_Reg := False;
      --     end if;

      Append_To (Elab_Code,
        Make_Assignment_Statement (Loc,
          Name       => New_Reference_To (No_Reg, Loc),
          Expression => New_Reference_To (Standard_False, Loc)));

      Append_To (Result,
        Make_Implicit_If_Statement (Typ,
          Condition       => New_Reference_To (No_Reg, Loc),
          Then_Statements => Elab_Code));

      return Result;
   end Make_DT;

   ---------------------------
   -- Make_DT_Access_Action --
   ---------------------------

   function Make_DT_Access_Action
     (Typ    : Entity_Id;
      Action : DT_Access_Action;
      Args   : List_Id)
      return Node_Id
   is
      Action_Name : Entity_Id;
      Loc         : Source_Ptr;

   begin
      if Is_CPP_Class (Root_Type (Typ)) then
         Action_Name := RTE (CPP_Actions (Action));
      else
         Action_Name := RTE (Ada_Actions (Action));
      end if;

      if No (Args) then

         --  This is a constant

         return New_Reference_To (Action_Name, Sloc (Typ));
      end if;

      pragma Assert (List_Length (Args) = Action_Nb_Arg (Action));

      Loc := Sloc (First (Args));

      if Action_Is_Proc (Action) then
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Action_Name, Loc),
             Parameter_Associations => Args);

      else
         return
           Make_Function_Call (Loc,
             Name => New_Reference_To (Action_Name, Loc),
             Parameter_Associations => Args);
      end if;
   end Make_DT_Access_Action;

   -----------------------------------
   -- Original_View_In_Visible_Part --
   -----------------------------------

   function Original_View_In_Visible_Part (Typ : Entity_Id) return Boolean is
      Scop : constant Entity_Id := Scope (Typ);

   begin
      --  The scope must be a package

      if Ekind (Scop) /= E_Package
        and then Ekind (Scop) /= E_Generic_Package
      then
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

   -------------------------
   -- Set_All_DT_Position --
   -------------------------

   procedure Set_All_DT_Position (Typ : Entity_Id) is
      Parent_Typ : constant Entity_Id := Etype (Typ);
      Root_Typ   : constant Entity_Id := Root_Type (Typ);
      First_Prim : constant Elmt_Id := First_Elmt (Primitive_Operations (Typ));
      The_Tag    : constant Entity_Id := Tag_Component (Typ);
      Adjusted   : Boolean := False;
      Finalized  : Boolean := False;
      Parent_EC  : Int;
      Nb_Prim    : Int;
      Prim       : Entity_Id;
      Prim_Elmt  : Elmt_Id;

   begin

      --  Get Entry_Count of the parent

      if Parent_Typ /= Typ
        and then DT_Entry_Count (Tag_Component (Parent_Typ)) /= No_Uint
      then
         Parent_EC := UI_To_Int (DT_Entry_Count (Tag_Component (Parent_Typ)));
      else
         Parent_EC := 0;
      end if;

      --  C++ Case, check that pragma CPP_Class, CPP_Virtual and CPP_Vtable
      --  give a coherent set of information

      if Is_CPP_Class (Root_Typ) then

         --  Compute the number of primitive operations in the main Vtable
         --  Set their position:
         --    - where it was set if overriden or inherited
         --    - after the end of the parent vtable otherwise

         Prim_Elmt := First_Prim;
         Nb_Prim := 0;
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);

            if not Is_CPP_Class (Typ) then
               Set_DTC_Entity (Prim, The_Tag);

            elsif Present (Alias (Prim)) then
               Set_DTC_Entity (Prim, DTC_Entity (Alias (Prim)));
               Set_DT_Position (Prim, DT_Position (Alias (Prim)));

            elsif No (DTC_Entity (Prim)) and then Is_CPP_Class (Typ) then
                  Error_Msg_NE ("is a primitive operation of&," &
                    " pragma Cpp_Virtual required", Prim, Typ);
            end if;

            if DTC_Entity (Prim) = The_Tag then

               --  Get the slot from the parent subprogram if any

               declare
                  H : Entity_Id := Homonym (Prim);

               begin
                  while Present (H) loop
                     if Present (DTC_Entity (H))
                       and then Root_Type (Scope (DTC_Entity (H))) = Root_Typ
                     then
                        Set_DT_Position (Prim, DT_Position (H));
                        exit;
                     end if;

                     H := Homonym (H);
                  end loop;
               end;

               --  Otherwise take the canonical slot after the end of the
               --  parent Vtable

               if DT_Position (Prim) = No_Uint then
                  Nb_Prim := Nb_Prim + 1;
                  Set_DT_Position (Prim, UI_From_Int (Parent_EC + Nb_Prim));

               elsif UI_To_Int (DT_Position (Prim)) > Parent_EC then
                  Nb_Prim := Nb_Prim + 1;
               end if;
            end if;

            Next_Elmt (Prim_Elmt);
         end loop;

         --  Check that the declared size of the Vtable is bigger or equal
         --  than the number of primitive operations (if bigger it means that
         --  some of the c++ virtual functions were not imported, that is
         --  allowed)

         if DT_Entry_Count (The_Tag) = No_Uint
           or else not Is_CPP_Class (Typ)
         then
            Set_DT_Entry_Count (The_Tag, UI_From_Int (Parent_EC + Nb_Prim));

         elsif UI_To_Int (DT_Entry_Count (The_Tag)) < Parent_EC + Nb_Prim then
            Error_Msg_N ("not enough room in the Vtable for all virtual"
              & " functions", The_Tag);
         end if;

         --  Check that Positions are not duplicate nor outside the range of
         --  the Vtable

         declare
            Size : constant Int := UI_To_Int (DT_Entry_Count (The_Tag));
            Pos  : Int;
            Prim_Pos_Table : array (1 .. Size) of Entity_Id :=
                                                        (others => Empty);

         begin
            Prim_Elmt := First_Prim;
            while Present (Prim_Elmt) loop
               Prim := Node (Prim_Elmt);

               if DTC_Entity (Prim) = The_Tag then
                  Pos := UI_To_Int (DT_Position (Prim));

                  if Pos not in Prim_Pos_Table'Range then
                     Error_Msg_N
                       ("position not in range of virtual table", Prim);

                  elsif Present (Prim_Pos_Table (Pos)) then
                     Error_Msg_NE ("cannot be at the same position in the"
                       & " vtable than&", Prim, Prim_Pos_Table (Pos));

                  else
                     Prim_Pos_Table (Pos) := Prim;
                  end if;
               end if;

               Next_Elmt (Prim_Elmt);
            end loop;
         end;

      --  For regular Ada tagged types, just set the DT_Position for
      --  each primitive operation. Perform some sanity checks to avoid
      --  to build completely inconsistant dispatch tables.

      --  Note that the _Size primitive is always set at position 1 in order
      --  to comply with the needs of Ada.Tags.Parent_Size (see documentation
      --  in a-tags.ad?)

      else
         Nb_Prim := 1;
         Prim_Elmt := First_Prim;
         while Present (Prim_Elmt) loop
            Nb_Prim := Nb_Prim + 1;
            Prim := Node (Prim_Elmt);
            Set_DTC_Entity (Prim, The_Tag);

            if Chars (Prim) = Name_uSize then
               Set_DT_Position (Prim, Uint_1);
               Nb_Prim := Nb_Prim - 1;
            else
               Set_DT_Position (Prim, UI_From_Int (Nb_Prim));
            end if;

            if Chars (Prim) = Name_Finalize
              and then
                (Is_Predefined_File_Name (Unit_File_Name (Current_Sem_Unit))
                   or else not Is_Predefined_File_Name
                                  (Unit_File_Name (Get_Source_Unit (Prim))))
            then
               Finalized := True;
            end if;

            if Chars (Prim) = Name_Adjust then
               Adjusted := True;
            end if;

            --  An abstract operation cannot be declared in the private part
            --  for a visible abstract type, because it could never be over-
            --  ridden. For explicit declarations this is checked at the point
            --  of declaration, but for inherited operations it must be done
            --  when building the dispatch table. Input is excluded because

            if Is_Abstract (Typ)
              and then Is_Abstract (Prim)
              and then Present (Alias (Prim))
              and then Is_Derived_Type (Typ)
              and then In_Private_Part (Current_Scope)
              and then List_Containing (Parent (Prim))
               =  Private_Declarations
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
                     " must be overridden ('R'M 3.9.3(10))",
                     Parent (Typ), Prim);
               end if;
            end if;
            Next_Elmt (Prim_Elmt);
         end loop;

         if Is_Controlled (Typ) then
            if not Finalized then
               Error_Msg_N
                 ("controlled type has no explicit Finalize method?", Typ);

            elsif not Adjusted then
               Error_Msg_N
                 ("controlled type has no explicit Adjust method?", Typ);
            end if;
         end if;

         Set_DT_Entry_Count (The_Tag, UI_From_Int (Nb_Prim));

         --  The derived type must have at least as many components as its
         --  parent (for root types, the Etype points back to itself
         --  and the test should not fail)

         pragma Assert (
           DT_Entry_Count (The_Tag) >=
           DT_Entry_Count (Tag_Component (Parent_Typ)));
      end if;
   end Set_All_DT_Position;

   -----------------------------
   -- Set_Default_Constructor --
   -----------------------------

   procedure Set_Default_Constructor (Typ : Entity_Id) is
      Loc   : Source_Ptr;
      Init  : Entity_Id;
      Param : Entity_Id;
      E     : Entity_Id;

   begin
      --  Look for the default constructor entity. For now only the
      --  default constructor has the flag Is_Constructor.

      E := Next_Entity (Typ);
      while Present (E)
        and then (Ekind (E) /= E_Function or else not Is_Constructor (E))
      loop
         Next_Entity (E);
      end loop;

      --  Create the init procedure

      if Present (E) then
         Loc   := Sloc (E);
         Init  := Make_Defining_Identifier (Loc, Make_Init_Proc_Name (Typ));
         Param := Make_Defining_Identifier (Loc, Name_X);

         Discard_Node (
           Make_Subprogram_Declaration (Loc,
             Make_Procedure_Specification (Loc,
               Defining_Unit_Name => Init,
               Parameter_Specifications => New_List (
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier => Param,
                   Parameter_Type      => New_Reference_To (Typ, Loc))))));

         Set_Init_Proc (Typ, Init);
         Set_Is_Imported    (Init);
         Set_Interface_Name (Init, Interface_Name (E));
         Set_Convention     (Init, Convention_C);
         Set_Is_Public      (Init);
         Set_Has_Completion (Init);

      --  If there are no constructors, mark the type as abstract since we
      --  won't be able to declare objects of that type.

      else
         Set_Is_Abstract (Typ);
      end if;
   end Set_Default_Constructor;

end Exp_Disp;
