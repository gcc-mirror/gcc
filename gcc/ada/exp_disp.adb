------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Disp; use Sem_Disp;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Disp is

   --------------------------------
   -- Select_Expansion_Utilities --
   --------------------------------

   --  The following package contains helper routines used in the expansion of
   --  dispatching asynchronous, conditional and timed selects.

   package Select_Expansion_Utilities is
      procedure Build_B
        (Loc    : Source_Ptr;
         Params : List_Id);
      --  Generate:
      --    B : out Communication_Block

      procedure Build_C
        (Loc    : Source_Ptr;
         Params : List_Id);
      --  Generate:
      --    C : out Prim_Op_Kind

      procedure Build_Common_Dispatching_Select_Statements
        (Loc    : Source_Ptr;
         Typ    : Entity_Id;
         DT_Ptr : Entity_Id;
         Stmts  : List_Id);
      --  Ada 2005 (AI-345): Generate statements that are common between
      --  asynchronous, conditional and timed select expansion.

      procedure Build_F
        (Loc    : Source_Ptr;
         Params : List_Id);
      --  Generate:
      --    F : out Boolean

      procedure Build_P
        (Loc    : Source_Ptr;
         Params : List_Id);
      --  Generate:
      --    P : Address

      procedure Build_S
        (Loc    : Source_Ptr;
         Params : List_Id);
      --  Generate:
      --    S : Integer

      procedure Build_T
        (Loc    : Source_Ptr;
         Typ    : Entity_Id;
         Params : List_Id);
      --  Generate:
      --    T : in out Typ
   end Select_Expansion_Utilities;

   package body Select_Expansion_Utilities is

      -------------
      -- Build_B --
      -------------

      procedure Build_B
        (Loc    : Source_Ptr;
         Params : List_Id)
      is
      begin
         Append_To (Params,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uB),
             Parameter_Type =>
               New_Reference_To (RTE (RE_Communication_Block), Loc),
             Out_Present => True));
      end Build_B;

      -------------
      -- Build_C --
      -------------

      procedure Build_C
        (Loc    : Source_Ptr;
         Params : List_Id)
      is
      begin
         Append_To (Params,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uC),
             Parameter_Type =>
               New_Reference_To (RTE (RE_Prim_Op_Kind), Loc),
             Out_Present => True));
      end Build_C;

      ------------------------------------------------
      -- Build_Common_Dispatching_Select_Statements --
      ------------------------------------------------

      procedure Build_Common_Dispatching_Select_Statements
        (Loc    : Source_Ptr;
         Typ    : Entity_Id;
         DT_Ptr : Entity_Id;
         Stmts  : List_Id)
      is
      begin
         --  Generate:
         --    C := get_prim_op_kind (tag! (<type>VP), S);

         --  where C is the out parameter capturing the call kind and S is the
         --  dispatch table slot number.

         Append_To (Stmts,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Identifier (Loc, Name_uC),
             Expression =>
               Make_DT_Access_Action (Typ,
                 Action =>
                   Get_Prim_Op_Kind,
                 Args =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         --  Generate:

         --    if C = POK_Procedure
         --      or else C = POK_Protected_Procedure
         --      or else C = POK_Task_Procedure;
         --    then
         --       F := True;
         --       return;

         --  where F is the out parameter capturing the status of a potential
         --  entry call.

         Append_To (Stmts,
           Make_If_Statement (Loc,

             Condition =>
               Make_Or_Else (Loc,
                 Left_Opnd =>
                   Make_Op_Eq (Loc,
                     Left_Opnd =>
                       Make_Identifier (Loc, Name_uC),
                     Right_Opnd =>
                       New_Reference_To (RTE (RE_POK_Procedure), Loc)),
                 Right_Opnd =>
                   Make_Or_Else (Loc,
                     Left_Opnd =>
                       Make_Op_Eq (Loc,
                         Left_Opnd =>
                           Make_Identifier (Loc, Name_uC),
                         Right_Opnd =>
                           New_Reference_To (RTE (
                             RE_POK_Protected_Procedure), Loc)),
                     Right_Opnd =>
                       Make_Op_Eq (Loc,
                         Left_Opnd =>
                           Make_Identifier (Loc, Name_uC),
                         Right_Opnd =>
                           New_Reference_To (RTE (
                             RE_POK_Task_Procedure), Loc)))),

             Then_Statements =>
               New_List (
                 Make_Assignment_Statement (Loc,
                   Name       => Make_Identifier (Loc, Name_uF),
                   Expression => New_Reference_To (Standard_True, Loc)),

                 Make_Return_Statement (Loc))));
      end Build_Common_Dispatching_Select_Statements;

      -------------
      -- Build_F --
      -------------

      procedure Build_F
        (Loc    : Source_Ptr;
         Params : List_Id)
      is
      begin
         Append_To (Params,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uF),
             Parameter_Type =>
               New_Reference_To (Standard_Boolean, Loc),
             Out_Present => True));
      end Build_F;

      -------------
      -- Build_P --
      -------------

      procedure Build_P
        (Loc    : Source_Ptr;
         Params : List_Id)
      is
      begin
         Append_To (Params,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uP),
             Parameter_Type =>
               New_Reference_To (RTE (RE_Address), Loc)));
      end Build_P;

      -------------
      -- Build_S --
      -------------

      procedure Build_S
        (Loc    : Source_Ptr;
         Params : List_Id)
      is
      begin
         Append_To (Params,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uS),
             Parameter_Type =>
               New_Reference_To (Standard_Integer, Loc)));
      end Build_S;

      -------------
      -- Build_T --
      -------------

      procedure Build_T
        (Loc    : Source_Ptr;
         Typ    : Entity_Id;
         Params : List_Id)
      is
      begin
         Append_To (Params,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uT),
             Parameter_Type =>
               New_Reference_To (Typ, Loc),
             In_Present  => True,
             Out_Present => True));
      end Build_T;
   end Select_Expansion_Utilities;

   package SEU renames Select_Expansion_Utilities;

   Ada_Actions : constant array (DT_Access_Action) of RE_Id :=
      (IW_Membership                  => RE_IW_Membership,
       Get_Entry_Index                => RE_Get_Entry_Index,
       Get_Prim_Op_Kind               => RE_Get_Prim_Op_Kind,
       Get_Tagged_Kind                => RE_Get_Tagged_Kind,
       Register_Interface_Tag         => RE_Register_Interface_Tag,
       Register_Tag                   => RE_Register_Tag,
       Set_Entry_Index                => RE_Set_Entry_Index,
       Set_Offset_Index               => RE_Set_Offset_Index,
       Set_OSD                        => RE_Set_OSD,
       Set_Prim_Op_Kind               => RE_Set_Prim_Op_Kind,
       Set_Signature                  => RE_Set_Signature,
       Set_SSD                        => RE_Set_SSD,
       Set_Tagged_Kind                => RE_Set_Tagged_Kind);

   Action_Is_Proc : constant array (DT_Access_Action) of Boolean :=
      (IW_Membership                  => False,
       Get_Entry_Index                => False,
       Get_Prim_Op_Kind               => False,
       Get_Tagged_Kind                => False,
       Register_Interface_Tag         => True,
       Register_Tag                   => True,
       Set_Entry_Index                => True,
       Set_Offset_Index               => True,
       Set_OSD                        => True,
       Set_Prim_Op_Kind               => True,
       Set_Signature                  => True,
       Set_SSD                        => True,
       Set_Tagged_Kind                => True);

   Action_Nb_Arg : constant array (DT_Access_Action) of Int :=
      (IW_Membership                  => 2,
       Get_Entry_Index                => 2,
       Get_Prim_Op_Kind               => 2,
       Get_Tagged_Kind                => 1,
       Register_Interface_Tag         => 3,
       Register_Tag                   => 1,
       Set_Entry_Index                => 3,
       Set_Offset_Index               => 3,
       Set_OSD                        => 2,
       Set_Prim_Op_Kind               => 3,
       Set_Signature                  => 2,
       Set_SSD                        => 2,
       Set_Tagged_Kind                => 2);

   function Default_Prim_Op_Position (E : Entity_Id) return Uint;
   --  Ada 2005 (AI-251): Returns the fixed position in the dispatch table
   --  of the default primitive operations.

   function Is_Predefined_Dispatching_Alias (Prim : Entity_Id) return Boolean;
   --  Returns true if Prim is not a predefined dispatching primitive but it is
   --  an alias of a predefined dispatching primitive (ie. through a renaming)

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

         elsif Chars (E) = Name_uDisp_Timed_Select then
            return Uint_15;
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

      Ctrl_Arg   : constant Node_Id := Controlling_Argument (Call_Node);
      Param_List : constant List_Id := Parameter_Associations (Call_Node);

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

   --  Start of processing for Expand_Dispatching_Call

   begin
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

      if Etype (Ctrl_Arg) = RTE (RE_Tag)
        or else (RTE_Available (RE_Interface_Tag)
                  and then Etype (Ctrl_Arg) = RTE (RE_Interface_Tag))
      then
         CW_Typ := Class_Wide_Type (Find_Dispatching_Type (Subp));

      elsif Is_Access_Type (Etype (Ctrl_Arg)) then
         CW_Typ := Designated_Type (Etype (Ctrl_Arg));

      else
         CW_Typ := Etype (Ctrl_Arg);
      end if;

      Typ := Root_Type (CW_Typ);

      if Ekind (Typ) = E_Incomplete_Type then
         Typ := Non_Limited_View (Typ);
      end if;

      if not Is_Limited_Type (Typ) then
         Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
      end if;

      --  Dispatching call to C++ primitive. Create a new parameter list
      --  with no tag checks.

      if Is_CPP_Class (Typ) then
         New_Params := New_List;
         Param := First_Actual (Call_Node);
         while Present (Param) loop
            Append_To (New_Params, Relocate_Node (Param));
            Next_Actual (Param);
         end loop;

      --  Dispatching call to Ada primitive

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

            --  No tag check for function dispatching on result if the
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
               --  to avoid a recursion in dispatching.

               if Is_Controlling_Formal (New_Formal) then
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

            Set_Next_Entity (New_Formal, Empty);
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

      --  If the controlling argument is a value of type Ada.Tag or an abstract
      --  interface class-wide type then use it directly. Otherwise, the tag
      --  must be extracted from the controlling object.

      if Etype (Ctrl_Arg) = RTE (RE_Tag)
        or else (RTE_Available (RE_Interface_Tag)
                  and then Etype (Ctrl_Arg) = RTE (RE_Interface_Tag))
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

      elsif Is_Interface (Etype (Ctrl_Arg))
         and then Is_Class_Wide_Type (Etype (Ctrl_Arg))
      then
         Controlling_Tag := Duplicate_Subexpr (Ctrl_Arg);

      else
         Controlling_Tag :=
           Make_Selected_Component (Loc,
             Prefix => Duplicate_Subexpr_Move_Checks (Ctrl_Arg),
             Selector_Name => New_Reference_To (DTC_Entity (Subp), Loc));
      end if;

      --  Handle dispatching calls to predefined primitives

      if Is_Predefined_Dispatching_Operation (Subp)
        or else Is_Predefined_Dispatching_Alias (Subp)
      then
         New_Call_Name :=
           Unchecked_Convert_To (Subp_Ptr_Typ,
             Build_Get_Predefined_Prim_Op_Address (Loc,
               Tag_Node => Controlling_Tag,
               Position_Node => Make_Integer_Literal (Loc,
                                  DT_Position (Subp))));

      --  Handle dispatching calls to user-defined primitives

      else
         New_Call_Name :=
           Unchecked_Convert_To (Subp_Ptr_Typ,
             Build_Get_Prim_Op_Address (Loc,
               Tag_Node      => Controlling_Tag,
               Position_Node => Make_Integer_Literal (Loc,
                                  DT_Position (Subp))));
      end if;

      if Nkind (Call_Node) = N_Function_Call then

         --  Ada 2005 (AI-251): A dispatching "=" with an abstract interface
         --  just requires the comparison of the tags.

         if Ekind (Etype (Ctrl_Arg)) = E_Class_Wide_Type
           and then Is_Interface (Etype (Ctrl_Arg))
           and then Subp = Eq_Prim_Op
         then
            Param := First_Actual (Call_Node);

            New_Call :=
                Make_Op_Eq (Loc,
                   Left_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix => New_Value (Param),
                       Selector_Name =>
                         New_Reference_To (First_Tag_Component (Typ), Loc)),

                   Right_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix =>
                         Unchecked_Convert_To (Typ,
                           New_Value (Next_Actual (Param))),
                       Selector_Name =>
                         New_Reference_To (First_Tag_Component (Typ), Loc)));

         else
            New_Call :=
              Make_Function_Call (Loc,
                Name => New_Call_Name,
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
                              Prefix => New_Value (Param),
                              Selector_Name =>
                                New_Reference_To (First_Tag_Component (Typ),
                                                  Loc)),

                          Right_Opnd =>
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Unchecked_Convert_To (Typ,
                                  New_Value (Next_Actual (Param))),
                              Selector_Name =>
                                New_Reference_To (First_Tag_Component (Typ),
                                                  Loc))),
                   Right_Opnd => New_Call);
            end if;
         end if;

      else
         New_Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Call_Name,
             Parameter_Associations => New_Params);
      end if;

      Rewrite (Call_Node, New_Call);
      Analyze_And_Resolve (Call_Node, Call_Typ);
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
      Fent        : Entity_Id;
      Func        : Node_Id;
      Iface_Typ   : Entity_Id           := Etype (N);
      Iface_Tag   : Entity_Id;
      New_Itype   : Entity_Id;

   begin
      pragma Assert (Nkind (Operand) /= N_Attribute_Reference);

      --  Ada 2005 (AI-345): Handle synchronized interface type derivations

      if Is_Concurrent_Type (Operand_Typ) then
         Operand_Typ := Base_Type (Corresponding_Record_Type (Operand_Typ));
      end if;

      --  Handle access types to interfaces

      if Is_Access_Type (Iface_Typ) then
         Iface_Typ := Etype (Directly_Designated_Type (Iface_Typ));
      end if;

      --  Handle class-wide interface types. This conversion can appear
      --  explicitly in the source code. Example: I'Class (Obj)

      if Is_Class_Wide_Type (Iface_Typ) then
         Iface_Typ := Etype (Iface_Typ);
      end if;

      pragma Assert (not Is_Static
        or else (not Is_Class_Wide_Type (Iface_Typ)
                  and then Is_Interface (Iface_Typ)));

      if not Is_Static then

         --  Give error if configurable run time and Displace not available

         if not RTE_Available (RE_Displace) then
            Error_Msg_CRT ("abstract interface types", N);
            return;
         end if;

         --  Handle conversion of access to class-wide interface types. The
         --  target can be an access to object or an access to another class
         --  wide interfac (see -1- and -2- in the following example):

         --     type Iface1_Ref is access all Iface1'Class;
         --     type Iface2_Ref is access all Iface1'Class;

         --     Acc1 : Iface1_Ref := new ...
         --     Obj  : Obj_Ref    := Obj_Ref (Acc);    -- 1
         --     Acc2 : Iface2_Ref := Iface2_Ref (Acc); -- 2

         if Is_Access_Type (Operand_Typ) then
            pragma Assert
              (Is_Class_Wide_Type (Directly_Designated_Type (Operand_Typ))
                 and then
               Is_Interface (Directly_Designated_Type (Operand_Typ)));

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

         New_Itype := Create_Itype (E_Anonymous_Access_Type, N);
         Set_Etype       (New_Itype, New_Itype);
         Init_Esize      (New_Itype);
         Init_Size_Align (New_Itype);
         Set_Directly_Designated_Type (New_Itype, Etyp);

         Rewrite (N, Make_Explicit_Dereference (Loc,
                          Unchecked_Convert_To (New_Itype,
                            Relocate_Node (N))));
         Analyze (N);
         Freeze_Itype (New_Itype, N);

         return;
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
         --     begin
         --        if O = Null_Address then
         --           return null;
         --        else
         --           return Iface_Typ!(Operand_Typ!(O).Iface_Tag'Address);
         --        end if;
         --     end Func;

         Fent := Make_Defining_Identifier (Loc, New_Internal_Name ('F'));
         Set_Is_Internal (Fent);

         declare
            Desig_Typ : Entity_Id;
         begin
            Desig_Typ := Etype (Expression (N));

            if Is_Access_Type (Desig_Typ) then
               Desig_Typ := Directly_Designated_Type (Desig_Typ);
            end if;

            New_Itype := Create_Itype (E_Anonymous_Access_Type, N);
            Set_Etype       (New_Itype, New_Itype);
            Set_Scope       (New_Itype, Fent);
            Init_Size_Align (New_Itype);
            Set_Directly_Designated_Type (New_Itype, Desig_Typ);
         end;

         Func :=
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Function_Specification (Loc,
                 Defining_Unit_Name       => Fent,

                 Parameter_Specifications => New_List (
                   Make_Parameter_Specification (Loc,
                     Defining_Identifier =>
                       Make_Defining_Identifier (Loc, Name_uO),
                     Parameter_Type =>
                       New_Reference_To (RTE (RE_Address), Loc))),

                 Result_Definition =>
                   New_Reference_To (Etype (N), Loc)),

             Declarations => Empty_List,

             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_If_Statement (Loc,
                     Condition       =>
                       Make_Op_Eq (Loc,
                          Left_Opnd  => Make_Identifier (Loc, Name_uO),
                          Right_Opnd => New_Reference_To
                                          (RTE (RE_Null_Address), Loc)),

                     Then_Statements => New_List (
                       Make_Return_Statement (Loc,
                         Make_Null (Loc))),

                     Else_Statements => New_List (
                       Make_Return_Statement (Loc,
                         Unchecked_Convert_To (Etype (N),
                           Make_Attribute_Reference (Loc,
                             Prefix =>
                               Make_Selected_Component (Loc,
                                 Prefix => Unchecked_Convert_To (New_Itype,
                                             Make_Identifier (Loc, Name_uO)),
                                 Selector_Name =>
                                   New_Occurrence_Of (Iface_Tag, Loc)),
                             Attribute_Name => Name_Address))))))));

         --  Place function body before the expression containing
         --  the conversion

         Insert_Action (N, Func);
         Analyze (Func);

         if Is_Access_Type (Etype (Expression (N))) then

            --  Generate: Operand_Typ!(Expression.all)'Address

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (Fent, Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix  => Unchecked_Convert_To (Operand_Typ,
                                 Make_Explicit_Dereference (Loc,
                                   Relocate_Node (Expression (N)))),
                    Attribute_Name => Name_Address))));

         else
            --  Generate: Operand_Typ!(Expression)'Address

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (Fent, Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix  => Unchecked_Convert_To (Operand_Typ,
                                 Relocate_Node (Expression (N))),
                    Attribute_Name => Name_Address))));
         end if;
      end if;

      Analyze (N);
   end Expand_Interface_Conversion;

   ------------------------------
   -- Expand_Interface_Actuals --
   ------------------------------

   procedure Expand_Interface_Actuals (Call_Node : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (Call_Node);
      Actual     : Node_Id;
      Actual_Dup : Node_Id;
      Actual_Typ : Entity_Id;
      Anon       : Entity_Id;
      Conversion : Node_Id;
      Formal     : Entity_Id;
      Formal_Typ : Entity_Id;
      Subp       : Entity_Id;
      Nam        : Name_Id;
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

      --  Normal case

      else
         Subp := Entity (Name (Call_Node));
      end if;

      Formal := First_Formal (Subp);
      Actual := First_Actual (Call_Node);
      while Present (Formal) loop

         --  Ada 2005 (AI-251): Conversion to interface to force "this"
         --  displacement.

         Formal_Typ := Etype (Etype (Formal));

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

         if Is_Interface (Formal_Typ) then

            --  No need to displace the pointer if the type of the actual
            --  is class-wide of the formal-type interface; in this case the
            --  displacement of the pointer was already done at the point of
            --  the call to the enclosing subprogram. This case corresponds
            --  with the call to P (Obj) in the following example:

            --     type I is interface;
            --     procedure P (X : I) is abstract;

            --     procedure General_Op (Obj : I'Class) is
            --     begin
            --        P (Obj);
            --     end General_Op;

            if Is_Class_Wide_Type (Actual_Typ)
              and then Etype (Actual_Typ) = Formal_Typ
            then
               null;

            --  No need to displace the pointer if the type of the actual is a
            --  derivation of the formal-type interface because in this case
            --  the interface primitives are located in the primary dispatch
            --  table.

            elsif Is_Parent (Formal_Typ, Actual_Typ) then
               null;

            else
               Conversion := Convert_To (Formal_Typ, Relocate_Node (Actual));
               Rewrite             (Actual, Conversion);
               Analyze_And_Resolve (Actual, Formal_Typ);
            end if;

         --  Anonymous access type

         elsif Is_Access_Type (Formal_Typ)
           and then Is_Interface (Etype (Formal_DDT))
           and then Interface_Present_In_Ancestor
                      (Typ   => Actual_DDT,
                       Iface => Etype (Formal_DDT))
         then
            if Nkind (Actual) = N_Attribute_Reference
              and then
               (Attribute_Name (Actual) = Name_Access
                 or else Attribute_Name (Actual) = Name_Unchecked_Access)
            then
               Nam := Attribute_Name (Actual);

               Conversion := Convert_To (Etype (Formal_DDT), Prefix (Actual));

               Rewrite (Actual, Conversion);
               Analyze_And_Resolve (Actual, Etype (Formal_DDT));

               Rewrite (Actual,
                 Unchecked_Convert_To (Formal_Typ,
                   Make_Attribute_Reference (Loc,
                     Prefix => Relocate_Node (Actual),
                     Attribute_Name => Nam)));

               Analyze_And_Resolve (Actual, Formal_Typ);

            --  No need to displace the pointer if the actual is a class-wide
            --  type of the formal-type interface because in this case the
            --  displacement of the pointer was already done at the point of
            --  the call to the enclosing subprogram (this case is similar
            --  to the example described above for the non access-type case)

            elsif Is_Class_Wide_Type (Actual_DDT)
              and then Etype (Actual_DDT) = Formal_DDT
            then
               null;

            --  No need to displace the pointer if the type of the actual is a
            --  derivation of the interface (because in this case the interface
            --  primitives are located in the primary dispatch table)

            elsif Is_Parent (Formal_DDT, Actual_DDT) then
               null;

            else
               Actual_Dup := Relocate_Node (Actual);

               if From_With_Type (Actual_Typ) then

                  --  If the type of the actual parameter comes from a limited
                  --  with-clause and the non-limited view is already available
                  --  we replace the anonymous access type by a duplicate decla
                  --  ration whose designated type is the non-limited view

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

   function Expand_Interface_Thunk
     (N           : Node_Id;
      Thunk_Alias : Entity_Id;
      Thunk_Id    : Entity_Id) return Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Actuals     : constant List_Id    := New_List;
      Decl        : constant List_Id    := New_List;
      Formals     : constant List_Id    := New_List;
      Target      : Entity_Id;
      New_Code    : Node_Id;
      Formal      : Node_Id;
      New_Formal  : Node_Id;
      Decl_1      : Node_Id;
      Decl_2      : Node_Id;
      E           : Entity_Id;

   begin
      --  Traverse the list of alias to find the final target

      Target := Thunk_Alias;
      while Present (Alias (Target)) loop
         Target := Alias (Target);
      end loop;

      --  Duplicate the formals

      Formal := First_Formal (Target);
      E      := First_Formal (N);
      while Present (Formal) loop
         New_Formal := Copy_Separate_Tree (Parent (Formal));

         --  Propagate the parameter type to the copy. This is required to
         --  properly handle the case in which the subprogram covering the
         --  interface has been inherited:

         --  Example:
         --     type I is interface;
         --     procedure P (X : I) is abstract;

         --     type T is tagged null record;
         --     procedure P (X : T);

         --     type DT is new T and I with ...

         Set_Parameter_Type (New_Formal, New_Reference_To (Etype (E), Loc));
         Append_To (Formals, New_Formal);

         Next_Formal (Formal);
         Next_Formal (E);
      end loop;

      --  Give message if configurable run-time and Offset_To_Top unavailable

      if not RTE_Available (RE_Offset_To_Top) then
         Error_Msg_CRT ("abstract interface types", N);
         return Empty;
      end if;

      if Ekind (First_Formal (Target)) = E_In_Parameter
        and then Ekind (Etype (First_Formal (Target)))
                  = E_Anonymous_Access_Type
      then
         --  Generate:

         --     type T is access all <<type of the first formal>>
         --     S1 := Storage_Offset!(First_formal)
         --           - Offset_To_Top (First_Formal.Tag)

         --  ... and the first actual of the call is generated as T!(S1)

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
                        (Etype (First_Formal (Target))), Loc)));

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
                      New_Reference_To
                        (Defining_Identifier (First (Formals)), Loc)),
                  Right_Opnd =>
                    Make_Function_Call (Loc,
                      Name => New_Reference_To (RTE (RE_Offset_To_Top), Loc),
                      Parameter_Associations => New_List (
                        Unchecked_Convert_To
                          (RTE (RE_Address),
                           New_Reference_To
                             (Defining_Identifier (First (Formals)), Loc))))));

         Append_To (Decl, Decl_2);
         Append_To (Decl, Decl_1);

         --  Reference the new first actual

         Append_To (Actuals,
           Unchecked_Convert_To
             (Defining_Identifier (Decl_2),
              New_Reference_To (Defining_Identifier (Decl_1), Loc)));

      else
         --  Generate:

         --     S1 := Storage_Offset!(First_formal'Address)
         --           - Offset_To_Top (First_Formal.Tag)
         --     S2 := Tag_Ptr!(S3)

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
                            (Defining_Identifier (First (Formals)), Loc),
                        Attribute_Name => Name_Address)),
                 Right_Opnd =>
                    Make_Function_Call (Loc,
                      Name => New_Reference_To (RTE (RE_Offset_To_Top), Loc),
                      Parameter_Associations => New_List (
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Reference_To
                                      (Defining_Identifier (First (Formals)),
                                       Loc),
                          Attribute_Name => Name_Address)))));

         Decl_2 :=
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, New_Internal_Name ('S')),
             Constant_Present    => True,
             Object_Definition   => New_Reference_To (RTE (RE_Addr_Ptr), Loc),
             Expression          =>
               Unchecked_Convert_To
                 (RTE (RE_Addr_Ptr),
                  New_Reference_To (Defining_Identifier (Decl_1), Loc)));

         Append_To (Decl, Decl_1);
         Append_To (Decl, Decl_2);

         --  Reference the new first actual

         Append_To (Actuals,
           Unchecked_Convert_To
             (Etype (First_Entity (Target)),
              Make_Explicit_Dereference (Loc,
                New_Reference_To (Defining_Identifier (Decl_2), Loc))));
      end if;

      Formal := Next (First (Formals));
      while Present (Formal) loop
         Append_To (Actuals,
            New_Reference_To (Defining_Identifier (Formal), Loc));
         Next (Formal);
      end loop;

      if Ekind (Target) = E_Procedure then
         New_Code :=
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

         New_Code :=
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
                    Make_Return_Statement (Loc,
                      Make_Function_Call (Loc,
                        Name => New_Occurrence_Of (Target, Loc),
                        Parameter_Associations => Actuals)))));
      end if;

      --  Analyze the code of the thunk with checks suppressed because we are
      --  in the middle of building the dispatch information itself and some
      --  characteristics of the type may not be fully available.

      Analyze (New_Code, Suppress => All_Checks);
      return New_Code;
   end Expand_Interface_Thunk;

   -------------------
   -- Fill_DT_Entry --
   -------------------

   function Fill_DT_Entry
     (Loc     : Source_Ptr;
      Prim    : Entity_Id) return Node_Id
   is
      Typ     : constant Entity_Id := Scope (DTC_Entity (Prim));
      DT_Ptr  : constant Entity_Id :=
                  Node (First_Elmt (Access_Disp_Table (Typ)));
      Pos     : constant Uint      := DT_Position (Prim);
      Tag     : constant Entity_Id := First_Tag_Component (Typ);

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      if Is_Predefined_Dispatching_Operation (Prim)
        or else Is_Predefined_Dispatching_Alias (Prim)
      then
         return
           Build_Set_Predefined_Prim_Op_Address (Loc,
             Tag_Node      => New_Reference_To (DT_Ptr, Loc),
             Position_Node => Make_Integer_Literal (Loc, Pos),
             Address_Node  => Make_Attribute_Reference (Loc,
                                Prefix => New_Reference_To (Prim, Loc),
                                Attribute_Name => Name_Address));

      else
         pragma Assert (Pos /= Uint_0 and then Pos <= DT_Entry_Count (Tag));

         return
           Build_Set_Prim_Op_Address (Loc,
             Tag_Node      => New_Reference_To (DT_Ptr, Loc),
             Position_Node => Make_Integer_Literal (Loc, Pos),
             Address_Node  => Make_Attribute_Reference (Loc,
                                Prefix => New_Reference_To (Prim, Loc),
                                Attribute_Name => Name_Address));
      end if;
   end Fill_DT_Entry;

   -----------------------------
   -- Fill_Secondary_DT_Entry --
   -----------------------------

   function Fill_Secondary_DT_Entry
     (Loc          : Source_Ptr;
      Prim         : Entity_Id;
      Thunk_Id     : Entity_Id;
      Iface_DT_Ptr : Entity_Id) return Node_Id
   is
      Iface_Prim : constant Entity_Id := Abstract_Interface_Alias (Prim);
      Pos        : constant Uint      := DT_Position (Iface_Prim);
      Tag        : constant Entity_Id :=
                     First_Tag_Component (Scope (DTC_Entity (Iface_Prim)));

   begin
      if Is_Predefined_Dispatching_Operation (Prim)
        or else Is_Predefined_Dispatching_Alias (Prim)
      then
         return
           Build_Set_Predefined_Prim_Op_Address (Loc,
             Tag_Node =>
               New_Reference_To (Iface_DT_Ptr, Loc),
             Position_Node =>
               Make_Integer_Literal (Loc, Pos),
             Address_Node =>
               Make_Attribute_Reference (Loc,
                 Prefix          => New_Reference_To (Thunk_Id, Loc),
                 Attribute_Name  => Name_Address));
      else
         pragma Assert (Pos /= Uint_0 and then Pos <= DT_Entry_Count (Tag));

         return
           Build_Set_Prim_Op_Address (Loc,
             Tag_Node      => New_Reference_To (Iface_DT_Ptr, Loc),
             Position_Node => Make_Integer_Literal (Loc, Pos),
             Address_Node  => Make_Attribute_Reference (Loc,
                                Prefix => New_Reference_To (Thunk_Id, Loc),
                                Attribute_Name => Name_Address));
      end if;
   end Fill_Secondary_DT_Entry;

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

   ----------------------------------------
   -- Make_Disp_Asynchronous_Select_Body --
   ----------------------------------------

   function Make_Disp_Asynchronous_Select_Body
     (Typ : Entity_Id) return Node_Id
   is
      Conc_Typ : Entity_Id           := Empty;
      Decls    : constant List_Id    := New_List;
      DT_Ptr   : Entity_Id;
      Loc      : constant Source_Ptr := Sloc (Typ);
      Stmts    : constant List_Id    := New_List;

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
         --    I : Integer := Get_Entry_Index (tag! (<type>VP), S);

         --  where I will be used to capture the entry index of the primitive
         --  wrapper at position S.

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uI),
             Object_Definition =>
               New_Reference_To (Standard_Integer, Loc),
             Expression =>
               Make_DT_Access_Action (Typ,
                 Action =>
                   Get_Entry_Index,
                 Args =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         if Ekind (Conc_Typ) = E_Protected_Type then

            --  Generate:
            --    Protected_Entry_Call (
            --      T._object'access,
            --      protected_entry_index! (I),
            --      P,
            --      Asynchronous_Call,
            --      B);

            --  where T is the protected object, I is the entry index, P are
            --  the wrapped parameters and B is the name of the communication
            --  block.

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Protected_Entry_Call), Loc),
                Parameter_Associations =>
                  New_List (

                    Make_Attribute_Reference (Loc,        -- T._object'access
                      Attribute_Name =>
                        Name_Unchecked_Access,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Make_Identifier (Loc, Name_uT),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_uObject))),

                    Make_Unchecked_Type_Conversion (Loc,  --  entry index
                      Subtype_Mark =>
                        New_Reference_To (RTE (RE_Protected_Entry_Index), Loc),
                      Expression =>
                        Make_Identifier (Loc, Name_uI)),

                    Make_Identifier (Loc, Name_uP),       --  parameter block
                    New_Reference_To (                    --  Asynchronous_Call
                      RTE (RE_Asynchronous_Call), Loc),
                    Make_Identifier (Loc, Name_uB))));    --  comm block
         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);

            --  Generate:
            --    Protected_Entry_Call (
            --      T._task_id,
            --      task_entry_index! (I),
            --      P,
            --      Conditional_Call,
            --      F);

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
                    New_Reference_To (                    --  Asynchronous_Call
                      RTE (RE_Asynchronous_Call), Loc),
                    Make_Identifier (Loc, Name_uF))));    --  status flag
         end if;
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

      --  "T" - Object parameter
      --  "S" - Primitive operation slot
      --  "P" - Wrapped parameters
      --  "B" - Communication block
      --  "F" - Status flag

      SEU.Build_T (Loc, Typ, Params);
      SEU.Build_S (Loc, Params);
      SEU.Build_P (Loc, Params);
      SEU.Build_B (Loc, Params);
      SEU.Build_F (Loc, Params);

      Set_Is_Internal (Def_Id);

      return
         Make_Procedure_Specification (Loc,
           Defining_Unit_Name       => Def_Id,
           Parameter_Specifications => Params);
   end Make_Disp_Asynchronous_Select_Spec;

   ---------------------------------------
   -- Make_Disp_Conditional_Select_Body --
   ---------------------------------------

   function Make_Disp_Conditional_Select_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Blk_Nam  : Entity_Id;
      Conc_Typ : Entity_Id           := Empty;
      Decls    : constant List_Id    := New_List;
      DT_Ptr   : Entity_Id;
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
         --    C := Get_Prim_Op_Kind (tag! (<type>VP), S);

         --    if C = POK_Procedure
         --      or else C = POK_Protected_Procedure
         --      or else C = POK_Task_Procedure;
         --    then
         --       F := True;
         --       return;
         --    end if;

         SEU.Build_Common_Dispatching_Select_Statements
          (Loc, Typ, DT_Ptr, Stmts);

         --  Generate:
         --    Bnn : Communication_Block;

         --  where Bnn is the name of the communication block used in
         --  the call to Protected_Entry_Call.

         Blk_Nam := Make_Defining_Identifier (Loc, New_Internal_Name ('B'));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Blk_Nam,
             Object_Definition =>
               New_Reference_To (RTE (RE_Communication_Block), Loc)));

         --  Generate:
         --    I := Get_Entry_Index (tag! (<type>VP), S);

         --  I is the entry index and S is the dispatch table slot

         Append_To (Stmts,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Identifier (Loc, Name_uI),
             Expression =>
               Make_DT_Access_Action (Typ,
                 Action =>
                   Get_Entry_Index,
                 Args =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         if Ekind (Conc_Typ) = E_Protected_Type then

            --  Generate:
            --    Protected_Entry_Call (
            --      T._object'access,
            --      protected_entry_index! (I),
            --      P,
            --      Conditional_Call,
            --      Bnn);

            --  where T is the protected object, I is the entry index, P are
            --  the wrapped parameters and Bnn is the name of the communication
            --  block.

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Protected_Entry_Call), Loc),
                Parameter_Associations =>
                  New_List (

                    Make_Attribute_Reference (Loc,        -- T._object'access
                      Attribute_Name =>
                        Name_Unchecked_Access,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Make_Identifier (Loc, Name_uT),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_uObject))),

                    Make_Unchecked_Type_Conversion (Loc,  --  entry index
                      Subtype_Mark =>
                        New_Reference_To (RTE (RE_Protected_Entry_Index), Loc),
                      Expression =>
                        Make_Identifier (Loc, Name_uI)),

                    Make_Identifier (Loc, Name_uP),       --  parameter block
                    New_Reference_To (                    --  Conditional_Call
                      RTE (RE_Conditional_Call), Loc),
                    New_Reference_To (                    --  Bnn
                      Blk_Nam, Loc))));

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
            --    Protected_Entry_Call (
            --      T._task_id,
            --      task_entry_index! (I),
            --      P,
            --      Conditional_Call,
            --      F);

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

      --  "T" - Object parameter
      --  "S" - Primitive operation slot
      --  "P" - Wrapped parameters
      --  "C" - Call kind
      --  "F" - Status flag

      SEU.Build_T (Loc, Typ, Params);
      SEU.Build_S (Loc, Params);
      SEU.Build_P (Loc, Params);
      SEU.Build_C (Loc, Params);
      SEU.Build_F (Loc, Params);

      Set_Is_Internal (Def_Id);

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
                    Make_DT_Access_Action (Typ,
                      Action =>
                        Get_Prim_Op_Kind,
                      Args =>
                        New_List (
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

      --  "T" - Object parameter
      --  "S" - Primitive operation slot
      --  "C" - Call kind

      SEU.Build_T (Loc, Typ, Params);
      SEU.Build_S (Loc, Params);
      SEU.Build_C (Loc, Params);

      Set_Is_Internal (Def_Id);

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
         Ret :=
           Make_Return_Statement (Loc,
             Expression =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Make_Identifier (Loc, Name_uT),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_uTask_Id)));

      --  A null body is constructed for non-task types

      else
         Ret :=
           Make_Return_Statement (Loc,
             Expression =>
               New_Reference_To (RTE (RO_ST_Null_Task), Loc));
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
      Loc    : constant Source_Ptr := Sloc (Typ);
      Def_Id : constant Node_Id    :=
                 Make_Defining_Identifier (Loc,
                   Name_uDisp_Get_Task_Id);

   begin
      pragma Assert (not Restriction_Active (No_Dispatching_Calls));

      Set_Is_Internal (Def_Id);

      return
        Make_Function_Specification (Loc,
          Defining_Unit_Name       => Def_Id,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_uT),
              Parameter_Type =>
                New_Reference_To (Typ, Loc))),
          Result_Definition =>
            New_Reference_To (RTE (RO_ST_Task_Id), Loc));
   end Make_Disp_Get_Task_Id_Spec;

   ---------------------------------
   -- Make_Disp_Timed_Select_Body --
   ---------------------------------

   function Make_Disp_Timed_Select_Body
     (Typ : Entity_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Conc_Typ : Entity_Id           := Empty;
      Decls    : constant List_Id    := New_List;
      DT_Ptr   : Entity_Id;
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

         SEU.Build_Common_Dispatching_Select_Statements
          (Loc, Typ, DT_Ptr, Stmts);

         --  Generate:
         --    I := Get_Entry_Index (tag! (<type>VP), S);

         --  I is the entry index and S is the dispatch table slot

         Append_To (Stmts,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Identifier (Loc, Name_uI),
             Expression =>
               Make_DT_Access_Action (Typ,
                 Action =>
                   Get_Entry_Index,
                 Args =>
                   New_List (
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (DT_Ptr, Loc)),
                     Make_Identifier (Loc, Name_uS)))));

         if Ekind (Conc_Typ) = E_Protected_Type then

            --  Generate:
            --    Timed_Protected_Entry_Call (
            --      T._object'access,
            --      protected_entry_index! (I),
            --      P,
            --      D,
            --      M,
            --      F);

            --  where T is the protected object, I is the entry index, P are
            --  the wrapped parameters, D is the delay amount, M is the delay
            --  mode and F is the status flag.

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Timed_Protected_Entry_Call), Loc),
                Parameter_Associations =>
                  New_List (

                    Make_Attribute_Reference (Loc,        -- T._object'access
                      Attribute_Name =>
                        Name_Unchecked_Access,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Make_Identifier (Loc, Name_uT),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_uObject))),

                    Make_Unchecked_Type_Conversion (Loc,  --  entry index
                      Subtype_Mark =>
                        New_Reference_To (RTE (RE_Protected_Entry_Index), Loc),
                      Expression =>
                        Make_Identifier (Loc, Name_uI)),

                    Make_Identifier (Loc, Name_uP),       --  parameter block
                    Make_Identifier (Loc, Name_uD),       --  delay
                    Make_Identifier (Loc, Name_uM),       --  delay mode
                    Make_Identifier (Loc, Name_uF))));    --  status flag

         else
            pragma Assert (Ekind (Conc_Typ) = E_Task_Type);

            --  Generate:
            --    Timed_Task_Entry_Call (
            --      T._task_id,
            --      task_entry_index! (I),
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

      --  "T" - Object parameter
      --  "S" - Primitive operation slot
      --  "P" - Wrapped parameters
      --  "D" - Delay
      --  "M" - Delay Mode
      --  "C" - Call kind
      --  "F" - Status flag

      SEU.Build_T (Loc, Typ, Params);
      SEU.Build_S (Loc, Params);
      SEU.Build_P (Loc, Params);

      Append_To (Params,
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uD),
          Parameter_Type =>
            New_Reference_To (Standard_Duration, Loc)));

      Append_To (Params,
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uM),
          Parameter_Type =>
            New_Reference_To (Standard_Integer, Loc)));

      SEU.Build_C (Loc, Params);
      SEU.Build_F (Loc, Params);

      Set_Is_Internal (Def_Id);

      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => Def_Id,
          Parameter_Specifications => Params);
   end Make_Disp_Timed_Select_Spec;

   -------------
   -- Make_DT --
   -------------

   function Make_DT (Typ : Entity_Id) return List_Id is
      Loc         : constant Source_Ptr := Sloc (Typ);
      Result      : constant List_Id    := New_List;
      Elab_Code   : constant List_Id    := New_List;

      Tname       : constant Name_Id := Chars (Typ);
      Name_DT     : constant Name_Id := New_External_Name (Tname, 'T');
      Name_DT_Ptr : constant Name_Id := New_External_Name (Tname, 'P');
      Name_SSD    : constant Name_Id := New_External_Name (Tname, 'S');
      Name_TSD    : constant Name_Id := New_External_Name (Tname, 'B');
      Name_Exname : constant Name_Id := New_External_Name (Tname, 'E');
      Name_No_Reg : constant Name_Id := New_External_Name (Tname, 'F');

      --  The following external name is only generated if Typ has interfaces
      Name_ITable : Name_Id;

      DT     : constant Node_Id := Make_Defining_Identifier (Loc, Name_DT);
      DT_Ptr : constant Node_Id := Make_Defining_Identifier (Loc, Name_DT_Ptr);
      SSD    : constant Node_Id := Make_Defining_Identifier (Loc, Name_SSD);
      TSD    : constant Node_Id := Make_Defining_Identifier (Loc, Name_TSD);
      Exname : constant Node_Id := Make_Defining_Identifier (Loc, Name_Exname);
      No_Reg : constant Node_Id := Make_Defining_Identifier (Loc, Name_No_Reg);

      Generalized_Tag    : constant Entity_Id := RTE (RE_Tag);
      Ancestor_Ifaces    : Elist_Id;
      AI                 : Elmt_Id;
      Has_Dispatch_Table : Boolean := True;
      I_Depth            : Nat := 0;
      ITable             : Node_Id;
      Iface_Table_Node   : Node_Id;
      Nb_Prim            : Nat := 0;
      Null_Parent_Tag    : Boolean := False;
      Num_Ifaces         : Nat := 0;
      Old_Tag1           : Node_Id;
      Old_Tag2           : Node_Id;
      Parent             : Entity_Id;
      Parent_Num_Ifaces  : Nat := 0;
      Remotely_Callable  : Entity_Id;
      RC_Offset_Node     : Node_Id;
      Size_Expr_Node     : Node_Id;
      Typ_Ifaces         : Elist_Id;
      TSD_Aggr_List      : List_Id;

   begin
      if not RTE_Available (RE_Tag) then
         Error_Msg_CRT ("tagged types", Typ);
         return New_List;
      end if;

      --  Ensure that the unit System_Storage_Elements is loaded. This is
      --  required to properly expand the routines of Ada.Tags

      if not RTU_Loaded (System_Storage_Elements)
        and then not Present (RTE (RE_Storage_Offset))
      then
         raise Program_Error;
      end if;

      if Ada_Version >= Ada_05 then

         --  Count the interface types of the parents

         Parent := Empty;

         if Typ /= Etype (Typ) then
            Parent := Etype (Typ);

         elsif Is_Concurrent_Record_Type (Typ) then
            Parent := Etype (First (Abstract_Interface_List (Typ)));
         end if;

         if Present (Parent) then
            Collect_Abstract_Interfaces (Parent, Ancestor_Ifaces);

            AI := First_Elmt (Ancestor_Ifaces);
            while Present (AI) loop
               Parent_Num_Ifaces := Parent_Num_Ifaces + 1;
               Next_Elmt (AI);
            end loop;
         end if;

         --  Count the additional interfaces implemented by Typ

         Collect_Abstract_Interfaces (Typ, Typ_Ifaces);

         AI := First_Elmt (Typ_Ifaces);
         while Present (AI) loop
            Num_Ifaces := Num_Ifaces + 1;
            Next_Elmt (AI);
         end loop;
      end if;

      --  Count ancestors to compute the inheritance depth. For private
      --  extensions, always go to the full view in order to compute the
      --  real inheritance depth.

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

      --  Calculate the number of primitives of the dispatch table and the
      --  size of the Type_Specific_Data record.

      --  Abstract interfaces don't need the dispatch table. In addition,
      --  compiling with restriction No_Dispatching_Calls we do not generate
      --  the dispatch table.

      Has_Dispatch_Table :=
        not Is_Interface (Typ)
          and then not Restriction_Active (No_Dispatching_Calls);

      if Has_Dispatch_Table then
         Nb_Prim := UI_To_Int (DT_Entry_Count (First_Tag_Component (Typ)));
      end if;

      --  Dispatch table and related entities are allocated statically

      Set_Ekind (DT, E_Variable);
      Set_Is_Statically_Allocated (DT);

      Set_Ekind (DT_Ptr, E_Variable);
      Set_Is_Statically_Allocated (DT_Ptr);

      if Num_Ifaces > 0 then
         Name_ITable := New_External_Name (Tname, 'I');
         ITable      := Make_Defining_Identifier (Loc, Name_ITable);

         Set_Ekind (ITable, E_Variable);
         Set_Is_Statically_Allocated (ITable);
      end if;

      Set_Ekind (SSD, E_Variable);
      Set_Is_Statically_Allocated (SSD);

      Set_Ekind (TSD, E_Variable);
      Set_Is_Statically_Allocated (TSD);

      Set_Ekind (Exname, E_Variable);
      Set_Is_Statically_Allocated (Exname);

      Set_Ekind (No_Reg, E_Variable);
      Set_Is_Statically_Allocated (No_Reg);

      --  Generate code to create the storage for the Dispatch_Table object:

      --   DT : Storage_Array (1 .. Size_Expr);
      --   for DT'Alignment use Address'Alignment

      --  Under No_Dispatching_Calls the size of the table is small just
      --  containing:
      --   1) the pointer to the TSD
      --   2) a dummy entry used as the Tag of the type (see a-tags.ads).

      if not Has_Dispatch_Table then
         Size_Expr_Node :=
           New_Reference_To (RTE (RE_DT_Min_Prologue_Size), Loc);

      --  If the object has no primitives we ensure that the table will
      --  have at least a dummy entry which will be used as the Tag.

      --   Size_Expr := DT_Prologue_Size + DT_Entry_Size

      elsif Nb_Prim = 0 then
         Size_Expr_Node :=
           Make_Op_Add (Loc,
             Left_Opnd  =>
               New_Reference_To (RTE (RE_DT_Prologue_Size), Loc),
             Right_Opnd =>
               New_Reference_To (RTE (RE_DT_Entry_Size), Loc));

      --  Common case. The dispatch table has space to save the pointers to
      --  all the predefined primitives, the C++ ABI header of the DT, and
      --  the pointers to the primitives of Typ. That is,

      --   Size_Expr := DT_Prologue_Size + nb_prim * DT_Entry_Size

      else
         Size_Expr_Node :=
           Make_Op_Add (Loc,
             Left_Opnd  =>
               New_Reference_To (RTE (RE_DT_Prologue_Size), Loc),
             Right_Opnd =>
               Make_Op_Multiply (Loc,
                 Left_Opnd  =>
                   New_Reference_To (RTE (RE_DT_Entry_Size), Loc),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc, Nb_Prim)));
      end if;

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => DT,
          Aliased_Present     => True,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To
                                (RTE (RE_Storage_Array), Loc),
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

      --    DT_Ptr : Tag := Tag!(DT'Address);

      --  According to the C++ ABI, the base of the vtable is located after a
      --  prologue containing Offset_To_Top, and Typeinfo_Ptr. Hence, we move
      --  down the pointer to the real base of the vtable

      if not Has_Dispatch_Table then
         Append_To (Result,
           Make_Object_Declaration (Loc,
             Defining_Identifier => DT_Ptr,
             Constant_Present    => True,
             Object_Definition   => New_Reference_To (Generalized_Tag, Loc),
             Expression          =>
               Unchecked_Convert_To (Generalized_Tag,
                 Make_Op_Add (Loc,
                   Left_Opnd =>
                     Unchecked_Convert_To (RTE (RE_Storage_Offset),
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Reference_To (DT, Loc),
                         Attribute_Name => Name_Address)),
                   Right_Opnd =>
                     New_Reference_To (RTE (RE_DT_Typeinfo_Ptr_Size), Loc)))));

      else
         Append_To (Result,
           Make_Object_Declaration (Loc,
             Defining_Identifier => DT_Ptr,
             Constant_Present    => True,
             Object_Definition   => New_Reference_To (Generalized_Tag, Loc),
             Expression          =>
               Unchecked_Convert_To (Generalized_Tag,
                 Make_Op_Add (Loc,
                   Left_Opnd =>
                     Unchecked_Convert_To (RTE (RE_Storage_Offset),
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Reference_To (DT, Loc),
                         Attribute_Name => Name_Address)),
                   Right_Opnd =>
                     New_Reference_To (RTE (RE_DT_Prologue_Size), Loc)))));
      end if;

      --  Save the tag in the Access_Disp_Table attribute

      if No (Access_Disp_Table (Typ)) then
         Set_Access_Disp_Table (Typ, New_Elmt_List);
      end if;

      Prepend_Elmt (DT_Ptr, Access_Disp_Table (Typ));

      --  Generate code to define the boolean that controls registration, in
      --  order to avoid multiple registrations for tagged types defined in
      --  multiple-called scopes.

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => No_Reg,
          Object_Definition   => New_Reference_To (Standard_Boolean, Loc),
          Expression          => New_Reference_To (Standard_True, Loc)));

      --  Generate:
      --    Set_Signature (DT_Ptr, Value);

      if Has_Dispatch_Table
        and then RTE_Available (RE_Set_Signature)
      then
         if Is_Interface (Typ) then
            Append_To (Elab_Code,
              Make_DT_Access_Action (Typ,
                Action => Set_Signature,
                Args   => New_List (
                  New_Reference_To (DT_Ptr, Loc),
                  New_Reference_To (RTE (RE_Abstract_Interface), Loc))));

         else
            Append_To (Elab_Code,
              Make_DT_Access_Action (Typ,
                Action => Set_Signature,
                Args   => New_List (
                  New_Reference_To (DT_Ptr, Loc),
                  New_Reference_To (RTE (RE_Primary_DT), Loc))));
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

      --  Calculate the value of the RC_Offset component. These are the
      --  valid valiues and their meaning:
      --   >0: For simple types with controlled components is
      --         type._record_controller'position
      --    0: For types with no controlled components
      --   -1: For complex types with controlled components where the position
      --       of the record controller is not statically computable but there
      --       are controlled components at this level. The _Controller field
      --       is available right after the _parent.
      --   -2: There are no controlled components at this level. We need to
      --       get the position from the parent.

      if Is_Interface (Typ)
        or else not Has_Controlled_Component (Typ)
      then
         RC_Offset_Node := Make_Integer_Literal (Loc, 0);

      elsif Etype (Typ) /= Typ
        and then Has_Discriminants (Etype (Typ))
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

      --  Set the pointer to the Interfaces_Table (if any). Otherwise the
      --  corresponding access component is set to null. The table of
      --  interfaces is required for AI-405

      if RTE_Record_Component_Available (RE_Ifaces_Table_Ptr) then
         if Num_Ifaces = 0 then
            Iface_Table_Node :=
              New_Reference_To (RTE (RE_Null_Address), Loc);

         --  Generate the Interface_Table object.

         else
            Append_To (Result,
              Make_Object_Declaration (Loc,
                Defining_Identifier => ITable,
                Aliased_Present     => True,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Reference_To
                      (RTE (RE_Interface_Data), Loc),
                    Constraint   => Make_Index_Or_Discriminant_Constraint (Loc,
                      Constraints => New_List (
                        Make_Integer_Literal (Loc,
                          Num_Ifaces))))));

            Iface_Table_Node :=
              Make_Attribute_Reference (Loc,
                Prefix         => New_Reference_To (ITable, Loc),
                Attribute_Name => Name_Address);
         end if;
      end if;

      --  Generate: Set_Remotely_Callable (DT_Ptr, Status); where Status is
      --  described in E.4 (18)

      Remotely_Callable :=
        Boolean_Literals
          (Is_Pure (Typ)
             or else Is_Shared_Passive (Typ)
             or else
               ((Is_Remote_Types (Typ)
                   or else Is_Remote_Call_Interface (Typ))
                and then Original_View_In_Visible_Part (Typ))
             or else not Comes_From_Source (Typ));

      --  Generate code to create the storage for the type specific data object
      --  with enough space to store the tags of the ancestors plus the tags
      --  of all the implemented interfaces (as described in a-tags.adb).

      --   TSD : Type_Specific_Data (I_Depth) :=
      --           (Idepth        => I_Depth,
      --            Access_Level  => Type_Access_Level (Typ),
      --            Expanded_Name => Cstring_Ptr!(Exname'Address))
      --            [ External_Tag  => Cstring_Ptr!(Exname'Address)) ]
      --            RC_Offset     => <<integer-value>>,
      --            Remotely_Callable => <<boolean-value>>
      --            [ Ifaces_Table_Ptr => <<access-value>> ]
      --            others => <>);
      --   for TSD'Alignment use Address'Alignment

      TSD_Aggr_List := New_List (
        Make_Component_Association (Loc,
          Choices => New_List (
            New_Occurrence_Of (RTE_Record_Component (RE_Idepth), Loc)),
          Expression => Make_Integer_Literal (Loc, I_Depth)),

        Make_Component_Association (Loc,
          Choices => New_List (
            New_Occurrence_Of (RTE_Record_Component (RE_Access_Level), Loc)),
          Expression => Make_Integer_Literal (Loc, Type_Access_Level (Typ))),

        Make_Component_Association (Loc,
          Choices => New_List (
            New_Occurrence_Of
              (RTE_Record_Component (RE_Expanded_Name), Loc)),
          Expression =>
            Unchecked_Convert_To (RTE (RE_Cstring_Ptr),
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (Exname, Loc),
                Attribute_Name => Name_Address))));

      if not Has_External_Tag_Rep_Clause (Typ) then

         --  Should be the external name not the qualified name???

         Append_To (TSD_Aggr_List,
           Make_Component_Association (Loc,
             Choices => New_List (
               New_Occurrence_Of
                 (RTE_Record_Component (RE_External_Tag), Loc)),
             Expression =>
               Unchecked_Convert_To (RTE (RE_Cstring_Ptr),
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Exname, Loc),
                   Attribute_Name => Name_Address))));
      end if;

      Append_List_To (TSD_Aggr_List, New_List (
        Make_Component_Association (Loc,
          Choices => New_List (
            New_Occurrence_Of (RTE_Record_Component (RE_RC_Offset), Loc)),
          Expression => RC_Offset_Node),

        Make_Component_Association (Loc,
          Choices => New_List (
            New_Occurrence_Of
             (RTE_Record_Component (RE_Remotely_Callable), Loc)),
          Expression => New_Occurrence_Of (Remotely_Callable, Loc))));

      if RTE_Record_Component_Available (RE_Ifaces_Table_Ptr) then
         Append_To (TSD_Aggr_List,
           Make_Component_Association (Loc,
             Choices => New_List (
               New_Occurrence_Of
                (RTE_Record_Component (RE_Ifaces_Table_Ptr), Loc)),
             Expression => Iface_Table_Node));
      end if;

      Append_To (TSD_Aggr_List,
        Make_Component_Association (Loc,
          Choices     => New_List (Make_Others_Choice (Loc)),
          Expression  => Empty,
          Box_Present => True));

      --  Save the expanded name in the dispatch table

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => TSD,
          Aliased_Present     => True,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (
                RTE (RE_Type_Specific_Data), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => New_List (
                    Make_Integer_Literal (Loc, I_Depth)))),
          Expression => Make_Aggregate (Loc,
            Component_Associations => TSD_Aggr_List)));

      Append_To (Result,
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Reference_To (TSD, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Generate code to put the Address of the TSD in the dispatch table

      Append_To (Elab_Code,
        Build_Set_TSD (Loc,
          Tag_Node => New_Reference_To (DT_Ptr, Loc),
          Value_Node =>
            Make_Attribute_Reference (Loc,
              Prefix          => New_Reference_To (TSD, Loc),
              Attribute_Name  => Name_Address)));

      --  Generate extra code required for synchronized interfaces

      if RTE_Available (RE_Set_Tagged_Kind) then
         if Ada_Version >= Ada_05
           and then not Is_Interface  (Typ)
           and then not Is_Abstract_Type   (Typ)
           and then not Is_Controlled (Typ)
           and then not Restriction_Active (No_Dispatching_Calls)
         then
            --  Generate:
            --    Set_Type_Kind (T'Tag, Type_Kind (Typ));

            Append_To (Elab_Code,
              Make_DT_Access_Action (Typ,
                Action => Set_Tagged_Kind,
                Args   => New_List (
                  New_Reference_To (DT_Ptr, Loc),               -- DTptr
                  Tagged_Kind (Typ))));                         -- Value

            --  Generate the Select Specific Data table for synchronized
            --  types that implement a synchronized interface. The size
            --  of the table is constrained by the number of non-predefined
            --  primitive operations.

            if Has_Dispatch_Table
              and then Is_Concurrent_Record_Type (Typ)
              and then Has_Abstract_Interfaces (Typ)
            then
               --  No need to generate this code if Nb_Prim = 0 ???

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

               --  Set the pointer to the Select Specific Data table in the TSD

               Append_To (Elab_Code,
                 Make_DT_Access_Action (Typ,
                   Action => Set_SSD,
                   Args   => New_List (
                     New_Reference_To (DT_Ptr, Loc),            -- DTptr
                     Make_Attribute_Reference (Loc,             -- Value
                       Prefix         => New_Reference_To (SSD, Loc),
                       Attribute_Name => Name_Address))));
            end if;
         end if;
      end if;

      --  If the ancestor is a CPP_Class type we inherit the dispatch tables
      --  in the init proc, and we don't need to fill them in here.

      if Is_CPP_Class (Etype (Typ)) then
         null;

         --  Otherwise we fill in the dispatch tables here

      else
         if Typ = Etype (Typ)
           or else Is_CPP_Class (Etype (Typ))
           or else Is_Interface (Typ)
         then
            Null_Parent_Tag := True;

            Old_Tag1 :=
              Unchecked_Convert_To (Generalized_Tag,
                Make_Integer_Literal (Loc, 0));
            Old_Tag2 :=
              Unchecked_Convert_To (Generalized_Tag,
                Make_Integer_Literal (Loc, 0));

         else
            Old_Tag1 :=
              New_Reference_To
                (Node (First_Elmt (Access_Disp_Table (Etype (Typ)))), Loc);
            Old_Tag2 :=
              New_Reference_To
                (Node (First_Elmt (Access_Disp_Table (Etype (Typ)))), Loc);
         end if;

         if Typ /= Etype (Typ)
           and then not Is_Interface (Typ)
           and then not Restriction_Active (No_Dispatching_Calls)
         then
            --  Inherit the dispatch table

            if not Is_Interface (Etype (Typ)) then
               if Restriction_Active (No_Dispatching_Calls) then
                  null;

               else
                  if not Null_Parent_Tag then
                     declare
                        Nb_Prims : constant Int :=
                                     UI_To_Int (DT_Entry_Count
                                       (First_Tag_Component (Etype (Typ))));
                     begin
                        Append_To (Elab_Code,
                          Build_Inherit_Predefined_Prims (Loc,
                            Old_Tag_Node => Old_Tag1,
                            New_Tag_Node =>
                              New_Reference_To (DT_Ptr, Loc)));

                        if Nb_Prims /= 0 then
                           Append_To (Elab_Code,
                             Build_Inherit_Prims (Loc,
                               Old_Tag_Node => Old_Tag2,
                               New_Tag_Node => New_Reference_To (DT_Ptr, Loc),
                               Num_Prims    => Nb_Prims));
                        end if;
                     end;
                  end if;
               end if;
            end if;

            --  Inherit the secondary dispatch tables of the ancestor

            if not Restriction_Active (No_Dispatching_Calls)
              and then not Is_CPP_Class (Etype (Typ))
            then
               declare
                  Sec_DT_Ancestor : Elmt_Id :=
                                      Next_Elmt
                                        (First_Elmt
                                           (Access_Disp_Table (Etype (Typ))));
                  Sec_DT_Typ      : Elmt_Id :=
                                      Next_Elmt
                                        (First_Elmt
                                           (Access_Disp_Table (Typ)));

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

                     if Present (Abstract_Interfaces (Typ))
                       and then not Is_Empty_Elmt_List
                                      (Abstract_Interfaces (Typ))
                     then
                        Iface := First_Elmt (Abstract_Interfaces (Typ));
                        E     := First_Entity (Typ);
                        while Present (E)
                          and then Present (Node (Sec_DT_Ancestor))
                        loop
                           if Is_Tag (E) and then Chars (E) /= Name_uTag then
                              if not Is_Interface (Etype (Typ)) then

                                 --  Inherit the dispatch table

                                 declare
                                    Num_Prims : constant Int :=
                                                UI_To_Int (DT_Entry_Count (E));
                                 begin
                                    Append_To (Elab_Code,
                                      Build_Inherit_Predefined_Prims (Loc,
                                        Old_Tag_Node =>
                                          Unchecked_Convert_To (RTE (RE_Tag),
                                             New_Reference_To
                                               (Node (Sec_DT_Ancestor), Loc)),
                                        New_Tag_Node =>
                                          Unchecked_Convert_To (RTE (RE_Tag),
                                            New_Reference_To
                                              (Node (Sec_DT_Typ), Loc))));

                                    if Num_Prims /= 0 then
                                       Append_To (Elab_Code,
                                         Build_Inherit_Prims (Loc,
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
                                           Num_Prims => Num_Prims));
                                    end if;
                                 end;
                              end if;

                              Next_Elmt (Sec_DT_Ancestor);
                              Next_Elmt (Sec_DT_Typ);
                              Next_Elmt (Iface);
                           end if;

                           Next_Entity (E);
                        end loop;
                     end if;
                  end Copy_Secondary_DTs;

               begin
                  if Present (Node (Sec_DT_Ancestor)) then

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

         --  Generate:
         --    Inherit_TSD (parent'tag, DT_Ptr);

         if not Is_Interface (Typ) then
            if Typ = Etype (Typ)
              or else Is_CPP_Class (Etype (Typ))
            then
               --  New_TSD (DT_Ptr);

               Append_List_To (Elab_Code,
                 Build_New_TSD (Loc,
                   New_Tag_Node => New_Reference_To (DT_Ptr, Loc)));
            else
               --  Inherit_TSD (parent'tag, DT_Ptr);

               Append_To (Elab_Code,
                 Build_Inherit_TSD (Loc,
                   Old_Tag_Node =>
                     New_Reference_To
                       (Node (First_Elmt (Access_Disp_Table (Etype (Typ)))),
                        Loc),
                   New_Tag_Node      => New_Reference_To (DT_Ptr, Loc),
                   I_Depth           => I_Depth,
                   Parent_Num_Ifaces => Parent_Num_Ifaces));
            end if;
         end if;
      end if;

      if not Is_Interface (Typ)
        and then RTE_Available (RE_Set_Offset_To_Top)
      then
         --  Generate:
         --    Set_Offset_To_Top (0, DT_Ptr, True, 0, null);

         Append_To (Elab_Code,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Set_Offset_To_Top), Loc),
             Parameter_Associations => New_List (
               New_Reference_To (RTE (RE_Null_Address), Loc),
               New_Reference_To (DT_Ptr, Loc),
               New_Occurrence_Of (Standard_True, Loc),
               Make_Integer_Literal (Loc, Uint_0),
               New_Reference_To (RTE (RE_Null_Address), Loc))));
      end if;

      --  Generate code to register the Tag in the External_Tag hash table for
      --  the pure Ada type only.

      --        Register_Tag (Dt_Ptr);

      --  Skip this if routine not available, or in No_Run_Time mode or Typ is
      --  an abstract interface type (because the table to register it is not
      --  available in the abstract type but in types implementing this
      --  interface)

      if not Has_External_Tag_Rep_Clause (Typ)
        and then not No_Run_Time_Mode
        and then RTE_Available (RE_Register_Tag)
        and then Is_RTE (RTE (RE_Tag), RE_Tag)
        and then not Is_Interface (Typ)
      then
         Append_To (Elab_Code,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Register_Tag), Loc),
             Parameter_Associations =>
               New_List (New_Reference_To (DT_Ptr, Loc))));
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

      --  Ada 2005 (AI-251): Register the tag of the interfaces into the table
      --  of interfaces.

      if Num_Ifaces > 0 then
         declare
            Position : Nat;

         begin
            --  If the parent is an interface we must generate code to register
            --  all its interfaces; otherwise this code is not needed because
            --  Inherit_TSD has already inherited such interfaces.

            if Is_Concurrent_Record_Type (Typ)
              or else (Etype (Typ) /= Typ and then Is_Interface (Etype (Typ)))
            then
               Position := 1;

               AI := First_Elmt (Ancestor_Ifaces);
               while Present (AI) loop
                  --  Generate:
                  --    Register_Interface (DT_Ptr, Interface'Tag);

                  Append_To (Result,
                    Make_DT_Access_Action (Typ,
                      Action => Register_Interface_Tag,
                      Args   => New_List (
                        Node1 => New_Reference_To (DT_Ptr, Loc),
                        Node2 => New_Reference_To
                                   (Node
                                    (First_Elmt
                                     (Access_Disp_Table (Node (AI)))),
                                    Loc),
                        Node3 => Make_Integer_Literal (Loc, Position))));

                  Position := Position + 1;
                  Next_Elmt (AI);
               end loop;
            end if;

            --  Register the interfaces that are not implemented by the
            --  ancestor

            AI := First_Elmt (Typ_Ifaces);

            --  Skip the interfaces implemented by the ancestor

            for Count in 1 .. Parent_Num_Ifaces loop
               Next_Elmt (AI);
            end loop;

            --  Register the additional interfaces

            Position := Parent_Num_Ifaces + 1;
            while Present (AI) loop

               --  Generate:
               --    Register_Interface (DT_Ptr, Interface'Tag);

               if not Is_Interface (Typ)
                 or else Typ /= Node (AI)
               then
                  Append_To (Result,
                    Make_DT_Access_Action (Typ,
                      Action => Register_Interface_Tag,
                      Args   => New_List (
                        Node1 => New_Reference_To (DT_Ptr, Loc),
                        Node2 => New_Reference_To
                                   (Node
                                    (First_Elmt
                                     (Access_Disp_Table (Node (AI)))),
                                    Loc),
                        Node3 => Make_Integer_Literal (Loc, Position))));

                  Position := Position + 1;
               end if;

               Next_Elmt (AI);
            end loop;

            pragma Assert (Position = Num_Ifaces + 1);
         end;
      end if;

      return Result;
   end Make_DT;

   ---------------------------
   -- Make_DT_Access_Action --
   ---------------------------

   function Make_DT_Access_Action
     (Typ    : Entity_Id;
      Action : DT_Access_Action;
      Args   : List_Id) return Node_Id
   is
      Action_Name : constant Entity_Id := RTE (Ada_Actions (Action));
      Loc         : Source_Ptr;

   begin
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

   -----------------------
   -- Make_Secondary_DT --
   -----------------------

   procedure Make_Secondary_DT
     (Typ             : Entity_Id;
      Ancestor_Typ    : Entity_Id;
      Suffix_Index    : Nat;
      Iface           : Entity_Id;
      AI_Tag          : Entity_Id;
      Acc_Disp_Tables : in out Elist_Id;
      Result          : out List_Id)
   is
      Loc             : constant Source_Ptr := Sloc (AI_Tag);
      Generalized_Tag : constant Entity_Id := RTE (RE_Interface_Tag);
      Name_DT         : constant Name_Id := New_Internal_Name ('T');
      Empty_DT        : Boolean := False;
      Iface_DT        : Node_Id;
      Iface_DT_Ptr    : Node_Id;
      Name_DT_Ptr     : Name_Id;
      Nb_Prim         : Nat;
      OSD             : Entity_Id;
      Size_Expr_Node  : Node_Id;
      Tname           : Name_Id;

   begin
      Result := New_List;

      --  Generate a unique external name associated with the secondary
      --  dispatch table. This external name will be used to declare an
      --  access to this secondary dispatch table, value that will be used
      --  for the elaboration of Typ's objects and also for the elaboration
      --  of objects of any derivation of Typ that do not override any
      --  primitive operation of Typ.

      Get_Secondary_DT_External_Name (Typ, Ancestor_Typ, Suffix_Index);

      Tname        := Name_Find;
      Name_DT_Ptr  := New_External_Name (Tname, "P");
      Iface_DT     := Make_Defining_Identifier (Loc, Name_DT);
      Iface_DT_Ptr := Make_Defining_Identifier (Loc, Name_DT_Ptr);

      --  Dispatch table and related entities are allocated statically

      Set_Ekind (Iface_DT, E_Variable);
      Set_Is_Statically_Allocated (Iface_DT);

      Set_Ekind (Iface_DT_Ptr, E_Variable);
      Set_Is_Statically_Allocated (Iface_DT_Ptr);

      --  Generate code to create the storage for the Dispatch_Table object.
      --  If the number of primitives of Typ is 0 we reserve a dummy single
      --  entry for its DT because at run-time the pointer to this dummy entry
      --  will be used as the tag.

      Nb_Prim := UI_To_Int (DT_Entry_Count (AI_Tag));

      if Nb_Prim = 0 then
         Empty_DT := True;
         Nb_Prim  := 1;
      end if;

      --    DT : Storage_Array (1..DT_Prologue_Size+nb_prim*DT_Entry_Size);
      --    for DT'Alignment use Address'Alignment

      Size_Expr_Node :=
        Make_Op_Add (Loc,
          Left_Opnd  =>
            New_Reference_To (RTE (RE_DT_Prologue_Size), Loc),
          Right_Opnd =>
            Make_Op_Multiply (Loc,
              Left_Opnd  =>
                New_Reference_To (RTE (RE_DT_Entry_Size), Loc),
              Right_Opnd =>
                Make_Integer_Literal (Loc, Nb_Prim)));

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Iface_DT,
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
          Name       => New_Reference_To (Iface_DT, Loc),
          Chars      => Name_Alignment,
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (RTE (RE_Integer_Address), Loc),
              Attribute_Name => Name_Alignment)));

      --  Generate code to create the pointer to the dispatch table

      --    Iface_DT_Ptr : Tag := Tag!(DT'Address);

      --  According to the C++ ABI, the base of the vtable is located
      --  after the following prologue: Offset_To_Top, and Typeinfo_Ptr.
      --  Hence, move the pointer down to the real base of the vtable.

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Iface_DT_Ptr,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Generalized_Tag, Loc),
          Expression          =>
            Unchecked_Convert_To (Generalized_Tag,
              Make_Op_Add (Loc,
                Left_Opnd =>
                  Unchecked_Convert_To (RTE (RE_Storage_Offset),
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Reference_To (Iface_DT, Loc),
                      Attribute_Name => Name_Address)),
                Right_Opnd =>
                  New_Reference_To (RTE (RE_DT_Prologue_Size), Loc)))));

      --  Note: Offset_To_Top will be initialized by the init subprogram

      --  Set Access_Disp_Table field to be the dispatch table pointer

      if not (Present (Acc_Disp_Tables)) then
         Acc_Disp_Tables := New_Elmt_List;
      end if;

      Append_Elmt (Iface_DT_Ptr, Acc_Disp_Tables);

      --  Step 1: Generate an Object Specific Data (OSD) table

      OSD := Make_Defining_Identifier (Loc, New_Internal_Name ('I'));

      --  Nothing to do if configurable run time does not support the
      --  Object_Specific_Data entity.

      if not RTE_Available (RE_Object_Specific_Data) then
         Error_Msg_CRT ("abstract interface types", Typ);
         return;
      end if;

      --  Generate:
      --    OSD : Ada.Tags.Object_Specific_Data (Nb_Prims);
      --  where the constraint is used to allocate space for the
      --  non-predefined primitive operations only.

      Append_To (Result,
        Make_Object_Declaration (Loc,
          Defining_Identifier => OSD,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (
                RTE (RE_Object_Specific_Data), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => New_List (
                    Make_Integer_Literal (Loc, Nb_Prim))))));

      Append_To (Result,
        Make_DT_Access_Action (Typ,
          Action => Set_Signature,
          Args   => New_List (
            Unchecked_Convert_To (RTE (RE_Tag),
              New_Reference_To (Iface_DT_Ptr, Loc)),
            New_Reference_To (RTE (RE_Secondary_DT), Loc))));

      --  Generate:
      --    Ada.Tags.Set_OSD (Iface_DT_Ptr, OSD);

      Append_To (Result,
        Make_DT_Access_Action (Typ,
          Action => Set_OSD,
          Args   => New_List (
            Unchecked_Convert_To (RTE (RE_Tag),
              New_Reference_To (Iface_DT_Ptr, Loc)),
            Make_Attribute_Reference (Loc,
              Prefix         => New_Reference_To (OSD, Loc),
              Attribute_Name => Name_Address))));

      if Ada_Version >= Ada_05
        and then not Is_Interface (Typ)
        and then not Is_Abstract_Type (Typ)
        and then not Is_Controlled (Typ)
        and then RTE_Available (RE_Set_Tagged_Kind)
        and then not Restriction_Active (No_Dispatching_Calls)
      then
         --  Generate:
         --    Set_Tagged_Kind (Iface'Tag, Tagged_Kind (Iface));

         Append_To (Result,
           Make_DT_Access_Action (Typ,
             Action => Set_Tagged_Kind,
             Args   => New_List (
               Unchecked_Convert_To (RTE (RE_Tag),              -- DTptr
                 New_Reference_To (Iface_DT_Ptr, Loc)),
               Tagged_Kind (Typ))));                            -- Value

         if not Empty_DT
           and then Is_Concurrent_Record_Type (Typ)
           and then Has_Abstract_Interfaces (Typ)
         then
            declare
               Prim       : Entity_Id;
               Prim_Alias : Entity_Id;
               Prim_Elmt  : Elmt_Id;

            begin
               --  Step 2: Populate the OSD table

               Prim_Alias := Empty;
               Prim_Elmt  := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if Present (Abstract_Interface_Alias (Prim))
                    and then Find_Dispatching_Type
                               (Abstract_Interface_Alias (Prim)) = Iface
                  then
                     Prim_Alias := Abstract_Interface_Alias (Prim);

                     --  Generate:
                     --    Ada.Tags.Set_Offset_Index (Tag (Iface_DT_Ptr),
                     --      Secondary_DT_Pos, Primary_DT_pos);

                     Append_To (Result,
                       Make_DT_Access_Action (Iface,
                         Action => Set_Offset_Index,
                         Args   => New_List (
                           Unchecked_Convert_To (RTE (RE_Tag),
                             New_Reference_To (Iface_DT_Ptr, Loc)),
                           Make_Integer_Literal (Loc,
                             DT_Position (Prim_Alias)),
                           Make_Integer_Literal (Loc,
                             DT_Position (Alias (Prim))))));
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
            end;
         end if;
      end if;
   end Make_Secondary_DT;

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

            if Present (Abstract_Interface_Alias (Prim))
              and then not Examined (UI_To_Int (DT_Position (Alias (Prim))))
            then
               Prim_Pos := DT_Position (Alias (Prim));
               pragma Assert (UI_To_Int (Prim_Pos) <= Nb_Prim);
               Examined (UI_To_Int (Prim_Pos)) := True;

               --  Set the primitive operation kind regardless of subprogram
               --  type. Generate:
               --    Ada.Tags.Set_Prim_Op_Kind (DT_Ptr, <position>, <kind>);

               Append_To (Assignments,
                 Make_DT_Access_Action (Typ,
                   Action => Set_Prim_Op_Kind,
                   Args => New_List (
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
                    Make_DT_Access_Action (Typ,
                      Action => Set_Entry_Index,
                      Args => New_List (
                                New_Reference_To (DT_Ptr, Loc),
                                Make_Integer_Literal (Loc, Prim_Pos),
                                Make_Integer_Literal (Loc,
                                  Find_Entry_Index
                                    (Wrapped_Entity (Prim_Als))))));
               end if;
            end if;

            Next_Elmt (Prim_Elmt);
         end loop;
      end;

      return Assignments;
   end Make_Select_Specific_Data_Table;

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

   -------------------------
   -- Set_All_DT_Position --
   -------------------------

   procedure Set_All_DT_Position (Typ : Entity_Id) is

      procedure Validate_Position (Prim : Entity_Id);
      --  Check that the position assignated to Prim is completely safe
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

            elsif Present (Abstract_Interface_Alias (Op)) then
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
      --  Set the DT_Position for each primitive operation. Perform some
      --  sanity checks to avoid to build completely inconsistant dispatch
      --  tables.

      --  First stage: Set the DTC entity of all the primitive operations
      --  This is required to properly read the DT_Position attribute in
      --  the latter stages.

      Prim_Elmt  := First_Prim;
      Count_Prim := 0;
      while Present (Prim_Elmt) loop
         Prim := Node (Prim_Elmt);

         --  Predefined primitives have a separate dispatch table

         if not (Is_Predefined_Dispatching_Operation (Prim)
                   or else Is_Predefined_Dispatching_Alias (Prim))
         then
            Count_Prim := Count_Prim + 1;
         end if;

         --  Ada 2005 (AI-251)

         if Present (Abstract_Interface_Alias (Prim))
           and then Is_Interface
                      (Find_Dispatching_Type
                        (Abstract_Interface_Alias (Prim)))
         then
            Set_DTC_Entity (Prim,
               Find_Interface_Tag
                 (T => Typ,
                  Iface => Find_Dispatching_Type
                            (Abstract_Interface_Alias (Prim))));
         else
            Set_DTC_Entity (Prim, The_Tag);
         end if;

         --  Clear any previous value of the DT_Position attribute. In this
         --  way we ensure that the final position of all the primitives is
         --  stablished by the following stages of this algorithm.

         Set_DT_Position (Prim, No_Uint);

         Next_Elmt (Prim_Elmt);
      end loop;

      declare
         Fixed_Prim : array (Int range 0 .. Count_Prim) of Boolean
                        := (others => False);
         E : Entity_Id;

         procedure Set_Fixed_Prim (Pos : Nat);
         --  Sets to true an element of the Fixed_Prim table to indicate
         --  that this entry of the dispatch table of Typ is occupied.

         --------------------
         -- Set_Fixed_Prim --
         --------------------

         procedure Set_Fixed_Prim (Pos : Nat) is
         begin
            pragma Assert (Pos >= 0 and then Pos <= Count_Prim);
            Fixed_Prim (Pos) := True;
         exception
            when Constraint_Error =>
               raise Program_Error;
         end Set_Fixed_Prim;

      begin
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

            elsif Present (Abstract_Interface_Alias (Prim))
              and then Is_Parent
                         (Find_Dispatching_Type
                           (Abstract_Interface_Alias (Prim)),
                          Typ)
            then
               pragma Assert (DT_Position (Prim) = No_Uint
                 and then Present (DTC_Entity
                                    (Abstract_Interface_Alias (Prim))));

               E := Abstract_Interface_Alias (Prim);
               Set_DT_Position (Prim, DT_Position (E));

               pragma Assert
                 (DT_Position (Alias (Prim)) = No_Uint
                    or else DT_Position (Alias (Prim)) = DT_Position (E));
               Set_DT_Position (Alias (Prim), DT_Position (E));
               Set_Fixed_Prim (UI_To_Int (DT_Position (Prim)));

            --  Overriding primitives must use the same entry as the
            --  overriden primitive

            elsif not Present (Abstract_Interface_Alias (Prim))
              and then Present (Alias (Prim))
              and then Find_Dispatching_Type (Alias (Prim)) /= Typ
              and then Is_Parent
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

            elsif Present (Abstract_Interface_Alias (Prim)) then
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
           and then Present (Abstract_Interface_Alias (Prim))
         then
            pragma Assert (Present (Alias (Prim))
              and then Find_Dispatching_Type (Alias (Prim)) = Typ);

            --  Check if this entry will be placed in the primary DT

            if Is_Parent (Find_Dispatching_Type
                           (Abstract_Interface_Alias (Prim)),
                          Typ)
            then
               pragma Assert (DT_Position (Alias (Prim)) /= No_Uint);
               Set_DT_Position (Prim, DT_Position (Alias (Prim)));

            --  Otherwise it will be placed in the secondary DT

            else
               pragma Assert
                 (DT_Position (Abstract_Interface_Alias (Prim)) /= No_Uint);
               Set_DT_Position (Prim,
                 DT_Position (Abstract_Interface_Alias (Prim)));
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
         --  in the dispatch table

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

         --  Ensure that the asignated position to non-predefined
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

         --  Ada 2005 (AI-251): Hidden entities associated with abstract
         --  interface primitives are not taken into account because the
         --  check is done with the aliased primitive.

         if Is_Abstract_Type (Typ)
           and then Is_Abstract_Subprogram (Prim)
           and then Present (Alias (Prim))
           and then not Present (Abstract_Interface_Alias (Prim))
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
                  " must be overridden ('R'M 3.9.3(10))",
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

      --  The derived type must have at least as many components as its
      --  parent (for root types, the Etype points back to itself
      --  and the test should not fail)

      --  This test fails compiling the partial view of a tagged type
      --  derived from an interface which defines the overriding subprogram
      --  in the private part. This needs further investigation???

      if not Has_Private_Declaration (Typ) then
         pragma Assert (
           DT_Entry_Count (The_Tag) >=
           DT_Entry_Count (First_Tag_Component (Parent_Typ)));
         null;
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
         Set_Is_Abstract_Type (Typ);
      end if;
   end Set_Default_Constructor;

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

      if not (Typ in First_Node_Id .. Last_Node_Id)
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

            if Present (Abstract_Interface_Alias (Prim)) then
               Write_Str  (", AI_Alias of ");
               Write_Name (Chars (Scope (DTC_Entity
                                          (Abstract_Interface_Alias (Prim)))));
               Write_Char (':');
               Write_Int  (Int (Abstract_Interface_Alias (Prim)));
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

         Write_Eol;

         Next_Elmt (Elmt);
      end loop;
   end Write_DT;

end Exp_Disp;
