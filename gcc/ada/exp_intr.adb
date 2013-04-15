------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I N T R                              --
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

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Atag; use Exp_Atag;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch11; use Exp_Ch11;
with Exp_Code; use Exp_Code;
with Exp_Fixd; use Exp_Fixd;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Intr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Binary_Operator_Call (N : Node_Id);
   --  Expand a call to an intrinsic arithmetic operator when the operand
   --  types or sizes are not identical.

   procedure Expand_Is_Negative (N : Node_Id);
   --  Expand a call to the intrinsic Is_Negative function

   procedure Expand_Dispatching_Constructor_Call (N : Node_Id);
   --  Expand a call to an instantiation of Generic_Dispatching_Constructor
   --  into a dispatching call to the actual subprogram associated with the
   --  Constructor formal subprogram, passing it the Parameters actual of
   --  the call to the instantiation and dispatching based on call's Tag
   --  parameter.

   procedure Expand_Exception_Call (N : Node_Id; Ent : RE_Id);
   --  Expand a call to Exception_Information/Message/Name. The first
   --  parameter, N, is the node for the function call, and Ent is the
   --  entity for the corresponding routine in the Ada.Exceptions package.

   procedure Expand_Import_Call (N : Node_Id);
   --  Expand a call to Import_Address/Longest_Integer/Value. The parameter
   --  N is the node for the function call.

   procedure Expand_Shift (N : Node_Id; E : Entity_Id; K : Node_Kind);
   --  Expand an intrinsic shift operation, N and E are from the call to
   --  Expand_Intrinsic_Call (call node and subprogram spec entity) and
   --  K is the kind for the shift node

   procedure Expand_Unc_Conversion (N : Node_Id; E : Entity_Id);
   --  Expand a call to an instantiation of Unchecked_Conversion into a node
   --  N_Unchecked_Type_Conversion.

   procedure Expand_Unc_Deallocation (N : Node_Id);
   --  Expand a call to an instantiation of Unchecked_Deallocation into a node
   --  N_Free_Statement and appropriate context.

   procedure Expand_To_Address (N : Node_Id);
   procedure Expand_To_Pointer (N : Node_Id);
   --  Expand a call to corresponding function, declared in an instance of
   --  System.Address_To_Access_Conversions.

   procedure Expand_Source_Info (N : Node_Id; Nam : Name_Id);
   --  Rewrite the node by the appropriate string or positive constant.
   --  Nam can be one of the following:
   --    Name_File             - expand string that is the name of source file
   --    Name_Line             - expand integer line number
   --    Name_Source_Location  - expand string of form file:line
   --    Name_Enclosing_Entity - expand string  with name of enclosing entity

   ---------------------------------
   -- Expand_Binary_Operator_Call --
   ---------------------------------

   procedure Expand_Binary_Operator_Call (N : Node_Id) is
      T1  : constant Entity_Id := Underlying_Type (Etype (Left_Opnd  (N)));
      T2  : constant Entity_Id := Underlying_Type (Etype (Right_Opnd (N)));
      TR  : constant Entity_Id := Etype (N);
      T3  : Entity_Id;
      Res : Node_Id;

      Siz : constant Uint := UI_Max (RM_Size (T1), RM_Size (T2));
      --  Maximum of operand sizes

   begin
      --  Nothing to do if the operands have the same modular type

      if Base_Type (T1) = Base_Type (T2)
        and then Is_Modular_Integer_Type (T1)
      then
         return;
      end if;

      --  Use Unsigned_32 for sizes of 32 or below, else Unsigned_64

      if Siz > 32 then
         T3 := RTE (RE_Unsigned_64);
      else
         T3 := RTE (RE_Unsigned_32);
      end if;

      --  Copy operator node, and reset type and entity fields, for
      --  subsequent reanalysis.

      Res := New_Copy (N);
      Set_Etype (Res, T3);

      case Nkind (N) is
         when N_Op_And =>
            Set_Entity (Res, Standard_Op_And);
         when N_Op_Or =>
            Set_Entity (Res, Standard_Op_Or);
         when N_Op_Xor =>
            Set_Entity (Res, Standard_Op_Xor);
         when others =>
            raise Program_Error;
      end case;

      --  Convert operands to large enough intermediate type

      Set_Left_Opnd (Res,
        Unchecked_Convert_To (T3, Relocate_Node (Left_Opnd (N))));
      Set_Right_Opnd (Res,
        Unchecked_Convert_To (T3, Relocate_Node (Right_Opnd (N))));

      --  Analyze and resolve result formed by conversion to target type

      Rewrite (N, Unchecked_Convert_To (TR, Res));
      Analyze_And_Resolve (N, TR);
   end Expand_Binary_Operator_Call;

   -----------------------------------------
   -- Expand_Dispatching_Constructor_Call --
   -----------------------------------------

   --  Transform a call to an instantiation of Generic_Dispatching_Constructor
   --  of the form:

   --     GDC_Instance (The_Tag, Parameters'Access)

   --  to a class-wide conversion of a dispatching call to the actual
   --  associated with the formal subprogram Construct, designating The_Tag
   --  as the controlling tag of the call:

   --     T'Class (Construct'Actual (Params)) -- Controlling tag is The_Tag

   --  which will eventually be expanded to the following:

   --     T'Class (The_Tag.all (Construct'Actual'Index).all (Params))

   --  A class-wide membership test is also generated, preceding the call, to
   --  ensure that the controlling tag denotes a type in T'Class.

   procedure Expand_Dispatching_Constructor_Call (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Tag_Arg    : constant Node_Id    := First_Actual (N);
      Param_Arg  : constant Node_Id    := Next_Actual (Tag_Arg);
      Subp_Decl  : constant Node_Id    := Parent (Parent (Entity (Name (N))));
      Inst_Pkg   : constant Node_Id    := Parent (Subp_Decl);
      Act_Rename : Node_Id;
      Act_Constr : Entity_Id;
      Iface_Tag  : Node_Id := Empty;
      Cnstr_Call : Node_Id;
      Result_Typ : Entity_Id;

   begin
      --  Remove side effects from tag argument early, before rewriting
      --  the dispatching constructor call, as Remove_Side_Effects relies
      --  on Tag_Arg's Parent link properly attached to the tree (once the
      --  call is rewritten, the Parent is inconsistent as it points to the
      --  rewritten node, which is not the syntactic parent of the Tag_Arg
      --  anymore).

      Remove_Side_Effects (Tag_Arg);

      --  The subprogram is the third actual in the instantiation, and is
      --  retrieved from the corresponding renaming declaration. However,
      --  freeze nodes may appear before, so we retrieve the declaration
      --  with an explicit loop.

      Act_Rename := First (Visible_Declarations (Inst_Pkg));
      while Nkind (Act_Rename) /= N_Subprogram_Renaming_Declaration loop
         Next (Act_Rename);
      end loop;

      Act_Constr := Entity (Name (Act_Rename));
      Result_Typ := Class_Wide_Type (Etype (Act_Constr));

      if Is_Interface (Etype (Act_Constr)) then

         --  If the result type is not known to be a parent of Tag_Arg then we
         --  need to locate the tag of the secondary dispatch table.

         if not Is_Ancestor (Etype (Result_Typ), Etype (Tag_Arg),
                             Use_Full_View => True)
           and then Tagged_Type_Expansion
         then
            --  Obtain the reference to the Ada.Tags service before generating
            --  the Object_Declaration node to ensure that if this service is
            --  not available in the runtime then we generate a clear error.

            declare
               Fname : constant Node_Id :=
                         New_Reference_To (RTE (RE_Secondary_Tag), Loc);

            begin
               pragma Assert (not Is_Interface (Etype (Tag_Arg)));

               Iface_Tag :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Make_Temporary (Loc, 'V'),
                   Object_Definition   =>
                     New_Reference_To (RTE (RE_Tag), Loc),
                   Expression          =>
                     Make_Function_Call (Loc,
                       Name                   => Fname,
                       Parameter_Associations => New_List (
                         Relocate_Node (Tag_Arg),
                         New_Reference_To
                           (Node (First_Elmt (Access_Disp_Table
                                               (Etype (Etype (Act_Constr))))),
                            Loc))));
               Insert_Action (N, Iface_Tag);
            end;
         end if;
      end if;

      --  Create the call to the actual Constructor function

      Cnstr_Call :=
        Make_Function_Call (Loc,
          Name                   => New_Occurrence_Of (Act_Constr, Loc),
          Parameter_Associations => New_List (Relocate_Node (Param_Arg)));

      --  Establish its controlling tag from the tag passed to the instance
      --  The tag may be given by a function call, in which case a temporary
      --  should be generated now, to prevent out-of-order insertions during
      --  the expansion of that call when stack-checking is enabled.

      if Present (Iface_Tag) then
         Set_Controlling_Argument (Cnstr_Call,
           New_Occurrence_Of (Defining_Identifier (Iface_Tag), Loc));
      else
         Set_Controlling_Argument (Cnstr_Call,
           Relocate_Node (Tag_Arg));
      end if;

      --  Rewrite and analyze the call to the instance as a class-wide
      --  conversion of the call to the actual constructor.

      Rewrite (N, Convert_To (Result_Typ, Cnstr_Call));
      Analyze_And_Resolve (N, Etype (Act_Constr));

      --  Do not generate a run-time check on the built object if tag
      --  checks are suppressed for the result type or VM_Target /= No_VM

      if Tag_Checks_Suppressed (Etype (Result_Typ))
        or else not Tagged_Type_Expansion
      then
         null;

      --  Generate a class-wide membership test to ensure that the call's tag
      --  argument denotes a type within the class. We must keep separate the
      --  case in which the Result_Type of the constructor function is a tagged
      --  type from the case in which it is an abstract interface because the
      --  run-time subprogram required to check these cases differ (and have
      --  one difference in their parameters profile).

      --  Call CW_Membership if the Result_Type is a tagged type to look for
      --  the tag in the table of ancestor tags.

      elsif not Is_Interface (Result_Typ) then
         declare
            Obj_Tag_Node : Node_Id := New_Copy_Tree (Tag_Arg);
            CW_Test_Node : Node_Id;

         begin
            Build_CW_Membership (Loc,
              Obj_Tag_Node => Obj_Tag_Node,
              Typ_Tag_Node =>
                New_Reference_To (
                   Node (First_Elmt (Access_Disp_Table (
                                       Root_Type (Result_Typ)))), Loc),
              Related_Nod => N,
              New_Node    => CW_Test_Node);

            Insert_Action (N,
              Make_Implicit_If_Statement (N,
                Condition =>
                  Make_Op_Not (Loc, CW_Test_Node),
                Then_Statements =>
                  New_List (Make_Raise_Statement (Loc,
                              New_Occurrence_Of (RTE (RE_Tag_Error), Loc)))));
         end;

      --  Call IW_Membership test if the Result_Type is an abstract interface
      --  to look for the tag in the table of interface tags.

      else
         Insert_Action (N,
           Make_Implicit_If_Statement (N,
             Condition =>
               Make_Op_Not (Loc,
                 Make_Function_Call (Loc,
                    Name => New_Occurrence_Of (RTE (RE_IW_Membership), Loc),
                    Parameter_Associations => New_List (
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Copy_Tree (Tag_Arg),
                        Attribute_Name => Name_Address),

                      New_Reference_To (
                        Node (First_Elmt (Access_Disp_Table (
                                            Root_Type (Result_Typ)))), Loc)))),
             Then_Statements =>
               New_List (
                 Make_Raise_Statement (Loc,
                   Name => New_Occurrence_Of (RTE (RE_Tag_Error), Loc)))));
      end if;
   end Expand_Dispatching_Constructor_Call;

   ---------------------------
   -- Expand_Exception_Call --
   ---------------------------

   --  If the function call is not within an exception handler, then the call
   --  is replaced by a null string. Otherwise the appropriate routine in
   --  Ada.Exceptions is called passing the choice parameter specification
   --  from the enclosing handler. If the enclosing handler lacks a choice
   --  parameter, then one is supplied.

   procedure Expand_Exception_Call (N : Node_Id; Ent : RE_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      P   : Node_Id;
      E   : Entity_Id;

   begin
      --  Climb up parents to see if we are in exception handler

      P := Parent (N);
      loop
         --  Case of not in exception handler, replace by null string

         if No (P) then
            Rewrite (N,
              Make_String_Literal (Loc,
                Strval => ""));
            exit;

         --  Case of in exception handler

         elsif Nkind (P) = N_Exception_Handler then

            --  Handler cannot be used for a local raise, and furthermore, this
            --  is a violation of the No_Exception_Propagation restriction.

            Set_Local_Raise_Not_OK (P);
            Check_Restriction (No_Exception_Propagation, N);

            --  If no choice parameter present, then put one there. Note that
            --  we do not need to put it on the entity chain, since no one will
            --  be referencing it by normal visibility methods.

            if No (Choice_Parameter (P)) then
               E := Make_Temporary (Loc, 'E');
               Set_Choice_Parameter (P, E);
               Set_Ekind (E, E_Variable);
               Set_Etype (E, RTE (RE_Exception_Occurrence));
               Set_Scope (E, Current_Scope);
            end if;

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (RTE (Ent), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Choice_Parameter (P), Loc))));
            exit;

         --  Keep climbing!

         else
            P := Parent (P);
         end if;
      end loop;

      Analyze_And_Resolve (N, Standard_String);
   end Expand_Exception_Call;

   ------------------------
   -- Expand_Import_Call --
   ------------------------

   --  The function call must have a static string as its argument. We create
   --  a dummy variable which uses this string as the external name in an
   --  Import pragma. The result is then obtained as the address of this
   --  dummy variable, converted to the appropriate target type.

   procedure Expand_Import_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Entity_Id  := Entity (Name (N));
      Str : constant Node_Id    := First_Actual (N);
      Dum : constant Entity_Id  := Make_Temporary (Loc, 'D');

   begin
      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Dum,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Character, Loc)),

        Make_Pragma (Loc,
          Chars                        => Name_Import,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Name_Ada)),

            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Chars (Dum))),

            Make_Pragma_Argument_Association (Loc,
              Chars => Name_Link_Name,
              Expression => Relocate_Node (Str))))));

      Rewrite (N,
        Unchecked_Convert_To (Etype (Ent),
          Make_Attribute_Reference (Loc,
            Prefix         => Make_Identifier (Loc, Chars (Dum)),
            Attribute_Name => Name_Address)));

      Analyze_And_Resolve (N, Etype (Ent));
   end Expand_Import_Call;

   ---------------------------
   -- Expand_Intrinsic_Call --
   ---------------------------

   procedure Expand_Intrinsic_Call (N : Node_Id; E : Entity_Id) is
      Nam : Name_Id;

   begin
      --  If an external name is specified for the intrinsic, it is handled
      --  by the back-end: leave the call node unchanged for now.

      if Present (Interface_Name (E)) then
         return;
      end if;

      --  If the intrinsic subprogram is generic, gets its original name

      if Present (Parent (E))
        and then Present (Generic_Parent (Parent (E)))
      then
         Nam := Chars (Generic_Parent (Parent (E)));
      else
         Nam := Chars (E);
      end if;

      if Nam = Name_Asm then
         Expand_Asm_Call (N);

      elsif Nam = Name_Divide then
         Expand_Decimal_Divide_Call (N);

      elsif Nam = Name_Exception_Information then
         Expand_Exception_Call (N, RE_Exception_Information);

      elsif Nam = Name_Exception_Message then
         Expand_Exception_Call (N, RE_Exception_Message);

      elsif Nam = Name_Exception_Name then
         Expand_Exception_Call (N, RE_Exception_Name_Simple);

      elsif Nam = Name_Generic_Dispatching_Constructor then
         Expand_Dispatching_Constructor_Call (N);

      elsif Nam_In (Nam, Name_Import_Address,
                         Name_Import_Largest_Value,
                         Name_Import_Value)
      then
         Expand_Import_Call (N);

      elsif Nam = Name_Is_Negative then
         Expand_Is_Negative (N);

      elsif Nam = Name_Rotate_Left then
         Expand_Shift (N, E, N_Op_Rotate_Left);

      elsif Nam = Name_Rotate_Right then
         Expand_Shift (N, E, N_Op_Rotate_Right);

      elsif Nam = Name_Shift_Left then
         Expand_Shift (N, E, N_Op_Shift_Left);

      elsif Nam = Name_Shift_Right then
         Expand_Shift (N, E, N_Op_Shift_Right);

      elsif Nam = Name_Shift_Right_Arithmetic then
         Expand_Shift (N, E, N_Op_Shift_Right_Arithmetic);

      elsif Nam = Name_Unchecked_Conversion then
         Expand_Unc_Conversion (N, E);

      elsif Nam = Name_Unchecked_Deallocation then
         Expand_Unc_Deallocation (N);

      elsif Nam = Name_To_Address then
         Expand_To_Address (N);

      elsif Nam = Name_To_Pointer then
         Expand_To_Pointer (N);

      elsif Nam_In (Nam, Name_File,
                         Name_Line,
                         Name_Source_Location,
                         Name_Enclosing_Entity)
      then
         Expand_Source_Info (N, Nam);

         --  If we have a renaming, expand the call to the original operation,
         --  which must itself be intrinsic, since renaming requires matching
         --  conventions and this has already been checked.

      elsif Present (Alias (E)) then
         Expand_Intrinsic_Call (N, Alias (E));

      elsif Nkind (N) in N_Binary_Op then
         Expand_Binary_Operator_Call (N);

         --  The only other case is where an external name was specified, since
         --  this is the only way that an otherwise unrecognized name could
         --  escape the checking in Sem_Prag. Nothing needs to be done in such
         --  a case, since we pass such a call to the back end unchanged.

      else
         null;
      end if;
   end Expand_Intrinsic_Call;

   ------------------------
   -- Expand_Is_Negative --
   ------------------------

   procedure Expand_Is_Negative (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Opnd  : constant Node_Id    := Relocate_Node (First_Actual (N));

   begin

      --  We replace the function call by the following expression

      --    if Opnd < 0.0 then
      --       True
      --    else
      --       if Opnd > 0.0 then
      --          False;
      --       else
      --          Float_Unsigned!(Float (Opnd)) /= 0
      --       end if;
      --    end if;

      Rewrite (N,
        Make_If_Expression (Loc,
          Expressions => New_List (
            Make_Op_Lt (Loc,
              Left_Opnd  => Duplicate_Subexpr (Opnd),
              Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),

            New_Occurrence_Of (Standard_True, Loc),

            Make_If_Expression (Loc,
             Expressions => New_List (
               Make_Op_Gt (Loc,
                 Left_Opnd  => Duplicate_Subexpr_No_Checks (Opnd),
                 Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),

               New_Occurrence_Of (Standard_False, Loc),

                Make_Op_Ne (Loc,
                  Left_Opnd =>
                    Unchecked_Convert_To
                      (RTE (RE_Float_Unsigned),
                       Convert_To
                         (Standard_Float,
                          Duplicate_Subexpr_No_Checks (Opnd))),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, 0)))))));

      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_Is_Negative;

   ------------------
   -- Expand_Shift --
   ------------------

   --  This procedure is used to convert a call to a shift function to the
   --  corresponding operator node. This conversion is not done by the usual
   --  circuit for converting calls to operator functions (e.g. "+"(1,2)) to
   --  operator nodes, because shifts are not predefined operators.

   --  As a result, whenever a shift is used in the source program, it will
   --  remain as a call until converted by this routine to the operator node
   --  form which Gigi is expecting to see.

   --  Note: it is possible for the expander to generate shift operator nodes
   --  directly, which will be analyzed in the normal manner by calling Analyze
   --  and Resolve. Such shift operator nodes will not be seen by Expand_Shift.

   procedure Expand_Shift (N : Node_Id; E : Entity_Id; K : Node_Kind) is
      Entyp : constant Entity_Id  := Etype (E);
      Left  : constant Node_Id    := First_Actual (N);
      Loc   : constant Source_Ptr := Sloc (N);
      Right : constant Node_Id    := Next_Actual (Left);
      Ltyp  : constant Node_Id    := Etype (Left);
      Rtyp  : constant Node_Id    := Etype (Right);
      Typ   : constant Entity_Id  := Etype (N);
      Snode : Node_Id;

   begin
      Snode := New_Node (K, Loc);
      Set_Right_Opnd (Snode, Relocate_Node (Right));
      Set_Chars      (Snode, Chars (E));
      Set_Etype      (Snode, Base_Type (Entyp));
      Set_Entity     (Snode, E);

      if Compile_Time_Known_Value (Type_High_Bound (Rtyp))
        and then Expr_Value (Type_High_Bound (Rtyp)) < Esize (Ltyp)
      then
         Set_Shift_Count_OK (Snode, True);
      end if;

      if Typ = Entyp then

         --  Note that we don't call Analyze and Resolve on this node, because
         --  it already got analyzed and resolved when it was a function call.

         Set_Left_Opnd (Snode, Relocate_Node (Left));
         Rewrite (N, Snode);
         Set_Analyzed (N);

      else

         --  If the context type is not the type of the operator, it is an
         --  inherited operator for a derived type. Wrap the node in a
         --  conversion so that it is type-consistent for possible further
         --  expansion (e.g. within a lock-free protected type).

         Set_Left_Opnd (Snode,
           Unchecked_Convert_To (Base_Type (Entyp), Relocate_Node (Left)));
         Rewrite (N, Unchecked_Convert_To (Typ, Snode));

         --  Analyze and resolve result formed by conversion to target type

         Analyze_And_Resolve (N, Typ);
      end if;
   end Expand_Shift;

   ------------------------
   -- Expand_Source_Info --
   ------------------------

   procedure Expand_Source_Info (N : Node_Id; Nam : Name_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : Entity_Id;

      procedure Write_Entity_Name (E : Entity_Id);
      --  Recursive procedure to construct string for qualified name of
      --  enclosing program unit. The qualification stops at an enclosing
      --  scope has no source name (block or loop). If entity is a subprogram
      --  instance, skip enclosing wrapper package.

      -----------------------
      -- Write_Entity_Name --
      -----------------------

      procedure Write_Entity_Name (E : Entity_Id) is
         SDef : Source_Ptr;
         TDef : constant Source_Buffer_Ptr :=
                  Source_Text (Get_Source_File_Index (Sloc (E)));

      begin
         --  Nothing to do if at outer level

         if Scope (E) = Standard_Standard then
            null;

         --  If scope comes from source, write its name

         elsif Comes_From_Source (Scope (E)) then
            Write_Entity_Name (Scope (E));
            Add_Char_To_Name_Buffer ('.');

         --  If in wrapper package skip past it

         elsif Is_Wrapper_Package (Scope (E)) then
            Write_Entity_Name (Scope (Scope (E)));
            Add_Char_To_Name_Buffer ('.');

         --  Otherwise nothing to output (happens in unnamed block statements)

         else
            null;
         end if;

         --  Loop to output the name

         --  is this right wrt wide char encodings ??? (no!)

         SDef := Sloc (E);
         while TDef (SDef) in '0' .. '9'
           or else TDef (SDef) >= 'A'
           or else TDef (SDef) = ASCII.ESC
         loop
            Add_Char_To_Name_Buffer (TDef (SDef));
            SDef := SDef + 1;
         end loop;
      end Write_Entity_Name;

   --  Start of processing for Expand_Source_Info

   begin
      --  Integer cases

      if Nam = Name_Line then
         Rewrite (N,
           Make_Integer_Literal (Loc,
             Intval => UI_From_Int (Int (Get_Logical_Line_Number (Loc)))));
         Analyze_And_Resolve (N, Standard_Positive);

      --  String cases

      else
         Name_Len := 0;

         case Nam is
            when Name_File =>
               Get_Decoded_Name_String
                 (Reference_Name (Get_Source_File_Index (Loc)));

            when Name_Source_Location =>
               Build_Location_String (Loc);

            when Name_Enclosing_Entity =>

               --  Skip enclosing blocks to reach enclosing unit

               Ent := Current_Scope;
               while Present (Ent) loop
                  exit when Ekind (Ent) /= E_Block
                    and then Ekind (Ent) /= E_Loop;
                  Ent := Scope (Ent);
               end loop;

               --  Ent now points to the relevant defining entity

               Write_Entity_Name (Ent);

            when others =>
               raise Program_Error;
         end case;

         Rewrite (N,
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));
         Analyze_And_Resolve (N, Standard_String);
      end if;

      Set_Is_Static_Expression (N);
   end Expand_Source_Info;

   ---------------------------
   -- Expand_Unc_Conversion --
   ---------------------------

   procedure Expand_Unc_Conversion (N : Node_Id; E : Entity_Id) is
      Func : constant Entity_Id  := Entity (Name (N));
      Conv : Node_Id;
      Ftyp : Entity_Id;
      Ttyp : Entity_Id;

   begin
      --  Rewrite as unchecked conversion node. Note that we must convert
      --  the operand to the formal type of the input parameter of the
      --  function, so that the resulting N_Unchecked_Type_Conversion
      --  call indicates the correct types for Gigi.

      --  Right now, we only do this if a scalar type is involved. It is
      --  not clear if it is needed in other cases. If we do attempt to
      --  do the conversion unconditionally, it crashes 3411-018. To be
      --  investigated further ???

      Conv := Relocate_Node (First_Actual (N));
      Ftyp := Etype (First_Formal (Func));

      if Is_Scalar_Type (Ftyp) then
         Conv := Convert_To (Ftyp, Conv);
         Set_Parent (Conv, N);
         Analyze_And_Resolve (Conv);
      end if;

      --  The instantiation of Unchecked_Conversion creates a wrapper package,
      --  and the target type is declared as a subtype of the actual. Recover
      --  the actual, which is the subtype indic. in the subtype declaration
      --  for the target type. This is semantically correct, and avoids
      --  anomalies with access subtypes. For entities, leave type as is.

      --  We do the analysis here, because we do not want the compiler
      --  to try to optimize or otherwise reorganize the unchecked
      --  conversion node.

      Ttyp := Etype (E);

      if Is_Entity_Name (Conv) then
         null;

      elsif Nkind (Parent (Ttyp)) = N_Subtype_Declaration then
         Ttyp := Entity (Subtype_Indication (Parent (Etype (E))));

      elsif Is_Itype (Ttyp) then
         Ttyp :=
           Entity (Subtype_Indication (Associated_Node_For_Itype (Ttyp)));
      else
         raise Program_Error;
      end if;

      Rewrite (N, Unchecked_Convert_To (Ttyp, Conv));
      Set_Etype (N, Ttyp);
      Set_Analyzed (N);

      if Nkind (N) = N_Unchecked_Type_Conversion then
         Expand_N_Unchecked_Type_Conversion (N);
      end if;
   end Expand_Unc_Conversion;

   -----------------------------
   -- Expand_Unc_Deallocation --
   -----------------------------

   --  Generate the following Code :

   --    if Arg /= null then
   --     <Finalize_Call> (.., T'Class(Arg.all), ..);  -- for controlled types
   --       Free (Arg);
   --       Arg := Null;
   --    end if;

   --  For a task, we also generate a call to Free_Task to ensure that the
   --  task itself is freed if it is terminated, ditto for a simple protected
   --  object, with a call to Finalize_Protection. For composite types that
   --  have tasks or simple protected objects as components, we traverse the
   --  structures to find and terminate those components.

   procedure Expand_Unc_Deallocation (N : Node_Id) is
      Arg       : constant Node_Id    := First_Actual (N);
      Loc       : constant Source_Ptr := Sloc (N);
      Typ       : constant Entity_Id  := Etype (Arg);
      Desig_T   : constant Entity_Id  := Designated_Type (Typ);
      Rtyp      : constant Entity_Id  := Underlying_Type (Root_Type (Typ));
      Pool      : constant Entity_Id  := Associated_Storage_Pool (Rtyp);
      Stmts     : constant List_Id    := New_List;
      Needs_Fin : constant Boolean    := Needs_Finalization (Desig_T);

      Finalizer_Data  : Finalization_Exception_Data;

      Blk        : Node_Id := Empty;
      Deref      : Node_Id;
      Final_Code : List_Id;
      Free_Arg   : Node_Id;
      Free_Node  : Node_Id;
      Gen_Code   : Node_Id;

      Arg_Known_Non_Null : constant Boolean := Known_Non_Null (N);
      --  This captures whether we know the argument to be non-null so that
      --  we can avoid the test. The reason that we need to capture this is
      --  that we analyze some generated statements before properly attaching
      --  them to the tree, and that can disturb current value settings.

   begin
      --  Nothing to do if we know the argument is null

      if Known_Null (N) then
         return;
      end if;

      --  Processing for pointer to controlled type

      if Needs_Fin then
         Deref :=
           Make_Explicit_Dereference (Loc,
             Prefix => Duplicate_Subexpr_No_Checks (Arg));

         --  If the type is tagged, then we must force dispatching on the
         --  finalization call because the designated type may not be the
         --  actual type of the object.

         if Is_Tagged_Type (Desig_T)
           and then not Is_Class_Wide_Type (Desig_T)
         then
            Deref := Unchecked_Convert_To (Class_Wide_Type (Desig_T), Deref);

         elsif not Is_Tagged_Type (Desig_T) then

            --  Set type of result, to force a conversion when needed (see
            --  exp_ch7, Convert_View), given that Deep_Finalize may be
            --  inherited from the parent type, and we need the type of the
            --  expression to see whether the conversion is in fact needed.

            Set_Etype (Deref, Desig_T);
         end if;

         --  The finalization call is expanded wrapped in a block to catch any
         --  possible exception. If an exception does occur, then Program_Error
         --  must be raised following the freeing of the object and its removal
         --  from the finalization collection's list. We set a flag to record
         --  that an exception was raised, and save its occurrence for use in
         --  the later raise.
         --
         --  Generate:
         --    Abort  : constant Boolean :=
         --               Exception_Occurrence (Get_Current_Excep.all.all) =
         --                 Standard'Abort_Signal'Identity;
         --      <or>
         --    Abort  : constant Boolean := False;  --  no abort

         --    E      : Exception_Occurrence;
         --    Raised : Boolean := False;
         --
         --    begin
         --       [Deep_]Finalize (Obj);
         --    exception
         --       when others =>
         --          Raised := True;
         --          Save_Occurrence (E, Get_Current_Excep.all.all);
         --    end;

         Build_Object_Declarations (Finalizer_Data, Stmts, Loc);

         Final_Code := New_List (
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements         => New_List (
                   Make_Final_Call (Obj_Ref => Deref, Typ => Desig_T)),
                 Exception_Handlers => New_List (
                   Build_Exception_Handler (Finalizer_Data)))));

         --  For .NET/JVM, detach the object from the containing finalization
         --  collection before finalizing it.

         if VM_Target /= No_VM and then Is_Controlled (Desig_T) then
            Prepend_To (Final_Code,
              Make_Detach_Call (New_Copy_Tree (Arg)));
         end if;

         --  If aborts are allowed, then the finalization code must be
         --  protected by an abort defer/undefer pair.

         if Abort_Allowed then
            Prepend_To (Final_Code,
              Build_Runtime_Call (Loc, RE_Abort_Defer));

            Blk :=
              Make_Block_Statement (Loc, Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements  => Final_Code,
                  At_End_Proc =>
                    New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc)));

            Append (Blk, Stmts);
         else
            Append_List_To (Stmts, Final_Code);
         end if;
      end if;

      --  For a task type, call Free_Task before freeing the ATCB

      if Is_Task_Type (Desig_T) then
         declare
            Stat : Node_Id := Prev (N);
            Nam1 : Node_Id;
            Nam2 : Node_Id;

         begin
            --  An Abort followed by a Free will not do what the user expects,
            --  because the abort is not immediate. This is worth a warning.

            while Present (Stat)
              and then not Comes_From_Source (Original_Node (Stat))
            loop
               Prev (Stat);
            end loop;

            if Present (Stat)
              and then Nkind (Original_Node (Stat)) = N_Abort_Statement
            then
               Stat := Original_Node (Stat);
               Nam1 := First (Names (Stat));
               Nam2 := Original_Node (First (Parameter_Associations (N)));

               if Nkind (Nam1) = N_Explicit_Dereference
                 and then Is_Entity_Name (Prefix (Nam1))
                 and then Is_Entity_Name (Nam2)
                 and then Entity (Prefix (Nam1)) = Entity (Nam2)
               then
                  Error_Msg_N ("abort may take time to complete??", N);
                  Error_Msg_N ("\deallocation might have no effect??", N);
                  Error_Msg_N ("\safer to wait for termination??", N);
               end if;
            end if;
         end;

         Append_To
           (Stmts, Cleanup_Task (N, Duplicate_Subexpr_No_Checks (Arg)));

      --  For composite types that contain tasks, recurse over the structure
      --  to build the selectors for the task subcomponents.

      elsif Has_Task (Desig_T) then
         if Is_Record_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Record (N, Arg, Desig_T));

         elsif Is_Array_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Array (N, Arg, Desig_T));
         end if;
      end if;

      --  Same for simple protected types. Eventually call Finalize_Protection
      --  before freeing the PO for each protected component.

      if Is_Simple_Protected_Type (Desig_T) then
         Append_To (Stmts,
           Cleanup_Protected_Object (N, Duplicate_Subexpr_No_Checks (Arg)));

      elsif Has_Simple_Protected_Object (Desig_T) then
         if Is_Record_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Record (N, Arg, Desig_T));
         elsif Is_Array_Type (Desig_T) then
            Append_List_To (Stmts, Cleanup_Array (N, Arg, Desig_T));
         end if;
      end if;

      --  Normal processing for non-controlled types

      Free_Arg := Duplicate_Subexpr_No_Checks (Arg);
      Free_Node := Make_Free_Statement (Loc, Empty);
      Append_To (Stmts, Free_Node);
      Set_Storage_Pool (Free_Node, Pool);

      --  Attach to tree before analysis of generated subtypes below

      Set_Parent (Stmts, Parent (N));

      --  Deal with storage pool

      if Present (Pool) then

         --  Freeing the secondary stack is meaningless

         if Is_RTE (Pool, RE_SS_Pool) then
            null;

         --  If the pool object is of a simple storage pool type, then attempt
         --  to locate the type's Deallocate procedure, if any, and set the
         --  free operation's procedure to call. If the type doesn't have a
         --  Deallocate (which is allowed), then the actual will simply be set
         --  to null.

         elsif Present (Get_Rep_Pragma
                          (Etype (Pool), Name_Simple_Storage_Pool_Type))
         then
            declare
               Pool_Type  : constant Entity_Id := Base_Type (Etype (Pool));
               Dealloc_Op : Entity_Id;
            begin
               Dealloc_Op := Get_Name_Entity_Id (Name_Deallocate);
               while Present (Dealloc_Op) loop
                  if Scope (Dealloc_Op) = Scope (Pool_Type)
                    and then Present (First_Formal (Dealloc_Op))
                    and then Etype (First_Formal (Dealloc_Op)) = Pool_Type
                  then
                     Set_Procedure_To_Call (Free_Node, Dealloc_Op);
                     exit;
                  else
                     Dealloc_Op := Homonym (Dealloc_Op);
                  end if;
               end loop;
            end;

         --  Case of a class-wide pool type: make a dispatching call to
         --  Deallocate through the class-wide Deallocate_Any.

         elsif Is_Class_Wide_Type (Etype (Pool)) then
            Set_Procedure_To_Call (Free_Node, RTE (RE_Deallocate_Any));

         --  Case of a specific pool type: make a statically bound call

         else
            Set_Procedure_To_Call (Free_Node,
              Find_Prim_Op (Etype (Pool), Name_Deallocate));
         end if;
      end if;

      if Present (Procedure_To_Call (Free_Node)) then

         --  For all cases of a Deallocate call, the back-end needs to be able
         --  to compute the size of the object being freed. This may require
         --  some adjustments for objects of dynamic size.
         --
         --  If the type is class wide, we generate an implicit type with the
         --  right dynamic size, so that the deallocate call gets the right
         --  size parameter computed by GIGI. Same for an access to
         --  unconstrained packed array.

         if Is_Class_Wide_Type (Desig_T)
           or else
            (Is_Array_Type (Desig_T)
              and then not Is_Constrained (Desig_T)
              and then Is_Packed (Desig_T))
         then
            declare
               Deref    : constant Node_Id :=
                            Make_Explicit_Dereference (Loc,
                              Duplicate_Subexpr_No_Checks (Arg));
               D_Subtyp : Node_Id;
               D_Type   : Entity_Id;

            begin
               --  Perform minor decoration as it is needed by the side effect
               --  removal mechanism.

               Set_Etype  (Deref, Desig_T);
               Set_Parent (Deref, Free_Node);
               D_Subtyp := Make_Subtype_From_Expr (Deref, Desig_T);

               if Nkind (D_Subtyp) in N_Has_Entity then
                  D_Type := Entity (D_Subtyp);

               else
                  D_Type := Make_Temporary (Loc, 'A');
                  Insert_Action (Deref,
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => D_Type,
                      Subtype_Indication  => D_Subtyp));
               end if;

               --  Force freezing at the point of the dereference. For the
               --  class wide case, this avoids having the subtype frozen
               --  before the equivalent type.

               Freeze_Itype (D_Type, Deref);

               Set_Actual_Designated_Subtype (Free_Node, D_Type);
            end;

         end if;
      end if;

      --  Ada 2005 (AI-251): In case of abstract interface type we must
      --  displace the pointer to reference the base of the object to
      --  deallocate its memory, unless we're targetting a VM, in which case
      --  no special processing is required.

      --  Generate:
      --    free (Base_Address (Obj_Ptr))

      if Is_Interface (Directly_Designated_Type (Typ))
        and then Tagged_Type_Expansion
      then
         Set_Expression (Free_Node,
           Unchecked_Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (RE_Base_Address), Loc),
               Parameter_Associations => New_List (
                 Unchecked_Convert_To (RTE (RE_Address), Free_Arg)))));

      --  Generate:
      --    free (Obj_Ptr)

      else
         Set_Expression (Free_Node, Free_Arg);
      end if;

      --  Only remaining step is to set result to null, or generate a raise of
      --  Constraint_Error if the target object is "not null".

      if Can_Never_Be_Null (Etype (Arg)) then
         Append_To (Stmts,
           Make_Raise_Constraint_Error (Loc,
             Reason => CE_Access_Check_Failed));

      else
         declare
            Lhs : constant Node_Id := Duplicate_Subexpr_No_Checks (Arg);
         begin
            Set_Assignment_OK (Lhs);
            Append_To (Stmts,
              Make_Assignment_Statement (Loc,
                Name       => Lhs,
                Expression => Make_Null (Loc)));
         end;
      end if;

      --  Generate a test of whether any earlier finalization raised an
      --  exception, and in that case raise Program_Error with the previous
      --  exception occurrence.

      --  Generate:
      --    if Raised and then not Abort then
      --       raise Program_Error;                  --  for .NET and
      --                                             --  restricted RTS
      --         <or>
      --       Raise_From_Controlled_Operation (E);  --  all other cases
      --    end if;

      if Needs_Fin then
         Append_To (Stmts, Build_Raise_Statement (Finalizer_Data));
      end if;

      --  If we know the argument is non-null, then make a block statement
      --  that contains the required statements, no need for a test.

      if Arg_Known_Non_Null then
         Gen_Code :=
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
             Statements => Stmts));

      --  If the argument may be null, wrap the statements inside an IF that
      --  does an explicit test to exclude the null case.

      else
         Gen_Code :=
           Make_Implicit_If_Statement (N,
             Condition =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Arg),
                 Right_Opnd => Make_Null (Loc)),
             Then_Statements => Stmts);
      end if;

      --  Rewrite the call

      Rewrite (N, Gen_Code);
      Analyze (N);

      --  If we generated a block with an At_End_Proc, expand the exception
      --  handler. We need to wait until after everything else is analyzed.

      if Present (Blk) then
         Expand_At_End_Handler
           (Handled_Statement_Sequence (Blk), Entity (Identifier (Blk)));
      end if;
   end Expand_Unc_Deallocation;

   -----------------------
   -- Expand_To_Address --
   -----------------------

   procedure Expand_To_Address (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Arg : constant Node_Id := First_Actual (N);
      Obj : Node_Id;

   begin
      Remove_Side_Effects (Arg);

      Obj := Make_Explicit_Dereference (Loc, Relocate_Node (Arg));

      Rewrite (N,
        Make_If_Expression (Loc,
          Expressions => New_List (
            Make_Op_Eq (Loc,
              Left_Opnd => New_Copy_Tree (Arg),
              Right_Opnd => Make_Null (Loc)),
            New_Occurrence_Of (RTE (RE_Null_Address), Loc),
            Make_Attribute_Reference (Loc,
              Prefix         => Obj,
              Attribute_Name => Name_Address))));

      Analyze_And_Resolve (N, RTE (RE_Address));
   end Expand_To_Address;

   -----------------------
   -- Expand_To_Pointer --
   -----------------------

   procedure Expand_To_Pointer (N : Node_Id) is
      Arg : constant Node_Id := First_Actual (N);

   begin
      Rewrite (N, Unchecked_Convert_To (Etype (N), Arg));
      Analyze (N);
   end Expand_To_Pointer;

end Exp_Intr;
