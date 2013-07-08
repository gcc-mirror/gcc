------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A T T R                              --
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
with Exp_Atag; use Exp_Atag;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Dist; use Exp_Dist;
with Exp_Imgv; use Exp_Imgv;
with Exp_Pakd; use Exp_Pakd;
with Exp_Strm; use Exp_Strm;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Exp_VFpt; use Exp_VFpt;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Gnatvsn;  use Gnatvsn;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Validsw;  use Validsw;

package body Exp_Attr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Build_Array_VS_Func
     (A_Type : Entity_Id;
      Nod    : Node_Id) return Entity_Id;
   --  Build function to test Valid_Scalars for array type A_Type. Nod is the
   --  Valid_Scalars attribute node, used to insert the function body, and the
   --  value returned is the entity of the constructed function body. We do not
   --  bother to generate a separate spec for this subprogram.

   procedure Compile_Stream_Body_In_Scope
     (N     : Node_Id;
      Decl  : Node_Id;
      Arr   : Entity_Id;
      Check : Boolean);
   --  The body for a stream subprogram may be generated outside of the scope
   --  of the type. If the type is fully private, it may depend on the full
   --  view of other types (e.g. indexes) that are currently private as well.
   --  We install the declarations of the package in which the type is declared
   --  before compiling the body in what is its proper environment. The Check
   --  parameter indicates if checks are to be suppressed for the stream body.
   --  We suppress checks for array/record reads, since the rule is that these
   --  are like assignments, out of range values due to uninitialized storage,
   --  or other invalid values do NOT cause a Constraint_Error to be raised.

   procedure Expand_Access_To_Protected_Op
     (N    : Node_Id;
      Pref : Node_Id;
      Typ  : Entity_Id);
   --  An attribute reference to a protected subprogram is transformed into
   --  a pair of pointers: one to the object, and one to the operations.
   --  This expansion is performed for 'Access and for 'Unrestricted_Access.

   procedure Expand_Fpt_Attribute
     (N    : Node_Id;
      Pkg  : RE_Id;
      Nam  : Name_Id;
      Args : List_Id);
   --  This procedure expands a call to a floating-point attribute function.
   --  N is the attribute reference node, and Args is a list of arguments to
   --  be passed to the function call. Pkg identifies the package containing
   --  the appropriate instantiation of System.Fat_Gen. Float arguments in Args
   --  have already been converted to the floating-point type for which Pkg was
   --  instantiated. The Nam argument is the relevant attribute processing
   --  routine to be called. This is the same as the attribute name, except in
   --  the Unaligned_Valid case.

   procedure Expand_Fpt_Attribute_R (N : Node_Id);
   --  This procedure expands a call to a floating-point attribute function
   --  that takes a single floating-point argument. The function to be called
   --  is always the same as the attribute name.

   procedure Expand_Fpt_Attribute_RI (N : Node_Id);
   --  This procedure expands a call to a floating-point attribute function
   --  that takes one floating-point argument and one integer argument. The
   --  function to be called is always the same as the attribute name.

   procedure Expand_Fpt_Attribute_RR (N : Node_Id);
   --  This procedure expands a call to a floating-point attribute function
   --  that takes two floating-point arguments. The function to be called
   --  is always the same as the attribute name.

   procedure Expand_Loop_Entry_Attribute (Attr : Node_Id);
   --  Handle the expansion of attribute 'Loop_Entry. As a result, the related
   --  loop may be converted into a conditional block. See body for details.

   procedure Expand_Pred_Succ (N : Node_Id);
   --  Handles expansion of Pred or Succ attributes for case of non-real
   --  operand with overflow checking required.

   procedure Expand_Update_Attribute (N : Node_Id);
   --  Handle the expansion of attribute Update

   function Get_Index_Subtype (N : Node_Id) return Entity_Id;
   --  Used for Last, Last, and Length, when the prefix is an array type.
   --  Obtains the corresponding index subtype.

   procedure Find_Fat_Info
     (T        : Entity_Id;
      Fat_Type : out Entity_Id;
      Fat_Pkg  : out RE_Id);
   --  Given a floating-point type T, identifies the package containing the
   --  attributes for this type (returned in Fat_Pkg), and the corresponding
   --  type for which this package was instantiated from Fat_Gen. Error if T
   --  is not a floating-point type.

   function Find_Stream_Subprogram
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id;
   --  Returns the stream-oriented subprogram attribute for Typ. For tagged
   --  types, the corresponding primitive operation is looked up, else the
   --  appropriate TSS from the type itself, or from its closest ancestor
   --  defining it, is returned. In both cases, inheritance of representation
   --  aspects is thus taken into account.

   function Full_Base (T : Entity_Id) return Entity_Id;
   --  The stream functions need to examine the underlying representation of
   --  composite types. In some cases T may be non-private but its base type
   --  is, in which case the function returns the corresponding full view.

   function Get_Stream_Convert_Pragma (T : Entity_Id) return Node_Id;
   --  Given a type, find a corresponding stream convert pragma that applies to
   --  the implementation base type of this type (Typ). If found, return the
   --  pragma node, otherwise return Empty if no pragma is found.

   function Is_Constrained_Packed_Array (Typ : Entity_Id) return Boolean;
   --  Utility for array attributes, returns true on packed constrained
   --  arrays, and on access to same.

   function Is_Inline_Floating_Point_Attribute (N : Node_Id) return Boolean;
   --  Returns true iff the given node refers to an attribute call that
   --  can be expanded directly by the back end and does not need front end
   --  expansion. Typically used for rounding and truncation attributes that
   --  appear directly inside a conversion to integer.

   -------------------------
   -- Build_Array_VS_Func --
   -------------------------

   function Build_Array_VS_Func
     (A_Type : Entity_Id;
      Nod    : Node_Id) return Entity_Id
   is
      Loc        : constant Source_Ptr := Sloc (Nod);
      Comp_Type  : constant Entity_Id  := Component_Type (A_Type);
      Body_Stmts : List_Id;
      Index_List : List_Id;
      Func_Id    : Entity_Id;
      Formals    : List_Id;

      function Test_Component return List_Id;
      --  Create one statement to test validity of one component designated by
      --  a full set of indexes. Returns statement list containing test.

      function Test_One_Dimension (N : Int) return List_Id;
      --  Create loop to test one dimension of the array. The single statement
      --  in the loop body tests the inner dimensions if any, or else the
      --  single component. Note that this procedure is called recursively,
      --  with N being the dimension to be initialized. A call with N greater
      --  than the number of dimensions simply generates the component test,
      --  terminating the recursion. Returns statement list containing tests.

      --------------------
      -- Test_Component --
      --------------------

      function Test_Component return List_Id is
         Comp : Node_Id;
         Anam : Name_Id;

      begin
         Comp :=
           Make_Indexed_Component (Loc,
             Prefix      => Make_Identifier (Loc, Name_uA),
             Expressions => Index_List);

         if Is_Scalar_Type (Comp_Type) then
            Anam := Name_Valid;
         else
            Anam := Name_Valid_Scalars;
         end if;

         return New_List (
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Not (Loc,
                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Attribute_Name => Anam,
                     Prefix         => Comp)),
             Then_Statements => New_List (
               Make_Simple_Return_Statement (Loc,
                 Expression => New_Occurrence_Of (Standard_False, Loc)))));
      end Test_Component;

      ------------------------
      -- Test_One_Dimension --
      ------------------------

      function Test_One_Dimension (N : Int) return List_Id is
         Index : Entity_Id;

      begin
         --  If all dimensions dealt with, we simply test the component

         if N > Number_Dimensions (A_Type) then
            return Test_Component;

         --  Here we generate the required loop

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append (New_Reference_To (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (Nod,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uA),
                            Attribute_Name  => Name_Range,
                            Expressions     => New_List (
                              Make_Integer_Literal (Loc, N))))),
                Statements =>  Test_One_Dimension (N + 1)),
              Make_Simple_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Standard_True, Loc)));
         end if;
      end Test_One_Dimension;

   --  Start of processing for Build_Array_VS_Func

   begin
      Index_List := New_List;
      Func_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('V'));

      Body_Stmts := Test_One_Dimension (1);

      --  Parameter is always (A : A_Typ)

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uA),
          In_Present          => True,
          Out_Present         => False,
          Parameter_Type      => New_Reference_To (A_Type, Loc)));

      --  Build body

      Set_Ekind       (Func_Id, E_Function);
      Set_Is_Internal (Func_Id);

      Insert_Action (Nod,
        Make_Subprogram_Body (Loc,
          Specification              =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Id,
              Parameter_Specifications => Formals,
                Result_Definition        =>
                  New_Occurrence_Of (Standard_Boolean, Loc)),
          Declarations               => New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Body_Stmts)));

      if not Debug_Generated_Code then
         Set_Debug_Info_Off (Func_Id);
      end if;

      return Func_Id;
   end Build_Array_VS_Func;

   ----------------------------------
   -- Compile_Stream_Body_In_Scope --
   ----------------------------------

   procedure Compile_Stream_Body_In_Scope
     (N     : Node_Id;
      Decl  : Node_Id;
      Arr   : Entity_Id;
      Check : Boolean)
   is
      Installed : Boolean := False;
      Scop      : constant Entity_Id := Scope (Arr);
      Curr      : constant Entity_Id := Current_Scope;

   begin
      if Is_Hidden (Arr)
        and then not In_Open_Scopes (Scop)
        and then Ekind (Scop) = E_Package
      then
         Push_Scope (Scop);
         Install_Visible_Declarations (Scop);
         Install_Private_Declarations (Scop);
         Installed := True;

         --  The entities in the package are now visible, but the generated
         --  stream entity must appear in the current scope (usually an
         --  enclosing stream function) so that itypes all have their proper
         --  scopes.

         Push_Scope (Curr);
      end if;

      if Check then
         Insert_Action (N, Decl);
      else
         Insert_Action (N, Decl, Suppress => All_Checks);
      end if;

      if Installed then

         --  Remove extra copy of current scope, and package itself

         Pop_Scope;
         End_Package_Scope (Scop);
      end if;
   end Compile_Stream_Body_In_Scope;

   -----------------------------------
   -- Expand_Access_To_Protected_Op --
   -----------------------------------

   procedure Expand_Access_To_Protected_Op
     (N    : Node_Id;
      Pref : Node_Id;
      Typ  : Entity_Id)
   is
      --  The value of the attribute_reference is a record containing two
      --  fields: an access to the protected object, and an access to the
      --  subprogram itself. The prefix is a selected component.

      Loc     : constant Source_Ptr := Sloc (N);
      Agg     : Node_Id;
      Btyp    : constant Entity_Id := Base_Type (Typ);
      Sub     : Entity_Id;
      Sub_Ref : Node_Id;
      E_T     : constant Entity_Id := Equivalent_Type (Btyp);
      Acc     : constant Entity_Id :=
                  Etype (Next_Component (First_Component (E_T)));
      Obj_Ref : Node_Id;
      Curr    : Entity_Id;

      function May_Be_External_Call return Boolean;
      --  If the 'Access is to a local operation, but appears in a context
      --  where it may lead to a call from outside the object, we must treat
      --  this as an external call. Clearly we cannot tell without full
      --  flow analysis, and a subsequent call that uses this 'Access may
      --  lead to a bounded error (trying to seize locks twice, e.g.). For
      --  now we treat 'Access as a potential external call if it is an actual
      --  in a call to an outside subprogram.

      --------------------------
      -- May_Be_External_Call --
      --------------------------

      function May_Be_External_Call return Boolean is
         Subp : Entity_Id;
         Par  : Node_Id := Parent (N);

      begin
         --  Account for the case where the Access attribute is part of a
         --  named parameter association.

         if Nkind (Par) = N_Parameter_Association then
            Par := Parent (Par);
         end if;

         if Nkind (Par) in N_Subprogram_Call
            and then Is_Entity_Name (Name (Par))
         then
            Subp := Entity (Name (Par));
            return not In_Open_Scopes (Scope (Subp));
         else
            return False;
         end if;
      end May_Be_External_Call;

   --  Start of processing for Expand_Access_To_Protected_Op

   begin
      --  Within the body of the protected type, the prefix designates a local
      --  operation, and the object is the first parameter of the corresponding
      --  protected body of the current enclosing operation.

      if Is_Entity_Name (Pref) then
         if May_Be_External_Call then
            Sub :=
              New_Occurrence_Of (External_Subprogram (Entity (Pref)), Loc);
         else
            Sub :=
              New_Occurrence_Of
                (Protected_Body_Subprogram (Entity (Pref)), Loc);
         end if;

         --  Don't traverse the scopes when the attribute occurs within an init
         --  proc, because we directly use the _init formal of the init proc in
         --  that case.

         Curr := Current_Scope;
         if not Is_Init_Proc (Curr) then
            pragma Assert (In_Open_Scopes (Scope (Entity (Pref))));

            while Scope (Curr) /= Scope (Entity (Pref)) loop
               Curr := Scope (Curr);
            end loop;
         end if;

         --  In case of protected entries the first formal of its Protected_
         --  Body_Subprogram is the address of the object.

         if Ekind (Curr) = E_Entry then
            Obj_Ref :=
               New_Occurrence_Of
                 (First_Formal
                   (Protected_Body_Subprogram (Curr)), Loc);

         --  If the current scope is an init proc, then use the address of the
         --  _init formal as the object reference.

         elsif Is_Init_Proc (Curr) then
            Obj_Ref :=
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (First_Formal (Curr), Loc),
                Attribute_Name => Name_Address);

         --  In case of protected subprograms the first formal of its
         --  Protected_Body_Subprogram is the object and we get its address.

         else
            Obj_Ref :=
              Make_Attribute_Reference (Loc,
                Prefix =>
                   New_Occurrence_Of
                     (First_Formal
                        (Protected_Body_Subprogram (Curr)), Loc),
                Attribute_Name => Name_Address);
         end if;

      --  Case where the prefix is not an entity name. Find the
      --  version of the protected operation to be called from
      --  outside the protected object.

      else
         Sub :=
           New_Occurrence_Of
             (External_Subprogram
               (Entity (Selector_Name (Pref))), Loc);

         Obj_Ref :=
           Make_Attribute_Reference (Loc,
             Prefix => Relocate_Node (Prefix (Pref)),
               Attribute_Name => Name_Address);
      end if;

      Sub_Ref :=
        Make_Attribute_Reference (Loc,
          Prefix         => Sub,
          Attribute_Name => Name_Access);

      --  We set the type of the access reference to the already generated
      --  access_to_subprogram type, and declare the reference analyzed, to
      --  prevent further expansion when the enclosing aggregate is analyzed.

      Set_Etype (Sub_Ref, Acc);
      Set_Analyzed (Sub_Ref);

      Agg :=
        Make_Aggregate (Loc,
          Expressions => New_List (Obj_Ref, Sub_Ref));

      --  Sub_Ref has been marked as analyzed, but we still need to make sure
      --  Sub is correctly frozen.

      Freeze_Before (N, Entity (Sub));

      Rewrite (N, Agg);
      Analyze_And_Resolve (N, E_T);

      --  For subsequent analysis, the node must retain its type. The backend
      --  will replace it with the equivalent type where needed.

      Set_Etype (N, Typ);
   end Expand_Access_To_Protected_Op;

   --------------------------
   -- Expand_Fpt_Attribute --
   --------------------------

   procedure Expand_Fpt_Attribute
     (N    : Node_Id;
      Pkg  : RE_Id;
      Nam  : Name_Id;
      Args : List_Id)
   is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Fnm : Node_Id;

   begin
      --  The function name is the selected component Attr_xxx.yyy where
      --  Attr_xxx is the package name, and yyy is the argument Nam.

      --  Note: it would be more usual to have separate RE entries for each
      --  of the entities in the Fat packages, but first they have identical
      --  names (so we would have to have lots of renaming declarations to
      --  meet the normal RE rule of separate names for all runtime entities),
      --  and second there would be an awful lot of them!

      Fnm :=
        Make_Selected_Component (Loc,
          Prefix        => New_Reference_To (RTE (Pkg), Loc),
          Selector_Name => Make_Identifier (Loc, Nam));

      --  The generated call is given the provided set of parameters, and then
      --  wrapped in a conversion which converts the result to the target type
      --  We use the base type as the target because a range check may be
      --  required.

      Rewrite (N,
        Unchecked_Convert_To (Base_Type (Etype (N)),
          Make_Function_Call (Loc,
            Name                   => Fnm,
            Parameter_Associations => Args)));

      Analyze_And_Resolve (N, Typ);
   end Expand_Fpt_Attribute;

   ----------------------------
   -- Expand_Fpt_Attribute_R --
   ----------------------------

   --  The single argument is converted to its root type to call the
   --  appropriate runtime function, with the actual call being built
   --  by Expand_Fpt_Attribute

   procedure Expand_Fpt_Attribute_R (N : Node_Id) is
      E1  : constant Node_Id    := First (Expressions (N));
      Ftp : Entity_Id;
      Pkg : RE_Id;
   begin
      Find_Fat_Info (Etype (E1), Ftp, Pkg);
      Expand_Fpt_Attribute
        (N, Pkg, Attribute_Name (N),
         New_List (Unchecked_Convert_To (Ftp, Relocate_Node (E1))));
   end Expand_Fpt_Attribute_R;

   -----------------------------
   -- Expand_Fpt_Attribute_RI --
   -----------------------------

   --  The first argument is converted to its root type and the second
   --  argument is converted to standard long long integer to call the
   --  appropriate runtime function, with the actual call being built
   --  by Expand_Fpt_Attribute

   procedure Expand_Fpt_Attribute_RI (N : Node_Id) is
      E1  : constant Node_Id   := First (Expressions (N));
      Ftp : Entity_Id;
      Pkg : RE_Id;
      E2  : constant Node_Id   := Next (E1);
   begin
      Find_Fat_Info (Etype (E1), Ftp, Pkg);
      Expand_Fpt_Attribute
        (N, Pkg, Attribute_Name (N),
         New_List (
           Unchecked_Convert_To (Ftp, Relocate_Node (E1)),
           Unchecked_Convert_To (Standard_Integer, Relocate_Node (E2))));
   end Expand_Fpt_Attribute_RI;

   -----------------------------
   -- Expand_Fpt_Attribute_RR --
   -----------------------------

   --  The two arguments are converted to their root types to call the
   --  appropriate runtime function, with the actual call being built
   --  by Expand_Fpt_Attribute

   procedure Expand_Fpt_Attribute_RR (N : Node_Id) is
      E1  : constant Node_Id := First (Expressions (N));
      E2  : constant Node_Id := Next (E1);
      Ftp : Entity_Id;
      Pkg : RE_Id;

   begin
      Find_Fat_Info (Etype (E1), Ftp, Pkg);
      Expand_Fpt_Attribute
        (N, Pkg, Attribute_Name (N),
         New_List (
           Unchecked_Convert_To (Ftp, Relocate_Node (E1)),
           Unchecked_Convert_To (Ftp, Relocate_Node (E2))));
   end Expand_Fpt_Attribute_RR;

   ---------------------------------
   -- Expand_Loop_Entry_Attribute --
   ---------------------------------

   procedure Expand_Loop_Entry_Attribute (Attr : Node_Id) is
      procedure Build_Conditional_Block
        (Loc       : Source_Ptr;
         Cond      : Node_Id;
         Loop_Stmt : Node_Id;
         If_Stmt   : out Node_Id;
         Blk_Stmt  : out Node_Id);
      --  Create a block Blk_Stmt with an empty declarative list and a single
      --  loop Loop_Stmt. The block is encased in an if statement If_Stmt with
      --  condition Cond. If_Stmt is Empty when there is no condition provided.

      function Is_Array_Iteration (N : Node_Id) return Boolean;
      --  Determine whether loop statement N denotes an Ada 2012 iteration over
      --  an array object.

      -----------------------------
      -- Build_Conditional_Block --
      -----------------------------

      procedure Build_Conditional_Block
        (Loc       : Source_Ptr;
         Cond      : Node_Id;
         Loop_Stmt : Node_Id;
         If_Stmt   : out Node_Id;
         Blk_Stmt  : out Node_Id)
      is
      begin
         --  Do not reanalyze the original loop statement because it is simply
         --  being relocated.

         Set_Analyzed (Loop_Stmt);

         Blk_Stmt :=
           Make_Block_Statement (Loc,
             Declarations               => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Loop_Stmt)));

         if Present (Cond) then
            If_Stmt :=
              Make_If_Statement (Loc,
                Condition       => Cond,
                Then_Statements => New_List (Blk_Stmt));
         else
            If_Stmt := Empty;
         end if;
      end Build_Conditional_Block;

      ------------------------
      -- Is_Array_Iteration --
      ------------------------

      function Is_Array_Iteration (N : Node_Id) return Boolean is
         Stmt : constant Node_Id := Original_Node (N);
         Iter : Node_Id;

      begin
         if Nkind (Stmt) = N_Loop_Statement
           and then Present (Iteration_Scheme (Stmt))
           and then Present (Iterator_Specification (Iteration_Scheme (Stmt)))
         then
            Iter := Iterator_Specification (Iteration_Scheme (Stmt));

            return
              Of_Present (Iter) and then Is_Array_Type (Etype (Name (Iter)));
         end if;

         return False;
      end Is_Array_Iteration;

      --  Local variables

      Exprs     : constant List_Id   := Expressions (Attr);
      Pref      : constant Node_Id   := Prefix (Attr);
      Typ       : constant Entity_Id := Etype (Pref);
      Blk       : Node_Id;
      Decls     : List_Id;
      Installed : Boolean;
      Loc       : Source_Ptr;
      Loop_Id   : Entity_Id;
      Loop_Stmt : Node_Id;
      Result    : Node_Id;
      Scheme    : Node_Id;
      Temp_Decl : Node_Id;
      Temp_Id   : Entity_Id;

   --  Start of processing for Expand_Loop_Entry_Attribute

   begin
      --  Step 1: Find the related loop

      --  The loop label variant of attribute 'Loop_Entry already has all the
      --  information in its expression.

      if Present (Exprs) then
         Loop_Id   := Entity (First (Exprs));
         Loop_Stmt := Label_Construct (Parent (Loop_Id));

      --  Climb the parent chain to find the nearest enclosing loop. Skip all
      --  internally generated loops for quantified expressions.

      else
         Loop_Stmt := Attr;
         while Present (Loop_Stmt) loop
            if Nkind (Loop_Stmt) = N_Loop_Statement
              and then Present (Identifier (Loop_Stmt))
            then
               exit;
            end if;

            Loop_Stmt := Parent (Loop_Stmt);
         end loop;

         Loop_Id := Entity (Identifier (Loop_Stmt));
      end if;

      Loc := Sloc (Loop_Stmt);

      --  Step 2: Transform the loop

      --  The loop has already been transformed during the expansion of a prior
      --  'Loop_Entry attribute. Retrieve the declarative list of the block.

      if Has_Loop_Entry_Attributes (Loop_Id) then

         --  When the related loop name appears as the argument of attribute
         --  Loop_Entry, the corresponding label construct is the generated
         --  block statement. This is because the expander reuses the label.

         if Nkind (Loop_Stmt) = N_Block_Statement then
            Decls := Declarations (Loop_Stmt);

         --  In all other cases, the loop must appear in the handled sequence
         --  of statements of the generated block.

         else
            pragma Assert
              (Nkind (Parent (Loop_Stmt)) = N_Handled_Sequence_Of_Statements
                and then Nkind (Parent (Parent (Loop_Stmt))) =
                                                      N_Block_Statement);

            Decls := Declarations (Parent (Parent (Loop_Stmt)));
         end if;

         Result := Empty;

      --  Transform the loop into a conditional block

      else
         Set_Has_Loop_Entry_Attributes (Loop_Id);
         Scheme := Iteration_Scheme (Loop_Stmt);

         --  Infinite loops are transformed into:

         --    declare
         --       Temp1 : constant <type of Pref1> := <Pref1>;
         --       . . .
         --       TempN : constant <type of PrefN> := <PrefN>;
         --    begin
         --       loop
         --          <original source statements with attribute rewrites>
         --       end loop;
         --    end;

         if No (Scheme) then
            Build_Conditional_Block (Loc,
              Cond      => Empty,
              Loop_Stmt => Relocate_Node (Loop_Stmt),
              If_Stmt   => Result,
              Blk_Stmt  => Blk);

            Result := Blk;

         --  While loops are transformed into:

         --    if <Condition> then
         --       declare
         --          Temp1 : constant <type of Pref1> := <Pref1>;
         --          . . .
         --          TempN : constant <type of PrefN> := <PrefN>;
         --       begin
         --          loop
         --             <original source statements with attribute rewrites>
         --             exit when not <Condition>;
         --          end loop;
         --       end;
         --    end if;

         --  Note that loops over iterators and containers are already
         --  converted into while loops.

         elsif Present (Condition (Scheme)) then
            declare
               Cond : constant Node_Id := Condition (Scheme);

            begin
               --  Transform the original while loop into an infinite loop
               --  where the last statement checks the negated condition. This
               --  placement ensures that the condition will not be evaluated
               --  twice on the first iteration.

               --  Generate:
               --    exit when not <Cond>:

               Append_To (Statements (Loop_Stmt),
                 Make_Exit_Statement (Loc,
                   Condition => Make_Op_Not (Loc, New_Copy_Tree (Cond))));

               Build_Conditional_Block (Loc,
                 Cond      => Relocate_Node (Cond),
                 Loop_Stmt => Relocate_Node (Loop_Stmt),
                 If_Stmt   => Result,
                 Blk_Stmt  => Blk);
            end;

         --  Ada 2012 iteration over an array is transformed into:

         --    if <Array_Nam>'Length (1) > 0
         --      and then <Array_Nam>'Length (N) > 0
         --    then
         --       declare
         --          Temp1 : constant <type of Pref1> := <Pref1>;
         --          . . .
         --          TempN : constant <type of PrefN> := <PrefN>;
         --       begin
         --          for X in ... loop  --  multiple loops depending on dims
         --             <original source statements with attribute rewrites>
         --          end loop;
         --       end;
         --    end if;

         elsif Is_Array_Iteration (Loop_Stmt) then
            declare
               Array_Nam : constant Entity_Id :=
                             Entity (Name (Iterator_Specification
                              (Iteration_Scheme (Original_Node (Loop_Stmt)))));
               Num_Dims  : constant Pos :=
                             Number_Dimensions (Etype (Array_Nam));
               Cond      : Node_Id := Empty;
               Check     : Node_Id;

            begin
               --  Generate a check which determines whether all dimensions of
               --  the array are non-null.

               for Dim in 1 .. Num_Dims loop
                  Check :=
                    Make_Op_Gt (Loc,
                      Left_Opnd  =>
                        Make_Attribute_Reference (Loc,
                          Prefix         => New_Reference_To (Array_Nam, Loc),
                          Attribute_Name => Name_Length,
                          Expressions    => New_List (
                            Make_Integer_Literal (Loc, Dim))),
                      Right_Opnd =>
                        Make_Integer_Literal (Loc, 0));

                  if No (Cond) then
                     Cond := Check;
                  else
                     Cond :=
                       Make_And_Then (Loc,
                         Left_Opnd  => Cond,
                         Right_Opnd => Check);
                  end if;
               end loop;

               Build_Conditional_Block (Loc,
                 Cond      => Cond,
                 Loop_Stmt => Relocate_Node (Loop_Stmt),
                 If_Stmt   => Result,
                 Blk_Stmt  => Blk);
            end;

         --  For loops are transformed into:

         --    if <Low> <= <High> then
         --       declare
         --          Temp1 : constant <type of Pref1> := <Pref1>;
         --          . . .
         --          TempN : constant <type of PrefN> := <PrefN>;
         --       begin
         --          for <Def_Id> in <Low> .. <High> loop
         --             <original source statements with attribute rewrites>
         --          end loop;
         --       end;
         --    end if;

         elsif Present (Loop_Parameter_Specification (Scheme)) then
            declare
               Loop_Spec : constant Node_Id :=
                             Loop_Parameter_Specification (Scheme);
               Cond      : Node_Id;
               Subt_Def  : Node_Id;

            begin
               Subt_Def := Discrete_Subtype_Definition (Loop_Spec);

               --  When the loop iterates over a subtype indication with a
               --  range, use the low and high bounds of the subtype itself.

               if Nkind (Subt_Def) = N_Subtype_Indication then
                  Subt_Def := Scalar_Range (Etype (Subt_Def));
               end if;

               pragma Assert (Nkind (Subt_Def) = N_Range);

               --  Generate
               --    Low <= High

               Cond :=
                 Make_Op_Le (Loc,
                   Left_Opnd  => New_Copy_Tree (Low_Bound (Subt_Def)),
                   Right_Opnd => New_Copy_Tree (High_Bound (Subt_Def)));

               Build_Conditional_Block (Loc,
                 Cond      => Cond,
                 Loop_Stmt => Relocate_Node (Loop_Stmt),
                 If_Stmt   => Result,
                 Blk_Stmt  => Blk);
            end;
         end if;

         Decls := Declarations (Blk);
      end if;

      --  Step 3: Create a constant to capture the value of the prefix at the
      --  entry point into the loop.

      --  Generate:
      --    Temp : constant <type of Pref> := <Pref>;

      Temp_Id := Make_Temporary (Loc, 'P');

      Temp_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Temp_Id,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Typ, Loc),
          Expression          => Relocate_Node (Pref));
      Append_To (Decls, Temp_Decl);

      --  Step 4: Analyze all bits

      Rewrite (Attr, New_Reference_To (Temp_Id, Loc));

      Installed := Current_Scope = Scope (Loop_Id);

      --  Depending on the pracement of attribute 'Loop_Entry relative to the
      --  associated loop, ensure the proper visibility for analysis.

      if not Installed then
         Push_Scope (Scope (Loop_Id));
      end if;

      --  The analysis of the conditional block takes care of the constant
      --  declaration.

      if Present (Result) then
         Rewrite (Loop_Stmt, Result);
         Analyze (Loop_Stmt);

      --  The conditional block was analyzed when a previous 'Loop_Entry was
      --  expanded. There is no point in reanalyzing the block, simply analyze
      --  the declaration of the constant.

      else
         Analyze (Temp_Decl);
      end if;

      Analyze (Attr);

      if not Installed then
         Pop_Scope;
      end if;
   end Expand_Loop_Entry_Attribute;

   ----------------------------------
   -- Expand_N_Attribute_Reference --
   ----------------------------------

   procedure Expand_N_Attribute_Reference (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Typ   : constant Entity_Id    := Etype (N);
      Btyp  : constant Entity_Id    := Base_Type (Typ);
      Pref  : constant Node_Id      := Prefix (N);
      Ptyp  : constant Entity_Id    := Etype (Pref);
      Exprs : constant List_Id      := Expressions (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));

      procedure Rewrite_Stream_Proc_Call (Pname : Entity_Id);
      --  Rewrites a stream attribute for Read, Write or Output with the
      --  procedure call. Pname is the entity for the procedure to call.

      ------------------------------
      -- Rewrite_Stream_Proc_Call --
      ------------------------------

      procedure Rewrite_Stream_Proc_Call (Pname : Entity_Id) is
         Item       : constant Node_Id   := Next (First (Exprs));
         Formal     : constant Entity_Id := Next_Formal (First_Formal (Pname));
         Formal_Typ : constant Entity_Id := Etype (Formal);
         Is_Written : constant Boolean   := (Ekind (Formal) /= E_In_Parameter);

      begin
         --  The expansion depends on Item, the second actual, which is
         --  the object being streamed in or out.

         --  If the item is a component of a packed array type, and
         --  a conversion is needed on exit, we introduce a temporary to
         --  hold the value, because otherwise the packed reference will
         --  not be properly expanded.

         if Nkind (Item) = N_Indexed_Component
           and then Is_Packed (Base_Type (Etype (Prefix (Item))))
           and then Base_Type (Etype (Item)) /= Base_Type (Formal_Typ)
           and then Is_Written
         then
            declare
               Temp : constant Entity_Id := Make_Temporary (Loc, 'V');
               Decl : Node_Id;
               Assn : Node_Id;

            begin
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition    =>
                     New_Occurrence_Of (Formal_Typ, Loc));
               Set_Etype (Temp, Formal_Typ);

               Assn :=
                 Make_Assignment_Statement (Loc,
                   Name => New_Copy_Tree (Item),
                   Expression =>
                     Unchecked_Convert_To
                       (Etype (Item), New_Occurrence_Of (Temp, Loc)));

               Rewrite (Item, New_Occurrence_Of (Temp, Loc));
               Insert_Actions (N,
                 New_List (
                   Decl,
                   Make_Procedure_Call_Statement (Loc,
                     Name => New_Occurrence_Of (Pname, Loc),
                     Parameter_Associations => Exprs),
                   Assn));

               Rewrite (N, Make_Null_Statement (Loc));
               return;
            end;
         end if;

         --  For the class-wide dispatching cases, and for cases in which
         --  the base type of the second argument matches the base type of
         --  the corresponding formal parameter (that is to say the stream
         --  operation is not inherited), we are all set, and can use the
         --  argument unchanged.

         --  For all other cases we do an unchecked conversion of the second
         --  parameter to the type of the formal of the procedure we are
         --  calling. This deals with the private type cases, and with going
         --  to the root type as required in elementary type case.

         if not Is_Class_Wide_Type (Entity (Pref))
           and then not Is_Class_Wide_Type (Etype (Item))
           and then Base_Type (Etype (Item)) /= Base_Type (Formal_Typ)
         then
            Rewrite (Item,
              Unchecked_Convert_To (Formal_Typ, Relocate_Node (Item)));

            --  For untagged derived types set Assignment_OK, to prevent
            --  copies from being created when the unchecked conversion
            --  is expanded (which would happen in Remove_Side_Effects
            --  if Expand_N_Unchecked_Conversion were allowed to call
            --  Force_Evaluation). The copy could violate Ada semantics
            --  in cases such as an actual that is an out parameter.
            --  Note that this approach is also used in exp_ch7 for calls
            --  to controlled type operations to prevent problems with
            --  actuals wrapped in unchecked conversions.

            if Is_Untagged_Derivation (Etype (Expression (Item))) then
               Set_Assignment_OK (Item);
            end if;
         end if;

         --  The stream operation to call maybe a renaming created by
         --  an attribute definition clause, and may not be frozen yet.
         --  Ensure that it has the necessary extra formals.

         if not Is_Frozen (Pname) then
            Create_Extra_Formals (Pname);
         end if;

         --  And now rewrite the call

         Rewrite (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Pname, Loc),
             Parameter_Associations => Exprs));

         Analyze (N);
      end Rewrite_Stream_Proc_Call;

   --  Start of processing for Expand_N_Attribute_Reference

   begin
      --  Do required validity checking, if enabled. Do not apply check to
      --  output parameters of an Asm instruction, since the value of this
      --  is not set till after the attribute has been elaborated, and do
      --  not apply the check to the arguments of a 'Read or 'Input attribute
      --  reference since the scalar argument is an OUT scalar.

      if Validity_Checks_On and then Validity_Check_Operands
        and then Id /= Attribute_Asm_Output
        and then Id /= Attribute_Read
        and then Id /= Attribute_Input
      then
         declare
            Expr : Node_Id;
         begin
            Expr := First (Expressions (N));
            while Present (Expr) loop
               Ensure_Valid (Expr);
               Next (Expr);
            end loop;
         end;
      end if;

      --  Ada 2005 (AI-318-02): If attribute prefix is a call to a build-in-
      --  place function, then a temporary return object needs to be created
      --  and access to it must be passed to the function. Currently we limit
      --  such functions to those with inherently limited result subtypes, but
      --  eventually we plan to expand the functions that are treated as
      --  build-in-place to include other composite result types.

      if Ada_Version >= Ada_2005
        and then Is_Build_In_Place_Function_Call (Pref)
      then
         Make_Build_In_Place_Call_In_Anonymous_Context (Pref);
      end if;

      --  If prefix is a protected type name, this is a reference to the
      --  current instance of the type. For a component definition, nothing
      --  to do (expansion will occur in the init proc). In other contexts,
      --  rewrite into reference to current instance.

      if Is_Protected_Self_Reference (Pref)
        and then not
          (Nkind_In (Parent (N), N_Index_Or_Discriminant_Constraint,
                                 N_Discriminant_Association)
            and then Nkind (Parent (Parent (Parent (Parent (N))))) =
                                                      N_Component_Definition)

         --  No action needed for these attributes since the current instance
         --  will be rewritten to be the name of the _object parameter
         --  associated with the enclosing protected subprogram (see below).

        and then Id /= Attribute_Access
        and then Id /= Attribute_Unchecked_Access
        and then Id /= Attribute_Unrestricted_Access
      then
         Rewrite (Pref, Concurrent_Ref (Pref));
         Analyze (Pref);
      end if;

      --  Remaining processing depends on specific attribute

      --  Note: individual sections of the following case statement are
      --  allowed to assume there is no code after the case statement, and
      --  are legitimately allowed to execute return statements if they have
      --  nothing more to do.

      case Id is

      --  Attributes related to Ada 2012 iterators (placeholder ???)

      when Attribute_Constant_Indexing    |
           Attribute_Default_Iterator     |
           Attribute_Implicit_Dereference |
           Attribute_Iterator_Element     |
           Attribute_Variable_Indexing    =>
         null;

      --  Internal attributes used to deal with Ada 2012 delayed aspects. These
      --  were already rejected by the parser. Thus they shouldn't appear here.

      when Internal_Attribute_Id =>
         raise Program_Error;

      ------------
      -- Access --
      ------------

      when Attribute_Access              |
           Attribute_Unchecked_Access    |
           Attribute_Unrestricted_Access =>

         Access_Cases : declare
            Ref_Object : constant Node_Id := Get_Referenced_Object (Pref);
            Btyp_DDT   : Entity_Id;

            function Enclosing_Object (N : Node_Id) return Node_Id;
            --  If N denotes a compound name (selected component, indexed
            --  component, or slice), returns the name of the outermost such
            --  enclosing object. Otherwise returns N. If the object is a
            --  renaming, then the renamed object is returned.

            ----------------------
            -- Enclosing_Object --
            ----------------------

            function Enclosing_Object (N : Node_Id) return Node_Id is
               Obj_Name : Node_Id;

            begin
               Obj_Name := N;
               while Nkind_In (Obj_Name, N_Selected_Component,
                                         N_Indexed_Component,
                                         N_Slice)
               loop
                  Obj_Name := Prefix (Obj_Name);
               end loop;

               return Get_Referenced_Object (Obj_Name);
            end Enclosing_Object;

            --  Local declarations

            Enc_Object : constant Node_Id := Enclosing_Object (Ref_Object);

         --  Start of processing for Access_Cases

         begin
            Btyp_DDT := Designated_Type (Btyp);

            --  Handle designated types that come from the limited view

            if Ekind (Btyp_DDT) = E_Incomplete_Type
              and then From_With_Type (Btyp_DDT)
              and then Present (Non_Limited_View (Btyp_DDT))
            then
               Btyp_DDT := Non_Limited_View (Btyp_DDT);

            elsif Is_Class_Wide_Type (Btyp_DDT)
               and then Ekind (Etype (Btyp_DDT)) = E_Incomplete_Type
               and then From_With_Type (Etype (Btyp_DDT))
               and then Present (Non_Limited_View (Etype (Btyp_DDT)))
               and then Present (Class_Wide_Type
                                  (Non_Limited_View (Etype (Btyp_DDT))))
            then
               Btyp_DDT :=
                 Class_Wide_Type (Non_Limited_View (Etype (Btyp_DDT)));
            end if;

            --  In order to improve the text of error messages, the designated
            --  type of access-to-subprogram itypes is set by the semantics as
            --  the associated subprogram entity (see sem_attr). Now we replace
            --  such node with the proper E_Subprogram_Type itype.

            if Id = Attribute_Unrestricted_Access
              and then Is_Subprogram (Directly_Designated_Type (Typ))
            then
               --  The following conditions ensure that this special management
               --  is done only for "Address!(Prim'Unrestricted_Access)" nodes.
               --  At this stage other cases in which the designated type is
               --  still a subprogram (instead of an E_Subprogram_Type) are
               --  wrong because the semantics must have overridden the type of
               --  the node with the type imposed by the context.

               if Nkind (Parent (N)) = N_Unchecked_Type_Conversion
                 and then Etype (Parent (N)) = RTE (RE_Prim_Ptr)
               then
                  Set_Etype (N, RTE (RE_Prim_Ptr));

               else
                  declare
                     Subp       : constant Entity_Id :=
                                    Directly_Designated_Type (Typ);
                     Etyp       : Entity_Id;
                     Extra      : Entity_Id := Empty;
                     New_Formal : Entity_Id;
                     Old_Formal : Entity_Id := First_Formal (Subp);
                     Subp_Typ   : Entity_Id;

                  begin
                     Subp_Typ := Create_Itype (E_Subprogram_Type, N);
                     Set_Etype (Subp_Typ, Etype (Subp));
                     Set_Returns_By_Ref (Subp_Typ, Returns_By_Ref (Subp));

                     if Present (Old_Formal) then
                        New_Formal := New_Copy (Old_Formal);
                        Set_First_Entity (Subp_Typ, New_Formal);

                        loop
                           Set_Scope (New_Formal, Subp_Typ);
                           Etyp := Etype (New_Formal);

                           --  Handle itypes. There is no need to duplicate
                           --  here the itypes associated with record types
                           --  (i.e the implicit full view of private types).

                           if Is_Itype (Etyp)
                             and then Ekind (Base_Type (Etyp)) /= E_Record_Type
                           then
                              Extra := New_Copy (Etyp);
                              Set_Parent (Extra, New_Formal);
                              Set_Etype (New_Formal, Extra);
                              Set_Scope (Extra, Subp_Typ);
                           end if;

                           Extra := New_Formal;
                           Next_Formal (Old_Formal);
                           exit when No (Old_Formal);

                           Set_Next_Entity (New_Formal,
                             New_Copy (Old_Formal));
                           Next_Entity (New_Formal);
                        end loop;

                        Set_Next_Entity (New_Formal, Empty);
                        Set_Last_Entity (Subp_Typ, Extra);
                     end if;

                     --  Now that the explicit formals have been duplicated,
                     --  any extra formals needed by the subprogram must be
                     --  created.

                     if Present (Extra) then
                        Set_Extra_Formal (Extra, Empty);
                     end if;

                     Create_Extra_Formals (Subp_Typ);
                     Set_Directly_Designated_Type (Typ, Subp_Typ);
                  end;
               end if;
            end if;

            if Is_Access_Protected_Subprogram_Type (Btyp) then
               Expand_Access_To_Protected_Op (N, Pref, Typ);

            --  If prefix is a type name, this is a reference to the current
            --  instance of the type, within its initialization procedure.

            elsif Is_Entity_Name (Pref)
              and then Is_Type (Entity (Pref))
            then
               declare
                  Par    : Node_Id;
                  Formal : Entity_Id;

               begin
                  --  If the current instance name denotes a task type, then
                  --  the access attribute is rewritten to be the name of the
                  --  "_task" parameter associated with the task type's task
                  --  procedure. An unchecked conversion is applied to ensure
                  --  a type match in cases of expander-generated calls (e.g.
                  --  init procs).

                  if Is_Task_Type (Entity (Pref)) then
                     Formal :=
                       First_Entity (Get_Task_Body_Procedure (Entity (Pref)));
                     while Present (Formal) loop
                        exit when Chars (Formal) = Name_uTask;
                        Next_Entity (Formal);
                     end loop;

                     pragma Assert (Present (Formal));

                     Rewrite (N,
                       Unchecked_Convert_To (Typ,
                         New_Occurrence_Of (Formal, Loc)));
                     Set_Etype (N, Typ);

                  elsif Is_Protected_Type (Entity (Pref)) then

                     --  No action needed for current instance located in a
                     --  component definition (expansion will occur in the
                     --  init proc)

                     if Is_Protected_Type (Current_Scope) then
                        null;

                     --  If the current instance reference is located in a
                     --  protected subprogram or entry then rewrite the access
                     --  attribute to be the name of the "_object" parameter.
                     --  An unchecked conversion is applied to ensure a type
                     --  match in cases of expander-generated calls (e.g. init
                     --  procs).

                     --  The code may be nested in a block, so find enclosing
                     --  scope that is a protected operation.

                     else
                        declare
                           Subp : Entity_Id;

                        begin
                           Subp := Current_Scope;
                           while Ekind_In (Subp, E_Loop, E_Block) loop
                              Subp := Scope (Subp);
                           end loop;

                           Formal :=
                             First_Entity
                               (Protected_Body_Subprogram (Subp));

                           --  For a protected subprogram the _Object parameter
                           --  is the protected record, so we create an access
                           --  to it. The _Object parameter of an entry is an
                           --  address.

                           if Ekind (Subp) = E_Entry then
                              Rewrite (N,
                                Unchecked_Convert_To (Typ,
                                  New_Occurrence_Of (Formal, Loc)));
                              Set_Etype (N, Typ);

                           else
                              Rewrite (N,
                                Unchecked_Convert_To (Typ,
                                  Make_Attribute_Reference (Loc,
                                    Attribute_Name => Name_Unrestricted_Access,
                                    Prefix         =>
                                      New_Occurrence_Of (Formal, Loc))));
                              Analyze_And_Resolve (N);
                           end if;
                        end;
                     end if;

                  --  The expression must appear in a default expression,
                  --  (which in the initialization procedure is the right-hand
                  --  side of an assignment), and not in a discriminant
                  --  constraint.

                  else
                     Par := Parent (N);
                     while Present (Par) loop
                        exit when Nkind (Par) = N_Assignment_Statement;

                        if Nkind (Par) = N_Component_Declaration then
                           return;
                        end if;

                        Par := Parent (Par);
                     end loop;

                     if Present (Par) then
                        Rewrite (N,
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uInit),
                            Attribute_Name  => Attribute_Name (N)));

                        Analyze_And_Resolve (N, Typ);
                     end if;
                  end if;
               end;

            --  If the prefix of an Access attribute is a dereference of an
            --  access parameter (or a renaming of such a dereference, or a
            --  subcomponent of such a dereference) and the context is a
            --  general access type (including the type of an object or
            --  component with an access_definition, but not the anonymous
            --  type of an access parameter or access discriminant), then
            --  apply an accessibility check to the access parameter. We used
            --  to rewrite the access parameter as a type conversion, but that
            --  could only be done if the immediate prefix of the Access
            --  attribute was the dereference, and didn't handle cases where
            --  the attribute is applied to a subcomponent of the dereference,
            --  since there's generally no available, appropriate access type
            --  to convert to in that case. The attribute is passed as the
            --  point to insert the check, because the access parameter may
            --  come from a renaming, possibly in a different scope, and the
            --  check must be associated with the attribute itself.

            elsif Id = Attribute_Access
              and then Nkind (Enc_Object) = N_Explicit_Dereference
              and then Is_Entity_Name (Prefix (Enc_Object))
              and then (Ekind (Btyp) = E_General_Access_Type
                         or else Is_Local_Anonymous_Access (Btyp))
              and then Ekind (Entity (Prefix (Enc_Object))) in Formal_Kind
              and then Ekind (Etype (Entity (Prefix (Enc_Object))))
                         = E_Anonymous_Access_Type
              and then Present (Extra_Accessibility
                                (Entity (Prefix (Enc_Object))))
            then
               Apply_Accessibility_Check (Prefix (Enc_Object), Typ, N);

            --  Ada 2005 (AI-251): If the designated type is an interface we
            --  add an implicit conversion to force the displacement of the
            --  pointer to reference the secondary dispatch table.

            elsif Is_Interface (Btyp_DDT)
              and then (Comes_From_Source (N)
                         or else Comes_From_Source (Ref_Object)
                         or else (Nkind (Ref_Object) in N_Has_Chars
                                   and then Chars (Ref_Object) = Name_uInit))
            then
               if Nkind (Ref_Object) /= N_Explicit_Dereference then

                  --  No implicit conversion required if types match, or if
                  --  the prefix is the class_wide_type of the interface. In
                  --  either case passing an object of the interface type has
                  --  already set the pointer correctly.

                  if Btyp_DDT = Etype (Ref_Object)
                    or else (Is_Class_Wide_Type (Etype (Ref_Object))
                              and then
                               Class_Wide_Type (Btyp_DDT) = Etype (Ref_Object))
                  then
                     null;

                  else
                     Rewrite (Prefix (N),
                       Convert_To (Btyp_DDT,
                         New_Copy_Tree (Prefix (N))));

                     Analyze_And_Resolve (Prefix (N), Btyp_DDT);
                  end if;

               --  When the object is an explicit dereference, convert the
               --  dereference's prefix.

               else
                  declare
                     Obj_DDT : constant Entity_Id :=
                                 Base_Type
                                   (Directly_Designated_Type
                                     (Etype (Prefix (Ref_Object))));
                  begin
                     --  No implicit conversion required if designated types
                     --  match, or if we have an unrestricted access.

                     if Obj_DDT /= Btyp_DDT
                       and then Id /= Attribute_Unrestricted_Access
                       and then not (Is_Class_Wide_Type (Obj_DDT)
                                      and then Etype (Obj_DDT) = Btyp_DDT)
                     then
                        Rewrite (N,
                          Convert_To (Typ,
                            New_Copy_Tree (Prefix (Ref_Object))));
                        Analyze_And_Resolve (N, Typ);
                     end if;
                  end;
               end if;
            end if;
         end Access_Cases;

      --------------
      -- Adjacent --
      --------------

      --  Transforms 'Adjacent into a call to the floating-point attribute
      --  function Adjacent in Fat_xxx (where xxx is the root type)

      when Attribute_Adjacent =>
         Expand_Fpt_Attribute_RR (N);

      -------------
      -- Address --
      -------------

      when Attribute_Address => Address : declare
         Task_Proc : Entity_Id;

      begin
         --  If the prefix is a task or a task type, the useful address is that
         --  of the procedure for the task body, i.e. the actual program unit.
         --  We replace the original entity with that of the procedure.

         if Is_Entity_Name (Pref)
           and then Is_Task_Type (Entity (Pref))
         then
            Task_Proc := Next_Entity (Root_Type (Ptyp));

            while Present (Task_Proc) loop
               exit when Ekind (Task_Proc) = E_Procedure
                 and then Etype (First_Formal (Task_Proc)) =
                                  Corresponding_Record_Type (Ptyp);
               Next_Entity (Task_Proc);
            end loop;

            if Present (Task_Proc) then
               Set_Entity (Pref, Task_Proc);
               Set_Etype  (Pref, Etype (Task_Proc));
            end if;

         --  Similarly, the address of a protected operation is the address
         --  of the corresponding protected body, regardless of the protected
         --  object from which it is selected.

         elsif Nkind (Pref) = N_Selected_Component
           and then Is_Subprogram (Entity (Selector_Name (Pref)))
           and then Is_Protected_Type (Scope (Entity (Selector_Name (Pref))))
         then
            Rewrite (Pref,
              New_Occurrence_Of (
                External_Subprogram (Entity (Selector_Name (Pref))), Loc));

         elsif Nkind (Pref) = N_Explicit_Dereference
           and then Ekind (Ptyp) = E_Subprogram_Type
           and then Convention (Ptyp) = Convention_Protected
         then
            --  The prefix is be a dereference of an access_to_protected_
            --  subprogram. The desired address is the second component of
            --  the record that represents the access.

            declare
               Addr : constant Entity_Id := Etype (N);
               Ptr  : constant Node_Id   := Prefix (Pref);
               T    : constant Entity_Id :=
                        Equivalent_Type (Base_Type (Etype (Ptr)));

            begin
               Rewrite (N,
                 Unchecked_Convert_To (Addr,
                   Make_Selected_Component (Loc,
                     Prefix => Unchecked_Convert_To (T, Ptr),
                     Selector_Name => New_Occurrence_Of (
                       Next_Entity (First_Entity (T)), Loc))));

               Analyze_And_Resolve (N, Addr);
            end;

         --  Ada 2005 (AI-251): Class-wide interface objects are always
         --  "displaced" to reference the tag associated with the interface
         --  type. In order to obtain the real address of such objects we
         --  generate a call to a run-time subprogram that returns the base
         --  address of the object.

         --  This processing is not needed in the VM case, where dispatching
         --  issues are taken care of by the virtual machine.

         elsif Is_Class_Wide_Type (Ptyp)
           and then Is_Interface (Ptyp)
           and then Tagged_Type_Expansion
           and then not (Nkind (Pref) in N_Has_Entity
                          and then Is_Subprogram (Entity (Pref)))
         then
            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (RTE (RE_Base_Address), Loc),
                Parameter_Associations => New_List (
                  Relocate_Node (N))));
            Analyze (N);
            return;
         end if;

         --  Deal with packed array reference, other cases are handled by
         --  the back end.

         if Involves_Packed_Array_Reference (Pref) then
            Expand_Packed_Address_Reference (N);
         end if;
      end Address;

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment => Alignment : declare
         New_Node : Node_Id;

      begin
         --  For class-wide types, X'Class'Alignment is transformed into a
         --  direct reference to the Alignment of the class type, so that the
         --  back end does not have to deal with the X'Class'Alignment
         --  reference.

         if Is_Entity_Name (Pref)
           and then Is_Class_Wide_Type (Entity (Pref))
         then
            Rewrite (Prefix (N), New_Occurrence_Of (Entity (Pref), Loc));
            return;

         --  For x'Alignment applied to an object of a class wide type,
         --  transform X'Alignment into a call to the predefined primitive
         --  operation _Alignment applied to X.

         elsif Is_Class_Wide_Type (Ptyp) then
            New_Node :=
              Make_Attribute_Reference (Loc,
                Prefix         => Pref,
                Attribute_Name => Name_Tag);

            if VM_Target = No_VM then
               New_Node := Build_Get_Alignment (Loc, New_Node);
            else
               New_Node :=
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (RTE (RE_Get_Alignment), Loc),
                   Parameter_Associations => New_List (New_Node));
            end if;

            --  Case where the context is a specific integer type with which
            --  the original attribute was compatible. The function has a
            --  specific type as well, so to preserve the compatibility we
            --  must convert explicitly.

            if Typ /= Standard_Integer then
               New_Node := Convert_To (Typ, New_Node);
            end if;

            Rewrite (N, New_Node);
            Analyze_And_Resolve (N, Typ);
            return;

         --  For all other cases, we just have to deal with the case of
         --  the fact that the result can be universal.

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Alignment;

      ---------------
      -- AST_Entry --
      ---------------

      when Attribute_AST_Entry => AST_Entry : declare
         Ttyp : Entity_Id;
         T_Id : Node_Id;
         Eent : Entity_Id;

         Entry_Ref : Node_Id;
         --  The reference to the entry or entry family

         Index : Node_Id;
         --  The index expression for an entry family reference, or
         --  the Empty if Entry_Ref references a simple entry.

      begin
         if Nkind (Pref) = N_Indexed_Component then
            Entry_Ref := Prefix (Pref);
            Index := First (Expressions (Pref));
         else
            Entry_Ref := Pref;
            Index := Empty;
         end if;

         --  Get expression for Task_Id and the entry entity

         if Nkind (Entry_Ref) = N_Selected_Component then
            T_Id :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Identity,
                Prefix         => Prefix (Entry_Ref));

            Ttyp := Etype (Prefix (Entry_Ref));
            Eent := Entity (Selector_Name (Entry_Ref));

         else
            T_Id :=
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (RTE (RE_Current_Task), Loc));

            Eent  := Entity (Entry_Ref);

            --  We have to find the enclosing task to get the task type
            --  There must be one, since we already validated this earlier

            Ttyp := Current_Scope;
            while not Is_Task_Type (Ttyp) loop
               Ttyp := Scope (Ttyp);
            end loop;
         end if;

         --  Now rewrite the attribute with a call to Create_AST_Handler

         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (RTE (RE_Create_AST_Handler), Loc),
             Parameter_Associations => New_List (
               T_Id,
               Entry_Index_Expression (Loc, Eent, Index, Ttyp))));

         Analyze_And_Resolve (N, RTE (RE_AST_Handler));
      end AST_Entry;

      ---------
      -- Bit --
      ---------

      --  We compute this if a packed array reference was present, otherwise we
      --  leave the computation up to the back end.

      when Attribute_Bit =>
         if Involves_Packed_Array_Reference (Pref) then
            Expand_Packed_Bit_Reference (N);
         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;

      ------------------
      -- Bit_Position --
      ------------------

      --  We compute this if a component clause was present, otherwise we leave
      --  the computation up to the back end, since we don't know what layout
      --  will be chosen.

      --  Note that the attribute can apply to a naked record component
      --  in generated code (i.e. the prefix is an identifier that
      --  references the component or discriminant entity).

      when Attribute_Bit_Position => Bit_Position : declare
         CE : Entity_Id;

      begin
         if Nkind (Pref) = N_Identifier then
            CE := Entity (Pref);
         else
            CE := Entity (Selector_Name (Pref));
         end if;

         if Known_Static_Component_Bit_Offset (CE) then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Intval => Component_Bit_Offset (CE)));
            Analyze_And_Resolve (N, Typ);

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Bit_Position;

      ------------------
      -- Body_Version --
      ------------------

      --  A reference to P'Body_Version or P'Version is expanded to

      --     Vnn : Unsigned;
      --     pragma Import (C, Vnn, "uuuuT");
      --     ...
      --     Get_Version_String (Vnn)

      --  where uuuu is the unit name (dots replaced by double underscore)
      --  and T is B for the cases of Body_Version, or Version applied to a
      --  subprogram acting as its own spec, and S for Version applied to a
      --  subprogram spec or package. This sequence of code references the
      --  unsigned constant created in the main program by the binder.

      --  A special exception occurs for Standard, where the string returned
      --  is a copy of the library string in gnatvsn.ads.

      when Attribute_Body_Version | Attribute_Version => Version : declare
         E    : constant Entity_Id := Make_Temporary (Loc, 'V');
         Pent : Entity_Id;
         S    : String_Id;

      begin
         --  If not library unit, get to containing library unit

         Pent := Entity (Pref);
         while Pent /= Standard_Standard
           and then Scope (Pent) /= Standard_Standard
           and then not Is_Child_Unit (Pent)
         loop
            Pent := Scope (Pent);
         end loop;

         --  Special case Standard and Standard.ASCII

         if Pent = Standard_Standard or else Pent = Standard_ASCII then
            Rewrite (N,
              Make_String_Literal (Loc,
                Strval => Verbose_Library_Version));

         --  All other cases

         else
            --  Build required string constant

            Get_Name_String (Get_Unit_Name (Pent));

            Start_String;
            for J in 1 .. Name_Len - 2 loop
               if Name_Buffer (J) = '.' then
                  Store_String_Chars ("__");
               else
                  Store_String_Char (Get_Char_Code (Name_Buffer (J)));
               end if;
            end loop;

            --  Case of subprogram acting as its own spec, always use body

            if Nkind (Declaration_Node (Pent)) in N_Subprogram_Specification
              and then Nkind (Parent (Declaration_Node (Pent))) =
                                                          N_Subprogram_Body
              and then Acts_As_Spec (Parent (Declaration_Node (Pent)))
            then
               Store_String_Chars ("B");

            --  Case of no body present, always use spec

            elsif not Unit_Requires_Body (Pent) then
               Store_String_Chars ("S");

            --  Otherwise use B for Body_Version, S for spec

            elsif Id = Attribute_Body_Version then
               Store_String_Chars ("B");
            else
               Store_String_Chars ("S");
            end if;

            S := End_String;
            Lib.Version_Referenced (S);

            --  Insert the object declaration

            Insert_Actions (N, New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => E,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Unsigned), Loc))));

            --  Set entity as imported with correct external name

            Set_Is_Imported (E);
            Set_Interface_Name (E, Make_String_Literal (Loc, S));

            --  Set entity as internal to ensure proper Sprint output of its
            --  implicit importation.

            Set_Is_Internal (E);

            --  And now rewrite original reference

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (RTE (RE_Get_Version_String), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (E, Loc))));
         end if;

         Analyze_And_Resolve (N, RTE (RE_Version_String));
      end Version;

      -------------
      -- Ceiling --
      -------------

      --  Transforms 'Ceiling into a call to the floating-point attribute
      --  function Ceiling in Fat_xxx (where xxx is the root type)

      when Attribute_Ceiling =>
         Expand_Fpt_Attribute_R (N);

      --------------
      -- Callable --
      --------------

      --  Transforms 'Callable attribute into a call to the Callable function

      when Attribute_Callable => Callable :
      begin
         --  We have an object of a task interface class-wide type as a prefix
         --  to Callable. Generate:
         --    callable (Task_Id (Pref._disp_get_task_id));

         if Ada_Version >= Ada_2005
           and then Ekind (Ptyp) = E_Class_Wide_Type
           and then Is_Interface (Ptyp)
           and then Is_Task_Interface (Ptyp)
         then
            Rewrite (N,
              Make_Function_Call (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Callable), Loc),
                Parameter_Associations => New_List (
                  Make_Unchecked_Type_Conversion (Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RO_ST_Task_Id), Loc),
                    Expression =>
                      Make_Selected_Component (Loc,
                        Prefix =>
                          New_Copy_Tree (Pref),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_uDisp_Get_Task_Id))))));

         else
            Rewrite (N,
              Build_Call_With_Task (Pref, RTE (RE_Callable)));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
      end Callable;

      ------------
      -- Caller --
      ------------

      --  Transforms 'Caller attribute into a call to either the
      --  Task_Entry_Caller or the Protected_Entry_Caller function.

      when Attribute_Caller => Caller : declare
         Id_Kind    : constant Entity_Id := RTE (RO_AT_Task_Id);
         Ent        : constant Entity_Id := Entity (Pref);
         Conctype   : constant Entity_Id := Scope (Ent);
         Nest_Depth : Integer := 0;
         Name       : Node_Id;
         S          : Entity_Id;

      begin
         --  Protected case

         if Is_Protected_Type (Conctype) then
            case Corresponding_Runtime_Package (Conctype) is
               when System_Tasking_Protected_Objects_Entries =>
                  Name :=
                    New_Reference_To
                      (RTE (RE_Protected_Entry_Caller), Loc);

               when System_Tasking_Protected_Objects_Single_Entry =>
                  Name :=
                    New_Reference_To
                      (RTE (RE_Protected_Single_Entry_Caller), Loc);

               when others =>
                  raise Program_Error;
            end case;

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind,
                Make_Function_Call (Loc,
                  Name => Name,
                  Parameter_Associations => New_List (
                    New_Reference_To
                      (Find_Protection_Object (Current_Scope), Loc)))));

         --  Task case

         else
            --  Determine the nesting depth of the E'Caller attribute, that
            --  is, how many accept statements are nested within the accept
            --  statement for E at the point of E'Caller. The runtime uses
            --  this depth to find the specified entry call.

            for J in reverse 0 .. Scope_Stack.Last loop
               S := Scope_Stack.Table (J).Entity;

               --  We should not reach the scope of the entry, as it should
               --  already have been checked in Sem_Attr that this attribute
               --  reference is within a matching accept statement.

               pragma Assert (S /= Conctype);

               if S = Ent then
                  exit;

               elsif Is_Entry (S) then
                  Nest_Depth := Nest_Depth + 1;
               end if;
            end loop;

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind,
                Make_Function_Call (Loc,
                  Name =>
                    New_Reference_To (RTE (RE_Task_Entry_Caller), Loc),
                  Parameter_Associations => New_List (
                    Make_Integer_Literal (Loc,
                      Intval => Int (Nest_Depth))))));
         end if;

         Analyze_And_Resolve (N, Id_Kind);
      end Caller;

      -------------
      -- Compose --
      -------------

      --  Transforms 'Compose into a call to the floating-point attribute
      --  function Compose in Fat_xxx (where xxx is the root type)

      --  Note: we strictly should have special code here to deal with the
      --  case of absurdly negative arguments (less than Integer'First)
      --  which will return a (signed) zero value, but it hardly seems
      --  worth the effort. Absurdly large positive arguments will raise
      --  constraint error which is fine.

      when Attribute_Compose =>
         Expand_Fpt_Attribute_RI (N);

      -----------------
      -- Constrained --
      -----------------

      when Attribute_Constrained => Constrained : declare
         Formal_Ent : constant Entity_Id := Param_Entity (Pref);

         function Is_Constrained_Aliased_View (Obj : Node_Id) return Boolean;
         --  Ada 2005 (AI-363): Returns True if the object name Obj denotes a
         --  view of an aliased object whose subtype is constrained.

         ---------------------------------
         -- Is_Constrained_Aliased_View --
         ---------------------------------

         function Is_Constrained_Aliased_View (Obj : Node_Id) return Boolean is
            E : Entity_Id;

         begin
            if Is_Entity_Name (Obj) then
               E := Entity (Obj);

               if Present (Renamed_Object (E)) then
                  return Is_Constrained_Aliased_View (Renamed_Object (E));
               else
                  return Is_Aliased (E) and then Is_Constrained (Etype (E));
               end if;

            else
               return Is_Aliased_View (Obj)
                        and then
                      (Is_Constrained (Etype (Obj))
                         or else
                           (Nkind (Obj) = N_Explicit_Dereference
                              and then
                                not Object_Type_Has_Constrained_Partial_View
                                      (Typ  => Base_Type (Etype (Obj)),
                                       Scop => Current_Scope)));
            end if;
         end Is_Constrained_Aliased_View;

      --  Start of processing for Constrained

      begin
         --  Reference to a parameter where the value is passed as an extra
         --  actual, corresponding to the extra formal referenced by the
         --  Extra_Constrained field of the corresponding formal. If this
         --  is an entry in-parameter, it is replaced by a constant renaming
         --  for which Extra_Constrained is never created.

         if Present (Formal_Ent)
           and then Ekind (Formal_Ent) /= E_Constant
           and then Present (Extra_Constrained (Formal_Ent))
         then
            Rewrite (N,
              New_Occurrence_Of
                (Extra_Constrained (Formal_Ent), Sloc (N)));

         --  For variables with a Extra_Constrained field, we use the
         --  corresponding entity.

         elsif Nkind (Pref) = N_Identifier
           and then Ekind (Entity (Pref)) = E_Variable
           and then Present (Extra_Constrained (Entity (Pref)))
         then
            Rewrite (N,
              New_Occurrence_Of
                (Extra_Constrained (Entity (Pref)), Sloc (N)));

         --  For all other entity names, we can tell at compile time

         elsif Is_Entity_Name (Pref) then
            declare
               Ent : constant Entity_Id   := Entity (Pref);
               Res : Boolean;

            begin
               --  (RM J.4) obsolescent cases

               if Is_Type (Ent) then

                  --  Private type

                  if Is_Private_Type (Ent) then
                     Res := not Has_Discriminants (Ent)
                              or else Is_Constrained (Ent);

                  --  It not a private type, must be a generic actual type
                  --  that corresponded to a private type. We know that this
                  --  correspondence holds, since otherwise the reference
                  --  within the generic template would have been illegal.

                  else
                     if Is_Composite_Type (Underlying_Type (Ent)) then
                        Res := Is_Constrained (Ent);
                     else
                        Res := True;
                     end if;
                  end if;

               --  If the prefix is not a variable or is aliased, then
               --  definitely true; if it's a formal parameter without an
               --  associated extra formal, then treat it as constrained.

               --  Ada 2005 (AI-363): An aliased prefix must be known to be
               --  constrained in order to set the attribute to True.

               elsif not Is_Variable (Pref)
                 or else Present (Formal_Ent)
                 or else (Ada_Version < Ada_2005
                            and then Is_Aliased_View (Pref))
                 or else (Ada_Version >= Ada_2005
                            and then Is_Constrained_Aliased_View (Pref))
               then
                  Res := True;

               --  Variable case, look at type to see if it is constrained.
               --  Note that the one case where this is not accurate (the
               --  procedure formal case), has been handled above.

               --  We use the Underlying_Type here (and below) in case the
               --  type is private without discriminants, but the full type
               --  has discriminants. This case is illegal, but we generate it
               --  internally for passing to the Extra_Constrained parameter.

               else
                  --  In Ada 2012, test for case of a limited tagged type, in
                  --  which case the attribute is always required to return
                  --  True. The underlying type is tested, to make sure we also
                  --  return True for cases where there is an unconstrained
                  --  object with an untagged limited partial view which has
                  --  defaulted discriminants (such objects always produce a
                  --  False in earlier versions of Ada). (Ada 2012: AI05-0214)

                  Res := Is_Constrained (Underlying_Type (Etype (Ent)))
                           or else
                             (Ada_Version >= Ada_2012
                               and then Is_Tagged_Type (Underlying_Type (Ptyp))
                               and then Is_Limited_Type (Ptyp));
               end if;

               Rewrite (N, New_Reference_To (Boolean_Literals (Res), Loc));
            end;

         --  Prefix is not an entity name. These are also cases where we can
         --  always tell at compile time by looking at the form and type of the
         --  prefix. If an explicit dereference of an object with constrained
         --  partial view, this is unconstrained (Ada 2005: AI95-0363). If the
         --  underlying type is a limited tagged type, then Constrained is
         --  required to always return True (Ada 2012: AI05-0214).

         else
            Rewrite (N,
              New_Reference_To (
                Boolean_Literals (
                  not Is_Variable (Pref)
                    or else
                     (Nkind (Pref) = N_Explicit_Dereference
                       and then
                         not Object_Type_Has_Constrained_Partial_View
                               (Typ  => Base_Type (Ptyp),
                                Scop => Current_Scope))
                    or else Is_Constrained (Underlying_Type (Ptyp))
                    or else (Ada_Version >= Ada_2012
                              and then Is_Tagged_Type (Underlying_Type (Ptyp))
                              and then Is_Limited_Type (Ptyp))),
                Loc));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
      end Constrained;

      ---------------
      -- Copy_Sign --
      ---------------

      --  Transforms 'Copy_Sign into a call to the floating-point attribute
      --  function Copy_Sign in Fat_xxx (where xxx is the root type)

      when Attribute_Copy_Sign =>
         Expand_Fpt_Attribute_RR (N);

      -----------
      -- Count --
      -----------

      --  Transforms 'Count attribute into a call to the Count function

      when Attribute_Count => Count : declare
         Call     : Node_Id;
         Conctyp  : Entity_Id;
         Entnam   : Node_Id;
         Entry_Id : Entity_Id;
         Index    : Node_Id;
         Name     : Node_Id;

      begin
         --  If the prefix is a member of an entry family, retrieve both
         --  entry name and index. For a simple entry there is no index.

         if Nkind (Pref) = N_Indexed_Component then
            Entnam := Prefix (Pref);
            Index := First (Expressions (Pref));
         else
            Entnam := Pref;
            Index := Empty;
         end if;

         Entry_Id := Entity (Entnam);

         --  Find the concurrent type in which this attribute is referenced
         --  (there had better be one).

         Conctyp := Current_Scope;
         while not Is_Concurrent_Type (Conctyp) loop
            Conctyp := Scope (Conctyp);
         end loop;

         --  Protected case

         if Is_Protected_Type (Conctyp) then
            case Corresponding_Runtime_Package (Conctyp) is
               when System_Tasking_Protected_Objects_Entries =>
                  Name := New_Reference_To (RTE (RE_Protected_Count), Loc);

                  Call :=
                    Make_Function_Call (Loc,
                      Name => Name,
                      Parameter_Associations => New_List (
                        New_Reference_To
                          (Find_Protection_Object (Current_Scope), Loc),
                        Entry_Index_Expression
                          (Loc, Entry_Id, Index, Scope (Entry_Id))));

               when System_Tasking_Protected_Objects_Single_Entry =>
                  Name :=
                    New_Reference_To (RTE (RE_Protected_Count_Entry), Loc);

                  Call :=
                    Make_Function_Call (Loc,
                      Name => Name,
                      Parameter_Associations => New_List (
                        New_Reference_To
                          (Find_Protection_Object (Current_Scope), Loc)));

               when others =>
                  raise Program_Error;
            end case;

         --  Task case

         else
            Call :=
              Make_Function_Call (Loc,
                Name => New_Reference_To (RTE (RE_Task_Count), Loc),
                Parameter_Associations => New_List (
                  Entry_Index_Expression (Loc,
                    Entry_Id, Index, Scope (Entry_Id))));
         end if;

         --  The call returns type Natural but the context is universal integer
         --  so any integer type is allowed. The attribute was already resolved
         --  so its Etype is the required result type. If the base type of the
         --  context type is other than Standard.Integer we put in a conversion
         --  to the required type. This can be a normal typed conversion since
         --  both input and output types of the conversion are integer types

         if Base_Type (Typ) /= Base_Type (Standard_Integer) then
            Rewrite (N, Convert_To (Typ, Call));
         else
            Rewrite (N, Call);
         end if;

         Analyze_And_Resolve (N, Typ);
      end Count;

      ---------------------
      -- Descriptor_Size --
      ---------------------

      when Attribute_Descriptor_Size =>

         --  Attribute Descriptor_Size is handled by the back end when applied
         --  to an unconstrained array type.

         if Is_Array_Type (Ptyp)
           and then not Is_Constrained (Ptyp)
         then
            Apply_Universal_Integer_Attribute_Checks (N);

         --  For any other type, the descriptor size is 0 because there is no
         --  actual descriptor, but the result is not formally static.

         else
            Rewrite (N, Make_Integer_Literal (Loc, 0));
            Analyze (N);
            Set_Is_Static_Expression (N, False);
         end if;

      ---------------
      -- Elab_Body --
      ---------------

      --  This processing is shared by Elab_Spec

      --  What we do is to insert the following declarations

      --     procedure tnn;
      --     pragma Import (C, enn, "name___elabb/s");

      --  and then the Elab_Body/Spec attribute is replaced by a reference
      --  to this defining identifier.

      when Attribute_Elab_Body      |
           Attribute_Elab_Spec      =>

         --  Leave attribute unexpanded in CodePeer mode: the gnat2scil
         --  back-end knows how to handle these attributes directly.

         if CodePeer_Mode then
            return;
         end if;

         Elab_Body : declare
            Ent  : constant Entity_Id := Make_Temporary (Loc, 'E');
            Str  : String_Id;
            Lang : Node_Id;

            procedure Make_Elab_String (Nod : Node_Id);
            --  Given Nod, an identifier, or a selected component, put the
            --  image into the current string literal, with double underline
            --  between components.

            ----------------------
            -- Make_Elab_String --
            ----------------------

            procedure Make_Elab_String (Nod : Node_Id) is
            begin
               if Nkind (Nod) = N_Selected_Component then
                  Make_Elab_String (Prefix (Nod));

                  case VM_Target is
                     when JVM_Target =>
                        Store_String_Char ('$');
                     when CLI_Target =>
                        Store_String_Char ('.');
                     when No_VM =>
                        Store_String_Char ('_');
                        Store_String_Char ('_');
                  end case;

                  Get_Name_String (Chars (Selector_Name (Nod)));

               else
                  pragma Assert (Nkind (Nod) = N_Identifier);
                  Get_Name_String (Chars (Nod));
               end if;

               Store_String_Chars (Name_Buffer (1 .. Name_Len));
            end Make_Elab_String;

         --  Start of processing for Elab_Body/Elab_Spec

         begin
            --  First we need to prepare the string literal for the name of
            --  the elaboration routine to be referenced.

            Start_String;
            Make_Elab_String (Pref);

            if VM_Target = No_VM then
               Store_String_Chars ("___elab");
               Lang := Make_Identifier (Loc, Name_C);
            else
               Store_String_Chars ("._elab");
               Lang := Make_Identifier (Loc, Name_Ada);
            end if;

            if Id = Attribute_Elab_Body then
               Store_String_Char ('b');
            else
               Store_String_Char ('s');
            end if;

            Str := End_String;

            Insert_Actions (N, New_List (
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name => Ent)),

              Make_Pragma (Loc,
                Chars                        => Name_Import,
                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Loc, Expression => Lang),

                  Make_Pragma_Argument_Association (Loc,
                    Expression => Make_Identifier (Loc, Chars (Ent))),

                  Make_Pragma_Argument_Association (Loc,
                    Expression => Make_String_Literal (Loc, Str))))));

            Set_Entity (N, Ent);
            Rewrite (N, New_Occurrence_Of (Ent, Loc));
         end Elab_Body;

      --------------------
      -- Elab_Subp_Body --
      --------------------

      --  Always ignored. In CodePeer mode, gnat2scil knows how to handle
      --  this attribute directly, and if we are not in CodePeer mode it is
      --  entirely ignored ???

      when Attribute_Elab_Subp_Body =>
         return;

      ----------------
      -- Elaborated --
      ----------------

      --  Elaborated is always True for preelaborated units, predefined units,
      --  pure units and units which have Elaborate_Body pragmas. These units
      --  have no elaboration entity.

      --  Note: The Elaborated attribute is never passed to the back end

      when Attribute_Elaborated => Elaborated : declare
         Ent : constant Entity_Id := Entity (Pref);

      begin
         if Present (Elaboration_Entity (Ent)) then
            Rewrite (N,
              Make_Op_Ne (Loc,
                Left_Opnd =>
                  New_Occurrence_Of (Elaboration_Entity (Ent), Loc),
                Right_Opnd =>
                  Make_Integer_Literal (Loc, Uint_0)));
            Analyze_And_Resolve (N, Typ);
         else
            Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
         end if;
      end Elaborated;

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep => Enum_Rep :
      begin
         --  X'Enum_Rep (Y) expands to

         --    target-type (Y)

         --  This is simply a direct conversion from the enumeration type to
         --  the target integer type, which is treated by the back end as a
         --  normal integer conversion, treating the enumeration type as an
         --  integer, which is exactly what we want! We set Conversion_OK to
         --  make sure that the analyzer does not complain about what otherwise
         --  might be an illegal conversion.

         if Is_Non_Empty_List (Exprs) then
            Rewrite (N,
              OK_Convert_To (Typ, Relocate_Node (First (Exprs))));

         --  X'Enum_Rep where X is an enumeration literal is replaced by
         --  the literal value.

         elsif Ekind (Entity (Pref)) = E_Enumeration_Literal then
            Rewrite (N,
              Make_Integer_Literal (Loc, Enumeration_Rep (Entity (Pref))));

         --  If this is a renaming of a literal, recover the representation
         --  of the original.

         elsif Ekind (Entity (Pref)) = E_Constant
           and then Present (Renamed_Object (Entity (Pref)))
           and then
             Ekind (Entity (Renamed_Object (Entity (Pref))))
               = E_Enumeration_Literal
         then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Enumeration_Rep (Entity (Renamed_Object (Entity (Pref))))));

         --  X'Enum_Rep where X is an object does a direct unchecked conversion
         --  of the object value, as described for the type case above.

         else
            Rewrite (N,
              OK_Convert_To (Typ, Relocate_Node (Pref)));
         end if;

         Set_Etype (N, Typ);
         Analyze_And_Resolve (N, Typ);
      end Enum_Rep;

      --------------
      -- Enum_Val --
      --------------

      when Attribute_Enum_Val => Enum_Val : declare
         Expr : Node_Id;
         Btyp : constant Entity_Id  := Base_Type (Ptyp);

      begin
         --  X'Enum_Val (Y) expands to

         --    [constraint_error when _rep_to_pos (Y, False) = -1, msg]
         --    X!(Y);

         Expr := Unchecked_Convert_To (Ptyp, First (Exprs));

         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd =>
                   Make_Function_Call (Loc,
                     Name =>
                       New_Reference_To (TSS (Btyp, TSS_Rep_To_Pos), Loc),
                     Parameter_Associations => New_List (
                       Relocate_Node (Duplicate_Subexpr (Expr)),
                         New_Occurrence_Of (Standard_False, Loc))),

                 Right_Opnd => Make_Integer_Literal (Loc, -1)),
             Reason => CE_Range_Check_Failed));

         Rewrite (N, Expr);
         Analyze_And_Resolve (N, Ptyp);
      end Enum_Val;

      --------------
      -- Exponent --
      --------------

      --  Transforms 'Exponent into a call to the floating-point attribute
      --  function Exponent in Fat_xxx (where xxx is the root type)

      when Attribute_Exponent =>
         Expand_Fpt_Attribute_R (N);

      ------------------
      -- External_Tag --
      ------------------

      --  transforme X'External_Tag into Ada.Tags.External_Tag (X'tag)

      when Attribute_External_Tag => External_Tag :
      begin
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (RTE (RE_External_Tag), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Tag,
                 Prefix => Prefix (N)))));

         Analyze_And_Resolve (N, Standard_String);
      end External_Tag;

      -----------
      -- First --
      -----------

      when Attribute_First =>

         --  If the prefix type is a constrained packed array type which
         --  already has a Packed_Array_Type representation defined, then
         --  replace this attribute with a direct reference to 'First of the
         --  appropriate index subtype (since otherwise the back end will try
         --  to give us the value of 'First for this implementation type).

         if Is_Constrained_Packed_Array (Ptyp) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_First,
                Prefix => New_Reference_To (Get_Index_Subtype (N), Loc)));
            Analyze_And_Resolve (N, Typ);

         elsif Is_Access_Type (Ptyp) then
            Apply_Access_Check (N);
         end if;

      ---------------
      -- First_Bit --
      ---------------

      --  Compute this if component clause was present, otherwise we leave the
      --  computation to be completed in the back-end, since we don't know what
      --  layout will be chosen.

      when Attribute_First_Bit => First_Bit_Attr : declare
         CE : constant Entity_Id := Entity (Selector_Name (Pref));

      begin
         --  In Ada 2005 (or later) if we have the non-default bit order, then
         --  we return the original value as given in the component clause
         --  (RM 2005 13.5.2(3/2)).

         if Present (Component_Clause (CE))
           and then Ada_Version >= Ada_2005
           and then Reverse_Bit_Order (Scope (CE))
         then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Intval => Expr_Value (First_Bit (Component_Clause (CE)))));
            Analyze_And_Resolve (N, Typ);

         --  Otherwise (Ada 83/95 or Ada 2005 or later with default bit order),
         --  rewrite with normalized value if we know it statically.

         elsif Known_Static_Component_Bit_Offset (CE) then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Component_Bit_Offset (CE) mod System_Storage_Unit));
            Analyze_And_Resolve (N, Typ);

         --  Otherwise left to back end, just do universal integer checks

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end First_Bit_Attr;

      -----------------
      -- Fixed_Value --
      -----------------

      --  We transform:

      --     fixtype'Fixed_Value (integer-value)

      --  into

      --     fixtype(integer-value)

      --  We do all the required analysis of the conversion here, because we do
      --  not want this to go through the fixed-point conversion circuits. Note
      --  that the back end always treats fixed-point as equivalent to the
      --  corresponding integer type anyway.

      when Attribute_Fixed_Value => Fixed_Value :
      begin
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Entity (Pref), Loc),
             Expression   => Relocate_Node (First (Exprs))));
         Set_Etype (N, Entity (Pref));
         Set_Analyzed (N);

      --  Note: it might appear that a properly analyzed unchecked conversion
      --  would be just fine here, but that's not the case, since the full
      --  range checks performed by the following call are critical!

         Apply_Type_Conversion_Checks (N);
      end Fixed_Value;

      -----------
      -- Floor --
      -----------

      --  Transforms 'Floor into a call to the floating-point attribute
      --  function Floor in Fat_xxx (where xxx is the root type)

      when Attribute_Floor =>
         Expand_Fpt_Attribute_R (N);

      ----------
      -- Fore --
      ----------

      --  For the fixed-point type Typ:

      --    Typ'Fore

      --  expands into

      --    Result_Type (System.Fore (Universal_Real (Type'First)),
      --                              Universal_Real (Type'Last))

      --  Note that we know that the type is a non-static subtype, or Fore
      --  would have itself been computed dynamically in Eval_Attribute.

      when Attribute_Fore => Fore : begin
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (RE_Fore), Loc),

               Parameter_Associations => New_List (
                 Convert_To (Universal_Real,
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (Ptyp, Loc),
                     Attribute_Name => Name_First)),

                 Convert_To (Universal_Real,
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (Ptyp, Loc),
                     Attribute_Name => Name_Last))))));

         Analyze_And_Resolve (N, Typ);
      end Fore;

      --------------
      -- Fraction --
      --------------

      --  Transforms 'Fraction into a call to the floating-point attribute
      --  function Fraction in Fat_xxx (where xxx is the root type)

      when Attribute_Fraction =>
         Expand_Fpt_Attribute_R (N);

      --------------
      -- From_Any --
      --------------

      when Attribute_From_Any => From_Any : declare
         P_Type : constant Entity_Id := Etype (Pref);
         Decls  : constant List_Id   := New_List;
      begin
         Rewrite (N,
           Build_From_Any_Call (P_Type,
             Relocate_Node (First (Exprs)),
             Decls));
         Insert_Actions (N, Decls);
         Analyze_And_Resolve (N, P_Type);
      end From_Any;

      --------------
      -- Identity --
      --------------

      --  For an exception returns a reference to the exception data:
      --      Exception_Id!(Prefix'Reference)

      --  For a task it returns a reference to the _task_id component of
      --  corresponding record:

      --    taskV!(Prefix)._Task_Id, converted to the type Task_Id defined

      --  in Ada.Task_Identification

      when Attribute_Identity => Identity : declare
         Id_Kind : Entity_Id;

      begin
         if Ptyp = Standard_Exception_Type then
            Id_Kind := RTE (RE_Exception_Id);

            if Present (Renamed_Object (Entity (Pref))) then
               Set_Entity (Pref, Renamed_Object (Entity (Pref)));
            end if;

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind, Make_Reference (Loc, Pref)));
         else
            Id_Kind := RTE (RO_AT_Task_Id);

            --  If the prefix is a task interface, the Task_Id is obtained
            --  dynamically through a dispatching call, as for other task
            --  attributes applied to interfaces.

            if Ada_Version >= Ada_2005
              and then Ekind (Ptyp) = E_Class_Wide_Type
              and then Is_Interface (Ptyp)
              and then Is_Task_Interface (Ptyp)
            then
               Rewrite (N,
                 Unchecked_Convert_To (Id_Kind,
                   Make_Selected_Component (Loc,
                     Prefix =>
                       New_Copy_Tree (Pref),
                     Selector_Name =>
                       Make_Identifier (Loc, Name_uDisp_Get_Task_Id))));

            else
               Rewrite (N,
                 Unchecked_Convert_To (Id_Kind, Concurrent_Ref (Pref)));
            end if;
         end if;

         Analyze_And_Resolve (N, Id_Kind);
      end Identity;

      -----------
      -- Image --
      -----------

      --  Image attribute is handled in separate unit Exp_Imgv

      when Attribute_Image =>
         Exp_Imgv.Expand_Image_Attribute (N);

      ---------
      -- Img --
      ---------

      --  X'Img is expanded to typ'Image (X), where typ is the type of X

      when Attribute_Img => Img :
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Ptyp, Loc),
             Attribute_Name => Name_Image,
             Expressions => New_List (Relocate_Node (Pref))));

         Analyze_And_Resolve (N, Standard_String);
      end Img;

      -----------
      -- Input --
      -----------

      when Attribute_Input => Input : declare
         P_Type : constant Entity_Id := Entity (Pref);
         B_Type : constant Entity_Id := Base_Type (P_Type);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Strm   : constant Node_Id   := First (Exprs);
         Fname  : Entity_Id;
         Decl   : Node_Id;
         Call   : Node_Id;
         Prag   : Node_Id;
         Arg2   : Node_Id;
         Rfunc  : Node_Id;

         Cntrl  : Node_Id := Empty;
         --  Value for controlling argument in call. Always Empty except in
         --  the dispatching (class-wide type) case, where it is a reference
         --  to the dummy object initialized to the right internal tag.

         procedure Freeze_Stream_Subprogram (F : Entity_Id);
         --  The expansion of the attribute reference may generate a call to
         --  a user-defined stream subprogram that is frozen by the call. This
         --  can lead to access-before-elaboration problem if the reference
         --  appears in an object declaration and the subprogram body has not
         --  been seen. The freezing of the subprogram requires special code
         --  because it appears in an expanded context where expressions do
         --  not freeze their constituents.

         ------------------------------
         -- Freeze_Stream_Subprogram --
         ------------------------------

         procedure Freeze_Stream_Subprogram (F : Entity_Id) is
            Decl : constant Node_Id := Unit_Declaration_Node (F);
            Bod  : Node_Id;

         begin
            --  If this is user-defined subprogram, the corresponding
            --  stream function appears as a renaming-as-body, and the
            --  user subprogram must be retrieved by tree traversal.

            if Present (Decl)
              and then Nkind (Decl) = N_Subprogram_Declaration
              and then Present (Corresponding_Body (Decl))
            then
               Bod := Corresponding_Body (Decl);

               if Nkind (Unit_Declaration_Node (Bod)) =
                 N_Subprogram_Renaming_Declaration
               then
                  Set_Is_Frozen (Entity (Name (Unit_Declaration_Node (Bod))));
               end if;
            end if;
         end Freeze_Stream_Subprogram;

      --  Start of processing for Input

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  If there is a TSS for Input, just call it

         Fname := Find_Stream_Subprogram (P_Type, TSS_Stream_Input);

         if Present (Fname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Input (stream)

            --  as

            --     sourcetyp (streamread (strmtyp'Input (stream)));

            --  where streamread is the given Read function that converts an
            --  argument of type strmtyp to type sourcetyp or a type from which
            --  it is derived (extra conversion required for the derived case).

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg2  := Next (First (Pragma_Argument_Associations (Prag)));
               Rfunc := Entity (Expression (Arg2));

               Rewrite (N,
                 Convert_To (B_Type,
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (Rfunc, Loc),
                     Parameter_Associations => New_List (
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           New_Occurrence_Of
                             (Etype (First_Formal (Rfunc)), Loc),
                         Attribute_Name => Name_Input,
                         Expressions => Exprs)))));

               Analyze_And_Resolve (N, B_Type);
               return;

            --  Elementary types

            elsif Is_Elementary_Type (U_Type) then

               --  A special case arises if we have a defined _Read routine,
               --  since in this case we are required to call this routine.

               if Present (TSS (Base_Type (U_Type), TSS_Stream_Read)) then
                  Build_Record_Or_Elementary_Input_Function
                    (Loc, U_Type, Decl, Fname);
                  Insert_Action (N, Decl);

               --  For normal cases, we call the I_xxx routine directly

               else
                  Rewrite (N, Build_Elementary_Input_Call (N));
                  Analyze_And_Resolve (N, P_Type);
                  return;
               end if;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Input_Function (Loc, U_Type, Decl, Fname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Dispatching case with class-wide type

            elsif Is_Class_Wide_Type (P_Type) then

               --  No need to do anything else compiling under restriction
               --  No_Dispatching_Calls. During the semantic analysis we
               --  already notified such violation.

               if Restriction_Active (No_Dispatching_Calls) then
                  return;
               end if;

               declare
                  Rtyp : constant Entity_Id := Root_Type (P_Type);
                  Dnn  : Entity_Id;
                  Decl : Node_Id;
                  Expr : Node_Id;

               begin
                  --  Read the internal tag (RM 13.13.2(34)) and use it to
                  --  initialize a dummy tag object:

                  --    Dnn : Ada.Tags.Tag :=
                  --            Descendant_Tag (String'Input (Strm), P_Type);

                  --  This dummy object is used only to provide a controlling
                  --  argument for the eventual _Input call. Descendant_Tag is
                  --  called rather than Internal_Tag to ensure that we have a
                  --  tag for a type that is descended from the prefix type and
                  --  declared at the same accessibility level (the exception
                  --  Tag_Error will be raised otherwise). The level check is
                  --  required for Ada 2005 because tagged types can be
                  --  extended in nested scopes (AI-344).

                  Expr :=
                    Make_Function_Call (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Descendant_Tag), Loc),
                      Parameter_Associations => New_List (
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Standard_String, Loc),
                          Attribute_Name => Name_Input,
                          Expressions => New_List (
                            Relocate_Node (Duplicate_Subexpr (Strm)))),
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Reference_To (P_Type, Loc),
                          Attribute_Name => Name_Tag)));

                  Dnn := Make_Temporary (Loc, 'D', Expr);

                  Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Dnn,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Tag), Loc),
                      Expression          => Expr);

                  Insert_Action (N, Decl);

                  --  Now we need to get the entity for the call, and construct
                  --  a function call node, where we preset a reference to Dnn
                  --  as the controlling argument (doing an unchecked convert
                  --  to the class-wide tagged type to make it look like a real
                  --  tagged object).

                  Fname := Find_Prim_Op (Rtyp, TSS_Stream_Input);
                  Cntrl :=
                    Unchecked_Convert_To (P_Type,
                      New_Occurrence_Of (Dnn, Loc));
                  Set_Etype (Cntrl, P_Type);
                  Set_Parent (Cntrl, N);
               end;

            --  For tagged types, use the primitive Input function

            elsif Is_Tagged_Type (U_Type) then
               Fname := Find_Prim_Op (U_Type, TSS_Stream_Input);

            --  All other record type cases, including protected records. The
            --  latter only arise for expander generated code for handling
            --  shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised executing default
               --  implementation of the Input attribute of an unchecked union
               --  type if the type lacks default discriminant values.

               if Is_Unchecked_Union (Base_Type (U_Type))
                 and then No (Discriminant_Constraint (U_Type))
               then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));

                  return;
               end if;

               --  Build the type's Input function, passing the subtype rather
               --  than its base type, because checks are needed in the case of
               --  constrained discriminants (see Ada 2012 AI05-0192).

               Build_Record_Or_Elementary_Input_Function
                 (Loc, U_Type, Decl, Fname);
               Insert_Action (N, Decl);

               if Nkind (Parent (N)) = N_Object_Declaration
                 and then Is_Record_Type (U_Type)
               then
                  --  The stream function may contain calls to user-defined
                  --  Read procedures for individual components.

                  declare
                     Comp : Entity_Id;
                     Func : Entity_Id;

                  begin
                     Comp := First_Component (U_Type);
                     while Present (Comp) loop
                        Func :=
                          Find_Stream_Subprogram
                            (Etype (Comp), TSS_Stream_Read);

                        if Present (Func) then
                           Freeze_Stream_Subprogram (Func);
                        end if;

                        Next_Component (Comp);
                     end loop;
                  end;
               end if;
            end if;
         end if;

         --  If we fall through, Fname is the function to be called. The result
         --  is obtained by calling the appropriate function, then converting
         --  the result. The conversion does a subtype check.

         Call :=
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (Fname, Loc),
             Parameter_Associations => New_List (
                Relocate_Node (Strm)));

         Set_Controlling_Argument (Call, Cntrl);
         Rewrite (N, Unchecked_Convert_To (P_Type, Call));
         Analyze_And_Resolve (N, P_Type);

         if Nkind (Parent (N)) = N_Object_Declaration then
            Freeze_Stream_Subprogram (Fname);
         end if;
      end Input;

      -------------------
      -- Integer_Value --
      -------------------

      --  We transform

      --    inttype'Fixed_Value (fixed-value)

      --  into

      --    inttype(integer-value))

      --  we do all the required analysis of the conversion here, because we do
      --  not want this to go through the fixed-point conversion circuits. Note
      --  that the back end always treats fixed-point as equivalent to the
      --  corresponding integer type anyway.

      when Attribute_Integer_Value => Integer_Value :
      begin
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Entity (Pref), Loc),
             Expression   => Relocate_Node (First (Exprs))));
         Set_Etype (N, Entity (Pref));
         Set_Analyzed (N);

      --  Note: it might appear that a properly analyzed unchecked conversion
      --  would be just fine here, but that's not the case, since the full
      --  range checks performed by the following call are critical!

         Apply_Type_Conversion_Checks (N);
      end Integer_Value;

      -------------------
      -- Invalid_Value --
      -------------------

      when Attribute_Invalid_Value =>
         Rewrite (N, Get_Simple_Init_Val (Ptyp, N));

      ----------
      -- Last --
      ----------

      when Attribute_Last =>

         --  If the prefix type is a constrained packed array type which
         --  already has a Packed_Array_Type representation defined, then
         --  replace this attribute with a direct reference to 'Last of the
         --  appropriate index subtype (since otherwise the back end will try
         --  to give us the value of 'Last for this implementation type).

         if Is_Constrained_Packed_Array (Ptyp) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Last,
                Prefix => New_Reference_To (Get_Index_Subtype (N), Loc)));
            Analyze_And_Resolve (N, Typ);

         elsif Is_Access_Type (Ptyp) then
            Apply_Access_Check (N);
         end if;

      --------------
      -- Last_Bit --
      --------------

      --  We compute this if a component clause was present, otherwise we leave
      --  the computation up to the back end, since we don't know what layout
      --  will be chosen.

      when Attribute_Last_Bit => Last_Bit_Attr : declare
         CE : constant Entity_Id := Entity (Selector_Name (Pref));

      begin
         --  In Ada 2005 (or later) if we have the non-default bit order, then
         --  we return the original value as given in the component clause
         --  (RM 2005 13.5.2(3/2)).

         if Present (Component_Clause (CE))
           and then Ada_Version >= Ada_2005
           and then Reverse_Bit_Order (Scope (CE))
         then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Intval => Expr_Value (Last_Bit (Component_Clause (CE)))));
            Analyze_And_Resolve (N, Typ);

         --  Otherwise (Ada 83/95 or Ada 2005 or later with default bit order),
         --  rewrite with normalized value if we know it statically.

         elsif Known_Static_Component_Bit_Offset (CE)
           and then Known_Static_Esize (CE)
         then
            Rewrite (N,
              Make_Integer_Literal (Loc,
               Intval => (Component_Bit_Offset (CE) mod System_Storage_Unit)
                                + Esize (CE) - 1));
            Analyze_And_Resolve (N, Typ);

         --  Otherwise leave to back end, just apply universal integer checks

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Last_Bit_Attr;

      ------------------
      -- Leading_Part --
      ------------------

      --  Transforms 'Leading_Part into a call to the floating-point attribute
      --  function Leading_Part in Fat_xxx (where xxx is the root type)

      --  Note: strictly, we should generate special case code to deal with
      --  absurdly large positive arguments (greater than Integer'Last), which
      --  result in returning the first argument unchanged, but it hardly seems
      --  worth the effort. We raise constraint error for absurdly negative
      --  arguments which is fine.

      when Attribute_Leading_Part =>
         Expand_Fpt_Attribute_RI (N);

      ------------
      -- Length --
      ------------

      when Attribute_Length => Length : declare
         Ityp : Entity_Id;
         Xnum : Uint;

      begin
         --  Processing for packed array types

         if Is_Array_Type (Ptyp) and then Is_Packed (Ptyp) then
            Ityp := Get_Index_Subtype (N);

            --  If the index type, Ityp, is an enumeration type with holes,
            --  then we calculate X'Length explicitly using

            --     Typ'Max
            --       (0, Ityp'Pos (X'Last  (N)) -
            --           Ityp'Pos (X'First (N)) + 1);

            --  Since the bounds in the template are the representation values
            --  and the back end would get the wrong value.

            if Is_Enumeration_Type (Ityp)
              and then Present (Enum_Pos_To_Rep (Base_Type (Ityp)))
            then
               if No (Exprs) then
                  Xnum := Uint_1;
               else
                  Xnum := Expr_Value (First (Expressions (N)));
               end if;

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Typ, Loc),
                   Attribute_Name => Name_Max,
                   Expressions    => New_List
                     (Make_Integer_Literal (Loc, 0),

                      Make_Op_Add (Loc,
                        Left_Opnd =>
                          Make_Op_Subtract (Loc,
                            Left_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Ityp, Loc),
                                Attribute_Name => Name_Pos,

                                Expressions => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix => Duplicate_Subexpr (Pref),
                                   Attribute_Name => Name_Last,
                                    Expressions => New_List (
                                      Make_Integer_Literal (Loc, Xnum))))),

                            Right_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Ityp, Loc),
                                Attribute_Name => Name_Pos,

                                Expressions => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix =>
                                      Duplicate_Subexpr_No_Checks (Pref),
                                   Attribute_Name => Name_First,
                                    Expressions => New_List (
                                      Make_Integer_Literal (Loc, Xnum)))))),

                        Right_Opnd => Make_Integer_Literal (Loc, 1)))));

               Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
               return;

            --  If the prefix type is a constrained packed array type which
            --  already has a Packed_Array_Type representation defined, then
            --  replace this attribute with a direct reference to 'Range_Length
            --  of the appropriate index subtype (since otherwise the back end
            --  will try to give us the value of 'Length for this
            --  implementation type).

            elsif Is_Constrained (Ptyp) then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Range_Length,
                   Prefix => New_Reference_To (Ityp, Loc)));
               Analyze_And_Resolve (N, Typ);
            end if;

         --  Access type case

         elsif Is_Access_Type (Ptyp) then
            Apply_Access_Check (N);

            --  If the designated type is a packed array type, then we convert
            --  the reference to:

            --    typ'Max (0, 1 +
            --                xtyp'Pos (Pref'Last (Expr)) -
            --                xtyp'Pos (Pref'First (Expr)));

            --  This is a bit complex, but it is the easiest thing to do that
            --  works in all cases including enum types with holes xtyp here
            --  is the appropriate index type.

            declare
               Dtyp : constant Entity_Id := Designated_Type (Ptyp);
               Xtyp : Entity_Id;

            begin
               if Is_Array_Type (Dtyp) and then Is_Packed (Dtyp) then
                  Xtyp := Get_Index_Subtype (N);

                  Rewrite (N,
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Typ, Loc),
                      Attribute_Name => Name_Max,
                      Expressions    => New_List (
                        Make_Integer_Literal (Loc, 0),

                        Make_Op_Add (Loc,
                          Make_Integer_Literal (Loc, 1),
                          Make_Op_Subtract (Loc,
                            Left_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Xtyp, Loc),
                                Attribute_Name => Name_Pos,
                                Expressions    => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix => Duplicate_Subexpr (Pref),
                                    Attribute_Name => Name_Last,
                                    Expressions =>
                                      New_Copy_List (Exprs)))),

                            Right_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Xtyp, Loc),
                                Attribute_Name => Name_Pos,
                                Expressions    => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix =>
                                      Duplicate_Subexpr_No_Checks (Pref),
                                    Attribute_Name => Name_First,
                                    Expressions =>
                                      New_Copy_List (Exprs)))))))));

                  Analyze_And_Resolve (N, Typ);
               end if;
            end;

         --  Otherwise leave it to the back end

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Length;

      --  Attribute Loop_Entry is replaced with a reference to a constant value
      --  which captures the prefix at the entry point of the related loop. The
      --  loop itself may be transformed into a conditional block.

      when Attribute_Loop_Entry =>
         Expand_Loop_Entry_Attribute (N);

      -------------
      -- Machine --
      -------------

      --  Transforms 'Machine into a call to the floating-point attribute
      --  function Machine in Fat_xxx (where xxx is the root type)

      when Attribute_Machine =>
         Expand_Fpt_Attribute_R (N);

      ----------------------
      -- Machine_Rounding --
      ----------------------

      --  Transforms 'Machine_Rounding into a call to the floating-point
      --  attribute function Machine_Rounding in Fat_xxx (where xxx is the root
      --  type). Expansion is avoided for cases the back end can handle
      --  directly.

      when Attribute_Machine_Rounding =>
         if not Is_Inline_Floating_Point_Attribute (N) then
            Expand_Fpt_Attribute_R (N);
         end if;

      ------------------
      -- Machine_Size --
      ------------------

      --  Machine_Size is equivalent to Object_Size, so transform it into
      --  Object_Size and that way the back end never sees Machine_Size.

      when Attribute_Machine_Size =>
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix => Prefix (N),
             Attribute_Name => Name_Object_Size));

         Analyze_And_Resolve (N, Typ);

      --------------
      -- Mantissa --
      --------------

      --  The only case that can get this far is the dynamic case of the old
      --  Ada 83 Mantissa attribute for the fixed-point case. For this case,
      --  we expand:

      --    typ'Mantissa

      --  into

      --    ityp (System.Mantissa.Mantissa_Value
      --           (Integer'Integer_Value (typ'First),
      --            Integer'Integer_Value (typ'Last)));

      when Attribute_Mantissa => Mantissa : begin
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (RE_Mantissa_Value), Loc),

               Parameter_Associations => New_List (

                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Standard_Integer, Loc),
                   Attribute_Name => Name_Integer_Value,
                   Expressions => New_List (

                     Make_Attribute_Reference (Loc,
                       Prefix => New_Occurrence_Of (Ptyp, Loc),
                       Attribute_Name => Name_First))),

                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Standard_Integer, Loc),
                   Attribute_Name => Name_Integer_Value,
                   Expressions => New_List (

                     Make_Attribute_Reference (Loc,
                       Prefix => New_Occurrence_Of (Ptyp, Loc),
                       Attribute_Name => Name_Last)))))));

         Analyze_And_Resolve (N, Typ);
      end Mantissa;

      ----------------------------------
      -- Max_Size_In_Storage_Elements --
      ----------------------------------

      when Attribute_Max_Size_In_Storage_Elements => declare
         Typ  : constant Entity_Id := Etype (N);
         Attr : Node_Id;

         Conversion_Added : Boolean := False;
         --  A flag which tracks whether the original attribute has been
         --  wrapped inside a type conversion.

      begin
         Apply_Universal_Integer_Attribute_Checks (N);

         --  The universal integer check may sometimes add a type conversion,
         --  retrieve the original attribute reference from the expression.

         Attr := N;
         if Nkind (Attr) = N_Type_Conversion then
            Attr := Expression (Attr);
            Conversion_Added := True;
         end if;

         --  Heap-allocated controlled objects contain two extra pointers which
         --  are not part of the actual type. Transform the attribute reference
         --  into a runtime expression to add the size of the hidden header.

         --  Do not perform this expansion on .NET/JVM targets because the
         --  two pointers are already present in the type.

         if VM_Target = No_VM
           and then Nkind (Attr) = N_Attribute_Reference
           and then Needs_Finalization (Ptyp)
           and then not Header_Size_Added (Attr)
         then
            Set_Header_Size_Added (Attr);

            --  Generate:
            --    P'Max_Size_In_Storage_Elements +
            --      Universal_Integer
            --        (Header_Size_With_Padding (Ptyp'Alignment))

            Rewrite (Attr,
              Make_Op_Add (Loc,
                Left_Opnd  => Relocate_Node (Attr),
                Right_Opnd =>
                  Convert_To (Universal_Integer,
                    Make_Function_Call (Loc,
                      Name                   =>
                        New_Reference_To
                          (RTE (RE_Header_Size_With_Padding), Loc),

                      Parameter_Associations => New_List (
                        Make_Attribute_Reference (Loc,
                          Prefix         =>
                            New_Reference_To (Ptyp, Loc),
                          Attribute_Name => Name_Alignment))))));

            --  Add a conversion to the target type

            if not Conversion_Added then
               Rewrite (Attr,
                 Make_Type_Conversion (Loc,
                   Subtype_Mark => New_Reference_To (Typ, Loc),
                   Expression   => Relocate_Node (Attr)));
            end if;

            Analyze (Attr);
            return;
         end if;
      end;

      --------------------
      -- Mechanism_Code --
      --------------------

      when Attribute_Mechanism_Code =>

         --  We must replace the prefix in the renamed case

         if Is_Entity_Name (Pref)
           and then Present (Alias (Entity (Pref)))
         then
            Set_Renamed_Subprogram (Pref, Alias (Entity (Pref)));
         end if;

      ---------
      -- Mod --
      ---------

      when Attribute_Mod => Mod_Case : declare
         Arg  : constant Node_Id := Relocate_Node (First (Exprs));
         Hi   : constant Node_Id := Type_High_Bound (Etype (Arg));
         Modv : constant Uint    := Modulus (Btyp);

      begin

         --  This is not so simple. The issue is what type to use for the
         --  computation of the modular value.

         --  The easy case is when the modulus value is within the bounds
         --  of the signed integer type of the argument. In this case we can
         --  just do the computation in that signed integer type, and then
         --  do an ordinary conversion to the target type.

         if Modv <= Expr_Value (Hi) then
            Rewrite (N,
              Convert_To (Btyp,
                Make_Op_Mod (Loc,
                  Left_Opnd  => Arg,
                  Right_Opnd => Make_Integer_Literal (Loc, Modv))));

         --  Here we know that the modulus is larger than type'Last of the
         --  integer type. There are two cases to consider:

         --    a) The integer value is non-negative. In this case, it is
         --    returned as the result (since it is less than the modulus).

         --    b) The integer value is negative. In this case, we know that the
         --    result is modulus + value, where the value might be as small as
         --    -modulus. The trouble is what type do we use to do the subtract.
         --    No type will do, since modulus can be as big as 2**64, and no
         --    integer type accommodates this value. Let's do bit of algebra

         --         modulus + value
         --      =  modulus - (-value)
         --      =  (modulus - 1) - (-value - 1)

         --    Now modulus - 1 is certainly in range of the modular type.
         --    -value is in the range 1 .. modulus, so -value -1 is in the
         --    range 0 .. modulus-1 which is in range of the modular type.
         --    Furthermore, (-value - 1) can be expressed as -(value + 1)
         --    which we can compute using the integer base type.

         --  Once this is done we analyze the if expression without range
         --  checks, because we know everything is in range, and we want
         --  to prevent spurious warnings on either branch.

         else
            Rewrite (N,
              Make_If_Expression (Loc,
                Expressions => New_List (
                  Make_Op_Ge (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Arg),
                    Right_Opnd => Make_Integer_Literal (Loc, 0)),

                  Convert_To (Btyp,
                    Duplicate_Subexpr_No_Checks (Arg)),

                  Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Integer_Literal (Loc,
                        Intval => Modv - 1),
                    Right_Opnd =>
                      Convert_To (Btyp,
                        Make_Op_Minus (Loc,
                          Right_Opnd =>
                            Make_Op_Add (Loc,
                              Left_Opnd  => Duplicate_Subexpr_No_Checks (Arg),
                              Right_Opnd =>
                                Make_Integer_Literal (Loc,
                                  Intval => 1))))))));

         end if;

         Analyze_And_Resolve (N, Btyp, Suppress => All_Checks);
      end Mod_Case;

      -----------
      -- Model --
      -----------

      --  Transforms 'Model into a call to the floating-point attribute
      --  function Model in Fat_xxx (where xxx is the root type)

      when Attribute_Model =>
         Expand_Fpt_Attribute_R (N);

      -----------------
      -- Object_Size --
      -----------------

      --  The processing for Object_Size shares the processing for Size

      ---------
      -- Old --
      ---------

      when Attribute_Old => Old : declare
         Tnn     : constant Entity_Id := Make_Temporary (Loc, 'T', Pref);
         Subp    : Node_Id;
         Asn_Stm : Node_Id;

      begin
         --  If assertions are disabled, no need to create the declaration
         --  that preserves the value.

         if not Assertions_Enabled then
            return;
         end if;

         --  Find the nearest subprogram body, ignoring _Preconditions

         Subp := N;
         loop
            Subp := Parent (Subp);
            exit when Nkind (Subp) = N_Subprogram_Body
              and then Chars (Defining_Entity (Subp)) /= Name_uPostconditions;
         end loop;

         --  Insert the initialized object declaration at the start of the
         --  subprogram's declarations.

         Asn_Stm :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tnn,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Etype (N), Loc),
             Expression          => Pref);

         --  Push the subprogram's scope, so that the object will be analyzed
         --  in that context (rather than the context of the Precondition
         --  subprogram) and will have its Scope set properly.

         if Present (Corresponding_Spec (Subp)) then
            Push_Scope (Corresponding_Spec (Subp));
         else
            Push_Scope (Defining_Entity (Subp));
         end if;

         if Is_Empty_List (Declarations (Subp)) then
            Set_Declarations (Subp, New_List (Asn_Stm));
            Analyze (Asn_Stm);
         else
            Insert_Action (First (Declarations (Subp)), Asn_Stm);
         end if;

         Pop_Scope;

         Rewrite (N, New_Occurrence_Of (Tnn, Loc));
      end Old;

      ----------------------
      -- Overlaps_Storage --
      ----------------------

      when Attribute_Overlaps_Storage => Overlaps_Storage : declare
         Loc : constant Source_Ptr := Sloc (N);

         X   : constant Node_Id := Prefix (N);
         Y   : constant Node_Id := First (Expressions (N));
         --  The argumens

         X_Addr, Y_Addr : Node_Id;
         --  the expressions for their integer addresses

         X_Size, Y_Size : Node_Id;
         --  the expressions for their sizes

         Cond : Node_Id;

      begin
         --  Attribute expands into:

         --    if X'Address < Y'address then
         --      (X'address + X'Size - 1) >= Y'address
         --    else
         --      (Y'address + Y'size - 1) >= X'Address
         --    end if;

         --  with the proper address operations. We convert addresses to
         --  integer addresses to use predefined arithmetic. The size is
         --  expressed in storage units.

         X_Addr :=
           Unchecked_Convert_To (RTE (RE_Integer_Address),
             Make_Attribute_Reference (Loc,
               Attribute_Name => Name_Address,
               Prefix         => New_Copy_Tree (X)));

         Y_Addr :=
           Unchecked_Convert_To (RTE (RE_Integer_Address),
             Make_Attribute_Reference (Loc,
               Attribute_Name => Name_Address,
               Prefix         => New_Copy_Tree (Y)));

         X_Size :=
           Make_Op_Divide (Loc,
             Left_Opnd  =>
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Size,
                 Prefix         => New_Copy_Tree (X)),
             Right_Opnd =>
               Make_Integer_Literal (Loc, System_Storage_Unit));

         Y_Size :=
           Make_Op_Divide (Loc,
             Left_Opnd  =>
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Size,
                 Prefix         => New_Copy_Tree (Y)),
             Right_Opnd =>
               Make_Integer_Literal (Loc, System_Storage_Unit));

         Cond :=
            Make_Op_Le (Loc,
              Left_Opnd  => X_Addr,
              Right_Opnd => Y_Addr);

         Rewrite (N,
           Make_If_Expression (Loc,
             New_List (
               Cond,

               Make_Op_Ge (Loc,
                  Left_Opnd   =>
                   Make_Op_Add (Loc,
                     Left_Opnd  => X_Addr,
                     Right_Opnd =>
                       Make_Op_Subtract (Loc,
                         Left_Opnd  => X_Size,
                         Right_Opnd => Make_Integer_Literal (Loc, 1))),
                  Right_Opnd => Y_Addr),

               Make_Op_Ge (Loc,
                   Make_Op_Add (Loc,
                     Left_Opnd  => Y_Addr,
                     Right_Opnd =>
                       Make_Op_Subtract (Loc,
                         Left_Opnd  => Y_Size,
                         Right_Opnd => Make_Integer_Literal (Loc, 1))),
                  Right_Opnd => X_Addr))));

         Analyze_And_Resolve (N, Standard_Boolean);
      end Overlaps_Storage;

      ------------
      -- Output --
      ------------

      when Attribute_Output => Output : declare
         P_Type : constant Entity_Id := Entity (Pref);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Pname  : Entity_Id;
         Decl   : Node_Id;
         Prag   : Node_Id;
         Arg3   : Node_Id;
         Wfunc  : Node_Id;

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  If TSS for Output is present, just call it

         Pname := Find_Stream_Subprogram (P_Type, TSS_Stream_Output);

         if Present (Pname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Output (stream, Item)

            --  as

            --     strmtyp'Output (Stream, strmwrite (acttyp (Item)));

            --  where strmwrite is the given Write function that converts an
            --  argument of type sourcetyp or a type acctyp, from which it is
            --  derived to type strmtyp. The conversion to acttyp is required
            --  for the derived case.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg3 :=
                 Next (Next (First (Pragma_Argument_Associations (Prag))));
               Wfunc := Entity (Expression (Arg3));

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Etype (Wfunc), Loc),
                   Attribute_Name => Name_Output,
                   Expressions => New_List (
                   Relocate_Node (First (Exprs)),
                     Make_Function_Call (Loc,
                       Name => New_Occurrence_Of (Wfunc, Loc),
                       Parameter_Associations => New_List (
                         OK_Convert_To (Etype (First_Formal (Wfunc)),
                           Relocate_Node (Next (First (Exprs)))))))));

               Analyze (N);
               return;

            --  For elementary types, we call the W_xxx routine directly.
            --  Note that the effect of Write and Output is identical for
            --  the case of an elementary type, since there are no
            --  discriminants or bounds.

            elsif Is_Elementary_Type (U_Type) then

               --  A special case arises if we have a defined _Write routine,
               --  since in this case we are required to call this routine.

               if Present (TSS (Base_Type (U_Type), TSS_Stream_Write)) then
                  Build_Record_Or_Elementary_Output_Procedure
                    (Loc, U_Type, Decl, Pname);
                  Insert_Action (N, Decl);

               --  For normal cases, we call the W_xxx routine directly

               else
                  Rewrite (N, Build_Elementary_Write_Call (N));
                  Analyze (N);
                  return;
               end if;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Output_Procedure (Loc, U_Type, Decl, Pname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Class-wide case, first output external tag, then dispatch
            --  to the appropriate primitive Output function (RM 13.13.2(31)).

            elsif Is_Class_Wide_Type (P_Type) then

               --  No need to do anything else compiling under restriction
               --  No_Dispatching_Calls. During the semantic analysis we
               --  already notified such violation.

               if Restriction_Active (No_Dispatching_Calls) then
                  return;
               end if;

               Tag_Write : declare
                  Strm : constant Node_Id := First (Exprs);
                  Item : constant Node_Id := Next (Strm);

               begin
                  --  Ada 2005 (AI-344): Check that the accessibility level
                  --  of the type of the output object is not deeper than
                  --  that of the attribute's prefix type.

                  --  if Get_Access_Level (Item'Tag)
                  --       /= Get_Access_Level (P_Type'Tag)
                  --  then
                  --     raise Tag_Error;
                  --  end if;

                  --  String'Output (Strm, External_Tag (Item'Tag));

                  --  We cannot figure out a practical way to implement this
                  --  accessibility check on virtual machines, so we omit it.

                  if Ada_Version >= Ada_2005
                    and then Tagged_Type_Expansion
                  then
                     Insert_Action (N,
                       Make_Implicit_If_Statement (N,
                         Condition =>
                           Make_Op_Ne (Loc,
                             Left_Opnd  =>
                               Build_Get_Access_Level (Loc,
                                 Make_Attribute_Reference (Loc,
                                   Prefix         =>
                                     Relocate_Node (
                                       Duplicate_Subexpr (Item,
                                         Name_Req => True)),
                                   Attribute_Name => Name_Tag)),

                             Right_Opnd =>
                               Make_Integer_Literal (Loc,
                                 Type_Access_Level (P_Type))),

                         Then_Statements =>
                           New_List (Make_Raise_Statement (Loc,
                                       New_Occurrence_Of (
                                         RTE (RE_Tag_Error), Loc)))));
                  end if;

                  Insert_Action (N,
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Standard_String, Loc),
                      Attribute_Name => Name_Output,
                      Expressions => New_List (
                        Relocate_Node (Duplicate_Subexpr (Strm)),
                        Make_Function_Call (Loc,
                          Name =>
                            New_Occurrence_Of (RTE (RE_External_Tag), Loc),
                          Parameter_Associations => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix =>
                               Relocate_Node
                                 (Duplicate_Subexpr (Item, Name_Req => True)),
                             Attribute_Name => Name_Tag))))));
               end Tag_Write;

               Pname := Find_Prim_Op (U_Type, TSS_Stream_Output);

            --  Tagged type case, use the primitive Output function

            elsif Is_Tagged_Type (U_Type) then
               Pname := Find_Prim_Op (U_Type, TSS_Stream_Output);

            --  All other record type cases, including protected records.
            --  The latter only arise for expander generated code for
            --  handling shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Output attribute of an
               --  unchecked union type if the type lacks default discriminant
               --  values.

               if Is_Unchecked_Union (Base_Type (U_Type))
                 and then No (Discriminant_Constraint (U_Type))
               then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));

                  return;
               end if;

               Build_Record_Or_Elementary_Output_Procedure
                 (Loc, Base_Type (U_Type), Decl, Pname);
               Insert_Action (N, Decl);
            end if;
         end if;

         --  If we fall through, Pname is the name of the procedure to call

         Rewrite_Stream_Proc_Call (Pname);
      end Output;

      ---------
      -- Pos --
      ---------

      --  For enumeration types with a standard representation, Pos is
      --  handled by the back end.

      --  For enumeration types, with a non-standard representation we generate
      --  a call to the _Rep_To_Pos function created when the type was frozen.
      --  The call has the form

      --    _rep_to_pos (expr, flag)

      --  The parameter flag is True if range checks are enabled, causing
      --  Program_Error to be raised if the expression has an invalid
      --  representation, and False if range checks are suppressed.

      --  For integer types, Pos is equivalent to a simple integer
      --  conversion and we rewrite it as such

      when Attribute_Pos => Pos :
      declare
         Etyp : Entity_Id := Base_Type (Entity (Pref));

      begin
         --  Deal with zero/non-zero boolean values

         if Is_Boolean_Type (Etyp) then
            Adjust_Condition (First (Exprs));
            Etyp := Standard_Boolean;
            Set_Prefix (N, New_Occurrence_Of (Standard_Boolean, Loc));
         end if;

         --  Case of enumeration type

         if Is_Enumeration_Type (Etyp) then

            --  Non-standard enumeration type (generate call)

            if Present (Enum_Pos_To_Rep (Etyp)) then
               Append_To (Exprs, Rep_To_Pos_Flag (Etyp, Loc));
               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Reference_To (TSS (Etyp, TSS_Rep_To_Pos), Loc),
                     Parameter_Associations => Exprs)));

               Analyze_And_Resolve (N, Typ);

            --  Standard enumeration type (do universal integer check)

            else
               Apply_Universal_Integer_Attribute_Checks (N);
            end if;

         --  Deal with integer types (replace by conversion)

         elsif Is_Integer_Type (Etyp) then
            Rewrite (N, Convert_To (Typ, First (Exprs)));
            Analyze_And_Resolve (N, Typ);
         end if;

      end Pos;

      --------------
      -- Position --
      --------------

      --  We compute this if a component clause was present, otherwise we leave
      --  the computation up to the back end, since we don't know what layout
      --  will be chosen.

      when Attribute_Position => Position_Attr :
      declare
         CE : constant Entity_Id := Entity (Selector_Name (Pref));

      begin
         if Present (Component_Clause (CE)) then

            --  In Ada 2005 (or later) if we have the non-default bit order,
            --  then we return the original value as given in the component
            --  clause (RM 2005 13.5.2(2/2)).

            if Ada_Version >= Ada_2005
              and then Reverse_Bit_Order (Scope (CE))
            then
               Rewrite (N,
                  Make_Integer_Literal (Loc,
                    Intval => Expr_Value (Position (Component_Clause (CE)))));

            --  Otherwise (Ada 83 or 95, or default bit order specified in
            --  later Ada version), return the normalized value.

            else
               Rewrite (N,
                 Make_Integer_Literal (Loc,
                   Intval => Component_Bit_Offset (CE) / System_Storage_Unit));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  If back end is doing things, just apply universal integer checks

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Position_Attr;

      ----------
      -- Pred --
      ----------

      --  1. Deal with enumeration types with holes
      --  2. For floating-point, generate call to attribute function
      --  3. For other cases, deal with constraint checking

      when Attribute_Pred => Pred :
      declare
         Etyp : constant Entity_Id := Base_Type (Ptyp);

      begin

         --  For enumeration types with non-standard representations, we
         --  expand typ'Pred (x) into

         --    Pos_To_Rep (Rep_To_Pos (x) - 1)

         --    If the representation is contiguous, we compute instead
         --    Lit1 + Rep_to_Pos (x -1), to catch invalid representations.
         --    The conversion function Enum_Pos_To_Rep is defined on the
         --    base type, not the subtype, so we have to use the base type
         --    explicitly for this and other enumeration attributes.

         if Is_Enumeration_Type (Ptyp)
           and then Present (Enum_Pos_To_Rep (Etyp))
         then
            if Has_Contiguous_Rep (Etyp) then
               Rewrite (N,
                  Unchecked_Convert_To (Ptyp,
                     Make_Op_Add (Loc,
                        Left_Opnd  =>
                         Make_Integer_Literal (Loc,
                           Enumeration_Rep (First_Literal (Ptyp))),
                        Right_Opnd =>
                          Make_Function_Call (Loc,
                            Name =>
                              New_Reference_To
                               (TSS (Etyp, TSS_Rep_To_Pos), Loc),

                            Parameter_Associations =>
                              New_List (
                                Unchecked_Convert_To (Ptyp,
                                  Make_Op_Subtract (Loc,
                                    Left_Opnd =>
                                     Unchecked_Convert_To (Standard_Integer,
                                       Relocate_Node (First (Exprs))),
                                    Right_Opnd =>
                                      Make_Integer_Literal (Loc, 1))),
                                Rep_To_Pos_Flag (Ptyp, Loc))))));

            else
               --  Add Boolean parameter True, to request program errror if
               --  we have a bad representation on our hands. If checks are
               --  suppressed, then add False instead

               Append_To (Exprs, Rep_To_Pos_Flag (Ptyp, Loc));
               Rewrite (N,
                 Make_Indexed_Component (Loc,
                   Prefix =>
                     New_Reference_To
                       (Enum_Pos_To_Rep (Etyp), Loc),
                   Expressions => New_List (
                     Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Function_Call (Loc,
                        Name =>
                          New_Reference_To
                            (TSS (Etyp, TSS_Rep_To_Pos), Loc),
                          Parameter_Associations => Exprs),
                    Right_Opnd => Make_Integer_Literal (Loc, 1)))));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  For floating-point, we transform 'Pred into a call to the Pred
         --  floating-point attribute function in Fat_xxx (xxx is root type)

         elsif Is_Floating_Point_Type (Ptyp) then
            Expand_Fpt_Attribute_R (N);
            Analyze_And_Resolve (N, Typ);

         --  For modular types, nothing to do (no overflow, since wraps)

         elsif Is_Modular_Integer_Type (Ptyp) then
            null;

         --  For other types, if argument is marked as needing a range check or
         --  overflow checking is enabled, we must generate a check.

         elsif not Overflow_Checks_Suppressed (Ptyp)
           or else Do_Range_Check (First (Exprs))
         then
            Set_Do_Range_Check (First (Exprs), False);
            Expand_Pred_Succ (N);
         end if;
      end Pred;

      --------------
      -- Priority --
      --------------

      --  Ada 2005 (AI-327): Dynamic ceiling priorities

      --  We rewrite X'Priority as the following run-time call:

      --     Get_Ceiling (X._Object)

      --  Note that although X'Priority is notionally an object, it is quite
      --  deliberately not defined as an aliased object in the RM. This means
      --  that it works fine to rewrite it as a call, without having to worry
      --  about complications that would other arise from X'Priority'Access,
      --  which is illegal, because of the lack of aliasing.

      when Attribute_Priority =>
         declare
            Call           : Node_Id;
            Conctyp        : Entity_Id;
            Object_Parm    : Node_Id;
            Subprg         : Entity_Id;
            RT_Subprg_Name : Node_Id;

         begin
            --  Look for the enclosing concurrent type

            Conctyp := Current_Scope;
            while not Is_Concurrent_Type (Conctyp) loop
               Conctyp := Scope (Conctyp);
            end loop;

            pragma Assert (Is_Protected_Type (Conctyp));

            --  Generate the actual of the call

            Subprg := Current_Scope;
            while not Present (Protected_Body_Subprogram (Subprg)) loop
               Subprg := Scope (Subprg);
            end loop;

            --  Use of 'Priority inside protected entries and barriers (in
            --  both cases the type of the first formal of their expanded
            --  subprogram is Address)

            if Etype (First_Entity (Protected_Body_Subprogram (Subprg)))
              = RTE (RE_Address)
            then
               declare
                  New_Itype : Entity_Id;

               begin
                  --  In the expansion of protected entries the type of the
                  --  first formal of the Protected_Body_Subprogram is an
                  --  Address. In order to reference the _object component
                  --  we generate:

                  --    type T is access p__ptTV;
                  --    freeze T []

                  New_Itype := Create_Itype (E_Access_Type, N);
                  Set_Etype (New_Itype, New_Itype);
                  Set_Directly_Designated_Type (New_Itype,
                    Corresponding_Record_Type (Conctyp));
                  Freeze_Itype (New_Itype, N);

                  --  Generate:
                  --    T!(O)._object'unchecked_access

                  Object_Parm :=
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Unchecked_Convert_To (New_Itype,
                              New_Reference_To
                                (First_Entity
                                  (Protected_Body_Subprogram (Subprg)),
                                 Loc)),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_uObject)),
                       Attribute_Name => Name_Unchecked_Access);
               end;

            --  Use of 'Priority inside a protected subprogram

            else
               Object_Parm :=
                 Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Reference_To
                                    (First_Entity
                                      (Protected_Body_Subprogram (Subprg)),
                                       Loc),
                        Selector_Name => Make_Identifier (Loc, Name_uObject)),
                    Attribute_Name => Name_Unchecked_Access);
            end if;

            --  Select the appropriate run-time subprogram

            if Number_Entries (Conctyp) = 0 then
               RT_Subprg_Name :=
                 New_Reference_To (RTE (RE_Get_Ceiling), Loc);
            else
               RT_Subprg_Name :=
                 New_Reference_To (RTE (RO_PE_Get_Ceiling), Loc);
            end if;

            Call :=
              Make_Function_Call (Loc,
                Name => RT_Subprg_Name,
                Parameter_Associations => New_List (Object_Parm));

            Rewrite (N, Call);

            --  Avoid the generation of extra checks on the pointer to the
            --  protected object.

            Analyze_And_Resolve (N, Typ, Suppress => Access_Check);
         end;

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length => Range_Length : begin

         --  The only special processing required is for the case where
         --  Range_Length is applied to an enumeration type with holes.
         --  In this case we transform

         --     X'Range_Length

         --  to

         --     X'Pos (X'Last) - X'Pos (X'First) + 1

         --  So that the result reflects the proper Pos values instead
         --  of the underlying representations.

         if Is_Enumeration_Type (Ptyp)
           and then Has_Non_Standard_Rep (Ptyp)
         then
            Rewrite (N,
              Make_Op_Add (Loc,
                Left_Opnd =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Pos,
                        Prefix => New_Occurrence_Of (Ptyp, Loc),
                        Expressions => New_List (
                          Make_Attribute_Reference (Loc,
                            Attribute_Name => Name_Last,
                            Prefix => New_Occurrence_Of (Ptyp, Loc)))),

                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Pos,
                        Prefix => New_Occurrence_Of (Ptyp, Loc),
                        Expressions => New_List (
                          Make_Attribute_Reference (Loc,
                            Attribute_Name => Name_First,
                            Prefix => New_Occurrence_Of (Ptyp, Loc))))),

                Right_Opnd => Make_Integer_Literal (Loc, 1)));

            Analyze_And_Resolve (N, Typ);

         --  For all other cases, the attribute is handled by the back end, but
         --  we need to deal with the case of the range check on a universal
         --  integer.

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Range_Length;

      ----------
      -- Read --
      ----------

      when Attribute_Read => Read : declare
         P_Type : constant Entity_Id := Entity (Pref);
         B_Type : constant Entity_Id := Base_Type (P_Type);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Pname  : Entity_Id;
         Decl   : Node_Id;
         Prag   : Node_Id;
         Arg2   : Node_Id;
         Rfunc  : Node_Id;
         Lhs    : Node_Id;
         Rhs    : Node_Id;

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  The simple case, if there is a TSS for Read, just call it

         Pname := Find_Stream_Subprogram (P_Type, TSS_Stream_Read);

         if Present (Pname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Read (stream, Item)

            --  as

            --     Item := sourcetyp (strmread (strmtyp'Input (Stream)));

            --  where strmread is the given Read function that converts an
            --  argument of type strmtyp to type sourcetyp or a type from which
            --  it is derived. The conversion to sourcetyp is required in the
            --  latter case.

            --  A special case arises if Item is a type conversion in which
            --  case, we have to expand to:

            --     Itemx := typex (strmread (strmtyp'Input (Stream)));

            --  where Itemx is the expression of the type conversion (i.e.
            --  the actual object), and typex is the type of Itemx.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg2  := Next (First (Pragma_Argument_Associations (Prag)));
               Rfunc := Entity (Expression (Arg2));
               Lhs := Relocate_Node (Next (First (Exprs)));
               Rhs :=
                 OK_Convert_To (B_Type,
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (Rfunc, Loc),
                     Parameter_Associations => New_List (
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           New_Occurrence_Of
                             (Etype (First_Formal (Rfunc)), Loc),
                         Attribute_Name => Name_Input,
                         Expressions => New_List (
                           Relocate_Node (First (Exprs)))))));

               if Nkind (Lhs) = N_Type_Conversion then
                  Lhs := Expression (Lhs);
                  Rhs := Convert_To (Etype (Lhs), Rhs);
               end if;

               Rewrite (N,
                 Make_Assignment_Statement (Loc,
                   Name       => Lhs,
                   Expression => Rhs));
               Set_Assignment_OK (Lhs);
               Analyze (N);
               return;

            --  For elementary types, we call the I_xxx routine using the first
            --  parameter and then assign the result into the second parameter.
            --  We set Assignment_OK to deal with the conversion case.

            elsif Is_Elementary_Type (U_Type) then
               declare
                  Lhs : Node_Id;
                  Rhs : Node_Id;

               begin
                  Lhs := Relocate_Node (Next (First (Exprs)));
                  Rhs := Build_Elementary_Input_Call (N);

                  if Nkind (Lhs) = N_Type_Conversion then
                     Lhs := Expression (Lhs);
                     Rhs := Convert_To (Etype (Lhs), Rhs);
                  end if;

                  Set_Assignment_OK (Lhs);

                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name       => Lhs,
                      Expression => Rhs));

                  Analyze (N);
                  return;
               end;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Read_Procedure (N, U_Type, Decl, Pname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Tagged type case, use the primitive Read function. Note that
            --  this will dispatch in the class-wide case which is what we want

            elsif Is_Tagged_Type (U_Type) then
               Pname := Find_Prim_Op (U_Type, TSS_Stream_Read);

            --  All other record type cases, including protected records. The
            --  latter only arise for expander generated code for handling
            --  shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Read attribute of an
               --  Unchecked_Union type.

               if Is_Unchecked_Union (Base_Type (U_Type)) then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));
               end if;

               if Has_Discriminants (U_Type)
                 and then Present
                   (Discriminant_Default_Value (First_Discriminant (U_Type)))
               then
                  Build_Mutable_Record_Read_Procedure
                    (Loc, Full_Base (U_Type), Decl, Pname);
               else
                  Build_Record_Read_Procedure
                    (Loc, Full_Base (U_Type), Decl, Pname);
               end if;

               --  Suppress checks, uninitialized or otherwise invalid
               --  data does not cause constraint errors to be raised for
               --  a complete record read.

               Insert_Action (N, Decl, All_Checks);
            end if;
         end if;

         Rewrite_Stream_Proc_Call (Pname);
      end Read;

      ---------
      -- Ref --
      ---------

      --  Ref is identical to To_Address, see To_Address for processing

      ---------------
      -- Remainder --
      ---------------

      --  Transforms 'Remainder into a call to the floating-point attribute
      --  function Remainder in Fat_xxx (where xxx is the root type)

      when Attribute_Remainder =>
         Expand_Fpt_Attribute_RR (N);

      ------------
      -- Result --
      ------------

      --  Transform 'Result into reference to _Result formal. At the point
      --  where a legal 'Result attribute is expanded, we know that we are in
      --  the context of a _Postcondition function with a _Result parameter.

      when Attribute_Result =>
         Rewrite (N, Make_Identifier (Loc, Chars => Name_uResult));
         Analyze_And_Resolve (N, Typ);

      -----------
      -- Round --
      -----------

      --  The handling of the Round attribute is quite delicate. The processing
      --  in Sem_Attr introduced a conversion to universal real, reflecting the
      --  semantics of Round, but we do not want anything to do with universal
      --  real at runtime, since this corresponds to using floating-point
      --  arithmetic.

      --  What we have now is that the Etype of the Round attribute correctly
      --  indicates the final result type. The operand of the Round is the
      --  conversion to universal real, described above, and the operand of
      --  this conversion is the actual operand of Round, which may be the
      --  special case of a fixed point multiplication or division (Etype =
      --  universal fixed)

      --  The exapander will expand first the operand of the conversion, then
      --  the conversion, and finally the round attribute itself, since we
      --  always work inside out. But we cannot simply process naively in this
      --  order. In the semantic world where universal fixed and real really
      --  exist and have infinite precision, there is no problem, but in the
      --  implementation world, where universal real is a floating-point type,
      --  we would get the wrong result.

      --  So the approach is as follows. First, when expanding a multiply or
      --  divide whose type is universal fixed, we do nothing at all, instead
      --  deferring the operation till later.

      --  The actual processing is done in Expand_N_Type_Conversion which
      --  handles the special case of Round by looking at its parent to see if
      --  it is a Round attribute, and if it is, handling the conversion (or
      --  its fixed multiply/divide child) in an appropriate manner.

      --  This means that by the time we get to expanding the Round attribute
      --  itself, the Round is nothing more than a type conversion (and will
      --  often be a null type conversion), so we just replace it with the
      --  appropriate conversion operation.

      when Attribute_Round =>
         Rewrite (N,
           Convert_To (Etype (N), Relocate_Node (First (Exprs))));
         Analyze_And_Resolve (N);

      --------------
      -- Rounding --
      --------------

      --  Transforms 'Rounding into a call to the floating-point attribute
      --  function Rounding in Fat_xxx (where xxx is the root type)

      when Attribute_Rounding =>
         Expand_Fpt_Attribute_R (N);

      ------------------
      -- Same_Storage --
      ------------------

      when Attribute_Same_Storage => Same_Storage : declare
         Loc : constant Source_Ptr := Sloc (N);

         X   : constant Node_Id := Prefix (N);
         Y   : constant Node_Id := First (Expressions (N));
         --  The arguments

         X_Addr, Y_Addr : Node_Id;
         --  Rhe expressions for their addresses

         X_Size, Y_Size : Node_Id;
         --  Rhe expressions for their sizes

      begin
         --  The attribute is expanded as:

         --    (X'address = Y'address)
         --      and then (X'Size = Y'Size)

         --  If both arguments have the same Etype the second conjunct can be
         --  omitted.

         X_Addr :=
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Address,
             Prefix         => New_Copy_Tree (X));

         Y_Addr :=
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Address,
             Prefix         => New_Copy_Tree (Y));

         X_Size :=
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Size,
             Prefix         => New_Copy_Tree (X));

         Y_Size :=
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Size,
             Prefix         => New_Copy_Tree (Y));

         if Etype (X) = Etype (Y) then
            Rewrite (N,
              (Make_Op_Eq (Loc,
                 Left_Opnd  => X_Addr,
                 Right_Opnd => Y_Addr)));
         else
            Rewrite (N,
              Make_Op_And (Loc,
                Left_Opnd  =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  => X_Addr,
                    Right_Opnd => Y_Addr),
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  => X_Size,
                    Right_Opnd => Y_Size)));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
      end Same_Storage;

      -------------
      -- Scaling --
      -------------

      --  Transforms 'Scaling into a call to the floating-point attribute
      --  function Scaling in Fat_xxx (where xxx is the root type)

      when Attribute_Scaling =>
         Expand_Fpt_Attribute_RI (N);

      -------------------------
      -- Simple_Storage_Pool --
      -------------------------

      when Attribute_Simple_Storage_Pool =>
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Reference_To (Etype (N), Loc),
             Expression   => New_Reference_To (Entity (N), Loc)));
         Analyze_And_Resolve (N, Typ);

      ----------
      -- Size --
      ----------

      when Attribute_Size        |
           Attribute_Object_Size |
           Attribute_Value_Size  |
           Attribute_VADS_Size   => Size :

      declare
         Siz      : Uint;
         New_Node : Node_Id;

      begin
         --  Processing for VADS_Size case. Note that this processing removes
         --  all traces of VADS_Size from the tree, and completes all required
         --  processing for VADS_Size by translating the attribute reference
         --  to an appropriate Size or Object_Size reference.

         if Id = Attribute_VADS_Size
           or else (Use_VADS_Size and then Id = Attribute_Size)
         then
            --  If the size is specified, then we simply use the specified
            --  size. This applies to both types and objects. The size of an
            --  object can be specified in the following ways:

            --    An explicit size object is given for an object
            --    A component size is specified for an indexed component
            --    A component clause is specified for a selected component
            --    The object is a component of a packed composite object

            --  If the size is specified, then VADS_Size of an object

            if (Is_Entity_Name (Pref)
                 and then Present (Size_Clause (Entity (Pref))))
              or else
                (Nkind (Pref) = N_Component_Clause
                  and then (Present (Component_Clause
                                     (Entity (Selector_Name (Pref))))
                             or else Is_Packed (Etype (Prefix (Pref)))))
              or else
                (Nkind (Pref) = N_Indexed_Component
                  and then (Component_Size (Etype (Prefix (Pref))) /= 0
                             or else Is_Packed (Etype (Prefix (Pref)))))
            then
               Set_Attribute_Name (N, Name_Size);

            --  Otherwise if we have an object rather than a type, then the
            --  VADS_Size attribute applies to the type of the object, rather
            --  than the object itself. This is one of the respects in which
            --  VADS_Size differs from Size.

            else
               if (not Is_Entity_Name (Pref)
                    or else not Is_Type (Entity (Pref)))
                 and then (Is_Scalar_Type (Ptyp) or else Is_Constrained (Ptyp))
               then
                  Rewrite (Pref, New_Occurrence_Of (Ptyp, Loc));
               end if;

               --  For a scalar type for which no size was explicitly given,
               --  VADS_Size means Object_Size. This is the other respect in
               --  which VADS_Size differs from Size.

               if Is_Scalar_Type (Ptyp) and then No (Size_Clause (Ptyp)) then
                  Set_Attribute_Name (N, Name_Object_Size);

               --  In all other cases, Size and VADS_Size are the sane

               else
                  Set_Attribute_Name (N, Name_Size);
               end if;
            end if;
         end if;

         --  For class-wide types, X'Class'Size is transformed into a direct
         --  reference to the Size of the class type, so that the back end does
         --  not have to deal with the X'Class'Size reference.

         if Is_Entity_Name (Pref)
           and then Is_Class_Wide_Type (Entity (Pref))
         then
            Rewrite (Prefix (N), New_Occurrence_Of (Entity (Pref), Loc));
            return;

         --  For X'Size applied to an object of a class-wide type, transform
         --  X'Size into a call to the primitive operation _Size applied to X.

         elsif Is_Class_Wide_Type (Ptyp)
           or else (Id = Attribute_Size
                      and then Is_Tagged_Type (Ptyp)
                      and then Has_Unknown_Discriminants (Ptyp))
         then
            --  No need to do anything else compiling under restriction
            --  No_Dispatching_Calls. During the semantic analysis we
            --  already notified such violation.

            if Restriction_Active (No_Dispatching_Calls) then
               return;
            end if;

            New_Node :=
              Make_Function_Call (Loc,
                Name => New_Reference_To
                  (Find_Prim_Op (Ptyp, Name_uSize), Loc),
                Parameter_Associations => New_List (Pref));

            if Typ /= Standard_Long_Long_Integer then

               --  The context is a specific integer type with which the
               --  original attribute was compatible. The function has a
               --  specific type as well, so to preserve the compatibility
               --  we must convert explicitly.

               New_Node := Convert_To (Typ, New_Node);
            end if;

            Rewrite (N, New_Node);
            Analyze_And_Resolve (N, Typ);
            return;

         --  Case of known RM_Size of a type

         elsif (Id = Attribute_Size or else Id = Attribute_Value_Size)
           and then Is_Entity_Name (Pref)
           and then Is_Type (Entity (Pref))
           and then Known_Static_RM_Size (Entity (Pref))
         then
            Siz := RM_Size (Entity (Pref));

         --  Case of known Esize of a type

         elsif Id = Attribute_Object_Size
           and then Is_Entity_Name (Pref)
           and then Is_Type (Entity (Pref))
           and then Known_Static_Esize (Entity (Pref))
         then
            Siz := Esize (Entity (Pref));

         --  Case of known size of object

         elsif Id = Attribute_Size
           and then Is_Entity_Name (Pref)
           and then Is_Object (Entity (Pref))
           and then Known_Esize (Entity (Pref))
           and then Known_Static_Esize (Entity (Pref))
         then
            Siz := Esize (Entity (Pref));

         --  For an array component, we can do Size in the front end
         --  if the component_size of the array is set.

         elsif Nkind (Pref) = N_Indexed_Component then
            Siz := Component_Size (Etype (Prefix (Pref)));

         --  For a record component, we can do Size in the front end if there
         --  is a component clause, or if the record is packed and the
         --  component's size is known at compile time.

         elsif Nkind (Pref) = N_Selected_Component then
            declare
               Rec  : constant Entity_Id := Etype (Prefix (Pref));
               Comp : constant Entity_Id := Entity (Selector_Name (Pref));

            begin
               if Present (Component_Clause (Comp)) then
                  Siz := Esize (Comp);

               elsif Is_Packed (Rec) then
                  Siz := RM_Size (Ptyp);

               else
                  Apply_Universal_Integer_Attribute_Checks (N);
                  return;
               end if;
            end;

         --  All other cases are handled by the back end

         else
            Apply_Universal_Integer_Attribute_Checks (N);

            --  If Size is applied to a formal parameter that is of a packed
            --  array subtype, then apply Size to the actual subtype.

            if Is_Entity_Name (Pref)
              and then Is_Formal (Entity (Pref))
              and then Is_Array_Type (Ptyp)
              and then Is_Packed (Ptyp)
            then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Occurrence_Of (Get_Actual_Subtype (Pref), Loc),
                   Attribute_Name => Name_Size));
               Analyze_And_Resolve (N, Typ);
            end if;

            --  If Size applies to a dereference of an access to unconstrained
            --  packed array, the back end needs to see its unconstrained
            --  nominal type, but also a hint to the actual constrained type.

            if Nkind (Pref) = N_Explicit_Dereference
              and then Is_Array_Type (Ptyp)
              and then not Is_Constrained (Ptyp)
              and then Is_Packed (Ptyp)
            then
               Set_Actual_Designated_Subtype (Pref,
                 Get_Actual_Subtype (Pref));
            end if;

            return;
         end if;

         --  Common processing for record and array component case

         if Siz /= No_Uint and then Siz /= 0 then
            declare
               CS : constant Boolean := Comes_From_Source (N);

            begin
               Rewrite (N, Make_Integer_Literal (Loc, Siz));

               --  This integer literal is not a static expression. We do not
               --  call Analyze_And_Resolve here, because this would activate
               --  the circuit for deciding that a static value was out of
               --  range, and we don't want that.

               --  So just manually set the type, mark the expression as non-
               --  static, and then ensure that the result is checked properly
               --  if the attribute comes from source (if it was internally
               --  generated, we never need a constraint check).

               Set_Etype (N, Typ);
               Set_Is_Static_Expression (N, False);

               if CS then
                  Apply_Constraint_Check (N, Typ);
               end if;
            end;
         end if;
      end Size;

      ------------------
      -- Storage_Pool --
      ------------------

      when Attribute_Storage_Pool =>
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Reference_To (Etype (N), Loc),
             Expression   => New_Reference_To (Entity (N), Loc)));
         Analyze_And_Resolve (N, Typ);

      ------------------
      -- Storage_Size --
      ------------------

      when Attribute_Storage_Size => Storage_Size : declare
         Alloc_Op  : Entity_Id := Empty;

      begin

         --  Access type case, always go to the root type

         --  The case of access types results in a value of zero for the case
         --  where no storage size attribute clause has been given. If a
         --  storage size has been given, then the attribute is converted
         --  to a reference to the variable used to hold this value.

         if Is_Access_Type (Ptyp) then
            if Present (Storage_Size_Variable (Root_Type (Ptyp))) then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Typ, Loc),
                   Attribute_Name => Name_Max,
                   Expressions => New_List (
                     Make_Integer_Literal (Loc, 0),
                     Convert_To (Typ,
                       New_Reference_To
                         (Storage_Size_Variable (Root_Type (Ptyp)), Loc)))));

            elsif Present (Associated_Storage_Pool (Root_Type (Ptyp))) then

               --  If the access type is associated with a simple storage pool
               --  object, then attempt to locate the optional Storage_Size
               --  function of the simple storage pool type. If not found,
               --  then the result will default to zero.

               if Present (Get_Rep_Pragma (Root_Type (Ptyp),
                                           Name_Simple_Storage_Pool_Type))
               then
                  declare
                     Pool_Type : constant Entity_Id :=
                                   Base_Type (Etype (Entity (N)));

                  begin
                     Alloc_Op := Get_Name_Entity_Id (Name_Storage_Size);
                     while Present (Alloc_Op) loop
                        if Scope (Alloc_Op) = Scope (Pool_Type)
                          and then Present (First_Formal (Alloc_Op))
                          and then Etype (First_Formal (Alloc_Op)) = Pool_Type
                        then
                           exit;
                        end if;

                        Alloc_Op := Homonym (Alloc_Op);
                     end loop;
                  end;

               --  In the normal Storage_Pool case, retrieve the primitive
               --  function associated with the pool type.

               else
                  Alloc_Op :=
                    Find_Prim_Op
                      (Etype (Associated_Storage_Pool (Root_Type (Ptyp))),
                       Attribute_Name (N));
               end if;

               --  If Storage_Size wasn't found (can only occur in the simple
               --  storage pool case), then simply use zero for the result.

               if not Present (Alloc_Op) then
                  Rewrite (N, Make_Integer_Literal (Loc, 0));

               --  Otherwise, rewrite the allocator as a call to pool type's
               --  Storage_Size function.

               else
                  Rewrite (N,
                    OK_Convert_To (Typ,
                      Make_Function_Call (Loc,
                        Name =>
                          New_Reference_To (Alloc_Op, Loc),

                        Parameter_Associations => New_List (
                          New_Reference_To
                            (Associated_Storage_Pool
                               (Root_Type (Ptyp)), Loc)))));
               end if;

            else
               Rewrite (N, Make_Integer_Literal (Loc, 0));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  For tasks, we retrieve the size directly from the TCB. The
         --  size may depend on a discriminant of the type, and therefore
         --  can be a per-object expression, so type-level information is
         --  not sufficient in general. There are four cases to consider:

         --  a) If the attribute appears within a task body, the designated
         --    TCB is obtained by a call to Self.

         --  b) If the prefix of the attribute is the name of a task object,
         --  the designated TCB is the one stored in the corresponding record.

         --  c) If the prefix is a task type, the size is obtained from the
         --  size variable created for each task type

         --  d) If no storage_size was specified for the type , there is no
         --  size variable, and the value is a system-specific default.

         else
            if In_Open_Scopes (Ptyp) then

               --  Storage_Size (Self)

               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Occurrence_Of (RTE (RE_Storage_Size), Loc),
                     Parameter_Associations =>
                       New_List (
                         Make_Function_Call (Loc,
                           Name =>
                             New_Reference_To (RTE (RE_Self), Loc))))));

            elsif not Is_Entity_Name (Pref)
              or else not Is_Type (Entity (Pref))
            then
               --  Storage_Size (Rec (Obj).Size)

               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Occurrence_Of (RTE (RE_Storage_Size), Loc),
                       Parameter_Associations =>
                          New_List (
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Unchecked_Convert_To (
                                  Corresponding_Record_Type (Ptyp),
                                    New_Copy_Tree (Pref)),
                              Selector_Name =>
                                 Make_Identifier (Loc, Name_uTask_Id))))));

            elsif Present (Storage_Size_Variable (Ptyp)) then

               --  Static storage size pragma given for type: retrieve value
               --  from its allocated storage variable.

               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (
                       RTE (RE_Adjust_Storage_Size), Loc),
                     Parameter_Associations =>
                       New_List (
                         New_Reference_To (
                           Storage_Size_Variable (Ptyp), Loc)))));
            else
               --  Get system default

               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Occurrence_Of (
                        RTE (RE_Default_Stack_Size), Loc))));
            end if;

            Analyze_And_Resolve (N, Typ);
         end if;
      end Storage_Size;

      -----------------
      -- Stream_Size --
      -----------------

      when Attribute_Stream_Size =>
         Rewrite (N,
           Make_Integer_Literal (Loc, Intval => Get_Stream_Size (Ptyp)));
         Analyze_And_Resolve (N, Typ);

      ----------
      -- Succ --
      ----------

      --  1. Deal with enumeration types with holes
      --  2. For floating-point, generate call to attribute function
      --  3. For other cases, deal with constraint checking

      when Attribute_Succ => Succ : declare
         Etyp : constant Entity_Id := Base_Type (Ptyp);

      begin

         --  For enumeration types with non-standard representations, we
         --  expand typ'Succ (x) into

         --    Pos_To_Rep (Rep_To_Pos (x) + 1)

         --    If the representation is contiguous, we compute instead
         --    Lit1 + Rep_to_Pos (x+1), to catch invalid representations.

         if Is_Enumeration_Type (Ptyp)
           and then Present (Enum_Pos_To_Rep (Etyp))
         then
            if Has_Contiguous_Rep (Etyp) then
               Rewrite (N,
                  Unchecked_Convert_To (Ptyp,
                     Make_Op_Add (Loc,
                        Left_Opnd  =>
                         Make_Integer_Literal (Loc,
                           Enumeration_Rep (First_Literal (Ptyp))),
                        Right_Opnd =>
                          Make_Function_Call (Loc,
                            Name =>
                              New_Reference_To
                               (TSS (Etyp, TSS_Rep_To_Pos), Loc),

                            Parameter_Associations =>
                              New_List (
                                Unchecked_Convert_To (Ptyp,
                                  Make_Op_Add (Loc,
                                  Left_Opnd =>
                                    Unchecked_Convert_To (Standard_Integer,
                                      Relocate_Node (First (Exprs))),
                                  Right_Opnd =>
                                    Make_Integer_Literal (Loc, 1))),
                                Rep_To_Pos_Flag (Ptyp, Loc))))));
            else
               --  Add Boolean parameter True, to request program errror if
               --  we have a bad representation on our hands. Add False if
               --  checks are suppressed.

               Append_To (Exprs, Rep_To_Pos_Flag (Ptyp, Loc));
               Rewrite (N,
                 Make_Indexed_Component (Loc,
                   Prefix =>
                     New_Reference_To
                       (Enum_Pos_To_Rep (Etyp), Loc),
                   Expressions => New_List (
                     Make_Op_Add (Loc,
                       Left_Opnd =>
                         Make_Function_Call (Loc,
                           Name =>
                             New_Reference_To
                               (TSS (Etyp, TSS_Rep_To_Pos), Loc),
                           Parameter_Associations => Exprs),
                       Right_Opnd => Make_Integer_Literal (Loc, 1)))));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  For floating-point, we transform 'Succ into a call to the Succ
         --  floating-point attribute function in Fat_xxx (xxx is root type)

         elsif Is_Floating_Point_Type (Ptyp) then
            Expand_Fpt_Attribute_R (N);
            Analyze_And_Resolve (N, Typ);

         --  For modular types, nothing to do (no overflow, since wraps)

         elsif Is_Modular_Integer_Type (Ptyp) then
            null;

         --  For other types, if argument is marked as needing a range check or
         --  overflow checking is enabled, we must generate a check.

         elsif not Overflow_Checks_Suppressed (Ptyp)
           or else Do_Range_Check (First (Exprs))
         then
            Set_Do_Range_Check (First (Exprs), False);
            Expand_Pred_Succ (N);
         end if;
      end Succ;

      ---------
      -- Tag --
      ---------

      --  Transforms X'Tag into a direct reference to the tag of X

      when Attribute_Tag => Tag : declare
         Ttyp           : Entity_Id;
         Prefix_Is_Type : Boolean;

      begin
         if Is_Entity_Name (Pref) and then Is_Type (Entity (Pref)) then
            Ttyp := Entity (Pref);
            Prefix_Is_Type := True;
         else
            Ttyp := Ptyp;
            Prefix_Is_Type := False;
         end if;

         if Is_Class_Wide_Type (Ttyp) then
            Ttyp := Root_Type (Ttyp);
         end if;

         Ttyp := Underlying_Type (Ttyp);

         --  Ada 2005: The type may be a synchronized tagged type, in which
         --  case the tag information is stored in the corresponding record.

         if Is_Concurrent_Type (Ttyp) then
            Ttyp := Corresponding_Record_Type (Ttyp);
         end if;

         if Prefix_Is_Type then

            --  For VMs we leave the type attribute unexpanded because
            --  there's not a dispatching table to reference.

            if Tagged_Type_Expansion then
               Rewrite (N,
                 Unchecked_Convert_To (RTE (RE_Tag),
                   New_Reference_To
                     (Node (First_Elmt (Access_Disp_Table (Ttyp))), Loc)));
               Analyze_And_Resolve (N, RTE (RE_Tag));
            end if;

         --  Ada 2005 (AI-251): The use of 'Tag in the sources always
         --  references the primary tag of the actual object. If 'Tag is
         --  applied to class-wide interface objects we generate code that
         --  displaces "this" to reference the base of the object.

         elsif Comes_From_Source (N)
            and then Is_Class_Wide_Type (Etype (Prefix (N)))
            and then Is_Interface (Etype (Prefix (N)))
         then
            --  Generate:
            --    (To_Tag_Ptr (Prefix'Address)).all

            --  Note that Prefix'Address is recursively expanded into a call
            --  to Base_Address (Obj.Tag)

            --  Not needed for VM targets, since all handled by the VM

            if Tagged_Type_Expansion then
               Rewrite (N,
                 Make_Explicit_Dereference (Loc,
                   Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                     Make_Attribute_Reference (Loc,
                       Prefix => Relocate_Node (Pref),
                       Attribute_Name => Name_Address))));
               Analyze_And_Resolve (N, RTE (RE_Tag));
            end if;

         else
            Rewrite (N,
              Make_Selected_Component (Loc,
                Prefix => Relocate_Node (Pref),
                Selector_Name =>
                  New_Reference_To (First_Tag_Component (Ttyp), Loc)));
            Analyze_And_Resolve (N, RTE (RE_Tag));
         end if;
      end Tag;

      ----------------
      -- Terminated --
      ----------------

      --  Transforms 'Terminated attribute into a call to Terminated function

      when Attribute_Terminated => Terminated :
      begin
         --  The prefix of Terminated is of a task interface class-wide type.
         --  Generate:
         --    terminated (Task_Id (Pref._disp_get_task_id));

         if Ada_Version >= Ada_2005
           and then Ekind (Ptyp) = E_Class_Wide_Type
           and then Is_Interface (Ptyp)
           and then Is_Task_Interface (Ptyp)
         then
            Rewrite (N,
              Make_Function_Call (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Terminated), Loc),
                Parameter_Associations => New_List (
                  Make_Unchecked_Type_Conversion (Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RO_ST_Task_Id), Loc),
                    Expression =>
                      Make_Selected_Component (Loc,
                        Prefix =>
                          New_Copy_Tree (Pref),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_uDisp_Get_Task_Id))))));

         elsif Restricted_Profile then
            Rewrite (N,
              Build_Call_With_Task (Pref, RTE (RE_Restricted_Terminated)));

         else
            Rewrite (N,
              Build_Call_With_Task (Pref, RTE (RE_Terminated)));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
      end Terminated;

      ----------------
      -- To_Address --
      ----------------

      --  Transforms System'To_Address (X) and System.Address'Ref (X) into
      --  unchecked conversion from (integral) type of X to type address.

      when Attribute_To_Address | Attribute_Ref =>
         Rewrite (N,
           Unchecked_Convert_To (RTE (RE_Address),
             Relocate_Node (First (Exprs))));
         Analyze_And_Resolve (N, RTE (RE_Address));

      ------------
      -- To_Any --
      ------------

      when Attribute_To_Any => To_Any : declare
         P_Type : constant Entity_Id := Etype (Pref);
         Decls  : constant List_Id   := New_List;
      begin
         Rewrite (N,
           Build_To_Any_Call
             (Loc,
              Convert_To (P_Type,
              Relocate_Node (First (Exprs))), Decls));
         Insert_Actions (N, Decls);
         Analyze_And_Resolve (N, RTE (RE_Any));
      end To_Any;

      ----------------
      -- Truncation --
      ----------------

      --  Transforms 'Truncation into a call to the floating-point attribute
      --  function Truncation in Fat_xxx (where xxx is the root type).
      --  Expansion is avoided for cases the back end can handle directly.

      when Attribute_Truncation =>
         if not Is_Inline_Floating_Point_Attribute (N) then
            Expand_Fpt_Attribute_R (N);
         end if;

      --------------
      -- TypeCode --
      --------------

      when Attribute_TypeCode => TypeCode : declare
         P_Type : constant Entity_Id := Etype (Pref);
         Decls  : constant List_Id   := New_List;
      begin
         Rewrite (N, Build_TypeCode_Call (Loc, P_Type, Decls));
         Insert_Actions (N, Decls);
         Analyze_And_Resolve (N, RTE (RE_TypeCode));
      end TypeCode;

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      --  Transforms 'Unbiased_Rounding into a call to the floating-point
      --  attribute function Unbiased_Rounding in Fat_xxx (where xxx is the
      --  root type). Expansion is avoided for cases the back end can handle
      --  directly.

      when Attribute_Unbiased_Rounding =>
         if not Is_Inline_Floating_Point_Attribute (N) then
            Expand_Fpt_Attribute_R (N);
         end if;

      -----------------
      -- UET_Address --
      -----------------

      when Attribute_UET_Address => UET_Address : declare
         Ent : constant Entity_Id := Make_Temporary (Loc, 'T');

      begin
         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Address), Loc)));

         --  Construct name __gnat_xxx__SDP, where xxx is the unit name
         --  in normal external form.

         Get_External_Unit_Name_String (Get_Unit_Name (Pref));
         Name_Buffer (1 + 7 .. Name_Len + 7) := Name_Buffer (1 .. Name_Len);
         Name_Len := Name_Len + 7;
         Name_Buffer (1 .. 7) := "__gnat_";
         Name_Buffer (Name_Len + 1 .. Name_Len + 5) := "__SDP";
         Name_Len := Name_Len + 5;

         Set_Is_Imported (Ent);
         Set_Interface_Name (Ent,
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));

         --  Set entity as internal to ensure proper Sprint output of its
         --  implicit importation.

         Set_Is_Internal (Ent);

         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Ent, Loc),
             Attribute_Name => Name_Address));

         Analyze_And_Resolve (N, Typ);
      end UET_Address;

      ------------
      -- Update --
      ------------

      when Attribute_Update =>
         Expand_Update_Attribute (N);

      ---------------
      -- VADS_Size --
      ---------------

      --  The processing for VADS_Size is shared with Size

      ---------
      -- Val --
      ---------

      --  For enumeration types with a standard representation, and for all
      --  other types, Val is handled by the back end. For enumeration types
      --  with a non-standard representation we use the _Pos_To_Rep array that
      --  was created when the type was frozen.

      when Attribute_Val => Val : declare
         Etyp : constant Entity_Id := Base_Type (Entity (Pref));

      begin
         if Is_Enumeration_Type (Etyp)
           and then Present (Enum_Pos_To_Rep (Etyp))
         then
            if Has_Contiguous_Rep (Etyp) then
               declare
                  Rep_Node : constant Node_Id :=
                    Unchecked_Convert_To (Etyp,
                       Make_Op_Add (Loc,
                         Left_Opnd =>
                            Make_Integer_Literal (Loc,
                              Enumeration_Rep (First_Literal (Etyp))),
                         Right_Opnd =>
                          (Convert_To (Standard_Integer,
                             Relocate_Node (First (Exprs))))));

               begin
                  Rewrite (N,
                     Unchecked_Convert_To (Etyp,
                         Make_Op_Add (Loc,
                           Left_Opnd =>
                             Make_Integer_Literal (Loc,
                               Enumeration_Rep (First_Literal (Etyp))),
                           Right_Opnd =>
                             Make_Function_Call (Loc,
                               Name =>
                                 New_Reference_To
                                   (TSS (Etyp, TSS_Rep_To_Pos), Loc),
                               Parameter_Associations => New_List (
                                 Rep_Node,
                                 Rep_To_Pos_Flag (Etyp, Loc))))));
               end;

            else
               Rewrite (N,
                 Make_Indexed_Component (Loc,
                   Prefix => New_Reference_To (Enum_Pos_To_Rep (Etyp), Loc),
                   Expressions => New_List (
                     Convert_To (Standard_Integer,
                       Relocate_Node (First (Exprs))))));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  If the argument is marked as requiring a range check then generate
         --  it here.

         elsif Do_Range_Check (First (Exprs)) then
            Set_Do_Range_Check (First (Exprs), False);
            Generate_Range_Check (First (Exprs), Etyp, CE_Range_Check_Failed);
         end if;
      end Val;

      -----------
      -- Valid --
      -----------

      --  The code for valid is dependent on the particular types involved.
      --  See separate sections below for the generated code in each case.

      when Attribute_Valid => Valid : declare
         Btyp : Entity_Id := Base_Type (Ptyp);
         Tst  : Node_Id;

         Save_Validity_Checks_On : constant Boolean := Validity_Checks_On;
         --  Save the validity checking mode. We always turn off validity
         --  checking during process of 'Valid since this is one place
         --  where we do not want the implicit validity checks to intefere
         --  with the explicit validity check that the programmer is doing.

         function Make_Range_Test return Node_Id;
         --  Build the code for a range test of the form
         --    Btyp!(Pref) in Btyp!(Ptyp'First) .. Btyp!(Ptyp'Last)

         ---------------------
         -- Make_Range_Test --
         ---------------------

         function Make_Range_Test return Node_Id is
            Temp : constant Node_Id := Duplicate_Subexpr (Pref);

         begin
            --  The value whose validity is being checked has been captured in
            --  an object declaration. We certainly don't want this object to
            --  appear valid because the declaration initializes it!

            if Is_Entity_Name (Temp) then
               Set_Is_Known_Valid (Entity (Temp), False);
            end if;

            return
              Make_In (Loc,
                Left_Opnd  =>
                  Unchecked_Convert_To (Btyp, Temp),
                Right_Opnd =>
                  Make_Range (Loc,
                    Low_Bound =>
                      Unchecked_Convert_To (Btyp,
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Ptyp, Loc),
                          Attribute_Name => Name_First)),
                    High_Bound =>
                      Unchecked_Convert_To (Btyp,
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Ptyp, Loc),
                          Attribute_Name => Name_Last))));
         end Make_Range_Test;

      --  Start of processing for Attribute_Valid

      begin
         --  Do not expand sourced code 'Valid reference in CodePeer mode,
         --  will be handled by the back-end directly.

         if CodePeer_Mode and then Comes_From_Source (N) then
            return;
         end if;

         --  Turn off validity checks. We do not want any implicit validity
         --  checks to intefere with the explicit check from the attribute

         Validity_Checks_On := False;

         --  Retrieve the base type. Handle the case where the base type is a
         --  private enumeration type.

         if Is_Private_Type (Btyp) and then Present (Full_View (Btyp)) then
            Btyp := Full_View (Btyp);
         end if;

         --  Floating-point case. This case is handled by the Valid attribute
         --  code in the floating-point attribute run-time library.

         if Is_Floating_Point_Type (Ptyp) then
            declare
               Pkg : RE_Id;
               Ftp : Entity_Id;

            begin
               case Float_Rep (Btyp) is

                  --  For vax fpt types, call appropriate routine in special
                  --  vax floating point unit. No need to worry about loads in
                  --  this case, since these types have no signalling NaN's.

                  when VAX_Native => Expand_Vax_Valid (N);

                  --  The AAMP back end handles Valid for floating-point types

                  when AAMP =>
                     Analyze_And_Resolve (Pref, Ptyp);
                     Set_Etype (N, Standard_Boolean);
                     Set_Analyzed (N);

                  when IEEE_Binary =>
                     Find_Fat_Info (Ptyp, Ftp, Pkg);

                     --  If the floating-point object might be unaligned, we
                     --  need to call the special routine Unaligned_Valid,
                     --  which makes the needed copy, being careful not to
                     --  load the value into any floating-point register.
                     --  The argument in this case is obj'Address (see
                     --  Unaligned_Valid routine in Fat_Gen).

                     if Is_Possibly_Unaligned_Object (Pref) then
                        Expand_Fpt_Attribute
                          (N, Pkg, Name_Unaligned_Valid,
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => Relocate_Node (Pref),
                               Attribute_Name => Name_Address)));

                     --  In the normal case where we are sure the object is
                     --  aligned, we generate a call to Valid, and the argument
                     --  in this case is obj'Unrestricted_Access (after
                     --  converting obj to the right floating-point type).

                     else
                        Expand_Fpt_Attribute
                          (N, Pkg, Name_Valid,
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => Unchecked_Convert_To (Ftp, Pref),
                               Attribute_Name => Name_Unrestricted_Access)));
                     end if;
               end case;

               --  One more task, we still need a range check. Required
               --  only if we have a constraint, since the Valid routine
               --  catches infinities properly (infinities are never valid).

               --  The way we do the range check is simply to create the
               --  expression: Valid (N) and then Base_Type(Pref) in Typ.

               if not Subtypes_Statically_Match (Ptyp, Btyp) then
                  Rewrite (N,
                    Make_And_Then (Loc,
                      Left_Opnd  => Relocate_Node (N),
                      Right_Opnd =>
                        Make_In (Loc,
                          Left_Opnd => Convert_To (Btyp, Pref),
                          Right_Opnd => New_Occurrence_Of (Ptyp, Loc))));
               end if;
            end;

         --  Enumeration type with holes

         --  For enumeration types with holes, the Pos value constructed by
         --  the Enum_Rep_To_Pos function built in Exp_Ch3 called with a
         --  second argument of False returns minus one for an invalid value,
         --  and the non-negative pos value for a valid value, so the
         --  expansion of X'Valid is simply:

         --     type(X)'Pos (X) >= 0

         --  We can't quite generate it that way because of the requirement
         --  for the non-standard second argument of False in the resulting
         --  rep_to_pos call, so we have to explicitly create:

         --     _rep_to_pos (X, False) >= 0

         --  If we have an enumeration subtype, we also check that the
         --  value is in range:

         --    _rep_to_pos (X, False) >= 0
         --      and then
         --       (X >= type(X)'First and then type(X)'Last <= X)

         elsif Is_Enumeration_Type (Ptyp)
           and then Present (Enum_Pos_To_Rep (Btyp))
         then
            Tst :=
              Make_Op_Ge (Loc,
                Left_Opnd =>
                  Make_Function_Call (Loc,
                    Name =>
                      New_Reference_To (TSS (Btyp, TSS_Rep_To_Pos), Loc),
                    Parameter_Associations => New_List (
                      Pref,
                      New_Occurrence_Of (Standard_False, Loc))),
                Right_Opnd => Make_Integer_Literal (Loc, 0));

            if Ptyp /= Btyp
              and then
                (Type_Low_Bound (Ptyp) /= Type_Low_Bound (Btyp)
                  or else
                 Type_High_Bound (Ptyp) /= Type_High_Bound (Btyp))
            then
               --  The call to Make_Range_Test will create declarations
               --  that need a proper insertion point, but Pref is now
               --  attached to a node with no ancestor. Attach to tree
               --  even if it is to be rewritten below.

               Set_Parent (Tst, Parent (N));

               Tst :=
                 Make_And_Then (Loc,
                   Left_Opnd  => Make_Range_Test,
                   Right_Opnd => Tst);
            end if;

            Rewrite (N, Tst);

         --  Fortran convention booleans

         --  For the very special case of Fortran convention booleans, the
         --  value is always valid, since it is an integer with the semantics
         --  that non-zero is true, and any value is permissible.

         elsif Is_Boolean_Type (Ptyp)
           and then Convention (Ptyp) = Convention_Fortran
         then
            Rewrite (N, New_Occurrence_Of (Standard_True, Loc));

         --  For biased representations, we will be doing an unchecked
         --  conversion without unbiasing the result. That means that the range
         --  test has to take this into account, and the proper form of the
         --  test is:

         --    Btyp!(Pref) < Btyp!(Ptyp'Range_Length)

         elsif Has_Biased_Representation (Ptyp) then
            Btyp := RTE (RE_Unsigned_32);
            Rewrite (N,
              Make_Op_Lt (Loc,
                Left_Opnd =>
                  Unchecked_Convert_To (Btyp, Duplicate_Subexpr (Pref)),
                Right_Opnd =>
                  Unchecked_Convert_To (Btyp,
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Ptyp, Loc),
                      Attribute_Name => Name_Range_Length))));

         --  For all other scalar types, what we want logically is a
         --  range test:

         --     X in type(X)'First .. type(X)'Last

         --  But that's precisely what won't work because of possible
         --  unwanted optimization (and indeed the basic motivation for
         --  the Valid attribute is exactly that this test does not work!)
         --  What will work is:

         --     Btyp!(X) >= Btyp!(type(X)'First)
         --       and then
         --     Btyp!(X) <= Btyp!(type(X)'Last)

         --  where Btyp is an integer type large enough to cover the full
         --  range of possible stored values (i.e. it is chosen on the basis
         --  of the size of the type, not the range of the values). We write
         --  this as two tests, rather than a range check, so that static
         --  evaluation will easily remove either or both of the checks if
         --  they can be -statically determined to be true (this happens
         --  when the type of X is static and the range extends to the full
         --  range of stored values).

         --  Unsigned types. Note: it is safe to consider only whether the
         --  subtype is unsigned, since we will in that case be doing all
         --  unsigned comparisons based on the subtype range. Since we use the
         --  actual subtype object size, this is appropriate.

         --  For example, if we have

         --    subtype x is integer range 1 .. 200;
         --    for x'Object_Size use 8;

         --  Now the base type is signed, but objects of this type are bits
         --  unsigned, and doing an unsigned test of the range 1 to 200 is
         --  correct, even though a value greater than 127 looks signed to a
         --  signed comparison.

         elsif Is_Unsigned_Type (Ptyp) then
            if Esize (Ptyp) <= 32 then
               Btyp := RTE (RE_Unsigned_32);
            else
               Btyp := RTE (RE_Unsigned_64);
            end if;

            Rewrite (N, Make_Range_Test);

         --  Signed types

         else
            if Esize (Ptyp) <= Esize (Standard_Integer) then
               Btyp := Standard_Integer;
            else
               Btyp := Universal_Integer;
            end if;

            Rewrite (N, Make_Range_Test);
         end if;

         --  If a predicate is present, then we do the predicate test, even if
         --  within the predicate function (infinite recursion is warned about
         --  in Sem_Attr in that case).

         declare
            Pred_Func : constant Entity_Id := Predicate_Function (Ptyp);

         begin
            if Present (Pred_Func) then
               Rewrite (N,
                 Make_And_Then (Loc,
                   Left_Opnd  => Relocate_Node (N),
                   Right_Opnd => Make_Predicate_Call (Ptyp, Pref)));
            end if;
         end;

         Analyze_And_Resolve (N, Standard_Boolean);
         Validity_Checks_On := Save_Validity_Checks_On;
      end Valid;

      -------------------
      -- Valid_Scalars --
      -------------------

      when Attribute_Valid_Scalars => Valid_Scalars : declare
         Ftyp : Entity_Id;

      begin
         if Present (Underlying_Type (Ptyp)) then
            Ftyp := Underlying_Type (Ptyp);
         else
            Ftyp := Ptyp;
         end if;

         --  For scalar types, Valid_Scalars is the same as Valid

         if Is_Scalar_Type (Ftyp) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Valid,
                Prefix         => Pref));
            Analyze_And_Resolve (N, Standard_Boolean);

         --  For array types, we construct a function that determines if there
         --  are any non-valid scalar subcomponents, and call the function.
         --  We only do this for arrays whose component type needs checking

         elsif Is_Array_Type (Ftyp)
           and then not No_Scalar_Parts (Component_Type (Ftyp))
         then
            Rewrite (N,
              Make_Function_Call (Loc,
                Name                   =>
                  New_Occurrence_Of (Build_Array_VS_Func (Ftyp, N), Loc),
                Parameter_Associations => New_List (Pref)));

            Analyze_And_Resolve (N, Standard_Boolean);

         --  For record types, we build a big if expression, applying Valid or
         --  Valid_Scalars as appropriate to all relevant components.

         elsif (Is_Record_Type (Ptyp) or else Has_Discriminants (Ptyp))
           and then not No_Scalar_Parts (Ptyp)
         then
            declare
               C : Entity_Id;
               X : Node_Id;
               A : Name_Id;

            begin
               X := New_Occurrence_Of (Standard_True, Loc);
               C := First_Component_Or_Discriminant (Ptyp);
               while Present (C) loop
                  if No_Scalar_Parts (Etype (C)) then
                     goto Continue;
                  elsif Is_Scalar_Type (Etype (C)) then
                     A := Name_Valid;
                  else
                     A := Name_Valid_Scalars;
                  end if;

                  X :=
                    Make_And_Then (Loc,
                      Left_Opnd   => X,
                      Right_Opnd  =>
                        Make_Attribute_Reference (Loc,
                          Attribute_Name => A,
                          Prefix         =>
                            Make_Selected_Component (Loc,
                              Prefix        =>
                                Duplicate_Subexpr (Pref, Name_Req => True),
                              Selector_Name =>
                                New_Occurrence_Of (C, Loc))));
               <<Continue>>
                  Next_Component_Or_Discriminant (C);
               end loop;

               Rewrite (N, X);
               Analyze_And_Resolve (N, Standard_Boolean);
            end;

         --  For all other types, result is True (but not static)

         else
            Rewrite (N, New_Occurrence_Of (Standard_Boolean, Loc));
            Analyze_And_Resolve (N, Standard_Boolean);
            Set_Is_Static_Expression (N, False);
         end if;
      end Valid_Scalars;

      -----------
      -- Value --
      -----------

      --  Value attribute is handled in separate unit Exp_Imgv

      when Attribute_Value =>
         Exp_Imgv.Expand_Value_Attribute (N);

      -----------------
      -- Value_Size --
      -----------------

      --  The processing for Value_Size shares the processing for Size

      -------------
      -- Version --
      -------------

      --  The processing for Version shares the processing for Body_Version

      ----------------
      -- Wide_Image --
      ----------------

      --  Wide_Image attribute is handled in separate unit Exp_Imgv

      when Attribute_Wide_Image =>
         Exp_Imgv.Expand_Wide_Image_Attribute (N);

      ---------------------
      -- Wide_Wide_Image --
      ---------------------

      --  Wide_Wide_Image attribute is handled in separate unit Exp_Imgv

      when Attribute_Wide_Wide_Image =>
         Exp_Imgv.Expand_Wide_Wide_Image_Attribute (N);

      ----------------
      -- Wide_Value --
      ----------------

      --  We expand typ'Wide_Value (X) into

      --    typ'Value
      --      (Wide_String_To_String (X, Wide_Character_Encoding_Method))

      --  Wide_String_To_String is a runtime function that converts its wide
      --  string argument to String, converting any non-translatable characters
      --  into appropriate escape sequences. This preserves the required
      --  semantics of Wide_Value in all cases, and results in a very simple
      --  implementation approach.

      --  Note: for this approach to be fully standard compliant for the cases
      --  where typ is Wide_Character and Wide_Wide_Character, the encoding
      --  method must cover the entire character range (e.g. UTF-8). But that
      --  is a reasonable requirement when dealing with encoded character
      --  sequences. Presumably if one of the restrictive encoding mechanisms
      --  is in use such as Shift-JIS, then characters that cannot be
      --  represented using this encoding will not appear in any case.

      when Attribute_Wide_Value => Wide_Value :
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => Pref,
             Attribute_Name => Name_Value,

             Expressions    => New_List (
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Wide_String_To_String), Loc),

                 Parameter_Associations => New_List (
                   Relocate_Node (First (Exprs)),
                   Make_Integer_Literal (Loc,
                     Intval => Int (Wide_Character_Encoding_Method)))))));

         Analyze_And_Resolve (N, Typ);
      end Wide_Value;

      ---------------------
      -- Wide_Wide_Value --
      ---------------------

      --  We expand typ'Wide_Value_Value (X) into

      --    typ'Value
      --      (Wide_Wide_String_To_String (X, Wide_Character_Encoding_Method))

      --  Wide_Wide_String_To_String is a runtime function that converts its
      --  wide string argument to String, converting any non-translatable
      --  characters into appropriate escape sequences. This preserves the
      --  required semantics of Wide_Wide_Value in all cases, and results in a
      --  very simple implementation approach.

      --  It's not quite right where typ = Wide_Wide_Character, because the
      --  encoding method may not cover the whole character type ???

      when Attribute_Wide_Wide_Value => Wide_Wide_Value :
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => Pref,
             Attribute_Name => Name_Value,

             Expressions    => New_List (
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Wide_Wide_String_To_String), Loc),

                 Parameter_Associations => New_List (
                   Relocate_Node (First (Exprs)),
                   Make_Integer_Literal (Loc,
                     Intval => Int (Wide_Character_Encoding_Method)))))));

         Analyze_And_Resolve (N, Typ);
      end Wide_Wide_Value;

      ---------------------
      -- Wide_Wide_Width --
      ---------------------

      --  Wide_Wide_Width attribute is handled in separate unit Exp_Imgv

      when Attribute_Wide_Wide_Width =>
         Exp_Imgv.Expand_Width_Attribute (N, Wide_Wide);

      ----------------
      -- Wide_Width --
      ----------------

      --  Wide_Width attribute is handled in separate unit Exp_Imgv

      when Attribute_Wide_Width =>
         Exp_Imgv.Expand_Width_Attribute (N, Wide);

      -----------
      -- Width --
      -----------

      --  Width attribute is handled in separate unit Exp_Imgv

      when Attribute_Width =>
         Exp_Imgv.Expand_Width_Attribute (N, Normal);

      -----------
      -- Write --
      -----------

      when Attribute_Write => Write : declare
         P_Type : constant Entity_Id := Entity (Pref);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Pname  : Entity_Id;
         Decl   : Node_Id;
         Prag   : Node_Id;
         Arg3   : Node_Id;
         Wfunc  : Node_Id;

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  The simple case, if there is a TSS for Write, just call it

         Pname := Find_Stream_Subprogram (P_Type, TSS_Stream_Write);

         if Present (Pname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Output (stream, Item)

            --  as

            --     strmtyp'Output (Stream, strmwrite (acttyp (Item)));

            --  where strmwrite is the given Write function that converts an
            --  argument of type sourcetyp or a type acctyp, from which it is
            --  derived to type strmtyp. The conversion to acttyp is required
            --  for the derived case.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg3 :=
                 Next (Next (First (Pragma_Argument_Associations (Prag))));
               Wfunc := Entity (Expression (Arg3));

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Etype (Wfunc), Loc),
                   Attribute_Name => Name_Output,
                   Expressions => New_List (
                     Relocate_Node (First (Exprs)),
                     Make_Function_Call (Loc,
                       Name => New_Occurrence_Of (Wfunc, Loc),
                       Parameter_Associations => New_List (
                         OK_Convert_To (Etype (First_Formal (Wfunc)),
                           Relocate_Node (Next (First (Exprs)))))))));

               Analyze (N);
               return;

            --  For elementary types, we call the W_xxx routine directly

            elsif Is_Elementary_Type (U_Type) then
               Rewrite (N, Build_Elementary_Write_Call (N));
               Analyze (N);
               return;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Write_Procedure (N, U_Type, Decl, Pname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Tagged type case, use the primitive Write function. Note that
            --  this will dispatch in the class-wide case which is what we want

            elsif Is_Tagged_Type (U_Type) then
               Pname := Find_Prim_Op (U_Type, TSS_Stream_Write);

            --  All other record type cases, including protected records.
            --  The latter only arise for expander generated code for
            --  handling shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Write attribute of an
               --  Unchecked_Union type. However, if the 'Write reference is
               --  within the generated Output stream procedure, Write outputs
               --  the components, and the default values of the discriminant
               --  are streamed by the Output procedure itself.

               if Is_Unchecked_Union (Base_Type (U_Type))
                 and not Is_TSS (Current_Scope, TSS_Stream_Output)
               then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));
               end if;

               if Has_Discriminants (U_Type)
                 and then Present
                   (Discriminant_Default_Value (First_Discriminant (U_Type)))
               then
                  Build_Mutable_Record_Write_Procedure
                    (Loc, Full_Base (U_Type), Decl, Pname);
               else
                  Build_Record_Write_Procedure
                    (Loc, Full_Base (U_Type), Decl, Pname);
               end if;

               Insert_Action (N, Decl);
            end if;
         end if;

         --  If we fall through, Pname is the procedure to be called

         Rewrite_Stream_Proc_Call (Pname);
      end Write;

      --  Component_Size is handled by the back end, unless the component size
      --  is known at compile time, which is always true in the packed array
      --  case. It is important that the packed array case is handled in the
      --  front end (see Eval_Attribute) since the back end would otherwise get
      --  confused by the equivalent packed array type.

      when Attribute_Component_Size =>
         null;

      --  The following attributes are handled by the back end (except that
      --  static cases have already been evaluated during semantic processing,
      --  but in any case the back end should not count on this). The one bit
      --  of special processing required is that these attributes typically
      --  generate conditionals in the code, so we need to check the relevant
      --  restriction.

      when Attribute_Max                          |
           Attribute_Min                          =>
         Check_Restriction (No_Implicit_Conditionals, N);

      --  The following attributes are handled by the back end (except that
      --  static cases have already been evaluated during semantic processing,
      --  but in any case the back end should not count on this).

      --  The back end also handles the non-class-wide cases of Size

      when Attribute_Bit_Order                    |
           Attribute_Code_Address                 |
           Attribute_Definite                     |
           Attribute_Null_Parameter               |
           Attribute_Passed_By_Reference          |
           Attribute_Pool_Address                 |
           Attribute_Scalar_Storage_Order         =>
         null;

      --  The following attributes are also handled by the back end, but return
      --  a universal integer result, so may need a conversion for checking
      --  that the result is in range.

      when Attribute_Aft                          |
           Attribute_Max_Alignment_For_Allocation =>
         Apply_Universal_Integer_Attribute_Checks (N);

      --  The following attributes should not appear at this stage, since they
      --  have already been handled by the analyzer (and properly rewritten
      --  with corresponding values or entities to represent the right values)

      when Attribute_Abort_Signal                 |
           Attribute_Address_Size                 |
           Attribute_Atomic_Always_Lock_Free      |
           Attribute_Base                         |
           Attribute_Class                        |
           Attribute_Compiler_Version             |
           Attribute_Default_Bit_Order            |
           Attribute_Delta                        |
           Attribute_Denorm                       |
           Attribute_Digits                       |
           Attribute_Emax                         |
           Attribute_Enabled                      |
           Attribute_Epsilon                      |
           Attribute_Fast_Math                    |
           Attribute_First_Valid                  |
           Attribute_Has_Access_Values            |
           Attribute_Has_Discriminants            |
           Attribute_Has_Tagged_Values            |
           Attribute_Large                        |
           Attribute_Last_Valid                   |
           Attribute_Lock_Free                    |
           Attribute_Machine_Emax                 |
           Attribute_Machine_Emin                 |
           Attribute_Machine_Mantissa             |
           Attribute_Machine_Overflows            |
           Attribute_Machine_Radix                |
           Attribute_Machine_Rounds               |
           Attribute_Maximum_Alignment            |
           Attribute_Model_Emin                   |
           Attribute_Model_Epsilon                |
           Attribute_Model_Mantissa               |
           Attribute_Model_Small                  |
           Attribute_Modulus                      |
           Attribute_Partition_ID                 |
           Attribute_Range                        |
           Attribute_Restriction_Set              |
           Attribute_Safe_Emax                    |
           Attribute_Safe_First                   |
           Attribute_Safe_Large                   |
           Attribute_Safe_Last                    |
           Attribute_Safe_Small                   |
           Attribute_Scale                        |
           Attribute_Signed_Zeros                 |
           Attribute_Small                        |
           Attribute_Storage_Unit                 |
           Attribute_Stub_Type                    |
           Attribute_System_Allocator_Alignment   |
           Attribute_Target_Name                  |
           Attribute_Type_Class                   |
           Attribute_Type_Key                     |
           Attribute_Unconstrained_Array          |
           Attribute_Universal_Literal_String     |
           Attribute_Wchar_T_Size                 |
           Attribute_Word_Size                    =>
         raise Program_Error;

      --  The Asm_Input and Asm_Output attributes are not expanded at this
      --  stage, but will be eliminated in the expansion of the Asm call, see
      --  Exp_Intr for details. So the back end will never see these either.

      when Attribute_Asm_Input                    |
           Attribute_Asm_Output                   =>
         null;
      end case;

   --  Note: as mentioned earlier, individual sections of the above case
   --  statement assume there is no code after the case statement, and are
   --  legitimately allowed to execute return statements if they have nothing
   --  more to do, so DO NOT add code at this point.

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Attribute_Reference;

   ----------------------
   -- Expand_Pred_Succ --
   ----------------------

   --  For typ'Pred (exp), we generate the check

   --    [constraint_error when exp = typ'Base'First]

   --  Similarly, for typ'Succ (exp), we generate the check

   --    [constraint_error when exp = typ'Base'Last]

   --  These checks are not generated for modular types, since the proper
   --  semantics for Succ and Pred on modular types is to wrap, not raise CE.
   --  We also suppress these checks if we are the right side of an assignment
   --  statement or the expression of an object declaration, where the flag
   --  Suppress_Assignment_Checks is set for the assignment/declaration.

   procedure Expand_Pred_Succ (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      P    : constant Node_Id    := Parent (N);
      Cnam : Name_Id;

   begin
      if Attribute_Name (N) = Name_Pred then
         Cnam := Name_First;
      else
         Cnam := Name_Last;
      end if;

      if not Nkind_In (P, N_Assignment_Statement, N_Object_Declaration)
        or else not Suppress_Assignment_Checks (P)
      then
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd =>
                   Duplicate_Subexpr_Move_Checks (First (Expressions (N))),
                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix =>
                       New_Reference_To (Base_Type (Etype (Prefix (N))), Loc),
                     Attribute_Name => Cnam)),
             Reason => CE_Overflow_Check_Failed));
      end if;
   end Expand_Pred_Succ;

   -----------------------------
   -- Expand_Update_Attribute --
   -----------------------------

   procedure Expand_Update_Attribute (N : Node_Id) is
      procedure Process_Component_Or_Element_Update
        (Temp : Entity_Id;
         Comp : Node_Id;
         Expr : Node_Id;
         Typ  : Entity_Id);
      --  Generate the statements necessary to update a single component or an
      --  element of the prefix. The code is inserted before the attribute N.
      --  Temp denotes the entity of the anonymous object created to reflect
      --  the changes in values. Comp is the component/index expression to be
      --  updated. Expr is an expression yielding the new value of Comp. Typ
      --  is the type of the prefix of attribute Update.

      procedure Process_Range_Update
        (Temp : Entity_Id;
         Comp : Node_Id;
         Expr : Node_Id);
      --  Generate the statements necessary to update a slice of the prefix.
      --  The code is inserted before the attribute N. Temp denotes the entity
      --  of the anonymous object created to reflect the changes in values.
      --  Comp is range of the slice to be updated. Expr is an expression
      --  yielding the new value of Comp.

      -----------------------------------------
      -- Process_Component_Or_Element_Update --
      -----------------------------------------

      procedure Process_Component_Or_Element_Update
        (Temp : Entity_Id;
         Comp : Node_Id;
         Expr : Node_Id;
         Typ  : Entity_Id)
      is
         Loc   : constant Source_Ptr := Sloc (Comp);
         Exprs : List_Id;
         LHS   : Node_Id;

      begin
         --  An array element may be modified by the following relations
         --  depending on the number of dimensions:

         --     1 => Expr           --  one dimensional update
         --    (1, ..., N) => Expr  --  multi dimensional update

         --  The above forms are converted in assignment statements where the
         --  left hand side is an indexed component:

         --    Temp (1) := Expr;          --  one dimensional update
         --    Temp (1, ..., N) := Expr;  --  multi dimensional update

         if Is_Array_Type (Typ) then

            --  The index expressions of a multi dimensional array update
            --  appear as an aggregate.

            if Nkind (Comp) = N_Aggregate then
               Exprs := New_Copy_List_Tree (Expressions (Comp));
            else
               Exprs := New_List (Relocate_Node (Comp));
            end if;

            LHS :=
              Make_Indexed_Component (Loc,
                Prefix      => New_Reference_To (Temp, Loc),
                Expressions => Exprs);

         --  A record component update appears in the following form:

         --    Comp => Expr

         --  The above relation is transformed into an assignment statement
         --  where the left hand side is a selected component:

         --    Temp.Comp := Expr;

         else pragma Assert (Is_Record_Type (Typ));
            LHS :=
              Make_Selected_Component (Loc,
                Prefix        => New_Reference_To (Temp, Loc),
                Selector_Name => Relocate_Node (Comp));
         end if;

         Insert_Action (N,
           Make_Assignment_Statement (Loc,
             Name       => LHS,
             Expression => Relocate_Node (Expr)));
      end Process_Component_Or_Element_Update;

      --------------------------
      -- Process_Range_Update --
      --------------------------

      procedure Process_Range_Update
        (Temp : Entity_Id;
         Comp : Node_Id;
         Expr : Node_Id)
      is
         Loc   : constant Source_Ptr := Sloc (Comp);
         Index : Entity_Id;

      begin
         --  A range update appears as

         --    (Low .. High => Expr)

         --  The above construct is transformed into a loop that iterates over
         --  the given range and modifies the corresponding array values to the
         --  value of Expr:

         --    for Index in Low .. High loop
         --       Temp (Index) := Expr;
         --    end loop;

         Index := Make_Temporary (Loc, 'I');

         Insert_Action (N,
           Make_Loop_Statement (Loc,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Loc,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Loc,
                     Defining_Identifier         => Index,
                     Discrete_Subtype_Definition => Relocate_Node (Comp))),

             Statements       => New_List (
               Make_Assignment_Statement (Loc,
                 Name       =>
                   Make_Indexed_Component (Loc,
                     Prefix      => New_Reference_To (Temp, Loc),
                     Expressions => New_List (New_Reference_To (Index, Loc))),
                 Expression => Relocate_Node (Expr))),

             End_Label        => Empty));
      end Process_Range_Update;

      --  Local variables

      Aggr  : constant Node_Id := First (Expressions (N));
      Loc   : constant Source_Ptr := Sloc (N);
      Pref  : constant Node_Id := Prefix (N);
      Typ   : constant Entity_Id := Etype (Pref);
      Assoc : Node_Id;
      Comp  : Node_Id;
      Expr  : Node_Id;
      Temp  : Entity_Id;

   --  Start of processing for Expand_Update_Attribute

   begin
      --  Create the anonymous object that stores the value of the prefix and
      --  reflects subsequent changes in value. Generate:

      --    Temp : <type of Pref> := Pref;

      Temp := Make_Temporary (Loc, 'T');

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Temp,
          Object_Definition   => New_Reference_To (Typ, Loc),
          Expression          => Relocate_Node (Pref)));

      --  Process the update aggregate

      Assoc := First (Component_Associations (Aggr));
      while Present (Assoc) loop
         Comp := First (Choices (Assoc));
         Expr := Expression (Assoc);
         while Present (Comp) loop
            if Nkind (Comp) = N_Range then
               Process_Range_Update (Temp, Comp, Expr);
            else
               Process_Component_Or_Element_Update (Temp, Comp, Expr, Typ);
            end if;

            Next (Comp);
         end loop;

         Next (Assoc);
      end loop;

      --  The attribute is replaced by a reference to the anonymous object

      Rewrite (N, New_Reference_To (Temp, Loc));
      Analyze (N);
   end Expand_Update_Attribute;

   -------------------
   -- Find_Fat_Info --
   -------------------

   procedure Find_Fat_Info
     (T        : Entity_Id;
      Fat_Type : out Entity_Id;
      Fat_Pkg  : out RE_Id)
   is
      Btyp : constant Entity_Id := Base_Type (T);
      Rtyp : constant Entity_Id := Root_Type (T);
      Digs : constant Nat       := UI_To_Int (Digits_Value (Btyp));

   begin
      --  If the base type is VAX float, then get appropriate VAX float type

      if Vax_Float (Btyp) then
         case Digs is
            when 6 =>
               Fat_Type := RTE (RE_Fat_VAX_F);
               Fat_Pkg  := RE_Attr_VAX_F_Float;

            when 9 =>
               Fat_Type := RTE (RE_Fat_VAX_D);
               Fat_Pkg  := RE_Attr_VAX_D_Float;

            when 15 =>
               Fat_Type := RTE (RE_Fat_VAX_G);
               Fat_Pkg  := RE_Attr_VAX_G_Float;

            when others =>
               raise Program_Error;
         end case;

      --  If root type is VAX float, this is the case where the library has
      --  been recompiled in VAX float mode, and we have an IEEE float type.
      --  This is when we use the special IEEE Fat packages.

      elsif Vax_Float (Rtyp) then
         case Digs is
            when 6 =>
               Fat_Type := RTE (RE_Fat_IEEE_Short);
               Fat_Pkg  := RE_Attr_IEEE_Short;

            when 15 =>
               Fat_Type := RTE (RE_Fat_IEEE_Long);
               Fat_Pkg  := RE_Attr_IEEE_Long;

            when others =>
               raise Program_Error;
         end case;

      --  If neither the base type nor the root type is VAX_Native then VAX
      --  float is out of the picture, and we can just use the root type.

      else
         Fat_Type := Rtyp;

         if Fat_Type = Standard_Short_Float then
            Fat_Pkg := RE_Attr_Short_Float;

         elsif Fat_Type = Standard_Float then
            Fat_Pkg := RE_Attr_Float;

         elsif Fat_Type = Standard_Long_Float then
            Fat_Pkg := RE_Attr_Long_Float;

         elsif Fat_Type = Standard_Long_Long_Float then
            Fat_Pkg := RE_Attr_Long_Long_Float;

         --  Universal real (which is its own root type) is treated as being
         --  equivalent to Standard.Long_Long_Float, since it is defined to
         --  have the same precision as the longest Float type.

         elsif Fat_Type = Universal_Real then
            Fat_Type := Standard_Long_Long_Float;
            Fat_Pkg := RE_Attr_Long_Long_Float;

         else
            raise Program_Error;
         end if;
      end if;
   end Find_Fat_Info;

   ----------------------------
   -- Find_Stream_Subprogram --
   ----------------------------

   function Find_Stream_Subprogram
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id
   is
      Base_Typ : constant Entity_Id := Base_Type (Typ);
      Ent      : constant Entity_Id := TSS (Typ, Nam);

      function Is_Available (Entity : RE_Id) return Boolean;
      pragma Inline (Is_Available);
      --  Function to check whether the specified run-time call is available
      --  in the run time used. In the case of a configurable run time, it
      --  is normal that some subprograms are not there.

      --  I don't understand this routine at all, why is this not just a
      --  call to RTE_Available? And if for some reason we need a different
      --  routine with different semantics, why is not in Rtsfind ???

      ------------------
      -- Is_Available --
      ------------------

      function Is_Available (Entity : RE_Id) return Boolean is
      begin
         --  Assume that the unit will always be available when using a
         --  "normal" (not configurable) run time.

         return not Configurable_Run_Time_Mode
           or else RTE_Available (Entity);
      end Is_Available;

   --  Start of processing for Find_Stream_Subprogram

   begin
      if Present (Ent) then
         return Ent;
      end if;

      --  Stream attributes for strings are expanded into library calls. The
      --  following checks are disabled when the run-time is not available or
      --  when compiling predefined types due to bootstrap issues. As a result,
      --  the compiler will generate in-place stream routines for string types
      --  that appear in GNAT's library, but will generate calls via rtsfind
      --  to library routines for user code.

      --  ??? For now, disable this code for JVM, since this generates a
      --  VerifyError exception at run time on e.g. c330001.

      --  This is disabled for AAMP, to avoid creating dependences on files not
      --  supported in the AAMP library (such as s-fileio.adb).

      --  Note: In the case of using a configurable run time, it is very likely
      --  that stream routines for string types are not present (they require
      --  file system support). In this case, the specific stream routines for
      --  strings are not used, relying on the regular stream mechanism
      --  instead. That is why we include the test Is_Available when dealing
      --  with these cases.

      if VM_Target /= JVM_Target
        and then not AAMP_On_Target
        and then
          not Is_Predefined_File_Name (Unit_File_Name (Current_Sem_Unit))
      then
         --  String as defined in package Ada

         if Base_Typ = Standard_String then
            if Restriction_Active (No_Stream_Optimizations) then
               if Nam = TSS_Stream_Input
                 and then Is_Available (RE_String_Input)
               then
                  return RTE (RE_String_Input);

               elsif Nam = TSS_Stream_Output
                 and then Is_Available (RE_String_Output)
               then
                  return RTE (RE_String_Output);

               elsif Nam = TSS_Stream_Read
                 and then Is_Available (RE_String_Read)
               then
                  return RTE (RE_String_Read);

               elsif Nam = TSS_Stream_Write
                 and then Is_Available (RE_String_Write)
               then
                  return RTE (RE_String_Write);

               elsif Nam /= TSS_Stream_Input and then
                     Nam /= TSS_Stream_Output and then
                     Nam /= TSS_Stream_Read and then
                     Nam /= TSS_Stream_Write
               then
                  raise Program_Error;
               end if;

            else
               if Nam = TSS_Stream_Input
                 and then Is_Available (RE_String_Input_Blk_IO)
               then
                  return RTE (RE_String_Input_Blk_IO);

               elsif Nam = TSS_Stream_Output
                 and then Is_Available (RE_String_Output_Blk_IO)
               then
                  return RTE (RE_String_Output_Blk_IO);

               elsif Nam = TSS_Stream_Read
                 and then Is_Available (RE_String_Read_Blk_IO)
               then
                  return RTE (RE_String_Read_Blk_IO);

               elsif Nam = TSS_Stream_Write
                 and then Is_Available (RE_String_Write_Blk_IO)
               then
                  return RTE (RE_String_Write_Blk_IO);

               elsif Nam /= TSS_Stream_Input and then
                     Nam /= TSS_Stream_Output and then
                     Nam /= TSS_Stream_Read and then
                     Nam /= TSS_Stream_Write
               then
                  raise Program_Error;
               end if;
            end if;

         --  Wide_String as defined in package Ada

         elsif Base_Typ = Standard_Wide_String then
            if Restriction_Active (No_Stream_Optimizations) then
               if Nam = TSS_Stream_Input
                 and then Is_Available (RE_Wide_String_Input)
               then
                  return RTE (RE_Wide_String_Input);

               elsif Nam = TSS_Stream_Output
                 and then Is_Available (RE_Wide_String_Output)
               then
                  return RTE (RE_Wide_String_Output);

               elsif Nam = TSS_Stream_Read
                 and then Is_Available (RE_Wide_String_Read)
               then
                  return RTE (RE_Wide_String_Read);

               elsif Nam = TSS_Stream_Write
                 and then Is_Available (RE_Wide_String_Write)
               then
                  return RTE (RE_Wide_String_Write);

               elsif Nam /= TSS_Stream_Input and then
                     Nam /= TSS_Stream_Output and then
                     Nam /= TSS_Stream_Read and then
                     Nam /= TSS_Stream_Write
               then
                  raise Program_Error;
               end if;

            else
               if Nam = TSS_Stream_Input
                 and then Is_Available (RE_Wide_String_Input_Blk_IO)
               then
                  return RTE (RE_Wide_String_Input_Blk_IO);

               elsif Nam = TSS_Stream_Output
                 and then Is_Available (RE_Wide_String_Output_Blk_IO)
               then
                  return RTE (RE_Wide_String_Output_Blk_IO);

               elsif Nam = TSS_Stream_Read
                 and then Is_Available (RE_Wide_String_Read_Blk_IO)
               then
                  return RTE (RE_Wide_String_Read_Blk_IO);

               elsif Nam = TSS_Stream_Write
                 and then Is_Available (RE_Wide_String_Write_Blk_IO)
               then
                  return RTE (RE_Wide_String_Write_Blk_IO);

               elsif Nam /= TSS_Stream_Input and then
                     Nam /= TSS_Stream_Output and then
                     Nam /= TSS_Stream_Read and then
                     Nam /= TSS_Stream_Write
               then
                  raise Program_Error;
               end if;
            end if;

         --  Wide_Wide_String as defined in package Ada

         elsif Base_Typ = Standard_Wide_Wide_String then
            if Restriction_Active (No_Stream_Optimizations) then
               if Nam = TSS_Stream_Input
                 and then Is_Available (RE_Wide_Wide_String_Input)
               then
                  return RTE (RE_Wide_Wide_String_Input);

               elsif Nam = TSS_Stream_Output
                 and then Is_Available (RE_Wide_Wide_String_Output)
               then
                  return RTE (RE_Wide_Wide_String_Output);

               elsif Nam = TSS_Stream_Read
                 and then Is_Available (RE_Wide_Wide_String_Read)
               then
                  return RTE (RE_Wide_Wide_String_Read);

               elsif Nam = TSS_Stream_Write
                 and then Is_Available (RE_Wide_Wide_String_Write)
               then
                  return RTE (RE_Wide_Wide_String_Write);

               elsif Nam /= TSS_Stream_Input and then
                     Nam /= TSS_Stream_Output and then
                     Nam /= TSS_Stream_Read and then
                     Nam /= TSS_Stream_Write
               then
                  raise Program_Error;
               end if;

            else
               if Nam = TSS_Stream_Input
                 and then Is_Available (RE_Wide_Wide_String_Input_Blk_IO)
               then
                  return RTE (RE_Wide_Wide_String_Input_Blk_IO);

               elsif Nam = TSS_Stream_Output
                 and then Is_Available (RE_Wide_Wide_String_Output_Blk_IO)
               then
                  return RTE (RE_Wide_Wide_String_Output_Blk_IO);

               elsif Nam = TSS_Stream_Read
                 and then Is_Available (RE_Wide_Wide_String_Read_Blk_IO)
               then
                  return RTE (RE_Wide_Wide_String_Read_Blk_IO);

               elsif Nam = TSS_Stream_Write
                 and then Is_Available (RE_Wide_Wide_String_Write_Blk_IO)
               then
                  return RTE (RE_Wide_Wide_String_Write_Blk_IO);

               elsif Nam /= TSS_Stream_Input and then
                     Nam /= TSS_Stream_Output and then
                     Nam /= TSS_Stream_Read and then
                     Nam /= TSS_Stream_Write
               then
                  raise Program_Error;
               end if;
            end if;
         end if;
      end if;

      if Is_Tagged_Type (Typ)
        and then Is_Derived_Type (Typ)
      then
         return Find_Prim_Op (Typ, Nam);
      else
         return Find_Inherited_TSS (Typ, Nam);
      end if;
   end Find_Stream_Subprogram;

   ---------------
   -- Full_Base --
   ---------------

   function Full_Base (T : Entity_Id) return Entity_Id is
      BT : Entity_Id;

   begin
      BT := Base_Type (T);

      if Is_Private_Type (BT)
        and then Present (Full_View (BT))
      then
         BT := Full_View (BT);
      end if;

      return BT;
   end Full_Base;

   -----------------------
   -- Get_Index_Subtype --
   -----------------------

   function Get_Index_Subtype (N : Node_Id) return Node_Id is
      P_Type : Entity_Id := Etype (Prefix (N));
      Indx   : Node_Id;
      J      : Int;

   begin
      if Is_Access_Type (P_Type) then
         P_Type := Designated_Type (P_Type);
      end if;

      if No (Expressions (N)) then
         J := 1;
      else
         J := UI_To_Int (Expr_Value (First (Expressions (N))));
      end if;

      Indx := First_Index (P_Type);
      while J > 1 loop
         Next_Index (Indx);
         J := J - 1;
      end loop;

      return Etype (Indx);
   end Get_Index_Subtype;

   -------------------------------
   -- Get_Stream_Convert_Pragma --
   -------------------------------

   function Get_Stream_Convert_Pragma (T : Entity_Id) return Node_Id is
      Typ : Entity_Id;
      N   : Node_Id;

   begin
      --  Note: we cannot use Get_Rep_Pragma here because of the peculiarity
      --  that a stream convert pragma for a tagged type is not inherited from
      --  its parent. Probably what is wrong here is that it is basically
      --  incorrect to consider a stream convert pragma to be a representation
      --  pragma at all ???

      N := First_Rep_Item (Implementation_Base_Type (T));
      while Present (N) loop
         if Nkind (N) = N_Pragma
           and then Pragma_Name (N) = Name_Stream_Convert
         then
            --  For tagged types this pragma is not inherited, so we
            --  must verify that it is defined for the given type and
            --  not an ancestor.

            Typ :=
              Entity (Expression (First (Pragma_Argument_Associations (N))));

            if not Is_Tagged_Type (T)
              or else T = Typ
              or else (Is_Private_Type (Typ) and then T = Full_View (Typ))
            then
               return N;
            end if;
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Stream_Convert_Pragma;

   ---------------------------------
   -- Is_Constrained_Packed_Array --
   ---------------------------------

   function Is_Constrained_Packed_Array (Typ : Entity_Id) return Boolean is
      Arr : Entity_Id := Typ;

   begin
      if Is_Access_Type (Arr) then
         Arr := Designated_Type (Arr);
      end if;

      return Is_Array_Type (Arr)
        and then Is_Constrained (Arr)
        and then Present (Packed_Array_Type (Arr));
   end Is_Constrained_Packed_Array;

   ----------------------------------------
   -- Is_Inline_Floating_Point_Attribute --
   ----------------------------------------

   function Is_Inline_Floating_Point_Attribute (N : Node_Id) return Boolean is
      Id : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));

   begin
      if Nkind (Parent (N)) /= N_Type_Conversion
        or else not Is_Integer_Type (Etype (Parent (N)))
      then
         return False;
      end if;

      --  Should also support 'Machine_Rounding and 'Unbiased_Rounding, but
      --  required back end support has not been implemented yet ???

      return Id = Attribute_Truncation;
   end Is_Inline_Floating_Point_Attribute;

end Exp_Attr;
