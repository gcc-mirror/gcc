------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Elists;   use Elists;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Disp; use Exp_Disp;
with Exp_Dist; use Exp_Dist;
with Exp_Intr; use Exp_Intr;
with Exp_Pakd; use Exp_Pakd;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Hostparm; use Hostparm;
with Inline;   use Inline;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Validsw;  use Validsw;

package body Exp_Ch6 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Overriding_Operation (Subp : Entity_Id);
   --  Subp is a dispatching operation. Check whether it may override an
   --  inherited private operation, in which case its DT entry is that of
   --  the hidden operation, not the one it may have received earlier.
   --  This must be done before emitting the code to set the corresponding
   --  DT to the address of the subprogram. The actual placement of Subp in
   --  the proper place in the list of primitive operations is done in
   --  Declare_Inherited_Private_Subprograms, which also has to deal with
   --  implicit operations. This duplication is unavoidable for now???

   procedure Detect_Infinite_Recursion (N : Node_Id; Spec : Entity_Id);
   --  This procedure is called only if the subprogram body N, whose spec
   --  has the given entity Spec, contains a parameterless recursive call.
   --  It attempts to generate runtime code to detect if this a case of
   --  infinite recursion.
   --
   --  The body is scanned to determine dependencies. If the only external
   --  dependencies are on a small set of scalar variables, then the values
   --  of these variables are captured on entry to the subprogram, and if
   --  the values are not changed for the call, we know immediately that
   --  we have an infinite recursion.

   procedure Expand_Actuals (N : Node_Id; Subp : Entity_Id);
   --  For each actual of an in-out parameter which is a numeric conversion
   --  of the form T(A), where A denotes a variable, we insert the declaration:
   --
   --    Temp : T := T (A);
   --
   --  prior to the call. Then we replace the actual with a reference to Temp,
   --  and append the assignment:
   --
   --    A := TypeA (Temp);
   --
   --  after the call. Here TypeA is the actual type of variable A.
   --  For out parameters, the initial declaration has no expression.
   --  If A is not an entity name, we generate instead:
   --
   --    Var  : TypeA renames A;
   --    Temp : T := Var;       --  omitting expression for out parameter.
   --    ...
   --    Var := TypeA (Temp);
   --
   --  For other in-out parameters, we emit the required constraint checks
   --  before and/or after the call.
   --
   --  For all parameter modes, actuals that denote components and slices
   --  of packed arrays are expanded into suitable temporaries.

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id);
   --  If called subprogram can be inlined by the front-end, retrieve the
   --  analyzed body, replace formals with actuals and expand call in place.
   --  Generate thunks for actuals that are expressions, and insert the
   --  corresponding constant declarations before the call. If the original
   --  call is to a derived operation, the return type is the one of the
   --  derived operation, but the body is that of the original, so return
   --  expressions in the body must be converted to the desired type (which
   --  is simply not noted in the tree without inline expansion).

   function Expand_Protected_Object_Reference
     (N    : Node_Id;
      Scop : Entity_Id)
      return Node_Id;

   procedure Expand_Protected_Subprogram_Call
     (N    : Node_Id;
      Subp : Entity_Id;
      Scop : Entity_Id);
   --  A call to a protected subprogram within the protected object may appear
   --  as a regular call. The list of actuals must be expanded to contain a
   --  reference to the object itself, and the call becomes a call to the
   --  corresponding protected subprogram.

   --------------------------------
   -- Check_Overriding_Operation --
   --------------------------------

   procedure Check_Overriding_Operation (Subp : Entity_Id) is
      Typ     : constant Entity_Id := Find_Dispatching_Type (Subp);
      Op_List : constant Elist_Id  := Primitive_Operations (Typ);
      Op_Elmt : Elmt_Id;
      Prim_Op : Entity_Id;
      Par_Op  : Entity_Id;

   begin
      if Is_Derived_Type (Typ)
        and then not Is_Private_Type (Typ)
        and then In_Open_Scopes (Scope (Etype (Typ)))
        and then Typ = Base_Type (Typ)
      then
         --  Subp overrides an inherited private operation if there is
         --  an inherited operation with a different name than Subp (see
         --  Derive_Subprogram) whose Alias is a hidden  subprogram with
         --  the same name as Subp.

         Op_Elmt := First_Elmt (Op_List);
         while Present (Op_Elmt) loop
            Prim_Op := Node (Op_Elmt);
            Par_Op  := Alias (Prim_Op);

            if Present (Par_Op)
              and then not Comes_From_Source (Prim_Op)
              and then Chars (Prim_Op) /= Chars (Par_Op)
              and then Chars (Par_Op) = Chars (Subp)
              and then Is_Hidden (Par_Op)
              and then Type_Conformant (Prim_Op, Subp)
            then
               Set_DT_Position (Subp, DT_Position (Prim_Op));
            end if;

            Next_Elmt (Op_Elmt);
         end loop;
      end if;
   end Check_Overriding_Operation;

   -------------------------------
   -- Detect_Infinite_Recursion --
   -------------------------------

   procedure Detect_Infinite_Recursion (N : Node_Id; Spec : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Var_List : constant Elist_Id := New_Elmt_List;
      --  List of globals referenced by body of procedure

      Call_List : constant Elist_Id := New_Elmt_List;
      --  List of recursive calls in body of procedure

      Shad_List : constant Elist_Id := New_Elmt_List;
      --  List of entity id's for entities created to capture the
      --  value of referenced globals on entry to the procedure.

      Scop : constant Uint := Scope_Depth (Spec);
      --  This is used to record the scope depth of the current
      --  procedure, so that we can identify global references.

      Max_Vars : constant := 4;
      --  Do not test more than four global variables

      Count_Vars : Natural := 0;
      --  Count variables found so far

      Var  : Entity_Id;
      Elm  : Elmt_Id;
      Ent  : Entity_Id;
      Call : Elmt_Id;
      Decl : Node_Id;
      Test : Node_Id;
      Elm1 : Elmt_Id;
      Elm2 : Elmt_Id;
      Last : Node_Id;

      function Process (Nod : Node_Id) return Traverse_Result;
      --  Function to traverse the subprogram body (using Traverse_Func)

      -------------
      -- Process --
      -------------

      function Process (Nod : Node_Id) return Traverse_Result is
      begin
         --  Procedure call

         if Nkind (Nod) = N_Procedure_Call_Statement then

            --  Case of one of the detected recursive calls

            if Is_Entity_Name (Name (Nod))
              and then Has_Recursive_Call (Entity (Name (Nod)))
              and then Entity (Name (Nod)) = Spec
            then
               Append_Elmt (Nod, Call_List);
               return Skip;

            --  Any other procedure call may have side effects

            else
               return Abandon;
            end if;

         --  A call to a pure function can always be ignored

         elsif Nkind (Nod) = N_Function_Call
           and then Is_Entity_Name (Name (Nod))
           and then Is_Pure (Entity (Name (Nod)))
         then
            return Skip;

         --  Case of an identifier reference

         elsif Nkind (Nod) = N_Identifier then
            Ent := Entity (Nod);

            --  If no entity, then ignore the reference

            --  Not clear why this can happen. To investigate, remove this
            --  test and look at the crash that occurs here in 3401-004 ???

            if No (Ent) then
               return Skip;

            --  Ignore entities with no Scope, again not clear how this
            --  can happen, to investigate, look at 4108-008 ???

            elsif No (Scope (Ent)) then
               return Skip;

            --  Ignore the reference if not to a more global object

            elsif Scope_Depth (Scope (Ent)) >= Scop then
               return Skip;

            --  References to types, exceptions and constants are always OK

            elsif Is_Type (Ent)
              or else Ekind (Ent) = E_Exception
              or else Ekind (Ent) = E_Constant
            then
               return Skip;

            --  If other than a non-volatile scalar variable, we have some
            --  kind of global reference (e.g. to a function) that we cannot
            --  deal with so we forget the attempt.

            elsif Ekind (Ent) /= E_Variable
              or else not Is_Scalar_Type (Etype (Ent))
              or else Treat_As_Volatile (Ent)
            then
               return Abandon;

            --  Otherwise we have a reference to a global scalar

            else
               --  Loop through global entities already detected

               Elm := First_Elmt (Var_List);
               loop
                  --  If not detected before, record this new global reference

                  if No (Elm) then
                     Count_Vars := Count_Vars + 1;

                     if Count_Vars <= Max_Vars then
                        Append_Elmt (Entity (Nod), Var_List);
                     else
                        return Abandon;
                     end if;

                     exit;

                  --  If recorded before, ignore

                  elsif Node (Elm) = Entity (Nod) then
                     return Skip;

                  --  Otherwise keep looking

                  else
                     Next_Elmt (Elm);
                  end if;
               end loop;

               return Skip;
            end if;

         --  For all other node kinds, recursively visit syntactic children

         else
            return OK;
         end if;
      end Process;

      function Traverse_Body is new Traverse_Func;

   --  Start of processing for Detect_Infinite_Recursion

   begin
      --  Do not attempt detection in No_Implicit_Conditional mode,
      --  since we won't be able to generate the code to handle the
      --  recursion in any case.

      if Restriction_Active (No_Implicit_Conditionals) then
         return;
      end if;

      --  Otherwise do traversal and quit if we get abandon signal

      if Traverse_Body (N) = Abandon then
         return;

      --  We must have a call, since Has_Recursive_Call was set. If not
      --  just ignore (this is only an error check, so if we have a funny
      --  situation, due to bugs or errors, we do not want to bomb!)

      elsif Is_Empty_Elmt_List (Call_List) then
         return;
      end if;

      --  Here is the case where we detect recursion at compile time

      --  Push our current scope for analyzing the declarations and
      --  code that we will insert for the checking.

      New_Scope (Spec);

      --  This loop builds temporary variables for each of the
      --  referenced globals, so that at the end of the loop the
      --  list Shad_List contains these temporaries in one-to-one
      --  correspondence with the elements in Var_List.

      Last := Empty;
      Elm := First_Elmt (Var_List);
      while Present (Elm) loop
         Var := Node (Elm);
         Ent :=
           Make_Defining_Identifier (Loc,
             Chars => New_Internal_Name ('S'));
         Append_Elmt (Ent, Shad_List);

         --  Insert a declaration for this temporary at the start of
         --  the declarations for the procedure. The temporaries are
         --  declared as constant objects initialized to the current
         --  values of the corresponding temporaries.

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => New_Occurrence_Of (Etype (Var), Loc),
             Constant_Present    => True,
             Expression          => New_Occurrence_Of (Var, Loc));

         if No (Last) then
            Prepend (Decl, Declarations (N));
         else
            Insert_After (Last, Decl);
         end if;

         Last := Decl;
         Analyze (Decl);
         Next_Elmt (Elm);
      end loop;

      --  Loop through calls

      Call := First_Elmt (Call_List);
      while Present (Call) loop

         --  Build a predicate expression of the form

         --    True
         --      and then global1 = temp1
         --      and then global2 = temp2
         --      ...

         --  This predicate determines if any of the global values
         --  referenced by the procedure have changed since the
         --  current call, if not an infinite recursion is assured.

         Test := New_Occurrence_Of (Standard_True, Loc);

         Elm1 := First_Elmt (Var_List);
         Elm2 := First_Elmt (Shad_List);
         while Present (Elm1) loop
            Test :=
              Make_And_Then (Loc,
                Left_Opnd  => Test,
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  => New_Occurrence_Of (Node (Elm1), Loc),
                    Right_Opnd => New_Occurrence_Of (Node (Elm2), Loc)));

            Next_Elmt (Elm1);
            Next_Elmt (Elm2);
         end loop;

         --  Now we replace the call with the sequence

         --    if no-changes (see above) then
         --       raise Storage_Error;
         --    else
         --       original-call
         --    end if;

         Rewrite (Node (Call),
           Make_If_Statement (Loc,
             Condition       => Test,
             Then_Statements => New_List (
               Make_Raise_Storage_Error (Loc,
                 Reason => SE_Infinite_Recursion)),

             Else_Statements => New_List (
               Relocate_Node (Node (Call)))));

         Analyze (Node (Call));

         Next_Elmt (Call);
      end loop;

      --  Remove temporary scope stack entry used for analysis

      Pop_Scope;
   end Detect_Infinite_Recursion;

   --------------------
   -- Expand_Actuals --
   --------------------

   procedure Expand_Actuals (N : Node_Id; Subp : Entity_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Actual    : Node_Id;
      Formal    : Entity_Id;
      N_Node    : Node_Id;
      Post_Call : List_Id;
      E_Formal  : Entity_Id;

      procedure Add_Call_By_Copy_Code;
      --  For cases where the parameter must be passed by copy, this routine
      --  generates a temporary variable into which the actual is copied and
      --  then passes this as the parameter. For an OUT or IN OUT parameter,
      --  an assignment is also generated to copy the result back. The call
      --  also takes care of any constraint checks required for the type
      --  conversion case (on both the way in and the way out).

      procedure Add_Packed_Call_By_Copy_Code;
      --  This is used when the actual involves a reference to an element
      --  of a packed array, where we can appropriately use a simpler
      --  approach than the full call by copy code. We just copy the value
      --  in and out of an appropriate temporary.

      procedure Check_Fortran_Logical;
      --  A value of type Logical that is passed through a formal parameter
      --  must be normalized because .TRUE. usually does not have the same
      --  representation as True. We assume that .FALSE. = False = 0.
      --  What about functions that return a logical type ???

      function Make_Var (Actual : Node_Id) return Entity_Id;
      --  Returns an entity that refers to the given actual parameter,
      --  Actual (not including any type conversion). If Actual is an
      --  entity name, then this entity is returned unchanged, otherwise
      --  a renaming is created to provide an entity for the actual.

      procedure Reset_Packed_Prefix;
      --  The expansion of a packed array component reference is delayed in
      --  the context of a call. Now we need to complete the expansion, so we
      --  unmark the analyzed bits in all prefixes.

      ---------------------------
      -- Add_Call_By_Copy_Code --
      ---------------------------

      procedure Add_Call_By_Copy_Code is
         Expr  : Node_Id;
         Init  : Node_Id;
         Temp  : Entity_Id;
         Indic : Node_Id := New_Occurrence_Of (Etype (Formal), Loc);
         Var   : Entity_Id;
         F_Typ : constant Entity_Id := Etype (Formal);
         V_Typ : Entity_Id;
         Crep  : Boolean;

      begin
         Temp := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

         if Nkind (Actual) = N_Type_Conversion then
            V_Typ := Etype (Expression (Actual));

            --  If the formal is an (in-)out parameter, capture the name
            --  of the variable in order to build the post-call assignment.

            Var := Make_Var (Expression (Actual));

            Crep := not Same_Representation
                          (F_Typ, Etype (Expression (Actual)));

         else
            V_Typ := Etype (Actual);
            Var   := Make_Var (Actual);
            Crep  := False;
         end if;

         --  Setup initialization for case of in out parameter, or an out
         --  parameter where the formal is an unconstrained array (in the
         --  latter case, we have to pass in an object with bounds).

         --  If this is an out parameter, the initial copy is wasteful, so as
         --  an optimization for the one-dimensional case we extract the
         --  bounds of the actual and build an uninitialized temporary of the
         --  right size.

         if Ekind (Formal) = E_In_Out_Parameter
           or else (Is_Array_Type (F_Typ) and then not Is_Constrained (F_Typ))
         then
            if Nkind (Actual) = N_Type_Conversion then
               if Conversion_OK (Actual) then
                  Init := OK_Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
               else
                  Init := Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
               end if;

            elsif Ekind (Formal) = E_Out_Parameter
              and then Is_Array_Type (F_Typ)
              and then Number_Dimensions (F_Typ) = 1
              and then not Has_Non_Null_Base_Init_Proc (F_Typ)
            then
               --  Actual is a one-dimensional array or slice, and the type
               --  requires no initialization. Create a temporary of the
               --  right size, but do copy actual into it (optimization).

               Init := Empty;
               Indic :=
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (F_Typ, Loc),
                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => New_List (
                         Make_Range (Loc,
                           Low_Bound  =>
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Occurrence_Of (Var, Loc),
                               Attribute_name => Name_First),
                           High_Bound =>
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Occurrence_Of (Var, Loc),
                               Attribute_Name => Name_Last)))));

            else
               Init := New_Occurrence_Of (Var, Loc);
            end if;

         --  An initialization is created for packed conversions as
         --  actuals for out parameters to enable Make_Object_Declaration
         --  to determine the proper subtype for N_Node. Note that this
         --  is wasteful because the extra copying on the call side is
         --  not required for such out parameters. ???

         elsif Ekind (Formal) = E_Out_Parameter
           and then Nkind (Actual) = N_Type_Conversion
           and then (Is_Bit_Packed_Array (F_Typ)
                       or else
                     Is_Bit_Packed_Array (Etype (Expression (Actual))))
         then
            if Conversion_OK (Actual) then
               Init :=
                 OK_Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
            else
               Init :=
                 Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
            end if;

         elsif Ekind (Formal) = E_In_Parameter then
            Init := New_Occurrence_Of (Var, Loc);

         else
            Init := Empty;
         end if;

         N_Node :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => Indic,
             Expression => Init);
         Set_Assignment_OK (N_Node);
         Insert_Action (N, N_Node);

         --  Now, normally the deal here is that we use the defining
         --  identifier created by that object declaration. There is
         --  one exception to this. In the change of representation case
         --  the above declaration will end up looking like:

         --    temp : type := identifier;

         --  And in this case we might as well use the identifier directly
         --  and eliminate the temporary. Note that the analysis of the
         --  declaration was not a waste of time in that case, since it is
         --  what generated the necessary change of representation code. If
         --  the change of representation introduced additional code, as in
         --  a fixed-integer conversion, the expression is not an identifier
         --  and must be kept.

         if Crep
           and then Present (Expression (N_Node))
           and then Is_Entity_Name (Expression (N_Node))
         then
            Temp := Entity (Expression (N_Node));
            Rewrite (N_Node, Make_Null_Statement (Loc));
         end if;

         --  For IN parameter, all we do is to replace the actual

         if Ekind (Formal) = E_In_Parameter then
            Rewrite (Actual, New_Reference_To (Temp, Loc));
            Analyze (Actual);

         --  Processing for OUT or IN OUT parameter

         else
            --  If type conversion, use reverse conversion on exit

            if Nkind (Actual) = N_Type_Conversion then
               if Conversion_OK (Actual) then
                  Expr := OK_Convert_To (V_Typ, New_Occurrence_Of (Temp, Loc));
               else
                  Expr := Convert_To (V_Typ, New_Occurrence_Of (Temp, Loc));
               end if;
            else
               Expr := New_Occurrence_Of (Temp, Loc);
            end if;

            Rewrite (Actual, New_Reference_To (Temp, Loc));
            Analyze (Actual);

            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Var, Loc),
                Expression => Expr));

            Set_Assignment_OK (Name (Last (Post_Call)));
         end if;
      end Add_Call_By_Copy_Code;

      ----------------------------------
      -- Add_Packed_Call_By_Copy_Code --
      ----------------------------------

      procedure Add_Packed_Call_By_Copy_Code is
         Temp   : Entity_Id;
         Incod  : Node_Id;
         Outcod : Node_Id;
         Lhs    : Node_Id;
         Rhs    : Node_Id;

      begin
         Reset_Packed_Prefix;

         --  Prepare to generate code

         Temp := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));
         Incod  := Relocate_Node (Actual);
         Outcod := New_Copy_Tree (Incod);

         --  Generate declaration of temporary variable, initializing it
         --  with the input parameter unless we have an OUT variable.

         if Ekind (Formal) = E_Out_Parameter then
            Incod := Empty;
         end if;

         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   =>
               New_Occurrence_Of (Etype (Formal), Loc),
             Expression => Incod));

         --  The actual is simply a reference to the temporary

         Rewrite (Actual, New_Occurrence_Of (Temp, Loc));

         --  Generate copy out if OUT or IN OUT parameter

         if Ekind (Formal) /= E_In_Parameter then
            Lhs := Outcod;
            Rhs := New_Occurrence_Of (Temp, Loc);

            --  Deal with conversion

            if Nkind (Lhs) = N_Type_Conversion then
               Lhs := Expression (Lhs);
               Rhs := Convert_To (Etype (Actual), Rhs);
            end if;

            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name       => Lhs,
                Expression => Rhs));
         end if;
      end Add_Packed_Call_By_Copy_Code;

      ---------------------------
      -- Check_Fortran_Logical --
      ---------------------------

      procedure Check_Fortran_Logical is
         Logical : constant Entity_Id := Etype (Formal);
         Var     : Entity_Id;

      --  Note: this is very incomplete, e.g. it does not handle arrays
      --  of logical values. This is really not the right approach at all???)

      begin
         if Convention (Subp) = Convention_Fortran
           and then Root_Type (Etype (Formal)) = Standard_Boolean
           and then Ekind (Formal) /= E_In_Parameter
         then
            Var := Make_Var (Actual);
            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name => New_Occurrence_Of (Var, Loc),
                Expression =>
                  Unchecked_Convert_To (
                    Logical,
                    Make_Op_Ne (Loc,
                      Left_Opnd  => New_Occurrence_Of (Var, Loc),
                      Right_Opnd =>
                        Unchecked_Convert_To (
                          Logical,
                          New_Occurrence_Of (Standard_False, Loc))))));
         end if;
      end Check_Fortran_Logical;

      --------------
      -- Make_Var --
      --------------

      function Make_Var (Actual : Node_Id) return Entity_Id is
         Var : Entity_Id;

      begin
         if Is_Entity_Name (Actual) then
            return Entity (Actual);

         else
            Var := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

            N_Node :=
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Var,
                Subtype_Mark        =>
                  New_Occurrence_Of (Etype (Actual), Loc),
                Name                => Relocate_Node (Actual));

            Insert_Action (N, N_Node);
            return Var;
         end if;
      end Make_Var;

      -------------------------
      -- Reset_Packed_Prefix --
      -------------------------

      procedure Reset_Packed_Prefix is
         Pfx : Node_Id := Actual;

      begin
         loop
            Set_Analyzed (Pfx, False);
            exit when Nkind (Pfx) /= N_Selected_Component
              and then Nkind (Pfx) /= N_Indexed_Component;
            Pfx := Prefix (Pfx);
         end loop;
      end Reset_Packed_Prefix;

   --  Start of processing for Expand_Actuals

   begin
      Formal := First_Formal (Subp);
      Actual := First_Actual (N);

      Post_Call := New_List;

      while Present (Formal) loop
         E_Formal := Etype (Formal);

         if Is_Scalar_Type (E_Formal)
           or else Nkind (Actual) = N_Slice
         then
            Check_Fortran_Logical;

         --  RM 6.4.1 (11)

         elsif Ekind (Formal) /= E_Out_Parameter then

            --  The unusual case of the current instance of a protected type
            --  requires special handling. This can only occur in the context
            --  of a call within the body of a protected operation.

            if Is_Entity_Name (Actual)
              and then Ekind (Entity (Actual)) = E_Protected_Type
              and then In_Open_Scopes (Entity (Actual))
            then
               if Scope (Subp) /= Entity (Actual) then
                  Error_Msg_N ("operation outside protected type may not "
                    & "call back its protected operations?", Actual);
               end if;

               Rewrite (Actual,
                 Expand_Protected_Object_Reference (N, Entity (Actual)));
            end if;

            Apply_Constraint_Check (Actual, E_Formal);

         --  Out parameter case. No constraint checks on access type
         --  RM 6.4.1 (13)

         elsif Is_Access_Type (E_Formal) then
            null;

         --  RM 6.4.1 (14)

         elsif Has_Discriminants (Base_Type (E_Formal))
           or else Has_Non_Null_Base_Init_Proc (E_Formal)
         then
            Apply_Constraint_Check (Actual, E_Formal);

         --  RM 6.4.1 (15)

         else
            Apply_Constraint_Check (Actual, Base_Type (E_Formal));
         end if;

         --  Processing for IN-OUT and OUT parameters

         if Ekind (Formal) /= E_In_Parameter then

            --  For type conversions of arrays, apply length/range checks

            if Is_Array_Type (E_Formal)
              and then Nkind (Actual) = N_Type_Conversion
            then
               if Is_Constrained (E_Formal) then
                  Apply_Length_Check (Expression (Actual), E_Formal);
               else
                  Apply_Range_Check (Expression (Actual), E_Formal);
               end if;
            end if;

            --  If argument is a type conversion for a type that is passed
            --  by copy, then we must pass the parameter by copy.

            if Nkind (Actual) = N_Type_Conversion
              and then
                (Is_Numeric_Type (E_Formal)
                  or else Is_Access_Type (E_Formal)
                  or else Is_Enumeration_Type (E_Formal)
                  or else Is_Bit_Packed_Array (Etype (Formal))
                  or else Is_Bit_Packed_Array (Etype (Expression (Actual)))

                  --  Also pass by copy if change of representation

                  or else not Same_Representation
                               (Etype (Formal),
                                Etype (Expression (Actual))))
            then
               Add_Call_By_Copy_Code;

            --  References to components of bit packed arrays are expanded
            --  at this point, rather than at the point of analysis of the
            --  actuals, to handle the expansion of the assignment to
            --  [in] out parameters.

            elsif Is_Ref_To_Bit_Packed_Array (Actual) then
               Add_Packed_Call_By_Copy_Code;

            --  References to slices of bit packed arrays are expanded

            elsif Is_Ref_To_Bit_Packed_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  References to possibly unaligned slices of arrays are expanded

            elsif Is_Possibly_Unaligned_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  Deal with access types where the actual subtpe and the
            --  formal subtype are not the same, requiring a check.

            --  It is necessary to exclude tagged types because of "downward
            --  conversion" errors and a strange assertion error in namet
            --  from gnatf in bug 1215-001 ???

            elsif Is_Access_Type (E_Formal)
              and then not Same_Type (E_Formal, Etype (Actual))
              and then not Is_Tagged_Type (Designated_Type (E_Formal))
            then
               Add_Call_By_Copy_Code;

            --  If the actual is not a scalar and is marked for volatile
            --  treatment, whereas the formal is not volatile, then pass
            --  by copy unless it is a by-reference type.

            elsif Is_Entity_Name (Actual)
              and then Treat_As_Volatile (Entity (Actual))
              and then not Is_By_Reference_Type (Etype (Actual))
              and then not Is_Scalar_Type (Etype (Entity (Actual)))
              and then not Treat_As_Volatile (E_Formal)
            then
               Add_Call_By_Copy_Code;

            elsif Nkind (Actual) = N_Indexed_Component
              and then Is_Entity_Name (Prefix (Actual))
              and then Has_Volatile_Components (Entity (Prefix (Actual)))
            then
               Add_Call_By_Copy_Code;
            end if;

         --  Processing for IN parameters

         else
            --  For IN parameters is in the packed array case, we expand an
            --  indexed component (the circuit in Exp_Ch4 deliberately left
            --  indexed components appearing as actuals untouched, so that
            --  the special processing above for the OUT and IN OUT cases
            --  could be performed. We could make the test in Exp_Ch4 more
            --  complex and have it detect the parameter mode, but it is
            --  easier simply to handle all cases here.

            if Nkind (Actual) = N_Indexed_Component
              and then Is_Packed (Etype (Prefix (Actual)))
            then
               Reset_Packed_Prefix;
               Expand_Packed_Element_Reference (Actual);

            --  If we have a reference to a bit packed array, we copy it,
            --  since the actual must be byte aligned.

            --  Is this really necessary in all cases???

            elsif Is_Ref_To_Bit_Packed_Array (Actual) then
               Add_Packed_Call_By_Copy_Code;

            --  Similarly, we have to expand slices of packed arrays here
            --  because the result must be byte aligned.

            elsif Is_Ref_To_Bit_Packed_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  Only processing remaining is to pass by copy if this is a
            --  reference to a possibly unaligned slice, since the caller
            --  expects an appropriately aligned argument.

            elsif Is_Possibly_Unaligned_Slice (Actual) then
               Add_Call_By_Copy_Code;
            end if;
         end if;

         Next_Formal (Formal);
         Next_Actual (Actual);
      end loop;

      --  Find right place to put post call stuff if it is present

      if not Is_Empty_List (Post_Call) then

         --  If call is not a list member, it must be the triggering
         --  statement of a triggering alternative or an entry call
         --  alternative, and we can add the post call stuff to the
         --  corresponding statement list.

         if not Is_List_Member (N) then
            declare
               P : constant Node_Id := Parent (N);

            begin
               pragma Assert (Nkind (P) = N_Triggering_Alternative
                 or else Nkind (P) = N_Entry_Call_Alternative);

               if Is_Non_Empty_List (Statements (P)) then
                  Insert_List_Before_And_Analyze
                    (First (Statements (P)), Post_Call);
               else
                  Set_Statements (P, Post_Call);
               end if;
            end;

         --  Otherwise, normal case where N is in a statement sequence,
         --  just put the post-call stuff after the call statement.

         else
            Insert_Actions_After (N, Post_Call);
         end if;
      end if;

      --  The call node itself is re-analyzed in Expand_Call

   end Expand_Actuals;

   -----------------
   -- Expand_Call --
   -----------------

   --  This procedure handles expansion of function calls and procedure call
   --  statements (i.e. it serves as the body for Expand_N_Function_Call and
   --  Expand_N_Procedure_Call_Statement. Processing for calls includes:

   --    Replace call to Raise_Exception by Raise_Exception always if possible
   --    Provide values of actuals for all formals in Extra_Formals list
   --    Replace "call" to enumeration literal function by literal itself
   --    Rewrite call to predefined operator as operator
   --    Replace actuals to in-out parameters that are numeric conversions,
   --     with explicit assignment to temporaries before and after the call.
   --    Remove optional actuals if First_Optional_Parameter specified.

   --   Note that the list of actuals has been filled with default expressions
   --   during semantic analysis of the call. Only the extra actuals required
   --   for the 'Constrained attribute and for accessibility checks are added
   --   at this point.

   procedure Expand_Call (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Remote        : constant Boolean    := Is_Remote_Call (N);
      Subp          : Entity_Id;
      Orig_Subp     : Entity_Id := Empty;
      Parent_Subp   : Entity_Id;
      Parent_Formal : Entity_Id;
      Actual        : Node_Id;
      Formal        : Entity_Id;
      Prev          : Node_Id := Empty;
      Prev_Orig     : Node_Id;
      Scop          : Entity_Id;
      Extra_Actuals : List_Id := No_List;
      Cond          : Node_Id;

      procedure Add_Actual_Parameter (Insert_Param : Node_Id);
      --  Adds one entry to the end of the actual parameter list. Used for
      --  default parameters and for extra actuals (for Extra_Formals).
      --  The argument is an N_Parameter_Association node.

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id);
      --  Adds an extra actual to the list of extra actuals. Expr
      --  is the expression for the value of the actual, EF is the
      --  entity for the extra formal.

      function Inherited_From_Formal (S : Entity_Id) return Entity_Id;
      --  Within an instance, a type derived from a non-tagged formal derived
      --  type inherits from the original parent, not from the actual. This is
      --  tested in 4723-003. The current derivation mechanism has the derived
      --  type inherit from the actual, which is only correct outside of the
      --  instance. If the subprogram is inherited, we test for this particular
      --  case through a convoluted tree traversal before setting the proper
      --  subprogram to be called.

      --------------------------
      -- Add_Actual_Parameter --
      --------------------------

      procedure Add_Actual_Parameter (Insert_Param : Node_Id) is
         Actual_Expr : constant Node_Id :=
                         Explicit_Actual_Parameter (Insert_Param);

      begin
         --  Case of insertion is first named actual

         if No (Prev) or else
            Nkind (Parent (Prev)) /= N_Parameter_Association
         then
            Set_Next_Named_Actual (Insert_Param, First_Named_Actual (N));
            Set_First_Named_Actual (N, Actual_Expr);

            if No (Prev) then
               if not Present (Parameter_Associations (N)) then
                  Set_Parameter_Associations (N, New_List);
                  Append (Insert_Param, Parameter_Associations (N));
               end if;
            else
               Insert_After (Prev, Insert_Param);
            end if;

         --  Case of insertion is not first named actual

         else
            Set_Next_Named_Actual
              (Insert_Param, Next_Named_Actual (Parent (Prev)));
            Set_Next_Named_Actual (Parent (Prev), Actual_Expr);
            Append (Insert_Param, Parameter_Associations (N));
         end if;

         Prev := Actual_Expr;
      end Add_Actual_Parameter;

      ----------------------
      -- Add_Extra_Actual --
      ----------------------

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (Expr);

      begin
         if Extra_Actuals = No_List then
            Extra_Actuals := New_List;
            Set_Parent (Extra_Actuals, N);
         end if;

         Append_To (Extra_Actuals,
           Make_Parameter_Association (Loc,
             Explicit_Actual_Parameter => Expr,
             Selector_Name =>
               Make_Identifier (Loc, Chars (EF))));

         Analyze_And_Resolve (Expr, Etype (EF));
      end Add_Extra_Actual;

      ---------------------------
      -- Inherited_From_Formal --
      ---------------------------

      function Inherited_From_Formal (S : Entity_Id) return Entity_Id is
         Par      : Entity_Id;
         Gen_Par  : Entity_Id;
         Gen_Prim : Elist_Id;
         Elmt     : Elmt_Id;
         Indic    : Node_Id;

      begin
         --  If the operation is inherited, it is attached to the corresponding
         --  type derivation. If the parent in the derivation is a generic
         --  actual, it is a subtype of the actual, and we have to recover the
         --  original derived type declaration to find the proper parent.

         if Nkind (Parent (S)) /= N_Full_Type_Declaration
           or else not Is_Derived_Type (Defining_Identifier (Parent (S)))
           or else Nkind (Type_Definition (Original_Node (Parent (S))))
             /= N_Derived_Type_Definition
           or else not In_Instance
         then
            return Empty;

         else
            Indic :=
              (Subtype_Indication
                (Type_Definition (Original_Node (Parent (S)))));

            if Nkind (Indic) = N_Subtype_Indication then
               Par := Entity (Subtype_Mark (Indic));
            else
               Par := Entity (Indic);
            end if;
         end if;

         if not Is_Generic_Actual_Type (Par)
           or else Is_Tagged_Type (Par)
           or else Nkind (Parent (Par)) /= N_Subtype_Declaration
           or else not In_Open_Scopes (Scope (Par))
         then
            return Empty;

         else
            Gen_Par := Generic_Parent_Type (Parent (Par));
         end if;

         --  If the generic parent type is still the generic type, this
         --  is a private formal, not a derived formal, and there are no
         --  operations inherited from the formal.

         if Nkind (Parent (Gen_Par)) = N_Formal_Type_Declaration then
            return Empty;
         end if;

         Gen_Prim := Collect_Primitive_Operations (Gen_Par);
         Elmt := First_Elmt (Gen_Prim);

         while Present (Elmt) loop
            if Chars (Node (Elmt)) = Chars (S) then
               declare
                  F1 : Entity_Id;
                  F2 : Entity_Id;
               begin

                  F1 := First_Formal (S);
                  F2 := First_Formal (Node (Elmt));

                  while Present (F1)
                    and then Present (F2)
                  loop

                     if Etype (F1) = Etype (F2)
                       or else Etype (F2) = Gen_Par
                     then
                        Next_Formal (F1);
                        Next_Formal (F2);
                     else
                        Next_Elmt (Elmt);
                        exit;   --  not the right subprogram
                     end if;

                     return Node (Elmt);
                  end loop;
               end;

            else
               Next_Elmt (Elmt);
            end if;
         end loop;

         raise Program_Error;
      end Inherited_From_Formal;

   --  Start of processing for Expand_Call

   begin
      --  Ignore if previous error

      if Nkind (N) in N_Has_Etype and then Etype (N) = Any_Type then
         return;
      end if;

      --  Call using access to subprogram with explicit dereference

      if Nkind (Name (N)) = N_Explicit_Dereference then
         Subp        := Etype (Name (N));
         Parent_Subp := Empty;

      --  Case of call to simple entry, where the Name is a selected component
      --  whose prefix is the task, and whose selector name is the entry name

      elsif Nkind (Name (N)) = N_Selected_Component then
         Subp        := Entity (Selector_Name (Name (N)));
         Parent_Subp := Empty;

      --  Case of call to member of entry family, where Name is an indexed
      --  component, with the prefix being a selected component giving the
      --  task and entry family name, and the index being the entry index.

      elsif Nkind (Name (N)) = N_Indexed_Component then
         Subp        := Entity (Selector_Name (Prefix (Name (N))));
         Parent_Subp := Empty;

      --  Normal case

      else
         Subp        := Entity (Name (N));
         Parent_Subp := Alias (Subp);

         --  Replace call to Raise_Exception by call to Raise_Exception_Always
         --  if we can tell that the first parameter cannot possibly be null.
         --  This helps optimization and also generation of warnings.

         if not Restriction_Active (No_Exception_Handlers)
           and then Is_RTE (Subp, RE_Raise_Exception)
         then
            declare
               FA : constant Node_Id := Original_Node (First_Actual (N));

            begin
               --  The case we catch is where the first argument is obtained
               --  using the Identity attribute (which must always be non-null)

               if Nkind (FA) = N_Attribute_Reference
                 and then Attribute_Name (FA) = Name_Identity
               then
                  Subp := RTE (RE_Raise_Exception_Always);
                  Set_Entity (Name (N), Subp);
               end if;
            end;
         end if;

         if Ekind (Subp) = E_Entry then
            Parent_Subp := Empty;
         end if;
      end if;

      --  First step, compute extra actuals, corresponding to any
      --  Extra_Formals present. Note that we do not access Extra_Formals
      --  directly, instead we simply note the presence of the extra
      --  formals as we process the regular formals and collect the
      --  corresponding actuals in Extra_Actuals.

      --  We also generate any required range checks for actuals as we go
      --  through the loop, since this is a convenient place to do this.

      Formal := First_Formal (Subp);
      Actual := First_Actual (N);
      while Present (Formal) loop

         --  Generate range check if required (not activated yet ???)

--         if Do_Range_Check (Actual) then
--            Set_Do_Range_Check (Actual, False);
--            Generate_Range_Check
--              (Actual, Etype (Formal), CE_Range_Check_Failed);
--         end if;

         --  Prepare to examine current entry

         Prev := Actual;
         Prev_Orig := Original_Node (Prev);

         --  Create possible extra actual for constrained case. Usually,
         --  the extra actual is of the form actual'constrained, but since
         --  this attribute is only available for unconstrained records,
         --  TRUE is expanded if the type of the formal happens to be
         --  constrained (for instance when this procedure is inherited
         --  from an unconstrained record to a constrained one) or if the
         --  actual has no discriminant (its type is constrained). An
         --  exception to this is the case of a private type without
         --  discriminants. In this case we pass FALSE because the
         --  object has underlying discriminants with defaults.

         if Present (Extra_Constrained (Formal)) then
            if Ekind (Etype (Prev)) in Private_Kind
              and then not Has_Discriminants (Base_Type (Etype (Prev)))
            then
               Add_Extra_Actual (
                 New_Occurrence_Of (Standard_False, Loc),
                 Extra_Constrained (Formal));

            elsif Is_Constrained (Etype (Formal))
              or else not Has_Discriminants (Etype (Prev))
            then
               Add_Extra_Actual (
                 New_Occurrence_Of (Standard_True, Loc),
                 Extra_Constrained (Formal));

            --  Do not produce extra actuals for Unchecked_Union parameters.
            --  Jump directly to the end of the loop.

            elsif Is_Unchecked_Union (Base_Type (Etype (Actual))) then
               goto Skip_Extra_Actual_Generation;

            else
               --  If the actual is a type conversion, then the constrained
               --  test applies to the actual, not the target type.

               declare
                  Act_Prev : Node_Id := Prev;

               begin
                  --  Test for unchecked conversions as well, which can
                  --  occur as out parameter actuals on calls to stream
                  --  procedures.

                  while Nkind (Act_Prev) = N_Type_Conversion
                    or else Nkind (Act_Prev) = N_Unchecked_Type_Conversion
                  loop
                     Act_Prev := Expression (Act_Prev);
                  end loop;

                  Add_Extra_Actual (
                    Make_Attribute_Reference (Sloc (Prev),
                      Prefix =>
                        Duplicate_Subexpr_No_Checks
                          (Act_Prev, Name_Req => True),
                      Attribute_Name => Name_Constrained),
                    Extra_Constrained (Formal));
               end;
            end if;
         end if;

         --  Create possible extra actual for accessibility level

         if Present (Extra_Accessibility (Formal)) then
            if Is_Entity_Name (Prev_Orig) then

               --  When passing an access parameter as the actual to another
               --  access parameter we need to pass along the actual's own
               --  associated access level parameter. This is done if we are
               --  in the scope of the formal access parameter (if this is an
               --  inlined body the extra formal is irrelevant).

               if Ekind (Entity (Prev_Orig)) in Formal_Kind
                 and then Ekind (Etype (Prev_Orig)) = E_Anonymous_Access_Type
                 and then In_Open_Scopes (Scope (Entity (Prev_Orig)))
               then
                  declare
                     Parm_Ent : constant Entity_Id := Param_Entity (Prev_Orig);

                  begin
                     pragma Assert (Present (Parm_Ent));

                     if Present (Extra_Accessibility (Parm_Ent)) then
                        Add_Extra_Actual (
                          New_Occurrence_Of
                            (Extra_Accessibility (Parm_Ent), Loc),
                          Extra_Accessibility (Formal));

                     --  If the actual access parameter does not have an
                     --  associated extra formal providing its scope level,
                     --  then treat the actual as having library-level
                     --  accessibility.

                     else
                        Add_Extra_Actual (
                          Make_Integer_Literal (Loc,
                            Intval => Scope_Depth (Standard_Standard)),
                          Extra_Accessibility (Formal));
                     end if;
                  end;

               --  The actual is a normal access value, so just pass the
               --  level of the actual's access type.

               else
                  Add_Extra_Actual (
                    Make_Integer_Literal (Loc,
                      Intval => Type_Access_Level (Etype (Prev_Orig))),
                    Extra_Accessibility (Formal));
               end if;

            else
               case Nkind (Prev_Orig) is

                  when N_Attribute_Reference =>

                     case Get_Attribute_Id (Attribute_Name (Prev_Orig)) is

                        --  For X'Access, pass on the level of the prefix X

                        when Attribute_Access =>
                           Add_Extra_Actual (
                             Make_Integer_Literal (Loc,
                               Intval =>
                                 Object_Access_Level (Prefix (Prev_Orig))),
                             Extra_Accessibility (Formal));

                        --  Treat the unchecked attributes as library-level

                        when Attribute_Unchecked_Access |
                           Attribute_Unrestricted_Access =>
                           Add_Extra_Actual (
                             Make_Integer_Literal (Loc,
                               Intval => Scope_Depth (Standard_Standard)),
                             Extra_Accessibility (Formal));

                        --  No other cases of attributes returning access
                        --  values that can be passed to access parameters

                        when others =>
                           raise Program_Error;

                     end case;

                  --  For allocators we pass the level of the execution of
                  --  the called subprogram, which is one greater than the
                  --  current scope level.

                  when N_Allocator =>
                     Add_Extra_Actual (
                       Make_Integer_Literal (Loc,
                        Scope_Depth (Current_Scope) + 1),
                       Extra_Accessibility (Formal));

                  --  For other cases we simply pass the level of the
                  --  actual's access type.

                  when others =>
                     Add_Extra_Actual (
                       Make_Integer_Literal (Loc,
                         Intval => Type_Access_Level (Etype (Prev_Orig))),
                       Extra_Accessibility (Formal));

               end case;
            end if;
         end if;

         --  Perform the check of 4.6(49) that prevents a null value
         --  from being passed as an actual to an access parameter.
         --  Note that the check is elided in the common cases of
         --  passing an access attribute or access parameter as an
         --  actual. Also, we currently don't enforce this check for
         --  expander-generated actuals and when -gnatdj is set.

         if Ekind (Etype (Formal)) /= E_Anonymous_Access_Type
           or else Access_Checks_Suppressed (Subp)
         then
            null;

         elsif Debug_Flag_J then
            null;

         elsif not Comes_From_Source (Prev) then
            null;

         elsif Is_Entity_Name (Prev)
           and then Ekind (Etype (Prev)) = E_Anonymous_Access_Type
         then
            null;

         elsif Nkind (Prev) = N_Allocator
           or else Nkind (Prev) = N_Attribute_Reference
         then
            null;

         --  Suppress null checks when passing to access parameters
         --  of Java subprograms. (Should this be done for other
         --  foreign conventions as well ???)

         elsif Convention (Subp) = Convention_Java then
            null;

            --  Ada 2005 (AI-231): do not force the check in case of Ada 2005
            --  unless it is a null-excluding type

         elsif Ada_Version < Ada_05
           or else Can_Never_Be_Null (Etype (Prev))
         then
            Cond :=
              Make_Op_Eq (Loc,
                Left_Opnd => Duplicate_Subexpr_No_Checks (Prev),
                Right_Opnd => Make_Null (Loc));
            Insert_Action (Prev,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Access_Parameter_Is_Null));
         end if;

         --  Perform appropriate validity checks on parameters that
         --  are entities.

         if Validity_Checks_On then
            if  (Ekind (Formal) = E_In_Parameter
                   and then Validity_Check_In_Params)
              or else
                (Ekind (Formal) = E_In_Out_Parameter
                   and then Validity_Check_In_Out_Params)
            then
               --  If the actual is an indexed component of a packed
               --  type, it has not been expanded yet. It will be
               --  copied in the validity code that follows, and has
               --  to be expanded appropriately, so reanalyze it.

               if Nkind (Actual) = N_Indexed_Component then
                  Set_Analyzed (Actual, False);
               end if;

               Ensure_Valid (Actual);
            end if;
         end if;

         --  For IN OUT and OUT parameters, ensure that subscripts are valid
         --  since this is a left side reference. We only do this for calls
         --  from the source program since we assume that compiler generated
         --  calls explicitly generate any required checks. We also need it
         --  only if we are doing standard validity checks, since clearly it
         --  is not needed if validity checks are off, and in subscript
         --  validity checking mode, all indexed components are checked with
         --  a call directly from Expand_N_Indexed_Component.

         if Comes_From_Source (N)
           and then Ekind (Formal) /= E_In_Parameter
           and then Validity_Checks_On
           and then Validity_Check_Default
           and then not Validity_Check_Subscripts
         then
            Check_Valid_Lvalue_Subscripts (Actual);
         end if;

         --  Mark any scalar OUT parameter that is a simple variable
         --  as no longer known to be valid (unless the type is always
         --  valid). This reflects the fact that if an OUT parameter
         --  is never set in a procedure, then it can become invalid
         --  on return from the procedure.

         if Ekind (Formal) = E_Out_Parameter
           and then Is_Entity_Name (Actual)
           and then Ekind (Entity (Actual)) = E_Variable
           and then not Is_Known_Valid (Etype (Actual))
         then
            Set_Is_Known_Valid (Entity (Actual), False);
         end if;

         --  For an OUT or IN OUT parameter of an access type, if the
         --  actual is an entity, then it is no longer known to be non-null.

         if Ekind (Formal) /= E_In_Parameter
           and then Is_Entity_Name (Actual)
           and then Is_Access_Type (Etype (Actual))
         then
            Set_Is_Known_Non_Null (Entity (Actual), False);
         end if;

         --  If the formal is class wide and the actual is an aggregate, force
         --  evaluation so that the back end who does not know about class-wide
         --  type, does not generate a temporary of the wrong size.

         if not Is_Class_Wide_Type (Etype (Formal)) then
            null;

         elsif Nkind (Actual) = N_Aggregate
           or else (Nkind (Actual) = N_Qualified_Expression
                     and then Nkind (Expression (Actual)) = N_Aggregate)
         then
            Force_Evaluation (Actual);
         end if;

         --  In a remote call, if the formal is of a class-wide type, check
         --  that the actual meets the requirements described in E.4(18).

         if Remote
           and then Is_Class_Wide_Type (Etype (Formal))
         then
            Insert_Action (Actual,
              Make_Implicit_If_Statement (N,
                Condition       =>
                  Make_Op_Not (Loc,
                    Get_Remotely_Callable
                      (Duplicate_Subexpr_Move_Checks (Actual))),
                Then_Statements => New_List (
                  Make_Raise_Program_Error (Loc,
                    Reason => PE_Illegal_RACW_E_4_18))));
         end if;

         --  This label is required when skipping extra actual generation for
         --  Unchecked_Union parameters.

         <<Skip_Extra_Actual_Generation>>

         Next_Actual (Actual);
         Next_Formal (Formal);
      end loop;

      --  If we are expanding a rhs of an assignement we need to check if
      --  tag propagation is needed. This code belongs theorically in Analyze
      --  Assignment but has to be done earlier (bottom-up) because the
      --  assignment might be transformed into a declaration for an uncons-
      --  trained value, if the expression is classwide.

      if Nkind (N) = N_Function_Call
        and then Is_Tag_Indeterminate (N)
        and then Is_Entity_Name (Name (N))
      then
         declare
            Ass : Node_Id := Empty;

         begin
            if Nkind (Parent (N)) = N_Assignment_Statement then
               Ass := Parent (N);

            elsif Nkind (Parent (N)) = N_Qualified_Expression
              and then Nkind (Parent (Parent (N))) = N_Assignment_Statement
            then
               Ass := Parent (Parent (N));
            end if;

            if Present (Ass)
              and then Is_Class_Wide_Type (Etype (Name (Ass)))
            then
               if Etype (N) /= Root_Type (Etype (Name (Ass))) then
                  Error_Msg_NE
                    ("tag-indeterminate expression must have type&"
                      & "('R'M 5.2 (6))", N, Root_Type (Etype (Name (Ass))));
               else
                  Propagate_Tag (Name (Ass), N);
               end if;

               --  The call will be rewritten as a dispatching call, and
               --  expanded as such.

               return;
            end if;
         end;
      end if;

      --  Deals with Dispatch_Call if we still have a call, before expanding
      --  extra actuals since this will be done on the re-analysis of the
      --  dispatching call. Note that we do not try to shorten the actual
      --  list for a dispatching call, it would not make sense to do so.
      --  Expansion of dispatching calls is suppressed when Java_VM, because
      --  the JVM back end directly handles the generation of dispatching
      --  calls and would have to undo any expansion to an indirect call.

      if (Nkind (N) = N_Function_Call
           or else Nkind (N) =  N_Procedure_Call_Statement)
        and then Present (Controlling_Argument (N))
        and then not Java_VM
      then
         Expand_Dispatching_Call (N);

         --  The following return is worrisome. Is it really OK to
         --  skip all remaining processing in this procedure ???

         return;

      --  Similarly, expand calls to RCI subprograms on which pragma
      --  All_Calls_Remote applies. The rewriting will be reanalyzed
      --  later. Do this only when the call comes from source since we do
      --  not want such a rewritting to occur in expanded code.

      elsif Is_All_Remote_Call (N) then
         Expand_All_Calls_Remote_Subprogram_Call (N);

      --  Similarly, do not add extra actuals for an entry call whose entity
      --  is a protected procedure, or for an internal protected subprogram
      --  call, because it will be rewritten as a protected subprogram call
      --  and reanalyzed (see Expand_Protected_Subprogram_Call).

      elsif Is_Protected_Type (Scope (Subp))
         and then (Ekind (Subp) = E_Procedure
                    or else Ekind (Subp) = E_Function)
      then
         null;

      --  During that loop we gathered the extra actuals (the ones that
      --  correspond to Extra_Formals), so now they can be appended.

      else
         while Is_Non_Empty_List (Extra_Actuals) loop
            Add_Actual_Parameter (Remove_Head (Extra_Actuals));
         end loop;
      end if;

      if Ekind (Subp) = E_Procedure
         or else (Ekind (Subp) = E_Subprogram_Type
                   and then Etype (Subp) = Standard_Void_Type)
         or else Is_Entry (Subp)
      then
         Expand_Actuals (N, Subp);
      end if;

      --  If the subprogram is a renaming, or if it is inherited, replace it
      --  in the call with the name of the actual subprogram being called.
      --  If this is a dispatching call, the run-time decides what to call.
      --  The Alias attribute does not apply to entries.

      if Nkind (N) /= N_Entry_Call_Statement
        and then No (Controlling_Argument (N))
        and then Present (Parent_Subp)
      then
         if Present (Inherited_From_Formal (Subp)) then
            Parent_Subp := Inherited_From_Formal (Subp);
         else
            while Present (Alias (Parent_Subp)) loop
               Parent_Subp := Alias (Parent_Subp);
            end loop;
         end if;

         Set_Entity (Name (N), Parent_Subp);

         if Is_Abstract (Parent_Subp)
           and then not In_Instance
         then
            Error_Msg_NE
              ("cannot call abstract subprogram &!", Name (N), Parent_Subp);
         end if;

         --  Add an explicit conversion for parameter of the derived type.
         --  This is only done for scalar and access in-parameters. Others
         --  have been expanded in expand_actuals.

         Formal := First_Formal (Subp);
         Parent_Formal := First_Formal (Parent_Subp);
         Actual := First_Actual (N);

         --  It is not clear that conversion is needed for intrinsic
         --  subprograms, but it certainly is for those that are user-
         --  defined, and that can be inherited on derivation, namely
         --  unchecked conversion and deallocation.
         --  General case needs study ???

         if not Is_Intrinsic_Subprogram (Parent_Subp)
           or else Is_Generic_Instance (Parent_Subp)
         then
            while Present (Formal) loop

               if Etype (Formal) /= Etype (Parent_Formal)
                 and then Is_Scalar_Type (Etype (Formal))
                 and then Ekind (Formal) = E_In_Parameter
                 and then not Raises_Constraint_Error (Actual)
               then
                  Rewrite (Actual,
                    OK_Convert_To (Etype (Parent_Formal),
                      Relocate_Node (Actual)));

                  Analyze (Actual);
                  Resolve (Actual, Etype (Parent_Formal));
                  Enable_Range_Check (Actual);

               elsif Is_Access_Type (Etype (Formal))
                 and then Base_Type (Etype (Parent_Formal))
                   /= Base_Type (Etype (Actual))
               then
                  if Ekind (Formal) /= E_In_Parameter then
                     Rewrite (Actual,
                       Convert_To (Etype (Parent_Formal),
                         Relocate_Node (Actual)));

                     Analyze (Actual);
                     Resolve (Actual, Etype (Parent_Formal));

                  elsif
                    Ekind (Etype (Parent_Formal)) = E_Anonymous_Access_Type
                      and then Designated_Type (Etype (Parent_Formal))
                                 /=
                               Designated_Type (Etype (Actual))
                      and then not Is_Controlling_Formal (Formal)
                  then
                     --  This unchecked conversion is not necessary unless
                     --  inlining is enabled, because in that case the type
                     --  mismatch may become visible in the body about to be
                     --  inlined.

                     Rewrite (Actual,
                       Unchecked_Convert_To (Etype (Parent_Formal),
                         Relocate_Node (Actual)));

                     Analyze (Actual);
                     Resolve (Actual, Etype (Parent_Formal));
                  end if;
               end if;

               Next_Formal (Formal);
               Next_Formal (Parent_Formal);
               Next_Actual (Actual);
            end loop;
         end if;

         Orig_Subp := Subp;
         Subp := Parent_Subp;
      end if;

      --  Check for violation of No_Abort_Statements

      if Is_RTE (Subp, RE_Abort_Task) then
         Check_Restriction (No_Abort_Statements, N);

      --  Check for violation of No_Dynamic_Attachment

      elsif RTU_Loaded (Ada_Interrupts)
        and then (Is_RTE (Subp, RE_Is_Reserved)      or else
                  Is_RTE (Subp, RE_Is_Attached)      or else
                  Is_RTE (Subp, RE_Current_Handler)  or else
                  Is_RTE (Subp, RE_Attach_Handler)   or else
                  Is_RTE (Subp, RE_Exchange_Handler) or else
                  Is_RTE (Subp, RE_Detach_Handler)   or else
                  Is_RTE (Subp, RE_Reference))
      then
         Check_Restriction (No_Dynamic_Attachment, N);
      end if;

      --  Deal with case where call is an explicit dereference

      if Nkind (Name (N)) = N_Explicit_Dereference then

      --  Handle case of access to protected subprogram type

         if Ekind (Base_Type (Etype (Prefix (Name (N))))) =
                               E_Access_Protected_Subprogram_Type
         then
            --  If this is a call through an access to protected operation,
            --  the prefix has the form (object'address, operation'access).
            --  Rewrite as a for other protected calls: the object is the
            --  first parameter of the list of actuals.

            declare
               Call : Node_Id;
               Parm : List_Id;
               Nam  : Node_Id;
               Obj  : Node_Id;
               Ptr  : constant Node_Id := Prefix (Name (N));

               T : constant Entity_Id :=
                     Equivalent_Type (Base_Type (Etype (Ptr)));

               D_T : constant Entity_Id :=
                       Designated_Type (Base_Type (Etype (Ptr)));

            begin
               Obj := Make_Selected_Component (Loc,
                 Prefix => Unchecked_Convert_To (T, Ptr),
                 Selector_Name => New_Occurrence_Of (First_Entity (T), Loc));

               Nam := Make_Selected_Component (Loc,
                 Prefix => Unchecked_Convert_To (T, Ptr),
                 Selector_Name => New_Occurrence_Of (
                   Next_Entity (First_Entity (T)), Loc));

               Nam := Make_Explicit_Dereference (Loc, Nam);

               if Present (Parameter_Associations (N))  then
                  Parm := Parameter_Associations (N);
               else
                  Parm := New_List;
               end if;

               Prepend (Obj, Parm);

               if Etype (D_T) = Standard_Void_Type then
                  Call := Make_Procedure_Call_Statement (Loc,
                    Name => Nam,
                    Parameter_Associations => Parm);
               else
                  Call := Make_Function_Call (Loc,
                    Name => Nam,
                    Parameter_Associations => Parm);
               end if;

               Set_First_Named_Actual (Call, First_Named_Actual (N));
               Set_Etype (Call, Etype (D_T));

               --  We do not re-analyze the call to avoid infinite recursion.
               --  We analyze separately the prefix and the object, and set
               --  the checks on the prefix that would otherwise be emitted
               --  when resolving a call.

               Rewrite (N, Call);
               Analyze (Nam);
               Apply_Access_Check (Nam);
               Analyze (Obj);
               return;
            end;
         end if;
      end if;

      --  If this is a call to an intrinsic subprogram, then perform the
      --  appropriate expansion to the corresponding tree node and we
      --  are all done (since after that the call is gone!)

      --  In the case where the intrinsic is to be processed by the back end,
      --  the call to Expand_Intrinsic_Call will do nothing, which is fine,
      --  since the idea in this case is to pass the call unchanged.

      if Is_Intrinsic_Subprogram (Subp) then
         Expand_Intrinsic_Call (N, Subp);
         return;
      end if;

      if Ekind (Subp) = E_Function
        or else Ekind (Subp) = E_Procedure
      then
         if Is_Inlined (Subp) then

            Inlined_Subprogram : declare
               Bod         : Node_Id;
               Must_Inline : Boolean := False;
               Spec        : constant Node_Id := Unit_Declaration_Node (Subp);
               Scop        : constant Entity_Id := Scope (Subp);

               function In_Unfrozen_Instance return Boolean;
               --  If the subprogram comes from an instance in the same
               --  unit, and the instance is not yet frozen, inlining might
               --  trigger order-of-elaboration problems in gigi.

               --------------------------
               -- In_Unfrozen_Instance --
               --------------------------

               function In_Unfrozen_Instance return Boolean is
                  S : Entity_Id := Scop;

               begin
                  while Present (S)
                    and then S /= Standard_Standard
                  loop
                     if Is_Generic_Instance (S)
                       and then Present (Freeze_Node (S))
                       and then not Analyzed (Freeze_Node (S))
                     then
                        return True;
                     end if;

                     S := Scope (S);
                  end loop;

                  return False;
               end In_Unfrozen_Instance;

            --  Start of processing for Inlined_Subprogram

            begin
               --  Verify that the body to inline has already been seen,
               --  and that if the body is in the current unit the inlining
               --  does not occur earlier. This avoids order-of-elaboration
               --  problems in gigi.

               if No (Spec)
                 or else Nkind (Spec) /= N_Subprogram_Declaration
                 or else No (Body_To_Inline (Spec))
               then
                  Must_Inline := False;

               --  If this an inherited function that returns a private
               --  type, do not inline if the full view is an unconstrained
               --  array, because such calls cannot be inlined.

               elsif Present (Orig_Subp)
                 and then Is_Array_Type (Etype (Orig_Subp))
                 and then not Is_Constrained (Etype (Orig_Subp))
               then
                  Must_Inline := False;

               elsif In_Unfrozen_Instance then
                  Must_Inline := False;

               else
                  Bod := Body_To_Inline (Spec);

                  if (In_Extended_Main_Code_Unit (N)
                        or else In_Extended_Main_Code_Unit (Parent (N))
                        or else Is_Always_Inlined (Subp))
                    and then (not In_Same_Extended_Unit (Sloc (Bod), Loc)
                               or else
                                 Earlier_In_Extended_Unit (Sloc (Bod), Loc))
                  then
                     Must_Inline := True;

                  --  If we are compiling a package body that is not the main
                  --  unit, it must be for inlining/instantiation purposes,
                  --  in which case we inline the call to insure that the same
                  --  temporaries are generated when compiling the body by
                  --  itself. Otherwise link errors can occur.

                  --  If the function being called is itself in the main unit,
                  --  we cannot inline, because there is a risk of double
                  --  elaboration and/or circularity: the inlining can make
                  --  visible a private entity in the body of the main unit,
                  --  that gigi will see before its sees its proper definition.

                  elsif not (In_Extended_Main_Code_Unit (N))
                    and then In_Package_Body
                  then
                     Must_Inline := not In_Extended_Main_Source_Unit (Subp);
                  end if;
               end if;

               if Must_Inline then
                  Expand_Inlined_Call (N, Subp, Orig_Subp);

               else
                  --  Let the back end handle it

                  Add_Inlined_Body (Subp);

                  if Front_End_Inlining
                    and then Nkind (Spec) = N_Subprogram_Declaration
                    and then (In_Extended_Main_Code_Unit (N))
                    and then No (Body_To_Inline (Spec))
                    and then not Has_Completion (Subp)
                    and then In_Same_Extended_Unit (Sloc (Spec), Loc)
                  then
                     Cannot_Inline
                      ("cannot inline& (body not seen yet)?",
                       N, Subp);
                  end if;
               end if;
            end Inlined_Subprogram;
         end if;
      end if;

      --  Check for a protected subprogram. This is either an intra-object
      --  call, or a protected function call. Protected procedure calls are
      --  rewritten as entry calls and handled accordingly.

      Scop := Scope (Subp);

      if Nkind (N) /= N_Entry_Call_Statement
        and then Is_Protected_Type (Scop)
      then
         --  If the call is an internal one, it is rewritten as a call to
         --  to the corresponding unprotected subprogram.

         Expand_Protected_Subprogram_Call (N, Subp, Scop);
      end if;

      --  Functions returning controlled objects need special attention

      if Controlled_Type (Etype (Subp))
        and then not Is_Return_By_Reference_Type (Etype (Subp))
      then
         Expand_Ctrl_Function_Call (N);
      end if;

      --  Test for First_Optional_Parameter, and if so, truncate parameter
      --  list if there are optional parameters at the trailing end.
      --  Note we never delete procedures for call via a pointer.

      if (Ekind (Subp) = E_Procedure or else Ekind (Subp) = E_Function)
        and then Present (First_Optional_Parameter (Subp))
      then
         declare
            Last_Keep_Arg : Node_Id;

         begin
            --  Last_Keep_Arg will hold the last actual that should be
            --  retained. If it remains empty at the end, it means that
            --  all parameters are optional.

            Last_Keep_Arg := Empty;

            --  Find first optional parameter, must be present since we
            --  checked the validity of the parameter before setting it.

            Formal := First_Formal (Subp);
            Actual := First_Actual (N);
            while Formal /= First_Optional_Parameter (Subp) loop
               Last_Keep_Arg := Actual;
               Next_Formal (Formal);
               Next_Actual (Actual);
            end loop;

            --  We have Formal and Actual pointing to the first potentially
            --  droppable argument. We can drop all the trailing arguments
            --  whose actual matches the default. Note that we know that all
            --  remaining formals have defaults, because we checked that this
            --  requirement was met before setting First_Optional_Parameter.

            --  We use Fully_Conformant_Expressions to check for identity
            --  between formals and actuals, which may miss some cases, but
            --  on the other hand, this is only an optimization (if we fail
            --  to truncate a parameter it does not affect functionality).
            --  So if the default is 3 and the actual is 1+2, we consider
            --  them unequal, which hardly seems worrisome.

            while Present (Formal) loop
               if not Fully_Conformant_Expressions
                    (Actual, Default_Value (Formal))
               then
                  Last_Keep_Arg := Actual;
               end if;

               Next_Formal (Formal);
               Next_Actual (Actual);
            end loop;

            --  If no arguments, delete entire list, this is the easy case

            if No (Last_Keep_Arg) then
               while Is_Non_Empty_List (Parameter_Associations (N)) loop
                  Delete_Tree (Remove_Head (Parameter_Associations (N)));
               end loop;

               Set_Parameter_Associations (N, No_List);
               Set_First_Named_Actual (N, Empty);

            --  Case where at the last retained argument is positional. This
            --  is also an easy case, since the retained arguments are already
            --  in the right form, and we don't need to worry about the order
            --  of arguments that get eliminated.

            elsif Is_List_Member (Last_Keep_Arg) then
               while Present (Next (Last_Keep_Arg)) loop
                  Delete_Tree (Remove_Next (Last_Keep_Arg));
               end loop;

               Set_First_Named_Actual (N, Empty);

            --  This is the annoying case where the last retained argument
            --  is a named parameter. Since the original arguments are not
            --  in declaration order, we may have to delete some fairly
            --  random collection of arguments.

            else
               declare
                  Temp   : Node_Id;
                  Passoc : Node_Id;

                  Discard : Node_Id;
                  pragma Warnings (Off, Discard);

               begin
                  --  First step, remove all the named parameters from the
                  --  list (they are still chained using First_Named_Actual
                  --  and Next_Named_Actual, so we have not lost them!)

                  Temp := First (Parameter_Associations (N));

                  --  Case of all parameters named, remove them all

                  if Nkind (Temp) = N_Parameter_Association then
                     while Is_Non_Empty_List (Parameter_Associations (N)) loop
                        Temp := Remove_Head (Parameter_Associations (N));
                     end loop;

                  --  Case of mixed positional/named, remove named parameters

                  else
                     while Nkind (Next (Temp)) /= N_Parameter_Association loop
                        Next (Temp);
                     end loop;

                     while Present (Next (Temp)) loop
                        Discard := Remove_Next (Temp);
                     end loop;
                  end if;

                  --  Now we loop through the named parameters, till we get
                  --  to the last one to be retained, adding them to the list.
                  --  Note that the Next_Named_Actual list does not need to be
                  --  touched since we are only reordering them on the actual
                  --  parameter association list.

                  Passoc := Parent (First_Named_Actual (N));
                  loop
                     Temp := Relocate_Node (Passoc);
                     Append_To
                       (Parameter_Associations (N), Temp);
                     exit when
                       Last_Keep_Arg = Explicit_Actual_Parameter (Passoc);
                     Passoc := Parent (Next_Named_Actual (Passoc));
                  end loop;

                  Set_Next_Named_Actual (Temp, Empty);

                  loop
                     Temp := Next_Named_Actual (Passoc);
                     exit when No (Temp);
                     Set_Next_Named_Actual
                       (Passoc, Next_Named_Actual (Parent (Temp)));
                     Delete_Tree (Temp);
                  end loop;
               end;
            end if;
         end;
      end if;
   end Expand_Call;

   --------------------------
   -- Expand_Inlined_Call --
   --------------------------

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Is_Predef : constant Boolean :=
                   Is_Predefined_File_Name
                     (Unit_File_Name (Get_Source_Unit (Subp)));
      Orig_Bod  : constant Node_Id :=
                    Body_To_Inline (Unit_Declaration_Node (Subp));

      Blk      : Node_Id;
      Bod      : Node_Id;
      Decl     : Node_Id;
      Exit_Lab : Entity_Id := Empty;
      F        : Entity_Id;
      A        : Node_Id;
      Lab_Decl : Node_Id;
      Lab_Id   : Node_Id;
      New_A    : Node_Id;
      Num_Ret  : Int := 0;
      Ret_Type : Entity_Id;
      Targ     : Node_Id;
      Temp     : Entity_Id;
      Temp_Typ : Entity_Id;

      procedure Make_Exit_Label;
      --  Build declaration for exit label to be used in Return statements

      function Process_Formals (N : Node_Id) return Traverse_Result;
      --  Replace occurrence of a formal with the corresponding actual, or
      --  the thunk generated for it.

      function Process_Sloc (Nod : Node_Id) return Traverse_Result;
      --  If the call being expanded is that of an internal subprogram,
      --  set the sloc of the generated block to that of the call itself,
      --  so that the expansion is skipped by the -next- command in gdb.
      --  Same processing for a subprogram in a predefined file, e.g.
      --  Ada.Tags. If Debug_Generated_Code is true, suppress this change
      --  to simplify our own development.

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id);
      --  If the function body is a single expression, replace call with
      --  expression, else insert block appropriately.

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id);
      --  If procedure body has no local variables, inline body without
      --  creating block,  otherwise rewrite call with block.

      function Formal_Is_Used_Once (Formal : Entity_Id) return Boolean;
      --  Determine whether a formal parameter is used only once in Orig_Bod

      ---------------------
      -- Make_Exit_Label --
      ---------------------

      procedure Make_Exit_Label is
      begin
         --  Create exit label for subprogram if one does not exist yet

         if No (Exit_Lab) then
            Lab_Id := Make_Identifier (Loc, New_Internal_Name ('L'));
            Set_Entity (Lab_Id,
              Make_Defining_Identifier (Loc, Chars (Lab_Id)));
            Exit_Lab := Make_Label (Loc, Lab_Id);

            Lab_Decl :=
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier  => Entity (Lab_Id),
                Label_Construct      => Exit_Lab);
         end if;
      end Make_Exit_Label;

      ---------------------
      -- Process_Formals --
      ---------------------

      function Process_Formals (N : Node_Id) return Traverse_Result is
         A   : Entity_Id;
         E   : Entity_Id;
         Ret : Node_Id;

      begin
         if Is_Entity_Name (N)
           and then Present (Entity (N))
         then
            E := Entity (N);

            if Is_Formal (E)
              and then Scope (E) = Subp
            then
               A := Renamed_Object (E);

               if Is_Entity_Name (A) then
                  Rewrite (N, New_Occurrence_Of (Entity (A), Loc));

               elsif Nkind (A) = N_Defining_Identifier then
                  Rewrite (N, New_Occurrence_Of (A, Loc));

               else   --  numeric literal
                  Rewrite (N, New_Copy (A));
               end if;
            end if;

            return Skip;

         elsif Nkind (N) = N_Return_Statement then

            if No (Expression (N)) then
               Make_Exit_Label;
               Rewrite (N, Make_Goto_Statement (Loc,
                 Name => New_Copy (Lab_Id)));

            else
               if Nkind (Parent (N)) = N_Handled_Sequence_Of_Statements
                 and then Nkind (Parent (Parent (N))) = N_Subprogram_Body
               then
                  --  Function body is a single expression. No need for
                  --  exit label.

                  null;

               else
                  Num_Ret := Num_Ret + 1;
                  Make_Exit_Label;
               end if;

               --  Because of the presence of private types, the views of the
               --  expression and the context may be different, so place an
               --  unchecked conversion to the context type to avoid spurious
               --  errors, eg. when the expression is a numeric literal and
               --  the context is private. If the expression is an aggregate,
               --  use a qualified expression, because an aggregate is not a
               --  legal argument of a conversion.

               if Nkind (Expression (N)) = N_Aggregate
                 or else Nkind (Expression (N)) = N_Null
               then
                  Ret :=
                    Make_Qualified_Expression (Sloc (N),
                       Subtype_Mark => New_Occurrence_Of (Ret_Type, Sloc (N)),
                       Expression => Relocate_Node (Expression (N)));
               else
                  Ret :=
                    Unchecked_Convert_To
                      (Ret_Type, Relocate_Node (Expression (N)));
               end if;

               if Nkind (Targ) = N_Defining_Identifier then
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name => New_Occurrence_Of (Targ, Loc),
                      Expression => Ret));
               else
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name => New_Copy (Targ),
                      Expression => Ret));
               end if;

               Set_Assignment_OK (Name (N));

               if Present (Exit_Lab) then
                  Insert_After (N,
                    Make_Goto_Statement (Loc,
                      Name => New_Copy (Lab_Id)));
               end if;
            end if;

            return OK;

         --  Remove pragma Unreferenced since it may refer to formals that
         --  are not visible in the inlined body, and in any case we will
         --  not be posting warnings on the inlined body so it is unneeded.

         elsif Nkind (N) = N_Pragma
           and then Chars (N) = Name_Unreferenced
         then
            Rewrite (N, Make_Null_Statement (Sloc (N)));
            return OK;

         else
            return OK;
         end if;
      end Process_Formals;

      procedure Replace_Formals is new Traverse_Proc (Process_Formals);

      ------------------
      -- Process_Sloc --
      ------------------

      function Process_Sloc (Nod : Node_Id) return Traverse_Result is
      begin
         if not Debug_Generated_Code then
            Set_Sloc (Nod, Sloc (N));
            Set_Comes_From_Source (Nod, False);
         end if;

         return OK;
      end Process_Sloc;

      procedure Reset_Slocs is new Traverse_Proc (Process_Sloc);

      ---------------------------
      -- Rewrite_Function_Call --
      ---------------------------

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id) is
         HSS : constant Node_Id := Handled_Statement_Sequence (Blk);
         Fst : constant Node_Id := First (Statements (HSS));

      begin
         --  Optimize simple case: function body is a single return statement,
         --  which has been expanded into an assignment.

         if Is_Empty_List (Declarations (Blk))
           and then Nkind (Fst) = N_Assignment_Statement
           and then No (Next (Fst))
         then

            --  The function call may have been rewritten as the temporary
            --  that holds the result of the call, in which case remove the
            --  now useless declaration.

            if Nkind (N) = N_Identifier
              and then Nkind (Parent (Entity (N))) = N_Object_Declaration
            then
               Rewrite (Parent (Entity (N)), Make_Null_Statement (Loc));
            end if;

            Rewrite (N, Expression (Fst));

         elsif Nkind (N) = N_Identifier
           and then Nkind (Parent (Entity (N))) = N_Object_Declaration
         then
            --  The block assigns the result of the call to the temporary

            Insert_After (Parent (Entity (N)), Blk);

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Entity_Name (Name (Parent (N)))
         then
            --  Replace assignment with the block

            declare
               Original_Assignment : constant Node_Id := Parent (N);

            begin
               --  Preserve the original assignment node to keep the
               --  complete assignment subtree consistent enough for
               --  Analyze_Assignment to proceed (specifically, the
               --  original Lhs node must still have an assignment
               --  statement as its parent).

               --  We cannot rely on Original_Node to go back from the
               --  block node to the assignment node, because the
               --  assignment might already be a rewrite substitution.

               Discard_Node (Relocate_Node (Original_Assignment));
               Rewrite (Original_Assignment, Blk);
            end;

         elsif Nkind (Parent (N)) = N_Object_Declaration then
            Set_Expression (Parent (N), Empty);
            Insert_After (Parent (N), Blk);
         end if;
      end Rewrite_Function_Call;

      ----------------------------
      -- Rewrite_Procedure_Call --
      ----------------------------

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id) is
         HSS  : constant Node_Id := Handled_Statement_Sequence (Blk);
      begin
         if Is_Empty_List (Declarations (Blk)) then
            Insert_List_After (N, Statements (HSS));
            Rewrite (N, Make_Null_Statement (Loc));
         else
            Rewrite (N, Blk);
         end if;
      end Rewrite_Procedure_Call;

      -------------------------
      -- Formal_Is_Used_Once --
      ------------------------

      function Formal_Is_Used_Once (Formal : Entity_Id) return Boolean is
         Use_Counter : Int := 0;

         function Count_Uses (N : Node_Id) return Traverse_Result;
         --  Traverse the tree and count the uses of the formal parameter.
         --  In this case, for optimization purposes, we do not need to
         --  continue the traversal once more than one use is encountered.

         ----------------
         -- Count_Uses --
         ----------------

         function Count_Uses (N : Node_Id) return Traverse_Result is
         begin
            --  The original node is an identifier

            if Nkind (N) = N_Identifier
              and then Present (Entity (N))

               --  The original node's entity points to the one in the
               --  copied body.

              and then Nkind (Entity (N)) = N_Identifier
              and then Present (Entity (Entity (N)))

               --  The entity of the copied node is the formal parameter

              and then Entity (Entity (N)) = Formal
            then
               Use_Counter := Use_Counter + 1;

               if Use_Counter > 1 then

                  --  Denote more than one use and abandon the traversal

                  Use_Counter := 2;
                  return Abandon;

               end if;
            end if;

            return OK;
         end Count_Uses;

         procedure Count_Formal_Uses is new Traverse_Proc (Count_Uses);

      --  Start of processing for Formal_Is_Used_Once

      begin
         Count_Formal_Uses (Orig_Bod);
         return Use_Counter = 1;
      end Formal_Is_Used_Once;

   --  Start of processing for Expand_Inlined_Call

   begin
      --  Check for special case of To_Address call, and if so, just
      --  do an unchecked conversion instead of expanding the call.
      --  Not only is this more efficient, but it also avoids a
      --  problem with order of elaboration when address clauses
      --  are inlined (address expr elaborated at wrong point).

      if Subp = RTE (RE_To_Address) then
         Rewrite (N,
           Unchecked_Convert_To
            (RTE (RE_Address),
             Relocate_Node (First_Actual (N))));
         return;
      end if;

      if Nkind (Orig_Bod) = N_Defining_Identifier then

         --  Subprogram is a renaming_as_body. Calls appearing after the
         --  renaming can be replaced with calls to the renamed entity
         --  directly, because the subprograms are subtype conformant.

         Set_Name (N, New_Occurrence_Of (Orig_Bod, Loc));
         return;
      end if;

      --  Use generic machinery to copy body of inlined subprogram, as if it
      --  were an instantiation, resetting source locations appropriately, so
      --  that nested inlined calls appear in the main unit.

      Save_Env (Subp, Empty);
      Set_Copied_Sloc_For_Inlined_Body (N, Defining_Entity (Orig_Bod));

      Bod := Copy_Generic_Node (Orig_Bod, Empty, Instantiating => True);
      Blk :=
        Make_Block_Statement (Loc,
          Declarations => Declarations (Bod),
          Handled_Statement_Sequence => Handled_Statement_Sequence (Bod));

      if No (Declarations (Bod)) then
         Set_Declarations (Blk, New_List);
      end if;

      --  If this is a derived function, establish the proper return type

      if Present (Orig_Subp)
        and then Orig_Subp /= Subp
      then
         Ret_Type := Etype (Orig_Subp);
      else
         Ret_Type := Etype (Subp);
      end if;

      F := First_Formal (Subp);
      A := First_Actual (N);

      --  Create temporaries for the actuals that are expressions, or that
      --  are scalars and require copying to preserve semantics.

      while Present (F) loop
         if Present (Renamed_Object (F)) then
            Error_Msg_N (" cannot inline call to recursive subprogram", N);
            return;
         end if;

         --  If the argument may be a controlling argument in a call within
         --  the inlined body, we must preserve its classwide nature to
         --  insure that dynamic dispatching take place subsequently.
         --  If the formal has a constraint it must be preserved to retain
         --  the semantics of the body.

         if Is_Class_Wide_Type (Etype (F))
           or else (Is_Access_Type (Etype (F))
                      and then
                    Is_Class_Wide_Type (Designated_Type (Etype (F))))
         then
            Temp_Typ := Etype (F);

         elsif Base_Type (Etype (F)) = Base_Type (Etype (A))
           and then Etype (F) /= Base_Type (Etype (F))
         then
            Temp_Typ := Etype (F);

         else
            Temp_Typ := Etype (A);
         end if;

         --  If the actual is a simple name or a literal, no need to
         --  create a temporary, object can be used directly.

         if (Is_Entity_Name (A)
              and then
               (not Is_Scalar_Type (Etype (A))
                 or else Ekind (Entity (A)) = E_Enumeration_Literal))

         --  When the actual is an identifier and the corresponding formal
         --  is used only once in the original body, the formal can be
         --  substituted directly with the actual parameter.

           or else (Nkind (A) = N_Identifier
             and then Formal_Is_Used_Once (F))

           or else Nkind (A) = N_Real_Literal
           or else Nkind (A) = N_Integer_Literal
           or else Nkind (A) = N_Character_Literal
         then
            if Etype (F) /= Etype (A) then
               Set_Renamed_Object
                (F, Unchecked_Convert_To (Etype (F), Relocate_Node (A)));
            else
               Set_Renamed_Object (F, A);
            end if;

         else
            Temp :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('C'));

            --  If the actual for an in/in-out parameter is a view conversion,
            --  make it into an unchecked conversion, given that an untagged
            --  type conversion is not a proper object for a renaming.

            --  In-out conversions that involve real conversions have already
            --  been transformed in Expand_Actuals.

            if Nkind (A) = N_Type_Conversion
              and then Ekind (F) /= E_In_Parameter
            then
               New_A := Make_Unchecked_Type_Conversion (Loc,
                 Subtype_Mark => New_Occurrence_Of (Etype (F), Loc),
                 Expression   => Relocate_Node (Expression (A)));

            elsif Etype (F) /= Etype (A) then
               New_A := Unchecked_Convert_To (Etype (F), Relocate_Node (A));
               Temp_Typ := Etype (F);

            else
               New_A := Relocate_Node (A);
            end if;

            Set_Sloc (New_A, Sloc (N));

            if Ekind (F) = E_In_Parameter
              and then not Is_Limited_Type (Etype (A))
            then
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present => True,
                   Object_Definition => New_Occurrence_Of (Temp_Typ, Loc),
                   Expression => New_A);
            else
               Decl :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Subtype_Mark        => New_Occurrence_Of (Temp_Typ, Loc),
                   Name                => New_A);
            end if;

            Prepend (Decl, Declarations (Blk));
            Set_Renamed_Object (F, Temp);
         end if;

         Next_Formal (F);
         Next_Actual (A);
      end loop;

      --  Establish target of function call. If context is not assignment or
      --  declaration, create a temporary as a target. The declaration for
      --  the temporary may be subsequently optimized away if the body is a
      --  single expression, or if the left-hand side of the assignment is
      --  simple enough.

      if Ekind (Subp) = E_Function then
         if Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Entity_Name (Name (Parent (N)))
         then
            Targ := Name (Parent (N));

         else
            --  Replace call with temporary and create its declaration

            Temp :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('C'));

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp,
                Object_Definition =>
                  New_Occurrence_Of (Ret_Type, Loc));

            Set_No_Initialization (Decl);
            Insert_Action (N, Decl);
            Rewrite (N, New_Occurrence_Of (Temp, Loc));
            Targ := Temp;
         end if;
      end if;

      --  Traverse the tree and replace formals with actuals or their thunks.
      --  Attach block to tree before analysis and rewriting.

      Replace_Formals (Blk);
      Set_Parent (Blk, N);

      if not Comes_From_Source (Subp)
        or else Is_Predef
      then
         Reset_Slocs (Blk);
      end if;

      if Present (Exit_Lab) then

         --  If the body was a single expression, the single return statement
         --  and the corresponding label are useless.

         if Num_Ret = 1
           and then
             Nkind (Last (Statements (Handled_Statement_Sequence (Blk)))) =
               N_Goto_Statement
         then
            Remove (Last (Statements (Handled_Statement_Sequence (Blk))));
         else
            Append (Lab_Decl, (Declarations (Blk)));
            Append (Exit_Lab, Statements (Handled_Statement_Sequence (Blk)));
         end if;
      end if;

      --  Analyze Blk with In_Inlined_Body set, to avoid spurious errors on
      --  conflicting private views that Gigi would ignore. If this is a
      --  predefined unit, analyze with checks off, as is done in the non-
      --  inlined run-time units.

      declare
         I_Flag : constant Boolean := In_Inlined_Body;

      begin
         In_Inlined_Body := True;

         if Is_Predef then
            declare
               Style : constant Boolean := Style_Check;
            begin
               Style_Check := False;
               Analyze (Blk, Suppress => All_Checks);
               Style_Check := Style;
            end;

         else
            Analyze (Blk);
         end if;

         In_Inlined_Body := I_Flag;
      end;

      if Ekind (Subp) = E_Procedure then
         Rewrite_Procedure_Call (N, Blk);
      else
         Rewrite_Function_Call (N, Blk);
      end if;

      Restore_Env;

      --  Cleanup mapping between formals and actuals for other expansions

      F := First_Formal (Subp);

      while Present (F) loop
         Set_Renamed_Object (F, Empty);
         Next_Formal (F);
      end loop;
   end Expand_Inlined_Call;

   ----------------------------
   -- Expand_N_Function_Call --
   ----------------------------

   procedure Expand_N_Function_Call (N : Node_Id) is
      Typ   : constant Entity_Id := Etype (N);

      function Returned_By_Reference return Boolean;
      --  If the return type is returned through the secondary stack. that is
      --  by reference, we don't want to create a temp to force stack checking.
      --  Shouldn't this function be moved to exp_util???

      function Rhs_Of_Assign_Or_Decl (N : Node_Id) return Boolean;
      --  If the call is the right side of an assignment or the expression in
      --  an object declaration, we don't need to create a temp as the left
      --  side will already trigger stack checking if necessary.

      ---------------------------
      -- Returned_By_Reference --
      ---------------------------

      function Returned_By_Reference return Boolean is
         S : Entity_Id := Current_Scope;

      begin
         if Is_Return_By_Reference_Type (Typ) then
            return True;

         elsif Nkind (Parent (N)) /= N_Return_Statement then
            return False;

         elsif Requires_Transient_Scope (Typ) then

            --  Verify that the return type of the enclosing function has
            --  the same constrained status as that of the expression.

            while Ekind (S) /= E_Function loop
               S := Scope (S);
            end loop;

            return Is_Constrained (Typ) = Is_Constrained (Etype (S));
         else
            return False;
         end if;
      end Returned_By_Reference;

      ---------------------------
      -- Rhs_Of_Assign_Or_Decl --
      ---------------------------

      function Rhs_Of_Assign_Or_Decl (N : Node_Id) return Boolean is
      begin
         if (Nkind (Parent (N)) = N_Assignment_Statement
               and then Expression (Parent (N)) = N)
           or else
             (Nkind (Parent (N)) = N_Qualified_Expression
                and then Nkind (Parent (Parent (N))) = N_Assignment_Statement
                  and then Expression (Parent (Parent (N))) = Parent (N))
           or else
             (Nkind (Parent (N)) = N_Object_Declaration
                and then Expression (Parent (N)) = N)
           or else
             (Nkind (Parent (N)) = N_Component_Association
                and then Expression (Parent (N)) = N
                  and then Nkind (Parent (Parent (N))) = N_Aggregate
                    and then Rhs_Of_Assign_Or_Decl (Parent (Parent (N))))
         then
            return True;
         else
            return False;
         end if;
      end Rhs_Of_Assign_Or_Decl;

   --  Start of processing for Expand_N_Function_Call

   begin
      --  A special check. If stack checking is enabled, and the return type
      --  might generate a large temporary, and the call is not the right
      --  side of an assignment, then generate an explicit temporary. We do
      --  this because otherwise gigi may generate a large temporary on the
      --  fly and this can cause trouble with stack checking.

      --  This is unecessary if the call is the expression in an object
      --  declaration, or if it appears outside of any library unit. This
      --  can only happen if it appears as an actual in a library-level
      --  instance, in which case a temporary will be generated for it once
      --  the instance itself is installed.

      if May_Generate_Large_Temp (Typ)
        and then not Rhs_Of_Assign_Or_Decl (N)
        and then not Returned_By_Reference
        and then Current_Scope /= Standard_Standard
      then
         if Stack_Checking_Enabled then

            --  Note: it might be thought that it would be OK to use a call
            --  to Force_Evaluation here, but that's not good enough, because
            --  that can results in a 'Reference construct that may still
            --  need a temporary.

            declare
               Loc      : constant Source_Ptr := Sloc (N);
               Temp_Obj : constant Entity_Id :=
                            Make_Defining_Identifier (Loc,
                              Chars => New_Internal_Name ('F'));
               Temp_Typ : Entity_Id := Typ;
               Decl     : Node_Id;
               A        : Node_Id;
               F        : Entity_Id;
               Proc     : Entity_Id;

            begin
               if Is_Tagged_Type (Typ)
                 and then Present (Controlling_Argument (N))
               then
                  if Nkind (Parent (N)) /= N_Procedure_Call_Statement
                    and then Nkind (Parent (N)) /= N_Function_Call
                  then
                     --  If this is a tag-indeterminate call, the object must
                     --  be classwide.

                     if Is_Tag_Indeterminate (N) then
                        Temp_Typ := Class_Wide_Type (Typ);
                     end if;

                  else
                     --  If this is a dispatching call that is itself the
                     --  controlling argument of an enclosing call, the
                     --  nominal subtype of the object that replaces it must
                     --  be classwide, so that dispatching will take place
                     --  properly. If it is not a controlling argument, the
                     --  object is not classwide.

                     Proc := Entity (Name (Parent (N)));
                     F    := First_Formal (Proc);
                     A    := First_Actual (Parent (N));

                     while A /= N loop
                        Next_Formal (F);
                        Next_Actual (A);
                     end loop;

                     if Is_Controlling_Formal (F) then
                        Temp_Typ := Class_Wide_Type (Typ);
                     end if;
                  end if;
               end if;

               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp_Obj,
                   Object_Definition   => New_Occurrence_Of (Temp_Typ, Loc),
                   Constant_Present    => True,
                   Expression          => Relocate_Node (N));
               Set_Assignment_OK (Decl);

               Insert_Actions (N, New_List (Decl));
               Rewrite (N, New_Occurrence_Of (Temp_Obj, Loc));
            end;

         else
            --  If stack-checking is not enabled, increment serial number
            --  for internal names, so that subsequent symbols are consistent
            --  with and without stack-checking.

            Synchronize_Serial_Number;

            --  Now we can expand the call with consistent symbol names

            Expand_Call (N);
         end if;

      --  Normal case, expand the call

      else
         Expand_Call (N);
      end if;
   end Expand_N_Function_Call;

   ---------------------------------------
   -- Expand_N_Procedure_Call_Statement --
   ---------------------------------------

   procedure Expand_N_Procedure_Call_Statement (N : Node_Id) is
   begin
      Expand_Call (N);
   end Expand_N_Procedure_Call_Statement;

   ------------------------------
   -- Expand_N_Subprogram_Body --
   ------------------------------

   --  Add poll call if ATC polling is enabled, unless the body will be
   --  inlined by the back-end.

   --  Add return statement if last statement in body is not a return
   --  statement (this makes things easier on Gigi which does not want
   --  to have to handle a missing return).

   --  Add call to Activate_Tasks if body is a task activator

   --  Deal with possible detection of infinite recursion

   --  Eliminate body completely if convention stubbed

   --  Encode entity names within body, since we will not need to reference
   --  these entities any longer in the front end.

   --  Initialize scalar out parameters if Initialize/Normalize_Scalars

   --  Reset Pure indication if any parameter has root type System.Address

   --  Wrap thread body

   procedure Expand_N_Subprogram_Body (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      H        : constant Node_Id    := Handled_Statement_Sequence (N);
      Body_Id  : Entity_Id;
      Spec_Id  : Entity_Id;
      Except_H : Node_Id;
      Scop     : Entity_Id;
      Dec      : Node_Id;
      Next_Op  : Node_Id;
      L        : List_Id;

      procedure Add_Return (S : List_Id);
      --  Append a return statement to the statement sequence S if the last
      --  statement is not already a return or a goto statement. Note that
      --  the latter test is not critical, it does not matter if we add a
      --  few extra returns, since they get eliminated anyway later on.

      procedure Expand_Thread_Body;
      --  Perform required expansion of a thread body

      ----------------
      -- Add_Return --
      ----------------

      procedure Add_Return (S : List_Id) is
      begin
         if not Is_Transfer (Last (S)) then

            --  The source location for the return is the end label
            --  of the procedure in all cases. This is a bit odd when
            --  there are exception handlers, but not much else we can do.

            Append_To (S, Make_Return_Statement (Sloc (End_Label (H))));
         end if;
      end Add_Return;

      ------------------------
      -- Expand_Thread_Body --
      ------------------------

      --  The required expansion of a thread body is as follows

      --  procedure <thread body procedure name> is

      --    _Secondary_Stack : aliased
      --       Storage_Elements.Storage_Array
      --         (1 .. Storage_Offset (Sec_Stack_Size));
      --    for _Secondary_Stack'Alignment use Standard'Maximum_Alignment;

      --    _Process_ATSD : aliased System.Threads.ATSD;

      --  begin
      --     System.Threads.Thread_Body_Enter;
      --       (_Secondary_Stack'Address,
      --        _Secondary_Stack'Length,
      --        _Process_ATSD'Address);

      --     declare
      --        <user declarations>
      --     begin
      --        <user statements>
      --     <user exception handlers>
      --     end;

      --    System.Threads.Thread_Body_Leave;

      --  exception
      --     when E : others =>
      --       System.Threads.Thread_Body_Exceptional_Exit (E);
      --  end;

      --  Note the exception handler is omitted if pragma Restriction
      --  No_Exception_Handlers is currently active.

      procedure Expand_Thread_Body is
         User_Decls    : constant List_Id := Declarations (N);
         Sec_Stack_Len : Node_Id;

         TB_Pragma  : constant Node_Id :=
                        Get_Rep_Pragma (Spec_Id, Name_Thread_Body);

         Ent_SS   : Entity_Id;
         Ent_ATSD : Entity_Id;
         Ent_EO   : Entity_Id;

         Decl_SS   : Node_Id;
         Decl_ATSD : Node_Id;

         Excep_Handlers : List_Id;

      begin
         New_Scope (Spec_Id);

         --  Get proper setting for secondary stack size

         if List_Length (Pragma_Argument_Associations (TB_Pragma)) = 2 then
            Sec_Stack_Len :=
              Expression (Last (Pragma_Argument_Associations (TB_Pragma)));
         else
            Sec_Stack_Len :=
              New_Occurrence_Of (RTE (RE_Default_Secondary_Stack_Size), Loc);
         end if;

         Sec_Stack_Len := Convert_To (RTE (RE_Storage_Offset), Sec_Stack_Len);

         --  Build and set declarations for the wrapped thread body

         Ent_SS   := Make_Defining_Identifier (Loc, Name_uSecondary_Stack);
         Ent_ATSD := Make_Defining_Identifier (Loc, Name_uProcess_ATSD);

         Decl_SS :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent_SS,
             Aliased_Present     => True,
             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Storage_Array), Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => New_List (
                       Make_Range (Loc,
                         Low_Bound  => Make_Integer_Literal (Loc, 1),
                         High_Bound => Sec_Stack_Len)))));

         Decl_ATSD :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent_ATSD,
             Aliased_Present     => True,
             Object_Definition   => New_Occurrence_Of (RTE (RE_ATSD), Loc));

         Set_Declarations (N, New_List (Decl_SS, Decl_ATSD));
         Analyze (Decl_SS);
         Analyze (Decl_ATSD);
         Set_Alignment (Ent_SS, UI_From_Int (Maximum_Alignment));

         --  Create new exception handler

         if Restriction_Active (No_Exception_Handlers) then
            Excep_Handlers := No_List;

         else
            Check_Restriction (No_Exception_Handlers, N);

            Ent_EO := Make_Defining_Identifier (Loc, Name_uE);

            Excep_Handlers := New_List (
              Make_Exception_Handler (Loc,
                Choice_Parameter => Ent_EO,
                Exception_Choices => New_List (
                  Make_Others_Choice (Loc)),
                Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name =>
                      New_Occurrence_Of
                        (RTE (RE_Thread_Body_Exceptional_Exit), Loc),
                    Parameter_Associations => New_List (
                      New_Occurrence_Of (Ent_EO, Loc))))));
         end if;

         --  Now build new handled statement sequence and analyze it

         Set_Handled_Statement_Sequence (N,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List (

               Make_Procedure_Call_Statement (Loc,
                 Name => New_Occurrence_Of (RTE (RE_Thread_Body_Enter), Loc),
                 Parameter_Associations => New_List (

                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ent_SS, Loc),
                     Attribute_Name => Name_Address),

                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ent_SS, Loc),
                     Attribute_Name => Name_Length),

                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ent_ATSD, Loc),
                     Attribute_Name => Name_Address))),

               Make_Block_Statement (Loc,
                 Declarations => User_Decls,
                 Handled_Statement_Sequence => H),

               Make_Procedure_Call_Statement (Loc,
                 Name => New_Occurrence_Of (RTE (RE_Thread_Body_Leave), Loc))),

             Exception_Handlers => Excep_Handlers));

         Analyze (Handled_Statement_Sequence (N));
         End_Scope;
      end Expand_Thread_Body;

   --  Start of processing for Expand_N_Subprogram_Body

   begin
      --  Set L to either the list of declarations if present, or
      --  to the list of statements if no declarations are present.
      --  This is used to insert new stuff at the start.

      if Is_Non_Empty_List (Declarations (N)) then
         L := Declarations (N);
      else
         L := Statements (Handled_Statement_Sequence (N));
      end if;

      --  Find entity for subprogram

      Body_Id := Defining_Entity (N);

      if Present (Corresponding_Spec (N)) then
         Spec_Id := Corresponding_Spec (N);
      else
         Spec_Id := Body_Id;
      end if;

      --  Need poll on entry to subprogram if polling enabled. We only
      --  do this for non-empty subprograms, since it does not seem
      --  necessary to poll for a dummy null subprogram. Do not add polling
      --  point if calls to this subprogram will be inlined by the back-end,
      --  to avoid repeated polling points in nested inlinings.

      if Is_Non_Empty_List (L) then
         if Is_Inlined (Spec_Id)
           and then Front_End_Inlining
           and then Optimization_Level > 1
         then
            null;
         else
            Generate_Poll_Call (First (L));
         end if;
      end if;

      --  If this is a Pure function which has any parameters whose root
      --  type is System.Address, reset the Pure indication, since it will
      --  likely cause incorrect code to be generated as the parameter is
      --  probably a pointer, and the fact that the same pointer is passed
      --  does not mean that the same value is being referenced.

      --  Note that if the programmer gave an explicit Pure_Function pragma,
      --  then we believe the programmer, and leave the subprogram Pure.

      --  This code should probably be at the freeze point, so that it
      --  happens even on a -gnatc (or more importantly -gnatt) compile
      --  so that the semantic tree has Is_Pure set properly ???

      if Is_Pure (Spec_Id)
        and then Is_Subprogram (Spec_Id)
        and then not Has_Pragma_Pure_Function (Spec_Id)
      then
         declare
            F : Entity_Id := First_Formal (Spec_Id);

         begin
            while Present (F) loop
               if Is_Descendent_Of_Address (Etype (F)) then
                  Set_Is_Pure (Spec_Id, False);

                  if Spec_Id /= Body_Id then
                     Set_Is_Pure (Body_Id, False);
                  end if;

                  exit;
               end if;

               Next_Formal (F);
            end loop;
         end;
      end if;

      --  Initialize any scalar OUT args if Initialize/Normalize_Scalars

      if Init_Or_Norm_Scalars and then Is_Subprogram (Spec_Id) then
         declare
            F : Entity_Id        := First_Formal (Spec_Id);
            V : constant Boolean := Validity_Checks_On;

         begin
            --  We turn off validity checking, since we do not want any
            --  check on the initializing value itself (which we know
            --  may well be invalid!)

            Validity_Checks_On := False;

            --  Loop through formals

            while Present (F) loop
               if Is_Scalar_Type (Etype (F))
                 and then Ekind (F) = E_Out_Parameter
               then
                  Insert_Before_And_Analyze (First (L),
                    Make_Assignment_Statement (Loc,
                      Name => New_Occurrence_Of (F, Loc),
                      Expression => Get_Simple_Init_Val (Etype (F), Loc)));
               end if;

               Next_Formal (F);
            end loop;

            Validity_Checks_On := V;
         end;
      end if;

      Scop := Scope (Spec_Id);

      --  Add discriminal renamings to protected subprograms.
      --  Install new discriminals for expansion of the next
      --  subprogram of this protected type, if any.

      if Is_List_Member (N)
        and then Present (Parent (List_Containing (N)))
        and then Nkind (Parent (List_Containing (N))) = N_Protected_Body
      then
         Add_Discriminal_Declarations
           (Declarations (N), Scop, Name_uObject, Loc);
         Add_Private_Declarations (Declarations (N), Scop, Name_uObject, Loc);

         --  Associate privals and discriminals with the next protected
         --  operation body to be expanded. These are used to expand
         --  references to private data objects and discriminants,
         --  respectively.

         Next_Op := Next_Protected_Operation (N);

         if Present (Next_Op) then
            Dec := Parent (Base_Type (Scop));
            Set_Privals (Dec, Next_Op, Loc);
            Set_Discriminals (Dec);
         end if;
      end if;

      --  Clear out statement list for stubbed procedure

      if Present (Corresponding_Spec (N)) then
         Set_Elaboration_Flag (N, Spec_Id);

         if Convention (Spec_Id) = Convention_Stubbed
           or else Is_Eliminated (Spec_Id)
         then
            Set_Declarations (N, Empty_List);
            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Null_Statement (Loc))));
            return;
         end if;
      end if;

      --  Returns_By_Ref flag is normally set when the subprogram is frozen
      --  but subprograms with no specs are not frozen

      declare
         Typ  : constant Entity_Id := Etype (Spec_Id);
         Utyp : constant Entity_Id := Underlying_Type (Typ);

      begin
         if not Acts_As_Spec (N)
           and then Nkind (Parent (Parent (Spec_Id))) /=
             N_Subprogram_Body_Stub
         then
            null;

         elsif Is_Return_By_Reference_Type (Typ) then
            Set_Returns_By_Ref (Spec_Id);

         elsif Present (Utyp) and then Controlled_Type (Utyp) then
            Set_Returns_By_Ref (Spec_Id);
         end if;
      end;

      --  For a procedure, we add a return for all possible syntactic ends
      --  of the subprogram. Note that reanalysis is not necessary in this
      --  case since it would require a lot of work and accomplish nothing.

      if Ekind (Spec_Id) = E_Procedure
        or else Ekind (Spec_Id) = E_Generic_Procedure
      then
         Add_Return (Statements (H));

         if Present (Exception_Handlers (H)) then
            Except_H := First_Non_Pragma (Exception_Handlers (H));

            while Present (Except_H) loop
               Add_Return (Statements (Except_H));
               Next_Non_Pragma (Except_H);
            end loop;
         end if;

      --  For a function, we must deal with the case where there is at least
      --  one missing return. What we do is to wrap the entire body of the
      --  function in a block:

      --    begin
      --      ...
      --    end;

      --  becomes

      --    begin
      --       begin
      --          ...
      --       end;

      --       raise Program_Error;
      --    end;

      --  This approach is necessary because the raise must be signalled
      --  to the caller, not handled by any local handler (RM 6.4(11)).

      --  Note: we do not need to analyze the constructed sequence here,
      --  since it has no handler, and an attempt to analyze the handled
      --  statement sequence twice is risky in various ways (e.g. the
      --  issue of expanding cleanup actions twice).

      elsif Has_Missing_Return (Spec_Id) then
         declare
            Hloc : constant Source_Ptr := Sloc (H);
            Blok : constant Node_Id    :=
                     Make_Block_Statement (Hloc,
                       Handled_Statement_Sequence => H);
            Rais : constant Node_Id    :=
                     Make_Raise_Program_Error (Hloc,
                       Reason => PE_Missing_Return);

         begin
            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Hloc,
                Statements => New_List (Blok, Rais)));

            New_Scope (Spec_Id);
            Analyze (Blok);
            Analyze (Rais);
            Pop_Scope;
         end;
      end if;

      --  If subprogram contains a parameterless recursive call, then we may
      --  have an infinite recursion, so see if we can generate code to check
      --  for this possibility if storage checks are not suppressed.

      if Ekind (Spec_Id) = E_Procedure
        and then Has_Recursive_Call (Spec_Id)
        and then not Storage_Checks_Suppressed (Spec_Id)
      then
         Detect_Infinite_Recursion (N, Spec_Id);
      end if;

      --  Finally, if we are in Normalize_Scalars mode, then any scalar out
      --  parameters must be initialized to the appropriate default value.

      if Ekind (Spec_Id) = E_Procedure and then Normalize_Scalars then
         declare
            Floc   : Source_Ptr;
            Formal : Entity_Id;
            Stm    : Node_Id;

         begin
            Formal := First_Formal (Spec_Id);

            while Present (Formal) loop
               Floc := Sloc (Formal);

               if Ekind (Formal) = E_Out_Parameter
                 and then Is_Scalar_Type (Etype (Formal))
               then
                  Stm :=
                    Make_Assignment_Statement (Floc,
                      Name => New_Occurrence_Of (Formal, Floc),
                      Expression =>
                        Get_Simple_Init_Val (Etype (Formal), Floc));
                  Prepend (Stm, Declarations (N));
                  Analyze (Stm);
               end if;

               Next_Formal (Formal);
            end loop;
         end;
      end if;

      --  Deal with thread body

      if Is_Thread_Body (Spec_Id) then
         Expand_Thread_Body;
      end if;

      --  If the subprogram does not have pending instantiations, then we
      --  must generate the subprogram descriptor now, since the code for
      --  the subprogram is complete, and this is our last chance. However
      --  if there are pending instantiations, then the code is not
      --  complete, and we will delay the generation.

      if Is_Subprogram (Spec_Id)
        and then not Delay_Subprogram_Descriptors (Spec_Id)
      then
         Generate_Subprogram_Descriptor_For_Subprogram (N, Spec_Id);
      end if;

      --  Set to encode entity names in package body before gigi is called

      Qualify_Entity_Names (N);
   end Expand_N_Subprogram_Body;

   -----------------------------------
   -- Expand_N_Subprogram_Body_Stub --
   -----------------------------------

   procedure Expand_N_Subprogram_Body_Stub (N : Node_Id) is
   begin
      if Present (Corresponding_Body (N)) then
         Expand_N_Subprogram_Body (
           Unit_Declaration_Node (Corresponding_Body (N)));
      end if;
   end Expand_N_Subprogram_Body_Stub;

   -------------------------------------
   -- Expand_N_Subprogram_Declaration --
   -------------------------------------

   --  If the declaration appears within a protected body, it is a private
   --  operation of the protected type. We must create the corresponding
   --  protected subprogram an associated formals. For a normal protected
   --  operation, this is done when expanding the protected type declaration.

   procedure Expand_N_Subprogram_Declaration (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Subp      : constant Entity_Id  := Defining_Entity (N);
      Scop      : constant Entity_Id  := Scope (Subp);
      Prot_Decl : Node_Id;
      Prot_Bod  : Node_Id;
      Prot_Id   : Entity_Id;

   begin
      --  Deal with case of protected subprogram. Do not generate
      --  protected operation if operation is flagged as eliminated.

      if Is_List_Member (N)
        and then Present (Parent (List_Containing (N)))
        and then Nkind (Parent (List_Containing (N))) = N_Protected_Body
        and then Is_Protected_Type (Scop)
      then
         if No (Protected_Body_Subprogram (Subp))
           and then not Is_Eliminated (Subp)
         then
            Prot_Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Sub_Specification
                    (N, Scop, Unprotected => True));

            --  The protected subprogram is declared outside of the protected
            --  body. Given that the body has frozen all entities so far, we
            --  analyze the subprogram and perform freezing actions explicitly.
            --  If the body is a subunit, the insertion point is before the
            --  stub in the parent.

            Prot_Bod := Parent (List_Containing (N));

            if Nkind (Parent (Prot_Bod)) = N_Subunit then
               Prot_Bod := Corresponding_Stub (Parent (Prot_Bod));
            end if;

            Insert_Before (Prot_Bod, Prot_Decl);
            Prot_Id := Defining_Unit_Name (Specification (Prot_Decl));

            New_Scope (Scope (Scop));
            Analyze (Prot_Decl);
            Create_Extra_Formals (Prot_Id);
            Set_Protected_Body_Subprogram (Subp, Prot_Id);
            Pop_Scope;
         end if;
      end if;
   end Expand_N_Subprogram_Declaration;

   ---------------------------------------
   -- Expand_Protected_Object_Reference --
   ---------------------------------------

   function Expand_Protected_Object_Reference
     (N    : Node_Id;
      Scop : Entity_Id)
     return Node_Id
   is
      Loc   : constant Source_Ptr := Sloc (N);
      Corr  : Entity_Id;
      Rec   : Node_Id;
      Param : Entity_Id;
      Proc  : Entity_Id;

   begin
      Rec := Make_Identifier (Loc, Name_uObject);
      Set_Etype (Rec, Corresponding_Record_Type (Scop));

      --  Find enclosing protected operation, and retrieve its first
      --  parameter, which denotes the enclosing protected object.
      --  If the enclosing operation is an entry, we are immediately
      --  within the protected body, and we can retrieve the object
      --  from the service entries procedure. A barrier function has
      --  has the same signature as an entry. A barrier function is
      --  compiled within the protected object, but unlike protected
      --  operations its never needs locks, so that its protected body
      --  subprogram points to itself.

      Proc := Current_Scope;

      while Present (Proc)
        and then Scope (Proc) /= Scop
      loop
         Proc := Scope (Proc);
      end loop;

      Corr := Protected_Body_Subprogram (Proc);

      if No (Corr) then

         --  Previous error left expansion incomplete.
         --  Nothing to do on this call.

         return Empty;
      end if;

      Param :=
        Defining_Identifier
          (First (Parameter_Specifications (Parent (Corr))));

      if Is_Subprogram (Proc)
        and then Proc /= Corr
      then
         --  Protected function or procedure

         Set_Entity (Rec, Param);

         --  Rec is a reference to an entity which will not be in scope
         --  when the call is reanalyzed, and needs no further analysis.

         Set_Analyzed (Rec);

      else
         --  Entry or barrier function for entry body.
         --  The first parameter of the entry body procedure is a
         --  pointer to the object. We create a local variable
         --  of the proper type, duplicating what is done to define
         --  _object later on.

         declare
            Decls : List_Id;
            Obj_Ptr : constant Entity_Id :=  Make_Defining_Identifier (Loc,
                                               Chars =>
                                                 New_Internal_Name ('T'));

         begin
            Decls := New_List (
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Obj_Ptr,
                  Type_Definition =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Reference_To
                      (Corresponding_Record_Type (Scop), Loc))));

            Insert_Actions (N, Decls);
            Insert_Actions (N, Freeze_Entity (Obj_Ptr, Sloc (N)));

            Rec :=
              Make_Explicit_Dereference (Loc,
                Unchecked_Convert_To (Obj_Ptr,
                  New_Occurrence_Of (Param, Loc)));

            --  Analyze new actual. Other actuals in calls are already
            --  analyzed and the list of actuals is not renalyzed after
            --  rewriting.

            Set_Parent (Rec, N);
            Analyze (Rec);
         end;
      end if;

      return Rec;
   end Expand_Protected_Object_Reference;

   --------------------------------------
   -- Expand_Protected_Subprogram_Call --
   --------------------------------------

   procedure Expand_Protected_Subprogram_Call
     (N    : Node_Id;
      Subp : Entity_Id;
      Scop : Entity_Id)
   is
      Rec   : Node_Id;

   begin
      --  If the protected object is not an enclosing scope, this is
      --  an inter-object function call. Inter-object procedure
      --  calls are expanded by Exp_Ch9.Build_Simple_Entry_Call.
      --  The call is intra-object only if the subprogram being
      --  called is in the protected body being compiled, and if the
      --  protected object in the call is statically the enclosing type.
      --  The object may be an component of some other data structure,
      --  in which case this must be handled as an inter-object call.

      if not In_Open_Scopes (Scop)
        or else not Is_Entity_Name (Name (N))
      then
         if Nkind (Name (N)) = N_Selected_Component then
            Rec := Prefix (Name (N));

         else
            pragma Assert (Nkind (Name (N)) = N_Indexed_Component);
            Rec := Prefix (Prefix (Name (N)));
         end if;

         Build_Protected_Subprogram_Call (N,
           Name => New_Occurrence_Of (Subp, Sloc (N)),
           Rec =>  Convert_Concurrent (Rec, Etype (Rec)),
           External => True);

      else
         Rec := Expand_Protected_Object_Reference (N, Scop);

         if No (Rec) then
            return;
         end if;

         Build_Protected_Subprogram_Call (N,
           Name     => Name (N),
           Rec      => Rec,
           External => False);

      end if;

      Analyze (N);

      --  If it is a function call it can appear in elaboration code and
      --  the called entity must be frozen here.

      if Ekind (Subp) = E_Function then
         Freeze_Expression (Name (N));
      end if;
   end Expand_Protected_Subprogram_Call;

   -----------------------
   -- Freeze_Subprogram --
   -----------------------

   procedure Freeze_Subprogram (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      --  When a primitive is frozen, enter its name in the corresponding
      --  dispatch table. If the DTC_Entity field is not set this is an
      --  overridden primitive that can be ignored. We suppress the
      --  initialization of the dispatch table entry when Java_VM because
      --  the dispatching mechanism is handled internally by the JVM.

      if Is_Dispatching_Operation (E)
        and then not Is_Abstract (E)
        and then Present (DTC_Entity (E))
        and then not Is_CPP_Class (Scope (DTC_Entity (E)))
        and then not Java_VM
      then
         Check_Overriding_Operation (E);
         Insert_After (N, Fill_DT_Entry (Sloc (N), E));
      end if;

      --  Mark functions that return by reference. Note that it cannot be
      --  part of the normal semantic analysis of the spec since the
      --  underlying returned type may not be known yet (for private types)

      declare
         Typ  : constant Entity_Id := Etype (E);
         Utyp : constant Entity_Id := Underlying_Type (Typ);

      begin
         if Is_Return_By_Reference_Type (Typ) then
            Set_Returns_By_Ref (E);

         elsif Present (Utyp) and then Controlled_Type (Utyp) then
            Set_Returns_By_Ref (E);
         end if;
      end;
   end Freeze_Subprogram;

end Exp_Ch6;
