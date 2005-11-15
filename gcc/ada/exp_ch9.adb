------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 9                               --
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
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch11; use Exp_Ch11;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Dbug; use Exp_Dbug;
with Exp_Smem; use Exp_Smem;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Hostparm;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch11; use Sem_Ch11;
with Sem_Elab; use Sem_Elab;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch9 is

   --------------------------------
   -- Select_Expansion_Utilities --
   --------------------------------

   --  The following constant establishes the upper bound for the index of
   --  an entry family. It is used to limit the allocated size of protected
   --  types with defaulted discriminant of an integer type, when the bound
   --  of some entry family depends on a discriminant. The limitation to
   --  entry families of 128K should be reasonable in all cases, and is a
   --  documented implementation restriction. It will be lifted when protected
   --  entry families are re-implemented as a single ordered queue.

   Entry_Family_Bound : constant Int := 2**16;

   --  The following package contains helper routines used in the expansion of
   --  dispatching asynchronous, conditional and timed selects.

   package Select_Expansion_Utilities is
      function Build_Abort_Block
        (Loc         : Source_Ptr;
         Abr_Blk_Ent : Entity_Id;
         Cln_Blk_Ent : Entity_Id;
         Blk         : Node_Id) return Node_Id;
      --  Generate:
      --    begin
      --       Blk
      --    exception
      --       when Abort_Signal => Abort_Undefer;
      --    end;
      --  Abr_Blk_Ent is the name of the generated block, Cln_Blk_Ent is
      --  the name of the encapsulated cleanup block, Blk is the actual
      --  block node.

      function Build_B
        (Loc   : Source_Ptr;
         Decls : List_Id) return Entity_Id;
      --  Generate:
      --    B : Boolean := False;
      --  Append the object declaration to the list and return the name of
      --  the object.

      function Build_C
        (Loc   : Source_Ptr;
         Decls : List_Id) return Entity_Id;
      --  Generate:
      --    C : Ada.Tags.Prim_Op_Kind;
      --  Append the object declaration to the list and return the name of
      --  the object.

      function Build_Cleanup_Block
        (Loc       : Source_Ptr;
         Blk_Ent   : Entity_Id;
         Stmts     : List_Id;
         Clean_Ent : Entity_Id) return Node_Id;
      --  Generate:
      --    declare
      --       procedure _clean is
      --       begin
      --          ...
      --       end _clean;
      --    begin
      --       Stmts
      --    at end
      --       _clean;
      --    end;
      --  Blk_Ent is the name of the generated block, Stmts is the list
      --  of encapsulated statements and Clean_Ent is the parameter to
      --  the _clean procedure.

      function Build_S
        (Loc      : Source_Ptr;
         Decls    : List_Id;
         Obj      : Entity_Id;
         Call_Ent : Entity_Id) return Entity_Id;
      --  Generate:
      --    S : constant Integer :=
      --          Ada.Tags.Get_Offset_Index (
      --            Unchecked_Convert_To (Ada.Tags.Interface_Tag, Obj),
      --            DT_Position (Call_Ent));
      --  where Obj is the pointer to a secondary table, Call_Ent is the
      --  entity of the dispatching call name. Append the object declaration
      --  to the list and return its defining identifier.

   end Select_Expansion_Utilities;

   -----------------------------------------
   -- Body for Select_Expansion_Utilities --
   -----------------------------------------

   package body Select_Expansion_Utilities is

      -----------------------
      -- Build_Abort_Block --
      -----------------------

      function Build_Abort_Block
        (Loc         : Source_Ptr;
         Abr_Blk_Ent : Entity_Id;
         Cln_Blk_Ent : Entity_Id;
         Blk         : Node_Id) return Node_Id
      is
      begin
         return
           Make_Block_Statement (Loc,
             Identifier   => New_Reference_To (Abr_Blk_Ent, Loc),

             Declarations => No_List,

             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements =>
                   New_List (
                     Make_Implicit_Label_Declaration (Loc,
                       Defining_Identifier =>
                         Cln_Blk_Ent,
                       Label_Construct =>
                         Blk),
                     Blk),

                 Exception_Handlers =>
                   New_List (
                     Make_Exception_Handler (Loc,
                       Exception_Choices =>
                         New_List (
                           New_Reference_To (Stand.Abort_Signal, Loc)),
                       Statements =>
                         New_List (
                           Make_Procedure_Call_Statement (Loc,
                             Name =>
                               New_Reference_To (RTE (
                                 RE_Abort_Undefer), Loc),
                             Parameter_Associations => No_List))))));
      end Build_Abort_Block;

      -------------
      -- Build_B --
      -------------

      function Build_B
        (Loc   : Source_Ptr;
         Decls : List_Id) return Entity_Id
      is
         B : constant Entity_Id := Make_Defining_Identifier (Loc,
                                     Chars => New_Internal_Name ('B'));

      begin
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               B,
             Object_Definition =>
               New_Reference_To (Standard_Boolean, Loc),
             Expression =>
               New_Reference_To (Standard_False, Loc)));

         return B;
      end Build_B;

      -------------
      -- Build_C --
      -------------

      function Build_C
        (Loc   : Source_Ptr;
         Decls : List_Id) return Entity_Id
      is
         C : constant Entity_Id := Make_Defining_Identifier (Loc,
                                     Chars => New_Internal_Name ('C'));

      begin
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               C,
             Object_Definition =>
               New_Reference_To (RTE (RE_Prim_Op_Kind), Loc)));

         return C;
      end Build_C;

      -------------------------
      -- Build_Cleanup_Block --
      -------------------------

      function Build_Cleanup_Block
        (Loc       : Source_Ptr;
         Blk_Ent   : Entity_Id;
         Stmts     : List_Id;
         Clean_Ent : Entity_Id) return Node_Id
      is
         Cleanup_Block : constant Node_Id :=
                           Make_Block_Statement (Loc,
                             Identifier   => New_Reference_To (Blk_Ent, Loc),
                             Declarations => No_List,
                             Handled_Statement_Sequence =>
                               Make_Handled_Sequence_Of_Statements (Loc,
                                 Statements => Stmts),
                             Is_Asynchronous_Call_Block => True);

      begin
         Set_Entry_Cancel_Parameter (Blk_Ent, Clean_Ent);

         return Cleanup_Block;
      end Build_Cleanup_Block;

      -------------
      -- Build_S --
      -------------

      function Build_S
        (Loc      : Source_Ptr;
         Decls    : List_Id;
         Obj      : Entity_Id;
         Call_Ent : Entity_Id) return Entity_Id
      is
         S : constant Entity_Id := Make_Defining_Identifier (Loc,
                                     Chars => New_Internal_Name ('S'));

      begin
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => S,
             Constant_Present    => True,

             Object_Definition   =>
               New_Reference_To (Standard_Integer, Loc),

             Expression          =>
               Make_Function_Call (Loc,
                 Name => New_Reference_To (RTE (RE_Get_Offset_Index), Loc),
                 Parameter_Associations => New_List (
                   Unchecked_Convert_To (RTE (RE_Interface_Tag), Obj),
                   Make_Integer_Literal (Loc, DT_Position (Call_Ent))))));

         return S;
      end Build_S;
   end Select_Expansion_Utilities;

   package SEU renames Select_Expansion_Utilities;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Actual_Index_Expression
     (Sloc  : Source_Ptr;
      Ent   : Entity_Id;
      Index : Node_Id;
      Tsk   : Entity_Id) return Node_Id;
   --  Compute the index position for an entry call. Tsk is the target
   --  task. If the bounds of some entry family depend on discriminants,
   --  the expression computed by this function uses the discriminants
   --  of the target task.

   procedure Add_Object_Pointer
     (Decls : List_Id;
      Pid   : Entity_Id;
      Loc   : Source_Ptr);
   --  Prepend an object pointer declaration to the declaration list
   --  Decls. This object pointer is initialized to a type conversion
   --  of the System.Address pointer passed to entry barrier functions
   --  and entry body procedures.

   procedure Add_Formal_Renamings
     (Spec  : Node_Id;
      Decls : List_Id;
      Ent   : Entity_Id;
      Loc   : Source_Ptr);
   --  Create renaming declarations for the formals, inside the procedure
   --  that implements an entry body. The renamings make the original names
   --  of the formals accessible to gdb, and serve no other purpose.
   --    Spec is the specification of the procedure being built.
   --    Decls is the list of declarations to be enhanced.
   --    Ent is the entity for the original entry body.

   function Build_Accept_Body (Astat : Node_Id) return Node_Id;
   --  Transform accept statement into a block with added exception handler.
   --  Used both for simple accept statements and for accept alternatives in
   --  select statements. Astat is the accept statement.

   function Build_Barrier_Function
     (N    : Node_Id;
      Ent  : Entity_Id;
      Pid  : Node_Id) return Node_Id;
   --  Build the function body returning the value of the barrier expression
   --  for the specified entry body.

   function Build_Barrier_Function_Specification
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr) return Node_Id;
   --  Build a specification for a function implementing
   --  the protected entry barrier of the specified entry body.

   function Build_Corresponding_Record
     (N    : Node_Id;
      Ctyp : Node_Id;
      Loc  : Source_Ptr) return Node_Id;
   --  Common to tasks and protected types. Copy discriminant specifications,
   --  build record declaration. N is the type declaration, Ctyp is the
   --  concurrent entity (task type or protected type).

   function Build_Entry_Count_Expression
     (Concurrent_Type : Node_Id;
      Component_List  : List_Id;
      Loc             : Source_Ptr) return Node_Id;
   --  Compute number of entries for concurrent object. This is a count of
   --  simple entries, followed by an expression that computes the length
   --  of the range of each entry family. A single array with that size is
   --  allocated for each concurrent object of the type.

   function Build_Parameter_Block
     (Loc     : Source_Ptr;
      Actuals : List_Id;
      Formals : List_Id;
      Decls   : List_Id) return Entity_Id;
   --  Generate an access type for each actual parameter in the list Actuals.
   --  Cleate an encapsulating record that contains all the actuals and return
   --  its type. Generate:
   --    type Ann1 is access all <actual1-type>
   --    ...
   --    type AnnN is access all <actualN-type>
   --    type Pnn is record
   --       <formal1> : Ann1;
   --       ...
   --       <formalN> : AnnN;
   --    end record;

   function Build_Wrapper_Body
     (Loc      : Source_Ptr;
      Proc_Nam : Entity_Id;
      Obj_Typ  : Entity_Id;
      Formals  : List_Id) return Node_Id;
   --  Ada 2005 (AI-345): Build the body that wraps a primitive operation
   --  associated with a protected or task type. This is required to implement
   --  dispatching calls through interfaces. Proc_Nam is the entry name to be
   --  wrapped, Obj_Typ is the type of the newly added formal parameter to
   --  handle object notation, Formals are the original entry formals that will
   --  be explicitly replicated.

   function Build_Wrapper_Spec
     (Loc      : Source_Ptr;
      Proc_Nam : Entity_Id;
      Obj_Typ  : Entity_Id;
      Formals  : List_Id) return Node_Id;
   --  Ada 2005 (AI-345): Build the specification of a primitive operation
   --  associated with a protected or task type. This is required implement
   --  dispatching calls through interfaces. Proc_Nam is the entry name to be
   --  wrapped, Obj_Typ is the type of the newly added formal parameter to
   --  handle object notation, Formals are the original entry formals that will
   --  be explicitly replicated.

   function Build_Find_Body_Index (Typ : Entity_Id) return Node_Id;
   --  Build the function that translates the entry index in the call
   --  (which depends on the size of entry families) into an index into the
   --  Entry_Bodies_Array, to determine the body and barrier function used
   --  in a protected entry call. A pointer to this function appears in every
   --  protected object.

   function Build_Find_Body_Index_Spec (Typ : Entity_Id) return Node_Id;
   --  Build subprogram declaration for previous one

   function Build_Protected_Entry
     (N   : Node_Id;
      Ent : Entity_Id;
      Pid : Node_Id) return Node_Id;
   --  Build the procedure implementing the statement sequence of
   --  the specified entry body.

   function Build_Protected_Entry_Specification
     (Def_Id : Entity_Id;
      Ent_Id : Entity_Id;
      Loc    : Source_Ptr) return Node_Id;
   --  Build a specification for a procedure implementing
   --  the statement sequence of the specified entry body.
   --  Add attributes associating it with the entry defining identifier
   --  Ent_Id.

   function Build_Protected_Subprogram_Body
     (N         : Node_Id;
      Pid       : Node_Id;
      N_Op_Spec : Node_Id) return Node_Id;
   --  This function is used to construct the protected version of a protected
   --  subprogram. Its statement sequence first defers abort, then locks
   --  the associated protected object, and then enters a block that contains
   --  a call to the unprotected version of the subprogram (for details, see
   --  Build_Unprotected_Subprogram_Body). This block statement requires
   --  a cleanup handler that unlocks the object in all cases.
   --  (see Exp_Ch7.Expand_Cleanup_Actions).

   function Build_Protected_Spec
     (N           : Node_Id;
      Obj_Type    : Entity_Id;
      Unprotected : Boolean := False;
      Ident       : Entity_Id) return List_Id;
   --  Utility shared by Build_Protected_Sub_Spec and Expand_Access_Protected_
   --  Subprogram_Type. Builds signature of protected subprogram, adding the
   --  formal that corresponds to the object itself. For an access to protected
   --  subprogram, there is no object type to specify, so the additional
   --  parameter has type Address and mode In. An indirect call through such
   --  a pointer converts the address to a reference to the actual object.
   --  The object is a limited record and therefore a by_reference type.

   function Build_Selected_Name
     (Prefix      : Entity_Id;
      Selector    : Entity_Id;
      Append_Char : Character := ' ') return Name_Id;
   --  Build a name in the form of Prefix__Selector, with an optional
   --  character appended. This is used for internal subprograms generated
   --  for operations of protected types, including barrier functions.
   --  For the subprograms generated for entry bodies and entry barriers,
   --  the generated name includes a sequence number that makes names
   --  unique in the presence of entry overloading. This is necessary
   --  because entry body procedures and barrier functions all have the
   --  same signature.

   procedure Build_Simple_Entry_Call
     (N       : Node_Id;
      Concval : Node_Id;
      Ename   : Node_Id;
      Index   : Node_Id);
   --  Some comments here would be useful ???

   function Build_Task_Proc_Specification (T : Entity_Id) return Node_Id;
   --  This routine constructs a specification for the procedure that we will
   --  build for the task body for task type T. The spec has the form:
   --
   --    procedure tnameB (_Task : access tnameV);
   --
   --  where name is the character name taken from the task type entity that
   --  is passed as the argument to the procedure, and tnameV is the task
   --  value type that is associated with the task type.

   function Build_Unprotected_Subprogram_Body
     (N   : Node_Id;
      Pid : Node_Id) return Node_Id;
   --  This routine constructs the unprotected version of a protected
   --  subprogram body, which is contains all of the code in the
   --  original, unexpanded body. This is the version of the protected
   --  subprogram that is called from all protected operations on the same
   --  object, including the protected version of the same subprogram.

   procedure Collect_Entry_Families
     (Loc          : Source_Ptr;
      Cdecls       : List_Id;
      Current_Node : in out Node_Id;
      Conctyp      : Entity_Id);
   --  For each entry family in a concurrent type, create an anonymous array
   --  type of the right size, and add a component to the corresponding_record.

   function Family_Offset
     (Loc  : Source_Ptr;
      Hi   : Node_Id;
      Lo   : Node_Id;
      Ttyp : Entity_Id) return Node_Id;
   --  Compute (Hi - Lo) for two entry family indices. Hi is the index in
   --  an accept statement, or the upper bound in the discrete subtype of
   --  an entry declaration. Lo is the corresponding lower bound. Ttyp is
   --  the concurrent type of the entry.

   function Family_Size
     (Loc  : Source_Ptr;
      Hi   : Node_Id;
      Lo   : Node_Id;
      Ttyp : Entity_Id) return Node_Id;
   --  Compute (Hi - Lo) + 1 Max 0, to determine the number of entries in
   --  a family, and handle properly the superflat case. This is equivalent
   --  to the use of 'Length on the index type, but must use Family_Offset
   --  to handle properly the case of bounds that depend on discriminants.

   procedure Extract_Dispatching_Call
     (N        : Node_Id;
      Call_Ent : out Entity_Id;
      Object   : out Entity_Id;
      Actuals  : out List_Id;
      Formals  : out List_Id);
   --  Given a dispatching call, extract the entity of the name of the call,
   --  its object parameter, its actual parameters and the formal parameters
   --  of the overriden interface-level version.

   procedure Extract_Entry
     (N       : Node_Id;
      Concval : out Node_Id;
      Ename   : out Node_Id;
      Index   : out Node_Id);
   --  Given an entry call, returns the associated concurrent object,
   --  the entry name, and the entry family index.

   function Find_Task_Or_Protected_Pragma
     (T : Node_Id;
      P : Name_Id) return Node_Id;
   --  Searches the task or protected definition T for the first occurrence
   --  of the pragma whose name is given by P. The caller has ensured that
   --  the pragma is present in the task definition. A special case is that
   --  when P is Name_uPriority, the call will also find Interrupt_Priority.
   --  ??? Should be implemented with the rep item chain mechanism.

   function Index_Constant_Declaration
     (N        : Node_Id;
      Index_Id : Entity_Id;
      Prot     : Entity_Id) return List_Id;
   --  For an entry family and its barrier function, we define a local entity
   --  that maps the index in the call into the entry index into the object:
   --
   --     I : constant Index_Type := Index_Type'Val (
   --       E - <<index of first family member>> +
   --       Protected_Entry_Index (Index_Type'Pos (Index_Type'First)));

   function Parameter_Block_Pack
     (Loc     : Source_Ptr;
      Blk_Typ : Entity_Id;
      Actuals : List_Id;
      Formals : List_Id;
      Decls   : List_Id;
      Stmts   : List_Id) return Entity_Id;
   --  Set the components of the generated parameter block with the values of
   --  the actual parameters. Generate aliased temporaries to capture the
   --  values for types that are passed by copy. Otherwise generate a reference
   --  to the actual's value. Return the address of the aggregate block.
   --  Generate:
   --    Jnn1 : alias <formal-type1>;
   --    Jnn1 := <actual1>;
   --    ...
   --    P : Blk_Typ := (
   --      Jnn1'unchecked_access;
   --      <actual2>'reference;
   --      ...);

   function Parameter_Block_Unpack
     (Loc     : Source_Ptr;
      P       : Entity_Id;
      Actuals : List_Id;
      Formals : List_Id) return List_Id;
   --  Retrieve the values of the components from the parameter block and
   --  assign then to the original actual parameters. Generate:
   --    <actual1> := P.<formal1>;
   --    ...
   --    <actualN> := P.<formalN>;

   procedure Update_Prival_Subtypes (N : Node_Id);
   --  The actual subtypes of the privals will differ from the type of the
   --  private declaration in the original protected type, if the protected
   --  type has discriminants or if the prival has constrained components.
   --  This is because the privals are generated out of sequence w.r.t. the
   --  analysis of a protected body. After generating the bodies for protected
   --  operations, we set correctly the type of all references to privals, by
   --  means of a recursive tree traversal, which is heavy-handed but
   --  correct.

   -----------------------------
   -- Actual_Index_Expression --
   -----------------------------

   function Actual_Index_Expression
     (Sloc  : Source_Ptr;
      Ent   : Entity_Id;
      Index : Node_Id;
      Tsk   : Entity_Id) return Node_Id
   is
      Ttyp : constant Entity_Id := Etype (Tsk);
      Expr : Node_Id;
      Num  : Node_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      Prev : Entity_Id;
      S    : Node_Id;

      function Actual_Family_Offset (Hi, Lo : Node_Id) return Node_Id;
      --  Compute difference between bounds of entry family

      --------------------------
      -- Actual_Family_Offset --
      --------------------------

      function Actual_Family_Offset (Hi, Lo : Node_Id) return Node_Id is

         function Actual_Discriminant_Ref (Bound : Node_Id) return Node_Id;
         --  Replace a reference to a discriminant with a selected component
         --  denoting the discriminant of the target task.

         -----------------------------
         -- Actual_Discriminant_Ref --
         -----------------------------

         function Actual_Discriminant_Ref (Bound : Node_Id) return Node_Id is
            Typ : constant Entity_Id := Etype (Bound);
            B   : Node_Id;

         begin
            if not Is_Entity_Name (Bound)
              or else Ekind (Entity (Bound)) /= E_Discriminant
            then
               if Nkind (Bound) = N_Attribute_Reference then
                  return Bound;
               else
                  B := New_Copy_Tree (Bound);
               end if;

            else
               B :=
                 Make_Selected_Component (Sloc,
                   Prefix => New_Copy_Tree (Tsk),
                   Selector_Name => New_Occurrence_Of (Entity (Bound), Sloc));

               Analyze_And_Resolve (B, Typ);
            end if;

            return
              Make_Attribute_Reference (Sloc,
                Attribute_Name => Name_Pos,
                Prefix => New_Occurrence_Of (Etype (Bound), Sloc),
                Expressions => New_List (B));
         end Actual_Discriminant_Ref;

      --  Start of processing for Actual_Family_Offset

      begin
         return
           Make_Op_Subtract (Sloc,
             Left_Opnd  => Actual_Discriminant_Ref (Hi),
             Right_Opnd => Actual_Discriminant_Ref (Lo));
      end Actual_Family_Offset;

   --  Start of processing for Actual_Index_Expression

   begin
      --  The queues of entries and entry families appear in  textual
      --  order in the associated record. The entry index is computed as
      --  the sum of the number of queues for all entries that precede the
      --  designated one, to which is added the index expression, if this
      --  expression denotes a member of a family.

      --  The following is a place holder for the count of simple entries

      Num := Make_Integer_Literal (Sloc, 1);

      --  We construct an expression which is a series of addition
      --  operations. See comments in Entry_Index_Expression, which is
      --  identical in structure.

      if Present (Index) then
         S := Etype (Discrete_Subtype_Definition (Declaration_Node (Ent)));

         Expr :=
           Make_Op_Add (Sloc,
             Left_Opnd  => Num,

             Right_Opnd =>
               Actual_Family_Offset (
                 Make_Attribute_Reference (Sloc,
                   Attribute_Name => Name_Pos,
                   Prefix => New_Reference_To (Base_Type (S), Sloc),
                   Expressions => New_List (Relocate_Node (Index))),
                 Type_Low_Bound (S)));
      else
         Expr := Num;
      end if;

      --  Now add lengths of preceding entries and entry families

      Prev := First_Entity (Ttyp);

      while Chars (Prev) /= Chars (Ent)
        or else (Ekind (Prev) /= Ekind (Ent))
        or else not Sem_Ch6.Type_Conformant (Ent, Prev)
      loop
         if Ekind (Prev) = E_Entry then
            Set_Intval (Num, Intval (Num) + 1);

         elsif Ekind (Prev) = E_Entry_Family then
            S :=
              Etype (Discrete_Subtype_Definition (Declaration_Node (Prev)));
            Lo := Type_Low_Bound  (S);
            Hi := Type_High_Bound (S);

            Expr :=
              Make_Op_Add (Sloc,
              Left_Opnd  => Expr,
              Right_Opnd =>
                Make_Op_Add (Sloc,
                  Left_Opnd =>
                    Actual_Family_Offset (Hi, Lo),
                  Right_Opnd =>
                    Make_Integer_Literal (Sloc, 1)));

         --  Other components are anonymous types to be ignored

         else
            null;
         end if;

         Next_Entity (Prev);
      end loop;

      return Expr;
   end Actual_Index_Expression;

   ----------------------------------
   -- Add_Discriminal_Declarations --
   ----------------------------------

   procedure Add_Discriminal_Declarations
     (Decls : List_Id;
      Typ   : Entity_Id;
      Name  : Name_Id;
      Loc   : Source_Ptr)
   is
      D     : Entity_Id;

   begin
      if Has_Discriminants (Typ) then
         D := First_Discriminant (Typ);

         while Present (D) loop

            Prepend_To (Decls,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Discriminal (D),
                Subtype_Mark => New_Reference_To (Etype (D), Loc),
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Name),
                    Selector_Name => Make_Identifier (Loc, Chars (D)))));

            Next_Discriminant (D);
         end loop;
      end if;
   end Add_Discriminal_Declarations;

   ------------------------
   -- Add_Object_Pointer --
   ------------------------

   procedure Add_Object_Pointer
     (Decls : List_Id;
      Pid   : Entity_Id;
      Loc   : Source_Ptr)
   is
      Decl    : Node_Id;
      Obj_Ptr : Node_Id;

   begin
      --  Prepend the declaration of _object. This must be first in the
      --  declaration list, since it is used by the discriminal and
      --  prival declarations.
      --  ??? An attempt to make this a renaming was unsuccessful.
      --
      --     type poVP is access poV;
      --     _object : poVP := poVP!O;

      Obj_Ptr :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name
              (Chars (Corresponding_Record_Type (Pid)), 'P'));

      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uObject),
          Object_Definition => New_Reference_To (Obj_Ptr, Loc),
          Expression =>
            Unchecked_Convert_To (Obj_Ptr,
              Make_Identifier (Loc, Name_uO)));
      Set_Needs_Debug_Info (Defining_Identifier (Decl));
      Prepend_To (Decls, Decl);

      Prepend_To (Decls,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Obj_Ptr,
          Type_Definition => Make_Access_To_Object_Definition (Loc,
            Subtype_Indication =>
              New_Reference_To (Corresponding_Record_Type (Pid), Loc))));
   end Add_Object_Pointer;

   --------------------------
   -- Add_Formal_Renamings --
   --------------------------

   procedure Add_Formal_Renamings
     (Spec  : Node_Id;
      Decls : List_Id;
      Ent   : Entity_Id;
      Loc   : Source_Ptr)
   is
      Ptr : constant Entity_Id :=
              Defining_Identifier
                (Next (First (Parameter_Specifications (Spec))));
      --  The name of the formal that holds the address of the parameter block
      --  for the call.

      Comp   : Entity_Id;
      Decl   : Node_Id;
      Formal : Entity_Id;
      New_F  : Entity_Id;

   begin
      Formal := First_Formal (Ent);
      while Present (Formal) loop
         Comp   := Entry_Component (Formal);
         New_F  :=
           Make_Defining_Identifier (Sloc (Formal), Chars (Formal));
         Set_Etype (New_F, Etype (Formal));
         Set_Scope (New_F, Ent);
         Set_Needs_Debug_Info (New_F);   --  That's the whole point.

         if Ekind (Formal) = E_In_Parameter then
            Set_Ekind (New_F, E_Constant);
         else
            Set_Ekind (New_F, E_Variable);
            Set_Extra_Constrained (New_F, Extra_Constrained (Formal));
         end if;

         Set_Actual_Subtype (New_F, Actual_Subtype (Formal));

         Decl :=
           Make_Object_Renaming_Declaration (Loc,
           Defining_Identifier => New_F,
           Subtype_Mark => New_Reference_To (Etype (Formal), Loc),
           Name =>
             Make_Explicit_Dereference (Loc,
               Make_Selected_Component (Loc,
                 Prefix =>
                   Unchecked_Convert_To (Entry_Parameters_Type (Ent),
                     Make_Identifier (Loc, Chars (Ptr))),
                 Selector_Name =>
                   New_Reference_To (Comp, Loc))));

         Append (Decl, Decls);
         Set_Renamed_Object (Formal, New_F);
         Next_Formal (Formal);
      end loop;
   end Add_Formal_Renamings;

   ------------------------------
   -- Add_Private_Declarations --
   ------------------------------

   procedure Add_Private_Declarations
     (Decls : List_Id;
      Typ   : Entity_Id;
      Name  : Name_Id;
      Loc   : Source_Ptr)
   is
      Def      : constant Node_Id   := Protected_Definition (Parent (Typ));
      Decl     : Node_Id;
      Body_Ent : constant Entity_Id := Corresponding_Body   (Parent (Typ));
      P        : Node_Id;
      Pdef     : Entity_Id;

   begin
      pragma Assert (Nkind (Def) = N_Protected_Definition);

      if Present (Private_Declarations (Def)) then
         P := First (Private_Declarations (Def));
         while Present (P) loop
            if Nkind (P) = N_Component_Declaration then
               Pdef := Defining_Identifier (P);
               Decl :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Prival (Pdef),
                   Subtype_Mark => New_Reference_To (Etype (Pdef), Loc),
                   Name =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name),
                       Selector_Name => Make_Identifier (Loc, Chars (Pdef))));
               Set_Needs_Debug_Info (Defining_Identifier (Decl));
               Prepend_To (Decls, Decl);
            end if;

            Next (P);
         end loop;
      end if;

      --  One more "prival" for object itself, with the right protection type

      declare
         Protection_Type : RE_Id;

      begin
         if Has_Attach_Handler (Typ) then
            if Restricted_Profile then
               if Has_Entries (Typ) then
                  Protection_Type := RE_Protection_Entry;
               else
                  Protection_Type := RE_Protection;
               end if;
            else
               Protection_Type := RE_Static_Interrupt_Protection;
            end if;

         elsif Has_Interrupt_Handler (Typ) then
            Protection_Type := RE_Dynamic_Interrupt_Protection;

         --  The type has explicit entries or generated primitive entry
         --  wrappers.

         elsif Has_Entries (Typ)
            or else (Ada_Version >= Ada_05
                       and then Present (Interface_List (Parent (Typ))))
         then
            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Typ) > 1
            then
               Protection_Type := RE_Protection_Entries;
            else
               Protection_Type := RE_Protection_Entry;
            end if;

         else
            Protection_Type := RE_Protection;
         end if;

         Decl :=
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Object_Ref (Body_Ent),
             Subtype_Mark => New_Reference_To (RTE (Protection_Type), Loc),
             Name =>
               Make_Selected_Component (Loc,
                 Prefix        => Make_Identifier (Loc, Name),
                 Selector_Name => Make_Identifier (Loc, Name_uObject)));
         Set_Needs_Debug_Info (Defining_Identifier (Decl));
         Prepend_To (Decls, Decl);
      end;
   end Add_Private_Declarations;

   -----------------------
   -- Build_Accept_Body --
   -----------------------

   function Build_Accept_Body (Astat : Node_Id) return  Node_Id is
      Loc     : constant Source_Ptr := Sloc (Astat);
      Stats   : constant Node_Id    := Handled_Statement_Sequence (Astat);
      New_S   : Node_Id;
      Hand    : Node_Id;
      Call    : Node_Id;
      Ohandle : Node_Id;

   begin
      --  At the end of the statement sequence, Complete_Rendezvous is called.
      --  A label skipping the Complete_Rendezvous, and all other accept
      --  processing, has already been added for the expansion of requeue
      --  statements.

      Call := Build_Runtime_Call (Loc, RE_Complete_Rendezvous);
      Insert_Before (Last (Statements (Stats)), Call);
      Analyze (Call);

      --  If exception handlers are present, then append Complete_Rendezvous
      --  calls to the handlers, and construct the required outer block.

      if Present (Exception_Handlers (Stats)) then
         Hand := First (Exception_Handlers (Stats));

         while Present (Hand) loop
            Call := Build_Runtime_Call (Loc, RE_Complete_Rendezvous);
            Append (Call, Statements (Hand));
            Analyze (Call);
            Next (Hand);
         end loop;

         New_S :=
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List (
               Make_Block_Statement (Loc,
                 Handled_Statement_Sequence => Stats)));

      else
         New_S := Stats;
      end if;

      --  At this stage we know that the new statement sequence does not
      --  have an exception handler part, so we supply one to call
      --  Exceptional_Complete_Rendezvous. This handler is

      --    when all others =>
      --       Exceptional_Complete_Rendezvous (Get_GNAT_Exception);

      --  We handle Abort_Signal to make sure that we properly catch the abort
      --  case and wake up the caller.

      Ohandle := Make_Others_Choice (Loc);
      Set_All_Others (Ohandle);

      Set_Exception_Handlers (New_S,
        New_List (
          Make_Exception_Handler (Loc,
            Exception_Choices => New_List (Ohandle),

            Statements =>  New_List (
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  RTE (RE_Exceptional_Complete_Rendezvous), Loc),
                Parameter_Associations => New_List (
                  Make_Function_Call (Loc,
                    Name => New_Reference_To (
                      RTE (RE_Get_GNAT_Exception), Loc))))))));

      Set_Parent (New_S, Astat); -- temp parent for Analyze call
      Analyze_Exception_Handlers (Exception_Handlers (New_S));
      Expand_Exception_Handlers (New_S);

      --  Exceptional_Complete_Rendezvous must be called with abort
      --  still deferred, which is the case for a "when all others" handler.

      return New_S;
   end Build_Accept_Body;

   -----------------------------------
   -- Build_Activation_Chain_Entity --
   -----------------------------------

   procedure Build_Activation_Chain_Entity (N : Node_Id) is
      P     : Node_Id;
      B     : Node_Id;
      Decls : List_Id;

   begin
      --  Loop to find enclosing construct containing activation chain variable

      P := Parent (N);

      while Nkind (P) /= N_Subprogram_Body
        and then Nkind (P) /= N_Package_Declaration
        and then Nkind (P) /= N_Package_Body
        and then Nkind (P) /= N_Block_Statement
        and then Nkind (P) /= N_Task_Body
      loop
         P := Parent (P);
      end loop;

      --  If we are in a package body, the activation chain variable is
      --  allocated in the corresponding spec. First, we save the package
      --  body node because we enter the new entity in its Declarations list.

      B := P;

      if Nkind (P) = N_Package_Body then
         P := Unit_Declaration_Node (Corresponding_Spec (P));
         Decls := Declarations (B);

      elsif Nkind (P) = N_Package_Declaration then
         Decls := Visible_Declarations (Specification (B));

      else
         Decls := Declarations (B);
      end if;

      --  If activation chain entity not already declared, declare it

      if No (Activation_Chain_Entity (P)) then
         Set_Activation_Chain_Entity
           (P, Make_Defining_Identifier (Sloc (N), Name_uChain));

         Prepend_To (Decls,
           Make_Object_Declaration (Sloc (P),
             Defining_Identifier => Activation_Chain_Entity (P),
             Aliased_Present => True,
             Object_Definition   =>
               New_Reference_To (RTE (RE_Activation_Chain), Sloc (P))));

         Analyze (First (Decls));
      end if;
   end Build_Activation_Chain_Entity;

   ----------------------------
   -- Build_Barrier_Function --
   ----------------------------

   function Build_Barrier_Function
     (N    : Node_Id;
      Ent  : Entity_Id;
      Pid  : Node_Id) return Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Ent_Formals : constant Node_Id    := Entry_Body_Formal_Part (N);
      Index_Spec  : constant Node_Id    := Entry_Index_Specification
                                                           (Ent_Formals);
      Op_Decls    : constant List_Id    := New_List;
      Bdef        : Entity_Id;
      Bspec       : Node_Id;

   begin
      Bdef :=
        Make_Defining_Identifier (Loc, Chars (Barrier_Function (Ent)));
      Bspec := Build_Barrier_Function_Specification (Bdef, Loc);

      --  <object pointer declaration>
      --  <discriminant renamings>
      --  <private object renamings>
      --  Add discriminal and private renamings. These names have
      --  already been used to expand references to discriminants
      --  and private data.

      Add_Discriminal_Declarations (Op_Decls, Pid, Name_uObject, Loc);
      Add_Private_Declarations (Op_Decls, Pid, Name_uObject, Loc);
      Add_Object_Pointer (Op_Decls, Pid, Loc);

      --  If this is the barrier for an entry family, the entry index is
      --  visible in the body of the barrier. Create a local variable that
      --  converts the entry index (which is the last formal of the barrier
      --  function) into the appropriate offset into the entry array. The
      --  entry index constant must be set, as for the entry body, so that
      --  local references to the entry index are correctly replaced with
      --  the local variable. This parallels what is done for entry bodies.

      if Present (Index_Spec) then
         declare
            Index_Id  : constant Entity_Id := Defining_Identifier (Index_Spec);
            Index_Con : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            Chars => New_Internal_Name ('J'));

         begin
            Set_Entry_Index_Constant (Index_Id, Index_Con);
            Append_List_To (Op_Decls,
              Index_Constant_Declaration (N, Index_Id, Pid));
         end;
      end if;

      --  Note: the condition in the barrier function needs to be properly
      --  processed for the C/Fortran boolean possibility, but this happens
      --  automatically since the return statement does this normalization.

      return
        Make_Subprogram_Body (Loc,
          Specification => Bspec,
          Declarations => Op_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Return_Statement (Loc,
                  Expression => Condition (Ent_Formals)))));
   end Build_Barrier_Function;

   ------------------------------------------
   -- Build_Barrier_Function_Specification --
   ------------------------------------------

   function Build_Barrier_Function_Specification
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr) return Node_Id
   is
   begin
      Set_Needs_Debug_Info (Def_Id);
      return Make_Function_Specification (Loc,
        Defining_Unit_Name => Def_Id,
        Parameter_Specifications => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_uO),
            Parameter_Type =>
              New_Reference_To (RTE (RE_Address), Loc)),

          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_uE),
            Parameter_Type =>
              New_Reference_To (RTE (RE_Protected_Entry_Index), Loc))),

        Result_Definition => New_Reference_To (Standard_Boolean, Loc));
   end Build_Barrier_Function_Specification;

   --------------------------
   -- Build_Call_With_Task --
   --------------------------

   function Build_Call_With_Task
     (N : Node_Id;
      E : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);
   begin
      return
        Make_Function_Call (Loc,
          Name => New_Reference_To (E, Loc),
          Parameter_Associations => New_List (Concurrent_Ref (N)));
   end Build_Call_With_Task;

   --------------------------------
   -- Build_Corresponding_Record --
   --------------------------------

   function Build_Corresponding_Record
    (N    : Node_Id;
     Ctyp : Entity_Id;
     Loc  : Source_Ptr) return Node_Id
   is
      Rec_Ent  : constant Entity_Id :=
                   Make_Defining_Identifier
                     (Loc, New_External_Name (Chars (Ctyp), 'V'));
      Disc     : Entity_Id;
      Dlist    : List_Id;
      New_Disc : Entity_Id;
      Cdecls   : List_Id;

   begin
      Set_Corresponding_Record_Type     (Ctyp, Rec_Ent);
      Set_Ekind                         (Rec_Ent, E_Record_Type);
      Set_Has_Delayed_Freeze            (Rec_Ent, Has_Delayed_Freeze (Ctyp));
      Set_Is_Concurrent_Record_Type     (Rec_Ent, True);
      Set_Corresponding_Concurrent_Type (Rec_Ent, Ctyp);
      Set_Stored_Constraint             (Rec_Ent, No_Elist);
      Cdecls := New_List;

      --  Use discriminals to create list of discriminants for record, and
      --  create new discriminals for use in default expressions, etc. It is
      --  worth noting that a task discriminant gives rise to 5 entities;

      --  a) The original discriminant.
      --  b) The discriminal for use in the task.
      --  c) The discriminant of the corresponding record.
      --  d) The discriminal for the init proc of the corresponding record.
      --  e) The local variable that renames the discriminant in the procedure
      --     for the task body.

      --  In fact the discriminals b) are used in the renaming declarations
      --  for e). See details in  einfo (Handling of Discriminants).

      if Present (Discriminant_Specifications (N)) then
         Dlist := New_List;
         Disc := First_Discriminant (Ctyp);

         while Present (Disc) loop
            New_Disc := CR_Discriminant (Disc);

            Append_To (Dlist,
              Make_Discriminant_Specification (Loc,
                Defining_Identifier => New_Disc,
                Discriminant_Type =>
                  New_Occurrence_Of (Etype (Disc), Loc),
                Expression =>
                  New_Copy (Discriminant_Default_Value (Disc))));

            Next_Discriminant (Disc);
         end loop;

      else
         Dlist := No_List;
      end if;

      --  Now we can construct the record type declaration. Note that this
      --  record is "limited tagged". It is "limited" to reflect the underlying
      --  limitedness of the task or protected object that it represents, and
      --  ensuring for example that it is properly passed by reference. It is
      --  "tagged" to give support to dispatching calls through interfaces (Ada
      --  2005: AI-345)

      return
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Rec_Ent,
          Discriminant_Specifications => Dlist,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Items => Cdecls),
              Tagged_Present  =>
                 Ada_Version >= Ada_05 and then Is_Tagged_Type (Ctyp),
              Limited_Present => True));
   end Build_Corresponding_Record;

   ----------------------------------
   -- Build_Entry_Count_Expression --
   ----------------------------------

   function Build_Entry_Count_Expression
     (Concurrent_Type : Node_Id;
      Component_List  : List_Id;
      Loc             : Source_Ptr) return Node_Id
   is
      Eindx  : Nat;
      Ent    : Entity_Id;
      Ecount : Node_Id;
      Comp   : Node_Id;
      Lo     : Node_Id;
      Hi     : Node_Id;
      Typ    : Entity_Id;

   begin
      --  Count number of non-family entries

      Eindx := 0;
      Ent := First_Entity (Concurrent_Type);
      while Present (Ent) loop
         if Ekind (Ent) = E_Entry then
            Eindx := Eindx + 1;
         end if;

         Next_Entity (Ent);
      end loop;

      Ecount := Make_Integer_Literal (Loc, Eindx);

      --  Loop through entry families building the addition nodes

      Ent := First_Entity (Concurrent_Type);
      Comp := First (Component_List);
      while Present (Ent) loop
         if Ekind (Ent) = E_Entry_Family then
            while Chars (Ent) /= Chars (Defining_Identifier (Comp)) loop
               Next (Comp);
            end loop;

            Typ := Etype (Discrete_Subtype_Definition (Parent (Ent)));
            Hi := Type_High_Bound (Typ);
            Lo := Type_Low_Bound  (Typ);

            Ecount :=
              Make_Op_Add (Loc,
                Left_Opnd  => Ecount,
                Right_Opnd => Family_Size (Loc, Hi, Lo, Concurrent_Type));
         end if;

         Next_Entity (Ent);
      end loop;

      return Ecount;
   end Build_Entry_Count_Expression;

   ---------------------------
   -- Build_Parameter_Block --
   ---------------------------

   function Build_Parameter_Block
     (Loc     : Source_Ptr;
      Actuals : List_Id;
      Formals : List_Id;
      Decls   : List_Id) return Entity_Id
   is
      Actual   : Entity_Id;
      Comp_Nam : Node_Id;
      Comps    : List_Id;
      Formal   : Entity_Id;
      Has_Comp : Boolean := False;
      Rec_Nam  : Node_Id;

   begin
      Actual := First (Actuals);
      Comps  := New_List;
      Formal := Defining_Identifier (First (Formals));

      while Present (Actual) loop
         if not Is_Controlling_Actual (Actual) then

            --  Generate:
            --    type Ann is access all <actual-type>

            Comp_Nam :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

            Append_To (Decls,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier =>
                  Comp_Nam,
                Type_Definition =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present =>
                      True,
                    Constant_Present =>
                      Ekind (Formal) = E_In_Parameter,
                    Subtype_Indication =>
                      New_Reference_To (Etype (Actual), Loc))));

            --  Generate:
            --    Param : Ann;

            Append_To (Comps,
              Make_Component_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Chars (Formal)),
                Component_Definition =>
                  Make_Component_Definition (Loc,
                    Aliased_Present =>
                      False,
                    Subtype_Indication =>
                      New_Reference_To (Comp_Nam, Loc))));

            Has_Comp := True;
         end if;

         Next_Actual (Actual);
         Next_Formal_With_Extras (Formal);
      end loop;

      Rec_Nam :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      if Has_Comp then

         --  Generate:
         --    type Pnn is record
         --       Param1 : Ann1;
         --       ...
         --       ParamN : AnnN;

         --  where Pnn is a parameter wrapping record, Param1 .. ParamN are
         --  the original parameter names and Ann1 .. AnnN are the access to
         --  actual types.

         Append_To (Decls,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier =>
               Rec_Nam,
             Type_Definition =>
               Make_Record_Definition (Loc,
                 Component_List =>
                   Make_Component_List (Loc, Comps))));
      else
         --  Generate:
         --    type Pnn is null record;

         Append_To (Decls,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier =>
               Rec_Nam,
             Type_Definition =>
               Make_Record_Definition (Loc,
                 Null_Present   => True,
                 Component_List => Empty)));
      end if;

      return Rec_Nam;
   end Build_Parameter_Block;

   ------------------------
   -- Build_Wrapper_Body --
   ------------------------

   function Build_Wrapper_Body
     (Loc      : Source_Ptr;
      Proc_Nam : Entity_Id;
      Obj_Typ  : Entity_Id;
      Formals  : List_Id) return Node_Id
   is
      Actuals      : List_Id := No_List;
      Body_Spec    : Node_Id;
      Conv_Id      : Node_Id;
      First_Formal : Node_Id;
      Formal       : Node_Id;

   begin
      Body_Spec := Build_Wrapper_Spec (Loc, Proc_Nam, Obj_Typ, Formals);

      --  If we did not generate the specification do have nothing else to do

      if Body_Spec = Empty then
         return Empty;
      end if;

      --  Map formals to actuals. Use the list built for the wrapper spec,
      --  skipping the object notation parameter.

      First_Formal := First (Parameter_Specifications (Body_Spec));

      Formal := First_Formal;
      Next (Formal);

      if Present (Formal) then
         Actuals := New_List;

         while Present (Formal) loop
            Append_To (Actuals,
              Make_Identifier (Loc, Chars =>
                Chars (Defining_Identifier (Formal))));

            Next (Formal);
         end loop;
      end if;

      --  An access-to-variable first parameter will require an explicit
      --  dereference in the unchecked conversion. This case occurs when
      --  a protected entry wrapper must override an interface-level
      --  procedure with interface access as first parameter.

      --     SubprgName (O.all).Proc_Nam (Formal_1 .. Formal_N)

      if Nkind (Parameter_Type (First_Formal)) = N_Access_Definition then
         Conv_Id :=
           Make_Explicit_Dereference (Loc,
             Prefix =>
               Make_Identifier (Loc, Chars => Name_uO));
      else
         Conv_Id :=
           Make_Identifier (Loc, Chars => Name_uO);
      end if;

      if Ekind (Proc_Nam) = E_Function then
         return
           Make_Subprogram_Body (Loc,
             Specification => Body_Spec,
             Declarations  => Empty_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements =>
                   New_List (
                     Make_Return_Statement (Loc,
                        Make_Function_Call (Loc,
                          Name =>
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Unchecked_Convert_To (
                                  Corresponding_Concurrent_Type (Obj_Typ),
                                  Conv_Id),
                              Selector_Name =>
                                New_Reference_To (Proc_Nam, Loc)),
                          Parameter_Associations => Actuals)))));
      else
         return
           Make_Subprogram_Body (Loc,
             Specification => Body_Spec,
             Declarations  => Empty_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements =>
                   New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Unchecked_Convert_To (
                               Corresponding_Concurrent_Type (Obj_Typ),
                               Conv_Id),
                           Selector_Name =>
                             New_Reference_To (Proc_Nam, Loc)),
                       Parameter_Associations => Actuals))));
      end if;
   end Build_Wrapper_Body;

   ------------------------
   -- Build_Wrapper_Spec --
   ------------------------

   function Build_Wrapper_Spec
     (Loc      : Source_Ptr;
      Proc_Nam : Entity_Id;
      Obj_Typ  : Entity_Id;
      Formals  : List_Id) return Node_Id
   is
      New_Name_Id : constant Entity_Id :=
                      Make_Defining_Identifier (Loc, Chars (Proc_Nam));

      First_Param        : Node_Id := Empty;
      Iface              : Entity_Id;
      Iface_Elmt         : Elmt_Id := No_Elmt;
      New_Formals        : List_Id;
      Obj_Param          : Node_Id;
      Obj_Param_Typ      : Node_Id;
      Iface_Prim_Op      : Entity_Id;
      Iface_Prim_Op_Elmt : Elmt_Id;

      function Overriding_Possible
        (Iface_Prim_Op : Entity_Id;
         Proc_Nam      : Entity_Id) return Boolean;
      --  Determine whether a primitive operation can be overriden by the
      --  wrapper. Iface_Prim_Op is the candidate primitive operation of an
      --  abstract interface type, Proc_Nam is the generated entry wrapper.

      function Replicate_Entry_Formals
        (Loc     : Source_Ptr;
         Formals : List_Id) return List_Id;
      --  An explicit parameter replication is required due to the
      --  Is_Entry_Formal flag being set for all the formals. The explicit
      --  replication removes the flag that would otherwise cause a different
      --  path of analysis.

      -------------------------
      -- Overriding_Possible --
      -------------------------

      function Overriding_Possible
        (Iface_Prim_Op : Entity_Id;
         Proc_Nam      : Entity_Id) return Boolean
      is
         Prim_Op_Spec  : constant Node_Id := Parent (Iface_Prim_Op);
         Proc_Spec     : constant Node_Id := Parent (Proc_Nam);

         Is_Access_To_Variable : Boolean;
         Is_Out_Present        : Boolean;

         function Type_Conformant_Parameters
           (Prim_Op_Param_Specs : List_Id;
            Proc_Param_Specs    : List_Id) return Boolean;
         --  Determine whether the parameters of the generated entry wrapper
         --  and those of a primitive operation are type conformant. During
         --  this check, the first parameter of the primitive operation is
         --  always skipped.

         --------------------------------
         -- Type_Conformant_Parameters --
         --------------------------------

         function Type_Conformant_Parameters
           (Prim_Op_Param_Specs : List_Id;
            Proc_Param_Specs    : List_Id) return Boolean
         is
            Prim_Op_Param : Node_Id;
            Proc_Param    : Node_Id;

         begin
            --  Skip the first parameter of the primitive operation

            Prim_Op_Param := Next (First (Prim_Op_Param_Specs));
            Proc_Param    := First (Proc_Param_Specs);
            while Present (Prim_Op_Param)
              and then Present (Proc_Param)
            loop
               --  The two parameters must be mode conformant and have
               --  the exact same types.

               if Ekind (Defining_Identifier (Prim_Op_Param)) /=
                  Ekind (Defining_Identifier (Proc_Param))
                 or else Etype (Parameter_Type (Prim_Op_Param)) /=
                         Etype (Parameter_Type (Proc_Param))
               then
                  return False;
               end if;

               Next (Prim_Op_Param);
               Next (Proc_Param);
            end loop;

            --  One of the lists is longer than the other

            if Present (Prim_Op_Param) or else Present (Proc_Param) then
               return False;
            end if;

            return True;
         end Type_Conformant_Parameters;

      --  Start of processing for Overriding_Possible

      begin
         if Chars (Iface_Prim_Op) /= Chars (Proc_Nam) then
            return False;
         end if;

         --  Special check for protected procedures: If an inherited subprogram
         --  is implemented by a protected procedure or an entry, then the
         --  first parameter of the inherited subprogram shall be of mode OUT
         --  or IN OUT, or an access-to-variable parameter.

         if Ekind (Iface_Prim_Op) = E_Procedure then

            Is_Out_Present :=
              Present (Parameter_Specifications (Prim_Op_Spec))
                and then
              Out_Present (First (Parameter_Specifications (Prim_Op_Spec)));

            Is_Access_To_Variable :=
              Present (Parameter_Specifications (Prim_Op_Spec))
                and then
              Nkind (Parameter_Type
                     (First
                      (Parameter_Specifications (Prim_Op_Spec))))
                        = N_Access_Definition;

            if not Is_Out_Present
              and then not Is_Access_To_Variable
            then
               return False;
            end if;
         end if;

         return Type_Conformant_Parameters (
           Parameter_Specifications (Prim_Op_Spec),
           Parameter_Specifications (Proc_Spec));
      end Overriding_Possible;

      -----------------------------
      -- Replicate_Entry_Formals --
      -----------------------------

      function Replicate_Entry_Formals
        (Loc     : Source_Ptr;
         Formals : List_Id) return List_Id
      is
         New_Formals : constant List_Id := New_List;
         Formal      : Node_Id;

      begin
         Formal := First (Formals);
         while Present (Formal) loop

            --  Create an explicit copy of the entry parameter

            Append_To (New_Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc,
                    Chars          => Chars (Defining_Identifier (Formal))),
                    In_Present     => In_Present  (Formal),
                    Out_Present    => Out_Present (Formal),
                    Parameter_Type => New_Reference_To (Etype (
                                        Parameter_Type (Formal)), Loc)));

            Next (Formal);
         end loop;

         return New_Formals;
      end Replicate_Entry_Formals;

   --  Start of processing for Build_Wrapper_Spec

   begin
      --  The mode is determined by the first parameter of the interface-level
      --  procedure that the current entry is trying to override.

      pragma Assert (Present (Abstract_Interfaces
                     (Corresponding_Record_Type (Scope (Proc_Nam)))));

      Iface_Elmt :=
        First_Elmt (Abstract_Interfaces
                    (Corresponding_Record_Type (Scope (Proc_Nam))));

      --  We must examine all the protected operations of the implemented
      --  interfaces in order to discover a possible overriding candidate.

      Examine_Interfaces : while Present (Iface_Elmt) loop
         Iface := Node (Iface_Elmt);

         if Present (Primitive_Operations (Iface)) then
            Iface_Prim_Op_Elmt := First_Elmt (Primitive_Operations (Iface));
            while Present (Iface_Prim_Op_Elmt) loop
               Iface_Prim_Op := Node (Iface_Prim_Op_Elmt);

               while Present (Alias (Iface_Prim_Op)) loop
                  Iface_Prim_Op := Alias (Iface_Prim_Op);
               end loop;

               --  The current primitive operation can be overriden by the
               --  generated entry wrapper.

               if Overriding_Possible (Iface_Prim_Op, Proc_Nam) then
                  First_Param :=
                    First (Parameter_Specifications (Parent (Iface_Prim_Op)));

                  exit Examine_Interfaces;
               end if;

               Next_Elmt (Iface_Prim_Op_Elmt);
            end loop;
         end if;

         Next_Elmt (Iface_Elmt);
      end loop Examine_Interfaces;

      --  Return if no interface primitive can be overriden

      if not Present (First_Param) then
         return Empty;
      end if;

      New_Formals := Replicate_Entry_Formals (Loc, Formals);

      --  ??? Certain source packages contain protected or task types that do
      --  not implement any interfaces and are compiled with the -gnat05
      --  switch.  In this case, a default first parameter is created.

      if Present (First_Param) then
         if Nkind (Parameter_Type (First_Param)) = N_Access_Definition then
            Obj_Param_Typ :=
              Make_Access_Definition (Loc,
                Subtype_Mark =>
                  New_Reference_To (Obj_Typ, Loc));
         else
            Obj_Param_Typ := New_Reference_To (Obj_Typ, Loc);
         end if;

         Obj_Param :=
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uO),
             In_Present  => In_Present  (First_Param),
             Out_Present => Out_Present (First_Param),
             Parameter_Type => Obj_Param_Typ);

      else
         Obj_Param :=
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uO),
             In_Present  => True,
             Out_Present => True,
               Parameter_Type => New_Reference_To (Obj_Typ, Loc));
      end if;

      Prepend_To (New_Formals, Obj_Param);

      --  Minimum decoration needed to catch the entity in
      --  Sem_Ch6.Override_Dispatching_Operation

      if Ekind (Proc_Nam) = E_Procedure
        or else Ekind (Proc_Nam) = E_Entry
      then
         Set_Ekind                (New_Name_Id, E_Procedure);
         Set_Is_Primitive_Wrapper (New_Name_Id);
         Set_Wrapped_Entity       (New_Name_Id, Proc_Nam);

         return
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name => New_Name_Id,
             Parameter_Specifications => New_Formals);

      else pragma Assert (Ekind (Proc_Nam) = E_Function);
         Set_Ekind (New_Name_Id, E_Function);

         return
           Make_Function_Specification (Loc,
             Defining_Unit_Name => New_Name_Id,
             Parameter_Specifications => New_Formals,
             Result_Definition =>
               New_Copy (Result_Definition (Parent (Proc_Nam))));
      end if;
   end Build_Wrapper_Spec;

   ---------------------------
   -- Build_Find_Body_Index --
   ---------------------------

   function Build_Find_Body_Index (Typ : Entity_Id) return Node_Id is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Ent   : Entity_Id;
      E_Typ : Entity_Id;
      Has_F : Boolean := False;
      Index : Nat;
      If_St : Node_Id := Empty;
      Lo    : Node_Id;
      Hi    : Node_Id;
      Decls : List_Id := New_List;
      Ret   : Node_Id;
      Spec  : Node_Id;
      Siz   : Node_Id := Empty;

      procedure Add_If_Clause (Expr : Node_Id);
      --  Add test for range of current entry

      function Convert_Discriminant_Ref (Bound : Node_Id) return Node_Id;
      --  If a bound of an entry is given by a discriminant, retrieve the
      --  actual value of the discriminant from the enclosing object.

      -------------------
      -- Add_If_Clause --
      -------------------

      procedure Add_If_Clause (Expr : Node_Id) is
         Cond  : Node_Id;
         Stats : constant List_Id :=
                   New_List (
                     Make_Return_Statement (Loc,
                       Expression => Make_Integer_Literal (Loc, Index + 1)));

      begin
         --  Index for current entry body

         Index := Index + 1;

         --  Compute total length of entry queues so far

         if No (Siz) then
            Siz := Expr;
         else
            Siz :=
              Make_Op_Add (Loc,
                Left_Opnd => Siz,
                Right_Opnd => Expr);
         end if;

         Cond :=
           Make_Op_Le (Loc,
             Left_Opnd => Make_Identifier (Loc, Name_uE),
             Right_Opnd => Siz);

         --  Map entry queue indices in the range of the current family
         --  into the current index, that designates the entry body.

         if No (If_St) then
            If_St :=
              Make_Implicit_If_Statement (Typ,
                Condition => Cond,
                Then_Statements => Stats,
                Elsif_Parts   => New_List);

            Ret := If_St;

         else
            Append (
              Make_Elsif_Part (Loc,
                Condition => Cond,
                Then_Statements => Stats),
              Elsif_Parts (If_St));
         end if;
      end Add_If_Clause;

      ------------------------------
      -- Convert_Discriminant_Ref --
      ------------------------------

      function Convert_Discriminant_Ref (Bound : Node_Id) return Node_Id is
         B   : Node_Id;

      begin
         if Is_Entity_Name (Bound)
           and then Ekind (Entity (Bound)) = E_Discriminant
         then
            B :=
              Make_Selected_Component (Loc,
               Prefix =>
                 Unchecked_Convert_To (Corresponding_Record_Type (Typ),
                   Make_Explicit_Dereference (Loc,
                     Make_Identifier (Loc, Name_uObject))),
               Selector_Name => Make_Identifier (Loc, Chars (Bound)));
            Set_Etype (B, Etype (Entity (Bound)));
         else
            B := New_Copy_Tree (Bound);
         end if;

         return B;
      end Convert_Discriminant_Ref;

   --  Start of processing for Build_Find_Body_Index

   begin
      Spec := Build_Find_Body_Index_Spec (Typ);

      Ent := First_Entity (Typ);
      while Present (Ent) loop
         if Ekind (Ent) = E_Entry_Family then
            Has_F := True;
            exit;
         end if;

         Next_Entity (Ent);
      end loop;

      if not Has_F then

         --  If the protected type has no entry families, there is a one-one
         --  correspondence between entry queue and entry body.

         Ret :=
           Make_Return_Statement (Loc,
             Expression => Make_Identifier (Loc, Name_uE));

      else
         --  Suppose entries e1, e2, ... have size l1, l2, ... we generate
         --  the following:
         --
         --  if E <= l1 then return 1;
         --  elsif E <= l1 + l2 then return 2;
         --  ...

         Index := 0;
         Siz   := Empty;
         Ent   := First_Entity (Typ);

         Add_Object_Pointer (Decls, Typ, Loc);

         while Present (Ent) loop

            if Ekind (Ent) = E_Entry then
               Add_If_Clause (Make_Integer_Literal (Loc, 1));

            elsif Ekind (Ent) = E_Entry_Family then

               E_Typ := Etype (Discrete_Subtype_Definition (Parent (Ent)));
               Hi := Convert_Discriminant_Ref (Type_High_Bound (E_Typ));
               Lo := Convert_Discriminant_Ref (Type_Low_Bound  (E_Typ));
               Add_If_Clause (Family_Size (Loc, Hi, Lo, Typ));
            end if;

            Next_Entity (Ent);
         end loop;

         if Index = 1 then
            Decls := New_List;
            Ret :=
              Make_Return_Statement (Loc,
                Expression => Make_Integer_Literal (Loc, 1));

         elsif Nkind (Ret) = N_If_Statement then

            --  Ranges are in increasing order, so last one doesn't need guard

            declare
               Nod : constant Node_Id := Last (Elsif_Parts (Ret));
            begin
               Remove (Nod);
               Set_Else_Statements (Ret, Then_Statements (Nod));
            end;
         end if;
      end if;

      return
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations  => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (Ret)));
   end Build_Find_Body_Index;

   --------------------------------
   -- Build_Find_Body_Index_Spec --
   --------------------------------

   function Build_Find_Body_Index_Spec (Typ : Entity_Id) return Node_Id is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Id    : constant Entity_Id :=
               Make_Defining_Identifier (Loc,
                 Chars => New_External_Name (Chars (Typ), 'F'));
      Parm1 : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uO);
      Parm2 : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uE);

   begin
      return
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Id,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Parm1,
              Parameter_Type =>
                New_Reference_To (RTE (RE_Address), Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Parm2,
              Parameter_Type =>
                New_Reference_To (RTE (RE_Protected_Entry_Index), Loc))),
          Result_Definition => New_Occurrence_Of (
            RTE (RE_Protected_Entry_Index), Loc));
   end Build_Find_Body_Index_Spec;

   -------------------------
   -- Build_Master_Entity --
   -------------------------

   procedure Build_Master_Entity (E : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (E);
      P    : Node_Id;
      Decl : Node_Id;
      S    : Entity_Id;

   begin
      S := Scope (E);

      --  Ada 2005 (AI-287): Do not set/get the has_master_entity reminder
      --  in internal scopes, unless present already.. Required for nested
      --  limited aggregates. This could use some more explanation ????

      if Ada_Version >= Ada_05 then
         while Is_Internal (S) loop
            S := Scope (S);
         end loop;
      end if;

      --  Nothing to do if we already built a master entity for this scope
      --  or if there is no task hierarchy.

      if Has_Master_Entity (S)
        or else Restriction_Active (No_Task_Hierarchy)
      then
         return;
      end if;

      --  Otherwise first build the master entity
      --    _Master : constant Master_Id := Current_Master.all;
      --  and insert it just before the current declaration

      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uMaster),
          Constant_Present => True,
          Object_Definition => New_Reference_To (RTE (RE_Master_Id), Loc),
          Expression =>
            Make_Explicit_Dereference (Loc,
              New_Reference_To (RTE (RE_Current_Master), Loc)));

      P := Parent (E);
      Insert_Before (P, Decl);
      Analyze (Decl);

      --  Ada 2005 (AI-287): Set the has_master_entity reminder in the
      --  non-internal scope selected above.

      if Ada_Version >= Ada_05 then
         Set_Has_Master_Entity (S);
      else
         Set_Has_Master_Entity (Scope (E));
      end if;

      --  Now mark the containing scope as a task master

      while Nkind (P) /= N_Compilation_Unit loop
         P := Parent (P);

         --  If we fall off the top, we are at the outer level, and the
         --  environment task is our effective master, so nothing to mark.

         if Nkind (P) = N_Task_Body
           or else Nkind (P) = N_Block_Statement
           or else Nkind (P) = N_Subprogram_Body
         then
            Set_Is_Task_Master (P, True);
            return;

         elsif Nkind (Parent (P)) = N_Subunit then
            P := Corresponding_Stub (Parent (P));
         end if;
      end loop;
   end Build_Master_Entity;

   ---------------------------
   -- Build_Protected_Entry --
   ---------------------------

   function Build_Protected_Entry
     (N   : Node_Id;
      Ent : Entity_Id;
      Pid : Node_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (N);
      Op_Decls : constant List_Id    := New_List;
      Edef     : Entity_Id;
      Espec    : Node_Id;
      Op_Stats : List_Id;
      Ohandle  : Node_Id;
      Complete : Node_Id;

   begin
      Edef :=
        Make_Defining_Identifier (Loc,
          Chars => Chars (Protected_Body_Subprogram (Ent)));
      Espec := Build_Protected_Entry_Specification (Edef, Empty, Loc);

      --  <object pointer declaration>

      --  Add object pointer declaration. This is needed by the discriminal and
      --  prival renamings, which should already have been inserted into the
      --  declaration list.

      Add_Object_Pointer (Op_Decls, Pid, Loc);

      --  Add renamings for formals for use by debugger

      Add_Formal_Renamings (Espec, Op_Decls, Ent, Loc);

      if Abort_Allowed
        or else Restriction_Active (No_Entry_Queue) = False
        or else Number_Entries (Pid) > 1
      then
         Complete := New_Reference_To (RTE (RE_Complete_Entry_Body), Loc);
      else
         Complete :=
           New_Reference_To (RTE (RE_Complete_Single_Entry_Body), Loc);
      end if;

      Op_Stats := New_List (
         Make_Block_Statement (Loc,
           Declarations => Declarations (N),
           Handled_Statement_Sequence =>
             Handled_Statement_Sequence (N)),

         Make_Procedure_Call_Statement (Loc,
           Name => Complete,
           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix =>
                 Make_Selected_Component (Loc,
                   Prefix =>
                     Make_Identifier (Loc, Name_uObject),

                   Selector_Name =>
                     Make_Identifier (Loc, Name_uObject)),
                 Attribute_Name => Name_Unchecked_Access))));

      if Restriction_Active (No_Exception_Handlers) then
         return
           Make_Subprogram_Body (Loc,
             Specification => Espec,
             Declarations => Op_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Op_Stats));

      else
         Ohandle := Make_Others_Choice (Loc);
         Set_All_Others (Ohandle);

         if Abort_Allowed
           or else Restriction_Active (No_Entry_Queue) = False
           or else Number_Entries (Pid) > 1
         then
            Complete :=
              New_Reference_To (RTE (RE_Exceptional_Complete_Entry_Body), Loc);

         else
            Complete := New_Reference_To (
              RTE (RE_Exceptional_Complete_Single_Entry_Body), Loc);
         end if;

         --  Create body of entry procedure. The renaming declarations are
         --  placed ahead of the block that contains the actual entry body.

         return
           Make_Subprogram_Body (Loc,
             Specification => Espec,
             Declarations => Op_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Op_Stats,
                 Exception_Handlers => New_List (
                   Make_Exception_Handler (Loc,
                     Exception_Choices => New_List (Ohandle),

                     Statements =>  New_List (
                       Make_Procedure_Call_Statement (Loc,
                         Name => Complete,
                         Parameter_Associations => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix =>
                               Make_Selected_Component (Loc,
                                 Prefix =>
                                   Make_Identifier (Loc, Name_uObject),
                                 Selector_Name =>
                                   Make_Identifier (Loc, Name_uObject)),
                               Attribute_Name => Name_Unchecked_Access),

                           Make_Function_Call (Loc,
                             Name => New_Reference_To (
                               RTE (RE_Get_GNAT_Exception), Loc)))))))));
      end if;
   end Build_Protected_Entry;

   -----------------------------------------
   -- Build_Protected_Entry_Specification --
   -----------------------------------------

   function Build_Protected_Entry_Specification
     (Def_Id : Entity_Id;
      Ent_Id : Entity_Id;
      Loc    : Source_Ptr) return Node_Id
   is
      P : Entity_Id;

   begin
      Set_Needs_Debug_Info (Def_Id);
      P := Make_Defining_Identifier (Loc, Name_uP);

      if Present (Ent_Id) then
         Append_Elmt (P, Accept_Address (Ent_Id));
      end if;

      return Make_Procedure_Specification (Loc,
        Defining_Unit_Name => Def_Id,
        Parameter_Specifications => New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_uO),
            Parameter_Type =>
              New_Reference_To (RTE (RE_Address), Loc)),

          Make_Parameter_Specification (Loc,
            Defining_Identifier => P,
            Parameter_Type =>
              New_Reference_To (RTE (RE_Address), Loc)),

          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Name_uE),
            Parameter_Type =>
              New_Reference_To (RTE (RE_Protected_Entry_Index), Loc))));
   end Build_Protected_Entry_Specification;

   --------------------------
   -- Build_Protected_Spec --
   --------------------------

   function Build_Protected_Spec
     (N           : Node_Id;
      Obj_Type    : Entity_Id;
      Unprotected : Boolean := False;
      Ident       : Entity_Id) return List_Id
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Decl        : Node_Id;
      Formal      : Entity_Id;
      New_Plist   : List_Id;
      New_Param   : Node_Id;

   begin
      New_Plist := New_List;
      Formal := First_Formal (Ident);
      while Present (Formal) loop
         New_Param :=
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Sloc (Formal), Chars (Formal)),
             In_Present => In_Present (Parent (Formal)),
             Out_Present => Out_Present (Parent (Formal)),
             Parameter_Type =>
               New_Reference_To (Etype (Formal), Loc));

         if Unprotected then
            Set_Protected_Formal (Formal, Defining_Identifier (New_Param));
         end if;

         Append (New_Param, New_Plist);
         Next_Formal (Formal);
      end loop;

      --  If the subprogram is a procedure and the context is not an access
      --  to protected subprogram, the parameter is in-out. Otherwise it is
      --  an in parameter.

      Decl :=
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uObject),
          In_Present => True,
          Out_Present =>
           (Etype (Ident) = Standard_Void_Type
              and then not Is_RTE (Obj_Type, RE_Address)),
          Parameter_Type => New_Reference_To (Obj_Type, Loc));
      Set_Needs_Debug_Info (Defining_Identifier (Decl));
      Prepend_To (New_Plist, Decl);

      return New_Plist;
   end Build_Protected_Spec;

   ---------------------------------------
   -- Build_Protected_Sub_Specification --
   ---------------------------------------

   function Build_Protected_Sub_Specification
     (N       : Node_Id;
      Prottyp : Entity_Id;
      Mode    : Subprogram_Protection_Mode) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Decl      : Node_Id;
      Ident     : Entity_Id;
      New_Id    : Entity_Id;
      New_Plist : List_Id;
      New_Spec  : Node_Id;

      Append_Chr : constant array (Subprogram_Protection_Mode) of Character :=
                     (Dispatching_Mode => ' ',
                      Protected_Mode   => 'P',
                      Unprotected_Mode => 'N');

   begin
      if Ekind
         (Defining_Unit_Name (Specification (N))) = E_Subprogram_Body
      then
         Decl := Unit_Declaration_Node (Corresponding_Spec (N));
      else
         Decl := N;
      end if;

      Ident := Defining_Unit_Name (Specification (Decl));

      New_Plist :=
        Build_Protected_Spec (Decl,
          Corresponding_Record_Type (Prottyp),
                              Mode = Unprotected_Mode, Ident);

      New_Id :=
        Make_Defining_Identifier (Loc,
          Chars => Build_Selected_Name (Prottyp, Ident, Append_Chr (Mode)));

      --  The unprotected operation carries the user code, and debugging
      --  information must be generated for it, even though this spec does
      --  not come from source. It is also convenient to allow gdb to step
      --  into the protected operation, even though it only contains lock/
      --  unlock calls.

      Set_Needs_Debug_Info (New_Id);

      if Nkind (Specification (Decl)) = N_Procedure_Specification then
         return
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name => New_Id,
             Parameter_Specifications => New_Plist);

      else
         New_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name => New_Id,
             Parameter_Specifications => New_Plist,
             Result_Definition =>
               New_Copy (Result_Definition (Specification (Decl))));
         Set_Return_Present (Defining_Unit_Name (New_Spec));
         return New_Spec;
      end if;
   end Build_Protected_Sub_Specification;

   -------------------------------------
   -- Build_Protected_Subprogram_Body --
   -------------------------------------

   function Build_Protected_Subprogram_Body
     (N         : Node_Id;
      Pid       : Node_Id;
      N_Op_Spec : Node_Id) return Node_Id
   is
      Loc          : constant Source_Ptr := Sloc (N);
      Op_Spec      : Node_Id;
      P_Op_Spec    : Node_Id;
      Uactuals     : List_Id;
      Pformal      : Node_Id;
      Unprot_Call  : Node_Id;
      Sub_Body     : Node_Id;
      Lock_Name    : Node_Id;
      Lock_Stmt    : Node_Id;
      Service_Name : Node_Id;
      R            : Node_Id;
      Return_Stmt  : Node_Id := Empty;    -- init to avoid gcc 3 warning
      Pre_Stmts    : List_Id := No_List;  -- init to avoid gcc 3 warning
      Stmts        : List_Id;
      Object_Parm  : Node_Id;
      Exc_Safe     : Boolean;

      function Is_Exception_Safe (Subprogram : Node_Id) return Boolean;
      --  Tell whether a given subprogram cannot raise an exception

      -----------------------
      -- Is_Exception_Safe --
      -----------------------

      function Is_Exception_Safe (Subprogram : Node_Id) return Boolean is

         function Has_Side_Effect (N : Node_Id) return Boolean;
         --  Return True whenever encountering a subprogram call or raise
         --  statement of any kind in the sequence of statements

         ---------------------
         -- Has_Side_Effect --
         ---------------------

         --  What is this doing buried two levels down in exp_ch9. It seems
         --  like a generally useful function, and indeed there may be code
         --  duplication going on here ???

         function Has_Side_Effect (N : Node_Id) return Boolean is
            Stmt : Node_Id;
            Expr : Node_Id;

            function Is_Call_Or_Raise (N : Node_Id) return Boolean;
            --  Indicate whether N is a subprogram call or a raise statement

            ----------------------
            -- Is_Call_Or_Raise --
            ----------------------

            function Is_Call_Or_Raise (N : Node_Id) return Boolean is
            begin
               return Nkind (N) = N_Procedure_Call_Statement
                 or else Nkind (N) = N_Function_Call
                 or else Nkind (N) = N_Raise_Statement
                 or else Nkind (N) = N_Raise_Constraint_Error
                 or else Nkind (N) = N_Raise_Program_Error
                 or else Nkind (N) = N_Raise_Storage_Error;
            end Is_Call_Or_Raise;

         --  Start of processing for Has_Side_Effect

         begin
            Stmt := N;
            while Present (Stmt) loop
               if Is_Call_Or_Raise (Stmt) then
                  return True;
               end if;

               --  An object declaration can also contain a function call
               --  or a raise statement

               if Nkind (Stmt) = N_Object_Declaration then
                  Expr := Expression (Stmt);

                  if Present (Expr) and then Is_Call_Or_Raise (Expr) then
                     return True;
                  end if;
               end if;

               Next (Stmt);
            end loop;

            return False;
         end Has_Side_Effect;

      --  Start of processing for Is_Exception_Safe

      begin
         --  If the checks handled by the back end are not disabled, we cannot
         --  ensure that no exception will be raised.

         if not Access_Checks_Suppressed (Empty)
           or else not Discriminant_Checks_Suppressed (Empty)
           or else not Range_Checks_Suppressed (Empty)
           or else not Index_Checks_Suppressed (Empty)
           or else Opt.Stack_Checking_Enabled
         then
            return False;
         end if;

         if Has_Side_Effect (First (Declarations (Subprogram)))
           or else
              Has_Side_Effect (
                First (Statements (Handled_Statement_Sequence (Subprogram))))
         then
            return False;
         else
            return True;
         end if;
      end Is_Exception_Safe;

   --  Start of processing for Build_Protected_Subprogram_Body

   begin
      Op_Spec := Specification (N);
      Exc_Safe := Is_Exception_Safe (N);

      P_Op_Spec :=
        Build_Protected_Sub_Specification (N, Pid, Protected_Mode);

      --  Build a list of the formal parameters of the protected version of
      --  the subprogram to use as the actual parameters of the unprotected
      --  version.

      Uactuals := New_List;
      Pformal := First (Parameter_Specifications (P_Op_Spec));
      while Present (Pformal) loop
         Append (
           Make_Identifier (Loc, Chars (Defining_Identifier (Pformal))),
           Uactuals);
         Next (Pformal);
      end loop;

      --  Make a call to the unprotected version of the subprogram built above
      --  for use by the protected version built below.

      if Nkind (Op_Spec) = N_Function_Specification then
         if Exc_Safe then
            R := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
            Unprot_Call :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => R,
                Constant_Present => True,
                Object_Definition => New_Copy (Result_Definition (N_Op_Spec)),
                Expression =>
                  Make_Function_Call (Loc,
                    Name => Make_Identifier (Loc,
                      Chars (Defining_Unit_Name (N_Op_Spec))),
                    Parameter_Associations => Uactuals));
            Return_Stmt := Make_Return_Statement (Loc,
              Expression => New_Reference_To (R, Loc));

         else
            Unprot_Call := Make_Return_Statement (Loc,
              Expression => Make_Function_Call (Loc,
                Name =>
                  Make_Identifier (Loc,
                    Chars (Defining_Unit_Name (N_Op_Spec))),
                Parameter_Associations => Uactuals));
         end if;

      else
         Unprot_Call := Make_Procedure_Call_Statement (Loc,
           Name =>
             Make_Identifier (Loc,
               Chars (Defining_Unit_Name (N_Op_Spec))),
           Parameter_Associations => Uactuals);
      end if;

      --  Wrap call in block that will be covered by an at_end handler

      if not Exc_Safe then
         Unprot_Call := Make_Block_Statement (Loc,
           Handled_Statement_Sequence =>
             Make_Handled_Sequence_Of_Statements (Loc,
               Statements => New_List (Unprot_Call)));
      end if;

      --  Make the protected subprogram body. This locks the protected
      --  object and calls the unprotected version of the subprogram.

      --  If the protected object is controlled (i.e it has entries or
      --  needs finalization for interrupt handling), call Lock_Entries,
      --  except if the protected object follows the Ravenscar profile, in
      --  which case call Lock_Entry, otherwise call the simplified version,
      --  Lock.

      if Has_Entries (Pid)
        or else Has_Interrupt_Handler (Pid)
        or else (Has_Attach_Handler (Pid)
                  and then not Restricted_Profile)
        or else (Ada_Version >= Ada_05
                  and then Present (Interface_List (Parent (Pid))))
      then
         if Abort_Allowed
           or else Restriction_Active (No_Entry_Queue) = False
           or else Number_Entries (Pid) > 1
         then
            Lock_Name := New_Reference_To (RTE (RE_Lock_Entries), Loc);
            Service_Name := New_Reference_To (RTE (RE_Service_Entries), Loc);

         else
            Lock_Name := New_Reference_To (RTE (RE_Lock_Entry), Loc);
            Service_Name := New_Reference_To (RTE (RE_Service_Entry), Loc);
         end if;

      else
         Lock_Name := New_Reference_To (RTE (RE_Lock), Loc);
         Service_Name := New_Reference_To (RTE (RE_Unlock), Loc);
      end if;

      Object_Parm :=
        Make_Attribute_Reference (Loc,
           Prefix =>
             Make_Selected_Component (Loc,
               Prefix =>
                 Make_Identifier (Loc, Name_uObject),
             Selector_Name =>
                 Make_Identifier (Loc, Name_uObject)),
           Attribute_Name => Name_Unchecked_Access);

      Lock_Stmt := Make_Procedure_Call_Statement (Loc,
        Name => Lock_Name,
        Parameter_Associations => New_List (Object_Parm));

      if Abort_Allowed then
         Stmts := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Abort_Defer), Loc),
             Parameter_Associations => Empty_List),
           Lock_Stmt);

      else
         Stmts := New_List (Lock_Stmt);
      end if;

      if not Exc_Safe then
         Append (Unprot_Call, Stmts);
      else
         if Nkind (Op_Spec) = N_Function_Specification then
            Pre_Stmts := Stmts;
            Stmts     := Empty_List;
         else
            Append (Unprot_Call, Stmts);
         end if;

         Append (
           Make_Procedure_Call_Statement (Loc,
             Name => Service_Name,
             Parameter_Associations =>
               New_List (New_Copy_Tree (Object_Parm))),
           Stmts);

         if Abort_Allowed then
            Append (
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Abort_Undefer), Loc),
                Parameter_Associations => Empty_List),
              Stmts);
         end if;

         if Nkind (Op_Spec) = N_Function_Specification then
            Append (Return_Stmt, Stmts);
            Append (Make_Block_Statement (Loc,
              Declarations => New_List (Unprot_Call),
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => Stmts)), Pre_Stmts);
            Stmts := Pre_Stmts;
         end if;
      end if;

      Sub_Body :=
        Make_Subprogram_Body (Loc,
          Declarations => Empty_List,
          Specification => P_Op_Spec,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Stmts));

      if not Exc_Safe then
         Set_Is_Protected_Subprogram_Body (Sub_Body);
      end if;

      return Sub_Body;
   end Build_Protected_Subprogram_Body;

   -------------------------------------
   -- Build_Protected_Subprogram_Call --
   -------------------------------------

   procedure Build_Protected_Subprogram_Call
     (N        : Node_Id;
      Name     : Node_Id;
      Rec      : Node_Id;
      External : Boolean := True)
   is
      Loc     : constant Source_Ptr := Sloc (N);
      Sub     : constant Entity_Id  := Entity (Name);
      New_Sub : Node_Id;
      Params  : List_Id;

   begin
      if External then
         New_Sub := New_Occurrence_Of (External_Subprogram (Sub), Loc);
      else
         New_Sub :=
           New_Occurrence_Of (Protected_Body_Subprogram (Sub), Loc);
      end if;

      if Present (Parameter_Associations (N)) then
         Params := New_Copy_List_Tree (Parameter_Associations (N));
      else
         Params := New_List;
      end if;

      Prepend (Rec, Params);

      if Ekind (Sub) = E_Procedure then
         Rewrite (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Sub,
             Parameter_Associations => Params));

      else
         pragma Assert (Ekind (Sub) = E_Function);
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Sub,
             Parameter_Associations => Params));
      end if;

      if External
        and then Nkind (Rec) = N_Unchecked_Type_Conversion
        and then Is_Entity_Name (Expression (Rec))
        and then Is_Shared_Passive (Entity (Expression (Rec)))
      then
         Add_Shared_Var_Lock_Procs (N);
      end if;
   end Build_Protected_Subprogram_Call;

   -------------------------
   -- Build_Selected_Name --
   -------------------------

   function Build_Selected_Name
     (Prefix      : Entity_Id;
      Selector    : Entity_Id;
      Append_Char : Character := ' ') return Name_Id
   is
      Select_Buffer : String (1 .. Hostparm.Max_Name_Length);
      Select_Len    : Natural;

   begin
      Get_Name_String (Chars (Selector));
      Select_Len := Name_Len;
      Select_Buffer (1 .. Select_Len) := Name_Buffer (1 .. Name_Len);
      Get_Name_String (Chars (Prefix));

      --  If scope is anonymous type, discard suffix to recover name of
      --  single protected object. Otherwise use protected type name.

      if Name_Buffer (Name_Len) = 'T' then
         Name_Len := Name_Len - 1;
      end if;

      Name_Buffer (Name_Len + 1) := '_';
      Name_Buffer (Name_Len + 2) := '_';

      Name_Len := Name_Len + 2;
      for J in 1 .. Select_Len loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Select_Buffer (J);
      end loop;

      --  Now add the Append_Char if specified. The encoding to follow
      --  depends on the type of entity. If Append_Char is either 'N' or 'P',
      --  then the entity is associated to a protected type subprogram.
      --  Otherwise, it is a protected type entry. For each case, the
      --  encoding to follow for the suffix is documented in exp_dbug.ads.

      --  It would be better to encapsulate this as a routine in Exp_Dbug ???

      if Append_Char /= ' ' then
         if Append_Char = 'P' or Append_Char = 'N' then
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Append_Char;
            return Name_Find;
         else
            Name_Buffer (Name_Len + 1) := '_';
            Name_Buffer (Name_Len + 2) := Append_Char;
            Name_Len := Name_Len + 2;
            return New_External_Name (Name_Find, ' ', -1);
         end if;
      else
         return Name_Find;
      end if;
   end Build_Selected_Name;

   -----------------------------
   -- Build_Simple_Entry_Call --
   -----------------------------

   --  A task entry call is converted to a call to Call_Simple

   --    declare
   --       P : parms := (parm, parm, parm);
   --    begin
   --       Call_Simple (acceptor-task, entry-index, P'Address);
   --       parm := P.param;
   --       parm := P.param;
   --       ...
   --    end;

   --  Here Pnn is an aggregate of the type constructed for the entry to hold
   --  the parameters, and the constructed aggregate value contains either the
   --  parameters or, in the case of non-elementary types, references to these
   --  parameters. Then the address of this aggregate is passed to the runtime
   --  routine, along with the task id value and the task entry index value.
   --  Pnn is only required if parameters are present.

   --  The assignments after the call are present only in the case of in-out
   --  or out parameters for elementary types, and are used to assign back the
   --  resulting values of such parameters.

   --  Note: the reason that we insert a block here is that in the context
   --  of selects, conditional entry calls etc. the entry call statement
   --  appears on its own, not as an element of a list.

   --  A protected entry call is converted to a Protected_Entry_Call:

   --  declare
   --     P   : E1_Params := (param, param, param);
   --     Pnn : Boolean;
   --     Bnn : Communications_Block;

   --  declare
   --     P   : E1_Params := (param, param, param);
   --     Bnn : Communications_Block;

   --  begin
   --     Protected_Entry_Call (
   --       Object => po._object'Access,
   --       E => <entry index>;
   --       Uninterpreted_Data => P'Address;
   --       Mode => Simple_Call;
   --       Block => Bnn);
   --     parm := P.param;
   --     parm := P.param;
   --       ...
   --  end;

   procedure Build_Simple_Entry_Call
     (N       : Node_Id;
      Concval : Node_Id;
      Ename   : Node_Id;
      Index   : Node_Id)
   is
   begin
      Expand_Call (N);

      --  Convert entry call to Call_Simple call

      declare
         Loc       : constant Source_Ptr := Sloc (N);
         Parms     : constant List_Id    := Parameter_Associations (N);
         Stats     : constant List_Id    := New_List;
         Actual    : Node_Id;
         Call      : Node_Id;
         Comm_Name : Entity_Id;
         Conctyp   : Node_Id;
         Decls     : List_Id;
         Ent       : Entity_Id;
         Ent_Acc   : Entity_Id;
         Formal    : Node_Id;
         Iface_Tag : Entity_Id;
         Iface_Typ : Entity_Id;
         N_Node    : Node_Id;
         N_Var     : Node_Id;
         P         : Entity_Id;
         Parm1     : Node_Id;
         Parm2     : Node_Id;
         Parm3     : Node_Id;
         Pdecl     : Node_Id;
         Plist     : List_Id;
         X         : Entity_Id;
         Xdecl     : Node_Id;

      begin
         --  Simple entry and entry family cases merge here

         Ent     := Entity (Ename);
         Ent_Acc := Entry_Parameters_Type (Ent);
         Conctyp := Etype (Concval);

         --  If prefix is an access type, dereference to obtain the task type

         if Is_Access_Type (Conctyp) then
            Conctyp := Designated_Type (Conctyp);
         end if;

         --  Special case for protected subprogram calls

         if Is_Protected_Type (Conctyp)
           and then Is_Subprogram (Entity (Ename))
         then
            if not Is_Eliminated (Entity (Ename)) then
               Build_Protected_Subprogram_Call
                 (N, Ename, Convert_Concurrent (Concval, Conctyp));
               Analyze (N);
            end if;

            return;
         end if;

         --  First parameter is the Task_Id value from the task value or the
         --  Object from the protected object value, obtained by selecting
         --  the _Task_Id or _Object from the result of doing an unchecked
         --  conversion to convert the value to the corresponding record type.

         Parm1 := Concurrent_Ref (Concval);

         --  Second parameter is the entry index, computed by the routine
         --  provided for this purpose. The value of this expression is
         --  assigned to an intermediate variable to assure that any entry
         --  family index expressions are evaluated before the entry
         --  parameters.

         if Abort_Allowed
           or else Restriction_Active (No_Entry_Queue) = False
           or else not Is_Protected_Type (Conctyp)
           or else Number_Entries (Conctyp) > 1
         then
            X := Make_Defining_Identifier (Loc, Name_uX);

            Xdecl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => X,
                Object_Definition =>
                  New_Reference_To (RTE (RE_Task_Entry_Index), Loc),
                Expression => Actual_Index_Expression (
                  Loc, Entity (Ename), Index, Concval));

            Decls := New_List (Xdecl);
            Parm2 := New_Reference_To (X, Loc);

         else
            Xdecl := Empty;
            Decls := New_List;
            Parm2 := Empty;
         end if;

         --  The third parameter is the packaged parameters. If there are
         --  none, then it is just the null address, since nothing is passed.

         if No (Parms) then
            Parm3 := New_Reference_To (RTE (RE_Null_Address), Loc);
            P := Empty;

         --  Case of parameters present, where third argument is the address
         --  of a packaged record containing the required parameter values.

         else
            --  First build a list of parameter values, which are references to
            --  objects of the parameter types.

            Plist := New_List;

            Actual := First_Actual (N);
            Formal := First_Formal (Ent);

            while Present (Actual) loop

               --  If it is a by_copy_type, copy it to a new variable. The
               --  packaged record has a field that points to this variable.

               if Is_By_Copy_Type (Etype (Actual)) then
                  N_Node :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars => New_Internal_Name ('J')),
                      Aliased_Present => True,
                      Object_Definition =>
                        New_Reference_To (Etype (Formal), Loc));

                  --  We have to make an assignment statement separate for the
                  --  case of limited type. We cannot assign it unless the
                  --  Assignment_OK flag is set first.

                  if Ekind (Formal) /= E_Out_Parameter then
                     N_Var :=
                       New_Reference_To (Defining_Identifier (N_Node), Loc);
                     Set_Assignment_OK (N_Var);
                     Append_To (Stats,
                       Make_Assignment_Statement (Loc,
                         Name => N_Var,
                         Expression => Relocate_Node (Actual)));
                  end if;

                  Append (N_Node, Decls);

                  Append_To (Plist,
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Unchecked_Access,
                    Prefix =>
                      New_Reference_To (Defining_Identifier (N_Node), Loc)));
               else
                  --  Interface class-wide formal

                  if Ada_Version >= Ada_05
                    and then Ekind (Etype (Formal)) = E_Class_Wide_Type
                    and then Is_Interface (Etype (Formal))
                  then
                     Iface_Typ := Etype (Etype (Formal));

                     --  Generate:
                     --    formal_iface_type! (actual.iface_tag)'reference

                     Iface_Tag :=
                       Find_Interface_Tag (Etype (Actual), Iface_Typ);
                     pragma Assert (Present (Iface_Tag));

                     Append_To (Plist,
                       Make_Reference (Loc,
                         Unchecked_Convert_To (Iface_Typ,
                           Make_Selected_Component (Loc,
                             Prefix =>
                               Relocate_Node (Actual),
                             Selector_Name =>
                               New_Reference_To (Iface_Tag, Loc)))));
                  else
                     --  Generate:
                     --    actual'reference

                     Append_To (Plist,
                       Make_Reference (Loc, Relocate_Node (Actual)));
                  end if;
               end if;

               Next_Actual (Actual);
               Next_Formal_With_Extras (Formal);
            end loop;

            --  Now build the declaration of parameters initialized with the
            --  aggregate containing this constructed parameter list.

            P := Make_Defining_Identifier (Loc, Name_uP);

            Pdecl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => P,
                Object_Definition =>
                  New_Reference_To (Designated_Type (Ent_Acc), Loc),
                Expression =>
                  Make_Aggregate (Loc, Expressions => Plist));

            Parm3 :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Address,
                Prefix => New_Reference_To (P, Loc));

            Append (Pdecl, Decls);
         end if;

         --  Now we can create the call, case of protected type

         if Is_Protected_Type (Conctyp) then
            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Conctyp) > 1
            then
               --  Change the type of the index declaration

               Set_Object_Definition (Xdecl,
                 New_Reference_To (RTE (RE_Protected_Entry_Index), Loc));

               --  Some additional declarations for protected entry calls

               if No (Decls) then
                  Decls := New_List;
               end if;

               --  Bnn : Communications_Block;

               Comm_Name :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('B'));

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Comm_Name,
                   Object_Definition =>
                     New_Reference_To (RTE (RE_Communication_Block), Loc)));

               --  Some additional statements for protected entry calls

               --     Protected_Entry_Call (
               --       Object => po._object'Access,
               --       E => <entry index>;
               --       Uninterpreted_Data => P'Address;
               --       Mode => Simple_Call;
               --       Block => Bnn);

               Call :=
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Reference_To (RTE (RE_Protected_Entry_Call), Loc),

                   Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Attribute_Name => Name_Unchecked_Access,
                       Prefix         => Parm1),
                     Parm2,
                     Parm3,
                     New_Reference_To (RTE (RE_Simple_Call), Loc),
                     New_Occurrence_Of (Comm_Name, Loc)));

            else
               --     Protected_Single_Entry_Call (
               --       Object => po._object'Access,
               --       Uninterpreted_Data => P'Address;
               --       Mode => Simple_Call);

               Call :=
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (
                     RTE (RE_Protected_Single_Entry_Call), Loc),

                   Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Attribute_Name => Name_Unchecked_Access,
                       Prefix         => Parm1),
                     Parm3,
                     New_Reference_To (RTE (RE_Simple_Call), Loc)));
            end if;

         --  Case of task type

         else
            Call :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Call_Simple), Loc),
                Parameter_Associations => New_List (Parm1, Parm2, Parm3));

         end if;

         Append_To (Stats, Call);

         --  If there are out or in/out parameters by copy add assignment
         --  statements for the result values.

         if Present (Parms) then
            Actual := First_Actual (N);
            Formal := First_Formal (Ent);

            Set_Assignment_OK (Actual);
            while Present (Actual) loop
               if Is_By_Copy_Type (Etype (Actual))
                 and then Ekind (Formal) /= E_In_Parameter
               then
                  N_Node :=
                    Make_Assignment_Statement (Loc,
                      Name => New_Copy (Actual),
                      Expression =>
                        Make_Explicit_Dereference (Loc,
                          Make_Selected_Component (Loc,
                            Prefix => New_Reference_To (P, Loc),
                            Selector_Name =>
                              Make_Identifier (Loc, Chars (Formal)))));

                  --  In all cases (including limited private types) we want
                  --  the assignment to be valid.

                  Set_Assignment_OK (Name (N_Node));

                  --  If the call is the triggering alternative in an
                  --  asynchronous select, or the entry_call alternative of a
                  --  conditional entry call, the assignments for in-out
                  --  parameters are incorporated into the statement list that
                  --  follows, so that there are executed only if the entry
                  --  call succeeds.

                  if (Nkind (Parent (N)) = N_Triggering_Alternative
                       and then N = Triggering_Statement (Parent (N)))
                    or else
                     (Nkind (Parent (N)) = N_Entry_Call_Alternative
                       and then N = Entry_Call_Statement (Parent (N)))
                  then
                     if No (Statements (Parent (N))) then
                        Set_Statements (Parent (N), New_List);
                     end if;

                     Prepend (N_Node, Statements (Parent (N)));

                  else
                     Insert_After (Call, N_Node);
                  end if;
               end if;

               Next_Actual (Actual);
               Next_Formal_With_Extras (Formal);
            end loop;
         end if;

         --  Finally, create block and analyze it

         Rewrite (N,
           Make_Block_Statement (Loc,
             Declarations => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stats)));

         Analyze (N);
      end;
   end Build_Simple_Entry_Call;

   --------------------------------
   -- Build_Task_Activation_Call --
   --------------------------------

   procedure Build_Task_Activation_Call (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Chain      : Entity_Id;
      Call       : Node_Id;
      Name       : Node_Id;
      P          : Node_Id;

   begin
      --  Get the activation chain entity. Except in the case of a package
      --  body, this is in the node that w as passed. For a package body, we
      --  have to find the corresponding package declaration node.

      if Nkind (N) = N_Package_Body then
         P := Corresponding_Spec (N);

         loop
            P := Parent (P);
            exit when Nkind (P) = N_Package_Declaration;
         end loop;

         Chain := Activation_Chain_Entity (P);

      else
         Chain := Activation_Chain_Entity (N);
      end if;

      if Present (Chain) then
         if Restricted_Profile then
            Name := New_Reference_To (RTE (RE_Activate_Restricted_Tasks), Loc);
         else
            Name := New_Reference_To (RTE (RE_Activate_Tasks), Loc);
         end if;

         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => Name,
             Parameter_Associations =>
               New_List (Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Chain, Loc),
                 Attribute_Name => Name_Unchecked_Access)));

         if Nkind (N) = N_Package_Declaration then
            if Present (Corresponding_Body (N)) then
               null;

            elsif Present (Private_Declarations (Specification (N))) then
               Append (Call, Private_Declarations (Specification (N)));

            else
               Append (Call, Visible_Declarations (Specification (N)));
            end if;

         else
            if Present (Handled_Statement_Sequence (N)) then

               --  The call goes at the start of the statement sequence, but
               --  after the start of exception range label if one is present.

               declare
                  Stm : Node_Id;

               begin
                  Stm := First (Statements (Handled_Statement_Sequence (N)));

                  if Nkind (Stm) = N_Label and then Exception_Junk (Stm) then
                     Next (Stm);
                  end if;

                  Insert_Before (Stm, Call);
               end;

            else
               Set_Handled_Statement_Sequence (N,
                  Make_Handled_Sequence_Of_Statements (Loc,
                     Statements => New_List (Call)));
            end if;
         end if;

         Analyze (Call);
         Check_Task_Activation (N);
      end if;
   end Build_Task_Activation_Call;

   -------------------------------
   -- Build_Task_Allocate_Block --
   -------------------------------

   procedure Build_Task_Allocate_Block
     (Actions : List_Id;
      N       : Node_Id;
      Args    : List_Id)
   is
      T      : constant Entity_Id  := Entity (Expression (N));
      Init   : constant Entity_Id  := Base_Init_Proc (T);
      Loc    : constant Source_Ptr := Sloc (N);
      Chain  : constant Entity_Id  :=
                 Make_Defining_Identifier (Loc, Name_uChain);

      Blkent : Entity_Id;
      Block  : Node_Id;

   begin
      Blkent := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

      Block :=
        Make_Block_Statement (Loc,
          Identifier => New_Reference_To (Blkent, Loc),
          Declarations => New_List (

            --  _Chain  : Activation_Chain;

            Make_Object_Declaration (Loc,
              Defining_Identifier => Chain,
              Aliased_Present => True,
              Object_Definition   =>
                New_Reference_To (RTE (RE_Activation_Chain), Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,

              Statements => New_List (

               --  Init (Args);

                Make_Procedure_Call_Statement (Loc,
                  Name => New_Reference_To (Init, Loc),
                  Parameter_Associations => Args),

               --  Activate_Tasks (_Chain);

                Make_Procedure_Call_Statement (Loc,
                  Name => New_Reference_To (RTE (RE_Activate_Tasks), Loc),
                  Parameter_Associations => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Chain, Loc),
                      Attribute_Name => Name_Unchecked_Access))))),

          Has_Created_Identifier => True,
          Is_Task_Allocation_Block => True);

      Append_To (Actions,
        Make_Implicit_Label_Declaration (Loc,
          Defining_Identifier => Blkent,
          Label_Construct     => Block));

      Append_To (Actions, Block);

      Set_Activation_Chain_Entity (Block, Chain);
   end Build_Task_Allocate_Block;

   -----------------------------------------------
   -- Build_Task_Allocate_Block_With_Init_Stmts --
   -----------------------------------------------

   procedure Build_Task_Allocate_Block_With_Init_Stmts
     (Actions    : List_Id;
      N          : Node_Id;
      Init_Stmts : List_Id)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Chain  : constant Entity_Id  :=
                 Make_Defining_Identifier (Loc, Name_uChain);
      Blkent : Entity_Id;
      Block  : Node_Id;

   begin
      Blkent := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

      Append_To (Init_Stmts,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Activate_Tasks), Loc),
          Parameter_Associations => New_List (
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Chain, Loc),
              Attribute_Name => Name_Unchecked_Access))));

      Block :=
        Make_Block_Statement (Loc,
          Identifier => New_Reference_To (Blkent, Loc),
          Declarations => New_List (

            --  _Chain  : Activation_Chain;

            Make_Object_Declaration (Loc,
              Defining_Identifier => Chain,
              Aliased_Present => True,
              Object_Definition   =>
                New_Reference_To (RTE (RE_Activation_Chain), Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Init_Stmts),

          Has_Created_Identifier => True,
          Is_Task_Allocation_Block => True);

      Append_To (Actions,
        Make_Implicit_Label_Declaration (Loc,
          Defining_Identifier => Blkent,
          Label_Construct     => Block));

      Append_To (Actions, Block);

      Set_Activation_Chain_Entity (Block, Chain);
   end Build_Task_Allocate_Block_With_Init_Stmts;

   -----------------------------------
   -- Build_Task_Proc_Specification --
   -----------------------------------

   function Build_Task_Proc_Specification (T : Entity_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (T);
      Nam : constant Name_Id    := Chars (T);
      Ent : Entity_Id;

   begin
      Ent :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Nam, 'B'));
      Set_Is_Internal (Ent);

      --  Associate the procedure with the task, if this is the declaration
      --  (and not the body) of the procedure.

      if No (Task_Body_Procedure (T)) then
         Set_Task_Body_Procedure (T, Ent);
      end if;

      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => Ent,
          Parameter_Specifications =>
            New_List (
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uTask),
                Parameter_Type =>
                  Make_Access_Definition (Loc,
                    Subtype_Mark =>
                      New_Reference_To
                        (Corresponding_Record_Type (T), Loc)))));
   end Build_Task_Proc_Specification;

   ---------------------------------------
   -- Build_Unprotected_Subprogram_Body --
   ---------------------------------------

   function Build_Unprotected_Subprogram_Body
     (N   : Node_Id;
      Pid : Node_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (N);
      N_Op_Spec : Node_Id;
      Op_Decls  : List_Id;

   begin
      --  Make an unprotected version of the subprogram for use within the same
      --  object, with a new name and an additional parameter representing the
      --  object.

      Op_Decls := Declarations (N);
      N_Op_Spec :=
        Build_Protected_Sub_Specification (N, Pid, Unprotected_Mode);

      return
        Make_Subprogram_Body (Loc,
          Specification => N_Op_Spec,
          Declarations => Op_Decls,
          Handled_Statement_Sequence =>
            Handled_Statement_Sequence (N));
   end Build_Unprotected_Subprogram_Body;

   ----------------------------
   -- Collect_Entry_Families --
   ----------------------------

   procedure Collect_Entry_Families
     (Loc          : Source_Ptr;
      Cdecls       : List_Id;
      Current_Node : in out Node_Id;
      Conctyp      : Entity_Id)
   is
      Efam      : Entity_Id;
      Efam_Decl : Node_Id;
      Efam_Type : Entity_Id;

   begin
      Efam := First_Entity (Conctyp);

      while Present (Efam) loop

         if Ekind (Efam) = E_Entry_Family then
            Efam_Type :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('F'));

            declare
               Bas : Entity_Id :=
                       Base_Type
                        (Etype (Discrete_Subtype_Definition (Parent (Efam))));
               Bas_Decl : Node_Id := Empty;
               Lo, Hi   : Node_Id;

            begin
               Get_Index_Bounds
                 (Discrete_Subtype_Definition (Parent (Efam)), Lo, Hi);
               if Scope (Bas) = Standard_Standard
                 and then Bas = Base_Type (Standard_Integer)
                 and then Has_Discriminants (Conctyp)
                 and then Present
                   (Discriminant_Default_Value (First_Discriminant (Conctyp)))
                 and then
                   (Denotes_Discriminant (Lo, True)
                     or else Denotes_Discriminant (Hi, True))
               then
                  Bas :=
                    Make_Defining_Identifier (Loc, New_Internal_Name ('B'));
                  Bas_Decl :=
                    Make_Subtype_Declaration (Loc,
                       Defining_Identifier => Bas,
                       Subtype_Indication =>
                         Make_Subtype_Indication (Loc,
                           Subtype_Mark =>
                             New_Occurrence_Of (Standard_Integer, Loc),
                           Constraint =>
                             Make_Range_Constraint (Loc,
                               Range_Expression => Make_Range (Loc,
                                 Make_Integer_Literal
                                   (Loc, -Entry_Family_Bound),
                                 Make_Integer_Literal
                                   (Loc, Entry_Family_Bound - 1)))));

                  Insert_After (Current_Node, Bas_Decl);
                  Current_Node := Bas_Decl;
                  Analyze (Bas_Decl);
               end if;

               Efam_Decl :=
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Efam_Type,
                   Type_Definition =>
                     Make_Unconstrained_Array_Definition (Loc,
                       Subtype_Marks =>
                         (New_List (New_Occurrence_Of (Bas, Loc))),

                    Component_Definition =>
                      Make_Component_Definition (Loc,
                        Aliased_Present    => False,
                        Subtype_Indication =>
                          New_Reference_To (Standard_Character, Loc))));
            end;

            Insert_After (Current_Node, Efam_Decl);
            Current_Node := Efam_Decl;
            Analyze (Efam_Decl);

            Append_To (Cdecls,
              Make_Component_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Chars (Efam)),

                Component_Definition =>
                  Make_Component_Definition (Loc,
                    Aliased_Present    => False,
                    Subtype_Indication =>
                      Make_Subtype_Indication (Loc,
                        Subtype_Mark =>
                          New_Occurrence_Of (Efam_Type, Loc),

                        Constraint  =>
                          Make_Index_Or_Discriminant_Constraint (Loc,
                            Constraints => New_List (
                              New_Occurrence_Of
                                (Etype (Discrete_Subtype_Definition
                                  (Parent (Efam))), Loc)))))));

         end if;

         Next_Entity (Efam);
      end loop;
   end Collect_Entry_Families;

   --------------------
   -- Concurrent_Ref --
   --------------------

   --  The expression returned for a reference to a concurrent object has the
   --  form:

   --    taskV!(name)._Task_Id

   --  for a task, and

   --    objectV!(name)._Object

   --  for a protected object. For the case of an access to a concurrent
   --  object, there is an extra explicit dereference:

   --    taskV!(name.all)._Task_Id
   --    objectV!(name.all)._Object

   --  here taskV and objectV are the types for the associated records, which
   --  contain the required _Task_Id and _Object fields for tasks and protected
   --  objects, respectively.

   --  For the case of a task type name, the expression is

   --    Self;

   --  i.e. a call to the Self function which returns precisely this Task_Id

   --  For the case of a protected type name, the expression is

   --    objectR

   --  which is a renaming of the _object field of the current object object
   --  record, passed into protected operations as a parameter.

   function Concurrent_Ref (N : Node_Id) return Node_Id is
      Loc  : constant Source_Ptr := Sloc (N);
      Ntyp : constant Entity_Id  := Etype (N);
      Dtyp : Entity_Id;
      Sel  : Name_Id;

      function Is_Current_Task (T : Entity_Id) return Boolean;
      --  Check whether the reference is to the immediately enclosing task
      --  type, or to an outer one (rare but legal).

      ---------------------
      -- Is_Current_Task --
      ---------------------

      function Is_Current_Task (T : Entity_Id) return Boolean is
         Scop : Entity_Id;

      begin
         Scop := Current_Scope;
         while Present (Scop)
           and then Scop /= Standard_Standard
         loop

            if Scop = T then
               return True;

            elsif Is_Task_Type (Scop) then
               return False;

            --  If this is a procedure nested within the task type, we must
            --  assume that it can be called from an inner task, and therefore
            --  cannot treat it as a local reference.

            elsif Is_Overloadable (Scop)
              and then In_Open_Scopes (T)
            then
               return False;

            else
               Scop := Scope (Scop);
            end if;
         end loop;

         --  We know that we are within the task body, so should have found it
         --  in scope.

         raise Program_Error;
      end Is_Current_Task;

   --  Start of processing for Concurrent_Ref

   begin
      if Is_Access_Type (Ntyp) then
         Dtyp := Designated_Type (Ntyp);

         if Is_Protected_Type (Dtyp) then
            Sel := Name_uObject;
         else
            Sel := Name_uTask_Id;
         end if;

         return
           Make_Selected_Component (Loc,
             Prefix =>
               Unchecked_Convert_To (Corresponding_Record_Type (Dtyp),
                 Make_Explicit_Dereference (Loc, N)),
             Selector_Name => Make_Identifier (Loc, Sel));

      elsif Is_Entity_Name (N)
        and then Is_Concurrent_Type (Entity (N))
      then
         if Is_Task_Type (Entity (N)) then

            if Is_Current_Task (Entity (N)) then
               return
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (RTE (RE_Self), Loc));

            else
               declare
                  Decl   : Node_Id;
                  T_Self : constant Entity_Id :=
                             Make_Defining_Identifier (Loc,
                               Chars => New_Internal_Name ('T'));
                  T_Body : constant Node_Id :=
                             Parent (Corresponding_Body (Parent (Entity (N))));

               begin
                  Decl := Make_Object_Declaration (Loc,
                     Defining_Identifier => T_Self,
                     Object_Definition =>
                       New_Occurrence_Of (RTE (RO_ST_Task_Id), Loc),
                     Expression =>
                       Make_Function_Call (Loc,
                         Name => New_Reference_To (RTE (RE_Self), Loc)));
                  Prepend (Decl, Declarations (T_Body));
                  Analyze (Decl);
                  Set_Scope (T_Self, Entity (N));
                  return New_Occurrence_Of (T_Self,  Loc);
               end;
            end if;

         else
            pragma Assert (Is_Protected_Type (Entity (N)));
            return
              New_Reference_To (
                Object_Ref (Corresponding_Body (Parent (Base_Type (Ntyp)))),
                Loc);
         end if;

      else
         pragma Assert (Is_Concurrent_Type (Ntyp));

         if Is_Protected_Type (Ntyp) then
            Sel := Name_uObject;
         else
            Sel := Name_uTask_Id;
         end if;

         return
           Make_Selected_Component (Loc,
             Prefix =>
               Unchecked_Convert_To (Corresponding_Record_Type (Ntyp),
                 New_Copy_Tree (N)),
             Selector_Name => Make_Identifier (Loc, Sel));
      end if;
   end Concurrent_Ref;

   ------------------------
   -- Convert_Concurrent --
   ------------------------

   function Convert_Concurrent
     (N   : Node_Id;
      Typ : Entity_Id) return Node_Id
   is
   begin
      if not Is_Concurrent_Type (Typ) then
         return N;
      else
         return
           Unchecked_Convert_To (Corresponding_Record_Type (Typ),
             New_Copy_Tree (N));
      end if;
   end Convert_Concurrent;

   ----------------------------
   -- Entry_Index_Expression --
   ----------------------------

   function Entry_Index_Expression
     (Sloc  : Source_Ptr;
      Ent   : Entity_Id;
      Index : Node_Id;
      Ttyp  : Entity_Id) return Node_Id
   is
      Expr : Node_Id;
      Num  : Node_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      Prev : Entity_Id;
      S    : Node_Id;

   begin
      --  The queues of entries and entry families appear in textual order in
      --  the associated record. The entry index is computed as the sum of the
      --  number of queues for all entries that precede the designated one, to
      --  which is added the index expression, if this expression denotes a
      --  member of a family.

      --  The following is a place holder for the count of simple entries

      Num := Make_Integer_Literal (Sloc, 1);

      --  We construct an expression which is a series of addition operations.
      --  The first operand is the number of single entries that precede this
      --  one, the second operand is the index value relative to the start of
      --  the referenced family, and the remaining operands are the lengths of
      --  the entry families that precede this entry, i.e. the constructed
      --  expression is:

      --    number_simple_entries +
      --      (s'pos (index-value) - s'pos (family'first)) + 1 +
      --      family'length + ...

      --  where index-value is the given index value, and s is the index
      --  subtype (we have to use pos because the subtype might be an
      --  enumeration type preventing direct subtraction). Note that the task
      --  entry array is one-indexed.

      --  The upper bound of the entry family may be a discriminant, so we
      --  retrieve the lower bound explicitly to compute offset, rather than
      --  using the index subtype which may mention a discriminant.

      if Present (Index) then
         S := Etype (Discrete_Subtype_Definition (Declaration_Node (Ent)));

         Expr :=
           Make_Op_Add (Sloc,
             Left_Opnd  => Num,

             Right_Opnd =>
               Family_Offset (
                 Sloc,
                 Make_Attribute_Reference (Sloc,
                   Attribute_Name => Name_Pos,
                   Prefix => New_Reference_To (Base_Type (S), Sloc),
                   Expressions => New_List (Relocate_Node (Index))),
                 Type_Low_Bound (S),
                 Ttyp));
      else
         Expr := Num;
      end if;

      --  Now add lengths of preceding entries and entry families

      Prev := First_Entity (Ttyp);

      while Chars (Prev) /= Chars (Ent)
        or else (Ekind (Prev) /= Ekind (Ent))
        or else not Sem_Ch6.Type_Conformant (Ent, Prev)
      loop
         if Ekind (Prev) = E_Entry then
            Set_Intval (Num, Intval (Num) + 1);

         elsif Ekind (Prev) = E_Entry_Family then
            S :=
              Etype (Discrete_Subtype_Definition (Declaration_Node (Prev)));
            Lo := Type_Low_Bound  (S);
            Hi := Type_High_Bound (S);

            Expr :=
              Make_Op_Add (Sloc,
              Left_Opnd  => Expr,
              Right_Opnd => Family_Size (Sloc, Hi, Lo, Ttyp));

         --  Other components are anonymous types to be ignored

         else
            null;
         end if;

         Next_Entity (Prev);
      end loop;

      return Expr;
   end Entry_Index_Expression;

   ---------------------------
   -- Establish_Task_Master --
   ---------------------------

   procedure Establish_Task_Master (N : Node_Id) is
      Call : Node_Id;
   begin
      if Restriction_Active (No_Task_Hierarchy) = False then
         Call := Build_Runtime_Call (Sloc (N), RE_Enter_Master);
         Prepend_To (Declarations (N), Call);
         Analyze (Call);
      end if;
   end Establish_Task_Master;

   --------------------------------
   -- Expand_Accept_Declarations --
   --------------------------------

   --  Part of the expansion of an accept statement involves the creation of
   --  a declaration that can be referenced from the statement sequence of
   --  the accept:

   --    Ann : Address;

   --  This declaration is inserted immediately before the accept statement
   --  and it is important that it be inserted before the statements of the
   --  statement sequence are analyzed. Thus it would be too late to create
   --  this declaration in the Expand_N_Accept_Statement routine, which is
   --  why there is a separate procedure to be called directly from Sem_Ch9.

   --  Ann is used to hold the address of the record containing the parameters
   --  (see Expand_N_Entry_Call for more details on how this record is built).
   --  References to the parameters do an unchecked conversion of this address
   --  to a pointer to the required record type, and then access the field that
   --  holds the value of the required parameter. The entity for the address
   --  variable is held as the top stack element (i.e. the last element) of the
   --  Accept_Address stack in the corresponding entry entity, and this element
   --  must be set in place  before the statements are processed.

   --  The above description applies to the case of a stand alone accept
   --  statement, i.e. one not appearing as part of a select alternative.

   --  For the case of an accept that appears as part of a select alternative
   --  of a selective accept, we must still create the declaration right away,
   --  since Ann is needed immediately, but there is an important difference:

   --    The declaration is inserted before the selective accept, not before
   --    the accept statement (which is not part of a list anyway, and so would
   --    not accommodate inserted declarations)

   --    We only need one address variable for the entire selective accept. So
   --    the Ann declaration is created only for the first accept alternative,
   --    and subsequent accept alternatives reference the same Ann variable.

   --  We can distinguish the two cases by seeing whether the accept statement
   --  is part of a list. If not, then it must be in an accept alternative.

   --  To expand the requeue statement, a label is provided at the end of the
   --  accept statement or alternative of which it is a part, so that the
   --  statement can be skipped after the requeue is complete. This label is
   --  created here rather than during the expansion of the accept statement,
   --  because it will be needed by any requeue statements within the accept,
   --  which are expanded before the accept.

   procedure Expand_Accept_Declarations (N : Node_Id; Ent : Entity_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Ann    : Entity_Id := Empty;
      Adecl  : Node_Id;
      Lab_Id : Node_Id;
      Lab    : Node_Id;
      Ldecl  : Node_Id;
      Ldecl2 : Node_Id;

   begin
      if Expander_Active then

         --  If we have no handled statement sequence, then build a dummy
         --  sequence consisting of a null statement. This is only done if
         --  pragma FIFO_Within_Priorities is specified. The issue here is
         --  that even a null accept body has an effect on the called task
         --  in terms of its position in the queue, so we cannot optimize
         --  the context switch away. However, if FIFO_Within_Priorities
         --  is not active, the optimization is legitimate, since we can
         --  say that our dispatching policy (i.e. the default dispatching
         --  policy) reorders the queue to be the same as just before the
         --  call. In the absence of a specified dispatching policy, we are
         --  allowed to modify queue orders for a given priority at will!

         if Opt.Task_Dispatching_Policy = 'F' and then
           not Present (Handled_Statement_Sequence (N))
         then
            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Loc,
                New_List (Make_Null_Statement (Loc))));
         end if;

         --  Create and declare two labels to be placed at the end of the
         --  accept statement. The first label is used to allow requeues to
         --  skip the remainder of entry processing. The second label is used
         --  to skip the remainder of entry processing if the rendezvous
         --  completes in the middle of the accept body.

         if Present (Handled_Statement_Sequence (N)) then
            Lab_Id := Make_Identifier (Loc, New_Internal_Name ('L'));
            Set_Entity (Lab_Id,
              Make_Defining_Identifier (Loc, Chars (Lab_Id)));
            Lab := Make_Label (Loc, Lab_Id);
            Ldecl :=
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier  => Entity (Lab_Id),
                Label_Construct      => Lab);
            Append (Lab, Statements (Handled_Statement_Sequence (N)));

            Lab_Id := Make_Identifier (Loc, New_Internal_Name ('L'));
            Set_Entity (Lab_Id,
              Make_Defining_Identifier (Loc, Chars (Lab_Id)));
            Lab := Make_Label (Loc, Lab_Id);
            Ldecl2 :=
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier  => Entity (Lab_Id),
                Label_Construct      => Lab);
            Append (Lab, Statements (Handled_Statement_Sequence (N)));

         else
            Ldecl := Empty;
            Ldecl2 := Empty;
         end if;

         --  Case of stand alone accept statement

         if Is_List_Member (N) then

            if Present (Handled_Statement_Sequence (N)) then
               Ann :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_Internal_Name ('A'));

               Adecl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Ann,
                   Object_Definition =>
                     New_Reference_To (RTE (RE_Address), Loc));

               Insert_Before (N, Adecl);
               Analyze (Adecl);

               Insert_Before (N, Ldecl);
               Analyze (Ldecl);

               Insert_Before (N, Ldecl2);
               Analyze (Ldecl2);
            end if;

         --  Case of accept statement which is in an accept alternative

         else
            declare
               Acc_Alt : constant Node_Id := Parent (N);
               Sel_Acc : constant Node_Id := Parent (Acc_Alt);
               Alt     : Node_Id;

            begin
               pragma Assert (Nkind (Acc_Alt) = N_Accept_Alternative);
               pragma Assert (Nkind (Sel_Acc) = N_Selective_Accept);

               --  ??? Consider a single label for select statements

               if Present (Handled_Statement_Sequence (N)) then
                  Prepend (Ldecl2,
                     Statements (Handled_Statement_Sequence (N)));
                  Analyze (Ldecl2);

                  Prepend (Ldecl,
                     Statements (Handled_Statement_Sequence (N)));
                  Analyze (Ldecl);
               end if;

               --  Find first accept alternative of the selective accept. A
               --  valid selective accept must have at least one accept in it.

               Alt := First (Select_Alternatives (Sel_Acc));

               while Nkind (Alt) /= N_Accept_Alternative loop
                  Next (Alt);
               end loop;

               --  If we are the first accept statement, then we have to create
               --  the Ann variable, as for the stand alone case, except that
               --  it is inserted before the selective accept. Similarly, a
               --  label for requeue expansion must be declared.

               if N = Accept_Statement (Alt) then
                  Ann :=
                    Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

                  Adecl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Ann,
                      Object_Definition =>
                        New_Reference_To (RTE (RE_Address), Loc));

                  Insert_Before (Sel_Acc, Adecl);
                  Analyze (Adecl);

               --  If we are not the first accept statement, then find the Ann
               --  variable allocated by the first accept and use it.

               else
                  Ann :=
                    Node (Last_Elmt (Accept_Address
                      (Entity (Entry_Direct_Name (Accept_Statement (Alt))))));
               end if;
            end;
         end if;

         --  Merge here with Ann either created or referenced, and Adecl
         --  pointing to the corresponding declaration. Remaining processing
         --  is the same for the two cases.

         if Present (Ann) then
            Append_Elmt (Ann, Accept_Address (Ent));
            Set_Needs_Debug_Info (Ann);
         end if;

         --  Create renaming declarations for the entry formals. Each reference
         --  to a formal becomes a dereference of a component of the parameter
         --  block, whose address is held in Ann. These declarations are
         --  eventually inserted into the accept block, and analyzed there so
         --  that they have the proper scope for gdb and do not conflict with
         --  other declarations.

         if Present (Parameter_Specifications (N))
           and then Present (Handled_Statement_Sequence (N))
         then
            declare
               Comp   : Entity_Id;
               Decl   : Node_Id;
               Formal : Entity_Id;
               New_F  : Entity_Id;

            begin
               New_Scope (Ent);
               Formal := First_Formal (Ent);

               while Present (Formal) loop
                  Comp  := Entry_Component (Formal);
                  New_F :=
                    Make_Defining_Identifier (Sloc (Formal), Chars (Formal));

                  Set_Etype (New_F, Etype (Formal));
                  Set_Scope (New_F, Ent);
                  Set_Needs_Debug_Info (New_F);   --  That's the whole point.

                  if Ekind (Formal) = E_In_Parameter then
                     Set_Ekind (New_F, E_Constant);
                  else
                     Set_Ekind (New_F, E_Variable);
                     Set_Extra_Constrained (New_F, Extra_Constrained (Formal));
                  end if;

                  Set_Actual_Subtype (New_F, Actual_Subtype (Formal));

                  Decl :=
                    Make_Object_Renaming_Declaration (Loc,
                      Defining_Identifier =>
                        New_F,
                      Subtype_Mark =>
                        New_Reference_To (Etype (Formal), Loc),
                      Name =>
                        Make_Explicit_Dereference (Loc,
                          Make_Selected_Component (Loc,
                            Prefix =>
                              Unchecked_Convert_To (
                                Entry_Parameters_Type (Ent),
                                New_Reference_To (Ann, Loc)),
                            Selector_Name =>
                              New_Reference_To (Comp, Loc))));

                  if No (Declarations (N)) then
                     Set_Declarations (N, New_List);
                  end if;

                  Append (Decl, Declarations (N));
                  Set_Renamed_Object (Formal, New_F);
                  Next_Formal (Formal);
               end loop;

               End_Scope;
            end;
         end if;
      end if;
   end Expand_Accept_Declarations;

   ---------------------------------------------
   -- Expand_Access_Protected_Subprogram_Type --
   ---------------------------------------------

   procedure Expand_Access_Protected_Subprogram_Type (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Comps  : List_Id;
      T      : constant Entity_Id  := Defining_Identifier (N);
      D_T    : constant Entity_Id  := Designated_Type (T);
      D_T2   : constant Entity_Id  := Make_Defining_Identifier (Loc,
                                        Chars => New_Internal_Name ('D'));
      E_T    : constant Entity_Id  := Make_Defining_Identifier (Loc,
                                        Chars => New_Internal_Name ('E'));
      P_List : constant List_Id    := Build_Protected_Spec
                                        (N, RTE (RE_Address), False, D_T);
      Decl1  : Node_Id;
      Decl2  : Node_Id;
      Def1   : Node_Id;

   begin
      --  Create access to protected subprogram with full signature

      if Nkind (Type_Definition (N)) = N_Access_Function_Definition then
         Def1 :=
           Make_Access_Function_Definition (Loc,
             Parameter_Specifications => P_List,
             Result_Definition =>
               New_Copy (Result_Definition (Type_Definition (N))));

      else
         Def1 :=
           Make_Access_Procedure_Definition (Loc,
             Parameter_Specifications => P_List);
      end if;

      Decl1 :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => D_T2,
          Type_Definition => Def1);

      Analyze (Decl1);
      Insert_After (N, Decl1);

      --  Create Equivalent_Type, a record with two components for an access to
      --  object and an access to subprogram.

      Comps := New_List (
        Make_Component_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, New_Internal_Name ('P')),
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present    => False,
              Subtype_Indication =>
                New_Occurrence_Of (RTE (RE_Address), Loc))),

        Make_Component_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, New_Internal_Name ('S')),
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present    => False,
              Subtype_Indication => New_Occurrence_Of (D_T2, Loc))));

      Decl2 :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => E_T,
          Type_Definition     =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Items => Comps)));

      Analyze (Decl2);
      Insert_After (Decl1, Decl2);
      Set_Equivalent_Type (T, E_T);
   end Expand_Access_Protected_Subprogram_Type;

   --------------------------
   -- Expand_Entry_Barrier --
   --------------------------

   procedure Expand_Entry_Barrier (N : Node_Id; Ent : Entity_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Prot      : constant Entity_Id  := Scope (Ent);
      Spec_Decl : constant Node_Id    := Parent (Prot);
      Cond      : constant Node_Id    :=
                    Condition (Entry_Body_Formal_Part (N));
      Func      : Node_Id;
      B_F       : Node_Id;
      Body_Decl : Node_Id;

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("entry barrier", N);
         return;
      end if;

      --  The body of the entry barrier must be analyzed in the context of the
      --  protected object, but its scope is external to it, just as any other
      --  unprotected version of a protected operation. The specification has
      --  been produced when the protected type declaration was elaborated. We
      --  build the body, insert it in the enclosing scope, but analyze it in
      --  the current context. A more uniform approach would be to treat
      --  barrier just as a protected function, and discard the protected
      --  version of it because it is never called.

      if Expander_Active then
         B_F := Build_Barrier_Function (N, Ent, Prot);
         Func := Barrier_Function (Ent);
         Set_Corresponding_Spec (B_F, Func);

         Body_Decl := Parent (Corresponding_Body (Spec_Decl));

         if Nkind (Parent (Body_Decl)) = N_Subunit then
            Body_Decl := Corresponding_Stub (Parent (Body_Decl));
         end if;

         Insert_Before_And_Analyze (Body_Decl, B_F);

         Update_Prival_Subtypes (B_F);

         Set_Privals (Spec_Decl, N, Loc, After_Barrier => True);
         Set_Discriminals (Spec_Decl);
         Set_Scope (Func, Scope (Prot));

      else
         Analyze_And_Resolve (Cond, Any_Boolean);
      end if;

      --  The Ravenscar profile restricts barriers to simple variables declared
      --  within the protected object. We also allow Boolean constants, since
      --  these appear in several published examples and are also allowed by
      --  the Aonix compiler.

      --  Note that after analysis variables in this context will be replaced
      --  by the corresponding prival, that is to say a renaming of a selected
      --  component of the form _Object.Var. If expansion is disabled, as
      --  within a generic, we check that the entity appears in the current
      --  scope.

      if Is_Entity_Name (Cond) then

         if Entity (Cond) = Standard_False
              or else
            Entity (Cond) = Standard_True
         then
            return;

         elsif not Expander_Active
           and then Scope (Entity (Cond)) = Current_Scope
         then
            return;

         --  Check for case of _object.all.field (note that the explicit
         --  dereference gets inserted by analyze/expand of _object.field)

         elsif Present (Renamed_Object (Entity (Cond)))
           and then
             Nkind (Renamed_Object (Entity (Cond))) = N_Selected_Component
           and then
             Chars
               (Prefix
                 (Prefix (Renamed_Object (Entity (Cond))))) = Name_uObject
         then
            return;
         end if;
      end if;

      --  It is not a boolean variable or literal, so check the restriction

      Check_Restriction (Simple_Barriers, Cond);
   end Expand_Entry_Barrier;

   ------------------------------------
   -- Expand_Entry_Body_Declarations --
   ------------------------------------

   procedure Expand_Entry_Body_Declarations (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Index_Spec : Node_Id;

   begin
      if Expander_Active then

         --  Expand entry bodies corresponding to entry families
         --  by assigning a placeholder for the constant that will
         --  be used to expand references to the entry index parameter.

         Index_Spec :=
           Entry_Index_Specification (Entry_Body_Formal_Part (N));

         if Present (Index_Spec) then
            Set_Entry_Index_Constant (
              Defining_Identifier (Index_Spec),
              Make_Defining_Identifier (Loc, New_Internal_Name ('J')));
         end if;
      end if;
   end Expand_Entry_Body_Declarations;

   ------------------------------
   -- Expand_N_Abort_Statement --
   ------------------------------

   --  Expand abort T1, T2, .. Tn; into:
   --    Abort_Tasks (Task_List'(1 => T1.Task_Id, 2 => T2.Task_Id ...))

   procedure Expand_N_Abort_Statement (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Tlist  : constant List_Id    := Names (N);
      Count  : Nat;
      Aggr   : Node_Id;
      Tasknm : Node_Id;

   begin
      Aggr := Make_Aggregate (Loc, Component_Associations => New_List);
      Count := 0;

      Tasknm := First (Tlist);

      while Present (Tasknm) loop
         Count := Count + 1;

         --  A task interface class-wide type object is being aborted.
         --  Retrieve its _task_id by calling a dispatching routine.

         if Ada_Version >= Ada_05
           and then Ekind (Etype (Tasknm)) = E_Class_Wide_Type
           and then Is_Interface      (Etype (Tasknm))
           and then Is_Task_Interface (Etype (Tasknm))
         then
            Append_To (Component_Associations (Aggr),
              Make_Component_Association (Loc,
                Choices => New_List (
                  Make_Integer_Literal (Loc, Count)),
                Expression =>

                  --  Tasknm._disp_get_task_id

                    Make_Selected_Component (Loc,
                      Prefix =>
                        New_Copy_Tree (Tasknm),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_uDisp_Get_Task_Id))));

         else
            Append_To (Component_Associations (Aggr),
              Make_Component_Association (Loc,
                Choices => New_List (
                  Make_Integer_Literal (Loc, Count)),
                Expression => Concurrent_Ref (Tasknm)));
         end if;

         Next (Tasknm);
      end loop;

      Rewrite (N,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Abort_Tasks), Loc),
          Parameter_Associations => New_List (
            Make_Qualified_Expression (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Task_List), Loc),
              Expression => Aggr))));

      Analyze (N);
   end Expand_N_Abort_Statement;

   -------------------------------
   -- Expand_N_Accept_Statement --
   -------------------------------

   --  This procedure handles expansion of accept statements that stand
   --  alone, i.e. they are not part of an accept alternative. The expansion
   --  of accept statement in accept alternatives is handled by the routines
   --  Expand_N_Accept_Alternative and Expand_N_Selective_Accept. The
   --  following description applies only to stand alone accept statements.

   --  If there is no handled statement sequence, or only null statements,
   --  then this is called a trivial accept, and the expansion is:

   --    Accept_Trivial (entry-index)

   --  If there is a handled statement sequence, then the expansion is:

   --    Ann : Address;
   --    {Lnn : Label}

   --    begin
   --       begin
   --          Accept_Call (entry-index, Ann);
   --          Renaming_Declarations for formals
   --          <statement sequence from N_Accept_Statement node>
   --          Complete_Rendezvous;
   --          <<Lnn>>
   --
   --       exception
   --          when ... =>
   --             <exception handler from N_Accept_Statement node>
   --             Complete_Rendezvous;
   --          when ... =>
   --             <exception handler from N_Accept_Statement node>
   --             Complete_Rendezvous;
   --          ...
   --       end;

   --    exception
   --       when all others =>
   --          Exceptional_Complete_Rendezvous (Get_GNAT_Exception);
   --    end;

   --  The first three declarations were already inserted ahead of the accept
   --  statement by the Expand_Accept_Declarations procedure, which was called
   --  directly from the semantics during analysis of the accept. statement,
   --  before analyzing its contained statements.

   --  The declarations from the N_Accept_Statement, as noted in Sinfo, come
   --  from possible expansion activity (the original source of course does
   --  not have any declarations associated with the accept statement, since
   --  an accept statement has no declarative part). In particular, if the
   --  expander is active, the first such declaration is the declaration of
   --  the Accept_Params_Ptr entity (see Sem_Ch9.Analyze_Accept_Statement).
   --
   --  The two blocks are merged into a single block if the inner block has
   --  no exception handlers, but otherwise two blocks are required, since
   --  exceptions might be raised in the exception handlers of the inner
   --  block, and Exceptional_Complete_Rendezvous must be called.

   procedure Expand_N_Accept_Statement (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Stats   : constant Node_Id    := Handled_Statement_Sequence (N);
      Ename   : constant Node_Id    := Entry_Direct_Name (N);
      Eindx   : constant Node_Id    := Entry_Index (N);
      Eent    : constant Entity_Id  := Entity (Ename);
      Acstack : constant Elist_Id   := Accept_Address (Eent);
      Ann     : constant Entity_Id  := Node (Last_Elmt (Acstack));
      Ttyp    : constant Entity_Id  := Etype (Scope (Eent));
      Blkent  : Entity_Id;
      Call    : Node_Id;
      Block   : Node_Id;

      function Null_Statements (Stats : List_Id) return Boolean;
      --  Check for null statement sequence (i.e a list of labels and
      --  null statements).

      ---------------------
      -- Null_Statements --
      ---------------------

      function Null_Statements (Stats : List_Id) return Boolean is
         Stmt : Node_Id;

      begin
         Stmt := First (Stats);
         while Nkind (Stmt) /= N_Empty
           and then (Nkind (Stmt) = N_Null_Statement
                       or else
                     Nkind (Stmt) = N_Label)
         loop
            Next (Stmt);
         end loop;

         return Nkind (Stmt) = N_Empty;
      end Null_Statements;

   --  Start of processing for Expand_N_Accept_Statement

   begin
      --  If accept statement is not part of a list, then its parent must be
      --  an accept alternative, and, as described above, we do not do any
      --  expansion for such accept statements at this level.

      if not Is_List_Member (N) then
         pragma Assert (Nkind (Parent (N)) = N_Accept_Alternative);
         return;

      --  Trivial accept case (no statement sequence, or null statements).
      --  If the accept statement has declarations, then just insert them
      --  before the procedure call.

      --  We avoid this optimization when FIFO_Within_Priorities is active,
      --  since it is not correct according to annex D semantics. The problem
      --  is that the call is required to reorder the acceptors position on
      --  its ready queue, even though there is nothing to be done. However,
      --  if no policy is specified, then we decide that our dispatching
      --  policy always reorders the queue right after the RV to look the
      --  way they were just before the RV. Since we are allowed to freely
      --  reorder same-priority queues (this is part of what dispatching
      --  policies are all about), the optimization is legitimate.

      elsif Opt.Task_Dispatching_Policy /= 'F'
        and then (No (Stats) or else Null_Statements (Statements (Stats)))
      then
         --  Remove declarations for renamings, because the parameter block
         --  will not be assigned.

         declare
            D      : Node_Id;
            Next_D : Node_Id;

         begin
            D := First (Declarations (N));

            while Present (D) loop
               Next_D := Next (D);
               if Nkind (D) = N_Object_Renaming_Declaration then
                  Remove (D);
               end if;

               D := Next_D;
            end loop;
         end;

         if Present (Declarations (N)) then
            Insert_Actions (N, Declarations (N));
         end if;

         Rewrite (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Accept_Trivial), Loc),
             Parameter_Associations => New_List (
               Entry_Index_Expression (Loc, Entity (Ename), Eindx, Ttyp))));

         Analyze (N);

         --  Discard Entry_Address that was created for it, so it will not be
         --  emitted if this accept statement is in the statement part of a
         --  delay alternative.

         if Present (Stats) then
            Remove_Last_Elmt (Acstack);
         end if;

      --  Case of statement sequence present

      else
         --  Construct the block, using the declarations from the accept
         --  statement if any to initialize the declarations of the block.

         Blkent := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         Set_Ekind (Blkent, E_Block);
         Set_Etype (Blkent, Standard_Void_Type);
         Set_Scope (Blkent, Current_Scope);

         Block :=
           Make_Block_Statement (Loc,
             Identifier                 => New_Reference_To (Blkent, Loc),
             Declarations               => Declarations (N),
             Handled_Statement_Sequence => Build_Accept_Body (N));

         --  Prepend call to Accept_Call to main statement sequence If the
         --  accept has exception handlers, the statement sequence is wrapped
         --  in a block. Insert call and renaming declarations in the
         --  declarations of the block, so they are elaborated before the
         --  handlers.

         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Accept_Call), Loc),
             Parameter_Associations => New_List (
               Entry_Index_Expression (Loc, Entity (Ename), Eindx, Ttyp),
               New_Reference_To (Ann, Loc)));

         if Parent (Stats) = N then
            Prepend (Call, Statements (Stats));
         else
            Set_Declarations
              (Parent (Stats),
                New_List (Call));
         end if;

         Analyze (Call);

         New_Scope (Blkent);

         declare
            D      : Node_Id;
            Next_D : Node_Id;
            Typ    : Entity_Id;

         begin
            D := First (Declarations (N));
            while Present (D) loop
               Next_D := Next (D);

               if Nkind (D) = N_Object_Renaming_Declaration then

                  --  The renaming declarations for the formals were created
                  --  during analysis of the accept statement, and attached to
                  --  the list of declarations. Place them now in the context
                  --  of the accept block or subprogram.

                  Remove (D);
                  Typ := Entity (Subtype_Mark (D));
                  Insert_After (Call, D);
                  Analyze (D);

                  --  If the formal is class_wide, it does not have an actual
                  --  subtype. The analysis of the renaming declaration creates
                  --  one, but we need to retain the class-wide nature of the
                  --  entity.

                  if Is_Class_Wide_Type (Typ) then
                     Set_Etype (Defining_Identifier (D), Typ);
                  end if;

               end if;

               D := Next_D;
            end loop;
         end;

         End_Scope;

         --  Replace the accept statement by the new block

         Rewrite (N, Block);
         Analyze (N);

         --  Last step is to unstack the Accept_Address value

         Remove_Last_Elmt (Acstack);
      end if;
   end Expand_N_Accept_Statement;

   ----------------------------------
   -- Expand_N_Asynchronous_Select --
   ----------------------------------

   --  This procedure assumes that the trigger statement is an entry call or
   --  a dispatching procedure call. A delay alternative should already have
   --  been expanded into an entry call to the appropriate delay object Wait
   --  entry.

   --  If the trigger is a task entry call, the select is implemented with
   --  a Task_Entry_Call:

   --    declare
   --       B : Boolean;
   --       C : Boolean;
   --       P : parms := (parm, parm, parm);

   --        --  Clean is added by Exp_Ch7.Expand_Cleanup_Actions

   --       procedure _clean is
   --       begin
   --          ...
   --          Cancel_Task_Entry_Call (C);
   --          ...
   --       end _clean;

   --    begin
   --       Abort_Defer;
   --       Task_Entry_Call
   --         (acceptor-task,
   --          entry-index,
   --          P'Address,
   --          Asynchronous_Call,
   --          B);

   --       begin
   --          begin
   --             Abort_Undefer;
   --             <abortable-part>
   --          at end
   --             _clean;        --  Added by Exp_Ch7.Expand_Cleanup_Actions.
   --          end;
   --       exception
   --          when Abort_Signal => Abort_Undefer;
   --       end;

   --       parm := P.param;
   --       parm := P.param;
   --       ...
   --       if not C then
   --          <triggered-statements>
   --       end if;
   --    end;

   --  Note that Build_Simple_Entry_Call is used to expand the entry
   --  of the asynchronous entry call (by the
   --  Expand_N_Entry_Call_Statement procedure) as follows:

   --    declare
   --       P : parms := (parm, parm, parm);
   --    begin
   --       Call_Simple (acceptor-task, entry-index, P'Address);
   --       parm := P.param;
   --       parm := P.param;
   --       ...
   --    end;

   --  so the task at hand is to convert the latter expansion into the former

   --  If the trigger is a protected entry call, the select is
   --  implemented with Protected_Entry_Call:

   --  declare
   --     P   : E1_Params := (param, param, param);
   --     Bnn : Communications_Block;

   --  begin
   --     declare
   --        --  Clean is added by Exp_Ch7.Expand_Cleanup_Actions.
   --        procedure _clean is
   --        begin
   --           ...
   --           if Enqueued (Bnn) then
   --              Cancel_Protected_Entry_Call (Bnn);
   --           end if;
   --           ...
   --        end _clean;

   --     begin
   --        begin
   --           Protected_Entry_Call (
   --             Object => po._object'Access,
   --             E => <entry index>;
   --             Uninterpreted_Data => P'Address;
   --             Mode => Asynchronous_Call;
   --             Block => Bnn);
   --           if Enqueued (Bnn) then
   --              <abortable-part>
   --           end if;
   --        at end
   --           _clean;        --  Added by Exp_Ch7.Expand_Cleanup_Actions.
   --        end;
   --     exception
   --        when Abort_Signal => Abort_Undefer;
   --     end;

   --     if not Cancelled (Bnn) then
   --        <triggered-statements>
   --     end if;
   --  end;

   --  Build_Simple_Entry_Call is used to expand the all to a simple
   --  protected entry call:

   --  declare
   --     P   : E1_Params := (param, param, param);
   --     Bnn : Communications_Block;

   --  begin
   --     Protected_Entry_Call (
   --       Object => po._object'Access,
   --       E => <entry index>;
   --       Uninterpreted_Data => P'Address;
   --       Mode => Simple_Call;
   --       Block => Bnn);
   --     parm := P.param;
   --     parm := P.param;
   --       ...
   --  end;

   --  Ada 2005 (AI-345): If the trigger is a dispatching call, the select is
   --  expanded into:

   --  declare
   --     B   : Boolean := False;
   --     Bnn : Communication_Block;
   --     C   : Ada.Tags.Prim_Op_Kind;
   --     P   : Parameters := (Param1 .. ParamN)
   --     S   : constant Integer := DT_Position (<dispatching-call>);
   --     U   : Boolean;

   --  begin
   --     disp_get_prim_op_kind (<object>, S, C);

   --     if C = POK_Protected_Entry then
   --        declare
   --           procedure _clean is
   --           begin
   --              if Enqueued (Bnn) then
   --                 Cancel_Protected_Entry_Call (Bnn);
   --              end if;
   --           end _clean;

   --        begin
   --           begin
   --              disp_asynchronous_select
   --                (Obj, S, P'address, Bnn, B);

   --              Param1 := P.Param1;
   --              ...
   --              ParamN := P.ParamN;

   --              if Enqueued (Bnn) then
   --                 <abortable-statements>
   --              end if;
   --           at end
   --              _clean;
   --           end;
   --        exception
   --           when Abort_Signal => Abort_Undefer;
   --        end;

   --        if not Cancelled (Bnn) then
   --           <triggering-statements>
   --        end if;

   --     elsif C = POK_Task_Entry then
   --        declare
   --           procedure _clean is
   --           begin
   --              Cancel_Task_Entry_Call (U);
   --           end _clean;

   --        begin
   --           Abort_Defer;

   --           disp_asynchronous_select
   --             (<object>, S, P'address, Bnn, B);

   --           Param1 := P.Param1;
   --           ...
   --           ParamN := P.ParamN;

   --           begin
   --              begin
   --                 Abort_Undefer;
   --                 <abortable-statements>
   --              at end
   --                 _clean;
   --              end;
   --           exception
   --              when Abort_Signal => Abort_Undefer;
   --           end;

   --           if not U then
   --              <triggering-statements>
   --           end if;
   --        end;

   --     else
   --        <dispatching-call>;
   --        <triggering-statements>
   --     end if;

   --  The job is to convert this to the asynchronous form

   --  If the trigger is a delay statement, it will have been expanded into a
   --  call to one of the GNARL delay procedures. This routine will convert
   --  this into a protected entry call on a delay object and then continue
   --  processing as for a protected entry call trigger. This requires
   --  declaring a Delay_Block object and adding a pointer to this object to
   --  the parameter list of the delay procedure to form the parameter list of
   --  the entry call. This object is used by the runtime to queue the delay
   --  request.

   --  For a description of the use of P and the assignments after the
   --  call, see Expand_N_Entry_Call_Statement.

   procedure Expand_N_Asynchronous_Select (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Abrt   : constant Node_Id    := Abortable_Part (N);
      Astats : constant List_Id    := Statements (Abrt);
      Trig   : constant Node_Id    := Triggering_Alternative (N);
      Tstats : constant List_Id    := Statements (Trig);

      Abort_Block_Ent   : Entity_Id;
      Abortable_Block   : Node_Id;
      Actuals           : List_Id;
      Blk_Ent           : Entity_Id;
      Blk_Typ           : Entity_Id;
      Call              : Node_Id;
      Call_Ent          : Entity_Id;
      Cancel_Param      : Entity_Id;
      Cleanup_Block     : Node_Id;
      Cleanup_Block_Ent : Entity_Id;
      Cleanup_Stmts     : List_Id;
      Concval           : Node_Id;
      Dblock_Ent        : Entity_Id;
      Decl              : Node_Id;
      Decls             : List_Id;
      Ecall             : Node_Id;
      Ename             : Node_Id;
      Enqueue_Call      : Node_Id;
      Formals           : List_Id;
      Hdle              : List_Id;
      Index             : Node_Id;
      N_Orig            : Node_Id;
      Obj               : Entity_Id;
      Param             : Node_Id;
      Params            : List_Id;
      Pdef              : Entity_Id;
      ProtE_Stmts       : List_Id;
      ProtP_Stmts       : List_Id;
      Stmt              : Node_Id;
      Stmts             : List_Id;
      Target_Undefer    : RE_Id;
      TaskE_Stmts       : List_Id;
      Undefer_Args      : List_Id := No_List;

      B   : Entity_Id;  --  Call status flag
      Bnn : Entity_Id;  --  Communication block
      C   : Entity_Id;  --  Call kind
      P   : Entity_Id;  --  Parameter block
      S   : Entity_Id;  --  Primitive operation slot
      T   : Entity_Id;  --  Additional status flag

   begin
      Blk_Ent := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      Ecall   := Triggering_Statement (Trig);

      --  The arguments in the call may require dynamic allocation, and the
      --  call statement may have been transformed into a block. The block
      --  may contain additional declarations for internal entities, and the
      --  original call is found by sequential search.

      if Nkind (Ecall) = N_Block_Statement then
         Ecall := First (Statements (Handled_Statement_Sequence (Ecall)));
         while Nkind (Ecall) /= N_Procedure_Call_Statement
           and then Nkind (Ecall) /= N_Entry_Call_Statement
         loop
            Next (Ecall);
         end loop;
      end if;

      --  This is either a dispatching call or a delay statement used as a
      --  trigger which was expanded into a procedure call.

      if Nkind (Ecall) = N_Procedure_Call_Statement then
         if Ada_Version >= Ada_05
           and then
             (not Present (Original_Node (Ecall))
                or else
              Nkind (Original_Node (Ecall)) /= N_Delay_Relative_Statement)
         then
            Extract_Dispatching_Call (Ecall, Call_Ent, Obj, Actuals, Formals);

            Decls := New_List;
            Stmts := New_List;

            --  Call status flag processing, generate:
            --    B : Boolean := False;

            B := SEU.Build_B (Loc, Decls);

            --  Communication block processing, generate:
            --    Bnn : Communication_Block;

            Bnn := Make_Defining_Identifier (Loc, New_Internal_Name ('B'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Bnn,
                Object_Definition =>
                  New_Reference_To (RTE (RE_Communication_Block), Loc)));

            --  Call kind processing, generate:
            --    C : Ada.Tags.Prim_Op_Kind;

            C := SEU.Build_C (Loc, Decls);

            --  Parameter block processing

            Blk_Typ := Build_Parameter_Block
                         (Loc, Actuals, Formals, Decls);
            P       := Parameter_Block_Pack
                         (Loc, Blk_Typ, Actuals, Formals, Decls, Stmts);

            --  Dispatch table slot processing, generate:
            --    S : constant Integer :=
            --          Ada.Tags.Get_Offset_Index (
            --            Unchecked_Convert_To (Ada.Tags.Interface_Tag, Obj),
            --            DT_Position (<dispatching-procedure>));

            S := SEU.Build_S (Loc, Decls, Obj, Call_Ent);

            --  Additional status flag processing, generate:

            T := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  T,
                Object_Definition =>
                  New_Reference_To (Standard_Boolean, Loc)));

            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (
                    Find_Prim_Op (Etype (Etype (Obj)),
                      Name_uDisp_Get_Prim_Op_Kind),
                  Loc),
                Parameter_Associations =>
                  New_List (
                    New_Copy_Tree    (Obj),
                    New_Reference_To (S, Loc),
                    New_Reference_To (C, Loc))));

            --  ---------------------------------------------------------------
            --  Protected entry handling

            --  Generate:
            --    Param1 := P.Param1;
            --    ...
            --    ParamN := P.ParamN;

            Cleanup_Stmts := Parameter_Block_Unpack (Loc, P, Actuals, Formals);

            --  Generate:
            --    _dispatching_asynchronous_select
            --      (<object>, S, P'address, Bnn, B);

            Prepend_To (Cleanup_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (
                    Find_Prim_Op (Etype (Etype (Obj)),
                      Name_uDisp_Asynchronous_Select),
                  Loc),
                Parameter_Associations =>
                  New_List (
                    New_Copy_Tree    (Obj),
                    New_Reference_To (S, Loc),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (P, Loc),
                      Attribute_Name => Name_Address),
                    New_Reference_To (Bnn, Loc),
                    New_Reference_To (B, Loc))));

            --  Generate:
            --    if Enqueued (Bnn) then
            --       <abortable-statements>
            --    end if;

            Append_To (Cleanup_Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Function_Call (Loc,
                    Name =>
                      New_Reference_To (RTE (RE_Enqueued), Loc),
                    Parameter_Associations =>
                      New_List (
                        New_Reference_To (Bnn, Loc))),

                Then_Statements =>
                  New_Copy_List_Tree (Astats)));

            --  Wrap the statements in a block. Exp_Ch7.Expand_Cleanup_Actions
            --  will then generate a _clean for the communication block Bnn.

            --  Generate:
            --    declare
            --       procedure _clean is
            --       begin
            --          if Enqueued (Bnn) then
            --             Cancel_Protected_Entry_Call (Bnn);
            --          end if;
            --       end _clean;
            --    begin
            --       Cleanup_Stmts
            --    at end
            --       _clean;
            --    end;

            Cleanup_Block_Ent :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('C'));

            Cleanup_Block := SEU.Build_Cleanup_Block (Loc,
              Cleanup_Block_Ent, Cleanup_Stmts, Bnn);

            --  Wrap the cleanup block in an exception handling block

            --  Generate:
            --    begin
            --       Cleanup_Block
            --    exception
            --       when Abort_Signal => Abort_Undefer;
            --    end;

            Abort_Block_Ent :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

            ProtE_Stmts :=
              New_List (
                Make_Implicit_Label_Declaration (Loc,
                  Defining_Identifier => Abort_Block_Ent),

                SEU.Build_Abort_Block (Loc,
                  Abort_Block_Ent, Cleanup_Block_Ent, Cleanup_Block));

            --  Generate:
            --    if not Cancelled (Bnn) then
            --       <triggering-statements>
            --    end if;

            Append_To (ProtE_Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Not (Loc,
                    Right_Opnd =>
                      Make_Function_Call (Loc,
                        Name =>
                          New_Reference_To (RTE (RE_Cancelled), Loc),
                        Parameter_Associations =>
                          New_List (
                            New_Reference_To (Bnn, Loc)))),

                Then_Statements =>
                  New_Copy_List_Tree (Tstats)));

            --  ---------------------------------------------------------------
            --  Task entry handling

            --  Generate:
            --    Param1 := P.Param1;
            --    ...
            --    ParamN := P.ParamN;

            TaskE_Stmts := Parameter_Block_Unpack (Loc, P, Actuals, Formals);

            --  Generate:
            --    _dispatching_asynchronous_select
            --      (<object>, S, P'address, Bnn, B);

            Prepend_To (TaskE_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (
                    Find_Prim_Op (Etype (Etype (Obj)),
                      Name_uDisp_Asynchronous_Select),
                  Loc),
                Parameter_Associations =>
                  New_List (
                    New_Copy_Tree    (Obj),
                    New_Reference_To (S, Loc),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (P, Loc),
                      Attribute_Name => Name_Address),
                    New_Reference_To (Bnn, Loc),
                    New_Reference_To (B, Loc))));

            --  Generate:
            --    Abort_Defer;

            Prepend_To (TaskE_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Abort_Defer), Loc),
                Parameter_Associations =>
                  No_List));

            --  Generate:
            --    Abort_Undefer;
            --    <abortable-statements>

            Cleanup_Stmts := New_Copy_List_Tree (Astats);

            Prepend_To (Cleanup_Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Abort_Undefer), Loc),
                Parameter_Associations =>
                  No_List));

            --  Wrap the statements in a block. Exp_Ch7.Expand_Cleanup_Actions
            --  will generate a _clean for the additional status flag.

            --  Generate:
            --    declare
            --       procedure _clean is
            --       begin
            --          Cancel_Task_Entry_Call (U);
            --       end _clean;
            --    begin
            --       Cleanup_Stmts
            --    at end
            --       _clean;
            --    end;

            Cleanup_Block_Ent :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('C'));

            Cleanup_Block := SEU.Build_Cleanup_Block (Loc,
              Cleanup_Block_Ent, Cleanup_Stmts, T);

            --  Wrap the cleanup block in an exception handling block

            --  Generate:
            --    begin
            --       Cleanup_Block
            --    exception
            --       when Abort_Signal => Abort_Undefer;
            --    end;

            Abort_Block_Ent :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

            Append_To (TaskE_Stmts,
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier => Abort_Block_Ent));

            Append_To (TaskE_Stmts,
              SEU.Build_Abort_Block (Loc,
                Abort_Block_Ent, Cleanup_Block_Ent, Cleanup_Block));

            --  Generate:
            --    if not T then
            --       <triggering-statements>
            --    end if;

            Append_To (TaskE_Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Not (Loc,
                    Right_Opnd =>
                      New_Reference_To (T, Loc)),

                Then_Statements =>
                  New_Copy_List_Tree (Tstats)));

            -------------------------------------------------------------------
            --  Protected procedure handling

            --  Generate:
            --    <dispatching-call>;
            --    <triggering-statements>

            ProtP_Stmts := New_Copy_List_Tree (Tstats);
            Prepend_To (ProtP_Stmts, New_Copy_Tree (Ecall));

            --  Generate:
            --    if C = POK_Procedure_Entry then
            --       ProtE_Stmts
            --    elsif C = POK_Task_Entry then
            --       TaskE_Stmts
            --    else
            --       ProtP_Stmts
            --    end if;

            Append_To (Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Eq (Loc,
                    Left_Opnd =>
                      New_Reference_To (C, Loc),
                    Right_Opnd =>
                      New_Reference_To (RTE (RE_POK_Protected_Entry), Loc)),

                Then_Statements =>
                  ProtE_Stmts,

                Elsif_Parts =>
                  New_List (
                    Make_Elsif_Part (Loc,
                      Condition =>
                        Make_Op_Eq (Loc,
                          Left_Opnd =>
                            New_Reference_To (C, Loc),
                          Right_Opnd =>
                            New_Reference_To (RTE (RE_POK_Task_Entry), Loc)),

                      Then_Statements =>
                        TaskE_Stmts)),

                Else_Statements =>
                  ProtP_Stmts));

            Rewrite (N,
              Make_Block_Statement (Loc,
                Declarations =>
                  Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Stmts)));

            Analyze (N);
            return;

         --  Delay triggering statement processing

         else
            --  Add a Delay_Block object to the parameter list of the delay
            --  procedure to form the parameter list of the Wait entry call.

            Dblock_Ent :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('D'));

            Pdef := Entity (Name (Ecall));

            if Is_RTE (Pdef, RO_CA_Delay_For) then
               Enqueue_Call :=
                 New_Reference_To (RTE (RE_Enqueue_Duration), Loc);

            elsif Is_RTE (Pdef, RO_CA_Delay_Until) then
               Enqueue_Call :=
                 New_Reference_To (RTE (RE_Enqueue_Calendar), Loc);

            else pragma Assert (Is_RTE (Pdef, RO_RT_Delay_Until));
               Enqueue_Call := New_Reference_To (RTE (RE_Enqueue_RT), Loc);
            end if;

            Append_To (Parameter_Associations (Ecall),
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (Dblock_Ent, Loc),
                Attribute_Name => Name_Unchecked_Access));

            --  Create the inner block to protect the abortable part

            Hdle := New_List (
              Make_Exception_Handler (Loc,
                Exception_Choices =>
                  New_List (New_Reference_To (Stand.Abort_Signal, Loc)),
                Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (RTE (RE_Abort_Undefer), Loc)))));

            Prepend_To (Astats,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Abort_Undefer), Loc)));

            Abortable_Block :=
              Make_Block_Statement (Loc,
                Identifier => New_Reference_To (Blk_Ent, Loc),
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Astats),
                Has_Created_Identifier => True,
                Is_Asynchronous_Call_Block => True);

            --  Append call to if Enqueue (When, DB'Unchecked_Access) then

            Rewrite (Ecall,
              Make_Implicit_If_Statement (N,
                Condition => Make_Function_Call (Loc,
                  Name => Enqueue_Call,
                  Parameter_Associations => Parameter_Associations (Ecall)),
                Then_Statements =>
                  New_List (Make_Block_Statement (Loc,
                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => New_List (
                          Make_Implicit_Label_Declaration (Loc,
                            Defining_Identifier => Blk_Ent,
                            Label_Construct     => Abortable_Block),
                          Abortable_Block),
                        Exception_Handlers => Hdle)))));

            Stmts := New_List (Ecall);

            --  Construct statement sequence for new block

            Append_To (Stmts,
              Make_Implicit_If_Statement (N,
                Condition => Make_Function_Call (Loc,
                  Name => New_Reference_To (
                    RTE (RE_Timed_Out), Loc),
                  Parameter_Associations => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Dblock_Ent, Loc),
                      Attribute_Name => Name_Unchecked_Access))),
                Then_Statements => Tstats));

            --  The result is the new block

            Set_Entry_Cancel_Parameter (Blk_Ent, Dblock_Ent);

            Rewrite (N,
              Make_Block_Statement (Loc,
                Declarations => New_List (
                  Make_Object_Declaration (Loc,
                    Defining_Identifier => Dblock_Ent,
                    Aliased_Present => True,
                    Object_Definition => New_Reference_To (
                      RTE (RE_Delay_Block), Loc))),

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Stmts)));

            Analyze (N);
            return;
         end if;

      else
         N_Orig := N;
      end if;

      Extract_Entry (Ecall, Concval, Ename, Index);
      Build_Simple_Entry_Call (Ecall, Concval, Ename, Index);

      Stmts := Statements (Handled_Statement_Sequence (Ecall));
      Decls := Declarations (Ecall);

      if Is_Protected_Type (Etype (Concval)) then

         --  Get the declarations of the block expanded from the entry call

         Decl := First (Decls);
         while Present (Decl)
           and then
             (Nkind (Decl) /= N_Object_Declaration
               or else not Is_RTE (Etype (Object_Definition (Decl)),
                                   RE_Communication_Block))
         loop
            Next (Decl);
         end loop;

         pragma Assert (Present (Decl));
         Cancel_Param := Defining_Identifier (Decl);

         --  Change the mode of the Protected_Entry_Call call

         --  Protected_Entry_Call (
         --    Object => po._object'Access,
         --    E => <entry index>;
         --    Uninterpreted_Data => P'Address;
         --    Mode => Asynchronous_Call;
         --    Block => Bnn);

         Stmt := First (Stmts);

         --  Skip assignments to temporaries created for in-out parameters

         --  This makes unwarranted assumptions about the shape of the expanded
         --  tree for the call, and should be cleaned up ???

         while Nkind (Stmt) /= N_Procedure_Call_Statement loop
            Next (Stmt);
         end loop;

         Call := Stmt;

         Param := First (Parameter_Associations (Call));
         while Present (Param)
           and then not Is_RTE (Etype (Param), RE_Call_Modes)
         loop
            Next (Param);
         end loop;

         pragma Assert (Present (Param));
         Rewrite (Param, New_Reference_To (RTE (RE_Asynchronous_Call), Loc));
         Analyze (Param);

         --  Append an if statement to execute the abortable part

         --  Generate:
         --    if Enqueued (Bnn) then

         Append_To (Stmts,
           Make_Implicit_If_Statement (N,
             Condition => Make_Function_Call (Loc,
               Name => New_Reference_To (
                 RTE (RE_Enqueued), Loc),
               Parameter_Associations => New_List (
                 New_Reference_To (Cancel_Param, Loc))),
             Then_Statements => Astats));

         Abortable_Block :=
           Make_Block_Statement (Loc,
             Identifier => New_Reference_To (Blk_Ent, Loc),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts),
             Has_Created_Identifier => True,
             Is_Asynchronous_Call_Block => True);

         --  For the JVM call Update_Exception instead of Abort_Undefer.
         --  See 4jexcept.ads for an explanation.

         if Hostparm.Java_VM then
            Target_Undefer := RE_Update_Exception;
            Undefer_Args :=
              New_List (Make_Function_Call (Loc,
                          Name => New_Occurrence_Of
                                    (RTE (RE_Current_Target_Exception), Loc)));
         else
            Target_Undefer := RE_Abort_Undefer;
         end if;

         Stmts := New_List (
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Implicit_Label_Declaration (Loc,
                     Defining_Identifier => Blk_Ent,
                     Label_Construct     => Abortable_Block),
                   Abortable_Block),

               --  exception

                 Exception_Handlers => New_List (
                   Make_Exception_Handler (Loc,

               --  when Abort_Signal =>
               --     Abort_Undefer.all;

                     Exception_Choices =>
                       New_List (New_Reference_To (Stand.Abort_Signal, Loc)),
                     Statements => New_List (
                       Make_Procedure_Call_Statement (Loc,
                         Name => New_Reference_To (
                           RTE (Target_Undefer), Loc),
                         Parameter_Associations => Undefer_Args)))))),

         --  if not Cancelled (Bnn) then
         --     triggered statements
         --  end if;

           Make_Implicit_If_Statement (N,
             Condition => Make_Op_Not (Loc,
               Right_Opnd =>
                 Make_Function_Call (Loc,
                   Name => New_Occurrence_Of (RTE (RE_Cancelled), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Cancel_Param, Loc)))),
             Then_Statements => Tstats));

      --  Asynchronous task entry call

      else
         if No (Decls) then
            Decls := New_List;
         end if;

         B := Make_Defining_Identifier (Loc, Name_uB);

         --  Insert declaration of B in declarations of existing block

         Prepend_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => B,
             Object_Definition => New_Reference_To (Standard_Boolean, Loc)));

         Cancel_Param := Make_Defining_Identifier (Loc, Name_uC);

         --  Insert declaration of C in declarations of existing block

         Prepend_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Cancel_Param,
             Object_Definition => New_Reference_To (Standard_Boolean, Loc)));

         --  Remove and save the call to Call_Simple

         Stmt := First (Stmts);

         --  Skip assignments to temporaries created for in-out parameters.
         --  This makes unwarranted assumptions about the shape of the expanded
         --  tree for the call, and should be cleaned up ???

         while Nkind (Stmt) /= N_Procedure_Call_Statement loop
            Next (Stmt);
         end loop;

         Call := Stmt;

         --  Create the inner block to protect the abortable part

         Hdle :=  New_List (
           Make_Exception_Handler (Loc,
             Exception_Choices =>
               New_List (New_Reference_To (Stand.Abort_Signal, Loc)),
             Statements => New_List (
               Make_Procedure_Call_Statement (Loc,
                 Name => New_Reference_To (RTE (RE_Abort_Undefer), Loc)))));

         Prepend_To (Astats,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Abort_Undefer), Loc)));

         Abortable_Block :=
           Make_Block_Statement (Loc,
             Identifier => New_Reference_To (Blk_Ent, Loc),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Astats),
             Has_Created_Identifier => True,
             Is_Asynchronous_Call_Block => True);

         Insert_After (Call,
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Implicit_Label_Declaration (Loc,
                     Defining_Identifier => Blk_Ent,
                     Label_Construct     => Abortable_Block),
                   Abortable_Block),
                 Exception_Handlers => Hdle)));

         --  Create new call statement

         Params := Parameter_Associations (Call);

         Append_To (Params,
           New_Reference_To (RTE (RE_Asynchronous_Call), Loc));
         Append_To (Params,
           New_Reference_To (B, Loc));

         Rewrite (Call,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (RTE (RE_Task_Entry_Call), Loc),
             Parameter_Associations => Params));

         --  Construct statement sequence for new block

         Append_To (Stmts,
           Make_Implicit_If_Statement (N,
             Condition =>
               Make_Op_Not (Loc,
                 New_Reference_To (Cancel_Param, Loc)),
             Then_Statements => Tstats));

         --  Protected the call against abort

         Prepend_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Abort_Defer), Loc),
             Parameter_Associations => Empty_List));
      end if;

      Set_Entry_Cancel_Parameter (Blk_Ent, Cancel_Param);

      --  The result is the new block

      Rewrite (N_Orig,
        Make_Block_Statement (Loc,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Stmts)));

      Analyze (N_Orig);
   end Expand_N_Asynchronous_Select;

   -------------------------------------
   -- Expand_N_Conditional_Entry_Call --
   -------------------------------------

   --  The conditional task entry call is converted to a call to
   --  Task_Entry_Call:

   --    declare
   --       B : Boolean;
   --       P : parms := (parm, parm, parm);

   --    begin
   --       Task_Entry_Call
   --         (acceptor-task,
   --          entry-index,
   --          P'Address,
   --          Conditional_Call,
   --          B);
   --       parm := P.param;
   --       parm := P.param;
   --       ...
   --       if B then
   --          normal-statements
   --       else
   --          else-statements
   --       end if;
   --    end;

   --  For a description of the use of P and the assignments after the
   --  call, see Expand_N_Entry_Call_Statement. Note that the entry call
   --  of the conditional entry call has already been expanded (by the
   --  Expand_N_Entry_Call_Statement procedure) as follows:

   --    declare
   --       P : parms := (parm, parm, parm);
   --    begin
   --       ... info for in-out parameters
   --       Call_Simple (acceptor-task, entry-index, P'Address);
   --       parm := P.param;
   --       parm := P.param;
   --       ...
   --    end;

   --  so the task at hand is to convert the latter expansion into the former

   --  The conditional protected entry call is converted to a call to
   --  Protected_Entry_Call:

   --    declare
   --       P : parms := (parm, parm, parm);
   --       Bnn : Communications_Block;

   --    begin
   --       Protected_Entry_Call (
   --         Object => po._object'Access,
   --         E => <entry index>;
   --         Uninterpreted_Data => P'Address;
   --         Mode => Conditional_Call;
   --         Block => Bnn);
   --       parm := P.param;
   --       parm := P.param;
   --       ...
   --       if Cancelled (Bnn) then
   --          else-statements
   --       else
   --          normal-statements
   --       end if;
   --    end;

   --  As for tasks, the entry call of the conditional entry call has
   --  already been expanded (by the Expand_N_Entry_Call_Statement procedure)
   --  as follows:

   --    declare
   --       P   : E1_Params := (param, param, param);
   --       Bnn : Communications_Block;

   --    begin
   --       Protected_Entry_Call (
   --         Object => po._object'Access,
   --         E => <entry index>;
   --         Uninterpreted_Data => P'Address;
   --         Mode => Simple_Call;
   --         Block => Bnn);
   --       parm := P.param;
   --       parm := P.param;
   --         ...
   --    end;

   --  Ada 2005 (AI-345): A dispatching conditional entry call is converted
   --  into:

   --    declare
   --       B : Boolean := False;
   --       C : Ada.Tags.Prim_Op_Kind;
   --       P : Parameters := (Param1 .. ParamN);
   --       S : constant Integer := DT_Position (<dispatching-procedure>);

   --    begin
   --       disp_conditional_select (<object>, S, P'address, C, B);

   --       if C = POK_Protected_Entry
   --         or else C = POK_Task_Entry
   --       then
   --          Param1 := P.Param1;
   --          ...
   --          ParamN := P.ParamN;
   --       end if;

   --       if B then
   --          if C = POK_Procedure
   --            or else C = POK_Protected_Procedure
   --            or else C = POK_Task_Procedure
   --          then
   --             <dispatching-procedure> (<object>, Param1 .. ParamN);
   --          end if;
   --          <normal-statements>
   --       else
   --          <else-statements>
   --       end if;
   --    end;

   procedure Expand_N_Conditional_Entry_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Alt : constant Node_Id    := Entry_Call_Alternative (N);
      Blk : Node_Id             := Entry_Call_Statement (Alt);
      Transient_Blk : Node_Id;

      Actuals  : List_Id;
      Blk_Typ  : Entity_Id;
      Call     : Node_Id;
      Call_Ent : Entity_Id;
      Decl     : Node_Id;
      Decls    : List_Id;
      Formals  : List_Id;
      N_Stats  : List_Id;
      Obj      : Entity_Id;
      Param    : Node_Id;
      Params   : List_Id;
      Stmt     : Node_Id;
      Stmts    : List_Id;
      Unpack   : List_Id;

      B        : Entity_Id;  --  Call status flag
      C        : Entity_Id;  --  Call kind
      P        : Entity_Id;  --  Parameter block
      S        : Entity_Id;  --  Primitive operation slot

   begin
      if Ada_Version >= Ada_05
        and then Nkind (Blk) = N_Procedure_Call_Statement
      then
         Extract_Dispatching_Call (Blk, Call_Ent, Obj, Actuals, Formals);

         Decls := New_List;
         Stmts := New_List;

         --  Call status flag processing, generate:
         --    B : Boolean := False;

         B := SEU.Build_B (Loc, Decls);

         --  Call kind processing, generate:
         --    C : Ada.Tags.Prim_Op_Kind;

         C := SEU.Build_C (Loc, Decls);

         --  Parameter block processing

         Blk_Typ := Build_Parameter_Block (Loc, Actuals, Formals, Decls);
         P       := Parameter_Block_Pack  (Loc, Blk_Typ, Actuals, Formals,
                      Decls, Stmts);

         --  Dispatch table slot processing, generate:
         --    S : constant Integer :=
         --          Ada.Tags.Get_Offset_Index (
         --            Unchecked_Convert_To (Ada.Tags.Interface_Tag, Obj),
         --            DT_Position (<dispatching-procedure>));

         S := SEU.Build_S (Loc, Decls, Obj, Call_Ent);

         --  Generate:
         --    _dispatching_conditional_select (<object>, S, P'address, C, B);

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (
                 Find_Prim_Op (Etype (Etype (Obj)),
                   Name_uDisp_Conditional_Select),
                 Loc),
             Parameter_Associations =>
               New_List (
                 New_Copy_Tree    (Obj),
                 New_Reference_To (S, Loc),
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (P, Loc),
                   Attribute_Name => Name_Address),
                 New_Reference_To (C, Loc),
                 New_Reference_To (B, Loc))));

         --  Generate:
         --    if C = POK_Protected_Entry
         --      or else C = POK_Task_Entry
         --    then
         --       Param1 := P.Param1;
         --       ...
         --       ParamN := P.ParamN;
         --    end if;

         Unpack := Parameter_Block_Unpack (Loc, P, Actuals, Formals);

         --  Generate the if statement only when the packed parameters need
         --  explicit assignments to their corresponding actuals.

         if Present (Unpack) then
            Append_To (Stmts,
              Make_If_Statement (Loc,

                Condition =>
                  Make_Or_Else (Loc,
                    Left_Opnd =>
                      Make_Op_Eq (Loc,
                        Left_Opnd =>
                          New_Reference_To (C, Loc),
                        Right_Opnd =>
                          New_Reference_To (RTE (
                            RE_POK_Protected_Entry), Loc)),
                    Right_Opnd =>
                      Make_Op_Eq (Loc,
                        Left_Opnd =>
                          New_Reference_To (C, Loc),
                        Right_Opnd =>
                          New_Reference_To (RTE (RE_POK_Task_Entry), Loc))),

                 Then_Statements => Unpack));
         end if;

         --  Generate:
         --    if B then
         --       if C = POK_Procedure
         --         or else C = POK_Protected_Procedure
         --         or else C = POK_Task_Procedure
         --       then
         --          <dispatching-procedure-call>
         --       end if;
         --       <normal-statements>
         --    else
         --       <else-statements>
         --    end if;

         N_Stats := New_Copy_List_Tree (Statements (Alt));

         Prepend_To (N_Stats,
           Make_If_Statement (Loc,
             Condition =>
               Make_Or_Else (Loc,
                 Left_Opnd =>
                   Make_Op_Eq (Loc,
                     Left_Opnd =>
                       New_Reference_To (C, Loc),
                     Right_Opnd =>
                       New_Reference_To (RTE (RE_POK_Procedure), Loc)),

                 Right_Opnd =>
                   Make_Or_Else (Loc,
                     Left_Opnd =>
                       Make_Op_Eq (Loc,
                         Left_Opnd =>
                           New_Reference_To (C, Loc),
                         Right_Opnd =>
                           New_Reference_To (RTE (
                             RE_POK_Protected_Procedure), Loc)),

                     Right_Opnd =>
                       Make_Op_Eq (Loc,
                         Left_Opnd =>
                           New_Reference_To (C, Loc),
                         Right_Opnd =>
                           New_Reference_To (RTE (
                             RE_POK_Task_Procedure), Loc)))),

             Then_Statements =>
               New_List (Blk)));

         Append_To (Stmts,
           Make_If_Statement (Loc,
             Condition       => New_Reference_To (B, Loc),
             Then_Statements => N_Stats,
             Else_Statements => Else_Statements (N)));

         Rewrite (N,
           Make_Block_Statement (Loc,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Stmts)));

      --  As described above, The entry alternative is transformed into a
      --  block that contains the gnulli call, and possibly assignment
      --  statements for in-out parameters. The gnulli call may itself be
      --  rewritten into a transient block if some unconstrained parameters
      --  require it. We need to retrieve the call to complete its parameter
      --  list.

      else
         Transient_Blk :=
            First_Real_Statement (Handled_Statement_Sequence (Blk));

         if Present (Transient_Blk)
           and then Nkind (Transient_Blk) = N_Block_Statement
         then
            Blk := Transient_Blk;
         end if;

         Stmts := Statements (Handled_Statement_Sequence (Blk));
         Stmt  := First (Stmts);
         while Nkind (Stmt) /= N_Procedure_Call_Statement loop
            Next (Stmt);
         end loop;

         Call   := Stmt;
         Params := Parameter_Associations (Call);

         if Is_RTE (Entity (Name (Call)), RE_Protected_Entry_Call) then

            --  Substitute Conditional_Entry_Call for Simple_Call parameter

            Param := First (Params);
            while Present (Param)
              and then not Is_RTE (Etype (Param), RE_Call_Modes)
            loop
               Next (Param);
            end loop;

            pragma Assert (Present (Param));
            Rewrite (Param, New_Reference_To (RTE (RE_Conditional_Call), Loc));

            Analyze (Param);

            --  Find the Communication_Block parameter for the call to the
            --  Cancelled function.

            Decl := First (Declarations (Blk));
            while Present (Decl)
              and then not Is_RTE (Etype (Object_Definition (Decl)),
                             RE_Communication_Block)
            loop
               Next (Decl);
            end loop;

            --  Add an if statement to execute the else part if the call
            --  does not succeed (as indicated by the Cancelled predicate).

            Append_To (Stmts,
              Make_Implicit_If_Statement (N,
                Condition => Make_Function_Call (Loc,
                  Name => New_Reference_To (RTE (RE_Cancelled), Loc),
                  Parameter_Associations => New_List (
                    New_Reference_To (Defining_Identifier (Decl), Loc))),
                Then_Statements => Else_Statements (N),
                Else_Statements => Statements (Alt)));

         else
            B := Make_Defining_Identifier (Loc, Name_uB);

            --  Insert declaration of B in declarations of existing block

            if No (Declarations (Blk)) then
               Set_Declarations (Blk, New_List);
            end if;

            Prepend_To (Declarations (Blk),
              Make_Object_Declaration (Loc,
                Defining_Identifier => B,
                Object_Definition =>
                  New_Reference_To (Standard_Boolean, Loc)));

            --  Create new call statement

            Append_To (Params,
              New_Reference_To (RTE (RE_Conditional_Call), Loc));
            Append_To (Params, New_Reference_To (B, Loc));

            Rewrite (Call,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Task_Entry_Call), Loc),
                Parameter_Associations => Params));

            --  Construct statement sequence for new block

            Append_To (Stmts,
              Make_Implicit_If_Statement (N,
                Condition => New_Reference_To (B, Loc),
                Then_Statements => Statements (Alt),
                Else_Statements => Else_Statements (N)));
         end if;

         --  The result is the new block

         Rewrite (N,
           Make_Block_Statement (Loc,
             Declarations => Declarations (Blk),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Stmts)));
      end if;

      Analyze (N);
   end Expand_N_Conditional_Entry_Call;

   ---------------------------------------
   -- Expand_N_Delay_Relative_Statement --
   ---------------------------------------

   --  Delay statement is implemented as a procedure call to Delay_For
   --  defined in Ada.Calendar.Delays in order to reduce the overhead of
   --  simple delays imposed by the use of Protected Objects.

   procedure Expand_N_Delay_Relative_Statement (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
   begin
      Rewrite (N,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RO_CA_Delay_For), Loc),
          Parameter_Associations => New_List (Expression (N))));
      Analyze (N);
   end Expand_N_Delay_Relative_Statement;

   ------------------------------------
   -- Expand_N_Delay_Until_Statement --
   ------------------------------------

   --  Delay Until statement is implemented as a procedure call to
   --  Delay_Until defined in Ada.Calendar.Delays and Ada.Real_Time.Delays.

   procedure Expand_N_Delay_Until_Statement (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : Entity_Id;

   begin
      if Is_RTE (Base_Type (Etype (Expression (N))), RO_CA_Time) then
         Typ := RTE (RO_CA_Delay_Until);
      else
         Typ := RTE (RO_RT_Delay_Until);
      end if;

      Rewrite (N,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (Typ, Loc),
          Parameter_Associations => New_List (Expression (N))));

      Analyze (N);
   end Expand_N_Delay_Until_Statement;

   -------------------------
   -- Expand_N_Entry_Body --
   -------------------------

   procedure Expand_N_Entry_Body (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Dec         : constant Node_Id    := Parent (Current_Scope);
      Ent_Formals : constant Node_Id    := Entry_Body_Formal_Part (N);
      Index_Spec  : constant Node_Id    :=
                      Entry_Index_Specification (Ent_Formals);
      Next_Op     : Node_Id;
      First_Decl  : constant Node_Id := First (Declarations (N));
      Index_Decl  : List_Id;

   begin
      --  Add the renamings for private declarations and discriminants

      Add_Discriminal_Declarations
        (Declarations (N), Defining_Identifier (Dec), Name_uObject, Loc);
      Add_Private_Declarations
        (Declarations (N), Defining_Identifier (Dec), Name_uObject, Loc);

      if Present (Index_Spec) then
         Index_Decl :=
           Index_Constant_Declaration
             (N,
               Defining_Identifier (Index_Spec), Defining_Identifier (Dec));

         --  If the entry has local declarations, insert index declaration
         --  before them, because the index may be used therein.

         if Present (First_Decl) then
            Insert_List_Before (First_Decl, Index_Decl);
         else
            Append_List_To (Declarations (N), Index_Decl);
         end if;
      end if;

      --  Associate privals and discriminals with the next protected operation
      --  body to be expanded. These are used to expand references to private
      --  data objects and discriminants, respectively.

      Next_Op := Next_Protected_Operation (N);

      if Present (Next_Op) then
         Set_Privals (Dec, Next_Op, Loc);
         Set_Discriminals (Dec);
      end if;
   end Expand_N_Entry_Body;

   -----------------------------------
   -- Expand_N_Entry_Call_Statement --
   -----------------------------------

   --  An entry call is expanded into GNARLI calls to implement
   --  a simple entry call (see Build_Simple_Entry_Call).

   procedure Expand_N_Entry_Call_Statement (N : Node_Id) is
      Concval : Node_Id;
      Ename   : Node_Id;
      Index   : Node_Id;

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("entry call", N);
         return;
      end if;

      --  If this entry call is part of an asynchronous select, don't expand it
      --  here; it will be expanded with the select statement. Don't expand
      --  timed entry calls either, as they are translated into asynchronous
      --  entry calls.

      --  ??? This whole approach is questionable; it may be better to go back
      --  to allowing the expansion to take place and then attempting to fix it
      --  up in Expand_N_Asynchronous_Select. The tricky part is figuring out
      --  whether the expanded call is on a task or protected entry.

      if (Nkind (Parent (N)) /= N_Triggering_Alternative
           or else N /= Triggering_Statement (Parent (N)))
        and then (Nkind (Parent (N)) /= N_Entry_Call_Alternative
                   or else N /= Entry_Call_Statement (Parent (N))
                   or else Nkind (Parent (Parent (N))) /= N_Timed_Entry_Call)
      then
         Extract_Entry (N, Concval, Ename, Index);
         Build_Simple_Entry_Call (N, Concval, Ename, Index);
      end if;
   end Expand_N_Entry_Call_Statement;

   --------------------------------
   -- Expand_N_Entry_Declaration --
   --------------------------------

   --  If there are parameters, then first, each of the formals is marked by
   --  setting Is_Entry_Formal. Next a record type is built which is used to
   --  hold the parameter values. The name of this record type is entryP where
   --  entry is the name of the entry, with an additional corresponding access
   --  type called entryPA. The record type has matching components for each
   --  formal (the component names are the same as the formal names). For
   --  elementary types, the component type matches the formal type. For
   --  composite types, an access type is declared (with the name formalA)
   --  which designates the formal type, and the type of the component is this
   --  access type. Finally the Entry_Component of each formal is set to
   --  reference the corresponding record component.

   procedure Expand_N_Entry_Declaration (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Entry_Ent  : constant Entity_Id  := Defining_Identifier (N);
      Components : List_Id;
      Formal     : Node_Id;
      Ftype      : Entity_Id;
      Last_Decl  : Node_Id;
      Component  : Entity_Id;
      Ctype      : Entity_Id;
      Decl       : Node_Id;
      Rec_Ent    : Entity_Id;
      Acc_Ent    : Entity_Id;

   begin
      Formal := First_Formal (Entry_Ent);
      Last_Decl := N;

      --  Most processing is done only if parameters are present

      if Present (Formal) then
         Components := New_List;

         --  Loop through formals

         while Present (Formal) loop
            Set_Is_Entry_Formal (Formal);
            Component :=
              Make_Defining_Identifier (Sloc (Formal), Chars (Formal));
            Set_Entry_Component (Formal, Component);
            Set_Entry_Formal (Component, Formal);
            Ftype := Etype (Formal);

            --  Declare new access type and then append

            Ctype :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

            Decl :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Ctype,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present        => True,
                    Constant_Present   => Ekind (Formal) = E_In_Parameter,
                    Subtype_Indication => New_Reference_To (Ftype, Loc)));

            Insert_After (Last_Decl, Decl);
            Last_Decl := Decl;

            Append_To (Components,
              Make_Component_Declaration (Loc,
                Defining_Identifier => Component,
                Component_Definition =>
                  Make_Component_Definition (Loc,
                    Aliased_Present    => False,
                    Subtype_Indication => New_Reference_To (Ctype, Loc))));

            Next_Formal_With_Extras (Formal);
         end loop;

         --  Create the Entry_Parameter_Record declaration

         Rec_Ent :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

         Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Rec_Ent,
             Type_Definition     =>
               Make_Record_Definition (Loc,
                 Component_List =>
                   Make_Component_List (Loc,
                     Component_Items => Components)));

         Insert_After (Last_Decl, Decl);
         Last_Decl := Decl;

         --  Construct and link in the corresponding access type

         Acc_Ent :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

         Set_Entry_Parameters_Type (Entry_Ent, Acc_Ent);

         Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Acc_Ent,
             Type_Definition     =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present        => True,
                 Subtype_Indication => New_Reference_To (Rec_Ent, Loc)));

         Insert_After (Last_Decl, Decl);
         Last_Decl := Decl;
      end if;
   end Expand_N_Entry_Declaration;

   -----------------------------
   -- Expand_N_Protected_Body --
   -----------------------------

   --  Protected bodies are expanded to the completion of the subprograms
   --  created for the corresponding protected type. These are a protected and
   --  unprotected version of each protected subprogram in the object, a
   --  function to calculate each entry barrier, and a procedure to execute the
   --  sequence of statements of each protected entry body. For example, for
   --  protected type ptype:

   --  function entB
   --    (O : System.Address;
   --     E : Protected_Entry_Index)
   --     return Boolean
   --  is
   --     <discriminant renamings>
   --     <private object renamings>
   --  begin
   --     return <barrier expression>;
   --  end entB;

   --  procedure pprocN (_object : in out poV;...) is
   --     <discriminant renamings>
   --     <private object renamings>
   --  begin
   --     <sequence of statements>
   --  end pprocN;

   --  procedure pprocP (_object : in out poV;...) is
   --     procedure _clean is
   --       Pn : Boolean;
   --     begin
   --       ptypeS (_object, Pn);
   --       Unlock (_object._object'Access);
   --       Abort_Undefer.all;
   --     end _clean;

   --  begin
   --     Abort_Defer.all;
   --     Lock (_object._object'Access);
   --     pprocN (_object;...);
   --  at end
   --     _clean;
   --  end pproc;

   --  function pfuncN (_object : poV;...) return Return_Type is
   --     <discriminant renamings>
   --     <private object renamings>
   --  begin
   --     <sequence of statements>
   --  end pfuncN;

   --  function pfuncP (_object : poV) return Return_Type is
   --     procedure _clean is
   --     begin
   --        Unlock (_object._object'Access);
   --        Abort_Undefer.all;
   --     end _clean;

   --  begin
   --     Abort_Defer.all;
   --     Lock (_object._object'Access);
   --     return pfuncN (_object);

   --  at end
   --     _clean;
   --  end pfunc;

   --  procedure entE
   --    (O : System.Address;
   --     P : System.Address;
   --     E : Protected_Entry_Index)
   --  is
   --     <discriminant renamings>
   --     <private object renamings>
   --     type poVP is access poV;
   --     _Object : ptVP := ptVP!(O);

   --  begin
   --     begin
   --        <statement sequence>
   --        Complete_Entry_Body (_Object._Object);
   --     exception
   --        when all others =>
   --           Exceptional_Complete_Entry_Body (
   --             _Object._Object, Get_GNAT_Exception);
   --     end;
   --  end entE;

   --  The type poV is the record created for the protected type to hold
   --  the state of the protected object.

   procedure Expand_N_Protected_Body (N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (N);
      Pid          : constant Entity_Id  := Corresponding_Spec (N);
      Has_Entries  : Boolean := False;
      Op_Decl      : Node_Id;
      Op_Body      : Node_Id;
      Op_Id        : Entity_Id;
      Disp_Op_Body : Node_Id;
      New_Op_Body  : Node_Id;
      Current_Node : Node_Id;
      Num_Entries  : Natural := 0;

      function Build_Dispatching_Subprogram_Body
        (N        : Node_Id;
         Pid      : Node_Id;
         Prot_Bod : Node_Id) return Node_Id;
      --  Build a dispatching version of the protected subprogram body. The
      --  newly generated subprogram contains a call to the original protected
      --  body. The following code is generated:
      --
      --  function <protected-function-name> (Param1 .. ParamN) return
      --    <return-type> is
      --  begin
      --     return <protected-function-name>P (Param1 .. ParamN);
      --  end <protected-function-name>;
      --
      --  or
      --
      --  procedure <protected-procedure-name> (Param1 .. ParamN) is
      --  begin
      --     <protected-procedure-name>P (Param1 .. ParamN);
      --  end <protected-procedure-name>

      ---------------------------------------
      -- Build_Dispatching_Subprogram_Body --
      ---------------------------------------

      function Build_Dispatching_Subprogram_Body
        (N        : Node_Id;
         Pid      : Node_Id;
         Prot_Bod : Node_Id) return Node_Id
      is
         Loc     : constant Source_Ptr := Sloc (N);
         Actuals : List_Id;
         Formal  : Node_Id;
         Spec    : Node_Id;
         Stmts   : List_Id;

      begin
         --  Generate a specification without a letter suffix in order to
         --  override an interface function or procedure.

         Spec :=
           Build_Protected_Sub_Specification (N, Pid, Dispatching_Mode);

         --  The formal parameters become the actuals of the protected
         --  function or procedure call.

         Actuals := New_List;
         Formal  := First (Parameter_Specifications (Spec));
         while Present (Formal) loop
            Append_To (Actuals,
              Make_Identifier (Loc, Chars (Defining_Identifier (Formal))));

            Next (Formal);
         end loop;

         if Nkind (Spec) = N_Procedure_Specification then
            Stmts :=
              New_List (
                Make_Procedure_Call_Statement (Loc,
                  Name =>
                    New_Reference_To (Corresponding_Spec (Prot_Bod), Loc),
                  Parameter_Associations => Actuals));
         else
            pragma Assert (Nkind (Spec) = N_Function_Specification);

            Stmts :=
              New_List (
                Make_Return_Statement (Loc,
                  Expression =>
                    Make_Function_Call (Loc,
                      Name =>
                        New_Reference_To (Corresponding_Spec (Prot_Bod), Loc),
                      Parameter_Associations => Actuals)));
         end if;

         return
           Make_Subprogram_Body (Loc,
             Declarations  => Empty_List,
             Specification => Spec,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Stmts));
      end Build_Dispatching_Subprogram_Body;

   --  Start of processing for Expand_N_Protected_Body

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("protected body", N);
         return;
      end if;

      if Nkind (Parent (N)) = N_Subunit then

         --  This is the proper body corresponding to a stub. The declarations
         --  must be inserted at the point of the stub, which is in the decla-
         --  rative part of the parent unit.

         Current_Node := Corresponding_Stub (Parent (N));

      else
         Current_Node := N;
      end if;

      Op_Body := First (Declarations (N));

      --  The protected body is replaced with the bodies of its
      --  protected operations, and the declarations for internal objects
      --  that may have been created for entry family bounds.

      Rewrite (N, Make_Null_Statement (Sloc (N)));
      Analyze (N);

      while Present (Op_Body) loop
         case Nkind (Op_Body) is
            when N_Subprogram_Declaration =>
               null;

            when N_Subprogram_Body =>

               --  Exclude functions created to analyze defaults

               if not Is_Eliminated (Defining_Entity (Op_Body))
                 and then not Is_Eliminated (Corresponding_Spec (Op_Body))
               then
                  New_Op_Body :=
                    Build_Unprotected_Subprogram_Body (Op_Body, Pid);

                  Insert_After (Current_Node, New_Op_Body);
                  Current_Node := New_Op_Body;
                  Analyze (New_Op_Body);

                  Update_Prival_Subtypes (New_Op_Body);

                  --  Build the corresponding protected operation only if
                  --  this is a visible operation of the type, or if it is
                  --  an interrupt handler. Otherwise it is only callable
                  --  from within the object, and the unprotected version
                  --  is sufficient.

                  if Present (Corresponding_Spec (Op_Body)) then
                     Op_Decl :=
                       Unit_Declaration_Node (Corresponding_Spec (Op_Body));

                     if Nkind (Parent (Op_Decl)) = N_Protected_Definition
                       and then
                         (List_Containing (Op_Decl) =
                                  Visible_Declarations (Parent (Op_Decl))
                           or else
                            Is_Interrupt_Handler
                              (Corresponding_Spec (Op_Body)))
                     then
                        New_Op_Body :=
                           Build_Protected_Subprogram_Body (
                             Op_Body, Pid, Specification (New_Op_Body));

                        Insert_After (Current_Node, New_Op_Body);
                        Analyze (New_Op_Body);

                        Current_Node := New_Op_Body;

                        --  Generate an overriding primitive operation body for
                        --  this subprogram if the protected type implements
                        --  an inerface.

                        if Ada_Version >= Ada_05
                          and then Present (Abstract_Interfaces (
                                     Corresponding_Record_Type (Pid)))
                        then
                           Disp_Op_Body :=
                             Build_Dispatching_Subprogram_Body (
                               Op_Body, Pid, New_Op_Body);

                           Insert_After (Current_Node, Disp_Op_Body);
                           Analyze (Disp_Op_Body);

                           Current_Node := Disp_Op_Body;
                        end if;
                     end if;
                  end if;
               end if;

            when N_Entry_Body =>
               Op_Id := Defining_Identifier (Op_Body);
               Has_Entries := True;
               Num_Entries := Num_Entries + 1;

               New_Op_Body := Build_Protected_Entry (Op_Body, Op_Id, Pid);

               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

               Update_Prival_Subtypes (New_Op_Body);

            when N_Implicit_Label_Declaration =>
               null;

            when N_Itype_Reference =>
               Insert_After (Current_Node, New_Copy (Op_Body));

            when N_Freeze_Entity =>
               New_Op_Body := New_Copy (Op_Body);

               if Present (Entity (Op_Body))
                 and then Freeze_Node (Entity (Op_Body)) = Op_Body
               then
                  Set_Freeze_Node (Entity (Op_Body), New_Op_Body);
               end if;

               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

            when N_Pragma =>
               New_Op_Body := New_Copy (Op_Body);
               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

            when N_Object_Declaration =>
               pragma Assert (not Comes_From_Source (Op_Body));
               New_Op_Body := New_Copy (Op_Body);
               Insert_After (Current_Node, New_Op_Body);
               Current_Node := New_Op_Body;
               Analyze (New_Op_Body);

            when others =>
               raise Program_Error;

         end case;

         Next (Op_Body);
      end loop;

      --  Finally, create the body of the function that maps an entry index
      --  into the corresponding body index, except when there is no entry,
      --  or in a ravenscar-like profile (no abort, no entry queue, 1 entry)

      if Has_Entries
        and then (Abort_Allowed
                    or else Restriction_Active (No_Entry_Queue) = False
                    or else Num_Entries > 1)
      then
         New_Op_Body := Build_Find_Body_Index (Pid);
         Insert_After (Current_Node, New_Op_Body);
         Current_Node := New_Op_Body;
         Analyze (New_Op_Body);
      end if;

      --  Ada 2005 (AI-345): Construct the primitive entry wrapper bodies after
      --  the protected body. At this point the entry specs have been created,
      --  frozen and included in the dispatch table for the protected type.

      pragma Assert (Present (Corresponding_Record_Type (Pid)));

      if Ada_Version >= Ada_05
        and then Present (Protected_Definition (Parent (Pid)))
        and then Present (Abstract_Interfaces
                          (Corresponding_Record_Type (Pid)))
      then
         declare
            Vis_Decl  : Node_Id :=
                          First (Visible_Declarations
                                 (Protected_Definition (Parent (Pid))));
            Wrap_Body : Node_Id;

         begin
            --  Examine the visible declarations of the protected type, looking
            --  for an entry declaration. We do not consider entry families
            --  since they cannot have dispatching operations, thus they do not
            --  need entry wrappers.

            while Present (Vis_Decl) loop
               if Nkind (Vis_Decl) = N_Entry_Declaration then
                  Wrap_Body :=
                    Build_Wrapper_Body (Loc,
                      Proc_Nam => Defining_Identifier (Vis_Decl),
                      Obj_Typ  => Corresponding_Record_Type (Pid),
                      Formals  => Parameter_Specifications (Vis_Decl));

                  if Wrap_Body /= Empty then
                     Insert_After (Current_Node, Wrap_Body);
                     Current_Node := Wrap_Body;

                     Analyze (Wrap_Body);
                  end if;

               elsif Nkind (Vis_Decl) = N_Subprogram_Declaration then
                  Wrap_Body :=
                    Build_Wrapper_Body (Loc,
                      Proc_Nam => Defining_Unit_Name
                                        (Specification (Vis_Decl)),
                      Obj_Typ  => Corresponding_Record_Type (Pid),
                      Formals  => Parameter_Specifications
                                        (Specification (Vis_Decl)));

                  if Wrap_Body /= Empty then
                     Insert_After (Current_Node, Wrap_Body);
                     Current_Node := Wrap_Body;

                     Analyze (Wrap_Body);
                  end if;
               end if;

               Next (Vis_Decl);
            end loop;
         end;
      end if;
   end Expand_N_Protected_Body;

   -----------------------------------------
   -- Expand_N_Protected_Type_Declaration --
   -----------------------------------------

   --  First we create a corresponding record type declaration used to
   --  represent values of this protected type.
   --  The general form of this type declaration is

   --    type poV (discriminants) is record
   --      _Object       : aliased <kind>Protection
   --         [(<entry count> [, <handler count>])];
   --      [entry_family  : array (bounds) of Void;]
   --      <private data fields>
   --    end record;

   --  The discriminants are present only if the corresponding protected type
   --  has discriminants, and they exactly mirror the protected type
   --  discriminants. The private data fields similarly mirror the private
   --  declarations of the protected type.

   --  The Object field is always present. It contains RTS specific data used
   --  to control the protected object. It is declared as Aliased so that it
   --  can be passed as a pointer to the RTS. This allows the protected record
   --  to be referenced within RTS data structures. An appropriate Protection
   --  type and discriminant are generated.

   --  The Service field is present for protected objects with entries. It
   --  contains sufficient information to allow the entry service procedure for
   --  this object to be called when the object is not known till runtime.

   --  One entry_family component is present for each entry family in the
   --  task definition (see Expand_N_Task_Type_Declaration).

   --  When a protected object is declared, an instance of the protected type
   --  value record is created. The elaboration of this declaration creates the
   --  correct bounds for the entry families, and also evaluates the priority
   --  expression if needed. The initialization routine for the protected type
   --  itself then calls Initialize_Protection with appropriate parameters to
   --  initialize the value of the Task_Id field. Install_Handlers may be also
   --  called if a pragma Attach_Handler applies.

   --  Note: this record is passed to the subprograms created by the expansion
   --  of protected subprograms and entries. It is an in parameter to protected
   --  functions and an in out parameter to procedures and entry bodies. The
   --  Entity_Id for this created record type is placed in the
   --  Corresponding_Record_Type field of the associated protected type entity.

   --  Next we create a procedure specifications for protected subprograms and
   --  entry bodies. For each protected subprograms two subprograms are
   --  created, an unprotected and a protected version. The unprotected version
   --  is called from within other operations of the same protected object.

   --  We also build the call to register the procedure if a pragma
   --  Interrupt_Handler applies.

   --  A single subprogram is created to service all entry bodies; it has an
   --  additional boolean out parameter indicating that the previous entry call
   --  made by the current task was serviced immediately, i.e. not by proxy.
   --  The O parameter contains a pointer to a record object of the type
   --  described above. An untyped interface is used here to allow this
   --  procedure to be called in places where the type of the object to be
   --  serviced is not known. This must be done, for example, when a call that
   --  may have been requeued is cancelled; the corresponding object must be
   --  serviced, but which object that is not known till runtime.

   --  procedure ptypeS
   --    (O : System.Address; P : out Boolean);
   --  procedure pprocN (_object : in out poV);
   --  procedure pproc (_object : in out poV);
   --  function pfuncN (_object : poV);
   --  function pfunc (_object : poV);
   --  ...

   --  Note that this must come after the record type declaration, since
   --  the specs refer to this type.

   procedure Expand_N_Protected_Type_Declaration (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Prottyp : constant Entity_Id  := Defining_Identifier (N);

      Pdef : constant Node_Id := Protected_Definition (N);
      --  This contains two lists; one for visible and one for private decls

      Rec_Decl     : Node_Id;
      Cdecls       : List_Id;
      Discr_Map    : constant Elist_Id := New_Elmt_List;
      Priv         : Node_Id;
      Pent         : Entity_Id;
      New_Priv     : Node_Id;
      Comp         : Node_Id;
      Comp_Id      : Entity_Id;
      Sub          : Node_Id;
      Current_Node : Node_Id := N;
      Bdef         : Entity_Id := Empty; -- avoid uninit warning
      Edef         : Entity_Id := Empty; -- avoid uninit warning
      Entries_Aggr : Node_Id;
      Body_Id      : Entity_Id;
      Body_Arr     : Node_Id;
      E_Count      : Int;
      Object_Comp  : Node_Id;

      procedure Register_Handler;
      --  For a protected operation that is an interrupt handler, add the
      --  freeze action that will register it as such.

      ----------------------
      -- Register_Handler --
      ----------------------

      procedure Register_Handler is

         --  All semantic checks already done in Sem_Prag

         Prot_Proc    : constant Entity_Id :=
                       Defining_Unit_Name
                         (Specification (Current_Node));

         Proc_Address : constant Node_Id :=
                          Make_Attribute_Reference (Loc,
                          Prefix => New_Reference_To (Prot_Proc, Loc),
                          Attribute_Name => Name_Address);

         RTS_Call     : constant Entity_Id :=
                          Make_Procedure_Call_Statement (Loc,
                            Name =>
                              New_Reference_To (
                                RTE (RE_Register_Interrupt_Handler), Loc),
                            Parameter_Associations =>
                              New_List (Proc_Address));
      begin
         Append_Freeze_Action (Prot_Proc, RTS_Call);
      end Register_Handler;

   --  Start of processing for Expand_N_Protected_Type_Declaration

   begin
      if Present (Corresponding_Record_Type (Prottyp)) then
         return;
      else
         Rec_Decl := Build_Corresponding_Record (N, Prottyp, Loc);
         Cdecls   := Component_Items
                      (Component_List (Type_Definition (Rec_Decl)));
      end if;

      --  Ada 2005 (AI-345): Propagate the attribute that contains the list
      --  of implemented interfaces.

      Set_Interface_List (Type_Definition (Rec_Decl), Interface_List (N));

      Qualify_Entity_Names (N);

      --  If the type has discriminants, their occurrences in the declaration
      --  have been replaced by the corresponding discriminals. For components
      --  that are constrained by discriminants, their homologues in the
      --  corresponding record type must refer to the discriminants of that
      --  record, so we must apply a new renaming to subtypes_indications:

      --     protected discriminant => discriminal => record discriminant

      --  This replacement is not applied to default expressions, for which
      --  the discriminal is correct.

      if Has_Discriminants (Prottyp) then
         declare
            Disc : Entity_Id;
            Decl : Node_Id;
         begin
            Disc := First_Discriminant (Prottyp);
            Decl := First (Discriminant_Specifications (Rec_Decl));
            while Present (Disc) loop
               Append_Elmt (Discriminal (Disc), Discr_Map);
               Append_Elmt (Defining_Identifier (Decl), Discr_Map);
               Next_Discriminant (Disc);
               Next (Decl);
            end loop;
         end;
      end if;

      --  Fill in the component declarations

      --  Add components for entry families. For each entry family, create an
      --  anonymous type declaration with the same size, and analyze the type.

      Collect_Entry_Families (Loc, Cdecls, Current_Node, Prottyp);

      --  Prepend the _Object field with the right type to the component list.
      --  We need to compute the number of entries, and in some cases the
      --  number of Attach_Handler pragmas.

      declare
         Ritem              : Node_Id;
         Num_Attach_Handler : Int := 0;
         Protection_Subtype : Node_Id;
         Entry_Count_Expr   : constant Node_Id :=
                                Build_Entry_Count_Expression
                                  (Prottyp, Cdecls, Loc);

      begin
         if Has_Attach_Handler (Prottyp) then
            Ritem := First_Rep_Item (Prottyp);
            while Present (Ritem) loop
               if Nkind (Ritem) = N_Pragma
                 and then Chars (Ritem) = Name_Attach_Handler
               then
                  Num_Attach_Handler := Num_Attach_Handler + 1;
               end if;

               Next_Rep_Item (Ritem);
            end loop;

            if Restricted_Profile then
               if Has_Entries (Prottyp) then
                  Protection_Subtype :=
                    New_Reference_To (RTE (RE_Protection_Entry), Loc);
               else
                  Protection_Subtype :=
                    New_Reference_To (RTE (RE_Protection), Loc);
               end if;
            else
               Protection_Subtype :=
                 Make_Subtype_Indication
                   (Sloc => Loc,
                    Subtype_Mark =>
                      New_Reference_To
                        (RTE (RE_Static_Interrupt_Protection), Loc),
                    Constraint =>
                      Make_Index_Or_Discriminant_Constraint (
                        Sloc => Loc,
                        Constraints => New_List (
                          Entry_Count_Expr,
                          Make_Integer_Literal (Loc, Num_Attach_Handler))));
            end if;

         elsif Has_Interrupt_Handler (Prottyp) then
            Protection_Subtype :=
               Make_Subtype_Indication (
                 Sloc => Loc,
                 Subtype_Mark => New_Reference_To
                   (RTE (RE_Dynamic_Interrupt_Protection), Loc),
                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (
                     Sloc => Loc,
                     Constraints => New_List (Entry_Count_Expr)));

         --  Type has explicit entries or generated primitive entry wrappers

         elsif Has_Entries (Prottyp)
           or else (Ada_Version >= Ada_05
                      and then Present (Interface_List (N)))
         then
            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Prottyp) > 1
            then
               Protection_Subtype :=
                  Make_Subtype_Indication (
                    Sloc => Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RE_Protection_Entries), Loc),
                    Constraint =>
                      Make_Index_Or_Discriminant_Constraint (
                        Sloc => Loc,
                        Constraints => New_List (Entry_Count_Expr)));

            else
               Protection_Subtype :=
                 New_Reference_To (RTE (RE_Protection_Entry), Loc);
            end if;

         else
            Protection_Subtype := New_Reference_To (RTE (RE_Protection), Loc);
         end if;

         Object_Comp :=
           Make_Component_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uObject),
             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => True,
                 Subtype_Indication => Protection_Subtype));
      end;

      pragma Assert (Present (Pdef));

      --  Add private field components

      if Present (Private_Declarations (Pdef)) then
         Priv := First (Private_Declarations (Pdef));

         while Present (Priv) loop

            if Nkind (Priv) = N_Component_Declaration then
               Pent := Defining_Identifier (Priv);
               New_Priv :=
                 Make_Component_Declaration (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Sloc (Pent), Chars (Pent)),
                   Component_Definition =>
                     Make_Component_Definition (Sloc (Pent),
                       Aliased_Present    => False,
                       Subtype_Indication =>
                         New_Copy_Tree (Subtype_Indication
                                         (Component_Definition (Priv)),
                                        Discr_Map)),
                   Expression => Expression (Priv));

               Append_To (Cdecls, New_Priv);

            elsif Nkind (Priv) = N_Subprogram_Declaration then

               --  Make the unprotected version of the subprogram available
               --  for expansion of intra object calls. There is need for
               --  a protected version only if the subprogram is an interrupt
               --  handler, otherwise  this operation can only be called from
               --  within the body.

               Sub :=
                 Make_Subprogram_Declaration (Loc,
                   Specification =>
                     Build_Protected_Sub_Specification
                       (Priv, Prottyp, Unprotected_Mode));

               Insert_After (Current_Node, Sub);
               Analyze (Sub);

               Set_Protected_Body_Subprogram
                 (Defining_Unit_Name (Specification (Priv)),
                  Defining_Unit_Name (Specification (Sub)));

               Current_Node := Sub;

               if Is_Interrupt_Handler
                 (Defining_Unit_Name (Specification (Priv)))
               then
                  Sub :=
                    Make_Subprogram_Declaration (Loc,
                      Specification =>
                        Build_Protected_Sub_Specification
                          (Priv, Prottyp, Protected_Mode));

                  Insert_After (Current_Node, Sub);
                  Analyze (Sub);
                  Current_Node := Sub;

                  if not Restricted_Profile then
                     Register_Handler;
                  end if;
               end if;
            end if;

            Next (Priv);
         end loop;
      end if;

      --  Put the _Object component after the private component so that it
      --  be finalized early as required by 9.4 (20)

      Append_To (Cdecls, Object_Comp);

      Insert_After (Current_Node, Rec_Decl);
      Current_Node := Rec_Decl;

      --  Analyze the record declaration immediately after construction,
      --  because the initialization procedure is needed for single object
      --  declarations before the next entity is analyzed (the freeze call
      --  that generates this initialization procedure is found below).

      Analyze (Rec_Decl, Suppress => All_Checks);

      --  Ada 2005 (AI-345): Construct the primitive entry wrappers before
      --  the corresponding record is frozen

      if Ada_Version >= Ada_05
        and then Present (Visible_Declarations (Pdef))
        and then Present (Corresponding_Record_Type
                          (Defining_Identifier (Parent (Pdef))))
        and then Present (Abstract_Interfaces
                          (Corresponding_Record_Type
                           (Defining_Identifier (Parent (Pdef)))))
      then
         declare
            Current_Node : Node_Id := Rec_Decl;
            Vis_Decl     : Node_Id;
            Wrap_Spec    : Node_Id;
            New_N        : Node_Id;

         begin
            --  Examine the visible declarations of the protected type, looking
            --  for declarations of entries, and subprograms. We do not
            --  consider entry families since they cannot have dispatching
            --  operations, thus they do not need entry wrappers.

            Vis_Decl := First (Visible_Declarations (Pdef));

            while Present (Vis_Decl) loop

               Wrap_Spec := Empty;

               if Nkind (Vis_Decl) = N_Entry_Declaration
                 and then not Present (Discrete_Subtype_Definition (Vis_Decl))
               then
                  Wrap_Spec :=
                    Build_Wrapper_Spec (Loc,
                      Proc_Nam => Defining_Identifier (Vis_Decl),
                      Obj_Typ  => Defining_Identifier (Rec_Decl),
                      Formals  => Parameter_Specifications (Vis_Decl));

               elsif Nkind (Vis_Decl) = N_Subprogram_Declaration then
                  Wrap_Spec :=
                    Build_Wrapper_Spec (Loc,
                      Proc_Nam => Defining_Unit_Name
                                    (Specification (Vis_Decl)),
                      Obj_Typ  => Defining_Identifier (Rec_Decl),
                      Formals  => Parameter_Specifications
                                    (Specification (Vis_Decl)));

               end if;

               if Wrap_Spec /= Empty then
                  New_N := Make_Subprogram_Declaration (Loc,
                             Specification => Wrap_Spec);

                  Insert_After (Current_Node, New_N);
                  Current_Node := New_N;

                  Analyze (New_N);
               end if;

               Next (Vis_Decl);
            end loop;
         end;
      end if;

      --  Collect pointers to entry bodies and their barriers, to be placed
      --  in the Entry_Bodies_Array for the type. For each entry/family we
      --  add an expression to the aggregate which is the initial value of
      --  this array. The array is declared after all protected subprograms.

      if Has_Entries (Prottyp) then
         Entries_Aggr :=
           Make_Aggregate (Loc, Expressions => New_List);

      else
         Entries_Aggr := Empty;
      end if;

      --  Build two new procedure specifications for each protected subprogram;
      --  one to call from outside the object and one to call from inside.
      --  Build a barrier function and an entry body action procedure
      --  specification for each protected entry. Initialize the entry body
      --  array. If subprogram is flagged as eliminated, do not generate any
      --  internal operations.

      E_Count := 0;

      Comp := First (Visible_Declarations (Pdef));

      while Present (Comp) loop
         if Nkind (Comp) = N_Subprogram_Declaration
           and then not Is_Eliminated (Defining_Entity (Comp))
         then
            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Sub_Specification
                    (Comp, Prottyp, Unprotected_Mode));

            Insert_After (Current_Node, Sub);
            Analyze (Sub);

            Set_Protected_Body_Subprogram
              (Defining_Unit_Name (Specification (Comp)),
               Defining_Unit_Name (Specification (Sub)));

            --  Make the protected version of the subprogram available for
            --  expansion of external calls.

            Current_Node := Sub;

            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Sub_Specification
                    (Comp, Prottyp, Protected_Mode));

            Insert_After (Current_Node, Sub);
            Analyze (Sub);

            Current_Node := Sub;

            --  Generate an overriding primitive operation specification for
            --  this subprogram if the protected type implements an inerface.

            if Ada_Version >= Ada_05
              and then
                Present (Abstract_Interfaces
                          (Corresponding_Record_Type (Prottyp)))
            then
               Sub :=
                 Make_Subprogram_Declaration (Loc,
                   Specification =>
                     Build_Protected_Sub_Specification
                       (Comp, Prottyp, Dispatching_Mode));

               Insert_After (Current_Node, Sub);
               Analyze (Sub);

               Current_Node := Sub;
            end if;

            --  If a pragma Interrupt_Handler applies, build and add a call to
            --  Register_Interrupt_Handler to the freezing actions of the
            --  protected version (Current_Node) of the subprogram:

            --    system.interrupts.register_interrupt_handler
            --       (prot_procP'address);

            if not Restricted_Profile
              and then Is_Interrupt_Handler
                         (Defining_Unit_Name (Specification (Comp)))
            then
               Register_Handler;
            end if;

         elsif Nkind (Comp) = N_Entry_Declaration then
            E_Count := E_Count + 1;
            Comp_Id := Defining_Identifier (Comp);
            Set_Privals_Chain (Comp_Id, New_Elmt_List);
            Edef :=
              Make_Defining_Identifier (Loc,
                Build_Selected_Name (Prottyp, Comp_Id, 'E'));
            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Entry_Specification (Edef, Comp_Id, Loc));

            Insert_After (Current_Node, Sub);
            Analyze (Sub);

            Set_Protected_Body_Subprogram (
              Defining_Identifier (Comp),
              Defining_Unit_Name (Specification (Sub)));

            Current_Node := Sub;

            Bdef :=
              Make_Defining_Identifier (Loc,
                Build_Selected_Name (Prottyp, Comp_Id, 'B'));
            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Barrier_Function_Specification (Bdef, Loc));

            Insert_After (Current_Node, Sub);
            Analyze (Sub);
            Set_Protected_Body_Subprogram (Bdef, Bdef);
            Set_Barrier_Function (Comp_Id, Bdef);
            Set_Scope (Bdef, Scope (Comp_Id));
            Current_Node := Sub;

            --  Collect pointers to the protected subprogram and the barrier
            --  of the current entry, for insertion into Entry_Bodies_Array.

            Append (
              Make_Aggregate (Loc,
                Expressions => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Reference_To (Bdef, Loc),
                    Attribute_Name => Name_Unrestricted_Access),
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Reference_To (Edef, Loc),
                    Attribute_Name => Name_Unrestricted_Access))),
              Expressions (Entries_Aggr));

         end if;

         Next (Comp);
      end loop;

      --  If there are some private entry declarations, expand it as if they
      --  were visible entries.

      if Present (Private_Declarations (Pdef)) then
         Comp := First (Private_Declarations (Pdef));
         while Present (Comp) loop
            if Nkind (Comp) = N_Entry_Declaration then
               E_Count := E_Count + 1;
               Comp_Id := Defining_Identifier (Comp);
               Set_Privals_Chain (Comp_Id, New_Elmt_List);
               Edef :=
                 Make_Defining_Identifier (Loc,
                  Build_Selected_Name (Prottyp, Comp_Id, 'E'));

               Sub :=
                 Make_Subprogram_Declaration (Loc,
                   Specification =>
                     Build_Protected_Entry_Specification (Edef, Comp_Id, Loc));

               Insert_After (Current_Node, Sub);
               Analyze (Sub);

               Set_Protected_Body_Subprogram (
                 Defining_Identifier (Comp),
                 Defining_Unit_Name (Specification (Sub)));

               Current_Node := Sub;

               Bdef :=
                 Make_Defining_Identifier (Loc,
                    Build_Selected_Name (Prottyp, Comp_Id, 'E'));

               Sub :=
                 Make_Subprogram_Declaration (Loc,
                   Specification =>
                     Build_Barrier_Function_Specification (Bdef, Loc));

               Insert_After (Current_Node, Sub);
               Analyze (Sub);
               Set_Protected_Body_Subprogram (Bdef, Bdef);
               Set_Barrier_Function (Comp_Id, Bdef);
               Set_Scope (Bdef, Scope (Comp_Id));
               Current_Node := Sub;

               --  Collect pointers to the protected subprogram and the barrier
               --  of the current entry, for insertion into Entry_Bodies_Array.

               Append (
                 Make_Aggregate (Loc,
                   Expressions => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Reference_To (Bdef, Loc),
                       Attribute_Name => Name_Unrestricted_Access),
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Reference_To (Edef, Loc),
                       Attribute_Name => Name_Unrestricted_Access))),
                 Expressions (Entries_Aggr));
            end if;

            Next (Comp);
         end loop;
      end if;

      --  Emit declaration for Entry_Bodies_Array, now that the addresses of
      --  all protected subprograms have been collected.

      if Has_Entries (Prottyp) then
         Body_Id := Make_Defining_Identifier (Sloc (Prottyp),
           New_External_Name (Chars (Prottyp), 'A'));

         if Abort_Allowed
           or else Restriction_Active (No_Entry_Queue) = False
           or else E_Count > 1
         then
            Body_Arr := Make_Object_Declaration (Loc,
              Defining_Identifier => Body_Id,
              Aliased_Present => True,
              Object_Definition =>
                Make_Subtype_Indication (Loc,
                  Subtype_Mark => New_Reference_To (
                    RTE (RE_Protected_Entry_Body_Array), Loc),
                  Constraint =>
                    Make_Index_Or_Discriminant_Constraint (Loc,
                      Constraints => New_List (
                         Make_Range (Loc,
                           Make_Integer_Literal (Loc, 1),
                           Make_Integer_Literal (Loc, E_Count))))),
              Expression => Entries_Aggr);

         else
            Body_Arr := Make_Object_Declaration (Loc,
              Defining_Identifier => Body_Id,
              Aliased_Present => True,
              Object_Definition => New_Reference_To (RTE (RE_Entry_Body), Loc),
              Expression =>
                Make_Aggregate (Loc,
                  Expressions => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Bdef, Loc),
                      Attribute_Name => Name_Unrestricted_Access),
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Edef, Loc),
                      Attribute_Name => Name_Unrestricted_Access))));
         end if;

         --  A pointer to this array will be placed in the corresponding record
         --  by its initialization procedure so this needs to be analyzed here.

         Insert_After (Current_Node, Body_Arr);
         Current_Node := Body_Arr;
         Analyze (Body_Arr);

         Set_Entry_Bodies_Array (Prottyp, Body_Id);

         --  Finally, build the function that maps an entry index into the
         --  corresponding body. A pointer to this function is placed in each
         --  object of the type. Except for a ravenscar-like profile (no abort,
         --  no entry queue, 1 entry)

         if Abort_Allowed
           or else Restriction_Active (No_Entry_Queue) = False
           or else E_Count > 1
         then
            Sub :=
              Make_Subprogram_Declaration (Loc,
                Specification => Build_Find_Body_Index_Spec (Prottyp));
            Insert_After (Current_Node, Sub);
            Analyze (Sub);
         end if;
      end if;
   end Expand_N_Protected_Type_Declaration;

   --------------------------------
   -- Expand_N_Requeue_Statement --
   --------------------------------

   --  A requeue statement is expanded into one of four GNARLI operations,
   --  depending on the source and destination (task or protected object). In
   --  addition, code must be generated to jump around the remainder of
   --  processing for the original entry and, if the destination is (different)
   --  protected object, to attempt to service it. The following illustrates
   --  the various cases:

   --  procedure entE
   --    (O : System.Address;
   --     P : System.Address;
   --     E : Protected_Entry_Index)
   --  is
   --     <discriminant renamings>
   --     <private object renamings>
   --     type poVP is access poV;
   --     _Object : ptVP := ptVP!(O);

   --  begin
   --     begin
   --        <start of statement sequence for entry>

   --        -- Requeue from one protected entry body to another protected
   --        -- entry.

   --        Requeue_Protected_Entry (
   --          _object._object'Access,
   --          new._object'Access,
   --          E,
   --          Abort_Present);
   --        return;

   --        <some more of the statement sequence for entry>

   --        --  Requeue from an entry body to a task entry

   --        Requeue_Protected_To_Task_Entry (
   --          New._task_id,
   --          E,
   --          Abort_Present);
   --        return;

   --        <rest of statement sequence for entry>
   --        Complete_Entry_Body (_Object._Object);

   --     exception
   --        when all others =>
   --           Exceptional_Complete_Entry_Body (
   --             _Object._Object, Get_GNAT_Exception);
   --     end;
   --  end entE;

   --  Requeue of a task entry call to a task entry

   --  Accept_Call (E, Ann);
   --     <start of statement sequence for accept statement>
   --     Requeue_Task_Entry (New._task_id, E, Abort_Present);
   --     goto Lnn;
   --     <rest of statement sequence for accept statement>
   --     <<Lnn>>
   --     Complete_Rendezvous;

   --  exception
   --     when all others =>
   --        Exceptional_Complete_Rendezvous (Get_GNAT_Exception);

   --  Requeue of a task entry call to a protected entry

   --  Accept_Call (E, Ann);
   --     <start of statement sequence for accept statement>
   --     Requeue_Task_To_Protected_Entry (
   --       new._object'Access,
   --       E,
   --       Abort_Present);
   --     newS (new, Pnn);
   --     goto Lnn;
   --     <rest of statement sequence for accept statement>
   --     <<Lnn>>
   --     Complete_Rendezvous;

   --  exception
   --     when all others =>
   --        Exceptional_Complete_Rendezvous (Get_GNAT_Exception);

   --  Further details on these expansions can be found in
   --  Expand_N_Protected_Body and Expand_N_Accept_Statement.

   procedure Expand_N_Requeue_Statement (N : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Acc_Stat   : Node_Id;
      Concval    : Node_Id;
      Ename      : Node_Id;
      Index      : Node_Id;
      Conctyp    : Entity_Id;
      Oldtyp     : Entity_Id;
      Lab_Node   : Node_Id;
      Rcall      : Node_Id;
      Abortable  : Node_Id;
      Skip_Stat  : Node_Id;
      Self_Param : Node_Id;
      New_Param  : Node_Id;
      Params     : List_Id;
      RTS_Call   : Entity_Id;

   begin
      Abortable :=
        New_Occurrence_Of (Boolean_Literals (Abort_Present (N)), Loc);

      --  Set up the target object

      Extract_Entry (N, Concval, Ename, Index);
      Conctyp := Etype (Concval);
      New_Param := Concurrent_Ref (Concval);

      --  The target entry index and abortable flag are the same for all cases

      Params := New_List (
        Entry_Index_Expression (Loc, Entity (Ename), Index, Conctyp),
        Abortable);

      --  Determine proper GNARLI call and required additional parameters
      --  Loop to find nearest enclosing task type or protected type

      Oldtyp := Current_Scope;
      loop
         if Is_Task_Type (Oldtyp) then
            if Is_Task_Type (Conctyp) then
               RTS_Call := RTE (RE_Requeue_Task_Entry);

            else
               pragma Assert (Is_Protected_Type (Conctyp));
               RTS_Call := RTE (RE_Requeue_Task_To_Protected_Entry);
               New_Param :=
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Param,
                   Attribute_Name => Name_Unchecked_Access);
            end if;

            Prepend (New_Param, Params);
            exit;

         elsif Is_Protected_Type (Oldtyp) then
            Self_Param :=
              Make_Attribute_Reference (Loc,
                Prefix => Concurrent_Ref (New_Occurrence_Of (Oldtyp, Loc)),
                Attribute_Name => Name_Unchecked_Access);

            if Is_Task_Type (Conctyp) then
               RTS_Call := RTE (RE_Requeue_Protected_To_Task_Entry);

            else
               pragma Assert (Is_Protected_Type (Conctyp));
               RTS_Call := RTE (RE_Requeue_Protected_Entry);
               New_Param :=
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Param,
                   Attribute_Name => Name_Unchecked_Access);
            end if;

            Prepend (New_Param, Params);
            Prepend (Self_Param, Params);
            exit;

         --  If neither task type or protected type, must be in some inner
         --  enclosing block, so move on out

         else
            Oldtyp := Scope (Oldtyp);
         end if;
      end loop;

      --  Create the GNARLI call

      Rcall := Make_Procedure_Call_Statement (Loc,
        Name =>
          New_Occurrence_Of (RTS_Call, Loc),
        Parameter_Associations => Params);

      Rewrite (N, Rcall);
      Analyze (N);

      if Is_Protected_Type (Oldtyp) then

         --  Build the return statement to skip the rest of the entry body

         Skip_Stat := Make_Return_Statement (Loc);

      else
         --  If the requeue is within a task, find the end label of the
         --  enclosing accept statement.

         Acc_Stat := Parent (N);
         while Nkind (Acc_Stat) /= N_Accept_Statement loop
            Acc_Stat := Parent (Acc_Stat);
         end loop;

         --  The last statement is the second label, used for completing the
         --  rendezvous the usual way. The label we are looking for is right
         --  before it.

         Lab_Node :=
           Prev (Last (Statements (Handled_Statement_Sequence (Acc_Stat))));

         pragma Assert (Nkind (Lab_Node) = N_Label);

         --  Build the goto statement to skip the rest of the accept
         --  statement.

         Skip_Stat :=
           Make_Goto_Statement (Loc,
             Name => New_Occurrence_Of (Entity (Identifier (Lab_Node)), Loc));
      end if;

      Set_Analyzed (Skip_Stat);

      Insert_After (N, Skip_Stat);
   end Expand_N_Requeue_Statement;

   -------------------------------
   -- Expand_N_Selective_Accept --
   -------------------------------

   procedure Expand_N_Selective_Accept (N : Node_Id) is
      Loc            : constant Source_Ptr := Sloc (N);
      Alts           : constant List_Id    := Select_Alternatives (N);

      --  Note: in the below declarations a lot of new lists are allocated
      --  unconditionally which may well not end up being used. That's
      --  not a good idea since it wastes space gratuitously ???

      Accept_Case    : List_Id;
      Accept_List    : constant List_Id := New_List;

      Alt            : Node_Id;
      Alt_List       : constant List_Id := New_List;
      Alt_Stats      : List_Id;
      Ann            : Entity_Id := Empty;

      Block          : Node_Id;
      Check_Guard    : Boolean := True;

      Decls          : constant List_Id := New_List;
      Stats          : constant List_Id := New_List;
      Body_List      : constant List_Id := New_List;
      Trailing_List  : constant List_Id := New_List;

      Choices        : List_Id;
      Else_Present   : Boolean := False;
      Terminate_Alt  : Node_Id := Empty;
      Select_Mode    : Node_Id;

      Delay_Case     : List_Id;
      Delay_Count    : Integer := 0;
      Delay_Val      : Entity_Id;
      Delay_Index    : Entity_Id;
      Delay_Min      : Entity_Id;
      Delay_Num      : Int := 1;
      Delay_Alt_List : List_Id := New_List;
      Delay_List     : constant List_Id := New_List;
      D              : Entity_Id;
      M              : Entity_Id;

      First_Delay    : Boolean := True;
      Guard_Open     : Entity_Id;

      End_Lab        : Node_Id;
      Index          : Int := 1;
      Lab            : Node_Id;
      Num_Alts       : Int;
      Num_Accept     : Nat := 0;
      Proc           : Node_Id;
      Q              : Node_Id;
      Time_Type      : Entity_Id;
      X              : Node_Id;
      Select_Call    : Node_Id;

      Qnam : constant Entity_Id :=
               Make_Defining_Identifier (Loc, New_External_Name ('S', 0));

      Xnam : constant Entity_Id :=
               Make_Defining_Identifier (Loc, New_External_Name ('J', 1));

      -----------------------
      -- Local subprograms --
      -----------------------

      function Accept_Or_Raise return List_Id;
      --  For the rare case where delay alternatives all have guards, and
      --  all of them are closed, it is still possible that there were open
      --  accept alternatives with no callers. We must reexamine the
      --  Accept_List, and execute a selective wait with no else if some
      --  accept is open. If none, we raise program_error.

      procedure Add_Accept (Alt : Node_Id);
      --  Process a single accept statement in a select alternative. Build
      --  procedure for body of accept, and add entry to dispatch table with
      --  expression for guard, in preparation for call to run time select.

      function Make_And_Declare_Label (Num : Int) return Node_Id;
      --  Manufacture a label using Num as a serial number and declare it.
      --  The declaration is appended to Decls. The label marks the trailing
      --  statements of an accept or delay alternative.

      function Make_Select_Call (Select_Mode : Entity_Id) return Node_Id;
      --  Build call to Selective_Wait runtime routine

      procedure Process_Delay_Alternative (Alt : Node_Id; Index : Int);
      --  Add code to compare value of delay with previous values, and
      --  generate case entry for trailing statements.

      procedure Process_Accept_Alternative
        (Alt   : Node_Id;
         Index : Int;
         Proc  : Node_Id);
      --  Add code to call corresponding procedure, and branch to
      --  trailing statements, if any.

      ---------------------
      -- Accept_Or_Raise --
      ---------------------

      function Accept_Or_Raise return List_Id is
         Cond  : Node_Id;
         Stats : List_Id;
         J     : constant Entity_Id := Make_Defining_Identifier (Loc,
                                                  New_Internal_Name ('J'));

      begin
         --  We generate the following:

         --    for J in q'range loop
         --       if q(J).S /=null_task_entry then
         --          selective_wait (simple_mode,...);
         --          done := True;
         --          exit;
         --       end if;
         --    end loop;
         --
         --    if no rendez_vous then
         --       raise program_error;
         --    end if;

         --    Note that the code needs to know that the selector name
         --    in an Accept_Alternative is named S.

         Cond := Make_Op_Ne (Loc,
           Left_Opnd =>
             Make_Selected_Component (Loc,
               Prefix => Make_Indexed_Component (Loc,
                 Prefix => New_Reference_To (Qnam, Loc),
                   Expressions => New_List (New_Reference_To (J, Loc))),
             Selector_Name => Make_Identifier (Loc, Name_S)),
           Right_Opnd =>
             New_Reference_To (RTE (RE_Null_Task_Entry), Loc));

         Stats := New_List (
           Make_Implicit_Loop_Statement (N,
             Identifier => Empty,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Loc,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Loc,
                     Defining_Identifier => J,
                     Discrete_Subtype_Definition =>
                       Make_Attribute_Reference (Loc,
                         Prefix => New_Reference_To (Qnam, Loc),
                         Attribute_Name => Name_Range,
                         Expressions => New_List (
                           Make_Integer_Literal (Loc, 1))))),

             Statements => New_List (
               Make_Implicit_If_Statement (N,
                 Condition =>  Cond,
                 Then_Statements => New_List (
                   Make_Select_Call (
                    New_Reference_To (RTE (RE_Simple_Mode), Loc)),
                   Make_Exit_Statement (Loc))))));

         Append_To (Stats,
           Make_Raise_Program_Error (Loc,
             Condition => Make_Op_Eq (Loc,
               Left_Opnd  => New_Reference_To (Xnam, Loc),
               Right_Opnd =>
                 New_Reference_To (RTE (RE_No_Rendezvous), Loc)),
             Reason => PE_All_Guards_Closed));

         return Stats;
      end Accept_Or_Raise;

      ----------------
      -- Add_Accept --
      ----------------

      procedure Add_Accept (Alt : Node_Id) is
         Acc_Stm   : constant Node_Id    := Accept_Statement (Alt);
         Ename     : constant Node_Id    := Entry_Direct_Name (Acc_Stm);
         Eent      : constant Entity_Id  := Entity (Ename);
         Index     : constant Node_Id    := Entry_Index (Acc_Stm);
         Null_Body : Node_Id;
         Proc_Body : Node_Id;
         PB_Ent    : Entity_Id;
         Expr      : Node_Id;
         Call      : Node_Id;

      begin
         if No (Ann) then
            Ann := Node (Last_Elmt (Accept_Address (Eent)));
         end if;

         if Present (Condition (Alt)) then
            Expr :=
              Make_Conditional_Expression (Loc, New_List (
                Condition (Alt),
                Entry_Index_Expression (Loc, Eent, Index, Scope (Eent)),
                New_Reference_To (RTE (RE_Null_Task_Entry), Loc)));
         else
            Expr :=
              Entry_Index_Expression
                (Loc, Eent, Index, Scope (Eent));
         end if;

         if Present (Handled_Statement_Sequence (Accept_Statement (Alt))) then
            Null_Body := New_Reference_To (Standard_False, Loc);

            if Abort_Allowed then
               Call := Make_Procedure_Call_Statement (Loc,
                 Name => New_Reference_To (RTE (RE_Abort_Undefer), Loc));
               Insert_Before (First (Statements (Handled_Statement_Sequence (
                 Accept_Statement (Alt)))), Call);
               Analyze (Call);
            end if;

            PB_Ent :=
              Make_Defining_Identifier (Sloc (Ename),
                New_External_Name (Chars (Ename), 'A', Num_Accept));

            Set_Needs_Debug_Info (PB_Ent, Comes_From_Source (Alt));

            Proc_Body :=
              Make_Subprogram_Body (Loc,
                Specification =>
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name => PB_Ent),
               Declarations => Declarations (Acc_Stm),
               Handled_Statement_Sequence =>
                 Build_Accept_Body (Accept_Statement (Alt)));

            --  During the analysis of the body of the accept statement, any
            --  zero cost exception handler records were collected in the
            --  Accept_Handler_Records field of the N_Accept_Alternative node.
            --  This is where we move them to where they belong, namely the
            --  newly created procedure.

            Set_Handler_Records (PB_Ent, Accept_Handler_Records (Alt));
            Append (Proc_Body, Body_List);

         else
            Null_Body := New_Reference_To (Standard_True,  Loc);

            --  if accept statement has declarations, insert above, given that
            --  we are not creating a body for the accept.

            if Present (Declarations (Acc_Stm)) then
               Insert_Actions (N, Declarations (Acc_Stm));
            end if;
         end if;

         Append_To (Accept_List,
           Make_Aggregate (Loc, Expressions => New_List (Null_Body, Expr)));

         Num_Accept := Num_Accept + 1;
      end Add_Accept;

      ----------------------------
      -- Make_And_Declare_Label --
      ----------------------------

      function Make_And_Declare_Label (Num : Int) return Node_Id is
         Lab_Id : Node_Id;

      begin
         Lab_Id := Make_Identifier (Loc, New_External_Name ('L', Num));
         Lab :=
           Make_Label (Loc, Lab_Id);

         Append_To (Decls,
           Make_Implicit_Label_Declaration (Loc,
             Defining_Identifier  =>
               Make_Defining_Identifier (Loc, Chars (Lab_Id)),
             Label_Construct => Lab));

         return Lab;
      end Make_And_Declare_Label;

      ----------------------
      -- Make_Select_Call --
      ----------------------

      function Make_Select_Call (Select_Mode : Entity_Id) return Node_Id is
         Params : constant List_Id := New_List;

      begin
         Append (
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Qnam, Loc),
             Attribute_Name => Name_Unchecked_Access),
           Params);
         Append (Select_Mode, Params);
         Append (New_Reference_To (Ann, Loc), Params);
         Append (New_Reference_To (Xnam, Loc), Params);

         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Selective_Wait), Loc),
             Parameter_Associations => Params);
      end Make_Select_Call;

      --------------------------------
      -- Process_Accept_Alternative --
      --------------------------------

      procedure Process_Accept_Alternative
        (Alt   : Node_Id;
         Index : Int;
         Proc  : Node_Id)
      is
         Choices   : List_Id := No_List;
         Alt_Stats : List_Id;

      begin
         Adjust_Condition (Condition (Alt));
         Alt_Stats := No_List;

         if Present (Handled_Statement_Sequence (Accept_Statement (Alt))) then
            Choices := New_List (
              Make_Integer_Literal (Loc, Index));

            Alt_Stats := New_List (
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  Defining_Unit_Name (Specification (Proc)), Loc)));
         end if;

         if Statements (Alt) /= Empty_List then

            if No (Alt_Stats) then

               --  Accept with no body, followed by trailing statements

               Choices := New_List (
                 Make_Integer_Literal (Loc, Index));

               Alt_Stats := New_List;
            end if;

            --  After the call, if any, branch to to trailing statements. We
            --  create a label for each, as well as the corresponding label
            --  declaration.

            Lab := Make_And_Declare_Label (Index);
            Append_To (Alt_Stats,
              Make_Goto_Statement (Loc,
                Name => New_Copy (Identifier (Lab))));

            Append (Lab, Trailing_List);
            Append_List (Statements (Alt), Trailing_List);
            Append_To (Trailing_List,
              Make_Goto_Statement (Loc,
                Name => New_Copy (Identifier (End_Lab))));
         end if;

         if Present (Alt_Stats) then

            --  Procedure call. and/or trailing statements

            Append_To (Alt_List,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => Choices,
                Statements => Alt_Stats));
         end if;
      end Process_Accept_Alternative;

      -------------------------------
      -- Process_Delay_Alternative --
      -------------------------------

      procedure Process_Delay_Alternative (Alt : Node_Id; Index : Int) is
         Choices   : List_Id;
         Cond      : Node_Id;
         Delay_Alt : List_Id;

      begin
         --  Deal with C/Fortran boolean as delay condition

         Adjust_Condition (Condition (Alt));

         --  Determine the smallest specified delay

         --  for each delay alternative generate:

         --    if guard-expression then
         --       Delay_Val  := delay-expression;
         --       Guard_Open := True;
         --       if Delay_Val < Delay_Min then
         --          Delay_Min   := Delay_Val;
         --          Delay_Index := Index;
         --       end if;
         --    end if;

         --  The enclosing if-statement is omitted if there is no guard

         if Delay_Count = 1
           or else First_Delay
         then
            First_Delay := False;

            Delay_Alt := New_List (
              Make_Assignment_Statement (Loc,
                Name => New_Reference_To (Delay_Min, Loc),
                Expression => Expression (Delay_Statement (Alt))));

            if Delay_Count > 1 then
               Append_To (Delay_Alt,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Reference_To (Delay_Index, Loc),
                   Expression => Make_Integer_Literal (Loc, Index)));
            end if;

         else
            Delay_Alt := New_List (
              Make_Assignment_Statement (Loc,
                Name => New_Reference_To (Delay_Val, Loc),
                Expression => Expression (Delay_Statement (Alt))));

            if Time_Type = Standard_Duration then
               Cond :=
                  Make_Op_Lt (Loc,
                    Left_Opnd  => New_Reference_To (Delay_Val, Loc),
                    Right_Opnd => New_Reference_To (Delay_Min, Loc));

            else
               --  The scope of the time type must define a comparison
               --  operator. The scope itself may not be visible, so we
               --  construct a node with entity information to insure that
               --  semantic analysis can find the proper operator.

               Cond :=
                 Make_Function_Call (Loc,
                   Name => Make_Selected_Component (Loc,
                     Prefix => New_Reference_To (Scope (Time_Type), Loc),
                     Selector_Name =>
                       Make_Operator_Symbol (Loc,
                         Chars => Name_Op_Lt,
                         Strval => No_String)),
                    Parameter_Associations =>
                      New_List (
                        New_Reference_To (Delay_Val, Loc),
                        New_Reference_To (Delay_Min, Loc)));

               Set_Entity (Prefix (Name (Cond)), Scope (Time_Type));
            end if;

            Append_To (Delay_Alt,
              Make_Implicit_If_Statement (N,
                Condition => Cond,
                Then_Statements => New_List (
                  Make_Assignment_Statement (Loc,
                    Name       => New_Reference_To (Delay_Min, Loc),
                    Expression => New_Reference_To (Delay_Val, Loc)),

                  Make_Assignment_Statement (Loc,
                    Name       => New_Reference_To (Delay_Index, Loc),
                    Expression => Make_Integer_Literal (Loc, Index)))));
         end if;

         if Check_Guard then
            Append_To (Delay_Alt,
              Make_Assignment_Statement (Loc,
                Name => New_Reference_To (Guard_Open, Loc),
                Expression => New_Reference_To (Standard_True, Loc)));
         end if;

         if Present (Condition (Alt)) then
            Delay_Alt := New_List (
              Make_Implicit_If_Statement (N,
                Condition => Condition (Alt),
                Then_Statements => Delay_Alt));
         end if;

         Append_List (Delay_Alt, Delay_List);

         --  If the delay alternative has a statement part, add choice to the
         --  case statements for delays.

         if Present (Statements (Alt)) then

            if Delay_Count = 1 then
               Append_List (Statements (Alt), Delay_Alt_List);

            else
               Choices := New_List (
                 Make_Integer_Literal (Loc, Index));

               Append_To (Delay_Alt_List,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => Choices,
                   Statements => Statements (Alt)));
            end if;

         elsif Delay_Count = 1 then

            --  If the single delay has no trailing statements, add a branch
            --  to the exit label to the selective wait.

            Delay_Alt_List := New_List (
              Make_Goto_Statement (Loc,
                Name => New_Copy (Identifier (End_Lab))));

         end if;
      end Process_Delay_Alternative;

   --  Start of processing for Expand_N_Selective_Accept

   begin
      --  First insert some declarations before the select. The first is:

      --    Ann : Address

      --  This variable holds the parameters passed to the accept body. This
      --  declaration has already been inserted by the time we get here by
      --  a call to Expand_Accept_Declarations made from the semantics when
      --  processing the first accept statement contained in the select. We
      --  can find this entity as Accept_Address (E), where E is any of the
      --  entries references by contained accept statements.

      --  The first step is to scan the list of Selective_Accept_Statements
      --  to find this entity, and also count the number of accepts, and
      --  determine if terminated, delay or else is present:

      Num_Alts := 0;

      Alt := First (Alts);
      while Present (Alt) loop

         if Nkind (Alt) = N_Accept_Alternative then
            Add_Accept (Alt);

         elsif Nkind (Alt) = N_Delay_Alternative then
            Delay_Count   := Delay_Count + 1;

            --  If the delays are relative delays, the delay expressions have
            --  type Standard_Duration. Otherwise they must have some time type
            --  recognized by GNAT.

            if Nkind (Delay_Statement (Alt)) = N_Delay_Relative_Statement then
               Time_Type := Standard_Duration;
            else
               Time_Type := Etype (Expression (Delay_Statement (Alt)));

               if Is_RTE (Base_Type (Etype (Time_Type)), RO_CA_Time)
                 or else Is_RTE (Base_Type (Etype (Time_Type)), RO_RT_Time)
               then
                  null;
               else
                  Error_Msg_NE (
                    "& is not a time type ('R'M 9.6(6))",
                       Expression (Delay_Statement (Alt)), Time_Type);
                  Time_Type := Standard_Duration;
                  Set_Etype (Expression (Delay_Statement (Alt)), Any_Type);
               end if;
            end if;

            if No (Condition (Alt)) then

               --  This guard will always be open

               Check_Guard := False;
            end if;

         elsif Nkind (Alt) = N_Terminate_Alternative then
            Adjust_Condition (Condition (Alt));
            Terminate_Alt := Alt;
         end if;

         Num_Alts := Num_Alts + 1;
         Next (Alt);
      end loop;

      Else_Present := Present (Else_Statements (N));

      --  At the same time (see procedure Add_Accept) we build the accept list:

      --    Qnn : Accept_List (1 .. num-select) := (
      --          (null-body, entry-index),
      --          (null-body, entry-index),
      --          ..
      --          (null_body, entry-index));

      --  In the above declaration, null-body is True if the corresponding
      --  accept has no body, and false otherwise. The entry is either the
      --  entry index expression if there is no guard, or if a guard is
      --  present, then a conditional expression of the form:

      --    (if guard then entry-index else Null_Task_Entry)

      --  If a guard is statically known to be false, the entry can simply
      --  be omitted from the accept list.

      Q :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Qnam,
          Object_Definition =>
            New_Reference_To (RTE (RE_Accept_List), Loc),
          Aliased_Present => True,

          Expression =>
             Make_Qualified_Expression (Loc,
               Subtype_Mark =>
                 New_Reference_To (RTE (RE_Accept_List), Loc),
               Expression =>
                 Make_Aggregate (Loc, Expressions => Accept_List)));

      Append (Q, Decls);

      --  Then we declare the variable that holds the index for the accept
      --  that will be selected for service:

      --    Xnn : Select_Index;

      X :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Xnam,
          Object_Definition =>
            New_Reference_To (RTE (RE_Select_Index), Loc),
          Expression =>
            New_Reference_To (RTE (RE_No_Rendezvous), Loc));

      Append (X, Decls);

      --  After this follow procedure declarations for each accept body

      --    procedure Pnn is
      --    begin
      --       ...
      --    end;

      --  where the ... are statements from the corresponding procedure body.
      --  No parameters are involved, since the parameters are passed via Ann
      --  and the parameter references have already been expanded to be direct
      --  references to Ann (see Exp_Ch2.Expand_Entry_Parameter). Furthermore,
      --  any embedded tasking statements (which would normally be illegal in
      --  procedures, have been converted to calls to the tasking runtime so
      --  there is no problem in putting them into procedures.

      --  The original accept statement has been expanded into a block in
      --  the same fashion as for simple accepts (see Build_Accept_Body).

      --  Note: we don't really need to build these procedures for the case
      --  where no delay statement is present, but it is just as easy to
      --  build them unconditionally, and not significantly inefficient,
      --  since if they are short they will be inlined anyway.

      --  The procedure declarations have been assembled in Body_List

      --  If delays are present, we must compute the required delay.
      --  We first generate the declarations:

      --    Delay_Index : Boolean := 0;
      --    Delay_Min   : Some_Time_Type.Time;
      --    Delay_Val   : Some_Time_Type.Time;

      --  Delay_Index will be set to the index of the minimum delay, i.e. the
      --  active delay that is actually chosen as the basis for the possible
      --  delay if an immediate rendez-vous is not possible.

      --  In the most common case there is a single delay statement, and this
      --  is handled specially.

      if Delay_Count > 0 then

         --  Generate the required declarations

         Delay_Val :=
           Make_Defining_Identifier (Loc, New_External_Name ('D', 1));
         Delay_Index :=
           Make_Defining_Identifier (Loc, New_External_Name ('D', 2));
         Delay_Min :=
           Make_Defining_Identifier (Loc, New_External_Name ('D', 3));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Delay_Val,
             Object_Definition   => New_Reference_To (Time_Type, Loc)));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Delay_Index,
             Object_Definition   => New_Reference_To (Standard_Integer, Loc),
             Expression          => Make_Integer_Literal (Loc, 0)));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Delay_Min,
             Object_Definition   => New_Reference_To (Time_Type, Loc),
             Expression          =>
               Unchecked_Convert_To (Time_Type,
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Occurrence_Of (Underlying_Type (Time_Type), Loc),
                   Attribute_Name => Name_Last))));

         --  Create Duration and Delay_Mode objects used for passing a delay
         --  value to RTS

         D := Make_Defining_Identifier (Loc, New_Internal_Name ('D'));
         M := Make_Defining_Identifier (Loc, New_Internal_Name ('M'));

         declare
            Discr : Entity_Id;

         begin
            --  Note that these values are defined in s-osprim.ads and must
            --  be kept in sync:
            --
            --     Relative          : constant := 0;
            --     Absolute_Calendar : constant := 1;
            --     Absolute_RT       : constant := 2;

            if Time_Type = Standard_Duration then
               Discr := Make_Integer_Literal (Loc, 0);

            elsif Is_RTE (Base_Type (Etype (Time_Type)), RO_CA_Time) then
               Discr := Make_Integer_Literal (Loc, 1);

            else
               pragma Assert
                 (Is_RTE (Base_Type (Etype (Time_Type)), RO_RT_Time));
               Discr := Make_Integer_Literal (Loc, 2);
            end if;

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => D,
                Object_Definition =>
                  New_Reference_To (Standard_Duration, Loc)));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => M,
                Object_Definition   =>
                  New_Reference_To (Standard_Integer, Loc),
                Expression          => Discr));
         end;

         if Check_Guard then
            Guard_Open :=
              Make_Defining_Identifier (Loc, New_External_Name ('G', 1));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                 Defining_Identifier => Guard_Open,
                 Object_Definition => New_Reference_To (Standard_Boolean, Loc),
                 Expression        => New_Reference_To (Standard_False, Loc)));
         end if;

      --  Delay_Count is zero, don't need M and D set (suppress warning)

      else
         M := Empty;
         D := Empty;
      end if;

      if Present (Terminate_Alt) then

         --  If the terminate alternative guard is False, use
         --  Simple_Mode; otherwise use Terminate_Mode.

         if Present (Condition (Terminate_Alt)) then
            Select_Mode := Make_Conditional_Expression (Loc,
              New_List (Condition (Terminate_Alt),
                        New_Reference_To (RTE (RE_Terminate_Mode), Loc),
                        New_Reference_To (RTE (RE_Simple_Mode), Loc)));
         else
            Select_Mode := New_Reference_To (RTE (RE_Terminate_Mode), Loc);
         end if;

      elsif Else_Present or Delay_Count > 0 then
         Select_Mode := New_Reference_To (RTE (RE_Else_Mode), Loc);

      else
         Select_Mode := New_Reference_To (RTE (RE_Simple_Mode), Loc);
      end if;

      Select_Call := Make_Select_Call (Select_Mode);
      Append (Select_Call, Stats);

      --  Now generate code to act on the result. There is an entry
      --  in this case for each accept statement with a non-null body,
      --  followed by a branch to the statements that follow the Accept.
      --  In the absence of delay alternatives, we generate:

      --    case X is
      --      when No_Rendezvous =>  --  omitted if simple mode
      --         goto Lab0;

      --      when 1 =>
      --         P1n;
      --         goto Lab1;

      --      when 2 =>
      --         P2n;
      --         goto Lab2;

      --      when others =>
      --         goto Exit;
      --    end case;
      --
      --    Lab0: Else_Statements;
      --    goto exit;

      --    Lab1:  Trailing_Statements1;
      --    goto Exit;
      --
      --    Lab2:  Trailing_Statements2;
      --    goto Exit;
      --    ...
      --    Exit:

      --  Generate label for common exit

      End_Lab := Make_And_Declare_Label (Num_Alts + 1);

      --  First entry is the default case, when no rendezvous is possible

      Choices := New_List (New_Reference_To (RTE (RE_No_Rendezvous), Loc));

      if Else_Present then

         --  If no rendezvous is possible, the else part is executed

         Lab := Make_And_Declare_Label (0);
         Alt_Stats := New_List (
           Make_Goto_Statement (Loc,
             Name => New_Copy (Identifier (Lab))));

         Append (Lab, Trailing_List);
         Append_List (Else_Statements (N), Trailing_List);
         Append_To (Trailing_List,
           Make_Goto_Statement (Loc,
             Name => New_Copy (Identifier (End_Lab))));
      else
         Alt_Stats := New_List (
           Make_Goto_Statement (Loc,
             Name => New_Copy (Identifier (End_Lab))));
      end if;

      Append_To (Alt_List,
        Make_Case_Statement_Alternative (Loc,
          Discrete_Choices => Choices,
          Statements => Alt_Stats));

      --  We make use of the fact that Accept_Index is an integer type, and
      --  generate successive literals for entries for each accept. Only those
      --  for which there is a body or trailing statements get a case entry.

      Alt := First (Select_Alternatives (N));
      Proc := First (Body_List);
      while Present (Alt) loop

         if Nkind (Alt) = N_Accept_Alternative then
            Process_Accept_Alternative (Alt, Index, Proc);
            Index := Index + 1;

            if Present
              (Handled_Statement_Sequence (Accept_Statement (Alt)))
            then
               Next (Proc);
            end if;

         elsif Nkind (Alt) = N_Delay_Alternative then
            Process_Delay_Alternative (Alt, Delay_Num);
            Delay_Num := Delay_Num + 1;
         end if;

         Next (Alt);
      end loop;

      --  An others choice is always added to the main case, as well
      --  as the delay case (to satisfy the compiler).

      Append_To (Alt_List,
        Make_Case_Statement_Alternative (Loc,
          Discrete_Choices =>
            New_List (Make_Others_Choice (Loc)),
          Statements       =>
            New_List (Make_Goto_Statement (Loc,
              Name => New_Copy (Identifier (End_Lab))))));

      Accept_Case := New_List (
        Make_Case_Statement (Loc,
          Expression   => New_Reference_To (Xnam, Loc),
          Alternatives => Alt_List));

      Append_List (Trailing_List, Accept_Case);
      Append (End_Lab, Accept_Case);
      Append_List (Body_List, Decls);

      --  Construct case statement for trailing statements of delay
      --  alternatives, if there are several of them.

      if Delay_Count > 1 then
         Append_To (Delay_Alt_List,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices =>
               New_List (Make_Others_Choice (Loc)),
             Statements       =>
               New_List (Make_Null_Statement (Loc))));

         Delay_Case := New_List (
           Make_Case_Statement (Loc,
             Expression   => New_Reference_To (Delay_Index, Loc),
             Alternatives => Delay_Alt_List));
      else
         Delay_Case := Delay_Alt_List;
      end if;

      --  If there are no delay alternatives, we append the case statement
      --  to the statement list.

      if Delay_Count = 0 then
         Append_List (Accept_Case, Stats);

      --  Delay alternatives present

      else
         --  If delay alternatives are present we generate:

         --    find minimum delay.
         --    DX := minimum delay;
         --    M := <delay mode>;
         --    Timed_Selective_Wait (Q'Unchecked_Access, Delay_Mode, P,
         --      DX, MX, X);
         --
         --    if X = No_Rendezvous then
         --      case statement for delay statements.
         --    else
         --      case statement for accept alternatives.
         --    end if;

         declare
            Cases : Node_Id;
            Stmt  : Node_Id;
            Parms : List_Id;
            Parm  : Node_Id;
            Conv  : Node_Id;

         begin
            --  The type of the delay expression is known to be legal

            if Time_Type = Standard_Duration then
               Conv := New_Reference_To (Delay_Min, Loc);

            elsif Is_RTE (Base_Type (Etype (Time_Type)), RO_CA_Time) then
               Conv := Make_Function_Call (Loc,
                 New_Reference_To (RTE (RO_CA_To_Duration), Loc),
                 New_List (New_Reference_To (Delay_Min, Loc)));

            else
               pragma Assert
                 (Is_RTE (Base_Type (Etype (Time_Type)), RO_RT_Time));

               Conv := Make_Function_Call (Loc,
                 New_Reference_To (RTE (RO_RT_To_Duration), Loc),
                 New_List (New_Reference_To (Delay_Min, Loc)));
            end if;

            Stmt := Make_Assignment_Statement (Loc,
              Name => New_Reference_To (D, Loc),
              Expression => Conv);

            --  Change the value for Accept_Modes. (Else_Mode -> Delay_Mode)

            Parms := Parameter_Associations (Select_Call);
            Parm := First (Parms);

            while Present (Parm)
              and then Parm /= Select_Mode
            loop
               Next (Parm);
            end loop;

            pragma Assert (Present (Parm));
            Rewrite (Parm, New_Reference_To (RTE (RE_Delay_Mode), Loc));
            Analyze (Parm);

            --  Prepare two new parameters of Duration and Delay_Mode type
            --  which represent the value and the mode of the minimum delay.

            Next (Parm);
            Insert_After (Parm, New_Reference_To (M, Loc));
            Insert_After (Parm, New_Reference_To (D, Loc));

            --  Create a call to RTS

            Rewrite (Select_Call,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Timed_Selective_Wait), Loc),
                Parameter_Associations => Parms));

            --  This new call should follow the calculation of the minimum
            --  delay.

            Insert_List_Before (Select_Call, Delay_List);

            if Check_Guard then
               Stmt :=
                 Make_Implicit_If_Statement (N,
                   Condition => New_Reference_To (Guard_Open, Loc),
                   Then_Statements =>
                     New_List (New_Copy_Tree (Stmt),
                       New_Copy_Tree (Select_Call)),
                   Else_Statements => Accept_Or_Raise);
               Rewrite (Select_Call, Stmt);
            else
               Insert_Before (Select_Call, Stmt);
            end if;

            Cases :=
              Make_Implicit_If_Statement (N,
                Condition => Make_Op_Eq (Loc,
                  Left_Opnd  => New_Reference_To (Xnam, Loc),
                  Right_Opnd =>
                    New_Reference_To (RTE (RE_No_Rendezvous), Loc)),

                Then_Statements => Delay_Case,
                Else_Statements => Accept_Case);

            Append (Cases, Stats);
         end;
      end if;

      --  Replace accept statement with appropriate block

      Block :=
        Make_Block_Statement (Loc,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stats));

      Rewrite (N, Block);
      Analyze (N);

      --  Note: have to worry more about abort deferral in above code ???

      --  Final step is to unstack the Accept_Address entries for all accept
      --  statements appearing in accept alternatives in the select statement

      Alt := First (Alts);
      while Present (Alt) loop
         if Nkind (Alt) = N_Accept_Alternative then
            Remove_Last_Elmt (Accept_Address
              (Entity (Entry_Direct_Name (Accept_Statement (Alt)))));
         end if;

         Next (Alt);
      end loop;
   end Expand_N_Selective_Accept;

   --------------------------------------
   -- Expand_N_Single_Task_Declaration --
   --------------------------------------

   --  Single task declarations should never be present after semantic
   --  analysis, since we expect them to be replaced by a declaration of an
   --  anonymous task type, followed by a declaration of the task object. We
   --  include this routine to make sure that is happening!

   procedure Expand_N_Single_Task_Declaration (N : Node_Id) is
   begin
      raise Program_Error;
   end Expand_N_Single_Task_Declaration;

   ------------------------
   -- Expand_N_Task_Body --
   ------------------------

   --  Given a task body

   --    task body tname is
   --       <declarations>
   --    begin
   --       <statements>
   --    end x;

   --  This expansion routine converts it into a procedure and sets the
   --  elaboration flag for the procedure to true, to represent the fact
   --  that the task body is now elaborated:

   --    procedure tnameB (_Task : access tnameV) is
   --       discriminal : dtype renames _Task.discriminant;

   --       procedure _clean is
   --       begin
   --          Abort_Defer.all;
   --          Complete_Task;
   --          Abort_Undefer.all;
   --          return;
   --       end _clean;

   --    begin
   --       Abort_Undefer.all;
   --       <declarations>
   --       System.Task_Stages.Complete_Activation;
   --       <statements>
   --    at end
   --       _clean;
   --    end tnameB;

   --    tnameE := True;

   --  In addition, if the task body is an activator, then a call to activate
   --  tasks is added at the start of the statements, before the call to
   --  Complete_Activation, and if in addition the task is a master then it
   --  must be established as a master. These calls are inserted and analyzed
   --  in Expand_Cleanup_Actions, when the Handled_Sequence_Of_Statements is
   --  expanded.

   --  There is one discriminal declaration line generated for each
   --  discriminant that is present to provide an easy reference point for
   --  discriminant references inside the body (see Exp_Ch2.Expand_Name).

   --  Note on relationship to GNARLI definition. In the GNARLI definition,
   --  task body procedures have a profile (Arg : System.Address). That is
   --  needed because GNARLI has to use the same access-to-subprogram type
   --  for all task types. We depend here on knowing that in GNAT, passing
   --  an address argument by value is identical to passing a record value
   --  by access (in either case a single pointer is passed), so even though
   --  this procedure has the wrong profile. In fact it's all OK, since the
   --  callings sequence is identical.

   procedure Expand_N_Task_Body (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Ttyp  : constant Entity_Id  := Corresponding_Spec (N);
      Call  : Node_Id;
      New_N : Node_Id;

   begin
      --  Here we start the expansion by generating discriminal declarations

      Add_Discriminal_Declarations (Declarations (N), Ttyp, Name_uTask, Loc);

      --  Add a call to Abort_Undefer at the very beginning of the task
      --  body since this body is called with abort still deferred.

      if Abort_Allowed then
         Call := Build_Runtime_Call (Loc, RE_Abort_Undefer);
         Insert_Before
           (First (Statements (Handled_Statement_Sequence (N))), Call);
         Analyze (Call);
      end if;

      --  The statement part has already been protected with an at_end and
      --  cleanup actions. The call to Complete_Activation must be placed
      --  at the head of the sequence of statements of that block. The
      --  declarations have been merged in this sequence of statements but
      --  the first real statement is accessible from the First_Real_Statement
      --  field (which was set for exactly this purpose).

      if Restricted_Profile then
         Call := Build_Runtime_Call (Loc, RE_Complete_Restricted_Activation);
      else
         Call := Build_Runtime_Call (Loc, RE_Complete_Activation);
      end if;

      Insert_Before
        (First_Real_Statement (Handled_Statement_Sequence (N)), Call);
      Analyze (Call);

      New_N :=
        Make_Subprogram_Body (Loc,
          Specification => Build_Task_Proc_Specification (Ttyp),
          Declarations  => Declarations (N),
          Handled_Statement_Sequence => Handled_Statement_Sequence (N));

      --  If the task contains generic instantiations, cleanup actions
      --  are delayed until after instantiation. Transfer the activation
      --  chain to the subprogram, to insure that the activation call is
      --  properly generated. It the task body contains inner tasks, indicate
      --  that the subprogram is a task master.

      if Delay_Cleanups (Ttyp) then
         Set_Activation_Chain_Entity (New_N, Activation_Chain_Entity (N));
         Set_Is_Task_Master  (New_N, Is_Task_Master (N));
      end if;

      Rewrite (N, New_N);
      Analyze (N);

      --  Set elaboration flag immediately after task body. If the body is a
      --  subunit, the flag is set in the declarative part containing the stub.

      if Nkind (Parent (N)) /= N_Subunit then
         Insert_After (N,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Identifier (Loc, New_External_Name (Chars (Ttyp), 'E')),
             Expression => New_Reference_To (Standard_True, Loc)));
      end if;

      --  Ada 2005 (AI-345): Construct the primitive entry wrapper bodies after
      --  the task body. At this point the entry specs have been created,
      --  frozen and included in the dispatch table for the task type.

      pragma Assert (Present (Corresponding_Record_Type (Ttyp)));

      if Ada_Version >= Ada_05
        and then Present (Task_Definition (Parent (Ttyp)))
        and then Present (Abstract_Interfaces
                          (Corresponding_Record_Type (Ttyp)))
      then
         declare
            Current_Node : Node_Id;
            Vis_Decl     : Node_Id :=
              First (Visible_Declarations (Task_Definition (Parent (Ttyp))));
            Wrap_Body    : Node_Id;

         begin
            if Nkind (Parent (N)) = N_Subunit then
               Current_Node := Corresponding_Stub (Parent (N));
            else
               Current_Node := N;
            end if;

            --  Examine the visible declarations of the task type, looking for
            --  an entry declaration. We do not consider entry families since
            --  they cannot have dispatching operations, thus they do not need
            --  entry wrappers.

            while Present (Vis_Decl) loop
               if Nkind (Vis_Decl) = N_Entry_Declaration
                 and then Ekind (Defining_Identifier (Vis_Decl)) = E_Entry
               then

                  --  Create the specification of the wrapper

                  Wrap_Body :=
                    Build_Wrapper_Body (Loc,
                      Proc_Nam => Defining_Identifier (Vis_Decl),
                      Obj_Typ  => Corresponding_Record_Type (Ttyp),
                      Formals  => Parameter_Specifications (Vis_Decl));

                  if Wrap_Body /= Empty then
                     Insert_After (Current_Node, Wrap_Body);
                     Current_Node := Wrap_Body;

                     Analyze (Wrap_Body);
                  end if;
               end if;

               Next (Vis_Decl);
            end loop;
         end;
      end if;
   end Expand_N_Task_Body;

   ------------------------------------
   -- Expand_N_Task_Type_Declaration --
   ------------------------------------

   --  We have several things to do. First we must create a Boolean flag used
   --  to mark if the body is elaborated yet. This variable gets set to True
   --  when the body of the task is elaborated (we can't rely on the normal
   --  ABE mechanism for the task body, since we need to pass an access to
   --  this elaboration boolean to the runtime routines).

   --    taskE : aliased Boolean := False;

   --  Next a variable is declared to hold the task stack size (either the
   --  default : Unspecified_Size, or a value that is set by a pragma
   --  Storage_Size). If the value of the pragma Storage_Size is static, then
   --  the variable is initialized with this value:

   --    taskZ : Size_Type := Unspecified_Size;
   --  or
   --    taskZ : Size_Type := Size_Type (size_expression);

   --  Next we create a corresponding record type declaration used to represent
   --  values of this task. The general form of this type declaration is

   --    type taskV (discriminants) is record
   --      _Task_Id     : Task_Id;
   --      entry_family : array (bounds) of Void;
   --      _Priority    : Integer         := priority_expression;
   --      _Size        : Size_Type       := Size_Type (size_expression);
   --      _Task_Info   : Task_Info_Type  := task_info_expression;
   --    end record;

   --  The discriminants are present only if the corresponding task type has
   --  discriminants, and they exactly mirror the task type discriminants.

   --  The Id field is always present. It contains the Task_Id value, as set by
   --  the call to Create_Task. Note that although the task is limited, the
   --  task value record type is not limited, so there is no problem in passing
   --  this field as an out parameter to Create_Task.

   --  One entry_family component is present for each entry family in the task
   --  definition. The bounds correspond to the bounds of the entry family
   --  (which may depend on discriminants). The element type is void, since we
   --  only need the bounds information for determining the entry index. Note
   --  that the use of an anonymous array would normally be illegal in this
   --  context, but this is a parser check, and the semantics is quite prepared
   --  to handle such a case.

   --  The _Size field is present only if a Storage_Size pragma appears in the
   --  task definition. The expression captures the argument that was present
   --  in the pragma, and is used to override the task stack size otherwise
   --  associated with the task type.

   --  The _Priority field is present only if a Priority or Interrupt_Priority
   --  pragma appears in the task definition. The expression captures the
   --  argument that was present in the pragma, and is used to provide the Size
   --  parameter to the call to Create_Task.

   --  The _Task_Info field is present only if a Task_Info pragma appears in
   --  the task definition. The expression captures the argument that was
   --  present in the pragma, and is used to provide the Task_Image parameter
   --  to the call to Create_Task.

   --  When a task is declared, an instance of the task value record is
   --  created. The elaboration of this declaration creates the correct bounds
   --  for the entry families, and also evaluates the size, priority, and
   --  task_Info expressions if needed. The initialization routine for the task
   --  type itself then calls Create_Task with appropriate parameters to
   --  initialize the value of the Task_Id field.

   --  Note: the address of this record is passed as the "Discriminants"
   --  parameter for Create_Task. Since Create_Task merely passes this onto the
   --  body procedure, it does not matter that it does not quite match the
   --  GNARLI model of what is being passed (the record contains more than just
   --  the discriminants, but the discriminants can be found from the record
   --  value).

   --  The Entity_Id for this created record type is placed in the
   --  Corresponding_Record_Type field of the associated task type entity.

   --  Next we create a procedure specification for the task body procedure:

   --    procedure taskB (_Task : access taskV);

   --  Note that this must come after the record type declaration, since
   --  the spec refers to this type. It turns out that the initialization
   --  procedure for the value type references the task body spec, but that's
   --  fine, since it won't be generated till the freeze point for the type,
   --  which is certainly after the task body spec declaration.

   --  Finally, we set the task index value field of the entry attribute in
   --  the case of a simple entry.

   procedure Expand_N_Task_Type_Declaration (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Tasktyp   : constant Entity_Id  := Etype (Defining_Identifier (N));
      Tasknm    : constant Name_Id    := Chars (Tasktyp);
      Taskdef   : constant Node_Id    := Task_Definition (N);

      Proc_Spec  : Node_Id;
      Rec_Decl   : Node_Id;
      Rec_Ent    : Entity_Id;
      Cdecls     : List_Id;
      Elab_Decl  : Node_Id;
      Size_Decl  : Node_Id;
      Body_Decl  : Node_Id;
      Task_Size  : Node_Id;
      Ent_Stack  : Entity_Id;
      Decl_Stack : Node_Id;

   begin
      --  If already expanded, nothing to do

      if Present (Corresponding_Record_Type (Tasktyp)) then
         return;
      end if;

      --  Here we will do the expansion

      Rec_Decl := Build_Corresponding_Record (N, Tasktyp, Loc);

      --  Ada 2005 (AI-345): Propagate the attribute that contains the list
      --  of implemented interfaces.

      Set_Interface_List (Type_Definition (Rec_Decl), Interface_List (N));

      Rec_Ent  := Defining_Identifier (Rec_Decl);
      Cdecls   := Component_Items (Component_List
                                     (Type_Definition (Rec_Decl)));

      Qualify_Entity_Names (N);

      --  First create the elaboration variable

      Elab_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Sloc (Tasktyp),
              Chars => New_External_Name (Tasknm, 'E')),
          Aliased_Present      => True,
          Object_Definition    => New_Reference_To (Standard_Boolean, Loc),
          Expression           => New_Reference_To (Standard_False, Loc));
      Insert_After (N, Elab_Decl);

      --  Next create the declaration of the size variable (tasknmZ)

      Set_Storage_Size_Variable (Tasktyp,
        Make_Defining_Identifier (Sloc (Tasktyp),
          Chars => New_External_Name (Tasknm, 'Z')));

      if Present (Taskdef) and then Has_Storage_Size_Pragma (Taskdef) and then
        Is_Static_Expression (Expression (First (
          Pragma_Argument_Associations (Find_Task_Or_Protected_Pragma (
            Taskdef, Name_Storage_Size)))))
      then
         Size_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Storage_Size_Variable (Tasktyp),
             Object_Definition => New_Reference_To (RTE (RE_Size_Type), Loc),
             Expression =>
               Convert_To (RTE (RE_Size_Type),
                 Relocate_Node (
                   Expression (First (
                     Pragma_Argument_Associations (
                       Find_Task_Or_Protected_Pragma
                         (Taskdef, Name_Storage_Size)))))));

      else
         Size_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Storage_Size_Variable (Tasktyp),
             Object_Definition => New_Reference_To (RTE (RE_Size_Type), Loc),
             Expression => New_Reference_To (RTE (RE_Unspecified_Size), Loc));
      end if;

      Insert_After (Elab_Decl, Size_Decl);

      --  Next build the rest of the corresponding record declaration. This is
      --  done last, since the corresponding record initialization procedure
      --  will reference the previously created entities.

      --  Fill in the component declarations -- first the _Task_Id field

      Append_To (Cdecls,
        Make_Component_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uTask_Id),
          Component_Definition =>
            Make_Component_Definition (Loc,
              Aliased_Present    => False,
              Subtype_Indication => New_Reference_To (RTE (RO_ST_Task_Id),
                                    Loc))));

      --  Declare static ATCB (that is, created by the expander) if we are
      --  using the Restricted run time.

      if Restricted_Profile then
         Append_To (Cdecls,
           Make_Component_Declaration (Loc,
             Defining_Identifier  =>
               Make_Defining_Identifier (Loc, Name_uATCB),

             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present     => True,
                 Subtype_Indication  => Make_Subtype_Indication (Loc,
                   Subtype_Mark => New_Occurrence_Of
                     (RTE (RE_Ada_Task_Control_Block), Loc),

                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints =>
                         New_List (Make_Integer_Literal (Loc, 0)))))));

      end if;

      --  Declare static stack (that is, created by the expander) if we are
      --  using the Restricted run time on a bare board configuration.

      if Restricted_Profile
        and then Preallocated_Stacks_On_Target
      then
         --  First we need to extract the appropriate stack size

         Ent_Stack := Make_Defining_Identifier (Loc, Name_uStack);

         if Present (Taskdef) and then Has_Storage_Size_Pragma (Taskdef) then
            Task_Size := Relocate_Node (
              Expression (First (
                Pragma_Argument_Associations (
                  Find_Task_Or_Protected_Pragma
                    (Taskdef, Name_Storage_Size)))));
         else
            Task_Size :=
              New_Reference_To (RTE (RE_Default_Stack_Size), Loc);
         end if;

         Decl_Stack := Make_Component_Declaration (Loc,
           Defining_Identifier  => Ent_Stack,

           Component_Definition =>
             Make_Component_Definition (Loc,
               Aliased_Present     => True,
               Subtype_Indication  => Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Storage_Array), Loc),

                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints  => New_List (Make_Range (Loc,
                       Low_Bound  => Make_Integer_Literal (Loc, 1),
                       High_Bound => Convert_To (RTE (RE_Storage_Offset),
                         Task_Size)))))));

         Append_To (Cdecls, Decl_Stack);

         --  The appropriate alignment for the stack is ensured by the run-time
         --  code in charge of task creation.

      end if;

      --  Add components for entry families

      Collect_Entry_Families (Loc, Cdecls, Size_Decl, Tasktyp);

      --  Add the _Priority component if a Priority pragma is present

      if Present (Taskdef) and then Has_Priority_Pragma (Taskdef) then
         declare
            Prag : constant Node_Id :=
                     Find_Task_Or_Protected_Pragma (Taskdef, Name_Priority);
            Expr : Node_Id;

         begin
            Expr := First (Pragma_Argument_Associations (Prag));

            if Nkind (Expr) = N_Pragma_Argument_Association then
               Expr := Expression (Expr);
            end if;

            Expr := New_Copy_Tree (Expr);

            --  Add conversion to proper type to do range check if required
            --  Note that for runtime units, we allow out of range interrupt
            --  priority values to be used in a priority pragma. This is for
            --  the benefit of some versions of System.Interrupts which use
            --  a special server task with maximum interrupt priority.

            if Chars (Prag) = Name_Priority
              and then not GNAT_Mode
            then
               Rewrite (Expr, Convert_To (RTE (RE_Priority), Expr));
            else
               Rewrite (Expr, Convert_To (RTE (RE_Any_Priority), Expr));
            end if;

            Append_To (Cdecls,
              Make_Component_Declaration (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uPriority),
                Component_Definition =>
                  Make_Component_Definition (Loc,
                    Aliased_Present    => False,
                    Subtype_Indication => New_Reference_To (Standard_Integer,
                                                            Loc)),
                Expression => Expr));
         end;
      end if;

      --  Add the _Task_Size component if a Storage_Size pragma is present

      if Present (Taskdef)
        and then Has_Storage_Size_Pragma (Taskdef)
      then
         Append_To (Cdecls,
           Make_Component_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uSize),

             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => False,
                 Subtype_Indication => New_Reference_To (RTE (RE_Size_Type),
                                                         Loc)),

             Expression =>
               Convert_To (RTE (RE_Size_Type),
                 Relocate_Node (
                   Expression (First (
                     Pragma_Argument_Associations (
                       Find_Task_Or_Protected_Pragma
                         (Taskdef, Name_Storage_Size))))))));
      end if;

      --  Add the _Task_Info component if a Task_Info pragma is present

      if Present (Taskdef) and then Has_Task_Info_Pragma (Taskdef) then
         Append_To (Cdecls,
           Make_Component_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Name_uTask_Info),

             Component_Definition =>
               Make_Component_Definition (Loc,
                 Aliased_Present    => False,
                 Subtype_Indication =>
                   New_Reference_To (RTE (RE_Task_Info_Type), Loc)),

             Expression => New_Copy (
               Expression (First (
                 Pragma_Argument_Associations (
                   Find_Task_Or_Protected_Pragma
                     (Taskdef, Name_Task_Info)))))));
      end if;

      Insert_After (Size_Decl, Rec_Decl);

      --  Analyze the record declaration immediately after construction,
      --  because the initialization procedure is needed for single task
      --  declarations before the next entity is analyzed.

      Analyze (Rec_Decl);

      --  Create the declaration of the task body procedure

      Proc_Spec := Build_Task_Proc_Specification (Tasktyp);
      Body_Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification => Proc_Spec);

      Insert_After (Rec_Decl, Body_Decl);

      --  The subprogram does not comes from source, so we have to indicate the
      --  need for debugging information explicitly.

      Set_Needs_Debug_Info
        (Defining_Entity (Proc_Spec), Comes_From_Source (Original_Node (N)));

      --  Ada 2005 (AI-345): Construct the primitive entry wrapper specs before
      --  the corresponding record has been frozen.

      if Ada_Version >= Ada_05
        and then Present (Taskdef)
        and then Present (Corresponding_Record_Type
                          (Defining_Identifier (Parent (Taskdef))))
        and then Present (Abstract_Interfaces
                          (Corresponding_Record_Type
                           (Defining_Identifier (Parent (Taskdef)))))
      then
         declare
            Current_Node : Node_Id := Rec_Decl;
            Vis_Decl     : Node_Id := First (Visible_Declarations (Taskdef));
            Wrap_Spec    : Node_Id;
            New_N        : Node_Id;

         begin
            --  Examine the visible declarations of the task type, looking for
            --  an entry declaration. We do not consider entry families since
            --  they cannot have dispatching operations, thus they do not need
            --  entry wrappers.

            while Present (Vis_Decl) loop
               if Nkind (Vis_Decl) = N_Entry_Declaration
                 and then Ekind (Defining_Identifier (Vis_Decl)) = E_Entry
               then
                  Wrap_Spec :=
                    Build_Wrapper_Spec (Loc,
                      Proc_Nam => Defining_Identifier (Vis_Decl),
                      Obj_Typ  => Etype (Rec_Ent),
                      Formals  => Parameter_Specifications (Vis_Decl));

                  if Wrap_Spec /= Empty then
                     New_N :=
                       Make_Subprogram_Declaration (Loc,
                         Specification => Wrap_Spec);

                     Insert_After (Current_Node, New_N);
                     Current_Node := New_N;

                     Analyze (New_N);
                  end if;
               end if;

               Next (Vis_Decl);
            end loop;
         end;
      end if;

      --  Ada 2005 (AI-345): We must defer freezing to allow further
      --  declaration of primitive subprograms covering task interfaces

      if Ada_Version <= Ada_95 then

         --  Now we can freeze the corresponding record. This needs manually
         --  freezing, since it is really part of the task type, and the task
         --  type is frozen at this stage. We of course need the initialization
         --  procedure for this corresponding record type and we won't get it
         --  in time if we don't freeze now.

         declare
            L : constant List_Id := Freeze_Entity (Rec_Ent, Loc);

         begin
            if Is_Non_Empty_List (L) then
               Insert_List_After (Body_Decl, L);
            end if;
         end;
      end if;

      --  Complete the expansion of access types to the current task type, if
      --  any were declared.

      Expand_Previous_Access_Type (Tasktyp);
   end Expand_N_Task_Type_Declaration;

   -------------------------------
   -- Expand_N_Timed_Entry_Call --
   -------------------------------

   --  A timed entry call in normal case is not implemented using ATC mechanism
   --  anymore for efficiency reason.

   --     select
   --        T.E;
   --        S1;
   --     or
   --        Delay D;
   --        S2;
   --     end select;

   --  is expanded as follow:

   --  1) When T.E is a task entry_call;

   --    declare
   --       B  : Boolean;
   --       X  : Task_Entry_Index := <entry index>;
   --       DX : Duration := To_Duration (D);
   --       M  : Delay_Mode := <discriminant>;
   --       P  : parms := (parm, parm, parm);

   --    begin
   --       Timed_Protected_Entry_Call (<acceptor-task>, X, P'Address,
   --         DX, M, B);
   --       if B then
   --          S1;
   --       else
   --          S2;
   --       end if;
   --    end;

   --  2) When T.E is a protected entry_call;

   --    declare
   --       B  : Boolean;
   --       X  : Protected_Entry_Index := <entry index>;
   --       DX : Duration := To_Duration (D);
   --       M  : Delay_Mode := <discriminant>;
   --       P  : parms := (parm, parm, parm);

   --    begin
   --       Timed_Protected_Entry_Call (<object>'unchecked_access, X,
   --         P'Address, DX, M, B);
   --       if B then
   --          S1;
   --       else
   --          S2;
   --       end if;
   --    end;

   --  3) Ada 2005 (AI-345): When T.E is a dispatching procedure call;

   --    declare
   --       B  : Boolean := False;
   --       C  : Ada.Tags.Prim_Op_Kind;
   --       DX : Duration := To_Duration (D)
   --       M  : Integer :=...;
   --       P  : Parameters := (Param1 .. ParamN);
   --       S  : constant Iteger := DT_Position (<dispatching-procedure>);

   --    begin
   --       disp_timed_select (<object>, S, P'Address, DX, M, C, B);

   --       if C = POK_Protected_Entry
   --         or else C = POK_Task_Entry
   --       then
   --          Param1 := P.Param1;
   --          ...
   --          ParamN := P.ParamN;
   --       end if;

   --       if B then
   --          if C = POK_Procedure
   --            or else C = POK_Protected_Procedure
   --            or else C = POK_Task_Procedure
   --          then
   --             T.E;
   --          end if;
   --          S1;
   --       else
   --          S2;
   --       end if;
   --    end;

   procedure Expand_N_Timed_Entry_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      E_Call  : Node_Id :=
                  Entry_Call_Statement (Entry_Call_Alternative (N));
      E_Stats : constant List_Id :=
                  Statements (Entry_Call_Alternative (N));
      D_Stat  : constant Node_Id :=
                  Delay_Statement (Delay_Alternative (N));
      D_Stats : constant List_Id :=
                  Statements (Delay_Alternative (N));

      Actuals  : List_Id;
      Blk_Typ  : Entity_Id;
      Call     : Node_Id;
      Call_Ent : Entity_Id;
      Concval  : Node_Id;
      D_Conv   : Node_Id;
      D_Disc   : Node_Id;
      D_Type   : Entity_Id;
      Decls    : List_Id;
      Dummy    : Node_Id;
      Ename    : Node_Id;
      Formals  : List_Id;
      Index    : Node_Id;
      N_Stats  : List_Id;
      Obj      : Entity_Id;
      Param    : Node_Id;
      Params   : List_Id;
      Stmt     : Node_Id;
      Stmts    : List_Id;
      Unpack   : List_Id;

      B : Entity_Id;  --  Call status flag
      C : Entity_Id;  --  Call kind
      D : Entity_Id;  --  Delay
      M : Entity_Id;  --  Delay mode
      P : Entity_Id;  --  Parameter block
      S : Entity_Id;  --  Primitive operation slot

   begin
      --  The arguments in the call may require dynamic allocation, and the
      --  call statement may have been transformed into a block. The block
      --  may contain additional declarations for internal entities, and the
      --  original call is found by sequential search.

      if Nkind (E_Call) = N_Block_Statement then
         E_Call := First (Statements (Handled_Statement_Sequence (E_Call)));
         while Nkind (E_Call) /= N_Procedure_Call_Statement
           and then Nkind (E_Call) /= N_Entry_Call_Statement
         loop
            Next (E_Call);
         end loop;
      end if;

      if Ada_Version >= Ada_05
        and then Nkind (E_Call) = N_Procedure_Call_Statement
      then
         Extract_Dispatching_Call (E_Call, Call_Ent, Obj, Actuals, Formals);

         Decls := New_List;
         Stmts := New_List;

      else
         --  Build an entry call using Simple_Entry_Call

         Extract_Entry (E_Call, Concval, Ename, Index);
         Build_Simple_Entry_Call (E_Call, Concval, Ename, Index);

         Decls := Declarations (E_Call);
         Stmts := Statements (Handled_Statement_Sequence (E_Call));

         if No (Decls) then
            Decls := New_List;
         end if;
      end if;

      --  Call status flag processing

      if Ada_Version >= Ada_05
        and then Nkind (E_Call) = N_Procedure_Call_Statement
      then
         --  Generate:
         --    B : Boolean := False;

         B := SEU.Build_B (Loc, Decls);

      else
         --  Generate:
         --    B : Boolean;

         B := Make_Defining_Identifier (Loc, Name_uB);

         Prepend_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               B,
             Object_Definition =>
               New_Reference_To (Standard_Boolean, Loc)));
      end if;

      --  Call kind processing

      if Ada_Version >= Ada_05
        and then Nkind (E_Call) = N_Procedure_Call_Statement
      then
         --  Generate:
         --    C : Ada.Tags.Prim_Op_Kind;

         C := SEU.Build_C (Loc, Decls);
      end if;

      --  Duration and mode processing

      D_Type := Base_Type (Etype (Expression (D_Stat)));

      --  Use the type of the delay expression (Calendar or Real_Time)
      --  to generate the appropriate conversion.

      if Nkind (D_Stat) = N_Delay_Relative_Statement then
         D_Disc := Make_Integer_Literal (Loc, 0);
         D_Conv := Relocate_Node (Expression (D_Stat));

      elsif Is_RTE (D_Type, RO_CA_Time) then
         D_Disc := Make_Integer_Literal (Loc, 1);
         D_Conv := Make_Function_Call (Loc,
           New_Reference_To (RTE (RO_CA_To_Duration), Loc),
           New_List (New_Copy (Expression (D_Stat))));

      else pragma Assert (Is_RTE (D_Type, RO_RT_Time));
         D_Disc := Make_Integer_Literal (Loc, 2);
         D_Conv := Make_Function_Call (Loc,
           New_Reference_To (RTE (RO_RT_To_Duration), Loc),
           New_List (New_Copy (Expression (D_Stat))));
      end if;

      D := Make_Defining_Identifier (Loc, New_Internal_Name ('D'));

      --  Generate:
      --    D : Duration;

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            D,
          Object_Definition =>
            New_Reference_To (Standard_Duration, Loc)));

      M := Make_Defining_Identifier (Loc, New_Internal_Name ('M'));

      --  Generate:
      --    M : Integer := (0 | 1 | 2);

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            M,
          Object_Definition =>
            New_Reference_To (Standard_Integer, Loc),
          Expression =>
            D_Disc));

      --  Do the assignement at this stage only because the evaluation of the
      --  expression must not occur before (see ACVC C97302A).

      Append_To (Stmts,
        Make_Assignment_Statement (Loc,
          Name =>
            New_Reference_To (D, Loc),
          Expression =>
            D_Conv));

      --  Parameter block processing

      --  Manually create the parameter block for dispatching calls. In the
      --  case of entries, the block has already been created during the call
      --  to Build_Simple_Entry_Call.

      if Ada_Version >= Ada_05
        and then Nkind (E_Call) = N_Procedure_Call_Statement
      then
         Blk_Typ := Build_Parameter_Block (Loc, Actuals, Formals, Decls);
         P       := Parameter_Block_Pack  (Loc, Blk_Typ, Actuals, Formals,
                      Decls, Stmts);

         --  Dispatch table slot processing, generate:
         --    S : constant Integer :=
         --          Ada.Tags.Get_Offset_Index (
         --            Unchecked_Convert_To (Ada.Tags.Interface_Tag, Obj),
         --            DT_Position (<dispatching-procedure>));

         S := SEU.Build_S (Loc, Decls, Obj, Call_Ent);

         --  Generate:
         --    _dispatching_timed_select (Obj, S, P'address, D, M, C, B);

         --  where Obj is the controlling formal parameter, S is the dispatch
         --  table slot number of the dispatching operation, P is the wrapped
         --  parameter block, D is the duration, M is the duration mode, C is
         --  the call kind and B is the call status.

         Params := New_List;

         Append_To (Params, New_Copy_Tree    (Obj));
         Append_To (Params, New_Reference_To (S, Loc));
         Append_To (Params, Make_Attribute_Reference (Loc,
                              Prefix => New_Reference_To (P, Loc),
                              Attribute_Name => Name_Address));
         Append_To (Params, New_Reference_To (D, Loc));
         Append_To (Params, New_Reference_To (M, Loc));
         Append_To (Params, New_Reference_To (C, Loc));
         Append_To (Params, New_Reference_To (B, Loc));

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (
                 Find_Prim_Op (Etype (Etype (Obj)),
                   Name_uDisp_Timed_Select),
                 Loc),
             Parameter_Associations =>
               Params));

         --  Generate:
         --    if C = POK_Protected_Entry
         --      or else C = POK_Task_Entry
         --    then
         --       Param1 := P.Param1;
         --       ...
         --       ParamN := P.ParamN;
         --    end if;

         Unpack := Parameter_Block_Unpack (Loc, P, Actuals, Formals);

         --  Generate the if statement only when the packed parameters need
         --  explicit assignments to their corresponding actuals.

         if Present (Unpack) then
            Append_To (Stmts,
              Make_If_Statement (Loc,

                Condition =>
                  Make_Or_Else (Loc,
                    Left_Opnd =>
                      Make_Op_Eq (Loc,
                        Left_Opnd =>
                          New_Reference_To (C, Loc),
                        Right_Opnd =>
                          New_Reference_To (RTE (
                            RE_POK_Protected_Entry), Loc)),
                    Right_Opnd =>
                      Make_Op_Eq (Loc,
                        Left_Opnd =>
                          New_Reference_To (C, Loc),
                        Right_Opnd =>
                          New_Reference_To (RTE (RE_POK_Task_Entry), Loc))),

                Then_Statements => Unpack));
         end if;

         --  Generate:

         --    if B then
         --       if C = POK_Procedure
         --         or else C = POK_Protected_Procedure
         --         or else C = POK_Task_Procedure
         --       then
         --          <dispatching-procedure-call>
         --       end if;
         --       <normal-statements>
         --    else
         --       <delay-statements>
         --    end if;

         N_Stats := New_Copy_List_Tree (E_Stats);

         Prepend_To (N_Stats,
           Make_If_Statement (Loc,

             Condition =>
               Make_Or_Else (Loc,
                 Left_Opnd =>
                   Make_Op_Eq (Loc,
                     Left_Opnd =>
                       New_Reference_To (C, Loc),
                     Right_Opnd =>
                       New_Reference_To (RTE (RE_POK_Procedure), Loc)),
                 Right_Opnd =>
                   Make_Or_Else (Loc,
                     Left_Opnd =>
                       Make_Op_Eq (Loc,
                         Left_Opnd =>
                           New_Reference_To (C, Loc),
                         Right_Opnd =>
                           New_Reference_To (RTE (
                             RE_POK_Protected_Procedure), Loc)),
                     Right_Opnd =>
                       Make_Op_Eq (Loc,
                         Left_Opnd =>
                           New_Reference_To (C, Loc),
                         Right_Opnd =>
                           New_Reference_To (RTE (
                             RE_POK_Task_Procedure), Loc)))),

             Then_Statements =>
               New_List (E_Call)));

         Append_To (Stmts,
           Make_If_Statement (Loc,
             Condition       => New_Reference_To (B, Loc),
             Then_Statements => N_Stats,
             Else_Statements => D_Stats));
      else
         --  Skip assignments to temporaries created for in-out parameters.
         --  This makes unwarranted assumptions about the shape of the expanded
         --  tree for the call, and should be cleaned up ???

         Stmt := First (Stmts);
         while Nkind (Stmt) /= N_Procedure_Call_Statement loop
            Next (Stmt);
         end loop;

         --  Do the assignement at this stage only because the evaluation
         --  of the expression must not occur before (see ACVC C97302A).

         Insert_Before (Stmt,
           Make_Assignment_Statement (Loc,
             Name => New_Reference_To (D, Loc),
             Expression => D_Conv));

         Call   := Stmt;
         Params := Parameter_Associations (Call);

         --  For a protected type, we build a Timed_Protected_Entry_Call

         if Is_Protected_Type (Etype (Concval)) then

            --  Create a new call statement

            Param := First (Params);
            while Present (Param)
              and then not Is_RTE (Etype (Param), RE_Call_Modes)
            loop
               Next (Param);
            end loop;

            Dummy := Remove_Next (Next (Param));

            --  Remove garbage is following the Cancel_Param if present

            Dummy := Next (Param);

            --  Remove the mode of the Protected_Entry_Call call, then remove
            --  the Communication_Block of the Protected_Entry_Call call, and
            --  finally add Duration and a Delay_Mode parameter

            pragma Assert (Present (Param));
            Rewrite (Param, New_Reference_To (D, Loc));

            Rewrite (Dummy, New_Reference_To (M, Loc));

            --  Add a Boolean flag for successful entry call

            Append_To (Params, New_Reference_To (B, Loc));

            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Etype (Concval)) > 1
            then
               Rewrite (Call,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Reference_To (RTE (
                       RE_Timed_Protected_Entry_Call), Loc),
                   Parameter_Associations => Params));
            else
               Param := First (Params);
               while Present (Param)
                 and then not Is_RTE (Etype (Param), RE_Protected_Entry_Index)
               loop
                  Next (Param);
               end loop;

               Remove (Param);

               Rewrite (Call,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (
                     RTE (RE_Timed_Protected_Single_Entry_Call), Loc),
                   Parameter_Associations => Params));
            end if;

         --  For the task case, build a Timed_Task_Entry_Call

         else
            --  Create a new call statement

            Append_To (Params, New_Reference_To (D, Loc));
            Append_To (Params, New_Reference_To (M, Loc));
            Append_To (Params, New_Reference_To (B, Loc));

            Rewrite (Call,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Timed_Task_Entry_Call), Loc),
                Parameter_Associations => Params));
         end if;

         Append_To (Stmts,
           Make_Implicit_If_Statement (N,
             Condition => New_Reference_To (B, Loc),
             Then_Statements => E_Stats,
             Else_Statements => D_Stats));
      end if;

      Rewrite (N,
        Make_Block_Statement (Loc,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Stmts)));

      Analyze (N);
   end Expand_N_Timed_Entry_Call;

   ----------------------------------------
   -- Expand_Protected_Body_Declarations --
   ----------------------------------------

   --  Part of the expansion of a protected body involves the creation of a
   --  declaration that can be referenced from the statement sequences of the
   --  entry bodies:

   --    A : Address;

   --  This declaration is inserted in the declarations of the service entries
   --  procedure for the protected body, and it is important that it be
   --  inserted before the statements of the entry body statement sequences are
   --  analyzed. Thus it would be too late to create this declaration in the
   --  Expand_N_Protected_Body routine, which is why there is a separate
   --  procedure to be called directly from Sem_Ch9.

   --  Ann is used to hold the address of the record containing the parameters
   --  (see Expand_N_Entry_Call for more details on how this record is built).
   --  References to the parameters do an unchecked conversion of this address
   --  to a pointer to the required record type, and then access the field that
   --  holds the value of the required parameter. The entity for the address
   --  variable is held as the top stack element (i.e. the last element) of the
   --  Accept_Address stack in the corresponding entry entity, and this element
   --  must be set in place  before the statements are processed.

   --  No stack is needed for entry bodies, since they cannot be nested, but it
   --  is kept for consistency between protected and task entries. The stack
   --  will never contain more than one element. There is also only one such
   --  variable for a given protected body, but this is placed on the
   --  Accept_Address stack of all of the entries, again for consistency.

   --  To expand the requeue statement, a label is provided at the end of the
   --  loop in the entry service routine created by the expander (see
   --  Expand_N_Protected_Body for details), so that the statement can be
   --  skipped after the requeue is complete. This label is created during the
   --  expansion of the entry body, which will take place after the expansion
   --  of the requeue statements that it contains, so a placeholder defining
   --  identifier is associated with the task type here.

   --  Another label is provided following case statement created by the
   --  expander. This label is need for implementing return statement from
   --  entry body so that a return can be expanded as a goto to this label.
   --  This label is created during the expansion of the entry body, which
   --  will take place after the expansion of the return statements that it
   --  contains. Therefore, just like the label for expanding requeues, we
   --  need another placeholder for the label.

   procedure Expand_Protected_Body_Declarations
     (N       : Node_Id;
      Spec_Id : Entity_Id)
   is
      Op : Node_Id;

   begin
      if No_Run_Time_Mode then
         Error_Msg_CRT ("protected body", N);
         return;

      elsif Expander_Active then

         --  Associate privals with the first subprogram or entry body to be
         --  expanded. These are used to expand references to private data
         --  objects.

         Op := First_Protected_Operation (Declarations (N));

         if Present (Op) then
            Set_Discriminals (Parent (Spec_Id));
            Set_Privals (Parent (Spec_Id), Op, Sloc (N));
         end if;
      end if;
   end Expand_Protected_Body_Declarations;

   -------------------------
   -- External_Subprogram --
   -------------------------

   function External_Subprogram (E : Entity_Id) return Entity_Id is
      Subp : constant Entity_Id := Protected_Body_Subprogram (E);
      Decl : constant Node_Id   := Unit_Declaration_Node (E);

   begin
      --  If the protected operation is defined in the visible part of the
      --  protected type, or if it is an interrupt handler, the internal and
      --  external subprograms follow each other on the entity chain. If the
      --  operation is defined in the private part of the type, there is no
      --  need for a separate locking version of the operation, and internal
      --  calls use the protected_body_subprogram directly.

      if List_Containing (Decl) = Visible_Declarations (Parent (Decl))
        or else Is_Interrupt_Handler (E)
      then
         return Next_Entity (Subp);
      else
         return (Subp);
      end if;
   end External_Subprogram;

   ------------------------------
   -- Extract_Dispatching_Call --
   ------------------------------

   procedure Extract_Dispatching_Call
     (N        : Node_Id;
      Call_Ent : out Entity_Id;
      Object   : out Entity_Id;
      Actuals  : out List_Id;
      Formals  : out List_Id)
   is
      Call_Nam : Node_Id;

   begin
      pragma Assert (Nkind (N) = N_Procedure_Call_Statement);

      if Present (Original_Node (N)) then
         Call_Nam := Name (Original_Node (N));
      else
         Call_Nam := Name (N);
      end if;

      --  Retrieve the name of the dispatching procedure. It contains the
      --  dispatch table slot number.

      loop
         case Nkind (Call_Nam) is
            when N_Identifier =>
               exit;

            when N_Selected_Component =>
               Call_Nam := Selector_Name (Call_Nam);

            when others =>
               raise Program_Error;

         end case;
      end loop;

      Actuals  := Parameter_Associations (N);
      Call_Ent := Entity (Call_Nam);
      Formals  := Parameter_Specifications (Parent (Call_Ent));
      Object   := First (Actuals);

      if Present (Original_Node (Object)) then
         Object := Original_Node (Object);
      end if;
   end Extract_Dispatching_Call;

   -------------------
   -- Extract_Entry --
   -------------------

   procedure Extract_Entry
     (N       : Node_Id;
      Concval : out Node_Id;
      Ename   : out Node_Id;
      Index   : out Node_Id)
   is
      Nam : constant Node_Id := Name (N);

   begin
      --  For a simple entry, the name is a selected component, with the
      --  prefix being the task value, and the selector being the entry.

      if Nkind (Nam) = N_Selected_Component then
         Concval := Prefix (Nam);
         Ename   := Selector_Name (Nam);
         Index   := Empty;

      --  For a member of an entry family, the name is an indexed component
      --  where the prefix is a selected component, whose prefix in turn is
      --  the task value, and whose selector is the entry family. The single
      --  expression in the expressions list of the indexed component is the
      --  subscript for the family.

      else pragma Assert (Nkind (Nam) = N_Indexed_Component);
         Concval := Prefix (Prefix (Nam));
         Ename   := Selector_Name (Prefix (Nam));
         Index   := First (Expressions (Nam));
      end if;
   end Extract_Entry;

   -------------------
   -- Family_Offset --
   -------------------

   function Family_Offset
     (Loc  : Source_Ptr;
      Hi   : Node_Id;
      Lo   : Node_Id;
      Ttyp : Entity_Id) return Node_Id
   is
      function Convert_Discriminant_Ref (Bound : Node_Id) return Node_Id;
      --  If one of the bounds is a reference to a discriminant, replace with
      --  corresponding discriminal of type. Within the body of a task retrieve
      --  the renamed discriminant by simple visibility, using its generated
      --  name. Within a protected object, find the original dis- criminant and
      --  replace it with the discriminal of the current prot- ected operation.

      ------------------------------
      -- Convert_Discriminant_Ref --
      ------------------------------

      function Convert_Discriminant_Ref (Bound : Node_Id) return Node_Id is
         Loc : constant Source_Ptr := Sloc (Bound);
         B   : Node_Id;
         D   : Entity_Id;

      begin
         if Is_Entity_Name (Bound)
           and then Ekind (Entity (Bound)) = E_Discriminant
         then
            if Is_Task_Type (Ttyp)
              and then Has_Completion (Ttyp)
            then
               B := Make_Identifier (Loc, Chars (Entity (Bound)));
               Find_Direct_Name (B);

            elsif Is_Protected_Type (Ttyp) then
               D := First_Discriminant (Ttyp);
               while Chars (D) /= Chars (Entity (Bound)) loop
                  Next_Discriminant (D);
               end loop;

               B := New_Reference_To  (Discriminal (D), Loc);

            else
               B := New_Reference_To (Discriminal (Entity (Bound)), Loc);
            end if;

         elsif Nkind (Bound) = N_Attribute_Reference then
            return Bound;

         else
            B := New_Copy_Tree (Bound);
         end if;

         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Pos,
             Prefix => New_Occurrence_Of (Etype (Bound), Loc),
             Expressions    => New_List (B));
      end Convert_Discriminant_Ref;

   --  Start of processing for Family_Offset

   begin
      return
        Make_Op_Subtract (Loc,
          Left_Opnd  => Convert_Discriminant_Ref (Hi),
          Right_Opnd => Convert_Discriminant_Ref (Lo));
   end Family_Offset;

   -----------------
   -- Family_Size --
   -----------------

   function Family_Size
     (Loc  : Source_Ptr;
      Hi   : Node_Id;
      Lo   : Node_Id;
      Ttyp : Entity_Id) return Node_Id
   is
      Ityp : Entity_Id;

   begin
      if Is_Task_Type (Ttyp) then
         Ityp := RTE (RE_Task_Entry_Index);
      else
         Ityp := RTE (RE_Protected_Entry_Index);
      end if;

      return
        Make_Attribute_Reference (Loc,
          Prefix         => New_Reference_To (Ityp, Loc),
          Attribute_Name => Name_Max,
          Expressions    => New_List (
            Make_Op_Add (Loc,
              Left_Opnd  =>
                Family_Offset (Loc, Hi, Lo, Ttyp),
              Right_Opnd =>
                Make_Integer_Literal (Loc, 1)),
            Make_Integer_Literal (Loc, 0)));
   end Family_Size;

   -----------------------------------
   -- Find_Task_Or_Protected_Pragma --
   -----------------------------------

   function Find_Task_Or_Protected_Pragma
     (T : Node_Id;
      P : Name_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := First (Visible_Declarations (T));
      while Present (N) loop
         if Nkind (N) = N_Pragma then
            if Chars (N) = P then
               return N;

            elsif P = Name_Priority
              and then Chars (N) = Name_Interrupt_Priority
            then
               return N;

            else
               Next (N);
            end if;

         else
            Next (N);
         end if;
      end loop;

      N := First (Private_Declarations (T));
      while Present (N) loop
         if Nkind (N) = N_Pragma then
            if  Chars (N) = P then
               return N;

            elsif P = Name_Priority
              and then Chars (N) = Name_Interrupt_Priority
            then
               return N;

            else
               Next (N);
            end if;

         else
            Next (N);
         end if;
      end loop;

      raise Program_Error;
   end Find_Task_Or_Protected_Pragma;

   -------------------------------
   -- First_Protected_Operation --
   -------------------------------

   function First_Protected_Operation (D : List_Id) return Node_Id is
      First_Op : Node_Id;

   begin
      First_Op := First (D);
      while Present (First_Op)
        and then Nkind (First_Op) /= N_Subprogram_Body
        and then Nkind (First_Op) /= N_Entry_Body
      loop
         Next (First_Op);
      end loop;

      return First_Op;
   end First_Protected_Operation;

   --------------------------------
   -- Index_Constant_Declaration --
   --------------------------------

   function Index_Constant_Declaration
     (N        : Node_Id;
      Index_Id : Entity_Id;
      Prot     : Entity_Id) return List_Id
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Decls     : constant List_Id    := New_List;
      Index_Con : constant Entity_Id  := Entry_Index_Constant (Index_Id);
      Index_Typ : Entity_Id;

      Hi : Node_Id := Type_High_Bound (Etype (Index_Id));
      Lo : Node_Id := Type_Low_Bound  (Etype (Index_Id));

      function Replace_Discriminant (Bound : Node_Id) return Node_Id;
      --  The bounds of the entry index may depend on discriminants, so each
      --  declaration of an entry_index_constant must have its own subtype
      --  declaration, using the local renaming of the object discriminant.

      --------------------------
      -- Replace_Discriminant --
      --------------------------

      function Replace_Discriminant (Bound : Node_Id) return Node_Id is
      begin
         if Nkind (Bound) = N_Identifier
           and then Ekind (Entity (Bound)) = E_Constant
           and then Present (Discriminal_Link (Entity (Bound)))
         then
            return Make_Identifier (Loc, Chars (Entity (Bound)));
         else
            return Duplicate_Subexpr (Bound);
         end if;
      end Replace_Discriminant;

   --  Start of processing for Index_Constant_Declaration

   begin
      Set_Discriminal_Link (Index_Con, Index_Id);

      if Is_Entity_Name (
        Original_Node (Discrete_Subtype_Definition (Parent (Index_Id))))
      then
         --  Simple case: entry family is given by a subtype mark, and index
         --  constant has the same type, no replacement needed.

         Index_Typ := Etype (Index_Id);

      else
         Hi := Replace_Discriminant (Hi);
         Lo := Replace_Discriminant (Lo);

         Index_Typ := Make_Defining_Identifier (Loc, New_Internal_Name ('J'));

         Append (
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Index_Typ,
             Subtype_Indication =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (Base_Type (Etype (Index_Id)), Loc),
                 Constraint =>
                   Make_Range_Constraint (Loc,
                     Range_Expression => Make_Range (Loc, Lo, Hi)))),
           Decls);

      end if;

      Append (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Index_Con,
          Constant_Present => True,
          Object_Definition => New_Occurrence_Of (Index_Typ, Loc),

          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Index_Typ, Loc),
              Attribute_Name => Name_Val,

              Expressions => New_List (

                Make_Op_Add (Loc,
                  Left_Opnd =>
                    Make_Op_Subtract (Loc,
                      Left_Opnd => Make_Identifier (Loc, Name_uE),
                      Right_Opnd =>
                        Entry_Index_Expression (Loc,
                          Defining_Identifier (N), Empty, Prot)),

                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Index_Typ, Loc),
                      Attribute_Name => Name_Pos,
                      Expressions => New_List (
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Reference_To (Index_Typ, Loc),
                    Attribute_Name => Name_First))))))),
      Decls);

      return Decls;
   end Index_Constant_Declaration;

   --------------------------------
   -- Make_Initialize_Protection --
   --------------------------------

   function Make_Initialize_Protection
     (Protect_Rec : Entity_Id) return List_Id
   is
      Loc         : constant Source_Ptr := Sloc (Protect_Rec);
      P_Arr       : Entity_Id;
      Pdef        : Node_Id;
      Pdec        : Node_Id;
      Ptyp        : constant Node_Id :=
                      Corresponding_Concurrent_Type (Protect_Rec);
      Args        : List_Id;
      L           : constant List_Id := New_List;
      Has_Entry   : constant Boolean := Has_Entries (Ptyp);
      Restricted  : constant Boolean := Restricted_Profile;

   begin
      --  We may need two calls to properly initialize the object, one to
      --  Initialize_Protection, and possibly one to Install_Handlers if we
      --  have a pragma Attach_Handler.

      --  Get protected declaration. In the case of a task type declaration,
      --  this is simply the parent of the protected type entity. In the single
      --  protected object declaration, this parent will be the implicit type,
      --  and we can find the corresponding single protected object declaration
      --  by searching forward in the declaration list in the tree.

      --  Is the test for N_Single_Protected_Declaration needed here??? Nodes
      --  of this type should have been removed during semantic analysis.

      Pdec := Parent (Ptyp);
      while Nkind (Pdec) /= N_Protected_Type_Declaration
        and then Nkind (Pdec) /= N_Single_Protected_Declaration
      loop
         Next (Pdec);
      end loop;

      --  Now we can find the object definition from this declaration

      Pdef := Protected_Definition (Pdec);

      --  Build the parameter list for the call. Note that _Init is the name
      --  of the formal for the object to be initialized, which is the task
      --  value record itself.

      Args := New_List;

      --  Object parameter. This is a pointer to the object of type
      --  Protection used by the GNARL to control the protected object.

      Append_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix =>
            Make_Selected_Component (Loc,
              Prefix => Make_Identifier (Loc, Name_uInit),
              Selector_Name => Make_Identifier (Loc, Name_uObject)),
          Attribute_Name => Name_Unchecked_Access));

      --  Priority parameter. Set to Unspecified_Priority unless there is a
      --  priority pragma, in which case we take the value from the pragma,
      --  or there is an interrupt pragma and no priority pragma, and we
      --  set the ceiling to Interrupt_Priority'Last, an implementation-
      --  defined value, see D.3(10).

      if Present (Pdef)
        and then Has_Priority_Pragma (Pdef)
      then
         Append_To (Args,
           Duplicate_Subexpr_No_Checks
             (Expression
               (First
                 (Pragma_Argument_Associations
                   (Find_Task_Or_Protected_Pragma (Pdef, Name_Priority))))));

      elsif Has_Interrupt_Handler (Ptyp)
        or else Has_Attach_Handler (Ptyp)
      then
         --  When no priority is specified but an xx_Handler pragma is,
         --  we default to System.Interrupts.Default_Interrupt_Priority,
         --  see D.3(10).

         Append_To (Args,
           New_Reference_To (RTE (RE_Default_Interrupt_Priority), Loc));

      else
         Append_To (Args,
           New_Reference_To (RTE (RE_Unspecified_Priority), Loc));
      end if;

      if Has_Entry
        or else Has_Interrupt_Handler (Ptyp)
        or else Has_Attach_Handler (Ptyp)
        or else (Ada_Version >= Ada_05
                   and then Present (Interface_List (Parent (Ptyp))))
      then
         --  Compiler_Info parameter. This parameter allows entry body
         --  procedures and barrier functions to be called from the runtime.
         --  It is a pointer to the record generated by the compiler to
         --  represent the protected object.

         if Has_Entry or else not Restricted then
            Append_To (Args,
               Make_Attribute_Reference (Loc,
                 Prefix => Make_Identifier (Loc, Name_uInit),
                 Attribute_Name => Name_Address));
         end if;

         if Has_Entry then

            --  Entry_Bodies parameter. This is a pointer to an array of
            --  pointers to the entry body procedures and barrier functions of
            --  the object. If the protected type has no entries this object
            --  will not exist; in this case, pass a null.

            P_Arr := Entry_Bodies_Array (Ptyp);

            Append_To (Args,
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (P_Arr, Loc),
                Attribute_Name => Name_Unrestricted_Access));

            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Ptyp) > 1
            then
               --  Find index mapping function (clumsy but ok for now)

               while Ekind (P_Arr) /= E_Function loop
                  Next_Entity (P_Arr);
               end loop;

               Append_To (Args,
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To (P_Arr, Loc),
                    Attribute_Name => Name_Unrestricted_Access));
            end if;

         elsif not Restricted then
            Append_To (Args, Make_Null (Loc));
            Append_To (Args, Make_Null (Loc));
         end if;

         if Abort_Allowed
           or else Restriction_Active (No_Entry_Queue) = False
           or else Number_Entries (Ptyp) > 1
         then
            Append_To (L,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  RTE (RE_Initialize_Protection_Entries), Loc),
                Parameter_Associations => Args));

         elsif not Has_Entry and then Restricted then
            Append_To (L,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  RTE (RE_Initialize_Protection), Loc),
                Parameter_Associations => Args));

         else
            Append_To (L,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  RTE (RE_Initialize_Protection_Entry), Loc),
                Parameter_Associations => Args));
         end if;

      else
         Append_To (L,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Initialize_Protection), Loc),
             Parameter_Associations => Args));
      end if;

      if Has_Attach_Handler (Ptyp) then

         --  We have a list of N Attach_Handler (ProcI, ExprI), and we have to
         --  make the following call:

         --  Install_Handlers (_object,
         --    ((Expr1, Proc1'access), ...., (ExprN, ProcN'access));

         --  or, in the case of Ravenscar:

         --  Install_Handlers
         --    ((Expr1, Proc1'access), ...., (ExprN, ProcN'access));

         declare
            Args  : constant List_Id := New_List;
            Table : constant List_Id := New_List;
            Ritem : Node_Id := First_Rep_Item (Ptyp);

         begin
            if not Restricted then

               --  Appends the _object argument

               Append_To (Args,
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     Make_Selected_Component (Loc,
                       Prefix => Make_Identifier (Loc, Name_uInit),
                       Selector_Name => Make_Identifier (Loc, Name_uObject)),
                   Attribute_Name => Name_Unchecked_Access));
            end if;

            --  Build the Attach_Handler table argument

            while Present (Ritem) loop
               if Nkind (Ritem) = N_Pragma
                 and then Chars (Ritem) = Name_Attach_Handler
               then
                  declare
                     Handler : constant Node_Id :=
                                 First (Pragma_Argument_Associations (Ritem));

                     Interrupt : constant Node_Id  := Next (Handler);
                     Expr      : constant  Node_Id := Expression (Interrupt);

                  begin
                     Append_To (Table,
                       Make_Aggregate (Loc, Expressions => New_List (
                         Unchecked_Convert_To
                          (RTE (RE_System_Interrupt_Id), Expr),
                         Make_Attribute_Reference (Loc,
                           Prefix => Make_Selected_Component (Loc,
                              Make_Identifier (Loc, Name_uInit),
                              Duplicate_Subexpr_No_Checks
                                (Expression (Handler))),
                           Attribute_Name => Name_Access))));
                  end;
               end if;

               Next_Rep_Item (Ritem);
            end loop;

            --  Append the table argument we just built

            Append_To (Args, Make_Aggregate (Loc, Table));

            --  Append the Install_Handler call to the statements

            Append_To (L,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Install_Handlers), Loc),
                Parameter_Associations => Args));
         end;
      end if;

      return L;
   end Make_Initialize_Protection;

   ---------------------------
   -- Make_Task_Create_Call --
   ---------------------------

   function Make_Task_Create_Call (Task_Rec : Entity_Id) return Node_Id is
      Loc    : constant Source_Ptr := Sloc (Task_Rec);
      Name   : Node_Id;
      Tdef   : Node_Id;
      Tdec   : Node_Id;
      Ttyp   : Node_Id;
      Tnam   : Name_Id;
      Args   : List_Id;
      Ecount : Node_Id;

   begin
      Ttyp := Corresponding_Concurrent_Type (Task_Rec);
      Tnam := Chars (Ttyp);

      --  Get task declaration. In the case of a task type declaration, this is
      --  simply the parent of the task type entity. In the single task
      --  declaration, this parent will be the implicit type, and we can find
      --  the corresponding single task declaration by searching forward in the
      --  declaration list in the tree.

      --  Is the test for N_Single_Task_Declaration needed here??? Nodes of
      --  this type should have been removed during semantic analysis.

      Tdec := Parent (Ttyp);
      while Nkind (Tdec) /= N_Task_Type_Declaration
        and then Nkind (Tdec) /= N_Single_Task_Declaration
      loop
         Next (Tdec);
      end loop;

      --  Now we can find the task definition from this declaration

      Tdef := Task_Definition (Tdec);

      --  Build the parameter list for the call. Note that _Init is the name
      --  of the formal for the object to be initialized, which is the task
      --  value record itself.

      Args := New_List;

      --  Priority parameter. Set to Unspecified_Priority unless there is a
      --  priority pragma, in which case we take the value from the pragma.

      if Present (Tdef) and then Has_Priority_Pragma (Tdef) then
         Append_To (Args,
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Selector_Name => Make_Identifier (Loc, Name_uPriority)));
      else
         Append_To (Args,
           New_Reference_To (RTE (RE_Unspecified_Priority), Loc));
      end if;

      --  Optional Stack parameter

      if Restricted_Profile then

         --  If the stack has been preallocated by the expander then
         --  pass its address. Otherwise, pass a null address.

         if Preallocated_Stacks_On_Target then
            Append_To (Args,
              Make_Attribute_Reference (Loc,
                Prefix         => Make_Selected_Component (Loc,
                  Prefix        => Make_Identifier (Loc, Name_uInit),
                  Selector_Name =>
                    Make_Identifier (Loc, Name_uStack)),
                Attribute_Name => Name_Address));

         else
            Append_To (Args,
              New_Reference_To (RTE (RE_Null_Address), Loc));
         end if;
      end if;

      --  Size parameter. If no Storage_Size pragma is present, then
      --  the size is taken from the taskZ variable for the type, which
      --  is either Unspecified_Size, or has been reset by the use of
      --  a Storage_Size attribute definition clause. If a pragma is
      --  present, then the size is taken from the _Size field of the
      --  task value record, which was set from the pragma value.

      if Present (Tdef)
        and then Has_Storage_Size_Pragma (Tdef)
      then
         Append_To (Args,
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Selector_Name => Make_Identifier (Loc, Name_uSize)));

      else
         Append_To (Args,
           New_Reference_To (Storage_Size_Variable (Ttyp), Loc));
      end if;

      --  Task_Info parameter. Set to Unspecified_Task_Info unless there is a
      --  Task_Info pragma, in which case we take the value from the pragma.

      if Present (Tdef)
        and then Has_Task_Info_Pragma (Tdef)
      then
         Append_To (Args,
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Selector_Name => Make_Identifier (Loc, Name_uTask_Info)));

      else
         Append_To (Args,
           New_Reference_To (RTE (RE_Unspecified_Task_Info), Loc));
      end if;

      if not Restricted_Profile then

         --  Number of entries. This is an expression of the form:
         --
         --    n + _Init.a'Length + _Init.a'B'Length + ...
         --
         --  where a,b... are the entry family names for the task definition

         Ecount := Build_Entry_Count_Expression (
           Ttyp,
           Component_Items (Component_List (
             Type_Definition (Parent (
               Corresponding_Record_Type (Ttyp))))),
           Loc);
         Append_To (Args, Ecount);

         --  Master parameter. This is a reference to the _Master parameter of
         --  the initialization procedure, except in the case of the pragma
         --  Restrictions (No_Task_Hierarchy) where the value is fixed to 3.
         --  See comments in System.Tasking.Initialization.Init_RTS for the
         --  value 3.

         if Restriction_Active (No_Task_Hierarchy) = False then
            Append_To (Args, Make_Identifier (Loc, Name_uMaster));
         else
            Append_To (Args, Make_Integer_Literal (Loc, 3));
         end if;
      end if;

      --  State parameter. This is a pointer to the task body procedure. The
      --  required value is obtained by taking the address of the task body
      --  procedure and converting it (with an unchecked conversion) to the
      --  type required by the task kernel. For further details, see the
      --  description of Expand_Task_Body

      Append_To (Args,
        Unchecked_Convert_To (RTE (RE_Task_Procedure_Access),
          Make_Attribute_Reference (Loc,
            Prefix =>
              New_Occurrence_Of (Get_Task_Body_Procedure (Ttyp), Loc),
            Attribute_Name => Name_Address)));

      --  Discriminants parameter. This is just the address of the task
      --  value record itself (which contains the discriminant values

      Append_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix => Make_Identifier (Loc, Name_uInit),
          Attribute_Name => Name_Address));

      --  Elaborated parameter. This is an access to the elaboration Boolean

      Append_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix => Make_Identifier (Loc, New_External_Name (Tnam, 'E')),
          Attribute_Name => Name_Unchecked_Access));

      --  Chain parameter. This is a reference to the _Chain parameter of
      --  the initialization procedure.

      Append_To (Args, Make_Identifier (Loc, Name_uChain));

      --  Task name parameter. Take this from the _Task_Id parameter to the
      --  init call unless there is a Task_Name pragma, in which case we take
      --  the value from the pragma.

      if Present (Tdef)
        and then Has_Task_Name_Pragma (Tdef)
      then
         Append_To (Args,
           New_Copy (
             Expression (First (
               Pragma_Argument_Associations (
                 Find_Task_Or_Protected_Pragma
                   (Tdef, Name_Task_Name))))));

      else
         Append_To (Args, Make_Identifier (Loc, Name_uTask_Name));
      end if;

      --  Created_Task parameter. This is the _Task_Id field of the task
      --  record value

      Append_To (Args,
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, Name_uInit),
          Selector_Name => Make_Identifier (Loc, Name_uTask_Id)));

      if Restricted_Profile then
         Name := New_Reference_To (RTE (RE_Create_Restricted_Task), Loc);
      else
         Name := New_Reference_To (RTE (RE_Create_Task), Loc);
      end if;

      return Make_Procedure_Call_Statement (Loc,
        Name => Name, Parameter_Associations => Args);
   end Make_Task_Create_Call;

   ------------------------------
   -- Next_Protected_Operation --
   ------------------------------

   function Next_Protected_Operation (N : Node_Id) return Node_Id is
      Next_Op : Node_Id;

   begin
      Next_Op := Next (N);
      while Present (Next_Op)
        and then Nkind (Next_Op) /= N_Subprogram_Body
        and then Nkind (Next_Op) /= N_Entry_Body
      loop
         Next (Next_Op);
      end loop;

      return Next_Op;
   end Next_Protected_Operation;

   --------------------------
   -- Parameter_Block_Pack --
   --------------------------

   function Parameter_Block_Pack
     (Loc     : Source_Ptr;
      Blk_Typ : Entity_Id;
      Actuals : List_Id;
      Formals : List_Id;
      Decls   : List_Id;
      Stmts   : List_Id) return Node_Id
   is
      Actual    : Entity_Id;
      Expr      : Node_Id := Empty;
      Formal    : Entity_Id;
      Has_Param : Boolean := False;
      P         : Entity_Id;
      Params    : List_Id;
      Temp_Asn  : Node_Id;
      Temp_Nam  : Node_Id;

   begin
      Actual := First (Actuals);
      Formal := Defining_Identifier (First (Formals));
      Params := New_List;

      while Present (Actual) loop
         if Is_By_Copy_Type (Etype (Actual)) then
            --  Generate:
            --    Jnn : aliased <formal-type>

            Temp_Nam :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('J'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Aliased_Present =>
                  True,
                Defining_Identifier =>
                  Temp_Nam,
                Object_Definition =>
                  New_Reference_To (Etype (Formal), Loc)));

            if Ekind (Formal) /= E_Out_Parameter then

               --  Generate:
               --    Jnn := <actual>

               Temp_Asn :=
                 New_Reference_To (Temp_Nam, Loc);

               Set_Assignment_OK (Temp_Asn);

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name =>
                     Temp_Asn,
                   Expression =>
                     New_Copy_Tree (Actual)));
            end if;

            --  Generate:
            --    Jnn'unchecked_access

            Append_To (Params,
              Make_Attribute_Reference (Loc,
                Attribute_Name =>
                  Name_Unchecked_Access,
                Prefix =>
                  New_Reference_To (Temp_Nam, Loc)));

            Has_Param := True;

         --  The controlling parameter is omitted

         else
            if not Is_Controlling_Actual (Actual) then
               Append_To (Params,
                 Make_Reference (Loc, New_Copy_Tree (Actual)));

               Has_Param := True;
            end if;
         end if;

         Next_Actual (Actual);
         Next_Formal_With_Extras (Formal);
      end loop;

      if Has_Param then
         Expr := Make_Aggregate (Loc, Params);
      end if;

      --  Generate:
      --    P : Ann := (
      --      J1'unchecked_access;
      --      <actual2>'reference;
      --      ...);

      P := Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            P,
          Object_Definition =>
            New_Reference_To (Blk_Typ, Loc),
          Expression =>
            Expr));

      return P;
   end Parameter_Block_Pack;

   ----------------------------
   -- Parameter_Block_Unpack --
   ----------------------------

   function Parameter_Block_Unpack
     (Loc     : Source_Ptr;
      P       : Entity_Id;
      Actuals : List_Id;
      Formals : List_Id) return List_Id
   is
      Actual    : Entity_Id;
      Asnmt     : Node_Id;
      Formal    : Entity_Id;
      Has_Asnmt : Boolean := False;
      Result    : constant List_Id := New_List;

   begin
      Actual := First (Actuals);
      Formal := Defining_Identifier (First (Formals));
      while Present (Actual) loop
         if Is_By_Copy_Type (Etype (Actual))
           and then Ekind (Formal) /= E_In_Parameter
         then
            --  Generate:
            --    <actual> := P.<formal>;

            Asnmt :=
              Make_Assignment_Statement (Loc,
                Name =>
                  New_Copy (Actual),
                Expression =>
                  Make_Explicit_Dereference (Loc,
                    Make_Selected_Component (Loc,
                      Prefix =>
                        New_Reference_To (P, Loc),
                      Selector_Name =>
                        Make_Identifier (Loc, Chars (Formal)))));

            Set_Assignment_OK (Name (Asnmt));
            Append_To (Result, Asnmt);

            Has_Asnmt := True;
         end if;

         Next_Actual (Actual);
         Next_Formal_With_Extras (Formal);
      end loop;

      if Has_Asnmt then
         return Result;
      else
         return New_List (Make_Null_Statement (Loc));
      end if;
   end Parameter_Block_Unpack;

   ----------------------
   -- Set_Discriminals --
   ----------------------

   procedure Set_Discriminals (Dec : Node_Id) is
      D       : Entity_Id;
      Pdef    : Entity_Id;
      D_Minal : Entity_Id;

   begin
      pragma Assert (Nkind (Dec) = N_Protected_Type_Declaration);
      Pdef := Defining_Identifier (Dec);

      if Has_Discriminants (Pdef) then
         D := First_Discriminant (Pdef);
         while Present (D) loop
            D_Minal :=
              Make_Defining_Identifier (Sloc (D),
                Chars => New_External_Name (Chars (D), 'D'));

            Set_Ekind (D_Minal, E_Constant);
            Set_Etype (D_Minal, Etype (D));
            Set_Scope (D_Minal, Pdef);
            Set_Discriminal (D, D_Minal);
            Set_Discriminal_Link (D_Minal, D);

            Next_Discriminant (D);
         end loop;
      end if;
   end Set_Discriminals;

   -----------------
   -- Set_Privals --
   -----------------

   procedure Set_Privals
      (Dec           : Node_Id;
       Op            : Node_Id;
       Loc           : Source_Ptr;
       After_Barrier : Boolean := False)
   is
      P_Decl      : Node_Id;
      P_Id        : Entity_Id;
      Priv        : Entity_Id;
      Def         : Node_Id;
      Body_Ent    : Entity_Id;
      For_Barrier : constant Boolean :=
                      Nkind (Op) = N_Entry_Body and then not After_Barrier;

      Prec_Decl : constant Node_Id :=
                    Parent (Corresponding_Record_Type
                             (Defining_Identifier (Dec)));
      Prec_Def  : constant Entity_Id := Type_Definition (Prec_Decl);
      Obj_Decl  : Node_Id;
      P_Subtype : Entity_Id;
      Assoc_L   : constant Elist_Id := New_Elmt_List;
      Op_Id     : Entity_Id;

   begin
      pragma Assert (Nkind (Dec) = N_Protected_Type_Declaration);
      pragma Assert
        (Nkind (Op) = N_Subprogram_Body or else Nkind (Op) = N_Entry_Body);

      Def := Protected_Definition (Dec);

      if Present (Private_Declarations (Def)) then
         P_Decl := First (Private_Declarations (Def));
         while Present (P_Decl) loop
            if Nkind (P_Decl) = N_Component_Declaration then
               P_Id := Defining_Identifier (P_Decl);

               if For_Barrier then
                  Priv :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (P_Id), 'P'));
               else
                  Priv :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (P_Id)));
               end if;

               Set_Ekind     (Priv, E_Variable);
               Set_Etype     (Priv, Etype (P_Id));
               Set_Scope     (Priv, Scope (P_Id));
               Set_Esize     (Priv, Esize (Etype (P_Id)));
               Set_Alignment (Priv, Alignment (Etype (P_Id)));

               --  If the type of the component is an itype, we must create a
               --  new itype for the corresponding prival in each protected
               --  operation, to avoid scoping problems. We create new itypes
               --  by copying the tree for the component definition.

               if Is_Itype (Etype (P_Id)) then
                  Append_Elmt (P_Id, Assoc_L);
                  Append_Elmt (Priv, Assoc_L);

                  if Nkind (Op) = N_Entry_Body then
                     Op_Id := Defining_Identifier (Op);
                  else
                     Op_Id := Defining_Unit_Name (Specification (Op));
                  end if;

                  Discard_Node
                    (New_Copy_Tree (P_Decl, Assoc_L, New_Scope => Op_Id));
               end if;

               Set_Protected_Operation (P_Id, Op);
               Set_Prival (P_Id, Priv);
            end if;

            Next (P_Decl);
         end loop;
      end if;

      --  There is one more implicit private decl: the object itself. "prival"
      --  for this is attached to the protected body defining identifier.

      Body_Ent := Corresponding_Body (Dec);

      Priv :=
        Make_Defining_Identifier (Sloc (Body_Ent),
          Chars => New_External_Name (Chars (Body_Ent), 'R'));

      --  Set the Etype to the implicit subtype of Protection created when
      --  the protected type declaration was expanded. This node will not
      --  be analyzed until it is used as the defining identifier for the
      --  renaming declaration in the protected operation body, and it will
      --  be needed in the references expanded before that body is expanded.
      --  Since the Protection field is aliased, set Is_Aliased as well.

      Obj_Decl := First (Component_Items (Component_List (Prec_Def)));
      while Chars (Defining_Identifier (Obj_Decl)) /= Name_uObject loop
         Next (Obj_Decl);
      end loop;

      P_Subtype  := Etype (Defining_Identifier (Obj_Decl));
      Set_Ekind (Priv, E_Variable);
      Set_Etype (Priv, P_Subtype);
      Set_Is_Aliased (Priv);
      Set_Object_Ref (Body_Ent, Priv);
   end Set_Privals;

   ----------------------------
   -- Update_Prival_Subtypes --
   ----------------------------

   procedure Update_Prival_Subtypes (N : Node_Id) is

      function Process (N : Node_Id) return Traverse_Result;
      --  Update the etype of occurrences of privals whose etype does not
      --  match the current Etype of the prival entity itself.

      procedure Update_Array_Bounds (E : Entity_Id);
      --  Itypes generated for array expressions may depend on the
      --  determinants of the protected object, and need to be processed
      --  separately because they are not attached to the tree.

      procedure Update_Index_Types (N : Node_Id);
      --  Similarly, update the types of expressions in indexed components
      --  which may depend on other discriminants.

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         if Is_Entity_Name (N)  then
            declare
               E : constant Entity_Id := Entity (N);
            begin
               if Present (E)
                 and then (Ekind (E) = E_Constant
                            or else Ekind (E) = E_Variable)
                 and then Nkind (Parent (E)) = N_Object_Renaming_Declaration
                 and then not Is_Scalar_Type (Etype (E))
                 and then Etype (N) /= Etype (E)
               then
                  Set_Etype (N, Etype (Entity (Original_Node (N))));
                  Update_Index_Types (N);

               elsif Present (E)
                 and then Ekind (E) = E_Constant
                 and then Present (Discriminal_Link (E))
               then
                  Set_Etype (N, Etype (E));
               end if;
            end;

            return OK;

         elsif Nkind (N) = N_Defining_Identifier
           or else Nkind (N) = N_Defining_Operator_Symbol
           or else Nkind (N) = N_Defining_Character_Literal
         then
            return Skip;

         elsif Nkind (N) = N_String_Literal then

            --  Array type, but bounds are constant

            return OK;

         elsif Nkind (N) = N_Object_Declaration
           and then Is_Itype (Etype (Defining_Identifier (N)))
           and then Is_Array_Type (Etype (Defining_Identifier (N)))
         then
            Update_Array_Bounds (Etype (Defining_Identifier (N)));
            return OK;

         --  For array components of discriminated records, use the base type
         --  directly, because it may depend indirectly on the discriminants of
         --  the protected type.

         --  Cleaner would be a systematic mechanism to compute actual subtypes
         --  of private components???

         elsif Nkind (N) in N_Has_Etype
           and then Present (Etype (N))
           and then Is_Array_Type (Etype (N))
           and then Nkind (N) = N_Selected_Component
           and then Has_Discriminants (Etype (Prefix (N)))
         then
            Set_Etype (N, Base_Type (Etype (N)));
            Update_Index_Types (N);
            return OK;

         else
            if Nkind (N) in N_Has_Etype
              and then Present (Etype (N))
              and then Is_Itype (Etype (N)) then

               if Is_Array_Type (Etype (N)) then
                  Update_Array_Bounds (Etype (N));

               elsif Is_Scalar_Type (Etype (N)) then
                  Update_Prival_Subtypes (Type_Low_Bound  (Etype (N)));
                  Update_Prival_Subtypes (Type_High_Bound (Etype (N)));
               end if;
            end if;

            return OK;
         end if;
      end Process;

      -------------------------
      -- Update_Array_Bounds --
      -------------------------

      procedure Update_Array_Bounds (E : Entity_Id) is
         Ind : Node_Id;
      begin
         Ind := First_Index (E);
         while Present (Ind) loop
            Update_Prival_Subtypes (Type_Low_Bound  (Etype (Ind)));
            Update_Prival_Subtypes (Type_High_Bound (Etype (Ind)));
            Next_Index (Ind);
         end loop;
      end Update_Array_Bounds;

      ------------------------
      -- Update_Index_Types --
      ------------------------

      procedure Update_Index_Types (N : Node_Id) is
         Indx1 : Node_Id;
         I_Typ : Node_Id;

      begin
         --  If the prefix has an actual subtype that is different from the
         --  nominal one, update the types of the indices, so that the proper
         --  constraints are applied. Do not apply this transformation to a
         --  packed array, where the index type is computed for a byte array
         --  and is different from the source index.

         if Nkind (Parent (N)) = N_Indexed_Component
           and then
             not Is_Bit_Packed_Array (Etype (Prefix (Parent (N))))
         then
            Indx1 := First (Expressions (Parent (N)));
            I_Typ := First_Index (Etype (N));

            while Present (Indx1) and then Present (I_Typ) loop

               if not Is_Entity_Name (Indx1) then
                  Set_Etype (Indx1, Base_Type (Etype (I_Typ)));
               end if;

               Next (Indx1);
               Next_Index (I_Typ);
            end loop;
         end if;
      end Update_Index_Types;

      procedure Traverse is new Traverse_Proc;

   --  Start of processing for Update_Prival_Subtypes

   begin
      Traverse (N);
   end Update_Prival_Subtypes;

end Exp_Ch9;
