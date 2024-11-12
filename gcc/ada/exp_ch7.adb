------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 7                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  This package contains virtually all expansion mechanisms related to
--    - controlled types
--    - transient scopes

with Aspects;        use Aspects;
with Atree;          use Atree;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Ch6;        use Exp_Ch6;
with Exp_Ch9;        use Exp_Ch9;
with Exp_Ch11;       use Exp_Ch11;
with Exp_Dbug;       use Exp_Dbug;
with Exp_Dist;       use Exp_Dist;
with Exp_Disp;       use Exp_Disp;
with Exp_Prag;       use Exp_Prag;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with GNAT_CUDA;      use GNAT_CUDA;
with Inline;         use Inline;
with Lib;            use Lib;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Output;         use Output;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch7;        use Sem_Ch7;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;

package body Exp_Ch7 is

   -----------------------------
   -- Finalization Management --
   -----------------------------

   --  This paragraph describes how Initialization/Adjustment/Finalization
   --  procedures are generated and called. Two cases must be considered: types
   --  that are controlled (Is_Controlled flag set) and composite types that
   --  contain controlled components (Has_Controlled_Component flag set). In
   --  the first case the procedures to call are the user-defined primitive
   --  operations Initialize/Adjust/Finalize. In the second case, the compiler
   --  generates Deep_Initialize, Deep_Adjust and Deep_Finalize that are in
   --  charge of calling the former procedures on the controlled components.

   --  Initialize calls: they are generated for either declarations or dynamic
   --  allocations of controlled objects with no initial value. They are always
   --  followed by an attachment to some finalization chain. For the dynamic
   --  dynamic allocation case, this is the collection attached to the access
   --  type definition; otherwise, this is the master of the current scope.

   --  Adjust calls: they are generated on two occasions: (1) for declarations
   --  or dynamic allocations of controlled objects with an initial value (with
   --  the exception of function calls), (2) after an assignment. In the first
   --  case they are followed by an attachment to the finalization chain, in
   --  the second case they are not.

   --  Finalization calls: they are generated on three occasions: (1) on scope
   --  exit, (2) assignments, (3) unchecked deallocations. In case (3) objects
   --  have to be detached from the finalization chain, in case (2) they must
   --  not and in case (1) this is optional as we are exiting the scope anyway.

   --  There are two kinds of finalization chain to which objects are attached,
   --  depending on the way they are created. For objects (statically) declared
   --  in a scope, the finalization chain is that of the master of the scope,
   --  which is embodied in a Finalization_Master object. As per RM 7.6.1(11/3)
   --  the finalization of the master (on scope exit) performs the finalization
   --  of objects attached to its chain in the reverse order of their creation.

   --  For dynamically allocated objects, the finalization chain is that of the
   --  finalization collection of the access type through which the objects are
   --  allocated, which is embodied in a Finalization_Collection object. As per
   --  RM 7.6.1(11.1/3), the finalization of the collection performs the
   --  finalization of objects attached to its chain in an arbitrary order.

   --  A Finalization_Collection object is implemented as a controlled object
   --  and its finalization is therefore driven by the finalization master of
   --  the scope where it is declared. As per RM 7.6.1(11.2/3), for a named
   --  access type, the Finalization_Collection object is declared in the list
   --  of actions of its freeze node.

   --  ??? For an anonymous access type, the implementation deviates from the
   --  RM 7.6.1 clause as follows: all the anonymous access types with the same
   --  designated type that are (implicitly) declared in a library unit share a
   --  single Finalization_Collection object declared in the outermost scope of
   --  the library unit, except if the designated type is declared in a dynamic
   --  scope nested in the unit; in this case no Finalization_Collection object
   --  is created. As a result, in the first case, objects allocated through
   --  the anonymous access types are finalized when the library unit goes out
   --  of scope, while in the second case, they are not finalized at all.

   --  Here is a simple example of the expansion of a controlled block:

   --    declare
   --       X : Ctrl;
   --       Y : Ctrl := Init;

   --       type Rec is record
   --          C : Ctrl;
   --       end record;

   --       W : Rec;
   --       Z : Rec := Init;

   --    begin
   --       X := Y;
   --       W := Z;
   --    end;
   --
   --  is expanded into:
   --
   --    declare
   --       Mnn : System.Finalization_Primitives.Finalization_Master;

   --       XMN : aliased System.Finalization_Primitives.Master_Node;
   --       X : Ctrl;
   --       Bnn : begin
   --          Abort_Defer;
   --          Initialize (X);
   --          System.Finalization_Primitives.Attach_To_Master
   --            (X'address,
   --             CtrlFD'unrestricted_access,
   --             XMN'unrestricted_access,
   --             Mnn);
   --       at end
   --          Abort_Undefer;
   --       end Bnn;

   --       YMN : aliased System.Finalization_Primitives.Master_Node;
   --       Y : Ctrl := Init;
   --       System.Finalization_Primitives.Attach_To_Master
   --         (Y'address,
   --          CtrlFD'unrestricted_access,
   --          YMN'unrestricted_access,
   --          Mnn);

   --       type Rec is record
   --          C : Ctrl;
   --       end record;

   --       WMN : aliased System.Finalization_Primitives.Master_Node;
   --       W : Rec;
   --       Bnn : begin
   --          Abort_Defer;
   --          Bnn : begin
   --             Deep_Initialize (W);
   --             System.Finalization_Primitives.Attach_To_Master
   --               (W'address,
   --                Rec_FD'unrestricted_access,
   --                WMN'unrestricted_access,
   --                Mnn);
   --          exception
   --             when others =>
   --                Deep_Finalize (W);
   --          end Bnn;
   --       at end
   --          Abort_Undefer;
   --       end Bnn;

   --       ZMN : aliaed System.Finalization_Primitives.Master_Node;
   --       Z : Rec := Init;
   --       System.Finalization_Primitives.Attach_To_Master
   --         (Z'address,
   --          Rec_FD'unrestricted_access,
   --          ZMN'unrestricted_access,
   --          Mnn);

   --       procedure _Finalizer is
   --          Ann : constant Boolean := Ada.Exceptions.Triggered_By_Abort;
   --          Rnn : boolean := False;
   --       begin
   --          Abort_Defer;
   --          Bnn : begin
   --             System.Finalization_Primitives.Finalize_Master (Mnn);
   --          exceptions
   --             when others =>
   --                Rnn := True;
   --          end Bnn;
   --          Abort_Undefer;
   --          if Rnn and then not Ann then
   --             [program_error "finalize raised exception"]
   --          end if;
   --       end _Finalizer;

   --    begin
   --       _Assign (X, Y);
   --       Deep_Finalize (W);
   --       W := Z;
   --       Deep_Adjust (W);
   --    end;
   --    at end
   --       _Finalizer;

   --  In the case of a block containing a single controlled object, the master
   --  degenerates into a single master node:

   --    declare
   --       X : Ctrl := Init;

   --    begin
   --       null;
   --    end;

   --  is expanded into:

   --    declare
   --       XMN : aliased System.Finalization_Primitives.Master_Node;
   --       X : Ctrl := Init;
   --       System.Finalization_Primitives.Attach_To_Node
   --         (X'address,
   --          CtrlFD'unrestricted_access,
   --          XMN'unrestricted_access);

   --       procedure _Finalizer is
   --          Ann : constant Boolean := Ada.Exceptions.Triggered_By_Abort;
   --          Rnn : boolean := False;
   --       begin
   --          Abort_Defer;
   --          Bnn : begin
   --              System.Finalization_Primitives.Finalize_Object (XMN);
   --          exceptions
   --             when others =>
   --                Rnn := True;
   --          end Bnn;
   --          Abort_Undefer;
   --          if Rnn and then not Ann then
   --             [program_error "finalize raised exception"]
   --          end if;
   --       end _Finalizer;

   --    begin
   --       null;
   --    end;
   --    at end
   --       _Finalizer;

   --  Here is the version with a dynamically allocated object:

   --    declare
   --       X : P_Ctrl := new Ctrl;

   --    begin
   --       null;
   --    end;
   --
   --  is expanded into:

   --    declare
   --       Cnn : System.Finalization_Primitives.Finalization_Collection_Ptr :=
   --               P_CtrlFC'unrestricted_access;
   --       [...]
   --       Pnn : constant P_Ctrl := new Ctrl[...][...];
   --       Bnn : begin
   --          Abort_Defer;
   --          Initialize (Pnn.all);
   --          System.Finalization_Primitives.Attach_To_Collection
   --            (Pnn.all'address,
   --             CtrlFD'unrestricted_access,
   --             Cnn.all);
   --       at end
   --          Abort_Undefer;
   --       end Bnn;
   --       X : P_Ctrl := Pnn;

   --  The implementation uses two different strategies for the finalization
   --  of (statically) declared objects and of dynamically allocated objects.

   --  For (statically) declared objects, the attachment to the finalization
   --  chain of the current scope and the call to the finalization procedure
   --  are generated during a post-processing phase of the expansion. These
   --  objects are first spotted in declarative parts and statement lists by
   --  Requires_Cleanup_Actions; then Build_Finalizer is called on the parent
   --  node to generate both the attachment and the finalization actions.

   --  This post processing is fully transparent for the rest of the expansion
   --  activities, in other words those have nothing to do or to care about.
   --  However this default processing may not be sufficient in specific cases,
   --  e.g. for the return object of an extended return statement in a function
   --  whose result type is controlled: in this case, the return object must be
   --  finalized only if the function returns abnormally. In order to deal with
   --  these cases, it is possible to directly generate detachment actions (for
   --  the return object case) or finalization actions (for transient objects)
   --  during the rest of expansion activities.

   --  These direct actions must be signalled to the post-processing machinery
   --  and this is achieved through the handling of Master_Node objects, which
   --  are the items actually chained in the finalization chains of masters.
   --  With the default processing, they are created by Build_Finalizer for the
   --  controlled objects spotted by Requires_Cleanup_Actions. But when direct
   --  actions are carried out, they are generated by these actions and later
   --  recognized by Requires_Cleanup_Actions and picked up by Build_Finalizer.

   --  For dynamically allocated objects, there is no post-processing phase and
   --  the attachment to the finalization chain of the access type, as well the
   --  the detachment from this chain for unchecked deallocation, are generated
   --  directly by the compiler during the expansion of allocators and calls to
   --  instances of the Unchecked_Deallocation procedure.

   --------------------------
   -- Relaxed Finalization --
   --------------------------

   --  This paragraph describes the differences between the implementation of
   --  finalization as specified by the Ada RM (called "strict" and documented
   --  in the previous paragraph) and that of finalization as specified by the
   --  GNAT RM (called "relaxed") for a second category of controlled objects.

   --  For objects (statically) declared in a scope, the default implementation
   --  documented in the previous paragraph is used for the scope as a whole as
   --  soon as one controlled object with strict finalization is present in it,
   --  including one transient controlled object. Otherwise, that is to say, if
   --  all the controlled objects in the scope have relaxed finalization, then
   --  no Finalization_Master is built for this scope, and all the objects are
   --  finalized explicitly in the reverse order of their creation:

   --    declare
   --       X : Ctrl := Init;
   --       Y : Ctrl := Init;

   --    begin
   --       null;
   --    end;

   --  is expanded into:

   --    declare
   --       XMN : aliased System.Finalization_Primitives.Master_Node;
   --       X : Ctrl := Init;
   --       System.Finalization_Primitives.Attach_To_Node
   --         (X'address,
   --          CtrlFD'unrestricted_access,
   --          XMN'unrestricted_access);
   --       YMN : aliased System.Finalization_Primitives.Master_Node;
   --       Y : Ctrl := Init;
   --       System.Finalization_Primitives.Attach_To_Node
   --         (Y'address,
   --          CtrlFD'unrestricted_access,
   --          YMN'unrestricted_access);

   --       procedure _Finalizer is
   --       begin
   --          Abort_Defer;
   --          System.Finalization_Primitives.Finalize_Object (YMN);
   --          System.Finalization_Primitives.Finalize_Object (XMN);
   --          Abort_Undefer;
   --       end _Finalizer;

   --    begin
   --       null;
   --    end;
   --    at end
   --       _Finalizer;

   --  Dynamically allocated objects with relaxed finalization need not be
   --  finalized and, therefore, are not attached to any finalization chain.

   type Final_Primitives is
     (Initialize_Case, Adjust_Case, Finalize_Case, Address_Case);
   --  This enumeration type is defined in order to ease sharing code for
   --  building finalization procedures for composite types.

   Name_Of      : constant array (Final_Primitives) of Name_Id :=
                    (Initialize_Case => Name_Initialize,
                     Adjust_Case     => Name_Adjust,
                     Finalize_Case   => Name_Finalize,
                     Address_Case    => Name_Finalize_Address);
   Deep_Name_Of : constant array (Final_Primitives) of TSS_Name_Type :=
                    (Initialize_Case => TSS_Deep_Initialize,
                     Adjust_Case     => TSS_Deep_Adjust,
                     Finalize_Case   => TSS_Deep_Finalize,
                     Address_Case    => TSS_Finalize_Address);

   function Allows_Finalization_Collection (Typ : Entity_Id) return Boolean;
   --  Determine whether access type Typ may have a finalization collection

   procedure Build_Array_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Controlled_Component set and store them using the TSS mechanism.

   function Build_Cleanup_Statements
     (N                  : Node_Id;
      Additional_Cleanup : List_Id) return List_Id;
   --  Create the cleanup calls for an asynchronous call block, task master,
   --  protected subprogram body, task allocation block or task body, or
   --  additional cleanup actions parked on a transient block. If the context
   --  does not contain the above constructs, the routine returns an empty
   --  list.

   procedure Build_Finalizer_Call (N : Node_Id; Fin_Id : Entity_Id);
   --  N is a construct that contains a handled sequence of statements, Fin_Id
   --  is the entity of a finalizer. Create an At_End handler that covers the
   --  statements of N and calls Fin_Id. If the handled statement sequence has
   --  an exception handler, the statements will be wrapped in a block to avoid
   --  unwanted interaction with the new At_End handler.

   procedure Build_Record_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Component_Component set and store them using the TSS mechanism.

   --------------------------------
   -- Transient Scope Management --
   --------------------------------

   --  A transient scope is needed when certain temporary objects are created
   --  by the compiler. These temporary objects are allocated on the secondary
   --  stack and/or need finalization, and the transient scope is responsible
   --  for finalizing the objects and reclaiming the memory of the secondary
   --  stack at the appropriate time. They are generally objects allocated to
   --  store the result of a function returning an unconstrained or controlled
   --  value. Expressions needing to be wrapped in a transient scope may appear
   --  in three different contexts, which lead to different kinds of transient
   --  scope expansion:

   --   1. In a simple statement (procedure call, assignment, ...). In this
   --      case the statement is wrapped into a transient block, which takes
   --      care of the finalization actions as well as the secondary stack
   --      deallocation, See Wrap_Transient_Statement for details.

   --   2. In an expression of a control structure (test in a If statement,
   --      expression in a Case statement, ...). In this case the expression
   --      is replaced by a temporary and the enclosing statement is wrapped
   --      into a transient block, which takes care of the finalization actions
   --      and the secondary stack deallocation. See Wrap_Transient_Expression
   --      for details.

   --   3. In an expression of an object declaration. No wrapping is possible
   --      here, so the finalization actions performed on the normal path, if
   --      any, are done right after the declaration, and those performed on
   --      the exceptional path, as well as the secondary stack deallocation,
   --      are deferred to the enclosing scope. See Wrap_Transient_Declaration
   --      for details.

   --  A transient scope is created by calling Establish_Transient_Scope on the
   --  node that needs to be serviced by it (the serviced node can subsequently
   --  be retrieved by invoking Node_To_Be_Wrapped when the current scope is a
   --  transient scope). Once this has been done, the normal processing of the
   --  Insert_Actions procedures is blocked and the procedures are redirected
   --  to the Store_xxx_Actions_In_Scope procedures and Store_Actions_In_Scope
   --  is ultimately invoked to store the pending actions.

   --  A transient scope is finalized by calling one of the Wrap_Transient_xxx
   --  procedures depending on the context as explained above. They ultimately
   --  invoke Insert_Actions_In_Scope_Around as per the following picture:

   --            Wrap_Transient_Expression          Wrap_Transient_Statement
   --                                  |              |
   --                                  V              V
   --                                Make_Transient_Block
   --                                        |
   --   Wrap_Transient_Declaration           |
   --                          |             |
   --                          V             V
   --                       Insert_Actions_In_Scope_Around

   procedure Insert_Actions_In_Scope_Around
     (N         : Node_Id;
      Clean     : Boolean;
      Manage_SS : Boolean);
   --  Insert the before-actions kept in the scope stack before N, and the
   --  after-actions after N, which must be a member of a list. If Clean is
   --  true, insert any cleanup actions kept in the scope stack and generate
   --  required finalization actions for the before-actions and after-actions.
   --  If Manage_SS is true, insert calls to mark/release the secondary stack.

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id;
      Par    : Node_Id) return Node_Id;
   --  Action is a single statement or object declaration. Par is the proper
   --  parent of the generated block. Create a transient block whose name is
   --  the current scope and the only handled statement is Action. If Action
   --  involves controlled objects or secondary stack usage, the corresponding
   --  cleanup actions are performed at the end of the block.

   procedure Store_Actions_In_Scope (AK : Scope_Action_Kind; L : List_Id);
   --  Shared processing for Store_xxx_Actions_In_Scope

   -------------------------------------------
   -- Unnesting procedures for CCG and LLVM --
   -------------------------------------------

   --  Expansion generates subprograms for controlled types management that
   --  may appear in declarative lists in package declarations and bodies.
   --  These subprograms appear within generated blocks that contain local
   --  declarations and a call to finalization procedures. To ensure that
   --  such subprograms get activation records when needed, we transform the
   --  block into a procedure body, followed by a call to it in the same
   --  declarative list.

   procedure Check_Unnesting_Elaboration_Code (N : Node_Id);
   --  The statement part of a package body that is a compilation unit may
   --  contain blocks that declare local subprograms. In Subprogram_Unnesting_
   --  Mode such subprograms must be handled as nested inside the (implicit)
   --  elaboration procedure that executes that statement part. To handle
   --  properly uplevel references we construct that subprogram explicitly,
   --  to contain blocks and inner subprograms, the statement part becomes
   --  a call to this subprogram. This is only done if blocks are present
   --  in the statement list of the body. (It would be nice to unify this
   --  procedure with Check_Unnesting_In_Decls_Or_Stmts, if possible, since
   --  they're doing very similar work, but are structured differently. ???)

   procedure Check_Unnesting_In_Decls_Or_Stmts (Decls_Or_Stmts : List_Id);
   --  Similarly, the declarations or statements in library-level packages may
   --  have created blocks with nested subprograms. Such a block must be
   --  transformed into a procedure followed by a call to it, so that unnesting
   --  can handle uplevel references within these nested subprograms (typically
   --  subprograms that handle finalization actions). This also applies to
   --  nested packages, including instantiations, in which case it must
   --  recursively process inner bodies.

   procedure Check_Unnesting_In_Handlers (N : Node_Id);
   --  Similarly, check for blocks with nested subprograms occurring within
   --  a set of exception handlers associated with a package body N.

   procedure Unnest_Block (Decl : Node_Id);
   --  Blocks that contain nested subprograms with up-level references need to
   --  create activation records for them. We do this by rewriting the block as
   --  a procedure, followed by a call to it in the same declarative list, to
   --  replicate the semantics of the original block.
   --
   --  A common source for such block is a transient block created for a
   --  construct (declaration, assignment, etc.) that involves controlled
   --  actions or secondary-stack management, in which case the nested
   --  subprogram is a finalizer.

   procedure Unnest_If_Statement (If_Stmt : Node_Id);
   --  The separate statement lists associated with an if-statement (then part,
   --  elsif parts, else part) may require unnesting if they directly contain
   --  a subprogram body that references up-level objects. Each statement list
   --  is traversed to locate such subprogram bodies, and if a part's statement
   --  list contains a body, then the list is replaced with a new procedure
   --  containing the part's statements followed by a call to the procedure.
   --  Furthermore, any nested blocks, loops, or if statements will also be
   --  traversed to determine the need for further unnesting transformations.

   procedure Unnest_Statement_List (Stmts : in out List_Id);
   --  A list of statements that directly contains a subprogram at its outer
   --  level, that may reference objects declared in that same statement list,
   --  is rewritten as a procedure containing the statement list Stmts (which
   --  includes any such objects as well as the nested subprogram), followed by
   --  a call to the new procedure, and Stmts becomes the list containing the
   --  procedure and the call. This ensures that Unnest_Subprogram will later
   --  properly handle up-level references from the nested subprogram to
   --  objects declared earlier in statement list, by creating an activation
   --  record and passing it to the nested subprogram. This procedure also
   --  resets the Scope of objects declared in the statement list, as well as
   --  the Scope of the nested subprogram, to refer to the new procedure.
   --  Also, the new procedure is marked Has_Nested_Subprogram, so this should
   --  only be called when known that the statement list contains a subprogram.

   procedure Unnest_Loop (Loop_Stmt : Node_Id);
   --  Top-level Loops that contain nested subprograms with up-level references
   --  need to have activation records. We do this by rewriting the loop as a
   --  procedure containing the loop, followed by a call to the procedure in
   --  the same library-level declarative list, to replicate the semantics of
   --  the original loop. Such loops can occur due to aggregate expansions and
   --  other constructs.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Visibly_Controlled
     (Prim : Final_Primitives;
      Typ  : Entity_Id;
      E    : in out Entity_Id;
      Cref : in out Node_Id);
   --  The controlled operation declared for a derived type may not be
   --  overriding, if the controlled operations of the parent type are hidden,
   --  for example when the parent is a private type whose full view is
   --  controlled. For other primitive operations we modify the name of the
   --  operation to indicate that it is not overriding, but this is not
   --  possible for Initialize, etc. because they have to be retrievable by
   --  name. Before generating the proper call to one of these operations we
   --  check whether Typ is known to be controlled at the point of definition.
   --  If it is not then we must retrieve the hidden operation of the parent
   --  and use it instead. This is one case that might be solved more cleanly
   --  once Overriding pragmas or declarations are in place.

   function Contains_Subprogram (Blk : Entity_Id) return Boolean;
   --  Check recursively whether a loop or block contains a subprogram that
   --  may need an activation record.

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Typ  : Entity_Id) return Node_Id;
   --  Proc is one of the Initialize/Adjust/Finalize operations, Arg is the one
   --  argument being passed to it, and Typ is its expected type. This function
   --  will, if necessary, generate a conversion between the partial and full
   --  views of Arg to match the type of the formal of Proc, or else force a
   --  conversion to the class-wide type in the case where the operation is
   --  abstract.

   function Finalize_Address_For_Node (Node : Entity_Id) return Entity_Id
     renames Einfo.Entities.Finalization_Master_Node;
   --  Return the Finalize_Address primitive for the object that has been
   --  attached to a finalization Master_Node.

   function Make_Call
     (Loc       : Source_Ptr;
      Proc_Id   : Entity_Id;
      Param     : Node_Id;
      Skip_Self : Boolean := False) return Node_Id;
   --  Subsidiary to Make_Adjust_Call and Make_Final_Call. Given the entity of
   --  routine [Deep_]Adjust or [Deep_]Finalize and an object parameter, create
   --  an adjust or finalization call. When flag Skip_Self is set, the related
   --  action has an effect on the components only (if any).

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id) return Entity_Id;
   --  This function generates the tree for Deep_Initialize, Deep_Adjust or
   --  Deep_Finalize procedures according to the first parameter. These
   --  procedures operate on the type Typ. The Stmts parameter gives the
   --  body of the procedure.

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures according to
   --  the first parameter, these procedures operate on the array type Typ.

   function Make_Deep_Record_Body
     (Prim     : Final_Primitives;
      Typ      : Entity_Id;
      Is_Local : Boolean := False) return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures according to
   --  the first parameter, these procedures operate on the record type Typ.
   --  Flag Is_Local is used in conjunction with Deep_Finalize to designate
   --  whether the inner logic should be dictated by state counters.

   function Make_Finalize_Address_Stmts (Typ : Entity_Id) return List_Id;
   --  Subsidiary to Make_Finalize_Address_Body, Make_Deep_Array_Body and
   --  Make_Deep_Record_Body. Generate the following statements:
   --
   --    declare
   --       type Acc_Typ is access all Typ;
   --       for Acc_Typ'Storage_Size use 0;
   --    begin
   --       [Deep_]Finalize (Acc_Typ (V).all);
   --    end;

   procedure Set_Finalize_Address_For_Node (Node, Fin_Id : Entity_Id)
     renames Einfo.Entities.Set_Finalization_Master_Node;
   --  Set the Finalize_Address primitive for the object that has been
   --  attached to a finalization Master_Node.

   ----------------------------------
   -- Attach_Object_To_Master_Node --
   ----------------------------------

   procedure Attach_Object_To_Master_Node
     (Obj_Decl    : Node_Id;
      Master_Node : Entity_Id)
   is
      Loc     : constant Source_Ptr := Sloc (Obj_Decl);
      Obj_Id  : constant Entity_Id  := Defining_Entity (Obj_Decl);
      Func_Id : constant Entity_Id  :=
                  (if Is_Return_Object (Obj_Id)
                   then Return_Applies_To (Scope (Obj_Id))
                   else Empty);

      function Build_BIP_Cleanup_Stmts
         (Func_Id  : Entity_Id;
          Obj_Addr : Node_Id) return Node_Id;
      --  Func_Id denotes a build-in-place function. Generate the following
      --  cleanup code:
      --
      --    if BIPallocform > Secondary_Stack'Pos
      --      and then BIPcollection /= null
      --    then
      --       declare
      --          type Ptr_Typ is access Fun_Typ;
      --          for Ptr_Typ'Storage_Pool use BIPstoragepool.all;
      --
      --       begin
      --          Free (Ptr_Typ (Obj_Addr));
      --       end;
      --    end if;
      --
      --  Fun_Typ is the return type of the Func_Id.

      -----------------------------
      -- Build_BIP_Cleanup_Stmts --
      -----------------------------

      function Build_BIP_Cleanup_Stmts
        (Func_Id  : Entity_Id;
         Obj_Addr : Node_Id) return Node_Id
      is
         Alloc_Id    : constant Entity_Id :=
           Build_In_Place_Formal (Func_Id, BIP_Alloc_Form);
         Decls       : constant List_Id := New_List;
         Fin_Coll_Id : constant Entity_Id :=
           Build_In_Place_Formal (Func_Id, BIP_Collection);
         Func_Typ    : constant Entity_Id := Etype (Func_Id);

         Cond      : Node_Id;
         Free_Blk  : Node_Id;
         Free_Stmt : Node_Id;
         Pool_Id   : Entity_Id;
         Ptr_Typ   : Entity_Id;

      begin
         --  Generate:
         --    Pool_Id renames BIPstoragepool.all;

         --  This formal is not added on ZFP as those targets do not
         --  support pools.

         if RTE_Available (RE_Root_Storage_Pool_Ptr) then
            Pool_Id := Make_Temporary (Loc, 'P');

            Append_To (Decls,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Pool_Id,
                Subtype_Mark        =>
                  New_Occurrence_Of (RTE (RE_Root_Storage_Pool), Loc),
                Name                =>
                  Make_Explicit_Dereference (Loc,
                    New_Occurrence_Of
                      (Build_In_Place_Formal
                         (Func_Id, BIP_Storage_Pool), Loc))));

            if Debug_Generated_Code then
               Set_Debug_Info_Needed (Pool_Id);
            end if;

         else
            Pool_Id := Empty;
         end if;

         --  Create an access type which uses the storage pool of the caller

         --  Generate:
         --    type Ptr_Typ is access Func_Typ;

         Ptr_Typ := Make_Temporary (Loc, 'P');

         Append_To (Decls,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Ptr_Typ,
             Type_Definition     =>
               Make_Access_To_Object_Definition (Loc,
                 Subtype_Indication => New_Occurrence_Of (Func_Typ, Loc))));

         --  Perform minor decoration in order to set the collection and the
         --  storage pool attributes.

         Mutate_Ekind (Ptr_Typ, E_Access_Type);
         Set_Finalization_Collection (Ptr_Typ, Fin_Coll_Id);
         Set_Associated_Storage_Pool (Ptr_Typ, Pool_Id);

         --  Create an explicit free statement. Note that the free uses the
         --  caller's pool expressed as a renaming.

         Free_Stmt :=
           Make_Free_Statement (Loc,
             Expression =>
               Unchecked_Convert_To (Ptr_Typ, Obj_Addr));

         Set_Storage_Pool (Free_Stmt, Pool_Id);

         --  Create a block to house the dummy type and the instantiation as
         --  well as to perform the cleanup the temporary.

         --  Generate:
         --    declare
         --       <Decls>
         --    begin
         --       Free (Ptr_Typ (Obj_Addr));
         --    end;

         Free_Blk :=
           Make_Block_Statement (Loc,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Free_Stmt)));

         --  Generate:
         --    if BIPallocform > Secondary_Stack'Pos
         --      and then BIPcollection /= null
         --    then

         Cond :=
           Make_And_Then (Loc,
             Left_Opnd  =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => New_Occurrence_Of (Alloc_Id, Loc),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc,
                     UI_From_Int (BIP_Allocation_Form'Pos (Secondary_Stack)))),
             Right_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => New_Occurrence_Of (Fin_Coll_Id, Loc),
                 Right_Opnd => Make_Null (Loc)));

         --  Generate:
         --    if <Cond> then
         --       <Free_Blk>
         --    end if;

         return
           Make_If_Statement (Loc,
             Condition       => Cond,
             Then_Statements => New_List (Free_Blk));
      end Build_BIP_Cleanup_Stmts;

      --  Local variables

      Fin_Id             : Entity_Id;
      Master_Node_Attach : Node_Id;
      Master_Node_Ins    : Node_Id;
      Obj_Ref            : Node_Id;
      Obj_Typ            : Entity_Id;

      --  Start of processing for Attach_Object_To_Master_Node

   begin
      --  Finalize_Address is not generated in CodePeer mode because the
      --  body contains address arithmetic. So we don't want to generate
      --  the attach in this case.

      if CodePeer_Mode then
         return;
      end if;

      --  When the object is initialized by an aggregate, the attachment must
      --  occur after the last aggregate assignment takes place; only then is
      --  the object considered initialized. Likewise if it is initialized by
      --  a build-in-place call: we must attach only after the call.

      if Ekind (Obj_Id) in E_Constant | E_Variable then
         if Present (Last_Aggregate_Assignment (Obj_Id)) then
            Master_Node_Ins := Last_Aggregate_Assignment (Obj_Id);
         elsif Present (BIP_Initialization_Call (Obj_Id)) then
            Master_Node_Ins := BIP_Initialization_Call (Obj_Id);
         else
            Master_Node_Ins := Obj_Decl;
         end if;

      else
         Master_Node_Ins := Obj_Decl;
      end if;

      --  Handle the object type and the reference to the object

      Obj_Ref := New_Occurrence_Of (Obj_Id, Loc);
      Obj_Typ := Etype (Obj_Id);
      if not Is_Class_Wide_Type (Obj_Typ) then
         Obj_Typ := Base_Type (Obj_Typ);
      end if;

      if Is_Access_Type (Obj_Typ) then
         Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);
         Obj_Typ := Available_View (Designated_Type (Obj_Typ));
      end if;

      --  If we are dealing with a return object of a build-in-place function
      --  and its allocation has been done in the function, we additionally
      --  need to detach it from the caller's finalization collection in order
      --  to prevent double finalization.

      if Present (Func_Id)
        and then Is_Build_In_Place_Function (Func_Id)
        and then Needs_BIP_Collection (Func_Id)
      then
         declare
            Ptr_Typ   : constant Node_Id   := Make_Temporary (Loc, 'P');
            Param     : constant Entity_Id :=
                          Make_Defining_Identifier (Loc, Name_V);

            Fin_Body  : Node_Id;
            Fin_Stmts : List_Id;

         begin
            Fin_Stmts := Make_Finalize_Address_Stmts (Obj_Typ);

            Append_To (Fin_Stmts,
              Build_BIP_Cleanup_Stmts
                (Func_Id, New_Occurrence_Of (Param, Loc)));

            Fin_Id :=
              Make_Defining_Identifier (Loc,
                Make_TSS_Name_Local
                  (Obj_Typ, TSS_Finalize_Address));

            Fin_Body :=
              Make_Subprogram_Body (Loc,
                Specification =>
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name       => Fin_Id,
                    Parameter_Specifications => New_List (
                      Make_Parameter_Specification (Loc,
                        Defining_Identifier => Param,
                        Parameter_Type      =>
                          New_Occurrence_Of (RTE (RE_Address), Loc)))),

                Declarations => New_List (
                  Make_Full_Type_Declaration (Loc,
                    Defining_Identifier => Ptr_Typ,
                    Type_Definition     =>
                      Make_Access_To_Object_Definition (Loc,
                        All_Present        => True,
                        Subtype_Indication =>
                          New_Occurrence_Of (Obj_Typ, Loc)))),

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Fin_Stmts));

            Insert_After_And_Analyze
              (Master_Node_Ins, Fin_Body, Suppress => All_Checks);

            Master_Node_Ins := Fin_Body;
         end;

      else
         Fin_Id := Finalize_Address (Obj_Typ);

         if No (Fin_Id) and then Ekind (Obj_Typ) = E_Class_Wide_Subtype then
            Fin_Id := TSS (Obj_Typ, TSS_Finalize_Address);
         end if;
      end if;

      --  Now build the attachment call that will initialize the object's
      --  Master_Node using the object's address and finalization procedure.

      Master_Node_Attach :=
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Attach_Object_To_Node), Loc),
          Parameter_Associations => New_List (
            Make_Address_For_Finalize (Loc, Obj_Ref, Obj_Typ),
            Make_Attribute_Reference (Loc,
              Prefix         =>
                New_Occurrence_Of (Fin_Id, Loc),
              Attribute_Name => Name_Unrestricted_Access),
            New_Occurrence_Of (Master_Node, Loc)));

      Set_Finalize_Address_For_Node (Master_Node, Fin_Id);

      --  Propagate the Ghost policy from the procedure to the node

      Set_Is_Ignored_Ghost_Entity
        (Master_Node, Is_Ignored_Ghost_Entity (Fin_Id));

      Insert_After_And_Analyze
        (Master_Node_Ins, Master_Node_Attach, Suppress => All_Checks);
   end Attach_Object_To_Master_Node;

   ------------------------------------
   -- Allows_Finalization_Collection --
   ------------------------------------

   function Allows_Finalization_Collection (Typ : Entity_Id) return Boolean is
      function In_Deallocation_Instance (E : Entity_Id) return Boolean;
      --  Determine whether entity E is inside a wrapper package created for
      --  an instance of Ada.Unchecked_Deallocation.

      ------------------------------
      -- In_Deallocation_Instance --
      ------------------------------

      function In_Deallocation_Instance (E : Entity_Id) return Boolean is
         Pkg : constant Entity_Id := Scope (E);
         Par : Node_Id := Empty;

      begin
         if Ekind (Pkg) = E_Package
           and then Present (Related_Instance (Pkg))
           and then Ekind (Related_Instance (Pkg)) = E_Procedure
         then
            Par := Generic_Parent (Parent (Related_Instance (Pkg)));

            return
              Present (Par)
                and then Chars (Par) = Name_Unchecked_Deallocation
                and then Chars (Scope (Par)) = Name_Ada
                and then Scope (Scope (Par)) = Standard_Standard;
         end if;

         return False;
      end In_Deallocation_Instance;

      --  Local variables

      Desig_Typ : constant Entity_Id := Designated_Type (Typ);
      Ptr_Typ   : constant Entity_Id :=
                    Root_Type_Of_Full_View (Base_Type (Typ));

   --  Start of processing for Allows_Finalization_Collection

   begin
      --  Certain run-time configurations and targets do not provide support
      --  for controlled types and therefore do not need collections.

      if Restriction_Active (No_Finalization) then
         return False;

      --  Do not consider C and C++ types since it is assumed that the non-Ada
      --  side will handle their cleanup.

      elsif Convention (Desig_Typ) = Convention_C
        or else Convention (Desig_Typ) = Convention_CPP
      then
         return False;

      --  Do not consider controlled types with relaxed finalization

      elsif Has_Relaxed_Finalization (Desig_Typ) then
         return False;

      --  Do not consider an access type that returns on the secondary stack

      elsif Present (Associated_Storage_Pool (Ptr_Typ))
        and then Is_RTE (Associated_Storage_Pool (Ptr_Typ), RE_SS_Pool)
      then
         return False;

      --  Do not consider an access type that can never allocate an object

      elsif No_Pool_Assigned (Ptr_Typ) then
         return False;

      --  Do not consider an access type coming from an Unchecked_Deallocation
      --  instance. Even though the designated type may be controlled, the
      --  access type will never participate in any allocations.

      elsif In_Deallocation_Instance (Ptr_Typ) then
         return False;

      --  Do not consider a non-library access type when No_Nested_Finalization
      --  is in effect, because finalization collections are controlled objects
      --  and, if created, will violate the restriction.

      elsif Restriction_Active (No_Nested_Finalization)
        and then not Is_Library_Level_Entity (Ptr_Typ)
      then
         return False;

      --  Do not consider an access type subject to pragma No_Heap_Finalization
      --  because objects allocated through such a type are not to be finalized
      --  when the access type goes out of scope.

      elsif No_Heap_Finalization (Ptr_Typ) then
         return False;

      --  Do not create finalization collections in GNATprove mode because this
      --  causes unwanted extra expansion. Compilation in this mode must always
      --  keep the tree as close as possible to the original sources.

      elsif GNATprove_Mode then
         return False;

      --  Otherwise the access type may use a finalization collection

      else
         return True;
      end if;
   end Allows_Finalization_Collection;

   --------------------------------
   -- Build_Anonymous_Collection --
   --------------------------------

   procedure Build_Anonymous_Collection (Ptr_Typ : Entity_Id) is
      function Create_Anonymous_Collection
        (Desig_Typ : Entity_Id;
         Unit_Id   : Entity_Id;
         Unit_Decl : Node_Id) return Entity_Id;
      --  Create a new anonymous collection for access type Ptr_Typ with
      --  designated type Desig_Typ. The declaration of the collection and
      --  its initialization are inserted in the declarative part of unit
      --  Unit_Decl. Unit_Id is the entity of Unit_Decl.

      function Current_Anonymous_Collection
        (Desig_Typ : Entity_Id;
         Unit_Id   : Entity_Id) return Entity_Id;
      --  Find an anonymous collection declared in unit Unit_Id which services
      --  designated type Desig_Typ. If there is none, return Empty.

      ---------------------------------
      -- Create_Anonymous_Collection --
      ---------------------------------

      function Create_Anonymous_Collection
        (Desig_Typ : Entity_Id;
         Unit_Id   : Entity_Id;
         Unit_Decl : Node_Id) return Entity_Id
      is
         Loc : constant Source_Ptr := Sloc (Unit_Id);

         All_FCs   : Elist_Id;
         Decls     : List_Id;
         FC_Decl   : Node_Id;
         FC_Id     : Entity_Id;
         Unit_Spec : Node_Id;

      begin
         --  Generate:
         --    <FC_Id> : Finalization_Collection;

         FC_Id := Make_Temporary (Loc, 'A');

         FC_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => FC_Id,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Finalization_Collection), Loc));

         --  Find the declarative list of the unit

         if Nkind (Unit_Decl) = N_Package_Declaration then
            Unit_Spec := Specification (Unit_Decl);
            Decls     := Visible_Declarations (Unit_Spec);

            if No (Decls) then
               Decls := New_List;
               Set_Visible_Declarations (Unit_Spec, Decls);
            end if;

         --  Package body or subprogram case

         --  ??? A subprogram spec or body that acts as a compilation unit may
         --  contain a formal parameter of an anonymous access-to-controlled
         --  type initialized by an allocator.

         --    procedure Comp_Unit_Proc (Param : access Ctrl := new Ctrl);

         --  There is no suitable place to create the collection because the
         --  subprogram is not in a declarative list.

         else
            Decls := Declarations (Unit_Decl);

            if No (Decls) then
               Decls := New_List;
               Set_Declarations (Unit_Decl, Decls);
            end if;
         end if;

         Prepend_To (Decls, FC_Decl);

         --  Use the scope of the unit when analyzing the declaration of the
         --  collection and its initialization actions.

         Push_Scope (Unit_Id);
         Analyze (FC_Decl);
         Pop_Scope;

         --  Mark the collection as servicing this specific designated type

         Set_Anonymous_Designated_Type (FC_Id, Desig_Typ);

         --  Include it in the list of existing anonymous collections which
         --  appear in this unit. This effectively creates a mapping between
         --  collections and designated types, which in turn allows for the
         --  reuse of collections on a per-unit basis.

         All_FCs := Anonymous_Collections (Unit_Id);

         if No (All_FCs) then
            All_FCs := New_Elmt_List;
            Set_Anonymous_Collections (Unit_Id, All_FCs);
         end if;

         Prepend_Elmt (FC_Id, All_FCs);

         return FC_Id;
      end Create_Anonymous_Collection;

      ----------------------------------
      -- Current_Anonymous_Collection --
      ----------------------------------

      function Current_Anonymous_Collection
        (Desig_Typ : Entity_Id;
         Unit_Id   : Entity_Id) return Entity_Id
      is
         All_FCs : constant Elist_Id := Anonymous_Collections (Unit_Id);
         FC_Elmt : Elmt_Id;
         FC_Id   : Entity_Id;

      begin
         --  Inspect the list of anonymous collections declared within the unit
         --  looking for an existing collection which services the designated
         --  type.

         if Present (All_FCs) then
            FC_Elmt := First_Elmt (All_FCs);
            while Present (FC_Elmt) loop
               FC_Id := Node (FC_Elmt);

               --  The current collection services the same designated type.
               --  As a result, the collection can be reused and associated
               --  with another anonymous access-to-controlled type.

               if Anonymous_Designated_Type (FC_Id) = Desig_Typ then
                  return FC_Id;
               end if;

               Next_Elmt (FC_Elmt);
            end loop;
         end if;

         return Empty;
      end Current_Anonymous_Collection;

      --  Local variables

      Desig_Typ : Entity_Id;
      FC_Id     : Entity_Id;
      Priv_View : Entity_Id;
      Scop      : Entity_Id;
      Unit_Decl : Node_Id;
      Unit_Id   : Entity_Id;

   --  Start of processing for Build_Anonymous_Collection

   begin
      --  Nothing to do if the circumstances do not allow for a finalization
      --  collection.

      if not Allows_Finalization_Collection (Ptr_Typ) then
         return;
      end if;

      Unit_Decl := Unit (Cunit (Current_Sem_Unit));
      Unit_Id   := Unique_Defining_Entity (Unit_Decl);

      --  The compilation unit is a package instantiation. In this case the
      --  anonymous collection is associated with the package spec, as both
      --  the spec and body appear at the same level.

      if Nkind (Unit_Decl) = N_Package_Body
        and then Nkind (Original_Node (Unit_Decl)) = N_Package_Instantiation
      then
         Unit_Id   := Corresponding_Spec (Unit_Decl);
         Unit_Decl := Unit_Declaration_Node (Unit_Id);
      end if;

      --  Use the initial declaration of the designated type when it denotes
      --  the full view of an incomplete or private type. This ensures that
      --  types with one and two views are treated the same.

      Desig_Typ := Directly_Designated_Type (Ptr_Typ);
      Priv_View := Incomplete_Or_Partial_View (Desig_Typ);

      if Present (Priv_View) then
         Desig_Typ := Priv_View;
      end if;

      --  For a designated type not declared at library level, we cannot create
      --  a finalization collection attached to an outer unit since this would
      --  generate dangling references to the dynamic scope through access-to-
      --  procedure values designating the local Finalize_Address primitive.

      Scop := Enclosing_Dynamic_Scope (Desig_Typ);
      if Scop /= Standard_Standard
        and then Scope_Depth (Scop) > Scope_Depth (Unit_Id)
      then
         return;
      end if;

      --  For the access result type of a function that is a library unit,
      --  we cannot create a finalization collection attached to the unit as
      --  this would cause premature finalization of objects created through
      --  the access result type, which may be returned from the function.

      if Is_Local_Anonymous_Access (Ptr_Typ)
        and then Ekind (Unit_Id) = E_Function
        and then Parent (Ptr_Typ) =
                   Result_Definition (Subprogram_Specification (Unit_Id))
      then
         return;
      end if;

      --  Determine whether the current semantic unit already has an anonymous
      --  collection which services the designated type.

      FC_Id := Current_Anonymous_Collection (Desig_Typ, Unit_Id);

      --  If this is not the case, create a new collection

      if No (FC_Id) then
         FC_Id := Create_Anonymous_Collection (Desig_Typ, Unit_Id, Unit_Decl);
      end if;

      Set_Finalization_Collection (Ptr_Typ, FC_Id);
   end Build_Anonymous_Collection;

   ----------------------------
   -- Build_Array_Deep_Procs --
   ----------------------------

   procedure Build_Array_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc
          (Prim  => Initialize_Case,
           Typ   => Typ,
           Stmts => Make_Deep_Array_Body (Initialize_Case, Typ)));

      if not Is_Inherently_Limited_Type (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Adjust_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Array_Body (Adjust_Case, Typ)));
      end if;

      --  Do not generate Deep_Finalize and Finalize_Address if finalization is
      --  suppressed since these routine will not be used.

      if not Restriction_Active (No_Finalization) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Finalize_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Array_Body (Finalize_Case, Typ)));

         --  Create TSS primitive Finalize_Address (unless CodePeer_Mode)

         if not CodePeer_Mode then
            Set_TSS (Typ,
              Make_Deep_Proc
                (Prim  => Address_Case,
                 Typ   => Typ,
                 Stmts => Make_Deep_Array_Body (Address_Case, Typ)));
         end if;
      end if;
   end Build_Array_Deep_Procs;

   ------------------------------
   -- Build_Cleanup_Statements --
   ------------------------------

   function Build_Cleanup_Statements
     (N                  : Node_Id;
      Additional_Cleanup : List_Id) return List_Id
   is
      Is_Asynchronous_Call : constant Boolean :=
        Nkind (N) = N_Block_Statement and then Is_Asynchronous_Call_Block (N);
      Is_Master : constant Boolean :=
        Nkind (N) /= N_Entry_Body and then Is_Task_Master (N);
      Is_Protected_Subp_Body : constant Boolean :=
        Nkind (N) = N_Subprogram_Body
        and then Is_Protected_Subprogram_Body (N);
      Is_Task_Allocation : constant Boolean :=
        Nkind (N) = N_Block_Statement and then Is_Task_Allocation_Block (N);
      Is_Task_Body : constant Boolean :=
        Nkind (Original_Node (N)) = N_Task_Body;

      Loc   : constant Source_Ptr := Sloc (N);
      Stmts : constant List_Id    := New_List;

   begin
      if Is_Task_Body then
         if Restricted_Profile then
            Append_To (Stmts,
              Build_Runtime_Call (Loc, RE_Complete_Restricted_Task));
         else
            Append_To (Stmts, Build_Runtime_Call (Loc, RE_Complete_Task));
         end if;

      elsif Is_Master then
         if Restriction_Active (No_Task_Hierarchy) = False then
            Append_To (Stmts, Build_Runtime_Call (Loc, RE_Complete_Master));
         end if;

      --  Add statements to undefer abort.

      elsif Is_Protected_Subp_Body then
         if Abort_Allowed then
            Append_To (Stmts, Build_Runtime_Call (Loc, RE_Abort_Undefer));
         end if;

      --  Add a call to Expunge_Unactivated_Tasks for dynamically allocated
      --  tasks. Other unactivated tasks are completed by Complete_Task or
      --  Complete_Master.

      --  NOTE: The generated code references _chain, a local object

      elsif Is_Task_Allocation then

         --  Generate:
         --     Expunge_Unactivated_Tasks (_chain);

         --  where _chain is the list of tasks created by the allocator but not
         --  yet activated. This list will be empty unless the block completes
         --  abnormally.

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of
                 (RTE (RE_Expunge_Unactivated_Tasks), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Activation_Chain_Entity (N), Loc))));

      --  Attempt to cancel an asynchronous entry call whenever the block which
      --  contains the abortable part is exited.

      --  NOTE: The generated code references Cnn, a local object

      elsif Is_Asynchronous_Call then
         declare
            Cancel_Param : constant Entity_Id :=
                             Entry_Cancel_Parameter (Entity (Identifier (N)));

         begin
            --  If it is of type Communication_Block, this must be a protected
            --  entry call. Generate:

            --    if Enqueued (Cancel_Param) then
            --       Cancel_Protected_Entry_Call (Cancel_Param);
            --    end if;

            if Is_RTE (Etype (Cancel_Param), RE_Communication_Block) then
               Append_To (Stmts,
                 Make_If_Statement (Loc,
                   Condition =>
                     Make_Function_Call (Loc,
                       Name                   =>
                         New_Occurrence_Of (RTE (RE_Enqueued), Loc),
                       Parameter_Associations => New_List (
                         New_Occurrence_Of (Cancel_Param, Loc))),

                   Then_Statements => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name =>
                         New_Occurrence_Of
                           (RTE (RE_Cancel_Protected_Entry_Call), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (Cancel_Param, Loc))))));

            --  Asynchronous delay, generate:
            --    Cancel_Async_Delay (Cancel_Param);

            elsif Is_RTE (Etype (Cancel_Param), RE_Delay_Block) then
               Append_To (Stmts,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Cancel_Async_Delay), Loc),
                   Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Cancel_Param, Loc),
                       Attribute_Name => Name_Unchecked_Access))));

            --  Task entry call, generate:
            --    Cancel_Task_Entry_Call (Cancel_Param);

            else
               Append_To (Stmts,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Cancel_Task_Entry_Call), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Cancel_Param, Loc))));
            end if;
         end;
      end if;

      Append_List_To (Stmts, Additional_Cleanup);
      return Stmts;
   end Build_Cleanup_Statements;

   -----------------------------
   -- Build_Controlling_Procs --
   -----------------------------

   procedure Build_Controlling_Procs (Typ : Entity_Id) is
   begin
      if Is_Array_Type (Typ) then
         Build_Array_Deep_Procs (Typ);
      else pragma Assert (Is_Record_Type (Typ));
         Build_Record_Deep_Procs (Typ);
      end if;
   end Build_Controlling_Procs;

   -----------------------------
   -- Build_Exception_Handler --
   -----------------------------

   function Build_Exception_Handler
     (Data        : Finalization_Exception_Data;
      For_Library : Boolean := False) return Node_Id
   is
      Actuals      : List_Id;
      Proc_To_Call : Entity_Id;
      Except       : Node_Id;
      Stmts        : List_Id;

   begin
      pragma Assert (Present (Data.Raised_Id));

      if Exception_Extra_Info
        or else (For_Library and not Restricted_Profile)
      then
         if Exception_Extra_Info then

            --  Generate:

            --    Get_Current_Excep.all

            Except :=
              Make_Function_Call (Data.Loc,
                Name =>
                  Make_Explicit_Dereference (Data.Loc,
                    Prefix =>
                      New_Occurrence_Of
                        (RTE (RE_Get_Current_Excep), Data.Loc)));

         else
            --  Generate:

            --    null

            Except := Make_Null (Data.Loc);
         end if;

         if For_Library and then not Restricted_Profile then
            Proc_To_Call := RTE (RE_Save_Library_Occurrence);
            Actuals := New_List (Except);

         else
            Proc_To_Call := RTE (RE_Save_Occurrence);

            --  The dereference occurs only when Exception_Extra_Info is true,
            --  and therefore Except is not null.

            Actuals :=
              New_List (
                New_Occurrence_Of (Data.E_Id, Data.Loc),
                Make_Explicit_Dereference (Data.Loc, Except));
         end if;

         --  Generate:

         --    when others =>
         --       if not Raised_Id then
         --          Raised_Id := True;

         --          Save_Occurrence (E_Id, Get_Current_Excep.all.all);
         --            or
         --          Save_Library_Occurrence (Get_Current_Excep.all);
         --       end if;

         Stmts :=
           New_List (
             Make_If_Statement (Data.Loc,
               Condition       =>
                 Make_Op_Not (Data.Loc,
                   Right_Opnd => New_Occurrence_Of (Data.Raised_Id, Data.Loc)),

               Then_Statements => New_List (
                 Make_Assignment_Statement (Data.Loc,
                   Name       => New_Occurrence_Of (Data.Raised_Id, Data.Loc),
                   Expression => New_Occurrence_Of (Standard_True, Data.Loc)),

                 Make_Procedure_Call_Statement (Data.Loc,
                   Name                   =>
                     New_Occurrence_Of (Proc_To_Call, Data.Loc),
                   Parameter_Associations => Actuals))));

      else
         --  Generate:

         --    Raised_Id := True;

         Stmts := New_List (
           Make_Assignment_Statement (Data.Loc,
             Name       => New_Occurrence_Of (Data.Raised_Id, Data.Loc),
             Expression => New_Occurrence_Of (Standard_True, Data.Loc)));
      end if;

      --  Generate:

      --    when others =>

      return
        Make_Exception_Handler (Data.Loc,
          Exception_Choices => New_List (Make_Others_Choice (Data.Loc)),
          Statements        => Stmts);
   end Build_Exception_Handler;

   -----------------------------------
   -- Build_Finalization_Collection --
   -----------------------------------

   procedure Build_Finalization_Collection
     (Typ            : Entity_Id;
      For_Lib_Level  : Boolean   := False;
      For_Private    : Boolean   := False;
      Context_Scope  : Entity_Id := Empty;
      Insertion_Node : Node_Id   := Empty)
   is
      Ptr_Typ : constant Entity_Id := Root_Type_Of_Full_View (Base_Type (Typ));
      --  Finalization collections built for named access types are associated
      --  with the full view (if applicable) as a consequence of freezing. The
      --  full view criteria does not apply to anonymous access types because
      --  those cannot have a private and a full view.

   --  Start of processing for Build_Finalization_Collection

   begin
      --  Nothing to do if the circumstances do not allow for a finalization
      --  collection.

      if not Allows_Finalization_Collection (Typ) then
         return;

      --  Various machinery such as freezing may have already created a
      --  finalization collection.

      elsif Present (Finalization_Collection (Ptr_Typ)) then
         return;
      end if;

      declare
         Actions : constant List_Id    := New_List;
         Loc     : constant Source_Ptr := Sloc (Ptr_Typ);

         Fin_Coll_Id : Entity_Id;
         Pool_Id     : Entity_Id;

      begin
         --  Source access types use fixed names since the collection will be
         --  inserted in the same source unit only once. The only exception to
         --  this are instances using the same access type as generic actual.

         if Comes_From_Source (Ptr_Typ) and then not Inside_A_Generic then
            Fin_Coll_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Ptr_Typ), "FC"));

         --  Internally generated access types use temporaries as their names
         --  due to possible collision with identical names coming from other
         --  packages.

         else
            Fin_Coll_Id := Make_Temporary (Loc, 'F');
         end if;

         Set_Finalization_Collection (Ptr_Typ, Fin_Coll_Id);

         --  Generate:
         --    <Ptr_Typ>FC : aliased Finalization_Collection;

         Append_To (Actions,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Fin_Coll_Id,
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Finalization_Collection), Loc)));

         if Debug_Generated_Code then
            Set_Debug_Info_Needed (Fin_Coll_Id);
         end if;

         --  Set the associated pool and primitive Finalize_Address of the new
         --  finalization collection.

         --  The access type has a user-defined storage pool, use it

         if Present (Associated_Storage_Pool (Ptr_Typ)) then
            Pool_Id := Associated_Storage_Pool (Ptr_Typ);

         --  Otherwise the default choice is the global storage pool

         else
            Pool_Id := RTE (RE_Global_Pool_Object);
            Set_Associated_Storage_Pool (Ptr_Typ, Pool_Id);
         end if;

         --  A finalization collection created for an access designating a type
         --  with private components is inserted before a context-dependent
         --  node.

         if For_Private then

            --  At this point both the scope of the context and the insertion
            --  mode must be known.

            pragma Assert (Present (Context_Scope));
            pragma Assert (Present (Insertion_Node));

            Push_Scope (Context_Scope);

            --  Treat use clauses as declarations and insert directly in front
            --  of them.

            if Nkind (Insertion_Node) in
                 N_Use_Package_Clause | N_Use_Type_Clause
            then
               Insert_List_Before_And_Analyze (Insertion_Node, Actions);
            else
               Insert_Actions (Insertion_Node, Actions);
            end if;

            Pop_Scope;

         --  The finalization collection belongs to an access type related
         --  to a build-in-place function call used to initialize a library
         --  level object. The collection must be inserted in front of the
         --  access type declaration denoted by Insertion_Node.

         elsif For_Lib_Level then
            pragma Assert (Present (Insertion_Node));
            Insert_Actions (Insertion_Node, Actions);

         --  Otherwise the finalization collection and its initialization
         --  become a part of the freeze node.

         else
            Append_Freeze_Actions (Ptr_Typ, Actions);
         end if;

         Analyze_List (Actions);

         --  When the type the finalization collection is being generated for
         --  was created to store a 'Old object, then mark it as such so its
         --  finalization can be delayed until after postconditions have been
         --  checked.

         if Stores_Attribute_Old_Prefix (Ptr_Typ) then
            Set_Stores_Attribute_Old_Prefix (Fin_Coll_Id);
         end if;
      end;
   end Build_Finalization_Collection;

   ---------------------
   -- Build_Finalizer --
   ---------------------

   procedure Build_Finalizer
     (N                 : Node_Id;
      Clean_Stmts       : List_Id;
      Mark_Id           : Entity_Id;
      Defer_Abort       : Boolean;
      Fin_Id            : out Entity_Id)
   is
      Acts_As_Clean    : constant Boolean :=
                           Present (Mark_Id)
                             or else
                               (Present (Clean_Stmts)
                                 and then Is_Non_Empty_List (Clean_Stmts));

      For_Package_Body : constant Boolean := Nkind (N) = N_Package_Body;
      For_Package_Spec : constant Boolean := Nkind (N) = N_Package_Declaration;
      For_Package      : constant Boolean :=
                           For_Package_Body or else For_Package_Spec;
      Loc              : constant Source_Ptr := Sloc (N);

      --  NOTE: Local variable declarations are conservative and do not create
      --  structures right from the start. Entities and lists are created once
      --  it has been established that N has at least one controlled object.

      Count : Nat := 0;
      --  Holds the number of controlled objects encountered so far

      Decls : List_Id := No_List;
      --  Declarative region of N (if available). If N is a package declaration
      --  Decls denotes the visible declarations.

      Finalizer_Data : Finalization_Exception_Data;
      --  Data for the exception

      Finalizer_Decls : List_Id := No_List;
      --  Local variable declarations

      Finalization_Master : Entity_Id;
      --  The Finalization Master object

      Finalizer_Stmts : List_Id := No_List;
      --  The statement list of the finalizer body

      Has_Strict_Ctrl_Objs : Boolean := False;
      --  A general flag which indicates whether N has at least one controlled
      --  object with strict semantics for finalization.

      Has_Tagged_Types : Boolean := False;
      --  A general flag which indicates whether N has at least one library-
      --  level tagged type declaration.

      HSS : Node_Id := Empty;
      --  The sequence of statements of N (if available)

      Prev_At_End : Entity_Id := Empty;
      --  The previous at end procedure of the handled statements block of N

      Priv_Decls : List_Id := No_List;
      --  The private declarations of N if N is a package declaration

      Spec_Id    : Entity_Id := Empty;
      Stmts      : List_Id   := No_List;

      Tagged_Type_Stmts : List_Id := No_List;
      --  Contains calls to Ada.Tags.Unregister_Tag for all library-level
      --  tagged types found in N.

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Build_Components;
      --  Create all entites and initialize all lists used in the creation of
      --  the finalizer.

      procedure Create_Finalizer;
      --  Create the spec and body of the finalizer and insert them in the
      --  proper place in the tree depending on the context.

      function Has_Ctrl_Objs return Boolean is (Count > 0);
      --  Return true if N contains a least one controlled object

      function New_Finalizer_Name
        (Spec_Id : Node_Id; For_Spec : Boolean) return Name_Id;
      --  Create a fully qualified name of a package spec or body finalizer.
      --  The generated name is of the form: xx__yy__finalize_[spec|body].

      procedure Process_Declarations
        (Decls      : List_Id;
         Preprocess : Boolean := False);
      --  Inspect a list of declarations or statements which may contain
      --  objects that need finalization. When flag Preprocess is set, the
      --  routine will simply count the total number of controlled objects in
      --  Decls and set Count accordingly.

      procedure Process_Object_Declaration
        (Decl         : Node_Id;
         Is_Protected : Boolean := False);
      --  Generate all the machinery associated with the finalization of a
      --  single object. Flag Is_Protected is set when Decl denotes a simple
      --  protected object.

      procedure Process_Tagged_Type_Declaration (Decl : Node_Id);
      --  Generate all the code necessary to unregister the external tag of a
      --  tagged type.

      ----------------------
      -- Build_Components --
      ----------------------

      procedure Build_Components is
         Constraints : List_Id;
         Master_Decl : Node_Id;
         Master_Name : Name_Id;

      begin
         pragma Assert (Present (Decls));

         --  If the context contains controlled objects with strict semantics
         --  for finalization, then we create the finalization master, unless
         --  there is a single such object: in this common case, we'll directly
         --  finalize the object.

         if Has_Strict_Ctrl_Objs then
            if Count > 1 then
               if For_Package_Spec then
                  Master_Name :=
                    New_External_Name (Name_uMaster, Suffix => "_spec");
               elsif For_Package_Body then
                  Master_Name :=
                    New_External_Name (Name_uMaster, Suffix => "_body");
               else
                  Master_Name := New_Internal_Name ('M');
               end if;

               Finalization_Master :=
                 Make_Defining_Identifier (Loc, Master_Name);

               --  The master is statically parameterized by the context

               Constraints := New_List;
               Append_To (Constraints,
                 New_Occurrence_Of (Boolean_Literals (Exceptions_OK), Loc));
               Append_To (Constraints,
                 New_Occurrence_Of
                  (Boolean_Literals (Exception_Extra_Info), Loc));
               Append_To (Constraints,
                 New_Occurrence_Of (Boolean_Literals (For_Package), Loc));

               Master_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Finalization_Master,
                   Object_Definition   =>
                     Make_Subtype_Indication (Loc,
                       Subtype_Mark =>
                         New_Occurrence_Of
                           (RTE (RE_Finalization_Master), Loc),
                       Constraint  =>
                         Make_Index_Or_Discriminant_Constraint (Loc,
                           Constraints => Constraints)));

               Prepend_To (Decls, Master_Decl);
               Analyze (Master_Decl, Suppress => All_Checks);
            end if;

            if Exceptions_OK then
               Finalizer_Decls := New_List;
               Build_Object_Declarations
                 (Finalizer_Data, Finalizer_Decls, Loc, For_Package);
            end if;
         end if;

         --  If the context requires additional cleanup, the finalization
         --  machinery is added after the cleanup code.

         if Acts_As_Clean then
            Finalizer_Stmts := Clean_Stmts;
         else
            Finalizer_Stmts := New_List;
         end if;

         if Has_Tagged_Types then
            Tagged_Type_Stmts := New_List;
         end if;
      end Build_Components;

      ----------------------
      -- Create_Finalizer --
      ----------------------

      procedure Create_Finalizer is
         Body_Id    : Entity_Id;
         Fin_Body   : Node_Id;
         Fin_Call   : Node_Id;
         Fin_Spec   : Node_Id;

      begin
         --  Step 1: Creation of the finalizer name

         --  Packages must use a distinct name for their finalizers since the
         --  binder will have to generate calls to them by name. The name is
         --  of the following form:

         --    xx__yy__finalize_[spec|body]

         if For_Package then
            Fin_Id := Make_Defining_Identifier
                        (Loc, New_Finalizer_Name (Spec_Id, For_Package_Spec));
            Set_Has_Qualified_Name       (Fin_Id);
            Set_Has_Fully_Qualified_Name (Fin_Id);

         --  The default name is _finalizer

         else
            Fin_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Name_uFinalizer));
            Set_Is_Finalizer (Fin_Id);

            --  The visibility semantics of At_End handlers force a strange
            --  separation of spec and body for stack-related finalizers:

            --     declare : Enclosing_Scope
            --        procedure _finalizer;
            --     begin
            --        <controlled objects>
            --        procedure _finalizer is
            --           ...
            --     at end
            --        _finalizer;
            --     end;

            --  Both spec and body are within the same construct and scope, but
            --  the body is part of the handled sequence of statements. This
            --  placement confuses the elaboration mechanism on targets where
            --  At_End handlers are expanded into "when all others" handlers:

            --     exception
            --        when all others =>
            --           _finalizer;  --  appears to require elab checks
            --     at end
            --        _finalizer;
            --     end;

            --  Since the compiler guarantees that the body of a _finalizer is
            --  always inserted in the same construct where the At_End handler
            --  resides, there is no need for elaboration checks.

            Set_Kill_Elaboration_Checks (Fin_Id);

            --  Inlining the finalizer produces a substantial speedup at -O2.
            --  It is inlined by default at -O3. Either way, it is called
            --  exactly twice (once on the normal path, and once for
            --  exceptions/abort), so this won't bloat the code too much.

            Set_Is_Inlined (Fin_Id);
         end if;

         if Debug_Generated_Code then
            Set_Debug_Info_Needed (Fin_Id);
         end if;

         --  Step 2: Creation of the finalizer specification

         --  Generate:
         --    procedure Fin_Id;

         Fin_Spec :=
           Make_Subprogram_Declaration (Loc,
             Specification =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Fin_Id));

         if For_Package then
            Set_Is_Exported (Fin_Id);
            Set_Interface_Name (Fin_Id,
              Make_String_Literal (Loc,
                Strval => Get_Name_String (Chars (Fin_Id))));
         end if;

         --  Step 3: Creation of the finalizer body

         --  Add the library-level tagged type unregistration machinery before
         --  the finalization circuitry. This ensures that external tags will
         --  be removed even if a finalization exception occurs at some point.

         if Has_Tagged_Types then
            Prepend_List_To (Finalizer_Stmts, Tagged_Type_Stmts);
         end if;

         --  Add a call to the previous At_End handler if it exists. The call
         --  must always precede the finalization circuitry.

         if Present (Prev_At_End) then
            Prepend_To (Finalizer_Stmts,
              Make_Procedure_Call_Statement (Loc, Prev_At_End));

            --  Clear the At_End handler since we have already generated the
            --  proper replacement call for it.

            Set_At_End_Proc (HSS, Empty);
         end if;

         --   If there are no controlled objects to be finalized, generate;

         --    procedure Fin_Id is
         --    begin
         --       Abort_Defer;               --  Added if abort is allowed
         --       <call to Prev_At_End>      --  Added if exists
         --       <tag unregistration>       --  Added if Has_Tagged_Types
         --       <cleanup statements>       --  Added if Acts_As_Clean
         --       <stack release>            --  Added if Mark_Id exists
         --       Abort_Undefer;             --  Added if abort is allowed
         --    end Fin_Id;

         --  If there are strict controlled objects to be finalized, generate:

         --    procedure Fin_Id is
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --       E      : Exception_Occurrence;
         --       Raised : Boolean := False;
         --    begin
         --       Abort_Defer;               --  Added if abort is allowed
         --       <call to Prev_At_End>      --  Added if exists
         --       <tag unregistration>       --  Added if Has_Tagged_Types
         --       <cleanup statements>       --  Added if Acts_As_Clean
         --       <finalization statements>
         --       <stack release>            --  Added if Mark_Id exists
         --       Abort_Undefer;             --  Added if abort is allowed
         --       <exception propagation>
         --    end Fin_Id;

         --  If there are only controlled objects with relaxed semantics for
         --  finalization, only the <finalization statements> are generated.

         if Has_Strict_Ctrl_Objs and then Count > 1 then
            Fin_Call :=
              Make_Procedure_Call_Statement (Loc,
               Name                   =>
                 New_Occurrence_Of (RTE (RE_Finalize_Master), Loc),
                 Parameter_Associations =>
                   New_List (New_Occurrence_Of (Finalization_Master, Loc)));

            --  For CodePeer, the exception handlers normally generated here
            --  generate complex flowgraphs which result in capacity problems.
            --  Omitting these handlers for CodePeer is justified as follows:

            --    If a handler is dead, then omitting it is surely ok

            --    If a handler is live, then CodePeer should flag the
            --      potentially-exception-raising construct that causes it
            --      to be live. That is what we are interested in, not what
            --      happens after the exception is raised.

            if Exceptions_OK and not CodePeer_Mode then
               Fin_Call :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (Fin_Call),

                    Exception_Handlers => New_List (
                      Build_Exception_Handler
                        (Finalizer_Data, For_Package))));
            end if;

            Append_To (Finalizer_Stmts, Fin_Call);
         end if;

         --  Release the secondary stack

         if Present (Mark_Id) then
            declare
               Release : Node_Id := Build_SS_Release_Call (Loc, Mark_Id);

            begin
               --  If the context is a build-in-place function, the secondary
               --  stack must be released, unless the build-in-place function
               --  itself is returning on the secondary stack. Generate:
               --
               --    if BIP_Alloc_Form /= Secondary_Stack then
               --       SS_Release (Mark_Id);
               --    end if;
               --
               --  Note that if the function returns on the secondary stack,
               --  then the responsibility of reclaiming the space is always
               --  left to the caller (recursively if needed).

               if Nkind (N) = N_Subprogram_Body then
                  declare
                     Spec_Id : constant Entity_Id :=
                                 Unique_Defining_Entity (N);
                     BIP_SS  : constant Boolean :=
                                 Is_Build_In_Place_Function (Spec_Id)
                                   and then Needs_BIP_Alloc_Form (Spec_Id);
                  begin
                     if BIP_SS then
                        Release :=
                          Make_If_Statement (Loc,
                            Condition       =>
                              Make_Op_Ne (Loc,
                                Left_Opnd  =>
                                  New_Occurrence_Of
                                    (Build_In_Place_Formal
                                      (Spec_Id, BIP_Alloc_Form), Loc),
                                Right_Opnd =>
                                  Make_Integer_Literal (Loc,
                                    UI_From_Int
                                      (BIP_Allocation_Form'Pos
                                        (Secondary_Stack)))),

                            Then_Statements => New_List (Release));
                     end if;
                  end;
               end if;

               Append_To (Finalizer_Stmts, Release);
            end;
         end if;

         --  Protect the statements with abort defer/undefer. This is only when
         --  aborts are allowed and the cleanup statements require deferral or
         --  there are controlled objects to be finalized. Note that the abort
         --  defer/undefer pair does not require an extra block because the
         --  finalization exception is caught in its corresponding finalization
         --  block. As a result, the call to Abort_Defer always takes place.

         if Abort_Allowed and then (Defer_Abort or Has_Ctrl_Objs) then
            Prepend_To (Finalizer_Stmts,
              Build_Runtime_Call (Loc, RE_Abort_Defer));

            Append_To (Finalizer_Stmts,
              Build_Runtime_Call (Loc, RE_Abort_Undefer));
         end if;

         --  The local exception does not need to be reraised for library-level
         --  finalizers. Note that this action must be carried out after object
         --  cleanup, secondary stack release, and abort undeferral. Generate:

         --    if Raised and then not Abort then
         --       Raise_From_Controlled_Operation (E);
         --    end if;

         if Has_Strict_Ctrl_Objs and Exceptions_OK and not For_Package then
            Append_To (Finalizer_Stmts,
              Build_Raise_Statement (Finalizer_Data));
         end if;

         --  Create the body of the finalizer

         Body_Id := Make_Defining_Identifier (Loc, Chars (Fin_Id));

         if Debug_Generated_Code then
            Set_Debug_Info_Needed (Body_Id);
         end if;

         if For_Package then
            Set_Has_Qualified_Name       (Body_Id);
            Set_Has_Fully_Qualified_Name (Body_Id);
         end if;

         Fin_Body :=
           Make_Subprogram_Body (Loc,
             Specification              =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Body_Id),
             Declarations               => Finalizer_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Finalizer_Stmts));

         --  Step 4: Spec and body insertion, analysis

         if For_Package then

            --  If a package spec has private declarations, both the finalizer
            --  spec and body are inserted at the end of this list.

            if For_Package_Spec and then Present (Priv_Decls) then
               Append_To (Priv_Decls, Fin_Spec);
               Append_To (Priv_Decls, Fin_Body);

            --  Otherwise, and for a package body, both the finalizer spec and
            --  body are inserted at the end of the package declarations.

            else
               Append_To (Decls, Fin_Spec);
               Append_To (Decls, Fin_Body);
            end if;

         --  Non-package case

         else
            --  Insert the spec for the finalizer. The At_End handler must be
            --  able to call the body which resides in a nested structure.

            --    declare
            --       procedure Fin_Id;                  --  Spec
            --    begin
            --       <objects and possibly statements>
            --       procedure Fin_Id is ...            --  Body
            --       <statements>
            --    at end
            --       Fin_Id;                            --  At_End handler
            --    end;

            pragma Assert (Present (Decls));

            Append_To (Decls, Fin_Spec);

            --  When the finalizer acts solely as a cleanup routine, the body
            --  is inserted right after the spec.

            if Acts_As_Clean and not Has_Ctrl_Objs then
               Insert_After (Fin_Spec, Fin_Body);

            --  In other cases the body is inserted after the last statement

            else
               --  Manually freeze the spec. This is somewhat of a hack because
               --  a subprogram is frozen when its body is seen and the freeze
               --  node appears right before the body. However, in this case,
               --  the spec must be frozen earlier since the At_End handler
               --  must be able to call it.
               --
               --    declare
               --       procedure Fin_Id;               --  Spec
               --       [Fin_Id]                        --  Freeze node
               --    begin
               --       ...
               --    at end
               --       Fin_Id;                         --  At_End handler
               --    end;

               Ensure_Freeze_Node (Fin_Id);
               Insert_After (Fin_Spec, Freeze_Node (Fin_Id));
               Set_Is_Frozen (Fin_Id);

               Append_To (Stmts, Fin_Body);
            end if;
         end if;

         Analyze (Fin_Spec, Suppress => All_Checks);
         Analyze (Fin_Body, Suppress => All_Checks);

         --  Never consider that the finalizer procedure is enabled Ghost, even
         --  when the corresponding unit is Ghost, as this would lead to an
         --  an external name with a ___ghost_ prefix that the binder cannot
         --  generate, as it has no knowledge of the Ghost status of units.

         Set_Is_Checked_Ghost_Entity (Fin_Id, False);
      end Create_Finalizer;

      ------------------------
      -- New_Finalizer_Name --
      ------------------------

      function New_Finalizer_Name
        (Spec_Id : Node_Id; For_Spec : Boolean) return Name_Id
      is
         procedure New_Finalizer_Name (Id : Entity_Id);
         --  Place "__<name-of-Id>" in the name buffer. If the identifier
         --  has a non-standard scope, process the scope first.

         ------------------------
         -- New_Finalizer_Name --
         ------------------------

         procedure New_Finalizer_Name (Id : Entity_Id) is
         begin
            if Scope (Id) = Standard_Standard then
               Get_Name_String (Chars (Id));

            else
               New_Finalizer_Name (Scope (Id));
               Add_Str_To_Name_Buffer ("__");
               Get_Name_String_And_Append (Chars (Id));
            end if;
         end New_Finalizer_Name;

      --  Start of processing for New_Finalizer_Name

      begin
         --  Create the fully qualified name of the enclosing scope

         New_Finalizer_Name (Spec_Id);

         --  Generate:
         --    __finalize_[spec|body]

         Add_Str_To_Name_Buffer ("__finalize_");

         if For_Spec then
            Add_Str_To_Name_Buffer ("spec");
         else
            Add_Str_To_Name_Buffer ("body");
         end if;

         return Name_Find;
      end New_Finalizer_Name;

      --------------------------
      -- Process_Declarations --
      --------------------------

      procedure Process_Declarations
        (Decls      : List_Id;
         Preprocess : Boolean := False)
      is
         procedure Process_Package_Body (Decl : Node_Id);
         --  Process an N_Package_Body node

         procedure Processing_Actions
           (Decl         : Node_Id;
            Is_Protected : Boolean := False;
            Strict       : Boolean := False);
         --  Depending on the mode of operation of Process_Declarations, either
         --  increment the controlled object count or process the declaration.
         --  The Flag Is_Protected is set when the declaration denotes a simple
         --  protected object. The flag Strict is true when the declaration is
         --  for a controlled object with strict semantics for finalization.

         --------------------------
         -- Process_Package_Body --
         --------------------------

         procedure Process_Package_Body (Decl : Node_Id) is
         begin
            --  Do not inspect an ignored Ghost package body because all
            --  code found within will not appear in the final tree.

            if Is_Ignored_Ghost_Entity (Defining_Entity (Decl)) then
               null;

            elsif Ekind (Corresponding_Spec (Decl)) /= E_Generic_Package then
               Process_Declarations (Declarations (Decl), Preprocess);
            end if;
         end Process_Package_Body;

         ------------------------
         -- Processing_Actions --
         ------------------------

         procedure Processing_Actions
           (Decl         : Node_Id;
            Is_Protected : Boolean := False;
            Strict       : Boolean := False)
         is
         begin
            --  Library-level tagged type

            if Nkind (Decl) = N_Full_Type_Declaration then
               if Preprocess then
                  Has_Tagged_Types := True;

               --  Unregister tagged type, unless No_Tagged_Type_Registration
               --  is active.

               elsif not Restriction_Active (No_Tagged_Type_Registration) then
                  Process_Tagged_Type_Declaration (Decl);
               end if;

            --  Controlled object declaration

            else
               if Preprocess then
                  Count := Count + 1;
                  if Strict then
                     Has_Strict_Ctrl_Objs := True;
                  end if;

               else
                  Process_Object_Declaration (Decl, Is_Protected);
               end if;
            end if;
         end Processing_Actions;

         --  Local variables

         Decl    : Node_Id;
         Expr    : Node_Id;
         Obj_Id  : Entity_Id;
         Obj_Typ : Entity_Id;
         Pack_Id : Entity_Id;
         Prev    : Node_Id;
         Spec    : Node_Id;
         Typ     : Entity_Id;

      --  Start of processing for Process_Declarations

      begin
         if Is_Empty_List (Decls) then
            return;
         end if;

         --  Process all declarations in reverse order and be prepared for them
         --  to be moved during the processing.

         Decl := Last_Non_Pragma (Decls);
         while Present (Decl) loop
            Prev := Prev_Non_Pragma (Decl);

            --  Library-level tagged types

            if Nkind (Decl) = N_Full_Type_Declaration then
               Typ := Defining_Identifier (Decl);

               --  Ignored Ghost types do not need any cleanup actions because
               --  they will not appear in the final tree.

               if Is_Ignored_Ghost_Entity (Typ) then
                  null;

               elsif Is_Tagged_Type (Typ)
                 and then Is_Library_Level_Entity (Typ)
                 and then Convention (Typ) = Convention_Ada
                 and then Present (Access_Disp_Table (Typ))
                 and then not Is_Abstract_Type (Typ)
                 and then not No_Run_Time_Mode
                 and then not Restriction_Active (No_Tagged_Type_Registration)
                 and then RTE_Available (RE_Register_Tag)
               then
                  Processing_Actions (Decl);
               end if;

            --  Regular object declarations

            elsif Nkind (Decl) = N_Object_Declaration then
               Obj_Id  := Defining_Identifier (Decl);
               Obj_Typ := Base_Type (Etype (Obj_Id));
               Expr    := Expression (Decl);

               --  Bypass any form of processing for objects which have their
               --  finalization disabled. This applies only to objects at the
               --  library level.

               if For_Package and then Finalize_Storage_Only (Obj_Typ) then
                  null;

               --  Finalization of transient objects is treated separately in
               --  order to handle sensitive cases. These include:

               --    * Conditional expressions
               --    * Expressions with actions
               --    * Transient scopes

               elsif Is_Finalized_Transient (Obj_Id) then
                  null;

               --  Finalization of specific objects is also treated separately

               elsif Is_Ignored_For_Finalization (Obj_Id) then
                  null;

               --  Ignored Ghost objects do not need any cleanup actions
               --  because they will not appear in the final tree.

               elsif Is_Ignored_Ghost_Entity (Obj_Id) then
                  null;

               --  Conversely, if one of the above cases created a Master_Node,
               --  finalization actions are required for the associated object.

               elsif Ekind (Obj_Id) = E_Variable
                 and then Is_RTE (Obj_Typ, RE_Master_Node)
               then
                  Processing_Actions (Decl);

               --  The object is of the form:
               --    Obj : [constant] Typ [:= Expr];

               --  Do not process the incomplete view of a deferred constant.
               --  Note that an object initialized by means of a BIP function
               --  call may appear as a deferred constant after expansion
               --  activities. These kinds of objects must be finalized.

               elsif not Is_Imported (Obj_Id)
                 and then Needs_Finalization (Obj_Typ)
                 and then not (Ekind (Obj_Id) = E_Constant
                                and then not Has_Completion (Obj_Id)
                                and then No (BIP_Initialization_Call (Obj_Id)))
               then
                  Processing_Actions
                    (Decl, Strict => not Has_Relaxed_Finalization (Obj_Typ));

               --  The object is of the form:
               --    Obj : Access_Typ := Non_BIP_Function_Call'reference;

               --    Obj : Access_Typ :=
               --            BIP_Function_Call (BIPalloc => 2, ...)'reference;

               elsif Is_Access_Type (Obj_Typ)
                 and then Needs_Finalization
                            (Available_View (Designated_Type (Obj_Typ)))
                 and then Present (Expr)
                 and then
                   (Is_Secondary_Stack_BIP_Func_Call (Expr)
                     or else
                       (Is_Non_BIP_Func_Call (Expr)
                         and then not Is_Related_To_Func_Return (Obj_Id)))
               then
                  Processing_Actions
                    (Decl,
                     Strict => not Has_Relaxed_Finalization
                                 (Available_View (Designated_Type (Obj_Typ))));

               --  Simple protected objects which use the type System.Tasking.
               --  Protected_Objects.Protection to manage their locks should
               --  be treated as controlled since they require manual cleanup.
               --  but not for restricted run-time libraries (Ravenscar), see
               --  also Cleanup_Protected_Object.

               --  The only exception is illustrated in the following example:

               --     package Pkg is
               --        type Ctrl is new Controlled ...
               --        procedure Finalize (Obj : in out Ctrl);
               --        Lib_Obj : Ctrl;
               --     end Pkg;

               --     package body Pkg is
               --        protected Prot is
               --           procedure Do_Something (Obj : in out Ctrl);
               --        end Prot;

               --        protected body Prot is
               --           procedure Do_Something (Obj : in out Ctrl) is ...
               --        end Prot;

               --        procedure Finalize (Obj : in out Ctrl) is
               --        begin
               --           Prot.Do_Something (Obj);
               --        end Finalize;
               --     end Pkg;

               --  Since for the most part entities in package bodies depend on
               --  those in package specs, Prot's lock should be cleaned up
               --  first. The subsequent cleanup of the spec finalizes Lib_Obj.
               --  This act however attempts to invoke Do_Something and fails
               --  because the lock has disappeared.

               elsif Ekind (Obj_Id) = E_Variable
                 and then not In_Library_Level_Package_Body (Obj_Id)
                 and then Has_Simple_Protected_Object (Obj_Typ)
                 and then not Restricted_Profile
               then
                  Processing_Actions
                    (Decl, Is_Protected => True, Strict => True);
               end if;

            --  Inspect the freeze node of an access-to-controlled type and
            --  look for a delayed finalization collection. This case arises
            --  when the freeze actions are inserted at a later time than the
            --  expansion of the context. Since Build_Finalizer is never called
            --  on a single construct twice, the collection would be ultimately
            --  left out and never finalized. This is also needed for freeze
            --  actions of designated types themselves, since in some cases the
            --  finalization collection is associated with a designated type's
            --  freeze node rather than that of the access type (see handling
            --  for freeze actions in Build_Finalization_Collection).

            elsif Nkind (Decl) = N_Freeze_Entity
              and then Present (Actions (Decl))
            then
               Typ := Entity (Decl);

               --  Freeze nodes for ignored Ghost types do not need cleanup
               --  actions because they will never appear in the final tree.

               if Is_Ignored_Ghost_Entity (Typ) then
                  null;

               elsif (Is_Access_Object_Type (Typ)
                        and then Needs_Finalization
                                   (Available_View (Designated_Type (Typ))))
                      or else (Is_Type (Typ) and then Needs_Finalization (Typ))
               then
                  --  Freeze nodes are considered to be identical to packages
                  --  and blocks in terms of nesting. The difference is that
                  --  a finalization collection created inside the freeze node
                  --  is at the same nesting level as the node itself.

                  Process_Declarations (Actions (Decl), Preprocess);
               end if;

            --  Nested package declarations, avoid generics

            elsif Nkind (Decl) = N_Package_Declaration then
               Pack_Id := Defining_Entity (Decl);
               Spec    := Specification   (Decl);

               --  Do not inspect an ignored Ghost package because all code
               --  found within will not appear in the final tree.

               if Is_Ignored_Ghost_Entity (Pack_Id) then
                  null;

               elsif Ekind (Pack_Id) /= E_Generic_Package then
                  Process_Declarations
                    (Private_Declarations (Spec), Preprocess);
                  Process_Declarations
                    (Visible_Declarations (Spec), Preprocess);
               end if;

            --  Nested package bodies, avoid generics

            elsif Nkind (Decl) = N_Package_Body then
               Process_Package_Body (Decl);

            elsif Nkind (Decl) = N_Package_Body_Stub
              and then Present (Stub_Subunit (Decl))
            then
               Process_Package_Body
                 (Proper_Body (Unit (Stub_Subunit (Decl))));
            end if;

            Decl := Prev;
         end loop;
      end Process_Declarations;

      --------------------------------
      -- Process_Object_Declaration --
      --------------------------------

      procedure Process_Object_Declaration
        (Decl         : Node_Id;
         Is_Protected : Boolean := False)
      is
         Obj_Id : constant Entity_Id := Defining_Identifier (Decl);
         Loc    : constant Source_Ptr := Sloc (Decl);

         Fin_Call           : Node_Id;
         Fin_Id             : Entity_Id;
         Master_Node_Attach : Node_Id;
         Master_Node_Decl   : Node_Id;
         Master_Node_Id     : Entity_Id;
         Master_Node_Ins    : Node_Id;
         Master_Node_Loc    : Source_Ptr;
         Obj_Ref            : Node_Id;
         Obj_Typ            : Entity_Id;

      --  Start of processing for Process_Object_Declaration

      begin
         --  Handle the object type and the reference to the object. Note
         --  that objects having simple protected components or of a CW type
         --  must retain their original type for the processing below to work.

         Obj_Ref := New_Occurrence_Of (Obj_Id, Loc);
         Obj_Typ := Etype (Obj_Id);
         if not Is_Protected and then not Is_Class_Wide_Type (Obj_Typ) then
            Obj_Typ := Base_Type (Obj_Typ);
         end if;

         if Is_Access_Type (Obj_Typ) then
            Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);
            Obj_Typ := Available_View (Designated_Type (Obj_Typ));
         end if;

         --  If the object is a Master_Node, then nothing to do, unless there
         --  is no or a single controlled object with strict semantics, in
         --  which case we move its declaration, call marker (if any) and
         --  initialization call, and also mark it to avoid double processing.

         if Is_RTE (Obj_Typ, RE_Master_Node) then
            Master_Node_Id := Obj_Id;

            if not Has_Strict_Ctrl_Objs or else Count = 1 then
               if Nkind (Next (Decl)) = N_Call_Marker then
                  Prepend_To (Decls, Remove_Next (Next (Decl)));
               end if;
               Prepend_To (Decls, Remove_Next (Decl));
               Remove (Decl);
               Prepend_To (Decls, Decl);
               Set_Is_Ignored_For_Finalization (Obj_Id);
            end if;

         --  Create the declaration of the Master_Node for the object and
         --  insert it before the declaration of the object itself, unless
         --  there is no or a single controlled object with strict semantics,
         --  because it will effectively play the role of a degenerated master
         --  and therefore needs to be inserted at the same place the master
         --  would have been.

         else pragma Assert (No (Finalization_Master_Node (Obj_Id)));
            --  In the latter case, use the Sloc the master would have had

            if not Has_Strict_Ctrl_Objs or else Count = 1 then
               Master_Node_Loc := Sloc (N);
            else
               Master_Node_Loc := Loc;
            end if;

            Master_Node_Id :=
              Make_Defining_Identifier (Master_Node_Loc,
                Chars => New_External_Name (Chars (Obj_Id), Suffix => "MN"));
            Master_Node_Decl :=
              Make_Master_Node_Declaration (Master_Node_Loc,
                Master_Node_Id, Obj_Id);

            Push_Scope (Scope (Obj_Id));
            if not Has_Strict_Ctrl_Objs or else Count = 1 then
               Prepend_To (Decls, Master_Node_Decl);
            else
               Insert_Before (Decl, Master_Node_Decl);
            end if;
            Analyze (Master_Node_Decl);
            Pop_Scope;

            --  Mark the Master_Node to avoid double processing

            Set_Is_Ignored_For_Finalization (Master_Node_Id);
         end if;

         --  Attach the Master_Node after all initialization has been done. The
         --  place of insertion depends on the context.

         if Ekind (Obj_Id) in E_Constant | E_Variable then

            --  The object has delayed freezing. The Master_Node insertion
            --  point is after the freeze node.

            if Has_Delayed_Freeze (Obj_Id) then
               Master_Node_Ins := Freeze_Node (Obj_Id);

            --  The object is initialized by an aggregate. The Master_Node
            --  insertion point is after the last aggregate assignment.

            elsif Present (Last_Aggregate_Assignment (Obj_Id)) then
               Master_Node_Ins := Last_Aggregate_Assignment (Obj_Id);

            --  The object is initialized by a build-in-place function call.
            --  The Master_Node insertion point is after the function call.

            elsif Present (BIP_Initialization_Call (Obj_Id)) then
               Master_Node_Ins := BIP_Initialization_Call (Obj_Id);

            --  In other cases the Master_Node is inserted after the last call
            --  to either [Deep_]Initialize or the type-specific init proc.

            else
               Master_Node_Ins := Find_Last_Init (Decl);
            end if;

         --  In all other cases the Master_Node is inserted after the last call
         --  to either [Deep_]Initialize or the type-specific init proc.

         else
            Master_Node_Ins := Find_Last_Init (Decl);
         end if;

         --  If the Initialize function is null or trivial, the call will have
         --  been replaced with a null statement and we place the attachment
         --  of the Master_Node after the declaration of the object itself.

         if No (Master_Node_Ins) then
            Master_Node_Ins := Decl;
         end if;

         --  Processing for simple protected objects. Such objects require
         --  manual finalization of their lock managers. Generate:

         --    procedure obj_typ_nnFD (v : system__address) is
         --       type Ptr_Typ is access all Obj_Typ;
         --       Rnn : Obj_Typ renames Ptr_Typ!(v).all;
         --    begin
         --       $system__tasking__protected_objects__finalize_protection
         --          (Obj_TypV!(Rnn)._object);
         --    exception
         --       when others =>
         --          null;
         --    end obj_typ_nnFD;

         if Is_Protected
           or else (Has_Simple_Protected_Object (Obj_Typ)
                     and then No (Finalize_Address (Obj_Typ)))
         then
            declare
               Param   : constant Entity_Id :=
                           Make_Defining_Identifier (Loc, Name_V);
               Ptr_Typ : constant Entity_Id := Make_Temporary (Loc, 'P');
               Ren_Id  : constant Entity_Id := Make_Temporary (Loc, 'R');
               Ren_Ref : constant Node_Id   := New_Occurrence_Of (Ren_Id, Loc);

               Fin_Body  : Node_Id;
               Fin_Call  : Node_Id;
               Fin_Stmts : List_Id := No_List;
               HSS       : Node_Id;

            begin
               Set_Etype (Ren_Ref, Obj_Typ);

               if Is_Simple_Protected_Type (Obj_Typ) then
                  Fin_Call := Cleanup_Protected_Object (Decl, Ren_Ref);

                  if Present (Fin_Call) then
                     Fin_Stmts := New_List (Fin_Call);
                  end if;

               elsif Is_Array_Type (Obj_Typ) then
                  Fin_Stmts := Cleanup_Array (Decl, Ren_Ref, Obj_Typ);

               else
                  Fin_Stmts := Cleanup_Record (Decl, Ren_Ref, Obj_Typ);
               end if;

               if No (Fin_Stmts) then
                  return;
               end if;

               HSS :=
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => Fin_Stmts);

               if Exceptions_OK then
                  Set_Exception_Handlers (HSS, New_List (
                    Make_Exception_Handler (Loc,
                      Exception_Choices => New_List (
                        Make_Others_Choice (Loc)),
                      Statements        => New_List (
                        Make_Null_Statement (Loc)))));
               end if;

               Fin_Id :=
                 Make_Defining_Identifier (Loc,
                   Make_TSS_Name_Local (Obj_Typ, TSS_Finalize_Address));

               Fin_Body :=
                 Make_Subprogram_Body (Loc,
                   Specification =>
                     Make_Procedure_Specification (Loc,
                       Defining_Unit_Name => Fin_Id,

                       Parameter_Specifications => New_List (
                         Make_Parameter_Specification (Loc,
                           Defining_Identifier => Param,
                           Parameter_Type =>
                             New_Occurrence_Of (RTE (RE_Address), Loc)))),

                   Declarations => New_List (
                     Make_Full_Type_Declaration (Loc,
                       Defining_Identifier => Ptr_Typ,
                       Type_Definition     =>
                         Make_Access_To_Object_Definition (Loc,
                           All_Present        => True,
                           Subtype_Indication =>
                             New_Occurrence_Of (Obj_Typ, Loc))),

                     Make_Object_Renaming_Declaration (Loc,
                       Defining_Identifier => Ren_Id,
                       Subtype_Mark        =>
                         New_Occurrence_Of (Obj_Typ, Loc),
                         Name              =>
                         Make_Explicit_Dereference (Loc,
                           Prefix =>
                             Unchecked_Convert_To
                               (Ptr_Typ, New_Occurrence_Of (Param, Loc))))),

                   Handled_Statement_Sequence => HSS);

               Push_Scope (Scope (Obj_Id));
               Insert_After_And_Analyze
                 (Master_Node_Ins, Fin_Body, Suppress => All_Checks);
               Pop_Scope;

               Master_Node_Ins := Fin_Body;
            end;

         --  If the object's subtype is an array that has a constrained first
         --  subtype and is not this first subtype, we need to build a special
         --  Finalize_Address primitive for the object's subtype because the
         --  Finalize_Address primitive of the base type has been tailored to
         --  the first subtype (see Make_Finalize_Address_Stmts). Generate:

         --    procedure obj_typ_nnFD (v : system__address) is
         --       type Ptr_Typ is access all Obj_Typ;
         --    begin
         --       obj_typBDF (Ptr_Typ!(v).all, f => true);
         --    end obj_typ_nnFD;

         elsif Is_Array_Type (Etype (Obj_Id))
           and then Is_Constrained (First_Subtype (Etype (Obj_Id)))
           and then Etype (Obj_Id) /= First_Subtype (Etype (Obj_Id))
         then
            declare
               Ptr_Typ   : constant Node_Id   := Make_Temporary (Loc, 'P');
               Param     : constant Entity_Id :=
                             Make_Defining_Identifier (Loc, Name_V);

               Fin_Body  : Node_Id;

            begin
               Obj_Typ := Etype (Obj_Id);

               Fin_Id :=
                 Make_Defining_Identifier (Loc,
                   Make_TSS_Name_Local
                     (Obj_Typ, TSS_Finalize_Address));

               Fin_Body :=
                 Make_Subprogram_Body (Loc,
                   Specification =>
                     Make_Procedure_Specification (Loc,
                       Defining_Unit_Name       => Fin_Id,
                       Parameter_Specifications => New_List (
                         Make_Parameter_Specification (Loc,
                           Defining_Identifier => Param,
                           Parameter_Type      =>
                             New_Occurrence_Of (RTE (RE_Address), Loc)))),

                   Declarations => New_List (
                     Make_Full_Type_Declaration (Loc,
                       Defining_Identifier => Ptr_Typ,
                       Type_Definition     =>
                         Make_Access_To_Object_Definition (Loc,
                           All_Present        => True,
                           Subtype_Indication =>
                             New_Occurrence_Of (Obj_Typ, Loc)))),

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Final_Call (
                           Obj_Ref =>
                             Make_Explicit_Dereference (Loc,
                               Prefix =>
                                 Unchecked_Convert_To (Ptr_Typ,
                                   Make_Identifier (Loc, Name_V))),
                           Typ     => Obj_Typ))));

               Push_Scope (Scope (Obj_Id));
               Insert_After_And_Analyze
                 (Master_Node_Ins, Fin_Body, Suppress => All_Checks);
               Pop_Scope;

               Master_Node_Ins := Fin_Body;
            end;

         else
            Fin_Id := Finalize_Address (Obj_Typ);

            if No (Fin_Id) and then Ekind (Obj_Typ) = E_Class_Wide_Subtype then
               Fin_Id := TSS (Obj_Typ, TSS_Finalize_Address);
            end if;
         end if;

         --  Now build the attachment call that will initialize the object's
         --  Master_Node using the object's address and type's finalization
         --  procedure and then attach the Master_Node to the master, unless
         --  there is no or a single controlled object with strict semantics.

         if not Has_Strict_Ctrl_Objs or else Count = 1 then
            --  Finalize_Address is not generated in CodePeer mode because the
            --  body contains address arithmetic. So we don't want to generate
            --  the attach in this case. Ditto if the object is a Master_Node.

            if CodePeer_Mode or else Obj_Id = Master_Node_Id then
               Master_Node_Attach := Make_Null_Statement (Loc);

            else
               Master_Node_Attach :=
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Attach_Object_To_Node), Loc),
                   Parameter_Associations => New_List (
                     Make_Address_For_Finalize (Loc, Obj_Ref, Obj_Typ),
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Fin_Id, Loc),
                       Attribute_Name => Name_Unrestricted_Access),
                     New_Occurrence_Of (Master_Node_Id, Loc)));

               Set_Finalize_Address_For_Node (Master_Node_Id, Fin_Id);
            end if;

            --  We also generate the direct finalization call here

            Fin_Call := Make_Finalize_Call_For_Node (Loc, Master_Node_Id);

            --  For CodePeer, the exception handlers normally generated here
            --  generate complex flowgraphs which result in capacity problems.
            --  Omitting these handlers for CodePeer is justified as follows:

            --    If a handler is dead, then omitting it is surely ok

            --    If a handler is live, then CodePeer should flag the
            --      potentially-exception-raising construct that causes it
            --      to be live. That is what we are interested in, not what
            --      happens after the exception is raised.

            if Has_Strict_Ctrl_Objs
              and then Exceptions_OK
              and then not CodePeer_Mode
            then
               Fin_Call :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (Fin_Call),

                    Exception_Handlers => New_List (
                      Build_Exception_Handler
                        (Finalizer_Data, For_Package))));
            end if;

            Append_To (Finalizer_Stmts, Fin_Call);

         else
            --  If the object is a Master_Node, we just need to chain it

            if Obj_Id = Master_Node_Id then
               Master_Node_Attach :=
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Chain_Node_To_Master), Loc),
                     Parameter_Associations => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Obj_Id, Loc),
                       Attribute_Name => Name_Unrestricted_Access),
                     New_Occurrence_Of (Finalization_Master, Loc)));

            --  Finalize_Address is not generated in CodePeer mode because the
            --  body contains address arithmetic. So we don't want to generate
            --  the attach in this case.

            elsif CodePeer_Mode then
               Master_Node_Attach := Make_Null_Statement (Loc);

            else
               Master_Node_Attach :=
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Attach_Object_To_Master), Loc),
                   Parameter_Associations => New_List (
                     Make_Address_For_Finalize (Loc, Obj_Ref, Obj_Typ),
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Fin_Id, Loc),
                       Attribute_Name => Name_Unrestricted_Access),
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Master_Node_Id, Loc),
                       Attribute_Name => Name_Unrestricted_Access),
                     New_Occurrence_Of (Finalization_Master, Loc)));
            end if;
         end if;

         Insert_After_And_Analyze
           (Master_Node_Ins, Master_Node_Attach, Suppress => All_Checks);
      end Process_Object_Declaration;

      -------------------------------------
      -- Process_Tagged_Type_Declaration --
      -------------------------------------

      procedure Process_Tagged_Type_Declaration (Decl : Node_Id) is
         Typ    : constant Entity_Id := Defining_Identifier (Decl);
         DT_Ptr : constant Entity_Id :=
                    Node (First_Elmt (Access_Disp_Table (Typ)));
      begin
         --  Generate:
         --    Ada.Tags.Unregister_Tag (<Typ>P);

         Append_To (Tagged_Type_Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Unregister_Tag), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (DT_Ptr, Loc))));
      end Process_Tagged_Type_Declaration;

   --  Start of processing for Build_Finalizer

   begin
      Fin_Id := Empty;

      --  Do not perform this expansion in SPARK mode because it is not
      --  necessary.

      if GNATprove_Mode then
         return;
      end if;

      --  Step 1: Extract all lists which may contain controlled objects or
      --  library-level tagged types.

      if For_Package_Spec then
         Decls      := Visible_Declarations (Specification (N));
         Priv_Decls := Private_Declarations (Specification (N));

         --  Retrieve the package spec id

         Spec_Id := Defining_Unit_Name (Specification (N));

         if Nkind (Spec_Id) = N_Defining_Program_Unit_Name then
            Spec_Id := Defining_Identifier (Spec_Id);
         end if;

      --  Accept statement, block, entry body, package body, protected body,
      --  subprogram body or task body.

      else
         Decls := Declarations (N);
         HSS   := Handled_Statement_Sequence (N);

         if Present (HSS) then
            if Present (Statements (HSS)) then
               Stmts := Statements (HSS);
            end if;

            if Present (At_End_Proc (HSS)) then
               Prev_At_End := At_End_Proc (HSS);
            end if;
         end if;

         --  Retrieve the package spec id for package bodies

         if For_Package_Body then
            Spec_Id := Corresponding_Spec (N);
         end if;
      end if;

      --  We do not need to process nested packages since they are handled by
      --  the finalizer of the enclosing scope, including at library level.
      --  And we do not build two finalizers for an instance without body that
      --  is a library unit (see Analyze_Package_Instantiation).

      if For_Package
        and then (not Is_Compilation_Unit (Spec_Id)
                   or else (Is_Generic_Instance (Spec_Id)
                             and then Package_Instantiation (Spec_Id) = N))
      then
         return;
      end if;

      --  Step 2: Object [pre]processing

      if For_Package then
         --  For package specs and bodies, we are invoked from the Standard
         --  scope, so we need to push the specs onto the scope stack first.

         Push_Scope (Spec_Id);

         --  Preprocess the visible declarations now in order to obtain the
         --  correct number of controlled object by the time the private
         --  declarations are processed.

         Process_Declarations (Decls, Preprocess => True);

         --  From all the possible contexts, only package specifications may
         --  have private declarations.

         if For_Package_Spec then
            Process_Declarations (Priv_Decls, Preprocess => True);
         end if;

         --  The current context may lack controlled objects, but require some
         --  other form of completion (task termination for instance). In such
         --  cases, the finalizer must be created and carry the additional
         --  statements.

         if Acts_As_Clean or else Has_Ctrl_Objs or else Has_Tagged_Types then
            Build_Components;
         end if;

         --  The preprocessing has determined that the context has controlled
         --  objects or library-level tagged types.

         if Has_Ctrl_Objs or else Has_Tagged_Types then

            --  Private declarations are processed first in order to preserve
            --  possible dependencies between public and private objects.

            if For_Package_Spec then
               Process_Declarations (Priv_Decls);
            end if;

            Process_Declarations (Decls);
         end if;

      --  Non-package case

      else
         --  Preprocess both declarations and statements

         Process_Declarations (Decls, Preprocess => True);
         Process_Declarations (Stmts, Preprocess => True);

         --  At this point it is known that N has controlled objects. Ensure
         --  that N has a declarative list since the finalizer spec will be
         --  attached to it.

         if Has_Ctrl_Objs and then No (Decls) then
            Set_Declarations (N, New_List);
            Decls := Declarations (N);
         end if;

         --  The current context may lack controlled objects, but require some
         --  other form of completion (task termination for instance). In such
         --  cases, the finalizer must be created and carry the additional
         --  statements.

         if Acts_As_Clean or else Has_Ctrl_Objs or else Has_Tagged_Types then
            Build_Components;
         end if;

         if Has_Ctrl_Objs or else Has_Tagged_Types then
            Process_Declarations (Stmts);
            Process_Declarations (Decls);
         end if;
      end if;

      --  Step 3: Finalizer creation

      if Acts_As_Clean or else Has_Ctrl_Objs or else Has_Tagged_Types then
         Create_Finalizer;
      end if;

      --  Pop the scope that was pushed above for package specs and bodies

      if For_Package then
         Pop_Scope;
      end if;
   end Build_Finalizer;

   --------------------------
   -- Build_Finalizer_Call --
   --------------------------

   procedure Build_Finalizer_Call (N : Node_Id; Fin_Id : Entity_Id) is
   begin
      --  Do not perform this expansion in SPARK mode because we do not create
      --  finalizers in the first place.

      if GNATprove_Mode then
         return;
      end if;

      --  If the construct to be cleaned up is a protected subprogram body, the
      --  finalizer call needs to be associated with the block that wraps the
      --  unprotected version of the subprogram. The following illustrates this
      --  scenario:

      --     procedure Prot_SubpP is
      --        procedure finalizer is
      --        begin
      --           Service_Entries (Prot_Obj);
      --           Abort_Undefer;
      --        end finalizer;

      --     begin
      --        . . .
      --        begin
      --           Prot_SubpN (Prot_Obj);
      --        at end
      --           finalizer;
      --        end;
      --     end Prot_SubpP;

      declare
         Loc : constant Source_Ptr := Sloc (N);

         Is_Protected_Subp_Body : constant Boolean :=
           Nkind (N) = N_Subprogram_Body
           and then Is_Protected_Subprogram_Body (N);
         --  True if N is the protected version of a subprogram that belongs to
         --  a protected type.

         HSS : constant Node_Id :=
           (if Is_Protected_Subp_Body
             then Handled_Statement_Sequence
               (Last (Statements (Handled_Statement_Sequence (N))))
             else Handled_Statement_Sequence (N));

         --  We attach the At_End_Proc to the HSS if this is an accept
         --  statement or extended return statement. Also in the case of
         --  a protected subprogram, because if Service_Entries raises an
         --  exception, we do not lock the PO, so we also do not want to
         --  unlock it.

         Use_HSS : constant Boolean :=
           Nkind (N) in N_Accept_Statement | N_Extended_Return_Statement
           or else Is_Protected_Subp_Body;

         At_End_Proc_Bearer : constant Node_Id := (if Use_HSS then HSS else N);
      begin
         pragma Assert (No (At_End_Proc (At_End_Proc_Bearer)));
         Set_At_End_Proc (At_End_Proc_Bearer, New_Occurrence_Of (Fin_Id, Loc));
         --  Attach reference to finalizer to tree, for LLVM use
         Set_Parent (At_End_Proc (At_End_Proc_Bearer), At_End_Proc_Bearer);
         Analyze (At_End_Proc (At_End_Proc_Bearer));
         Expand_At_End_Handler (At_End_Proc_Bearer, Empty);
      end;
   end Build_Finalizer_Call;

   ---------------------
   -- Build_Late_Proc --
   ---------------------

   procedure Build_Late_Proc (Typ : Entity_Id; Nam : Name_Id) is
   begin
      for Final_Prim in Name_Of'Range loop
         if Name_Of (Final_Prim) = Nam then
            Set_TSS (Typ,
              Make_Deep_Proc
                (Prim  => Final_Prim,
                 Typ   => Typ,
                 Stmts => Make_Deep_Record_Body (Final_Prim, Typ)));
         end if;
      end loop;
   end Build_Late_Proc;

   -------------------------------
   -- Build_Object_Declarations --
   -------------------------------

   procedure Build_Object_Declarations
     (Data        : out Finalization_Exception_Data;
      Decls       : List_Id;
      Loc         : Source_Ptr;
      For_Package : Boolean := False)
   is
      Decl : Node_Id;

      Dummy : Entity_Id;
      --  This variable captures an unused dummy internal entity, see the
      --  comment associated with its use.

   begin
      pragma Assert (Decls /= No_List);

      --  Always set the proper location as it may be needed even when
      --  exception propagation is forbidden.

      Data.Loc := Loc;

      if Restriction_Active (No_Exception_Propagation) then
         Data.Abort_Id  := Empty;
         Data.E_Id      := Empty;
         Data.Raised_Id := Empty;
         return;
      end if;

      Data.Raised_Id := Make_Temporary (Loc, 'R');

      --  In certain scenarios, finalization can be triggered by an abort. If
      --  the finalization itself fails and raises an exception, the resulting
      --  Program_Error must be supressed and replaced by an abort signal. In
      --  order to detect this scenario, save the state of entry into the
      --  finalization code.

      --  This is not needed for library-level finalizers as they are called by
      --  the environment task and cannot be aborted.

      if not For_Package then
         if Abort_Allowed then
            Data.Abort_Id := Make_Temporary (Loc, 'A');

            --  Generate:
            --    Abort_Id : constant Boolean := <A_Expr>;

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Data.Abort_Id,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Boolean, Loc),
                Expression          =>
                  New_Occurrence_Of (RTE (RE_Triggered_By_Abort), Loc)));

         --  Abort is not required

         else
            --  Generate a dummy entity to ensure that the internal symbols are
            --  in sync when a unit is compiled with and without aborts.

            Dummy := Make_Temporary (Loc, 'A');
            Data.Abort_Id := Empty;
         end if;

      --  Library-level finalizers

      else
         Data.Abort_Id := Empty;
      end if;

      if Exception_Extra_Info then
         Data.E_Id := Make_Temporary (Loc, 'E');

         --  Generate:
         --    E_Id : Exception_Occurrence;

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Data.E_Id,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc));
         Set_No_Initialization (Decl);

         Append_To (Decls, Decl);

      else
         Data.E_Id := Empty;
      end if;

      --  Generate:
      --    Raised_Id : Boolean := False;

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Data.Raised_Id,
          Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_False, Loc)));

      if Debug_Generated_Code then
         Set_Debug_Info_Needed (Data.Raised_Id);
      end if;
   end Build_Object_Declarations;

   ---------------------------
   -- Build_Raise_Statement --
   ---------------------------

   function Build_Raise_Statement
     (Data : Finalization_Exception_Data) return Node_Id
   is
      Stmt : Node_Id;
      Expr : Node_Id;

   begin
      --  Standard run-time use the specialized routine
      --  Raise_From_Controlled_Operation.

      if Exception_Extra_Info
        and then RTE_Available (RE_Raise_From_Controlled_Operation)
      then
         Stmt :=
           Make_Procedure_Call_Statement (Data.Loc,
              Name                   =>
                New_Occurrence_Of
                  (RTE (RE_Raise_From_Controlled_Operation), Data.Loc),
              Parameter_Associations =>
                New_List (New_Occurrence_Of (Data.E_Id, Data.Loc)));

      --  Restricted run-time: exception messages are not supported and hence
      --  Raise_From_Controlled_Operation is not supported. Raise Program_Error
      --  instead.

      else
         Stmt :=
           Make_Raise_Program_Error (Data.Loc,
             Reason => PE_Finalize_Raised_Exception);
      end if;

      --  Generate:

      --    Raised_Id and then not Abort_Id
      --      <or>
      --    Raised_Id

      Expr := New_Occurrence_Of (Data.Raised_Id, Data.Loc);

      if Present (Data.Abort_Id) then
         Expr := Make_And_Then (Data.Loc,
           Left_Opnd  => Expr,
           Right_Opnd =>
             Make_Op_Not (Data.Loc,
               Right_Opnd => New_Occurrence_Of (Data.Abort_Id, Data.Loc)));
      end if;

      --  Generate:

      --    if Raised_Id and then not Abort_Id then
      --       Raise_From_Controlled_Operation (E_Id);
      --         <or>
      --       raise Program_Error;  --  restricted runtime
      --    end if;

      return
        Make_If_Statement (Data.Loc,
          Condition       => Expr,
          Then_Statements => New_List (Stmt));
   end Build_Raise_Statement;

   -----------------------------
   -- Build_Record_Deep_Procs --
   -----------------------------

   procedure Build_Record_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc
          (Prim  => Initialize_Case,
           Typ   => Typ,
           Stmts => Make_Deep_Record_Body (Initialize_Case, Typ)));

      if not Is_Inherently_Limited_Type (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Adjust_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Record_Body (Adjust_Case, Typ)));
      end if;

      --  Do not generate Deep_Finalize and Finalize_Address if finalization is
      --  suppressed since these routine will not be used.

      if not Restriction_Active (No_Finalization) then
         Set_TSS (Typ,
           Make_Deep_Proc
             (Prim  => Finalize_Case,
              Typ   => Typ,
              Stmts => Make_Deep_Record_Body (Finalize_Case, Typ)));

         --  Create TSS primitive Finalize_Address (unless CodePeer_Mode)

         if not CodePeer_Mode then
            Set_TSS (Typ,
              Make_Deep_Proc
                (Prim  => Address_Case,
                 Typ   => Typ,
                 Stmts => Make_Deep_Record_Body (Address_Case, Typ)));
         end if;
      end if;
   end Build_Record_Deep_Procs;

   -------------------
   -- Cleanup_Array --
   -------------------

   function Cleanup_Array
     (N    : Node_Id;
      Obj  : Node_Id;
      Typ  : Entity_Id) return List_Id
   is
      Loc        : constant Source_Ptr := Sloc (N);
      Index_List : constant List_Id := New_List;

      function Free_Component return List_Id;
      --  Generate the code to finalize the task or protected  subcomponents
      --  of a single component of the array.

      function Free_One_Dimension (Dim : Int) return List_Id;
      --  Generate a loop over one dimension of the array

      --------------------
      -- Free_Component --
      --------------------

      function Free_Component return List_Id is
         Stmts : List_Id := New_List;
         Tsk   : Node_Id;
         C_Typ : constant Entity_Id := Component_Type (Typ);

      begin
         --  Component type is known to contain tasks or protected objects

         Tsk :=
           Make_Indexed_Component (Loc,
             Prefix        => Duplicate_Subexpr_No_Checks (Obj),
             Expressions   => Index_List);

         Set_Etype (Tsk, C_Typ);

         if Is_Task_Type (C_Typ) then
            Append_To (Stmts, Cleanup_Task (N, Tsk));

         elsif Is_Simple_Protected_Type (C_Typ) then
            Append_To (Stmts, Cleanup_Protected_Object (N, Tsk));

         elsif Is_Record_Type (C_Typ) then
            Stmts := Cleanup_Record (N, Tsk, C_Typ);

         elsif Is_Array_Type (C_Typ) then
            Stmts := Cleanup_Array (N, Tsk, C_Typ);
         end if;

         return Stmts;
      end Free_Component;

      ------------------------
      -- Free_One_Dimension --
      ------------------------

      function Free_One_Dimension (Dim : Int) return List_Id is
         Index : Entity_Id;

      begin
         if Dim > Number_Dimensions (Typ) then
            return Free_Component;

         --  Here we generate the required loop

         else
            Index := Make_Temporary (Loc, 'J');
            Append (New_Occurrence_Of (Index, Loc), Index_List);

            return New_List (
              Make_Implicit_Loop_Statement (N,
                Identifier       => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier         => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix          => Duplicate_Subexpr (Obj),
                            Attribute_Name  => Name_Range,
                            Expressions     => New_List (
                              Make_Integer_Literal (Loc, Dim))))),
                Statements       => Free_One_Dimension (Dim + 1)));
         end if;
      end Free_One_Dimension;

   --  Start of processing for Cleanup_Array

   begin
      return Free_One_Dimension (1);
   end Cleanup_Array;

   --------------------
   -- Cleanup_Record --
   --------------------

   function Cleanup_Record
     (N    : Node_Id;
      Obj  : Node_Id;
      Typ  : Entity_Id) return List_Id
   is
      Loc   : constant Source_Ptr := Sloc (N);
      Stmts : constant List_Id    := New_List;
      U_Typ : constant Entity_Id  := Underlying_Type (Typ);

      Comp : Entity_Id;
      Tsk  : Node_Id;

   begin
      if Has_Discriminants (U_Typ)
        and then Nkind (Parent (U_Typ)) = N_Full_Type_Declaration
        and then Nkind (Type_Definition (Parent (U_Typ))) = N_Record_Definition
        and then
          Present
            (Variant_Part (Component_List (Type_Definition (Parent (U_Typ)))))
      then
         --  For now, do not attempt to free a component that may appear in a
         --  variant, and instead issue a warning. Doing this "properly" would
         --  require building a case statement and would be quite a mess. Note
         --  that the RM only requires that free "work" for the case of a task
         --  access value, so already we go way beyond this in that we deal
         --  with the array case and non-discriminated record cases.

         Error_Msg_N
           ("task/protected object in variant record will not be freed??", N);
         return New_List (Make_Null_Statement (Loc));
      end if;

      Comp := First_Component (U_Typ);
      while Present (Comp) loop
         if Chars (Comp) /= Name_uParent
           and then (Has_Task (Etype (Comp))
             or else Has_Simple_Protected_Object (Etype (Comp)))
         then
            Tsk :=
              Make_Selected_Component (Loc,
                Prefix        => Duplicate_Subexpr_No_Checks (Obj),
                Selector_Name => New_Occurrence_Of (Comp, Loc));
            Set_Etype (Tsk, Etype (Comp));

            if Is_Task_Type (Etype (Comp)) then
               Append_To (Stmts, Cleanup_Task (N, Tsk));

            elsif Is_Simple_Protected_Type (Etype (Comp)) then
               Append_To (Stmts, Cleanup_Protected_Object (N, Tsk));

            elsif Is_Record_Type (Etype (Comp)) then

               --  Recurse, by generating the prefix of the argument to the
               --  eventual cleanup call.

               Append_List_To (Stmts, Cleanup_Record (N, Tsk, Etype (Comp)));

            elsif Is_Array_Type (Etype (Comp)) then
               Append_List_To (Stmts, Cleanup_Array (N, Tsk, Etype (Comp)));
            end if;
         end if;

         Next_Component (Comp);
      end loop;

      return Stmts;
   end Cleanup_Record;

   ------------------------------
   -- Cleanup_Protected_Object --
   ------------------------------

   function Cleanup_Protected_Object
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  For restricted run-time libraries (Ravenscar), tasks are
      --  non-terminating, and protected objects can only appear at library
      --  level, so we do not want finalization of protected objects.

      if Restricted_Profile then
         return Empty;

      else
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Finalize_Protection), Loc),
             Parameter_Associations => New_List (Concurrent_Ref (Ref)));
      end if;
   end Cleanup_Protected_Object;

   ------------------
   -- Cleanup_Task --
   ------------------

   function Cleanup_Task
     (N   : Node_Id;
      Ref : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  For restricted run-time libraries (Ravenscar), tasks are
      --  non-terminating and they can only appear at library level,
      --  so we do not want finalization of task objects.

      if Restricted_Profile then
         return Empty;

      else
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Free_Task), Loc),
             Parameter_Associations => New_List (Concurrent_Ref (Ref)));
      end if;
   end Cleanup_Task;

   --------------------------------------
   -- Check_Unnesting_Elaboration_Code --
   --------------------------------------

   procedure Check_Unnesting_Elaboration_Code (N : Node_Id) is
      Loc             : constant Source_Ptr := Sloc (N);
      Block_Elab_Proc : Entity_Id           := Empty;

      procedure Set_Block_Elab_Proc;
      --  Create a defining identifier for a procedure that will replace
      --  a block with nested subprograms (unless it has already been created,
      --  in which case this is a no-op).

      procedure Set_Block_Elab_Proc is
      begin
         if No (Block_Elab_Proc) then
            Block_Elab_Proc := Make_Temporary (Loc, 'I');
         end if;
      end Set_Block_Elab_Proc;

      procedure Reset_Scopes_To_Block_Elab_Proc (L : List_Id);
      --  Find entities in the elaboration code of a library package body that
      --  contain or represent a subprogram body. A body can appear within a
      --  block or a loop or can appear by itself if generated for an object
      --  declaration that involves controlled actions. The first such entity
      --  forces creation of a new procedure entity (via Set_Block_Elab_Proc)
      --  that will be used to reset the scopes of all entities that become
      --  local to the new elaboration procedure. This is needed for subsequent
      --  unnesting actions, which depend on proper setting of the Scope links
      --  to determine the nesting level of each subprogram.

      --------------------------------------
      --  Reset_Scopes_To_Block_Elab_Proc --
      --------------------------------------
      Maybe_Reset_Scopes_For_Decl : constant Elist_Id := New_Elmt_List;

      procedure Reset_Scopes_To_Block_Elab_Proc (L : List_Id) is
         Id   : Entity_Id;
         Stat : Node_Id;
         Node : Node_Id;

      begin
         Stat := First (L);
         while Present (Stat) loop
            case Nkind (Stat) is
               when N_Block_Statement =>
                  if Present (Identifier (Stat)) then
                     Id := Entity (Identifier (Stat));

                     --  The Scope of this block needs to be reset to the new
                     --  procedure if the block contains nested subprograms.

                     if Present (Id) and then Contains_Subprogram (Id) then
                        Set_Block_Elab_Proc;
                        Set_Scope (Id, Block_Elab_Proc);
                     end if;
                  end if;

               when N_Loop_Statement =>
                  Id := Entity (Identifier (Stat));

                  if Present (Id) and then Contains_Subprogram (Id) then
                     if Scope (Id) = Current_Scope then
                        Set_Block_Elab_Proc;
                        Set_Scope (Id, Block_Elab_Proc);
                     end if;
                  end if;

                  --  We traverse the loop's statements as well, which may
                  --  include other block (etc.) statements that need to have
                  --  their Scope set to Block_Elab_Proc. (Is this really the
                  --  case, or do such nested blocks refer to the loop scope
                  --  rather than the loop's enclosing scope???.)

                  Reset_Scopes_To_Block_Elab_Proc (Statements (Stat));

               when N_If_Statement =>
                  Reset_Scopes_To_Block_Elab_Proc (Then_Statements (Stat));
                  Reset_Scopes_To_Block_Elab_Proc (Else_Statements (Stat));

                  Node := First (Elsif_Parts (Stat));
                  while Present (Node) loop
                     Reset_Scopes_To_Block_Elab_Proc (Then_Statements (Node));
                     Next (Node);
                  end loop;

               when N_Case_Statement =>
                  Node := First (Alternatives (Stat));
                  while Present (Node) loop
                     Reset_Scopes_To_Block_Elab_Proc (Statements (Node));
                     Next (Node);
                  end loop;

               --  Reset the Scope of a subprogram and object declaration
               --  occurring at the top level

               when N_Subprogram_Body =>
                  Id := Defining_Entity (Stat);

                  Set_Block_Elab_Proc;
                  Set_Scope (Id, Block_Elab_Proc);

               when N_Object_Declaration
                 | N_Object_Renaming_Declaration =>
                  Id := Defining_Entity (Stat);
                  if No (Block_Elab_Proc) then
                     Append_Elmt (Id, Maybe_Reset_Scopes_For_Decl);
                  else
                     Set_Scope (Id, Block_Elab_Proc);
                  end if;

               when others =>
                  null;
            end case;

            Next (Stat);
         end loop;

         --  If we are creating an Elab procedure, move all the gathered
         --  declarations in its scope.

         if Present (Block_Elab_Proc) then
            while not Is_Empty_Elmt_List (Maybe_Reset_Scopes_For_Decl) loop
               Set_Scope
                 (Elists.Node
                   (Last_Elmt (Maybe_Reset_Scopes_For_Decl)), Block_Elab_Proc);
               Remove_Last_Elmt (Maybe_Reset_Scopes_For_Decl);
            end loop;
         end if;
      end Reset_Scopes_To_Block_Elab_Proc;

      --  Local variables

      H_Seq     : constant Node_Id := Handled_Statement_Sequence (N);
      Elab_Body : Node_Id;
      Elab_Call : Node_Id;

   --  Start of processing for Check_Unnesting_Elaboration_Code

   begin
      if Present (H_Seq) then
         Reset_Scopes_To_Block_Elab_Proc (Statements (H_Seq));

         --  There may be subprograms declared in the exception handlers
         --  of the current body.

         if Present (Exception_Handlers (H_Seq)) then
            declare
               Handler : Node_Id := First (Exception_Handlers (H_Seq));
            begin
               while Present (Handler) loop
                  Reset_Scopes_To_Block_Elab_Proc (Statements (Handler));

                  Next (Handler);
               end loop;
            end;
         end if;

         if Present (Block_Elab_Proc) then
            Elab_Body :=
              Make_Subprogram_Body (Loc,
                Specification              =>
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name => Block_Elab_Proc),
                Declarations               => New_List,
                Handled_Statement_Sequence =>
                  Relocate_Node (Handled_Statement_Sequence (N)));

            Elab_Call :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Block_Elab_Proc, Loc));

            Append_To (Declarations (N), Elab_Body);
            Analyze (Elab_Body);
            Set_Has_Nested_Subprogram (Block_Elab_Proc);

            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (Elab_Call)));

            Analyze (Elab_Call);

            --  Could we reset the scopes of entities associated with the new
            --  procedure here via a loop over entities rather than doing it in
            --  the recursive Reset_Scopes_To_Elab_Proc procedure???
         end if;
      end if;
   end Check_Unnesting_Elaboration_Code;

   ---------------------------------------
   -- Check_Unnesting_In_Decls_Or_Stmts --
   ---------------------------------------

   procedure Check_Unnesting_In_Decls_Or_Stmts (Decls_Or_Stmts : List_Id) is
      Decl_Or_Stmt : Node_Id;

   begin
      if Unnest_Subprogram_Mode
        and then Present (Decls_Or_Stmts)
      then
         Decl_Or_Stmt := First (Decls_Or_Stmts);
         while Present (Decl_Or_Stmt) loop
            if Nkind (Decl_Or_Stmt) = N_Block_Statement
              and then Contains_Subprogram (Entity (Identifier (Decl_Or_Stmt)))
            then
               Unnest_Block (Decl_Or_Stmt);

            --  If-statements may contain subprogram bodies at the outer level
            --  of their statement lists, and the subprograms may make up-level
            --  references (such as to objects declared in the same statement
            --  list). Unlike block and loop cases, however, we don't have an
            --  entity on which to test the Contains_Subprogram flag, so
            --  Unnest_If_Statement must traverse the statement lists to
            --  determine whether there are nested subprograms present.

            elsif Nkind (Decl_Or_Stmt) = N_If_Statement then
               Unnest_If_Statement (Decl_Or_Stmt);

            elsif Nkind (Decl_Or_Stmt) = N_Loop_Statement then
               declare
                  Id : constant Entity_Id :=
                         Entity (Identifier (Decl_Or_Stmt));

               begin
                  --  When a top-level loop within declarations of a library
                  --  package spec or body contains nested subprograms, we wrap
                  --  it in a procedure to handle possible up-level references
                  --  to entities associated with the loop (such as loop
                  --  parameters).

                  if Present (Id) and then Contains_Subprogram (Id) then
                     Unnest_Loop (Decl_Or_Stmt);
                  end if;
               end;

            elsif Nkind (Decl_Or_Stmt) = N_Package_Declaration then
               Check_Unnesting_In_Decls_Or_Stmts
                 (Visible_Declarations (Specification (Decl_Or_Stmt)));
               Check_Unnesting_In_Decls_Or_Stmts
                 (Private_Declarations (Specification (Decl_Or_Stmt)));

            elsif Nkind (Decl_Or_Stmt) = N_Package_Body then
               Check_Unnesting_In_Decls_Or_Stmts (Declarations (Decl_Or_Stmt));
               if Present (Statements
                    (Handled_Statement_Sequence (Decl_Or_Stmt)))
               then
                  Check_Unnesting_In_Decls_Or_Stmts (Statements
                    (Handled_Statement_Sequence (Decl_Or_Stmt)));
                  Check_Unnesting_In_Handlers (Decl_Or_Stmt);
               end if;
            end if;

            Next (Decl_Or_Stmt);
         end loop;
      end if;
   end Check_Unnesting_In_Decls_Or_Stmts;

   ---------------------------------
   -- Check_Unnesting_In_Handlers --
   ---------------------------------

   procedure Check_Unnesting_In_Handlers (N : Node_Id) is
      Stmt_Seq : constant Node_Id := Handled_Statement_Sequence (N);

   begin
      if Present (Stmt_Seq)
        and then Present (Exception_Handlers (Stmt_Seq))
      then
         declare
            Handler : Node_Id := First (Exception_Handlers (Stmt_Seq));
         begin
            while Present (Handler) loop
               if Present (Statements (Handler)) then
                  Check_Unnesting_In_Decls_Or_Stmts (Statements (Handler));
               end if;

               Next (Handler);
            end loop;
         end;
      end if;
   end Check_Unnesting_In_Handlers;

   ------------------------------
   -- Check_Visibly_Controlled --
   ------------------------------

   procedure Check_Visibly_Controlled
     (Prim : Final_Primitives;
      Typ  : Entity_Id;
      E    : in out Entity_Id;
      Cref : in out Node_Id)
   is
      Parent_Type : Entity_Id;
      Op          : Entity_Id;

   begin
      if Is_Derived_Type (Typ)
        and then Comes_From_Source (E)
        and then No (Overridden_Operation (E))
      then
         --  We know that the explicit operation on the type does not override
         --  the inherited operation of the parent, and that the derivation
         --  is from a private type that is not visibly controlled.

         Parent_Type := Etype (Typ);
         Op := Find_Controlled_Prim_Op (Parent_Type, Name_Of (Prim));

         if Present (Op) then
            E := Op;

            --  Wrap the object to be initialized into the proper
            --  unchecked conversion, to be compatible with the operation
            --  to be called.

            if Nkind (Cref) = N_Unchecked_Type_Conversion then
               Cref := Unchecked_Convert_To (Parent_Type, Expression (Cref));
            else
               Cref := Unchecked_Convert_To (Parent_Type, Cref);
            end if;
         end if;
      end if;
   end Check_Visibly_Controlled;

   --------------------------
   --  Contains_Subprogram --
   --------------------------

   function Contains_Subprogram (Blk : Entity_Id) return Boolean is
      E : Entity_Id;

   begin
      E := First_Entity (Blk);

      --  The compiler may generate loops with a declare block containing
      --  nested procedures used for finalization. Recursively search for
      --  subprograms in such constructs.

      if Ekind (Blk) = E_Loop
        and then Parent_Kind (Blk) = N_Loop_Statement
      then
         declare
            Stmt : Node_Id := First (Statements (Parent (Blk)));
         begin
            while Present (Stmt) loop
               if Nkind (Stmt) = N_Block_Statement then
                  declare
                     Id : constant Entity_Id :=
                              Entity (Identifier (Stmt));
                  begin
                     if Contains_Subprogram (Id) then
                        return True;
                     end if;
                  end;
               end if;
               Next (Stmt);
            end loop;
         end;
      end if;

      while Present (E) loop
         if Is_Subprogram (E) then
            return True;

         elsif Ekind (E) in E_Block | E_Loop
           and then Contains_Subprogram (E)
         then
            return True;
         end if;

         Next_Entity (E);
      end loop;

      return False;
   end Contains_Subprogram;

   ------------------
   -- Convert_View --
   ------------------

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Typ  : Entity_Id) return Node_Id
   is
      Ftyp : constant Entity_Id := Etype (First_Formal (Proc));

      Atyp : Entity_Id;

   begin
      if Nkind (Arg) in N_Type_Conversion | N_Unchecked_Type_Conversion then
         Atyp := Entity (Subtype_Mark (Arg));
      elsif Present (Etype (Arg)) then
         Atyp := Etype (Arg);
      else
         Atyp := Typ;
      end if;

      if Is_Abstract_Subprogram (Proc) and then Is_Tagged_Type (Ftyp) then
         return Unchecked_Convert_To (Class_Wide_Type (Ftyp), Arg);

      elsif Present (Atyp)
        and then Atyp /= Ftyp
        and then (Is_Private_Type (Ftyp)
                   or else Is_Private_Type (Atyp)
                   or else Is_Private_Type (Base_Type (Atyp)))
        and then Implementation_Base_Type (Atyp) =
                 Implementation_Base_Type (Ftyp)
      then
         return Unchecked_Convert_To (Ftyp, Arg);

      --  If the argument is already a conversion, as generated by
      --  Make_Init_Call, set the target type to the type of the formal
      --  directly, to avoid spurious typing problems.

      elsif Nkind (Arg) in N_Unchecked_Type_Conversion | N_Type_Conversion
        and then not Is_Class_Wide_Type (Atyp)
      then
         Set_Subtype_Mark (Arg, New_Occurrence_Of (Ftyp, Sloc (Arg)));
         Set_Etype (Arg, Ftyp);
         return Arg;

      --  Otherwise, introduce a conversion when the designated object
      --  has a type derived from the formal of the controlled routine.

      elsif Is_Private_Type (Ftyp)
        and then Present (Atyp)
        and then Is_Derived_Type (Underlying_Type (Base_Type (Atyp)))
      then
         return Unchecked_Convert_To (Ftyp, Arg);

      else
         return Arg;
      end if;
   end Convert_View;

   -------------------------------
   -- Establish_Transient_Scope --
   -------------------------------

   --  This procedure is called each time a transient block has to be inserted
   --  that is to say for each call to a function with unconstrained or tagged
   --  result. It creates a new scope on the scope stack in order to enclose
   --  all transient variables generated.

   procedure Establish_Transient_Scope
     (N                : Node_Id;
      Manage_Sec_Stack : Boolean)
   is
      function Is_Package_Or_Subprogram (Id : Entity_Id) return Boolean;
      --  Determine whether arbitrary Id denotes a package or subprogram [body]

      function Find_Enclosing_Transient_Scope return Int;
      --  Examine the scope stack looking for the nearest enclosing transient
      --  scope within the innermost enclosing package or subprogram. Return
      --  its index in the table or else -1 if no such scope exists.

      function Find_Transient_Context (N : Node_Id) return Node_Id;
      --  Locate a suitable context for arbitrary node N which may need to be
      --  serviced by a transient scope. Return Empty if no suitable context
      --  is available.

      procedure Delegate_Sec_Stack_Management;
      --  Move the management of the secondary stack to the nearest enclosing
      --  suitable scope.

      procedure Create_Transient_Scope (Context : Node_Id);
      --  Place a new scope on the scope stack in order to service construct
      --  Context. Context is the node found by Find_Transient_Context. The
      --  new scope may also manage the secondary stack.

      ----------------------------
      -- Create_Transient_Scope --
      ----------------------------

      procedure Create_Transient_Scope (Context : Node_Id) is
         Loc : constant Source_Ptr := Sloc (N);

         Iter_Loop  : Entity_Id;
         Trans_Scop : constant Entity_Id :=
           New_Internal_Entity (E_Block, Current_Scope, Loc, 'B');

      begin
         Set_Etype (Trans_Scop, Standard_Void_Type);

         --  Push a new scope, and set its Node_To_Be_Wrapped and Is_Transient
         --  fields.

         Push_Scope (Trans_Scop);
         Scope_Stack.Table (Scope_Stack.Last).Node_To_Be_Wrapped := Context;
         Scope_Stack.Table (Scope_Stack.Last).Is_Transient := True;

         --  The transient scope must also manage the secondary stack

         if Manage_Sec_Stack then
            Set_Uses_Sec_Stack (Trans_Scop);
            Check_Restriction (No_Secondary_Stack, N);

            --  The expansion of iterator loops generates references to objects
            --  in order to extract elements from a container:

            --    Ref : Reference_Type_Ptr := Reference (Container, Cursor);
            --    Obj : <object type> renames Ref.all.Element.all;

            --  These references are controlled and returned on the secondary
            --  stack. A new reference is created at each iteration of the loop
            --  and as a result it must be finalized and the space occupied by
            --  it on the secondary stack reclaimed at the end of the current
            --  iteration.

            --  When the context that requires a transient scope is a call to
            --  routine Reference, the node to be wrapped is the source object:

            --    for Obj of Container loop

            --  Routine Wrap_Transient_Declaration however does not generate
            --  a physical block as wrapping a declaration will kill it too
            --  early. To handle this peculiar case, mark the related iterator
            --  loop as requiring the secondary stack. This signals the
            --  finalization machinery to manage the secondary stack (see
            --  routine Process_Statements_For_Controlled_Objects).

            Iter_Loop := Find_Enclosing_Iterator_Loop (Trans_Scop);

            if Present (Iter_Loop) then
               Set_Uses_Sec_Stack (Iter_Loop);
            end if;
         end if;

         if Debug_Flag_W then
            Write_Str ("    <Transient>");
            Write_Eol;
         end if;
      end Create_Transient_Scope;

      -----------------------------------
      -- Delegate_Sec_Stack_Management --
      -----------------------------------

      procedure Delegate_Sec_Stack_Management is
      begin
         for Index in reverse Scope_Stack.First .. Scope_Stack.Last loop
            declare
               Scope : Scope_Stack_Entry renames Scope_Stack.Table (Index);
            begin
               --  Prevent the search from going too far or within the scope
               --  space of another unit.

               if Scope.Entity = Standard_Standard then
                  return;

               --  No transient scope should be encountered during the
               --  traversal because Establish_Transient_Scope should have
               --  already handled this case.

               elsif Scope.Is_Transient then
                  raise Program_Error;

               --  The construct that requires secondary stack management is
               --  always enclosed by a package or subprogram scope.

               elsif Is_Package_Or_Subprogram (Scope.Entity) then
                  Set_Uses_Sec_Stack (Scope.Entity);
                  Check_Restriction (No_Secondary_Stack, N);

                  return;
               end if;
            end;
         end loop;

         --  At this point no suitable scope was found. This should never occur
         --  because a construct is always enclosed by a compilation unit which
         --  has a scope.

         pragma Assert (False);
      end Delegate_Sec_Stack_Management;

      ------------------------------------
      -- Find_Enclosing_Transient_Scope --
      ------------------------------------

      function Find_Enclosing_Transient_Scope return Int is
      begin
         for Index in reverse Scope_Stack.First .. Scope_Stack.Last loop
            declare
               Scope : Scope_Stack_Entry renames Scope_Stack.Table (Index);
            begin
               --  Prevent the search from going too far or within the scope
               --  space of another unit.

               if Scope.Entity = Standard_Standard
                 or else Is_Package_Or_Subprogram (Scope.Entity)
               then
                  exit;

               elsif Scope.Is_Transient then
                  return Index;
               end if;
            end;
         end loop;

         return -1;
      end Find_Enclosing_Transient_Scope;

      ----------------------------
      -- Find_Transient_Context --
      ----------------------------

      function Find_Transient_Context (N : Node_Id) return Node_Id is
         Curr : Node_Id := N;
         Prev : Node_Id := Empty;

      begin
         while Present (Curr) loop
            case Nkind (Curr) is

               --  Declarations

               --  Declarations act as a boundary for a transient scope even if
               --  they are not wrapped, see Wrap_Transient_Declaration.

               when N_Object_Declaration
                  | N_Object_Renaming_Declaration
                  | N_Subtype_Declaration
               =>
                  return Curr;

               --  Statements

               --  Statements and statement-like constructs act as a boundary
               --  for a transient scope.

               when N_Accept_Alternative
                  | N_Attribute_Definition_Clause
                  | N_Case_Statement
                  | N_Case_Statement_Alternative
                  | N_Code_Statement
                  | N_Delay_Alternative
                  | N_Delay_Until_Statement
                  | N_Delay_Relative_Statement
                  | N_Discriminant_Association
                  | N_Elsif_Part
                  | N_Entry_Body_Formal_Part
                  | N_Exit_Statement
                  | N_If_Statement
                  | N_Iteration_Scheme
                  | N_Terminate_Alternative
               =>
                  pragma Assert (Present (Prev));
                  return Prev;

               when N_Assignment_Statement =>
                  return Curr;

               when N_Entry_Call_Statement
                  | N_Procedure_Call_Statement
               =>
                  --  When an entry or procedure call acts as the alternative
                  --  of a conditional or timed entry call, the proper context
                  --  is that of the alternative.

                  if Nkind (Parent (Curr)) = N_Entry_Call_Alternative
                    and then Nkind (Parent (Parent (Curr))) in
                               N_Conditional_Entry_Call | N_Timed_Entry_Call
                  then
                     return Parent (Parent (Curr));

                  --  General case for entry or procedure calls

                  else
                     return Curr;
                  end if;

               when N_Pragma =>

                  --  Pragma Check is not a valid transient context in
                  --  GNATprove mode because the pragma must remain unchanged.

                  if GNATprove_Mode
                    and then Get_Pragma_Id (Curr) = Pragma_Check
                  then
                     return Empty;

                  --  General case for pragmas

                  else
                     return Curr;
                  end if;

               when N_Raise_Statement =>
                  return Curr;

               when N_Simple_Return_Statement =>
                  declare
                     Fun_Id : constant Entity_Id :=
                       Return_Applies_To (Return_Statement_Entity (Curr));

                  begin
                     --  A transient context that must manage the secondary
                     --  stack cannot be a return statement of a function that
                     --  itself requires secondary stack management, because
                     --  the function's result would be reclaimed too early.
                     --  And returns of thunks never require transient scopes.

                     if (Manage_Sec_Stack
                          and then Needs_Secondary_Stack (Etype (Fun_Id)))
                       or else Is_Thunk (Fun_Id)
                     then
                        return Empty;

                     --  General case for return statements

                     else
                        return Curr;
                     end if;
                  end;

               --  Special

               when N_Attribute_Reference =>
                  if Is_Procedure_Attribute_Name (Attribute_Name (Curr)) then
                     return Curr;
                  end if;

               --  An Ada 2012 iterator specification is not a valid context
               --  because Analyze_Iterator_Specification already employs
               --  special processing for it.

               when N_Iterator_Specification =>
                  return Empty;

               when N_Loop_Parameter_Specification =>

                  --  An iteration scheme is not a valid context because
                  --  routine Analyze_Iteration_Scheme already employs
                  --  special processing.

                  if Nkind (Parent (Curr)) = N_Iteration_Scheme then
                     return Empty;
                  else
                     return Parent (Curr);
                  end if;

               --  Termination

               --  The following nodes represent "dummy contexts" which do not
               --  need to be wrapped.

               when N_Component_Declaration
                  | N_Discriminant_Specification
                  | N_Parameter_Specification
               =>
                  return Empty;

               --  If the traversal leaves a scope without having been able to
               --  find a construct to wrap, something is going wrong, but this
               --  can happen in error situations that are not detected yet
               --  (such as a dynamic string in a pragma Export).

               when N_Block_Statement
                  | N_Entry_Body
                  | N_Package_Body
                  | N_Package_Declaration
                  | N_Protected_Body
                  | N_Subprogram_Body
                  | N_Task_Body
               =>
                  return Empty;

               --  Default

               when others =>
                  null;
            end case;

            Prev := Curr;
            Curr := Parent (Curr);
         end loop;

         return Empty;
      end Find_Transient_Context;

      ------------------------------
      -- Is_Package_Or_Subprogram --
      ------------------------------

      function Is_Package_Or_Subprogram (Id : Entity_Id) return Boolean is
      begin
         return Ekind (Id) in E_Entry
                            | E_Entry_Family
                            | E_Function
                            | E_Package
                            | E_Procedure
                            | E_Subprogram_Body;
      end Is_Package_Or_Subprogram;

      --  Local variables

      Trans_Idx : constant Int := Find_Enclosing_Transient_Scope;
      Context   : Node_Id;

   --  Start of processing for Establish_Transient_Scope

   begin
      --  Do not create a new transient scope if there is already an enclosing
      --  transient scope within the innermost enclosing package or subprogram.

      if Trans_Idx >= 0 then

         --  If the transient scope was requested for purposes of managing the
         --  secondary stack, then the existing scope must perform this task,
         --  unless the node to be wrapped is a return statement of a function
         --  that requires secondary stack management, because the function's
         --  result would be reclaimed too early (see Find_Transient_Context).

         if Manage_Sec_Stack then
            declare
               SE : Scope_Stack_Entry renames Scope_Stack.Table (Trans_Idx);

            begin
               if Nkind (SE.Node_To_Be_Wrapped) /= N_Simple_Return_Statement
                 or else not
                   Needs_Secondary_Stack
                     (Etype
                       (Return_Applies_To
                         (Return_Statement_Entity (SE.Node_To_Be_Wrapped))))
               then
                  Set_Uses_Sec_Stack (SE.Entity);
               end if;
            end;
         end if;

         return;
      end if;

      --  Find the construct that must be serviced by a new transient scope, if
      --  it exists.

      Context := Find_Transient_Context (N);

      if Present (Context) then
         if Nkind (Context) = N_Assignment_Statement then

            --  An assignment statement with suppressed controlled semantics
            --  does not need a transient scope because finalization is not
            --  desirable at this point. Note that No_Ctrl_Actions is also
            --  set for non-controlled assignments to suppress dispatching
            --  _assign.

            if No_Ctrl_Actions (Context)
              and then Needs_Finalization (Etype (Name (Context)))
            then
               --  When a controlled component is initialized by a function
               --  call, the result on the secondary stack is always assigned
               --  to the component. Signal the nearest suitable scope that it
               --  is safe to manage the secondary stack.

               if Manage_Sec_Stack and then Within_Init_Proc then
                  Delegate_Sec_Stack_Management;
               end if;

            --  Otherwise the assignment is a normal transient context and thus
            --  requires a transient scope.

            else
               Create_Transient_Scope (Context);
            end if;

         --  General case

         else
            Create_Transient_Scope (Context);
         end if;
      end if;
   end Establish_Transient_Scope;

   ----------------------------
   -- Expand_Cleanup_Actions --
   ----------------------------

   procedure Expand_Cleanup_Actions (N : Node_Id) is
      pragma Assert
        (Nkind (N) in N_Block_Statement
                    | N_Subprogram_Body
                    | N_Task_Body
                    | N_Entry_Body
                    | N_Extended_Return_Statement);

      Scop : constant Entity_Id := Current_Scope;

      Is_Asynchronous_Call   : constant Boolean :=
                                 Nkind (N) = N_Block_Statement
                                   and then Is_Asynchronous_Call_Block (N);
      Is_Master              : constant Boolean :=
                                 Nkind (N) /= N_Extended_Return_Statement
                                   and then Nkind (N) /= N_Entry_Body
                                   and then Is_Task_Master (N);
      Is_Protected_Subp_Body : constant Boolean :=
                                 Nkind (N) = N_Subprogram_Body
                                   and then Is_Protected_Subprogram_Body (N);
      Is_Task_Allocation     : constant Boolean :=
                                 Nkind (N) = N_Block_Statement
                                   and then Is_Task_Allocation_Block (N);
      Is_Task_Body           : constant Boolean :=
                                 Nkind (Original_Node (N)) = N_Task_Body;

      --  We mark the secondary stack if it is used in this construct, and
      --  we're not returning a function result on the secondary stack, except
      --  that a build-in-place function that might or might not return on the
      --  secondary stack always needs a mark. A run-time test is required in
      --  the case where the build-in-place function has a BIP_Alloc extra
      --  parameter (see Create_Finalizer).

      Needs_Sec_Stack_Mark   : constant Boolean :=
                                   (Uses_Sec_Stack (Scop)
                                     and then
                                       not Sec_Stack_Needed_For_Return (Scop))
                                 or else
                                   (Is_Build_In_Place_Function (Scop)
                                     and then Needs_BIP_Alloc_Form (Scop));

      Needs_Custom_Cleanup   : constant Boolean :=
                                 Nkind (N) = N_Block_Statement
                                   and then Present (Cleanup_Actions (N));

      Actions_Required       : constant Boolean :=
                                 Requires_Cleanup_Actions (N, True)
                                   or else Is_Asynchronous_Call
                                   or else Is_Master
                                   or else Is_Protected_Subp_Body
                                   or else Is_Task_Allocation
                                   or else Is_Task_Body
                                   or else Needs_Sec_Stack_Mark
                                   or else Needs_Custom_Cleanup;

      Loc : Source_Ptr;
      Cln : List_Id;

   --  Start of processing for Expand_Cleanup_Actions

   begin
      --  The current construct does not need any form of servicing

      if not Actions_Required then
         return;
      end if;

      --  If an extended return statement contains something like
      --
      --     X := F (...);
      --
      --  where F is a build-in-place function call returning a controlled
      --  type, then a temporary object will be implicitly declared as part
      --  of the statement list, and this will need cleanup. In such cases,
      --  we transform:
      --
      --    return Result : T := ... do
      --       <statements> -- possibly with handlers
      --    end return;
      --
      --  into:
      --
      --    return Result : T := ... do
      --       declare -- no declarations
      --       begin
      --          <statements> -- possibly with handlers
      --       end; -- no handlers
      --    end return;
      --
      --  So Expand_Cleanup_Actions will end up being called recursively on the
      --  block statement.

      if Nkind (N) = N_Extended_Return_Statement then
         declare
            Block : constant Node_Id :=
                      Make_Block_Statement (Sloc (N),
                        Declarations               => Empty_List,
                        Handled_Statement_Sequence =>
                          Handled_Statement_Sequence (N));
         begin
            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Sloc (N),
                Statements => New_List (Block)));

            Analyze (Block);
         end;

         --  Analysis of the block did all the work

         return;
      end if;

      if Needs_Custom_Cleanup then
         Cln := Cleanup_Actions (N);
      else
         Cln := No_List;
      end if;

      if No (Declarations (N)) then
         Set_Declarations (N, New_List);
      end if;

      declare
         Fin_Id : Entity_Id;
         Mark   : Entity_Id := Empty;

      begin
         --  If we are generating expanded code for debugging purposes, use the
         --  Sloc of the point of insertion for the cleanup code. The Sloc will
         --  be updated subsequently to reference the proper line in .dg files.
         --  If we are not debugging generated code, use No_Location instead,
         --  so that no debug information is generated for the cleanup code.
         --  This makes the behavior of the NEXT command in GDB monotonic, and
         --  makes the placement of breakpoints more accurate.

         if Debug_Generated_Code then
            Loc := Sloc (Scop);
         else
            Loc := No_Location;
         end if;

         --  A task activation call has already been built for a task
         --  allocation block.

         if not Is_Task_Allocation then
            Build_Task_Activation_Call (N);
         end if;

         if Is_Master then
            Establish_Task_Master (N);
         end if;

         --  If secondary stack is in use, generate:
         --
         --    Mnn : constant Mark_Id := SS_Mark;

         if Needs_Sec_Stack_Mark then
            Set_Uses_Sec_Stack (Scop, False); -- avoid duplicate SS marks
            Mark := Make_Temporary (Loc, 'M');

            declare
               Mark_Call : constant Node_Id := Build_SS_Mark_Call (Loc, Mark);
            begin
               Prepend_To (Declarations (N), Mark_Call);
               Analyze (Mark_Call);
            end;
         end if;

         --  Generate finalization calls for all controlled objects appearing
         --  in the statements of N. Add context specific cleanup for various
         --  constructs.

         Build_Finalizer
           (N           => N,
            Clean_Stmts => Build_Cleanup_Statements (N, Cln),
            Mark_Id     => Mark,
            Defer_Abort => Nkind (Original_Node (N)) = N_Task_Body
                             or else Is_Master,
            Fin_Id      => Fin_Id);

         if Present (Fin_Id) then
            Build_Finalizer_Call (N, Fin_Id);
         end if;
      end;
   end Expand_Cleanup_Actions;

   ---------------------------
   -- Expand_N_Package_Body --
   ---------------------------

   --  Add call to Activate_Tasks if body is an activator (actual processing
   --  is in chapter 9).

   --  Generate subprogram descriptor for elaboration routine

   --  Encode entity names in package body

   procedure Expand_N_Package_Body (N : Node_Id) is
      Id      : constant Entity_Id := Defining_Entity (N);
      Spec_Id : constant Entity_Id := Corresponding_Spec (N);

      Fin_Id  : Entity_Id;

   begin
      --  This is done only for non-generic packages

      if Ekind (Spec_Id) = E_Package then
         --  Build dispatch tables of library-level tagged types for bodies
         --  that are not compilation units (see Analyze_Compilation_Unit),
         --  except for instances because they have no N_Compilation_Unit.

         if Tagged_Type_Expansion
           and then Is_Library_Level_Entity (Spec_Id)
           and then (not Is_Compilation_Unit (Spec_Id)
                      or else Is_Generic_Instance (Spec_Id))
         then
            Build_Static_Dispatch_Tables (N);
         end if;

         Push_Scope (Spec_Id);

         Expand_CUDA_Package (N);

         Build_Task_Activation_Call (N);

         --  Verify the run-time semantics of pragma Initial_Condition at the
         --  end of the body statements.

         Expand_Pragma_Initial_Condition (Spec_Id, N);

         --  If this is a library-level package and unnesting is enabled,
         --  check for the presence of blocks with nested subprograms occurring
         --  in elaboration code, and generate procedures to encapsulate the
         --  blocks in case the nested subprograms make up-level references.

         if Unnest_Subprogram_Mode
           and then
             Is_Library_Level_Entity (Current_Scope)
         then
            Check_Unnesting_Elaboration_Code (N);
            Check_Unnesting_In_Decls_Or_Stmts (Declarations (N));
            Check_Unnesting_In_Handlers (N);
         end if;

         Pop_Scope;
      end if;

      Set_Elaboration_Flag (N, Spec_Id);
      Set_In_Package_Body (Spec_Id, False);

      --  Set to encode entity names in package body before gigi is called

      Qualify_Entity_Names (N);

      if Ekind (Spec_Id) /= E_Generic_Package
        and then not Delay_Cleanups (Id)
      then
         Build_Finalizer
           (N           => N,
            Clean_Stmts => No_List,
            Mark_Id     => Empty,
            Defer_Abort => False,
            Fin_Id      => Fin_Id);

         if Present (Fin_Id) then
            Set_Finalizer (Defining_Entity (N), Fin_Id);
         end if;
      end if;
   end Expand_N_Package_Body;

   ----------------------------------
   -- Expand_N_Package_Declaration --
   ----------------------------------

   --  Add call to Activate_Tasks if there are tasks declared and the package
   --  has no body. Note that in Ada 83 this may result in premature activation
   --  of some tasks, given that we cannot tell whether a body will eventually
   --  appear.

   procedure Expand_N_Package_Declaration (N : Node_Id) is
      Id     : constant Entity_Id := Defining_Entity (N);
      Spec   : constant Node_Id   := Specification (N);
      Decls  : List_Id;
      Fin_Id : Entity_Id;

      No_Body : Boolean := False;
      --  True in the case of a package declaration that is a compilation
      --  unit and for which no associated body will be compiled in this
      --  compilation.

   begin
      --  Case of a package declaration other than a compilation unit

      if Nkind (Parent (N)) /= N_Compilation_Unit then
         null;

      --  Case of a compilation unit that does not require a body

      elsif not Body_Required (Parent (N))
        and then not Unit_Requires_Body (Id)
      then
         No_Body := True;

      --  Special case of generating calling stubs for a remote call interface
      --  package: even though the package declaration requires one, the body
      --  won't be processed in this compilation (so any stubs for RACWs
      --  declared in the package must be generated here, along with the spec).

      elsif Parent (N) = Cunit (Main_Unit)
        and then Is_Remote_Call_Interface (Id)
        and then Distribution_Stub_Mode = Generate_Caller_Stub_Body
      then
         No_Body := True;
      end if;

      --  For a nested instance, delay processing until freeze point

      if Has_Delayed_Freeze (Id)
        and then Nkind (Parent (N)) /= N_Compilation_Unit
      then
         return;
      end if;

      --  For a package declaration that implies no associated body, generate
      --  task activation call and RACW supporting bodies now (since we won't
      --  have a specific separate compilation unit for that).

      if No_Body then
         Push_Scope (Id);

         --  Generate RACW subprogram bodies

         if Has_RACW (Id) then
            Decls := Private_Declarations (Spec);

            if No (Decls) then
               Decls := Visible_Declarations (Spec);
            end if;

            if No (Decls) then
               Decls := New_List;
               Set_Visible_Declarations (Spec, Decls);
            end if;

            Append_RACW_Bodies (Decls, Id);
            Analyze_List (Decls);
         end if;

         --  Generate task activation call as last step of elaboration

         if Present (Activation_Chain_Entity (N)) then
            Build_Task_Activation_Call (N);
         end if;

         --  Verify the run-time semantics of pragma Initial_Condition at the
         --  end of the private declarations when the package lacks a body.

         Expand_Pragma_Initial_Condition (Id, N);

         Pop_Scope;
      end if;

      --  Build dispatch tables of library-level tagged types for instances
      --  that are not compilation units (see Analyze_Compilation_Unit).

      if Tagged_Type_Expansion
        and then Is_Library_Level_Entity (Id)
        and then Is_Generic_Instance (Id)
        and then not Is_Compilation_Unit (Id)
      then
         Build_Static_Dispatch_Tables (N);
      end if;

      --  Note: it is not necessary to worry about generating a subprogram
      --  descriptor, since the only way to get exception handlers into a
      --  package spec is to include instantiations, and that would cause
      --  generation of subprogram descriptors to be delayed in any case.

      --  Set to encode entity names in package spec before gigi is called

      Qualify_Entity_Names (N);

      if Ekind (Id) /= E_Generic_Package
        and then not Delay_Cleanups (Id)
      then
         Build_Finalizer
           (N           => N,
            Clean_Stmts => No_List,
            Mark_Id     => Empty,
            Defer_Abort => False,
            Fin_Id      => Fin_Id);

         if Present (Fin_Id) then
            Set_Finalizer (Id, Fin_Id);
         end if;
      end if;

      --  If this is a library-level package and unnesting is enabled,
      --  check for the presence of blocks with nested subprograms occurring
      --  in elaboration code, and generate procedures to encapsulate the
      --  blocks in case the nested subprograms make up-level references.

      if Unnest_Subprogram_Mode
        and then Is_Library_Level_Entity (Current_Scope)
      then
         Check_Unnesting_In_Decls_Or_Stmts (Visible_Declarations (Spec));
         Check_Unnesting_In_Decls_Or_Stmts (Private_Declarations (Spec));
      end if;
   end Expand_N_Package_Declaration;

   ---------------------------------
   -- Has_Simple_Protected_Object --
   ---------------------------------

   function Has_Simple_Protected_Object (T : Entity_Id) return Boolean is
   begin
      if Has_Task (T) then
         return False;

      elsif Is_Simple_Protected_Type (T) then
         return True;

      elsif Is_Array_Type (T) then
         return Has_Simple_Protected_Object (Component_Type (T));

      elsif Is_Record_Type (T) then
         declare
            Comp : Entity_Id;

         begin
            Comp := First_Component (T);
            while Present (Comp) loop
               if Has_Simple_Protected_Object (Etype (Comp)) then
                  return True;
               end if;

               Next_Component (Comp);
            end loop;

            return False;
         end;

      else
         return False;
      end if;
   end Has_Simple_Protected_Object;

   ------------------------------------
   -- Insert_Actions_In_Scope_Around --
   ------------------------------------

   procedure Insert_Actions_In_Scope_Around
     (N         : Node_Id;
      Clean     : Boolean;
      Manage_SS : Boolean)
   is
      Act_Before  : constant List_Id :=
        Scope_Stack.Table (Scope_Stack.Last).Actions_To_Be_Wrapped (Before);
      Act_After   : constant List_Id :=
        Scope_Stack.Table (Scope_Stack.Last).Actions_To_Be_Wrapped (After);
      Act_Cleanup : constant List_Id :=
        Scope_Stack.Table (Scope_Stack.Last).Actions_To_Be_Wrapped (Cleanup);
      --  Note: We used to use renamings of Scope_Stack.Table (Scope_Stack.
      --  Last), but this was incorrect as Process_Transients_In_Scope may
      --  introduce new scopes and cause a reallocation of Scope_Stack.Table.

      procedure Process_Transients_In_Scope
        (First_Object : Node_Id;
         Last_Object  : Node_Id;
         Related_Node : Node_Id);
      --  Find all transient objects in the list First_Object .. Last_Object
      --  and generate finalization actions for them. Related_Node denotes the
      --  node which created all transient objects.

      ---------------------------------
      -- Process_Transients_In_Scope --
      ---------------------------------

      procedure Process_Transients_In_Scope
        (First_Object : Node_Id;
         Last_Object  : Node_Id;
         Related_Node : Node_Id)
      is
         function Is_Subprogram_Call (N : Node_Id) return Traverse_Result;
         --  Return Abandon if arbitrary node denotes a subprogram call

         function Has_Subprogram_Call is
           new Traverse_Func (Is_Subprogram_Call);

         procedure Process_Transient_In_Scope
           (Obj_Decl    : Node_Id;
            Insert_Nod  : Node_Id;
            Must_Export : Boolean);
         --  Generate finalization actions for a single transient object
         --  denoted by object declaration Obj_Decl.

         ------------------------
         -- Is_Subprogram_Call --
         ------------------------

         function Is_Subprogram_Call (N : Node_Id) return Traverse_Result is
         begin
            --  A regular procedure or function call

            if Nkind (N) in N_Subprogram_Call then
               return Abandon;

            --  Special cases

            --  Heavy expansion may relocate function calls outside the related
            --  node. Inspect the original node to detect the initial placement
            --  of the call.

            elsif Is_Rewrite_Substitution (N) then
               return Has_Subprogram_Call (Original_Node (N));

            --  Generalized indexing always involves a function call

            elsif Nkind (N) = N_Indexed_Component
              and then Present (Generalized_Indexing (N))
            then
               return Abandon;

            --  Keep searching

            else
               return OK;
            end if;
         end Is_Subprogram_Call;

         --------------------------------
         -- Process_Transient_In_Scope --
         --------------------------------

         procedure Process_Transient_In_Scope
           (Obj_Decl    : Node_Id;
            Insert_Nod  : Node_Id;
            Must_Export : Boolean)
         is
            Loc    : constant Source_Ptr := Sloc (Obj_Decl);
            Obj_Id : constant Entity_Id  := Defining_Entity (Obj_Decl);

            Master_Node_Id   : Entity_Id;
            Master_Node_Decl : Node_Id;
            Obj_Ref          : Node_Id;
            Obj_Typ          : Entity_Id;

         begin
            --  If the object needs to be exported to the outer finalizer,
            --  create the declaration of the Master_Node for the object,
            --  which will later be picked up by Build_Finalizer.

            if Must_Export then
               Master_Node_Id := Make_Temporary (Loc, 'N');
               Master_Node_Decl :=
                 Make_Master_Node_Declaration (Loc, Master_Node_Id, Obj_Id);
               Insert_Before_And_Analyze (Obj_Decl, Master_Node_Decl);

               --  Generate the attachment of the object to the Master_Node

               Attach_Object_To_Master_Node (Obj_Decl, Master_Node_Id);

               --  Then add the finalization call for the object

               Insert_After_And_Analyze (Insert_Nod,
                 Make_Finalize_Call_For_Node (Loc, Master_Node_Id));

            --  Otherwise generate a direct finalization call for the object

            else
               --  Handle the object type and the reference to the object

               Obj_Ref := New_Occurrence_Of (Obj_Id, Loc);
               Obj_Typ := Base_Type (Etype (Obj_Id));

               if Is_Access_Type (Obj_Typ) then
                  Obj_Ref := Make_Explicit_Dereference (Loc, Obj_Ref);
                  Obj_Typ := Available_View (Designated_Type (Obj_Typ));
               end if;

               Insert_After_And_Analyze (Insert_Nod,
                 Make_Final_Call (Obj_Ref => Obj_Ref, Typ => Obj_Typ));
            end if;

            --  Mark the transient object to avoid double finalization

            Set_Is_Finalized_Transient (Obj_Id);
         end Process_Transient_In_Scope;

         --  Local variables

         Insert_Nod : Node_Id;
         --  Insertion node for the finalization actions

         Must_Export : Boolean;
         --  Flag denoting whether the context requires transient object
         --  export to the outer finalizer.

         Obj_Decl : Node_Id;

      --  Start of processing for Process_Transients_In_Scope

      begin
         --  The expansion performed by this routine is as follows:

         --    Ctrl_Trans_Obj_1MN : Master_Node;
         --    Ctrl_Trans_Obj_1 : ...;
         --    . . .
         --    Ctrl_Trans_Obj_NMN : Master_Node;
         --    Ctrl_Trans_Obj_N : ...;

         --    Finalize_Object (Ctrl_Trans_Obj_NMN);
         --    . . .
         --    Finalize_Object (Ctrl_Trans_Obj_1MN);

         --  Recognize a scenario where the transient context is an object
         --  declaration initialized by a build-in-place function call:

         --    Obj : ... := BIP_Function_Call (Ctrl_Func_Call);

         --  The rough expansion of the above is:

         --    Temp : ... := Ctrl_Func_Call;
         --    Obj  : ...;
         --    Res  : ... := BIP_Func_Call (..., Obj, ...);

         --  The finalization of any transient object must happen after the
         --  build-in-place function call is executed.

         if Nkind (N) = N_Object_Declaration
           and then Present (BIP_Initialization_Call (Defining_Identifier (N)))
         then
            Must_Export := True;
            Insert_Nod  := BIP_Initialization_Call (Defining_Identifier (N));

         --  Search the context for at least one subprogram call. If found, the
         --  machinery exports all transient objects to the enclosing finalizer
         --  due to the possibility of abnormal call termination.

         else
            Must_Export := Has_Subprogram_Call (N) = Abandon;
            Insert_Nod  := Last_Object;
         end if;

         Insert_List_After_And_Analyze (Insert_Nod, Act_Cleanup);

         --  Examine all the objects in the list First_Object .. Last_Object
         --  but skip the node to be wrapped because it is not transient as
         --  far as this scope is concerned.

         Obj_Decl := First_Object;
         while Present (Obj_Decl) loop
            if Obj_Decl /= Related_Node
              and then Nkind (Obj_Decl) = N_Object_Declaration
              and then Analyzed (Obj_Decl)
              and then Is_Finalizable_Transient (Obj_Decl, N)
            then
               Process_Transient_In_Scope (Obj_Decl, Insert_Nod, Must_Export);
            end if;

            exit when Obj_Decl = Last_Object;

            Next (Obj_Decl);
         end loop;
      end Process_Transients_In_Scope;

      --  Local variables

      Loc          : constant Source_Ptr := Sloc (N);
      Node_To_Wrap : constant Node_Id    := Node_To_Be_Wrapped;
      First_Obj    : Node_Id;
      Last_Obj     : Node_Id;
      Mark_Id      : Entity_Id;
      Marker       : Node_Id;
      Target       : Node_Id;

   --  Start of processing for Insert_Actions_In_Scope_Around

   begin
      --  Nothing to do if the scope does not manage the secondary stack or
      --  does not contain meaningful actions for insertion.

      if not Manage_SS
        and then No (Act_Before)
        and then No (Act_After)
        and then No (Act_Cleanup)
      then
         return;
      end if;

      --  If the node to be wrapped is the trigger of an asynchronous select,
      --  it is not part of a statement list. The actions must be inserted
      --  before the select itself, which is part of some list of statements.
      --  Note that the triggering alternative includes the triggering
      --  statement and an optional statement list. If the node to be
      --  wrapped is part of that list, the normal insertion applies.

      if Nkind (Parent (Node_To_Wrap)) = N_Triggering_Alternative
        and then not Is_List_Member (Node_To_Wrap)
      then
         Target := Parent (Parent (Node_To_Wrap));
      else
         Target := N;
      end if;

      --  Add all actions associated with a transient scope into the main tree.
      --  There are several scenarios here:

      --       +--- Before ----+        +----- After ---+
      --    1) First_Obj ....... Target ........ Last_Obj

      --    2) First_Obj ....... Target

      --    3)                   Target ........ Last_Obj

      --  Declarations are inserted before the target

      if Present (Act_Before) then
         First_Obj := First (Act_Before);
         Insert_List_Before (Target, Act_Before);
      else
         First_Obj := Target;
      end if;

      --  Set a marker on the next statement

      Marker := Next (Target);

      --  Finalization calls are inserted after the target

      if Present (Act_After) then
         Last_Obj := Last (Act_After);
         Insert_List_After (Target, Act_After);
      else
         Last_Obj := Target;
      end if;

      --  Mark and release the secondary stack when the context warrants it

      if Manage_SS then
         Mark_Id := Make_Temporary (Loc, 'M');

         --  Generate:
         --    Mnn : constant Mark_Id := SS_Mark;

         Insert_Before_And_Analyze
           (First_Obj, Build_SS_Mark_Call (Loc, Mark_Id));

         --  Generate:
         --    SS_Release (Mnn);

         Insert_After_And_Analyze
           (Last_Obj, Build_SS_Release_Call (Loc, Mark_Id));
      end if;

      --  If we are handling cleanups, check for transient objects associated
      --  with Target and generate the required finalization actions for them.

      if Clean then
         Process_Transients_In_Scope
           (First_Object => First_Obj,
            Last_Object  => Last_Obj,
            Related_Node => Target);
      end if;

      --  If the target is the declaration of an object with an address clause
      --  or aspect, move all the statements that have been inserted after it
      --  into its Initialization_Statements list, so they can be inserted into
      --  its freeze actions later.

      if Nkind (Target) = N_Object_Declaration
        and then (Present (Following_Address_Clause (Target))
                   or else
                  Has_Aspect (Defining_Identifier (Target), Aspect_Address))
        and then Next (Target) /= Marker
      then
         declare
            Obj_Id : constant Entity_Id := Defining_Identifier (Target);
            Stmts  : constant List_Id   := New_List;

         begin
            while Next (Target) /= Marker loop
               Append_To (Stmts, Remove_Next (Target));
            end loop;

            pragma Assert (No (Initialization_Statements (Obj_Id)));

            Set_Initialization_Statements
              (Obj_Id, Make_Compound_Statement (Loc, Actions => Stmts));
         end;
      end if;

      --  Reset the action lists

      Scope_Stack.Table
        (Scope_Stack.Last).Actions_To_Be_Wrapped (Before) := No_List;
      Scope_Stack.Table
        (Scope_Stack.Last).Actions_To_Be_Wrapped (After)  := No_List;

      if Clean then
         Scope_Stack.Table
           (Scope_Stack.Last).Actions_To_Be_Wrapped (Cleanup) := No_List;
      end if;
   end Insert_Actions_In_Scope_Around;

   ------------------------------
   -- Is_Simple_Protected_Type --
   ------------------------------

   function Is_Simple_Protected_Type (T : Entity_Id) return Boolean is
   begin
      return
        Is_Protected_Type (T)
          and then not Uses_Lock_Free (T)
          and then not Has_Entries (T)
          and then Is_RTE (Find_Protection_Type (T), RE_Protection);
   end Is_Simple_Protected_Type;

   -------------------------------
   -- Make_Address_For_Finalize --
   -------------------------------

   function Make_Address_For_Finalize
     (Loc     : Source_Ptr;
      Obj_Ref : Node_Id;
      Obj_Typ : Entity_Id) return Node_Id
   is
      Obj_Addr : Node_Id;

   begin
      Obj_Addr :=
        Make_Attribute_Reference (Loc,
          Prefix => Obj_Ref,
          Attribute_Name => Name_Address);

      --  If the type of a constrained array has an unconstrained first
      --  subtype, its Finalize_Address primitive expects the address of
      --  an object with a dope vector (see Make_Finalize_Address_Stmts).
      --  This is achieved by setting Is_Constr_Array_Subt_With_Bounds,
      --  but the address of the object is still that of its elements,
      --  so we need to shift it.

      if Is_Array_Type (Obj_Typ)
        and then not Is_Constrained (First_Subtype (Obj_Typ))
      then
         --  Shift the address from the start of the elements to the
         --  start of the dope vector:

         --    V - (Obj_Typ'Descriptor_Size / Storage_Unit)

         Obj_Addr :=
           Make_Function_Call (Loc,
             Name                   =>
               Make_Expanded_Name (Loc,
                 Chars => Name_Op_Subtract,
                 Prefix =>
                   New_Occurrence_Of
                     (RTU_Entity (System_Storage_Elements), Loc),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Op_Subtract)),
             Parameter_Associations => New_List (
               Obj_Addr,
               Make_Op_Divide (Loc,
                 Left_Opnd  =>
                   Make_Attribute_Reference (Loc,
                     Prefix         => New_Occurrence_Of (Obj_Typ, Loc),
                     Attribute_Name => Name_Descriptor_Size),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc, System_Storage_Unit))));
      end if;

      return Obj_Addr;
   end Make_Address_For_Finalize;

   -----------------------
   -- Make_Adjust_Call --
   -----------------------

   function Make_Adjust_Call
     (Obj_Ref   : Node_Id;
      Typ       : Entity_Id;
      Skip_Self : Boolean := False) return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Obj_Ref);
      Adj_Id : Entity_Id := Empty;
      Ref    : Node_Id;
      Utyp   : Entity_Id;

   begin
      Ref := Obj_Ref;

      --  Recover the proper type which contains Deep_Adjust

      if Is_Class_Wide_Type (Typ) then
         Utyp := Root_Type (Typ);
      else
         Utyp := Typ;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Ref);

      --  Deal with untagged derivation of private views

      if Present (Utyp) and then Is_Untagged_Derivation (Typ) then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Ref  := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  When dealing with the completion of a private type, use the base
      --  type instead.

      if Present (Utyp) and then Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));

         Utyp := Base_Type (Utyp);
         Ref  := Unchecked_Convert_To (Utyp, Ref);
      end if;

      --  The underlying type may not be present due to a missing full view. In
      --  this case freezing did not take place and there is no [Deep_]Adjust
      --  primitive to call.

      if No (Utyp) then
         return Empty;

      elsif Skip_Self then
         if Has_Controlled_Component (Utyp) then
            if Is_Tagged_Type (Utyp) then
               Adj_Id := Find_Optional_Prim_Op (Utyp, TSS_Deep_Adjust);
            else
               Adj_Id := TSS (Utyp, TSS_Deep_Adjust);
            end if;
         end if;

      --  Class-wide types, interfaces and types with controlled components

      elsif Is_Class_Wide_Type (Typ)
        or else Is_Interface (Typ)
        or else Has_Controlled_Component (Utyp)
      then
         if Is_Tagged_Type (Utyp) then
            Adj_Id := Find_Optional_Prim_Op (Utyp, TSS_Deep_Adjust);
         else
            Adj_Id := TSS (Utyp, TSS_Deep_Adjust);
         end if;

      --  Derivations from [Limited_]Controlled

      elsif Is_Controlled (Utyp) then
         Adj_Id := Find_Controlled_Prim_Op (Utyp, Name_Adjust);

      --  Tagged types

      elsif Is_Tagged_Type (Utyp) then
         Adj_Id := Find_Optional_Prim_Op (Utyp, TSS_Deep_Adjust);

      else
         raise Program_Error;
      end if;

      if Present (Adj_Id) then
         --  The object reference may need another conversion depending on the
         --  type of the formal and that of the actual.

         if not Is_Class_Wide_Type (Typ) then
            Ref := Convert_View (Adj_Id, Ref, Typ);
         end if;

         return
           Make_Call (Loc,
             Proc_Id   => Adj_Id,
             Param     => Ref,
             Skip_Self => Skip_Self);
      else
         return Empty;
      end if;
   end Make_Adjust_Call;

   ---------------
   -- Make_Call --
   ---------------

   function Make_Call
     (Loc       : Source_Ptr;
      Proc_Id   : Entity_Id;
      Param     : Node_Id;
      Skip_Self : Boolean := False) return Node_Id
   is
      Params : constant List_Id := New_List (Param);

   begin
      --  Do not apply the controlled action to the object itself by signaling
      --  the related routine to avoid self.

      if Skip_Self then
         Append_To (Params, New_Occurrence_Of (Standard_False, Loc));
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Id, Loc),
          Parameter_Associations => Params);
   end Make_Call;

   --------------------------
   -- Make_Deep_Array_Body --
   --------------------------

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id) return List_Id
   is
      function Build_Adjust_Or_Finalize_Statements
        (Typ : Entity_Id) return List_Id;
      --  Create the statements necessary to adjust or finalize an array of
      --  controlled elements. Generate:
      --
      --    declare
      --       Abort  : constant Boolean := Triggered_By_Abort;
      --         <or>
      --       Abort  : constant Boolean := False;  --  no abort
      --
      --       E      : Exception_Occurrence;
      --       Raised : Boolean := False;
      --
      --    begin
      --       for J1 in [reverse] Typ'First (1) .. Typ'Last (1) loop
      --                 ^--  in the finalization case
      --          ...
      --          for Jn in [reverse] Typ'First (n) .. Typ'Last (n) loop
      --             begin
      --                [Deep_]Adjust / Finalize (V (J1, ..., Jn));
      --
      --             exception
      --                when others =>
      --                   if not Raised then
      --                      Raised := True;
      --                      Save_Occurrence (E, Get_Current_Excep.all.all);
      --                   end if;
      --             end;
      --          end loop;
      --          ...
      --       end loop;
      --
      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      function Build_Initialize_Statements (Typ : Entity_Id) return List_Id;
      --  Create the statements necessary to initialize an array of controlled
      --  elements. Include a mechanism to carry out partial finalization if an
      --  exception occurs. Generate:
      --
      --    declare
      --       Counter : Integer := 0;
      --
      --    begin
      --       for J1 in V'Range (1) loop
      --          ...
      --          for JN in V'Range (N) loop
      --             begin
      --                [Deep_]Initialize (V (J1, ..., JN));
      --
      --                Counter := Counter + 1;
      --
      --             exception
      --                when others =>
      --                   declare
      --                      Abort  : constant Boolean := Triggered_By_Abort;
      --                        <or>
      --                      Abort  : constant Boolean := False; --  no abort
      --                      E      : Exception_Occurrence;
      --                      Raised : Boolean := False;

      --                   begin
      --                      Counter :=
      --                        V'Length (1) *
      --                        V'Length (2) *
      --                        ...
      --                        V'Length (N) - Counter;

      --                      for F1 in reverse V'Range (1) loop
      --                         ...
      --                         for FN in reverse V'Range (N) loop
      --                            if Counter > 0 then
      --                               Counter := Counter - 1;
      --                            else
      --                               begin
      --                                  [Deep_]Finalize (V (F1, ..., FN));

      --                               exception
      --                                  when others =>
      --                                     if not Raised then
      --                                        Raised := True;
      --                                        Save_Occurrence (E,
      --                                          Get_Current_Excep.all.all);
      --                                     end if;
      --                               end;
      --                            end if;
      --                         end loop;
      --                         ...
      --                      end loop;
      --                   end;
      --
      --                   if Raised and then not Abort then
      --                      Raise_From_Controlled_Operation (E);
      --                   end if;
      --
      --                   raise;
      --             end;
      --          end loop;
      --       end loop;
      --    end;

      function New_References_To
        (L   : List_Id;
         Loc : Source_Ptr) return List_Id;
      --  Given a list of defining identifiers, return a list of references to
      --  the original identifiers, in the same order as they appear.

      -----------------------------------------
      -- Build_Adjust_Or_Finalize_Statements --
      -----------------------------------------

      function Build_Adjust_Or_Finalize_Statements
        (Typ : Entity_Id) return List_Id
      is
         Comp_Typ   : constant Entity_Id  := Component_Type (Typ);
         Index_List : constant List_Id    := New_List;
         Loc        : constant Source_Ptr := Sloc (Typ);
         Num_Dims   : constant Int        := Number_Dimensions (Typ);

         procedure Build_Indexes;
         --  Generate the indexes used in the dimension loops

         -------------------
         -- Build_Indexes --
         -------------------

         procedure Build_Indexes is
         begin
            --  Generate the following identifiers:
            --    Jnn  -  for initialization

            for Dim in 1 .. Num_Dims loop
               Append_To (Index_List,
                 Make_Defining_Identifier (Loc, New_External_Name ('J', Dim)));
            end loop;
         end Build_Indexes;

         --  Local variables

         Final_Decls : List_Id := No_List;
         Final_Data  : Finalization_Exception_Data;
         Block       : Node_Id;
         Call        : Node_Id;
         Comp_Ref    : Node_Id;
         Core_Loop   : Node_Id;
         Dim         : Int;
         J           : Entity_Id;
         Loop_Id     : Entity_Id;
         Stmts       : List_Id;

      --  Start of processing for Build_Adjust_Or_Finalize_Statements

      begin
         Final_Decls := New_List;

         Build_Indexes;
         Build_Object_Declarations (Final_Data, Final_Decls, Loc);

         Comp_Ref :=
           Make_Indexed_Component (Loc,
             Prefix      => Make_Identifier (Loc, Name_V),
             Expressions => New_References_To (Index_List, Loc));
         Set_Etype (Comp_Ref, Comp_Typ);

         --  Generate:
         --    [Deep_]Adjust (V (J1, ..., JN))

         if Prim = Adjust_Case then
            Call := Make_Adjust_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);

         --  Generate:
         --    [Deep_]Finalize (V (J1, ..., JN))

         else pragma Assert (Prim = Finalize_Case);
            Call := Make_Final_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);
         end if;

         if Present (Call) then

            --  Generate the block which houses the adjust or finalize call:

            --    begin
            --       <adjust or finalize call>

            --    exception
            --       when others =>
            --          if not Raised then
            --             Raised := True;
            --             Save_Occurrence (E, Get_Current_Excep.all.all);
            --          end if;
            --    end;

            if Exceptions_OK then
               Core_Loop :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements         => New_List (Call),
                       Exception_Handlers => New_List (
                         Build_Exception_Handler (Final_Data))));
            else
               Core_Loop := Call;
            end if;

            --  Generate the dimension loops starting from the innermost one

            --    for Jnn in [reverse] V'Range (Dim) loop
            --       <core loop>
            --    end loop;

            J := Last (Index_List);
            Dim := Num_Dims;
            while Present (J) and then Dim > 0 loop
               Loop_Id := J;
               Prev (J);
               Remove (Loop_Id);

               Core_Loop :=
                 Make_Loop_Statement (Loc,
                   Iteration_Scheme =>
                     Make_Iteration_Scheme (Loc,
                       Loop_Parameter_Specification =>
                         Make_Loop_Parameter_Specification (Loc,
                           Defining_Identifier         => Loop_Id,
                           Discrete_Subtype_Definition =>
                             Make_Attribute_Reference (Loc,
                               Prefix         => Make_Identifier (Loc, Name_V),
                               Attribute_Name => Name_Range,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, Dim))),

                           Reverse_Present             =>
                             Prim = Finalize_Case)),

                   Statements       => New_List (Core_Loop),
                   End_Label        => Empty);

               Dim := Dim - 1;
            end loop;

            --  Generate the block which contains the core loop, declarations
            --  of the abort flag, the exception occurrence, the raised flag
            --  and the conditional raise:

            --    declare
            --       Abort  : constant Boolean := Triggered_By_Abort;
            --         <or>
            --       Abort  : constant Boolean := False;  --  no abort

            --       E      : Exception_Occurrence;
            --       Raised : Boolean := False;

            --    begin
            --       <core loop>

            --       if Raised and then not Abort then
            --          Raise_From_Controlled_Operation (E);
            --       end if;
            --    end;

            Stmts := New_List (Core_Loop);

            if Exceptions_OK then
               Append_To (Stmts, Build_Raise_Statement (Final_Data));
            end if;

            Block :=
              Make_Block_Statement (Loc,
                Declarations               => Final_Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Stmts));

         --  Otherwise previous errors or a missing full view may prevent the
         --  proper freezing of the component type. If this is the case, there
         --  is no [Deep_]Adjust or [Deep_]Finalize primitive to call.

         else
            Block := Make_Null_Statement (Loc);
         end if;

         return New_List (Block);
      end Build_Adjust_Or_Finalize_Statements;

      ---------------------------------
      -- Build_Initialize_Statements --
      ---------------------------------

      function Build_Initialize_Statements (Typ : Entity_Id) return List_Id is
         Comp_Typ   : constant Entity_Id  := Component_Type (Typ);
         Final_List : constant List_Id    := New_List;
         Index_List : constant List_Id    := New_List;
         Loc        : constant Source_Ptr := Sloc (Typ);
         Num_Dims   : constant Int        := Number_Dimensions (Typ);

         function Build_Assignment (Counter_Id : Entity_Id) return Node_Id;
         --  Generate the following assignment:
         --    Counter := V'Length (1) *
         --               ...
         --               V'Length (N) - Counter;
         --
         --  Counter_Id denotes the entity of the counter.

         function Build_Finalization_Call return Node_Id;
         --  Generate a deep finalization call for an array element

         procedure Build_Indexes;
         --  Generate the initialization and finalization indexes used in the
         --  dimension loops.

         function Build_Initialization_Call return Node_Id;
         --  Generate a deep initialization call for an array element

         ----------------------
         -- Build_Assignment --
         ----------------------

         function Build_Assignment (Counter_Id : Entity_Id) return Node_Id is
            Dim  : Int;
            Expr : Node_Id;

         begin
            --  Start from the first dimension and generate:
            --    V'Length (1)

            Dim := 1;
            Expr :=
              Make_Attribute_Reference (Loc,
                Prefix         => Make_Identifier (Loc, Name_V),
                Attribute_Name => Name_Length,
                Expressions    => New_List (Make_Integer_Literal (Loc, Dim)));

            --  Process the rest of the dimensions, generate:
            --    Expr * V'Length (N)

            Dim := Dim + 1;
            while Dim <= Num_Dims loop
               Expr :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd  => Expr,
                   Right_Opnd =>
                     Make_Attribute_Reference (Loc,
                       Prefix         => Make_Identifier (Loc, Name_V),
                       Attribute_Name => Name_Length,
                       Expressions    => New_List (
                         Make_Integer_Literal (Loc, Dim))));

               Dim := Dim + 1;
            end loop;

            --  Generate:
            --    Counter := Expr - Counter;

            return
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Counter_Id, Loc),
                Expression =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd  => Expr,
                    Right_Opnd => New_Occurrence_Of (Counter_Id, Loc)));
         end Build_Assignment;

         -----------------------------
         -- Build_Finalization_Call --
         -----------------------------

         function Build_Finalization_Call return Node_Id is
            Comp_Ref : constant Node_Id :=
                         Make_Indexed_Component (Loc,
                           Prefix      => Make_Identifier (Loc, Name_V),
                           Expressions => New_References_To (Final_List, Loc));

         begin
            Set_Etype (Comp_Ref, Comp_Typ);

            --  Generate:
            --    [Deep_]Finalize (V);

            return Make_Final_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);
         end Build_Finalization_Call;

         -------------------
         -- Build_Indexes --
         -------------------

         procedure Build_Indexes is
         begin
            --  Generate the following identifiers:
            --    Jnn  -  for initialization
            --    Fnn  -  for finalization

            for Dim in 1 .. Num_Dims loop
               Append_To (Index_List,
                 Make_Defining_Identifier (Loc, New_External_Name ('J', Dim)));

               Append_To (Final_List,
                 Make_Defining_Identifier (Loc, New_External_Name ('F', Dim)));
            end loop;
         end Build_Indexes;

         -------------------------------
         -- Build_Initialization_Call --
         -------------------------------

         function Build_Initialization_Call return Node_Id is
            Comp_Ref : constant Node_Id :=
                         Make_Indexed_Component (Loc,
                           Prefix      => Make_Identifier (Loc, Name_V),
                           Expressions => New_References_To (Index_List, Loc));

         begin
            Set_Etype (Comp_Ref, Comp_Typ);

            --  Generate:
            --    [Deep_]Initialize (V (J1, ..., JN));

            return Make_Init_Call (Obj_Ref => Comp_Ref, Typ => Comp_Typ);
         end Build_Initialization_Call;

         --  Local variables

         Counter_Id  : Entity_Id;
         Dim         : Int;
         F           : Node_Id;
         Fin_Stmt    : Node_Id;
         Final_Block : Node_Id;
         Final_Data  : Finalization_Exception_Data;
         Final_Decls : List_Id := No_List;
         Final_Loop  : Node_Id;
         Init_Block  : Node_Id;
         Init_Call   : Node_Id;
         Init_Loop   : Node_Id;
         J           : Node_Id;
         Loop_Id     : Node_Id;
         Stmts       : List_Id;

      --  Start of processing for Build_Initialize_Statements

      begin
         Counter_Id  := Make_Temporary (Loc, 'C');
         Final_Decls := New_List;

         Build_Indexes;
         Build_Object_Declarations (Final_Data, Final_Decls, Loc);

         --  Generate the block which houses the finalization call, the index
         --  guard and the handler which triggers Program_Error later on.

         --    if Counter > 0 then
         --       Counter := Counter - 1;
         --    else
         --       begin
         --          [Deep_]Finalize (V (F1, ..., FN));
         --       exception
         --          when others =>
         --             if not Raised then
         --                Raised := True;
         --                Save_Occurrence (E, Get_Current_Excep.all.all);
         --             end if;
         --       end;
         --    end if;

         Fin_Stmt := Build_Finalization_Call;

         if Present (Fin_Stmt) then
            if Exceptions_OK then
               Fin_Stmt :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements         => New_List (Fin_Stmt),
                       Exception_Handlers => New_List (
                         Build_Exception_Handler (Final_Data))));
            end if;

            --  This is the core of the loop, the dimension iterators are added
            --  one by one in reverse.

            Final_Loop :=
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
                    Right_Opnd => Make_Integer_Literal (Loc, 0)),

                Then_Statements => New_List (
                  Make_Assignment_Statement (Loc,
                    Name       => New_Occurrence_Of (Counter_Id, Loc),
                    Expression =>
                      Make_Op_Subtract (Loc,
                        Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
                        Right_Opnd => Make_Integer_Literal (Loc, 1)))),

                Else_Statements => New_List (Fin_Stmt));

            --  Generate all finalization loops starting from the innermost
            --  dimension.

            --    for Fnn in reverse V'Range (Dim) loop
            --       <final loop>
            --    end loop;

            F := Last (Final_List);
            Dim := Num_Dims;
            while Present (F) and then Dim > 0 loop
               Loop_Id := F;
               Prev (F);
               Remove (Loop_Id);

               Final_Loop :=
                 Make_Loop_Statement (Loc,
                   Iteration_Scheme =>
                     Make_Iteration_Scheme (Loc,
                       Loop_Parameter_Specification =>
                         Make_Loop_Parameter_Specification (Loc,
                           Defining_Identifier         => Loop_Id,
                           Discrete_Subtype_Definition =>
                             Make_Attribute_Reference (Loc,
                               Prefix         => Make_Identifier (Loc, Name_V),
                               Attribute_Name => Name_Range,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, Dim))),

                           Reverse_Present             => True)),

                   Statements       => New_List (Final_Loop),
                   End_Label        => Empty);

               Dim := Dim - 1;
            end loop;

            --  Generate the block which contains the finalization loops, the
            --  declarations of the abort flag, the exception occurrence, the
            --  raised flag and the conditional raise.

            --    declare
            --       Abort  : constant Boolean := Triggered_By_Abort;
            --         <or>
            --       Abort  : constant Boolean := False;  --  no abort

            --       E      : Exception_Occurrence;
            --       Raised : Boolean := False;

            --    begin
            --       Counter :=
            --         V'Length (1) *
            --         ...
            --         V'Length (N) - Counter;

            --       <final loop>

            --       if Raised and then not Abort then
            --          Raise_From_Controlled_Operation (E);
            --       end if;

            --       raise;
            --    end;

            Stmts := New_List (Build_Assignment (Counter_Id), Final_Loop);

            if Exceptions_OK then
               Append_To (Stmts, Build_Raise_Statement (Final_Data));
               Append_To (Stmts, Make_Raise_Statement (Loc));
            end if;

            Final_Block :=
              Make_Block_Statement (Loc,
                Declarations               => Final_Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Stmts));

         --  Otherwise previous errors or a missing full view may prevent the
         --  proper freezing of the component type. If this is the case, there
         --  is no [Deep_]Finalize primitive to call.

         else
            Final_Block := Make_Null_Statement (Loc);
         end if;

         --  Generate the block which contains the initialization call and
         --  the partial finalization code.

         --    begin
         --       [Deep_]Initialize (V (J1, ..., JN));

         --       Counter := Counter + 1;

         --    exception
         --       when others =>
         --          <finalization code>
         --    end;

         Init_Call := Build_Initialization_Call;

         --  Only create finalization block if there is a nontrivial call
         --  to initialization or a Default_Initial_Condition check to be
         --  performed.

         if (Present (Init_Call)
              and then Nkind (Init_Call) /= N_Null_Statement)
           or else
             (Has_DIC (Comp_Typ)
               and then not GNATprove_Mode
               and then Present (DIC_Procedure (Comp_Typ))
               and then not Has_Null_Body (DIC_Procedure (Comp_Typ)))
         then
            declare
               Init_Stmts : constant List_Id := New_List;

            begin
               if Present (Init_Call) then
                  Append_To (Init_Stmts, Init_Call);
               end if;

               if Has_DIC (Comp_Typ)
                 and then Present (DIC_Procedure (Comp_Typ))
               then
                  Append_To
                    (Init_Stmts,
                     Build_DIC_Call (Loc,
                         Make_Indexed_Component (Loc,
                           Prefix      => Make_Identifier (Loc, Name_V),
                           Expressions => New_References_To (Index_List, Loc)),
                         Comp_Typ));
               end if;

               Init_Loop :=
                 Make_Block_Statement (Loc,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements         => Init_Stmts,
                       Exception_Handlers => New_List (
                         Make_Exception_Handler (Loc,
                           Exception_Choices => New_List (
                             Make_Others_Choice (Loc)),
                           Statements        => New_List (Final_Block)))));
            end;

            Append_To (Statements (Handled_Statement_Sequence (Init_Loop)),
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Counter_Id, Loc),
                Expression =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => New_Occurrence_Of (Counter_Id, Loc),
                    Right_Opnd => Make_Integer_Literal (Loc, 1))));

            --  Generate all initialization loops starting from the innermost
            --  dimension.

            --    for Jnn in V'Range (Dim) loop
            --       <init loop>
            --    end loop;

            J := Last (Index_List);
            Dim := Num_Dims;
            while Present (J) and then Dim > 0 loop
               Loop_Id := J;
               Prev (J);
               Remove (Loop_Id);

               Init_Loop :=
                 Make_Loop_Statement (Loc,
                   Iteration_Scheme =>
                     Make_Iteration_Scheme (Loc,
                       Loop_Parameter_Specification =>
                         Make_Loop_Parameter_Specification (Loc,
                           Defining_Identifier => Loop_Id,
                           Discrete_Subtype_Definition =>
                             Make_Attribute_Reference (Loc,
                               Prefix         => Make_Identifier (Loc, Name_V),
                               Attribute_Name => Name_Range,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, Dim))))),

                   Statements => New_List (Init_Loop),
                   End_Label => Empty);

               Dim := Dim - 1;
            end loop;

            --  Generate the block which contains the counter variable and the
            --  initialization loops.

            --    declare
            --       Counter : Integer := 0;
            --    begin
            --       <init loop>
            --    end;

            Init_Block :=
              Make_Block_Statement (Loc,
               Declarations               => New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Counter_Id,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Integer, Loc),
                   Expression          => Make_Integer_Literal (Loc, 0))),

               Handled_Statement_Sequence =>
                 Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => New_List (Init_Loop)));

            if Debug_Generated_Code then
               Set_Debug_Info_Needed (Counter_Id);
            end if;

         --  Otherwise previous errors or a missing full view may prevent the
         --  proper freezing of the component type. If this is the case, there
         --  is no [Deep_]Initialize primitive to call.

         else
            Init_Block := Make_Null_Statement (Loc);
         end if;

         return New_List (Init_Block);
      end Build_Initialize_Statements;

      -----------------------
      -- New_References_To --
      -----------------------

      function New_References_To
        (L   : List_Id;
         Loc : Source_Ptr) return List_Id
      is
         Refs : constant List_Id := New_List;
         Id   : Node_Id;

      begin
         Id := First (L);
         while Present (Id) loop
            Append_To (Refs, New_Occurrence_Of (Id, Loc));
            Next (Id);
         end loop;

         return Refs;
      end New_References_To;

   --  Start of processing for Make_Deep_Array_Body

   begin
      case Prim is
         when Address_Case =>
            return Make_Finalize_Address_Stmts (Typ);

         when Adjust_Case
            | Finalize_Case
         =>
            return Build_Adjust_Or_Finalize_Statements (Typ);

         when Initialize_Case =>
            return Build_Initialize_Statements (Typ);
      end case;
   end Make_Deep_Array_Body;

   --------------------
   -- Make_Deep_Proc --
   --------------------

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id) return Entity_Id
   is
      Loc     : constant Source_Ptr := Sloc (Typ);
      Formals : List_Id;
      Proc_Id : Entity_Id;

   begin
      --  Create the object formal, generate:
      --    V : System.Address

      if Prim = Address_Case then
         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Parameter_Type      =>
               New_Occurrence_Of (RTE (RE_Address), Loc)));

      --  Default case

      else
         --  V : in out Typ

         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      => New_Occurrence_Of (Typ, Loc)));

         --  F : Boolean := True

         if Prim = Adjust_Case
           or else Prim = Finalize_Case
         then
            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Make_Defining_Identifier (Loc, Name_F),
                Parameter_Type      =>
                  New_Occurrence_Of (Standard_Boolean, Loc),
                Expression          =>
                  New_Occurrence_Of (Standard_True, Loc)));
         end if;
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name (Typ, Deep_Name_Of (Prim)));

      --  Generate:
      --    procedure Deep_Initialize / Adjust / Finalize (V : in out <typ>) is
      --    begin
      --       <stmts>
      --    exception                --  Finalize and Adjust cases only
      --       raise Program_Error;
      --    end Deep_Initialize / Adjust / Finalize;

      --       or

      --    procedure Finalize_Address (V : System.Address) is
      --    begin
      --       <stmts>
      --    end Finalize_Address;

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Id,
              Parameter_Specifications => Formals),

          Declarations => Empty_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Stmts)));

      --  If there are no calls to component initialization, indicate that
      --  the procedure is trivial, so prevent calls to it.

      if Is_Empty_List (Stmts)
        or else Nkind (First (Stmts)) = N_Null_Statement
      then
         Set_Is_Trivial_Subprogram (Proc_Id);
      end if;

      return Proc_Id;
   end Make_Deep_Proc;

   ---------------------------
   -- Make_Deep_Record_Body --
   ---------------------------

   function Make_Deep_Record_Body
     (Prim     : Final_Primitives;
      Typ      : Entity_Id;
      Is_Local : Boolean := False) return List_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      function Build_Adjust_Statements (Typ : Entity_Id) return List_Id;
      --  Build the statements necessary to adjust a record type. The type may
      --  have discriminants and contain variant parts. Generate:
      --
      --    begin
      --       begin
      --          [Deep_]Adjust (V.Comp_1);
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --       .  .  .
      --       begin
      --          [Deep_]Adjust (V.Comp_N);
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --
      --       begin
      --          Deep_Adjust (V._parent, False);  --  If applicable
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --
      --       if F then
      --          begin
      --             Adjust (V);  --  If applicable
      --          exception
      --             when others =>
      --                if not Raised then
      --                   Raised := True;
      --                   Save_Occurrence (E, Get_Current_Excep.all.all);
      --                end if;
      --          end;
      --       end if;
      --
      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      function Build_Finalize_Statements (Typ : Entity_Id) return List_Id;
      --  Build the statements necessary to finalize a record type. The type
      --  may have discriminants and contain variant parts. Generate:
      --
      --    declare
      --       Abort  : constant Boolean := Triggered_By_Abort;
      --         <or>
      --       Abort  : constant Boolean := False;  --  no abort
      --       E      : Exception_Occurrence;
      --       Raised : Boolean := False;
      --
      --    begin
      --       if F then
      --          begin
      --             Finalize (V);  --  If applicable
      --          exception
      --             when others =>
      --                if not Raised then
      --                   Raised := True;
      --                   Save_Occurrence (E, Get_Current_Excep.all.all);
      --                end if;
      --          end;
      --       end if;
      --
      --       case Variant_1 is
      --          when Value_1 =>
      --             case State_Counter_N =>  --  If Is_Local is enabled
      --                when N =>                 .
      --                   goto LN;               .
      --                ...                       .
      --                when 1 =>                 .
      --                   goto L1;               .
      --                when others =>            .
      --                   goto L0;               .
      --             end case;                    .
      --
      --             <<LN>>                   --  If Is_Local is enabled
      --             begin
      --                [Deep_]Finalize (V.Comp_N);
      --             exception
      --                when others =>
      --                   if not Raised then
      --                      Raised := True;
      --                      Save_Occurrence (E, Get_Current_Excep.all.all);
      --                   end if;
      --             end;
      --             .  .  .
      --             <<L1>>
      --             begin
      --                [Deep_]Finalize (V.Comp_1);
      --             exception
      --                when others =>
      --                   if not Raised then
      --                      Raised := True;
      --                      Save_Occurrence (E, Get_Current_Excep.all.all);
      --                   end if;
      --             end;
      --             <<L0>>
      --       end case;
      --
      --       case State_Counter_1 =>  --  If Is_Local is enabled
      --          when M =>                 .
      --             goto LM;               .
      --       ...
      --
      --       begin
      --          Deep_Finalize (V._parent, False);  --  If applicable
      --       exception
      --          when Id : others =>
      --             if not Raised then
      --                Raised := True;
      --                Save_Occurrence (E, Get_Current_Excep.all.all);
      --             end if;
      --       end;
      --
      --       if Raised and then not Abort then
      --          Raise_From_Controlled_Operation (E);
      --       end if;
      --    end;

      function Parent_Field_Type (Typ : Entity_Id) return Entity_Id;
      --  Given a derived tagged type Typ, traverse all components, find field
      --  _parent and return its type.

      procedure Preprocess_Components
        (Comps     : Node_Id;
         Num_Comps : out Nat;
         Has_POC   : out Boolean);
      --  Examine all components in component list Comps, count all controlled
      --  components and determine whether at least one of them is per-object
      --  constrained. Component _parent is always skipped.

      -----------------------------
      -- Build_Adjust_Statements --
      -----------------------------

      function Build_Adjust_Statements (Typ : Entity_Id) return List_Id is
         Typ_Def : constant Node_Id    := Type_Definition (Parent (Typ));

         Finalizer_Data : Finalization_Exception_Data;

         function Process_Component_List_For_Adjust
           (Comps : Node_Id) return List_Id;
         --  Build all necessary adjust statements for a single component list

         ---------------------------------------
         -- Process_Component_List_For_Adjust --
         ---------------------------------------

         function Process_Component_List_For_Adjust
           (Comps : Node_Id) return List_Id
         is
            Stmts : constant List_Id := New_List;

            procedure Process_Component_For_Adjust (Decl : Node_Id);
            --  Process the declaration of a single controlled component

            ----------------------------------
            -- Process_Component_For_Adjust --
            ----------------------------------

            procedure Process_Component_For_Adjust (Decl : Node_Id) is
               Id  : constant Entity_Id := Defining_Identifier (Decl);
               Typ : constant Entity_Id := Etype (Id);

               Adj_Call : Node_Id;

            begin
               --    begin
               --       [Deep_]Adjust (V.Id);

               --    exception
               --       when others =>
               --          if not Raised then
               --             Raised := True;
               --             Save_Occurrence (E, Get_Current_Excep.all.all);
               --          end if;
               --    end;

               Adj_Call :=
                 Make_Adjust_Call (
                   Obj_Ref =>
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_V),
                       Selector_Name => Make_Identifier (Loc, Chars (Id))),
                   Typ     => Typ);

               --  Guard against a missing [Deep_]Adjust when the component
               --  type was not properly frozen.

               if Present (Adj_Call) then
                  if Exceptions_OK then
                     Adj_Call :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                             Statements         => New_List (Adj_Call),
                             Exception_Handlers => New_List (
                               Build_Exception_Handler (Finalizer_Data))));
                  end if;

                  Append_To (Stmts, Adj_Call);
               end if;
            end Process_Component_For_Adjust;

            --  Local variables

            Decl      : Node_Id;
            Decl_Id   : Entity_Id;
            Decl_Typ  : Entity_Id;
            Has_POC   : Boolean;
            Num_Comps : Nat;
            Var_Case  : Node_Id;

         --  Start of processing for Process_Component_List_For_Adjust

         begin
            --  Perform an initial check, determine the number of controlled
            --  components in the current list and whether at least one of them
            --  is per-object constrained.

            Preprocess_Components (Comps, Num_Comps, Has_POC);

            --  The processing in this routine is done in the following order:
            --    1) Regular components
            --    2) Per-object constrained components
            --    3) Variant parts

            if Num_Comps > 0 then

               --  Process all regular components in order of declarations

               Decl := First_Non_Pragma (Component_Items (Comps));
               while Present (Decl) loop
                  Decl_Id  := Defining_Identifier (Decl);
                  Decl_Typ := Etype (Decl_Id);

                  --  Skip _parent as well as per-object constrained components

                  if Chars (Decl_Id) /= Name_uParent
                    and then Needs_Finalization (Decl_Typ)
                  then
                     if Has_Access_Constraint (Decl_Id)
                       and then No (Expression (Decl))
                     then
                        null;
                     else
                        Process_Component_For_Adjust (Decl);
                     end if;
                  end if;

                  Next_Non_Pragma (Decl);
               end loop;

               --  Process all per-object constrained components in order of
               --  declarations.

               if Has_POC then
                  Decl := First_Non_Pragma (Component_Items (Comps));
                  while Present (Decl) loop
                     Decl_Id  := Defining_Identifier (Decl);
                     Decl_Typ := Etype (Decl_Id);

                     --  Skip _parent

                     if Chars (Decl_Id) /= Name_uParent
                       and then Needs_Finalization (Decl_Typ)
                       and then Has_Access_Constraint (Decl_Id)
                       and then No (Expression (Decl))
                     then
                        Process_Component_For_Adjust (Decl);
                     end if;

                     Next_Non_Pragma (Decl);
                  end loop;
               end if;
            end if;

            --  Process all variants, if any

            Var_Case := Empty;
            if Present (Variant_Part (Comps)) then
               declare
                  Var_Alts : constant List_Id := New_List;
                  Var      : Node_Id;

               begin
                  Var := First_Non_Pragma (Variants (Variant_Part (Comps)));
                  while Present (Var) loop

                     --  Generate:
                     --     when <discrete choices> =>
                     --        <adjust statements>

                     Append_To (Var_Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices =>
                           New_Copy_List (Discrete_Choices (Var)),
                         Statements       =>
                           Process_Component_List_For_Adjust (
                             Component_List (Var))));

                     Next_Non_Pragma (Var);
                  end loop;

                  --  Generate:
                  --     case V.<discriminant> is
                  --        when <discrete choices 1> =>
                  --           <adjust statements 1>
                  --        ...
                  --        when <discrete choices N> =>
                  --           <adjust statements N>
                  --     end case;

                  Var_Case :=
                    Make_Case_Statement (Loc,
                      Expression =>
                        Make_Selected_Component (Loc,
                          Prefix        => Make_Identifier (Loc, Name_V),
                          Selector_Name =>
                            Make_Identifier (Loc,
                              Chars => Chars (Name (Variant_Part (Comps))))),
                      Alternatives => Var_Alts);
               end;
            end if;

            --  Add the variant case statement to the list of statements

            if Present (Var_Case) then
               Append_To (Stmts, Var_Case);
            end if;

            --  If the component list did not have any controlled components
            --  nor variants, return null.

            if Is_Empty_List (Stmts) then
               Append_To (Stmts, Make_Null_Statement (Loc));
            end if;

            return Stmts;
         end Process_Component_List_For_Adjust;

         --  Local variables

         Bod_Stmts       : List_Id := No_List;
         Finalizer_Decls : List_Id := No_List;
         Rec_Def         : Node_Id;

      --  Start of processing for Build_Adjust_Statements

      begin
         Finalizer_Decls := New_List;
         Build_Object_Declarations (Finalizer_Data, Finalizer_Decls, Loc);

         if Nkind (Typ_Def) = N_Derived_Type_Definition then
            Rec_Def := Record_Extension_Part (Typ_Def);
         else
            Rec_Def := Typ_Def;
         end if;

         --  Create an adjust sequence for all record components

         if Present (Component_List (Rec_Def)) then
            Bod_Stmts :=
              Process_Component_List_For_Adjust (Component_List (Rec_Def));
         end if;

         --  A derived record type must adjust all inherited components. This
         --  action poses the following problem:

         --    procedure Deep_Adjust (Obj : in out Parent_Typ) is
         --    begin
         --       Adjust (Obj);
         --       ...

         --    procedure Deep_Adjust (Obj : in out Derived_Typ) is
         --    begin
         --       Deep_Adjust (Obj._parent);
         --       ...
         --       Adjust (Obj);
         --       ...

         --  Adjusting the derived type will invoke Adjust of the parent and
         --  then that of the derived type. This is undesirable because both
         --  routines may modify shared components. Only the Adjust of the
         --  derived type should be invoked.

         --  To prevent this double adjustment of shared components,
         --  Deep_Adjust uses a flag to control the invocation of Adjust:

         --    procedure Deep_Adjust
         --      (Obj  : in out Some_Type;
         --       Flag : Boolean := True)
         --    is
         --    begin
         --       if Flag then
         --          Adjust (Obj);
         --       end if;
         --       ...

         --  When Deep_Adjust is invoked for field _parent, a value of False is
         --  provided for the flag:

         --    Deep_Adjust (Obj._parent, False);

         if Is_Tagged_Type (Typ) and then Is_Derived_Type (Typ) then
            declare
               Par_Typ  : constant Entity_Id := Parent_Field_Type (Typ);
               Adj_Stmt : Node_Id;
               Call     : Node_Id;

            begin
               if Needs_Finalization (Par_Typ) then
                  Call :=
                    Make_Adjust_Call
                      (Obj_Ref   =>
                         Make_Selected_Component (Loc,
                           Prefix        => Make_Identifier (Loc, Name_V),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_uParent)),
                       Typ       => Par_Typ,
                       Skip_Self => True);

                  --  Generate:
                  --    begin
                  --       Deep_Adjust (V._parent, False);

                  --    exception
                  --       when Id : others =>
                  --          if not Raised then
                  --             Raised := True;
                  --             Save_Occurrence (E,
                  --               Get_Current_Excep.all.all);
                  --          end if;
                  --    end;

                  if Present (Call) then
                     Adj_Stmt := Call;

                     if Exceptions_OK then
                        Adj_Stmt :=
                          Make_Block_Statement (Loc,
                            Handled_Statement_Sequence =>
                              Make_Handled_Sequence_Of_Statements (Loc,
                                Statements         => New_List (Adj_Stmt),
                                Exception_Handlers => New_List (
                                  Build_Exception_Handler (Finalizer_Data))));
                     end if;

                     Prepend_To (Bod_Stmts, Adj_Stmt);
                  end if;
               end if;
            end;
         end if;

         --  Adjust the object. This action must be performed last after all
         --  components have been adjusted.

         if Is_Controlled (Typ) then
            declare
               Adj_Stmt : Node_Id;
               Proc     : Entity_Id;

            begin
               Proc := Find_Controlled_Prim_Op (Typ, Name_Adjust);

               --  Generate:
               --    if F then
               --       begin
               --          Adjust (V);

               --       exception
               --          when others =>
               --             if not Raised then
               --                Raised := True;
               --                Save_Occurrence (E,
               --                  Get_Current_Excep.all.all);
               --             end if;
               --       end;
               --    end if;

               if Present (Proc) then
                  Adj_Stmt :=
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => New_Occurrence_Of (Proc, Loc),
                      Parameter_Associations => New_List (
                        Make_Identifier (Loc, Name_V)));

                  if Exceptions_OK then
                     Adj_Stmt :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                             Statements         => New_List (Adj_Stmt),
                             Exception_Handlers => New_List (
                               Build_Exception_Handler
                                 (Finalizer_Data))));
                  end if;

                  Append_To (Bod_Stmts,
                    Make_If_Statement (Loc,
                      Condition       => Make_Identifier (Loc, Name_F),
                      Then_Statements => New_List (Adj_Stmt)));
               end if;
            end;
         end if;

         --  At this point either all adjustment statements have been generated
         --  or the type is not controlled.

         if Is_Empty_List (Bod_Stmts) then
            Append_To (Bod_Stmts, Make_Null_Statement (Loc));

            return Bod_Stmts;

         --  Generate:
         --    declare
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurrence;
         --       Raised : Boolean := False;

         --    begin
         --       <adjust statements>

         --       if Raised and then not Abort then
         --          Raise_From_Controlled_Operation (E);
         --       end if;
         --    end;

         else
            if Exceptions_OK then
               Append_To (Bod_Stmts, Build_Raise_Statement (Finalizer_Data));
            end if;

            return
              New_List (
                Make_Block_Statement (Loc,
                  Declarations               =>
                    Finalizer_Decls,
                  Handled_Statement_Sequence =>
                    Make_Handled_Sequence_Of_Statements (Loc, Bod_Stmts)));
         end if;
      end Build_Adjust_Statements;

      -------------------------------
      -- Build_Finalize_Statements --
      -------------------------------

      function Build_Finalize_Statements (Typ : Entity_Id) return List_Id is
         Typ_Def : constant Node_Id := Type_Definition (Parent (Typ));

         Counter        : Nat := 0;
         Finalizer_Data : Finalization_Exception_Data;
         Last_POC_Call  : Node_Id := Empty;

         function Process_Component_List_For_Finalize
           (Comps           : Node_Id;
            In_Variant_Part : Boolean := False) return List_Id;
         --  Build all necessary finalization statements for a single component
         --  list. The statements may include a jump circuitry if flag Is_Local
         --  is enabled. In_Variant_Part indicates whether this is a recursive
         --  call.

         -----------------------------------------
         -- Process_Component_List_For_Finalize --
         -----------------------------------------

         function Process_Component_List_For_Finalize
           (Comps           : Node_Id;
            In_Variant_Part : Boolean := False) return List_Id
         is
            procedure Process_Component_For_Finalize
              (Decl      : Node_Id;
               Alts      : List_Id;
               Decls     : List_Id;
               Stmts     : List_Id;
               Num_Comps : in out Nat);
            --  Process the declaration of a single controlled component. If
            --  flag Is_Local is enabled, create the corresponding label and
            --  jump circuitry. Alts is the list of case alternatives, Decls
            --  is the top level declaration list where labels are declared
            --  and Stmts is the list of finalization actions. Num_Comps
            --  denotes the current number of components needing finalization.

            ------------------------------------
            -- Process_Component_For_Finalize --
            ------------------------------------

            procedure Process_Component_For_Finalize
              (Decl      : Node_Id;
               Alts      : List_Id;
               Decls     : List_Id;
               Stmts     : List_Id;
               Num_Comps : in out Nat)
            is
               Id       : constant Entity_Id := Defining_Identifier (Decl);
               Typ      : constant Entity_Id := Etype (Id);
               Fin_Call : Node_Id;

            begin
               if Is_Local then
                  declare
                     Label    : Node_Id;
                     Label_Id : Entity_Id;

                  begin
                     --  Generate:
                     --    LN : label;

                     Label_Id :=
                       Make_Identifier (Loc,
                         Chars => New_External_Name ('L', Num_Comps));
                     Set_Entity (Label_Id,
                       Make_Defining_Identifier (Loc, Chars (Label_Id)));
                     Label := Make_Label (Loc, Label_Id);

                     Append_To (Decls,
                       Make_Implicit_Label_Declaration (Loc,
                         Defining_Identifier => Entity (Label_Id),
                         Label_Construct     => Label));

                     --  Generate:
                     --    when N =>
                     --      goto LN;

                     Append_To (Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices => New_List (
                           Make_Integer_Literal (Loc, Num_Comps)),

                         Statements => New_List (
                           Make_Goto_Statement (Loc,
                             Name =>
                               New_Occurrence_Of (Entity (Label_Id), Loc)))));

                     --  Generate:
                     --    <<LN>>

                     Append_To (Stmts, Label);

                     --  Decrease the number of components to be processed.
                     --  This action yields a new Label_Id in future calls.

                     Num_Comps := Num_Comps - 1;
                  end;
               end if;

               --  Generate:
               --    [Deep_]Finalize (V.Id);  --  No_Exception_Propagation

               --    begin                    --  Exception handlers allowed
               --       [Deep_]Finalize (V.Id);
               --    exception
               --       when others =>
               --          if not Raised then
               --             Raised := True;
               --             Save_Occurrence (E,
               --               Get_Current_Excep.all.all);
               --          end if;
               --    end;

               Fin_Call :=
                 Make_Final_Call
                   (Obj_Ref =>
                      Make_Selected_Component (Loc,
                        Prefix        => Make_Identifier (Loc, Name_V),
                        Selector_Name => Make_Identifier (Loc, Chars (Id))),
                    Typ     => Typ);

               --  Guard against a missing [Deep_]Finalize when the component
               --  type was not properly frozen.

               if Present (Fin_Call) then
                  if Exceptions_OK then
                     Fin_Call :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                             Statements         => New_List (Fin_Call),
                             Exception_Handlers => New_List (
                               Build_Exception_Handler (Finalizer_Data))));
                  end if;

                  Append_To (Stmts, Fin_Call);
               end if;
            end Process_Component_For_Finalize;

            --  Local variables

            Alts       : List_Id;
            Counter_Id : Entity_Id := Empty;
            Decl       : Node_Id;
            Decl_Id    : Entity_Id;
            Decl_Typ   : Entity_Id;
            Decls      : List_Id;
            Has_POC    : Boolean;
            Jump_Block : Node_Id;
            Label      : Node_Id;
            Label_Id   : Entity_Id;
            Num_Comps  : Nat;
            Stmts      : List_Id;
            Var_Case   : Node_Id;

         --  Start of processing for Process_Component_List_For_Finalize

         begin
            --  Perform an initial check, look for controlled and per-object
            --  constrained components.

            Preprocess_Components (Comps, Num_Comps, Has_POC);

            --  Create a state counter to service the current component list.
            --  This step is performed before the variants are inspected in
            --  order to generate the same state counter names as those from
            --  Build_Initialize_Statements.

            if Num_Comps > 0 and then Is_Local then
               Counter := Counter + 1;

               Counter_Id :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name ('C', Counter));
            end if;

            --  Process the component in the following order:
            --    1) Variants
            --    2) Per-object constrained components
            --    3) Regular components

            --  Start with the variant parts

            Var_Case := Empty;
            if Present (Variant_Part (Comps)) then
               declare
                  Var_Alts : constant List_Id := New_List;
                  Var      : Node_Id;

               begin
                  Var := First_Non_Pragma (Variants (Variant_Part (Comps)));
                  while Present (Var) loop

                     --  Generate:
                     --     when <discrete choices> =>
                     --        <finalize statements>

                     Append_To (Var_Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices =>
                           New_Copy_List (Discrete_Choices (Var)),
                         Statements =>
                           Process_Component_List_For_Finalize (
                             Component_List (Var),
                             In_Variant_Part => True)));

                     Next_Non_Pragma (Var);
                  end loop;

                  --  Generate:
                  --     case V.<discriminant> is
                  --        when <discrete choices 1> =>
                  --           <finalize statements 1>
                  --        ...
                  --        when <discrete choices N> =>
                  --           <finalize statements N>
                  --     end case;

                  Var_Case :=
                    Make_Case_Statement (Loc,
                      Expression =>
                        Make_Selected_Component (Loc,
                          Prefix        => Make_Identifier (Loc, Name_V),
                          Selector_Name =>
                            Make_Identifier (Loc,
                              Chars => Chars (Name (Variant_Part (Comps))))),
                      Alternatives => Var_Alts);
               end;
            end if;

            --  The current component list does not have a single controlled
            --  component, however it may contain variants. Return the case
            --  statement for the variants or nothing.

            if Num_Comps = 0 then
               if Present (Var_Case) then
                  return New_List (Var_Case);
               else
                  return New_List (Make_Null_Statement (Loc));
               end if;
            end if;

            --  Prepare all lists

            Alts  := New_List;
            Decls := New_List;
            Stmts := New_List;

            --  Process all per-object constrained components in reverse order

            if Has_POC then
               Decl := Last_Non_Pragma (Component_Items (Comps));
               while Present (Decl) loop
                  Decl_Id  := Defining_Identifier (Decl);
                  Decl_Typ := Etype (Decl_Id);

                  --  Skip _parent

                  if Chars (Decl_Id) /= Name_uParent
                    and then Needs_Finalization (Decl_Typ)
                    and then Has_Access_Constraint (Decl_Id)
                    and then No (Expression (Decl))
                  then
                     Process_Component_For_Finalize
                       (Decl, Alts, Decls, Stmts, Num_Comps);
                  end if;

                  Prev_Non_Pragma (Decl);
               end loop;
            end if;

            if not In_Variant_Part then
               Last_POC_Call := Last (Stmts);
               --  In the case of a type extension, the deep-finalize call
               --  for the _Parent component will be inserted here.
            end if;

            --  Process the rest of the components in reverse order

            Decl := Last_Non_Pragma (Component_Items (Comps));
            while Present (Decl) loop
               Decl_Id  := Defining_Identifier (Decl);
               Decl_Typ := Etype (Decl_Id);

               --  Skip _parent

               if Chars (Decl_Id) /= Name_uParent
                 and then Needs_Finalization (Decl_Typ)
               then
                  --  Skip per-object constrained components since they were
                  --  handled in the above step.

                  if Has_Access_Constraint (Decl_Id)
                    and then No (Expression (Decl))
                  then
                     null;
                  else
                     Process_Component_For_Finalize
                       (Decl, Alts, Decls, Stmts, Num_Comps);
                  end if;
               end if;

               Prev_Non_Pragma (Decl);
            end loop;

            --  Generate:
            --    declare
            --       LN : label;        --  If Is_Local is enabled
            --       ...                    .
            --       L0 : label;            .

            --    begin                     .
            --       case CounterX is       .
            --          when N =>           .
            --             goto LN;         .
            --          ...                 .
            --          when 1 =>           .
            --             goto L1;         .
            --          when others =>      .
            --             goto L0;         .
            --       end case;              .

            --       <<LN>>             --  If Is_Local is enabled
            --          begin
            --             [Deep_]Finalize (V.CompY);
            --          exception
            --             when Id : others =>
            --                if not Raised then
            --                   Raised := True;
            --                   Save_Occurrence (E,
            --                     Get_Current_Excep.all.all);
            --                end if;
            --          end;
            --       ...
            --       <<L0>>  --  If Is_Local is enabled
            --    end;

            if Is_Local then

               --  Add the declaration of default jump location L0, its
               --  corresponding alternative and its place in the statements.

               Label_Id := Make_Identifier (Loc, New_External_Name ('L', 0));
               Set_Entity (Label_Id,
                 Make_Defining_Identifier (Loc, Chars (Label_Id)));
               Label := Make_Label (Loc, Label_Id);

               Append_To (Decls,          --  declaration
                 Make_Implicit_Label_Declaration (Loc,
                   Defining_Identifier => Entity (Label_Id),
                   Label_Construct     => Label));

               Append_To (Alts,           --  alternative
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => New_List (
                     Make_Others_Choice (Loc)),

                   Statements => New_List (
                     Make_Goto_Statement (Loc,
                       Name => New_Occurrence_Of (Entity (Label_Id), Loc)))));

               Append_To (Stmts, Label);  --  statement

               --  Create the jump block

               Prepend_To (Stmts,
                 Make_Case_Statement (Loc,
                   Expression   => Make_Identifier (Loc, Chars (Counter_Id)),
                   Alternatives => Alts));
            end if;

            Jump_Block :=
              Make_Block_Statement (Loc,
                Declarations               => Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Stmts));

            if Present (Var_Case) then
               return New_List (Var_Case, Jump_Block);
            else
               return New_List (Jump_Block);
            end if;
         end Process_Component_List_For_Finalize;

         --  Local variables

         Bod_Stmts       : List_Id := No_List;
         Finalizer_Decls : List_Id := No_List;
         Rec_Def         : Node_Id;

      --  Start of processing for Build_Finalize_Statements

      begin
         Finalizer_Decls := New_List;
         Build_Object_Declarations (Finalizer_Data, Finalizer_Decls, Loc);

         if Nkind (Typ_Def) = N_Derived_Type_Definition then
            Rec_Def := Record_Extension_Part (Typ_Def);
         else
            Rec_Def := Typ_Def;
         end if;

         --  Create a finalization sequence for all record components

         if Present (Component_List (Rec_Def)) then
            Bod_Stmts :=
              Process_Component_List_For_Finalize (Component_List (Rec_Def));
         end if;

         --  A derived record type must finalize all inherited components. This
         --  action poses the following problem:

         --    procedure Deep_Finalize (Obj : in out Parent_Typ) is
         --    begin
         --       Finalize (Obj);
         --       ...

         --    procedure Deep_Finalize (Obj : in out Derived_Typ) is
         --    begin
         --       Deep_Finalize (Obj._parent);
         --       ...
         --       Finalize (Obj);
         --       ...

         --  Finalizing the derived type will invoke Finalize of the parent and
         --  then that of the derived type. This is undesirable because both
         --  routines may modify shared components. Only the Finalize of the
         --  derived type should be invoked.

         --  To prevent this double adjustment of shared components,
         --  Deep_Finalize uses a flag to control the invocation of Finalize:

         --    procedure Deep_Finalize
         --      (Obj  : in out Some_Type;
         --       Flag : Boolean := True)
         --    is
         --    begin
         --       if Flag then
         --          Finalize (Obj);
         --       end if;
         --       ...

         --  When Deep_Finalize is invoked for field _parent, a value of False
         --  is provided for the flag:

         --    Deep_Finalize (Obj._parent, False);

         if Is_Tagged_Type (Typ) and then Is_Derived_Type (Typ) then
            declare
               Par_Typ  : constant Entity_Id := Parent_Field_Type (Typ);
               Call     : Node_Id;
               Fin_Stmt : Node_Id;

            begin
               if Needs_Finalization (Par_Typ) then
                  Call :=
                    Make_Final_Call
                      (Obj_Ref   =>
                         Make_Selected_Component (Loc,
                           Prefix        => Make_Identifier (Loc, Name_V),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_uParent)),
                       Typ       => Par_Typ,
                       Skip_Self => True);

                  --  Generate:
                  --    begin
                  --       Deep_Finalize (V._parent, False);

                  --    exception
                  --       when Id : others =>
                  --          if not Raised then
                  --             Raised := True;
                  --             Save_Occurrence (E,
                  --               Get_Current_Excep.all.all);
                  --          end if;
                  --    end;

                  if Present (Call) then
                     Fin_Stmt := Call;

                     if Exceptions_OK then
                        Fin_Stmt :=
                          Make_Block_Statement (Loc,
                            Handled_Statement_Sequence =>
                              Make_Handled_Sequence_Of_Statements (Loc,
                                Statements         => New_List (Fin_Stmt),
                                Exception_Handlers => New_List (
                                  Build_Exception_Handler
                                    (Finalizer_Data))));
                     end if;

                     --  The intended component finalization order is
                     --    1) POC components of extension
                     --    2) _Parent component
                     --    3) non-POC components of extension.
                     --
                     --  With this "finalize the parent part in the middle"
                     --  ordering, we can avoid the need for making two
                     --  calls to the parent's subprogram in the way that
                     --  is necessary for Init_Procs. This does have the
                     --  peculiar (but legal) consequence that the parent's
                     --  non-POC components are finalized before the
                     --  non-POC extension components. This violates the
                     --  usual "finalize in reverse declaration order"
                     --  principle, but that's ok (see RM 7.6.1(9)).
                     --
                     --  Last_POC_Call should be non-empty if the extension
                     --  has at least one POC. Interactions with variant
                     --  parts are incorrectly ignored.

                     if Present (Last_POC_Call) then
                        Insert_After (Last_POC_Call, Fin_Stmt);
                     else
                        --  At this point, we could look for the common case
                        --  where there are no POC components anywhere in
                        --  sight (inherited or not) and, in that common case,
                        --  call Append_To instead of Prepend_To. That would
                        --  result in finalizing the parent part after, rather
                        --  than before, the extension components. That might
                        --  be more intuitive (as discussed in preceding
                        --  comment), but it is not required.
                        Prepend_To (Bod_Stmts, Fin_Stmt);
                     end if;
                  end if;
               end if;
            end;
         end if;

         --  Finalize the object. This action must be performed first before
         --  all components have been finalized.

         if Is_Controlled (Typ) and then not Is_Local then
            declare
               Fin_Stmt : Node_Id;
               Proc     : Entity_Id;

            begin
               Proc := Find_Controlled_Prim_Op (Typ, Name_Finalize);

               --  Generate:
               --    if F then
               --       begin
               --          Finalize (V);

               --       exception
               --          when others =>
               --             if not Raised then
               --                Raised := True;
               --                Save_Occurrence (E,
               --                  Get_Current_Excep.all.all);
               --             end if;
               --       end;
               --    end if;

               if Present (Proc) then
                  Fin_Stmt :=
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => New_Occurrence_Of (Proc, Loc),
                      Parameter_Associations => New_List (
                        Make_Identifier (Loc, Name_V)));

                  if Exceptions_OK then
                     Fin_Stmt :=
                       Make_Block_Statement (Loc,
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                             Statements         => New_List (Fin_Stmt),
                             Exception_Handlers => New_List (
                               Build_Exception_Handler
                                 (Finalizer_Data))));
                  end if;

                  Prepend_To (Bod_Stmts,
                    Make_If_Statement (Loc,
                      Condition       => Make_Identifier (Loc, Name_F),
                      Then_Statements => New_List (Fin_Stmt)));
               end if;
            end;
         end if;

         --  At this point either all finalization statements have been
         --  generated or the type is not controlled.

         if No (Bod_Stmts) then
            return New_List (Make_Null_Statement (Loc));

         --  Generate:
         --    declare
         --       Abort  : constant Boolean := Triggered_By_Abort;
         --         <or>
         --       Abort  : constant Boolean := False;  --  no abort

         --       E      : Exception_Occurrence;
         --       Raised : Boolean := False;

         --    begin
         --       <finalize statements>

         --       if Raised and then not Abort then
         --          Raise_From_Controlled_Operation (E);
         --       end if;
         --    end;

         else
            if Exceptions_OK then
               Append_To (Bod_Stmts, Build_Raise_Statement (Finalizer_Data));
            end if;

            return
              New_List (
                Make_Block_Statement (Loc,
                  Declarations               =>
                    Finalizer_Decls,
                  Handled_Statement_Sequence =>
                    Make_Handled_Sequence_Of_Statements (Loc, Bod_Stmts)));
         end if;
      end Build_Finalize_Statements;

      -----------------------
      -- Parent_Field_Type --
      -----------------------

      function Parent_Field_Type (Typ : Entity_Id) return Entity_Id is
         Field : Entity_Id;

      begin
         Field := First_Entity (Typ);
         while Present (Field) loop
            if Chars (Field) = Name_uParent then
               return Etype (Field);
            end if;

            Next_Entity (Field);
         end loop;

         --  A derived tagged type should always have a parent field

         raise Program_Error;
      end Parent_Field_Type;

      ---------------------------
      -- Preprocess_Components --
      ---------------------------

      procedure Preprocess_Components
        (Comps     : Node_Id;
         Num_Comps : out Nat;
         Has_POC   : out Boolean)
      is
         Decl : Node_Id;
         Id   : Entity_Id;
         Typ  : Entity_Id;

      begin
         Num_Comps := 0;
         Has_POC   := False;

         Decl := First_Non_Pragma (Component_Items (Comps));
         while Present (Decl) loop
            Id  := Defining_Identifier (Decl);
            Typ := Etype (Id);

            --  Skip field _parent

            if Chars (Id) /= Name_uParent
              and then Needs_Finalization (Typ)
            then
               Num_Comps := Num_Comps + 1;

               if Has_Access_Constraint (Id)
                 and then No (Expression (Decl))
               then
                  Has_POC := True;
               end if;
            end if;

            Next_Non_Pragma (Decl);
         end loop;
      end Preprocess_Components;

   --  Start of processing for Make_Deep_Record_Body

   begin
      case Prim is
         when Address_Case =>
            return Make_Finalize_Address_Stmts (Typ);

         when Adjust_Case =>
            return Build_Adjust_Statements (Typ);

         when Finalize_Case =>
            return Build_Finalize_Statements (Typ);

         when Initialize_Case =>
            if Is_Controlled (Typ) then
               return New_List (
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of
                       (Find_Controlled_Prim_Op (Typ, Name_Initialize), Loc),
                   Parameter_Associations => New_List (
                     Make_Identifier (Loc, Name_V))));
            else
               return Empty_List;
            end if;
      end case;
   end Make_Deep_Record_Body;

   ----------------------
   -- Make_Final_Call --
   ----------------------

   function Make_Final_Call
     (Obj_Ref   : Node_Id;
      Typ       : Entity_Id;
      Skip_Self : Boolean := False) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (Obj_Ref);
      Atyp     : Entity_Id;
      Prot_Typ : Entity_Id := Empty;
      Fin_Id   : Entity_Id := Empty;
      Ref      : Node_Id;
      Utyp     : Entity_Id;

   begin
      Ref := Obj_Ref;

      --  Recover the proper type which contains [Deep_]Finalize

      if Is_Class_Wide_Type (Typ) then
         Utyp := Root_Type (Typ);
         Atyp := Utyp;

      elsif Is_Concurrent_Type (Typ) then
         Utyp := Corresponding_Record_Type (Typ);
         Atyp := Empty;
         Ref  := Convert_Concurrent (Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Underlying_Type (Typ))
        and then Is_Concurrent_Type (Underlying_Type (Typ))
      then
         Utyp := Corresponding_Record_Type (Underlying_Type (Typ));
         Atyp := Typ;
         Ref  := Convert_Concurrent (Ref, Underlying_Type (Typ));

      else
         Utyp := Typ;
         Atyp := Typ;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Ref);

      --  Deal with untagged derivation of private views. If the parent type
      --  is a protected type, Deep_Finalize is found on the corresponding
      --  record of the ancestor.

      if Is_Untagged_Derivation (Typ) then
         if Is_Protected_Type (Typ) then
            Utyp := Corresponding_Record_Type (Root_Type (Base_Type (Typ)));
         else
            Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));

            if Is_Protected_Type (Utyp) then
               Utyp := Corresponding_Record_Type (Utyp);
            end if;
         end if;

         Ref := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  Deal with derived private types which do not inherit primitives from
      --  their parents. In this case, [Deep_]Finalize can be found in the full
      --  view of the parent type.

      if Present (Utyp)
        and then Is_Tagged_Type (Utyp)
        and then Is_Derived_Type (Utyp)
        and then Is_Empty_Elmt_List (Primitive_Operations (Utyp))
        and then Is_Private_Type (Etype (Utyp))
        and then Present (Full_View (Etype (Utyp)))
      then
         Utyp := Full_View (Etype (Utyp));
         Ref  := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  When dealing with the completion of a private type, use the base type
      --  instead.

      if Present (Utyp) and then Utyp /= Base_Type (Utyp) then
         pragma Assert (Present (Atyp) and then Is_Private_Type (Atyp));

         Utyp := Base_Type (Utyp);
         Ref  := Unchecked_Convert_To (Utyp, Ref);
         Set_Assignment_OK (Ref);
      end if;

      --  Detect if Typ is a protected type or an expanded protected type and
      --  store the relevant type within Prot_Typ for later processing.

      if Is_Protected_Type (Typ) then
         Prot_Typ := Typ;

      elsif Ekind (Typ) = E_Record_Type
        and then Present (Corresponding_Concurrent_Type (Typ))
        and then Is_Protected_Type (Corresponding_Concurrent_Type (Typ))
      then
         Prot_Typ := Corresponding_Concurrent_Type (Typ);
      end if;

      --  The underlying type may not be present due to a missing full view. In
      --  this case freezing did not take place and there is no [Deep_]Finalize
      --  primitive to call.

      if No (Utyp) then
         return Empty;

      elsif Skip_Self then
         if Has_Controlled_Component (Utyp) then
            if Is_Tagged_Type (Utyp) then
               Fin_Id := Find_Optional_Prim_Op (Utyp, TSS_Deep_Finalize);
            else
               Fin_Id := TSS (Utyp, TSS_Deep_Finalize);
            end if;
         end if;

      --  Class-wide types, interfaces and types with controlled components

      elsif Is_Class_Wide_Type (Typ)
        or else Is_Interface (Typ)
        or else Has_Controlled_Component (Utyp)
      then
         if Is_Tagged_Type (Utyp) then
            Fin_Id := Find_Optional_Prim_Op (Utyp, TSS_Deep_Finalize);
         else
            Fin_Id := TSS (Utyp, TSS_Deep_Finalize);
         end if;

      --  Derivations from [Limited_]Controlled

      elsif Is_Controlled (Utyp) then
         Fin_Id := Find_Controlled_Prim_Op (Utyp, Name_Finalize);

      --  Tagged types

      elsif Is_Tagged_Type (Utyp) then
         Fin_Id := Find_Optional_Prim_Op (Utyp, TSS_Deep_Finalize);

      --  Protected types: these also require finalization even though they
      --  are not marked controlled explicitly.

      elsif Present (Prot_Typ) then
         --  Protected objects do not need to be finalized on restricted
         --  runtimes.

         if Restricted_Profile then
            return Empty;

         --  ??? Only handle the simple case for now. Will not support a record
         --  or array containing protected objects.

         elsif Is_Simple_Protected_Type (Prot_Typ) then
            Fin_Id := RTE (RE_Finalize_Protection);
         else
            raise Program_Error;
         end if;

      else
         raise Program_Error;
      end if;

      if Present (Fin_Id) then

         --  When finalizing a class-wide object, do not convert to the root
         --  type in order to produce a dispatching call.

         if Is_Class_Wide_Type (Typ) then
            null;

         --  Ensure that a finalization routine is at least decorated in order
         --  to inspect the object parameter.

         elsif Analyzed (Fin_Id)
           or else Ekind (Fin_Id) = E_Procedure
         then
            --  In certain cases, such as the creation of Stream_Read, the
            --  visible entity of the type is its full view. Since Stream_Read
            --  will have to create an object of type Typ, the local object
            --  will be finalzed by the scope finalizer generated later on. The
            --  object parameter of Deep_Finalize will always use the private
            --  view of the type. To avoid such a clash between a private and a
            --  full view, perform an unchecked conversion of the object
            --  reference to the private view.

            declare
               Formal_Typ : constant Entity_Id :=
                              Etype (First_Formal (Fin_Id));
            begin
               if Is_Private_Type (Formal_Typ)
                 and then Present (Full_View (Formal_Typ))
                 and then Full_View (Formal_Typ) = Utyp
               then
                  Ref := Unchecked_Convert_To (Formal_Typ, Ref);
               end if;
            end;

            Ref := Convert_View (Fin_Id, Ref, Typ);
         end if;

         return
           Make_Call (Loc,
             Proc_Id   => Fin_Id,
             Param     => Ref,
             Skip_Self => Skip_Self);
      else
         pragma Assert (Serious_Errors_Detected > 0
                        or else not Has_Controlled_Component (Utyp));
         return Empty;
      end if;
   end Make_Final_Call;

   --------------------------------
   -- Make_Finalize_Address_Body --
   --------------------------------

   procedure Make_Finalize_Address_Body (Typ : Entity_Id) is
      Is_Task : constant Boolean :=
                  Ekind (Typ) = E_Record_Type
                    and then Is_Concurrent_Record_Type (Typ)
                    and then Ekind (Corresponding_Concurrent_Type (Typ)) =
                                                                 E_Task_Type;
      Loc     : constant Source_Ptr := Sloc (Typ);
      Proc_Id : Entity_Id;
      Stmts   : List_Id;

   begin
      --  The corresponding records of task types are not controlled by design.
      --  For the sake of completeness, create an empty Finalize_Address to be
      --  used in task class-wide allocations.

      if Is_Task then
         null;

      --  Nothing to do if the type does not need finalization or already has
      --  a TSS entry for Finalize_Address. Skip class-wide subtypes that do
      --  not come from source, as they are usually generated for completeness
      --  and need no Finalize_Address.

      elsif not Needs_Finalization (Typ)
        or else Present (TSS (Typ, TSS_Finalize_Address))
        or else
          (Is_Class_Wide_Type (Typ)
            and then Ekind (Root_Type (Typ)) = E_Record_Subtype
            and then not Comes_From_Source (Root_Type (Typ)))
      then
         return;
      end if;

      --  Do not generate Finalize_Address routine for CodePeer

      if CodePeer_Mode then
         return;
      end if;

      Proc_Id :=
        Make_Defining_Identifier (Loc,
          Make_TSS_Name (Typ, TSS_Finalize_Address));

      --  Generate:

      --    procedure <Typ>FD (V : System.Address) is
      --    begin
      --       null;                            --  for tasks

      --       declare                          --  for all other types
      --          type Pnn is access all Typ;
      --          for Pnn'Storage_Size use 0;
      --       begin
      --          [Deep_]Finalize (Pnn (V).all);
      --       end;
      --    end TypFD;

      if Is_Task then
         Stmts := New_List (Make_Null_Statement (Loc));
      else
         Stmts := Make_Finalize_Address_Stmts (Typ);
      end if;

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Proc_Id,

              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Name_V),
                  Parameter_Type =>
                    New_Occurrence_Of (RTE (RE_Address), Loc)))),

          Declarations => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts)));

      --  If the type has relaxed semantics for finalization, the indirect
      --  calls to Finalize_Address may be turned into direct ones and, in
      --  this case, inlining them is generally profitable.

      if Has_Relaxed_Finalization (Typ) then
         Set_Is_Inlined (Proc_Id);
      end if;

      Set_TSS (Typ, Proc_Id);
   end Make_Finalize_Address_Body;

   ---------------------------------
   -- Make_Finalize_Address_Stmts --
   ---------------------------------

   function Make_Finalize_Address_Stmts (Typ : Entity_Id) return List_Id is
      Loc : constant Source_Ptr := Sloc (Typ);

      Decls     : List_Id;
      Desig_Typ : Entity_Id;
      Fin_Block : Node_Id;
      Fin_Call  : Node_Id;
      Obj_Expr  : Node_Id;
      Ptr_Typ   : Entity_Id;

   begin
      if Is_Array_Type (Typ) then
         if Is_Constrained (First_Subtype (Typ)) then
            Desig_Typ := First_Subtype (Typ);
         else
            Desig_Typ := Base_Type (Typ);
         end if;

      --  Class-wide types of constrained root types

      elsif Is_Class_Wide_Type (Typ)
        and then Has_Discriminants (Root_Type (Typ))
        and then not
          Is_Empty_Elmt_List (Discriminant_Constraint (Root_Type (Typ)))
      then
         declare
            Parent_Typ  : Entity_Id;
            Parent_Utyp : Entity_Id;

         begin
            --  Climb the parent type chain looking for a non-constrained type

            Parent_Typ := Root_Type (Typ);
            while Parent_Typ /= Etype (Parent_Typ)
              and then Has_Discriminants (Parent_Typ)
              and then not
                Is_Empty_Elmt_List (Discriminant_Constraint (Parent_Typ))
            loop
               Parent_Typ := Etype (Parent_Typ);
            end loop;

            --  Handle views created for tagged types with unknown
            --  discriminants.

            if Is_Underlying_Record_View (Parent_Typ) then
               Parent_Typ := Underlying_Record_View (Parent_Typ);
            end if;

            Parent_Utyp := Underlying_Type (Parent_Typ);

            --  Handle views created for a synchronized private extension with
            --  known, non-defaulted discriminants. In that case, parent_typ
            --  will be the private extension, as it is the first "non
            --  -constrained" type in the parent chain. Unfortunately, the
            --  underlying type, being a protected or task type, is not the
            --  "real" type needing finalization. Rather, the "corresponding
            --  record type" should be the designated type here. In fact, TSS
            --  finalizer generation is specifically skipped for the nominal
            --  class-wide type of (the full view of) a concurrent type (see
            --  exp_ch7.Expand_Freeze_Class_Wide_Type). If we don't designate
            --  the underlying record (Tprot_typeVC), we will end up trying to
            --  dispatch to prot_typeVDF from an incorrectly designated
            --  Tprot_typeC, which is, of course, not actually a member of
            --  prot_typeV'Class, and thus incompatible.

            if Ekind (Parent_Utyp) in Concurrent_Kind
              and then Present (Corresponding_Record_Type (Parent_Utyp))
            then
               Parent_Utyp := Corresponding_Record_Type (Parent_Utyp);
            end if;

            Desig_Typ := Class_Wide_Type (Parent_Utyp);
         end;

      --  General case

      else
         Desig_Typ := Typ;
      end if;

      --  Generate:
      --    type Ptr_Typ is access all Typ;
      --    for Ptr_Typ'Storage_Size use 0;

      Ptr_Typ := Make_Temporary (Loc, 'P');

      Decls := New_List (
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication => New_Occurrence_Of (Desig_Typ, Loc))),

        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Occurrence_Of (Ptr_Typ, Loc),
          Chars      => Name_Storage_Size,
          Expression => Make_Integer_Literal (Loc, 0)));

      Obj_Expr := Make_Identifier (Loc, Name_V);

      --  Unconstrained arrays require special processing in order to retrieve
      --  the elements. To achieve this, we have to skip the dope vector which
      --  lays in front of the elements and then use a thin pointer to perform
      --  the address-to-access conversion.

      if Is_Array_Type (Typ)
        and then not Is_Constrained (First_Subtype (Typ))
      then
         declare
            Dope_Id : Entity_Id;

         begin
            --  Ensure that Ptr_Typ is a thin pointer; generate:
            --    for Ptr_Typ'Size use System.Address'Size;

            Append_To (Decls,
              Make_Attribute_Definition_Clause (Loc,
                Name       => New_Occurrence_Of (Ptr_Typ, Loc),
                Chars      => Name_Size,
                Expression =>
                  Make_Integer_Literal (Loc, System_Address_Size)));

            --  Generate:
            --    Dnn : constant Storage_Offset :=
            --            Desig_Typ'Descriptor_Size / Storage_Unit;

            Dope_Id := Make_Temporary (Loc, 'D');

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Dope_Id,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Storage_Offset), Loc),
                Expression          =>
                  Make_Op_Divide (Loc,
                    Left_Opnd  =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Desig_Typ, Loc),
                        Attribute_Name => Name_Descriptor_Size),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc, System_Storage_Unit))));

            --  Shift the address from the start of the dope vector to the
            --  start of the elements:
            --
            --    V + Dnn

            Obj_Expr :=
              Make_Function_Call (Loc,
                Name                   =>
                  Make_Expanded_Name (Loc,
                    Chars => Name_Op_Add,
                    Prefix =>
                      New_Occurrence_Of
                        (RTU_Entity (System_Storage_Elements), Loc),
                    Selector_Name =>
                      Make_Identifier (Loc, Name_Op_Add)),
                Parameter_Associations => New_List (
                  Obj_Expr,
                  New_Occurrence_Of (Dope_Id, Loc)));
         end;
      end if;

      Fin_Call :=
        Make_Final_Call (
          Obj_Ref =>
            Make_Explicit_Dereference (Loc,
              Prefix => Unchecked_Convert_To (Ptr_Typ, Obj_Expr)),
          Typ     => Desig_Typ);

      if Present (Fin_Call) then
         Fin_Block :=
           Make_Block_Statement (Loc,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Fin_Call)));

      --  Otherwise previous errors or a missing full view may prevent the
      --  proper freezing of the designated type. If this is the case, there
      --  is no [Deep_]Finalize primitive to call.

      else
         Fin_Block := Make_Null_Statement (Loc);
      end if;

      return New_List (Fin_Block);
   end Make_Finalize_Address_Stmts;

   ---------------------------------
   -- Make_Finalize_Call_For_Node --
   ---------------------------------

   function Make_Finalize_Call_For_Node
     (Loc  : Source_Ptr;
      Node : Entity_Id) return Node_Id
   is
      Fin_Id : constant Entity_Id := Finalize_Address_For_Node (Node);

      Fin_Call : Node_Id;
      Fin_Ref  : Node_Id;

   begin
      --  Finalize_Address is not generated in CodePeer mode because the
      --  body contains address arithmetic. So we don't want to generate
      --  the call in this case.

      if CodePeer_Mode then
         return Make_Null_Statement (Loc);
      end if;

      --  The Finalize_Address primitive may be missing when the Master_Node
      --  is written down in the source code for testing purposes.

      if Present (Fin_Id) then
         Fin_Ref :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Fin_Id, Loc),
             Attribute_Name => Name_Unrestricted_Access);

      else
         Fin_Ref :=
           Make_Selected_Component (Loc,
             Prefix        => New_Occurrence_Of (Node, Loc),
             Selector_Name => Make_Identifier (Loc, Name_Finalize_Address));
      end if;

      Fin_Call :=
        Make_Procedure_Call_Statement (Loc,
           Name                   =>
             New_Occurrence_Of (RTE (RE_Finalize_Object), Loc),
           Parameter_Associations => New_List (
             New_Occurrence_Of (Node, Loc),
             Fin_Ref));

      --  Present Finalize_Address procedure to the back end so that it can
      --  inline the call to the procedure made by Finalize_Object.

      if Present (Fin_Id) and then Is_Inlined (Fin_Id) then
         Add_Inlined_Body (Fin_Id, Fin_Call);
      end if;

      return Fin_Call;
   end Make_Finalize_Call_For_Node;

   -------------------------------------
   -- Make_Handler_For_Ctrl_Operation --
   -------------------------------------

   --  Generate:

   --    when E : others =>
   --      Raise_From_Controlled_Operation (E);

   --  or:

   --    when others =>
   --      raise Program_Error [finalize raised exception];

   --  depending on whether Raise_From_Controlled_Operation is available

   function Make_Handler_For_Ctrl_Operation
     (Loc : Source_Ptr) return Node_Id
   is
      E_Occ : Entity_Id;
      --  Choice parameter (for the first case above)

      Raise_Node : Node_Id;
      --  Procedure call or raise statement

   begin
      --  Standard run-time: add choice parameter E and pass it to
      --  Raise_From_Controlled_Operation so that the original exception
      --  name and message can be recorded in the exception message for
      --  Program_Error.

      if RTE_Available (RE_Raise_From_Controlled_Operation) then
         E_Occ := Make_Defining_Identifier (Loc, Name_E);
         Raise_Node :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of
                 (RTE (RE_Raise_From_Controlled_Operation), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (E_Occ, Loc)));

      --  Restricted run-time: exception messages are not supported

      else
         E_Occ := Empty;
         Raise_Node :=
           Make_Raise_Program_Error (Loc,
             Reason => PE_Finalize_Raised_Exception);
      end if;

      return
        Make_Implicit_Exception_Handler (Loc,
          Exception_Choices => New_List (Make_Others_Choice (Loc)),
          Choice_Parameter  => E_Occ,
          Statements        => New_List (Raise_Node));
   end Make_Handler_For_Ctrl_Operation;

   --------------------
   -- Make_Init_Call --
   --------------------

   function Make_Init_Call
     (Obj_Ref : Node_Id;
      Typ     : Entity_Id) return Node_Id
   is
      Loc     : constant Source_Ptr := Sloc (Obj_Ref);
      Is_Conc : Boolean;
      Proc    : Entity_Id;
      Ref     : Node_Id;
      Utyp    : Entity_Id;

   begin
      Ref := Obj_Ref;

      --  Deal with the type and object reference. Depending on the context, an
      --  object reference may need several conversions.

      if Is_Concurrent_Type (Typ) then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Typ);
         Ref     := Convert_Concurrent (Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Underlying_Type (Typ))
      then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Underlying_Type (Typ));
         Ref     := Convert_Concurrent (Ref, Underlying_Type (Typ));

      else
         Is_Conc := False;
         Utyp    := Typ;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Ref);

      --  Deal with untagged derivation of private views

      if Is_Untagged_Derivation (Typ) and then not Is_Conc then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Ref  := Unchecked_Convert_To (Utyp, Ref);

         --  The following is to prevent problems with UC see 1.156 RH ???

         Set_Assignment_OK (Ref);
      end if;

      --  If the underlying_type is a subtype, then we are dealing with the
      --  completion of a private type. We need to access the base type and
      --  generate a conversion to it.

      if Present (Utyp) and then Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Ref  := Unchecked_Convert_To (Utyp, Ref);
      end if;

      --  The underlying type may not be present due to a missing full view.
      --  In this case freezing did not take place and there is no suitable
      --  [Deep_]Initialize primitive to call.
      --  If Typ is protected then no additional processing is needed either.

      if No (Utyp)
        or else Is_Protected_Type (Typ)
      then
         return Empty;
      end if;

      --  Select the appropriate version of initialize

      if Has_Controlled_Component (Utyp) then
         Proc := TSS (Utyp, TSS_Deep_Initialize);
      elsif Is_Mutably_Tagged_Type (Utyp) then
         Proc := Find_Controlled_Prim_Op (Etype (Utyp), Name_Initialize);
         Check_Visibly_Controlled (Initialize_Case, Etype (Typ), Proc, Ref);
      else
         Proc := Find_Controlled_Prim_Op (Utyp, Name_Initialize);
         Check_Visibly_Controlled (Initialize_Case, Typ, Proc, Ref);
      end if;

      --  If initialization procedure for an array of controlled objects is
      --  trivial, do not generate a useless call to it.
      --  The initialization procedure may be missing altogether in the case
      --  of a derived container whose components have trivial initialization.

      if No (Proc)
        or else (Is_Array_Type (Utyp) and then Is_Trivial_Subprogram (Proc))
        or else
          (not Comes_From_Source (Proc)
            and then Present (Alias (Proc))
            and then Is_Trivial_Subprogram (Alias (Proc)))
      then
         return Empty;
      end if;

      --  The object reference may need another conversion depending on the
      --  type of the formal and that of the actual.

      Ref := Convert_View (Proc, Ref, Typ);

      --  Generate:
      --    [Deep_]Initialize (Ref);

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc, Loc),
          Parameter_Associations => New_List (Ref));
   end Make_Init_Call;

   ------------------------------
   -- Make_Local_Deep_Finalize --
   ------------------------------

   function Make_Local_Deep_Finalize
     (Typ : Entity_Id;
      Nam : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);
      Formals : List_Id;

   begin
      Formals := New_List (

         --  V : in out Typ

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          In_Present          => True,
          Out_Present         => True,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)),

         --  F : Boolean := True

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_F),
          Parameter_Type      => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_True, Loc)));

      --  Add the necessary number of counters to represent the initialization
      --  state of an object.

      return
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Nam,
              Parameter_Specifications => Formals),

          Declarations => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Make_Deep_Record_Body (Finalize_Case, Typ, True)));
   end Make_Local_Deep_Finalize;

   ----------------------------------
   -- Make_Master_Node_Declaration --
   ----------------------------------

   function Make_Master_Node_Declaration
     (Loc         : Source_Ptr;
      Master_Node : Entity_Id;
      Obj         : Entity_Id) return Node_Id
   is
   begin
      Set_Finalization_Master_Node (Obj, Master_Node);

      return
        Make_Object_Declaration (Loc,
          Defining_Identifier => Master_Node,
          Aliased_Present     => True,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Master_Node), Loc));
   end Make_Master_Node_Declaration;

   ----------------------------------------
   -- Make_Suppress_Object_Finalize_Call --
   ----------------------------------------

   function Make_Suppress_Object_Finalize_Call
     (Loc : Source_Ptr;
      Obj : Entity_Id) return Node_Id
   is
      Obj_Decl : constant Node_Id := Declaration_Node (Obj);

      Master_Node_Decl : Node_Id;
      Master_Node_Id   : Entity_Id;

   begin
      --  Create the declaration of the Master_Node for the object and
      --  insert it before the declaration of the object itself.

      if Present (Finalization_Master_Node (Obj)) then
         Master_Node_Id := Finalization_Master_Node (Obj);

      else
         Master_Node_Id := Make_Temporary (Loc, 'N');
         Master_Node_Decl :=
           Make_Master_Node_Declaration (Loc, Master_Node_Id, Obj);
         Insert_Before_And_Analyze (Obj_Decl, Master_Node_Decl);

         --  Generate the attachment of the object to the Master_Node

         Attach_Object_To_Master_Node (Obj_Decl, Master_Node_Id);

         --  Mark the object to avoid double finalization

         Set_Is_Ignored_For_Finalization (Obj);
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Suppress_Object_Finalize_At_End), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Master_Node_Id, Loc)));
   end Make_Suppress_Object_Finalize_Call;

   --------------------------
   -- Make_Transient_Block --
   --------------------------

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id;
      Par    : Node_Id) return Node_Id
   is
      function Manages_Sec_Stack (Id : Entity_Id) return Boolean;
      --  Determine whether scoping entity Id manages the secondary stack

      function Within_Loop_Statement (N : Node_Id) return Boolean;
      --  Return True when N appears within a loop and no block is containing N

      -----------------------
      -- Manages_Sec_Stack --
      -----------------------

      function Manages_Sec_Stack (Id : Entity_Id) return Boolean is
      begin
         case Ekind (Id) is

            --  An exception handler with a choice parameter utilizes a dummy
            --  block to provide a declarative region. Such a block should not
            --  be considered because it never manifests in the tree and can
            --  never release the secondary stack.

            when E_Block =>
               return
                 Uses_Sec_Stack (Id) and then not Is_Exception_Handler (Id);

            when E_Entry
               | E_Entry_Family
               | E_Function
               | E_Procedure
            =>
               return Uses_Sec_Stack (Id);

            when others =>
               return False;
         end case;
      end Manages_Sec_Stack;

      ---------------------------
      -- Within_Loop_Statement --
      ---------------------------

      function Within_Loop_Statement (N : Node_Id) return Boolean is
         Par : Node_Id := Parent (N);

      begin
         while Nkind (Par) not in
           N_Handled_Sequence_Of_Statements | N_Loop_Statement |
           N_Package_Specification          | N_Proper_Body
         loop
            pragma Assert (Present (Par));
            Par := Parent (Par);
         end loop;

         return Nkind (Par) = N_Loop_Statement;
      end Within_Loop_Statement;

      --  Local variables

      Decls    : constant List_Id   := New_List;
      Instrs   : constant List_Id   := New_List (Action);
      Trans_Id : constant Entity_Id := Current_Scope;

      Block  : Node_Id;
      Insert : Node_Id;
      Scop   : Entity_Id;

   --  Start of processing for Make_Transient_Block

   begin
      --  Even though the transient block is tasked with managing the secondary
      --  stack, the block may forgo this functionality depending on how the
      --  secondary stack is managed by enclosing scopes.

      if Manages_Sec_Stack (Trans_Id) then

         --  Determine whether an enclosing scope already manages the secondary
         --  stack.

         Scop := Scope (Trans_Id);
         while Present (Scop) loop

            --  It should not be possible to reach Standard without hitting one
            --  of the other cases first unless Standard was manually pushed.

            if Scop = Standard_Standard then
               exit;

            --  The transient block is within a function which returns on the
            --  secondary stack. Take a conservative approach and assume that
            --  the value on the secondary stack is part of the result. Note
            --  that it is not possible to detect this dependency without flow
            --  analysis which the compiler does not have. Letting the object
            --  live longer than the transient block will not leak any memory
            --  because the caller will reclaim the total storage used by the
            --  function.

            elsif Ekind (Scop) = E_Function
              and then Sec_Stack_Needed_For_Return (Scop)
            then
               Set_Uses_Sec_Stack (Trans_Id, False);
               exit;

            --  The transient block must manage the secondary stack when the
            --  block appears within a loop in order to reclaim the memory at
            --  each iteration.

            elsif Ekind (Scop) = E_Loop then
               exit;

            --  Ditto when the block appears without a block that does not
            --  manage the secondary stack and is located within a loop.

            elsif Ekind (Scop) = E_Block
              and then not Manages_Sec_Stack (Scop)
              and then Present (Block_Node (Scop))
              and then Within_Loop_Statement (Block_Node (Scop))
            then
               exit;

            --  The transient block does not need to manage the secondary stack
            --  when there is an enclosing construct which already does that.
            --  This optimization saves on SS_Mark and SS_Release calls but may
            --  allow objects to live a little longer than required.

            --  The transient block must manage the secondary stack when switch
            --  -gnatd.s (strict management) is in effect.

            elsif Manages_Sec_Stack (Scop) and then not Debug_Flag_Dot_S then
               Set_Uses_Sec_Stack (Trans_Id, False);
               exit;

            --  Prevent the search from going too far because transient blocks
            --  are bounded by packages and subprogram scopes.

            elsif Ekind (Scop) in E_Entry
                                | E_Entry_Family
                                | E_Function
                                | E_Package
                                | E_Procedure
                                | E_Subprogram_Body
            then
               exit;
            end if;

            Scop := Scope (Scop);
         end loop;
      end if;

      --  Create the transient block. Set the parent now since the block itself
      --  is not part of the tree. The current scope is the E_Block entity that
      --  has been pushed by Establish_Transient_Scope.

      pragma Assert (Ekind (Trans_Id) = E_Block);

      Block :=
        Make_Block_Statement (Loc,
          Identifier                 => New_Occurrence_Of (Trans_Id, Loc),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Instrs),
          Has_Created_Identifier     => True);
      Set_Parent (Block, Par);

      --  Insert actions stuck in the transient scopes as well as all freezing
      --  nodes needed by those actions. Do not insert cleanup actions here,
      --  they will be transferred to the newly created block.

      Insert_Actions_In_Scope_Around
        (Action, Clean => False, Manage_SS => False);

      Insert := Prev (Action);

      if Present (Insert) then
         Freeze_All (First_Entity (Trans_Id), Insert);
      end if;

      --  Transfer cleanup actions to the newly created block

      declare
         Cleanup_Actions : List_Id
           renames Scope_Stack.Table (Scope_Stack.Last).
                     Actions_To_Be_Wrapped (Cleanup);
      begin
         Set_Cleanup_Actions (Block, Cleanup_Actions);
         Cleanup_Actions := No_List;
      end;

      --  When the transient scope was established, we pushed the entry for the
      --  transient scope onto the scope stack, so that the scope was active
      --  for the installation of finalizable entities etc. Now we must remove
      --  this entry, since we have constructed a proper block.

      Pop_Scope;

      return Block;
   end Make_Transient_Block;

   ------------------------
   -- Node_To_Be_Wrapped --
   ------------------------

   function Node_To_Be_Wrapped return Node_Id is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Node_To_Be_Wrapped;
   end Node_To_Be_Wrapped;

   ----------------------------
   -- Store_Actions_In_Scope --
   ----------------------------

   procedure Store_Actions_In_Scope (AK : Scope_Action_Kind; L : List_Id) is
      SE      : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);
      Actions : List_Id renames SE.Actions_To_Be_Wrapped (AK);

   begin
      if Is_Empty_List (Actions) then
         Actions := L;

         if Is_List_Member (SE.Node_To_Be_Wrapped) then
            Set_Parent (L, Parent (SE.Node_To_Be_Wrapped));
         else
            Set_Parent (L, SE.Node_To_Be_Wrapped);
         end if;

         Analyze_List (L);

      elsif AK = Before then
         Insert_List_After_And_Analyze (Last (Actions), L);

      else
         Insert_List_Before_And_Analyze (First (Actions), L);
      end if;
   end Store_Actions_In_Scope;

   ----------------------------------
   -- Store_After_Actions_In_Scope --
   ----------------------------------

   procedure Store_After_Actions_In_Scope (L : List_Id) is
   begin
      Store_Actions_In_Scope (After, L);
   end Store_After_Actions_In_Scope;

   -----------------------------------
   -- Store_Before_Actions_In_Scope --
   -----------------------------------

   procedure Store_Before_Actions_In_Scope (L : List_Id) is
   begin
      Store_Actions_In_Scope (Before, L);
   end Store_Before_Actions_In_Scope;

   -----------------------------------
   -- Store_Cleanup_Actions_In_Scope --
   -----------------------------------

   procedure Store_Cleanup_Actions_In_Scope (L : List_Id) is
   begin
      Store_Actions_In_Scope (Cleanup, L);
   end Store_Cleanup_Actions_In_Scope;

   ------------------
   -- Unnest_Block --
   ------------------

   procedure Unnest_Block (Decl : Node_Id) is
      Loc        : constant Source_Ptr := Sloc (Decl);
      Ent        : Entity_Id;
      Local_Body : Node_Id;
      Local_Call : Node_Id;
      Local_Proc : Entity_Id;
      Local_Scop : Entity_Id;

   begin
      Local_Scop := Entity (Identifier (Decl));
      Ent := First_Entity (Local_Scop);

      Local_Proc := Make_Temporary (Loc, 'P');

      Local_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Local_Proc),
              Declarations       => Declarations (Decl),
          Handled_Statement_Sequence =>
            Handled_Statement_Sequence (Decl),
          At_End_Proc                => New_Copy_Tree (At_End_Proc (Decl)));

      --  Handlers in the block may contain nested subprograms that require
      --  unnesting.

      Check_Unnesting_In_Handlers (Local_Body);

      Rewrite (Decl, Local_Body);
      Analyze (Decl);
      Set_Has_Nested_Subprogram (Local_Proc);

      Local_Call :=
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Local_Proc, Loc));

      Insert_After (Decl, Local_Call);
      Analyze (Local_Call);

      --  The new subprogram has the same scope as the original block

      Set_Scope (Local_Proc, Scope (Local_Scop));

      --  And the entity list of the new procedure is that of the block

      Set_First_Entity (Local_Proc, Ent);

      --  Reset the scopes of all the entities to the new procedure

      while Present (Ent) loop
         Set_Scope (Ent, Local_Proc);
         Next_Entity (Ent);
      end loop;
   end Unnest_Block;

   -------------------------
   -- Unnest_If_Statement --
   -------------------------

   procedure Unnest_If_Statement (If_Stmt : Node_Id) is

      procedure Check_Stmts_For_Subp_Unnesting (Stmts : in out List_Id);
      --  A list of statements (that may be a list associated with a then,
      --  elsif, or else part of an if-statement) is traversed at the top
      --  level to determine whether it contains a subprogram body, and if so,
      --  the statements will be replaced with a new procedure body containing
      --  the statements followed by a call to the procedure. The individual
      --  statements may also be blocks, loops, or other if statements that
      --  themselves may require contain nested subprograms needing unnesting.

      procedure Check_Stmts_For_Subp_Unnesting (Stmts : in out List_Id) is
         Subp_Found : Boolean := False;

      begin
         if Is_Empty_List (Stmts) then
            return;
         end if;

         declare
            Stmt : Node_Id := First (Stmts);
         begin
            while Present (Stmt) loop
               if Nkind (Stmt) = N_Subprogram_Body then
                  Subp_Found := True;
                  exit;
               end if;

               Next (Stmt);
            end loop;
         end;

         --  The statements themselves may be blocks, loops, etc. that in turn
         --  contain nested subprograms requiring an unnesting transformation.
         --  We perform this traversal after looking for subprogram bodies, to
         --  avoid considering procedures created for one of those statements
         --  (such as a block rewritten as a procedure) as a nested subprogram
         --  of the statement list (which could result in an unneeded wrapper
         --  procedure).

         Check_Unnesting_In_Decls_Or_Stmts (Stmts);

         --  If there was a top-level subprogram body in the statement list,
         --  then perform an unnesting transformation on the list by replacing
         --  the statements with a wrapper procedure body containing the
         --  original statements followed by a call to that procedure.

         if Subp_Found then
            Unnest_Statement_List (Stmts);
         end if;
      end Check_Stmts_For_Subp_Unnesting;

      --  Local variables

      Then_Stmts : List_Id := Then_Statements (If_Stmt);
      Else_Stmts : List_Id := Else_Statements (If_Stmt);

   --  Start of processing for Unnest_If_Statement

   begin
      Check_Stmts_For_Subp_Unnesting (Then_Stmts);
      Set_Then_Statements (If_Stmt, Then_Stmts);

      if not Is_Empty_List (Elsif_Parts (If_Stmt)) then
         declare
            Elsif_Part  : Node_Id :=
                            First (Elsif_Parts (If_Stmt));
            Elsif_Stmts : List_Id;
         begin
            while Present (Elsif_Part) loop
               Elsif_Stmts := Then_Statements (Elsif_Part);

               Check_Stmts_For_Subp_Unnesting (Elsif_Stmts);
               Set_Then_Statements (Elsif_Part, Elsif_Stmts);

               Next (Elsif_Part);
            end loop;
         end;
      end if;

      Check_Stmts_For_Subp_Unnesting (Else_Stmts);
      Set_Else_Statements (If_Stmt, Else_Stmts);
   end Unnest_If_Statement;

   -----------------
   -- Unnest_Loop --
   -----------------

   procedure Unnest_Loop (Loop_Stmt : Node_Id) is

      procedure Fixup_Inner_Scopes (Loop_Or_Block : Node_Id);
      --  This procedure fixes the scope for 2 identified cases of incorrect
      --  scope information.
      --
      --  1) The loops created by the compiler for array aggregates can have
      --  nested finalization procedure when the type of the array components
      --  needs finalization. It has the following form:

      --  for J4b in 10 .. 12 loop
      --     declare
      --        procedure __finalizer;
      --     begin
      --        procedure __finalizer is
      --          ...
      --        end;
      --        ...
      --        obj (J4b) := ...;

      --  When the compiler creates the N_Block_Statement, it sets its scope to
      --  the outer scope (the one containing the loop).

      --  The Unnest_Loop procedure moves the N_Loop_Statement inside a new
      --  procedure and correctly sets the scopes for both the new procedure
      --  and the loop entity. The inner block scope is not modified and this
      --  leaves the Tree in an incoherent state (i.e. the inner procedure must
      --  have its enclosing procedure in its scope ancestries).

      --  2) The second case happens when an object declaration is created
      --  within a loop used to initialize the 'others' components of an
      --  aggregate that is nested within a transient scope. When the transient
      --  scope is removed, the object scope is set to the outer scope. For
      --  example:

      --  package pack
      --   ...
      --     L98s : for J90s in 2 .. 19 loop
      --        B101s : declare
      --           R92s : aliased some_type;
      --           ...

      --  The loop L98s was initially wrapped in a transient scope B72s and
      --  R92s was nested within it. Then the transient scope is removed and
      --  the scope of R92s is set to 'pack'. And finally, when the unnester
      --  moves the loop body in a new procedure, R92s's scope is still left
      --  unchanged.

      --  This procedure finds the two previous patterns and fixes the scope
      --  information.

      --  Another (better) fix would be to have the block scope set to be the
      --  loop entity earlier (when the block is created or when the loop gets
      --  an actual entity set). But unfortunately this proved harder to
      --  implement ???

      procedure Fixup_Inner_Scopes (Loop_Or_Block : Node_Id) is
         Stmt              : Node_Id;
         Loop_Or_Block_Ent : Entity_Id;
         Ent_To_Fix        : Entity_Id;
         Decl              : Node_Id := Empty;
      begin
         pragma Assert (Nkind (Loop_Or_Block) in
           N_Loop_Statement | N_Block_Statement);

         Loop_Or_Block_Ent := Entity (Identifier (Loop_Or_Block));
         if Nkind (Loop_Or_Block) = N_Loop_Statement then
            Stmt := First (Statements (Loop_Or_Block));
         else -- N_Block_Statement
            Stmt := First
              (Statements (Handled_Statement_Sequence (Loop_Or_Block)));
            Decl := First (Declarations (Loop_Or_Block));
         end if;

         --  Fix scopes for any object declaration found in the block
         while Present (Decl) loop
            if Nkind (Decl) = N_Object_Declaration then
               Ent_To_Fix := Defining_Identifier (Decl);
               Set_Scope (Ent_To_Fix, Loop_Or_Block_Ent);
            end if;
            Next (Decl);
         end loop;

         while Present (Stmt) loop
            if Nkind (Stmt) = N_Block_Statement
              and then Is_Abort_Block (Stmt)
            then
               Ent_To_Fix := Entity (Identifier (Stmt));
               Set_Scope (Ent_To_Fix, Loop_Or_Block_Ent);
            elsif Nkind (Stmt) in N_Block_Statement | N_Loop_Statement
            then
               Fixup_Inner_Scopes (Stmt);
            end if;
            Next (Stmt);
         end loop;
      end Fixup_Inner_Scopes;

      Loc        : constant Source_Ptr := Sloc (Loop_Stmt);
      Ent        : Entity_Id;
      Local_Body : Node_Id;
      Local_Call : Node_Id;
      Loop_Ent   : Entity_Id;
      Local_Proc : Entity_Id;
      Loop_Copy  : constant Node_Id :=
                     Relocate_Node (Loop_Stmt);
   begin
      Loop_Ent := Entity (Identifier (Loop_Stmt));
      Ent := First_Entity (Loop_Ent);

      Local_Proc := Make_Temporary (Loc, 'P');

      Local_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Local_Proc),
              Declarations       => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (Loop_Copy)));

      Rewrite (Loop_Stmt, Local_Body);
      Analyze (Loop_Stmt);

      Set_Has_Nested_Subprogram (Local_Proc);

      Local_Call :=
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Local_Proc, Loc));

      Insert_After (Loop_Stmt, Local_Call);
      Analyze (Local_Call);

      --  New procedure has the same scope as the original loop, and the scope
      --  of the loop is the new procedure.

      Set_Scope (Local_Proc, Scope (Loop_Ent));
      Set_Scope (Loop_Ent, Local_Proc);

      Fixup_Inner_Scopes (Loop_Copy);

      --  The entity list of the new procedure is that of the loop

      Set_First_Entity (Local_Proc, Ent);

      --  Note that the entities associated with the loop don't need to have
      --  their Scope fields reset, since they're still associated with the
      --  same loop entity that now belongs to the copied loop statement.
   end Unnest_Loop;

   ---------------------------
   -- Unnest_Statement_List --
   ---------------------------

   procedure Unnest_Statement_List (Stmts : in out List_Id) is
      Loc        : constant Source_Ptr := Sloc (First (Stmts));
      Local_Body : Node_Id;
      Local_Call : Node_Id;
      Local_Proc : Entity_Id;
      New_Stmts  : constant List_Id := Empty_List;

   begin
      Local_Proc := Make_Temporary (Loc, 'P');

      Local_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Local_Proc),
          Declarations               => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));

      Append_To (New_Stmts, Local_Body);

      Analyze (Local_Body);

      Set_Has_Nested_Subprogram (Local_Proc);

      Local_Call :=
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Local_Proc, Loc));

      Append_To (New_Stmts, Local_Call);
      Analyze (Local_Call);

      --  Traverse the statements, and for any that are declarations or
      --  subprogram bodies that have entities, set the Scope of those
      --  entities to the new procedure's Entity_Id.

      declare
         Stmt : Node_Id := First (Stmts);

      begin
         while Present (Stmt) loop
            case Nkind (Stmt) is
               when N_Declaration
                  | N_Renaming_Declaration
               =>
                  Set_Scope (Defining_Identifier (Stmt), Local_Proc);

               when N_Subprogram_Body =>
                  Set_Scope
                    (Defining_Unit_Name (Specification (Stmt)), Local_Proc);

               when others =>
                  null;
            end case;

            Next (Stmt);
         end loop;
      end;

      Stmts := New_Stmts;
   end Unnest_Statement_List;

   --------------------------------
   -- Wrap_Transient_Declaration --
   --------------------------------

   --  If a transient scope has been established during the processing of the
   --  Expression of an Object_Declaration, it is not possible to wrap the
   --  declaration into a transient block as usual case, otherwise the object
   --  would be itself declared in the wrong scope. Therefore, all entities (if
   --  any) defined in the transient block are moved to the proper enclosing
   --  scope. Furthermore, if they are controlled variables they are finalized
   --  right after the declaration. The finalization list of the transient
   --  scope is defined as a renaming of the enclosing one so during their
   --  initialization they will be attached to the proper finalization list.
   --  For instance, the following declaration :

   --        X : Typ := F (G (A), G (B));

   --  (where G(A) and G(B) return controlled values, expanded as _v1 and _v2)
   --  is expanded into :

   --    X : Typ := [ complex Expression-Action ];
   --    [Deep_]Finalize (_v1);
   --    [Deep_]Finalize (_v2);

   procedure Wrap_Transient_Declaration (N : Node_Id) is
      Curr_S : Entity_Id;
      Encl_S : Entity_Id;

   begin
      Curr_S := Current_Scope;
      Encl_S := Scope (Curr_S);

      --  Insert all actions including cleanup generated while analyzing or
      --  expanding the transient context back into the tree. Manage the
      --  secondary stack when the object declaration appears in a library
      --  level package [body].

      Insert_Actions_In_Scope_Around
        (N         => N,
         Clean     => True,
         Manage_SS =>
           Uses_Sec_Stack (Curr_S)
             and then Nkind (N) = N_Object_Declaration
             and then Ekind (Encl_S) in E_Package | E_Package_Body
             and then Is_Library_Level_Entity (Encl_S));
      Pop_Scope;

      --  Relocate local entities declared within the transient scope to the
      --  enclosing scope. This action sets their Is_Public flag accordingly.

      Transfer_Entities (Curr_S, Encl_S);

      --  Mark the enclosing dynamic scope to ensure that the secondary stack
      --  is properly released upon exiting the said scope.

      if Uses_Sec_Stack (Curr_S) then
         Curr_S := Enclosing_Dynamic_Scope (Curr_S);

         --  Do not mark a function that returns on the secondary stack as the
         --  reclamation is done by the caller.

         if Ekind (Curr_S) = E_Function
           and then Needs_Secondary_Stack (Etype (Curr_S))
         then
            null;

         --  Otherwise mark the enclosing dynamic scope

         else
            Set_Uses_Sec_Stack (Curr_S);
            Check_Restriction (No_Secondary_Stack, N);
         end if;
      end if;
   end Wrap_Transient_Declaration;

   -------------------------------
   -- Wrap_Transient_Expression --
   -------------------------------

   procedure Wrap_Transient_Expression (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Expr : Node_Id             := Relocate_Node (N);
      Temp : constant Entity_Id  := Make_Temporary (Loc, 'E', N);
      Typ  : constant Entity_Id  := Etype (N);

   begin
      --  Generate:

      --    Temp : Typ;
      --    declare
      --       M : constant Mark_Id := SS_Mark;
      --       procedure Finalizer is ...  (See Build_Finalizer)

      --    begin
      --       Temp := <Expr>;                           --  general case
      --       Temp := (if <Expr> then True else False); --  boolean case

      --    at end
      --       Finalizer;
      --    end;

      --  A special case is made for Boolean expressions so that the back end
      --  knows to generate a conditional branch instruction, if running with
      --  -fpreserve-control-flow. This ensures that a control-flow change
      --  signaling the decision outcome occurs before the cleanup actions.

      if Opt.Suppress_Control_Flow_Optimizations
        and then Is_Boolean_Type (Typ)
      then
         Expr :=
           Make_If_Expression (Loc,
             Expressions => New_List (
               Expr,
               New_Occurrence_Of (Standard_True, Loc),
               New_Occurrence_Of (Standard_False, Loc)));
      end if;

      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Temp,
          Object_Definition   => New_Occurrence_Of (Typ, Loc)),

        Make_Transient_Block (Loc,
          Action =>
            Make_Assignment_Statement (Loc,
              Name       => New_Occurrence_Of (Temp, Loc),
              Expression => Expr),
          Par    => Parent (N))));

      if Debug_Generated_Code then
         Set_Debug_Info_Needed (Temp);
      end if;

      Rewrite (N, New_Occurrence_Of (Temp, Loc));
      Analyze_And_Resolve (N, Typ);
   end Wrap_Transient_Expression;

   ------------------------------
   -- Wrap_Transient_Statement --
   ------------------------------

   procedure Wrap_Transient_Statement (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      New_Stmt : constant Node_Id    := Relocate_Node (N);

   begin
      --  Generate:
      --    declare
      --       M : constant Mark_Id := SS_Mark;
      --       procedure Finalizer is ...  (See Build_Finalizer)
      --
      --    begin
      --       <New_Stmt>;
      --
      --    at end
      --       Finalizer;
      --    end;

      Rewrite (N,
        Make_Transient_Block (Loc,
          Action => New_Stmt,
          Par    => Parent (N)));

      --  With the scope stack back to normal, we can call analyze on the
      --  resulting block. At this point, the transient scope is being
      --  treated like a perfectly normal scope, so there is nothing
      --  special about it.

      --  Note: Wrap_Transient_Statement is called with the node already
      --  analyzed (i.e. Analyzed (N) is True). This is important, since
      --  otherwise we would get a recursive processing of the node when
      --  we do this Analyze call.

      Analyze (N);
   end Wrap_Transient_Statement;

end Exp_Ch7;
