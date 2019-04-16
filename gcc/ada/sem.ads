------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S E M                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--------------------------------------
-- Semantic Analysis: General Model --
--------------------------------------

--  Semantic processing involves 3 phases which are highly intertwined
--  (i.e. mutually recursive):

--    Analysis     implements the bulk of semantic analysis such as
--                 name analysis and type resolution for declarations,
--                 instructions and expressions.  The main routine
--                 driving this process is procedure Analyze given below.
--                 This analysis phase is really a bottom up pass that is
--                 achieved during the recursive traversal performed by the
--                 Analyze_... procedures implemented in the sem_* packages.
--                 For expressions this phase determines unambiguous types
--                 and collects sets of possible types where the
--                 interpretation is potentially ambiguous.

--    Resolution   is carried out only for expressions to finish type
--                 resolution that was initiated but not necessarily
--                 completed during analysis (because of overloading
--                 ambiguities). Specifically, after completing the bottom
--                 up pass carried out during analysis for expressions, the
--                 Resolve routine (see the spec of sem_res for more info)
--                 is called to perform a top down resolution with
--                 recursive calls to itself to resolve operands.

--    Expansion    if we are not generating code this phase is a no-op.
--                 otherwise this phase expands, i.e. transforms, original
--                 declaration, expressions or instructions into simpler
--                 structures that can be handled by the back-end. This
--                 phase is also in charge of generating code which is
--                 implicit in the original source (for instance for
--                 default initializations, controlled types, etc.)
--                 There are two separate instances where expansion is
--                 invoked. For declarations and instructions, expansion is
--                 invoked just after analysis since no resolution needs
--                 to be performed. For expressions, expansion is done just
--                 after resolution. In both cases expansion is done from the
--                 bottom up just before the end of Analyze for instructions
--                 and declarations or the call to Resolve for expressions.
--                 The main routine driving expansion is Expand.
--                 See the spec of Expander for more details.

--  To summarize, in normal code generation mode we recursively traverse the
--  abstract syntax tree top-down performing semantic analysis bottom
--  up. For instructions and declarations, before the call to the Analyze
--  routine completes we perform expansion since at that point we have all
--  semantic information needed. For expression nodes, after the call to
--  Analysis terminates we invoke the Resolve routine to transmit top-down
--  the type that was gathered by Analyze which will resolve possible
--  ambiguities in the expression. Just before the call to Resolve
--  terminates, the expression can be expanded since all the semantic
--  information is available at that point.

--  If we are not generating code then the expansion phase is a no-op

--  When generating code there are a number of exceptions to the basic
--  Analysis-Resolution-Expansion model for expressions. The most prominent
--  examples are the handling of default expressions and aggregates.

-----------------------------------------------------------------------
-- Handling of Default and Per-Object Expressions (Spec-Expressions) --
-----------------------------------------------------------------------

--  The default expressions in component declarations and in procedure
--  specifications (but not the ones in object declarations) are quite tricky
--  to handle. The problem is that some processing is required at the point
--  where the expression appears:

--    visibility analysis (including user defined operators)
--    freezing of static expressions

--  but other processing must be deferred until the enclosing entity (record or
--  procedure specification) is frozen:

--    freezing of any other types in the expression expansion
--    generation of code

--  A similar situation occurs with the argument of priority and interrupt
--  priority pragmas that appear in task and protected definition specs and
--  other cases of per-object expressions (see RM 3.8(18)).

--  Another similar case is the conditions in precondition and postcondition
--  pragmas that appear with subprogram specifications rather than in the body.

--  Collectively we call these Spec_Expressions. The routine that performs the
--  special analysis is called Analyze_Spec_Expression.

--  Expansion has to be deferred since you can't generate code for expressions
--  that reference types that have not been frozen yet. As an example, consider
--  the following:

--      type x is delta 0.5 range -10.0 .. +10.0;
--      ...
--      type q is record
--        xx : x := y * z;
--      end record;

--      for x'small use 0.25;

--  The expander is in charge of dealing with fixed-point, and of course the
--  small declaration, which is not too late, since the declaration of type q
--  does *not* freeze type x, definitely affects the expanded code.

--  Another reason that we cannot expand early is that expansion can generate
--  range checks. These range checks need to be inserted not at the point of
--  definition but at the point of use. The whole point here is that the value
--  of the expression cannot be obtained at the point of declaration, only at
--  the point of use.

--  Generally our model is to combine analysis resolution and expansion, but
--  this is the one case where this model falls down. Here is how we patch
--  it up without causing too much distortion to our basic model.

--  A switch (In_Spec_Expression) is set to show that we are in the initial
--  occurrence of a default expression. The analyzer is then called on this
--  expression with the switch set true. Analysis and resolution proceed almost
--  as usual, except that Freeze_Expression will not freeze non-static
--  expressions if this switch is set, and the call to Expand at the end of
--  resolution is skipped. This also skips the code that normally sets the
--  Analyzed flag to True. The result is that when we are done the tree is
--  still marked as unanalyzed, but all types for static expressions are frozen
--  as required, and all entities of variables have been recorded. We then turn
--  off the switch, and later on reanalyze the expression with the switch off.
--  The effect is that this second analysis freezes the rest of the types as
--  required, and generates code but visibility analysis is not repeated since
--  all the entities are marked.

--  The second analysis (the one that generates code) is in the context
--  where the code is required. For a record field default, this is in the
--  initialization procedure for the record and for a subprogram default
--  parameter, it is at the point the subprogram is frozen. For a priority or
--  storage size pragma it is in the context of the Init_Proc for the task or
--  protected object. For a pre/postcondition pragma it is in the body when
--  code for the pragma is generated.

------------------
-- Preanalysis --
------------------

--  For certain kind of expressions, such as aggregates, we need to defer
--  expansion of the aggregate and its inner expressions until after the whole
--  set of expressions appearing inside the aggregate have been analyzed.
--  Consider, for instance the following example:
--
--     (1 .. 100 => new Thing (Function_Call))
--
--  The normal Analysis-Resolution-Expansion mechanism where expansion of the
--  children is performed before expansion of the parent does not work if the
--  code generated for the children by the expander needs to be evaluated
--  repeatedly (for instance in the above aggregate "new Thing (Function_Call)"
--  needs to be called 100 times.)

--  The reason this mechanism does not work is that the expanded code for the
--  children is typically inserted above the parent and thus when the father
--  gets expanded no re-evaluation takes place. For instance in the case of
--  aggregates if "new Thing (Function_Call)" is expanded before the aggregate
--  the expanded code will be placed outside of the aggregate and when
--  expanding the aggregate the loop from 1 to 100 will not surround the
--  expanded code for "new Thing (Function_Call)".

--  To remedy this situation we introduce a flag that signals whether we want a
--  full analysis (i.e. expansion is enabled) or a preanalysis which performs
--  Analysis and Resolution but no expansion.

--  After the complete preanalysis of an expression has been carried out we
--  can transform the expression and then carry out the full three stage
--  (Analyze-Resolve-Expand) cycle on the transformed expression top-down so
--  that the expansion of inner expressions happens inside the newly generated
--  node for the parent expression.

--  Note that the difference between processing of default expressions and
--  preanalysis of other expressions is that we do carry out freezing in
--  the latter but not in the former (except for static scalar expressions).
--  The routine that performs preanalysis and corresponding resolution is
--  called Preanalyze_And_Resolve and is in Sem_Res.

with Alloc;
with Einfo;  use Einfo;
with Opt;    use Opt;
with Table;
with Types;  use Types;

package Sem is

   -----------------------------
   -- Semantic Analysis Flags --
   -----------------------------

   Full_Analysis : Boolean := True;
   --  Switch to indicate if we are doing a full analysis or a preanalysis.
   --  In normal analysis mode (Analysis-Expansion for instructions or
   --  declarations) or (Analysis-Resolution-Expansion for expressions) this
   --  flag is set. Note that if we are not generating code the expansion phase
   --  merely sets the Analyzed flag to True in this case. If we are in
   --  Preanalysis mode (see above) this flag is set to False then the
   --  expansion phase is skipped.
   --
   --  When this flag is False the flag Expander_Active is also False (the
   --  Expander_Active flag defined in the spec of package Expander tells you
   --  whether expansion is currently enabled). You should really regard this
   --  as a read only flag.

   In_Spec_Expression : Boolean := False;
   --  Switch to indicate that we are in a spec-expression, as described
   --  above. Note that this must be recursively saved on a Semantics call
   --  since it is possible for the analysis of an expression to result in a
   --  recursive call (e.g. to get the entity for System.Address as part of the
   --  processing of an Address attribute reference). When this switch is True
   --  then Full_Analysis above must be False. You should really regard this as
   --  a read only flag.

   In_Deleted_Code : Boolean := False;
   --  If the condition in an if-statement is statically known, the branch
   --  that is not taken is analyzed with expansion disabled, and the tree
   --  is deleted after analysis. Itypes generated in deleted code must be
   --  frozen from start, because the tree on which they depend will not
   --  be available at the freeze point.

   In_Assertion_Expr : Nat := 0;
   --  This is set non-zero if we are within the expression of an assertion
   --  pragma or aspect. It is a counter which is incremented at the start of
   --  expanding such an expression, and decremented on completion of expanding
   --  that expression. Probably a boolean would be good enough, since we think
   --  that such expressions cannot nest, but that might not be true in the
   --  future (e.g. if let expressions are added to Ada) so we prepare for that
   --  future possibility by making it a counter. As with In_Spec_Expression,
   --  it must be recursively saved and restored for a Semantics call.

   In_Compile_Time_Warning_Or_Error : Boolean := False;
   --  Switch to indicate that we are validating a pragma Compile_Time_Warning
   --  or Compile_Time_Error after the back end has been called (to check these
   --  pragmas for size and alignment appropriateness).

   In_Default_Expr : Boolean := False;
   --  Switch to indicate that we are analyzing a default component expression.
   --  As with In_Spec_Expression, it must be recursively saved and restored
   --  for a Semantics call.

   In_Inlined_Body : Boolean := False;
   --  Switch to indicate that we are analyzing and resolving an inlined body.
   --  Type checking is disabled in this context, because types are known to be
   --  compatible. This avoids problems with private types whose full view is
   --  derived from private types.

   Inside_A_Generic : Boolean := False;
   --  This flag is set if we are processing a generic specification, generic
   --  definition, or generic body. When this flag is True the Expander_Active
   --  flag is False to disable any code expansion (see package Expander). Only
   --  the generic processing can modify the status of this flag, any other
   --  client should regard it as read-only.
   --  Probably should be called Inside_A_Generic_Template ???

   Inside_Freezing_Actions : Nat := 0;
   --  Flag indicating whether we are within a call to Expand_N_Freeze_Actions.
   --  Non-zero means we are inside (it is actually a level counter to deal
   --  with nested calls). Used to avoid traversing the tree each time a
   --  subprogram call is processed to know if we must not clear all constant
   --  indications from entities in the current scope. Only the expansion of
   --  freezing nodes can modify the status of this flag, any other client
   --  should regard it as read-only.

   Inside_Preanalysis_Without_Freezing : Nat := 0;
   --  Flag indicating whether we are preanalyzing an expression performing no
   --  freezing. Non-zero means we are inside (it is actually a level counter
   --  to deal with nested calls).

   Unloaded_Subunits : Boolean := False;
   --  This flag is set True if we have subunits that are not loaded. This
   --  occurs when the main unit is a subunit, and contains lower level
   --  subunits that are not loaded. We use this flag to suppress warnings
   --  about unused variables, since these warnings are unreliable in this
   --  case. We could perhaps do a more accurate job and retain some of the
   --  warnings, but it is quite a tricky job.

   -----------------------------------
   -- Handling of Check Suppression --
   -----------------------------------

   --  There are two kinds of suppress checks: scope based suppress checks,
   --  and entity based suppress checks.

   --  Scope based suppress checks for the predefined checks (from initial
   --  command line arguments, or from Suppress pragmas not including an entity
   --  name) are recorded in the Sem.Scope_Suppress variable, and all that
   --  is necessary is to save the state of this variable on scope entry, and
   --  restore it on scope exit. This mechanism allows for fast checking of the
   --  scope suppress state without needing complex data structures.

   --  Entity based checks, from Suppress/Unsuppress pragmas giving an
   --  Entity_Id and scope based checks for non-predefined checks (introduced
   --  using pragma Check_Name), are handled as follows. If a suppress or
   --  unsuppress pragma is encountered for a given entity, then the flag
   --  Checks_May_Be_Suppressed is set in the entity and an entry is made in
   --  either the Local_Entity_Suppress stack (case of pragma that appears in
   --  other than a package spec), or in the Global_Entity_Suppress stack (case
   --  of pragma that appears in a package spec, which is by the rule of RM
   --  11.5(7) applicable throughout the life of the entity). Similarly, a
   --  Suppress/Unsuppress pragma for a non-predefined check which does not
   --  specify an entity is also stored in one of these stacks.

   --  If the Checks_May_Be_Suppressed flag is set in an entity then the
   --  procedure is to search first the local and then the global suppress
   --  stacks (we search these in reverse order, top element first). The only
   --  other point is that we have to make sure that we have proper nested
   --  interaction between such specific pragmas and locally applied general
   --  pragmas applying to all entities. This is achieved by including in the
   --  Local_Entity_Suppress table dummy entries with an empty Entity field
   --  that are applicable to all entities. A similar search is needed for any
   --  non-predefined check even if no specific entity is involved.

   Scope_Suppress : Suppress_Record;
   --  This variable contains the current scope based settings of the suppress
   --  switches. It is initialized from Suppress_Options in Gnat1drv, and then
   --  modified by pragma Suppress. On entry to each scope, the current setting
   --  is saved on the scope stack, and then restored on exit from the scope.
   --  This record may be rapidly checked to determine the current status of
   --  a check if no specific entity is involved or if the specific entity
   --  involved is one for which no specific Suppress/Unsuppress pragma has
   --  been set (as indicated by the Checks_May_Be_Suppressed flag being set).

   --  This scheme is a little complex, but serves the purpose of enabling
   --  a very rapid check in the common case where no entity specific pragma
   --  applies, and gives the right result when such pragmas are used even
   --  in complex cases of nested Suppress and Unsuppress pragmas.

   --  The Local_Entity_Suppress and Global_Entity_Suppress stacks are handled
   --  using dynamic allocation and linked lists. We do not often use this
   --  approach in the compiler (preferring to use extensible tables instead).
   --  The reason we do it here is that scope stack entries save a pointer to
   --  the current local stack top, which is also saved and restored on scope
   --  exit. Furthermore for processing of generics we save pointers to the
   --  top of the stack, so that the local stack is actually a tree of stacks
   --  rather than a single stack, a structure that is easy to represent using
   --  linked lists, but impossible to represent using a single table. Note
   --  that because of the generic issue, we never release entries in these
   --  stacks, but that's no big deal, since we are unlikely to have a huge
   --  number of Suppress/Unsuppress entries in a single compilation.

   type Suppress_Stack_Entry;
   type Suppress_Stack_Entry_Ptr is access all Suppress_Stack_Entry;

   type Suppress_Stack_Entry is record
      Entity : Entity_Id;
      --  Entity to which the check applies, or Empty for a check that has
      --  no entity name (and thus applies to all entities).

      Check : Check_Id;
      --  Check which is set (can be All_Checks for the All_Checks case)

      Suppress : Boolean;
      --  Set True for Suppress, and False for Unsuppress

      Prev : Suppress_Stack_Entry_Ptr;
      --  Pointer to previous entry on stack

      Next : Suppress_Stack_Entry_Ptr;
      --  All allocated Suppress_Stack_Entry records are chained together in
      --  a linked list whose head is Suppress_Stack_Entries, and the Next
      --  field is used as a forward pointer (null ends the list). This is
      --  used to free all entries in Sem.Init (which will be important if
      --  we ever setup the compiler to be reused).
   end record;

   Suppress_Stack_Entries : Suppress_Stack_Entry_Ptr := null;
   --  Pointer to linked list of records (see comments for Next above)

   Local_Suppress_Stack_Top : Suppress_Stack_Entry_Ptr;
   --  Pointer to top element of local suppress stack. This is the entry that
   --  is saved and restored in the scope stack, and also saved for generic
   --  body expansion.

   Global_Suppress_Stack_Top : Suppress_Stack_Entry_Ptr;
   --  Pointer to top element of global suppress stack

   procedure Push_Local_Suppress_Stack_Entry
     (Entity   : Entity_Id;
      Check    : Check_Id;
      Suppress : Boolean);
   --  Push a new entry on to the top of the local suppress stack, updating
   --  the value in Local_Suppress_Stack_Top;

   procedure Push_Global_Suppress_Stack_Entry
     (Entity   : Entity_Id;
      Check    : Check_Id;
      Suppress : Boolean);
   --  Push a new entry on to the top of the global suppress stack, updating
   --  the value in Global_Suppress_Stack_Top;

   -----------------
   -- Scope Stack --
   -----------------

   --  The scope stack indicates the declarative regions that are currently
   --  being processed (analyzed and/or expanded). The scope stack is one of
   --  the basic visibility structures in the compiler: entities that are
   --  declared in a scope that is currently on the scope stack are immediately
   --  visible (leaving aside issues of hiding and overloading).

   --  Initially, the scope stack only contains an entry for package Standard.
   --  When a compilation unit, subprogram unit, block or declarative region
   --  is being processed, the corresponding entity is pushed on the scope
   --  stack. It is removed after the processing step is completed. A given
   --  entity can be placed several times on the scope stack, for example
   --  when processing derived type declarations, freeze nodes, etc. The top
   --  of the scope stack is the innermost scope currently being processed.
   --  It is obtained through function Current_Scope. After a compilation unit
   --  has been processed, the scope stack must contain only Standard.
   --  The predicate In_Open_Scopes specifies whether a scope is currently
   --  on the scope stack.

   --  This model is complicated by the need to compile units on the fly, in
   --  the middle of the compilation of other units. This arises when compiling
   --  instantiations, and when compiling run-time packages obtained through
   --  rtsfind. Given that the scope stack is a single static and global
   --  structure (not originally designed for the recursive processing required
   --  by rtsfind for example) additional machinery is needed to indicate what
   --  is currently being compiled. As a result, the scope stack holds several
   --  contiguous sections that correspond to the compilation of a given
   --  compilation unit. These sections are separated by distinct occurrences
   --  of package Standard. The currently active section of the scope stack
   --  goes from the current scope to the first (innermost) occurrence of
   --  Standard, which is additionally marked with flag Is_Active_Stack_Base.
   --  The basic visibility routine (Find_Direct_Name, in Sem_Ch8) uses this
   --  contiguous section of the scope stack to determine whether a given
   --  entity is or is not visible at a point. In_Open_Scopes only examines
   --  the currently active section of the scope stack.

   --  Similar complications arise when processing child instances. These
   --  must be compiled in the context of parent instances, and therefore the
   --  parents must be pushed on the stack before compiling the child, and
   --  removed afterwards. Routines Save_Scope_Stack and Restore_Scope_Stack
   --  are used to set/reset the visibility of entities declared in scopes
   --  that are currently on the scope stack, and are used when compiling
   --  instance bodies on the fly.

   --  It is clear in retrospect that all semantic processing and visibility
   --  structures should have been fully recursive. The rtsfind mechanism,
   --  and the complexities brought about by subunits and by generic child
   --  units and their instantiations, have led to a hybrid model that carries
   --  more state than one would wish.

   type Scope_Action_Kind is (Before, After, Cleanup);
   type Scope_Actions is array (Scope_Action_Kind) of List_Id;
   --  Transient blocks have three associated actions list, to be inserted
   --  before and after the block's statements, and as cleanup actions.

   Configuration_Component_Alignment : Component_Alignment_Kind :=
                                         Calign_Default;
   --  Used for handling the pragma Component_Alignment in the context of a
   --  configuration file.

   type Scope_Stack_Entry is record
      Entity : Entity_Id;
      --  Entity representing the scope

      Last_Subprogram_Name : String_Ptr;
      --  Pointer to name of last subprogram body in this scope. Used for
      --  testing proper alpha ordering of subprogram bodies in scope.

      Save_Scope_Suppress : Suppress_Record;
      --  Save contents of Scope_Suppress on entry

      Save_Local_Suppress_Stack_Top : Suppress_Stack_Entry_Ptr;
      --  Save contents of Local_Suppress_Stack on entry to restore on exit

      Save_Check_Policy_List : Node_Id;
      --  Save contents of Check_Policy_List on entry to restore on exit. The
      --  Check_Policy pragmas are chained with Check_Policy_List pointing to
      --  the most recent entry. This list is searched starting here, so that
      --  the search finds the most recent appicable entry. When we restore
      --  Check_Policy_List on exit from the scope, the effect is to remove
      --  all entries set in the scope being exited.

      Save_Default_Storage_Pool : Node_Id;
      --  Save contents of Default_Storage_Pool on entry to restore on exit

      Save_SPARK_Mode : SPARK_Mode_Type;
      --  Setting of SPARK_Mode on entry to restore on exit

      Save_SPARK_Mode_Pragma : Node_Id;
      --  Setting of SPARK_Mode_Pragma on entry to restore on exit

      Save_No_Tagged_Streams : Node_Id;
      --  Setting of No_Tagged_Streams to restore on exit

      Save_Default_SSO : Character;
      --  Setting of Default_SSO on entry to restore on exit

      Save_Uneval_Old : Character;
      --  Setting of Uneval_Old on entry to restore on exit

      Is_Transient : Boolean;
      --  Marks transient scopes (see Exp_Ch7 body for details)

      Previous_Visibility : Boolean;
      --  Used when installing the parent(s) of the current compilation unit.
      --  The parent may already be visible because of an ongoing compilation,
      --  and the proper visibility must be restored on exit. The flag is
      --  typically needed when the context of a child unit requires
      --  compilation of a sibling. In other cases the flag is set to False.
      --  See Sem_Ch10 (Install_Parents, Remove_Parents).

      Node_To_Be_Wrapped : Node_Id;
      --  Only used in transient scopes. Records the node which will be wrapped
      --  by the transient block.

      Actions_To_Be_Wrapped : Scope_Actions;
      --  Actions that have to be inserted at the start, at the end, or as
      --  cleanup actions of a transient block. Used to temporarily hold these
      --  actions until the block is created, at which time the actions are
      --  moved to the block.

      Pending_Freeze_Actions : List_Id;
      --  Used to collect freeze entity nodes and associated actions that are
      --  generated in an inner context but need to be analyzed outside, such
      --  as records and initialization procedures. On exit from the scope,
      --  this list of actions is inserted before the scope construct and
      --  analyzed to generate the corresponding freeze processing and
      --  elaboration of other associated actions.

      First_Use_Clause : Node_Id;
      --  Head of list of Use_Clauses in current scope. The list is built when
      --  the declarations in the scope are processed. The list is traversed
      --  on scope exit to undo the effect of the use clauses.

      Component_Alignment_Default : Component_Alignment_Kind;
      --  Component alignment to be applied to any record or array types that
      --  are declared for which a specific component alignment pragma does not
      --  set the alignment.

      Is_Active_Stack_Base : Boolean;
      --  Set to true only when entering the scope for Standard_Standard from
      --  from within procedure Semantics. Indicates the base of the current
      --  active set of scopes. Needed by In_Open_Scopes to handle cases where
      --  Standard_Standard can be pushed anew on the scope stack to start a
      --  new active section (see comment above).

      Locked_Shared_Objects : Elist_Id;
      --  List of shared passive protected objects that have been locked in
      --  this transient scope (always No_Elist for non-transient scopes).
   end record;

   package Scope_Stack is new Table.Table (
     Table_Component_Type => Scope_Stack_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Scope_Stack_Initial,
     Table_Increment      => Alloc.Scope_Stack_Increment,
     Table_Name           => "Sem.Scope_Stack");

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling back end

   procedure Unlock;
   --  Unlock internal tables

   procedure Semantics (Comp_Unit : Node_Id);
   --  This procedure is called to perform semantic analysis on the specified
   --  node which is the N_Compilation_Unit node for the unit.

   procedure Analyze (N : Node_Id);
   procedure Analyze (N : Node_Id; Suppress : Check_Id);
   --  This is the recursive procedure that is applied to individual nodes of
   --  the tree, starting at the top level node (compilation unit node) and
   --  then moving down the tree in a top down traversal. It calls individual
   --  routines with names Analyze_xxx to analyze node xxx. Each of these
   --  routines is responsible for calling Analyze on the components of the
   --  subtree.
   --
   --  Note: In the case of expression components (nodes whose Nkind is in
   --  N_Subexpr), the call to Analyze does not complete the semantic analysis
   --  of the node, since the type resolution cannot be completed until the
   --  complete context is analyzed. The completion of the type analysis occurs
   --  in the corresponding Resolve routine (see Sem_Res).
   --
   --  Note: for integer and real literals, the analyzer sets the flag to
   --  indicate that the result is a static expression. If the expander
   --  generates a literal that does NOT correspond to a static expression,
   --  e.g. by folding an expression whose value is known at compile time,
   --  but is not technically static, then the caller should reset the
   --  Is_Static_Expression flag after analyzing but before resolving.
   --
   --  If the Suppress argument is present, then the analysis is done
   --  with the specified check suppressed (can be All_Checks to suppress
   --  all checks).

   procedure Analyze_List (L : List_Id);
   procedure Analyze_List (L : List_Id; Suppress : Check_Id);
   --  Analyzes each element of a list. If the Suppress argument is present,
   --  then the analysis is done with the specified check suppressed (can
   --  be All_Checks to suppress all checks).

   procedure Copy_Suppress_Status
     (C    : Check_Id;
      From : Entity_Id;
      To   : Entity_Id);
   --  If From is an entity for which check C is explicitly suppressed
   --  then also explicitly suppress the corresponding check in To.

   procedure Insert_List_After_And_Analyze
     (N : Node_Id; L : List_Id);
   --  Inserts list L after node N using Nlists.Insert_List_After, and then,
   --  after this insertion is complete, analyzes all the nodes in the list,
   --  including any additional nodes generated by this analysis. If the list
   --  is empty or No_List, the call has no effect.

   procedure Insert_List_Before_And_Analyze
     (N : Node_Id; L : List_Id);
   --  Inserts list L before node N using Nlists.Insert_List_Before, and then,
   --  after this insertion is complete, analyzes all the nodes in the list,
   --  including any additional nodes generated by this analysis. If the list
   --  is empty or No_List, the call has no effect.

   procedure Insert_After_And_Analyze
     (N : Node_Id; M : Node_Id);
   procedure Insert_After_And_Analyze
     (N : Node_Id; M : Node_Id; Suppress : Check_Id);
   --  Inserts node M after node N and then after the insertion is complete,
   --  analyzes the inserted node and all nodes that are generated by
   --  this analysis. If the node is empty, the call has no effect. If the
   --  Suppress argument is present, then the analysis is done with the
   --  specified check suppressed (can be All_Checks to suppress all checks).

   procedure Insert_Before_And_Analyze
     (N : Node_Id; M : Node_Id);
   procedure Insert_Before_And_Analyze
     (N : Node_Id; M : Node_Id; Suppress : Check_Id);
   --  Inserts node M before node N and then after the insertion is complete,
   --  analyzes the inserted node and all nodes that could be generated by
   --  this analysis. If the node is empty, the call has no effect. If the
   --  Suppress argument is present, then the analysis is done with the
   --  specified check suppressed (can be All_Checks to suppress all checks).

   function External_Ref_In_Generic (E : Entity_Id) return Boolean;
   --  Return True if we are in the context of a generic and E is
   --  external (more global) to it.

   procedure Enter_Generic_Scope (S : Entity_Id);
   --  Called each time a Generic subprogram or package scope is entered. S is
   --  the entity of the scope.
   --
   --  ??? At the moment, only called for package specs because this mechanism
   --  is only used for avoiding freezing of external references in generics
   --  and this can only be an issue if the outer generic scope is a package
   --  spec (otherwise all external entities are already frozen)

   procedure Exit_Generic_Scope  (S : Entity_Id);
   --  Called each time a Generic subprogram or package scope is exited. S is
   --  the entity of the scope.
   --
   --  ??? At the moment, only called for package specs exit.

   function Explicit_Suppress (E : Entity_Id; C : Check_Id) return Boolean;
   --  This function returns True if an explicit pragma Suppress for check C
   --  is present in the package defining E.

   function Preanalysis_Active return Boolean;
   pragma Inline (Preanalysis_Active);
   --  Determine whether preanalysis is active at the point of invocation

   procedure Preanalyze (N : Node_Id);
   --  Performs a preanalysis of node N. During preanalysis no expansion is
   --  carried out for N or its children. See above for more info on
   --  preanalysis.

   generic
      with procedure Action (Item : Node_Id);
   procedure Walk_Library_Items;
   --  Primarily for use by CodePeer and GNATprove. Must be called after
   --  semantic analysis (and expansion in the case of CodePeer) are complete.
   --  Walks each relevant library item, calling Action for each, in an order
   --  such that one will not run across forward references. Each Item passed
   --  to Action is the declaration or body of a library unit, including
   --  generics and renamings. The first item is the N_Package_Declaration node
   --  for package Standard. Bodies are not included, except for the main unit
   --  itself, which always comes last.
   --
   --  Item is never a subunit
   --
   --  Item is never an instantiation. Instead, the instance declaration is
   --  passed, and (if the instantiation is the main unit), the instance body.

   ------------------------
   -- Debugging Routines --
   ------------------------

   function ss (Index : Int) return Scope_Stack_Entry;
   pragma Export (Ada, ss);
   --  "ss" = "scope stack"; returns the Index'th entry in the Scope_Stack

   function sst return Scope_Stack_Entry;
   pragma Export (Ada, sst);
   --  "sst" = "scope stack top"; same as ss(Scope_Stack.Last)

end Sem;
