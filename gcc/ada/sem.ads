------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S E M                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--------------------------------------
-- Semantic Analysis: General Model --
--------------------------------------

--  Semantic processing involves 3 phases which are highly interwined
--  (ie mutually recursive):
--
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
--
--    Resolution   is carried out only for expressions to finish type
--                 resolution that was initiated but not necessarily
--                 completed during analysis (because of overloading
--                 ambiguities). Specifically, after completing the bottom
--                 up pass carried out during analysis for expressions, the
--                 Resolve routine (see the spec of sem_res for more info)
--                 is called to perform a top down resolution with
--                 recursive calls to itself to resolve operands.
--
--    Expansion    if we are not generating code this phase is a no-op.
--                 otherwise this phase expands, ie transforms, original
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
--
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
--
--  If we are not generating code then the expansion phase is a no-op.
--
--  When generating code there are a number of exceptions to the basic
--  Analysis-Resolution-Expansion model for expressions. The most prominent
--  examples are the handling of default expressions and aggregates.

-------------------------------------
-- Handling of Default Expressions --
-------------------------------------

--  The default expressions in component declarations and in procedure
--  specifications (but not the ones in object declarations) are quite
--  tricky to handle. The problem is that some processing is required
--  at the point where the expression appears:
--
--    visibility analysis (including user defined operators)
--    freezing of static expressions
--
--  but other processing must be deferred until the enclosing entity
--  (record or procedure specification) is frozen:
--
--    freezing of any other types in the expression
--    expansion
--
--  Expansion has to be deferred since you can't generate code for
--  expressions that refernce types that have not been frozen yet. As an
--  example, consider the following:
--
--      type x is delta 0.5 range -10.0 .. +10.0;
--      ...
--      type q is record
--        xx : x := y * z;
--      end record;
--
--      for x'small use 0.25
--
--  The expander is in charge of dealing with fixed-point, and of course
--  the small declaration, which is not too late, since the declaration of
--  type q does *not* freeze type x, definitely affects the expanded code.
--
--  Generally our model is to combine analysis resolution and expansion, but
--  this is the one case where this model falls down. Here is how we patch
--  it up without causing too much distortion to our basic model.
--
--  A switch (sede below) is set to indicate that we are in the initial
--  occurrence of a default expression. The analyzer is then called on this
--  expression with the switch set true. Analysis and resolution proceed
--  almost as usual, except that Freeze_Expression will not freeze
--  non-static expressions if this switch is set, and the call to Expand at
--  the end of resolution is skipped. This also skips the code that normally
--  sets the Analyzed flag to True). The result is that when we are done the
--  tree is still marked as unanalyzed, but all types for static expressions
--  are frozen as required, and all entities of variables have been
--  recorded.  We then turn off the switch, and later on reanalyze the
--  expression with the switch off. The effect is that this second analysis
--  freezes the rest of the types as required, and generates code but
--  visibility analysis is not repeated since all the entities are marked.
--
--  The second analysis (the one that generates code) is in the context
--  where the code is required. For a record field default, this is in
--  the initialization procedure for the record and for a subprogram
--  default parameter, it is at the point the subprogram is frozen.

------------------
-- Pre-Analysis --
------------------

--  For certain kind of expressions, such as aggregates, we need to defer
--  expansion of the aggregate and its inner expressions after the whole
--  set of expressions appearing inside the aggregate have been analyzed.
--  Consider, for instance the following example:
--
--     (1 .. 100 => new Thing (Function_Call))
--
--  The normal Analysis-Resolution-Expansion mechanism where expansion
--  of the children is performed before expansion of the parent does not
--  work if the code generated for the children by the expander needs
--  to be evaluated repeatdly (for instance in the above aggregate
--  "new Thing (Function_Call)" needs to be called 100 times.)
--  The reason why this mecanism does not work is that, the expanded code
--  for the children is typically inserted above the parent and thus
--  when the father gets expanded no re-evaluation takes place. For instance
--  in the case of aggregates if "new Thing (Function_Call)" is expanded
--  before of the aggregate the expanded code will be placed outside
--  of the aggregate and when expanding the aggregate the loop from 1 to 100
--  will not surround the expanded code for "new Thing (Function_Call)".
--
--  To remedy this situation we introduce a new flag which signals whether
--  we want a full analysis (ie expansion is enabled) or a pre-analysis
--  which performs Analysis and Resolution but no expansion.
--
--  After the complete pre-analysis of an expression has been carried out
--  we can transform the expression and then carry out the full
--  Analyze-Resolve-Expand cycle on the transformed expression top-down
--  so that the expansion of inner expressions happens inside the newly
--  generated node for the parent expression.
--
--  Note that the difference between processing of default expressions and
--  pre-analysis of other expressions is that we do carry out freezing in
--  the latter but not in the former (except for static scalar expressions).
--  The routine that performs pre-analysis is called Pre_Analyze_And_Resolve
--  and is in Sem_Res.

with Alloc;
with Einfo;  use Einfo;
with Opt;    use Opt;
with Snames; use Snames;
with Table;
with Types;  use Types;

package Sem is

   New_Nodes_OK : Int := 1;
   --  Temporary flag for use in checking out HLO. Set non-zero if it is
   --  OK to generate new nodes.

   -----------------------------
   -- Semantic Analysis Flags --
   -----------------------------

   Full_Analysis : Boolean := True;
   --  Switch to indicate whether we are doing a full analysis or a
   --  pre-analysis. In normal analysis mode (Analysis-Expansion for
   --  instructions or declarations) or (Analysis-Resolution-Expansion for
   --  expressions) this flag is set. Note that if we are not generating
   --  code the expansion phase merely sets the Analyzed flag to True in
   --  this case. If we are in Pre-Analysis mode (see above) this flag is
   --  set to False then the expansion phase is skipped.
   --  When this flag is False the flag Expander_Active is also False
   --  (the Expander_Activer flag defined in the spec of package Expander
   --  tells you whether expansion is currently enabled).
   --  You should really regard this as a read only flag.

   In_Default_Expression : Boolean := False;
   --  Switch to indicate that we are in a default expression, as described
   --  above. Note that this must be recursively saved on a Semantics call
   --  since it is possible for the analysis of an expression to result in
   --  a recursive call (e.g. to get the entity for System.Address as part
   --  of the processing of an Address attribute reference).
   --  When this switch is True then Full_Analysis above must be False.
   --  You should really regard this as a read only flag.

   In_Inlined_Body : Boolean := False;
   --  Switch to indicate that we are analyzing and resolving an inlined
   --  body. Type checking is disabled in this context, because types are
   --  known to be compatible. This avoids problems with private types whose
   --  full view is derived from private types.

   Inside_A_Generic : Boolean := False;
   --  This flag is set if we are processing a generic specification,
   --  generic definition, or generic body. When this flag is True the
   --  Expander_Active flag is False to disable any code expansion (see
   --  package Expander). Only the generic processing can modify the
   --  status of this flag, any other client should regard it as read-only.

   Unloaded_Subunits : Boolean := False;
   --  This flag is set True if we have subunits that are not loaded. This
   --  occurs when the main unit is a subunit, and contains lower level
   --  subunits that are not loaded. We use this flag to suppress warnings
   --  about unused variables, since these warnings are unreliable in this
   --  case. We could perhaps do a more accurate job and retain some of the
   --  warnings, but it is quite a tricky job. See test 4323-002.

   -----------------
   -- Scope Stack --
   -----------------

   Scope_Suppress : Suppress_Record := Suppress_Options;
   --  This record contains the current scope based settings of the suppress
   --  switches. It is initialized from the options as shown, and then modified
   --  by pragma Suppress. On entry to each scope, the current setting is saved
   --  the scope stack, and then restored on exit from the scope.

   --  The scope stack holds all entries of the scope table. As in the parser,
   --  we use Last as the stack pointer, so that we can always find the scope
   --  that is currently open in Scope_Stack.Table (Scope_Stack.Last). The
   --  oldest entry, at Scope_Stack (0) is Standard. The entries in the table
   --  include the entity for the referenced scope, together with information
   --  used to restore the proper setting of check suppressions on scope exit.

   --  There are two kinds of suppress checks, scope based suppress checks
   --  (from initial command line arguments, or from Suppress pragmas not
   --  including an entity name). The scope based suppress checks are recorded
   --  in the Sem.Supress variable, and all that is necessary is to save the
   --  state of this variable on scope entry, and restore it on scope exit.

   --  The other kind of suppress check is entity based suppress checks, from
   --  Suppress pragmas giving an Entity_Id. These checks are reflected by the
   --  appropriate bit being set in the corresponding entity, and restoring the
   --  setting of these bits is a little trickier. In particular a given pragma
   --  Suppress may or may not affect the current state. If it sets a check for
   --  an entity that is already checked, then it is important that this check
   --  not be restored on scope exit. The situation is made more complicated
   --  by the fact that a given suppress pragma can specify multiple entities
   --  (in the overloaded case), and multiple checks (by using All_Checks), so
   --  that it may be partially effective. On exit only checks that were in
   --  fact effective must be removed. Logically we could do this by saving
   --  the entire state of the entity flags on scope entry and restoring them
   --  on scope exit, but that would be ludicrous, so what we do instead is to
   --  maintain the following differential structure that shows what checks
   --  were installed for the current scope.

   --  Note: Suppress pragmas that specify entities defined in a package
   --  spec do not make entries in this table, since such checks suppress
   --  requests are valid for the entire life of the entity.

   type Entity_Check_Suppress_Record is record
      Entity : Entity_Id;
      --  Entity to which the check applies

      Check : Check_Id;
      --  Check which is set (note this cannot be All_Checks, if the All_Checks
      --  case, a sequence of eentries appears for the individual checks.
   end record;

   --  Entity_Suppress is a stack, to which new entries are added as they
   --  are processed (see pragma Suppress circuit in Sem_Prag). The scope
   --  stack entry simply saves the stack pointer on entry, and restores
   --  it on exit by reversing the checks one by one.

   package Entity_Suppress is new Table.Table (
     Table_Component_Type => Entity_Check_Suppress_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Entity_Suppress_Initial,
     Table_Increment      => Alloc.Entity_Suppress_Increment,
     Table_Name           => "Entity_Suppress");

   --  Here is the scope stack itself

   type Scope_Stack_Entry is record
      Entity : Entity_Id;
      --  Entity representing the scope

      Last_Subprogram_Name : String_Ptr;
      --  Pointer to name of last subprogram body in this scope. Used for
      --  testing proper alpha ordering of subprogram bodies in scope.

      Save_Scope_Suppress  : Suppress_Record;
      --  Save contents of Scope_Suppress on entry

      Save_Entity_Suppress : Int;
      --  Save contents of Entity_Suppress.Last on entry

      Is_Transient : Boolean;
      --  Marks Transient Scopes (See Exp_Ch7 body for details)

      Previous_Visibility : Boolean;
      --  Used when installing the parent (s) of the current compilation
      --  unit. The parent may already be visible because of an ongoing
      --  compilation, and the proper visibility must be restored on exit.

      Node_To_Be_Wrapped : Node_Id;
      --  Only used in transient scopes. Records the node which will
      --  be wrapped by the transient block.

      Actions_To_Be_Wrapped_Before : List_Id;
      Actions_To_Be_Wrapped_After  : List_Id;
      --  Actions that have to be inserted at the start or at the end of a
      --  transient block. Used to temporarily hold these actions until the
      --  block is created, at which time the actions are moved to the
      --  block.

      Pending_Freeze_Actions : List_Id;
      --  Used to collect freeze entity nodes and associated actions that
      --  are generated in a inner context but need to be analyzed outside,
      --  such as records and initialization procedures. On exit from the
      --  scope, this list of actions is inserted before the scope construct
      --  and analyzed to generate the corresponding freeze processing and
      --  elaboration of other associated actions.

      First_Use_Clause : Node_Id;
      --  Head of list of Use_Clauses in current scope. The list is built
      --  when the declarations in the scope are processed. The list is
      --  traversed on scope exit to undo the effect of the use clauses.

      Component_Alignment_Default : Component_Alignment_Kind;
      --  Component alignment to be applied to any record or array types
      --  that are declared for which a specific component alignment pragma
      --  does not set the alignment.

      Is_Active_Stack_Base : Boolean;
      --  Set to true only when entering the scope for Standard_Standard from
      --  from within procedure Semantics. Indicates the base of the current
      --  active set of scopes. Needed by In_Open_Scopes to handle cases
      --  where Standard_Standard can be pushed in the middle of the active
      --  set of scopes (occurs for instantiations of generic child units).
   end record;

   package Scope_Stack is new Table.Table (
     Table_Component_Type => Scope_Stack_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Scope_Stack_Initial,
     Table_Increment      => Alloc.Scope_Stack_Increment,
     Table_Name           => "Sem.Scope_Stack");

   function Get_Scope_Suppress (C : Check_Id) return Boolean;
   --  Get suppress status of check C for the current scope

   procedure Set_Scope_Suppress (C : Check_Id; B : Boolean);
   --  Set suppress status of check C for the current scope

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling back end

   procedure Semantics (Comp_Unit : Node_Id);
   --  This procedure is called to perform semantic analysis on the specified
   --  node which is the N_Compilation_Unit node for the unit.

   procedure Analyze (N : Node_Id);
   procedure Analyze (N : Node_Id; Suppress : Check_Id);
   --  This is the recursive procedure which is applied to individual nodes
   --  of the tree, starting at the top level node (compilation unit node)
   --  and then moving down the tree in a top down traversal. It calls
   --  individual routines with names Analyze_xxx to analyze node xxx. Each
   --  of these routines is responsible for calling Analyze on the components
   --  of the subtree.
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
   --  e.g. by folding an expression whose value is known at compile-time,
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

   procedure Insert_List_After_And_Analyze
     (N : Node_Id; L : List_Id);
   procedure Insert_List_After_And_Analyze
     (N : Node_Id; L : List_Id; Suppress : Check_Id);
   --  Inserts list L after node N using Nlists.Insert_List_After, and then,
   --  after this insertion is complete, analyzes all the nodes in the list,
   --  including any additional nodes generated by this analysis. If the list
   --  is empty or be No_List, the call has no effect. If the Suppress
   --  argument is present, then the analysis is done with the specified
   --  check suppressed (can be All_Checks to suppress all checks).

   procedure Insert_List_Before_And_Analyze
     (N : Node_Id; L : List_Id);
   procedure Insert_List_Before_And_Analyze
     (N : Node_Id; L : List_Id; Suppress : Check_Id);
   --  Inserts list L before node N using Nlists.Insert_List_Before, and then,
   --  after this insertion is complete, analyzes all the nodes in the list,
   --  including any additional nodes generated by this analysis. If the list
   --  is empty or be No_List, the call has no effect. If the Suppress
   --  argument is present, then the analysis is done with the specified
   --  check suppressed (can be All_Checks to suppress all checks).

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
   --  Shall be called each time a Generic subprogram or package scope is
   --  entered.  S is the entity of the scope.
   --  ??? At the moment, only called for package specs because this mechanism
   --  is only used for avoiding freezing of external references in generics
   --  and this can only be an issue if the outer generic scope is a package
   --  spec (otherwise all external entities are already frozen)

   procedure Exit_Generic_Scope  (S : Entity_Id);
   --  Shall be called each time a Generic subprogram or package scope is
   --  exited.  S is the entity of the scope.
   --  ??? At the moment, only called for package specs exit.

end Sem;
