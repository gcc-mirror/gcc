------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S I N F O                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the structure of the abstract syntax tree. The Tree
--  package provides a basic tree structure. Sinfo describes how this
--  structure is used to represent the syntax of an Ada program.

--  Note: the grammar used here is taken from Version 5.95 of the RM, dated
--  November 1994. The grammar in the RM is followed very closely in the tree
--  design, and is repeated as part of this source file.

--  The tree contains not only the full syntactic representation of the
--  program, but also the results of semantic analysis. In particular, the
--  nodes for defining identifiers, defining character literals and defining
--  operator symbols, collectively referred to as entities, represent what
--  would normally be regarded as the symbol table information. In addition
--  a number of the tree nodes contain semantic information.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in this C header file sinfo.h
--  which is created automatically from sinfo.ads using xsinfo.spt.

with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package Sinfo is

   ---------------------------------
   -- Making Changes to This File --
   ---------------------------------

   --  If changes are made to this file, a number of related steps must be
   --  carried out to ensure consistency. First, if a field access function
   --  is added, it appears in seven places:

   --    The documentation associated with the node
   --    The spec of the access function in sinfo.ads
   --    The body of the access function in sinfo.adb
   --    The pragma Inline at the end of sinfo.ads for the access function
   --    The spec of the set procedure in sinfo.ads
   --    The body of the set procedure in sinfo.adb
   --    The pragma Inline at the end of sinfo.ads for the set procedure

   --  The field chosen must be consistent in all places, and, for a node
   --  that is a subexpression, must not overlap any of the standard
   --  expression fields. In the body, the calls to the Dcheck_Node debug
   --  procedure will need cross-references adding in alphabetical order.

   --  In addition, if any of the standard expression fields is changed, then
   --  the utiliy program which creates the Treeprs spec (in file treeprs.ads)
   --  must be updated appropriately, since it special cases expression fields.

   --  If a new tree node is added, then the following changes are made

   --    Add it to the documentation in the appropriate place
   --    Add its fields to this documentation section
   --    Define it in the appropriate classification in Node_Kind
   --    In the body (sinfo), add entries to the Dcheck calls for all
   --     its fields (except standard expression fields) to include
   --     the new node in the debug cross-reference list
   --    Add an appropriate section to the case statement in sprint.adb
   --    Add an appropriate section to the case statement in sem.adb
   --    Add an appropraite section to the case statement in exp_util.adb
   --     (Insert_Actions procedure)
   --    For a subexpression, add an appropriate sections to the case
   --     statement in sem_eval.adb
   --    For a subexpression, add an appropriate sections to the case
   --     statement in sem_res.adb

   --  Finally, four utility programs must be run:

   --    Run CSinfo to check that you have made the changes consistently.
   --     It checks most of the rules given above, with clear error messages.
   --     This utility reads sinfo.ads and sinfo.adb and generates a report
   --     to standard output.

   --    Run XSinfo to create a-sinfo.h, the corresponding C header. This
   --     utility reads sinfo.ads and generates a-sinfo.h. Note that it
   --     does not need to read sinfo.adb, since the contents of the body
   --     are algorithmically determinable from the spec.

   --    Run XTreeprs to create treeprs.ads, an updated version of
   --     the module that is used to drive the tree print routine. This
   --     utility reads (but does not modify) treeprs.adt, the template
   --     that provides the basic structure of the file, and then fills
   --     in the data from the comments in sinfo.ads.

   --    Run XNmake to create nmake.ads and nmake.adb, the package body
   --     and spec of the Nmake package which contains functions for
   --     constructing nodes.

   --  Note: sometime we could write a utility that actually generated the
   --  body of sinfo from the spec instead of simply checking it, since, as
   --  noted above, the contents of the body can be determined from the spec.

   --------------------------------
   -- Implicit Nodes in the Tree --
   --------------------------------

   --  Generally the structure of the tree very closely follows the grammar
   --  as defined in the RM. However, certain nodes are omitted to save
   --  space and simplify semantic processing. Two general classes of such
   --  omitted nodes are as follows:

   --   If the only possibilities for a non-terminal are one or more other
   --   non terminals (i.e. the rule is a "skinny" rule), then usually the
   --   corresponding node is omitted from the tree, and the target construct
   --   appears directly. For example, a real type definition is either a
   --   floating point definition or a fixed point definition. No explicit
   --   node appears for real type definition. Instead either the floating
   --   point definition or fixed point definition appears directly.

   --   If a non-terminal corresponds to a list of some other non-terminal
   --   (possibly with separating punctuation), then usually it is omitted
   --   from the tree, and a list of components appears instead. For
   --   example, sequence of statements does not appear explicitly in the
   --   tree. Instead a list of statements appears directly.

   --  Some additional cases of omitted nodes occur and are documented
   --  individually. In particular, many nodes are omitted in the tree
   --  generated for an expression.

   -------------------------------------------
   -- Handling of Defining Identifier Lists --
   -------------------------------------------

   --  In several declarative forms in the syntax, lists of defining
   --  identifiers appear (object declarations, component declarations,
   --  number declarations etc.)

   --  The semantics of such statements are equivalent to a series of
   --  identical declarations of single defining identifiers (except that
   --  conformance checks require the same grouping of identifiers in the
   --  parameter case).

   --  To simplify semantic processing, the parser breaks down such multiple
   --  declaration cases into sequences of single declarations, duplicating
   --  type and initialization information as required. The flags More_Ids
   --  and Prev_Ids are used to record the original form of the source in
   --  the case where the original source used a list of names, More_Ids
   --  being set on all but the last name and Prev_Ids being set on all
   --  but the first name. These flags are used to reconstruct the original
   --  source (e.g. in the Sprint package), and also are included in the
   --  conformance checks, but otherwise have no semantic significance.

   --  Note: the reason that we use More_Ids and Prev_Ids rather than
   --  First_Name and Last_Name flags is so that the flags are off in the
   --  normal one identifier case, which minimizes tree print output.

   -----------------------
   -- Use of Node Lists --
   -----------------------

   --  With a few exceptions, if a construction of the form {non-terminal}
   --  appears in the tree, lists are used in the corresponding tree node
   --  (see package Nlists for handling of node lists). In this case a field
   --  of the parent node points to a list of nodes for the non-terminal. The
   --  field name for such fields has a plural name which always ends in "s".
   --  For example, a case statement has a field Alternatives pointing to a
   --  list of case statement alternative nodes.

   --  Only fields pointing to lists have names ending in "s", so generally
   --  the structure is strongly typed, fields not ending in s point to
   --  single nodes, and fields ending in s point to lists.

   --  The following example shows how a traversal of a list is written. We
   --  suppose here that Stmt points to a N_Case_Statement node which has
   --  a list field called Alternatives:

   --   Alt := First (Alternatives (Stmt));
   --   while Present (Alt) loop
   --      ..
   --      -- processing for case statement alternative Alt
   --      ..
   --      Alt := Next (Alt);
   --   end loop;

   --  The Present function tests for Empty, which in this case signals the
   --  end of the list. First returns Empty immediately if the list is empty.
   --  Present is defined in Atree, First and Next are defined in Nlists.

   --  The exceptions to this rule occur with {DEFINING_IDENTIFIERS} in all
   --  contexts, which is handled as described in the previous section, and
   --  with {,library_unit_NAME} in the N_With_Clause mode, which is handled
   --  using the First_Name and Last_Name flags, as further detailed in the
   --  description of the N_With_Clause node.

   -------------
   -- Pragmas --
   -------------

   --  Pragmas can appear in many different context, but are not included
   --  in the grammar. Still they must appear in the tree, so they can be
   --  properly processed.

   --  Two approaches are used. In some cases, an extra field is defined
   --  in an appropriate node that contains a list of pragmas appearing
   --  in the expected context. For example pragmas can appear before an
   --  Accept_Alternative in a Selective_Accept_Statement, and these pragmas
   --  appear in the Pragmas_Before field of the N_Accept_Alternative node.

   --  The other approach is to simply allow pragmas to appear in syntactic
   --  lists where the grammar (of course) does not include the possibility.
   --  For example, the Variants field of an N_Variant_Part node points to
   --  a list that can contain both N_Pragma and N_Variant nodes.

   --  To make processing easier in the latter case, the Nlists package
   --  provides a set of routines (First_Non_Pragma, Last_Non_Pragma,
   --  Next_Non_Pragma, Prev_Non_Pragma) that allow such lists to be
   --  handled ignoring all pragmas.

   --  In the case of the variants list, we can either write:

   --      Variant := First (Variants (N));
   --      while Present (Variant) loop
   --         ...
   --         Alt := Next (Alt);
   --      end loop;

   --  or

   --      Variant := First_Non_Pragma (Variants (N));
   --      while Present (Variant) loop
   --         ...
   --         Alt := Next_Non_Pragma (Alt);
   --      end loop;

   --  In the first form of the loop, Variant can either be an N_Pragma or
   --  an N_Variant node. In the second form, Variant can only be N_Variant
   --  since all pragmas are skipped.

   ---------------------
   -- Optional Fields --
   ---------------------

   --  Fields which correspond to a section of the syntax enclosed in square
   --  brackets are generally omitted (and the corresponding field set to
   --  Empty for a node, or No_List for a list). The documentation of such
   --  fields notes these cases. One exception to this rule occurs in the
   --  case of possibly empty statement sequences (such as the sequence of
   --  statements in an entry call alternative). Such cases appear in the
   --  syntax rules as [SEQUENCE_OF_STATEMENTS] and the fields corresponding
   --  to such optional statement sequences always contain an empty list (not
   --  No_List) if no statements are present.

   --  Note: the utility program that constructs the body and spec of the
   --  Nmake package relies on the format of the comments to determine if
   --  a field should have a default value in the corresponding make routine.
   --  The rule is that if the first line of the description of the field
   --  contains the string "(set to xxx if", then a default value of xxx is
   --  provided for this field in the corresponding Make_yyy routine.

   -----------------------------------
   -- Note on Body/Spec Terminology --
   -----------------------------------

   --  In informal discussions about Ada, it is customary to refer to package
   --  and subprogram specs and bodies. However, this is not technically
   --  correct, what is normally referred to as a spec or specification is in
   --  fact a package declaration or subprogram declaration. We are careful
   --  in GNAT to use the correct terminology and in particular, the full
   --  word specification is never used as an incorrect substitute for
   --  declaration. The structure and terminology used in the tree also
   --  reflects the grammar and thus uses declaration and specification in
   --  the technically correct manner.

   --  However, there are contexts in which the informal terminology is
   --  useful. We have the word "body" to refer to the Interp_Etype declared by
   --  the declaration of a unit body, and in some contexts we need a
   --  similar term to refer to the entity declared by the package or
   --  subprogram declaration, and simply using declaration can be confusing
   --  since the body also has a declaration.

   --  An example of such a context is the link between the package body
   --  and its declaration. With_Declaration is confusing, since
   --  the package body itself is a declaration.

   --  To deal with this problem, we reserve the informal term Spec, i.e.
   --  the popular abbreviation used in this context, to refer to the entity
   --  declared by the package or subprogram declaration. So in the above
   --  example case, the field in the body is called With_Spec.

   --  Another important context for the use of the word Spec is in error
   --  messages, where a hyper-correct use of declaration would be confusing
   --  to a typical Ada programmer, and even for an expert programmer can
   --  cause confusion since the body has a declaration as well.

   --  So, to summarize:

   --     Declaration    always refers to the syntactic entity that is called
   --                    a declaration. In particular, subprogram declaration
   --                    and package declaration are used to describe the
   --                    syntactic entity that includes the semicolon.

   --     Specification  always refers to the syntactic entity that is called
   --                    a specification. In particular, the terms procedure
   --                    specification, function specification, package
   --                    specification, subprogram specification always refer
   --                    to the syntactic entity that has no semicolon.

   --     Spec           is an informal term, used to refer to the entity
   --                    that is declared by a task declaration, protected
   --                    declaration, generic declaration, subprogram
   --                    declaration or package declaration.

   --  This convention is followed throughout the GNAT documentation
   --  both internal and external, and in all error message text.

   ------------------------
   -- Internal Use Nodes --
   ------------------------

   --  These are Node_Kind settings used in the internal implementation
   --  which are not logically part of the specification.

   --  N_Unused_At_Start
   --  Completely unused entry at the start of the enumeration type. This
   --  is inserted so that no legitimate value is zero, which helps to get
   --  better debugging behavior, since zero is a likely uninitialized value).

   --  N_Unused_At_End
   --  Completely unused entry at the end of the enumeration type. This is
   --  handy so that arrays with Node_Kind as the index type have an extra
   --  entry at the end (see for example the use of the Pchar_Pos_Array in
   --  Treepr, where the extra entry provides the limit value when dealing
   --  with the last used entry in the array).

   -----------------------------------------
   -- Note on the settings of Sloc fields --
   -----------------------------------------

   --  The Sloc field of nodes that come from the source is set by the
   --  parser. For internal nodes, and nodes generated during expansion
   --  the Sloc is usually set in the call to the constructor for the node.
   --  In general the Sloc value chosen for an internal node is the Sloc of
   --  the source node whose processing is responsible for the expansion. For
   --  example, the Sloc of an inherited primitive operation is the Sloc of
   --  the corresponding derived type declaration.

   --  For the nodes of a generic instantiation, the Sloc value is encoded
   --  to represent both the original Sloc in the generic unit, and the Sloc
   --  of the instantiation itself. See Sinput.ads for details.

   --  Subprogram instances create two callable entities: one is the visible
   --  subprogram instance, and the other is an anonymous subprogram nested
   --  within a wrapper package that contains the renamings for the actuals.
   --  Both of these entities have the Sloc of the defining entity in the
   --  instantiation node. This simplifies some ASIS queries.

   -----------------------
   -- Field Definitions --
   -----------------------

   --  In the following node definitions, all fields, both syntactic and
   --  semantic, are documented. The one exception is in the case of entities
   --  (defining identifiers, character literals and operator symbols),
   --  where the usage of the fields depends on the entity kind. Entity
   --  fields are fully documented in the separate package Einfo.

   --  In the node definitions, three common sets of fields are abbreviated
   --  to save both space in the documentation, and also space in the string
   --  (defined in Tree_Print_Strings) used to print trees. The following
   --  abbreviations are used:

   --  Note: the utility program that creates the Treeprs spec (in the file
   --  treeprs.ads) knows about the special fields here, so it must be
   --  modified if any change is made to these fields.

   --    "plus fields for binary operator"
   --       Chars                    (Name1)      Name_Id for the operator
   --       Left_Opnd                (Node2)      left operand expression
   --       Right_Opnd               (Node3)      right operand expression
   --       Entity                   (Node4-Sem)  defining entity for operator
   --       Associated_Node          (Node4-Sem)  for generic processing
   --       Do_Overflow_Check        (Flag17-Sem) set if overflow check needed
   --       Has_Private_View         (Flag11-Sem) set in generic units.

   --    "plus fields for unary operator"
   --       Chars                    (Name1)      Name_Id for the operator
   --       Right_Opnd               (Node3)      right operand expression
   --       Entity                   (Node4-Sem)  defining entity for operator
   --       Associated_Node          (Node4-Sem)  for generic processing
   --       Do_Overflow_Check        (Flag17-Sem) set if overflow check needed
   --       Has_Private_View         (Flag11-Sem) set in generic units.

   --    "plus fields for expression"
   --       Paren_Count                           number of parentheses levels
   --       Etype                    (Node5-Sem)  type of the expression
   --       Is_Overloaded            (Flag5-Sem)  >1 type interpretation exists
   --       Is_Static_Expression     (Flag6-Sem)  set for static expression
   --       Raises_Constraint_Error  (Flag7-Sem)  evaluation raises CE
   --       Must_Not_Freeze          (Flag8-Sem)  set if must not freeze
   --       Do_Range_Check           (Flag9-Sem)  set if a range check needed
   --       Assignment_OK            (Flag15-Sem) set if modification is OK
   --       Is_Controlling_Actual    (Flag16-Sem) set for controlling argument

   --  Note: see under (EXPRESSION) for further details on the use of
   --  the Paren_Count field to record the number of parentheses levels.

   --  Node_Kind is the type used in the Nkind field to indicate the node
   --  kind. The actual definition of this type is given later (the reason
   --  for this is that we want the descriptions ordered by logical chapter
   --  in the RM, but the type definition is reordered to facilitate the
   --  definition of some subtype ranges. The individual descriptions of
   --  the nodes show how the various fields are used in each node kind,
   --  as well as providing logical names for the fields. Functions and
   --  procedures are provided for accessing and setting these fields
   --  using these logical names.

   -----------------------
   -- Gigi Restrictions --
   -----------------------

   --  The tree passed to Gigi is more restricted than the general tree form.
   --  For example, as a result of expansion, most of the tasking nodes can
   --  never appear. For each node to which either a complete or partial
   --  restriction applies, a note entitled "Gigi restriction" appears which
   --  documents the restriction.

   --  Note that most of these restrictions apply only to trees generated when
   --  code is being generated, since they involved expander actions that
   --  destroy the tree.

   ------------------------
   -- Common Flag Fields --
   ------------------------

   --  The following flag fields appear in all nodes

   --  Analyzed
   --    This flag is used to indicate that a node (and all its children
   --    have been analyzed. It is used to avoid reanalysis of a node that
   --    has already been analyzed, both for efficiency and functional
   --    correctness reasons.

   --  Error_Posted
   --    This flag is used to avoid multiple error messages being posted
   --    on or referring to the same node. This flag is set if an error
   --    message refers to a node or is posted on its source location,
   --    and has the effect of inhibiting further messages involving
   --    this same node.

   --  Comes_From_Source
   --    This flag is on for any nodes built by the scanner or parser from
   --    the source program, and off for any nodes built by the analyzer or
   --    expander. It indicates that a node comes from the original source.
   --    This flag is defined in Atree.

   --  Has_Dynamic_Length_Check and Has_Dynamic_Range_Check also appear on
   --  all nodes. They are fully described in the next section.

   ------------------------------------
   -- Description of Semantic Fields --
   ------------------------------------

   --  The meaning of the syntactic fields is generally clear from their
   --  names without any further description, since the names are chosen
   --  to correspond very closely to the syntax in the reference manual.
   --  This section describes the usage of the semantic fields, which are
   --  used to contain additional information determined during semantic
   --  analysis.

   --  ABE_Is_Certain (Flag18-Sem)
   --    This flag is set in an instantiation node or a call node is
   --    determined to be sure to raise an ABE. This is used to trigger
   --    special handling of such cases, particularly in the instantiation
   --    case where we avoid instantiating the body if this flag is set.
   --    This flag is also present in an N_Formal_Package_Declaration_Node
   --    since formal package declarations are treated like instantiations,
   --    but it is always set to False in this context.

   --  Accept_Handler_Records (List5-Sem)
   --    This field is present only in an N_Accept_Alternative node. It is
   --    used to temporarily hold the exception handler records from an
   --    accept statement in a selective accept. These exception handlers
   --    will eventually be placed in the Handler_Records list of the
   --    procedure built for this accept (see Expand_N_Selective_Accept
   --    procedure in Exp_Ch9 for further details).

   --  Access_Types_To_Process (Elist2-Sem)
   --    Present in N_Freeze_Entity nodes for Incomplete or private types.
   --    Contains the list of access types which may require specific
   --    treatment when the nature of the type completion is completely
   --    known. An example of such treatement is the generation of the
   --    associated_final_chain.

   --  Actions (List1-Sem)
   --    This field contains a sequence of actions that are associated
   --    with the node holding the field. See the individual node types
   --    for details of how this field is used, as well as the description
   --    of the specific use for a particular node type.

   --  Activation_Chain_Entity (Node3-Sem)
   --    This is used in tree nodes representing task activators (blocks,
   --    subprogram bodies, package declarations, and task bodies). It is
   --    initially Empty, and then gets set to point to the entity for the
   --    declared Activation_Chain variable when the first task is declared.
   --    When tasks are declared in the corresponding declarative region
   --    this entity is located by name (its name is always _Chain) and
   --    the declared tasks are added to the chain.

   --  Acts_As_Spec (Flag4-Sem)
   --    A flag set in the N_Subprogram_Body node for a subprogram body
   --    which is acting as its own spec. This flag also appears in the
   --    compilation unit node at the library level for such a subprogram
   --    (see further description in spec of Lib package).

   --  Aggregate_Bounds (Node3-Sem)
   --    Present in array N_Aggregate nodes. If the aggregate contains
   --    component associations this field points to an N_Range node whose
   --    bounds give the lowest and highest discrete choice values. If the
   --    named aggregate contains a dynamic or null choice this field is
   --    empty. If the aggregate contains positional elements this field
   --    points to an N_Integer_Literal node giving the number of positional
   --    elements. Note that if the aggregate contains positional elements
   --    and an other choice the N_Integer_Literal only accounts for the
   --    number of positional elements.

   --  All_Others (Flag11-Sem)
   --    Present in an N_Others_Choice node. This flag is set in the case
   --    of an others exception where all exceptions, even those that are
   --    not normally handled (in particular the tasking abort signal) by
   --    others. This is used for translation of the at end handler into
   --    a normal exception handler.

   --  Assignment_OK (Flag15-Sem)
   --    This flag is set in a subexpression node for an object, indicating
   --    that the associated object can be modified, even if this would not
   --    normally be permissible (either by direct assignment, or by being
   --    passed as an out or in-out parameter). This is used by the expander
   --    for a number of purposes, including initialzation of constants and
   --    limited type objects (such as tasks), setting discriminant fields,
   --    setting tag values, etc. N_Object_Declaration nodes also have this
   --    flag defined. Here it is used to indicate that an initialization
   --    expression is valid, even where it would normally not be allowed
   --    (e.g. where the type involved is limited).

   --  Associated_Node (Node4-Sem)
   --    Present in nodes that can denote an entity: identifiers, character
   --    literals, operator symbols, expanded names, operator nodes and
   --    attribute reference nodes (all these nodes have an Entity field).
   --    This field is also present in N_Aggregate, N_Selected_Component,
   --    and N_Extension_Aggregate nodes. This field is used during generic
   --    processing to relate nodes in the original template to nodes in the
   --    generic copy. It overlaps the Entity field, and is used to capture
   --    global references in the analyzed copy and place them in the template.
   --    See description in Sem_Ch12 for further details on this usage.

   --  At_End_Proc (Node1)
   --    This field is present in an N_Handled_Sequence_Of_Statements node.
   --    It contains an identifier reference for the cleanup procedure to
   --    be called. See description of this node for further details.

   --  Backwards_OK (Flag6-Sem)
   --    A flag present in the N_Assignment_Statement node. It is used only
   --    if the type being assigned is an array type, and is set if analysis
   --    determines that it is definitely safe to do the copy backwards, i.e.
   --    starting at the highest addressed element. Note that if neither of
   --    the flags Forwards_OK or Backwards_OK is set, it means that the
   --    front end could not determine that either direction is definitely
   --    safe, and a runtime check is required.

   --  Body_To_Inline (Node3-Sem)
   --    present in subprogram declarations. Denotes analyzed but unexpanded
   --    body of subprogram, to be used when inlining calls. Present when the
   --    subprogram has an Inline pragma and inlining is enabled. If the
   --    declaration is completed by a renaming_as_body, and the renamed en-
   --    tity is a subprogram, the Body_To_Inline is the name of that entity,
   --    which is used directly in later calls to the original subprogram.

   --  Body_Required (Flag13-Sem)
   --    A flag that appears in the N_Compilation_Unit node indicating that
   --    the corresponding unit requires a body. For the package case, this
   --    indicates that a completion is required. In Ada 95, if the flag
   --    is not set for the package case, then a body may not be present.
   --    In Ada 83, if the flag is not set for the package case, then a
   --    body is optional. For a subprogram declaration, the flag is set
   --    except in the case where a pragma Import or Interface applies,
   --    in which case no body is permitted (in Ada 83 or Ada 95).

   --  By_Ref (Flag5-Sem)
   --    A flag present in the N_Return_Statement_Node. It is set when the
   --    returned expression is already allocated on the secondary stack
   --    and thus the result is passed by reference rather than copied
   --    another time.

   --  Compile_Time_Known_Aggregate (Flag18-Sem)
   --    Present in N_Aggregate nodes. Set for aggregates which can be
   --    fully evaluated at compile time without raising constraint error.
   --    Such aggregates can be passed as is to Gigi without any expansion.
   --    See Sem_Aggr for the specific conditions under which an aggregate
   --    has this flag set. See also the flag Static_Processing_OK.

   --  Condition_Actions (List3-Sem)
   --    This field appears in else-if nodes and in the iteration scheme
   --    node for while loops. This field is only used during semantic
   --    processing to temporarily hold actions inserted into the tree.
   --    In the tree passed to gigi, the condition actions field is always
   --    set to No_List. For details on how this field is used, see the
   --    routine Insert_Actions in package Exp_Util, and also the expansion
   --    routines for the relevant nodes.

   --  Controlling_Argument (Node1-Sem)
   --    This field is set in procedure and function call nodes if the call
   --    is a dispatching call (it is Empty for a non-dispatching call).
   --    It indicates the source of the controlling tag for the call. For
   --    Procedure calls, the Controlling_Argument is one of the actuals.
   --    For a function that has a dispatching result, it is an entity in
   --    the context of the call that can provide a tag, or else it is the
   --    tag of the root type of the class.

   --  Conversion_OK (Flag14-Sem)
   --    A flag set on type conversion nodes to indicate that the conversion
   --    is to be considered as being valid, even though it is the case that
   --    the conversion is not valid Ada. This is used for the Enum_Rep,
   --    Fixed_Value and Integer_Value attributes, for internal conversions
   --    done for fixed-point operations, and for certain conversions for
   --    calls to initialization procedures. If Conversion_OK is set, then
   --    Etype must be set (the analyzer assumes that Etype has been set).
   --    For the case of fixed-point operands, it also indicates that the
   --    conversion is to be a direct conversion of the underlying integer
   --    result, with no regard to the small operand.

   --  Corresponding_Body (Node5-Sem)
   --    This field is set in subprogram declarations, where it is needed
   --    if a pragma Inline is present and the subprogram is called, in
   --    generic declarations if the generic is instantiated, and also in
   --    package declarations that contain inlined subprograms that are
   --    called, or generic declarations that are instantiated. It points
   --    to the defining entity for the corresponding body.

   --  Corresponding_Generic_Association (Node5-Sem)
   --    This field is defined for object declarations and object renaming
   --    declarations. It is set for the declarations within an instance that
   --    map generic formals to their actuals.  If set, the field points to
   --    a generic_association which is the original parent of the expression
   --    or name appearing in the declaration. This simplifies ASIS queries.

   --  Corresponding_Integer_Value (Uint4-Sem)
   --    This field is set in real literals of fixed-point types (it is not
   --    used for floating-point types). It contains the integer value used
   --    to represent the fixed-point value. It is also set on the universal
   --    real literals used to represent bounds of fixed-point base types
   --    and their first named subtypes.

   --  Corresponding_Spec (Node5-Sem)
   --    This field is set in subprogram, package, task, and protected body
   --    nodes, where it points to the defining entity in the corresponding
   --    spec. The attribute is also set in N_With_Clause nodes, where
   --    it points to the defining entity for the with'ed spec, and in
   --    a subprogram renaming declaration when it is a Renaming_As_Body.
   --    The field is Empty if there is no corresponding spec, as in the
   --    case of a subprogram body that serves as its own spec.

   --  Corresponding_Stub (Node3-Sem)
   --    This field is present in an N_Subunit node. It holds the node in
   --    the parent unit that is the stub declaration for the subunit. it is
   --    set when analysis of the stub forces loading of the proper body. If
   --    expansion of the proper body creates new declarative nodes, they are
   --    inserted at the point of the corresponding_stub.

   --  Dcheck_Function (Node5-Sem)
   --    This field is present in an N_Variant node, It references the entity
   --    for the discriminant checking function for the variant.

   --  Debug_Statement (Node3)
   --    This field is present in an N_Pragma node. It is used only for
   --    a Debug pragma or pragma Assert with a second parameter. The
   --    parameter is of the form of an expression, as required by the
   --    pragma syntax, but is actually a procedure call. To simplify
   --    semantic processing, the parser creates a copy of the argument
   --    rearranged into a procedure call statement and places it in the
   --    Debug_Statement field. Note that this field is considered a
   --    syntactic field, since it is created by the parser.

   --  Default_Expression (Node5-Sem)
   --    This field is Empty if there is no default expression. If there
   --    is a simple default expression (one with no side effects), then
   --    this field simply contains a copy of the Expression field (both
   --    point to the tree for the default expression). Default_Expression
   --    is used for conformance checking.

   --  Delay_Finalize_Attach (Flag14-Sem)
   --    This flag is present in an N_Object_Declaration node. If it is set,
   --    then in the case of a controlled type being declared and initialized,
   --    the normal code for attaching the result to the appropriate local
   --    finalization list is suppressed. This is used for functions that
   --    return controlled types without using the secondary stack, where
   --    it is the caller who must do the attachment.

   --  Discr_Check_Funcs_Built (Flag11-Sem)
   --    This flag is present in N_Full_Type_Declaration nodes. It is set when
   --    discriminant checking functions are constructed. The purpose is to
   --    avoid attempting to set these functions more than once.

   --  Do_Access_Check (Flag11-Sem)
   --    This flag is set on nodes with a Prefix field that can be an object
   --    of an access type. If the flag is set, it indicates that a check is
   --    required to ensure that the value of the referenced object is not
   --    null. The actual check (which may be explicit or implicit by means
   --    of some trap), is generated by Gigi (all the front end does is to
   --    set this flag to request the trap).

   --  Do_Accessibility_Check (Flag13-Sem)
   --    This flag is set on N_Parameter_Specification nodes to indicate
   --    that an accessibility check is required for the parameter. It is
   --    not yet decided who takes care of this check (TBD ???).

   --  Do_Discriminant_Check (Flag13-Sem)
   --    This flag is set on N_Selected_Component nodes to indicate that a
   --    discriminant check is required using the discriminant check routine
   --    associated with the selector. The actual check is dealt with by
   --    Gigi (all the front end does is to set the flag).

   --  Do_Division_Check (Flag13-Sem)
   --    This flag is set on a division operator (/ mod rem) to indicate
   --    that a zero divide check is required. The actual check is dealt
   --    with by the backend (all the front end does is to set the flag).

   --  Do_Length_Check (Flag4-Sem)
   --    This flag is set in an N_Assignment_Statement, N_Op_And, N_Op_Or,
   --    N_Op_Xor, or N_Type_Conversion node to indicate that a length check
   --    is required. It is not determined who deals with this flag (???).

   --  Do_Overflow_Check (Flag17-Sem)
   --    This flag is set on an operator where an overflow check is required
   --    on the operation. The actual check is dealt with by the backend
   --    (all the front end does is to set the flag). The other cases where
   --    this flag is used is on a Type_Conversion node and for attribute
   --    reference nodes. For a type conversion, it means that the conversion
   --    is from one base type to another, and the value may not fit in the
   --    target base type. See also the description of Do_Range_Check for
   --    this case. The only attribute references which use this flag are
   --    Pred and Succ, where it means that the result should be checked
   --    for going outside the base range.

   --  Do_Range_Check (Flag9-Sem)
   --    This flag is set on an expression which appears in a context where
   --    a range check is required. The target type is clear from the
   --    context. The contexts in which this flag can appear are limited to
   --    the following.

   --      Right side of an assignment. In this case the target type is
   --      taken from the left side of the assignment, which is referenced
   --      by the Name of the N_Assignment_Statement node.

   --      Subscript expressions in an indexed component. In this case the
   --      target type is determined from the type of the array, which is
   --      referenced by the Prefix of the N_Indexed_Component node.

   --      Argument expression for a parameter, appearing either directly
   --      in the Parameter_Associations list of a call or as the Expression
   --      of an N_Parameter_Association node that appears in this list. In
   --      either case, the check is against the type of the formal. Note
   --      that the flag is relevant only in IN and IN OUT parameters, and
   --      will be ignored for OUT parameters, where no check is required
   --      in the call, and if a check is required on the return, it is
   --      generated explicitly with a type conversion.

   --      Initialization expression for the initial value in an object
   --      declaration. In this case the Do_Range_Check flag is set on
   --      the initialization expression, and the check is against the
   --      range of the type of the object being declared.

   --      The expression of a type conversion. In this case the range check
   --      is against the target type of the conversion. See also the use of
   --      Do_Overflow_Check on a type conversion. The distinction is that
   --      the ovrflow check protects against a value that is outside the
   --      range of the target base type, whereas a range check checks that
   --      the resulting value (which is a value of the base type of the
   --      target type), satisfies the range constraint of the target type.

   --    Note: when a range check is required in contexts other than those
   --    listed above (e.g. in a return statement), an additional type
   --    conversion node is introduced to represent the required check.

   --  Do_Storage_Check (Flag17-Sem)
   --    This flag is set in an N_Allocator node to indicate that a storage
   --    check is required for the allocation, or in an N_Subprogram_Body
   --    node to indicate that a stack check is required in the subprogram
   --    prolog. The N_Allocator case is handled by the routine that expands
   --    the call to the runtime routine. The N_Subprogram_Body case is
   --    handled by the backend, and all the semantics does is set the flag.

   --  Do_Tag_Check (Flag13-Sem)
   --    This flag is set on an N_Assignment_Statement, N_Function_Call,
   --    N_Procedure_Call_Statement, N_Type_Conversion or N_Return_Statememt
   --    node to indicate that the tag check can be suppressed. It is not
   --    yet decided how this flag is used (TBD ???).

   --  Elaborate_Present (Flag4-Sem)
   --    This flag is set in the N_With_Clause node to indicate that a
   --    pragma Elaborate pragma appears for the with'ed units.

   --  Elaborate_All_Present (Flag15-Sem)
   --    This flag is set in the N_With_Clause node to indicate that a
   --    pragma Elaborate_All pragma appears for the with'ed units.

   --  Elaboration_Boolean (Node2-Sem)
   --    This field is present in function and procedure specification
   --    nodes. If set, it points to the entity for a Boolean flag that
   --    must be tested for certain calls to check for access before
   --    elaboration. See body of Sem_Elab for further details. This
   --    field is Empty if no elaboration boolean is required.

   --  Else_Actions (List3-Sem)
   --    This field is present in conditional expression nodes. During code
   --    expansion we use the Insert_Actions procedure (in Exp_Util) to insert
   --    actions at an appropriate place in the tree to get elaborated at the
   --    right time. For conditional expressions, we have to be sure that the
   --    actions for the Else branch are only elaborated if the condition is
   --    False. The Else_Actions field is used as a temporary parking place
   --    for these actions. The final tree is always rewritten to eliminate
   --    the need for this field, so in the tree passed to Gigi, this field
   --    is always set to No_List.

   --  Enclosing_Variant (Node2-Sem)
   --    This field is present in the N_Variant node and identifies the
   --    Node_Id corresponding to the immediately enclosing variant when
   --    the variant is nested, and N_Empty otherwise. Set during semantic
   --    processing of the variant part of a record type.

   --  Entity (Node4-Sem)
   --    Appears in all direct names (identifier, character literal,
   --    operator symbol), as well as expanded names, and attributes that
   --    denote entities, such as 'Class. Points to the entity for the
   --    corresponding defining occurrence. Set after name resolution.
   --    In the case of identifiers in a WITH list, the corresponding
   --    defining occurrence is in a separately compiled file, and this
   --    pointer must be set using the library Load procedure. Note that
   --    during name resolution, the value in Entity may be temporarily
   --    incorrect (e.g. during overload resolution, Entity is initially
   --    set to the first possible correct interpretation, and then later
   --    modified if necessary to contain the correct value after resolution).
   --    Note that Associated_Node overlays this field during the processing
   --    of generics. See Sem_Ch12 for further details.

   --  Etype (Node5-Sem)
   --    Appears in all expression nodes, all direct names, and all
   --    entities. Points to the entity for the related type. Set after
   --    type resolution. Normally this is the actual subtype of the
   --    expression. However, in certain contexts such as the right side
   --    of an assignment, subscripts, arguments to calls, returned value
   --    in a function, initial value etc. it is the desired target type.
   --    In the event that this is different from the actual type, the
   --    Do_Range_Check flag will be set if a range check is required.
   --    Note: if the Is_Overloaded flag is set, then Etype points to
   --    an essentially arbitrary choice from the possible set of types.

   --  Exception_Junk (Flag11-Sem)
   --    This flag is set in a various nodes appearing in a statement
   --    sequence to indicate that the corresponding node is an artifact
   --    of the generated code for exception handling, and should be
   --    ignored when analyzing the control flow of the relevant sequence
   --    of statements (e.g. to check that it does not end with a bad
   --    return statement).

   --  Expansion_Delayed (Flag11-Sem)
   --    Set on aggregates and extension aggregates that need a top-down
   --    rather than bottom up expansion. Typically aggregate expansion
   --    happens bottom up. For nested aggregates the expansion is delayed
   --    until the enclosing aggregate itself is expanded, e.g. in the context
   --    of a declaration. To delay it we set this flag. This is done to
   --    avoid creating a temporary for each level of a nested aggregates,
   --    and also to prevent the premature generation of constraint checks.
   --    This is also a requirement if we want to generate the proper
   --    attachment to the internal finalization lists (for record with
   --    controlled components). Top down expansion of aggregates is also
   --    used for in-place array aggregate assignment or initialization.
   --    When the full context is known, the target of the assignment or
   --    initialization is used to generate the left-hand side of individual
   --    assignment to each sub-component.

   --  First_Inlined_Subprogram (Node3-Sem)
   --    Present in the N_Compilation_Unit node for the main program. Points
   --    to a chain of entities for subprograms that are to be inlined. The
   --    Next_Inlined_Subprogram field of these entities is used as a link
   --    pointer with Empty marking the end of the list. This field is Empty
   --    if there are no inlined subprograms or inlining is not active.

   --  First_Named_Actual (Node4-Sem)
   --    Present in procedure call statement and function call nodes, and
   --    also in Intrinsic nodes. Set during semantic analysis to point to
   --    the first named parameter where parameters are ordered by declaration
   --    order (as opposed to the actual order in the call which may be
   --    different due to named associations). Note: this field points to the
   --    explicit actual parameter itself, not the N_Parameter_Association
   --    node (its parent).

   --  First_Real_Statement (Node2-Sem)
   --    Present in N_Handled_Sequence_Of_Statements node. Normally set to
   --    Empty. Used only when declarations are moved into the statement
   --    part of a construct as a result of wrapping an AT END handler that
   --    is required to cover the declarations. In this case, this field is
   --    used to remember the location in the statements list of the first
   --    real statement, i.e. the statement that used to be first in the
   --    statement list before the declarations were prepended.

   --  First_Subtype_Link (Node5-Sem)
   --    Present in N_Freeze_Entity node for an anonymous base type that
   --    is implicitly created by the declaration of a first subtype. It
   --    points to the entity for the first subtype.

   --  Float_Truncate (Flag11-Sem)
   --    A flag present in type conversion nodes. This is used for float
   --    to integer conversions where truncation is required rather than
   --    rounding. Note that Gigi does not handle type conversions from real
   --    to integer with rounding (see Expand_N_Type_Conversion).

   --  Forwards_OK (Flag5-Sem)
   --    A flag present in the N_Assignment_Statement node. It is used only
   --    if the type being assigned is an array type, and is set if analysis
   --    determines that it is definitely safe to do the copy forwards, i.e.
   --    starting at the lowest addressed element. Note that if neither of
   --    the flags Forwards_OK or Backwards_OK is set, it means that the
   --    front end could not determine that either direction is definitely
   --    safe, and a runtime check is required.

   --  From_At_Mod (Flag4-Sem)
   --    This flag is set on the attribute definition clause node that is
   --    generated by a transformation of an at mod phrase in a record
   --    representation clause. This is used to give slightly different
   --    (Ada 83 compatible) semantics to such a clause, namely it is
   --    used to specify a minimum acceptable alignment for the base type
   --    and all subtypes. In Ada 95 terms, the actual alignment of the
   --    base type and all subtypes must be a multiple of the given value,
   --    and the representation clause is considered to be type specific
   --    instead of subtype specific.

   --  Generic_Parent (Node5-Sem)
   --    Generic_parent is defined on declaration nodes that are instances.
   --    The value of Generic_Parent is the generic entity from which the
   --    instance is obtained. Generic_Parent is also defined for the renaming
   --    declarations and object declarations created for the actuals in an
   --    instantiation. The generic parent of such a declaration is the
   --    corresponding generic association in the Instantiation node.

   --  Generic_Parent_Type (Node4-Sem)
   --    Generic_Parent_Type is defined on Subtype_Declaration nodes for
   --    the actuals of formal private and derived types. Within the instance,
   --    the operations on the actual are those inherited from the parent.
   --    For a formal private type, the parent type is the generic type
   --    itself. The Generic_Parent_Type is also used in an instance to
   --    determine whether a private operation overrides an inherited one.

   --  Handler_List_Entry (Node2-Sem)
   --    This field is present in N_Object_Declaration nodes. It is set only
   --    for the Handler_Record entry generated for an exception in zero cost
   --    exception handling mode. It references the corresponding item in the
   --    handler list, and is used to delete this entry if the corresponding
   --    handler is deleted during optimization. For further details on why
   --    this is required, see Exp_Ch11.Remove_Handler_Entries.

   --  Has_Dynamic_Length_Check (Flag10-Sem)
   --    This flag is present on all nodes. It is set to indicate that one
   --    of the routines in unit Checks has generated a length check action
   --    which has been inserted at the flagged node. This is used to avoid
   --    the generation of duplicate checks.

   --  Has_Dynamic_Range_Check (Flag12-Sem)
   --    This flag is present on all nodes. It is set to indicate that one
   --    of the routines in unit Checks has generated a range check action
   --    which has been inserted at the flagged node. This is used to avoid
   --    the generation of duplicate checks.

   --  Has_No_Elaboration_Code (Flag17-Sem)
   --    A flag that appears in the N_Compilation_Unit node to indicate
   --    whether or not elaboration code is present for this unit. It is
   --    initially set true for subprogram specs and bodies and for all
   --    generic units and false for non-generic package specs and bodies.
   --    Gigi may set the flag in the non-generic package case if it
   --    determines that no elaboration code is generated. Note that this
   --    flag is not related to the Is_Preelaborated status, there can be
   --    preelaborated packages that generate elaboration code, and non-
   --    preelaborated packages which do not generate elaboration code.

   --  Has_Priority_Pragma (Flag6-Sem)
   --    A flag present in N_Subprogram_Body, N_Task_Definition and
   --    N_Protected_Definition nodes to flag the presence of either
   --    a Priority or Interrupt_Priority pragma in the declaration
   --    sequence (public or private in the task and protected cases)

   --  Has_Private_View (Flag11-Sem)
   --    A flag present in generic nodes that have an entity, to indicate
   --    that the node has a private type. Used to exchange private
   --    and full declarations if the visibility at instantiation is
   --    different from the visibility at generic definition.

   --  Has_Storage_Size_Pragma (Flag5-Sem)
   --    A flag present in an N_Task_Definition node to flag the presence
   --    of a Storage_Size pragma

   --  Has_Task_Info_Pragma (Flag7-Sem)
   --    A flag present in an N_Task_Definition node to flag the presence
   --    of a Task_Info pragma. Used to detect duplicate pragmas.

   --  Has_Task_Name_Pragma (Flag8-Sem)
   --    A flag present in N_Task_Definition nodes to flag the presence
   --    of a Task_Name pragma in the declaration sequence for the task.

   --  Has_Wide_Character (Flag11-Sem)
   --    Present in string literals, set if any wide character (i.e. a
   --    character code outside the Character range) appears in the string.

   --  Hidden_By_Use_Clause (Elist4-Sem)
   --     An entity list present in use clauses that appear within
   --     instantiations. For the resolution of local entities, entities
   --     introduced by these use clauses have priority over global ones,
   --     and outer entities must be explicitly hidden/restored on exit.

   --  Implicit_With (Flag17-Sem)
   --    This flag is set in the N_With_Clause node that is implicitly
   --    generated for runtime units that are loaded by the expander, and
   --    also for package System, if it is loaded implicitly by a use of
   --    the 'Address or 'Tag attribute

   --  Includes_Infinities (Flag11-Sem)
   --    This flag is present in N_Range nodes. It is set for the range
   --    of unconstrained float types defined in Standard, which include
   --    not only the given range of values, but also legtitimately can
   --    include infinite values. This flag is false for any float type
   --    for which an explicit range is given by the programmer, even if
   --    that range is identical to the range for float.

   --  Instance_Spec (Node5-Sem)
   --    This field is present in generic instantiation nodes, and also in
   --    formal package declaration nodes (formal package declarations are
   --    treated in a manner very similar to package instantiations). It
   --    points to the node for the spec of the instance, inserted as part
   --    of the semantic processing for instantiations in Sem_Ch12.

   --  Is_Asynchronous_Call_Block (Flag7-Sem)
   --    A flag set in a Block_Statement node to indicate that it is the
   --    expansion of an asynchronous entry call. Such a block needs a
   --    cleanup handler to assure that the call is cancelled.

   --  Is_Component_Left_Opnd  (Flag13-Sem)
   --  Is_Component_Right_Opnd (Flag14-Sem)
   --    Present in concatenation nodes, to indicate that the corresponding
   --    operand is of the component type of the result. Used in resolving
   --    concatenation nodes in instances.

   --  Is_Controlling_Actual (Flag16-Sem)
   --    This flag is set on in an expression that is a controlling argument
   --    in a dispatching call. It is off in all other cases. See Sem_Disp
   --    for details of its use.

   --  Is_Machine_Number (Flag11-Sem)
   --    This flag is set in an N_Real_Literal node to indicate that the
   --    value is a machine number. This avoids some unnecessary cases
   --    of converting real literals to machine numbers.

   --  Is_Power_Of_2_For_Shift (Flag13-Sem)
   --    A flag present only in N_Op_Expon nodes. It is set when the
   --    exponentiation is of the forma 2 ** N, where the type of N is
   --    an unsigned integral subtype whose size does not exceed the size
   --    of Standard_Integer (i.e. a type that can be safely converted to
   --    Natural), and the exponentiation appears as the right operand of
   --    an integer multiplication or an integer division where the dividend
   --    is unsigned. It is also required that overflow checking is off for
   --    both the exponentiation and the multiply/divide node. If this set
   --    of conditions holds, and the flag is set, then the division or
   --    multiplication can be (and is) converted to a shift.

   --  Is_Overloaded (Flag5-Sem)
   --    A flag present in all expression nodes. Used temporarily during
   --    overloading determination. The setting of this flag is not
   --    relevant once overloading analysis is complete.

   --  Is_Protected_Subprogram_Body (Flag7-Sem)
   --    A flag set in a Subprogram_Body block to indicate that it is the
   --    implemenation of a protected subprogram. Such a body needs a
   --    cleanup handler to make sure that the associated protected object
   --    is unlocked when the subprogram completes.

   --  Is_Static_Expression (Flag6-Sem)
   --    Indicates that an expression is a static expression (RM 4.9). See
   --    spec of package Sem_Eval for full details on the use of this flag.

   --  Is_Subprogram_Descriptor (Flag16-Sem)
   --    Present in N_Object_Declaration, and set only for the object
   --    declaration generated for a subprogram descriptor in fast exception
   --    mode. See Exp_Ch11 for details of use.

   --  Is_Task_Allocation_Block (Flag6-Sem)
   --    A flag set in a Block_Statement node to indicate that it is the
   --    expansion of a task allocator, or the allocator of an object
   --    containing tasks. Such a block requires a cleanup handler to call
   --    Expunge_Unactivted_Tasks to complete any tasks that have been
   --    allocated but not activated when the allocator completes abnormally.

   --  Is_Task_Master (Flag5-Sem)
   --    A flag set in a Subprogram_Body, Block_Statement or Task_Body node
   --    to indicate that the construct is a task master (i.e. has declared
   --    tasks or declares an access to a task type).

   --  Itype (Node1-Sem)
   --    Used in N_Itype_Reference node to reference an itype for which it
   --    is important to ensure that it is defined. See description of this
   --    node for further details.

   --  Kill_Range_Check (Flag11-Sem)
   --    Used in an N_Unchecked_Type_Conversion node to indicate that the
   --    result should not be subjected to range checks. This is used for
   --    the implementation of Normalize_Scalars.

   --  Label_Construct (Node2-Sem)
   --    Used in an N_Implicit_Label_Declaration node. Refers to an N_Label,
   --    N_Block_Statement or N_Loop_Statement node to which the label
   --    declaration applies. This is not currently used in the compiler
   --    itself, but it is useful in the implementation of ASIS queries.

   --  Library_Unit (Node4-Sem)
   --    In a stub node, the Library_Unit field points to the compilation unit
   --    node of the corresponding subunit.
   --
   --    In a with clause node, the Library_Unit field points to the spec
   --    of the with'ed unit.
   --
   --    In a compilation unit node, the use of this field depends on
   --    the unit type:
   --
   --     For a subprogram body, the Library_Unit field points to the
   --     compilation unit node of the corresponding spec, unless
   --     Acts_As_Spec is set, in which case it points to itself.
   --
   --     For a package body, the Library_Unit field points to the
   --     compilation unit node of the corresponding spec.
   --
   --     For a subprogram spec to which pragma Inline applies, the
   --     Library_Unit field points to the compilation unit node of
   --     the corresponding body, if inlining is active.
   --
   --     For a generic declaration, the Library_Unit field points
   --     to the compilation unit node of the corresponding generic body.
   --
   --     For a subunit, the Library_Unit field points to the compilation
   --     unit node of the parent body.
   --
   --    Note that this field is not used to hold the parent pointer for a
   --    child unit (which might in any case need to use it for some other
   --    purpose as described above). Instead for a child unit, implicit
   --    with's are generated for all parents.

   --  Loop_Actions (List2-Sem)
   --    A list present in Component_Association nodes in array aggregates.
   --    Used to collect actions that must be executed within the loop because
   --    they may need to be evaluated anew each time through.

   --  Must_Not_Freeze (Flag8-Sem)
   --    A flag present in all expression nodes. Normally expressions cause
   --    freezing as described in the RM. If this flag is set, then this
   --    is inhibited. This is used by the analyzer and expander to label
   --    nodes that are created by semantic analysis or expansion and which
   --    must not cause freezing even though they normally would. This flag
   --    is also present in an N_Subtype_Indication node, since we also use
   --    these in calls to Freeze_Expression.

   --  Next_Entity (Node2-Sem)
   --    Present in defining identifiers, defining character literals and
   --    defining operator symbols (i.e. in all entities). The entities of
   --    a scope are chained, and this field is used as the forward pointer
   --    for this list. See Einfo for further details.

   --  Next_Named_Actual (Node4-Sem)
   --    Present in parameter association node. Set during semantic
   --    analysis to point to the next named parameter, where parameters
   --    are ordered by declaration order (as opposed to the actual order
   --    in the call, which may be different due to named associations).
   --    Not that this field points to the explicit actual parameter itself,
   --    not to the N_Parameter_Association node (its parent).

   --  Next_Rep_Item (Node4-Sem)
   --    Present in pragma nodes and attribute definition nodes. Used to
   --    link representation items that apply to an entity. See description
   --    of First_Rep_Item field in Einfo for full details.

   --  Next_Use_Clause (Node3-Sem)
   --    While use clauses are active during semantic processing, they
   --    are chained from the scope stack entry, using Next_Use_Clause
   --    as a link pointer, with Empty marking the end of the list. The
   --    head pointer is in the scope stack entry (First_Use_Clause). At
   --    the end of semantic processing (i.e. when Gigi sees the tree,
   --    the contents of this field is undefined and should not be read).

   --  No_Ctrl_Actions (Flag7-Sem)
   --    Present in N_Assignment_Statement to indicate that no finalize nor
   --    nor adjust should take place on this assignment eventhough the rhs
   --    is controlled. This is used in init_procs and aggregate expansions
   --    where the generated assignments are more initialisations than real
   --    assignments.

   --  No_Entities_Ref_In_Spec (Flag8-Sem)
   --    Present in N_With_Clause nodes. Set if the with clause is on the
   --    package or subprogram spec where the main unit is the corresponding
   --    body, and no entities of the with'ed unit are referenced by the spec
   --    (an entity may still be referenced in the body, so this flag is used
   --    to generate the proper message (see Sem_Util.Check_Unused_Withs for
   --    full details)

   --  No_Initialization (Flag13-Sem)
   --    Present in N_Object_Declaration & N_Allocator to indicate
   --    that the object must not be initialized (by Initialize or a
   --    call to _init_proc). This is needed for controlled aggregates.
   --    When the Object declaration has an expression, this flag means
   --    that this expression should not be taken into account (needed
   --    for in place initialization with aggregates)

   --  OK_For_Stream (Flag4-Sem)
   --    Present in N_Attribute_Definition clauses for stream attributes. If
   --    set, indicates that the attribute is permitted even though the type
   --    involved is a limited type. In the case of a protected type, the
   --    result is to stream all components (including discriminants) in
   --    lexical order. For other limited types, the effect is simply to
   --    use the corresponding stream routine for the full type. This flag
   --    is used for internally generated code, where the streaming of these
   --    types is required, even though not normally allowed by the language.

   --  Original_Discriminant (Node2-Sem)
   --    Present in identifiers. Used in references to discriminants that
   --    appear in generic units. Because the names of the discriminants
   --    may be different in  an instance, we use this field to recover the
   --    position of the discriminant in the original type, and replace it
   --    with the discriminant at the same position in the instantiated type.

   --  Others_Discrete_Choices (List1-Sem)
   --    When a case statement or variant is analyzed, the semantic checks
   --    determine the actual list of choices that correspond to an others
   --    choice. This list is materialized for later use by the expander
   --    and the Others_Discrete_Choices field of an N_Others_Choice node
   --    points to this materialized list of choices, which is in standard
   --    format for a list of discrete choices, except that of course it
   --    cannot contain an N_Others_Choice entry.

   --  Parameter_List_Truncated (Flag17-Sem)
   --    Present in N_Function_Call and N_Procedure_Call_Statement nodes.
   --    Set (for OpenVMS ports of GNAT only) if the parameter list is
   --    truncated as a result of a First_Optional_Parameter specification
   --    in an Import_Function, Import_Procedure, or Import_Valued_Procedure
   --    pragma. The truncation is done by the expander by removing trailing
   --    parameters from the argument list, in accordance with the set of
   --    rules allowing such parameter removal. In particular, parameters
   --    can be removed working from the end of the parameter list backwards
   --    up to and including the entry designated by First_Optional_Parameter
   --    in the Import pragma. Parameters can be removed if they are implicit
   --    and the default value is a known-at-compile-time value, including
   --    the use of the Null_Parameter attribute, or if explicit parameter
   --    values are present that match the corresponding defaults.

   --  Parent_Spec (Node4-Sem)
   --    For a library unit that is a child unit spec (package or subprogram
   --    declaration, generic declaration or instantiation, or library level
   --    rename, this field points to the compilation unit node for the parent
   --    package specification. This field is Empty for library bodies (the
   --    parent spec in this case can be found from the corresponding spec).

   --  Present_Expr (Uint3-Sem)
   --    Present in an N_Variant node. This has a meaningful value only after
   --    Gigi has back annotated the tree with representation information.
   --    At this point, it contains a reference to a gcc expression that
   --    depends on the values of one or more discriminants. Give a set of
   --    discriminant values, this expression evaluates to False (zero) if
   --    variant is not present, and True (non-zero) if it is present. See
   --    unit Repinfo for further details on gigi back annotation. This
   --    field is used during ASIS processing (data decomposition annex)
   --    to determine if a field is present or not.

   --  Print_In_Hex (Flag13-Sem)
   --    Set on an N_Integer_Literal node to indicate that the value should
   --    be printed in hexadecimal in the sprint listing. Has no effect on
   --    legality or semantics of program, only on the displayed output.
   --    This is used to clarify output from the packed array cases.

   --  Procedure_To_Call (Node4-Sem)
   --    Present in N_Allocator. N_Free_Statement, and N_Return_Statement
   --    nodes. References the entity for the declaration of the procedure
   --    to be called to accomplish the required operation (i.e. for the
   --    Allocate procedure in the case of N_Allocator and N_Return_Statement
   --    (for allocating the return value), and for the Deallocate procedure
   --    in the case of N_Free_Statement.

   --  Raises_Constraint_Error (Flag7-Sem)
   --    Set on an expression whose evaluation will definitely fail a
   --    constraint error check. In the case of static expressions, this
   --    flag must be set accurately (and if it is set, the expression is
   --    typically illegal unless it appears as a non-elaborated branch of
   --    a short-circuit form). For a non-static expression, this flag may
   --    be set whenever an expression (e.g. an aggregate) is known to raise
   --    constraint error. If set, the expression definitely will raise CE
   --    if elaborated at runtime. If not set, the expression may or may
   --    not raise CE. In other words, on static expressions, the flag is
   --    set accurately, on non-static expressions it is set conservatively.

   --  Redundant_Use (Flag13-Sem)
   --    A flag present in nodes that can appear as an operand in a use
   --    clause or use type clause (identifiers, expanded names, attribute
   --    references). Set to indicate that a use is redundant (and therefore
   --    need not be undone on scope exit).

   --  Return_Type (Node2-Sem)
   --    Present in N_Return_Statement node. For a procedure, this is set
   --    to Standard_Void_Type. For a function it references the entity
   --    for the returned type.

   --  Rounded_Result (Flag18-Sem)
   --    Present in N_Type_Conversion, N_Op_Divide and N_Op_Multiply nodes.
   --    Used in the fixed-point cases to indicate that the result must be
   --    rounded as a result of the use of the 'Round attribute. Also used
   --    for integer N_Op_Divide nodes to indicate that the result should
   --    be rounded to the nearest integer (breaking ties away from zero),
   --    rather than truncated towards zero as usual. These rounded integer
   --    operations are the result of expansion of rounded fixed-point
   --    divide, conersion and multiplication operations.

   --  Scope (Node3-Sem)
   --    Present in defining identifiers, defining character literals and
   --    defining operator symbols (i.e. in all entities). The entities of
   --    a scope all use this field to reference the corresponding scope
   --    entity. See Einfo for further details.

   --  Shift_Count_OK (Flag4-Sem)
   --    A flag present in shift nodes to indicate that the shift count is
   --    known to be in range, i.e. is in the range from zero to word length
   --    minus one. If this flag is not set, then the shift count may be
   --    outside this range, i.e. larger than the word length, and the code
   --    must ensure that such shift counts give the appropriate result.

   --  Source_Type (Node1-Sem)
   --    Used in an N_Validate_Unchecked_Conversion node to point to the
   --    source type entity for the unchecked conversion instantiation
   --    which gigi must do size validation for.

   --  Static_Processing_OK (Flag4-Sem)
   --    Present in N_Aggregate nodes. When the Compile_Time_Known_Aggregate
   --    flag is set, the full value of the aggregate can be determined at
   --    compile time and the aggregate can be passed as is to the back-end.
   --    In this event it is irrelevant whether this flag is set or not.
   --    However, if the Compile_Time_Known_Aggregate flag is not set but
   --    Static_Processing_OK is set, the aggregate can (but need not) be
   --    converted into a compile time known aggregate by the expander.
   --    See Sem_Aggr for the specific conditions under which an aggregate
   --    has its Static_Processing_OK flag set.

   --  Storage_Pool (Node1-Sem)
   --    Present in N_Allocator, N_Free_Statement and N_Return_Statement
   --    nodes. References the entity for the storage pool to be used for
   --    the allocate or free call or for the allocation of the returned
   --    value from a function. Empty indicates that the global default
   --    default pool is to be used. Note that in the case of a return
   --    statement, this field is set only if the function returns a
   --    value of a type whose size is not known at compile time on the
   --    secondary stack. It is never set on targets for which the target
   --    parameter Targparm.Functions_Return_By_DSP_On_Target is True.

   --  Target_Type (Node2-Sem)
   --    Used in an N_Validate_Unchecked_Conversion node to point to the
   --    target type entity for the unchecked conversion instantiation
   --    which gigi must do size validation for.

   --  Task_Body_Procedure (Node2-Sem)
   --    Present in task type declaration nodes. Points to the entity for
   --    the task body procedure (as further described in Exp_Ch9, task
   --    bodies are expanded into procedures). A convenient function to
   --    retrieve this field is Sem_Util.Get_Task_Body_Procedure.

   --  Then_Actions (List3-Sem)
   --    This field is present in conditional expression nodes. During code
   --    expansion we use the Insert_Actions procedure (in Exp_Util) to insert
   --    actions at an appropriate place in the tree to get elaborated at the
   --    right time. For conditional expressions, we have to be sure that the
   --    actions for the Then branch are only elaborated if the condition is
   --    True. The Then_Actions field is used as a temporary parking place
   --    for these actions. The final tree is always rewritten to eliminate
   --    the need for this field, so in the tree passed to Gigi, this field
   --    is always set to No_List.

   --  Treat_Fixed_As_Integer (Flag14-Sem)
   --    This flag appears in operator nodes for divide, multiply, mod and
   --    rem on fixed-point operands. It indicates that the operands are
   --    to be treated as integer values, ignoring small values. This flag
   --    is only set as a result of expansion of fixed-point operations.
   --    Typically a fixed-point multplication in the source generates
   --    subsidiary multiplication and division operations that work with
   --    the underlying integer values and have this flag set. Note that
   --    this flag is not needed on other arithmetic operations (add, neg,
   --    subtract etc) since in these cases it is always the case that fixed
   --    is treated as integer. The Etype field MUST be set if this flag
   --    is set. The analyzer knows to leave such nodes alone, and whoever
   --    makes them must set the correct Etype value.

   --  TSS_Elist (Elist3-Sem)
   --    Present in N_Freeze_Entity nodes. Holds an element list containing
   --    entries for each TSS (type support subprogram) associated with the
   --    frozen type. The elements of the list are the entities for the
   --    subprograms (see package Exp_TSS for further details). Set to
   --    No_Elist if there are no type support subprograms for the type
   --    or if the freeze node is not for a type.

   --  Unreferenced_In_Spec (Flag7-Sem)
   --    Present in N_With_Clause nodes. Set if the with clause is on the
   --    package or subprogram spec where the main unit is the corresponding
   --    body, and is not referenced by the spec (it may still be referenced
   --    by the body, so this flag is used to generate the proper message
   --    (see Sem_Util.Check_Unused_Withs for details)

   --  Was_Originally_Stub (Flag13-Sem)
   --    This flag is set in the node for a proper body that replaces a
   --    stub. During the analysis procedure, stubs in some situations
   --    get rewritten by the corresponding bodies, and we set this flag
   --    to remember that this happened. Note that it is not good enough
   --    to rely on the use of Original_Tree here because of the case of
   --    nested instantiations where the substituted node can be copied.

   --  Zero_Cost_Handling (Flag5-Sem)
   --    This flag is set in all handled sequence of statement and exception
   --    handler nodes if eceptions are to be handled using the zero-cost
   --    mechanism (see Ada.Exceptions and System.Exceptions in files
   --    a-except.ads/adb and s-except.ads for full details). What gigi
   --    needs to do for such a handler is simply to put the code in the
   --    handler somewhere. The front end has generated all necessary labels.

   --------------------------------------------------
   -- Note on Use of End_Label and End_Span Fields --
   --------------------------------------------------

   --  Several constructs have end lines:

   --    Loop Statement             end loop [loop_IDENTIFIER];
   --    Package Specification      end [[PARENT_UNIT_NAME .] IDENTIFIER]
   --    Task Definition            end [task_IDENTIFIER]
   --    Protected Definition       end [protected_IDENTIFIER]
   --    Protected Body             end [protected_IDENTIFIER]

   --    Block Statement            end [block_IDENTIFIER];
   --    Subprogram Body            end [DESIGNATOR];
   --    Package Body               end [[PARENT_UNIT_NAME .] IDENTIFIER];
   --    Task Body                  end [task_IDENTIFIER];
   --    Accept Statement           end [entry_IDENTIFIER]];
   --    Entry Body                 end [entry_IDENTIFIER];

   --    If Statement               end if;
   --    Case Statement             end case;

   --    Record Definition          end record;

   --  The End_Label and End_Span fields are used to mark the locations
   --  of these lines, and also keep track of the label in the case where
   --  a label is present.

   --  For the first group above, the End_Label field of the corresponding
   --  node is used to point to the label identifier. In the case where
   --  there is no label in the source, the parser supplies a dummy
   --  identifier (with Comes_From_Source set to False), and the Sloc
   --  of this dummy identifier marks the location of the token following
   --  the END token.

   --  For the second group, the use of End_Label is similar, but the
   --  End_Label is found in the N_Handled_Sequence_Of_Statements node.
   --  This is done simply because in some cases there is no room in
   --  the parent node.

   --  For the third group, there is never any label, and instead of
   --  using End_Label, we use the End_Span field which gives the
   --  location of the token following END, relative to the starting
   --  Sloc of the construct, i.e. add Sloc (Node) + End_Span (Node)
   --  to get the Sloc of the IF or CASE following the End_Label.

   --  The record definition case is handled specially, we treat it
   --  as though it required an optional label which is never present,
   --  and so the parser always builds a dummy identifier with Comes
   --  From Source set False. The reason we do this, rather than using
   --  End_Span in this case, is that we want to generate a cross-ref
   --  entry for the end of a record, since it represents a scope for
   --  name declaration purposes.

   --  Note: the reason we store the difference as a Uint, instead of
   --  storing the Source_Ptr value directly, is that Source_Ptr values
   --  cannot be distinguished from other types of values, and we count
   --  on all general use fields being self describing. To make things
   --  easier for clients, note that we provide function End_Location,
   --  and procedure Set_End_Location to allow access to the logical
   --  value (which is the Source_Ptr value for the end token).

   ---------------------
   -- Syntactic Nodes --
   ---------------------

      ---------------------
      -- 2.3  Identifier --
      ---------------------

      --  IDENTIFIER ::= IDENTIFIER_LETTER {[UNDERLINE] LETTER_OR_DIGIT}
      --  LETTER_OR_DIGIT ::= IDENTIFIER_LETTER | DIGIT

      --  An IDENTIFIER shall not be a reserved word

      --  In the Ada grammar identifiers are the bottom level tokens which
      --  have very few semantics. Actual program identifiers are direct
      --  names. If we were being 100% honest with the grammar, then we would
      --  have a node called N_Direct_Name which would point to an identifier.
      --  However, that's too many extra nodes, so we just use the N_Identifier
      --  node directly as a direct name, and it contains the expression fields
      --  and Entity field that correspond to its use as a direct name. In
      --  those few cases where identifiers appear in contexts where they are
      --  not direct names (pragmas, pragma argument associations, attribute
      --  references and attribute definition clauses), the Chars field of the
      --  node contains the Name_Id for the identifier name.

      --  Note: in GNAT, a reserved word can be treated as an identifier
      --  in two cases. First, an incorrect use of a reserved word as an
      --  identifier is diagnosed and then treated as a normal identifier.
      --  Second, an attribute designator of the form of a reserved word
      --  (access, delta, digits, range) is treated as an identifier.

      --  Note: The set of letters that is permitted in an identifier depends
      --  on the character set in use. See package Csets for full details.

      --  N_Identifier
      --  Sloc points to identifier
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Entity (Node4-Sem)
      --  Associated_Node (Node4-Sem)
      --  Original_Discriminant (Node2-Sem)
      --  Redundant_Use (Flag13-Sem)
      --  Has_Private_View (Flag11-Sem) (set in generic units)
      --  plus fields for expression

      --------------------------
      -- 2.4  Numeric Literal --
      --------------------------

      --  NUMERIC_LITERAL ::= DECIMAL_LITERAL | BASED_LITERAL

      ----------------------------
      -- 2.4.1  Decimal Literal --
      ----------------------------

      --  DECIMAL_LITERAL ::= NUMERAL [.NUMERAL] [EXPONENT]

      --  NUMERAL ::= DIGIT {[UNDERLINE] DIGIT}

      --  EXPONENT ::= E [+] NUMERAL | E - NUMERAL

      --  Decimal literals appear in the tree as either integer literal nodes
      --  or real literal nodes, depending on whether a period is present.

      --  Note: literal nodes appear as a result of direct use of literals
      --  in the source program, and also as the result of evaluating
      --  expressions at compile time. In the latter case, it is possible
      --  to construct real literals that have no syntactic representation
      --  using the standard literal format. Such literals are listed by
      --  Sprint using the notation [numerator / denominator].

      --  N_Integer_Literal
      --  Sloc points to literal
      --  Intval (Uint3) contains integer value of literal
      --  plus fields for expression
      --  Print_In_Hex (Flag13-Sem)

      --  N_Real_Literal
      --  Sloc points to literal
      --  Realval (Ureal3) contains real value of literal
      --  Corresponding_Integer_Value (Uint4-Sem)
      --  Is_Machine_Number (Flag11-Sem)
      --  plus fields for expression

      --------------------------
      -- 2.4.2  Based Literal --
      --------------------------

      --  BASED_LITERAL ::=
      --   BASE # BASED_NUMERAL [.BASED_NUMERAL] # [EXPONENT]

      --  BASE ::= NUMERAL

      --  BASED_NUMERAL ::=
      --    EXTENDED_DIGIT {[UNDERLINE] EXTENDED_DIGIT}

      --  EXTENDED_DIGIT ::= DIGIT | A | B | C | D | E | F

      --  Based literals appear in the tree as either integer literal nodes
      --  or real literal nodes, depending on whether a period is present.

      ----------------------------
      -- 2.5  Character Literal --
      ----------------------------

      --  CHARACTER_LITERAL ::= ' GRAPHIC_CHARACTER '

      --  N_Character_Literal
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Char_Literal_Value (Char_Code2) contains the literal value
      --  Entity (Node4-Sem)
      --  Associated_Node (Node4-Sem)
      --  Has_Private_View (Flag11-Sem) set in generic units.
      --  plus fields for expression

      --  Note: the Entity field will be missing (and set to Empty) for
      --  character literals whose type is Standard.Wide_Character or
      --  Standard.Character or a type derived from one of these two.
      --  In this case the character literal stands for its own coding.
      --  The reason we take this irregular short cut is to avoid the
      --  need to build lots of junk defining character literal nodes.

      -------------------------
      -- 2.6  String Literal --
      -------------------------

      --  STRING LITERAL ::= "{STRING_ELEMENT}"

      --  A STRING_ELEMENT is either a pair of quotation marks ("), or a
      --  single GRAPHIC_CHARACTER other than a quotation mark.

      --  N_String_Literal
      --  Sloc points to literal
      --  Strval (Str3) contains Id of string value
      --  Has_Wide_Character (Flag11-Sem)
      --  plus fields for expression

      ------------------
      -- 2.7  Comment --
      ------------------

      --  A COMMENT starts with two adjacent hyphens and extends up to the
      --  end of the line. A COMMENT may appear on any line of a program.

      --  Comments are skipped by the scanner and do not appear in the tree.
      --  It is possible to reconstruct the position of comments with respect
      --  to the elements of the tree by using the source position (Sloc)
      --  pointers that appear in every tree node.

      -----------------
      -- 2.8  Pragma --
      -----------------

      --  PRAGMA ::= pragma IDENTIFIER
      --    [(PRAGMA_ARGUMENT_ASSOCIATION {, PRAGMA_ARGUMENT_ASSOCIATION})];

      --  Note that a pragma may appear in the tree anywhere a declaration
      --  or a statement may appear, as well as in some other situations
      --  which are explicitly documented.

      --  N_Pragma
      --  Sloc points to PRAGMA
      --  Chars (Name1) identifier name from pragma identifier
      --  Pragma_Argument_Associations (List2) (set to No_List if none)
      --  Debug_Statement (Node3) (set to Empty if not Debug, Assert)
      --  Next_Rep_Item (Node4-Sem)

      --------------------------------------
      -- 2.8  Pragma Argument Association --
      --------------------------------------

      --  PRAGMA_ARGUMENT_ASSOCIATION ::=
      --    [pragma_argument_IDENTIFIER =>] NAME
      --  | [pragma_argument_IDENTIFIER =>] EXPRESSION

      --  N_Pragma_Argument_Association
      --  Sloc points to first token in association
      --  Chars (Name1) (set to No_Name if no pragma argument identifier)
      --  Expression (Node3)

      ------------------------
      -- 2.9  Reserved Word --
      ------------------------

      --  Reserved words are parsed by the scanner, and returned as the
      --  corresponding token types (e.g. PACKAGE is returned as Tok_Package)

      ----------------------------
      -- 3.1  Basic Declaration --
      ----------------------------

      --  BASIC_DECLARATION ::=
      --    TYPE_DECLARATION          | SUBTYPE_DECLARATION
      --  | OBJECT_DECLARATION        | NUMBER_DECLARATION
      --  | SUBPROGRAM_DECLARATION    | ABSTRACT_SUBPROGRAM_DECLARATION
      --  | PACKAGE_DECLARATION       | RENAMING_DECLARATION
      --  | EXCEPTION_DECLARATION     | GENERIC_DECLARATION
      --  | GENERIC_INSTANTIATION

      --  Basic declaration also includes IMPLICIT_LABEL_DECLARATION
      --  see further description in section on semantic nodes.

      --  Also, in the tree that is constructed, a pragma may appear
      --  anywhere that a declaration may appear.

      ------------------------------
      -- 3.1  Defining Identifier --
      ------------------------------

      --  DEFINING_IDENTIFIER ::= IDENTIFIER

      --  A defining identifier is an entity, which has additional fields
      --  depending on the setting of the Ekind field. These additional
      --  fields are defined (and access subprograms declared) in package
      --  Einfo.

      --  Note: N_Defining_Identifier is an extended node whose fields are
      --  deliberate layed out to match the layout of fields in an ordinary
      --  N_Identifier node allowing for easy alteration of an identifier
      --  node into a defining identifier node. For details, see procedure
      --  Sinfo.CN.Change_Identifier_To_Defining_Identifier.

      --  N_Defining_Identifier
      --  Sloc points to identifier
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Next_Entity (Node2-Sem)
      --  Scope (Node3-Sem)
      --  Etype (Node5-Sem)

      -----------------------------
      -- 3.2.1  Type Declaration --
      -----------------------------

      --  TYPE_DECLARATION ::=
      --    FULL_TYPE_DECLARATION
      --  | INCOMPLETE_TYPE_DECLARATION
      --  | PRIVATE_TYPE_DECLARATION
      --  | PRIVATE_EXTENSION_DECLARATION

      ----------------------------------
      -- 3.2.1  Full Type Declaration --
      ----------------------------------

      --  FULL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      is TYPE_DEFINITION;
      --  | TASK_TYPE_DECLARATION
      --  | PROTECTED_TYPE_DECLARATION

      --  The full type declaration node is used only for the first case. The
      --  second case (concurrent type declaration), is represented directly
      --  by a task type declaration or a protected type declaration.

      --  N_Full_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List4) (set to No_List if none)
      --  Type_Definition (Node3)
      --  Discr_Check_Funcs_Built (Flag11-Sem)

      ----------------------------
      -- 3.2.1  Type Definition --
      ----------------------------

      --  TYPE_DEFINITION ::=
      --    ENUMERATION_TYPE_DEFINITION  | INTEGER_TYPE_DEFINITION
      --  | REAL_TYPE_DEFINITION         | ARRAY_TYPE_DEFINITION
      --  | RECORD_TYPE_DEFINITION       | ACCESS_TYPE_DEFINITION
      --  | DERIVED_TYPE_DEFINITION

      --------------------------------
      -- 3.2.2  Subtype Declaration --
      --------------------------------

      --  SUBTYPE_DECLARATION ::=
      --    subtype DEFINING_IDENTIFIER is SUBTYPE_INDICATION;

      --  The subtype indication field is set to Empty for subtypes
      --  declared in package Standard (Positive, Natural).

      --  N_Subtype_Declaration
      --  Sloc points to SUBTYPE
      --  Defining_Identifier (Node1)
      --  Subtype_Indication (Node5)
      --  Generic_Parent_Type (Node4-Sem) (set for an actual derived type).
      --  Exception_Junk (Flag11-Sem)

      -------------------------------
      -- 3.2.2  Subtype Indication --
      -------------------------------

      --  SUBTYPE_INDICATION ::= SUBTYPE_MARK [CONSTRAINT]

      --  Note: if no constraint is present, the subtype indication appears
      --  directly in the tree as a subtype mark. The N_Subtype_Indication
      --  node is used only if a constraint is present.

      --  Note: the reason that this node has expression fields is that a
      --  subtype indication can appear as an operand of a membership test.

      --  N_Subtype_Indication
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark (Node4)
      --  Constraint (Node3)
      --  Etype (Node5-Sem)
      --  Must_Not_Freeze (Flag8-Sem)

      --  Note: Etype is a copy of the Etype field of the Subtype_Mark. The
      --  reason for this redundancy is so that in a list of array index types,
      --  the Etype can be uniformly accessed to determine the subscript type.
      --  This means that no Itype is constructed for the actual subtype that
      --  is created by the subtype indication. If such an Itype is required,
      --  it is constructed in the context in which the indication appears.

      -------------------------
      -- 3.2.2  Subtype Mark --
      -------------------------

      --  SUBTYPE_MARK ::= subtype_NAME

      -----------------------
      -- 3.2.2  Constraint --
      -----------------------

      --  CONSTRAINT ::= SCALAR_CONSTRAINT | COMPOSITE_CONSTRAINT

      ------------------------------
      -- 3.2.2  Scalar Constraint --
      ------------------------------

      --  SCALAR_CONSTRAINT ::=
      --    RANGE_CONSTRAINT | DIGITS_CONSTRAINT | DELTA_CONSTRAINT

      ---------------------------------
      -- 3.2.2  Composite Constraint --
      ---------------------------------

      --  COMPOSITE_CONSTRAINT ::=
      --    INDEX_CONSTRAINT | DISCRIMINANT_CONSTRAINT

      -------------------------------
      -- 3.3.1  Object Declaration --
      -------------------------------

      --  OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      SUBTYPE_INDICATION [:= EXPRESSION];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ARRAY_TYPE_DEFINITION [:= EXPRESSION];
      --  | SINGLE_TASK_DECLARATION
      --  | SINGLE_PROTECTED_DECLARATION

      --  Note: aliased is not permitted in Ada 83 mode

      --  The N_Object_Declaration node is only for the first two cases.
      --  Single task declaration is handled by P_Task (9.1)
      --  Single protected declaration is handled by P_protected (9.5)

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  Note: if a range check is required for the initialization
      --  expression then the Do_Range_Check flag is set in the Expression,
      --  with the check being done against the type given by the object
      --  definition, which is also the Etype of the defining identifier.

      --  Note: the contents of the Expression field must be ignored (i.e.
      --  treated as though it were Empty) if No_Initialization is set True.

      --  N_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Aliased_Present (Flag4) set if ALIASED appears
      --  Constant_Present (Flag17) set if CONSTANT appears
      --  Object_Definition (Node4) subtype indication/array type definition
      --  Expression (Node3) (set to Empty if not present)
      --  Handler_List_Entry (Node2-Sem)
      --  Corresponding_Generic_Association (Node5-Sem)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)
      --  No_Initialization (Flag13-Sem)
      --  Assignment_OK (Flag15-Sem)
      --  Exception_Junk (Flag11-Sem)
      --  Delay_Finalize_Attach (Flag14-Sem)
      --  Is_Subprogram_Descriptor (Flag16-Sem)

      -------------------------------------
      -- 3.3.1  Defining Identifier List --
      -------------------------------------

      --  DEFINING_IDENTIFIER_LIST ::=
      --    DEFINING_IDENTIFIER {, DEFINING_IDENTIFIER}

      -------------------------------
      -- 3.3.2  Number Declaration --
      -------------------------------

      --  NUMBER_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : constant := static_EXPRESSION;

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical expressions. To simplify semantic processing, the parser
      --  represents a multiple declaration case as a sequence of single
      --  declarations, using the More_Ids and Prev_Ids flags to preserve
      --  the original source form as described in the section on "Handling
      --  of Defining Identifier Lists".

      --  N_Number_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Expression (Node3)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      ----------------------------------
      -- 3.4  Derived Type Definition --
      ----------------------------------

      --  DERIVED_TYPE_DEFINITION ::=
      --    [abstract] new parent_SUBTYPE_INDICATION [RECORD_EXTENSION_PART]

      --  Note: ABSTRACT, record extension part not permitted in Ada 83 mode

      --  Note: a record extension part is required if ABSTRACT is present

      --  N_Derived_Type_Definition
      --  Sloc points to NEW
      --  Abstract_Present (Flag4)
      --  Subtype_Indication (Node5)
      --  Record_Extension_Part (Node3) (set to Empty if not present)

      ---------------------------
      -- 3.5  Range Constraint --
      ---------------------------

      --  RANGE_CONSTRAINT ::= range RANGE

      --  N_Range_Constraint
      --  Sloc points to RANGE
      --  Range_Expression (Node4)

      ----------------
      -- 3.5  Range --
      ----------------

      --  RANGE ::=
      --    RANGE_ATTRIBUTE_REFERENCE
      --  | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

      --  Note: the case of a range given as a range attribute reference
      --  appears directly in the tree as an attribute reference.

      --  Note: the field name for a reference to a range is Range_Expression
      --  rather than Range, because range is a reserved keyword in Ada!

      --  Note: the reason that this node has expression fields is that a
      --  range can appear as an operand of a membership test. The Etype
      --  field is the type of the range (we do NOT construct an implicit
      --  subtype to represent the range exactly).

      --  N_Range
      --  Sloc points to ..
      --  Low_Bound (Node1)
      --  High_Bound (Node2)
      --  Includes_Infinities (Flag11)
      --  plus fields for expression

      --  Note: if the range appears in a context, such as a subtype
      --  declaration, where range checks are required on one or both of
      --  the expression fields, then type conversion nodes are inserted
      --  to represent the required checks.

      ----------------------------------------
      -- 3.5.1  Enumeration Type Definition --
      ----------------------------------------

      --  ENUMERATION_TYPE_DEFINITION ::=
      --    (ENUMERATION_LITERAL_SPECIFICATION
      --      {, ENUMERATION_LITERAL_SPECIFICATION})

      --  Note: the Literals field in the node described below is null for
      --  the case of the standard types CHARACTER and WIDE_CHARACTER, for
      --  which special processing handles these types as special cases.

      --  N_Enumeration_Type_Definition
      --  Sloc points to left parenthesis
      --  Literals (List1) (Empty for CHARACTER or WIDE_CHARACTER)

      ----------------------------------------------
      -- 3.5.1  Enumeration Literal Specification --
      ----------------------------------------------

      --  ENUMERATION_LITERAL_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER | DEFINING_CHARACTER_LITERAL

      ---------------------------------------
      -- 3.5.1  Defining Character Literal --
      ---------------------------------------

      --  DEFINING_CHARACTER_LITERAL ::= CHARACTER_LITERAL

      --  A defining character literal is an entity, which has additional
      --  fields depending on the setting of the Ekind field. These
      --  additional fields are defined (and access subprograms declared)
      --  in package Einfo.

      --  Note: N_Defining_Character_Literal is an extended node whose fields
      --  are deliberate layed out to match the layout of fields in an ordinary
      --  N_Character_Literal node allowing for easy alteration of a character
      --  literal node into a defining character literal node. For details, see
      --  Sinfo.CN.Change_Character_Literal_To_Defining_Character_Literal.

      --  N_Defining_Character_Literal
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Next_Entity (Node2-Sem)
      --  Scope (Node3-Sem)
      --  Etype (Node5-Sem)

      ------------------------------------
      -- 3.5.4  Integer Type Definition --
      ------------------------------------

      --  Note: there is an error in this rule in the latest version of the
      --  grammar, so we have retained the old rule pending clarification.

      --  INTEGER_TYPE_DEFINITION ::=
      --    SIGNED_INTEGER_TYPE_DEFINITION
      --    MODULAR_TYPE_DEFINITION

      -------------------------------------------
      -- 3.5.4  Signed Integer Type Definition --
      -------------------------------------------

      --  SIGNED_INTEGER_TYPE_DEFINITION ::=
      --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

      --  Note: the Low_Bound and High_Bound fields are set to Empty for
      --  integer types defined in package Standard.

      --  N_Signed_Integer_Type_Definition
      --  Sloc points to RANGE
      --  Low_Bound (Node1)
      --  High_Bound (Node2)

      -----------------------------------------
      -- 3.5.4  Unsigned Range Specification --
      -----------------------------------------

      --  MODULAR_TYPE_DEFINITION ::= mod static_EXPRESSION

      --  N_Modular_Type_Definition
      --  Sloc points to MOD
      --  Expression (Node3)

      ---------------------------------
      -- 3.5.6  Real Type Definition --
      ---------------------------------

      --  REAL_TYPE_DEFINITION ::=
      --    FLOATING_POINT_DEFINITION | FIXED_POINT_DEFINITION

      --------------------------------------
      -- 3.5.7  Floating Point Definition --
      --------------------------------------

      --  FLOATING_POINT_DEFINITION ::=
      --    digits static_SIMPLE_EXPRESSION [REAL_RANGE_SPECIFICATION]

      --  Note: The Digits_Expression and Real_Range_Specifications fields
      --  are set to Empty for floating-point types declared in Standard.

      --  N_Floating_Point_Definition
      --  Sloc points to DIGITS
      --  Digits_Expression (Node2)
      --  Real_Range_Specification (Node4) (set to Empty if not present)

      -------------------------------------
      -- 3.5.7  Real Range Specification --
      -------------------------------------

      --  REAL_RANGE_SPECIFICATION ::=
      --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

      --  N_Real_Range_Specification
      --  Sloc points to RANGE
      --  Low_Bound (Node1)
      --  High_Bound (Node2)

      -----------------------------------
      -- 3.5.9  Fixed Point Definition --
      -----------------------------------

      --  FIXED_POINT_DEFINITION ::=
      --    ORDINARY_FIXED_POINT_DEFINITION | DECIMAL_FIXED_POINT_DEFINITION

      --------------------------------------------
      -- 3.5.9  Ordinary Fixed Point Definition --
      --------------------------------------------

      --  ORDINARY_FIXED_POINT_DEFINITION ::=
      --    delta static_EXPRESSION REAL_RANGE_SPECIFICATION

      --  Note: In Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION

      --  Note: the Delta_Expression and Real_Range_Specification fields
      --  are set to Empty for fixed point types declared in Standard.

      --  N_Ordinary_Fixed_Point_Definition
      --  Sloc points to DELTA
      --  Delta_Expression (Node3)
      --  Real_Range_Specification (Node4)

      -------------------------------------------
      -- 3.5.9  Decimal Fixed Point Definition --
      -------------------------------------------

      --  DECIMAL_FIXED_POINT_DEFINITION ::=
      --    delta static_EXPRESSION
      --      digits static_EXPRESSION [REAL_RANGE_SPECIFICATION]

      --  Note: decimal types are not permitted in Ada 83 mode

      --  N_Decimal_Fixed_Point_Definition
      --  Sloc points to DELTA
      --  Delta_Expression (Node3)
      --  Digits_Expression (Node2)
      --  Real_Range_Specification (Node4) (set to Empty if not present)

      ------------------------------
      -- 3.5.9  Digits Constraint --
      ------------------------------

      --  DIGITS_CONSTRAINT ::=
      --    digits static_EXPRESSION [RANGE_CONSTRAINT]

      --  Note: in Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION
      --  Note: in Ada 95, reduced accuracy subtypes are obsolescent

      --  N_Digits_Constraint
      --  Sloc points to DIGITS
      --  Digits_Expression (Node2)
      --  Range_Constraint (Node4) (set to Empty if not present)

      --------------------------------
      -- 3.6  Array Type Definition --
      --------------------------------

      --  ARRAY_TYPE_DEFINITION ::=
      --    UNCONSTRAINED_ARRAY_DEFINITION | CONSTRAINED_ARRAY_DEFINITION

      -----------------------------------------
      -- 3.6  Unconstrained Array Definition --
      -----------------------------------------

      --  UNCONSTRAINED_ARRAY_DEFINITION ::=
      --    array (INDEX_SUBTYPE_DEFINITION {, INDEX_SUBTYPE_DEFINITION}) of
      --      COMPONENT_DEFINITION

      --  Note: dimensionality of array is indicated by number of entries in
      --  the Subtype_Marks list, which has one entry for each dimension.

      --  N_Unconstrained_Array_Definition
      --  Sloc points to ARRAY
      --  Subtype_Marks (List2)
      --  Aliased_Present (Flag4) from component definition
      --  Subtype_Indication (Node5) from component definition

      -----------------------------------
      -- 3.6  Index Subtype Definition --
      -----------------------------------

      --  INDEX_SUBTYPE_DEFINITION ::= SUBTYPE_MARK range <>

      --  There is no explicit node in the tree for an index subtype
      --  definition since the N_Unconstrained_Array_Definition node
      --  incorporates the type marks which appear in this context.

      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    array (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION})
      --        of COMPONENT_DEFINITION

      --  Note: dimensionality of array is indicated by number of entries
      --  in the Discrete_Subtype_Definitions list, which has one entry
      --  for each dimension.

      --  N_Constrained_Array_Definition
      --  Sloc points to ARRAY
      --  Discrete_Subtype_Definitions (List2)
      --  Aliased_Present (Flag4) from component definition
      --  Subtype_Indication (Node5) from component definition

      --------------------------------------
      -- 3.6  Discrete Subtype Definition --
      --------------------------------------

      --  DISCRETE_SUBTYPE_DEFINITION ::=
      --    discrete_SUBTYPE_INDICATION | RANGE

      -------------------------------
      -- 3.6  Component Definition --
      -------------------------------

      --  COMPONENT_DEFINITION ::= [aliased] SUBTYPE_INDICATION

      --  There is no explicit node in the tree for a component definition.
      --  Instead the subtype indication appears directly, and the ALIASED
      --  indication (Aliased_Present flag) is in the parent node.

      --  Note: although the syntax does not permit a component definition to
      --  be an anonymous array (and the parser will diagnose such an attempt
      --  with an appropriate message), it is possible for anonymous arrays
      --  to appear as component definitions. The semantics and back end handle
      --  this case properly, and the expander in fact generates such cases.

      -----------------------------
      -- 3.6.1  Index Constraint --
      -----------------------------

      --  INDEX_CONSTRAINT ::= (DISCRETE_RANGE {, DISCRETE_RANGE})

      --  It is not in general possible to distinguish between discriminant
      --  constraints and index constraints at parse time, since a simple
      --  name could be either the subtype mark of a discrete range, or an
      --  expression in a discriminant association with no name. Either
      --  entry appears simply as the name, and the semantic parse must
      --  distinguish between the two cases. Thus we use a common tree
      --  node format for both of these constraint types.

      --  See Discriminant_Constraint for format of node

      ---------------------------
      -- 3.6.1  Discrete Range --
      ---------------------------

      --  DISCRETE_RANGE ::= discrete_SUBTYPE_INDICATION | RANGE

      ----------------------------
      -- 3.7  Discriminant Part --
      ----------------------------

      --  DISCRIMINANT_PART ::=
      --    UNKNOWN_DISCRIMINANT_PART | KNOWN_DISCRIMINANT_PART

      ------------------------------------
      -- 3.7  Unknown Discriminant Part --
      ------------------------------------

      --  UNKNOWN_DISCRIMINANT_PART ::= (<>)

      --  Note: unknown discriminant parts are not permitted in Ada 83 mode

      --  There is no explicit node in the tree for an unknown discriminant
      --  part. Instead the Unknown_Discriminants_Present flag is set in the
      --  parent node.

      ----------------------------------
      -- 3.7  Known Discriminant Part --
      ----------------------------------

      --  KNOWN_DISCRIMINANT_PART ::=
      --    (DISCRIMINANT_SPECIFICATION {; DISCRIMINANT_SPECIFICATION})

      -------------------------------------
      -- 3.7  Discriminant Specification --
      -------------------------------------

      --  DISCRIMINANT_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : SUBTYPE_MARK
      --      [:= DEFAULT_EXPRESSION]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive specifications were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single specifications, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Discriminant_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Discriminant_Type (Node5) subtype mark or
      --    access parameter definition
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -----------------------------
      -- 3.7  Default Expression --
      -----------------------------

      --  DEFAULT_EXPRESSION ::= EXPRESSION

      ------------------------------------
      -- 3.7.1  Discriminant Constraint --
      ------------------------------------

      --  DISCRIMINANT_CONSTRAINT ::=
      --    (DISCRIMINANT_ASSOCIATION {, DISCRIMINANT_ASSOCIATION})

      --  It is not in general possible to distinguish between discriminant
      --  constraints and index constraints at parse time, since a simple
      --  name could be either the subtype mark of a discrete range, or an
      --  expression in a discriminant association with no name. Either
      --  entry appears simply as the name, and the semantic parse must
      --  distinguish between the two cases. Thus we use a common tree
      --  node format for both of these constraint types.

      --  N_Index_Or_Discriminant_Constraint
      --  Sloc points to left paren
      --  Constraints (List1) points to list of discrete ranges or
      --    discriminant associations

      -------------------------------------
      -- 3.7.1  Discriminant Association --
      -------------------------------------

      --  DISCRIMINANT_ASSOCIATION ::=
      --    [discriminant_SELECTOR_NAME
      --      {| discriminant_SELECTOR_NAME} =>] EXPRESSION

      --  Note: a discriminant association that has no selector name list
      --  appears directly as an expression in the tree.

      --  N_Discriminant_Association
      --  Sloc points to first token of discriminant association
      --  Selector_Names (List1) (always non-empty, since if no selector
      --   names are present, this node is not used, see comment above)
      --  Expression (Node3)

      ---------------------------------
      -- 3.8  Record Type Definition --
      ---------------------------------

      --  RECORD_TYPE_DEFINITION ::=
      --    [[abstract] tagged] [limited] RECORD_DEFINITION

      --  Note: ABSTRACT, TAGGED, LIMITED are not permitted in Ada 83 mode

      --  There is no explicit node in the tree for a record type definition.
      --  Instead the flags for Tagged_Present and Limited_Present appear in
      --  the N_Record_Definition node for a record definition appearing in
      --  the context of a record type definition.

      ----------------------------
      -- 3.8  Record Definition --
      ----------------------------

      --  RECORD_DEFINITION ::=
      --    record
      --      COMPONENT_LIST
      --    end record
      --  | null record

      --  Note: the Abstract_Present, Tagged_Present and Limited_Present
      --  flags appear only for a record definition appearing in a record
      --  type definition.

      --  Note: the NULL RECORD case is not permitted in Ada 83

      --  N_Record_Definition
      --  Sloc points to RECORD or NULL
      --  End_Label (Node4) (set to Empty if internally generated record)
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag15)
      --  Limited_Present (Flag17)
      --  Component_List (Node1) empty in null record case
      --  Null_Present (Flag13) set in null record case

      -------------------------
      -- 3.8  Component List --
      -------------------------

      --  COMPONENT_LIST ::=
      --    COMPONENT_ITEM {COMPONENT_ITEM}
      --  | {COMPONENT_ITEM} VARIANT_PART
      --  | null;

      --  N_Component_List
      --  Sloc points to first token of component list
      --  Component_Items (List3)
      --  Variant_Part (Node4) (set to Empty if no variant part)
      --  Null_Present (Flag13)

      -------------------------
      -- 3.8  Component Item --
      -------------------------

      --  COMPONENT_ITEM ::= COMPONENT_DECLARATION | REPRESENTATION_CLAUSE

      --  Note: A component item can also be a pragma, and in the tree
      --  that is obtained after semantic processing, a component item
      --  can be an N_Null node resulting from a non-recognized pragma.

      --------------------------------
      -- 3.8  Component Declaration --
      --------------------------------

      --  COMPONENT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : COMPONENT_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      --  Note: although the syntax does not permit a component definition to
      --  be an anonymous array (and the parser will diagnose such an attempt
      --  with an appropriate message), it is possible for anonymous arrays
      --  to appear as component definitions. The semantics and back end handle
      --  this case properly, and the expander in fact generates such cases.

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with the
      --  same component definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Component_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Aliased_Present (Flag4) from component definition
      --  Subtype_Indication (Node5) from component definition
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -------------------------
      -- 3.8.1  Variant Part --
      -------------------------

      --  VARIANT_PART ::=
      --    case discriminant_DIRECT_NAME is
      --      VARIANT
      --      {VARIANT}
      --    end case;

      --  Note: the variants list can contain pragmas as well as variants.
      --  In a properly formed program there is at least one variant.

      --  N_Variant_Part
      --  Sloc points to CASE
      --  Name (Node2)
      --  Variants (List1)

      --------------------
      -- 3.8.1  Variant --
      --------------------

      --  VARIANT ::=
      --    when DISCRETE_CHOICE_LIST =>
      --      COMPONENT_LIST

      --  N_Variant
      --  Sloc points to WHEN
      --  Discrete_Choices (List4)
      --  Component_List (Node1)
      --  Enclosing_Variant (Node2-Sem)
      --  Present_Expr (Uint3-Sem)
      --  Dcheck_Function (Node5-Sem)

      ---------------------------------
      -- 3.8.1  Discrete Choice List --
      ---------------------------------

      --  DISCRETE_CHOICE_LIST ::= DISCRETE_CHOICE {| DISCRETE_CHOICE}

      ----------------------------
      -- 3.8.1  Discrete Choice --
      ----------------------------

      --  DISCRETE_CHOICE ::= EXPRESSION | DISCRETE_RANGE | others

      --  Note: in Ada 83 mode, the expression must be a simple expression

      --  The only choice that appears explicitly is the OTHERS choice, as
      --  defined here. Other cases of discrete choice (expression and
      --  discrete range) appear directly. This production is also used
      --  for the OTHERS possibility of an exception choice.

      --  Note: in accordance with the syntax, the parser does not check that
      --  OTHERS appears at the end on its own in a choice list context. This
      --  is a semantic check.

      --  N_Others_Choice
      --  Sloc points to OTHERS
      --  Others_Discrete_Choices (List1-Sem)
      --  All_Others (Flag11-Sem)

      ----------------------------------
      -- 3.9.1  Record Extension Part --
      ----------------------------------

      --  RECORD_EXTENSION_PART ::= with RECORD_DEFINITION

      --  Note: record extension parts are not permitted in Ada 83 mode

      ----------------------------------
      -- 3.10  Access Type Definition --
      ----------------------------------

      --  ACCESS_TYPE_DEFINITION ::=
      --    ACCESS_TO_OBJECT_DEFINITION
      --  | ACCESS_TO_SUBPROGRAM_DEFINITION

      ---------------------------------------
      -- 3.10  Access To Object Definition --
      ---------------------------------------

      --  ACCESS_TO_OBJECT_DEFINITION ::=
      --    access [GENERAL_ACCESS_MODIFIER] SUBTYPE_INDICATION

      --  N_Access_To_Object_Definition
      --  Sloc points to ACCESS
      --  All_Present (Flag15)
      --  Subtype_Indication (Node5)
      --  Constant_Present (Flag17)

      -----------------------------------
      -- 3.10  General Access Modifier --
      -----------------------------------

      --  GENERAL_ACCESS_MODIFIER ::= all | constant

      --  Note: general access modifiers are not permitted in Ada 83 mode

      --  There is no explicit node in the tree for general access modifier.
      --  Instead the All_Present or Constant_Present flags are set in the
      --  parent node.

      -------------------------------------------
      -- 3.10  Access To Subprogram Definition --
      -------------------------------------------

      --  ACCESS_TO_SUBPROGRAM_DEFINITION
      --    access [protected] procedure PARAMETER_PROFILE
      --  | access [protected] function PARAMETER_AND_RESULT_PROFILE

      --  Note: access to subprograms are not permitted in Ada 83 mode

      --  N_Access_Function_Definition
      --  Sloc points to ACCESS
      --  Protected_Present (Flag15)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)
      --  Subtype_Mark (Node4) result subtype

      --  N_Access_Procedure_Definition
      --  Sloc points to ACCESS
      --  Protected_Present (Flag15)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)

      -----------------------------
      -- 3.10  Access Definition --
      -----------------------------

      --  ACCESS_DEFINITION ::= access SUBTYPE_MARK

      --  N_Access_Definition
      --  Sloc points to ACCESS
      --  Subtype_Mark (Node4)

      -----------------------------------------
      -- 3.10.1  Incomplete Type Declaration --
      -----------------------------------------

      --  INCOMPLETE_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART];

      --  N_Incomplete_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part, or if the discriminant part is an
      --   unknown discriminant part)
      --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant

      ----------------------------
      -- 3.11  Declarative Part --
      ----------------------------

      --  DECLARATIVE_PART ::= {DECLARATIVE_ITEM}

      --  Note: although the parser enforces the syntactic requirement that
      --  a declarative part can contain only declarations, the semantic
      --  processing may add statements to the list of actions in a
      --  declarative part, so the code generator should be prepared
      --  to accept a statement in this position.

      ----------------------------
      -- 3.11  Declarative Item --
      ----------------------------

      --  DECLARATIVE_ITEM ::= BASIC_DECLARATIVE_ITEM | BODY

      ----------------------------------
      -- 3.11  Basic Declarative Item --
      ----------------------------------

      --  BASIC_DECLARATIVE_ITEM ::=
      --    BASIC_DECLARATION | REPRESENTATION_CLAUSE | USE_CLAUSE

      ----------------
      -- 3.11  Body --
      ----------------

      --  BODY ::= PROPER_BODY | BODY_STUB

      -----------------------
      -- 3.11  Proper Body --
      -----------------------

      --  PROPER_BODY ::=
      --    SUBPROGRAM_BODY | PACKAGE_BODY | TASK_BODY | PROTECTED_BODY

      ---------------
      -- 4.1  Name --
      ---------------

      --  NAME ::=
      --    DIRECT_NAME        | EXPLICIT_DEREFERENCE
      --  | INDEXED_COMPONENT  | SLICE
      --  | SELECTED_COMPONENT | ATTRIBUTE_REFERENCE
      --  | TYPE_CONVERSION    | FUNCTION_CALL
      --  | CHARACTER_LITERAL

      ----------------------
      -- 4.1  Direct Name --
      ----------------------

      --  DIRECT_NAME ::= IDENTIFIER | OPERATOR_SYMBOL

      -----------------
      -- 4.1  Prefix --
      -----------------

      --  PREFIX ::= NAME | IMPLICIT_DEREFERENCE

      -------------------------------
      -- 4.1  Explicit Dereference --
      -------------------------------

      --  EXPLICIT_DEREFERENCE ::= NAME . all

      --  N_Explicit_Dereference
      --  Sloc points to ALL
      --  Prefix (Node3)
      --  Do_Access_Check (Flag11-Sem)
      --  plus fields for expression

      -------------------------------
      -- 4.1  Implicit Dereference --
      -------------------------------

      --  IMPLICIT_DEREFERENCE ::= NAME

      ------------------------------
      -- 4.1.1  Indexed Component --
      ------------------------------

      --  INDEXED_COMPONENT ::= PREFIX (EXPRESSION {, EXPRESSION})

      --  Note: the parser may generate this node in some situations where it
      --  should be a function call. The semantic  pass must correct this
      --  misidentification (which is inevitable at the parser level).

      --  N_Indexed_Component
      --  Sloc contains a copy of the Sloc value of the Prefix
      --  Prefix (Node3)
      --  Expressions (List1)
      --  Do_Access_Check (Flag11-Sem)
      --  plus fields for expression

      --  Note: if any of the subscripts requires a range check, then the
      --  Do_Range_Check flag is set on the corresponding expression, with
      --  the index type being determined from the type of the Prefix, which
      --  references the array being indexed.

      --  Note: in a fully analyzed and expanded indexed component node, and
      --  hence in any such node that gigi sees, if the prefix is an access
      --  type, then an explicit dereference operation has been inserted.

      ------------------
      -- 4.1.2  Slice --
      ------------------

      --  SLICE ::= PREFIX (DISCRETE_RANGE)

      --  Note: an implicit subtype is created to describe the resulting
      --  type, so that the bounds of this type are the bounds of the slice.

      --  N_Slice
      --  Sloc points to first token of prefix
      --  Prefix (Node3)
      --  Discrete_Range (Node4)
      --  Do_Access_Check (Flag11-Sem)
      --  plus fields for expression

      -------------------------------
      -- 4.1.3  Selected Component --
      -------------------------------

      --  SELECTED_COMPONENT ::= PREFIX . SELECTOR_NAME

      --  Note: selected components that are semantically expanded names get
      --  changed during semantic processing into the separate N_Expanded_Name
      --  node. See description of this node in the section on semantic nodes.

      --  N_Selected_Component
      --  Sloc points to period
      --  Prefix (Node3)
      --  Selector_Name (Node2)
      --  Associated_Node (Node4-Sem)
      --  Do_Access_Check (Flag11-Sem)
      --  Do_Discriminant_Check (Flag13-Sem)
      --  plus fields for expression

      --------------------------
      -- 4.1.3  Selector Name --
      --------------------------

      --  SELECTOR_NAME ::= IDENTIFIER | CHARACTER_LITERAL | OPERATOR_SYMBOL

      --------------------------------
      -- 4.1.4  Attribute Reference --
      --------------------------------

      --  ATTRIBUTE_REFERENCE ::= PREFIX ' ATTRIBUTE_DESIGNATOR

      --  Note: the syntax is quite ambiguous at this point. Consider:

      --    A'Length (X)  X is part of the attribute designator
      --    A'Pos (X)     X is an explicit actual parameter of function A'Pos
      --    A'Class (X)   X is the expression of a type conversion

      --  It would be possible for the parser to distinguish these cases
      --  by looking at the attribute identifier. However, that would mean
      --  more work in introducing new implementation defined attributes,
      --  and also it would mean that special processing for attributes
      --  would be scattered around, instead of being centralized in the
      --  semantic routine that handles an N_Attribute_Reference node.
      --  Consequently, the parser in all the above cases stores the
      --  expression (X in these examples) as a single element list in
      --  in the Expressions field of the N_Attribute_Reference node.

      --  Similarly, for attributes like Max which take two arguments,
      --  we store the two arguments as a two element list in the
      --  Expressions field. Of course it is clear at parse time that
      --  this case is really a function call with an attribute as the
      --  prefix, but it turns out to be convenient to handle the two
      --  argument case in a similar manner to the one argument case,
      --  and indeed in general the parser will accept any number of
      --  expressions in this position and store them as a list in the
      --  attribute reference node. This allows for future addition of
      --  attributes that take more than two arguments.

      --  Note: named associates are not permitted in function calls where
      --  the function is an attribute (see RM 6.4(3)) so it is legitimate
      --  to skip the normal subprogram argument processing.

      --  Note: for the attributes whose designators are technically keywords,
      --  i.e. digits, access, delta, range, the Attribute_Name field contains
      --  the corresponding name, even though no identifier is involved.

      --  The flag OK_For_Stream is used in generated code to indicate that
      --  a stream attribute is permissible for a limited type, and results
      --  in the use of the stream attribute for the underlying full type,
      --  or in the case of a protected type, the components (including any
      --  disriminants) are merely streamed in order.

      --  See Exp_Attr for a complete description of which attributes are
      --  passed onto Gigi, and which are handled entirely by the front end.

      --  Gigi restriction: For the Pos attribute, the prefix cannot be
      --  a non-standard enumeration type or a nonzero/zero semantics
      --  boolean type, so the value is simply the stored representation.

      --  N_Attribute_Reference
      --  Sloc points to apostrophe
      --  Prefix (Node3)
      --  Attribute_Name (Name2) identifier name from attribute designator
      --  Expressions (List1) (set to No_List if no associated expressions)
      --  Entity (Node4-Sem) used if the attribute yields a type
      --  Associated_Node (Node4-Sem)
      --  Do_Access_Check (Flag11-Sem)
      --  Do_Overflow_Check (Flag17-Sem)
      --  Redundant_Use (Flag13-Sem)
      --  OK_For_Stream (Flag4-Sem)
      --  plus fields for expression

      ---------------------------------
      -- 4.1.4  Attribute Designator --
      ---------------------------------

      --  ATTRIBUTE_DESIGNATOR ::=
      --    IDENTIFIER [(static_EXPRESSION)]
      --  | access | delta | digits

      --  There is no explicit node in the tree for an attribute designator.
      --  Instead the Attribute_Name and Expressions fields of the parent
      --  node (N_Attribute_Reference node) hold the information.

      --  Note: if ACCESS, DELTA or DIGITS appears in an attribute
      --  designator, then they are treated as identifiers internally
      --  rather than the keywords of the same name.

      --------------------------------------
      -- 4.1.4  Range Attribute Reference --
      --------------------------------------

      --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

      --  A range attribute reference is represented in the tree using the
      --  normal N_Attribute_Reference node.

      ---------------------------------------
      -- 4.1.4  Range Attribute Designator --
      ---------------------------------------

      --  RANGE_ATTRIBUTE_DESIGNATOR ::= Range [(static_EXPRESSION)]

      --  A range attribute designator is represented in the tree using the
      --  normal N_Attribute_Reference node.

      --------------------
      -- 4.3  Aggregate --
      --------------------

      --  AGGREGATE ::=
      --    RECORD_AGGREGATE | EXTENSION_AGGREGATE | ARRAY_AGGREGATE

      -----------------------------
      -- 4.3.1  Record Aggregate --
      -----------------------------

      --  RECORD_AGGREGATE ::= (RECORD_COMPONENT_ASSOCIATION_LIST)

      --  N_Aggregate
      --  Sloc points to left parenthesis
      --  Expressions (List1) (set to No_List if none or null record case)
      --  Component_Associations (List2) (set to No_List if none)
      --  Null_Record_Present (Flag17)
      --  Aggregate_Bounds (Node3-Sem)
      --  Associated_Node (Node4-Sem)
      --  Static_Processing_OK (Flag4-Sem)
      --  Compile_Time_Known_Aggregate (Flag18-Sem)
      --  Expansion_Delayed (Flag11-Sem)
      --  plus fields for expression

      --  Note: this structure is used for both record and array aggregates
      --  since the two cases are not separable by the parser. The parser
      --  makes no attempt to enforce consistency here, so it is up to the
      --  semantic phase to make sure that the aggregate is consistent (i.e.
      --  that it is not a "half-and-half" case that mixes record and array
      --  syntax. In particular, for a record aggregate, the expressions
      --  field will be set if there are positional associations.

      --  Note: gigi/gcc can handle array aggregates correctly providing that
      --  they are entirely positional, and the array subtype involved has a
      --  known at compile time length and is not bit packed, or a convention
      --  Fortran array with more than one dimension. If these conditions
      --  are not met, then the front end must translate the aggregate into
      --  an appropriate set  of assignments into a temporary.

      --  Note: for the record aggregate case, gigi/gcc can handle all cases
      --  of record aggregates, including those for packed, and rep-claused
      --  records, and also variant records, providing that there are no
      --  variable length fields whose size is not known at runtime, and
      --  providing that the aggregate is presented in fully named form.

      ----------------------------------------------
      -- 4.3.1  Record Component Association List --
      ----------------------------------------------

      --  RECORD_COMPONENT_ASSOCIATION_LIST ::=
      --     RECORD_COMPONENT_ASSOCIATION {, RECORD_COMPONENT_ASSOCIATION}
      --   | null record

      --  There is no explicit node in the tree for a record component
      --  association list. Instead the Null_Record_Present flag is set in
      --  the parent node for the NULL RECORD case.

      ------------------------------------------------------
      -- 4.3.1  Record Component Association (also 4.3.3) --
      ------------------------------------------------------

      --  RECORD_COMPONENT_ASSOCIATION ::=
      --    [COMPONENT_CHOICE_LIST =>] EXPRESSION

      --  N_Component_Association
      --  Sloc points to first selector name
      --  Choices (List1)
      --  Loop_Actions (List2-Sem)
      --  Expression (Node3)

      --  Note: this structure is used for both record component associations
      --  and array component associations, since the two cases aren't always
      --  separable by the parser. The choices list may represent either a
      --  list of selector names in the record aggregate case, or a list of
      --  discrete choices in the array aggregate case or an N_Others_Choice
      --  node (which appears as a singleton list).

      ------------------------------------
      --  4.3.1  Commponent Choice List --
      ------------------------------------

      --  COMPONENT_CHOICE_LIST ::=
      --    component_SELECTOR_NAME {| component_SELECTOR_NAME}
      --  | others

      --  The entries of a component choice list appear in the Choices list
      --  of the associated N_Component_Association, as either selector
      --  names, or as an N_Others_Choice node.

      --------------------------------
      -- 4.3.2  Extension Aggregate --
      --------------------------------

      --  EXTENSION_AGGREGATE ::=
      --    (ANCESTOR_PART with RECORD_COMPONENT_ASSOCIATION_LIST)

      --  Note: extension aggregates are not permitted in Ada 83 mode

      --  N_Extension_Aggregate
      --  Sloc points to left parenthesis
      --  Ancestor_Part (Node3)
      --  Associated_Node (Node4-Sem)
      --  Expressions (List1) (set to No_List if none or null record case)
      --  Component_Associations (List2) (set to No_List if none)
      --  Null_Record_Present (Flag17)
      --  Expansion_Delayed (Flag11-Sem)
      --  plus fields for expression

      --------------------------
      -- 4.3.2  Ancestor Part --
      --------------------------

      --  ANCESTOR_PART ::= EXPRESSION | SUBTYPE_MARK

      ----------------------------
      -- 4.3.3  Array Aggregate --
      ----------------------------

      --  ARRAY_AGGREGATE ::=
      --    POSITIONAL_ARRAY_AGGREGATE | NAMED_ARRAY_AGGREGATE

      ---------------------------------------
      -- 4.3.3  Positional Array Aggregate --
      ---------------------------------------

      --  POSITIONAL_ARRAY_AGGREGATE ::=
      --    (EXPRESSION, EXPRESSION {, EXPRESSION})
      --  | (EXPRESSION {, EXPRESSION}, others => EXPRESSION)

      --  See Record_Aggregate (4.3.1) for node structure

      ----------------------------------
      -- 4.3.3  Named Array Aggregate --
      ----------------------------------

      --  NAMED_ARRAY_AGGREGATE ::=
      --  | (ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION})

      --  See Record_Aggregate (4.3.1) for node structure

      ----------------------------------------
      -- 4.3.3  Array Component Association --
      ----------------------------------------

      --  ARRAY_COMPONENT_ASSOCIATION ::=
      --    DISCRETE_CHOICE_LIST => EXPRESSION

      --  See Record_Component_Association (4.3.1) for node structure

      --------------------------------------------------
      -- 4.4  Expression/Relation/Term/Factor/Primary --
      --------------------------------------------------

      --  EXPRESSION ::=
      --    RELATION {and RELATION} | RELATION {and then RELATION}
      --  | RELATION {or RELATION}  | RELATION {or else RELATION}
      --  | RELATION {xor RELATION}

      --  RELATION ::=
      --    SIMPLE_EXPRESSION [RELATIONAL_OPERATOR SIMPLE_EXPRESSION]
      --  | SIMPLE_EXPRESSION [not] in RANGE
      --  | SIMPLE_EXPRESSION [not] in SUBTYPE_MARK

      --  SIMPLE_EXPRESSION ::=
      --    [UNARY_ADDING_OPERATOR] TERM {BINARY_ADDING_OPERATOR TERM}

      --  TERM ::= FACTOR {MULTIPLYING_OPERATOR FACTOR}

      --  FACTOR ::= PRIMARY [** PRIMARY] | abs PRIMARY | not PRIMARY

      --  No nodes are generated for any of these constructs. Instead, the
      --  node for the operator appears directly. When we refer to an
      --  expression in this description, we mean any of the possible
      --  consistuent components of an expression (e.g. identifier is
      --  an example of an expression).

      ------------------
      -- 4.4  Primary --
      ------------------

      --  PRIMARY ::=
      --    NUMERIC_LITERAL  | null
      --  | STRING_LITERAL   | AGGREGATE
      --  | NAME             | QUALIFIED_EXPRESSION
      --  | ALLOCATOR        | (EXPRESSION)

      --  Usually there is no explicit node in the tree for primary. Instead
      --  the constituent (e.g. AGGREGATE) appears directly. There are two
      --  exceptions. First, there is an explicit node for a null primary.

      --  N_Null
      --  Sloc points to NULL
      --  plus fields for expression

      --  Second, the case of (EXPRESSION) is handled specially. Ada requires
      --  that the parser keep track of which subexpressions are enclosed
      --  in parentheses, and how many levels of parentheses are used. This
      --  information is required for optimization purposes, and also for
      --  some semantic checks (e.g. (((1))) in a procedure spec does not
      --  conform with ((((1)))) in the body).

      --  The parentheses are recorded by keeping a Paren_Count field in every
      --  subexpression node (it is actually present in all nodes, but only
      --  used in subexpression nodes). This count records the number of
      --  levels of parentheses. If the number of levels in the source exceeds
      --  the maximum accomodated by this count, then the count is simply left
      --  at the maximum value. This means that there are some pathalogical
      --  cases of failure to detect conformance failures (e.g. an expression
      --  with 500 levels of parens will conform with one with 501 levels),
      --  but we do not need to lose sleep over this.

      --  Historical note: in versions of GNAT prior to 1.75, there was a node
      --  type N_Parenthesized_Expression used to accurately record unlimited
      --  numbers of levels of parentheses. However, it turned out to be a
      --  real nuisance to have to take into account the possible presence of
      --  this node during semantic analysis, since basically parentheses have
      --  zero relevance to semantic analysis.

      --  Note: the level of parentheses always present in things like
      --  aggregates does not count, only the parentheses in the primary
      --  (EXPRESSION) affect the setting of the Paren_Count field.

      --  2nd Note: the contents of the Expression field must be ignored (i.e.
      --  treated as though it were Empty) if No_Initialization is set True.

      --------------------------------------
      -- 4.5  Short Circuit Control Forms --
      --------------------------------------

      --  EXPRESSION ::=
      --    RELATION {and then RELATION} | RELATION {or else RELATION}

      --  Gigi restriction: For both these control forms, the operand and
      --  result types are always Standard.Boolean. The expander inserts the
      --  required conversion operations where needed to ensure this is the
      --  case.

      --  N_And_Then
      --  Sloc points to AND of AND THEN
      --  Left_Opnd (Node2)
      --  Right_Opnd (Node3)
      --  Actions (List1-Sem)
      --  plus fields for expression

      --  N_Or_Else
      --  Sloc points to OR of OR ELSE
      --  Left_Opnd (Node2)
      --  Right_Opnd (Node3)
      --  Actions (List1-Sem)
      --  plus fields for expression

      --  Note: The Actions field is used to hold actions associated with
      --  the right hand operand. These have to be treated specially since
      --  they are not unconditionally executed. See Insert_Actions for a
      --  more detailed description of how these actions are handled.

      ---------------------------
      -- 4.5  Membership Tests --
      ---------------------------

      --  RELATION ::=
      --    SIMPLE_EXPRESSION [not] in RANGE
      --  | SIMPLE_EXPRESSION [not] in SUBTYPE_MARK

      --  Note: although the grammar above allows only a range or a
      --  subtype mark, the parser in fact will accept any simple
      --  expression in place of a subtype mark. This means that the
      --  semantic analyzer must be prepared to deal with, and diagnose
      --  a simple expression other than a name for the right operand.
      --  This simplifies error recovery in the parser.

      --  N_In
      --  Sloc points to IN
      --  Left_Opnd (Node2)
      --  Right_Opnd (Node3)
      --  plus fields for expression

      --  N_Not_In
      --  Sloc points to NOT of NOT IN
      --  Left_Opnd (Node2)
      --  Right_Opnd (Node3)
      --  plus fields for expression

      --------------------
      -- 4.5  Operators --
      --------------------

      --  LOGICAL_OPERATOR             ::=  and | or  | xor

      --  RELATIONAL_OPERATOR          ::=  =   | /=  | <   | <= | > | >=

      --  BINARY_ADDING_OPERATOR       ::=  +   |  -  | &

      --  UNARY_ADDING_OPERATOR        ::=  +   |  -

      --  MULTIPLYING_OPERATOR         ::=  *   |  /  | mod | rem

      --  HIGHEST_PRECEDENCE_OPERATOR  ::=  **  | abs | not

      --  Sprint syntax if Treat_Fixed_As_Integer is set:

      --     x #* y
      --     x #/ y
      --     x #mod y
      --     x #rem y

      --  Gigi restriction: For * / mod rem with fixed-point operands, Gigi
      --  will only be given nodes with the Treat_Fixed_As_Integer flag set.
      --  All handling of smalls for multiplication and division is handled
      --  by the front end (mod and rem result only from expansion). Gigi
      --  thus never needs to worry about small values (for other operators
      --  operating on fixed-point, e.g. addition, the small value does not
      --  have any semantic effect anyway, these are always integer operations.

      --  Gigi restriction: For all operators taking Boolean operands, the
      --  type is always Standard.Boolean. The expander inserts the required
      --  conversion operations where needed to ensure this is the case.

      --  N_Op_And
      --  Sloc points to AND
      --  Do_Length_Check (Flag4-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Or
      --  Sloc points to OR
      --  Do_Length_Check (Flag4-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Xor
      --  Sloc points to XOR
      --  Do_Length_Check (Flag4-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Eq
      --  Sloc points to =
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Ne
      --  Sloc points to /=
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Lt
      --  Sloc points to <
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Le
      --  Sloc points to <=
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Gt
      --  Sloc points to >
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Ge
      --  Sloc points to >=
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Add
      --  Sloc points to + (binary)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Subtract
      --  Sloc points to - (binary)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Concat
      --  Sloc points to &
      --  Is_Component_Left_Opnd (Flag13-Sem)
      --  Is_Component_Right_Opnd (Flag14-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Multiply
      --  Sloc points to *
      --  Treat_Fixed_As_Integer (Flag14-Sem)
      --  Rounded_Result (Flag18-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Divide
      --  Sloc points to /
      --  Treat_Fixed_As_Integer (Flag14-Sem)
      --  Do_Division_Check (Flag13-Sem)
      --  Rounded_Result (Flag18-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Mod
      --  Sloc points to MOD
      --  Treat_Fixed_As_Integer (Flag14-Sem)
      --  Do_Division_Check (Flag13-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Rem
      --  Sloc points to REM
      --  Treat_Fixed_As_Integer (Flag14-Sem)
      --  Do_Division_Check (Flag13-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Expon
      --  Is_Power_Of_2_For_Shift (Flag13-Sem)
      --  Sloc points to **
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Plus
      --  Sloc points to + (unary)
      --  plus fields for unary operator
      --  plus fields for expression

      --  N_Op_Minus
      --  Sloc points to - (unary)
      --  plus fields for unary operator
      --  plus fields for expression

      --  N_Op_Abs
      --  Sloc points to ABS
      --  plus fields for unary operator
      --  plus fields for expression

      --  N_Op_Not
      --  Sloc points to NOT
      --  plus fields for unary operator
      --  plus fields for expression

      --  See also shift operators in section B.2

      --  Note on fixed-point operations passed to Gigi: For adding operators,
      --  the semantics is to treat these simply as integer operations, with
      --  the small values being ignored (the bounds are already stored in
      --  units of small, so that constraint checking works as usual). For the
      --  case of multiply/divide/rem/mod operations, Gigi will only see fixed
      --  point operands if the Treat_Fixed_As_Integer flag is set and will
      --  thus treat these nodes in identical manner, ignoring small values.

      --------------------------
      -- 4.6  Type Conversion --
      --------------------------

      --  TYPE_CONVERSION ::=
      --    SUBTYPE_MARK (EXPRESSION) | SUBTYPE_MARK (NAME)

      --  In the (NAME) case, the name is stored as the expression

      --  Note: the parser never generates a type conversion node, since it
      --  looks like an indexed component which is generated by preference.
      --  The semantic pass must correct this misidentification.

      --  Gigi handles conversions that involve no change in the root type,
      --  and also all conversions from integer to floating-point types.
      --  Conversions from floating-point to integer are only handled in
      --  the case where Float_Truncate flag set. Other conversions from
      --  floating-point to integer (involving rounding) and all conversions
      --  involving fixed-point types are handled by the expander.

      --  Sprint syntax if Float_Truncate set: X^(Y)
      --  Sprint syntax if Conversion_OK set X?(Y)
      --  Sprint syntax if both flags set X?^(Y)

      --  Note: If either the operand or result type is fixed-point, Gigi will
      --  only see a type conversion node with Conversion_OK set. The front end
      --  takes care of all handling of small's for fixed-point conversions.

      --  N_Type_Conversion
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark (Node4)
      --  Expression (Node3)
      --  Do_Overflow_Check (Flag17-Sem)
      --  Do_Tag_Check (Flag13-Sem)
      --  Do_Length_Check (Flag4-Sem)
      --  Float_Truncate (Flag11-Sem)
      --  Rounded_Result (Flag18-Sem)
      --  Conversion_OK (Flag14-Sem)
      --  plus fields for expression

      --  Note: if a range check is required, then the Do_Range_Check flag
      --  is set in the Expression with the check being done against the
      --  target type range (after the base type conversion, if any).

      -------------------------------
      -- 4.7  Qualified Expression --
      -------------------------------

      --  QUALIFIED_EXPRESSION ::=
      --    SUBTYPE_MARK ' (EXPRESSION) | SUBTYPE_MARK ' AGGREGATE

      --  Note: the parentheses in the (EXPRESSION) case are deemed to enclose
      --  the expression, so the Expression field of this node always points
      --  to a parenthesized expression in this case (i.e. Paren_Count will
      --  always be non-zero for the referenced expression if it is not an
      --  aggregate).

      --  N_Qualified_Expression
      --  Sloc points to apostrophe
      --  Subtype_Mark (Node4)
      --  Expression (Node3) expression or aggregate
      --  plus fields for expression

      --------------------
      -- 4.8  Allocator --
      --------------------

      --  ALLOCATOR ::=
      --    new SUBTYPE_INDICATION | new QUALIFIED_EXPRESSION

      --  Sprint syntax (when storage pool present)
      --    new xxx (storage_pool = pool)

      --  N_Allocator
      --  Sloc points to NEW
      --  Expression (Node3) subtype indication or qualified expression
      --  Storage_Pool (Node1-Sem)
      --  Procedure_To_Call (Node4-Sem)
      --  No_Initialization (Flag13-Sem)
      --  Do_Storage_Check (Flag17-Sem)
      --  plus fields for expression

      ---------------------------------
      -- 5.1  Sequence Of Statements --
      ---------------------------------

      --  SEQUENCE_OF_STATEMENTS ::= STATEMENT {STATEMENT}

      --  Note: Although the parser will not accept a declaration as a
      --  statement, the semantic analyzer may insert declarations (e.g.
      --  declarations of implicit types needed for execution of other
      --  statements) into a sequence of statements, so the code genmerator
      --  should be prepared to accept a declaration where a statement is
      --  expected. Note also that pragmas can appear as statements.

      --------------------
      -- 5.1  Statement --
      --------------------

      --  STATEMENT ::=
      --    {LABEL} SIMPLE_STATEMENT | {LABEL} COMPOUND_STATEMENT

      --  There is no explicit node in the tree for a statement. Instead, the
      --  individual statement appears directly. Labels are treated  as a
      --  kind of statement, i.e. they are linked into a statement list at
      --  the point they appear, so the labeled statement appears following
      --  the label or labels in the statement list.

      ---------------------------
      -- 5.1  Simple Statement --
      ---------------------------

      --  SIMPLE_STATEMENT ::=      NULL_STATEMENT
      --  | ASSIGNMENT_STATEMENT  | EXIT_STATEMENT
      --  | GOTO_STATEMENT        | PROCEDURE_CALL_STATEMENT
      --  | RETURN_STATEMENT      | ENTRY_CALL_STATEMENT
      --  | REQUEUE_STATEMENT     | DELAY_STATEMENT
      --  | ABORT_STATEMENT       | RAISE_STATEMENT
      --  | CODE_STATEMENT

      -----------------------------
      -- 5.1  Compound Statement --
      -----------------------------

      --  COMPOUND_STATEMENT ::=
      --    IF_STATEMENT         | CASE_STATEMENT
      --  | LOOP_STATEMENT       | BLOCK_STATEMENT
      --  | ACCEPT_STATEMENT     | SELECT_STATEMENT

      -------------------------
      -- 5.1  Null Statement --
      -------------------------

      --  NULL_STATEMENT ::= null;

      --  N_Null_Statement
      --  Sloc points to NULL

      ----------------
      -- 5.1  Label --
      ----------------

      --  LABEL ::= <<label_STATEMENT_IDENTIFIER>>

      --  Note that the occurrence of a label is not a defining identifier,
      --  but rather a referencing occurrence. The defining occurrence is
      --  in the implicit label declaration which occurs in the innermost
      --  enclosing block.

      --  N_Label
      --  Sloc points to <<
      --  Identifier (Node1) direct name of statement identifier
      --  Exception_Junk (Flag11-Sem)

      -------------------------------
      -- 5.1  Statement Identifier --
      -------------------------------

      --  STATEMENT_IDENTIFIER ::= DIRECT_NAME

      --  The IDENTIFIER of a STATEMENT_IDENTIFIER shall be an identifier
      --  (not an OPERATOR_SYMBOL)

      -------------------------------
      -- 5.2  Assignment Statement --
      -------------------------------

      --  ASSIGNMENT_STATEMENT ::=
      --    variable_NAME := EXPRESSION;

      --  N_Assignment_Statement
      --  Sloc points to :=
      --  Name (Node2)
      --  Expression (Node3)
      --  Do_Tag_Check (Flag13-Sem)
      --  Do_Length_Check (Flag4-Sem)
      --  Forwards_OK (Flag5-Sem)
      --  Backwards_OK (Flag6-Sem)
      --  No_Ctrl_Actions (Flag7-Sem)

      --  Note: if a range check is required, then the Do_Range_Check flag
      --  is set in the Expression (right hand side), with the check being
      --  done against the type of the Name (left hand side).

      -----------------------
      -- 5.3  If Statement --
      -----------------------

      --  IF_STATEMENT ::=
      --    if CONDITION then
      --      SEQUENCE_OF_STATEMENTS
      --    {elsif CONDITION then
      --      SEQUENCE_OF_STATEMENTS}
      --    [else
      --      SEQUENCE_OF_STATEMENTS]
      --    end if;

      --  Gigi restriction: This expander ensures that the type of the
      --  Condition fields is always Standard.Boolean, even if the type
      --  in the source is some non-standard boolean type.

      --  N_If_Statement
      --  Sloc points to IF
      --  Condition (Node1)
      --  Then_Statements (List2)
      --  Elsif_Parts (List3) (set to No_List if none present)
      --  Else_Statements (List4) (set to No_List if no else part present)
      --  End_Span (Uint5) (set to No_Uint if expander generated)

      --  N_Elsif_Part
      --  Sloc points to ELSIF
      --  Condition (Node1)
      --  Then_Statements (List2)
      --  Condition_Actions (List3-Sem)

      --------------------
      -- 5.3  Condition --
      --------------------

      --  CONDITION ::= boolean_EXPRESSION

      -------------------------
      -- 5.4  Case Statement --
      -------------------------

      --  CASE_STATEMENT ::=
      --    case EXPRESSION is
      --      CASE_STATEMENT_ALTERNATIVE
      --      {CASE_STATEMENT_ALTERNATIVE}
      --    end case;

      --  Note: the Alternatives can contain pragmas. These only occur at
      --  the start of the list, since any pragmas occurring after the first
      --  alternative are absorbed into the corresponding statement sequence.

      --  N_Case_Statement
      --  Sloc points to CASE
      --  Expression (Node3)
      --  Alternatives (List4)
      --  End_Span (Uint5) (set to No_Uint if expander generated)

      -------------------------------------
      -- 5.4  Case Statement Alternative --
      -------------------------------------

      --  CASE_STATEMENT_ALTERNATIVE ::=
      --    when DISCRETE_CHOICE_LIST =>
      --      SEQUENCE_OF_STATEMENTS

      --  N_Case_Statement_Alternative
      --  Sloc points to WHEN
      --  Discrete_Choices (List4)
      --  Statements (List3)

      -------------------------
      -- 5.5  Loop Statement --
      -------------------------

      --  LOOP_STATEMENT ::=
      --    [loop_STATEMENT_IDENTIFIER :]
      --      [ITERATION_SCHEME] loop
      --        SEQUENCE_OF_STATEMENTS
      --      end loop [loop_IDENTIFIER];

      --  Note: The occurrence of a loop label is not a defining identifier
      --  but rather a referencing occurrence. The defining occurrence is in
      --  the implicit label declaration which occurs in the innermost
      --  enclosing block.

      --  Note: there is always a loop statement identifier present in
      --  the tree, even if none was given in the source. In the case where
      --  no loop identifier is given in the source, the parser creates
      --  a name of the form _Loop_n, where n is a decimal integer (the
      --  two underlines ensure that the loop names created in this manner
      --  do not conflict with any user defined identifiers), and the flag
      --  Has_Created_Identifier is set to True. The only exception to the
      --  rule that all loop statement nodes have identifiers occurs for
      --  loops constructed by the expander, and the semantic analyzer will
      --  create and supply dummy loop identifiers in these cases.

      --  N_Loop_Statement
      --  Sloc points to LOOP
      --  Identifier (Node1) loop identifier (set to Empty if no identifier)
      --  Iteration_Scheme (Node2) (set to Empty if no iteration scheme)
      --  Statements (List3)
      --  End_Label (Node4)
      --  Has_Created_Identifier (Flag15)

      --------------------------
      -- 5.5 Iteration Scheme --
      --------------------------

      --  ITERATION_SCHEME ::=
      --    while CONDITION | for LOOP_PARAMETER_SPECIFICATION

      --  Gigi restriction: This expander ensures that the type of the
      --  Condition field is always Standard.Boolean, even if the type
      --  in the source is some non-standard boolean type.

      --  N_Iteration_Scheme
      --  Sloc points to WHILE or FOR
      --  Condition (Node1) (set to Empty if FOR case)
      --  Condition_Actions (List3-Sem)
      --  Loop_Parameter_Specification (Node4) (set to Empty if WHILE case)

      ---------------------------------------
      -- 5.5  Loop parameter specification --
      ---------------------------------------

      --  LOOP_PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER in [reverse] DISCRETE_SUBTYPE_DEFINITION

      --  N_Loop_Parameter_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Reverse_Present (Flag15)
      --  Discrete_Subtype_Definition (Node4)

      --------------------------
      -- 5.6  Block Statement --
      --------------------------

      --  BLOCK_STATEMENT ::=
      --    [block_STATEMENT_IDENTIFIER:]
      --      [declare
      --        DECLARATIVE_PART]
      --      begin
      --        HANDLED_SEQUENCE_OF_STATEMENTS
      --      end [block_IDENTIFIER];

      --  Note that the occurrence of a block identifier is not a defining
      --  identifier, but rather a referencing occurrence. The defining
      --  occurrence is in the implicit label declaration which occurs in
      --  the innermost enclosing block.

      --  Note: there is always a block statement identifier present in
      --  the tree, even if none was given in the source. In the case where
      --  no block identifier is given in the source, the parser creates
      --  a name of the form _Block_n, where n is a decimal integer (the
      --  two underlines ensure that the block names created in this manner
      --  do not conflict with any user defined identifiers), and the flag
      --  Has_Created_Identifier is set to True. The only exception to the
      --  rule that all loop statement nodes have identifiers occurs for
      --  blocks constructed by the expander, and the semantic analyzer
      --  creates and supplies dummy names for the blocks).

      --  N_Block_Statement
      --  Sloc points to DECLARE or BEGIN
      --  Identifier (Node1) block direct name (set to Empty if not present)
      --  Declarations (List2) (set to No_List if no DECLARE part)
      --  Handled_Statement_Sequence (Node4)
      --  Is_Task_Master (Flag5-Sem)
      --  Activation_Chain_Entity (Node3-Sem)
      --  Has_Created_Identifier (Flag15)
      --  Is_Task_Allocation_Block (Flag6)
      --  Is_Asynchronous_Call_Block (Flag7)

      -------------------------
      -- 5.7  Exit Statement --
      -------------------------

      --  EXIT_STATEMENT ::= exit [loop_NAME] [when CONDITION];

      --  Gigi restriction: This expander ensures that the type of the
      --  Condition field is always Standard.Boolean, even if the type
      --  in the source is some non-standard boolean type.

      --  N_Exit_Statement
      --  Sloc points to EXIT
      --  Name (Node2) (set to Empty if no loop name present)
      --  Condition (Node1) (set to Empty if no when part present)

      -------------------------
      -- 5.9  Goto Statement --
      -------------------------

      --  GOTO_STATEMENT ::= goto label_NAME;

      --  N_Goto_Statement
      --  Sloc points to GOTO
      --  Name (Node2)
      --  Exception_Junk (Flag11-Sem)

      ---------------------------------
      -- 6.1  Subprogram Declaration --
      ---------------------------------

      --  SUBPROGRAM_DECLARATION ::= SUBPROGRAM_SPECIFICATION;

      --  N_Subprogram_Declaration
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification (Node1)
      --  Body_To_Inline (Node3-Sem)
      --  Corresponding_Body (Node5-Sem)
      --  Parent_Spec (Node4-Sem)

      ------------------------------------------
      -- 6.1  Abstract Subprogram Declaration --
      ------------------------------------------

      --  ABSTRACT_SUBPROGRAM_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION is abstract;

      --  N_Abstract_Subprogram_Declaration
      --  Sloc points to ABSTRACT
      --  Specification (Node1)

      -----------------------------------
      -- 6.1  Subprogram Specification --
      -----------------------------------

      --  SUBPROGRAM_SPECIFICATION ::=
      --    procedure DEFINING_PROGRAM_UNIT_NAME PARAMETER_PROFILE
      --  | function DEFINING_DESIGNATOR PARAMETER_AND_RESULT_PROFILE

      --  Note: there are no separate nodes for the profiles, instead the
      --  information appears directly in the following nodes.

      --  N_Function_Specification
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name (Node1) (the designator)
      --  Elaboration_Boolean (Node2-Sem)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)
      --  Subtype_Mark (Node4) for return type
      --  Generic_Parent (Node5-Sem)

      --  N_Procedure_Specification
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name (Node1)
      --  Elaboration_Boolean (Node2-Sem)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)
      --  Generic_Parent (Node5-Sem)

      ---------------------
      -- 6.1  Designator --
      ---------------------

      --  DESIGNATOR ::=
      --    [PARENT_UNIT_NAME .] IDENTIFIER | OPERATOR_SYMBOL

      --  Designators that are simply identifiers or operator symbols appear
      --  directly in the tree in this form. The following node is used only
      --  in the case where the designator has a parent unit name component.

      --  N_Designator
      --  Sloc points to period
      --  Name (Node2) holds the parent unit name. Note that this is always
      --   non-Empty, since this node is only used for the case where a
      --   parent library unit package name is present.
      --  Identifier (Node1)

      --  Note that the identifier can also be an operator symbol here.

      ------------------------------
      -- 6.1  Defining Designator --
      ------------------------------

      --  DEFINING_DESIGNATOR ::=
      --    DEFINING_PROGRAM_UNIT_NAME | DEFINING_OPERATOR_SYMBOL

      -------------------------------------
      -- 6.1  Defining Program Unit Name --
      -------------------------------------

      --  DEFINING_PROGRAM_UNIT_NAME ::=
      --    [PARENT_UNIT_NAME .] DEFINING_IDENTIFIER

      --  The parent unit name is present only in the case of a child unit
      --  name (permissible only for Ada 95 for a library level unit, i.e.
      --  a unit at scope level one). If no such name is present, the defining
      --  program unit name is represented simply as the defining identifier.
      --  In the child unit case, the following node is used to represent the
      --  child unit name.

      --  N_Defining_Program_Unit_Name
      --  Sloc points to period
      --  Name (Node2) holds the parent unit name. Note that this is always
      --   non-Empty, since this node is only used for the case where a
      --   parent unit name is present.
      --  Defining_Identifier (Node1)

      --------------------------
      -- 6.1  Operator Symbol --
      --------------------------

      --  OPERATOR_SYMBOL ::= STRING_LITERAL

      --  Note: the fields of the N_Operator_Symbol node are laid out to
      --  match the corresponding fields of an N_Character_Literal node. This
      --  allows easy conversion of the operator symbol node into a character
      --  literal node in the case where a string constant of the form of an
      --  operator symbol is scanned out as such, but turns out semantically
      --  to be a string literal that is not an operator. For details see
      --  Sinfo.CN.Change_Operator_Symbol_To_String_Literal.

      --  N_Operator_Symbol
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the operator symbol
      --  Strval (Str3) Id of string value. This is used if the operator
      --   symbol turns out to be a normal string after all.
      --  Entity (Node4-Sem)
      --  Associated_Node (Node4-Sem)
      --  Has_Private_View (Flag11-Sem) set in generic units.
      --  Etype (Node5-Sem)

      --  Note: the Strval field may be set to No_String for generated
      --  operator symbols that are known not to be string literals
      --  semantically.

      -----------------------------------
      -- 6.1  Defining Operator Symbol --
      -----------------------------------

      --  DEFINING_OPERATOR_SYMBOL ::= OPERATOR_SYMBOL

      --  A defining operator symbol is an entity, which has additional
      --  fields depending on the setting of the Ekind field. These
      --  additional fields are defined (and access subprograms declared)
      --  in package Einfo.

      --  Note: N_Defining_Operator_Symbol is an extended node whose fields
      --  are deliberately layed out to match the layout of fields in an
      --  ordinary N_Operator_Symbol node allowing for easy alteration of
      --  an operator symbol node into a defining operator symbol node.
      --  See Sinfo.CN.Change_Operator_Symbol_To_Defining_Operator_Symbol
      --  for further details.

      --  N_Defining_Operator_Symbol
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the operator symbol
      --  Next_Entity (Node2-Sem)
      --  Scope (Node3-Sem)
      --  Etype (Node5-Sem)

      ----------------------------
      -- 6.1  Parameter Profile --
      ----------------------------

      --  PARAMETER_PROFILE ::= [FORMAL_PART]

      ---------------------------------------
      -- 6.1  Parameter and Result Profile --
      ---------------------------------------

      --  PARAMETER_AND_RESULT_PROFILE ::= [FORMAL_PART] return SUBTYPE_MARK

      --  There is no explicit node in the tree for a parameter and result
      --  profile. Instead the information appears directly in the parent.

      ----------------------
      -- 6.1  Formal part --
      ----------------------

      --  FORMAL_PART ::=
      --    (PARAMETER_SPECIFICATION {; PARAMETER_SPECIFICATION})

      ----------------------------------
      -- 6.1  Parameter specification --
      ----------------------------------

      --  PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : MODE SUBTYPE_MARK
      --      [:= DEFAULT_EXPRESSION]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive specifications were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single Specifications, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Parameter_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  In_Present (Flag15)
      --  Out_Present (Flag17)
      --  Parameter_Type (Node2) subtype mark or access definition
      --  Expression (Node3) (set to Empty if no default expression present)
      --  Do_Accessibility_Check (Flag13-Sem)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)
      --  Default_Expression (Node5-Sem)

      ---------------
      -- 6.1  Mode --
      ---------------

      --  MODE ::= [in] | in out | out

      --  There is no explicit node in the tree for the Mode. Instead the
      --  In_Present and Out_Present flags are set in the parent node to
      --  record the presence of keywords specifying the mode.

      --------------------------
      -- 6.3  Subprogram Body --
      --------------------------

      --  SUBPROGRAM_BODY ::=
      --    SUBPROGRAM_SPECIFICATION is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [DESIGNATOR];

      --  N_Subprogram_Body
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification (Node1)
      --  Declarations (List2)
      --  Handled_Statement_Sequence (Node4)
      --  Activation_Chain_Entity (Node3-Sem)
      --  Corresponding_Spec (Node5-Sem)
      --  Acts_As_Spec (Flag4-Sem)
      --  Bad_Is_Detected (Flag15) used only by parser
      --  Do_Storage_Check (Flag17-Sem)
      --  Has_Priority_Pragma (Flag6-Sem)
      --  Is_Protected_Subprogram_Body (Flag7-Sem)
      --  Is_Task_Master (Flag5-Sem)
      --  Was_Originally_Stub (Flag13-Sem)

      -----------------------------------
      -- 6.4  Procedure Call Statement --
      -----------------------------------

      --  PROCEDURE_CALL_STATEMENT ::=
      --    procedure_NAME; | procedure_PREFIX ACTUAL_PARAMETER_PART;

      --  Note: the reason that a procedure call has expression fields is
      --  that it semantically resembles an expression, e.g. overloading is
      --  allowed and a type is concocted for semantic processing purposes.
      --  Certain of these fields, such as Parens are not relevant, but it
      --  is easier to just supply all of them together!

      --  N_Procedure_Call_Statement
      --  Sloc points to first token of name or prefix
      --  Name (Node2) stores name or prefix
      --  Parameter_Associations (List3) (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual (Node4-Sem)
      --  Controlling_Argument (Node1-Sem) (set to Empty if not dispatching)
      --  Do_Tag_Check (Flag13-Sem)
      --  Parameter_List_Truncated (Flag17-Sem)
      --  ABE_Is_Certain (Flag18-Sem)
      --  plus fields for expression

      --  If any IN parameter requires a range check, then the corresponding
      --  argument expression has the Do_Range_Check flag set, and the range
      --  check is done against the formal type. Note that this argument
      --  expression may appear directly in the Parameter_Associations list,
      --  or may be a descendent of an N_Parameter_Association node that
      --  appears in this list.

      ------------------------
      -- 6.4  Function Call --
      ------------------------

      --  FUNCTION_CALL ::=
      --    function_NAME | function_PREFIX ACTUAL_PARAMETER_PART

      --  Note: the parser may generate an indexed component node or simply
      --  a name node instead of a function call node. The semantic pass must
      --  correct this misidentification.

      --  N_Function_Call
      --  Sloc points to first token of name or prefix
      --  Name (Node2) stores name or prefix
      --  Parameter_Associations (List3) (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual (Node4-Sem)
      --  Controlling_Argument (Node1-Sem) (set to Empty if not dispatching)
      --  Do_Tag_Check (Flag13-Sem)
      --  Parameter_List_Truncated (Flag17-Sem)
      --  ABE_Is_Certain (Flag18-Sem)
      --  plus fields for expression

      --------------------------------
      -- 6.4  Actual Parameter Part --
      --------------------------------

      --  ACTUAL_PARAMETER_PART ::=
      --    (PARAMETER_ASSOCIATION {,PARAMETER_ASSOCIATION})

      --------------------------------
      -- 6.4  Parameter Association --
      --------------------------------

      --  PARAMETER_ASSOCIATION ::=
      --    [formal_parameter_SELECTOR_NAME =>] EXPLICIT_ACTUAL_PARAMETER

      --  Note: the N_Parameter_Association node is built only if a formal
      --  parameter selector name is present, otherwise the parameter
      --  association appears in the tree simply as the node for the
      --  explicit actual parameter.

      --  N_Parameter_Association
      --  Sloc points to formal parameter
      --  Selector_Name (Node2) (always non-Empty, since this node is
      --   only used if a formal parameter selector name is present)
      --  Explicit_Actual_Parameter (Node3)
      --  Next_Named_Actual (Node4-Sem)

      ---------------------------
      -- 6.4  Actual Parameter --
      ---------------------------

      --  EXPLICIT_ACTUAL_PARAMETER ::= EXPRESSION | variable_NAME

      ---------------------------
      -- 6.5  Return Statement --
      ---------------------------

      --  RETURN_STATEMENT ::= return [EXPRESSION];

      --  N_Return_Statement
      --  Sloc points to RETURN
      --  Expression (Node3) (set to Empty if no expression present)
      --  Storage_Pool (Node1-Sem)
      --  Procedure_To_Call (Node4-Sem)
      --  Do_Tag_Check (Flag13-Sem)
      --  Return_Type (Node2-Sem)
      --  By_Ref (Flag5-Sem)

      --  Note: if a range check is required, then Do_Range_Check is set
      --  on the Expression. The range check is against Return_Type.

      ------------------------------
      -- 7.1  Package Declaration --
      ------------------------------

      --  PACKAGE_DECLARATION ::= PACKAGE_SPECIFICATION;

      --  Note: the activation chain entity for a package spec is used for
      --  all tasks declared in the package spec, or in the package body.

      --  N_Package_Declaration
      --  Sloc points to PACKAGE
      --  Specification (Node1)
      --  Corresponding_Body (Node5-Sem)
      --  Parent_Spec (Node4-Sem)
      --  Activation_Chain_Entity (Node3-Sem)

      --------------------------------
      -- 7.1  Package Specification --
      --------------------------------

      --  PACKAGE_SPECIFICATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME is
      --      {BASIC_DECLARATIVE_ITEM}
      --    [private
      --      {BASIC_DECLARATIVE_ITEM}]
      --    end [[PARENT_UNIT_NAME .] IDENTIFIER]

      --  N_Package_Specification
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Visible_Declarations (List2)
      --  Private_Declarations (List3) (set to No_List if no private
      --   part present)
      --  End_Label (Node4)
      --  Generic_Parent (Node5-Sem)

      -----------------------
      -- 7.1  Package Body --
      -----------------------

      --  PACKAGE_BODY ::=
      --    package body DEFINING_PROGRAM_UNIT_NAME is
      --      DECLARATIVE_PART
      --    [begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS]
      --    end [[PARENT_UNIT_NAME .] IDENTIFIER];

      --  N_Package_Body
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Declarations (List2)
      --  Handled_Statement_Sequence (Node4) (set to Empty if no HSS present)
      --  Corresponding_Spec (Node5-Sem)
      --  Was_Originally_Stub (Flag13-Sem)

      --  Note: if a source level package does not contain a handled sequence
      --  of statements, then the parser supplies a dummy one with a null
      --  sequence of statements. Comes_From_Source will be False in this
      --  constructed sequence. The reason we need this is for the End_Label
      --  field in the HSS.

      -----------------------------------
      -- 7.4  Private Type Declaration --
      -----------------------------------

      --  PRIVATE_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is [[abstract] tagged] [limited] private;

      --  Note: TAGGED is not permitted in Ada 83 mode

      --  N_Private_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag15)
      --  Limited_Present (Flag17)

      ----------------------------------------
      -- 7.4  Private Extension Declaration --
      ----------------------------------------

      --  PRIVATE_EXTENSION_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART] is
      --      [abstract] new ancestor_SUBTYPE_INDICATION with private;

      --  Note: private extension declarations are not allowed in Ada 83 mode

      --  N_Private_Extension_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant
      --  Abstract_Present (Flag4)
      --  Subtype_Indication (Node5)

      ---------------------
      -- 8.4  Use Clause --
      ---------------------

      --  USE_CLAUSE ::= USE_PACKAGE_CLAUSE | USE_TYPE_CLAUSE

      -----------------------------
      -- 8.4  Use Package Clause --
      -----------------------------

      --  USE_PACKAGE_CLAUSE ::= use package_NAME {, package_NAME};

      --  N_Use_Package_Clause
      --  Sloc points to USE
      --  Names (List2)
      --  Next_Use_Clause (Node3-Sem)
      --  Hidden_By_Use_Clause (Elist4-Sem)

      --------------------------
      -- 8.4  Use Type Clause --
      --------------------------

      --  USE_TYPE_CLAUSE ::= use type SUBTYPE_MARK {, SUBTYPE_MARK};

      --  Note: use type clause is not permitted in Ada 83 mode

      --  N_Use_Type_Clause
      --  Sloc points to USE
      --  Subtype_Marks (List2)
      --  Next_Use_Clause (Node3-Sem)
      --  Hidden_By_Use_Clause (Elist4-Sem)

      -------------------------------
      -- 8.5  Renaming Declaration --
      -------------------------------

      --  RENAMING_DECLARATION ::=
      --    OBJECT_RENAMING_DECLARATION
      --  | EXCEPTION_RENAMING_DECLARATION
      --  | PACKAGE_RENAMING_DECLARATION
      --  | SUBPROGRAM_RENAMING_DECLARATION
      --  | GENERIC_RENAMING_DECLARATION

      --------------------------------------
      -- 8.5  Object Renaming Declaration --
      --------------------------------------

      --  OBJECT_RENAMING_DECLARATION ::=
      --    DEFINING_IDENTIFIER : SUBTYPE_MARK renames object_NAME;

      --  N_Object_Renaming_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Subtype_Mark (Node4)
      --  Name (Node2)
      --  Corresponding_Generic_Association (Node5-Sem)

      -----------------------------------------
      -- 8.5  Exception Renaming Declaration --
      -----------------------------------------

      --  EXCEPTION_RENAMING_DECLARATION ::=
      --    DEFINING_IDENTIFIER : exception renames exception_NAME;

      --  N_Exception_Renaming_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Name (Node2)

      ---------------------------------------
      -- 8.5  Package Renaming Declaration --
      ---------------------------------------

      --  PACKAGE_RENAMING_DECLARATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME renames package_NAME;

      --  N_Package_Renaming_Declaration
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)

      ------------------------------------------
      -- 8.5  Subprogram Renaming Declaration --
      ------------------------------------------

      --  SUBPROGRAM_RENAMING_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION renames callable_entity_NAME;

      --  N_Subprogram_Renaming_Declaration
      --  Sloc points to RENAMES
      --  Specification (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)
      --  Corresponding_Spec (Node5-Sem)

      -----------------------------------------
      -- 8.5.5  Generic Renaming Declaration --
      -----------------------------------------

      --  GENERIC_RENAMING_DECLARATION ::=
      --    generic package DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_package_NAME
      --  | generic procedure DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_procedure_NAME
      --  | generic function DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_function_NAME

      --  N_Generic_Package_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)

      --  N_Generic_Procedure_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)

      --  N_Generic_Function_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)

      --------------------------------
      -- 9.1  Task Type Declaration --
      --------------------------------

      --  TASK_TYPE_DECLARATION ::=
      --    task type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      [is TASK_DEFINITITION];

      --  N_Task_Type_Declaration
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Task_Body_Procedure (Node2-Sem)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Task_Definition (Node3) (set to Empty if not present)
      --  Corresponding_Body (Node5-Sem)

      ----------------------------------
      -- 9.1  Single Task Declaration --
      ----------------------------------

      --  SINGLE_TASK_DECLARATION ::=
      --    task DEFINING_IDENTIFIER [is TASK_DEFINITION];

      --  N_Single_Task_Declaration
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Task_Definition (Node3) (set to Empty if not present)

      --------------------------
      -- 9.1  Task Definition --
      --------------------------

      --  TASK_DEFINITION ::=
      --      {TASK_ITEM}
      --    [private
      --      {TASK_ITEM}]
      --    end [task_IDENTIFIER]

      --  Note: as a result of semantic analysis, the list of task items can
      --  include implicit type declarations resulting from entry families.

      --  N_Task_Definition
      --  Sloc points to first token of task definition
      --  Visible_Declarations (List2)
      --  Private_Declarations (List3) (set to No_List if no private part)
      --  End_Label (Node4)
      --  Has_Priority_Pragma (Flag6-Sem)
      --  Has_Storage_Size_Pragma (Flag5-Sem)
      --  Has_Task_Info_Pragma (Flag7-Sem)
      --  Has_Task_Name_Pragma (Flag8-Sem)

      --------------------
      -- 9.1  Task Item --
      --------------------

      --  TASK_ITEM ::= ENTRY_DECLARATION | REPRESENTATION_CLAUSE

      --------------------
      -- 9.1  Task Body --
      --------------------

      --  TASK_BODY ::=
      --    task body task_DEFINING_IDENTIFIER is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [task_IDENTIFIER];

      --  Gigi restriction: This node never appears.

      --  N_Task_Body
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Declarations (List2)
      --  Handled_Statement_Sequence (Node4)
      --  Is_Task_Master (Flag5-Sem)
      --  Activation_Chain_Entity (Node3-Sem)
      --  Corresponding_Spec (Node5-Sem)
      --  Was_Originally_Stub (Flag13-Sem)

      -------------------------------------
      -- 9.4  Protected Type Declaration --
      -------------------------------------

      --  PROTECTED_TYPE_DECLARATION ::=
      --    protected type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      is PROTECTED_DEFINITION;

      --  Note: protected type declarations are not permitted in Ada 83 mode

      --  N_Protected_Type_Declaration
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Protected_Definition (Node3)
      --  Corresponding_Body (Node5-Sem)

      ---------------------------------------
      -- 9.4  Single Protected Declaration --
      ---------------------------------------

      --  SINGLE_PROTECTED_DECLARATION ::=
      --    protected DEFINING_IDENTIFIER is PROTECTED_DEFINITION;

      --  Note: single protected declarations are not allowed in Ada 83 mode

      --  N_Single_Protected_Declaration
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Protected_Definition (Node3)

      -------------------------------
      -- 9.4  Protected Definition --
      -------------------------------

      --  PROTECTED_DEFINITION ::=
      --      {PROTECTED_OPERATION_DECLARATION}
      --    [private
      --      {PROTECTED_ELEMENT_DECLARATION}]
      --    end [protected_IDENTIFIER]

      --  N_Protected_Definition
      --  Sloc points to first token of protected definition
      --  Visible_Declarations (List2)
      --  Private_Declarations (List3) (set to No_List if no private part)
      --  End_Label (Node4)
      --  Has_Priority_Pragma (Flag6-Sem)

      ------------------------------------------
      -- 9.4  Protected Operation Declaration --
      ------------------------------------------

      --  PROTECTED_OPERATION_DECLARATION ::=
      --    SUBPROGRAM_DECLARATION
      --  | ENTRY_DECLARATION
      --  | REPRESENTATION_CLAUSE

      ----------------------------------------
      -- 9.4  Protected Element Declaration --
      ----------------------------------------

      --  PROTECTED_ELEMENT_DECLARATION ::=
      --    PROTECTED_OPERATION_DECLARATION | COMPONENT_DECLARATION

      -------------------------
      -- 9.4  Protected Body --
      -------------------------

      --  PROTECTED_BODY ::=
      --    protected body DEFINING_IDENTIFIER is
      --      {PROTECTED_OPERATION_ITEM}
      --    end [protected_IDENTIFIER];

      --  Note: protected bodies are not allowed in Ada 83 mode

      --  Gigi restriction: This node never appears.

      --  N_Protected_Body
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Declarations (List2) protected operation items (and pragmas)
      --  End_Label (Node4)
      --  Corresponding_Spec (Node5-Sem)
      --  Was_Originally_Stub (Flag13-Sem)

      -----------------------------------
      -- 9.4  Protected Operation Item --
      -----------------------------------

      --  PROTECTED_OPERATION_ITEM ::=
      --    SUBPROGRAM_DECLARATION
      --  | SUBPROGRAM_BODY
      --  | ENTRY_BODY
      --  | REPRESENTATION_CLAUSE

      ------------------------------
      -- 9.5.2  Entry Declaration --
      ------------------------------

      --  ENTRY_DECLARATION ::=
      --    entry DEFINING_IDENTIFIER
      --      [(DISCRETE_SUBTYPE_DEFINITION)] PARAMETER_PROFILE;

      --  N_Entry_Declaration
      --  Sloc points to ENTRY
      --  Defining_Identifier (Node1)
      --  Discrete_Subtype_Definition (Node4) (set to Empty if not present)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)

      -----------------------------
      -- 9.5.2  Accept statement --
      -----------------------------

      --  ACCEPT_STATEMENT ::=
      --    accept entry_DIRECT_NAME
      --      [(ENTRY_INDEX)] PARAMETER_PROFILE [do
      --        HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [entry_IDENTIFIER]];

      --  Gigi restriction: This node never appears.

      --  Note: there are no explicit declarations allowed in an accept
      --  statement. However, the implicit declarations for any statement
      --  identifiers (labels and block/loop identifiers) are declarations
      --  that belong logically to the accept statement, and that is why
      --  there is a Declarations field in this node.

      --  N_Accept_Statement
      --  Sloc points to ACCEPT
      --  Entry_Direct_Name (Node1)
      --  Entry_Index (Node5) (set to Empty if not present)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)
      --  Handled_Statement_Sequence (Node4)
      --  Declarations (List2) (set to No_List if no declarations)

      ------------------------
      -- 9.5.2  Entry Index --
      ------------------------

      --  ENTRY_INDEX ::= EXPRESSION

      -----------------------
      -- 9.5.2  Entry Body --
      -----------------------

      --  ENTRY_BODY ::=
      --    entry DEFINING_IDENTIFIER ENTRY_BODY_FORMAL_PART ENTRY_BARRIER is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [entry_IDENTIFIER];

      --  ENTRY_BARRIER ::= when CONDITION

      --  Note: we store the CONDITION of the ENTRY_BARRIER in the node for
      --  the ENTRY_BODY_FORMAL_PART to avoid the N_Entry_Body node getting
      --  too full (it would otherwise have too many fields)

      --  Gigi restriction: This node never appears.

      --  N_Entry_Body
      --  Sloc points to ENTRY
      --  Defining_Identifier (Node1)
      --  Entry_Body_Formal_Part (Node5)
      --  Declarations (List2)
      --  Handled_Statement_Sequence (Node4)
      --  Activation_Chain_Entity (Node3-Sem)

      -----------------------------------
      -- 9.5.2  Entry Body Formal Part --
      -----------------------------------

      --  ENTRY_BODY_FORMAL_PART ::=
      --    [(ENTRY_INDEX_SPECIFICATION)] PARAMETER_PROFILE

      --  Note that an entry body formal part node is present even if it is
      --  empty. This reflects the grammar, in which it is the components of
      --  the entry body formal part that are optional, not the entry body
      --  formal part itself. Also this means that the barrier condition
      --  always has somewhere to be stored.

      --  Gigi restriction: This node never appears.

      --  N_Entry_Body_Formal_Part
      --  Sloc points to first token
      --  Entry_Index_Specification (Node4) (set to Empty if not present)
      --  Parameter_Specifications (List3) (set to No_List if no formal part)
      --  Condition (Node1) from entry barrier of entry body

      --------------------------
      -- 9.5.2  Entry Barrier --
      --------------------------

      --  ENTRY_BARRIER ::= when CONDITION

      --------------------------------------
      -- 9.5.2  Entry Index Specification --
      --------------------------------------

      --  ENTRY_INDEX_SPECIFICATION ::=
      --    for DEFINING_IDENTIFIER in DISCRETE_SUBTYPE_DEFINITION

      --  Gigi restriction: This node never appears.

      --  N_Entry_Index_Specification
      --  Sloc points to FOR
      --  Defining_Identifier (Node1)
      --  Discrete_Subtype_Definition (Node4)

      ---------------------------------
      -- 9.5.3  Entry Call Statement --
      ---------------------------------

      --  ENTRY_CALL_STATEMENT ::= entry_NAME [ACTUAL_PARAMETER_PART];

      --  The parser may generate a procedure call for this construct. The
      --  semantic pass must correct this misidentification where needed.

      --  Gigi restriction: This node never appears.

      --  N_Entry_Call_Statement
      --  Sloc points to first token of name
      --  Name (Node2)
      --  Parameter_Associations (List3) (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual (Node4-Sem)

      ------------------------------
      -- 9.5.4  Requeue Statement --
      ------------------------------

      --  REQUEUE_STATEMENT ::= requeue entry_NAME [with abort];

      --  Note: requeue statements are not permitted in Ada 83 mode

      --  Gigi restriction: This node never appears.

      --  N_Requeue_Statement
      --  Sloc points to REQUEUE
      --  Name (Node2)
      --  Abort_Present (Flag15)

      --------------------------
      -- 9.6  Delay Statement --
      --------------------------

      --  DELAY_STATEMENT ::=
      --    DELAY_UNTIL_STATEMENT
      --  | DELAY_RELATIVE_STATEMENT

      --------------------------------
      -- 9.6  Delay Until Statement --
      --------------------------------

      --  DELAY_UNTIL_STATEMENT ::= delay until delay_EXPRESSION;

      --  Note: delay until statements are not permitted in Ada 83 mode

      --  Gigi restriction: This node never appears.

      --  N_Delay_Until_Statement
      --  Sloc points to DELAY
      --  Expression (Node3)

      -----------------------------------
      -- 9.6  Delay Relative Statement --
      -----------------------------------

      --  DELAY_RELATIVE_STATEMENT ::= delay delay_EXPRESSION;

      --  Gigi restriction: This node never appears.

      --  N_Delay_Relative_Statement
      --  Sloc points to DELAY
      --  Expression (Node3)

      ---------------------------
      -- 9.7  Select Statement --
      ---------------------------

      --  SELECT_STATEMENT ::=
      --    SELECTIVE_ACCEPT
      --  | TIMED_ENTRY_CALL
      --  | CONDITIONAL_ENTRY_CALL
      --  | ASYNCHRONOUS_SELECT

      -----------------------------
      -- 9.7.1  Selective Accept --
      -----------------------------

      --  SELECTIVE_ACCEPT ::=
      --    select
      --      [GUARD]
      --        SELECT_ALTERNATIVE
      --    {or
      --      [GUARD]
      --        SELECT_ALTERNATIVE}
      --    [else
      --      SEQUENCE_OF_STATEMENTS]
      --    end select;

      --  Gigi restriction: This node never appears.

      --  Note: the guard expression, if present, appears in the node for
      --  the select alternative.

      --  N_Selective_Accept
      --  Sloc points to SELECT
      --  Select_Alternatives (List1)
      --  Else_Statements (List4) (set to No_List if no else part)

      ------------------
      -- 9.7.1  Guard --
      ------------------

      --  GUARD ::= when CONDITION =>

      --  As noted above, the CONDITION that is part of a GUARD is included
      --  in the node for the select alernative for convenience.

      -------------------------------
      -- 9.7.1  Select Alternative --
      -------------------------------

      --  SELECT_ALTERNATIVE ::=
      --    ACCEPT_ALTERNATIVE
      --  | DELAY_ALTERNATIVE
      --  | TERMINATE_ALTERNATIVE

      -------------------------------
      -- 9.7.1  Accept Alternative --
      -------------------------------

      --  ACCEPT_ALTERNATIVE ::=
      --    ACCEPT_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  Gigi restriction: This node never appears.

      --  N_Accept_Alternative
      --  Sloc points to ACCEPT
      --  Accept_Statement (Node2)
      --  Condition (Node1) from the guard (set to Empty if no guard present)
      --  Statements (List3) (set to Empty_List if no statements)
      --  Pragmas_Before (List4) pragmas before alt (set to No_List if none)
      --  Accept_Handler_Records (List5-Sem)

      ------------------------------
      -- 9.7.1  Delay Alternative --
      ------------------------------

      --  DELAY_ALTERNATIVE ::=
      --    DELAY_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  Gigi restriction: This node never appears.

      --  N_Delay_Alternative
      --  Sloc points to DELAY
      --  Delay_Statement (Node2)
      --  Condition (Node1) from the guard (set to Empty if no guard present)
      --  Statements (List3) (set to Empty_List if no statements)
      --  Pragmas_Before (List4) pragmas before alt (set to No_List if none)

      ----------------------------------
      -- 9.7.1  Terminate Alternative --
      ----------------------------------

      --  TERMINATE_ALTERNATIVE ::= terminate;

      --  Gigi restriction: This node never appears.

      --  N_Terminate_Alternative
      --  Sloc points to TERMINATE
      --  Condition (Node1) from the guard (set to Empty if no guard present)
      --  Pragmas_Before (List4) pragmas before alt (set to No_List if none)
      --  Pragmas_After (List5) pragmas after alt (set to No_List if none)

      -----------------------------
      -- 9.7.2  Timed Entry Call --
      -----------------------------

      --  TIMED_ENTRY_CALL ::=
      --    select
      --      ENTRY_CALL_ALTERNATIVE
      --    or
      --      DELAY_ALTERNATIVE
      --    end select;

      --  Gigi restriction: This node never appears.

      --  N_Timed_Entry_Call
      --  Sloc points to SELECT
      --  Entry_Call_Alternative (Node1)
      --  Delay_Alternative (Node4)

      -----------------------------------
      -- 9.7.2  Entry Call Alternative --
      -----------------------------------

      --  ENTRY_CALL_ALTERNATIVE ::=
      --    ENTRY_CALL_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  Gigi restriction: This node never appears.

      --  N_Entry_Call_Alternative
      --  Sloc points to first token of entry call statement
      --  Entry_Call_Statement (Node1)
      --  Statements (List3) (set to Empty_List if no statements)
      --  Pragmas_Before (List4) pragmas before alt (set to No_List if none)

      -----------------------------------
      -- 9.7.3  Conditional Entry Call --
      -----------------------------------

      --  CONDITIONAL_ENTRY_CALL ::=
      --    select
      --      ENTRY_CALL_ALTERNATIVE
      --    else
      --      SEQUENCE_OF_STATEMENTS
      --    end select;

      --  Gigi restriction: This node never appears.

      --  N_Conditional_Entry_Call
      --  Sloc points to SELECT
      --  Entry_Call_Alternative (Node1)
      --  Else_Statements (List4)

      --------------------------------
      -- 9.7.4  Asynchronous Select --
      --------------------------------

      --  ASYNCHRONOUS_SELECT ::=
      --    select
      --      TRIGGERING_ALTERNATIVE
      --    then abort
      --      ABORTABLE_PART
      --    end select;

      --  Note: asynchronous select is not permitted in Ada 83 mode

      --  Gigi restriction: This node never appears.

      --  N_Asynchronous_Select
      --  Sloc points to SELECT
      --  Triggering_Alternative (Node1)
      --  Abortable_Part (Node2)

      -----------------------------------
      -- 9.7.4  Triggering Alternative --
      -----------------------------------

      --  TRIGGERING_ALTERNATIVE ::=
      --    TRIGGERING_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  Gigi restriction: This node never appears.

      --  N_Triggering_Alternative
      --  Sloc points to first token of triggering statement
      --  Triggering_Statement (Node1)
      --  Statements (List3) (set to Empty_List if no statements)
      --  Pragmas_Before (List4) pragmas before alt (set to No_List if none)

      ---------------------------------
      -- 9.7.4  Triggering Statement --
      ---------------------------------

      --  TRIGGERING_STATEMENT ::= ENTRY_CALL_STATEMENT | DELAY_STATEMENT

      ---------------------------
      -- 9.7.4  Abortable Part --
      ---------------------------

      --  ABORTABLE_PART ::= SEQUENCE_OF_STATEMENTS

      --  Gigi restriction: This node never appears.

      --  N_Abortable_Part
      --  Sloc points to ABORT
      --  Statements (List3)

      --------------------------
      -- 9.8  Abort Statement --
      --------------------------

      --  ABORT_STATEMENT ::= abort task_NAME {, task_NAME};

      --  Gigi restriction: This node never appears.

      --  N_Abort_Statement
      --  Sloc points to ABORT
      --  Names (List2)

      -------------------------
      -- 10.1.1  Compilation --
      -------------------------

      --  COMPILATION ::= {COMPILATION_UNIT}

      --  There is no explicit node in the tree for a compilation, since in
      --  general the compiler is processing only a single compilation unit
      --  at a time. It is possible to parse multiple units in syntax check
      --  only mode, but they the trees are discarded in any case.

      ------------------------------
      -- 10.1.1  Compilation Unit --
      ------------------------------

      --  COMPILATION_UNIT ::=
      --    CONTEXT_CLAUSE LIBRARY_ITEM
      --  | CONTEXT_CLAUSE SUBUNIT

      --  The N_Compilation_Unit node itself respresents the above syntax.
      --  However, there are two additional items not reflected in the above
      --  syntax. First we have the global declarations that are added by the
      --  code generator. These are outer level declarations (so they cannot
      --  be represented as being inside the units). An example is the wrapper
      --  subprograms that are created to do ABE checking. As always a list of
      --  declarations can contain actions as well (i.e. statements), and such
      --  statements are executed as part of the elaboration of the unit. Note
      --  that all such declarations are elaborated before the library unit.

      --  Similarly, certain actions need to be elaborated at the completion
      --  of elaboration of the library unit (notably the statement that sets
      --  the Boolean flag indicating that elaboration is complete).

      --  The third item not reflected in the syntax is pragmas that appear
      --  after the compilation unit. As always pragmas are a problem since
      --  they are not part of the formal syntax, but can be stuck into the
      --  source following a set of ad hoc rules, and we have to find an ad
      --  hoc way of sticking them into the tree. For pragmas that appear
      --  before the library unit, we just consider them to be part of the
      --  context clause, and pragmas can appear in the Context_Items list
      --  of the compilation unit. However, pragmas can also appear after
      --  the library item.

      --  To deal with all these problems, we create an auxiliary node for
      --  a compilation unit, referenced from the N_Compilation_Unit node
      --  that contains these three items.

      --  N_Compilation_Unit
      --  Sloc points to first token of defining unit name
      --  Library_Unit (Node4-Sem) corresponding/parent spec/body
      --  Context_Items (List1) context items and pragmas preceding unit
      --  Private_Present (Flag15) set if library unit has private keyword
      --  Unit (Node2) library item or subunit
      --  Aux_Decls_Node (Node5) points to the N_Compilation_Unit_Aux node
      --  Has_No_Elaboration_Code (Flag17-Sem)
      --  Body_Required (Flag13-Sem) set for spec if body is required
      --  Acts_As_Spec (Flag4-Sem) flag for subprogram body with no spec
      --  First_Inlined_Subprogram (Node3-Sem)

      --  N_Compilation_Unit_Aux
      --  Sloc is a copy of the Sloc from the N_Compilation_Unit node
      --  Declarations (List2) (set to No_List if no global declarations)
      --  Actions (List1) (set to No_List if no actions)
      --  Pragmas_After (List5) pragmas after unit (set to No_List if none)

      --------------------------
      -- 10.1.1  Library Item --
      --------------------------

      --  LIBRARY_ITEM ::=
      --    [private] LIBRARY_UNIT_DECLARATION
      --  | LIBRARY_UNIT_BODY
      --  | [private] LIBRARY_UNIT_RENAMING_DECLARATION

      --  Note: PRIVATE is not allowed in Ada 83 mode

      --  There is no explicit node in the tree for library item, instead
      --  the declaration or body, and the flag for private if present,
      --  appear in the N_Compilation_Unit clause.

      ----------------------------------------
      -- 10.1.1  Library Unit Declararation --
      ----------------------------------------

      --  LIBRARY_UNIT_DECLARATION ::=
      --    SUBPROGRAM_DECLARATION | PACKAGE_DECLARATION
      --  | GENERIC_DECLARATION    | GENERIC_INSTANTIATION

      -------------------------------------------------
      -- 10.1.1  Library Unit Renaming Declararation --
      -------------------------------------------------

      --  LIBRARY_UNIT_RENAMING_DECLARATION ::=
      --    PACKAGE_RENAMING_DECLARATION
      --  | GENERIC_RENAMING_DECLARATION
      --  | SUBPROGRAM_RENAMING_DECLARATION

      -------------------------------
      -- 10.1.1  Library unit body --
      -------------------------------

      --  LIBRARY_UNIT_BODY ::= SUBPROGRAM_BODY | PACKAGE_BODY

      ------------------------------
      -- 10.1.1  Parent Unit Name --
      ------------------------------

      --  PARENT_UNIT_NAME ::= NAME

      ----------------------------
      -- 10.1.2  Context clause --
      ----------------------------

      --  CONTEXT_CLAUSE ::= {CONTEXT_ITEM}

      --  The context clause can include pragmas, and any pragmas that appear
      --  before the context clause proper (i.e. all configuration pragmas,
      --  also appear at the front of this list).

      --------------------------
      -- 10.1.2  Context_Item --
      --------------------------

      --  CONTEXT_ITEM ::= WITH_CLAUSE | USE_CLAUSE  | WITH_TYPE_CLAUSE

      -------------------------
      -- 10.1.2  With clause --
      -------------------------

      --  WITH_CLAUSE ::=
      --    with library_unit_NAME {,library_unit_NAME};

      --  A separate With clause is built for each name, so that we have
      --  a Corresponding_Spec field for each with'ed spec. The flags
      --  First_Name and Last_Name are used to reconstruct the exact
      --  source form. When a list of names appears in one with clause,
      --  the first name in the list has First_Name set, and the last
      --  has Last_Name set. If the with clause has only one name, then
      --  both of the flags First_Name and Last_Name are set in this name.

      --  Note: in the case of implicit with's that are installed by the
      --  Rtsfind routine, Implicit_With is set, and the Sloc is typically
      --  set to Standard_Location, but it is incorrect to test the Sloc
      --  to find out if a with clause is implicit, test the flag instead.

      --  N_With_Clause
      --  Sloc points to first token of library unit name
      --  Name (Node2)
      --  Library_Unit (Node4-Sem)
      --  Corresponding_Spec (Node5-Sem)
      --  First_Name (Flag5) (set to True if first name or only one name)
      --  Last_Name (Flag6) (set to True if last name or only one name)
      --  Context_Installed (Flag13-Sem)
      --  Elaborate_Present (Flag4-Sem)
      --  Elaborate_All_Present (Flag15-Sem)
      --  Implicit_With (Flag17-Sem)
      --  Unreferenced_In_Spec (Flag7-Sem)
      --  No_Entities_Ref_In_Spec (Flag8-Sem)

      ----------------------
      -- With_Type clause --
      ----------------------

      --  This is a GNAT extension, used to implement mutually recursive
      --  types declared in different packages.

      --  WITH_TYPE_CLAUSE ::=
      --    with type type_NAME is access | with type type_NAME is tagged

      --  N_With_Type_Clause
      --  Sloc points to first token of type name
      --  Name (Node2)
      --  Tagged_Present (Flag15)

      ---------------------
      -- 10.2  Body stub --
      ---------------------

      --  BODY_STUB ::=
      --    SUBPROGRAM_BODY_STUB
      --  | PACKAGE_BODY_STUB
      --  | TASK_BODY_STUB
      --  | PROTECTED_BODY_STUB

      ----------------------------------
      -- 10.1.3  Subprogram Body Stub --
      ----------------------------------

      --  SUBPROGRAM_BODY_STUB ::=
      --    SUBPROGRAM_SPECIFICATION is separate;

      --  N_Subprogram_Body_Stub
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification (Node1)
      --  Library_Unit (Node4-Sem) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      -------------------------------
      -- 10.1.3  Package Body Stub --
      -------------------------------

      --  PACKAGE_BODY_STUB ::=
      --    package body DEFINING_IDENTIFIER is separate;

      --  N_Package_Body_Stub
      --  Sloc points to PACKAGE
      --  Defining_Identifier (Node1)
      --  Library_Unit (Node4-Sem) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      ----------------------------
      -- 10.1.3  Task Body Stub --
      ----------------------------

      --  TASK_BODY_STUB ::=
      --    task body DEFINING_IDENTIFIER is separate;

      --  N_Task_Body_Stub
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Library_Unit (Node4-Sem) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      ---------------------------------
      -- 10.1.3  Protected Body Stub --
      ---------------------------------

      --  PROTECTED_BODY_STUB ::=
      --    protected body DEFINING_IDENTIFIER is separate;

      --  Note: protected body stubs are not allowed in Ada 83 mode

      --  N_Protected_Body_Stub
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Library_Unit (Node4-Sem) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      ---------------------
      -- 10.1.3  Subunit --
      ---------------------

      --  SUBUNIT ::= separate (PARENT_UNIT_NAME) PROPER_BODY

      --  N_Subunit
      --  Sloc points to SEPARATE
      --  Name (Node2) is the name of the parent unit
      --  Proper_Body (Node1) is the subunit body
      --  Corresponding_Stub (Node3-Sem) is the stub declaration for the unit.

      ---------------------------------
      -- 11.1  Exception Declaration --
      ---------------------------------

      --  EXCEPTION_DECLARATION ::= DEFINING_IDENTIFIER_LIST : exception;

      --  For consistency with object declarations etc, the parser converts
      --  the case of multiple identifiers being declared to a series of
      --  declarations in which the expression is copied, using the More_Ids
      --  and Prev_Ids flags to remember the souce form as described in the
      --  section on "Handling of Defining Identifier Lists".

      --  N_Exception_Declaration
      --  Sloc points to EXCEPTION
      --  Defining_Identifier (Node1)
      --  Expression (Node3-Sem)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      ------------------------------------------
      -- 11.2  Handled Sequence Of Statements --
      ------------------------------------------

      --  HANDLED_SEQUENCE_OF_STATEMENTS ::=
      --      SEQUENCE_OF_STATEMENTS
      --    [exception
      --      EXCEPTION_HANDLER
      --      {EXCEPTION_HANDLER}]
      --    [at end
      --      cleanup_procedure_call (param, param, param, ...);]

      --  The AT END phrase is a GNAT extension to provide for cleanups. It is
      --  used only internally currently, but is considered to be syntactic.
      --  At the moment, the only cleanup action allowed is a single call to
      --  a parameterless procedure, and the Identifier field of the node is
      --  the procedure to be called. Also there is a current restriction
      --  that exception handles and a cleanup cannot be present in the same
      --  frame, so at least one of Exception_Handlers or the Identifier must
      --  be missing.

      --  Actually, more accurately, this restriction applies to the original
      --  source program. In the expanded tree, if the At_End_Proc field is
      --  present, then there will also be an exception handler of the form:

      --     when all others =>
      --        cleanup;
      --        raise;

      --  where cleanup is the procedure to be generated. The reason we do
      --  this is so that the front end can handle the necessary entries in
      --  the exception tables, and other exception handler actions required
      --  as part of the normal handling for exception handlers.

      --  The AT END cleanup handler protects only the sequence of statements
      --  (not the associated declarations of the parent), just like exception
      --  handlers. The big difference is that the cleanup procedure is called
      --  on either a normal or an abnormal exit from the statement sequence.

      --  Note: the list of Exception_Handlers can contain pragmas as well
      --  as actual handlers. In practice these pragmas can only occur at
      --  the start of the list, since any pragmas occurring later on will
      --  be included in the statement list of the corresponding handler.

      --  Note: although in the Ada syntax, the sequence of statements in
      --  a handled sequence of statements can only contain statements, we
      --  allow free mixing of declarations and statements in the resulting
      --  expanded tree. This is for example used to deal with the case of
      --  a cleanup procedure that must handle declarations as well as the
      --  statements of a block.

      --  N_Handled_Sequence_Of_Statements
      --  Sloc points to first token of first statement
      --  Statements (List3)
      --  End_Label (Node4) (set to Empty if expander generated)
      --  Exception_Handlers (List5) (set to No_List if none present)
      --  At_End_Proc (Node1) (set to Empty if no clean up procedure)
      --  First_Real_Statement (Node2-Sem)
      --  Zero_Cost_Handling (Flag5-Sem)

      --  Note: the parent always contains a Declarations field which contains
      --  declarations associated with the handled sequence of statements. This
      --  is true even in the case of an accept statement (see description of
      --  the N_Accept_Statement node).

      --  End_Label refers to the containing construct.

      -----------------------------
      -- 11.2  Exception Handler --
      -----------------------------

      --  EXCEPTION_HANDLER ::=
      --    when [CHOICE_PARAMETER_SPECIFICATION :]
      --      EXCEPTION_CHOICE {| EXCEPTION_CHOICE} =>
      --        SEQUENCE_OF_STATEMENTS

      --  Note: choice parameter specification is not allowed in Ada 83 mode

      --  N_Exception_Handler
      --  Sloc points to WHEN
      --  Choice_Parameter (Node2) (set to Empty if not present)
      --  Exception_Choices (List4)
      --  Statements (List3)
      --  Zero_Cost_Handling (Flag5-Sem)

      ------------------------------------------
      -- 11.2  Choice parameter specification --
      ------------------------------------------

      --  CHOICE_PARAMETER_SPECIFICATION ::= DEFINING_IDENTIFIER

      ----------------------------
      -- 11.2  Exception Choice --
      ----------------------------

      --  EXCEPTION_CHOICE ::= exception_NAME | others

      --  Except in the case of OTHERS, no explicit node appears in the tree
      --  for exception choice. Instead the exception name appears directly.
      --  An OTHERS choice is represented by a N_Others_Choice node (see
      --  section 3.8.1.

      --  Note: for the exception choice created for an at end handler, the
      --  exception choice is an N_Others_Choice node with All_Others set.

      ---------------------------
      -- 11.3  Raise Statement --
      ---------------------------

      --  RAISE_STATEMENT ::= raise [exception_NAME];

      --  N_Raise_Statement
      --  Sloc points to RAISE
      --  Name (Node2) (set to Empty if no exception name present)

      -------------------------------
      -- 12.1  Generic Declaration --
      -------------------------------

      --  GENERIC_DECLARATION ::=
      --    GENERIC_SUBPROGRAM_DECLARATION | GENERIC_PACKAGE_DECLARATION

      ------------------------------------------
      -- 12.1  Generic Subprogram Declaration --
      ------------------------------------------

      --  GENERIC_SUBPROGRAM_DECLARATION ::=
      --    GENERIC_FORMAL_PART SUBPROGRAM_SPECIFICATION;

      --  Note: Generic_Formal_Declarations can include pragmas

      --  N_Generic_Subprogram_Declaration
      --  Sloc points to GENERIC
      --  Specification (Node1) subprogram specification
      --  Corresponding_Body (Node5-Sem)
      --  Generic_Formal_Declarations (List2) from generic formal part
      --  Parent_Spec (Node4-Sem)

      ---------------------------------------
      -- 12.1  Generic Package Declaration --
      ---------------------------------------

      --  GENERIC_PACKAGE_DECLARATION ::=
      --    GENERIC_FORMAL_PART PACKAGE_SPECIFICATION;

      --  Note: when we do generics right, the Activation_Chain_Entity entry
      --  for this node can be removed (since the expander won't see generic
      --  units any more)???.

      --  Note: Generic_Formal_Declarations can include pragmas

      --  N_Generic_Package_Declaration
      --  Sloc points to GENERIC
      --  Specification (Node1) package specification
      --  Corresponding_Body (Node5-Sem)
      --  Generic_Formal_Declarations (List2) from generic formal part
      --  Parent_Spec (Node4-Sem)
      --  Activation_Chain_Entity (Node3-Sem)

      -------------------------------
      -- 12.1  Generic Formal Part --
      -------------------------------

      --  GENERIC_FORMAL_PART ::=
      --    generic {GENERIC_FORMAL_PARAMETER_DECLARATION | USE_CLAUSE}

      ------------------------------------------------
      -- 12.1  Generic Formal Parameter Declaration --
      ------------------------------------------------

      --  GENERIC_FORMAL_PARAMETER_DECLARATION ::=
      --    FORMAL_OBJECT_DECLARATION
      --  | FORMAL_TYPE_DECLARATION
      --  | FORMAL_SUBPROGRAM_DECLARATION
      --  | FORMAL_PACKAGE_DECLARATION

      ---------------------------------
      -- 12.3  Generic Instantiation --
      ---------------------------------

      --  GENERIC_INSTANTIATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_package_NAME [GENERIC_ACTUAL_PART];
      --  | procedure DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_procedure_NAME [GENERIC_ACTUAL_PART];
      --  | function DEFINING_DESIGNATOR is
      --      new generic_function_NAME [GENERIC_ACTUAL_PART];

      --  N_Package_Instantiation
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Parent_Spec (Node4-Sem)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)

      --  N_Procedure_Instantiation
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Sem)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)

      --  N_Function_Instantiation
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Parent_Spec (Node4-Sem)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)

      ------------------------------
      -- 12.3 Generic Actual Part --
      ------------------------------

      --  GENERIC_ACTUAL_PART ::=
      --    (GENERIC_ASSOCIATION {, GENERIC_ASSOCIATION})

      -------------------------------
      -- 12.3  Generic Association --
      -------------------------------

      --  GENERIC_ASSOCIATION ::=
      --    [generic_formal_parameter_SELECTOR_NAME =>]
      --      EXPLICIT_GENERIC_ACTUAL_PARAMETER

      --  Note: unlike the procedure call case, a generic association node
      --  is generated for every association, even if no formal is present.
      --  In this case the parser will leave the Selector_Name field set
      --  to Empty, to be filled in later by the semantic pass.

      --  N_Generic_Association
      --  Sloc points to first token of generic association
      --  Selector_Name (Node2) (set to Empty if no formal
      --   parameter selector name)
      --  Explicit_Generic_Actual_Parameter (Node1)

      ---------------------------------------------
      -- 12.3  Explicit Generic Actual Parameter --
      ---------------------------------------------

      --  EXPLICIT_GENERIC_ACTUAL_PARAMETER ::=
      --    EXPRESSION      | variable_NAME   | subprogram_NAME
      --  | entry_NAME      | SUBTYPE_MARK    | package_instance_NAME

      -------------------------------------
      -- 12.4  Formal Object Declaration --
      -------------------------------------

      --  FORMAL_OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST :
      --      MODE SUBTYPE_MARK [:= DEFAULT_EXPRESSION];

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Formal_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  In_Present (Flag15)
      --  Out_Present (Flag17)
      --  Subtype_Mark (Node4)
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -----------------------------------
      -- 12.5  Formal Type Declaration --
      -----------------------------------

      --  FORMAL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is FORMAL_TYPE_DEFINITION;

      --  N_Formal_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Formal_Type_Definition (Node3)
      --  Discriminant_Specifications (List4) (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present (Flag13) set if (<>) discriminant

      ----------------------------------
      -- 12.5  Formal type definition --
      ----------------------------------

      --  FORMAL_TYPE_DEFINITION ::=
      --    FORMAL_PRIVATE_TYPE_DEFINITION
      --  | FORMAL_DERIVED_TYPE_DEFINITION
      --  | FORMAL_DISCRETE_TYPE_DEFINITION
      --  | FORMAL_SIGNED_INTEGER_TYPE_DEFINITION
      --  | FORMAL_MODULAR_TYPE_DEFINITION
      --  | FORMAL_FLOATING_POINT_DEFINITION
      --  | FORMAL_ORDINARY_FIXED_POINT_DEFINITION
      --  | FORMAL_DECIMAL_FIXED_POINT_DEFINITION
      --  | FORMAL_ARRAY_TYPE_DEFINITION
      --  | FORMAL_ACCESS_TYPE_DEFINITION

      ---------------------------------------------
      -- 12.5.1  Formal Private Type Definition --
      ---------------------------------------------

      --  FORMAL_PRIVATE_TYPE_DEFINITION ::=
      --    [[abstract] tagged] [limited] private

      --  Note: TAGGED is not allowed in Ada 83 mode

      --  N_Formal_Private_Type_Definition
      --  Sloc points to PRIVATE
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag15)
      --  Limited_Present (Flag17)

      --------------------------------------------
      -- 12.5.1  Formal Derived Type Definition --
      --------------------------------------------

      --  FORMAL_DERIVED_TYPE_DEFINITION ::=
      --    [abstract] new SUBTYPE_MARK [with private]

      --  Note: this construct is not allowed in Ada 83 mode

      --  N_Formal_Derived_Type_Definition
      --  Sloc points to NEW
      --  Subtype_Mark (Node4)
      --  Private_Present (Flag15)
      --  Abstract_Present (Flag4)

      ---------------------------------------------
      -- 12.5.2  Formal Discrete Type Definition --
      ---------------------------------------------

      --  FORMAL_DISCRETE_TYPE_DEFINITION ::= (<>)

      --  N_Formal_Discrete_Type_Definition
      --  Sloc points to (

      ---------------------------------------------------
      -- 12.5.2  Formal Signed Integer Type Definition --
      ---------------------------------------------------

      --  FORMAL_SIGNED_INTEGER_TYPE_DEFINITION ::= range <>

      --  N_Formal_Signed_Integer_Type_Definition
      --  Sloc points to RANGE

      --------------------------------------------
      -- 12.5.2  Formal Modular Type Definition --
      --------------------------------------------

      --  FORMAL_MODULAR_TYPE_DEFINITION ::= mod <>

      --  N_Formal_Modular_Type_Definition
      --  Sloc points to MOD

      ----------------------------------------------
      -- 12.5.2  Formal Floating Point Definition --
      ----------------------------------------------

      --  FORMAL_FLOATING_POINT_DEFINITION ::= digits <>

      --  N_Formal_Floating_Point_Definition
      --  Sloc points to DIGITS

      ----------------------------------------------------
      -- 12.5.2  Formal Ordinary Fixed Point Definition --
      ----------------------------------------------------

      --  FORMAL_ORDINARY_FIXED_POINT_DEFINITION ::= delta <>

      --  N_Formal_Ordinary_Fixed_Point_Definition
      --  Sloc points to DELTA

      ---------------------------------------------------
      -- 12.5.2  Formal Decimal Fixed Point Definition --
      ---------------------------------------------------

      --  FORMAL_DECIMAL_FIXED_POINT_DEFINITION ::= delta <> digits <>

      --  Note: formal decimal fixed point definition not allowed in Ada 83

      --  N_Formal_Decimal_Fixed_Point_Definition
      --  Sloc points to DELTA

      ------------------------------------------
      -- 12.5.3  Formal Array Type Definition --
      ------------------------------------------

      --  FORMAL_ARRAY_TYPE_DEFINITION ::= ARRAY_TYPE_DEFINITION

      -------------------------------------------
      -- 12.5.4  Formal Access Type Definition --
      -------------------------------------------

      --  FORMAL_ACCESS_TYPE_DEFINITION ::= ACCESS_TYPE_DEFINITION

      -----------------------------------------
      -- 12.6  Formal Subprogram Declaration --
      -----------------------------------------

      --  FORMAL_SUBPROGRAM_DECLARATION ::=
      --    with SUBPROGRAM_SPECIFICATION [is SUBPROGRAM_DEFAULT];

      --  N_Formal_Subprogram_Declaration
      --  Sloc points to WITH
      --  Specification (Node1)
      --  Default_Name (Node2) (set to Empty if no subprogram default)
      --  Box_Present (Flag15)

      --  Note: if no subprogram default is present, then Name is set
      --  to Empty, and Box_Present is False.

      ------------------------------
      -- 12.6  Subprogram Default --
      ------------------------------

      --  SUBPROGRAM_DEFAULT ::= DEFAULT_NAME | <>

      --  There is no separate node in the tree for a subprogram default.
      --  Instead the parent (N_Formal_Subprogram_Declaration) node contains
      --  the default name or box indication, as needed.

      ------------------------
      -- 12.6  Default Name --
      ------------------------

      --  DEFAULT_NAME ::= NAME

      --------------------------------------
      -- 12.7  Formal Package Declaration --
      --------------------------------------

      --  FORMAL_PACKAGE_DECLARATION ::=
      --    with package DEFINING_IDENTIFIER
      --      is new generic_package_NAME FORMAL_PACKAGE_ACTUAL_PART;

      --  Note: formal package declarations not allowed in Ada 83 mode

      --  N_Formal_Package_Declaration
      --  Sloc points to WITH
      --  Defining_Identifier (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if (<>) case or
      --   empty generic actual part)
      --  Box_Present (Flag15)
      --  Instance_Spec (Node5-Sem)
      --  ABE_Is_Certain (Flag18-Sem)

      --------------------------------------
      -- 12.7  Formal Package Actual Part --
      --------------------------------------

      --  FORMAL_PACKAGE_ACTUAL_PART ::=
      --    (<>) | [GENERIC_ACTUAL_PART]

      --  There is no explicit node in the tree for a formal package
      --  actual part. Instead the information appears in the parent node
      --  (i.e. the formal package declaration node itself).

      ---------------------------------
      -- 13.1  Representation clause --
      ---------------------------------

      --  REPRESENTATION_CLAUSE ::=
      --    ATTRIBUTE_DEFINITION_CLAUSE
      --  | ENUMERATION_REPRESENTATION_CLAUSE
      --  | RECORD_REPRESENTATION_CLAUSE
      --  | AT_CLAUSE

      ----------------------
      -- 13.1  Local Name --
      ----------------------

      --  LOCAL_NAME :=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      --  The construct DIRECT_NAME'ATTRIBUTE_DESIGNATOR appears in the tree
      --  as an attribute reference, which has essentially the same form.

      ---------------------------------------
      -- 13.3  Attribute definition clause --
      ---------------------------------------

      --  ATTRIBUTE_DEFINITION_CLAUSE ::=
      --    for LOCAL_NAME'ATTRIBUTE_DESIGNATOR use EXPRESSION;
      --  | for LOCAL_NAME'ATTRIBUTE_DESIGNATOR use NAME;

      --  In Ada 83, the expression must be a simple expression and the
      --  local name must be a direct name.

      --  Note: The only attribute definition clause that is processed
      --  by Gigi is the alignment clause (for all other cases, the
      --  information is extracted by the front end and either results
      --  in setting entity information, e.g. Esize for the Size case,
      --  or in appropriate expansion actions (e.g. in the storage size
      --  case). For the alignment case, Gigi requires that the expression
      --  be an integer literal.

      --  N_Attribute_Definition_Clause
      --  Sloc points to FOR
      --  Name (Node2) the local name
      --  Chars (Name1) the identifier name from the attribute designator
      --  Expression (Node3) the expression or name
      --  Next_Rep_Item (Node4-Sem)
      --  From_At_Mod (Flag4-Sem)

      ---------------------------------------------
      -- 13.4  Enumeration representation clause --
      ---------------------------------------------

      --  ENUMERATION_REPRESENTATION_CLAUSE ::=
      --    for first_subtype_LOCAL_NAME use ENUMERATION_AGGREGATE;

      --  In Ada 83, the name must be a direct name

      --  N_Enumeration_Representation_Clause
      --  Sloc points to FOR
      --  Identifier (Node1) direct name
      --  Array_Aggregate (Node3)
      --  Next_Rep_Item (Node4-Sem)

      ---------------------------------
      -- 13.4  Enumeration aggregate --
      ---------------------------------

      --  ENUMERATION_AGGREGATE ::= ARRAY_AGGREGATE

      ------------------------------------------
      -- 13.5.1  Record representation clause --
      ------------------------------------------

      --  RECORD_REPRESENTATION_CLAUSE ::=
      --    for first_subtype_LOCAL_NAME use
      --      record [MOD_CLAUSE]
      --        {COMPONENT_CLAUSE}
      --      end record;

      --  Gigi restriction: Mod_Clause is always Empty (if present it is
      --  replaced by a corresponding Alignment attribute definition clause).

      --  Note: Component_Clauses can include pragmas

      --  N_Record_Representation_Clause
      --  Sloc points to FOR
      --  Identifier (Node1) direct name
      --  Mod_Clause (Node2) (set to Empty if no mod clause present)
      --  Component_Clauses (List3)
      --  Next_Rep_Item (Node4-Sem)

      ------------------------------
      -- 13.5.1  Component clause --
      ------------------------------

      --  COMPONENT_CLAUSE ::=
      --    component_LOCAL_NAME at POSITION
      --      range FIRST_BIT .. LAST_BIT;

      --  N_Component_Clause
      --  Sloc points to AT
      --  Component_Name (Node1) points to Name or Attribute_Reference
      --  Position (Node2)
      --  First_Bit (Node3)
      --  Last_Bit (Node4)

      ----------------------
      -- 13.5.1  Position --
      ----------------------

      --  POSITION ::= static_EXPRESSION

      -----------------------
      -- 13.5.1  First_Bit --
      -----------------------

      --  FIRST_BIT ::= static_SIMPLE_EXPRESSION

      ----------------------
      -- 13.5.1  Last_Bit --
      ----------------------

      --  LAST_BIT ::= static_SIMPLE_EXPRESSION

      --------------------------
      -- 13.8  Code statement --
      --------------------------

      --  CODE_STATEMENT ::= QUALIFIED_EXPRESSION;

      --  Note: in GNAT, the qualified expression has the form

      --    Asm_Insn'(Asm (...));

      --      or

      --    Asm_Insn'(Asm_Volatile (...))

      --  See package System.Machine_Code in file s-maccod.ads for details
      --  on the allowed parameters to Asm[_Volatile]. There are two ways
      --  this node can arise, as a code statement, in which case the
      --  expression is the qualified expression, or as a result of the
      --  expansion of an intrinsic call to the Asm or Asm_Input procedure.

      --  N_Code_Statement
      --  Sloc points to first token of the expression
      --  Expression (Node3)

      --  Note: package Exp_Code contains an abstract functional interface
      --  for use by Gigi in accessing the data from N_Code_Statement nodes.

      ------------------------
      -- 13.12  Restriction --
      ------------------------

      --  RESTRICTION ::=
      --    restriction_IDENTIFIER
      --  | restriction_parameter_IDENTIFIER => EXPRESSION

      --  There is no explicit node for restrictions. Instead the restriction
      --  appears in normal pragma syntax as a pragma argument association,
      --  which has the same syntactic form.

      --------------------------
      -- B.2  Shift Operators --
      --------------------------

      --  Calls to the intrinsic shift functions are converted to one of
      --  the following shift nodes, which have the form of normal binary
      --  operator names. Note that for a given shift operation, one node
      --  covers all possible types, as for normal operators.

      --  Note: it is perfectly permissible for the expander to generate
      --  shift operation nodes directly, in which case they will be analyzed
      --  and parsed in the usual manner.

      --  Sprint syntax: shift-function-name!(expr, count)

      --  Note: the Left_Opnd field holds the first argument (the value to
      --  be shifted). The Right_Opnd field holds the second argument (the
      --  shift count). The Chars field is the name of the intrinsic function.

      --  N_Op_Rotate_Left
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK (Flag4-Sem)

      --  N_Op_Rotate_Right
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK (Flag4-Sem)

      --  N_Op_Shift_Left
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK (Flag4-Sem)

      --  N_Op_Shift_Right_Arithmetic
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK (Flag4-Sem)

      --  N_Op_Shift_Right
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK (Flag4-Sem)

   --------------------------
   -- Obsolescent Features --
   --------------------------

      --  The syntax descriptions and tree nodes for obsolescent features are
      --  grouped together, corresponding to their location in appendix I in
      --  the RM. However, parsing and semantic analysis for these constructs
      --  is located in an appropriate chapter (see individual notes).

      ---------------------------
      -- J.3  Delta Constraint --
      ---------------------------

      --  Note: the parse routine for this construct is located in section
      --  3.5.9 of Par-Ch3, and semantic analysis is in Sem_Ch3, which is
      --  where delta constraint logically belongs.

      --  DELTA_CONSTRAINT ::= DELTA static_EXPRESSION [RANGE_CONSTRAINT]

      --  N_Delta_Constraint
      --  Sloc points to DELTA
      --  Delta_Expression (Node3)
      --  Range_Constraint (Node4) (set to Empty if not present)

      --------------------
      -- J.7  At Clause --
      --------------------

      --  AT_CLAUSE ::= for DIRECT_NAME use at EXPRESSION;

      --  Note: the parse routine for this construct is located in Par-Ch13,
      --  and the semantic analysis is in Sem_Ch13, where at clause logically
      --  belongs if it were not obsolescent.

      --  Note: in Ada 83 the expression must be a simple expression

      --  Gigi restriction: This node never appears, it is rewritten as an
      --  address attribute definition clause.

      --  N_At_Clause
      --  Sloc points to FOR
      --  Identifier (Node1)
      --  Expression (Node3)

      ---------------------
      -- J.8  Mod clause --
      ---------------------

      --  MOD_CLAUSE ::= at mod static_EXPRESSION;

      --  Note: the parse routine for this construct is located in Par-Ch13,
      --  and the semantic analysis is in Sem_Ch13, where mod clause logically
      --  belongs if it were not obsolescent.

      --  Note: in Ada 83, the expression must be a simple expression

      --  Gigi restriction: this node never appears. It is replaced
      --  by a corresponding Alignment attribute definition clause.

      --  Note: pragmas can appear before and after the MOD_CLAUSE since
      --  its name has "clause" in it. This is rather strange, but is quite
      --  definitely specified. The pragmas before are collected in the
      --  Pragmas_Before field of the mod clause node itself, and pragmas
      --  after are simply swallowed up in the list of component clauses.

      --  N_Mod_Clause
      --  Sloc points to AT
      --  Expression (Node3)
      --  Pragmas_Before (List4) Pragmas before mod clause (No_List if none)

   --------------------
   -- Semantic Nodes --
   --------------------

   --  These semantic nodes are used to hold additional semantic information.
   --  They are inserted into the tree as a result of semantic processing.
   --  Although there are no legitimate source syntax constructions that
   --  correspond directly to these nodes, we need a source syntax for the
   --  reconstructed tree printed by Sprint, and the node descriptions here
   --  show this syntax.

      ----------------------------
      -- Conditional Expression --
      ----------------------------

      --  This node is used to represent an expression corresponding to the
      --  C construct (condition ? then-expression : else_expression), where
      --  Expressions is a three element list, whose first expression is the
      --  condition, and whose second and third expressions are the then and
      --  else expressions respectively.

      --  Note: the Then_Actions and Else_Actions fields are always set to
      --  No_List in the tree passed to Gigi. These fields are used only
      --  for temporary processing purposes in the expander.

      --  Sprint syntax: (if expr then expr else expr)

      --  N_Conditional_Expression
      --  Sloc points to related node
      --  Expressions (List1)
      --  Then_Actions (List2-Sem)
      --  Else_Actions (List3-Sem)
      --  plus fields for expression

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the IF keyword in the Sprint file output.

      -------------------
      -- Expanded_Name --
      -------------------

      --  The N_Expanded_Name node is used to represent a selected component
      --  name that has been resolved to an expanded name. The semantic phase
      --  replaces N_Selected_Component nodes that represent names by the use
      --  of this node, leaving the N_Selected_Component node used only when
      --  the prefix is a record or protected type.

      --  The fields of the N_Expanded_Name node are layed out identically
      --  to those of the N_Selected_Component node, allowing conversion of
      --  an expanded name node to a selected component node to be done
      --  easily, see Sinfo.CN.Change_Selected_Component_To_Expanded_Name.

      --  There is no special sprint syntax for an expanded name.

      --  N_Expanded_Name
      --  Sloc points to the period
      --  Chars (Name1) copy of Chars field of selector name
      --  Prefix (Node3)
      --  Selector_Name (Node2)
      --  Entity (Node4-Sem)
      --  Associated_Node (Node4-Sem)
      --  Redundant_Use (Flag13-Sem)
      --  Has_Private_View (Flag11-Sem) set in generic units.
      --  plus fields for expression

      --------------------
      -- Free Statement --
      --------------------

      --  The N_Free_Statement node is generated as a result of a call to an
      --  instantiation of Unchecked_Deallocation. The instantiation of this
      --  generic is handled specially and generates this node directly.

      --  Sprint syntax: free expression

      --  N_Free_Statement
      --  Sloc is copied from the unchecked deallocation call
      --  Expression (Node3) argument to unchecked deallocation call
      --  Storage_Pool (Node1-Sem)
      --  Procedure_To_Call (Node4-Sem)

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the FREE keyword in the Sprint file output.

      -------------------
      -- Freeze Entity --
      -------------------

      --  This node marks the point in a declarative part at which an entity
      --  declared therein becomes frozen. The expander places initialization
      --  procedures for types at those points. Gigi uses the freezing point
      --  to elaborate entities that may depend on previous private types.

      --  See the section in Einfo "Delayed Freezing and Elaboration" for
      --  a full description of the use of this node.

      --  The Entity field points back to the entity for the type (whose
      --  Freeze_Node field points back to this freeze node).

      --  The Actions field contains a list of declarations and statements
      --  generated by the expander which are associated with the freeze
      --  node, and are elaborated as though the freeze node were replaced
      --  by this sequence of actions.

      --  Note: the Sloc field in the freeze node references a construct
      --  associated with the freezing point. This is used for posting
      --  messages in some error/warning situations, e.g. the case where
      --  a primitive operation of a tagged type is declared too late.

      --  Sprint syntax: freeze entity-name [
      --                   freeze actions
      --                 ]

      --  N_Freeze_Entity
      --  Sloc points near freeze point (see above special note)
      --  Entity (Node4-Sem)
      --  Access_Types_To_Process (Elist2-Sem) (set to No_Elist if none)
      --  TSS_Elist (Elist3-Sem) (set to No_Elist if no associated TSS's)
      --  Actions (List1) (set to No_List if no freeze actions)
      --  First_Subtype_Link (Node5-Sem) (set to Empty if no link)

      --  The Actions field holds actions associated with the freeze. These
      --  actions are elaborated at the point where the type is frozen.

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the FREEZE keyword in the Sprint file output.

      --------------------------------
      -- Implicit Label Declaration --
      --------------------------------

      --  An implicit label declaration is created for every occurrence of a
      --  label on a statement or a label on a block or loop. It is chained
      --  in the declarations of the innermost enclosing block as specified
      --  in RM section 5.1 (3).

      --  The Defining_Identifier is the actual identifier for the
      --  statement identifier. Note that the occurrence of the label
      --  is a reference, NOT the defining occurrence. The defining
      --  occurrence occurs at the head of the innermost enclosing
      --  block, and is represented by this node.

      --  Note: from the grammar, this might better be called an implicit
      --  statement identifier declaration, but the term we choose seems
      --  friendlier, since at least informally statement identifiers are
      --  called labels in both cases (i.e. when used in labels, and when
      --  used as the identifiers of blocks and loops).

      --  Note: although this is logically a semantic node, since it does
      --  not correspond directly to a source syntax construction, these
      --  nodes are actually created by the parser in a post pass done just
      --  after parsing is complete, before semantic analysis is started (see
      --  the Par.Labl subunit in file par-labl.adb).

      --  Sprint syntax: labelname : label;

      --  N_Implicit_Label_Declaration
      --  Sloc points to the << of the label
      --  Defining_Identifier (Node1)
      --  Label_Construct (Node2-Sem)

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the label name in the generated declaration.

      ---------------------
      -- Itype_Reference --
      ---------------------

      --  This node is used to create a reference to an Itype. The only
      --  purpose is to make sure that the Itype is defined if this is the
      --  first reference.

      --  A typical use of this node is when an Itype is to be referenced in
      --  two branches of an if statement. In this case it is important that
      --  the first use of the Itype not be inside the conditional, since
      --  then it might not be defined if the wrong branch of the if is
      --  taken in the case where the definition generates elaboration code.

      --  The Itype field points to the referenced Itype

      --  sprint syntax: reference itype-name

      --  N_Itype_Reference
      --  Sloc points to the node generating the reference
      --  Itype (Node1-Sem)

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the REFERENCE keyword in the file output.

      ---------------------
      -- Raise_xxx_Error --
      ---------------------

      --  One of these nodes is created during semantic analysis to replace
      --  a node for an expression that is determined to definitely raise
      --  the corresponding exception.

      --  The N_Raise_xxx_Error node may also stand alone in place
      --  of a declaration or statement, in which case it simply causes
      --  the exception to be raised (i.e. it is equivalent to a raise
      --  statement that raises the corresponding exception). This use
      --  is distinguished by the fact that the Etype in this case is
      --  Standard_Void_Type, In the subexprssion case, the Etype is the
      --  same as the type of the subexpression which it replaces.

      --  If Condition is empty, then the raise is unconditional. If the
      --  Condition field is non-empty, it is a boolean expression which
      --  is first evaluated, and the exception is raised only if the
      --  value of the expression is True. In the unconditional case, the
      --  creation of this node is usually accompanied by a warning message
      --  error. The creation of this node will usually be accompanied by a
      --  message (unless it appears within the right operand of a short
      --  circuit form whose left argument is static and decisively
      --  eliminates elaboration of the raise operation.

      --  Gigi restriction: This expander ensures that the type of the
      --  Condition field is always Standard.Boolean, even if the type
      --  in the source is some non-standard boolean type.

      --  Sprint syntax: [xxx_error]
      --             or: [xxx_error when condition]

      --  N_Raise_Constraint_Error
      --  Sloc references related construct
      --  Condition (Node1) (set to Empty if no condition)
      --  Sloc is copied from the expression generating the exception
      --  plus fields for expression

      --  N_Raise_Program_Error
      --  Sloc references related construct
      --  Condition (Node1) (set to Empty if no condition)
      --  Sloc is copied from the construct generating the exception
      --  plus fields for expression

      --  N_Raise_Storage_Error
      --  Sloc references related construct
      --  Condition (Node1) (set to Empty if no condition)
      --  Sloc is copied from the construct generating the exception
      --  plus fields for expression

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the left bracket in the Sprint file output.

      ---------------
      -- Reference --
      ---------------

      --  For a number of purposes, we need to construct references to objects.
      --  These references are subsequently treated as normal access values.
      --  An example is the construction of the parameter block passed to a
      --  task entry. The N_Reference node is provided for this purpose. It is
      --  similar in effect to the use of the Unrestricted_Access attribute,
      --  and like Unrestricted_Access can be applied to objects which would
      --  not be valid prefixes for the Unchecked_Access attribute (e.g.
      --  objects which are not aliased, and slices). In addition it can be
      --  applied to composite type values as well as objects, including string
      --  values and aggregates.

      --  Note: we use the Prefix field for this expression so that the
      --  resulting node can be treated using common code with the attribute
      --  nodes for the 'Access and related attributes. Logically it would make
      --  more sense to call it an Expression field, but then we would have to
      --  special case the treatment of the N_Reference node.

      --  Sprint syntax: prefix'reference

      --  N_Reference
      --  Sloc is copied from the expression
      --  Prefix (Node3)
      --  plus fields for expression

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the quote in the Sprint file output.

      ---------------------
      -- Subprogram_Info --
      ---------------------

      --  This node generates the appropriate Subprogram_Info value for a
      --  given procedure. See Ada.Exceptions for further details

      --  Sprint syntax: subprog'subprogram_info

      --  N_Subprogram_Info
      --  Sloc points to the entity for the procedure
      --  Identifier (Node1) identifier referencing the procedure
      --  Etype (Node5-Sem) type (always set to Ada.Exceptions.Code_Loc

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the quote in the Sprint file output.

      --------------------------
      -- Unchecked Expression --
      --------------------------

      --  An unchecked expression is one that must be analyzed and resolved
      --  with all checks off, regardless of the current setting of scope
      --  suppress flags.

      --  Sprint syntax: `(expression).

      --  Note: this node is always removed from the tree (and replaced by
      --  its constituent expression) on completion of analysis, so it only
      --  appears in intermediate trees, and will never be seen by Gigi.

      --  N_Unchecked_Expression
      --  Sloc is a copy of the Sloc of the expression
      --  Expression (Node3)
      --  plus fields for expression

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the back quote in the Sprint file output.

      -------------------------------
      -- Unchecked Type Conversion --
      -------------------------------

      --  An unchecked type conversion node represents the semantic action
      --  corresponding to a call to an instantiation of Unchecked_Conversion.
      --  It is generated as a result of actual use of Unchecked_Conversion
      --  and also the expander generates unchecked type conversion nodes
      --  directly for expansion of complex semantic actions.

      --  Note: an unchecked type conversion is a variable as far as the
      --  semantics are concerned, which is convenient for the expander.
      --  This does not change what Ada source programs are legal, since
      --  clearly a function call to an instantiation of Unchecked_Conversion
      --  is not a variable in any case.

      --  Sprint syntax: subtype-mark!(expression).

      --  N_Unchecked_Type_Conversion
      --  Sloc points to related node in source
      --  Subtype_Mark (Node4)
      --  Expression (Node3)
      --  Kill_Range_Check (Flag11-Sem)
      --  plus fields for expression

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the exclamation in the Sprint file output.

      -----------------------------------
      -- Validate_Unchecked_Conversion --
      -----------------------------------

      --  The front end does most of the validation of unchecked conversion,
      --  including checking sizes (this is done after the back end is called
      --  to take advantage of back-annotation of calculated sizes).

      --  The front end also deals with specific cases that are not allowed
      --  e.g. involving unconstrained array types.

      --  For the case of the standard gigi backend, this means that all
      --  checks are done in the front-end.

      --  However, in the case of specialized back-ends, notably the JVM
      --  backend for JGNAT, additional requirements and restrictions apply
      --  to unchecked conversion, and these are most conveniently performed
      --  in the specialized back-end.

      --  To accommodate this requirement, for such back ends, the following
      --  special node is generated recording an unchecked conversion that
      --  needs to be validated. The back end should post an appropriate
      --  error message if the unchecked conversion is invalid or warrants
      --  a special warning message.

      --  Source_Type and Target_Type point to the entities for the two
      --  types involved in the unchecked conversion instantiation that
      --  is to be validated.

      --  Sprint syntax: validate Unchecked_Conversion (source, target);

      --  N_Validate_Unchecked_Conversion
      --  Sloc points to instantiation (location for warning message)
      --  Source_Type (Node1-Sem)
      --  Target_Type (Node2-Sem)

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the VALIDATE keyword in the file output.

   -----------
   -- Empty --
   -----------

   --  Used as the contents of the Nkind field of the dummy Empty node
   --  and in some other situations to indicate an uninitialized value.

   --  N_Empty
   --  Chars (Name1) is set to No_Name

   -----------
   -- Error --
   -----------

   --  Used as the contents of the Nkind field of the dummy Error node.
   --  Has an Etype field, which gets set to Any_Type later on, to help
   --  error recovery (Error_Posted is also set in the Error node).

   --  N_Error
   --  Chars (Name1) is set to Error_Name
   --  Etype (Node5-Sem)

   --------------------------
   -- Node Type Definition --
   --------------------------

   --  The following is the definition of the Node_Kind type. As previously
   --  discussed, this is separated off to allow rearrangement of the order
   --  to facilitiate definition of subtype ranges. The comments show the
   --  subtype classes which apply to each set of node kinds. The first
   --  entry in the comment characterizes the following list of nodes.

   type Node_Kind is (
      N_Unused_At_Start,

      --  N_Representation_Clause
      N_At_Clause,
      N_Component_Clause,
      N_Enumeration_Representation_Clause,
      N_Mod_Clause,
      N_Record_Representation_Clause,

      --  N_Representation_Clause, N_Has_Chars
      N_Attribute_Definition_Clause,

      --  N_Has_Chars
      N_Empty,
      N_Pragma,
      N_Pragma_Argument_Association,

      --  N_Has_Etype
      N_Error,

      --  N_Entity, N_Has_Etype, N_Has_Chars
      N_Defining_Character_Literal,
      N_Defining_Identifier,
      N_Defining_Operator_Symbol,

      --  N_Subexpr, N_Has_Etype, N_Has_Chars, N_Has_Entity
      N_Expanded_Name,

      --  N_Direct_Name, N_Subexpr, N_Has_Etype,
      --  N_Has_Chars, N_Has_Entity
      N_Identifier,
      N_Operator_Symbol,

      --  N_Direct_Name, N_Subexpr, N_Has_Etype,
      --  N_Has_Chars, N_Has_Entity
      N_Character_Literal,

      --  N_Binary_Op, N_Op, N_Subexpr,
      --  N_Has_Etype, N_Has_Chars, N_Has_Entity
      N_Op_Add,
      N_Op_Concat,
      N_Op_Divide,
      N_Op_Expon,
      N_Op_Mod,
      N_Op_Multiply,
      N_Op_Rem,
      N_Op_Subtract,

      --  N_Binary_Op, N_Op, N_Subexpr, N_Has_Etype
      --  N_Has_Entity, N_Has_Chars, N_Op_Boolean
      N_Op_And,

      --  N_Binary_Op, N_Op, N_Subexpr, N_Has_Etype
      --  N_Has_Entity, N_Has_Chars, N_Op_Boolean,
      --  N_Op_Compare
      N_Op_Eq,
      N_Op_Ge,
      N_Op_Gt,
      N_Op_Le,
      N_Op_Lt,
      N_Op_Ne,

      --  N_Binary_Op, N_Op, N_Subexpr, N_Has_Etype
      --  N_Has_Entity, N_Has_Chars, N_Op_Boolean
      N_Op_Or,
      N_Op_Xor,

      --  N_Binary_Op, N_Op, N_Subexpr, N_Has_Etype,
      --  N_Op_Shift, N_Has_Chars, N_Has_Entity
      N_Op_Rotate_Left,
      N_Op_Rotate_Right,
      N_Op_Shift_Left,
      N_Op_Shift_Right,
      N_Op_Shift_Right_Arithmetic,

      --  N_Unary_Op, N_Op, N_Subexpr, N_Has_Etype,
      --  N_Has_Chars, N_Has_Entity
      N_Op_Abs,
      N_Op_Minus,
      N_Op_Not,
      N_Op_Plus,

      --  N_Subexpr, N_Has_Etype, N_Has_Entity
      N_Attribute_Reference,

      --  N_Subexpr, N_Has_Etype
      N_And_Then,
      N_Conditional_Expression,
      N_Explicit_Dereference,
      N_Function_Call,
      N_In,
      N_Indexed_Component,
      N_Integer_Literal,
      N_Not_In,
      N_Null,
      N_Or_Else,
      N_Procedure_Call_Statement,
      N_Qualified_Expression,

      --  N_Raise_xxx_Error, N_Subexpr, N_Has_Etype

      N_Raise_Constraint_Error,
      N_Raise_Program_Error,
      N_Raise_Storage_Error,

      --  N_Subexpr, N_Has_Etype

      N_Aggregate,
      N_Allocator,
      N_Extension_Aggregate,
      N_Range,
      N_Real_Literal,
      N_Reference,
      N_Selected_Component,
      N_Slice,
      N_String_Literal,
      N_Subprogram_Info,
      N_Type_Conversion,
      N_Unchecked_Expression,
      N_Unchecked_Type_Conversion,

      --  N_Has_Etype
      N_Subtype_Indication,

      --  N_Declaration
      N_Component_Declaration,
      N_Entry_Declaration,
      N_Formal_Object_Declaration,
      N_Formal_Type_Declaration,
      N_Full_Type_Declaration,
      N_Incomplete_Type_Declaration,
      N_Loop_Parameter_Specification,
      N_Object_Declaration,
      N_Protected_Type_Declaration,
      N_Private_Extension_Declaration,
      N_Private_Type_Declaration,
      N_Subtype_Declaration,

      --  N_Subprogram_Specification, N_Declaration
      N_Function_Specification,
      N_Procedure_Specification,

      --  (nothing special)
      N_Entry_Index_Specification,
      N_Freeze_Entity,

      --  N_Access_To_Subprogram_Definition
      N_Access_Function_Definition,
      N_Access_Procedure_Definition,

      --  N_Later_Decl_Item,
      N_Task_Type_Declaration,

      --  N_Body_Stub, N_Later_Decl_Item
      N_Package_Body_Stub,
      N_Protected_Body_Stub,
      N_Subprogram_Body_Stub,
      N_Task_Body_Stub,

      --  N_Generic_Instantiation, N_Later_Decl_Item
      N_Function_Instantiation,
      N_Package_Instantiation,
      N_Procedure_Instantiation,

      --  N_Unit_Body, N_Later_Decl_Item, N_Proper_Body
      N_Package_Body,
      N_Subprogram_Body,

      --  N_Later_Decl_Item, N_Proper_Body
      N_Protected_Body,
      N_Task_Body,

      --  N_Later_Decl_Item
      N_Implicit_Label_Declaration,
      N_Package_Declaration,
      N_Single_Task_Declaration,
      N_Subprogram_Declaration,
      N_Use_Package_Clause,

      --  N_Generic_Declaration, N_Later_Decl_Item
      N_Generic_Package_Declaration,
      N_Generic_Subprogram_Declaration,

      --  N_Array_Type_Definition
      N_Constrained_Array_Definition,
      N_Unconstrained_Array_Definition,

      --  N_Renaming_Declaration
      N_Exception_Renaming_Declaration,
      N_Object_Renaming_Declaration,
      N_Package_Renaming_Declaration,
      N_Subprogram_Renaming_Declaration,

      --  N_Generic_Renaming_Declarations, N_Renaming_Declaration
      N_Generic_Function_Renaming_Declaration,
      N_Generic_Package_Renaming_Declaration,
      N_Generic_Procedure_Renaming_Declaration,

      --  N_Statement_Other_Than_Procedure_Call
      N_Abort_Statement,
      N_Accept_Statement,
      N_Assignment_Statement,
      N_Asynchronous_Select,
      N_Block_Statement,
      N_Case_Statement,
      N_Code_Statement,
      N_Conditional_Entry_Call,
      N_Delay_Relative_Statement,
      N_Delay_Until_Statement,
      N_Entry_Call_Statement,
      N_Free_Statement,
      N_Goto_Statement,
      N_Loop_Statement,
      N_Null_Statement,
      N_Raise_Statement,
      N_Requeue_Statement,
      N_Return_Statement,
      N_Selective_Accept,
      N_Timed_Entry_Call,

      --  N_Statement_Other_Than_Procedure_Call, N_Has_Condition
      N_Exit_Statement,
      N_If_Statement,

      --  N_Has_Condition
      N_Accept_Alternative,
      N_Delay_Alternative,
      N_Elsif_Part,
      N_Entry_Body_Formal_Part,
      N_Iteration_Scheme,
      N_Terminate_Alternative,

      --  Other nodes (not part of any subtype class)
      N_Abortable_Part,
      N_Abstract_Subprogram_Declaration,
      N_Access_Definition,
      N_Access_To_Object_Definition,
      N_Case_Statement_Alternative,
      N_Compilation_Unit,
      N_Compilation_Unit_Aux,
      N_Component_Association,
      N_Component_List,
      N_Derived_Type_Definition,
      N_Decimal_Fixed_Point_Definition,
      N_Defining_Program_Unit_Name,
      N_Delta_Constraint,
      N_Designator,
      N_Digits_Constraint,
      N_Discriminant_Association,
      N_Discriminant_Specification,
      N_Enumeration_Type_Definition,
      N_Entry_Body,
      N_Entry_Call_Alternative,
      N_Exception_Declaration,
      N_Exception_Handler,
      N_Floating_Point_Definition,
      N_Formal_Decimal_Fixed_Point_Definition,
      N_Formal_Derived_Type_Definition,
      N_Formal_Discrete_Type_Definition,
      N_Formal_Floating_Point_Definition,
      N_Formal_Modular_Type_Definition,
      N_Formal_Ordinary_Fixed_Point_Definition,
      N_Formal_Package_Declaration,
      N_Formal_Private_Type_Definition,
      N_Formal_Signed_Integer_Type_Definition,
      N_Formal_Subprogram_Declaration,
      N_Generic_Association,
      N_Handled_Sequence_Of_Statements,
      N_Index_Or_Discriminant_Constraint,
      N_Itype_Reference,
      N_Label,
      N_Modular_Type_Definition,
      N_Number_Declaration,
      N_Ordinary_Fixed_Point_Definition,
      N_Others_Choice,
      N_Package_Specification,
      N_Parameter_Association,
      N_Parameter_Specification,
      N_Protected_Definition,
      N_Range_Constraint,
      N_Real_Range_Specification,
      N_Record_Definition,
      N_Signed_Integer_Type_Definition,
      N_Single_Protected_Declaration,
      N_Subunit,
      N_Task_Definition,
      N_Triggering_Alternative,
      N_Use_Type_Clause,
      N_Validate_Unchecked_Conversion,
      N_Variant,
      N_Variant_Part,
      N_With_Clause,
      N_With_Type_Clause,
      N_Unused_At_End);

   for Node_Kind'Size use 8;
   --  The data structures in Atree assume this!

   ----------------------------
   -- Node Class Definitions --
   ----------------------------

   subtype N_Access_To_Subprogram_Definition is Node_Kind range
     N_Access_Function_Definition ..
     N_Access_Procedure_Definition;

   subtype N_Array_Type_Definition is Node_Kind range
     N_Constrained_Array_Definition ..
     N_Unconstrained_Array_Definition;

   subtype N_Binary_Op is Node_Kind range
     N_Op_Add ..
     N_Op_Shift_Right_Arithmetic;

   subtype N_Body_Stub is Node_Kind range
     N_Package_Body_Stub ..
     N_Task_Body_Stub;

   subtype N_Declaration is Node_Kind range
     N_Component_Declaration ..
     N_Procedure_Specification;
   --  Note: this includes all constructs normally thought of as declarations
   --  except those which are separately grouped as later declarations.

   subtype N_Direct_Name is Node_Kind range
     N_Identifier ..
     N_Character_Literal;

   subtype N_Entity is Node_Kind range
     N_Defining_Character_Literal ..
     N_Defining_Operator_Symbol;

   subtype N_Generic_Declaration is Node_Kind range
     N_Generic_Package_Declaration ..
     N_Generic_Subprogram_Declaration;

   subtype N_Generic_Instantiation is Node_Kind range
     N_Function_Instantiation ..
     N_Procedure_Instantiation;

   subtype N_Generic_Renaming_Declaration is Node_Kind range
     N_Generic_Function_Renaming_Declaration ..
     N_Generic_Procedure_Renaming_Declaration;

   subtype N_Has_Chars is Node_Kind range
     N_Attribute_Definition_Clause ..
     N_Op_Plus;

   subtype N_Has_Entity is Node_Kind range
     N_Expanded_Name ..
     N_Attribute_Reference;
   --  Nodes that have Entity fields
   --  Warning: DOES NOT INCLUDE N_Freeze_Entity!

   subtype N_Has_Etype is Node_Kind range
     N_Error ..
     N_Subtype_Indication;

   subtype N_Later_Decl_Item is Node_Kind range
     N_Task_Type_Declaration ..
     N_Generic_Subprogram_Declaration;
   --  Note: this is Ada 83 relevant only (see Ada 83 RM 3.9 (2)) and
   --  includes only those items which can appear as later declarative
   --  items. This also includes N_Implicit_Label_Declaration which is
   --  not specifically in the grammar but may appear as a valid later
   --  declarative items. It does NOT include N_Pragma which can also
   --  appear among later declarative items. It does however include
   --  N_Protected_Body, which is a bit peculiar, but harmless since
   --  this cannot appear in Ada 83 mode anyway.

   subtype N_Op is Node_Kind range
     N_Op_Add ..
     N_Op_Plus;

   subtype N_Op_Boolean is Node_Kind range
     N_Op_And ..
     N_Op_Xor;
   --  Binary operators which take operands of a boolean type, and yield
   --  a result of a boolean type.

   subtype N_Op_Compare is Node_Kind range
     N_Op_Eq ..
     N_Op_Ne;

   subtype N_Op_Shift is Node_Kind range
     N_Op_Rotate_Left ..
     N_Op_Shift_Right_Arithmetic;

   subtype N_Proper_Body is Node_Kind range
     N_Package_Body ..
     N_Task_Body;

   subtype N_Raise_xxx_Error is Node_Kind range
     N_Raise_Constraint_Error ..
     N_Raise_Storage_Error;

   subtype N_Renaming_Declaration is Node_Kind range
     N_Exception_Renaming_Declaration ..
     N_Generic_Procedure_Renaming_Declaration;

   subtype N_Representation_Clause is Node_Kind range
     N_At_Clause ..
     N_Attribute_Definition_Clause;

   subtype N_Statement_Other_Than_Procedure_Call is Node_Kind range
     N_Abort_Statement ..
     N_If_Statement;
   --  Note that this includes all statement types except for the cases of the
   --  N_Procedure_Call_Statement which is considered to be a subexpression
   --  (since overloading is possible, so it needs to go through the normal
   --  overloading resolution for expressions).

   subtype N_Has_Condition is Node_Kind range
     N_Exit_Statement ..
     N_Terminate_Alternative;
   --  Nodes with condition fields (does not include N_Raise_xxx_Error)

   subtype N_Subexpr is Node_Kind range
     N_Expanded_Name ..
     N_Unchecked_Type_Conversion;
   --  Nodes with expression fields

   subtype N_Subprogram_Specification is Node_Kind range
     N_Function_Specification ..
     N_Procedure_Specification;

   subtype N_Unary_Op is Node_Kind range
     N_Op_Abs ..
     N_Op_Plus;

   subtype N_Unit_Body is Node_Kind range
     N_Package_Body ..
     N_Subprogram_Body;

   ---------------------------
   -- Node Access Functions --
   ---------------------------

   --  The following functions return the contents of the indicated field of
   --  the node referenced by the argument, which is a Node_Id. They provide
   --  logical access to fields in the node which could be accessed using the
   --  Atree.Unchecked_Access package, but the idea is always to use these
   --  higher level routines which preserve strong typing. In debug mode,
   --  these routines check that they are being applied to an appropriate
   --  node, as well as checking that the node is in range.

   function ABE_Is_Certain
     (N : Node_Id) return Boolean;    -- Flag18

   function Abort_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Abortable_Part
     (N : Node_Id) return Node_Id;    -- Node2

   function Abstract_Present
     (N : Node_Id) return Boolean;    -- Flag4

   function Accept_Handler_Records
     (N : Node_Id) return List_Id;    -- List5

   function Accept_Statement
     (N : Node_Id) return Node_Id;    -- Node2

   function Access_Types_To_Process
     (N : Node_Id) return Elist_Id;   -- Elist2

   function Actions
     (N : Node_Id) return List_Id;    -- List1

   function Activation_Chain_Entity
     (N : Node_Id) return Node_Id;    -- Node3

   function Acts_As_Spec
     (N : Node_Id) return Boolean;    -- Flag4

   function Aggregate_Bounds
     (N : Node_Id) return Node_Id;    -- Node3

   function Aliased_Present
     (N : Node_Id) return Boolean;    -- Flag4

   function All_Others
     (N : Node_Id) return Boolean;    -- Flag11

   function All_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Alternatives
     (N : Node_Id) return List_Id;    -- List4

   function Ancestor_Part
     (N : Node_Id) return Node_Id;    -- Node3

   function Array_Aggregate
     (N : Node_Id) return Node_Id;    -- Node3

   function Assignment_OK
     (N : Node_Id) return Boolean;    -- Flag15

   function Associated_Node
     (N : Node_Id) return Node_Id;    -- Node4

   function At_End_Proc
     (N : Node_Id) return Node_Id;    -- Node1

   function Attribute_Name
     (N : Node_Id) return Name_Id;    -- Name2

   function Aux_Decls_Node
     (N : Node_Id) return Node_Id;    -- Node5

   function Backwards_OK
     (N : Node_Id) return Boolean;    -- Flag6

   function Bad_Is_Detected
     (N : Node_Id) return Boolean;    -- Flag15

   function By_Ref
     (N : Node_Id) return Boolean;    -- Flag5

   function Body_Required
     (N : Node_Id) return Boolean;    -- Flag13

   function Body_To_Inline
     (N : Node_Id) return Node_Id;    -- Node3

   function Box_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Char_Literal_Value
     (N : Node_Id) return Char_Code;  -- Char_Code2

   function Chars
     (N : Node_Id) return Name_Id;    -- Name1

   function Choice_Parameter
     (N : Node_Id) return Node_Id;    -- Node2

   function Choices
     (N : Node_Id) return List_Id;    -- List1

   function Compile_Time_Known_Aggregate
     (N : Node_Id) return Boolean;    -- Flag18

   function Component_Associations
     (N : Node_Id) return List_Id;    -- List2

   function Component_Clauses
     (N : Node_Id) return List_Id;    -- List3

   function Component_Items
     (N : Node_Id) return List_Id;    -- List3

   function Component_List
     (N : Node_Id) return Node_Id;    -- Node1

   function Component_Name
     (N : Node_Id) return Node_Id;    -- Node1

   function Condition
     (N : Node_Id) return Node_Id;    -- Node1

   function Condition_Actions
     (N : Node_Id) return List_Id;    -- List3

   function Constant_Present
     (N : Node_Id) return Boolean;    -- Flag17

   function Constraint
     (N : Node_Id) return Node_Id;    -- Node3

   function Constraints
     (N : Node_Id) return List_Id;    -- List1

   function Context_Installed
     (N : Node_Id) return Boolean;    -- Flag13

   function Context_Items
     (N : Node_Id) return List_Id;    -- List1

   function Controlling_Argument
     (N : Node_Id) return Node_Id;    -- Node1

   function Conversion_OK
     (N : Node_Id) return Boolean;    -- Flag14

   function Corresponding_Body
     (N : Node_Id) return Node_Id;    -- Node5

   function Corresponding_Generic_Association
     (N : Node_Id) return Node_Id;    -- Node5

   function Corresponding_Integer_Value
     (N : Node_Id) return Uint;       -- Uint4

   function Corresponding_Spec
     (N : Node_Id) return Node_Id;    -- Node5

   function Corresponding_Stub
     (N : Node_Id) return Node_Id;    -- Node3

   function Dcheck_Function
     (N : Node_Id) return Entity_Id;  -- Node5

   function Debug_Statement
     (N : Node_Id) return Node_Id;    -- Node3

   function Declarations
     (N : Node_Id) return List_Id;    -- List2

   function Default_Expression
     (N : Node_Id) return Node_Id;    -- Node5

   function Default_Name
     (N : Node_Id) return Node_Id;    -- Node2

   function Defining_Identifier
     (N : Node_Id) return Entity_Id;  -- Node1

   function Defining_Unit_Name
     (N : Node_Id) return Node_Id;    -- Node1

   function Delay_Alternative
     (N : Node_Id) return Node_Id;    -- Node4

   function Delay_Finalize_Attach
     (N : Node_Id) return Boolean;    -- Flag14

   function Delay_Statement
     (N : Node_Id) return Node_Id;    -- Node2

   function Delta_Expression
     (N : Node_Id) return Node_Id;    -- Node3

   function Digits_Expression
     (N : Node_Id) return Node_Id;    -- Node2

   function Discr_Check_Funcs_Built
     (N : Node_Id) return Boolean;    -- Flag11

   function Discrete_Choices
     (N : Node_Id) return List_Id;    -- List4

   function Discrete_Range
     (N : Node_Id) return Node_Id;    -- Node4

   function Discrete_Subtype_Definition
     (N : Node_Id) return Node_Id;    -- Node4

   function Discrete_Subtype_Definitions
     (N : Node_Id) return List_Id;    -- List2

   function Discriminant_Specifications
     (N : Node_Id) return List_Id;    -- List4

   function Discriminant_Type
     (N : Node_Id) return Node_Id;    -- Node5

   function Do_Access_Check
     (N : Node_Id) return Boolean;    -- Flag11

   function Do_Accessibility_Check
     (N : Node_Id) return Boolean;    -- Flag13

   function Do_Discriminant_Check
     (N : Node_Id) return Boolean;    -- Flag13

   function Do_Division_Check
     (N : Node_Id) return Boolean;    -- Flag13

   function Do_Length_Check
     (N : Node_Id) return Boolean;    -- Flag4

   function Do_Overflow_Check
     (N : Node_Id) return Boolean;    -- Flag17

   function Do_Range_Check
     (N : Node_Id) return Boolean;    -- Flag9

   function Do_Storage_Check
     (N : Node_Id) return Boolean;    -- Flag17

   function Do_Tag_Check
     (N : Node_Id) return Boolean;    -- Flag13

   function Elaborate_All_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Elaborate_Present
     (N : Node_Id) return Boolean;    -- Flag4

   function Elaboration_Boolean
     (N : Node_Id) return Node_Id;    -- Node2

   function Else_Actions
     (N : Node_Id) return List_Id;    -- List3

   function Else_Statements
     (N : Node_Id) return List_Id;    -- List4

   function Elsif_Parts
     (N : Node_Id) return List_Id;    -- List3

   function Enclosing_Variant
     (N : Node_Id) return Node_Id;    -- Node2

   function End_Label
     (N : Node_Id) return Node_Id;    -- Node4

   function End_Span
     (N : Node_Id) return Uint;       -- Uint5

   function Entity
     (N : Node_Id) return Node_Id;    -- Node4

   function Entry_Body_Formal_Part
     (N : Node_Id) return Node_Id;    -- Node5

   function Entry_Call_Alternative
     (N : Node_Id) return Node_Id;    -- Node1

   function Entry_Call_Statement
     (N : Node_Id) return Node_Id;    -- Node1

   function Entry_Direct_Name
     (N : Node_Id) return Node_Id;    -- Node1

   function Entry_Index
     (N : Node_Id) return Node_Id;    -- Node5

   function Entry_Index_Specification
     (N : Node_Id) return Node_Id;    -- Node4

   function Etype
     (N : Node_Id) return Node_Id;    -- Node5

   function Exception_Choices
     (N : Node_Id) return List_Id;    -- List4

   function Exception_Handlers
     (N : Node_Id) return List_Id;    -- List5

   function Exception_Junk
     (N : Node_Id) return Boolean;    -- Flag11

   function Explicit_Actual_Parameter
     (N : Node_Id) return Node_Id;    -- Node3

   function Expansion_Delayed
     (N : Node_Id) return Boolean;    -- Flag11

   function Explicit_Generic_Actual_Parameter
     (N : Node_Id) return Node_Id;    -- Node1

   function Expression
     (N : Node_Id) return Node_Id;    -- Node3

   function Expressions
     (N : Node_Id) return List_Id;    -- List1

   function First_Bit
     (N : Node_Id) return Node_Id;    -- Node3

   function First_Inlined_Subprogram
     (N : Node_Id) return Entity_Id;  -- Node3

   function First_Name
     (N : Node_Id) return Boolean;    -- Flag5

   function First_Named_Actual
     (N : Node_Id) return Node_Id;    -- Node4

   function First_Real_Statement
     (N : Node_Id) return Node_Id;    -- Node2

   function First_Subtype_Link
     (N : Node_Id) return Entity_Id;  -- Node5

   function Float_Truncate
     (N : Node_Id) return Boolean;    -- Flag11

   function Formal_Type_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Forwards_OK
     (N : Node_Id) return Boolean;    -- Flag5

   function From_At_Mod
     (N : Node_Id) return Boolean;    -- Flag4

   function Generic_Associations
     (N : Node_Id) return List_Id;    -- List3

   function Generic_Formal_Declarations
     (N : Node_Id) return List_Id;    -- List2

   function Generic_Parent
     (N : Node_Id) return Node_Id;    -- Node5

   function Generic_Parent_Type
     (N : Node_Id) return Node_Id;    -- Node4

   function Handled_Statement_Sequence
     (N : Node_Id) return Node_Id;    -- Node4

   function Handler_List_Entry
     (N : Node_Id) return Node_Id;    -- Node2

   function Has_Created_Identifier
     (N : Node_Id) return Boolean;    -- Flag15

   function Has_Dynamic_Length_Check
     (N : Node_Id) return Boolean;    -- Flag10

   function Has_Dynamic_Range_Check
     (N : Node_Id) return Boolean;    -- Flag12

   function Has_No_Elaboration_Code
     (N : Node_Id) return Boolean;    -- Flag17

   function Has_Priority_Pragma
     (N : Node_Id) return Boolean;    -- Flag6

   function Has_Private_View
     (N : Node_Id) return Boolean;    -- Flag11

   function Has_Storage_Size_Pragma
     (N : Node_Id) return Boolean;    -- Flag5

   function Has_Task_Info_Pragma
     (N : Node_Id) return Boolean;    -- Flag7

   function Has_Task_Name_Pragma
     (N : Node_Id) return Boolean;    -- Flag8

   function Has_Wide_Character
     (N : Node_Id) return Boolean;    -- Flag11

   function Hidden_By_Use_Clause
     (N : Node_Id) return Elist_Id;   -- Elist4

   function High_Bound
     (N : Node_Id) return Node_Id;    -- Node2

   function Identifier
     (N : Node_Id) return Node_Id;    -- Node1

   function Implicit_With
     (N : Node_Id) return Boolean;    -- Flag17

   function In_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Includes_Infinities
     (N : Node_Id) return Boolean;    -- Flag11

   function Instance_Spec
     (N : Node_Id) return Node_Id;    -- Node5

   function Intval
     (N : Node_Id) return Uint;       -- Uint3

   function Is_Asynchronous_Call_Block
     (N : Node_Id) return Boolean;    -- Flag7

   function Is_Component_Left_Opnd
     (N : Node_Id) return Boolean;    -- Flag13

   function Is_Component_Right_Opnd
     (N : Node_Id) return Boolean;    -- Flag14

   function Is_Controlling_Actual
     (N : Node_Id) return Boolean;    -- Flag16

   function Is_Machine_Number
     (N : Node_Id) return Boolean;    -- Flag11

   function Is_Overloaded
     (N : Node_Id) return Boolean;    -- Flag5

   function Is_Power_Of_2_For_Shift
     (N : Node_Id) return Boolean;    -- Flag13

   function Is_Protected_Subprogram_Body
     (N : Node_Id) return Boolean;    -- Flag7

   function Is_Static_Expression
     (N : Node_Id) return Boolean;    -- Flag6

   function Is_Subprogram_Descriptor
     (N : Node_Id) return Boolean;    -- Flag16

   function Is_Task_Allocation_Block
     (N : Node_Id) return Boolean;    -- Flag6

   function Is_Task_Master
     (N : Node_Id) return Boolean;    -- Flag5

   function Iteration_Scheme
     (N : Node_Id) return Node_Id;    -- Node2

   function Itype
     (N : Node_Id) return Entity_Id;  -- Node1

   function Kill_Range_Check
     (N : Node_Id) return Boolean;    -- Flag11

   function Label_Construct
     (N : Node_Id) return Node_Id;    -- Node2

   function Left_Opnd
     (N : Node_Id) return Node_Id;    -- Node2

   function Last_Bit
     (N : Node_Id) return Node_Id;    -- Node4

   function Last_Name
     (N : Node_Id) return Boolean;    -- Flag6

   function Library_Unit
     (N : Node_Id) return Node_Id;    -- Node4

   function Limited_Present
     (N : Node_Id) return Boolean;    -- Flag17

   function Literals
     (N : Node_Id) return List_Id;    -- List1

   function Loop_Actions
     (N : Node_Id) return List_Id;    -- List2

   function Loop_Parameter_Specification
     (N : Node_Id) return Node_Id;    -- Node4

   function Low_Bound
     (N : Node_Id) return Node_Id;    -- Node1

   function Mod_Clause
     (N : Node_Id) return Node_Id;    -- Node2

   function More_Ids
     (N : Node_Id) return Boolean;    -- Flag5

   function Must_Not_Freeze
     (N : Node_Id) return Boolean;    -- Flag8

   function Name
     (N : Node_Id) return Node_Id;    -- Node2

   function Names
     (N : Node_Id) return List_Id;    -- List2

   function Next_Entity
     (N : Node_Id) return Node_Id;    -- Node2

   function Next_Named_Actual
     (N : Node_Id) return Node_Id;    -- Node4

   function Next_Rep_Item
     (N : Node_Id) return Node_Id;    -- Node4

   function Next_Use_Clause
     (N : Node_Id) return Node_Id;    -- Node3

   function No_Ctrl_Actions
     (N : Node_Id) return Boolean;    -- Flag7

   function No_Entities_Ref_In_Spec
     (N : Node_Id) return Boolean;    -- Flag8

   function No_Initialization
     (N : Node_Id) return Boolean;    -- Flag13

   function Null_Present
     (N : Node_Id) return Boolean;    -- Flag13

   function Null_Record_Present
     (N : Node_Id) return Boolean;    -- Flag17

   function Object_Definition
     (N : Node_Id) return Node_Id;    -- Node4

   function OK_For_Stream
     (N : Node_Id) return Boolean;    -- Flag4

   function Original_Discriminant
     (N : Node_Id) return Node_Id;    -- Node2

   function Others_Discrete_Choices
     (N : Node_Id) return List_Id;    -- List1

   function Out_Present
     (N : Node_Id) return Boolean;    -- Flag17

   function Parameter_Associations
     (N : Node_Id) return List_Id;    -- List3

   function Parameter_List_Truncated
     (N : Node_Id) return Boolean;    -- Flag17

   function Parameter_Specifications
     (N : Node_Id) return List_Id;    -- List3

   function Parameter_Type
     (N : Node_Id) return Node_Id;    -- Node2

   function Parent_Spec
     (N : Node_Id) return Node_Id;    -- Node4

   function Position
     (N : Node_Id) return Node_Id;    -- Node2

   function Pragma_Argument_Associations
     (N : Node_Id) return List_Id;    -- List2

   function Pragmas_After
     (N : Node_Id) return List_Id;    -- List5

   function Pragmas_Before
     (N : Node_Id) return List_Id;    -- List4

   function Prefix
     (N : Node_Id) return Node_Id;    -- Node3

   function Present_Expr
     (N : Node_Id) return Uint;       -- Uint3

   function Prev_Ids
     (N : Node_Id) return Boolean;    -- Flag6

   function Print_In_Hex
     (N : Node_Id) return Boolean;    -- Flag13

   function Private_Declarations
     (N : Node_Id) return List_Id;    -- List3

   function Private_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Procedure_To_Call
     (N : Node_Id) return Node_Id;    -- Node4

   function Proper_Body
     (N : Node_Id) return Node_Id;    -- Node1

   function Protected_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Protected_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Raises_Constraint_Error
     (N : Node_Id) return Boolean;    -- Flag7

   function Range_Constraint
     (N : Node_Id) return Node_Id;    -- Node4

   function Range_Expression
     (N : Node_Id) return Node_Id;    -- Node4

   function Real_Range_Specification
     (N : Node_Id) return Node_Id;    -- Node4

   function Realval
     (N : Node_Id) return Ureal;      -- Ureal3

   function Record_Extension_Part
     (N : Node_Id) return Node_Id;    -- Node3

   function Redundant_Use
     (N : Node_Id) return Boolean;    -- Flag13

   function Return_Type
     (N : Node_Id) return Node_Id;    -- Node2

   function Reverse_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Right_Opnd
     (N : Node_Id) return Node_Id;    -- Node3

   function Rounded_Result
     (N : Node_Id) return Boolean;    -- Flag18

   function Scope
     (N : Node_Id) return Node_Id;    -- Node3

   function Select_Alternatives
     (N : Node_Id) return List_Id;    -- List1

   function Selector_Name
     (N : Node_Id) return Node_Id;    -- Node2

   function Selector_Names
     (N : Node_Id) return List_Id;    -- List1

   function Shift_Count_OK
     (N : Node_Id) return Boolean;    -- Flag4

   function Source_Type
     (N : Node_Id) return Entity_Id;  -- Node1

   function Specification
     (N : Node_Id) return Node_Id;    -- Node1

   function Statements
     (N : Node_Id) return List_Id;    -- List3

   function Static_Processing_OK
     (N : Node_Id) return Boolean;    -- Flag4

   function Storage_Pool
     (N : Node_Id) return Node_Id;    -- Node1

   function Strval
     (N : Node_Id) return String_Id;  -- Str3

   function Subtype_Indication
     (N : Node_Id) return Node_Id;    -- Node5

   function Subtype_Mark
     (N : Node_Id) return Node_Id;    -- Node4

   function Subtype_Marks
     (N : Node_Id) return List_Id;    -- List2

   function Tagged_Present
     (N : Node_Id) return Boolean;    -- Flag15

   function Target_Type
     (N : Node_Id) return Entity_Id;  -- Node2

   function Task_Body_Procedure
     (N : Node_Id) return Entity_Id;  -- Node2

   function Task_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Then_Actions
     (N : Node_Id) return List_Id;    -- List2

   function Then_Statements
     (N : Node_Id) return List_Id;    -- List2

   function Treat_Fixed_As_Integer
     (N : Node_Id) return Boolean;    -- Flag14

   function Triggering_Alternative
     (N : Node_Id) return Node_Id;    -- Node1

   function Triggering_Statement
     (N : Node_Id) return Node_Id;    -- Node1

   function TSS_Elist
     (N : Node_Id) return Elist_Id;   -- Elist3

   function Type_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Unit
     (N : Node_Id) return Node_Id;    -- Node2

   function Unknown_Discriminants_Present
     (N : Node_Id) return Boolean;    -- Flag13

   function Unreferenced_In_Spec
     (N : Node_Id) return Boolean;    -- Flag7

   function Variant_Part
     (N : Node_Id) return Node_Id;    -- Node4

   function Variants
     (N : Node_Id) return List_Id;    -- List1

   function Visible_Declarations
     (N : Node_Id) return List_Id;    -- List2

   function Was_Originally_Stub
     (N : Node_Id) return Boolean;    -- Flag13

   function Zero_Cost_Handling
     (N : Node_Id) return Boolean;    -- Flag5

   --  End functions (note used by xsinfo utility program to end processing)

   ----------------------------
   -- Node Update Procedures --
   ----------------------------

   --  These are the corresponding node update routines, which again provide
   --  a high level logical access with type checking. In addition to setting
   --  the indicated field of the node N to the given Val, in the case of
   --  tree pointers (List1-4), the parent pointer of the Val node is set to
   --  point back to node N. This automates the setting of the parent pointer.

   procedure Set_ABE_Is_Certain
     (N : Node_Id; Val : Boolean := True);    -- Flag18

   procedure Set_Abort_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Abortable_Part
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Abstract_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Accept_Handler_Records
     (N : Node_Id; Val : List_Id);            -- List5

   procedure Set_Accept_Statement
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Access_Types_To_Process
     (N : Node_Id; Val : Elist_Id);           -- Elist2

   procedure Set_Actions
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Activation_Chain_Entity
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Acts_As_Spec
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Aggregate_Bounds
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Aliased_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_All_Others
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_All_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Alternatives
     (N : Node_Id; Val : List_Id);            -- List4

   procedure Set_Ancestor_Part
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Array_Aggregate
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Assignment_OK
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Associated_Node
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Attribute_Name
     (N : Node_Id; Val : Name_Id);            -- Name2

   procedure Set_At_End_Proc
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Aux_Decls_Node
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Backwards_OK
     (N : Node_Id; Val : Boolean := True);    -- Flag6

   procedure Set_Bad_Is_Detected
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Body_Required
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Body_To_Inline
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Box_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_By_Ref
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_Char_Literal_Value
     (N : Node_Id; Val : Char_Code);          -- Char_Code2

   procedure Set_Chars
     (N : Node_Id; Val : Name_Id);            -- Name1

   procedure Set_Choice_Parameter
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Choices
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Compile_Time_Known_Aggregate
     (N : Node_Id; Val : Boolean := True);    -- Flag18

   procedure Set_Component_Associations
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Component_Clauses
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Component_Items
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Component_List
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Component_Name
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Condition
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Condition_Actions
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Constant_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Constraint
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Constraints
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Context_Installed
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Context_Items
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Controlling_Argument
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Conversion_OK
     (N : Node_Id; Val : Boolean := True);    -- Flag14

   procedure Set_Corresponding_Body
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Corresponding_Generic_Association
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Corresponding_Integer_Value
     (N : Node_Id; Val : Uint);               -- Uint4

   procedure Set_Corresponding_Spec
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Corresponding_Stub
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Dcheck_Function
     (N : Node_Id; Val : Entity_Id);          -- Node5

   procedure Set_Debug_Statement
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Declarations
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Default_Expression
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Default_Name
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Defining_Identifier
     (N : Node_Id; Val : Entity_Id);          -- Node1

   procedure Set_Defining_Unit_Name
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Delay_Alternative
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Delay_Finalize_Attach
     (N : Node_Id; Val : Boolean := True);    -- Flag14

   procedure Set_Delay_Statement
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Delta_Expression
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Digits_Expression
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Discr_Check_Funcs_Built
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Discrete_Choices
     (N : Node_Id; Val : List_Id);            -- List4

   procedure Set_Discrete_Range
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Discrete_Subtype_Definition
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Discrete_Subtype_Definitions
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Discriminant_Specifications
     (N : Node_Id; Val : List_Id);            -- List4

   procedure Set_Discriminant_Type
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Do_Access_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Do_Accessibility_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Do_Discriminant_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Do_Division_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Do_Length_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Do_Overflow_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Do_Range_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag9

   procedure Set_Do_Storage_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Do_Tag_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Elaborate_All_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Elaborate_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Elaboration_Boolean
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Else_Actions
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Else_Statements
     (N : Node_Id; Val : List_Id);            -- List4

   procedure Set_Elsif_Parts
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Enclosing_Variant
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_End_Label
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_End_Span
     (N : Node_Id; Val : Uint);               -- Uint5

   procedure Set_Entity
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Entry_Body_Formal_Part
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Entry_Call_Alternative
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Entry_Call_Statement
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Entry_Direct_Name
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Entry_Index
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Entry_Index_Specification
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Etype
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Exception_Choices
     (N : Node_Id; Val : List_Id);            -- List4

   procedure Set_Exception_Handlers
     (N : Node_Id; Val : List_Id);            -- List5

   procedure Set_Exception_Junk
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Expansion_Delayed
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Explicit_Actual_Parameter
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Explicit_Generic_Actual_Parameter
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Expression
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Expressions
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_First_Bit
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_First_Inlined_Subprogram
     (N : Node_Id; Val : Entity_Id);          -- Node3

   procedure Set_First_Name
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_First_Named_Actual
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_First_Real_Statement
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_First_Subtype_Link
     (N : Node_Id; Val : Entity_Id);          -- Node5

   procedure Set_Float_Truncate
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Formal_Type_Definition
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Forwards_OK
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_From_At_Mod
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Generic_Associations
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Generic_Formal_Declarations
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Generic_Parent
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Generic_Parent_Type
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Handled_Statement_Sequence
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Handler_List_Entry
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Has_Created_Identifier
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Has_Dynamic_Length_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag10

   procedure Set_Has_Dynamic_Range_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag12

   procedure Set_Has_No_Elaboration_Code
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Has_Priority_Pragma
     (N : Node_Id; Val : Boolean := True);    -- Flag6

   procedure Set_Has_Private_View
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Has_Storage_Size_Pragma
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_Has_Task_Info_Pragma
     (N : Node_Id; Val : Boolean := True);    -- Flag7

   procedure Set_Has_Task_Name_Pragma
     (N : Node_Id; Val : Boolean := True);    -- Flag8

   procedure Set_Has_Wide_Character
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Hidden_By_Use_Clause
     (N : Node_Id; Val : Elist_Id);           -- Elist4

   procedure Set_High_Bound
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Identifier
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Implicit_With
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_In_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Includes_Infinities
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Instance_Spec
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Intval
     (N : Node_Id; Val : Uint);               -- Uint3

   procedure Set_Is_Asynchronous_Call_Block
     (N : Node_Id; Val : Boolean := True);    -- Flag7

   procedure Set_Is_Component_Left_Opnd
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Is_Component_Right_Opnd
     (N : Node_Id; Val : Boolean := True);    -- Flag14

   procedure Set_Is_Controlling_Actual
     (N : Node_Id; Val : Boolean := True);    -- Flag16

   procedure Set_Is_Machine_Number
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Is_Overloaded
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_Is_Power_Of_2_For_Shift
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Is_Protected_Subprogram_Body
     (N : Node_Id; Val : Boolean := True);    -- Flag7

   procedure Set_Is_Static_Expression
     (N : Node_Id; Val : Boolean := True);    -- Flag6

   procedure Set_Is_Subprogram_Descriptor
     (N : Node_Id; Val : Boolean := True);    -- Flag16

   procedure Set_Is_Task_Allocation_Block
     (N : Node_Id; Val : Boolean := True);    -- Flag6

   procedure Set_Is_Task_Master
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_Iteration_Scheme
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Itype
     (N : Node_Id; Val : Entity_Id);          -- Node1

   procedure Set_Kill_Range_Check
     (N : Node_Id; Val : Boolean := True);    -- Flag11

   procedure Set_Last_Bit
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Last_Name
     (N : Node_Id; Val : Boolean := True);    -- Flag6

   procedure Set_Library_Unit
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Label_Construct
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Left_Opnd
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Limited_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Literals
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Loop_Actions
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Loop_Parameter_Specification
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Low_Bound
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Mod_Clause
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_More_Ids
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   procedure Set_Must_Not_Freeze
     (N : Node_Id; Val : Boolean := True);    -- Flag8

   procedure Set_Name
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Names
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Next_Entity
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Next_Named_Actual
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Next_Rep_Item
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Next_Use_Clause
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_No_Ctrl_Actions
     (N : Node_Id; Val : Boolean := True);    -- Flag7

   procedure Set_No_Entities_Ref_In_Spec
     (N : Node_Id; Val : Boolean := True);    -- Flag8

   procedure Set_No_Initialization
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Null_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Null_Record_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Object_Definition
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_OK_For_Stream
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Original_Discriminant
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Others_Discrete_Choices
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Out_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Parameter_Associations
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Parameter_List_Truncated
     (N : Node_Id; Val : Boolean := True);    -- Flag17

   procedure Set_Parameter_Specifications
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Parameter_Type
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Parent_Spec
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Position
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Pragma_Argument_Associations
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Pragmas_After
     (N : Node_Id; Val : List_Id);            -- List5

   procedure Set_Pragmas_Before
     (N : Node_Id; Val : List_Id);            -- List4

   procedure Set_Prefix
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Present_Expr
     (N : Node_Id; Val : Uint);               -- Uint3

   procedure Set_Prev_Ids
     (N : Node_Id; Val : Boolean := True);    -- Flag6

   procedure Set_Print_In_Hex
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Private_Declarations
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Private_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Procedure_To_Call
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Proper_Body
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Protected_Definition
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Protected_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Raises_Constraint_Error
     (N : Node_Id; Val : Boolean := True);    -- Flag7

   procedure Set_Range_Constraint
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Range_Expression
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Real_Range_Specification
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Realval
     (N : Node_Id; Val : Ureal);              -- Ureal3

   procedure Set_Record_Extension_Part
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Redundant_Use
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Return_Type
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Reverse_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Right_Opnd
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Rounded_Result
     (N : Node_Id; Val : Boolean := True);    -- Flag18

   procedure Set_Scope
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Select_Alternatives
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Selector_Name
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Selector_Names
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Shift_Count_OK
     (N : Node_Id; Val : Boolean := True);    -- Flag4

   procedure Set_Source_Type
     (N : Node_Id; Val : Entity_Id);          -- Node1

   procedure Set_Specification
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Statements
     (N : Node_Id; Val : List_Id);            -- List3

   procedure Set_Static_Processing_OK
     (N : Node_Id; Val : Boolean);            -- Flag4

   procedure Set_Storage_Pool
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Strval
     (N : Node_Id; Val : String_Id);          -- Str3

   procedure Set_Subtype_Indication
     (N : Node_Id; Val : Node_Id);            -- Node5

   procedure Set_Subtype_Mark
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Subtype_Marks
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Tagged_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag15

   procedure Set_Target_Type
     (N : Node_Id; Val : Entity_Id);          -- Node2

   procedure Set_Task_Body_Procedure
     (N : Node_Id; Val : Entity_Id);          -- Node2

   procedure Set_Task_Definition
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Then_Actions
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Then_Statements
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Treat_Fixed_As_Integer
     (N : Node_Id; Val : Boolean := True);    -- Flag14

   procedure Set_Triggering_Alternative
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_Triggering_Statement
     (N : Node_Id; Val : Node_Id);            -- Node1

   procedure Set_TSS_Elist
     (N : Node_Id; Val : Elist_Id);           -- Elist3

   procedure Set_Type_Definition
     (N : Node_Id; Val : Node_Id);            -- Node3

   procedure Set_Unit
     (N : Node_Id; Val : Node_Id);            -- Node2

   procedure Set_Unknown_Discriminants_Present
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Unreferenced_In_Spec
     (N : Node_Id; Val : Boolean := True);    -- Flag7

   procedure Set_Variant_Part
     (N : Node_Id; Val : Node_Id);            -- Node4

   procedure Set_Variants
     (N : Node_Id; Val : List_Id);            -- List1

   procedure Set_Visible_Declarations
     (N : Node_Id; Val : List_Id);            -- List2

   procedure Set_Was_Originally_Stub
     (N : Node_Id; Val : Boolean := True);    -- Flag13

   procedure Set_Zero_Cost_Handling
     (N : Node_Id; Val : Boolean := True);    -- Flag5

   -------------------------
   -- Iterator Procedures --
   -------------------------

   --  The call to Next_xxx (N) is equivalent to N := Next_xxx (N)

   procedure Next_Entity       (N : in out Node_Id);
   procedure Next_Named_Actual (N : in out Node_Id);
   procedure Next_Rep_Item     (N : in out Node_Id);
   procedure Next_Use_Clause   (N : in out Node_Id);

   --------------------------------------
   -- Logical Access to End_Span Field --
   --------------------------------------

   function End_Location (N : Node_Id) return Source_Ptr;
   --  N is an N_If_Statement or N_Case_Statement node, and this
   --  function returns the location of the IF token in the END IF
   --  sequence by translating the value of the End_Span field.

   procedure Set_End_Location (N : Node_Id; S : Source_Ptr);
   --  N is an N_If_Statement or N_Case_Statement node. This procedure
   --  sets the End_Span field to correspond to the given value S. In
   --  other words, End_Span is set to the difference between S and
   --  Sloc (N), the starting location.

   --------------------
   -- Inline Pragmas --
   --------------------

   pragma Inline (ABE_Is_Certain);
   pragma Inline (Abort_Present);
   pragma Inline (Abortable_Part);
   pragma Inline (Abstract_Present);
   pragma Inline (Accept_Handler_Records);
   pragma Inline (Accept_Statement);
   pragma Inline (Access_Types_To_Process);
   pragma Inline (Actions);
   pragma Inline (Activation_Chain_Entity);
   pragma Inline (Acts_As_Spec);
   pragma Inline (Aggregate_Bounds);
   pragma Inline (Aliased_Present);
   pragma Inline (All_Others);
   pragma Inline (All_Present);
   pragma Inline (Alternatives);
   pragma Inline (Ancestor_Part);
   pragma Inline (Array_Aggregate);
   pragma Inline (Assignment_OK);
   pragma Inline (Associated_Node);
   pragma Inline (At_End_Proc);
   pragma Inline (Attribute_Name);
   pragma Inline (Aux_Decls_Node);
   pragma Inline (Backwards_OK);
   pragma Inline (Bad_Is_Detected);
   pragma Inline (Body_To_Inline);
   pragma Inline (Body_Required);
   pragma Inline (By_Ref);
   pragma Inline (Box_Present);
   pragma Inline (Char_Literal_Value);
   pragma Inline (Chars);
   pragma Inline (Choice_Parameter);
   pragma Inline (Choices);
   pragma Inline (Compile_Time_Known_Aggregate);
   pragma Inline (Component_Associations);
   pragma Inline (Component_Clauses);
   pragma Inline (Component_Items);
   pragma Inline (Component_List);
   pragma Inline (Component_Name);
   pragma Inline (Condition);
   pragma Inline (Condition_Actions);
   pragma Inline (Constant_Present);
   pragma Inline (Constraint);
   pragma Inline (Constraints);
   pragma Inline (Context_Installed);
   pragma Inline (Context_Items);
   pragma Inline (Controlling_Argument);
   pragma Inline (Conversion_OK);
   pragma Inline (Corresponding_Body);
   pragma Inline (Corresponding_Generic_Association);
   pragma Inline (Corresponding_Integer_Value);
   pragma Inline (Corresponding_Spec);
   pragma Inline (Corresponding_Stub);
   pragma Inline (Dcheck_Function);
   pragma Inline (Debug_Statement);
   pragma Inline (Declarations);
   pragma Inline (Default_Expression);
   pragma Inline (Default_Name);
   pragma Inline (Defining_Identifier);
   pragma Inline (Defining_Unit_Name);
   pragma Inline (Delay_Alternative);
   pragma Inline (Delay_Finalize_Attach);
   pragma Inline (Delay_Statement);
   pragma Inline (Delta_Expression);
   pragma Inline (Digits_Expression);
   pragma Inline (Discr_Check_Funcs_Built);
   pragma Inline (Discrete_Choices);
   pragma Inline (Discrete_Range);
   pragma Inline (Discrete_Subtype_Definition);
   pragma Inline (Discrete_Subtype_Definitions);
   pragma Inline (Discriminant_Specifications);
   pragma Inline (Discriminant_Type);
   pragma Inline (Do_Access_Check);
   pragma Inline (Do_Accessibility_Check);
   pragma Inline (Do_Discriminant_Check);
   pragma Inline (Do_Length_Check);
   pragma Inline (Do_Division_Check);
   pragma Inline (Do_Overflow_Check);
   pragma Inline (Do_Range_Check);
   pragma Inline (Do_Storage_Check);
   pragma Inline (Do_Tag_Check);
   pragma Inline (Elaborate_Present);
   pragma Inline (Elaborate_All_Present);
   pragma Inline (Elaboration_Boolean);
   pragma Inline (Else_Actions);
   pragma Inline (Else_Statements);
   pragma Inline (Elsif_Parts);
   pragma Inline (Enclosing_Variant);
   pragma Inline (End_Label);
   pragma Inline (End_Span);
   pragma Inline (Entity);
   pragma Inline (Entry_Body_Formal_Part);
   pragma Inline (Entry_Call_Alternative);
   pragma Inline (Entry_Call_Statement);
   pragma Inline (Entry_Direct_Name);
   pragma Inline (Entry_Index);
   pragma Inline (Entry_Index_Specification);
   pragma Inline (Etype);
   pragma Inline (Exception_Choices);
   pragma Inline (Exception_Junk);
   pragma Inline (Exception_Handlers);
   pragma Inline (Expansion_Delayed);
   pragma Inline (Explicit_Actual_Parameter);
   pragma Inline (Explicit_Generic_Actual_Parameter);
   pragma Inline (Expression);
   pragma Inline (Expressions);
   pragma Inline (First_Bit);
   pragma Inline (First_Inlined_Subprogram);
   pragma Inline (First_Name);
   pragma Inline (First_Named_Actual);
   pragma Inline (First_Real_Statement);
   pragma Inline (First_Subtype_Link);
   pragma Inline (Float_Truncate);
   pragma Inline (Formal_Type_Definition);
   pragma Inline (Forwards_OK);
   pragma Inline (From_At_Mod);
   pragma Inline (Generic_Associations);
   pragma Inline (Generic_Formal_Declarations);
   pragma Inline (Generic_Parent);
   pragma Inline (Generic_Parent_Type);
   pragma Inline (Handled_Statement_Sequence);
   pragma Inline (Handler_List_Entry);
   pragma Inline (Has_Created_Identifier);
   pragma Inline (Has_Dynamic_Length_Check);
   pragma Inline (Has_Dynamic_Range_Check);
   pragma Inline (Has_No_Elaboration_Code);
   pragma Inline (Has_Priority_Pragma);
   pragma Inline (Has_Private_View);
   pragma Inline (Has_Storage_Size_Pragma);
   pragma Inline (Has_Task_Info_Pragma);
   pragma Inline (Has_Task_Name_Pragma);
   pragma Inline (Has_Wide_Character);
   pragma Inline (Hidden_By_Use_Clause);
   pragma Inline (High_Bound);
   pragma Inline (Identifier);
   pragma Inline (Implicit_With);
   pragma Inline (Includes_Infinities);
   pragma Inline (In_Present);
   pragma Inline (Instance_Spec);
   pragma Inline (Intval);
   pragma Inline (Is_Asynchronous_Call_Block);
   pragma Inline (Is_Component_Left_Opnd);
   pragma Inline (Is_Component_Right_Opnd);
   pragma Inline (Is_Controlling_Actual);
   pragma Inline (Is_Machine_Number);
   pragma Inline (Is_Overloaded);
   pragma Inline (Is_Power_Of_2_For_Shift);
   pragma Inline (Is_Protected_Subprogram_Body);
   pragma Inline (Is_Static_Expression);
   pragma Inline (Is_Subprogram_Descriptor);
   pragma Inline (Is_Task_Allocation_Block);
   pragma Inline (Is_Task_Master);
   pragma Inline (Iteration_Scheme);
   pragma Inline (Itype);
   pragma Inline (Kill_Range_Check);
   pragma Inline (Last_Bit);
   pragma Inline (Last_Name);
   pragma Inline (Library_Unit);
   pragma Inline (Label_Construct);
   pragma Inline (Left_Opnd);
   pragma Inline (Limited_Present);
   pragma Inline (Literals);
   pragma Inline (Loop_Actions);
   pragma Inline (Loop_Parameter_Specification);
   pragma Inline (Low_Bound);
   pragma Inline (Mod_Clause);
   pragma Inline (More_Ids);
   pragma Inline (Must_Not_Freeze);
   pragma Inline (Name);
   pragma Inline (Names);
   pragma Inline (Next_Entity);
   pragma Inline (Next_Named_Actual);
   pragma Inline (Next_Rep_Item);
   pragma Inline (Next_Use_Clause);
   pragma Inline (No_Ctrl_Actions);
   pragma Inline (No_Entities_Ref_In_Spec);
   pragma Inline (No_Initialization);
   pragma Inline (Null_Present);
   pragma Inline (Null_Record_Present);
   pragma Inline (Object_Definition);
   pragma Inline (OK_For_Stream);
   pragma Inline (Original_Discriminant);
   pragma Inline (Others_Discrete_Choices);
   pragma Inline (Out_Present);
   pragma Inline (Parameter_Associations);
   pragma Inline (Parameter_Specifications);
   pragma Inline (Parameter_List_Truncated);
   pragma Inline (Parameter_Type);
   pragma Inline (Parent_Spec);
   pragma Inline (Position);
   pragma Inline (Pragma_Argument_Associations);
   pragma Inline (Pragmas_After);
   pragma Inline (Pragmas_Before);
   pragma Inline (Prefix);
   pragma Inline (Present_Expr);
   pragma Inline (Prev_Ids);
   pragma Inline (Print_In_Hex);
   pragma Inline (Private_Declarations);
   pragma Inline (Private_Present);
   pragma Inline (Procedure_To_Call);
   pragma Inline (Proper_Body);
   pragma Inline (Protected_Definition);
   pragma Inline (Protected_Present);
   pragma Inline (Raises_Constraint_Error);
   pragma Inline (Range_Constraint);
   pragma Inline (Range_Expression);
   pragma Inline (Realval);
   pragma Inline (Real_Range_Specification);
   pragma Inline (Record_Extension_Part);
   pragma Inline (Redundant_Use);
   pragma Inline (Return_Type);
   pragma Inline (Reverse_Present);
   pragma Inline (Right_Opnd);
   pragma Inline (Rounded_Result);
   pragma Inline (Scope);
   pragma Inline (Select_Alternatives);
   pragma Inline (Selector_Name);
   pragma Inline (Selector_Names);
   pragma Inline (Shift_Count_OK);
   pragma Inline (Source_Type);
   pragma Inline (Specification);
   pragma Inline (Statements);
   pragma Inline (Static_Processing_OK);
   pragma Inline (Storage_Pool);
   pragma Inline (Strval);
   pragma Inline (Subtype_Indication);
   pragma Inline (Subtype_Mark);
   pragma Inline (Subtype_Marks);
   pragma Inline (Tagged_Present);
   pragma Inline (Target_Type);
   pragma Inline (Task_Body_Procedure);
   pragma Inline (Task_Definition);
   pragma Inline (Then_Actions);
   pragma Inline (Then_Statements);
   pragma Inline (Triggering_Alternative);
   pragma Inline (Triggering_Statement);
   pragma Inline (Treat_Fixed_As_Integer);
   pragma Inline (TSS_Elist);
   pragma Inline (Type_Definition);
   pragma Inline (Unit);
   pragma Inline (Unknown_Discriminants_Present);
   pragma Inline (Unreferenced_In_Spec);
   pragma Inline (Variant_Part);
   pragma Inline (Variants);
   pragma Inline (Visible_Declarations);
   pragma Inline (Was_Originally_Stub);
   pragma Inline (Zero_Cost_Handling);

   pragma Inline (Set_ABE_Is_Certain);
   pragma Inline (Set_Abort_Present);
   pragma Inline (Set_Abortable_Part);
   pragma Inline (Set_Abstract_Present);
   pragma Inline (Set_Accept_Handler_Records);
   pragma Inline (Set_Accept_Statement);
   pragma Inline (Set_Access_Types_To_Process);
   pragma Inline (Set_Actions);
   pragma Inline (Set_Activation_Chain_Entity);
   pragma Inline (Set_Acts_As_Spec);
   pragma Inline (Set_Aggregate_Bounds);
   pragma Inline (Set_Aliased_Present);
   pragma Inline (Set_All_Others);
   pragma Inline (Set_All_Present);
   pragma Inline (Set_Alternatives);
   pragma Inline (Set_Ancestor_Part);
   pragma Inline (Set_Array_Aggregate);
   pragma Inline (Set_Assignment_OK);
   pragma Inline (Set_Associated_Node);
   pragma Inline (Set_At_End_Proc);
   pragma Inline (Set_Attribute_Name);
   pragma Inline (Set_Aux_Decls_Node);
   pragma Inline (Set_Backwards_OK);
   pragma Inline (Set_Bad_Is_Detected);
   pragma Inline (Set_Body_To_Inline);
   pragma Inline (Set_Body_Required);
   pragma Inline (Set_By_Ref);
   pragma Inline (Set_Box_Present);
   pragma Inline (Set_Char_Literal_Value);
   pragma Inline (Set_Chars);
   pragma Inline (Set_Choice_Parameter);
   pragma Inline (Set_Choices);
   pragma Inline (Set_Compile_Time_Known_Aggregate);
   pragma Inline (Set_Component_Associations);
   pragma Inline (Set_Component_Clauses);
   pragma Inline (Set_Component_Items);
   pragma Inline (Set_Component_List);
   pragma Inline (Set_Component_Name);
   pragma Inline (Set_Condition);
   pragma Inline (Set_Condition_Actions);
   pragma Inline (Set_Constant_Present);
   pragma Inline (Set_Constraint);
   pragma Inline (Set_Constraints);
   pragma Inline (Set_Context_Installed);
   pragma Inline (Set_Context_Items);
   pragma Inline (Set_Controlling_Argument);
   pragma Inline (Set_Conversion_OK);
   pragma Inline (Set_Corresponding_Body);
   pragma Inline (Set_Corresponding_Generic_Association);
   pragma Inline (Set_Corresponding_Integer_Value);
   pragma Inline (Set_Corresponding_Spec);
   pragma Inline (Set_Corresponding_Stub);
   pragma Inline (Set_Dcheck_Function);
   pragma Inline (Set_Debug_Statement);
   pragma Inline (Set_Declarations);
   pragma Inline (Set_Default_Expression);
   pragma Inline (Set_Default_Name);
   pragma Inline (Set_Defining_Identifier);
   pragma Inline (Set_Defining_Unit_Name);
   pragma Inline (Set_Delay_Alternative);
   pragma Inline (Set_Delay_Finalize_Attach);
   pragma Inline (Set_Delay_Statement);
   pragma Inline (Set_Delta_Expression);
   pragma Inline (Set_Digits_Expression);
   pragma Inline (Set_Discr_Check_Funcs_Built);
   pragma Inline (Set_Discrete_Choices);
   pragma Inline (Set_Discrete_Range);
   pragma Inline (Set_Discrete_Subtype_Definition);
   pragma Inline (Set_Discrete_Subtype_Definitions);
   pragma Inline (Set_Discriminant_Specifications);
   pragma Inline (Set_Discriminant_Type);
   pragma Inline (Set_Do_Access_Check);
   pragma Inline (Set_Do_Accessibility_Check);
   pragma Inline (Set_Do_Discriminant_Check);
   pragma Inline (Set_Do_Length_Check);
   pragma Inline (Set_Do_Division_Check);
   pragma Inline (Set_Do_Overflow_Check);
   pragma Inline (Set_Do_Range_Check);
   pragma Inline (Set_Do_Storage_Check);
   pragma Inline (Set_Do_Tag_Check);
   pragma Inline (Set_Elaborate_Present);
   pragma Inline (Set_Elaborate_All_Present);
   pragma Inline (Set_Elaboration_Boolean);
   pragma Inline (Set_Else_Actions);
   pragma Inline (Set_Else_Statements);
   pragma Inline (Set_Elsif_Parts);
   pragma Inline (Set_Enclosing_Variant);
   pragma Inline (Set_End_Label);
   pragma Inline (Set_End_Span);
   pragma Inline (Set_Entity);
   pragma Inline (Set_Entry_Body_Formal_Part);
   pragma Inline (Set_Entry_Call_Alternative);
   pragma Inline (Set_Entry_Call_Statement);
   pragma Inline (Set_Entry_Direct_Name);
   pragma Inline (Set_Entry_Index);
   pragma Inline (Set_Entry_Index_Specification);
   pragma Inline (Set_Etype);
   pragma Inline (Set_Exception_Choices);
   pragma Inline (Set_Exception_Junk);
   pragma Inline (Set_Exception_Handlers);
   pragma Inline (Set_Expansion_Delayed);
   pragma Inline (Set_Explicit_Actual_Parameter);
   pragma Inline (Set_Explicit_Generic_Actual_Parameter);
   pragma Inline (Set_Expression);
   pragma Inline (Set_Expressions);
   pragma Inline (Set_First_Bit);
   pragma Inline (Set_First_Inlined_Subprogram);
   pragma Inline (Set_First_Name);
   pragma Inline (Set_First_Named_Actual);
   pragma Inline (Set_First_Real_Statement);
   pragma Inline (Set_First_Subtype_Link);
   pragma Inline (Set_Float_Truncate);
   pragma Inline (Set_Formal_Type_Definition);
   pragma Inline (Set_Forwards_OK);
   pragma Inline (Set_From_At_Mod);
   pragma Inline (Set_Generic_Associations);
   pragma Inline (Set_Generic_Formal_Declarations);
   pragma Inline (Set_Generic_Parent);
   pragma Inline (Set_Generic_Parent_Type);
   pragma Inline (Set_Handled_Statement_Sequence);
   pragma Inline (Set_Handler_List_Entry);
   pragma Inline (Set_Has_Created_Identifier);
   pragma Inline (Set_Has_Dynamic_Length_Check);
   pragma Inline (Set_Has_Dynamic_Range_Check);
   pragma Inline (Set_Has_No_Elaboration_Code);
   pragma Inline (Set_Has_Priority_Pragma);
   pragma Inline (Set_Has_Private_View);
   pragma Inline (Set_Has_Storage_Size_Pragma);
   pragma Inline (Set_Has_Task_Info_Pragma);
   pragma Inline (Set_Has_Task_Name_Pragma);
   pragma Inline (Set_Has_Wide_Character);
   pragma Inline (Set_Hidden_By_Use_Clause);
   pragma Inline (Set_High_Bound);
   pragma Inline (Set_Identifier);
   pragma Inline (Set_Implicit_With);
   pragma Inline (Set_Includes_Infinities);
   pragma Inline (Set_In_Present);
   pragma Inline (Set_Instance_Spec);
   pragma Inline (Set_Intval);
   pragma Inline (Set_Is_Asynchronous_Call_Block);
   pragma Inline (Set_Is_Component_Left_Opnd);
   pragma Inline (Set_Is_Component_Right_Opnd);
   pragma Inline (Set_Is_Controlling_Actual);
   pragma Inline (Set_Is_Machine_Number);
   pragma Inline (Set_Is_Overloaded);
   pragma Inline (Set_Is_Power_Of_2_For_Shift);
   pragma Inline (Set_Is_Protected_Subprogram_Body);
   pragma Inline (Set_Is_Static_Expression);
   pragma Inline (Set_Is_Subprogram_Descriptor);
   pragma Inline (Set_Is_Task_Allocation_Block);
   pragma Inline (Set_Is_Task_Master);
   pragma Inline (Set_Iteration_Scheme);
   pragma Inline (Set_Itype);
   pragma Inline (Set_Kill_Range_Check);
   pragma Inline (Set_Last_Bit);
   pragma Inline (Set_Last_Name);
   pragma Inline (Set_Library_Unit);
   pragma Inline (Set_Label_Construct);
   pragma Inline (Set_Left_Opnd);
   pragma Inline (Set_Limited_Present);
   pragma Inline (Set_Literals);
   pragma Inline (Set_Loop_Actions);
   pragma Inline (Set_Loop_Parameter_Specification);
   pragma Inline (Set_Low_Bound);
   pragma Inline (Set_Mod_Clause);
   pragma Inline (Set_More_Ids);
   pragma Inline (Set_Must_Not_Freeze);
   pragma Inline (Set_Name);
   pragma Inline (Set_Names);
   pragma Inline (Set_Next_Entity);
   pragma Inline (Set_Next_Named_Actual);
   pragma Inline (Set_Next_Use_Clause);
   pragma Inline (Set_No_Ctrl_Actions);
   pragma Inline (Set_No_Entities_Ref_In_Spec);
   pragma Inline (Set_No_Initialization);
   pragma Inline (Set_Null_Present);
   pragma Inline (Set_Null_Record_Present);
   pragma Inline (Set_Object_Definition);
   pragma Inline (Set_OK_For_Stream);
   pragma Inline (Set_Original_Discriminant);
   pragma Inline (Set_Others_Discrete_Choices);
   pragma Inline (Set_Out_Present);
   pragma Inline (Set_Parameter_Associations);
   pragma Inline (Set_Parameter_Specifications);
   pragma Inline (Set_Parameter_List_Truncated);
   pragma Inline (Set_Parameter_Type);
   pragma Inline (Set_Parent_Spec);
   pragma Inline (Set_Position);
   pragma Inline (Set_Pragma_Argument_Associations);
   pragma Inline (Set_Pragmas_After);
   pragma Inline (Set_Pragmas_Before);
   pragma Inline (Set_Prefix);
   pragma Inline (Set_Present_Expr);
   pragma Inline (Set_Prev_Ids);
   pragma Inline (Set_Print_In_Hex);
   pragma Inline (Set_Private_Declarations);
   pragma Inline (Set_Private_Present);
   pragma Inline (Set_Procedure_To_Call);
   pragma Inline (Set_Proper_Body);
   pragma Inline (Set_Protected_Definition);
   pragma Inline (Set_Protected_Present);
   pragma Inline (Set_Raises_Constraint_Error);
   pragma Inline (Set_Range_Constraint);
   pragma Inline (Set_Range_Expression);
   pragma Inline (Set_Realval);
   pragma Inline (Set_Real_Range_Specification);
   pragma Inline (Set_Record_Extension_Part);
   pragma Inline (Set_Redundant_Use);
   pragma Inline (Set_Return_Type);
   pragma Inline (Set_Reverse_Present);
   pragma Inline (Set_Right_Opnd);
   pragma Inline (Set_Rounded_Result);
   pragma Inline (Set_Scope);
   pragma Inline (Set_Select_Alternatives);
   pragma Inline (Set_Selector_Name);
   pragma Inline (Set_Selector_Names);
   pragma Inline (Set_Shift_Count_OK);
   pragma Inline (Set_Source_Type);
   pragma Inline (Set_Specification);
   pragma Inline (Set_Statements);
   pragma Inline (Set_Static_Processing_OK);
   pragma Inline (Set_Storage_Pool);
   pragma Inline (Set_Strval);
   pragma Inline (Set_Subtype_Indication);
   pragma Inline (Set_Subtype_Mark);
   pragma Inline (Set_Subtype_Marks);
   pragma Inline (Set_Tagged_Present);
   pragma Inline (Set_Target_Type);
   pragma Inline (Set_Task_Body_Procedure);
   pragma Inline (Set_Task_Definition);
   pragma Inline (Set_Then_Actions);
   pragma Inline (Set_Then_Statements);
   pragma Inline (Set_Triggering_Alternative);
   pragma Inline (Set_Triggering_Statement);
   pragma Inline (Set_Treat_Fixed_As_Integer);
   pragma Inline (Set_TSS_Elist);
   pragma Inline (Set_Type_Definition);
   pragma Inline (Set_Unit);
   pragma Inline (Set_Unknown_Discriminants_Present);
   pragma Inline (Set_Unreferenced_In_Spec);
   pragma Inline (Set_Variant_Part);
   pragma Inline (Set_Variants);
   pragma Inline (Set_Visible_Declarations);
   pragma Inline (Set_Was_Originally_Stub);
   pragma Inline (Set_Zero_Cost_Handling);

end Sinfo;
