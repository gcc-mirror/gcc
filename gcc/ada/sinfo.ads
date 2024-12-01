------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S I N F O                                 --
--                                                                          --
--                                 S p e c                                  --
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

--  This package documents the structure of the abstract syntax tree. The Atree
--  package provides a basic tree structure. Sinfo describes how this structure
--  is used to represent the syntax of an Ada program.

--  The grammar in the RM is followed very closely in the tree design, and is
--  repeated as part of this source file.

--  The tree contains not only the full syntactic representation of the
--  program, but also the results of semantic analysis. In particular, the
--  nodes for defining identifiers, defining character literals, and defining
--  operator symbols, collectively referred to as entities, represent what
--  would normally be regarded as the symbol table information. In addition a
--  number of the tree nodes contain semantic information.

--  See the spec of Gen_IL.Gen for instructions on making changes to this file.
--  Note that the official definition of what nodes have what fields is in
--  Gen_IL.Gen.Gen_Nodes; if there is a discrepancy between that and the
--  comments here, Gen_IL.Gen.Gen_Nodes wins.

pragma Warnings (Off); -- with/use clauses for children
with Namet;  use Namet;
with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;
pragma Warnings (On);

package Sinfo is

   ----------------------------------------
   -- Definitions of fields in tree node --
   ----------------------------------------

   --  The following fields are common to all nodes:

   --   Nkind         Indicates the kind of the node. This field is present
   --                 in all nodes.

   --   Sloc          Location (Source_Ptr) of the corresponding token
   --                 in the Source buffer. The individual node definitions
   --                 show which token is referenced by this pointer.

   --   In_List       A flag used to indicate if the node is a member
   --                 of a node list (see package Nlists).

   --   Rewrite_Ins   A flag set if a node is marked as a rewrite inserted
   --                 node as a result of a call to Mark_Rewrite_Insertion.

   --   Small_Paren_Count
   --                 A 2-bit count used in subexpression nodes to indicate
   --                 the level of parentheses. The settings are 0,1,2 and
   --                 3 for many. If the value is 3, then an auxiliary table
   --                 is used to indicate the real value, which is computed by
   --                 Paren_Count. Set to zero for nonsubexpression nodes.

   --                 Note: the required parentheses surrounding conditional
   --                 and quantified expressions count as a level of parens
   --                 for this purpose, so e.g. in X := (if A then B else C);
   --                 Paren_Count for the right side will be 1.

   --   Comes_From_Source
   --                 This flag is present in all nodes. It is set if the
   --                 node is built by the scanner or parser, and clear if
   --                 the node is built by the analyzer or expander. It
   --                 indicates that the node corresponds to a construct
   --                 that appears in the original source program.

   --   Analyzed      This flag is present in all nodes. It is set when
   --                 a node is analyzed, and is used to avoid analyzing
   --                 the same node twice. Analysis includes expansion if
   --                 expansion is active, so in this case if the flag is
   --                 set it means the node has been analyzed and expanded.

   --   Error_Posted  This flag is present in all nodes. It is set when
   --                 an error message is posted which is associated with
   --                 the flagged node. This is used to avoid posting more
   --                 than one message on the same node.

   --   Link          For a node, points to the Parent. For a list, points
   --                 to the list header. Note that in the latter case, a
   --                 client cannot modify the link field. This field is
   --                 private to the Atree package (but is also modified
   --                 by the Nlists package).

   --  The following additional fields are common to all entities (that is,
   --  nodes whose Nkind is in N_Entity):

   --   Ekind         Entity type.

   --   Convention    Entity convention (Convention_Id value)

   --------------------------------
   -- Implicit Nodes in the Tree --
   --------------------------------

   --  Generally the structure of the tree very closely follows the grammar as
   --  defined in the RM. However, certain nodes are omitted to save space and
   --  simplify semantic processing. Two general classes of such omitted nodes
   --  are as follows:

   --   If the only possibilities for a non-terminal are one or more other
   --   non-terminals (i.e. the rule is a "skinny" rule), then usually the
   --   corresponding node is omitted from the tree, and the target construct
   --   appears directly. For example, a real type definition is either
   --   floating point definition or a fixed point definition. No explicit node
   --   appears for real type definition. Instead either the floating point
   --   definition or fixed point definition appears directly.

   --   If a non-terminal corresponds to a list of some other non-terminal
   --   (possibly with separating punctuation), then usually it is omitted from
   --   the tree, and a list of components appears instead. For example,
   --   sequence of statements does not appear explicitly in the tree. Instead
   --   a list of statements appears directly.

   --  Some additional cases of omitted nodes occur and are documented
   --  individually. In particular, many nodes are omitted in the tree
   --  generated for an expression.

   -------------------------------------------
   -- Handling of Defining Identifier Lists --
   -------------------------------------------

   --  In several declarative forms in the syntax, lists of defining
   --  identifiers appear (object declarations, component declarations, number
   --  declarations etc.)

   --  The semantics of such statements are equivalent to a series of identical
   --  declarations of single defining identifiers (except that conformance
   --  checks require the same grouping of identifiers in the parameter case).

   --  To simplify semantic processing, the parser breaks down such multiple
   --  declaration cases into sequences of single declarations, duplicating
   --  type and initialization information as required. The flags More_Ids and
   --  Prev_Ids are used to record the original form of the source in the case
   --  where the original source used a list of names, More_Ids being set on
   --  all but the last name and Prev_Ids being set on all but the first name.
   --  These flags are used to reconstruct the original source (e.g. in the
   --  Sprint package), and also are included in the conformance checks, but
   --  otherwise have no semantic significance.

   --  Note: the reason that we use More_Ids and Prev_Ids rather than
   --  First_Name and Last_Name flags is so that the flags are off in the
   --  normal one identifier case, which minimizes tree print output.

   -----------------------
   -- Use of Node Lists --
   -----------------------

   --  With a few exceptions, if a construction of the form {non-terminal}
   --  appears in the tree, lists are used in the corresponding tree node (see
   --  package Nlists for handling of node lists). In this case a field of the
   --  parent node points to a list of nodes for the non-terminal. The field
   --  name for such fields has a plural name which always ends in "s". For
   --  example, a case statement has a field Alternatives pointing to list of
   --  case statement alternative nodes.

   --  Only fields pointing to lists have names ending in "s", so generally the
   --  structure is strongly typed, fields not ending in s point to single
   --  nodes, and fields ending in s point to lists.

   --  The following example shows how a traversal of a list is written. We
   --  suppose here that Stmt points to a N_Case_Statement node which has a
   --  list field called Alternatives:

   --   Alt := First (Alternatives (Stmt));
   --   while Present (Alt) loop
   --      ..
   --      -- processing for case statement alternative Alt
   --      ..
   --      Alt := Next (Alt);
   --   end loop;

   --  The Present function tests for Empty, which in this case signals the end
   --  of the list. First returns Empty immediately if the list is empty.
   --  Present is defined in Atree; First and Next are defined in Nlists.

   --  The exceptions to this rule occur with {DEFINING_IDENTIFIERS} in all
   --  contexts, which is handled as described in the previous section, and
   --  with {,library_unit_NAME} in the N_With_Clause node, which is handled
   --  using the First_Name and Last_Name flags, as further detailed in the
   --  description of the N_With_Clause node.

   -------------
   -- Pragmas --
   -------------

   --  Pragmas can appear in many different context, but are not included in
   --  the grammar. Still they must appear in the tree, so they can be properly
   --  processed.

   --  Two approaches are used. In some cases, an extra field is defined in an
   --  appropriate node that contains a list of pragmas appearing in the
   --  expected context. For example pragmas can appear before an
   --  Accept_Alternative in a Selective_Accept_Statement, and these pragmas
   --  appear in the Pragmas_Before field of the N_Accept_Alternative node.

   --  The other approach is to simply allow pragmas to appear in syntactic
   --  lists where the grammar (of course) does not include the possibility.
   --  For example, the Variants field of an N_Variant_Part node points to a
   --  list that can contain both N_Pragma and N_Variant nodes.

   --  To make processing easier in the latter case, the Nlists package
   --  provides a set of routines (First_Non_Pragma, Last_Non_Pragma,
   --  Next_Non_Pragma, Prev_Non_Pragma) that allow such lists to be handled
   --  ignoring all pragmas.

   --  In the case of the variants list, we can either write:

   --      Variant := First (Variants (N));
   --      while Present (Variant) loop
   --         ...
   --         Variant := Next (Variant);
   --      end loop;

   --  or

   --      Variant := First_Non_Pragma (Variants (N));
   --      while Present (Variant) loop
   --         ...
   --         Variant := Next_Non_Pragma (Variant);
   --      end loop;

   --  In the first form of the loop, Variant can either be an N_Pragma or an
   --  N_Variant node. In the second form, Variant can only be N_Variant since
   --  all pragmas are skipped.

   ---------------------
   -- Optional Fields --
   ---------------------

   --  Fields which correspond to a section of the syntax enclosed in square
   --  brackets are generally omitted (and the corresponding field set to Empty
   --  for a node, or No_List for a list). The documentation of such fields
   --  notes these cases. One exception to this rule occurs in the case of
   --  possibly empty statement sequences (such as the sequence of statements
   --  in an entry call alternative). Such cases appear in the syntax rules as
   --  [SEQUENCE_OF_STATEMENTS] and the fields corresponding to such optional
   --  statement sequences always contain an empty list (not No_List) if no
   --  statements are present.

   --  Note: the utility program that constructs the body and spec of the Nmake
   --  package relies on the format of the comments to determine if a field
   --  should have a default value in the corresponding make routine. The rule
   --  is that if the first line of the description of the field contains the
   --  string "(set to xxx if", then a default value of xxx is provided for
   --  this field in the corresponding Make_yyy routine.

   -----------------------------------
   -- Note on Body/Spec Terminology --
   -----------------------------------

   --  In informal discussions about Ada, it is customary to refer to package
   --  and subprogram specs and bodies. However, this is not technically
   --  correct, what is normally referred to as a spec or specification is in
   --  fact a package declaration or subprogram declaration. We are careful in
   --  GNAT to use the correct terminology and in particular, the full word
   --  specification is never used as an incorrect substitute for declaration.
   --  The structure and terminology used in the tree also reflects the grammar
   --  and thus uses declaration and specification in the technically correct
   --  manner.

   --  However, there are contexts in which the informal terminology is useful.
   --  We have the word "body" to refer to the Interp_Etype declared by the
   --  declaration of a unit body, and in some contexts we need similar term to
   --  refer to the entity declared by the package or subprogram declaration,
   --  and simply using declaration can be confusing since the body also has a
   --  declaration.

   --  An example of such a context is the link between the package body and
   --  its declaration. With_Declaration is confusing, since the package body
   --  itself is a declaration.

   --  To deal with this problem, we reserve the informal term Spec, i.e. the
   --  popular abbreviation used in this context, to refer to the entity
   --  declared by the package or subprogram declaration. So in the above
   --  example case, the field in the body is called With_Spec.

   --  Another important context for the use of the word Spec is in error
   --  messages, where a hyper-correct use of declaration would be confusing to
   --  a typical Ada programmer, and even for an expert programmer can cause
   --  confusion since the body has a declaration as well.

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

   --  These are Node_Kind settings used in the internal implementation which
   --  are not logically part of the specification.

   --  N_Unused_At_Start
   --  Completely unused entry at the start of the enumeration type. This
   --  is inserted so that no legitimate value is zero, which helps to get
   --  better debugging behavior, since zero is a likely uninitialized value).

   --  N_Unused_At_End
   --  Completely unused entry at the end of the enumeration type. This is
   --  handy so that arrays with Node_Kind as the index type have an extra
   --  entry at the end (see for example the use of the Pchar_Pos_Array in
   --  Treepr, where the extra entry provides the limit value when dealing with
   --  the last used entry in the array).

   -----------------------------------------
   -- Note on the settings of Sloc fields --
   -----------------------------------------

   --  The Sloc field of nodes that come from the source is set by the parser.
   --  For internal nodes, and nodes generated during expansion the Sloc is
   --  usually set in the call to the constructor for the node. In general the
   --  Sloc value chosen for an internal node is the Sloc of the source node
   --  whose processing is responsible for the expansion. For example, the Sloc
   --  of an inherited primitive operation is the Sloc of the corresponding
   --  derived type declaration.

   --  For the nodes of a generic instantiation, the Sloc value is encoded to
   --  represent both the original Sloc in the generic unit, and the Sloc of
   --  the instantiation itself. See Sinput.ads for details.

   --  Subprogram instances create two callable entities: one is the visible
   --  subprogram instance, and the other is an anonymous subprogram nested
   --  within a wrapper package that contains the renamings for the actuals.
   --  Both of these entities have the Sloc of the defining entity in the
   --  instantiation node. This simplified for instance in the past some ASIS
   --  queries.

   -----------------------
   -- Field Definitions --
   -----------------------

   --  In the following node definitions, all fields, both syntactic and
   --  semantic, are documented. The one exception is in the case of entities
   --  (defining identifiers, character literals, and operator symbols), where
   --  the usage of the fields depends on the entity kind. Entity fields are
   --  fully documented in the separate package Einfo.

   --  In the node definitions, three common sets of fields are abbreviated to
   --  save both space in the documentation, and also space in the string
   --  (defined in Tree_Print_Strings) used to print trees. The following
   --  abbreviations are used:

   --    "plus fields for binary operator"
   --       Chars                       Name_Id for the operator
   --       Left_Opnd                   left operand expression
   --       Right_Opnd                  right operand expression
   --       Entity                      defining entity for operator
   --       Associated_Node             for generic processing
   --       Do_Overflow_Check           set if overflow check needed
   --       Has_Private_View            set in generic units
   --       Has_Secondary_Private_View  set in generic units

   --    "plus fields for unary operator"
   --       Chars                       Name_Id for the operator
   --       Right_Opnd                  right operand expression
   --       Entity                      defining entity for operator
   --       Associated_Node             for generic processing
   --       Do_Overflow_Check           set if overflow check needed
   --       Has_Private_View            set in generic units
   --       Has_Secondary_Private_View  set in generic units

   --    "plus fields for expression"
   --       Paren_Count               number of parentheses levels
   --       Etype                     type of the expression
   --       Is_Overloaded             >1 type interpretation exists
   --       Is_Static_Expression      set for static expression
   --       Raises_Constraint_Error   evaluation raises CE
   --       Must_Not_Freeze           set if must not freeze
   --       Do_Range_Check            set if a range check needed
   --       Has_Dynamic_Length_Check  set if length check inserted
   --       Assignment_OK             set if modification is OK
   --       Is_Controlling_Actual     set for controlling argument

   --  Note: see under (EXPRESSION) for further details on the use of
   --  the Paren_Count field to record the number of parentheses levels.

   --  Node_Kind is the type used in the Nkind field to indicate the node kind.
   --  The actual definition of this type is given later (the reason for this
   --  is that we want the descriptions ordered by logical chapter in the RM,
   --  but the type definition is reordered to facilitate the definition of
   --  some subtype ranges. The individual descriptions of the nodes show how
   --  the various fields are used in each node kind, as well as providing
   --  logical names for the fields. Functions and procedures are provided for
   --  accessing and setting these fields using these logical names.

   -----------------------
   -- Gigi Restrictions --
   -----------------------

   --  The tree passed to Gigi is more restricted than the general tree form.
   --  For example, as a result of expansion, most of the tasking nodes can
   --  never appear. For each node to which either a complete or partial
   --  restriction applies, a note entitled "Gigi restriction" appears which
   --  documents the restriction.

   --  Note that most of these restrictions apply only to trees generated when
   --  code is being generated, since they involve expander actions that
   --  destroy the tree.

   ----------------
   -- Ghost Mode --
   ----------------

   --  The SPARK RM 6.9 defines two classes of constructs - Ghost entities and
   --  Ghost statements. The intent of the feature is to treat Ghost constructs
   --  as non-existent when Ghost assertion policy Ignore is in effect.
   --
   --  The corresponding nodes which map to Ghost constructs are:
   --
   --    Ghost entities
   --      Declaration nodes
   --      N_Package_Body
   --      N_Subprogram_Body
   --
   --    Ghost statements
   --      N_Assignment_Statement
   --      N_Procedure_Call_Statement
   --      N_Pragma
   --
   --  In addition, the compiler treats instantiations as Ghost entities
   --
   --  To achieve the removal of ignored Ghost constructs, the compiler relies
   --  on global variables Ghost_Mode and Ignored_Ghost_Region, which comprise
   --  a mechanism called "Ghost regions".
   --
   --  The values of Ghost_Mode are as follows:
   --
   --    1. Check - All static semantics as defined in SPARK RM 6.9 are in
   --       effect. The Ghost region has mode Check.
   --
   --    2. Ignore - Same as Check, ignored Ghost code is not present in ALI
   --       files, object files, and the final executable. The Ghost region
   --       has mode Ignore.
   --
   --    3. None - No Ghost region is in effect
   --
   --  The value of Ignored_Ghost_Region captures the node which initiates an
   --  ignored Ghost region.
   --
   --  A Ghost region is a compiler operating mode, similar to Check_Syntax,
   --  however a region is much more finely grained and depends on the policy
   --  in effect. The region starts prior to the analysis of a Ghost construct
   --  and ends immediately after its expansion. The region is established as
   --  follows:
   --
   --    1. Declarations - Prior to analysis, if the declaration is subject to
   --       pragma Ghost.
   --
   --    2. Renaming declarations - Same as 1) or when the renamed entity is
   --       Ghost.
   --
   --    3. Completing declarations - Same as 1) or when the declaration is
   --       partially analyzed and the declaration completes a Ghost entity.
   --
   --    4. N_Package_Body, N_Subprogram_Body - Same as 1) or when the body is
   --       partially analyzed and completes a Ghost entity.
   --
   --    5. N_Assignment_Statement - After the left hand side is analyzed and
   --       references a Ghost entity.
   --
   --    6. N_Procedure_Call_Statement - After the name is analyzed and denotes
   --       a Ghost procedure.
   --
   --    7. N_Pragma - During analysis, when the related entity is Ghost or the
   --       pragma encloses a Ghost entity.
   --
   --    8. Instantiations - Save as 1) or when the instantiation is partially
   --       analyzed and the generic template is Ghost.
   --
   --  The following routines install a new Ghost region:
   --
   --     Install_Ghost_Region
   --     Mark_And_Set_Ghost_xxx
   --     Set_Ghost_Mode
   --
   --  The following routine ends a Ghost region:
   --
   --     Restore_Ghost_Region
   --
   --  A region may be reinstalled similarly to scopes for decoupled expansion
   --  such as the generation of dispatch tables or the creation of a predicate
   --  function.
   --
   --  If the mode of a Ghost region is Ignore, any newly created nodes as well
   --  as source entities are marked as ignored Ghost. In addition, the marking
   --  process signals all enclosing scopes that an ignored Ghost node resides
   --  within. The compilation unit where the node resides is also added to an
   --  auxiliary table for post processing.
   --
   --  After the analysis and expansion of all compilation units takes place
   --  as well as the instantiation of all inlined [generic] bodies, the GNAT
   --  driver initiates a separate pass which removes all ignored Ghost nodes
   --  from all units stored in the auxiliary table.

   --------------------
   -- GNATprove Mode --
   --------------------

   --  When a file is compiled in GNATprove mode (-gnatd.F), a very light
   --  expansion is performed and the analysis must generate a tree in a
   --  form that meets additional requirements.

   --  This light expansion does two transformations of the tree that cannot
   --  be postponed till after semantic analysis:

   --    1. Replace object renamings by renamed object. This requires the
   --       introduction of temporaries at the point of the renaming, which
   --       must be properly analyzed.

   --    2. Fully qualify entity names. This is needed to generate suitable
   --       local effects and call-graphs in ALI files, with the completely
   --       qualified names (in particular the suffix to distinguish homonyms).

   --  The tree after this light expansion should be fully analyzed
   --  semantically, which sometimes requires the insertion of semantic
   --  preanalysis, for example for subprogram contracts and pragma
   --  check/assert. In particular, all expressions must have their proper
   --  type, and semantic links should be set between tree nodes (partial to
   --  full view, etc.). Some kinds of nodes should be either absent, or can be
   --  ignored by the formal verification backend:

   --      N_Object_Renaming_Declaration: can be ignored safely
   --      N_Expression_Function:         absent (rewritten)
   --      N_Expression_With_Actions:     absent (not generated)

   --  SPARK cross-references are generated from the regular cross-references
   --  (used for browsing and code understanding) and additional references
   --  collected during semantic analysis, in particular on all dereferences.
   --  These SPARK cross-references are output in a separate section of ALI
   --  files, as described in spark_xrefs.adb. They are the basis for the
   --  computation of data dependences in GNATprove. This implies that all
   --  cross-references should be generated in this mode, even those that would
   --  not make sense from a user point-of-view, and that cross-references that
   --  do not lead to data dependences for subprograms can be safely ignored.

   --  GNATprove relies on the following front end behaviors:

   --    1. The first declarations in the list of visible declarations of
   --       a package declaration for a generic instance, up to the first
   --       declaration which comes from source, should correspond to
   --       the "mappings nodes" between formal and actual generic parameters.

   --    2. In addition pragma Debug statements are removed from the tree
   --       (rewritten to NULL stmt), since they should be ignored in formal
   --       verification.

   --    3. An error is also issued for missing subunits, similar to the
   --       warning issued when generating code, to avoid formal verification
   --       of a partial unit.

   --    4. Unconstrained types are not replaced by constrained types whose
   --       bounds are generated from an expression: Expand_Subtype_From_Expr
   --       should be a no-op.

   --    5. Errors (instead of warnings) are issued on compile-time-known
   --       constraint errors even though such cases do not correspond to
   --       illegalities in the Ada RM (this is simply another case where
   --       GNATprove implements a subset of the full language).
   --
   --       However, there are a few exceptions to this rule for cases where
   --       we want to allow the GNATprove analysis to proceed (e.g. range
   --       checks on empty ranges, which typically appear in deactivated
   --       code in a particular configuration).

   --    6. Subtypes should match in the AST, even after a generic is
   --       instantiated. In particular, GNATprove relies on the fact that,
   --       on a selected component, the type of the selected component is
   --       the type of the corresponding component in the prefix of the
   --       selected component.
   --
   --       Note that, in some cases, we know that this rule is broken by the
   --       frontend. In particular, if the selected component is a packed
   --       array depending on a discriminant of a unconstrained formal object
   --       parameter of a generic.

   ----------------
   -- SPARK Mode --
   ----------------

   --  The SPARK RM 1.6.5 defines a mode of operation called "SPARK mode" which
   --  starts a scope where the SPARK language semantics are either On, Off, or
   --  Auto, where Auto leaves the choice to the tools. A SPARK mode may be
   --  specified by means of an aspect or a pragma.

   --  The following entities may be subject to a SPARK mode. Entities marked
   --  with * may possess two different SPARK modes.

   --     E_Entry
   --     E_Entry_Family
   --     E_Function
   --     E_Generic_Function
   --     E_Generic_Package *
   --     E_Generic_Procedure
   --     E_Operator
   --     E_Package *
   --     E_Package_Body *
   --     E_Procedure
   --     E_Protected_Body
   --     E_Protected_Subtype
   --     E_Protected_Type *
   --     E_Subprogram_Body
   --     E_Task_Body
   --     E_Task_Subtype
   --     E_Task_Type *
   --     E_Variable

   --  In order to manage SPARK scopes, the compiler relies on global variables
   --  SPARK_Mode and SPARK_Mode_Pragma and a mechanism called "SPARK regions."
   --  Routines Install_SPARK_Mode and Set_SPARK_Mode create a new SPARK region
   --  and routine Restore_SPARK_Mode ends a SPARK region. A region may be
   --  reinstalled similarly to scopes.

   -----------------------
   -- Check Flag Fields --
   -----------------------

   --  The following flag fields appear in expression nodes:

   --    Do_Division_Check
   --    Do_Overflow_Check
   --    Do_Range_Check

   --  These three flags are always set by the front end during semantic
   --  analysis, on expression nodes that may trigger the corresponding
   --  check. The front end then inserts or not the check during expansion. In
   --  particular, these flags should also be correctly set in GNATprove mode.
   --  As a special case, the front end does not insert a Do_Division_Check
   --  flag on float exponentiation expressions, for the case where the value
   --  is 0.0 and the exponent is negative, although this case does lead to a
   --  division check failure. As another special case, the front end does not
   --  insert a Do_Range_Check on an allocator where the designated type is
   --  scalar, and the designated type is more constrained than the type of the
   --  initialized allocator value or the type of the default value for an
   --  uninitialized allocator.

   --  Note that the expander always takes care of the Do_Range_Check case, so
   --  this flag will never be set in the expanded tree passed to the back end.
   --  For the other two flags, the check can be generated either by the back
   --  end or by the front end, depending on the setting of a target parameter.

   --  Note that this accounts for all nodes that trigger the corresponding
   --  checks, except for range checks on subtype_indications, which may be
   --  required to check that a range_constraint is compatible with the given
   --  subtype (RM 3.2.2(11)).

   --  The following flag fields appear in various nodes:

   --    Do_Discriminant_Check
   --    Do_Length_Check
   --    Do_Storage_Check

   --  These flags are used in some specific cases by the front end, either
   --  during semantic analysis or during expansion, and cannot be expected
   --  to be set on all nodes that trigger the corresponding check.

   ------------------------
   -- Common Flag Fields --
   ------------------------

   --  The following flag fields appear in all nodes:

   --  Analyzed
   --    This flag is used to indicate that a node (and all its children) have
   --    been analyzed. It is used to avoid reanalysis of a node that has
   --    already been analyzed, both for efficiency and functional correctness
   --    reasons.

   --  Comes_From_Source
   --    This flag is set if the node comes directly from an explicit construct
   --    in the source. It is normally on for any nodes built by the scanner or
   --    parser from the source program, with the exception that in a few cases
   --    the parser adds nodes to normalize the representation (in particular,
   --    a null statement is added to a package body if there is no begin/end
   --    initialization section).
   --
   --    Most nodes inserted by the analyzer or expander are not considered
   --    as coming from source, so the flag is off for such nodes. In a few
   --    cases, the expander constructs nodes closely equivalent to nodes
   --    from the source program (e.g. the allocator built for build-in-place
   --    case), and the Comes_From_Source flag is deliberately set.

   --  Error_Posted
   --    This flag is used to avoid multiple error messages being posted on or
   --    referring to the same node. This flag is set if an error message
   --    refers to a node or is posted on its source location, and has the
   --    effect of inhibiting further messages involving this same node.

   ------------------------------------
   -- Description of Semantic Fields --
   ------------------------------------

   --  The meaning of the syntactic fields is generally clear from their names
   --  without any further description, since the names are chosen to
   --  correspond very closely to the syntax in the reference manual. This
   --  section describes the usage of the semantic fields, which are used to
   --  contain additional information determined during semantic analysis.

   --  Accept_Handler_Records
   --    This field is present only in an N_Accept_Alternative node. It is used
   --    to temporarily hold the exception handler records from an accept
   --    statement in a selective accept. These exception handlers will
   --    eventually be placed in the Handler_Records list of the procedure
   --    built for this accept (see Expand_N_Selective_Accept procedure in
   --    Exp_Ch9 for further details).

   --  Access_Types_To_Process
   --    Present in N_Freeze_Entity nodes for Incomplete or private types.
   --    Contains the list of access types which may require specific treatment
   --    when the nature of the type completion is completely known. An example
   --    of such treatment is the generation of the associated_final_chain.

   --  Actions
   --    This field contains a sequence of actions that are associated with the
   --    node holding the field. See the individual node types for details of
   --    how this field is used, as well as the description of the specific use
   --    for a particular node type.

   --  Activation_Chain_Entity
   --    This is used in tree nodes representing task activators (blocks,
   --    subprogram bodies, package declarations, and task bodies). It is
   --    initially Empty, and then gets set to point to the entity for the
   --    declared Activation_Chain variable when the first task is declared.
   --    When tasks are declared in the corresponding declarative region this
   --    entity is located by name (its name is always _Chain) and the declared
   --    tasks are added to the chain. Note that N_Extended_Return_Statement
   --    does not have this attribute, although it does have an activation
   --    chain. This chain is used to store the tasks temporarily, and is not
   --    used for activating them. On successful completion of the return
   --    statement, the tasks are moved to the caller's chain, and the caller
   --    activates them.

   --  Acts_As_Spec
   --    A flag set in the N_Subprogram_Body node for a subprogram body which
   --    is acting as its own spec. In the case of a library-level subprogram
   --    the flag is set as well on the parent compilation unit node.

   --  Actual_Designated_Subtype
   --    Present in N_Free_Statement and N_Explicit_Dereference nodes. If gigi
   --    needs to know the dynamic constrained subtype of the designated
   --    object, this attribute is set to that subtype. This is done for
   --    N_Free_Statements for access-to-classwide types and access-to-
   --    unconstrained packed array types. For N_Explicit_Dereference,
   --    this is done in two circumstances: 1) when the designated type is
   --    an unconstrained packed array and the dereference is the prefix of
   --    a 'Size attribute reference, or 2) when the dereference node is
   --    created for the expansion of an allocator with a subtype_indication
   --    and the designated subtype is an unconstrained composite type.

   --  Address_Warning_Posted
   --    Present in N_Attribute_Definition nodes. Set to indicate that we have
   --    posted a warning for the address clause regarding size or alignment
   --    issues. Used to inhibit multiple redundant messages.

   --  Aggregate_Bounds
   --    Present in array N_Aggregate nodes. If the bounds of the aggregate are
   --    known at compile time, this field points to an N_Range node with those
   --    bounds. Otherwise Empty.

   --  All_Others
   --    Present in an N_Others_Choice node. This flag is set for an others
   --    exception where all exceptions are to be caught, even those that are
   --    not normally handled (in particular the tasking abort signal). This
   --    is used for translation of the at end handler into a normal exception
   --    handler.

   --  Ancestor_Type
   --    Present in record N_Aggregate nodes. Used to store the first global
   --    ancestor of the type of the aggregate in a generic context, if any,
   --    when the type is a derived tagged type. Otherwise Empty.

   --  Aspect_On_Partial_View
   --    Present on an N_Aspect_Specification node. For an aspect that applies
   --    to a type entity, indicates whether the specification appears on the
   --    partial view of a private type or extension. Undefined for aspects
   --    that apply to other entities.

   --  Aspect_Rep_Item
   --    Present in N_Aspect_Specification nodes. Points to the corresponding
   --    pragma/attribute definition node used to process the aspect.

   --  Assignment_OK
   --    This flag is set in a subexpression node for an object, indicating
   --    that the associated object can be modified, even if this would not
   --    normally be permissible (either by direct assignment, or by being
   --    passed as an out or in-out parameter). This is used by the expander
   --    for a number of purposes, including initialization of constants and
   --    limited type objects (such as tasks), setting discriminant fields,
   --    setting tag values, etc. N_Object_Declaration nodes also have this
   --    flag defined: here it is used to indicate that an initialization
   --    expression is valid, even where it would normally not be allowed
   --    (e.g. where the type involved is limited).

   --  Associated_Node
   --    Present in nodes that can denote an entity: identifiers, character
   --    literals, operator symbols, expanded names, operator nodes, and
   --    attribute reference nodes (all these nodes have an Entity field).
   --    This field is also present in N_Aggregate, N_Selected_Component, and
   --    N_Extension_Aggregate nodes. This field is used in generic processing
   --    to create links between the generic template and the generic copy.
   --    See Sem_Ch12.Get_Associated_Node for full details. Note that this
   --    field overlaps Entity, which is fine, since, as explained in Sem_Ch12,
   --    the normal function of Entity is not required at the point where the
   --    Associated_Node is set. Note also, that in generic templates, this
   --    means that the Entity field does not necessarily point to an Entity.
   --    Since the back end is expected to ignore generic templates, this is
   --    harmless.

   --  Atomic_Sync_Required
   --    This flag is set on a node for which atomic synchronization is
   --    required for the corresponding reference or modification.

   --  At_End_Proc
   --    This field is present in N_Handled_Sequence_Of_Statements,
   --    N_Package_Body, N_Subprogram_Body, N_Task_Body, N_Block_Statement,
   --    and N_Entry_Body.
   --    It contains an identifier reference for the cleanup procedure to be
   --    called. See description of N_Handled_Sequence_Of_Statements node
   --    for further details.

   --  Backwards_OK
   --    A flag present in the N_Assignment_Statement node. It is used only
   --    if the type being assigned is an array type, and is set if analysis
   --    determines that it is definitely safe to do the copy backwards, i.e.
   --    starting at the highest addressed element. This is the case if either
   --    the operands do not overlap, or they may overlap, but if they do,
   --    then the left operand is at a higher address than the right operand.
   --
   --    Note: If neither of the flags Forwards_OK or Backwards_OK is set, it
   --    means that the front end could not determine that either direction is
   --    definitely safe, and a runtime check may be required if the backend
   --    cannot figure it out. If both flags Forwards_OK and Backwards_OK are
   --    set, it means that the front end can assure no overlap of operands.

   --  Body_To_Inline
   --    Present in subprogram declarations. Denotes analyzed but unexpanded
   --    body of subprogram, to be used when inlining calls. Present when the
   --    subprogram has an Inline pragma and inlining is enabled. If the
   --    declaration is completed by a renaming_as_body, and the renamed entity
   --    is a subprogram, the Body_To_Inline is the name of that entity, which
   --    is used directly in later calls to the original subprogram.

   --  Body_Required
   --    A flag that appears in the N_Compilation_Unit node indicating that
   --    the corresponding unit requires a body. For the package case, this
   --    indicates that a completion is required. In Ada 95, if the flag is not
   --    set for the package case, then a body may not be present. In Ada 83,
   --    if the flag is not set for the package case, then body is optional.
   --    For a subprogram declaration, the flag is set except in the case where
   --    a pragma Import or Interface applies, in which case no body is
   --    permitted (in Ada 83 or Ada 95).

   --  Cannot_Be_Superflat
   --    This flag is present in N_Range nodes. It is set if the range is of a
   --    discrete type and cannot be superflat, i.e. it is guaranteed that the
   --    inequality High_Bound >= Low_Bound - 1 is true. At the time of this
   --    writing, it is only used by the code generator to streamline things.

   --  Cleanup_Actions
   --    Present in block statements created for transient blocks, contains
   --    additional cleanup actions carried over from the transient scope.

   --  Check_Address_Alignment
   --    A flag present in N_Attribute_Definition clause for a 'Address
   --    attribute definition. This flag is set if a dynamic check should be
   --    generated at the freeze point for the entity to which this address
   --    clause applies. The reason that we need this flag is that we want to
   --    check for range checks being suppressed at the point where the
   --    attribute definition clause is given, rather than testing this at the
   --    freeze point.

   --  Comes_From_Check_Or_Contract
   --    This flag is present in all N_If_Statement nodes and
   --    gets set when an N_If_Statement is generated as part of
   --    the expansion of a Check, Assert, or contract-related
   --    pragma.

   --  Comes_From_Extended_Return_Statement
   --    Present in N_Simple_Return_Statement nodes. True if this node was
   --    constructed as part of the N_Extended_Return_Statement expansion.

   --  Comes_From_Iterator
   --    Present in N_Object_Renaming_Declaration nodes. True if this node was
   --    was constructed as part of the expansion of an iterator
   --    specification.

   --  Compare_Type
   --    Present in N_Op_Compare nodes. Set during resolution to the type of
   --    the operands. It is used to propagate the type of the operands from
   --    a N_Op_Compare node in a generic construct to the nodes created from
   --    it in the various instances, when this type is global to the generic
   --    construct. Resolution for global types cannot be redone in instances
   --    because the instantiation can be done out of context, e.g. for bodies,
   --    and the visibility of global types is incorrect in this case; that is
   --    why the result of the resolution done in the generic construct needs
   --    to be available in the instances but, unlike for arithmetic operators,
   --    the Etype cannot be used to that effect for comparison operators. It
   --    is also used as the type subject to the Has_Private_View processing on
   --    the nodes instead of the Etype.

   --  Compile_Time_Known_Aggregate
   --    Present in N_Aggregate nodes. Set for aggregates which can be fully
   --    evaluated at compile time without raising constraint error. Such
   --    aggregates can be passed as is to the back end without any expansion.
   --    See Exp_Aggr for specific conditions under which this flag gets set.

   --  Componentwise_Assignment
   --    Present in N_Assignment_Statement nodes. Set for a record assignment
   --    where all that needs doing is to expand it into component-by-component
   --    assignments. This is used internally for the case of tagged types with
   --    rep clauses, where we need to avoid recursion (we don't want to try to
   --    generate a call to the primitive operation, because this is the case
   --    where we are compiling the primitive operation). Note that when we are
   --    expanding component assignments in this case, we never assign the _tag
   --    field, but we recursively assign components of the parent type.

   --  Condition_Actions
   --    This field appears in else-if nodes and in the iteration scheme node
   --    for while loops. This field is only used during semantic processing to
   --    temporarily hold actions inserted into the tree. In the tree passed
   --    to gigi, the condition actions field is always set to No_List. For
   --    details on how this field is used, see the routine Insert_Actions in
   --    package Exp_Util, and also the expansion routines for the relevant
   --    nodes.

   --  Context_Pending
   --    This field appears in Compilation_Unit nodes, to indicate that the
   --    context of the unit is being compiled. Used to detect circularities
   --    that are not otherwise detected by the loading mechanism. Such
   --    circularities can occur in the presence of limited and non-limited
   --    with_clauses that mention the same units.

   --  Controlling_Argument
   --    This field is set in procedure and function call nodes if the call
   --    is a dispatching call (it is Empty for a non-dispatching call). It
   --    indicates the source of the call's controlling tag. For procedure
   --    calls, the Controlling_Argument is one of the actuals. For function
   --    that has a dispatching result, it is an entity in the context of the
   --    call that can provide a tag, or else it is the tag of the root type
   --    of the class. It can also specify a tag directly rather than being a
   --    tagged object. The latter is needed by the implementations of AI-239
   --    and AI-260.

   --  Conversion_OK
   --    A flag set on type conversion nodes to indicate that the conversion
   --    is to be considered as being valid, even though it is the case that
   --    the conversion is not valid Ada. This is used for attributes Enum_Rep,
   --    Pos, Val, Fixed_Value and Integer_Value, for internal conversions done
   --    for fixed-point operations, and for certain conversions for calls to
   --    initialization procedures. If Conversion_OK is set, then Etype must be
   --    set (the analyzer assumes that Etype has been set). For the case of
   --    fixed-point operands, it also indicates that the conversion is to be
   --    direct conversion of the underlying integer result, with no regard to
   --    the small operand.

   --  Corresponding_Aspect
   --    Present in N_Pragma node. Used to point back to the source aspect from
   --    the corresponding pragma. This field is Empty for source pragmas.

   --  Corresponding_Body
   --    This field is set in subprogram declarations, package declarations,
   --    entry declarations of protected types, and in generic units. It points
   --    to the defining entity for the corresponding body (NOT the node for
   --    the body itself).

   --    Corresponding_Entry_Body
   --    Defined in N_Subprogram_Body. Set for subprogram bodies that implement
   --    a protected type entry; points to the body for the entry.

   --  Corresponding_Formal_Spec
   --    This field is set in subprogram renaming declarations, where it points
   --    to the defining entity for a formal subprogram in the case where the
   --    renaming corresponds to a generic formal subprogram association in an
   --    instantiation. The field is Empty if the renaming does not correspond
   --    to such a formal association.

   --  Corresponding_Generic_Association
   --    This field is defined for object declarations and object renaming
   --    declarations. It is set for the declarations within an instance that
   --    map generic formals to their actuals. If set, the field points either
   --    to a copy of a default expression for an actual of mode IN or to a
   --    generic_association which is the original parent of the expression or
   --    name appearing in the declaration. This simplifies GNATprove queries.

   --  Corresponding_Integer_Value
   --    This field is set in real literals of fixed-point types (it is not
   --    used for floating-point types). It contains the integer value used
   --    to represent the fixed-point value. It is also set on the universal
   --    real literals used to represent bounds of fixed-point base types
   --    and their first named subtypes.

   --  Corresponding_Spec
   --    This field is set in subprogram, package, task, entry and protected
   --    body nodes where it points to the defining entity in the corresponding
   --    spec. The attribute is also set in N_With_Clause nodes where it points
   --    to the defining entity for the with'ed spec, and in a subprogram
   --    renaming declaration when it is a Renaming_As_Body. The field is Empty
   --    if there is no corresponding spec, as in the case of a subprogram body
   --    that serves as its own spec.
   --
   --    In Ada 2012, Corresponding_Spec is set on expression functions that
   --    complete a subprogram declaration.

   --  Corresponding_Spec_Of_Stub
   --    This field is present in subprogram, package, task, and protected body
   --    stubs where it points to the corresponding spec of the stub. Due to
   --    clashes in the structure of nodes, we cannot use Corresponding_Spec.

   --  Corresponding_Stub
   --    This field is present in an N_Subunit node. It holds the node in
   --    the parent unit that is the stub declaration for the subunit. It is
   --    set when analysis of the stub forces loading of the proper body. If
   --    expansion of the proper body creates new declarative nodes, they are
   --    inserted at the point of the corresponding_stub.

   --  Dcheck_Function
   --    This field is present in an N_Variant node, It references the entity
   --    for the discriminant checking function for the variant.

   --  Default_Expression
   --    This field is Empty if there is no default expression. If there is a
   --    simple default expression (one with no side effects), then this field
   --    simply contains a copy of the Expression field (both point to the tree
   --    for the default expression). Default_Expression is used for
   --    conformance checking.

   --  Default_Storage_Pool
   --    This field is present in N_Compilation_Unit_Aux nodes. It is set to a
   --    copy of Opt.Default_Pool at the end of the compilation unit. See
   --    package Opt for details. This is used for inheriting the
   --    Default_Storage_Pool in child units.

   --  Discr_Check_Funcs_Built
   --    This flag is present in N_Full_Type_Declaration nodes. It is set when
   --    discriminant checking functions are constructed. The purpose is to
   --    avoid attempting to set these functions more than once.

   --  Do_Discriminant_Check
   --    This flag is set on N_Selected_Component nodes to indicate that a
   --    discriminant check is required using the discriminant check routine
   --    associated with the selector. The actual check is generated by the
   --    expander when processing selected components. In the case of
   --    Unchecked_Union, the flag is also set, but no discriminant check
   --    routine is associated with the selector, and the expander does not
   --    generate a check. This flag is also present in assignment statements
   --    (and set if the assignment requires a discriminant check), and in type
   --    conversion nodes (and set if the conversion requires a check).

   --  Do_Division_Check
   --    This flag is set on a division operator (/ mod rem) to indicate that
   --    a zero divide check is required. The actual check is either dealt with
   --    by the back end if Backend_Divide_Checks is set to true, or by the
   --    front end itself if it is set to false.

   --  Do_Length_Check
   --    This flag is set in an N_Assignment_Statement, N_Op_And, N_Op_Or,
   --    N_Op_Xor, or N_Type_Conversion node to indicate that a length check
   --    is required. It is not determined who deals with this flag (???).

   --  Do_Overflow_Check
   --    This flag is set on an operator where an overflow check is required on
   --    the operation. The actual check is either dealt with by the back end
   --    if Backend_Overflow_Checks is set to true, or by the front end itself
   --    if it is set to false. The other cases where this flag is used is on a
   --    Type_Conversion node as well on if and case expression nodes.
   --    For a type conversion, it means that the conversion is from one base
   --    type to another, and the value may not fit in the target base type.
   --    See also the description of Do_Range_Check for this case. This flag is
   --    also set on if and case expression nodes if we are operating in either
   --    MINIMIZED or ELIMINATED overflow checking mode (to make sure that we
   --    properly process overflow checking for dependent expressions).

   --  Do_Range_Check
   --    This flag is set on an expression which appears in a context where a
   --    range check is required. The target type is clear from the context.
   --    The contexts in which this flag can appear are the following:

   --      Right side of an assignment. In this case the target type is taken
   --      from the left side of the assignment, which is referenced by the
   --      Name of the N_Assignment_Statement node.

   --      Subscript expressions in an indexed component. In this case the
   --      target type is determined from the type of the array, which is
   --      referenced by the Prefix of the N_Indexed_Component node.

   --      Argument expression for a parameter, appearing either directly in
   --      the Parameter_Associations list of a call or as the Expression of an
   --      N_Parameter_Association node that appears in this list. In either
   --      case, the check is against the type of the formal. Note that the
   --      flag is relevant only in IN and IN OUT parameters, and will be
   --      ignored for OUT parameters, where no check is required in the call,
   --      and if a check is required on the return, it is generated explicitly
   --      with a type conversion.

   --      Initialization expression for the initial value in an object
   --      declaration. In this case the Do_Range_Check flag is set on
   --      the initialization expression, and the check is against the
   --      range of the type of the object being declared. This includes the
   --      cases of expressions providing default discriminant values, and
   --      expressions used to initialize record components.

   --      The expression of a type conversion. In this case the range check is
   --      against the target type of the conversion. See also the use of
   --      Do_Overflow_Check on a type conversion. The distinction is that the
   --      overflow check protects against a value that is outside the range of
   --      the target base type, whereas a range check checks that the
   --      resulting value (which is a value of the base type of the target
   --      type), satisfies the range constraint of the target type.

   --    Note: when a range check is required in contexts other than those
   --    listed above (e.g. in a return statement), an additional type
   --    conversion node is introduced to represent the required check.

   --  Do_Storage_Check
   --    This flag is set in an N_Allocator node to indicate that a storage
   --    check is required for the allocation, or in an N_Subprogram_Body node
   --    to indicate that a stack check is required in the subprogram prologue.
   --    The N_Allocator case is handled by the routine that expands the call
   --    to the runtime routine. The N_Subprogram_Body case is handled by the
   --    backend, and all the semantics does is set the flag.

   --  Elaborate_Present
   --    This flag is set in the N_With_Clause node to indicate that pragma
   --    Elaborate pragma appears for the with'ed units.

   --  Elaborate_All_Desirable
   --    This flag is set in the N_With_Clause mode to indicate that the static
   --    elaboration processing has determined that an Elaborate_All pragma is
   --    desirable for correct elaboration for this unit.

   --  Elaborate_All_Present
   --    This flag is set in the N_With_Clause node to indicate that a
   --    pragma Elaborate_All pragma appears for the with'ed units.

   --  Elaborate_Desirable
   --    This flag is set in the N_With_Clause mode to indicate that the static
   --    elaboration processing has determined that an Elaborate pragma is
   --    desirable for correct elaboration for this unit.

   --  Else_Actions
   --    This field is present in if expression nodes. During code
   --    expansion we use the Insert_Actions procedure (in Exp_Util) to insert
   --    actions at an appropriate place in the tree to get elaborated at the
   --    right time. For if expressions, we have to be sure that the actions
   --    for the Else branch are only elaborated if the condition is False.
   --    The Else_Actions field is used as a temporary parking place for
   --    these actions. The final tree is always rewritten to eliminate the
   --    need for this field, so in the tree passed to Gigi, this field is
   --    always set to No_List.

   --  Enclosing_Variant
   --    This field is present in the N_Variant node and identifies the Node_Id
   --    corresponding to the immediately enclosing variant when the variant is
   --    nested, and N_Empty otherwise. Set during semantic processing of the
   --    variant part of a record type.

   --  Entity
   --    Appears in all direct names (identifiers, character literals, and
   --    operator symbols), as well as expanded names, and attributes that
   --    denote entities, such as 'Class. Points to entity for corresponding
   --    defining occurrence. Set after name resolution. For identifiers in a
   --    WITH list, the corresponding defining occurrence is in a separately
   --    compiled file, and Entity must be set by the library Load procedure.
   --
   --    Note: During name resolution, the value in Entity may be temporarily
   --    incorrect (e.g. during overload resolution, Entity is initially set to
   --    the first possible correct interpretation, and then later modified if
   --    necessary to contain the correct value after resolution).
   --
   --    Note: This field overlaps Associated_Node, which is used during
   --    generic processing (see Sem_Ch12 for details). Note also that in
   --    generic templates, this means that the Entity field does not always
   --    point to an Entity. Since the back end is expected to ignore generic
   --    templates, this is harmless.
   --
   --    Note: This field also appears in N_Attribute_Definition_Clause nodes.
   --    It is used only for stream attributes definition clauses. In this
   --    case, it denotes a (possibly dummy) subprogram entity that is declared
   --    conceptually at the point of the clause. Thus the visibility of the
   --    attribute definition clause (in the sense of 8.3(23) as amended by
   --    AI-195) can be checked by testing the visibility of that subprogram.
   --
   --    Note: Normally the Entity field of an identifier points to the entity
   --    for the corresponding defining identifier, and hence the Chars field
   --    of an identifier will match the Chars field of the entity. However,
   --    there is no requirement that these match, and there are obscure cases
   --    of generated code where they do not match.

   --    Note: Ada 2012 aspect specifications require additional links between
   --    identifiers and various attributes. These attributes can be of
   --    arbitrary types, and the entity field of identifiers that denote
   --    aspects must be used to store arbitrary expressions for later semantic
   --    checks. See section on aspect specifications for details.

   --  Entity_Or_Associated_Node
   --    A synonym for both Entity and Associated_Node. Used by convention in
   --    the code when referencing this field in cases where it is not known
   --    whether the field contains an Entity or an Associated_Node.

   --  Etype
   --    Appears in all expression nodes, all direct names, and all entities.
   --    Points to the entity for the related type. Set after type resolution.
   --    Normally this is the actual subtype of the expression. However, in
   --    certain contexts such as the right side of an assignment, subscripts,
   --    arguments to calls, returned value in a function, initial value etc.
   --    it is the desired target type. In the event that this is different
   --    from the actual type, the Do_Range_Check flag will be set if a range
   --    check is required. Note: if the Is_Overloaded flag is set, then Etype
   --    points to an essentially arbitrary choice from the possible set of
   --    types.

   --  Exception_Junk
   --    This flag is set in a various nodes appearing in a statement sequence
   --    to indicate that the corresponding node is an artifact of the
   --    generated code for exception handling, and should be ignored when
   --    analyzing the control flow of the relevant sequence of statements
   --    (e.g. to check that it does not end with a bad return statement).

   --  Exception_Label
   --    Appears in N_Push_xxx_Label nodes. Points to the entity of the label
   --    to be used for transforming the corresponding exception into a goto,
   --    or contains Empty, if this exception is not to be transformed. Also
   --    appears in N_Exception_Handler nodes, where, if set, it indicates
   --    that there may be a local raise for the handler, so that expansion
   --    to allow a goto is required (and this field contains the label for
   --    this goto). See Exp_Ch11.Expand_Local_Exception_Handlers for details.

   --  Expansion_Delayed
   --    Set on aggregates and extension aggregates that need a top-down rather
   --    than bottom-up expansion. Typically aggregate expansion happens bottom
   --    up. For nested aggregates the expansion is delayed until the enclosing
   --    aggregate itself is expanded, e.g. in the context of a declaration. To
   --    delay it we set this flag. This is done to avoid creating a temporary
   --    for each level of a nested aggregate, and also to prevent the
   --    premature generation of constraint checks. This is also a requirement
   --    if we want to generate the proper attachment to the internal
   --    finalization lists (for record with controlled components). Top down
   --    expansion of aggregates is also used for in-place array aggregate
   --    assignment or initialization. When the full context is known, the
   --    target of the assignment or initialization is used to generate the
   --    left-hand side of individual assignment to each subcomponent.
   --    Also set on conditional expressions whose dependent expressions are
   --    nested aggregates (recursively), or which are expressions of simple
   --    return statements (recursively again), in order to avoid creating a
   --    temporary for them.

   --  Expression_Copy
   --    Present in N_Pragma_Argument_Association nodes. Contains a copy of the
   --    original expression. This field is best used to store pragma-dependent
   --    modifications performed on the original expression such as replacement
   --    of the current type instance or substitutions of primitives.

   --  File_Index
   --    Present in N_External_Initializer nodes. Contains a Source_File_Index
   --    that references the file the external initializer points to.

   --  First_Inlined_Subprogram
   --    Present in the N_Compilation_Unit node for the main program. Points
   --    to a chain of entities for subprograms that are to be inlined. The
   --    Next_Inlined_Subprogram field of these entities is used as a link
   --    pointer with Empty marking the end of the list. This field is Empty
   --    if there are no inlined subprograms or inlining is not active.

   --  First_Named_Actual
   --    Present in procedure call statement and function call nodes, and also
   --    in Intrinsic nodes. Set during semantic analysis to point to the first
   --    named parameter where parameters are ordered by declaration order (as
   --    opposed to the actual order in the call which may be different due to
   --    named associations). Note: this field points to the explicit actual
   --    parameter itself, not the N_Parameter_Association node (its parent).

   --  First_Subtype_Link
   --    Present in N_Freeze_Entity node for an anonymous base type that is
   --    implicitly created by the declaration of a first subtype. It points
   --    to the entity for the first subtype.

   --  Float_Truncate
   --    A flag present in type conversion nodes. It is used for floating-point
   --    to fixed-point or integer conversions, where truncation is required
   --    rather than rounding.

   --  Forwards_OK
   --    A flag present in the N_Assignment_Statement node. It is used only
   --    if the type being assigned is an array type, and is set if analysis
   --    determines that it is definitely safe to do the copy forwards, i.e.
   --    starting at the lowest addressed element. This is the case if either
   --    the operands do not overlap, or they may overlap, but if they do,
   --    then the left operand is at a lower address than the right operand.
   --
   --    Note: If neither of the flags Forwards_OK or Backwards_OK is set, it
   --    means that the front end could not determine that either direction is
   --    definitely safe, and a runtime check may be required if the backend
   --    cannot figure it out. If both flags Forwards_OK and Backwards_OK are
   --    set, it means that the front end can assure no overlap of operands.

   --  For_Special_Return_Object
   --    Present in N_Allocator nodes. True if the allocator is generated for
   --    the initialization of a special return object.

   --  From_Aspect_Specification
   --    Processing of aspect specifications typically results in insertion in
   --    the tree of corresponding pragma or attribute definition clause nodes.
   --    These generated nodes have the From_Aspect_Specification flag set to
   --    indicate that they came from aspect specifications originally.

   --  From_At_Mod
   --    This flag is set on the attribute definition clause node that is
   --    generated by a transformation of an at mod phrase in a record
   --    representation clause. This is used to give slightly different (Ada 83
   --    compatible) semantics to such a clause, namely it is used to specify a
   --    minimum acceptable alignment for the base type and all subtypes. In
   --    Ada 95 terms, the actual alignment of the base type and all subtypes
   --    must be a multiple of the given value, and the representation clause
   --    is considered to be type specific instead of subtype specific.

   --  From_Conditional_Expression
   --    This flag is set on if and case statements generated by the expansion
   --    of if and case expressions respectively. The flag is used to suppress
   --    any finalization of controlled objects found within these statements.

   --  From_Default
   --    This flag is set on the subprogram renaming declaration created in an
   --    instance for a formal subprogram, when the formal is declared with a
   --    box, and there is no explicit actual. If the flag is present, the
   --    declaration is treated as an implicit reference to the formal in the
   --    ali file.

   --  Generalized_Indexing
   --    Present in N_Indexed_Component nodes. Set for Indexed_Component nodes
   --    that are Ada 2012 container indexing operations. The value of the
   --    attribute is a function call (possibly dereferenced) that corresponds
   --    to the proper expansion of the source indexing operation. Before
   --    expansion, the source node is rewritten as the resolved generalized
   --    indexing.

   --  Generic_Parent
   --    Generic_Parent is defined on declaration nodes that are instances. The
   --    value of Generic_Parent is the generic entity from which the instance
   --    is obtained.

   --  Generic_Parent_Type
   --    Generic_Parent_Type is defined on Subtype_Declaration nodes for the
   --    actuals of formal private and derived types. Within the instance, the
   --    operations on the actual are those inherited from the parent. For a
   --    formal private type, the parent type is the generic type itself. The
   --    Generic_Parent_Type is also used in an instance to determine whether a
   --    private operation overrides an inherited one.

   --  Handler_List_Entry
   --    This field is present in N_Object_Declaration nodes. It is set only
   --    for the Handler_Record entry generated for an exception in zero cost
   --    exception handling mode. It references the corresponding item in the
   --    handler list, and is used to delete this entry if the corresponding
   --    handler is deleted during optimization. For further details on why
   --    this is required, see Exp_Ch11.Remove_Handler_Entries.

   --  Has_Dereference_Action
   --    This flag is present in N_Explicit_Dereference nodes. It is set to
   --    indicate that the expansion has aready produced a call to primitive
   --    Dereference of a System.Checked_Pools.Checked_Pool implementation.
   --    Such dereference actions are produced for debugging purposes.

   --  Has_Dynamic_Length_Check
   --    This flag is present in all expression nodes. It is set to indicate
   --    that one of the routines in unit Checks has generated a length check
   --    action which has been inserted at the flagged node. This is used to
   --    avoid the generation of duplicate checks.

   --  Has_Local_Raise
   --    Present in exception handler nodes. Set if the handler can be entered
   --    via a local raise that gets transformed to a goto statement. This will
   --    always be set if Local_Raise_Statements is non-empty, but can also be
   --    set as a result of generation of N_Raise_xxx nodes, or flags set in
   --    nodes requiring generation of back end checks.

   --  Has_No_Elaboration_Code
   --    A flag that appears in the N_Compilation_Unit node to indicate whether
   --    or not elaboration code is present for this unit. It is initially set
   --    true for subprogram specs and bodies and for all generic units and
   --    false for non-generic package specs and bodies. Gigi may set the flag
   --    in the non-generic package case if it determines that no elaboration
   --    code is generated. Note that this flag is not related to the
   --    Is_Preelaborated status, there can be preelaborated packages that
   --    generate elaboration code, and non-preelaborated packages which do
   --    not generate elaboration code.

   --  Has_Pragma_Suppress_All
   --    This flag is set in an N_Compilation_Unit node if the Suppress_All
   --    pragma appears anywhere in the unit. This accommodates the rather
   --    strange placement rules of other compilers (DEC permits it at the
   --    end of a unit, and Rational allows it as a program unit pragma). We
   --    allow it anywhere at all, and consider it equivalent to a pragma
   --    Suppress (All_Checks) appearing at the start of the configuration
   --    pragmas for the unit.

   --  Has_Private_View
   --    A flag present in generic nodes that have an entity, to indicate that
   --    the node has a private type. Used to exchange private and full
   --    declarations if the visibility at instantiation is different from the
   --    visibility at generic definition.

   --  Has_Relative_Deadline_Pragma
   --    A flag present in N_Subprogram_Body and N_Task_Definition nodes to
   --    flag the presence of a pragma Relative_Deadline.

   --  Has_Secondary_Private_View
   --    A flag present in generic nodes that have an entity, to indicate that
   --    the node is either of an access type whose Designated_Type is private
   --    or of an array type whose Component_Type is private. Used to exchange
   --    private and full declarations if the visibility at instantiation is
   --    different from the visibility at generic definition.

   --  Has_Self_Reference
   --    Present in N_Aggregate and N_Extension_Aggregate. Indicates that one
   --    of the expressions contains an access attribute reference to the
   --    enclosing type. Such a self-reference can only appear in default-
   --    initialized aggregate for a record type.

   --  Has_SP_Choice
   --    Present in all nodes containing a Discrete_Choices field (N_Variant,
   --    N_Case_Expression_Alternative, N_Case_Statement_Alternative). Set to
   --    True if the Discrete_Choices list has at least one occurrence of a
   --    statically predicated subtype.

   --  Has_Storage_Size_Pragma
   --    A flag present in an N_Task_Definition node to flag the presence of a
   --    Storage_Size pragma.

   --  Has_Target_Names
   --    Present in assignment statements. Indicates that the RHS contains
   --    target names (see AI12-0125-3) and must be expanded accordingly.

   --  Has_Wide_Character
   --    Present in string literals, set if any wide character (i.e. character
   --    code outside the Character range but within Wide_Character range)
   --    appears in the string. Used to implement pragma preference rules.

   --  Has_Wide_Wide_Character
   --    Present in string literals, set if any wide character (i.e. character
   --    code outside the Wide_Character range) appears in the string. Used to
   --    implement pragma preference rules.

   --  Header_Size_Added
   --    Present in N_Attribute_Reference nodes, set only for attribute
   --    Max_Size_In_Storage_Elements. The flag indicates that the size of the
   --    hidden list header used by the runtime finalization support has been
   --    added to the size of the prefix. The flag also prevents the infinite
   --    expansion of the same attribute in the said context.

   --  Hidden_By_Use_Clause
   --    An entity list present in use clauses that appear within
   --    instantiations. For the resolution of local entities, entities
   --    introduced by these use clauses have priority over global ones,
   --    and outer entities must be explicitly hidden/restored on exit.

   --  Import_Interface_Present
   --    This flag is set in an Interface or Import pragma if a matching
   --    pragma of the other kind is also present. This is used to avoid
   --    generating some unwanted error messages.

   --  Includes_Infinities
   --    This flag is present in N_Range nodes. It is set for the range of
   --    unconstrained float types defined in Standard, which include not only
   --    the given range of values, but also legitimately can include infinite
   --    values. This flag is false for any float type for which an explicit
   --    range is given by the programmer, even if that range is identical to
   --    the range for Float.

   --  Incomplete_View
   --    Present in full type declarations that are completions of incomplete
   --    type declarations. Denotes the corresponding incomplete view declared
   --    by the incomplete declaration.

   --  Inherited_Discriminant
   --    This flag is present in N_Component_Association nodes. It indicates
   --    that a given component association in an extension aggregate is the
   --    value obtained from a constraint on an ancestor. Used to prevent
   --    double expansion when the aggregate has expansion delayed.

   --  Instance_Spec
   --    This field is present in generic instantiation nodes, and also in
   --    formal package declaration nodes (formal package declarations are
   --    treated similarly to package instantiations). It points to the node
   --    for the spec of the instance, inserted as part of the semantic
   --    processing for instantiations in Sem_Ch12.

   --  Is_Abort_Block
   --    Present in N_Block_Statement nodes. True if the block protects a list
   --    of statements with an Abort_Defer / Abort_Undefer_Direct pair.

   --  Is_Accessibility_Actual
   --    Present in N_Parameter_Association nodes. True if the parameter is
   --    an extra actual that carries the accessibility level of the actual
   --    for an access parameter, in a function that dispatches on result and
   --    is called in a dispatching context. Used to prevent a formal/actual
   --    mismatch when the call is rewritten as a dispatching call.

   --  Is_Analyzed_Pragma
   --    Present in N_Pragma nodes. Set for delayed pragmas that require a two
   --    step analysis. The initial step is performed by routine Analyze_Pragma
   --    and verifies the overall legality of the pragma. The second step takes
   --    place in the various Analyze_xxx_In_Decl_Part routines which perform
   --    full analysis. The flag prevents the reanalysis of a delayed pragma.

   --  Is_Asynchronous_Call_Block
   --    A flag set in a Block_Statement node to indicate that it is the
   --    expansion of an asynchronous entry call. Such a block needs cleanup
   --    handler to assure that the call is cancelled.

   --  Is_Boolean_Aspect
   --    Present in N_Aspect_Specification node. Set if the aspect is for a
   --    boolean aspect (i.e. Aspect_Id is in Boolean_Aspect subtype).

   --  Is_Checked
   --    Present in N_Aspect_Specification and N_Pragma nodes. Set for an
   --    assertion aspect or pragma, or check pragma for an assertion, that
   --    is to be checked at run time. If either Is_Checked or Is_Ignored
   --    is set (they cannot both be set), then this means that the status of
   --    the pragma has been checked at the appropriate point and should not
   --    be further modified (in some cases these flags are copied when a
   --    pragma is rewritten).

   --  Is_Checked_Ghost_Pragma
   --    This flag is present in N_Pragma nodes. It is set when the pragma is
   --    related to a checked Ghost entity or encloses a checked Ghost entity.
   --    This flag has no relation to Is_Checked.

   --  Is_Component_Left_Opnd
   --  Is_Component_Right_Opnd
   --    Present in concatenation nodes, to indicate that the corresponding
   --    operand is of the component type of the result. Used in resolving
   --    concatenation nodes in instances.

   --  Is_Controlling_Actual
   --    This flag is set on an expression that is a controlling argument in
   --    a dispatching call. It is off in all other cases. See Sem_Disp for
   --    details of its use.

   --  Is_Declaration_Level_Node
   --    Present in call marker and instantiation nodes. Set when the constuct
   --    appears within the declarations of a block statement, an entry body,
   --    a subprogram body, or a task body. The flag aids the ABE Processing
   --    phase to catch certain forms of guaranteed ABEs.

   --  Is_Delayed_Aspect
   --    Present in N_Pragma and N_Attribute_Definition_Clause nodes which
   --    come from aspect specifications, where the evaluation of the aspect
   --    must be delayed to the freeze point. This flag is also set True in
   --    the corresponding N_Aspect_Specification node.

   --  Is_Disabled
   --    A flag set in an N_Aspect_Specification or N_Pragma node if there was
   --    a Check_Policy or Assertion_Policy (or in the case of a Debug_Pragma)
   --    a Debug_Policy pragma that resulted in totally disabling the flagged
   --    aspect or policy as a result of using the GNAT-defined policy DISABLE.
   --    If this flag is set, the aspect or policy is not analyzed for semantic
   --    correctness, so any expressions etc will not be marked as analyzed.

   --  Is_Dispatching_Call
   --    Present in call marker nodes. Set when the related call which prompted
   --    the creation of the marker is dispatching.

   --  Is_Dynamic_Coextension
   --    Present in allocator nodes, to indicate that this is an allocator
   --    for an access discriminant of a dynamically allocated object. The
   --    coextension must be deallocated and finalized at the same time as
   --    the enclosing object. The partner flag Is_Static_Coextension must
   --    be cleared before setting this flag to True.

   --  Is_Effective_Use_Clause
   --    Present in both N_Use_Type_Clause and N_Use_Package_Clause to indicate
   --    a use clause is "used" in the current source.

   --  Is_Elaboration_Checks_OK_Node
   --    Present in the following nodes:
   --
   --      assignment statement
   --      attribute reference
   --      call marker
   --      entry call statement
   --      expanded name
   --      function call
   --      function instantiation
   --      identifier
   --      package instantiation
   --      procedure call statement
   --      procedure instantiation
   --      requeue statement
   --      variable reference marker
   --
   --    Set when the node appears within a context which allows the generation
   --    of run-time ABE checks. This flag determines whether the ABE
   --    Processing phase generates conditional ABE checks and guaranteed ABE
   --    failures.

   --  Is_Elaboration_Code
   --    Present in assignment statements. Set for an assignment which updates
   --    the elaboration flag of a package or subprogram when the corresponding
   --    body is successfully elaborated.

   --  Is_Elaboration_Warnings_OK_Node
   --    Present in the following nodes:
   --
   --      attribute reference
   --      call marker
   --      entry call statement
   --      expanded name
   --      function call
   --      function instantiation
   --      identifier
   --      package instantiation
   --      procedure call statement
   --      procedure instantiation
   --      requeue statement
   --      variable reference marker
   --
   --    Set when the node appears within a context where elaboration warnings
   --    are enabled. This flag determines whether the ABE processing phase
   --    generates diagnostics on various elaboration issues.

   --  Is_Entry_Barrier_Function
   --    This flag is set on N_Subprogram_Declaration and N_Subprogram_Body
   --    nodes which emulate the barrier function of a protected entry body.
   --    The flag is used when checking for incorrect use of Current_Task.

   --  Is_Expanded_Build_In_Place_Call
   --    This flag is set in an N_Function_Call node to indicate that the extra
   --    actuals to support a build-in-place style of call have been added to
   --    the call.

   --  Is_Expanded_Prefixed_Call
   --    This flag is set in N_Function_Call and N_Procedure_Call_Statement
   --    nodes to indicate that it is an expanded prefixed call.

   --  Is_Generic_Contract_Pragma
   --    This flag is present in N_Pragma nodes. It is set when the pragma is
   --    a source construct, applies to a generic unit or its body, and denotes
   --    one of the following contract-related annotations:
   --      Abstract_State
   --      Always_Terminates
   --      Contract_Cases
   --      Depends
   --      Exceptional_Cases
   --      Extensions_Visible
   --      Global
   --      Initial_Condition
   --      Initializes
   --      Post
   --      Post_Class
   --      Postcondition
   --      Pre
   --      Pre_Class
   --      Precondition
   --      Refined_Depends
   --      Refined_Global
   --      Refined_Post
   --      Refined_State
   --      Subprogram_Variant
   --      Test_Case

   --  Is_Homogeneous_Aggregate
   --    A flag set on an Ada 2022 aggregate that uses square brackets as
   --    delimiters, and thus denotes an array or container aggregate, or
   --    the prefix of a reduction attribute.

   --  Is_Ignored
   --    A flag set in an N_Aspect_Specification or N_Pragma node if there was
   --    a Check_Policy or Assertion_Policy (or in the case of a Debug_Pragma)
   --    a Debug_Policy pragma that specified a policy of IGNORE, DISABLE, or
   --    OFF, for the pragma/aspect. If there was a Policy pragma specifying
   --    a Policy of ON or CHECK, then this flag is reset. If no Policy pragma
   --    gives a policy for the aspect or pragma, then there are two cases. For
   --    an assertion aspect or pragma (one of the assertion kinds allowed in
   --    an Assertion_Policy pragma), then Is_Ignored is set if assertions are
   --    ignored because of the absence of a -gnata switch. For any other
   --    aspects or pragmas, the flag is off. If this flag is set, the
   --    aspect/pragma is fully analyzed and checked for other syntactic
   --    and semantic errors, but it does not have any semantic effect.

   --  Is_Ignored_Ghost_Pragma
   --    This flag is present in N_Pragma nodes. It is set when the pragma is
   --    related to an ignored Ghost entity or encloses ignored Ghost entity.
   --    This flag has no relation to Is_Ignored.

   --  Is_Implicit_With
   --    Present in N_With_Clause nodes. Indicates that the clause does not
   --    come from source, or is self referential. Is_Implicit_With is True
   --    in the following cases:
   --
   --      * ABE mechanism - The static elaboration model of both the default
   --        and the legacy ABE mechanism use with clauses to encode implicit
   --        Elaborate[_All] pragmas.
   --
   --      * Analysis - A with clause for child unit A.B.C is equivalent to
   --        a series of clauses for A, A.B, and A.B.C.
   --
   --      * RTSfind - The compiler generates code that references entities
   --        from the runtime.
   --
   --      * Self-referential withs. If a with clause on the body of X says
   --        "with X", this is legal but useless. These are not really
   --        implicit, but are treated as such.

   --  Is_In_Discriminant_Check
   --    This flag is present in a selected component, and is used to indicate
   --    that the reference occurs within a discriminant check. The
   --    significance is that optimizations based on assuming that the
   --    discriminant check has a correct value cannot be performed in this
   --    case (or the discriminant check may be optimized away).

   --  Is_Inherited_Pragma
   --    This flag is set in an N_Pragma node that appears in a N_Contract node
   --    to indicate that the pragma has been inherited from a parent context.

   --  Is_Initialization_Block
   --    Defined in block nodes. Set when the block statement was created by
   --    the finalization machinery to wrap initialization statements. This
   --    flag aids the ABE Processing phase to suppress the diagnostics of
   --    finalization actions in initialization contexts.

   --  Is_Interpolated_String_Literal
   --    Defined in string literals. Used to differentiate string literals
   --    composed of interpolated string elements from string literals found
   --    in interpolated expressions.

   --  Is_Known_Guaranteed_ABE
   --    NOTE: this flag is shared between the legacy ABE mechanism and the
   --    default ABE mechanism.
   --
   --    Present in the following nodes:
   --
   --      call marker
   --      formal package declaration
   --      function call
   --      function instantiation
   --      package instantiation
   --      procedure call statement
   --      procedure instantiation
   --
   --    Set when the elaboration or evaluation of the scenario results in
   --    a guaranteed ABE. The flag is used to suppress the instantiation of
   --    generic bodies because gigi cannot handle certain forms of premature
   --    instantiation, as well as to prevent the reexamination of the node by
   --    the ABE Processing phase.

   --  Is_Machine_Number
   --    This flag is set in an N_Real_Literal node to indicate that the value
   --    is a machine number. This avoids some unnecessary cases of converting
   --    real literals to machine numbers.

   --  Is_Null_Loop
   --    This flag is set in an N_Loop_Statement node if the corresponding loop
   --    can be determined to be null at compile time. This is used to remove
   --    the loop entirely at expansion time.

   --  Is_Overloaded
   --    A flag present in all expression nodes. Used temporarily during
   --    overloading determination. The setting of this flag is not relevant
   --    once overloading analysis is complete.

   --  Is_Parenthesis_Aggregate
   --    A flag set on an aggregate that uses parentheses as delimiters

   --  Is_Power_Of_2_For_Shift
   --    A flag present only in N_Op_Expon nodes. It is set when the
   --    exponentiation is of the form 2 ** N, where the type of N is an
   --    unsigned integral subtype whose size does not exceed the size of
   --    Standard_Integer (i.e. a type that can be safely converted to
   --    Natural), and the exponentiation appears as the right operand of an
   --    integer multiplication or an integer division where the dividend is
   --    unsigned. It is also required that overflow checking is off for both
   --    the exponentiation and the multiply/divide node. If this set of
   --    conditions holds, and the flag is set, then the division or
   --    multiplication can be (and is) converted to a shift.

   --  Is_Preelaborable_Call
   --    Present in call marker nodes. Set when the related call is non-static
   --    but preelaborable.

   --  Is_Prefixed_Call
   --    This flag is set in a selected component within a generic unit, if
   --    it resolves to a prefixed call to a primitive operation. The flag
   --    is used to prevent accidental overloadings in an instance, when a
   --    primitive operation and a private record component may be homographs.

   --  Is_Protected_Subprogram_Body
   --    A flag set in a Subprogram_Body block to indicate that it is the
   --    implementation of a protected subprogram. Such a body needs cleanup
   --    handler to make sure that the associated protected object is unlocked
   --    when the subprogram completes.

   --  Is_Qualified_Universal_Literal
   --    Present in N_Qualified_Expression nodes. Set when the qualification is
   --    converting a universal literal to a specific type. Such qualifiers aid
   --    the resolution of accidental overloading of binary or unary operators
   --    which may occur in instances.

   --  Is_Read
   --    Present in variable reference markers. Set when the original variable
   --    reference constitutes a read of the variable.

   --  Is_Source_Call
   --    Present in call marker nodes. Set when the related call came from
   --    source.

   --  Is_SPARK_Mode_On_Node
   --    Present in the following nodes:
   --
   --      assignment statement
   --      attribute reference
   --      call marker
   --      entry call statement
   --      expanded name
   --      function call
   --      function instantiation
   --      identifier
   --      package instantiation
   --      procedure call statement
   --      procedure instantiation
   --      requeue statement
   --      variable reference marker
   --
   --    Set when the node appears within a context subject to SPARK_Mode On.
   --    This flag determines when the SPARK model of elaboration be activated
   --    by the ABE Processing phase.

   --  Is_Static_Coextension
   --    Present in N_Allocator nodes. Set if the allocator is a coextension
   --    of an object allocated on the stack rather than the heap. The partner
   --    flag Is_Dynamic_Coextension must be cleared before setting this flag
   --    to True.

   --  Is_Static_Expression
   --    Indicates that an expression is a static expression according to the
   --    rules in RM-4.9. See Sem_Eval for details.

   --  Is_Subprogram_Descriptor
   --    Present in N_Object_Declaration, and set only for the object
   --    declaration generated for a subprogram descriptor in fast exception
   --    mode. See Exp_Ch11 for details of use.

   --  Is_Task_Allocation_Block
   --    A flag set in a Block_Statement node to indicate that it is the
   --    expansion of a task allocator, or the allocator of an object
   --    containing tasks. Such a block requires a cleanup handler to call
   --    Expunge_Unactivated_Tasks to complete any tasks that have been
   --    allocated but not activated when the allocator completes abnormally.

   --  Is_Task_Body_Procedure
   --    This flag is set on N_Subprogram_Declaration and N_Subprogram_Body
   --    nodes which emulate the body of a task unit.

   --  Is_Task_Master
   --    A flag set in a Subprogram_Body, Block_Statement, or Task_Body node to
   --    indicate that the construct is a task master (i.e. has declared tasks
   --    or declares an access to a task type).

   --  Is_Write
   --    Present in variable reference markers. Set when the original variable
   --    reference constitutes a write of the variable.

   --  Iterator_Filter
   --    Present in N_Loop_Parameter_Specification and N_Iterator_Specification
   --    nodes for Ada 2022. It is used to store the condition present in the
   --    eponymous Ada 2022 construct.

   --  Itype
   --    Used in N_Itype_Reference node to reference an itype for which it is
   --    important to ensure that it is defined. See description of this node
   --    for further details.

   --  Kill_Range_Check
   --    Used in N_Indexed_Component to indicate that its expressions should
   --    not be subjected to range checks and in N_Unchecked_Type_Conversion
   --    to indicate that the result of the conversion should not be subjected
   --    to range checks. This is used for the implementation of aggregates and
   --    Normalize_Scalars respectively.

   --  Label_Construct
   --    Used in an N_Implicit_Label_Declaration node. Refers to an N_Label,
   --    N_Block_Statement or N_Loop_Statement node to which the label
   --    declaration applies. The field is left empty for the special labels
   --    generated as part of expanding raise statements with a local exception
   --    handler.

   --  Library_Unit
   --    Direct use of this field should be avoided; use the wrappers in
   --    Sinfo.Utils instead.
   --
   --    This field is used to store the following:
   --
   --    In N_Compilation_Unit: Spec_Lib_Unit, Body_Lib_Unit, Subunit_Parent.
   --
   --    In N_Body_Stub: Stub_Subunit.
   --
   --    In N_With_Clause: Withed_Lib_Unit
   --
   --    See Sinfo.Utils for details.

   --  Local_Raise_Statements
   --    This field is present in exception handler nodes. It is set to
   --    No_Elist in the normal case. If there is at least one raise statement
   --    which can potentially be handled as a local raise, then this field
   --    points to a list of raise nodes, which are calls to a routine to raise
   --    an exception. These are raise nodes which can be optimized into gotos
   --    if the handler turns out to meet the conditions which permit this
   --    transformation. Note that this does NOT include instances of the
   --    N_Raise_xxx_Error nodes since the transformation of these nodes is
   --    handled by the back end (using the N_Push/N_Pop mechanism).

   --  Loop_Actions
   --    A list present in Component_Association nodes in array aggregates.
   --    Used to collect actions that must be executed within the loop because
   --    they may need to be evaluated anew each time through.

   --  Limited_View_Installed
   --    Present in With_Clauses and in package specifications. If set on
   --    with_clause, it indicates that this clause has created the current
   --    limited view of the designated package. On a package specification, it
   --    indicates that the limited view has already been created because the
   --    package is mentioned in a limited_with_clause in the closure of the
   --    unit being compiled.

   --  Local_Raise_Not_OK
   --    Present in N_Exception_Handler nodes. Set if the handler contains
   --    a construct (reraise statement, or call to subprogram in package
   --    GNAT.Current_Exception) that makes the handler unsuitable as a target
   --    for a local raise (one that could otherwise be converted to a goto).

   --  Must_Be_Byte_Aligned
   --    This flag is present in N_Attribute_Reference nodes. It can be set
   --    only for the Address and Unrestricted_Access attributes. If set it
   --    means that the object for which the address/access is given must be on
   --    a byte (more accurately a storage unit) boundary. If necessary, a copy
   --    of the object is to be made before taking the address (this copy is in
   --    the current scope on the stack frame). This is used for certain cases
   --    of code generated by the expander that passes parameters by address.
   --
   --    The reason the copy is not made by the front end is that the back end
   --    has more information about type layout and may be able to (but is not
   --    guaranteed to) prevent making unnecessary copies.

   --  Must_Not_Freeze
   --    A flag present in all expression nodes. Normally expressions cause
   --    freezing as described in the RM. If this flag is set, then this is
   --    inhibited. This is used by the analyzer and expander to label nodes
   --    that are created by semantic analysis or expansion and which must not
   --    cause freezing even though they normally would. This flag is also
   --    present in an N_Subtype_Indication node, since we also use these in
   --    calls to Freeze_Expression.

   --  Next_Entity
   --    Present in defining identifiers, defining character literals, and
   --    defining operator symbols (i.e. in all entities). The entities of a
   --    scope are chained, and this field is used as the forward pointer for
   --    this list. See Einfo for further details.

   --  Next_Exit_Statement
   --    Present in N_Exit_Statement nodes. The exit statements for a loop are
   --    chained (in reverse order of appearance) from the First_Exit_Statement
   --    field of the E_Loop entity for the loop. Next_Exit_Statement points to
   --    the next entry on this chain (Empty = end of list).

   --  Next_Implicit_With
   --    Present in N_With_Clause. Part of a chain of with_clauses generated
   --    in rtsfind to indicate implicit dependencies on predefined units. Used
   --    to prevent multiple with_clauses for the same unit in a given context.
   --    A postorder traversal of the tree whose nodes are units and whose
   --    links are with_clauses defines the order in which CodePeer must
   --    examine a compiled unit and its full context. This ordering ensures
   --    that any subprogram call is examined after the subprogram declaration
   --    has been seen.

   --  Next_Named_Actual
   --    Present in parameter association nodes. Set during semantic analysis
   --    to point to the next named parameter, where parameters are ordered by
   --    declaration order (as opposed to the actual order in the call, which
   --    may be different due to named associations). Not that this field
   --    points to the explicit actual parameter itself, not to the
   --    N_Parameter_Association node (its parent).

   --  Next_Pragma
   --    Present in N_Pragma nodes. Used to create a linked list of pragma
   --    nodes. Currently used for two purposes:
   --
   --      Create a list of linked Check_Policy pragmas. The head of this list
   --      is stored in Opt.Check_Policy_List (which has further details).
   --
   --      Used by processing for Pre/Postcondition pragmas to store a list of
   --      pragmas associated with the spec of a subprogram (see Sem_Prag for
   --      details).
   --
   --      Used by processing for pragma SPARK_Mode to store multiple pragmas
   --      the apply to the same construct. These are visible/private mode for
   --      a package spec and declarative/statement mode for package body.

   --  Next_Rep_Item
   --    Present in pragma nodes, attribute definition nodes, enumeration rep
   --    clauses, record rep clauses, aspect specification and null statement
   --    nodes. Used to link representation items that apply to an entity. See
   --    full description of First_Rep_Item field in Einfo for further details.

   --  Next_Use_Clause
   --    While use clauses are active during semantic processing, they are
   --    chained from the scope stack entry, using Next_Use_Clause as a link
   --    pointer, with Empty marking the end of the list. The head pointer is
   --    in the scope stack entry (First_Use_Clause). At the end of semantic
   --    processing (i.e. when Gigi sees the tree, the contents of this field
   --    is undefined and should not be read).

   --  No_Ctrl_Actions
   --    Present in N_Assignment_Statement to indicate that neither Finalize
   --    nor Adjust should take place on this assignment even though the LHS
   --    and RHS are controlled. Also to indicate that the primitive _assign
   --    should not be used for a tagged assignment. This flag is used in init
   --    proc and aggregate expansion where the generated assignments are
   --    initializations, not real assignments. Note that it also suppresses
   --    the creation of transient scopes around the N_Assignment_Statement,
   --    in other words it disables all controlled actions for the assignment.

   --  No_Elaboration_Check
   --    NOTE: this flag is relevant only for the legacy ABE mechanism and
   --    should not be used outside of that context.
   --
   --    Present in N_Function_Call and N_Procedure_Call_Statement. Indicates
   --    that no elaboration check is needed on the call, because it appears in
   --    the context of a local Suppress pragma. This is used on calls within
   --    task bodies, where the actual elaboration checks are applied after
   --    analysis, when the local scope stack is not present.

   --  No_Entities_Ref_In_Spec
   --    Present in N_With_Clause nodes. Set if the with clause is on the
   --    package or subprogram spec where the main unit is the corresponding
   --    body, and no entities of the with'ed unit are referenced by the spec
   --    (an entity may still be referenced in the body, so this flag is used
   --    to generate the proper message (see Sem_Util.Check_Unused_Withs for
   --    full details).

   --  No_Finalize_Actions
   --    Present in N_Assignment_Statement to indicate that no Finalize should
   --    take place on this assignment even though the LHS is controlled. Also
   --    to indicate that the primitive _assign should not be used for a tagged
   --    assignment. This flag is only used in aggregates expansion where the
   --    generated assignments are initializations, not real assignments. Note
   --    that, unlike the No_Ctrl_Actions flag, it does *not* suppress the
   --    creation of transient scopes around the N_Assignment_Statement.

   --  No_Initialization
   --    Present in N_Object_Declaration and N_Allocator to indicate that the
   --    object must not be initialized (by Initialize or call to an init
   --    proc). This is needed for controlled aggregates. When the Object
   --    declaration has an expression, this flag means that this expression
   --    should not be taken into account (needed for in place initialization
   --    with aggregates, and for object with an address clause, which are
   --    initialized with an assignment at freeze time).

   --  No_Minimize_Eliminate
   --    This flag is present in membership operator nodes (N_In/N_Not_In).
   --    It is used to indicate that processing for extended overflow checking
   --    modes is not required (this is used to prevent infinite recursion).

   --  No_Truncation
   --    Present in N_Unchecked_Type_Conversion node. This flag has an effect
   --    only if the RM_Size of the source is greater than the RM_Size of the
   --    target for scalar operands. Normally in such a case we truncate some
   --    higher order bits of the source, and then sign/zero extend the result
   --    to form the output value. But if this flag is set, then we do not do
   --    any truncation, so for example, if an 8 bit input is converted to 5
   --    bit result which is in fact stored in 8 bits, then the high order
   --    three bits of the target result will be copied from the source. This
   --    is used for properly setting out of range values for use by pragmas
   --    Initialize_Scalars and Normalize_Scalars.

   --  Null_Excluding_Subtype
   --    Present in N_Access_To_Object_Definition. Indicates that the subtype
   --    indication carries a null-exclusion indicator, which is distinct from
   --    the null-exclusion indicator that may precede the access keyword.

   --  Original_Discriminant
   --    Present in identifiers. Used in references to discriminants that
   --    appear in generic units. Because the names of the discriminants may be
   --    different in an instance, we use this field to recover the position of
   --    the discriminant in the original type, and replace it with the
   --    discriminant at the same position in the instantiated type.

   --  Original_Entity
   --    Present in numeric literals. Used to denote the named number that has
   --    been constant-folded into the given literal. If literal is from
   --    source, or the result of some other constant-folding operation, then
   --    Original_Entity is empty. This field is needed to handle properly
   --    named numbers in generic units, where the Associated_Node field
   --    interferes with the Entity field, making it impossible to preserve the
   --    original entity at the point of instantiation.

   --  Others_Discrete_Choices
   --    When a case statement or variant is analyzed, the semantic checks
   --    determine the actual list of choices that correspond to an others
   --    choice. This list is materialized for later use by the expander and
   --    the Others_Discrete_Choices field of an N_Others_Choice node points to
   --    this materialized list of choices, which is in standard format for a
   --    list of discrete choices, except that of course it cannot contain an
   --    N_Others_Choice entry.

   --  Parent_Spec
   --    For a library unit that is a child unit spec (package or subprogram
   --    declaration, generic declaration or instantiation, or library level
   --    rename) this field points to the compilation unit node for the parent
   --    package specification. This field is Empty for library bodies (the
   --    parent spec in this case can be found from the corresponding spec).

   --  Parent_With
   --    Present in N_With_Clause nodes. The flag indicates that the clause
   --    was generated for an ancestor unit to provide proper visibility. A
   --    with clause for child unit A.B.C produces two implicit parent with
   --    clauses for A and A.B.

   --  Premature_Use
   --    Present in N_Incomplete_Type_Declaration node. Used for improved
   --    error diagnostics: if there is a premature usage of an incomplete
   --    type, a subsequently generated error message indicates the position
   --    of its full declaration.

   --  Present_Expr
   --    Present in an N_Variant node. This has a meaningful value only after
   --    Gigi has back annotated the tree with representation information. At
   --    this point, it contains a reference to a gcc expression that depends
   --    on the values of one or more discriminants. Given a set of
   --    discriminant values, this expression evaluates to False (zero) if
   --    variant is not present, and True (non-zero) if it is present. See
   --    unit Repinfo for further details on gigi back annotation. This field
   --    is used during back-annotation processing (for -gnatR -gnatc) to
   --    determine if a field is present or not.

   --  Prev_Use_Clause
   --    Present in both N_Use_Package_Clause and N_Use_Type_Clause. Used in
   --    detection of ineffective use clauses by allowing a chain of related
   --    clauses together to avoid traversing the current scope stack.

   --  Print_In_Hex
   --    Set on an N_Integer_Literal node to indicate that the value should be
   --    printed in hexadecimal in the sprint listing. Has no effect on
   --    legality or semantics of program, only on the displayed output. This
   --    is used to clarify output from the packed array cases.

   --  Procedure_To_Call
   --    Present in N_Allocator, N_Free_Statement, N_Simple_Return_Statement,
   --    and N_Extended_Return_Statement nodes. References the entity for the
   --    declaration of the procedure to be called to accomplish the required
   --    operation (i.e. for the Allocate procedure in the case of N_Allocator
   --    and N_Simple_Return_Statement and N_Extended_Return_Statement (for
   --    allocating the return value), and for the Deallocate procedure in the
   --    case of N_Free_Statement.

   --  Raises_Constraint_Error
   --    Set on an expression whose evaluation will definitely fail constraint
   --    error check. See Sem_Eval for details.

   --  Redundant_Use
   --    Present in nodes that can appear as an operand in a use clause or use
   --    type clause (identifiers, expanded names, attribute references). Set
   --    to indicate that a use is redundant (and therefore need not be undone
   --    on scope exit).

   --  Renaming_Exception
   --    Present in N_Exception_Declaration node. Used to point back to the
   --    exception renaming for an exception declared within a subprogram.
   --    What happens is that an exception declared in a subprogram is moved
   --    to the library level with a unique name, and the original exception
   --    becomes a renaming. This link from the library level exception to the
   --    renaming declaration allows registering of the proper exception name.

   --  Return_Statement_Entity
   --    Present in N_Simple_Return_Statement and N_Extended_Return_Statement.
   --    Points to an E_Return_Statement representing the return statement.

   --  Return_Object_Declarations
   --    Present in N_Extended_Return_Statement. Points to a list initially
   --    containing a single N_Object_Declaration representing the return
   --    object. We use a list (instead of just a pointer to the object decl)
   --    because Analyze wants to insert extra actions on this list, before the
   --    N_Object_Declaration, which always remains last on the list.

   --  Rounded_Result
   --    Present in N_Type_Conversion, N_Op_Divide, and N_Op_Multiply nodes.
   --    Used in the fixed-point cases to indicate that the result must be
   --    rounded as a result of the use of the 'Round attribute. Also used for
   --    integer N_Op_Divide nodes to indicate that the result should be
   --    rounded to the nearest integer (breaking ties away from zero), rather
   --    than truncated towards zero as usual. These rounded integer operations
   --    are the result of expansion of rounded fixed-point divide, conversion
   --    and multiplication operations.

   --  Save_Invocation_Graph_Of_Body
   --    Present in compilation unit nodes. Set when the elaboration mechanism
   --    must record all invocation constructs and invocation relations within
   --    the body of the compilation unit.
   --
   --  SCIL_Entity
   --    Present in SCIL nodes. References the specific tagged type associated
   --    with the SCIL node (for an N_SCIL_Dispatching_Call node, this is
   --    the controlling type of the call; for an N_SCIL_Membership_Test node
   --    generated as part of testing membership in T'Class, this is T; for an
   --    N_SCIL_Dispatch_Table_Tag_Init node, this is the type being declared).

   --  SCIL_Controlling_Tag
   --    Present in N_SCIL_Dispatching_Call nodes. References the controlling
   --    tag of a dispatching call. This is usually an N_Selected_Component
   --    node (for a _tag component), but may be an N_Object_Declaration or
   --    N_Parameter_Specification node in some cases (e.g., for a call to
   --    a classwide streaming operation or a call to an instance of
   --    Ada.Tags.Generic_Dispatching_Constructor).

   --  SCIL_Tag_Value
   --    Present in N_SCIL_Membership_Test nodes. Used to reference the tag
   --    of the value that is being tested.

   --  SCIL_Target_Prim
   --    Present in N_SCIL_Dispatching_Call nodes. References the primitive
   --    operation named (statically) in a dispatching call.

   --  Scope
   --    Present in defining identifiers, defining character literals, and
   --    defining operator symbols (i.e. in all entities). The entities of a
   --    scope all use this field to reference the corresponding scope entity.
   --    See Einfo for further details.

   --  Selector_Name
   --    Present in N_Expanded_Name N_Selected_Component,
   --    N_Generic_Association, and N_Parameter_Association nodes.

   --  Shift_Count_OK
   --    A flag present in shift nodes to indicate that the shift count is
   --    known to be in range, i.e. is in the range from zero to word length
   --    minus one. If this flag is not set, then the shift count may be
   --    outside this range, i.e. larger than the word length, and the code
   --    must ensure that such shift counts give the appropriate result.

   --  Source_Type
   --    Used in an N_Validate_Unchecked_Conversion node to point to the
   --    source type entity for the unchecked conversion instantiation
   --    which gigi must do size validation for.

   --  Storage_Pool
   --    Present in N_Allocator, N_Free_Statement, N_Simple_Return_Statement,
   --    and N_Extended_Return_Statement nodes. References the entity for the
   --    storage pool to be used for the allocate or free call or for the
   --    allocation of the returned value from function. Empty indicates that
   --    the global default pool is to be used. Note that in the case
   --    of a return statement, this field is set only if the function returns
   --    value of a type whose size is not known at compile time on the
   --    secondary stack.

   --  Suppress_Assignment_Checks
   --    Used in generated N_Assignment_Statement nodes to suppress predicate
   --    and range checks in cases where the generated code knows that the
   --    value being assigned is in range and satisfies any predicate. Also
   --    can be set in N_Object_Declaration nodes, to similarly suppress any
   --    checks on the initializing value. In assignment statements it also
   --    suppresses access checks in the generated code for out- and in-out
   --    parameters in entry calls, as well as discriminant and length checks.

   --  Suppress_Loop_Warnings
   --    Used in N_Loop_Statement node to indicate that warnings within the
   --    body of the loop should be suppressed. This is set when the range
   --    of a FOR loop is known to be null, or is probably null (loop would
   --    only execute if invalid values are present).

   --  Target
   --    Present in call and variable reference marker nodes. References the
   --    entity of the original entity, operator, or subprogram being invoked,
   --    or the original variable being read or written.

   --  Target_Type
   --    Used in an N_Validate_Unchecked_Conversion node to point to the target
   --    type entity for the unchecked conversion instantiation which gigi must
   --    do size validation for.

   --  Then_Actions
   --    This field is present in if expression nodes. During code expansion
   --    we use the Insert_Actions procedure (in Exp_Util) to insert actions
   --    at an appropriate place in the tree to get elaborated at the right
   --    time. For if expressions, we have to be sure that the actions for
   --    for the Then branch are only elaborated if the condition is True.
   --    The Then_Actions field is used as a temporary parking place for
   --    these actions. The final tree is always rewritten to eliminate the
   --    need for this field, so in the tree passed to Gigi, this field is
   --    always set to No_List.

   --  TSS_Elist
   --    Present in N_Freeze_Entity nodes. Holds an element list containing
   --    entries for each TSS (type support subprogram) associated with the
   --    frozen type. The elements of the list are the entities for the
   --    subprograms (see package Exp_TSS for further details). Set to No_Elist
   --    if there are no type support subprograms for the type or if the freeze
   --    node is not for a type.

   --  Uneval_Old_Accept
   --    Present in N_Pragma nodes. Set True if Opt.Uneval_Old is set to 'A'
   --    (accept) at the point where the pragma is encountered (including the
   --    case of a pragma generated from an aspect specification). It is this
   --    setting that is relevant, rather than the setting at the point where
   --    a contract is finally analyzed after the delay till the freeze point.

   --  Uneval_Old_Warn
   --    Present in N_Pragma nodes. Set True if Opt.Uneval_Old is set to 'W'
   --    (warn) at the point where the pragma is encountered (including the
   --    case of a pragma generated from an aspect specification). It is this
   --    setting that is relevant, rather than the setting at the point where
   --    a contract is finally analyzed after the delay till the freeze point.

   --  Unreferenced_In_Spec
   --    Present in N_With_Clause nodes. Set if the with clause is on the
   --    package or subprogram spec where the main unit is the corresponding
   --    body, and is not referenced by the spec (it may still be referenced by
   --    the body, so this flag is used to generate the proper message (see
   --    Sem_Util.Check_Unused_Withs for details)

   --  Uninitialized_Variable
   --    Present in N_Formal_Private_Type_Definition and in N_Private_
   --    Extension_Declarations. Indicates that a variable in a generic unit
   --    whose type is a formal private or derived type is read without being
   --    initialized. Used to warn if the corresponding actual type is not
   --    a fully initialized type.

   --  Used_Operations
   --    Present in N_Use_Type_Clause nodes. Holds the list of operations that
   --    are made potentially use-visible by the clause. Simplifies processing
   --    on exit from the scope of the use_type_clause, in particular in the
   --    case of Use_All_Type, when those operations several scopes.

   --  Was_Attribute_Reference
   --    Present in N_Subprogram_Body. Set to True if the original source is an
   --    attribute reference which is an actual in a generic instantiation. The
   --    instantiation prologue renames these attributes, and expansion later
   --    converts them into subprogram bodies.

   --  Was_Expression_Function
   --    Present in N_Subprogram_Body. True if the original source had an
   --    N_Expression_Function, which was converted to the N_Subprogram_Body
   --    by Analyze_Expression_Function.

   --  Was_Originally_Stub
   --    This flag is set in the node for a proper body that replaces stub.
   --    During the analysis procedure, stubs in some situations get rewritten
   --    by the corresponding bodies, and we set this flag to remember that
   --    this happened. Note that it is not good enough to rely on the use of
   --    Original_Node here because of the case of nested instantiations where
   --    the substituted node can be copied.

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
   --    Enumeration Definition     );

   --  The End_Label and End_Span fields are used to mark the locations of
   --  these lines, and also keep track of the label in the case where a label
   --  is present.

   --  For the first group above, the End_Label field of the corresponding node
   --  is used to point to the label identifier. In the case where there is no
   --  label in the source, the parser supplies a dummy identifier (with
   --  Comes_From_Source set to False), and the Sloc of this dummy identifier
   --  marks the location of the token following the END token.

   --  For the second group, the use of End_Label is similar, but the End_Label
   --  is found in the N_Handled_Sequence_Of_Statements node. This is done
   --  simply because in some cases there is no room in the parent node.

   --  For the third group, there is never any label, and instead of using
   --  End_Label, we use the End_Span field which gives the location of the
   --  token following END, relative to the starting Sloc of the construct,
   --  i.e. add Sloc (Node) + End_Span (Node) to get the Sloc of the IF or CASE
   --  following the End_Label.

   --  The record definition case is handled specially, we treat it as though
   --  it required an optional label which is never present, and so the parser
   --  always builds a dummy identifier with Comes From Source set False. The
   --  reason we do this, rather than using End_Span in this case, is that we
   --  want to generate a cross-ref entry for the end of a record, since it
   --  represents a scope for name declaration purposes.

   --  The enumeration definition case is handled in an exactly similar manner,
   --  building a dummy identifier to get a cross-reference.

   --  Note: the reason we store the difference as a Uint, instead of storing
   --  the Source_Ptr value directly, is that Source_Ptr values cannot be
   --  distinguished from other types of values, and we count on all general
   --  use fields being self describing. To make things easier for clients,
   --  note that we provide function End_Location, and procedure
   --  Set_End_Location to allow access to the logical value (which is the
   --  Source_Ptr value for the end token).

   ---------------------
   -- Syntactic Nodes --
   ---------------------

      ---------------------
      -- 2.3  Identifier --
      ---------------------

      --  IDENTIFIER ::= IDENTIFIER_LETTER {[UNDERLINE] LETTER_OR_DIGIT}
      --  LETTER_OR_DIGIT ::= IDENTIFIER_LETTER | DIGIT

      --  An IDENTIFIER shall not be a reserved word

      --  In the Ada grammar identifiers are the bottom level tokens which have
      --  very few semantics. Actual program identifiers are direct names. If
      --  we were being 100% honest with the grammar, then we would have a node
      --  called N_Direct_Name which would point to an identifier. However,
      --  that's too many extra nodes, so we just use the N_Identifier node
      --  directly as a direct name, and it contains the expression fields and
      --  Entity field that correspond to its use as a direct name. In those
      --  few cases where identifiers appear in contexts where they are not
      --  direct names (pragmas, pragma argument associations, attribute
      --  references and attribute definition clauses), the Chars field of the
      --  node contains the Name_Id for the identifier name.

      --  Note: in GNAT, a reserved word can be treated as an identifier in two
      --  cases. First, an incorrect use of a reserved word as an identifier is
      --  diagnosed and then treated as a normal identifier. Second, an
      --  attribute designator of the form of a reserved word (access, delta,
      --  digits, range) is treated as an identifier.

      --  Note: The set of letters that is permitted in an identifier depends
      --  on the character set in use. See package Csets for full details.

      --  N_Identifier
      --  Sloc points to identifier
      --  Chars contains the Name_Id for the identifier
      --  Entity
      --  Associated_Node
      --  Original_Discriminant
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Has_Private_View (set in generic units)
      --  Has_Secondary_Private_View (set in generic units)
      --  Redundant_Use
      --  Atomic_Sync_Required
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

      --  Note: the value of an integer literal node created by the front end
      --  is never outside the range of values of the base type. However, it
      --  can be the case that the created value is outside the range of the
      --  particular subtype. This happens in the case of integer overflows
      --  with checks suppressed.

      --  N_Integer_Literal
      --  Sloc points to literal
      --  Original_Entity If not Empty, holds Named_Number that
      --  has been constant-folded into its literal value.
      --  Intval contains integer value of literal
      --  Print_In_Hex
      --  plus fields for expression

      --  N_Real_Literal
      --  Sloc points to literal
      --  Original_Entity If not Empty, holds Named_Number that
      --  has been constant-folded into its literal value.
      --  Realval contains real value of literal
      --  Corresponding_Integer_Value
      --  Is_Machine_Number
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
      --  Chars contains the Name_Id for the identifier
      --  Char_Literal_Value contains the literal value
      --  Entity
      --  Associated_Node
      --  Has_Private_View (set in generic units)
      --  Has_Secondary_Private_View (set in generic units)
      --  plus fields for expression

      --  Note: the Entity field will be missing (set to Empty) for character
      --  literals whose type is Standard.Wide_Character or Standard.Character
      --  or a type derived from one of these two. In this case the character
      --  literal stands for its own coding. The reason we take this irregular
      --  short cut is to avoid the need to build lots of junk defining
      --  character literal nodes.

      -------------------------
      -- 2.6  String Literal --
      -------------------------

      --  STRING LITERAL ::= "{STRING_ELEMENT}"

      --  A STRING_ELEMENT is either a pair of quotation marks ("), or a
      --  single GRAPHIC_CHARACTER other than a quotation mark.
      --
      --  Is_Folded_In_Parser is True if the parser created this literal by
      --  folding a sequence of "&" operators. For example, if the source code
      --  says "aaa" & "bbb" & "ccc", and this produces "aaabbbccc", the flag
      --  is set. This flag is needed because the parser doesn't know about
      --  visibility, so the folded result might be wrong, and semantic
      --  analysis needs to check for that.

      --  N_String_Literal
      --  Sloc points to literal
      --  Strval contains Id of string value
      --  Has_Wide_Character
      --  Has_Wide_Wide_Character
      --  Is_Folded_In_Parser
      --  Is_Interpolated_String_Literal
      --  plus fields for expression

      ---------------------------------------
      --  2.6  Interpolated String Literal --
      ---------------------------------------

      --  INTERPOLATED_STRING_LITERAL ::=
      --    'f' "{INTERPOLATED_STRING_ELEMENT}"

      --  INTERPOLATED_STRING_ELEMENT ::=
      --      ESCAPED_CHARACTER | INTERPOLATED_EXPRESSION
      --    | non_quotation_mark_non_left_brace_GRAPHIC_CHARACTER

      --  ESCAPED_CHARACTER ::= '\GRAPHIC_CHARACTER'

      --  INTERPOLATED_EXPRESSION ::= '{' EXPRESSION '}'

      --  Most of these syntax rules are omitted as tree nodes to simplify
      --  semantic processing. The scanner handles escaped characters as part
      --  of processing an interpolated string literal, and the parser stores
      --  in the Expressions field of this node a list containing the sequence
      --  of string literals and the roots of the interpolated expressions.

      --  N_Interpolated_String_Literal
      --  Sloc points to literal
      --  Expressions
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
      --  Next_Pragma
      --  Pragma_Argument_Associations (set to No_List if none)
      --  Corresponding_Aspect (set to Empty if not present)
      --  Pragma_Identifier
      --  Next_Rep_Item
      --  Is_Generic_Contract_Pragma
      --  Is_Checked_Ghost_Pragma
      --  Is_Inherited_Pragma
      --  Is_Analyzed_Pragma
      --  Class_Present set if from Aspect with 'Class
      --  Uneval_Old_Accept
      --  Is_Ignored_Ghost_Pragma
      --  Is_Ignored
      --  Is_Checked
      --  From_Aspect_Specification
      --  Is_Delayed_Aspect
      --  Is_Disabled
      --  Import_Interface_Present
      --  Uneval_Old_Warn

      --  Note: we should have a section on what pragmas are passed on to
      --  the back end to be processed. This section should note that pragma
      --  Psect_Object is always converted to Common_Object, but there are
      --  undoubtedly many other similar notes required ???

      --  Note: utility functions Pragma_Name_Unmapped and Pragma_Name may be
      --  applied to pragma nodes to obtain the Chars or its mapped version.

      --  Note: if From_Aspect_Specification is set, then Sloc points to the
      --  aspect name, as does the Pragma_Identifier. In this case if the
      --  pragma has a local name argument (such as pragma Inline), it is
      --  resolved to point to the specific entity affected by the pragma.

      --------------------------------------
      -- 2.8  Pragma Argument Association --
      --------------------------------------

      --  PRAGMA_ARGUMENT_ASSOCIATION ::=
      --    [pragma_argument_IDENTIFIER =>] NAME
      --  | [pragma_argument_IDENTIFIER =>] EXPRESSION

      --  In Ada 2012, there are two more possibilities:

      --  PRAGMA_ARGUMENT_ASSOCIATION ::=
      --    [pragma_argument_ASPECT_MARK =>] NAME
      --  | [pragma_argument_ASPECT_MARK =>] EXPRESSION

      --  where the interesting allowed cases (which do not fit the syntax of
      --  the first alternative above) are

      --  ASPECT_MARK => Pre'Class |
      --                 Post'Class |
      --                 Type_Invariant'Class |
      --                 Invariant'Class

      --  We allow this special usage in all Ada modes, but it would be a
      --  pain to allow these aspects to pervade the pragma syntax, and the
      --  representation of pragma nodes internally. So what we do is to
      --  replace these ASPECT_MARK forms with identifiers whose name is one
      --  of the special internal names _Pre, _Post, or _Type_Invariant.

      --  We do a similar replacement of these Aspect_Mark forms in the
      --  Expression of a pragma argument association for the cases of
      --  the first arguments of any Check pragmas and Check_Policy pragmas

      --  N_Pragma_Argument_Association
      --  Sloc points to first token in association
      --  Chars (set to No_Name if no pragma argument identifier)
      --  Expression_Copy
      --  Expression

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

      --  N_Defining_Identifier
      --  Sloc points to identifier
      --  Chars contains the Name_Id for the identifier
      --  Next_Entity
      --  Scope
      --  Etype

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
      --      is TYPE_DEFINITION
      --        [ASPECT_SPECIFICATIONS];
      --  | TASK_TYPE_DECLARATION
      --  | PROTECTED_TYPE_DECLARATION

      --  The full type declaration node is used only for the first case. The
      --  second case (concurrent type declaration), is represented directly
      --  by a task type declaration or a protected type declaration.

      --  N_Full_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier
      --  Incomplete_View
      --  Discriminant_Specifications (set to No_List if none)
      --  Type_Definition
      --  Discr_Check_Funcs_Built

      ----------------------------
      -- 3.2.1  Type Definition --
      ----------------------------

      --  TYPE_DEFINITION ::=
      --    ENUMERATION_TYPE_DEFINITION  | INTEGER_TYPE_DEFINITION
      --  | REAL_TYPE_DEFINITION         | ARRAY_TYPE_DEFINITION
      --  | RECORD_TYPE_DEFINITION       | ACCESS_TYPE_DEFINITION
      --  | DERIVED_TYPE_DEFINITION      | INTERFACE_TYPE_DEFINITION

      --------------------------------
      -- 3.2.2  Subtype Declaration --
      --------------------------------

      --  SUBTYPE_DECLARATION ::=
      --    subtype DEFINING_IDENTIFIER is [NULL_EXCLUSION] SUBTYPE_INDICATION
      --      [ASPECT_SPECIFICATIONS];

      --  The subtype indication field is set to Empty for subtypes
      --  declared in package Standard (Positive, Natural).

      --  N_Subtype_Declaration
      --  Sloc points to SUBTYPE
      --  Defining_Identifier
      --  Null_Exclusion_Present
      --  Subtype_Indication
      --  Generic_Parent_Type (for actual of formal private or derived type)
      --  Exception_Junk

      -------------------------------
      -- 3.2.2  Subtype Indication --
      -------------------------------

      --  SUBTYPE_INDICATION ::= SUBTYPE_MARK [CONSTRAINT]

      --  Note: if no constraint is present, the subtype indication appears
      --  directly in the tree as a subtype mark. The N_Subtype_Indication
      --  node is used only if a constraint is present.

      --  Note: [For Ada 2005 (AI-231)]: Because Ada 2005 extends this rule
      --  with the null-exclusion part (see AI-231), we had to introduce a new
      --  attribute in all the parents of subtype_indication nodes to indicate
      --  if the null-exclusion is present.

      --  Note: the reason that this node has expression fields is that a
      --  subtype indication can appear as an operand of a membership test.

      --  N_Subtype_Indication
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark
      --  Constraint
      --  Etype
      --  Must_Not_Freeze

      --  Note: Depending on context, the Etype is either the entity of the
      --  Subtype_Mark field, or it is an itype constructed to reify the
      --  subtype indication. In particular, such itypes are created for a
      --  subtype indication that appears in an array type declaration. This
      --  simplifies constraint checking in indexed components.

      --  For subtype indications that appear in scalar type and subtype
      --  declarations, the Etype is the entity of the subtype mark.

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
      --      [NULL_EXCLUSION] SUBTYPE_INDICATION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ACCESS_DEFINITION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ARRAY_TYPE_DEFINITION [:= EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | SINGLE_TASK_DECLARATION
      --  | SINGLE_PROTECTED_DECLARATION

      --  Note: aliased is not permitted in Ada 83 mode

      --  The N_Object_Declaration node is only for the first three cases.
      --  Single task declaration is handled by P_Task (9.1)
      --  Single protected declaration is handled by P_protected (9.5)

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  The flag Has_Init_Expression is set if an initializing expression
      --  is present. Normally it is set if and only if Expression contains
      --  a non-empty value, but there is an exception to this. When the
      --  initializing expression is an aggregate which requires explicit
      --  assignments, the Expression field gets set to Empty, but this flag
      --  is still set, so we don't forget we had an initializing expression.

      --  Note: if a range check is required for the initialization
      --  expression then the Do_Range_Check flag is set in the Expression,
      --  with the check being done against the type given by the object
      --  definition, which is also the Etype of the defining identifier.

      --  Note: the contents of the Expression field must be ignored (i.e.
      --  treated as though it were Empty) if No_Initialization is set True.

      --  Note: the back end places some restrictions on the form of the
      --  Expression field. If the object being declared is Atomic, then
      --  the Expression may not have the form of an aggregate (since this
      --  might cause the back end to generate separate assignments). In this
      --  case the front end must generate an extra temporary and initialize
      --  this temporary as required (the temporary itself is not atomic).

      --  Note: there is no node kind for object definition. Instead, the
      --  corresponding field holds a subtype indication, an array type
      --  definition, or (Ada 2005, AI-406) an access definition.

      --  N_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier
      --  Aliased_Present
      --  Constant_Present set if CONSTANT appears
      --  Null_Exclusion_Present
      --  Object_Definition subtype indic./array type def./access def.
      --  Expression (set to Empty if not present)
      --  Handler_List_Entry
      --  Corresponding_Generic_Association
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)
      --  No_Initialization
      --  Assignment_OK
      --  Exception_Junk
      --  Is_Subprogram_Descriptor
      --  Has_Init_Expression
      --  Suppress_Assignment_Checks

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
      --  Defining_Identifier
      --  Expression
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)

      ----------------------------------
      -- 3.4  Derived Type Definition --
      ----------------------------------

      --  DERIVED_TYPE_DEFINITION ::=
      --    [abstract] [limited] new [NULL_EXCLUSION] parent_SUBTYPE_INDICATION
      --    [[and INTERFACE_LIST] RECORD_EXTENSION_PART]

      --  Note: ABSTRACT, LIMITED, and record extension part are not permitted
      --  in Ada 83 mode.

      --  Note: a record extension part is required if ABSTRACT is present

      --  N_Derived_Type_Definition
      --  Sloc points to NEW
      --  Abstract_Present
      --  Null_Exclusion_Present (set to False if not present)
      --  Subtype_Indication
      --  Record_Extension_Part (set to Empty if not present)
      --  Limited_Present
      --  Task_Present set in task interfaces
      --  Protected_Present set in protected interfaces
      --  Synchronized_Present set in interfaces
      --  Interface_List (set to No_List if none)
      --  Interface_Present set in abstract interfaces

      --  Note: Task_Present, Protected_Present, Synchronized_Present,
      --        Interface_List, and Interface_Present are used for abstract
      --        interfaces (see comments for INTERFACE_TYPE_DEFINITION).

      ---------------------------
      -- 3.5  Range Constraint --
      ---------------------------

      --  RANGE_CONSTRAINT ::= range RANGE

      --  N_Range_Constraint
      --  Sloc points to RANGE
      --  Range_Expression

      ----------------
      -- 3.5  Range --
      ----------------

      --  RANGE ::=
      --    RANGE_ATTRIBUTE_REFERENCE
      --  | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

      --  Note: the case of a range given as a range attribute reference
      --  appears directly in the tree as an attribute reference.

      --  Note: the field name for a reference to a range is Range_Expression
      --  rather than Range, because range is a reserved keyword in Ada.

      --  Note: the reason that this node has expression fields is that a
      --  range can appear as an operand of a membership test. The Etype
      --  field is the type of the range (we do NOT construct an implicit
      --  subtype to represent the range exactly).

      --  N_Range
      --  Sloc points to ..
      --  Low_Bound
      --  High_Bound
      --  Cannot_Be_Superflat
      --  Includes_Infinities
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
      --  Literals (Empty for CHARACTER or WIDE_CHARACTER)
      --  End_Label (set to Empty if internally generated record)

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

      --  N_Defining_Character_Literal
      --  Sloc points to literal
      --  Chars contains the Name_Id for the identifier
      --  Next_Entity
      --  Scope
      --  Etype

      ------------------------------------
      -- 3.5.4  Integer Type Definition --
      ------------------------------------

      --  Note: there is an error in this rule in the latest version of the
      --  grammar, so we have retained the old rule pending clarification.

      --  INTEGER_TYPE_DEFINITION ::=
      --    SIGNED_INTEGER_TYPE_DEFINITION
      --  | MODULAR_TYPE_DEFINITION

      -------------------------------------------
      -- 3.5.4  Signed Integer Type Definition --
      -------------------------------------------

      --  SIGNED_INTEGER_TYPE_DEFINITION ::=
      --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

      --  Note: the Low_Bound and High_Bound fields are set to Empty
      --  for integer types defined in package Standard.

      --  N_Signed_Integer_Type_Definition
      --  Sloc points to RANGE
      --  Low_Bound
      --  High_Bound

      ------------------------------------
      -- 3.5.4  Modular Type Definition --
      ------------------------------------

      --  MODULAR_TYPE_DEFINITION ::= mod static_EXPRESSION

      --  N_Modular_Type_Definition
      --  Sloc points to MOD
      --  Expression

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
      --  Digits_Expression
      --  Real_Range_Specification (set to Empty if not present)

      -------------------------------------
      -- 3.5.7  Real Range Specification --
      -------------------------------------

      --  REAL_RANGE_SPECIFICATION ::=
      --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

      --  N_Real_Range_Specification
      --  Sloc points to RANGE
      --  Low_Bound
      --  High_Bound

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

      --  N_Ordinary_Fixed_Point_Definition
      --  Sloc points to DELTA
      --  Delta_Expression
      --  Real_Range_Specification

      -------------------------------------------
      -- 3.5.9  Decimal Fixed Point Definition --
      -------------------------------------------

      --  DECIMAL_FIXED_POINT_DEFINITION ::=
      --    delta static_EXPRESSION
      --      digits static_EXPRESSION [REAL_RANGE_SPECIFICATION]

      --  Note: decimal types are not permitted in Ada 83 mode

      --  N_Decimal_Fixed_Point_Definition
      --  Sloc points to DELTA
      --  Delta_Expression
      --  Digits_Expression
      --  Real_Range_Specification (set to Empty if not present)

      ------------------------------
      -- 3.5.9  Digits Constraint --
      ------------------------------

      --  DIGITS_CONSTRAINT ::=
      --    digits static_EXPRESSION [RANGE_CONSTRAINT]

      --  Note: in Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION
      --  Note: in Ada 95, reduced accuracy subtypes are obsolescent

      --  N_Digits_Constraint
      --  Sloc points to DIGITS
      --  Digits_Expression
      --  Range_Constraint (set to Empty if not present)

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
      --  Subtype_Marks
      --  Component_Definition

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
      --  Discrete_Subtype_Definitions
      --  Component_Definition

      --  Note: although the language allows the full syntax for discrete
      --  subtype definitions (i.e. a discrete subtype indication or a range),
      --  in the generated tree, we always rewrite these as N_Range nodes.

      --------------------------------------
      -- 3.6  Discrete Subtype Definition --
      --------------------------------------

      --  DISCRETE_SUBTYPE_DEFINITION ::=
      --    discrete_SUBTYPE_INDICATION | RANGE

      -------------------------------
      -- 3.6  Component Definition --
      -------------------------------

      --  COMPONENT_DEFINITION ::=
      --    [aliased] [NULL_EXCLUSION] SUBTYPE_INDICATION | ACCESS_DEFINITION

      --  Note: although the syntax does not permit a component definition to
      --  be an anonymous array (and the parser will diagnose such an attempt
      --  with an appropriate message), it is possible for anonymous arrays
      --  to appear as component definitions. The semantics and back end handle
      --  this case properly, and the expander in fact generates such cases.
      --  Access_Definition is an optional field that gives support to
      --  Ada 2005 (AI-230). The parser generates nodes that have either the
      --  Subtype_Indication field or else the Access_Definition field.

      --  N_Component_Definition
      --  Sloc points to ALIASED, ACCESS, or to first token of subtype mark
      --  Aliased_Present
      --  Null_Exclusion_Present
      --  Subtype_Indication (set to Empty if not present)
      --  Access_Definition (set to Empty if not present)

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
      --    DEFINING_IDENTIFIER_LIST : [NULL_EXCLUSION] SUBTYPE_MARK
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
      --  Defining_Identifier
      --  Null_Exclusion_Present
      --  Discriminant_Type subtype mark or access parameter definition
      --  Expression (set to Empty if no default expression)
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)

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
      --  Constraints points to list of discrete ranges or
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
      --  Selector_Names (always non-empty, since if no selector
      --    names are present, this node is not used, see comment above)
      --  Expression

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

      --  Note: the Abstract_Present, Tagged_Present, and Limited_Present
      --  flags appear only for a record definition appearing in a record
      --  type definition.

      --  Note: the NULL RECORD case is not permitted in Ada 83

      --  N_Record_Definition
      --  Sloc points to RECORD or NULL
      --  End_Label (set to Empty if internally generated record)
      --  Abstract_Present
      --  Tagged_Present
      --  Limited_Present
      --  Component_List empty in null record case
      --  Null_Present set in null record case
      --  Task_Present set in task interfaces
      --  Protected_Present set in protected interfaces
      --  Synchronized_Present set in interfaces
      --  Interface_Present set in abstract interfaces
      --  Interface_List (set to No_List if none)

      --  Note: Task_Present, Protected_Present, Synchronized _Present,
      --        Interface_List and Interface_Present are used for abstract
      --        interfaces (see comments for INTERFACE_TYPE_DEFINITION).

      -------------------------
      -- 3.8  Component List --
      -------------------------

      --  COMPONENT_LIST ::=
      --    COMPONENT_ITEM {COMPONENT_ITEM}
      --  | {COMPONENT_ITEM} VARIANT_PART
      --  | null;

      --  N_Component_List
      --  Sloc points to first token of component list
      --  Component_Items
      --  Variant_Part (set to Empty if no variant part)
      --  Null_Present

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
      --        [ASPECT_SPECIFICATIONS];

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
      --  Defining_Identifier
      --  Component_Definition
      --  Expression (set to Empty if no default expression)
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)

      -------------------------
      -- 3.8.1  Variant Part --
      -------------------------

      --  VARIANT_PART ::=
      --    case discriminant_DIRECT_NAME is
      --      VARIANT {VARIANT}
      --    end case;

      --  Note: the variants list can contain pragmas as well as variants.
      --  In a properly formed program there is at least one variant.

      --  N_Variant_Part
      --  Sloc points to CASE
      --  Name
      --  Variants

      --------------------
      -- 3.8.1  Variant --
      --------------------

      --  VARIANT ::=
      --    when DISCRETE_CHOICE_LIST =>
      --      COMPONENT_LIST

      --  N_Variant
      --  Sloc points to WHEN
      --  Discrete_Choices
      --  Component_List
      --  Enclosing_Variant
      --  Present_Expr
      --  Dcheck_Function
      --  Has_SP_Choice

      --  Note: in the list of Discrete_Choices, the tree passed to the back
      --  end does not have choice entries corresponding to names of statically
      --  predicated subtypes. Such entries are always expanded out to the list
      --  of equivalent values or ranges.

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
      --  discrete range) appear directly. N_Others_Choice is also used
      --  in exception handlers and generic formal packages.

      --  Note: in accordance with the syntax, the parser does not check that
      --  OTHERS appears at the end on its own in a choice list context. This
      --  is a semantic check.

      --  N_Others_Choice
      --  Sloc points to OTHERS
      --  Others_Discrete_Choices
      --  All_Others

      ----------------------------------
      -- 3.9.1  Record Extension Part --
      ----------------------------------

      --  RECORD_EXTENSION_PART ::= with RECORD_DEFINITION

      --  Note: record extension parts are not permitted in Ada 83 mode

      --------------------------------------
      -- 3.9.4  Interface Type Definition --
      --------------------------------------

      --  INTERFACE_TYPE_DEFINITION ::=
      --    [limited | task | protected | synchronized]
      --    interface [interface_list]

      --  Note: Interfaces are implemented with N_Record_Definition and
      --        N_Derived_Type_Definition nodes because most of the support
      --        for the analysis of abstract types has been reused to
      --        analyze abstract interfaces.

      ----------------------------------
      -- 3.10  Access Type Definition --
      ----------------------------------

      --  ACCESS_TYPE_DEFINITION ::=
      --    ACCESS_TO_OBJECT_DEFINITION
      --  | ACCESS_TO_SUBPROGRAM_DEFINITION

      --------------------------
      -- 3.10  Null Exclusion --
      --------------------------

      --  NULL_EXCLUSION ::= not null

      ---------------------------------------
      -- 3.10  Access To Object Definition --
      ---------------------------------------

      --  ACCESS_TO_OBJECT_DEFINITION ::=
      --    [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER]
      --    SUBTYPE_INDICATION

      --  N_Access_To_Object_Definition
      --  Sloc points to ACCESS
      --  All_Present
      --  Null_Exclusion_Present
      --  Null_Excluding_Subtype
      --  Subtype_Indication
      --  Constant_Present

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
      --    [NULL_EXCLUSION] access [protected] procedure PARAMETER_PROFILE
      --  | [NULL_EXCLUSION] access [protected] function
      --    PARAMETER_AND_RESULT_PROFILE

      --  Note: access to subprograms are not permitted in Ada 83 mode

      --  N_Access_Function_Definition
      --  Sloc points to ACCESS
      --  Null_Exclusion_Present
      --  Null_Exclusion_In_Return_Present
      --  Protected_Present
      --  Parameter_Specifications (set to No_List if no formal part)
      --  Result_Definition result subtype (subtype mark or access def)

      --  N_Access_Procedure_Definition
      --  Sloc points to ACCESS
      --  Null_Exclusion_Present
      --  Protected_Present
      --  Parameter_Specifications (set to No_List if no formal part)

      -----------------------------
      -- 3.10  Access Definition --
      -----------------------------

      --  ACCESS_DEFINITION ::=
      --    [NULL_EXCLUSION] access [GENERAL_ACCESS_MODIFIER] SUBTYPE_MARK
      --  | ACCESS_TO_SUBPROGRAM_DEFINITION

      --  Note: access to subprograms are an Ada 2005 (AI-254) extension

      --  N_Access_Definition
      --  Sloc points to ACCESS
      --  Null_Exclusion_Present
      --  All_Present
      --  Constant_Present
      --  Subtype_Mark
      --  Access_To_Subprogram_Definition (set to Empty if not present)

      -----------------------------------------
      -- 3.10.1  Incomplete Type Declaration --
      -----------------------------------------

      --  INCOMPLETE_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART] [IS TAGGED];

      --  N_Incomplete_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier
      --  Discriminant_Specifications (set to No_List if no
      --   discriminant part, or if the discriminant part is an
      --   unknown discriminant part)
      --  Premature_Use used for improved diagnostics.
      --  Unknown_Discriminants_Present set if (<>) discriminant
      --  Tagged_Present

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
      --  Prefix
      --  Actual_Designated_Subtype
      --  Has_Dereference_Action
      --  Atomic_Sync_Required
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
      --  should be a function call. The semantic pass must correct this
      --  misidentification (which is inevitable at the parser level).

      --  N_Indexed_Component
      --  Sloc contains a copy of the Sloc value of the Prefix
      --  Prefix
      --  Expressions
      --  Atomic_Sync_Required
      --  Generalized_Indexing
      --  Kill_Range_Check
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
      --  Prefix
      --  Discrete_Range
      --  plus fields for expression

      -------------------------------
      -- 4.1.3  Selected Component --
      -------------------------------

      --  SELECTED_COMPONENT ::= PREFIX . SELECTOR_NAME

      --  Note: selected components that are semantically expanded names get
      --  changed during semantic processing into the separate N_Expanded_Name
      --  node. See description of this node in the section on semantic nodes.

      --  N_Selected_Component
      --  Sloc points to the period
      --  Prefix
      --  Selector_Name
      --  Associated_Node
      --  Do_Discriminant_Check
      --  Is_In_Discriminant_Check
      --  Atomic_Sync_Required
      --  Is_Prefixed_Call
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

      --  Note: the generated code may contain stream attributes applied to
      --  limited types for which no stream routines exist officially. In such
      --  case, the result is to use the stream attribute for the underlying
      --  full type, or in the case of a protected type, the components
      --  (including any discriminants) are merely streamed in order.

      --  See Exp_Attr for a complete description of which attributes are
      --  passed onto Gigi, and which are handled entirely by the front end.

      --  Gigi restriction: For the Pos attribute, the prefix cannot be
      --  a non-standard enumeration type or a nonzero/zero semantics
      --  boolean type, so the value is simply the stored representation.

      --  Gigi requirement: For the Mechanism_Code attribute, if the prefix
      --  references a subprogram that is a renaming, then the front end must
      --  rewrite the attribute to refer directly to the renamed entity.

      --  Note: syntactically the prefix of an attribute reference must be a
      --  name, and this (somewhat artificial) requirement is enforced by the
      --  parser. However, for many attributes, such as 'Valid, it is quite
      --  reasonable to apply the attribute to any value, and hence to any
      --  expression. Internally in the tree, the prefix is an expression which
      --  does not have to be a name, and this is handled fine by the semantic
      --  analysis and expansion, and back ends. This arises for the case of
      --  attribute references built by the expander (e.g. 'Valid for the case
      --  of an implicit validity check).

      --  Note: In generated code, the Address and Unrestricted_Access
      --  attributes can be applied to any expression, and the meaning is
      --  to create an object containing the value (the object is in the
      --  current stack frame), and pass the address of this value. If the
      --  Must_Be_Byte_Aligned flag is set, then the object whose address
      --  is taken must be on a byte (storage unit) boundary, and if it is
      --  not (or may not be), then the generated code must create a copy
      --  that is byte aligned, and pass the address of this copy.

      --  N_Attribute_Reference
      --  Sloc points to apostrophe
      --  Prefix (general expression, see note above)
      --  Attribute_Name identifier name from attribute designator
      --  Expressions (set to No_List if no associated expressions)
      --  Entity used if the attribute yields a type
      --  Associated_Node
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Header_Size_Added
      --  Redundant_Use
      --  Must_Be_Byte_Aligned
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

      --  Note: if ACCESS, DELTA, or DIGITS appears in an attribute
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
      --  Expressions (set to No_List if none or null record case)
      --  Component_Associations (set to No_List if none)
      --  Null_Record_Present
      --  Aggregate_Bounds (array) or Ancestor_Type (record)
      --  Associated_Node
      --  Compile_Time_Known_Aggregate
      --  Expansion_Delayed
      --  Has_Self_Reference
      --  Is_Homogeneous_Aggregate
      --  Is_Parenthesis_Aggregate
      --  plus fields for expression

      --  Note: this structure is used for both record and array aggregates
      --  since the two cases are not separable by the parser. The parser
      --  makes no attempt to enforce consistency here, so it is up to the
      --  semantic phase to make sure that the aggregate is consistent (i.e.
      --  that it is not a "half-and-half" case that mixes record and array
      --  syntax). In particular, for a record aggregate, the expressions
      --  field will be set if there are positional associations.

      --  Note: N_Aggregate is not used for all aggregates; in particular,
      --  there is a separate node kind for extension aggregates.

      --  Note: gigi/gcc can handle array aggregates correctly providing that
      --  they are entirely positional, and the array subtype involved has a
      --  known at compile time length and is not bit packed, or a convention
      --  Fortran array with more than one dimension. If these conditions
      --  are not met, then the front end must translate the aggregate into
      --  an appropriate set of assignments into a temporary.

      --  Note: for the record aggregate case, gigi/gcc can handle most cases
      --  of record aggregates, including those for packed, and rep-claused
      --  records, and also variant records, providing that there are no
      --  variable length fields whose size is not known at compile time,
      --  and providing that the aggregate is presented in fully named form.

      --  The other situation in which array aggregates and record aggregates
      --  cannot be passed to the back end is if assignment to one or more
      --  components itself needs expansion, e.g. in the case of an assignment
      --  of an object of a controlled type. In such cases, the front end
      --  must expand the aggregate to a series of assignments, and apply
      --  the required expansion to the individual assignment statements.

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
      --  Choices
      --  Expression (empty if Box_Present)
      --  Loop_Actions
      --  Box_Present
      --  Inherited_Discriminant
      --  Binding_Chars

      --  Note: this structure is used for both record component associations
      --  and array component associations, since the two cases aren't always
      --  separable by the parser. The choices list may represent either a
      --  list of selector names in the record aggregate case, or a list of
      --  discrete choices in the array aggregate case or an N_Others_Choice
      --  node (which appears as a singleton list). Box_Present gives support
      --  to Ada 2005 (AI-287). Binding_Chars is only set if GNAT extensions
      --  are enabled and the given component association occurs within a
      --  choice_expression; in this case, it is the Name_Id, if any, specified
      --  via either of two syntactic forms: "Foo => Bar is Abc" or
      --  "Foo => <Abc>".

      ----------------------------------
      -- 4.3.1  Component Choice List --
      ----------------------------------

      --  COMPONENT_CHOICE_LIST ::=
      --    component_SELECTOR_NAME {| component_SELECTOR_NAME}
      --  | others

      --  The entries of a component choice list appear in the Choices list of
      --  the associated N_Component_Association, as either selector names, or
      --  as an N_Others_Choice node.

      --------------------------------
      -- 4.3.2  Extension Aggregate --
      --------------------------------

      --  EXTENSION_AGGREGATE ::=
      --    (ANCESTOR_PART with RECORD_COMPONENT_ASSOCIATION_LIST)

      --  Note: extension aggregates are not permitted in Ada 83 mode

      --  N_Extension_Aggregate
      --  Sloc points to left parenthesis
      --  Ancestor_Part
      --  Associated_Node
      --  Expressions (set to No_List if none or null record case)
      --  Component_Associations (set to No_List if none)
      --  Null_Record_Present
      --  Expansion_Delayed
      --  Has_Self_Reference
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
      --    (ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION})

      --  See Record_Aggregate (4.3.1) for node structure

      ----------------------------------------
      -- 4.3.3  Array Component Association --
      ----------------------------------------

      --  ARRAY_COMPONENT_ASSOCIATION ::=
      --    DISCRETE_CHOICE_LIST => EXPRESSION
      --  | ITERATED_COMPONENT_ASSOCIATION

      --  See Record_Component_Association (4.3.1) for node structure
      --  The iterated_component_association is introduced into the
      --  Corrigendum of Ada_2012 by AI12-061.

      ------------------------------------------
      -- 4.3.3 Iterated component Association --
      ------------------------------------------

      --  ITERATED_COMPONENT_ASSOCIATION ::=
      --    for DEFINING_IDENTIFIER in DISCRETE_CHOICE_LIST => EXPRESSION
      --    for ITERATOR_SPECIFICATION => EXPRESSION

      --  At most one of (Defining_Identifier, Iterator_Specification)
      --  is present at a time, in which case the other one is empty.
      --  The Reverse_Present flag is present for cases where semantic analysis
      --  later changes the association to have an N_Iterator_Specification
      --  rather than a Defining_Identifier (due to the "discrete choice"
      --  being resolved as an iterator name), and needs to set that flag on
      --  the N_Iterator_Specification node.

      --  N_Iterated_Component_Association
      --  Sloc points to FOR
      --  Defining_Identifier
      --  Iterator_Specification
      --  Expression
      --  Discrete_Choices
      --  Reverse_Present
      --  Loop_Actions
      --  Box_Present

      --  Note that Box_Present is always False, but it is intentionally added
      --  for completeness.

      ----------------------------
      --  4.3.4 Delta Aggregate --
      ----------------------------

      --  N_Delta_Aggregate
      --  Sloc points to left parenthesis
      --  Expression
      --  Component_Associations
      --  Etype

      ---------------------------------
      --  4.3.5 Container_Aggregates --
      ---------------------------------

      --  ITERATED_ELEMENT_ASSOCIATION ::=
      --    for LOOP_PARAMETER_SPECIFICATION[ use KEY_EXPRESSION] => EXPRESSION
      --  | for ITERATOR_SPECIFICATION[ use KEY_EXPRESSION] => EXPRESSION

      --  N_Iterated_Element_Association
      --  Key_Expression
      --  Iterator_Specification
      --  Expression
      --  Loop_Parameter_Specification
      --  Loop_Actions
      --  Box_Present

      --  Exactly one of Iterator_Specification or Loop_Parameter_
      --  specification is present. If the Key_Expression is absent,
      --  the construct is parsed as an Iterated_Component_Association,
      --  and legality checks are performed during semantic analysis.

      --  Both iterated associations are Ada 2022 features that are
      --  expanded during aggregate construction, and do not appear in
      --  expanded code.

      --------------------------------------------------
      -- 4.4  Expression/Relation/Term/Factor/Primary --
      --------------------------------------------------

      --  EXPRESSION ::=
      --    RELATION {LOGICAL_OPERATOR RELATION}

      --  CHOICE_EXPRESSION ::=
      --    CHOICE_RELATION {LOGICAL_OPERATOR CHOICE_RELATION}

      --  CHOICE_RELATION ::=
      --    SIMPLE_EXPRESSION [RELATIONAL_OPERATOR SIMPLE_EXPRESSION]

      --  RELATION ::=
      --    SIMPLE_EXPRESSION [not] in MEMBERSHIP_CHOICE_LIST
      --  | RAISE_EXPRESSION

      --  MEMBERSHIP_CHOICE_LIST ::=
      --    MEMBERSHIP_CHOICE {'|' MEMBERSHIP CHOICE}

      --  MEMBERSHIP_CHOICE ::=
      --    CHOICE_EXPRESSION | RANGE | SUBTYPE_MARK

      --  LOGICAL_OPERATOR ::= and | and then | or | or else | xor

      --  SIMPLE_EXPRESSION ::=
      --    [UNARY_ADDING_OPERATOR] TERM {BINARY_ADDING_OPERATOR TERM}

      --  TERM ::= FACTOR {MULTIPLYING_OPERATOR FACTOR}

      --  FACTOR ::= PRIMARY [** PRIMARY] | abs PRIMARY | not PRIMARY

      --  No nodes are generated for any of these constructs. Instead, the
      --  node for the operator appears directly. When we refer to an
      --  expression in this description, we mean any of the possible
      --  constituent components of an expression (e.g. identifier is
      --  an example of an expression).

      --  Note: the above syntax is that Ada 2012 syntax which restricts
      --  choice relations to simple expressions to avoid ambiguities in
      --  some contexts with set membership notation. It has been decided
      --  that in retrospect, the Ada 95 change allowing general expressions
      --  in this context was a mistake, so we have reverted to the above
      --  syntax in Ada 95 and Ada 2005 modes (the restriction to simple
      --  expressions was there in Ada 83 from the start).

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
      --  the maximum accommodated by this count, then the count is simply left
      --  at the maximum value. This means that there are some pathological
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
      -- 4.5  Short-Circuit Control Forms --
      --------------------------------------

      --  EXPRESSION ::=
      --    RELATION {and then RELATION} | RELATION {or else RELATION}

      --  Gigi restriction: For both these control forms, the operand and
      --  result types are always Standard.Boolean. The expander inserts the
      --  required conversion operations where needed to ensure this is the
      --  case.

      --  N_And_Then
      --  Sloc points to AND of AND THEN
      --  Left_Opnd
      --  Right_Opnd
      --  Actions
      --  plus fields for expression

      --  N_Or_Else
      --  Sloc points to OR of OR ELSE
      --  Left_Opnd
      --  Right_Opnd
      --  Actions
      --  plus fields for expression

      --  Note: The Actions field is used to hold actions associated with
      --  the right hand operand. These have to be treated specially since
      --  they are not unconditionally executed. See Insert_Actions for a
      --  more detailed description of how these actions are handled.

      ---------------------------
      -- 4.5  Membership Tests --
      ---------------------------

      --  RELATION ::=
      --    SIMPLE_EXPRESSION [not] in MEMBERSHIP_CHOICE_LIST

      --  MEMBERSHIP_CHOICE_LIST ::=
      --    MEMBERSHIP_CHOICE {'|' MEMBERSHIP CHOICE}

      --  MEMBERSHIP_CHOICE ::=
      --    CHOICE_EXPRESSION | RANGE | SUBTYPE_MARK

      --  Note: although the grammar above allows only a range or a subtype
      --  mark, the parser in fact will accept any simple expression in place
      --  of a subtype mark. This means that the semantic analyzer must be able
      --  to deal with, and diagnose a simple expression other than a name for
      --  the right operand. This simplifies error recovery in the parser.

      --  The Alternatives field below is present only if there is more than
      --  one Membership_Choice present (which is legitimate only in Ada 2012
      --  mode) in which case Right_Opnd is Empty, and Alternatives contains
      --  the list of choices. In the tree passed to the back end, Alternatives
      --  is always No_List, and Right_Opnd is set (i.e. the expansion circuit
      --  expands out the complex set membership case using simple membership
      --  and equality operations).

      --  Should we rename Alternatives here to Membership_Choices ???

      --  N_In
      --  Sloc points to IN
      --  Left_Opnd
      --  Right_Opnd
      --  Alternatives (set to No_List if only one set alternative)
      --  No_Minimize_Eliminate
      --  plus fields for expression

      --  N_Not_In
      --  Sloc points to NOT of NOT IN
      --  Left_Opnd
      --  Right_Opnd
      --  Alternatives (set to No_List if only one set alternative)
      --  No_Minimize_Eliminate
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

      --  Gigi restriction: Gigi will never be given * / mod rem nodes with
      --  fixed-point operands. All handling of smalls for multiplication and
      --  division is handled by the front end (mod and rem result only from
      --  expansion). Gigi thus never needs to worry about small values (for
      --  other operators operating on fixed-point, e.g. addition, the small
      --  value does not have any semantic effect anyway, these are always
      --  integer operations).

      --  Gigi restriction: For all operators taking Boolean operands, the
      --  type is always Standard.Boolean. The expander inserts the required
      --  conversion operations where needed to ensure this is the case.

      --  N_Op_And
      --  Sloc points to AND
      --  Do_Length_Check
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Or
      --  Sloc points to OR
      --  Do_Length_Check
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Xor
      --  Sloc points to XOR
      --  Do_Length_Check
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Eq
      --  Sloc points to =
      --  Compare_Type
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Ne
      --  Sloc points to /=
      --  Compare_Type
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Lt
      --  Sloc points to <
      --  Compare_Type
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Le
      --  Sloc points to <=
      --  Compare_Type
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Gt
      --  Sloc points to >
      --  Compare_Type
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Ge
      --  Sloc points to >=
      --  Compare_Type
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
      --  Is_Component_Left_Opnd
      --  Is_Component_Right_Opnd
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Multiply
      --  Sloc points to *
      --  Rounded_Result
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Divide
      --  Sloc points to /
      --  Do_Division_Check
      --  Rounded_Result
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Mod
      --  Sloc points to MOD
      --  Do_Division_Check
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Rem
      --  Sloc points to REM
      --  Do_Division_Check
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Expon
      --  Sloc points to **
      --  Is_Power_Of_2_For_Shift
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
      --  case of multiply/divide/rem/mod operations, Gigi will never see them.

      --  Note on equality/inequality tests for records. In the expanded tree,
      --  record comparisons are always expanded to be a series of component
      --  comparisons, so the back end will never see an equality or inequality
      --  operation with operands of a record type.

      --  Note on overflow handling: When the overflow checking mode is set to
      --  MINIMIZED or ELIMINATED, nodes for signed arithmetic operations may
      --  be modified to use a larger type for the operands and result. In
      --  the case where the computed range exceeds that of Long_Long_Integer,
      --  and we are running in ELIMINATED mode, the operator node will be
      --  changed to be a call to the appropriate routine in System.Bignums.

      ------------------------------------
      -- 4.5.7  Conditional Expressions --
      ------------------------------------

      --  CONDITIONAL_EXPRESSION ::= IF_EXPRESSION | CASE_EXPRESSION

      --------------------------
      -- 4.5.7  If Expression --
      --------------------------

      --  IF_EXPRESSION ::=
      --    if CONDITION then DEPENDENT_EXPRESSION
      --                {elsif CONDITION then DEPENDENT_EXPRESSION}
      --                [else DEPENDENT_EXPRESSION]

      --  DEPENDENT_EXPRESSION ::= EXPRESSION

      --  Note: if we have (IF x1 THEN x2 ELSIF x3 THEN x4 ELSE x5) then it
      --  is represented as (IF x1 THEN x2 ELSE (IF x3 THEN x4 ELSE x5)) and
      --  the Is_Elsif flag is set on the inner if expression.

      --  N_If_Expression
      --  Sloc points to IF or ELSIF keyword
      --  Expressions
      --  Then_Actions
      --  Else_Actions
      --  Is_Elsif (set if comes from ELSIF)
      --  Do_Overflow_Check
      --  Expansion_Delayed
      --  plus fields for expression

      --  Expressions here is a three-element list, whose first element is the
      --  condition, the second element is the dependent expression after THEN
      --  and the third element is the dependent expression after the ELSE
      --  (explicitly set to True if missing).

      --  Note: the Then_Actions and Else_Actions fields are always set to
      --  No_List in the tree passed to the back end. These are used only
      --  for temporary processing purposes in the expander. Even though they
      --  are semantic fields, their parent pointers are set because analysis
      --  of actions nodes in those lists may generate additional actions that
      --  need to know their insertion point (for example for the creation of
      --  transient scopes).

      --  Note: in the tree passed to the back end, if the result type is
      --  an unconstrained array, the if expression can only appears in the
      --  initializing expression of an object declaration (this avoids the
      --  back end having to create a variable length temporary on the fly).

      ----------------------------
      -- 4.5.7  Case Expression --
      ----------------------------

      --  CASE_EXPRESSION ::=
      --    case SELECTING_EXPRESSION is
      --      CASE_EXPRESSION_ALTERNATIVE
      --      {,CASE_EXPRESSION_ALTERNATIVE}

      --  Note that the Alternatives cannot include pragmas (this contrasts
      --  with the situation of case statements where pragmas are allowed).

      --  N_Case_Expression
      --  Sloc points to CASE
      --  Expression (the selecting expression)
      --  Alternatives (the case expression alternatives)
      --  Etype
      --  Do_Overflow_Check
      --  Expansion_Delayed

      ----------------------------------------
      -- 4.5.7  Case Expression Alternative --
      ----------------------------------------

      --  CASE_EXPRESSION_ALTERNATIVE ::=
      --    when DISCRETE_CHOICE_LIST =>
      --      DEPENDENT_EXPRESSION

      --  N_Case_Expression_Alternative
      --  Sloc points to WHEN
      --  Actions
      --  Discrete_Choices
      --  Expression
      --  Has_SP_Choice

      --  Note: The Actions field temporarily holds any actions associated with
      --  evaluation of the Expression. During expansion of the case expression
      --  these actions are wrapped into an N_Expression_With_Actions node
      --  replacing the original expression.

      --  Note: this node never appears in the tree passed to the back end,
      --  since the expander converts case expressions into case statements.

      ---------------------------------
      -- 4.5.8 Quantified Expression --
      ---------------------------------

      --  QUANTIFIED_EXPRESSION ::=
      --    for QUANTIFIER LOOP_PARAMETER_SPECIFICATION => PREDICATE
      --  | for QUANTIFIER ITERATOR_SPECIFICATION => PREDICATE
      --
      --  QUANTIFIER ::= all | some

      --  At most one of (Iterator_Specification, Loop_Parameter_Specification)
      --  is present at a time, in which case the other one is empty.

      --  N_Quantified_Expression
      --  Sloc points to FOR
      --  Iterator_Specification
      --  Loop_Parameter_Specification
      --  Condition
      --  All_Present

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
      --  involving fixed-point types are handled by the expander, unless the
      --  Conversion_OK flag is set.

      --  Sprint syntax if Float_Truncate set: X^(Y)
      --  Sprint syntax if Conversion_OK set X?(Y)
      --  Sprint syntax if both flags set X?^(Y)

      --  Note: If either the operand or result type is fixed-point, Gigi will
      --  only see a type conversion node with Conversion_OK set. The front end
      --  takes care of all handling of small's for fixed-point conversions.

      --  N_Type_Conversion
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark
      --  Expression
      --  Do_Discriminant_Check
      --  Do_Length_Check
      --  Float_Truncate
      --  Conversion_OK
      --  Do_Overflow_Check
      --  Rounded_Result
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
      --  Subtype_Mark
      --  Expression expression or aggregate
      --  Is_Qualified_Universal_Literal
      --  plus fields for expression

      --------------------
      -- 4.8  Allocator --
      --------------------

      --  ALLOCATOR ::=
      --      new [SUBPOOL_SPECIFICATION] SUBTYPE_INDICATION
      --    | new [SUBPOOL_SPECIFICATION] QUALIFIED_EXPRESSION
      --
      --  SUBPOOL_SPECIFICATION ::= (subpool_handle_NAME)

      --  Sprint syntax (when storage pool present)
      --    new xxx (storage_pool = pool)
      --  or
      --    new (subpool) xxx (storage_pool = pool)

      --  N_Allocator
      --  Sloc points to NEW
      --  Expression subtype indication or qualified expression
      --  Subpool_Handle_Name (set to Empty if not present)
      --  Storage_Pool
      --  Procedure_To_Call
      --  For_Special_Return_Object
      --  Null_Exclusion_Present
      --  No_Initialization
      --  Is_Static_Coextension
      --  Do_Storage_Check
      --  Is_Dynamic_Coextension
      --  plus fields for expression

      --  Note: like all nodes, the N_Allocator has the Comes_From_Source flag.
      --  This flag has a special function in conjunction with the restriction
      --  No_Implicit_Heap_Allocations, which will be triggered if this flag
      --  is not set. This means that if a source allocator is replaced with
      --  a constructed allocator, the Comes_From_Source flag should be copied
      --  to the newly created allocator.

      ---------------------------------
      -- 5.1  Sequence Of Statements --
      ---------------------------------

      --  SEQUENCE_OF_STATEMENTS ::= STATEMENT {STATEMENT}

      --  Note: Although the parser will not accept a declaration as a
      --  statement, the semantic analyzer may insert declarations (e.g.
      --  declarations of implicit types needed for execution of other
      --  statements) into a sequence of statements, so the code generator
      --  should be prepared to accept a declaration where a statement is
      --  expected. Note also that pragmas can appear as statements.

      --------------------
      -- 5.1  Statement --
      --------------------

      --  STATEMENT ::=
      --    {LABEL} SIMPLE_STATEMENT | {LABEL} COMPOUND_STATEMENT

      --  There is no explicit node in the tree for a statement. Instead, the
      --  individual statement appears directly. Labels are treated as a
      --  kind of statement, i.e. they are linked into a statement list at
      --  the point they appear, so the labeled statement appears following
      --  the label or labels in the statement list.

      ---------------------------
      -- 5.1  Simple Statement --
      ---------------------------

      --  SIMPLE_STATEMENT ::=        NULL_STATEMENT
      --  | ASSIGNMENT_STATEMENT    | EXIT_STATEMENT
      --  | GOTO_STATEMENT          | PROCEDURE_CALL_STATEMENT
      --  | SIMPLE_RETURN_STATEMENT | ENTRY_CALL_STATEMENT
      --  | REQUEUE_STATEMENT       | DELAY_STATEMENT
      --  | ABORT_STATEMENT         | RAISE_STATEMENT
      --  | CODE_STATEMENT

      -----------------------------
      -- 5.1  Compound Statement --
      -----------------------------

      --  COMPOUND_STATEMENT ::=
      --    IF_STATEMENT              | CASE_STATEMENT
      --  | LOOP_STATEMENT            | BLOCK_STATEMENT
      --  | EXTENDED_RETURN_STATEMENT
      --  | ACCEPT_STATEMENT          | SELECT_STATEMENT

      -------------------------
      -- 5.1  Null Statement --
      -------------------------

      --  NULL_STATEMENT ::= null;

      --  N_Null_Statement
      --  Sloc points to NULL
      --  Next_Rep_Item

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
      --  Identifier direct name of statement identifier
      --  Exception_Junk

      --  Note: Before Ada 2012, a label is always followed by a statement,
      --  and this is true in the tree even in Ada 2012 mode (the parser
      --  inserts a null statement marked with Comes_From_Source False).

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
      --  Name
      --  Expression
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Do_Discriminant_Check
      --  Do_Length_Check
      --  Forwards_OK
      --  Backwards_OK
      --  No_Ctrl_Actions
      --  No_Finalize_Actions
      --  Has_Target_Names
      --  Is_Elaboration_Code
      --  Componentwise_Assignment
      --  Suppress_Assignment_Checks

      --  Note: if a range check is required, then the Do_Range_Check flag
      --  is set in the Expression (right hand side), with the check being
      --  done against the type of the Name (left hand side).

      --  Note: the back end places some restrictions on the form of the
      --  Expression field. If the object being assigned to is Atomic, then
      --  the Expression may not have the form of an aggregate (since this
      --  might cause the back end to generate separate assignments). In this
      --  case the front end must generate an extra temporary and initialize
      --  this temporary as required (the temporary itself is not atomic).

      ------------------
      --  Target_Name --
      ------------------

      --  N_Target_Name
      --  Sloc points to @
      --  Etype

      --  Note (Ada 2022): node is used during analysis as a placeholder for
      --  the value of the LHS of the enclosing assignment statement. Node is
      --  eventually rewritten together with enclosing assignment, and backends
      --  are not aware of it.

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
      --  Condition
      --  Then_Statements
      --  Elsif_Parts (set to No_List if none present)
      --  Else_Statements (set to No_List if no else part present)
      --  End_Span (set to Uint_0 if expander generated)
      --  From_Conditional_Expression

      --  N_Elsif_Part
      --  Sloc points to ELSIF
      --  Condition
      --  Then_Statements
      --  Condition_Actions

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
      --  Expression
      --  Alternatives
      --  End_Span (set to Uint_0 if expander generated)
      --  From_Conditional_Expression

      --  Note: Before Ada 2012, a pragma in a statement sequence is always
      --  followed by a statement, and this is true in the tree even in Ada
      --  2012 mode (the parser inserts a null statement marked with the flag
      --  Comes_From_Source False).

      -------------------------------------
      -- 5.4  Case Statement Alternative --
      -------------------------------------

      --  CASE_STATEMENT_ALTERNATIVE ::=
      --    when DISCRETE_CHOICE_LIST =>
      --      SEQUENCE_OF_STATEMENTS

      --  N_Case_Statement_Alternative
      --  Sloc points to WHEN
      --  Discrete_Choices
      --  Statements
      --  Has_SP_Choice
      --  Multidefined_Bindings

      --  Note: in the list of Discrete_Choices, the tree passed to the back
      --  end does not have choice entries corresponding to names of statically
      --  predicated subtypes. Such entries are always expanded out to the list
      --  of equivalent values or ranges. Multidefined_Bindings is True iff
      --  more than one choice is present and each choice contains
      --  at least one component association having a non-null Binding_Chars
      --  attribute; this can only occur if GNAT extensions are enabled
      --  and the type of the case selector is composite.

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

      --  Note: there is always a loop statement identifier present in the
      --  tree, even if none was given in the source. In the case where no loop
      --  identifier is given in the source, the parser creates a name of the
      --  form _Loop_n, where n is a decimal integer (the two underlines ensure
      --  that the loop names created in this manner do not conflict with any
      --  user defined identifiers), and the flag Has_Created_Identifier is set
      --  to True. The only exception to the rule that all loop statement nodes
      --  have identifiers occurs for loops constructed by the expander, and
      --  the semantic analyzer will create and supply dummy loop identifiers
      --  in these cases.

      --  N_Loop_Statement
      --  Sloc points to LOOP
      --  Identifier loop identifier (set to Empty if no identifier)
      --  Iteration_Scheme (set to Empty if no iteration scheme)
      --  Statements
      --  End_Label
      --  Has_Created_Identifier
      --  Is_Null_Loop
      --  Suppress_Loop_Warnings

      --  Note: the parser fills in the Identifier field if there is an
      --  explicit loop identifier. Otherwise the parser leaves this field
      --  set to Empty, and then the semantic processing for a loop statement
      --  creates an identifier, setting the Has_Created_Identifier flag to
      --  True. So after semantic analysis, the Identifier is always set,
      --  referencing an identifier whose entity has an Ekind of E_Loop.

      ---------------------------
      -- 5.5  Iteration Scheme --
      ---------------------------

      --  ITERATION_SCHEME ::=
      --    while CONDITION
      --  | for LOOP_PARAMETER_SPECIFICATION
      --  | for ITERATOR_SPECIFICATION

      --  At most one of (Iterator_Specification, Loop_Parameter_Specification)
      --  is present at a time, in which case the other one is empty. Both are
      --  empty in the case of a WHILE loop.

      --  Gigi restriction: The expander ensures that the type of the Condition
      --  field is always Standard.Boolean, even if the type in the source is
      --  some non-standard boolean type.

      --  N_Iteration_Scheme
      --  Sloc points to WHILE or FOR
      --  Condition (set to Empty if FOR case)
      --  Condition_Actions
      --  Iterator_Specification (set to Empty if WHILE case)
      --  Loop_Parameter_Specification (set to Empty if WHILE case)

      ---------------------------------------
      -- 5.5  Loop Parameter Specification --
      ---------------------------------------

      --  LOOP_PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER in [reverse] DISCRETE_SUBTYPE_DEFINITION
      --    [Iterator_Filter]

      --  Note: the optional Iterator_Filter is an Ada 2022 construct.

      --  N_Loop_Parameter_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier
      --  Reverse_Present
      --  Iterator_Filter (set to Empty if not present)
      --  Discrete_Subtype_Definition

      -----------------------------------
      -- 5.5.1  Iterator Specification --
      -----------------------------------

      --  ITERATOR_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER in [reverse] NAME
      --  | DEFINING_IDENTIFIER [: SUBTYPE_INDICATION] of [reverse] NAME

      --  N_Iterator_Specification
      --  Sloc points to defining identifier
      --  Defining_Identifier
      --  Name
      --  Reverse_Present
      --  Of_Present
      --  Iterator_Filter (set to Empty if not present)
      --  Subtype_Indication

      --  Note: The Of_Present flag distinguishes the two forms

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
      --  occurrence is an E_Block entity declared by the implicit label
      --  declaration which occurs in the innermost enclosing block statement
      --  or body; the block identifier denotes that E_Block.

      --  For block statements that come from source code, there is always a
      --  block statement identifier present in the tree, denoting an E_Block.
      --  In the case where no block identifier is given in the source,
      --  the parser creates a name of the form B_n, where n is a decimal
      --  integer, and the flag Has_Created_Identifier is set to True. Blocks
      --  constructed by the expander usually have no identifier, and no
      --  corresponding entity.

      --  Note: the block statement created for an extended return statement
      --  has an entity, and this entity is an E_Return_Statement, rather than
      --  the usual E_Block.

      --  Note: Exception_Junk is set for the wrapping blocks created during
      --  local raise optimization (Exp_Ch11.Expand_Local_Exception_Handlers).

      --  Note: from a control flow viewpoint, a block statement defines an
      --  extended basic block, i.e. the entry of the block dominates every
      --  statement in the sequence. When generating new statements with
      --  exception handlers in the expander at the end of a sequence that
      --  comes from source code, it can be necessary to wrap them all in a
      --  block statement in order to expose the implicit control flow to
      --  gigi and thus prevent it from issuing bogus control flow warnings.

      --  N_Block_Statement
      --  Sloc points to DECLARE or BEGIN
      --  Identifier block direct name (set to Empty if not present)
      --  Declarations (set to No_List if no DECLARE part)
      --  Handled_Statement_Sequence
      --  Activation_Chain_Entity
      --  Cleanup_Actions
      --  At_End_Proc (set to Empty if no clean up procedure)
      --  Exception_Junk
      --  Has_Created_Identifier
      --  Is_Abort_Block
      --  Is_Asynchronous_Call_Block
      --  Is_Initialization_Block
      --  Is_Task_Allocation_Block
      --  Is_Task_Master

      -------------------------
      -- 5.7  Exit Statement --
      -------------------------

      --  EXIT_STATEMENT ::= exit [loop_NAME] [when CONDITION];

      --  Gigi restriction: The expander ensures that the type of the Condition
      --  field is always Standard.Boolean, even if the type in the source is
      --  some non-standard boolean type.

      --  N_Exit_Statement
      --  Sloc points to EXIT
      --  Name (set to Empty if no loop name present)
      --  Condition (set to Empty if no WHEN part present)
      --  Next_Exit_Statement : Next exit on chain

      -------------------------
      -- 5.9  Goto Statement --
      -------------------------

      --  GOTO_STATEMENT ::= goto label_NAME;

      --  N_Goto_Statement
      --  Sloc points to GOTO
      --  Name
      --  Exception_Junk

      ---------------------------------
      -- 6.1  Subprogram Declaration --
      ---------------------------------

      --  SUBPROGRAM_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION
      --      [ASPECT_SPECIFICATIONS];

      --  N_Subprogram_Declaration
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification
      --  Body_To_Inline
      --  Corresponding_Body
      --  Parent_Spec
      --  Is_Entry_Barrier_Function
      --  Is_Task_Body_Procedure

      ------------------------------------------
      -- 6.1  Abstract Subprogram Declaration --
      ------------------------------------------

      --  ABSTRACT_SUBPROGRAM_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION is abstract
      --      [ASPECT_SPECIFICATIONS];

      --  N_Abstract_Subprogram_Declaration
      --  Sloc points to ABSTRACT
      --  Specification

      -----------------------------------
      -- 6.1  Subprogram Specification --
      -----------------------------------

      --  SUBPROGRAM_SPECIFICATION ::=
      --    [[not] overriding]
      --    procedure DEFINING_PROGRAM_UNIT_NAME PARAMETER_PROFILE
      --  | [[not] overriding]
      --    function DEFINING_DESIGNATOR PARAMETER_AND_RESULT_PROFILE

      --  Note: there are no separate nodes for the profiles, instead the
      --  information appears directly in the following nodes.

      --  N_Function_Specification
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name (the designator)
      --  Parameter_Specifications (set to No_List if no formal part)
      --  Null_Exclusion_Present
      --  Result_Definition for result subtype
      --  Generic_Parent
      --  Must_Override set if overriding indicator present
      --  Must_Not_Override set if not_overriding indicator present

      --  N_Procedure_Specification
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name
      --  Null_Statement NULL statement for body, if Null_Present
      --  Parameter_Specifications (set to No_List if no formal part)
      --  Generic_Parent
      --  Null_Present set for null procedure case (Ada 2005 feature)
      --  Must_Override set if overriding indicator present
      --  Must_Not_Override set if not_overriding indicator present

      --  Note: overriding indicator is an Ada 2005 feature

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
      --  Name holds the parent unit name
      --  Identifier

      --  Note: Name is always non-Empty, since this node is only used for the
      --  case where a parent library unit package name is present.

      --  Note that the identifier can also be an operator symbol here

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

      --  The parent unit name is present only in the case of a child unit name
      --  (permissible only for Ada 95 for a library level unit, i.e. a unit
      --  at scope level one). If no such name is present, the defining program
      --  unit name is represented simply as the defining identifier. In the
      --  child unit case, the following node is used to represent the child
      --  unit name.

      --  N_Defining_Program_Unit_Name
      --  Sloc points to period
      --  Name holds the parent unit name
      --  Defining_Identifier

      --  Note: Name is always non-Empty, since this node is only used for the
      --  case where a parent unit name is present.

      --------------------------
      -- 6.1  Operator Symbol --
      --------------------------

      --  OPERATOR_SYMBOL ::= STRING_LITERAL

      --  Note: the fields of the N_Operator_Symbol node are laid out to match
      --  the corresponding fields of an N_Character_Literal node. This allows
      --  easy conversion of the operator symbol node into a character literal
      --  node in the case where a string constant of the form of an operator
      --  symbol is scanned out as such, but turns out semantically to be a
      --  string literal that is not an operator. For details see Sinfo.CN.
      --  Change_Operator_Symbol_To_String_Literal.

      --  N_Operator_Symbol
      --  Sloc points to literal
      --  Chars contains the Name_Id for the operator symbol
      --  Strval Id of string value. This is used if the operator
      --   symbol turns out to be a normal string after all.
      --  Entity
      --  Associated_Node Note this is shared with Entity
      --  Etype
      --  Has_Private_View (set in generic units)
      --  Has_Secondary_Private_View (set in generic units)

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

      --  N_Defining_Operator_Symbol
      --  Sloc points to literal
      --  Chars contains the Name_Id for the operator symbol
      --  Next_Entity
      --  Scope
      --  Etype

      ----------------------------
      -- 6.1  Parameter Profile --
      ----------------------------

      --  PARAMETER_PROFILE ::= [FORMAL_PART]

      ---------------------------------------
      -- 6.1  Parameter and Result Profile --
      ---------------------------------------

      --  PARAMETER_AND_RESULT_PROFILE ::=
      --    [FORMAL_PART] return [NULL_EXCLUSION] SUBTYPE_MARK
      --  | [FORMAL_PART] return ACCESS_DEFINITION

      --  There is no explicit node in the tree for a parameter and result
      --  profile. Instead the information appears directly in the parent.

      ----------------------
      -- 6.1  Formal Part --
      ----------------------

      --  FORMAL_PART ::=
      --    (PARAMETER_SPECIFICATION {; PARAMETER_SPECIFICATION})

      ----------------------------------
      -- 6.1  Parameter Specification --
      ----------------------------------

      --  PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : [ALIASED] MODE [NULL_EXCLUSION]
      --      SUBTYPE_MARK [:= DEFAULT_EXPRESSION] [ASPECT_SPECIFICATIONS]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION] [ASPECT_SPECIFICATIONS]

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive specifications were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single Specifications, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  ALIASED can only be present in Ada 2012 mode

      --  N_Parameter_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier
      --  Aliased_Present
      --  In_Present
      --  Out_Present
      --  Null_Exclusion_Present
      --  Parameter_Type subtype mark or access definition
      --  Expression (set to Empty if no default expression present)
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)
      --  Default_Expression

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
      --    SUBPROGRAM_SPECIFICATION [ASPECT_SPECIFICATIONS] is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [DESIGNATOR];

      --  N_Subprogram_Body
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification
      --  Declarations
      --  Handled_Statement_Sequence
      --  Activation_Chain_Entity
      --  Corresponding_Spec
      --  At_End_Proc (set to Empty if no clean up procedure)
      --  Acts_As_Spec
      --  Bad_Is_Detected used only by parser
      --  Do_Storage_Check
      --  Has_Relative_Deadline_Pragma
      --  Is_Entry_Barrier_Function
      --  Is_Protected_Subprogram_Body
      --  Is_Task_Body_Procedure
      --  Is_Task_Master
      --  Was_Attribute_Reference
      --  Was_Expression_Function
      --  Was_Originally_Stub

      -----------------------------------
      -- 6.4  Procedure Call Statement --
      -----------------------------------

      --  PROCEDURE_CALL_STATEMENT ::=
      --    procedure_NAME; | procedure_PREFIX ACTUAL_PARAMETER_PART;

      --  Note: the reason that a procedure call has expression fields is that
      --  it semantically resembles an expression, e.g. overloading is allowed
      --  and a type is concocted for semantic processing purposes. Certain of
      --  these fields, such as Parens are not relevant, but it is easier to
      --  just supply all of them together.

      --  N_Procedure_Call_Statement
      --  Sloc points to first token of name or prefix
      --  Name stores name or prefix
      --  Parameter_Associations (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual
      --  Controlling_Argument (set to Empty if not dispatching)
      --  Is_Elaboration_Checks_OK_Node
      --  Is_Expanded_Prefixed_Call
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  No_Elaboration_Check
      --  Is_Known_Guaranteed_ABE
      --  plus fields for expression

      --  If any IN parameter requires a range check, then the corresponding
      --  argument expression has the Do_Range_Check flag set, and the range
      --  check is done against the formal type. Note that this argument
      --  expression may appear directly in the Parameter_Associations list,
      --  or may be a descendant of an N_Parameter_Association node that
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
      --  Name stores name or prefix
      --  Parameter_Associations (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual
      --  Controlling_Argument (set to Empty if not dispatching)
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  No_Elaboration_Check
      --  Is_Expanded_Build_In_Place_Call
      --  Is_Expanded_Prefixed_Call
      --  Is_Known_Guaranteed_ABE
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
      --  Selector_Name (always non-Empty)
      --  Explicit_Actual_Parameter
      --  Next_Named_Actual
      --  Is_Accessibility_Actual

      ---------------------------
      -- 6.4  Actual Parameter --
      ---------------------------

      --  EXPLICIT_ACTUAL_PARAMETER ::=
      --    EXPRESSION | variable_NAME | REDUCTION_EXPRESSION_PARAMETER

      ---------------------------
      -- 6.5  Return Statement --
      ---------------------------

      --  SIMPLE_RETURN_STATEMENT ::= return [EXPRESSION];

      --  EXTENDED_RETURN_STATEMENT ::=
      --    return DEFINING_IDENTIFIER : [aliased] RETURN_SUBTYPE_INDICATION
      --                                           [:= EXPRESSION] [do
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end return];

      --  RETURN_SUBTYPE_INDICATION ::= SUBTYPE_INDICATION | ACCESS_DEFINITION

      --  The term "return statement" is defined in 6.5 to mean either a
      --  SIMPLE_RETURN_STATEMENT or an EXTENDED_RETURN_STATEMENT. We avoid
      --  the use of this term, since it used to mean something else in earlier
      --  versions of Ada.

      --  N_Simple_Return_Statement
      --  Sloc points to RETURN
      --  Return_Statement_Entity
      --  Expression (set to Empty if no expression present)
      --  Storage_Pool
      --  Procedure_To_Call
      --  Comes_From_Extended_Return_Statement

      --  Note: Return_Statement_Entity points to an E_Return_Statement

      --  If a range check is required, then Do_Range_Check is set on the
      --  Expression. The check is against the return subtype of the function.

      --  N_Extended_Return_Statement
      --  Sloc points to RETURN
      --  Return_Statement_Entity
      --  Return_Object_Declarations
      --  Handled_Statement_Sequence (set to Empty if not present)
      --  Storage_Pool
      --  Procedure_To_Call

      --  Note: Return_Statement_Entity points to an E_Return_Statement.

      --  Note that Return_Object_Declarations is a list containing the
      --  N_Object_Declaration -- see comment on this field above.

      --  The declared object will have Is_Return_Object = True.

      --  There is no such syntactic category as return_object_declaration
      --  in the RM. Return_Object_Declarations represents this portion of
      --  the syntax for EXTENDED_RETURN_STATEMENT:
      --      DEFINING_IDENTIFIER : [aliased] RETURN_SUBTYPE_INDICATION
      --                                      [:= EXPRESSION]

      --  There are two entities associated with an extended_return_statement:
      --  the Return_Statement_Entity represents the statement itself,
      --  and the Defining_Identifier of the Object_Declaration in
      --  Return_Object_Declarations represents the object being
      --  returned. N_Simple_Return_Statement has only the former.

      ------------------------------
      -- 6.8  Expression Function --
      ------------------------------

      --  EXPRESSION_FUNCTION ::=
      --    FUNCTION SPECIFICATION IS (EXPRESSION)
      --      [ASPECT_SPECIFICATIONS];

      --  N_Expression_Function
      --  Sloc points to FUNCTION
      --  Specification
      --  Expression
      --  Corresponding_Spec

      ------------------------------
      -- 7.1  Package Declaration --
      ------------------------------

      --  PACKAGE_DECLARATION ::=
      --    PACKAGE_SPECIFICATION;

      --  Note: the activation chain entity for a package spec is used for
      --  all tasks declared in the package spec, or in the package body.

      --  N_Package_Declaration
      --  Sloc points to PACKAGE
      --  Specification
      --  Corresponding_Body
      --  Parent_Spec
      --  Activation_Chain_Entity

      --------------------------------
      -- 7.1  Package Specification --
      --------------------------------

      --  PACKAGE_SPECIFICATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME
      --      [ASPECT_SPECIFICATIONS]
      --    is
      --      {BASIC_DECLARATIVE_ITEM}
      --    [private
      --      {BASIC_DECLARATIVE_ITEM}]
      --    end [[PARENT_UNIT_NAME .] IDENTIFIER]

      --  N_Package_Specification
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name
      --  Visible_Declarations
      --  Private_Declarations (set to No_List if no private
      --   part present)
      --  End_Label
      --  Generic_Parent
      --  Limited_View_Installed

      -----------------------
      -- 7.1  Package Body --
      -----------------------

      --  PACKAGE_BODY ::=
      --    package body DEFINING_PROGRAM_UNIT_NAME
      --      [ASPECT_SPECIFICATIONS]
      --    is
      --      DECLARATIVE_PART
      --    [begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS]
      --    end [[PARENT_UNIT_NAME .] IDENTIFIER];

      --  N_Package_Body
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name
      --  Declarations
      --  Handled_Statement_Sequence (set to Empty if no HSS present)
      --  Corresponding_Spec
      --  At_End_Proc (set to Empty if no clean up procedure)
      --  Was_Originally_Stub

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
      --      is [[abstract] tagged] [limited] private
      --        [ASPECT_SPECIFICATIONS];

      --  Note: TAGGED is not permitted in Ada 83 mode

      --  N_Private_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier
      --  Discriminant_Specifications (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present set if (<>) discriminant
      --  Abstract_Present
      --  Tagged_Present
      --  Limited_Present

      ----------------------------------------
      -- 7.4  Private Extension Declaration --
      ----------------------------------------

      --  PRIVATE_EXTENSION_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART] is
      --      [abstract] [limited | synchronized]
      --        new ancestor_SUBTYPE_INDICATION [and INTERFACE_LIST]
      --           with private [ASPECT_SPECIFICATIONS];

      --  Note: LIMITED, and private extension declarations are not allowed
      --        in Ada 83 mode.

      --  N_Private_Extension_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier
      --  Uninitialized_Variable
      --  Discriminant_Specifications (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present set if (<>) discriminant
      --  Abstract_Present
      --  Limited_Present
      --  Synchronized_Present
      --  Subtype_Indication
      --  Interface_List (set to No_List if none)

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
      --  Prev_Use_Clause
      --  Name
      --  Next_Use_Clause
      --  Associated_Node
      --  Hidden_By_Use_Clause
      --  Is_Effective_Use_Clause
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)

      --------------------------
      -- 8.4  Use Type Clause --
      --------------------------

      --  USE_TYPE_CLAUSE ::= use [ALL] type SUBTYPE_MARK {, SUBTYPE_MARK};

      --  Note: use type clause is not permitted in Ada 83 mode

      --  Note: the ALL keyword can appear only in Ada 2012 mode

      --  N_Use_Type_Clause
      --  Sloc points to USE
      --  Prev_Use_Clause
      --  Used_Operations
      --  Next_Use_Clause
      --  Subtype_Mark
      --  Hidden_By_Use_Clause
      --  Is_Effective_Use_Clause
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)
      --  All_Present

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
      --    DEFINING_IDENTIFIER :
      --      [NULL_EXCLUSION] SUBTYPE_MARK renames object_NAME
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER :
      --      ACCESS_DEFINITION renames object_NAME
      --        [ASPECT_SPECIFICATIONS];

      --  Note: Access_Definition is an optional field that gives support to
      --  Ada 2005 (AI-230). The parser generates nodes that have either the
      --  Subtype_Indication field or else the Access_Definition field.

      --  N_Object_Renaming_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier
      --  Null_Exclusion_Present (set to False if not present)
      --  Subtype_Mark (set to Empty if not present)
      --  Access_Definition (set to Empty if not present)
      --  Name
      --  Corresponding_Generic_Association

      -----------------------------------------
      -- 8.5  Exception Renaming Declaration --
      -----------------------------------------

      --  EXCEPTION_RENAMING_DECLARATION ::=
      --    DEFINING_IDENTIFIER : exception renames exception_NAME
      --      [ASPECT_SPECIFICATIONS];

      --  N_Exception_Renaming_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier
      --  Name

      ---------------------------------------
      -- 8.5  Package Renaming Declaration --
      ---------------------------------------

      --  PACKAGE_RENAMING_DECLARATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME renames package_NAME
      --      [ASPECT_SPECIFICATIONS];

      --  N_Package_Renaming_Declaration
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name
      --  Name
      --  Parent_Spec

      ------------------------------------------
      -- 8.5  Subprogram Renaming Declaration --
      ------------------------------------------

      --  SUBPROGRAM_RENAMING_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION renames callable_entity_NAME
      --      [ASPECT_SPECIFICATIONS];

      --  N_Subprogram_Renaming_Declaration
      --  Sloc points to RENAMES
      --  Specification
      --  Name
      --  Parent_Spec
      --  Corresponding_Spec
      --  Corresponding_Formal_Spec
      --  From_Default

      -----------------------------------------
      -- 8.5.5  Generic Renaming Declaration --
      -----------------------------------------

      --  GENERIC_RENAMING_DECLARATION ::=
      --    generic package DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_package_NAME
      --        [ASPECT_SPECIFICATIONS];
      --  | generic procedure DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_procedure_NAME
      --        [ASPECT_SPECIFICATIONS];
      --  | generic function DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_function_NAME
      --        [ASPECT_SPECIFICATIONS];

      --  N_Generic_Package_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name
      --  Name
      --  Parent_Spec

      --  N_Generic_Procedure_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name
      --  Name
      --  Parent_Spec

      --  N_Generic_Function_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name
      --  Name
      --  Parent_Spec

      --------------------------------
      -- 9.1  Task Type Declaration --
      --------------------------------

      --  TASK_TYPE_DECLARATION ::=
      --    task type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      [ASPECT_SPECIFICATIONS]
      --    [is [new INTERFACE_LIST with] TASK_DEFINITION];

      --  N_Task_Type_Declaration
      --  Sloc points to TASK
      --  Defining_Identifier
      --  Discriminant_Specifications (set to No_List if no
      --   discriminant part)
      --  Interface_List (set to No_List if none)
      --  Task_Definition (set to Empty if not present)
      --  Corresponding_Body

      ----------------------------------
      -- 9.1  Single Task Declaration --
      ----------------------------------

      --  SINGLE_TASK_DECLARATION ::=
      --    task DEFINING_IDENTIFIER
      --      [ASPECT_SPECIFICATIONS]
      --    [is [new INTERFACE_LIST with] TASK_DEFINITION];

      --  N_Single_Task_Declaration
      --  Sloc points to TASK
      --  Defining_Identifier
      --  Interface_List (set to No_List if none)
      --  Task_Definition (set to Empty if not present)

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
      --  Visible_Declarations
      --  Private_Declarations (set to No_List if no private part)
      --  End_Label
      --  Has_Storage_Size_Pragma
      --  Has_Relative_Deadline_Pragma

      --------------------
      -- 9.1  Task Item --
      --------------------

      --  TASK_ITEM ::= ENTRY_DECLARATION | REPRESENTATION_CLAUSE

      --------------------
      -- 9.1  Task Body --
      --------------------

      --  TASK_BODY ::=
      --    task body task_DEFINING_IDENTIFIER
      --      [ASPECT_SPECIFICATIONS]
      --    is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [task_IDENTIFIER];

      --  Gigi restriction: This node never appears

      --  N_Task_Body
      --  Sloc points to TASK
      --  Defining_Identifier
      --  Declarations
      --  Handled_Statement_Sequence
      --  Is_Task_Master
      --  Activation_Chain_Entity
      --  Corresponding_Spec
      --  Was_Originally_Stub

      -------------------------------------
      -- 9.4  Protected Type Declaration --
      -------------------------------------

      --  PROTECTED_TYPE_DECLARATION ::=
      --    protected type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      [ASPECT_SPECIFICATIONS]
      --    is [new INTERFACE_LIST with] PROTECTED_DEFINITION;

      --  Note: protected type declarations are not permitted in Ada 83 mode

      --  N_Protected_Type_Declaration
      --  Sloc points to PROTECTED
      --  Defining_Identifier
      --  Discriminant_Specifications (set to No_List if no
      --   discriminant part)
      --  Interface_List (set to No_List if none)
      --  Protected_Definition
      --  Corresponding_Body

      ---------------------------------------
      -- 9.4  Single Protected Declaration --
      ---------------------------------------

      --  SINGLE_PROTECTED_DECLARATION ::=
      --    protected DEFINING_IDENTIFIER
      --      [ASPECT_SPECIFICATIONS]
      --    is [new INTERFACE_LIST with] PROTECTED_DEFINITION;

      --  Note: single protected declarations are not allowed in Ada 83 mode

      --  N_Single_Protected_Declaration
      --  Sloc points to PROTECTED
      --  Defining_Identifier
      --  Interface_List (set to No_List if none)
      --  Protected_Definition

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
      --  Visible_Declarations
      --  Private_Declarations (set to No_List if no private part)
      --  End_Label

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
      --    protected body DEFINING_IDENTIFIER
      --      [ASPECT_SPECIFICATIONS];
      --    is
      --      {PROTECTED_OPERATION_ITEM}
      --    end [protected_IDENTIFIER];

      --  Note: protected bodies are not allowed in Ada 83 mode

      --  Gigi restriction: This node never appears

      --  N_Protected_Body
      --  Sloc points to PROTECTED
      --  Defining_Identifier
      --  Declarations protected operation items (and pragmas)
      --  End_Label
      --  Corresponding_Spec
      --  Was_Originally_Stub

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
      --    [[not] overriding]
      --    entry DEFINING_IDENTIFIER
      --      [(DISCRETE_SUBTYPE_DEFINITION)] PARAMETER_PROFILE
      --        [ASPECT_SPECIFICATIONS];

      --  N_Entry_Declaration
      --  Sloc points to ENTRY
      --  Defining_Identifier
      --  Discrete_Subtype_Definition (set to Empty if not present)
      --  Parameter_Specifications (set to No_List if no formal part)
      --  Corresponding_Body
      --  Must_Override set if overriding indicator present
      --  Must_Not_Override set if not_overriding indicator present

      --  Note: overriding indicator is an Ada 2005 feature

      -----------------------------
      -- 9.5.2  Accept statement --
      -----------------------------

      --  ACCEPT_STATEMENT ::=
      --    accept entry_DIRECT_NAME
      --      [(ENTRY_INDEX)] PARAMETER_PROFILE [do
      --        HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [entry_IDENTIFIER]];

      --  Gigi restriction: This node never appears

      --  Note: there are no explicit declarations allowed in an accept
      --  statement. However, the implicit declarations for any statement
      --  identifiers (labels and block/loop identifiers) are declarations
      --  that belong logically to the accept statement, and that is why
      --  there is a Declarations field in this node.

      --  N_Accept_Statement
      --  Sloc points to ACCEPT
      --  Entry_Direct_Name
      --  Entry_Index (set to Empty if not present)
      --  Parameter_Specifications (set to No_List if no formal part)
      --  Handled_Statement_Sequence
      --  Declarations (set to No_List if no declarations)

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

      --  Gigi restriction: This node never appears

      --  N_Entry_Body
      --  Sloc points to ENTRY
      --  Defining_Identifier
      --  Entry_Body_Formal_Part
      --  Declarations
      --  Handled_Statement_Sequence
      --  Activation_Chain_Entity
      --  Corresponding_Spec
      --  At_End_Proc (set to Empty if no clean up procedure)

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

      --  Gigi restriction: This node never appears

      --  N_Entry_Body_Formal_Part
      --  Sloc points to first token
      --  Entry_Index_Specification (set to Empty if not present)
      --  Parameter_Specifications (set to No_List if no formal part)
      --  Condition from entry barrier of entry body

      --------------------------
      -- 9.5.2  Entry Barrier --
      --------------------------

      --  ENTRY_BARRIER ::= when CONDITION

      --------------------------------------
      -- 9.5.2  Entry Index Specification --
      --------------------------------------

      --  ENTRY_INDEX_SPECIFICATION ::=
      --    for DEFINING_IDENTIFIER in DISCRETE_SUBTYPE_DEFINITION

      --  Gigi restriction: This node never appears

      --  N_Entry_Index_Specification
      --  Sloc points to FOR
      --  Defining_Identifier
      --  Discrete_Subtype_Definition

      ---------------------------------
      -- 9.5.3  Entry Call Statement --
      ---------------------------------

      --  ENTRY_CALL_STATEMENT ::= entry_NAME [ACTUAL_PARAMETER_PART];

      --  The parser may generate a procedure call for this construct. The
      --  semantic pass must correct this misidentification where needed.

      --  Gigi restriction: This node never appears

      --  N_Entry_Call_Statement
      --  Sloc points to first token of name
      --  Name
      --  Parameter_Associations (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node

      ------------------------------
      -- 9.5.4  Requeue Statement --
      ------------------------------

      --  REQUEUE_STATEMENT ::= requeue entry_NAME [with abort];

      --  Note: requeue statements are not permitted in Ada 83 mode

      --  Gigi restriction: This node never appears

      --  N_Requeue_Statement
      --  Sloc points to REQUEUE
      --  Name
      --  Abort_Present
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node

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

      --  Gigi restriction: This node never appears

      --  N_Delay_Until_Statement
      --  Sloc points to DELAY
      --  Expression

      -----------------------------------
      -- 9.6  Delay Relative Statement --
      -----------------------------------

      --  DELAY_RELATIVE_STATEMENT ::= delay delay_EXPRESSION;

      --  Gigi restriction: This node never appears

      --  N_Delay_Relative_Statement
      --  Sloc points to DELAY
      --  Expression

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

      --  Gigi restriction: This node never appears

      --  Note: the guard expression, if present, appears in the node for
      --  the select alternative.

      --  N_Selective_Accept
      --  Sloc points to SELECT
      --  Select_Alternatives
      --  Else_Statements (set to No_List if no else part)

      ------------------
      -- 9.7.1  Guard --
      ------------------

      --  GUARD ::= when CONDITION =>

      --  As noted above, the CONDITION that is part of a GUARD is included
      --  in the node for the select alternative for convenience.

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

      --  Gigi restriction: This node never appears

      --  N_Accept_Alternative
      --  Sloc points to ACCEPT
      --  Accept_Statement
      --  Condition from the guard (set to Empty if no guard present)
      --  Statements (set to Empty_List if no statements)
      --  Pragmas_Before pragmas before alt (set to No_List if none)
      --  Accept_Handler_Records

      ------------------------------
      -- 9.7.1  Delay Alternative --
      ------------------------------

      --  DELAY_ALTERNATIVE ::=
      --    DELAY_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  Gigi restriction: This node never appears

      --  N_Delay_Alternative
      --  Sloc points to DELAY
      --  Delay_Statement
      --  Condition from the guard (set to Empty if no guard present)
      --  Statements (set to Empty_List if no statements)
      --  Pragmas_Before pragmas before alt (set to No_List if none)

      ----------------------------------
      -- 9.7.1  Terminate Alternative --
      ----------------------------------

      --  TERMINATE_ALTERNATIVE ::= terminate;

      --  Gigi restriction: This node never appears

      --  N_Terminate_Alternative
      --  Sloc points to TERMINATE
      --  Condition from the guard (set to Empty if no guard present)
      --  Pragmas_Before pragmas before alt (set to No_List if none)
      --  Pragmas_After pragmas after alt (set to No_List if none)

      -----------------------------
      -- 9.7.2  Timed Entry Call --
      -----------------------------

      --  TIMED_ENTRY_CALL ::=
      --    select
      --      ENTRY_CALL_ALTERNATIVE
      --    or
      --      DELAY_ALTERNATIVE
      --    end select;

      --  Gigi restriction: This node never appears

      --  N_Timed_Entry_Call
      --  Sloc points to SELECT
      --  Entry_Call_Alternative
      --  Delay_Alternative

      -----------------------------------
      -- 9.7.2  Entry Call Alternative --
      -----------------------------------

      --  ENTRY_CALL_ALTERNATIVE ::=
      --    PROCEDURE_OR_ENTRY_CALL [SEQUENCE_OF_STATEMENTS]

      --  PROCEDURE_OR_ENTRY_CALL ::=
      --    PROCEDURE_CALL_STATEMENT | ENTRY_CALL_STATEMENT

      --  Gigi restriction: This node never appears

      --  N_Entry_Call_Alternative
      --  Sloc points to first token of entry call statement
      --  Entry_Call_Statement
      --  Statements (set to Empty_List if no statements)
      --  Pragmas_Before pragmas before alt (set to No_List if none)

      -----------------------------------
      -- 9.7.3  Conditional Entry Call --
      -----------------------------------

      --  CONDITIONAL_ENTRY_CALL ::=
      --    select
      --      ENTRY_CALL_ALTERNATIVE
      --    else
      --      SEQUENCE_OF_STATEMENTS
      --    end select;

      --  Gigi restriction: This node never appears

      --  N_Conditional_Entry_Call
      --  Sloc points to SELECT
      --  Entry_Call_Alternative
      --  Else_Statements

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

      --  Gigi restriction: This node never appears

      --  N_Asynchronous_Select
      --  Sloc points to SELECT
      --  Triggering_Alternative
      --  Abortable_Part

      -----------------------------------
      -- 9.7.4  Triggering Alternative --
      -----------------------------------

      --  TRIGGERING_ALTERNATIVE ::=
      --    TRIGGERING_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  Gigi restriction: This node never appears

      --  N_Triggering_Alternative
      --  Sloc points to first token of triggering statement
      --  Triggering_Statement
      --  Statements (set to Empty_List if no statements)
      --  Pragmas_Before pragmas before alt (set to No_List if none)

      ---------------------------------
      -- 9.7.4  Triggering Statement --
      ---------------------------------

      --  TRIGGERING_STATEMENT ::= PROCEDURE_OR_ENTRY_CALL | DELAY_STATEMENT

      ---------------------------
      -- 9.7.4  Abortable Part --
      ---------------------------

      --  ABORTABLE_PART ::= SEQUENCE_OF_STATEMENTS

      --  Gigi restriction: This node never appears

      --  N_Abortable_Part
      --  Sloc points to ABORT
      --  Statements

      --------------------------
      -- 9.8  Abort Statement --
      --------------------------

      --  ABORT_STATEMENT ::= abort task_NAME {, task_NAME};

      --  Gigi restriction: This node never appears

      --  N_Abort_Statement
      --  Sloc points to ABORT
      --  Names

      -------------------------
      -- 10.1.1  Compilation --
      -------------------------

      --  COMPILATION ::= {COMPILATION_UNIT}

      --  There is no explicit node in the tree for a compilation, since in
      --  general the compiler is processing only a single compilation unit
      --  at a time. It is possible to parse multiple units in syntax check
      --  only mode, but the trees are discarded in that case.

      ------------------------------
      -- 10.1.1  Compilation Unit --
      ------------------------------

      --  COMPILATION_UNIT ::=
      --    CONTEXT_CLAUSE LIBRARY_ITEM
      --  | CONTEXT_CLAUSE SUBUNIT

      --  The N_Compilation_Unit node itself represents the above syntax.
      --  However, there are additional items not reflected in the above
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

      --  Pragmas that appear after the compilation unit are not reflected
      --  in the syntax. (Pragmas that appear before the library unit, are
      --  considered part of the context clause. Pragmas can also appear in
      --  the Context_Items list of the compilation unit.)

      --  ???For historical reasons, the above information is stored in a
      --  separate N_Compilation_Unit_Aux node associated with each
      --  N_Compilation_Unit node. This information could be moved into
      --  N_Compilation_Unit at this point.

      --  N_Compilation_Unit
      --  Sloc points to first token of defining unit name
      --  Context_Items context items and pragmas preceding unit
      --  Private_Present set if library unit has private keyword
      --  Unit library item or subunit
      --  Aux_Decls_Node points to the N_Compilation_Unit_Aux node
      --  First_Inlined_Subprogram
      --  Library_Unit corresponding/parent spec/body
      --  Save_Invocation_Graph_Of_Body
      --  Acts_As_Spec flag for subprogram body with no spec
      --  Body_Required set for spec if body is required
      --  Has_Pragma_Suppress_All
      --  Context_Pending
      --  Has_No_Elaboration_Code

      --  Note: The Unit field can be any of N_Lib_Unit_Declaration,
      --  N_Lib_Unit_Body, N_Lib_Unit_Renaming_Declaration, N_Subunit,
      --  or (in the case of ignored ghost code) N_Null_Statement.

      --  N_Compilation_Unit_Aux
      --  Sloc is a copy of the Sloc from the N_Compilation_Unit node
      --  Declarations (set to No_List if no global declarations)
      --  Actions (set to No_List if no actions)
      --  Pragmas_After pragmas after unit (set to No_List if none)
      --  Config_Pragmas config pragmas (set to Empty_List if none)
      --  Default_Storage_Pool

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
      --  appear in the N_Compilation_Unit node.

      --------------------------------------
      -- 10.1.1  Library Unit Declaration --
      --------------------------------------

      --  LIBRARY_UNIT_DECLARATION ::=
      --    SUBPROGRAM_DECLARATION | PACKAGE_DECLARATION
      --  | GENERIC_DECLARATION    | GENERIC_INSTANTIATION

      -----------------------------------------------
      -- 10.1.1  Library Unit Renaming Declaration --
      -----------------------------------------------

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
      --  Rtsfind routine, Is_Implicit_With is set, and the Sloc is typically
      --  set to Standard_Location, but it is incorrect to test the Sloc
      --  to find out if a with clause is implicit, test the flag instead.

      --  N_With_Clause
      --  Sloc points to first token of library unit name
      --  Name
      --  Private_Present set if with_clause has private keyword
      --  Limited_Present set if LIMITED is present
      --  Next_Implicit_With
      --  Library_Unit (i.e. Withed_Lib_Unit)
      --  Corresponding_Spec
      --  First_Name (set to True if first name or only one name)
      --  Last_Name (set to True if last name or only one name)
      --  Context_Installed
      --  Elaborate_Present
      --  Elaborate_All_Present
      --  Elaborate_All_Desirable
      --  Elaborate_Desirable
      --  Is_Implicit_With
      --  Limited_View_Installed
      --  Parent_With
      --  Unreferenced_In_Spec
      --  No_Entities_Ref_In_Spec

      --  Note: Limited_Present and Limited_View_Installed are used to support
      --  the implementation of Ada 2005 (AI-50217).

      --  Similarly, Private_Present is used to support the implementation of
      --  Ada 2005 (AI-50262).

      --  Note: if the WITH clause refers to a standard library unit, then a
      --  limited with clause is changed into a normal with clause, because we
      --  are not prepared to deal with limited with in the context of Rtsfind.
      --  So in this case, the Limited_Present flag will be False in the final
      --  tree.

      ----------------------
      -- With_Type clause --
      ----------------------

      --  This is a GNAT extension, used to implement mutually recursive
      --  types declared in different packages.

      --  Note: this is now obsolete. The functionality of this construct
      --  is now implemented by the Ada 2005 limited_with_clause.

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
      --    SUBPROGRAM_SPECIFICATION is separate
      --      [ASPECT_SPECIFICATION];

      --  N_Subprogram_Body_Stub
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification
      --  Corresponding_Spec_Of_Stub
      --  Library_Unit (i.e. Stub_Subunit)
      --  Corresponding_Body

      -------------------------------
      -- 10.1.3  Package Body Stub --
      -------------------------------

      --  PACKAGE_BODY_STUB ::=
      --    package body DEFINING_IDENTIFIER is separate
      --      [ASPECT_SPECIFICATION];

      --  N_Package_Body_Stub
      --  Sloc points to PACKAGE
      --  Defining_Identifier
      --  Corresponding_Spec_Of_Stub
      --  Library_Unit (i.e. Stub_Subunit)
      --  Corresponding_Body

      ----------------------------
      -- 10.1.3  Task Body Stub --
      ----------------------------

      --  TASK_BODY_STUB ::=
      --    task body DEFINING_IDENTIFIER is separate
      --      [ASPECT_SPECIFICATION];

      --  N_Task_Body_Stub
      --  Sloc points to TASK
      --  Defining_Identifier
      --  Corresponding_Spec_Of_Stub
      --  Library_Unit (i.e. Stub_Subunit)
      --  Corresponding_Body
      --  At_End_Proc (set to Empty if no clean up procedure)

      ---------------------------------
      -- 10.1.3  Protected Body Stub --
      ---------------------------------

      --  PROTECTED_BODY_STUB ::=
      --    protected body DEFINING_IDENTIFIER is separate
      --      [ASPECT_SPECIFICATION];

      --  Note: protected body stubs are not allowed in Ada 83 mode

      --  N_Protected_Body_Stub
      --  Sloc points to PROTECTED
      --  Defining_Identifier
      --  Corresponding_Spec_Of_Stub
      --  Library_Unit (i.e. Stub_Subunit)
      --  Corresponding_Body

      ---------------------
      -- 10.1.3  Subunit --
      ---------------------

      --  SUBUNIT ::= separate (PARENT_UNIT_NAME) PROPER_BODY

      --  N_Subunit
      --  Sloc points to SEPARATE
      --  Name is the name of the parent unit
      --  Proper_Body is the subunit body
      --  Corresponding_Stub is the stub declaration for the unit.

      ---------------------------------
      -- 11.1  Exception Declaration --
      ---------------------------------

      --  EXCEPTION_DECLARATION ::= DEFINING_IDENTIFIER_LIST : exception
      --    [ASPECT_SPECIFICATIONS];

      --  For consistency with object declarations etc., the parser converts
      --  the case of multiple identifiers being declared to a series of
      --  declarations in which the expression is copied, using the More_Ids
      --  and Prev_Ids flags to remember the source form as described in the
      --  section on "Handling of Defining Identifier Lists".

      --  N_Exception_Declaration
      --  Sloc points to EXCEPTION
      --  Defining_Identifier
      --  Expression
      --  Renaming_Exception
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)

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
      --  At the moment, the only cleanup action allowed is a single call to a
      --  parameterless procedure; this restriction could be lifted if we make
      --  some changes in gigi. The At_End_Proc field is an N_Identifier node
      --  that denotes the procedure to be called. The cleanup action occurs
      --  whenever the sequence of statements is left for any reason. The
      --  possible reasons are:
      --
      --      1. reaching the end of the sequence
      --      2. exit, return, or goto
      --      3. exception or abort
      --
      --  The cleanup action also occurs whenever the exception handlers are
      --  left.

      --  The AT END cleanup handler protects only the sequence of statements
      --  and the exception handlers (not the associated declarations of
      --  the parent), just like exception handlers do not protect the
      --  declarations. The big difference is that the cleanup actions occur
      --  on either a normal or an abnormal exit from the statement sequence.

      --  At_End_Proc is also a field of various nodes that can contain
      --  both Declarations and Handled_Statement_Sequence, such as subprogram
      --  bodies and block statements. In that case, the At_End_Proc
      --  protects the Declarations as well as the Handled_Statement_Sequence.

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

      --  Note: the cleanup_procedure_call does not go through the common
      --  processing for calls, which in particular means that it will not be
      --  automatically inlined in all cases, even though the procedure to be
      --  called is marked inline. More specifically, if the procedure comes
      --  from another unit than the main source unit, for example a run-time
      --  unit, then it needs to be manually added to the list of bodies to be
      --  inlined by invoking Add_Inlined_Body on it.

      --  N_Handled_Sequence_Of_Statements
      --  Sloc points to first token of first statement
      --  Statements
      --  End_Label (set to Empty if expander generated)
      --  Exception_Handlers (set to No_List if none present)
      --  At_End_Proc (set to Empty if no clean up procedure)

      --  Note: A Handled_Sequence_Of_Statements can contain both
      --  Exception_Handlers and an At_End_Proc.

      --  Note: the parent always contains a Declarations field which contains
      --  declarations associated with the handled sequence of statements. This
      --  is true even in the case of an accept statement (see description of
      --  the N_Accept_Statement node).

      --  End_Label refers to the containing construct

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
      --  Choice_Parameter (set to Empty if not present)
      --  Exception_Choices
      --  Statements
      --  Exception_Label (set to Empty if not present)
      --  Local_Raise_Statements (set to No_Elist if not present)
      --  Local_Raise_Not_OK
      --  Has_Local_Raise

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

      --  In Ada 2005, we have

      --  RAISE_STATEMENT ::=
      --    raise; | raise exception_NAME [with string_EXPRESSION];

      --  N_Raise_Statement
      --  Sloc points to RAISE
      --  Name (set to Empty if no exception name present)
      --  Expression (set to Empty if no expression present)

      ----------------------------
      -- 11.3  Raise Expression --
      ----------------------------

      --  RAISE_EXPRESSION ::= raise exception_NAME [with string_EXPRESSION]

      --  N_Raise_Expression
      --  Sloc points to RAISE
      --  Name (always present)
      --  Expression (set to Empty if no expression present)
      --  plus fields for expression

      -------------------------------
      -- 12.1  Generic Declaration --
      -------------------------------

      --  GENERIC_DECLARATION ::=
      --    GENERIC_SUBPROGRAM_DECLARATION | GENERIC_PACKAGE_DECLARATION

      ------------------------------------------
      -- 12.1  Generic Subprogram Declaration --
      ------------------------------------------

      --  GENERIC_SUBPROGRAM_DECLARATION ::=
      --    GENERIC_FORMAL_PART SUBPROGRAM_SPECIFICATION
      --      [ASPECT_SPECIFICATIONS];

      --  Note: Generic_Formal_Declarations can include pragmas and use clauses

      --  N_Generic_Subprogram_Declaration
      --  Sloc points to GENERIC
      --  Specification subprogram specification
      --  Corresponding_Body
      --  Generic_Formal_Declarations from generic formal part
      --  Parent_Spec

      ---------------------------------------
      -- 12.1  Generic Package Declaration --
      ---------------------------------------

      --  GENERIC_PACKAGE_DECLARATION ::=
      --    GENERIC_FORMAL_PART PACKAGE_SPECIFICATION
      --      [ASPECT_SPECIFICATIONS];

      --  Note: Generic_Formal_Declarations can include pragmas and use clauses

      --  N_Generic_Package_Declaration
      --  Sloc points to GENERIC
      --  Specification package specification
      --  Corresponding_Body
      --  Generic_Formal_Declarations from generic formal part
      --  Parent_Spec

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
      --      new generic_package_NAME [GENERIC_ACTUAL_PART]
      --        [ASPECT_SPECIFICATIONS];
      --  | [[not] overriding]
      --    procedure DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_procedure_NAME [GENERIC_ACTUAL_PART]
      --        [ASPECT_SPECIFICATIONS];
      --  | [[not] overriding]
      --    function DEFINING_DESIGNATOR is
      --      new generic_function_NAME [GENERIC_ACTUAL_PART]
      --        [ASPECT_SPECIFICATIONS];

      --  N_Package_Instantiation
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name
      --  Name
      --  Generic_Associations (set to No_List if no
      --   generic actual part)
      --  Parent_Spec
      --  Instance_Spec
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Is_Declaration_Level_Node
      --  Is_Known_Guaranteed_ABE

      --  N_Procedure_Instantiation
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name
      --  Name
      --  Parent_Spec
      --  Generic_Associations (set to No_List if no
      --   generic actual part)
      --  Instance_Spec
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Is_Declaration_Level_Node
      --  Must_Override set if overriding indicator present
      --  Must_Not_Override set if not_overriding indicator present
      --  Is_Known_Guaranteed_ABE

      --  N_Function_Instantiation
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name
      --  Name
      --  Generic_Associations (set to No_List if no
      --   generic actual part)
      --  Parent_Spec
      --  Instance_Spec
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Is_Declaration_Level_Node
      --  Must_Override set if overriding indicator present
      --  Must_Not_Override set if not_overriding indicator present
      --  Is_Known_Guaranteed_ABE

      --  Note: overriding indicator is an Ada 2005 feature

      -------------------------------
      -- 12.3  Generic Actual Part --
      -------------------------------

      --  GENERIC_ACTUAL_PART ::=
      --    (GENERIC_ASSOCIATION {, GENERIC_ASSOCIATION})

      -------------------------------
      -- 12.3  Generic Association --
      -------------------------------

      --  GENERIC_ASSOCIATION ::=
      --    [generic_formal_parameter_SELECTOR_NAME =>]
      --      EXPLICIT_GENERIC_ACTUAL_PARAMETER

      --  Note: unlike the procedure call case, a generic association node
      --  is generated for every association, even if no formal parameter
      --  selector name is present, in which case Selector_Name is Empty.

      --  In Ada 2005, a formal may be associated with a box, if the
      --  association is part of the list of actuals for a formal package.
      --  If the association is given by OTHERS => <>, the association is
      --  an N_Others_Choice (not an N_Generic_Association whose Selector_Name
      --  is an N_Others_Choice).

      --  In source nodes, either Explicit_Generic_Actual_Parameter is present,
      --  or Box_Present is True. However, Sem_Ch12 generates "dummy" nodes
      --  with Explicit_Generic_Actual_Parameter = Empty and Box_Present =
      --  False.

      --  N_Generic_Association
      --  Sloc points to first token of generic association
      --  Selector_Name (set to Empty if no formal
      --   parameter selector name)
      --  Explicit_Generic_Actual_Parameter (Empty if box present)
      --  Box_Present (for formal_package associations with a box)

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
      --      MODE [NULL_EXCLUSION] SUBTYPE_MARK [:= DEFAULT_EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];
      --  | DEFINING_IDENTIFIER_LIST :
      --      MODE ACCESS_DEFINITION [:= DEFAULT_EXPRESSION]
      --        [ASPECT_SPECIFICATIONS];

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Formal_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier
      --  In_Present
      --  Out_Present
      --  Null_Exclusion_Present (set to False if not present)
      --  Subtype_Mark (set to Empty if not present)
      --  Access_Definition (set to Empty if not present)
      --  Default_Expression (set to Empty if no default expression)
      --  More_Ids (set to False if no more identifiers in list)
      --  Prev_Ids (set to False if no previous identifiers in list)

      -----------------------------------
      -- 12.5  Formal Type Declaration --
      -----------------------------------

      --  FORMAL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is FORMAL_TYPE_DEFINITION
      --        [ASPECT_SPECIFICATIONS];
      --  | type DEFINING_IDENTIFIER [DISCRIMINANT_PART] [is tagged]

      --  N_Formal_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier
      --  Formal_Type_Definition
      --  Discriminant_Specifications (set to No_List if no
      --   discriminant part)
      --  Unknown_Discriminants_Present set if (<>) discriminant
      --  Default_Subtype_Mark

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
      --  | FORMAL_INTERFACE_TYPE_DEFINITION
      --  | FORMAL_INCOMPLETE_TYPE_DEFINITION

      --  The Ada 2012 syntax introduces two new non-terminals:
      --  Formal_{Complete,Incomplete}_Type_Declaration just to introduce
      --  the latter category. Here we introduce an incomplete type definition
      --  in order to preserve as much as possible the existing structure.

      ---------------------------------------------
      -- 12.5.1  Formal Private Type Definition --
      ---------------------------------------------

      --  FORMAL_PRIVATE_TYPE_DEFINITION ::=
      --    [[abstract] tagged] [limited] private

      --  Note: TAGGED is not allowed in Ada 83 mode

      --  N_Formal_Private_Type_Definition
      --  Sloc points to PRIVATE
      --  Uninitialized_Variable
      --  Abstract_Present
      --  Tagged_Present
      --  Limited_Present

      --------------------------------------------
      -- 12.5.1  Formal Derived Type Definition --
      --------------------------------------------

      --  FORMAL_DERIVED_TYPE_DEFINITION ::=
      --    [abstract] [limited | synchronized]
      --       new SUBTYPE_MARK [[and INTERFACE_LIST] with private]
      --  Note: this construct is not allowed in Ada 83 mode

      --  N_Formal_Derived_Type_Definition
      --  Sloc points to NEW
      --  Subtype_Mark
      --  Private_Present
      --  Abstract_Present
      --  Limited_Present
      --  Synchronized_Present
      --  Interface_List (set to No_List if none)

      -----------------------------------------------
      -- 12.5.1  Formal Incomplete Type Definition --
      -----------------------------------------------

      --  FORMAL_INCOMPLETE_TYPE_DEFINITION ::= [tagged]

      --  N_Formal_Incomplete_Type_Definition
      --  Sloc points to identifier of parent
      --  Tagged_Present

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

      ----------------------------------------------
      -- 12.5.5  Formal Interface Type Definition --
      ----------------------------------------------

      --  FORMAL_INTERFACE_TYPE_DEFINITION ::= INTERFACE_TYPE_DEFINITION

      -----------------------------------------
      -- 12.6  Formal Subprogram Declaration --
      -----------------------------------------

      --  FORMAL_SUBPROGRAM_DECLARATION ::=
      --    FORMAL_CONCRETE_SUBPROGRAM_DECLARATION
      --  | FORMAL_ABSTRACT_SUBPROGRAM_DECLARATION

      --------------------------------------------------
      -- 12.6  Formal Concrete Subprogram Declaration --
      --------------------------------------------------

      --  FORMAL_CONCRETE_SUBPROGRAM_DECLARATION ::=
      --    with SUBPROGRAM_SPECIFICATION [is SUBPROGRAM_DEFAULT]
      --      [ASPECT_SPECIFICATIONS];

      --  N_Formal_Concrete_Subprogram_Declaration
      --  Sloc points to WITH
      --  Specification
      --  Default_Name (set to Empty if no subprogram default)
      --  Box_Present
      --  Expression (set to Empty if no expression present)
      --  If the default is "is null", then Null_Present is set
      --  on the Specification of this node.

      --  Note: If no subprogram default is present, then Name is set
      --  to Empty, and Box_Present is False.

      --  Note: The Expression field is for the GNAT extension that allows a
      --  FORMAL_CONCRETE_SUBPROGRAM_DECLARATION to specify an expression
      --  default for generic formal functions.

      --------------------------------------------------
      -- 12.6  Formal Abstract Subprogram Declaration --
      --------------------------------------------------

      --  FORMAL_ABSTRACT_SUBPROGRAM_DECLARATION ::=
      --    with SUBPROGRAM_SPECIFICATION is abstract [SUBPROGRAM_DEFAULT]
      --      [ASPECT_SPECIFICATIONS];

      --  N_Formal_Abstract_Subprogram_Declaration
      --  Sloc points to WITH
      --  Specification
      --  Default_Name (set to Empty if no subprogram default)
      --  Box_Present

      --  Note: if no subprogram default is present, then Name is set
      --  to Empty, and Box_Present is False.

      ------------------------------
      -- 12.6  Subprogram Default --
      ------------------------------

      --  SUBPROGRAM_DEFAULT ::= DEFAULT_NAME | <> | (EXPRESSION)

      --  There is no separate node in the tree for a subprogram default.
      --  Instead the parent (N_Formal_Concrete_Subprogram_Declaration
      --  or N_Formal_Abstract_Subprogram_Declaration) node contains the
      --  default name or box indication, as needed.

      --  Note: The syntax "(EXPRESSION)" is a GNAT extension, and allows
      --  a FORMAL_CONCRETE_SUBPROGRAM_DECLARATION to specify an expression
      --  default for formal functions, in analogy with expression_functions.

      ------------------------
      -- 12.6  Default Name --
      ------------------------

      --  DEFAULT_NAME ::= NAME

      --------------------------------------
      -- 12.7  Formal Package Declaration --
      --------------------------------------

      --  FORMAL_PACKAGE_DECLARATION ::=
      --    with package DEFINING_IDENTIFIER
      --      is new generic_package_NAME FORMAL_PACKAGE_ACTUAL_PART
      --        [ASPECT_SPECIFICATIONS];

      --  Note: formal package declarations not allowed in Ada 83 mode

      --  N_Formal_Package_Declaration
      --  Sloc points to WITH
      --  Defining_Identifier
      --  Name
      --  Generic_Associations (set to No_List if (<>) case or
      --   empty formal package actual part)
      --  Box_Present
      --  Instance_Spec
      --  Is_Known_Guaranteed_ABE

      --------------------------------------
      -- 12.7  Formal Package Actual Part --
      --------------------------------------

      --  FORMAL_PACKAGE_ACTUAL_PART ::=
      --    ([OTHERS =>] <>)
      --    | [GENERIC_ACTUAL_PART]
      --    | (FORMAL_PACKAGE_ASSOCIATION {, FORMAL_PACKAGE_ASSOCIATION}
      --        [, OTHERS => <>])

      --  FORMAL_PACKAGE_ASSOCIATION ::=
      --   GENERIC_ASSOCIATION
      --  | GENERIC_FORMAL_PARAMETER_SELECTOR_NAME => <>

      --  There is no explicit node in the tree for a formal package actual
      --  part, nor for a formal package association. A formal package
      --  association is represented as a generic association, possibly with
      --  Box_Present.
      --
      --  The "others => <>" syntax (both cases) is represented as an
      --  N_Others_Choice (not an N_Generic_Association whose Selector_Name
      --  is an N_Others_Choice). This admittedly odd representation does not
      --  lose information, because "others" cannot be followed by anything
      --  other than "=> <>". Thus:
      --
      --  "... is new G;"
      --    The N_Formal_Package_Declaration has empty Generic_Associations,
      --    and Box_Present = False.
      --
      --  "... is new G(<>);"
      --    The N_Formal_Package_Declaration has empty Generic_Associations,
      --    and Box_Present = True.
      --
      --  "... is new G(others => <>);"
      --    The N_Formal_Package_Declaration has Generic_Associations with a
      --    single element, which is an N_Others_Choice.
      --    The N_Formal_Package_Declaration has Box_Present = False.
      --
      --  "... is new G(X, F => Y, others => <>);"
      --    The N_Formal_Package_Declaration has Generic_Associations with
      --    three elements, the last of which is an N_Others_Choice.
      --    The N_Formal_Package_Declaration has Box_Present = False.
      --
      --  "... is new G(F1 => X, F2 => <>, others => <>);"
      --    The N_Formal_Package_Declaration has Generic_Associations with
      --    three elements. The first is an N_Generic_Association with
      --    Box_Present = False. The second is an N_Generic_Association with
      --    Box_Present = True. The last is an N_Others_Choice.
      --    The N_Formal_Package_Declaration has Box_Present = False.

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

      --  Note: the only attribute definition clause that is processed by
      --  gigi is an address clause. For all other cases, the information
      --  is extracted by the front end and either results in setting entity
      --  information, e.g. Esize for the Size clause, or in appropriate
      --  expansion actions (e.g. in the case of Storage_Size).

      --  For an address clause, Gigi constructs the appropriate addressing
      --  code. It also ensures that no aliasing optimizations are made
      --  for the object for which the address clause appears.

      --  Note: for an address clause used to achieve an overlay:

      --    A : Integer;
      --    B : Integer;
      --    for B'Address use A'Address;

      --  the above rule means that Gigi will ensure that no optimizations
      --  will be made for B that would violate the implementation advice
      --  of RM 13.3(19). However, this advice applies only to B and not
      --  to A, which seems unfortunate. The GNAT front end will mark the
      --  object A as volatile to also prevent unwanted optimization
      --  assumptions based on no aliasing being made for B.

      --  N_Attribute_Definition_Clause
      --  Sloc points to FOR
      --  Name the local name
      --  Chars the identifier name from the attribute designator
      --  Expression the expression or name
      --  Entity
      --  Next_Rep_Item
      --  From_At_Mod
      --  Check_Address_Alignment
      --  From_Aspect_Specification
      --  Is_Delayed_Aspect
      --  Address_Warning_Posted

      --  Note: if From_Aspect_Specification is set, then Sloc points to the
      --  aspect name, and Entity is resolved already to reference the entity
      --  to which the aspect applies.

      -----------------------------------
      -- 13.3.1  Aspect Specifications --
      -----------------------------------

      --  We modify the RM grammar here, the RM grammar is:

      --     ASPECT_SPECIFICATION ::=
      --       with ASPECT_MARK [=> ASPECT_DEFINITION] {,
      --            ASPECT_MARK [=> ASPECT_DEFINITION] }

      --     ASPECT_MARK ::= aspect_IDENTIFIER['Class]

      --     ASPECT_DEFINITION ::= NAME | EXPRESSION

      --  That's inconvenient, since there is no non-terminal name for a single
      --  entry in the list of aspects. So we use this grammar instead:

      --     ASPECT_SPECIFICATIONS ::=
      --       with ASPECT_SPECIFICATION {, ASPECT_SPECIFICATION}

      --     ASPECT_SPECIFICATION =>
      --       ASPECT_MARK [=> ASPECT_DEFINITION]

      --     ASPECT_MARK ::= aspect_IDENTIFIER['Class]

      --     ASPECT_DEFINITION ::= NAME | EXPRESSION

      --  Note that for Annotate, the ASPECT_DEFINITION is a pure positional
      --  aggregate with the elements of the aggregate corresponding to the
      --  successive arguments of the corresponding pragma.

      --  See separate package Aspects for details on the incorporation of
      --  these nodes into the tree, and how aspect specifications for a given
      --  declaration node are associated with that node.

      --  N_Aspect_Specification
      --  Sloc points to aspect identifier
      --  Identifier aspect identifier
      --  Aspect_Rep_Item
      --  Expression (set to Empty if none)
      --  Entity entity to which the aspect applies
      --  Next_Rep_Item
      --  Class_Present Set if 'Class present
      --  Is_Ignored
      --  Is_Checked
      --  Is_Delayed_Aspect
      --  Is_Disabled
      --  Is_Boolean_Aspect
      --  Aspect_On_Partial_View

      --  Note: Aspect_Specification is an Ada 2012 feature

      --  Note: The Identifier serves to identify the aspect involved (it
      --  is the aspect whose name corresponds to the Chars field). This
      --  means that the other fields of this identifier are unused, and
      --  in particular we use the Entity field of this identifier to save
      --  a copy of the expression for visibility analysis, see spec of
      --  Sem_Ch13 for full details of this usage.

      --  In the case of aspects of the form xxx'Class, the aspect identifier
      --  is for xxx, and Class_Present is set to True.

      ---------------------------------------------
      -- 13.4  Enumeration representation clause --
      ---------------------------------------------

      --  ENUMERATION_REPRESENTATION_CLAUSE ::=
      --    for first_subtype_LOCAL_NAME use ENUMERATION_AGGREGATE;

      --  In Ada 83, the name must be a direct name

      --  N_Enumeration_Representation_Clause
      --  Sloc points to FOR
      --  Identifier direct name
      --  Array_Aggregate
      --  Next_Rep_Item

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
      --  Identifier direct name
      --  Mod_Clause (set to Empty if no mod clause present)
      --  Component_Clauses
      --  Next_Rep_Item

      ------------------------------
      -- 13.5.1  Component clause --
      ------------------------------

      --  COMPONENT_CLAUSE ::=
      --    component_LOCAL_NAME at POSITION
      --      range FIRST_BIT .. LAST_BIT;

      --  N_Component_Clause
      --  Sloc points to AT
      --  Component_Name points to Name or Attribute_Reference
      --  Position
      --  First_Bit
      --  Last_Bit

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

      --  See package System.Machine_Code in file s-maccod.ads for details on
      --  the allowed parameters to Asm. There are two ways this node can
      --  arise, as a code statement, in which case the expression is the
      --  qualified expression, or as a result of the expansion of an intrinsic
      --  call to the Asm or Asm_Input procedure.

      --  N_Code_Statement
      --  Sloc points to first token of the expression
      --  Expression

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
      --  Shift_Count_OK

      --  N_Op_Rotate_Right
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK

      --  N_Op_Shift_Left
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK

      --  N_Op_Shift_Right_Arithmetic
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK

      --  N_Op_Shift_Right
      --  Sloc points to the function name
      --  plus fields for binary operator
      --  plus fields for expression
      --  Shift_Count_OK

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
      --  Delta_Expression
      --  Range_Constraint (set to Empty if not present)

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
      --  Identifier
      --  Expression

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
      --  Expression
      --  Pragmas_Before Pragmas before mod clause (No_List if none)

   --------------------
   -- Semantic Nodes --
   --------------------

   --  These semantic nodes are used to hold additional semantic information.
   --  They are inserted into the tree as a result of semantic processing.
   --  Although there are no legitimate source syntax constructions that
   --  correspond directly to these nodes, we need a source syntax for the
   --  reconstructed tree printed by Sprint, and the node descriptions here
   --  show this syntax.

      -----------------
      -- Call_Marker --
      -----------------

      --  This node is created during the analysis/resolution of entry calls,
      --  requeues, and subprogram calls. It performs several functions:

      --    * Call markers provide a uniform model for handling calls by the
      --      ABE mechanism, regardless of whether expansion took place.

      --    * The call marker captures the target of the related call along
      --      with other attributes which are either unavailable or expensive
      --      to recompute once analysis, resolution, and expansion are over.

      --    * The call marker aids the ABE Processing phase by signaling the
      --      presence of a call in case the original call was transformed by
      --      expansion.

      --    * The call marker acts as a reference point for the insertion of
      --      run-time conditional ABE checks or guaranteed ABE failures.

      --  Sprint syntax: #target#

      --  The Sprint syntax shown above is not enabled by default

      --  N_Call_Marker
      --  Sloc points to Sloc of original call
      --  Target
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Is_Source_Call
      --  Is_Declaration_Level_Node
      --  Is_Dispatching_Call
      --  Is_Preelaborable_Call
      --  Is_Known_Guaranteed_ABE

      ------------------------
      -- Compound Statement --
      ------------------------

      --  This node is created by the analyzer/expander to handle some
      --  expansion cases where a sequence of actions needs to be captured
      --  within a single node (which acts as a container and allows the
      --  entire list of actions to be moved around as a whole) appearing
      --  in a sequence of statements.

      --  This is the statement counterpart to the expression node
      --  N_Expression_With_Actions.

      --  The required semantics is that the set of actions is executed in
      --  the order in which it appears, as though they appeared by themselves
      --  in the enclosing list of declarations or statements. Unlike what
      --  happens when using an N_Block_Statement, no new scope is introduced.

      --  Note: for the time being, this is used only as a transient
      --  representation during expansion, and all compound statement nodes
      --  must be exploded back to their constituent statements before handing
      --  the tree to the back end.

      --  Sprint syntax:  do
      --                    action;
      --                    action;
      --                    ...
      --                    action;
      --                  end;

      --  N_Compound_Statement
      --  Actions

      --------------
      -- Contract --
      --------------

      --  This node is used to hold the various parts of an entry, subprogram
      --  [body] or package [body] contract, in particular:
      --     Abstract states declared by a package declaration
      --     Contract cases that apply to a subprogram
      --     Dependency relations of inputs and output of a subprogram
      --     Global annotations classifying data as input or output
      --     Initialization sequences for a package declaration
      --     Pre- and postconditions that apply to a subprogram

      --  The node appears in an entry and [generic] subprogram [body] entity.

      --  Sprint syntax:  <none> as the node should not appear in the tree, but
      --                  only attached to an entry or [generic] subprogram
      --                  entity.

      --  N_Contract
      --  Sloc points to the subprogram's name
      --  Pre_Post_Conditions (set to Empty if none)
      --  Contract_Test_Cases (set to Empty if none)
      --  Classifications (set to Empty if none)

      --  Pre_Post_Conditions contains a collection of pragmas that correspond
      --  to pre- and postconditions associated with an entry or a subprogram
      --  [body or stub]. The pragmas can either come from source or be the
      --  byproduct of aspect expansion. Currently the following pragmas appear
      --  in this list:
      --    Post
      --    Postcondition
      --    Pre
      --    Precondition
      --    Refined_Post
      --  The ordering in the list is in LIFO fashion.

      --  Note that there might be multiple preconditions or postconditions
      --  in this list, because they come from separate pragmas in the source.

      --  In GNATprove mode, the inherited classwide pre- and postconditions
      --  (suitably specialized for the specific type of the overriding
      --  operation) are also in this list.

      --  Contract_Test_Cases contains a collection of pragmas that correspond
      --  to aspects/pragmas Contract_Cases, Exceptional_Cases, Test_Case and
      --  Subprogram_Variant. The ordering in the list is in LIFO fashion.

      --  Classifications contains pragmas that either declare, categorize, or
      --  establish dependencies between subprogram or package inputs and
      --  outputs. Currently the following pragmas appear in this list:
      --    Abstract_States
      --    Always_Terminates
      --    Async_Readers
      --    Async_Writers
      --    Constant_After_Elaboration
      --    Depends
      --    Effective_Reads
      --    Effective_Writes
      --    Extensions_Visible
      --    Global
      --    Initial_Condition
      --    Initializes
      --    Part_Of
      --    Refined_Depends
      --    Refined_Global
      --    Refined_States
      --    Volatile_Function
      --  The ordering is in LIFO fashion.

      -------------------
      -- Expanded Name --
      -------------------

      --  The N_Expanded_Name node is used to represent a selected component
      --  name that has been resolved to an expanded name. The semantic phase
      --  replaces N_Selected_Component nodes that represent names by the use
      --  of this node, leaving the N_Selected_Component node used only when
      --  the prefix is a record or protected type.

      --  The fields of the N_Expanded_Name node are laid out identically
      --  to those of the N_Selected_Component node, allowing conversion of
      --  an expanded name node to a selected component node to be done
      --  easily, see Sinfo.CN.Change_Selected_Component_To_Expanded_Name.

      --  There is no special sprint syntax for an expanded name

      --  N_Expanded_Name
      --  Sloc points to the period
      --  Chars copy of Chars field of selector name
      --  Prefix
      --  Selector_Name
      --  Entity
      --  Associated_Node
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Has_Private_View (set in generic units)
      --  Has_Secondary_Private_View (set in generic units)
      --  Redundant_Use
      --  Atomic_Sync_Required
      --  plus fields for expression

      -----------------------------
      -- Expression With Actions --
      -----------------------------

      --  This node is created by the analyzer/expander to handle some
      --  expansion cases, notably short-circuit forms where there are
      --  actions associated with the right-hand side operand.

      --  The N_Expression_With_Actions node represents an expression with
      --  an associated set of actions (which are executable statements and
      --  declarations, as might occur in a handled statement sequence).

      --  The required semantics is that the set of actions is executed in
      --  the order in which it appears just before the expression is
      --  evaluated (and these actions must only be executed if the value
      --  of the expression is evaluated). The node is considered to be
      --  a subexpression, whose value is the value of the Expression after
      --  executing all the actions.

      --  If the actions contain declarations, then these declarations may
      --  be referenced within the expression.

      --  (AI12-0236-1): In Ada 2022, for a declare_expression, the parser
      --  generates an N_Expression_With_Actions. Declare_expressions have
      --  various restrictions, which we do not enforce on
      --  N_Expression_With_Actions nodes that are generated by the
      --  expander. The two cases can be distinguished by looking at
      --  Comes_From_Source.

      --  ???Perhaps we should change the name of this node to
      --  N_Declare_Expression, and perhaps we should change the Sprint syntax
      --  to match the RM syntax for declare_expression.

      --  Sprint syntax:  do
      --                    action;
      --                    action;
      --                    ...
      --                    action;
      --                  in expression end

      --  N_Expression_With_Actions
      --  Actions
      --  Expression
      --  plus fields for expression

      --  Note: In the final generated tree presented to the code generator,
      --  the actions list is always non-null, since there is no point in this
      --  node if the actions are Empty. During semantic analysis there are
      --  cases where it is convenient to temporarily generate an empty actions
      --  list. This arises in cases where we create such an empty actions
      --  list, and it may or may not end up being a place where additional
      --  actions are inserted. The expander removes such empty cases after
      --  the expression of the node is fully analyzed and expanded, at which
      --  point it is safe to remove it, since no more actions can be inserted.

      --------------------------
      -- External Initializer --
      --------------------------

      --  This node is used to represent an instance of the
      --  External_Initialization aspect.

      --  N_External_Initializer
      --  File_Index
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
      --  Expression argument to unchecked deallocation call
      --  Storage_Pool
      --  Procedure_To_Call
      --  Actual_Designated_Subtype

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
      --  Entity
      --  Access_Types_To_Process (set to No_Elist if none)
      --  TSS_Elist (set to No_Elist if no associated TSS's)
      --  Actions (set to No_List if no freeze actions)
      --  First_Subtype_Link (set to Empty if no link)

      --  The Actions field holds actions associated with the freeze. These
      --  actions are elaborated at the point where the type is frozen.

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the FREEZE keyword in the Sprint file output.

      ---------------------------
      -- Freeze Generic Entity --
      ---------------------------

      --  The freeze point of an entity indicates the point at which the
      --  information needed to generate code for the entity is complete.
      --  The freeze node for an entity triggers expander activities, such as
      --  build initialization procedures, and backend activities, such as
      --  completing the elaboration of packages.

      --  For entities declared within a generic unit, for which no code is
      --  generated, the freeze point is not equally meaningful. However, in
      --  Ada 2012 several semantic checks on declarations must be delayed to
      --  the freeze point, and we need to include such a mark in the tree to
      --  trigger these checks. The Freeze_Generic_Entity node plays no other
      --  role, and is ignored by the expander and the back-end.

      --  Sprint syntax: freeze_generic entity-name

      --  N_Freeze_Generic_Entity
      --  Sloc points near freeze point
      --  Entity

      --------------------------------
      -- Implicit Label Declaration --
      --------------------------------

      --  An implicit label declaration is created for every occurrence of a
      --  label on a statement or a label on a block or loop. It is chained
      --  in the declarations of the innermost enclosing block as specified
      --  in RM section 5.1 (3).

      --  The Defining_Identifier is the actual identifier for the statement
      --  identifier. Note that the occurrence of the label is a reference, NOT
      --  the defining occurrence. The defining occurrence occurs at the head
      --  of the innermost enclosing block, and is represented by this node.

      --  Note: from the grammar, this might better be called an implicit
      --  statement identifier declaration, but the term we choose seems
      --  friendlier, since at least informally statement identifiers are
      --  called labels in both cases (i.e. when used in labels, and when
      --  used as the identifiers of blocks and loops).

      --  Note: although this is logically a semantic node, since it does not
      --  correspond directly to a source syntax construction, these nodes are
      --  actually created by the parser in a post pass done just after parsing
      --  is complete, before semantic analysis is started (see Par.Labl).

      --  Sprint syntax: labelname : label;

      --  N_Implicit_Label_Declaration
      --  Sloc points to the << token for a statement identifier, or to the
      --    LOOP, DECLARE, or BEGIN token for a loop or block identifier
      --  Defining_Identifier
      --  Label_Construct

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the label name in the generated declaration.

      ---------------------
      -- Itype Reference --
      ---------------------

      --  This node is used to create a reference to an Itype. The only purpose
      --  is to make sure the Itype is defined if this is the first reference.

      --  A typical use of this node is when an Itype is to be referenced in
      --  two branches of an IF statement. In this case it is important that
      --  the first use of the Itype not be inside the conditional, since then
      --  it might not be defined if the other branch of the IF is taken, in
      --  the case where the definition generates elaboration code.

      --  The Itype field points to the referenced Itype

      --  Sprint syntax: reference itype-name

      --  N_Itype_Reference
      --  Sloc points to the node generating the reference
      --  Itype

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the REFERENCE keyword in the file output.

      ---------------------
      -- Raise xxx Error --
      ---------------------

      --  One of these nodes is created during semantic analysis to replace
      --  a node for an expression that is determined to definitely raise
      --  the corresponding exception.

      --  The N_Raise_xxx_Error node may also stand alone in place
      --  of a declaration or statement, in which case it simply causes
      --  the exception to be raised (i.e. it is equivalent to a raise
      --  statement that raises the corresponding exception). This use
      --  is distinguished by the fact that the Etype in this case is
      --  Standard_Void_Type; in the subexpression case, the Etype is the
      --  same as the type of the subexpression which it replaces.

      --  If Condition is empty, then the raise is unconditional. If the
      --  Condition field is non-empty, it is a boolean expression which is
      --  first evaluated, and the exception is raised only if the value of the
      --  expression is True. In the unconditional case, the creation of this
      --  node is usually accompanied by a warning message (unless it appears
      --  within the right operand of a short-circuit form whose left argument
      --  is static and decisively eliminates elaboration of the raise
      --  operation). The condition field can ONLY be present when the node is
      --  used as a statement form; it must NOT be present in the case where
      --  the node appears within an expression.

      --  The exception is generated with a message that contains the
      --  file name and line number, and then appended text. The Reason
      --  code shows the text to be added. The Reason code is an element
      --  of the type Types.RT_Exception_Code, and indicates both the
      --  message to be added, and the exception to be raised (which must
      --  match the node type). The value is stored by storing a Uint which
      --  is the Pos value of the enumeration element in this type.

      --  Gigi restriction: This expander ensures that the type of the
      --  Condition field is always Standard.Boolean, even if the type
      --  in the source is some non-standard boolean type.

      --  Sprint syntax: [xxx_error "msg"]
      --             or: [xxx_error when condition "msg"]

      --  N_Raise_Constraint_Error
      --  Sloc references related construct
      --  Condition (set to Empty if no condition)
      --  Reason
      --  plus fields for expression

      --  N_Raise_Program_Error
      --  Sloc references related construct
      --  Condition (set to Empty if no condition)
      --  Reason
      --  plus fields for expression

      --  N_Raise_Storage_Error
      --  Sloc references related construct
      --  Condition (set to Empty if no condition)
      --  Reason
      --  plus fields for expression

      --  Note: Sloc is copied from the expression generating the exception.
      --  In the case where a debug source file is generated, the Sloc for
      --  this node points to the left bracket in the Sprint file output.

      --  Note: the back end may be required to translate these nodes into
      --  appropriate goto statements. See description of N_Push/Pop_xxx_Label.

      ---------------------------------------------
      -- Optimization of Exception Raise to Goto --
      ---------------------------------------------

      --  In some cases, the front end will determine that any exception raised
      --  by the back end for a certain exception should be transformed into a
      --  goto statement.

      --  There are three kinds of exceptions raised by the back end (note that
      --  for this purpose we consider gigi to be part of the back end in the
      --  gcc case):

      --     1. Exceptions resulting from N_Raise_xxx_Error nodes
      --     2. Exceptions from checks triggered by Do_xxx_Check flags
      --     3. Other cases not specifically marked by the front end

      --  Normally all such exceptions are translated into calls to the proper
      --  Rcheck_xx procedure, where xx encodes both the exception to be raised
      --  and the exception message.

      --  The front end may determine that for a particular sequence of code,
      --  exceptions in any of these three categories for a particular builtin
      --  exception should result in a goto, rather than a call to Rcheck_xx.
      --  The exact sequence to be generated is:

      --      Local_Raise (exception'Identity);
      --      goto Label

      --  The front end marks such a sequence of code by bracketing it with
      --  push and pop nodes:

      --       N_Push_xxx_Label (referencing the label)
      --       ...
      --       (code where transformation is expected for exception xxx)
      --       ...
      --       N_Pop_xxx_Label

      --  The use of push/pop reflects the fact that such regions can properly
      --  nest, and one special case is a subregion in which no transformation
      --  is allowed. Such a region is marked by a N_Push_xxx_Label node whose
      --  Exception_Label field is Empty.

      --  N_Push_Constraint_Error_Label
      --  Sloc references first statement in region covered
      --  Exception_Label

      --  N_Push_Program_Error_Label
      --  Sloc references first statement in region covered
      --  Exception_Label

      --  N_Push_Storage_Error_Label
      --  Sloc references first statement in region covered
      --  Exception_Label

      --  N_Pop_Constraint_Error_Label
      --  Sloc references last statement in region covered

      --  N_Pop_Program_Error_Label
      --  Sloc references last statement in region covered

      --  N_Pop_Storage_Error_Label
      --  Sloc references last statement in region covered

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

      --  Note: evaluating a N_Reference node is guaranteed to yield a non-null
      --  value at run time. Therefore, it is valid to set Is_Known_Non_Null on
      --  a temporary initialized to a N_Reference node in order to eliminate
      --  superfluous access checks.

      --  Sprint syntax: prefix'reference

      --  N_Reference
      --  Sloc is copied from the expression
      --  Prefix
      --  plus fields for expression

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the quote in the Sprint file output.

      ----------------
      -- SCIL Nodes --
      ----------------

      --  SCIL nodes are special nodes added to the tree when the CodePeer mode
      --  is active. They are only generated if SCIL generation is enabled.
      --  A standard tree-walk will not encounter these nodes even if they
      --  are present; these nodes are only accessible via the function
      --  SCIL_LL.Get_SCIL_Node. These nodes have no associated dynamic
      --  semantics.

      --  Sprint syntax: [ <node kind> ]
      --  No semantic field values are displayed.

      --  N_SCIL_Dispatch_Table_Tag_Init
      --  Sloc references a node for a tag initialization
      --  SCIL_Entity
      --
      --  An N_SCIL_Dispatch_Table_Tag_Init node may be associated (via
      --  Get_SCIL_Node) with the N_Object_Declaration node corresponding to
      --  the declaration of the dispatch table for a tagged type.

      --  N_SCIL_Dispatching_Call
      --  Sloc references the node of a dispatching call
      --  SCIL_Target_Prim
      --  SCIL_Entity
      --  SCIL_Controlling_Tag
      --
      --  An N_Scil_Dispatching call node may be associated (via Get_SCIL_Node)
      --  with the N_Procedure_Call_Statement or N_Function_Call node (or a
      --  rewriting thereof) corresponding to a dispatching call.

      --  N_SCIL_Membership_Test
      --  Sloc references the node of a membership test
      --  SCIL_Tag_Value
      --  SCIL_Entity
      --
      --  An N_Scil_Membership_Test node may be associated (via Get_SCIL_Node)
      --  with the N_In node (or a rewriting thereof) corresponding to a
      --  classwide membership test.

      -------------------------------
      -- Unchecked Type Conversion --
      -------------------------------

      --  An unchecked type conversion node represents the semantic action
      --  corresponding to a call to an instantiation of Unchecked_Conversion.
      --  It is generated as a result of actual use of Unchecked_Conversion
      --  and also by the expander.

      --  Unchecked type conversion nodes should be created by calling
      --  Tbuild.Unchecked_Convert_To, rather than by directly calling
      --  Nmake.Make_Unchecked_Type_Conversion.

      --  Note: an unchecked type conversion is a variable as far as the
      --  semantics are concerned, which is convenient for the expander.
      --  This does not change what Ada source programs are legal, since
      --  clearly a function call to an instantiation of Unchecked_Conversion
      --  is not a variable in any case.

      --  Sprint syntax: subtype-mark!(expression)

      --  N_Unchecked_Type_Conversion
      --  Sloc points to related node in source
      --  Subtype_Mark
      --  Expression
      --  Kill_Range_Check
      --  No_Truncation
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
      --  checks are done in the front end.

      --  However, in the case of specialized back-ends, in particular the JVM
      --  backend in the past, additional requirements and restrictions may
      --  apply to unchecked conversion, and these are most conveniently
      --  performed in the specialized back-end.

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
      --  Source_Type
      --  Target_Type

      --  Note: in the case where a debug source file is generated, the Sloc
      --  for this node points to the VALIDATE keyword in the file output.

      -------------------------------
      -- Variable_Reference_Marker --
      -------------------------------

      --  This node is created during the analysis of direct or expanded names,
      --  and the resolution of entry and subprogram calls. It performs several
      --  functions:

      --    * Variable reference markers provide a uniform model for handling
      --      variable references by the ABE mechanism, regardless of whether
      --      expansion took place.

      --    * The variable reference marker captures the entity of the variable
      --      being read or written.

      --    * The variable reference markers aid the ABE Processing phase by
      --      signaling the presence of a call in case the original variable
      --      reference was transformed by expansion.

      --  Sprint syntax:  r#target#  --  for a read
      --                 rw#target#  --  for a read/write
      --                  w#target#  --  for a write

      --  The Sprint syntax shown above is not enabled by default

      --  N_Variable_Reference_Marker
      --  Sloc points to Sloc of original variable reference
      --  Target
      --  Is_Elaboration_Checks_OK_Node
      --  Is_SPARK_Mode_On_Node
      --  Is_Elaboration_Warnings_OK_Node
      --  Is_Read
      --  Is_Write

   -----------
   -- Empty --
   -----------

   --  Used as the contents of the Nkind field of the dummy Empty node and in
   --  some other situations to indicate an uninitialized value.

   --  N_Empty
   --  Chars is set to No_Name

   -----------
   -- Error --
   -----------

   --  Used as the contents of the Nkind field of the dummy Error node.
   --  Has an Etype field, which gets set to Any_Type later on, to help
   --  error recovery (Error_Posted is also set in the Error node).

   --  N_Error
   --  Chars is set to Error_Name
   --  Etype

end Sinfo;
