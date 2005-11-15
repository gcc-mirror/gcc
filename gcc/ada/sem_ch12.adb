------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 2                              --
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
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Freeze;   use Freeze;
with Hostparm;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rident;   use Rident;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Elab; use Sem_Elab;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Uname;    use Uname;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with GNAT.HTable;

package body Sem_Ch12 is

   ----------------------------------------------------------
   -- Implementation of Generic Analysis and Instantiation --
   -----------------------------------------------------------

   --  GNAT implements generics by macro expansion. No attempt is made to
   --  share generic instantiations (for now). Analysis of a generic definition
   --  does not perform any expansion action, but the expander must be called
   --  on the tree for each instantiation, because the expansion may of course
   --  depend on the generic actuals. All of this is best achieved as follows:
   --
   --  a) Semantic analysis of a generic unit is performed on a copy of the
   --  tree for the generic unit. All tree modifications that follow analysis
   --  do not affect the original tree. Links are kept between the original
   --  tree and the copy, in order to recognize non-local references within
   --  the generic, and propagate them to each instance (recall that name
   --  resolution is done on the generic declaration: generics are not really
   --  macros!). This is summarized in the following diagram:
   --
   --              .-----------.               .----------.
   --              |  semantic |<--------------|  generic |
   --              |    copy   |               |    unit  |
   --              |           |==============>|          |
   --              |___________|    global     |__________|
   --                             references     |   |  |
   --                                            |   |  |
   --                                          .-----|--|.
   --                                          |  .-----|---.
   --                                          |  |  .----------.
   --                                          |  |  |  generic |
   --                                          |__|  |          |
   --                                             |__| instance |
   --                                                |__________|
   --
   --  b) Each instantiation copies the original tree, and inserts into it a
   --  series of declarations that describe the mapping between generic formals
   --  and actuals. For example, a generic In OUT parameter is an object
   --  renaming of the corresponing actual, etc. Generic IN parameters are
   --  constant declarations.
   --
   --  c) In order to give the right visibility for these renamings, we use
   --  a different scheme for package and subprogram instantiations. For
   --  packages, the list of renamings is inserted into the package
   --  specification, before the visible declarations of the package. The
   --  renamings are analyzed before any of the text of the instance, and are
   --  thus visible at the right place. Furthermore, outside of the instance,
   --  the generic parameters are visible and denote their corresponding
   --  actuals.

   --  For subprograms, we create a container package to hold the renamings
   --  and the subprogram instance itself. Analysis of the package makes the
   --  renaming declarations visible to the subprogram. After analyzing the
   --  package, the defining entity for the subprogram is touched-up so that
   --  it appears declared in the current scope, and not inside the container
   --  package.

   --  If the instantiation is a compilation unit, the container package is
   --  given the same name as the subprogram instance. This ensures that
   --  the elaboration procedure called by the binder, using the compilation
   --  unit name, calls in fact the elaboration procedure for the package.

   --  Not surprisingly, private types complicate this approach. By saving in
   --  the original generic object the non-local references, we guarantee that
   --  the proper entities are referenced at the point of instantiation.
   --  However, for private types, this by itself does not insure that the
   --  proper VIEW of the entity is used (the full type may be visible at the
   --  point of generic definition, but not at instantiation, or vice-versa).
   --  In  order to reference the proper view, we special-case any reference
   --  to private types in the generic object, by saving both views, one in
   --  the generic and one in the semantic copy. At time of instantiation, we
   --  check whether the two views are consistent, and exchange declarations if
   --  necessary, in order to restore the correct visibility. Similarly, if
   --  the instance view is private when the generic view was not, we perform
   --  the exchange. After completing the instantiation, we restore the
   --  current visibility. The flag Has_Private_View marks identifiers in the
   --  the generic unit that require checking.

   --  Visibility within nested generic units requires special handling.
   --  Consider the following scheme:
   --
   --  type Global is ...         --  outside of generic unit.
   --  generic ...
   --  package Outer is
   --     ...
   --     type Semi_Global is ... --  global to inner.
   --
   --     generic ...                                         -- 1
   --     procedure inner (X1 : Global;  X2 : Semi_Global);
   --
   --     procedure in2 is new inner (...);                   -- 4
   --  end Outer;

   --  package New_Outer is new Outer (...);                  -- 2
   --  procedure New_Inner is new New_Outer.Inner (...);      -- 3

   --  The semantic analysis of Outer captures all occurrences of Global.
   --  The semantic analysis of Inner (at 1) captures both occurrences of
   --  Global and Semi_Global.

   --  At point 2 (instantiation of Outer), we also produce a generic copy
   --  of Inner, even though Inner is, at that point, not being instantiated.
   --  (This is just part of the semantic analysis of New_Outer).

   --  Critically, references to Global within Inner must be preserved, while
   --  references to Semi_Global should not preserved, because they must now
   --  resolve to an entity within New_Outer. To distinguish between these, we
   --  use a global variable, Current_Instantiated_Parent, which is set when
   --  performing a generic copy during instantiation (at 2). This variable is
   --  used when performing a generic copy that is not an instantiation, but
   --  that is nested within one, as the occurrence of 1 within 2. The analysis
   --  of a nested generic only preserves references that are global to the
   --  enclosing Current_Instantiated_Parent. We use the Scope_Depth value to
   --  determine whether a reference is external to the given parent.

   --  The instantiation at point 3 requires no special treatment. The method
   --  works as well for further nestings of generic units, but of course the
   --  variable Current_Instantiated_Parent must be stacked because nested
   --  instantiations can occur, e.g. the occurrence of 4 within 2.

   --  The instantiation of package and subprogram bodies is handled in a
   --  similar manner, except that it is delayed until after semantic
   --  analysis is complete. In this fashion complex cross-dependencies
   --  between several package declarations and bodies containing generics
   --  can be compiled which otherwise would diagnose spurious circularities.

   --  For example, it is possible to compile two packages A and B that
   --  have the following structure:

   --    package A is                         package B is
   --       generic ...                          generic ...
   --       package G_A is                       package G_B is

   --    with B;                              with A;
   --    package body A is                    package body B is
   --       package N_B is new G_B (..)          package N_A is new G_A (..)

   --  The table Pending_Instantiations in package Inline is used to keep
   --  track of body instantiations that are delayed in this manner. Inline
   --  handles the actual calls to do the body instantiations. This activity
   --  is part of Inline, since the processing occurs at the same point, and
   --  for essentially the same reason, as the handling of inlined routines.

   ----------------------------------------------
   -- Detection of Instantiation Circularities --
   ----------------------------------------------

   --  If we have a chain of instantiations that is circular, this is a
   --  static error which must be detected at compile time. The detection
   --  of these circularities is carried out at the point that we insert
   --  a generic instance spec or body. If there is a circularity, then
   --  the analysis of the offending spec or body will eventually result
   --  in trying to load the same unit again, and we detect this problem
   --  as we analyze the package instantiation for the second time.

   --  At least in some cases after we have detected the circularity, we
   --  get into trouble if we try to keep going. The following flag is
   --  set if a circularity is detected, and used to abandon compilation
   --  after the messages have been posted.

   Circularity_Detected : Boolean := False;
   --  This should really be reset on encountering a new main unit, but in
   --  practice we are not using multiple main units so it is not critical.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Abandon_Instantiation (N : Node_Id);
   pragma No_Return (Abandon_Instantiation);
   --  Posts an error message "instantiation abandoned" at the indicated
   --  node and then raises the exception Instantiation_Error to do it.

   procedure Analyze_Formal_Array_Type
     (T   : in out Entity_Id;
      Def : Node_Id);
   --  A formal array type is treated like an array type declaration, and
   --  invokes Array_Type_Declaration (sem_ch3) whose first parameter is
   --  in-out, because in the case of an anonymous type the entity is
   --  actually created in the procedure.

   --  The following procedures treat other kinds of formal parameters

   procedure Analyze_Formal_Derived_Interface_Type
     (T   : Entity_Id;
      Def : Node_Id);

   procedure Analyze_Formal_Derived_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);

   --  The following subprograms create abbreviated declarations for formal
   --  scalar types. We introduce an anonymous base of the proper class for
   --  each of them, and define the formals as constrained first subtypes of
   --  their bases. The bounds are expressions that are non-static in the
   --  generic.

   procedure Analyze_Formal_Decimal_Fixed_Point_Type
                                                (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Discrete_Type       (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Floating_Type       (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Interface_Type      (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Signed_Integer_Type (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Modular_Type        (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Ordinary_Fixed_Point_Type
                                                (T : Entity_Id; Def : Node_Id);

   procedure Analyze_Formal_Private_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);
   --  This needs comments???

   procedure Analyze_Generic_Formal_Part (N : Node_Id);

   procedure Analyze_Generic_Access_Type (T : Entity_Id; Def : Node_Id);
   --  This needs comments ???

   function Analyze_Associations
     (I_Node  : Node_Id;
      Formals : List_Id;
      F_Copy  : List_Id) return List_Id;
   --  At instantiation time, build the list of associations between formals
   --  and actuals. Each association becomes a renaming declaration for the
   --  formal entity. F_Copy is the analyzed list of formals in the generic
   --  copy. It is used to apply legality checks to the actuals. I_Node is the
   --  instantiation node itself.

   procedure Analyze_Subprogram_Instantiation
     (N : Node_Id;
      K : Entity_Kind);

   procedure Build_Instance_Compilation_Unit_Nodes
     (N        : Node_Id;
      Act_Body : Node_Id;
      Act_Decl : Node_Id);
   --  This procedure is used in the case where the generic instance of a
   --  subprogram body or package body is a library unit. In this case, the
   --  original library unit node for the generic instantiation must be
   --  replaced by the resulting generic body, and a link made to a new
   --  compilation unit node for the generic declaration. The argument N is
   --  the original generic instantiation. Act_Body and Act_Decl are the body
   --  and declaration of the instance (either package body and declaration
   --  nodes or subprogram body and declaration nodes depending on the case).
   --  On return, the node N has been rewritten with the actual body.

   procedure Check_Formal_Packages (P_Id : Entity_Id);
   --  Apply the following to all formal packages in generic associations

   procedure Check_Formal_Package_Instance
     (Formal_Pack : Entity_Id;
      Actual_Pack : Entity_Id);
   --  Verify that the actuals of the actual instance match the actuals of
   --  the template for a formal package that is not declared with a box.

   procedure Check_Forward_Instantiation (Decl : Node_Id);
   --  If the generic is a local entity and the corresponding body has not
   --  been seen yet, flag enclosing packages to indicate that it will be
   --  elaborated after the generic body. Subprograms declared in the same
   --  package cannot be inlined by the front-end because front-end inlining
   --  requires a strict linear order of elaboration.

   procedure Check_Hidden_Child_Unit
     (N           : Node_Id;
      Gen_Unit    : Entity_Id;
      Act_Decl_Id : Entity_Id);
   --  If the generic unit is an implicit child instance within a parent
   --  instance, we need to make an explicit test that it is not hidden by
   --  a child instance of the same name and parent.

   procedure Check_Private_View (N : Node_Id);
   --  Check whether the type of a generic entity has a different view between
   --  the point of generic analysis and the point of instantiation. If the
   --  view has changed, then at the point of instantiation we restore the
   --  correct view to perform semantic analysis of the instance, and reset
   --  the current view after instantiation. The processing is driven by the
   --  current private status of the type of the node, and Has_Private_View,
   --  a flag that is set at the point of generic compilation. If view and
   --  flag are inconsistent then the type is updated appropriately.

   procedure Check_Generic_Actuals
     (Instance      : Entity_Id;
      Is_Formal_Box : Boolean);
   --  Similar to previous one. Check the actuals in the instantiation,
   --  whose views can change between the point of instantiation and the point
   --  of instantiation of the body. In addition, mark the generic renamings
   --  as generic actuals, so that they are not compatible with other actuals.
   --  Recurse on an actual that is a formal package whose declaration has
   --  a box.

   function Contains_Instance_Of
     (Inner : Entity_Id;
      Outer : Entity_Id;
      N     : Node_Id) return Boolean;
   --  Inner is instantiated within the generic Outer. Check whether Inner
   --  directly or indirectly contains an instance of Outer or of one of its
   --  parents, in the case of a subunit. Each generic unit holds a list of
   --  the entities instantiated within (at any depth). This procedure
   --  determines whether the set of such lists contains a cycle, i.e. an
   --  illegal circular instantiation.

   function Denotes_Formal_Package
     (Pack    : Entity_Id;
      On_Exit : Boolean := False) return Boolean;
   --  Returns True if E is a formal package of an enclosing generic, or
   --  the actual for such a formal in an enclosing instantiation. If such
   --  a package is used as a formal in an nested generic, or as an actual
   --  in a nested instantiation, the visibility of ITS formals should not
   --  be modified. When called from within Restore_Private_Views, the flag
   --  On_Exit is true, to indicate that the search for a possible enclosing
   --  instance should ignore the current one.

   function Find_Actual_Type
     (Typ       : Entity_Id;
      Gen_Scope : Entity_Id) return Entity_Id;
   --  When validating the actual types of a child instance, check whether
   --  the formal is a formal type of the parent unit, and retrieve the current
   --  actual for it. Typ is the entity in the analyzed formal type declaration
   --  (component or index type of an array type) and Gen_Scope is the scope of
   --  the analyzed formal array type.

   function In_Same_Declarative_Part
     (F_Node : Node_Id;
      Inst   : Node_Id) return Boolean;
   --  True if the instantiation Inst and the given freeze_node F_Node appear
   --  within the same declarative part, ignoring subunits, but with no inter-
   --  vening suprograms or concurrent units. If true, the freeze node
   --  of the instance can be placed after the freeze node of the parent,
   --  which it itself an instance.

   function In_Main_Context (E : Entity_Id) return Boolean;
   --  Check whether an instantiation is in the context of the main unit.
   --  Used to determine whether its body should be elaborated to allow
   --  front-end inlining.

   procedure Set_Instance_Env
     (Gen_Unit : Entity_Id;
      Act_Unit : Entity_Id);
   --  Save current instance on saved environment, to be used to determine
   --  the global status of entities in nested instances. Part of Save_Env.
   --  called after verifying that the generic unit is legal for the instance.

   procedure Set_Instance_Of (A : Entity_Id; B : Entity_Id);
   --  Associate analyzed generic parameter with corresponding
   --  instance. Used for semantic checks at instantiation time.

   function Has_Been_Exchanged (E : Entity_Id) return Boolean;
   --  Traverse the Exchanged_Views list to see if a type was private
   --  and has already been flipped during this phase of instantiation.

   procedure Hide_Current_Scope;
   --  When compiling a generic child unit, the parent context must be
   --  present, but the instance and all entities that may be generated
   --  must be inserted in the current scope. We leave the current scope
   --  on the stack, but make its entities invisible to avoid visibility
   --  problems. This is reversed at the end of instantiations. This is
   --  not done for the instantiation of the bodies, which only require the
   --  instances of the generic parents to be in scope.

   procedure Install_Body
     (Act_Body : Node_Id;
      N        : Node_Id;
      Gen_Body : Node_Id;
      Gen_Decl : Node_Id);
   --  If the instantiation happens textually before the body of the generic,
   --  the instantiation of the body must be analyzed after the generic body,
   --  and not at the point of instantiation. Such early instantiations can
   --  happen if the generic and the instance appear in  a package declaration
   --  because the generic body can only appear in the corresponding package
   --  body. Early instantiations can also appear if generic, instance and
   --  body are all in the declarative part of a subprogram or entry. Entities
   --  of packages that are early instantiations are delayed, and their freeze
   --  node appears after the generic body.

   procedure Insert_After_Last_Decl (N : Node_Id; F_Node : Node_Id);
   --  Insert freeze node at the end of the declarative part that includes the
   --  instance node N. If N is in the visible part of an enclosing package
   --  declaration, the freeze node has to be inserted at the end of the
   --  private declarations, if any.

   procedure Freeze_Subprogram_Body
     (Inst_Node : Node_Id;
      Gen_Body  : Node_Id;
      Pack_Id   : Entity_Id);
   --  The generic body may appear textually after the instance, including
   --  in the proper body of a stub, or within a different package instance.
   --  Given that the instance can only be elaborated after the generic, we
   --  place freeze_nodes for the instance and/or for packages that may enclose
   --  the instance and the generic, so that the back-end can establish the
   --  proper order of elaboration.

   procedure Init_Env;
   --  Establish environment for subsequent instantiation. Separated from
   --  Save_Env because data-structures for visibility handling must be
   --  initialized before call to Check_Generic_Child_Unit.

   procedure Install_Parent (P : Entity_Id; In_Body : Boolean := False);
   --  When compiling an instance of a child unit the parent (which is
   --  itself an instance) is an enclosing scope that must be made
   --  immediately visible. This procedure is also used to install the non-
   --  generic parent of a generic child unit when compiling its body, so
   --  that full views of types in the parent are made visible.

   procedure Remove_Parent (In_Body : Boolean := False);
   --  Reverse effect after instantiation of child is complete

   procedure Inline_Instance_Body
     (N        : Node_Id;
      Gen_Unit : Entity_Id;
      Act_Decl : Node_Id);
   --  If front-end inlining is requested, instantiate the package body,
   --  and preserve the visibility of its compilation unit, to insure
   --  that successive instantiations succeed.

   --  The functions Instantiate_XXX perform various legality checks and build
   --  the declarations for instantiated generic parameters. In all of these
   --  Formal is the entity in the generic unit, Actual is the entity of
   --  expression in the generic associations, and Analyzed_Formal is the
   --  formal in the generic copy, which contains the semantic information to
   --  be used to validate the actual.

   function Instantiate_Object
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return List_Id;

   function Instantiate_Type
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id;
      Actual_Decls    : List_Id) return Node_Id;

   function Instantiate_Formal_Subprogram
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return Node_Id;

   function Instantiate_Formal_Package
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return List_Id;
   --  If the formal package is declared with a box, special visibility rules
   --  apply to its formals: they are in the visible part of the package. This
   --  is true in the declarative region of the formal package, that is to say
   --  in the enclosing generic or instantiation. For an instantiation, the
   --  parameters of the formal package are made visible in an explicit step.
   --  Furthermore, if the actual is a visible use_clause, these formals must
   --  be made potentially use_visible as well. On exit from the enclosing
   --  instantiation, the reverse must be done.

   --  For a formal package declared without a box, there are conformance rules
   --  that apply to the actuals in the generic declaration and the actuals of
   --  the actual package in the enclosing instantiation. The simplest way to
   --  apply these rules is to repeat the instantiation of the formal package
   --  in the context of the enclosing instance, and compare the generic
   --  associations of this instantiation with those of the actual package.

   function Is_In_Main_Unit (N : Node_Id) return Boolean;
   --  Test if given node is in the main unit

   procedure Load_Parent_Of_Generic (N : Node_Id; Spec : Node_Id);
   --  If the generic appears in a separate non-generic library unit,
   --  load the corresponding body to retrieve the body of the generic.
   --  N is the node for the generic instantiation, Spec is the generic
   --  package declaration.

   procedure Inherit_Context (Gen_Decl : Node_Id; Inst : Node_Id);
   --  Add the context clause of the unit containing a generic unit to
   --  an instantiation that is a compilation unit.

   function Get_Associated_Node (N : Node_Id) return Node_Id;
   --  In order to propagate semantic information back from the analyzed
   --  copy to the original generic, we maintain links between selected nodes
   --  in the generic and their corresponding copies. At the end of generic
   --  analysis, the routine Save_Global_References traverses the generic
   --  tree, examines the semantic information, and preserves the links to
   --  those nodes that contain global information. At instantiation, the
   --  information from the associated node is placed on the new copy, so
   --  that name resolution is not repeated.
   --
   --  Three kinds of source nodes have associated nodes:
   --
   --    a) those that can reference (denote) entities, that is identifiers,
   --       character literals, expanded_names, operator symbols, operators,
   --       and attribute reference nodes. These nodes have an Entity field
   --       and are the set of nodes that are in N_Has_Entity.
   --
   --    b) aggregates (N_Aggregate and N_Extension_Aggregate)
   --
   --    c) selected components (N_Selected_Component)
   --
   --  For the first class, the associated node preserves the entity if it is
   --  global. If the generic contains nested instantiations, the associated
   --  node itself has been recopied, and a chain of them must be followed.
   --
   --  For aggregates, the associated node allows retrieval of the type, which
   --  may otherwise not appear in the generic. The view of this type may be
   --  different between generic and instantiation, and the full view can be
   --  installed before the instantiation is analyzed. For aggregates of
   --  type extensions, the same view exchange may have to be performed for
   --  some of the ancestor types, if their view is private at the point of
   --  instantiation.
   --
   --  Nodes that are selected components in the parse tree may be rewritten
   --  as expanded names after resolution, and must be treated as potential
   --  entity holders. which is why they also have an Associated_Node.
   --
   --  Nodes that do not come from source, such as freeze nodes, do not appear
   --  in the generic tree, and need not have an associated node.
   --
   --  The associated node is stored in the Associated_Node field. Note that
   --  this field overlaps Entity, which is fine, because the whole point is
   --  that we don't need or want the normal Entity field in this situation.

   procedure Move_Freeze_Nodes
     (Out_Of : Entity_Id;
      After  : Node_Id;
      L      : List_Id);
   --  Freeze nodes can be generated in the analysis of a generic unit, but
   --  will not be seen by the back-end. It is necessary to move those nodes
   --  to the enclosing scope if they freeze an outer entity. We place them
   --  at the end of the enclosing generic package, which is semantically
   --  neutral.

   procedure Pre_Analyze_Actuals (N : Node_Id);
   --  Analyze actuals to perform name resolution. Full resolution is done
   --  later, when the expected types are known, but names have to be captured
   --  before installing parents of generics, that are not visible for the
   --  actuals themselves.

   procedure Valid_Default_Attribute (Nam : Entity_Id; Def : Node_Id);
   --  Verify that an attribute that appears as the default for a formal
   --  subprogram is a function or procedure with the correct profile.

   -------------------------------------------
   -- Data Structures for Generic Renamings --
   -------------------------------------------

   --  The map Generic_Renamings associates generic entities with their
   --  corresponding actuals. Currently used to validate type instances.
   --  It will eventually be used for all generic parameters to eliminate
   --  the need for overload resolution in the instance.

   type Assoc_Ptr is new Int;

   Assoc_Null : constant Assoc_Ptr := -1;

   type Assoc is record
      Gen_Id         : Entity_Id;
      Act_Id         : Entity_Id;
      Next_In_HTable : Assoc_Ptr;
   end record;

   package Generic_Renamings is new Table.Table
     (Table_Component_Type => Assoc,
      Table_Index_Type     => Assoc_Ptr,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Generic_Renamings");

   --  Variable to hold enclosing instantiation. When the environment is
   --  saved for a subprogram inlining, the corresponding Act_Id is empty.

   Current_Instantiated_Parent : Assoc := (Empty, Empty, Assoc_Null);

   --  Hash table for associations

   HTable_Size : constant := 37;
   type HTable_Range is range 0 .. HTable_Size - 1;

   procedure Set_Next_Assoc (E : Assoc_Ptr; Next : Assoc_Ptr);
   function  Next_Assoc     (E : Assoc_Ptr) return Assoc_Ptr;
   function Get_Gen_Id      (E : Assoc_Ptr) return Entity_Id;
   function Hash            (F : Entity_Id) return HTable_Range;

   package Generic_Renamings_HTable is new GNAT.HTable.Static_HTable (
      Header_Num => HTable_Range,
      Element    => Assoc,
      Elmt_Ptr   => Assoc_Ptr,
      Null_Ptr   => Assoc_Null,
      Set_Next   => Set_Next_Assoc,
      Next       => Next_Assoc,
      Key        => Entity_Id,
      Get_Key    => Get_Gen_Id,
      Hash       => Hash,
      Equal      => "=");

   Exchanged_Views : Elist_Id;
   --  This list holds the private views that have been exchanged during
   --  instantiation to restore the visibility of the generic declaration.
   --  (see comments above). After instantiation, the current visibility is
   --  reestablished by means of a traversal of this list.

   Hidden_Entities : Elist_Id;
   --  This list holds the entities of the current scope that are removed
   --  from immediate visibility when instantiating a child unit. Their
   --  visibility is restored in Remove_Parent.

   --  Because instantiations can be recursive, the following must be saved
   --  on entry and restored on exit from an instantiation (spec or body).
   --  This is done by the two procedures Save_Env and Restore_Env. For
   --  package and subprogram instantiations (but not for the body instances)
   --  the action of Save_Env is done in two steps: Init_Env is called before
   --  Check_Generic_Child_Unit, because setting the parent instances requires
   --  that the visibility data structures be properly initialized. Once the
   --  generic is unit is validated, Set_Instance_Env completes Save_Env.

   Parent_Unit_Visible : Boolean := False;
   --  Parent_Unit_Visible is used when the generic is a child unit, and
   --  indicates whether the ultimate parent of the generic is visible in the
   --  instantiation environment. It is used to reset the visibility of the
   --  parent at the end of the instantiation (see Remove_Parent).

   Instance_Parent_Unit : Entity_Id := Empty;
   --  This records the ultimate parent unit of an instance of a generic
   --  child unit and is used in conjunction with Parent_Unit_Visible to
   --  indicate the unit to which the Parent_Unit_Visible flag corresponds.

   type Instance_Env is record
      Ada_Version          : Ada_Version_Type;
      Ada_Version_Explicit : Ada_Version_Type;
      Instantiated_Parent  : Assoc;
      Exchanged_Views      : Elist_Id;
      Hidden_Entities      : Elist_Id;
      Current_Sem_Unit     : Unit_Number_Type;
      Parent_Unit_Visible  : Boolean   := False;
      Instance_Parent_Unit : Entity_Id := Empty;
   end record;

   package Instance_Envs is new Table.Table (
     Table_Component_Type => Instance_Env,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 32,
     Table_Increment      => 100,
     Table_Name           => "Instance_Envs");

   procedure Restore_Private_Views
     (Pack_Id    : Entity_Id;
      Is_Package : Boolean := True);
   --  Restore the private views of external types, and unmark the generic
   --  renamings of actuals, so that they become comptible subtypes again.
   --  For subprograms, Pack_Id is the package constructed to hold the
   --  renamings.

   procedure Switch_View (T : Entity_Id);
   --  Switch the partial and full views of a type and its private
   --  dependents (i.e. its subtypes and derived types).

   ------------------------------------
   -- Structures for Error Reporting --
   ------------------------------------

   Instantiation_Node : Node_Id;
   --  Used by subprograms that validate instantiation of formal parameters
   --  where there might be no actual on which to place the error message.
   --  Also used to locate the instantiation node for generic subunits.

   Instantiation_Error : exception;
   --  When there is a semantic error in the generic parameter matching,
   --  there is no point in continuing the instantiation, because the
   --  number of cascaded errors is unpredictable. This exception aborts
   --  the instantiation process altogether.

   S_Adjustment : Sloc_Adjustment;
   --  Offset created for each node in an instantiation, in order to keep
   --  track of the source position of the instantiation in each of its nodes.
   --  A subsequent semantic error or warning on a construct of the instance
   --  points to both places: the original generic node, and the point of
   --  instantiation. See Sinput and Sinput.L for additional details.

   ------------------------------------------------------------
   -- Data structure for keeping track when inside a Generic --
   ------------------------------------------------------------

   --  The following table is used to save values of the Inside_A_Generic
   --  flag (see spec of Sem) when they are saved by Start_Generic.

   package Generic_Flags is new Table.Table (
     Table_Component_Type => Boolean,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 32,
     Table_Increment      => 200,
     Table_Name           => "Generic_Flags");

   ---------------------------
   -- Abandon_Instantiation --
   ---------------------------

   procedure Abandon_Instantiation (N : Node_Id) is
   begin
      Error_Msg_N ("instantiation abandoned!", N);
      raise Instantiation_Error;
   end Abandon_Instantiation;

   --------------------------
   -- Analyze_Associations --
   --------------------------

   function Analyze_Associations
     (I_Node  : Node_Id;
      Formals : List_Id;
      F_Copy  : List_Id) return List_Id
   is
      Actual_Types : constant Elist_Id  := New_Elmt_List;
      Assoc        : constant List_Id   := New_List;
      Defaults     : constant Elist_Id  := New_Elmt_List;
      Gen_Unit     : constant Entity_Id := Defining_Entity (Parent (F_Copy));
      Actuals         : List_Id;
      Actual          : Node_Id;
      Formal          : Node_Id;
      Next_Formal     : Node_Id;
      Temp_Formal     : Node_Id;
      Analyzed_Formal : Node_Id;
      Match           : Node_Id;
      Named           : Node_Id;
      First_Named     : Node_Id := Empty;
      Found_Assoc     : Node_Id;
      Is_Named_Assoc  : Boolean;
      Num_Matched     : Int := 0;
      Num_Actuals     : Int := 0;

      function Matching_Actual
        (F   : Entity_Id;
         A_F : Entity_Id) return Node_Id;
      --  Find actual that corresponds to a given a formal parameter. If the
      --  actuals are positional, return the next one, if any. If the actuals
      --  are named, scan the parameter associations to find the right one.
      --  A_F is the corresponding entity in the analyzed generic,which is
      --  placed on the selector name for ASIS use.

      procedure Set_Analyzed_Formal;
      --  Find the node in the generic copy that corresponds to a given formal.
      --  The semantic information on this node is used to perform legality
      --  checks on the actuals. Because semantic analysis can introduce some
      --  anonymous entities or modify the declaration node itself, the
      --  correspondence between the two lists is not one-one. In addition to
      --  anonymous types, the presence a formal equality will introduce an
      --  implicit declaration for the corresponding inequality.

      ---------------------
      -- Matching_Actual --
      ---------------------

      function Matching_Actual
        (F   : Entity_Id;
         A_F : Entity_Id) return Node_Id
      is
         Found : Node_Id;
         Prev  : Node_Id;

      begin
         Is_Named_Assoc := False;

         --  End of list of purely positional parameters

         if No (Actual) then
            Found := Empty;

         --  Case of positional parameter corresponding to current formal

         elsif No (Selector_Name (Actual)) then
            Found := Explicit_Generic_Actual_Parameter (Actual);
            Found_Assoc := Actual;
            Num_Matched := Num_Matched + 1;
            Next (Actual);

         --  Otherwise scan list of named actuals to find the one with the
         --  desired name. All remaining actuals have explicit names.

         else
            Is_Named_Assoc := True;
            Found := Empty;
            Prev  := Empty;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (F) then
                  Found := Explicit_Generic_Actual_Parameter (Actual);
                  Set_Entity (Selector_Name (Actual), A_F);
                  Set_Etype  (Selector_Name (Actual), Etype (A_F));
                  Generate_Reference (A_F, Selector_Name (Actual));
                  Found_Assoc := Actual;
                  Num_Matched := Num_Matched + 1;
                  exit;
               end if;

               Prev := Actual;
               Next (Actual);
            end loop;

            --  Reset for subsequent searches. In most cases the named
            --  associations are in order. If they are not, we reorder them
            --  to avoid scanning twice the same actual. This is not just a
            --  question of efficiency: there may be multiple defaults with
            --  boxes that have the same name. In a nested instantiation we
            --  insert actuals for those defaults, and cannot rely on their
            --  names to disambiguate them.

            if Actual = First_Named  then
               Next (First_Named);

            elsif Present (Actual) then
               Insert_Before (First_Named, Remove_Next (Prev));
            end if;

            Actual := First_Named;
         end if;

         return Found;
      end Matching_Actual;

      -------------------------
      -- Set_Analyzed_Formal --
      -------------------------

      procedure Set_Analyzed_Formal is
         Kind : Node_Kind;
      begin
         while Present (Analyzed_Formal) loop
            Kind := Nkind (Analyzed_Formal);

            case Nkind (Formal) is

               when N_Formal_Subprogram_Declaration =>
                  exit when Kind in N_Formal_Subprogram_Declaration
                    and then
                      Chars
                        (Defining_Unit_Name (Specification (Formal))) =
                      Chars
                        (Defining_Unit_Name (Specification (Analyzed_Formal)));

               when N_Formal_Package_Declaration =>
                  exit when
                    Kind = N_Formal_Package_Declaration
                      or else
                    Kind = N_Generic_Package_Declaration;

               when N_Use_Package_Clause | N_Use_Type_Clause => exit;

               when others =>

                  --  Skip freeze nodes, and nodes inserted to replace
                  --  unrecognized pragmas.

                  exit when
                    Kind not in N_Formal_Subprogram_Declaration
                      and then Kind /= N_Subprogram_Declaration
                      and then Kind /= N_Freeze_Entity
                      and then Kind /= N_Null_Statement
                      and then Kind /= N_Itype_Reference
                      and then Chars (Defining_Identifier (Formal)) =
                               Chars (Defining_Identifier (Analyzed_Formal));
            end case;

            Next (Analyzed_Formal);
         end loop;

      end Set_Analyzed_Formal;

   --  Start of processing for Analyze_Associations

   begin
      --  If named associations are present, save the first named association
      --  (it may of course be Empty) to facilitate subsequent name search.

      Actuals := Generic_Associations (I_Node);

      if Present (Actuals) then
         First_Named := First (Actuals);

         while Present (First_Named)
           and then No (Selector_Name (First_Named))
         loop
            Num_Actuals := Num_Actuals + 1;
            Next (First_Named);
         end loop;
      end if;

      Named := First_Named;
      while Present (Named) loop
         if No (Selector_Name (Named)) then
            Error_Msg_N ("invalid positional actual after named one", Named);
            Abandon_Instantiation (Named);
         end if;

         --  A named association may lack an actual parameter, if it was
         --  introduced for a default subprogram that turns out to be local
         --  to the outer instantiation.

         if Present (Explicit_Generic_Actual_Parameter (Named)) then
            Num_Actuals := Num_Actuals + 1;
         end if;

         Next (Named);
      end loop;

      if Present (Formals) then
         Formal := First_Non_Pragma (Formals);
         Analyzed_Formal := First_Non_Pragma (F_Copy);

         if Present (Actuals) then
            Actual := First (Actuals);

         --  All formals should have default values

         else
            Actual := Empty;
         end if;

         while Present (Formal) loop
            Set_Analyzed_Formal;
            Next_Formal := Next_Non_Pragma (Formal);

            case Nkind (Formal) is
               when N_Formal_Object_Declaration =>
                  Match :=
                    Matching_Actual (
                      Defining_Identifier (Formal),
                      Defining_Identifier (Analyzed_Formal));

                  Append_List
                    (Instantiate_Object (Formal, Match, Analyzed_Formal),
                     Assoc);

               when N_Formal_Type_Declaration =>
                  Match :=
                    Matching_Actual (
                      Defining_Identifier (Formal),
                      Defining_Identifier (Analyzed_Formal));

                  if No (Match) then
                     Error_Msg_Sloc := Sloc (Gen_Unit);
                     Error_Msg_NE
                       ("missing actual&",
                         Instantiation_Node, Defining_Identifier (Formal));
                     Error_Msg_NE ("\in instantiation of & declared#",
                         Instantiation_Node, Gen_Unit);
                     Abandon_Instantiation (Instantiation_Node);

                  else
                     Analyze (Match);
                     Append_To (Assoc,
                       Instantiate_Type
                         (Formal, Match, Analyzed_Formal, Assoc));

                     --  An instantiation is a freeze point for the actuals,
                     --  unless this is a rewritten formal package.

                     if Nkind (I_Node) /= N_Formal_Package_Declaration then
                        Append_Elmt (Entity (Match), Actual_Types);
                     end if;
                  end if;

                  --  A remote access-to-class-wide type must not be an
                  --  actual parameter for a generic formal of an access
                  --  type (E.2.2 (17)).

                  if Nkind (Analyzed_Formal) = N_Formal_Type_Declaration
                    and then
                      Nkind (Formal_Type_Definition (Analyzed_Formal)) =
                                            N_Access_To_Object_Definition
                  then
                     Validate_Remote_Access_To_Class_Wide_Type (Match);
                  end if;

               when N_Formal_Subprogram_Declaration =>
                  Match :=
                    Matching_Actual (
                      Defining_Unit_Name (Specification (Formal)),
                      Defining_Unit_Name (Specification (Analyzed_Formal)));

                  --  If the formal subprogram has the same name as
                  --  another formal subprogram of the generic, then
                  --  a named association is illegal (12.3(9)). Exclude
                  --  named associations that are generated for a nested
                  --  instance.

                  if Present (Match)
                    and then Is_Named_Assoc
                    and then Comes_From_Source (Found_Assoc)
                  then
                     Temp_Formal := First (Formals);
                     while Present (Temp_Formal) loop
                        if Nkind (Temp_Formal) in
                             N_Formal_Subprogram_Declaration
                          and then Temp_Formal /= Formal
                          and then
                            Chars (Selector_Name (Found_Assoc)) =
                              Chars (Defining_Unit_Name
                                       (Specification (Temp_Formal)))
                        then
                           Error_Msg_N
                             ("name not allowed for overloaded formal",
                              Found_Assoc);
                           Abandon_Instantiation (Instantiation_Node);
                        end if;

                        Next (Temp_Formal);
                     end loop;
                  end if;

                  Append_To (Assoc,
                    Instantiate_Formal_Subprogram
                      (Formal, Match, Analyzed_Formal));

                  if No (Match)
                    and then Box_Present (Formal)
                  then
                     Append_Elmt
                       (Defining_Unit_Name (Specification (Last (Assoc))),
                         Defaults);
                  end if;

               when N_Formal_Package_Declaration =>
                  Match :=
                    Matching_Actual (
                      Defining_Identifier (Formal),
                      Defining_Identifier (Original_Node (Analyzed_Formal)));

                  if No (Match) then
                     Error_Msg_Sloc := Sloc (Gen_Unit);
                     Error_Msg_NE
                       ("missing actual&",
                         Instantiation_Node, Defining_Identifier (Formal));
                     Error_Msg_NE ("\in instantiation of & declared#",
                         Instantiation_Node, Gen_Unit);

                     Abandon_Instantiation (Instantiation_Node);

                  else
                     Analyze (Match);
                     Append_List
                       (Instantiate_Formal_Package
                         (Formal, Match, Analyzed_Formal),
                        Assoc);
                  end if;

               --  For use type and use package appearing in the context
               --  clause, we have already copied them, so we can just
               --  move them where they belong (we mustn't recopy them
               --  since this would mess up the Sloc values).

               when N_Use_Package_Clause |
                    N_Use_Type_Clause    =>
                  Remove (Formal);
                  Append (Formal, Assoc);

               when others =>
                  raise Program_Error;

            end case;

            Formal := Next_Formal;
            Next_Non_Pragma (Analyzed_Formal);
         end loop;

         if Num_Actuals > Num_Matched then
            Error_Msg_Sloc := Sloc (Gen_Unit);

            if Present (Selector_Name (Actual)) then
               Error_Msg_NE
                 ("unmatched actual&",
                    Actual, Selector_Name (Actual));
               Error_Msg_NE ("\in instantiation of& declared#",
                    Actual, Gen_Unit);
            else
               Error_Msg_NE
                 ("unmatched actual in instantiation of& declared#",
                   Actual, Gen_Unit);
            end if;
         end if;

      elsif Present (Actuals) then
         Error_Msg_N
           ("too many actuals in generic instantiation", Instantiation_Node);
      end if;

      declare
         Elmt : Elmt_Id := First_Elmt (Actual_Types);

      begin
         while Present (Elmt) loop
            Freeze_Before (I_Node, Node (Elmt));
            Next_Elmt (Elmt);
         end loop;
      end;

      --  If there are default subprograms, normalize the tree by adding
      --  explicit associations for them. This is required if the instance
      --  appears within a generic.

      declare
         Elmt  : Elmt_Id;
         Subp  : Entity_Id;
         New_D : Node_Id;

      begin
         Elmt := First_Elmt (Defaults);
         while Present (Elmt) loop
            if No (Actuals) then
               Actuals := New_List;
               Set_Generic_Associations (I_Node, Actuals);
            end if;

            Subp := Node (Elmt);
            New_D :=
              Make_Generic_Association (Sloc (Subp),
                Selector_Name => New_Occurrence_Of (Subp, Sloc (Subp)),
                  Explicit_Generic_Actual_Parameter =>
                    New_Occurrence_Of (Subp, Sloc (Subp)));
            Mark_Rewrite_Insertion (New_D);
            Append_To (Actuals, New_D);
            Next_Elmt (Elmt);
         end loop;
      end;

      return Assoc;
   end Analyze_Associations;

   -------------------------------
   -- Analyze_Formal_Array_Type --
   -------------------------------

   procedure Analyze_Formal_Array_Type
     (T   : in out Entity_Id;
      Def : Node_Id)
   is
      DSS : Node_Id;

   begin
      --  Treated like a non-generic array declaration, with
      --  additional semantic checks.

      Enter_Name (T);

      if Nkind (Def) = N_Constrained_Array_Definition then
         DSS := First (Discrete_Subtype_Definitions (Def));
         while Present (DSS) loop
            if Nkind (DSS) = N_Subtype_Indication
              or else Nkind (DSS) = N_Range
              or else Nkind (DSS) = N_Attribute_Reference
            then
               Error_Msg_N ("only a subtype mark is allowed in a formal", DSS);
            end if;

            Next (DSS);
         end loop;
      end if;

      Array_Type_Declaration (T, Def);
      Set_Is_Generic_Type (Base_Type (T));

      if Ekind (Component_Type (T)) = E_Incomplete_Type
        and then No (Full_View (Component_Type (T)))
      then
         Error_Msg_N ("premature usage of incomplete type", Def);

      --  Check that range constraint is not allowed on the component type
      --  of a generic formal array type (AARM 12.5.3(3))

      elsif Is_Internal (Component_Type (T))
        and then Present (Subtype_Indication (Component_Definition (Def)))
        and then Nkind (Original_Node
                        (Subtype_Indication (Component_Definition (Def))))
          = N_Subtype_Indication
      then
         Error_Msg_N
           ("in a formal, a subtype indication can only be "
             & "a subtype mark ('R'M 12.5.3(3))",
             Subtype_Indication (Component_Definition (Def)));
      end if;

   end Analyze_Formal_Array_Type;

   ---------------------------------------------
   -- Analyze_Formal_Decimal_Fixed_Point_Type --
   ---------------------------------------------

   --  As for other generic types, we create a valid type representation
   --  with legal but arbitrary attributes, whose values are never considered
   --  static. For all scalar types we introduce an anonymous base type, with
   --  the same attributes. We choose the corresponding integer type to be
   --  Standard_Integer.

   procedure Analyze_Formal_Decimal_Fixed_Point_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc       : constant Source_Ptr := Sloc (Def);
      Base      : constant Entity_Id :=
                    New_Internal_Entity
                      (E_Decimal_Fixed_Point_Type,
                       Current_Scope, Sloc (Def), 'G');
      Int_Base  : constant Entity_Id := Standard_Integer;
      Delta_Val : constant Ureal := Ureal_1;
      Digs_Val  : constant Uint  := Uint_6;

   begin
      Enter_Name (T);

      Set_Etype          (Base, Base);
      Set_Size_Info      (Base, Int_Base);
      Set_RM_Size        (Base, RM_Size (Int_Base));
      Set_First_Rep_Item (Base, First_Rep_Item (Int_Base));
      Set_Digits_Value   (Base, Digs_Val);
      Set_Delta_Value    (Base, Delta_Val);
      Set_Small_Value    (Base, Delta_Val);
      Set_Scalar_Range   (Base,
        Make_Range (Loc,
          Low_Bound  => Make_Real_Literal (Loc, Ureal_1),
          High_Bound => Make_Real_Literal (Loc, Ureal_1)));

      Set_Is_Generic_Type (Base);
      Set_Parent          (Base, Parent (Def));

      Set_Ekind          (T, E_Decimal_Fixed_Point_Subtype);
      Set_Etype          (T, Base);
      Set_Size_Info      (T, Int_Base);
      Set_RM_Size        (T, RM_Size (Int_Base));
      Set_First_Rep_Item (T, First_Rep_Item (Int_Base));
      Set_Digits_Value   (T, Digs_Val);
      Set_Delta_Value    (T, Delta_Val);
      Set_Small_Value    (T, Delta_Val);
      Set_Scalar_Range   (T, Scalar_Range (Base));
      Set_Is_Constrained (T);

      Check_Restriction (No_Fixed_Point, Def);
   end Analyze_Formal_Decimal_Fixed_Point_Type;

   -------------------------------------------
   -- Analyze_Formal_Derived_Interface_Type --
   -------------------------------------------

   procedure Analyze_Formal_Derived_Interface_Type
     (T : Entity_Id;
      Def : Node_Id)
   is
   begin
      Enter_Name (T);
      Set_Ekind  (T, E_Record_Type);
      Set_Etype  (T, T);
      Analyze (Subtype_Indication (Def));
      Analyze_Interface_Declaration (T, Def);
      Make_Class_Wide_Type (T);
      Set_Primitive_Operations (T, New_Elmt_List);
      Analyze_List (Interface_List (Def));
      Collect_Interfaces (Def, T);
   end Analyze_Formal_Derived_Interface_Type;

   ---------------------------------
   -- Analyze_Formal_Derived_Type --
   ---------------------------------

   procedure Analyze_Formal_Derived_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id)
   is
      Loc      : constant Source_Ptr := Sloc (Def);
      Unk_Disc : constant Boolean    := Unknown_Discriminants_Present (N);
      New_N    : Node_Id;

   begin
      Set_Is_Generic_Type (T);

      if Private_Present (Def) then
         New_N :=
           Make_Private_Extension_Declaration (Loc,
             Defining_Identifier           => T,
             Discriminant_Specifications   => Discriminant_Specifications (N),
             Unknown_Discriminants_Present => Unk_Disc,
             Subtype_Indication            => Subtype_Mark (Def));

         Set_Abstract_Present (New_N, Abstract_Present (Def));

      else
         New_N :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => T,
             Discriminant_Specifications =>
               Discriminant_Specifications (Parent (T)),
              Type_Definition =>
                Make_Derived_Type_Definition (Loc,
                  Subtype_Indication => Subtype_Mark (Def)));

         Set_Abstract_Present
           (Type_Definition (New_N), Abstract_Present (Def));
      end if;

      Rewrite (N, New_N);
      Analyze (N);

      if Unk_Disc then
         if not Is_Composite_Type (T) then
            Error_Msg_N
              ("unknown discriminants not allowed for elementary types", N);
         else
            Set_Has_Unknown_Discriminants (T);
            Set_Is_Constrained (T, False);
         end if;
      end if;

      --  If the parent type has a known size, so does the formal, which
      --  makes legal representation clauses that involve the formal.

      Set_Size_Known_At_Compile_Time
        (T, Size_Known_At_Compile_Time (Entity (Subtype_Mark (Def))));

   end Analyze_Formal_Derived_Type;

   ----------------------------------
   -- Analyze_Formal_Discrete_Type --
   ----------------------------------

   --  The operations defined for a discrete types are those of an
   --  enumeration type. The size is set to an arbitrary value, for use
   --  in analyzing the generic unit.

   procedure Analyze_Formal_Discrete_Type (T : Entity_Id; Def : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Def);
      Lo  : Node_Id;
      Hi  : Node_Id;

      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Floating_Point_Type, Current_Scope, Sloc (Def), 'G');
   begin
      Enter_Name          (T);
      Set_Ekind           (T, E_Enumeration_Subtype);
      Set_Etype           (T, Base);
      Init_Size           (T, 8);
      Init_Alignment      (T);
      Set_Is_Generic_Type (T);
      Set_Is_Constrained  (T);

      --  For semantic analysis, the bounds of the type must be set to some
      --  non-static value. The simplest is to create attribute nodes for
      --  those bounds, that refer to the type itself. These bounds are never
      --  analyzed but serve as place-holders.

      Lo :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_First,
          Prefix => New_Reference_To (T, Loc));
      Set_Etype (Lo, T);

      Hi :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_Last,
          Prefix => New_Reference_To (T, Loc));
      Set_Etype (Hi, T);

      Set_Scalar_Range (T,
        Make_Range (Loc,
          Low_Bound => Lo,
          High_Bound => Hi));

      Set_Ekind           (Base, E_Enumeration_Type);
      Set_Etype           (Base, Base);
      Init_Size           (Base, 8);
      Init_Alignment      (Base);
      Set_Is_Generic_Type (Base);
      Set_Scalar_Range    (Base, Scalar_Range (T));
      Set_Parent          (Base, Parent (Def));

   end Analyze_Formal_Discrete_Type;

   ----------------------------------
   -- Analyze_Formal_Floating_Type --
   ---------------------------------

   procedure Analyze_Formal_Floating_Type (T : Entity_Id; Def : Node_Id) is
      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Floating_Point_Type, Current_Scope, Sloc (Def), 'G');

   begin
      --  The various semantic attributes are taken from the predefined type
      --  Float, just so that all of them are initialized. Their values are
      --  never used because no constant folding or expansion takes place in
      --  the generic itself.

      Enter_Name (T);
      Set_Ekind          (T, E_Floating_Point_Subtype);
      Set_Etype          (T, Base);
      Set_Size_Info      (T,              (Standard_Float));
      Set_RM_Size        (T, RM_Size      (Standard_Float));
      Set_Digits_Value   (T, Digits_Value (Standard_Float));
      Set_Scalar_Range   (T, Scalar_Range (Standard_Float));
      Set_Is_Constrained (T);

      Set_Is_Generic_Type (Base);
      Set_Etype           (Base, Base);
      Set_Size_Info       (Base,              (Standard_Float));
      Set_RM_Size         (Base, RM_Size      (Standard_Float));
      Set_Digits_Value    (Base, Digits_Value (Standard_Float));
      Set_Scalar_Range    (Base, Scalar_Range (Standard_Float));
      Set_Parent          (Base, Parent (Def));

      Check_Restriction (No_Floating_Point, Def);
   end Analyze_Formal_Floating_Type;

   -----------------------------------
   -- Analyze_Formal_Interface_Type;--
   -----------------------------------

   procedure Analyze_Formal_Interface_Type (T : Entity_Id; Def : Node_Id) is
   begin
      Enter_Name (T);
      Set_Ekind  (T, E_Record_Type);
      Set_Etype  (T, T);
      Analyze_Interface_Declaration (T, Def);
      Make_Class_Wide_Type (T);
      Set_Primitive_Operations (T, New_Elmt_List);
   end Analyze_Formal_Interface_Type;

   ---------------------------------
   -- Analyze_Formal_Modular_Type --
   ---------------------------------

   procedure Analyze_Formal_Modular_Type (T : Entity_Id; Def : Node_Id) is
   begin
      --  Apart from their entity kind, generic modular types are treated
      --  like signed integer types, and have the same attributes.

      Analyze_Formal_Signed_Integer_Type (T, Def);
      Set_Ekind (T, E_Modular_Integer_Subtype);
      Set_Ekind (Etype (T), E_Modular_Integer_Type);

   end Analyze_Formal_Modular_Type;

   ---------------------------------------
   -- Analyze_Formal_Object_Declaration --
   ---------------------------------------

   procedure Analyze_Formal_Object_Declaration (N : Node_Id) is
      E  : constant Node_Id := Expression (N);
      Id : constant Node_Id := Defining_Identifier (N);
      K  : Entity_Kind;
      T  : Node_Id;

   begin
      Enter_Name (Id);

      --  Determine the mode of the formal object

      if Out_Present (N) then
         K := E_Generic_In_Out_Parameter;

         if not In_Present (N) then
            Error_Msg_N ("formal generic objects cannot have mode OUT", N);
         end if;

      else
         K := E_Generic_In_Parameter;
      end if;

      Find_Type (Subtype_Mark (N));
      T  := Entity (Subtype_Mark (N));

      if Ekind (T) = E_Incomplete_Type then
         Error_Msg_N ("premature usage of incomplete type", Subtype_Mark (N));
      end if;

      if K = E_Generic_In_Parameter then

         --  Ada 2005 (AI-287): Limited aggregates allowed in generic formals

         if Ada_Version < Ada_05 and then Is_Limited_Type (T) then
            Error_Msg_N
              ("generic formal of mode IN must not be of limited type", N);
            Explain_Limited_Type (T, N);
         end if;

         if Is_Abstract (T) then
            Error_Msg_N
              ("generic formal of mode IN must not be of abstract type", N);
         end if;

         if Present (E) then
            Analyze_Per_Use_Expression (E, T);
         end if;

         Set_Ekind (Id, K);
         Set_Etype (Id, T);

      --  Case of generic IN OUT parameter

      else
         --  If the formal has an unconstrained type, construct its
         --  actual subtype, as is done for subprogram formals. In this
         --  fashion, all its uses can refer to specific bounds.

         Set_Ekind (Id, K);
         Set_Etype (Id, T);

         if (Is_Array_Type (T)
              and then not Is_Constrained (T))
           or else
            (Ekind (T) = E_Record_Type
              and then Has_Discriminants (T))
         then
            declare
               Non_Freezing_Ref : constant Node_Id :=
                                    New_Reference_To (Id, Sloc (Id));
               Decl : Node_Id;

            begin
               --  Make sure that the actual subtype doesn't generate
               --  bogus freezing.

               Set_Must_Not_Freeze (Non_Freezing_Ref);
               Decl := Build_Actual_Subtype (T, Non_Freezing_Ref);
               Insert_Before_And_Analyze (N, Decl);
               Set_Actual_Subtype (Id, Defining_Identifier (Decl));
            end;
         else
            Set_Actual_Subtype (Id, T);
         end if;

         if Present (E) then
            Error_Msg_N
              ("initialization not allowed for `IN OUT` formals", N);
         end if;
      end if;

   end Analyze_Formal_Object_Declaration;

   ----------------------------------------------
   -- Analyze_Formal_Ordinary_Fixed_Point_Type --
   ----------------------------------------------

   procedure Analyze_Formal_Ordinary_Fixed_Point_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Def);
      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Ordinary_Fixed_Point_Type, Current_Scope, Sloc (Def), 'G');
   begin
      --  The semantic attributes are set for completeness only, their
      --  values will never be used, because all properties of the type
      --  are non-static.

      Enter_Name (T);
      Set_Ekind            (T, E_Ordinary_Fixed_Point_Subtype);
      Set_Etype            (T, Base);
      Set_Size_Info        (T, Standard_Integer);
      Set_RM_Size          (T, RM_Size (Standard_Integer));
      Set_Small_Value      (T, Ureal_1);
      Set_Delta_Value      (T, Ureal_1);
      Set_Scalar_Range     (T,
        Make_Range (Loc,
          Low_Bound  => Make_Real_Literal (Loc, Ureal_1),
          High_Bound => Make_Real_Literal (Loc, Ureal_1)));
      Set_Is_Constrained   (T);

      Set_Is_Generic_Type (Base);
      Set_Etype           (Base, Base);
      Set_Size_Info       (Base, Standard_Integer);
      Set_RM_Size         (Base, RM_Size (Standard_Integer));
      Set_Small_Value     (Base, Ureal_1);
      Set_Delta_Value     (Base, Ureal_1);
      Set_Scalar_Range    (Base, Scalar_Range (T));
      Set_Parent          (Base, Parent (Def));

      Check_Restriction (No_Fixed_Point, Def);
   end Analyze_Formal_Ordinary_Fixed_Point_Type;

   ----------------------------
   -- Analyze_Formal_Package --
   ----------------------------

   procedure Analyze_Formal_Package (N : Node_Id) is
      Loc              : constant Source_Ptr := Sloc (N);
      Pack_Id          : constant Entity_Id := Defining_Identifier (N);
      Formal           : Entity_Id;
      Gen_Id           : constant Node_Id    := Name (N);
      Gen_Decl         : Node_Id;
      Gen_Unit         : Entity_Id;
      New_N            : Node_Id;
      Parent_Installed : Boolean := False;
      Renaming         : Node_Id;
      Parent_Instance  : Entity_Id;
      Renaming_In_Par  : Entity_Id;

   begin
      Text_IO_Kludge (Gen_Id);

      Init_Env;
      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      --  Check for a formal package that is a package renaming

      if Present (Renamed_Object (Gen_Unit)) then
         Gen_Unit := Renamed_Object (Gen_Unit);
      end if;

      if Ekind (Gen_Unit) /= E_Generic_Package then
         Error_Msg_N ("expect generic package name", Gen_Id);
         Restore_Env;
         return;

      elsif  Gen_Unit = Current_Scope then
         Error_Msg_N
           ("generic package cannot be used as a formal package of itself",
             Gen_Id);
         Restore_Env;
         return;

      elsif In_Open_Scopes (Gen_Unit) then
         if Is_Compilation_Unit (Gen_Unit)
           and then Is_Child_Unit (Current_Scope)
         then
            --  Special-case the error when the formal is a parent, and
            --  continue analysis to minimize cascaded errors.

            Error_Msg_N
              ("generic parent cannot be used as formal package "
                & "of a child unit",
                Gen_Id);

         else
            Error_Msg_N
              ("generic package cannot be used as a formal package "
                & "within itself",
                Gen_Id);
            Restore_Env;
            return;
         end if;
      end if;

      --  The formal package is treated like a regular instance, but only
      --  the specification needs to be instantiated, to make entities visible.

      if not Box_Present (N) then
         Hidden_Entities := New_Elmt_List;
         Analyze_Package_Instantiation (N);

         if Parent_Installed then
            Remove_Parent;
         end if;

      else
         --  If there are no generic associations, the generic parameters
         --  appear as local entities and are instantiated like them. We copy
         --  the generic package declaration as if it were an instantiation,
         --  and analyze it like a regular package, except that we treat the
         --  formals as additional visible components.

         Gen_Decl := Unit_Declaration_Node (Gen_Unit);

         if In_Extended_Main_Source_Unit (N) then
            Set_Is_Instantiated (Gen_Unit);
            Generate_Reference  (Gen_Unit, N);
         end if;

         Formal := New_Copy (Pack_Id);
         Create_Instantiation_Source (N, Gen_Unit, False, S_Adjustment);

         New_N :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);
         Rewrite (N, New_N);
         Set_Defining_Unit_Name (Specification (New_N), Formal);
         Set_Generic_Parent (Specification (N), Gen_Unit);
         Set_Instance_Env (Gen_Unit, Formal);

         Enter_Name (Formal);
         Set_Ekind  (Formal, E_Generic_Package);
         Set_Etype  (Formal, Standard_Void_Type);
         Set_Inner_Instances (Formal, New_Elmt_List);
         New_Scope  (Formal);

         --  Within the formal, the name of the generic package is a renaming
         --  of the formal (as for a regular instantiation).

         Renaming := Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc, Chars (Gen_Unit)),
             Name => New_Reference_To (Formal, Loc));

         if Present (Visible_Declarations (Specification (N))) then
            Prepend (Renaming, To => Visible_Declarations (Specification (N)));
         elsif Present (Private_Declarations (Specification (N))) then
            Prepend (Renaming, To => Private_Declarations (Specification (N)));
         end if;

         if Is_Child_Unit (Gen_Unit)
           and then Parent_Installed
         then
            --  Similarly, we have to make the name of the formal visible in
            --  the parent instance, to resolve properly fully qualified names
            --  that may appear in the generic unit. The parent instance has
            --  been placed on the scope stack ahead of the current scope.

            Parent_Instance := Scope_Stack.Table (Scope_Stack.Last - 1).Entity;

            Renaming_In_Par :=
              Make_Defining_Identifier (Loc, Chars (Gen_Unit));
            Set_Ekind (Renaming_In_Par, E_Package);
            Set_Etype (Renaming_In_Par, Standard_Void_Type);
            Set_Scope (Renaming_In_Par, Parent_Instance);
            Set_Parent (Renaming_In_Par, Parent (Formal));
            Set_Renamed_Object (Renaming_In_Par, Formal);
            Append_Entity (Renaming_In_Par, Parent_Instance);
         end if;

         Analyze_Generic_Formal_Part (N);
         Analyze (Specification (N));
         End_Package_Scope (Formal);

         if Parent_Installed then
            Remove_Parent;
         end if;

         Restore_Env;

         --  Inside the generic unit, the formal package is a regular
         --  package, but no body is needed for it. Note that after
         --  instantiation, the defining_unit_name we need is in the
         --  new tree and not in the original. (see Package_Instantiation).
         --  A generic formal package is an instance, and can be used as
         --  an actual for an inner instance.

         Set_Ekind (Formal, E_Package);
         Set_Has_Completion (Formal, True);

         Set_Ekind (Pack_Id, E_Package);
         Set_Etype (Pack_Id, Standard_Void_Type);
         Set_Scope (Pack_Id, Scope (Formal));
         Set_Has_Completion (Pack_Id, True);
      end if;
   end Analyze_Formal_Package;

   ---------------------------------
   -- Analyze_Formal_Private_Type --
   ---------------------------------

   procedure Analyze_Formal_Private_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id)
   is
   begin
      New_Private_Type (N, T, Def);

      --  Set the size to an arbitrary but legal value

      Set_Size_Info (T, Standard_Integer);
      Set_RM_Size   (T, RM_Size (Standard_Integer));
   end Analyze_Formal_Private_Type;

   ----------------------------------------
   -- Analyze_Formal_Signed_Integer_Type --
   ----------------------------------------

   procedure Analyze_Formal_Signed_Integer_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Signed_Integer_Type, Current_Scope, Sloc (Def), 'G');

   begin
      Enter_Name (T);

      Set_Ekind          (T, E_Signed_Integer_Subtype);
      Set_Etype          (T, Base);
      Set_Size_Info      (T, Standard_Integer);
      Set_RM_Size        (T, RM_Size (Standard_Integer));
      Set_Scalar_Range   (T, Scalar_Range (Standard_Integer));
      Set_Is_Constrained (T);

      Set_Is_Generic_Type (Base);
      Set_Size_Info       (Base, Standard_Integer);
      Set_RM_Size         (Base, RM_Size (Standard_Integer));
      Set_Etype           (Base, Base);
      Set_Scalar_Range    (Base, Scalar_Range (Standard_Integer));
      Set_Parent          (Base, Parent (Def));
   end Analyze_Formal_Signed_Integer_Type;

   -------------------------------
   -- Analyze_Formal_Subprogram --
   -------------------------------

   procedure Analyze_Formal_Subprogram (N : Node_Id) is
      Spec : constant Node_Id   := Specification (N);
      Def  : constant Node_Id   := Default_Name (N);
      Nam  : constant Entity_Id := Defining_Unit_Name (Spec);
      Subp : Entity_Id;

   begin
      if Nam = Error then
         return;
      end if;

      if Nkind (Nam) = N_Defining_Program_Unit_Name then
         Error_Msg_N ("name of formal subprogram must be a direct name", Nam);
         return;
      end if;

      Analyze_Subprogram_Declaration (N);
      Set_Is_Formal_Subprogram (Nam);
      Set_Has_Completion (Nam);

      if Nkind (N) = N_Formal_Abstract_Subprogram_Declaration then
         Set_Is_Abstract (Nam);
         Set_Is_Dispatching_Operation (Nam);

         declare
            Ctrl_Type : constant Entity_Id := Find_Dispatching_Type (Nam);

         begin
            if not Present (Ctrl_Type) then
               Error_Msg_N
                 ("abstract formal subprogram must have a controlling type",
                  N);

            else
               Check_Controlling_Formals (Ctrl_Type, Nam);
            end if;
         end;
      end if;

      --  Default name is resolved at the point of instantiation

      if Box_Present (N) then
         null;

      --  Else default is bound at the point of generic declaration

      elsif Present (Def) then
         if Nkind (Def) = N_Operator_Symbol then
            Find_Direct_Name (Def);

         elsif Nkind (Def) /= N_Attribute_Reference then
            Analyze (Def);

         else
            --  For an attribute reference, analyze the prefix and verify
            --  that it has the proper profile for the subprogram.

            Analyze (Prefix (Def));
            Valid_Default_Attribute (Nam, Def);
            return;
         end if;

         --  Default name may be overloaded, in which case the interpretation
         --  with the correct profile must be  selected, as for a renaming.

         if Etype (Def) = Any_Type then
            return;

         elsif Nkind (Def) = N_Selected_Component then
            Subp := Entity (Selector_Name (Def));

            if Ekind (Subp) /= E_Entry then
               Error_Msg_N ("expect valid subprogram name as default", Def);
               return;
            end if;

         elsif Nkind (Def) = N_Indexed_Component then

            if  Nkind (Prefix (Def)) /= N_Selected_Component then
               Error_Msg_N ("expect valid subprogram name as default", Def);
               return;

            else
               Subp := Entity (Selector_Name (Prefix (Def)));

               if Ekind (Subp) /= E_Entry_Family then
                  Error_Msg_N ("expect valid subprogram name as default", Def);
                  return;
               end if;
            end if;

         elsif Nkind (Def) = N_Character_Literal then

            --  Needs some type checks: subprogram should be parameterless???

            Resolve (Def, (Etype (Nam)));

         elsif not Is_Entity_Name (Def)
           or else not Is_Overloadable (Entity (Def))
         then
            Error_Msg_N ("expect valid subprogram name as default", Def);
            return;

         elsif not Is_Overloaded (Def) then
            Subp := Entity (Def);

            if Subp = Nam then
               Error_Msg_N ("premature usage of formal subprogram", Def);

            elsif not Entity_Matches_Spec (Subp, Nam) then
               Error_Msg_N ("no visible entity matches specification", Def);
            end if;

         else
            declare
               I   : Interp_Index;
               I1  : Interp_Index := 0;
               It  : Interp;
               It1 : Interp;

            begin
               Subp := Any_Id;
               Get_First_Interp (Def, I, It);
               while Present (It.Nam) loop

                  if Entity_Matches_Spec (It.Nam, Nam) then
                     if Subp /= Any_Id then
                        It1 := Disambiguate (Def, I1, I, Etype (Subp));

                        if It1 = No_Interp then
                           Error_Msg_N ("ambiguous default subprogram", Def);
                        else
                           Subp := It1.Nam;
                        end if;

                        exit;

                     else
                        I1  := I;
                        Subp := It.Nam;
                     end if;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end;

            if Subp /= Any_Id then
               Set_Entity (Def, Subp);

               if Subp = Nam then
                  Error_Msg_N ("premature usage of formal subprogram", Def);

               elsif Ekind (Subp) /= E_Operator then
                  Check_Mode_Conformant (Subp, Nam);
               end if;

            else
               Error_Msg_N ("no visible subprogram matches specification", N);
            end if;
         end if;
      end if;
   end Analyze_Formal_Subprogram;

   -------------------------------------
   -- Analyze_Formal_Type_Declaration --
   -------------------------------------

   procedure Analyze_Formal_Type_Declaration (N : Node_Id) is
      Def : constant Node_Id := Formal_Type_Definition (N);
      T   : Entity_Id;

   begin
      T := Defining_Identifier (N);

      if Present (Discriminant_Specifications (N))
        and then Nkind (Def) /= N_Formal_Private_Type_Definition
      then
         Error_Msg_N
           ("discriminants not allowed for this formal type",
            Defining_Identifier (First (Discriminant_Specifications (N))));
      end if;

      --  Enter the new name, and branch to specific routine

      case Nkind (Def) is
         when N_Formal_Private_Type_Definition         =>
            Analyze_Formal_Private_Type (N, T, Def);

         when N_Formal_Derived_Type_Definition         =>
            Analyze_Formal_Derived_Type (N, T, Def);

         when N_Formal_Discrete_Type_Definition        =>
            Analyze_Formal_Discrete_Type (T, Def);

         when N_Formal_Signed_Integer_Type_Definition  =>
            Analyze_Formal_Signed_Integer_Type (T, Def);

         when N_Formal_Modular_Type_Definition         =>
            Analyze_Formal_Modular_Type (T, Def);

         when N_Formal_Floating_Point_Definition       =>
            Analyze_Formal_Floating_Type (T, Def);

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            Analyze_Formal_Ordinary_Fixed_Point_Type (T, Def);

         when N_Formal_Decimal_Fixed_Point_Definition  =>
            Analyze_Formal_Decimal_Fixed_Point_Type (T, Def);

         when N_Array_Type_Definition =>
            Analyze_Formal_Array_Type (T, Def);

         when N_Access_To_Object_Definition            |
              N_Access_Function_Definition             |
              N_Access_Procedure_Definition            =>
            Analyze_Generic_Access_Type (T, Def);

         --  Ada 2005: a interface declaration is encoded as an abstract
         --  record declaration or a abstract type derivation.

         when N_Record_Definition                      =>
            Analyze_Formal_Interface_Type (T, Def);

         when N_Derived_Type_Definition                =>
            Analyze_Formal_Derived_Interface_Type (T, Def);

         when N_Error                                  =>
            null;

         when others                                   =>
            raise Program_Error;

      end case;

      Set_Is_Generic_Type (T);
   end Analyze_Formal_Type_Declaration;

   ------------------------------------
   -- Analyze_Function_Instantiation --
   ------------------------------------

   procedure Analyze_Function_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Function);
   end Analyze_Function_Instantiation;

   ---------------------------------
   -- Analyze_Generic_Access_Type --
   ---------------------------------

   procedure Analyze_Generic_Access_Type (T : Entity_Id; Def : Node_Id) is
   begin
      Enter_Name (T);

      if Nkind (Def) = N_Access_To_Object_Definition then
         Access_Type_Declaration (T, Def);

         if Is_Incomplete_Or_Private_Type (Designated_Type (T))
           and then No (Full_View (Designated_Type (T)))
           and then not Is_Generic_Type (Designated_Type (T))
         then
            Error_Msg_N ("premature usage of incomplete type", Def);

         elsif Is_Internal (Designated_Type (T)) then
            Error_Msg_N
              ("only a subtype mark is allowed in a formal", Def);
         end if;

      else
         Access_Subprogram_Declaration (T, Def);
      end if;
   end Analyze_Generic_Access_Type;

   ---------------------------------
   -- Analyze_Generic_Formal_Part --
   ---------------------------------

   procedure Analyze_Generic_Formal_Part (N : Node_Id) is
      Gen_Parm_Decl : Node_Id;

   begin
      --  The generic formals are processed in the scope of the generic
      --  unit, where they are immediately visible. The scope is installed
      --  by the caller.

      Gen_Parm_Decl := First (Generic_Formal_Declarations (N));

      while Present (Gen_Parm_Decl) loop
         Analyze (Gen_Parm_Decl);
         Next (Gen_Parm_Decl);
      end loop;

      Generate_Reference_To_Generic_Formals (Current_Scope);
   end Analyze_Generic_Formal_Part;

   ------------------------------------------
   -- Analyze_Generic_Package_Declaration  --
   ------------------------------------------

   procedure Analyze_Generic_Package_Declaration (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Id          : Entity_Id;
      New_N       : Node_Id;
      Save_Parent : Node_Id;
      Renaming    : Node_Id;
      Decls       : constant List_Id :=
                      Visible_Declarations (Specification (N));
      Decl        : Node_Id;

   begin
      --  We introduce a renaming of the enclosing package, to have a usable
      --  entity as the prefix of an expanded name for a local entity of the
      --  form Par.P.Q, where P is the generic package. This is because a local
      --  entity named P may hide it, so that the usual visibility rules in
      --  the instance will not resolve properly.

      Renaming :=
        Make_Package_Renaming_Declaration (Loc,
          Defining_Unit_Name =>
            Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Defining_Entity (N)), "GH")),
          Name => Make_Identifier (Loc, Chars (Defining_Entity (N))));

      if Present (Decls) then
         Decl := First (Decls);
         while Present (Decl)
           and then Nkind (Decl) = N_Pragma
         loop
            Next (Decl);
         end loop;

         if Present (Decl) then
            Insert_Before (Decl, Renaming);
         else
            Append (Renaming, Visible_Declarations (Specification (N)));
         end if;

      else
         Set_Visible_Declarations (Specification (N), New_List (Renaming));
      end if;

      --  Create copy of generic unit, and save for instantiation.
      --  If the unit is a child unit, do not copy the specifications
      --  for the parent, which are not part of the generic tree.

      Save_Parent := Parent_Spec (N);
      Set_Parent_Spec (N, Empty);

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Set_Parent_Spec (New_N, Save_Parent);
      Rewrite (N, New_N);
      Id := Defining_Entity (N);
      Generate_Definition (Id);

      --  Expansion is not applied to generic units

      Start_Generic;

      Enter_Name (Id);
      Set_Ekind (Id, E_Generic_Package);
      Set_Etype (Id, Standard_Void_Type);
      New_Scope (Id);
      Enter_Generic_Scope (Id);
      Set_Inner_Instances (Id, New_Elmt_List);

      Set_Categorization_From_Pragmas (N);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      --  Link the declaration of the generic homonym in the generic copy
      --  to the package it renames, so that it is always resolved properly.

      Set_Generic_Homonym (Id, Defining_Unit_Name (Renaming));
      Set_Entity (Associated_Node (Name (Renaming)), Id);

      --  For a library unit, we have reconstructed the entity for the
      --  unit, and must reset it in the library tables.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Cunit_Entity (Current_Sem_Unit, Id);
      end if;

      Analyze_Generic_Formal_Part (N);

      --  After processing the generic formals, analysis proceeds
      --  as for a non-generic package.

      Analyze (Specification (N));

      Validate_Categorization_Dependency (N, Id);

      End_Generic;

      End_Package_Scope (Id);
      Exit_Generic_Scope (Id);

      if Nkind (Parent (N)) /= N_Compilation_Unit then
         Move_Freeze_Nodes (Id, N, Visible_Declarations (Specification (N)));
         Move_Freeze_Nodes (Id, N, Private_Declarations (Specification (N)));
         Move_Freeze_Nodes (Id, N, Generic_Formal_Declarations (N));

      else
         Set_Body_Required (Parent (N), Unit_Requires_Body (Id));
         Validate_RT_RAT_Component (N);

         --  If this is a spec without a body, check that generic parameters
         --  are referenced.

         if not Body_Required (Parent (N)) then
            Check_References (Id);
         end if;
      end if;
   end Analyze_Generic_Package_Declaration;

   --------------------------------------------
   -- Analyze_Generic_Subprogram_Declaration --
   --------------------------------------------

   procedure Analyze_Generic_Subprogram_Declaration (N : Node_Id) is
      Spec        : Node_Id;
      Id          : Entity_Id;
      Formals     : List_Id;
      New_N       : Node_Id;
      Result_Type : Entity_Id;
      Save_Parent : Node_Id;

   begin
      --  Create copy of generic unit,and save for instantiation.
      --  If the unit is a child unit, do not copy the specifications
      --  for the parent, which are not part of the generic tree.

      Save_Parent := Parent_Spec (N);
      Set_Parent_Spec (N, Empty);

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Set_Parent_Spec (New_N, Save_Parent);
      Rewrite (N, New_N);

      Spec := Specification (N);
      Id := Defining_Entity (Spec);
      Generate_Definition (Id);

      if Nkind (Id) = N_Defining_Operator_Symbol then
         Error_Msg_N
           ("operator symbol not allowed for generic subprogram", Id);
      end if;

      Start_Generic;

      Enter_Name (Id);

      Set_Scope_Depth_Value (Id, Scope_Depth (Current_Scope) + 1);
      New_Scope (Id);
      Enter_Generic_Scope (Id);
      Set_Inner_Instances (Id, New_Elmt_List);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      Analyze_Generic_Formal_Part (N);

      Formals := Parameter_Specifications (Spec);

      if Present (Formals) then
         Process_Formals (Formals, Spec);
      end if;

      if Nkind (Spec) = N_Function_Specification then
         Set_Ekind (Id, E_Generic_Function);

         if Nkind (Result_Definition (Spec)) = N_Access_Definition then
            Result_Type := Access_Definition (Spec, Result_Definition (Spec));
            Set_Etype (Id, Result_Type);
         else
            Find_Type (Result_Definition (Spec));
            Set_Etype (Id, Entity (Result_Definition (Spec)));
         end if;

      else
         Set_Ekind (Id, E_Generic_Procedure);
         Set_Etype (Id, Standard_Void_Type);
      end if;

      --  For a library unit, we have reconstructed the entity for the unit,
      --  and must reset it in the library tables. We also make sure that
      --  Body_Required is set properly in the original compilation unit node.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Cunit_Entity (Current_Sem_Unit, Id);
         Set_Body_Required (Parent (N), Unit_Requires_Body (Id));
      end if;

      Set_Categorization_From_Pragmas (N);
      Validate_Categorization_Dependency (N, Id);

      Save_Global_References (Original_Node (N));

      End_Generic;
      End_Scope;
      Exit_Generic_Scope (Id);
      Generate_Reference_To_Formals (Id);
   end Analyze_Generic_Subprogram_Declaration;

   -----------------------------------
   -- Analyze_Package_Instantiation --
   -----------------------------------

   --  Note: this procedure is also used for formal package declarations, in
   --  which case the argument N is an N_Formal_Package_Declaration node.
   --  This should really be noted in the spec! ???

   procedure Analyze_Package_Instantiation (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Gen_Id : constant Node_Id    := Name (N);

      Act_Decl      : Node_Id;
      Act_Decl_Name : Node_Id;
      Act_Decl_Id   : Entity_Id;
      Act_Spec      : Node_Id;
      Act_Tree      : Node_Id;

      Gen_Decl : Node_Id;
      Gen_Unit : Entity_Id;

      Is_Actual_Pack : constant Boolean :=
                         Is_Internal (Defining_Entity (N));

      Env_Installed    : Boolean := False;
      Parent_Installed : Boolean := False;
      Renaming_List    : List_Id;
      Unit_Renaming    : Node_Id;
      Needs_Body       : Boolean;
      Inline_Now       : Boolean := False;

      procedure Delay_Descriptors (E : Entity_Id);
      --  Delay generation of subprogram descriptors for given entity

      function Might_Inline_Subp return Boolean;
      --  If inlining is active and the generic contains inlined subprograms,
      --  we instantiate the body. This may cause superfluous instantiations,
      --  but it is simpler than detecting the need for the body at the point
      --  of inlining, when the context of the instance is not available.

      -----------------------
      -- Delay_Descriptors --
      -----------------------

      procedure Delay_Descriptors (E : Entity_Id) is
      begin
         if not Delay_Subprogram_Descriptors (E) then
            Set_Delay_Subprogram_Descriptors (E);
            Pending_Descriptor.Increment_Last;
            Pending_Descriptor.Table (Pending_Descriptor.Last) := E;
         end if;
      end Delay_Descriptors;

      -----------------------
      -- Might_Inline_Subp --
      -----------------------

      function Might_Inline_Subp return Boolean is
         E : Entity_Id;

      begin
         if not Inline_Processing_Required then
            return False;

         else
            E := First_Entity (Gen_Unit);
            while Present (E) loop
               if Is_Subprogram (E)
                 and then Is_Inlined (E)
               then
                  return True;
               end if;

               Next_Entity (E);
            end loop;
         end if;

         return False;
      end Might_Inline_Subp;

   --  Start of processing for Analyze_Package_Instantiation

   begin
      --  Very first thing: apply the special kludge for Text_IO processing
      --  in case we are instantiating one of the children of [Wide_]Text_IO.

      Text_IO_Kludge (Name (N));

      --  Make node global for error reporting

      Instantiation_Node := N;

      --  Case of instantiation of a generic package

      if Nkind (N) = N_Package_Instantiation then
         Act_Decl_Id := New_Copy (Defining_Entity (N));
         Set_Comes_From_Source (Act_Decl_Id, True);

         if Nkind (Defining_Unit_Name (N)) = N_Defining_Program_Unit_Name then
            Act_Decl_Name :=
              Make_Defining_Program_Unit_Name (Loc,
                Name => New_Copy_Tree (Name (Defining_Unit_Name (N))),
                Defining_Identifier => Act_Decl_Id);
         else
            Act_Decl_Name :=  Act_Decl_Id;
         end if;

      --  Case of instantiation of a formal package

      else
         Act_Decl_Id   := Defining_Identifier (N);
         Act_Decl_Name := Act_Decl_Id;
      end if;

      Generate_Definition (Act_Decl_Id);
      Pre_Analyze_Actuals (N);

      Init_Env;
      Env_Installed := True;
      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      --  Verify that it is the name of a generic package

      if Etype (Gen_Unit) = Any_Type then
         Restore_Env;
         return;

      elsif Ekind (Gen_Unit) /= E_Generic_Package then

         --  Ada 2005 (AI-50217): Cannot use instance in limited with_clause

         if From_With_Type (Gen_Unit) then
            Error_Msg_N
              ("cannot instantiate a limited withed package", Gen_Id);
         else
            Error_Msg_N
              ("expect name of generic package in instantiation", Gen_Id);
         end if;

         Restore_Env;
         return;
      end if;

      if In_Extended_Main_Source_Unit (N) then
         Set_Is_Instantiated (Gen_Unit);
         Generate_Reference  (Gen_Unit, N);

         if Present (Renamed_Object (Gen_Unit)) then
            Set_Is_Instantiated (Renamed_Object (Gen_Unit));
            Generate_Reference  (Renamed_Object (Gen_Unit), N);
         end if;
      end if;

      if Nkind (Gen_Id) = N_Identifier
        and then Chars (Gen_Unit) = Chars (Defining_Entity (N))
      then
         Error_Msg_NE
           ("& is hidden within declaration of instance", Gen_Id, Gen_Unit);

      elsif Nkind (Gen_Id) = N_Expanded_Name
        and then Is_Child_Unit (Gen_Unit)
        and then Nkind (Prefix (Gen_Id)) = N_Identifier
        and then Chars (Act_Decl_Id) = Chars (Prefix (Gen_Id))
      then
         Error_Msg_N
           ("& is hidden within declaration of instance ", Prefix (Gen_Id));
      end if;

      Set_Entity (Gen_Id, Gen_Unit);

      --  If generic is a renaming, get original generic unit

      if Present (Renamed_Object (Gen_Unit))
        and then Ekind (Renamed_Object (Gen_Unit)) = E_Generic_Package
      then
         Gen_Unit := Renamed_Object (Gen_Unit);
      end if;

      --  Verify that there are no circular instantiations

      if In_Open_Scopes (Gen_Unit) then
         Error_Msg_NE ("instantiation of & within itself", N, Gen_Unit);
         Restore_Env;
         return;

      elsif Contains_Instance_Of (Gen_Unit, Current_Scope, Gen_Id) then
         Error_Msg_Node_2 := Current_Scope;
         Error_Msg_NE
           ("circular Instantiation: & instantiated in &!", N, Gen_Unit);
         Circularity_Detected := True;
         Restore_Env;
         return;

      else
         Set_Instance_Env (Gen_Unit, Act_Decl_Id);
         Gen_Decl := Unit_Declaration_Node (Gen_Unit);

         --  Initialize renamings map, for error checking, and the list
         --  that holds private entities whose views have changed between
         --  generic definition and instantiation. If this is the instance
         --  created to validate an actual package, the instantiation
         --  environment is that of the enclosing instance.

         Generic_Renamings.Set_Last (0);
         Generic_Renamings_HTable.Reset;

         Create_Instantiation_Source (N, Gen_Unit, False, S_Adjustment);

         --  Copy original generic tree, to produce text for instantiation

         Act_Tree :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);

         Act_Spec := Specification (Act_Tree);

         --  If this is the instance created to validate an actual package,
         --  only the formals matter, do not examine the package spec itself.

         if Is_Actual_Pack then
            Set_Visible_Declarations (Act_Spec, New_List);
            Set_Private_Declarations (Act_Spec, New_List);
         end if;

         Renaming_List :=
           Analyze_Associations
             (N,
              Generic_Formal_Declarations (Act_Tree),
              Generic_Formal_Declarations (Gen_Decl));

         Set_Defining_Unit_Name (Act_Spec, Act_Decl_Name);
         Set_Is_Generic_Instance (Act_Decl_Id);

         Set_Generic_Parent (Act_Spec, Gen_Unit);

         --  References to the generic in its own declaration or its body
         --  are references to the instance. Add a renaming declaration for
         --  the generic unit itself. This declaration, as well as the renaming
         --  declarations for the generic formals, must remain private to the
         --  unit: the formals, because this is the language semantics, and
         --  the unit because its use is an artifact of the implementation.

         Unit_Renaming :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc, Chars (Gen_Unit)),
             Name => New_Reference_To (Act_Decl_Id, Loc));

         Append (Unit_Renaming, Renaming_List);

         --  The renaming declarations are the first local declarations of
         --  the new unit.

         if Is_Non_Empty_List (Visible_Declarations (Act_Spec)) then
            Insert_List_Before
              (First (Visible_Declarations (Act_Spec)), Renaming_List);
         else
            Set_Visible_Declarations (Act_Spec, Renaming_List);
         end if;

         Act_Decl :=
           Make_Package_Declaration (Loc,
             Specification => Act_Spec);

         --  Save the instantiation node, for subsequent instantiation
         --  of the body, if there is one and we are generating code for
         --  the current unit. Mark the unit as having a body, to avoid
         --  a premature error message.

         --  We instantiate the body if we are generating code, if we are
         --  generating cross-reference information, or if we are building
         --  trees for ASIS use.

         declare
            Enclosing_Body_Present : Boolean := False;
            --  If the generic unit is not a compilation unit, then a body
            --  may be present in its parent even if none is required. We
            --  create a tentative pending instantiation for the body, which
            --  will be discarded if none is actually present.

            Scop : Entity_Id;

         begin
            if Scope (Gen_Unit) /= Standard_Standard
              and then not Is_Child_Unit (Gen_Unit)
            then
               Scop := Scope (Gen_Unit);

               while Present (Scop)
                 and then Scop /= Standard_Standard
               loop
                  if Unit_Requires_Body (Scop) then
                     Enclosing_Body_Present := True;
                     exit;

                  elsif In_Open_Scopes (Scop)
                    and then In_Package_Body (Scop)
                  then
                     Enclosing_Body_Present := True;
                     exit;
                  end if;

                  exit when Is_Compilation_Unit (Scop);
                  Scop := Scope (Scop);
               end loop;
            end if;

            --  If front-end inlining is enabled, and this is a unit for which
            --  code will be generated, we instantiate the body at once.
            --  This is done if the instance is not the main unit, and if the
            --  generic is not a child unit of another generic, to avoid scope
            --  problems and the reinstallation of parent instances.

            if Expander_Active
              and then (not Is_Child_Unit (Gen_Unit)
                         or else not Is_Generic_Unit (Scope (Gen_Unit)))
              and then Might_Inline_Subp
              and then not Is_Actual_Pack
            then
               if Front_End_Inlining
                 and then (Is_In_Main_Unit (N)
                            or else In_Main_Context (Current_Scope))
                 and then Nkind (Parent (N)) /= N_Compilation_Unit
               then
                  Inline_Now := True;

               --  In configurable_run_time mode we force the inlining of
               --  predefined subprogram marked Inline_Always, to minimize
               --  the use of the run-time library.

               elsif Is_Predefined_File_Name
                       (Unit_File_Name (Get_Source_Unit (Gen_Decl)))
                 and then Configurable_Run_Time_Mode
                 and then Nkind (Parent (N)) /= N_Compilation_Unit
               then
                  Inline_Now := True;
               end if;

               --  If the current scope is itself an instance within a child
               --  unit, and that unit itself is not an instance, it is
               --  duplicated in the scope stack, and the unstacking mechanism
               --  in Inline_Instance_Body will fail. This loses some rare
               --  cases of optimization, and might be improved some day ????

               if Is_Generic_Instance (Current_Scope)
                  and then Is_Child_Unit (Scope (Current_Scope))
                  and then not Is_Generic_Instance (Scope (Current_Scope))
               then
                  Inline_Now := False;
               end if;
            end if;

            Needs_Body :=
              (Unit_Requires_Body (Gen_Unit)
                  or else Enclosing_Body_Present
                  or else Present (Corresponding_Body (Gen_Decl)))
                and then (Is_In_Main_Unit (N)
                           or else Might_Inline_Subp)
                and then not Is_Actual_Pack
                and then not Inline_Now
                and then (Operating_Mode = Generate_Code
                            or else (Operating_Mode = Check_Semantics
                                      and then ASIS_Mode));

            --  If front_end_inlining is enabled, do not instantiate a
            --  body if within a generic context.

            if (Front_End_Inlining
                  and then not Expander_Active)
              or else Is_Generic_Unit (Cunit_Entity (Main_Unit))
            then
               Needs_Body := False;
            end if;

            --  If the current context is generic, and the package being
            --  instantiated is declared within a formal package, there is no
            --  body to instantiate until the enclosing generic is instantiated
            --  and there is an actual for the formal package. If the formal
            --  package has parameters, we build regular package instance for
            --  it, that preceeds the original formal package declaration.

            if In_Open_Scopes (Scope (Scope (Gen_Unit))) then
               declare
                  Decl : constant Node_Id :=
                           Original_Node
                             (Unit_Declaration_Node (Scope (Gen_Unit)));
               begin
                  if Nkind (Decl) = N_Formal_Package_Declaration
                    or else (Nkind (Decl) = N_Package_Declaration
                      and then Is_List_Member (Decl)
                      and then Present (Next (Decl))
                      and then
                        Nkind (Next (Decl)) = N_Formal_Package_Declaration)
                  then
                     Needs_Body := False;
                  end if;
               end;
            end if;
         end;

         --  If we are generating the calling stubs from the instantiation of
         --  a generic RCI package, we will not use the body of the generic
         --  package.

         if Distribution_Stub_Mode = Generate_Caller_Stub_Body
           and then Is_Compilation_Unit (Defining_Entity (N))
         then
            Needs_Body := False;
         end if;

         if Needs_Body then

            --  Here is a defence against a ludicrous number of instantiations
            --  caused by a circular set of instantiation attempts.

            if Pending_Instantiations.Last >
                 Hostparm.Max_Instantiations
            then
               Error_Msg_N ("too many instantiations", N);
               raise Unrecoverable_Error;
            end if;

            --  Indicate that the enclosing scopes contain an instantiation,
            --  and that cleanup actions should be delayed until after the
            --  instance body is expanded.

            Check_Forward_Instantiation (Gen_Decl);
            if Nkind (N) = N_Package_Instantiation then
               declare
                  Enclosing_Master : Entity_Id := Current_Scope;

               begin
                  while Enclosing_Master /= Standard_Standard loop

                     if Ekind (Enclosing_Master) = E_Package then
                        if Is_Compilation_Unit (Enclosing_Master) then
                           if In_Package_Body (Enclosing_Master) then
                              Delay_Descriptors
                                (Body_Entity (Enclosing_Master));
                           else
                              Delay_Descriptors
                                (Enclosing_Master);
                           end if;

                           exit;

                        else
                           Enclosing_Master := Scope (Enclosing_Master);
                        end if;

                     elsif Ekind (Enclosing_Master) = E_Generic_Package then
                        Enclosing_Master := Scope (Enclosing_Master);

                     elsif Is_Generic_Subprogram (Enclosing_Master)
                       or else Ekind (Enclosing_Master) = E_Void
                     then
                        --  Cleanup actions will eventually be performed on
                        --  the enclosing instance, if any. enclosing scope
                        --  is void in the formal part of a generic subp.

                        exit;

                     else
                        if Ekind (Enclosing_Master) = E_Entry
                          and then
                            Ekind (Scope (Enclosing_Master)) = E_Protected_Type
                        then
                           Enclosing_Master :=
                             Protected_Body_Subprogram (Enclosing_Master);
                        end if;

                        Set_Delay_Cleanups (Enclosing_Master);

                        while Ekind (Enclosing_Master) = E_Block loop
                           Enclosing_Master := Scope (Enclosing_Master);
                        end loop;

                        if Is_Subprogram (Enclosing_Master) then
                           Delay_Descriptors (Enclosing_Master);

                        elsif Is_Task_Type (Enclosing_Master) then
                           declare
                              TBP : constant Node_Id :=
                                      Get_Task_Body_Procedure
                                        (Enclosing_Master);

                           begin
                              if Present (TBP) then
                                 Delay_Descriptors  (TBP);
                                 Set_Delay_Cleanups (TBP);
                              end if;
                           end;
                        end if;

                        exit;
                     end if;
                  end loop;
               end;

               --  Make entry in table

               Pending_Instantiations.Increment_Last;
               Pending_Instantiations.Table (Pending_Instantiations.Last) :=
                 (N, Act_Decl, Expander_Active, Current_Sem_Unit);
            end if;
         end if;

         Set_Categorization_From_Pragmas (Act_Decl);

         if Parent_Installed then
            Hide_Current_Scope;
         end if;

         Set_Instance_Spec (N, Act_Decl);

         --  If not a compilation unit, insert the package declaration
         --  before the original instantiation node.

         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Mark_Rewrite_Insertion (Act_Decl);
            Insert_Before (N, Act_Decl);
            Analyze (Act_Decl);

         --  For an instantiation that is a compilation unit, place
         --  declaration on current node so context is complete
         --  for analysis (including nested instantiations). It this
         --  is the main unit, the declaration eventually replaces the
         --  instantiation node. If the instance body is later created, it
         --  replaces the instance node, and the declation is attached to
         --  it (see Build_Instance_Compilation_Unit_Nodes).

         else
            if Cunit_Entity (Current_Sem_Unit) = Defining_Entity (N) then

               --  The entity for the current unit is the newly created one,
               --  and all semantic information is attached to it.

               Set_Cunit_Entity (Current_Sem_Unit, Act_Decl_Id);

               --  If this is the main unit, replace the main entity as well

               if Current_Sem_Unit = Main_Unit then
                  Main_Unit_Entity := Act_Decl_Id;
               end if;
            end if;

            --  There is a problem with inlining here
            --  More comments needed??? what problem

            Set_Unit (Parent (N), Act_Decl);
            Set_Parent_Spec (Act_Decl, Parent_Spec (N));
            Set_Package_Instantiation (Act_Decl_Id, N);
            Analyze (Act_Decl);
            Set_Unit (Parent (N), N);
            Set_Body_Required (Parent (N), False);

            --  We never need elaboration checks on instantiations, since
            --  by definition, the body instantiation is elaborated at the
            --  same time as the spec instantiation.

            Set_Suppress_Elaboration_Warnings (Act_Decl_Id);
            Set_Kill_Elaboration_Checks       (Act_Decl_Id);
         end if;

         Check_Elab_Instantiation (N);

         if ABE_Is_Certain (N) and then Needs_Body then
            Pending_Instantiations.Decrement_Last;
         end if;
         Check_Hidden_Child_Unit (N, Gen_Unit, Act_Decl_Id);

         Set_First_Private_Entity (Defining_Unit_Name (Unit_Renaming),
           First_Private_Entity (Act_Decl_Id));

         --  If the instantiation will receive a body, the unit will
         --  be transformed into a package body, and receive its own
         --  elaboration entity. Otherwise, the nature of the unit is
         --  now a package declaration.

         if Nkind (Parent (N)) = N_Compilation_Unit
           and then not Needs_Body
         then
            Rewrite (N, Act_Decl);
         end if;

         if Present (Corresponding_Body (Gen_Decl))
           or else Unit_Requires_Body (Gen_Unit)
         then
            Set_Has_Completion (Act_Decl_Id);
         end if;

         Check_Formal_Packages (Act_Decl_Id);

         Restore_Private_Views (Act_Decl_Id);

         if not Generic_Separately_Compiled (Gen_Unit) then
            Inherit_Context (Gen_Decl, N);
         end if;

         if Parent_Installed then
            Remove_Parent;
         end if;

         Restore_Env;
         Env_Installed := False;
      end if;

      Validate_Categorization_Dependency (N, Act_Decl_Id);

      --  Check restriction, but skip this if something went wrong in
      --  the above analysis, indicated by Act_Decl_Id being void.

      if Ekind (Act_Decl_Id) /= E_Void
        and then not Is_Library_Level_Entity (Act_Decl_Id)
      then
         Check_Restriction (No_Local_Allocators, N);
      end if;

      if Inline_Now then
         Inline_Instance_Body (N, Gen_Unit, Act_Decl);
      end if;

      --  The following is a tree patch for ASIS: ASIS needs separate nodes
      --  to be used as defining identifiers for a formal package and for the
      --  corresponding expanded package

      if Nkind (N) = N_Formal_Package_Declaration then
         Act_Decl_Id := New_Copy (Defining_Entity (N));
         Set_Comes_From_Source (Act_Decl_Id, True);
         Set_Is_Generic_Instance (Act_Decl_Id, False);
         Set_Defining_Identifier (N, Act_Decl_Id);
      end if;

   exception
      when Instantiation_Error =>
         if Parent_Installed then
            Remove_Parent;
         end if;

         if Env_Installed then
            Restore_Env;
         end if;
   end Analyze_Package_Instantiation;

   --------------------------
   -- Inline_Instance_Body --
   --------------------------

   procedure Inline_Instance_Body
     (N        : Node_Id;
      Gen_Unit : Entity_Id;
      Act_Decl : Node_Id)
   is
      Vis          : Boolean;
      Gen_Comp     : constant Entity_Id :=
                      Cunit_Entity (Get_Source_Unit (Gen_Unit));
      Curr_Comp    : constant Node_Id := Cunit (Current_Sem_Unit);
      Curr_Scope   : Entity_Id := Empty;
      Curr_Unit    : constant Entity_Id :=
                       Cunit_Entity (Current_Sem_Unit);
      Removed      : Boolean := False;
      Num_Scopes   : Int := 0;
      Use_Clauses  : array (1 .. Scope_Stack.Last) of Node_Id;
      Instances    : array (1 .. Scope_Stack.Last) of Entity_Id;
      Inner_Scopes : array (1 .. Scope_Stack.Last) of Entity_Id;
      Num_Inner    : Int := 0;
      N_Instances  : Int := 0;
      S            : Entity_Id;

   begin
      --  Case of generic unit defined in another unit. We must remove the
      --  complete context of the current unit to install that of the generic.

      if Gen_Comp /= Cunit_Entity (Current_Sem_Unit) then

         --  Add some comments for the following two loops ???

         S := Current_Scope;
         while Present (S) and then S /= Standard_Standard loop
            loop
               Num_Scopes := Num_Scopes + 1;

               Use_Clauses (Num_Scopes) :=
                 (Scope_Stack.Table
                    (Scope_Stack.Last - Num_Scopes + 1).
                       First_Use_Clause);
               End_Use_Clauses (Use_Clauses (Num_Scopes));

               exit when Scope_Stack.Last - Num_Scopes + 1 = Scope_Stack.First
                 or else Scope_Stack.Table
                           (Scope_Stack.Last - Num_Scopes).Entity
                             = Scope (S);
            end loop;

            exit when Is_Generic_Instance (S)
              and then (In_Package_Body (S)
                          or else Ekind (S) = E_Procedure
                          or else Ekind (S) = E_Function);
            S := Scope (S);
         end loop;

         Vis := Is_Immediately_Visible (Gen_Comp);

         --  Find and save all enclosing instances

         S := Current_Scope;

         while Present (S)
           and then S /= Standard_Standard
         loop
            if Is_Generic_Instance (S) then
               N_Instances := N_Instances + 1;
               Instances (N_Instances) := S;

               exit when In_Package_Body (S);
            end if;

            S := Scope (S);
         end loop;

         --  Remove context of current compilation unit, unless we are within a
         --  nested package instantiation, in which case the context has been
         --  removed previously.

         --  If current scope is the body of a child unit, remove context of
         --  spec as well.

         S := Current_Scope;

         while Present (S)
           and then S /= Standard_Standard
         loop
            exit when Is_Generic_Instance (S)
                 and then (In_Package_Body (S)
                            or else Ekind (S) = E_Procedure
                            or else Ekind (S) = E_Function);

            if S = Curr_Unit
              or else (Ekind (Curr_Unit) = E_Package_Body
                        and then S = Spec_Entity (Curr_Unit))
              or else (Ekind (Curr_Unit) = E_Subprogram_Body
                        and then S =
                          Corresponding_Spec
                            (Unit_Declaration_Node (Curr_Unit)))
            then
               Removed := True;

               --  Remove entities in current scopes from visibility, so
               --  that instance body is compiled in a clean environment.

               Save_Scope_Stack (Handle_Use => False);

               if Is_Child_Unit (S) then

                  --  Remove child unit from stack, as well as inner scopes.
                  --  Removing the context of a child unit removes parent
                  --  units as well.

                  while Current_Scope /= S loop
                     Num_Inner := Num_Inner + 1;
                     Inner_Scopes (Num_Inner) := Current_Scope;
                     Pop_Scope;
                  end loop;

                  Pop_Scope;
                  Remove_Context (Curr_Comp);
                  Curr_Scope := S;

               else
                  Remove_Context (Curr_Comp);
               end if;

               if Ekind (Curr_Unit) = E_Package_Body then
                  Remove_Context (Library_Unit (Curr_Comp));
               end if;
            end if;

            S := Scope (S);
         end loop;
         pragma Assert (Num_Inner < Num_Scopes);

         New_Scope (Standard_Standard);
         Scope_Stack.Table (Scope_Stack.Last).Is_Active_Stack_Base := True;
         Instantiate_Package_Body
           ((N, Act_Decl, Expander_Active, Current_Sem_Unit), True);
         Pop_Scope;

         --  Restore context

         Set_Is_Immediately_Visible (Gen_Comp, Vis);

         --  Reset Generic_Instance flag so that use clauses can be installed
         --  in the proper order. (See Use_One_Package for effect of enclosing
         --  instances on processing of use clauses).

         for J in 1 .. N_Instances loop
            Set_Is_Generic_Instance (Instances (J), False);
         end loop;

         if Removed then
            Install_Context (Curr_Comp);

            if Present (Curr_Scope)
              and then Is_Child_Unit (Curr_Scope)
            then
               New_Scope (Curr_Scope);
               Set_Is_Immediately_Visible (Curr_Scope);

               --  Finally, restore inner scopes as well

               for J in reverse 1 .. Num_Inner loop
                  New_Scope (Inner_Scopes (J));
               end loop;
            end if;

            Restore_Scope_Stack (Handle_Use => False);

            if Present (Curr_Scope)
              and then
                (In_Private_Part (Curr_Scope)
                  or else In_Package_Body (Curr_Scope))
            then
               --  Install private declaration of ancestor units, which
               --  are currently available. Restore_Scope_Stack and
               --  Install_Context only install the visible part of parents.

               declare
                  Par : Entity_Id;
               begin
                  Par := Scope (Curr_Scope);
                  while (Present (Par))
                    and then Par /= Standard_Standard
                  loop
                     Install_Private_Declarations (Par);
                     Par := Scope (Par);
                  end loop;
               end;
            end if;
         end if;

         --  Restore use clauses. For a child unit, use clauses in the parents
         --  are restored when installing the context, so only those in inner
         --  scopes (and those local to the child unit itself) need to be
         --  installed explicitly.

         if Is_Child_Unit (Curr_Unit)
           and then Removed
         then
            for J in reverse 1 .. Num_Inner + 1 loop
               Scope_Stack.Table (Scope_Stack.Last - J + 1).First_Use_Clause :=
                 Use_Clauses (J);
               Install_Use_Clauses (Use_Clauses (J));
            end  loop;

         else
            for J in reverse 1 .. Num_Scopes loop
               Scope_Stack.Table (Scope_Stack.Last - J + 1).First_Use_Clause :=
                 Use_Clauses (J);
               Install_Use_Clauses (Use_Clauses (J));
            end  loop;
         end if;

         for J in 1 .. N_Instances loop
            Set_Is_Generic_Instance (Instances (J), True);
         end loop;

      --  If generic unit is in current unit, current context is correct

      else
         Instantiate_Package_Body
           ((N, Act_Decl, Expander_Active, Current_Sem_Unit), True);
      end if;
   end Inline_Instance_Body;

   -------------------------------------
   -- Analyze_Procedure_Instantiation --
   -------------------------------------

   procedure Analyze_Procedure_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Procedure);
   end Analyze_Procedure_Instantiation;

   --------------------------------------
   -- Analyze_Subprogram_Instantiation --
   --------------------------------------

   procedure Analyze_Subprogram_Instantiation
     (N : Node_Id;
      K : Entity_Kind)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Gen_Id : constant Node_Id    := Name (N);

      Anon_Id : constant Entity_Id :=
                  Make_Defining_Identifier (Sloc (Defining_Entity (N)),
                    Chars => New_External_Name
                               (Chars (Defining_Entity (N)), 'R'));

      Act_Decl_Id : Entity_Id;
      Act_Decl    : Node_Id;
      Act_Spec    : Node_Id;
      Act_Tree    : Node_Id;

      Env_Installed    : Boolean := False;
      Gen_Unit         : Entity_Id;
      Gen_Decl         : Node_Id;
      Pack_Id          : Entity_Id;
      Parent_Installed : Boolean := False;
      Renaming_List    : List_Id;

      procedure Analyze_Instance_And_Renamings;
      --  The instance must be analyzed in a context that includes the
      --  mappings of generic parameters into actuals. We create a package
      --  declaration for this purpose, and a subprogram with an internal
      --  name within the package. The subprogram instance is simply an
      --  alias for the internal subprogram, declared in the current scope.

      ------------------------------------
      -- Analyze_Instance_And_Renamings --
      ------------------------------------

      procedure Analyze_Instance_And_Renamings is
         Def_Ent   : constant Entity_Id := Defining_Entity (N);
         Pack_Decl : Node_Id;

      begin
         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  For the case of a compilation unit, the container package
            --  has the same name as the instantiation, to insure that the
            --  binder calls the elaboration procedure with the right name.
            --  Copy the entity of the instance, which may have compilation
            --  level flags (e.g. Is_Child_Unit) set.

            Pack_Id := New_Copy (Def_Ent);

         else
            --  Otherwise we use the name of the instantiation concatenated
            --  with its source position to ensure uniqueness if there are
            --  several instantiations with the same name.

            Pack_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name
                           (Related_Id   => Chars (Def_Ent),
                            Suffix       => "GP",
                            Suffix_Index => Source_Offset (Sloc (Def_Ent))));
         end if;

         Pack_Decl := Make_Package_Declaration (Loc,
           Specification => Make_Package_Specification (Loc,
             Defining_Unit_Name   => Pack_Id,
             Visible_Declarations => Renaming_List,
             End_Label            => Empty));

         Set_Instance_Spec (N, Pack_Decl);
         Set_Is_Generic_Instance (Pack_Id);
         Set_Needs_Debug_Info (Pack_Id);

         --  Case of not a compilation unit

         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Mark_Rewrite_Insertion (Pack_Decl);
            Insert_Before (N, Pack_Decl);
            Set_Has_Completion (Pack_Id);

         --  Case of an instantiation that is a compilation unit

         --  Place declaration on current node so context is complete
         --  for analysis (including nested instantiations), and for
         --  use in a context_clause (see Analyze_With_Clause).

         else
            Set_Unit (Parent (N), Pack_Decl);
            Set_Parent_Spec (Pack_Decl, Parent_Spec (N));
         end if;

         Analyze (Pack_Decl);
         Check_Formal_Packages (Pack_Id);
         Set_Is_Generic_Instance (Pack_Id, False);

         --  Body of the enclosing package is supplied when instantiating
         --  the subprogram body, after semantic  analysis is completed.

         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  Remove package itself from visibility, so it does not
            --  conflict with subprogram.

            Set_Name_Entity_Id (Chars (Pack_Id), Homonym (Pack_Id));

            --  Set name and scope of internal subprogram so that the
            --  proper external name will be generated. The proper scope
            --  is the scope of the wrapper package. We need to generate
            --  debugging information for the internal subprogram, so set
            --  flag accordingly.

            Set_Chars (Anon_Id, Chars (Defining_Entity (N)));
            Set_Scope (Anon_Id, Scope (Pack_Id));

            --  Mark wrapper package as referenced, to avoid spurious
            --  warnings if the instantiation appears in various with_
            --  clauses of subunits of the main unit.

            Set_Referenced (Pack_Id);
         end if;

         Set_Is_Generic_Instance (Anon_Id);
         Set_Needs_Debug_Info    (Anon_Id);
         Act_Decl_Id := New_Copy (Anon_Id);

         Set_Parent            (Act_Decl_Id, Parent (Anon_Id));
         Set_Chars             (Act_Decl_Id, Chars (Defining_Entity (N)));
         Set_Sloc              (Act_Decl_Id, Sloc (Defining_Entity (N)));
         Set_Comes_From_Source (Act_Decl_Id, True);

         --  The signature may involve types that are not frozen yet, but
         --  the subprogram will be frozen at the point the wrapper package
         --  is frozen, so it does not need its own freeze node. In fact, if
         --  one is created, it might conflict with the freezing actions from
         --  the wrapper package (see 7206-013).

         Set_Has_Delayed_Freeze (Anon_Id, False);

         --  If the instance is a child unit, mark the Id accordingly. Mark
         --  the anonymous entity as well, which is the real subprogram and
         --  which is used when the instance appears in a context clause.

         Set_Is_Child_Unit (Act_Decl_Id, Is_Child_Unit (Defining_Entity (N)));
         Set_Is_Child_Unit (Anon_Id, Is_Child_Unit (Defining_Entity (N)));
         New_Overloaded_Entity (Act_Decl_Id);
         Check_Eliminated  (Act_Decl_Id);

         --  In compilation unit case, kill elaboration checks on the
         --  instantiation, since they are never needed -- the body is
         --  instantiated at the same point as the spec.

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Set_Suppress_Elaboration_Warnings (Act_Decl_Id);
            Set_Kill_Elaboration_Checks       (Act_Decl_Id);
            Set_Is_Compilation_Unit (Anon_Id);

            Set_Cunit_Entity (Current_Sem_Unit, Pack_Id);
         end if;

         --  The instance is not a freezing point for the new subprogram

         Set_Is_Frozen (Act_Decl_Id, False);

         if Nkind (Defining_Entity (N)) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Act_Decl_Id);
         end if;

         Set_Alias  (Act_Decl_Id, Anon_Id);
         Set_Parent (Act_Decl_Id, Parent (Anon_Id));
         Set_Has_Completion (Act_Decl_Id);
         Set_Related_Instance (Pack_Id, Act_Decl_Id);

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Set_Body_Required (Parent (N), False);
         end if;

      end Analyze_Instance_And_Renamings;

   --  Start of processing for Analyze_Subprogram_Instantiation

   begin
      --  Very first thing: apply the special kludge for Text_IO processing
      --  in case we are instantiating one of the children of [Wide_]Text_IO.
      --  Of course such an instantiation is bogus (these are packages, not
      --  subprograms), but we get a better error message if we do this.

      Text_IO_Kludge (Gen_Id);

      --  Make node global for error reporting

      Instantiation_Node := N;
      Pre_Analyze_Actuals (N);

      Init_Env;
      Env_Installed := True;
      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      Generate_Reference (Gen_Unit, Gen_Id);

      if Nkind (Gen_Id) = N_Identifier
        and then Chars (Gen_Unit) = Chars (Defining_Entity (N))
      then
         Error_Msg_NE
           ("& is hidden within declaration of instance", Gen_Id, Gen_Unit);
      end if;

      if Etype (Gen_Unit) = Any_Type then
         Restore_Env;
         return;
      end if;

      --  Verify that it is a generic subprogram of the right kind, and that
      --  it does not lead to a circular instantiation.

      if Ekind (Gen_Unit) /= E_Generic_Procedure
        and then Ekind (Gen_Unit) /= E_Generic_Function
      then
         Error_Msg_N ("expect generic subprogram in instantiation", Gen_Id);

      elsif In_Open_Scopes (Gen_Unit) then
         Error_Msg_NE ("instantiation of & within itself", N, Gen_Unit);

      elsif K = E_Procedure
        and then Ekind (Gen_Unit) /= E_Generic_Procedure
      then
         if Ekind (Gen_Unit) = E_Generic_Function then
            Error_Msg_N
              ("cannot instantiate generic function as procedure", Gen_Id);
         else
            Error_Msg_N
              ("expect name of generic procedure in instantiation", Gen_Id);
         end if;

      elsif K = E_Function
        and then Ekind (Gen_Unit) /= E_Generic_Function
      then
         if Ekind (Gen_Unit) = E_Generic_Procedure then
            Error_Msg_N
              ("cannot instantiate generic procedure as function", Gen_Id);
         else
            Error_Msg_N
              ("expect name of generic function in instantiation", Gen_Id);
         end if;

      else
         Set_Entity (Gen_Id, Gen_Unit);
         Set_Is_Instantiated (Gen_Unit);

         if In_Extended_Main_Source_Unit (N) then
            Generate_Reference (Gen_Unit, N);
         end if;

         --  If renaming, get original unit

         if Present (Renamed_Object (Gen_Unit))
           and then (Ekind (Renamed_Object (Gen_Unit)) = E_Generic_Procedure
                       or else
                     Ekind (Renamed_Object (Gen_Unit)) = E_Generic_Function)
         then
            Gen_Unit := Renamed_Object (Gen_Unit);
            Set_Is_Instantiated (Gen_Unit);
            Generate_Reference  (Gen_Unit, N);
         end if;

         if Contains_Instance_Of (Gen_Unit, Current_Scope, Gen_Id) then
            Error_Msg_Node_2 := Current_Scope;
            Error_Msg_NE
              ("circular Instantiation: & instantiated in &!", N, Gen_Unit);
            Circularity_Detected := True;
            return;
         end if;

         Gen_Decl := Unit_Declaration_Node (Gen_Unit);

         --  The subprogram itself cannot contain a nested instance, so
         --  the current parent is left empty.

         Set_Instance_Env (Gen_Unit, Empty);

         --  Initialize renamings map, for error checking

         Generic_Renamings.Set_Last (0);
         Generic_Renamings_HTable.Reset;

         Create_Instantiation_Source (N, Gen_Unit, False, S_Adjustment);

         --  Copy original generic tree, to produce text for instantiation

         Act_Tree :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);

         Act_Spec := Specification (Act_Tree);
         Renaming_List :=
           Analyze_Associations
             (N,
              Generic_Formal_Declarations (Act_Tree),
              Generic_Formal_Declarations (Gen_Decl));

         --  Build the subprogram declaration, which does not appear
         --  in the generic template, and give it a sloc consistent
         --  with that of the template.

         Set_Defining_Unit_Name (Act_Spec, Anon_Id);
         Set_Generic_Parent (Act_Spec, Gen_Unit);
         Act_Decl :=
           Make_Subprogram_Declaration (Sloc (Act_Spec),
             Specification => Act_Spec);

         Set_Categorization_From_Pragmas (Act_Decl);

         if Parent_Installed then
            Hide_Current_Scope;
         end if;

         Append (Act_Decl, Renaming_List);
         Analyze_Instance_And_Renamings;

         --  If the generic is marked Import (Intrinsic), then so is the
         --  instance. This indicates that there is no body to instantiate.
         --  If generic is marked inline, so it the instance, and the
         --  anonymous subprogram it renames. If inlined, or else if inlining
         --  is enabled for the compilation, we generate the instance body
         --  even if it is not within the main unit.

         --  Any other  pragmas might also be inherited ???

         if Is_Intrinsic_Subprogram (Gen_Unit) then
            Set_Is_Intrinsic_Subprogram (Anon_Id);
            Set_Is_Intrinsic_Subprogram (Act_Decl_Id);

            if Chars (Gen_Unit) = Name_Unchecked_Conversion then
               Validate_Unchecked_Conversion (N, Act_Decl_Id);
            end if;
         end if;

         Generate_Definition (Act_Decl_Id);

         Set_Is_Inlined (Act_Decl_Id, Is_Inlined (Gen_Unit));
         Set_Is_Inlined (Anon_Id,     Is_Inlined (Gen_Unit));

         if not Is_Intrinsic_Subprogram (Gen_Unit) then
            Check_Elab_Instantiation (N);
         end if;

         if Is_Dispatching_Operation (Act_Decl_Id)
           and then Ada_Version >= Ada_05
         then
            declare
               Formal : Entity_Id;

            begin
               Formal := First_Formal (Act_Decl_Id);
               while Present (Formal) loop
                  if Ekind (Etype (Formal)) = E_Anonymous_Access_Type
                    and then Is_Controlling_Formal (Formal)
                    and then not Can_Never_Be_Null (Formal)
                  then
                     Error_Msg_NE ("access parameter& is controlling,",
                       N, Formal);
                     Error_Msg_NE ("\corresponding parameter of & must be"
                       & " explicitly null-excluding", N, Gen_Id);
                  end if;

                  Next_Formal (Formal);
               end loop;
            end;
         end if;

         Check_Hidden_Child_Unit (N, Gen_Unit, Act_Decl_Id);

         --  Subject to change, pending on if other pragmas are inherited ???

         Validate_Categorization_Dependency (N, Act_Decl_Id);

         if not Is_Intrinsic_Subprogram (Act_Decl_Id) then
            if not Generic_Separately_Compiled (Gen_Unit) then
               Inherit_Context (Gen_Decl, N);
            end if;

            Restore_Private_Views (Pack_Id, False);

            --  If the context requires a full instantiation, mark node for
            --  subsequent construction of the body.

            if (Is_In_Main_Unit (N)
                  or else Is_Inlined (Act_Decl_Id))
              and then (Operating_Mode = Generate_Code
                          or else (Operating_Mode = Check_Semantics
                                     and then ASIS_Mode))
              and then (Expander_Active or else ASIS_Mode)
              and then not ABE_Is_Certain (N)
              and then not Is_Eliminated (Act_Decl_Id)
            then
               Pending_Instantiations.Increment_Last;
               Pending_Instantiations.Table (Pending_Instantiations.Last) :=
                 (N, Act_Decl, Expander_Active, Current_Sem_Unit);
               Check_Forward_Instantiation (Gen_Decl);

               --  The wrapper package is always delayed, because it does
               --  not constitute a freeze point, but to insure that the
               --  freeze node is placed properly, it is created directly
               --  when instantiating the body (otherwise the freeze node
               --  might appear to early for nested instantiations).

            elsif Nkind (Parent (N)) = N_Compilation_Unit then

               --  For ASIS purposes, indicate that the wrapper package has
               --  replaced the instantiation node.

               Rewrite (N, Unit (Parent (N)));
               Set_Unit (Parent (N), N);
            end if;

         elsif Nkind (Parent (N)) = N_Compilation_Unit then

               --  Replace instance node for library-level instantiations
               --  of intrinsic subprograms, for ASIS use.

               Rewrite (N, Unit (Parent (N)));
               Set_Unit (Parent (N), N);
         end if;

         if Parent_Installed then
            Remove_Parent;
         end if;

         Restore_Env;
         Env_Installed := False;
         Generic_Renamings.Set_Last (0);
         Generic_Renamings_HTable.Reset;
      end if;

   exception
      when Instantiation_Error =>
         if Parent_Installed then
            Remove_Parent;
         end if;

         if Env_Installed then
            Restore_Env;
         end if;
   end Analyze_Subprogram_Instantiation;

   -------------------------
   -- Get_Associated_Node --
   -------------------------

   function Get_Associated_Node (N : Node_Id) return Node_Id is
      Assoc : Node_Id := Associated_Node (N);

   begin
      if Nkind (Assoc) /= Nkind (N) then
         return Assoc;

      elsif Nkind (Assoc) = N_Aggregate
        or else Nkind (Assoc) = N_Extension_Aggregate
      then
         return Assoc;

      else
         --  If the node is part of an inner generic, it may itself have been
         --  remapped into a further generic copy. Associated_Node is otherwise
         --  used for the entity of the node, and will be of a different node
         --  kind, or else N has been rewritten as a literal or function call.

         while Present (Associated_Node (Assoc))
           and then Nkind (Associated_Node (Assoc)) = Nkind (Assoc)
         loop
            Assoc := Associated_Node (Assoc);
         end loop;

         --  Follow and additional link in case the final node was rewritten.
         --  This can only happen with nested generic units.

         if (Nkind (Assoc) = N_Identifier or else Nkind (Assoc) in N_Op)
           and then Present (Associated_Node (Assoc))
           and then (Nkind (Associated_Node (Assoc)) = N_Function_Call
                       or else
                     Nkind (Associated_Node (Assoc)) = N_Explicit_Dereference
                       or else
                     Nkind (Associated_Node (Assoc)) = N_Integer_Literal
                       or else
                     Nkind (Associated_Node (Assoc)) = N_Real_Literal
                       or else
                     Nkind (Associated_Node (Assoc)) = N_String_Literal)
         then
            Assoc := Associated_Node (Assoc);
         end if;

         return Assoc;
      end if;
   end Get_Associated_Node;

   -------------------------------------------
   -- Build_Instance_Compilation_Unit_Nodes --
   -------------------------------------------

   procedure Build_Instance_Compilation_Unit_Nodes
     (N        : Node_Id;
      Act_Body : Node_Id;
      Act_Decl : Node_Id)
   is
      Decl_Cunit : Node_Id;
      Body_Cunit : Node_Id;
      Citem      : Node_Id;
      New_Main   : constant Entity_Id := Defining_Entity (Act_Decl);
      Old_Main   : constant Entity_Id := Cunit_Entity (Main_Unit);

   begin
      --  A new compilation unit node is built for the instance declaration

      Decl_Cunit :=
        Make_Compilation_Unit (Sloc (N),
          Context_Items  => Empty_List,
          Unit           => Act_Decl,
          Aux_Decls_Node =>
            Make_Compilation_Unit_Aux (Sloc (N)));

      Set_Parent_Spec   (Act_Decl, Parent_Spec (N));
      Set_Body_Required (Decl_Cunit, True);

      --  We use the original instantiation compilation unit as the resulting
      --  compilation unit of the instance, since this is the main unit.

      Rewrite (N, Act_Body);
      Body_Cunit := Parent (N);

      --  The two compilation unit nodes are linked by the Library_Unit field

      Set_Library_Unit  (Decl_Cunit, Body_Cunit);
      Set_Library_Unit  (Body_Cunit, Decl_Cunit);

      --  Preserve the private nature of the package if needed

      Set_Private_Present (Decl_Cunit, Private_Present (Body_Cunit));

      --  If the instance is not the main unit, its context, categorization,
      --  and elaboration entity are not relevant to the compilation.

      if Parent (N) /= Cunit (Main_Unit) then
         return;
      end if;

      --  The context clause items on the instantiation, which are now
      --  attached to the body compilation unit (since the body overwrote
      --  the original instantiation node), semantically belong on the spec,
      --  so copy them there. It's harmless to leave them on the body as well.
      --  In fact one could argue that they belong in both places.

      Citem := First (Context_Items (Body_Cunit));
      while Present (Citem) loop
         Append (New_Copy (Citem), Context_Items (Decl_Cunit));
         Next (Citem);
      end loop;

      --  Propagate categorization flags on packages, so that they appear
      --  in ali file for the spec of the unit.

      if Ekind (New_Main) = E_Package then
         Set_Is_Pure           (Old_Main, Is_Pure (New_Main));
         Set_Is_Preelaborated  (Old_Main, Is_Preelaborated (New_Main));
         Set_Is_Remote_Types   (Old_Main, Is_Remote_Types (New_Main));
         Set_Is_Shared_Passive (Old_Main, Is_Shared_Passive (New_Main));
         Set_Is_Remote_Call_Interface
           (Old_Main, Is_Remote_Call_Interface (New_Main));
      end if;

      --  Make entry in Units table, so that binder can generate call to
      --  elaboration procedure for body, if any.

      Make_Instance_Unit (Body_Cunit);
      Main_Unit_Entity := New_Main;
      Set_Cunit_Entity (Main_Unit, Main_Unit_Entity);

      --  Build elaboration entity, since the instance may certainly
      --  generate elaboration code requiring a flag for protection.

      Build_Elaboration_Entity (Decl_Cunit, New_Main);
   end Build_Instance_Compilation_Unit_Nodes;

   -----------------------------------
   -- Check_Formal_Package_Instance --
   -----------------------------------

   --  If the formal has specific parameters, they must match those of the
   --  actual. Both of them are instances, and the renaming declarations
   --  for their formal parameters appear in the same order in both. The
   --  analyzed formal has been analyzed in the context of the current
   --  instance.

   procedure Check_Formal_Package_Instance
     (Formal_Pack : Entity_Id;
      Actual_Pack : Entity_Id)
   is
      E1 : Entity_Id := First_Entity (Actual_Pack);
      E2 : Entity_Id := First_Entity (Formal_Pack);

      Expr1 : Node_Id;
      Expr2 : Node_Id;

      procedure Check_Mismatch (B : Boolean);
      --  Common error routine for mismatch between the parameters of
      --  the actual instance and those of the formal package.

      function Same_Instantiated_Constant (E1, E2 : Entity_Id) return Boolean;
      --  The formal may come from a nested formal package, and the actual
      --  may have been constant-folded. To determine whether the two denote
      --  the same entity we may have to traverse several definitions to
      --  recover the ultimate entity that they refer to.

      function Same_Instantiated_Variable (E1, E2 : Entity_Id) return Boolean;
      --  Similarly, if the formal comes from a nested formal package, the
      --  actual may designate the formal through multiple renamings, which
      --  have to be followed to determine the original variable in question.

      --------------------
      -- Check_Mismatch --
      --------------------

      procedure Check_Mismatch (B : Boolean) is
      begin
         if B then
            Error_Msg_NE
              ("actual for & in actual instance does not match formal",
               Parent (Actual_Pack), E1);
         end if;
      end Check_Mismatch;

      --------------------------------
      -- Same_Instantiated_Constant --
      --------------------------------

      function Same_Instantiated_Constant
        (E1, E2 : Entity_Id) return Boolean
      is
         Ent : Entity_Id;

      begin
         Ent := E2;
         while Present (Ent) loop
            if E1 = Ent then
               return True;

            elsif Ekind (Ent) /= E_Constant then
               return False;

            elsif Is_Entity_Name (Constant_Value (Ent)) then
               if  Entity (Constant_Value (Ent)) = E1 then
                  return True;
               else
                  Ent := Entity (Constant_Value (Ent));
               end if;

            --  The actual may be a constant that has been folded. Recover
            --  original name.

            elsif Is_Entity_Name (Original_Node (Constant_Value (Ent))) then
                  Ent := Entity (Original_Node (Constant_Value (Ent)));
            else
               return False;
            end if;
         end loop;

         return False;
      end Same_Instantiated_Constant;

      --------------------------------
      -- Same_Instantiated_Variable --
      --------------------------------

      function Same_Instantiated_Variable
        (E1, E2 : Entity_Id) return Boolean
      is
         function Original_Entity (E : Entity_Id) return Entity_Id;
         --  Follow chain of renamings to the ultimate ancestor

         ---------------------
         -- Original_Entity --
         ---------------------

         function Original_Entity (E : Entity_Id) return Entity_Id is
            Orig : Entity_Id;

         begin
            Orig := E;
            while Nkind (Parent (Orig)) = N_Object_Renaming_Declaration
              and then Present (Renamed_Object (Orig))
              and then Is_Entity_Name (Renamed_Object (Orig))
            loop
               Orig := Entity (Renamed_Object (Orig));
            end loop;

            return Orig;
         end Original_Entity;

      --  Start of processing for Same_Instantiated_Variable

      begin
         return Ekind (E1) = Ekind (E2)
           and then Original_Entity (E1) = Original_Entity (E2);
      end Same_Instantiated_Variable;

   --  Start of processing for Check_Formal_Package_Instance

   begin
      while Present (E1)
        and then Present (E2)
      loop
         exit when Ekind (E1) = E_Package
           and then Renamed_Entity (E1) = Renamed_Entity (Actual_Pack);

         if Is_Type (E1) then

            --  Subtypes must statically match. E1 and E2 are the
            --  local entities that are subtypes of the actuals.
            --  Itypes generated for other parameters need not be checked,
            --  the check will be performed on the parameters themselves.

            if not Is_Itype (E1)
              and then not Is_Itype (E2)
            then
               Check_Mismatch
                 (not Is_Type (E2)
                   or else Etype (E1) /= Etype (E2)
                   or else not Subtypes_Statically_Match (E1, E2));
            end if;

         elsif Ekind (E1) = E_Constant then

            --  IN parameters must denote the same static value, or
            --  the same constant, or the literal null.

            Expr1 := Expression (Parent (E1));

            if Ekind (E2) /= E_Constant then
               Check_Mismatch (True);
               goto Next_E;
            else
               Expr2 := Expression (Parent (E2));
            end if;

            if Is_Static_Expression (Expr1) then

               if not Is_Static_Expression (Expr2) then
                  Check_Mismatch (True);

               elsif Is_Integer_Type (Etype (E1)) then

                  declare
                     V1 : constant Uint := Expr_Value (Expr1);
                     V2 : constant Uint := Expr_Value (Expr2);
                  begin
                     Check_Mismatch (V1 /= V2);
                  end;

               elsif Is_Real_Type (Etype (E1)) then
                  declare
                     V1 : constant Ureal := Expr_Value_R (Expr1);
                     V2 : constant Ureal := Expr_Value_R (Expr2);
                  begin
                     Check_Mismatch (V1 /= V2);
                  end;

               elsif Is_String_Type (Etype (E1))
                 and then Nkind (Expr1) = N_String_Literal
               then

                  if Nkind (Expr2) /= N_String_Literal then
                     Check_Mismatch (True);
                  else
                     Check_Mismatch
                       (not String_Equal (Strval (Expr1), Strval (Expr2)));
                  end if;
               end if;

            elsif Is_Entity_Name (Expr1) then
               if Is_Entity_Name (Expr2) then
                  if Entity (Expr1) = Entity (Expr2) then
                     null;
                  else
                     Check_Mismatch
                       (not Same_Instantiated_Constant
                         (Entity (Expr1), Entity (Expr2)));
                  end if;
               else
                  Check_Mismatch (True);
               end if;

            elsif Is_Entity_Name (Original_Node (Expr1))
              and then Is_Entity_Name (Expr2)
            and then
              Same_Instantiated_Constant
                (Entity (Original_Node (Expr1)), Entity (Expr2))
            then
               null;

            elsif Nkind (Expr1) = N_Null then
               Check_Mismatch (Nkind (Expr1) /= N_Null);

            else
               Check_Mismatch (True);
            end if;

         elsif Ekind (E1) = E_Variable then
            Check_Mismatch (not Same_Instantiated_Variable (E1, E2));

         elsif Ekind (E1) = E_Package then
            Check_Mismatch
              (Ekind (E1) /= Ekind (E2)
                or else Renamed_Object (E1) /= Renamed_Object (E2));

         elsif Is_Overloadable (E1) then

            --  Verify that the names of the  entities match.
            --  What if actual is an attribute ???

            Check_Mismatch
              (Ekind (E2) /= Ekind (E1) or else (Alias (E1)) /= Alias (E2));

         else
            raise Program_Error;
         end if;

         <<Next_E>>
            Next_Entity (E1);
            Next_Entity (E2);
      end loop;
   end Check_Formal_Package_Instance;

   ---------------------------
   -- Check_Formal_Packages --
   ---------------------------

   procedure Check_Formal_Packages (P_Id : Entity_Id) is
      E        : Entity_Id;
      Formal_P : Entity_Id;

   begin
      --  Iterate through the declarations in the instance, looking for
      --  package renaming declarations that denote instances of formal
      --  packages. Stop when we find the renaming of the current package
      --  itself. The declaration for a formal package without a box is
      --  followed by an internal entity that repeats the instantiation.

      E := First_Entity (P_Id);
      while Present (E) loop
         if Ekind (E) = E_Package then
            if Renamed_Object (E) = P_Id then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            elsif not Box_Present (Parent (Associated_Formal_Package (E))) then
               Formal_P := Next_Entity (E);
               Check_Formal_Package_Instance (Formal_P, E);
            end if;
         end if;

         Next_Entity (E);
      end loop;
   end Check_Formal_Packages;

   ---------------------------------
   -- Check_Forward_Instantiation --
   ---------------------------------

   procedure Check_Forward_Instantiation (Decl : Node_Id) is
      S        : Entity_Id;
      Gen_Comp : Entity_Id := Cunit_Entity (Get_Source_Unit (Decl));

   begin
      --  The instantiation appears before the generic body if we are in the
      --  scope of the unit containing the generic, either in its spec or in
      --  the package body. and before the generic body.

      if Ekind (Gen_Comp) = E_Package_Body then
         Gen_Comp := Spec_Entity (Gen_Comp);
      end if;

      if In_Open_Scopes (Gen_Comp)
        and then No (Corresponding_Body (Decl))
      then
         S := Current_Scope;

         while Present (S)
           and then not Is_Compilation_Unit (S)
           and then not Is_Child_Unit (S)
         loop
            if Ekind (S) = E_Package then
               Set_Has_Forward_Instantiation (S);
            end if;

            S := Scope (S);
         end loop;
      end if;
   end Check_Forward_Instantiation;

   ---------------------------
   -- Check_Generic_Actuals --
   ---------------------------

   --  The visibility of the actuals may be different between the
   --  point of generic instantiation and the instantiation of the body.

   procedure Check_Generic_Actuals
     (Instance      : Entity_Id;
      Is_Formal_Box : Boolean)
   is
      E      : Entity_Id;
      Astype : Entity_Id;

      function Denotes_Previous_Actual (Typ : Entity_Id) return Boolean;
      --  For a formal that is an array type, the component type is often
      --  a previous formal in the same unit. The privacy status of the
      --  component type will have been examined earlier in the traversal
      --  of the corresponding actuals, and this status should not be
      --  modified for the array type itself.
      --  To detect this case we have to rescan the list of formals, which
      --  is usually short enough to ignore the resulting inefficiency.

      function Denotes_Previous_Actual (Typ : Entity_Id) return Boolean is
         Prev : Entity_Id;
      begin
         Prev := First_Entity (Instance);
         while Present (Prev) loop
            if Is_Type (Prev)
              and then Nkind (Parent (Prev)) = N_Subtype_Declaration
              and then Is_Entity_Name (Subtype_Indication (Parent (Prev)))
              and then Entity (Subtype_Indication (Parent (Prev))) = Typ
            then
               return True;
            elsif Prev = E then
               return False;
            else
               Next_Entity (Prev);
            end if;
         end loop;
         return False;
      end Denotes_Previous_Actual;

   --  Start of processing for Check_Generic_Actuals

   begin
      E := First_Entity (Instance);
      while Present (E) loop
         if Is_Type (E)
           and then Nkind (Parent (E)) = N_Subtype_Declaration
           and then Scope (Etype (E)) /= Instance
           and then Is_Entity_Name (Subtype_Indication (Parent (E)))
         then
            if Is_Array_Type (E)
              and then Denotes_Previous_Actual (Component_Type (E))
            then
               null;
            else
               Check_Private_View (Subtype_Indication (Parent (E)));
            end if;
            Set_Is_Generic_Actual_Type (E, True);
            Set_Is_Hidden (E, False);
            Set_Is_Potentially_Use_Visible (E,
              In_Use (Instance));

            --  We constructed the generic actual type as a subtype of
            --  the supplied type. This means that it normally would not
            --  inherit subtype specific attributes of the actual, which
            --  is wrong for the generic case.

            Astype := Ancestor_Subtype (E);

            if No (Astype) then

               --  can happen when E is an itype that is the full view of
               --  a private type completed, e.g. with a constrained array.

               Astype := Base_Type (E);
            end if;

            Set_Size_Info      (E,                (Astype));
            Set_RM_Size        (E, RM_Size        (Astype));
            Set_First_Rep_Item (E, First_Rep_Item (Astype));

            if Is_Discrete_Or_Fixed_Point_Type (E) then
               Set_RM_Size (E, RM_Size (Astype));

            --  In  nested instances, the base type of an access actual
            --  may itself be private, and need to be exchanged.

            elsif Is_Access_Type (E)
              and then Is_Private_Type (Etype (E))
            then
               Check_Private_View
                 (New_Occurrence_Of (Etype (E), Sloc (Instance)));
            end if;

         elsif Ekind (E) = E_Package then

            --  If this is the renaming for the current instance, we're done.
            --  Otherwise it is a formal package. If the corresponding formal
            --  was declared with a box, the (instantiations of the) generic
            --  formal part are also visible. Otherwise, ignore the entity
            --  created to validate the actuals.

            if Renamed_Object (E) = Instance then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            --  The visibility of a formal of an enclosing generic is already
            --  correct.

            elsif Denotes_Formal_Package (E) then
               null;

            elsif Present (Associated_Formal_Package (E)) then
               if Box_Present (Parent (Associated_Formal_Package (E))) then
                  Check_Generic_Actuals (Renamed_Object (E), True);
               end if;

               Set_Is_Hidden (E, False);
            end if;

         --  If this is a subprogram instance (in a wrapper package) the
         --  actual is fully visible.

         elsif Is_Wrapper_Package (Instance) then
            Set_Is_Hidden (E, False);

         else
            Set_Is_Hidden (E, not Is_Formal_Box);
         end if;

         Next_Entity (E);
      end loop;
   end Check_Generic_Actuals;

   ------------------------------
   -- Check_Generic_Child_Unit --
   ------------------------------

   procedure Check_Generic_Child_Unit
     (Gen_Id           : Node_Id;
      Parent_Installed : in out Boolean)
   is
      Loc      : constant Source_Ptr := Sloc (Gen_Id);
      Gen_Par  : Entity_Id := Empty;
      Inst_Par : Entity_Id;
      E        : Entity_Id;
      S        : Node_Id;

      function Find_Generic_Child
        (Scop : Entity_Id;
         Id   : Node_Id) return Entity_Id;
      --  Search generic parent for possible child unit with the given name

      function In_Enclosing_Instance return Boolean;
      --  Within an instance of the parent, the child unit may be denoted
      --  by a simple name, or an abbreviated expanded name. Examine enclosing
      --  scopes to locate a possible parent instantiation.

      ------------------------
      -- Find_Generic_Child --
      ------------------------

      function Find_Generic_Child
        (Scop : Entity_Id;
         Id   : Node_Id) return Entity_Id
      is
         E : Entity_Id;

      begin
         --  If entity of name is already set, instance has already been
         --  resolved, e.g. in an enclosing instantiation.

         if Present (Entity (Id)) then
            if Scope (Entity (Id)) = Scop then
               return Entity (Id);
            else
               return Empty;
            end if;

         else
            E := First_Entity (Scop);
            while Present (E) loop
               if Chars (E) = Chars (Id)
                 and then Is_Child_Unit (E)
               then
                  if Is_Child_Unit (E)
                    and then not Is_Visible_Child_Unit (E)
                  then
                     Error_Msg_NE
                       ("generic child unit& is not visible", Gen_Id, E);
                  end if;

                  Set_Entity (Id, E);
                  return E;
               end if;

               Next_Entity (E);
            end loop;

            return Empty;
         end if;
      end Find_Generic_Child;

      ---------------------------
      -- In_Enclosing_Instance --
      ---------------------------

      function In_Enclosing_Instance return Boolean is
         Enclosing_Instance : Node_Id;
         Instance_Decl      : Node_Id;

      begin
         --  We do not inline any call that contains instantiations, except
         --  for instantiations of Unchecked_Conversion, so if we are within
         --  an inlined body the current instance does not require parents.

         if In_Inlined_Body then
            pragma Assert (Chars (Gen_Id) = Name_Unchecked_Conversion);
            return False;
         end if;

         --  Loop to check enclosing scopes

         Enclosing_Instance := Current_Scope;
         while Present (Enclosing_Instance) loop
            Instance_Decl := Unit_Declaration_Node (Enclosing_Instance);

            if Ekind (Enclosing_Instance) = E_Package
              and then Is_Generic_Instance (Enclosing_Instance)
              and then Present
                (Generic_Parent (Specification (Instance_Decl)))
            then
               --  Check whether the generic we are looking for is a child
               --  of this instance.

               E := Find_Generic_Child
                      (Generic_Parent (Specification (Instance_Decl)), Gen_Id);
               exit when Present (E);

            else
               E := Empty;
            end if;

            Enclosing_Instance := Scope (Enclosing_Instance);
         end loop;

         if No (E) then

            --  Not a child unit

            Analyze (Gen_Id);
            return False;

         else
            Rewrite (Gen_Id,
              Make_Expanded_Name (Loc,
                Chars         => Chars (E),
                Prefix        => New_Occurrence_Of (Enclosing_Instance, Loc),
                Selector_Name => New_Occurrence_Of (E, Loc)));

            Set_Entity (Gen_Id, E);
            Set_Etype  (Gen_Id, Etype (E));
            Parent_Installed := False;      -- Already in scope.
            return True;
         end if;
      end In_Enclosing_Instance;

   --  Start of processing for Check_Generic_Child_Unit

   begin
      --  If the name of the generic is given by a selected component, it
      --  may be the name of a generic child unit, and the prefix is the name
      --  of an instance of the parent, in which case the child unit must be
      --  visible. If this instance is not in scope, it must be placed there
      --  and removed after instantiation, because what is being instantiated
      --  is not the original child, but the corresponding child present in
      --  the instance of the parent.

      --  If the child is instantiated within the parent, it can be given by
      --  a simple name. In this case the instance is already in scope, but
      --  the child generic must be recovered from the generic parent as well.

      if Nkind (Gen_Id) = N_Selected_Component then
         S := Selector_Name (Gen_Id);
         Analyze (Prefix (Gen_Id));
         Inst_Par := Entity (Prefix (Gen_Id));

         if Ekind (Inst_Par) = E_Package
           and then Present (Renamed_Object (Inst_Par))
         then
            Inst_Par := Renamed_Object (Inst_Par);
         end if;

         if Ekind (Inst_Par) = E_Package then
            if Nkind (Parent (Inst_Par)) = N_Package_Specification then
               Gen_Par := Generic_Parent (Parent (Inst_Par));

            elsif Nkind (Parent (Inst_Par)) = N_Defining_Program_Unit_Name
              and then
                Nkind (Parent (Parent (Inst_Par))) = N_Package_Specification
            then
               Gen_Par := Generic_Parent (Parent (Parent (Inst_Par)));
            end if;

         elsif Ekind (Inst_Par) = E_Generic_Package
           and then Nkind (Parent (Gen_Id)) = N_Formal_Package_Declaration
         then
            --  A formal package may be a real child package, and not the
            --  implicit instance within a parent. In this case the child is
            --  not visible and has to be retrieved explicitly as well.

            Gen_Par := Inst_Par;
         end if;

         if Present (Gen_Par) then

            --  The prefix denotes an instantiation. The entity itself
            --  may be a nested generic, or a child unit.

            E := Find_Generic_Child (Gen_Par, S);

            if Present (E) then
               Change_Selected_Component_To_Expanded_Name (Gen_Id);
               Set_Entity (Gen_Id, E);
               Set_Etype (Gen_Id, Etype (E));
               Set_Entity (S, E);
               Set_Etype (S, Etype (E));

               --  Indicate that this is a reference to the parent

               if In_Extended_Main_Source_Unit (Gen_Id) then
                  Set_Is_Instantiated (Inst_Par);
               end if;

               --  A common mistake is to replicate the naming scheme of
               --  a hierarchy by instantiating a generic child directly,
               --  rather than the implicit child in a parent instance:

               --  generic .. package Gpar is ..
               --  generic .. package Gpar.Child is ..
               --  package Par is new Gpar ();

               --  with Gpar.Child;
               --  package Par.Child is new Gpar.Child ();
               --                           rather than Par.Child

               --  In this case the instantiation is within Par, which is
               --  an instance, but Gpar does not denote Par because we are
               --  not IN the instance of Gpar, so this is illegal. The test
               --  below recognizes this particular case.

               if Is_Child_Unit (E)
                 and then not Comes_From_Source (Entity (Prefix (Gen_Id)))
                 and then (not In_Instance
                             or else Nkind (Parent (Parent (Gen_Id))) =
                                                         N_Compilation_Unit)
               then
                  Error_Msg_N
                    ("prefix of generic child unit must be instance of parent",
                      Gen_Id);
               end if;

               if not In_Open_Scopes (Inst_Par)
                 and then Nkind (Parent (Gen_Id)) not in
                                           N_Generic_Renaming_Declaration
               then
                  Install_Parent (Inst_Par);
                  Parent_Installed := True;
               end if;

            else
               --  If the generic parent does not contain an entity that
               --  corresponds to the selector, the instance doesn't either.
               --  Analyzing the node will yield the appropriate error message.
               --  If the entity is not a child unit, then it is an inner
               --  generic in the parent.

               Analyze (Gen_Id);
            end if;

         else
            Analyze (Gen_Id);

            if Is_Child_Unit (Entity (Gen_Id))
              and then
                Nkind (Parent (Gen_Id)) not in N_Generic_Renaming_Declaration
              and then not In_Open_Scopes (Inst_Par)
            then
               Install_Parent (Inst_Par);
               Parent_Installed := True;
            end if;
         end if;

      elsif Nkind (Gen_Id) = N_Expanded_Name then

         --  Entity already present, analyze prefix, whose meaning may be
         --  an instance in the current context. If it is an instance of
         --  a relative within another, the proper parent may still have
         --  to be installed, if they are not of the same generation.

         Analyze (Prefix (Gen_Id));
         Inst_Par := Entity (Prefix (Gen_Id));

         if In_Enclosing_Instance then
            null;

         elsif Present (Entity (Gen_Id))
           and then Is_Child_Unit (Entity (Gen_Id))
           and then not In_Open_Scopes (Inst_Par)
         then
            Install_Parent (Inst_Par);
            Parent_Installed := True;
         end if;

      elsif In_Enclosing_Instance then

         --  The child unit is found in some enclosing scope

         null;

      else
         Analyze (Gen_Id);

         --  If this is the renaming of the implicit child in a parent
         --  instance, recover the parent name and install it.

         if Is_Entity_Name (Gen_Id) then
            E := Entity (Gen_Id);

            if Is_Generic_Unit (E)
              and then Nkind (Parent (E)) in N_Generic_Renaming_Declaration
              and then Is_Child_Unit (Renamed_Object (E))
              and then Is_Generic_Unit (Scope (Renamed_Object (E)))
              and then Nkind (Name (Parent (E))) = N_Expanded_Name
            then
               Rewrite (Gen_Id,
                 New_Copy_Tree (Name (Parent (E))));
               Inst_Par := Entity (Prefix (Gen_Id));

               if not In_Open_Scopes (Inst_Par) then
                  Install_Parent (Inst_Par);
                  Parent_Installed := True;
               end if;

            --  If it is a child unit of a non-generic parent, it may be
            --  use-visible and given by a direct name. Install parent as
            --  for other cases.

            elsif Is_Generic_Unit (E)
              and then Is_Child_Unit (E)
              and then
                Nkind (Parent (Gen_Id)) not in N_Generic_Renaming_Declaration
              and then not Is_Generic_Unit (Scope (E))
            then
               if not In_Open_Scopes (Scope (E)) then
                  Install_Parent (Scope (E));
                  Parent_Installed := True;
               end if;
            end if;
         end if;
      end if;
   end Check_Generic_Child_Unit;

   -----------------------------
   -- Check_Hidden_Child_Unit --
   -----------------------------

   procedure Check_Hidden_Child_Unit
     (N           : Node_Id;
      Gen_Unit    : Entity_Id;
      Act_Decl_Id : Entity_Id)
   is
      Gen_Id : constant Node_Id := Name (N);

   begin
      if Is_Child_Unit (Gen_Unit)
        and then Is_Child_Unit (Act_Decl_Id)
        and then Nkind (Gen_Id) = N_Expanded_Name
        and then Entity (Prefix (Gen_Id)) = Scope (Act_Decl_Id)
        and then Chars (Gen_Unit) = Chars (Act_Decl_Id)
      then
         Error_Msg_Node_2 := Scope (Act_Decl_Id);
         Error_Msg_NE
           ("generic unit & is implicitly declared in &",
             Defining_Unit_Name (N), Gen_Unit);
         Error_Msg_N ("\instance must have different name",
           Defining_Unit_Name (N));
      end if;
   end Check_Hidden_Child_Unit;

   ------------------------
   -- Check_Private_View --
   ------------------------

   procedure Check_Private_View (N : Node_Id) is
      T : constant Entity_Id := Etype (N);
      BT : Entity_Id;

   begin
      --  Exchange views if the type was not private in the generic but is
      --  private at the point of instantiation. Do not exchange views if
      --  the scope of the type is in scope. This can happen if both generic
      --  and instance are sibling units, or if type is defined in a parent.
      --  In this case the visibility of the type will be correct for all
      --  semantic checks.

      if Present (T) then
         BT := Base_Type (T);

         if Is_Private_Type (T)
           and then not Has_Private_View (N)
           and then Present (Full_View (T))
           and then not In_Open_Scopes (Scope (T))
         then
            --  In the generic, the full type was visible. Save the
            --  private entity, for subsequent exchange.

            Switch_View (T);

         elsif Has_Private_View (N)
           and then not Is_Private_Type (T)
           and then not Has_Been_Exchanged (T)
           and then Etype (Get_Associated_Node (N)) /= T
         then
            --  Only the private declaration was visible in the generic. If
            --  the type appears in a subtype declaration, the subtype in the
            --  instance must have a view compatible with that of its parent,
            --  which must be exchanged (see corresponding code in Restore_
            --  Private_Views). Otherwise, if the type is defined in a parent
            --  unit, leave full visibility within instance, which is safe.

            if In_Open_Scopes (Scope (Base_Type (T)))
              and then not Is_Private_Type (Base_Type (T))
              and then Comes_From_Source (Base_Type (T))
            then
               null;

            elsif Nkind (Parent (N)) = N_Subtype_Declaration
              or else not In_Private_Part (Scope (Base_Type (T)))
            then
               Prepend_Elmt (T, Exchanged_Views);
               Exchange_Declarations (Etype (Get_Associated_Node (N)));
            end if;

         --  For composite types with inconsistent representation
         --  exchange component types accordingly.

         elsif Is_Access_Type (T)
           and then Is_Private_Type (Designated_Type (T))
           and then not Has_Private_View (N)
           and then Present (Full_View (Designated_Type (T)))
         then
            Switch_View (Designated_Type (T));

         elsif Is_Array_Type (T)
           and then Is_Private_Type (Component_Type (T))
           and then not Has_Private_View (N)
           and then Present (Full_View (Component_Type (T)))
         then
            Switch_View (Component_Type (T));

         elsif Is_Private_Type (T)
           and then Present (Full_View (T))
           and then Is_Array_Type (Full_View (T))
           and then Is_Private_Type (Component_Type (Full_View (T)))
         then
            Switch_View (T);

         --  Finally, a non-private subtype may have a private base type,
         --  which must be exchanged for consistency. This can happen when
         --  instantiating a package body, when the scope stack is empty
         --  but in fact the subtype and the base type are declared in an
         --  enclosing scope.

         elsif not Is_Private_Type (T)
           and then not Has_Private_View (N)
           and then Is_Private_Type (Base_Type (T))
           and then Present (Full_View (BT))
           and then not Is_Generic_Type (BT)
           and then not In_Open_Scopes (BT)
         then
            Prepend_Elmt (Full_View (BT), Exchanged_Views);
            Exchange_Declarations (BT);
         end if;
      end if;
   end Check_Private_View;

   --------------------------
   -- Contains_Instance_Of --
   --------------------------

   function Contains_Instance_Of
     (Inner : Entity_Id;
      Outer : Entity_Id;
      N     : Node_Id) return Boolean
   is
      Elmt : Elmt_Id;
      Scop : Entity_Id;

   begin
      Scop := Outer;

      --  Verify that there are no circular instantiations. We check whether
      --  the unit contains an instance of the current scope or some enclosing
      --  scope (in case one of the instances appears in a subunit). Longer
      --  circularities involving subunits might seem too pathological to
      --  consider, but they were not too pathological for the authors of
      --  DEC bc30vsq, so we loop over all enclosing scopes, and mark all
      --  enclosing generic scopes as containing an instance.

      loop
         --  Within a generic subprogram body, the scope is not generic, to
         --  allow for recursive subprograms. Use the declaration to determine
         --  whether this is a generic unit.

         if Ekind (Scop) = E_Generic_Package
           or else (Is_Subprogram (Scop)
                      and then Nkind (Unit_Declaration_Node (Scop)) =
                                        N_Generic_Subprogram_Declaration)
         then
            Elmt := First_Elmt (Inner_Instances (Inner));

            while Present (Elmt) loop
               if Node (Elmt) = Scop then
                  Error_Msg_Node_2 := Inner;
                  Error_Msg_NE
                    ("circular Instantiation: & instantiated within &!",
                       N, Scop);
                  return True;

               elsif Node (Elmt) = Inner then
                  return True;

               elsif Contains_Instance_Of (Node (Elmt), Scop, N) then
                  Error_Msg_Node_2 := Inner;
                  Error_Msg_NE
                    ("circular Instantiation: & instantiated within &!",
                      N, Node (Elmt));
                  return True;
               end if;

               Next_Elmt (Elmt);
            end loop;

            --  Indicate that Inner is being instantiated within Scop

            Append_Elmt (Inner, Inner_Instances (Scop));
         end if;

         if Scop = Standard_Standard then
            exit;
         else
            Scop := Scope (Scop);
         end if;
      end loop;

      return False;
   end Contains_Instance_Of;

   -----------------------
   -- Copy_Generic_Node --
   -----------------------

   function Copy_Generic_Node
     (N             : Node_Id;
      Parent_Id     : Node_Id;
      Instantiating : Boolean) return Node_Id
   is
      Ent   : Entity_Id;
      New_N : Node_Id;

      function Copy_Generic_Descendant (D : Union_Id) return Union_Id;
      --  Check the given value of one of the Fields referenced by the
      --  current node to determine whether to copy it recursively. The
      --  field may hold a Node_Id, a List_Id, or an Elist_Id, or a plain
      --  value (Sloc, Uint, Char) in which case it need not be copied.

      procedure Copy_Descendants;
      --  Common utility for various nodes

      function Copy_Generic_Elist (E : Elist_Id) return Elist_Id;
      --  Make copy of element list

      function Copy_Generic_List
        (L         : List_Id;
         Parent_Id : Node_Id) return List_Id;
      --  Apply Copy_Node recursively to the members of a node list

      function In_Defining_Unit_Name (Nam : Node_Id) return Boolean;
      --  True if an identifier is part of the defining program unit name
      --  of a child unit. The entity of such an identifier must be kept
      --  (for ASIS use) even though as the name of an enclosing generic
      --   it would otherwise not be preserved in the generic tree.

      ----------------------
      -- Copy_Descendants --
      ----------------------

      procedure Copy_Descendants is

         use Atree.Unchecked_Access;
         --  This code section is part of the implementation of an untyped
         --  tree traversal, so it needs direct access to node fields.

      begin
         Set_Field1 (New_N, Copy_Generic_Descendant (Field1 (N)));
         Set_Field2 (New_N, Copy_Generic_Descendant (Field2 (N)));
         Set_Field3 (New_N, Copy_Generic_Descendant (Field3 (N)));
         Set_Field4 (New_N, Copy_Generic_Descendant (Field4 (N)));
         Set_Field5 (New_N, Copy_Generic_Descendant (Field5 (N)));
      end Copy_Descendants;

      -----------------------------
      -- Copy_Generic_Descendant --
      -----------------------------

      function Copy_Generic_Descendant (D : Union_Id) return Union_Id is
      begin
         if D = Union_Id (Empty) then
            return D;

         elsif D in Node_Range then
            return Union_Id
              (Copy_Generic_Node (Node_Id (D), New_N, Instantiating));

         elsif D in List_Range then
            return Union_Id (Copy_Generic_List (List_Id (D), New_N));

         elsif D in Elist_Range then
            return Union_Id (Copy_Generic_Elist (Elist_Id (D)));

         --  Nothing else is copyable (e.g. Uint values), return as is

         else
            return D;
         end if;
      end Copy_Generic_Descendant;

      ------------------------
      -- Copy_Generic_Elist --
      ------------------------

      function Copy_Generic_Elist (E : Elist_Id) return Elist_Id is
         M : Elmt_Id;
         L : Elist_Id;

      begin
         if Present (E) then
            L := New_Elmt_List;
            M := First_Elmt (E);
            while Present (M) loop
               Append_Elmt
                 (Copy_Generic_Node (Node (M), Empty, Instantiating), L);
               Next_Elmt (M);
            end loop;

            return L;

         else
            return No_Elist;
         end if;
      end Copy_Generic_Elist;

      -----------------------
      -- Copy_Generic_List --
      -----------------------

      function Copy_Generic_List
        (L         : List_Id;
         Parent_Id : Node_Id) return List_Id
      is
         N     : Node_Id;
         New_L : List_Id;

      begin
         if Present (L) then
            New_L := New_List;
            Set_Parent (New_L, Parent_Id);

            N := First (L);
            while Present (N) loop
               Append (Copy_Generic_Node (N, Empty, Instantiating), New_L);
               Next (N);
            end loop;

            return New_L;

         else
            return No_List;
         end if;
      end Copy_Generic_List;

      ---------------------------
      -- In_Defining_Unit_Name --
      ---------------------------

      function In_Defining_Unit_Name (Nam : Node_Id) return Boolean is
      begin
         return Present (Parent (Nam))
           and then (Nkind (Parent (Nam)) = N_Defining_Program_Unit_Name
                      or else
                        (Nkind (Parent (Nam)) = N_Expanded_Name
                          and then In_Defining_Unit_Name (Parent (Nam))));
      end In_Defining_Unit_Name;

   --  Start of processing for Copy_Generic_Node

   begin
      if N = Empty then
         return N;
      end if;

      New_N := New_Copy (N);

      if Instantiating then
         Adjust_Instantiation_Sloc (New_N, S_Adjustment);
      end if;

      if not Is_List_Member (N) then
         Set_Parent (New_N, Parent_Id);
      end if;

      --  If defining identifier, then all fields have been copied already

      if Nkind (New_N) in N_Entity then
         null;

      --  Special casing for identifiers and other entity names and operators

      elsif     Nkind (New_N) = N_Identifier
        or else Nkind (New_N) = N_Character_Literal
        or else Nkind (New_N) = N_Expanded_Name
        or else Nkind (New_N) = N_Operator_Symbol
        or else Nkind (New_N) in N_Op
      then
         if not Instantiating then

            --  Link both nodes in order to assign subsequently the
            --  entity of the copy to the original node, in case this
            --  is a global reference.

            Set_Associated_Node (N, New_N);

            --  If we are within an instantiation, this is a nested generic
            --  that has already been analyzed at the point of definition. We
            --  must preserve references that were global to the enclosing
            --  parent at that point. Other occurrences, whether global or
            --  local to the current generic, must be resolved anew, so we
            --  reset the entity in the generic copy. A global reference has
            --  a smaller depth than the parent, or else the same depth in
            --  case both are distinct compilation units.

            --  It is also possible for Current_Instantiated_Parent to be
            --  defined, and for this not to be a nested generic, namely
            --  if the unit is loaded through Rtsfind. In that case, the
            --  entity of New_N is only a link to the associated node, and
            --  not a defining occurrence.

            --  The entities for parent units in the defining_program_unit
            --  of a generic child unit are established when the context of
            --  the unit is first analyzed, before the generic copy is made.
            --  They are preserved in the copy for use in ASIS queries.

            Ent := Entity (New_N);

            if No (Current_Instantiated_Parent.Gen_Id) then
               if No (Ent)
                 or else Nkind (Ent) /= N_Defining_Identifier
                 or else not In_Defining_Unit_Name (N)
               then
                  Set_Associated_Node (New_N, Empty);
               end if;

            elsif No (Ent)
              or else
                not (Nkind (Ent) = N_Defining_Identifier
                       or else
                     Nkind (Ent) = N_Defining_Character_Literal
                       or else
                     Nkind (Ent) = N_Defining_Operator_Symbol)
              or else No (Scope (Ent))
              or else Scope (Ent) = Current_Instantiated_Parent.Gen_Id
              or else (Scope_Depth (Scope (Ent)) >
                             Scope_Depth (Current_Instantiated_Parent.Gen_Id)
                         and then
                       Get_Source_Unit (Ent) =
                       Get_Source_Unit (Current_Instantiated_Parent.Gen_Id))
            then
               Set_Associated_Node (New_N, Empty);
            end if;

         --  Case of instantiating identifier or some other name or operator

         else
            --  If the associated node is still defined, the entity in
            --  it is global, and must be copied to the instance.
            --  If this copy is being made for a body to inline, it is
            --  applied to an instantiated tree, and the entity is already
            --  present and must be also preserved.

            declare
               Assoc : constant Node_Id := Get_Associated_Node (N);
            begin
               if Present (Assoc) then
                  if Nkind (Assoc) = Nkind (N) then
                     Set_Entity (New_N, Entity (Assoc));
                     Check_Private_View (N);

                  elsif Nkind (Assoc) = N_Function_Call then
                     Set_Entity (New_N, Entity (Name (Assoc)));

                  elsif (Nkind (Assoc) = N_Defining_Identifier
                          or else Nkind (Assoc) = N_Defining_Character_Literal
                          or else Nkind (Assoc) = N_Defining_Operator_Symbol)
                    and then Expander_Active
                  then
                     --  Inlining case: we are copying a tree that contains
                     --  global entities, which are preserved in the copy
                     --  to be used for subsequent inlining.

                     null;

                  else
                     Set_Entity (New_N, Empty);
                  end if;
               end if;
            end;
         end if;

         --  For expanded name, we must copy the Prefix and Selector_Name

         if Nkind (N) = N_Expanded_Name then
            Set_Prefix
              (New_N, Copy_Generic_Node (Prefix (N), New_N, Instantiating));

            Set_Selector_Name (New_N,
              Copy_Generic_Node (Selector_Name (N), New_N, Instantiating));

         --  For operators, we must copy the right operand

         elsif Nkind (N) in N_Op then
            Set_Right_Opnd (New_N,
              Copy_Generic_Node (Right_Opnd (N), New_N, Instantiating));

            --  And for binary operators, the left operand as well

            if Nkind (N) in N_Binary_Op then
               Set_Left_Opnd (New_N,
                 Copy_Generic_Node (Left_Opnd (N), New_N, Instantiating));
            end if;
         end if;

      --  Special casing for stubs

      elsif Nkind (N) in N_Body_Stub then

         --  In any case, we must copy the specification or defining
         --  identifier as appropriate.

         if Nkind (N) = N_Subprogram_Body_Stub then
            Set_Specification (New_N,
              Copy_Generic_Node (Specification (N), New_N, Instantiating));

         else
            Set_Defining_Identifier (New_N,
              Copy_Generic_Node
                (Defining_Identifier (N), New_N, Instantiating));
         end if;

         --  If we are not instantiating, then this is where we load and
         --  analyze subunits, i.e. at the point where the stub occurs. A
         --  more permissivle system might defer this analysis to the point
         --  of instantiation, but this seems to complicated for now.

         if not Instantiating then
            declare
               Subunit_Name : constant Unit_Name_Type := Get_Unit_Name (N);
               Subunit      : Node_Id;
               Unum         : Unit_Number_Type;
               New_Body     : Node_Id;

            begin
               Unum :=
                 Load_Unit
                   (Load_Name  => Subunit_Name,
                    Required   => False,
                    Subunit    => True,
                    Error_Node => N);

               --  If the proper body is not found, a warning message will
               --  be emitted when analyzing the stub, or later at the the
               --  point of instantiation. Here we just leave the stub as is.

               if Unum = No_Unit then
                  Subunits_Missing := True;
                  goto Subunit_Not_Found;
               end if;

               Subunit := Cunit (Unum);

               if Nkind (Unit (Subunit)) /= N_Subunit then
                  Error_Msg_Sloc := Sloc (N);
                  Error_Msg_N
                    ("expected SEPARATE subunit to complete stub at#,"
                       & " found child unit", Subunit);
                  goto Subunit_Not_Found;
               end if;

               --  We must create a generic copy of the subunit, in order
               --  to perform semantic analysis on it, and we must replace
               --  the stub in the original generic unit with the subunit,
               --  in order to preserve non-local references within.

               --  Only the proper body needs to be copied. Library_Unit and
               --  context clause are simply inherited by the generic copy.
               --  Note that the copy (which may be recursive if there are
               --  nested subunits) must be done first, before attaching it
               --  to the enclosing generic.

               New_Body :=
                 Copy_Generic_Node
                   (Proper_Body (Unit (Subunit)),
                    Empty, Instantiating => False);

               --  Now place the original proper body in the original
               --  generic unit. This is a body, not a compilation unit.

               Rewrite (N, Proper_Body (Unit (Subunit)));
               Set_Is_Compilation_Unit (Defining_Entity (N), False);
               Set_Was_Originally_Stub (N);

               --  Finally replace the body of the subunit with its copy,
               --  and make this new subunit into the library unit of the
               --  generic copy, which does not have stubs any longer.

               Set_Proper_Body (Unit (Subunit), New_Body);
               Set_Library_Unit (New_N, Subunit);
               Inherit_Context (Unit (Subunit), N);
            end;

         --  If we are instantiating, this must be an error case, since
         --  otherwise we would have replaced the stub node by the proper
         --  body that corresponds. So just ignore it in the copy (i.e.
         --  we have copied it, and that is good enough).

         else
            null;
         end if;

         <<Subunit_Not_Found>> null;

      --  If the node is a compilation unit, it is the subunit of a stub,
      --  which has been loaded already (see code below). In this case,
      --  the library unit field of N points to the parent unit (which
      --  is a compilation unit) and need not (and cannot!) be copied.

      --  When the proper body of the stub is analyzed, thie library_unit
      --  link is used to establish the proper context (see sem_ch10).

      --  The other fields of a compilation unit are copied as usual

      elsif Nkind (N) = N_Compilation_Unit then

         --  This code can only be executed when not instantiating, because
         --  in the copy made for an instantiation, the compilation unit
         --  node has disappeared at the point that a stub is replaced by
         --  its proper body.

         pragma Assert (not Instantiating);

         Set_Context_Items (New_N,
           Copy_Generic_List (Context_Items (N), New_N));

         Set_Unit (New_N,
           Copy_Generic_Node (Unit (N), New_N, False));

         Set_First_Inlined_Subprogram (New_N,
           Copy_Generic_Node
             (First_Inlined_Subprogram (N), New_N, False));

         Set_Aux_Decls_Node (New_N,
           Copy_Generic_Node (Aux_Decls_Node (N), New_N, False));

      --  For an assignment node, the assignment is known to be semantically
      --  legal if we are instantiating the template. This avoids incorrect
      --  diagnostics in generated code.

      elsif Nkind (N) = N_Assignment_Statement then

         --  Copy name and expression fields in usual manner

         Set_Name (New_N,
           Copy_Generic_Node (Name (N), New_N, Instantiating));

         Set_Expression (New_N,
           Copy_Generic_Node (Expression (N), New_N, Instantiating));

         if Instantiating then
            Set_Assignment_OK (Name (New_N), True);
         end if;

      elsif Nkind (N) = N_Aggregate
              or else Nkind (N) = N_Extension_Aggregate
      then

         if not Instantiating then
            Set_Associated_Node (N, New_N);

         else
            if Present (Get_Associated_Node (N))
              and then Nkind (Get_Associated_Node (N)) = Nkind (N)
            then
               --  In the generic the aggregate has some composite type. If at
               --  the point of instantiation the type has a private view,
               --  install the full view (and that of its ancestors, if any).

               declare
                  T   : Entity_Id := (Etype (Get_Associated_Node (New_N)));
                  Rt  : Entity_Id;

               begin
                  if Present (T)
                    and then Is_Private_Type (T)
                  then
                     Switch_View (T);
                  end if;

                  if Present (T)
                    and then Is_Tagged_Type (T)
                    and then Is_Derived_Type (T)
                  then
                     Rt := Root_Type (T);

                     loop
                        T := Etype (T);

                        if Is_Private_Type (T) then
                           Switch_View (T);
                        end if;

                        exit when T = Rt;
                     end loop;
                  end if;
               end;
            end if;
         end if;

         --  Do not copy the associated node, which points to
         --  the generic copy of the aggregate.

         declare
            use Atree.Unchecked_Access;
            --  This code section is part of the implementation of an untyped
            --  tree traversal, so it needs direct access to node fields.

         begin
            Set_Field1 (New_N, Copy_Generic_Descendant (Field1 (N)));
            Set_Field2 (New_N, Copy_Generic_Descendant (Field2 (N)));
            Set_Field3 (New_N, Copy_Generic_Descendant (Field3 (N)));
            Set_Field5 (New_N, Copy_Generic_Descendant (Field5 (N)));
         end;

      --  Allocators do not have an identifier denoting the access type,
      --  so we must locate it through the expression to check whether
      --  the views are consistent.

      elsif Nkind (N) = N_Allocator
        and then Nkind (Expression (N)) = N_Qualified_Expression
        and then Is_Entity_Name (Subtype_Mark (Expression (N)))
        and then Instantiating
      then
         declare
            T     : constant Node_Id :=
                      Get_Associated_Node (Subtype_Mark (Expression (N)));
            Acc_T : Entity_Id;

         begin
            if Present (T) then
               --  Retrieve the allocator node in the generic copy

               Acc_T := Etype (Parent (Parent (T)));
               if Present (Acc_T)
                 and then Is_Private_Type (Acc_T)
               then
                  Switch_View (Acc_T);
               end if;
            end if;

            Copy_Descendants;
         end;

      --  For a proper body, we must catch the case of a proper body that
      --  replaces a stub. This represents the point at which a separate
      --  compilation unit, and hence template file, may be referenced, so
      --  we must make a new source instantiation entry for the template
      --  of the subunit, and ensure that all nodes in the subunit are
      --  adjusted using this new source instantiation entry.

      elsif Nkind (N) in N_Proper_Body then
         declare
            Save_Adjustment : constant Sloc_Adjustment := S_Adjustment;

         begin
            if Instantiating and then Was_Originally_Stub (N) then
               Create_Instantiation_Source
                 (Instantiation_Node,
                  Defining_Entity (N),
                  False,
                  S_Adjustment);
            end if;

            --  Now copy the fields of the proper body, using the new
            --  adjustment factor if one was needed as per test above.

            Copy_Descendants;

            --  Restore the original adjustment factor in case changed

            S_Adjustment := Save_Adjustment;
         end;

      --  Don't copy Ident or Comment pragmas, since the comment belongs
      --  to the generic unit, not to the instantiating unit.

      elsif Nkind (N) = N_Pragma
        and then Instantiating
      then
         declare
            Prag_Id : constant Pragma_Id := Get_Pragma_Id (Chars (N));

         begin
            if Prag_Id = Pragma_Ident
              or else Prag_Id = Pragma_Comment
            then
               New_N := Make_Null_Statement (Sloc (N));

            else
               Copy_Descendants;
            end if;
         end;

      elsif Nkind (N) = N_Integer_Literal
        or else Nkind (N) = N_Real_Literal
      then
         --  No descendant fields need traversing

         null;

      --  For the remaining nodes, copy recursively their descendants

      else
         Copy_Descendants;

         if Instantiating
           and then Nkind (N) = N_Subprogram_Body
         then
            Set_Generic_Parent (Specification (New_N), N);
         end if;
      end if;

      return New_N;
   end Copy_Generic_Node;

   ----------------------------
   -- Denotes_Formal_Package --
   ----------------------------

   function Denotes_Formal_Package
     (Pack    : Entity_Id;
      On_Exit : Boolean := False) return Boolean
   is
      Par  : Entity_Id;
      Scop : constant Entity_Id := Scope (Pack);
      E    : Entity_Id;

   begin
      if On_Exit then
         Par :=
           Instance_Envs.Table
             (Instance_Envs.Last).Instantiated_Parent.Act_Id;
      else
         Par  := Current_Instantiated_Parent.Act_Id;
      end if;

      if Ekind (Scop) = E_Generic_Package
        or else Nkind (Unit_Declaration_Node (Scop)) =
                                         N_Generic_Subprogram_Declaration
      then
         return True;

      elsif Nkind (Parent (Pack)) = N_Formal_Package_Declaration then
         return True;

      elsif No (Par) then
         return False;

      else
         --  Check whether this package is associated with a formal
         --  package of the enclosing instantiation. Iterate over the
         --  list of renamings.

         E := First_Entity (Par);
         while Present (E) loop
            if Ekind (E) /= E_Package
              or else Nkind (Parent (E)) /= N_Package_Renaming_Declaration
            then
               null;
            elsif Renamed_Object (E) = Par then
               return False;

            elsif Renamed_Object (E) = Pack then
               return True;
            end if;

            Next_Entity (E);
         end loop;

         return False;
      end if;
   end Denotes_Formal_Package;

   -----------------
   -- End_Generic --
   -----------------

   procedure End_Generic is
   begin
      --  ??? More things could be factored out in this
      --  routine. Should probably be done at a later stage.

      Inside_A_Generic := Generic_Flags.Table (Generic_Flags.Last);
      Generic_Flags.Decrement_Last;

      Expander_Mode_Restore;
   end End_Generic;

   ----------------------
   -- Find_Actual_Type --
   ----------------------

   function Find_Actual_Type
     (Typ       : Entity_Id;
      Gen_Scope : Entity_Id) return Entity_Id
   is
      T : Entity_Id;

   begin
      if not Is_Child_Unit (Gen_Scope) then
         return Get_Instance_Of (Typ);

      elsif not Is_Generic_Type (Typ)
        or else Scope (Typ) = Gen_Scope
      then
         return Get_Instance_Of (Typ);

      else
         T := Current_Entity (Typ);
         while Present (T) loop
            if In_Open_Scopes (Scope (T)) then
               return T;
            end if;

            T := Homonym (T);
         end loop;

         return Typ;
      end if;
   end Find_Actual_Type;

   ----------------------------
   -- Freeze_Subprogram_Body --
   ----------------------------

   procedure Freeze_Subprogram_Body
     (Inst_Node : Node_Id;
      Gen_Body  : Node_Id;
      Pack_Id   : Entity_Id)
  is
      F_Node   : Node_Id;
      Gen_Unit : constant Entity_Id := Get_Generic_Entity (Inst_Node);
      Par      : constant Entity_Id := Scope (Gen_Unit);
      Enc_G    : Entity_Id;
      Enc_I    : Node_Id;
      E_G_Id   : Entity_Id;

      function Earlier (N1, N2 : Node_Id) return Boolean;
      --  Yields True if N1 and N2 appear in the same compilation unit,
      --  ignoring subunits, and if N1 is to the left of N2 in a left-to-right
      --  traversal of the tree for the unit.

      function Enclosing_Body (N : Node_Id) return Node_Id;
      --  Find innermost package body that encloses the given node, and which
      --  is not a compilation unit. Freeze nodes for the instance, or for its
      --  enclosing body, may be inserted after the enclosing_body of the
      --  generic unit.

      function Package_Freeze_Node (B : Node_Id) return Node_Id;
      --  Find entity for given package body, and locate or create a freeze
      --  node for it.

      function True_Parent (N : Node_Id) return Node_Id;
      --  For a subunit, return parent of corresponding stub

      -------------
      -- Earlier --
      -------------

      function Earlier (N1, N2 : Node_Id) return Boolean is
         D1 : Integer := 0;
         D2 : Integer := 0;
         P1 : Node_Id := N1;
         P2 : Node_Id := N2;

         procedure Find_Depth (P : in out Node_Id; D : in out Integer);
         --  Find distance from given node to enclosing compilation unit

         ----------------
         -- Find_Depth --
         ----------------

         procedure Find_Depth (P : in out Node_Id; D : in out Integer) is
         begin
            while Present (P)
              and then Nkind (P) /= N_Compilation_Unit
            loop
               P := True_Parent (P);
               D := D + 1;
            end loop;
         end Find_Depth;

      --  Start of procesing for Earlier

      begin
         Find_Depth (P1, D1);
         Find_Depth (P2, D2);

         if P1 /= P2 then
            return False;
         else
            P1 := N1;
            P2 := N2;
         end if;

         while D1 > D2 loop
            P1 := True_Parent (P1);
            D1 := D1 - 1;
         end loop;

         while D2 > D1 loop
            P2 := True_Parent (P2);
            D2 := D2 - 1;
         end loop;

         --  At this point P1 and P2 are at the same distance from the root.
         --  We examine their parents until we find a common declarative
         --  list, at which point we can establish their relative placement
         --  by comparing their ultimate slocs. If we reach the root,
         --  N1 and N2 do not descend from the same declarative list (e.g.
         --  one is nested in the declarative part and the other is in a block
         --  in the statement part) and the earlier one is already frozen.

         while not Is_List_Member (P1)
           or else not Is_List_Member (P2)
           or else List_Containing (P1) /= List_Containing (P2)
         loop
            P1 := True_Parent (P1);
            P2 := True_Parent (P2);

            if Nkind (Parent (P1)) = N_Subunit then
               P1 := Corresponding_Stub (Parent (P1));
            end if;

            if Nkind (Parent (P2)) = N_Subunit then
               P2 := Corresponding_Stub (Parent (P2));
            end if;

            if P1 = P2 then
               return False;
            end if;
         end loop;

         return
           Top_Level_Location (Sloc (P1)) < Top_Level_Location (Sloc (P2));
      end Earlier;

      --------------------
      -- Enclosing_Body --
      --------------------

      function Enclosing_Body (N : Node_Id) return Node_Id is
         P : Node_Id := Parent (N);

      begin
         while Present (P)
           and then Nkind (Parent (P)) /= N_Compilation_Unit
         loop
            if Nkind (P) = N_Package_Body then

               if Nkind (Parent (P)) = N_Subunit then
                  return Corresponding_Stub (Parent (P));
               else
                  return P;
               end if;
            end if;

            P := True_Parent (P);
         end loop;

         return Empty;
      end Enclosing_Body;

      -------------------------
      -- Package_Freeze_Node --
      -------------------------

      function Package_Freeze_Node (B : Node_Id) return Node_Id is
         Id : Entity_Id;

      begin
         if Nkind (B) = N_Package_Body then
            Id := Corresponding_Spec (B);

         else pragma Assert (Nkind (B) = N_Package_Body_Stub);
            Id := Corresponding_Spec (Proper_Body (Unit (Library_Unit (B))));
         end if;

         Ensure_Freeze_Node (Id);
         return Freeze_Node (Id);
      end Package_Freeze_Node;

      -----------------
      -- True_Parent --
      -----------------

      function True_Parent (N : Node_Id) return Node_Id is
      begin
         if Nkind (Parent (N)) = N_Subunit then
            return Parent (Corresponding_Stub (Parent (N)));
         else
            return Parent (N);
         end if;
      end True_Parent;

   --  Start of processing of Freeze_Subprogram_Body

   begin
      --  If the instance and the generic body appear within the same
      --  unit, and the instance preceeds the generic, the freeze node for
      --  the instance must appear after that of the generic. If the generic
      --  is nested within another instance I2, then current instance must
      --  be frozen after I2. In both cases, the freeze nodes are those of
      --  enclosing packages. Otherwise, the freeze node is placed at the end
      --  of the current declarative part.

      Enc_G  := Enclosing_Body (Gen_Body);
      Enc_I  := Enclosing_Body (Inst_Node);
      Ensure_Freeze_Node (Pack_Id);
      F_Node := Freeze_Node (Pack_Id);

      if Is_Generic_Instance (Par)
        and then Present (Freeze_Node (Par))
        and then
          In_Same_Declarative_Part (Freeze_Node (Par), Inst_Node)
      then
         if ABE_Is_Certain (Get_Package_Instantiation_Node (Par)) then

            --  The parent was a premature instantiation. Insert freeze
            --  node at the end the current declarative part.

            Insert_After_Last_Decl (Inst_Node, F_Node);

         else
            Insert_After (Freeze_Node (Par), F_Node);
         end if;

      --  The body enclosing the instance should be frozen after the body
      --  that includes the generic, because the body of the instance may
      --  make references to entities therein. If the two are not in the
      --  same declarative part, or if the one enclosing the instance is
      --  frozen already, freeze the instance at the end of the current
      --  declarative part.

      elsif Is_Generic_Instance (Par)
        and then Present (Freeze_Node (Par))
        and then Present (Enc_I)
      then
         if In_Same_Declarative_Part (Freeze_Node (Par), Enc_I)
           or else
             (Nkind (Enc_I) = N_Package_Body
               and then
             In_Same_Declarative_Part (Freeze_Node (Par), Parent (Enc_I)))
         then
            --  The enclosing package may contain several instances. Rather
            --  than computing the earliest point at which to insert its
            --  freeze node, we place it at the end of the declarative part
            --  of the parent of the generic.

            Insert_After_Last_Decl
              (Freeze_Node (Par), Package_Freeze_Node (Enc_I));
         end if;

         Insert_After_Last_Decl (Inst_Node, F_Node);

      elsif Present (Enc_G)
        and then Present (Enc_I)
        and then Enc_G /= Enc_I
        and then Earlier (Inst_Node, Gen_Body)
      then
         if Nkind (Enc_G) = N_Package_Body then
            E_G_Id := Corresponding_Spec (Enc_G);
         else pragma Assert (Nkind (Enc_G) = N_Package_Body_Stub);
            E_G_Id :=
              Corresponding_Spec (Proper_Body (Unit (Library_Unit (Enc_G))));
         end if;

         --  Freeze package that encloses instance, and place node after
         --  package that encloses generic. If enclosing package is already
         --  frozen we have to assume it is at the proper place. This may
         --  be a potential ABE that requires dynamic checking.

         Insert_After_Last_Decl (Enc_G, Package_Freeze_Node (Enc_I));

         --  Freeze enclosing subunit before instance

         Ensure_Freeze_Node (E_G_Id);

         if not Is_List_Member (Freeze_Node (E_G_Id)) then
            Insert_After (Enc_G, Freeze_Node (E_G_Id));
         end if;

         Insert_After_Last_Decl (Inst_Node, F_Node);

      else
         --  If none of the above, insert freeze node at the end of the
         --  current declarative part.

         Insert_After_Last_Decl (Inst_Node, F_Node);
      end if;
   end Freeze_Subprogram_Body;

   ----------------
   -- Get_Gen_Id --
   ----------------

   function Get_Gen_Id (E : Assoc_Ptr) return Entity_Id is
   begin
      return Generic_Renamings.Table (E).Gen_Id;
   end Get_Gen_Id;

   ---------------------
   -- Get_Instance_Of --
   ---------------------

   function Get_Instance_Of (A : Entity_Id) return Entity_Id is
      Res : constant Assoc_Ptr := Generic_Renamings_HTable.Get (A);

   begin
      if Res /= Assoc_Null then
         return Generic_Renamings.Table (Res).Act_Id;
      else
         --  On exit, entity is not instantiated: not a generic parameter,
         --  or else parameter of an inner generic unit.

         return A;
      end if;
   end Get_Instance_Of;

   ------------------------------------
   -- Get_Package_Instantiation_Node --
   ------------------------------------

   function Get_Package_Instantiation_Node (A : Entity_Id) return Node_Id is
      Decl : Node_Id := Unit_Declaration_Node (A);
      Inst : Node_Id;

   begin
      --  If the Package_Instantiation attribute has been set on the package
      --  entity, then use it directly when it (or its Original_Node) refers
      --  to an N_Package_Instantiation node. In principle it should be
      --  possible to have this field set in all cases, which should be
      --  investigated, and would allow this function to be significantly
      --  simplified. ???

      if Present (Package_Instantiation (A)) then
         if Nkind (Package_Instantiation (A)) = N_Package_Instantiation then
            return Package_Instantiation (A);

         elsif Nkind (Original_Node (Package_Instantiation (A)))
                 = N_Package_Instantiation
         then
            return Original_Node (Package_Instantiation (A));
         end if;
      end if;

      --  If the instantiation is a compilation unit that does not need a
      --  body then the instantiation node has been rewritten as a package
      --  declaration for the instance, and we return the original node.

      --  If it is a compilation unit and the instance node has not been
      --  rewritten, then it is still the unit of the compilation. Finally,
      --  if a body is present, this is a parent of the main unit whose body
      --  has been compiled for inlining purposes, and the instantiation node
      --  has been rewritten with the instance body.

      --  Otherwise the instantiation node appears after the declaration.
      --  If the entity is a formal package, the declaration may have been
      --  rewritten as a generic declaration (in the case of a formal with a
      --  box) or left as a formal package declaration if it has actuals, and
      --  is found with a forward search.

      if Nkind (Parent (Decl)) = N_Compilation_Unit then
         if Nkind (Decl) = N_Package_Declaration
           and then Present (Corresponding_Body (Decl))
         then
            Decl := Unit_Declaration_Node (Corresponding_Body (Decl));
         end if;

         if Nkind (Original_Node (Decl)) = N_Package_Instantiation then
            return Original_Node (Decl);
         else
            return Unit (Parent (Decl));
         end if;

      elsif Nkind (Decl) = N_Generic_Package_Declaration
        and then Nkind (Original_Node (Decl)) = N_Formal_Package_Declaration
      then
         return Original_Node (Decl);

      else
         Inst := Next (Decl);
         while Nkind (Inst) /= N_Package_Instantiation
           and then Nkind (Inst) /= N_Formal_Package_Declaration
         loop
            Next (Inst);
         end loop;

         return Inst;
      end if;
   end Get_Package_Instantiation_Node;

   ------------------------
   -- Has_Been_Exchanged --
   ------------------------

   function Has_Been_Exchanged (E : Entity_Id) return Boolean is
      Next : Elmt_Id := First_Elmt (Exchanged_Views);

   begin
      while Present (Next) loop
         if Full_View (Node (Next)) = E then
            return True;
         end if;

         Next_Elmt (Next);
      end loop;

      return False;
   end Has_Been_Exchanged;

   ----------
   -- Hash --
   ----------

   function Hash (F : Entity_Id) return HTable_Range is
   begin
      return HTable_Range (F mod HTable_Size);
   end Hash;

   ------------------------
   -- Hide_Current_Scope --
   ------------------------

   procedure Hide_Current_Scope is
      C : constant Entity_Id := Current_Scope;
      E : Entity_Id;

   begin
      Set_Is_Hidden_Open_Scope (C);
      E := First_Entity (C);

      while Present (E) loop
         if Is_Immediately_Visible (E) then
            Set_Is_Immediately_Visible (E, False);
            Append_Elmt (E, Hidden_Entities);
         end if;

         Next_Entity (E);
      end loop;

      --  Make the scope name invisible as well. This is necessary, but
      --  might conflict with calls to Rtsfind later on, in case the scope
      --  is a predefined one. There is no clean solution to this problem, so
      --  for now we depend on the user not redefining Standard itself in one
      --  of the parent units.

      if Is_Immediately_Visible (C)
        and then C /= Standard_Standard
      then
         Set_Is_Immediately_Visible (C, False);
         Append_Elmt (C, Hidden_Entities);
      end if;

   end Hide_Current_Scope;

   --------------
   -- Init_Env --
   --------------

   procedure Init_Env is
      Saved : Instance_Env;

   begin
      Saved.Ada_Version          := Ada_Version;
      Saved.Ada_Version_Explicit := Ada_Version_Explicit;
      Saved.Instantiated_Parent  := Current_Instantiated_Parent;
      Saved.Exchanged_Views      := Exchanged_Views;
      Saved.Hidden_Entities      := Hidden_Entities;
      Saved.Current_Sem_Unit     := Current_Sem_Unit;
      Saved.Parent_Unit_Visible  := Parent_Unit_Visible;
      Saved.Instance_Parent_Unit := Instance_Parent_Unit;
      Instance_Envs.Increment_Last;
      Instance_Envs.Table (Instance_Envs.Last) := Saved;

      Exchanged_Views := New_Elmt_List;
      Hidden_Entities := New_Elmt_List;

      --  Make dummy entry for Instantiated parent. If generic unit is
      --  legal, this is set properly in Set_Instance_Env.

      Current_Instantiated_Parent :=
        (Current_Scope, Current_Scope, Assoc_Null);
   end Init_Env;

   ------------------------------
   -- In_Same_Declarative_Part --
   ------------------------------

   function In_Same_Declarative_Part
     (F_Node : Node_Id;
      Inst   : Node_Id) return Boolean
   is
      Decls : constant Node_Id := Parent (F_Node);
      Nod   : Node_Id := Parent (Inst);

   begin
      while Present (Nod) loop
         if Nod = Decls then
            return True;

         elsif Nkind (Nod) = N_Subprogram_Body
           or else Nkind (Nod) = N_Package_Body
           or else Nkind (Nod) = N_Task_Body
           or else Nkind (Nod) = N_Protected_Body
           or else Nkind (Nod) = N_Block_Statement
         then
            return False;

         elsif Nkind (Nod) = N_Subunit then
            Nod :=  Corresponding_Stub (Nod);

         elsif Nkind (Nod) = N_Compilation_Unit then
            return False;
         else
            Nod := Parent (Nod);
         end if;
      end loop;

      return False;
   end In_Same_Declarative_Part;

   ---------------------
   -- In_Main_Context --
   ---------------------

   function In_Main_Context (E : Entity_Id) return Boolean is
      Context : List_Id;
      Clause  : Node_Id;
      Nam     : Node_Id;

   begin
      if not Is_Compilation_Unit (E)
        or else Ekind (E) /= E_Package
        or else In_Private_Part (E)
      then
         return False;
      end if;

      Context := Context_Items (Cunit (Main_Unit));

      Clause  := First (Context);
      while Present (Clause) loop
         if Nkind (Clause) = N_With_Clause then
            Nam := Name (Clause);

            --  If the current scope is part of the context of the main unit,
            --  analysis of the corresponding with_clause is not complete, and
            --  the entity is not set. We use the Chars field directly, which
            --  might produce false positives in rare cases, but guarantees
            --  that we produce all the instance bodies we will need.

            if (Nkind (Nam) = N_Identifier
                 and then Chars (Nam) = Chars (E))
              or else (Nkind (Nam) = N_Selected_Component
                        and then Chars (Selector_Name (Nam)) = Chars (E))
            then
               return True;
            end if;
         end if;

         Next (Clause);
      end loop;

      return False;
   end In_Main_Context;

   ---------------------
   -- Inherit_Context --
   ---------------------

   procedure Inherit_Context (Gen_Decl : Node_Id; Inst : Node_Id) is
      Current_Context : List_Id;
      Current_Unit    : Node_Id;
      Item            : Node_Id;
      New_I           : Node_Id;

   begin
      if Nkind (Parent (Gen_Decl)) = N_Compilation_Unit then

         --  The inherited context is attached to the enclosing compilation
         --  unit. This is either the main unit, or the declaration for the
         --  main unit (in case the instantation appears within the package
         --  declaration and the main unit is its body).

         Current_Unit := Parent (Inst);
         while Present (Current_Unit)
           and then Nkind (Current_Unit) /= N_Compilation_Unit
         loop
            Current_Unit := Parent (Current_Unit);
         end loop;

         Current_Context := Context_Items (Current_Unit);

         Item := First (Context_Items (Parent (Gen_Decl)));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause then
               New_I := New_Copy (Item);
               Set_Implicit_With (New_I, True);
               Append (New_I, Current_Context);
            end if;

            Next (Item);
         end loop;
      end if;
   end Inherit_Context;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Generic_Renamings.Init;
      Instance_Envs.Init;
      Generic_Flags.Init;
      Generic_Renamings_HTable.Reset;
      Circularity_Detected := False;
      Exchanged_Views      := No_Elist;
      Hidden_Entities      := No_Elist;
   end Initialize;

   ----------------------------
   -- Insert_After_Last_Decl --
   ----------------------------

   procedure Insert_After_Last_Decl (N : Node_Id; F_Node : Node_Id) is
      L : List_Id          := List_Containing (N);
      P : constant Node_Id := Parent (L);

   begin
      if not Is_List_Member (F_Node) then
         if Nkind (P) = N_Package_Specification
           and then L = Visible_Declarations (P)
           and then Present (Private_Declarations (P))
           and then not Is_Empty_List (Private_Declarations (P))
         then
            L := Private_Declarations (P);
         end if;

         Insert_After (Last (L), F_Node);
      end if;
   end Insert_After_Last_Decl;

   ------------------
   -- Install_Body --
   ------------------

   procedure Install_Body
     (Act_Body : Node_Id;
      N        : Node_Id;
      Gen_Body : Node_Id;
      Gen_Decl : Node_Id)
   is
      Act_Id    : constant Entity_Id := Corresponding_Spec (Act_Body);
      Act_Unit  : constant Node_Id   := Unit (Cunit (Get_Source_Unit (N)));
      Gen_Id    : constant Entity_Id := Corresponding_Spec (Gen_Body);
      Par       : constant Entity_Id := Scope (Gen_Id);
      Gen_Unit  : constant Node_Id :=
                    Unit (Cunit (Get_Source_Unit (Gen_Decl)));
      Orig_Body : Node_Id := Gen_Body;
      F_Node    : Node_Id;
      Body_Unit : Node_Id;

      Must_Delay : Boolean;

      function Enclosing_Subp (Id : Entity_Id) return Entity_Id;
      --  Find subprogram (if any) that encloses instance and/or generic body

      function True_Sloc (N : Node_Id) return Source_Ptr;
      --  If the instance is nested inside a generic unit, the Sloc of the
      --  instance indicates the place of the original definition, not the
      --  point of the current enclosing instance. Pending a better usage of
      --  Slocs to indicate instantiation places, we determine the place of
      --  origin of a node by finding the maximum sloc of any ancestor node.
      --  Why is this not equivalent to Top_Level_Location ???

      --------------------
      -- Enclosing_Subp --
      --------------------

      function Enclosing_Subp (Id : Entity_Id) return Entity_Id is
         Scop : Entity_Id := Scope (Id);

      begin
         while Scop /= Standard_Standard
           and then not Is_Overloadable (Scop)
         loop
            Scop := Scope (Scop);
         end loop;

         return Scop;
      end Enclosing_Subp;

      ---------------
      -- True_Sloc --
      ---------------

      function True_Sloc (N : Node_Id) return Source_Ptr is
         Res : Source_Ptr;
         N1  : Node_Id;

      begin
         Res := Sloc (N);
         N1 := N;
         while Present (N1) and then N1 /= Act_Unit loop
            if Sloc (N1) > Res then
               Res := Sloc (N1);
            end if;

            N1 := Parent (N1);
         end loop;

         return Res;
      end True_Sloc;

   --  Start of processing for Install_Body

   begin
      --  If the body is a subunit, the freeze point is the corresponding
      --  stub in the current compilation, not the subunit itself.

      if Nkind (Parent (Gen_Body)) = N_Subunit then
         Orig_Body :=  Corresponding_Stub (Parent (Gen_Body));
      else
         Orig_Body := Gen_Body;
      end if;

      Body_Unit := Unit (Cunit (Get_Source_Unit (Orig_Body)));

      --  If the instantiation and the generic definition appear in the
      --  same package declaration, this is an early instantiation.
      --  If they appear in the same declarative part, it is an early
      --  instantiation only if the generic body appears textually later,
      --  and the generic body is also in the main unit.

      --  If instance is nested within a subprogram, and the generic body is
      --  not, the instance is delayed because the enclosing body is. If
      --  instance and body are within the same scope, or the same sub-
      --  program body, indicate explicitly that the instance is delayed.

      Must_Delay :=
        (Gen_Unit = Act_Unit
          and then ((Nkind (Gen_Unit) = N_Package_Declaration)
                      or else Nkind (Gen_Unit) = N_Generic_Package_Declaration
                      or else (Gen_Unit = Body_Unit
                                and then True_Sloc (N) < Sloc (Orig_Body)))
          and then Is_In_Main_Unit (Gen_Unit)
          and then (Scope (Act_Id) = Scope (Gen_Id)
                      or else
                    Enclosing_Subp (Act_Id) = Enclosing_Subp (Gen_Id)));

      --  If this is an early instantiation, the freeze node is placed after
      --  the generic body. Otherwise, if the generic appears in an instance,
      --  we cannot freeze the current instance until the outer one is frozen.
      --  This is only relevant if the current instance is nested within some
      --  inner scope not itself within the outer instance. If this scope is
      --  a package body in the same declarative part as the outer instance,
      --  then that body needs to be frozen after the outer instance. Finally,
      --  if no delay is needed, we place the freeze node at the end of the
      --  current declarative part.

      if Expander_Active then
         Ensure_Freeze_Node (Act_Id);
         F_Node := Freeze_Node (Act_Id);

         if Must_Delay then
            Insert_After (Orig_Body, F_Node);

         elsif Is_Generic_Instance (Par)
           and then Present (Freeze_Node (Par))
           and then Scope (Act_Id) /= Par
         then
            --  Freeze instance of inner generic after instance of enclosing
            --  generic.

            if In_Same_Declarative_Part (Freeze_Node (Par), N) then
               Insert_After (Freeze_Node (Par), F_Node);

            --  Freeze package enclosing instance of inner generic after
            --  instance of enclosing generic.

            elsif Nkind (Parent (N)) = N_Package_Body
              and then In_Same_Declarative_Part (Freeze_Node (Par), Parent (N))
            then

               declare
                  Enclosing : constant Entity_Id :=
                                Corresponding_Spec (Parent (N));

               begin
                  Insert_After_Last_Decl (N, F_Node);
                  Ensure_Freeze_Node (Enclosing);

                  if not Is_List_Member (Freeze_Node (Enclosing)) then
                     Insert_After (Freeze_Node (Par), Freeze_Node (Enclosing));
                  end if;
               end;

            else
               Insert_After_Last_Decl (N, F_Node);
            end if;

         else
            Insert_After_Last_Decl (N, F_Node);
         end if;
      end if;

      Set_Is_Frozen (Act_Id);
      Insert_Before (N, Act_Body);
      Mark_Rewrite_Insertion (Act_Body);
   end Install_Body;

   --------------------
   -- Install_Parent --
   --------------------

   procedure Install_Parent (P : Entity_Id; In_Body : Boolean := False) is
      Ancestors : constant Elist_Id  := New_Elmt_List;
      S         : constant Entity_Id := Current_Scope;
      Inst_Par  : Entity_Id;
      First_Par : Entity_Id;
      Inst_Node : Node_Id;
      Gen_Par   : Entity_Id;
      First_Gen : Entity_Id;
      Elmt      : Elmt_Id;

      procedure Install_Formal_Packages (Par : Entity_Id);
      --  If any of the formals of the parent are formal packages with box,
      --  their formal parts are visible in the parent and thus in the child
      --  unit as well. Analogous to what is done in Check_Generic_Actuals
      --  for the unit itself.

      procedure Install_Noninstance_Specs (Par : Entity_Id);
      --  Install the scopes of noninstance parent units ending with Par

      procedure Install_Spec (Par : Entity_Id);
      --  The child unit is within the declarative part of the parent, so
      --  the declarations within the parent are immediately visible.

      -----------------------------
      -- Install_Formal_Packages --
      -----------------------------

      procedure Install_Formal_Packages (Par : Entity_Id) is
         E : Entity_Id;

      begin
         E := First_Entity (Par);
         while Present (E) loop
            if Ekind (E) = E_Package
              and then Nkind (Parent (E)) = N_Package_Renaming_Declaration
            then
               --  If this is the renaming for the parent instance, done

               if Renamed_Object (E) = Par then
                  exit;

               --  The visibility of a formal of an enclosing generic is
               --  already correct.

               elsif Denotes_Formal_Package (E) then
                  null;

               elsif Present (Associated_Formal_Package (E))
                 and then Box_Present (Parent (Associated_Formal_Package (E)))
               then
                  Check_Generic_Actuals (Renamed_Object (E), True);
                  Set_Is_Hidden (E, False);
               end if;
            end if;

            Next_Entity (E);
         end loop;
      end Install_Formal_Packages;

      -------------------------------
      -- Install_Noninstance_Specs --
      -------------------------------

      procedure Install_Noninstance_Specs (Par : Entity_Id) is
      begin
         if Present (Par)
           and then Par /= Standard_Standard
           and then not In_Open_Scopes (Par)
         then
            Install_Noninstance_Specs (Scope (Par));
            Install_Spec (Par);
         end if;
      end Install_Noninstance_Specs;

      ------------------
      -- Install_Spec --
      ------------------

      procedure Install_Spec (Par : Entity_Id) is
         Spec : constant Node_Id :=
                  Specification (Unit_Declaration_Node (Par));

      begin
         --  If this parent of the child instance is a top-level unit,
         --  then record the unit and its visibility for later resetting
         --  in Remove_Parent. We exclude units that are generic instances,
         --  as we only want to record this information for the ultimate
         --  top-level noninstance parent (is that always correct???).

         if Scope (Par) = Standard_Standard
           and then not Is_Generic_Instance (Par)
         then
            Parent_Unit_Visible := Is_Immediately_Visible (Par);
            Instance_Parent_Unit := Par;
         end if;

         --  Open the parent scope and make it and its declarations visible.
         --  If this point is not within a body, then only the visible
         --  declarations should be made visible, and installation of the
         --  private declarations is deferred until the appropriate point
         --  within analysis of the spec being instantiated (see the handling
         --  of parent visibility in Analyze_Package_Specification). This is
         --  relaxed in the case where the parent unit is Ada.Tags, to avoid
         --  private view problems that occur when compiling instantiations of
         --  a generic child of that package (Generic_Dispatching_Constructor).
         --  If the instance freezes a tagged type, inlinings of operations
         --  from Ada.Tags may need the full view of type Tag. If inlining
         --  took proper account of establishing visibility of inlined
         --  subprograms' parents then it should be possible to remove this
         --  special check. ???

         New_Scope (Par);
         Set_Is_Immediately_Visible   (Par);
         Install_Visible_Declarations (Par);
         Set_Use (Visible_Declarations (Spec));

         if In_Body or else Is_RTU (Par, Ada_Tags) then
            Install_Private_Declarations (Par);
            Set_Use (Private_Declarations (Spec));
         end if;
      end Install_Spec;

   --  Start of processing for Install_Parent

   begin
      --  We need to install the parent instance to compile the instantiation
      --  of the child, but the child instance must appear in the current
      --  scope. Given that we cannot place the parent above the current
      --  scope in the scope stack, we duplicate the current scope and unstack
      --  both after the instantiation is complete.

      --  If the parent is itself the instantiation of a child unit, we must
      --  also stack the instantiation of its parent, and so on. Each such
      --  ancestor is the prefix of the name in a prior instantiation.

      --  If this is a nested instance, the parent unit itself resolves to
      --  a renaming of the parent instance, whose declaration we need.

      --  Finally, the parent may be a generic (not an instance) when the
      --  child unit appears as a formal package.

      Inst_Par := P;

      if Present (Renamed_Entity (Inst_Par)) then
         Inst_Par := Renamed_Entity (Inst_Par);
      end if;

      First_Par := Inst_Par;

      Gen_Par :=
        Generic_Parent (Specification (Unit_Declaration_Node (Inst_Par)));

      First_Gen := Gen_Par;

      while Present (Gen_Par)
        and then Is_Child_Unit (Gen_Par)
      loop
         --  Load grandparent instance as well

         Inst_Node := Get_Package_Instantiation_Node (Inst_Par);

         if Nkind (Name (Inst_Node)) = N_Expanded_Name then
            Inst_Par := Entity (Prefix (Name (Inst_Node)));

            if Present (Renamed_Entity (Inst_Par)) then
               Inst_Par := Renamed_Entity (Inst_Par);
            end if;

            Gen_Par :=
              Generic_Parent
                (Specification (Unit_Declaration_Node (Inst_Par)));

            if Present (Gen_Par) then
               Prepend_Elmt (Inst_Par, Ancestors);

            else
               --  Parent is not the name of an instantiation

               Install_Noninstance_Specs (Inst_Par);

               exit;
            end if;

         else
            --  Previous error

            exit;
         end if;
      end loop;

      if Present (First_Gen) then
         Append_Elmt (First_Par, Ancestors);

      else
         Install_Noninstance_Specs (First_Par);
      end if;

      if not Is_Empty_Elmt_List (Ancestors) then
         Elmt := First_Elmt (Ancestors);

         while Present (Elmt) loop
            Install_Spec (Node (Elmt));
            Install_Formal_Packages (Node (Elmt));

            Next_Elmt (Elmt);
         end loop;
      end if;

      if not In_Body then
         New_Scope (S);
      end if;
   end Install_Parent;

   --------------------------------
   -- Instantiate_Formal_Package --
   --------------------------------

   function Instantiate_Formal_Package
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return List_Id
   is
      Loc         : constant Source_Ptr := Sloc (Actual);
      Actual_Pack : Entity_Id;
      Formal_Pack : Entity_Id;
      Gen_Parent  : Entity_Id;
      Decls       : List_Id;
      Nod         : Node_Id;
      Parent_Spec : Node_Id;

      procedure Find_Matching_Actual
       (F    : Node_Id;
        Act  : in out Entity_Id);
      --  We need to associate each formal entity in the formal package
      --  with the corresponding entity in the actual package. The actual
      --  package has been analyzed and possibly expanded, and as a result
      --  there is no one-to-one correspondence between the two lists (for
      --  example, the actual may include subtypes, itypes, and inherited
      --  primitive operations, interspersed among the renaming declarations
      --  for the actuals) . We retrieve the corresponding actual by name
      --  because each actual has the same name as the formal, and they do
      --  appear in the same order.

      function Formal_Entity
        (F       : Node_Id;
         Act_Ent : Entity_Id) return Entity_Id;
      --  Returns the entity associated with the given formal F. In the
      --  case where F is a formal package, this function will iterate
      --  through all of F's formals and enter map associations from the
      --  actuals occurring in the formal package's corresponding actual
      --  package (obtained via Act_Ent) to the formal package's formal
      --  parameters. This function is called recursively for arbitrary
      --  levels of formal packages.

      function Is_Instance_Of
        (Act_Spec : Entity_Id;
         Gen_Anc  : Entity_Id) return Boolean;
      --  The actual can be an instantiation of a generic within another
      --  instance, in which case there is no direct link from it to the
      --  original generic ancestor. In that case, we recognize that the
      --  ultimate ancestor is the same by examining names and scopes.

      procedure Map_Entities (Form : Entity_Id; Act : Entity_Id);
      --  Within the generic part, entities in the formal package are
      --  visible. To validate subsequent type declarations, indicate
      --  the correspondence betwen the entities in the analyzed formal,
      --  and the entities in  the actual package. There are three packages
      --  involved in the instantiation of a formal package: the parent
      --  generic P1 which appears in the generic declaration, the fake
      --  instantiation P2 which appears in the analyzed generic, and whose
      --  visible entities may be used in subsequent formals, and the actual
      --  P3 in the instance. To validate subsequent formals, me indicate
      --  that the entities in P2 are mapped into those of P3. The mapping of
      --  entities has to be done recursively for nested packages.

      procedure Process_Nested_Formal (Formal : Entity_Id);
      --  If the current formal is declared with a box, its own formals are
      --  visible in the instance, as they were in the generic, and their
      --  Hidden flag must be reset. If some of these formals are themselves
      --  packages declared with a box, the processing must be recursive.

      --------------------------
      -- Find_Matching_Actual --
      --------------------------

      procedure Find_Matching_Actual
        (F   : Node_Id;
         Act : in out Entity_Id)
     is
         Formal_Ent : Entity_Id;

      begin
         case Nkind (Original_Node (F)) is
            when N_Formal_Object_Declaration |
                 N_Formal_Type_Declaration   =>
               Formal_Ent := Defining_Identifier (F);

               while Chars (Act) /= Chars (Formal_Ent) loop
                  Next_Entity (Act);
               end loop;

            when N_Formal_Subprogram_Declaration |
                 N_Formal_Package_Declaration    |
                 N_Package_Declaration           |
                 N_Generic_Package_Declaration   =>
               Formal_Ent := Defining_Entity (F);

               while Chars (Act) /= Chars (Formal_Ent) loop
                  Next_Entity (Act);
               end loop;

            when others =>
               raise Program_Error;
         end case;
      end Find_Matching_Actual;

      -------------------
      -- Formal_Entity --
      -------------------

      function Formal_Entity
        (F       : Node_Id;
         Act_Ent : Entity_Id) return Entity_Id
      is
         Orig_Node : Node_Id := F;
         Act_Pkg   : Entity_Id;

      begin
         case Nkind (Original_Node (F)) is
            when N_Formal_Object_Declaration     =>
               return Defining_Identifier (F);

            when N_Formal_Type_Declaration       =>
               return Defining_Identifier (F);

            when N_Formal_Subprogram_Declaration =>
               return Defining_Unit_Name (Specification (F));

            when N_Package_Declaration           =>
               return Defining_Unit_Name (Specification (F));

            when N_Formal_Package_Declaration |
                 N_Generic_Package_Declaration   =>

               if Nkind (F) = N_Generic_Package_Declaration then
                  Orig_Node := Original_Node (F);
               end if;

               Act_Pkg := Act_Ent;

               --  Find matching actual package, skipping over itypes and
               --  other entities generated when analyzing the formal. We
               --  know that if the instantiation is legal then there is
               --  a matching package for the formal.

               while Ekind (Act_Pkg) /= E_Package loop
                  Act_Pkg := Next_Entity (Act_Pkg);
               end loop;

               declare
                  Actual_Ent  : Entity_Id := First_Entity (Act_Pkg);
                  Formal_Node : Node_Id;
                  Formal_Ent  : Entity_Id;

                  Gen_Decl : Node_Id;
                  Formals  : List_Id;

               begin
                  --  The actual may be a renamed generic package, in which
                  --  case we want to retrieve the original generic in order
                  --  to traverse its formal part.

                  if Present (Renamed_Entity (Entity (Name (Orig_Node)))) then
                     Gen_Decl :=
                       Unit_Declaration_Node (
                         Renamed_Entity (Entity (Name (Orig_Node))));
                  else
                     Gen_Decl :=
                        Unit_Declaration_Node (Entity (Name (Orig_Node)));
                  end if;

                  Formals := Generic_Formal_Declarations (Gen_Decl);

                  if Present (Formals) then
                     Formal_Node := First_Non_Pragma (Formals);
                  else
                     Formal_Node := Empty;
                  end if;

                  while Present (Actual_Ent)
                    and then Present (Formal_Node)
                    and then Actual_Ent /= First_Private_Entity (Act_Pkg)
                  loop
                     --  ???  Are the following calls also needed here:
                     --
                     --  Set_Is_Hidden (Actual_Ent, False);
                     --  Set_Is_Potentially_Use_Visible
                     --    (Actual_Ent, In_Use (Act_Ent));

                     Formal_Ent := Formal_Entity (Formal_Node, Actual_Ent);
                     if Present (Formal_Ent) then
                        Set_Instance_Of (Formal_Ent, Actual_Ent);
                     end if;
                     Next_Non_Pragma (Formal_Node);

                     Next_Entity (Actual_Ent);
                  end loop;
               end;

               return Defining_Identifier (Orig_Node);

            when N_Use_Package_Clause =>
               return Empty;

            when N_Use_Type_Clause =>
               return Empty;

            --  We return Empty for all other encountered forms of
            --  declarations because there are some cases of nonformal
            --  sorts of declaration that can show up (e.g., when array
            --  formals are present). Since it's not clear what kinds
            --  can appear among the formals, we won't raise failure here.

            when others =>
               return Empty;

         end case;
      end Formal_Entity;

      --------------------
      -- Is_Instance_Of --
      --------------------

      function Is_Instance_Of
        (Act_Spec : Entity_Id;
         Gen_Anc  : Entity_Id) return Boolean
      is
         Gen_Par : constant Entity_Id := Generic_Parent (Act_Spec);

      begin
         if No (Gen_Par) then
            return False;

         --  Simplest case: the generic parent of the actual is the formal

         elsif Gen_Par = Gen_Anc then
            return True;

         elsif Chars (Gen_Par) /= Chars (Gen_Anc) then
            return False;

         --  The actual may be obtained through several instantiations. Its
         --  scope must itself be an instance of a generic declared in the
         --  same scope as the formal. Any other case is detected above.

         elsif not Is_Generic_Instance (Scope (Gen_Par)) then
            return False;

         else
            return Generic_Parent (Parent (Scope (Gen_Par))) = Scope (Gen_Anc);
         end if;
      end Is_Instance_Of;

      ------------------
      -- Map_Entities --
      ------------------

      procedure Map_Entities (Form : Entity_Id; Act : Entity_Id) is
         E1 : Entity_Id;
         E2 : Entity_Id;

      begin
         Set_Instance_Of (Form, Act);

         --  Traverse formal and actual package to map the corresponding
         --  entities. We skip over internal entities that may be generated
         --  during semantic analysis, and find the matching entities by
         --  name, given that they must appear in the same order.

         E1 := First_Entity (Form);
         E2 := First_Entity (Act);
         while Present (E1)
           and then E1 /= First_Private_Entity (Form)
         loop
            --  Could this test be a single condition???
            --  Seems like it could, and isn't FPE (Form) a constant anyway???

            if not Is_Internal (E1)
              and then Present (Parent (E1))
              and then not Is_Class_Wide_Type (E1)
              and then not Is_Internal_Name (Chars (E1))
            then
               while Present (E2)
                 and then Chars (E2) /= Chars (E1)
               loop
                  Next_Entity (E2);
               end loop;

               if No (E2) then
                  exit;
               else
                  Set_Instance_Of (E1, E2);

                  if Is_Type (E1)
                    and then Is_Tagged_Type (E2)
                  then
                     Set_Instance_Of
                       (Class_Wide_Type (E1), Class_Wide_Type (E2));
                  end if;

                  if Ekind (E1) = E_Package
                    and then No (Renamed_Object (E1))
                  then
                     Map_Entities (E1, E2);
                  end if;
               end if;
            end if;

            Next_Entity (E1);
         end loop;
      end Map_Entities;

      ---------------------------
      -- Process_Nested_Formal --
      ---------------------------

      procedure Process_Nested_Formal (Formal : Entity_Id) is
         Ent : Entity_Id;

      begin
         if Present (Associated_Formal_Package (Formal))
           and then Box_Present (Parent (Associated_Formal_Package (Formal)))
         then
            Ent := First_Entity (Formal);
            while Present (Ent) loop
               Set_Is_Hidden (Ent, False);
               Set_Is_Potentially_Use_Visible
                 (Ent, Is_Potentially_Use_Visible (Formal));

               if Ekind (Ent) = E_Package then
                  exit when Renamed_Entity (Ent) = Renamed_Entity (Formal);
                  Process_Nested_Formal (Ent);
               end if;

               Next_Entity (Ent);
            end loop;
         end if;
      end Process_Nested_Formal;

   --  Start of processing for Instantiate_Formal_Package

   begin
      Analyze (Actual);

      if not Is_Entity_Name (Actual)
        or else  Ekind (Entity (Actual)) /= E_Package
      then
         Error_Msg_N
           ("expect package instance to instantiate formal", Actual);
         Abandon_Instantiation (Actual);
         raise Program_Error;

      else
         Actual_Pack := Entity (Actual);
         Set_Is_Instantiated (Actual_Pack);

         --  The actual may be a renamed package, or an outer generic
         --  formal package whose instantiation is converted into a renaming.

         if Present (Renamed_Object (Actual_Pack)) then
            Actual_Pack := Renamed_Object (Actual_Pack);
         end if;

         if Nkind (Analyzed_Formal) = N_Formal_Package_Declaration then
            Gen_Parent  := Get_Instance_Of (Entity (Name (Analyzed_Formal)));
            Formal_Pack := Defining_Identifier (Analyzed_Formal);
         else
            Gen_Parent :=
              Generic_Parent (Specification (Analyzed_Formal));
            Formal_Pack :=
              Defining_Unit_Name (Specification (Analyzed_Formal));
         end if;

         if Nkind (Parent (Actual_Pack)) = N_Defining_Program_Unit_Name then
            Parent_Spec := Specification (Unit_Declaration_Node (Actual_Pack));
         else
            Parent_Spec := Parent (Actual_Pack);
         end if;

         if Gen_Parent = Any_Id then
            Error_Msg_N
              ("previous error in declaration of formal package", Actual);
            Abandon_Instantiation (Actual);

         elsif
           Is_Instance_Of (Parent_Spec, Get_Instance_Of (Gen_Parent))
         then
            null;

         else
            Error_Msg_NE
              ("actual parameter must be instance of&", Actual, Gen_Parent);
            Abandon_Instantiation (Actual);
         end if;

         Set_Instance_Of (Defining_Identifier (Formal), Actual_Pack);
         Map_Entities (Formal_Pack, Actual_Pack);

         Nod :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name => New_Copy (Defining_Identifier (Formal)),
             Name               => New_Reference_To (Actual_Pack, Loc));

         Set_Associated_Formal_Package (Defining_Unit_Name (Nod),
           Defining_Identifier (Formal));
         Decls := New_List (Nod);

         --  If the formal F has a box, then the generic declarations are
         --  visible in the generic G. In an instance of G, the corresponding
         --  entities in the actual for F (which are the actuals for the
         --  instantiation of the generic that F denotes) must also be made
         --  visible for analysis of the current instance. On exit from the
         --  current instance, those entities are made private again. If the
         --  actual is currently in use, these entities are also use-visible.

         --  The loop through the actual entities also steps through the
         --  formal entities and enters associations from formals to
         --  actuals into the renaming map. This is necessary to properly
         --  handle checking of actual parameter associations for later
         --  formals that depend on actuals declared in the formal package.

         if Box_Present (Formal) then
            declare
               Gen_Decl    : constant Node_Id :=
                               Unit_Declaration_Node (Gen_Parent);
               Formals     : constant List_Id :=
                               Generic_Formal_Declarations (Gen_Decl);
               Actual_Ent  : Entity_Id;
               Formal_Node : Node_Id;
               Formal_Ent  : Entity_Id;

            begin
               if Present (Formals) then
                  Formal_Node := First_Non_Pragma (Formals);
               else
                  Formal_Node := Empty;
               end if;

               Actual_Ent := First_Entity (Actual_Pack);

               while Present (Actual_Ent)
                 and then Actual_Ent /= First_Private_Entity (Actual_Pack)
               loop
                  Set_Is_Hidden (Actual_Ent, False);
                  Set_Is_Potentially_Use_Visible
                    (Actual_Ent, In_Use (Actual_Pack));

                  if Ekind (Actual_Ent) = E_Package then
                     Process_Nested_Formal (Actual_Ent);
                  end if;

                  if Present (Formal_Node) then
                     Formal_Ent := Formal_Entity (Formal_Node, Actual_Ent);

                     if Present (Formal_Ent) then
                        Find_Matching_Actual (Formal_Node, Actual_Ent);
                        Set_Instance_Of (Formal_Ent, Actual_Ent);
                     end if;

                     Next_Non_Pragma (Formal_Node);

                  else
                     --  No further formals to match, but the generic
                     --  part may contain inherited operation that are
                     --  not hidden in the enclosing instance.

                     Next_Entity (Actual_Ent);
                  end if;

               end loop;
            end;

         --  If the formal is not declared with a box, reanalyze it as
         --  an instantiation, to verify the matching rules of 12.7. The
         --  actual checks are performed after the generic associations
         --  been analyzed.

         else
            declare
               I_Pack : constant Entity_Id :=
                          Make_Defining_Identifier (Sloc (Actual),
                            Chars => New_Internal_Name  ('P'));

            begin
               Set_Is_Internal (I_Pack);

               Append_To (Decls,
                 Make_Package_Instantiation (Sloc (Actual),
                   Defining_Unit_Name => I_Pack,
                   Name => New_Occurrence_Of (Gen_Parent, Sloc (Actual)),
                   Generic_Associations =>
                     Generic_Associations (Formal)));
            end;
         end if;

         return Decls;
      end if;
   end Instantiate_Formal_Package;

   -----------------------------------
   -- Instantiate_Formal_Subprogram --
   -----------------------------------

   function Instantiate_Formal_Subprogram
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return Node_Id
   is
      Loc        : Source_Ptr := Sloc (Instantiation_Node);
      Formal_Sub : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Formal));
      Analyzed_S : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Analyzed_Formal));
      Decl_Node  : Node_Id;
      Nam        : Node_Id;
      New_Spec   : Node_Id;

      function From_Parent_Scope (Subp : Entity_Id) return Boolean;
      --  If the generic is a child unit, the parent has been installed on the
      --  scope stack, but a default subprogram cannot resolve to something on
      --  the parent because that parent is not really part of the visible
      --  context (it is there to resolve explicit local entities). If the
      --  default has resolved in this way, we remove the entity from
      --  immediate visibility and analyze the node again to emit an error
      --  message or find another visible candidate.

      procedure Valid_Actual_Subprogram (Act : Node_Id);
      --  Perform legality check and raise exception on failure

      -----------------------
      -- From_Parent_Scope --
      -----------------------

      function From_Parent_Scope (Subp : Entity_Id) return Boolean is
         Gen_Scope : Node_Id := Scope (Analyzed_S);

      begin
         while Present (Gen_Scope)
           and then  Is_Child_Unit (Gen_Scope)
         loop
            if Scope (Subp) = Scope (Gen_Scope) then
               return True;
            end if;

            Gen_Scope := Scope (Gen_Scope);
         end loop;

         return False;
      end From_Parent_Scope;

      -----------------------------
      -- Valid_Actual_Subprogram --
      -----------------------------

      procedure Valid_Actual_Subprogram (Act : Node_Id) is
         Act_E : Entity_Id := Empty;

      begin
         if Is_Entity_Name (Act) then
            Act_E := Entity (Act);
         elsif Nkind (Act) = N_Selected_Component
           and then Is_Entity_Name (Selector_Name (Act))
         then
            Act_E := Entity (Selector_Name (Act));
         end if;

         if (Present (Act_E) and then Is_Overloadable (Act_E))
           or else Nkind (Act) = N_Attribute_Reference
           or else Nkind (Act) = N_Indexed_Component
           or else Nkind (Act) = N_Character_Literal
           or else Nkind (Act) = N_Explicit_Dereference
         then
            return;
         end if;

         Error_Msg_NE
           ("expect subprogram or entry name in instantiation of&",
            Instantiation_Node, Formal_Sub);
         Abandon_Instantiation (Instantiation_Node);

      end Valid_Actual_Subprogram;

   --  Start of processing for Instantiate_Formal_Subprogram

   begin
      New_Spec := New_Copy_Tree (Specification (Formal));

      --  Create new entity for the actual (New_Copy_Tree does not)

      Set_Defining_Unit_Name
        (New_Spec, Make_Defining_Identifier (Loc, Chars (Formal_Sub)));

      --  Find entity of actual. If the actual is an attribute reference, it
      --  cannot be resolved here (its formal is missing) but is handled
      --  instead in Attribute_Renaming. If the actual is overloaded, it is
      --  fully resolved subsequently, when the renaming declaration for the
      --  formal is analyzed. If it is an explicit dereference, resolve the
      --  prefix but not the actual itself, to prevent interpretation as a
      --  call.

      if Present (Actual) then
         Loc := Sloc (Actual);
         Set_Sloc (New_Spec, Loc);

         if Nkind (Actual) = N_Operator_Symbol then
            Find_Direct_Name (Actual);

         elsif Nkind (Actual) = N_Explicit_Dereference then
            Analyze (Prefix (Actual));

         elsif Nkind (Actual) /= N_Attribute_Reference then
            Analyze (Actual);
         end if;

         Valid_Actual_Subprogram (Actual);
         Nam := Actual;

      elsif Present (Default_Name (Formal)) then
         if Nkind (Default_Name (Formal)) /= N_Attribute_Reference
           and then Nkind (Default_Name (Formal)) /= N_Selected_Component
           and then Nkind (Default_Name (Formal)) /= N_Indexed_Component
           and then Nkind (Default_Name (Formal)) /= N_Character_Literal
           and then Present (Entity (Default_Name (Formal)))
         then
            Nam := New_Occurrence_Of (Entity (Default_Name (Formal)), Loc);
         else
            Nam := New_Copy (Default_Name (Formal));
            Set_Sloc (Nam, Loc);
         end if;

      elsif Box_Present (Formal) then

         --  Actual is resolved at the point of instantiation. Create
         --  an identifier or operator with the same name as the formal.

         if Nkind (Formal_Sub) = N_Defining_Operator_Symbol then
            Nam := Make_Operator_Symbol (Loc,
              Chars =>  Chars (Formal_Sub),
              Strval => No_String);
         else
            Nam := Make_Identifier (Loc, Chars (Formal_Sub));
         end if;

      elsif Nkind (Specification (Formal)) = N_Procedure_Specification
        and then Null_Present (Specification (Formal))
      then
         --  Generate null body for procedure, for use in the instance

         Decl_Node :=
           Make_Subprogram_Body (Loc,
             Specification              => New_Spec,
             Declarations               => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Make_Null_Statement (Loc))));

         Set_Is_Intrinsic_Subprogram (Defining_Unit_Name (New_Spec));
         return Decl_Node;

      else
         Error_Msg_Sloc := Sloc (Scope (Analyzed_S));
         Error_Msg_NE
           ("missing actual&", Instantiation_Node, Formal_Sub);
         Error_Msg_NE
           ("\in instantiation of & declared#",
              Instantiation_Node, Scope (Analyzed_S));
         Abandon_Instantiation (Instantiation_Node);
      end if;

      Decl_Node :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => New_Spec,
          Name          => Nam);

      --  If we do not have an actual and the formal specified <> then
      --  set to get proper default.

      if No (Actual) and then Box_Present (Formal) then
         Set_From_Default (Decl_Node);
      end if;

      --  Gather possible interpretations for the actual before analyzing the
      --  instance. If overloaded, it will be resolved when analyzing the
      --  renaming declaration.

      if Box_Present (Formal)
        and then No (Actual)
      then
         Analyze (Nam);

         if Is_Child_Unit (Scope (Analyzed_S))
           and then Present (Entity (Nam))
         then
            if not Is_Overloaded (Nam) then

               if From_Parent_Scope (Entity (Nam)) then
                  Set_Is_Immediately_Visible (Entity (Nam), False);
                  Set_Entity (Nam, Empty);
                  Set_Etype (Nam, Empty);

                  Analyze (Nam);

                  Set_Is_Immediately_Visible (Entity (Nam));
               end if;

            else
               declare
                  I  : Interp_Index;
                  It : Interp;

               begin
                  Get_First_Interp (Nam, I, It);

                  while Present (It.Nam) loop
                     if From_Parent_Scope (It.Nam) then
                        Remove_Interp (I);
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end;
            end if;
         end if;
      end if;

      --  The generic instantiation freezes the actual. This can only be
      --  done once the actual is resolved, in the analysis of the renaming
      --  declaration. To make the formal subprogram entity available, we set
      --  Corresponding_Formal_Spec to point to the formal subprogram entity.
      --  This is also needed in Analyze_Subprogram_Renaming for the processing
      --  of formal abstract subprograms.

      Set_Corresponding_Formal_Spec (Decl_Node, Analyzed_S);

      --  We cannot analyze the renaming declaration, and thus find the
      --  actual, until the all the actuals are assembled in the instance.
      --  For subsequent checks of other actuals, indicate the node that
      --  will hold the instance of this formal.

      Set_Instance_Of (Analyzed_S, Nam);

      if Nkind (Actual) = N_Selected_Component
        and then Is_Task_Type (Etype (Prefix (Actual)))
        and then not Is_Frozen (Etype (Prefix (Actual)))
      then
         --  The renaming declaration will create a body, which must appear
         --  outside of the instantiation, We move the renaming declaration
         --  out of the instance, and create an additional renaming inside,
         --  to prevent freezing anomalies.

         declare
            Anon_Id : constant Entity_Id :=
                        Make_Defining_Identifier
                          (Loc, New_Internal_Name ('E'));
         begin
            Set_Defining_Unit_Name (New_Spec, Anon_Id);
            Insert_Before (Instantiation_Node, Decl_Node);
            Analyze (Decl_Node);

            --  Now create renaming within the instance

            Decl_Node :=
              Make_Subprogram_Renaming_Declaration (Loc,
                Specification => New_Copy_Tree (New_Spec),
                Name => New_Occurrence_Of (Anon_Id, Loc));

            Set_Defining_Unit_Name (Specification (Decl_Node),
              Make_Defining_Identifier (Loc, Chars (Formal_Sub)));
         end;
      end if;

      return Decl_Node;
   end Instantiate_Formal_Subprogram;

   ------------------------
   -- Instantiate_Object --
   ------------------------

   function Instantiate_Object
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return List_Id
   is
      Formal_Id : constant Entity_Id  := Defining_Identifier (Formal);
      Type_Id   : constant Node_Id    := Subtype_Mark (Formal);
      Loc       : constant Source_Ptr := Sloc (Actual);
      Act_Assoc : constant Node_Id    := Parent (Actual);
      Orig_Ftyp : constant Entity_Id  :=
                    Etype (Defining_Identifier (Analyzed_Formal));
      List      : constant List_Id    := New_List;
      Ftyp      : Entity_Id;
      Decl_Node : Node_Id;
      Subt_Decl : Node_Id := Empty;

   begin
      --  Sloc for error message on missing actual

      Error_Msg_Sloc := Sloc (Scope (Defining_Identifier (Analyzed_Formal)));

      if Get_Instance_Of (Formal_Id) /= Formal_Id then
         Error_Msg_N ("duplicate instantiation of generic parameter", Actual);
      end if;

      Set_Parent (List, Parent (Actual));

      --  OUT present

      if Out_Present (Formal) then

         --  An IN OUT generic actual must be a name. The instantiation is a
         --  renaming declaration. The actual is the name being renamed. We
         --  use the actual directly, rather than a copy, because it is not
         --  used further in the list of actuals, and because a copy or a use
         --  of relocate_node is incorrect if the instance is nested within a
         --  generic. In order to simplify ASIS searches, the Generic_Parent
         --  field links the declaration to the generic association.

         if No (Actual) then
            Error_Msg_NE
              ("missing actual&",
               Instantiation_Node, Formal_Id);
            Error_Msg_NE
              ("\in instantiation of & declared#",
                 Instantiation_Node,
                   Scope (Defining_Identifier (Analyzed_Formal)));
            Abandon_Instantiation (Instantiation_Node);
         end if;

         Decl_Node :=
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => New_Copy (Formal_Id),
             Subtype_Mark        => New_Copy_Tree (Type_Id),
             Name                => Actual);

         Set_Corresponding_Generic_Association (Decl_Node, Act_Assoc);

         --  The analysis of the actual may produce insert_action nodes, so
         --  the declaration must have a context in which to attach them.

         Append (Decl_Node, List);
         Analyze (Actual);

         --  Return if the analysis of the actual reported some error

         if Etype (Actual) = Any_Type then
            return List;
         end if;

         --  This check is performed here because Analyze_Object_Renaming
         --  will not check it when Comes_From_Source is False. Note
         --  though that the check for the actual being the name of an
         --  object will be performed in Analyze_Object_Renaming.

         if Is_Object_Reference (Actual)
           and then Is_Dependent_Component_Of_Mutable_Object (Actual)
         then
            Error_Msg_N
              ("illegal discriminant-dependent component for in out parameter",
               Actual);
         end if;

         --  The actual has to be resolved in order to check that it is
         --  a variable (due to cases such as F(1), where F returns
         --  access to an array, and for overloaded prefixes).

         Ftyp :=
           Get_Instance_Of (Etype (Defining_Identifier (Analyzed_Formal)));

         if Is_Private_Type (Ftyp)
           and then not Is_Private_Type (Etype (Actual))
           and then (Base_Type (Full_View (Ftyp)) = Base_Type (Etype (Actual))
                      or else Base_Type (Etype (Actual)) = Ftyp)
         then
            --  If the actual has the type of the full view of the formal,
            --  or else a non-private subtype of the formal, then
            --  the visibility of the formal type has changed. Add to the
            --  actuals a subtype declaration that will force the exchange
            --  of views in the body of the instance as well.

            Subt_Decl :=
              Make_Subtype_Declaration (Loc,
                 Defining_Identifier =>
                   Make_Defining_Identifier (Loc, New_Internal_Name ('P')),
                 Subtype_Indication  => New_Occurrence_Of (Ftyp, Loc));

            Prepend (Subt_Decl, List);

            Prepend_Elmt (Full_View (Ftyp), Exchanged_Views);
            Exchange_Declarations (Ftyp);
         end if;

         Resolve (Actual, Ftyp);

         if not Is_Variable (Actual) or else Paren_Count (Actual) > 0 then
            Error_Msg_NE
              ("actual for& must be a variable", Actual, Formal_Id);

         elsif Base_Type (Ftyp) /= Base_Type (Etype (Actual)) then
            Error_Msg_NE (
              "type of actual does not match type of&", Actual, Formal_Id);

         end if;

         Note_Possible_Modification (Actual);

         --  Check for instantiation of atomic/volatile actual for
         --  non-atomic/volatile formal (RM C.6 (12)).

         if Is_Atomic_Object (Actual)
           and then not Is_Atomic (Orig_Ftyp)
         then
            Error_Msg_N
              ("cannot instantiate non-atomic formal object " &
               "with atomic actual", Actual);

         elsif Is_Volatile_Object (Actual)
           and then not Is_Volatile (Orig_Ftyp)
         then
            Error_Msg_N
              ("cannot instantiate non-volatile formal object " &
               "with volatile actual", Actual);
         end if;

      --  OUT not present

      else
         --  The instantiation of a generic formal in-parameter
         --  is a constant declaration. The actual is the expression for
         --  that declaration.

         if Present (Actual) then

            Decl_Node := Make_Object_Declaration (Loc,
              Defining_Identifier => New_Copy (Formal_Id),
              Constant_Present => True,
              Object_Definition => New_Copy_Tree (Type_Id),
              Expression => Actual);

            Set_Corresponding_Generic_Association (Decl_Node, Act_Assoc);

            --  A generic formal object of a tagged type is defined
            --  to be aliased so the new constant must also be treated
            --  as aliased.

            if Is_Tagged_Type
                 (Etype (Defining_Identifier (Analyzed_Formal)))
            then
               Set_Aliased_Present (Decl_Node);
            end if;

            Append (Decl_Node, List);

            --  No need to repeat (pre-)analysis of some expression nodes
            --  already handled in Pre_Analyze_Actuals.

            if Nkind (Actual) /= N_Allocator then
               Analyze (Actual);

               --  Return if the analysis of the actual reported some error

               if Etype (Actual) = Any_Type then
                  return List;
               end if;
            end if;

            declare
               Typ : constant Entity_Id :=
                       Get_Instance_Of
                         (Etype (Defining_Identifier (Analyzed_Formal)));

            begin
               Freeze_Before (Instantiation_Node, Typ);

               --  If the actual is an aggregate, perform name resolution on
               --  its components (the analysis of an aggregate does not do
               --  it) to capture local names that may be hidden if the
               --  generic is a child unit.

               if Nkind (Actual) = N_Aggregate then
                     Pre_Analyze_And_Resolve (Actual, Typ);
               end if;
            end;

         elsif Present (Expression (Formal)) then

            --  Use default to construct declaration

            Decl_Node :=
              Make_Object_Declaration (Sloc (Formal),
                Defining_Identifier => New_Copy (Formal_Id),
                Constant_Present    => True,
                Object_Definition   => New_Copy (Type_Id),
                Expression          => New_Copy_Tree (Expression (Formal)));

            Append (Decl_Node, List);
            Set_Analyzed (Expression (Decl_Node), False);

         else
            Error_Msg_NE
              ("missing actual&",
                Instantiation_Node, Formal_Id);
            Error_Msg_NE ("\in instantiation of & declared#",
              Instantiation_Node,
                Scope (Defining_Identifier (Analyzed_Formal)));

            if Is_Scalar_Type
                 (Etype (Defining_Identifier (Analyzed_Formal)))
            then
               --  Create dummy constant declaration so that instance can
               --  be analyzed, to minimize cascaded visibility errors.

               Decl_Node :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => New_Copy (Formal_Id),
                   Constant_Present    => True,
                   Object_Definition   => New_Copy (Type_Id),
                   Expression          =>
                      Make_Attribute_Reference (Sloc (Formal_Id),
                        Attribute_Name => Name_First,
                        Prefix         => New_Copy (Type_Id)));

               Append (Decl_Node, List);

            else
               Abandon_Instantiation (Instantiation_Node);
            end if;
         end if;

      end if;

      return List;
   end Instantiate_Object;

   ------------------------------
   -- Instantiate_Package_Body --
   ------------------------------

   procedure Instantiate_Package_Body
     (Body_Info    : Pending_Body_Info;
      Inlined_Body : Boolean := False)
   is
      Act_Decl    : constant Node_Id    := Body_Info.Act_Decl;
      Inst_Node   : constant Node_Id    := Body_Info.Inst_Node;
      Loc         : constant Source_Ptr := Sloc (Inst_Node);

      Gen_Id      : constant Node_Id    := Name (Inst_Node);
      Gen_Unit    : constant Entity_Id  := Get_Generic_Entity (Inst_Node);
      Gen_Decl    : constant Node_Id    := Unit_Declaration_Node (Gen_Unit);
      Act_Spec    : constant Node_Id    := Specification (Act_Decl);
      Act_Decl_Id : constant Entity_Id  := Defining_Entity (Act_Spec);

      Act_Body_Name : Node_Id;
      Gen_Body      : Node_Id;
      Gen_Body_Id   : Node_Id;
      Act_Body      : Node_Id;
      Act_Body_Id   : Entity_Id;

      Parent_Installed : Boolean := False;
      Save_Style_Check : constant Boolean := Style_Check;

   begin
      Gen_Body_Id := Corresponding_Body (Gen_Decl);

      --  The instance body may already have been processed, as the parent
      --  of another instance that is inlined. (Load_Parent_Of_Generic).

      if Present (Corresponding_Body (Instance_Spec (Inst_Node))) then
         return;
      end if;

      Expander_Mode_Save_And_Set (Body_Info.Expander_Status);

      if No (Gen_Body_Id) then
         Load_Parent_Of_Generic (Inst_Node, Specification (Gen_Decl));
         Gen_Body_Id := Corresponding_Body (Gen_Decl);
      end if;

      --  Establish global variable for sloc adjustment and for error
      --  recovery.

      Instantiation_Node := Inst_Node;

      if Present (Gen_Body_Id) then
         Save_Env (Gen_Unit, Act_Decl_Id);
         Style_Check := False;
         Current_Sem_Unit := Body_Info.Current_Sem_Unit;

         Gen_Body := Unit_Declaration_Node (Gen_Body_Id);

         Create_Instantiation_Source
          (Inst_Node, Gen_Body_Id, False, S_Adjustment);

         Act_Body :=
           Copy_Generic_Node
             (Original_Node (Gen_Body), Empty, Instantiating => True);

         --  Build new name (possibly qualified) for body declaration

         Act_Body_Id := New_Copy (Act_Decl_Id);

         --  Some attributes of the spec entity are not inherited by the
         --  body entity.

         Set_Handler_Records (Act_Body_Id, No_List);

         if Nkind (Defining_Unit_Name (Act_Spec)) =
                                           N_Defining_Program_Unit_Name
         then
            Act_Body_Name :=
              Make_Defining_Program_Unit_Name (Loc,
                Name => New_Copy_Tree (Name (Defining_Unit_Name (Act_Spec))),
                Defining_Identifier => Act_Body_Id);
         else
            Act_Body_Name :=  Act_Body_Id;
         end if;

         Set_Defining_Unit_Name (Act_Body, Act_Body_Name);

         Set_Corresponding_Spec (Act_Body, Act_Decl_Id);
         Check_Generic_Actuals (Act_Decl_Id, False);

         --  If it is a child unit, make the parent instance (which is an
         --  instance of the parent of the generic) visible. The parent
         --  instance is the prefix of the name of the generic unit.

         if Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Nkind (Gen_Id) = N_Expanded_Name
         then
            Install_Parent (Entity (Prefix (Gen_Id)), In_Body => True);
            Parent_Installed := True;

         elsif Is_Child_Unit (Gen_Unit) then
            Install_Parent (Scope (Gen_Unit), In_Body => True);
            Parent_Installed := True;
         end if;

         --  If the instantiation is a library unit, and this is the main
         --  unit, then build the resulting compilation unit nodes for the
         --  instance. If this is a compilation unit but it is not the main
         --  unit, then it is the body of a unit in the context, that is being
         --  compiled because it is encloses some inlined unit or another
         --  generic unit being instantiated. In that case, this body is not
         --  part of the current compilation, and is not attached to the tree,
         --  but its parent must be set for analysis.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then

            --  Replace instance node with body of instance, and create
            --  new node for corresponding instance declaration.

            Build_Instance_Compilation_Unit_Nodes
              (Inst_Node, Act_Body, Act_Decl);
            Analyze (Inst_Node);

            if Parent (Inst_Node) = Cunit (Main_Unit) then

               --  If the instance is a child unit itself, then set the
               --  scope of the expanded body to be the parent of the
               --  instantiation (ensuring that the fully qualified name
               --  will be generated for the elaboration subprogram).

               if Nkind (Defining_Unit_Name (Act_Spec)) =
                                              N_Defining_Program_Unit_Name
               then
                  Set_Scope
                    (Defining_Entity (Inst_Node), Scope (Act_Decl_Id));
               end if;
            end if;

         --  Case where instantiation is not a library unit

         else
            --  If this is an early instantiation, i.e. appears textually
            --  before the corresponding body and must be elaborated first,
            --  indicate that the body instance is to be delayed.

            Install_Body (Act_Body, Inst_Node, Gen_Body, Gen_Decl);

            --  Now analyze the body. We turn off all checks if this is
            --  an internal unit, since there is no reason to have checks
            --  on for any predefined run-time library code. All such
            --  code is designed to be compiled with checks off.

            --  Note that we do NOT apply this criterion to children of
            --  GNAT (or on VMS, children of DEC). The latter units must
            --  suppress checks explicitly if this is needed.

            if Is_Predefined_File_Name
                 (Unit_File_Name (Get_Source_Unit (Gen_Decl)))
            then
               Analyze (Act_Body, Suppress => All_Checks);
            else
               Analyze (Act_Body);
            end if;
         end if;

         if not Generic_Separately_Compiled (Gen_Unit) then
            Inherit_Context (Gen_Body, Inst_Node);
         end if;

         --  Remove the parent instances if they have been placed on the
         --  scope stack to compile the body.

         if Parent_Installed then
            Remove_Parent (In_Body => True);
         end if;

         Restore_Private_Views (Act_Decl_Id);

         --  Remove the current unit from visibility if this is an instance
         --  that is not elaborated on the fly for inlining purposes.

         if not Inlined_Body then
            Set_Is_Immediately_Visible (Act_Decl_Id, False);
         end if;

         Restore_Env;
         Style_Check := Save_Style_Check;

      --  If we have no body, and the unit requires a body, then complain.
      --  This complaint is suppressed if we have detected other errors
      --  (since a common reason for missing the body is that it had errors).

      elsif Unit_Requires_Body (Gen_Unit) then
         if Serious_Errors_Detected = 0 then
            Error_Msg_NE
              ("cannot find body of generic package &", Inst_Node, Gen_Unit);

         --  Don't attempt to perform any cleanup actions if some other
         --  error was aready detected, since this can cause blowups.

         else
            return;
         end if;

      --  Case of package that does not need a body

      else
         --  If the instantiation of the declaration is a library unit,
         --  rewrite the original package instantiation as a package
         --  declaration in the compilation unit node.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then
            Set_Parent_Spec (Act_Decl, Parent_Spec (Inst_Node));
            Rewrite (Inst_Node, Act_Decl);

            --  Generate elaboration entity, in case spec has elaboration
            --  code. This cannot be done when the instance is analyzed,
            --  because it is not known yet whether the body exists.

            Set_Elaboration_Entity_Required (Act_Decl_Id, False);
            Build_Elaboration_Entity (Parent (Inst_Node), Act_Decl_Id);

         --  If the instantiation is not a library unit, then append the
         --  declaration to the list of implicitly generated entities.
         --  unless it is already a list member which means that it was
         --  already processed

         elsif not Is_List_Member (Act_Decl) then
            Mark_Rewrite_Insertion (Act_Decl);
            Insert_Before (Inst_Node, Act_Decl);
         end if;
      end if;

      Expander_Mode_Restore;
   end Instantiate_Package_Body;

   ---------------------------------
   -- Instantiate_Subprogram_Body --
   ---------------------------------

   procedure Instantiate_Subprogram_Body
     (Body_Info : Pending_Body_Info)
   is
      Act_Decl      : constant Node_Id    := Body_Info.Act_Decl;
      Inst_Node     : constant Node_Id    := Body_Info.Inst_Node;
      Loc           : constant Source_Ptr := Sloc (Inst_Node);
      Gen_Id        : constant Node_Id   := Name (Inst_Node);
      Gen_Unit      : constant Entity_Id := Get_Generic_Entity (Inst_Node);
      Gen_Decl      : constant Node_Id   := Unit_Declaration_Node (Gen_Unit);
      Anon_Id       : constant Entity_Id :=
                        Defining_Unit_Name (Specification (Act_Decl));
      Pack_Id       : constant Entity_Id :=
                        Defining_Unit_Name (Parent (Act_Decl));
      Decls         : List_Id;
      Gen_Body      : Node_Id;
      Gen_Body_Id   : Node_Id;
      Act_Body      : Node_Id;
      Act_Body_Id   : Entity_Id;
      Pack_Body     : Node_Id;
      Prev_Formal   : Entity_Id;
      Ret_Expr      : Node_Id;
      Unit_Renaming : Node_Id;

      Parent_Installed : Boolean := False;
      Save_Style_Check : constant Boolean := Style_Check;

   begin
      Gen_Body_Id := Corresponding_Body (Gen_Decl);

      Expander_Mode_Save_And_Set (Body_Info.Expander_Status);

      if No (Gen_Body_Id) then
         Load_Parent_Of_Generic (Inst_Node, Specification (Gen_Decl));
         Gen_Body_Id := Corresponding_Body (Gen_Decl);
      end if;

      Instantiation_Node := Inst_Node;

      if Present (Gen_Body_Id) then
         Gen_Body := Unit_Declaration_Node (Gen_Body_Id);

         if Nkind (Gen_Body) = N_Subprogram_Body_Stub then

            --  Either body is not present, or context is non-expanding, as
            --  when compiling a subunit. Mark the instance as completed, and
            --  diagnose a missing body when needed.

            if Expander_Active
              and then Operating_Mode = Generate_Code
            then
               Error_Msg_N
                 ("missing proper body for instantiation", Gen_Body);
            end if;

            Set_Has_Completion (Anon_Id);
            return;
         end if;

         Save_Env (Gen_Unit, Anon_Id);
         Style_Check := False;
         Current_Sem_Unit := Body_Info.Current_Sem_Unit;
         Create_Instantiation_Source
           (Inst_Node,
            Gen_Body_Id,
            False,
            S_Adjustment);

         Act_Body :=
           Copy_Generic_Node
             (Original_Node (Gen_Body), Empty, Instantiating => True);
         Act_Body_Id := Defining_Entity (Act_Body);
         Set_Chars (Act_Body_Id, Chars (Anon_Id));
         Set_Sloc (Act_Body_Id, Sloc (Defining_Entity (Inst_Node)));
         Set_Corresponding_Spec (Act_Body, Anon_Id);
         Set_Has_Completion (Anon_Id);
         Check_Generic_Actuals (Pack_Id, False);

         --  If it is a child unit, make the parent instance (which is an
         --  instance of the parent of the generic) visible. The parent
         --  instance is the prefix of the name of the generic unit.

         if Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Nkind (Gen_Id) = N_Expanded_Name
         then
            Install_Parent (Entity (Prefix (Gen_Id)), In_Body => True);
            Parent_Installed := True;

         elsif Is_Child_Unit (Gen_Unit) then
            Install_Parent (Scope (Gen_Unit), In_Body => True);
            Parent_Installed := True;
         end if;

         --  Inside its body, a reference to the generic unit is a reference
         --  to the instance. The corresponding renaming is the first
         --  declaration in the body.

         Unit_Renaming :=
           Make_Subprogram_Renaming_Declaration (Loc,
             Specification =>
               Copy_Generic_Node (
                 Specification (Original_Node (Gen_Body)),
                 Empty,
                 Instantiating => True),
             Name => New_Occurrence_Of (Anon_Id, Loc));

         --  If there is a formal subprogram with the same name as the
         --  unit itself, do not add this renaming declaration. This is
         --  a temporary fix for one ACVC test. ???

         Prev_Formal := First_Entity (Pack_Id);
         while Present (Prev_Formal) loop
            if Chars (Prev_Formal) = Chars (Gen_Unit)
              and then Is_Overloadable (Prev_Formal)
            then
               exit;
            end if;

            Next_Entity (Prev_Formal);
         end loop;

         if Present (Prev_Formal) then
            Decls :=  New_List (Act_Body);
         else
            Decls :=  New_List (Unit_Renaming, Act_Body);
         end if;

         --  The subprogram body is placed in the body of a dummy package
         --  body, whose spec contains the subprogram declaration as well
         --  as the renaming declarations for the generic parameters.

         Pack_Body := Make_Package_Body (Loc,
           Defining_Unit_Name => New_Copy (Pack_Id),
           Declarations       => Decls);

         Set_Corresponding_Spec (Pack_Body, Pack_Id);

         --  If the instantiation is a library unit, then build resulting
         --  compilation unit nodes for the instance. The declaration of
         --  the enclosing package is the grandparent of the subprogram
         --  declaration. First replace the instantiation node as the unit
         --  of the corresponding compilation.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then
            if Parent (Inst_Node) = Cunit (Main_Unit) then
               Set_Unit (Parent (Inst_Node), Inst_Node);
               Build_Instance_Compilation_Unit_Nodes
                 (Inst_Node, Pack_Body, Parent (Parent (Act_Decl)));
               Analyze (Inst_Node);
            else
               Set_Parent (Pack_Body, Parent (Inst_Node));
               Analyze (Pack_Body);
            end if;

         else
            Insert_Before (Inst_Node, Pack_Body);
            Mark_Rewrite_Insertion (Pack_Body);
            Analyze (Pack_Body);

            if Expander_Active then
               Freeze_Subprogram_Body (Inst_Node, Gen_Body, Pack_Id);
            end if;
         end if;

         if not Generic_Separately_Compiled (Gen_Unit) then
            Inherit_Context (Gen_Body, Inst_Node);
         end if;

         Restore_Private_Views (Pack_Id, False);

         if Parent_Installed then
            Remove_Parent (In_Body => True);
         end if;

         Restore_Env;
         Style_Check := Save_Style_Check;

      --  Body not found. Error was emitted already. If there were no
      --  previous errors, this may be an instance whose scope is a premature
      --  instance. In that case we must insure that the (legal) program does
      --  raise program error if executed. We generate a subprogram body for
      --  this purpose. See DEC ac30vso.

      elsif Serious_Errors_Detected = 0
        and then Nkind (Parent (Inst_Node)) /= N_Compilation_Unit
      then
         if Ekind (Anon_Id) = E_Procedure then
            Act_Body :=
              Make_Subprogram_Body (Loc,
                 Specification              =>
                   Make_Procedure_Specification (Loc,
                     Defining_Unit_Name         =>
                       Make_Defining_Identifier (Loc, Chars (Anon_Id)),
                       Parameter_Specifications =>
                       New_Copy_List
                         (Parameter_Specifications (Parent (Anon_Id)))),

                 Declarations               => Empty_List,
                 Handled_Statement_Sequence =>
                   Make_Handled_Sequence_Of_Statements (Loc,
                     Statements =>
                       New_List (
                         Make_Raise_Program_Error (Loc,
                           Reason =>
                             PE_Access_Before_Elaboration))));

         else
            Ret_Expr :=
              Make_Raise_Program_Error (Loc,
                Reason => PE_Access_Before_Elaboration);

            Set_Etype (Ret_Expr, (Etype (Anon_Id)));
            Set_Analyzed (Ret_Expr);

            Act_Body :=
              Make_Subprogram_Body (Loc,
                Specification =>
                  Make_Function_Specification (Loc,
                     Defining_Unit_Name         =>
                       Make_Defining_Identifier (Loc, Chars (Anon_Id)),
                       Parameter_Specifications =>
                       New_Copy_List
                         (Parameter_Specifications (Parent (Anon_Id))),
                     Result_Definition =>
                       New_Occurrence_Of (Etype (Anon_Id), Loc)),

                  Declarations               => Empty_List,
                  Handled_Statement_Sequence =>
                    Make_Handled_Sequence_Of_Statements (Loc,
                      Statements =>
                        New_List (Make_Return_Statement (Loc, Ret_Expr))));
         end if;

         Pack_Body := Make_Package_Body (Loc,
           Defining_Unit_Name => New_Copy (Pack_Id),
           Declarations       => New_List (Act_Body));

         Insert_After (Inst_Node, Pack_Body);
         Set_Corresponding_Spec (Pack_Body, Pack_Id);
         Analyze (Pack_Body);
      end if;

      Expander_Mode_Restore;
   end Instantiate_Subprogram_Body;

   ----------------------
   -- Instantiate_Type --
   ----------------------

   function Instantiate_Type
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id;
      Actual_Decls    : List_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Actual);
      Gen_T     : constant Entity_Id  := Defining_Identifier (Formal);
      A_Gen_T   : constant Entity_Id  := Defining_Identifier (Analyzed_Formal);
      Ancestor  : Entity_Id := Empty;
      Def       : constant Node_Id    := Formal_Type_Definition (Formal);
      Act_T     : Entity_Id;
      Decl_Node : Node_Id;

      procedure Validate_Array_Type_Instance;
      procedure Validate_Access_Subprogram_Instance;
      procedure Validate_Access_Type_Instance;
      procedure Validate_Derived_Type_Instance;
      procedure Validate_Derived_Interface_Type_Instance;
      procedure Validate_Interface_Type_Instance;
      procedure Validate_Private_Type_Instance;
      --  These procedures perform validation tests for the named case

      function Subtypes_Match (Gen_T, Act_T : Entity_Id) return Boolean;
      --  Check that base types are the same and that the subtypes match
      --  statically. Used in several of the above.

      --------------------
      -- Subtypes_Match --
      --------------------

      function Subtypes_Match (Gen_T, Act_T : Entity_Id) return Boolean is
         T : constant Entity_Id := Get_Instance_Of (Gen_T);

      begin
         return (Base_Type (T) = Base_Type (Act_T)
--  why is the and then commented out here???
--                  and then Is_Constrained (T) = Is_Constrained (Act_T)
                  and then Subtypes_Statically_Match (T, Act_T))

           or else (Is_Class_Wide_Type (Gen_T)
                     and then Is_Class_Wide_Type (Act_T)
                     and then
                       Subtypes_Match (
                         Get_Instance_Of (Root_Type (Gen_T)),
                         Root_Type (Act_T)));
      end Subtypes_Match;

      -----------------------------------------
      -- Validate_Access_Subprogram_Instance --
      -----------------------------------------

      procedure Validate_Access_Subprogram_Instance is
      begin
         if not Is_Access_Type (Act_T)
           or else Ekind (Designated_Type (Act_T)) /= E_Subprogram_Type
         then
            Error_Msg_NE
              ("expect access type in instantiation of &", Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         Check_Mode_Conformant
           (Designated_Type (Act_T),
            Designated_Type (A_Gen_T),
            Actual,
            Get_Inst => True);

         if Ekind (Base_Type (Act_T)) = E_Access_Protected_Subprogram_Type then
            if Ekind (A_Gen_T) = E_Access_Subprogram_Type then
               Error_Msg_NE
                 ("protected access type not allowed for formal &",
                  Actual, Gen_T);
            end if;

         elsif Ekind (A_Gen_T) = E_Access_Protected_Subprogram_Type then
            Error_Msg_NE
              ("expect protected access type for formal &",
               Actual, Gen_T);
         end if;
      end Validate_Access_Subprogram_Instance;

      -----------------------------------
      -- Validate_Access_Type_Instance --
      -----------------------------------

      procedure Validate_Access_Type_Instance is
         Desig_Type : constant Entity_Id :=
                        Find_Actual_Type
                          (Designated_Type (A_Gen_T), Scope (A_Gen_T));

      begin
         if not Is_Access_Type (Act_T) then
            Error_Msg_NE
              ("expect access type in instantiation of &", Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         if Is_Access_Constant (A_Gen_T) then
            if not Is_Access_Constant (Act_T) then
               Error_Msg_N
                 ("actual type must be access-to-constant type", Actual);
               Abandon_Instantiation (Actual);
            end if;
         else
            if Is_Access_Constant (Act_T) then
               Error_Msg_N
                 ("actual type must be access-to-variable type", Actual);
               Abandon_Instantiation (Actual);

            elsif Ekind (A_Gen_T) = E_General_Access_Type
              and then Ekind (Base_Type (Act_T)) /= E_General_Access_Type
            then
               Error_Msg_N ("actual must be general access type!", Actual);
               Error_Msg_NE ("add ALL to }!", Actual, Act_T);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         --  The designated subtypes, that is to say the subtypes introduced
         --  by an access type declaration (and not by a subtype declaration)
         --  must match.

         if not Subtypes_Match
           (Desig_Type, Designated_Type (Base_Type (Act_T)))
         then
            Error_Msg_NE
              ("designated type of actual does not match that of formal &",
                 Actual, Gen_T);
            Abandon_Instantiation (Actual);

         elsif Is_Access_Type (Designated_Type (Act_T))
           and then Is_Constrained (Designated_Type (Designated_Type (Act_T)))
                      /=
                  Is_Constrained (Designated_Type (Desig_Type))
         then
            Error_Msg_NE
              ("designated type of actual does not match that of formal &",
                 Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;
      end Validate_Access_Type_Instance;

      ----------------------------------
      -- Validate_Array_Type_Instance --
      ----------------------------------

      procedure Validate_Array_Type_Instance is
         I1 : Node_Id;
         I2 : Node_Id;
         T2 : Entity_Id;

         function Formal_Dimensions return Int;
         --  Count number of dimensions in array type formal

         -----------------------
         -- Formal_Dimensions --
         -----------------------

         function Formal_Dimensions return Int is
            Num   : Int := 0;
            Index : Node_Id;

         begin
            if Nkind (Def) = N_Constrained_Array_Definition then
               Index := First (Discrete_Subtype_Definitions (Def));
            else
               Index := First (Subtype_Marks (Def));
            end if;

            while Present (Index) loop
               Num := Num + 1;
               Next_Index (Index);
            end loop;

            return Num;
         end Formal_Dimensions;

      --  Start of processing for Validate_Array_Type_Instance

      begin
         if not Is_Array_Type (Act_T) then
            Error_Msg_NE
              ("expect array type in instantiation of &", Actual, Gen_T);
            Abandon_Instantiation (Actual);

         elsif Nkind (Def) = N_Constrained_Array_Definition then
            if not (Is_Constrained (Act_T)) then
               Error_Msg_NE
                 ("expect constrained array in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         else
            if Is_Constrained (Act_T) then
               Error_Msg_NE
                 ("expect unconstrained array in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         if Formal_Dimensions /= Number_Dimensions (Act_T) then
            Error_Msg_NE
              ("dimensions of actual do not match formal &", Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         I1 := First_Index (A_Gen_T);
         I2 := First_Index (Act_T);
         for J in 1 .. Formal_Dimensions loop

            --  If the indices of the actual were given by a subtype_mark,
            --  the index was transformed into a range attribute. Retrieve
            --  the original type mark for checking.

            if Is_Entity_Name (Original_Node (I2)) then
               T2 := Entity (Original_Node (I2));
            else
               T2 := Etype (I2);
            end if;

            if not Subtypes_Match
              (Find_Actual_Type (Etype (I1), Scope (A_Gen_T)), T2)
            then
               Error_Msg_NE
                 ("index types of actual do not match those of formal &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

            Next_Index (I1);
            Next_Index (I2);
         end loop;

         if not Subtypes_Match (
            Find_Actual_Type (Component_Type (A_Gen_T), Scope (A_Gen_T)),
            Component_Type (Act_T))
         then
            Error_Msg_NE
              ("component subtype of actual does not match that of formal &",
               Actual, Gen_T);
            Abandon_Instantiation (Actual);
         end if;

         if Has_Aliased_Components (A_Gen_T)
           and then not Has_Aliased_Components (Act_T)
         then
            Error_Msg_NE
              ("actual must have aliased components to match formal type &",
               Actual, Gen_T);
         end if;

      end Validate_Array_Type_Instance;

      -----------------------------------------------
      --  Validate_Derived_Interface_Type_Instance --
      -----------------------------------------------

      procedure Validate_Derived_Interface_Type_Instance is
         Par  : constant Entity_Id := Entity (Subtype_Indication (Def));
         Elmt : Elmt_Id;

      begin
         --  First apply interface instance checks

         Validate_Interface_Type_Instance;

         --  Verify that immediate parent interface is an ancestor of
         --  the actual.

         if Present (Par)
           and then  not Interface_Present_In_Ancestor (Act_T, Par)
         then
            Error_Msg_NE
              ("interface actual must include progenitor&", Actual, Par);
         end if;

         --  Now verify that the actual includes all other ancestors of
         --  the formal.

         Elmt := First_Elmt (Abstract_Interfaces (A_Gen_T));
         while Present (Elmt) loop
            if not Interface_Present_In_Ancestor (Act_T, Node (Elmt)) then
               Error_Msg_NE
                 ("interface actual must include progenitor&",
                    Actual, Node (Elmt));
            end if;

            Next_Elmt (Elmt);
         end loop;
      end Validate_Derived_Interface_Type_Instance;

      ------------------------------------
      -- Validate_Derived_Type_Instance --
      ------------------------------------

      procedure Validate_Derived_Type_Instance is
         Actual_Discr   : Entity_Id;
         Ancestor_Discr : Entity_Id;

      begin
         --  If the parent type in the generic declaration is itself a previous
         --  formal type, then it is local to the generic and absent from the
         --  analyzed generic definition. In that case the ancestor is the
         --  instance of the formal (which must have been instantiated
         --  previously), unless the ancestor is itself a formal derived type.
         --  In this latter case (which is the subject of Corrigendum 8652/0038
         --  (AI-202) the ancestor of the formals is the ancestor of its
         --  parent. Otherwise, the analyzed generic carries the parent type.
         --  If the parent type is defined in a previous formal package, then
         --  the scope of that formal package is that of the generic type
         --  itself, and it has already been mapped into the corresponding type
         --  in the actual package.

         --  Common case: parent type defined outside of the generic

         if Is_Entity_Name (Subtype_Mark (Def))
           and then Present (Entity (Subtype_Mark (Def)))
         then
            Ancestor := Get_Instance_Of (Entity (Subtype_Mark (Def)));

         --  Check whether parent is defined in a previous formal package

         elsif
           Scope (Scope (Base_Type (Etype (A_Gen_T)))) = Scope (A_Gen_T)
         then
            Ancestor :=
              Get_Instance_Of (Base_Type (Etype (A_Gen_T)));

         --  The type may be a local derivation, or a type extension of
         --  a previous formal, or of a formal of a parent package.

         elsif Is_Derived_Type (Get_Instance_Of (A_Gen_T))
          or else
            Ekind (Get_Instance_Of (A_Gen_T)) = E_Record_Type_With_Private
         then
            --  Check whether the parent is another derived formal type
            --  in the same generic unit.

            if Etype (A_Gen_T) /= A_Gen_T
              and then Is_Generic_Type (Etype (A_Gen_T))
              and then Scope (Etype (A_Gen_T)) = Scope (A_Gen_T)
              and then Etype (Etype (A_Gen_T)) /= Etype (A_Gen_T)
            then
               --  Locate ancestor of parent from the subtype declaration
               --  created for the actual.

               declare
                  Decl : Node_Id;

               begin
                  Decl := First (Actual_Decls);
                  while Present (Decl) loop
                     if Nkind (Decl) = N_Subtype_Declaration
                       and then Chars (Defining_Identifier (Decl)) =
                                                    Chars (Etype (A_Gen_T))
                     then
                        Ancestor := Generic_Parent_Type (Decl);
                        exit;
                     else
                        Next (Decl);
                     end if;
                  end loop;
               end;

               pragma Assert (Present (Ancestor));

            else
               Ancestor :=
                 Get_Instance_Of (Base_Type (Get_Instance_Of (A_Gen_T)));
            end if;

         else
            Ancestor := Get_Instance_Of (Etype (Base_Type (A_Gen_T)));
         end if;

         --  Ada 2005 (AI-251)

         if Ada_Version >= Ada_05
           and then Is_Interface (Ancestor)
         then
            if not Interface_Present_In_Ancestor (Act_T, Ancestor) then
               Error_Msg_NE
                 ("(Ada 2005) expected type implementing & in instantiation",
                  Actual, Ancestor);
            end if;

         elsif not Is_Ancestor (Base_Type (Ancestor), Act_T) then
            Error_Msg_NE
              ("expect type derived from & in instantiation",
               Actual, First_Subtype (Ancestor));
            Abandon_Instantiation (Actual);
         end if;

         --  Perform atomic/volatile checks (RM C.6(12))

         if Is_Atomic (Act_T) and then not Is_Atomic (Ancestor) then
            Error_Msg_N
              ("cannot have atomic actual type for non-atomic formal type",
               Actual);

         elsif Is_Volatile (Act_T)
           and then not Is_Volatile (Ancestor)
           and then Is_By_Reference_Type (Ancestor)
         then
            Error_Msg_N
              ("cannot have volatile actual type for non-volatile formal type",
               Actual);
         end if;

         --  It should not be necessary to check for unknown discriminants
         --  on Formal, but for some reason Has_Unknown_Discriminants is
         --  false for A_Gen_T, so Is_Indefinite_Subtype incorrectly
         --  returns False. This needs fixing. ???

         if not Is_Indefinite_Subtype (A_Gen_T)
           and then not Unknown_Discriminants_Present (Formal)
           and then Is_Indefinite_Subtype (Act_T)
         then
            Error_Msg_N
              ("actual subtype must be constrained", Actual);
            Abandon_Instantiation (Actual);
         end if;

         if not Unknown_Discriminants_Present (Formal) then
            if Is_Constrained (Ancestor) then
               if not Is_Constrained (Act_T) then
                  Error_Msg_N
                    ("actual subtype must be constrained", Actual);
                  Abandon_Instantiation (Actual);
               end if;

            --  Ancestor is unconstrained

            elsif Is_Constrained (Act_T) then
               if Ekind (Ancestor) = E_Access_Type
                 or else Is_Composite_Type (Ancestor)
               then
                  Error_Msg_N
                    ("actual subtype must be unconstrained", Actual);
                  Abandon_Instantiation (Actual);
               end if;

            --  A class-wide type is only allowed if the formal has
            --  unknown discriminants.

            elsif Is_Class_Wide_Type (Act_T)
              and then not Has_Unknown_Discriminants (Ancestor)
            then
               Error_Msg_NE
                 ("actual for & cannot be a class-wide type", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            --  Otherwise, the formal and actual shall have the same
            --  number of discriminants and each discriminant of the
            --  actual must correspond to a discriminant of the formal.

            elsif Has_Discriminants (Act_T)
              and then not Has_Unknown_Discriminants (Act_T)
              and then Has_Discriminants (Ancestor)
            then
               Actual_Discr   := First_Discriminant (Act_T);
               Ancestor_Discr := First_Discriminant (Ancestor);
               while Present (Actual_Discr)
                 and then Present (Ancestor_Discr)
               loop
                  if Base_Type (Act_T) /= Base_Type (Ancestor) and then
                    not Present (Corresponding_Discriminant (Actual_Discr))
                  then
                     Error_Msg_NE
                       ("discriminant & does not correspond " &
                        "to ancestor discriminant", Actual, Actual_Discr);
                     Abandon_Instantiation (Actual);
                  end if;

                  Next_Discriminant (Actual_Discr);
                  Next_Discriminant (Ancestor_Discr);
               end loop;

               if Present (Actual_Discr) or else Present (Ancestor_Discr) then
                  Error_Msg_NE
                    ("actual for & must have same number of discriminants",
                     Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;

            --  This case should be caught by the earlier check for
            --  for constrainedness, but the check here is added for
            --  completeness.

            elsif Has_Discriminants (Act_T)
              and then not Has_Unknown_Discriminants (Act_T)
            then
               Error_Msg_NE
                 ("actual for & must not have discriminants", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            elsif Has_Discriminants (Ancestor) then
               Error_Msg_NE
                 ("actual for & must have known discriminants", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

            if not Subtypes_Statically_Compatible (Act_T, Ancestor) then
               Error_Msg_N
                 ("constraint on actual is incompatible with formal", Actual);
               Abandon_Instantiation (Actual);
            end if;
         end if;
      end Validate_Derived_Type_Instance;

      --------------------------------------
      -- Validate_Interface_Type_Instance --
      --------------------------------------

      procedure Validate_Interface_Type_Instance is
      begin
         if not Is_Interface (Act_T) then
            Error_Msg_NE
              ("actual for formal interface type must be an interface",
                Actual, Gen_T);

         elsif Is_Limited_Type (Act_T) /= Is_Limited_Type (A_Gen_T)
           or else
             Is_Task_Interface (A_Gen_T) /= Is_Task_Interface (Act_T)
           or else
             Is_Protected_Interface (A_Gen_T) /=
               Is_Protected_Interface (Act_T)
           or else
             Is_Synchronized_Interface (A_Gen_T) /=
               Is_Synchronized_Interface (Act_T)
         then
            Error_Msg_NE
              ("actual for interface& does not match ('R'M 12.5.5(5))",
                 Actual, Gen_T);
         end if;
      end Validate_Interface_Type_Instance;

      ------------------------------------
      -- Validate_Private_Type_Instance --
      ------------------------------------

      procedure Validate_Private_Type_Instance is
         Formal_Discr : Entity_Id;
         Actual_Discr : Entity_Id;
         Formal_Subt  : Entity_Id;

      begin
         if Is_Limited_Type (Act_T)
           and then not Is_Limited_Type (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for non-limited  & cannot be a limited type", Actual,
               Gen_T);
            Explain_Limited_Type (Act_T, Actual);
            Abandon_Instantiation (Actual);

         elsif Is_Indefinite_Subtype (Act_T)
            and then not Is_Indefinite_Subtype (A_Gen_T)
            and then Ada_Version >= Ada_95
         then
            Error_Msg_NE
              ("actual for & must be a definite subtype", Actual, Gen_T);

         elsif not Is_Tagged_Type (Act_T)
           and then Is_Tagged_Type (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for & must be a tagged type", Actual, Gen_T);

         elsif Has_Discriminants (A_Gen_T) then
            if not Has_Discriminants (Act_T) then
               Error_Msg_NE
                 ("actual for & must have discriminants", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            elsif Is_Constrained (Act_T) then
               Error_Msg_NE
                 ("actual for & must be unconstrained", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            else
               Formal_Discr := First_Discriminant (A_Gen_T);
               Actual_Discr := First_Discriminant (Act_T);
               while Formal_Discr /= Empty loop
                  if Actual_Discr = Empty then
                     Error_Msg_NE
                       ("discriminants on actual do not match formal",
                        Actual, Gen_T);
                     Abandon_Instantiation (Actual);
                  end if;

                  Formal_Subt := Get_Instance_Of (Etype (Formal_Discr));

                  --  Access discriminants match if designated types do

                  if Ekind (Base_Type (Formal_Subt)) = E_Anonymous_Access_Type
                    and then (Ekind (Base_Type (Etype (Actual_Discr)))) =
                                E_Anonymous_Access_Type
                    and then
                      Get_Instance_Of
                        (Designated_Type (Base_Type (Formal_Subt))) =
                           Designated_Type (Base_Type (Etype (Actual_Discr)))
                  then
                     null;

                  elsif Base_Type (Formal_Subt) /=
                          Base_Type (Etype (Actual_Discr))
                  then
                     Error_Msg_NE
                       ("types of actual discriminants must match formal",
                        Actual, Gen_T);
                     Abandon_Instantiation (Actual);

                  elsif not Subtypes_Statically_Match
                              (Formal_Subt, Etype (Actual_Discr))
                    and then Ada_Version >= Ada_95
                  then
                     Error_Msg_NE
                       ("subtypes of actual discriminants must match formal",
                        Actual, Gen_T);
                     Abandon_Instantiation (Actual);
                  end if;

                  Next_Discriminant (Formal_Discr);
                  Next_Discriminant (Actual_Discr);
               end loop;

               if Actual_Discr /= Empty then
                  Error_Msg_NE
                    ("discriminants on actual do not match formal",
                     Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;
            end if;

         end if;

         Ancestor := Gen_T;
      end Validate_Private_Type_Instance;

   --  Start of processing for Instantiate_Type

   begin
      if Get_Instance_Of (A_Gen_T) /= A_Gen_T then
         Error_Msg_N ("duplicate instantiation of generic type", Actual);
         return Error;

      elsif not Is_Entity_Name (Actual)
        or else not Is_Type (Entity (Actual))
      then
         Error_Msg_NE
           ("expect valid subtype mark to instantiate &", Actual, Gen_T);
         Abandon_Instantiation (Actual);

      else
         Act_T := Entity (Actual);

         --  Ada 2005 (AI-216): An Unchecked_Union subtype shall only be passed
         --  as a generic actual parameter if the corresponding formal type
         --  does not have a known_discriminant_part, or is a formal derived
         --  type that is an Unchecked_Union type.

         if Is_Unchecked_Union (Base_Type (Act_T)) then
            if not Has_Discriminants (A_Gen_T)
                     or else
                   (Is_Derived_Type (A_Gen_T)
                     and then
                    Is_Unchecked_Union (A_Gen_T))
            then
               null;
            else
               Error_Msg_N ("Unchecked_Union cannot be the actual for a" &
                 " discriminated formal type", Act_T);

            end if;
         end if;

         --  Deal with fixed/floating restrictions

         if Is_Floating_Point_Type (Act_T) then
            Check_Restriction (No_Floating_Point, Actual);
         elsif Is_Fixed_Point_Type (Act_T) then
            Check_Restriction (No_Fixed_Point, Actual);
         end if;

         --  Deal with error of using incomplete type as generic actual

         if Ekind (Act_T) = E_Incomplete_Type then
            if No (Underlying_Type (Act_T)) then
               Error_Msg_N ("premature use of incomplete type", Actual);
               Abandon_Instantiation (Actual);
            else
               Act_T := Full_View (Act_T);
               Set_Entity (Actual, Act_T);

               if Has_Private_Component (Act_T) then
                  Error_Msg_N
                    ("premature use of type with private component", Actual);
               end if;
            end if;

         --  Deal with error of premature use of private type as generic actual

         elsif Is_Private_Type (Act_T)
           and then Is_Private_Type (Base_Type (Act_T))
           and then not Is_Generic_Type (Act_T)
           and then not Is_Derived_Type (Act_T)
           and then No (Full_View (Root_Type (Act_T)))
         then
            Error_Msg_N ("premature use of private type", Actual);

         elsif Has_Private_Component (Act_T) then
            Error_Msg_N
              ("premature use of type with private component", Actual);
         end if;

         Set_Instance_Of (A_Gen_T, Act_T);

         --  If the type is generic, the class-wide type may also be used

         if Is_Tagged_Type (A_Gen_T)
           and then Is_Tagged_Type (Act_T)
           and then not Is_Class_Wide_Type (A_Gen_T)
         then
            Set_Instance_Of (Class_Wide_Type (A_Gen_T),
              Class_Wide_Type (Act_T));
         end if;

         if not Is_Abstract (A_Gen_T)
           and then Is_Abstract (Act_T)
         then
            Error_Msg_N
              ("actual of non-abstract formal cannot be abstract", Actual);
         end if;

         if Is_Scalar_Type (Gen_T) then
            Set_Instance_Of (Etype (A_Gen_T), Etype (Act_T));
         end if;
      end if;

      case Nkind (Def) is
         when N_Formal_Private_Type_Definition =>
            Validate_Private_Type_Instance;

         when N_Formal_Derived_Type_Definition =>
            Validate_Derived_Type_Instance;

         when N_Formal_Discrete_Type_Definition =>
            if not Is_Discrete_Type (Act_T) then
               Error_Msg_NE
                 ("expect discrete type in instantiation of&", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Signed_Integer_Type_Definition =>
            if not Is_Signed_Integer_Type (Act_T) then
               Error_Msg_NE
                 ("expect signed integer type in instantiation of&",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Modular_Type_Definition =>
            if not Is_Modular_Integer_Type (Act_T) then
               Error_Msg_NE
                 ("expect modular type in instantiation of &", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Floating_Point_Definition =>
            if not Is_Floating_Point_Type (Act_T) then
               Error_Msg_NE
                 ("expect float type in instantiation of &", Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            if not Is_Ordinary_Fixed_Point_Type (Act_T) then
               Error_Msg_NE
                 ("expect ordinary fixed point type in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Formal_Decimal_Fixed_Point_Definition =>
            if not Is_Decimal_Fixed_Point_Type (Act_T) then
               Error_Msg_NE
                 ("expect decimal type in instantiation of &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

         when N_Array_Type_Definition =>
            Validate_Array_Type_Instance;

         when N_Access_To_Object_Definition =>
            Validate_Access_Type_Instance;

         when N_Access_Function_Definition |
              N_Access_Procedure_Definition =>
            Validate_Access_Subprogram_Instance;

         when N_Record_Definition           =>
            Validate_Interface_Type_Instance;

         when N_Derived_Type_Definition     =>
            Validate_Derived_Interface_Type_Instance;

         when others =>
            raise Program_Error;

      end case;

      Decl_Node :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => New_Copy (Gen_T),
          Subtype_Indication  => New_Reference_To (Act_T, Loc));

      if Is_Private_Type (Act_T) then
         Set_Has_Private_View (Subtype_Indication (Decl_Node));

      elsif Is_Access_Type (Act_T)
        and then Is_Private_Type (Designated_Type (Act_T))
      then
         Set_Has_Private_View (Subtype_Indication (Decl_Node));
      end if;

      --  Flag actual derived types so their elaboration produces the
      --  appropriate renamings for the primitive operations of the ancestor.
      --  Flag actual for formal private types as well, to determine whether
      --  operations in the private part may override inherited operations.

      if Nkind (Def) = N_Formal_Derived_Type_Definition
        or else Nkind (Def) = N_Formal_Private_Type_Definition
      then
         Set_Generic_Parent_Type (Decl_Node, Ancestor);
      end if;

      return Decl_Node;
   end Instantiate_Type;

   ---------------------
   -- Is_In_Main_Unit --
   ---------------------

   function Is_In_Main_Unit (N : Node_Id) return Boolean is
      Unum         : constant Unit_Number_Type := Get_Source_Unit (N);
      Current_Unit : Node_Id;

   begin
      if Unum = Main_Unit then
         return True;

      --  If the current unit is a subunit then it is either the main unit
      --  or is being compiled as part of the main unit.

      elsif Nkind (N) = N_Compilation_Unit then
         return Nkind (Unit (N)) = N_Subunit;
      end if;

      Current_Unit := Parent (N);
      while Present (Current_Unit)
        and then Nkind (Current_Unit) /= N_Compilation_Unit
      loop
         Current_Unit := Parent (Current_Unit);
      end loop;

      --  The instantiation node is in the main unit, or else the current
      --  node (perhaps as the result of nested instantiations) is in the
      --  main unit, or in the declaration of the main unit, which in this
      --  last case must be a body.

      return Unum = Main_Unit
        or else Current_Unit = Cunit (Main_Unit)
        or else Current_Unit = Library_Unit (Cunit (Main_Unit))
        or else (Present (Library_Unit (Current_Unit))
                  and then Is_In_Main_Unit (Library_Unit (Current_Unit)));
   end Is_In_Main_Unit;

   ----------------------------
   -- Load_Parent_Of_Generic --
   ----------------------------

   procedure Load_Parent_Of_Generic (N : Node_Id; Spec : Node_Id) is
      Comp_Unit        : constant Node_Id := Cunit (Get_Source_Unit (Spec));
      Save_Style_Check : constant Boolean := Style_Check;
      True_Parent      : Node_Id;
      Inst_Node        : Node_Id;
      OK               : Boolean;

   begin
      if not In_Same_Source_Unit (N, Spec)
        or else Nkind (Unit (Comp_Unit)) = N_Package_Declaration
        or else (Nkind (Unit (Comp_Unit)) = N_Package_Body
                   and then not Is_In_Main_Unit (Spec))
      then
         --  Find body of parent of spec, and analyze it. A special case
         --  arises when the parent is an instantiation, that is to say when
         --  we are currently instantiating a nested generic. In that case,
         --  there is no separate file for the body of the enclosing instance.
         --  Instead, the enclosing body must be instantiated as if it were
         --  a pending instantiation, in order to produce the body for the
         --  nested generic we require now. Note that in that case the
         --  generic may be defined in a package body, the instance defined
         --  in the same package body, and the original enclosing body may not
         --  be in the main unit.

         True_Parent := Parent (Spec);
         Inst_Node   := Empty;

         while Present (True_Parent)
           and then Nkind (True_Parent) /= N_Compilation_Unit
         loop
            if Nkind (True_Parent) = N_Package_Declaration
              and then
                Nkind (Original_Node (True_Parent)) = N_Package_Instantiation
            then
               --  Parent is a compilation unit that is an instantiation.
               --  Instantiation node has been replaced with package decl.

               Inst_Node := Original_Node (True_Parent);
               exit;

            elsif Nkind (True_Parent) = N_Package_Declaration
              and then Present (Generic_Parent (Specification (True_Parent)))
              and then Nkind (Parent (True_Parent)) /= N_Compilation_Unit
            then
               --  Parent is an instantiation within another specification.
               --  Declaration for instance has been inserted before original
               --  instantiation node. A direct link would be preferable?

               Inst_Node := Next (True_Parent);

               while Present (Inst_Node)
                 and then Nkind (Inst_Node) /= N_Package_Instantiation
               loop
                  Next (Inst_Node);
               end loop;

               --  If the instance appears within a generic, and the generic
               --  unit is defined within a formal package of the enclosing
               --  generic, there is no generic body available, and none
               --  needed. A more precise test should be used ???

               if No (Inst_Node) then
                  return;
               end if;

               exit;
            else
               True_Parent := Parent (True_Parent);
            end if;
         end loop;

         --  Case where we are currently instantiating a nested generic

         if Present (Inst_Node) then
            if Nkind (Parent (True_Parent)) = N_Compilation_Unit then

               --  Instantiation node and declaration of instantiated package
               --  were exchanged when only the declaration was needed.
               --  Restore instantiation node before proceeding with body.

               Set_Unit (Parent (True_Parent), Inst_Node);
            end if;

            --  Now complete instantiation of enclosing body, if it appears
            --  in some other unit. If it appears in the current unit, the
            --  body will have been instantiated already.

            if No (Corresponding_Body (Instance_Spec (Inst_Node))) then

               --  We need to determine the expander mode to instantiate
               --  the enclosing body. Because the generic body we need
               --  may use global entities declared in the enclosing package
               --  (including aggregates) it is in general necessary to
               --  compile this body with expansion enabled. The exception
               --  is if we are within a generic package, in which case
               --  the usual generic rule applies.

               declare
                  Exp_Status : Boolean := True;
                  Scop       : Entity_Id;

               begin
                  --  Loop through scopes looking for generic package

                  Scop := Scope (Defining_Entity (Instance_Spec (Inst_Node)));
                  while Present (Scop)
                    and then Scop /= Standard_Standard
                  loop
                     if Ekind (Scop) = E_Generic_Package then
                        Exp_Status := False;
                        exit;
                     end if;

                     Scop := Scope (Scop);
                  end loop;

                  Instantiate_Package_Body
                    (Pending_Body_Info'(
                       Inst_Node, True_Parent, Exp_Status,
                         Get_Code_Unit (Sloc (Inst_Node))));
               end;
            end if;

         --  Case where we are not instantiating a nested generic

         else
            Opt.Style_Check := False;
            Expander_Mode_Save_And_Set (True);
            Load_Needed_Body (Comp_Unit, OK);
            Opt.Style_Check := Save_Style_Check;
            Expander_Mode_Restore;

            if not OK
              and then Unit_Requires_Body (Defining_Entity (Spec))
            then
               declare
                  Bname : constant Unit_Name_Type :=
                            Get_Body_Name (Get_Unit_Name (Unit (Comp_Unit)));

               begin
                  Error_Msg_Unit_1 := Bname;
                  Error_Msg_N ("this instantiation requires$!", N);
                  Error_Msg_Name_1 :=
                    Get_File_Name (Bname, Subunit => False);
                  Error_Msg_N ("\but file{ was not found!", N);
                  raise Unrecoverable_Error;
               end;
            end if;
         end if;
      end if;

      --  If loading the parent of the generic caused an instantiation
      --  circularity, we abandon compilation at this point, because
      --  otherwise in some cases we get into trouble with infinite
      --  recursions after this point.

      if Circularity_Detected then
         raise Unrecoverable_Error;
      end if;
   end Load_Parent_Of_Generic;

   -----------------------
   -- Move_Freeze_Nodes --
   -----------------------

   procedure Move_Freeze_Nodes
     (Out_Of : Entity_Id;
      After  : Node_Id;
      L      : List_Id)
   is
      Decl      : Node_Id;
      Next_Decl : Node_Id;
      Next_Node : Node_Id := After;
      Spec      : Node_Id;

      function Is_Outer_Type (T : Entity_Id) return Boolean;
      --  Check whether entity is declared in a scope external to that
      --  of the generic unit.

      -------------------
      -- Is_Outer_Type --
      -------------------

      function Is_Outer_Type (T : Entity_Id) return Boolean is
         Scop : Entity_Id := Scope (T);

      begin
         if Scope_Depth (Scop) < Scope_Depth (Out_Of) then
            return True;

         else
            while Scop /= Standard_Standard loop

               if Scop = Out_Of then
                  return False;
               else
                  Scop := Scope (Scop);
               end if;
            end loop;

            return True;
         end if;
      end Is_Outer_Type;

   --  Start of processing for Move_Freeze_Nodes

   begin
      if No (L) then
         return;
      end if;

      --  First remove the freeze nodes that may appear before all other
      --  declarations.

      Decl := First (L);
      while Present (Decl)
        and then Nkind (Decl) = N_Freeze_Entity
        and then Is_Outer_Type (Entity (Decl))
      loop
         Decl := Remove_Head (L);
         Insert_After (Next_Node, Decl);
         Set_Analyzed (Decl, False);
         Next_Node := Decl;
         Decl := First (L);
      end loop;

      --  Next scan the list of declarations and remove each freeze node that
      --  appears ahead of the current node.

      while Present (Decl) loop
         while Present (Next (Decl))
           and then Nkind (Next (Decl)) = N_Freeze_Entity
           and then Is_Outer_Type (Entity (Next (Decl)))
         loop
            Next_Decl := Remove_Next (Decl);
            Insert_After (Next_Node, Next_Decl);
            Set_Analyzed (Next_Decl, False);
            Next_Node := Next_Decl;
         end loop;

         --  If the declaration is a nested package or concurrent type, then
         --  recurse. Nested generic packages will have been processed from the
         --  inside out.

         if Nkind (Decl) = N_Package_Declaration then
            Spec := Specification (Decl);

         elsif Nkind (Decl) = N_Task_Type_Declaration then
            Spec := Task_Definition (Decl);

         elsif Nkind (Decl) = N_Protected_Type_Declaration then
            Spec := Protected_Definition (Decl);

         else
            Spec := Empty;
         end if;

         if Present (Spec) then
            Move_Freeze_Nodes (Out_Of, Next_Node,
              Visible_Declarations (Spec));
            Move_Freeze_Nodes (Out_Of, Next_Node,
              Private_Declarations (Spec));
         end if;

         Next (Decl);
      end loop;
   end Move_Freeze_Nodes;

   ----------------
   -- Next_Assoc --
   ----------------

   function Next_Assoc (E : Assoc_Ptr) return Assoc_Ptr is
   begin
      return Generic_Renamings.Table (E).Next_In_HTable;
   end Next_Assoc;

   ------------------------
   -- Preanalyze_Actuals --
   ------------------------

   procedure Pre_Analyze_Actuals (N : Node_Id) is
      Assoc : Node_Id;
      Act   : Node_Id;
      Errs  : constant Int := Serious_Errors_Detected;

   begin
      Assoc := First (Generic_Associations (N));

      while Present (Assoc) loop
         Act := Explicit_Generic_Actual_Parameter (Assoc);

         --  Within a nested instantiation, a defaulted actual is an
         --  empty association, so nothing to analyze. If the actual for
         --  a subprogram is an attribute, analyze prefix only, because
         --  actual is not a complete attribute reference.

         --  If actual is an allocator, analyze expression only. The full
         --  analysis can generate code, and if the instance is a compilation
         --  unit we have to wait until the package instance is installed to
         --  have a proper place to insert this code.

         --  String literals may be operators, but at this point we do not
         --  know whether the actual is a formal subprogram or a string.

         if No (Act) then
            null;

         elsif Nkind (Act) = N_Attribute_Reference then
            Analyze (Prefix (Act));

         elsif Nkind (Act) = N_Explicit_Dereference then
            Analyze (Prefix (Act));

         elsif Nkind (Act) = N_Allocator then
            declare
               Expr : constant Node_Id := Expression (Act);

            begin
               if Nkind (Expr) = N_Subtype_Indication then
                  Analyze (Subtype_Mark (Expr));
                  Analyze_List (Constraints (Constraint (Expr)));
               else
                  Analyze (Expr);
               end if;
            end;

         elsif Nkind (Act) /= N_Operator_Symbol then
            Analyze (Act);
         end if;

         if Errs /= Serious_Errors_Detected then
            Abandon_Instantiation (Act);
         end if;

         Next (Assoc);
      end loop;
   end Pre_Analyze_Actuals;

   -------------------
   -- Remove_Parent --
   -------------------

   procedure Remove_Parent (In_Body : Boolean := False) is
      S      : Entity_Id := Current_Scope;
      E      : Entity_Id;
      P      : Entity_Id;
      Hidden : Elmt_Id;

   begin
      --  After child instantiation is complete, remove from scope stack
      --  the extra copy of the current scope, and then remove parent
      --  instances.

      if not In_Body then
         Pop_Scope;

         while Current_Scope /= S loop
            P := Current_Scope;
            End_Package_Scope (Current_Scope);

            if In_Open_Scopes (P) then
               E := First_Entity (P);

               while Present (E) loop
                  Set_Is_Immediately_Visible (E, True);
                  Next_Entity (E);
               end loop;

               if Is_Generic_Instance (Current_Scope)
                 and then P /= Current_Scope
               then
                  --  We are within an instance of some sibling. Retain
                  --  visibility of parent, for proper subsequent cleanup,
                  --  and reinstall private declarations as well.

                  Set_In_Private_Part (P);
                  Install_Private_Declarations (P);
               end if;

            --  If the ultimate parent is a top-level unit recorded in
            --  Instance_Parent_Unit, then reset its visibility to what
            --  it was before instantiation. (It's not clear what the
            --  purpose is of testing whether Scope (P) is In_Open_Scopes,
            --  but that test was present before the ultimate parent test
            --  was added.???)

            elsif not In_Open_Scopes (Scope (P))
              or else (P = Instance_Parent_Unit
                        and then not Parent_Unit_Visible)
            then
               Set_Is_Immediately_Visible (P, False);
            end if;
         end loop;

         --  Reset visibility of entities in the enclosing scope

         Set_Is_Hidden_Open_Scope (Current_Scope, False);
         Hidden := First_Elmt (Hidden_Entities);

         while Present (Hidden) loop
            Set_Is_Immediately_Visible (Node (Hidden), True);
            Next_Elmt (Hidden);
         end loop;

      else
         --  Each body is analyzed separately, and there is no context
         --  that needs preserving from one body instance to the next,
         --  so remove all parent scopes that have been installed.

         while Present (S) loop
            End_Package_Scope (S);
            Set_Is_Immediately_Visible (S, False);
            S := Current_Scope;
            exit when S = Standard_Standard;
         end loop;
      end if;

   end Remove_Parent;

   -----------------
   -- Restore_Env --
   -----------------

   procedure Restore_Env is
      Saved : Instance_Env renames Instance_Envs.Table (Instance_Envs.Last);

   begin
      Ada_Version := Saved.Ada_Version;
      Ada_Version_Explicit := Saved.Ada_Version_Explicit;

      if No (Current_Instantiated_Parent.Act_Id) then

         --  Restore environment after subprogram inlining

         Restore_Private_Views (Empty);
      end if;

      Current_Instantiated_Parent  := Saved.Instantiated_Parent;
      Exchanged_Views              := Saved.Exchanged_Views;
      Hidden_Entities              := Saved.Hidden_Entities;
      Current_Sem_Unit             := Saved.Current_Sem_Unit;
      Parent_Unit_Visible          := Saved.Parent_Unit_Visible;
      Instance_Parent_Unit         := Saved.Instance_Parent_Unit;

      Instance_Envs.Decrement_Last;
   end Restore_Env;

   ---------------------------
   -- Restore_Private_Views --
   ---------------------------

   procedure Restore_Private_Views
     (Pack_Id    : Entity_Id;
      Is_Package : Boolean := True)
   is
      M        : Elmt_Id;
      E        : Entity_Id;
      Typ      : Entity_Id;
      Dep_Elmt : Elmt_Id;
      Dep_Typ  : Node_Id;

      procedure Restore_Nested_Formal (Formal : Entity_Id);
      --  Hide the generic formals of formal packages declared with box
      --  which were reachable in the current instantiation.

      procedure Restore_Nested_Formal (Formal : Entity_Id) is
         Ent : Entity_Id;
      begin
         if Present (Renamed_Object (Formal))
           and then Denotes_Formal_Package (Renamed_Object (Formal), True)
         then
            return;

         elsif Present (Associated_Formal_Package (Formal))
          and then Box_Present (Parent (Associated_Formal_Package (Formal)))
         then
            Ent := First_Entity (Formal);

            while Present (Ent) loop
               exit when Ekind (Ent) = E_Package
                 and then Renamed_Entity (Ent) = Renamed_Entity (Formal);

               Set_Is_Hidden (Ent);
               Set_Is_Potentially_Use_Visible (Ent, False);

               --  If package, then recurse

               if Ekind (Ent) = E_Package then
                  Restore_Nested_Formal (Ent);
               end if;

               Next_Entity (Ent);
            end loop;
         end if;
      end Restore_Nested_Formal;

   begin
      M := First_Elmt (Exchanged_Views);
      while Present (M) loop
         Typ := Node (M);

         --  Subtypes of types whose views have been exchanged, and that
         --  are defined within the instance, were not on the list of
         --  Private_Dependents on entry to the instance, so they have to
         --  be exchanged explicitly now, in order to remain consistent with
         --  the view of the parent type.

         if Ekind (Typ) = E_Private_Type
           or else Ekind (Typ) = E_Limited_Private_Type
           or else Ekind (Typ) = E_Record_Type_With_Private
         then
            Dep_Elmt := First_Elmt (Private_Dependents (Typ));

            while Present (Dep_Elmt) loop
               Dep_Typ := Node (Dep_Elmt);

               if Scope (Dep_Typ) = Pack_Id
                 and then Present (Full_View (Dep_Typ))
               then
                  Replace_Elmt (Dep_Elmt, Full_View (Dep_Typ));
                  Exchange_Declarations (Dep_Typ);
               end if;

               Next_Elmt (Dep_Elmt);
            end loop;
         end if;

         Exchange_Declarations (Node (M));
         Next_Elmt (M);
      end loop;

      if No (Pack_Id) then
         return;
      end if;

      --  Make the generic formal parameters private, and make the formal
      --  types into subtypes of the actuals again.

      E := First_Entity (Pack_Id);

      while Present (E) loop
         Set_Is_Hidden (E, True);

         if Is_Type (E)
           and then Nkind (Parent (E)) = N_Subtype_Declaration
         then
            Set_Is_Generic_Actual_Type (E, False);

            --  An unusual case of aliasing: the actual may also be directly
            --  visible in the generic, and be private there, while it is
            --  fully visible in the context of the instance. The internal
            --  subtype is private in the instance, but has full visibility
            --  like its parent in the enclosing scope. This enforces the
            --  invariant that the privacy status of all private dependents of
            --  a type coincide with that of the parent type. This can only
            --  happen when a generic child unit is instantiated within a
            --  sibling.

            if Is_Private_Type (E)
              and then not Is_Private_Type (Etype (E))
            then
               Exchange_Declarations (E);
            end if;

         elsif Ekind (E) = E_Package then

            --  The end of the renaming list is the renaming of the generic
            --  package itself. If the instance is a subprogram, all entities
            --  in the corresponding package are renamings. If this entity is
            --  a formal package, make its own formals private as well. The
            --  actual in this case is itself the renaming of an instantation.
            --  If the entity is not a package renaming, it is the entity
            --  created to validate formal package actuals: ignore.

            --  If the actual is itself a formal package for the enclosing
            --  generic, or the actual for such a formal package, it remains
            --  visible on exit from the instance, and therefore nothing
            --  needs to be done either, except to keep it accessible.

            if Is_Package
              and then Renamed_Object (E) = Pack_Id
            then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            elsif Denotes_Formal_Package (Renamed_Object (E), True) then
               Set_Is_Hidden (E, False);

            else
               declare
                  Act_P : constant Entity_Id := Renamed_Object (E);
                  Id    : Entity_Id;

               begin
                  Id := First_Entity (Act_P);
                  while Present (Id)
                    and then Id /= First_Private_Entity (Act_P)
                  loop
                     exit when Ekind (Id) = E_Package
                                 and then Renamed_Object (Id) = Act_P;

                     Set_Is_Hidden (Id, True);
                     Set_Is_Potentially_Use_Visible (Id, In_Use (Act_P));

                     if Ekind (Id) = E_Package then
                        Restore_Nested_Formal (Id);
                     end if;

                     Next_Entity (Id);
                  end loop;
               end;
            end if;
         end if;

         Next_Entity (E);
      end loop;
   end Restore_Private_Views;

   --------------
   -- Save_Env --
   --------------

   procedure Save_Env
     (Gen_Unit : Entity_Id;
      Act_Unit : Entity_Id)
   is
   begin
      Init_Env;
      Set_Instance_Env (Gen_Unit, Act_Unit);
   end Save_Env;

   ----------------------------
   -- Save_Global_References --
   ----------------------------

   procedure Save_Global_References (N : Node_Id) is
      Gen_Scope : Entity_Id;
      E         : Entity_Id;
      N2        : Node_Id;

      function Is_Global (E : Entity_Id) return Boolean;
      --  Check whether entity is defined outside of generic unit.
      --  Examine the scope of an entity, and the scope of the scope,
      --  etc, until we find either Standard, in which case the entity
      --  is global, or the generic unit itself, which indicates that
      --  the entity is local. If the entity is the generic unit itself,
      --  as in the case of a recursive call, or the enclosing generic unit,
      --  if different from the current scope, then it is local as well,
      --  because it will be replaced at the point of instantiation. On
      --  the other hand, if it is a reference to a child unit of a common
      --  ancestor, which appears in an instantiation, it is global because
      --  it is used to denote a specific compilation unit at the time the
      --  instantiations will be analyzed.

      procedure Reset_Entity (N : Node_Id);
      --  Save semantic information on global entity, so that it is not
      --  resolved again at instantiation time.

      procedure Save_Entity_Descendants (N : Node_Id);
      --  Apply Save_Global_References to the two syntactic descendants of
      --  non-terminal nodes that carry an Associated_Node and are processed
      --  through Reset_Entity. Once the global entity (if any) has been
      --  captured together with its type, only two syntactic descendants
      --  need to be traversed to complete the processing of the tree rooted
      --  at N. This applies to Selected_Components, Expanded_Names, and to
      --  Operator nodes. N can also be a character literal, identifier, or
      --  operator symbol node, but the call has no effect in these cases.

      procedure Save_Global_Defaults (N1, N2 : Node_Id);
      --  Default actuals in nested instances must be handled specially
      --  because there is no link to them from the original tree. When an
      --  actual subprogram is given by a default, we add an explicit generic
      --  association for it in the instantiation node. When we save the
      --  global references on the name of the instance, we recover the list
      --  of generic associations, and add an explicit one to the original
      --  generic tree, through which a global actual can be preserved.
      --  Similarly, if a child unit is instantiated within a sibling, in the
      --  context of the parent, we must preserve the identifier of the parent
      --  so that it can be properly resolved in a subsequent instantiation.

      procedure Save_Global_Descendant (D : Union_Id);
      --  Apply Save_Global_References recursively to the descendents of
      --  current node.

      procedure Save_References (N : Node_Id);
      --  This is the recursive procedure that does the work, once the
      --  enclosing generic scope has been established.

      ---------------
      -- Is_Global --
      ---------------

      function Is_Global (E : Entity_Id) return Boolean is
         Se  : Entity_Id := Scope (E);

         function Is_Instance_Node (Decl : Node_Id) return Boolean;
         --  Determine whether the parent node of a reference to a child unit
         --  denotes an instantiation or a formal package, in which case the
         --  reference to the child unit is global, even if it appears within
         --  the current scope (e.g. when the instance appears within the body
         --  of an ancestor).

         ----------------------
         -- Is_Instance_Node --
         ----------------------

         function Is_Instance_Node (Decl : Node_Id) return Boolean is
         begin
            return (Nkind (Decl) in N_Generic_Instantiation
              or else
                Nkind (Original_Node (Decl)) = N_Formal_Package_Declaration);
         end Is_Instance_Node;

      --  Start of processing for Is_Global

      begin
         if E = Gen_Scope then
            return False;

         elsif E = Standard_Standard then
            return True;

         elsif Is_Child_Unit (E)
           and then (Is_Instance_Node (Parent (N2))
             or else (Nkind (Parent (N2)) = N_Expanded_Name
                       and then N2 = Selector_Name (Parent (N2))
                       and then Is_Instance_Node (Parent (Parent (N2)))))
         then
            return True;

         else
            while Se /= Gen_Scope loop
               if Se = Standard_Standard then
                  return True;
               else
                  Se := Scope (Se);
               end if;
            end loop;

            return False;
         end if;
      end Is_Global;

      ------------------
      -- Reset_Entity --
      ------------------

      procedure Reset_Entity (N : Node_Id) is

         procedure Set_Global_Type (N : Node_Id; N2 : Node_Id);
         --  The type of N2 is global to the generic unit. Save the
         --  type in the generic node.

         function Top_Ancestor (E : Entity_Id) return Entity_Id;
         --  Find the ultimate ancestor of the current unit. If it is
         --  not a generic unit, then the name of the current unit
         --  in the prefix of an expanded name must be replaced with
         --  its generic homonym to ensure that it will be properly
         --  resolved in an instance.

         ---------------------
         -- Set_Global_Type --
         ---------------------

         procedure Set_Global_Type (N : Node_Id; N2 : Node_Id) is
            Typ : constant Entity_Id := Etype (N2);

         begin
            Set_Etype (N, Typ);

            if Entity (N) /= N2
              and then Has_Private_View (Entity (N))
            then
               --  If the entity of N is not the associated node, this is
               --  a nested generic and it has an associated node as well,
               --  whose type is already the full view (see below). Indicate
               --  that the original node has a private view.

               Set_Has_Private_View (N);
            end if;

            --  If not a private type, nothing else to do

            if not Is_Private_Type (Typ) then
               if Is_Array_Type (Typ)
                 and then Is_Private_Type (Component_Type (Typ))
               then
                  Set_Has_Private_View (N);
               end if;

            --  If it is a derivation of a private type in a context where
            --  no full view is needed, nothing to do either.

            elsif No (Full_View (Typ)) and then Typ /= Etype (Typ) then
               null;

            --  Otherwise mark the type for flipping and use the full_view
            --  when available.

            else
               Set_Has_Private_View (N);

               if Present (Full_View (Typ)) then
                  Set_Etype (N2, Full_View (Typ));
               end if;
            end if;
         end Set_Global_Type;

         ------------------
         -- Top_Ancestor --
         ------------------

         function Top_Ancestor (E : Entity_Id) return Entity_Id is
            Par : Entity_Id := E;

         begin
            while Is_Child_Unit (Par) loop
               Par := Scope (Par);
            end loop;

            return Par;
         end Top_Ancestor;

      --  Start of processing for Reset_Entity

      begin
         N2 := Get_Associated_Node (N);
         E := Entity (N2);

         if Present (E) then
            if Is_Global (E) then
               Set_Global_Type (N, N2);

            elsif Nkind (N) = N_Op_Concat
              and then Is_Generic_Type (Etype (N2))
              and then
               (Base_Type (Etype (Right_Opnd (N2))) = Etype (N2)
                  or else Base_Type (Etype (Left_Opnd (N2))) = Etype (N2))
              and then Is_Intrinsic_Subprogram (E)
            then
               null;

            else
               --  Entity is local. Mark generic node as unresolved.
               --  Note that now it does not have an entity.

               Set_Associated_Node (N, Empty);
               Set_Etype  (N, Empty);
            end if;

            if Nkind (Parent (N)) in N_Generic_Instantiation
              and then N = Name (Parent (N))
            then
               Save_Global_Defaults (Parent (N), Parent (N2));
            end if;

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then Nkind (Parent (N2)) = N_Expanded_Name
         then
            if Is_Global (Entity (Parent (N2))) then
               Change_Selected_Component_To_Expanded_Name (Parent (N));
               Set_Associated_Node (Parent (N), Parent (N2));
               Set_Global_Type (Parent (N), Parent (N2));
               Save_Entity_Descendants (N);

            --  If this is a reference to the current generic entity,
            --  replace by the name of the generic homonym of the current
            --  package. This is because in an instantiation  Par.P.Q will
            --  not resolve to the name of the instance, whose enclosing
            --  scope is not necessarily Par. We use the generic homonym
            --  rather that the name of the generic itself, because it may
            --  be hidden by a local declaration.

            elsif In_Open_Scopes (Entity (Parent (N2)))
              and then not
                Is_Generic_Unit (Top_Ancestor (Entity (Prefix (Parent (N2)))))
            then
               if Ekind (Entity (Parent (N2))) = E_Generic_Package then
                  Rewrite (Parent (N),
                    Make_Identifier (Sloc (N),
                      Chars =>
                        Chars (Generic_Homonym (Entity (Parent (N2))))));
               else
                  Rewrite (Parent (N),
                    Make_Identifier (Sloc (N),
                      Chars => Chars (Selector_Name (Parent (N2)))));
               end if;
            end if;

            if Nkind (Parent (Parent (N))) in N_Generic_Instantiation
              and then Parent (N) = Name (Parent (Parent (N)))
            then
               Save_Global_Defaults
                 (Parent (Parent (N)), Parent (Parent ((N2))));
            end if;

         --  A selected component may denote a static constant that has
         --  been folded. Make the same replacement in original tree.

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then (Nkind (Parent (N2)) = N_Integer_Literal
                      or else Nkind (Parent (N2)) = N_Real_Literal)
         then
            Rewrite (Parent (N),
              New_Copy (Parent (N2)));
            Set_Analyzed (Parent (N), False);

         --  A selected component may be transformed into a parameterless
         --  function call. If the called entity is global, rewrite the
         --  node appropriately, i.e. as an extended name for the global
         --  entity.

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then Nkind (Parent (N2)) = N_Function_Call
           and then Is_Global (Entity (Name (Parent (N2))))
         then
            Change_Selected_Component_To_Expanded_Name (Parent (N));
            Set_Associated_Node (Parent (N), Name (Parent (N2)));
            Set_Global_Type (Parent (N), Name (Parent (N2)));
            Save_Entity_Descendants (N);

         else
            --  Entity is local. Reset in generic unit, so that node
            --  is resolved anew at the point of instantiation.

            Set_Associated_Node (N, Empty);
            Set_Etype (N, Empty);
         end if;
      end Reset_Entity;

      -----------------------------
      -- Save_Entity_Descendants --
      -----------------------------

      procedure Save_Entity_Descendants (N : Node_Id) is
      begin
         case Nkind (N) is
            when N_Binary_Op =>
               Save_Global_Descendant (Union_Id (Left_Opnd (N)));
               Save_Global_Descendant (Union_Id (Right_Opnd (N)));

            when N_Unary_Op =>
               Save_Global_Descendant (Union_Id (Right_Opnd (N)));

            when N_Expanded_Name | N_Selected_Component =>
               Save_Global_Descendant (Union_Id (Prefix (N)));
               Save_Global_Descendant (Union_Id (Selector_Name (N)));

            when N_Identifier | N_Character_Literal | N_Operator_Symbol =>
               null;

            when others =>
               raise Program_Error;
         end case;
      end Save_Entity_Descendants;

      --------------------------
      -- Save_Global_Defaults --
      --------------------------

      procedure Save_Global_Defaults (N1, N2 : Node_Id) is
         Loc    : constant Source_Ptr := Sloc (N1);
         Assoc2 : constant List_Id    := Generic_Associations (N2);
         Gen_Id : constant Entity_Id  := Get_Generic_Entity (N2);
         Assoc1 : List_Id;
         Act1   : Node_Id;
         Act2   : Node_Id;
         Def    : Node_Id;
         Ndec   : Node_Id;
         Subp   : Entity_Id;
         Actual : Entity_Id;

      begin
         Assoc1 := Generic_Associations (N1);

         if Present (Assoc1) then
            Act1 := First (Assoc1);
         else
            Act1 := Empty;
            Set_Generic_Associations (N1, New_List);
            Assoc1 := Generic_Associations (N1);
         end if;

         if Present (Assoc2) then
            Act2 := First (Assoc2);
         else
            return;
         end if;

         while Present (Act1) and then Present (Act2) loop
            Next (Act1);
            Next (Act2);
         end loop;

         --  Find the associations added for default suprograms

         if Present (Act2) then
            while Nkind (Act2) /= N_Generic_Association
              or else No (Entity (Selector_Name (Act2)))
              or else not Is_Overloadable (Entity (Selector_Name (Act2)))
            loop
               Next (Act2);
            end loop;

            --  Add a similar association if the default is global. The
            --  renaming declaration for the actual has been analyzed, and
            --  its alias is the program it renames. Link the actual in the
            --  original generic tree with the node in the analyzed tree.

            while Present (Act2) loop
               Subp := Entity (Selector_Name (Act2));
               Def  := Explicit_Generic_Actual_Parameter (Act2);

               --  Following test is defence against rubbish errors

               if No (Alias (Subp)) then
                  return;
               end if;

               --  Retrieve the resolved actual from the renaming declaration
               --  created for the instantiated formal.

               Actual := Entity (Name (Parent (Parent (Subp))));
               Set_Entity (Def, Actual);
               Set_Etype (Def, Etype (Actual));

               if Is_Global (Actual) then
                  Ndec :=
                    Make_Generic_Association (Loc,
                      Selector_Name => New_Occurrence_Of (Subp, Loc),
                        Explicit_Generic_Actual_Parameter =>
                          New_Occurrence_Of (Actual, Loc));

                  Set_Associated_Node
                    (Explicit_Generic_Actual_Parameter (Ndec), Def);

                  Append (Ndec, Assoc1);

               --  If there are other defaults, add a dummy association
               --  in case there are other defaulted formals with the same
               --  name.

               elsif Present (Next (Act2)) then
                  Ndec :=
                    Make_Generic_Association (Loc,
                      Selector_Name => New_Occurrence_Of (Subp, Loc),
                        Explicit_Generic_Actual_Parameter => Empty);

                  Append (Ndec, Assoc1);
               end if;

               Next (Act2);
            end loop;
         end if;

         if Nkind (Name (N1)) = N_Identifier
           and then Is_Child_Unit (Gen_Id)
           and then Is_Global (Gen_Id)
           and then Is_Generic_Unit (Scope (Gen_Id))
           and then In_Open_Scopes (Scope (Gen_Id))
         then
            --  This is an instantiation of a child unit within a sibling,
            --  so that the generic parent is in scope. An eventual instance
            --  must occur within the scope of an instance of the parent.
            --  Make name in instance into an expanded name, to preserve the
            --  identifier of the parent, so it can be resolved subsequently.

            Rewrite (Name (N2),
              Make_Expanded_Name (Loc,
                Chars         => Chars (Gen_Id),
                Prefix        => New_Occurrence_Of (Scope (Gen_Id), Loc),
                Selector_Name => New_Occurrence_Of (Gen_Id, Loc)));
            Set_Entity (Name (N2), Gen_Id);

            Rewrite (Name (N1),
               Make_Expanded_Name (Loc,
                Chars         => Chars (Gen_Id),
                Prefix        => New_Occurrence_Of (Scope (Gen_Id), Loc),
                Selector_Name => New_Occurrence_Of (Gen_Id, Loc)));

            Set_Associated_Node (Name (N1), Name (N2));
            Set_Associated_Node (Prefix (Name (N1)), Empty);
            Set_Associated_Node
              (Selector_Name (Name (N1)), Selector_Name (Name (N2)));
            Set_Etype (Name (N1), Etype (Gen_Id));
         end if;

      end Save_Global_Defaults;

      ----------------------------
      -- Save_Global_Descendant --
      ----------------------------

      procedure Save_Global_Descendant (D : Union_Id) is
         N1 : Node_Id;

      begin
         if D in Node_Range then
            if D = Union_Id (Empty) then
               null;

            elsif Nkind (Node_Id (D)) /= N_Compilation_Unit then
               Save_References (Node_Id (D));
            end if;

         elsif D in List_Range then
            if D = Union_Id (No_List)
              or else Is_Empty_List (List_Id (D))
            then
               null;

            else
               N1 := First (List_Id (D));
               while Present (N1) loop
                  Save_References (N1);
                  Next (N1);
               end loop;
            end if;

         --  Element list or other non-node field, nothing to do

         else
            null;
         end if;
      end Save_Global_Descendant;

      ---------------------
      -- Save_References --
      ---------------------

      --  This is the recursive procedure that does the work, once the
      --  enclosing generic scope has been established. We have to treat
      --  specially a number of node rewritings that are required by semantic
      --  processing and which change the kind of nodes in the generic copy:
      --  typically constant-folding, replacing an operator node by a string
      --  literal, or a selected component by an expanded name. In  each of
      --  those cases, the transformation is propagated to the generic unit.

      procedure Save_References (N : Node_Id) is
      begin
         if N = Empty then
            null;

         elsif Nkind (N) = N_Character_Literal
           or else Nkind (N) = N_Operator_Symbol
         then
            if Nkind (N) = Nkind (Get_Associated_Node (N)) then
               Reset_Entity (N);

            elsif Nkind (N) = N_Operator_Symbol
              and then Nkind (Get_Associated_Node (N)) = N_String_Literal
            then
               Change_Operator_Symbol_To_String_Literal (N);
            end if;

         elsif Nkind (N) in N_Op then

            if Nkind (N) = Nkind (Get_Associated_Node (N)) then

               if Nkind (N) = N_Op_Concat then
                  Set_Is_Component_Left_Opnd (N,
                    Is_Component_Left_Opnd (Get_Associated_Node (N)));

                  Set_Is_Component_Right_Opnd (N,
                    Is_Component_Right_Opnd (Get_Associated_Node (N)));
               end if;

               Reset_Entity (N);
            else
               --  Node may be transformed into call to a user-defined operator

               N2 := Get_Associated_Node (N);

               if Nkind (N2) = N_Function_Call then
                  E := Entity (Name (N2));

                  if Present (E)
                    and then Is_Global (E)
                  then
                     Set_Etype (N, Etype (N2));
                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               elsif Nkind (N2) = N_Integer_Literal
                 or else Nkind (N2) = N_Real_Literal
                 or else Nkind (N2) = N_String_Literal
               then
                  --  Operation was constant-folded, perform the same
                  --  replacement in generic.

                  Rewrite (N, New_Copy (N2));
                  Set_Analyzed (N, False);

               elsif Nkind (N2) = N_Identifier
                 and then Ekind (Entity (N2)) = E_Enumeration_Literal
               then
                  --  Same if call was folded into a literal, but in this
                  --  case retain the entity to avoid spurious ambiguities
                  --  if id is overloaded at the point of instantiation or
                  --  inlining.

                  Rewrite (N, New_Copy (N2));
                  Set_Analyzed (N, False);
               end if;
            end if;

            --  Complete the check on operands, if node has not been
            --  constant-folded.

            if Nkind (N) in N_Op then
               Save_Entity_Descendants (N);
            end if;

         elsif Nkind (N) = N_Identifier then
            if Nkind (N) = Nkind (Get_Associated_Node (N)) then

               --  If this is a discriminant reference, always save it.
               --  It is used in the instance to find the corresponding
               --  discriminant positionally rather than  by name.

               Set_Original_Discriminant
                 (N, Original_Discriminant (Get_Associated_Node (N)));
               Reset_Entity (N);

            else
               N2 := Get_Associated_Node (N);

               if Nkind (N2) = N_Function_Call then
                  E := Entity (Name (N2));

                  --  Name resolves to a call to parameterless function.
                  --  If original entity is global, mark node as resolved.

                  if Present (E)
                    and then Is_Global (E)
                  then
                     Set_Etype (N, Etype (N2));
                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               elsif
                 Nkind (N2) = N_Integer_Literal or else
                 Nkind (N2) = N_Real_Literal    or else
                 Nkind (N2) = N_String_Literal
               then
                  --  Name resolves to named number that is constant-folded,
                  --  or to string literal from concatenation.
                  --  Perform the same replacement in generic.

                  Rewrite (N, New_Copy (N2));
                  Set_Analyzed (N, False);

               elsif Nkind (N2) = N_Explicit_Dereference then

                  --  An identifier is rewritten as a dereference if it is
                  --  the prefix in a selected component, and it denotes an
                  --  access to a composite type, or a parameterless function
                  --  call that returns an access type.

                  --  Check whether corresponding entity in prefix is global

                  if Is_Entity_Name (Prefix (N2))
                    and then Present (Entity (Prefix (N2)))
                    and then Is_Global (Entity (Prefix (N2)))
                  then
                     Rewrite (N,
                       Make_Explicit_Dereference (Sloc (N),
                          Prefix => Make_Identifier (Sloc (N),
                            Chars => Chars (N))));
                     Set_Associated_Node (Prefix (N), Prefix (N2));

                  elsif Nkind (Prefix (N2)) = N_Function_Call
                    and then Is_Global (Entity (Name (Prefix (N2))))
                  then
                     Rewrite (N,
                       Make_Explicit_Dereference (Sloc (N),
                          Prefix => Make_Function_Call (Sloc (N),
                            Name  =>
                              Make_Identifier (Sloc (N),
                              Chars => Chars (N)))));

                     Set_Associated_Node
                      (Name (Prefix (N)), Name (Prefix (N2)));

                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               --  The subtype mark of a nominally unconstrained object
               --  is rewritten as a subtype indication using the bounds
               --  of the expression. Recover the original subtype mark.

               elsif Nkind (N2) = N_Subtype_Indication
                 and then Is_Entity_Name (Original_Node (N2))
               then
                  Set_Associated_Node (N, Original_Node (N2));
                  Reset_Entity (N);

               else
                  null;
               end if;
            end if;

         elsif Nkind (N) in N_Entity then
            null;

         else
            declare
               Loc  : constant Source_Ptr := Sloc (N);
               Qual : Node_Id := Empty;
               Typ  : Entity_Id := Empty;
               Nam  : Node_Id;

               use Atree.Unchecked_Access;
               --  This code section is part of implementing an untyped tree
               --  traversal, so it needs direct access to node fields.

            begin
               if Nkind (N) = N_Aggregate
                    or else
                  Nkind (N) = N_Extension_Aggregate
               then
                  N2 := Get_Associated_Node (N);

                  if No (N2) then
                     Typ := Empty;
                  else
                     Typ := Etype (N2);

                     --  In an instance within a generic, use the name of
                     --  the actual and not the original generic parameter.
                     --  If the actual is global in the current generic it
                     --  must be preserved for its instantiation.

                     if Nkind (Parent (Typ)) = N_Subtype_Declaration
                       and then
                         Present (Generic_Parent_Type (Parent (Typ)))
                     then
                        Typ := Base_Type (Typ);
                        Set_Etype (N2, Typ);
                     end if;
                  end if;

                  if No (N2)
                    or else No (Typ)
                    or else not Is_Global (Typ)
                  then
                     Set_Associated_Node (N, Empty);

                     --  If the aggregate is an actual in a call, it has been
                     --  resolved in the current context, to some local type.
                     --  The enclosing call may have been disambiguated by
                     --  the aggregate, and this disambiguation might fail at
                     --  instantiation time because the type to which the
                     --  aggregate did resolve is not preserved. In order to
                     --  preserve some of this information, we wrap the
                     --  aggregate in a qualified expression, using the id of
                     --  its type. For further disambiguation we qualify the
                     --  type name with its scope (if visible) because both
                     --  id's will have corresponding entities in an instance.
                     --  This resolves most of the problems with missing type
                     --  information on aggregates in instances.

                     if Nkind (N2) = Nkind (N)
                       and then
                         (Nkind (Parent (N2)) = N_Procedure_Call_Statement
                           or else Nkind (Parent (N2)) = N_Function_Call)
                       and then Comes_From_Source (Typ)
                     then
                        if Is_Immediately_Visible (Scope (Typ)) then
                           Nam := Make_Selected_Component (Loc,
                             Prefix =>
                               Make_Identifier (Loc, Chars (Scope (Typ))),
                             Selector_Name =>
                               Make_Identifier (Loc, Chars (Typ)));
                        else
                           Nam := Make_Identifier (Loc, Chars (Typ));
                        end if;

                        Qual :=
                          Make_Qualified_Expression (Loc,
                            Subtype_Mark => Nam,
                            Expression => Relocate_Node (N));
                     end if;
                  end if;

                  Save_Global_Descendant (Field1 (N));
                  Save_Global_Descendant (Field2 (N));
                  Save_Global_Descendant (Field3 (N));
                  Save_Global_Descendant (Field5 (N));

                  if Present (Qual) then
                     Rewrite (N, Qual);
                  end if;

               --  All other cases than aggregates

               else
                  Save_Global_Descendant (Field1 (N));
                  Save_Global_Descendant (Field2 (N));
                  Save_Global_Descendant (Field3 (N));
                  Save_Global_Descendant (Field4 (N));
                  Save_Global_Descendant (Field5 (N));
               end if;
            end;
         end if;
      end Save_References;

   --  Start of processing for Save_Global_References

   begin
      Gen_Scope := Current_Scope;

      --  If the generic unit is a child unit, references to entities in
      --  the parent are treated as local, because they will be resolved
      --  anew in the context of the instance of the parent.

      while Is_Child_Unit (Gen_Scope)
        and then Ekind (Scope (Gen_Scope)) = E_Generic_Package
      loop
         Gen_Scope := Scope (Gen_Scope);
      end loop;

      Save_References (N);
   end Save_Global_References;

   --------------------------------------
   -- Set_Copied_Sloc_For_Inlined_Body --
   --------------------------------------

   procedure Set_Copied_Sloc_For_Inlined_Body (N : Node_Id; E : Entity_Id) is
   begin
      Create_Instantiation_Source (N, E, True, S_Adjustment);
   end Set_Copied_Sloc_For_Inlined_Body;

   ---------------------
   -- Set_Instance_Of --
   ---------------------

   procedure Set_Instance_Of (A : Entity_Id; B : Entity_Id) is
   begin
      Generic_Renamings.Table (Generic_Renamings.Last) := (A, B, Assoc_Null);
      Generic_Renamings_HTable.Set (Generic_Renamings.Last);
      Generic_Renamings.Increment_Last;
   end Set_Instance_Of;

   --------------------
   -- Set_Next_Assoc --
   --------------------

   procedure Set_Next_Assoc (E : Assoc_Ptr; Next : Assoc_Ptr) is
   begin
      Generic_Renamings.Table (E).Next_In_HTable := Next;
   end Set_Next_Assoc;

   -------------------
   -- Start_Generic --
   -------------------

   procedure Start_Generic is
   begin
      --  ??? I am sure more things could be factored out in this
      --  routine. Should probably be done at a later stage.

      Generic_Flags.Increment_Last;
      Generic_Flags.Table (Generic_Flags.Last) := Inside_A_Generic;
      Inside_A_Generic := True;

      Expander_Mode_Save_And_Set (False);
   end Start_Generic;

   ----------------------
   -- Set_Instance_Env --
   ----------------------

   procedure Set_Instance_Env
     (Gen_Unit : Entity_Id;
      Act_Unit : Entity_Id)
   is
   begin
      --  Regardless of the current mode, predefined units are analyzed in
      --  the most current Ada mode, and earlier version Ada checks do not
      --  apply to predefined units.

      --  Why is this not using the routine Opt.Set_Opt_Config_Switches ???

      if Is_Internal_File_Name
          (Fname => Unit_File_Name (Get_Source_Unit (Gen_Unit)),
           Renamings_Included => True) then
         Ada_Version := Ada_Version_Type'Last;
         Ada_Version_Explicit := Ada_Version_Explicit_Config;
      end if;

      Current_Instantiated_Parent := (Gen_Unit, Act_Unit, Assoc_Null);
   end Set_Instance_Env;

   -----------------
   -- Switch_View --
   -----------------

   procedure Switch_View (T : Entity_Id) is
      BT        : constant Entity_Id := Base_Type (T);
      Priv_Elmt : Elmt_Id := No_Elmt;
      Priv_Sub  : Entity_Id;

   begin
      --  T may be private but its base type may have been exchanged through
      --  some other occurrence, in which case there is nothing to switch.

      if not Is_Private_Type (BT) then
         return;
      end if;

      Priv_Elmt := First_Elmt (Private_Dependents (BT));

      if Present (Full_View (BT)) then
         Prepend_Elmt (Full_View (BT), Exchanged_Views);
         Exchange_Declarations (BT);
      end if;

      while Present (Priv_Elmt) loop
         Priv_Sub := (Node (Priv_Elmt));

         --  We avoid flipping the subtype if the Etype of its full
         --  view is private because this would result in a malformed
         --  subtype. This occurs when the Etype of the subtype full
         --  view is the full view of the base type (and since the
         --  base types were just switched, the subtype is pointing
         --  to the wrong view). This is currently the case for
         --  tagged record types, access types (maybe more?) and
         --  needs to be resolved. ???

         if Present (Full_View (Priv_Sub))
           and then not Is_Private_Type (Etype (Full_View (Priv_Sub)))
         then
            Prepend_Elmt (Full_View (Priv_Sub), Exchanged_Views);
            Exchange_Declarations (Priv_Sub);
         end if;

         Next_Elmt (Priv_Elmt);
      end loop;
   end Switch_View;

   -----------------------------
   -- Valid_Default_Attribute --
   -----------------------------

   procedure Valid_Default_Attribute (Nam : Entity_Id; Def : Node_Id) is
      Attr_Id : constant Attribute_Id :=
                  Get_Attribute_Id (Attribute_Name (Def));
      T       : constant Entity_Id := Entity (Prefix (Def));
      Is_Fun  : constant Boolean := (Ekind (Nam) = E_Function);
      F       : Entity_Id;
      Num_F   : Int;
      OK      : Boolean;

   begin
      if No (T)
        or else T = Any_Id
      then
         return;
      end if;

      Num_F := 0;
      F := First_Formal (Nam);
      while Present (F) loop
         Num_F := Num_F + 1;
         Next_Formal (F);
      end loop;

      case Attr_Id is
         when Attribute_Adjacent |  Attribute_Ceiling   | Attribute_Copy_Sign |
              Attribute_Floor    |  Attribute_Fraction  | Attribute_Machine   |
              Attribute_Model    |  Attribute_Remainder | Attribute_Rounding  |
              Attribute_Unbiased_Rounding  =>
            OK := Is_Fun
                    and then Num_F = 1
                    and then Is_Floating_Point_Type (T);

         when Attribute_Image    | Attribute_Pred       | Attribute_Succ |
              Attribute_Value    | Attribute_Wide_Image |
              Attribute_Wide_Value  =>
            OK := (Is_Fun and then Num_F = 1 and then Is_Scalar_Type (T));

         when Attribute_Max      |  Attribute_Min  =>
            OK := (Is_Fun and then Num_F = 2 and then Is_Scalar_Type (T));

         when Attribute_Input =>
            OK := (Is_Fun and then Num_F = 1);

         when Attribute_Output | Attribute_Read | Attribute_Write =>
            OK := (not Is_Fun and then Num_F = 2);

         when others =>
            OK := False;
      end case;

      if not OK then
         Error_Msg_N ("attribute reference has wrong profile for subprogram",
           Def);
      end if;
   end Valid_Default_Attribute;

end Sem_Ch12;
