------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 2                              --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Contracts;      use Contracts;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Expander;       use Expander;
with Fname;          use Fname;
with Fname.UF;       use Fname.UF;
with Freeze;         use Freeze;
with Ghost;          use Ghost;
with Itypes;         use Itypes;
with Lib;            use Lib;
with Lib.Load;       use Lib.Load;
with Lib.Xref;       use Lib.Xref;
with Mutably_Tagged; use Mutably_Tagged;
with Nlists;         use Nlists;
with Namet;          use Namet;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Rident;         use Rident;
with Restrict;       use Restrict;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Cat;        use Sem_Cat;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch7;        use Sem_Ch7;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch10;       use Sem_Ch10;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Dim;        use Sem_Dim;
with Sem_Disp;       use Sem_Disp;
with Sem_Elab;       use Sem_Elab;
with Sem_Elim;       use Sem_Elim;
with Sem_Eval;       use Sem_Eval;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sem_Warn;       use Sem_Warn;
with Stand;          use Stand;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinfo.CN;       use Sinfo.CN;
with Sinput;         use Sinput;
with Sinput.L;       use Sinput.L;
with Snames;         use Snames;
with Stringt;        use Stringt;
with Uname;          use Uname;
with Table;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;
with Urealp;         use Urealp;
with Warnsw;         use Warnsw;

with GNAT.HTable;

package body Sem_Ch12 is

   ----------------------------------------------------------
   -- Implementation of Generic Analysis and Instantiation --
   ----------------------------------------------------------

   --  GNAT implements generics by macro expansion. No attempt is made to share
   --  generic instantiations (for now). Analysis of a generic definition does
   --  not perform any expansion action, but the expander must be called on the
   --  tree for each instantiation, because the expansion may of course depend
   --  on the generic actuals. All of this is best achieved as follows:
   --
   --  a) Semantic analysis of a generic unit is performed on a copy of the
   --  tree for the generic unit. All tree modifications that follow analysis
   --  do not affect the original tree. Links are kept between the original
   --  tree and the copy, in order to recognize non-local references within
   --  the generic, and propagate them to each instance (recall that name
   --  resolution is done on the generic declaration: generics are not really
   --  macros). This is summarized in the following diagram:

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

   --  b) Each instantiation copies the original tree, and inserts into it a
   --  series of declarations that describe the mapping between generic formals
   --  and actuals. For example, a generic In OUT parameter is an object
   --  renaming of the corresponding actual, etc. Generic IN parameters are
   --  constant declarations.

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
   --  In order to reference the proper view, we special-case any reference
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

   --  type Global is ...         --  outside of generic unit.
   --  generic ...
   --  package Outer is
   --     ...
   --     type Semi_Global is ... --  global to inner.

   --     generic ...                                         -- 1
   --     procedure inner (X1 : Global;  X2 : Semi_Global);

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
   --  references to Semi_Global should not be preserved, because they must now
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

   --  If we have a chain of instantiations that is circular, this is static
   --  error which must be detected at compile time. The detection of these
   --  circularities is carried out at the point that we insert a generic
   --  instance spec or body. If there is a circularity, then the analysis of
   --  the offending spec or body will eventually result in trying to load the
   --  same unit again, and we detect this problem as we analyze the package
   --  instantiation for the second time.

   --  At least in some cases after we have detected the circularity, we get
   --  into trouble if we try to keep going. The following flag is set if a
   --  circularity is detected, and used to abandon compilation after the
   --  messages have been posted.

   Circularity_Detected : Boolean := False;
   --  It should really be reset upon encountering a new main unit, but in
   --  practice we do not use multiple main units so this is not critical.

   -----------------------------------------
   -- Implementation of Generic Contracts --
   -----------------------------------------

   --  A "contract" is a collection of aspects and pragmas that either verify a
   --  property of a construct at runtime or classify the data flow to and from
   --  the construct in some fashion.

   --  Generic packages, subprograms and their respective bodies may be subject
   --  to the following contract-related aspects or pragmas collectively known
   --  as annotations:

   --     package                  subprogram [body]
   --       Abstract_State           Always_Terminates
   --       Initial_Condition        Contract_Cases
   --       Initializes              Depends
   --                                Exceptional_Cases
   --                                Extensions_Visible
   --                                Global
   --     package body               Post
   --       Refined_State            Post_Class
   --                                Postcondition
   --                                Pre
   --                                Pre_Class
   --                                Precondition
   --                                Refined_Depends
   --                                Refined_Global
   --                                Refined_Post
   --                                Subprogram_Variant
   --                                Test_Case

   --  Most package contract annotations utilize forward references to classify
   --  data declared within the package [body]. Subprogram annotations then use
   --  the classifications to further refine them. These inter dependencies are
   --  problematic with respect to the implementation of generics because their
   --  analysis, capture of global references and instantiation does not mesh
   --  well with the existing mechanism.

   --  1) Analysis of generic contracts is carried out the same way non-generic
   --  contracts are analyzed:

   --    1.1) General rule - a contract is analyzed after all related aspects
   --    and pragmas are analyzed. This is done by routines

   --       Analyze_Package_Body_Contract
   --       Analyze_Package_Contract
   --       Analyze_Subprogram_Body_Contract
   --       Analyze_Subprogram_Contract

   --    1.2) Compilation unit - the contract is analyzed after Pragmas_After
   --    are processed.

   --    1.3) Compilation unit body - the contract is analyzed at the end of
   --    the body declaration list.

   --    1.4) Package - the contract is analyzed at the end of the private or
   --    visible declarations, prior to analyzing the contracts of any nested
   --    packages or subprograms.

   --    1.5) Package body - the contract is analyzed at the end of the body
   --    declaration list, prior to analyzing the contracts of any nested
   --    packages or subprograms.

   --    1.6) Subprogram - if the subprogram is declared inside a block, a
   --    package or a subprogram, then its contract is analyzed at the end of
   --    the enclosing declarations, otherwise the subprogram is a compilation
   --    unit 1.2).

   --    1.7) Subprogram body - if the subprogram body is declared inside a
   --    block, a package body or a subprogram body, then its contract is
   --    analyzed at the end of the enclosing declarations, otherwise the
   --    subprogram is a compilation unit 1.3).

   --  2) Capture of global references within contracts is done after capturing
   --  global references within the generic template. There are two reasons for
   --  this delay - pragma annotations are not part of the generic template in
   --  the case of a generic subprogram declaration, and analysis of contracts
   --  is delayed.

   --  Contract-related source pragmas within generic templates are prepared
   --  for delayed capture of global references by routine

   --    Create_Generic_Contract

   --  The routine associates these pragmas with the contract of the template.
   --  In the case of a generic subprogram declaration, the routine creates
   --  generic templates for the pragmas declared after the subprogram because
   --  they are not part of the template.

   --    generic                                --  template starts
   --    procedure Gen_Proc (Input : Integer);  --  template ends
   --    pragma Precondition (Input > 0);       --  requires own template

   --    2.1) The capture of global references with aspect specifications and
   --    source pragmas that apply to a generic unit must be suppressed when
   --    the generic template is being processed because the contracts have not
   --    been analyzed yet. Any attempts to capture global references at that
   --    point will destroy the Associated_Node linkages and leave the template
   --    undecorated. This delay is controlled by routine

   --       Requires_Delayed_Save

   --    2.2) The real capture of global references within a contract is done
   --    after the contract has been analyzed, by routine

   --       Save_Global_References_In_Contract

   --  3) The instantiation of a generic contract occurs as part of the
   --  instantiation of the contract owner. Generic subprogram declarations
   --  require additional processing when the contract is specified by pragmas
   --  because the pragmas are not part of the generic template. This is done
   --  by routine

   --    Instantiate_Subprogram_Contract

   --------------------------------------------------
   -- Formal packages and partial parameterization --
   --------------------------------------------------

   --  When compiling a generic, a formal package is a local instantiation. If
   --  declared with a box, its generic formals are visible in the enclosing
   --  generic. If declared with a partial list of actuals, those actuals that
   --  are defaulted (covered by an Others clause, or given an explicit box
   --  initialization) are also visible in the enclosing generic, while those
   --  that have a corresponding actual are not.

   --  In our source model of instantiation, the same visibility must be
   --  present in the spec and body of an instance: the names of the formals
   --  that are defaulted must be made visible within the instance, and made
   --  invisible (hidden) after the instantiation is complete, so that they
   --  are not accessible outside of the instance.

   --  In a generic, a formal package is treated like a special instantiation.
   --  Our Ada 95 compiler handled formals with and without box in different
   --  ways. With partial parameterization, we use a single model for both.
   --  We create a package declaration that consists of the specification of
   --  the generic package, and a set of declarations that map the actuals
   --  into local renamings, just as we do for bona fide instantiations. For
   --  defaulted parameters and formals with a box, we copy directly the
   --  declarations of the formals into this local package. The result is a
   --  package whose visible declarations may include generic formals. This
   --  package is only used for type checking and visibility analysis, and
   --  never reaches the back end, so it can freely violate the placement
   --  rules for generic formal declarations.

   --  The list of declarations (renamings and copies of formals) is built
   --  by Analyze_Associations, just as for regular instantiations.

   --  At the point of instantiation, conformance checking must be applied only
   --  to those parameters that were specified in the formals. We perform this
   --  checking by creating another internal instantiation, this one including
   --  only the renamings and the formals (the rest of the package spec is not
   --  relevant to conformance checking). We can then traverse two lists: the
   --  list of actuals in the instance that corresponds to the formal package,
   --  and the list of actuals produced for this bogus instantiation. We apply
   --  the conformance rules to those actuals that are not defaulted, i.e.
   --  which still appear as generic formals.

   --  When we compile an instance body we must make the right parameters
   --  visible again. The predicate Is_Generic_Formal indicates which of the
   --  formals should have its Is_Hidden flag reset.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Abandon_Instantiation (N : Node_Id);
   pragma No_Return (Abandon_Instantiation);
   --  Posts an error message "instantiation abandoned" at the indicated node
   --  and then raises the exception Instantiation_Error to do it.

   procedure Analyze_Formal_Array_Type
     (T   : in out Entity_Id;
      Def : Node_Id);
   --  A formal array type is treated like an array type declaration, and
   --  invokes Array_Type_Declaration (sem_ch3) whose first parameter is
   --  in-out, because in the case of an anonymous type the entity is
   --  actually created in the procedure.

   --  The following procedures treat other kinds of formal parameters

   procedure Analyze_Formal_Derived_Interface_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);

   procedure Analyze_Formal_Derived_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);

   procedure Analyze_Formal_Interface_Type
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
   procedure Analyze_Formal_Signed_Integer_Type (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Modular_Type        (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Ordinary_Fixed_Point_Type
                                                (T : Entity_Id; Def : Node_Id);

   procedure Analyze_Formal_Private_Type
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id);
   --  Creates a new private type, which does not require completion

   procedure Analyze_Formal_Incomplete_Type (T : Entity_Id; Def : Node_Id);
   --  Ada 2012: Creates a new incomplete type whose actual does not freeze

   procedure Analyze_Generic_Formal_Part (N : Node_Id);
   --  Analyze generic formal part

   procedure Analyze_Generic_Access_Type (T : Entity_Id; Def : Node_Id);
   --  Create a new access type with the given designated type

   function Analyze_Associations
     (I_Node  : Node_Id;
      Formals : List_Id;
      F_Copy  : List_Id) return List_Id;
   --  At instantiation time, build the list of associations between formals
   --  and actuals. Each association becomes a renaming declaration for the
   --  formal entity. F_Copy is the analyzed list of formals in the generic
   --  copy. It is used to apply legality checks to the actuals. I_Node is the
   --  instantiation node.

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

   function Build_Subprogram_Decl_Wrapper
     (Formal_Subp : Entity_Id) return Node_Id;
   --  Ada 2022 allows formal subprograms to carry pre/postconditions.
   --  At the point of instantiation these contracts apply to uses of
   --  the actual subprogram. This is implemented by creating wrapper
   --  subprograms instead of the renamings previously used to link
   --  formal subprograms and the corresponding actuals. If the actual
   --  is not an entity (e.g. an attribute reference) a renaming is
   --  created to handle the expansion of the attribute.

   function Build_Subprogram_Body_Wrapper
     (Formal_Subp : Entity_Id;
      Actual_Name : Node_Id) return Node_Id;
   --  The body of the wrapper is a call to the actual, with the generated
   --  pre/postconditon checks added.

   procedure Build_Subprogram_Wrappers
     (Match, Analyzed_Formal : Node_Id; Renamings : List_Id);
   --  Ada 2022: AI12-0272 introduces pre/postconditions for formal
   --  subprograms. The implementation of making the formal into a renaming
   --  of the actual does not work, given that subprogram renaming cannot
   --  carry aspect specifications. Instead we must create subprogram
   --  wrappers whose body is a call to the actual, and whose declaration
   --  carries the aspects of the formal.
   --  The wrapper declaration and body are appended to Renamings.
   --  ???But renaming declarations CAN have aspects specs,
   --  and that was true from the start (see AI05-0183-1).

   procedure Check_Abbreviated_Instance
     (N                : Node_Id;
      Parent_Installed : in out Boolean);
   --  If the name of the generic unit in an abbreviated instantiation is an
   --  expanded name, then the prefix may be an instance and the selector may
   --  designate a child unit. If the parent is installed as a result of this
   --  call, then Parent_Installed is set True, otherwise Parent_Installed is
   --  unchanged by the call.

   --  This routine needs to be called for declaration nodes of formal objects,
   --  types and subprograms to check whether they are the copy, present in the
   --  visible part of the abbreviated instantiation of formal packages, of the
   --  declaration node of their corresponding formal parameter in the template
   --  of the formal package, as specified by RM 12.7(10/2), so as to establish
   --  the proper context for their analysis.

   procedure Check_Access_Definition (N : Node_Id);
   --  Subsidiary routine to null exclusion processing. Perform an assertion
   --  check on Ada version and the presence of an access definition in N.

   procedure Check_Formal_Packages (P_Id : Entity_Id);
   --  Apply the following to all formal packages in generic associations.
   --  Restore the visibility of the formals of the instance that are not
   --  defaulted (see RM 12.7 (10)). Remove the anonymous package declaration
   --  created for formal instances that are not defaulted.

   procedure Check_Formal_Package_Instance
     (Formal_Pack : Entity_Id;
      Actual_Pack : Entity_Id);
   --  Verify that the actuals of the actual instance match the actuals of
   --  the template for a formal package that is not declared with a box.

   procedure Check_Forward_Instantiation (Decl : Node_Id);
   --  If the generic is a local entity and the corresponding body has not
   --  been seen yet, flag enclosing packages to indicate that it will be
   --  elaborated after the generic body. Subprograms declared in the same
   --  package cannot be inlined by the front end because front-end inlining
   --  requires a strict linear order of elaboration.

   function Check_Hidden_Primitives (Renamings : List_Id) return Elist_Id;
   --  Check if some association between formals and actuals requires to make
   --  visible primitives of a tagged type, and make those primitives visible.
   --  Return the list of primitives whose visibility is modified (to restore
   --  their visibility later through Restore_Hidden_Primitives). If no
   --  candidate is found then return No_Elist.

   procedure Check_Hidden_Child_Unit
     (N           : Node_Id;
      Gen_Unit    : Entity_Id;
      Act_Decl_Id : Entity_Id);
   --  If the generic unit is an implicit child instance within a parent
   --  instance, we need to make an explicit test that it is not hidden by
   --  a child instance of the same name and parent.

   procedure Check_Generic_Actuals
     (Instance      : Entity_Id;
      Is_Formal_Box : Boolean);
   --  Similar to previous one. Check the actuals in the instantiation,
   --  whose views can change between the point of instantiation and the point
   --  of instantiation of the body. In addition, mark the generic renamings
   --  as generic actuals, so that they are not compatible with other actuals.
   --  Recurse on an actual that is a formal package whose declaration has
   --  a box.

   function Component_Type_For_Private_View (T : Entity_Id) return Entity_Id;
   --  Return the component type of array type T, with the following addition:
   --  if this component type itself is an array type which has not been first
   --  declared as private, then recurse on it. This makes it possible to deal
   --  with arrays of arrays the same way as multi-dimensional arrays in the
   --  mechanism handling private views.

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
     (Pack     : Entity_Id;
      On_Exit  : Boolean := False;
      Instance : Entity_Id := Empty) return Boolean;
   --  Returns True if E is a formal package of an enclosing generic, or
   --  the actual for such a formal in an enclosing instantiation. If such
   --  a package is used as a formal in an nested generic, or as an actual
   --  in a nested instantiation, the visibility of ITS formals should not
   --  be modified. When called from within Restore_Private_Views, the flag
   --  On_Exit is true, to indicate that the search for a possible enclosing
   --  instance should ignore the current one. In that case Instance denotes
   --  the declaration for which this is an actual. This declaration may be
   --  an instantiation in the source, or the internal instantiation that
   --  corresponds to the actual for a formal package.

   function Earlier (N1, N2 : Node_Id) return Boolean;
   --  Yields True if N1 and N2 appear in the same compilation unit,
   --  ignoring subunits, and if N1 is to the left of N2 in a left-to-right
   --  traversal of the tree for the unit. Used to determine the placement
   --  of freeze nodes for instance bodies that may depend on other instances.

   function Find_Actual_Type
     (Typ       : Entity_Id;
      Gen_Type  : Entity_Id) return Entity_Id;
   --  When validating the actual types of a child instance, check whether
   --  the formal is a formal type of the parent unit, and retrieve the current
   --  actual for it. Typ is the entity in the analyzed formal type declaration
   --  (component or index type of an array type, or designated type of an
   --  access formal) and Gen_Type is the enclosing analyzed formal array
   --  or access type. The desired actual may be a formal of a parent, or may
   --  be declared in a formal package of a parent. In both cases it is a
   --  generic actual type because it appears within a visible instance.
   --  Finally, it may be declared in a parent unit without being a formal
   --  of that unit, in which case it must be retrieved by visibility.
   --  Ambiguities may still arise if two homonyms are declared in two formal
   --  packages, and the prefix of the formal type may be needed to resolve
   --  the ambiguity in the instance ???

   procedure Freeze_Package_Instance
     (N        : Node_Id;
      Gen_Body : Node_Id;
      Gen_Decl : Node_Id;
      Act_Id   : Entity_Id);
   --  If the instantiation happens textually before the body of the generic,
   --  the instantiation of the body must be analyzed after the generic body,
   --  and not at the point of instantiation. Such early instantiations can
   --  happen if the generic and the instance appear in a package declaration
   --  because the generic body can only appear in the corresponding package
   --  body. Early instantiations can also appear if generic, instance and
   --  body are all in the declarative part of a subprogram or entry. Entities
   --  of packages that are early instantiations are delayed, and their freeze
   --  node appears after the generic body. This rather complex machinery is
   --  needed when nested instantiations are present, because the source does
   --  not carry any indication of where the corresponding instance bodies must
   --  be installed and frozen.

   procedure Freeze_Subprogram_Instance
     (N         : Node_Id;
      Gen_Body  : Node_Id;
      Pack_Id   : Entity_Id);
   --  The generic body may appear textually after the instance, including
   --  in the proper body of a stub, or within a different package instance.
   --  Given that the instance can only be elaborated after the generic, we
   --  place freeze nodes for the instance and/or for packages that may enclose
   --  the instance and the generic, so that the back-end can establish the
   --  proper order of elaboration.

   function Get_Associated_Entity (Id : Entity_Id) return Entity_Id;
   --  Similar to Get_Associated_Node below, but for entities

   function Get_Associated_Node (N : Node_Id) return Node_Id;
   --  In order to propagate semantic information back from the analyzed copy
   --  to the original generic, we maintain links between selected nodes in the
   --  generic and their corresponding copies. At the end of generic analysis,
   --  the routine Save_Global_References traverses the generic tree, examines
   --  the semantic information, and preserves the links to those nodes that
   --  contain global information. At instantiation, the information from the
   --  associated node is placed on the new copy, so that name resolution is
   --  not repeated.
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
   --  installed before the instantiation is analyzed. For aggregates of type
   --  extensions, the same view exchange may have to be performed for some of
   --  the ancestor types, if their view is private at the point of
   --  instantiation.
   --
   --  Nodes that are selected components in the parse tree may be rewritten
   --  as expanded names after resolution, and must be treated as potential
   --  entity holders, which is why they also have an Associated_Node.
   --
   --  Nodes that do not come from source, such as freeze nodes, do not appear
   --  in the generic tree, and need not have an associated node.
   --
   --  The associated node is stored in the Associated_Node field. Note that
   --  this field overlaps Entity, which is fine, because the whole point is
   --  that we don't need or want the normal Entity field in this situation.

   function Has_Been_Exchanged (E : Entity_Id) return Boolean;
   --  Traverse the Exchanged_Views list to see if a type was private
   --  and has already been flipped during this phase of instantiation.

   function Has_Contracts (Decl : Node_Id) return Boolean;
   --  Determine whether a formal subprogram has a Pre- or Postcondition,
   --  in which case a subprogram wrapper has to be built for the actual.

   function Has_Fully_Defined_Profile (Subp : Entity_Id) return Boolean;
   --  Determine whether the parameter types and the return type of Subp
   --  are fully defined at the point of instantiation.

   function Has_Null_Default (N : Node_Id) return Boolean is
     (Nkind (N) in N_Formal_Subprogram_Declaration
      and then Nkind (Specification (N)) = N_Procedure_Specification
      and then Null_Present (Specification (N)));
   --  True if N is the declaration of a formal procedure with "is null"
   --  as the default.

   procedure Hide_Current_Scope;
   --  When instantiating a generic child unit, the parent context must be
   --  present, but the instance and all entities that may be generated
   --  must be inserted in the current scope. We leave the current scope
   --  on the stack, but make its entities invisible to avoid visibility
   --  problems. This is reversed at the end of the instantiation. This is
   --  not done for the instantiation of the bodies, which only require the
   --  instances of the generic parents to be in scope.

   function In_Main_Context (E : Entity_Id) return Boolean;
   --  Check whether an instantiation is in the context of the main unit.
   --  Used to determine whether its body should be elaborated to allow
   --  front-end inlining.

   procedure Inherit_Context (Gen_Decl : Node_Id; Inst : Node_Id);
   --  Add the context clause of the unit containing a generic unit to a
   --  compilation unit that is, or contains, an instantiation.

   procedure Init_Env;
   --  Establish environment for subsequent instantiation. Separated from
   --  Save_Env because data-structures for visibility handling must be
   --  initialized before call to Check_Generic_Child_Unit.

   procedure Inline_Instance_Body
     (N        : Node_Id;
      Gen_Unit : Entity_Id;
      Act_Decl : Node_Id);
   --  If front-end inlining is requested, instantiate the package body,
   --  and preserve the visibility of its compilation unit, to insure
   --  that successive instantiations succeed.

   procedure Insert_Freeze_Node_For_Instance
     (N      : Node_Id;
      F_Node : Node_Id);
   --  N denotes a package or a subprogram instantiation and F_Node is the
   --  associated freeze node. Insert the freeze node before the first source
   --  body which follows immediately after N. If no such body is found, the
   --  freeze node is inserted at the end of the declarative region which
   --  contains N, unless the instantiation is done in a package spec that is
   --  not at library level, in which case it is inserted at the outer level.
   --  This can also be invoked to insert the freeze node of a package that
   --  encloses an instantiation, in which case N may denote an arbitrary node.

   procedure Install_Formal_Packages (Par : Entity_Id);
   --  Install the visible part of any formal of the parent that is a formal
   --  package. Note that for the case of a formal package with a box, this
   --  includes the formal part of the formal package (12.7(10/2)).

   procedure Install_Hidden_Primitives
     (Prims_List : in out Elist_Id;
      Gen_T      : Entity_Id;
      Act_T      : Entity_Id);
   --  Remove suffix 'P' from hidden primitives of Act_T to match the
   --  visibility of primitives of Gen_T. The list of primitives to which
   --  the suffix is removed is added to Prims_List to restore them later.

   procedure Install_Parent (P : Entity_Id; In_Body : Boolean := False);
   --  When compiling an instance of a child unit the parent (which is
   --  itself an instance) is an enclosing scope that must be made
   --  immediately visible. This procedure is also used to install the non-
   --  generic parent of a generic child unit when compiling its body, so
   --  that full views of types in the parent are made visible.

   --  The functions Instantiate_... perform various legality checks and build
   --  the declarations for instantiated generic parameters. In all of these
   --  Formal is the entity in the generic unit, Actual is the entity or
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
      Actual_Decls    : List_Id) return List_Id;
   --  Actual_Decls is the list of renamings being built; this is used for
   --  formal derived types, to determine whether the parent type is another
   --  formal derived type in the same generic unit.
   --  Note that the call site appends the result of this function onto
   --  the same list it is passing to Actual_Decls.

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
   --  Furthermore, if the actual has a visible USE clause, these formals must
   --  be made potentially use-visible as well. On exit from the enclosing
   --  instantiation, the reverse must be done.

   --  For a formal package declared without a box, there are conformance rules
   --  that apply to the actuals in the generic declaration and the actuals of
   --  the actual package in the enclosing instantiation. The simplest way to
   --  apply these rules is to repeat the instantiation of the formal package
   --  in the context of the enclosing instance, and compare the generic
   --  associations of this instantiation with those of the actual package.
   --  This internal instantiation only needs to contain the renamings of the
   --  formals: the visible and private declarations themselves need not be
   --  created.

   --  In Ada 2005, the formal package may be only partially parameterized.
   --  In that case the visibility step must make visible those actuals whose
   --  corresponding formals were given with a box. A final complication
   --  involves inherited operations from formal derived types, which must
   --  be visible if the type is.

   function Is_In_Main_Unit (N : Node_Id) return Boolean;
   --  Test if given node is in the main unit

   procedure Load_Parent_Of_Generic
     (N             : Node_Id;
      Spec          : Node_Id;
      Body_Optional : Boolean := False);
   --  If the generic appears in a separate non-generic library unit, load the
   --  corresponding body to retrieve the body of the generic. N is the node
   --  for the generic instantiation, Spec is the generic package declaration.
   --
   --  Body_Optional is a flag that indicates that the body is being loaded to
   --  ensure that temporaries are generated consistently when there are other
   --  instances in the current declarative part that precede the one being
   --  loaded. In that case a missing body is acceptable.

   procedure Map_Formal_Package_Entities (Form : Entity_Id; Act : Entity_Id);
   --  Within the generic part, entities in the formal package are
   --  visible. To validate subsequent type declarations, indicate
   --  the correspondence between the entities in the analyzed formal,
   --  and the entities in the actual package. There are three packages
   --  involved in the instantiation of a formal package: the parent
   --  generic P1 which appears in the generic declaration, the fake
   --  instantiation P2 which appears in the analyzed generic, and whose
   --  visible entities may be used in subsequent formals, and the actual
   --  P3 in the instance. To validate subsequent formals, me indicate
   --  that the entities in P2 are mapped into those of P3. The mapping of
   --  entities has to be done recursively for nested packages.

   procedure Move_Freeze_Nodes
     (Out_Of : Entity_Id;
      After  : Node_Id;
      L      : List_Id);
   --  Freeze nodes can be generated in the analysis of a generic unit, but
   --  will not be seen by the back-end. It is necessary to move those nodes
   --  to the enclosing scope if they freeze an outer entity. We place them
   --  at the end of the enclosing generic package, which is semantically
   --  neutral.

   procedure Preanalyze_Actuals (N : Node_Id; Inst : Entity_Id := Empty);
   --  Analyze actuals to perform name resolution. Full resolution is done
   --  later, when the expected types are known, but names have to be captured
   --  before installing parents of generics, that are not visible for the
   --  actuals themselves.
   --
   --  If Inst is present, it is the entity of the package instance. This
   --  entity is marked as having a limited_view actual when some actual is
   --  a limited view. This is used to place the instance body properly.

   procedure Provide_Completing_Bodies (N : Node_Id);
   --  Generate completing bodies for all subprograms found within package or
   --  subprogram declaration N.

   procedure Remove_Parent (In_Body : Boolean := False);
   --  Reverse effect after instantiation of child is complete

   function Renames_Standard_Subprogram (Subp : Entity_Id) return Boolean;
   --  Determine whether Subp renames one of the subprograms defined in the
   --  generated package Standard.

   function Requires_Conformance_Checking (N : Node_Id) return Boolean;
   --  Determine whether the formal package declaration N requires conformance
   --  checking with actuals in instantiations.

   procedure Restore_Hidden_Primitives (Prims_List : in out Elist_Id);
   --  Restore suffix 'P' to primitives of Prims_List and leave Prims_List
   --  set to No_Elist.

   procedure Set_Instance_Env
     (Gen_Unit : Entity_Id;
      Act_Unit : Entity_Id);
   --  Save current instance on saved environment, to be used to determine
   --  the global status of entities in nested instances. Part of Save_Env.
   --  called after verifying that the generic unit is legal for the instance,
   --  The procedure also examines whether the generic unit is a predefined
   --  unit, in order to set configuration switches accordingly. As a result
   --  the procedure must be called after analyzing and freezing the actuals.

   procedure Set_Instance_Of (A : Entity_Id; B : Entity_Id);
   --  Associate analyzed generic parameter with corresponding instance. Used
   --  for semantic checks at instantiation time.

   function True_Parent (N : Node_Id) return Node_Id;
   --  For a subunit, return parent of corresponding stub, else return
   --  parent of node.

   procedure Valid_Default_Attribute (Nam : Entity_Id; Def : Node_Id);
   --  Verify that an attribute that appears as the default for a formal
   --  subprogram is a function or procedure with the correct profile.

   procedure Validate_Formal_Type_Default (Decl : Node_Id);
   --  Ada_2022 AI12-205: if a default subtype_mark is present, verify
   --  that it is the name of a type in the same class as the formal.
   --  The treatment parallels what is done in Instantiate_Type but differs
   --  in a few ways so that this machinery cannot be reused as is: on one
   --  hand there are no visibility issues for a default, because it is
   --  analyzed in the same context as the formal type definition; on the
   --  other hand the check needs to take into acount the use of a previous
   --  formal type in the current formal type definition (see details in
   --  AI12-0205).

   -------------------------------------------
   -- Data Structures for Generic Renamings --
   -------------------------------------------

   --  The map Generic_Renamings associates generic entities with their
   --  corresponding actuals. Currently used to validate type instances. It
   --  will eventually be used for all generic parameters to eliminate the
   --  need for overload resolution in the instance.

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
      Instantiated_Parent  : Assoc;
      Exchanged_Views      : Elist_Id;
      Hidden_Entities      : Elist_Id;
      Current_Sem_Unit     : Unit_Number_Type;
      Parent_Unit_Visible  : Boolean   := False;
      Instance_Parent_Unit : Entity_Id := Empty;
      Switches             : Config_Switches_Type;
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
   --  renamings of actuals, so that they become compatible subtypes again.
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

   ------------------
   -- Associations --
   ------------------

   package Associations is

      type Actual_Kind is
        (None,
         None_Use_Clause,
         --  Used when the "formal" is a use clause; there is no corresponding
         --  actual.
         Box_Subp_Default,
         --  Used for "is <>" as a subprogram default
         Box_Actual,
         --  Used for explicit "name => <>" and "others => <>" in formal
         --  packages.
         Name_Exp,
         --  Name or expression or ....
         --  Used for an explicit_generic_actual_parameter, and also for the
         --  default_expression of an in-mode formal, the default_subtype_mark
         --  of a formal type, and the default_name of a formal subprogram.
         Null_Default,
         --  Used for "is null" as a subprogram default.
         Exp_Func_Default,
         --  Used for "is (expression)" as a subprogram default,
         --  which is a language extension (and is different from "is name"
         --  without parentheses).
         Dummy_Assoc
         --  Used for the dummy associations that are created in
         --  Save_Global_Defaults. These have Explicit_Generic_Actual_Parameter
         --  = Empty and Box_Present = False
        );
      --  ???We wouldn't need this enumeration type if we created new node
      --  kinds for N_Box_Subp_Default, N_Box_Actual, N_Null_Default, and
      --  N_Exp_Func_Default.

      type Generic_Actual_Rec (Kind : Actual_Kind := None) is record
         --  Representation of one generic actual parameter
         case Kind is
            when None | None_Use_Clause | Box_Subp_Default | Box_Actual |
              Null_Default | Dummy_Assoc =>
               null;
            when Name_Exp | Exp_Func_Default =>
               Name_Exp : Node_Id;
         end case;
      end record;

      type Actual_Origin_Enum is
        (None, From_Explicit_Actual, From_Default, From_Inference,
         From_Others_Box);
      --  Indication of where the Actual came from -- explicitly in the
      --  instantiation, inferred from some other type, or defaulted.

      type Inference_Reason is
      --  Reason an actual type corresponding to a formal type was (or could
      --  be) inferred from the actual type corresponding to another formal
      --  type.
        (Designated_Type, -- designated type from formal access
         Index_Type, -- index type from formal array
         Component_Type, -- component type from formal array
         Discriminant_Type); -- discriminant type from formal discriminated

      function Image (Reason : Inference_Reason) return String is
        (case Reason is
           when Designated_Type => "designated type",
           when Index_Type => "index type",
           when Component_Type => "component type",
           when Discriminant_Type => "discriminant type");

      type Assoc_Index is new Pos;
      subtype Assoc_Count is Assoc_Index'Base range 0 .. Assoc_Index'Last;

      type Assoc_Rec is record
         --  Association between a single formal/actual pair. But we store both
         --  the unanalyzed and analyzed formal.

         Un_Formal, An_Formal : Node_Id; -- unanalyzed and analyzed formals
         --  An_Formal is the node in the generic copy that corresponds to
         --  Un_Formal. The semantic information on this node is used to
         --  perform legality checks on the actuals. Because semantic analysis
         --  can introduce some anonymous entities or modify the declaration
         --  node itself, the correspondence between the two lists is not
         --  one-one. In addition to anonymous types, a formal "=" will
         --  introduce an implicit equal and opposite "/=".

         Explicit_Assoc : Opt_N_Generic_Association_Id;
         --  Explicit association, if any, from the source or generated.

         Actual : Generic_Actual_Rec;
         --  Generic actual parameter corresponding to Un_Formal/An_Formal,
         --  possibly from defaults or others/boxes.

         Actual_Origin : Actual_Origin_Enum;
         --  Reason why Actual was set; where it came from

         Info_Inferred_Actual : Opt_Type_Kind_Id;
         --  An inferred actual is always a type entity, not a box, and not
         --  something like T'Base. This is used only for messages and
         --  assertions. It contains the type that was, or could have been,
         --  inferred.

         Inferred_From : Assoc_Index;
         --  Index of a later Assoc_Rec in the same Gen_Assocs_Rec from which
         --  this one was inferred, or could be inferred.
         --  Valid only if Info_Inferred_Actual is present.

         Reason : Inference_Reason;
         --  Reason the type was inferred, or could have been inferred.
         --  Valid only if Info_Inferred_Actual is present.
      end record; -- Assoc_Rec

      type Assoc_Array is array (Assoc_Index range <>) of Assoc_Rec;
      --  One element for each formal and (if legal) for each corresponding
      --  actual.

      type Gen_Assocs_Rec (Num_Assocs : Assoc_Count) is record
         --  Representation of formal/actual matching. Num_Assocs
         --  is the number of formals and (if legal) the number
         --  of actuals.
         Gen_Unit : Entity_Id;
         --  the generic unit being instantiated
         Others_Present : Boolean;
         --  True if "others => <>" (only for formal packages)
         Assocs : Assoc_Array (1 .. Num_Assocs);
         --  One for each formal/actual pair; defaulted and inferred actuals
         --  are included.
      end record;

      function Match_Assocs
        (I_Node : Node_Id; Formals : List_Id; F_Copy : List_Id)
         return Gen_Assocs_Rec;
      --  I_Node is the instantiation node. Formals is the list of unanalyzed
      --  formals. F_Copy is the analyzed list of formals in the generic copy.
      --  Return a Gen_Assocs_Rec with formals, explicit actuals, and default
      --  actuals filled in. Check legality rules related to formal/actual
      --  matching.

      procedure Note_Potential_Inference
        (I_Node : Node_Id; Gen_Assocs : Gen_Assocs_Rec);
      --  If -gnatd_I, print "info:" messages about type inference that could
      --  have been done.

   end Associations;

   procedure Analyze_One_Association
     (I_Node   :  Node_Id; -- instantiation node
      Assoc    : Associations.Assoc_Rec;
      --  Logical 'in out' parameters:
      Result_Renamings  : List_Id;
      Default_Actuals   : List_Id;
      Actuals_To_Freeze : Elist_Id);
   --  Called by Analyze_Associations for each association. The renamings
   --  are appended onto Result_Renamings. Defaulted actuals are appended
   --  onto Default_Actuals, and actuals that require freezing are
   --  appended onto Actuals_To_Freeze.

   procedure Check_Fixed_Point_Warning
     (Gen_Assocs : Associations.Gen_Assocs_Rec;
      Renamings : List_Id);
   --  Warn if any actual is a fixed-point type that has user-defined
   --  arithmetic operators, but there is no corresponding formal in the
   --  generic, in which case the predefined operators will be used. This
   --  merits a warning because of the special semantics of fixed point
   --  operators. However, do not warn if the formal is private, because there
   --  can be no arithmetic operators in the generic so there no danger of
   --  confusion.

   ------------------
   -- Associations --
   ------------------

   package body Associations is

      generic
         with procedure Action (F : Node_Id; Index : Assoc_Index);
      procedure Formal_Iter (Formals : List_Id);
      --  Iterate through the unanalyzed formals, calling Action for each one.
      --  Skip pragmas, but do not skip use clauses.

      function Num_Formals (Formals : List_Id) return Assoc_Count;
      --  Note: does not include pragmas that occur in the Formals list;
      --  it does include use clauses.

      generic
         with procedure Action (F : Node_Id; Index : Assoc_Index);
      procedure An_Formal_Iter (An_Formals : List_Id);
      --  Iterate through the analyzed formals, calling Action for each one
      --  that corresponds to an unanalyzed formal. This should call Action
      --  exactly the same number of times that Formal_Iter calls its Action.
      --  Skip pragmas, but do not skip use clauses. Skip extraneous
      --  analyzed formals in cases where there are multiple ones
      --  corresponding to a particular unanalyzed one.

      function Num_An_Formals (F_Copy  : List_Id) return Assoc_Count;
      --  Number of analyzed formals that correspond directly to unanalyzed
      --  formals. There are all sorts of other things in F_Copy, which
      --  are not counted.

      procedure Check_Box (I_Node, Actual : Node_Id);
      --  Check for errors in "others => <>" and "Name => <>"

      function Default (Un_Formal : Node_Id) return Generic_Actual_Rec;
      --  Return the default for a given formal, which can be a name,
      --  expression, box, etc.

      procedure Match_Positional
        (Src_Assoc : in out Node_Id; Assoc : in out Assoc_Rec);
      --  Called by Match_Assocs to match one positional parameter association.
      --  If the current formal (in Assoc) is not a use clause, then there is a
      --  match, and we set Assoc.Actual and move Src_Assoc to the next one.

      procedure Match_Named
        (Src_Assoc : Node_Id; Assoc : in out Assoc_Rec;
         Found : in out Boolean);
      --  Called by Match_Assocs to match one named parameter association.
      --  If the current formal (in Assoc) is not a use clause, and the
      --  selector name matches the formal name, then there is a match,
      --  and we set Assoc.Actual. We also set the Selector_Name to denote
      --  the matched formal, and set Found to True.

      procedure Inference_Msg
        (Gen_Unit : Entity_Id;
         Inferred_To, Inferred_From : Assoc_Rec;
         Was_Inferred : Boolean);
      --  If Was_Inferred is True, this prints out an "info:" message
      --  showing the inference.
      --  If Was_Inferred is False, the message says that it could have
      --  been inferred.

      function Find_Assoc
        (Gen_Assocs : Gen_Assocs_Rec; F : Entity_Id) return Assoc_Index;
      --  Return the index of F in Gen_Assocs.Assocs, which must be
      --  present.

      procedure Maybe_Infer_One
        (Gen_Assocs : in out Gen_Assocs_Rec;
         FF, AA : N_Entity_Id; Inferred_From : Assoc_Index;
         Reason : Inference_Reason);
      --  If it makes sense to infer that formal FF is associated with
      --  actual AA, then do so.

      procedure Infer_From_Access
        (Gen_Assocs : in out Gen_Assocs_Rec;
         Index : Assoc_Index;
         F : Node_Id;
         A_Full : Entity_Id);
      --  Try to infer the designated type

      procedure Infer_From_Array
        (Gen_Assocs : in out Gen_Assocs_Rec;
         Index : Assoc_Index;
         F : Node_Id;
         A_Full : Entity_Id);
      --  Try to infer the index and component types

      procedure Infer_From_Discriminated
        (Gen_Assocs : in out Gen_Assocs_Rec;
         Index : Assoc_Index;
         F : Node_Id;
         A_Full : Entity_Id);
      --  Try to infer the types of discriminants

      procedure Infer_Actuals (Gen_Assocs : in out Gen_Assocs_Rec);
      --  Called by Match_Assocs after processing explicit and defaulted
      --  parameters to infer any that are still missing.

      -----------------
      -- Formal_Iter --
      -----------------

      --  Formal_Iter is straightforward; An_Formal_Iter is not.

      procedure Formal_Iter (Formals : List_Id) is
         F : Node_Id := First (Formals);
         Index : Assoc_Index := 1;
      begin
         while Present (F) loop
            case Nkind (F) is
               when N_Formal_Object_Declaration
                  | N_Formal_Type_Declaration
                  | N_Formal_Subprogram_Declaration
                  | N_Formal_Package_Declaration
                  | N_Use_Package_Clause
                  | N_Use_Type_Clause
                =>
                  Action (F, Index);
                  Index := Index + 1;
               when N_Pragma =>
                  null;
               when others =>
                  raise Program_Error;
            end case;

            Next (F);
         end loop;
      end Formal_Iter;

      -----------------
      -- Num_Formals --
      -----------------

      function Num_Formals (Formals : List_Id) return Assoc_Count is
         Result : Assoc_Count := 0;
         procedure Action (Ignore_F : Node_Id; Ignore : Assoc_Index);
         procedure Action (Ignore_F : Node_Id; Ignore : Assoc_Index) is
         begin
            Result := Result + 1;
         end Action;
         procedure Iter is new Formal_Iter (Action);
      begin
         Iter (Formals);
         return Result;
      end Num_Formals;

      --------------------
      -- An_Formal_Iter --
      --------------------

      procedure An_Formal_Iter (An_Formals : List_Id) is
         F : Node_Id := First (An_Formals);
         Index : Assoc_Index := 1;
      begin
         --  The correspondence between unanalyzed and analyzed formals is not
         --  one-one; hence this needs to do some fancy footwork to skip some
         --  items in the analyzed formals list. In each case where multiple
         --  items in An_Formals correspond to a particular unanalyzed formal,
         --  we must pick the "main" one.

         while Present (F) loop
            case Nkind (F) is
               when N_Use_Package_Clause | N_Use_Type_Clause =>
                  Action (F, Index);
                  Index := Index + 1;

               when N_Formal_Object_Declaration
                  | N_Formal_Type_Declaration
                  | N_Formal_Subprogram_Declaration
                  | N_Package_Declaration
                  | N_Full_Type_Declaration
                  | N_Private_Type_Declaration
                  | N_Private_Extension_Declaration
                =>
                  if Is_Internal_Name (Chars (Defining_Entity (F))) then
                     null;
                  else
                     Action (F, Index);
                     Index := Index + 1;

                     --  Skip full type of derived type

                     if Nkind (F) = N_Full_Type_Declaration
                       and then Nkind (Type_Definition (F)) =
                         N_Derived_Type_Definition
                       and then Present (Next (F))
                       and then Nkind (Next (F)) = N_Full_Type_Declaration
                       and then Chars (Defining_Identifier (F)) =
                         Chars (Defining_Identifier (Next (F)))
                     then
                        Next (F);
                     end if;
                  end if;

               when N_Subtype_Declaration =>
                  if Nkind (Original_Node (F)) in N_Formal_Type_Declaration
                  then
                     pragma Assert
                       (not Is_Internal_Name (Chars (Defining_Entity (F))));
                     Action (F, Index);
                     Index := Index + 1;

                  elsif Nkind (Original_Node (F)) in N_Full_Type_Declaration
                  then
                     null;
                  else
                     --  subtype of a formal object

                     pragma Assert
                       (Nkind (Next (F)) = N_Formal_Object_Declaration);
                  end if;

               when N_Pragma =>
                  null;

               when N_Formal_Package_Declaration =>
                  --  If there were no errors, this would have been transformed
                  --  into an N_Package_Declaration.

                  Check_Error_Detected;
                  pragma Assert (Error_Posted (F));
                  Abandon_Instantiation (Instantiation_Node);

               when others =>
                  raise Program_Error;
            end case;

            Next (F);
         end loop;
      end An_Formal_Iter;

      --------------------
      -- Num_An_Formals --
      --------------------

      function Num_An_Formals (F_Copy  : List_Id) return Assoc_Count is
         Result : Assoc_Count := 0;
         procedure Action (Ignore_F : Node_Id; Ignore : Assoc_Index);
         procedure Action (Ignore_F : Node_Id; Ignore : Assoc_Index) is
         begin
            Result := Result + 1;
         end Action;
         procedure Iter is new An_Formal_Iter (Action);
      begin
         Iter (F_Copy);
         return Result;
      end Num_An_Formals;

      ---------------
      -- Check_Box --
      ---------------

      procedure Check_Box (I_Node, Actual : Node_Id) is
      begin
         --  "... => <>" is allowed only in formal packages, not old-fashioned
         --  instantiations.

         if Nkind (I_Node) /= N_Formal_Package_Declaration
           and then Comes_From_Source (I_Node)
         then
            if Actual in N_Others_Choice_Id then
               Error_Msg_N
                 ("OTHERS association not allowed in an instance", Actual);
            elsif Box_Present (Actual) then
               Error_Msg_N
                 ("box association not allowed in an instance", Actual);
            end if;
         end if;

         --  "others => <>" must come last

         if Actual in N_Others_Choice_Id
           and then Present (Next (Actual))
         then
            Error_Msg_N
              ("OTHERS must be last association", Actual);
         end if;
      end Check_Box;

      -------------
      -- Default --
      -------------

      function Default (Un_Formal : Node_Id) return Generic_Actual_Rec is
      begin
         return Result : Generic_Actual_Rec do
            case Nkind (Un_Formal) is
               when N_Formal_Object_Declaration =>
                  if Present (Default_Expression (Un_Formal)) then
                     Result := (Name_Exp, Default_Expression (Un_Formal));
                  end if;
               when N_Formal_Type_Declaration =>
                  if Present (Default_Subtype_Mark (Un_Formal)) then
                     Result := (Name_Exp, Default_Subtype_Mark (Un_Formal));
                  end if;
               when N_Formal_Subprogram_Declaration =>
                  if Present (Default_Name (Un_Formal)) then
                     pragma Assert (Result.Kind = None);
                     Result := (Name_Exp, Default_Name (Un_Formal));
                  end if;

                  if Box_Present (Un_Formal) then
                     pragma Assert (Result.Kind = None);
                     Result := (Kind => Box_Subp_Default);
                  end if;

                  if Present (Expression (Un_Formal)) then
                     pragma Assert (Result.Kind = None);
                     Result := (Exp_Func_Default, Expression (Un_Formal));
                  end if;

                  if Has_Null_Default (Un_Formal) then
                     pragma Assert (Result.Kind = None);
                     Result := (Kind => Null_Default);
                  end if;

               when N_Formal_Package_Declaration => null;

               when others => raise Program_Error;
            end case;
            pragma Assert
              (if Result.Kind in Name_Exp | Exp_Func_Default then
                 Present (Result.Name_Exp));
         end return;
      end Default;

      ----------------------
      -- Match_Positional --
      ----------------------

      procedure Match_Positional
        (Src_Assoc : in out Node_Id; Assoc : in out Assoc_Rec) is
      begin
         if Nkind (Assoc.Un_Formal) not in
           N_Use_Package_Clause | N_Use_Type_Clause
         then
            pragma Assert (No (Assoc.Explicit_Assoc));
            pragma Assert (Assoc.Actual.Kind = None);
            Assoc.Explicit_Assoc := Src_Assoc;

            --  A "<>" without "name =>" is illegal syntax

            if Box_Present (Src_Assoc) then
               Assoc.Actual := (Kind => Box_Actual);
               if False then -- ???
                  --  Disable this for now, because we have various
                  --  code that needs to be updated.
                  Error_Msg_N
                    ("box requires named notation", Src_Assoc);
               end if;
            else
               Assoc.Actual :=
                 (Name_Exp,
                  Explicit_Generic_Actual_Parameter (Src_Assoc));
               pragma Assert (Present (Assoc.Actual.Name_Exp));
            end if;
            Assoc.Actual_Origin := From_Explicit_Actual;

            Next (Src_Assoc);
         end if;
      end Match_Positional;

      -----------------
      -- Match_Named --
      -----------------

      procedure Match_Named
        (Src_Assoc : Node_Id; Assoc : in out Assoc_Rec;
         Found : in out Boolean) is
      begin
         if Nkind (Assoc.Un_Formal) not in
             N_Use_Package_Clause | N_Use_Type_Clause
           and then Chars (Selector_Name (Src_Assoc)) =
             Chars (Defining_Entity (Assoc.Un_Formal))
         then
            if Found then -- second formal with the same name
               pragma Assert (Comes_From_Source (Src_Assoc));
               Error_Msg_N
                 ("named association not allowed for " &
                  "overloaded formal", Src_Assoc);
               Abandon_Instantiation (Instantiation_Node);
            end if;

            if Assoc.Actual.Kind /= None then
               if Comes_From_Source (Src_Assoc) then
                  Error_Msg_NE
                    ("duplicate actual for &",
                     Src_Assoc, Selector_Name (Src_Assoc));
               end if;
            else
               Assoc.Explicit_Assoc := Src_Assoc;
               if Box_Present (Src_Assoc) then
                  Assoc.Actual := (Kind => Box_Actual);

               else
                  if No (Explicit_Generic_Actual_Parameter (Src_Assoc)) then
                     Assoc.Actual := (Kind => Dummy_Assoc);
                  else
                     Assoc.Actual :=
                       (Name_Exp,
                        Explicit_Generic_Actual_Parameter (Src_Assoc));
                  end if;

                  --  Set Entity (etc.) of the selector name:

                  declare
                     A_F : constant Entity_Id :=
                       Defining_Entity (Assoc.An_Formal);
                     Orig_F : constant Node_Id :=
                       Original_Node (Assoc.An_Formal);
                     Sel : constant Node_Id :=
                       Selector_Name (Assoc.Explicit_Assoc);
                  begin
                     Set_Entity (Sel, A_F);
                     Set_Etype  (Sel, Etype (A_F));

                     if Nkind (Orig_F) = N_Formal_Package_Declaration then
                        Generate_Reference (Defining_Identifier (Orig_F), Sel);
                        --  ???Original_Node makes no sense, but we're
                        --  preserving the old behavior.
                     else
                        Generate_Reference (A_F, Sel);
                     end if;
                  end;
               end if;

               Assoc.Actual_Origin := From_Explicit_Actual;
               Found := True;
            end if;
         end if;
      end Match_Named;

      ------------------
      -- Match_Assocs --
      ------------------

      function Match_Assocs
        (I_Node : Node_Id; Formals : List_Id; F_Copy : List_Id)
         return Gen_Assocs_Rec
      is
         Src_Assocs : constant List_Id := Generic_Associations (I_Node);
         Gen_Unit : constant Entity_Id := Defining_Entity (Parent (F_Copy));
      begin
         pragma Assert
           (Num_An_Formals (F_Copy) = Num_Formals (Formals)
            or else Serious_Errors_Detected > 0);

         return Result : Gen_Assocs_Rec (Num_Assocs => Num_Formals (Formals))
         do
            Result.Gen_Unit := Gen_Unit;
            Result.Others_Present := False;

            --  Loop through the unanalyzed formals:

            declare
               procedure Set_Formal (F : Node_Id; Index : Assoc_Index);
               --  Initialize one Assoc_Rec so the formal is set.
               --  Use a dummy assoc for use clauses.

               procedure Set_Formal (F : Node_Id; Index : Assoc_Index) is
                  Assoc : Assoc_Rec renames Result.Assocs (Index);
               begin
                  if Nkind (F) in N_Use_Package_Clause | N_Use_Type_Clause then
                     Assoc :=
                       (Un_Formal => F,
                        An_Formal => Empty,
                        Explicit_Assoc => Empty,
                        Actual => (Kind => None_Use_Clause),
                        Actual_Origin => None,
                        Info_Inferred_Actual => Empty,
                        Inferred_From => <>,
                        Reason => <>);
                  else
                     Assoc :=
                       (Un_Formal => F,
                        An_Formal => Empty,
                        Explicit_Assoc => Empty,
                        Actual => <>,
                        Actual_Origin => None,
                        Info_Inferred_Actual => Empty,
                        Inferred_From => <>,
                        Reason => <>);
                  end if;
               end Set_Formal;
               procedure Iter is new Formal_Iter (Set_Formal);
            begin
               Iter (Formals);
            end;

            --  Loop through the analyzed copy of the formals:

            declare
               procedure Set_An_Formal (F : Node_Id; Index : Assoc_Index);
               procedure Set_An_Formal (F : Node_Id; Index : Assoc_Index) is
                  Assoc : Assoc_Rec renames Result.Assocs (Index);
               begin
                  Assoc.An_Formal := F;
                  if Nkind (F) in N_Use_Package_Clause | N_Use_Type_Clause then
                     pragma Assert
                       (Nkind (Assoc.Un_Formal) = Nkind (Assoc.An_Formal));

                  else
                     case Nkind (Assoc.Un_Formal) is
                        when N_Formal_Object_Declaration
                           | N_Formal_Subprogram_Declaration
                        =>
                           pragma Assert
                             (Nkind (Assoc.Un_Formal) =
                              Nkind (Assoc.An_Formal));

                        when N_Formal_Type_Declaration =>
                           pragma Assert
                             (Nkind (Original_Node (Assoc.An_Formal)) =
                              N_Formal_Type_Declaration);
                           pragma Assert
                             (Nkind (Assoc.An_Formal) in
                                N_Formal_Type_Declaration
                              | N_Full_Type_Declaration
                              | N_Private_Type_Declaration
                              | N_Private_Extension_Declaration
                              | N_Subtype_Declaration);

                        when N_Formal_Package_Declaration =>
                           pragma Assert
                             (Nkind (Original_Node (Assoc.An_Formal)) =
                              N_Formal_Package_Declaration);
                           pragma Assert
                             (Nkind (Assoc.An_Formal) = N_Package_Declaration);

                        when others => pragma Assert (False);
                     end case;

                     pragma Assert
                       (Chars (Defining_Entity (Assoc.Un_Formal)) =
                        Chars (Defining_Entity (Assoc.An_Formal)));
                  end if;
               end Set_An_Formal;

               procedure Iter is new An_Formal_Iter (Set_An_Formal);
            begin
               pragma Assert
                 (Num_An_Formals (F_Copy) = Result.Assocs'Last
                  or else Serious_Errors_Detected > 0);
               Iter (F_Copy);
            end;

            --  Loop through actual source associations:

            declare
               Src_Assoc : Node_Id := First (Src_Assocs);
               --  Generic association from the source

               function Positional return Boolean is
                 (Present (Src_Assoc)
                  and then Src_Assoc not in N_Others_Choice_Id
                  and then No (Selector_Name (Src_Assoc)));
               --  True if Src_Assoc is position; i.e. not named and not others
            begin
               --  Loop through positional actuals:

               for Index in Result.Assocs'Range loop
                  exit when not Positional;
                  Match_Positional (Src_Assoc, Result.Assocs (Index));
               end loop;

               if Positional then
                  Error_Msg_Sloc := Sloc (Gen_Unit);
                  Error_Msg_NE
                    ("unmatched actual in instantiation of & declared#",
                     Src_Assoc, Gen_Unit);
               else
                  --  Loop through named actuals and "others => <>":

                  while Present (Src_Assoc) loop
                     Check_Box (I_Node, Src_Assoc);
                     if Src_Assoc in N_Others_Choice_Id then
                        Result.Others_Present := True;
                        exit;
                     end if;

                     if Positional then
                        Error_Msg_N
                          ("invalid positional actual after named one",
                           Src_Assoc);
                     else
                        --  For actual "X => ...", find formal whose name is X.
                        --  Complain if X has already been specified (could be
                        --  by a positional association, or by a previous named
                        --  one). Also complain if there's more than one X.
                        --  See RM-12.3(9/3) and 12.7(4.1/3).
                        --  However, this rule does not apply to generated
                        --  code,because for nested instances, we routinely
                        --  generate things like:
                        --     X => ..., X => ...
                        --  where the first one refers to the first formal X,
                        --  and the second one refers to the second formal X,
                        --  and so on. (The X's are formal subprograms in this
                        --  case.)

                        declare
                           Found : Boolean := False;
                        begin
                           for Index in Result.Assocs'Range loop
                              Match_Named
                                (Src_Assoc, Result.Assocs (Index), Found);
                              exit when Found
                                and then not Comes_From_Source (Src_Assoc);
                           end loop;

                           if not Found and then Comes_From_Source (Src_Assoc)
                           then
                              Error_Msg_Sloc := Sloc (Gen_Unit);
                              Error_Msg_NE
                                ("unmatched actual &",
                                 Src_Assoc, Selector_Name (Src_Assoc));
                              Error_Msg_NE
                                ("\in instantiation of & declared#",
                                 Src_Assoc, Gen_Unit);
                           end if;
                        end;
                     end if;

                     Next (Src_Assoc);
                  end loop;
               end if;
            end;

            --  Fill in defaults. For each formal F with no associated actual,
            --  if there is "others => <>", set the actual to "F => <>".
            --  Otherwise, if the formal has a default, set the actual to
            --  "F => default". Otherwise leave it Empty.
            --  (If Empty, it could be inferred, or it could be an error).

            for Index in Result.Assocs'Range loop
               declare
                  Assoc : Assoc_Rec renames Result.Assocs (Index);
               begin
                  if Assoc.Actual.Kind = None then
                     pragma Assert (No (Assoc.Explicit_Assoc));
                     if Result.Others_Present then
                        Assoc.Actual := (Kind => Box_Actual);
                        Assoc.Actual_Origin := From_Others_Box;
                     else
                        Assoc.Actual := Default (Assoc.Un_Formal);
                        if Assoc.Actual.Kind /= None then
                           Assoc.Actual_Origin := From_Default;
                        end if;
                     end if;
                  end if;
               end;
            end loop;

            if Nkind (I_Node) /= N_Formal_Package_Declaration then
               Infer_Actuals (Gen_Assocs => Result);
            end if;

            --  Check for missing actuals

            for Index in Result.Assocs'Range loop
               if Result.Assocs (Index).Actual.Kind = None then
                  Error_Msg_Sloc := Sloc (Gen_Unit);
                  Error_Msg_NE
                    ("missing actual &",
                     Instantiation_Node,
                     Defining_Entity (Result.Assocs (Index).Un_Formal));
                  Error_Msg_NE
                    ("\in instantiation of & declared#",
                     Instantiation_Node, Gen_Unit);
                  Abandon_Instantiation (Instantiation_Node);
               end if;
            end loop;
         end return;
      end Match_Assocs;

      -------------------
      -- Inference_Msg --
      -------------------

      procedure Inference_Msg
        (Gen_Unit : Entity_Id;
         Inferred_To, Inferred_From : Assoc_Rec;
         Was_Inferred : Boolean)
      is
         pragma Assert (Debug_Flag_Underscore_II); -- This is only for -gnatd_I

         Was : constant String := (if Was_Inferred then "" else "could have ");

         --  "if True" below to leave out some verbosity for now:
         Inst : constant String :=
           (if True then ""
            else " gen: " & Get_Name_String (Chars (Gen_Unit)));
         Decl : constant String := (if True then "" else " declared # ");

         R : constant String := " (" & Image (Inferred_To.Reason) & ")";

         Mess : constant String :=
           "info: " & Was & "inferred `% ='> &`" & Decl & Inst & R;
         Mess_2 : constant String :=
           "info: `% ='> ...`";
      begin
         Error_Msg_Name_1 := Chars (Defining_Entity (Inferred_To.An_Formal));
         Error_Msg_Sloc := Sloc (Inferred_To.Info_Inferred_Actual);
         if not In_Instance then
            if Debug_Flag_Underscore_II then
               Error_Msg_NE
                 (Mess, Inferred_From.Actual.Name_Exp,
                  Inferred_To.Info_Inferred_Actual);
               Error_Msg_Name_1 :=
                 Chars (Defining_Identifier (Inferred_From.An_Formal));
               Error_Msg_N (Mess_2, Inferred_From.Actual.Name_Exp);
            end if;
         end if;
      end Inference_Msg;

      ------------------------------
      -- Note_Potential_Inference --
      ------------------------------

      procedure Note_Potential_Inference
        (I_Node : Node_Id; Gen_Assocs : Gen_Assocs_Rec)
      is
      begin
         if not Debug_Flag_Underscore_II or else Serious_Errors_Detected > 0
         then
            return;
         end if;

         for Index in Gen_Assocs.Assocs'Range loop
            declare
               Assoc : Assoc_Rec renames Gen_Assocs.Assocs (Index);
            begin
               if Assoc.Actual_Origin = From_Explicit_Actual
                 and then Present (Assoc.Info_Inferred_Actual)
                 and then In_Extended_Main_Source_Unit (I_Node)
                 and then not In_Internal_Unit (I_Node)
               then
                  Inference_Msg
                    (Gen_Assocs.Gen_Unit,
                     Inferred_To => Assoc,
                     Inferred_From => Gen_Assocs.Assocs (Assoc.Inferred_From),
                     Was_Inferred => False);
               end if;
            end;
         end loop;
      end Note_Potential_Inference;

      --------------
      -- Find_Assoc --
      --------------

      function Find_Assoc
        (Gen_Assocs : Gen_Assocs_Rec; F : Entity_Id) return Assoc_Index
      is
      begin
         for Index in Gen_Assocs.Assocs'Range loop
            if Defining_Entity (Gen_Assocs.Assocs (Index).An_Formal) = F then
               return Index;
            end if;
         end loop;

         raise Program_Error; -- it must be present
      end Find_Assoc;

      ---------------------
      -- Maybe_Infer_One --
      ---------------------

      procedure Maybe_Infer_One
        (Gen_Assocs : in out Gen_Assocs_Rec;
         FF, AA : N_Entity_Id; Inferred_From : Assoc_Index;
         Reason : Inference_Reason)
      is
      begin
         if not (Is_Generic_Type (FF)
                 and then Scope (FF) = Gen_Assocs.Gen_Unit)
         then
            return; -- no inference if not a formal type of this generic
         end if;

         if Is_Internal_Name (Chars (FF)) or else Is_Itype (AA) then
            return; -- no inference if internally generated
         end if;

         declare
            Index : constant Assoc_Index := Find_Assoc (Gen_Assocs, FF);
            Assoc : Assoc_Rec renames Gen_Assocs.Assocs (Index);
            pragma Assert (Defining_Entity (Assoc.An_Formal) = FF);

            From_Actual : constant Node_Id :=
              Gen_Assocs.Assocs (Inferred_From).Actual.Name_Exp;

         begin
            Assoc.Info_Inferred_Actual := AA;
            Assoc.Inferred_From := Inferred_From;
            Assoc.Reason := Reason;

            if Assoc.Actual.Kind = None then
               Assoc.Actual :=
                 (Name_Exp, New_Occurrence_Of (AA, Sloc (From_Actual)));
               Assoc.Actual_Origin := From_Inference;

               Error_Msg_GNAT_Extension
                 ("type inference of generic parameters",
                  Sloc (From_Actual));

               if Debug_Flag_Underscore_II then
                  Inference_Msg
                    (Gen_Assocs.Gen_Unit,
                     Inferred_To => Assoc,
                     Inferred_From => Gen_Assocs.Assocs (Assoc.Inferred_From),
                     Was_Inferred => True);
               end if;
            end if;
         end;
      end Maybe_Infer_One;

      -------------------
      -- Infer_Actuals --
      -------------------

      procedure Infer_From_Access
        (Gen_Assocs : in out Gen_Assocs_Rec;
         Index : Assoc_Index;
         F : Node_Id;
         A_Full : Entity_Id)
      is
      begin
         if Ekind (A_Full) in Access_Kind then
            declare
               FF : constant Entity_Id :=
                 Designated_Type (Defining_Entity (F));
               AA : constant Entity_Id := Designated_Type (A_Full);
            begin
               Maybe_Infer_One
                 (Gen_Assocs,
                 FF,
                 AA,
                 Inferred_From => Index,
                 Reason => Designated_Type);
            end;
         end if;
      end Infer_From_Access;

      procedure Infer_From_Array
        (Gen_Assocs : in out Gen_Assocs_Rec;
         Index : Assoc_Index;
         F : Node_Id;
         A_Full : Entity_Id)
      is
      begin
         if Ekind (A_Full) in Array_Kind then
            declare
               F_Index_Type : Opt_N_Is_Index_Id :=
                 First_Index (Defining_Entity (F));
               A_Index_Type : Opt_N_Is_Index_Id :=
                 First_Index (A_Full);
            begin
               while Present (F_Index_Type) and then Present (A_Index_Type)
               loop
                  Maybe_Infer_One
                    (Gen_Assocs,
                     Etype (F_Index_Type),
                     Etype (A_Index_Type),
                     Inferred_From => Index,
                     Reason => Index_Type);

                  Next_Index (F_Index_Type);
                  Next_Index (A_Index_Type);
               end loop;
            end;

            declare
               F_Comp_Type : constant Type_Kind_Id :=
                 Component_Type (Defining_Entity (F));
               A_Comp_Type : constant Type_Kind_Id :=
                 Component_Type (A_Full);
            begin
               Maybe_Infer_One
                 (Gen_Assocs,
                  F_Comp_Type,
                  A_Comp_Type,
                  Inferred_From => Index,
                  Reason => Component_Type);
            end;
         end if;
      end Infer_From_Array;

      procedure Infer_From_Discriminated
        (Gen_Assocs : in out Gen_Assocs_Rec;
         Index : Assoc_Index;
         F : Node_Id;
         A_Full : Entity_Id)
      is
      begin
         if Has_Discriminants (Defining_Entity (F))
            and then Present (A_Full)
            and then Has_Discriminants (A_Full)
            and then Number_Discriminants (A_Full) =
              Number_Discriminants (Defining_Entity (F))
         then
            declare
               F_Discrim : Node_Id := First_Discriminant (Defining_Entity (F));
               A_Discrim : Node_Id := First_Discriminant (A_Full);
            begin
               while Present (F_Discrim) loop
                  Maybe_Infer_One
                    (Gen_Assocs,
                     Etype (F_Discrim),
                     Etype (A_Discrim),
                     Inferred_From => Index,
                     Reason => Discriminant_Type);

                  Next_Discriminant (F_Discrim);
                  Next_Discriminant (A_Discrim);
               end loop;
               pragma Assert (No (A_Discrim)); -- same number as F_Discrim
            end;
         end if;
      end Infer_From_Discriminated;

      procedure Infer_Actuals (Gen_Assocs : in out Gen_Assocs_Rec) is
         --  Note that we can infer FROM defaults, but we cannot infer TO a
         --  parameter that has a default. We can also infer from inferred
         --  types.

         --  We don't need to check that multiple inferences get the same
         --  answer; the second one will get a type mismatch or nonstatically
         --  matching error.

         --  This code needs to be robust, in the sense of tolerating illegal
         --  code, because we have not yet checked all legality rules. For
         --  example, if a formal type F has a discriminant whose type is
         --  another formal type, then we want to infer the type of the
         --  discriminant from the actual for F. That actual must have
         --  discriminants, but we have not checked that rule yet, so we
         --  need to tolerate an actual for F that has no discriminants.

      begin
         --  For each parameter, check whether we can infer FROM that one TO
         --  other ones.

         --  Process the parameters in reverse order, because the inferred type
         --  always comes before the parameter it is inferred from. This
         --  ensures that we can do the inference in one pass, including in
         --  cases where an inferred type leads to another inferred type.
         --  For example, an array type that allows us to infer the component
         --  type, which is an access type that allows us to infer the
         --  designated type. The reverse loop implies that we will see the
         --  array type, then the access type, then the designated type.

         for Index in reverse Gen_Assocs.Assocs'Range loop -- NB: "reverse"
            if Gen_Assocs.Assocs (Index).Actual.Kind = Name_Exp then
               declare
                  F : constant Node_Id := Gen_Assocs.Assocs (Index).An_Formal;
                  A_E : constant Node_Id :=
                    Gen_Assocs.Assocs (Index).Actual.Name_Exp;
                  A_Full : Entity_Id := Empty;
               begin
                  if Nkind (A_E) in N_Has_Entity then
                     A_Full := Entity (A_E);

                     if Present (A_Full)
                       and then Ekind (A_Full) in Incomplete_Kind
                       and then Present (Full_View (A_Full))
                     then
                        A_Full := Full_View (A_Full);
                     end if;
                  end if;

                  if Nkind (F) = N_Formal_Type_Declaration
                    and then Present (A_Full)
                  then
                     case Ekind (Defining_Entity (F)) is
                        when E_Access_Type | E_General_Access_Type =>
                           Infer_From_Access (Gen_Assocs, Index, F, A_Full);

                        when E_Access_Subtype
                          | E_Access_Attribute_Type
                          | E_Allocator_Type
                          | E_Anonymous_Access_Type =>
                           raise Program_Error;

                        when E_Array_Type | E_Array_Subtype =>
                           Infer_From_Array (Gen_Assocs, Index, F, A_Full);

                        when E_String_Literal_Subtype =>
                           raise Program_Error;

                        when others =>
                           null;
                     end case;

                     Infer_From_Discriminated (Gen_Assocs, Index, F, A_Full);
                  end if;
               end;
            end if;
         end loop;
      end Infer_Actuals;

   end Associations;

   ---------------------------
   -- Abandon_Instantiation --
   ---------------------------

   procedure Abandon_Instantiation (N : Node_Id) is
   begin
      Error_Msg_N ("instantiation abandoned!", N);
      raise Instantiation_Error;
   end Abandon_Instantiation;

   ----------------------------------
   -- Adjust_Inherited_Pragma_Sloc --
   ----------------------------------

   procedure Adjust_Inherited_Pragma_Sloc (N : Node_Id) is
   begin
      Adjust_Instantiation_Sloc (N, S_Adjustment);
   end Adjust_Inherited_Pragma_Sloc;

   --------------------------
   -- Analyze_Associations --
   --------------------------

   function Analyze_Associations
     (I_Node  : Node_Id;
      Formals : List_Id;
      F_Copy  : List_Id) return List_Id
   is
      use Associations;

      Result_Renamings : constant List_Id  := New_List;
      --  To be returned. Includes "renamings" broadly interpreted
      --  (e.g. subtypes are used for types).

      Actuals_To_Freeze : constant Elist_Id := New_Elmt_List;
      Default_Actuals   : constant List_Id   := New_List;

      Gen_Assocs : constant Gen_Assocs_Rec :=
        Match_Assocs (I_Node, Formals, F_Copy);

   begin
      for Matching_Actual_Index in Gen_Assocs.Assocs'Range loop
         declare
            Assoc : Assoc_Rec renames
              Gen_Assocs.Assocs (Matching_Actual_Index);
         begin
            if Nkind (Assoc.Un_Formal) = N_Formal_Package_Declaration
              and then Error_Posted (Assoc.An_Formal)
            then
               --  Restrict this to N_Formal_Package_Declaration,
               --  because otherwise we miss errors.
               Abandon_Instantiation (Instantiation_Node);
            end if;

            if Nkind (Assoc.Un_Formal) in
              N_Use_Package_Clause | N_Use_Type_Clause
            then
               --  Copy the use clause to where it belongs:
               Append (New_Copy_Tree (Assoc.Un_Formal), Result_Renamings);

            else
               Analyze_One_Association
                 (I_Node, Assoc,
                  Result_Renamings, Default_Actuals, Actuals_To_Freeze);
            end if;
         end;
      end loop;

      --  An instantiation freezes all generic actuals, except for incomplete
      --  types and subprograms that are not fully defined at the point of
      --  instantiation.

      declare
         Elmt : Elmt_Id := First_Elmt (Actuals_To_Freeze);
      begin
         while Present (Elmt) loop
            Freeze_Before (I_Node, Node (Elmt));
            Next_Elmt (Elmt);
         end loop;
      end;

      --  If there are defaults, normalize the tree by adding explicit
      --  associations for them. This is required if the instance appears
      --  within a generic.

      if not Is_Empty_List (Default_Actuals) then
         declare
            Default : Node_Id;

         begin
            Default := First (Default_Actuals);
            while Present (Default) loop
               Mark_Rewrite_Insertion (Default);
               Next (Default);
            end loop;

            if No (Generic_Associations (I_Node)) then
               Set_Generic_Associations (I_Node, Default_Actuals);
            else
               Append_List_To (Generic_Associations (I_Node), Default_Actuals);
            end if;
         end;
      end if;

      Note_Potential_Inference (I_Node, Gen_Assocs);

      Check_Fixed_Point_Warning (Gen_Assocs, Result_Renamings);

      return Result_Renamings;
   end Analyze_Associations;

   -----------------------------
   -- Analyze_One_Association --
   -----------------------------

   procedure Analyze_One_Association
     (I_Node   :  Node_Id;
      Assoc    : Associations.Assoc_Rec;
      --  Logical 'in out' parameters:
      Result_Renamings  : List_Id;
      Default_Actuals   : List_Id;
      Actuals_To_Freeze : Elist_Id)
   is
      use Associations;

      procedure Process_Box_Actual (Formal : Node_Id);
      --  Called for "Formal => <>", and also if "Formal => ..." is missing,
      --  but there is "others => <>". Add a copy of the declaration of the
      --  generic formal to the Result_Renamings.

      ------------------------
      -- Process_Box_Actual --
      ------------------------

      procedure Process_Box_Actual (Formal : Node_Id) is
         pragma Assert (Assoc.Actual.Kind = Box_Actual);
         F_Id : constant Entity_Id := Defining_Entity (Formal);
         Decl : constant Node_Id   := New_Copy_Tree (Formal);
         Id   : constant Entity_Id :=
           Make_Defining_Identifier (Sloc (F_Id), Chars (F_Id));
      begin
         if Nkind (Formal) in N_Formal_Subprogram_Declaration then
            Set_Defining_Unit_Name (Specification (Decl), Id);

         else
            Set_Defining_Identifier (Decl, Id);
         end if;

         Append (Decl, Result_Renamings);
      end Process_Box_Actual;

      Match : Node_Id;

   --  Start of processing for Analyze_One_Association

   begin
      pragma Assert (Assoc.Actual_Origin /= None);

      if Assoc.Actual_Origin = From_Explicit_Actual
        and then Assoc.Actual.Kind = Name_Exp
      then
         Match := Assoc.Actual.Name_Exp;

         if Is_Entity_Name (Match) and then Present (Entity (Match)) then
            Set_Used_As_Generic_Actual (Entity (Match));
         end if;
      else
         Match := Empty;
      end if;

      case Nkind (Assoc.Un_Formal) is
         when N_Formal_Object_Declaration =>
            if Assoc.Actual.Kind = Box_Actual then
               Process_Box_Actual (Assoc.Un_Formal);

            else
               Append_List
                 (Instantiate_Object (Assoc.Un_Formal, Match, Assoc.An_Formal),
                  Result_Renamings);

               --  GNATprove: For a defaulted in-mode parameter, create
               --  an entry in the list of defaulted actuals, for
               --  GNATprove use. Do not include these defaults for an
               --  instance nested within a generic, because the defaults
               --  are also used in the analysis of the enclosing
               --  generic, and only defaulted subprograms are relevant
               --  there.

               if No (Match) and then not Inside_A_Generic then
                  Append_To (Default_Actuals,
                    Make_Generic_Association (Sloc (I_Node),
                      Selector_Name =>
                        New_Occurrence_Of
                          (Defining_Identifier
                            (Assoc.Un_Formal), Sloc (I_Node)),
                      Explicit_Generic_Actual_Parameter =>
                        New_Copy_Tree (Default_Expression (Assoc.Un_Formal))));
               end if;
            end if;

            --  If the object is a call to an expression function, this
            --  is a freezing point for it.

            if Is_Entity_Name (Match)
              and then Present (Entity (Match))
              and then Nkind
                (Original_Node (Unit_Declaration_Node (Entity (Match))))
                                               = N_Expression_Function
            then
               Append_Elmt (Entity (Match), Actuals_To_Freeze);
            end if;

         when N_Formal_Type_Declaration =>
            if Assoc.Actual.Kind = Box_Actual then
               Process_Box_Actual (Assoc.Un_Formal);

            elsif No (Match) then
               --  No explicit actual; try default

               if Present (Default_Subtype_Mark (Assoc.Un_Formal)) then
                  Match := New_Copy (Default_Subtype_Mark (Assoc.Un_Formal));
                  Append_List
                   (Instantiate_Type
                      (Assoc.Un_Formal, Match, Assoc.An_Formal,
                       Result_Renamings),
                    Result_Renamings);
                  Append_Elmt (Entity (Match), Actuals_To_Freeze);

               --  No explicit actual and no default; must be inference

               else
                  pragma Assert (Assoc.Actual_Origin = From_Inference);

                  Match := Assoc.Actual.Name_Exp;
                  Append_List
                    (Instantiate_Type
                      (Assoc.Un_Formal,
                       Match,
                       Assoc.An_Formal,
                       Result_Renamings),
                    Result_Renamings);
                  Append_Elmt (Entity (Match), Actuals_To_Freeze);
               end if;

            else
               Analyze (Match);

               --  Rewrite mutably tagged types to be their class-wide
               --  equivalent type.

               if Ekind (Etype (Match)) /= E_Void
                 and then Is_Mutably_Tagged_Type (Etype (Match))
               then
                  Rewrite (Match, New_Occurrence_Of
                    (Class_Wide_Equivalent_Type
                      (Etype (Match)), Sloc (Match)));
                  Analyze (Match);
               end if;

               Append_List
                 (Instantiate_Type
                    (Assoc.Un_Formal, Match, Assoc.An_Formal,
                     Result_Renamings),
                  Result_Renamings);

               --  An instantiation is a freeze point for the actuals,
               --  unless this is a rewritten formal package, or the
               --  formal is an Ada 2012 formal incomplete type.

               if Nkind (I_Node) = N_Formal_Package_Declaration
                 or else
                   (Ada_Version >= Ada_2012
                     and then
                       Ekind (Defining_Identifier (Assoc.An_Formal)) =
                                                      E_Incomplete_Type)
               then
                  null;

               else
                  Append_Elmt (Entity (Match), Actuals_To_Freeze);
               end if;
            end if;

            --  A remote access-to-class-wide type is not a legal actual
            --  for a generic formal of an access type (E.2.2(17/2)).
            --  In GNAT an exception to this rule is introduced when
            --  the formal is marked as remote using implementation
            --  defined aspect/pragma Remote_Access_Type. In that case
            --  the actual must be remote as well.

            --  If the current instantiation is the construction of a
            --  local copy for a formal package the actuals may be
            --  defaulted, and there is no matching actual to check.

            if Nkind (Assoc.An_Formal) = N_Formal_Type_Declaration
              and then
                Nkind (Formal_Type_Definition (Assoc.An_Formal)) =
                                      N_Access_To_Object_Definition
               and then Present (Match)
            then
               declare
                  Formal_Ent : constant Entity_Id :=
                                 Defining_Identifier (Assoc.An_Formal);
               begin
                  if Is_Remote_Access_To_Class_Wide_Type (Entity (Match))
                                          = Is_Remote_Types (Formal_Ent)
                  then
                     --  Remoteness of formal and actual match

                     null;

                  elsif Is_Remote_Types (Formal_Ent) then

                     --  Remote formal, non-remote actual

                     Error_Msg_NE
                       ("actual for& must be remote", Match, Formal_Ent);

                  else
                     --  Non-remote formal, remote actual

                     Error_Msg_NE
                       ("actual for& may not be remote",
                        Match, Formal_Ent);
                  end if;
               end;
            end if;

         when N_Formal_Subprogram_Declaration =>
            --  If there is no corresponding actual, this may be case
            --  of partial parameterization, or else the formal has a
            --  default or a box.

            if Assoc.Actual.Kind = Box_Actual then
               Process_Box_Actual (Assoc.Un_Formal);

            else
               Append_To (Result_Renamings,
                 Instantiate_Formal_Subprogram
                   (Assoc.Un_Formal, Match, Assoc.An_Formal));

               --  If formal subprogram has contracts, create wrappers
               --  for it. This is an expansion activity that cannot
               --  take place e.g. within an enclosing generic unit.

               if Has_Contracts (Assoc.An_Formal)
                 and then (Expander_Active or GNATprove_Mode)
               then
                  Build_Subprogram_Wrappers
                    (Match, Assoc.An_Formal, Result_Renamings);
               end if;

               --  An instantiation is a freeze point for the actuals,
               --  unless this is a rewritten formal package.

               if Nkind (I_Node) /= N_Formal_Package_Declaration
                 and then Nkind (Match) = N_Identifier
                 and then Is_Subprogram (Entity (Match))

                 --  The actual subprogram may rename a routine defined
                 --  in Standard. Avoid freezing such renamings because
                 --  subprograms coming from Standard cannot be frozen.

                 and then
                   not Renames_Standard_Subprogram (Entity (Match))

                 --  If the actual subprogram comes from a different
                 --  unit, it is already frozen, either by a body in
                 --  that unit or by the end of the declarative part
                 --  of the unit. This check avoids the freezing of
                 --  subprograms defined in Standard which are used
                 --  as generic actuals.

                 and then In_Same_Code_Unit (Entity (Match), I_Node)
                 and then Has_Fully_Defined_Profile (Entity (Match))
               then
                  --  Mark the subprogram as having a delayed freeze
                  --  since this may be an out-of-order action.

                  Set_Has_Delayed_Freeze (Entity (Match));
                  Append_Elmt (Entity (Match), Actuals_To_Freeze);
               end if;
            end if;

            --  If this is a nested generic, preserve default for later
            --  instantiations. We do this as well for GNATprove use,
            --  so that the list of generic associations is complete.

            if No (Match) and then Box_Present (Assoc.Un_Formal) then
               declare
                  Subp : constant Entity_Id :=
                    Defining_Unit_Name
                      (Specification (Last (Result_Renamings)));

               begin
                  Append_To (Default_Actuals,
                    Make_Generic_Association (Sloc (I_Node),
                      Selector_Name                     =>
                        New_Occurrence_Of (Subp, Sloc (I_Node)),
                      Explicit_Generic_Actual_Parameter =>
                        New_Occurrence_Of (Subp, Sloc (I_Node))));
               end;
            end if;

         when N_Formal_Package_Declaration =>
            if Assoc.Actual.Kind = Box_Actual then
               Process_Box_Actual (Assoc.Un_Formal);

            else
               Analyze (Match);
               Append_List
                 (Instantiate_Formal_Package
                    (Assoc.Un_Formal, Match, Assoc.An_Formal),
                  Result_Renamings);

               --  Determine whether the actual package needs an explicit
               --  freeze node. This is only the case if the actual is
               --  declared in the same unit and has a body. Normally
               --  packages do not have explicit freeze nodes, and gigi
               --  only uses them to elaborate entities in a package
               --  body.

               Explicit_Freeze_Check : declare
                  Actual  : constant Entity_Id := Entity (Match);
                  Gen_Par : Entity_Id;

                  Needs_Freezing : Boolean;
                  P              : Node_Id;

                  procedure Check_Generic_Parent;
                  --  The actual may be an instantiation of a unit
                  --  declared in a previous instantiation. If that
                  --  one is also in the current compilation, it must
                  --  itself be frozen before the actual. The actual
                  --  may be an instantiation of a generic child unit,
                  --  in which case the same applies to the instance
                  --  of the parent which must be frozen before the
                  --  actual.
                  --  Should this itself be recursive ???

                  --------------------------
                  -- Check_Generic_Parent --
                  --------------------------

                  procedure Check_Generic_Parent is
                     Inst : constant Node_Id :=
                              Get_Unit_Instantiation_Node (Actual);
                     Par  : Entity_Id;

                  begin
                     Par := Empty;

                     if Nkind (Parent (Actual)) = N_Package_Specification
                     then
                        Par := Scope (Generic_Parent (Parent (Actual)));

                        if Is_Generic_Instance (Par) then
                           null;

                        --  If the actual is a child generic unit, check
                        --  whether the instantiation of the parent is
                        --  also local and must also be frozen now. We
                        --  must retrieve the instance node to locate the
                        --  parent instance if any.

                        elsif Ekind (Par) = E_Generic_Package
                          and then Is_Child_Unit (Gen_Par)
                          and then Ekind (Scope (Gen_Par)) =
                                     E_Generic_Package
                        then
                           if Nkind (Inst) = N_Package_Instantiation
                             and then Nkind (Name (Inst)) =
                                        N_Expanded_Name
                           then
                              --  Retrieve entity of parent instance

                              Par := Entity (Prefix (Name (Inst)));
                           end if;

                        else
                           Par := Empty;
                        end if;
                     end if;

                     if Present (Par)
                       and then Is_Generic_Instance (Par)
                       and then Scope (Par) = Current_Scope
                       and then
                         (No (Freeze_Node (Par))
                           or else
                             not Is_List_Member (Freeze_Node (Par)))
                     then
                        Set_Has_Delayed_Freeze (Par);
                        Append_Elmt (Par, Actuals_To_Freeze);
                     end if;
                  end Check_Generic_Parent;

               --  Start of processing for Explicit_Freeze_Check

               begin
                  if Present (Renamed_Entity (Actual)) then
                     Gen_Par :=
                       Generic_Parent (Specification
                         (Unit_Declaration_Node
                           (Renamed_Entity (Actual))));
                  else
                     Gen_Par :=
                       Generic_Parent (Specification
                         (Unit_Declaration_Node (Actual)));
                  end if;

                  if not Expander_Active
                    or else not Has_Completion (Actual)
                    or else not In_Same_Source_Unit (I_Node, Actual)
                    or else Is_Frozen (Actual)
                    or else
                      (Present (Renamed_Entity (Actual))
                        and then
                          not In_Same_Source_Unit
                                (I_Node, (Renamed_Entity (Actual))))
                  then
                     null;

                  else
                     --  Finally we want to exclude such freeze nodes
                     --  from statement sequences, which freeze
                     --  everything before them.
                     --  Is this strictly necessary ???

                     Needs_Freezing := True;

                     P := Parent (I_Node);
                     while Nkind (P) /= N_Compilation_Unit loop
                        if Nkind (P) = N_Handled_Sequence_Of_Statements
                        then
                           Needs_Freezing := False;
                           exit;
                        end if;

                        P := Parent (P);
                     end loop;

                     if Needs_Freezing then
                        Check_Generic_Parent;

                        --  If the actual is a renaming of a proper
                        --  instance of the formal package, indicate
                        --  that it is the instance that must be frozen.

                        if Nkind (Parent (Actual)) =
                             N_Package_Renaming_Declaration
                        then
                           Set_Has_Delayed_Freeze
                             (Renamed_Entity (Actual));
                           Append_Elmt
                             (Renamed_Entity (Actual),
                              Actuals_To_Freeze);
                        else
                           Set_Has_Delayed_Freeze (Actual);
                           Append_Elmt (Actual, Actuals_To_Freeze);
                        end if;
                     end if;
                  end if;
               end Explicit_Freeze_Check;
            end if;

         when others =>
            raise Program_Error;
      end case;

      --  Check for correct use of Ghost entities in generic
      --  instantiations (SPARK RM 6.9(10)).

      Check_Ghost_Context_In_Generic_Association
        (Actual => Match,
         Formal => Defining_Entity (Assoc.An_Formal));
   end Analyze_One_Association;

   -------------------------------
   -- Analyze_Formal_Array_Type --
   -------------------------------

   procedure Analyze_Formal_Array_Type
     (T   : in out Entity_Id;
      Def : Node_Id)
   is
      DSS : Node_Id;

   begin
      --  Treated like a non-generic array declaration, with additional
      --  semantic checks.

      Enter_Name (T);

      if Nkind (Def) = N_Constrained_Array_Definition then
         DSS := First (Discrete_Subtype_Definitions (Def));
         while Present (DSS) loop
            if Nkind (DSS) in N_Subtype_Indication
                            | N_Range
                            | N_Attribute_Reference
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
                         (Subtype_Indication (Component_Definition (Def)))) =
                                                         N_Subtype_Indication
      then
         Error_Msg_N
           ("in a formal, a subtype indication can only be "
            & "a subtype mark (RM 12.5.3(3))",
            Subtype_Indication (Component_Definition (Def)));
      end if;

   end Analyze_Formal_Array_Type;

   ---------------------------------------------
   -- Analyze_Formal_Decimal_Fixed_Point_Type --
   ---------------------------------------------

   --  As for other generic types, we create a valid type representation with
   --  legal but arbitrary attributes, whose values are never considered
   --  static. For all scalar types we introduce an anonymous base type, with
   --  the same attributes. We choose the corresponding integer type to be
   --  Standard_Integer.
   --  Here and in other similar routines, the Sloc of the generated internal
   --  type must be the same as the sloc of the defining identifier of the
   --  formal type declaration, to provide proper source navigation.

   procedure Analyze_Formal_Decimal_Fixed_Point_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Def);

      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Decimal_Fixed_Point_Type,
                  Current_Scope,
                  Sloc (Defining_Identifier (Parent (Def))), 'G');

      Int_Base  : constant Entity_Id := Standard_Integer;
      Delta_Val : constant Ureal := Ureal_1;
      Digs_Val  : constant Uint  := Uint_6;

      function Make_Dummy_Bound return Node_Id;
      --  Return a properly typed universal real literal to use as a bound

      ----------------------
      -- Make_Dummy_Bound --
      ----------------------

      function Make_Dummy_Bound return Node_Id is
         Bound : constant Node_Id := Make_Real_Literal (Loc, Ureal_1);
      begin
         Set_Etype (Bound, Universal_Real);
         return Bound;
      end Make_Dummy_Bound;

   --  Start of processing for Analyze_Formal_Decimal_Fixed_Point_Type

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
          Low_Bound  => Make_Dummy_Bound,
          High_Bound => Make_Dummy_Bound));

      Set_Is_Generic_Type (Base);
      Set_Parent          (Base, Parent (Def));

      Mutate_Ekind       (T, E_Decimal_Fixed_Point_Subtype);
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
     (N   : Node_Id;
      T   : Entity_Id;
      Def : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Def);

   begin
      --  Rewrite as a type declaration of a derived type. This ensures that
      --  the interface list and primitive operations are properly captured.

      Rewrite (N,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => T,
          Type_Definition     => Def));

      --  Keep the aspects from the original node

      Move_Aspects (Original_Node (N), N);

      Analyze (N);
      Set_Is_Generic_Type (T);
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
             Subtype_Indication            => Subtype_Mark (Def),
             Interface_List                => Interface_List (Def));

         Set_Abstract_Present     (New_N, Abstract_Present     (Def));
         Set_Limited_Present      (New_N, Limited_Present      (Def));
         Set_Synchronized_Present (New_N, Synchronized_Present (Def));

      else
         New_N :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier         => T,
             Discriminant_Specifications =>
               Discriminant_Specifications (Parent (T)),
             Type_Definition             =>
               Make_Derived_Type_Definition (Loc,
                 Subtype_Indication => Subtype_Mark (Def)));

         Set_Abstract_Present
           (Type_Definition (New_N), Abstract_Present (Def));
         Set_Limited_Present
           (Type_Definition (New_N), Limited_Present  (Def));
      end if;

      Rewrite (N, New_N);

      --  Keep the aspects from the original node

      Move_Aspects (Original_Node (N), N);

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

      if Subtype_Mark (Def) <= Empty_Or_Error then
         pragma Assert (Serious_Errors_Detected > 0);
         --  avoid passing bad argument to Entity
         return;
      end if;

      --  If the parent type has a known size, so does the formal, which makes
      --  legal representation clauses that involve the formal.

      Set_Size_Known_At_Compile_Time
        (T, Size_Known_At_Compile_Time (Entity (Subtype_Mark (Def))));
   end Analyze_Formal_Derived_Type;

   ----------------------------------
   -- Analyze_Formal_Discrete_Type --
   ----------------------------------

   --  The operations defined for a discrete types are those of an enumeration
   --  type. The size is set to an arbitrary value, for use in analyzing the
   --  generic unit.

   procedure Analyze_Formal_Discrete_Type (T : Entity_Id; Def : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Def);
      Lo  : Node_Id;
      Hi  : Node_Id;

      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Floating_Point_Type, Current_Scope,
                  Sloc (Defining_Identifier (Parent (Def))), 'G');

   begin
      Enter_Name          (T);
      Mutate_Ekind        (T, E_Enumeration_Subtype);
      Set_Etype           (T, Base);
      Init_Size           (T, 8);
      Reinit_Alignment    (T);
      Set_Is_Generic_Type (T);
      Set_Is_Constrained  (T);

      --  For semantic analysis, the bounds of the type must be set to some
      --  non-static value. The simplest is to create attribute nodes for those
      --  bounds, that refer to the type itself. These bounds are never
      --  analyzed but serve as place-holders.

      Lo :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_First,
          Prefix         => New_Occurrence_Of (T, Loc));
      Set_Etype (Lo, T);

      Hi :=
        Make_Attribute_Reference (Loc,
          Attribute_Name => Name_Last,
          Prefix         => New_Occurrence_Of (T, Loc));
      Set_Etype (Hi, T);

      Set_Scalar_Range (T,
        Make_Range (Loc,
          Low_Bound  => Lo,
          High_Bound => Hi));

      Mutate_Ekind        (Base, E_Enumeration_Type);
      Set_Etype           (Base, Base);
      Init_Size           (Base, 8);
      Reinit_Alignment    (Base);
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
                 (E_Floating_Point_Type, Current_Scope,
                  Sloc (Defining_Identifier (Parent (Def))), 'G');

   begin
      --  The various semantic attributes are taken from the predefined type
      --  Float, just so that all of them are initialized. Their values are
      --  never used because no constant folding or expansion takes place in
      --  the generic itself.

      Enter_Name (T);
      Mutate_Ekind       (T, E_Floating_Point_Subtype);
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

   procedure Analyze_Formal_Interface_Type
      (N   : Node_Id;
       T   : Entity_Id;
       Def : Node_Id)
   is
      Loc   : constant Source_Ptr := Sloc (N);
      New_N : Node_Id;

   begin
      New_N :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => T,
          Type_Definition     => Def);

      Rewrite (N, New_N);

      --  Keep the aspects from the original node

      Move_Aspects (Original_Node (N), N);

      Analyze (N);
      Set_Is_Generic_Type (T);
   end Analyze_Formal_Interface_Type;

   ---------------------------------
   -- Analyze_Formal_Modular_Type --
   ---------------------------------

   procedure Analyze_Formal_Modular_Type (T : Entity_Id; Def : Node_Id) is
   begin
      --  Apart from their entity kind, generic modular types are treated like
      --  signed integer types, and have the same attributes.

      Analyze_Formal_Signed_Integer_Type (T, Def);
      Mutate_Ekind (T, E_Modular_Integer_Subtype);
      Mutate_Ekind (Etype (T), E_Modular_Integer_Type);

   end Analyze_Formal_Modular_Type;

   ---------------------------------------
   -- Analyze_Formal_Object_Declaration --
   ---------------------------------------

   procedure Analyze_Formal_Object_Declaration (N : Node_Id) is
      E  : constant Node_Id := Default_Expression (N);
      Id : constant Node_Id := Defining_Identifier (N);

      K                : Entity_Kind;
      Parent_Installed : Boolean := False;
      T                : Node_Id;

   begin
      Enter_Name (Id);

      Check_Abbreviated_Instance (Parent (N), Parent_Installed);

      --  Determine the mode of the formal object

      if Out_Present (N) then
         K := E_Generic_In_Out_Parameter;

         if not In_Present (N) then
            Error_Msg_N ("formal generic objects cannot have mode OUT", N);
         end if;

      else
         K := E_Generic_In_Parameter;
      end if;

      if Present (Subtype_Mark (N)) then
         Find_Type (Subtype_Mark (N));
         T := Entity (Subtype_Mark (N));

         --  Verify that there is no redundant null exclusion

         if Null_Exclusion_Present (N) then
            if not Is_Access_Type (T) then
               Error_Msg_N
                 ("null exclusion can only apply to an access type", N);

            elsif Can_Never_Be_Null (T) then
               Error_Msg_NE
                 ("`NOT NULL` not allowed (& already excludes null)", N, T);
            end if;
         end if;

      --  Ada 2005 (AI-423): Formal object with an access definition

      else
         Check_Access_Definition (N);
         T := Access_Definition
                (Related_Nod => N,
                 N           => Access_Definition (N));
      end if;

      if Ekind (T) = E_Incomplete_Type then
         declare
            Error_Node : Node_Id;

         begin
            if Present (Subtype_Mark (N)) then
               Error_Node := Subtype_Mark (N);
            else
               Check_Access_Definition (N);
               Error_Node := Access_Definition (N);
            end if;

            Error_Msg_N ("premature usage of incomplete type", Error_Node);
         end;
      end if;

      if K = E_Generic_In_Parameter then

         --  Ada 2005 (AI-287): Limited aggregates allowed in generic formals

         if Ada_Version < Ada_2005 and then Is_Limited_Type (T) then
            Error_Msg_N
              ("generic formal of mode IN must not be of limited type", N);
            Explain_Limited_Type (T, N);
         end if;

         if Is_Abstract_Type (T) then
            Error_Msg_N
              ("generic formal of mode IN must not be of abstract type", N);
         end if;

         if Present (E) then
            Preanalyze_Spec_Expression (E, T);

            --  The default for a ghost generic formal IN parameter of
            --  access-to-variable type should be a ghost object (SPARK
            --  RM 6.9(13)).

            if Is_Access_Variable (T) then
               Check_Ghost_Formal_Variable
                 (Actual     => E,
                  Formal     => Id,
                  Is_Default => True);
            end if;

            if Is_Limited_Type (T) and then not OK_For_Limited_Init (T, E) then
               Error_Msg_N
                 ("initialization not allowed for limited types", E);
               Explain_Limited_Type (T, E);
            end if;
         end if;

         Mutate_Ekind (Id, K);
         Set_Etype (Id, T);

      --  Case of generic IN OUT parameter

      else
         --  If the formal has an unconstrained type, construct its actual
         --  subtype, as is done for subprogram formals. In this fashion, all
         --  its uses can refer to specific bounds.

         Mutate_Ekind (Id, K);
         Set_Etype (Id, T);

         if (Is_Array_Type (T) and then not Is_Constrained (T))
           or else (Ekind (T) = E_Record_Type and then Has_Discriminants (T))
         then
            declare
               Non_Freezing_Ref : constant Node_Id :=
                                    New_Occurrence_Of (Id, Sloc (Id));
               Decl : Node_Id;

            begin
               --  Make sure the actual subtype doesn't generate bogus freezing

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

      Analyze_Aspect_Specifications (N, Id);

      if Parent_Installed then
         Remove_Parent;
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
                 (E_Ordinary_Fixed_Point_Type, Current_Scope,
                  Sloc (Defining_Identifier (Parent (Def))), 'G');

   begin
      --  The semantic attributes are set for completeness only, their values
      --  will never be used, since all properties of the type are non-static.

      Enter_Name (T);
      Mutate_Ekind         (T, E_Ordinary_Fixed_Point_Subtype);
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

   ----------------------------------------
   -- Analyze_Formal_Package_Declaration --
   ----------------------------------------

   procedure Analyze_Formal_Package_Declaration (N : Node_Id) is
      Gen_Id   : constant Node_Id    := Name (N);
      Loc      : constant Source_Ptr := Sloc (N);
      Pack_Id  : constant Entity_Id  := Defining_Identifier (N);
      Formal   : Entity_Id;
      Gen_Decl : Node_Id;
      Gen_Unit : Entity_Id;
      Renaming : Node_Id;

      Vis_Prims_List : Elist_Id := No_Elist;
      --  List of primitives made temporarily visible in the instantiation
      --  to match the visibility of the formal type.

      function Build_Local_Package return Node_Id;
      --  The formal package is rewritten so that its parameters are replaced
      --  with corresponding declarations. For parameters with bona fide
      --  associations these declarations are created by Analyze_Associations
      --  as for a regular instantiation. For boxed parameters, we preserve
      --  the formal declarations and analyze them, in order to introduce
      --  entities of the right kind in the environment of the formal.

      -------------------------
      -- Build_Local_Package --
      -------------------------

      function Build_Local_Package return Node_Id is
         Decls     : List_Id;
         Pack_Decl : Node_Id;

      begin
         --  Within the formal, the name of the generic package is a renaming
         --  of the formal (as for a regular instantiation).

         Pack_Decl :=
           Make_Package_Declaration (Loc,
             Specification =>
               Copy_Generic_Node
                 (Specification (Original_Node (Gen_Decl)),
                    Empty, Instantiating => True));

         Renaming :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc, Chars (Gen_Unit)),
             Name               => New_Occurrence_Of (Formal, Loc));

         if Nkind (Gen_Id) = N_Identifier
           and then Chars (Gen_Id) = Chars (Pack_Id)
         then
            Error_Msg_NE
              ("& is hidden within declaration of instance", Gen_Id, Gen_Unit);
         end if;

         --  If the formal is declared with a box, or with an others choice,
         --  create corresponding declarations for all entities in the formal
         --  part, so that names with the proper types are available in the
         --  specification of the formal package.

         --  On the other hand, if there are no associations (as in "new G;"),
         --  then all the formals must have defaults, and this will be checked
         --  by the call to Analyze_Associations.

         if Box_Present (N)
           or else Nkind (First (Generic_Associations (N))) = N_Others_Choice
         then
            declare
               Formal_Decl : Node_Id;

            begin
               --  TBA : for a formal package, need to recurse ???

               Decls := New_List;
               Formal_Decl :=
                 First
                   (Generic_Formal_Declarations (Original_Node (Gen_Decl)));
               while Present (Formal_Decl) loop
                  Append_To
                    (Decls,
                     Copy_Generic_Node
                       (Formal_Decl, Empty, Instantiating => True));
                  Next (Formal_Decl);
               end loop;
            end;

         --  If generic associations are present, use Analyze_Associations to
         --  create the proper renaming declarations.

         else
            declare
               Act_Tree : constant Node_Id :=
                            Copy_Generic_Node
                              (Original_Node (Gen_Decl), Empty,
                               Instantiating => True);

            begin
               Generic_Renamings.Set_Last (0);
               Generic_Renamings_HTable.Reset;
               Instantiation_Node := N;

               Decls :=
                 Analyze_Associations
                   (I_Node  => Original_Node (N),
                    Formals => Generic_Formal_Declarations (Act_Tree),
                    F_Copy  => Generic_Formal_Declarations (Gen_Decl));

               Vis_Prims_List := Check_Hidden_Primitives (Decls);
            end;
         end if;

         Append (Renaming, To => Decls);

         --  Add generated declarations ahead of local declarations in
         --  the package.

         if No (Visible_Declarations (Specification (Pack_Decl))) then
            Set_Visible_Declarations (Specification (Pack_Decl), Decls);
         else
            Insert_List_Before
              (First (Visible_Declarations (Specification (Pack_Decl))),
                 Decls);
         end if;

         return Pack_Decl;
      end Build_Local_Package;

      --  Local variables

      Save_ISMP : constant Boolean := Ignore_SPARK_Mode_Pragmas_In_Instance;
      --  Save flag Ignore_SPARK_Mode_Pragmas_In_Instance for restore on exit

      Associations     : Boolean := True;
      New_N            : Node_Id;
      Parent_Installed : Boolean := False;
      Parent_Instance  : Entity_Id;
      Renaming_In_Par  : Entity_Id;

   --  Start of processing for Analyze_Formal_Package_Declaration

   begin
      Check_Text_IO_Special_Unit (Gen_Id);

      Init_Env;
      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      --  Check for a formal package that is a package renaming

      if Present (Renamed_Entity (Gen_Unit)) then

         --  Indicate that unit is used, before replacing it with renamed
         --  entity for use below.

         if In_Extended_Main_Source_Unit (N) then
            Set_Is_Instantiated (Gen_Unit);
            Generate_Reference  (Gen_Unit, N);
         end if;

         Gen_Unit := Renamed_Entity (Gen_Unit);
      end if;

      if Ekind (Gen_Unit) /= E_Generic_Package then
         Error_Msg_N ("expect generic package name", Gen_Id);
         Restore_Env;
         goto Leave;

      elsif Gen_Unit = Current_Scope then
         Error_Msg_N
           ("generic package cannot be used as a formal package of itself",
            Gen_Id);
         Restore_Env;
         goto Leave;

      elsif In_Open_Scopes (Gen_Unit) then
         if Is_Compilation_Unit (Gen_Unit)
           and then Is_Child_Unit (Current_Scope)
         then
            --  Special-case the error when the formal is a parent, and
            --  continue analysis to minimize cascaded errors.

            Error_Msg_N
              ("generic parent cannot be used as formal package of a child "
               & "unit", Gen_Id);

         else
            Error_Msg_N
              ("generic package cannot be used as a formal package within "
               & "itself", Gen_Id);
            Restore_Env;
            goto Leave;
         end if;
      end if;

      --  Check that name of formal package does not hide name of generic,
      --  or its leading prefix. This check must be done separately because
      --  the name of the generic has already been analyzed.

      declare
         Gen_Name : Entity_Id;

      begin
         Gen_Name := Gen_Id;
         while Nkind (Gen_Name) = N_Expanded_Name loop
            Gen_Name := Prefix (Gen_Name);
         end loop;

         if Chars (Gen_Name) = Chars (Pack_Id) then
            Error_Msg_NE
             ("& is hidden within declaration of formal package",
              Gen_Id, Gen_Name);
         end if;
      end;

      if Box_Present (N)
        or else No (Generic_Associations (N))
        or else Nkind (First (Generic_Associations (N))) = N_Others_Choice
      then
         Associations := False;
      end if;

      --  If there are no generic associations, the generic parameters appear
      --  as local entities and are instantiated like them. We copy the generic
      --  package declaration as if it were an instantiation, and analyze it
      --  like a regular package, except that we treat the formals as
      --  additional visible components.

      Gen_Decl := Unit_Declaration_Node (Gen_Unit);

      if In_Extended_Main_Source_Unit (N) then
         Set_Is_Instantiated (Gen_Unit);
         Generate_Reference  (Gen_Unit, N);
      end if;

      Formal := New_Copy (Pack_Id);
      Create_Instantiation_Source (N, Gen_Unit, S_Adjustment);

      --  Make local generic without formals. The formals will be replaced with
      --  internal declarations.

      begin
         New_N := Build_Local_Package;

      --  If there are errors in the parameter list, Analyze_Associations
      --  raises Instantiation_Error. Patch the declaration to prevent further
      --  exception propagation.

      exception
         when Instantiation_Error =>
            Enter_Name (Formal);
            Mutate_Ekind (Formal, E_Variable);
            Set_Etype (Formal, Any_Type);
            Restore_Hidden_Primitives (Vis_Prims_List);

            if Parent_Installed then
               Remove_Parent;
            end if;

            goto Leave;
      end;

      Rewrite (N, New_N);
      Set_Defining_Unit_Name (Specification (New_N), Formal);
      Set_Generic_Parent (Specification (N), Gen_Unit);
      Set_Instance_Env (Gen_Unit, Formal);
      Set_Is_Generic_Instance (Formal);

      Enter_Name (Formal);
      Mutate_Ekind (Formal, E_Package);
      Set_Etype (Formal, Standard_Void_Type);
      Set_Inner_Instances (Formal, New_Elmt_List);

      --  It is unclear that any aspects can apply to a formal package
      --  declaration, given that they look like a hidden conformance
      --  requirement on the corresponding actual. However, Abstract_State
      --  must be treated specially because it generates declarations that
      --  must appear before other declarations in the specification and
      --  must be analyzed at once.

      if Present (Aspect_Specifications (Gen_Decl)) then
         if No (Aspect_Specifications (N)) then
            Set_Aspect_Specifications (N, New_List);
         end if;

         declare
            ASN   : Node_Id := First (Aspect_Specifications (Gen_Decl));
            New_A : Node_Id;

         begin
            while Present (ASN) loop
               if Get_Aspect_Id (ASN) = Aspect_Abstract_State then
                  New_A :=
                    Copy_Generic_Node (ASN, Empty, Instantiating => True);
                  Set_Entity (New_A, Formal);
                  Set_Analyzed (New_A, False);
                  Append (New_A, Aspect_Specifications (N));
                  Analyze_Aspect_Specifications (N, Formal);
                  exit;
               end if;

               Next (ASN);
            end loop;
         end;
      end if;

      Push_Scope  (Formal);

      --  Manually set the SPARK_Mode from the context because the package
      --  declaration is never analyzed.

      Set_SPARK_Pragma               (Formal, SPARK_Mode_Pragma);
      Set_SPARK_Aux_Pragma           (Formal, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited     (Formal);
      Set_SPARK_Aux_Pragma_Inherited (Formal);

      if Is_Child_Unit (Gen_Unit) and then Parent_Installed then

         --  Similarly, we have to make the name of the formal visible in the
         --  parent instance, to resolve properly fully qualified names that
         --  may appear in the generic unit. The parent instance has been
         --  placed on the scope stack ahead of the current scope.

         Parent_Instance := Scope_Stack.Table (Scope_Stack.Last - 1).Entity;

         Renaming_In_Par :=
           Make_Defining_Identifier (Loc, Chars (Gen_Unit));
         Mutate_Ekind (Renaming_In_Par, E_Package);
         Set_Is_Not_Self_Hidden (Renaming_In_Par);
         Set_Etype (Renaming_In_Par, Standard_Void_Type);
         Set_Scope (Renaming_In_Par, Parent_Instance);
         Set_Parent (Renaming_In_Par, Parent (Formal));
         Set_Renamed_Entity (Renaming_In_Par, Formal);
         Append_Entity (Renaming_In_Par, Parent_Instance);
      end if;

      --  A formal package declaration behaves as a package instantiation with
      --  respect to SPARK_Mode "off". If the annotation is "off" or altogether
      --  missing, set the global flag which signals Analyze_Pragma to ingnore
      --  all SPARK_Mode pragmas within the generic_package_name.

      if SPARK_Mode /= On then
         Ignore_SPARK_Mode_Pragmas_In_Instance := True;

         --  Mark the formal spec in case the body is instantiated at a later
         --  pass. This preserves the original context in effect for the body.

         Set_Ignore_SPARK_Mode_Pragmas (Formal);
      end if;

      Analyze (Specification (N));

      --  The formals for which associations are provided are not visible
      --  outside of the formal package. The others are still declared by a
      --  formal parameter declaration.

      --  If there are no associations, the only local entity to hide is the
      --  generated package renaming itself.

      declare
         E : Entity_Id;

      begin
         E := First_Entity (Formal);
         while Present (E) loop
            if Associations and then not Is_Generic_Formal (E) then
               Set_Is_Hidden (E);
            end if;

            if Ekind (E) = E_Package and then Renamed_Entity (E) = Formal then
               Set_Is_Hidden (E);
               exit;
            end if;

            Next_Entity (E);
         end loop;
      end;

      End_Package_Scope (Formal);
      Restore_Hidden_Primitives (Vis_Prims_List);

      if Parent_Installed then
         Remove_Parent;
      end if;

      Restore_Env;

      --  Inside the generic unit, the formal package is a regular package, but
      --  no body is needed for it. Note that after instantiation, the defining
      --  unit name we need is in the new tree and not in the original (see
      --  Package_Instantiation). A generic formal package is an instance, and
      --  can be used as an actual for an inner instance.

      Set_Has_Completion (Formal, True);

      --  Add semantic information to the original defining identifier.

      Mutate_Ekind (Pack_Id, E_Package);
      Set_Etype (Pack_Id, Standard_Void_Type);
      Set_Scope (Pack_Id, Scope (Formal));
      Set_Has_Completion (Pack_Id, True);

   <<Leave>>
      --  Unclear that any other aspects may appear here, analyze them
      --  for completion, given that the grammar allows their appearance.

      Analyze_Aspect_Specifications (N, Pack_Id);

      Ignore_SPARK_Mode_Pragmas_In_Instance := Save_ISMP;
   end Analyze_Formal_Package_Declaration;

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

   ------------------------------------
   -- Analyze_Formal_Incomplete_Type --
   ------------------------------------

   procedure Analyze_Formal_Incomplete_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
   begin
      Enter_Name (T);
      Mutate_Ekind (T, E_Incomplete_Type);
      Set_Etype (T, T);
      Set_Private_Dependents (T, New_Elmt_List);

      if Tagged_Present (Def) then
         Set_Is_Tagged_Type (T);
         Make_Class_Wide_Type (T);
         Set_Direct_Primitive_Operations (T, New_Elmt_List);
      end if;
   end Analyze_Formal_Incomplete_Type;

   ----------------------------------------
   -- Analyze_Formal_Signed_Integer_Type --
   ----------------------------------------

   procedure Analyze_Formal_Signed_Integer_Type
     (T   : Entity_Id;
      Def : Node_Id)
   is
      Base : constant Entity_Id :=
               New_Internal_Entity
                 (E_Signed_Integer_Type,
                  Current_Scope,
                  Sloc (Defining_Identifier (Parent (Def))), 'G');

   begin
      Enter_Name (T);

      Mutate_Ekind       (T, E_Signed_Integer_Subtype);
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

   -------------------------------------------
   -- Analyze_Formal_Subprogram_Declaration --
   -------------------------------------------

   procedure Analyze_Formal_Subprogram_Declaration (N : Node_Id) is
      Spec : constant Node_Id   := Specification (N);
      Def  : constant Node_Id   := Default_Name (N);
      Expr : constant Node_Id   := Expression (N);
      Nam  : constant Entity_Id := Defining_Unit_Name (Spec);

      Parent_Installed : Boolean := False;
      Subp             : Entity_Id;

   begin
      if Nam = Error then
         return;
      end if;

      if Nkind (Nam) = N_Defining_Program_Unit_Name then
         Error_Msg_N ("name of formal subprogram must be a direct name", Nam);
         goto Leave;
      end if;

      Check_Abbreviated_Instance (Parent (N), Parent_Installed);

      Analyze_Subprogram_Declaration (N);
      Set_Is_Formal_Subprogram (Nam);
      Set_Has_Completion (Nam);

      if Nkind (N) = N_Formal_Abstract_Subprogram_Declaration then
         Set_Is_Abstract_Subprogram (Nam);

         Set_Is_Dispatching_Operation (Nam);

         --  A formal abstract procedure cannot have a null default
         --  (RM 12.6(4.1/2)).

         if Has_Null_Default (N) then
            Error_Msg_N
              ("a formal abstract subprogram cannot default to null", Spec);
         end if;

         --  A formal abstract function cannot have an expression default
         --  (expression defaults are allowed for nonabstract formal functions
         --  when extensions are enabled).

         if Nkind (Spec) = N_Function_Specification
           and then Present (Expr)
         then
            Error_Msg_N
              ("a formal abstract subprogram cannot default to an expression",
               Spec);
         end if;

         declare
            Ctrl_Type : constant Entity_Id := Find_Dispatching_Type (Nam);
         begin
            if No (Ctrl_Type) then
               Error_Msg_N
                 ("abstract formal subprogram must have a controlling type",
                  N);

            elsif Ada_Version >= Ada_2012
              and then Is_Incomplete_Type (Ctrl_Type)
            then
               Error_Msg_NE
                 ("controlling type of abstract formal subprogram cannot "
                  & "be incomplete type", N, Ctrl_Type);

            else
               Check_Controlling_Formals (Ctrl_Type, Nam);
            end if;
         end;
      end if;

      --  Default name is resolved at the point of instantiation

      if Box_Present (N) then
         null;

      --  Default name is bound at the point of generic declaration

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
            goto Leave;
         end if;

         --  The default for a ghost generic formal procedure should be a ghost
         --  procedure (SPARK RM 6.9(13)).

         if Ekind (Nam) = E_Procedure then
            declare
               Def_E : Entity_Id := Empty;
            begin
               if Nkind (Def) in N_Has_Entity then
                  Def_E := Entity (Def);
               end if;

               Check_Ghost_Formal_Procedure_Or_Package
                 (N          => Def,
                  Actual     => Def_E,
                  Formal     => Nam,
                  Is_Default => True);
            end;
         end if;

         --  Default name may be overloaded, in which case the interpretation
         --  with the correct profile must be selected, as for a renaming.
         --  If the definition is an indexed component, it must denote a
         --  member of an entry family. If it is a selected component, it
         --  can be a protected operation.

         if Etype (Def) = Any_Type then
            goto Leave;

         elsif Nkind (Def) = N_Selected_Component then
            if not Is_Overloadable (Entity (Selector_Name (Def))) then
               Error_Msg_N ("expect valid subprogram name as default", Def);
            end if;

         elsif Nkind (Def) = N_Indexed_Component then
            if Is_Entity_Name (Prefix (Def)) then
               if Ekind (Entity (Prefix (Def))) /= E_Entry_Family then
                  Error_Msg_N ("expect valid subprogram name as default", Def);
               end if;

            elsif Nkind (Prefix (Def)) = N_Selected_Component then
               if Ekind (Entity (Selector_Name (Prefix (Def)))) /=
                                                          E_Entry_Family
               then
                  Error_Msg_N ("expect valid subprogram name as default", Def);
               end if;

            else
               Error_Msg_N ("expect valid subprogram name as default", Def);
               goto Leave;
            end if;

         elsif Nkind (Def) = N_Character_Literal then

            --  Needs some type checks: subprogram should be parameterless???

            Resolve (Def, (Etype (Nam)));

         elsif not Is_Entity_Name (Def)
           or else not Is_Overloadable (Entity (Def))
         then
            Error_Msg_N ("expect valid subprogram name as default", Def);
            goto Leave;

         elsif not Is_Overloaded (Def) then
            Subp := Entity (Def);

            if Subp = Nam then
               Error_Msg_N ("premature usage of formal subprogram", Def);

            elsif not Entity_Matches_Spec (Subp, Nam) then
               Error_Msg_N ("no visible entity matches specification", Def);
            end if;

         --  More than one interpretation, so disambiguate as for a renaming

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

               --  Subprogram found, generate reference to it

               Set_Entity (Def, Subp);
               Generate_Reference (Subp, Def);

               if Subp = Nam then
                  Error_Msg_N ("premature usage of formal subprogram", Def);

               elsif Ekind (Subp) /= E_Operator then
                  Check_Mode_Conformant (Subp, Nam);
               end if;

            else
               Error_Msg_N ("no visible subprogram matches specification", N);
            end if;
         end if;

      --  When extensions are enabled, an expression can be given as default
      --  for a formal function. The expression must be of the function result
      --  type and can reference formal parameters of the function.

      elsif Present (Expr) then
         Push_Scope (Nam);
         Install_Formals (Nam);
         Preanalyze_Spec_Expression (Expr, Etype (Nam));
         End_Scope;
      end if;

   <<Leave>>
      Analyze_Aspect_Specifications (N, Nam);

      if Parent_Installed then
         Remove_Parent;
      end if;
   end Analyze_Formal_Subprogram_Declaration;

   -------------------------------------
   -- Analyze_Formal_Type_Declaration --
   -------------------------------------

   procedure Analyze_Formal_Type_Declaration (N : Node_Id) is
      Def : constant Node_Id := Formal_Type_Definition (N);

      Parent_Installed : Boolean := False;
      T                : Entity_Id;

   begin
      T := Defining_Identifier (N);

      if Present (Discriminant_Specifications (N))
        and then Nkind (Def) /= N_Formal_Private_Type_Definition
      then
         Error_Msg_N
           ("discriminants not allowed for this formal type", T);
      end if;

      Check_Abbreviated_Instance (Parent (N), Parent_Installed);

      --  Enter the new name, and branch to specific routine

      case Nkind (Def) is
         when N_Formal_Private_Type_Definition =>
            Analyze_Formal_Private_Type (N, T, Def);

         when N_Formal_Derived_Type_Definition =>
            Analyze_Formal_Derived_Type (N, T, Def);

         when N_Formal_Incomplete_Type_Definition =>
            Analyze_Formal_Incomplete_Type (T, Def);

         when N_Formal_Discrete_Type_Definition =>
            Analyze_Formal_Discrete_Type (T, Def);

         when N_Formal_Signed_Integer_Type_Definition =>
            Analyze_Formal_Signed_Integer_Type (T, Def);

         when N_Formal_Modular_Type_Definition =>
            Analyze_Formal_Modular_Type (T, Def);

         when N_Formal_Floating_Point_Definition =>
            Analyze_Formal_Floating_Type (T, Def);

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            Analyze_Formal_Ordinary_Fixed_Point_Type (T, Def);

         when N_Formal_Decimal_Fixed_Point_Definition =>
            Analyze_Formal_Decimal_Fixed_Point_Type (T, Def);

         when N_Array_Type_Definition =>
            Analyze_Formal_Array_Type (T, Def);

         when N_Access_Function_Definition
            | N_Access_Procedure_Definition
            | N_Access_To_Object_Definition
         =>
            Analyze_Generic_Access_Type (T, Def);

         --  Ada 2005: a interface declaration is encoded as an abstract
         --  record declaration or a abstract type derivation.

         when N_Record_Definition =>
            Analyze_Formal_Interface_Type (N, T, Def);

         when N_Derived_Type_Definition =>
            Analyze_Formal_Derived_Interface_Type (N, T, Def);

         when N_Error =>
            null;

         when others =>
            raise Program_Error;
      end case;

      --  A formal type declaration declares a type and its first
      --  subtype.

      Set_Is_Generic_Type (T);
      Set_Is_First_Subtype (T);

      if Present (Default_Subtype_Mark (Original_Node (N))) then
         Validate_Formal_Type_Default (N);
      end if;

      Analyze_Aspect_Specifications (N, T);

      if Parent_Installed then
         Remove_Parent;
      end if;
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

         elsif not Is_Entity_Name (Subtype_Indication (Def)) then
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
      --  The generic formals are processed in the scope of the generic unit,
      --  where they are immediately visible. The scope is installed by the
      --  caller.

      Gen_Parm_Decl := First (Generic_Formal_Declarations (N));
      while Present (Gen_Parm_Decl) loop
         Analyze (Gen_Parm_Decl);
         Next (Gen_Parm_Decl);
      end loop;

      Generate_Reference_To_Generic_Formals (Current_Scope);

      --  For Ada 2022, some formal parameters can carry aspects, which must
      --  be name-resolved at the end of the list of formal parameters (which
      --  has the semantics of a declaration list).

      Analyze_Contracts (Generic_Formal_Declarations (N));
   end Analyze_Generic_Formal_Part;

   ------------------------------------------
   -- Analyze_Generic_Package_Declaration  --
   ------------------------------------------

   procedure Analyze_Generic_Package_Declaration (N : Node_Id) is
      Decls : constant List_Id    := Visible_Declarations (Specification (N));
      Loc   : constant Source_Ptr := Sloc (N);

      Decl        : Node_Id;
      Id          : Entity_Id;
      New_N       : Node_Id;
      Renaming    : Node_Id;
      Save_Parent : Node_Id;

   begin
      --  A generic may grant access to its private enclosing context depending
      --  on the placement of its corresponding body. From elaboration point of
      --  view, the flow of execution may enter this private context, and then
      --  reach an external unit, thus producing a dependency on that external
      --  unit. For such a path to be properly discovered and encoded in the
      --  ALI file of the main unit, let the ABE mechanism process the body of
      --  the main unit, and encode all relevant invocation constructs and the
      --  relations between them.

      Mark_Save_Invocation_Graph_Of_Body;

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
          Name               =>
            Make_Identifier (Loc, Chars (Defining_Entity (N))));

      --  The declaration is inserted before other declarations, but before
      --  pragmas that may be library-unit pragmas and must appear before other
      --  declarations. The pragma Compile_Time_Error is not in this class, and
      --  may contain an expression that includes such a qualified name, so the
      --  renaming declaration must appear before it.

      --  Are there other pragmas that require this special handling ???

      if Present (Decls) then
         Decl := First (Decls);
         while Present (Decl)
           and then Nkind (Decl) = N_Pragma
           and then Get_Pragma_Id (Decl) /= Pragma_Compile_Time_Error
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

      --  Create copy of generic unit, and save for instantiation. If the unit
      --  is a child unit, do not copy the specifications for the parent, which
      --  are not part of the generic tree.

      Save_Parent := Parent_Spec (N);
      Set_Parent_Spec (N, Empty);

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Set_Parent_Spec (New_N, Save_Parent);
      Rewrite (N, New_N);

      --  Collect all contract-related source pragmas found within the template
      --  and attach them to the contract of the package spec. This contract is
      --  used in the capture of global references within annotations.

      Create_Generic_Contract (N);

      Id := Defining_Entity (N);
      Generate_Definition (Id);

      --  Expansion is not applied to generic units

      Start_Generic;

      Enter_Name (Id);
      Mutate_Ekind (Id, E_Generic_Package);
      Set_Is_Not_Self_Hidden (Id);
      Set_Etype (Id, Standard_Void_Type);

      --  Set SPARK_Mode from context

      Set_SPARK_Pragma               (Id, SPARK_Mode_Pragma);
      Set_SPARK_Aux_Pragma           (Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited     (Id);
      Set_SPARK_Aux_Pragma_Inherited (Id);

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => Id,
         Checks   => True,
         Warnings => True);

      --  Analyze aspects now, so that generated pragmas appear in the
      --  declarations before building and analyzing the generic copy.

      Analyze_Aspect_Specifications (N, Id);

      Push_Scope (Id);
      Enter_Generic_Scope (Id);
      Set_Inner_Instances (Id, New_Elmt_List);

      Set_Categorization_From_Pragmas (N);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      --  Link the declaration of the generic homonym in the generic copy to
      --  the package it renames, so that it is always resolved properly.

      Set_Generic_Homonym (Id, Defining_Unit_Name (Renaming));
      Set_Entity (Associated_Node (Name (Renaming)), Id);

      --  For a library unit, we have reconstructed the entity for the unit,
      --  and must reset it in the library tables.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Cunit_Entity (Current_Sem_Unit, Id);
      end if;

      Analyze_Generic_Formal_Part (N);

      --  After processing the generic formals, analysis proceeds as for a
      --  non-generic package.

      Analyze (Specification (N));

      Validate_Categorization_Dependency (N, Id);

      End_Generic;

      End_Package_Scope (Id);
      Exit_Generic_Scope (Id);

      --  If the generic appears within a package unit, the body of that unit
      --  has to be present for instantiation and inlining.

      if Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Declaration then
         Set_Body_Needed_For_Inlining
           (Defining_Entity (Unit (Cunit (Current_Sem_Unit))));
      end if;

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

      --  If there is a specified storage pool in the context, create an
      --  aspect on the package declaration, so that it is used in any
      --  instance that does not override it.

      if Present (Default_Pool) then
         declare
            ASN : Node_Id;

         begin
            ASN :=
              Make_Aspect_Specification (Loc,
                Identifier => Make_Identifier (Loc, Name_Default_Storage_Pool),
                Expression => New_Copy (Default_Pool));

            if No (Aspect_Specifications (Specification (N))) then
               Set_Aspect_Specifications (Specification (N), New_List (ASN));
            else
               Append (ASN, Aspect_Specifications (Specification (N)));
            end if;
         end;
      end if;
   end Analyze_Generic_Package_Declaration;

   --------------------------------------------
   -- Analyze_Generic_Subprogram_Declaration --
   --------------------------------------------

   procedure Analyze_Generic_Subprogram_Declaration (N : Node_Id) is
      Formals     : List_Id;
      Id          : Entity_Id;
      New_N       : Node_Id;
      Result_Type : Entity_Id;
      Save_Parent : Node_Id;
      Spec        : Node_Id;
      Typ         : Entity_Id;

   begin
      --  A generic may grant access to its private enclosing context depending
      --  on the placement of its corresponding body. From elaboration point of
      --  view, the flow of execution may enter this private context, and then
      --  reach an external unit, thus producing a dependency on that external
      --  unit. For such a path to be properly discovered and encoded in the
      --  ALI file of the main unit, let the ABE mechanism process the body of
      --  the main unit, and encode all relevant invocation constructs and the
      --  relations between them.

      Mark_Save_Invocation_Graph_Of_Body;

      --  Create copy of generic unit, and save for instantiation. If the unit
      --  is a child unit, do not copy the specifications for the parent, which
      --  are not part of the generic tree.

      Save_Parent := Parent_Spec (N);
      Set_Parent_Spec (N, Empty);

      New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
      Set_Parent_Spec (New_N, Save_Parent);
      Rewrite (N, New_N);

      --  Collect all contract-related source pragmas found within the template
      --  and attach them to the contract of the subprogram spec. This contract
      --  is used in the capture of global references within annotations.

      Create_Generic_Contract (N);

      Spec := Specification (N);
      Id   := Defining_Entity (Spec);
      Generate_Definition (Id);

      if Nkind (Id) = N_Defining_Operator_Symbol then
         Error_Msg_N
           ("operator symbol not allowed for generic subprogram", Id);
      end if;

      Start_Generic;

      Enter_Name (Id);
      Set_Scope_Depth_Value (Id, Scope_Depth (Current_Scope) + 1);

      Push_Scope (Id);
      Enter_Generic_Scope (Id);
      Set_Inner_Instances (Id, New_Elmt_List);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      Analyze_Generic_Formal_Part (N);

      if Nkind (Spec) = N_Function_Specification then
         Mutate_Ekind (Id, E_Generic_Function);
      else
         Mutate_Ekind (Id, E_Generic_Procedure);
      end if;

      --  Set SPARK_Mode from context

      Set_SPARK_Pragma           (Id, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (Id);

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => Id,
         Checks   => True,
         Warnings => True);

      Formals := Parameter_Specifications (Spec);

      if Present (Formals) then
         Process_Formals (Formals, Spec);
      end if;

      if Nkind (Spec) = N_Function_Specification then
         if Nkind (Result_Definition (Spec)) = N_Access_Definition then
            Result_Type := Access_Definition (Spec, Result_Definition (Spec));
            Set_Etype (Id, Result_Type);

            --  Check restriction imposed by AI05-073: a generic function
            --  cannot return an abstract type or an access to such.

            if Is_Abstract_Type (Designated_Type (Result_Type)) then
               Error_Msg_N
                 ("generic function cannot have an access result "
                  & "that designates an abstract type", Spec);
            end if;

         else
            Find_Type (Result_Definition (Spec));
            Typ := Entity (Result_Definition (Spec));

            if Is_Abstract_Type (Typ)
              and then Ada_Version >= Ada_2012
            then
               Error_Msg_N
                 ("generic function cannot have abstract result type", Spec);
            end if;

            --  If a null exclusion is imposed on the result type, then create
            --  a null-excluding itype (an access subtype) and use it as the
            --  function's Etype.

            if Is_Access_Type (Typ)
              and then Null_Exclusion_Present (Spec)
            then
               Set_Etype  (Id,
                 Create_Null_Excluding_Itype
                   (T           => Typ,
                    Related_Nod => Spec,
                    Scope_Id    => Defining_Unit_Name (Spec)));
            else
               Set_Etype (Id, Typ);
            end if;
         end if;

      else
         Set_Etype (Id, Standard_Void_Type);
      end if;

      Set_Is_Not_Self_Hidden (Id);

      --  Analyze the aspects of the generic copy to ensure that all generated
      --  pragmas (if any) perform their semantic effects.

      Analyze_Aspect_Specifications (N, Id);

      --  For a library unit, we have reconstructed the entity for the unit,
      --  and must reset it in the library tables. We also make sure that
      --  Body_Required is set properly in the original compilation unit node.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Cunit_Entity (Current_Sem_Unit, Id);
         Set_Body_Required (Parent (N), Unit_Requires_Body (Id));
      end if;

      --  If the generic appears within a package unit, the body of that unit
      --  has to be present for instantiation and inlining.

      if Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Declaration
        and then Unit_Requires_Body (Id)
      then
         Set_Body_Needed_For_Inlining
           (Defining_Entity (Unit (Cunit (Current_Sem_Unit))));
      end if;

      Set_Categorization_From_Pragmas (N);
      Validate_Categorization_Dependency (N, Id);

      --  Capture all global references that occur within the profile of the
      --  generic subprogram. Aspects are not part of this processing because
      --  they must be delayed. If processed now, Save_Global_References will
      --  destroy the Associated_Node links and prevent the capture of global
      --  references when the contract of the generic subprogram is analyzed.

      Save_Global_References (Original_Node (N));

      End_Generic;
      End_Scope;
      Exit_Generic_Scope (Id);
      Generate_Reference_To_Formals (Id);

      List_Inherited_Pre_Post_Aspects (Id);
   end Analyze_Generic_Subprogram_Declaration;

   -----------------------------------
   -- Analyze_Package_Instantiation --
   -----------------------------------

   --  WARNING: This routine manages Ghost and SPARK regions. Return statements
   --  must be replaced by gotos which jump to the end of the routine in order
   --  to restore the Ghost and SPARK modes.

   procedure Analyze_Package_Instantiation (N : Node_Id) is
      Has_Inline_Always : Boolean := False;
      --  Set if the generic unit contains any subprograms with Inline_Always.
      --  Only relevant when back-end inlining is not enabled.

      function Might_Inline_Subp (Gen_Unit : Entity_Id) return Boolean;
      --  Return True if inlining is active and Gen_Unit contains inlined
      --  subprograms. In this case, we may either instantiate the body when
      --  front-end inlining is enabled, or add a pending instantiation when
      --  back-end inlining is enabled. In the former case, this may cause
      --  superfluous instantiations, but in either case we need to perform
      --  the instantiation of the body in the context of the instance and
      --  not in that of the point of inlining.

      function Needs_Body_Instantiated (Gen_Unit : Entity_Id) return Boolean;
      --  Return True if Gen_Unit needs to have its body instantiated in the
      --  context of N. This in particular excludes generic contexts.

      -----------------------
      -- Might_Inline_Subp --
      -----------------------

      function Might_Inline_Subp (Gen_Unit : Entity_Id) return Boolean is
         E : Entity_Id;

      begin
         if Inline_Processing_Required then
            --  No need to recompute the answer if we know it is positive
            --  and back-end inlining is enabled.

            if Is_Inlined (Gen_Unit) and then Back_End_Inlining then
               return True;
            end if;

            E := First_Entity (Gen_Unit);
            while Present (E) loop
               if Is_Subprogram (E) and then Is_Inlined (E) then
                  --  Remember if there are any subprograms with Inline_Always

                  if Has_Pragma_Inline_Always (E) then
                     Has_Inline_Always := True;
                  end if;

                  Set_Is_Inlined (Gen_Unit);
                  return True;
               end if;

               Next_Entity (E);
            end loop;
         end if;

         return False;
      end Might_Inline_Subp;

      -------------------------------
      --  Needs_Body_Instantiated  --
      -------------------------------

      function Needs_Body_Instantiated (Gen_Unit : Entity_Id) return Boolean is
      begin
         --  No need to instantiate bodies in generic units

         if Is_Generic_Unit (Cunit_Entity (Main_Unit)) then
            return False;
         end if;

         --  If the instantiation is in the main unit, then the body is needed

         if Is_In_Main_Unit (N) then
            return True;
         end if;

         --  In GNATprove mode, never instantiate bodies outside of the main
         --  unit, as it does not use frontend/backend inlining in the way that
         --  GNAT does, so does not benefit from such instantiations. On the
         --  contrary, such instantiations may bring artificial constraints,
         --  as for example such bodies may require preprocessing.

         if GNATprove_Mode then
            return False;
         end if;

         --  If not, then again no need to instantiate bodies in generic units

         if Is_Generic_Unit (Cunit_Entity (Get_Code_Unit (N))) then
            return False;
         end if;

         --  Here we have a special handling for back-end inlining: if inline
         --  processing is required, then we unconditionally want to have the
         --  body instantiated. The reason is that Might_Inline_Subp does not
         --  catch all the cases (as it does not recurse into nested packages)
         --  so this avoids the need to patch things up afterwards. Moreover,
         --  these instantiations are only performed on demand when back-end
         --  inlining is enabled, so this causes very little extra work.

         if Inline_Processing_Required and then Back_End_Inlining then
            return True;
         end if;

         --  We want to have the bodies instantiated in non-main units if
         --  they might contribute inlined subprograms.

         return Might_Inline_Subp (Gen_Unit);
      end Needs_Body_Instantiated;

      --  Local declarations

      Gen_Id         : constant Node_Id    := Name (N);
      Inst_Id        : constant Entity_Id  := Defining_Entity (N);
      Is_Actual_Pack : constant Boolean    := Is_Internal (Inst_Id);
      Loc            : constant Source_Ptr := Sloc (N);

      Saved_GM   : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR  : constant Node_Id         := Ignored_Ghost_Region;
      Saved_ISMP : constant Boolean         :=
                     Ignore_SPARK_Mode_Pragmas_In_Instance;
      Saved_SM   : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP  : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the Ghost and SPARK mode-related data to restore on exit

      Saved_Style_Check : constant Boolean := Style_Check;
      --  Save style check mode for restore on exit

      Act_Decl         : Node_Id;
      Act_Decl_Name    : Node_Id;
      Act_Decl_Id      : Entity_Id;
      Act_Spec         : Node_Id;
      Act_Tree         : Node_Id;
      Env_Installed    : Boolean := False;
      Gen_Decl         : Node_Id;
      Gen_Spec         : Node_Id;
      Gen_Unit         : Entity_Id;
      Inline_Now       : Boolean := False;
      Needs_Body       : Boolean;
      Parent_Installed : Boolean := False;
      Renamings        : List_Id;
      Unit_Renaming    : Node_Id;

      Vis_Prims_List : Elist_Id := No_Elist;
      --  List of primitives made temporarily visible in the instantiation
      --  to match the visibility of the formal type

   --  Start of processing for Analyze_Package_Instantiation

   begin
      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => N,
         Checks   => True,
         Level    => True,
         Modes    => True,
         Warnings => True);

      --  Very first thing: check for Text_IO special unit in case we are
      --  instantiating one of the children of [[Wide_]Wide_]Text_IO.

      Check_Text_IO_Special_Unit (Name (N));

      --  Make node global for error reporting

      Instantiation_Node := N;

      --  Case of instantiation of a generic package

      if Nkind (N) = N_Package_Instantiation then
         Act_Decl_Id := New_Copy (Defining_Entity (N));

         if Nkind (Defining_Unit_Name (N)) = N_Defining_Program_Unit_Name then
            Act_Decl_Name :=
              Make_Defining_Program_Unit_Name (Loc,
                Name                =>
                  New_Copy_Tree (Name (Defining_Unit_Name (N))),
                Defining_Identifier => Act_Decl_Id);
         else
            Act_Decl_Name := Act_Decl_Id;
         end if;

      --  Case of instantiation of a formal package

      else
         Act_Decl_Id   := Defining_Identifier (N);
         Act_Decl_Name := Act_Decl_Id;
      end if;

      Generate_Definition (Act_Decl_Id);
      Mutate_Ekind (Act_Decl_Id, E_Package);
      Set_Is_Not_Self_Hidden (Act_Decl_Id);

      --  Initialize list of incomplete actuals before analysis

      Set_Incomplete_Actuals (Act_Decl_Id, New_Elmt_List);

      Preanalyze_Actuals (N, Act_Decl_Id);

      --  Turn off style checking in instances. If the check is enabled on the
      --  generic unit, a warning in an instance would just be noise. If not
      --  enabled on the generic, then a warning in an instance is just wrong.
      --  This must be done after analyzing the actuals, which do come from
      --  source and are subject to style checking.

      Style_Check := False;

      Init_Env;
      Env_Installed := True;

      --  Reset renaming map for formal types. The mapping is established
      --  when analyzing the generic associations, but some mappings are
      --  inherited from formal packages of parent units, and these are
      --  constructed when the parents are installed.

      Generic_Renamings.Set_Last (0);
      Generic_Renamings_HTable.Reset;

      --  Except for an abbreviated instance created to check a formal package,
      --  install the parent if this is a generic child unit.

      if not Is_Abbreviated_Instance (Inst_Id) then
         Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      end if;

      Gen_Unit := Entity (Gen_Id);

      --  A package instantiation is Ghost when it is subject to pragma Ghost
      --  or the generic template is Ghost. Set the mode now to ensure that
      --  any nodes generated during analysis and expansion are marked as
      --  Ghost.

      Mark_And_Set_Ghost_Instantiation (N, Gen_Unit);

      --  Verify that it is the name of a generic package

      --  A visibility glitch: if the instance is a child unit and the generic
      --  is the generic unit of a parent instance (i.e. both the parent and
      --  the child units are instances of the same package) the name now
      --  denotes the renaming within the parent, not the intended generic
      --  unit. See if there is a homonym that is the desired generic. The
      --  renaming declaration must be visible inside the instance of the
      --  child, but not when analyzing the name in the instantiation itself.

      if Ekind (Gen_Unit) = E_Package
        and then Present (Renamed_Entity (Gen_Unit))
        and then In_Open_Scopes (Renamed_Entity (Gen_Unit))
        and then Is_Generic_Instance (Renamed_Entity (Gen_Unit))
        and then Present (Homonym (Gen_Unit))
      then
         Gen_Unit := Homonym (Gen_Unit);
      end if;

      if Etype (Gen_Unit) = Any_Type then
         Restore_Env;
         goto Leave;

      elsif Ekind (Gen_Unit) /= E_Generic_Package then

         --  Ada 2005 (AI-50217): Cannot use instance in limited with_clause

         if From_Limited_With (Gen_Unit) then
            Error_Msg_N
              ("cannot instantiate a limited withed package", Gen_Id);
         else
            Error_Msg_NE
              ("& is not the name of a generic package", Gen_Id, Gen_Unit);
         end if;

         Restore_Env;
         goto Leave;
      end if;

      if In_Extended_Main_Source_Unit (N) then
         Set_Is_Instantiated (Gen_Unit);
         Generate_Reference  (Gen_Unit, N);

         if Present (Renamed_Entity (Gen_Unit)) then
            Set_Is_Instantiated (Renamed_Entity (Gen_Unit));
            Generate_Reference  (Renamed_Entity (Gen_Unit), N);
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
           ("& is hidden within declaration of instance", Prefix (Gen_Id));
      end if;

      Set_Entity (Gen_Id, Gen_Unit);

      --  If generic is a renaming, get original generic unit

      if Present (Renamed_Entity (Gen_Unit))
        and then Ekind (Renamed_Entity (Gen_Unit)) = E_Generic_Package
      then
         Gen_Unit := Renamed_Entity (Gen_Unit);
      end if;

      --  Verify that there are no circular instantiations

      if In_Open_Scopes (Gen_Unit) then
         Error_Msg_NE ("instantiation of & within itself", N, Gen_Unit);
         Restore_Env;
         goto Leave;

      elsif Contains_Instance_Of (Gen_Unit, Current_Scope, Gen_Id) then
         Error_Msg_Node_2 := Current_Scope;
         Error_Msg_NE
           ("circular instantiation: & instantiated in &!", N, Gen_Unit);
         Circularity_Detected := True;
         Restore_Env;
         goto Leave;

      else
         Mutate_Ekind (Inst_Id, E_Package);
         Set_Scope (Inst_Id, Current_Scope);

         --  If the context of the instance is subject to SPARK_Mode "off" or
         --  the annotation is altogether missing, set the global flag which
         --  signals Analyze_Pragma to ignore all SPARK_Mode pragmas within
         --  the instance.

         if SPARK_Mode /= On then
            Ignore_SPARK_Mode_Pragmas_In_Instance := True;

            --  Mark the instance spec in case the body is instantiated at a
            --  later pass. This preserves the original context in effect for
            --  the body.

            Set_Ignore_SPARK_Mode_Pragmas (Act_Decl_Id);
         end if;

         Gen_Decl := Unit_Declaration_Node (Gen_Unit);
         Gen_Spec := Specification (Gen_Decl);

         --  Initialize renamings map, for error checking, and the list that
         --  holds private entities whose views have changed between generic
         --  definition and instantiation. If this is the instance created to
         --  validate an actual package, the instantiation environment is that
         --  of the enclosing instance.

         Create_Instantiation_Source (N, Gen_Unit, S_Adjustment);

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

         Renamings :=
           Analyze_Associations
             (I_Node  => N,
              Formals => Generic_Formal_Declarations (Act_Tree),
              F_Copy  => Generic_Formal_Declarations (Gen_Decl));

         Vis_Prims_List := Check_Hidden_Primitives (Renamings);

         Set_Instance_Env (Gen_Unit, Act_Decl_Id);
         Set_Defining_Unit_Name (Act_Spec, Act_Decl_Name);
         Set_Is_Generic_Instance (Act_Decl_Id);
         Set_Generic_Parent (Act_Spec, Gen_Unit);

         --  References to the generic in its own declaration or its body are
         --  references to the instance. Add a renaming declaration for the
         --  generic unit itself. This declaration, as well as the renaming
         --  declarations for the generic formals, must remain private to the
         --  unit: the formals, because this is the language semantics, and
         --  the unit because its use is an artifact of the implementation.

         Unit_Renaming :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc, Chars (Gen_Unit)),
             Name               => New_Occurrence_Of (Act_Decl_Id, Loc));

         Append (Unit_Renaming, Renamings);

         --  The renaming declarations are the first local declarations of the
         --  new unit.

         if Is_Non_Empty_List (Visible_Declarations (Act_Spec)) then
            Insert_List_Before
              (First (Visible_Declarations (Act_Spec)), Renamings);
         else
            Set_Visible_Declarations (Act_Spec, Renamings);
         end if;

         Act_Decl := Make_Package_Declaration (Loc, Specification => Act_Spec);

         --  Propagate the aspect specifications from the package declaration
         --  template to the instantiated version of the package declaration.

         if Has_Aspects (Act_Tree) then
            Set_Aspect_Specifications (Act_Decl,
              New_Copy_List_Tree (Aspect_Specifications (Act_Tree)));
         end if;

         --  The generic may have a generated Default_Storage_Pool aspect,
         --  set at the point of generic declaration. If the instance has
         --  that aspect, it overrides the one inherited from the generic.

         if Has_Aspects (Gen_Spec) then
            if No (Aspect_Specifications (N)) then
               Set_Aspect_Specifications (N,
                 (New_Copy_List_Tree
                   (Aspect_Specifications (Gen_Spec))));

            else
               declare
                  Inherited_Aspects : constant List_Id :=
                                        New_Copy_List_Tree
                                          (Aspect_Specifications (Gen_Spec));

                  ASN1         : Node_Id;
                  ASN2         : Node_Id;
                  Pool_Present : Boolean := False;

               begin
                  ASN1 := First (Aspect_Specifications (N));
                  while Present (ASN1) loop
                     if Chars (Identifier (ASN1)) =
                          Name_Default_Storage_Pool
                     then
                        Pool_Present := True;
                        exit;
                     end if;

                     Next (ASN1);
                  end loop;

                  if Pool_Present then

                     --  If generic carries a default storage pool, remove it
                     --  in favor of the instance one.

                     ASN2 := First (Inherited_Aspects);
                     while Present (ASN2) loop
                        if Chars (Identifier (ASN2)) =
                             Name_Default_Storage_Pool
                        then
                           Remove (ASN2);
                           exit;
                        end if;

                        Next (ASN2);
                     end loop;
                  end if;

                  Prepend_List_To
                    (Aspect_Specifications (N), Inherited_Aspects);
               end;
            end if;
         end if;

         --  Save the instantiation node for a subsequent instantiation of the
         --  body if there is one and it needs to be instantiated here.

         --  We instantiate the body only if we are generating code, or if we
         --  are generating cross-reference information, or for GNATprove use.

         declare
            Enclosing_Body_Present : Boolean := False;
            --  If the generic unit is not a compilation unit, then a body may
            --  be present in its parent even if none is required. We create a
            --  tentative pending instantiation for the body, which will be
            --  discarded if none is actually present.

            Scop : Entity_Id;

         begin
            if Scope (Gen_Unit) /= Standard_Standard
              and then not Is_Child_Unit (Gen_Unit)
            then
               Scop := Scope (Gen_Unit);
               while Present (Scop) and then Scop /= Standard_Standard loop
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

            --  If front-end inlining is enabled or there are any subprograms
            --  marked with Inline_Always, and this is a unit for which code
            --  will be generated, we instantiate the body at once.

            --  This is done if the instance is not the main unit, and if the
            --  generic is not a child unit of another generic, to avoid scope
            --  problems and the reinstallation of parent instances.

            if Expander_Active
              and then (not Is_Child_Unit (Gen_Unit)
                         or else not Is_Generic_Unit (Scope (Gen_Unit)))
              and then Might_Inline_Subp (Gen_Unit)
              and then not Is_Actual_Pack
            then
               if not Back_End_Inlining
                 and then (Front_End_Inlining or else Has_Inline_Always)
                 and then (Is_In_Main_Unit (N)
                            or else In_Main_Context (Current_Scope))
                 and then Nkind (Parent (N)) /= N_Compilation_Unit
               then
                  Inline_Now := True;

               --  In configurable_run_time mode we force the inlining of
               --  predefined subprograms marked Inline_Always, to minimize
               --  the use of the run-time library.

               elsif In_Predefined_Unit (Gen_Decl)
                 and then Configurable_Run_Time_Mode
                 and then Nkind (Parent (N)) /= N_Compilation_Unit
               then
                  Inline_Now := True;
               end if;

               --  If the current scope is itself an instance within a child
               --  unit, there will be duplications in the scope stack, and the
               --  unstacking mechanism in Inline_Instance_Body will fail.
               --  This loses some rare cases of optimization.

               if Is_Generic_Instance (Current_Scope) then
                  declare
                     Curr_Unit : constant Entity_Id :=
                                   Cunit_Entity (Current_Sem_Unit);
                  begin
                     if Curr_Unit /= Current_Scope
                       and then Is_Child_Unit (Curr_Unit)
                     then
                        Inline_Now := False;
                     end if;
                  end;
               end if;
            end if;

            Needs_Body :=
              (Unit_Requires_Body (Gen_Unit)
                or else Enclosing_Body_Present
                or else Present (Corresponding_Body (Gen_Decl)))
               and then Needs_Body_Instantiated (Gen_Unit)
               and then not Is_Actual_Pack
               and then not Inline_Now
               and then (Operating_Mode = Generate_Code
                          or else (Operating_Mode = Check_Semantics
                                    and then GNATprove_Mode));

            --  If front-end inlining is enabled or there are any subprograms
            --  marked with Inline_Always, do not instantiate body when within
            --  a generic context.

            if not Back_End_Inlining
              and then (Front_End_Inlining or else Has_Inline_Always)
              and then not Expander_Active
            then
               Needs_Body := False;
            end if;

            --  If the current context is generic, and the package being
            --  instantiated is declared within a formal package, there is no
            --  body to instantiate until the enclosing generic is instantiated
            --  and there is an actual for the formal package. If the formal
            --  package has parameters, we build a regular package instance for
            --  it, that precedes the original formal package declaration.

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
                                Nkind (Next (Decl)) =
                                                N_Formal_Package_Declaration)
                  then
                     Needs_Body := False;
                  end if;
               end;
            end if;
         end;

         --  For RCI unit calling stubs, we omit the instance body if the
         --  instance is the RCI library unit itself.

         --  However there is a special case for nested instances: in this case
         --  we do generate the instance body, as it might be required, e.g.
         --  because it provides stream attributes for some type used in the
         --  profile of a remote subprogram. This is consistent with 12.3(12),
         --  which indicates that the instance body occurs at the place of the
         --  instantiation, and thus is part of the RCI declaration, which is
         --  present on all client partitions (this is E.2.3(18)).

         --  Note that AI12-0002 may make it illegal at some point to have
         --  stream attributes defined in an RCI unit, in which case this
         --  special case will become unnecessary. In the meantime, there
         --  is known application code in production that depends on this
         --  being possible, so we definitely cannot eliminate the body in
         --  the case of nested instances for the time being.

         --  When we generate a nested instance body, calling stubs for any
         --  relevant subprogram will be inserted immediately after the
         --  subprogram declarations, and will take precedence over the
         --  subsequent (original) body. (The stub and original body will be
         --  complete homographs, but this is permitted in an instance).
         --  (Could we do better and remove the original body???)

         if Distribution_Stub_Mode = Generate_Caller_Stub_Body
           and then Comes_From_Source (N)
           and then Nkind (Parent (N)) = N_Compilation_Unit
         then
            Needs_Body := False;
         end if;

         --  If the context requires a full instantiation, set things up for
         --  subsequent construction of the body.

         if Needs_Body then
            declare
               Fin_Scop, S : Entity_Id;

            begin
               Check_Forward_Instantiation (Gen_Decl);

               Fin_Scop := Empty;

               --  For a package instantiation that is not a compilation unit,
               --  indicate that cleanup actions of the innermost enclosing
               --  scope for which they are generated should be delayed until
               --  after the package body is instantiated.

               if Nkind (N) = N_Package_Instantiation
                 and then not Is_Compilation_Unit (Act_Decl_Id)
               then
                  S := Current_Scope;

                  while S /= Standard_Standard loop
                     --  Cleanup actions are not generated within generic units
                     --  or in the formal part of generic units.

                     if not Expander_Active then
                        exit;

                     --  For package scopes, cleanup actions are generated only
                     --  for compilation units, for spec and body separately.

                     elsif Ekind (S) = E_Package then
                        if Is_Compilation_Unit (S) then
                           if In_Package_Body (S) then
                              Fin_Scop := Body_Entity (S);
                           else
                              Fin_Scop := S;
                           end if;

                           Set_Delay_Cleanups (Fin_Scop);
                           exit;

                        else
                           S := Scope (S);
                        end if;

                     --  Cleanup actions are generated for all dynamic scopes

                     else
                        Fin_Scop := S;
                        Set_Delay_Cleanups (Fin_Scop);
                        exit;
                     end if;
                  end loop;
               end if;

               Add_Pending_Instantiation (N, Act_Decl, Fin_Scop);
            end;
         end if;

         Set_Categorization_From_Pragmas (Act_Decl);

         if Parent_Installed then
            Hide_Current_Scope;
         end if;

         Set_Instance_Spec (N, Act_Decl);

         --  If not a compilation unit, insert the package declaration before
         --  the original instantiation node.

         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Mark_Rewrite_Insertion (Act_Decl);
            Insert_Before (N, Act_Decl);

            if Has_Aspects (N) then
               Analyze_Aspect_Specifications (N, Act_Decl_Id);

               --  The pragma created for a Default_Storage_Pool aspect must
               --  appear ahead of the declarations in the instance spec.
               --  Analysis has placed it after the instance node, so remove
               --  it and reinsert it properly now.

               declare
                  ASN : constant Node_Id := First (Aspect_Specifications (N));
                  A_Name : constant Name_Id := Chars (Identifier (ASN));
                  Decl : Node_Id;

               begin
                  if A_Name = Name_Default_Storage_Pool then
                     if No (Visible_Declarations (Act_Spec)) then
                        Set_Visible_Declarations (Act_Spec, New_List);
                     end if;

                     Decl := Next (N);
                     while Present (Decl) loop
                        if Nkind (Decl) = N_Pragma then
                           Remove (Decl);
                           Prepend (Decl, Visible_Declarations (Act_Spec));
                           exit;
                        end if;

                        Next (Decl);
                     end loop;
                  end if;
               end;
            end if;

            Analyze (Act_Decl);

         --  For an instantiation that is a compilation unit, place
         --  declaration on current node so context is complete for analysis
         --  (including nested instantiations). If this is the main unit,
         --  the declaration eventually replaces the instantiation node.
         --  If the instance body is created later, it replaces the
         --  instance node, and the declaration is attached to it
         --  (see Build_Instance_Compilation_Unit_Nodes).

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

            Set_Unit (Parent (N), Act_Decl);
            Set_Parent_Spec (Act_Decl, Parent_Spec (N));
            Set_Package_Instantiation (Act_Decl_Id, N);

            --  Process aspect specifications of the instance node, if any, to
            --  take into account categorization pragmas before analyzing the
            --  instance.

            Analyze_Aspect_Specifications (N, Act_Decl_Id);

            Analyze (Act_Decl);
            Set_Unit (Parent (N), N);
            Set_Body_Required (Parent (N), False);

            --  We never need elaboration checks on instantiations, since by
            --  definition, the body instantiation is elaborated at the same
            --  time as the spec instantiation.

            if Legacy_Elaboration_Checks then
               Set_Kill_Elaboration_Checks       (Act_Decl_Id);
               Set_Suppress_Elaboration_Warnings (Act_Decl_Id);
            end if;
         end if;

         if Legacy_Elaboration_Checks then
            Check_Elab_Instantiation (N);
         end if;

         --  Save the scenario for later examination by the ABE Processing
         --  phase.

         Record_Elaboration_Scenario (N);

         --  The instantiation results in a guaranteed ABE

         if Is_Known_Guaranteed_ABE (N) and then Needs_Body then
            --  Do not instantiate the corresponding body because gigi cannot
            --  handle certain types of premature instantiations.

            Remove_Dead_Instance (N);

            --  Create completing bodies for all subprogram declarations since
            --  their real bodies will not be instantiated.

            Provide_Completing_Bodies (Instance_Spec (N));
         end if;

         Check_Hidden_Child_Unit (N, Gen_Unit, Act_Decl_Id);

         Set_First_Private_Entity (Defining_Unit_Name (Unit_Renaming),
           First_Private_Entity (Act_Decl_Id));

         --  If the instantiation needs a body, the unit will be turned into
         --  a package body and receive its own elaboration entity. Otherwise,
         --  the nature of the unit is now a package declaration.

         --  Note that the below rewriting means that Act_Decl, which has been
         --  analyzed and expanded, will be re-expanded as the rewritten N.

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

         Restore_Hidden_Primitives (Vis_Prims_List);
         Restore_Private_Views (Act_Decl_Id);

         Inherit_Context (Gen_Decl, N);

         if Parent_Installed then
            Remove_Parent;
         end if;

         Restore_Env;
         Env_Installed := False;
      end if;

      Validate_Categorization_Dependency (N, Act_Decl_Id);

      --  There used to be a check here to prevent instantiations in local
      --  contexts if the No_Local_Allocators restriction was active. This
      --  check was removed by a binding interpretation in AI-95-00130/07,
      --  but we retain the code for documentation purposes.

      --  if Ekind (Act_Decl_Id) /= E_Void
      --    and then not Is_Library_Level_Entity (Act_Decl_Id)
      --  then
      --     Check_Restriction (No_Local_Allocators, N);
      --  end if;

      if Inline_Now then
         Inline_Instance_Body (N, Gen_Unit, Act_Decl);
      end if;

      --  Check that if N is an instantiation of System.Dim_Float_IO or
      --  System.Dim_Integer_IO, the formal type has a dimension system.

      if Nkind (N) = N_Package_Instantiation
        and then Is_Dim_IO_Package_Instantiation (N)
      then
         declare
            Assoc : constant Node_Id := First (Generic_Associations (N));
         begin
            if not Has_Dimension_System
                     (Etype (Explicit_Generic_Actual_Parameter (Assoc)))
            then
               Error_Msg_N ("type with a dimension system expected", Assoc);
            end if;
         end;
      end if;

   <<Leave>>
      if Nkind (Parent (N)) /= N_Compilation_Unit then
         Analyze_Aspect_Specifications (N, Act_Decl_Id);
      end if;

      Ignore_SPARK_Mode_Pragmas_In_Instance := Saved_ISMP;
      Restore_Ghost_Region (Saved_GM, Saved_IGR);
      Restore_SPARK_Mode   (Saved_SM, Saved_SMP);
      Style_Check := Saved_Style_Check;

   exception
      when Instantiation_Error =>
         if Parent_Installed then
            Remove_Parent;
         end if;

         if Env_Installed then
            Restore_Env;
         end if;

         Ignore_SPARK_Mode_Pragmas_In_Instance := Saved_ISMP;
         Restore_Ghost_Region (Saved_GM, Saved_IGR);
         Restore_SPARK_Mode   (Saved_SM, Saved_SMP);
         Style_Check := Saved_Style_Check;
   end Analyze_Package_Instantiation;

   --------------------------
   -- Inline_Instance_Body --
   --------------------------

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Inline_Instance_Body
     (N        : Node_Id;
      Gen_Unit : Entity_Id;
      Act_Decl : Node_Id)
   is
      Config_Attrs : constant Config_Switches_Type := Save_Config_Switches;

      Curr_Comp : constant Node_Id   := Cunit (Current_Sem_Unit);
      Curr_Unit : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);
      Gen_Comp  : constant Entity_Id :=
                    Cunit_Entity (Get_Source_Unit (Gen_Unit));

      Scope_Stack_Depth : constant Pos :=
                            Scope_Stack.Last - Scope_Stack.First + 1;

      Inner_Scopes : array (1 .. Scope_Stack_Depth) of Entity_Id;
      Instances    : array (1 .. Scope_Stack_Depth) of Entity_Id;
      Use_Clauses  : array (1 .. Scope_Stack_Depth) of Node_Id;

      Curr_Scope  : Entity_Id := Empty;
      List        : Elist_Id := No_Elist; -- init to avoid warning
      N_Instances : Nat := 0;
      Num_Inner   : Nat := 0;
      Num_Scopes  : Nat := 0;
      Removed     : Boolean := False;
      S           : Entity_Id;
      Vis         : Boolean;

   begin
      --  Case of generic unit defined in another unit. We must remove the
      --  complete context of the current unit to install that of the generic.

      if Gen_Comp /= Cunit_Entity (Current_Sem_Unit) then

         --  Loop through enclosing scopes until we reach a generic instance,
         --  package body, or subprogram.

         S := Current_Scope;
         while Present (S) and then S /= Standard_Standard loop

            --  Save use clauses from enclosing scopes into Use_Clauses

            loop
               Num_Scopes := Num_Scopes + 1;

               Use_Clauses (Num_Scopes) :=
                 (Scope_Stack.Table
                    (Scope_Stack.Last - Num_Scopes + 1).First_Use_Clause);
               End_Use_Clauses (Use_Clauses (Num_Scopes));

               exit when Scope_Stack.Last - Num_Scopes + 1 = Scope_Stack.First
                 or else Scope_Stack.Table
                           (Scope_Stack.Last - Num_Scopes).Entity = Scope (S);
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
         --  spec as well. If an enclosing scope is an instance body, the
         --  context has already been removed, but the entities in the body
         --  must be made invisible as well.

         S := Current_Scope;
         while Present (S) and then S /= Standard_Standard loop
            if Is_Generic_Instance (S)
              and then (In_Package_Body (S)
                         or else Ekind (S) in E_Procedure | E_Function)
            then
               --  We still have to remove the entities of the enclosing
               --  instance from direct visibility.

               declare
                  E : Entity_Id;
               begin
                  E := First_Entity (S);
                  while Present (E) loop
                     Set_Is_Immediately_Visible (E, False);
                     Next_Entity (E);
                  end loop;
               end;

               exit;
            end if;

            if S = Curr_Unit
              or else (Ekind (Curr_Unit) = E_Package_Body
                        and then S = Spec_Entity (Curr_Unit))
              or else (Ekind (Curr_Unit) = E_Subprogram_Body
                        and then S = Corresponding_Spec
                                       (Unit_Declaration_Node (Curr_Unit)))
            then
               Removed := True;

               --  Remove entities in current scopes from visibility, so that
               --  instance body is compiled in a clean environment.

               List := Save_Scope_Stack (Handle_Use => False);

               if Is_Child_Unit (S) then

                  --  Remove child unit from stack, as well as inner scopes.
                  --  Removing the context of a child unit removes parent units
                  --  as well.

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

         Push_Scope (Standard_Standard);
         Scope_Stack.Table (Scope_Stack.Last).Is_Active_Stack_Base := True;

         --  The inlined package body is analyzed with the configuration state
         --  of the context prior to the scope manipulations performed above.

         --  ??? shouldn't this also use the warning state of the context prior
         --  to the scope manipulations?

         Instantiate_Package_Body
           (Body_Info =>
             ((Inst_Node                => N,
               Act_Decl                 => Act_Decl,
               Fin_Scop                 => Empty,
               Config_Switches          => Config_Attrs,
               Current_Sem_Unit         => Current_Sem_Unit,
               Expander_Status          => Expander_Active,
               Local_Suppress_Stack_Top => Local_Suppress_Stack_Top,
               Scope_Suppress           => Scope_Suppress,
               Warnings                 => Save_Warnings)),
            Inlined_Body => True);

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
            Install_Context (Curr_Comp, Chain => False);

            if Present (Curr_Scope)
              and then Is_Child_Unit (Curr_Scope)
            then
               Push_Scope (Curr_Scope);
               Set_Is_Immediately_Visible (Curr_Scope);

               --  Finally, restore inner scopes as well

               for J in reverse 1 .. Num_Inner loop
                  Push_Scope (Inner_Scopes (J));
               end loop;
            end if;

            Restore_Scope_Stack (List, Handle_Use => False);

            if Present (Curr_Scope)
              and then
                (In_Private_Part (Curr_Scope)
                  or else In_Package_Body (Curr_Scope))
            then
               --  Install private declaration of ancestor units, which are
               --  currently available. Restore_Scope_Stack and Install_Context
               --  only install the visible part of parents.

               declare
                  Par : Entity_Id;
               begin
                  Par := Scope (Curr_Scope);
                  while Present (Par) and then Par /= Standard_Standard loop
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

         if Is_Child_Unit (Curr_Unit) and then Removed then
            for J in reverse 1 .. Num_Inner + 1 loop
               Scope_Stack.Table (Scope_Stack.Last - J + 1).First_Use_Clause :=
                 Use_Clauses (J);
               Install_Use_Clauses (Use_Clauses (J));
            end loop;

         else
            for J in reverse 1 .. Num_Scopes loop
               Scope_Stack.Table (Scope_Stack.Last - J + 1).First_Use_Clause :=
                 Use_Clauses (J);
               Install_Use_Clauses (Use_Clauses (J));
            end loop;
         end if;

         --  Restore status of instances. If one of them is a body, make its
         --  local entities visible again.

         declare
            E    : Entity_Id;
            Inst : Entity_Id;

         begin
            for J in 1 .. N_Instances loop
               Inst := Instances (J);
               Set_Is_Generic_Instance (Inst, True);

               if In_Package_Body (Inst)
                 or else Ekind (S) in E_Procedure | E_Function
               then
                  E := First_Entity (Instances (J));
                  while Present (E) loop
                     Set_Is_Immediately_Visible (E);
                     Next_Entity (E);
                  end loop;
               end if;
            end loop;
         end;

      --  If generic unit is in current unit, current context is correct. Note
      --  that the context is guaranteed to carry the correct SPARK_Mode as no
      --  enclosing scopes were removed.

      else
         Instantiate_Package_Body
           (Body_Info =>
             ((Inst_Node                => N,
               Act_Decl                 => Act_Decl,
               Fin_Scop                 => Empty,
               Config_Switches          => Save_Config_Switches,
               Current_Sem_Unit         => Current_Sem_Unit,
               Expander_Status          => Expander_Active,
               Local_Suppress_Stack_Top => Local_Suppress_Stack_Top,
               Scope_Suppress           => Scope_Suppress,
               Warnings                 => Save_Warnings)),
            Inlined_Body => True);
      end if;
   end Inline_Instance_Body;

   -------------------------------------
   -- Analyze_Procedure_Instantiation --
   -------------------------------------

   procedure Analyze_Procedure_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Procedure);
   end Analyze_Procedure_Instantiation;

   -----------------------------------
   -- Need_Subprogram_Instance_Body --
   -----------------------------------

   function Need_Subprogram_Instance_Body
     (N    : Node_Id;
      Subp : Entity_Id) return Boolean
   is
      function Is_Inlined_Or_Child_Of_Inlined (E : Entity_Id) return Boolean;
      --  Return True if E is an inlined subprogram, an inlined renaming or a
      --  subprogram nested in an inlined subprogram. The inlining machinery
      --  totally disregards nested subprograms since it considers that they
      --  will always be compiled if the parent is (see Inline.Is_Nested).

      ------------------------------------
      -- Is_Inlined_Or_Child_Of_Inlined --
      ------------------------------------

      function Is_Inlined_Or_Child_Of_Inlined (E : Entity_Id) return Boolean is
         Scop : Entity_Id;

      begin
         if Is_Inlined (E) or else Is_Inlined (Alias (E)) then
            return True;
         end if;

         Scop := Scope (E);
         while Scop /= Standard_Standard loop
            if Is_Subprogram (Scop) and then Is_Inlined (Scop) then
               return True;
            end if;

            Scop := Scope (Scop);
         end loop;

         return False;
      end Is_Inlined_Or_Child_Of_Inlined;

   --  Start of processing for Need_Subprogram_Instance_Body

   begin
      --  Must be in the main unit or inlined (or child of inlined)

      if (Is_In_Main_Unit (N) or else Is_Inlined_Or_Child_Of_Inlined (Subp))

        --  Must be generating code or analyzing code in GNATprove mode

        and then (Operating_Mode = Generate_Code
                   or else (Operating_Mode = Check_Semantics
                             and then GNATprove_Mode))

        --  The body is needed when generating code (full expansion) and in
        --  in GNATprove mode (special expansion) for formal verification of
        --  the body itself.

        and then (Expander_Active or GNATprove_Mode)

        --  No point in inlining if ABE is inevitable

        and then not Is_Known_Guaranteed_ABE (N)

        --  Or if subprogram is eliminated

        and then not Is_Eliminated (Subp)
      then
         Add_Pending_Instantiation (N, Unit_Declaration_Node (Subp));
         return True;

      --  Here if not inlined, or we ignore the inlining

      else
         return False;
      end if;
   end Need_Subprogram_Instance_Body;

   --------------------------------------
   -- Analyze_Subprogram_Instantiation --
   --------------------------------------

   --  WARNING: This routine manages Ghost and SPARK regions. Return statements
   --  must be replaced by gotos which jump to the end of the routine in order
   --  to restore the Ghost and SPARK modes.

   procedure Analyze_Subprogram_Instantiation
     (N : Node_Id;
      K : Entity_Kind)
   is
      Errs    : constant Nat        := Serious_Errors_Detected;
      Gen_Id  : constant Node_Id    := Name (N);
      Inst_Id : constant Entity_Id  := Defining_Entity (N);
      Anon_Id : constant Entity_Id  :=
                  Make_Defining_Identifier (Sloc (Inst_Id),
                    Chars => New_External_Name (Chars (Inst_Id), 'R'));
      Loc     : constant Source_Ptr := Sloc (N);

      Act_Decl_Id : Entity_Id := Empty; -- init to avoid warning
      Act_Decl    : Node_Id;
      Act_Spec    : Node_Id;
      Act_Tree    : Node_Id;

      Env_Installed    : Boolean := False;
      Gen_Unit         : Entity_Id;
      Gen_Decl         : Node_Id;
      Pack_Id          : Entity_Id;
      Parent_Installed : Boolean := False;

      Renamings : List_Id;
      --  The list of declarations that link formals and actuals of the
      --  instance. These are subtype declarations for formal types, and
      --  renaming declarations for other formals. The subprogram declaration
      --  for the instance is then appended to the list, and the last item on
      --  the list is the renaming declaration for the instance.

      procedure Analyze_Instance_And_Renamings;
      --  The instance must be analyzed in a context that includes the mappings
      --  of generic parameters into actuals. We create a package declaration
      --  for this purpose, and a subprogram with an internal name within the
      --  package. The subprogram instance is simply an alias for the internal
      --  subprogram, declared in the current scope.

      procedure Build_Subprogram_Renaming;
      --  If the subprogram is recursive, there are occurrences of the name of
      --  the generic within the body, which must resolve to the current
      --  instance. We add a renaming declaration after the declaration, which
      --  is available in the instance body, as well as in the analysis of
      --  aspects that appear in the generic. This renaming declaration is
      --  inserted after the instance declaration which it renames.

      ------------------------------------
      -- Analyze_Instance_And_Renamings --
      ------------------------------------

      procedure Analyze_Instance_And_Renamings is
         Def_Ent   : constant Entity_Id := Defining_Entity (N);
         Pack_Decl : Node_Id;

      begin
         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  For the case of a compilation unit, the container package has
            --  the same name as the instantiation, to insure that the binder
            --  calls the elaboration procedure with the right name. Copy the
            --  entity of the instance, which may have compilation level flags
            --  (e.g. Is_Child_Unit) set.

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

         Pack_Decl :=
           Make_Package_Declaration (Loc,
             Specification => Make_Package_Specification (Loc,
               Defining_Unit_Name   => Pack_Id,
               Visible_Declarations => Renamings,
               End_Label            => Empty));

         Set_Instance_Spec (N, Pack_Decl);
         Set_Is_Generic_Instance (Pack_Id);
         Set_Debug_Info_Needed (Pack_Id);

         --  Case of not a compilation unit

         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Mark_Rewrite_Insertion (Pack_Decl);
            Insert_Before (N, Pack_Decl);
            Set_Has_Completion (Pack_Id);

         --  Case of an instantiation that is a compilation unit

         --  Place declaration on current node so context is complete for
         --  analysis (including nested instantiations), and for use in a
         --  context_clause (see Analyze_With_Clause).

         else
            Set_Unit (Parent (N), Pack_Decl);
            Set_Parent_Spec (Pack_Decl, Parent_Spec (N));
         end if;

         Analyze (Pack_Decl);
         Check_Formal_Packages (Pack_Id);

         --  Body of the enclosing package is supplied when instantiating the
         --  subprogram body, after semantic analysis is completed.

         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  Remove package itself from visibility, so it does not
            --  conflict with subprogram.

            Set_Name_Entity_Id (Chars (Pack_Id), Homonym (Pack_Id));

            --  Set name and scope of internal subprogram so that the proper
            --  external name will be generated. The proper scope is the scope
            --  of the wrapper package. We need to generate debugging info for
            --  the internal subprogram, so set flag accordingly.

            Set_Chars (Anon_Id, Chars (Defining_Entity (N)));
            Set_Scope (Anon_Id, Scope (Pack_Id));

            --  Mark wrapper package as referenced, to avoid spurious warnings
            --  if the instantiation appears in various with_ clauses of
            --  subunits of the main unit.

            Set_Referenced (Pack_Id);
         end if;

         Set_Is_Generic_Instance (Anon_Id);
         Set_Debug_Info_Needed   (Anon_Id);
         Act_Decl_Id := New_Copy (Anon_Id);

         Set_Parent (Act_Decl_Id, Parent (Anon_Id));
         Set_Chars  (Act_Decl_Id, Chars (Defining_Entity (N)));
         Set_Sloc   (Act_Decl_Id, Sloc (Defining_Entity (N)));

         --  Subprogram instance comes from source only if generic does

         Preserve_Comes_From_Source (Act_Decl_Id, Gen_Unit);

         --  If the instance is a child unit, mark the Id accordingly. Mark
         --  the anonymous entity as well, which is the real subprogram and
         --  which is used when the instance appears in a context clause.
         --  Similarly, propagate the Is_Eliminated flag to handle properly
         --  nested eliminated subprograms.

         Set_Is_Child_Unit (Act_Decl_Id, Is_Child_Unit (Defining_Entity (N)));
         Set_Is_Child_Unit (Anon_Id, Is_Child_Unit (Defining_Entity (N)));
         New_Overloaded_Entity (Act_Decl_Id);
         Check_Eliminated  (Act_Decl_Id);
         Set_Is_Eliminated (Anon_Id, Is_Eliminated (Act_Decl_Id));

         if Nkind (Parent (N)) = N_Compilation_Unit then

            --  In compilation unit case, kill elaboration checks on the
            --  instantiation, since they are never needed - the body is
            --  instantiated at the same point as the spec.

            if Legacy_Elaboration_Checks then
               Set_Kill_Elaboration_Checks       (Act_Decl_Id);
               Set_Suppress_Elaboration_Warnings (Act_Decl_Id);
            end if;

            Set_Is_Compilation_Unit (Anon_Id);
            Set_Cunit_Entity (Current_Sem_Unit, Pack_Id);
         end if;

         --  The instance is not a freezing point for the new subprogram.
         --  The anonymous subprogram may have a freeze node, created for
         --  some delayed aspects. This freeze node must not be inherited
         --  by the visible subprogram entity.

         Set_Is_Frozen   (Act_Decl_Id, False);
         Set_Freeze_Node (Act_Decl_Id, Empty);

         if Nkind (Defining_Entity (N)) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Act_Decl_Id);
         end if;

         Set_Alias (Act_Decl_Id, Anon_Id);
         Set_Has_Completion (Act_Decl_Id);
         Set_Related_Instance (Pack_Id, Act_Decl_Id);

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Set_Body_Required (Parent (N), False);
         end if;
      end Analyze_Instance_And_Renamings;

      -------------------------------
      -- Build_Subprogram_Renaming --
      -------------------------------

      procedure Build_Subprogram_Renaming is
         Renaming_Decl : Node_Id;
         Unit_Renaming : Node_Id;

      begin
         Unit_Renaming :=
           Make_Subprogram_Renaming_Declaration (Loc,
             Specification =>
               Copy_Generic_Node
                 (Specification (Original_Node (Gen_Decl)),
                  Empty,
                  Instantiating => True),
             Name          => New_Occurrence_Of (Anon_Id, Loc));

         --  The generic may be a child unit. The renaming needs an identifier
         --  with the proper name.

         Set_Defining_Unit_Name (Specification (Unit_Renaming),
            Make_Defining_Identifier (Loc, Chars (Gen_Unit)));

         --  If there is a formal subprogram with the same name as the unit
         --  itself, do not add this renaming declaration, to prevent
         --  ambiguities when there is a call with that name in the body.

         Renaming_Decl := First (Renamings);
         while Present (Renaming_Decl) loop
            if Nkind (Renaming_Decl) = N_Subprogram_Renaming_Declaration
              and then
                Chars (Defining_Entity (Renaming_Decl)) = Chars (Gen_Unit)
            then
               exit;
            end if;

            Next (Renaming_Decl);
         end loop;

         if No (Renaming_Decl) then
            Append (Unit_Renaming, Renamings);
         end if;
      end Build_Subprogram_Renaming;

      --  Local variables

      Saved_GM   : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR  : constant Node_Id         := Ignored_Ghost_Region;
      Saved_ISMP : constant Boolean         :=
                     Ignore_SPARK_Mode_Pragmas_In_Instance;
      Saved_SM   : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP  : constant Node_Id         := SPARK_Mode_Pragma;
      --  Save the Ghost and SPARK mode-related data to restore on exit

      Vis_Prims_List : Elist_Id := No_Elist;
      --  List of primitives made temporarily visible in the instantiation
      --  to match the visibility of the formal type

   --  Start of processing for Analyze_Subprogram_Instantiation

   begin
      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => N,
         Checks   => True,
         Level    => True,
         Modes    => True,
         Warnings => True);

      --  Very first thing: check for special Text_IO unit in case we are
      --  instantiating one of the children of [[Wide_]Wide_]Text_IO. Of course
      --  such an instantiation is bogus (these are packages, not subprograms),
      --  but we get a better error message if we do this.

      Check_Text_IO_Special_Unit (Gen_Id);

      --  Make node global for error reporting

      Instantiation_Node := N;

      --  For package instantiations we turn off style checks, because they
      --  will have been emitted in the generic. For subprogram instantiations
      --  we want to apply at least the check on overriding indicators so we
      --  do not modify the style check status.

      --  The renaming declarations for the actuals do not come from source and
      --  will not generate spurious warnings.

      Preanalyze_Actuals (N);

      Init_Env;
      Env_Installed := True;
      Check_Generic_Child_Unit (Gen_Id, Parent_Installed);
      Gen_Unit := Entity (Gen_Id);

      --  A subprogram instantiation is Ghost when it is subject to pragma
      --  Ghost or the generic template is Ghost. Set the mode now to ensure
      --  that any nodes generated during analysis and expansion are marked as
      --  Ghost.

      Mark_And_Set_Ghost_Instantiation (N, Gen_Unit);

      Generate_Reference (Gen_Unit, Gen_Id);

      if Nkind (Gen_Id) = N_Identifier
        and then Chars (Gen_Unit) = Chars (Defining_Entity (N))
      then
         Error_Msg_NE
           ("& is hidden within declaration of instance", Gen_Id, Gen_Unit);
      end if;

      if Etype (Gen_Unit) = Any_Type then
         Restore_Env;
         goto Leave;
      end if;

      --  Verify that it is a generic subprogram of the right kind, and that
      --  it does not lead to a circular instantiation.

      if K = E_Procedure and then Ekind (Gen_Unit) /= E_Generic_Procedure then
         Error_Msg_NE
           ("& is not the name of a generic procedure", Gen_Id, Gen_Unit);

      elsif K = E_Function and then Ekind (Gen_Unit) /= E_Generic_Function then
         Error_Msg_NE
           ("& is not the name of a generic function", Gen_Id, Gen_Unit);

      elsif In_Open_Scopes (Gen_Unit) then
         Error_Msg_NE ("instantiation of & within itself", N, Gen_Unit);

      else
         Mutate_Ekind (Inst_Id, K);
         Set_Scope (Inst_Id, Current_Scope);

         Set_Entity (Gen_Id, Gen_Unit);

         if In_Extended_Main_Source_Unit (N) then
            Set_Is_Instantiated (Gen_Unit);
            Generate_Reference (Gen_Unit, N);
         end if;

         --  If renaming, get original unit

         if Present (Renamed_Entity (Gen_Unit))
           and then Is_Generic_Subprogram (Renamed_Entity (Gen_Unit))
         then
            Gen_Unit := Renamed_Entity (Gen_Unit);
            Set_Is_Instantiated (Gen_Unit);
            Generate_Reference  (Gen_Unit, N);
         end if;

         if Contains_Instance_Of (Gen_Unit, Current_Scope, Gen_Id) then
            Error_Msg_Node_2 := Current_Scope;
            Error_Msg_NE
              ("circular instantiation: & instantiated in &!", N, Gen_Unit);
            Circularity_Detected := True;
            Restore_Hidden_Primitives (Vis_Prims_List);
            goto Leave;
         end if;

         Gen_Decl := Unit_Declaration_Node (Gen_Unit);

         --  Initialize renamings map, for error checking

         Generic_Renamings.Set_Last (0);
         Generic_Renamings_HTable.Reset;

         Create_Instantiation_Source (N, Gen_Unit, S_Adjustment);

         --  Copy original generic tree, to produce text for instantiation

         Act_Tree :=
           Copy_Generic_Node
             (Original_Node (Gen_Decl), Empty, Instantiating => True);

         --  Inherit overriding indicator from instance node

         Act_Spec := Specification (Act_Tree);
         Set_Must_Override     (Act_Spec, Must_Override (N));
         Set_Must_Not_Override (Act_Spec, Must_Not_Override (N));

         Renamings :=
           Analyze_Associations
             (I_Node  => N,
              Formals => Generic_Formal_Declarations (Act_Tree),
              F_Copy  => Generic_Formal_Declarations (Gen_Decl));

         Vis_Prims_List := Check_Hidden_Primitives (Renamings);

         --  The subprogram itself cannot contain a nested instance, so the
         --  current parent is left empty.

         Set_Instance_Env (Gen_Unit, Empty);

         --  Build the subprogram declaration, which does not appear in the
         --  generic template, and give it a sloc consistent with that of the
         --  template.

         Set_Defining_Unit_Name (Act_Spec, Anon_Id);
         Set_Generic_Parent (Act_Spec, Gen_Unit);
         Act_Decl :=
           Make_Subprogram_Declaration (Sloc (Act_Spec),
             Specification => Act_Spec);

         --  The aspects have been copied previously, but they have to be
         --  linked explicitly to the new subprogram declaration. Explicit
         --  pre/postconditions on the instance are analyzed below, in a
         --  separate step.

         Move_Aspects (Act_Tree, To => Act_Decl);
         Set_Categorization_From_Pragmas (Act_Decl);

         if Parent_Installed then
            Hide_Current_Scope;
         end if;

         Append (Act_Decl, Renamings);

         --  Contract-related source pragmas that follow a generic subprogram
         --  must be instantiated explicitly because they are not part of the
         --  subprogram template.

         Instantiate_Subprogram_Contract
           (Original_Node (Gen_Decl), Renamings);

         Build_Subprogram_Renaming;

         --  If the context of the instance is subject to SPARK_Mode "off" or
         --  the annotation is altogether missing, set the global flag which
         --  signals Analyze_Pragma to ignore all SPARK_Mode pragmas within
         --  the instance. This should be done prior to analyzing the instance.

         if SPARK_Mode /= On then
            Ignore_SPARK_Mode_Pragmas_In_Instance := True;
         end if;

         --  If the context of an instance is not subject to SPARK_Mode "off",
         --  and the generic spec is subject to an explicit SPARK_Mode pragma,
         --  the latter should be the one applicable to the instance.

         if not Ignore_SPARK_Mode_Pragmas_In_Instance
           and then Saved_SM /= Off
           and then Present (SPARK_Pragma (Gen_Unit))
         then
            Set_SPARK_Mode (Gen_Unit);
         end if;

         --  Need to mark Anon_Id intrinsic before calling
         --  Analyze_Instance_And_Renamings because this flag may be propagated
         --  to other nodes.

         if Is_Intrinsic_Subprogram (Gen_Unit) then
            Set_Is_Intrinsic_Subprogram (Anon_Id);
            Set_Interface_Name (Anon_Id, Interface_Name (Gen_Unit));
         end if;

         Analyze_Instance_And_Renamings;

         --  Restore SPARK_Mode from the context after analysis of the package
         --  declaration, so that the SPARK_Mode on the generic spec does not
         --  apply to the pending instance for the instance body.

         if not Ignore_SPARK_Mode_Pragmas_In_Instance
           and then Saved_SM /= Off
           and then Present (SPARK_Pragma (Gen_Unit))
         then
            Restore_SPARK_Mode (Saved_SM, Saved_SMP);
         end if;

         --  If the generic is marked Import (Intrinsic), then so is the
         --  instance; this indicates that there is no body to instantiate.
         --  We also copy the interface name in case this is handled by the
         --  back-end and deal with an instance of unchecked conversion.

         if Is_Intrinsic_Subprogram (Gen_Unit) then
            Set_Is_Intrinsic_Subprogram (Act_Decl_Id);
            Set_Interface_Name (Act_Decl_Id, Interface_Name (Gen_Unit));

            if Chars (Gen_Unit) = Name_Unchecked_Conversion then
               Validate_Unchecked_Conversion (N, Act_Decl_Id);
            end if;
         end if;

         --  Inherit convention from generic unit. Intrinsic convention, as for
         --  an instance of unchecked conversion, is not inherited because an
         --  explicit Ada instance has been created.

         if Has_Convention_Pragma (Gen_Unit)
           and then Convention (Gen_Unit) /= Convention_Intrinsic
         then
            Set_Convention (Act_Decl_Id, Convention (Gen_Unit));
            Set_Is_Exported (Act_Decl_Id, Is_Exported (Gen_Unit));
         end if;

         Generate_Definition (Act_Decl_Id);

         --  Inherit all inlining-related flags which apply to the generic in
         --  the subprogram and its declaration.

         Set_Is_Inlined (Act_Decl_Id, Is_Inlined (Gen_Unit));
         Set_Is_Inlined (Anon_Id,     Is_Inlined (Gen_Unit));

         Set_Has_Pragma_Inline (Act_Decl_Id, Has_Pragma_Inline (Gen_Unit));
         Set_Has_Pragma_Inline (Anon_Id,     Has_Pragma_Inline (Gen_Unit));

         Set_Has_Pragma_Inline_Always
           (Act_Decl_Id, Has_Pragma_Inline_Always (Gen_Unit));
         Set_Has_Pragma_Inline_Always
           (Anon_Id,     Has_Pragma_Inline_Always (Gen_Unit));

         Set_Has_Pragma_No_Inline
           (Act_Decl_Id, Has_Pragma_No_Inline (Gen_Unit));
         Set_Has_Pragma_No_Inline
           (Anon_Id,     Has_Pragma_No_Inline (Gen_Unit));

         --  Propagate No_Raise if pragma applied to generic unit. This must
         --  be done explicitly because the pragma does not appear in generic
         --  declarations (unlike the aspect).

         if No_Raise (Gen_Unit) then
            Set_No_Raise (Act_Decl_Id);
            Set_No_Raise (Anon_Id);
         end if;

         --  Likewise for No_Return

         if No_Return (Gen_Unit) then
            Set_No_Return (Act_Decl_Id);
            Set_No_Return (Anon_Id);
         end if;

         --  Mark both the instance spec and the anonymous package in case the
         --  body is instantiated at a later pass. This preserves the original
         --  context in effect for the body.

         if SPARK_Mode /= On then
            Set_Ignore_SPARK_Mode_Pragmas (Act_Decl_Id);
            Set_Ignore_SPARK_Mode_Pragmas (Anon_Id);
         end if;

         if Legacy_Elaboration_Checks
           and then not Is_Intrinsic_Subprogram (Gen_Unit)
         then
            Check_Elab_Instantiation (N);
         end if;

         --  Save the scenario for later examination by the ABE Processing
         --  phase.

         Record_Elaboration_Scenario (N);

         --  The instantiation results in a guaranteed ABE. Create a completing
         --  body for the subprogram declaration because the real body will not
         --  be instantiated.

         if Is_Known_Guaranteed_ABE (N) then
            Provide_Completing_Bodies (Instance_Spec (N));
         end if;

         if Is_Dispatching_Operation (Act_Decl_Id)
           and then Ada_Version >= Ada_2005
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
                     Error_Msg_NE
                       ("access parameter& is controlling,", N, Formal);
                     Error_Msg_NE
                       ("\corresponding parameter of & must be explicitly "
                        & "null-excluding", N, Gen_Id);
                  end if;

                  Next_Formal (Formal);
               end loop;
            end;
         end if;

         Check_Hidden_Child_Unit (N, Gen_Unit, Act_Decl_Id);

         Validate_Categorization_Dependency (N, Act_Decl_Id);

         if not Is_Intrinsic_Subprogram (Act_Decl_Id) then
            Inherit_Context (Gen_Decl, N);

            Restore_Private_Views (Pack_Id, False);

            --  If the context requires a full instantiation, mark node for
            --  subsequent construction of the body.

            if Need_Subprogram_Instance_Body (N, Act_Decl_Id) then
               Check_Forward_Instantiation (Gen_Decl);

            --  The wrapper package is always delayed, because it does not
            --  constitute a freeze point, but to insure that the freeze node
            --  is placed properly, it is created directly when instantiating
            --  the body (otherwise the freeze node might appear to early for
            --  nested instantiations).

            elsif Nkind (Parent (N)) = N_Compilation_Unit then
               Rewrite (N, Unit (Parent (N)));
               Set_Unit (Parent (N), N);
            end if;

         --  Replace instance node for library-level instantiations of
         --  intrinsic subprograms.

         elsif Nkind (Parent (N)) = N_Compilation_Unit then
            Rewrite (N, Unit (Parent (N)));
            Set_Unit (Parent (N), N);
         end if;

         if Parent_Installed then
            Remove_Parent;
         end if;

         Restore_Hidden_Primitives (Vis_Prims_List);
         Restore_Env;
         Env_Installed := False;
         Generic_Renamings.Set_Last (0);
         Generic_Renamings_HTable.Reset;
      end if;

   <<Leave>>
      --  Analyze aspects in declaration if no errors appear in the instance.

      if Has_Aspects (N) and then Serious_Errors_Detected = Errs then
         Analyze_Aspect_Specifications (N, Act_Decl_Id);
      end if;

      Ignore_SPARK_Mode_Pragmas_In_Instance := Saved_ISMP;
      Restore_Ghost_Region (Saved_GM, Saved_IGR);
      Restore_SPARK_Mode   (Saved_SM, Saved_SMP);

   exception
      when Instantiation_Error =>
         if Parent_Installed then
            Remove_Parent;
         end if;

         if Env_Installed then
            Restore_Env;
         end if;

         Ignore_SPARK_Mode_Pragmas_In_Instance := Saved_ISMP;
         Restore_Ghost_Region (Saved_GM, Saved_IGR);
         Restore_SPARK_Mode   (Saved_SM, Saved_SMP);
   end Analyze_Subprogram_Instantiation;

   ---------------------------
   -- Get_Associated_Entity --
   ---------------------------

   function Get_Associated_Entity (Id : Entity_Id) return Entity_Id is
      Assoc : Entity_Id;

   begin
      Assoc := Associated_Entity (Id);

      if Present (Assoc) then
         while Present (Associated_Entity (Assoc)) loop
            Assoc := Associated_Entity (Assoc);
         end loop;
      end if;

      return Assoc;
   end Get_Associated_Entity;

   -------------------------
   -- Get_Associated_Node --
   -------------------------

   function Get_Associated_Node (N : Node_Id) return Node_Id is
      Assoc : Node_Id;

   begin
      Assoc := Associated_Node (N);

      if Nkind (Assoc) /= Nkind (N) then
         return Assoc;

      elsif Nkind (Assoc) in N_Aggregate | N_Extension_Aggregate then
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

         --  Follow an additional link in case the final node was rewritten.
         --  This can only happen with nested generic units.

         if (Nkind (Assoc) = N_Identifier or else Nkind (Assoc) in N_Op)
           and then Present (Associated_Node (Assoc))
           and then Nkind (Associated_Node (Assoc)) in N_Function_Call
                                                     | N_Explicit_Dereference
                                                     | N_Integer_Literal
                                                     | N_Real_Literal
                                                     | N_String_Literal
         then
            Assoc := Associated_Node (Assoc);
         end if;

         --  An additional special case: an unconstrained type in an object
         --  declaration may have been rewritten as a local subtype constrained
         --  by the expression in the declaration. We need to recover the
         --  original entity, which may be global.

         if Present (Original_Node (Assoc))
           and then Nkind (Parent (N)) = N_Object_Declaration
         then
            Assoc := Original_Node (Assoc);
         end if;

         return Assoc;
      end if;
   end Get_Associated_Node;

   -----------------------------------
   -- Build_Subprogram_Decl_Wrapper --
   -----------------------------------

   function Build_Subprogram_Decl_Wrapper
     (Formal_Subp : Entity_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Current_Scope);
      Ret_Type  : constant Entity_Id  := Get_Instance_Of (Etype (Formal_Subp));
      Decl      : Node_Id;
      Subp      : Entity_Id;
      Parm_Spec : Node_Id;
      Profile   : List_Id := New_List;
      Spec      : Node_Id;
      Form_F    : Entity_Id;
      New_F     : Entity_Id;

   begin
      Subp := Make_Defining_Identifier (Loc, Chars (Formal_Subp));
      Mutate_Ekind (Subp, Ekind (Formal_Subp));
      Set_Is_Generic_Actual_Subprogram (Subp);

      Profile := Parameter_Specifications (
                   New_Copy_Tree
                    (Specification (Unit_Declaration_Node (Formal_Subp))));

      Form_F := First_Formal (Formal_Subp);
      Parm_Spec := First (Profile);

      --  Create new entities for the formals. Reset entities so that
      --  parameter types are properly resolved when wrapper declaration
      --  is analyzed.

      while Present (Parm_Spec) loop
         New_F := Make_Defining_Identifier (Loc, Chars (Form_F));
         Set_Defining_Identifier (Parm_Spec, New_F);
         Set_Entity (Parameter_Type (Parm_Spec), Empty);
         Next (Parm_Spec);
         Next_Formal (Form_F);
      end loop;

      if Ret_Type = Standard_Void_Type then
         Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Subp,
             Parameter_Specifications => Profile);
      else
         Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Subp,
             Parameter_Specifications => Profile,
             Result_Definition        => New_Occurrence_Of (Ret_Type, Loc));
      end if;

      Decl :=
        Make_Subprogram_Declaration (Loc, Specification => Spec);

      return Decl;
   end Build_Subprogram_Decl_Wrapper;

   -----------------------------------
   -- Build_Subprogram_Body_Wrapper --
   -----------------------------------

   function Build_Subprogram_Body_Wrapper
     (Formal_Subp : Entity_Id;
      Actual_Name : Node_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Current_Scope);
      Ret_Type  : constant Entity_Id  := Get_Instance_Of (Etype (Formal_Subp));
      Spec_Node : constant Node_Id :=
        Specification
          (Build_Subprogram_Decl_Wrapper (Formal_Subp));
      Act       : Node_Id;
      Actuals   : List_Id;
      Body_Node : Node_Id;
      Stmt      : Node_Id;
   begin
      Actuals := New_List;
      Act := First (Parameter_Specifications (Spec_Node));

      while Present (Act) loop
         Append_To (Actuals,
            Make_Identifier (Loc, Chars (Defining_Identifier (Act))));
         Next (Act);
      end loop;

      if Ret_Type = Standard_Void_Type then
         Stmt := Make_Procedure_Call_Statement (Loc,
          Name                   => Actual_Name,
          Parameter_Associations => Actuals);

      else
         Stmt := Make_Simple_Return_Statement (Loc,
            Expression =>
              Make_Function_Call (Loc,
                Name                   => Actual_Name,
                Parameter_Associations => Actuals));
      end if;

      Body_Node := Make_Subprogram_Body (Loc,
        Specification => Spec_Node,
        Declarations  => New_List,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc,
            Statements => New_List (Stmt)));

      return Body_Node;
   end Build_Subprogram_Body_Wrapper;

   -------------------------------
   -- Build_Subprogram_Wrappers --
   -------------------------------

   procedure Build_Subprogram_Wrappers
     (Match, Analyzed_Formal : Node_Id; Renamings : List_Id)
   is
      function Adjust_Aspect_Sloc (N : Node_Id) return Traverse_Result;
      --  Adjust Sloc so that errors will be reported on the instance rather
      --  than the generic.

      ------------------------
      -- Adjust_Aspect_Sloc --
      ------------------------

      function Adjust_Aspect_Sloc (N : Node_Id) return Traverse_Result is
      begin
         Adjust_Instantiation_Sloc (N, S_Adjustment);
         return OK;
      end Adjust_Aspect_Sloc;

      procedure Adjust_Aspect_Slocs is new
        Traverse_Proc (Adjust_Aspect_Sloc);

      Formal : constant Entity_Id :=
        Defining_Unit_Name (Specification (Analyzed_Formal));
      Aspect_Spec : Node_Id;
      Decl_Node   : Node_Id;
      Actual_Name : Node_Id;

   --  Start of processing for Build_Subprogram_Wrappers

   begin
      --  Create declaration for wrapper subprogram.
      --  The actual can be overloaded, in which case it will be
      --  resolved when the call in the wrapper body is analyzed.
      --  We attach the possible interpretations of the actual to
      --  the name to be used in the call in the wrapper body.

      if Is_Entity_Name (Match) then
         Actual_Name := New_Occurrence_Of (Entity (Match), Sloc (Match));

         if Is_Overloaded (Match) then
            Save_Interps (Match, Actual_Name);
         end if;

      else
         --  Use renaming declaration created when analyzing actual.
         --  This may be incomplete if there are several formal
         --  subprograms whose actual is an attribute ???

         declare
            Renaming_Decl : constant Node_Id := Last (Renamings);

         begin
            Actual_Name := New_Occurrence_Of
                  (Defining_Entity (Renaming_Decl), Sloc (Match));
            Set_Etype (Actual_Name, Get_Instance_Of (Etype (Formal)));
         end;
      end if;

      Decl_Node := Build_Subprogram_Decl_Wrapper (Formal);

      --  Transfer aspect specifications from formal subprogram to wrapper

      Set_Aspect_Specifications (Decl_Node,
        New_Copy_List_Tree (Aspect_Specifications (Analyzed_Formal)));

      Aspect_Spec := First (Aspect_Specifications (Decl_Node));
      while Present (Aspect_Spec) loop
         Adjust_Aspect_Slocs (Aspect_Spec);
         Set_Analyzed (Aspect_Spec, False);
         Next (Aspect_Spec);
      end loop;

      Append_To (Renamings, Decl_Node);

      --  Create corresponding body, and append it to association list
      --  that appears at the head of the declarations in the instance.
      --  The subprogram may be called in the analysis of subsequent
      --  actuals.

      Append_To (Renamings,
        Build_Subprogram_Body_Wrapper (Formal, Actual_Name));
   end Build_Subprogram_Wrappers;

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
      --  A new compilation unit node is built for the instance declaration.
      --  It relocates the auxiliary declaration node from the compilation unit
      --  where the instance appeared, so that declarations that originally
      --  followed the instance will be attached to the spec compilation unit.

      Decl_Cunit :=
        Make_Compilation_Unit (Sloc (N),
          Context_Items  => Empty_List,
          Unit           => Act_Decl,
          Aux_Decls_Node => Relocate_Node (Aux_Decls_Node (Parent (N))));

      Set_Parent_Spec (Act_Decl, Parent_Spec (N));

      --  The new compilation unit is linked to its body, but both share the
      --  same file, so we do not set Body_Required on the new unit so as not
      --  to create a spurious dependency on a non-existent body in the ali.
      --  This simplifies CodePeer unit traversal.

      --  We use the original instantiation compilation unit as the resulting
      --  compilation unit of the instance, since this is the main unit.

      Rewrite (N, Act_Body);

      Body_Cunit := Parent (N);

      --  The two compilation unit nodes are linked by the Library_Unit field

      Set_Library_Unit (Decl_Cunit, Body_Cunit);
      Set_Library_Unit (Body_Cunit, Decl_Cunit);

      --  Preserve the private nature of the package if needed

      Set_Private_Present (Decl_Cunit, Private_Present (Body_Cunit));

      --  If the instance is not the main unit, its context, categorization
      --  and elaboration entity are not relevant to the compilation.

      if Body_Cunit /= Cunit (Main_Unit) then
         Make_Instance_Unit (Body_Cunit, In_Main => False);
         return;
      end if;

      --  The context clause items on the instantiation, which are now attached
      --  to the body compilation unit (since the body overwrote the original
      --  instantiation node), semantically belong on the spec, so copy them
      --  there. It's harmless to leave them on the body as well. In fact one
      --  could argue that they belong in both places.

      Citem := First (Context_Items (Body_Cunit));
      while Present (Citem) loop
         Append (New_Copy (Citem), Context_Items (Decl_Cunit));
         Next (Citem);
      end loop;

      --  Propagate categorization flags on packages, so that they appear in
      --  the ali file for the spec of the unit.

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

      Make_Instance_Unit (Body_Cunit, In_Main => True);
      Main_Unit_Entity := New_Main;
      Set_Cunit_Entity (Main_Unit, Main_Unit_Entity);

      --  Build elaboration entity, since the instance may certainly generate
      --  elaboration code requiring a flag for protection.

      Build_Elaboration_Entity (Decl_Cunit, New_Main);
   end Build_Instance_Compilation_Unit_Nodes;

   --------------------------------
   -- Check_Abbreviated_Instance --
   --------------------------------

   procedure Check_Abbreviated_Instance
     (N                : Node_Id;
      Parent_Installed : in out Boolean)
   is
      Inst_Node : Node_Id;

   begin
      if Nkind (N) = N_Package_Specification
        and then Is_Abbreviated_Instance (Defining_Entity (N))
      then
         Inst_Node := Get_Unit_Instantiation_Node (Defining_Entity (N));
         Check_Generic_Child_Unit (Name (Inst_Node), Parent_Installed);
      end if;
   end Check_Abbreviated_Instance;

   -----------------------------
   -- Check_Access_Definition --
   -----------------------------

   procedure Check_Access_Definition (N : Node_Id) is
   begin
      pragma Assert
        (Ada_Version >= Ada_2005 and then Present (Access_Definition (N)));
      null;
   end Check_Access_Definition;

   -----------------------------------
   -- Check_Formal_Package_Instance --
   -----------------------------------

   --  If the formal has specific parameters, they must match those of the
   --  actual. Both of them are instances, and the renaming declarations for
   --  their formal parameters appear in the same order in both. The analyzed
   --  formal has been analyzed in the context of the current instance.

   procedure Check_Formal_Package_Instance
     (Formal_Pack : Entity_Id;
      Actual_Pack : Entity_Id)
   is
      E1      : Entity_Id := First_Entity (Actual_Pack);
      E2      : Entity_Id := First_Entity (Formal_Pack);
      Prev_E1 : Entity_Id;

      Expr1 : Node_Id;
      Expr2 : Node_Id;

      procedure Check_Mismatch (B : Boolean);
      --  Common error routine for mismatch between the parameters of the
      --  actual instance and those of the formal package.

      function Is_Defaulted (Param : Entity_Id) return Boolean;
      --  If the formal package has partly box-initialized formals, skip
      --  conformance check for these formals. Previously the code assumed
      --  that box initialization for a formal package applied to all its
      --  formal parameters.

      function Same_Instantiated_Constant (E1, E2 : Entity_Id) return Boolean;
      --  The formal may come from a nested formal package, and the actual may
      --  have been constant-folded. To determine whether the two denote the
      --  same entity we may have to traverse several definitions to recover
      --  the ultimate entity that they refer to.

      function Same_Instantiated_Function (E1, E2 : Entity_Id) return Boolean;
      --  The formal and the actual must be identical, but if both are
      --  given by attributes they end up renaming different generated bodies,
      --  and we must verify that the attributes themselves match.

      function Same_Instantiated_Variable (E1, E2 : Entity_Id) return Boolean;
      --  Similarly, if the formal comes from a nested formal package, the
      --  actual may designate the formal through multiple renamings, which
      --  have to be followed to determine the original variable in question.

      --------------------
      -- Check_Mismatch --
      --------------------

      procedure Check_Mismatch (B : Boolean) is
         --  A Formal_Type_Declaration for a derived private type is rewritten
         --  as a private extension decl. (see Analyze_Formal_Derived_Type),
         --  which is why we examine the original node.

         Kind : constant Node_Kind := Nkind (Original_Node (Parent (E2)));

      begin
         if Kind = N_Formal_Type_Declaration then
            return;

         elsif Kind in N_Formal_Object_Declaration
                     | N_Formal_Package_Declaration
                     | N_Formal_Subprogram_Declaration
         then
            null;

         --  Ada 2012: If both formal and actual are incomplete types they
         --  are conformant.

         elsif Is_Incomplete_Type (E1) and then Is_Incomplete_Type (E2) then
            null;

         elsif B then
            Error_Msg_NE
              ("actual for & in actual instance does not match formal",
               Parent (Actual_Pack), E1);
         end if;
      end Check_Mismatch;

      ------------------
      -- Is_Defaulted --
      ------------------

      function Is_Defaulted (Param : Entity_Id) return Boolean is
         Assoc : Node_Id;

      begin
         Assoc :=
            First (Generic_Associations (Parent
              (Associated_Formal_Package (Actual_Pack))));

         while Present (Assoc) loop
            if Nkind (Assoc) = N_Others_Choice then
               return True;

            elsif Nkind (Assoc) = N_Generic_Association
              and then Chars (Selector_Name (Assoc)) = Chars (Param)
            then
               return Box_Present (Assoc);
            end if;

            Next (Assoc);
         end loop;

         return False;
      end Is_Defaulted;

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
               if Entity (Constant_Value (Ent)) = E1 then
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
      -- Same_Instantiated_Function --
      --------------------------------

      function Same_Instantiated_Function
        (E1, E2 : Entity_Id) return Boolean
      is
         U1, U2 : Node_Id;
      begin
         if Alias (E1) = Alias (E2) then
            return True;

         elsif Present (Alias (E2)) then
            U1 := Original_Node (Unit_Declaration_Node (E1));
            U2 := Original_Node (Unit_Declaration_Node (Alias (E2)));

            return Nkind (U1) = N_Subprogram_Renaming_Declaration
              and then Nkind (Name (U1)) = N_Attribute_Reference

              and then Nkind (U2) = N_Subprogram_Renaming_Declaration
              and then Nkind (Name (U2)) = N_Attribute_Reference

              and then
                Attribute_Name (Name (U1)) = Attribute_Name (Name (U2));
         else
            return False;
         end if;
      end Same_Instantiated_Function;

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
      Prev_E1 := E1;
      while Present (E1) and then Present (E2) loop
         exit when Ekind (E1) = E_Package
           and then Renamed_Entity (E1) = Renamed_Entity (Actual_Pack);

         --  If the formal is the renaming of the formal package, this
         --  is the end of its formal part, which may occur before the
         --  end of the formal part in the actual in the presence of
         --  defaulted parameters in the formal package.

         exit when Nkind (Parent (E2)) = N_Package_Renaming_Declaration
           and then Renamed_Entity (E2) = Scope (E2);

         --  The analysis of the actual may generate additional internal
         --  entities. If the formal is defaulted, there is no corresponding
         --  analysis and the internal entities must be skipped, until we
         --  find corresponding entities again.

         if Comes_From_Source (E2)
           and then not Comes_From_Source (E1)
           and then Chars (E1) /= Chars (E2)
         then
            while Present (E1) and then Chars (E1) /= Chars (E2) loop
               Next_Entity (E1);
            end loop;
         end if;

         if No (E1) then
            return;

         --  Entities may be declared without full declaration, such as
         --  itypes and predefined operators (concatenation for arrays, eg).
         --  Skip it and keep the formal entity to find a later match for it.

         elsif No (Parent (E2)) and then Ekind (E1) /= Ekind (E2) then
            E1 := Prev_E1;
            goto Next_E;

         --  If the formal entity comes from a formal declaration, it was
         --  defaulted in the formal package, and no check is needed on it.

         elsif Nkind (Original_Node (Parent (E2))) in
                 N_Formal_Object_Declaration | N_Formal_Type_Declaration
         then
            --  If the formal is a tagged type the corresponding class-wide
            --  type has been generated as well, and it must be skipped.
            --  Likewise, for a formal discrete type, the base type has been
            --  generated as well (see Analyze_Formal_Discrete_Type).

            if Is_Type (E2)
              and then (Is_Tagged_Type (E2) or else Is_Enumeration_Type (E2))
            then
               Next_Entity (E2);
            end if;

            goto Next_E;

         --  Ditto for defaulted formal subprograms.

         elsif Is_Overloadable (E1)
           and then Nkind (Unit_Declaration_Node (E2)) in
                      N_Formal_Subprogram_Declaration
         then
            goto Next_E;

         elsif Is_Defaulted (E1) then
            goto Next_E;

         elsif Is_Type (E1) then

            --  Subtypes must statically match. E1, E2 are the local entities
            --  that are subtypes of the actuals. Itypes generated for other
            --  parameters need not be checked, the check will be performed
            --  on the parameters themselves.

            --  If E2 is a formal type declaration, it is a defaulted parameter
            --  and needs no checking.

            if not Is_Itype (E1) and then not Is_Itype (E2) then
               Check_Mismatch
                 (not Is_Type (E2)
                   or else Etype (E1) /= Etype (E2)
                   or else not Subtypes_Statically_Match (E1, E2));
            end if;

         elsif Ekind (E1) = E_Constant then

            --  IN parameters must denote the same static value, or the same
            --  constant, or the literal null.

            Expr1 := Expression (Parent (E1));

            if Ekind (E2) /= E_Constant then
               Check_Mismatch (True);
               goto Next_E;
            else
               Expr2 := Expression (Parent (E2));
            end if;

            if Is_OK_Static_Expression (Expr1) then
               if not Is_OK_Static_Expression (Expr2) then
                  Check_Mismatch (True);

               elsif Is_Discrete_Type (Etype (E1)) then
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
              and then Same_Instantiated_Constant
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
                or else (Present (Renamed_Entity (E2))
                          and then Renamed_Entity (E1) /=
                                     Renamed_Entity (E2)));

         elsif Is_Overloadable (E1) then
            --  Verify that the actual subprograms match. Note that actuals
            --  that are attributes are rewritten as subprograms. If the
            --  subprogram in the formal package is defaulted, no check is
            --  needed. Note that this can only happen in Ada 2005 when the
            --  formal package can be partially parameterized.

            if Nkind (Unit_Declaration_Node (E1)) =
                                           N_Subprogram_Renaming_Declaration
              and then From_Default (Unit_Declaration_Node (E1))
            then
               null;

            --  If the formal package has an "others"  box association that
            --  covers this formal, there is no need for a check either.

            elsif Nkind (Unit_Declaration_Node (E2)) in
                    N_Formal_Subprogram_Declaration
              and then Box_Present (Unit_Declaration_Node (E2))
            then
               null;

            --  No check needed if subprogram is a defaulted null procedure

            elsif No (Alias (E2))
              and then Ekind (E2) = E_Procedure
              and then
                Null_Present (Specification (Unit_Declaration_Node (E2)))
            then
               null;

            --  Otherwise the actual in the formal and the actual in the
            --  instantiation of the formal must match, up to renamings.

            else
               Check_Mismatch
                 (Ekind (E2) /= Ekind (E1)
                    or else not Same_Instantiated_Function (E1, E2));
            end if;

         else
            raise Program_Error;
         end if;

         <<Next_E>>
            Prev_E1 := E1;
            Next_Entity (E1);
            Next_Entity (E2);
      end loop;
   end Check_Formal_Package_Instance;

   -------------------------------
   --  Check_Fixed_Point_Warning --
   -------------------------------

   procedure Check_Fixed_Point_Warning
     (Gen_Assocs : Associations.Gen_Assocs_Rec;
      Renamings : List_Id)
   is
      use Associations;
   begin
      for Type_Index in Gen_Assocs.Assocs'Range loop
         declare
            Assoc : Assoc_Rec renames Gen_Assocs.Assocs (Type_Index);
         begin
            if Nkind (Assoc.An_Formal) = N_Formal_Type_Declaration
              and then Is_Fixed_Point_Type (Defining_Entity (Assoc.An_Formal))
              and then Assoc.Actual.Kind = Name_Exp
            then
               declare
                  Typ : constant Entity_Id := Entity (Assoc.Actual.Name_Exp);
                  pragma Assert (Is_Fixed_Point_Type (Typ));

                  Prims  : constant Elist_Id  :=
                    Collect_Primitive_Operations (Typ);
                  Elem   : Elmt_Id := First_Elmt (Prims);
                  Formal : Node_Id;
                  Op     : Entity_Id;
               begin
                  --  Locate primitive operations of the type that are
                  --  arithmetic operations.

                  while Present (Elem) loop
                     if Nkind (Node (Elem)) = N_Defining_Operator_Symbol then

                        --  Check whether the generic unit has a formal
                        --  subprogram of the same name. This does not check
                        --  types but is good enough to justify a warning.

                        Op := Alias (Node (Elem));

                        for Op_Index in Type_Index + 1 ..
                          Gen_Assocs.Assocs'Last
                        loop
                           Formal := Gen_Assocs.Assocs (Op_Index).Un_Formal;

                           if Nkind (Formal) =
                               N_Formal_Concrete_Subprogram_Declaration
                             and then Chars (Defining_Entity (Formal)) =
                                        Chars (Node (Elem))
                           then
                              goto OK;

                           elsif Nkind (Formal) = N_Formal_Package_Declaration
                           then
                              declare
                                 Assoc : Node_Id;
                                 Ent   : Entity_Id;

                              begin
                                 --  Locate corresponding actual, and check
                                 --  whether it includes a fixed-point type.

                                 Assoc := First (Renamings);
                                 while Present (Assoc) loop
                                    exit when
                                      Nkind (Assoc) =
                                          N_Package_Renaming_Declaration
                                        and then
                                          Chars (Defining_Unit_Name (Assoc)) =
                                          Chars (Defining_Identifier (Formal));

                                    Next (Assoc);
                                 end loop;

                                 if Present (Assoc) then
                                    --  If the formal package declares a
                                    --  fixed-point type, and the user-defined
                                    --  operator is derived from a generic
                                    --  instance package, the fixed-point type
                                    --  does not use the corresponding
                                    --  predefined op.

                                    Ent :=
                                      First_Entity (Entity (Name (Assoc)));
                                    while Present (Ent) loop
                                       if Is_Fixed_Point_Type (Ent)
                                         and then Present (Op)
                                         and then
                                           Is_Generic_Instance (Scope (Op))
                                       then
                                          goto OK;
                                       end if;

                                       Next_Entity (Ent);
                                    end loop;
                                 end if;
                              end;
                           end if;
                        end loop;

                        Error_Msg_Sloc := Sloc (Node (Elem));
                        Error_Msg_NE
                          ("?instance uses predefined, not primitive, " &
                           "operator&#",
                           Assoc.Actual.Name_Exp, Node (Elem));
                        <<OK>> null;
                     end if;

                     Next_Elmt (Elem);
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Check_Fixed_Point_Warning;

   ---------------------------
   -- Check_Formal_Packages --
   ---------------------------

   procedure Check_Formal_Packages (P_Id : Entity_Id) is
      E           : Entity_Id;
      Formal_P    : Entity_Id;
      Formal_Decl : Node_Id;

   begin
      --  Iterate through the declarations in the instance, looking for package
      --  renaming declarations that denote instances of formal packages, until
      --  we find the renaming of the current package itself. The declaration
      --  of a formal package that requires conformance checking is followed by
      --  an internal entity that is the abbreviated instance.

      E := First_Entity (P_Id);
      while Present (E) loop
         if Ekind (E) = E_Package then
            exit when Renamed_Entity (E) = P_Id;

            if Nkind (Parent (E)) = N_Package_Renaming_Declaration then
               Formal_Decl := Parent (Associated_Formal_Package (E));

               if Requires_Conformance_Checking (Formal_Decl) then
                  Formal_P := Next_Entity (E);

                  --  If the instance is within an enclosing instance body
                  --  there is no need to verify the legality of current formal
                  --  packages because they were legal in the generic body.
                  --  This optimization may be applicable elsewhere, and it
                  --  also removes spurious errors that may arise with
                  --  on-the-fly inlining and confusion between private and
                  --  full views.

                  if not In_Instance_Body then
                     Check_Formal_Package_Instance (Formal_P, E);
                  end if;

                  --  Restore the visibility of formals of the formal instance
                  --  that are not defaulted, and are hidden within the current
                  --  generic. These formals may be visible within an enclosing
                  --  generic.

                  declare
                     Elmt : Elmt_Id;
                  begin
                     Elmt := First_Elmt (Hidden_In_Formal_Instance (Formal_P));
                     while Present (Elmt) loop
                        Set_Is_Hidden (Node (Elmt), False);
                        Next_Elmt (Elmt);
                     end loop;
                  end;

                  --  After checking, remove the internal validating package.
                  --  It is only needed for semantic checks, and as it may
                  --  contain generic formal declarations it should not reach
                  --  gigi.

                  Remove (Unit_Declaration_Node (Formal_P));
               end if;
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
      --  the package body, and before the generic body.

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

   --  The visibility of the actuals may be different between the point of
   --  generic instantiation and the instantiation of the body.

   procedure Check_Generic_Actuals
     (Instance      : Entity_Id;
      Is_Formal_Box : Boolean)
   is
      Gen_Id : constant Entity_Id :=
        (if Is_Generic_Unit (Instance) then
           Instance
         elsif Is_Wrapper_Package (Instance) then
           Generic_Parent
             (Specification
               (Unit_Declaration_Node (Related_Instance (Instance))))
         else
           Generic_Parent (Package_Specification (Instance)));
      --  The generic unit

      Parent_Scope : constant Entity_Id := Scope (Gen_Id);
      --  The enclosing scope of the generic unit

      procedure Check_Actual_Type (Typ : Entity_Id);
      --  If the type of the actual is a private type declared in the enclosing
      --  scope of the generic, either directly or through packages nested in
      --  bodies, but not a derived type of a private type declared elsewhere,
      --  then the body of the generic sees the full view of the type because
      --  it has to appear in the package body. If the type is private now then
      --  exchange views to restore the proper visibility in the instance.

      -----------------------
      -- Check_Actual_Type --
      -----------------------

      procedure Check_Actual_Type (Typ : Entity_Id) is
         Btyp : constant Entity_Id := Base_Type (Typ);

         function Scope_Within_Body_Or_Same
           (Inner : Entity_Id;
            Outer : Entity_Id) return Boolean;
         --  Determine whether scope Inner is within the body of scope Outer
         --  or is Outer itself.

         -------------------------------
         -- Scope_Within_Body_Or_Same --
         -------------------------------

         function Scope_Within_Body_Or_Same
           (Inner : Entity_Id;
            Outer : Entity_Id) return Boolean
         is
            Curr : Entity_Id := Inner;

         begin
            while Curr /= Standard_Standard loop
               if Curr = Outer then
                  return True;

               elsif Is_Package_Body_Entity (Curr) then
                  Curr := Scope (Curr);

               else
                  exit;
               end if;
            end loop;

            return False;
         end Scope_Within_Body_Or_Same;

      --  Start of processing for Check_Actual_Type

      begin
         --  The exchange is only needed if the generic is defined
         --  within a package which is not a common ancestor of the
         --  scope of the instance, and is not already in scope.

         if Is_Private_Type (Btyp)
           and then not Has_Private_Ancestor (Btyp)
           and then Ekind (Parent_Scope) in E_Package | E_Generic_Package
           and then Scope_Within_Body_Or_Same (Parent_Scope, Scope (Btyp))
           and then Parent_Scope /= Scope (Instance)
           and then not Is_Child_Unit (Gen_Id)
         then
            Switch_View (Btyp);

            --  If the type of the entity is a subtype, it may also have
            --  to be made visible, together with the base type of its
            --  full view, after exchange.

            if Is_Private_Type (Typ) then
               Switch_View (Typ);
               Switch_View (Base_Type (Typ));
            end if;
         end if;
      end Check_Actual_Type;

      --  Local variables

      Astype : Entity_Id;
      E      : Entity_Id;
      Formal : Node_Id;

   --  Start of processing for Check_Generic_Actuals

   begin
      E := First_Entity (Instance);
      while Present (E) loop
         if Is_Type (E)
           and then Nkind (Parent (E)) = N_Subtype_Declaration
           and then Scope (Etype (E)) /= Instance
           and then Is_Entity_Name (Subtype_Indication (Parent (E)))
         then
            declare
               Indic : constant Node_Id := Subtype_Indication (Parent (E));

            begin
               --  Restore the proper view of the actual from the information
               --  saved earlier by Instantiate_Type.

               Check_Private_View (Indic);

               --  If this view is an array type, check its component type.
               --  This handles the case of an array type whose component
               --  type is private, used as the actual in an instantiation
               --  of a generic construct declared in the same package as
               --  the component type and taking an array type with this
               --  component type as formal type parameter.

               if Is_Array_Type (Etype (Indic)) then
                  Check_Actual_Type
                    (Component_Type_For_Private_View (Etype (Indic)));
               end if;
            end;

            --  If the actual is itself the formal of a parent instance,
            --  then also restore the proper view of its actual and so on.
            --  That's necessary for nested instantiations of the form

            --    generic
            --      type Component is private;
            --      type Array_Type is array (Positive range <>) of Component;
            --    procedure Proc;

            --  when the outermost actuals have inconsistent views, because
            --  the Component_Type of Array_Type of the inner instantiations
            --  is the actual of Component of the outermost one and not that
            --  of the corresponding inner instantiations.

            Astype := Ancestor_Subtype (E);
            while Present (Astype)
              and then Nkind (Parent (Astype)) = N_Subtype_Declaration
              and then Present (Generic_Parent_Type (Parent (Astype)))
              and then Is_Entity_Name (Subtype_Indication (Parent (Astype)))
            loop
               Check_Private_View (Subtype_Indication (Parent (Astype)));
               Astype := Ancestor_Subtype (Astype);
            end loop;

            Set_Is_Generic_Actual_Type (E);

            if Is_Private_Type (E) and then Present (Full_View (E)) then
               Set_Is_Generic_Actual_Type (Full_View (E));
            end if;

            Set_Is_Hidden (E, False);
            Set_Is_Potentially_Use_Visible (E, In_Use (Instance));

            --  We constructed the generic actual type as a subtype of the
            --  supplied type. This means that it normally would not inherit
            --  subtype specific attributes of the actual, which is wrong for
            --  the generic case.

            Astype := Ancestor_Subtype (E);

            if No (Astype) then

               --  This can happen when E is an itype that is the full view of
               --  a private type completed, e.g. with a constrained array. In
               --  that case, use the first subtype, which will carry size
               --  information. The base type itself is unconstrained and will
               --  not carry it.

               Astype := First_Subtype (E);
            end if;

            Set_Size_Info      (E, Astype);
            Copy_RM_Size       (To => E, From => Astype);
            Set_First_Rep_Item (E, First_Rep_Item (Astype));

            if Is_Discrete_Or_Fixed_Point_Type (E) then
               Set_RM_Size (E, RM_Size (Astype));
            end if;

         elsif Ekind (E) = E_Package then

            --  If this is the renaming for the current instance, we're done.
            --  Otherwise it is a formal package. If the corresponding formal
            --  was declared with a box, the (instantiations of the) generic
            --  formal part are also visible. Otherwise, ignore the entity
            --  created to validate the actuals.

            if Renamed_Entity (E) = Instance then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            --  The visibility of a formal of an enclosing generic is already
            --  correct.

            elsif Denotes_Formal_Package (E) then
               null;

            elsif Present (Associated_Formal_Package (E))
              and then not Is_Generic_Formal (E)
            then
               Check_Generic_Actuals
                 (Renamed_Entity (E),
                  Is_Formal_Box =>
                    Box_Present (Parent (Associated_Formal_Package (E))));

               Set_Is_Hidden (E, False);
            end if;

         --  If this is a subprogram instance (in a wrapper package) the
         --  actual is fully visible.

         elsif Is_Wrapper_Package (Instance) then
            Set_Is_Hidden (E, False);

         --  If the formal package is declared with a box, or if the formal
         --  parameter is defaulted, it is visible in the body.

         elsif Is_Formal_Box or else Is_Visible_Formal (E) then
            Set_Is_Hidden (E, False);
         end if;

         --  Check directly the type of the actual objects, including the
         --  component type for array types.

         if Ekind (E) in E_Constant | E_Variable then
            Check_Actual_Type (Etype (E));

            if Is_Array_Type (Etype (E)) then
               Check_Actual_Type (Component_Type (Etype (E)));
            end if;

         --  As well as the type of formal parameters of actual subprograms

         elsif Ekind (E) in E_Function | E_Procedure
           and then Is_Generic_Actual_Subprogram (E)
           and then Present (Alias (E))
         then
            Formal := First_Formal (Alias (E));
            while Present (Formal) loop
               Check_Actual_Type (Etype (Formal));
               Next_Formal (Formal);
            end loop;
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
      E        : Entity_Id;
      Inst_Par : Entity_Id := Empty;
      S        : Node_Id;

      function Find_Generic_Child
        (Scop : Entity_Id;
         Id   : Node_Id) return Entity_Id;
      --  Search generic parent for possible child unit with the given name

      function In_Enclosing_Instance return Boolean;
      --  Within an instance of the parent, the child unit may be denoted by
      --  a simple name, or an abbreviated expanded name. Examine enclosing
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
                    and then not Is_Visible_Lib_Unit (E)
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
               --  Check whether the generic we are looking for is a child of
               --  this instance.

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
      --  If the name of the generic is given by a selected component, it may
      --  be the name of a generic child unit, and the prefix is the name of an
      --  instance of the parent, in which case the child unit must be visible.
      --  If this instance is not in scope, it must be placed there and removed
      --  after instantiation, because what is being instantiated is not the
      --  original child, but the corresponding child present in the instance
      --  of the parent.

      --  If the child is instantiated within the parent, it can be given by
      --  a simple name. In this case the instance is already in scope, but
      --  the child generic must be recovered from the generic parent as well.

      if Nkind (Gen_Id) = N_Selected_Component then
         S := Selector_Name (Gen_Id);
         Analyze (Prefix (Gen_Id));
         Inst_Par := Entity (Prefix (Gen_Id));

         if Ekind (Inst_Par) = E_Package
           and then Present (Renamed_Entity (Inst_Par))
         then
            Inst_Par := Renamed_Entity (Inst_Par);
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

            --  The prefix denotes an instantiation. The entity itself may be a
            --  nested generic, or a child unit.

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

               --  A common mistake is to replicate the naming scheme of a
               --  hierarchy by instantiating a generic child directly, rather
               --  than the implicit child in a parent instance:

               --  generic .. package Gpar is ..
               --  generic .. package Gpar.Child is ..
               --  package Par is new Gpar ();

               --  with Gpar.Child;
               --  package Par.Child is new Gpar.Child ();
               --                           rather than Par.Child

               --  In this case the instantiation is within Par, which is an
               --  instance, but Gpar does not denote Par because we are not IN
               --  the instance of Gpar, so this is illegal. The test below
               --  recognizes this particular case.

               declare
                  --  We want to reject the final instantiation in
                  --    generic package G1 is end G1;
                  --    generic package G1.G2 is end G1.G2;
                  --    with G1; package I1 is new G1;
                  --    with G1.G2; package I1.I2 is new G1.G2;
                  --  because the use of G1.G2 should instead be either
                  --  I1.G2 or simply G2. However, the tree that is built
                  --  in this case is wrong. In the expanded copy
                  --  of G2, we need (and therefore generate) a renaming
                  --    package G1 renames I1;
                  --  but this renaming should not participate in resolving
                  --  this occurrence of the name "G1.G2"; unfortunately,
                  --  it does. Rather than correct this error, we compensate
                  --  for it in this function.
                  --
                  --  We also perform another adjustment here. If we are
                  --  currently inside a generic package, then that
                  --  generic package needs to be treated as a package.
                  --  For example, if a generic Aaa declares a nested generic
                  --  Bbb (perhaps as a child unit) then Aaa can also legally
                  --  declare an instance of Aaa.Bbb.

                  function Adjusted_Inst_Par_Ekind return Entity_Kind;

                  -----------------------------
                  -- Adjusted_Inst_Par_Ekind --
                  -----------------------------

                  function Adjusted_Inst_Par_Ekind return Entity_Kind is
                     Prefix_Entity   : Entity_Id;
                     Inst_Par_GP     : Node_Id;
                     Inst_Par_Parent : Node_Id := Parent (Inst_Par);
                  begin
                     if Nkind (Inst_Par_Parent) = N_Defining_Program_Unit_Name
                     then
                        Inst_Par_Parent := Parent (Inst_Par_Parent);
                     end if;

                     Inst_Par_GP := Generic_Parent (Inst_Par_Parent);

                     if Nkind (Gen_Id) = N_Expanded_Name
                       and then Present (Inst_Par_GP)
                       and then Ekind (Inst_Par_GP) = E_Generic_Package
                     then
                        Prefix_Entity := Entity (Prefix (Gen_Id));

                        if Present (Prefix_Entity)
                           and then not Comes_From_Source (Prefix_Entity)
                           and then Nkind (Parent (Prefix_Entity)) =
                                      N_Package_Renaming_Declaration
                           and then Chars (Prefix_Entity) = Chars (Inst_Par_GP)
                        then
                           return E_Generic_Package;
                        end if;
                     end if;

                     if Ekind (Inst_Par) = E_Generic_Package
                       and then In_Open_Scopes (Inst_Par)
                     then
                        --  If we are inside a generic package then
                        --  treat it as a package.
                        return E_Package;
                     end if;

                     --  The usual path
                     return Ekind (Inst_Par);
                  end Adjusted_Inst_Par_Ekind;

               begin
                  if Is_Child_Unit (E)
                    and then (No (Inst_Par)
                              or else Adjusted_Inst_Par_Ekind =
                                      E_Generic_Package)
                    and then (not In_Instance
                              or else Nkind (Parent (Parent (Gen_Id))) =
                                        N_Compilation_Unit)
                  then
                     Error_Msg_N
                       ("prefix of generic child unit must be " &
                          "instance of parent",
                        Gen_Id);
                  end if;
               end;

               if not In_Open_Scopes (Inst_Par)
                 and then Nkind (Parent (Gen_Id)) not in
                                           N_Generic_Renaming_Declaration
               then
                  Install_Parent (Inst_Par);
                  Parent_Installed := True;

               elsif In_Open_Scopes (Inst_Par) then

                  --  If the parent is already installed, install the actuals
                  --  for its formal packages. This is necessary when the child
                  --  instance is a child of the parent instance: in this case,
                  --  the parent is placed on the scope stack but the formal
                  --  packages are not made visible.

                  Install_Formal_Packages (Inst_Par);
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

            --  The generic unit may be the renaming of the implicit child
            --  present in an instance. In that case the parent instance is
            --  obtained from the name of the renamed entity.

            elsif Ekind (Entity (Gen_Id)) = E_Generic_Package
              and then Present (Renamed_Entity (Entity (Gen_Id)))
              and then Is_Child_Unit (Renamed_Entity (Entity (Gen_Id)))
            then
               declare
                  Renamed_Package : constant Node_Id :=
                                      Name (Parent (Entity (Gen_Id)));
               begin
                  if Nkind (Renamed_Package) = N_Expanded_Name then
                     Inst_Par := Entity (Prefix (Renamed_Package));
                     Install_Parent (Inst_Par);
                     Parent_Installed := True;
                  end if;
               end;
            end if;
         end if;

      elsif Nkind (Gen_Id) = N_Expanded_Name then

         --  Entity already present, analyze prefix, whose meaning may be an
         --  instance in the current context. If it is an instance of a
         --  relative within another, the proper parent may still have to be
         --  installed, if they are not of the same generation.

         Analyze (Prefix (Gen_Id));

         --  Prevent cascaded errors

         if Etype (Prefix (Gen_Id)) = Any_Type then
            return;
         end if;

         --  In the unlikely case that a local declaration hides the name of
         --  the parent package, locate it on the homonym chain. If the context
         --  is an instance of the parent, the renaming entity is flagged as
         --  such.

         Inst_Par := Entity (Prefix (Gen_Id));
         while Present (Inst_Par)
           and then not Is_Package_Or_Generic_Package (Inst_Par)
         loop
            Inst_Par := Homonym (Inst_Par);
         end loop;

         pragma Assert (Present (Inst_Par));
         Set_Entity (Prefix (Gen_Id), Inst_Par);

         if In_Enclosing_Instance then
            null;

         elsif Present (Entity (Gen_Id))
           and then No (Renamed_Entity (Entity (Gen_Id)))
           and then Is_Child_Unit (Entity (Gen_Id))
           and then not In_Open_Scopes (Inst_Par)
         then
            Install_Parent (Inst_Par);
            Parent_Installed := True;

         --  Handle renaming of generic child unit

         elsif Present (Entity (Gen_Id))
           and then Present (Renamed_Entity (Entity (Gen_Id)))
           and then Is_Child_Unit (Renamed_Entity (Entity (Gen_Id)))
         then
            declare
               E        : Entity_Id;
               Ren_Decl : Node_Id;

            begin
               --  The entity of the renamed generic child unit does not
               --  have any reference to the instantiated parent. In order to
               --  locate it we traverse the scope containing the renaming
               --  declaration; the instance of the parent is available in
               --  the prefix of the renaming declaration. For example:

               --     package A is
               --       package Inst_Par is new ...
               --       generic package Ren_Child renames Ins_Par.Child;
               --     end;

               --     with A;
               --     package B is
               --       package Inst_Child is new A.Ren_Child;
               --     end;

               E := First_Entity (Entity (Prefix (Gen_Id)));
               while Present (E) loop
                  if not Is_Object (E)
                    and then Present (Renamed_Entity (E))
                    and then
                      Renamed_Entity (E) = Renamed_Entity (Entity (Gen_Id))
                  then
                     Ren_Decl := Parent (E);
                     Inst_Par := Entity (Prefix (Name (Ren_Decl)));

                     if not In_Open_Scopes (Inst_Par) then
                        Install_Parent (Inst_Par);
                        Parent_Installed := True;
                     end if;

                     exit;
                  end if;

                  E := Next_Entity (E);
               end loop;
            end;
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
              and then Is_Child_Unit (Renamed_Entity (E))
              and then Is_Generic_Unit (Scope (Renamed_Entity (E)))
              and then Nkind (Name (Parent (E))) = N_Expanded_Name
            then
               Rewrite (Gen_Id, New_Copy_Tree (Name (Parent (E))));
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
      Comparison : constant Boolean := Nkind (N) in N_Op_Compare;
      Typ        : constant Entity_Id :=
        (if Comparison then Compare_Type (N) else Etype (N));

      procedure Check_Private_Type (T : Entity_Id; Private_View : Boolean);
      --  Check that the available view of T matches Private_View and, if not,
      --  switch the view of T or of its base type.

      procedure Check_Private_Type (T : Entity_Id; Private_View : Boolean) is
         BT : constant Entity_Id := Base_Type (T);

      begin
         --  If the full declaration was not visible in the generic, stop here

         if Private_View then
            return;
         end if;

         --  Exchange views if the type was not private in the generic but is
         --  private at the point of instantiation. Do not exchange views if
         --  the scope of the type is in scope. This can happen if both generic
         --  and instance are sibling units, or if type is defined in a parent.
         --  In this case the visibility of the type will be correct for all
         --  semantic checks.

         if Is_Private_Type (T)
           and then Present (Full_View (T))
           and then not In_Open_Scopes (Scope (T))
         then
            Switch_View (T);

         --  Finally, a nonprivate subtype may have a private base type, which
         --  must be exchanged for consistency. This can happen when a package
         --  body is instantiated, when the scope stack is empty but in fact
         --  the subtype and the base type are declared in an enclosing scope.

         --  Note that in this case we introduce an inconsistency in the view
         --  set, because we switch the base type BT, but there could be some
         --  private dependent subtypes of BT which remain unswitched. Such
         --  subtypes might need to be switched at a later point (see specific
         --  provision for that case in Switch_View).

         elsif not Is_Private_Type (T)
           and then Is_Private_Type (BT)
           and then Present (Full_View (BT))
           and then not In_Open_Scopes (BT)
         then
            Prepend_Elmt (Full_View (BT), Exchanged_Views);
            Exchange_Declarations (BT);
         end if;
      end Check_Private_Type;

   --  Start of processing for Check_Private_View

   begin
      if Present (Typ) then
         --  If the type appears in a subtype declaration, the subtype in
         --  instance must have a view compatible with that of its parent,
         --  which must be exchanged (see corresponding code in Restore_
         --  Private_Views) so we make an exception to the open scope rule
         --  implemented by Check_Private_Type above.

         if Has_Private_View (N)
           and then not Is_Private_Type (Typ)
           and then not Has_Been_Exchanged (Typ)
           and then (not In_Open_Scopes (Scope (Typ))
                      or else Nkind (Parent (N)) = N_Subtype_Declaration)
         then
            declare
               Assoc : constant Node_Id := Get_Associated_Node (N);

            begin
               --  In the generic, only the private declaration was visible

               Prepend_Elmt (Typ, Exchanged_Views);
               Exchange_Declarations
                 (if Comparison then Compare_Type (Assoc) else Etype (Assoc));
            end;

         --  Check that the available views of Typ match their respective flag.
         --  Note that the type of a visible discriminant is never private.

         else
            Check_Private_Type (Typ, Has_Private_View (N));

            if Is_Access_Type (Typ) then
               Check_Private_Type
                 (Designated_Type (Typ), Has_Secondary_Private_View (N));

            elsif Is_Array_Type (Typ) then
               Check_Private_Type
                 (Component_Type_For_Private_View (Typ),
                  Has_Secondary_Private_View (N));

            elsif (Is_Record_Type (Typ) or else Is_Concurrent_Type (Typ))
              and then Has_Discriminants (Typ)
            then
               declare
                  Disc : Entity_Id;

               begin
                  Disc := First_Discriminant (Typ);
                  while Present (Disc) loop
                     Check_Private_Type (Etype (Disc), False);
                     Next_Discriminant (Disc);
                  end loop;
               end;
            end if;
         end if;
      end if;
   end Check_Private_View;

   -----------------------------
   -- Check_Hidden_Primitives --
   -----------------------------

   function Check_Hidden_Primitives (Renamings : List_Id) return Elist_Id is
      Actual : Node_Id;
      Gen_T  : Entity_Id;
      Result : Elist_Id := No_Elist;

   begin
      if No (Renamings) then
         return No_Elist;
      end if;

      --  Traverse the list of associations between formals and actuals
      --  searching for renamings of tagged types

      Actual := First (Renamings);
      while Present (Actual) loop
         if Nkind (Actual) = N_Subtype_Declaration then
            Gen_T := Generic_Parent_Type (Actual);

            if Present (Gen_T) and then Is_Tagged_Type (Gen_T) then

               --  Traverse the list of primitives of the actual types
               --  searching for hidden primitives that are visible in the
               --  corresponding generic formal; leave them visible and
               --  append them to Result to restore their decoration later.

               Install_Hidden_Primitives
                 (Prims_List => Result,
                  Gen_T      => Gen_T,
                  Act_T      => Entity (Subtype_Indication (Actual)));
            end if;
         end if;

         Next (Actual);
      end loop;

      return Result;
   end Check_Hidden_Primitives;

   -------------------------------------
   -- Component_Type_For_Private_View --
   -------------------------------------

   function Component_Type_For_Private_View (T : Entity_Id) return Entity_Id is
      Typ : constant Entity_Id := Component_Type (T);

   begin
      if Is_Array_Type (Typ) and then not Has_Private_Declaration (Typ) then
         return Component_Type_For_Private_View (Typ);
      else
         return Typ;
      end if;
   end Component_Type_For_Private_View;

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
                    ("circular instantiation: & instantiated within &!",
                     N, Scop);
                  return True;

               elsif Node (Elmt) = Inner then
                  return True;

               elsif Contains_Instance_Of (Node (Elmt), Scop, N) then
                  Error_Msg_Node_2 := Inner;
                  Error_Msg_NE
                    ("circular instantiation: & instantiated within &!",
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
      --  Check the given value of one of the Fields referenced by the current
      --  node to determine whether to copy it recursively. The field may hold
      --  a Node_Id, a List_Id, or an Elist_Id, or a plain value (Sloc, Uint,
      --  Char) in which case it need not be copied.

      procedure Copy_Descendants;
      --  Common utility for various nodes

      function Copy_Generic_Elist (E : Elist_Id) return Elist_Id;
      --  Make copy of element list

      function Copy_Generic_List
        (L         : List_Id;
         Parent_Id : Node_Id) return List_Id;
      --  Apply Copy_Generic_Node recursively to the members of a node list

      function In_Defining_Unit_Name (Nam : Node_Id) return Boolean;
      --  True if an identifier is part of the defining program unit name of
      --  a child unit.
      --  Consider removing this subprogram now that ASIS no longer uses it.

      ----------------------
      -- Copy_Descendants --
      ----------------------

      procedure Copy_Descendants is
         procedure Walk is new
           Walk_Sinfo_Fields_Pairwise (Copy_Generic_Descendant);
      begin
         Walk (New_N, N);
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
         return
           Present (Parent (Nam))
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

      --  If we are instantiating, we want to adjust the sloc based on the
      --  current S_Adjustment. However, if this is the root node of a subunit,
      --  we need to defer that adjustment to below (see "elsif Instantiating
      --  and Was_Stub"), so it comes after Create_Instantiation_Source has
      --  computed the adjustment.

      if Instantiating
        and then not (Nkind (N) in N_Proper_Body
                       and then Was_Originally_Stub (N))
      then
         Adjust_Instantiation_Sloc (New_N, S_Adjustment);
      end if;

      if not Is_List_Member (N) then
         Set_Parent (New_N, Parent_Id);
      end if;

      --  Special casing for identifiers and other entity names and operators

      if Nkind (N) in N_Character_Literal
                    | N_Expanded_Name
                    | N_Identifier
                    | N_Operator_Symbol
                    | N_Op
      then
         if not Instantiating then

            --  Link both nodes in order to assign subsequently the entity of
            --  the copy to the original node, in case this is a global
            --  reference.

            Set_Associated_Node (N, New_N);

            --  If we are within an instantiation, this is a nested generic
            --  that has already been analyzed at the point of definition.
            --  We must preserve references that were global to the enclosing
            --  parent at that point. Other occurrences, whether global or
            --  local to the current generic, must be resolved anew, so we
            --  reset the entity in the generic copy. A global reference has a
            --  smaller depth than the parent, or else the same depth in case
            --  both are distinct compilation units.

            --  A child unit is implicitly declared within the enclosing parent
            --  but is in fact global to it, and must be preserved.

            --  It is also possible for Current_Instantiated_Parent to be
            --  defined, and for this not to be a nested generic, namely if
            --  the unit is loaded through Rtsfind. In that case, the entity of
            --  New_N is only a link to the associated node, and not a defining
            --  occurrence.

            --  The entities for parent units in the defining_program_unit of a
            --  generic child unit are established when the context of the unit
            --  is first analyzed, before the generic copy is made. They are
            --  preserved in the copy for use in e.g. ASIS queries.

            Ent := Entity (New_N);

            if No (Current_Instantiated_Parent.Gen_Id) then
               if No (Ent)
                 or else Nkind (Ent) /= N_Defining_Identifier
                 or else not In_Defining_Unit_Name (N)
               then
                  Set_Associated_Node (New_N, Empty);
               end if;

            elsif No (Ent)
              or else Nkind (Ent) not in N_Entity
              or else No (Scope (Ent))
              or else
                (Scope (Ent) = Current_Instantiated_Parent.Gen_Id
                  and then not Is_Child_Unit (Ent))
              or else
                (Scope_Depth_Set (Scope (Ent))
                  and then
                 Scope_Depth (Scope (Ent)) >
                             Scope_Depth (Current_Instantiated_Parent.Gen_Id)
                  and then
                    Get_Source_Unit (Ent) =
                    Get_Source_Unit (Current_Instantiated_Parent.Gen_Id))
            then
               Set_Associated_Node (New_N, Empty);
            end if;

         --  Case of instantiating identifier or some other name or operator

         else
            --  If the associated node is still defined, the entity in it
            --  is global, and must be copied to the instance. If this copy
            --  is being made for a body to inline, it is applied to an
            --  instantiated tree, and the entity is already present and
            --  must be also preserved.

            declare
               Assoc : constant Node_Id := Get_Associated_Node (N);

            begin
               if Present (Assoc) then
                  if Nkind (Assoc) = Nkind (N) then
                     Set_Entity (New_N, Entity (Assoc));
                     Check_Private_View (N);

                  --  The node is a reference to a global type and acts as the
                  --  subtype mark of a qualified expression created in order
                  --  to aid resolution of accidental overloading in instances.
                  --  Since N is a reference to a type, the Associated_Node of
                  --  N denotes an entity rather than another identifier. See
                  --  Qualify_Universal_Operands for details.

                  elsif Nkind (N) = N_Identifier
                    and then Nkind (Parent (N)) = N_Qualified_Expression
                    and then Subtype_Mark (Parent (N)) = N
                    and then Is_Qualified_Universal_Literal (Parent (N))
                  then
                     Set_Entity (New_N, Assoc);

                  --  Cope with the rewriting into expanded name that may have
                  --  occurred in between, e.g. in Check_Generic_Child_Unit for
                  --  generic renaming declarations.

                  elsif Nkind (Assoc) = N_Expanded_Name then
                     Rewrite (N, New_Copy_Tree (Assoc));
                     Set_Associated_Node (N, Assoc);
                     return Copy_Generic_Node (N, Parent_Id, Instantiating);

                  --  The name in the call may be a selected component if the
                  --  call has not been analyzed yet, as may be the case for
                  --  pre/post conditions in a generic unit.

                  elsif Nkind (Assoc) = N_Function_Call
                    and then Is_Entity_Name (Name (Assoc))
                  then
                     Set_Entity (New_N, Entity (Name (Assoc)));
                     Check_Private_View (N);

                  elsif Nkind (Assoc) in N_Entity
                    and then (Expander_Active
                               or else (GNATprove_Mode
                                         and then not In_Spec_Expression
                                         and then not Inside_A_Generic))
                  then
                     --  Inlining case: we are copying a tree that contains
                     --  global entities, which are preserved in the copy to be
                     --  used for subsequent inlining.

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

         --  For operators, copy the operands

         elsif Nkind (N) in N_Op then
            if Nkind (N) in N_Binary_Op then
               Set_Left_Opnd (New_N,
                 Copy_Generic_Node (Left_Opnd (N), New_N, Instantiating));
            end if;

            Set_Right_Opnd (New_N,
              Copy_Generic_Node (Right_Opnd (N), New_N, Instantiating));
         end if;

      --  Establish a link between an entity from the generic template and the
      --  corresponding entity in the generic copy to be analyzed.

      elsif Nkind (N) in N_Entity then
         if not Instantiating then
            Set_Associated_Entity (N, New_N);
         end if;

         --  Clear any existing link the copy may inherit from the replicated
         --  generic template entity.

         Set_Associated_Entity (New_N, Empty);

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
         --  more permissive system might defer this analysis to the point
         --  of instantiation, but this seems too complicated for now.

         if not Instantiating then
            declare
               Subunit_Name : constant Unit_Name_Type := Get_Unit_Name (N);
               Subunit      : Node_Id;
               Unum         : Unit_Number_Type;
               New_Body     : Node_Id;

            begin
               --  Make sure that, if it is a subunit of the main unit that is
               --  preprocessed and if -gnateG is specified, the preprocessed
               --  file will be written.

               Lib.Analysing_Subunit_Of_Main :=
                 Lib.In_Extended_Main_Source_Unit (N);
               Unum :=
                 Load_Unit
                   (Load_Name  => Subunit_Name,
                    Required   => False,
                    Subunit    => True,
                    Error_Node => N);
               Lib.Analysing_Subunit_Of_Main := False;

               --  If the proper body is not found, a warning message will be
               --  emitted when analyzing the stub, or later at the point of
               --  instantiation. Here we just leave the stub as is.

               if Unum = No_Unit then
                  Subunits_Missing := True;
                  goto Subunit_Not_Found;
               end if;

               Subunit := Cunit (Unum);

               if Nkind (Unit (Subunit)) /= N_Subunit then
                  Error_Msg_N
                    ("found child unit instead of expected SEPARATE subunit",
                     Subunit);
                  Error_Msg_Sloc := Sloc (N);
                  Error_Msg_N ("\to complete stub #", Subunit);
                  goto Subunit_Not_Found;
               end if;

               --  We must create a generic copy of the subunit, in order to
               --  perform semantic analysis on it, and we must replace the
               --  stub in the original generic unit with the subunit, in order
               --  to preserve non-local references within.

               --  Only the proper body needs to be copied. Library_Unit and
               --  context clause are simply inherited by the generic copy.
               --  Note that the copy (which may be recursive if there are
               --  nested subunits) must be done first, before attaching it to
               --  the enclosing generic.

               New_Body :=
                 Copy_Generic_Node
                   (Proper_Body (Unit (Subunit)),
                    Empty, Instantiating => False);

               --  Now place the original proper body in the original generic
               --  unit. This is a body, not a compilation unit.

               Rewrite (N, Proper_Body (Unit (Subunit)));
               Set_Is_Compilation_Unit (Defining_Entity (N), False);
               Set_Was_Originally_Stub (N);

               --  Finally replace the body of the subunit with its copy, and
               --  make this new subunit into the library unit of the generic
               --  copy, which does not have stubs any longer.

               Set_Proper_Body (Unit (Subunit), New_Body);
               Set_Library_Unit (New_N, Subunit);
               Inherit_Context (Unit (Subunit), N);
            end;

         --  If we are instantiating, this must be an error case, since
         --  otherwise we would have replaced the stub node by the proper body
         --  that corresponds. So just ignore it in the copy (i.e. we have
         --  copied it, and that is good enough).

         else
            null;
         end if;

         <<Subunit_Not_Found>> null;

      --  If the node is a compilation unit, it is the subunit of a stub, which
      --  has been loaded already (see code below). In this case, the library
      --  unit field of N points to the parent unit (which is a compilation
      --  unit) and need not (and cannot) be copied.

      --  When the proper body of the stub is analyzed, the library_unit link
      --  is used to establish the proper context (see sem_ch10).

      --  The other fields of a compilation unit are copied as usual

      elsif Nkind (N) = N_Compilation_Unit then

         --  This code can only be executed when not instantiating, because in
         --  the copy made for an instantiation, the compilation unit node has
         --  disappeared at the point that a stub is replaced by its proper
         --  body.

         pragma Assert (not Instantiating);

         Set_Context_Items (New_N,
           Copy_Generic_List (Context_Items (N), New_N));

         Set_Unit (New_N,
           Copy_Generic_Node (Unit (N), New_N, Instantiating => False));

         Set_First_Inlined_Subprogram (New_N,
           Copy_Generic_Node
             (First_Inlined_Subprogram (N), New_N, Instantiating => False));

         Set_Aux_Decls_Node
           (New_N,
            Copy_Generic_Node
              (Aux_Decls_Node (N), New_N, Instantiating => False));

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

      elsif Nkind (N) in N_Aggregate | N_Extension_Aggregate then
         if not Instantiating then
            Set_Associated_Node (N, New_N);

         else
            --  If, in the generic, the aggregate has a global composite type
            --  and, at the point of instantiation, the type has a private view
            --  then install the full view.

            declare
               Assoc : constant Node_Id := Get_Associated_Node (N);

            begin
               if Present (Assoc)
                 and then Nkind (Assoc) = Nkind (N)
                 and then Present (Etype (Assoc))
                 and then Is_Private_Type (Etype (Assoc))
               then
                  Switch_View (Etype (Assoc));
               end if;
            end;

            --  Moreover, for a full aggregate, if the type is a derived tagged
            --  type and has a global ancestor, then also restore the full view
            --  of this ancestor and do so up to the root type. Beware that the
            --  Ancestor_Type field is overloaded, so test that it's an entity.

            if Nkind (N) = N_Aggregate
              and then Present (Ancestor_Type (N))
              and then Nkind (Ancestor_Type (N)) in N_Entity
            then
               declare
                  Root_Typ : constant Entity_Id :=
                               Root_Type (Ancestor_Type (N));

                  Typ : Entity_Id := Ancestor_Type (N);

               begin
                  loop
                     if Is_Private_Type (Typ) then
                        Switch_View (Typ);
                     end if;

                     exit when Typ = Root_Typ;

                     Typ := Etype (Typ);
                  end loop;
               end;
            end if;
         end if;

         --  Do not copy the associated node, which points to the generic copy
         --  of the aggregate.

         if Nkind (N) = N_Aggregate then
            Set_Aggregate_Bounds
              (New_N,
               Node_Id (Copy_Generic_Descendant
                          (Union_Id (Aggregate_Bounds (N)))));

         elsif Nkind (N) = N_Extension_Aggregate then
            Set_Ancestor_Part
              (New_N,
               Node_Id (Copy_Generic_Descendant
                          (Union_Id (Ancestor_Part (N)))));

         else
            pragma Assert (False);
         end if;

         Set_Expressions
           (New_N,
            List_Id (Copy_Generic_Descendant (Union_Id (Expressions (N)))));
         Set_Component_Associations
           (New_N,
            List_Id (Copy_Generic_Descendant
                       (Union_Id (Component_Associations (N)))));
         Set_Etype
           (New_N, Node_Id (Copy_Generic_Descendant (Union_Id (Etype (N)))));

      --  Allocators do not have an identifier denoting the access type, so we
      --  must locate it through the expression to check whether the views are
      --  consistent.

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

               if Present (Acc_T) and then Is_Private_Type (Acc_T) then
                  Switch_View (Acc_T);
               end if;
            end if;

            Copy_Descendants;
         end;

      --  Loop parameter specifications do not have an identifier denoting the
      --  index type, so we must locate it through the defining identifier to
      --  check whether the views are consistent.

      elsif Nkind (N) = N_Loop_Parameter_Specification
        and then Instantiating
      then
         declare
            Id : constant Entity_Id :=
                   Get_Associated_Entity (Defining_Identifier (N));

            Index_T : Entity_Id;

         begin
            if Present (Id) and then Present (Etype (Id)) then
               Index_T := First_Subtype (Etype (Id));

               if Present (Index_T) and then Is_Private_Type (Index_T) then
                  Switch_View (Index_T);
               end if;
            end if;

            Copy_Descendants;
         end;

      --  For a proper body, we must catch the case of a proper body that
      --  replaces a stub. This represents the point at which a separate
      --  compilation unit, and hence template file, may be referenced, so we
      --  must make a new source instantiation entry for the template of the
      --  subunit, and ensure that all nodes in the subunit are adjusted using
      --  this new source instantiation entry.

      elsif Nkind (N) in N_Proper_Body then
         declare
            Save_Adjustment : constant Sloc_Adjustment := S_Adjustment;
         begin
            if Instantiating and then Was_Originally_Stub (N) then
               Create_Instantiation_Source
                 (Instantiation_Node,
                  Defining_Entity (N),
                  S_Adjustment);

               Adjust_Instantiation_Sloc (New_N, S_Adjustment);
            end if;

            --  Now copy the fields of the proper body, using the new
            --  adjustment factor if one was needed as per test above.

            Copy_Descendants;

            --  Restore the original adjustment factor

            S_Adjustment := Save_Adjustment;
         end;

      elsif Nkind (N) = N_Pragma and then Instantiating then

         --  Do not copy Comment or Ident pragmas their content is relevant to
         --  the generic unit, not to the instantiating unit.

         if Pragma_Name_Unmapped (N) in Name_Comment | Name_Ident then
            New_N := Make_Null_Statement (Sloc (N));

         --  Do not copy pragmas generated from aspects because the pragmas do
         --  not carry any semantic information, plus they will be regenerated
         --  in the instance.

         --  However, generating C we need to copy them since postconditions
         --  are inlined by the front end, and the front-end inlining machinery
         --  relies on this routine to perform inlining.

         elsif From_Aspect_Specification (N) then
            New_N := Make_Null_Statement (Sloc (N));

         else
            Copy_Descendants;
         end if;

      elsif Nkind (N) in N_Integer_Literal | N_Real_Literal then

         --  No descendant fields need traversing

         null;

      elsif Nkind (N) = N_String_Literal
        and then Present (Etype (N))
        and then Instantiating
      then
         --  If the string is declared in an outer scope, the string_literal
         --  subtype created for it may have the wrong scope. Force reanalysis
         --  of the constant to generate a new itype in the proper context.

         Set_Etype (New_N, Empty);
         Set_Analyzed (New_N, False);

      --  For the remaining nodes, copy their descendants recursively

      else
         Copy_Descendants;

         if Instantiating and then Nkind (N) = N_Subprogram_Body then
            Set_Generic_Parent (Specification (New_N), N);

            --  Should preserve Corresponding_Spec??? (12.3(14))
         end if;
      end if;

      --  Propagate dimensions if present, so that they are reflected in the
      --  instance.

      if Nkind (N) in N_Has_Etype
        and then (Nkind (N) in N_Op or else Is_Entity_Name (N))
        and then Present (Etype (N))
        and then Is_Floating_Point_Type (Etype (N))
        and then Has_Dimension_System (Etype (N))
      then
         Copy_Dimensions (N, New_N);
      end if;

      return New_N;
   end Copy_Generic_Node;

   ----------------------------
   -- Denotes_Formal_Package --
   ----------------------------

   function Denotes_Formal_Package
     (Pack     : Entity_Id;
      On_Exit  : Boolean := False;
      Instance : Entity_Id := Empty) return Boolean
   is
      Par  : Entity_Id;
      Scop : constant Entity_Id := Scope (Pack);
      E    : Entity_Id;

      function Is_Actual_Of_Previous_Formal (P : Entity_Id) return Boolean;
      --  The package in question may be an actual for a previous formal
      --  package P of the current instance, so examine its actuals as well.
      --  This must be recursive over other formal packages.

      ----------------------------------
      -- Is_Actual_Of_Previous_Formal --
      ----------------------------------

      function Is_Actual_Of_Previous_Formal (P : Entity_Id) return Boolean is
         E1 : Entity_Id;

      begin
         E1 := First_Entity (P);
         while Present (E1) and then E1 /= Instance loop
            if Ekind (E1) = E_Package
              and then Nkind (Parent (E1)) = N_Package_Renaming_Declaration
            then
               if Renamed_Entity (E1) = Pack then
                  return True;

               elsif E1 = P or else Renamed_Entity (E1) = P then
                  return False;

               elsif Is_Actual_Of_Previous_Formal (E1) then
                  return True;
               end if;
            end if;

            Next_Entity (E1);
         end loop;

         return False;
      end Is_Actual_Of_Previous_Formal;

   --  Start of processing for Denotes_Formal_Package

   begin
      if On_Exit then
         Par :=
           Instance_Envs.Table
             (Instance_Envs.Last).Instantiated_Parent.Act_Id;
      else
         Par := Current_Instantiated_Parent.Act_Id;
      end if;

      if Ekind (Scop) = E_Generic_Package
        or else Nkind (Unit_Declaration_Node (Scop)) =
                                         N_Generic_Subprogram_Declaration
      then
         return True;

      elsif Nkind (Original_Node (Unit_Declaration_Node (Pack))) =
        N_Formal_Package_Declaration
      then
         return True;

      elsif No (Par) then
         return False;

      else
         --  Check whether this package is associated with a formal package of
         --  the enclosing instantiation. Iterate over the list of renamings.

         E := First_Entity (Par);
         while Present (E) loop
            if Ekind (E) /= E_Package
              or else Nkind (Parent (E)) /= N_Package_Renaming_Declaration
            then
               null;

            elsif Renamed_Entity (E) = Par then
               return False;

            elsif Renamed_Entity (E) = Pack then
               return True;

            elsif Is_Actual_Of_Previous_Formal (E) then
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
      --  ??? More things could be factored out in this routine. Should
      --  probably be done at a later stage.

      Inside_A_Generic := Generic_Flags.Table (Generic_Flags.Last);
      Generic_Flags.Decrement_Last;

      Expander_Mode_Restore;
   end End_Generic;

   -------------
   -- Earlier --
   -------------

   function Earlier (N1, N2 : Node_Id) return Boolean is
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

      --  Local declarations

      D1 : Integer := 0;
      D2 : Integer := 0;
      P1 : Node_Id := N1;
      P2 : Node_Id := N2;
      T1 : Source_Ptr;
      T2 : Source_Ptr;

   --  Start of processing for Earlier

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
      --  We examine their parents until we find a common declarative list.
      --  If we reach the root, N1 and N2 do not descend from the same
      --  declarative list (e.g. one is nested in the declarative part and
      --  the other is in a block in the statement part) and the earlier
      --  one is already frozen.

      while not Is_List_Member (P1)
        or else not Is_List_Member (P2)
        or else not In_Same_List (P1, P2)
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

      --  Expanded code usually shares the source location of the original
      --  construct it was generated for. This however may not necessarily
      --  reflect the true location of the code within the tree.

      --  Before comparing the slocs of the two nodes, make sure that we are
      --  working with correct source locations. Assume that P1 is to the left
      --  of P2. If either one does not come from source, traverse the common
      --  list heading towards the other node and locate the first source
      --  statement.

      --             P1                     P2
      --     ----+===+===+--------------+===+===+----
      --          expanded code          expanded code

      if not Comes_From_Source (P1) then
         while Present (P1) loop

            --  Neither P2 nor a source statement were located during the
            --  search. If we reach the end of the list, then P1 does not
            --  occur earlier than P2.

            --                     ---->
            --   start --- P2 ----- P1 --- end

            if No (Next (P1)) then
               return False;

            --  We encounter P2 while going to the right of the list. This
            --  means that P1 does indeed appear earlier.

            --             ---->
            --    start --- P1 ===== P2 --- end
            --                 expanded code in between

            elsif P1 = P2 then
               return True;

            --  No need to look any further since we have located a source
            --  statement.

            elsif Comes_From_Source (P1) then
               exit;
            end if;

            --  Keep going right

            Next (P1);
         end loop;
      end if;

      if not Comes_From_Source (P2) then
         while Present (P2) loop

            --  Neither P1 nor a source statement were located during the
            --  search. If we reach the start of the list, then P1 does not
            --  occur earlier than P2.

            --            <----
            --    start --- P2 --- P1 --- end

            if No (Prev (P2)) then
               return False;

            --  We encounter P1 while going to the left of the list. This
            --  means that P1 does indeed appear earlier.

            --                     <----
            --    start --- P1 ===== P2 --- end
            --                 expanded code in between

            elsif P2 = P1 then
               return True;

            --  No need to look any further since we have located a source
            --  statement.

            elsif Comes_From_Source (P2) then
               exit;
            end if;

            --  Keep going left

            Prev (P2);
         end loop;
      end if;

      --  At this point either both nodes came from source or we approximated
      --  their source locations through neighboring source statements.

      T1 := Top_Level_Location (Sloc (P1));
      T2 := Top_Level_Location (Sloc (P2));

      --  When two nodes come from the same instance, they have identical top
      --  level locations. To determine proper relation within the tree, check
      --  their locations within the template.

      if T1 = T2 then
         return Sloc (P1) < Sloc (P2);

      --  The two nodes either come from unrelated instances or do not come
      --  from instantiated code at all.

      else
         return T1 < T2;
      end if;
   end Earlier;

   ----------------------
   -- Find_Actual_Type --
   ----------------------

   function Find_Actual_Type
     (Typ      : Entity_Id;
      Gen_Type : Entity_Id) return Entity_Id
   is
      Gen_Scope : constant Entity_Id := Scope (Gen_Type);
      T         : Entity_Id;

   begin
      --  Special processing only applies to child units

      if not Is_Child_Unit (Gen_Scope) then
         return Get_Instance_Of (Typ);

      --  If designated or component type is itself a formal of the child unit,
      --  its instance is available.

      elsif Scope (Typ) = Gen_Scope then
         return Get_Instance_Of (Typ);

      --  If the array or access type is not declared in the parent unit,
      --  no special processing needed.

      elsif not Is_Generic_Type (Typ)
        and then Scope (Gen_Scope) /= Scope (Typ)
      then
         return Get_Instance_Of (Typ);

      --  Otherwise, retrieve designated or component type by visibility

      else
         T := Current_Entity (Typ);
         while Present (T) loop
            if In_Open_Scopes (Scope (T)) then
               return T;
            elsif Is_Generic_Actual_Type (T) then
               return T;
            end if;

            T := Homonym (T);
         end loop;

         return Typ;
      end if;
   end Find_Actual_Type;

   -----------------------------
   -- Freeze_Package_Instance --
   -----------------------------

   procedure Freeze_Package_Instance
     (N        : Node_Id;
      Gen_Body : Node_Id;
      Gen_Decl : Node_Id;
      Act_Id   : Entity_Id)
   is
      function In_Same_Scope (Gen_Id, Act_Id : Node_Id) return Boolean;
      --  Check if the generic definition and the instantiation come from
      --  a common scope, in which case the instance must be frozen after
      --  the generic body.

      function True_Sloc (N, Act_Unit : Node_Id) return Source_Ptr;
      --  If the instance is nested inside a generic unit, the Sloc of the
      --  instance indicates the place of the original definition, not the
      --  point of the current enclosing instance. Pending a better usage of
      --  Slocs to indicate instantiation places, we determine the place of
      --  origin of a node by finding the maximum sloc of any ancestor node.

      --  Why is this not equivalent to Top_Level_Location ???

      -------------------
      -- In_Same_Scope --
      -------------------

      function In_Same_Scope (Gen_Id, Act_Id : Node_Id) return Boolean is
         Act_Scop : Entity_Id := Scope (Act_Id);
         Gen_Scop : Entity_Id := Scope (Gen_Id);

      begin
         while Act_Scop /= Standard_Standard
           and then Gen_Scop /= Standard_Standard
         loop
            if Act_Scop = Gen_Scop then
               return True;
            end if;

            Act_Scop := Scope (Act_Scop);
            Gen_Scop := Scope (Gen_Scop);
         end loop;

         return False;
      end In_Same_Scope;

      ---------------
      -- True_Sloc --
      ---------------

      function True_Sloc (N, Act_Unit : Node_Id) return Source_Ptr is
         N1  : Node_Id;
         Res : Source_Ptr;

      begin
         Res := Sloc (N);
         N1  := N;
         while Present (N1) and then N1 /= Act_Unit loop
            if Sloc (N1) > Res then
               Res := Sloc (N1);
            end if;

            N1 := Parent (N1);
         end loop;

         return Res;
      end True_Sloc;

      --  Local variables

      Gen_Id    : constant Entity_Id := Get_Generic_Entity (N);
      Par_Id    : constant Entity_Id := Scope (Gen_Id);
      Act_Unit  : constant Node_Id   := Unit (Cunit (Get_Source_Unit (N)));
      Gen_Unit  : constant Node_Id   :=
                    Unit (Cunit (Get_Source_Unit (Gen_Decl)));

      Body_Unit  : Node_Id;
      F_Node     : Node_Id;
      Must_Delay : Boolean;
      Orig_Body  : Node_Id;

   --  Start of processing for Freeze_Package_Instance

   begin
      --  If the body is a subunit, the freeze point is the corresponding stub
      --  in the current compilation, not the subunit itself.

      if Nkind (Parent (Gen_Body)) = N_Subunit then
         Orig_Body := Corresponding_Stub (Parent (Gen_Body));
      else
         Orig_Body := Gen_Body;
      end if;

      Body_Unit := Unit (Cunit (Get_Source_Unit (Orig_Body)));

      --  If the instantiation and the generic definition appear in the same
      --  package declaration, this is an early instantiation. If they appear
      --  in the same declarative part, it is an early instantiation only if
      --  the generic body appears textually later, and the generic body is
      --  also in the main unit.

      --  If instance is nested within a subprogram, and the generic body
      --  is not, the instance is delayed because the enclosing body is. If
      --  instance and body are within the same scope, or the same subprogram
      --  body, indicate explicitly that the instance is delayed.

      Must_Delay :=
        (Gen_Unit = Act_Unit
          and then (Nkind (Gen_Unit) in N_Generic_Package_Declaration
                                      | N_Package_Declaration
                     or else (Gen_Unit = Body_Unit
                               and then
                              True_Sloc (N, Act_Unit) < Sloc (Orig_Body)))
          and then Is_In_Main_Unit (Original_Node (Gen_Unit))
          and then In_Same_Scope (Gen_Id, Act_Id));

      --  If this is an early instantiation, the freeze node is placed after
      --  the generic body. Otherwise, if the generic appears in an instance,
      --  we cannot freeze the current instance until the outer one is frozen.
      --  This is only relevant if the current instance is nested within some
      --  inner scope not itself within the outer instance. If this scope is
      --  a package body in the same declarative part as the outer instance,
      --  then that body needs to be frozen after the outer instance. Finally,
      --  if no delay is needed, we place the freeze node at the end of the
      --  current declarative part.

      if No (Freeze_Node (Act_Id))
        or else not Is_List_Member (Freeze_Node (Act_Id))
      then
         Ensure_Freeze_Node (Act_Id);
         F_Node := Freeze_Node (Act_Id);

         if Must_Delay then
            Insert_After (Orig_Body, F_Node);

         elsif Is_Generic_Instance (Par_Id)
           and then Present (Freeze_Node (Par_Id))
           and then Scope (Act_Id) /= Par_Id
         then
            --  Freeze instance of inner generic after instance of enclosing
            --  generic.

            if In_Same_Declarative_Part (Parent (Freeze_Node (Par_Id)), N) then

               --  Handle the following case:

               --    package Parent_Inst is new ...
               --    freeze Parent_Inst []

               --    procedure P ...  --  this body freezes Parent_Inst

               --    package Inst is new ...

               --  In this particular scenario, the freeze node for Inst must
               --  be inserted in the same manner as that of Parent_Inst,
               --  before the next source body or at the end of the declarative
               --  list (body not available). If body P did not exist and
               --  Parent_Inst was frozen after Inst, either by a body
               --  following Inst or at the end of the declarative region,
               --  the freeze node for Inst must be inserted after that of
               --  Parent_Inst. This relation is established by comparing
               --  the Slocs of Parent_Inst freeze node and Inst.
               --  We examine the parents of the enclosing lists to handle
               --  the case where the parent instance is in the visible part
               --  of a package declaration, and the inner instance is in
               --  the corresponding private part.

               if Parent (List_Containing (Freeze_Node (Par_Id)))
                    = Parent (List_Containing (N))
                 and then Sloc (Freeze_Node (Par_Id)) <= Sloc (N)
               then
                  Insert_Freeze_Node_For_Instance (N, F_Node);
               else
                  Insert_After (Freeze_Node (Par_Id), F_Node);
               end if;

            --  Freeze package enclosing instance of inner generic after
            --  instance of enclosing generic.

            elsif Nkind (Parent (N)) in N_Package_Body | N_Subprogram_Body
              and then In_Same_Declarative_Part
                         (Parent (Freeze_Node (Par_Id)), Parent (N))
            then
               declare
                  Enclosing :  Entity_Id;

               begin
                  Enclosing := Corresponding_Spec (Parent (N));

                  if No (Enclosing) then
                     Enclosing := Defining_Entity (Parent (N));
                  end if;

                  Insert_Freeze_Node_For_Instance (N, F_Node);
                  Ensure_Freeze_Node (Enclosing);

                  if not Is_List_Member (Freeze_Node (Enclosing)) then

                     --  The enclosing context is a subunit, insert the freeze
                     --  node after the stub.

                     if Nkind (Parent (Parent (N))) = N_Subunit then
                        Insert_Freeze_Node_For_Instance
                          (Corresponding_Stub (Parent (Parent (N))),
                           Freeze_Node (Enclosing));

                     --  The enclosing context is a package with a stub body
                     --  which has already been replaced by the real body.
                     --  Insert the freeze node after the actual body.

                     elsif Ekind (Enclosing) = E_Package
                       and then Present (Body_Entity (Enclosing))
                       and then Was_Originally_Stub
                                  (Parent (Body_Entity (Enclosing)))
                     then
                        Insert_Freeze_Node_For_Instance
                          (Parent (Body_Entity (Enclosing)),
                           Freeze_Node (Enclosing));

                     --  The parent instance has been frozen before the body of
                     --  the enclosing package, insert the freeze node after
                     --  the body.

                     elsif In_Same_List (Freeze_Node (Par_Id), Parent (N))
                       and then
                         Sloc (Freeze_Node (Par_Id)) <= Sloc (Parent (N))
                     then
                        Insert_Freeze_Node_For_Instance
                          (Parent (N), Freeze_Node (Enclosing));

                     else
                        Insert_After
                          (Freeze_Node (Par_Id), Freeze_Node (Enclosing));
                     end if;
                  end if;
               end;

            else
               Insert_Freeze_Node_For_Instance (N, F_Node);
            end if;

         else
            Insert_Freeze_Node_For_Instance (N, F_Node);
         end if;
      end if;
   end Freeze_Package_Instance;

   --------------------------------
   -- Freeze_Subprogram_Instance --
   --------------------------------

   procedure Freeze_Subprogram_Instance
     (N         : Node_Id;
      Gen_Body  : Node_Id;
      Pack_Id   : Entity_Id)
  is
      function Enclosing_Package_Body (N : Node_Id) return Node_Id;
      --  Find innermost package body that encloses the given node, and which
      --  is not a compilation unit. Freeze nodes for the instance, or for its
      --  enclosing body, may be inserted after the enclosing_body of the
      --  generic unit. Used to determine proper placement of freeze node for
      --  both package and subprogram instances.

      function Package_Freeze_Node (B : Node_Id) return Node_Id;
      --  Find entity for given package body, and locate or create a freeze
      --  node for it.

      ----------------------------
      -- Enclosing_Package_Body --
      ----------------------------

      function Enclosing_Package_Body (N : Node_Id) return Node_Id is
         P : Node_Id;

      begin
         P := Parent (N);
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
      end Enclosing_Package_Body;

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

      --  Local variables

      Enc_G  : constant Node_Id   := Enclosing_Package_Body (Gen_Body);
      Enc_N  : constant Node_Id   := Enclosing_Package_Body (N);
      Par_Id : constant Entity_Id := Scope (Get_Generic_Entity (N));

      Enc_G_F  : Node_Id;
      F_Node   : Node_Id;

   --  Start of processing for Freeze_Subprogram_Instance

   begin
      --  If the instance and the generic body appear within the same unit, and
      --  the instance precedes the generic, the freeze node for the instance
      --  must appear after that of the generic. If the generic is nested
      --  within another instance I2, then current instance must be frozen
      --  after I2. In both cases, the freeze nodes are those of enclosing
      --  packages. Otherwise, the freeze node is placed at the end of the
      --  current declarative part.

      Ensure_Freeze_Node (Pack_Id);
      F_Node := Freeze_Node (Pack_Id);

      if Is_Generic_Instance (Par_Id)
        and then Present (Freeze_Node (Par_Id))
        and then In_Same_Declarative_Part (Parent (Freeze_Node (Par_Id)), N)
      then
         --  The parent was a premature instantiation. Insert freeze node at
         --  the end the current declarative part.

         if Is_Known_Guaranteed_ABE (Get_Unit_Instantiation_Node (Par_Id)) then
            Insert_Freeze_Node_For_Instance (N, F_Node);

         --  Handle the following case:
         --
         --    package Parent_Inst is new ...
         --    freeze Parent_Inst []
         --
         --    procedure P ...  --  this body freezes Parent_Inst
         --
         --    procedure Inst is new ...
         --
         --  In this particular scenario, the freeze node for Inst must be
         --  inserted in the same manner as that of Parent_Inst - before the
         --  next source body or at the end of the declarative list (body not
         --  available). If body P did not exist and Parent_Inst was frozen
         --  after Inst, either by a body following Inst or at the end of the
         --  declarative region, the freeze node for Inst must be inserted
         --  after that of Parent_Inst. This relation is established by
         --  comparing the Slocs of Parent_Inst freeze node and Inst.

         elsif In_Same_List (Freeze_Node (Par_Id), N)
           and then Sloc (Freeze_Node (Par_Id)) <= Sloc (N)
         then
            Insert_Freeze_Node_For_Instance (N, F_Node);

         else
            Insert_After (Freeze_Node (Par_Id), F_Node);
         end if;

      --  The body enclosing the instance should be frozen after the body that
      --  includes the generic, because the body of the instance may make
      --  references to entities therein. If the two are not in the same
      --  declarative part, or if the one enclosing the instance is frozen
      --  already, freeze the instance at the end of the current declarative
      --  part.

      elsif Is_Generic_Instance (Par_Id)
        and then Present (Freeze_Node (Par_Id))
        and then Present (Enc_N)
      then
         if In_Same_Declarative_Part (Parent (Freeze_Node (Par_Id)), Enc_N)
         then
            --  The enclosing package may contain several instances. Rather
            --  than computing the earliest point at which to insert its freeze
            --  node, we place it at the end of the declarative part of the
            --  parent of the generic.

            Insert_Freeze_Node_For_Instance
              (Freeze_Node (Par_Id), Package_Freeze_Node (Enc_N));
         end if;

         Insert_Freeze_Node_For_Instance (N, F_Node);

      elsif Present (Enc_G)
        and then Present (Enc_N)
        and then Enc_G /= Enc_N
        and then Earlier (N, Gen_Body)
      then
         --  Freeze package that encloses instance, and place node after the
         --  package that encloses generic. If enclosing package is already
         --  frozen we have to assume it is at the proper place. This may be a
         --  potential ABE that requires dynamic checking. Do not add a freeze
         --  node if the package that encloses the generic is inside the body
         --  that encloses the instance, because the freeze node would be in
         --  the wrong scope. Additional contortions needed if the bodies are
         --  within a subunit.

         declare
            Enclosing_Body : Node_Id;

         begin
            if Nkind (Enc_N) = N_Package_Body_Stub then
               Enclosing_Body := Proper_Body (Unit (Library_Unit (Enc_N)));
            else
               Enclosing_Body := Enc_N;
            end if;

            if Parent (List_Containing (Enc_G)) /= Enclosing_Body then
               Insert_Freeze_Node_For_Instance
                 (Enc_G, Package_Freeze_Node (Enc_N));
            end if;
         end;

         --  Freeze enclosing subunit before instance

         Enc_G_F := Package_Freeze_Node (Enc_G);

         if not Is_List_Member (Enc_G_F) then
            Insert_After (Enc_G, Enc_G_F);
         end if;

         Insert_Freeze_Node_For_Instance (N, F_Node);

      else
         --  If none of the above, insert freeze node at the end of the current
         --  declarative part.

         Insert_Freeze_Node_For_Instance (N, F_Node);
      end if;
   end Freeze_Subprogram_Instance;

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
         --  On exit, entity is not instantiated: not a generic parameter, or
         --  else parameter of an inner generic unit.

         return A;
      end if;
   end Get_Instance_Of;

   ---------------------------------
   -- Get_Unit_Instantiation_Node --
   ---------------------------------

   function Get_Unit_Instantiation_Node (A : Entity_Id) return Node_Id is
      Decl : Node_Id := Unit_Declaration_Node (A);
      Inst : Node_Id;

   begin
      --  If the Package_Instantiation attribute has been set on the package
      --  entity, then use it directly when it (or its Original_Node) refers
      --  to an N_Package_Instantiation node. In principle it should be
      --  possible to have this field set in all cases, which should be
      --  investigated, and would allow this function to be significantly
      --  simplified. ???

      Inst := Package_Instantiation (A);

      if Present (Inst) then
         if Nkind (Inst) = N_Package_Instantiation then
            return Inst;

         elsif Nkind (Original_Node (Inst)) = N_Package_Instantiation then
            return Original_Node (Inst);
         end if;
      end if;

      --  If the instantiation is a compilation unit that does not need body
      --  then the instantiation node has been rewritten as a package
      --  declaration for the instance, and we return the original node.

      --  If it is a compilation unit and the instance node has not been
      --  rewritten, then it is still the unit of the compilation. Finally, if
      --  a body is present, this is a parent of the main unit whose body has
      --  been compiled for inlining purposes, and the instantiation node has
      --  been rewritten with the instance body.

      --  Otherwise the instantiation node appears after the declaration. If
      --  the entity is a formal package, the declaration may have been
      --  rewritten as a generic declaration (in the case of a formal with box)
      --  or left as a formal package declaration if it has actuals, and is
      --  found with a forward search.

      if Nkind (Parent (Decl)) = N_Compilation_Unit then
         if Nkind (Decl) = N_Package_Declaration
           and then Present (Corresponding_Body (Decl))
         then
            Decl := Unit_Declaration_Node (Corresponding_Body (Decl));
         end if;

         if Nkind (Original_Node (Decl)) in N_Generic_Instantiation then
            return Original_Node (Decl);
         else
            return Unit (Parent (Decl));
         end if;

      elsif Nkind (Decl) = N_Package_Declaration
        and then Nkind (Original_Node (Decl)) = N_Formal_Package_Declaration
      then
         return Original_Node (Decl);

      else
         Inst := Next (Decl);
         while Nkind (Inst) not in N_Formal_Package_Declaration
                                 | N_Function_Instantiation
                                 | N_Package_Instantiation
                                 | N_Procedure_Instantiation
         loop
            Next (Inst);
         end loop;

         return Inst;
      end if;
   end Get_Unit_Instantiation_Node;

   ------------------------
   -- Has_Been_Exchanged --
   ------------------------

   function Has_Been_Exchanged (E : Entity_Id) return Boolean is
      Next : Elmt_Id;

   begin
      Next := First_Elmt (Exchanged_Views);
      while Present (Next) loop
         if Full_View (Node (Next)) = E then
            return True;
         end if;

         Next_Elmt (Next);
      end loop;

      return False;
   end Has_Been_Exchanged;

   -------------------
   -- Has_Contracts --
   -------------------

   function Has_Contracts (Decl : Node_Id) return Boolean is
      A_List : constant List_Id := Aspect_Specifications (Decl);
      A_Spec : Node_Id;
      A_Id   : Aspect_Id;
   begin
      A_Spec := First (A_List);
      while Present (A_Spec) loop
         A_Id := Get_Aspect_Id (A_Spec);
         if A_Id = Aspect_Pre or else A_Id = Aspect_Post then
            return True;
         end if;

         Next (A_Spec);
      end loop;

      return False;
   end Has_Contracts;

   -------------------------------
   -- Has_Fully_Defined_Profile --
   -------------------------------

   function Has_Fully_Defined_Profile (Subp : Entity_Id) return Boolean is
      function Is_Fully_Defined_Type (Typ : Entity_Id) return Boolean;
      --  Determine whethet type Typ is fully defined

      ---------------------------
      -- Is_Fully_Defined_Type --
      ---------------------------

      function Is_Fully_Defined_Type (Typ : Entity_Id) return Boolean is
      begin
         --  A private type without a full view is not fully defined

         if Is_Private_Type (Typ)
           and then No (Full_View (Typ))
         then
            return False;

         --  An incomplete type is never fully defined

         elsif Is_Incomplete_Type (Typ) then
            return False;

         --  All other types are fully defined

         else
            return True;
         end if;
      end Is_Fully_Defined_Type;

      --  Local declarations

      Param : Entity_Id;

   --  Start of processing for Has_Fully_Defined_Profile

   begin
      --  Check the parameters

      Param := First_Formal (Subp);
      while Present (Param) loop
         if not Is_Fully_Defined_Type (Etype (Param)) then
            return False;
         end if;

         Next_Formal (Param);
      end loop;

      --  Check the return type

      return Is_Fully_Defined_Type (Etype (Subp));
   end Has_Fully_Defined_Profile;

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

      --  Make the scope name invisible as well. This is necessary, but might
      --  conflict with calls to Rtsfind later on, in case the scope is a
      --  predefined one. There is no clean solution to this problem, so for
      --  now we depend on the user not redefining Standard itself in one of
      --  the parent units.

      if Is_Immediately_Visible (C) and then C /= Standard_Standard then
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
      Saved.Instantiated_Parent  := Current_Instantiated_Parent;
      Saved.Exchanged_Views      := Exchanged_Views;
      Saved.Hidden_Entities      := Hidden_Entities;
      Saved.Current_Sem_Unit     := Current_Sem_Unit;
      Saved.Parent_Unit_Visible  := Parent_Unit_Visible;
      Saved.Instance_Parent_Unit := Instance_Parent_Unit;

      --  Save configuration switches. These may be reset if the unit is a
      --  predefined unit, and the current mode is not Ada 2005.

      Saved.Switches := Save_Config_Switches;

      Instance_Envs.Append (Saved);

      Exchanged_Views := New_Elmt_List;
      Hidden_Entities := New_Elmt_List;

      --  Make dummy entry for Instantiated parent. If generic unit is legal,
      --  this is set properly in Set_Instance_Env.

      Current_Instantiated_Parent :=
        (Current_Scope, Current_Scope, Assoc_Null);
   end Init_Env;

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

            if (Is_Entity_Name (Nam) and then Chars (Nam) = Chars (E))
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

      Clause   : Node_Id;
      OK       : Boolean;
      Lib_Unit : Node_Id;

   begin
      if Nkind (Parent (Gen_Decl)) = N_Compilation_Unit then

         --  The inherited context is attached to the enclosing compilation
         --  unit. This is either the main unit, or the declaration for the
         --  main unit (in case the instantiation appears within the package
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
               Lib_Unit := Library_Unit (Item);

               --  Take care to prevent direct cyclic with's

               if Lib_Unit /= Current_Unit then

                  --  Do not add a unit if it is already in the context

                  Clause := First (Current_Context);
                  OK := True;
                  while Present (Clause) loop
                     if Nkind (Clause) = N_With_Clause
                       and then Library_Unit (Clause) = Lib_Unit
                     then
                        OK := False;
                        exit;
                     end if;

                     Next (Clause);
                  end loop;

                  if OK then
                     New_I := New_Copy (Item);
                     Set_Is_Implicit_With (New_I);

                     Append (New_I, Current_Context);
                  end if;
               end if;
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

   -------------------------------------
   -- Insert_Freeze_Node_For_Instance --
   -------------------------------------

   procedure Insert_Freeze_Node_For_Instance
     (N      : Node_Id;
      F_Node : Node_Id)
   is
      function Enclosing_Body (N : Node_Id) return Node_Id;
      --  Find enclosing package or subprogram body, if any. Freeze node may
      --  be placed at end of current declarative list if previous instance
      --  and current one have different enclosing bodies.

      function Previous_Instance (Gen : Entity_Id) return Entity_Id;
      --  Find the local instance, if any, that declares the generic that is
      --  being instantiated. If present, the freeze node for this instance
      --  must follow the freeze node for the previous instance.

      --------------------
      -- Enclosing_Body --
      --------------------

      function Enclosing_Body (N : Node_Id) return Node_Id is
         P : Node_Id;

      begin
         P := Parent (N);
         while Present (P)
           and then Nkind (Parent (P)) /= N_Compilation_Unit
         loop
            if Nkind (P) in N_Package_Body | N_Subprogram_Body then
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

      -----------------------
      -- Previous_Instance --
      -----------------------

      function Previous_Instance (Gen : Entity_Id) return Entity_Id is
         S : Entity_Id;

      begin
         S := Scope (Gen);
         while Present (S) and then S /= Standard_Standard loop
            if Is_Generic_Instance (S)
              and then In_Same_Source_Unit (S, N)
            then
               return S;
            end if;

            S := Scope (S);
         end loop;

         return Empty;
      end Previous_Instance;

      --  Local variables

      Decl     : Node_Id;
      Decls    : List_Id;
      Inst     : Entity_Id;
      Origin   : Entity_Id;
      Par_Inst : Node_Id;
      Par_N    : Node_Id;

   --  Start of processing for Insert_Freeze_Node_For_Instance

   begin
      --  Nothing to do if the freeze node has already been inserted

      if Is_List_Member (F_Node) then
         return;
      end if;

      Inst := Entity (F_Node);

      --  When processing a subprogram instantiation, utilize the actual
      --  subprogram instantiation rather than its package wrapper as it
      --  carries all the context information.

      if Is_Wrapper_Package (Inst) then
         Inst := Related_Instance (Inst);
      end if;

      Par_Inst := Parent (Inst);

      --  If this is a package instance, check whether the generic is declared
      --  in a previous instance and the current instance is not within the
      --  previous one.

      if Present (Generic_Parent (Par_Inst)) and then Is_In_Main_Unit (N) then
         declare
            Enclosing_N : constant Node_Id   := Enclosing_Body (N);
            Par_I       : constant Entity_Id :=
                            Previous_Instance (Generic_Parent (Par_Inst));
            Scop        : Entity_Id;

         begin
            if Present (Par_I) and then Earlier (N, Freeze_Node (Par_I)) then
               Scop := Scope (Inst);

               --  If the current instance is within the one that contains
               --  the generic, the freeze node for the current one must
               --  appear in the current declarative part. Ditto, if the
               --  current instance is within another package instance or
               --  within a body that does not enclose the current instance.
               --  In these three cases the freeze node of the previous
               --  instance is not relevant.

               while Present (Scop) and then Scop /= Standard_Standard loop
                  exit when Scop = Par_I
                    or else
                      (Is_Generic_Instance (Scop)
                        and then Scope_Depth (Scop) > Scope_Depth (Par_I));
                  Scop := Scope (Scop);
               end loop;

               --  Previous instance encloses current instance

               if Scop = Par_I then
                  null;

               --  If the next node is a source body we must freeze in the
               --  current scope as well.

               elsif Present (Next (N))
                 and then Nkind (Next (N)) in N_Subprogram_Body
                                            | N_Package_Body
                 and then Comes_From_Source (Next (N))
               then
                  null;

               --  Current instance is within an unrelated instance

               elsif Is_Generic_Instance (Scop) then
                  null;

               --  Current instance is within an unrelated body

               elsif Present (Enclosing_N)
                 and then Enclosing_N /= Enclosing_Body (Par_I)
               then
                  null;

               else
                  Insert_After (Freeze_Node (Par_I), F_Node);
                  return;
               end if;
            end if;
         end;
      end if;

      Decl   := N;
      Decls  := List_Containing (N);
      Par_N  := Parent (Decls);
      Origin := Empty;

      --  Determine the proper freeze point of an instantiation

      if Is_Generic_Instance (Inst) then
         loop
            --  When the instantiation occurs in a package spec, append the
            --  freeze node to the private declarations (if any).

            if Nkind (Par_N) = N_Package_Specification
              and then Decls = Visible_Declarations (Par_N)
              and then not Is_Empty_List (Private_Declarations (Par_N))
            then
               Decls := Private_Declarations (Par_N);
               Decl  := First (Decls);
            end if;

            --  We adhere to the general rule of a package or subprogram body
            --  causing freezing of anything before it in the same declarative
            --  region. In this respect, the proper freeze point of a package
            --  instantiation is before the first source body which follows, or
            --  before a stub. This ensures that entities from the instance are
            --  already frozen and therefore usable in source bodies.

            if Nkind (Par_N) /= N_Package_Declaration
              and then
                not In_Same_Source_Unit (Generic_Parent (Par_Inst), Inst)
            then
               while Present (Decl) loop
                  if ((Nkind (Decl) in N_Unit_Body
                        or else
                       Nkind (Decl) in N_Body_Stub)
                      and then Comes_From_Source (Decl))
                    or else (Present (Origin)
                              and then Nkind (Decl) in N_Generic_Instantiation
                              and then Instance_Spec (Decl) /= Origin)
                  then
                     Set_Sloc (F_Node, Sloc (Decl));
                     Insert_Before (Decl, F_Node);
                     return;
                  end if;

                  Next (Decl);
               end loop;
            end if;

            --  When the instantiation occurs in a package spec and there is
            --  no source body which follows, and the package has a body but
            --  is delayed, then insert immediately before its freeze node.

            if Nkind (Par_N) = N_Package_Specification
              and then Present (Corresponding_Body (Parent (Par_N)))
              and then Present (Freeze_Node (Defining_Entity (Par_N)))
            then
               Set_Sloc (F_Node, Sloc (Freeze_Node (Defining_Entity (Par_N))));
               Insert_Before (Freeze_Node (Defining_Entity (Par_N)), F_Node);
               return;

            --  When the instantiation occurs in a package spec and there is
            --  no source body which follows, not even of the package itself,
            --  then insert into the declaration list of the outer level, but
            --  do not jump over following instantiations in this list because
            --  they may have a body that has not materialized yet, see above.

            elsif Nkind (Par_N) = N_Package_Specification
              and then No (Corresponding_Body (Parent (Par_N)))
              and then Is_List_Member (Parent (Par_N))
            then
               Decl   := Parent (Par_N);
               Decls  := List_Containing (Decl);
               Par_N  := Parent (Decls);
               Origin := Decl;

            --  In a package declaration, or if no source body which follows
            --  and at library level, then insert at end of list.

            else
               exit;
            end if;
         end loop;
      end if;

      --  Insert and adjust the Sloc of the freeze node

      Set_Sloc (F_Node, Sloc (Last (Decls)));
      Insert_After (Last (Decls), F_Node);
   end Insert_Freeze_Node_For_Instance;

   -----------------------------
   -- Install_Formal_Packages --
   -----------------------------

   procedure Install_Formal_Packages (Par : Entity_Id) is
      E     : Entity_Id;
      Gen   : Entity_Id;
      Gen_E : Entity_Id := Empty;

   begin
      E := First_Entity (Par);

      --  If we are installing an instance parent, locate the formal packages
      --  of its generic parent.

      if Is_Generic_Instance (Par) then
         Gen   := Generic_Parent (Package_Specification (Par));
         Gen_E := First_Entity (Gen);
      end if;

      while Present (E) loop
         if Ekind (E) = E_Package
           and then Nkind (Parent (E)) = N_Package_Renaming_Declaration
         then
            --  If this is the renaming for the parent instance, done

            if Renamed_Entity (E) = Par then
               exit;

            --  The visibility of a formal of an enclosing generic is already
            --  correct.

            elsif Denotes_Formal_Package (E) then
               null;

            elsif Present (Associated_Formal_Package (E)) then
               Check_Generic_Actuals (Renamed_Entity (E), True);
               Set_Is_Hidden (E, False);

               --  Find formal package in generic unit that corresponds to
               --  (instance of) formal package in instance.

               while Present (Gen_E) and then Chars (Gen_E) /= Chars (E) loop
                  Next_Entity (Gen_E);
               end loop;

               if Present (Gen_E) then
                  Map_Formal_Package_Entities (Gen_E, E);
               end if;
            end if;
         end if;

         Next_Entity (E);

         if Present (Gen_E) then
            Next_Entity (Gen_E);
         end if;
      end loop;
   end Install_Formal_Packages;

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

      procedure Install_Noninstance_Specs (Par : Entity_Id);
      --  Install the scopes of noninstance parent units ending with Par

      procedure Install_Spec (Par : Entity_Id);
      --  The child unit is within the declarative part of the parent, so the
      --  declarations within the parent are immediately visible.

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
         Spec : constant Node_Id := Package_Specification (Par);

      begin
         --  If this parent of the child instance is a top-level unit,
         --  then record the unit and its visibility for later resetting in
         --  Remove_Parent. We exclude units that are generic instances, as we
         --  only want to record this information for the ultimate top-level
         --  noninstance parent (is that always correct???).

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
         --  from Ada.Tags may need the full view of type Tag. If inlining took
         --  proper account of establishing visibility of inlined subprograms'
         --  parents then it should be possible to remove this
         --  special check. ???

         Push_Scope (Par);
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
      --  scope. Given that we cannot place the parent above the current scope
      --  in the scope stack, we duplicate the current scope and unstack both
      --  after the instantiation is complete.

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

      Gen_Par := Generic_Parent (Package_Specification (Inst_Par));

      First_Gen := Gen_Par;

      while Present (Gen_Par) and then Is_Child_Unit (Gen_Par) loop

         --  Load grandparent instance as well

         Inst_Node := Get_Unit_Instantiation_Node (Inst_Par);

         if Nkind (Name (Inst_Node)) = N_Expanded_Name then
            Inst_Par := Entity (Prefix (Name (Inst_Node)));

            if Present (Renamed_Entity (Inst_Par)) then
               Inst_Par := Renamed_Entity (Inst_Par);
            end if;

            Gen_Par := Generic_Parent (Package_Specification (Inst_Par));

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
         Push_Scope (S);
      end if;
   end Install_Parent;

   -------------------------------
   -- Install_Hidden_Primitives --
   -------------------------------

   procedure Install_Hidden_Primitives
     (Prims_List : in out Elist_Id;
      Gen_T      : Entity_Id;
      Act_T      : Entity_Id)
   is
      Elmt        : Elmt_Id;
      List        : Elist_Id := No_Elist;
      Prim_G_Elmt : Elmt_Id;
      Prim_A_Elmt : Elmt_Id;
      Prim_G      : Node_Id;
      Prim_A      : Node_Id;

   begin
      --  No action needed in case of serious errors because we cannot trust
      --  in the order of primitives

      if Serious_Errors_Detected > 0 then
         return;

      --  No action possible if we don't have available the list of primitive
      --  operations

      elsif No (Gen_T)
        or else not Is_Record_Type (Gen_T)
        or else not Is_Tagged_Type (Gen_T)
        or else not Is_Record_Type (Act_T)
        or else not Is_Tagged_Type (Act_T)
      then
         return;

      --  There is no need to handle interface types since their primitives
      --  cannot be hidden

      elsif Is_Interface (Gen_T) then
         return;
      end if;

      Prim_G_Elmt := First_Elmt (Primitive_Operations (Gen_T));

      if not Is_Class_Wide_Type (Act_T) then
         Prim_A_Elmt := First_Elmt (Primitive_Operations (Act_T));
      else
         Prim_A_Elmt := First_Elmt (Primitive_Operations (Root_Type (Act_T)));
      end if;

      loop
         --  Skip predefined primitives in the generic formal

         while Present (Prim_G_Elmt)
           and then Is_Predefined_Dispatching_Operation (Node (Prim_G_Elmt))
         loop
            Next_Elmt (Prim_G_Elmt);
         end loop;

         --  Skip predefined primitives in the generic actual

         while Present (Prim_A_Elmt)
           and then Is_Predefined_Dispatching_Operation (Node (Prim_A_Elmt))
         loop
            Next_Elmt (Prim_A_Elmt);
         end loop;

         exit when No (Prim_G_Elmt) or else No (Prim_A_Elmt);

         Prim_G := Node (Prim_G_Elmt);
         Prim_A := Node (Prim_A_Elmt);

         --  There is no need to handle interface primitives because their
         --  primitives are not hidden

         exit when Present (Interface_Alias (Prim_G));

         --  Here we install one hidden primitive

         if Chars (Prim_G) /= Chars (Prim_A)
           and then Has_Suffix (Prim_A, 'P')
           and then Remove_Suffix (Prim_A, 'P') = Chars (Prim_G)
         then
            Set_Chars (Prim_A, Chars (Prim_G));
            Append_New_Elmt (Prim_A, To => List);
         end if;

         Next_Elmt (Prim_A_Elmt);
         Next_Elmt (Prim_G_Elmt);
      end loop;

      --  Append the elements to the list of temporarily visible primitives
      --  avoiding duplicates.

      if Present (List) then
         if No (Prims_List) then
            Prims_List := New_Elmt_List;
         end if;

         Elmt := First_Elmt (List);
         while Present (Elmt) loop
            Append_Unique_Elmt (Node (Elmt), Prims_List);
            Next_Elmt (Elmt);
         end loop;
      end if;
   end Install_Hidden_Primitives;

   ---------------------------------
   -- Renames_Standard_Subprogram --
   ---------------------------------

   function Renames_Standard_Subprogram (Subp : Entity_Id) return Boolean is
      Id : Entity_Id;

   begin
      Id := Alias (Subp);
      while Present (Id) loop
         if Scope (Id) = Standard_Standard then
            return True;
         end if;

         Id := Alias (Id);
      end loop;

      return False;
   end Renames_Standard_Subprogram;

   -------------------------------
   -- Restore_Hidden_Primitives --
   -------------------------------

   procedure Restore_Hidden_Primitives (Prims_List : in out Elist_Id) is
      Prim_Elmt : Elmt_Id;
      Prim      : Node_Id;

   begin
      if Present (Prims_List) then
         Prim_Elmt := First_Elmt (Prims_List);
         while Present (Prim_Elmt) loop
            Prim := Node (Prim_Elmt);
            Set_Chars (Prim, Add_Suffix (Prim, 'P'));
            Next_Elmt (Prim_Elmt);
         end loop;

         Prims_List := No_Elist;
      end if;
   end Restore_Hidden_Primitives;

   --------------------------------
   -- Instantiate_Formal_Package --
   --------------------------------

   function Instantiate_Formal_Package
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id) return List_Id
   is
      Loc            : constant Source_Ptr := Sloc (Actual);
      Hidden_Formals : constant Elist_Id   := New_Elmt_List;

      Actual_Pack : Entity_Id;
      Formal_Pack : Entity_Id;
      Gen_Parent  : Entity_Id;
      Decls       : List_Id;
      Nod         : Node_Id;
      Parent_Spec : Node_Id;

      procedure Find_Matching_Actual
       (F    : Node_Id;
        Act  : in out Entity_Id);
      --  We need to associate each formal entity in the formal package with
      --  the corresponding entity in the actual package. The actual package
      --  has been analyzed and possibly expanded, and as a result there is
      --  no one-to-one correspondence between the two lists (for example,
      --  the actual may include subtypes, itypes, and inherited primitive
      --  operations, interspersed among the renaming declarations for the
      --  actuals). We retrieve the corresponding actual by name because each
      --  actual has the same name as the formal, and they do appear in the
      --  same order.

      function Get_Formal_Entity (N : Node_Id) return Entity_Id;
      --  Retrieve entity of defining entity of generic formal parameter.
      --  Only the declarations of formals need to be considered when
      --  linking them to actuals, but the declarative list may include
      --  internal entities generated during analysis, and those are ignored.

      procedure Match_Formal_Entity
        (Formal_Node : Node_Id;
         Formal_Ent  : Entity_Id;
         Actual_Ent  : Entity_Id);
      --  Associates the formal entity with the actual. In the case where
      --  Formal_Ent is a formal package, this procedure iterates through all
      --  of its formals and enters associations between the actuals occurring
      --  in the formal package's corresponding actual package (given by
      --  Actual_Ent) and the formal package's formal parameters. This
      --  procedure recurses if any of the parameters is itself a package.

      function Is_Instance_Of
        (Act_Spec : Entity_Id;
         Gen_Anc  : Entity_Id) return Boolean;
      --  The actual can be an instantiation of a generic within another
      --  instance, in which case there is no direct link from it to the
      --  original generic ancestor. In that case, we recognize that the
      --  ultimate ancestor is the same by examining names and scopes.

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
            when N_Formal_Object_Declaration
               | N_Formal_Type_Declaration
            =>
               Formal_Ent := Defining_Identifier (F);

               while Present (Act)
                 and then Chars (Act) /= Chars (Formal_Ent)
               loop
                  Next_Entity (Act);
               end loop;

            when N_Formal_Package_Declaration
               | N_Formal_Subprogram_Declaration
               | N_Generic_Package_Declaration
               | N_Package_Declaration
            =>
               Formal_Ent := Defining_Entity (F);

               while Present (Act)
                 and then Chars (Act) /= Chars (Formal_Ent)
               loop
                  Next_Entity (Act);
               end loop;

            when others =>
               raise Program_Error;
         end case;
      end Find_Matching_Actual;

      -------------------------
      -- Match_Formal_Entity --
      -------------------------

      procedure Match_Formal_Entity
        (Formal_Node : Node_Id;
         Formal_Ent  : Entity_Id;
         Actual_Ent  : Entity_Id)
      is
         Act_Pkg : Entity_Id;

      begin
         Set_Instance_Of (Formal_Ent, Actual_Ent);

         if Ekind (Actual_Ent) = E_Package then

            --  Record associations for each parameter

            Act_Pkg := Actual_Ent;

            declare
               A_Ent  : Entity_Id := First_Entity (Act_Pkg);
               F_Ent  : Entity_Id;
               F_Node : Node_Id;

               Gen_Decl : Node_Id;
               Formals  : List_Id;
               Actual   : Entity_Id;

            begin
               --  Retrieve the actual given in the formal package declaration

               Actual := Entity (Name (Original_Node (Formal_Node)));

               --  The actual in the formal package declaration may be a
               --  renamed generic package, in which case we want to retrieve
               --  the original generic in order to traverse its formal part.

               if Present (Renamed_Entity (Actual)) then
                  Gen_Decl := Unit_Declaration_Node (Renamed_Entity (Actual));
               else
                  Gen_Decl := Unit_Declaration_Node (Actual);
               end if;

               Formals := Generic_Formal_Declarations (Gen_Decl);

               if Present (Formals) then
                  F_Node := First_Non_Pragma (Formals);
               else
                  F_Node := Empty;
               end if;

               while Present (A_Ent)
                 and then Present (F_Node)
                 and then A_Ent /= First_Private_Entity (Act_Pkg)
               loop
                  F_Ent := Get_Formal_Entity (F_Node);

                  if Present (F_Ent) then

                     --  This is a formal of the original package. Record
                     --  association and recurse.

                     Find_Matching_Actual (F_Node, A_Ent);
                     Match_Formal_Entity (F_Node, F_Ent, A_Ent);
                     Next_Entity (A_Ent);
                  end if;

                  Next_Non_Pragma (F_Node);
               end loop;
            end;
         end if;
      end Match_Formal_Entity;

      -----------------------
      -- Get_Formal_Entity --
      -----------------------

      function Get_Formal_Entity (N : Node_Id) return Entity_Id is
         Kind : constant Node_Kind := Nkind (Original_Node (N));
      begin
         case Kind is
            when N_Formal_Object_Declaration =>
               return Defining_Identifier (N);

            when N_Formal_Type_Declaration =>
               return Defining_Identifier (N);

            when N_Formal_Subprogram_Declaration =>
               return Defining_Unit_Name (Specification (N));

            when N_Formal_Package_Declaration =>
               return Defining_Identifier (Original_Node (N));

            when N_Generic_Package_Declaration =>
               return Defining_Identifier (Original_Node (N));

            --  All other declarations are introduced by semantic analysis and
            --  have no match in the actual.

            when others =>
               return Empty;
         end case;
      end Get_Formal_Entity;

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
               Set_Is_Visible_Formal (Ent);
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

      --  The actual must be a package instance, or else a current instance
      --  such as a parent generic within the body of a generic child.

      if not Is_Entity_Name (Actual)
        or else not Is_Package_Or_Generic_Package (Entity (Actual))
      then
         Error_Msg_N
           ("expect package instance to instantiate formal", Actual);
         Abandon_Instantiation (Actual);

      else
         Actual_Pack := Entity (Actual);
         Set_Is_Instantiated (Actual_Pack);

         --  The actual may be a renamed package, or an outer generic formal
         --  package whose instantiation is converted into a renaming.

         if Present (Renamed_Entity (Actual_Pack)) then
            Actual_Pack := Renamed_Entity (Actual_Pack);
         end if;

         --  The analyzed formal is expected to be the result of the rewriting
         --  of the formal package into a regular package by analysis.

         pragma Assert (Nkind (Analyzed_Formal) = N_Package_Declaration
           and then Nkind (Original_Node (Analyzed_Formal)) =
                                                 N_Formal_Package_Declaration);

         Gen_Parent := Generic_Parent (Specification (Analyzed_Formal));
         Formal_Pack := Defining_Unit_Name (Specification (Analyzed_Formal));

         --  The actual for a ghost generic formal package should be a ghost
         --  package (SPARK RM 6.9(14)).

         Check_Ghost_Formal_Procedure_Or_Package
           (N      => Actual,
            Actual => Actual_Pack,
            Formal => Formal_Pack);

         if Nkind (Parent (Actual_Pack)) = N_Defining_Program_Unit_Name then
            Parent_Spec := Package_Specification (Actual_Pack);
         else
            Parent_Spec := Parent (Actual_Pack);
         end if;

         if Gen_Parent = Any_Id then
            Error_Msg_N
              ("previous error in declaration of formal package", Actual);
            Abandon_Instantiation (Actual);

         elsif Is_Instance_Of (Parent_Spec, Get_Instance_Of (Gen_Parent)) then
            null;

         --  If this is the current instance of an enclosing generic, that unit
         --  is the generic package we need.

         elsif In_Open_Scopes (Actual_Pack)
           and then Ekind (Actual_Pack) = E_Generic_Package
         then
            null;

         else
            Error_Msg_NE
              ("actual parameter must be instance of&", Actual, Gen_Parent);
            Abandon_Instantiation (Actual);
         end if;

         Set_Instance_Of (Defining_Identifier (Formal), Actual_Pack);
         Map_Formal_Package_Entities (Formal_Pack, Actual_Pack);

         Nod :=
           Make_Package_Renaming_Declaration (Loc,
             Defining_Unit_Name => New_Copy (Defining_Identifier (Formal)),
             Name               => New_Occurrence_Of (Actual_Pack, Loc));

         Set_Associated_Formal_Package
           (Defining_Unit_Name (Nod), Defining_Identifier (Formal));
         Decls := New_List (Nod);

         --  If the formal F has a box, then the generic declarations are
         --  visible in the generic G. In an instance of G, the corresponding
         --  entities in the actual for F (which are the actuals for the
         --  instantiation of the generic that F denotes) must also be made
         --  visible for analysis of the current instance. On exit from the
         --  current instance, those entities are made private again. If the
         --  actual is currently in use, these entities are also use-visible.

         --  The loop through the actual entities also steps through the formal
         --  entities and enters associations from formals to actuals into the
         --  renaming map. This is necessary to properly handle checking of
         --  actual parameter associations for later formals that depend on
         --  actuals declared in the formal package.

         --  In Ada 2005, partial parameterization requires that we make
         --  visible the actuals corresponding to formals that were defaulted
         --  in the formal package. There formals are identified because they
         --  remain formal generics within the formal package, rather than
         --  being renamings of the actuals supplied.

         declare
            Gen_Decl : constant Node_Id :=
                         Unit_Declaration_Node (Gen_Parent);
            Formals  : constant List_Id :=
                         Generic_Formal_Declarations (Gen_Decl);

            Actual_Ent       : Entity_Id;
            Actual_Of_Formal : Node_Id;
            Formal_Node      : Node_Id;
            Formal_Ent       : Entity_Id;

         begin
            if Present (Formals) then
               Formal_Node := First_Non_Pragma (Formals);
            else
               Formal_Node := Empty;
            end if;

            Actual_Ent := First_Entity (Actual_Pack);
            Actual_Of_Formal :=
              First (Visible_Declarations (Specification (Analyzed_Formal)));
            while Present (Actual_Ent)
              and then Actual_Ent /= First_Private_Entity (Actual_Pack)
            loop
               if Present (Formal_Node) then
                  Formal_Ent := Get_Formal_Entity (Formal_Node);

                  if Present (Formal_Ent) then
                     Find_Matching_Actual (Formal_Node, Actual_Ent);
                     Match_Formal_Entity (Formal_Node, Formal_Ent, Actual_Ent);

                     --  We iterate at the same time over the actuals of the
                     --  local package created for the formal, to determine
                     --  which one of the formals of the original generic were
                     --  defaulted in the formal. The corresponding actual
                     --  entities are visible in the enclosing instance.

                     if Box_Present (Formal)
                       or else
                         (Present (Actual_Of_Formal)
                           and then
                             Is_Generic_Formal
                               (Get_Formal_Entity (Actual_Of_Formal)))
                     then
                        Set_Is_Hidden (Actual_Ent, False);
                        Set_Is_Visible_Formal (Actual_Ent);
                        Set_Is_Potentially_Use_Visible
                          (Actual_Ent, In_Use (Actual_Pack));

                        if Ekind (Actual_Ent) = E_Package then
                           Process_Nested_Formal (Actual_Ent);
                        end if;

                     else
                        if not Is_Hidden (Actual_Ent) then
                           Append_Elmt (Actual_Ent, Hidden_Formals);
                        end if;

                        Set_Is_Hidden (Actual_Ent);
                        Set_Is_Potentially_Use_Visible (Actual_Ent, False);
                     end if;
                  end if;

                  Next_Non_Pragma (Formal_Node);
                  Next (Actual_Of_Formal);

                  --  A formal subprogram may be overloaded, so advance in
                  --  the list of actuals to make sure we do not match two
                  --  successive formals to the same actual. This is only
                  --  relevant for overloadable entities, others have
                  --  distinct names.

                  if Is_Overloadable (Actual_Ent) then
                     Next_Entity (Actual_Ent);
                  end if;

               else
                  --  No further formals to match, but the generic part may
                  --  contain inherited operation that are not hidden in the
                  --  enclosing instance.

                  Next_Entity (Actual_Ent);
               end if;
            end loop;

            --  Inherited subprograms generated by formal derived types are
            --  also visible if the types are.

            Actual_Ent := First_Entity (Actual_Pack);
            while Present (Actual_Ent)
              and then Actual_Ent /= First_Private_Entity (Actual_Pack)
            loop
               if Is_Overloadable (Actual_Ent)
                 and then
                   Nkind (Parent (Actual_Ent)) = N_Subtype_Declaration
                 and then
                   not Is_Hidden (Defining_Identifier (Parent (Actual_Ent)))
               then
                  Set_Is_Hidden (Actual_Ent, False);
                  Set_Is_Potentially_Use_Visible
                    (Actual_Ent, In_Use (Actual_Pack));
               end if;

               Next_Entity (Actual_Ent);
            end loop;
         end;

         --  If the formal requires conformance checking, reanalyze it as an
         --  abbreviated instantiation, to verify the matching rules of 12.7.
         --  The actual checks are performed after the generic associations
         --  have been analyzed, to guarantee the same visibility for this
         --  instantiation and for the actuals.

         --  In Ada 2005, the generic associations for the formal can include
         --  defaulted parameters. These are ignored during check. This
         --  internal instantiation is removed from the tree after conformance
         --  checking, because it contains formal declarations for those
         --  defaulted parameters, and those should not reach the back-end.

         if Requires_Conformance_Checking (Formal) then
            declare
               I_Pack : constant Entity_Id := Make_Temporary (Loc, 'P');
               I_Nam : Node_Id;
            begin
               Set_Is_Internal (I_Pack);
               Mutate_Ekind (I_Pack, E_Package);

               --  Insert the package into the list of its hidden entities so
               --  that the list is not empty for Is_Abbreviated_Instance.

               Append_Elmt (I_Pack, Hidden_Formals);

               Set_Hidden_In_Formal_Instance (I_Pack, Hidden_Formals);

               --  If the generic is a child unit, Check_Generic_Child_Unit
               --  needs its original name in case it is qualified.

               if Is_Child_Unit (Gen_Parent) then
                  I_Nam :=
                    New_Copy_Tree (Name (Original_Node (Analyzed_Formal)));
                  pragma Assert (Entity (I_Nam) = Gen_Parent
                    or else Renamed_Entity (Entity (I_Nam)) = Gen_Parent);

               else
                  I_Nam :=
                    New_Occurrence_Of (Get_Instance_Of (Gen_Parent), Loc);
               end if;

               Append_To (Decls,
                 Make_Package_Instantiation (Loc,
                   Defining_Unit_Name   => I_Pack,
                   Name                 => I_Nam,
                   Generic_Associations => Generic_Associations (Formal)));
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
      Analyzed_S : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Analyzed_Formal));
      Formal_Sub : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Formal));

      function From_Parent_Scope (Subp : Entity_Id) return Boolean;
      --  If the generic is a child unit, the parent has been installed on the
      --  scope stack, but a default subprogram cannot resolve to something
      --  on the parent because that parent is not really part of the visible
      --  context (it is there to resolve explicit local entities). If the
      --  default has resolved in this way, we remove the entity from immediate
      --  visibility and analyze the node again to emit an error message or
      --  find another visible candidate.

      procedure Valid_Actual_Subprogram (Act : Node_Id);
      --  Perform legality check and raise exception on failure

      -----------------------
      -- From_Parent_Scope --
      -----------------------

      function From_Parent_Scope (Subp : Entity_Id) return Boolean is
         Gen_Scope : Node_Id;

      begin
         Gen_Scope := Scope (Analyzed_S);
         while Present (Gen_Scope) and then Is_Child_Unit (Gen_Scope) loop
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
         Act_E : Entity_Id;

      begin
         if Is_Entity_Name (Act) then
            Act_E := Entity (Act);

         elsif Nkind (Act) = N_Selected_Component
           and then Is_Entity_Name (Selector_Name (Act))
         then
            Act_E := Entity (Selector_Name (Act));

         else
            Act_E := Empty;
         end if;

         --  The actual for a ghost generic formal procedure should be a ghost
         --  procedure (SPARK RM 6.9(14)).

         if Present (Act_E)
           and then Ekind (Act_E) = E_Procedure
         then
            Check_Ghost_Formal_Procedure_Or_Package
              (N      => Act,
               Actual => Act_E,
               Formal => Analyzed_S);
         end if;

         if (Present (Act_E) and then Is_Overloadable (Act_E))
           or else Nkind (Act) in N_Attribute_Reference
                                | N_Indexed_Component
                                | N_Character_Literal
                                | N_Explicit_Dereference
         then
            return;
         end if;

         Error_Msg_NE
           ("expect subprogram or entry name in instantiation of &",
            Instantiation_Node, Formal_Sub);
         Abandon_Instantiation (Instantiation_Node);
      end Valid_Actual_Subprogram;

      --  Local variables

      Decl_Node  : Node_Id;
      Loc        : Source_Ptr;
      Nam        : Node_Id;
      New_Spec   : Node_Id;
      New_Subp   : Entity_Id;

   --  Start of processing for Instantiate_Formal_Subprogram

   begin
      New_Spec := New_Copy_Tree (Specification (Formal));

      --  The tree copy has created the proper instantiation sloc for the
      --  new specification. Use this location for all other constructed
      --  declarations.

      Loc := Sloc (Defining_Unit_Name (New_Spec));

      --  Create new entity for the actual (New_Copy_Tree does not), and
      --  indicate that it is an actual.

      --  If the actual is not an entity (i.e. an attribute reference)
      --  and the formal includes aspect specifications for contracts,
      --  we create an internal name for the renaming declaration. The
      --  constructed wrapper contains a call to the entity in the renaming.
      --  This is an expansion activity, as is the wrapper creation.

      if Ada_Version >= Ada_2022
        and then Has_Contracts (Analyzed_Formal)
        and then not Is_Entity_Name (Actual)
        and then Expander_Active
      then
         New_Subp := Make_Temporary (Sloc (Actual), 'S');
      else
         New_Subp := Make_Defining_Identifier (Loc, Chars (Formal_Sub));
      end if;

      Mutate_Ekind (New_Subp, Ekind (Analyzed_S));
      Set_Is_Generic_Actual_Subprogram (New_Subp);
      Set_Defining_Unit_Name (New_Spec, New_Subp);

      --  Create new entities for the each of the formals in the specification
      --  of the renaming declaration built for the actual.

      if Present (Parameter_Specifications (New_Spec)) then
         declare
            F    : Node_Id;
            F_Id : Entity_Id;

         begin
            F := First (Parameter_Specifications (New_Spec));
            while Present (F) loop
               F_Id := Defining_Identifier (F);

               Set_Defining_Identifier (F,
                  Make_Defining_Identifier (Sloc (F_Id), Chars (F_Id)));
               Next (F);
            end loop;
         end;
      end if;

      --  Find entity of actual. If the actual is an attribute reference, it
      --  cannot be resolved here (its formal is missing) but is handled
      --  instead in Attribute_Renaming. If the actual is overloaded, it is
      --  fully resolved subsequently, when the renaming declaration for the
      --  formal is analyzed. If it is an explicit dereference, resolve the
      --  prefix but not the actual itself, to prevent interpretation as call.

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
         if Nkind (Default_Name (Formal)) not in N_Attribute_Reference
                                               | N_Selected_Component
                                               | N_Indexed_Component
                                               | N_Character_Literal
           and then Present (Entity (Default_Name (Formal)))
         then
            Nam := New_Occurrence_Of (Entity (Default_Name (Formal)), Loc);
         else
            Nam := New_Copy (Default_Name (Formal));
            Set_Sloc (Nam, Loc);
         end if;

      elsif Box_Present (Formal) then

         --  Actual is resolved at the point of instantiation. Create an
         --  identifier or operator with the same name as the formal.

         if Nkind (Formal_Sub) = N_Defining_Operator_Symbol then
            Nam :=
              Make_Operator_Symbol (Loc,
                Chars  => Chars (Formal_Sub),
                Strval => No_String);
         else
            Nam := Make_Identifier (Loc, Chars (Formal_Sub));
         end if;

      elsif Has_Null_Default (Formal) then
         --  Generate null body for procedure, for use in the instance

         Decl_Node :=
           Make_Subprogram_Body (Loc,
             Specification              => New_Spec,
             Declarations               => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Make_Null_Statement (Loc))));

         --  RM 12.6 (16.2/2): The procedure has convention Intrinsic

         Set_Convention (Defining_Unit_Name (New_Spec), Convention_Intrinsic);

         Copy_Ghost_Aspect (Formal, To => Decl_Node);

         --  Eliminate the calls to it when optimization is enabled

         Set_Is_Inlined (Defining_Unit_Name (New_Spec));
         return Decl_Node;

      --  Handle case of a formal function with an expression default (allowed
      --  when extensions are enabled).

      elsif Nkind (Specification (Formal)) = N_Function_Specification
        and then Present (Expression (Formal))
      then
         --  Generate body for function, for use in the instance

         declare
            Expr : constant Node_Id := New_Copy (Expression (Formal));
            Stmt : constant Node_Id := Make_Simple_Return_Statement (Loc);
         begin
            Set_Sloc (Expr, Loc);
            Set_Expression (Stmt, Expr);

            Decl_Node :=
              Make_Subprogram_Body (Loc,
                Specification              => New_Spec,
                Declarations               => New_List,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Stmt)));
         end;

         --  RM 12.6 (16.2/2): Like a null procedure default, the function
         --  has convention Intrinsic.

         Set_Convention (Defining_Unit_Name (New_Spec), Convention_Intrinsic);

         --  Inline calls to it when optimization is enabled

         Set_Is_Inlined (Defining_Unit_Name (New_Spec));
         return Decl_Node;

      else
         pragma Assert (False);
      end if;

      Decl_Node :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => New_Spec,
          Name          => Nam);

      --  If we do not have an actual and the formal specified <> then set to
      --  get proper default.

      if No (Actual) and then Box_Present (Formal) then
         Set_From_Default (Decl_Node);
      end if;

      --  Gather possible interpretations for the actual before analyzing the
      --  instance. If overloaded, it will be resolved when analyzing the
      --  renaming declaration.

      if Box_Present (Formal) and then No (Actual) then
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

      --  The generic instantiation freezes the actual. This can only be done
      --  once the actual is resolved, in the analysis of the renaming
      --  declaration. To make the formal subprogram entity available, we set
      --  Corresponding_Formal_Spec to point to the formal subprogram entity.
      --  This is also needed in Analyze_Subprogram_Renaming for the processing
      --  of formal abstract subprograms.

      Set_Corresponding_Formal_Spec (Decl_Node, Analyzed_S);

      --  We cannot analyze the renaming declaration, and thus find the actual,
      --  until all the actuals are assembled in the instance. For subsequent
      --  checks of other actuals, indicate the node that will hold the
      --  instance of this formal.

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
            Anon_Id : constant Entity_Id := Make_Temporary (Loc, 'E');

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
      Gen_Obj     : constant Entity_Id  := Defining_Identifier (Formal);
      A_Gen_Obj   : constant Entity_Id  :=
                      Defining_Identifier (Analyzed_Formal);
      Acc_Def     : Node_Id             := Empty;
      Act_Assoc   : constant Node_Id    :=
        (if No (Actual) then Empty else Parent (Actual));
      Actual_Decl : Node_Id             := Empty;
      Decl_Node   : Node_Id;
      Def         : Node_Id;
      Ftyp        : Entity_Id;
      List        : constant List_Id    := New_List;
      Loc         : constant Source_Ptr := Sloc (Actual);
      Orig_Ftyp   : constant Entity_Id  := Etype (A_Gen_Obj);
      Subt_Decl   : Node_Id             := Empty;
      Subt_Mark   : Node_Id             := Empty;

   --  Start of processing for Instantiate_Object

   begin
      --  Formal may be an anonymous access

      if Present (Subtype_Mark (Formal)) then
         Subt_Mark := Subtype_Mark (Formal);
      else
         Check_Access_Definition (Formal);
         Acc_Def := Access_Definition (Formal);
      end if;

      Set_Parent (List, Act_Assoc);

      --  OUT present

      if Out_Present (Formal) then

         --  An IN OUT generic actual must be a name. The instantiation is a
         --  renaming declaration. The actual is the name being renamed. We
         --  use the actual directly, rather than a copy, because it is not
         --  used further in the list of actuals, and because a copy or a use
         --  of Relocate_Node is incorrect if the instance is nested within a
         --  generic. In order to simplify e.g. ASIS queries, the
         --  Generic_Parent field links the declaration to the generic
         --  association.

         if Present (Subt_Mark) then
            Decl_Node :=
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => New_Copy (Gen_Obj),
                Subtype_Mark        => New_Copy_Tree (Subt_Mark),
                Name                => Actual);

         else pragma Assert (Present (Acc_Def));
            Decl_Node :=
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => New_Copy (Gen_Obj),
                Access_Definition   => New_Copy_Tree (Acc_Def),
                Name                => Actual);
         end if;

         Set_Corresponding_Generic_Association (Decl_Node, Act_Assoc);

         --  The analysis of the actual may produce Insert_Action nodes, so
         --  the declaration must have a context in which to attach them.

         Append (Decl_Node, List);
         Analyze (Actual);

         --  Return if the analysis of the actual reported some error

         if Etype (Actual) = Any_Type then
            return List;
         end if;

         --  This check is performed here because Analyze_Object_Renaming will
         --  not check it when Comes_From_Source is False. Note though that the
         --  check for the actual being the name of an object will be performed
         --  in Analyze_Object_Renaming.

         if Is_Object_Reference (Actual)
           and then Is_Dependent_Component_Of_Mutable_Object (Actual)
         then
            Error_Msg_N
              ("illegal discriminant-dependent component for in out parameter",
               Actual);
         elsif Depends_On_Mutably_Tagged_Ext_Comp (Actual) then
            Error_Msg_N
              ("illegal mutably tagged component for in out parameter",
               Actual);
         end if;

         --  The actual has to be resolved in order to check that it is a
         --  variable (due to cases such as F (1), where F returns access to
         --  an array, and for overloaded prefixes).

         Ftyp := Get_Instance_Of (Etype (A_Gen_Obj));

         --  If the type of the formal is not itself a formal, and the current
         --  unit is a child unit, the formal type must be declared in a
         --  parent, and must be retrieved by visibility.

         if Ftyp = Orig_Ftyp
           and then Is_Generic_Unit (Scope (Ftyp))
           and then Is_Child_Unit (Scope (A_Gen_Obj))
         then
            declare
               Temp : constant Node_Id :=
                        New_Copy_Tree (Subtype_Mark (Analyzed_Formal));
            begin
               Set_Entity (Temp, Empty);
               Find_Type (Temp);
               Ftyp := Entity (Temp);
            end;
         end if;

         if Is_Private_Type (Ftyp)
           and then not Is_Private_Type (Etype (Actual))
           and then (Base_Type (Full_View (Ftyp)) = Base_Type (Etype (Actual))
                      or else Base_Type (Etype (Actual)) = Ftyp)
         then
            --  If the actual has the type of the full view of the formal, or
            --  else a non-private subtype of the formal, then the visibility
            --  of the formal type has changed. Add to the actuals a subtype
            --  declaration that will force the exchange of views in the body
            --  of the instance as well.

            Subt_Decl :=
              Make_Subtype_Declaration (Loc,
                 Defining_Identifier => Make_Temporary (Loc, 'P'),
                 Subtype_Indication  => New_Occurrence_Of (Ftyp, Loc));

            Prepend (Subt_Decl, List);

            Prepend_Elmt (Full_View (Ftyp), Exchanged_Views);
            Exchange_Declarations (Ftyp);
         end if;

         Resolve (Actual, Ftyp);

         if not Denotes_Variable (Actual) then
            Error_Msg_NE ("actual for& must be a variable", Actual, Gen_Obj);

         elsif Base_Type (Ftyp) /= Base_Type (Etype (Actual)) then

            --  Ada 2005 (AI-423): For a generic formal object of mode in out,
            --  the type of the actual shall resolve to a specific anonymous
            --  access type.

            if Ada_Version < Ada_2005
              or else not Is_Anonymous_Access_Type (Base_Type (Ftyp))
              or else not Is_Anonymous_Access_Type (Base_Type (Etype (Actual)))
            then
               Error_Msg_NE
                 ("type of actual does not match type of&", Actual, Gen_Obj);
            end if;
         end if;

         Note_Possible_Modification (Actual, Sure => True);

         --  Check for instantiation with atomic/volatile/VFA object actual for
         --  nonatomic/nonvolatile/nonVFA formal (RM C.6 (12)).

         if Is_Atomic_Object (Actual) and then not Is_Atomic (Orig_Ftyp) then
            Error_Msg_NE
              ("cannot instantiate nonatomic formal & of mode in out",
               Actual, Gen_Obj);
            Error_Msg_N ("\with atomic object actual (RM C.6(12))", Actual);

         elsif Is_Volatile_Object_Ref (Actual)
           and then not Is_Volatile (Orig_Ftyp)
         then
            Error_Msg_NE
              ("cannot instantiate nonvolatile formal & of mode in out",
               Actual, Gen_Obj);
            Error_Msg_N ("\with volatile object actual (RM C.6(12))", Actual);

         elsif Is_Volatile_Full_Access_Object_Ref (Actual)
           and then not Is_Volatile_Full_Access (Orig_Ftyp)
         then
            Error_Msg_NE
              ("cannot instantiate nonfull access formal & of mode in out",
               Actual, Gen_Obj);
            Error_Msg_N
              ("\with full access object actual (RM C.6(12))", Actual);
         end if;

         --  Check for instantiation on nonatomic subcomponent of a full access
         --  object in Ada 2022 (RM C.6 (12)).

         if Ada_Version >= Ada_2022
            and then Is_Subcomponent_Of_Full_Access_Object (Actual)
            and then not Is_Atomic_Object (Actual)
         then
            Error_Msg_NE
              ("cannot instantiate formal & of mode in out with actual",
               Actual, Gen_Obj);
            Error_Msg_N
              ("\nonatomic subcomponent of full access object (RM C.6(12))",
               Actual);
         end if;

         --  The actual for a ghost generic formal IN OUT parameter should be a
         --  ghost object (SPARK RM 6.9(14)).

         Check_Ghost_Formal_Variable
           (Actual => Actual,
            Formal => A_Gen_Obj);

      --  Formal in-mode parameter

      else
         --  The instantiation of a generic formal in-mode parameter is a
         --  constant declaration. The actual is the expression for that
         --  declaration.  Its type is a full copy of the type of the
         --  formal. This may be an access to subprogram, for which we need
         --  to generate entities for the formals in the new signature.

         if Present (Actual) then
            if Present (Subt_Mark) then
               Def := New_Copy_Tree (Subt_Mark);
            else
               pragma Assert (Present (Acc_Def));
               Def := New_Copy_Tree (Acc_Def);
            end if;

            Decl_Node :=
              Make_Object_Declaration (Loc,
                Defining_Identifier    => New_Copy (Gen_Obj),
                Constant_Present       => True,
                Null_Exclusion_Present => Null_Exclusion_Present (Formal),
                Object_Definition      => Def,
                Expression             => Actual);

            Copy_Ghost_Aspect (Formal, To => Decl_Node);
            Set_Corresponding_Generic_Association (Decl_Node, Act_Assoc);

            --  A generic formal object of a tagged type is defined to be
            --  aliased so the new constant must also be treated as aliased.

            if Is_Tagged_Type (Etype (A_Gen_Obj)) then
               Set_Aliased_Present (Decl_Node);
            end if;

            Append (Decl_Node, List);

            --  The actual for a ghost generic formal IN parameter of
            --  access-to-variable type should be a ghost object (SPARK
            --  RM 6.9(14)).

            if Is_Access_Variable (Etype (A_Gen_Obj)) then
               Check_Ghost_Formal_Variable
                 (Actual => Actual,
                  Formal => A_Gen_Obj);
            end if;

            --  No need to repeat (pre-)analysis of some expression nodes
            --  already handled in Preanalyze_Actuals.

            if Nkind (Actual) /= N_Allocator then
               Analyze (Actual);

               --  Return if the analysis of the actual reported some error

               if Etype (Actual) = Any_Type then
                  return List;
               end if;
            end if;

            declare
               Formal_Type : constant Entity_Id := Etype (A_Gen_Obj);
               Typ         : Entity_Id;

            begin
               Typ := Get_Instance_Of (Formal_Type);

               --  If the actual appears in the current or an enclosing scope,
               --  use its type directly. This is relevant if it has an actual
               --  subtype that is distinct from its nominal one. This cannot
               --  be done in general because the type of the actual may
               --  depend on other actuals, and only be fully determined when
               --  the enclosing instance is analyzed.

               if Present (Etype (Actual))
                 and then Is_Constr_Subt_For_U_Nominal (Etype (Actual))
               then
                  Freeze_Before (Instantiation_Node, Etype (Actual));
               else
                  Freeze_Before (Instantiation_Node, Typ);
               end if;

               --  If the actual is an aggregate, perform name resolution on
               --  its components (the analysis of an aggregate does not do it)
               --  to capture local names that may be hidden if the generic is
               --  a child unit.

               if Nkind (Actual) = N_Aggregate then
                  Preanalyze_And_Resolve (Actual, Typ);
               end if;

               if Is_Limited_Type (Typ)
                 and then not OK_For_Limited_Init (Typ, Actual)
               then
                  Error_Msg_N
                    ("initialization not allowed for limited types", Actual);
                  Explain_Limited_Type (Typ, Actual);
               end if;
            end;

         elsif Present (Default_Expression (Formal)) then

            --  Use default to construct declaration

            if Present (Subt_Mark) then
               Def := New_Copy_Tree (Subt_Mark);
            else
               pragma Assert (Present (Acc_Def));
               Def := New_Copy_Tree (Acc_Def);
            end if;

            Decl_Node :=
              Make_Object_Declaration (Sloc (Formal),
                Defining_Identifier    => New_Copy (Gen_Obj),
                Constant_Present       => True,
                Null_Exclusion_Present => Null_Exclusion_Present (Formal),
                Object_Definition      => Def,
                Expression             => New_Copy_Tree
                                            (Default_Expression (Formal)));

            Copy_Ghost_Aspect (Formal, To => Decl_Node);
            Set_Corresponding_Generic_Association
              (Decl_Node, Expression (Decl_Node));

            Append (Decl_Node, List);
            Set_Analyzed (Expression (Decl_Node), False);

         else
            pragma Assert (False);
         end if;
      end if;

      if Nkind (Actual) in N_Has_Entity
        and then Present (Entity (Actual))
      then
         Actual_Decl := Parent (Entity (Actual));
      end if;

      --  Ada 2005 (AI-423) refined by AI12-0287:
      --  For an object_renaming_declaration with a null_exclusion or an
      --  access_definition that has a null_exclusion, the subtype of the
      --  object_name shall exclude null. In addition, if the
      --  object_renaming_declaration occurs within the body of a generic unit
      --  G or within the body of a generic unit declared within the
      --  declarative region of generic unit G, then:
      --  * if the object_name statically denotes a generic formal object of
      --    mode in out of G, then the declaration of that object shall have a
      --    null_exclusion;
      --  * if the object_name statically denotes a call of a generic formal
      --    function of G, then the declaration of the result of that function
      --    shall have a null_exclusion.

      if Ada_Version >= Ada_2005
        and then Present (Actual_Decl)
        and then Nkind (Actual_Decl) in N_Formal_Object_Declaration
                                      | N_Object_Declaration
        and then Nkind (Analyzed_Formal) = N_Formal_Object_Declaration
        and then not Has_Null_Exclusion (Actual_Decl)
        and then Has_Null_Exclusion (Analyzed_Formal)
        and then Ekind (Defining_Identifier (Analyzed_Formal))
                  = E_Generic_In_Out_Parameter
        and then ((In_Generic_Scope (Entity (Actual))
                    and then In_Package_Body (Scope (Entity (Actual))))
                  or else not Can_Never_Be_Null (Etype (Actual)))
      then
         Error_Msg_Sloc := Sloc (Analyzed_Formal);
         Error_Msg_N
           ("actual must exclude null to match generic formal#", Actual);
      end if;

      return List;
   end Instantiate_Object;

   ------------------------------
   -- Instantiate_Package_Body --
   ------------------------------

   --  WARNING: This routine manages Ghost and SPARK regions. Return statements
   --  must be replaced by gotos which jump to the end of the routine in order
   --  to restore the Ghost and SPARK modes.

   procedure Instantiate_Package_Body
     (Body_Info     : Pending_Body_Info;
      Inlined_Body  : Boolean := False;
      Body_Optional : Boolean := False)
   is
      Act_Decl    : constant Node_Id    := Body_Info.Act_Decl;
      Act_Decl_Id : constant Entity_Id  := Defining_Entity (Act_Decl);
      Act_Spec    : constant Node_Id    := Specification (Act_Decl);
      Ctx_Parents : Elist_Id            := No_Elist;
      Ctx_Top     : Int                 := 0;
      Inst_Node   : constant Node_Id    := Body_Info.Inst_Node;
      Gen_Id      : constant Node_Id    := Name (Inst_Node);
      Gen_Unit    : constant Entity_Id  := Get_Generic_Entity (Inst_Node);
      Gen_Decl    : constant Node_Id    := Unit_Declaration_Node (Gen_Unit);
      Loc         : constant Source_Ptr := Sloc (Inst_Node);

      procedure Check_Initialized_Types;
      --  In a generic package body, an entity of a generic private type may
      --  appear uninitialized. This is suspicious, unless the actual is a
      --  fully initialized type.

      procedure Install_Parents_Of_Generic_Context
        (Inst_Scope  : Entity_Id;
         Ctx_Parents : out Elist_Id);
      --  Inst_Scope is the scope where the instance appears within; when it
      --  appears within a generic child package G, this routine collects and
      --  installs the enclosing packages of G in the scopes stack; installed
      --  packages are returned in Ctx_Parents.

      procedure Remove_Parents_Of_Generic_Context (Ctx_Parents : Elist_Id);
      --  Reverse effect after instantiation is complete

      -----------------------------
      -- Check_Initialized_Types --
      -----------------------------

      procedure Check_Initialized_Types is
         Decl       : Node_Id;
         Formal     : Entity_Id;
         Actual     : Entity_Id;
         Uninit_Var : Entity_Id;

      begin
         Decl := First (Generic_Formal_Declarations (Gen_Decl));
         while Present (Decl) loop
            Uninit_Var := Empty;

            if Nkind (Decl) = N_Private_Extension_Declaration then
               Uninit_Var := Uninitialized_Variable (Decl);

            elsif Nkind (Decl) = N_Formal_Type_Declaration
                    and then Nkind (Formal_Type_Definition (Decl)) =
                                          N_Formal_Private_Type_Definition
            then
               Uninit_Var :=
                 Uninitialized_Variable (Formal_Type_Definition (Decl));
            end if;

            if Present (Uninit_Var) then
               Formal := Defining_Identifier (Decl);
               Actual := First_Entity (Act_Decl_Id);

               --  For each formal there is a subtype declaration that renames
               --  the actual and has the same name as the formal. Locate the
               --  formal for warning message about uninitialized variables
               --  in the generic, for which the actual type should be a fully
               --  initialized type.

               while Present (Actual) loop
                  exit when Ekind (Actual) = E_Package
                    and then Present (Renamed_Entity (Actual));

                  if Chars (Actual) = Chars (Formal)
                    and then not Is_Scalar_Type (Actual)
                    and then not Is_Fully_Initialized_Type (Actual)
                    and then Warn_On_No_Value_Assigned
                  then
                     Error_Msg_Node_2 := Formal;
                     Error_Msg_NE
                       ("generic unit has uninitialized variable& of "
                        & "formal private type &?v?", Actual, Uninit_Var);
                     Error_Msg_NE
                       ("actual type for& should be fully initialized type?v?",
                        Actual, Formal);
                     exit;
                  end if;

                  Next_Entity (Actual);
               end loop;
            end if;

            Next (Decl);
         end loop;
      end Check_Initialized_Types;

      ----------------------------------------
      -- Install_Parents_Of_Generic_Context --
      ----------------------------------------

      procedure Install_Parents_Of_Generic_Context
        (Inst_Scope  : Entity_Id;
         Ctx_Parents : out Elist_Id)
      is
         Elmt : Elmt_Id;
         S    : Entity_Id;

      begin
         Ctx_Parents := New_Elmt_List;

         --  Collect context parents (ie. parents where the instantiation
         --  appears within).

         S := Inst_Scope;
         while S /= Standard_Standard loop
            Prepend_Elmt (S, Ctx_Parents);
            S := Scope (S);
         end loop;

         --  Install enclosing parents

         Elmt := First_Elmt (Ctx_Parents);
         while Present (Elmt) loop
            Push_Scope (Node (Elmt));
            Set_Is_Immediately_Visible (Node (Elmt));
            Next_Elmt (Elmt);
         end loop;
      end Install_Parents_Of_Generic_Context;

      ---------------------------------------
      -- Remove_Parents_Of_Generic_Context --
      ---------------------------------------

      procedure Remove_Parents_Of_Generic_Context (Ctx_Parents : Elist_Id) is
         Elmt : Elmt_Id;

      begin
         --  Traverse Ctx_Parents in LIFO order to check the removed scopes

         Elmt := Last_Elmt (Ctx_Parents);
         while Present (Elmt) loop
            pragma Assert (Current_Scope = Node (Elmt));
            Set_Is_Immediately_Visible (Current_Scope, False);
            Pop_Scope;

            Remove_Last_Elmt (Ctx_Parents);
            Elmt := Last_Elmt (Ctx_Parents);
         end loop;
      end Remove_Parents_Of_Generic_Context;

      --  Local variables

      --  The following constants capture the context prior to instantiating
      --  the package body.

      Saved_CS   : constant Config_Switches_Type     := Save_Config_Switches;
      Saved_GM   : constant Ghost_Mode_Type          := Ghost_Mode;
      Saved_IGR  : constant Node_Id                  := Ignored_Ghost_Region;
      Saved_ISMP : constant Boolean                  :=
                     Ignore_SPARK_Mode_Pragmas_In_Instance;
      Saved_LSST : constant Suppress_Stack_Entry_Ptr :=
                     Local_Suppress_Stack_Top;
      Saved_SC   : constant Boolean                  := Style_Check;
      Saved_SM   : constant SPARK_Mode_Type          := SPARK_Mode;
      Saved_SMP  : constant Node_Id                  := SPARK_Mode_Pragma;
      Saved_SS   : constant Suppress_Record          := Scope_Suppress;
      Saved_Warn : constant Warnings_State           := Save_Warnings;

      Act_Body      : Node_Id;
      Act_Body_Id   : Entity_Id;
      Act_Body_Name : Node_Id;
      Gen_Body      : Node_Id;
      Gen_Body_Id   : Node_Id;
      Par_Ent       : Entity_Id := Empty;
      Par_Installed : Boolean := False;
      Par_Vis       : Boolean   := False;

      Scope_Check_Id   : Entity_Id;
      Scope_Check_Last : Nat;
      --  Value of Current_Scope before calls to Install_Parents; used to check
      --  that scopes are correctly removed after instantiation.

      Vis_Prims_List : Elist_Id := No_Elist;
      --  List of primitives made temporarily visible in the instantiation
      --  to match the visibility of the formal type.

   --  Start of processing for Instantiate_Package_Body

   begin
      Gen_Body_Id := Corresponding_Body (Gen_Decl);

      --  The instance body may already have been processed, as the parent of
      --  another instance that is inlined (Load_Parent_Of_Generic).

      if Present (Corresponding_Body (Instance_Spec (Inst_Node))) then
         return;
      end if;

      Expander_Mode_Save_And_Set (Body_Info.Expander_Status);

      --  Re-establish the state of information on which checks are suppressed.
      --  This information was set in Body_Info at the point of instantiation,
      --  and now we restore it so that the instance is compiled using the
      --  check status at the instantiation (RM 11.5(7.2/2), AI95-00224-01).

      Local_Suppress_Stack_Top := Body_Info.Local_Suppress_Stack_Top;
      Scope_Suppress           := Body_Info.Scope_Suppress;

      Restore_Config_Switches (Body_Info.Config_Switches);
      Restore_Warnings        (Body_Info.Warnings);

      if No (Gen_Body_Id) then

         --  Do not look for parent of generic body if none is required.
         --  This may happen when the routine is called as part of the
         --  Pending_Instantiations processing, when nested instances
         --  may precede the one generated from the main unit.

         if not Unit_Requires_Body (Defining_Entity (Gen_Decl))
           and then Body_Optional
         then
            goto Leave;
         else
            Load_Parent_Of_Generic
              (Inst_Node, Specification (Gen_Decl), Body_Optional);

            --  Surprisingly enough, loading the body of the parent can cause
            --  the body to be instantiated and the double instantiation needs
            --  to be prevented in order to avoid giving bogus semantic errors.

            --  This case can occur because of the Collect_Previous_Instances
            --  machinery of Load_Parent_Of_Generic, which will instantiate
            --  bodies that are deemed to be ahead of the body of the parent
            --  in the compilation unit. But the relative position of these
            --  bodies is computed using the mere comparison of their Sloc.

            --  Now suppose that you have two generic packages G and H, with
            --  G containing a mere instantiation of H:

            --    generic
            --    package H is

            --      generic
            --      package Nested_G is
            --         ...
            --      end Nested_G;

            --    end H;

            --    with H;

            --    generic
            --    package G is

            --      package My_H is new H;

            --    end G;

            --  and a third package Q instantiating G and Nested_G:

            --    with G;

            --    package Q is

            --      package My_G is new G;

            --      package My_Nested_G is new My_G.My_H.Nested_G;

            --    end Q;

            --  The body to be instantiated is that of My_Nested_G and its
            --  parent is the instance My_G.My_H. This latter instantiation
            --  is done when My_G is analyzed, i.e. after the declarations
            --  of My_G and My_Nested_G have been parsed; as a result, the
            --  Sloc of My_G.My_H is greater than the Sloc of My_Nested_G.

            --  Therefore loading the body of My_G.My_H will cause the body
            --  of My_Nested_G to be instantiated because it is deemed to be
            --  ahead of My_G.My_H. This means that Load_Parent_Of_Generic
            --  will again be invoked on My_G.My_H, but this time with the
            --  Collect_Previous_Instances machinery disabled, so there is
            --  no endless mutual recursion and things are done in order.

            if Present (Corresponding_Body (Instance_Spec (Inst_Node))) then
               goto Leave;
            end if;

            Gen_Body_Id := Corresponding_Body (Gen_Decl);
         end if;
      end if;

      --  Establish global variable for sloc adjustment and for error recovery
      --  In the case of an instance body for an instantiation with actuals
      --  from a limited view, the instance body is placed at the beginning
      --  of the enclosing package body: use the body entity as the source
      --  location for nodes of the instance body.

      if not Is_Empty_Elmt_List (Incomplete_Actuals (Act_Decl_Id)) then
         declare
            Scop    : constant Entity_Id := Scope (Act_Decl_Id);
            Body_Id : constant Node_Id :=
                         Corresponding_Body (Unit_Declaration_Node (Scop));

         begin
            Instantiation_Node := Body_Id;
         end;
      else
         Instantiation_Node := Inst_Node;
      end if;

      --  The package being instantiated may be subject to pragma Ghost. Set
      --  the mode now to ensure that any nodes generated during instantiation
      --  are properly marked as Ghost.

      Set_Ghost_Mode (Act_Decl_Id);

      if Present (Gen_Body_Id) then
         Save_Env (Gen_Unit, Act_Decl_Id);
         Style_Check := False;

         --  If the context of the instance is subject to SPARK_Mode "off", the
         --  annotation is missing, or the body is instantiated at a later pass
         --  and its spec ignored SPARK_Mode pragma, set the global flag which
         --  signals Analyze_Pragma to ignore all SPARK_Mode pragmas within the
         --  instance.

         if SPARK_Mode /= On
           or else Ignore_SPARK_Mode_Pragmas (Act_Decl_Id)
         then
            Ignore_SPARK_Mode_Pragmas_In_Instance := True;
         end if;

         Current_Sem_Unit := Body_Info.Current_Sem_Unit;
         Gen_Body := Unit_Declaration_Node (Gen_Body_Id);

         Create_Instantiation_Source
           (Inst_Node, Gen_Body_Id, S_Adjustment);

         Act_Body :=
           Copy_Generic_Node
             (Original_Node (Gen_Body), Empty, Instantiating => True);

         --  Create proper (possibly qualified) defining name for the body, to
         --  correspond to the one in the spec.

         Act_Body_Id :=
           Make_Defining_Identifier (Sloc (Act_Decl_Id), Chars (Act_Decl_Id));
         Preserve_Comes_From_Source (Act_Body_Id, Act_Decl_Id);

         --  Some attributes of spec entity are not inherited by body entity

         Set_Handler_Records (Act_Body_Id, No_List);

         if Nkind (Defining_Unit_Name (Act_Spec)) =
                                           N_Defining_Program_Unit_Name
         then
            Act_Body_Name :=
              Make_Defining_Program_Unit_Name (Loc,
                Name                =>
                  New_Copy_Tree (Name (Defining_Unit_Name (Act_Spec))),
                Defining_Identifier => Act_Body_Id);
         else
            Act_Body_Name := Act_Body_Id;
         end if;

         Set_Defining_Unit_Name (Act_Body, Act_Body_Name);

         Set_Corresponding_Spec (Act_Body, Act_Decl_Id);
         Check_Generic_Actuals (Act_Decl_Id, False);
         Check_Initialized_Types;

         --  Install primitives hidden at the point of the instantiation but
         --  visible when processing the generic formals

         declare
            E : Entity_Id;

         begin
            E := First_Entity (Act_Decl_Id);
            while Present (E) loop
               if Is_Type (E)
                 and then not Is_Itype (E)
                 and then Is_Generic_Actual_Type (E)
                 and then Is_Tagged_Type (E)
               then
                  Install_Hidden_Primitives
                    (Prims_List => Vis_Prims_List,
                     Gen_T      => Generic_Parent_Type (Parent (E)),
                     Act_T      => E);
               end if;

               Next_Entity (E);
            end loop;
         end;

         Scope_Check_Id   := Current_Scope;
         Scope_Check_Last := Scope_Stack.Last;

         --  If the instantiation appears within a generic child some actual
         --  parameter may be the current instance of the enclosing generic
         --  parent.

         declare
            Inst_Scope : constant Entity_Id := Scope (Act_Decl_Id);

         begin
            if Is_Child_Unit (Inst_Scope)
              and then Ekind (Inst_Scope) = E_Generic_Package
              and then Present (Generic_Associations (Inst_Node))
            then
               Install_Parents_Of_Generic_Context (Inst_Scope, Ctx_Parents);

               --  Hide them from visibility; required to avoid conflicts
               --  installing the parent instance.

               if Present (Ctx_Parents) then
                  Push_Scope (Standard_Standard);
                  Ctx_Top := Scope_Stack.Last;
                  Scope_Stack.Table (Ctx_Top).Is_Active_Stack_Base := True;
               end if;
            end if;
         end;

         --  If it is a child unit, make the parent instance (which is an
         --  instance of the parent of the generic) visible.

         --  1) The child unit's parent is an explicit parent instance (the
         --  prefix of the name of the generic unit):

         --    package Child_Package is new Parent_Instance.Child_Unit;

         --  2) The child unit's parent is an implicit parent instance (e.g.
         --  when instantiating a sibling package):

         --  generic
         --  package Parent.Second_Child is
         --    ...

         --  generic
         --  package Parent.First_Child is
         --    package Sibling_Package is new Second_Child;

         --  3) The child unit's parent is not an instance, so the scope is
         --  simply the one of the unit.

         if Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Nkind (Gen_Id) = N_Expanded_Name
         then
            Par_Ent := Entity (Prefix (Gen_Id));

         elsif Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Ekind (Scope (Act_Decl_Id)) = E_Package
           and then Is_Generic_Instance (Scope (Act_Decl_Id))
           and then Nkind
             (Name (Get_Unit_Instantiation_Node
                      (Scope (Act_Decl_Id)))) = N_Expanded_Name
         then
            Par_Ent := Entity
              (Prefix (Name (Get_Unit_Instantiation_Node
                               (Scope (Act_Decl_Id)))));

         elsif Is_Child_Unit (Gen_Unit) then
            Par_Ent := Scope (Gen_Unit);
         end if;

         if Present (Par_Ent) then
            Par_Vis := Is_Immediately_Visible (Par_Ent);
            Install_Parent (Par_Ent, In_Body => True);
            Par_Installed := True;
         end if;

         --  If the instantiation is a library unit, and this is the main unit,
         --  then build the resulting compilation unit nodes for the instance.
         --  If this is a compilation unit but it is not the main unit, then it
         --  is the body of a unit in the context, that is being compiled
         --  because it is encloses some inlined unit or another generic unit
         --  being instantiated. In that case, this body is not part of the
         --  current compilation, and is not attached to the tree, but its
         --  parent must be set for analysis.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then

            --  Replace instance node with body of instance, and create new
            --  node for corresponding instance declaration.

            Build_Instance_Compilation_Unit_Nodes
              (Inst_Node, Act_Body, Act_Decl);

            --  If the instantiation appears within a generic child package
            --  enable visibility of current instance of enclosing generic
            --  parents.

            if Present (Ctx_Parents) then
               Scope_Stack.Table (Ctx_Top).Is_Active_Stack_Base := False;
               Analyze (Inst_Node);
               Scope_Stack.Table (Ctx_Top).Is_Active_Stack_Base := True;
            else
               Analyze (Inst_Node);
            end if;

            if Parent (Inst_Node) = Cunit (Main_Unit) then

               --  If the instance is a child unit itself, then set the scope
               --  of the expanded body to be the parent of the instantiation
               --  (ensuring that the fully qualified name will be generated
               --  for the elaboration subprogram).

               if Nkind (Defining_Unit_Name (Act_Spec)) =
                                                   N_Defining_Program_Unit_Name
               then
                  Set_Scope (Defining_Entity (Inst_Node), Scope (Act_Decl_Id));
               end if;
            end if;

         --  Case where instantiation is not a library unit

         else
            --  Handle the case of an instance with incomplete actual types.
            --  The instance body cannot be placed just after the declaration
            --  because full views have not been seen yet. Any use of the non-
            --  limited views in the instance body requires the presence of a
            --  regular with_clause in the enclosing unit. Therefore we place
            --  the instance body at the beginning of the enclosing body, and
            --  the freeze node for the instance is then placed after the body.

            if not Is_Empty_Elmt_List (Incomplete_Actuals (Act_Decl_Id))
              and then Ekind (Scope (Act_Decl_Id)) = E_Package
            then
               declare
                  Scop    : constant Entity_Id := Scope (Act_Decl_Id);
                  Body_Id : constant Node_Id :=
                    Corresponding_Body (Unit_Declaration_Node (Scop));

                  F_Node  : Node_Id;

               begin
                  pragma Assert (Present (Body_Id));

                  Prepend (Act_Body, Declarations (Parent (Body_Id)));

                  if Expander_Active then
                     Ensure_Freeze_Node (Act_Decl_Id);
                     F_Node := Freeze_Node (Act_Decl_Id);
                     Set_Is_Frozen (Act_Decl_Id, False);
                     if Is_List_Member (F_Node) then
                        Remove (F_Node);
                     end if;

                     Insert_After (Act_Body, F_Node);
                  end if;
               end;

            else
               Insert_Before (Inst_Node, Act_Body);
               Mark_Rewrite_Insertion (Act_Body);

               --  Insert the freeze node for the instance if need be

               if Expander_Active then
                  Freeze_Package_Instance
                    (Inst_Node, Gen_Body, Gen_Decl, Act_Decl_Id);
                  Set_Is_Frozen (Act_Decl_Id);
               end if;
            end if;

            --  If the instantiation appears within a generic child package
            --  enable visibility of current instance of enclosing generic
            --  parents.

            if Present (Ctx_Parents) then
               Scope_Stack.Table (Ctx_Top).Is_Active_Stack_Base := False;
               Analyze (Act_Body);
               Scope_Stack.Table (Ctx_Top).Is_Active_Stack_Base := True;
            else
               Analyze (Act_Body);
            end if;
         end if;

         Inherit_Context (Gen_Body, Inst_Node);

         if Par_Installed then
            Remove_Parent (In_Body => True);

            --  Restore the previous visibility of the parent

            Set_Is_Immediately_Visible (Par_Ent, Par_Vis);
         end if;

         --  Remove the parent instances if they have been placed on the scope
         --  stack to compile the body.

         if Present (Ctx_Parents) then
            pragma Assert (Scope_Stack.Last = Ctx_Top
              and then Current_Scope = Standard_Standard);
            Pop_Scope;

            Remove_Parents_Of_Generic_Context (Ctx_Parents);
         end if;

         pragma Assert (Current_Scope    = Scope_Check_Id);
         pragma Assert (Scope_Stack.Last = Scope_Check_Last);

         Restore_Hidden_Primitives (Vis_Prims_List);

         --  Restore the private views that were made visible when the body of
         --  the instantiation was created. Note that, in the case where one of
         --  these private views is declared in the parent, there is a nesting
         --  issue with the calls to Install_Parent and Remove_Parent made in
         --  between above with In_Body set to True, because these calls also
         --  want to swap and restore this private view respectively. In this
         --  case, the call to Install_Parent does nothing, but the call to
         --  Remove_Parent does restore the private view, thus undercutting the
         --  call to Restore_Private_Views. That's OK under the condition that
         --  the two mechanisms swap exactly the same entities, in particular
         --  the private entities dependent on the primary private entities.

         Restore_Private_Views (Act_Decl_Id);

         --  Remove the current unit from visibility if this is an instance
         --  that is not elaborated on the fly for inlining purposes.

         if not Inlined_Body then
            Set_Is_Immediately_Visible (Act_Decl_Id, False);
         end if;

         Restore_Env;

      --  If we have no body, and the unit requires a body, then complain. This
      --  complaint is suppressed if we have detected other errors (since a
      --  common reason for missing the body is that it had errors).
      --  In CodePeer mode, a warning has been emitted already, no need for
      --  further messages.

      elsif Unit_Requires_Body (Gen_Unit)
        and then not Body_Optional
      then
         if CodePeer_Mode then
            null;

         elsif Serious_Errors_Detected = 0 then
            Error_Msg_NE
              ("cannot find body of generic package &", Inst_Node, Gen_Unit);

         --  Don't attempt to perform any cleanup actions if some other error
         --  was already detected, since this can cause blowups.

         else
            goto Leave;
         end if;

      --  Case of package that does not need a body

      else
         --  If the instantiation of the declaration is a library unit, rewrite
         --  the original package instantiation as a package declaration in the
         --  compilation unit node.

         if Nkind (Parent (Inst_Node)) = N_Compilation_Unit then
            Set_Parent_Spec (Act_Decl, Parent_Spec (Inst_Node));
            Rewrite (Inst_Node, Act_Decl);

            --  Generate elaboration entity, in case spec has elaboration code.
            --  This cannot be done when the instance is analyzed, because it
            --  is not known yet whether the body exists.

            Set_Elaboration_Entity_Required (Act_Decl_Id, False);
            Build_Elaboration_Entity (Parent (Inst_Node), Act_Decl_Id);

         --  If the instantiation is not a library unit, then append the
         --  declaration to the list of implicitly generated entities, unless
         --  it is already a list member which means that it was already
         --  processed

         elsif not Is_List_Member (Act_Decl) then
            Mark_Rewrite_Insertion (Act_Decl);
            Insert_Before (Inst_Node, Act_Decl);
         end if;
      end if;

   <<Leave>>

      --  Restore the context that was in effect prior to instantiating the
      --  package body.

      Ignore_SPARK_Mode_Pragmas_In_Instance := Saved_ISMP;
      Local_Suppress_Stack_Top              := Saved_LSST;
      Scope_Suppress                        := Saved_SS;
      Style_Check                           := Saved_SC;

      Expander_Mode_Restore;
      Restore_Config_Switches (Saved_CS);
      Restore_Ghost_Region    (Saved_GM, Saved_IGR);
      Restore_SPARK_Mode      (Saved_SM, Saved_SMP);
      Restore_Warnings        (Saved_Warn);
   end Instantiate_Package_Body;

   ---------------------------------
   -- Instantiate_Subprogram_Body --
   ---------------------------------

   --  WARNING: This routine manages Ghost and SPARK regions. Return statements
   --  must be replaced by gotos which jump to the end of the routine in order
   --  to restore the Ghost and SPARK modes.

   procedure Instantiate_Subprogram_Body
     (Body_Info     : Pending_Body_Info;
      Body_Optional : Boolean := False)
   is
      Act_Decl    : constant Node_Id    := Body_Info.Act_Decl;
      Act_Decl_Id : constant Entity_Id  := Defining_Entity (Act_Decl);
      Inst_Node   : constant Node_Id    := Body_Info.Inst_Node;
      Gen_Id      : constant Node_Id    := Name (Inst_Node);
      Gen_Unit    : constant Entity_Id  := Get_Generic_Entity (Inst_Node);
      Gen_Decl    : constant Node_Id    := Unit_Declaration_Node (Gen_Unit);
      Loc         : constant Source_Ptr := Sloc (Inst_Node);
      Pack_Id     : constant Entity_Id  :=
                      Defining_Unit_Name (Parent (Act_Decl));

      --  The following constants capture the context prior to instantiating
      --  the subprogram body.

      Saved_CS   : constant Config_Switches_Type     := Save_Config_Switches;
      Saved_GM   : constant Ghost_Mode_Type          := Ghost_Mode;
      Saved_IGR  : constant Node_Id                  := Ignored_Ghost_Region;
      Saved_ISMP : constant Boolean                  :=
                     Ignore_SPARK_Mode_Pragmas_In_Instance;
      Saved_LSST : constant Suppress_Stack_Entry_Ptr :=
                     Local_Suppress_Stack_Top;
      Saved_SC   : constant Boolean                  := Style_Check;
      Saved_SM   : constant SPARK_Mode_Type          := SPARK_Mode;
      Saved_SMP  : constant Node_Id                  := SPARK_Mode_Pragma;
      Saved_SS   : constant Suppress_Record          := Scope_Suppress;
      Saved_Warn : constant Warnings_State           := Save_Warnings;

      Act_Body      : Node_Id;
      Act_Body_Id   : Entity_Id;
      Gen_Body      : Node_Id;
      Gen_Body_Id   : Node_Id;
      Pack_Body     : Node_Id;
      Par_Ent       : Entity_Id := Empty;
      Par_Installed : Boolean   := False;
      Par_Vis       : Boolean   := False;
      Ret_Expr      : Node_Id;

   begin
      Gen_Body_Id := Corresponding_Body (Gen_Decl);

      --  Subprogram body may have been created already because of an inline
      --  pragma, or because of multiple elaborations of the enclosing package
      --  when several instances of the subprogram appear in the main unit.

      if Present (Corresponding_Body (Act_Decl)) then
         return;
      end if;

      Expander_Mode_Save_And_Set (Body_Info.Expander_Status);

      --  Re-establish the state of information on which checks are suppressed.
      --  This information was set in Body_Info at the point of instantiation,
      --  and now we restore it so that the instance is compiled using the
      --  check status at the instantiation (RM 11.5(7.2/2), AI95-00224-01).

      Local_Suppress_Stack_Top := Body_Info.Local_Suppress_Stack_Top;
      Scope_Suppress           := Body_Info.Scope_Suppress;

      Restore_Config_Switches (Body_Info.Config_Switches);
      Restore_Warnings        (Body_Info.Warnings);

      if No (Gen_Body_Id) then

         --  For imported generic subprogram, no body to compile, complete
         --  the spec entity appropriately.

         if Is_Imported (Gen_Unit) then
            Set_Is_Imported (Act_Decl_Id);
            Set_First_Rep_Item (Act_Decl_Id, First_Rep_Item (Gen_Unit));
            Set_Interface_Name (Act_Decl_Id, Interface_Name (Gen_Unit));
            Set_Convention     (Act_Decl_Id, Convention     (Gen_Unit));
            Set_Has_Completion (Act_Decl_Id);
            goto Leave;

         --  For other cases, compile the body

         else
            Load_Parent_Of_Generic
              (Inst_Node, Specification (Gen_Decl), Body_Optional);
            Gen_Body_Id := Corresponding_Body (Gen_Decl);
         end if;
      end if;

      Instantiation_Node := Inst_Node;

      --  The subprogram being instantiated may be subject to pragma Ghost. Set
      --  the mode now to ensure that any nodes generated during instantiation
      --  are properly marked as Ghost.

      Set_Ghost_Mode (Act_Decl_Id);

      if Present (Gen_Body_Id) then
         Gen_Body := Unit_Declaration_Node (Gen_Body_Id);

         if Nkind (Gen_Body) = N_Subprogram_Body_Stub then

            --  Either body is not present, or context is non-expanding, as
            --  when compiling a subunit. Mark the instance as completed, and
            --  diagnose a missing body when needed.

            if Expander_Active
              and then Operating_Mode = Generate_Code
            then
               Error_Msg_N ("missing proper body for instantiation", Gen_Body);
            end if;

            Set_Has_Completion (Act_Decl_Id);
            goto Leave;
         end if;

         Save_Env (Gen_Unit, Act_Decl_Id);
         Style_Check := False;

         --  If the context of the instance is subject to SPARK_Mode "off", the
         --  annotation is missing, or the body is instantiated at a later pass
         --  and its spec ignored SPARK_Mode pragma, set the global flag which
         --  signals Analyze_Pragma to ignore all SPARK_Mode pragmas within the
         --  instance.

         if SPARK_Mode /= On
           or else Ignore_SPARK_Mode_Pragmas (Act_Decl_Id)
         then
            Ignore_SPARK_Mode_Pragmas_In_Instance := True;
         end if;

         --  If the context of an instance is not subject to SPARK_Mode "off",
         --  and the generic body is subject to an explicit SPARK_Mode pragma,
         --  the latter should be the one applicable to the instance.

         if not Ignore_SPARK_Mode_Pragmas_In_Instance
           and then SPARK_Mode /= Off
           and then Present (SPARK_Pragma (Gen_Body_Id))
         then
            Set_SPARK_Mode (Gen_Body_Id);
         end if;

         Current_Sem_Unit := Body_Info.Current_Sem_Unit;
         Create_Instantiation_Source
           (Inst_Node,
            Gen_Body_Id,
            S_Adjustment);

         Act_Body :=
           Copy_Generic_Node
             (Original_Node (Gen_Body), Empty, Instantiating => True);

         --  Create proper defining name for the body, to correspond to the one
         --  in the spec.

         Act_Body_Id :=
           Make_Defining_Identifier (Sloc (Act_Decl_Id), Chars (Act_Decl_Id));

         Preserve_Comes_From_Source (Act_Body_Id, Act_Decl_Id);
         Set_Defining_Unit_Name (Specification (Act_Body), Act_Body_Id);

         Set_Corresponding_Spec (Act_Body, Act_Decl_Id);
         Set_Has_Completion (Act_Decl_Id);
         Check_Generic_Actuals (Pack_Id, False);

         --  Generate a reference to link the visible subprogram instance to
         --  the generic body, which for navigation purposes is the only
         --  available source for the instance.

         Generate_Reference
           (Related_Instance (Pack_Id),
             Gen_Body_Id, 'b', Set_Ref => False, Force => True);

         --  If it is a child unit, make the parent instance (which is an
         --  instance of the parent of the generic) visible. The parent
         --  instance is the prefix of the name of the generic unit.

         if Ekind (Scope (Gen_Unit)) = E_Generic_Package
           and then Nkind (Gen_Id) = N_Expanded_Name
         then
            Par_Ent := Entity (Prefix (Gen_Id));
         elsif Is_Child_Unit (Gen_Unit) then
            Par_Ent := Scope (Gen_Unit);
         end if;

         if Present (Par_Ent) then
            Par_Vis := Is_Immediately_Visible (Par_Ent);
            Install_Parent (Par_Ent, In_Body => True);
            Par_Installed := True;
         end if;

         --  Subprogram body is placed in the body of wrapper package,
         --  whose spec contains the subprogram declaration as well as
         --  the renaming declarations for the generic parameters.

         Pack_Body :=
           Make_Package_Body (Loc,
             Defining_Unit_Name => New_Copy (Pack_Id),
             Declarations       => New_List (Act_Body));

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

            --  Insert the freeze node for the instance if need be

            if Expander_Active then
               Freeze_Subprogram_Instance (Inst_Node, Gen_Body, Pack_Id);
            end if;

            Analyze (Pack_Body);
         end if;

         Inherit_Context (Gen_Body, Inst_Node);

         Restore_Private_Views (Pack_Id, False);

         if Par_Installed then
            Remove_Parent (In_Body => True);

            --  Restore the previous visibility of the parent

            Set_Is_Immediately_Visible (Par_Ent, Par_Vis);
         end if;

         Restore_Env;

      --  Body not found. Error was emitted already. If there were no previous
      --  errors, this may be an instance whose scope is a premature instance.
      --  In that case we must insure that the (legal) program does raise
      --  program error if executed. We generate a subprogram body for this
      --  purpose.

      elsif Serious_Errors_Detected = 0
        and then Nkind (Parent (Inst_Node)) /= N_Compilation_Unit
      then
         if Body_Optional then
            goto Leave;

         elsif Ekind (Act_Decl_Id) = E_Procedure then
            Act_Body :=
              Make_Subprogram_Body (Loc,
                Specification              =>
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name       =>
                      Make_Defining_Identifier (Loc, Chars (Act_Decl_Id)),
                    Parameter_Specifications =>
                      New_Copy_List
                        (Parameter_Specifications (Parent (Act_Decl_Id)))),

                Declarations               => Empty_List,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (
                      Make_Raise_Program_Error (Loc,
                        Reason => PE_Access_Before_Elaboration))));

         else
            Ret_Expr :=
              Make_Raise_Program_Error (Loc,
                Reason => PE_Access_Before_Elaboration);

            Set_Etype (Ret_Expr, (Etype (Act_Decl_Id)));
            Set_Analyzed (Ret_Expr);

            Act_Body :=
              Make_Subprogram_Body (Loc,
                Specification =>
                  Make_Function_Specification (Loc,
                     Defining_Unit_Name       =>
                       Make_Defining_Identifier (Loc, Chars (Act_Decl_Id)),
                     Parameter_Specifications =>
                       New_Copy_List
                         (Parameter_Specifications (Parent (Act_Decl_Id))),
                     Result_Definition =>
                       New_Occurrence_Of (Etype (Act_Decl_Id), Loc)),

                  Declarations               => Empty_List,
                  Handled_Statement_Sequence =>
                    Make_Handled_Sequence_Of_Statements (Loc,
                      Statements => New_List (
                        Make_Simple_Return_Statement (Loc, Ret_Expr))));
         end if;

         Pack_Body :=
           Make_Package_Body (Loc,
             Defining_Unit_Name => New_Copy (Pack_Id),
             Declarations       => New_List (Act_Body));

         Insert_After (Inst_Node, Pack_Body);
         Set_Corresponding_Spec (Pack_Body, Pack_Id);
         Analyze (Pack_Body);
      end if;

   <<Leave>>

      --  Restore the context that was in effect prior to instantiating the
      --  subprogram body.

      Ignore_SPARK_Mode_Pragmas_In_Instance := Saved_ISMP;
      Local_Suppress_Stack_Top              := Saved_LSST;
      Scope_Suppress                        := Saved_SS;
      Style_Check                           := Saved_SC;

      Expander_Mode_Restore;
      Restore_Config_Switches (Saved_CS);
      Restore_Ghost_Region    (Saved_GM, Saved_IGR);
      Restore_SPARK_Mode      (Saved_SM, Saved_SMP);
      Restore_Warnings        (Saved_Warn);
   end Instantiate_Subprogram_Body;

   ----------------------
   -- Instantiate_Type --
   ----------------------

   function Instantiate_Type
     (Formal          : Node_Id;
      Actual          : Node_Id;
      Analyzed_Formal : Node_Id;
      Actual_Decls    : List_Id) return List_Id
   is
      A_Gen_T    : constant Entity_Id :=
                     Defining_Identifier (Analyzed_Formal);
      Def        : constant Node_Id   := Formal_Type_Definition (Formal);
      Gen_T      : constant Entity_Id := Defining_Identifier (Formal);
      Act_T      : Entity_Id;
      Ancestor   : Entity_Id := Empty;
      Decl_Node  : Node_Id;
      Decl_Nodes : List_Id; -- result
      Loc        : Source_Ptr;
      Subt       : Entity_Id;

      procedure Check_Shared_Variable_Control_Aspects;
      --  Ada 2022: Verify that shared variable control aspects (RM C.6)
      --  that may be specified for a formal type are obeyed by the actual.

      procedure Diagnose_Predicated_Actual;
      --  There are a number of constructs in which a discrete type with
      --  predicates is illegal, e.g. as an index in an array type declaration.
      --  If a generic type is used is such a construct in a generic package
      --  declaration, it carries the flag No_Predicate_On_Actual. It is part
      --  of the generic contract that the actual cannot have predicates.

      function Subtypes_Match (Gen_T, Act_T : Entity_Id) return Boolean;
      --  Check that base types are the same and that the subtypes match
      --  statically. Used in several of the validation subprograms for
      --  actuals in instantiations.

      procedure Validate_Array_Type_Instance;
      procedure Validate_Access_Subprogram_Instance;
      procedure Validate_Access_Type_Instance;
      procedure Validate_Derived_Type_Instance;
      procedure Validate_Derived_Interface_Type_Instance;
      procedure Validate_Discriminated_Formal_Type;
      procedure Validate_Interface_Type_Instance;
      procedure Validate_Private_Type_Instance;
      procedure Validate_Incomplete_Type_Instance;
      --  These procedures perform validation tests for the named case.
      --  Validate_Discriminated_Formal_Type is shared by formal private
      --  types and Ada 2012 formal incomplete types.

      --------------------------------------------
      --  Check_Shared_Variable_Control_Aspects --
      --------------------------------------------

      --  Ada 2022: Verify that shared variable control aspects (RM C.6)
      --  that may be specified for the formal are obeyed by the actual.
      --  If the formal is a derived type the aspect specifications must match.
      --  NOTE: AI12-0282 implies that matching of aspects is required between
      --  formal and actual in all cases, but this is too restrictive.
      --  In particular it violates a language design rule: a limited private
      --  indefinite formal can be matched by any actual. The current code
      --  reflects an older and more permissive version of RM C.6 (12/5).

      procedure Check_Shared_Variable_Control_Aspects is
      begin
         if Ada_Version >= Ada_2022 then
            if Is_Atomic (A_Gen_T) and then not Is_Atomic (Act_T) then
               Error_Msg_NE
                  ("actual for& must have Atomic aspect", Actual, A_Gen_T);

            elsif Is_Derived_Type (A_Gen_T)
              and then Is_Atomic (A_Gen_T) /= Is_Atomic (Act_T)
            then
               Error_Msg_NE
                  ("actual for& has different Atomic aspect", Actual, A_Gen_T);
            end if;

            if Is_Volatile (A_Gen_T) and then not Is_Volatile (Act_T) then
               Error_Msg_NE
                  ("actual for& must have Volatile aspect",
                   Actual, A_Gen_T);

            elsif Is_Derived_Type (A_Gen_T)
              and then Is_Volatile (A_Gen_T) /= Is_Volatile (Act_T)
            then
               Error_Msg_NE
                  ("actual for& has different Volatile aspect",
                   Actual, A_Gen_T);
            end if;

            --  We assume that an array type whose atomic component type
            --  is Atomic is equivalent to an array type with the explicit
            --  aspect Has_Atomic_Components. This is a reasonable inference
            --  from the intent of AI12-0282, and makes it legal to use an
            --  actual that does not have the identical aspect as the formal.
            --  Ditto for volatile components.

            declare
               Actual_Atomic_Comp : constant Boolean :=
               Has_Atomic_Components (Act_T)
                      or else (Is_Array_Type (Act_T)
                                and then Is_Atomic (Component_Type (Act_T)));
            begin
               if Has_Atomic_Components (A_Gen_T) /= Actual_Atomic_Comp then
                  Error_Msg_NE
                    ("formal and actual for& must agree on atomic components",
                       Actual, A_Gen_T);
               end if;
            end;

            declare
               Actual_Volatile_Comp : constant Boolean :=
                 Has_Volatile_Components (Act_T)
                   or else (Is_Array_Type (Act_T)
                             and then Is_Volatile (Component_Type (Act_T)));
            begin
               if Has_Volatile_Components (A_Gen_T) /= Actual_Volatile_Comp
               then
                  Error_Msg_NE
                    ("actual for& must have volatile components",
                       Actual, A_Gen_T);
               end if;
            end;

            --  The following two aspects do not require exact matching,
            --  but only one-way agreement. See RM C.6.

            if Is_Independent (A_Gen_T) and then not Is_Independent (Act_T)
            then
               Error_Msg_NE
                 ("actual for& must have Independent aspect specified",
                     Actual, A_Gen_T);
            end if;

            if Has_Independent_Components (A_Gen_T)
              and then not Has_Independent_Components (Act_T)
            then
               Error_Msg_NE
                 ("actual for& must have Independent_Components specified",
                     Actual, A_Gen_T);
            end if;
         end if;
      end Check_Shared_Variable_Control_Aspects;

      ---------------------------------
      --  Diagnose_Predicated_Actual --
      ---------------------------------

      procedure Diagnose_Predicated_Actual is
      begin
         if No_Predicate_On_Actual (A_Gen_T)
           and then Has_Predicates (Act_T)
         then
            Error_Msg_NE
              ("actual for& cannot be a type with predicate",
               Instantiation_Node, A_Gen_T);

         elsif No_Dynamic_Predicate_On_Actual (A_Gen_T)
           and then Has_Predicates (Act_T)
           and then not Has_Static_Predicate_Aspect (Act_T)
         then
            Error_Msg_NE
              ("actual for& cannot be a type with a dynamic predicate",
               Instantiation_Node, A_Gen_T);
         end if;
      end Diagnose_Predicated_Actual;

      --------------------
      -- Subtypes_Match --
      --------------------

      function Subtypes_Match (Gen_T, Act_T : Entity_Id) return Boolean is
         T : constant Entity_Id := Get_Instance_Of (Gen_T);

      begin
         --  Check that the base types, root types (when dealing with class
         --  wide types), or designated types (when dealing with anonymous
         --  access types) of Gen_T and Act_T are statically matching subtypes.

         return (Base_Type (Base_Type (T)) = Base_Type (Act_T)
                   and then Subtypes_Statically_Match (T, Act_T))

           or else (Is_Class_Wide_Type (Gen_T)
                     and then Is_Class_Wide_Type (Act_T)
                     and then Subtypes_Match
                                (Get_Instance_Of (Root_Type (Gen_T)),
                                 Root_Type (Act_T)))

           or else (Is_Anonymous_Access_Type (Gen_T)
               and then Ekind (Act_T) = Ekind (Gen_T)
               and then Subtypes_Statically_Match
                          (Designated_Type (Gen_T), Designated_Type (Act_T)));
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

         --  According to AI05-288, actuals for access_to_subprograms must be
         --  subtype conformant with the generic formal. Previous to AI05-288
         --  only mode conformance was required.

         --  This is a binding interpretation that applies to previous versions
         --  of the language, no need to maintain previous weaker checks.

         Check_Subtype_Conformant
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

         --  If the formal has a specified convention (which in most cases
         --  will be StdCall) verify that the actual has the same convention.

         if Has_Convention_Pragma (A_Gen_T)
           and then Convention (A_Gen_T) /= Convention (Act_T)
         then
            Error_Msg_Name_1 := Get_Convention_Name (Convention (A_Gen_T));
            Error_Msg_NE
              ("actual for formal & must have convention %", Actual, Gen_T);
         end if;

         if Can_Never_Be_Null (A_Gen_T) /= Can_Never_Be_Null (Act_T) then
            Error_Msg_NE
               ("non null exclusion of actual and formal & do not match",
               Actual, Gen_T);
         end if;
      end Validate_Access_Subprogram_Instance;

      -----------------------------------
      -- Validate_Access_Type_Instance --
      -----------------------------------

      procedure Validate_Access_Type_Instance is
         Desig_Type : constant Entity_Id :=
                        Find_Actual_Type (Designated_Type (A_Gen_T), A_Gen_T);
         Desig_Act  : Entity_Id;

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
               Error_Msg_N
                 ("actual must be general access type!", Actual);
               Error_Msg_NE -- CODEFIX
                 ("\add ALL to }!", Actual, Act_T);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         --  The designated subtypes, that is to say the subtypes introduced
         --  by an access type declaration (and not by a subtype declaration)
         --  must match.

         Desig_Act := Designated_Type (Base_Type (Act_T));

         --  The designated type may have been introduced through a limited_
         --  with clause, in which case retrieve the non-limited view. This
         --  applies to incomplete types as well as to class-wide types.

         if From_Limited_With (Desig_Act) then
            Desig_Act := Available_View (Desig_Act);
         end if;

         if not Subtypes_Match (Desig_Type, Desig_Act) then
            Error_Msg_NE
              ("designated type of actual does not match that of formal &",
               Actual, Gen_T);

            if not Predicates_Match (Desig_Type, Desig_Act) then
               Error_Msg_N ("\predicates do not match", Actual);
            end if;

            Abandon_Instantiation (Actual);
         end if;

         --  Ada 2005: null-exclusion indicators of the two types must agree

         if Can_Never_Be_Null (A_Gen_T) /= Can_Never_Be_Null (Act_T) then
            Error_Msg_NE
              ("non null exclusion of actual and formal & do not match",
                 Actual, Gen_T);
         end if;
      end Validate_Access_Type_Instance;

      ----------------------------------
      -- Validate_Array_Type_Instance --
      ----------------------------------

      procedure Validate_Array_Type_Instance is
         I1 : Node_Id;
         I2 : Node_Id;
         T2 : Entity_Id;

         function Formal_Dimensions return Nat;
         --  Count number of dimensions in array type formal

         -----------------------
         -- Formal_Dimensions --
         -----------------------

         function Formal_Dimensions return Nat is
            Num   : Nat := 0;
            Index : Node_Id;

         begin
            if Nkind (Def) = N_Constrained_Array_Definition then
               Index := First (Discrete_Subtype_Definitions (Def));
            else
               Index := First (Subtype_Marks (Def));
            end if;

            while Present (Index) loop
               Num := Num + 1;
               Next (Index);
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
            if not Is_Constrained (Act_T) then
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

            --  If the indexes of the actual were given by a subtype_mark,
            --  the index was transformed into a range attribute. Retrieve
            --  the original type mark for checking.

            if Is_Entity_Name (Original_Node (I2)) then
               T2 := Entity (Original_Node (I2));
            else
               T2 := Etype (I2);
            end if;

            if not Subtypes_Match
                     (Find_Actual_Type (Etype (I1), A_Gen_T), T2)
            then
               Error_Msg_NE
                 ("index types of actual do not match those of formal &",
                  Actual, Gen_T);
               Abandon_Instantiation (Actual);
            end if;

            Next_Index (I1);
            Next_Index (I2);
         end loop;

         --  Check matching subtypes. Note that there are complex visibility
         --  issues when the generic is a child unit and some aspect of the
         --  generic type is declared in a parent unit of the generic. We do
         --  the test to handle this special case only after a direct check
         --  for static matching has failed. The case where both the component
         --  type and the array type are separate formals, and the component
         --  type is a private view may also require special checking in
         --  Subtypes_Match. Finally, we assume that a child instance where
         --  the component type comes from a formal of a parent instance is
         --  correct because the generic was correct. A more precise check
         --  seems too complex to install???

         if Subtypes_Match
           (Component_Type (A_Gen_T), Component_Type (Act_T))
             or else
               Subtypes_Match
                 (Find_Actual_Type (Component_Type (A_Gen_T), A_Gen_T),
                  Component_Type (Act_T))
            or else
              (not Inside_A_Generic
                 and then Is_Child_Unit (Scope (Component_Type (A_Gen_T))))
         then
            null;
         else
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
           and then not Interface_Present_In_Ancestor (Act_T, Par)
         then
            Error_Msg_NE
              ("interface actual must include progenitor&", Actual, Par);
         end if;

         --  Now verify that the actual includes all other ancestors of
         --  the formal.

         Elmt := First_Elmt (Interfaces (A_Gen_T));
         while Present (Elmt) loop
            if not Interface_Present_In_Ancestor
                     (Act_T, Get_Instance_Of (Node (Elmt)))
            then
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
         --  Verify that the actual includes the progenitors of the formal,
         --  if any. The formal may depend on previous formals and their
         --  instance, so we must examine instance of interfaces if present.
         --  The actual may be an extension of an interface, in which case
         --  it does not appear in the interface list, so this must be
         --  checked separately.

         if Present (Interface_List (Def)) then
            if not Has_Interfaces (Act_T) then
               Error_Msg_NE
                 ("actual must implement all interfaces of formal&",
                   Actual, A_Gen_T);

            else
               declare
                  Act_Iface_List : Elist_Id;
                  Iface          : Node_Id;
                  Iface_Ent      : Entity_Id;

                  function Instance_Exists (I : Entity_Id) return Boolean;
                  --  If the interface entity is declared in a generic unit,
                  --  this can only be legal if we are within an instantiation
                  --  of a child of that generic. There is currently no
                  --  mechanism to relate an interface declared within a
                  --  generic to the corresponding interface in an instance,
                  --  so we traverse the list of interfaces of the actual,
                  --  looking for a name match.

                  ---------------------
                  -- Instance_Exists --
                  ---------------------

                  function Instance_Exists (I : Entity_Id) return Boolean is
                     Iface_Elmt : Elmt_Id;

                  begin
                     Iface_Elmt := First_Elmt (Act_Iface_List);
                     while Present (Iface_Elmt) loop
                        if Is_Generic_Instance (Scope (Node (Iface_Elmt)))
                          and then Chars (Node (Iface_Elmt)) = Chars (I)
                        then
                           return True;
                        end if;

                        Next_Elmt (Iface_Elmt);
                     end loop;

                     return False;
                  end Instance_Exists;

               begin
                  Iface := First (Abstract_Interface_List (A_Gen_T));
                  Collect_Interfaces (Act_T, Act_Iface_List);

                  while Present (Iface) loop
                     Iface_Ent := Get_Instance_Of (Entity (Iface));

                     if Is_Ancestor (Iface_Ent, Act_T)
                      or else Is_Progenitor (Iface_Ent, Act_T)
                     then
                        null;

                     elsif Ekind (Scope (Iface_Ent)) = E_Generic_Package
                       and then Instance_Exists (Iface_Ent)
                     then
                        null;

                     else
                        Error_Msg_Name_1 := Chars (Act_T);
                        Error_Msg_NE
                          ("actual% must implement interface&",
                           Actual, Etype (Iface));
                     end if;

                     Next (Iface);
                  end loop;
               end;
            end if;
         end if;

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

         --  The type may be a local derivation, or a type extension of a
         --  previous formal, or of a formal of a parent package.

         elsif Is_Derived_Type (Get_Instance_Of (A_Gen_T))
          or else
            Ekind (Get_Instance_Of (A_Gen_T)) = E_Record_Type_With_Private
         then
            --  Check whether the parent is another formal derived type in the
            --  same generic unit.

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

               --  The ancestor itself may be a previous formal that has been
               --  instantiated.

               Ancestor := Get_Instance_Of (Ancestor);

            else
               Ancestor := Get_Instance_Of (Etype (Get_Instance_Of (A_Gen_T)));
            end if;

         --  Check whether parent is a previous formal of the current generic

         elsif Is_Derived_Type (A_Gen_T)
           and then Is_Generic_Type (Etype (A_Gen_T))
           and then Scope (A_Gen_T) = Scope (Etype (A_Gen_T))
         then
            Ancestor := Get_Instance_Of (First_Subtype (Etype (A_Gen_T)));

         --  An unusual case: the actual is a type declared in a parent unit,
         --  but is not a formal type so there is no instance_of for it.
         --  Retrieve it by analyzing the record extension.

         elsif Is_Child_Unit (Scope (A_Gen_T))
           and then In_Open_Scopes (Scope (Act_T))
           and then Is_Generic_Instance (Scope (Act_T))
         then
            Analyze (Subtype_Mark (Def));
            Ancestor := Entity (Subtype_Mark (Def));

         else
            Ancestor := Get_Instance_Of (Etype (Base_Type (A_Gen_T)));
         end if;

         --  If the formal derived type has pragma Preelaborable_Initialization
         --  then the actual type must have preelaborable initialization.

         if Known_To_Have_Preelab_Init (A_Gen_T)
           and then not Has_Preelaborable_Initialization (Act_T)
         then
            Error_Msg_NE
              ("actual for & must have preelaborable initialization",
               Actual, Gen_T);
         end if;

         --  Ada 2005 (AI-251)

         if Ada_Version >= Ada_2005 and then Is_Interface (Ancestor) then
            if not Interface_Present_In_Ancestor (Act_T, Ancestor) then
               Error_Msg_NE
                 ("(Ada 2005) expected type implementing & in instantiation",
                  Actual, Ancestor);
            end if;

         --  Finally verify that the (instance of) the ancestor is an ancestor
         --  of the actual.

         elsif not Is_Ancestor (Base_Type (Ancestor), Act_T) then
            Error_Msg_NE
              ("expect type derived from & in instantiation",
               Actual, First_Subtype (Ancestor));
            Abandon_Instantiation (Actual);
         end if;

         --  Ada 2005 (AI-443): Synchronized formal derived type checks. Note
         --  that the formal type declaration has been rewritten as a private
         --  extension.

         if Ada_Version >= Ada_2005
           and then Nkind (Parent (A_Gen_T)) = N_Private_Extension_Declaration
           and then Synchronized_Present (Parent (A_Gen_T))
         then
            --  The actual must be a synchronized tagged type

            if not Is_Tagged_Type (Act_T) then
               Error_Msg_N
                 ("actual of synchronized type must be tagged", Actual);
               Abandon_Instantiation (Actual);

            elsif Nkind (Parent (Act_T)) = N_Full_Type_Declaration
              and then Nkind (Type_Definition (Parent (Act_T))) =
                                                 N_Derived_Type_Definition
              and then not Synchronized_Present
                             (Type_Definition (Parent (Act_T)))
            then
               Error_Msg_N
                 ("actual of synchronized type must be synchronized", Actual);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         --  Perform atomic/volatile checks (RM C.6(12)). Note that AI05-0218-1
         --  removes the second instance of the phrase "or allow pass by copy".

         --  For Ada 2022, the aspect may be specified explicitly for the
         --  formal regardless of whether an ancestor obeys it.

         if Is_Atomic (Act_T)
             and then not Is_Atomic (Ancestor)
             and then not Is_Atomic (A_Gen_T)
         then
            Error_Msg_N
              ("cannot have atomic actual type for non-atomic formal type",
               Actual);

         elsif Is_Volatile (Act_T)
           and then not Is_Volatile (Ancestor)
           and then not Is_Volatile (A_Gen_T)
         then
            Error_Msg_N
              ("cannot have volatile actual type for non-volatile formal type",
               Actual);
         end if;

         --  It should not be necessary to check for unknown discriminants on
         --  Formal, but for some reason Has_Unknown_Discriminants is false for
         --  A_Gen_T, so Is_Definite_Subtype incorrectly returns True. This
         --  needs fixing. ???

         if Is_Definite_Subtype (A_Gen_T)
           and then not Unknown_Discriminants_Present (Formal)
           and then not Is_Definite_Subtype (Act_T)
         then
            Error_Msg_N ("actual subtype must be constrained", Actual);
            Abandon_Instantiation (Actual);
         end if;

         if not Unknown_Discriminants_Present (Formal) then
            if Is_Constrained (Ancestor) then
               if not Is_Constrained (Act_T) then
                  Error_Msg_N ("actual subtype must be constrained", Actual);
                  Abandon_Instantiation (Actual);
               end if;

            --  Ancestor is unconstrained, Check if generic formal and actual
            --  agree on constrainedness. The check only applies to array types
            --  and discriminated types.

            elsif Is_Constrained (Act_T) then
               if Ekind (Ancestor) = E_Access_Type
                 or else (not Is_Constrained (A_Gen_T)
                           and then Is_Composite_Type (A_Gen_T))
               then
                  Error_Msg_N ("actual subtype must be unconstrained", Actual);
                  Abandon_Instantiation (Actual);
               end if;

            --  A class-wide type is only allowed if the formal has unknown
            --  discriminants.

            elsif Is_Class_Wide_Type (Act_T)
              and then not Has_Unknown_Discriminants (Ancestor)
            then
               Error_Msg_NE
                 ("actual for & cannot be a class-wide type", Actual, Gen_T);
               Abandon_Instantiation (Actual);

            --  Otherwise, the formal and actual must have the same number
            --  of discriminants and each discriminant of the actual must
            --  correspond to a discriminant of the formal.

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
                    No (Corresponding_Discriminant (Actual_Discr))
                  then
                     Error_Msg_NE
                       ("discriminant & does not correspond "
                        & "to ancestor discriminant", Actual, Actual_Discr);
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
            --  constrainedness, but the check here is added for completeness.

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

            if not Subtypes_Statically_Compatible
                     (Act_T, Ancestor, Formal_Derived_Matching => True)
            then
               Error_Msg_NE
                 ("actual for & must be statically compatible with ancestor",
                  Actual, Gen_T);

               if not Predicates_Compatible (Act_T, Ancestor) then
                  Error_Msg_N
                    ("\predicate on actual is not compatible with ancestor",
                     Actual);
               end if;

               Abandon_Instantiation (Actual);
            end if;
         end if;

         --  If the formal and actual types are abstract, check that there
         --  are no abstract primitives of the actual type that correspond to
         --  nonabstract primitives of the formal type (second sentence of
         --  RM95 3.9.3(9)).

         if Is_Abstract_Type (A_Gen_T) and then Is_Abstract_Type (Act_T) then
            Check_Abstract_Primitives : declare
               Gen_Prims  : constant Elist_Id :=
                             Primitive_Operations (A_Gen_T);
               Gen_Elmt   : Elmt_Id;
               Gen_Subp   : Entity_Id;
               Anc_Subp   : Entity_Id;
               Anc_Formal : Entity_Id;
               Anc_F_Type : Entity_Id;

               Act_Prims  : constant Elist_Id  := Primitive_Operations (Act_T);
               Act_Elmt   : Elmt_Id;
               Act_Subp   : Entity_Id;
               Act_Formal : Entity_Id;
               Act_F_Type : Entity_Id;

               Subprograms_Correspond : Boolean;

               function Is_Tagged_Ancestor (T1, T2 : Entity_Id) return Boolean;
               --  Returns true if T2 is derived directly or indirectly from
               --  T1, including derivations from interfaces. T1 and T2 are
               --  required to be specific tagged base types.

               ------------------------
               -- Is_Tagged_Ancestor --
               ------------------------

               function Is_Tagged_Ancestor (T1, T2 : Entity_Id) return Boolean
               is
                  Intfc_Elmt : Elmt_Id;

               begin
                  --  The predicate is satisfied if the types are the same

                  if T1 = T2 then
                     return True;

                  --  If we've reached the top of the derivation chain then
                  --  we know that T1 is not an ancestor of T2.

                  elsif Etype (T2) = T2 then
                     return False;

                  --  Proceed to check T2's immediate parent

                  elsif Is_Ancestor (T1, Base_Type (Etype (T2))) then
                     return True;

                  --  Finally, check to see if T1 is an ancestor of any of T2's
                  --  progenitors.

                  else
                     Intfc_Elmt := First_Elmt (Interfaces (T2));
                     while Present (Intfc_Elmt) loop
                        if Is_Ancestor (T1, Node (Intfc_Elmt)) then
                           return True;
                        end if;

                        Next_Elmt (Intfc_Elmt);
                     end loop;
                  end if;

                  return False;
               end Is_Tagged_Ancestor;

            --  Start of processing for Check_Abstract_Primitives

            begin
               --  Loop over all of the formal derived type's primitives

               Gen_Elmt := First_Elmt (Gen_Prims);
               while Present (Gen_Elmt) loop
                  Gen_Subp := Node (Gen_Elmt);

                  --  If the primitive of the formal is not abstract, then
                  --  determine whether there is a corresponding primitive of
                  --  the actual type that's abstract.

                  if not Is_Abstract_Subprogram (Gen_Subp) then
                     Act_Elmt := First_Elmt (Act_Prims);
                     while Present (Act_Elmt) loop
                        Act_Subp := Node (Act_Elmt);

                        --  If we find an abstract primitive of the actual,
                        --  then we need to test whether it corresponds to the
                        --  subprogram from which the generic formal primitive
                        --  is inherited.

                        if Is_Abstract_Subprogram (Act_Subp) then
                           Anc_Subp := Alias (Gen_Subp);

                           --  Test whether we have a corresponding primitive
                           --  by comparing names, kinds, formal types, and
                           --  result types.

                           if Chars (Anc_Subp) = Chars (Act_Subp)
                             and then Ekind (Anc_Subp) = Ekind (Act_Subp)
                           then
                              Anc_Formal := First_Formal (Anc_Subp);
                              Act_Formal := First_Formal (Act_Subp);
                              while Present (Anc_Formal)
                                and then Present (Act_Formal)
                              loop
                                 Anc_F_Type := Etype (Anc_Formal);
                                 Act_F_Type := Etype (Act_Formal);

                                 if Ekind (Anc_F_Type) =
                                                        E_Anonymous_Access_Type
                                 then
                                    Anc_F_Type := Designated_Type (Anc_F_Type);

                                    if Ekind (Act_F_Type) =
                                                        E_Anonymous_Access_Type
                                    then
                                       Act_F_Type :=
                                         Designated_Type (Act_F_Type);
                                    else
                                       exit;
                                    end if;

                                 elsif
                                   Ekind (Act_F_Type) = E_Anonymous_Access_Type
                                 then
                                    exit;
                                 end if;

                                 Anc_F_Type := Base_Type (Anc_F_Type);
                                 Act_F_Type := Base_Type (Act_F_Type);

                                 --  If the formal is controlling, then the
                                 --  the type of the actual primitive's formal
                                 --  must be derived directly or indirectly
                                 --  from the type of the ancestor primitive's
                                 --  formal.

                                 if Is_Controlling_Formal (Anc_Formal) then
                                    if not Is_Tagged_Ancestor
                                             (Anc_F_Type, Act_F_Type)
                                    then
                                       exit;
                                    end if;

                                 --  Otherwise the types of the formals must
                                 --  be the same.

                                 elsif Anc_F_Type /= Act_F_Type then
                                    exit;
                                 end if;

                                 Next_Formal (Anc_Formal);
                                 Next_Formal (Act_Formal);
                              end loop;

                              --  If we traversed through all of the formals
                              --  then so far the subprograms correspond, so
                              --  now check that any result types correspond.

                              if No (Anc_Formal) and then No (Act_Formal) then
                                 Subprograms_Correspond := True;

                                 if Ekind (Act_Subp) = E_Function then
                                    Anc_F_Type := Etype (Anc_Subp);
                                    Act_F_Type := Etype (Act_Subp);

                                    if Ekind (Anc_F_Type) =
                                                        E_Anonymous_Access_Type
                                    then
                                       Anc_F_Type :=
                                         Designated_Type (Anc_F_Type);

                                       if Ekind (Act_F_Type) =
                                                        E_Anonymous_Access_Type
                                       then
                                          Act_F_Type :=
                                            Designated_Type (Act_F_Type);
                                       else
                                          Subprograms_Correspond := False;
                                       end if;

                                    elsif
                                      Ekind (Act_F_Type)
                                        = E_Anonymous_Access_Type
                                    then
                                       Subprograms_Correspond := False;
                                    end if;

                                    Anc_F_Type := Base_Type (Anc_F_Type);
                                    Act_F_Type := Base_Type (Act_F_Type);

                                    --  Now either the result types must be
                                    --  the same or, if the result type is
                                    --  controlling, the result type of the
                                    --  actual primitive must descend from the
                                    --  result type of the ancestor primitive.

                                    if Subprograms_Correspond
                                      and then Anc_F_Type /= Act_F_Type
                                      and then
                                        Has_Controlling_Result (Anc_Subp)
                                      and then not Is_Tagged_Ancestor
                                                     (Anc_F_Type, Act_F_Type)
                                    then
                                       Subprograms_Correspond := False;
                                    end if;
                                 end if;

                                 --  Found a matching subprogram belonging to
                                 --  formal ancestor type, so actual subprogram
                                 --  corresponds and this violates 3.9.3(9).

                                 if Subprograms_Correspond then
                                    Error_Msg_NE
                                      ("abstract subprogram & overrides "
                                       & "nonabstract subprogram of ancestor",
                                       Actual, Act_Subp);
                                 end if;
                              end if;
                           end if;
                        end if;

                        Next_Elmt (Act_Elmt);
                     end loop;
                  end if;

                  Next_Elmt (Gen_Elmt);
               end loop;
            end Check_Abstract_Primitives;
         end if;

         --  Verify that limitedness matches. If parent is a limited
         --  interface then the generic formal is not unless declared
         --  explicitly so. If not declared limited, the actual cannot be
         --  limited (see AI05-0087).

         if Is_Limited_Type (Act_T) and then not Is_Limited_Type (A_Gen_T) then
            if not In_Instance then
               Error_Msg_NE
                 ("actual for non-limited & cannot be a limited type",
                  Actual, Gen_T);
               Explain_Limited_Type (Act_T, Actual);
               Abandon_Instantiation (Actual);
            end if;
         end if;

         --  Check for AI12-0036

         declare
            Formal_Is_Private_Extension : constant Boolean :=
              Nkind (Parent (A_Gen_T)) = N_Private_Extension_Declaration;

            Actual_Is_Tagged : constant Boolean := Is_Tagged_Type (Act_T);

         begin
            if Actual_Is_Tagged /= Formal_Is_Private_Extension then
               if not In_Instance then
                  if Actual_Is_Tagged then
                     Error_Msg_NE
                       ("actual for & cannot be a tagged type", Actual, Gen_T);
                  else
                     Error_Msg_NE
                       ("actual for & must be a tagged type", Actual, Gen_T);
                  end if;

                  Abandon_Instantiation (Actual);
               end if;
            end if;
         end;
      end Validate_Derived_Type_Instance;

      ----------------------------------------
      -- Validate_Discriminated_Formal_Type --
      ----------------------------------------

      procedure Validate_Discriminated_Formal_Type is
         Formal_Discr : Entity_Id;
         Actual_Discr : Entity_Id;
         Formal_Subt  : Entity_Id;

      begin
         if Has_Discriminants (A_Gen_T) then
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
                     Error_Msg_N
                       ("discriminants on actual do not match formal",
                        Actual);
                     Abandon_Instantiation (Actual);
                  end if;

                  Formal_Subt := Get_Instance_Of (Etype (Formal_Discr));

                  --  Access discriminants match if designated types do

                  if Ekind (Base_Type (Formal_Subt)) = E_Anonymous_Access_Type
                    and then (Ekind (Base_Type (Etype (Actual_Discr)))) =
                                E_Anonymous_Access_Type
                    and then
                      Subtypes_Match
                        (Get_Instance_Of
                           (Designated_Type (Base_Type (Formal_Subt))),
                         Designated_Type (Base_Type (Etype (Actual_Discr))))
                  then
                     null;

                  elsif Base_Type (Formal_Subt) /=
                          Base_Type (Etype (Actual_Discr))
                  then
                     Error_Msg_N
                       ("types of actual discriminants must match formal",
                        Actual);
                     Abandon_Instantiation (Actual);

                  elsif not Subtypes_Statically_Match
                              (Formal_Subt, Etype (Actual_Discr))
                    and then Ada_Version >= Ada_95
                  then
                     Error_Msg_N
                       ("subtypes of actual discriminants must match formal",
                        Actual);
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
      end Validate_Discriminated_Formal_Type;

      ---------------------------------------
      -- Validate_Incomplete_Type_Instance --
      ---------------------------------------

      procedure Validate_Incomplete_Type_Instance is
      begin
         if not Is_Tagged_Type (Act_T)
           and then Is_Tagged_Type (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for & must be a tagged type", Actual, Gen_T);
         end if;

         Validate_Discriminated_Formal_Type;
      end Validate_Incomplete_Type_Instance;

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
           or else Is_Task_Interface (A_Gen_T) /= Is_Task_Interface (Act_T)
           or else Is_Protected_Interface (A_Gen_T) /=
                   Is_Protected_Interface (Act_T)
           or else Is_Synchronized_Interface (A_Gen_T) /=
                   Is_Synchronized_Interface (Act_T)
         then
            Error_Msg_NE
              ("actual for interface& does not match (RM 12.5.5(4))",
               Actual, Gen_T);
         end if;
      end Validate_Interface_Type_Instance;

      ------------------------------------
      -- Validate_Private_Type_Instance --
      ------------------------------------

      procedure Validate_Private_Type_Instance is
      begin
         if Is_Limited_Type (Act_T)
           and then not Is_Limited_Type (A_Gen_T)
         then
            if In_Instance then
               null;
            else
               Error_Msg_NE
                 ("actual for non-limited & cannot be a limited type", Actual,
                  Gen_T);
               Explain_Limited_Type (Act_T, Actual);
               Abandon_Instantiation (Actual);
            end if;

         elsif Known_To_Have_Preelab_Init (A_Gen_T)
           and then not Has_Preelaborable_Initialization (Act_T)
         then
            Error_Msg_NE
              ("actual for & must have preelaborable initialization", Actual,
               Gen_T);

         elsif not Is_Definite_Subtype (Act_T)
            and then Is_Definite_Subtype (A_Gen_T)
            and then No (Class_Wide_Equivalent_Type (Act_T))
            and then Ada_Version >= Ada_95
         then
            Error_Msg_NE
              ("actual for & must be a definite subtype", Actual, Gen_T);

         elsif not Is_Tagged_Type (Act_T)
           and then Is_Tagged_Type (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for & must be a tagged type", Actual, Gen_T);

         --  For generic formal tagged types with the First_Controlling_Param
         --  aspect, ensure that the actual type also has this aspect.

         elsif Is_Tagged_Type (Act_T)
           and then Is_Tagged_Type (A_Gen_T)
           and then not Has_First_Controlling_Parameter_Aspect (Act_T)
           and then Has_First_Controlling_Parameter_Aspect (A_Gen_T)
         then
            Error_Msg_NE
              ("actual for & must be a 'First_'Controlling_'Parameter tagged "
               & "type", Actual, Gen_T);
         end if;

         Validate_Discriminated_Formal_Type;
         Ancestor := Gen_T;
      end Validate_Private_Type_Instance;

   --  Start of processing for Instantiate_Type

   begin
      if not Is_Entity_Name (Actual)
        or else not Is_Type (Entity (Actual))
      then
         Error_Msg_NE
           ("expect valid subtype mark to instantiate &", Actual, Gen_T);
         Abandon_Instantiation (Actual);
      end if;

      Act_T := Entity (Actual);

      --  Obtain the class-wide equivalent type and use it for the
      --  instantiation instead of a mutably tagged type.

      if Present (Class_Wide_Equivalent_Type (Act_T)) then
         Act_T := Class_Wide_Equivalent_Type (Act_T);
      end if;

      --  Ada 2005 (AI-216): An Unchecked_Union subtype shall only be passed
      --  as a generic actual parameter if the corresponding formal type
      --  does not have a known_discriminant_part, or is a formal derived
      --  type that is an Unchecked_Union type.

      if Is_Unchecked_Union (Base_Type (Act_T)) then
         if not Has_Discriminants (A_Gen_T)
           or else (Is_Derived_Type (A_Gen_T)
                     and then Is_Unchecked_Union (A_Gen_T))
         then
            null;
         else
            Error_Msg_N ("unchecked union cannot be the actual for a "
                         & "discriminated formal type", Act_T);

         end if;
      end if;

      --  Deal with fixed/floating restrictions

      if Is_Floating_Point_Type (Act_T) then
         Check_Restriction (No_Floating_Point, Actual);
      elsif Is_Fixed_Point_Type (Act_T) then
         Check_Restriction (No_Fixed_Point, Actual);
      end if;

      --  Deal with error of using incomplete type as generic actual.
      --  This includes limited views of a type, even if the non-limited
      --  view may be available.

      if Ekind (Act_T) = E_Incomplete_Type
        or else (Is_Class_Wide_Type (Act_T)
                  and then Ekind (Root_Type (Act_T)) = E_Incomplete_Type)
      then
         --  If the formal is an incomplete type, the actual can be
         --  incomplete as well, but if an actual incomplete type has
         --  a full view, then we'll retrieve that.

         if Ekind (A_Gen_T) = E_Incomplete_Type
           and then No (Full_View (Act_T))
         then
            null;

         elsif Is_Class_Wide_Type (Act_T)
           or else No (Full_View (Act_T))
         then
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

      --  Deal with error of premature use of private type as generic actual,
      --  which is allowed for incomplete formals.

      elsif Ekind (A_Gen_T) /= E_Incomplete_Type then
         if Is_Private_Type (Act_T)
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

      if not Is_Abstract_Type (A_Gen_T)
        and then Is_Abstract_Type (Act_T)
      then
         Error_Msg_N
           ("actual of non-abstract formal cannot be abstract", Actual);
      end if;

      --  A generic scalar type is a first subtype for which we generate
      --  an anonymous base type. Indicate that the instance of this base
      --  is the base type of the actual.

      if Is_Scalar_Type (A_Gen_T) then
         Set_Instance_Of (Etype (A_Gen_T), Etype (Act_T));
      end if;

      Check_Shared_Variable_Control_Aspects;

      if not Error_Posted (Act_T) then
         case Nkind (Def) is
            when N_Formal_Private_Type_Definition =>
               Validate_Private_Type_Instance;

            when N_Formal_Incomplete_Type_Definition =>
               Validate_Incomplete_Type_Instance;

            when N_Formal_Derived_Type_Definition =>
               Validate_Derived_Type_Instance;

            when N_Formal_Discrete_Type_Definition =>
               if not Is_Discrete_Type (Act_T) then
                  Error_Msg_NE
                    ("expect discrete type in instantiation of&",
                     Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;

               Diagnose_Predicated_Actual;

            when N_Formal_Signed_Integer_Type_Definition =>
               if not Is_Signed_Integer_Type (Act_T) then
                  Error_Msg_NE
                    ("expect signed integer type in instantiation of&",
                     Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;

               Diagnose_Predicated_Actual;

            when N_Formal_Modular_Type_Definition =>
               if not Is_Modular_Integer_Type (Act_T) then
                  Error_Msg_NE
                    ("expect modular type in instantiation of &",
                       Actual, Gen_T);
                  Abandon_Instantiation (Actual);
               end if;

               Diagnose_Predicated_Actual;

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

            when N_Access_Function_Definition
               | N_Access_Procedure_Definition
            =>
               Validate_Access_Subprogram_Instance;

            when N_Record_Definition =>
               Validate_Interface_Type_Instance;

            when N_Derived_Type_Definition =>
               Validate_Derived_Interface_Type_Instance;

            when others =>
               raise Program_Error;
         end case;
      end if;

      Subt := New_Copy (Gen_T);

      --  Use adjusted sloc of subtype name as the location for other nodes in
      --  the subtype declaration.

      Loc  := Sloc (Subt);

      Decl_Node :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication  => New_Occurrence_Of (Act_T, Loc));

      Copy_Ghost_Aspect (Formal, To => Decl_Node);

      --  Record whether the actual is private at this point, so that
      --  Check_Generic_Actuals can restore its proper view before the
      --  semantic analysis of the instance.

      if Is_Private_Type (Act_T) then
         Set_Has_Private_View (Subtype_Indication (Decl_Node));

      elsif (Is_Access_Type (Act_T)
              and then Is_Private_Type (Designated_Type (Act_T)))
        or else (Is_Array_Type (Act_T)
                  and then
                 Is_Private_Type (Component_Type_For_Private_View (Act_T)))
      then
         Set_Has_Secondary_Private_View (Subtype_Indication (Decl_Node));
      end if;

      --  In Ada 2012 the actual may be a limited view. Indicate that
      --  the local subtype must be treated as such.

      if From_Limited_With (Act_T) then
         Mutate_Ekind (Subt, E_Incomplete_Subtype);
         Set_From_Limited_With (Subt);
      end if;

      Decl_Nodes := New_List (Decl_Node);

      --  Flag actual derived types so their elaboration produces the
      --  appropriate renamings for the primitive operations of the ancestor.
      --  Flag actual for formal private types as well, to determine whether
      --  operations in the private part may override inherited operations.
      --  If the formal has an interface list, the ancestor is not the
      --  parent, but the analyzed formal that includes the interface
      --  operations of all its progenitors.

      --  Same treatment for formal private types, so we can check whether the
      --  type is tagged limited when validating derivations in the private
      --  part. (See AI05-096).

      if Nkind (Def) = N_Formal_Derived_Type_Definition then
         if Present (Interface_List (Def)) then
            Set_Generic_Parent_Type (Decl_Node, A_Gen_T);
         else
            Set_Generic_Parent_Type (Decl_Node, Ancestor);
         end if;

      elsif Nkind (Def) in N_Formal_Private_Type_Definition
                         | N_Formal_Incomplete_Type_Definition
      then
         Set_Generic_Parent_Type (Decl_Node, A_Gen_T);
      end if;

      --  If the actual is a synchronized type that implements an interface,
      --  the primitive operations are attached to the corresponding record,
      --  and we have to treat it as an additional generic actual, so that its
      --  primitive operations become visible in the instance. The task or
      --  protected type itself does not carry primitive operations.

      if Is_Concurrent_Type (Act_T)
        and then Is_Tagged_Type (Act_T)
        and then Present (Corresponding_Record_Type (Act_T))
        and then Present (Ancestor)
        and then Is_Interface (Ancestor)
      then
         declare
            Corr_Rec  : constant Entity_Id :=
                          Corresponding_Record_Type (Act_T);
            New_Corr  : Entity_Id;
            Corr_Decl : Node_Id;

         begin
            New_Corr := Make_Temporary (Loc, 'S');
            Corr_Decl :=
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => New_Corr,
                Subtype_Indication  =>
                  New_Occurrence_Of (Corr_Rec, Loc));
            Append_To (Decl_Nodes, Corr_Decl);

            if Ekind (Act_T) = E_Task_Type then
               Mutate_Ekind (Subt, E_Task_Subtype);
            else
               Mutate_Ekind (Subt, E_Protected_Subtype);
            end if;

            Set_Corresponding_Record_Type (Subt, Corr_Rec);
            Set_Generic_Parent_Type (Corr_Decl, Ancestor);
            Set_Generic_Parent_Type (Decl_Node, Empty);
         end;
      end if;

      --  For a floating-point type, capture dimension info if any, because
      --  the generated subtype declaration does not come from source and
      --  will not process dimensions.

      if Is_Floating_Point_Type (Act_T) then
         Copy_Dimensions (Act_T, Subt);
      end if;

      return Decl_Nodes;
   end Instantiate_Type;

   -----------------------------
   -- Is_Abbreviated_Instance --
   -----------------------------

   function Is_Abbreviated_Instance (E : Entity_Id) return Boolean is
   begin
      return Ekind (E) = E_Package
        and then Present (Hidden_In_Formal_Instance (E));
   end Is_Abbreviated_Instance;

   ---------------------
   -- Is_In_Main_Unit --
   ---------------------

   function Is_In_Main_Unit (N : Node_Id) return Boolean is
      Unum         : constant Unit_Number_Type := Get_Source_Unit (N);
      Current_Unit : Node_Id;

   begin
      if Unum = Main_Unit then
         return True;

      --  If the current unit is a subunit then it is either the main unit or
      --  is being compiled as part of the main unit.

      elsif Nkind (N) = N_Compilation_Unit then
         return Nkind (Unit (N)) = N_Subunit;
      end if;

      Current_Unit := Parent (N);
      while Present (Current_Unit)
        and then Nkind (Current_Unit) /= N_Compilation_Unit
      loop
         Current_Unit := Parent (Current_Unit);
      end loop;

      --  The instantiation node is in the main unit, or else the current node
      --  (perhaps as the result of nested instantiations) is in the main unit,
      --  or in the declaration of the main unit, which in this last case must
      --  be a body.

      return
        Current_Unit = Cunit (Main_Unit)
          or else Current_Unit = Library_Unit (Cunit (Main_Unit))
          or else (Present (Current_Unit)
                    and then Present (Library_Unit (Current_Unit))
                    and then Is_In_Main_Unit (Library_Unit (Current_Unit)));
   end Is_In_Main_Unit;

   ----------------------------
   -- Load_Parent_Of_Generic --
   ----------------------------

   procedure Load_Parent_Of_Generic
     (N             : Node_Id;
      Spec          : Node_Id;
      Body_Optional : Boolean := False)
   is
      Comp_Unit          : constant Node_Id := Cunit (Get_Source_Unit (Spec));
      Saved_Style_Check  : constant Boolean := Style_Check;
      Saved_Warn         : constant Warnings_State := Save_Warnings;
      True_Parent        : Node_Id;
      Inst_Node          : Node_Id;
      OK                 : Boolean;
      Previous_Instances : constant Elist_Id := New_Elmt_List;

      procedure Collect_Previous_Instances (Decls : List_Id);
      --  Collect all instantiations in the given list of declarations, that
      --  precede the generic that we need to load. If the bodies of these
      --  instantiations are available, we must analyze them, to ensure that
      --  the public symbols generated are the same when the unit is compiled
      --  to generate code, and when it is compiled in the context of a unit
      --  that needs a particular nested instance. This process is applied to
      --  both package and subprogram instances.

      --------------------------------
      -- Collect_Previous_Instances --
      --------------------------------

      procedure Collect_Previous_Instances (Decls : List_Id) is
         Decl : Node_Id;

      begin
         Decl := First (Decls);
         while Present (Decl) loop
            if Sloc (Decl) >= Sloc (Inst_Node) then
               return;

            --  If Decl is an instantiation, then record it as requiring
            --  instantiation of the corresponding body, except if it is an
            --  abbreviated instantiation generated internally for conformance
            --  checking purposes only for the case of a formal package
            --  declared without a box (see Instantiate_Formal_Package). Such
            --  an instantiation does not generate any code (the actual code
            --  comes from actual) and thus does not need to be analyzed here.
            --  If the instantiation appears with a generic package body it is
            --  not analyzed here either.

            elsif Nkind (Decl) = N_Package_Instantiation
              and then not Is_Abbreviated_Instance (Defining_Entity (Decl))
            then
               Append_Elmt (Decl, Previous_Instances);

            --  For a subprogram instantiation, omit instantiations intrinsic
            --  operations (Unchecked_Conversions, etc.) that have no bodies.

            elsif Nkind (Decl) in N_Function_Instantiation
                                | N_Procedure_Instantiation
              and then not Is_Intrinsic_Subprogram (Entity (Name (Decl)))
            then
               Append_Elmt (Decl, Previous_Instances);

            elsif Nkind (Decl) = N_Package_Declaration then
               Collect_Previous_Instances
                 (Visible_Declarations (Specification (Decl)));
               Collect_Previous_Instances
                 (Private_Declarations (Specification (Decl)));

            --  Previous non-generic bodies may contain instances as well

            elsif Nkind (Decl) = N_Package_Body
              and then Ekind (Corresponding_Spec (Decl)) /= E_Generic_Package
            then
               Collect_Previous_Instances (Declarations (Decl));

            elsif Nkind (Decl) = N_Subprogram_Body
              and then not Acts_As_Spec (Decl)
              and then not Is_Generic_Subprogram (Corresponding_Spec (Decl))
            then
               Collect_Previous_Instances (Declarations (Decl));
            end if;

            Next (Decl);
         end loop;
      end Collect_Previous_Instances;

   --  Start of processing for Load_Parent_Of_Generic

   begin
      if not In_Same_Source_Unit (N, Spec)
        or else Nkind (Unit (Comp_Unit)) = N_Package_Declaration
        or else (Nkind (Unit (Comp_Unit)) = N_Package_Body
                  and then not Is_In_Main_Unit (Spec))
      then
         --  Find body of parent of spec, and analyze it. A special case arises
         --  when the parent is an instantiation, that is to say when we are
         --  currently instantiating a nested generic. In that case, there is
         --  no separate file for the body of the enclosing instance. Instead,
         --  the enclosing body must be instantiated as if it were a pending
         --  instantiation, in order to produce the body for the nested generic
         --  we require now. Note that in that case the generic may be defined
         --  in a package body, the instance defined in the same package body,
         --  and the original enclosing body may not be in the main unit.

         Inst_Node := Empty;

         True_Parent := Parent (Spec);
         while Present (True_Parent)
           and then Nkind (True_Parent) /= N_Compilation_Unit
         loop
            if Nkind (True_Parent) = N_Package_Declaration
              and then
                Nkind (Original_Node (True_Parent)) = N_Package_Instantiation
            then
               --  Parent is a compilation unit that is an instantiation, and
               --  instantiation node has been replaced with package decl.

               Inst_Node := Original_Node (True_Parent);
               exit;

            elsif Nkind (True_Parent) = N_Package_Declaration
             and then Nkind (Parent (True_Parent)) = N_Compilation_Unit
             and then
               Nkind (Unit (Parent (True_Parent))) = N_Package_Instantiation
            then
               --  Parent is a compilation unit that is an instantiation, but
               --  instantiation node has not been replaced with package decl.

               Inst_Node := Unit (Parent (True_Parent));
               exit;

            elsif Nkind (True_Parent) = N_Package_Declaration
              and then Nkind (Parent (True_Parent)) /= N_Compilation_Unit
              and then Present (Generic_Parent (Specification (True_Parent)))
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

            --  If an ancestor of the generic comes from a formal package
            --  there is no source for the ancestor body. This is detected
            --  by examining the scope of the ancestor and its declaration.
            --  The body, if any is needed, will be available when the
            --  current unit (containing a formal package) is instantiated.

            elsif Nkind (True_Parent) = N_Package_Specification
              and then Present (Generic_Parent (True_Parent))
              and then Nkind
                (Original_Node (Unit_Declaration_Node
                  (Scope (Generic_Parent (True_Parent)))))
                     = N_Formal_Package_Declaration
            then
               return;

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

            --  Now complete instantiation of enclosing body, if it appears in
            --  some other unit. If it appears in the current unit, the body
            --  will have been instantiated already.

            if No (Corresponding_Body (Instance_Spec (Inst_Node))) then

               --  We need to determine the expander mode to instantiate the
               --  enclosing body. Because the generic body we need may use
               --  global entities declared in the enclosing package (including
               --  aggregates) it is in general necessary to compile this body
               --  with expansion enabled, except if we are within a generic
               --  package, in which case the usual generic rule applies.

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

                  --  Collect previous instantiations in the unit that contains
                  --  the desired generic.

                  if Nkind (Parent (True_Parent)) /= N_Compilation_Unit
                    and then not Body_Optional
                  then
                     declare
                        Decl : Elmt_Id;
                        Info : Pending_Body_Info;
                        Par  : Node_Id;

                     begin
                        Par := Parent (Inst_Node);
                        while Present (Par) loop
                           exit when Nkind (Parent (Par)) = N_Compilation_Unit;
                           Par := Parent (Par);
                        end loop;

                        pragma Assert (Present (Par));

                        if Nkind (Par) = N_Package_Body then
                           Collect_Previous_Instances (Declarations (Par));

                        elsif Nkind (Par) = N_Package_Declaration then
                           Collect_Previous_Instances
                             (Visible_Declarations (Specification (Par)));
                           Collect_Previous_Instances
                             (Private_Declarations (Specification (Par)));

                        else
                           --  Enclosing unit is a subprogram body. In this
                           --  case all instance bodies are processed in order
                           --  and there is no need to collect them separately.

                           null;
                        end if;

                        Decl := First_Elmt (Previous_Instances);
                        while Present (Decl) loop
                           Info :=
                             (Inst_Node                => Node (Decl),
                              Act_Decl                 =>
                                Instance_Spec (Node (Decl)),
                              Fin_Scop                 => Empty,
                              Config_Switches          => Save_Config_Switches,
                              Current_Sem_Unit         =>
                                Get_Code_Unit (Sloc (Node (Decl))),
                              Expander_Status          => Exp_Status,
                              Local_Suppress_Stack_Top =>
                                Local_Suppress_Stack_Top,
                              Scope_Suppress           => Scope_Suppress,
                              Warnings                 => Save_Warnings);

                           --  Package instance

                           if Nkind (Node (Decl)) = N_Package_Instantiation
                           then
                              Instantiate_Package_Body
                                (Info, Body_Optional => True);

                           --  Subprogram instance

                           else
                              --  The instance_spec is in the wrapper package,
                              --  usually followed by its local renaming
                              --  declaration. See Build_Subprogram_Renaming
                              --  for details. If the instance carries aspects,
                              --  these result in the corresponding pragmas,
                              --  inserted after the subprogram declaration.
                              --  They must be skipped as well when retrieving
                              --  the desired spec. Some of them may have been
                              --  rewritten as null statements.
                              --  A direct link would be more robust ???

                              declare
                                 Decl : Node_Id :=
                                          (Last (Visible_Declarations
                                            (Specification (Info.Act_Decl))));
                              begin
                                 while Nkind (Decl) in
                                   N_Null_Statement                  |
                                   N_Pragma                          |
                                   N_Subprogram_Renaming_Declaration
                                 loop
                                    Decl := Prev (Decl);
                                 end loop;

                                 Info.Act_Decl := Decl;
                              end;

                              Instantiate_Subprogram_Body
                                (Info, Body_Optional => True);
                           end if;

                           Next_Elmt (Decl);
                        end loop;
                     end;
                  end if;

                  Instantiate_Package_Body
                    (Body_Info =>
                       ((Inst_Node                => Inst_Node,
                         Act_Decl                 => True_Parent,
                         Fin_Scop                 => Empty,
                         Config_Switches          => Save_Config_Switches,
                         Current_Sem_Unit         =>
                           Get_Code_Unit (Sloc (Inst_Node)),
                         Expander_Status          => Exp_Status,
                         Local_Suppress_Stack_Top => Local_Suppress_Stack_Top,
                         Scope_Suppress           => Scope_Suppress,
                         Warnings                 => Save_Warnings)),
                     Body_Optional => Body_Optional);
               end;
            end if;

         --  Case where we are not instantiating a nested generic

         else
            Opt.Style_Check := False;
            Expander_Mode_Save_And_Set (True);
            Load_Needed_Body (Comp_Unit, OK);
            Opt.Style_Check := Saved_Style_Check;
            Restore_Warnings (Saved_Warn);
            Expander_Mode_Restore;

            if not OK
              and then Unit_Requires_Body (Defining_Entity (Spec))
              and then not Body_Optional
            then
               declare
                  Bname : constant Unit_Name_Type :=
                            Get_Body_Name (Get_Unit_Name (Unit (Comp_Unit)));

               begin
                  --  In CodePeer mode, the missing body may make the analysis
                  --  incomplete, but we do not treat it as fatal.

                  if CodePeer_Mode then
                     return;

                  else
                     Error_Msg_Unit_1 := Bname;
                     Error_Msg_N ("this instantiation requires$!", N);
                     Error_Msg_File_1 :=
                       Get_File_Name (Bname, Subunit => False);
                     Error_Msg_N ("\but file{ was not found!", N);
                     raise Unrecoverable_Error;
                  end if;
               end;
            end if;
         end if;
      end if;

      --  If loading parent of the generic caused an instantiation circularity,
      --  we abandon compilation at this point, because otherwise in some cases
      --  we get into trouble with infinite recursions after this point.

      if Circularity_Detected then
         raise Unrecoverable_Error;
      end if;
   end Load_Parent_Of_Generic;

   ---------------------------------
   -- Map_Formal_Package_Entities --
   ---------------------------------

   procedure Map_Formal_Package_Entities (Form : Entity_Id; Act : Entity_Id) is
      E1 : Entity_Id;
      E2 : Entity_Id;

   begin
      Set_Instance_Of (Form, Act);

      --  Traverse formal and actual package to map the corresponding entities.
      --  We skip over internal entities that may be generated during semantic
      --  analysis, and find the matching entities by name, given that they
      --  must appear in the same order.

      E1 := First_Entity (Form);
      E2 := First_Entity (Act);
      while Present (E1) and then E1 /= First_Private_Entity (Form) loop
         --  Could this test be a single condition??? Seems like it could, and
         --  isn't FPE (Form) a constant anyway???

         if not Is_Internal (E1)
           and then Present (Parent (E1))
           and then not Is_Class_Wide_Type (E1)
           and then not Is_Internal_Name (Chars (E1))
         then
            while Present (E2) and then Chars (E2) /= Chars (E1) loop
               Next_Entity (E2);
            end loop;

            if No (E2) then
               exit;
            else
               Set_Instance_Of (E1, E2);

               if Is_Type (E1) and then Is_Tagged_Type (E2) then
                  Set_Instance_Of (Class_Wide_Type (E1), Class_Wide_Type (E2));
               end if;

               if Is_Constrained (E1) then
                  Set_Instance_Of (Base_Type (E1), Base_Type (E2));
               end if;

               if Ekind (E1) = E_Package and then No (Renamed_Entity (E1)) then
                  Map_Formal_Package_Entities (E1, E2);
               end if;
            end if;
         end if;

         Next_Entity (E1);
      end loop;
   end Map_Formal_Package_Entities;

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
      --  Check whether entity is declared in a scope external to that of the
      --  generic unit.

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

         case Nkind (Decl) is
            when N_Package_Declaration =>
               Spec := Specification (Decl);

            when N_Task_Type_Declaration =>
               Spec := Task_Definition (Decl);

            when N_Protected_Type_Declaration =>
               Spec := Protected_Definition (Decl);

            when others =>
               Spec := Empty;
         end case;

         if Present (Spec) then
            Move_Freeze_Nodes (Out_Of, Next_Node, Visible_Declarations (Spec));
            Move_Freeze_Nodes (Out_Of, Next_Node, Private_Declarations (Spec));
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

   procedure Preanalyze_Actuals (N : Node_Id; Inst : Entity_Id := Empty) is
      procedure Perform_Appropriate_Analysis (N : Node_Id);
      --  Determine if the actuals we are analyzing come from a generic
      --  instantiation that is a library unit and dispatch accordingly.

      ----------------------------------
      -- Perform_Appropriate_Analysis --
      ----------------------------------

      procedure Perform_Appropriate_Analysis (N : Node_Id) is
      begin
         --  When we have a library instantiation we cannot allow any expansion
         --  to occur, since there may be no place to put it. Instead, in that
         --  case we perform a preanalysis of the actual.

         if Present (Inst) and then Is_Compilation_Unit (Inst) then
            Preanalyze (N);
         else
            Analyze (N);
         end if;
      end Perform_Appropriate_Analysis;

      --  Local variables

      Errs : constant Nat := Serious_Errors_Detected;

      Assoc : Node_Id;
      Act   : Node_Id;

      Cur : Entity_Id := Empty;
      --  Current homograph of the instance name

      Vis : Boolean := False;
      --  Saved visibility status of the current homograph

   --  Start of processing for Preanalyze_Actuals

   begin
      Assoc := First (Generic_Associations (N));

      --  If the instance is a child unit, its name may hide an outer homonym,
      --  so make it invisible to perform name resolution on the actuals.

      if Nkind (Defining_Unit_Name (N)) = N_Defining_Program_Unit_Name
        and then Present
          (Current_Entity (Defining_Identifier (Defining_Unit_Name (N))))
      then
         Cur := Current_Entity (Defining_Identifier (Defining_Unit_Name (N)));

         if Is_Compilation_Unit (Cur) then
            Vis := Is_Immediately_Visible (Cur);
            Set_Is_Immediately_Visible (Cur, False);
         else
            Cur := Empty;
         end if;
      end if;

      while Present (Assoc) loop
         if Nkind (Assoc) /= N_Others_Choice then
            Act := Explicit_Generic_Actual_Parameter (Assoc);

            --  Within a nested instantiation, a defaulted actual is an empty
            --  association, so nothing to analyze. If the subprogram actual
            --  is an attribute, analyze prefix only, because actual is not a
            --  complete attribute reference.

            --  If actual is an allocator, analyze expression only. The full
            --  analysis can generate code, and if instance is a compilation
            --  unit we have to wait until the package instance is installed
            --  to have a proper place to insert this code.

            --  String literals may be operators, but at this point we do not
            --  know whether the actual is a formal subprogram or a string.

            if No (Act) then
               null;

            elsif Nkind (Act) = N_Attribute_Reference then
               Perform_Appropriate_Analysis (Prefix (Act));

            elsif Nkind (Act) = N_Explicit_Dereference then
               Perform_Appropriate_Analysis (Prefix (Act));

            elsif Nkind (Act) = N_Allocator then
               declare
                  Expr : constant Node_Id := Expression (Act);

               begin
                  if Nkind (Expr) = N_Subtype_Indication then
                     Perform_Appropriate_Analysis (Subtype_Mark (Expr));

                     --  Analyze separately each discriminant constraint, when
                     --  given with a named association.

                     declare
                        Constr : Node_Id;

                     begin
                        Constr := First (Constraints (Constraint (Expr)));
                        while Present (Constr) loop
                           if Nkind (Constr) = N_Discriminant_Association then
                              Perform_Appropriate_Analysis
                                (Expression (Constr));
                           else
                              Perform_Appropriate_Analysis (Constr);
                           end if;

                           Next (Constr);
                        end loop;
                     end;

                  else
                     Perform_Appropriate_Analysis (Expr);
                  end if;
               end;

            elsif Nkind (Act) /= N_Operator_Symbol then
               Perform_Appropriate_Analysis (Act);

               --  Within a package instance, mark actuals that are limited
               --  views, so their use can be moved to the body of the
               --  enclosing unit.

               if Is_Entity_Name (Act)
                 and then Is_Type (Entity (Act))
                 and then From_Limited_With (Entity (Act))
                 and then Present (Inst)
               then
                  Append_Elmt (Entity (Act), Incomplete_Actuals (Inst));
               end if;
            end if;

            if Errs /= Serious_Errors_Detected then

               --  Do a minimal analysis of the generic, to prevent spurious
               --  warnings complaining about the generic being unreferenced,
               --  before abandoning the instantiation.

               Perform_Appropriate_Analysis (Name (N));

               if Is_Entity_Name (Name (N))
                 and then Etype (Name (N)) /= Any_Type
               then
                  Generate_Reference  (Entity (Name (N)), Name (N));
                  Set_Is_Instantiated (Entity (Name (N)));
               end if;

               if Present (Cur) then

                  --  For the case of a child instance hiding an outer homonym,
                  --  provide additional warning which might explain the error.

                  Set_Is_Immediately_Visible (Cur, Vis);
                  Error_Msg_NE
                    ("& hides outer unit with the same name??",
                     N, Defining_Unit_Name (N));
               end if;

               Abandon_Instantiation (Act);
            end if;
         end if;

         Next (Assoc);
      end loop;

      if Present (Cur) then
         Set_Is_Immediately_Visible (Cur, Vis);
      end if;
   end Preanalyze_Actuals;

   -------------------------------
   -- Provide_Completing_Bodies --
   -------------------------------

   procedure Provide_Completing_Bodies (N : Node_Id) is
      procedure Build_Completing_Body (Subp_Decl : Node_Id);
      --  Generate the completing body for subprogram declaration Subp_Decl

      procedure Provide_Completing_Bodies_In (Decls : List_Id);
      --  Generating completing bodies for all subprograms found in declarative
      --  list Decls.

      ---------------------------
      -- Build_Completing_Body --
      ---------------------------

      procedure Build_Completing_Body (Subp_Decl : Node_Id) is
         Loc     : constant Source_Ptr := Sloc (Subp_Decl);
         Subp_Id : constant Entity_Id  := Defining_Entity (Subp_Decl);
         Spec    : Node_Id;

      begin
         --  Nothing to do if the subprogram already has a completing body

         if Present (Corresponding_Body (Subp_Decl)) then
            return;

         --  Mark the function as having a valid return statement even though
         --  the body contains a single raise statement.

         elsif Ekind (Subp_Id) = E_Function then
            Set_Return_Present (Subp_Id);
         end if;

         --  Clone the specification to obtain new entities and reset the only
         --  semantic field.

         Spec := Copy_Subprogram_Spec (Specification (Subp_Decl));
         Set_Generic_Parent (Spec, Empty);

         --  Generate:
         --    function Func ... return ... is
         --      <or>
         --    procedure Proc ... is
         --    begin
         --       raise Program_Error with "access before elaboration";
         --    edn Proc;

         Insert_After_And_Analyze (Subp_Decl,
           Make_Subprogram_Body (Loc,
             Specification              => Spec,
             Declarations               => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Raise_Program_Error (Loc,
                     Reason => PE_Access_Before_Elaboration)))));
      end Build_Completing_Body;

      ----------------------------------
      -- Provide_Completing_Bodies_In --
      ----------------------------------

      procedure Provide_Completing_Bodies_In (Decls : List_Id) is
         Decl : Node_Id;

      begin
         if Present (Decls) then
            Decl := First (Decls);
            while Present (Decl) loop
               Provide_Completing_Bodies (Decl);
               Next (Decl);
            end loop;
         end if;
      end Provide_Completing_Bodies_In;

      --  Local variables

      Spec : Node_Id;

   --  Start of processing for Provide_Completing_Bodies

   begin
      if Nkind (N) = N_Package_Declaration then
         Spec := Specification (N);

         Push_Scope (Defining_Entity (N));
         Provide_Completing_Bodies_In (Visible_Declarations (Spec));
         Provide_Completing_Bodies_In (Private_Declarations (Spec));
         Pop_Scope;

      elsif Nkind (N) = N_Subprogram_Declaration then
         Build_Completing_Body (N);
      end if;
   end Provide_Completing_Bodies;

   -------------------
   -- Remove_Parent --
   -------------------

   procedure Remove_Parent (In_Body : Boolean := False) is
      S : Entity_Id := Current_Scope;
      --  S is the scope containing the instantiation just completed. The scope
      --  stack contains the parent instances of the instantiation, followed by
      --  the original S.

      Cur_P  : Entity_Id;
      E      : Entity_Id;
      P      : Entity_Id;
      Hidden : Elmt_Id;

   begin
      --  After child instantiation is complete, remove from scope stack the
      --  extra copy of the current scope, and then remove parent instances.

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

               --  If instantiation is declared in a block, it is the enclosing
               --  scope that might be a parent instance. Note that only one
               --  block can be involved, because the parent instances have
               --  been installed within it.

               if Ekind (P) = E_Block then
                  Cur_P := Scope (P);
               else
                  Cur_P := P;
               end if;

               if Is_Generic_Instance (Cur_P) and then P /= Current_Scope then
                  --  We are within an instance of some sibling. Retain
                  --  visibility of parent, for proper subsequent cleanup, and
                  --  reinstall private declarations as well.

                  Set_In_Private_Part (P);
                  Install_Private_Declarations (P);
               end if;

            --  If the ultimate parent is a top-level unit recorded in
            --  Instance_Parent_Unit, then reset its visibility to what it was
            --  before instantiation. (It's not clear what the purpose is of
            --  testing whether Scope (P) is In_Open_Scopes, but that test was
            --  present before the ultimate parent test was added.???)

            elsif not In_Open_Scopes (Scope (P))
              or else (P = Instance_Parent_Unit
                        and then not Parent_Unit_Visible)
            then
               Set_Is_Immediately_Visible (P, False);

            --  If the current scope is itself an instantiation of a generic
            --  nested within P, and we are in the private part of body of this
            --  instantiation, restore the full views of P, that were removed
            --  in End_Package_Scope above. This obscure case can occur when a
            --  subunit of a generic contains an instance of a child unit of
            --  its generic parent unit.

            elsif S = Current_Scope and then Is_Generic_Instance (S)
              and then (In_Package_Body (S) or else In_Private_Part (S))
            then
               declare
                  Par : constant Entity_Id :=
                          Generic_Parent (Package_Specification (S));
               begin
                  if Present (Par)
                    and then P = Scope (Par)
                  then
                     Set_In_Private_Part (P);
                     Install_Private_Declarations (P);
                  end if;
               end;
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
         --  Each body is analyzed separately, and there is no context that
         --  needs preserving from one body instance to the next, so remove all
         --  parent scopes that have been installed.

         while Present (S) loop
            End_Package_Scope (S);
            Set_Is_Immediately_Visible (S, False);
            S := Current_Scope;
            exit when S = Standard_Standard;
         end loop;
      end if;
   end Remove_Parent;

   -----------------------------------
   -- Requires_Conformance_Checking --
   -----------------------------------

   function Requires_Conformance_Checking (N : Node_Id) return Boolean is
   begin
      --  No conformance checking required if the generic actual part is empty,
      --  or is a box or an others_clause (necessarily with a box).

      return Present (Generic_Associations (N))
        and then not Box_Present (N)
        and then Nkind (First (Generic_Associations (N))) /= N_Others_Choice;
   end Requires_Conformance_Checking;

   -----------------
   -- Restore_Env --
   -----------------

   procedure Restore_Env is
      Saved : Instance_Env renames Instance_Envs.Table (Instance_Envs.Last);

   begin
      if No (Current_Instantiated_Parent.Act_Id) then
         --  Restore environment after subprogram inlining

         Restore_Private_Views (Empty);
      end if;

      Current_Instantiated_Parent := Saved.Instantiated_Parent;
      Exchanged_Views             := Saved.Exchanged_Views;
      Hidden_Entities             := Saved.Hidden_Entities;
      Current_Sem_Unit            := Saved.Current_Sem_Unit;
      Parent_Unit_Visible         := Saved.Parent_Unit_Visible;
      Instance_Parent_Unit        := Saved.Instance_Parent_Unit;

      Restore_Config_Switches (Saved.Switches);

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
      --  Hide the generic formals of formal packages declared with box which
      --  were reachable in the current instantiation.

      ---------------------------
      -- Restore_Nested_Formal --
      ---------------------------

      procedure Restore_Nested_Formal (Formal : Entity_Id) is
         pragma Assert (Ekind (Formal) = E_Package);
         Ent : Entity_Id;
      begin
         if Present (Renamed_Entity (Formal))
           and then Denotes_Formal_Package (Renamed_Entity (Formal), True)
         then
            return;

         elsif Present (Associated_Formal_Package (Formal)) then
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

   --  Start of processing for Restore_Private_Views

   begin
      M := First_Elmt (Exchanged_Views);
      while Present (M) loop
         Typ := Node (M);

         --  Subtypes of types whose views have been exchanged, and that are
         --  defined within the instance, were not on the Private_Dependents
         --  list on entry to the instance, so they have to be exchanged
         --  explicitly now, in order to remain consistent with the view of the
         --  parent type.

         if Ekind (Typ) in E_Private_Type
                         | E_Limited_Private_Type
                         | E_Record_Type_With_Private
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

         Exchange_Declarations (Typ);
         Next_Elmt (M);
      end loop;

      if No (Pack_Id) then
         return;
      end if;

      --  Make the generic formal parameters private, and make the formal types
      --  into subtypes of the actuals again.

      E := First_Entity (Pack_Id);
      while Present (E) loop
         Set_Is_Hidden (E, True);

         if Is_Type (E)
           and then Nkind (Parent (E)) = N_Subtype_Declaration
         then
            --  Always preserve the flag Is_Generic_Actual_Type for GNATprove,
            --  as it is needed to identify the subtype with the type it
            --  renames, when there are conversions between access types
            --  to these.

            if GNATprove_Mode then
               null;

            --  If the actual for E is itself a generic actual type from
            --  an enclosing instance, E is still a generic actual type
            --  outside of the current instance. This matter when resolving
            --  an overloaded call that may be ambiguous in the enclosing
            --  instance, when two of its actuals coincide.

            elsif Is_Entity_Name (Subtype_Indication (Parent (E)))
              and then Is_Generic_Actual_Type
                         (Entity (Subtype_Indication (Parent (E))))
            then
               null;
            else
               Set_Is_Generic_Actual_Type (E, False);

               --  It might seem reasonable to clear the Is_Generic_Actual_Type
               --  flag also on the Full_View if the type is private, since it
               --  was set also on this Full_View. However, this flag is relied
               --  upon by Covers to spot "types exported from instantiations"
               --  which are implicit Full_Views built for instantiations made
               --  on private types and we get type mismatches if we do it when
               --  the block exchanging the declarations below triggers ???

               --  if Is_Private_Type (E) and then Present (Full_View (E)) then
               --    Set_Is_Generic_Actual_Type (Full_View (E), False);
               --  end if;
            end if;

            --  An unusual case of aliasing: the actual may also be directly
            --  visible in the generic, and be private there, while it is fully
            --  visible in the context of the instance. The internal subtype
            --  is private in the instance but has full visibility like its
            --  parent in the enclosing scope. This enforces the invariant that
            --  the privacy status of all private dependents of a type coincide
            --  with that of the parent type. This can only happen when a
            --  generic child unit is instantiated within a sibling.

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
            --  actual in this case is itself the renaming of an instantiation.
            --  If the entity is not a package renaming, it is the entity
            --  created to validate formal package actuals: ignore it.

            --  If the actual is itself a formal package for the enclosing
            --  generic, or the actual for such a formal package, it remains
            --  visible on exit from the instance, and therefore nothing needs
            --  to be done either, except to keep it accessible.

            if Is_Package and then Renamed_Entity (E) = Pack_Id then
               exit;

            elsif Nkind (Parent (E)) /= N_Package_Renaming_Declaration then
               null;

            elsif
              Denotes_Formal_Package (Renamed_Entity (E), True, Pack_Id)
            then
               Set_Is_Hidden (E, False);

            else
               declare
                  Act_P : constant Entity_Id := Renamed_Entity (E);
                  Id    : Entity_Id;

               begin
                  Id := First_Entity (Act_P);
                  while Present (Id)
                    and then Id /= First_Private_Entity (Act_P)
                  loop
                     exit when Ekind (Id) = E_Package
                                 and then Renamed_Entity (Id) = Act_P;

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

   procedure Save_Global_References (Templ : Node_Id) is

      --  ??? it is horrible to use global variables in highly recursive code

      E : Entity_Id;
      --  The entity of the current associated node

      Gen_Scope : Entity_Id;
      --  The scope of the generic for which references are being saved

      N2 : Node_Id;
      --  The current associated node

      function Is_Global (E : Entity_Id) return Boolean;
      --  Check whether entity is defined outside of generic unit. Examine the
      --  scope of an entity, and the scope of the scope, etc, until we find
      --  either Standard, in which case the entity is global, or the generic
      --  unit itself, which indicates that the entity is local. If the entity
      --  is the generic unit itself, as in the case of a recursive call, or
      --  the enclosing generic unit, if different from the current scope, then
      --  it is local as well, because it will be replaced at the point of
      --  instantiation. On the other hand, if it is a reference to a child
      --  unit of a common ancestor, which appears in an instantiation, it is
      --  global because it is used to denote a specific compilation unit at
      --  the time the instantiations will be analyzed.

      procedure Qualify_Universal_Operands
        (Op        : Node_Id;
         Func_Call : Node_Id);
      --  Op denotes a binary or unary operator in generic template Templ. Node
      --  Func_Call is the function call alternative of the operator within the
      --  the analyzed copy of the template. Change each operand which yields a
      --  universal type by wrapping it into a qualified expression
      --
      --    Actual_Typ'(Operand)
      --
      --  where Actual_Typ is the type of corresponding actual parameter of
      --  Operand in Func_Call.

      procedure Reset_Entity (N : Node_Id);
      --  Save semantic information on global entity so that it is not resolved
      --  again at instantiation time.

      procedure Save_Entity_Descendants (N : Node_Id);
      --  Apply Save_Global_References to the two syntactic descendants of
      --  non-terminal nodes that carry an Associated_Node and are processed
      --  through Reset_Entity. Once the global entity (if any) has been
      --  captured together with its type, only two syntactic descendants need
      --  to be traversed to complete the processing of the tree rooted at N.
      --  This applies to Selected_Components, Expanded_Names, and to Operator
      --  nodes. N can also be a character literal, identifier, or operator
      --  symbol node, but the call has no effect in these cases.

      procedure Save_Global_Defaults (N1 : Node_Id; N2 : Node_Id);
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
      --  Apply Save_References recursively to the descendants of node D

      procedure Save_References (N : Node_Id);
      --  This is the recursive procedure that does the work, once the
      --  enclosing generic scope has been established.

      procedure Set_Global_Type (N : Node_Id; N2 : Node_Id);
      --  If the type of N2 is global to the generic unit, save the type in
      --  the generic node. Just as we perform name capture for explicit
      --  references within the generic, we must capture the global types
      --  of local entities because they may participate in resolution in
      --  the instance.

      ---------------
      -- Is_Global --
      ---------------

      function Is_Global (E : Entity_Id) return Boolean is
         Se : Entity_Id;

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
            return Nkind (Decl) in N_Generic_Instantiation
                     or else
                   Nkind (Original_Node (Decl)) = N_Formal_Package_Declaration;
         end Is_Instance_Node;

      --  Start of processing for Is_Global

      begin
         if E = Gen_Scope then
            return False;

         elsif E = Standard_Standard then
            return True;

         --  E should be an entity, but it is not always

         elsif Nkind (E) not in N_Entity then
            return False;

         elsif Nkind (E) /= N_Expanded_Name
           and then Is_Child_Unit (E)
           and then (Is_Instance_Node (Parent (N2))
                      or else (Nkind (Parent (N2)) = N_Expanded_Name
                                and then N2 = Selector_Name (Parent (N2))
                                and then
                                  Is_Instance_Node (Parent (Parent (N2)))))
         then
            return True;

         else
            --  E may be an expanded name - typically an operator - in which
            --  case we must find its enclosing scope since expanded names
            --  don't have corresponding scopes.

            if Nkind (E) = N_Expanded_Name then
               Se := Find_Enclosing_Scope (E);

            --  Otherwise, E is an entity and will have Scope set

            else
               Se := Scope (E);
            end if;

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

      --------------------------------
      -- Qualify_Universal_Operands --
      --------------------------------

      procedure Qualify_Universal_Operands
        (Op        : Node_Id;
         Func_Call : Node_Id)
      is
         procedure Qualify_Operand (Opnd : Node_Id; Actual : Node_Id);
         --  Rewrite operand Opnd as a qualified expression of the form
         --
         --    Actual_Typ'(Opnd)
         --
         --  where Actual is the corresponding actual parameter of Opnd in
         --  function call Func_Call.

         function Qualify_Type
           (Loc : Source_Ptr;
            Typ : Entity_Id) return Node_Id;
         --  Qualify type Typ by creating a selected component of the form
         --
         --    Scope_Of_Typ.Typ

         ---------------------
         -- Qualify_Operand --
         ---------------------

         procedure Qualify_Operand (Opnd : Node_Id; Actual : Node_Id) is
            Loc  : constant Source_Ptr := Sloc (Opnd);
            Typ  : constant Entity_Id  := Etype (Actual);
            Mark : Node_Id;
            Qual : Node_Id;

         begin
            --  Qualify the operand when it is of a universal type. Note that
            --  the template is unanalyzed and it is not possible to directly
            --  query the type. This transformation is not done when the type
            --  of the actual is internally generated because the type will be
            --  regenerated in the instance.

            if Yields_Universal_Type (Opnd)
              and then Comes_From_Source (Typ)
              and then not Is_Hidden (Typ)
            then
               --  The type of the actual may be a global reference. Save this
               --  information by creating a reference to it.

               if Is_Global (Typ) then
                  Mark := New_Occurrence_Of (Typ, Loc);

               --  Otherwise rely on resolution to find the proper type within
               --  the instance.

               else
                  Mark := Qualify_Type (Loc, Typ);
               end if;

               Qual :=
                 Make_Qualified_Expression (Loc,
                   Subtype_Mark => Mark,
                   Expression   => Relocate_Node (Opnd));

               --  Mark the qualification to distinguish it from other source
               --  constructs and signal the instantiation mechanism that this
               --  node requires special processing. See Copy_Generic_Node for
               --  details.

               Set_Is_Qualified_Universal_Literal (Qual);

               Rewrite (Opnd, Qual);
            end if;
         end Qualify_Operand;

         ------------------
         -- Qualify_Type --
         ------------------

         function Qualify_Type
           (Loc : Source_Ptr;
            Typ : Entity_Id) return Node_Id
         is
            Scop   : constant Entity_Id := Scope (Typ);
            Result : Node_Id;

         begin
            Result := Make_Identifier (Loc, Chars (Typ));

            if Present (Scop) and then not Is_Generic_Unit (Scop) then
               Result :=
                 Make_Selected_Component (Loc,
                   Prefix        => Make_Identifier (Loc, Chars (Scop)),
                   Selector_Name => Result);
            end if;

            return Result;
         end Qualify_Type;

         --  Local variables

         Actuals : constant List_Id := Parameter_Associations (Func_Call);

      --  Start of processing for Qualify_Universal_Operands

      begin
         if Nkind (Op) in N_Binary_Op then
            Qualify_Operand (Left_Opnd  (Op), First (Actuals));
            Qualify_Operand (Right_Opnd (Op), Next (First (Actuals)));

         elsif Nkind (Op) in N_Unary_Op then
            Qualify_Operand (Right_Opnd (Op), First (Actuals));
         end if;
      end Qualify_Universal_Operands;

      ------------------
      -- Reset_Entity --
      ------------------

      procedure Reset_Entity (N : Node_Id) is
         function Top_Ancestor (E : Entity_Id) return Entity_Id;
         --  Find the ultimate ancestor of the current unit. If it is not a
         --  generic unit, then the name of the current unit in the prefix of
         --  an expanded name must be replaced with its generic homonym to
         --  ensure that it will be properly resolved in an instance.

         ------------------
         -- Top_Ancestor --
         ------------------

         function Top_Ancestor (E : Entity_Id) return Entity_Id is
            Par : Entity_Id;

         begin
            Par := E;
            while Is_Child_Unit (Par) loop
               Par := Scope (Par);
            end loop;

            return Par;
         end Top_Ancestor;

      --  Start of processing for Reset_Entity

      begin
         N2 := Get_Associated_Node (N);
         E  := Entity (N2);

         if Present (E) then

            --  If the node is an entry call to an entry in an enclosing task,
            --  it is rewritten as a selected component. No global entity to
            --  preserve in this case, since the expansion will be redone in
            --  the instance.

            if Nkind (E) not in N_Entity then
               Set_Associated_Node (N, Empty);
               Set_Etype (N, Empty);
               return;
            end if;

            --  If the entity is an itype created as a subtype of an access
            --  type with a null exclusion restore source entity for proper
            --  visibility. The itype will be created anew in the instance.

            if Is_Itype (E)
              and then Ekind (E) = E_Access_Subtype
              and then Is_Entity_Name (N)
              and then Chars (Etype (E)) = Chars (N)
            then
               E := Etype (E);
               Set_Entity (N2, E);
               Set_Etype  (N2, E);
            end if;

            if Is_Global (E) then
               Set_Global_Type (N, N2);

            elsif Nkind (N) = N_Op_Concat
              and then Is_Generic_Type (Etype (N2))
              and then (Base_Type (Etype (Right_Opnd (N2))) = Etype (N2)
                          or else
                        Base_Type (Etype (Left_Opnd  (N2))) = Etype (N2))
              and then Is_Intrinsic_Subprogram (E)
            then
               null;

            --  Entity is local. Mark generic node as unresolved. Note that now
            --  it does not have an entity.

            else
               Set_Associated_Node (N, Empty);
               Set_Etype (N, Empty);
            end if;

            if Nkind (Parent (N)) in N_Generic_Instantiation
              and then N = Name (Parent (N))
            then
               Save_Global_Defaults (Parent (N), Parent (N2));
            end if;

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then Nkind (Parent (N2)) = N_Expanded_Name
         then
            --  In case of previous errors, the tree might be malformed

            if No (Entity (Parent (N2))) then
               null;

            elsif Is_Global (Entity (Parent (N2))) then
               Change_Selected_Component_To_Expanded_Name (Parent (N));
               Set_Associated_Node (Parent (N), Parent (N2));
               Set_Global_Type     (Parent (N), Parent (N2));
               Save_Entity_Descendants (N);

            --  If this is a reference to the current generic entity, replace
            --  by the name of the generic homonym of the current package. This
            --  is because in an instantiation Par.P.Q will not resolve to the
            --  name of the instance, whose enclosing scope is not necessarily
            --  Par. We use the generic homonym rather that the name of the
            --  generic itself because it may be hidden by a local declaration.

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
                 (Parent (Parent (N)), Parent (Parent (N2)));
            end if;

         --  A selected component may denote a static constant that has been
         --  folded. If the static constant is global to the generic, capture
         --  its value. Otherwise the folding will happen in any instantiation.

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then Nkind (Parent (N2)) in N_Integer_Literal | N_Real_Literal
         then
            if Present (Entity (Original_Node (Parent (N2))))
              and then Is_Global (Entity (Original_Node (Parent (N2))))
            then
               Rewrite (Parent (N), New_Copy (Parent (N2)));
               Set_Analyzed (Parent (N), False);
            end if;

         --  A selected component may be transformed into a parameterless
         --  function call. If the called entity is global, rewrite the node
         --  appropriately, i.e. as an extended name for the global entity.

         elsif Nkind (Parent (N)) = N_Selected_Component
           and then Nkind (Parent (N2)) = N_Function_Call
           and then N = Selector_Name (Parent (N))
         then
            if No (Parameter_Associations (Parent (N2))) then
               if Is_Global (Entity (Name (Parent (N2)))) then
                  Change_Selected_Component_To_Expanded_Name (Parent (N));
                  Set_Associated_Node (Parent (N), Name (Parent (N2)));
                  Set_Global_Type     (Parent (N), Name (Parent (N2)));
                  Save_Entity_Descendants (N);

               else
                  Set_Is_Prefixed_Call (Parent (N));
                  Set_Associated_Node (N, Empty);
                  Set_Etype (N, Empty);
               end if;

            --  In Ada 2005, X.F may be a call to a primitive operation,
            --  rewritten as F (X). This rewriting will be done again in an
            --  instance, so keep the original node. Global entities will be
            --  captured as for other constructs. Indicate that this must
            --  resolve as a call, to prevent accidental overloading in the
            --  instance, if both a component and a primitive operation appear
            --  as candidates.

            else
               Set_Is_Prefixed_Call (Parent (N));
            end if;

         --  Entity is local. Reset in generic unit, so that node is resolved
         --  anew at the point of instantiation.

         else
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
               Save_Global_Descendant (Union_Id (Left_Opnd  (N)));
               Save_Global_Descendant (Union_Id (Right_Opnd (N)));

            when N_Unary_Op =>
               Save_Global_Descendant (Union_Id (Right_Opnd (N)));

            when N_Expanded_Name
               | N_Selected_Component
            =>
               Save_Global_Descendant (Union_Id (Prefix (N)));
               Save_Global_Descendant (Union_Id (Selector_Name (N)));

            when N_Character_Literal
               | N_Identifier
               | N_Operator_Symbol
            =>
               null;

            when others =>
               raise Program_Error;
         end case;
      end Save_Entity_Descendants;

      --------------------------
      -- Save_Global_Defaults --
      --------------------------

      procedure Save_Global_Defaults (N1 : Node_Id; N2 : Node_Id) is
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

         --  Find the associations added for default subprograms

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
                      Selector_Name                     =>
                        New_Occurrence_Of (Subp, Loc),
                      Explicit_Generic_Actual_Parameter =>
                        New_Occurrence_Of (Actual, Loc));

                  Set_Associated_Node
                    (Explicit_Generic_Actual_Parameter (Ndec), Def);

                  Append (Ndec, Assoc1);

               --  If there are other defaults, add a dummy association in case
               --  there are other defaulted formals with the same name.
               --  Note that we are creating an N_Generic_Association with
               --  neither Explicit_Generic_Actual_Parameter nor Box_Present.

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
            --  This is an instantiation of a child unit within a sibling, so
            --  that the generic parent is in scope. An eventual instance must
            --  occur within the scope of an instance of the parent. Make name
            --  in instance into an expanded name, to preserve the identifier
            --  of the parent, so it can be resolved subsequently.

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
            pragma Assert (D /= Union_Id (No_List));
            --  Because No_List = Empty, which is in Node_Range above

            N1 := First (List_Id (D));
            while Present (N1) loop
               Save_References (N1);
               Next (N1);
            end loop;

         --  Element list or other non-node field, nothing to do

         else
            null;
         end if;
      end Save_Global_Descendant;

      ---------------------
      -- Save_References --
      ---------------------

      --  This is the recursive procedure that does the work once the enclosing
      --  generic scope has been established. We have to treat specially a
      --  number of node rewritings that are required by semantic processing
      --  and which change the kind of nodes in the generic copy: typically
      --  constant-folding, replacing an operator node by a string literal, or
      --  a selected component by an expanded name. In each of those cases, the
      --  transformation is propagated to the generic unit.

      procedure Save_References (N : Node_Id) is
         Loc : constant Source_Ptr := Sloc (N);

         function Requires_Delayed_Save (Nod : Node_Id) return Boolean;
         --  Determine whether arbitrary node Nod requires delayed capture of
         --  global references within its aspect specifications.

         procedure Save_References_In_Aggregate (N : Node_Id);
         --  Save all global references in [extension] aggregate node N

         procedure Save_References_In_Char_Lit_Or_Op_Symbol (N : Node_Id);
         --  Save all global references in a character literal or operator
         --  symbol denoted by N.

         procedure Save_References_In_Descendants (N : Node_Id);
         --  Save all global references in all descendants of node N

         procedure Save_References_In_Identifier (N : Node_Id);
         --  Save all global references in identifier node N

         procedure Save_References_In_Operator (N : Node_Id);
         --  Save all global references in operator node N

         procedure Save_References_In_Pragma (Prag : Node_Id);
         --  Save all global references found within the expression of pragma
         --  Prag.

         ---------------------------
         -- Requires_Delayed_Save --
         ---------------------------

         function Requires_Delayed_Save (Nod : Node_Id) return Boolean is
         begin
            --  Generic packages and subprograms require delayed capture of
            --  global references within their aspects due to the timing of
            --  annotation analysis.

            if Nkind (Nod) in N_Generic_Package_Declaration
                            | N_Generic_Subprogram_Declaration
                            | N_Package_Body
                            | N_Package_Body_Stub
                            | N_Subprogram_Body
                            | N_Subprogram_Body_Stub
            then
               --  Since the capture of global references is done on the
               --  unanalyzed generic template, there is no information around
               --  to infer the context. Use the Associated_Entity linkages to
               --  peek into the analyzed generic copy and determine what the
               --  template corresponds to.

               if Nod = Templ then
                  return
                    Is_Generic_Declaration_Or_Body
                      (Unit_Declaration_Node
                        (Get_Associated_Entity (Defining_Entity (Nod))));

               --  Otherwise the generic unit being processed is not the top
               --  level template. It is safe to capture of global references
               --  within the generic unit because at this point the top level
               --  copy is fully analyzed.

               else
                  return False;
               end if;

            --  Otherwise capture the global references without interference

            else
               return False;
            end if;
         end Requires_Delayed_Save;

         ----------------------------------
         -- Save_References_In_Aggregate --
         ----------------------------------

         procedure Save_References_In_Aggregate (N : Node_Id) is
            Nam   : Node_Id;
            Qual  : Node_Id   := Empty;
            Typ   : Entity_Id := Empty;

         begin
            N2 := Get_Associated_Node (N);

            if Present (N2) then
               Typ := Etype (N2);

               --  In an instance within a generic, use the name of the actual
               --  and not the original generic parameter. If the actual is
               --  global in the current generic it must be preserved for its
               --  instantiation.

               if Parent_Kind (Typ) = N_Subtype_Declaration
                 and then Present (Generic_Parent_Type (Parent (Typ)))
               then
                  Typ := Base_Type (Typ);
                  Set_Etype (N2, Typ);
               end if;
            end if;

            if No (N2) or else No (Typ) or else not Is_Global (Typ) then
               Set_Associated_Node (N, Empty);

               --  For a full aggregate, if the type is local but is a derived
               --  tagged type of a global ancestor, we will need to have the
               --  full view of this global ancestor available in the instance
               --  in order to analyze the full aggregate.

               if Present (N2)
                 and then Nkind (N2) = N_Aggregate
                 and then Present (Typ)
                 and then Is_Tagged_Type (Typ)
                 and then Is_Derived_Type (Typ)
               then
                  declare
                     Root_Typ : constant Entity_Id := Root_Type (Typ);

                     Parent_Typ : Entity_Id := Typ;

                  begin
                     loop
                        Parent_Typ := Etype (Parent_Typ);

                        if Is_Global (Parent_Typ) then
                           Set_Ancestor_Type (N, Parent_Typ);
                           exit;
                        end if;

                        exit when Parent_Typ = Root_Typ;
                     end loop;
                  end;
               end if;

               --  If the aggregate is an actual in a call, it has been
               --  resolved in the current context, to some local type. The
               --  enclosing call may have been disambiguated by the aggregate,
               --  and this disambiguation might fail at instantiation time
               --  because the type to which the aggregate did resolve is not
               --  preserved. In order to preserve some of this information,
               --  wrap the aggregate in a qualified expression, using the id
               --  of its type. For further disambiguation we qualify the type
               --  name with its scope (if visible and not hidden by a local
               --  homograph) because both id's will have corresponding
               --  entities in an instance. This resolves most of the problems
               --  with missing type information on aggregates in instances.

               if Present (N2)
                 and then Nkind (N2) = Nkind (N)
                 and then Nkind (Parent (N2)) in N_Subprogram_Call
                 and then Present (Typ)
                 and then Comes_From_Source (Typ)
               then
                  Nam := Make_Identifier (Loc, Chars (Typ));

                  if Is_Immediately_Visible (Scope (Typ))
                    and then
                      (not In_Open_Scopes (Scope (Typ))
                         or else Current_Entity (Scope (Typ)) = Scope (Typ))
                  then
                     Nam :=
                       Make_Selected_Component (Loc,
                         Prefix        =>
                           Make_Identifier (Loc, Chars (Scope (Typ))),
                         Selector_Name => Nam);
                  end if;

                  Qual :=
                    Make_Qualified_Expression (Loc,
                      Subtype_Mark => Nam,
                      Expression   => Relocate_Node (N));
               end if;

            --  For a full aggregate, if the type is global and a derived
            --  tagged type, we will also need to have the full view of its
            --  ancestor available in the instance in order to analyze the
            --  full aggregate.

            elsif Present (N2)
              and then Nkind (N2) = N_Aggregate
              and then Present (Typ)
              and then Is_Tagged_Type (Typ)
              and then Is_Derived_Type (Typ)
            then
               Set_Ancestor_Type (N, Etype (Typ));
            end if;

            if Nkind (N) = N_Aggregate then
               Save_Global_Descendant (Union_Id (Aggregate_Bounds (N)));

            elsif Nkind (N) = N_Extension_Aggregate then
               Save_Global_Descendant (Union_Id (Ancestor_Part (N)));

            else
               pragma Assert (False);
            end if;

            Save_Global_Descendant (Union_Id (Expressions (N)));
            Save_Global_Descendant (Union_Id (Component_Associations (N)));
            Save_Global_Descendant (Union_Id (Etype (N)));

            if Present (Qual) then
               Rewrite (N, Qual);
            end if;
         end Save_References_In_Aggregate;

         ----------------------------------------------
         -- Save_References_In_Char_Lit_Or_Op_Symbol --
         ----------------------------------------------

         procedure Save_References_In_Char_Lit_Or_Op_Symbol (N : Node_Id) is
         begin
            if Nkind (N) = Nkind (Get_Associated_Node (N)) then
               Reset_Entity (N);

            elsif Nkind (N) = N_Operator_Symbol
              and then Nkind (Get_Associated_Node (N)) = N_String_Literal
            then
               Change_Operator_Symbol_To_String_Literal (N);
            end if;
         end Save_References_In_Char_Lit_Or_Op_Symbol;

         ------------------------------------
         -- Save_References_In_Descendants --
         ------------------------------------

         procedure Save_References_In_Descendants (N : Node_Id) is
            procedure Walk is new Walk_Sinfo_Fields (Save_Global_Descendant);
         begin
            Walk (N);
         end Save_References_In_Descendants;

         -----------------------------------
         -- Save_References_In_Identifier --
         -----------------------------------

         procedure Save_References_In_Identifier (N : Node_Id) is
         begin
            --  The node did not undergo a transformation

            if Nkind (N) = Nkind (Get_Associated_Node (N)) then
               --  If this is a discriminant reference, always save it.
               --  It is used in the instance to find the corresponding
               --  discriminant positionally rather than by name.

               Set_Original_Discriminant
                 (N, Original_Discriminant (Get_Associated_Node (N)));

               Reset_Entity (N);

            --  The analysis of the generic copy transformed the identifier
            --  into another construct. Propagate the changes to the template.

            else
               N2 := Get_Associated_Node (N);

               --  The identifier denotes a call to a parameterless function.
               --  Mark the node as resolved when the function is external.

               if Nkind (N2) = N_Function_Call then
                  E := Entity (Name (N2));

                  if Present (E) and then Is_Global (E) then
                     Set_Global_Type (N, N2);
                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               --  The identifier denotes a named number that was constant
               --  folded. Preserve the original name for ASIS and undo the
               --  constant folding which will be repeated in the instance.
               --  Is this still needed???

               elsif Nkind (N2) in N_Integer_Literal | N_Real_Literal
                 and then Is_Entity_Name (Original_Node (N2))
               then
                  Set_Associated_Node (N, Original_Node (N2));
                  Reset_Entity (N);

               --  The identifier resolved to a string literal. Propagate this
               --  information to the generic template.

               elsif Nkind (N2) = N_String_Literal then
                  Rewrite (N, New_Copy (N2));

               --  The identifier is rewritten as a dereference if it is the
               --  prefix of an implicit dereference. Preserve the original
               --  tree as the analysis of the instance will expand the node
               --  again, but preserve the resolved entity if it is global.

               elsif Nkind (N2) = N_Explicit_Dereference then
                  if Is_Entity_Name (Prefix (N2))
                    and then Present (Entity (Prefix (N2)))
                    and then Is_Global (Entity (Prefix (N2)))
                  then
                     Set_Associated_Node (N, Prefix (N2));
                     Set_Global_Type (N, Prefix (N2));

                  elsif Nkind (Prefix (N2)) = N_Function_Call
                    and then Is_Entity_Name (Name (Prefix (N2)))
                    and then Present (Entity (Name (Prefix (N2))))
                    and then Is_Global (Entity (Name (Prefix (N2))))
                  then
                     Rewrite (N,
                       Make_Explicit_Dereference (Loc,
                         Prefix =>
                           Make_Function_Call (Loc,
                             Name =>
                               New_Occurrence_Of
                                 (Entity (Name (Prefix (N2))), Loc))));
                     Set_Associated_Node
                       (Name (Prefix (N)), Name (Prefix (N2)));
                     Set_Global_Type (Name (Prefix (N)), Name (Prefix (N2)));

                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype (N, Empty);
                  end if;

               --  The subtype mark of a nominally unconstrained object is
               --  rewritten as a subtype indication using the bounds of the
               --  expression. Recover the original subtype mark.

               elsif Nkind (N2) = N_Subtype_Indication
                 and then Is_Entity_Name (Original_Node (N2))
               then
                  Set_Associated_Node (N, Original_Node (N2));
                  Reset_Entity (N);
               end if;
            end if;
         end Save_References_In_Identifier;

         ---------------------------------
         -- Save_References_In_Operator --
         ---------------------------------

         procedure Save_References_In_Operator (N : Node_Id) is
         begin
            N2 := Get_Associated_Node (N);

            --  The node did not undergo a transformation

            if Nkind (N) = Nkind (N2) then
               if Nkind (N) = N_Op_Concat then
                  Set_Is_Component_Left_Opnd
                    (N, Is_Component_Left_Opnd (N2));
                  Set_Is_Component_Right_Opnd
                    (N, Is_Component_Right_Opnd (N2));
               end if;

               Reset_Entity (N);

            --  The analysis of the generic copy transformed the operator into
            --  some other construct. Propagate the changes to the template if
            --  applicable.

            else
               --  The operator resoved to a function call

               if Nkind (N2) = N_Function_Call then

                  --  Add explicit qualifications in the generic template for
                  --  all operands of universal type. This aids resolution by
                  --  preserving the actual type of a literal or an attribute
                  --  that yields a universal result.

                  Qualify_Universal_Operands (N, N2);

                  E := Entity (Name (N2));

                  if Present (E) and then Is_Global (E) then
                     Set_Global_Type (N, N2);
                  else
                     Set_Associated_Node (N, Empty);
                     Set_Etype           (N, Empty);
                  end if;

               --  The operator was folded into a literal

               elsif Nkind (N2) in N_Integer_Literal
                                 | N_Real_Literal
                                 | N_String_Literal
               then
                  if Present (Original_Node (N2))
                    and then Nkind (Original_Node (N2)) = Nkind (N)
                  then
                     --  Operation was constant-folded. Whenever possible,
                     --  recover semantic information from unfolded node.
                     --  This was initially done for ASIS but is apparently
                     --  needed also for e.g. compiling a-nbnbin.adb.

                     Set_Associated_Node (N, Original_Node (N2));

                     if Nkind (N) = N_Op_Concat then
                        Set_Is_Component_Left_Opnd (N,
                          Is_Component_Left_Opnd  (Get_Associated_Node (N)));
                        Set_Is_Component_Right_Opnd (N,
                          Is_Component_Right_Opnd (Get_Associated_Node (N)));
                     end if;

                     Reset_Entity (N);

                  --  Propagate the constant folding back to the template

                  else
                     Rewrite (N, New_Copy (N2));
                     Set_Analyzed (N, False);
                  end if;

               --  The operator was folded into an enumeration literal. Retain
               --  the entity to avoid spurious ambiguities if it is overloaded
               --  at the point of instantiation or inlining.

               elsif Nkind (N2) = N_Identifier
                 and then Ekind (Entity (N2)) = E_Enumeration_Literal
               then
                  Rewrite (N, New_Copy (N2));
                  Set_Analyzed (N, False);
               end if;
            end if;

            --  Complete the operands check if node has not been constant
            --  folded.

            if Nkind (N) in N_Op then
               Save_Entity_Descendants (N);
            end if;
         end Save_References_In_Operator;

         -------------------------------
         -- Save_References_In_Pragma --
         -------------------------------

         procedure Save_References_In_Pragma (Prag : Node_Id) is
            Context : Node_Id;
            Do_Save : Boolean := True;

         begin
            --  Do not save global references in pragmas generated from aspects
            --  because the pragmas will be regenerated at instantiation time.

            if From_Aspect_Specification (Prag) then
               Do_Save := False;

            --  The capture of global references within contract-related source
            --  pragmas associated with generic packages, subprograms or their
            --  respective bodies must be delayed due to timing of annotation
            --  analysis. Global references are still captured in routine
            --  Save_Global_References_In_Contract.

            elsif Is_Generic_Contract_Pragma (Prag) and then Prag /= Templ then
               if Is_Package_Contract_Annotation (Prag) then
                  Context := Find_Related_Package_Or_Body (Prag);
               else
                  pragma Assert (Is_Subprogram_Contract_Annotation (Prag));
                  Context := Find_Related_Declaration_Or_Body (Prag);
               end if;

               --  The use of Original_Node accounts for the case when the
               --  related context is generic template.

               if Requires_Delayed_Save (Original_Node (Context)) then
                  Do_Save := False;
               end if;
            end if;

            --  For all other cases, save all global references within the
            --  descendants, but skip the following semantic fields:
            --  Next_Pragma, Corresponding_Aspect, Next_Rep_Item.

            if Do_Save then
               Save_Global_Descendant
                 (Union_Id (Pragma_Argument_Associations (N)));
               Save_Global_Descendant (Union_Id (Pragma_Identifier (N)));
            end if;
         end Save_References_In_Pragma;

      --  Start of processing for Save_References

      begin
         if N = Empty then
            null;

         --  Aggregates

         elsif Nkind (N) in N_Aggregate | N_Extension_Aggregate then
            Save_References_In_Aggregate (N);

         --  Character literals, operator symbols

         elsif Nkind (N) in N_Character_Literal | N_Operator_Symbol then
            Save_References_In_Char_Lit_Or_Op_Symbol (N);

         --  Defining identifiers

         elsif Nkind (N) in N_Entity then
            null;

         --  Identifiers

         elsif Nkind (N) = N_Identifier then
            Save_References_In_Identifier (N);

         --  Operators

         elsif Nkind (N) in N_Op then
            Save_References_In_Operator (N);

         --  Pragmas

         elsif Nkind (N) = N_Pragma then
            Save_References_In_Pragma (N);

         elsif Nkind (N) =  N_Aspect_Specification then
            declare
               P : constant Node_Id := Parent (N);
               Expr : Node_Id;
            begin

               if Permits_Aspect_Specifications (P) then

                  --  The capture of global references within aspects
                  --  associated with generic packages, subprograms or
                  --  their bodies must be delayed due to timing of
                  --  annotation analysis. Global references are still
                  --  captured in routine Save_Global_References_In_Contract.

                  if Requires_Delayed_Save (Original_Node (P)) then
                     null;

                     --  Otherwise save all global references within the
                     --  aspects

                  else
                     Expr := Expression (N);

                     if Present (Expr) then
                        Save_Global_References (Expr);
                     end if;
                  end if;
               end if;
            end;

         --  Do not walk the node pointed to by Label_Construct twice

         elsif Nkind (N) = N_Implicit_Label_Declaration then
            null;

         else
            Save_References_In_Descendants (N);
         end if;

      end Save_References;

      ---------------------
      -- Set_Global_Type --
      ---------------------

      procedure Set_Global_Type (N : Node_Id; N2 : Node_Id) is
         Comparison : constant Boolean   := Nkind (N2) in N_Op_Compare;
         Typ        : constant Entity_Id :=
           (if Comparison then Compare_Type (N2) else Etype (N2));

      begin
         --  For a comparison (or equality) operator, the Etype is Boolean, so
         --  it is always global. But the type subject to the Has_Private_View
         --  processing is the Compare_Type, so we must specifically check it.

         if Comparison then
            Set_Etype (N, Etype (N2));

            if not Is_Global (Typ) then
               return;
            end if;

            Set_Compare_Type (N, Typ);

         else
            Set_Etype (N, Typ);
         end if;

         --  If the entity of N is not the associated node, this is a
         --  nested generic and it has an associated node as well, whose
         --  type is already the full view (see below). Indicate that the
         --  original node has a private view.

         if Entity (N) /= N2 then
            if Has_Private_View (Entity (N)) then
               Set_Has_Private_View (N);
            end if;

            if Has_Secondary_Private_View (Entity (N)) then
               Set_Has_Secondary_Private_View (N);
            end if;
         end if;

         --  If not a private type, deal with a secondary private view

         if not Is_Private_Type (Typ) then
            if (Is_Access_Type (Typ)
                 and then Is_Private_Type (Designated_Type (Typ)))
              or else (Is_Array_Type (Typ)
                        and then
                       Is_Private_Type (Component_Type_For_Private_View (Typ)))
            then
               Set_Has_Secondary_Private_View (N);
            end if;

         --  If it is a derivation of a private type in a context where no
         --  full view is needed, nothing to do either.

         elsif No (Full_View (Typ)) and then Typ /= Etype (Typ) then
            null;

         --  Otherwise mark the type for flipping and set the full view on N2
         --  when available, which is necessary for Check_Private_View to swap
         --  back the views in case the full declaration of Typ is visible in
         --  the instantiation context. Note that this will be problematic if
         --  N2 is re-analyzed later, e.g. if it's a default value in a call.

         else
            Set_Has_Private_View (N);

            if Present (Full_View (Typ)) then
               if Comparison then
                  Set_Compare_Type (N2, Full_View (Typ));
               else
                  Set_Etype (N2, Full_View (Typ));
               end if;
            end if;
         end if;

         if Is_Floating_Point_Type (Typ)
           and then Has_Dimension_System (Typ)
         then
            Copy_Dimensions (N2, N);
         end if;
      end Set_Global_Type;

   --  Start of processing for Save_Global_References

   begin
      Gen_Scope := Current_Scope;

      --  If the generic unit is a child unit, references to entities in the
      --  parent are treated as local, because they will be resolved anew in
      --  the context of the instance of the parent.

      while Is_Child_Unit (Gen_Scope)
        and then Ekind (Scope (Gen_Scope)) = E_Generic_Package
      loop
         Gen_Scope := Scope (Gen_Scope);
      end loop;

      Save_References (Templ);
   end Save_Global_References;

   ---------------------------------------
   -- Save_Global_References_In_Aspects --
   ---------------------------------------

   procedure Save_Global_References_In_Aspects (N : Node_Id) is
      Asp  : Node_Id;
      Expr : Node_Id;

   begin
      Asp := First (Aspect_Specifications (N));
      while Present (Asp) loop
         Expr := Expression (Asp);

         if Present (Expr) then
            Save_Global_References (Expr);
         end if;

         Next (Asp);
      end loop;
   end Save_Global_References_In_Aspects;

   ------------------------------------------
   -- Set_Copied_Sloc_For_Inherited_Pragma --
   ------------------------------------------

   procedure Set_Copied_Sloc_For_Inherited_Pragma
     (N : Node_Id;
      E : Entity_Id)
   is
   begin
      Create_Instantiation_Source (N, E,
        Inlined_Body     => False,
        Inherited_Pragma => True,
        Factor           => S_Adjustment);
   end Set_Copied_Sloc_For_Inherited_Pragma;

   --------------------------------------
   -- Set_Copied_Sloc_For_Inlined_Body --
   --------------------------------------

   procedure Set_Copied_Sloc_For_Inlined_Body (N : Node_Id; E : Entity_Id) is
   begin
      Create_Instantiation_Source (N, E,
        Inlined_Body     => True,
        Inherited_Pragma => False,
        Factor           => S_Adjustment);
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
      --  ??? More things could be factored out in this routine.
      --  Should probably be done at a later stage.

      Generic_Flags.Append (Inside_A_Generic);
      Inside_A_Generic := True;

      Expander_Mode_Save_And_Set (False);
   end Start_Generic;

   ----------------------
   -- Set_Instance_Env --
   ----------------------

   --  WARNING: This routine manages SPARK regions

   procedure Set_Instance_Env
     (Gen_Unit : Entity_Id;
      Act_Unit : Entity_Id)
   is
      Saved_AE  : constant Boolean         := Assertions_Enabled;
      Saved_CPL : constant Node_Id         := Check_Policy_List;
      Saved_DEC : constant Boolean         := Dynamic_Elaboration_Checks;
      Saved_SM  : constant SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : constant Node_Id         := SPARK_Mode_Pragma;

   begin
      --  Regardless of the current mode, predefined units are analyzed in the
      --  most current Ada mode, and earlier version Ada checks do not apply
      --  to predefined units. Nothing needs to be done for non-internal units.
      --  These are always analyzed in the current mode.

      if In_Internal_Unit (Gen_Unit) then

         --  The following call resets all configuration attributes to default
         --  or the xxx_Config versions of the attributes when the current sem
         --  unit is the main unit. At the same time, internal units must also
         --  inherit certain configuration attributes from their context. It
         --  is unclear what these two sets are.

         Set_Config_Switches (True, Current_Sem_Unit = Main_Unit);

         --  Reinstall relevant configuration attributes of the context

         Assertions_Enabled         := Saved_AE;
         Check_Policy_List          := Saved_CPL;
         Dynamic_Elaboration_Checks := Saved_DEC;

         Install_SPARK_Mode (Saved_SM, Saved_SMP);
      end if;

      Current_Instantiated_Parent :=
        (Gen_Id         => Gen_Unit,
         Act_Id         => Act_Unit,
         Next_In_HTable => Assoc_Null);
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
      --  some other occurrence, in which case there is nothing to switch
      --  besides T itself. Note that a private dependent subtype of a private
      --  type might not have been switched even if the base type has been,
      --  because of the last branch of Check_Private_View (see comment there).

      if not Is_Private_Type (BT) then
         Prepend_Elmt (Full_View (T), Exchanged_Views);
         Exchange_Declarations (T);
         return;
      end if;

      Priv_Elmt := First_Elmt (Private_Dependents (BT));

      if Present (Full_View (BT)) then
         Prepend_Elmt (Full_View (BT), Exchanged_Views);
         Exchange_Declarations (BT);
      end if;

      while Present (Priv_Elmt) loop
         Priv_Sub := Node (Priv_Elmt);

         if Present (Full_View (Priv_Sub)) then
            Prepend_Elmt (Full_View (Priv_Sub), Exchanged_Views);
            Exchange_Declarations (Priv_Sub);
         end if;

         Next_Elmt (Priv_Elmt);
      end loop;
   end Switch_View;

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

   -----------------------------
   -- Valid_Default_Attribute --
   -----------------------------

   procedure Valid_Default_Attribute (Nam : Entity_Id; Def : Node_Id) is
      Attr_Id : constant Attribute_Id :=
                  Get_Attribute_Id (Attribute_Name (Def));
      T       : constant Entity_Id := Entity (Prefix (Def));
      Is_Fun  : constant Boolean := (Ekind (Nam) = E_Function);
      F       : Entity_Id;
      Num_F   : Nat;
      OK      : Boolean;

   begin
      if No (T) or else T = Any_Id then
         return;
      end if;

      Num_F := 0;
      F := First_Formal (Nam);
      while Present (F) loop
         Num_F := Num_F + 1;
         Next_Formal (F);
      end loop;

      case Attr_Id is
         when Attribute_Adjacent
            | Attribute_Ceiling
            | Attribute_Copy_Sign
            | Attribute_Floor
            | Attribute_Fraction
            | Attribute_Machine
            | Attribute_Model
            | Attribute_Remainder
            | Attribute_Rounding
            | Attribute_Unbiased_Rounding
         =>
            OK := Is_Fun
                    and then Num_F = 1
                    and then Is_Floating_Point_Type (T);

         when Attribute_Image
            | Attribute_Pred
            | Attribute_Succ
            | Attribute_Value
            | Attribute_Wide_Image
            | Attribute_Wide_Value
         =>
            OK := Is_Fun and then Num_F = 1 and then Is_Scalar_Type (T);

         when Attribute_Max
            | Attribute_Min
         =>
            OK := Is_Fun and then Num_F = 2 and then Is_Scalar_Type (T);

         when Attribute_Input =>
            OK := (Is_Fun and then Num_F = 1);

         when Attribute_Output
            | Attribute_Put_Image
            | Attribute_Read
            | Attribute_Write
         =>
            OK := not Is_Fun and then Num_F = 2;

         when others =>
            OK := False;
      end case;

      if not OK then
         Error_Msg_N
           ("attribute reference has wrong profile for subprogram", Def);
      end if;
   end Valid_Default_Attribute;

   ----------------------------------
   -- Validate_Formal_Type_Default --
   ----------------------------------

   procedure Validate_Formal_Type_Default (Decl : Node_Id) is
      Default : constant Node_Id :=
        Default_Subtype_Mark (Original_Node (Decl));
      Formal  : constant Entity_Id := Defining_Identifier (Decl);

      Def_Sub  : Entity_Id;     --  Default subtype mark
      Type_Def : Node_Id;

      procedure Check_Discriminated_Formal;
      --  Check that discriminants of default for private or incomplete
      --  type match those of formal type.

      function Reference_Formal (N : Node_Id) return Traverse_Result;
      --  Check whether formal type definition mentions a previous formal
      --  type of the same generic.

      ----------------------
      -- Reference_Formal --
      ----------------------

      function Reference_Formal (N : Node_Id) return Traverse_Result is
      begin
         if Is_Entity_Name (N)
           and then Scope (Entity (N)) = Current_Scope
         then
            return Abandon;
         else
            return OK;
         end if;
      end Reference_Formal;

      function Depends_On_Other_Formals is
           new Traverse_Func (Reference_Formal);

      function Default_Subtype_Matches
        (Gen_T, Def_T : Entity_Id) return Boolean;

      procedure Validate_Array_Type_Default;
      --  Verify that dimension, indices, and component types of default
      --  are compatible with formal array type definition.

      procedure Validate_Derived_Type_Default;
      --  Verify that ancestor and progenitor types match.

      ---------------------------------
      -- Check_Discriminated_Formal --
      ---------------------------------

      procedure Check_Discriminated_Formal is
         Formal_Discr : Entity_Id;
         Actual_Discr : Entity_Id;
         Formal_Subt  : Entity_Id;

      begin
         if Has_Discriminants (Formal) then
            if not Has_Discriminants (Def_Sub) then
               Error_Msg_NE
                 ("default for & must have discriminants", Default, Formal);

            elsif Is_Constrained (Def_Sub) then
               Error_Msg_NE
                 ("default for & must be unconstrained", Default, Formal);

            else
               Formal_Discr := First_Discriminant (Formal);
               Actual_Discr := First_Discriminant (Def_Sub);
               while Formal_Discr /= Empty loop
                  if Actual_Discr = Empty then
                     Error_Msg_N
                       ("discriminants on Formal do not match formal",
                        Default);
                  end if;

                  Formal_Subt := Etype (Formal_Discr);

                  --  Access discriminants match if designated types do

                  if Ekind (Base_Type (Formal_Subt)) = E_Anonymous_Access_Type
                    and then (Ekind (Base_Type (Etype (Actual_Discr)))) =
                                E_Anonymous_Access_Type
                    and then
                        Base_Type
                          (Designated_Type (Base_Type (Formal_Subt))) =
                        Base_Type
                          (Designated_Type (Base_Type (Etype (Actual_Discr))))
                    and then
                        Subtypes_Statically_Match
                         (Designated_Type (Base_Type (Formal_Subt)),
                          Designated_Type (Base_Type (Etype (Actual_Discr))))
                  then
                     null;

                  elsif Base_Type (Formal_Subt) /=
                          Base_Type (Etype (Actual_Discr))
                  then
                     Error_Msg_N
                       ("types of discriminants of default must match formal",
                        Default);

                  elsif not Subtypes_Statically_Match
                              (Formal_Subt, Etype (Actual_Discr))
                    and then Ada_Version >= Ada_95
                  then
                     Error_Msg_N
                       ("subtypes of discriminants of default "
                         & "must match formal",
                        Default);
                  end if;

                  Next_Discriminant (Formal_Discr);
                  Next_Discriminant (Actual_Discr);
               end loop;

               if Actual_Discr /= Empty then
                  Error_Msg_NE
                    ("discriminants on default do not match formal",
                     Default, Formal);
               end if;
            end if;
         end if;
      end Check_Discriminated_Formal;

      ---------------------------
      -- Default_Subtype_Matches --
      ---------------------------

      function Default_Subtype_Matches
        (Gen_T, Def_T : Entity_Id) return Boolean
      is
      begin
         --  Check that the base types, root types (when dealing with class
         --  wide types), or designated types (when dealing with anonymous
         --  access types) of Gen_T and Def_T are statically matching subtypes.

         return (Base_Type (Gen_T) = Base_Type (Def_T)
                  and then Subtypes_Statically_Match (Gen_T, Def_T))

           or else (Is_Class_Wide_Type (Gen_T)
                     and then Is_Class_Wide_Type (Def_T)
                     and then Default_Subtype_Matches
                                (Root_Type (Gen_T), Root_Type (Def_T)))

           or else (Is_Anonymous_Access_Type (Gen_T)
               and then Ekind (Def_T) = Ekind (Gen_T)
               and then Subtypes_Statically_Match
                          (Designated_Type (Gen_T), Designated_Type (Def_T)));

      end Default_Subtype_Matches;

      ----------------------------------
      --  Validate_Array_Type_Default --
      ----------------------------------

      procedure Validate_Array_Type_Default is
         I1, I2 : Node_Id;
         T2     : Entity_Id;
      begin
         if not Is_Array_Type (Def_Sub) then
            Error_Msg_NE ("default for& must be an array type ",
              Default, Formal);
            return;

         elsif Number_Dimensions (Def_Sub) /= Number_Dimensions (Formal)
           or else Is_Constrained (Def_Sub) /=
                   Is_Constrained (Formal)
         then
            Error_Msg_NE ("default array type does not match&",
              Default, Formal);
            return;
         end if;

         I1 := First_Index (Formal);
         I2 := First_Index (Def_Sub);
         for J in 1 .. Number_Dimensions (Formal) loop

            --  If the indexes of the actual were given by a subtype_mark,
            --  the index was transformed into a range attribute. Retrieve
            --  the original type mark for checking.

            if Is_Entity_Name (Original_Node (I2)) then
               T2 := Entity (Original_Node (I2));
            else
               T2 := Etype (I2);
            end if;

            if not Subtypes_Statically_Match (Etype (I1), T2) then
               Error_Msg_NE
                 ("index types of default do not match those of formal &",
                  Default, Formal);
            end if;

            Next_Index (I1);
            Next_Index (I2);
         end loop;

         if not Default_Subtype_Matches
           (Component_Type (Formal), Component_Type (Def_Sub))
         then
            Error_Msg_NE
              ("component subtype of default does not match that of formal &",
               Default, Formal);
         end if;

         if Has_Aliased_Components (Formal)
           and then not Has_Aliased_Components (Default)
         then
            Error_Msg_NE
              ("default must have aliased components to match formal type &",
               Default, Formal);
         end if;
      end Validate_Array_Type_Default;

      -----------------------------------
      -- Validate_Derived_Type_Default --
      -----------------------------------

      procedure Validate_Derived_Type_Default is
      begin
         if not Is_Ancestor (Etype (Formal), Def_Sub) then
            Error_Msg_NE ("default must be a descendent of&",
              Default, Etype (Formal));
         end if;

         if Has_Interfaces (Formal) then
            if not Has_Interfaces (Def_Sub) then
               Error_Msg_NE
                 ("default must implement all interfaces of formal&",
                   Default, Formal);

            else
               declare
                  Iface     : Node_Id;
                  Iface_Ent : Entity_Id;

               begin
                  Iface := First (Abstract_Interface_List (Formal));

                  while Present (Iface) loop
                     Iface_Ent := Entity (Iface);

                     if Is_Ancestor (Iface_Ent, Def_Sub)
                      or else Is_Progenitor (Iface_Ent, Def_Sub)
                     then
                        null;

                     else
                        Error_Msg_NE
                          ("Default must implement interface&",
                           Default, Etype (Iface));
                     end if;

                     Next (Iface);
                  end loop;
               end;
            end if;
         end if;
      end Validate_Derived_Type_Default;

   --  Start of processing for Validate_Formal_Type_Default

   begin
      Analyze (Default);
      if not Is_Entity_Name (Default)
        or else not Is_Type (Entity (Default))
      then
         Error_Msg_N
           ("Expect type name for default of formal type", Default);
         return;
      else
         Def_Sub := Entity (Default);
      end if;

      --  Formal derived_type declarations are transformed into full
      --  type declarations or Private_Type_Extensions for ease of processing.

      if Nkind (Decl) = N_Full_Type_Declaration then
         Type_Def := Type_Definition (Decl);

      elsif Nkind (Decl) = N_Private_Extension_Declaration then
         Type_Def := Subtype_Indication (Decl);

      else
         Type_Def := Formal_Type_Definition (Decl);
      end if;

      if Depends_On_Other_Formals (Type_Def) = Abandon
           and then Scope (Def_Sub) /= Current_Scope
      then
         Error_Msg_N ("default of formal type that depends on "
           & "other formals must be a previous formal type", Default);
         return;

      elsif Def_Sub = Formal then
         Error_Msg_N
           ("default for formal type cannot be formal itsef", Default);
         return;
      end if;

      case Nkind (Type_Def) is

         when N_Formal_Private_Type_Definition =>
            if (Is_Abstract_Type (Formal)
              and then not Is_Abstract_Type (Def_Sub))
            or else (Is_Limited_Type (Formal)
              and then not Is_Limited_Type (Def_Sub))
            then
               Error_Msg_NE
                 ("default for private type$ does not match",
                     Default, Formal);
            end if;

            Check_Discriminated_Formal;

         when N_Formal_Derived_Type_Definition =>
            Check_Discriminated_Formal;
            Validate_Derived_Type_Default;

         when N_Formal_Incomplete_Type_Definition =>
            if Is_Tagged_Type (Formal)
              and then not Is_Tagged_Type (Def_Sub)
            then
               Error_Msg_NE
                 ("default for & must be a tagged type", Default, Formal);
            end if;

            Check_Discriminated_Formal;

         when N_Formal_Discrete_Type_Definition =>
            if not Is_Discrete_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be a discrete type",
                 Default, Formal);
            end if;

         when N_Formal_Signed_Integer_Type_Definition =>
            if not Is_Integer_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be a discrete type",
                 Default, Formal);
            end if;

         when N_Formal_Modular_Type_Definition =>
            if not Is_Modular_Integer_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be a modular_integer Type",
                 Default, Formal);
            end if;

         when N_Formal_Floating_Point_Definition =>
            if not Is_Floating_Point_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be a floating_point type",
                 Default, Formal);
            end if;

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            if not Is_Ordinary_Fixed_Point_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be "
                 & "an ordinary_fixed_point type ",
                 Default, Formal);
            end if;

         when N_Formal_Decimal_Fixed_Point_Definition =>
            if not Is_Decimal_Fixed_Point_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be "
                 & "an Decimal_fixed_point type ",
                 Default, Formal);
            end if;

         when N_Array_Type_Definition =>
            Validate_Array_Type_Default;

         when N_Access_Function_Definition |
           N_Access_Procedure_Definition =>
            if Ekind (Def_Sub) /= E_Access_Subprogram_Type then
               Error_Msg_NE ("default for& must be an Access_To_Subprogram",
                 Default, Formal);
            end if;
            Check_Subtype_Conformant
              (Designated_Type (Formal), Designated_Type (Def_Sub));

         when N_Access_To_Object_Definition =>
            if not Is_Access_Object_Type (Def_Sub) then
               Error_Msg_NE ("default for& must be an Access_To_Object",
                Default, Formal);

            elsif not Default_Subtype_Matches
              (Designated_Type (Formal), Designated_Type (Def_Sub))
            then
               Error_Msg_NE ("designated type of defaul does not match "
                 & "designated type of formal type",
                 Default, Formal);
            end if;

         when N_Record_Definition =>   -- Formal interface type
            if not Is_Interface (Def_Sub) then
               Error_Msg_NE
                 ("default for formal interface type must be an interface",
                  Default, Formal);

            elsif Is_Limited_Type (Def_Sub) /= Is_Limited_Type (Formal)
              or else Is_Task_Interface (Formal) /= Is_Task_Interface (Def_Sub)
              or else Is_Protected_Interface (Formal) /=
                      Is_Protected_Interface (Def_Sub)
              or else Is_Synchronized_Interface (Formal) /=
                      Is_Synchronized_Interface (Def_Sub)
            then
               Error_Msg_NE
                 ("default for interface& does not match", Def_Sub, Formal);
            end if;

         when N_Derived_Type_Definition =>
            Validate_Derived_Type_Default;

         when N_Identifier =>   --  case of a private extension
            Validate_Derived_Type_Default;

         when N_Error =>
            null;

         when others =>
            raise Program_Error;
      end case;
   end Validate_Formal_Type_Default;

   package body Instance_Context is

      --------------------
      -- Save_And_Reset --
      --------------------

      function Save_And_Reset return Context is
      begin
         return Result : Context (0 .. Integer (Generic_Renamings.Last)) do
            for Index in Result'Range loop
               declare
                  Indexed_Assoc : Assoc renames Generic_Renamings.Table
                                                  (Assoc_Ptr (Index));
                  Result_Pair : Binding_Pair renames Result (Index);
               begin
                  --  If we have called Increment_Last but have not yet
                  --  initialized the new last element of the table, then
                  --  that last element might be invalid. Saving and
                  --  restoring (especially restoring, it turns out) invalid
                  --  values can result in exceptions if predicate checking
                  --  is enabled, so replace invalid values with Empty.

                  if Indexed_Assoc.Gen_Id'Valid then
                     Result_Pair.Formal_Id := Indexed_Assoc.Gen_Id;
                  else
                     pragma Assert (Index = Result'Last);
                     Result_Pair.Formal_Id := Empty;
                  end if;

                  if Indexed_Assoc.Act_Id'Valid then
                     Result_Pair.Actual_Id := Indexed_Assoc.Act_Id;
                  else
                     pragma Assert (Index = Result'Last);
                     Result_Pair.Actual_Id := Empty;
                  end if;
               end;
            end loop;

            Generic_Renamings.Init;
            Generic_Renamings.Set_Last (-1);
            Generic_Renamings_HTable.Reset;
         end return;
      end Save_And_Reset;

      -------------
      -- Restore --
      -------------

      procedure Restore (Saved : Context) is
      begin
         Generic_Renamings.Init;
         Generic_Renamings.Set_Last (0);
         Generic_Renamings_HTable.Reset;
         Generic_Renamings.Increment_Last;
         for Pair of Saved loop
            Set_Instance_Of (Pair.Formal_Id, Pair.Actual_Id);
         end loop;
         Generic_Renamings.Decrement_Last;
      end Restore;

   end Instance_Context;
end Sem_Ch12;
