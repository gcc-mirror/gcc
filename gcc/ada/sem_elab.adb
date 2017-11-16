------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E L A B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2017, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch11; use Exp_Ch11;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Uname;    use Uname;

with GNAT.HTable; use GNAT.HTable;

package body Sem_Elab is

   -----------------------------------------
   -- Access-before-elaboration mechanism --
   -----------------------------------------

   --  The access-before-elaboration (ABE) mechanism implemented in this unit
   --  has the following objectives:
   --
   --    * Diagnose at compile-time or install run-time checks to prevent ABE
   --      access to data and behaviour.
   --
   --      The high-level idea is to accurately diagnose ABE issues within a
   --      single unit because the ABE mechanism can inspect the whole unit.
   --      As soon as the elaboration graph extends to an external unit, the
   --      diagnostics stop because the body of the unit may not be available.
   --      Due to control and data flow, the ABE mechanism cannot accurately
   --      determine whether a particular scenario will be elaborated or not.
   --      Conditional ABE checks are therefore used to verify the elaboration
   --      status of a local and external target at run time.
   --
   --    * Supply elaboration dependencies for a unit to binde
   --
   --      The ABE mechanism registers each outgoing elaboration edge for the
   --      main unit in its ALI file. GNATbind and binde can then reconstruct
   --      the full elaboration graph and determine the proper elaboration
   --      order for all units in the compilation.
   --
   --  The ABE mechanism supports three models of elaboration:
   --
   --    * Dynamic model - This is the most permissive of the three models.
   --      When the dynamic model is in effect, the mechanism performs very
   --      little diagnostics and generates run-time checks to detect ABE
   --      issues. The behaviour of this model is identical to that specified
   --      by the Ada RM. This model is enabled with switch -gnatE.
   --
   --    * Static model - This is the middle ground of the three models. When
   --      the static model is in effect, the mechanism diagnoses and installs
   --      run-time checks to detect ABE issues in the main unit. In addition,
   --      the mechanism generates implicit Elaborate or Elaborate_All pragmas
   --      to ensure the prior elaboration of withed units. The model employs
   --      textual order, with clause context, and elaboration-related source
   --      pragmas. This is the default model.
   --
   --    * SPARK model - This is the most conservative of the three models and
   --      impelements the semantics defined in SPARK RM 7.7. The SPARK model
   --      is in effect only when a context resides in a SPARK_Mode On region,
   --      otherwise the mechanism falls back to one of the previous models.
   --
   --  The ABE mechanism consists of a "recording" phase and a "processing"
   --  phase.

   -----------------
   -- Terminology --
   -----------------

   --  * Bridge target - A type of target. A bridge target is a link between
   --    scenarios. It is usually a byproduct of expansion and does not have
   --    any direct ABE ramifications.
   --
   --  * Call marker - A special node used to indicate the presence of a call
   --    in the tree in case expansion transforms or eliminates the original
   --    call. N_Call_Marker nodes do not have static and run-time semantics.
   --
   --  * Conditional ABE - A type of ABE. A conditional ABE occurs when the
   --    elaboration or invocation of a target by a scenario within the main
   --    unit causes an ABE, but does not cause an ABE for another scenarios
   --    within the main unit.
   --
   --  * Declaration level - A type of enclosing level. A scenario or target is
   --    at the declaration level when it appears within the declarations of a
   --    block statement, entry body, subprogram body, or task body, ignoring
   --    enclosing packages.
   --
   --  * Early call region - A section of code which ends at a subprogram body
   --    and starts from the nearest non-preelaborable construct which precedes
   --    the subprogram body. The early call region extends from a package body
   --    to a package spec when the spec carries pragma Elaborate_Body.
   --
   --  * Generic library level - A type of enclosing level. A scenario or
   --    target is at the generic library level if it appears in a generic
   --    package library unit, ignoring enclosing packages.
   --
   --  * Guaranteed ABE - A type of ABE. A guaranteed ABE occurs when the
   --    elaboration or invocation of a target by all scenarios within the
   --    main unit causes an ABE.
   --
   --  * Instantiation library level - A type of enclosing level. A scenario
   --    or target is at the instantiation library level if it appears in an
   --    instantiation library unit, ignoring enclosing packages.
   --
   --  * Library level - A type of enclosing level. A scenario or target is at
   --    the library level if it appears in a package library unit, ignoring
   --    enclosng packages.
   --
   --  * Non-library-level encapsulator - A construct that cannot be elaborated
   --    on its own and requires elaboration by a top-level scenario.
   --
   --  * Scenario - A construct or context which may be elaborated or executed
   --    by elaboration code. The scenarios recognized by the ABE mechanism are
   --    as follows:
   --
   --      - '[Unrestricted_]Access of entries, operators, and subprograms
   --
   --      - Assignments to variables
   --
   --      - Calls to entries, operators, and subprograms
   --
   --      - Derived type declarations
   --
   --      - Instantiations
   --
   --      - Pragma Refined_State
   --
   --      - Reads of variables
   --
   --      - Task activation
   --
   --  * Target - A construct referenced by a scenario. The targets recognized
   --    by the ABE mechanism are as follows:
   --
   --      - For '[Unrestricted_]Access of entries, operators, and subprograms,
   --        the target is the entry, operator, or subprogram.
   --
   --      - For assignments to variables, the target is the variable
   --
   --      - For calls, the target is the entry, operator, or subprogram
   --
   --      - For derived type declarations, the target is the derived type
   --
   --      - For instantiations, the target is the generic template
   --
   --      - For pragma Refined_State, the targets are the constituents
   --
   --      - For reads of variables, the target is the variable
   --
   --      - For task activation, the target is the task body
   --
   --  * Top-level scenario - A scenario which appears in a non-generic main
   --    unit. Depending on the elaboration model is in effect, the following
   --    addotional restrictions apply:
   --
   --      - Dynamic model - No restrictions
   --
   --      - SPARK model - Falls back to either the dynamic or static model
   --
   --      - Static model - The scenario must be at the library level

   ---------------------
   -- Recording phase --
   ---------------------

   --  The Recording phase coincides with the analysis/resolution phase of the
   --  compiler. It has the following objectives:
   --
   --    * Record all top-level scenarios for examination by the Processing
   --      phase.
   --
   --      Saving only a certain number of nodes improves the performance of
   --      the ABE mechanism. This eliminates the need to examine the whole
   --      tree in a separate pass.
   --
   --    * Record certain SPARK scenarios which are not necessarily executable
   --      during elaboration, but still require elaboration-related checks.
   --
   --      Saving only a certain number of nodes improves the performance of
   --      the ABE mechanism. This eliminates the need to examine the whole
   --      tree in a separate pass.
   --
   --    * Detect and diagnose calls in preelaborable or pure units, including
   --      generic bodies.
   --
   --      This diagnostic is carried out during the Recording phase because it
   --      does not need the heavy recursive traversal done by the Processing
   --      phase.
   --
   --    * Detect and diagnose guaranteed ABEs caused by instantiations,
   --      calls, and task activation.
   --
   --      The issues detected by the ABE mechanism are reported as warnings
   --      because they do not violate Ada semantics. Forward instantiations
   --      may thus reach gigi, however gigi cannot handle certain kinds of
   --      premature instantiations and may crash. To avoid this limitation,
   --      the ABE mechanism must identify forward instantiations as early as
   --      possible and suppress their bodies. Calls and task activations are
   --      included in this category for completeness.

   ----------------------
   -- Processing phase --
   ----------------------

   --  The Processing phase is a separate pass which starts after instantiating
   --  and/or inlining of bodies, but before the removal of Ghost code. It has
   --  the following objectives:
   --
   --    * Examine all top-level scenarios saved during the Recording phase
   --
   --      The top-level scenarios act as roots for depth-first traversal of
   --      the call/instantiation/task activation graph. The traversal stops
   --      when an outgoing edge leaves the main unit.
   --
   --    * Examine all SPARK scenarios saved during the Recording phase
   --
   --    * Depending on the elaboration model in effect, perform the following
   --      actions:
   --
   --        - Dynamic model - Diagnose guaranteed ABEs and install run-time
   --          conditional ABE checks.
   --
   --        - SPARK model - Enforce the SPARK elaboration rules
   --
   --        - Static model - Diagnose conditional/guaranteed ABEs, install
   --          run-time conditional ABE checks, and guarantee the elaboration
   --          of external units.
   --
   --    * Examine nested scenarios
   --
   --      Nested scenarios discovered during the depth-first traversal are
   --      in turn subjected to the same actions outlined above and examined
   --      for the next level of nested scenarios.

   ------------------
   -- Architecture --
   ------------------

   --     Analysis/Resolution
   --     |
   --     +- Build_Call_Marker
   --     |
   --     +- Build_Variable_Reference_Marker
   --     |
   --  +- | -------------------- Recording phase ---------------------------+
   --  |  v                                                                 |
   --  |  Record_Elaboration_Scenario                                       |
   --  |  |                                                                 |
   --  |  +--> Check_Preelaborated_Call                                     |
   --  |  |                                                                 |
   --  |  +--> Process_Guaranteed_ABE                                       |
   --  |  |    |                                                            |
   --  |  |    +--> Process_Guaranteed_ABE_Activation                       |
   --  |  |    |                                                            |
   --  |  |    +--> Process_Guaranteed_ABE_Call                             |
   --  |  |    |                                                            |
   --  |  |    +--> Process_Guaranteed_ABE_Instantiation                    |
   --  |  |                                                                 |
   --  +- | ----------------------------------------------------------------+
   --     |
   --     |
   --     +--> SPARK_Scenarios
   --     |    +-----------+-----------+ .. +-----------+
   --     |    | Scenario1 | Scenario2 | .. | ScenarioN |
   --     |    +-----------+-----------+ .. +-----------+
   --     |
   --     +--> Top_Level_Scenarios
   --     |    +-----------+-----------+ .. +-----------+
   --     |    | Scenario1 | Scenario2 | .. | ScenarioN |
   --     |    +-----------+-----------+ .. +-----------+
   --     |
   --     End of Compilation
   --     |
   --  +- | --------------------- Processing phase -------------------------+
   --  |  v                                                                 |
   --  |  Check_Elaboration_Scenarios                                       |
   --  |  |                                                                 |
   --  |  +--> Check_SPARK_Scenario                                         |
   --  |  |    |                                                            |
   --  |  |    +--> Check_SPARK_Derived_Type                                |
   --  |  |    |                                                            |
   --  |  |    +--> Check_SPARK_Instantiation                               |
   --  |  |    |                                                            |
   --  |  |    +--> Check_SPARK_Refined_State_Pragma                        |
   --  |  |                                                                 |
   --  |  +--> Process_Conditional_ABE <---------------------------+        |
   --  |       |                                                   |        |
   --  |       +--> Process_Conditional_ABE_Access    Is_Suitable_Scenario  |
   --  |       |                                                   ^        |
   --  |       +--> Process_Conditional_ABE_Activation             |        |
   --  |       |    |                                              |        |
   --  |       |    +-----------------------------+                |        |
   --  |       |                                  |                |        |
   --  |       +--> Process_Conditional_ABE_Call  +--------> Traverse_Body  |
   --  |       |    |                             |                         |
   --  |       |    +-----------------------------+                         |
   --  |       |                                                            |
   --  |       +--> Process_Conditional_ABE_Instantiation                   |
   --  |       |                                                            |
   --  |       +--> Process_Conditional_ABE_Variable_Assignment             |
   --  |       |                                                            |
   --  |       +--> Process_Conditional_ABE_Variable_Reference              |
   --  |                                                                    |
   --  +--------------------------------------------------------------------+

   ----------------------
   -- Important points --
   ----------------------

   --  The Processing phase starts after the analysis, resolution, expansion
   --  phase has completed. As a result, no current semantic information is
   --  available. The scope stack is empty, global flags such as In_Instance
   --  or Inside_A_Generic become useless. To remedy this, the ABE mechanism
   --  must either save or recompute semantic information.

   --  Expansion heavily transforms calls and to some extent instantiations. To
   --  remedy this, the ABE mechanism generates N_Call_Marker nodes in order to
   --  capture the target and relevant attributes of the original call.

   --  The diagnostics of the ABE mechanism depend on accurate source locations
   --  to determine the spacial relation of nodes.

   --------------
   -- Switches --
   --------------

   --  The following switches may be used to control the behavior of the ABE
   --  mechanism.
   --
   --  -gnatdE  elaboration checks on predefined units
   --
   --           The ABE mechanism considers scenarios which appear in internal
   --           units (Ada, GNAT, Interfaces, System).
   --
   --  -gnatd.G ignore calls through generic formal parameters for elaboration
   --
   --           The ABE mechanism does not generate N_Call_Marker nodes for
   --           calls which occur in expanded instances, and invoke generic
   --           actual subprograms through generic formal subprograms. As a
   --           result, the calls are not recorded or processed.
   --
   --           If switches -gnatd.G and -gnatdL are used together, then the
   --           ABE mechanism effectively ignores all calls which cause the
   --           elaboration flow to "leave" the instance.
   --
   --  -gnatdL  ignore external calls from instances for elaboration
   --
   --           The ABE mechanism does not generate N_Call_Marker nodes for
   --           calls which occur in expanded instances, do not invoke generic
   --           actual subprograms through formal subprograms, and the target
   --           is external to the instance. As a result, the calls are not
   --           recorded or processed.
   --
   --           If switches -gnatd.G and -gnatdL are used together, then the
   --           ABE mechanism effectively ignores all calls which cause the
   --           elaboration flow to "leave" the instance.
   --
   --  -gnatd.o conservative elaboration order for indirect calls
   --
   --           The ABE mechanism treats '[Unrestricted_]Access of an entry,
   --           operator, or subprogram as an immediate invocation of the
   --           target. As a result, it performs ABE checks and diagnostics on
   --           the immediate call.
   --
   --  -gnatd.U ignore indirect calls for static elaboration
   --
   --           The ABE mechanism does not consider '[Unrestricted_]Access of
   --           entries, operators, and subprograms. As a result, the scenarios
   --           are not recorder or processed.
   --
   --  -gnatd.v enforce SPARK elaboration rules in SPARK code
   --
   --           The ABE mechanism applies some of the SPARK elaboration rules
   --           defined in the SPARK reference manual, chapter 7.7. Note that
   --           certain rules are always enforced, regardless of whether the
   --           switch is active.
   --
   --  -gnatd.y disable implicit pragma Elaborate_All on task bodies
   --
   --           The ABE mechanism does not generate implicit Elaborate_All when
   --           the need for the pragma came from a task body.
   --
   --  -gnatE   dynamic elaboration checking mode enabled
   --
   --           The ABE mechanism assumes that any scenario is elaborated or
   --           invoked by elaboration code. The ABE mechanism performs very
   --           little diagnostics and generates condintional ABE checks to
   --           detect ABE issues at run-time.
   --
   --  -gnatel  turn on info messages on generated Elaborate[_All] pragmas
   --
   --           The ABE mechanism produces information messages on generated
   --           implicit Elabote[_All] pragmas along with traceback showing
   --           why the pragma was generated. In addition, the ABE mechanism
   --           produces information messages for each scenario elaborated or
   --           invoked by elaboration code.
   --
   --  -gnateL  turn off info messages on generated Elaborate[_All] pragmas
   --
   --           The complimentary switch for -gnatel.
   --
   --  -gnatw.f turn on warnings for suspicious Subp'Access
   --
   --           The ABE mechanism treats '[Unrestricted_]Access of an entry,
   --           operator, or subprogram as a pseudo invocation of the target.
   --           As a result, it performs ABE diagnostics on the pseudo call.
   --
   --  -gnatw.F turn off warnings for suspicious Subp'Access
   --
   --           The complimentary switch for -gnatw.f.
   --
   --  -gnatwl  turn on warnings for elaboration problems
   --
   --           The ABE mechanism produces warnings on detected ABEs along with
   --           traceback showing the graph of the ABE.
   --
   --  -gnatwL  turn off warnings for elaboration problems
   --
   --           The complimentary switch for -gnatwl.

   ---------------------------
   -- Adding a new scenario --
   ---------------------------

   --  The following steps describe how to add a new elaboration scenario and
   --  preserve the existing architecture. Note that not all of the steps may
   --  need to be carried out.
   --
   --    1) Update predicate Is_Scenario
   --
   --    2) Add predicate Is_Suitable_xxx. Include a call to it in predicate
   --       Is_Suitable_Scenario.
   --
   --    3) Update routine Record_Elaboration_Scenario
   --
   --    4) Add routine Process_Conditional_ABE_xxx. Include a call to it in
   --       routine Process_Conditional_ABE.
   --
   --    5) Add routine Process_Guaranteed_ABE_xxx. Include a call to it in
   --       routine Process_Guaranteed_ABE.
   --
   --    6) Add routine Check_SPARK_xxx. Include a call to it in routine
   --       Check_SPARK_Scenario.
   --
   --    7) Add routine Info_xxx. Include a call to it in routine
   --       Process_Conditional_ABE_xxx.
   --
   --    8) Add routine Output_xxx. Include a call to it in routine
   --       Output_Active_Scenarios.
   --
   --    9) Add routine Extract_xxx_Attributes
   --
   --   10) Update routine Is_Potential_Scenario

   -------------------------
   -- Adding a new target --
   -------------------------

   --  The following steps describe how to add a new elaboration target and
   --  preserve the existing architecture. Note that not all of the steps may
   --  need to be carried out.
   --
   --    1) Add predicate Is_xxx.
   --
   --    2) Update predicates Is_Ada_Semantic_Target, Is_Bridge_Target, or
   --       Is_SPARK_Semantic_Target. If necessary, create a new category.
   --
   --    3) Update the appropriate Info_xxx routine.
   --
   --    4) Update the appropriate Output_xxx routine.
   --
   --    5) Update routine Extract_Target_Attributes. If necessary, create a
   --       new Extract_xxx routine.

   --------------------------
   -- Debugging ABE issues --
   --------------------------

   --  * If the issue involves a call, ensure that the call is eligible for ABE
   --    processing and receives a corresponding call marker. The routines of
   --    interest are
   --
   --      Build_Call_Marker
   --      Record_Elaboration_Scenario

   --  * If the issue involves an arbitrary scenario, ensure that the scenario
   --    is either recorded, or is successfully recognized while traversing a
   --    body. The routines of interest are
   --
   --      Record_Elaboration_Scenario
   --      Process_Conditional_ABE
   --      Process_Guaranteed_ABE
   --      Traverse_Body

   --  * If the issue involves a circularity in the elaboration order, examine
   --    the ALI files and look for the following encodings next to units:
   --
   --       E indicates a source Elaborate
   --
   --      EA indicates a source Elaborate_All
   --
   --      AD indicates an implicit Elaborate_All
   --
   --      ED indicates an implicit Elaborate
   --
   --    If possible, compare these encodings with those generated by the old
   --    ABE mechanism. The routines of interest are
   --
   --      Ensure_Prior_Elaboration

   ----------------
   -- Attributes --
   ----------------

   --  To minimize the amount of code within routines, the ABE mechanism relies
   --  on "attribute" records to capture relevant information for a scenario or
   --  a target.

   --  The following type captures relevant attributes which pertain to a call

   type Call_Attributes is record
      Elab_Checks_OK : Boolean;
      --  This flag is set when the call has elaboration checks enabled

      Elab_Warnings_OK : Boolean;
      --  This flag is set when the call has elaboration warnings elabled

      From_Source : Boolean;
      --  This flag is set when the call comes from source

      Ghost_Mode_Ignore : Boolean;
      --  This flag is set when the call appears in a region subject to pragma
      --  Ghost with policy Ignore.

      In_Declarations : Boolean;
      --  This flag is set when the call appears at the declaration level

      Is_Dispatching : Boolean;
      --  This flag is set when the call is dispatching

      SPARK_Mode_On : Boolean;
      --  This flag is set when the call appears in a region subject to pragma
      --  SPARK_Mode with value On.
   end record;

   --  The following type captures relevant attributes which pertain to the
   --  prior elaboration of a unit. This type is coupled together with a unit
   --  to form a key -> value relationship.

   type Elaboration_Attributes is record
      Source_Pragma : Node_Id;
      --  This attribute denotes a source Elaborate or Elaborate_All pragma
      --  which guarantees the prior elaboration of some unit with respect
      --  to the main unit. The pragma may come from the following contexts:

      --    * The main unit
      --    * The spec of the main unit (if applicable)
      --    * Any parent spec of the main unit (if applicable)
      --    * Any parent subunit of the main unit (if applicable)

      --  The attribute remains Empty if no such pragma is available. Source
      --  pragmas play a role in satisfying SPARK elaboration requirements.

      With_Clause : Node_Id;
      --  This attribute denotes an internally generated or source with clause
      --  for some unit withed by the main unit. With clauses carry flags which
      --  represent implicit Elaborate or Elaborate_All pragmas. These clauses
      --  play a role in supplying the elaboration dependencies to binde.
   end record;

   No_Elaboration_Attributes : constant Elaboration_Attributes :=
     (Source_Pragma => Empty,
      With_Clause   => Empty);

   --  The following type captures relevant attributes which pertain to an
   --  instantiation.

   type Instantiation_Attributes is record
      Elab_Checks_OK : Boolean;
      --  This flag is set when the instantiation has elaboration checks
      --  enabled.

      Elab_Warnings_OK : Boolean;
      --  This flag is set when the instantiation has elaboration warnings
      --  enabled.

      Ghost_Mode_Ignore : Boolean;
      --  This flag is set when the instantiation appears in a region subject
      --  to pragma Ghost with policy ignore, or starts one such region.

      In_Declarations : Boolean;
      --  This flag is set when the instantiation appears at the declaration
      --  level.

      SPARK_Mode_On : Boolean;
      --  This flag is set when the instantiation appears in a region subject
      --  to pragma SPARK_Mode with value On, or starts one such region.
   end record;

   --  The following type captures relevant attributes which pertain to a
   --  target.

   type Target_Attributes is record
      Elab_Checks_OK : Boolean;
      --  This flag is set when the target has elaboration checks enabled

      From_Source : Boolean;
      --  This flag is set when the target comes from source

      Ghost_Mode_Ignore : Boolean;
      --  This flag is set when the target appears in a region subject to
      --  pragma Ghost with policy ignore, or starts one such region.

      SPARK_Mode_On : Boolean;
      --  This flag is set when the target appears in a region subject to
      --  pragma SPARK_Mode with value On, or starts one such region.

      Spec_Decl : Node_Id;
      --  This attribute denotes the declaration of Spec_Id

      Unit_Id : Entity_Id;
      --  This attribute denotes the top unit where Spec_Id resides

      --  The semantics of the following attributes depend on the target

      Body_Barf : Node_Id;
      Body_Decl : Node_Id;
      Spec_Id   : Entity_Id;

      --  The target is a generic package or a subprogram
      --
      --    * Body_Barf - Empty
      --
      --    * Body_Decl - This attribute denotes the generic or subprogram
      --      body.
      --
      --    * Spec_Id - This attribute denotes the entity of the generic
      --      package or subprogram.

      --  The target is a protected entry
      --
      --    * Body_Barf - This attribute denotes the body of the barrier
      --      function if expansion took place, otherwise it is Empty.
      --
      --    * Body_Decl - This attribute denotes the body of the procedure
      --      which emulates the entry if expansion took place, otherwise it
      --      denotes the body of the protected entry.
      --
      --    * Spec_Id - This attribute denotes the entity of the procedure
      --      which emulates the entry if expansion took place, otherwise it
      --      denotes the protected entry.

      --  The target is a protected subprogram
      --
      --    * Body_Barf - Empty
      --
      --    * Body_Decl - This attribute denotes the body of the protected or
      --      unprotected version of the protected subprogram if expansion took
      --      place, otherwise it denotes the body of the protected subprogram.
      --
      --    * Spec_Id - This attribute denotes the entity of the protected or
      --      unprotected version of the protected subprogram if expansion took
      --      place, otherwise it is the entity of the protected subprogram.

      --  The target is a task entry
      --
      --    * Body_Barf - Empty
      --
      --    * Body_Decl - This attribute denotes the body of the procedure
      --      which emulates the task body if expansion took place, otherwise
      --      it denotes the body of the task type.
      --
      --    * Spec_Id - This attribute denotes the entity of the procedure
      --      which emulates the task body if expansion took place, otherwise
      --      it denotes the entity of the task type.
   end record;

   --  The following type captures relevant attributes which pertain to a task
   --  type.

   type Task_Attributes is record
      Body_Decl : Node_Id;
      --  This attribute denotes the declaration of the procedure body which
      --  emulates the behaviour of the task body.

      Elab_Checks_OK : Boolean;
      --  This flag is set when the task type has elaboration checks enabled

      Ghost_Mode_Ignore : Boolean;
      --  This flag is set when the task type appears in a region subject to
      --  pragma Ghost with policy ignore, or starts one such region.

      SPARK_Mode_On : Boolean;
      --  This flag is set when the task type appears in a region subject to
      --  pragma SPARK_Mode with value On, or starts one such region.

      Spec_Id : Entity_Id;
      --  This attribute denotes the entity of the initial declaration of the
      --  procedure body which emulates the behaviour of the task body.

      Task_Decl : Node_Id;
      --  This attribute denotes the declaration of the task type

      Unit_Id : Entity_Id;
      --  This attribute denotes the entity of the compilation unit where the
      --  task type resides.
   end record;

   --  The following type captures relevant attributes which pertain to a
   --  variable.

   type Variable_Attributes is record
      Unit_Id : Entity_Id;
      --  This attribute denotes the entity of the compilation unit where the
      --  variable resides.
   end record;

   ---------------------
   -- Data structures --
   ---------------------

   --  The ABE mechanism employs lists and hash tables to store information
   --  pertaining to scenarios and targets, as well as the Processing phase.
   --  The need for data structures comes partly from the size limitation of
   --  nodes. Note that the use of hash tables is conservative and operations
   --  are carried out only when a particular hash table has at least one key
   --  value pair (see xxx_In_Use flags).

   --  The following table stores the early call regions of subprogram bodies

   Early_Call_Regions_Max : constant := 101;

   type Early_Call_Regions_Index is range 0 .. Early_Call_Regions_Max - 1;

   function Early_Call_Regions_Hash
     (Key : Entity_Id) return Early_Call_Regions_Index;
   --  Obtain the hash value of entity Key

   Early_Call_Regions_In_Use : Boolean := False;
   --  This flag determines whether table Early_Call_Regions contains at least
   --  least one key/value pair.

   Early_Call_Regions_No_Element : constant Node_Id := Empty;

   package Early_Call_Regions is new Simple_HTable
     (Header_Num => Early_Call_Regions_Index,
      Element    => Node_Id,
      No_Element => Early_Call_Regions_No_Element,
      Key        => Entity_Id,
      Hash       => Early_Call_Regions_Hash,
      Equal      => "=");

   --  The following table stores the elaboration status of all units withed by
   --  the main unit.

   Elaboration_Statuses_Max : constant := 1009;

   type Elaboration_Statuses_Index is range 0 .. Elaboration_Statuses_Max - 1;

   function Elaboration_Statuses_Hash
     (Key : Entity_Id) return Elaboration_Statuses_Index;
   --  Obtain the hash value of entity Key

   Elaboration_Statuses_In_Use : Boolean := False;
   --  This flag flag determines whether table Elaboration_Statuses contains at
   --  least one key/value pair.

   Elaboration_Statuses_No_Element : constant Elaboration_Attributes :=
                                       No_Elaboration_Attributes;

   package Elaboration_Statuses is new Simple_HTable
     (Header_Num => Elaboration_Statuses_Index,
      Element    => Elaboration_Attributes,
      No_Element => Elaboration_Statuses_No_Element,
      Key        => Entity_Id,
      Hash       => Elaboration_Statuses_Hash,
      Equal      => "=");

   --  The following table stores a status flag for each SPARK scenario saved
   --  in table SPARK_Scenarios.

   Recorded_SPARK_Scenarios_Max : constant := 127;

   type Recorded_SPARK_Scenarios_Index is
     range 0 .. Recorded_SPARK_Scenarios_Max - 1;

   function Recorded_SPARK_Scenarios_Hash
     (Key : Node_Id) return Recorded_SPARK_Scenarios_Index;
   --  Obtain the hash value of Key

   Recorded_SPARK_Scenarios_In_Use : Boolean := False;
   --  This flag flag determines whether table Recorded_SPARK_Scenarios
   --  contains at least one key/value pair.

   Recorded_SPARK_Scenarios_No_Element : constant Boolean := False;

   package Recorded_SPARK_Scenarios is new Simple_HTable
     (Header_Num => Recorded_SPARK_Scenarios_Index,
      Element    => Boolean,
      No_Element => Recorded_SPARK_Scenarios_No_Element,
      Key        => Node_Id,
      Hash       => Recorded_SPARK_Scenarios_Hash,
      Equal      => "=");

   --  The following table stores a status flag for each top-level scenario
   --  recorded in table Top_Level_Scenarios.

   Recorded_Top_Level_Scenarios_Max : constant := 503;

   type Recorded_Top_Level_Scenarios_Index is
     range 0 .. Recorded_Top_Level_Scenarios_Max - 1;

   function Recorded_Top_Level_Scenarios_Hash
     (Key : Node_Id) return Recorded_Top_Level_Scenarios_Index;
   --  Obtain the hash value of entity Key

   Recorded_Top_Level_Scenarios_In_Use : Boolean := False;
   --  This flag flag determines whether table Recorded_Top_Level_Scenarios
   --  contains at least one key/value pair.

   Recorded_Top_Level_Scenarios_No_Element : constant Boolean := False;

   package Recorded_Top_Level_Scenarios is new Simple_HTable
     (Header_Num => Recorded_Top_Level_Scenarios_Index,
      Element    => Boolean,
      No_Element => Recorded_Top_Level_Scenarios_No_Element,
      Key        => Node_Id,
      Hash       => Recorded_Top_Level_Scenarios_Hash,
      Equal      => "=");

   --  The following table stores all active scenarios in a recursive traversal
   --  starting from a top-level scenario. This table must be maintained in a
   --  FIFO fashion.

   package Scenario_Stack is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100,
      Table_Name           => "Scenario_Stack");

   --  The following table stores SPARK scenarios which are not necessarily
   --  executable during elaboration, but still require elaboration-related
   --  checks.

   package SPARK_Scenarios is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100,
      Table_Name           => "SPARK_Scenarios");

   --  The following table stores all top-level scenario saved during the
   --  Recording phase. The contents of this table act as traversal roots
   --  later in the Processing phase. This table must be maintained in a
   --  LIFO fashion.

   package Top_Level_Scenarios is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 100,
      Table_Name           => "Top_Level_Scenarios");

   --  The following table stores the bodies of all eligible scenarios visited
   --  during a traversal starting from a top-level scenario. The contents of
   --  this table must be reset upon each new traversal.

   Visited_Bodies_Max : constant := 511;

   type Visited_Bodies_Index is range 0 .. Visited_Bodies_Max - 1;

   function Visited_Bodies_Hash (Key : Node_Id) return Visited_Bodies_Index;
   --  Obtain the hash value of node Key

   Visited_Bodies_In_Use : Boolean := False;
   --  This flag determines whether table Visited_Bodies contains at least one
   --  key/value pair.

   Visited_Bodies_No_Element : constant Boolean := False;

   package Visited_Bodies is new Simple_HTable
     (Header_Num => Visited_Bodies_Index,
      Element    => Boolean,
      No_Element => Visited_Bodies_No_Element,
      Key        => Node_Id,
      Hash       => Visited_Bodies_Hash,
      Equal      => "=");

   -----------------------
   -- Local subprograms --
   -----------------------

   --  Multiple local subprograms are utilized to lower the semantic complexity
   --  of the Recording and Processing phase.

   procedure Check_Preelaborated_Call (Call : Node_Id);
   pragma Inline (Check_Preelaborated_Call);
   --  Verify that entry, operator, or subprogram call Call does not appear at
   --  the library level of a preelaborated unit.

   procedure Check_SPARK_Derived_Type (Typ_Decl : Node_Id);
   pragma Inline (Check_SPARK_Derived_Type);
   --  Verify that the freeze node of a derived type denoted by declaration
   --  Typ_Decl is within the early call region of each overriding primitive
   --  body that belongs to the derived type (SPARK RM 7.7(8)).

   procedure Check_SPARK_Instantiation (Exp_Inst : Node_Id);
   pragma Inline (Check_SPARK_Instantiation);
   --  Verify that expanded instance Exp_Inst does not precede the generic body
   --  it instantiates (SPARK RM 7.7(6)).

   procedure Check_SPARK_Scenario (N : Node_Id);
   pragma Inline (Check_SPARK_Scenario);
   --  Top-level dispatcher for verifying SPARK scenarios which are not always
   --  executable during elaboration but still need elaboration-related checks.

   procedure Check_SPARK_Refined_State_Pragma (N : Node_Id);
   pragma Inline (Check_SPARK_Refined_State_Pragma);
   --  Verify that each constituent of Refined_State pragma N which belongs to
   --  an abstract state mentioned in pragma Initializes has prior elaboration
   --  with respect to the main unit (SPARK RM 7.7.1(7)).

   function Compilation_Unit (Unit_Id : Entity_Id) return Node_Id;
   pragma Inline (Compilation_Unit);
   --  Return the N_Compilation_Unit node of unit Unit_Id

   function Early_Call_Region (Body_Id : Entity_Id) return Node_Id;
   pragma Inline (Early_Call_Region);
   --  Return the early call region associated with entry or subprogram body
   --  Body_Id. IMPORTANT: This routine does not find the early call region.
   --  To compute it, use routine Find_Early_Call_Region.

   procedure Elab_Msg_NE
     (Msg      : String;
      N        : Node_Id;
      Id       : Entity_Id;
      Info_Msg : Boolean;
      In_SPARK : Boolean);
   pragma Inline (Elab_Msg_NE);
   --  Wrapper around Error_Msg_NE. Emit message Msg concerning arbitrary node
   --  N and entity. If flag Info_Msg is set, the routine emits an information
   --  message, otherwise it emits an error. If flag In_SPARK is set, then
   --  string " in SPARK" is added to the end of the message.

   function Elaboration_Status
     (Unit_Id : Entity_Id) return Elaboration_Attributes;
   pragma Inline (Elaboration_Status);
   --  Return the set of elaboration attributes associated with unit Unit_Id

   procedure Ensure_Prior_Elaboration
     (N              : Node_Id;
      Unit_Id        : Entity_Id;
      Prag_Nam       : Name_Id;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Guarantee the elaboration of unit Unit_Id with respect to the main unit
   --  by installing pragma Elaborate or Elaborate_All denoted by Prag_Nam. N
   --  denotes the related scenario. The flags should be set when the need for
   --  elaboration was initiated as follows:
   --
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Ensure_Prior_Elaboration_Dynamic
     (N        : Node_Id;
      Unit_Id  : Entity_Id;
      Prag_Nam : Name_Id);
   --  Guarantee the elaboration of unit Unit_Id with respect to the main unit
   --  by suggesting the use of Elaborate[_All] with name Prag_Nam. N denotes
   --  the related scenario.

   procedure Ensure_Prior_Elaboration_Static
     (N        : Node_Id;
      Unit_Id  : Entity_Id;
      Prag_Nam : Name_Id);
   --  Guarantee the elaboration of unit Unit_Id with respect to the main unit
   --  by installing an implicit Elaborate[_All] pragma with name Prag_Nam. N
   --  denotes the related scenario.

   function Extract_Assignment_Name (Asmt : Node_Id) return Node_Id;
   pragma Inline (Extract_Assignment_Name);
   --  Obtain the Name attribute of assignment statement Asmt

   procedure Extract_Call_Attributes
     (Call      : Node_Id;
      Target_Id : out Entity_Id;
      Attrs     : out Call_Attributes);
   pragma Inline (Extract_Call_Attributes);
   --  Obtain attributes Attrs associated with call Call. Target_Id is the
   --  entity of the call target.

   function Extract_Call_Name (Call : Node_Id) return Node_Id;
   pragma Inline (Extract_Call_Name);
   --  Obtain the Name attribute of entry or subprogram call Call

   procedure Extract_Instance_Attributes
     (Exp_Inst  : Node_Id;
      Inst_Body : out Node_Id;
      Inst_Decl : out Node_Id);
   pragma Inline (Extract_Instance_Attributes);
   --  Obtain body Inst_Body and spec Inst_Decl of expanded instance Exp_Inst

   procedure Extract_Instantiation_Attributes
     (Exp_Inst : Node_Id;
      Inst     : out Node_Id;
      Inst_Id  : out Entity_Id;
      Gen_Id   : out Entity_Id;
      Attrs    : out Instantiation_Attributes);
   pragma Inline (Extract_Instantiation_Attributes);
   --  Obtain attributes Attrs associated with expanded instantiation Exp_Inst.
   --  Inst is the instantiation. Inst_Id is the entity of the instance. Gen_Id
   --  is the entity of the generic unit being instantiated.

   procedure Extract_Target_Attributes
     (Target_Id : Entity_Id;
      Attrs     : out Target_Attributes);
   --  Obtain attributes Attrs associated with an entry, package, or subprogram
   --  denoted by Target_Id.

   procedure Extract_Task_Attributes
     (Typ   : Entity_Id;
      Attrs : out Task_Attributes);
   pragma Inline (Extract_Task_Attributes);
   --  Obtain attributes Attrs associated with task type Typ

   procedure Extract_Variable_Reference_Attributes
     (Ref    : Node_Id;
      Var_Id : out Entity_Id;
      Attrs  : out Variable_Attributes);
   pragma Inline (Extract_Variable_Reference_Attributes);
   --  Obtain attributes Attrs associated with reference Ref that mentions
   --  variable Var_Id.

   function Find_Code_Unit (N : Node_Or_Entity_Id) return Entity_Id;
   pragma Inline (Find_Code_Unit);
   --  Return the code unit which contains arbitrary node or entity N. This
   --  is the unit of the file which physically contains the related construct
   --  denoted by N except when N is within an instantiation. In that case the
   --  unit is that of the top-level instantiation.

   function Find_Early_Call_Region
     (Body_Decl        : Node_Id;
      Assume_Elab_Body : Boolean := False;
      Skip_Memoization : Boolean := False) return Node_Id;
   --  Find the start of the early call region which belongs to subprogram body
   --  Body_Decl as defined in SPARK RM 7.7. The behavior of the routine is to
   --  find the early call region, memoize it, and return it, but this behavior
   --  can be altered. Flag Assume_Elab_Body should be set when a package spec
   --  may lack pragma Elaborate_Body, but the routine must still examine that
   --  spec. Flag Skip_Memoization should be set when the routine must avoid
   --  memoizing the region.

   procedure Find_Elaborated_Units;
   --  Populate table Elaboration_Statuses with all units which have prior
   --  elaboration with respect to the main unit.

   function Find_Enclosing_Instance (N : Node_Id) return Node_Id;
   pragma Inline (Find_Enclosing_Instance);
   --  Find the declaration or body of the nearest expanded instance which
   --  encloses arbitrary node N. Return Empty if no such instance exists.

   function Find_Top_Unit (N : Node_Or_Entity_Id) return Entity_Id;
   pragma Inline (Find_Top_Unit);
   --  Return the top unit which contains arbitrary node or entity N. The unit
   --  is obtained by logically unwinding instantiations and subunits when N
   --  resides within one.

   function Find_Unit_Entity (N : Node_Id) return Entity_Id;
   pragma Inline (Find_Unit_Entity);
   --  Return the entity of unit N

   function First_Formal_Type (Subp_Id : Entity_Id) return Entity_Id;
   pragma Inline (First_Formal_Type);
   --  Return the type of subprogram Subp_Id's first formal parameter. If the
   --  subprogram lacks formal parameters, return Empty.

   function Has_Body (Pack_Decl : Node_Id) return Boolean;
   --  Determine whether package declaration Pack_Decl has a corresponding body
   --  or would eventually have one.

   function Has_Prior_Elaboration
     (Unit_Id      : Entity_Id;
      Context_OK   : Boolean := False;
      Elab_Body_OK : Boolean := False;
      Same_Unit_OK : Boolean := False) return Boolean;
   pragma Inline (Has_Prior_Elaboration);
   --  Determine whether unit Unit_Id is elaborated prior to the main unit.
   --  If flag Context_OK is set, the routine considers the following case
   --  as valid prior elaboration:
   --
   --    * Unit_Id is in the elaboration context of the main unit
   --
   --  If flag Elab_Body_OK is set, the routine considers the following case
   --  as valid prior elaboration:
   --
   --    * Unit_Id has pragma Elaborate_Body and is not the main unit
   --
   --  If flag Same_Unit_OK is set, the routine considers the following cases
   --  as valid prior elaboration:
   --
   --    * Unit_Id is the main unit
   --
   --    * Unit_Id denotes the spec of the main unit body

   function In_External_Instance
     (N           : Node_Id;
      Target_Decl : Node_Id) return Boolean;
   pragma Inline (In_External_Instance);
   --  Determine whether a target desctibed by its declaration Target_Decl
   --  resides in a package instance which is external to scenario N.

   function In_Main_Context (N : Node_Id) return Boolean;
   pragma Inline (In_Main_Context);
   --  Determine whether arbitrary node N appears within the main compilation
   --  unit.

   function In_Same_Context
     (N1        : Node_Id;
      N2        : Node_Id;
      Nested_OK : Boolean := False) return Boolean;
   --  Determine whether two arbitrary nodes N1 and N2 appear within the same
   --  context ignoring enclosing library levels. Nested_OK should be set when
   --  the context of N1 can enclose that of N2.

   procedure Info_Call
     (Call      : Node_Id;
      Target_Id : Entity_Id;
      Info_Msg  : Boolean;
      In_SPARK  : Boolean);
   --  Output information concerning call Call which invokes target Target_Id.
   --  If flag Info_Msg is set, the routine emits an information message,
   --  otherwise it emits an error. If flag In_SPARK is set, then the string
   --  " in SPARK" is added to the end of the message.

   procedure Info_Instantiation
     (Inst     : Node_Id;
      Gen_Id   : Entity_Id;
      Info_Msg : Boolean;
      In_SPARK : Boolean);
   pragma Inline (Info_Instantiation);
   --  Output information concerning instantiation Inst which instantiates
   --  generic unit Gen_Id. If flag Info_Msg is set, the routine emits an
   --  information message, otherwise it emits an error. If flag In_SPARK
   --  is set, then string " in SPARK" is added to the end of the message.

   procedure Info_Variable_Reference
     (Ref      : Node_Id;
      Var_Id   : Entity_Id;
      Info_Msg : Boolean;
      In_SPARK : Boolean);
   pragma Inline (Info_Variable_Reference);
   --  Output information concerning reference Ref which mentions variable
   --  Var_Id. If flag Info_Msg is set, the routine emits an information
   --  message, otherwise it emits an error. If flag In_SPARK is set, then
   --  string " in SPARK" is added to the end of the message.

   function Insertion_Node (N : Node_Id; Ins_Nod : Node_Id) return Node_Id;
   pragma Inline (Insertion_Node);
   --  Obtain the proper insertion node of an ABE check or failure for scenario
   --  N and candidate insertion node Ins_Nod.

   procedure Install_ABE_Check
     (N       : Node_Id;
      Id      : Entity_Id;
      Ins_Nod : Node_Id);
   --  Insert a run-time ABE check for elaboration scenario N which verifies
   --  whether arbitrary entity Id is elaborated. The check in inserted prior
   --  to node Ins_Nod.

   procedure Install_ABE_Check
     (N           : Node_Id;
      Target_Id   : Entity_Id;
      Target_Decl : Node_Id;
      Target_Body : Node_Id;
      Ins_Nod     : Node_Id);
   --  Insert a run-time ABE check for elaboration scenario N which verifies
   --  whether target Target_Id with initial declaration Target_Decl and body
   --  Target_Body is elaborated. The check is inserted prior to node Ins_Nod.

   procedure Install_ABE_Failure (N : Node_Id; Ins_Nod : Node_Id);
   --  Insert a Program_Error concerning a guaranteed ABE for elaboration
   --  scenario N. The failure is inserted prior to node Node_Id.

   function Is_Accept_Alternative_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Accept_Alternative_Proc);
   --  Determine whether arbitrary entity Id denotes an internally generated
   --  procedure which encapsulates the statements of an accept alternative.

   function Is_Activation_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Activation_Proc);
   --  Determine whether arbitrary entity Id denotes a runtime procedure in
   --  charge with activating tasks.

   function Is_Ada_Semantic_Target (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Ada_Semantic_Target);
   --  Determine whether arbitrary entity Id nodes a source or internally
   --  generated subprogram which emulates Ada semantics.

   function Is_Bodiless_Subprogram (Subp_Id : Entity_Id) return Boolean;
   pragma Inline (Is_Bodiless_Subprogram);
   --  Determine whether subprogram Subp_Id will never have a body

   function Is_Controlled_Proc
     (Subp_Id  : Entity_Id;
      Subp_Nam : Name_Id) return Boolean;
   pragma Inline (Is_Controlled_Proc);
   --  Determine whether subprogram Subp_Id denotes controlled type primitives
   --  Adjust, Finalize, or Initialize as denoted by name Subp_Nam.

   function Is_Default_Initial_Condition_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Default_Initial_Condition_Proc);
   --  Determine whether arbitrary entity Id denotes internally generated
   --  routine Default_Initial_Condition.

   function Is_Finalizer_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Finalizer_Proc);
   --  Determine whether arbitrary entity Id denotes internally generated
   --  routine _Finalizer.

   function Is_Guaranteed_ABE
     (N           : Node_Id;
      Target_Decl : Node_Id;
      Target_Body : Node_Id) return Boolean;
   pragma Inline (Is_Guaranteed_ABE);
   --  Determine whether scenario N with a target described by its initial
   --  declaration Target_Decl and body Target_Decl results in a guaranteed
   --  ABE.

   function Is_Initial_Condition_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Initial_Condition_Proc);
   --  Determine whether arbitrary entity Id denotes internally generated
   --  routine Initial_Condition.

   function Is_Initialized (Obj_Decl : Node_Id) return Boolean;
   pragma Inline (Is_Initialized);
   --  Determine whether object declaration Obj_Decl is initialized

   function Is_Invariant_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Invariant_Proc);
   --  Determine whether arbitrary entity Id denotes an invariant procedure

   function Is_Non_Library_Level_Encapsulator (N : Node_Id) return Boolean;
   pragma Inline (Is_Non_Library_Level_Encapsulator);
   --  Determine whether arbitrary node N is a non-library encapsulator

   function Is_Partial_Invariant_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Partial_Invariant_Proc);
   --  Determine whether arbitrary entity Id denotes a partial invariant
   --  procedure.

   function Is_Postconditions_Proc (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Postconditions_Proc);
   --  Determine whether arbitrary entity Id denotes internally generated
   --  routine _Postconditions.

   function Is_Preelaborated_Unit (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Preelaborated_Unit);
   --  Determine whether arbitrary entity Id denotes a unit which is subject to
   --  one of the following pragmas:
   --
   --    * Preelaborable
   --    * Pure
   --    * Remote_Call_Interface
   --    * Remote_Types
   --    * Shared_Passive

   function Is_Protected_Entry (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Protected_Entry);
   --  Determine whether arbitrary entity Id denotes a protected entry

   function Is_Protected_Subp (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Protected_Subp);
   --  Determine whether entity Id denotes a protected subprogram

   function Is_Protected_Body_Subp (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Protected_Body_Subp);
   --  Determine whether entity Id denotes the protected or unprotected version
   --  of a protected subprogram.

   function Is_Recorded_SPARK_Scenario (N : Node_Id) return Boolean;
   pragma Inline (Is_Recorded_SPARK_Scenario);
   --  Determine whether arbitrary node N is a recorded SPARK scenario which
   --  appears in table SPARK_Scenarios.

   function Is_Recorded_Top_Level_Scenario (N : Node_Id) return Boolean;
   pragma Inline (Is_Recorded_Top_Level_Scenario);
   --  Determine whether arbitrary node N is a recorded top-level scenario
   --  which appears in table Top_Level_Scenarios.

   function Is_Safe_Activation
     (Call      : Node_Id;
      Task_Decl : Node_Id) return Boolean;
   pragma Inline (Is_Safe_Activation);
   --  Determine whether call Call which activates a task object described by
   --  declaration Task_Decl is always ABE-safe.

   function Is_Safe_Call
     (Call         : Node_Id;
      Target_Attrs : Target_Attributes) return Boolean;
   pragma Inline (Is_Safe_Call);
   --  Determine whether call Call which invokes a target described by
   --  attributes Target_Attrs is always ABE-safe.

   function Is_Safe_Instantiation
     (Inst      : Node_Id;
      Gen_Attrs : Target_Attributes) return Boolean;
   pragma Inline (Is_Safe_Instantiation);
   --  Determine whether instance Inst which instantiates a generic unit
   --  described by attributes Gen_Attrs is always ABE-safe.

   function Is_Same_Unit
     (Unit_1 : Entity_Id;
      Unit_2 : Entity_Id) return Boolean;
   pragma Inline (Is_Same_Unit);
   --  Determine whether entities Unit_1 and Unit_2 denote the same unit

   function Is_Scenario (N : Node_Id) return Boolean;
   pragma Inline (Is_Scenario);
   --  Determine whether attribute node N denotes a scenario. The scenario may
   --  not necessarily be eligible for ABE processing.

   function Is_SPARK_Semantic_Target (Id : Entity_Id) return Boolean;
   pragma Inline (Is_SPARK_Semantic_Target);
   --  Determine whether arbitrary entity Id nodes a source or internally
   --  generated subprogram which emulates SPARK semantics.

   function Is_Suitable_Access (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_Access);
   --  Determine whether arbitrary node N denotes a suitable attribute for ABE
   --  processing.

   function Is_Suitable_Call (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_Call);
   --  Determine whether arbitrary node N denotes a suitable call for ABE
   --  processing.

   function Is_Suitable_Instantiation (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_Instantiation);
   --  Determine whether arbitrary node N is a suitable instantiation for ABE
   --  processing.

   function Is_Suitable_Scenario (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_Scenario);
   --  Determine whether arbitrary node N is a suitable scenario for ABE
   --  processing.

   function Is_Suitable_SPARK_Derived_Type (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_SPARK_Derived_Type);
   --  Determine whether arbitrary node N denotes a suitable derived type
   --  declaration for ABE processing using the SPARK rules.

   function Is_Suitable_SPARK_Instantiation (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_SPARK_Instantiation);
   --  Determine whether arbitrary node N denotes a suitable instantiation for
   --  ABE processing using the SPARK rules.

   function Is_Suitable_SPARK_Refined_State_Pragma
     (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_SPARK_Refined_State_Pragma);
   --  Determine whether arbitrary node N denotes a suitable Refined_State
   --  pragma for ABE processing using the SPARK rules.

   function Is_Suitable_Variable_Assignment (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_Variable_Assignment);
   --  Determine whether arbitrary node N denotes a suitable assignment for ABE
   --  processing.

   function Is_Suitable_Variable_Reference (N : Node_Id) return Boolean;
   pragma Inline (Is_Suitable_Variable_Reference);
   --  Determine whether arbitrary node N is a suitable variable reference for
   --  ABE processing.

   function Is_Task_Entry (Id : Entity_Id) return Boolean;
   pragma Inline (Is_Task_Entry);
   --  Determine whether arbitrary entity Id denotes a task entry

   function Is_Up_Level_Target (Target_Decl : Node_Id) return Boolean;
   pragma Inline (Is_Up_Level_Target);
   --  Determine whether the current root resides at the declaration level. If
   --  this is the case, determine whether a target described by declaration
   --  Target_Decl is within a context which encloses the current root or is in
   --  a different unit.

   function Is_Visited_Body (Body_Decl : Node_Id) return Boolean;
   pragma Inline (Is_Visited_Body);
   --  Determine whether subprogram body Body_Decl is already visited during a
   --  recursive traversal started from a top-level scenario.

   procedure Meet_Elaboration_Requirement
     (N         : Node_Id;
      Target_Id : Entity_Id;
      Req_Nam   : Name_Id);
   --  Determine whether elaboration requirement Req_Nam for scenario N with
   --  target Target_Id is met by the context of the main unit using the SPARK
   --  rules. Req_Nam must denote either Elaborate or Elaborate_All. Emit an
   --  error if this is not the case.

   function Non_Private_View (Typ : Entity_Id) return Entity_Id;
   pragma Inline (Non_Private_View);
   --  Return the full view of private type Typ if available, otherwise return
   --  type Typ.

   procedure Output_Active_Scenarios (Error_Nod : Node_Id);
   --  Output the contents of the active scenario stack from earliest to latest
   --  to supplement an earlier error emitted for node Error_Nod.

   procedure Pop_Active_Scenario (N : Node_Id);
   pragma Inline (Pop_Active_Scenario);
   --  Pop the top of the scenario stack. A check is made to ensure that the
   --  scenario being removed is the same as N.

   generic
      with procedure Process_Single_Activation
        (Call           : Node_Id;
         Call_Attrs     : Call_Attributes;
         Obj_Id         : Entity_Id;
         Task_Attrs     : Task_Attributes;
         In_Init_Cond   : Boolean;
         In_Partial_Fin : Boolean;
         In_Task_Body   : Boolean);
      --  Perform ABE checks and diagnostics for task activation call Call
      --  which activates task Obj_Id. Call_Attrs are the attributes of the
      --  activation call. Task_Attrs are the attributes of the task type.
      --  The flags should be set when the processing was initiated as follows:
      --
      --    In_Init_Cond   - initial condition procedure
      --    In_Partial_Fin - partial finalization procedure
      --    In_Task_Body   - task body

   procedure Process_Activation_Generic
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform ABE checks and diagnostics for activation call Call by invoking
   --  routine Process_Single_Activation on each task object being activated.
   --  Call_Attrs are the attributes of the activation call. The flags should
   --  be set when the processing was initiated as follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE
     (N              : Node_Id;
      In_Init_Cond   : Boolean := False;
      In_Partial_Fin : Boolean := False;
      In_Task_Body   : Boolean := False);
   --  Top-level dispatcher for processing of various elaboration scenarios.
   --  Perform conditional ABE checks and diagnostics for scenario N. The flags
   --  should be set when the processing was initiated as follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Access
     (Attr           : Node_Id;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform ABE checks and diagnostics for 'Access to entry, operator, or
   --  subprogram denoted by Attr. The flags should be set when the processing
   --  was initiated as follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Activation_Impl
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Obj_Id         : Entity_Id;
      Task_Attrs     : Task_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform common conditional ABE checks and diagnostics for call Call
   --  which activates task Obj_Id ignoring the Ada or SPARK rules. Call_Attrs
   --  are the attributes of the activation call. Task_Attrs are the attributes
   --  of the task type. The flags should be set when the processing was
   --  initiated as follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Call
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Target_Id      : Entity_Id;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Top-level dispatcher for processing of calls. Perform ABE checks and
   --  diagnostics for call Call which invokes target Target_Id. Call_Attrs
   --  are the attributes of the call. The flags should be set when the
   --  processing was initiated as follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Call_Ada
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Target_Id      : Entity_Id;
      Target_Attrs   : Target_Attributes;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform ABE checks and diagnostics for call Call which invokes target
   --  Target_Id using the Ada rules. Call_Attrs are the attributes of the
   --  call. Target_Attrs are attributes of the target. The flags should be
   --  set when the processing was initiated as follows:
   --
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Call_SPARK
     (Call           : Node_Id;
      Target_Id      : Entity_Id;
      Target_Attrs   : Target_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform ABE checks and diagnostics for call Call which invokes target
   --  Target_Id using the SPARK rules. Target_Attrs denotes the attributes of
   --  the target. The flags should be set when the processing was initiated as
   --  follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Instantiation
     (Exp_Inst       : Node_Id;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Top-level dispatcher for processing of instantiations. Perform ABE
   --  checks and diagnostics for expanded instantiation Exp_Inst. The flags
   --  should be set when the processing was initiated as follows:
   --
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Instantiation_Ada
     (Exp_Inst       : Node_Id;
      Inst           : Node_Id;
      Inst_Attrs     : Instantiation_Attributes;
      Gen_Id         : Entity_Id;
      Gen_Attrs      : Target_Attributes;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform ABE checks and diagnostics for expanded instantiation Exp_Inst
   --  of generic Gen_Id using the Ada rules. Inst is the instantiation node.
   --  Inst_Attrs are the attributes of the instance. Gen_Attrs denotes the
   --  attributes of the generic. The flags should be set when the processing
   --  was initiated as follows:
   --
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Instantiation_SPARK
     (Inst           : Node_Id;
      Gen_Id         : Entity_Id;
      Gen_Attrs      : Target_Attributes;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform ABE checks and diagnostics for instantiation Inst of generic
   --  Gen_Id using the SPARK rules. Gen_Attrs denotes the attributes of the
   --  generic. The flags should be set when the processing was initiated as
   --  follows:
   --
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Process_Conditional_ABE_Variable_Assignment (Asmt : Node_Id);
   --  Top-level dispatcher for processing of variable assignments. Perform ABE
   --  checks and diagnostics for assignment statement Asmt.

   procedure Process_Conditional_ABE_Variable_Assignment_Ada
     (Asmt   : Node_Id;
      Var_Id : Entity_Id);
   --  Perform ABE checks and diagnostics for assignment statement Asmt that
   --  updates the value of variable Var_Id using the Ada rules.

   procedure Process_Conditional_ABE_Variable_Assignment_SPARK
     (Asmt   : Node_Id;
      Var_Id : Entity_Id);
   --  Perform ABE checks and diagnostics for assignment statement Asmt that
   --  updates the value of variable Var_Id using the SPARK rules.

   procedure Process_Conditional_ABE_Variable_Reference (Ref : Node_Id);
   --  Top-level dispatcher for processing of variable references. Perform ABE
   --  checks and diagnostics for variable reference Ref.

   procedure Process_Conditional_ABE_Variable_Reference_Read
     (Ref    : Node_Id;
      Var_Id : Entity_Id;
      Attrs  : Variable_Attributes);
   --  Perform ABE checks and diagnostics for reference Ref described by its
   --  attributes Attrs, that reads variable Var_Id.

   procedure Process_Guaranteed_ABE (N : Node_Id);
   --  Top-level dispatcher for processing of scenarios which result in a
   --  guaranteed ABE.

   procedure Process_Guaranteed_ABE_Activation_Impl
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Obj_Id         : Entity_Id;
      Task_Attrs     : Task_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Perform common guaranteed ABE checks and diagnostics for call Call which
   --  activates task Obj_Id ignoring the Ada or SPARK rules. Call_Attrs are
   --  the attributes of the activation call. Task_Attrs are the attributes of
   --  the task type. The following parameters are provided for compatibility
   --  and are not used.
   --
   --    In_Init_Cond
   --    In_Partial_Fin
   --    In_Task_Body

   procedure Process_Guaranteed_ABE_Call
     (Call       : Node_Id;
      Call_Attrs : Call_Attributes;
      Target_Id  : Entity_Id);
   --  Perform common guaranteed ABE checks and diagnostics for call Call which
   --  invokes target Target_Id ignoring the Ada or SPARK rules. Call_Attrs are
   --  the attributes of the call.

   procedure Process_Guaranteed_ABE_Instantiation (Exp_Inst : Node_Id);
   --  Perform common guaranteed ABE checks and diagnostics for expanded
   --  instantiation Exp_Inst of generic Gen_Id ignoring the Ada or SPARK
   --  rules.

   procedure Push_Active_Scenario (N : Node_Id);
   pragma Inline (Push_Active_Scenario);
   --  Push scenario N on top of the scenario stack

   procedure Record_SPARK_Elaboration_Scenario (N : Node_Id);
   pragma Inline (Record_SPARK_Elaboration_Scenario);
   --  Save SPARK scenario N in table SPARK_Scenarios for later processing

   procedure Reset_Visited_Bodies;
   pragma Inline (Reset_Visited_Bodies);
   --  Clear the contents of table Visited_Bodies

   function Root_Scenario return Node_Id;
   pragma Inline (Root_Scenario);
   --  Return the top-level scenario which started a recursive search for other
   --  scenarios. It is assumed that there is a valid top-level scenario on the
   --  active scenario stack.

   procedure Set_Early_Call_Region (Body_Id : Entity_Id; Start : Node_Id);
   pragma Inline (Set_Early_Call_Region);
   --  Associate an early call region with begins at construct Start with entry
   --  or subprogram body Body_Id.

   procedure Set_Elaboration_Status
     (Unit_Id : Entity_Id;
      Val     : Elaboration_Attributes);
   pragma Inline (Set_Elaboration_Status);
   --  Associate an set of elaboration attributes with unit Unit_Id

   procedure Set_Is_Recorded_SPARK_Scenario
     (N   : Node_Id;
      Val : Boolean := True);
   pragma Inline (Set_Is_Recorded_SPARK_Scenario);
   --  Mark scenario N as being recorded in table SPARK_Scenarios

   procedure Set_Is_Recorded_Top_Level_Scenario
     (N   : Node_Id;
      Val : Boolean := True);
   pragma Inline (Set_Is_Recorded_Top_Level_Scenario);
   --  Mark scenario N as being recorded in table Top_Level_Scenarios

   procedure Set_Is_Visited_Body (Subp_Body : Node_Id);
   pragma Inline (Set_Is_Visited_Body);
   --  Mark subprogram body Subp_Body as being visited during a recursive
   --  traversal started from a top-level scenario.

   function Static_Elaboration_Checks return Boolean;
   pragma Inline (Static_Elaboration_Checks);
   --  Determine whether the static model is in effect

   procedure Traverse_Body
     (N              : Node_Id;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean);
   --  Inspect the declarations and statements of subprogram body N for
   --  suitable elaboration scenarios and process them. The flags should
   --  be set when the processing was initiated as follows:
   --
   --    In_Init_Cond   - initial condition procedure
   --    In_Partial_Fin - partial finalization procedure
   --    In_Task_Body   - task body

   procedure Update_Elaboration_Scenario (New_N : Node_Id; Old_N : Node_Id);
   pragma Inline (Update_Elaboration_Scenario);
   --  Update all relevant internal data structures when scenario Old_N is
   --  transformed into scenario New_N by Atree.Rewrite.

   -----------------------
   -- Build_Call_Marker --
   -----------------------

   procedure Build_Call_Marker (N : Node_Id) is
      function In_External_Context
        (Call      : Node_Id;
         Target_Id : Entity_Id) return Boolean;
      pragma Inline (In_External_Context);
      --  Determine whether target Target_Id is external to call N which must
      --  reside within an instance.

      function In_Premature_Context (Call : Node_Id) return Boolean;
      --  Determine whether call Call appears within a premature context

      function Is_Bridge_Target (Id : Entity_Id) return Boolean;
      pragma Inline (Is_Bridge_Target);
      --  Determine whether arbitrary entity Id denotes a bridge target

      function Is_Default_Expression (Call : Node_Id) return Boolean;
      pragma Inline (Is_Default_Expression);
      --  Determine whether call Call acts as the expression of a defaulted
      --  parameter within a source call.

      function Is_Generic_Formal_Subp (Subp_Id : Entity_Id) return Boolean;
      pragma Inline (Is_Generic_Formal_Subp);
      --  Determine whether subprogram Subp_Id denotes a generic formal
      --  subprogram which appears in the "prologue" of an instantiation.

      -------------------------
      -- In_External_Context --
      -------------------------

      function In_External_Context
        (Call      : Node_Id;
         Target_Id : Entity_Id) return Boolean
      is
         Target_Decl : constant Node_Id := Unit_Declaration_Node (Target_Id);

         Inst      : Node_Id;
         Inst_Body : Node_Id;
         Inst_Decl : Node_Id;

      begin
         --  Performance note: parent traversal

         Inst := Find_Enclosing_Instance (Call);

         --  The call appears within an instance

         if Present (Inst) then

            --  The call comes from the main unit and the target does not

            if In_Extended_Main_Code_Unit (Call)
              and then not In_Extended_Main_Code_Unit (Target_Decl)
            then
               return True;

            --  Otherwise the target declaration must not appear within the
            --  instance spec or body.

            else
               Extract_Instance_Attributes
                 (Exp_Inst  => Inst,
                  Inst_Decl => Inst_Decl,
                  Inst_Body => Inst_Body);

               --  Performance note: parent traversal

               return not In_Subtree
                            (N     => Target_Decl,
                             Root1 => Inst_Decl,
                             Root2 => Inst_Body);
            end if;
         end if;

         return False;
      end In_External_Context;

      --------------------------
      -- In_Premature_Context --
      --------------------------

      function In_Premature_Context (Call : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         --  Climb the parent chain looking for premature contexts

         Par := Parent (Call);
         while Present (Par) loop

            --  Aspect specifications and generic associations are premature
            --  contexts because nested calls has not been relocated to their
            --  final context.

            if Nkind_In (Par, N_Aspect_Specification,
                              N_Generic_Association)
            then
               return True;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Premature_Context;

      ----------------------
      -- Is_Bridge_Target --
      ----------------------

      function Is_Bridge_Target (Id : Entity_Id) return Boolean is
      begin
         return
           Is_Accept_Alternative_Proc (Id)
             or else Is_Finalizer_Proc (Id)
             or else Is_Partial_Invariant_Proc (Id)
             or else Is_Postconditions_Proc (Id)
             or else Is_TSS (Id, TSS_Deep_Adjust)
             or else Is_TSS (Id, TSS_Deep_Finalize)
             or else Is_TSS (Id, TSS_Deep_Initialize);
      end Is_Bridge_Target;

      ---------------------------
      -- Is_Default_Expression --
      ---------------------------

      function Is_Default_Expression (Call : Node_Id) return Boolean is
         Outer_Call : constant Node_Id := Parent (Call);
         Outer_Nam  : Node_Id;

      begin
         --  To qualify, the node must appear immediately within a source call
         --  which invokes a source target.

         if Nkind_In (Outer_Call, N_Entry_Call_Statement,
                                  N_Function_Call,
                                  N_Procedure_Call_Statement)
           and then Comes_From_Source (Outer_Call)
         then
            Outer_Nam := Extract_Call_Name (Outer_Call);

            return
              Is_Entity_Name (Outer_Nam)
                and then Present (Entity (Outer_Nam))
                and then Is_Subprogram_Or_Entry (Entity (Outer_Nam))
                and then Comes_From_Source (Entity (Outer_Nam));
         end if;

         return False;
      end Is_Default_Expression;

      ----------------------------
      -- Is_Generic_Formal_Subp --
      ----------------------------

      function Is_Generic_Formal_Subp (Subp_Id : Entity_Id) return Boolean is
         Subp_Decl : constant Node_Id := Unit_Declaration_Node (Subp_Id);
         Context   : constant Node_Id := Parent (Subp_Decl);

      begin
         --  To qualify, the subprogram must rename a generic actual subprogram
         --  where the enclosing context is an instantiation.

         return
           Nkind (Subp_Decl) = N_Subprogram_Renaming_Declaration
             and then not Comes_From_Source (Subp_Decl)
             and then Nkind_In (Context, N_Function_Specification,
                                         N_Package_Specification,
                                         N_Procedure_Specification)
             and then Present (Generic_Parent (Context));
      end Is_Generic_Formal_Subp;

      --  Local variables

      Call_Attrs : Call_Attributes;
      Call_Nam   : Node_Id;
      Marker     : Node_Id;
      Target_Id  : Entity_Id;

   --  Start of processing for Build_Call_Marker

   begin
      --  Nothing to do for ASIS. As a result, ABE checks and diagnostics are
      --  not performed in this mode.

      if ASIS_Mode then
         return;

      --  Nothing to do when the call is being preanalyzed as the marker will
      --  be inserted in the wrong place.

      elsif Preanalysis_Active then
         return;

      --  Nothing to do when the input does not denote a call or a requeue

      elsif not Nkind_In (N, N_Entry_Call_Statement,
                             N_Function_Call,
                             N_Procedure_Call_Statement,
                             N_Requeue_Statement)
      then
         return;

      --  Nothing to do when the call is analyzed/resolved too early within an
      --  intermediate context.

      --  Performance note: parent traversal

      elsif In_Premature_Context (N) then
         return;
      end if;

      Call_Nam := Extract_Call_Name (N);

      --  Nothing to do when the call is erroneous or left in a bad state

      if not (Is_Entity_Name (Call_Nam)
               and then Present (Entity (Call_Nam))
               and then Is_Subprogram_Or_Entry (Entity (Call_Nam)))
      then
         return;

      --  Nothing to do when the call invokes a generic formal subprogram and
      --  switch -gnatd.G (ignore calls through generic formal parameters for
      --  elaboration) is in effect. This check must be performed with the
      --  direct target of the call to avoid the side effects of mapping
      --  actuals to formals using renamings.

      elsif Debug_Flag_Dot_GG
        and then Is_Generic_Formal_Subp (Entity (Call_Nam))
      then
         return;
      end if;

      Extract_Call_Attributes
        (Call      => N,
         Target_Id => Target_Id,
         Attrs     => Call_Attrs);

      --  Nothing to do when the call appears within the expanded spec or
      --  body of an instantiated generic, the call does not invoke a generic
      --  formal subprogram, the target is external to the instance, and switch
      --  -gnatdL (ignore external calls from instances for elaboration) is in
      --  effect. This behaviour approximates that of the old ABE mechanism.

      if Debug_Flag_LL
        and then not Is_Generic_Formal_Subp (Entity (Call_Nam))

        --  Performance note: parent traversal

        and then In_External_Context
                   (Call      => N,
                    Target_Id => Target_Id)
      then
         return;

      --  Source calls to source targets are always considered because they
      --  reflect the original call graph.

      elsif Comes_From_Source (Target_Id) and then Call_Attrs.From_Source then
         null;

      --  A call to a source function which acts as the default expression in
      --  another call requires special detection.

      elsif Comes_From_Source (Target_Id)
        and then Nkind (N) = N_Function_Call
        and then Is_Default_Expression (N)
      then
         null;

      --  The target emulates Ada semantics

      elsif Is_Ada_Semantic_Target (Target_Id) then
         null;

      --  The target acts as a link between scenarios

      elsif Is_Bridge_Target (Target_Id) then
         null;

      --  The target emulates SPARK semantics

      elsif Is_SPARK_Semantic_Target (Target_Id) then
         null;

      --  Otherwise the call is not suitable for ABE processing. This prevents
      --  the generation of call markers which will never play a role in ABE
      --  diagnostics.

      else
         return;
      end if;

      --  At this point it is known that the call will play some role in ABE
      --  checks and diagnostics. Create a corresponding call marker in case
      --  the original call is heavily transformed by expansion later on.

      Marker := Make_Call_Marker (Sloc (N));

      --  Inherit the attributes of the original call

      Set_Target                    (Marker, Target_Id);
      Set_Is_Declaration_Level_Node (Marker, Call_Attrs.In_Declarations);
      Set_Is_Dispatching_Call       (Marker, Call_Attrs.Is_Dispatching);
      Set_Is_Elaboration_Checks_OK_Node
                                    (Marker, Call_Attrs.Elab_Checks_OK);
      Set_Is_Elaboration_Warnings_OK_Node
                                    (Marker, Call_Attrs.Elab_Warnings_OK);
      Set_Is_Ignored_Ghost_Node     (Marker, Call_Attrs.Ghost_Mode_Ignore);
      Set_Is_Source_Call            (Marker, Call_Attrs.From_Source);
      Set_Is_SPARK_Mode_On_Node     (Marker, Call_Attrs.SPARK_Mode_On);

      --  The marker is inserted prior to the original call. This placement has
      --  several desirable effects:

      --    1) The marker appears in the same context, in close proximity to
      --       the call.

      --         <marker>
      --         <call>

      --    2) Inserting the marker prior to the call ensures that an ABE check
      --       will take effect prior to the call.

      --         <ABE check>
      --         <marker>
      --         <call>

      --    3) The above two properties are preserved even when the call is a
      --       function which is subsequently relocated in order to capture its
      --       result. Note that if the call is relocated to a new context, the
      --       relocated call will receive a marker of its own.

      --         <ABE check>
      --         <maker>
      --         Temp : ... := Func_Call ...;
      --         ... Temp ...

      --  The insertion must take place even when the call does not occur in
      --  the main unit to keep the tree symmetric. This ensures that internal
      --  name serialization is consistent in case the call marker causes the
      --  tree to transform in some way.

      Insert_Action (N, Marker);

      --  The marker becomes the "corresponding" scenario for the call. Save
      --  the marker for later processing by the ABE phase.

      Record_Elaboration_Scenario (Marker);
   end Build_Call_Marker;

   -------------------------------------
   -- Build_Variable_Reference_Marker --
   -------------------------------------

   procedure Build_Variable_Reference_Marker
     (N     : Node_Id;
      Read  : Boolean;
      Write : Boolean)
   is
      function In_Pragma (Nod : Node_Id) return Boolean;
      --  Determine whether arbitrary node Nod appears within a pragma

      ---------------
      -- In_Pragma --
      ---------------

      function In_Pragma (Nod : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         Par := Nod;
         while Present (Par) loop
            if Nkind (Par) = N_Pragma then
               return True;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Pragma;

      --  Local variables

      Marker    : Node_Id;
      Prag      : Node_Id;
      Var_Attrs : Variable_Attributes;
      Var_Id    : Entity_Id;

   --  Start of processing for Build_Variable_Reference_Marker

   begin
      --  Nothing to do for ASIS. As a result, ABE checks and diagnostics are
      --  not performed in this mode.

      if ASIS_Mode then
         return;

      --  Nothing to do when the reference is being preanalyzed as the marker
      --  will be inserted in the wrong place.

      elsif Preanalysis_Active then
         return;

      --  Nothing to do when the input does not denote a reference

      elsif not Nkind_In (N, N_Expanded_Name, N_Identifier) then
         return;

      --  Nothing to do for internally-generated references

      elsif not Comes_From_Source (N) then
         return;

      --  Nothing to do when the reference is erroneous, left in a bad state,
      --  or does not denote a variable.

      elsif not (Present (Entity (N))
                  and then Ekind (Entity (N)) = E_Variable
                  and then Entity (N) /= Any_Id)
      then
         return;
      end if;

      Extract_Variable_Reference_Attributes
        (Ref    => N,
         Var_Id => Var_Id,
         Attrs  => Var_Attrs);

      Prag := SPARK_Pragma (Var_Id);

      if Comes_From_Source (Var_Id)

         --  Both the variable and the reference must appear in SPARK_Mode On
         --  regions because this scenario falls under the SPARK rules.

         and then Present (Prag)
         and then Get_SPARK_Mode_From_Annotation (Prag) = On
         and then Is_SPARK_Mode_On_Node (N)

         --  The reference must not be considered when it appears in a pragma.
         --  If the pragma has run-time semantics, then the reference will be
         --  reconsidered once the pragma is expanded.

         --  Performance note: parent traversal

         and then not In_Pragma (N)
      then
         null;

      --  Otherwise the reference is not suitable for ABE processing. This
      --  prevents the generation of variable markers which will never play
      --  a role in ABE diagnostics.

      else
         return;
      end if;

      --  At this point it is known that the variable reference will play some
      --  role in ABE checks and diagnostics. Create a corresponding variable
      --  marker in case the original variable reference is folded or optimized
      --  away.

      Marker := Make_Variable_Reference_Marker (Sloc (N));

      --  Inherit the attributes of the original variable reference

      Set_Target   (Marker, Var_Id);
      Set_Is_Read  (Marker, Read);
      Set_Is_Write (Marker, Write);

      --  The marker is inserted prior to the original variable reference. The
      --  insertion must take place even when the reference does not occur in
      --  the main unit to keep the tree symmetric. This ensures that internal
      --  name serialization is consistent in case the variable marker causes
      --  the tree to transform in some way.

      Insert_Action (N, Marker);

      --  The marker becomes the "corresponding" scenario for the reference.
      --  Save the marker for later processing for the ABE phase.

      Record_Elaboration_Scenario (Marker);
   end Build_Variable_Reference_Marker;

   ---------------------------------
   -- Check_Elaboration_Scenarios --
   ---------------------------------

   procedure Check_Elaboration_Scenarios is
   begin
      --  Nothing to do for ASIS. As a result, no ABE checks and diagnostics
      --  are performed in this mode.

      if ASIS_Mode then
         return;
      end if;

      --  Examine the context of the main unit and record all units with prior
      --  elaboration with respect to it.

      Find_Elaborated_Units;

      --  Examine each top-level scenario saved during the Recording phase for
      --  conditional ABEs and perform various actions depending on the model
      --  in effect. The table of visited bodies is created for each new top-
      --  level scenario.

      for Index in Top_Level_Scenarios.First .. Top_Level_Scenarios.Last loop
         Reset_Visited_Bodies;

         Process_Conditional_ABE (Top_Level_Scenarios.Table (Index));
      end loop;

      --  Examine each SPARK scenario saved during the Recording phase which
      --  is not necessarily executable during elaboration, but still requires
      --  elaboration-related checks.

      for Index in SPARK_Scenarios.First .. SPARK_Scenarios.Last loop
         Check_SPARK_Scenario (SPARK_Scenarios.Table (Index));
      end loop;
   end Check_Elaboration_Scenarios;

   ------------------------------
   -- Check_Preelaborated_Call --
   ------------------------------

   procedure Check_Preelaborated_Call (Call : Node_Id) is
      function In_Preelaborated_Context (N : Node_Id) return Boolean;
      --  Determine whether arbitrary node appears in a preelaborated context

      ------------------------------
      -- In_Preelaborated_Context --
      ------------------------------

      function In_Preelaborated_Context (N : Node_Id) return Boolean is
         Body_Id : constant Entity_Id := Find_Code_Unit (N);
         Spec_Id : constant Entity_Id := Unique_Entity (Body_Id);

      begin
         --  The node appears within a package body whose corresponding spec is
         --  subject to pragma Remote_Call_Interface or Remote_Types. This does
         --  not result in a preelaborated context because the package body may
         --  be on another machine.

         if Ekind (Body_Id) = E_Package_Body
           and then Ekind_In (Spec_Id, E_Generic_Package, E_Package)
           and then (Is_Remote_Call_Interface (Spec_Id)
                      or else Is_Remote_Types (Spec_Id))
         then
            return False;

         --  Otherwise the node appears within a preelaborated context when the
         --  associated unit is preelaborated.

         else
            return Is_Preelaborated_Unit (Spec_Id);
         end if;
      end In_Preelaborated_Context;

      --  Local variables

      Call_Attrs : Call_Attributes;
      Level      : Enclosing_Level_Kind;
      Target_Id  : Entity_Id;

   --  Start of processing for Check_Preelaborated_Call

   begin
      Extract_Call_Attributes
        (Call      => Call,
         Target_Id => Target_Id,
         Attrs     => Call_Attrs);

      --  Nothing to do when the call is internally generated because it is
      --  assumed that it will never violate preelaboration.

      if not Call_Attrs.From_Source then
         return;
      end if;

      --  Performance note: parent traversal

      Level := Find_Enclosing_Level (Call);

      --  Library-level calls are always considered because they are part of
      --  the associated unit's elaboration actions.

      if Level in Library_Level then
         null;

      --  Calls at the library level of a generic package body must be checked
      --  because they would render an instantiation illegal if the template is
      --  marked as preelaborated. Note that this does not apply to calls at
      --  the library level of a generic package spec.

      elsif Level = Generic_Package_Body then
         null;

      --  Otherwise the call does not appear at the proper level and must not
      --  be considered for this check.

      else
         return;
      end if;

      --  The call appears within a preelaborated unit. Emit a warning only for
      --  internal uses, otherwise this is an error.

      if In_Preelaborated_Context (Call) then
         Error_Msg_Warn := GNAT_Mode;
         Error_Msg_N
           ("<<non-static call not allowed in preelaborated unit", Call);
      end if;
   end Check_Preelaborated_Call;

   ------------------------------
   -- Check_SPARK_Derived_Type --
   ------------------------------

   procedure Check_SPARK_Derived_Type (Typ_Decl : Node_Id) is
      Typ : constant Entity_Id := Defining_Entity (Typ_Decl);

      --  NOTE: The routines within Check_SPARK_Derived_Type are intentionally
      --  unnested to avoid deep indentation of code.

      Stop_Check : exception;
      --  This exception is raised when the freeze node violates the placement
      --  rules.

      procedure Check_Overriding_Primitive
        (Prim  : Entity_Id;
         FNode : Node_Id);
      pragma Inline (Check_Overriding_Primitive);
      --  Verify that freeze node FNode is within the early call region of
      --  overriding primitive Prim's body.

      function Freeze_Node_Location (FNode : Node_Id) return Source_Ptr;
      pragma Inline (Freeze_Node_Location);
      --  Return a more accurate source location associated with freeze node
      --  FNode.

      function Precedes_Source_Construct (N : Node_Id) return Boolean;
      pragma Inline (Precedes_Source_Construct);
      --  Determine whether arbitrary node N appears prior to some source
      --  construct.

      procedure Suggest_Elaborate_Body
        (N         : Node_Id;
         Body_Decl : Node_Id;
         Error_Nod : Node_Id);
      pragma Inline (Suggest_Elaborate_Body);
      --  Suggest the use of pragma Elaborate_Body when the pragma will allow
      --  for node N to appear within the early call region of subprogram body
      --  Body_Decl. The suggestion is attached to Error_Nod as a continuation
      --  error.

      --------------------------------
      -- Check_Overriding_Primitive --
      --------------------------------

      procedure Check_Overriding_Primitive
        (Prim  : Entity_Id;
         FNode : Node_Id)
      is
         Prim_Decl : constant Node_Id := Unit_Declaration_Node (Prim);
         Body_Decl : Node_Id;
         Body_Id   : Entity_Id;
         Region    : Node_Id;

      begin
         Body_Id := Corresponding_Body (Prim_Decl);

         --  Nothing to do when the primitive does not have a corresponding
         --  body. This can happen when the unit with the bodies is not the
         --  main unit subjected to ABE checks.

         if No (Body_Id) then
            return;

         --  The primitive overrides a parent or progenitor primitive

         elsif Present (Overridden_Operation (Prim)) then

            --  Nothing to do when overriding an interface primitive happens by
            --  inheriting a non-interface primitive as the check would be done
            --  on the parent primitive.

            if Present (Alias (Prim)) then
               return;
            end if;

         --  Nothing to do when the primitive is not overriding. The body of
         --  such a primitive cannot be targeted by a dispatching call which
         --  is executable during elaboration, and cannot cause an ABE.

         else
            return;
         end if;

         Body_Decl := Unit_Declaration_Node (Body_Id);
         Region    := Find_Early_Call_Region (Body_Decl);

         --  The freeze node appears prior to the early call region of the
         --  primitive body.

         --  IMPORTANT: This check must always be performed even when -gnatd.v
         --  (enforce SPARK elaboration rules in SPARK code) is not specified
         --  because the static model cannot guarantee the absence of ABEs in
         --  in the presence of dispatching calls.

         if Earlier_In_Extended_Unit (FNode, Region) then
            Error_Msg_Node_2 := Prim;
            Error_Msg_NE
              ("first freezing point of type & must appear within early call "
               & "region of primitive body & (SPARK RM 7.7(8))",
               Typ_Decl, Typ);

            Error_Msg_Sloc := Sloc (Region);
            Error_Msg_N ("\region starts #", Typ_Decl);

            Error_Msg_Sloc := Sloc (Body_Decl);
            Error_Msg_N ("\region ends #", Typ_Decl);

            Error_Msg_Sloc := Freeze_Node_Location (FNode);
            Error_Msg_N ("\first freezing point #", Typ_Decl);

            --  If applicable, suggest the use of pragma Elaborate_Body in the
            --  associated package spec.

            Suggest_Elaborate_Body
              (N         => FNode,
               Body_Decl => Body_Decl,
               Error_Nod => Typ_Decl);

            raise Stop_Check;
         end if;
      end Check_Overriding_Primitive;

      --------------------------
      -- Freeze_Node_Location --
      --------------------------

      function Freeze_Node_Location (FNode : Node_Id) return Source_Ptr is
         Context : constant Node_Id    := Parent (FNode);
         Loc     : constant Source_Ptr := Sloc (FNode);

         Prv_Decls : List_Id;
         Vis_Decls : List_Id;

      begin
         --  In general, the source location of the freeze node is as close as
         --  possible to the real freeze point, except when the freeze node is
         --  at the "bottom" of a package spec.

         if Nkind (Context) = N_Package_Specification then
            Prv_Decls := Private_Declarations (Context);
            Vis_Decls := Visible_Declarations (Context);

            --  The freeze node appears in the private declarations of the
            --  package.

            if Present (Prv_Decls)
              and then List_Containing (FNode) = Prv_Decls
            then
               null;

            --  The freeze node appears in the visible declarations of the
            --  package and there are no private declarations.

            elsif Present (Vis_Decls)
              and then List_Containing (FNode) = Vis_Decls
              and then (No (Prv_Decls) or else Is_Empty_List (Prv_Decls))
            then
               null;

            --  Otherwise the freeze node is not in the "last" declarative list
            --  of the package. Use the existing source location of the freeze
            --  node.

            else
               return Loc;
            end if;

            --  The freeze node appears at the "bottom" of the package when it
            --  is in the "last" declarative list and is either the last in the
            --  list or is followed by internal constructs only. In that case
            --  the more appropriate source location is that of the package end
            --  label.

            if not Precedes_Source_Construct (FNode) then
               return Sloc (End_Label (Context));
            end if;
         end if;

         return Loc;
      end Freeze_Node_Location;

      -------------------------------
      -- Precedes_Source_Construct --
      -------------------------------

      function Precedes_Source_Construct (N : Node_Id) return Boolean is
         Decl : Node_Id;

      begin
         Decl := Next (N);
         while Present (Decl) loop
            if Comes_From_Source (Decl) then
               return True;

            --  A generated body for a source expression function is treated as
            --  a source construct.

            elsif Nkind (Decl) = N_Subprogram_Body
              and then Was_Expression_Function (Decl)
              and then Comes_From_Source (Original_Node (Decl))
            then
               return True;
            end if;

            Next (Decl);
         end loop;

         return False;
      end Precedes_Source_Construct;

      ----------------------------
      -- Suggest_Elaborate_Body --
      ----------------------------

      procedure Suggest_Elaborate_Body
        (N         : Node_Id;
         Body_Decl : Node_Id;
         Error_Nod : Node_Id)
      is
         Unt    : constant Node_Id := Unit (Cunit (Main_Unit));
         Region : Node_Id;

      begin
         --  The suggestion applies only when the subprogram body resides in a
         --  compilation package body, and a pragma Elaborate_Body would allow
         --  for the node to appear in the early call region of the subprogram
         --  body. This implies that all code from the subprogram body up to
         --  the node is preelaborable.

         if Nkind (Unt) = N_Package_Body then

            --  Find the start of the early call region again assuming that the
            --  package spec has pragma Elaborate_Body. Note that the internal
            --  data structures are intentionally not updated because this is a
            --  speculative search.

            Region :=
              Find_Early_Call_Region
                (Body_Decl        => Body_Decl,
                 Assume_Elab_Body => True,
                 Skip_Memoization => True);

            --  If the node appears within the early call region, assuming that
            --  the package spec carries pragma Elaborate_Body, then it is safe
            --  to suggest the pragma.

            if Earlier_In_Extended_Unit (Region, N) then
               Error_Msg_Name_1 := Name_Elaborate_Body;
               Error_Msg_NE
                 ("\consider adding pragma % in spec of unit &",
                  Error_Nod, Defining_Entity (Unt));
            end if;
         end if;
      end Suggest_Elaborate_Body;

      --  Local variables

      FNode : constant Node_Id  := Freeze_Node (Typ);
      Prims : constant Elist_Id := Direct_Primitive_Operations (Typ);

      Prim_Elmt : Elmt_Id;

   --  Start of processing for Check_SPARK_Derived_Type

   begin
      --  A type should have its freeze node set by the time SPARK scenarios
      --  are being verified.

      pragma Assert (Present (FNode));

      --  Verify that the freeze node of the derived type is within the early
      --  call region of each overriding primitive body (SPARK RM 7.7(8)).

      if Present (Prims) then
         Prim_Elmt := First_Elmt (Prims);
         while Present (Prim_Elmt) loop
            Check_Overriding_Primitive
              (Prim  => Node (Prim_Elmt),
               FNode => FNode);

            Next_Elmt (Prim_Elmt);
         end loop;
      end if;

   exception
      when Stop_Check =>
         null;
   end Check_SPARK_Derived_Type;

   -------------------------------
   -- Check_SPARK_Instantiation --
   -------------------------------

   procedure Check_SPARK_Instantiation (Exp_Inst : Node_Id) is
      Gen_Attrs  : Target_Attributes;
      Gen_Id     : Entity_Id;
      Inst       : Node_Id;
      Inst_Attrs : Instantiation_Attributes;
      Inst_Id    : Entity_Id;

   begin
      Extract_Instantiation_Attributes
        (Exp_Inst => Exp_Inst,
         Inst     => Inst,
         Inst_Id  => Inst_Id,
         Gen_Id   => Gen_Id,
         Attrs    => Inst_Attrs);

      Extract_Target_Attributes (Gen_Id, Gen_Attrs);

      --  The instantiation and the generic body are both in the main unit

      if Present (Gen_Attrs.Body_Decl)
        and then In_Extended_Main_Code_Unit (Gen_Attrs.Body_Decl)

        --  If the instantiation appears prior to the generic body, then the
        --  instantiation is illegal (SPARK RM 7.7(6)).

        --  IMPORTANT: This check must always be performed even when -gnatd.v
        --  (enforce SPARK elaboration rules in SPARK code) is not specified
        --  because the rule prevents use-before-declaration of objects that
        --  may precede the generic body.

        and then Earlier_In_Extended_Unit (Inst, Gen_Attrs.Body_Decl)
      then
         Error_Msg_NE ("cannot instantiate & before body seen", Inst, Gen_Id);
      end if;
   end Check_SPARK_Instantiation;

   --------------------------
   -- Check_SPARK_Scenario --
   --------------------------

   procedure Check_SPARK_Scenario (N : Node_Id) is
   begin
      --  Add the current scenario to the stack of active scenarios

      Push_Active_Scenario (N);

      if Is_Suitable_SPARK_Derived_Type (N) then
         Check_SPARK_Derived_Type (N);

      elsif Is_Suitable_SPARK_Instantiation (N) then
         Check_SPARK_Instantiation (N);

      elsif Is_Suitable_SPARK_Refined_State_Pragma (N) then
         Check_SPARK_Refined_State_Pragma (N);
      end if;

      --  Remove the current scenario from the stack of active scenarios once
      --  all ABE diagnostics and checks have been performed.

      Pop_Active_Scenario (N);
   end Check_SPARK_Scenario;

   --------------------------------------
   -- Check_SPARK_Refined_State_Pragma --
   --------------------------------------

   procedure Check_SPARK_Refined_State_Pragma (N : Node_Id) is

      --  NOTE: The routines within Check_SPARK_Refined_State_Pragma are
      --  intentionally unnested to avoid deep indentation of code.

      procedure Check_SPARK_Constituent (Constit_Id : Entity_Id);
      pragma Inline (Check_SPARK_Constituent);
      --  Ensure that a single constituent Constit_Id is elaborated prior to
      --  the main unit.

      procedure Check_SPARK_Constituents (Constits : Elist_Id);
      pragma Inline (Check_SPARK_Constituents);
      --  Ensure that all constituents found in list Constits are elaborated
      --  prior to the main unit.

      procedure Check_SPARK_Initialized_State (State : Node_Id);
      pragma Inline (Check_SPARK_Initialized_State);
      --  Ensure that the constituents of single abstract state State are
      --  elaborated prior to the main unit.

      procedure Check_SPARK_Initialized_States (Pack_Id : Entity_Id);
      pragma Inline (Check_SPARK_Initialized_States);
      --  Ensure that the constituents of all abstract states which appear in
      --  the Initializes pragma of package Pack_Id are elaborated prior to the
      --  main unit.

      -----------------------------
      -- Check_SPARK_Constituent --
      -----------------------------

      procedure Check_SPARK_Constituent (Constit_Id : Entity_Id) is
         Prag : Node_Id;

      begin
         --  Nothing to do for "null" constituents

         if Nkind (Constit_Id) = N_Null then
            return;

         --  Nothing to do for illegal constituents

         elsif Error_Posted (Constit_Id) then
            return;
         end if;

         Prag := SPARK_Pragma (Constit_Id);

         --  The check applies only when the constituent is subject to pragma
         --  SPARK_Mode On.

         if Present (Prag)
           and then Get_SPARK_Mode_From_Annotation (Prag) = On
         then
            --  An external constituent of an abstract state which appears in
            --  the Initializes pragma of a package spec imposes an Elaborate
            --  requirement on the context of the main unit. Determine whether
            --  the context has a pragma strong enough to meet the requirement.

            --  IMPORTANT: This check is performed only when -gnatd.v (enforce
            --  SPARK elaboration rules in SPARK code) is in effect because the
            --  static model can ensure the prior elaboration of the unit which
            --  contains a constituent by installing implicit Elaborate pragma.

            if Debug_Flag_Dot_V then
               Meet_Elaboration_Requirement
                 (N         => N,
                  Target_Id => Constit_Id,
                  Req_Nam   => Name_Elaborate);

            --  Otherwise ensure that the unit with the external constituent is
            --  elaborated prior to the main unit.

            else
               Ensure_Prior_Elaboration
                 (N              => N,
                  Unit_Id        => Find_Top_Unit (Constit_Id),
                  Prag_Nam       => Name_Elaborate,
                  In_Partial_Fin => False,
                  In_Task_Body   => False);
            end if;
         end if;
      end Check_SPARK_Constituent;

      ------------------------------
      -- Check_SPARK_Constituents --
      ------------------------------

      procedure Check_SPARK_Constituents (Constits : Elist_Id) is
         Constit_Elmt : Elmt_Id;

      begin
         if Present (Constits) then
            Constit_Elmt := First_Elmt (Constits);
            while Present (Constit_Elmt) loop
               Check_SPARK_Constituent (Node (Constit_Elmt));
               Next_Elmt (Constit_Elmt);
            end loop;
         end if;
      end Check_SPARK_Constituents;

      -----------------------------------
      -- Check_SPARK_Initialized_State --
      -----------------------------------

      procedure Check_SPARK_Initialized_State (State : Node_Id) is
         Prag     : Node_Id;
         State_Id : Entity_Id;

      begin
         --  Nothing to do for "null" initialization items

         if Nkind (State) = N_Null then
            return;

         --  Nothing to do for illegal states

         elsif Error_Posted (State) then
            return;
         end if;

         State_Id := Entity_Of (State);

         --  Sanitize the state

         if No (State_Id) then
            return;

         elsif Error_Posted (State_Id) then
            return;

         elsif Ekind (State_Id) /= E_Abstract_State then
            return;
         end if;

         --  The check is performed only when the abstract state is subject to
         --  SPARK_Mode On.

         Prag := SPARK_Pragma (State_Id);

         if Present (Prag)
           and then Get_SPARK_Mode_From_Annotation (Prag) = On
         then
            Check_SPARK_Constituents (Refinement_Constituents (State_Id));
         end if;
      end Check_SPARK_Initialized_State;

      ------------------------------------
      -- Check_SPARK_Initialized_States --
      ------------------------------------

      procedure Check_SPARK_Initialized_States (Pack_Id : Entity_Id) is
         Prag  : constant Node_Id := Get_Pragma (Pack_Id, Pragma_Initializes);
         Init  : Node_Id;
         Inits : Node_Id;

      begin
         if Present (Prag) then
            Inits := Expression (Get_Argument (Prag, Pack_Id));

            --  Avoid processing a "null" initialization list. The only other
            --  alternative is an aggregate.

            if Nkind (Inits) = N_Aggregate then

               --  The initialization items appear in list form:
               --
               --    (state1, state2)

               if Present (Expressions (Inits)) then
                  Init := First (Expressions (Inits));
                  while Present (Init) loop
                     Check_SPARK_Initialized_State (Init);
                     Next (Init);
                  end loop;
               end if;

               --  The initialization items appear in associated form:
               --
               --    (state1 => item1,
               --     state2 => (item2, item3))

               if Present (Component_Associations (Inits)) then
                  Init := First (Component_Associations (Inits));
                  while Present (Init) loop
                     Check_SPARK_Initialized_State (Init);
                     Next (Init);
                  end loop;
               end if;
            end if;
         end if;
      end Check_SPARK_Initialized_States;

      --  Local variables

      Pack_Body : constant Node_Id := Find_Related_Package_Or_Body (N);

   --  Start of processing for Check_SPARK_Refined_State_Pragma

   begin
      --  Pragma Refined_State must be associated with a package body

      pragma Assert
        (Present (Pack_Body) and then Nkind (Pack_Body) = N_Package_Body);

      --  Verify that each external contitunent of an abstract state mentioned
      --  in pragma Initializes is properly elaborated.

      Check_SPARK_Initialized_States (Unique_Defining_Entity (Pack_Body));
   end Check_SPARK_Refined_State_Pragma;

   ----------------------
   -- Compilation_Unit --
   ----------------------

   function Compilation_Unit (Unit_Id : Entity_Id) return Node_Id is
      Comp_Unit : Node_Id;

   begin
      Comp_Unit := Parent (Unit_Id);

      --  Handle the case where a concurrent subunit is rewritten as a null
      --  statement due to expansion activities.

      if Nkind (Comp_Unit) = N_Null_Statement
        and then Nkind_In (Original_Node (Comp_Unit), N_Protected_Body,
                                                      N_Task_Body)
      then
         Comp_Unit := Parent (Comp_Unit);
         pragma Assert (Nkind (Comp_Unit) = N_Subunit);

      --  Otherwise use the declaration node of the unit

      else
         Comp_Unit := Parent (Unit_Declaration_Node (Unit_Id));
      end if;

      --  Handle the case where a subprogram instantiation which acts as a
      --  compilation unit is expanded into an anonymous package that wraps
      --  the instantiated subprogram.

      if Nkind (Comp_Unit) = N_Package_Specification
        and then Nkind_In (Original_Node (Parent (Comp_Unit)),
                           N_Function_Instantiation,
                           N_Procedure_Instantiation)
      then
         Comp_Unit := Parent (Parent (Comp_Unit));

      --  Handle the case where the compilation unit is a subunit

      elsif Nkind (Comp_Unit) = N_Subunit then
         Comp_Unit := Parent (Comp_Unit);
      end if;

      pragma Assert (Nkind (Comp_Unit) = N_Compilation_Unit);

      return Comp_Unit;
   end Compilation_Unit;

   -----------------------
   -- Early_Call_Region --
   -----------------------

   function Early_Call_Region (Body_Id : Entity_Id) return Node_Id is
   begin
      pragma Assert (Ekind_In (Body_Id, E_Entry,
                                        E_Entry_Family,
                                        E_Function,
                                        E_Procedure,
                                        E_Subprogram_Body));

      if Early_Call_Regions_In_Use then
         return Early_Call_Regions.Get (Body_Id);
      end if;

      return Early_Call_Regions_No_Element;
   end Early_Call_Region;

   -----------------------------
   -- Early_Call_Regions_Hash --
   -----------------------------

   function Early_Call_Regions_Hash
     (Key : Entity_Id) return Early_Call_Regions_Index
   is
   begin
      return Early_Call_Regions_Index (Key mod Early_Call_Regions_Max);
   end Early_Call_Regions_Hash;

   -----------------
   -- Elab_Msg_NE --
   -----------------

   procedure Elab_Msg_NE
     (Msg      : String;
      N        : Node_Id;
      Id       : Entity_Id;
      Info_Msg : Boolean;
      In_SPARK : Boolean)
   is
      function Prefix return String;
      --  Obtain the prefix of the message

      function Suffix return String;
      --  Obtain the suffix of the message

      ------------
      -- Prefix --
      ------------

      function Prefix return String is
      begin
         if Info_Msg then
            return "info: ";
         else
            return "";
         end if;
      end Prefix;

      ------------
      -- Suffix --
      ------------

      function Suffix return String is
      begin
         if In_SPARK then
            return " in SPARK";
         else
            return "";
         end if;
      end Suffix;

   --  Start of processing for Elab_Msg_NE

   begin
      Error_Msg_NE (Prefix & Msg & Suffix, N, Id);
   end Elab_Msg_NE;

   ------------------------
   -- Elaboration_Status --
   ------------------------

   function Elaboration_Status
     (Unit_Id : Entity_Id) return Elaboration_Attributes
   is
   begin
      if Elaboration_Statuses_In_Use then
         return Elaboration_Statuses.Get (Unit_Id);
      end if;

      return Elaboration_Statuses_No_Element;
   end Elaboration_Status;

   -------------------------------
   -- Elaboration_Statuses_Hash --
   -------------------------------

   function Elaboration_Statuses_Hash
     (Key : Entity_Id) return Elaboration_Statuses_Index
   is
   begin
      return Elaboration_Statuses_Index (Key mod Elaboration_Statuses_Max);
   end Elaboration_Statuses_Hash;

   ------------------------------
   -- Ensure_Prior_Elaboration --
   ------------------------------

   procedure Ensure_Prior_Elaboration
     (N              : Node_Id;
      Unit_Id        : Entity_Id;
      Prag_Nam       : Name_Id;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
   begin
      pragma Assert (Nam_In (Prag_Nam, Name_Elaborate, Name_Elaborate_All));

      --  Nothing to do when the need for prior elaboration came from a partial
      --  finalization routine which occurs in an initialization context. This
      --  behaviour parallels that of the old ABE mechanism.

      if In_Partial_Fin then
         return;

      --  Nothing to do when the need for prior elaboration came from a task
      --  body and switch -gnatd.y (disable implicit pragma Elaborate_All on
      --  task bodies) is in effect.

      elsif Debug_Flag_Dot_Y and then In_Task_Body then
         return;

      --  Nothing to do when the unit is elaborated prior to the main unit.
      --  This check must also consider the following cases:

      --  * No check is made against the context of the main unit because this
      --    is specific to the elaboration model in effect and requires custom
      --    handling (see Ensure_xxx_Prior_Elaboration).

      --  * Unit_Id is subject to pragma Elaborate_Body. An implicit pragma
      --    Elaborate[_All] MUST be generated even though Unit_Id is always
      --    elaborated prior to the main unit. This is a conservative strategy
      --    which ensures that other units withed by Unit_Id will not lead to
      --    an ABE.

      --      package A is               package body A is
      --         procedure ABE;             procedure ABE is ... end ABE;
      --      end A;                     end A;

      --      with A;
      --      package B is               package body B is
      --         pragma Elaborate_Body;     procedure Proc is
      --                                    begin
      --         procedure Proc;               A.ABE;
      --      package B;                    end Proc;
      --                                 end B;

      --      with B;
      --      package C is               package body C is
      --         ...                        ...
      --      end C;                     begin
      --                                    B.Proc;
      --                                 end C;

      --    In the example above, the elaboration of C invokes B.Proc. B is
      --    subject to pragma Elaborate_Body. If no pragma Elaborate[_All] is
      --    generated for B in C, then the following elaboratio order will lead
      --    to an ABE:

      --       spec of A elaborated
      --       spec of B elaborated
      --       body of B elaborated
      --       spec of C elaborated
      --       body of C elaborated  <--  calls B.Proc which calls A.ABE
      --       body of A elaborated  <--  problem

      --    The generation of an implicit pragma Elaborate_All (B) ensures that
      --    the elaboration order mechanism will not pick the above order.

      --    An implicit Elaborate is NOT generated when the unit is subject to
      --    Elaborate_Body because both pragmas have the exact same effect.

      --  * Unit_Id is the main unit. An implicit pragma Elaborate[_All] MUST
      --    NOT be generated in this case because a unit cannot depend on its
      --    own elaboration. This case is therefore treated as valid prior
      --    elaboration.

      elsif Has_Prior_Elaboration
              (Unit_Id      => Unit_Id,
               Same_Unit_OK => True,
               Elab_Body_OK => Prag_Nam = Name_Elaborate)
      then
         return;

      --  Suggest the use of pragma Prag_Nam when the dynamic model is in
      --  effect.

      elsif Dynamic_Elaboration_Checks then
         Ensure_Prior_Elaboration_Dynamic
           (N        => N,
            Unit_Id  => Unit_Id,
            Prag_Nam => Prag_Nam);

      --  Install an implicit pragma Prag_Nam when the static model is in
      --  effect.

      else
         pragma Assert (Static_Elaboration_Checks);

         Ensure_Prior_Elaboration_Static
           (N        => N,
            Unit_Id  => Unit_Id,
            Prag_Nam => Prag_Nam);
      end if;
   end Ensure_Prior_Elaboration;

   --------------------------------------
   -- Ensure_Prior_Elaboration_Dynamic --
   --------------------------------------

   procedure Ensure_Prior_Elaboration_Dynamic
     (N        : Node_Id;
      Unit_Id  : Entity_Id;
      Prag_Nam : Name_Id)
   is
      procedure Info_Missing_Pragma;
      pragma Inline (Info_Missing_Pragma);
      --  Output information concerning missing Elaborate or Elaborate_All
      --  pragma with name Prag_Nam for scenario N, which would ensure the
      --  prior elaboration of Unit_Id.

      -------------------------
      -- Info_Missing_Pragma --
      -------------------------

      procedure Info_Missing_Pragma is
      begin
         --  Internal units are ignored as they cause unnecessary noise

         if not In_Internal_Unit (Unit_Id) then

            --  The name of the unit subjected to the elaboration pragma is
            --  fully qualified to improve the clarity of the info message.

            Error_Msg_Name_1     := Prag_Nam;
            Error_Msg_Qual_Level := Nat'Last;

            Error_Msg_NE ("info: missing pragma % for unit &", N, Unit_Id);
            Error_Msg_Qual_Level := 0;
         end if;
      end Info_Missing_Pragma;

      --  Local variables

      Elab_Attrs : Elaboration_Attributes;
      Level      : Enclosing_Level_Kind;

   --  Start of processing for Ensure_Prior_Elaboration_Dynamic

   begin
      Elab_Attrs := Elaboration_Status (Unit_Id);

      --  Nothing to do when the unit is guaranteed prior elaboration by means
      --  of a source Elaborate[_All] pragma.

      if Present (Elab_Attrs.Source_Pragma) then
         return;
      end if;

      --  Output extra information on a missing Elaborate[_All] pragma when
      --  switch -gnatel (info messages on implicit Elaborate[_All] pragmas
      --  is in effect.

      if Elab_Info_Messages then

         --  Performance note: parent traversal

         Level := Find_Enclosing_Level (N);

         --  Declaration-level scenario

         if (Is_Suitable_Call (N) or else Is_Suitable_Instantiation (N))
           and then Level = Declaration_Level
         then
            null;

         --  Library-level scenario

         elsif Level in Library_Level then
            null;

         --  Instantiation library-level scenario

         elsif Level = Instantiation then
            null;

         --  Otherwise the scenario does not appear at the proper level and
         --  cannot possibly act as a top-level scenario.

         else
            return;
         end if;

         Info_Missing_Pragma;
      end if;
   end Ensure_Prior_Elaboration_Dynamic;

   -------------------------------------
   -- Ensure_Prior_Elaboration_Static --
   -------------------------------------

   procedure Ensure_Prior_Elaboration_Static
     (N        : Node_Id;
      Unit_Id  : Entity_Id;
      Prag_Nam : Name_Id)
   is
      function Find_With_Clause
        (Items     : List_Id;
         Withed_Id : Entity_Id) return Node_Id;
      pragma Inline (Find_With_Clause);
      --  Find a nonlimited with clause in the list of context items Items
      --  that withs unit Withed_Id. Return Empty if no such clause is found.

      procedure Info_Implicit_Pragma;
      pragma Inline (Info_Implicit_Pragma);
      --  Output information concerning an implicitly generated Elaborate or
      --  Elaborate_All pragma with name Prag_Nam for scenario N which ensures
      --  the prior elaboration of unit Unit_Id.

      ----------------------
      -- Find_With_Clause --
      ----------------------

      function Find_With_Clause
        (Items     : List_Id;
         Withed_Id : Entity_Id) return Node_Id
      is
         Item : Node_Id;

      begin
         --  Examine the context clauses looking for a suitable with. Note that
         --  limited clauses do not affect the elaboration order.

         Item := First (Items);
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then not Error_Posted (Item)
              and then not Limited_Present (Item)
              and then Entity (Name (Item)) = Withed_Id
            then
               return Item;
            end if;

            Next (Item);
         end loop;

         return Empty;
      end Find_With_Clause;

      --------------------------
      -- Info_Implicit_Pragma --
      --------------------------

      procedure Info_Implicit_Pragma is
      begin
         --  Internal units are ignored as they cause unnecessary noise

         if not In_Internal_Unit (Unit_Id) then

            --  The name of the unit subjected to the elaboration pragma is
            --  fully qualified to improve the clarity of the info message.

            Error_Msg_Name_1     := Prag_Nam;
            Error_Msg_Qual_Level := Nat'Last;

            Error_Msg_NE
              ("info: implicit pragma % generated for unit &", N, Unit_Id);

            Error_Msg_Qual_Level := 0;
            Output_Active_Scenarios (N);
         end if;
      end Info_Implicit_Pragma;

      --  Local variables

      Main_Cunit : constant Node_Id    := Cunit (Main_Unit);
      Loc        : constant Source_Ptr := Sloc (Main_Cunit);
      Unit_Cunit : constant Node_Id    := Compilation_Unit (Unit_Id);

      Is_Instantiation : constant Boolean :=
                           Nkind (N) in N_Generic_Instantiation;

      Clause     : Node_Id;
      Elab_Attrs : Elaboration_Attributes;
      Items      : List_Id;

   --  Start of processing for Ensure_Prior_Elaboration_Static

   begin
      Elab_Attrs := Elaboration_Status (Unit_Id);

      --  Nothing to do when the unit is guaranteed prior elaboration by means
      --  of a source Elaborate[_All] pragma.

      if Present (Elab_Attrs.Source_Pragma) then
         return;

      --  Nothing to do when the unit has an existing implicit Elaborate[_All]
      --  pragma installed by a previous scenario.

      elsif Present (Elab_Attrs.With_Clause) then

         --  The unit is already guaranteed prior elaboration by means of an
         --  implicit Elaborate pragma, however the current scenario imposes
         --  a stronger requirement of Elaborate_All. "Upgrade" the existing
         --  pragma to match this new requirement.

         if Elaborate_Desirable (Elab_Attrs.With_Clause)
           and then Prag_Nam = Name_Elaborate_All
         then
            Set_Elaborate_All_Desirable (Elab_Attrs.With_Clause);
            Set_Elaborate_Desirable     (Elab_Attrs.With_Clause, False);
         end if;

         return;
      end if;

      --  At this point it is known that the unit has no prior elaboration
      --  according to pragmas and hierarchical relationships.

      Items := Context_Items (Main_Cunit);

      if No (Items) then
         Items := New_List;
         Set_Context_Items (Main_Cunit, Items);
      end if;

      --  Locate the with clause for the unit. Note that there may not be a
      --  clause if the unit is visible through a subunit-body, body-spec, or
      --  spec-parent relationship.

      Clause :=
        Find_With_Clause
          (Items     => Items,
           Withed_Id => Unit_Id);

      --  Generate:
      --    with Id;

      --  Note that adding implicit with clauses is safe because analysis,
      --  resolution, and expansion have already taken place and it is not
      --  possible to interfere with visibility.

      if No (Clause) then
         Clause :=
           Make_With_Clause (Loc,
             Name => New_Occurrence_Of (Unit_Id, Loc));

         Set_Implicit_With (Clause);
         Set_Library_Unit  (Clause, Unit_Cunit);

         Append_To (Items, Clause);
      end if;

      --  Instantiations require an implicit Elaborate because Elaborate_All is
      --  too conservative and may introduce non-existent elaboration cycles.

      if Is_Instantiation then
         Set_Elaborate_Desirable (Clause);

      --  Otherwise generate an implicit Elaborate_All

      else
         Set_Elaborate_All_Desirable (Clause);
      end if;

      --  The implicit Elaborate[_All] ensures the prior elaboration of the
      --  unit. Include the unit in the elaboration context of the main unit.

      Set_Elaboration_Status
        (Unit_Id => Unit_Id,
         Val     => Elaboration_Attributes'(Source_Pragma => Empty,
                                            With_Clause   => Clause));

      --  Output extra information on an implicit Elaborate[_All] pragma when
      --  switch -gnatel (info messages on implicit Elaborate[_All] pragmas is
      --  in effect.

      if Elab_Info_Messages then
         Info_Implicit_Pragma;
      end if;
   end Ensure_Prior_Elaboration_Static;

   -----------------------------
   -- Extract_Assignment_Name --
   -----------------------------

   function Extract_Assignment_Name (Asmt : Node_Id) return Node_Id is
      Nam : Node_Id;

   begin
      Nam := Name (Asmt);

      --  When the name denotes an array or record component, find the whole
      --  object.

      while Nkind_In (Nam, N_Explicit_Dereference,
                           N_Indexed_Component,
                           N_Selected_Component,
                           N_Slice)
      loop
         Nam := Prefix (Nam);
      end loop;

      return Nam;
   end Extract_Assignment_Name;

   -----------------------------
   -- Extract_Call_Attributes --
   -----------------------------

   procedure Extract_Call_Attributes
     (Call      : Node_Id;
      Target_Id : out Entity_Id;
      Attrs     : out Call_Attributes)
   is
      From_Source     : Boolean;
      In_Declarations : Boolean;
      Is_Dispatching  : Boolean;

   begin
      --  Extraction for call markers

      if Nkind (Call) = N_Call_Marker then
         Target_Id       := Target (Call);
         From_Source     := Is_Source_Call (Call);
         In_Declarations := Is_Declaration_Level_Node (Call);
         Is_Dispatching  := Is_Dispatching_Call (Call);

      --  Extraction for entry calls, requeue, and subprogram calls

      else
         pragma Assert (Nkind_In (Call, N_Entry_Call_Statement,
                                        N_Function_Call,
                                        N_Procedure_Call_Statement,
                                        N_Requeue_Statement));

         Target_Id   := Entity (Extract_Call_Name (Call));
         From_Source := Comes_From_Source (Call);

         --  Performance note: parent traversal

         In_Declarations := Find_Enclosing_Level (Call) = Declaration_Level;
         Is_Dispatching  :=
           Nkind_In (Call, N_Function_Call, N_Procedure_Call_Statement)
             and then Present (Controlling_Argument (Call));
      end if;

      --  Obtain the original entry or subprogram which the target may rename
      --  except when the target is an instantiation. In this case the alias
      --  is the internally generated subprogram which appears within the the
      --  anonymous package created for the instantiation. Such an alias is not
      --  a suitable target.

      if not (Is_Subprogram (Target_Id)
               and then Is_Generic_Instance (Target_Id))
      then
         Target_Id := Get_Renamed_Entity (Target_Id);
      end if;

      --  Set all attributes

      Attrs.Elab_Checks_OK    := Is_Elaboration_Checks_OK_Node (Call);
      Attrs.Elab_Warnings_OK  := Is_Elaboration_Warnings_OK_Node (Call);
      Attrs.From_Source       := From_Source;
      Attrs.Ghost_Mode_Ignore := Is_Ignored_Ghost_Node (Call);
      Attrs.In_Declarations   := In_Declarations;
      Attrs.Is_Dispatching    := Is_Dispatching;
      Attrs.SPARK_Mode_On     := Is_SPARK_Mode_On_Node (Call);
   end Extract_Call_Attributes;

   -----------------------
   -- Extract_Call_Name --
   -----------------------

   function Extract_Call_Name (Call : Node_Id) return Node_Id is
      Nam : Node_Id;

   begin
      Nam := Name (Call);

      --  When the call invokes an entry family, the name appears as an indexed
      --  component.

      if Nkind (Nam) = N_Indexed_Component then
         Nam := Prefix (Nam);
      end if;

      --  When the call employs the object.operation form, the name appears as
      --  a selected component.

      if Nkind (Nam) = N_Selected_Component then
         Nam := Selector_Name (Nam);
      end if;

      return Nam;
   end Extract_Call_Name;

   ---------------------------------
   -- Extract_Instance_Attributes --
   ---------------------------------

   procedure Extract_Instance_Attributes
     (Exp_Inst  : Node_Id;
      Inst_Body : out Node_Id;
      Inst_Decl : out Node_Id)
   is
      Body_Id : Entity_Id;

   begin
      --  Assume that the attributes are unavailable

      Inst_Body := Empty;
      Inst_Decl := Empty;

      --  Generic package or subprogram spec

      if Nkind_In (Exp_Inst, N_Package_Declaration,
                             N_Subprogram_Declaration)
      then
         Inst_Decl := Exp_Inst;
         Body_Id   := Corresponding_Body (Inst_Decl);

         if Present (Body_Id) then
            Inst_Body := Unit_Declaration_Node (Body_Id);
         end if;

      --  Generic package or subprogram body

      else
         pragma Assert
           (Nkind_In (Exp_Inst, N_Package_Body, N_Subprogram_Body));

         Inst_Body := Exp_Inst;
         Inst_Decl := Unit_Declaration_Node (Corresponding_Spec (Inst_Body));
      end if;
   end Extract_Instance_Attributes;

   --------------------------------------
   -- Extract_Instantiation_Attributes --
   --------------------------------------

   procedure Extract_Instantiation_Attributes
     (Exp_Inst : Node_Id;
      Inst     : out Node_Id;
      Inst_Id  : out Entity_Id;
      Gen_Id   : out Entity_Id;
      Attrs    : out Instantiation_Attributes)
   is
   begin
      Inst    := Original_Node (Exp_Inst);
      Inst_Id := Defining_Entity (Inst);

      --  Traverse a possible chain of renamings to obtain the original generic
      --  being instantiatied.

      Gen_Id := Get_Renamed_Entity (Entity (Name (Inst)));

      --  Set all attributes

      Attrs.Elab_Checks_OK    := Is_Elaboration_Checks_OK_Node (Inst);
      Attrs.Elab_Warnings_OK  := Is_Elaboration_Warnings_OK_Node (Inst);
      Attrs.Ghost_Mode_Ignore := Is_Ignored_Ghost_Node (Inst);
      Attrs.In_Declarations   := Is_Declaration_Level_Node (Inst);
      Attrs.SPARK_Mode_On     := Is_SPARK_Mode_On_Node (Inst);
   end Extract_Instantiation_Attributes;

   -------------------------------
   -- Extract_Target_Attributes --
   -------------------------------

   procedure Extract_Target_Attributes
     (Target_Id : Entity_Id;
      Attrs     : out Target_Attributes)
   is
      procedure Extract_Package_Or_Subprogram_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id);
      --  Obtain the attributes associated with a package or a subprogram.
      --  Spec_Id is the package or subprogram. Body_Decl is the declaration
      --  of the corresponding package or subprogram body.

      procedure Extract_Protected_Entry_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id;
         Body_Barf : out Node_Id);
      --  Obtain the attributes associated with a protected entry [family].
      --  Spec_Id is the entity of the protected body subprogram. Body_Decl
      --  is the declaration of Spec_Id's corresponding body. Body_Barf is
      --  the declaration of the barrier function body.

      procedure Extract_Protected_Subprogram_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id);
      --  Obtain the attributes associated with a protected subprogram. Formal
      --  Spec_Id is the entity of the protected body subprogram. Body_Decl is
      --  the declaration of Spec_Id's corresponding body.

      procedure Extract_Task_Entry_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id);
      --  Obtain the attributes associated with a task entry [family]. Formal
      --  Spec_Id is the entity of the task body procedure. Body_Decl is the
      --  declaration of Spec_Id's corresponding body.

      ----------------------------------------------
      -- Extract_Package_Or_Subprogram_Attributes --
      ----------------------------------------------

      procedure Extract_Package_Or_Subprogram_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id)
      is
         Body_Id   : Entity_Id;
         Init_Id   : Entity_Id;
         Spec_Decl : Node_Id;

      begin
         --  Assume that the body is not available

         Body_Decl := Empty;
         Spec_Id   := Target_Id;

         --  For body retrieval purposes, the entity of the initial declaration
         --  is that of the spec.

         Init_Id := Spec_Id;

         --  The only exception to the above is a function which returns a
         --  constrained array type in a SPARK-to-C compilation. In this case
         --  the function receives a corresponding procedure which has an out
         --  parameter. The proper body for ABE checks and diagnostics is that
         --  of the procedure.

         if Ekind (Init_Id) = E_Function
           and then Rewritten_For_C (Init_Id)
         then
            Init_Id := Corresponding_Procedure (Init_Id);
         end if;

         --  Extract the attributes of the body

         Spec_Decl := Unit_Declaration_Node (Init_Id);

         --  The initial declaration is a stand alone subprogram body

         if Nkind (Spec_Decl) = N_Subprogram_Body then
            Body_Decl := Spec_Decl;

         --  Otherwise the package or subprogram has a spec and a completing
         --  body.

         elsif Nkind_In (Spec_Decl, N_Generic_Package_Declaration,
                                    N_Generic_Subprogram_Declaration,
                                    N_Package_Declaration,
                                    N_Subprogram_Body_Stub,
                                    N_Subprogram_Declaration)
         then
            Body_Id := Corresponding_Body (Spec_Decl);

            if Present (Body_Id) then
               Body_Decl := Unit_Declaration_Node (Body_Id);
            end if;
         end if;
      end Extract_Package_Or_Subprogram_Attributes;

      ----------------------------------------
      -- Extract_Protected_Entry_Attributes --
      ----------------------------------------

      procedure Extract_Protected_Entry_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id;
         Body_Barf : out Node_Id)
      is
         Barf_Id : Entity_Id;
         Body_Id : Entity_Id;

      begin
         --  Assume that the bodies are not available

         Body_Barf := Empty;
         Body_Decl := Empty;

         --  When the entry [family] has already been expanded, it carries both
         --  the procedure which emulates the behavior of the entry [family] as
         --  well as the barrier function.

         if Present (Protected_Body_Subprogram (Target_Id)) then
            Spec_Id := Protected_Body_Subprogram (Target_Id);

            --  Extract the attributes of the barrier function

            Barf_Id :=
              Corresponding_Body
                (Unit_Declaration_Node (Barrier_Function (Target_Id)));

            if Present (Barf_Id) then
               Body_Barf := Unit_Declaration_Node (Barf_Id);
            end if;

         --  Otherwise no expansion took place

         else
            Spec_Id := Target_Id;
         end if;

         --  Extract the attributes of the entry body

         Body_Id := Corresponding_Body (Unit_Declaration_Node (Spec_Id));

         if Present (Body_Id) then
            Body_Decl := Unit_Declaration_Node (Body_Id);
         end if;
      end Extract_Protected_Entry_Attributes;

      ---------------------------------------------
      -- Extract_Protected_Subprogram_Attributes --
      ---------------------------------------------

      procedure Extract_Protected_Subprogram_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id)
      is
         Body_Id : Entity_Id;

      begin
         --  Assume that the body is not available

         Body_Decl := Empty;

         --  When the protected subprogram has already been expanded, it
         --  carries the subprogram which seizes the lock and invokes the
         --  original statements.

         if Present (Protected_Subprogram (Target_Id)) then
            Spec_Id :=
              Protected_Body_Subprogram (Protected_Subprogram (Target_Id));

         --  Otherwise no expansion took place

         else
            Spec_Id := Target_Id;
         end if;

         --  Extract the attributes of the body

         Body_Id := Corresponding_Body (Unit_Declaration_Node (Spec_Id));

         if Present (Body_Id) then
            Body_Decl := Unit_Declaration_Node (Body_Id);
         end if;
      end Extract_Protected_Subprogram_Attributes;

      -----------------------------------
      -- Extract_Task_Entry_Attributes --
      -----------------------------------

      procedure Extract_Task_Entry_Attributes
        (Spec_Id   : out Entity_Id;
         Body_Decl : out Node_Id)
      is
         Task_Typ : constant Entity_Id := Non_Private_View (Scope (Target_Id));
         Body_Id  : Entity_Id;

      begin
         --  Assume that the body is not available

         Body_Decl := Empty;

         --  The the task type has already been expanded, it carries the
         --  procedure which emulates the behavior of the task body.

         if Present (Task_Body_Procedure (Task_Typ)) then
            Spec_Id := Task_Body_Procedure (Task_Typ);

         --  Otherwise no expansion took place

         else
            Spec_Id := Task_Typ;
         end if;

         --  Extract the attributes of the body

         Body_Id := Corresponding_Body (Unit_Declaration_Node (Spec_Id));

         if Present (Body_Id) then
            Body_Decl := Unit_Declaration_Node (Body_Id);
         end if;
      end Extract_Task_Entry_Attributes;

      --  Local variables

      Prag      : constant Node_Id := SPARK_Pragma (Target_Id);
      Body_Barf : Node_Id;
      Body_Decl : Node_Id;
      Spec_Id   : Entity_Id;

   --  Start of processing for Extract_Target_Attributes

   begin
      --  Assume that the body of the barrier function is not available

      Body_Barf := Empty;

      --  The target is a protected entry [family]

      if Is_Protected_Entry (Target_Id) then
         Extract_Protected_Entry_Attributes
           (Spec_Id   => Spec_Id,
            Body_Decl => Body_Decl,
            Body_Barf => Body_Barf);

      --  The target is a protected subprogram

      elsif Is_Protected_Subp (Target_Id)
        or else Is_Protected_Body_Subp (Target_Id)
      then
         Extract_Protected_Subprogram_Attributes
           (Spec_Id   => Spec_Id,
            Body_Decl => Body_Decl);

      --  The target is a task entry [family]

      elsif Is_Task_Entry (Target_Id) then
         Extract_Task_Entry_Attributes
           (Spec_Id   => Spec_Id,
            Body_Decl => Body_Decl);

      --  Otherwise the target is a package or a subprogram

      else
         Extract_Package_Or_Subprogram_Attributes
           (Spec_Id   => Spec_Id,
            Body_Decl => Body_Decl);
      end if;

      --  Set all attributes

      Attrs.Body_Barf         := Body_Barf;
      Attrs.Body_Decl         := Body_Decl;
      Attrs.Elab_Checks_OK    := Is_Elaboration_Checks_OK_Id (Target_Id);
      Attrs.From_Source       := Comes_From_Source (Target_Id);
      Attrs.Ghost_Mode_Ignore := Is_Ignored_Ghost_Entity (Target_Id);
      Attrs.SPARK_Mode_On     :=
        Present (Prag) and then Get_SPARK_Mode_From_Annotation (Prag) = On;
      Attrs.Spec_Decl         := Unit_Declaration_Node (Spec_Id);
      Attrs.Spec_Id           := Spec_Id;
      Attrs.Unit_Id           := Find_Top_Unit (Target_Id);

      --  At this point certain attributes should always be available

      pragma Assert (Present (Attrs.Spec_Decl));
      pragma Assert (Present (Attrs.Spec_Id));
      pragma Assert (Present (Attrs.Unit_Id));
   end Extract_Target_Attributes;

   -----------------------------
   -- Extract_Task_Attributes --
   -----------------------------

   procedure Extract_Task_Attributes
     (Typ   : Entity_Id;
      Attrs : out Task_Attributes)
   is
      Task_Typ : constant Entity_Id := Non_Private_View (Typ);

      Body_Decl : Node_Id;
      Body_Id   : Entity_Id;
      Prag      : Node_Id;
      Spec_Id   : Entity_Id;

   begin
      --  Assume that the body of the task procedure is not available

      Body_Decl := Empty;

      --  The initial declaration is that of the task body procedure

      Spec_Id := Get_Task_Body_Procedure (Task_Typ);
      Body_Id := Corresponding_Body (Unit_Declaration_Node (Spec_Id));

      if Present (Body_Id) then
         Body_Decl := Unit_Declaration_Node (Body_Id);
      end if;

      Prag := SPARK_Pragma (Task_Typ);

      --  Set all attributes

      Attrs.Body_Decl         := Body_Decl;
      Attrs.Elab_Checks_OK    := Is_Elaboration_Checks_OK_Id (Task_Typ);
      Attrs.Ghost_Mode_Ignore := Is_Ignored_Ghost_Entity (Task_Typ);
      Attrs.SPARK_Mode_On     :=
        Present (Prag) and then Get_SPARK_Mode_From_Annotation (Prag) = On;
      Attrs.Spec_Id           := Spec_Id;
      Attrs.Task_Decl         := Declaration_Node (Task_Typ);
      Attrs.Unit_Id           := Find_Top_Unit (Task_Typ);

      --  At this point certain attributes should always be available

      pragma Assert (Present (Attrs.Spec_Id));
      pragma Assert (Present (Attrs.Task_Decl));
      pragma Assert (Present (Attrs.Unit_Id));
   end Extract_Task_Attributes;

   -------------------------------------------
   -- Extract_Variable_Reference_Attributes --
   -------------------------------------------

   procedure Extract_Variable_Reference_Attributes
     (Ref    : Node_Id;
      Var_Id : out Entity_Id;
      Attrs  : out Variable_Attributes)
   is
      function Get_Renamed_Variable (Id : Entity_Id) return Entity_Id;
      --  Obtain the ultimate renamed variable of variable Id

      --------------------------
      -- Get_Renamed_Variable --
      --------------------------

      function Get_Renamed_Variable (Id : Entity_Id) return Entity_Id is
         Ren_Id : Entity_Id;

      begin
         Ren_Id := Id;
         while Present (Renamed_Entity (Ren_Id))
           and then Nkind (Renamed_Entity (Ren_Id)) in N_Entity
         loop
            Ren_Id := Renamed_Entity (Ren_Id);
         end loop;

         return Ren_Id;
      end Get_Renamed_Variable;

   --  Start of processing for Extract_Variable_Reference_Attributes

   begin
      --  Extraction for variable reference markers

      if Nkind (Ref) = N_Variable_Reference_Marker then
         Var_Id := Target (Ref);

      --  Extraction for expanded names and identifiers

      else
         Var_Id := Entity (Ref);
      end if;

      --  Obtain the original variable which the reference mentions

      Var_Id        := Get_Renamed_Variable (Var_Id);
      Attrs.Unit_Id := Find_Top_Unit (Var_Id);

      --  At this point certain attributes should always be available

      pragma Assert (Present (Attrs.Unit_Id));
   end Extract_Variable_Reference_Attributes;

   --------------------
   -- Find_Code_Unit --
   --------------------

   function Find_Code_Unit (N : Node_Or_Entity_Id) return Entity_Id is
   begin
      return Find_Unit_Entity (Unit (Cunit (Get_Code_Unit (N))));
   end Find_Code_Unit;

   ----------------------------
   -- Find_Early_Call_Region --
   ----------------------------

   function Find_Early_Call_Region
     (Body_Decl        : Node_Id;
      Assume_Elab_Body : Boolean := False;
      Skip_Memoization : Boolean := False) return Node_Id
   is
      --  NOTE: The routines within Find_Early_Call_Region are intentionally
      --  unnested to avoid deep indentation of code.

      ECR_Found : exception;
      --  This exception is raised when the early call region has been found

      Start : Node_Id := Empty;
      --  The start of the early call region. This variable is updated by the
      --  various nested routines. Due to the use of exceptions, the variable
      --  must be global to the nested routines.

      --  The algorithm implemented in this routine attempts to find the early
      --  call region of a subprogram body by inspecting constructs in reverse
      --  declarative order, while navigating the tree. The algorithm consists
      --  of an Inspection phase and an Advancement phase. The pseudocode is as
      --  follows:
      --
      --    loop
      --       inspection phase
      --       advancement phase
      --    end loop
      --
      --  The infinite loop is terminated by raising exception ECR_Found. The
      --  algorithm utilizes two pointers, Curr and Start, to represent the
      --  current construct to inspect and the start of the early call region.
      --
      --  IMPORTANT: The algorithm must maintain the following invariant at all
      --  time for it to function properly - a nested construct is entered only
      --  when it contains suitable constructs. This guarantees that leaving a
      --  nested or encapsulating construct functions properly.
      --
      --  The Inspection phase determines whether the current construct is non-
      --  preelaborable, and if it is, the algorithm terminates.
      --
      --  The Advancement phase walks the tree in reverse declarative order,
      --  while entering and leaving nested and encapsulating constructs. It
      --  may also terminate the elaborithm. There are several special cases
      --  of advancement.
      --
      --  1) General case:
      --
      --    <construct 1>
      --     ...
      --    <construct N-1>                      <- Curr
      --    <construct N>                        <- Start
      --    <subprogram body>
      --
      --  In the general case, a declarative or statement list is traversed in
      --  reverse order where Curr is the lead pointer, and Start indicates the
      --  last preelaborable construct.
      --
      --  2) Entering handled bodies
      --
      --    package body Nested is               <- Curr (2.3)
      --       <declarations>                    <- Curr (2.2)
      --    begin
      --       <statements>                      <- Curr (2.1)
      --    end Nested;
      --    <construct>                          <- Start
      --
      --  In this case, the algorithm enters a handled body by starting from
      --  the last statement (2.1), or the last declaration (2.2), or the body
      --  is consumed (2.3) because it is empty and thus preelaborable.
      --
      --  3) Entering package declarations
      --
      --    package Nested is                    <- Curr (2.3)
      --       <visible declarations>            <- Curr (2.2)
      --    private
      --       <private declarations>            <- Curr (2.1)
      --    end Nested;
      --    <construct>                          <- Start
      --
      --  In this case, the algorithm enters a package declaration by starting
      --  from the last private declaration (2.1), the last visible declaration
      --  (2.2), or the package is consumed (2.3) because it is empty and thus
      --  preelaborable.
      --
      --  4) Transitioning from list to list of the same construct
      --
      --  Certain constructs have two eligible lists. The algorithm must thus
      --  transition from the second to the first list when the second list is
      --  exhausted.
      --
      --    declare                              <- Curr (4.2)
      --       <declarations>                    <- Curr (4.1)
      --    begin
      --       <statements>                      <- Start
      --    end;
      --
      --  In this case, the algorithm has exhausted the second list (statements
      --  in the example), and continues with the last declaration (4.1) or the
      --  construct is consumed (4.2) because it contains only preelaborable
      --  code.
      --
      --  5) Transitioning from list to construct
      --
      --    tack body Task is                    <- Curr (5.1)
      --                                         <- Curr (Empty)
      --       <construct 1>                     <- Start
      --
      --  In this case, the algorithm has exhausted a list, Curr is Empty, and
      --  the owner of the list is consumed (5.1).
      --
      --  6) Transitioning from unit to unit
      --
      --  A package body with a spec subject to pragma Elaborate_Body extends
      --  the possible range of the early call region to the package spec.
      --
      --    package Pack is                      <- Curr (6.3)
      --       pragma Elaborate_Body;            <- Curr (6.2)
      --       <visible declarations>            <- Curr (6.2)
      --    private
      --       <private declarations>            <- Curr (6.1)
      --    end Pack;
      --
      --    package body Pack is                 <- Curr, Start
      --
      --  In this case, the algorithm has reached a package body compilation
      --  unit whose spec is subject to pragma Elaborate_Body, or the caller
      --  of the algorithm has specified this behavior. This transition is
      --  equivalent to 3).
      --
      --  7) Transitioning from unit to termination
      --
      --  Reaching a compilation unit always terminates the algorithm as there
      --  are no more lists to examine. This must take 6) into account.
      --
      --  8) Transitioning from subunit to stub
      --
      --    package body Pack is separate;       <- Curr (8.1)
      --
      --    separate (...)
      --    package body Pack is                 <- Curr, Start
      --
      --  Reaching a subunit continues the search from the corresponding stub
      --  (8.1).

      procedure Advance (Curr : in out Node_Id);
      pragma Inline (Advance);
      --  Update the Curr and Start pointers depending on their location in the
      --  tree to the next eligible construct. This routine raises ECR_Found.

      procedure Enter_Handled_Body (Curr : in out Node_Id);
      pragma Inline (Enter_Handled_Body);
      --  Update the Curr and Start pointers to enter a nested handled body if
      --  applicable. This routine raises ECR_Found.

      procedure Enter_Package_Declaration (Curr : in out Node_Id);
      pragma Inline (Enter_Package_Declaration);
      --  Update the Curr and Start pointers to enter a nested package spec if
      --  applicable. This routine raises ECR_Found.

      function Find_ECR (N : Node_Id) return Node_Id;
      pragma Inline (Find_ECR);
      --  Find an early call region starting from arbitrary node N

      function Has_Suitable_Construct (List : List_Id) return Boolean;
      pragma Inline (Has_Suitable_Construct);
      --  Determine whether list List contains at least one suitable construct
      --  for inclusion into an early call region.

      procedure Include (N : Node_Id; Curr : out Node_Id);
      pragma Inline (Include);
      --  Update the Curr and Start pointers to include arbitrary construct N
      --  in the early call region. This routine raises ECR_Found.

      function Is_OK_Preelaborable_Construct (N : Node_Id) return Boolean;
      pragma Inline (Is_OK_Preelaborable_Construct);
      --  Determine whether arbitrary node N denotes a preelaboration-safe
      --  construct.

      function Is_Suitable_Construct (N : Node_Id) return Boolean;
      pragma Inline (Is_Suitable_Construct);
      --  Determine whether arbitrary node N denotes a suitable construct for
      --  inclusion into the early call region.

      procedure Transition_Body_Declarations
        (Bod  : Node_Id;
         Curr : in out Node_Id);
      pragma Inline (Transition_Body_Declarations);
      --  Update the Curr and Start pointers when construct Bod denotes a block
      --  statement or a suitable body. This routine raises ECR_Found.

      procedure Transition_Handled_Statements
        (HSS  : Node_Id;
         Curr : in out Node_Id);
      pragma Inline (Transition_Handled_Statements);
      --  Update the Curr and Start pointers when node HSS denotes a handled
      --  sequence of statements. This routine raises ECR_Found.

      procedure Transition_Spec_Declarations
        (Spec : Node_Id;
         Curr : in out Node_Id);
      pragma Inline (Transition_Spec_Declarations);
      --  Update the Curr and Start pointers when construct Spec denotes
      --  a concurrent definition or a package spec. This routine raises
      --  ECR_Found.

      procedure Transition_Unit (Unit : Node_Id; Curr : in out Node_Id);
      pragma Inline (Transition_Unit);
      --  Update the Curr and Start pointers when node Unit denotes a potential
      --  compilation unit. This routine raises ECR_Found.

      -------------
      -- Advance --
      -------------

      procedure Advance (Curr : in out Node_Id) is
         Context : Node_Id;

      begin
         --  Curr denotes one of the following cases upon entry into this
         --  routine:
         --
         --    * Empty - There is no current construct when a declarative or a
         --      statement list has been exhausted. This does not necessarily
         --      indicate that the early call region has been computed as it
         --      may still be possible to transition to another list.
         --
         --    * Encapsulator - The current construct encapsulates declarations
         --      and/or statements. This indicates that the early call region
         --      may extend within the nested construct.
         --
         --    * Preelaborable - The current construct is always preelaborable
         --      because Find_ECR would not invoke Advance if this was not the
         --      case.

         --  The current construct is an encapsulator or is preelaborable

         if Present (Curr) then

            --  Enter encapsulators by inspecting their declarations and/or
            --  statements.

            if Nkind_In (Curr, N_Block_Statement, N_Package_Body) then
               Enter_Handled_Body (Curr);

            elsif Nkind (Curr) = N_Package_Declaration then
               Enter_Package_Declaration (Curr);

            --  Early call regions have a property which can be exploited to
            --  optimize the algorithm.
            --
            --    <preceding subprogram body>
            --    <preelaborable construct 1>
            --     ...
            --    <preelaborable construct N>
            --    <initiating subprogram body>
            --
            --  If a traversal initiated from a subprogram body reaches a
            --  preceding subprogram body, then both bodies share the same
            --  early call region.
            --
            --  The property results in the following desirable effects:
            --
            --  * If the preceding body already has an early call region, then
            --    the initiating body can reuse it. This minimizes the amount
            --    of processing performed by the algorithm.
            --
            --  * If the preceding body lack an early call region, then the
            --    algorithm can compute the early call region, and reuse it
            --    for the initiating body. This processing performs the same
            --    amount of work, but has the beneficial effect of computing
            --    the early call regions of all preceding bodies.

            elsif Nkind_In (Curr, N_Entry_Body, N_Subprogram_Body) then
               Start :=
                 Find_Early_Call_Region
                   (Body_Decl        => Curr,
                    Assume_Elab_Body => Assume_Elab_Body,
                    Skip_Memoization => Skip_Memoization);

               raise ECR_Found;

            --  Otherwise current construct is preelaborable. Unpdate the early
            --  call region to include it.

            else
               Include (Curr, Curr);
            end if;

         --  Otherwise the current construct is missing, indicating that the
         --  current list has been exhausted. Depending on the context of the
         --  list, several transitions are possible.

         else
            --  The invariant of the algorithm ensures that Curr and Start are
            --  at the same level of nesting at the point of a transition. The
            --  algorithm can determine which list the traversal came from by
            --  examining Start.

            Context := Parent (Start);

            --  Attempt the following transitions:
            --
            --    private declarations -> visible declarations
            --    private declarations -> upper level
            --    private declarations -> terminate
            --    visible declarations -> upper level
            --    visible declarations -> terminate

            if Nkind_In (Context, N_Package_Specification,
                                  N_Protected_Definition,
                                  N_Task_Definition)
            then
               Transition_Spec_Declarations (Context, Curr);

            --  Attempt the following transitions:
            --
            --    statements -> declarations
            --    statements -> upper level
            --    statements -> corresponding package spec (Elab_Body)
            --    statements -> terminate

            elsif Nkind (Context) = N_Handled_Sequence_Of_Statements then
               Transition_Handled_Statements (Context, Curr);

            --  Attempt the following transitions:
            --
            --    declarations -> upper level
            --    declarations -> corresponding package spec (Elab_Body)
            --    declarations -> terminate

            elsif Nkind_In (Context, N_Block_Statement,
                                     N_Entry_Body,
                                     N_Package_Body,
                                     N_Protected_Body,
                                     N_Subprogram_Body,
                                     N_Task_Body)
            then
               Transition_Body_Declarations (Context, Curr);

            --  Otherwise it is not possible to transition. Stop the search
            --  because there are no more declarations or statements to check.

            else
               raise ECR_Found;
            end if;
         end if;
      end Advance;

      --------------------------
      -- Enter_Handled_Body --
      --------------------------

      procedure Enter_Handled_Body (Curr : in out Node_Id) is
         Decls : constant List_Id := Declarations (Curr);
         HSS   : constant Node_Id := Handled_Statement_Sequence (Curr);
         Stmts : List_Id := No_List;

      begin
         if Present (HSS) then
            Stmts := Statements (HSS);
         end if;

         --  The handled body has a non-empty statement sequence. The construct
         --  to inspect is the last statement.

         if Has_Suitable_Construct (Stmts) then
            Curr := Last (Stmts);

         --  The handled body lacks statements, but has non-empty declarations.
         --  The construct to inspect is the last declaration.

         elsif Has_Suitable_Construct (Decls) then
            Curr := Last (Decls);

         --  Otherwise the handled body lacks both declarations and statements.
         --  The construct to inspect is the node which precedes the handled
         --  body. Update the early call region to include the handled body.

         else
            Include (Curr, Curr);
         end if;
      end Enter_Handled_Body;

      -------------------------------
      -- Enter_Package_Declaration --
      -------------------------------

      procedure Enter_Package_Declaration (Curr : in out Node_Id) is
         Pack_Spec : constant Node_Id := Specification (Curr);
         Prv_Decls : constant List_Id := Private_Declarations (Pack_Spec);
         Vis_Decls : constant List_Id := Visible_Declarations (Pack_Spec);

      begin
         --  The package has a non-empty private declarations. The construct to
         --  inspect is the last private declaration.

         if Has_Suitable_Construct (Prv_Decls) then
            Curr := Last (Prv_Decls);

         --  The package lacks private declarations, but has non-empty visible
         --  declarations. In this case the construct to inspect is the last
         --  visible declaration.

         elsif Has_Suitable_Construct (Vis_Decls) then
            Curr := Last (Vis_Decls);

         --  Otherwise the package lacks any declarations. The construct to
         --  inspect is the node which precedes the package. Update the early
         --  call region to include the package declaration.

         else
            Include (Curr, Curr);
         end if;
      end Enter_Package_Declaration;

      --------------
      -- Find_ECR --
      --------------

      function Find_ECR (N : Node_Id) return Node_Id is
         Curr : Node_Id;

      begin
         --  The early call region starts at N

         Curr  := Prev (N);
         Start := N;

         --  Inspect each node in reverse declarative order while going in and
         --  out of nested and enclosing constructs. Note that the only way to
         --  terminate this infinite loop is to raise exception ECR_Found.

         loop
            --  The current construct is not preelaboration-safe. Terminate the
            --  traversal.

            if Present (Curr)
              and then not Is_OK_Preelaborable_Construct (Curr)
            then
               raise ECR_Found;
            end if;

            --  Advance to the next suitable construct. This may terminate the
            --  traversal by raising ECR_Found.

            Advance (Curr);
         end loop;

      exception
         when ECR_Found =>
            return Start;
      end Find_ECR;

      ----------------------------
      -- Has_Suitable_Construct --
      ----------------------------

      function Has_Suitable_Construct (List : List_Id) return Boolean is
         Item : Node_Id;

      begin
         --  Examine the list in reverse declarative order, looking for a
         --  suitable construct.

         if Present (List) then
            Item := Last (List);
            while Present (Item) loop
               if Is_Suitable_Construct (Item) then
                  return True;
               end if;

               Prev (Item);
            end loop;
         end if;

         return False;
      end Has_Suitable_Construct;

      -------------
      -- Include --
      -------------

      procedure Include (N : Node_Id; Curr : out Node_Id) is
      begin
         Start := N;

         --  The input node is a compilation unit. This terminates the search
         --  because there are no more lists to inspect and there are no more
         --  enclosing constructs to climb up to. The transitions are:
         --
         --    private declarations -> terminate
         --    visible declarations -> terminate
         --    statements           -> terminate
         --    declarations         -> terminate

         if Nkind (Parent (Start)) = N_Compilation_Unit then
            raise ECR_Found;

         --  Otherwise the input node is still within some list

         else
            Curr := Prev (Start);
         end if;
      end Include;

      -----------------------------------
      -- Is_OK_Preelaborable_Construct --
      -----------------------------------

      function Is_OK_Preelaborable_Construct (N : Node_Id) return Boolean is
      begin
         --  Assignment statements are acceptable as long as they were produced
         --  by the ABE mechanism to update elaboration flags.

         if Nkind (N) = N_Assignment_Statement then
            return Is_Elaboration_Code (N);

         --  Block statements are acceptable even though they directly violate
         --  preelaborability. The intention is not to penalize the early call
         --  region when a block contains only preelaborable constructs.
         --
         --    declare
         --       Val : constant Integer := 1;
         --    begin
         --       pragma Assert (Val = 1);
         --       null;
         --    end;
         --
         --  Note that the Advancement phase does enter blocks, and will detect
         --  any non-preelaborable declarations or statements within.

         elsif Nkind (N) = N_Block_Statement then
            return True;
         end if;

         --  Otherwise the construct must be preelaborable. The check must take
         --  the syntactic and semantic structure of the construct. DO NOT use
         --  Is_Preelaborable_Construct here.

         return not Is_Non_Preelaborable_Construct (N);
      end Is_OK_Preelaborable_Construct;

      ---------------------------
      -- Is_Suitable_Construct --
      ---------------------------

      function Is_Suitable_Construct (N : Node_Id) return Boolean is
         Context : constant Node_Id := Parent (N);

      begin
         --  An internally-generated statement sequence which contains only a
         --  single null statement is not a suitable construct because it is a
         --  byproduct of the parser. Such a null statement should be excluded
         --  from the early call region because it carries the source location
         --  of the "end" keyword, and may lead to confusing diagnistics.

         if Nkind (N) = N_Null_Statement
           and then not Comes_From_Source (N)
           and then Present (Context)
           and then Nkind (Context) = N_Handled_Sequence_Of_Statements
           and then not Comes_From_Source (N)
         then
            return False;
         end if;

         --  Otherwise only constructs which correspond to pure Ada constructs
         --  are considered suitable.

         case Nkind (N) is
            when N_Call_Marker
               | N_Freeze_Entity
               | N_Freeze_Generic_Entity
               | N_Implicit_Label_Declaration
               | N_Itype_Reference
               | N_Pop_Constraint_Error_Label
               | N_Pop_Program_Error_Label
               | N_Pop_Storage_Error_Label
               | N_Push_Constraint_Error_Label
               | N_Push_Program_Error_Label
               | N_Push_Storage_Error_Label
               | N_SCIL_Dispatch_Table_Tag_Init
               | N_SCIL_Dispatching_Call
               | N_SCIL_Membership_Test
               | N_Variable_Reference_Marker
            =>
               return False;

            when others =>
               return True;
         end case;
      end Is_Suitable_Construct;

      ----------------------------------
      -- Transition_Body_Declarations --
      ----------------------------------

      procedure Transition_Body_Declarations
        (Bod  : Node_Id;
         Curr : in out Node_Id)
      is
         Decls : constant List_Id := Declarations (Bod);

      begin
         --  The search must come from the declarations of the body

         pragma Assert
           (Is_Non_Empty_List (Decls)
             and then List_Containing (Start) = Decls);

         --  The search finished inspecting the declarations. The construct
         --  to inspect is the node which precedes the handled body, unless
         --  the body is a compilation unit. The transitions are:
         --
         --    declarations -> upper level
         --    declarations -> corresponding package spec (Elab_Body)
         --    declarations -> terminate

         Transition_Unit (Bod, Curr);
      end Transition_Body_Declarations;

      -----------------------------------
      -- Transition_Handled_Statements --
      -----------------------------------

      procedure Transition_Handled_Statements
        (HSS  : Node_Id;
         Curr : in out Node_Id)
      is
         Bod   : constant Node_Id := Parent (HSS);
         Decls : constant List_Id := Declarations (Bod);
         Stmts : constant List_Id := Statements (HSS);

      begin
         --  The search must come from the statements of certain bodies or
         --  statements.

         pragma Assert (Nkind_In (Bod, N_Block_Statement,
                                       N_Entry_Body,
                                       N_Package_Body,
                                       N_Protected_Body,
                                       N_Subprogram_Body,
                                       N_Task_Body));

         --  The search must come from the statements of the handled sequence

         pragma Assert
           (Is_Non_Empty_List (Stmts)
             and then List_Containing (Start) = Stmts);

         --  The search finished inspecting the statements. The handled body
         --  has non-empty declarations. The construct to inspect is the last
         --  declaration. The transitions are:
         --
         --    statements -> declarations

         if Has_Suitable_Construct (Decls) then
            Curr := Last (Decls);

         --  Otherwise the handled body lacks declarations. The construct to
         --  inspect is the node which precedes the handled body, unless the
         --  body is a compilation unit. The transitions are:
         --
         --    statements -> upper level
         --    statements -> corresponding package spec (Elab_Body)
         --    statements -> terminate

         else
            Transition_Unit (Bod, Curr);
         end if;
      end Transition_Handled_Statements;

      ----------------------------------
      -- Transition_Spec_Declarations --
      ----------------------------------

      procedure Transition_Spec_Declarations
        (Spec : Node_Id;
         Curr : in out Node_Id)
      is
         Prv_Decls : constant List_Id := Private_Declarations (Spec);
         Vis_Decls : constant List_Id := Visible_Declarations (Spec);

      begin
         pragma Assert (Present (Start) and then Is_List_Member (Start));

         --  The search came from the private declarations and finished their
         --  inspection.

         if Has_Suitable_Construct (Prv_Decls)
           and then List_Containing (Start) = Prv_Decls
         then
            --  The context has non-empty visible declarations. The node to
            --  inspect is the last visible declaration. The transitions are:
            --
            --    private declarations -> visible declarations

            if Has_Suitable_Construct (Vis_Decls) then
               Curr := Last (Vis_Decls);

            --  Otherwise the context lacks visible declarations. The construct
            --  to inspect is the node which precedes the context unless the
            --  context is a compilation unit. The transitions are:
            --
            --    private declarations -> upper level
            --    private declarations -> terminate

            else
               Transition_Unit (Parent (Spec), Curr);
            end if;

         --  The search came from the visible declarations and finished their
         --  inspections. The construct to inspect is the node which precedes
         --  the context, unless the context is a compilaton unit. The
         --  transitions are:
         --
         --    visible declarations -> upper level
         --    visible declarations -> terminate

         elsif Has_Suitable_Construct (Vis_Decls)
           and then List_Containing (Start) = Vis_Decls
         then
            Transition_Unit (Parent (Spec), Curr);

         --  At this point both declarative lists are empty, but the traversal
         --  still came from within the spec. This indicates that the invariant
         --  of the algorithm has been violated.

         else
            pragma Assert (False);
            raise ECR_Found;
         end if;
      end Transition_Spec_Declarations;

      ---------------------
      -- Transition_Unit --
      ---------------------

      procedure Transition_Unit
        (Unit : Node_Id;
         Curr : in out Node_Id)
      is
         Context : constant Node_Id := Parent (Unit);

      begin
         --  The unit is a compilation unit. This terminates the search because
         --  there are no more lists to inspect and there are no more enclosing
         --  constructs to climb up to.

         if Nkind (Context) = N_Compilation_Unit then

            --  A package body with a corresponding spec subject to pragma
            --  Elaborate_Body is an exception to the above. The annotation
            --  allows the search to continue into the package declaration.
            --  The transitions are:
            --
            --    statements   -> corresponding package spec (Elab_Body)
            --    declarations -> corresponding package spec (Elab_Body)

            if Nkind (Unit) = N_Package_Body
              and then (Assume_Elab_Body
                         or else Has_Pragma_Elaborate_Body
                                   (Corresponding_Spec (Unit)))
            then
               Curr := Unit_Declaration_Node (Corresponding_Spec (Unit));
               Enter_Package_Declaration (Curr);

            --  Otherwise terminate the search. The transitions are:
            --
            --    private declarations -> terminate
            --    visible declarations -> terminate
            --    statements           -> terminate
            --    declarations         -> terminate

            else
               raise ECR_Found;
            end if;

         --  The unit is a subunit. The construct to inspect is the node which
         --  precedes the corresponding stub. Update the early call region to
         --  include the unit.

         elsif Nkind (Context) = N_Subunit then
            Start := Unit;
            Curr  := Corresponding_Stub (Context);

         --  Otherwise the unit is nested. The construct to inspect is the node
         --  which precedes the unit. Update the early call region to include
         --  the unit.

         else
            Include (Unit, Curr);
         end if;
      end Transition_Unit;

      --  Local variables

      Body_Id : constant Entity_Id := Defining_Entity (Body_Decl);
      Region  : Node_Id;

   --  Start of processing for Find_Early_Call_Region

   begin
      --  The caller demands the start of the early call region without saving
      --  or retrieving it to/from internal data structures.

      if Skip_Memoization then
         Region := Find_ECR (Body_Decl);

      --  Default behavior

      else
         --  Check whether the early call region of the subprogram body is
         --  available.

         Region := Early_Call_Region (Body_Id);

         if No (Region) then

            --  Traverse the declarations in reverse order, starting from the
            --  subprogram body, searching for the nearest non-preelaborable
            --  construct. The early call region starts after this construct
            --  and ends at the subprogram body.

            Region := Find_ECR (Body_Decl);

            --  Associate the early call region with the subprogram body in
            --  case other scenarios need it.

            Set_Early_Call_Region (Body_Id, Region);
         end if;
      end if;

      --  A subprogram body must always have an early call region

      pragma Assert (Present (Region));

      return Region;
   end Find_Early_Call_Region;

   ---------------------------
   -- Find_Elaborated_Units --
   ---------------------------

   procedure Find_Elaborated_Units is
      procedure Add_Pragma (Prag : Node_Id);
      --  Determine whether pragma Prag denotes a legal Elaborate[_All] pragma.
      --  If this is the case, add the related unit to the elaboration context.
      --  For pragma Elaborate_All, include recursively all units withed by the
      --  related unit.

      procedure Add_Unit
        (Unit_Id      : Entity_Id;
         Prag         : Node_Id;
         Full_Context : Boolean);
      --  Add unit Unit_Id to the elaboration context. Prag denotes the pragma
      --  which prompted the inclusion of the unit to the elaboration context.
      --  If flag Full_Context is set, examine the nonlimited clauses of unit
      --  Unit_Id and add each withed unit to the context.

      procedure Find_Elaboration_Context (Comp_Unit : Node_Id);
      --  Examine the context items of compilation unit Comp_Unit for suitable
      --  elaboration-related pragmas and add all related units to the context.

      ----------------
      -- Add_Pragma --
      ----------------

      procedure Add_Pragma (Prag : Node_Id) is
         Prag_Args : constant List_Id := Pragma_Argument_Associations (Prag);
         Prag_Nam  : constant Name_Id := Pragma_Name (Prag);
         Unit_Arg  : Node_Id;

      begin
         --  Nothing to do if the pragma is not related to elaboration

         if not Nam_In (Prag_Nam, Name_Elaborate, Name_Elaborate_All) then
            return;

         --  Nothing to do when the pragma is illegal

         elsif Error_Posted (Prag) then
            return;
         end if;

         Unit_Arg := Get_Pragma_Arg (First (Prag_Args));

         --  The argument of the pragma may appear in package.package form

         if Nkind (Unit_Arg) = N_Selected_Component then
            Unit_Arg := Selector_Name (Unit_Arg);
         end if;

         Add_Unit
           (Unit_Id      => Entity (Unit_Arg),
            Prag         => Prag,
            Full_Context => Prag_Nam = Name_Elaborate_All);
      end Add_Pragma;

      --------------
      -- Add_Unit --
      --------------

      procedure Add_Unit
        (Unit_Id      : Entity_Id;
         Prag         : Node_Id;
         Full_Context : Boolean)
      is
         Clause     : Node_Id;
         Elab_Attrs : Elaboration_Attributes;

      begin
         --  Nothing to do when some previous error left a with clause or a
         --  pragma in a bad state.

         if No (Unit_Id) then
            return;
         end if;

         Elab_Attrs := Elaboration_Status (Unit_Id);

         --  The unit is already included in the context by means of pragma
         --  Elaborate[_All].

         if Present (Elab_Attrs.Source_Pragma) then

            --  Upgrade an existing pragma Elaborate when the unit is subject
            --  to Elaborate_All because the new pragma covers a larger set of
            --  units.

            if Pragma_Name (Elab_Attrs.Source_Pragma) = Name_Elaborate
              and then Pragma_Name (Prag) = Name_Elaborate_All
            then
               Elab_Attrs.Source_Pragma := Prag;

            --  Otherwise the unit retains its existing pragma and does not
            --  need to be included in the context again.

            else
               return;
            end if;

         --  The current unit is not part of the context. Prepare a new set of
         --  attributes.

         else
            Elab_Attrs :=
              Elaboration_Attributes'(Source_Pragma => Prag,
                                      With_Clause   => Empty);
         end if;

         --  Add or update the attributes of the unit

         Set_Elaboration_Status (Unit_Id, Elab_Attrs);

         --  Includes all units withed by the current one when computing the
         --  full context.

         if Full_Context then

            --  Process all nonlimited with clauses found in the context of
            --  the current unit. Note that limited clauses do not impose an
            --  elaboration order.

            Clause := First (Context_Items (Compilation_Unit (Unit_Id)));
            while Present (Clause) loop
               if Nkind (Clause) = N_With_Clause
                 and then not Error_Posted (Clause)
                 and then not Limited_Present (Clause)
               then
                  Add_Unit
                    (Unit_Id      => Entity (Name (Clause)),
                     Prag         => Prag,
                     Full_Context => Full_Context);
               end if;

               Next (Clause);
            end loop;
         end if;
      end Add_Unit;

      ------------------------------
      -- Find_Elaboration_Context --
      ------------------------------

      procedure Find_Elaboration_Context (Comp_Unit : Node_Id) is
         Prag : Node_Id;

      begin
         pragma Assert (Nkind (Comp_Unit) = N_Compilation_Unit);

         --  Process all elaboration-related pragmas found in the context of
         --  the compilation unit.

         Prag := First (Context_Items (Comp_Unit));
         while Present (Prag) loop
            if Nkind (Prag) = N_Pragma then
               Add_Pragma (Prag);
            end if;

            Next (Prag);
         end loop;
      end Find_Elaboration_Context;

      --  Local variables

      Par_Id : Entity_Id;
      Unt    : Node_Id;

   --  Start of processing for Find_Elaborated_Units

   begin
      --  Perform a traversal which examines the context of the main unit and
      --  populates the Elaboration_Context table with all units elaborated
      --  prior to the main unit. The traversal performs the following jumps:

      --    subunit        -> parent subunit
      --    parent subunit -> body
      --    body           -> spec
      --    spec           -> parent spec
      --    parent spec    -> grandparent spec and so on

      --  The traversal relies on units rather than scopes because the scope of
      --  a subunit is some spec, while this traversal must process the body as
      --  well. Given that protected and task bodies can also be subunits, this
      --  complicates the scope approach even further.

      Unt := Unit (Cunit (Main_Unit));

      --  Perform the following traversals when the main unit is a subunit

      --    subunit        -> parent subunit
      --    parent subunit -> body

      while Present (Unt) and then Nkind (Unt) = N_Subunit loop
         Find_Elaboration_Context (Parent (Unt));

         --  Continue the traversal by going to the unit which contains the
         --  corresponding stub.

         if Present (Corresponding_Stub (Unt)) then
            Unt := Unit (Cunit (Get_Source_Unit (Corresponding_Stub (Unt))));

         --  Otherwise the subunit may be erroneous or left in a bad state

         else
            exit;
         end if;
      end loop;

      --  Perform the following traversal now that subunits have been taken
      --  care of, or the main unit is a body.

      --    body -> spec

      if Present (Unt)
        and then Nkind_In (Unt, N_Package_Body, N_Subprogram_Body)
      then
         Find_Elaboration_Context (Parent (Unt));

         --  Continue the traversal by going to the unit which contains the
         --  corresponding spec.

         if Present (Corresponding_Spec (Unt)) then
            Unt := Unit (Cunit (Get_Source_Unit (Corresponding_Spec (Unt))));
         end if;
      end if;

      --  Perform the following traversals now that the body has been taken
      --  care of, or the main unit is a spec.

      --    spec        -> parent spec
      --    parent spec -> grandparent spec and so on

      if Present (Unt)
        and then Nkind_In (Unt, N_Generic_Package_Declaration,
                                N_Generic_Subprogram_Declaration,
                                N_Package_Declaration,
                                N_Subprogram_Declaration)
      then
         Find_Elaboration_Context (Parent (Unt));

         --  Process a potential chain of parent units which ends with the
         --  main unit spec. The traversal can now safely rely on the scope
         --  chain.

         Par_Id := Scope (Defining_Entity (Unt));
         while Present (Par_Id) and then Par_Id /= Standard_Standard loop
            Find_Elaboration_Context (Compilation_Unit (Par_Id));

            Par_Id := Scope (Par_Id);
         end loop;
      end if;
   end Find_Elaborated_Units;

   -----------------------------
   -- Find_Enclosing_Instance --
   -----------------------------

   function Find_Enclosing_Instance (N : Node_Id) return Node_Id is
      Par     : Node_Id;
      Spec_Id : Entity_Id;

   begin
      --  Climb the parent chain looking for an enclosing instance spec or body

      Par := N;
      while Present (Par) loop

         --  Generic package or subprogram spec

         if Nkind_In (Par, N_Package_Declaration,
                           N_Subprogram_Declaration)
           and then Is_Generic_Instance (Defining_Entity (Par))
         then
            return Par;

         --  Generic package or subprogram body

         elsif Nkind_In (Par, N_Package_Body, N_Subprogram_Body) then
            Spec_Id := Corresponding_Spec (Par);

            if Present (Spec_Id) and then Is_Generic_Instance (Spec_Id) then
               return Par;
            end if;
         end if;

         Par := Parent (Par);
      end loop;

      return Empty;
   end Find_Enclosing_Instance;

   --------------------------
   -- Find_Enclosing_Level --
   --------------------------

   function Find_Enclosing_Level (N : Node_Id) return Enclosing_Level_Kind is
      function Level_Of (Unit : Node_Id) return Enclosing_Level_Kind;
      --  Obtain the corresponding level of unit Unit

      --------------
      -- Level_Of --
      --------------

      function Level_Of (Unit : Node_Id) return Enclosing_Level_Kind is
         Spec_Id : Entity_Id;

      begin
         if Nkind (Unit) in N_Generic_Instantiation then
            return Instantiation;

         elsif Nkind (Unit) = N_Generic_Package_Declaration then
            return Generic_Package_Spec;

         elsif Nkind (Unit) = N_Package_Declaration then
            return Package_Spec;

         elsif Nkind (Unit) = N_Package_Body then
            Spec_Id := Corresponding_Spec (Unit);

            --  The body belongs to a generic package

            if Present (Spec_Id)
              and then Ekind (Spec_Id) = E_Generic_Package
            then
               return Generic_Package_Body;

            --  Otherwise the body belongs to a non-generic package. This also
            --  treats an illegal package body without a corresponding spec as
            --  a non-generic package body.

            else
               return Package_Body;
            end if;
         end if;

         return No_Level;
      end Level_Of;

      --  Local variables

      Context : Node_Id;
      Curr    : Node_Id;
      Prev    : Node_Id;

   --  Start of processing for Find_Enclosing_Level

   begin
      --  Call markers and instantiations which appear at the declaration level
      --  but are later relocated in a different context retain their original
      --  declaration level.

      if Nkind_In (N, N_Call_Marker,
                      N_Function_Instantiation,
                      N_Package_Instantiation,
                      N_Procedure_Instantiation)
        and then Is_Declaration_Level_Node (N)
      then
         return Declaration_Level;
      end if;

      --  Climb the parent chain looking at the enclosing levels

      Prev := N;
      Curr := Parent (Prev);
      while Present (Curr) loop

         --  A traversal from a subunit continues via the corresponding stub

         if Nkind (Curr) = N_Subunit then
            Curr := Corresponding_Stub (Curr);

         --  The current construct is a package. Packages are ignored because
         --  they are always elaborated when the enclosing context is invoked
         --  or elaborated.

         elsif Nkind_In (Curr, N_Package_Body, N_Package_Declaration) then
            null;

         --  The current construct is a block statement

         elsif Nkind (Curr) = N_Block_Statement then

            --  Ignore internally generated blocks created by the expander for
            --  various purposes such as abort defer/undefer.

            if not Comes_From_Source (Curr) then
               null;

            --  If the traversal came from the handled sequence of statments,
            --  then the node appears at the level of the enclosing construct.
            --  This is a more reliable test because transients scopes within
            --  the declarative region of the encapsulator are hard to detect.

            elsif Nkind (Prev) = N_Handled_Sequence_Of_Statements
              and then Handled_Statement_Sequence (Curr) = Prev
            then
               return Find_Enclosing_Level (Parent (Curr));

            --  Otherwise the traversal came from the declarations, the node is
            --  at the declaration level.

            else
               return Declaration_Level;
            end if;

         --  The current construct is a declaration-level encapsulator

         elsif Nkind_In (Curr, N_Entry_Body,
                               N_Subprogram_Body,
                               N_Task_Body)
         then
            --  If the traversal came from the handled sequence of statments,
            --  then the node cannot possibly appear at any level. This is
            --  a more reliable test because transients scopes within the
            --  declarative region of the encapsulator are hard to detect.

            if Nkind (Prev) = N_Handled_Sequence_Of_Statements
              and then Handled_Statement_Sequence (Curr) = Prev
            then
               return No_Level;

            --  Otherwise the traversal came from the declarations, the node is
            --  at the declaration level.

            else
               return Declaration_Level;
            end if;

         --  The current construct is a non-library-level encapsulator which
         --  indicates that the node cannot possibly appear at any level.
         --  Note that this check must come after the declaration-level check
         --  because both predicates share certain nodes.

         elsif Is_Non_Library_Level_Encapsulator (Curr) then
            Context := Parent (Curr);

            --  The sole exception is when the encapsulator is the compilation
            --  utit itself because the compilation unit node requires special
            --  processing (see below).

            if Present (Context)
              and then Nkind (Context) = N_Compilation_Unit
            then
               null;

            --  Otherwise the node is not at any level

            else
               return No_Level;
            end if;

         --  The current construct is a compilation unit. The node appears at
         --  the [generic] library level when the unit is a [generic] package.

         elsif Nkind (Curr) = N_Compilation_Unit then
            return Level_Of (Unit (Curr));
         end if;

         Prev := Curr;
         Curr := Parent (Prev);
      end loop;

      return No_Level;
   end Find_Enclosing_Level;

   -------------------
   -- Find_Top_Unit --
   -------------------

   function Find_Top_Unit (N : Node_Or_Entity_Id) return Entity_Id is
   begin
      return Find_Unit_Entity (Unit (Cunit (Get_Top_Level_Code_Unit (N))));
   end Find_Top_Unit;

   ----------------------
   -- Find_Unit_Entity --
   ----------------------

   function Find_Unit_Entity (N : Node_Id) return Entity_Id is
      Context : constant Node_Id := Parent (N);
      Orig_N  : constant Node_Id := Original_Node (N);

   begin
      --  The unit denotes a package body of an instantiation which acts as
      --  a compilation unit. The proper entity is that of the package spec.

      if Nkind (N) = N_Package_Body
        and then Nkind (Orig_N) = N_Package_Instantiation
        and then Nkind (Context) = N_Compilation_Unit
      then
         return Corresponding_Spec (N);

      --  The unit denotes an anonymous package created to wrap a subprogram
      --  instantiation which acts as a compilation unit. The proper entity is
      --  that of the "related instance".

      elsif Nkind (N) = N_Package_Declaration
        and then Nkind_In (Orig_N, N_Function_Instantiation,
                                   N_Procedure_Instantiation)
        and then Nkind (Context) = N_Compilation_Unit
      then
         return
           Related_Instance (Defining_Entity (N, Concurrent_Subunit => True));

      --  Otherwise the proper entity is the defining entity

      else
         return Defining_Entity (N, Concurrent_Subunit => True);
      end if;
   end Find_Unit_Entity;

   -----------------------
   -- First_Formal_Type --
   -----------------------

   function First_Formal_Type (Subp_Id : Entity_Id) return Entity_Id is
      Formal_Id : constant Entity_Id := First_Formal (Subp_Id);
      Typ       : Entity_Id;

   begin
      if Present (Formal_Id) then
         Typ := Etype (Formal_Id);

         --  Handle various combinations of concurrent and private types

         loop
            if Ekind_In (Typ, E_Protected_Type, E_Task_Type)
              and then Present (Anonymous_Object (Typ))
            then
               Typ := Anonymous_Object (Typ);

            elsif Is_Concurrent_Record_Type (Typ) then
               Typ := Corresponding_Concurrent_Type (Typ);

            elsif Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
               Typ := Full_View (Typ);

            else
               exit;
            end if;
         end loop;

         return Typ;
      end if;

      return Empty;
   end First_Formal_Type;

   --------------
   -- Has_Body --
   --------------

   function Has_Body (Pack_Decl : Node_Id) return Boolean is
      function Find_Corresponding_Body (Spec_Id : Entity_Id) return Node_Id;
      --  Try to locate the corresponding body of spec Spec_Id. If no body is
      --  found, return Empty.

      function Find_Body
        (Spec_Id : Entity_Id;
         From    : Node_Id) return Node_Id;
      --  Try to locate the corresponding body of spec Spec_Id in the node list
      --  which follows arbitrary node From. If no body is found, return Empty.

      function Load_Package_Body (Unit_Nam : Unit_Name_Type) return Node_Id;
      --  Attempt to load the body of unit Unit_Nam. If the load failed, return
      --  Empty. If the compilation will not generate code, return Empty.

      -----------------------------
      -- Find_Corresponding_Body --
      -----------------------------

      function Find_Corresponding_Body (Spec_Id : Entity_Id) return Node_Id is
         Context   : constant Entity_Id := Scope (Spec_Id);
         Spec_Decl : constant Node_Id   := Unit_Declaration_Node (Spec_Id);
         Body_Decl : Node_Id;
         Body_Id   : Entity_Id;

      begin
         if Is_Compilation_Unit (Spec_Id) then
            Body_Id := Corresponding_Body (Spec_Decl);

            if Present (Body_Id) then
               return Unit_Declaration_Node (Body_Id);

            --  The package is at the library and requires a body. Load the
            --  corresponding body because the optional body may be declared
            --  there.

            elsif Unit_Requires_Body (Spec_Id) then
               return
                 Load_Package_Body
                   (Get_Body_Name (Unit_Name (Get_Source_Unit (Spec_Decl))));

            --  Otherwise there is no optional body

            else
               return Empty;
            end if;

         --  The immediate context is a package. The optional body may be
         --  within the body of that package.

         --    procedure Proc is
         --       package Nested_1 is
         --          package Nested_2 is
         --             generic
         --             package Pack is
         --             end Pack;
         --          end Nested_2;
         --       end Nested_1;

         --       package body Nested_1 is
         --          package body Nested_2 is separate;
         --       end Nested_1;

         --    separate (Proc.Nested_1.Nested_2)
         --    package body Nested_2 is
         --       package body Pack is           --  optional body
         --          ...
         --       end Pack;
         --    end Nested_2;

         elsif Is_Package_Or_Generic_Package (Context) then
            Body_Decl := Find_Corresponding_Body (Context);

            --  The optional body is within the body of the enclosing package

            if Present (Body_Decl) then
               return
                 Find_Body
                   (Spec_Id => Spec_Id,
                    From    => First (Declarations (Body_Decl)));

            --  Otherwise the enclosing package does not have a body. This may
            --  be the result of an error or a genuine lack of a body.

            else
               return Empty;
            end if;

         --  Otherwise the immediate context is a body. The optional body may
         --  be within the same list as the spec.

         --    procedure Proc is
         --       generic
         --       package Pack is
         --       end Pack;

         --       package body Pack is           --  optional body
         --          ...
         --       end Pack;

         else
            return
              Find_Body
                (Spec_Id => Spec_Id,
                 From    => Next (Spec_Decl));
         end if;
      end Find_Corresponding_Body;

      ---------------
      -- Find_Body --
      ---------------

      function Find_Body
        (Spec_Id : Entity_Id;
         From    : Node_Id) return Node_Id
      is
         Spec_Nam : constant Name_Id := Chars (Spec_Id);
         Item     : Node_Id;
         Lib_Unit : Node_Id;

      begin
         Item := From;
         while Present (Item) loop

            --  The current item denotes the optional body

            if Nkind (Item) = N_Package_Body
              and then Chars (Defining_Entity (Item)) = Spec_Nam
            then
               return Item;

            --  The current item denotes a stub, the optional body may be in
            --  the subunit.

            elsif Nkind (Item) = N_Package_Body_Stub
              and then Chars (Defining_Entity (Item)) = Spec_Nam
            then
               Lib_Unit := Library_Unit (Item);

               --  The corresponding subunit was previously loaded

               if Present (Lib_Unit) then
                  return Lib_Unit;

               --  Otherwise attempt to load the corresponding subunit

               else
                  return Load_Package_Body (Get_Unit_Name (Item));
               end if;
            end if;

            Next (Item);
         end loop;

         return Empty;
      end Find_Body;

      -----------------------
      -- Load_Package_Body --
      -----------------------

      function Load_Package_Body (Unit_Nam : Unit_Name_Type) return Node_Id is
         Body_Decl : Node_Id;
         Unit_Num  : Unit_Number_Type;

      begin
         --  The load is performed only when the compilation will generate code

         if Operating_Mode = Generate_Code then
            Unit_Num :=
              Load_Unit
                (Load_Name  => Unit_Nam,
                 Required   => False,
                 Subunit    => False,
                 Error_Node => Pack_Decl);

            --  The load failed most likely because the physical file is
            --  missing.

            if Unit_Num = No_Unit then
               return Empty;

            --  Otherwise the load was successful, return the body of the unit

            else
               Body_Decl := Unit (Cunit (Unit_Num));

               --  If the unit is a subunit with an available proper body,
               --  return the proper body.

               if Nkind (Body_Decl) = N_Subunit
                 and then Present (Proper_Body (Body_Decl))
               then
                  Body_Decl := Proper_Body (Body_Decl);
               end if;

               return Body_Decl;
            end if;
         end if;

         return Empty;
      end Load_Package_Body;

      --  Local variables

      Pack_Id : constant Entity_Id := Defining_Entity (Pack_Decl);

   --  Start of processing for Has_Body

   begin
      --  The body is available

      if Present (Corresponding_Body (Pack_Decl)) then
         return True;

      --  The body is required if the package spec contains a construct which
      --  requires a completion in a body.

      elsif Unit_Requires_Body (Pack_Id) then
         return True;

      --  The body may be optional

      else
         return Present (Find_Corresponding_Body (Pack_Id));
      end if;
   end Has_Body;

   ---------------------------
   -- Has_Prior_Elaboration --
   ---------------------------

   function Has_Prior_Elaboration
     (Unit_Id      : Entity_Id;
      Context_OK   : Boolean := False;
      Elab_Body_OK : Boolean := False;
      Same_Unit_OK : Boolean := False) return Boolean
   is
      Main_Id : constant Entity_Id := Cunit_Entity (Main_Unit);

   begin
      --  A preelaborated unit is always elaborated prior to the main unit

      if Is_Preelaborated_Unit (Unit_Id) then
         return True;

      --  An internal unit is always elaborated prior to a non-internal main
      --  unit.

      elsif In_Internal_Unit (Unit_Id)
        and then not In_Internal_Unit (Main_Id)
      then
         return True;

      --  A unit has prior elaboration if it appears within the context of the
      --  main unit. Consider this case only when requested by the caller.

      elsif Context_OK
        and then Elaboration_Status (Unit_Id) /= No_Elaboration_Attributes
      then
         return True;

      --  A unit whose body is elaborated together with its spec has prior
      --  elaboration except with respect to itself. Consider this case only
      --  when requested by the caller.

      elsif Elab_Body_OK
        and then Has_Pragma_Elaborate_Body (Unit_Id)
        and then not Is_Same_Unit (Unit_Id, Main_Id)
      then
         return True;

      --  A unit has no prior elaboration with respect to itself, but does not
      --  require any means of ensuring its own elaboration either. Treat this
      --  case as valid prior elaboration only when requested by the caller.

      elsif Same_Unit_OK and then Is_Same_Unit (Unit_Id, Main_Id) then
         return True;
      end if;

      return False;
   end Has_Prior_Elaboration;

   --------------------------
   -- In_External_Instance --
   --------------------------

   function In_External_Instance
     (N           : Node_Id;
      Target_Decl : Node_Id) return Boolean
   is
      Dummy     : Node_Id;
      Inst_Body : Node_Id;
      Inst_Decl : Node_Id;

   begin
      --  Performance note: parent traversal

      Inst_Decl := Find_Enclosing_Instance (Target_Decl);

      --  The target declaration appears within an instance spec. Visibility is
      --  ignored because internally generated primitives for private types may
      --  reside in the private declarations and still be invoked from outside.

      if Present (Inst_Decl)
        and then Nkind (Inst_Decl) = N_Package_Declaration
      then
         --  The scenario comes from the main unit and the instance does not

         if In_Extended_Main_Code_Unit (N)
           and then not In_Extended_Main_Code_Unit (Inst_Decl)
         then
            return True;

         --  Otherwise the scenario must not appear within the instance spec or
         --  body.

         else
            Extract_Instance_Attributes
              (Exp_Inst  => Inst_Decl,
               Inst_Body => Inst_Body,
               Inst_Decl => Dummy);

            --  Performance note: parent traversal

            return not In_Subtree
                         (N     => N,
                          Root1 => Inst_Decl,
                          Root2 => Inst_Body);
         end if;
      end if;

      return False;
   end In_External_Instance;

   ---------------------
   -- In_Main_Context --
   ---------------------

   function In_Main_Context (N : Node_Id) return Boolean is
   begin
      --  Scenarios outside the main unit are not considered because the ALI
      --  information supplied to binde is for the main unit only.

      if not In_Extended_Main_Code_Unit (N) then
         return False;

      --  Scenarios within internal units are not considered unless switch
      --  -gnatdE (elaboration checks on predefined units) is in effect.

      elsif not Debug_Flag_EE and then In_Internal_Unit (N) then
         return False;
      end if;

      return True;
   end In_Main_Context;

   ---------------------
   -- In_Same_Context --
   ---------------------

   function In_Same_Context
     (N1        : Node_Id;
      N2        : Node_Id;
      Nested_OK : Boolean := False) return Boolean
   is
      function Find_Enclosing_Context (N : Node_Id) return Node_Id;
      --  Return the nearest enclosing non-library-level or compilation unit
      --  node which which encapsulates arbitrary node N. Return Empty is no
      --  such context is available.

      function In_Nested_Context
        (Outer : Node_Id;
         Inner : Node_Id) return Boolean;
      --  Determine whether arbitrary node Outer encapsulates arbitrary node
      --  Inner.

      ----------------------------
      -- Find_Enclosing_Context --
      ----------------------------

      function Find_Enclosing_Context (N : Node_Id) return Node_Id is
         Context : Node_Id;
         Par     : Node_Id;

      begin
         Par := Parent (N);
         while Present (Par) loop

            --  A traversal from a subunit continues via the corresponding stub

            if Nkind (Par) = N_Subunit then
               Par := Corresponding_Stub (Par);

            --  Stop the traversal when the nearest enclosing non-library-level
            --  encapsulator has been reached.

            elsif Is_Non_Library_Level_Encapsulator (Par) then
               Context := Parent (Par);

               --  The sole exception is when the encapsulator is the unit of
               --  compilation because this case requires special processing
               --  (see below).

               if Present (Context)
                 and then Nkind (Context) = N_Compilation_Unit
               then
                  null;

               else
                  return Par;
               end if;

            --  Reaching a compilation unit node without hitting a non-library-
            --  level encapsulator indicates that N is at the library level in
            --  which case the compilation unit is the context.

            elsif Nkind (Par) = N_Compilation_Unit then
               return Par;
            end if;

            Par := Parent (Par);
         end loop;

         return Empty;
      end Find_Enclosing_Context;

      -----------------------
      -- In_Nested_Context --
      -----------------------

      function In_Nested_Context
        (Outer : Node_Id;
         Inner : Node_Id) return Boolean
      is
         Par : Node_Id;

      begin
         Par := Inner;
         while Present (Par) loop

            --  A traversal from a subunit continues via the corresponding stub

            if Nkind (Par) = N_Subunit then
               Par := Corresponding_Stub (Par);

            elsif Par = Outer then
               return True;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Nested_Context;

      --  Local variables

      Context_1 : constant Node_Id := Find_Enclosing_Context (N1);
      Context_2 : constant Node_Id := Find_Enclosing_Context (N2);

   --  Start of processing for In_Same_Context

   begin
      --  Both nodes appear within the same context

      if Context_1 = Context_2 then
         return True;

      --  Both nodes appear in compilation units. Determine whether one unit
      --  is the body of the other.

      elsif Nkind (Context_1) = N_Compilation_Unit
        and then Nkind (Context_2) = N_Compilation_Unit
      then
         return
           Is_Same_Unit
             (Unit_1 => Defining_Entity (Unit (Context_1)),
              Unit_2 => Defining_Entity (Unit (Context_2)));

      --  The context of N1 encloses the context of N2

      elsif Nested_OK and then In_Nested_Context (Context_1, Context_2) then
         return True;
      end if;

      return False;
   end In_Same_Context;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Set the soft link which enables Atree.Rewrite to update a top-level
      --  scenario each time it is transformed into another node.

      Set_Rewriting_Proc (Update_Elaboration_Scenario'Access);
   end Initialize;

   ---------------
   -- Info_Call --
   ---------------

   procedure Info_Call
     (Call      : Node_Id;
      Target_Id : Entity_Id;
      Info_Msg  : Boolean;
      In_SPARK  : Boolean)
   is
      procedure Info_Accept_Alternative;
      pragma Inline (Info_Accept_Alternative);
      --  Output information concerning an accept alternative

      procedure Info_Simple_Call;
      pragma Inline (Info_Simple_Call);
      --  Output information concerning the call

      procedure Info_Type_Actions (Action : String);
      pragma Inline (Info_Type_Actions);
      --  Output information concerning action Action of a type

      procedure Info_Verification_Call
        (Pred    : String;
         Id      : Entity_Id;
         Id_Kind : String);
      pragma Inline (Info_Verification_Call);
      --  Output information concerning the verification of predicate Pred
      --  applied to related entity Id with kind Id_Kind.

      -----------------------------
      -- Info_Accept_Alternative --
      -----------------------------

      procedure Info_Accept_Alternative is
         Entry_Id : constant Entity_Id := Receiving_Entry (Target_Id);

      begin
         pragma Assert (Present (Entry_Id));

         Elab_Msg_NE
           (Msg      => "accept for entry & during elaboration",
            N        => Call,
            Id       => Entry_Id,
            Info_Msg => Info_Msg,
            In_SPARK => In_SPARK);
      end Info_Accept_Alternative;

      ----------------------
      -- Info_Simple_Call --
      ----------------------

      procedure Info_Simple_Call is
      begin
         Elab_Msg_NE
           (Msg      => "call to & during elaboration",
            N        => Call,
            Id       => Target_Id,
            Info_Msg => Info_Msg,
            In_SPARK => In_SPARK);
      end Info_Simple_Call;

      -----------------------
      -- Info_Type_Actions --
      -----------------------

      procedure Info_Type_Actions (Action : String) is
         Typ : constant Entity_Id := First_Formal_Type (Target_Id);

      begin
         pragma Assert (Present (Typ));

         Elab_Msg_NE
           (Msg      => Action & " actions for type & during elaboration",
            N        => Call,
            Id       => Typ,
            Info_Msg => Info_Msg,
            In_SPARK => In_SPARK);
      end Info_Type_Actions;

      ----------------------------
      -- Info_Verification_Call --
      ----------------------------

      procedure Info_Verification_Call
        (Pred    : String;
         Id      : Entity_Id;
         Id_Kind : String)
      is
      begin
         pragma Assert (Present (Id));

         Elab_Msg_NE
           (Msg      =>
              "verification of " & Pred & " of " & Id_Kind & " & during "
              & "elaboration",
            N        => Call,
            Id       => Id,
            Info_Msg => Info_Msg,
            In_SPARK => In_SPARK);
      end Info_Verification_Call;

   --  Start of processing for Info_Call

   begin
      --  Do not output anything for targets defined in internal units because
      --  this creates noise.

      if not In_Internal_Unit (Target_Id) then

         --  Accept alternative

         if Is_Accept_Alternative_Proc (Target_Id) then
            Info_Accept_Alternative;

         --  Adjustment

         elsif Is_TSS (Target_Id, TSS_Deep_Adjust) then
            Info_Type_Actions ("adjustment");

         --  Default_Initial_Condition

         elsif Is_Default_Initial_Condition_Proc (Target_Id) then
            Info_Verification_Call
              (Pred    => "Default_Initial_Condition",
               Id      => First_Formal_Type (Target_Id),
               Id_Kind => "type");

         --  Entries

         elsif Is_Protected_Entry (Target_Id) then
            Info_Simple_Call;

         --  Task entry calls are never processed because the entry being
         --  invoked does not have a corresponding "body", it has a select.

         elsif Is_Task_Entry (Target_Id) then
            null;

         --  Finalization

         elsif Is_TSS (Target_Id, TSS_Deep_Finalize) then
            Info_Type_Actions ("finalization");

         --  Calls to _Finalizer procedures must not appear in the output
         --  because this creates confusing noise.

         elsif Is_Finalizer_Proc (Target_Id) then
            null;

         --  Initial_Condition

         elsif Is_Initial_Condition_Proc (Target_Id) then
            Info_Verification_Call
              (Pred    => "Initial_Condition",
               Id      => Find_Enclosing_Scope (Call),
               Id_Kind => "package");

         --  Initialization

         elsif Is_Init_Proc (Target_Id)
           or else Is_TSS (Target_Id, TSS_Deep_Initialize)
         then
            Info_Type_Actions ("initialization");

         --  Invariant

         elsif Is_Invariant_Proc (Target_Id) then
            Info_Verification_Call
              (Pred    => "invariants",
               Id      => First_Formal_Type (Target_Id),
               Id_Kind => "type");

         --  Partial invariant calls must not appear in the output because this
         --  creates confusing noise.

         elsif Is_Partial_Invariant_Proc (Target_Id) then
            null;

         --  _Postconditions

         elsif Is_Postconditions_Proc (Target_Id) then
            Info_Verification_Call
              (Pred    => "postconditions",
               Id      => Find_Enclosing_Scope (Call),
               Id_Kind => "subprogram");

         --  Subprograms must come last because some of the previous cases fall
         --  under this category.

         elsif Ekind (Target_Id) = E_Function then
            Info_Simple_Call;

         elsif Ekind (Target_Id) = E_Procedure then
            Info_Simple_Call;

         else
            pragma Assert (False);
            null;
         end if;
      end if;
   end Info_Call;

   ------------------------
   -- Info_Instantiation --
   ------------------------

   procedure Info_Instantiation
     (Inst     : Node_Id;
      Gen_Id   : Entity_Id;
      Info_Msg : Boolean;
      In_SPARK : Boolean)
   is
   begin
      Elab_Msg_NE
        (Msg      => "instantiation of & during elaboration",
         N        => Inst,
         Id       => Gen_Id,
         Info_Msg => Info_Msg,
         In_SPARK => In_SPARK);
   end Info_Instantiation;

   -----------------------------
   -- Info_Variable_Reference --
   -----------------------------

   procedure Info_Variable_Reference
     (Ref      : Node_Id;
      Var_Id   : Entity_Id;
      Info_Msg : Boolean;
      In_SPARK : Boolean)
   is
   begin
      if Is_Read (Ref) then
         Elab_Msg_NE
           (Msg      => "read of variable & during elaboration",
            N        => Ref,
            Id       => Var_Id,
            Info_Msg => Info_Msg,
            In_SPARK => In_SPARK);
      end if;
   end Info_Variable_Reference;

   --------------------
   -- Insertion_Node --
   --------------------

   function Insertion_Node (N : Node_Id; Ins_Nod : Node_Id) return Node_Id is
   begin
      --  When the scenario denotes an instantiation, the proper insertion node
      --  is the instance spec. This ensures that the generic actuals will not
      --  be evaluated prior to a potential ABE.

      if Nkind (N) in N_Generic_Instantiation
        and then Present (Instance_Spec (N))
      then
         return Instance_Spec (N);

      --  Otherwise the proper insertion node is the candidate insertion node

      else
         return Ins_Nod;
      end if;
   end Insertion_Node;

   -----------------------
   -- Install_ABE_Check --
   -----------------------

   procedure Install_ABE_Check
     (N       : Node_Id;
      Id      : Entity_Id;
      Ins_Nod : Node_Id)
   is
      Check_Ins_Nod : constant Node_Id := Insertion_Node (N, Ins_Nod);
      --  Insert the check prior to this node

      Loc     : constant Source_Ptr := Sloc (N);
      Spec_Id : constant Entity_Id  := Unique_Entity (Id);
      Unit_Id : constant Entity_Id  := Find_Top_Unit (Id);
      Scop_Id : Entity_Id;

   begin
      --  Nothing to do when compiling for GNATprove because raise statements
      --  are not supported.

      if GNATprove_Mode then
         return;

      --  Nothing to do when the compilation will not produce an executable

      elsif Serious_Errors_Detected > 0 then
         return;

      --  Nothing to do for a compilation unit because there is no executable
      --  environment at that level.

      elsif Nkind (Parent (Check_Ins_Nod)) = N_Compilation_Unit then
         return;

      --  Nothing to do when the unit is elaborated prior to the main unit.
      --  This check must also consider the following cases:

      --  * Id's unit appears in the context of the main unit

      --  * Id's unit is subject to pragma Elaborate_Body. An ABE check MUST
      --    NOT be generated because Id's unit is always elaborated prior to
      --    the main unit.

      --  * Id's unit is the main unit. An ABE check MUST be generated in this
      --    case because a conditional ABE may be raised depending on the flow
      --    of execution within the main unit (flag Same_Unit_OK is False).

      elsif Has_Prior_Elaboration
              (Unit_Id      => Unit_Id,
               Context_OK   => True,
               Elab_Body_OK => True)
      then
         return;
      end if;

      --  Prevent multiple scenarios from installing the same ABE check

      Set_Is_Elaboration_Checks_OK_Node (N, False);

      --  Install the nearest enclosing scope of the scenario as there must be
      --  something on the scope stack.

      --  Performance note: parent traversal

      Scop_Id := Find_Enclosing_Scope (Check_Ins_Nod);
      pragma Assert (Present (Scop_Id));

      Push_Scope (Scop_Id);

      --  Generate:
      --    if not Spec_Id'Elaborated then
      --       raise Program_Error with "access before elaboration";
      --    end if;

      Insert_Action (Check_Ins_Nod,
        Make_Raise_Program_Error (Loc,
          Condition =>
            Make_Op_Not (Loc,
              Right_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix         => New_Occurrence_Of (Spec_Id, Loc),
                  Attribute_Name => Name_Elaborated)),
          Reason    => PE_Access_Before_Elaboration));

      Pop_Scope;
   end Install_ABE_Check;

   -----------------------
   -- Install_ABE_Check --
   -----------------------

   procedure Install_ABE_Check
     (N           : Node_Id;
      Target_Id   : Entity_Id;
      Target_Decl : Node_Id;
      Target_Body : Node_Id;
      Ins_Nod     : Node_Id)
   is
      procedure Build_Elaboration_Entity;
      pragma Inline (Build_Elaboration_Entity);
      --  Create a new elaboration flag for Target_Id, insert it prior to
      --  Target_Decl, and set it after Body_Decl.

      ------------------------------
      -- Build_Elaboration_Entity --
      ------------------------------

      procedure Build_Elaboration_Entity is
         Loc     : constant Source_Ptr := Sloc (Target_Id);
         Flag_Id : Entity_Id;

      begin
         --  Create the declaration of the elaboration flag. The name carries a
         --  unique counter in case of name overloading.

         Flag_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Target_Id), 'E', -1));

         Set_Elaboration_Entity          (Target_Id, Flag_Id);
         Set_Elaboration_Entity_Required (Target_Id);

         Push_Scope (Scope (Target_Id));

         --  Generate:
         --    Enn : Short_Integer := 0;

         Insert_Action (Target_Decl,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flag_Id,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Short_Integer, Loc),
             Expression          => Make_Integer_Literal (Loc, Uint_0)));

         --  Generate:
         --    Enn := 1;

         Set_Elaboration_Flag (Target_Body, Target_Id);

         Pop_Scope;
      end Build_Elaboration_Entity;

      --  Local variables

      Target_Unit_Id : constant Entity_Id := Find_Top_Unit (Target_Id);

   --  Start for processing for Install_ABE_Check

   begin
      --  Nothing to do when compiling for GNATprove because raise statements
      --  are not supported.

      if GNATprove_Mode then
         return;

      --  Nothing to do when the compilation will not produce an executable

      elsif Serious_Errors_Detected > 0 then
         return;

      --  Nothing to do when the target is a protected subprogram because the
      --  check is associated with the protected body subprogram.

      elsif Is_Protected_Subp (Target_Id) then
         return;

      --  Nothing to do when the target is elaborated prior to the main unit.
      --  This check must also consider the following cases:

      --  * The unit of the target appears in the context of the main unit

      --  * The unit of the target is subject to pragma Elaborate_Body. An ABE
      --    check MUST NOT be generated because the unit is always elaborated
      --    prior to the main unit.

      --  * The unit of the target is the main unit. An ABE check MUST be added
      --    in this case because a conditional ABE may be raised depending on
      --    the flow of execution within the main unit (flag Same_Unit_OK is
      --    False).

      elsif Has_Prior_Elaboration
              (Unit_Id      => Target_Unit_Id,
               Context_OK   => True,
               Elab_Body_OK => True)
      then
         return;

      --  Create an elaboration flag for the target when it does not have one

      elsif No (Elaboration_Entity (Target_Id)) then
         Build_Elaboration_Entity;
      end if;

      Install_ABE_Check
        (N       => N,
         Ins_Nod => Ins_Nod,
         Id      => Target_Id);
   end Install_ABE_Check;

   -------------------------
   -- Install_ABE_Failure --
   -------------------------

   procedure Install_ABE_Failure (N : Node_Id; Ins_Nod : Node_Id) is
      Fail_Ins_Nod : constant Node_Id := Insertion_Node (N, Ins_Nod);
      --  Insert the failure prior to this node

      Loc     : constant Source_Ptr := Sloc (N);
      Scop_Id : Entity_Id;

   begin
      --  Nothing to do when compiling for GNATprove because raise statements
      --  are not supported.

      if GNATprove_Mode then
         return;

      --  Nothing to do when the compilation will not produce an executable

      elsif Serious_Errors_Detected > 0 then
         return;

      --  Do not install an ABE check for a compilation unit because there is
      --  no executable environment at that level.

      elsif Nkind (Parent (Fail_Ins_Nod)) = N_Compilation_Unit then
         return;
      end if;

      --  Prevent multiple scenarios from installing the same ABE failure

      Set_Is_Elaboration_Checks_OK_Node (N, False);

      --  Install the nearest enclosing scope of the scenario as there must be
      --  something on the scope stack.

      --  Performance note: parent traversal

      Scop_Id := Find_Enclosing_Scope (Fail_Ins_Nod);
      pragma Assert (Present (Scop_Id));

      Push_Scope (Scop_Id);

      --  Generate:
      --    raise Program_Error with "access before elaboration";

      Insert_Action (Fail_Ins_Nod,
        Make_Raise_Program_Error (Loc,
          Reason => PE_Access_Before_Elaboration));

      Pop_Scope;
   end Install_ABE_Failure;

   --------------------------------
   -- Is_Accept_Alternative_Proc --
   --------------------------------

   function Is_Accept_Alternative_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote a procedure with a receiving entry

      return Ekind (Id) = E_Procedure and then Present (Receiving_Entry (Id));
   end Is_Accept_Alternative_Proc;

   ------------------------
   -- Is_Activation_Proc --
   ------------------------

   function Is_Activation_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote one of the runtime procedures in
      --  charge of task activation.

      if Ekind (Id) = E_Procedure then
         if Restricted_Profile then
            return Is_RTE (Id, RE_Activate_Restricted_Tasks);
         else
            return Is_RTE (Id, RE_Activate_Tasks);
         end if;
      end if;

      return False;
   end Is_Activation_Proc;

   ----------------------------
   -- Is_Ada_Semantic_Target --
   ----------------------------

   function Is_Ada_Semantic_Target (Id : Entity_Id) return Boolean is
   begin
      return
        Is_Activation_Proc (Id)
          or else Is_Controlled_Proc (Id, Name_Adjust)
          or else Is_Controlled_Proc (Id, Name_Finalize)
          or else Is_Controlled_Proc (Id, Name_Initialize)
          or else Is_Init_Proc (Id)
          or else Is_Invariant_Proc (Id)
          or else Is_Protected_Entry (Id)
          or else Is_Protected_Subp (Id)
          or else Is_Protected_Body_Subp (Id)
          or else Is_Task_Entry (Id);
   end Is_Ada_Semantic_Target;

   ----------------------------
   -- Is_Bodiless_Subprogram --
   ----------------------------

   function Is_Bodiless_Subprogram (Subp_Id : Entity_Id) return Boolean is
   begin
      --  An abstract subprogram does not have a body

      if Ekind_In (Subp_Id, E_Function,
                            E_Operator,
                            E_Procedure)
        and then Is_Abstract_Subprogram (Subp_Id)
      then
         return True;

      --  A formal subprogram does not have a body

      elsif Is_Formal_Subprogram (Subp_Id) then
         return True;

      --  An imported subprogram may have a body, however it is not known at
      --  compile or bind time where the body resides and whether it will be
      --  elaborated on time.

      elsif Is_Imported (Subp_Id) then
         return True;
      end if;

      return False;
   end Is_Bodiless_Subprogram;

   ------------------------
   -- Is_Controlled_Proc --
   ------------------------

   function Is_Controlled_Proc
     (Subp_Id  : Entity_Id;
      Subp_Nam : Name_Id) return Boolean
   is
      Formal_Id : Entity_Id;

   begin
      pragma Assert (Nam_In (Subp_Nam, Name_Adjust,
                                       Name_Finalize,
                                       Name_Initialize));

      --  To qualify, the subprogram must denote a source procedure with name
      --  Adjust, Finalize, or Initialize where the sole formal is controlled.

      if Comes_From_Source (Subp_Id)
        and then Ekind (Subp_Id) = E_Procedure
        and then Chars (Subp_Id) = Subp_Nam
      then
         Formal_Id := First_Formal (Subp_Id);

         return
           Present (Formal_Id)
             and then Is_Controlled (Etype (Formal_Id))
             and then No (Next_Formal (Formal_Id));
      end if;

      return False;
   end Is_Controlled_Proc;

   ---------------------------------------
   -- Is_Default_Initial_Condition_Proc --
   ---------------------------------------

   function Is_Default_Initial_Condition_Proc
     (Id : Entity_Id) return Boolean
   is
   begin
      --  To qualify, the entity must denote a Default_Initial_Condition
      --  procedure.

      return Ekind (Id) = E_Procedure and then Is_DIC_Procedure (Id);
   end Is_Default_Initial_Condition_Proc;

   -----------------------
   -- Is_Finalizer_Proc --
   -----------------------

   function Is_Finalizer_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote a _Finalizer procedure

      return Ekind (Id) = E_Procedure and then Chars (Id) = Name_uFinalizer;
   end Is_Finalizer_Proc;

   -----------------------
   -- Is_Guaranteed_ABE --
   -----------------------

   function Is_Guaranteed_ABE
     (N           : Node_Id;
      Target_Decl : Node_Id;
      Target_Body : Node_Id) return Boolean
   is
   begin
      --  Avoid cascaded errors if there were previous serious infractions.
      --  As a result the scenario will not be treated as a guaranteed ABE.
      --  This behaviour parallels that of the old ABE mechanism.

      if Serious_Errors_Detected > 0 then
         return False;

      --  The scenario and the target appear within the same context ignoring
      --  enclosing library levels.

      --  Performance note: parent traversal

      elsif In_Same_Context (N, Target_Decl) then

         --  The target body has already been encountered. The scenario results
         --  in a guaranteed ABE if it appears prior to the body.

         if Present (Target_Body) then
            return Earlier_In_Extended_Unit (N, Target_Body);

         --  Otherwise the body has not been encountered yet. The scenario is
         --  a guaranteed ABE since the body will appear later. It is assumed
         --  that the caller has already checked whether the scenario is ABE-
         --  safe as optional bodies are not considered here.

         else
            return True;
         end if;
      end if;

      return False;
   end Is_Guaranteed_ABE;

   -------------------------------
   -- Is_Initial_Condition_Proc --
   -------------------------------

   function Is_Initial_Condition_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote an Initial_Condition procedure

      return
        Ekind (Id) = E_Procedure and then Is_Initial_Condition_Procedure (Id);
   end Is_Initial_Condition_Proc;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized (Obj_Decl : Node_Id) return Boolean is
   begin
      --  To qualify, the object declaration must have an expression

      return
        Present (Expression (Obj_Decl)) or else Has_Init_Expression (Obj_Decl);
   end Is_Initialized;

   -----------------------
   -- Is_Invariant_Proc --
   -----------------------

   function Is_Invariant_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote the "full" invariant procedure

      return Ekind (Id) = E_Procedure and then Is_Invariant_Procedure (Id);
   end Is_Invariant_Proc;

   ---------------------------------------
   -- Is_Non_Library_Level_Encapsulator --
   ---------------------------------------

   function Is_Non_Library_Level_Encapsulator (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Abstract_Subprogram_Declaration
            | N_Aspect_Specification
            | N_Component_Declaration
            | N_Entry_Body
            | N_Entry_Declaration
            | N_Expression_Function
            | N_Formal_Abstract_Subprogram_Declaration
            | N_Formal_Concrete_Subprogram_Declaration
            | N_Formal_Object_Declaration
            | N_Formal_Package_Declaration
            | N_Formal_Type_Declaration
            | N_Generic_Association
            | N_Implicit_Label_Declaration
            | N_Incomplete_Type_Declaration
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration
            | N_Protected_Body
            | N_Protected_Type_Declaration
            | N_Single_Protected_Declaration
            | N_Single_Task_Declaration
            | N_Subprogram_Body
            | N_Subprogram_Declaration
            | N_Task_Body
            | N_Task_Type_Declaration
         =>
            return True;

         when others =>
            return Is_Generic_Declaration_Or_Body (N);
      end case;
   end Is_Non_Library_Level_Encapsulator;

   -------------------------------
   -- Is_Partial_Invariant_Proc --
   -------------------------------

   function Is_Partial_Invariant_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote the "partial" invariant procedure

      return
        Ekind (Id) = E_Procedure and then Is_Partial_Invariant_Procedure (Id);
   end Is_Partial_Invariant_Proc;

   ----------------------------
   -- Is_Postconditions_Proc --
   ----------------------------

   function Is_Postconditions_Proc (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote a _Postconditions procedure

      return
        Ekind (Id) = E_Procedure and then Chars (Id) = Name_uPostconditions;
   end Is_Postconditions_Proc;

   ---------------------------
   -- Is_Preelaborated_Unit --
   ---------------------------

   function Is_Preelaborated_Unit (Id : Entity_Id) return Boolean is
   begin
      return
        Is_Preelaborated (Id)
          or else Is_Pure (Id)
          or else Is_Remote_Call_Interface (Id)
          or else Is_Remote_Types (Id)
          or else Is_Shared_Passive (Id);
   end Is_Preelaborated_Unit;

   ------------------------
   -- Is_Protected_Entry --
   ------------------------

   function Is_Protected_Entry (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote an entry defined in a protected
      --  type.

      return
        Is_Entry (Id)
          and then Is_Protected_Type (Non_Private_View (Scope (Id)));
   end Is_Protected_Entry;

   -----------------------
   -- Is_Protected_Subp --
   -----------------------

   function Is_Protected_Subp (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote a subprogram defined within a
      --  protected type.

      return
        Ekind_In (Id, E_Function, E_Procedure)
          and then Is_Protected_Type (Non_Private_View (Scope (Id)));
   end Is_Protected_Subp;

   ----------------------------
   -- Is_Protected_Body_Subp --
   ----------------------------

   function Is_Protected_Body_Subp (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote a subprogram with attribute
      --  Protected_Subprogram set.

      return
        Ekind_In (Id, E_Function, E_Procedure)
          and then Present (Protected_Subprogram (Id));
   end Is_Protected_Body_Subp;

   --------------------------------
   -- Is_Recorded_SPARK_Scenario --
   --------------------------------

   function Is_Recorded_SPARK_Scenario (N : Node_Id) return Boolean is
   begin
      if Recorded_SPARK_Scenarios_In_Use then
         return Recorded_SPARK_Scenarios.Get (N);
      end if;

      return Recorded_SPARK_Scenarios_No_Element;
   end Is_Recorded_SPARK_Scenario;

   ------------------------------------
   -- Is_Recorded_Top_Level_Scenario --
   ------------------------------------

   function Is_Recorded_Top_Level_Scenario (N : Node_Id) return Boolean is
   begin
      if Recorded_Top_Level_Scenarios_In_Use then
         return Recorded_Top_Level_Scenarios.Get (N);
      end if;

      return Recorded_Top_Level_Scenarios_No_Element;
   end Is_Recorded_Top_Level_Scenario;

   ------------------------
   -- Is_Safe_Activation --
   ------------------------

   function Is_Safe_Activation
     (Call      : Node_Id;
      Task_Decl : Node_Id) return Boolean
   is
   begin
      --  The activation of a task coming from an external instance cannot
      --  cause an ABE because the generic was already instantiated. Note
      --  that the instantiation itself may lead to an ABE.

      return
        In_External_Instance
          (N           => Call,
           Target_Decl => Task_Decl);
   end Is_Safe_Activation;

   ------------------
   -- Is_Safe_Call --
   ------------------

   function Is_Safe_Call
     (Call         : Node_Id;
      Target_Attrs : Target_Attributes) return Boolean
   is
   begin
      --  The target is either an abstract subprogram, formal subprogram, or
      --  imported, in which case it does not have a body at compile or bind
      --  time. Assume that the call is ABE-safe.

      if Is_Bodiless_Subprogram (Target_Attrs.Spec_Id) then
         return True;

      --  The target is an instantiation of a generic subprogram. The call
      --  cannot cause an ABE because the generic was already instantiated.
      --  Note that the instantiation itself may lead to an ABE.

      elsif Is_Generic_Instance (Target_Attrs.Spec_Id) then
         return True;

      --  The invocation of a target coming from an external instance cannot
      --  cause an ABE because the generic was already instantiated. Note that
      --  the instantiation itself may lead to an ABE.

      elsif In_External_Instance
              (N           => Call,
               Target_Decl => Target_Attrs.Spec_Decl)
      then
         return True;

      --  The target is a subprogram body without a previous declaration. The
      --  call cannot cause an ABE because the body has already been seen.

      elsif Nkind (Target_Attrs.Spec_Decl) = N_Subprogram_Body
        and then No (Corresponding_Spec (Target_Attrs.Spec_Decl))
      then
         return True;

      --  The target is a subprogram body stub without a prior declaration.
      --  The call cannot cause an ABE because the proper body substitutes
      --  the stub.

      elsif Nkind (Target_Attrs.Spec_Decl) = N_Subprogram_Body_Stub
        and then No (Corresponding_Spec_Of_Stub (Target_Attrs.Spec_Decl))
      then
         return True;

      --  Subprogram bodies which wrap attribute references used as actuals
      --  in instantiations are always ABE-safe. These bodies are artifacts
      --  of expansion.

      elsif Present (Target_Attrs.Body_Decl)
        and then Nkind (Target_Attrs.Body_Decl) = N_Subprogram_Body
        and then Was_Attribute_Reference (Target_Attrs.Body_Decl)
      then
         return True;
      end if;

      return False;
   end Is_Safe_Call;

   ---------------------------
   -- Is_Safe_Instantiation --
   ---------------------------

   function Is_Safe_Instantiation
     (Inst      : Node_Id;
      Gen_Attrs : Target_Attributes) return Boolean
   is
   begin
      --  The generic is an intrinsic subprogram in which case it does not
      --  have a body at compile or bind time. Assume that the instantiation
      --  is ABE-safe.

      if Is_Bodiless_Subprogram (Gen_Attrs.Spec_Id) then
         return True;

      --  The instantiation of an external nested generic cannot cause an ABE
      --  if the outer generic was already instantiated. Note that the instance
      --  of the outer generic may lead to an ABE.

      elsif In_External_Instance
              (N           => Inst,
               Target_Decl => Gen_Attrs.Spec_Decl)
      then
         return True;

      --  The generic is a package. The instantiation cannot cause an ABE when
      --  the package has no body.

      elsif Ekind (Gen_Attrs.Spec_Id) = E_Generic_Package
        and then not Has_Body (Gen_Attrs.Spec_Decl)
      then
         return True;
      end if;

      return False;
   end Is_Safe_Instantiation;

   ------------------
   -- Is_Same_Unit --
   ------------------

   function Is_Same_Unit
     (Unit_1 : Entity_Id;
      Unit_2 : Entity_Id) return Boolean
   is
      function Is_Subunit (Unit_Id : Entity_Id) return Boolean;
      pragma Inline (Is_Subunit);
      --  Determine whether unit Unit_Id is a subunit

      function Normalize_Unit (Unit_Id : Entity_Id) return Entity_Id;
      --  Strip a potential subunit chain ending with unit Unit_Id and return
      --  the corresponding spec.

      ----------------
      -- Is_Subunit --
      ----------------

      function Is_Subunit (Unit_Id : Entity_Id) return Boolean is
      begin
         return Nkind (Parent (Unit_Declaration_Node (Unit_Id))) = N_Subunit;
      end Is_Subunit;

      --------------------
      -- Normalize_Unit --
      --------------------

      function Normalize_Unit (Unit_Id : Entity_Id) return Entity_Id is
         Result : Entity_Id;

      begin
         --  Eliminate a potential chain of subunits to reach to proper body

         Result := Unit_Id;
         while Present (Result)
           and then Result /= Standard_Standard
           and then Is_Subunit (Result)
         loop
            Result := Scope (Result);
         end loop;

         --  Obtain the entity of the corresponding spec (if any)

         return Unique_Entity (Result);
      end Normalize_Unit;

   --  Start of processing for Is_Same_Unit

   begin
      return Normalize_Unit (Unit_1) = Normalize_Unit (Unit_2);
   end Is_Same_Unit;

   -----------------
   -- Is_Scenario --
   -----------------

   function Is_Scenario (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Assignment_Statement
            | N_Attribute_Reference
            | N_Call_Marker
            | N_Entry_Call_Statement
            | N_Expanded_Name
            | N_Function_Call
            | N_Function_Instantiation
            | N_Identifier
            | N_Package_Instantiation
            | N_Procedure_Call_Statement
            | N_Procedure_Instantiation
            | N_Requeue_Statement
         =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Scenario;

   ------------------------------
   -- Is_SPARK_Semantic_Target --
   ------------------------------

   function Is_SPARK_Semantic_Target (Id : Entity_Id) return Boolean is
   begin
      return
        Is_Default_Initial_Condition_Proc (Id)
          or else Is_Initial_Condition_Proc (Id);
   end Is_SPARK_Semantic_Target;

   ------------------------
   -- Is_Suitable_Access --
   ------------------------

   function Is_Suitable_Access (N : Node_Id) return Boolean is
      Nam     : Name_Id;
      Pref    : Node_Id;
      Subp_Id : Entity_Id;

   begin
      --  This scenario is relevant only when the static model is in effect
      --  because it is graph-dependent and does not involve any run-time
      --  checks. Allowing it in the dynamic model would create confusing
      --  noise.

      if not Static_Elaboration_Checks then
         return False;

      --  Nothing to do when switch -gnatd.U (ignore 'Access) is in effect

      elsif Debug_Flag_Dot_UU then
         return False;

      --  Nothing to do when the scenario is not an attribute reference

      elsif Nkind (N) /= N_Attribute_Reference then
         return False;

      --  Nothing to do for internally-generated attributes because they are
      --  assumed to be ABE safe.

      elsif not Comes_From_Source (N) then
         return False;
      end if;

      Nam  := Attribute_Name (N);
      Pref := Prefix (N);

      --  Sanitize the prefix of the attribute

      if not Is_Entity_Name (Pref) then
         return False;

      elsif No (Entity (Pref)) then
         return False;
      end if;

      Subp_Id := Entity (Pref);

      if not Is_Subprogram_Or_Entry (Subp_Id) then
         return False;
      end if;

      --  Traverse a possible chain of renamings to obtain the original entry
      --  or subprogram which the prefix may rename.

      Subp_Id := Get_Renamed_Entity (Subp_Id);

      --  To qualify, the attribute must meet the following prerequisites:

      return

        --  The prefix must denote a source entry, operator, or subprogram
        --  which is not imported.

        Comes_From_Source (Subp_Id)
          and then Is_Subprogram_Or_Entry (Subp_Id)
          and then not Is_Bodiless_Subprogram (Subp_Id)

          --  The attribute name must be one of the 'Access forms. Note that
          --  'Unchecked_Access cannot apply to a subprogram.

          and then Nam_In (Nam, Name_Access, Name_Unrestricted_Access);
   end Is_Suitable_Access;

   ----------------------
   -- Is_Suitable_Call --
   ----------------------

   function Is_Suitable_Call (N : Node_Id) return Boolean is
   begin
      --  Entry and subprogram calls are intentionally ignored because they
      --  may undergo expansion depending on the compilation mode, previous
      --  errors, generic context, etc. Call markers play the role of calls
      --  and provide a uniform foundation for ABE processing.

      return Nkind (N) = N_Call_Marker;
   end Is_Suitable_Call;

   -------------------------------
   -- Is_Suitable_Instantiation --
   -------------------------------

   function Is_Suitable_Instantiation (N : Node_Id) return Boolean is
      Orig_N : constant Node_Id := Original_Node (N);
      --  Use the original node in case an instantiation library unit is
      --  rewritten as a package or subprogram.

   begin
      --  To qualify, the instantiation must come from source

      return
        Comes_From_Source (Orig_N)
          and then Nkind (Orig_N) in N_Generic_Instantiation;
   end Is_Suitable_Instantiation;

   --------------------------
   -- Is_Suitable_Scenario --
   --------------------------

   function Is_Suitable_Scenario (N : Node_Id) return Boolean is
   begin
      --  NOTE: Derived types and pragma Refined_State are intentionally left
      --  out because they are not executable during elaboration.

      return
        Is_Suitable_Access (N)
          or else Is_Suitable_Call (N)
          or else Is_Suitable_Instantiation (N)
          or else Is_Suitable_Variable_Assignment (N)
          or else Is_Suitable_Variable_Reference (N);
   end Is_Suitable_Scenario;

   ------------------------------------
   -- Is_Suitable_SPARK_Derived_Type --
   ------------------------------------

   function Is_Suitable_SPARK_Derived_Type (N : Node_Id) return Boolean is
      Prag : Node_Id;
      Typ  : Entity_Id;

   begin
      --  To qualify, the type declaration must denote a derived tagged type
      --  with primitive operations, subject to pragma SPARK_Mode On.

      if Nkind (N) = N_Full_Type_Declaration
        and then Nkind (Type_Definition (N)) = N_Derived_Type_Definition
      then
         Typ  := Defining_Entity (N);
         Prag := SPARK_Pragma (Typ);

         return
           Is_Tagged_Type (Typ)
             and then Has_Primitive_Operations (Typ)
             and then Present (Prag)
             and then Get_SPARK_Mode_From_Annotation (Prag) = On;
      end if;

      return False;
   end Is_Suitable_SPARK_Derived_Type;

   -------------------------------------
   -- Is_Suitable_SPARK_Instantiation --
   -------------------------------------

   function Is_Suitable_SPARK_Instantiation (N : Node_Id) return Boolean is
      Gen_Attrs  : Target_Attributes;
      Gen_Id     : Entity_Id;
      Inst       : Node_Id;
      Inst_Attrs : Instantiation_Attributes;
      Inst_Id    : Entity_Id;

   begin
      --  To qualify, both the instantiation and the generic must be subject to
      --  SPARK_Mode On.

      if Is_Suitable_Instantiation (N) then
         Extract_Instantiation_Attributes
           (Exp_Inst => N,
            Inst     => Inst,
            Inst_Id  => Inst_Id,
            Gen_Id   => Gen_Id,
            Attrs    => Inst_Attrs);

         Extract_Target_Attributes (Gen_Id, Gen_Attrs);

         return Inst_Attrs.SPARK_Mode_On and Gen_Attrs.SPARK_Mode_On;
      end if;

      return False;
   end Is_Suitable_SPARK_Instantiation;

   --------------------------------------------
   -- Is_Suitable_SPARK_Refined_State_Pragma --
   --------------------------------------------

   function Is_Suitable_SPARK_Refined_State_Pragma
     (N : Node_Id) return Boolean
   is
   begin
      --  To qualfy, the pragma must denote Refined_State

      return
        Nkind (N) = N_Pragma
          and then Pragma_Name (N) = Name_Refined_State;
   end Is_Suitable_SPARK_Refined_State_Pragma;

   -------------------------------------
   -- Is_Suitable_Variable_Assignment --
   -------------------------------------

   function Is_Suitable_Variable_Assignment (N : Node_Id) return Boolean is
      N_Unit      : Node_Id;
      N_Unit_Id   : Entity_Id;
      Nam         : Node_Id;
      Var_Decl    : Node_Id;
      Var_Id      : Entity_Id;
      Var_Unit    : Node_Id;
      Var_Unit_Id : Entity_Id;

   begin
      --  This scenario is relevant only when the static model is in effect
      --  because it is graph-dependent and does not involve any run-time
      --  checks. Allowing it in the dynamic model would create confusing
      --  noise.

      if not Static_Elaboration_Checks then
         return False;

      --  Nothing to do when the scenario is not an assignment

      elsif Nkind (N) /= N_Assignment_Statement then
         return False;

      --  Nothing to do for internally-generated assignments because they are
      --  assumed to be ABE safe.

      elsif not Comes_From_Source (N) then
         return False;

      --  Assignments are ignored in GNAT mode on the assumption that they are
      --  ABE-safe. This behaviour parallels that of the old ABE mechanism.

      elsif GNAT_Mode then
         return False;
      end if;

      Nam := Extract_Assignment_Name (N);

      --  Sanitize the left hand side of the assignment

      if not Is_Entity_Name (Nam) then
         return False;

      elsif No (Entity (Nam)) then
         return False;
      end if;

      Var_Id := Entity (Nam);

      --  Sanitize the variable

      if Var_Id = Any_Id then
         return False;

      elsif Ekind (Var_Id) /= E_Variable then
         return False;
      end if;

      Var_Decl := Declaration_Node (Var_Id);

      if Nkind (Var_Decl) /= N_Object_Declaration then
         return False;
      end if;

      N_Unit_Id := Find_Top_Unit (N);
      N_Unit    := Unit_Declaration_Node (N_Unit_Id);

      Var_Unit_Id := Find_Top_Unit (Var_Decl);
      Var_Unit    := Unit_Declaration_Node (Var_Unit_Id);

      --  To qualify, the assignment must meet the following prerequisites:

      return
        Comes_From_Source (Var_Id)

          --  The variable must be declared in the spec of compilation unit U

          and then Nkind (Var_Unit) = N_Package_Declaration

          --  Performance note: parent traversal

          and then Find_Enclosing_Level (Var_Decl) = Package_Spec

          --  The assignment must occur in the body of compilation unit U

          and then Nkind (N_Unit) = N_Package_Body
          and then Present (Corresponding_Body (Var_Unit))
          and then Corresponding_Body (Var_Unit) = N_Unit_Id;
   end Is_Suitable_Variable_Assignment;

   ------------------------------------
   -- Is_Suitable_Variable_Reference --
   ------------------------------------

   function Is_Suitable_Variable_Reference (N : Node_Id) return Boolean is
   begin
      --  Expanded names and identifiers are intentionally ignored because they
      --  be folded, optimized away, etc. Variable references markers play the
      --  role of variable references and provide a uniform foundation for ABE
      --  processing.

      return Nkind (N) = N_Variable_Reference_Marker;
   end Is_Suitable_Variable_Reference;

   -------------------
   -- Is_Task_Entry --
   -------------------

   function Is_Task_Entry (Id : Entity_Id) return Boolean is
   begin
      --  To qualify, the entity must denote an entry defined in a task type

      return
        Is_Entry (Id) and then Is_Task_Type (Non_Private_View (Scope (Id)));
   end Is_Task_Entry;

   ------------------------
   -- Is_Up_Level_Target --
   ------------------------

   function Is_Up_Level_Target (Target_Decl : Node_Id) return Boolean is
      Root : constant Node_Id := Root_Scenario;

   begin
      --  The root appears within the declaratons of a block statement, entry
      --  body, subprogram body, or task body ignoring enclosing packages. The
      --  root is always within the main unit. An up-level target is a notion
      --  applicable only to the static model because scenarios are reached by
      --  means of graph traversal started from a fixed declarative or library
      --  level.

      --  Performance note: parent traversal

      if Static_Elaboration_Checks
        and then Find_Enclosing_Level (Root) = Declaration_Level
      then
         --  The target is within the main unit. It acts as an up-level target
         --  when it appears within a context which encloses the root.

         --    package body Main_Unit is
         --       function Func ...;             --  target

         --       procedure Proc is
         --          X : ... := Func;            --  root scenario

         if In_Extended_Main_Code_Unit (Target_Decl) then

            --  Performance note: parent traversal

            return not In_Same_Context (Root, Target_Decl, Nested_OK => True);

         --  Otherwise the target is external to the main unit which makes it
         --  an up-level target.

         else
            return True;
         end if;
      end if;

      return False;
   end Is_Up_Level_Target;

   ---------------------
   -- Is_Visited_Body --
   ---------------------

   function Is_Visited_Body (Body_Decl : Node_Id) return Boolean is
   begin
      if Visited_Bodies_In_Use then
         return Visited_Bodies.Get (Body_Decl);
      end if;

      return Visited_Bodies_No_Element;
   end Is_Visited_Body;

   -------------------------------
   -- Kill_Elaboration_Scenario --
   -------------------------------

   procedure Kill_Elaboration_Scenario (N : Node_Id) is
      procedure Kill_SPARK_Scenario;
      pragma Inline (Kill_SPARK_Scenario);
      --  Eliminate scenario N from table SPARK_Scenarios if it is recorded
      --  there.

      procedure Kill_Top_Level_Scenario;
      pragma Inline (Kill_Top_Level_Scenario);
      --  Eliminate scenario N from table Top_Level_Scenarios if it is recorded
      --  there.

      -------------------------
      -- Kill_SPARK_Scenario --
      -------------------------

      procedure Kill_SPARK_Scenario is
         package Scenarios renames SPARK_Scenarios;

      begin
         if Is_Recorded_SPARK_Scenario (N) then

            --  Performance note: list traversal

            for Index in Scenarios.First .. Scenarios.Last loop
               if Scenarios.Table (Index) = N then
                  Scenarios.Table (Index) := Empty;

                  --  The SPARK scenario is no longer recorded

                  Set_Is_Recorded_SPARK_Scenario (N, False);
                  return;
               end if;
            end loop;

            --  A recorded SPARK scenario must be in the table of recorded
            --  SPARK scenarios.

            pragma Assert (False);
         end if;
      end Kill_SPARK_Scenario;

      -----------------------------
      -- Kill_Top_Level_Scenario --
      -----------------------------

      procedure Kill_Top_Level_Scenario is
         package Scenarios renames Top_Level_Scenarios;

      begin
         if Is_Recorded_Top_Level_Scenario (N) then

            --  Performance node: list traversal

            for Index in Scenarios.First .. Scenarios.Last loop
               if Scenarios.Table (Index) = N then
                  Scenarios.Table (Index) := Empty;

                  --  The top-level scenario is no longer recorded

                  Set_Is_Recorded_Top_Level_Scenario (N, False);
                  return;
               end if;
            end loop;

            --  A recorded top-level scenario must be in the table of recorded
            --  top-level scenarios.

            pragma Assert (False);
         end if;
      end Kill_Top_Level_Scenario;

   --  Start of processing for Kill_Elaboration_Scenario

   begin
      --  Eliminate a recorded scenario when it appears within dead code
      --  because it will not be executed at elaboration time.

      if Is_Scenario (N) then
         Kill_SPARK_Scenario;
         Kill_Top_Level_Scenario;
      end if;
   end Kill_Elaboration_Scenario;

   ----------------------------------
   -- Meet_Elaboration_Requirement --
   ----------------------------------

   procedure Meet_Elaboration_Requirement
     (N         : Node_Id;
      Target_Id : Entity_Id;
      Req_Nam   : Name_Id)
   is
      Main_Id : constant Entity_Id := Cunit_Entity (Main_Unit);
      Unit_Id : constant Entity_Id := Find_Top_Unit (Target_Id);

      function Find_Preelaboration_Pragma
        (Prag_Nam : Name_Id) return Node_Id;
      pragma Inline (Find_Preelaboration_Pragma);
      --  Traverse the visible declarations of unit Unit_Id and locate a source
      --  preelaboration-related pragma with name Prag_Nam.

      procedure Info_Requirement_Met (Prag : Node_Id);
      pragma Inline (Info_Requirement_Met);
      --  Output information concerning pragma Prag which meets requirement
      --  Req_Nam.

      procedure Info_Scenario;
      pragma Inline (Info_Scenario);
      --  Output information concerning scenario N

      --------------------------------
      -- Find_Preelaboration_Pragma --
      --------------------------------

      function Find_Preelaboration_Pragma
        (Prag_Nam : Name_Id) return Node_Id
      is
         Spec : constant Node_Id := Parent (Unit_Id);
         Decl : Node_Id;

      begin
         --  A preelaboration-related pragma comes from source and appears at
         --  the top of the visible declarations of a package.

         if Nkind (Spec) = N_Package_Specification then
            Decl := First (Visible_Declarations (Spec));
            while Present (Decl) loop
               if Comes_From_Source (Decl) then
                  if Nkind (Decl) = N_Pragma
                    and then Pragma_Name (Decl) = Prag_Nam
                  then
                     return Decl;

                  --  Otherwise the construct terminates the region where the
                  --  preelabortion-related pragma may appear.

                  else
                     exit;
                  end if;
               end if;

               Next (Decl);
            end loop;
         end if;

         return Empty;
      end Find_Preelaboration_Pragma;

      --------------------------
      -- Info_Requirement_Met --
      --------------------------

      procedure Info_Requirement_Met (Prag : Node_Id) is
      begin
         pragma Assert (Present (Prag));

         Error_Msg_Name_1 := Req_Nam;
         Error_Msg_Sloc   := Sloc (Prag);
         Error_Msg_NE
           ("\\% requirement for unit & met by pragma #", N, Unit_Id);
      end Info_Requirement_Met;

      -------------------
      -- Info_Scenario --
      -------------------

      procedure Info_Scenario is
      begin
         if Is_Suitable_Call (N) then
            Info_Call
              (Call      => N,
               Target_Id => Target_Id,
               Info_Msg  => False,
               In_SPARK  => True);

         elsif Is_Suitable_Instantiation (N) then
            Info_Instantiation
              (Inst     => N,
               Gen_Id   => Target_Id,
               Info_Msg => False,
               In_SPARK => True);

         elsif Is_Suitable_SPARK_Refined_State_Pragma (N) then
            Error_Msg_N
              ("read of refinement constituents during elaboration in SPARK",
               N);

         elsif Is_Suitable_Variable_Reference (N) then
            Info_Variable_Reference
              (Ref      => N,
               Var_Id   => Target_Id,
               Info_Msg => False,
               In_SPARK => True);

         --  No other scenario may impose a requirement on the context of the
         --  main unit.

         else
            pragma Assert (False);
            null;
         end if;
      end Info_Scenario;

      --  Local variables

      Elab_Attrs : Elaboration_Attributes;
      Elab_Nam   : Name_Id;
      Req_Met    : Boolean;

   --  Start of processing for Meet_Elaboration_Requirement

   begin
      pragma Assert (Nam_In (Req_Nam, Name_Elaborate, Name_Elaborate_All));

      --  Assume that the requirement has not been met

      Req_Met := False;

      --  Elaboration requirements are verified only when the static model is
      --  in effect because this diagnostic is graph-dependent.

      if not Static_Elaboration_Checks then
         return;

      --  If the target is within the main unit, either at the source level or
      --  through an instantiation, then there is no real requirement to meet
      --  because the main unit cannot force its own elaboration by means of an
      --  Elaborate[_All] pragma. Treat this case as valid coverage.

      elsif In_Extended_Main_Code_Unit (Target_Id) then
         Req_Met := True;

      --  Otherwise the target resides in an external unit

      --  The requirement is met when the target comes from an internal unit
      --  because such a unit is elaborated prior to a non-internal unit.

      elsif In_Internal_Unit (Unit_Id)
        and then not In_Internal_Unit (Main_Id)
      then
         Req_Met := True;

      --  The requirement is met when the target comes from a preelaborated
      --  unit. This portion must parallel predicate Is_Preelaborated_Unit.

      elsif Is_Preelaborated_Unit (Unit_Id) then
         Req_Met := True;

         --  Output extra information when switch -gnatel (info messages on
         --  implicit Elaborate[_All] pragmas.

         if Elab_Info_Messages then
            if Is_Preelaborated (Unit_Id) then
               Elab_Nam := Name_Preelaborate;

            elsif Is_Pure (Unit_Id) then
               Elab_Nam := Name_Pure;

            elsif Is_Remote_Call_Interface (Unit_Id) then
               Elab_Nam := Name_Remote_Call_Interface;

            elsif Is_Remote_Types (Unit_Id) then
               Elab_Nam := Name_Remote_Types;

            else
               pragma Assert (Is_Shared_Passive (Unit_Id));
               Elab_Nam := Name_Shared_Passive;
            end if;

            Info_Requirement_Met (Find_Preelaboration_Pragma (Elab_Nam));
         end if;

      --  Determine whether the context of the main unit has a pragma strong
      --  enough to meet the requirement.

      else
         Elab_Attrs := Elaboration_Status (Unit_Id);

         --  The pragma must be either Elaborate_All or be as strong as the
         --  requirement.

         if Present (Elab_Attrs.Source_Pragma)
           and then Nam_In (Pragma_Name (Elab_Attrs.Source_Pragma),
                            Name_Elaborate_All,
                            Req_Nam)
         then
            Req_Met := True;

            --  Output extra information when switch -gnatel (info messages on
            --  implicit Elaborate[_All] pragmas.

            if Elab_Info_Messages then
               Info_Requirement_Met (Elab_Attrs.Source_Pragma);
            end if;
         end if;
      end if;

      --  The requirement was not met by the context of the main unit, issue an
      --  error.

      if not Req_Met then
         Info_Scenario;

         Error_Msg_Name_1 := Req_Nam;
         Error_Msg_Node_2 := Unit_Id;
         Error_Msg_NE ("\\unit & requires pragma % for &", N, Main_Id);

         Output_Active_Scenarios (N);
      end if;
   end Meet_Elaboration_Requirement;

   ----------------------
   -- Non_Private_View --
   ----------------------

   function Non_Private_View (Typ : Entity_Id) return Entity_Id is
      Result : Entity_Id;

   begin
      Result := Typ;

      if Is_Private_Type (Result) and then Present (Full_View (Result)) then
         Result := Full_View (Result);
      end if;

      return Result;
   end Non_Private_View;

   -----------------------------
   -- Output_Active_Scenarios --
   -----------------------------

   procedure Output_Active_Scenarios (Error_Nod : Node_Id) is
      procedure Output_Access (N : Node_Id);
      --  Emit a specific diagnostic message for 'Access denote by N

      procedure Output_Activation_Call (N : Node_Id);
      --  Emit a specific diagnostic message for task activation N

      procedure Output_Call (N : Node_Id; Target_Id : Entity_Id);
      --  Emit a specific diagnostic message for call N which invokes target
      --  Target_Id.

      procedure Output_Header;
      --  Emit a specific diagnostic message for the unit of the root scenario

      procedure Output_Instantiation (N : Node_Id);
      --  Emit a specific diagnostic message for instantiation N

      procedure Output_SPARK_Refined_State_Pragma (N : Node_Id);
      --  Emit a specific diagnostic message for Refined_State pragma N

      procedure Output_Variable_Assignment (N : Node_Id);
      --  Emit a specific diagnostic message for assignment statement N

      procedure Output_Variable_Reference (N : Node_Id);
      --  Emit a specific diagnostic message for reference N which mentions a
      --  variable.

      -------------------
      -- Output_Access --
      -------------------

      procedure Output_Access (N : Node_Id) is
         Subp_Id : constant Entity_Id := Entity (Prefix (N));

      begin
         Error_Msg_Name_1 := Attribute_Name (N);
         Error_Msg_Sloc   := Sloc (N);
         Error_Msg_NE ("\\  % of & taken #", Error_Nod, Subp_Id);
      end Output_Access;

      ----------------------------
      -- Output_Activation_Call --
      ----------------------------

      procedure Output_Activation_Call (N : Node_Id) is
         function Find_Activator (Call : Node_Id) return Entity_Id;
         --  Find the nearest enclosing construct which houses call Call

         --------------------
         -- Find_Activator --
         --------------------

         function Find_Activator (Call : Node_Id) return Entity_Id is
            Par : Node_Id;

         begin
            --  Climb the parent chain looking for a package [body] or a
            --  construct with a statement sequence.

            Par := Parent (Call);
            while Present (Par) loop
               if Nkind_In (Par, N_Package_Body, N_Package_Declaration) then
                  return Defining_Entity (Par);

               elsif Nkind (Par) = N_Handled_Sequence_Of_Statements then
                  return Defining_Entity (Parent (Par));
               end if;

               Par := Parent (Par);
            end loop;

            return Empty;
         end Find_Activator;

         --  Local variables

         Activator : constant Entity_Id := Find_Activator (N);

      --  Start of processing for Output_Activation_Call

      begin
         pragma Assert (Present (Activator));

         Error_Msg_NE ("\\  local tasks of & activated", Error_Nod, Activator);
      end Output_Activation_Call;

      -----------------
      -- Output_Call --
      -----------------

      procedure Output_Call (N : Node_Id; Target_Id : Entity_Id) is
         procedure Output_Accept_Alternative;
         pragma Inline (Output_Accept_Alternative);
         --  Emit a specific diagnostic message concerning an accept
         --  alternative.

         procedure Output_Call (Kind : String);
         pragma Inline (Output_Call);
         --  Emit a specific diagnostic message concerning a call of kind Kind

         procedure Output_Type_Actions (Action : String);
         pragma Inline (Output_Type_Actions);
         --  Emit a specific diagnostic message concerning action Action of a
         --  type.

         procedure Output_Verification_Call
           (Pred    : String;
            Id      : Entity_Id;
            Id_Kind : String);
         pragma Inline (Output_Verification_Call);
         --  Emit a specific diagnostic message concerning the verification of
         --  predicate Pred applied to related entity Id with kind Id_Kind.

         -------------------------------
         -- Output_Accept_Alternative --
         -------------------------------

         procedure Output_Accept_Alternative is
            Entry_Id : constant Entity_Id := Receiving_Entry (Target_Id);

         begin
            pragma Assert (Present (Entry_Id));

            Error_Msg_NE ("\\  entry & selected #", Error_Nod, Entry_Id);
         end Output_Accept_Alternative;

         -----------------
         -- Output_Call --
         -----------------

         procedure Output_Call (Kind : String) is
         begin
            Error_Msg_NE ("\\  " & Kind & " & called #", Error_Nod, Target_Id);
         end Output_Call;

         -------------------------
         -- Output_Type_Actions --
         -------------------------

         procedure Output_Type_Actions (Action : String) is
            Typ : constant Entity_Id := First_Formal_Type (Target_Id);

         begin
            pragma Assert (Present (Typ));

            Error_Msg_NE
              ("\\  " & Action & " actions for type & #", Error_Nod, Typ);
         end Output_Type_Actions;

         ------------------------------
         -- Output_Verification_Call --
         ------------------------------

         procedure Output_Verification_Call
           (Pred    : String;
            Id      : Entity_Id;
            Id_Kind : String)
         is
         begin
            pragma Assert (Present (Id));

            Error_Msg_NE
              ("\\  " & Pred & " of " & Id_Kind & " & verified #",
               Error_Nod, Id);
         end Output_Verification_Call;

      --  Start of processing for Output_Call

      begin
         Error_Msg_Sloc := Sloc (N);

         --  Accept alternative

         if Is_Accept_Alternative_Proc (Target_Id) then
            Output_Accept_Alternative;

         --  Adjustment

         elsif Is_TSS (Target_Id, TSS_Deep_Adjust) then
            Output_Type_Actions ("adjustment");

         --  Default_Initial_Condition

         elsif Is_Default_Initial_Condition_Proc (Target_Id) then
            Output_Verification_Call
              (Pred    => "Default_Initial_Condition",
               Id      => First_Formal_Type (Target_Id),
               Id_Kind => "type");

         --  Entries

         elsif Is_Protected_Entry (Target_Id) then
            Output_Call ("entry");

         --  Task entry calls are never processed because the entry being
         --  invoked does not have a corresponding "body", it has a select. A
         --  task entry call appears in the stack of active scenarios for the
         --  sole purpose of checking No_Entry_Calls_In_Elaboration_Code and
         --  nothing more.

         elsif Is_Task_Entry (Target_Id) then
            null;

         --  Finalization

         elsif Is_TSS (Target_Id, TSS_Deep_Finalize) then
            Output_Type_Actions ("finalization");

         --  Calls to _Finalizer procedures must not appear in the output
         --  because this creates confusing noise.

         elsif Is_Finalizer_Proc (Target_Id) then
            null;

         --  Initial_Condition

         elsif Is_Initial_Condition_Proc (Target_Id) then
            Output_Verification_Call
              (Pred    => "Initial_Condition",
               Id      => Find_Enclosing_Scope (N),
               Id_Kind => "package");

         --  Initialization

         elsif Is_Init_Proc (Target_Id)
           or else Is_TSS (Target_Id, TSS_Deep_Initialize)
         then
            Output_Type_Actions ("initialization");

         --  Invariant

         elsif Is_Invariant_Proc (Target_Id) then
            Output_Verification_Call
              (Pred    => "invariants",
               Id      => First_Formal_Type (Target_Id),
               Id_Kind => "type");

         --  Partial invariant calls must not appear in the output because this
         --  creates confusing noise. Note that a partial invariant is always
         --  invoked by the "full" invariant which is already placed on the
         --  stack.

         elsif Is_Partial_Invariant_Proc (Target_Id) then
            null;

         --  _Postconditions

         elsif Is_Postconditions_Proc (Target_Id) then
            Output_Verification_Call
              (Pred    => "postconditions",
               Id      => Find_Enclosing_Scope (N),
               Id_Kind => "subprogram");

         --  Subprograms must come last because some of the previous cases fall
         --  under this category.

         elsif Ekind (Target_Id) = E_Function then
            Output_Call ("function");

         elsif Ekind (Target_Id) = E_Procedure then
            Output_Call ("procedure");

         else
            pragma Assert (False);
            null;
         end if;
      end Output_Call;

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header is
         Unit_Id : constant Entity_Id := Find_Top_Unit (Root_Scenario);

      begin
         if Ekind (Unit_Id) = E_Package then
            Error_Msg_NE ("\\  spec of unit & elaborated", Error_Nod, Unit_Id);

         elsif Ekind (Unit_Id) = E_Package_Body then
            Error_Msg_NE ("\\  body of unit & elaborated", Error_Nod, Unit_Id);

         else
            Error_Msg_NE ("\\  in body of unit &", Error_Nod, Unit_Id);
         end if;
      end Output_Header;

      --------------------------
      -- Output_Instantiation --
      --------------------------

      procedure Output_Instantiation (N : Node_Id) is
         procedure Output_Instantiation (Gen_Id : Entity_Id; Kind : String);
         pragma Inline (Output_Instantiation);
         --  Emit a specific diagnostic message concerning an instantiation of
         --  generic unit Gen_Id. Kind denotes the kind of the instantiation.

         --------------------------
         -- Output_Instantiation --
         --------------------------

         procedure Output_Instantiation (Gen_Id : Entity_Id; Kind : String) is
         begin
            Error_Msg_NE
              ("\\  " & Kind & " & instantiated as & #", Error_Nod, Gen_Id);
         end Output_Instantiation;

         --  Local variables

         Inst       : Node_Id;
         Inst_Attrs : Instantiation_Attributes;
         Inst_Id    : Entity_Id;
         Gen_Id     : Entity_Id;

      --  Start of processing for Output_Instantiation

      begin
         Extract_Instantiation_Attributes
           (Exp_Inst => N,
            Inst     => Inst,
            Inst_Id  => Inst_Id,
            Gen_Id   => Gen_Id,
            Attrs    => Inst_Attrs);

         Error_Msg_Node_2 := Inst_Id;
         Error_Msg_Sloc   := Sloc (Inst);

         if Nkind (Inst) = N_Function_Instantiation then
            Output_Instantiation (Gen_Id, "function");

         elsif Nkind (Inst) = N_Package_Instantiation then
            Output_Instantiation (Gen_Id, "package");

         elsif Nkind (Inst) = N_Procedure_Instantiation then
            Output_Instantiation (Gen_Id, "procedure");

         else
            pragma Assert (False);
            null;
         end if;
      end Output_Instantiation;

      ---------------------------------------
      -- Output_SPARK_Refined_State_Pragma --
      ---------------------------------------

      procedure Output_SPARK_Refined_State_Pragma (N : Node_Id) is
      begin
         Error_Msg_Sloc := Sloc (N);
         Error_Msg_N ("\\  refinement constituents read #", Error_Nod);
      end Output_SPARK_Refined_State_Pragma;

      --------------------------------
      -- Output_Variable_Assignment --
      --------------------------------

      procedure Output_Variable_Assignment (N : Node_Id) is
         Var_Id : constant Entity_Id := Entity (Extract_Assignment_Name (N));

      begin
         Error_Msg_Sloc := Sloc (N);
         Error_Msg_NE ("\\  variable & assigned #", Error_Nod, Var_Id);
      end Output_Variable_Assignment;

      -------------------------------
      -- Output_Variable_Reference --
      -------------------------------

      procedure Output_Variable_Reference (N : Node_Id) is
         Dummy  : Variable_Attributes;
         Var_Id : Entity_Id;

      begin
         Extract_Variable_Reference_Attributes
           (Ref    => N,
            Var_Id => Var_Id,
            Attrs  => Dummy);

         Error_Msg_Sloc := Sloc (N);

         if Is_Read (N) then
            Error_Msg_NE ("\\  variable & read #", Error_Nod, Var_Id);

         else
            pragma Assert (False);
            null;
         end if;
      end Output_Variable_Reference;

      --  Local variables

      package Stack renames Scenario_Stack;

      Dummy     : Call_Attributes;
      N         : Node_Id;
      Posted    : Boolean;
      Target_Id : Entity_Id;

   --  Start of processing for Output_Active_Scenarios

   begin
      --  Active scenarios are emitted only when the static model is in effect
      --  because there is an inherent order by which all these scenarios were
      --  reached from the declaration or library level.

      if not Static_Elaboration_Checks then
         return;
      end if;

      Posted := False;

      for Index in Stack.First .. Stack.Last loop
         N := Stack.Table (Index);

         if not Posted then
            Posted := True;
            Output_Header;
         end if;

         --  'Access

         if Nkind (N) = N_Attribute_Reference then
            Output_Access (N);

         --  Calls

         elsif Is_Suitable_Call (N) then
            Extract_Call_Attributes
              (Call      => N,
               Target_Id => Target_Id,
               Attrs     => Dummy);

            if Is_Activation_Proc (Target_Id) then
               Output_Activation_Call (N);
            else
               Output_Call (N, Target_Id);
            end if;

         --  Instantiations

         elsif Is_Suitable_Instantiation (N) then
            Output_Instantiation (N);

         --  Pragma Refined_State

         elsif Is_Suitable_SPARK_Refined_State_Pragma (N) then
            Output_SPARK_Refined_State_Pragma (N);

         --  Variable assignments

         elsif Nkind (N) = N_Assignment_Statement then
            Output_Variable_Assignment (N);

         --  Variable references

         elsif Is_Suitable_Variable_Reference (N) then
            Output_Variable_Reference (N);

         else
            pragma Assert (False);
            null;
         end if;
      end loop;
   end Output_Active_Scenarios;

   -------------------------
   -- Pop_Active_Scenario --
   -------------------------

   procedure Pop_Active_Scenario (N : Node_Id) is
      Top : Node_Id renames Scenario_Stack.Table (Scenario_Stack.Last);

   begin
      pragma Assert (Top = N);
      Scenario_Stack.Decrement_Last;
   end Pop_Active_Scenario;

   --------------------------------
   -- Process_Activation_Generic --
   --------------------------------

   procedure Process_Activation_Generic
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      procedure Process_Task_Object (Obj_Id : Entity_Id; Typ : Entity_Id);
      --  Perform ABE checks and diagnostics for object Obj_Id with type Typ.
      --  Typ may be a task type or a composite type with at least one task
      --  component.

      procedure Process_Task_Objects (List : List_Id);
      --  Perform ABE checks and diagnostics for all task objects found in
      --  the list List.

      -------------------------
      -- Process_Task_Object --
      -------------------------

      procedure Process_Task_Object (Obj_Id : Entity_Id; Typ : Entity_Id) is
         Base_Typ : constant Entity_Id := Base_Type (Typ);

         Comp_Id    : Entity_Id;
         Task_Attrs : Task_Attributes;

      begin
         if Is_Task_Type (Typ) then
            Extract_Task_Attributes
              (Typ   => Base_Typ,
               Attrs => Task_Attrs);

            Process_Single_Activation
              (Call           => Call,
               Call_Attrs     => Call_Attrs,
               Obj_Id         => Obj_Id,
               Task_Attrs     => Task_Attrs,
               In_Init_Cond   => In_Init_Cond,
               In_Partial_Fin => In_Partial_Fin,
               In_Task_Body   => In_Task_Body);

         --  Examine the component type when the object is an array

         elsif Is_Array_Type (Typ) and then Has_Task (Base_Typ) then
            Process_Task_Object (Obj_Id, Component_Type (Typ));

         --  Examine individual component types when the object is a record

         elsif Is_Record_Type (Typ) and then Has_Task (Base_Typ) then
            Comp_Id := First_Component (Typ);
            while Present (Comp_Id) loop
               Process_Task_Object (Obj_Id, Etype (Comp_Id));
               Next_Component (Comp_Id);
            end loop;
         end if;
      end Process_Task_Object;

      --------------------------
      -- Process_Task_Objects --
      --------------------------

      procedure Process_Task_Objects (List : List_Id) is
         Item     : Node_Id;
         Item_Id  : Entity_Id;
         Item_Typ : Entity_Id;

      begin
         --  Examine the contents of the list looking for an object declaration
         --  of a task type or one that contains a task within.

         Item := First (List);
         while Present (Item) loop
            if Nkind (Item) = N_Object_Declaration then
               Item_Id  := Defining_Entity (Item);
               Item_Typ := Etype (Item_Id);

               if Has_Task (Item_Typ) then
                  Process_Task_Object (Item_Id, Item_Typ);
               end if;
            end if;

            Next (Item);
         end loop;
      end Process_Task_Objects;

      --  Local variables

      Context : Node_Id;
      Spec    : Node_Id;

   --  Start of processing for Process_Activation_Generic

   begin
      --  Nothing to do when the activation is a guaranteed ABE

      if Is_Known_Guaranteed_ABE (Call) then
         return;
      end if;

      --  Find the proper context of the activation call where all task objects
      --  being activated are declared. This is usually the immediate parent of
      --  the call.

      Context := Parent (Call);

      --  In the case of package bodies, the activation call is in the handled
      --  sequence of statements, but the task objects are in the declaration
      --  list of the body.

      if Nkind (Context) = N_Handled_Sequence_Of_Statements
        and then Nkind (Parent (Context)) = N_Package_Body
      then
         Context := Parent (Context);
      end if;

      --  Process all task objects defined in both the spec and body when the
      --  activation call precedes the "begin" of a package body.

      if Nkind (Context) = N_Package_Body then
         Spec :=
           Specification
             (Unit_Declaration_Node (Corresponding_Spec (Context)));

         Process_Task_Objects (Visible_Declarations (Spec));
         Process_Task_Objects (Private_Declarations (Spec));
         Process_Task_Objects (Declarations (Context));

      --  Process all task objects defined in the spec when the activation call
      --  appears at the end of a package spec.

      elsif Nkind (Context) = N_Package_Specification then
         Process_Task_Objects (Visible_Declarations (Context));
         Process_Task_Objects (Private_Declarations (Context));

      --  Otherwise the context of the activation is some construct with a
      --  declarative part. Note that the corresponding record type of a task
      --  type is controlled. Because of this, the finalization machinery must
      --  relocate the task object to the handled statements of the construct
      --  to perform proper finalization in case of an exception. Examine the
      --  statements of the construct rather than the declarations.

      else
         pragma Assert (Nkind (Context) = N_Handled_Sequence_Of_Statements);

         Process_Task_Objects (Statements (Context));
      end if;
   end Process_Activation_Generic;

   ------------------------------------
   -- Process_Conditional_ABE_Access --
   ------------------------------------

   procedure Process_Conditional_ABE_Access
     (Attr           : Node_Id;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      function Build_Access_Marker (Target_Id : Entity_Id) return Node_Id;
      pragma Inline (Build_Access_Marker);
      --  Create a suitable call marker which invokes target Target_Id

      -------------------------
      -- Build_Access_Marker --
      -------------------------

      function Build_Access_Marker (Target_Id : Entity_Id) return Node_Id is
         Marker : Node_Id;

      begin
         Marker := Make_Call_Marker (Sloc (Attr));

         --  Inherit relevant attributes from the attribute

         --  Performance note: parent traversal

         Set_Target (Marker, Target_Id);
         Set_Is_Declaration_Level_Node
                    (Marker, Find_Enclosing_Level (Attr) = Declaration_Level);
         Set_Is_Dispatching_Call
                    (Marker, False);
         Set_Is_Elaboration_Checks_OK_Node
                    (Marker, Is_Elaboration_Checks_OK_Node (Attr));
         Set_Is_Source_Call
                    (Marker, Comes_From_Source (Attr));
         Set_Is_SPARK_Mode_On_Node
                    (Marker, Is_SPARK_Mode_On_Node (Attr));

         --  Partially insert the call marker into the tree by setting its
         --  parent pointer.

         Set_Parent (Marker, Attr);

         return Marker;
      end Build_Access_Marker;

      --  Local variables

      Root      : constant Node_Id   := Root_Scenario;
      Target_Id : constant Entity_Id := Entity (Prefix (Attr));

      Target_Attrs : Target_Attributes;

   --  Start of processing for Process_Conditional_ABE_Access

   begin
      --  Output relevant information when switch -gnatel (info messages on
      --  implicit Elaborate[_All] pragmas) is in effect.

      if Elab_Info_Messages then
         Error_Msg_NE
           ("info: access to & during elaboration", Attr, Target_Id);
      end if;

      Extract_Target_Attributes
        (Target_Id => Target_Id,
         Attrs     => Target_Attrs);

      --  Both the attribute and the corresponding body are in the same unit.
      --  The corresponding body must appear prior to the root scenario which
      --  started the recursive search. If this is not the case, then there is
      --  a potential ABE if the access value is used to call the subprogram.
      --  Emit a warning only when switch -gnatw.f (warnings on suspucious
      --  'Access) is in effect.

      if Warn_On_Elab_Access
        and then Present (Target_Attrs.Body_Decl)
        and then In_Extended_Main_Code_Unit (Target_Attrs.Body_Decl)
        and then Earlier_In_Extended_Unit (Root, Target_Attrs.Body_Decl)
      then
         Error_Msg_Name_1 := Attribute_Name (Attr);
         Error_Msg_NE ("??% attribute of & before body seen", Attr, Target_Id);
         Error_Msg_N ("\possible Program_Error on later references", Attr);

         Output_Active_Scenarios (Attr);
      end if;

      --  Treat the attribute as an immediate invocation of the target when
      --  switch -gnatd.o (conservative elaboration order for indirect calls)
      --  is in effect. Note that the prior elaboration of the unit containing
      --  the target is ensured processing the corresponding call marker.

      if Debug_Flag_Dot_O then
         Process_Conditional_ABE
           (N              => Build_Access_Marker (Target_Id),
            In_Init_Cond   => In_Init_Cond,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);

      --  Otherwise ensure that the unit with the corresponding body is
      --  elaborated prior to the main unit.

      else
         Ensure_Prior_Elaboration
           (N              => Attr,
            Unit_Id        => Target_Attrs.Unit_Id,
            Prag_Nam       => Name_Elaborate_All,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;
   end Process_Conditional_ABE_Access;

   ---------------------------------------------
   -- Process_Conditional_ABE_Activation_Impl --
   ---------------------------------------------

   procedure Process_Conditional_ABE_Activation_Impl
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Obj_Id         : Entity_Id;
      Task_Attrs     : Task_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      Check_OK : constant Boolean :=
                   not Is_Ignored_Ghost_Entity (Obj_Id)
                     and then not Task_Attrs.Ghost_Mode_Ignore
                     and then Is_Elaboration_Checks_OK_Id (Obj_Id)
                     and then Task_Attrs.Elab_Checks_OK;
      --  A run-time ABE check may be installed only when the object and the
      --  task type have active elaboration checks, and both are not ignored
      --  Ghost constructs.

      Root : constant Node_Id := Root_Scenario;

   begin
      --  Output relevant information when switch -gnatel (info messages on
      --  implicit Elaborate[_All] pragmas) is in effect.

      if Elab_Info_Messages then
         Error_Msg_NE
           ("info: activation of & during elaboration", Call, Obj_Id);
      end if;

      --  Nothing to do when the activation is a guaranteed ABE

      if Is_Known_Guaranteed_ABE (Call) then
         return;

      --  Nothing to do when the root scenario appears at the declaration
      --  level and the task is in the same unit, but outside this context.

      --    task type Task_Typ;                  --  task declaration

      --    procedure Proc is
      --       function A ... is
      --       begin
      --          if Some_Condition then
      --             declare
      --                T : Task_Typ;
      --             begin
      --                <activation call>        --  activation site
      --             end;
      --          ...
      --       end A;

      --       X : ... := A;                     --  root scenario
      --    ...

      --    task body Task_Typ is
      --       ...
      --    end Task_Typ;

      --  In the example above, the context of X is the declarative list of
      --  Proc. The "elaboration" of X may reach the activation of T whose body
      --  is defined outside of X's context. The task body is relevant only
      --  when Proc is invoked, but this happens only in "normal" elaboration,
      --  therefore the task body must not be considered if this is not the
      --  case.

      --  Performance note: parent traversal

      elsif Is_Up_Level_Target (Task_Attrs.Task_Decl) then
         return;

      --  Nothing to do when the activation is ABE-safe

      --    generic
      --    package Gen is
      --       task type Task_Typ;
      --    end Gen;

      --    package body Gen is
      --       task body Task_Typ is
      --       begin
      --          ...
      --       end Task_Typ;
      --    end Gen;

      --    with Gen;
      --    procedure Main is
      --       package Nested is
      --          ...
      --       end Nested;

      --       package body Nested is
      --          package Inst is new Gen;
      --          T : Inst.Task_Typ;
      --      [begin]
      --          <activation call>              --  safe activation
      --       end Nested;
      --    ...

      elsif Is_Safe_Activation (Call, Task_Attrs.Task_Decl) then

         --  Note that the task body must still be examined for any nested
         --  scenarios.

         null;

      --  The activation call and the task body are both in the main unit

      elsif Present (Task_Attrs.Body_Decl)
        and then In_Extended_Main_Code_Unit (Task_Attrs.Body_Decl)
      then
         --  If the root scenario appears prior to the task body, then this is
         --  a possible ABE with respect to the root scenario.

         --    task type Task_Typ;

         --    function A ... is
         --    begin
         --       if Some_Condition then
         --          declare
         --             package Pack is
         --                ...
         --             end Pack;

         --             package body Pack is
         --                T : Task_Typ;
         --            [begin]
         --                <activation call>     --  activation of T
         --             end Pack;
         --       ...
         --    end A;

         --    X : ... := A;                     --  root scenario

         --    task body Task_Typ is             --  task body
         --       ...
         --    end Task_Typ;

         --    Y : ... := A;                     --  root scenario

         --  IMPORTANT: The activation of T is a possible ABE for X, but
         --  not for Y. Intalling an unconditional ABE raise prior to the
         --  activation call would be wrong as it will fail for Y as well
         --  but in Y's case the activation of T is never an ABE.

         if Earlier_In_Extended_Unit (Root, Task_Attrs.Body_Decl) then

            --  Do not emit any ABE diagnostics when the activation occurs in
            --  a partial finalization context because this leads to confusing
            --  noise.

            if In_Partial_Fin then
               null;

            --  ABE diagnostics are emitted only in the static model because
            --  there is a well-defined order to visiting scenarios. Without
            --  this order diagnostics appear jumbled and result in unwanted
            --  noise.

            elsif Static_Elaboration_Checks
              and then Call_Attrs.Elab_Warnings_OK
            then
               Error_Msg_Sloc := Sloc (Call);
               Error_Msg_N
                 ("??task & will be activated # before elaboration of its "
                  & "body", Obj_Id);
               Error_Msg_N
                 ("\Program_Error may be raised at run time", Obj_Id);

               Output_Active_Scenarios (Obj_Id);
            end if;

            --  Install a conditional run-time ABE check to verify that the
            --  task body has been elaborated prior to the activation call.

            if Check_OK then
               Install_ABE_Check
                 (N           => Call,
                  Ins_Nod     => Call,
                  Target_Id   => Task_Attrs.Spec_Id,
                  Target_Decl => Task_Attrs.Task_Decl,
                  Target_Body => Task_Attrs.Body_Decl);
            end if;
         end if;

      --  Otherwise the task body is not available in this compilation or it
      --  resides in an external unit. Install a run-time ABE check to verify
      --  that the task body has been elaborated prior to the activation call
      --  when the dynamic model is in effect.

      elsif Dynamic_Elaboration_Checks and then Check_OK then
         Install_ABE_Check
           (N       => Call,
            Ins_Nod => Call,
            Id      => Task_Attrs.Unit_Id);
      end if;

      --  Both the activation call and task type are subject to SPARK_Mode
      --  On, this triggers the SPARK rules for task activation. Compared to
      --  calls and instantiations, task activation in SPARK does not require
      --  the presence of Elaborate[_All] pragmas in case the task type is
      --  defined outside the main unit. This is because SPARK utilizes a
      --  special policy which activates all tasks after the main unit has
      --  finished its elaboration.

      if Call_Attrs.SPARK_Mode_On and Task_Attrs.SPARK_Mode_On then
         null;

      --  Otherwise the Ada rules are in effect. Ensure that the unit with the
      --  task body is elaborated prior to the main unit.

      else
         Ensure_Prior_Elaboration
           (N              => Call,
            Unit_Id        => Task_Attrs.Unit_Id,
            Prag_Nam       => Name_Elaborate_All,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;

      Traverse_Body
        (N              => Task_Attrs.Body_Decl,
         In_Init_Cond   => In_Init_Cond,
         In_Partial_Fin => In_Partial_Fin,
         In_Task_Body   => True);
   end Process_Conditional_ABE_Activation_Impl;

   procedure Process_Conditional_ABE_Activation is
     new Process_Activation_Generic (Process_Conditional_ABE_Activation_Impl);

   ----------------------------------
   -- Process_Conditional_ABE_Call --
   ----------------------------------

   procedure Process_Conditional_ABE_Call
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Target_Id      : Entity_Id;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      function In_Initialization_Context (N : Node_Id) return Boolean;
      --  Determine whether arbitrary node N appears within a type init proc,
      --  primitive [Deep_]Initialize, or a block created for initialization
      --  purposes.

      function Is_Partial_Finalization_Proc return Boolean;
      pragma Inline (Is_Partial_Finalization_Proc);
      --  Determine whether call Call with target Target_Id invokes a partial
      --  finalization procedure.

      -------------------------------
      -- In_Initialization_Context --
      -------------------------------

      function In_Initialization_Context (N : Node_Id) return Boolean is
         Par     : Node_Id;
         Spec_Id : Entity_Id;

      begin
         --  Climb the parent chain looking for initialization actions

         Par := Parent (N);
         while Present (Par) loop

            --  A block may be part of the initialization actions of a default
            --  initialized object.

            if Nkind (Par) = N_Block_Statement
              and then Is_Initialization_Block (Par)
            then
               return True;

            --  A subprogram body may denote an initialization routine

            elsif Nkind (Par) = N_Subprogram_Body then
               Spec_Id := Unique_Defining_Entity (Par);

               --  The current subprogram body denotes a type init proc or
               --  primitive [Deep_]Initialize.

               if Is_Init_Proc (Spec_Id)
                 or else Is_Controlled_Proc (Spec_Id, Name_Initialize)
                 or else Is_TSS (Spec_Id, TSS_Deep_Initialize)
               then
                  return True;
               end if;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Initialization_Context;

      ----------------------------------
      -- Is_Partial_Finalization_Proc --
      ----------------------------------

      function Is_Partial_Finalization_Proc return Boolean is
      begin
         --  To qualify, the target must denote primitive [Deep_]Finalize or a
         --  finalizer procedure, and the call must appear in an initialization
         --  context.

         return
           (Is_Controlled_Proc (Target_Id, Name_Finalize)
              or else Is_Finalizer_Proc (Target_Id)
              or else Is_TSS (Target_Id, TSS_Deep_Finalize))
            and then In_Initialization_Context (Call);
      end Is_Partial_Finalization_Proc;

      --  Local variables

      Init_Cond_On   : Boolean;
      Partial_Fin_On : Boolean;
      SPARK_Rules_On : Boolean;
      Target_Attrs   : Target_Attributes;

   --  Start of processing for Process_Conditional_ABE_Call

   begin
      Extract_Target_Attributes
        (Target_Id => Target_Id,
         Attrs     => Target_Attrs);

      --  The call occurs in an initial condition context when a prior
      --  scenario is already in that mode, or when the target denotes
      --  an Initial_Condition procedure.

      Init_Cond_On :=
        In_Init_Cond or else Is_Initial_Condition_Proc (Target_Id);

      --  The call occurs in a partial finalization context when a prior
      --  scenario is already in that mode, or when the target denotes a
      --  [Deep_]Finalize primitive or a finalizer within an initialization
      --  context.

      Partial_Fin_On := In_Partial_Fin or else Is_Partial_Finalization_Proc;

      --  The SPARK rules are in effect when both the call and target are
      --  subject to SPARK_Mode On.

      SPARK_Rules_On :=
        Call_Attrs.SPARK_Mode_On and Target_Attrs.SPARK_Mode_On;

      --  Output relevant information when switch -gnatel (info messages on
      --  implicit Elaborate[_All] pragmas) is in effect.

      if Elab_Info_Messages then
         Info_Call
           (Call      => Call,
            Target_Id => Target_Id,
            Info_Msg  => True,
            In_SPARK  => SPARK_Rules_On);
      end if;

      --  Check whether the invocation of an entry clashes with an existing
      --  restriction.

      if Is_Protected_Entry (Target_Id) then
         Check_Restriction (No_Entry_Calls_In_Elaboration_Code, Call);

      elsif Is_Task_Entry (Target_Id) then
         Check_Restriction (No_Entry_Calls_In_Elaboration_Code, Call);

         --  Task entry calls are never processed because the entry being
         --  invoked does not have a corresponding "body", it has a select.

         return;
      end if;

      --  Nothing to do when the call is a guaranteed ABE

      if Is_Known_Guaranteed_ABE (Call) then
         return;

      --  Nothing to do when the root scenario appears at the declaration level
      --  and the target is in the same unit, but outside this context.

      --    function B ...;                      --  target declaration

      --    procedure Proc is
      --       function A ... is
      --       begin
      --          if Some_Condition then
      --             return B;                   --  call site
      --          ...
      --       end A;

      --       X : ... := A;                     --  root scenario
      --    ...

      --    function B ... is
      --       ...
      --    end B;

      --  In the example above, the context of X is the declarative region of
      --  Proc. The "elaboration" of X may eventually reach B which is defined
      --  outside of X's context. B is relevant only when Proc is invoked, but
      --  this happens only by means of "normal" elaboration, therefore B must
      --  not be considered if this is not the case.

      --  Performance note: parent traversal

      elsif Is_Up_Level_Target (Target_Attrs.Spec_Decl) then
         return;

      --  The SPARK rules are in effect. Note that -gnatd.v (enforce SPARK
      --  elaboration rules in SPARK code) is intentionally not taken into
      --  account here because Process_Conditional_ABE_Call_SPARK has two
      --  separate modes of operation.

      elsif SPARK_Rules_On then
         Process_Conditional_ABE_Call_SPARK
           (Call           => Call,
            Target_Id      => Target_Id,
            Target_Attrs   => Target_Attrs,
            In_Init_Cond   => Init_Cond_On,
            In_Partial_Fin => Partial_Fin_On,
            In_Task_Body   => In_Task_Body);

      --  Otherwise the Ada rules are in effect

      else
         Process_Conditional_ABE_Call_Ada
           (Call           => Call,
            Call_Attrs     => Call_Attrs,
            Target_Id      => Target_Id,
            Target_Attrs   => Target_Attrs,
            In_Partial_Fin => Partial_Fin_On,
            In_Task_Body   => In_Task_Body);
      end if;

      --  Inspect the target body (and barried function) for other suitable
      --  elaboration scenarios.

      Traverse_Body
        (N              => Target_Attrs.Body_Barf,
         In_Init_Cond   => Init_Cond_On,
         In_Partial_Fin => Partial_Fin_On,
         In_Task_Body   => In_Task_Body);

      Traverse_Body
        (N              => Target_Attrs.Body_Decl,
         In_Init_Cond   => Init_Cond_On,
         In_Partial_Fin => Partial_Fin_On,
         In_Task_Body   => In_Task_Body);
   end Process_Conditional_ABE_Call;

   --------------------------------------
   -- Process_Conditional_ABE_Call_Ada --
   --------------------------------------

   procedure Process_Conditional_ABE_Call_Ada
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Target_Id      : Entity_Id;
      Target_Attrs   : Target_Attributes;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      Check_OK : constant Boolean :=
                   not Call_Attrs.Ghost_Mode_Ignore
                     and then not Target_Attrs.Ghost_Mode_Ignore
                     and then Call_Attrs.Elab_Checks_OK
                     and then Target_Attrs.Elab_Checks_OK;
      --  A run-time ABE check may be installed only when both the call and the
      --  target have active elaboration checks, and both are not ignored Ghost
      --  constructs.

      Root : constant Node_Id := Root_Scenario;

   begin
      --  Nothing to do for an Ada dispatching call because there are no ABE
      --  diagnostics for either models. ABE checks for the dynamic model are
      --  handled by Install_Primitive_Elaboration_Check.

      if Call_Attrs.Is_Dispatching then
         return;

      --  Nothing to do when the call is ABE-safe

      --    generic
      --    function Gen ...;

      --    function Gen ... is
      --    begin
      --       ...
      --    end Gen;

      --    with Gen;
      --    procedure Main is
      --       function Inst is new Gen;
      --       X : ... := Inst;                  --  safe call
      --    ...

      elsif Is_Safe_Call (Call, Target_Attrs) then
         return;

      --  The call and the target body are both in the main unit

      elsif Present (Target_Attrs.Body_Decl)
        and then In_Extended_Main_Code_Unit (Target_Attrs.Body_Decl)
      then
         --  If the root scenario appears prior to the target body, then this
         --  is a possible ABE with respect to the root scenario.

         --    function B ...;

         --    function A ... is
         --    begin
         --       if Some_Condition then
         --          return B;                      --  call site
         --       ...
         --    end A;

         --    X : ... := A;                        --  root scenario

         --    function B ... is                    --  target body
         --       ...
         --    end B;

         --    Y : ... := A;                        --  root scenario

         --  IMPORTANT: The call to B from A is a possible ABE for X, but not
         --  for Y. Installing an unconditional ABE raise prior to the call to
         --  B would be wrong as it will fail for Y as well, but in Y's case
         --  the call to B is never an ABE.

         if Earlier_In_Extended_Unit (Root, Target_Attrs.Body_Decl) then

            --  Do not emit any ABE diagnostics when the call occurs in a
            --  partial finalization context because this leads to confusing
            --  noise.

            if In_Partial_Fin then
               null;

            --  ABE diagnostics are emitted only in the static model because
            --  there is a well-defined order to visiting scenarios. Without
            --  this order diagnostics appear jumbled and result in unwanted
            --  noise.

            elsif Static_Elaboration_Checks
              and then Call_Attrs.Elab_Warnings_OK
            then
               Error_Msg_NE
                 ("??cannot call & before body seen", Call, Target_Id);
               Error_Msg_N ("\Program_Error may be raised at run time", Call);

               Output_Active_Scenarios (Call);
            end if;

            --  Install a conditional run-time ABE check to verify that the
            --  target body has been elaborated prior to the call.

            if Check_OK then
               Install_ABE_Check
                 (N           => Call,
                  Ins_Nod     => Call,
                  Target_Id   => Target_Attrs.Spec_Id,
                  Target_Decl => Target_Attrs.Spec_Decl,
                  Target_Body => Target_Attrs.Body_Decl);
            end if;
         end if;

      --  Otherwise the target body is not available in this compilation or it
      --  resides in an external unit. Install a run-time ABE check to verify
      --  that the target body has been elaborated prior to the call site when
      --  the dynamic model is in effect.

      elsif Dynamic_Elaboration_Checks and then Check_OK then
         Install_ABE_Check
           (N       => Call,
            Ins_Nod => Call,
            Id      => Target_Attrs.Unit_Id);
      end if;

      --  Ensure that the unit with the target body is elaborated prior to the
      --  main unit. The implicit Elaborate[_All] is generated only when the
      --  call has elaboration checks enabled. This behaviour parallels that of
      --  the old ABE mechanism.

      if Call_Attrs.Elab_Checks_OK then
         Ensure_Prior_Elaboration
           (N              => Call,
            Unit_Id        => Target_Attrs.Unit_Id,
            Prag_Nam       => Name_Elaborate_All,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;
   end Process_Conditional_ABE_Call_Ada;

   ----------------------------------------
   -- Process_Conditional_ABE_Call_SPARK --
   ----------------------------------------

   procedure Process_Conditional_ABE_Call_SPARK
     (Call           : Node_Id;
      Target_Id      : Entity_Id;
      Target_Attrs   : Target_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      Region : Node_Id;

   begin
      --  The call and the target body are both in the main unit

      if Present (Target_Attrs.Body_Decl)
        and then In_Extended_Main_Code_Unit (Target_Attrs.Body_Decl)
      then
         --  If the call appears prior to the target body, then the call must
         --  appear within the early call region of the target body.

         --    function B ...;

         --    X : ... := B;                     --  call site

         --    <preelaborable construct 1>       --+
         --               ...                      | early call region
         --    <preelaborable construct N>       --+

         --    function B ... is                 --  target body
         --       ...
         --    end B;

         --  When the call to B is not nested within some other scenario, the
         --  call is automatically illegal because it can never appear in the
         --  early call region of B's body. This is equivalent to a guaranteed
         --  ABE.

         --    <preelaborable construct 1>       --+
         --                                        |
         --    function B ...;                     |
         --                                        |
         --    function A ... is                   |
         --    begin                               | early call region
         --       if Some_Condition then
         --          return B;                   --  call site
         --       ...
         --    end A;                              |
         --                                        |
         --    <preelaborable construct N>       --+

         --    function B ... is                 --  target body
         --       ...
         --    end B;

         --  When the call to B is nested within some other scenario, the call
         --  is always ABE-safe. It is not immediately obvious why this is the
         --  case. The elaboration safety follows from the early call region
         --  rule being applied to ALL calls preceding their associated bodies.

         --  In the example above, the call to B is safe as long as the call to
         --  A is safe. There are several cases to consider:

         --    <call 1 to A>
         --    function B ...;

         --    <call 2 to A>
         --    function A ... is
         --    begin
         --       if Some_Condition then
         --          return B;
         --       ...
         --    end A;

         --    <call 3 to A>
         --    function B ... is
         --       ...
         --    end B;

         --  * Call 1 - This call is either nested within some scenario or not,
         --    which falls under the two general cases outlined above.

         --  * Call 2 - This is the same case as Call 1.

         --  * Call 3 - The placement of this call limits the range of B's
         --    early call region unto call 3, therefore the call to B is no
         --    longer within the early call region of B's body, making it ABE-
         --    unsafe and therefore illegal.

         if Earlier_In_Extended_Unit (Call, Target_Attrs.Body_Decl) then

            --  Do not emit any ABE diagnostics when the call occurs in an
            --  initial condition context because this leads to incorrect
            --  diagnostics.

            if In_Init_Cond then
               null;

            --  Do not emit any ABE diagnostics when the call occurs in a
            --  partial finalization context because this leads to confusing
            --  noise.

            elsif In_Partial_Fin then
               null;

            --  ABE diagnostics are emitted only in the static model because
            --  there is a well-defined order to visiting scenarios. Without
            --  this order diagnostics appear jumbled and result in unwanted
            --  noise.

            elsif Static_Elaboration_Checks then

               --  Ensure that a call which textually precedes the subprogram
               --  body it invokes appears within the early call region of the
               --  subprogram body.

               --  IMPORTANT: This check must always be performed even when
               --  -gnatd.v (enforce SPARK elaboration rules in SPARK code) is
               --  not specified because the static model cannot guarantee the
               --  absence of elaboration issues in the presence of dispatching
               --  calls.

               Region := Find_Early_Call_Region (Target_Attrs.Body_Decl);

               if Earlier_In_Extended_Unit (Call, Region) then
                  Error_Msg_NE
                    ("call must appear within early call region of subprogram "
                     & "body & (SPARK RM 7.7(3))", Call, Target_Id);

                  Error_Msg_Sloc := Sloc (Region);
                  Error_Msg_N ("\region starts #", Call);

                  Error_Msg_Sloc := Sloc (Target_Attrs.Body_Decl);
                  Error_Msg_N ("\region ends #", Call);

                  Output_Active_Scenarios (Call);
               end if;
            end if;

         --  Otherwise the call appears after the target body. The call is
         --  ABE-safe as a consequence of applying the early call region rule
         --  to ALL calls preceding their associated bodies.

         else
            null;
         end if;
      end if;

      --  A call to a source target or to a target which emulates Ada or SPARK
      --  semantics imposes an Elaborate_All requirement on the context of the
      --  main unit. Determine whether the context has a pragma strong enough
      --  to meet the requirement.

      --  IMPORTANT: This check must be performed only when -gnatd.v (enforce
      --  SPARK elaboration rules in SPARK code) is active because the static
      --  model can ensure the prior elaboration of the unit which contains a
      --  body by installing an implicit Elaborate[_All] pragma.

      if Debug_Flag_Dot_V then
         if Target_Attrs.From_Source
           or else Is_Ada_Semantic_Target (Target_Id)
           or else Is_SPARK_Semantic_Target (Target_Id)
         then
            Meet_Elaboration_Requirement
              (N         => Call,
               Target_Id => Target_Id,
               Req_Nam   => Name_Elaborate_All);
         end if;

      --  Otherwise ensure that the unit with the target body is elaborated
      --  prior to the main unit.

      else
         Ensure_Prior_Elaboration
           (N              => Call,
            Unit_Id        => Target_Attrs.Unit_Id,
            Prag_Nam       => Name_Elaborate_All,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;
   end Process_Conditional_ABE_Call_SPARK;

   -------------------------------------------
   -- Process_Conditional_ABE_Instantiation --
   -------------------------------------------

   procedure Process_Conditional_ABE_Instantiation
     (Exp_Inst       : Node_Id;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      Gen_Attrs  : Target_Attributes;
      Gen_Id     : Entity_Id;
      Inst       : Node_Id;
      Inst_Attrs : Instantiation_Attributes;
      Inst_Id    : Entity_Id;

      SPARK_Rules_On : Boolean;
      --  This flag is set when the SPARK rules are in effect

   begin
      Extract_Instantiation_Attributes
        (Exp_Inst => Exp_Inst,
         Inst     => Inst,
         Inst_Id  => Inst_Id,
         Gen_Id   => Gen_Id,
         Attrs    => Inst_Attrs);

      Extract_Target_Attributes (Gen_Id, Gen_Attrs);

      --  The SPARK rules are in effect when both the instantiation and generic
      --  are subject to SPARK_Mode On.

      SPARK_Rules_On := Inst_Attrs.SPARK_Mode_On and Gen_Attrs.SPARK_Mode_On;

      --  Output relevant information when switch -gnatel (info messages on
      --  implicit Elaborate[_All] pragmas) is in effect.

      if Elab_Info_Messages then
         Info_Instantiation
           (Inst     => Inst,
            Gen_Id   => Gen_Id,
            Info_Msg => True,
            In_SPARK => SPARK_Rules_On);
      end if;

      --  Nothing to do when the instantiation is a guaranteed ABE

      if Is_Known_Guaranteed_ABE (Inst) then
         return;

      --  Nothing to do when the root scenario appears at the declaration level
      --  and the generic is in the same unit, but outside this context.

      --    generic
      --    procedure Gen is ...;                --  generic declaration

      --    procedure Proc is
      --       function A ... is
      --       begin
      --          if Some_Condition then
      --             declare
      --                procedure I is new Gen;  --  instantiation site
      --             ...
      --          ...
      --       end A;

      --       X : ... := A;                     --  root scenario
      --    ...

      --    procedure Gen is
      --       ...
      --    end Gen;

      --  In the example above, the context of X is the declarative region of
      --  Proc. The "elaboration" of X may eventually reach Gen which appears
      --  outside of X's context. Gen is relevant only when Proc is invoked,
      --  but this happens only by means of "normal" elaboration, therefore
      --  Gen must not be considered if this is not the case.

      --  Performance note: parent traversal

      elsif Is_Up_Level_Target (Gen_Attrs.Spec_Decl) then
         return;

      --  The SPARK rules are in effect

      elsif SPARK_Rules_On then
         Process_Conditional_ABE_Instantiation_SPARK
           (Inst           => Inst,
            Gen_Id         => Gen_Id,
            Gen_Attrs      => Gen_Attrs,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);

      --  Otherwise the Ada rules are in effect, or SPARK code is allowed to
      --  violate the SPARK rules.

      else
         Process_Conditional_ABE_Instantiation_Ada
           (Exp_Inst       => Exp_Inst,
            Inst           => Inst,
            Inst_Attrs     => Inst_Attrs,
            Gen_Id         => Gen_Id,
            Gen_Attrs      => Gen_Attrs,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;
   end Process_Conditional_ABE_Instantiation;

   -----------------------------------------------
   -- Process_Conditional_ABE_Instantiation_Ada --
   -----------------------------------------------

   procedure Process_Conditional_ABE_Instantiation_Ada
     (Exp_Inst       : Node_Id;
      Inst           : Node_Id;
      Inst_Attrs     : Instantiation_Attributes;
      Gen_Id         : Entity_Id;
      Gen_Attrs      : Target_Attributes;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      Check_OK : constant Boolean :=
                   not Inst_Attrs.Ghost_Mode_Ignore
                     and then not Gen_Attrs.Ghost_Mode_Ignore
                     and then Inst_Attrs.Elab_Checks_OK
                     and then Gen_Attrs.Elab_Checks_OK;
      --  A run-time ABE check may be installed only when both the instance and
      --  the generic have active elaboration checks and both are not ignored
      --  Ghost constructs.

      Root : constant Node_Id := Root_Scenario;

   begin
      --  Nothing to do when the instantiation is ABE-safe

      --    generic
      --    package Gen is
      --       ...
      --    end Gen;

      --    package body Gen is
      --       ...
      --    end Gen;

      --    with Gen;
      --    procedure Main is
      --       package Inst is new Gen (ABE);    --  safe instantiation
      --    ...

      if Is_Safe_Instantiation (Inst, Gen_Attrs) then
         return;

      --  The instantiation and the generic body are both in the main unit

      elsif Present (Gen_Attrs.Body_Decl)
        and then In_Extended_Main_Code_Unit (Gen_Attrs.Body_Decl)
      then
         --  If the root scenario appears prior to the generic body, then this
         --  is a possible ABE with respect to the root scenario.

         --    generic
         --    package Gen is
         --       ...
         --    end Gen;

         --    function A ... is
         --    begin
         --       if Some_Condition then
         --          declare
         --             package Inst is new Gen;    --  instantiation site
         --       ...
         --    end A;

         --    X : ... := A;                        --  root scenario

         --    package body Gen is                  --  generic body
         --       ...
         --    end Gen;

         --    Y : ... := A;                        --  root scenario

         --  IMPORTANT: The instantiation of Gen is a possible ABE for X, but
         --  not for Y. Installing an unconditional ABE raise prior to the
         --  instance site would be wrong as it will fail for Y as well, but in
         --  Y's case the instantiation of Gen is never an ABE.

         if Earlier_In_Extended_Unit (Root, Gen_Attrs.Body_Decl) then

            --  Do not emit any ABE diagnostics when the instantiation occurs
            --  in partial finalization context because this leads to unwanted
            --  noise.

            if In_Partial_Fin then
               null;

            --  ABE diagnostics are emitted only in the static model because
            --  there is a well-defined order to visiting scenarios. Without
            --  this order diagnostics appear jumbled and result in unwanted
            --  noise.

            elsif Static_Elaboration_Checks
              and then Inst_Attrs.Elab_Warnings_OK
            then
               Error_Msg_NE
                 ("??cannot instantiate & before body seen", Inst, Gen_Id);
               Error_Msg_N ("\Program_Error may be raised at run time", Inst);

               Output_Active_Scenarios (Inst);
            end if;

            --  Install a conditional run-time ABE check to verify that the
            --  generic body has been elaborated prior to the instantiation.

            if Check_OK then
               Install_ABE_Check
                 (N           => Inst,
                  Ins_Nod     => Exp_Inst,
                  Target_Id   => Gen_Attrs.Spec_Id,
                  Target_Decl => Gen_Attrs.Spec_Decl,
                  Target_Body => Gen_Attrs.Body_Decl);
            end if;
         end if;

      --  Otherwise the generic body is not available in this compilation or it
      --  resides in an external unit. Install a run-time ABE check to verify
      --  that the generic body has been elaborated prior to the instantiation
      --  when the dynamic model is in effect.

      elsif Dynamic_Elaboration_Checks and then Check_OK then
         Install_ABE_Check
           (N       => Inst,
            Ins_Nod => Exp_Inst,
            Id      => Gen_Attrs.Unit_Id);
      end if;

      --  Ensure that the unit with the generic body is elaborated prior to
      --  the main unit. No implicit pragma Elaborate is generated if the
      --  instantiation has elaboration checks suppressed. This behaviour
      --  parallels that of the old ABE mechanism.

      if Inst_Attrs.Elab_Checks_OK then
         Ensure_Prior_Elaboration
           (N              => Inst,
            Unit_Id        => Gen_Attrs.Unit_Id,
            Prag_Nam       => Name_Elaborate,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;
   end Process_Conditional_ABE_Instantiation_Ada;

   -------------------------------------------------
   -- Process_Conditional_ABE_Instantiation_SPARK --
   -------------------------------------------------

   procedure Process_Conditional_ABE_Instantiation_SPARK
     (Inst           : Node_Id;
      Gen_Id         : Entity_Id;
      Gen_Attrs      : Target_Attributes;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      Req_Nam : Name_Id;

   begin
      --  A source instantiation imposes an Elaborate[_All] requirement on the
      --  context of the main unit. Determine whether the context has a pragma
      --  strong enough to meet the requirement. The check is orthogonal to the
      --  ABE ramifications of the instantiation.

      --  IMPORTANT: This check must be performed only when -gnatd.v (enforce
      --  SPARK elaboration rules in SPARK code) is active because the static
      --  model can ensure the prior elaboration of the unit which contains a
      --  body by installing an implicit Elaborate[_All] pragma.

      if Debug_Flag_Dot_V then
         if Nkind (Inst) = N_Package_Instantiation then
            Req_Nam := Name_Elaborate_All;
         else
            Req_Nam := Name_Elaborate;
         end if;

         Meet_Elaboration_Requirement
           (N         => Inst,
            Target_Id => Gen_Id,
            Req_Nam   => Req_Nam);

      --  Otherwise ensure that the unit with the target body is elaborated
      --  prior to the main unit.

      else
         Ensure_Prior_Elaboration
           (N              => Inst,
            Unit_Id        => Gen_Attrs.Unit_Id,
            Prag_Nam       => Name_Elaborate,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);
      end if;
   end Process_Conditional_ABE_Instantiation_SPARK;

   -------------------------------------------------
   -- Process_Conditional_ABE_Variable_Assignment --
   -------------------------------------------------

   procedure Process_Conditional_ABE_Variable_Assignment (Asmt : Node_Id) is
      Var_Id : constant Entity_Id := Entity (Extract_Assignment_Name (Asmt));
      Prag   : constant Node_Id   := SPARK_Pragma (Var_Id);

      SPARK_Rules_On : Boolean;
      --  This flag is set when the SPARK rules are in effect

   begin
      --  The SPARK rules are in effect when both the assignment and the
      --  variable are subject to SPARK_Mode On.

      SPARK_Rules_On :=
        Present (Prag)
          and then Get_SPARK_Mode_From_Annotation (Prag) = On
          and then Is_SPARK_Mode_On_Node (Asmt);

      --  Output relevant information when switch -gnatel (info messages on
      --  implicit Elaborate[_All] pragmas) is in effect.

      if Elab_Info_Messages then
         Elab_Msg_NE
           (Msg      => "assignment to & during elaboration",
            N        => Asmt,
            Id       => Var_Id,
            Info_Msg => True,
            In_SPARK => SPARK_Rules_On);
      end if;

      --  The SPARK rules are in effect. These rules are applied regardless of
      --  whether -gnatd.v (enforce SPARK elaboration rules in SPARK code) is
      --  in effect because the static model cannot ensure safe assignment of
      --  variables.

      if SPARK_Rules_On then
         Process_Conditional_ABE_Variable_Assignment_SPARK
           (Asmt   => Asmt,
            Var_Id => Var_Id);

      --  Otherwise the Ada rules are in effect

      else
         Process_Conditional_ABE_Variable_Assignment_Ada
           (Asmt   => Asmt,
            Var_Id => Var_Id);
      end if;
   end Process_Conditional_ABE_Variable_Assignment;

   -----------------------------------------------------
   -- Process_Conditional_ABE_Variable_Assignment_Ada --
   -----------------------------------------------------

   procedure Process_Conditional_ABE_Variable_Assignment_Ada
     (Asmt   : Node_Id;
      Var_Id : Entity_Id)
   is
      Var_Decl : constant Node_Id   := Declaration_Node (Var_Id);
      Spec_Id  : constant Entity_Id := Find_Top_Unit (Var_Decl);

   begin
      --  Emit a warning when an uninitialized variable declared in a package
      --  spec without a pragma Elaborate_Body is initialized by elaboration
      --  code within the corresponding body.

      if not Warnings_Off (Var_Id)
        and then not Is_Initialized (Var_Decl)
        and then not Has_Pragma_Elaborate_Body (Spec_Id)
      then
         Error_Msg_NE
           ("??variable & can be accessed by clients before this "
            & "initialization", Asmt, Var_Id);

         Error_Msg_NE
           ("\add pragma ""Elaborate_Body"" to spec & to ensure proper "
            & "initialization", Asmt, Spec_Id);

         Output_Active_Scenarios (Asmt);

         --  Generate an implicit Elaborate_Body in the spec

         Set_Elaborate_Body_Desirable (Spec_Id);
      end if;
   end Process_Conditional_ABE_Variable_Assignment_Ada;

   -------------------------------------------------------
   -- Process_Conditional_ABE_Variable_Assignment_SPARK --
   -------------------------------------------------------

   procedure Process_Conditional_ABE_Variable_Assignment_SPARK
     (Asmt   : Node_Id;
      Var_Id : Entity_Id)
   is
      Var_Decl : constant Node_Id   := Declaration_Node (Var_Id);
      Spec_Id  : constant Entity_Id := Find_Top_Unit (Var_Decl);

   begin
      --  Emit an error when an initialized variable declared in a package spec
      --  without pragma Elaborate_Body is further modified by elaboration code
      --  within the corresponding body.

      if Is_Initialized (Var_Decl)
        and then not Has_Pragma_Elaborate_Body (Spec_Id)
      then
         Error_Msg_NE
           ("variable & modified by elaboration code in package body",
            Asmt, Var_Id);

         Error_Msg_NE
           ("\add pragma ""Elaborate_Body"" to spec & to ensure full "
            & "initialization", Asmt, Spec_Id);

         Output_Active_Scenarios (Asmt);
      end if;
   end Process_Conditional_ABE_Variable_Assignment_SPARK;

   ------------------------------------------------
   -- Process_Conditional_ABE_Variable_Reference --
   ------------------------------------------------

   procedure Process_Conditional_ABE_Variable_Reference (Ref : Node_Id) is
      Var_Attrs : Variable_Attributes;
      Var_Id    : Entity_Id;

   begin
      Extract_Variable_Reference_Attributes
        (Ref    => Ref,
         Var_Id => Var_Id,
         Attrs  => Var_Attrs);

      if Is_Read (Ref) then
         Process_Conditional_ABE_Variable_Reference_Read
           (Ref    => Ref,
            Var_Id => Var_Id,
            Attrs  => Var_Attrs);
      end if;
   end Process_Conditional_ABE_Variable_Reference;

   -----------------------------------------------------
   -- Process_Conditional_ABE_Variable_Reference_Read --
   -----------------------------------------------------

   procedure Process_Conditional_ABE_Variable_Reference_Read
     (Ref    : Node_Id;
      Var_Id : Entity_Id;
      Attrs  : Variable_Attributes)
   is
   begin
      --  Output relevant information when switch -gnatel (info messages on
      --  implicit Elaborate[_All] pragmas) is in effect.

      if Elab_Info_Messages then
         Elab_Msg_NE
           (Msg      => "read of variable & during elaboration",
            N        => Ref,
            Id       => Var_Id,
            Info_Msg => True,
            In_SPARK => True);
      end if;

      --  Nothing to do when the variable appears within the main unit because
      --  diagnostics on reads are relevant only for external variables.

      if Is_Same_Unit (Attrs.Unit_Id, Cunit_Entity (Main_Unit)) then
         null;

      --  Nothing to do when the variable is already initialized. Note that the
      --  variable may be further modified by the external unit.

      elsif Is_Initialized (Declaration_Node (Var_Id)) then
         null;

      --  Nothing to do when the external unit guarantees the initialization of
      --  the variable by means of pragma Elaborate_Body.

      elsif Has_Pragma_Elaborate_Body (Attrs.Unit_Id) then
         null;

      --  A variable read imposes an Elaborate requirement on the context of
      --  the main unit. Determine whether the context has a pragma strong
      --  enough to meet the requirement.

      else
         Meet_Elaboration_Requirement
           (N         => Ref,
            Target_Id => Var_Id,
            Req_Nam   => Name_Elaborate);
      end if;
   end Process_Conditional_ABE_Variable_Reference_Read;

   -----------------------------
   -- Process_Conditional_ABE --
   -----------------------------

   --  NOTE: The body of this routine is intentionally out of order because it
   --  invokes an instantiated subprogram (Process_Conditional_ABE_Activation).
   --  Placing the body in alphabetical order will result in a guaranteed ABE.

   procedure Process_Conditional_ABE
     (N              : Node_Id;
      In_Init_Cond   : Boolean := False;
      In_Partial_Fin : Boolean := False;
      In_Task_Body   : Boolean := False)
   is
      Call_Attrs : Call_Attributes;
      Target_Id  : Entity_Id;

   begin
      --  Add the current scenario to the stack of active scenarios

      Push_Active_Scenario (N);

      --  'Access

      if Is_Suitable_Access (N) then
         Process_Conditional_ABE_Access
           (Attr           => N,
            In_Init_Cond   => In_Init_Cond,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);

      --  Calls

      elsif Is_Suitable_Call (N) then

         --  In general, only calls found within the main unit are processed
         --  because the ALI information supplied to binde is for the main
         --  unit only. However, to preserve the consistency of the tree and
         --  ensure proper serialization of internal names, external calls
         --  also receive corresponding call markers (see Build_Call_Marker).
         --  Regardless of the reason, external calls must not be processed.

         if In_Main_Context (N) then
            Extract_Call_Attributes
              (Call      => N,
               Target_Id => Target_Id,
               Attrs     => Call_Attrs);

            if Is_Activation_Proc (Target_Id) then
               Process_Conditional_ABE_Activation
                 (Call           => N,
                  Call_Attrs     => Call_Attrs,
                  In_Init_Cond   => In_Init_Cond,
                  In_Partial_Fin => In_Partial_Fin,
                  In_Task_Body   => In_Task_Body);

            else
               Process_Conditional_ABE_Call
                 (Call           => N,
                  Call_Attrs     => Call_Attrs,
                  Target_Id      => Target_Id,
                  In_Init_Cond   => In_Init_Cond,
                  In_Partial_Fin => In_Partial_Fin,
                  In_Task_Body   => In_Task_Body);
            end if;
         end if;

      --  Instantiations

      elsif Is_Suitable_Instantiation (N) then
         Process_Conditional_ABE_Instantiation
           (Exp_Inst       => N,
            In_Partial_Fin => In_Partial_Fin,
            In_Task_Body   => In_Task_Body);

      --  Variable assignments

      elsif Is_Suitable_Variable_Assignment (N) then
         Process_Conditional_ABE_Variable_Assignment (N);

      --  Variable references

      elsif Is_Suitable_Variable_Reference (N) then

         --  In general, only variable references found within the main unit
         --  are processed because the ALI information supplied to binde is for
         --  the main unit only. However, to preserve the consistency of the
         --  tree and ensure proper serialization of internal names, external
         --  variable references also receive corresponding variable reference
         --  markers (see Build_Varaible_Reference_Marker). Regardless of the
         --  reason, external variable references must not be processed.

         if In_Main_Context (N) then
            Process_Conditional_ABE_Variable_Reference (N);
         end if;
      end if;

      --  Remove the current scenario from the stack of active scenarios once
      --  all ABE diagnostics and checks have been performed.

      Pop_Active_Scenario (N);
   end Process_Conditional_ABE;

   --------------------------------------------
   -- Process_Guaranteed_ABE_Activation_Impl --
   --------------------------------------------

   procedure Process_Guaranteed_ABE_Activation_Impl
     (Call           : Node_Id;
      Call_Attrs     : Call_Attributes;
      Obj_Id         : Entity_Id;
      Task_Attrs     : Task_Attributes;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      pragma Unreferenced (In_Init_Cond);
      pragma Unreferenced (In_Partial_Fin);
      pragma Unreferenced (In_Task_Body);

      Check_OK : constant Boolean :=
                   not Is_Ignored_Ghost_Entity (Obj_Id)
                     and then not Task_Attrs.Ghost_Mode_Ignore
                     and then Is_Elaboration_Checks_OK_Id (Obj_Id)
                     and then Task_Attrs.Elab_Checks_OK;
      --  A run-time ABE check may be installed only when the object and the
      --  task type have active elaboration checks, and both are not ignored
      --  Ghost constructs.

   begin
      --  Nothing to do when the root scenario appears at the declaration
      --  level and the task is in the same unit, but outside this context.

      --    task type Task_Typ;                  --  task declaration

      --    procedure Proc is
      --       function A ... is
      --       begin
      --          if Some_Condition then
      --             declare
      --                T : Task_Typ;
      --             begin
      --                <activation call>        --  activation site
      --             end;
      --          ...
      --       end A;

      --       X : ... := A;                     --  root scenario
      --    ...

      --    task body Task_Typ is
      --       ...
      --    end Task_Typ;

      --  In the example above, the context of X is the declarative list of
      --  Proc. The "elaboration" of X may reach the activation of T whose body
      --  is defined outside of X's context. The task body is relevant only
      --  when Proc is invoked, but this happens only in "normal" elaboration,
      --  therefore the task body must not be considered if this is not the
      --  case.

      --  Performance note: parent traversal

      if Is_Up_Level_Target (Task_Attrs.Task_Decl) then
         return;

      --  Nothing to do when the activation is ABE-safe

      --    generic
      --    package Gen is
      --       task type Task_Typ;
      --    end Gen;

      --    package body Gen is
      --       task body Task_Typ is
      --       begin
      --          ...
      --       end Task_Typ;
      --    end Gen;

      --    with Gen;
      --    procedure Main is
      --       package Nested is
      --          ...
      --       end Nested;

      --       package body Nested is
      --          package Inst is new Gen;
      --          T : Inst.Task_Typ;
      --      [begin]
      --          <activation call>              --  safe activation
      --       end Nested;
      --    ...

      elsif Is_Safe_Activation (Call, Task_Attrs.Task_Decl) then
         return;

      --  An activation call leads to a guaranteed ABE when the activation
      --  call and the task appear within the same context ignoring library
      --  levels, and the body of the task has not been seen yet or appears
      --  after the activation call.

      --    procedure Guaranteed_ABE is
      --       task type Task_Typ;

      --       package Nested is
      --          ...
      --       end Nested;

      --       package body Nested is
      --          T : Task_Typ;
      --      [begin]
      --          <activation call>              --  guaranteed ABE
      --       end Nested;

      --       task body Task_Typ is
      --          ...
      --       end Task_Typ;
      --    ...

      --  Performance note: parent traversal

      elsif Is_Guaranteed_ABE
              (N           => Call,
               Target_Decl => Task_Attrs.Task_Decl,
               Target_Body => Task_Attrs.Body_Decl)
      then
         if Call_Attrs.Elab_Warnings_OK then
            Error_Msg_Sloc := Sloc (Call);
            Error_Msg_N
              ("??task & will be activated # before elaboration of its body",
               Obj_Id);
            Error_Msg_N ("\Program_Error will be raised at run time", Obj_Id);
         end if;

         --  Mark the activation call as a guaranteed ABE

         Set_Is_Known_Guaranteed_ABE (Call);

         --  Install a run-time ABE failue because this activation call will
         --  always result in an ABE.

         if Check_OK then
            Install_ABE_Failure
              (N       => Call,
               Ins_Nod => Call);
         end if;
      end if;
   end Process_Guaranteed_ABE_Activation_Impl;

   procedure Process_Guaranteed_ABE_Activation is
     new Process_Activation_Generic (Process_Guaranteed_ABE_Activation_Impl);

   ---------------------------------
   -- Process_Guaranteed_ABE_Call --
   ---------------------------------

   procedure Process_Guaranteed_ABE_Call
     (Call       : Node_Id;
      Call_Attrs : Call_Attributes;
      Target_Id  : Entity_Id)
   is
      Target_Attrs : Target_Attributes;

   begin
      Extract_Target_Attributes
        (Target_Id => Target_Id,
         Attrs     => Target_Attrs);

      --  Nothing to do when the root scenario appears at the declaration level
      --  and the target is in the same unit, but outside this context.

      --    function B ...;                      --  target declaration

      --    procedure Proc is
      --       function A ... is
      --       begin
      --          if Some_Condition then
      --             return B;                   --  call site
      --          ...
      --       end A;

      --       X : ... := A;                     --  root scenario
      --    ...

      --    function B ... is
      --       ...
      --    end B;

      --  In the example above, the context of X is the declarative region of
      --  Proc. The "elaboration" of X may eventually reach B which is defined
      --  outside of X's context. B is relevant only when Proc is invoked, but
      --  this happens only by means of "normal" elaboration, therefore B must
      --  not be considered if this is not the case.

      --  Performance note: parent traversal

      if Is_Up_Level_Target (Target_Attrs.Spec_Decl) then
         return;

      --  Nothing to do when the call is ABE-safe

      --    generic
      --    function Gen ...;

      --    function Gen ... is
      --    begin
      --       ...
      --    end Gen;

      --    with Gen;
      --    procedure Main is
      --       function Inst is new Gen;
      --       X : ... := Inst;                  --  safe call
      --    ...

      elsif Is_Safe_Call (Call, Target_Attrs) then
         return;

      --  A call leads to a guaranteed ABE when the call and the target appear
      --  within the same context ignoring library levels, and the body of the
      --  target has not been seen yet or appears after the call.

      --    procedure Guaranteed_ABE is
      --       function Func ...;

      --       package Nested is
      --          Obj : ... := Func;             --  guaranteed ABE
      --       end Nested;

      --       function Func ... is
      --          ...
      --       end Func;
      --    ...

      --  Performance note: parent traversal

      elsif Is_Guaranteed_ABE
              (N           => Call,
               Target_Decl => Target_Attrs.Spec_Decl,
               Target_Body => Target_Attrs.Body_Decl)
      then
         if Call_Attrs.Elab_Warnings_OK then
            Error_Msg_NE ("??cannot call & before body seen", Call, Target_Id);
            Error_Msg_N ("\Program_Error will be raised at run time", Call);
         end if;

         --  Mark the call as a guarnateed ABE

         Set_Is_Known_Guaranteed_ABE (Call);

         --  Install a run-time ABE failure because the call will always result
         --  in an ABE. The failure is installed when both the call and target
         --  have enabled elaboration checks, and both are not ignored Ghost
         --  constructs.

         if Call_Attrs.Elab_Checks_OK
           and then Target_Attrs.Elab_Checks_OK
           and then not Call_Attrs.Ghost_Mode_Ignore
           and then not Target_Attrs.Ghost_Mode_Ignore
         then
            Install_ABE_Failure
              (N       => Call,
               Ins_Nod => Call);
         end if;
      end if;
   end Process_Guaranteed_ABE_Call;

   ------------------------------------------
   -- Process_Guaranteed_ABE_Instantiation --
   ------------------------------------------

   procedure Process_Guaranteed_ABE_Instantiation (Exp_Inst : Node_Id) is
      Gen_Attrs  : Target_Attributes;
      Gen_Id     : Entity_Id;
      Inst       : Node_Id;
      Inst_Attrs : Instantiation_Attributes;
      Inst_Id    : Entity_Id;

   begin
      Extract_Instantiation_Attributes
        (Exp_Inst => Exp_Inst,
         Inst     => Inst,
         Inst_Id  => Inst_Id,
         Gen_Id   => Gen_Id,
         Attrs    => Inst_Attrs);

      Extract_Target_Attributes (Gen_Id, Gen_Attrs);

      --  Nothing to do when the root scenario appears at the declaration level
      --  and the generic is in the same unit, but outside this context.

      --    generic
      --    procedure Gen is ...;                --  generic declaration

      --    procedure Proc is
      --       function A ... is
      --       begin
      --          if Some_Condition then
      --             declare
      --                procedure I is new Gen;  --  instantiation site
      --             ...
      --          ...
      --       end A;

      --       X : ... := A;                     --  root scenario
      --    ...

      --    procedure Gen is
      --       ...
      --    end Gen;

      --  In the example above, the context of X is the declarative region of
      --  Proc. The "elaboration" of X may eventually reach Gen which appears
      --  outside of X's context. Gen is relevant only when Proc is invoked,
      --  but this happens only by means of "normal" elaboration, therefore
      --  Gen must not be considered if this is not the case.

      --  Performance note: parent traversal

      if Is_Up_Level_Target (Gen_Attrs.Spec_Decl) then
         return;

      --  Nothing to do when the instantiation is ABE-safe

      --    generic
      --    package Gen is
      --       ...
      --    end Gen;

      --    package body Gen is
      --       ...
      --    end Gen;

      --    with Gen;
      --    procedure Main is
      --       package Inst is new Gen (ABE);    --  safe instantiation
      --    ...

      elsif Is_Safe_Instantiation (Inst, Gen_Attrs) then
         return;

      --  An instantiation leads to a guaranteed ABE when the instantiation and
      --  the generic appear within the same context ignoring library levels,
      --  and the body of the generic has not been seen yet or appears after
      --  the instantiation.

      --    procedure Guaranteed_ABE is
      --       generic
      --       procedure Gen;

      --       package Nested is
      --          procedure Inst is new Gen;     --  guaranteed ABE
      --       end Nested;

      --       procedure Gen is
      --          ...
      --       end Gen;
      --    ...

      --  Performance note: parent traversal

      elsif Is_Guaranteed_ABE
              (N           => Inst,
               Target_Decl => Gen_Attrs.Spec_Decl,
               Target_Body => Gen_Attrs.Body_Decl)
      then
         if Inst_Attrs.Elab_Warnings_OK then
            Error_Msg_NE
              ("??cannot instantiate & before body seen", Inst, Gen_Id);
            Error_Msg_N ("\Program_Error will be raised at run time", Inst);
         end if;

         --  Mark the instantiation as a guarantee ABE. This automatically
         --  suppresses the instantiation of the generic body.

         Set_Is_Known_Guaranteed_ABE (Inst);

         --  Install a run-time ABE failure because the instantiation will
         --  always result in an ABE. The failure is installed when both the
         --  instance and the generic have enabled elaboration checks, and both
         --  are not ignored Ghost constructs.

         if Inst_Attrs.Elab_Checks_OK
           and then Gen_Attrs.Elab_Checks_OK
           and then not Inst_Attrs.Ghost_Mode_Ignore
           and then not Gen_Attrs.Ghost_Mode_Ignore
         then
            Install_ABE_Failure
              (N       => Inst,
               Ins_Nod => Exp_Inst);
         end if;
      end if;
   end Process_Guaranteed_ABE_Instantiation;

   ----------------------------
   -- Process_Guaranteed_ABE --
   ----------------------------

   --  NOTE: The body of this routine is intentionally out of order because it
   --  invokes an instantiated subprogram (Process_Guaranteed_ABE_Activation).
   --  Placing the body in alphabetical order will result in a guaranteed ABE.

   procedure Process_Guaranteed_ABE (N : Node_Id) is
      Call_Attrs : Call_Attributes;
      Target_Id  : Entity_Id;

   begin
      --  Add the current scenario to the stack of active scenarios

      Push_Active_Scenario (N);

      --  Only calls, instantiations, and task activations may result in a
      --  guaranteed ABE.

      if Is_Suitable_Call (N) then
         Extract_Call_Attributes
           (Call      => N,
            Target_Id => Target_Id,
            Attrs     => Call_Attrs);

         if Is_Activation_Proc (Target_Id) then
            Process_Guaranteed_ABE_Activation
              (Call           => N,
               Call_Attrs     => Call_Attrs,
               In_Init_Cond   => False,
               In_Partial_Fin => False,
               In_Task_Body   => False);

         else
            Process_Guaranteed_ABE_Call
              (Call       => N,
               Call_Attrs => Call_Attrs,
               Target_Id  => Target_Id);
         end if;

      elsif Is_Suitable_Instantiation (N) then
         Process_Guaranteed_ABE_Instantiation (N);
      end if;

      --  Remove the current scenario from the stack of active scenarios once
      --  all ABE diagnostics and checks have been performed.

      Pop_Active_Scenario (N);
   end Process_Guaranteed_ABE;

   --------------------------
   -- Push_Active_Scenario --
   --------------------------

   procedure Push_Active_Scenario (N : Node_Id) is
   begin
      Scenario_Stack.Append (N);
   end Push_Active_Scenario;

   ---------------------------------
   -- Record_Elaboration_Scenario --
   ---------------------------------

   procedure Record_Elaboration_Scenario (N : Node_Id) is
      Level : Enclosing_Level_Kind;

      Any_Level_OK : Boolean;
      --  This flag is set when a particular scenario is allowed to appear at
      --  any level.

      Declaration_Level_OK : Boolean;
      --  This flag is set when a particular scenario is allowed to appear at
      --  the declaration level.

      Library_Level_OK : Boolean;
      --  This flag is set when a particular scenario is allowed to appear at
      --  the library level.

   begin
      --  Assume that the scenario cannot appear on any level

      Any_Level_OK         := False;
      Declaration_Level_OK := False;
      Library_Level_OK     := False;

      --  Nothing to do for ASIS. As a result, no ABE checks and diagnostics
      --  are performed in this mode.

      if ASIS_Mode then
         return;

      --  Nothing to do when the scenario is being preanalyzed

      elsif Preanalysis_Active then
         return;
      end if;

      --  Ensure that a library-level call does not appear in a preelaborated
      --  unit. The check must come before ignoring scenarios within external
      --  units or inside generics because calls in those context must also be
      --  verified.

      if Is_Suitable_Call (N) then
         Check_Preelaborated_Call (N);
      end if;

      --  Nothing to do when the scenario does not appear within the main unit

      if not In_Main_Context (N) then
         return;

      --  Scenarios within a generic unit are never considered because generics
      --  cannot be elaborated.

      elsif Inside_A_Generic then
         return;

      --  Scenarios which do not fall in one of the elaboration categories
      --  listed below are not considered. The categories are:

      --   'Access for entries, operators, and subprograms
      --    Assignments to variables
      --    Calls (includes task activation)
      --    Derived types
      --    Instantiations
      --    Pragma Refined_State
      --    Reads of variables

      elsif Is_Suitable_Access (N) then
         Library_Level_OK := True;

         --  Signal any enclosing local exception handlers that the 'Access may
         --  raise Program_Error due to a failed ABE check when switch -gnatd.o
         --  (conservative elaboration order for indirect calls) is in effect.
         --  Marking the exception handlers ensures proper expansion by both
         --  the front and back end restriction when No_Exception_Propagation
         --  is in effect.

         if Debug_Flag_Dot_O then
            Possible_Local_Raise (N, Standard_Program_Error);
         end if;

      elsif Is_Suitable_Call (N) or else Is_Suitable_Instantiation (N) then
         Declaration_Level_OK := True;
         Library_Level_OK     := True;

         --  Signal any enclosing local exception handlers that the call or
         --  instantiation may raise Program_Error due to a failed ABE check.
         --  Marking the exception handlers ensures proper expansion by both
         --  the front and back end restriction when No_Exception_Propagation
         --  is in effect.

         Possible_Local_Raise (N, Standard_Program_Error);

      elsif Is_Suitable_SPARK_Derived_Type (N) then
         Any_Level_OK := True;

      elsif Is_Suitable_SPARK_Refined_State_Pragma (N) then
         Library_Level_OK := True;

      elsif Is_Suitable_Variable_Assignment (N)
        or else Is_Suitable_Variable_Reference (N)
      then
         Library_Level_OK := True;

      --  Otherwise the input does not denote a suitable scenario

      else
         return;
      end if;

      --  The static model imposes additional restrictions on the placement of
      --  scenarios. In contrast, the dynamic model assumes that every scenario
      --  will be elaborated or invoked at some point.

      if Static_Elaboration_Checks then

         --  Certain scenarios are allowed to appear at any level. This check
         --  is performed here in order to save on a parent traversal.

         if Any_Level_OK then
            null;

         --  Otherwise the scenario must appear at a specific level

         else
            --  Performance note: parent traversal

            Level := Find_Enclosing_Level (N);

            --  Declaration-level scenario

            if Declaration_Level_OK and then Level = Declaration_Level then
               null;

            --  Library-level scenario

            elsif Library_Level_OK
              and then Level in Library_Or_Instantiation_Level
            then
               null;

            --  Otherwise the scenario does not appear at the proper level and
            --  cannot possibly act as a top-level scenario.

            else
               return;
            end if;
         end if;
      end if;

      --  Derived types subject to SPARK_Mode On require elaboration-related
      --  checks even though the type may not be declared within elaboration
      --  code. The types are recorded in a separate table which is examined
      --  during the Processing phase. Note that the checks must be delayed
      --  because the bodies of overriding primitives are not available yet.

      if Is_Suitable_SPARK_Derived_Type (N) then
         Record_SPARK_Elaboration_Scenario (N);

         --  Nothing left to do for derived types

         return;

      --  Instantiations of generics both subject to SPARK_Mode On require
      --  elaboration-related checks even though the instantiations may not
      --  appear within elaboration code. The instantiations are recored in
      --  a separate table which is examined during the Procesing phase. Note
      --  that the checks must be delayed because it is not known yet whether
      --  the generic unit has a body or not.

      --  IMPORTANT: A SPARK instantiation is also a normal instantiation which
      --  is subject to common conditional and guaranteed ABE checks.

      elsif Is_Suitable_SPARK_Instantiation (N) then
         Record_SPARK_Elaboration_Scenario (N);

      --  External constituents that refine abstract states which appear in
      --  pragma Initializes require elaboration-related checks even though
      --  a Refined_State pragma lacks any elaboration semantic.

      elsif Is_Suitable_SPARK_Refined_State_Pragma (N) then
         Record_SPARK_Elaboration_Scenario (N);

         --  Nothing left to do for pragma Refined_State

         return;
      end if;

      --  Perform early detection of guaranteed ABEs in order to suppress the
      --  instantiation of generic bodies as gigi cannot handle certain types
      --  of premature instantiations.

      Process_Guaranteed_ABE (N);

      --  At this point all checks have been performed. Record the scenario for
      --  later processing by the ABE phase.

      Top_Level_Scenarios.Append (N);
      Set_Is_Recorded_Top_Level_Scenario (N);
   end Record_Elaboration_Scenario;

   ---------------------------------------
   -- Record_SPARK_Elaboration_Scenario --
   ---------------------------------------

   procedure Record_SPARK_Elaboration_Scenario (N : Node_Id) is
   begin
      SPARK_Scenarios.Append (N);
      Set_Is_Recorded_SPARK_Scenario (N);
   end Record_SPARK_Elaboration_Scenario;

   -----------------------------------
   -- Recorded_SPARK_Scenarios_Hash --
   -----------------------------------

   function Recorded_SPARK_Scenarios_Hash
     (Key : Node_Id) return Recorded_SPARK_Scenarios_Index
   is
   begin
      return
        Recorded_SPARK_Scenarios_Index (Key mod Recorded_SPARK_Scenarios_Max);
   end Recorded_SPARK_Scenarios_Hash;

   ---------------------------------------
   -- Recorded_Top_Level_Scenarios_Hash --
   ---------------------------------------

   function Recorded_Top_Level_Scenarios_Hash
     (Key : Node_Id) return Recorded_Top_Level_Scenarios_Index
   is
   begin
      return
        Recorded_Top_Level_Scenarios_Index
          (Key mod Recorded_Top_Level_Scenarios_Max);
   end Recorded_Top_Level_Scenarios_Hash;

   --------------------------
   -- Reset_Visited_Bodies --
   --------------------------

   procedure Reset_Visited_Bodies is
   begin
      if Visited_Bodies_In_Use then
         Visited_Bodies_In_Use := False;
         Visited_Bodies.Reset;
      end if;
   end Reset_Visited_Bodies;

   -------------------
   -- Root_Scenario --
   -------------------

   function Root_Scenario return Node_Id is
      package Stack renames Scenario_Stack;

   begin
      --  Ensure that the scenario stack has at least one active scenario in
      --  it. The one at the bottom (index First) is the root scenario.

      pragma Assert (Stack.Last >= Stack.First);
      return Stack.Table (Stack.First);
   end Root_Scenario;

   ---------------------------
   -- Set_Early_Call_Region --
   ---------------------------

   procedure Set_Early_Call_Region (Body_Id : Entity_Id; Start : Node_Id) is
   begin
      pragma Assert (Ekind_In (Body_Id, E_Entry,
                                        E_Entry_Family,
                                        E_Function,
                                        E_Procedure,
                                        E_Subprogram_Body));

      Early_Call_Regions_In_Use := True;
      Early_Call_Regions.Set (Body_Id, Start);
   end Set_Early_Call_Region;

   ----------------------------
   -- Set_Elaboration_Status --
   ----------------------------

   procedure Set_Elaboration_Status
     (Unit_Id : Entity_Id;
      Val     : Elaboration_Attributes)
   is
   begin
      Elaboration_Statuses_In_Use := True;
      Elaboration_Statuses.Set (Unit_Id, Val);
   end Set_Elaboration_Status;

   ------------------------------------
   -- Set_Is_Recorded_SPARK_Scenario --
   ------------------------------------

   procedure Set_Is_Recorded_SPARK_Scenario
     (N   : Node_Id;
      Val : Boolean := True)
   is
   begin
      Recorded_SPARK_Scenarios_In_Use := True;
      Recorded_SPARK_Scenarios.Set (N, Val);
   end Set_Is_Recorded_SPARK_Scenario;

   ----------------------------------------
   -- Set_Is_Recorded_Top_Level_Scenario --
   ----------------------------------------

   procedure Set_Is_Recorded_Top_Level_Scenario
     (N   : Node_Id;
      Val : Boolean := True)
   is
   begin
      Recorded_Top_Level_Scenarios_In_Use := True;
      Recorded_Top_Level_Scenarios.Set (N, Val);
   end Set_Is_Recorded_Top_Level_Scenario;

   -------------------------
   -- Set_Is_Visited_Body --
   -------------------------

   procedure Set_Is_Visited_Body (Subp_Body : Node_Id) is
   begin
      Visited_Bodies_In_Use := True;
      Visited_Bodies.Set (Subp_Body, True);
   end Set_Is_Visited_Body;

   -------------------------------
   -- Static_Elaboration_Checks --
   -------------------------------

   function Static_Elaboration_Checks return Boolean is
   begin
      return not Dynamic_Elaboration_Checks;
   end Static_Elaboration_Checks;

   -------------------
   -- Traverse_Body --
   -------------------

   procedure Traverse_Body
     (N              : Node_Id;
      In_Init_Cond   : Boolean;
      In_Partial_Fin : Boolean;
      In_Task_Body   : Boolean)
   is
      procedure Find_And_Process_Nested_Scenarios;
      pragma Inline (Find_And_Process_Nested_Scenarios);
      --  Examine the declarations and statements of subprogram body N for
      --  suitable scenarios. Save each discovered scenario and process it
      --  accordingly.

      procedure Process_Nested_Scenarios (Nested : Elist_Id);
      pragma Inline (Process_Nested_Scenarios);
      --  Invoke Process_Conditional_ABE on each individual scenario found in
      --  list Nested.

      ---------------------------------------
      -- Find_And_Process_Nested_Scenarios --
      ---------------------------------------

      procedure Find_And_Process_Nested_Scenarios is
         Body_Id : constant Entity_Id := Defining_Entity (N);

         function Is_Potential_Scenario
           (Nod : Node_Id) return Traverse_Result;
         --  Determine whether arbitrary node Nod denotes a suitable scenario.
         --  If it does, save it in the Nested_Scenarios list of the subprogram
         --  body, and process it.

         procedure Save_Scenario (Nod : Node_Id);
         pragma Inline (Save_Scenario);
         --  Save scenario Nod in the Nested_Scenarios list of the subprogram
         --  body.

         procedure Traverse_List (List : List_Id);
         pragma Inline (Traverse_List);
         --  Invoke Traverse_Potential_Scenarios on each node in list List

         procedure Traverse_Potential_Scenarios is
           new Traverse_Proc (Is_Potential_Scenario);

         ---------------------------
         -- Is_Potential_Scenario --
         ---------------------------

         function Is_Potential_Scenario
           (Nod : Node_Id) return Traverse_Result
         is
         begin
            --  Special cases

            --  Skip constructs which do not have elaboration of their own and
            --  need to be elaborated by other means such as invocation, task
            --  activation, etc.

            if Is_Non_Library_Level_Encapsulator (Nod) then
               return Skip;

            --  Terminate the traversal of a task body with an accept statement
            --  when no entry calls in elaboration are allowed because the task
            --  will block at run-time and the remaining statements will not be
            --  executed.

            elsif Nkind_In (Original_Node (Nod), N_Accept_Statement,
                                                 N_Selective_Accept)
              and then Restriction_Active (No_Entry_Calls_In_Elaboration_Code)
            then
               return Abandon;

            --  Certain nodes carry semantic lists which act as repositories
            --  until expansion transforms the node and relocates the contents.
            --  Examine these lists in case expansion is disabled.

            elsif Nkind_In (Nod, N_And_Then, N_Or_Else) then
               Traverse_List (Actions (Nod));

            elsif Nkind_In (Nod, N_Elsif_Part, N_Iteration_Scheme) then
               Traverse_List (Condition_Actions (Nod));

            elsif Nkind (Nod) = N_If_Expression then
               Traverse_List (Then_Actions (Nod));
               Traverse_List (Else_Actions (Nod));

            elsif Nkind_In (Nod, N_Component_Association,
                                 N_Iterated_Component_Association)
            then
               Traverse_List (Loop_Actions (Nod));

            --  General case

            --  Save a suitable scenario in the Nested_Scenarios list of the
            --  subprogram body. As a result any subsequent traversals of the
            --  subprogram body started from a different top-level scenario no
            --  longer need to reexamine the tree.

            elsif Is_Suitable_Scenario (Nod) then
               Save_Scenario (Nod);

               Process_Conditional_ABE
                 (N              => Nod,
                  In_Init_Cond   => In_Init_Cond,
                  In_Partial_Fin => In_Partial_Fin,
                  In_Task_Body   => In_Task_Body);
            end if;

            return OK;
         end Is_Potential_Scenario;

         -------------------
         -- Save_Scenario --
         -------------------

         procedure Save_Scenario (Nod : Node_Id) is
            Nested : Elist_Id;

         begin
            Nested := Nested_Scenarios (Body_Id);

            if No (Nested) then
               Nested := New_Elmt_List;
               Set_Nested_Scenarios (Body_Id, Nested);
            end if;

            Append_Elmt (Nod, Nested);
         end Save_Scenario;

         -------------------
         -- Traverse_List --
         -------------------

         procedure Traverse_List (List : List_Id) is
            Item : Node_Id;

         begin
            Item := First (List);
            while Present (Item) loop
               Traverse_Potential_Scenarios (Item);
               Next (Item);
            end loop;
         end Traverse_List;

      --  Start of processing for Find_And_Process_Nested_Scenarios

      begin
         --  Examine the declarations for suitable scenarios

         Traverse_List (Declarations (N));

         --  Examine the handled sequence of statements. This also includes any
         --  exceptions handlers.

         Traverse_Potential_Scenarios (Handled_Statement_Sequence (N));
      end Find_And_Process_Nested_Scenarios;

      ------------------------------
      -- Process_Nested_Scenarios --
      ------------------------------

      procedure Process_Nested_Scenarios (Nested : Elist_Id) is
         Nested_Elmt : Elmt_Id;

      begin
         Nested_Elmt := First_Elmt (Nested);
         while Present (Nested_Elmt) loop
            Process_Conditional_ABE
              (N              => Node (Nested_Elmt),
               In_Init_Cond   => In_Init_Cond,
               In_Partial_Fin => In_Partial_Fin,
               In_Task_Body   => In_Task_Body);

            Next_Elmt (Nested_Elmt);
         end loop;
      end Process_Nested_Scenarios;

      --  Local variables

      Nested : Elist_Id;

   --  Start of processing for Traverse_Body

   begin
      --  Nothing to do when there is no body

      if No (N) then
         return;

      elsif Nkind (N) /= N_Subprogram_Body then
         return;
      end if;

      --  Nothing to do if the body was already traversed during the processing
      --  of the same top-level scenario.

      if Is_Visited_Body (N) then
         return;

      --  Otherwise mark the body as traversed

      else
         Set_Is_Visited_Body (N);
      end if;

      Nested := Nested_Scenarios (Defining_Entity (N));

      --  The subprogram body was already examined as part of the elaboration
      --  graph starting from a different top-level scenario. There is no need
      --  to traverse the declarations and statements again because this will
      --  yield the exact same scenarios. Use the nested scenarios collected
      --  during the first inspection of the body.

      if Present (Nested) then
         Process_Nested_Scenarios (Nested);

      --  Otherwise examine the declarations and statements of the subprogram
      --  body for suitable scenarios, save and process them accordingly.

      else
         Find_And_Process_Nested_Scenarios;
      end if;
   end Traverse_Body;

   ---------------------------------
   -- Update_Elaboration_Scenario --
   ---------------------------------

   procedure Update_Elaboration_Scenario (New_N : Node_Id; Old_N : Node_Id) is
      procedure Update_SPARK_Scenario;
      pragma Inline (Update_SPARK_Scenario);
      --  Update the contents of table SPARK_Scenarios if Old_N is recorded
      --  there.

      procedure Update_Top_Level_Scenario;
      pragma Inline (Update_Top_Level_Scenario);
      --  Update the contexts of table Top_Level_Scenarios if Old_N is recorded
      --  there.

      ---------------------------
      -- Update_SPARK_Scenario --
      ---------------------------

      procedure Update_SPARK_Scenario is
         package Scenarios renames SPARK_Scenarios;

      begin
         if Is_Recorded_SPARK_Scenario (Old_N) then

            --  Performance note: list traversal

            for Index in Scenarios.First .. Scenarios.Last loop
               if Scenarios.Table (Index) = Old_N then
                  Scenarios.Table (Index) := New_N;

                  --  The old SPARK scenario is no longer recorded, but the new
                  --  one is.

                  Set_Is_Recorded_Top_Level_Scenario (Old_N, False);
                  Set_Is_Recorded_Top_Level_Scenario (New_N);
                  return;
               end if;
            end loop;

            --  A recorded SPARK scenario must be in the table of recorded
            --  SPARK scenarios.

            pragma Assert (False);
         end if;
      end Update_SPARK_Scenario;

      -------------------------------
      -- Update_Top_Level_Scenario --
      -------------------------------

      procedure Update_Top_Level_Scenario is
         package Scenarios renames Top_Level_Scenarios;

      begin
         if Is_Recorded_Top_Level_Scenario (Old_N) then

            --  Performance note: list traversal

            for Index in Scenarios.First .. Scenarios.Last loop
               if Scenarios.Table (Index) = Old_N then
                  Scenarios.Table (Index) := New_N;

                  --  The old top-level scenario is no longer recorded, but the
                  --  new one is.

                  Set_Is_Recorded_Top_Level_Scenario (Old_N, False);
                  Set_Is_Recorded_Top_Level_Scenario (New_N);
                  return;
               end if;
            end loop;

            --  A recorded top-level scenario must be in the table of recorded
            --  top-level scenarios.

            pragma Assert (False);
         end if;
      end Update_Top_Level_Scenario;

   --  Start of processing for Update_Elaboration_Requirement

   begin
      --  Nothing to do when the old and new scenarios are one and the same

      if Old_N = New_N then
         return;

      --  A scenario is being transformed by Atree.Rewrite. Update all relevant
      --  internal data structures to reflect this change. This ensures that a
      --  potential run-time conditional ABE check or a guaranteed ABE failure
      --  is inserted at the proper place in the tree.

      elsif Is_Scenario (Old_N) then
         Update_SPARK_Scenario;
         Update_Top_Level_Scenario;
      end if;
   end Update_Elaboration_Scenario;

   -------------------------
   -- Visited_Bodies_Hash --
   -------------------------

   function Visited_Bodies_Hash (Key : Node_Id) return Visited_Bodies_Index is
   begin
      return Visited_Bodies_Index (Key mod Visited_Bodies_Max);
   end Visited_Bodies_Hash;

end Sem_Elab;
