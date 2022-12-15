.. role:: switch(samp)

.. |with| replace:: *with*
.. |withs| replace:: *with*\ s
.. |withed| replace:: *with*\ ed
.. |withing| replace:: *with*\ ing

.. -- Example: A |withing| unit has a |with| clause, it |withs| a |withed| unit


.. _Elaboration_Order_Handling_in_GNAT:

**********************************
Elaboration Order Handling in GNAT
**********************************

.. index:: Order of elaboration
.. index:: Elaboration control

This appendix describes the handling of elaboration code in Ada and GNAT, and
discusses how the order of elaboration of program units can be controlled in
GNAT, either automatically or with explicit programming features.

.. _Elaboration_Code:

Elaboration Code
================

Ada defines the term *execution* as the process by which a construct achieves
its run-time effect. This process is also referred to as **elaboration** for
declarations and *evaluation* for expressions.

The execution model in Ada allows for certain sections of an Ada program to be
executed prior to execution of the program itself, primarily with the intent of
initializing data. These sections are referred to as **elaboration code**.
Elaboration code is executed as follows:

* All partitions of an Ada program are executed in parallel with one another,
  possibly in a separate address space, and possibly on a separate computer.

* The execution of a partition involves running the environment task for that
  partition.

* The environment task executes all elaboration code (if available) for all
  units within that partition. This code is said to be executed at
  **elaboration time**.

* The environment task executes the Ada program (if available) for that
  partition.

In addition to the Ada terminology, this appendix defines the following terms:

* *Invocation*

  The act of calling a subprogram, instantiating a generic, or activating a
  task.

* *Scenario*

  A construct that is elaborated or invoked by elaboration code is referred to
  as an *elaboration scenario* or simply a **scenario**. GNAT recognizes the
  following scenarios:

  - ``'Access`` of entries, operators, and subprograms

  - Activation of tasks

  - Calls to entries, operators, and subprograms

  - Instantiations of generic templates

* *Target*

  A construct elaborated by a scenario is referred to as *elaboration target*
  or simply **target**. GNAT recognizes the following targets:

  - For ``'Access`` of entries, operators, and subprograms, the target is the
    entry, operator, or subprogram being aliased.

  - For activation of tasks, the target is the task body

  - For calls to entries, operators, and subprograms, the target is the entry,
    operator, or subprogram being invoked.

  - For instantiations of generic templates, the target is the generic template
    being instantiated.

Elaboration code may appear in two distinct contexts:

* *Library level*

  A scenario appears at the library level when it is encapsulated by a package
  [body] compilation unit, ignoring any other package [body] declarations in
  between.

  .. code-block:: ada

     with Server;
     package Client is
        procedure Proc;

        package Nested is
           Val : ... := Server.Func;
        end Nested;
     end Client;

  In the example above, the call to ``Server.Func`` is an elaboration scenario
  because it appears at the library level of package ``Client``. Note that the
  declaration of package ``Nested`` is ignored according to the definition
  given above. As a result, the call to ``Server.Func`` will be invoked when
  the spec of unit ``Client`` is elaborated.

* *Package body statements*

  A scenario appears within the statement sequence of a package body when it is
  bounded by the region starting from the ``begin`` keyword of the package body
  and ending at the ``end`` keyword of the package body.

  .. code-block:: ada

     package body Client is
        procedure Proc is
        begin
           ...
        end Proc;
     begin
        Proc;
     end Client;

  In the example above, the call to ``Proc`` is an elaboration scenario because
  it appears within the statement sequence of package body ``Client``. As a
  result, the call to ``Proc`` will be invoked when the body of ``Client`` is
  elaborated.

.. _Elaboration_Order:

Elaboration Order
=================

The sequence by which the elaboration code of all units within a partition is
executed is referred to as **elaboration order**.

Within a single unit, elaboration code is executed in sequential order.

  .. code-block:: ada

     package body Client is
        Result : ... := Server.Func;

        procedure Proc is
           package Inst is new Server.Gen;
        begin
           Inst.Eval (Result);
        end Proc;
     begin
        Proc;
     end Client;

In the example above, the elaboration order within package body ``Client`` is
as follows:

1. The object declaration of ``Result`` is elaborated.

   * Function ``Server.Func`` is invoked.

2. The subprogram body of ``Proc`` is elaborated.

3. Procedure ``Proc`` is invoked.

   * Generic unit ``Server.Gen`` is instantiated as ``Inst``.

   * Instance ``Inst`` is elaborated.

   * Procedure ``Inst.Eval`` is invoked.

The elaboration order of all units within a partition depends on the following
factors:

* |withed| units

* parent units

* purity of units

* preelaborability of units

* presence of elaboration-control pragmas

* invocations performed in elaboration code

A program may have several elaboration orders depending on its structure.

  .. code-block:: ada

     package Server is
        function Func (Index : Integer) return Integer;
     end Server;

  .. code-block:: ada

     package body Server is
        Results : array (1 .. 5) of Integer := (1, 2, 3, 4, 5);

        function Func (Index : Integer) return Integer is
        begin
           return Results (Index);
        end Func;
     end Server;

  .. code-block:: ada

     with Server;
     package Client is
        Val : constant Integer := Server.Func (3);
     end Client;

  .. code-block:: ada

     with Client;
     procedure Main is begin null; end Main;

The following elaboration order exhibits a fundamental problem referred to as
*access-before-elaboration* or simply **ABE**.

  ::

     spec of Server
     spec of Client
     body of Server
     body of Main

The elaboration of ``Server``'s spec materializes function ``Func``, making it
callable. The elaboration of ``Client``'s spec elaborates the declaration of
``Val``. This invokes function ``Server.Func``, however the body of
``Server.Func`` has not been elaborated yet because ``Server``'s body comes
after ``Client``'s spec in the elaboration order. As a result, the value of
constant ``Val`` is now undefined.

Without any guarantees from the language, an undetected ABE problem may hinder
proper initialization of data, which in turn may lead to undefined behavior at
run time. To prevent such ABE problems, Ada employs dynamic checks in the same
vein as index or null exclusion checks. A failed ABE check raises exception
``Program_Error``.

The following elaboration order avoids the ABE problem and the program can be
successfully elaborated.

  ::

     spec of Server
     body of Server
     spec of Client
     body of Main

Ada states that a total elaboration order must exist, but it does not define
what this order is. A compiler is thus tasked with choosing a suitable
elaboration order which satisfies the dependencies imposed by |with| clauses,
unit categorization, elaboration-control pragmas, and invocations performed in
elaboration code. Ideally an order that avoids ABE problems should be chosen,
however a compiler may not always find such an order due to complications with
respect to control and data flow.

.. _Checking_the_Elaboration_Order:

Checking the Elaboration Order
==============================

To avoid placing the entire elaboration-order burden on the programmer, Ada
provides three lines of defense:

* *Static semantics*

  Static semantic rules restrict the possible choice of elaboration order. For
  instance, if unit Client |withs| unit Server, then the spec of Server is
  always elaborated prior to Client. The same principle applies to child units
  - the spec of a parent unit is always elaborated prior to the child unit.

* *Dynamic semantics*

  Dynamic checks are performed at run time, to ensure that a target is
  elaborated prior to a scenario that invokes it, thus avoiding ABE problems.
  A failed run-time check raises exception ``Program_Error``. The following
  restrictions apply:

  - *Restrictions on calls*

    An entry, operator, or subprogram can be called from elaboration code only
    when the corresponding body has been elaborated.

  - *Restrictions on instantiations*

    A generic unit can be instantiated by elaboration code only when the
    corresponding body has been elaborated.

  - *Restrictions on task activation*

    A task can be activated by elaboration code only when the body of the
    associated task type has been elaborated.

  The restrictions above can be summarized by the following rule:

  *If a target has a body, then this body must be elaborated prior to the
  scenario that invokes the target.*

* *Elaboration control*

  Pragmas are provided for the programmer to specify the desired elaboration
  order.

.. _Controlling_the_Elaboration_Order_in_Ada:

Controlling the Elaboration Order in Ada
========================================

Ada provides several idioms and pragmas to aid the programmer with specifying
the desired elaboration order and avoiding ABE problems altogether.

* *Packages without a body*

  A library package which does not require a completing body does not suffer
  from ABE problems.

  .. code-block:: ada

     package Pack is
        generic
           type Element is private;
        package Containers is
           type Element_Array is array (1 .. 10) of Element;
        end Containers;
     end Pack;

  In the example above, package ``Pack`` does not require a body because it
  does not contain any constructs which require completion in a body. As a
  result, generic ``Pack.Containers`` can be instantiated without encountering
  any ABE problems.

.. index:: pragma Pure

* *pragma Pure*

  Pragma ``Pure`` places sufficient restrictions on a unit to guarantee that no
  scenario within the unit can result in an ABE problem.

.. index:: pragma Preelaborate

* *pragma Preelaborate*

  Pragma ``Preelaborate`` is slightly less restrictive than pragma ``Pure``,
  but still strong enough to prevent ABE problems within a unit.

.. index:: pragma Elaborate_Body

* *pragma Elaborate_Body*

  Pragma ``Elaborate_Body`` requires that the body of a unit is elaborated
  immediately after its spec. This restriction guarantees that no client
  scenario can invoke a server target before the target body has been
  elaborated because the spec and body are effectively "glued" together.

  .. code-block:: ada

     package Server is
        pragma Elaborate_Body;

        function Func return Integer;
     end Server;

  .. code-block:: ada

     package body Server is
        function Func return Integer is
        begin
           ...
        end Func;
     end Server;

  .. code-block:: ada

     with Server;
     package Client is
        Val : constant Integer := Server.Func;
     end Client;

  In the example above, pragma ``Elaborate_Body`` guarantees the following
  elaboration order:

  ::

     spec of Server
     body of Server
     spec of Client

  because the spec of ``Server`` must be elaborated prior to ``Client`` by
  virtue of the |with| clause, and in addition the body of ``Server`` must be
  elaborated immediately after the spec of ``Server``.

  Removing pragma ``Elaborate_Body`` could result in the following incorrect
  elaboration order:

  ::

     spec of Server
     spec of Client
     body of Server

  where ``Client`` invokes ``Server.Func``, but the body of ``Server.Func`` has
  not been elaborated yet.

The pragmas outlined above allow a server unit to guarantee safe elaboration
use by client units. Thus it is a good rule to mark units as ``Pure`` or
``Preelaborate``, and if this is not possible, mark them as ``Elaborate_Body``.

There are however situations where ``Pure``, ``Preelaborate``, and
``Elaborate_Body`` are not applicable. Ada provides another set of pragmas for
use by client units to help ensure the elaboration safety of server units they
depend on.

.. index:: pragma Elaborate (Unit)

* *pragma Elaborate (Unit)*

  Pragma ``Elaborate`` can be placed in the context clauses of a unit, after a
  |with| clause. It guarantees that both the spec and body of its argument will
  be elaborated prior to the unit with the pragma. Note that other unrelated
  units may be elaborated in between the spec and the body.

  .. code-block:: ada

     package Server is
        function Func return Integer;
     end Server;

  .. code-block:: ada

     package body Server is
        function Func return Integer is
        begin
           ...
        end Func;
     end Server;

  .. code-block:: ada

     with Server;
     pragma Elaborate (Server);
     package Client is
        Val : constant Integer := Server.Func;
     end Client;

  In the example above, pragma ``Elaborate`` guarantees the following
  elaboration order:

  ::

     spec of Server
     body of Server
     spec of Client

  Removing pragma ``Elaborate`` could result in the following incorrect
  elaboration order:

  ::

     spec of Server
     spec of Client
     body of Server

  where ``Client`` invokes ``Server.Func``, but the body of ``Server.Func``
  has not been elaborated yet.

.. index:: pragma Elaborate_All (Unit)

* *pragma Elaborate_All (Unit)*

  Pragma ``Elaborate_All`` is placed in the context clauses of a unit, after
  a |with| clause. It guarantees that both the spec and body of its argument
  will be elaborated prior to the unit with the pragma, as well as all units
  |withed| by the spec and body of the argument, recursively. Note that other
  unrelated units may be elaborated in between the spec and the body.

  .. code-block:: ada

     package Math is
        function Factorial (Val : Natural) return Natural;
     end Math;

  .. code-block:: ada

     package body Math is
        function Factorial (Val : Natural) return Natural is
        begin
           ...;
        end Factorial;
     end Math;

  .. code-block:: ada

     package Computer is
        type Operation_Kind is (None, Op_Factorial);

        function Compute
          (Val : Natural;
           Op  : Operation_Kind) return Natural;
     end Computer;

  .. code-block:: ada

     with Math;
     package body Computer is
        function Compute
          (Val : Natural;
           Op  : Operation_Kind) return Natural
        is
           if Op = Op_Factorial then
              return Math.Factorial (Val);
           end if;

           return 0;
        end Compute;
     end Computer;

  .. code-block:: ada

     with Computer;
     pragma Elaborate_All (Computer);
     package Client is
        Val : constant Natural :=
                Computer.Compute (123, Computer.Op_Factorial);
     end Client;

  In the example above, pragma ``Elaborate_All`` can result in the following
  elaboration order:

  ::

     spec of Math
     body of Math
     spec of Computer
     body of Computer
     spec of Client

  Note that there are several allowable suborders for the specs and bodies of
  ``Math`` and ``Computer``, but the point is that these specs and bodies will
  be elaborated prior to ``Client``.

  Removing pragma ``Elaborate_All`` could result in the following incorrect
  elaboration order:

  ::

     spec of Math
     spec of Computer
     body of Computer
     spec of Client
     body of Math

  where ``Client`` invokes ``Computer.Compute``, which in turn invokes
  ``Math.Factorial``, but the body of ``Math.Factorial`` has not been
  elaborated yet.

All pragmas shown above can be summarized by the following rule:

*If a client unit elaborates a server target directly or indirectly, then if
the server unit requires a body and does not have pragma Pure, Preelaborate,
or Elaborate_Body, then the client unit should have pragma Elaborate or
Elaborate_All for the server unit.*

If the rule outlined above is not followed, then a program may fall in one of
the following states:

* *No elaboration order exists*

  In this case a compiler must diagnose the situation, and refuse to build an
  executable program.

* *One or more incorrect elaboration orders exist*

  In this case a compiler can build an executable program, but
  ``Program_Error`` will be raised when the program is run.

* *Several elaboration orders exist, some correct, some incorrect*

  In this case the programmer has not controlled the elaboration order. As a
  result, a compiler may or may not pick one of the correct orders, and the
  program may or may not raise ``Program_Error`` when it is run. This is the
  worst possible state because the program may fail on another compiler, or
  even another version of the same compiler.

* *One or more correct orders exist*

  In this case a compiler can build an executable program, and the program is
  run successfully. This state may be guaranteed by following the outlined
  rules, or may be the result of good program architecture.

Note that one additional advantage of using ``Elaborate`` and ``Elaborate_All``
is that the program continues to stay in the last state (one or more correct
orders exist) even if maintenance changes the bodies of targets.

.. _Controlling_the_Elaboration_Order_in_GNAT:

Controlling the Elaboration Order in GNAT
=========================================

In addition to Ada semantics and rules synthesized from them, GNAT offers
three elaboration models to aid the programmer with specifying the correct
elaboration order and to diagnose elaboration problems.

.. index:: Dynamic elaboration model

* *Dynamic elaboration model*

  This is the most permissive of the three elaboration models and emulates the
  behavior specified by the Ada Reference Manual. When the dynamic model is in
  effect, GNAT makes the following assumptions:

  - All code within all units in a partition is considered to be elaboration
    code.

  - Some of the invocations in elaboration code may not take place at run time
    due to conditional execution.

  GNAT performs extensive diagnostics on a unit-by-unit basis for all scenarios
  that invoke internal targets. In addition, GNAT generates run-time checks for
  all external targets and for all scenarios that may exhibit ABE problems.

  The elaboration order is obtained by honoring all |with| clauses, purity and
  preelaborability of units, and elaboration-control pragmas. The dynamic model
  attempts to take all invocations in elaboration code into account. If an
  invocation leads to a circularity, GNAT ignores the invocation based on the
  assumptions stated above. An order obtained using the dynamic model may fail
  an ABE check at run time when GNAT ignored an invocation.

  The dynamic model is enabled with compiler switch :switch:`-gnatE`.

.. index:: Static elaboration model

* *Static elaboration model*

  This is the middle ground of the three models. When the static model is in
  effect, GNAT makes the following assumptions:

  - Only code at the library level and in package body statements within all
    units in a partition is considered to be elaboration code.

  - All invocations in elaboration will take place at run time, regardless of
    conditional execution.

  GNAT performs extensive diagnostics on a unit-by-unit basis for all scenarios
  that invoke internal targets. In addition, GNAT generates run-time checks for
  all external targets and for all scenarios that may exhibit ABE problems.

  The elaboration order is obtained by honoring all |with| clauses, purity and
  preelaborability of units, presence of elaboration-control pragmas, and all
  invocations in elaboration code. An order obtained using the static model is
  guaranteed to be ABE problem-free, excluding dispatching calls and
  access-to-subprogram types.

  The static model is the default model in GNAT.

.. index:: SPARK elaboration model

* *SPARK elaboration model*

  This is the most conservative of the three models and enforces the SPARK
  rules of elaboration as defined in the SPARK Reference Manual, section 7.7.
  The SPARK model is in effect only when a scenario and a target reside in a
  region subject to ``SPARK_Mode On``, otherwise the dynamic or static model
  is in effect.

  The SPARK model is enabled with compiler switch :switch:`-gnatd.v`.

.. index:: Legacy elaboration models

* *Legacy elaboration models*

  In addition to the three elaboration models outlined above, GNAT provides the
  following legacy models:

  - `Legacy elaboration-checking model` available in pre-18.x versions of GNAT.
    This model is enabled with compiler switch :switch:`-gnatH`.

  - `Legacy elaboration-order model` available in pre-20.x versions of GNAT.
    This model is enabled with binder switch :switch:`-H`.

.. index:: Relaxed elaboration mode

The dynamic, legacy, and static models can be relaxed using compiler switch
:switch:`-gnatJ`, making them more permissive. Note that in this mode, GNAT
may not diagnose certain elaboration issues or install run-time checks.

.. _Mixing_Elaboration_Models:

Mixing Elaboration Models
=========================

It is possible to mix units compiled with a different elaboration model,
however the following rules must be observed:

* A client unit compiled with the dynamic model can only |with| a server unit
  that meets at least one of the following criteria:

  - The server unit is compiled with the dynamic model.

  - The server unit is a GNAT implementation unit from the ``Ada``, ``GNAT``,
    ``Interfaces``, or ``System`` hierarchies.

  - The server unit has pragma ``Pure`` or ``Preelaborate``.

  - The client unit has an explicit ``Elaborate_All`` pragma for the server
    unit.

These rules ensure that elaboration checks are not omitted. If the rules are
violated, the binder emits a warning:

  ::

     warning: "x.ads" has dynamic elaboration checks and with's
     warning:   "y.ads" which has static elaboration checks

The warnings can be suppressed by binder switch :switch:`-ws`.

.. _ABE_Diagnostics:

ABE Diagnostics
===============

GNAT performs extensive diagnostics on a unit-by-unit basis for all scenarios
that invoke internal targets, regardless of whether the dynamic, SPARK, or
static model is in effect.

Note that GNAT emits warnings rather than hard errors whenever it encounters an
elaboration problem. This is because the elaboration model in effect may be too
conservative, or a particular scenario may not be invoked due conditional
execution. The warnings can be suppressed selectively with ``pragma Warnings
(Off)`` or globally with compiler switch :switch:`-gnatwL`.

A *guaranteed ABE* arises when the body of a target is not elaborated early
enough, and causes *all* scenarios that directly invoke the target to fail.

  .. code-block:: ada

     package body Guaranteed_ABE is
        function ABE return Integer;

        Val : constant Integer := ABE;

        function ABE return Integer is
        begin
          ...
        end ABE;
     end Guaranteed_ABE;

In the example above, the elaboration of ``Guaranteed_ABE``'s body elaborates
the declaration of ``Val``. This invokes function ``ABE``, however the body of
``ABE`` has not been elaborated yet. GNAT emits the following diagnostic:

  ::

     4.    Val : constant Integer := ABE;
                                     |
        >>> warning: cannot call "ABE" before body seen
        >>> warning: Program_Error will be raised at run time

A *conditional ABE* arises when the body of a target is not elaborated early
enough, and causes *some* scenarios that directly invoke the target to fail.

  .. code-block:: ada

      1. package body Conditional_ABE is
      2.    procedure Force_Body is null;
      3.
      4.    generic
      5.       with function Func return Integer;
      6.    package Gen is
      7.       Val : constant Integer := Func;
      8.    end Gen;
      9.
     10.    function ABE return Integer;
     11.
     12.    function Cause_ABE return Boolean is
     13.       package Inst is new Gen (ABE);
     14.    begin
     15.       ...
     16.    end Cause_ABE;
     17.
     18.    Val : constant Boolean := Cause_ABE;
     19.
     20.    function ABE return Integer is
     21.    begin
     22.       ...
     23.    end ABE;
     24.
     25.    Safe : constant Boolean := Cause_ABE;
     26. end Conditional_ABE;

In the example above, the elaboration of package body ``Conditional_ABE``
elaborates the declaration of ``Val``. This invokes function ``Cause_ABE``,
which instantiates generic unit ``Gen`` as ``Inst``. The elaboration of
``Inst`` invokes function ``ABE``, however the body of ``ABE`` has not been
elaborated yet. GNAT emits the following diagnostic:

  ::

     13.       package Inst is new Gen (ABE);
               |
         >>> warning: in instantiation at line 7
         >>> warning: cannot call "ABE" before body seen
         >>> warning: Program_Error may be raised at run time
         >>> warning:   body of unit "Conditional_ABE" elaborated
         >>> warning:   function "Cause_ABE" called at line 18
         >>> warning:   function "ABE" called at line 7, instance at line 13

Note that the same ABE problem does not occur with the elaboration of
declaration ``Safe`` because the body of function ``ABE`` has already been
elaborated at that point.

.. _SPARK_Diagnostics:

SPARK Diagnostics
=================

GNAT enforces the SPARK rules of elaboration as defined in the SPARK Reference
Manual section 7.7 when compiler switch :switch:`-gnatd.v` is in effect. Note
that GNAT emits hard errors whenever it encounters a violation of the SPARK
rules.

  ::

     1. with Server;
     2. package body SPARK_Diagnostics with SPARK_Mode is
     3.    Val : constant Integer := Server.Func;
                                           |
        >>> call to "Func" during elaboration in SPARK
        >>> unit "SPARK_Diagnostics" requires pragma "Elaborate_All" for "Server"
        >>>   body of unit "SPARK_Model" elaborated
        >>>   function "Func" called at line 3

     4. end SPARK_Diagnostics;

.. _Elaboration_Circularities:

Elaboration Circularities
=========================

An **elaboration circularity** occurs whenever the elaboration of a set of
units enters a deadlocked state, where each unit is waiting for another unit
to be elaborated. This situation may be the result of improper use of |with|
clauses, elaboration-control pragmas, or invocations in elaboration code.

The following example exhibits an elaboration circularity.

  .. code-block:: ada

     with B; pragma Elaborate (B);
     package A is
     end A;

  .. code-block:: ada

     package B is
        procedure Force_Body;
     end B;

  .. code-block:: ada

     with C;
     package body B is
        procedure Force_Body is null;

        Elab : constant Integer := C.Func;
     end B;

  .. code-block:: ada

     package C is
        function Func return Integer;
     end C;

  .. code-block:: ada

     with A;
     package body C is
        function Func return Integer is
        begin
           ...
        end Func;
     end C;

The binder emits the following diagnostic:

  ::

     error: Elaboration circularity detected
     info:
     info:    Reason:
     info:
     info:      unit "a (spec)" depends on its own elaboration
     info:
     info:    Circularity:
     info:
     info:      unit "a (spec)" has with clause and pragma Elaborate for unit "b (spec)"
     info:      unit "b (body)" is in the closure of pragma Elaborate
     info:      unit "b (body)" invokes a construct of unit "c (body)" at elaboration time
     info:      unit "c (body)" has with clause for unit "a (spec)"
     info:
     info:    Suggestions:
     info:
     info:      remove pragma Elaborate for unit "b (body)" in unit "a (spec)"
     info:      use the dynamic elaboration model (compiler switch -gnatE)

The diagnostic consist of the following sections:

* Reason

  This section provides a short explanation describing why the set of units
  could not be ordered.

* Circularity

  This section enumerates the units comprising the deadlocked set, along with
  their interdependencies.

* Suggestions

  This section enumerates various tactics for eliminating the circularity.

.. _Resolving_Elaboration_Circularities:

Resolving Elaboration Circularities
===================================

The most desirable option from the point of view of long-term maintenance is to
rearrange the program so that the elaboration problems are avoided. One useful
technique is to place the elaboration code into separate child packages.
Another is to move some of the initialization code to explicitly invoked
subprograms, where the program controls the order of initialization explicitly.
Although this is the most desirable option, it may be impractical and involve
too much modification, especially in the case of complex legacy code.

When faced with an elaboration circularity, the programmer should also consider
the tactics given in the suggestions section of the circularity diagnostic.
Depending on the units involved in the circularity, their |with| clauses,
purity, preelaborability, presence of elaboration-control pragmas and
invocations at elaboration time, the binder may suggest one or more of the
following tactics to eliminate the circularity:

* Pragma Elaborate elimination

  ::

     remove pragma Elaborate for unit "..." in unit "..."

  This tactic is suggested when the binder has determined that pragma
  ``Elaborate``:

  - Prevents a set of units from being elaborated.

  - The removal of the pragma will not eliminate the semantic effects of the
    pragma. In other words, the argument of the pragma will still be elaborated
    prior to the unit containing the pragma.

  - The removal of the pragma will enable the successful ordering of the units.

  The programmer should remove the pragma as advised, and rebuild the program.

* Pragma Elaborate_All elimination

  ::

     remove pragma Elaborate_All for unit "..." in unit "..."

  This tactic is suggested when the binder has determined that pragma
  ``Elaborate_All``:

  - Prevents a set of units from being elaborated.

  - The removal of the pragma will not eliminate the semantic effects of the
    pragma. In other words, the argument of the pragma along with its |with|
    closure will still be elaborated prior to the unit containing the pragma.

  - The removal of the pragma will enable the successful ordering of the units.

  The programmer should remove the pragma as advised, and rebuild the program.

* Pragma Elaborate_All downgrade

  ::

     change pragma Elaborate_All for unit "..." to Elaborate in unit "..."

  This tactic is always suggested with the pragma ``Elaborate_All`` elimination
  tactic. It offers a different alternative of guaranteeing that the argument
  of the pragma will still be elaborated prior to the unit containing the
  pragma.

  The programmer should update the pragma as advised, and rebuild the program.

* Pragma Elaborate_Body elimination

  ::

     remove pragma Elaborate_Body in unit "..."

  This tactic is suggested when the binder has determined that pragma
  ``Elaborate_Body``:

  - Prevents a set of units from being elaborated.

  - The removal of the pragma will enable the successful ordering of the units.

  Note that the binder cannot determine whether the pragma is required for
  other purposes, such as guaranteeing the initialization of a variable
  declared in the spec by elaboration code in the body.

  The programmer should remove the pragma as advised, and rebuild the program.

* Use of pragma Restrictions

  ::

     use pragma Restrictions (No_Entry_Calls_In_Elaboration_Code)

  This tactic is suggested when the binder has determined that a task
  activation at elaboration time:

  - Prevents a set of units from being elaborated.

  Note that the binder cannot determine with certainty whether the task will
  block at elaboration time.

  The programmer should create a configuration file, place the pragma within,
  update the general compilation arguments, and rebuild the program.

* Use of dynamic elaboration model

  ::

     use the dynamic elaboration model (compiler switch -gnatE)

  This tactic is suggested when the binder has determined that an invocation at
  elaboration time:

  - Prevents a set of units from being elaborated.

  - The use of the dynamic model will enable the successful ordering of the
    units.

  The programmer has two options:

  - Determine the units involved in the invocation using the detailed
    invocation information, and add compiler switch :switch:`-gnatE` to the
    compilation arguments of selected files only. This approach will yield
    safer elaboration orders compared to the other option because it will
    minimize the opportunities presented to the dynamic model for ignoring
    invocations.

  - Add compiler switch :switch:`-gnatE` to the general compilation arguments.

* Use of detailed invocation information

  ::

     use detailed invocation information (compiler switch -gnatd_F)

  This tactic is always suggested with the use of the dynamic model tactic. It
  causes the circularity section of the circularity diagnostic to describe the
  flow of elaboration code from a unit to a unit, enumerating all such paths in
  the process.

  The programmer should analyze this information to determine which units
  should be compiled with the dynamic model.

* Forced-dependency elimination

  ::

     remove the dependency of unit "..." on unit "..." from the argument of switch -f

  This tactic is suggested when the binder has determined that a dependency
  present in the forced-elaboration-order file indicated by binder switch
  :switch:`-f`:

  - Prevents a set of units from being elaborated.

  - The removal of the dependency will enable the successful ordering of the
    units.

  The programmer should edit the forced-elaboration-order file, remove the
  dependency, and rebind the program.

* All forced-dependency elimination

  ::

     remove switch -f

  This tactic is suggested in case editing the forced-elaboration-order file is
  not an option.

  The programmer should remove binder switch :switch:`-f` from the binder
  arguments, and rebind.

* Multiple-circularities diagnostic

  ::

     diagnose all circularities (binder switch -d_C)

  By default, the binder will diagnose only the highest-precedence circularity.
  If the program contains multiple circularities, the binder will suggest the
  use of binder switch :switch:`-d_C` in order to obtain the diagnostics of all
  circularities.

  The programmer should add binder switch :switch:`-d_C` to the binder
  arguments, and rebind.

If none of the tactics suggested by the binder eliminate the elaboration
circularity, the programmer should consider using one of the legacy elaboration
models, in the following order:

* Use the pre-20.x legacy elaboration-order model, with binder switch
  :switch:`-H`.

* Use both pre-18.x and pre-20.x legacy elaboration models, with compiler
  switch :switch:`-gnatH` and binder switch :switch:`-H`.

* Use the relaxed static-elaboration model, with compiler switches
  :switch:`-gnatH` :switch:`-gnatJ` and binder switch :switch:`-H`.

* Use the relaxed dynamic-elaboration model, with compiler switches
  :switch:`-gnatH` :switch:`-gnatJ` :switch:`-gnatE` and binder switch
  :switch:`-H`.

.. _Elaboration_Related_Compiler_Switches:

Elaboration-related Compiler Switches
=====================================

GNAT has several switches that affect the elaboration model and consequently
the elaboration order chosen by the binder.

.. index:: -gnatE  (gnat)

:switch:`-gnatE`
  Dynamic elaboration checking mode enabled

  When this switch is in effect, GNAT activates the dynamic model.

.. index:: -gnatel  (gnat)

:switch:`-gnatel`
  Turn on info messages on generated Elaborate[_All] pragmas

  This switch is only applicable to the pre-20.x legacy elaboration models.
  The post-20.x elaboration model no longer relies on implicitly generated
  ``Elaborate`` and ``Elaborate_All`` pragmas to order units.

  When this switch is in effect, GNAT will emit the following supplementary
  information depending on the elaboration model in effect.

  - *Dynamic model*

    GNAT will indicate missing ``Elaborate`` and ``Elaborate_All`` pragmas for
    all library-level scenarios within the partition.

  - *Static model*

    GNAT will indicate all scenarios invoked during elaboration. In addition,
    it will provide detailed traceback when an implicit ``Elaborate`` or
    ``Elaborate_All`` pragma is generated.

  - *SPARK model*

    GNAT will indicate how an elaboration requirement is met by the context of
    a unit. This diagnostic requires compiler switch :switch:`-gnatd.v`.

    ::

       1. with Server; pragma Elaborate_All (Server);
       2. package Client with SPARK_Mode is
       3.    Val : constant Integer := Server.Func;
                                             |
          >>> info: call to "Func" during elaboration in SPARK
          >>> info: "Elaborate_All" requirement for unit "Server" met by pragma at line 1

       4. end Client;

.. index:: -gnatH  (gnat)

:switch:`-gnatH`
  Legacy elaboration checking mode enabled

  When this switch is in effect, GNAT will utilize the pre-18.x elaboration
  model.

.. index:: -gnatJ  (gnat)

:switch:`-gnatJ`
  Relaxed elaboration checking mode enabled

  When this switch is in effect, GNAT will not process certain scenarios,
  resulting in a more permissive elaboration model. Note that this may
  eliminate some diagnostics and run-time checks.

.. index:: -gnatw.f  (gnat)

:switch:`-gnatw.f`
  Turn on warnings for suspicious Subp'Access

  When this switch is in effect, GNAT will treat ``'Access`` of an entry,
  operator, or subprogram as a potential call to the target and issue warnings:

  ::

     1. package body Attribute_Call is
     2.    function Func return Integer;
     3.    type Func_Ptr is access function return Integer;
     4.
     5.    Ptr : constant Func_Ptr := Func'Access;
                                          |
        >>> warning: "Access" attribute of "Func" before body seen
        >>> warning: possible Program_Error on later references
        >>> warning:   body of unit "Attribute_Call" elaborated
        >>> warning:   "Access" of "Func" taken at line 5

     6.
     7.    function Func return Integer is
     8.    begin
     9.       ...
    10.    end Func;
    11. end Attribute_Call;

  In the example above, the elaboration of declaration ``Ptr`` is assigned
  ``Func'Access`` before the body of ``Func`` has been elaborated.

.. index:: -gnatwl  (gnat)

:switch:`-gnatwl`
  Turn on warnings for elaboration problems

  When this switch is in effect, GNAT emits diagnostics in the form of warnings
  concerning various elaboration problems. The warnings are enabled by default.
  The switch is provided in case all warnings are suppressed, but elaboration
  warnings are still desired.

:switch:`-gnatwL`
  Turn off warnings for elaboration problems

  When this switch is in effect, GNAT no longer emits any diagnostics in the
  form of warnings. Selective suppression of elaboration problems is possible
  using ``pragma Warnings (Off)``.

  ::

     1. package body Selective_Suppression is
     2.    function ABE return Integer;
     3.
     4.    Val_1 : constant Integer := ABE;
                                       |
        >>> warning: cannot call "ABE" before body seen
        >>> warning: Program_Error will be raised at run time

     5.
     6.    pragma Warnings (Off);
     7.    Val_2 : constant Integer := ABE;
     8.    pragma Warnings (On);
     9.
    10.    function ABE return Integer is
    11.    begin
    12.       ...
    13.    end ABE;
    14. end Selective_Suppression;

  Note that suppressing elaboration warnings does not eliminate run-time
  checks. The example above will still fail at run time with an ABE.

.. _Summary_of_Procedures_for_Elaboration_Control:

Summary of Procedures for Elaboration Control
=============================================

A programmer should first compile the program with the default options, using
none of the binder or compiler switches. If the binder succeeds in finding an
elaboration order, then apart from possible cases involving dispatching calls
and access-to-subprogram types, the program is free of elaboration errors.

If it is important for the program to be portable to compilers other than GNAT,
then the programmer should use compiler switch :switch:`-gnatel` and consider
the messages about missing or implicitly created ``Elaborate`` and
``Elaborate_All`` pragmas.

If the binder reports an elaboration circularity, the programmer has several
options:

* Ensure that elaboration warnings are enabled. This will allow the static
  model to output trace information of elaboration issues. The trace
  information could shed light on previously unforeseen dependencies, as well
  as their origins. Elaboration warnings are enabled with compiler switch
  :switch:`-gnatwl`.

* Cosider the tactics given in the suggestions section of the circularity
  diagnostic.

* If none of the steps outlined above resolve the circularity, use a more
  permissive elaboration model, in the following order:

  - Use the pre-20.x legacy elaboration-order model, with binder switch
    :switch:`-H`.

  - Use both pre-18.x and pre-20.x legacy elaboration models, with compiler
    switch :switch:`-gnatH` and binder switch :switch:`-H`.

  - Use the relaxed static elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatJ` and binder switch :switch:`-H`.

  - Use the relaxed dynamic elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatJ` :switch:`-gnatE` and binder switch
    :switch:`-H`.

.. _Inspecting_the_Chosen_Elaboration_Order:

Inspecting the Chosen Elaboration Order
=======================================

To see the elaboration order chosen by the binder, inspect the contents of file
`b~xxx.adb`. On certain targets, this file appears as `b_xxx.adb`. The
elaboration order appears as a sequence of calls to ``Elab_Body`` and
``Elab_Spec``, interspersed with assignments to `Exxx` which indicates that a
particular unit is elaborated. For example:

  ::

     System.Soft_Links'Elab_Body;
     E14 := True;
     System.Secondary_Stack'Elab_Body;
     E18 := True;
     System.Exception_Table'Elab_Body;
     E24 := True;
     Ada.Io_Exceptions'Elab_Spec;
     E67 := True;
     Ada.Tags'Elab_Spec;
     Ada.Streams'Elab_Spec;
     E43 := True;
     Interfaces.C'Elab_Spec;
     E69 := True;
     System.Finalization_Root'Elab_Spec;
     E60 := True;
     System.Os_Lib'Elab_Body;
     E71 := True;
     System.Finalization_Implementation'Elab_Spec;
     System.Finalization_Implementation'Elab_Body;
     E62 := True;
     Ada.Finalization'Elab_Spec;
     E58 := True;
     Ada.Finalization.List_Controller'Elab_Spec;
     E76 := True;
     System.File_Control_Block'Elab_Spec;
     E74 := True;
     System.File_Io'Elab_Body;
     E56 := True;
     Ada.Tags'Elab_Body;
     E45 := True;
     Ada.Text_Io'Elab_Spec;
     Ada.Text_Io'Elab_Body;
     E07 := True;

Note also binder switch :switch:`-l`, which outputs the chosen elaboration
order and provides a more readable form of the above:

  ::

     ada (spec)
     interfaces (spec)
     system (spec)
     system.case_util (spec)
     system.case_util (body)
     system.concat_2 (spec)
     system.concat_2 (body)
     system.concat_3 (spec)
     system.concat_3 (body)
     system.htable (spec)
     system.parameters (spec)
     system.parameters (body)
     system.crtl (spec)
     interfaces.c_streams (spec)
     interfaces.c_streams (body)
     system.restrictions (spec)
     system.restrictions (body)
     system.standard_library (spec)
     system.exceptions (spec)
     system.exceptions (body)
     system.storage_elements (spec)
     system.storage_elements (body)
     system.secondary_stack (spec)
     system.stack_checking (spec)
     system.stack_checking (body)
     system.string_hash (spec)
     system.string_hash (body)
     system.htable (body)
     system.strings (spec)
     system.strings (body)
     system.traceback (spec)
     system.traceback (body)
     system.traceback_entries (spec)
     system.traceback_entries (body)
     ada.exceptions (spec)
     ada.exceptions.last_chance_handler (spec)
     system.soft_links (spec)
     system.soft_links (body)
     ada.exceptions.last_chance_handler (body)
     system.secondary_stack (body)
     system.exception_table (spec)
     system.exception_table (body)
     ada.io_exceptions (spec)
     ada.tags (spec)
     ada.streams (spec)
     interfaces.c (spec)
     interfaces.c (body)
     system.finalization_root (spec)
     system.finalization_root (body)
     system.memory (spec)
     system.memory (body)
     system.standard_library (body)
     system.os_lib (spec)
     system.os_lib (body)
     system.unsigned_types (spec)
     system.stream_attributes (spec)
     system.stream_attributes (body)
     system.finalization_implementation (spec)
     system.finalization_implementation (body)
     ada.finalization (spec)
     ada.finalization (body)
     ada.finalization.list_controller (spec)
     ada.finalization.list_controller (body)
     system.file_control_block (spec)
     system.file_io (spec)
     system.file_io (body)
     system.val_uns (spec)
     system.val_util (spec)
     system.val_util (body)
     system.val_uns (body)
     system.wch_con (spec)
     system.wch_con (body)
     system.wch_cnv (spec)
     system.wch_jis (spec)
     system.wch_jis (body)
     system.wch_cnv (body)
     system.wch_stw (spec)
     system.wch_stw (body)
     ada.tags (body)
     ada.exceptions (body)
     ada.text_io (spec)
     ada.text_io (body)
     text_io (spec)
     gdbstr (body)
