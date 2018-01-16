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

* *Scenario*

  A construct that is elaborated or executed by elaboration code is referred to
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

  ::

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
  given above. As a result, the call to ``Server.Func`` will be executed when
  the spec of unit ``Client`` is elaborated.

* *Package body statements*

  A scenario appears within the statement sequence of a package body when it is
  bounded by the region starting from the ``begin`` keyword of the package body
  and ending at the ``end`` keyword of the package body.

  ::

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
  result, the call to ``Proc`` will be executed when the body of ``Client`` is
  elaborated.

.. _Elaboration_Order:

Elaboration Order
=================

The sequence by which the elaboration code of all units within a partition is
executed is referred to as **elaboration order**.

Within a single unit, elaboration code is executed in sequential order.

::

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

* purity of units

* preelaborability of units

* presence of elaboration control pragmas

A program may have several elaboration orders depending on its structure.

::

   package Server is
      function Func (Index : Integer) return Integer;
   end Server;

::

   package body Server is
      Results : array (1 .. 5) of Integer := (1, 2, 3, 4, 5);

      function Func (Index : Integer) return Integer is
      begin
         return Results (Index);
      end Func;
   end Server;

::

   with Server;
   package Client is
      Val : constant Integer := Server.Func (3);
   end Client;

::

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
unit categorization, and elaboration control pragmas. Ideally an order which
avoids ABE problems should be chosen, however a compiler may not always find
such an order due to complications with respect to control and data flow.

.. _Checking_the_Elaboration_Order:

Checking the Elaboration Order
==============================

To avoid placing the entire elaboration order burden on the programmer, Ada 
provides three lines of defense:

* *Static semantics*

  Static semantic rules restrict the possible choice of elaboration order. For
  instance, if unit Client |withs| unit Server, then the spec of Server is
  always elaborated prior to Client. The same principle applies to child units
  - the spec of a parent unit is always elaborated prior to the child unit.

* *Dynamic semantics*

  Dynamic checks are performed at run time, to ensure that a target is
  elaborated prior to a scenario that executes it, thus avoiding ABE problems.
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
  execution of the scenario that invokes, instantiates, or activates the
  target.*

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

  ::

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
  scenario can execute a server target before the target body has been
  elaborated because the spec and body are effectively "glued" together.

  ::

     package Server is
        pragma Elaborate_Body;

        function Func return Integer;
     end Server;

  ::

     package body Server is
        function Func return Integer is
        begin
           ...
        end Func;
     end Server;

  ::

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

  ::

     package Server is
        function Func return Integer;
     end Server;

  ::

     package body Server is
        function Func return Integer is
        begin
           ...
        end Func;
     end Server;

  ::

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

  ::

     package Math is
        function Factorial (Val : Natural) return Natural;
     end Math;

  ::

     package body Math is
        function Factorial (Val : Natural) return Natural is
        begin
           ...;
        end Factorial;
     end Math;

  ::

     package Computer is
        type Operation_Kind is (None, Op_Factorial);

        function Compute
          (Val : Natural;
           Op  : Operation_Kind) return Natural;
     end Computer;

  ::

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

  ::

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
  elaboration order

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

  This is the most permissive of the three elaboration models. When the
  dynamic model is in effect, GNAT assumes that all code within all units in
  a partition is elaboration code. GNAT performs very few diagnostics and
  generates run-time checks to verify the elaboration order of a program. This
  behavior is identical to that specified by the Ada Reference Manual. The
  dynamic model is enabled with compiler switch :switch:`-gnatE`.

.. index:: Static elaboration model

* *Static elaboration model*

  This is the middle ground of the three models. When the static model is in
  effect, GNAT performs extensive diagnostics on a unit-by-unit basis for all
  scenarios that elaborate or execute internal targets. GNAT also generates
  run-time checks for all external targets and for all scenarios that may
  exhibit ABE problems. Finally, GNAT installs implicit ``Elaborate`` and
  ``Elaborate_All`` pragmas for server units based on the dependencies of
  client units. The static model is the default model in GNAT.

.. index:: SPARK elaboration model

* *SPARK elaboration model*

  This is the most conservative of the three models and enforces the SPARK
  rules of elaboration as defined in the SPARK Reference Manual, section 7.7.
  The SPARK model is in effect only when a scenario and a target reside in a
  region subject to SPARK_Mode On, otherwise the dynamic or static model is in
  effect.

.. index:: Legacy elaboration model

* *Legacy elaboration model*

  In addition to the three elaboration models outlined above, GNAT provides the
  elaboration model of pre-18.x versions referred to as `legacy elaboration
  model`. The legacy elaboration model is enabled with compiler switch
  :switch:`-gnatH`.

.. index:: Relaxed elaboration mode

The dynamic, legacy, and static models can be relaxed using compiler switch
:switch:`-gnatJ`, making them more permissive. Note that in this mode, GNAT
may not diagnose certain elaboration issues or install run-time checks.

.. _Common_Elaboration_Model_Traits":

Common Elaboration-model Traits
===============================

All three GNAT models are able to detect elaboration problems related to
dispatching calls and a particular kind of ABE referred to as *guaranteed ABE*.

* *Dispatching calls*

  GNAT installs run-time checks for each primitive subprogram of each tagged
  type defined in a partition on the assumption that a dispatching call
  invoked at elaboration time will execute one of these primitives. As a
  result, a dispatching call that executes a primitive whose body has not
  been elaborated yet will raise exception ``Program_Error`` at run time. The
  checks can be suppressed using pragma ``Suppress (Elaboration_Check)``.

* *Guaranteed ABE*

  A guaranteed ABE arises when the body of a target is not elaborated early
  enough, and causes all scenarios that directly execute the target to fail.

  ::

     package body Guaranteed_ABE is
        function ABE return Integer;

        Val : constant Integer := ABE;

        function ABE return Integer is
        begin
           ...
        end ABE;
     end Guaranteed_ABE;

  In the example above, the elaboration of ``Guaranteed_ABE``'s body elaborates
  the declaration of ``Val``. This invokes function ``ABE``, however the body
  of ``ABE`` has not been elaborated yet. GNAT emits similar diagnostics in all
  three models:

  ::

      1. package body Guaranteed_ABE is
      2.    function ABE return Integer;
      3.
      4.    Val : constant Integer := ABE;
                                      |
         >>> warning: cannot call "ABE" before body seen
         >>> warning: Program_Error will be raised at run time

      5.
      6.    function ABE return Integer is
      7.    begin
      8.       ...
      9.    end ABE;
      10. end Guaranteed_ABE;

Note that GNAT emits warnings rather than hard errors whenever it encounters an
elaboration problem. This is because the elaboration model in effect may be too
conservative, or a particular scenario may not be elaborated or executed due to
data and control flow. The warnings can be suppressed selectively with ``pragma
Warnigns (Off)`` or globally with compiler switch :switch:`-gnatwL`.

.. _Dynamic_Elaboration_Model_in_GNAT:

Dynamic Elaboration Model in GNAT
=================================

The dynamic model assumes that all code within all units in a partition is
elaboration code. As a result, run-time checks are installed for each scenario
regardless of whether the target is internal or external. The checks can be
suppressed using pragma ``Suppress (Elaboration_Check)``. This behavior is
identical to that specified by the Ada Reference Manual. The following example
showcases run-time checks installed by GNAT to verify the elaboration state of
package ``Dynamic_Model``.

::

   with Server;
   package body Dynamic_Model is
      procedure API is
      begin
         ...
      end API;

      <check that the body of Server.Gen is elaborated>
      package Inst is new Server.Gen;

      T : Server.Task_Type;

   begin
      <check that the body of Server.Task_Type is elaborated>

      <check that the body of Server.Proc is elaborated>
      Server.Proc;
   end Dynamic_Model;

The checks verify that the body of a target has been successfully elaborated
before a scenario activates, calls, or instantiates a target.

Note that no scenario within package ``Dynamic_Model`` calls procedure ``API``.
In fact, procedure ``API`` may not be invoked by elaboration code within the
partition, however the dynamic model assumes that this can happen.

The dynamic model emits very few diagnostics, but can make suggestions on
missing ``Elaborate`` and ``Elaborate_All`` pragmas for library-level
scenarios. This information is available when compiler switch :switch:`-gnatel`
is in effect.

::

   1. with Server;
   2. package body Dynamic_Model is
   3.    Val : constant Integer := Server.Func;
                                         |
      >>> info: call to "Func" during elaboration
      >>> info: missing pragma "Elaborate_All" for unit "Server"

   4. end Dynamic_Model;

.. _Static_Elaboration_Model_in_GNAT:

Static Elaboration Model in GNAT
================================

In contrast to the dynamic model, the static model is more precise in its
analysis of elaboration code. The model makes a clear distinction between
internal and external targets, and resorts to different diagnostics and
run-time checks based on the nature of the target.

* *Internal targets*

  The static model performs extensive diagnostics on scenarios which elaborate
  or execute internal targets. The warnings resulting from these diagnostics
  are enabled by default, but can be suppressed selectively with ``pragma
  Warnings (Off)`` or globally with compiler switch :switch:`-gnatwL`.

  ::

      1. package body Static_Model is
      2.    generic
      3.       with function Func return Integer;
      4.    package Gen is
      5.       Val : constant Integer := Func;
      6.    end Gen;
      7.
      8.    function ABE return Integer;
      9.
     10.    function Cause_ABE return Boolean is
     11.       package Inst is new Gen (ABE);
               |
         >>> warning: in instantiation at line 5
         >>> warning: cannot call "ABE" before body seen
         >>> warning: Program_Error may be raised at run time
         >>> warning:   body of unit "Static_Model" elaborated
         >>> warning:   function "Cause_ABE" called at line 16
         >>> warning:   function "ABE" called at line 5, instance at line 11

     12.    begin
     13.       ...
     14.    end Cause_ABE;
     15.
     16.    Val : constant Boolean := Cause_ABE;
     17.
     18.    function ABE return Integer is
     19.    begin
     20.       ...
     21.    end ABE;
     22. end Static_Model;

  The example above illustrates an ABE problem within package ``Static_Model``,
  which is hidden by several layers of indirection. The elaboration of package
  body ``Static_Model`` elaborates the declaration of ``Val``. This invokes
  function ``Cause_ABE``, which instantiates generic unit ``Gen`` as ``Inst``.
  The elaboration of ``Inst`` invokes function ``ABE``, however the body of
  ``ABE`` has not been elaborated yet.

* *External targets*

  The static model installs run-time checks to verify the elaboration status
  of server targets only when the scenario that elaborates or executes that
  target is part of the elaboration code of the client unit. The checks can be
  suppressed using pragma ``Suppress (Elaboration_Check)``.

  ::

     with Server;
     package body Static_Model is
        generic
           with function Func return Integer;
        package Gen is
           Val : constant Integer := Func;
        end Gen;

        function Call_Func return Boolean is
           <check that the body of Server.Func is elaborated>
           package Inst is new Gen (Server.Func);
        begin
           ...
        end Call_Func;

        Val : constant Boolean := Call_Func;
     end Static_Model;

  In the example above, the elaboration of package body ``Static_Model``
  elaborates the declaration of ``Val``. This invokes function ``Call_Func``,
  which instantiates generic unit ``Gen`` as ``Inst``. The elaboration of
  ``Inst`` invokes function ``Server.Func``. Since ``Server.Func`` is an
  external target, GNAT installs a run-time check to verify that its body has
  been elaborated.

  In addition to checks, the static model installs implicit ``Elaborate`` and
  ``Elaborate_All`` pragmas to guarantee safe elaboration use of server units.
  This information is available when compiler switch :switch:`-gnatel` is in
  effect.

  ::

      1. with Server;
      2. package body Static_Model is
      3.    generic
      4.       with function Func return Integer;
      5.    package Gen is
      6.       Val : constant Integer := Func;
      7.    end Gen;
      8.
      9.    function Call_Func return Boolean is
     10.       package Inst is new Gen (Server.Func);
               |
         >>> info: instantiation of "Gen" during elaboration
         >>> info: in instantiation at line 6
         >>> info: call to "Func" during elaboration
         >>> info: in instantiation at line 6
         >>> info: implicit pragma "Elaborate_All" generated for unit "Server"
         >>> info:   body of unit "Static_Model" elaborated
         >>> info:   function "Call_Func" called at line 15
         >>> info:   function "Func" called at line 6, instance at line 10

     11.    begin
     12.       ...
     13.    end Call_Func;
     14.
     15.    Val : constant Boolean := Call_Func;
                                      |
         >>> info: call to "Call_Func" during elaboration

     16. end Static_Model;

  In the example above, the elaboration of package body ``Static_Model``
  elaborates the declaration of ``Val``. This invokes function ``Call_Func``,
  which instantiates generic unit ``Gen`` as ``Inst``. The elaboration of
  ``Inst`` invokes function ``Server.Func``. Since ``Server.Func`` is an
  external target, GNAT installs an implicit ``Elaborate_All`` pragma for unit
  ``Server``. The pragma guarantees that both the spec and body of ``Server``,
  along with any additional dependencies that ``Server`` may require, are
  elaborated prior to the body of ``Static_Model``.

.. _SPARK_Elaboration_Model_in_GNAT:

SPARK Elaboration Model in GNAT
===============================

The SPARK model is identical to the static model in its handling of internal
targets. The SPARK model, however, requires explicit ``Elaborate`` or
``Elaborate_All`` pragmas to be present in the program when a target is
external, and compiler switch :switch:`-gnatd.v` is in effect.

::

   1. with Server;
   2. package body SPARK_Model with SPARK_Mode is
   3.    Val : constant Integer := Server.Func;
                                         |
      >>> call to "Func" during elaboration in SPARK
      >>> unit "SPARK_Model" requires pragma "Elaborate_All" for "Server"
      >>>   body of unit "SPARK_Model" elaborated
      >>>   function "Func" called at line 3

   4. end SPARK_Model;

Legacy Elaboration Model in GNAT
================================

The legacy elaboration model is provided for compatibility with code bases
developed with pre-18.x versions of GNAT. It is similar in functionality to
the dynamic and static models of post-18.x version of GNAT, but may differ
in terms of diagnostics and run-time checks. The legacy elaboration model is
enabled with compiler switch :switch:`-gnatH`.

.. _Mixing_Elaboration_Models:

Mixing Elaboration Models
=========================

It is possible to mix units compiled with a different elaboration model,
however the following rules must be observed:

* A client unit compiled with the dynamic model can only |with| a server unit
  that meets at least one of the following criteria:

  - The server unit is compiled with the dynamic model.

  - The server unit is a GNAT implementation unit from the Ada, GNAT,
    Interfaces, or System hierarchies.

  - The server unit has pragma ``Pure`` or ``Preelaborate``.

  - The client unit has an explicit ``Elaborate_All`` pragma for the server
    unit.

These rules ensure that elaboration checks are not omitted. If the rules are
violated, the binder emits a warning:

::

   warning: "x.ads" has dynamic elaboration checks and with's
   warning:   "y.ads" which has static elaboration checks

The warnings can be suppressed by binder switch :switch:`-ws`.

.. _Elaboration_Circularities:

Elaboration Circularities
=========================

If the binder cannot find an acceptable elaboration order, it outputs detailed
diagnostics describing an **elaboration circularity**.

::

   package Server is
      function Func return Integer;
   end Server;

::

   with Client;
   package body Server is
      function Func return Integer is
      begin
         ...
      end Func;
   end Server;

::

   with Server;
   package Client is
      Val : constant Integer := Server.Func;
   end Client;

::

   with Client;
   procedure Main is begin null; end Main;

::

   error: elaboration circularity detected
   info:    "server (body)" must be elaborated before "client (spec)"
   info:       reason: implicit Elaborate_All in unit "client (spec)"
   info:       recompile "client (spec)" with -gnatel for full details
   info:          "server (body)"
   info:             must be elaborated along with its spec:
   info:          "server (spec)"
   info:             which is withed by:
   info:          "client (spec)"
   info:    "client (spec)" must be elaborated before "server (body)"
   info:       reason: with clause

In the example above, ``Client`` must be elaborated prior to ``Main`` by virtue
of a |with| clause. The elaboration of ``Client`` invokes ``Server.Func``, and
static model generates an implicit ``Elaborate_All`` pragma for ``Server``. The
pragma implies that both the spec and body of ``Server``, along with any units
they |with|, must be elaborated prior to ``Client``. However, ``Server``'s body
|withs| ``Client``, implying that ``Client`` must be elaborated prior to
``Server``. The end result is that ``Client`` must be elaborated prior to
``Client``, and this leads to a circularity.

.. _Resolving_Elaboration_Circularities:

Resolving Elaboration Circularities
===================================

When faced with an elaboration circularity, a programmer has several options
available.

* *Fix the program*

  The most desirable option from the point of view of long-term maintenance
  is to rearrange the program so that the elaboration problems are avoided.
  One useful technique is to place the elaboration code into separate child
  packages. Another is to move some of the initialization code to explicitly
  invoked subprograms, where the program controls the order of initialization
  explicitly. Although this is the most desirable option, it may be impractical
  and involve too much modification, especially in the case of complex legacy
  code.

* *Switch to more permissive elaboration model*

  If the compilation was performed using the static model, enable the dynamic
  model with compiler switch :switch:`-gnatE`. GNAT will no longer generate
  implicit ``Elaborate`` and ``Elaborate_All`` pragmas, resulting in a behavior
  identical to that specified by the Ada Reference Manual. The binder will
  generate an executable program that may or may not raise ``Program_Error``,
  and it is the programmer's responsibility to ensure that it does not raise
  ``Program_Error``.

  If the compilation was performed using a post-18.x version of GNAT, consider
  using the legacy elaboration model, in the following order:

  - Use the legacy static elaboration model, with compiler switch
    :switch:`-gnatH`.

  - Use the legacy dynamic elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatE`.

  - Use the relaxed legacy static elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatJ`.

  - Use the relaxed legacy dynamic elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatJ` :switch:`-gnatE`.

* *Suppress all elaboration checks*

  The drawback of run-time checks is that they generate overhead at run time,
  both in space and time. If the programmer is absolutely sure that a program
  will not raise an elaboration-related ``Program_Error``, then using the
  pragma ``Suppress (Elaboration_Check)`` globally (as a configuration pragma)
  will eliminate all run-time checks.

* *Suppress elaboration checks selectively*

  If a scenario cannot possibly lead to an elaboration ``Program_Error``,
  and the binder nevertheless complains about implicit ``Elaborate`` and
  ``Elaborate_All`` pragmas that lead to elaboration circularities, it
  is possible to suppress the generation of implicit ``Elaborate`` and
  ``Elaborate_All`` pragmas, as well as run-time checks. Clearly this can
  be unsafe, and it is the responsibility of the programmer to make sure
  that the resulting program has no elaboration anomalies. Pragma
  ``Suppress (Elaboration_Check)`` can be used with different levels of
  granularity to achieve these effects.

  - *Target suppression*

    When the pragma is placed in a declarative part, without a second argument
    naming an entity, it will suppress implicit ``Elaborate`` and
    ``Elaborate_All`` pragma generation, as well as run-time checks, on all
    targets within the region.

    ::

       package Range_Suppress is
          pragma Suppress (Elaboration_Check);

          function Func return Integer;

          generic
          procedure Gen;

          pragma Unsuppress (Elaboration_Check);

          task type Tsk;
       end Range_Suppress;

    In the example above, a pair of Suppress/Unsuppress pragmas define a region
    of suppression within package ``Range_Suppress``. As a result, no implicit
    ``Elaborate`` and ``Elaborate_All`` pragmas, nor any run-time checks, will
    be generated by callers of ``Func`` and instantiators of ``Gen``. Note that
    task type ``Tsk`` is not within this region.

    An alternative to the region-based suppression is to use multiple
    ``Suppress`` pragmas with arguments naming specific entities for which
    elaboration checks should be suppressed:

    ::

       package Range_Suppress is
          function Func return Integer;
          pragma Suppress (Elaboration_Check, Func);

          generic
          procedure Gen;
          pragma Suppress (Elaboration_Check, Gen);

          task type Tsk;
       end Range_Suppress;

  - *Scenario suppression*

    When the pragma ``Suppress`` is placed in a declarative or statement
    part, without an entity argument, it will suppress implicit ``Elaborate``
    and ``Elaborate_All`` pragma generation, as well as run-time checks, on
    all scenarios within the region.

    ::

       with Server;
       package body Range_Suppress is
          pragma Suppress (Elaboration_Check);

          function Func return Integer is
          begin
             return Server.Func;
          end Func;

          procedure Gen is
          begin
             Server.Proc;
          end Gen;

          pragma Unsuppress (Elaboration_Check);

          task body Tsk is
          begin
             Server.Proc;
          end Tsk;
       end Range_Suppress;

    In the example above, a pair of Suppress/Unsuppress pragmas define a region
    of suppression within package body ``Range_Suppress``. As a result, the
    calls to ``Server.Func`` in ``Func`` and ``Server.Proc`` in ``Gen`` will
    not generate any implicit ``Elaborate`` and ``Elaborate_All`` pragmas or
    run-time checks.

.. _Resolving_Task_Issues:

Resolving Task Issues
=====================

The model of execution in Ada dictates that elaboration must first take place,
and only then can the main program be started. Tasks which are activated during
elaboration violate this model and may lead to serious concurrent problems at
elaboration time.

A task can be activated in two different ways:

* The task is created by an allocator in which case it is activated immediately
  after the allocator is evaluated.

* The task is declared at the library level or within some nested master in
  which case it is activated before starting execution of the statement
  sequence of the master defining the task.

Since the elaboration of a partition is performed by the environment task
servicing that partition, any tasks activated during elaboration may be in
a race with the environment task, and lead to unpredictable state and behavior.
The static model seeks to avoid such interactions by assuming that all code in
the task body is executed at elaboration time, if the task was activated by
elaboration code.

::

   package Decls is
      task Lib_Task is
         entry Start;
      end Lib_Task;

      type My_Int is new Integer;

      function Ident (M : My_Int) return My_Int;
   end Decls;

::

   with Utils;
   package body Decls is
      task body Lib_Task is
      begin
         accept Start;
         Utils.Put_Val (2);
      end Lib_Task;

      function Ident (M : My_Int) return My_Int is
      begin
         return M;
      end Ident;
   end Decls;

::

   with Decls;
   package Utils is
      procedure Put_Val (Arg : Decls.My_Int);
   end Utils;

::

   with Ada.Text_IO; use Ada.Text_IO;
   package body Utils is
      procedure Put_Val (Arg : Decls.My_Int) is
      begin
         Put_Line (Arg'Img);
      end Put_Val;
   end Utils;

::

   with Decls;
   procedure Main is
   begin
      Decls.Lib_Task.Start;
   end Main;

When the above example is compiled with the static model, an elaboration
circularity arises:

::

   error: elaboration circularity detected
   info:    "decls (body)" must be elaborated before "decls (body)"
   info:       reason: implicit Elaborate_All in unit "decls (body)"
   info:       recompile "decls (body)" with -gnatel for full details
   info:          "decls (body)"
   info:             must be elaborated along with its spec:
   info:          "decls (spec)"
   info:             which is withed by:
   info:          "utils (spec)"
   info:             which is withed by:
   info:          "decls (body)"

In the above example, ``Decls`` must be elaborated prior to ``Main`` by virtue
of a with clause. The elaboration of ``Decls`` activates task ``Lib_Task``. The
static model conservatibely assumes that all code within the body of
``Lib_Task`` is executed, and generates an implicit ``Elaborate_All`` pragma
for ``Units`` due to the call to ``Utils.Put_Val``. The pragma implies that
both the spec and body of ``Utils``, along with any units they |with|,
must be elaborated prior to ``Decls``. However, ``Utils``'s spec |withs|
``Decls``, implying that ``Decls`` must be elaborated before ``Utils``. The end
result is that ``Utils`` must be elaborated prior to ``Utils``, and this
leads to a circularity.

In reality, the example above will not exhibit an ABE problem at run time.
When the body of task ``Lib_Task`` is activated, execution will wait for entry
``Start`` to be accepted, and the call to ``Utils.Put_Val`` will not take place
at elaboration time. Task ``Lib_Task`` will resume its execution after the main
program is executed because ``Main`` performs a rendezvous with
``Lib_Task.Start``, and at that point all units have already been elaborated.
As a result, the static model may seem overly conservative, partly because it
does not take control and data flow into account.

When faced with a task elaboration circularity, a programmer has several
options available:

* *Use the dynamic model*

  The dynamic model does not generate implicit ``Elaborate`` and
  ``Elaborate_All`` pragmas. Instead, it will install checks prior to every
  call in the example above, thus verifying the successful elaboration of
  ``Utils.Put_Val`` in case the call to it takes place at elaboration time.
  The dynamic model is enabled with compiler switch :switch:`-gnatE`.

* *Isolate the tasks*

  Relocating tasks in their own separate package could decouple them from
  dependencies that would otherwise cause an elaboration circularity. The
  example above can be rewritten as follows:

  ::

     package Decls1 is                --  new
        task Lib_Task is
           entry Start;
        end Lib_Task;
     end Decls1;

  ::

     with Utils;
     package body Decls1 is           --  new
        task body Lib_Task is
        begin
           accept Start;
           Utils.Put_Val (2);
        end Lib_Task;
     end Decls1;

  ::

     package Decls2 is                --  new
        type My_Int is new Integer;
        function Ident (M : My_Int) return My_Int;
     end Decls2;

  ::

     with Utils;
     package body Decls2 is           --  new
        function Ident (M : My_Int) return My_Int is
        begin
           return M;
        end Ident;
     end Decls2;

  ::

     with Decls2;
     package Utils is
        procedure Put_Val (Arg : Decls2.My_Int);
     end Utils;

  ::

     with Ada.Text_IO; use Ada.Text_IO;
     package body Utils is
        procedure Put_Val (Arg : Decls2.My_Int) is
        begin
           Put_Line (Arg'Img);
        end Put_Val;
     end Utils;

  ::

     with Decls1;
     procedure Main is
     begin
        Decls1.Lib_Task.Start;
     end Main;
   
* *Declare the tasks*

  The original example uses a single task declaration for ``Lib_Task``. An
  explicit task type declaration and a properly placed task object could avoid
  the dependencies that would otherwise cause an elaboration circularity. The
  example can be rewritten as follows:

  ::

     package Decls is
        task type Lib_Task is         --  new
           entry Start;
        end Lib_Task;

        type My_Int is new Integer;

        function Ident (M : My_Int) return My_Int;
     end Decls;

  ::

     with Utils;
     package body Decls is
        task body Lib_Task is
        begin
           accept Start;
           Utils.Put_Val (2);
        end Lib_Task;

        function Ident (M : My_Int) return My_Int is
        begin
           return M;
        end Ident;
     end Decls;

  ::

     with Decls;
     package Utils is
        procedure Put_Val (Arg : Decls.My_Int);
     end Utils;

  ::

     with Ada.Text_IO; use Ada.Text_IO;
     package body Utils is
        procedure Put_Val (Arg : Decls.My_Int) is
        begin
           Put_Line (Arg'Img);
        end Put_Val;
     end Utils;

  ::

     with Decls;
     package Obj_Decls is             --  new
        Task_Obj : Decls.Lib_Task;
     end Obj_Decls;

  ::

     with Obj_Decls;
     procedure Main is
     begin
        Obj_Decls.Task_Obj.Start;     --  new
     end Main;

* *Use restriction No_Entry_Calls_In_Elaboration_Code*

  The issue exhibited in the original example under this section revolves
  around the body of ``Lib_Task`` blocking on an accept statement. There is
  no rule to prevent elaboration code from performing entry calls, however in
  practice this is highly unusual. In addition, the pattern of starting tasks
  at elaboration time and then immediately blocking on accept or select
  statements is quite common.

  If a programmer knows that elaboration code will not perform any entry
  calls, then the programmer can indicate that the static model should not
  process the remainder of a task body once an accept or select statement has
  been encountered. This behavior can be specified by a configuration pragma:

  ::

     pragma Restrictions (No_Entry_Calls_In_Elaboration_Code);

  In addition to the change in behavior with respect to task bodies, the
  static model will verify that no entry calls take place at elaboration time.

.. _Elaboration_Related_Compiler_Switches:

Elaboration-related Compiler Switches
=====================================

GNAT has several switches that affect the elaboration model and consequently
the elaboration order chosen by the binder.

.. index:: -gnatE  (gnat)

:switch:`-gnatE`
  Dynamic elaboration checking mode enabled

  When this switch is in effect, GNAT activates the dynamic elaboration model.

.. index:: -gnatel  (gnat)

:switch:`-gnatel`
  Turn on info messages on generated Elaborate[_All] pragmas

  When this switch is in effect, GNAT will emit the following supplementary
  information depending on the elaboration model in effect.

  - *Dynamic model*

    GNAT will indicate missing ``Elaborate`` and ``Elaborate_All`` pragmas for
    all library-level scenarios within the partition.

  - *Static model*

    GNAT will indicate all scenarios executed during elaboration. In addition,
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
elaboration order, then apart from possible cases involing dispatching calls
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

* Use switch :switch:`-gnatel` to obtain messages on generated implicit
  ``Elaborate`` and ``Elaborate_All`` pragmas. The trace information could
  indicate why a server unit must be elaborated prior to a client unit.

* If the warnings produced by the static model indicate that a task is
  involved, consider the options in section `Resolving Task Issues`_.

* If none of the steps outlined above resolve the circularity, use a more
  permissive elaboration model, in the following order:

  - Use the dynamic elaboration model, with compiler switch :switch:`-gnatE`.

  - Use the legacy static elaboration model, with compiler switch
    :switch:`-gnatH`.

  - Use the legacy dynamic elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatE`.

  - Use the relaxed legacy static elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatJ`.

  - Use the relaxed legacy dynamic elaboration model, with compiler switches
    :switch:`-gnatH` :switch:`-gnatJ` :switch:`-gnatE`.

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
