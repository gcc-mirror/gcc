.. _Standard_and_Implementation_Defined_Restrictions:

************************************************
Standard and Implementation Defined Restrictions
************************************************

All Ada Reference Manual-defined Restriction identifiers are implemented:

* language-defined restrictions (see 13.12.1)
* tasking restrictions (see D.7)
* high integrity restrictions (see H.4)

GNAT implements additional restriction identifiers. All restrictions, whether
language defined or GNAT-specific, are listed in the following.

.. _Partition-Wide_Restrictions:

Partition-Wide Restrictions
===========================

There are two separate lists of restriction identifiers. The first
set requires consistency throughout a partition (in other words, if the
restriction identifier is used for any compilation unit in the partition,
then all compilation units in the partition must obey the restriction).

Immediate_Reclamation
---------------------
.. index:: Immediate_Reclamation

[RM H.4] This restriction ensures that, except for storage occupied by
objects created by allocators and not deallocated via unchecked
deallocation, any storage reserved at run time for an object is
immediately reclaimed when the object no longer exists.

Max_Asynchronous_Select_Nesting
-------------------------------
.. index:: Max_Asynchronous_Select_Nesting

[RM D.7] Specifies the maximum dynamic nesting level of asynchronous
selects. Violations of this restriction with a value of zero are
detected at compile time. Violations of this restriction with values
other than zero cause Storage_Error to be raised.

Max_Entry_Queue_Length
----------------------
.. index:: Max_Entry_Queue_Length

[RM D.7] This restriction is a declaration that any protected entry compiled in
the scope of the restriction has at most the specified number of
tasks waiting on the entry at any one time, and so no queue is required.
Note that this restriction is checked at run time. Violation of this
restriction results in the raising of Program_Error exception at the point of
the call.

.. index:: Max_Entry_Queue_Depth

The restriction ``Max_Entry_Queue_Depth`` is recognized as a
synonym for ``Max_Entry_Queue_Length``. This is retained for historical
compatibility purposes (and a warning will be generated for its use if
warnings on obsolescent features are activated).

Max_Protected_Entries
---------------------
.. index:: Max_Protected_Entries

[RM D.7] Specifies the maximum number of entries per protected type. The
bounds of every entry family of a protected unit shall be static, or shall be
defined by a discriminant of a subtype whose corresponding bound is static.

Max_Select_Alternatives
-----------------------
.. index:: Max_Select_Alternatives

[RM D.7] Specifies the maximum number of alternatives in a selective accept.

Max_Storage_At_Blocking
-----------------------
.. index:: Max_Storage_At_Blocking

[RM D.7] Specifies the maximum portion (in storage elements) of a task's
Storage_Size that can be retained by a blocked task. A violation of this
restriction causes Storage_Error to be raised.

Max_Task_Entries
----------------
.. index:: Max_Task_Entries

[RM D.7] Specifies the maximum number of entries
per task.  The bounds of every entry family
of a task unit shall be static, or shall be
defined by a discriminant of a subtype whose
corresponding bound is static.

Max_Tasks
---------
.. index:: Max_Tasks

[RM D.7] Specifies the maximum number of task that may be created, not
counting the creation of the environment task.  Violations of this
restriction with a value of zero are detected at compile
time. Violations of this restriction with values other than zero cause
Storage_Error to be raised.

No_Abort_Statements
-------------------
.. index:: No_Abort_Statements

[RM D.7] There are no abort_statements, and there are
no calls to Task_Identification.Abort_Task.

No_Access_Parameter_Allocators
------------------------------
.. index:: No_Access_Parameter_Allocators

[RM H.4] This restriction ensures at compile time that there are no
occurrences of an allocator as the actual parameter to an access
parameter.

No_Access_Subprograms
---------------------
.. index:: No_Access_Subprograms

[RM H.4] This restriction ensures at compile time that there are no
declarations of access-to-subprogram types.

No_Allocators
-------------
.. index:: No_Allocators

[RM H.4] This restriction ensures at compile time that there are no
occurrences of an allocator.

No_Anonymous_Allocators
-----------------------
.. index:: No_Anonymous_Allocators

[RM H.4] This restriction ensures at compile time that there are no
occurrences of an allocator of anonymous access type.

No_Asynchronous_Control
-----------------------
.. index:: No_Asynchronous_Control

[RM J.13] This restriction ensures at compile time that there are no semantic
dependences on the predefined package Asynchronous_Task_Control.

No_Calendar
-----------
.. index:: No_Calendar

[GNAT] This restriction ensures at compile time that there are no semantic
dependences on package Calendar.

No_Coextensions
---------------
.. index:: No_Coextensions

[RM H.4] This restriction ensures at compile time that there are no
coextensions. See 3.10.2.

No_Default_Initialization
-------------------------
.. index:: No_Default_Initialization

[GNAT] This restriction prohibits any instance of default initialization
of variables.  The binder implements a consistency rule which prevents
any unit compiled without the restriction from with'ing a unit with the
restriction (this allows the generation of initialization procedures to
be skipped, since you can be sure that no call is ever generated to an
initialization procedure in a unit with the restriction active). If used
in conjunction with Initialize_Scalars or Normalize_Scalars, the effect
is to prohibit all cases of variables declared without a specific
initializer (including the case of OUT scalar parameters).

No_Delay
--------
.. index:: No_Delay

[RM H.4] This restriction ensures at compile time that there are no
delay statements and no semantic dependences on package Calendar.

No_Dependence
-------------
.. index:: No_Dependence

[RM 13.12.1] This restriction ensures at compile time that there are no
dependences on a library unit. For GNAT, this includes implicit implementation
dependences on units of the runtime library that are created by the compiler
to support specific constructs of the language. Here are some examples:

* ``System.Arith_64``: 64-bit arithmetics for 32-bit platforms,
* ``System.Arith_128``: 128-bit arithmetics for 64-bit platforms,
* ``System.Memory``: heap memory allocation routines,
* ``System.Memory_Compare``: memory comparison routine (aka ``memcmp`` for C),
* ``System.Memory_Copy``: memory copy routine (aka ``memcpy`` for C),
* ``System.Memory_Move``: memoy move routine (aka ``memmove`` for C),
* ``System.Memory_Set``: memory set routine (aka ``memset`` for C),
* ``System.Stack_Checking[.Operations]``: stack checking without MMU,
* ``System.GCC``: support routines from the GCC library.

No_Direct_Boolean_Operators
---------------------------
.. index:: No_Direct_Boolean_Operators

[GNAT] This restriction ensures that no logical operators (and/or/xor)
are used on operands of type Boolean (or any type derived from Boolean).
This is intended for use in safety critical programs where the certification
protocol requires the use of short-circuit (and then, or else) forms for all
composite boolean operations.

No_Dispatch
-----------
.. index:: No_Dispatch

[RM H.4] This restriction ensures at compile time that there are no
occurrences of ``T'Class``, for any (tagged) subtype ``T``.

No_Dispatching_Calls
--------------------
.. index:: No_Dispatching_Calls

[GNAT] This restriction ensures at compile time that the code generated by the
compiler involves no dispatching calls. The use of this restriction allows the
safe use of record extensions, classwide membership tests and other classwide
features not involving implicit dispatching. This restriction ensures that
the code contains no indirect calls through a dispatching mechanism. Note that
this includes internally-generated calls created by the compiler, for example
in the implementation of class-wide objects assignments. The
membership test is allowed in the presence of this restriction, because its
implementation requires no dispatching.
This restriction is comparable to the official Ada restriction
``No_Dispatch`` except that it is a bit less restrictive in that it allows
all classwide constructs that do not imply dispatching.
The following example indicates constructs that violate this restriction.


.. code-block:: ada

  package Pkg is
    type T is tagged record
      Data : Natural;
    end record;
    procedure P (X : T);

    type DT is new T with record
      More_Data : Natural;
    end record;
    procedure Q (X : DT);
  end Pkg;

  with Pkg; use Pkg;
  procedure Example is
    procedure Test (O : T'Class) is
      N : Natural := O'Size; --  Error: Dispatching call
      C : T'Class := O;      --  Error: implicit Dispatching Call
    begin
      if O in DT'Class then  --  OK   : Membership test
         Q (DT (O));         --  OK   : Type conversion plus direct call
      else
         P (O);              --  Error: Dispatching call
      end if;
    end Test;

    Obj : DT;
  begin
    P (Obj);                 --  OK   : Direct call
    P (T (Obj));             --  OK   : Type conversion plus direct call
    P (T'Class (Obj));       --  Error: Dispatching call

    Test (Obj);              --  OK   : Type conversion

    if Obj in T'Class then   --  OK   : Membership test
       null;
    end if;
  end Example;


No_Dynamic_Attachment
---------------------
.. index:: No_Dynamic_Attachment

[RM D.7] This restriction ensures that there is no call to any of the
operations defined in package Ada.Interrupts
(Is_Reserved, Is_Attached, Current_Handler, Attach_Handler, Exchange_Handler,
Detach_Handler, and Reference).

.. index:: No_Dynamic_Interrupts

The restriction ``No_Dynamic_Interrupts`` is recognized as a
synonym for ``No_Dynamic_Attachment``. This is retained for historical
compatibility purposes (and a warning will be generated for its use if
warnings on obsolescent features are activated).

No_Dynamic_Priorities
---------------------
.. index:: No_Dynamic_Priorities

[RM D.7] There are no semantic dependencies on the package Dynamic_Priorities.

No_Entry_Calls_In_Elaboration_Code
----------------------------------
.. index:: No_Entry_Calls_In_Elaboration_Code

[GNAT] This restriction ensures at compile time that no task or protected entry
calls are made during elaboration code.  As a result of the use of this
restriction, the compiler can assume that no code past an accept statement
in a task can be executed at elaboration time.

No_Enumeration_Maps
-------------------
.. index:: No_Enumeration_Maps

[GNAT] This restriction ensures at compile time that no operations requiring
enumeration maps are used (that is Image and Value attributes applied
to enumeration types).

No_Exception_Handlers
---------------------
.. index:: No_Exception_Handlers

[GNAT] This restriction ensures at compile time that there are no explicit
exception handlers. It also indicates that no exception propagation will
be provided. In this mode, exceptions may be raised but will result in
an immediate call to the last chance handler, a routine that the user
must define with the following profile:


.. code-block:: ada

  procedure Last_Chance_Handler
    (Source_Location : System.Address; Line : Integer);
  pragma Export (C, Last_Chance_Handler,
                 "__gnat_last_chance_handler");


The parameter is a C null-terminated string representing a message to be
associated with the exception (typically the source location of the raise
statement generated by the compiler). The Line parameter when nonzero
represents the line number in the source program where the raise occurs.

No_Exception_Propagation
------------------------
.. index:: No_Exception_Propagation

[GNAT] This restriction guarantees that exceptions are never propagated
to an outer subprogram scope. The only case in which an exception may
be raised is when the handler is statically in the same subprogram, so
that the effect of a raise is essentially like a goto statement. Any
other raise statement (implicit or explicit) will be considered
unhandled. Exception handlers are allowed, but may not contain an
exception occurrence identifier (exception choice). In addition, use of
the package GNAT.Current_Exception is not permitted, and reraise
statements (raise with no operand) are not permitted.

No_Exception_Registration
-------------------------
.. index:: No_Exception_Registration

[GNAT] This restriction ensures at compile time that no stream operations for
types Exception_Id or Exception_Occurrence are used. This also makes it
impossible to pass exceptions to or from a partition with this restriction
in a distributed environment. If this restriction is active, the generated
code is simplified by omitting the otherwise-required global registration
of exceptions when they are declared.

No_Exceptions
-------------
.. index:: No_Exceptions

[RM H.4] This restriction ensures at compile time that there are no
raise statements and no exception handlers and also suppresses the
generation of language-defined run-time checks.


No_Finalization
---------------
.. index:: No_Finalization

[GNAT] This restriction disables the language features described in
chapter 7.6 of the Ada 2005 RM as well as all form of code generation
performed by the compiler to support these features. The following types
are no longer considered controlled when this restriction is in effect:

*
  ``Ada.Finalization.Controlled``
*
  ``Ada.Finalization.Limited_Controlled``
*
  Derivations from ``Controlled`` or ``Limited_Controlled``
*
  Class-wide types
*
  Protected types
*
  Task types
*
  Array and record types with controlled components

The compiler no longer generates code to initialize, finalize or adjust an
object or a nested component, either declared on the stack or on the heap. The
deallocation of a controlled object no longer finalizes its contents.

No_Fixed_Point
--------------
.. index:: No_Fixed_Point

[RM H.4] This restriction ensures at compile time that there are no
occurrences of fixed point types and operations.

No_Floating_Point
-----------------
.. index:: No_Floating_Point

[RM H.4] This restriction ensures at compile time that there are no
occurrences of floating point types and operations.

No_Implicit_Conditionals
------------------------
.. index:: No_Implicit_Conditionals

[GNAT] This restriction ensures that the generated code does not contain any
implicit conditionals, either by modifying the generated code where possible,
or by rejecting any construct that would otherwise generate an implicit
conditional. Note that this check does not include run time constraint
checks, which on some targets may generate implicit conditionals as
well. To control the latter, constraint checks can be suppressed in the
normal manner. Constructs generating implicit conditionals include comparisons
of composite objects and the Max/Min attributes.

No_Implicit_Dynamic_Code
------------------------
.. index:: No_Implicit_Dynamic_Code
.. index:: trampoline

[GNAT] This restriction prevents the compiler from building 'trampolines'.
This is a structure that is built on the stack and contains dynamic
code to be executed at run time. On some targets, a trampoline is
built for the following features: ``Access``,
``Unrestricted_Access``, or ``Address`` of a nested subprogram;
nested task bodies; primitive operations of nested tagged types.
Trampolines do not work on machines that prevent execution of stack
data. For example, on windows systems, enabling DEP (data execution
protection) will cause trampolines to raise an exception.
Trampolines are also quite slow at run time.

On many targets, trampolines have been largely eliminated. Look at the
version of system.ads for your target --- if it has
Always_Compatible_Rep equal to False, then trampolines are largely
eliminated. In particular, a trampoline is built for the following
features: ``Address`` of a nested subprogram;
``Access`` or ``Unrestricted_Access`` of a nested subprogram,
but only if pragma Favor_Top_Level applies, or the access type has a
foreign-language convention; primitive operations of nested tagged
types.

No_Implicit_Heap_Allocations
----------------------------
.. index:: No_Implicit_Heap_Allocations

[RM D.7] No constructs are allowed to cause implicit heap allocation.

No_Implicit_Protected_Object_Allocations
----------------------------------------
.. index:: No_Implicit_Protected_Object_Allocations

[GNAT] No constructs are allowed to cause implicit heap allocation of a
protected object.

No_Implicit_Task_Allocations
----------------------------
.. index:: No_Implicit_Task_Allocations

[GNAT] No constructs are allowed to cause implicit heap allocation of a task.

No_Initialize_Scalars
---------------------
.. index:: No_Initialize_Scalars

[GNAT] This restriction ensures that no unit in the partition is compiled with
pragma Initialize_Scalars. This allows the generation of more efficient
code, and in particular eliminates dummy null initialization routines that
are otherwise generated for some record and array types.

No_IO
-----
.. index:: No_IO

[RM H.4] This restriction ensures at compile time that there are no
dependences on any of the library units Sequential_IO, Direct_IO,
Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, or Stream_IO.

No_Local_Allocators
-------------------
.. index:: No_Local_Allocators

[RM H.4] This restriction ensures at compile time that there are no
occurrences of an allocator in subprograms, generic subprograms, tasks,
and entry bodies.

No_Local_Protected_Objects
--------------------------
.. index:: No_Local_Protected_Objects

[RM D.7] This restriction ensures at compile time that protected objects are
only declared at the library level.

No_Local_Tagged_Types
---------------------
.. index:: No_Local_Tagged_Types

[GNAT] This restriction ensures at compile time that tagged types are only
declared at the library level.

No_Local_Timing_Events
----------------------
.. index:: No_Local_Timing_Events

[RM D.7] All objects of type Ada.Real_Time.Timing_Events.Timing_Event are
declared at the library level.

No_Long_Long_Integers
---------------------
.. index:: No_Long_Long_Integers

[GNAT] This partition-wide restriction forbids any explicit reference to
type Standard.Long_Long_Integer, and also forbids declaring range types whose
implicit base type is Long_Long_Integer, and modular types whose size exceeds
Long_Integer'Size.

No_Multiple_Elaboration
-----------------------
.. index:: No_Multiple_Elaboration

[GNAT] When this restriction is active and the static elaboration model is
used, and -fpreserve-control-flow is not used, the compiler is allowed to
suppress the elaboration counter normally associated with the unit, even if
the unit has elaboration code. This counter is typically used to check for
access before elaboration and to control multiple elaboration attempts. If the
restriction is used, then the situations in which multiple elaboration is
possible, including non-Ada main programs and Stand Alone libraries, are not
permitted and will be diagnosed by the binder.

No_Nested_Finalization
----------------------
.. index:: No_Nested_Finalization

[RM D.7] All objects requiring finalization are declared at the library level.

No_Protected_Type_Allocators
----------------------------
.. index:: No_Protected_Type_Allocators

[RM D.7] This restriction ensures at compile time that there are no allocator
expressions that attempt to allocate protected objects.

No_Protected_Types
------------------
.. index:: No_Protected_Types

[RM H.4] This restriction ensures at compile time that there are no
declarations of protected types or protected objects.

No_Recursion
------------
.. index:: No_Recursion

[RM H.4] A program execution is erroneous if a subprogram is invoked as
part of its execution.

No_Reentrancy
-------------
.. index:: No_Reentrancy

[RM H.4] A program execution is erroneous if a subprogram is executed by
two tasks at the same time.

No_Relative_Delay
-----------------
.. index:: No_Relative_Delay

[RM D.7] This restriction ensures at compile time that there are no delay
relative statements and prevents expressions such as ``delay 1.23;`` from
appearing in source code.

No_Requeue_Statements
---------------------
.. index:: No_Requeue_Statements

[RM D.7] This restriction ensures at compile time that no requeue statements
are permitted and prevents keyword ``requeue`` from being used in source
code.

.. index:: No_Requeue

The restriction ``No_Requeue`` is recognized as a
synonym for ``No_Requeue_Statements``. This is retained for historical
compatibility purposes (and a warning will be generated for its use if
warnings on oNobsolescent features are activated).

No_Secondary_Stack
------------------
.. index:: No_Secondary_Stack

[GNAT] This restriction ensures at compile time that the generated code
does not contain any reference to the secondary stack.  The secondary
stack is used to implement functions returning unconstrained objects
(arrays or records) on some targets. Suppresses the allocation of
secondary stacks for tasks (excluding the environment task) at run time.

No_Select_Statements
--------------------
.. index:: No_Select_Statements

[RM D.7] This restriction ensures at compile time no select statements of any
kind are permitted, that is the keyword ``select`` may not appear.

No_Specific_Termination_Handlers
--------------------------------
.. index:: No_Specific_Termination_Handlers

[RM D.7] There are no calls to Ada.Task_Termination.Set_Specific_Handler
or to Ada.Task_Termination.Specific_Handler.

No_Specification_of_Aspect
--------------------------
.. index:: No_Specification_of_Aspect

[RM 13.12.1] This restriction checks at compile time that no aspect
specification, attribute definition clause, or pragma is given for a
given aspect.

No_Standard_Allocators_After_Elaboration
----------------------------------------
.. index:: No_Standard_Allocators_After_Elaboration

[RM D.7] Specifies that an allocator using a standard storage pool
should never be evaluated at run time after the elaboration of the
library items of the partition has completed. Otherwise, Storage_Error
is raised.

No_Standard_Storage_Pools
-------------------------
.. index:: No_Standard_Storage_Pools

[GNAT] This restriction ensures at compile time that no access types
use the standard default storage pool.  Any access type declared must
have an explicit Storage_Pool attribute defined specifying a
user-defined storage pool.

No_Stream_Optimizations
-----------------------
.. index:: No_Stream_Optimizations

[GNAT] This restriction affects the performance of stream operations on types
``String``, ``Wide_String`` and ``Wide_Wide_String``. By default, the
compiler uses block reads and writes when manipulating ``String`` objects
due to their superior performance. When this restriction is in effect, the
compiler performs all IO operations on a per-character basis.

No_Streams
----------
.. index:: No_Streams

[GNAT] This restriction ensures at compile/bind time that there are no
stream objects created and no use of stream attributes.
This restriction does not forbid dependences on the package
``Ada.Streams``. So it is permissible to with
``Ada.Streams`` (or another package that does so itself)
as long as no actual stream objects are created and no
stream attributes are used.

Note that the use of restriction allows optimization of tagged types,
since they do not need to worry about dispatching stream operations.
To take maximum advantage of this space-saving optimization, any
unit declaring a tagged type should be compiled with the restriction,
though this is not required.

No_Tagged_Type_Registration
---------------------------
.. index:: No_Tagged_Type_Registration

[GNAT] If this restriction is active, then class-wide streaming
attributes are not supported. In addition, the subprograms in
Ada.Tags are not supported.
If this restriction is active, the generated code is simplified by
omitting the otherwise-required global registration of tagged types when they
are declared. This restriction may be necessary in order to also apply
the No_Elaboration_Code restriction.

No_Task_Allocators
------------------
.. index:: No_Task_Allocators

[RM D.7] There are no allocators for task types
or types containing task subcomponents.

No_Task_At_Interrupt_Priority
-----------------------------
.. index:: No_Task_At_Interrupt_Priority

[GNAT] This restriction ensures at compile time that there is no
Interrupt_Priority aspect or pragma for a task or a task type. As
a consequence, the tasks are always created with a priority below
that an interrupt priority.

No_Task_Attributes_Package
--------------------------
.. index:: No_Task_Attributes_Package

[GNAT] This restriction ensures at compile time that there are no implicit or
explicit dependencies on the package ``Ada.Task_Attributes``.

.. index:: No_Task_Attributes

The restriction ``No_Task_Attributes`` is recognized as a synonym
for ``No_Task_Attributes_Package``. This is retained for historical
compatibility purposes (and a warning will be generated for its use if
warnings on obsolescent features are activated).

No_Task_Hierarchy
-----------------
.. index:: No_Task_Hierarchy

[RM D.7] All (non-environment) tasks depend
directly on the environment task of the partition.

No_Task_Termination
-------------------
.. index:: No_Task_Termination

[RM D.7] Tasks that terminate are erroneous.

No_Tasking
----------
.. index:: No_Tasking

[GNAT] This restriction prevents the declaration of tasks or task types
throughout the partition.  It is similar in effect to the use of
``Max_Tasks => 0`` except that violations are caught at compile time
and cause an error message to be output either by the compiler or
binder.

No_Terminate_Alternatives
-------------------------
.. index:: No_Terminate_Alternatives

[RM D.7] There are no selective accepts with terminate alternatives.

No_Unchecked_Access
-------------------
.. index:: No_Unchecked_Access

[RM H.4] This restriction ensures at compile time that there are no
occurrences of the Unchecked_Access attribute.

No_Unchecked_Conversion
-----------------------
.. index:: No_Unchecked_Conversion

[RM J.13] This restriction ensures at compile time that there are no semantic
dependences on the predefined generic function Unchecked_Conversion.

No_Unchecked_Deallocation
-------------------------
.. index:: No_Unchecked_Deallocation

[RM J.13] This restriction ensures at compile time that there are no semantic
dependences on the predefined generic procedure Unchecked_Deallocation.

No_Use_Of_Attribute
-------------------
.. index:: No_Use_Of_Attribute

[RM 13.12.1] This is a standard Ada 2012 restriction that is GNAT defined in
earlier versions of Ada.

No_Use_Of_Entity
----------------
.. index:: No_Use_Of_Entity

[GNAT] This restriction ensures at compile time that there are no references
to the entity given in the form ::

   No_Use_Of_Entity => Name

where ``Name`` is the fully qualified entity, for example ::

   No_Use_Of_Entity => Ada.Text_IO.Put_Line

No_Use_Of_Pragma
----------------
.. index:: No_Use_Of_Pragma

[RM 13.12.1] This is a standard Ada 2012 restriction that is GNAT defined in
earlier versions of Ada.

Pure_Barriers
-------------
.. index:: Pure_Barriers

[GNAT] This restriction ensures at compile time that protected entry
barriers are restricted to:

* components of the protected object (excluding selection from dereferences),
* constant declarations,
* named numbers,
* enumeration literals,
* integer literals,
* real literals,
* character literals,
* implicitly defined comparison operators,
* uses of the Standard."not" operator,
* short-circuit operator,
* the Count attribute

This restriction is a relaxation of the Simple_Barriers restriction,
but still ensures absence of side effects, exceptions, and recursion
during the evaluation of the barriers.

Simple_Barriers
---------------
.. index:: Simple_Barriers

[RM D.7] This restriction ensures at compile time that barriers in entry
declarations for protected types are restricted to either static boolean
expressions or references to simple boolean variables defined in the private
part of the protected type.  No other form of entry barriers is permitted.

.. index:: Boolean_Entry_Barriers

The restriction ``Boolean_Entry_Barriers`` is recognized as a
synonym for ``Simple_Barriers``. This is retained for historical
compatibility purposes (and a warning will be generated for its use if
warnings on obsolescent features are activated).

Static_Priorities
-----------------
.. index:: Static_Priorities

[GNAT] This restriction ensures at compile time that all priority expressions
are static, and that there are no dependences on the package
``Ada.Dynamic_Priorities``.

Static_Storage_Size
-------------------
.. index:: Static_Storage_Size

[GNAT] This restriction ensures at compile time that any expression appearing
in a Storage_Size pragma or attribute definition clause is static.

.. _Program_Unit_Level_Restrictions:


Program Unit Level Restrictions
===============================

The second set of restriction identifiers
does not require partition-wide consistency.
The restriction may be enforced for a single
compilation unit without any effect on any of the
other compilation units in the partition.

No_Elaboration_Code
-------------------
.. index:: No_Elaboration_Code

[GNAT] This restriction ensures at compile time that no elaboration code is
generated.  Note that this is not the same condition as is enforced
by pragma ``Preelaborate``.  There are cases in which pragma
``Preelaborate`` still permits code to be generated (e.g., code
to initialize a large array to all zeroes), and there are cases of units
which do not meet the requirements for pragma ``Preelaborate``,
but for which no elaboration code is generated.  Generally, it is
the case that preelaborable units will meet the restrictions, with
the exception of large aggregates initialized with an others_clause,
and exception declarations (which generate calls to a run-time
registry procedure).  This restriction is enforced on
a unit by unit basis, it need not be obeyed consistently
throughout a partition.

In the case of aggregates with others, if the aggregate has a dynamic
size, there is no way to eliminate the elaboration code (such dynamic
bounds would be incompatible with ``Preelaborate`` in any case). If
the bounds are static, then use of this restriction actually modifies
the code choice of the compiler to avoid generating a loop, and instead
generate the aggregate statically if possible, no matter how many times
the data for the others clause must be repeatedly generated.

It is not possible to precisely document
the constructs which are compatible with this restriction, since,
unlike most other restrictions, this is not a restriction on the
source code, but a restriction on the generated object code. For
example, if the source contains a declaration:


.. code-block:: ada

     Val : constant Integer := X;


where X is not a static constant, it may be possible, depending
on complex optimization circuitry, for the compiler to figure
out the value of X at compile time, in which case this initialization
can be done by the loader, and requires no initialization code. It
is not possible to document the precise conditions under which the
optimizer can figure this out.

Note that this the implementation of this restriction requires full
code generation. If it is used in conjunction with "semantics only"
checking, then some cases of violations may be missed.

When this restriction is active, we are not requesting control-flow
preservation with -fpreserve-control-flow, and the static elaboration model is
used, the compiler is allowed to suppress the elaboration counter normally
associated with the unit. This counter is typically used to check for access
before elaboration and to control multiple elaboration attempts.

No_Dynamic_Accessibility_Checks
-------------------------------
.. index:: No_Dynamic_Accessibility_Checks

[GNAT] No dynamic accessibility checks are generated when this restriction is
in effect. Instead, dangling references are prevented via more conservative
compile-time checking. More specifically, existing compile-time checks are
enforced but with more conservative assumptions about the accessibility levels
of the relevant entities. These conservative assumptions eliminate the need for
dynamic accessibility checks.

These new rules for computing (at compile-time) the accessibility level of an
anonymous access type T are as follows:

*
 If T is a function result type then, from the caller's perspective, its level
 is that of the innermost master enclosing the function call. From the callee's
 perspective, the level of parameters and local variables of the callee is
 statically deeper than the level of T.

 For any other accessibility level L such that the level of parameters and local
 variables of the callee is statically deeper than L, the level of T (from the
 callee's perspective) is also statically deeper than L.
*
 If T is the type of a formal parameter then, from the caller's perspective,
 its level is at least as deep as that of the type of the corresponding actual
 parameter (whatever that actual parameter might be). From the callee's
 perspective, the level of parameters and local variables of the callee is
 statically deeper than the level of T.
*
 If T is the type of a discriminant then its level is that of the discriminated
 type.
*
 If T is the type of a stand-alone object then its level is the level of the
 object.
*
 In all other cases, the level of T is as defined by the existing rules of Ada.

No_Dynamic_Sized_Objects
------------------------
.. index:: No_Dynamic_Sized_Objects

[GNAT] This restriction disallows certain constructs that might lead to the
creation of dynamic-sized composite objects (or array or discriminated type).
An array subtype indication is illegal if the bounds are not static
or references to discriminants of an enclosing type.
A discriminated subtype indication is illegal if the type has
discriminant-dependent array components or a variant part, and the
discriminants are not static. In addition, array and record aggregates are
illegal in corresponding cases. Note that this restriction does not forbid
access discriminants. It is often a good idea to combine this restriction
with No_Secondary_Stack.

No_Entry_Queue
--------------
.. index:: No_Entry_Queue

[GNAT] This restriction is a declaration that any protected entry compiled in
the scope of the restriction has at most one task waiting on the entry
at any one time, and so no queue is required.  This restriction is not
checked at compile time.  A program execution is erroneous if an attempt
is made to queue a second task on such an entry.

No_Implementation_Aspect_Specifications
---------------------------------------
.. index:: No_Implementation_Aspect_Specifications

[RM 13.12.1] This restriction checks at compile time that no
GNAT-defined aspects are present.  With this restriction, the only
aspects that can be used are those defined in the Ada Reference Manual.

No_Implementation_Attributes
----------------------------
.. index:: No_Implementation_Attributes

[RM 13.12.1] This restriction checks at compile time that no
GNAT-defined attributes are present.  With this restriction, the only
attributes that can be used are those defined in the Ada Reference
Manual.

No_Implementation_Identifiers
-----------------------------
.. index:: No_Implementation_Identifiers

[RM 13.12.1] This restriction checks at compile time that no
implementation-defined identifiers (marked with pragma Implementation_Defined)
occur within language-defined packages.

No_Implementation_Pragmas
-------------------------
.. index:: No_Implementation_Pragmas

[RM 13.12.1] This restriction checks at compile time that no
GNAT-defined pragmas are present.  With this restriction, the only
pragmas that can be used are those defined in the Ada Reference Manual.

No_Implementation_Restrictions
------------------------------
.. index:: No_Implementation_Restrictions

[GNAT] This restriction checks at compile time that no GNAT-defined restriction
identifiers (other than ``No_Implementation_Restrictions`` itself)
are present.  With this restriction, the only other restriction identifiers
that can be used are those defined in the Ada Reference Manual.

No_Implementation_Units
-----------------------
.. index:: No_Implementation_Units

[RM 13.12.1] This restriction checks at compile time that there is no
mention in the context clause of any implementation-defined descendants
of packages Ada, Interfaces, or System.

No_Implicit_Aliasing
--------------------
.. index:: No_Implicit_Aliasing

[GNAT] This restriction, which is not required to be partition-wide consistent,
requires an explicit aliased keyword for an object to which 'Access,
'Unchecked_Access, or 'Address is applied, and forbids entirely the use of
the 'Unrestricted_Access attribute for objects. Note: the reason that
Unrestricted_Access is forbidden is that it would require the prefix
to be aliased, and in such cases, it can always be replaced by
the standard attribute Unchecked_Access which is preferable.

No_Implicit_Loops
-----------------
.. index:: No_Implicit_Loops

[GNAT] This restriction ensures that the generated code of the unit marked
with this restriction does not contain any implicit ``for`` loops, either by
modifying the generated code where possible, or by rejecting any construct
that would otherwise generate an implicit ``for`` loop. If this restriction is
active, it is possible to build large array aggregates with all static
components without generating an intermediate temporary, and without generating
a loop to initialize individual components. Otherwise, a loop is created for
arrays larger than about 5000 scalar components. Note that if this restriction
is set in the spec of a package, it will not apply to its body.

No_Obsolescent_Features
-----------------------
.. index:: No_Obsolescent_Features

[RM 13.12.1] This restriction checks at compile time that no obsolescent
features are used, as defined in Annex J of the Ada Reference Manual.

No_Wide_Characters
------------------
.. index:: No_Wide_Characters

[GNAT] This restriction ensures at compile time that no uses of the types
``Wide_Character`` or ``Wide_String`` or corresponding wide
wide types
appear, and that no wide or wide wide string or character literals
appear in the program (that is literals representing characters not in
type ``Character``).

Static_Dispatch_Tables
----------------------
.. index:: Static_Dispatch_Tables

[GNAT] This restriction checks at compile time that all the artifacts
associated with dispatch tables can be placed in read-only memory.

SPARK_05
--------
.. index:: SPARK_05

[GNAT] This restriction no longer has any effect and is superseded by
SPARK 2014, whose restrictions are checked by the tool GNATprove. To check that
a codebase respects SPARK 2014 restrictions, mark the code with pragma or
aspect ``SPARK_Mode``, and run the tool GNATprove at Stone assurance level, as
follows::

  gnatprove -P project.gpr --mode=stone

or equivalently::

  gnatprove -P project.gpr --mode=check_all
