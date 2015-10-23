.. _Implementation_of_Specific_Ada_Features:

***************************************
Implementation of Specific Ada Features
***************************************

This chapter describes the GNAT implementation of several Ada language
facilities.

.. _Machine_Code_Insertions:

Machine Code Insertions
=======================

.. index:: Machine Code insertions

Package `Machine_Code` provides machine code support as described
in the Ada Reference Manual in two separate forms:

*
  Machine code statements, consisting of qualified expressions that
  fit the requirements of RM section 13.8.
*
  An intrinsic callable procedure, providing an alternative mechanism of
  including machine instructions in a subprogram.

The two features are similar, and both are closely related to the mechanism
provided by the asm instruction in the GNU C compiler.  Full understanding
and use of the facilities in this package requires understanding the asm
instruction, see the section on Extended Asm in
:title:`Using_the_GNU_Compiler_Collection_(GCC)`.

Calls to the function `Asm` and the procedure `Asm` have identical
semantic restrictions and effects as described below.  Both are provided so
that the procedure call can be used as a statement, and the function call
can be used to form a code_statement.

Consider this C `asm` instruction:

::

     asm ("fsinx %1 %0" : "=f" (result) : "f" (angle));


The equivalent can be written for GNAT as:

.. code-block:: ada

  Asm ("fsinx %1 %0",
       My_Float'Asm_Output ("=f", result),
       My_Float'Asm_Input  ("f",  angle));


The first argument to `Asm` is the assembler template, and is
identical to what is used in GNU C.  This string must be a static
expression.  The second argument is the output operand list.  It is
either a single `Asm_Output` attribute reference, or a list of such
references enclosed in parentheses (technically an array aggregate of
such references).

The `Asm_Output` attribute denotes a function that takes two
parameters.  The first is a string, the second is the name of a variable
of the type designated by the attribute prefix.  The first (string)
argument is required to be a static expression and designates the
constraint (see the section on Constraints in
:title:`Using_the_GNU_Compiler_Collection_(GCC)`)
for the parameter; e.g., what kind of register is required.  The second
argument is the variable to be written or updated with the
result.  The possible values for constraint are the same as those used in
the RTL, and are dependent on the configuration file used to build the
GCC back end.  If there are no output operands, then this argument may
either be omitted, or explicitly given as `No_Output_Operands`.
No support is provided for GNU C's symbolic names for output parameters.

The second argument of ``my_float'Asm_Output`` functions as
though it were an `out` parameter, which is a little curious, but
all names have the form of expressions, so there is no syntactic
irregularity, even though normally functions would not be permitted
`out` parameters.  The third argument is the list of input
operands.  It is either a single `Asm_Input` attribute reference, or
a list of such references enclosed in parentheses (technically an array
aggregate of such references).

The `Asm_Input` attribute denotes a function that takes two
parameters.  The first is a string, the second is an expression of the
type designated by the prefix.  The first (string) argument is required
to be a static expression, and is the constraint for the parameter,
(e.g., what kind of register is required).  The second argument is the
value to be used as the input argument.  The possible values for the
constraint are the same as those used in the RTL, and are dependent on
the configuration file used to built the GCC back end.
No support is provided for GNU C's symbolic names for input parameters.

If there are no input operands, this argument may either be omitted, or
explicitly given as `No_Input_Operands`.  The fourth argument, not
present in the above example, is a list of register names, called the
*clobber* argument.  This argument, if given, must be a static string
expression, and is a space or comma separated list of names of registers
that must be considered destroyed as a result of the `Asm` call.  If
this argument is the null string (the default value), then the code
generator assumes that no additional registers are destroyed.
In addition to registers, the special clobbers `memory` and
`cc` as described in the GNU C docs are both supported.

The fifth argument, not present in the above example, called the
*volatile* argument, is by default `False`.  It can be set to
the literal value `True` to indicate to the code generator that all
optimizations with respect to the instruction specified should be
suppressed, and in particular an instruction that has outputs
will still be generated, even if none of the outputs are
used.  See :title:`Using_the_GNU_Compiler_Collection_(GCC)`
for the full description.
Generally it is strongly advisable to use Volatile for any ASM statement
that is missing either input or output operands or to avoid unwanted
optimizations. A warning is generated if this advice is not followed.

No support is provided for GNU C's `asm goto` feature.

The `Asm` subprograms may be used in two ways.  First the procedure
forms can be used anywhere a procedure call would be valid, and
correspond to what the RM calls 'intrinsic' routines.  Such calls can
be used to intersperse machine instructions with other Ada statements.
Second, the function forms, which return a dummy value of the limited
private type `Asm_Insn`, can be used in code statements, and indeed
this is the only context where such calls are allowed.  Code statements
appear as aggregates of the form:

.. code-block:: ada

  Asm_Insn'(Asm (...));
  Asm_Insn'(Asm_Volatile (...));

In accordance with RM rules, such code statements are allowed only
within subprograms whose entire body consists of such statements.  It is
not permissible to intermix such statements with other Ada statements.

Typically the form using intrinsic procedure calls is more convenient
and more flexible.  The code statement form is provided to meet the RM
suggestion that such a facility should be made available.  The following
is the exact syntax of the call to `Asm`. As usual, if named notation
is used, the arguments may be given in arbitrary order, following the
normal rules for use of positional and named arguments:

::

  ASM_CALL ::= Asm (
                   [Template =>] static_string_EXPRESSION
                 [,[Outputs  =>] OUTPUT_OPERAND_LIST      ]
                 [,[Inputs   =>] INPUT_OPERAND_LIST       ]
                 [,[Clobber  =>] static_string_EXPRESSION ]
                 [,[Volatile =>] static_boolean_EXPRESSION] )

  OUTPUT_OPERAND_LIST ::=
    [PREFIX.]No_Output_Operands
  | OUTPUT_OPERAND_ATTRIBUTE
  | (OUTPUT_OPERAND_ATTRIBUTE {,OUTPUT_OPERAND_ATTRIBUTE})

  OUTPUT_OPERAND_ATTRIBUTE ::=
    SUBTYPE_MARK'Asm_Output (static_string_EXPRESSION, NAME)

  INPUT_OPERAND_LIST ::=
    [PREFIX.]No_Input_Operands
  | INPUT_OPERAND_ATTRIBUTE
  | (INPUT_OPERAND_ATTRIBUTE {,INPUT_OPERAND_ATTRIBUTE})

  INPUT_OPERAND_ATTRIBUTE ::=
    SUBTYPE_MARK'Asm_Input (static_string_EXPRESSION, EXPRESSION)

The identifiers `No_Input_Operands` and `No_Output_Operands`
are declared in the package `Machine_Code` and must be referenced
according to normal visibility rules. In particular if there is no
`use` clause for this package, then appropriate package name
qualification is required.

.. _GNAT_Implementation_of_Tasking:

GNAT Implementation of Tasking
==============================

This chapter outlines the basic GNAT approach to tasking (in particular,
a multi-layered library for portability) and discusses issues related
to compliance with the Real-Time Systems Annex.

.. _Mapping_Ada_Tasks_onto_the_Underlying_Kernel_Threads:

Mapping Ada Tasks onto the Underlying Kernel Threads
----------------------------------------------------

GNAT's run-time support comprises two layers:

* GNARL (GNAT Run-time Layer)
* GNULL (GNAT Low-level Library)

In GNAT, Ada's tasking services rely on a platform and OS independent
layer known as GNARL.  This code is responsible for implementing the
correct semantics of Ada's task creation, rendezvous, protected
operations etc.

GNARL decomposes Ada's tasking semantics into simpler lower level
operations such as create a thread, set the priority of a thread,
yield, create a lock, lock/unlock, etc.  The spec for these low-level
operations constitutes GNULLI, the GNULL Interface.  This interface is
directly inspired from the POSIX real-time API.

If the underlying executive or OS implements the POSIX standard
faithfully, the GNULL Interface maps as is to the services offered by
the underlying kernel.  Otherwise, some target dependent glue code maps
the services offered by the underlying kernel to the semantics expected
by GNARL.

Whatever the underlying OS (VxWorks, UNIX, Windows, etc.) the
key point is that each Ada task is mapped on a thread in the underlying
kernel.  For example, in the case of VxWorks, one Ada task = one VxWorks task.

In addition Ada task priorities map onto the underlying thread priorities.
Mapping Ada tasks onto the underlying kernel threads has several advantages:

*
  The underlying scheduler is used to schedule the Ada tasks.  This
  makes Ada tasks as efficient as kernel threads from a scheduling
  standpoint.

*
  Interaction with code written in C containing threads is eased
  since at the lowest level Ada tasks and C threads map onto the same
  underlying kernel concept.

*
  When an Ada task is blocked during I/O the remaining Ada tasks are
  able to proceed.

*
  On multiprocessor systems Ada tasks can execute in parallel.

Some threads libraries offer a mechanism to fork a new process, with the
child process duplicating the threads from the parent.
GNAT does not
support this functionality when the parent contains more than one task.

.. index:: Forking a new process

.. _Ensuring_Compliance_with_the_Real-Time_Annex:

Ensuring Compliance with the Real-Time Annex
--------------------------------------------

.. index:: Real-Time Systems Annex compliance

Although mapping Ada tasks onto
the underlying threads has significant advantages, it does create some
complications when it comes to respecting the scheduling semantics
specified in the real-time annex (Annex D).

For instance the Annex D requirement for the `FIFO_Within_Priorities`
scheduling policy states:

  *When the active priority of a ready task that is not running
  changes, or the setting of its base priority takes effect, the
  task is removed from the ready queue for its old active priority
  and is added at the tail of the ready queue for its new active
  priority, except in the case where the active priority is lowered
  due to the loss of inherited priority, in which case the task is
  added at the head of the ready queue for its new active priority.*

While most kernels do put tasks at the end of the priority queue when
a task changes its priority, (which respects the main
FIFO_Within_Priorities requirement), almost none keep a thread at the
beginning of its priority queue when its priority drops from the loss
of inherited priority.

As a result most vendors have provided incomplete Annex D implementations.

The GNAT run-time, has a nice cooperative solution to this problem
which ensures that accurate FIFO_Within_Priorities semantics are
respected.

The principle is as follows.  When an Ada task T is about to start
running, it checks whether some other Ada task R with the same
priority as T has been suspended due to the loss of priority
inheritance.  If this is the case, T yields and is placed at the end of
its priority queue.  When R arrives at the front of the queue it
executes.

Note that this simple scheme preserves the relative order of the tasks
that were ready to execute in the priority queue where R has been
placed at the end.

.. _GNAT_Implementation_of_Shared_Passive_Packages:

GNAT Implementation of Shared Passive Packages
==============================================

.. index:: Shared passive packages

GNAT fully implements the :index:`pragma <pragma Shared_Passive>`
`Shared_Passive` for
the purpose of designating shared passive packages.
This allows the use of passive partitions in the
context described in the Ada Reference Manual; i.e., for communication
between separate partitions of a distributed application using the
features in Annex E.

.. index:: Annex E

.. index:: Distribution Systems Annex

However, the implementation approach used by GNAT provides for more
extensive usage as follows:

*Communication between separate programs*
  This allows separate programs to access the data in passive
  partitions, using protected objects for synchronization where
  needed. The only requirement is that the two programs have a
  common shared file system. It is even possible for programs
  running on different machines with different architectures
  (e.g., different endianness) to communicate via the data in
  a passive partition.

*Persistence between program runs*
  The data in a passive package can persist from one run of a
  program to another, so that a later program sees the final
  values stored by a previous run of the same program.

The implementation approach used is to store the data in files. A
separate stream file is created for each object in the package, and
an access to an object causes the corresponding file to be read or
written.

.. index:: SHARED_MEMORY_DIRECTORY environment variable

The environment variable `SHARED_MEMORY_DIRECTORY` should be
set to the directory to be used for these files.
The files in this directory
have names that correspond to their fully qualified names. For
example, if we have the package

.. code-block:: ada

  package X is
    pragma Shared_Passive (X);
    Y : Integer;
    Z : Float;
  end X;

and the environment variable is set to `/stemp/`, then the files created
will have the names:

::

  /stemp/x.y
  /stemp/x.z


These files are created when a value is initially written to the object, and
the files are retained until manually deleted. This provides the persistence
semantics. If no file exists, it means that no partition has assigned a value
to the variable; in this case the initial value declared in the package
will be used. This model ensures that there are no issues in synchronizing
the elaboration process, since elaboration of passive packages elaborates the
initial values, but does not create the files.

The files are written using normal `Stream_IO` access.
If you want to be able
to communicate between programs or partitions running on different
architectures, then you should use the XDR versions of the stream attribute
routines, since these are architecture independent.

If active synchronization is required for access to the variables in the
shared passive package, then as described in the Ada Reference Manual, the
package may contain protected objects used for this purpose. In this case
a lock file (whose name is :file:`___lock` (three underscores)
is created in the shared memory directory.

.. index:: ___lock file (for shared passive packages)

This is used to provide the required locking
semantics for proper protected object synchronization.

GNAT supports shared passive packages on all platforms
except for OpenVMS.

.. _Code_Generation_for_Array_Aggregates:

Code Generation for Array Aggregates
====================================

Aggregates have a rich syntax and allow the user to specify the values of
complex data structures by means of a single construct.  As a result, the
code generated for aggregates can be quite complex and involve loops, case
statements and multiple assignments.  In the simplest cases, however, the
compiler will recognize aggregates whose components and constraints are
fully static, and in those cases the compiler will generate little or no
executable code.  The following is an outline of the code that GNAT generates
for various aggregate constructs.  For further details, you will find it
useful to examine the output produced by the -gnatG flag to see the expanded
source that is input to the code generator.  You may also want to examine
the assembly code generated at various levels of optimization.

The code generated for aggregates depends on the context, the component values,
and the type.  In the context of an object declaration the code generated is
generally simpler than in the case of an assignment.  As a general rule, static
component values and static subtypes also lead to simpler code.

.. _Static_constant_aggregates_with_static_bounds:

Static constant aggregates with static bounds
---------------------------------------------

For the declarations:

.. code-block:: ada

      type One_Dim is array (1..10) of integer;
      ar0 : constant One_Dim := (1, 2, 3, 4, 5, 6, 7, 8, 9, 0);


GNAT generates no executable code: the constant ar0 is placed in static memory.
The same is true for constant aggregates with named associations:


.. code-block:: ada

      Cr1 : constant One_Dim := (4 => 16, 2 => 4, 3 => 9, 1 => 1, 5 .. 10 => 0);
      Cr3 : constant One_Dim := (others => 7777);


The same is true for multidimensional constant arrays such as:

.. code-block:: ada

      type two_dim is array (1..3, 1..3) of integer;
      Unit : constant two_dim := ( (1,0,0), (0,1,0), (0,0,1));


The same is true for arrays of one-dimensional arrays: the following are
static:


.. code-block:: ada

  type ar1b  is array (1..3) of boolean;
  type ar_ar is array (1..3) of ar1b;
  None  : constant ar1b := (others => false);     --  fully static
  None2 : constant ar_ar := (1..3 => None);       --  fully static


However, for multidimensional aggregates with named associations, GNAT will
generate assignments and loops, even if all associations are static.  The
following two declarations generate a loop for the first dimension, and
individual component assignments for the second dimension:


.. code-block:: ada

  Zero1: constant two_dim := (1..3 => (1..3 => 0));
  Zero2: constant two_dim := (others => (others => 0));


.. _Constant_aggregates_with_unconstrained_nominal_types:

Constant aggregates with unconstrained nominal types
----------------------------------------------------

In such cases the aggregate itself establishes the subtype, so that
associations with `others` cannot be used.  GNAT determines the
bounds for the actual subtype of the aggregate, and allocates the
aggregate statically as well.  No code is generated for the following:


.. code-block:: ada

      type One_Unc is array (natural range <>) of integer;
      Cr_Unc : constant One_Unc := (12,24,36);


.. _Aggregates_with_static_bounds:

Aggregates with static bounds
-----------------------------

In all previous examples the aggregate was the initial (and immutable) value
of a constant.  If the aggregate initializes a variable, then code is generated
for it as a combination of individual assignments and loops over the target
object.  The declarations


.. code-block:: ada

         Cr_Var1 : One_Dim := (2, 5, 7, 11, 0, 0, 0, 0, 0, 0);
         Cr_Var2 : One_Dim := (others > -1);


generate the equivalent of


.. code-block:: ada

         Cr_Var1 (1) := 2;
         Cr_Var1 (2) := 3;
         Cr_Var1 (3) := 5;
         Cr_Var1 (4) := 11;

         for I in Cr_Var2'range loop
            Cr_Var2 (I) := -1;
         end loop;


.. _Aggregates_with_nonstatic_bounds:

Aggregates with nonstatic bounds
---------------------------------

If the bounds of the aggregate are not statically compatible with the bounds
of the nominal subtype  of the target, then constraint checks have to be
generated on the bounds.  For a multidimensional array, constraint checks may
have to be applied to sub-arrays individually, if they do not have statically
compatible subtypes.

.. _Aggregates_in_assignment_statements:

Aggregates in assignment statements
-----------------------------------

In general, aggregate assignment requires the construction of a temporary,
and a copy from the temporary to the target of the assignment.  This is because
it is not always possible to convert the assignment into a series of individual
component assignments.  For example, consider the simple case:


.. code-block:: ada

          A := (A(2), A(1));


This cannot be converted into:


.. code-block:: ada

          A(1) := A(2);
          A(2) := A(1);


So the aggregate has to be built first in a separate location, and then
copied into the target.  GNAT recognizes simple cases where this intermediate
step is not required, and the assignments can be performed in place, directly
into the target.  The following sufficient criteria are applied:

*
  The bounds of the aggregate are static, and the associations are static.
*
  The components of the aggregate are static constants, names of
  simple variables that are not renamings, or expressions not involving
  indexed components whose operands obey these rules.

If any of these conditions are violated, the aggregate will be built in
a temporary (created either by the front-end or the code generator) and then
that temporary will be copied onto the target.

.. _The_Size_of_Discriminated_Records_with_Default_Discriminants:

The Size of Discriminated Records with Default Discriminants
============================================================

If a discriminated type `T` has discriminants with default values, it is
possible to declare an object of this type without providing an explicit
constraint:


.. code-block:: ada

  type Size is range 1..100;

  type Rec (D : Size := 15) is record
     Name : String (1..D);
  end T;

  Word : Rec;


Such an object is said to be *unconstrained*.
The discriminant of the object
can be modified by a full assignment to the object, as long as it preserves the
relation between the value of the discriminant, and the value of the components
that depend on it:


.. code-block:: ada

  Word := (3, "yes");

  Word := (5, "maybe");

  Word := (5, "no"); -- raises Constraint_Error

In order to support this behavior efficiently, an unconstrained object is
given the maximum size that any value of the type requires. In the case
above, `Word` has storage for the discriminant and for
a `String` of length 100.
It is important to note that unconstrained objects do not require dynamic
allocation. It would be an improper implementation to place on the heap those
components whose size depends on discriminants. (This improper implementation
was used by some Ada83 compilers, where the `Name` component above
would have
been stored as a pointer to a dynamic string). Following the principle that
dynamic storage management should never be introduced implicitly,
an Ada compiler should reserve the full size for an unconstrained declared
object, and place it on the stack.

This maximum size approach
has been a source of surprise to some users, who expect the default
values of the discriminants to determine the size reserved for an
unconstrained object: "If the default is 15, why should the object occupy
a larger size?"
The answer, of course, is that the discriminant may be later modified,
and its full range of values must be taken into account. This is why the
declaration:


.. code-block:: ada

  type Rec (D : Positive := 15) is record
     Name : String (1..D);
  end record;

  Too_Large : Rec;

is flagged by the compiler with a warning:
an attempt to create `Too_Large` will raise `Storage_Error`,
because the required size includes `Positive'Last`
bytes. As the first example indicates, the proper approach is to declare an
index type of 'reasonable' range so that unconstrained objects are not too
large.

One final wrinkle: if the object is declared to be `aliased`, or if it is
created in the heap by means of an allocator, then it is *not*
unconstrained:
it is constrained by the default values of the discriminants, and those values
cannot be modified by full assignment. This is because in the presence of
aliasing all views of the object (which may be manipulated by different tasks,
say) must be consistent, so it is imperative that the object, once created,
remain invariant.

.. _Strict_Conformance_to_the_Ada_Reference_Manual:

Strict Conformance to the Ada Reference Manual
==============================================

The dynamic semantics defined by the Ada Reference Manual impose a set of
run-time checks to be generated. By default, the GNAT compiler will insert many
run-time checks into the compiled code, including most of those required by the
Ada Reference Manual. However, there are two checks that are not enabled in
the default mode for efficiency reasons: checks for access before elaboration
on subprogram calls, and stack overflow checking (most operating systems do not
perform this check by default).

Strict conformance to the Ada Reference Manual can be achieved by adding two
compiler options for dynamic checks for access-before-elaboration on subprogram
calls and generic instantiations (*-gnatE*), and stack overflow checking
(*-fstack-check*).

Note that the result of a floating point arithmetic operation in overflow and
invalid situations, when the `Machine_Overflows` attribute of the result
type is `False`, is to generate IEEE NaN and infinite values. This is the
case for machines compliant with the IEEE floating-point standard, but on
machines that are not fully compliant with this standard, such as Alpha, the
*-mieee* compiler flag must be used for achieving IEEE confirming
behavior (although at the cost of a significant performance penalty), so
infinite and NaN values are properly generated.
