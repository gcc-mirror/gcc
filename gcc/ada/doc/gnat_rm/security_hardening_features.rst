.. _Security_Hardening_Features:

***************************
Security Hardening Features
***************************

This chapter describes Ada extensions aimed at security hardening that
are provided by GNAT.

The features in this chapter are currently experimental and subject to
change.

.. Register Scrubbing:

Register Scrubbing
==================

GNAT can generate code to zero-out hardware registers before returning
from a subprogram.

It can be enabled with the :switch:`-fzero-call-used-regs={choice}`
command-line option, to affect all subprograms in a compilation, and
with a :samp:`Machine_Attribute` pragma, to affect only specific
subprograms.

.. code-block:: ada

     procedure Foo;
     pragma Machine_Attribute (Foo, "zero_call_used_regs", "used");
     --  Before returning, Foo scrubs only call-clobbered registers
     --  that it uses itself.

     function Bar return Integer;
     pragma Machine_Attribute (Bar, "zero_call_used_regs", "all");
     --  Before returning, Bar scrubs all call-clobbered registers.


For usage and more details on the command-line option, on the
``zero_call_used_regs`` attribute, and on their use with other
programming languages, see :title:`Using the GNU Compiler Collection
(GCC)`.


.. Stack Scrubbing:

Stack Scrubbing
===============

GNAT can generate code to zero-out stack frames used by subprograms.

It can be activated with the :samp:`Machine_Attribute` pragma, on
specific subprograms and variables, or their types.  (This attribute
always applies to a type, even when it is associated with a subprogram
or a variable.)

.. code-block:: ada

     function Foo returns Integer;
     pragma Machine_Attribute (Foo, "strub");
     --  Foo and its callers are modified so as to scrub the stack
     --  space used by Foo after it returns.  Shorthand for:
     --  pragma Machine_Attribute (Foo, "strub", "at-calls");

     procedure Bar;
     pragma Machine_Attribute (Bar, "strub", "internal");
     --  Bar is turned into a wrapper for its original body,
     --  and they scrub the stack used by the original body.

     Var : Integer;
     pragma Machine_Attribute (Var, "strub");
     --  Reading from Var in a subprogram enables stack scrubbing
     --  of the stack space used by the subprogram.  Furthermore, if
     --  Var is declared within a subprogram, this also enables
     --  scrubbing of the stack space used by that subprogram.


There are also :switch:`-fstrub={choice}` command-line options to
control default settings.  For usage and more details on the
command-line options, on the ``strub`` attribute, and their use with
other programming languages, see :title:`Using the GNU Compiler
Collection (GCC)`.

Note that Ada secondary stacks are not scrubbed.  The restriction
``No_Secondary_Stack`` avoids their use, and thus their accidental
preservation of data that should be scrubbed.

Attributes ``Access`` and ``Unconstrained_Access`` of variables and
constants with ``strub`` enabled require types with ``strub`` enabled;
there is no way to express an access-to-strub type otherwise.
``Unchecked_Access`` bypasses this constraint, but the resulting
access type designates a non-strub type.

.. code-block:: ada

     VI : aliased Integer;
     pragma Machine_Attribute (VI, "strub");
     XsVI : access Integer := VI'Access; -- Error.
     UXsVI : access Integer := VI'Unchecked_Access; -- OK,
     --  UXsVI does *not* enable strub in subprograms that
     --  dereference it to obtain the UXsVI.all value.

     type Strub_Int is new Integer;
     pragma Machine_Attribute (Strub_Int, "strub");
     VSI : aliased Strub_Int;
     XsVSI : access Strub_Int := VSI'Access; -- OK,
     --  VSI and XsVSI.all both enable strub in subprograms that
     --  read their values.


Every access-to-subprogram type, renaming, and overriding and
overridden dispatching operations that may refer to a subprogram with
an attribute-modified interface must be annotated with the same
interface-modifying attribute.  Access-to-subprogram types can be
explicitly converted to different strub modes, as long as they are
interface-compatible (i.e., adding or removing ``at-calls`` is not
allowed).  For example, a ``strub``-``disabled`` subprogram can be
turned ``callable`` through such an explicit conversion:

.. code-block:: ada

     type TBar is access procedure;

     type TBar_Callable is access procedure;
     pragma Machine_Attribute (TBar_Callable, "strub", "callable");
     --  The attribute modifies the procedure type, rather than the
     --  access type, because of the extra argument after "strub",
     --  only applicable to subprogram types.

     Bar_Callable_Ptr : constant TBar_Callable
		:= TBar_Callable (TBar'(Bar'Access));

     procedure Bar_Callable renames Bar_Callable_Ptr.all;
     pragma Machine_Attribute (Bar_Callable, "strub", "callable");


Note that the renaming declaration is expanded to a full subprogram
body, it won't be just an alias.  Only if it is inlined will it be as
efficient as a call by dereferencing the access-to-subprogram constant
Bar_Callable_Ptr.


.. Hardened Conditionals:

Hardened Conditionals
=====================

GNAT can harden conditionals to protect against control-flow attacks.

This is accomplished by two complementary transformations, each
activated by a separate command-line option.

The option :switch:`-fharden-compares` enables hardening of compares
that compute results stored in variables, adding verification that the
reversed compare yields the opposite result.

The option :switch:`-fharden-conditional-branches` enables hardening
of compares that guard conditional branches, adding verification of
the reversed compare to both execution paths.

These transformations are introduced late in the compilation pipeline,
long after boolean expressions are decomposed into separate compares,
each one turned into either a conditional branch or a compare whose
result is stored in a boolean variable or temporary.  Compiler
optimizations, if enabled, may also turn conditional branches into
stored compares, and vice-versa, or into operations with implied
conditionals (e.g. MIN and MAX).  Conditionals may also be optimized
out entirely, if their value can be determined at compile time, and
occasionally multiple compares can be combined into one.

It is thus difficult to predict which of these two options will affect
a specific compare operation expressed in source code.  Using both
options ensures that every compare that is neither optimized out nor
optimized into implied conditionals will be hardened.

The addition of reversed compares can be observed by enabling the dump
files of the corresponding passes, through command-line options
:switch:`-fdump-tree-hardcmp` and :switch:`-fdump-tree-hardcbr`,
respectively.

They are separate options, however, because of the significantly
different performance impact of the hardening transformations.

For usage and more details on the command-line options, see
:title:`Using the GNU Compiler Collection (GCC)`.  These options can
be used with other programming languages supported by GCC.


.. Hardened Booleans:

Hardened Booleans
=================

Ada has built-in support for introducing boolean types with
alternative representations, using representation clauses:

.. code-block:: ada

   type HBool is new Boolean;
   for HBool use (16#5a#, 16#a5#);
   for HBool'Size use 8;


When validity checking is enabled, the compiler will check that
variables of such types hold values corresponding to the selected
representations.

There are multiple strategies for where to introduce validity checking
(see :switch:`-gnatV` options).  Their goal is to guard against
various kinds of programming errors, and GNAT strives to omit checks
when program logic rules out an invalid value, and optimizers may
further remove checks found to be redundant.

For additional hardening, the ``hardbool`` :samp:`Machine_Attribute`
pragma can be used to annotate boolean types with representation
clauses, so that expressions of such types used as conditions are
checked even when compiling with :switch:`-gnatVT`.

.. code-block:: ada

   pragma Machine_Attribute (HBool, "hardbool");


Note that :switch:`-gnatVn` will disable even ``hardbool`` testing.

Analogous behavior is available as a GCC extension to the C and
Objective C programming languages, through the ``hardbool`` attribute.
For usage and more details on that attribute, see :title:`Using the
GNU Compiler Collection (GCC)`.


.. Control Flow Redundancy:

Control Flow Redundancy
=======================

GNAT can guard against unexpected execution flows, such as branching
into the middle of subprograms, as in Return Oriented Programming
exploits.

In units compiled with :switch:`-fharden-control-flow-redundancy`,
subprograms are instrumented so that, every time they are called,
basic blocks take note as control flows through them, and, before
returning, subprograms verify that the taken notes are consistent with
the control-flow graph.

Functions with too many basic blocks, or with multiple return points,
call a run-time function to perform the verification.  Other functions
perform the verification inline before returning.

Optimizing the inlined verification can be quite time consuming, so
the default upper limit for the inline mode is set at 16 blocks.
Command-line option :switch:`--param hardcfr-max-inline-blocks=` can
override it.

Even though typically sparse control-flow graphs exhibit run-time
verification time nearly proportional to the block count of a
subprogram, it may become very significant for generated subprograms
with thousands of blocks.  Command-line option
:switch:`--param hardcfr-max-blocks=` can set an upper limit for
instrumentation.

For each block that is marked as visited, the mechanism checks that at
least one of its predecessors, and at least one of its successors, are
also marked as visited.

Verification is performed just before returning.  Subprogram
executions that complete by raising or propagating an exception bypass
verification-and-return points.  A subprogram that can only complete
by raising or propagating an exception may have instrumentation
disabled altogether.

The instrumentation for hardening with control flow redundancy can be
observed in dump files generated by the command-line option
:switch:`-fdump-tree-hardcfr`.

For more details on the control flow redundancy command-line options,
see :title:`Using the GNU Compiler Collection (GCC)`.  These options
can be used with other programming languages supported by GCC.
