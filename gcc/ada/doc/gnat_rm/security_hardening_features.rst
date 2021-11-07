.. _Security_Hardening_Features:

***************************
Security Hardening Features
***************************

This chapter describes Ada extensions aimed at security hardening that
are provided by GNAT.

.. Register Scrubbing:

Register Scrubbing
==================

GNAT can generate code to zero-out hardware registers before returning
from a subprogram.

It can be enabled with the *-fzero-call-used-regs* command line
option, to affect all subprograms in a compilation, and with a
:samp:`Machine_Attribute` pragma, to affect only specific subprograms.

.. code-block:: ada

     procedure Foo;
     pragma Machine_Attribute (Foo, "zero_call_used_regs", "used");
     --  Before returning, Foo scrubs only call-clobbered registers
     --  that it uses itself.

     function Bar return Integer;
     pragma Machine_Attribute (Bar, "zero_call_used_regs", "all");
     --  Before returning, Bar scrubs all call-clobbered registers.


For usage and more details on the command line option, and on the
``zero_call_used_regs`` attribute, see :title:`Using the GNU Compiler
Collection (GCC)`.


.. Stack Scrubbing:

Stack Scrubbing
===============

GNAT can generate code to zero-out stack frames used by subprograms.

It can be activated with the :samp:`Machine_Attribute` pragma, on
specific subprograms and variables.

.. code-block:: ada

     function Foo returns Integer;
     pragma Machine_Attribute (Foo, "strub");
     --  Foo and its callers are modified so as to scrub the stack
     --  space used by Foo after it returns.

     procedure Bar;
     pragma Machine_Attribute (Bar, "strub", "internal");
     --  Bar is turned into a wrapper for its original body,
     --  and they scrub the stack used by the original body.

     Var : Integer;
     pragma Machine_Attribute (Var, "strub");
     --  Reading from Var in a subprogram enables stack scrubbing
     --  of the stack space used by the subprogram.


There are also *-fstrub* command line options to control default
settings.  For usage and more details on the command line option, and
on the ``strub`` attribute, see :title:`Using the GNU Compiler
Collection (GCC)`.

Note that Ada secondary stacks are not scrubbed.  The restriction
``No_Secondary_Stack`` avoids their use, and thus their accidental
preservation of data that should be scrubbed.

Also note that the machine attribute is not integrated in the Ada type
system.  Though it may modify subprogram and variable interfaces, it
is not fully reflected in Ada types, ``Access`` attributes, renaming
and overriding.  Every access type, renaming, and overriding and
overridden dispatching operations that may refer to an entity with an
attribute-modified interface must be annotated with the same
interface-modifying attribute, or with an interface-compatible one.

Even then, the pragma is currently only functional when applied to
subprograms and scalar variables; other uses, such as directly on
types and subtypes, may be silently ignored.  Specifically, it is not
currently recommended to rely on any effects this pragma might be
expected to have when calling subprograms through access-to-subprogram
variables.


.. Hardened Conditionals:

Hardened Conditionals
=====================

GNAT can harden conditionals to protect against control flow attacks.

This is accomplished by two complementary transformations, each
activated by a separate command-line option.

The option *-fharden-compares* enables hardening of compares that
compute results stored in variables, adding verification that the
reversed compare yields the opposite result.

The option *-fharden-conditional-branches* enables hardening of
compares that guard conditional branches, adding verification of the
reversed compare to both execution paths.

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
files of the corresponding passes, through command line options
*-fdump-tree-hardcmp* and *-fdump-tree-hardcbr*, respectively.

They are separate options, however, because of the significantly
different performance impact of the hardening transformations.
