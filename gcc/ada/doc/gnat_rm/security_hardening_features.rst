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
