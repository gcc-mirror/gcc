..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: support for nested functions, trampolines for nested functions, descriptors for nested functions, nested functions, support for

.. _trampolines:

Support for Nested Functions
****************************

Taking the address of a nested function requires special compiler
handling to ensure that the static chain register is loaded when
the function is invoked via an indirect call.

GCC has traditionally supported nested functions by creating an
executable :dfn:`trampoline` at run time when the address of a nested
function is taken.  This is a small piece of code which normally
resides on the stack, in the stack frame of the containing function.
The trampoline loads the static chain register and then jumps to the
real address of the nested function.

The use of trampolines requires an executable stack, which is a
security risk.  To avoid this problem, GCC also supports another
strategy: using descriptors for nested functions.  Under this model,
taking the address of a nested function results in a pointer to a
non-executable function descriptor object.  Initializing the static chain
from the descriptor is handled at indirect call sites.

On some targets, including HPPA and IA-64, function descriptors may be
mandated by the ABI or be otherwise handled in a target-specific way
by the back end in its code generation strategy for indirect calls.
GCC also provides its own generic descriptor implementation to support the
:option:`-fno-trampolines` option.  In this case runtime detection of
function descriptors at indirect call sites relies on descriptor
pointers being tagged with a bit that is never set in bare function
addresses.  Since GCC's generic function descriptors are
not ABI-compliant, this option is typically used only on a
per-language basis (notably by Ada) or when it can otherwise be
applied to the whole program.

For languages other than Ada, the ``-ftrampolines`` and
``-fno-trampolines`` options currently have no effect, and
trampolines are always generated on platforms that need them
for nested functions.

Define the following hook if your backend either implements ABI-specified
descriptor support, or can use GCC's generic descriptor implementation
for nested functions.

.. include:: tm.rst.in
  :start-after: [TARGET_CUSTOM_FUNCTION_DESCRIPTORS]
  :end-before: [TARGET_CUSTOM_FUNCTION_DESCRIPTORS]


The following macros tell GCC how to generate code to allocate and
initialize an executable trampoline.  You can also use this interface
if your back end needs to create ABI-specified non-executable descriptors; in
this case the "trampoline" created is the descriptor containing data only.

The instructions in an executable trampoline must do two things: load
a constant address into the static chain register, and jump to the real
address of the nested function.  On CISC machines such as the m68k,
this requires two instructions, a move immediate and a jump.  Then the
two addresses exist in the trampoline as word-long immediate operands.
On RISC machines, it is often necessary to load each address into a
register in two parts.  Then pieces of each address form separate
immediate operands.

The code generated to initialize the trampoline must store the variable
parts---the static chain value and the function address---into the
immediate operands of the instructions.  On a CISC machine, this is
simply a matter of copying each address to a memory reference at the
proper offset from the start of the trampoline.  On a RISC machine, it
may be necessary to take out pieces of the address and store them
separately.

.. include:: tm.rst.in
  :start-after: [TARGET_ASM_TRAMPOLINE_TEMPLATE]
  :end-before: [TARGET_ASM_TRAMPOLINE_TEMPLATE]


.. c:macro:: TRAMPOLINE_SECTION

  Return the section into which the trampoline template is to be placed
  (see :ref:`sections`).  The default value is ``readonly_data_section``.

.. c:macro:: TRAMPOLINE_SIZE

  A C expression for the size in bytes of the trampoline, as an integer.

.. c:macro:: TRAMPOLINE_ALIGNMENT

  Alignment required for trampolines, in bits.

  If you don't define this macro, the value of ``FUNCTION_ALIGNMENT``
  is used for aligning trampolines.

.. include:: tm.rst.in
  :start-after: [TARGET_TRAMPOLINE_INIT]
  :end-before: [TARGET_TRAMPOLINE_INIT]


.. include:: tm.rst.in
  :start-after: [TARGET_EMIT_CALL_BUILTIN___CLEAR_CACHE]
  :end-before: [TARGET_EMIT_CALL_BUILTIN___CLEAR_CACHE]


.. include:: tm.rst.in
  :start-after: [TARGET_TRAMPOLINE_ADJUST_ADDRESS]
  :end-before: [TARGET_TRAMPOLINE_ADJUST_ADDRESS]


Implementing trampolines is difficult on many machines because they have
separate instruction and data caches.  Writing into a stack location
fails to clear the memory in the instruction cache, so when the program
jumps to that location, it executes the old contents.

Here are two possible solutions.  One is to clear the relevant parts of
the instruction cache whenever a trampoline is set up.  The other is to
make all trampolines identical, by having them jump to a standard
subroutine.  The former technique makes trampoline execution faster; the
latter makes initialization faster.

To clear the instruction cache when a trampoline is initialized, define
the following macro.

.. c:macro:: CLEAR_INSN_CACHE (beg, end)

  If defined, expands to a C expression clearing the *instruction
  cache* in the specified interval.  The definition of this macro would
  typically be a series of ``asm`` statements.  Both :samp:`{beg}` and
  :samp:`{end}` are pointer expressions.

To use a standard subroutine, define the following macro.  In addition,
you must make sure that the instructions in a trampoline fill an entire
cache line with identical instructions, or else ensure that the
beginning of the trampoline code is always aligned at the same point in
its cache line.  Look in :samp:`m68k.h` as a guide.

.. c:macro:: TRANSFER_FROM_TRAMPOLINE

  Define this macro if trampolines need a special subroutine to do their
  work.  The macro should expand to a series of ``asm`` statements
  which will be compiled with GCC.  They go in a library function named
  ``__transfer_from_trampoline``.

  If you need to avoid executing the ordinary prologue code of a compiled
  C function when you jump to the subroutine, you can do so by placing a
  special label of your own in the assembler code.  Use one ``asm``
  statement to generate an assembler label, and another to make the label
  global.  Then trampolines can use that label to jump directly to your
  special assembler code.
