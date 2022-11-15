..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: M32R/D

.. index:: M32R/D options

.. _m32r-d-options:

M32R/D Options
^^^^^^^^^^^^^^

These :option:`-m` options are defined for Renesas M32R/D architectures:

.. option:: -m32r2

  Generate code for the M32R/2.

.. option:: -m32rx

  Generate code for the M32R/X.

.. option:: -m32r

  Generate code for the M32R.  This is the default.

.. option:: -mmodel=small

  Assume all objects live in the lower 16MB of memory (so that their addresses
  can be loaded with the ``ld24`` instruction), and assume all subroutines
  are reachable with the ``bl`` instruction.
  This is the default.

  The addressability of a particular object can be set with the
  ``model`` attribute.

.. option:: -mmodel=medium

  Assume objects may be anywhere in the 32-bit address space (the compiler
  generates ``seth/add3`` instructions to load their addresses), and
  assume all subroutines are reachable with the ``bl`` instruction.

.. option:: -mmodel=large

  Assume objects may be anywhere in the 32-bit address space (the compiler
  generates ``seth/add3`` instructions to load their addresses), and
  assume subroutines may not be reachable with the ``bl`` instruction
  (the compiler generates the much slower ``seth/add3/jl``
  instruction sequence).

.. option:: -msdata=none

  Disable use of the small data area.  Variables are put into
  one of ``.data``, ``.bss``, or ``.rodata`` (unless the
  ``section`` attribute has been specified).
  This is the default.

  The small data area consists of sections ``.sdata`` and ``.sbss``.
  Objects may be explicitly put in the small data area with the
  ``section`` attribute using one of these sections.

.. option:: -msdata=sdata

  Put small global and static data in the small data area, but do not
  generate special code to reference them.

.. option:: -msdata=use

  Put small global and static data in the small data area, and generate
  special instructions to reference them.

.. index:: smaller data references

.. option:: -G {num}

  Put global and static objects less than or equal to :samp:`{num}` bytes
  into the small data or BSS sections instead of the normal data or BSS
  sections.  The default value of :samp:`{num}` is 8.
  The :option:`-msdata` option must be set to one of :samp:`sdata` or :samp:`use`
  for this option to have any effect.

  All modules should be compiled with the same :option:`-G num` value.
  Compiling with different values of :samp:`{num}` may or may not work; if it
  doesn't the linker gives an error message---incorrect code is not
  generated.

.. option:: -mdebug

  Makes the M32R-specific code in the compiler display some statistics
  that might help in debugging programs.

.. option:: -malign-loops

  Align all loops to a 32-byte boundary.

.. option:: -mno-align-loops

  Do not enforce a 32-byte alignment for loops.  This is the default.

.. index:: missue-rate=number

.. option:: -missue-rate={number}

  Issue :samp:`{number}` instructions per cycle.  :samp:`{number}` can only be 1
  or 2.

.. index:: mbranch-cost=number

.. option:: -mbranch-cost={number}

  :samp:`{number}` can only be 1 or 2.  If it is 1 then branches are
  preferred over conditional code, if it is 2, then the opposite applies.

.. index:: mflush-trap=number

.. option:: -mflush-trap={number}

  Specifies the trap number to use to flush the cache.  The default is
  12.  Valid numbers are between 0 and 15 inclusive.

.. option:: -mno-flush-trap

  Specifies that the cache cannot be flushed by using a trap.

.. index:: mflush-func=name

.. option:: -mflush-func={name}

  Specifies the name of the operating system function to call to flush
  the cache.  The default is :samp:`_flush_cache`, but a function call
  is only used if a trap is not available.

.. option:: -mno-flush-func

  Indicates that there is no OS function for flushing the cache.
