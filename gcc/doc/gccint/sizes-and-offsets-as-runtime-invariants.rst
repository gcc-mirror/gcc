..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: polynomial integers, poly_int

.. _poly_int:

Sizes and offsets as runtime invariants
---------------------------------------

GCC allows the size of a hardware register to be a runtime invariant
rather than a compile-time constant.  This in turn means that various
sizes and offsets must also be runtime invariants rather than
compile-time constants, such as:

* the size of a general ``machine_mode`` (see :ref:`machine-modes`);

* the size of a spill slot;

* the offset of something within a stack frame;

* the number of elements in a vector;

* the size and offset of a ``mem`` rtx (see :ref:`regs-and-memory`); and

* the byte offset in a ``subreg`` rtx (see :ref:`regs-and-memory`).

The motivating example is the Arm SVE ISA, whose vector registers can be
any multiple of 128 bits between 128 and 2048 inclusive.  The compiler
normally produces code that works for all SVE register sizes, with the
actual size only being known at runtime.

GCC's main representation of such runtime invariants is the
``poly_int`` class.  This chapter describes what ``poly_int``
does, lists the available operations, and gives some general
usage guidelines.

.. toctree::
  :maxdepth: 2

  sizes-and-offsets-as-runtime-invariants/overview-of-polyint
  sizes-and-offsets-as-runtime-invariants/consequences-of-using-polyint
  sizes-and-offsets-as-runtime-invariants/comparisons-involving-polyint
  sizes-and-offsets-as-runtime-invariants/arithmetic-on-polyints
  sizes-and-offsets-as-runtime-invariants/alignment-of-polyints
  sizes-and-offsets-as-runtime-invariants/computing-bounds-on-polyints
  sizes-and-offsets-as-runtime-invariants/converting-polyints
  sizes-and-offsets-as-runtime-invariants/miscellaneous-polyint-routines
  sizes-and-offsets-as-runtime-invariants/guidelines-for-using-polyint
