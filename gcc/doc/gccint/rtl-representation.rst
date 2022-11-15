..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL representation, representation of RTL, Register Transfer Language (RTL)

.. _rtl:

RTL Representation
------------------

The last part of the compiler work is done on a low-level intermediate
representation called Register Transfer Language.  In this language, the
instructions to be output are described, pretty much one by one, in an
algebraic form that describes what the instruction does.

RTL is inspired by Lisp lists.  It has both an internal form, made up of
structures that point at other structures, and a textual form that is used
in the machine description and in printed debugging dumps.  The textual
form uses nested parentheses to indicate the pointers in the internal form.

.. toctree::
  :maxdepth: 2

  rtl-representation/rtl-object-types
  rtl-representation/rtl-classes-and-formats
  rtl-representation/access-to-operands
  rtl-representation/access-to-special-operands
  rtl-representation/flags-in-an-rtl-expression
  rtl-representation/machine-modes
  rtl-representation/constant-expression-types
  rtl-representation/registers-and-memory
  rtl-representation/rtl-expressions-for-arithmetic
  rtl-representation/comparison-operations
  rtl-representation/bit-fields
  rtl-representation/vector-operations
  rtl-representation/conversions
  rtl-representation/declarations
  rtl-representation/side-effect-expressions
  rtl-representation/embedded-side-effects-on-addresses
  rtl-representation/assembler-instructions-as-expressions
  rtl-representation/variable-location-debug-information-in-rtl
  rtl-representation/insns
  rtl-representation/rtl-representation-of-function-call-insns
  rtl-representation/on-the-side-ssa-form-for-rtl
  rtl-representation/structure-sharing-assumptions
  rtl-representation/reading-rtl
