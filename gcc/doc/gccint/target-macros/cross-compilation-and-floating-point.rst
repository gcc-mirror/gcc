..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: cross compilation and floating point, floating point and cross compilation

.. _floating-point:

Cross Compilation and Floating Point
************************************

While all modern machines use twos-complement representation for integers,
there are a variety of representations for floating point numbers.  This
means that in a cross-compiler the representation of floating point numbers
in the compiled program may be different from that used in the machine
doing the compilation.

Because different representation systems may offer different amounts of
range and precision, all floating point constants must be represented in
the target machine's format.  Therefore, the cross compiler cannot
safely use the host machine's floating point arithmetic; it must emulate
the target's arithmetic.  To ensure consistency, GCC always uses
emulation to work with floating point values, even when the host and
target floating point formats are identical.

The following macros are provided by :samp:`real.h` for the compiler to
use.  All parts of the compiler which generate or optimize
floating-point calculations must use these macros.  They may evaluate
their operands more than once, so operands must not have side effects.

.. c:macro:: REAL_VALUE_TYPE

  The C data type to be used to hold a floating point value in the target
  machine's format.  Typically this is a ``struct`` containing an
  array of ``HOST_WIDE_INT``, but all code should treat it as an opaque
  quantity.

.. function:: HOST_WIDE_INT REAL_VALUE_FIX (REAL_VALUE_TYPE x)

  Truncates :samp:`{x}` to a signed integer, rounding toward zero.

.. function:: unsigned HOST_WIDE_INT REAL_VALUE_UNSIGNED_FIX (REAL_VALUE_TYPE x)

  Truncates :samp:`{x}` to an unsigned integer, rounding toward zero.  If
  :samp:`{x}` is negative, returns zero.

.. function:: REAL_VALUE_TYPE REAL_VALUE_ATOF (const char *string, machine_mode mode)

  Converts :samp:`{string}` into a floating point number in the target machine's
  representation for mode :samp:`{mode}`.  This routine can handle both
  decimal and hexadecimal floating point constants, using the syntax
  defined by the C language for both.

.. function:: int REAL_VALUE_NEGATIVE (REAL_VALUE_TYPE x)

  Returns 1 if :samp:`{x}` is negative (including negative zero), 0 otherwise.

.. function:: int REAL_VALUE_ISINF (REAL_VALUE_TYPE x)

  Determines whether :samp:`{x}` represents infinity (positive or negative).

.. function:: int REAL_VALUE_ISNAN (REAL_VALUE_TYPE x)

  Determines whether :samp:`{x}` represents a 'NaN' (not-a-number).

.. function:: REAL_VALUE_TYPE REAL_VALUE_NEGATE (REAL_VALUE_TYPE x)

  Returns the negative of the floating point value :samp:`{x}`.

.. function:: REAL_VALUE_TYPE REAL_VALUE_ABS (REAL_VALUE_TYPE x)

  Returns the absolute value of :samp:`{x}`.