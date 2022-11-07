..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: arithmetic, in RTL, math, in RTL, RTL expressions for arithmetic

.. _arithmetic:

RTL Expressions for Arithmetic
******************************

Unless otherwise specified, all the operands of arithmetic expressions
must be valid for mode :samp:`{m}`.  An operand is valid for mode :samp:`{m}`
if it has mode :samp:`{m}`, or if it is a ``const_int`` or
``const_double`` and :samp:`{m}` is a mode of class ``MODE_INT``.

For commutative binary operations, constants should be placed in the
second operand.

.. index:: plus, ss_plus, us_plus, RTL sum, RTL addition, RTL addition with signed saturation, RTL addition with unsigned saturation

:samp:`(plus:{m} {x} {y})` :samp:`(ss_plus:{m} {x} {y})` :samp:`(us_plus:{m} {x} {y})`
  These three expressions all represent the sum of the values
  represented by :samp:`{x}` and :samp:`{y}` carried out in machine mode
  :samp:`{m}`.  They differ in their behavior on overflow of integer modes.
  ``plus`` wraps round modulo the width of :samp:`{m}` ; ``ss_plus``
  saturates at the maximum signed value representable in :samp:`{m}` ;
  ``us_plus`` saturates at the maximum unsigned value.

  .. ??? What happens on overflow of floating point modes?

  .. index:: lo_sum

:samp:`(lo_sum:{m} {x} {y})`
  This expression represents the sum of :samp:`{x}` and the low-order bits
  of :samp:`{y}`.  It is used with ``high`` (see :ref:`constants`) to
  represent the typical two-instruction sequence used in RISC machines to
  reference large immediate values and/or link-time constants such
  as global memory addresses.  In the latter case, :samp:`{m}` is ``Pmode``
  and :samp:`{y}` is usually a constant expression involving ``symbol_ref``.

  The number of low order bits is machine-dependent but is
  normally the number of bits in mode :samp:`{m}` minus the number of
  bits set by ``high``.

  .. index:: minus, ss_minus, us_minus, RTL difference, RTL subtraction, RTL subtraction with signed saturation, RTL subtraction with unsigned saturation

:samp:`(minus:{m} {x} {y})` :samp:`(ss_minus:{m} {x} {y})` :samp:`(us_minus:{m} {x} {y})`
  These three expressions represent the result of subtracting :samp:`{y}`
  from :samp:`{x}`, carried out in mode :samp:`{M}`.  Behavior on overflow is
  the same as for the three variants of ``plus`` (see above).

  .. index:: compare, RTL comparison

:samp:`(compare:{m} {x} {y})`
  Represents the result of subtracting :samp:`{y}` from :samp:`{x}` for purposes
  of comparison.  The result is computed without overflow, as if with
  infinite precision.

  Of course, machines cannot really subtract with infinite precision.
  However, they can pretend to do so when only the sign of the result will
  be used, which is the case when the result is stored in the condition
  code.  And that is the *only* way this kind of expression may
  validly be used: as a value to be stored in the condition codes, in a
  register.  See :ref:`comparisons`.

  The mode :samp:`{m}` is not related to the modes of :samp:`{x}` and :samp:`{y}`, but
  instead is the mode of the condition code value.  It is some mode in class
  ``MODE_CC``, often ``CCmode``.  See :ref:`condition-code`.  If :samp:`{m}`
  is ``CCmode``, the operation returns sufficient
  information (in an unspecified format) so that any comparison operator
  can be applied to the result of the ``COMPARE`` operation.  For other
  modes in class ``MODE_CC``, the operation only returns a subset of
  this information.

  Normally, :samp:`{x}` and :samp:`{y}` must have the same mode.  Otherwise,
  ``compare`` is valid only if the mode of :samp:`{x}` is in class
  ``MODE_INT`` and :samp:`{y}` is a ``const_int`` or
  ``const_double`` with mode ``VOIDmode``.  The mode of :samp:`{x}`
  determines what mode the comparison is to be done in; thus it must not
  be ``VOIDmode``.

  If one of the operands is a constant, it should be placed in the
  second operand and the comparison code adjusted as appropriate.

  A ``compare`` specifying two ``VOIDmode`` constants is not valid
  since there is no way to know in what mode the comparison is to be
  performed; the comparison must either be folded during the compilation
  or the first operand must be loaded into a register while its mode is
  still known.

  .. index:: neg, ss_neg, us_neg, negation, negation with signed saturation, negation with unsigned saturation

:samp:`(neg:{m} {x})` :samp:`(ss_neg:{m} {x})` :samp:`(us_neg:{m} {x})`
  These two expressions represent the negation (subtraction from zero) of
  the value represented by :samp:`{x}`, carried out in mode :samp:`{m}`.  They
  differ in the behavior on overflow of integer modes.  In the case of
  ``neg``, the negation of the operand may be a number not representable
  in mode :samp:`{m}`, in which case it is truncated to :samp:`{m}`.  ``ss_neg``
  and ``us_neg`` ensure that an out-of-bounds result saturates to the
  maximum or minimum signed or unsigned value.

  .. index:: mult, ss_mult, us_mult, multiplication, product, multiplication with signed saturation, multiplication with unsigned saturation

:samp:`(mult:{m} {x} {y})` :samp:`(ss_mult:{m} {x} {y})` :samp:`(us_mult:{m} {x} {y})`
  Represents the signed product of the values represented by :samp:`{x}` and
  :samp:`{y}` carried out in machine mode :samp:`{m}`.
  ``ss_mult`` and ``us_mult`` ensure that an out-of-bounds result
  saturates to the maximum or minimum signed or unsigned value.

  Some machines support a multiplication that generates a product wider
  than the operands.  Write the pattern for this as

  .. code-block:: c++

    (mult:m (sign_extend:m x) (sign_extend:m y))

  where :samp:`{m}` is wider than the modes of :samp:`{x}` and :samp:`{y}`, which need
  not be the same.

  For unsigned widening multiplication, use the same idiom, but with
  ``zero_extend`` instead of ``sign_extend``.

  .. index:: smul_highpart, umul_highpart, high-part multiplication, multiplication high part

:samp:`(smul_highpart:{m} {x} {y})` :samp:`(umul_highpart:{m} {x} {y})`
  Represents the high-part multiplication of :samp:`{x}` and :samp:`{y}` carried
  out in machine mode :samp:`{m}`.  ``smul_highpart`` returns the high part
  of a signed multiplication, ``umul_highpart`` returns the high part
  of an unsigned multiplication.

  .. index:: fma, fused multiply-add

:samp:`(fma:{m} {x} {y} {z})`
  Represents the ``fma``, ``fmaf``, and ``fmal`` builtin
  functions, which compute :samp:`{x} * {y} + {z}`
  without doing an intermediate rounding step.

  .. index:: div, ss_div, division, signed division, signed division with signed saturation, quotient

:samp:`(div:{m} {x} {y})` :samp:`(ss_div:{m} {x} {y})`
  Represents the quotient in signed division of :samp:`{x}` by :samp:`{y}`,
  carried out in machine mode :samp:`{m}`.  If :samp:`{m}` is a floating point
  mode, it represents the exact quotient; otherwise, the integerized
  quotient.
  ``ss_div`` ensures that an out-of-bounds result saturates to the maximum
  or minimum signed value.

  Some machines have division instructions in which the operands and
  quotient widths are not all the same; you should represent
  such instructions using ``truncate`` and ``sign_extend`` as in,

  .. code-block:: c++

    (truncate:m1 (div:m2 x (sign_extend:m2 y)))

  .. index:: udiv, unsigned division, unsigned division with unsigned saturation, division

:samp:`(udiv:{m} {x} {y})` :samp:`(us_div:{m} {x} {y})`
  Like ``div`` but represents unsigned division.
  ``us_div`` ensures that an out-of-bounds result saturates to the maximum
  or minimum unsigned value.

  .. index:: mod, umod, remainder, division

:samp:`(mod:{m} {x} {y})` :samp:`(umod:{m} {x} {y})`
  Like ``div`` and ``udiv`` but represent the remainder instead of
  the quotient.

  .. index:: smin, smax, signed minimum, signed maximum

:samp:`(smin:{m} {x} {y})` :samp:`(smax:{m} {x} {y})`
  Represents the smaller (for ``smin``) or larger (for ``smax``) of
  :samp:`{x}` and :samp:`{y}`, interpreted as signed values in mode :samp:`{m}`.
  When used with floating point, if both operands are zeros, or if either
  operand is ``NaN``, then it is unspecified which of the two operands
  is returned as the result.

  .. index:: umin, umax, unsigned minimum and maximum

:samp:`(umin:{m} {x} {y})` :samp:`(umax:{m} {x} {y})`
  Like ``smin`` and ``smax``, but the values are interpreted as unsigned
  integers.

  .. index:: not, complement, bitwise, bitwise complement

:samp:`(not:{m} {x})`
  Represents the bitwise complement of the value represented by :samp:`{x}`,
  carried out in mode :samp:`{m}`, which must be a fixed-point machine mode.

  .. index:: and, logical-and, bitwise, bitwise logical-and

:samp:`(and:{m} {x} {y})`
  Represents the bitwise logical-and of the values represented by
  :samp:`{x}` and :samp:`{y}`, carried out in machine mode :samp:`{m}`, which must be
  a fixed-point machine mode.

  .. index:: ior, inclusive-or, bitwise, bitwise inclusive-or

:samp:`(ior:{m} {x} {y})`
  Represents the bitwise inclusive-or of the values represented by :samp:`{x}`
  and :samp:`{y}`, carried out in machine mode :samp:`{m}`, which must be a
  fixed-point mode.

  .. index:: xor, exclusive-or, bitwise, bitwise exclusive-or

:samp:`(xor:{m} {x} {y})`
  Represents the bitwise exclusive-or of the values represented by :samp:`{x}`
  and :samp:`{y}`, carried out in machine mode :samp:`{m}`, which must be a
  fixed-point mode.

  .. index:: ashift, ss_ashift, us_ashift, left shift, shift, arithmetic shift, arithmetic shift with signed saturation, arithmetic shift with unsigned saturation

:samp:`(ashift:{m} {x} {c})` :samp:`(ss_ashift:{m} {x} {c})` :samp:`(us_ashift:{m} {x} {c})`
  These three expressions represent the result of arithmetically shifting :samp:`{x}`
  left by :samp:`{c}` places.  They differ in their behavior on overflow of integer
  modes.  An ``ashift`` operation is a plain shift with no special behavior
  in case of a change in the sign bit; ``ss_ashift`` and ``us_ashift``
  saturates to the minimum or maximum representable value if any of the bits
  shifted out differs from the final sign bit.

  :samp:`{x}` have mode :samp:`{m}`, a fixed-point machine mode.  :samp:`{c}`
  be a fixed-point mode or be a constant with mode ``VOIDmode`` ; which
  mode is determined by the mode called for in the machine description
  entry for the left-shift instruction.  For example, on the VAX, the mode
  of :samp:`{c}` is ``QImode`` regardless of :samp:`{m}`.

  .. index:: lshiftrt, right shift, ashiftrt

:samp:`(lshiftrt:{m} {x} {c})` :samp:`(ashiftrt:{m} {x} {c})`
  Like ``ashift`` but for right shift.  Unlike the case for left shift,
  these two operations are distinct.

  .. index:: rotate, rotate, left rotate, rotatert, right rotate

:samp:`(rotate:{m} {x} {c})` :samp:`(rotatert:{m} {x} {c})`
  Similar but represent left and right rotate.  If :samp:`{c}` is a constant,
  use ``rotate``.

  .. index:: abs, ss_abs, absolute value

  :samp:`(abs:{m} {x})`
:samp:`(ss_abs:{m} {x})`
  Represents the absolute value of :samp:`{x}`, computed in mode :samp:`{m}`.
  ``ss_abs`` ensures that an out-of-bounds result saturates to the
  maximum signed value.

  .. index:: sqrt, square root

:samp:`(sqrt:{m} {x})`
  Represents the square root of :samp:`{x}`, computed in mode :samp:`{m}`.
  Most often :samp:`{m}` will be a floating point mode.

  .. index:: ffs

:samp:`(ffs:{m} {x})`
  Represents one plus the index of the least significant 1-bit in
  :samp:`{x}`, represented as an integer of mode :samp:`{m}`.  (The value is
  zero if :samp:`{x}` is zero.)  The mode of :samp:`{x}` must be :samp:`{m}`
  or ``VOIDmode``.

  .. index:: clrsb

:samp:`(clrsb:{m} {x})`
  Represents the number of redundant leading sign bits in :samp:`{x}`,
  represented as an integer of mode :samp:`{m}`, starting at the most
  significant bit position.  This is one less than the number of leading
  sign bits (either 0 or 1), with no special cases.  The mode of :samp:`{x}`
  must be :samp:`{m}` or ``VOIDmode``.

  .. index:: clz

:samp:`(clz:{m} {x})`
  Represents the number of leading 0-bits in :samp:`{x}`, represented as an
  integer of mode :samp:`{m}`, starting at the most significant bit position.
  If :samp:`{x}` is zero, the value is determined by
  ``CLZ_DEFINED_VALUE_AT_ZERO`` (see :ref:`misc`).  Note that this is one of
  the few expressions that is not invariant under widening.  The mode of
  :samp:`{x}` must be :samp:`{m}` or ``VOIDmode``.

  .. index:: ctz

:samp:`(ctz:{m} {x})`
  Represents the number of trailing 0-bits in :samp:`{x}`, represented as an
  integer of mode :samp:`{m}`, starting at the least significant bit position.
  If :samp:`{x}` is zero, the value is determined by
  ``CTZ_DEFINED_VALUE_AT_ZERO`` (see :ref:`misc`).  Except for this case,
  ``ctz(x)`` is equivalent to ``ffs(x) - 1``.  The mode of
  :samp:`{x}` must be :samp:`{m}` or ``VOIDmode``.

  .. index:: popcount

:samp:`(popcount:{m} {x})`
  Represents the number of 1-bits in :samp:`{x}`, represented as an integer of
  mode :samp:`{m}`.  The mode of :samp:`{x}` must be :samp:`{m}` or ``VOIDmode``.

  .. index:: parity

:samp:`(parity:{m} {x})`
  Represents the number of 1-bits modulo 2 in :samp:`{x}`, represented as an
  integer of mode :samp:`{m}`.  The mode of :samp:`{x}` must be :samp:`{m}` or
  ``VOIDmode``.

  .. index:: bswap

:samp:`(bswap:{m} {x})`
  Represents the value :samp:`{x}` with the order of bytes reversed, carried out
  in mode :samp:`{m}`, which must be a fixed-point machine mode.
  The mode of :samp:`{x}` must be :samp:`{m}` or ``VOIDmode``.