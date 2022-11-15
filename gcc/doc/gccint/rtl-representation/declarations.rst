..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL declarations, declarations, RTL

.. _rtl-declarations:

Declarations
************

Declaration expression codes do not represent arithmetic operations
but rather state assertions about their operands.

.. index:: strict_low_part, subreg, in strict_low_part

:samp:`(strict_low_part (subreg:{m} (reg:{n} {r}) 0))`
  This expression code is used in only one context: as the destination operand of a
  ``set`` expression.  In addition, the operand of this expression
  must be a non-paradoxical ``subreg`` expression.

  The presence of ``strict_low_part`` says that the part of the
  register which is meaningful in mode :samp:`{n}`, but is not part of
  mode :samp:`{m}`, is not to be altered.  Normally, an assignment to such
  a subreg is allowed to have undefined effects on the rest of the
  register when :samp:`{m}` is smaller than :samp:`REGMODE_NATURAL_SIZE ({n})`.
