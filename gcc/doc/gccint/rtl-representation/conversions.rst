..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: conversions, machine mode conversions

.. _conversions:

Conversions
***********

All conversions between machine modes must be represented by
explicit conversion operations.  For example, an expression
which is the sum of a byte and a full word cannot be written as
``(plus:SI (reg:QI 34) (reg:SI 80))`` because the ``plus``
operation requires two operands of the same machine mode.
Therefore, the byte-sized operand is enclosed in a conversion
operation, as in

.. code-block:: c++

  (plus:SI (sign_extend:SI (reg:QI 34)) (reg:SI 80))

The conversion operation is not a mere placeholder, because there
may be more than one way of converting from a given starting mode
to the desired final mode.  The conversion operation code says how
to do it.

For all conversion operations, :samp:`{x}` must not be ``VOIDmode``
because the mode in which to do the conversion would not be known.
The conversion must either be done at compile-time or :samp:`{x}`
must be placed into a register.

.. index:: sign_extend

:samp:`(sign_extend:{m} {x})`
  Represents the result of sign-extending the value :samp:`{x}`
  to machine mode :samp:`{m}`.  :samp:`{m}` must be a fixed-point mode
  and :samp:`{x}` a fixed-point value of a mode narrower than :samp:`{m}`.

  .. index:: zero_extend

:samp:`(zero_extend:{m} {x})`
  Represents the result of zero-extending the value :samp:`{x}`
  to machine mode :samp:`{m}`.  :samp:`{m}` must be a fixed-point mode
  and :samp:`{x}` a fixed-point value of a mode narrower than :samp:`{m}`.

  .. index:: float_extend

:samp:`(float_extend:{m} {x})`
  Represents the result of extending the value :samp:`{x}`
  to machine mode :samp:`{m}`.  :samp:`{m}` must be a floating point mode
  and :samp:`{x}` a floating point value of a mode narrower than :samp:`{m}`.

  .. index:: truncate

:samp:`(truncate:{m} {x})`
  Represents the result of truncating the value :samp:`{x}`
  to machine mode :samp:`{m}`.  :samp:`{m}` must be a fixed-point mode
  and :samp:`{x}` a fixed-point value of a mode wider than :samp:`{m}`.

  .. index:: ss_truncate

:samp:`(ss_truncate:{m} {x})`
  Represents the result of truncating the value :samp:`{x}`
  to machine mode :samp:`{m}`, using signed saturation in the case of
  overflow.  Both :samp:`{m}` and the mode of :samp:`{x}` must be fixed-point
  modes.

  .. index:: us_truncate

:samp:`(us_truncate:{m} {x})`
  Represents the result of truncating the value :samp:`{x}`
  to machine mode :samp:`{m}`, using unsigned saturation in the case of
  overflow.  Both :samp:`{m}` and the mode of :samp:`{x}` must be fixed-point
  modes.

  .. index:: float_truncate

:samp:`(float_truncate:{m} {x})`
  Represents the result of truncating the value :samp:`{x}`
  to machine mode :samp:`{m}`.  :samp:`{m}` must be a floating point mode
  and :samp:`{x}` a floating point value of a mode wider than :samp:`{m}`.

  .. index:: float

:samp:`(float:{m} {x})`
  Represents the result of converting fixed point value :samp:`{x}`,
  regarded as signed, to floating point mode :samp:`{m}`.

  .. index:: unsigned_float

:samp:`(unsigned_float:{m} {x})`
  Represents the result of converting fixed point value :samp:`{x}`,
  regarded as unsigned, to floating point mode :samp:`{m}`.

  .. index:: fix

:samp:`(fix:{m} {x})`
  When :samp:`{m}` is a floating-point mode, represents the result of
  converting floating point value :samp:`{x}` (valid for mode :samp:`{m}`) to an
  integer, still represented in floating point mode :samp:`{m}`, by rounding
  towards zero.

  When :samp:`{m}` is a fixed-point mode, represents the result of
  converting floating point value :samp:`{x}` to mode :samp:`{m}`, regarded as
  signed.  How rounding is done is not specified, so this operation may
  be used validly in compiling C code only for integer-valued operands.

  .. index:: unsigned_fix

:samp:`(unsigned_fix:{m} {x})`
  Represents the result of converting floating point value :samp:`{x}` to
  fixed point mode :samp:`{m}`, regarded as unsigned.  How rounding is done
  is not specified.

  .. index:: fract_convert

:samp:`(fract_convert:{m} {x})`
  Represents the result of converting fixed-point value :samp:`{x}` to
  fixed-point mode :samp:`{m}`, signed integer value :samp:`{x}` to
  fixed-point mode :samp:`{m}`, floating-point value :samp:`{x}` to
  fixed-point mode :samp:`{m}`, fixed-point value :samp:`{x}` to integer mode :samp:`{m}`
  regarded as signed, or fixed-point value :samp:`{x}` to floating-point mode :samp:`{m}`.
  When overflows or underflows happen, the results are undefined.

  .. index:: sat_fract

:samp:`(sat_fract:{m} {x})`
  Represents the result of converting fixed-point value :samp:`{x}` to
  fixed-point mode :samp:`{m}`, signed integer value :samp:`{x}` to
  fixed-point mode :samp:`{m}`, or floating-point value :samp:`{x}` to
  fixed-point mode :samp:`{m}`.
  When overflows or underflows happen, the results are saturated to the
  maximum or the minimum.

  .. index:: unsigned_fract_convert

:samp:`(unsigned_fract_convert:{m} {x})`
  Represents the result of converting fixed-point value :samp:`{x}` to
  integer mode :samp:`{m}` regarded as unsigned, or unsigned integer value :samp:`{x}` to
  fixed-point mode :samp:`{m}`.
  When overflows or underflows happen, the results are undefined.

  .. index:: unsigned_sat_fract

:samp:`(unsigned_sat_fract:{m} {x})`
  Represents the result of converting unsigned integer value :samp:`{x}` to
  fixed-point mode :samp:`{m}`.
  When overflows or underflows happen, the results are saturated to the
  maximum or the minimum.
