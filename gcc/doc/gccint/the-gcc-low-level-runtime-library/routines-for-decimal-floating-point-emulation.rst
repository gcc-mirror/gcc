..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: decimal float library, IEEE 754-2008

.. _decimal-float-library-routines:

Routines for decimal floating point emulation
*********************************************

The software decimal floating point library implements IEEE 754-2008
decimal floating point arithmetic and is only activated on selected
targets.

The software decimal floating point library supports either DPD
(Densely Packed Decimal) or BID (Binary Integer Decimal) encoding
as selected at configure time.

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: _Decimal32 __dpd_addsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal32 __bid_addsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal64 __dpd_adddd3 (_Decimal64 a, _Decimal64 b)
              _Decimal64 __bid_adddd3 (_Decimal64 a, _Decimal64 b)
              _Decimal128 __dpd_addtd3 (_Decimal128 a, _Decimal128 b)
              _Decimal128 __bid_addtd3 (_Decimal128 a, _Decimal128 b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}`.

.. function:: _Decimal32 __dpd_subsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal32 __bid_subsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal64 __dpd_subdd3 (_Decimal64 a, _Decimal64 b)
              _Decimal64 __bid_subdd3 (_Decimal64 a, _Decimal64 b)
              _Decimal128 __dpd_subtd3 (_Decimal128 a, _Decimal128 b)
              _Decimal128 __bid_subtd3 (_Decimal128 a, _Decimal128 b)

  These functions return the difference between :samp:`{b}` and :samp:`{a}` ;
  that is, :samp:`{a}` - :samp:`{b}`.

.. function:: _Decimal32 __dpd_mulsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal32 __bid_mulsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal64 __dpd_muldd3 (_Decimal64 a, _Decimal64 b)
              _Decimal64 __bid_muldd3 (_Decimal64 a, _Decimal64 b)
              _Decimal128 __dpd_multd3 (_Decimal128 a, _Decimal128 b)
              _Decimal128 __bid_multd3 (_Decimal128 a, _Decimal128 b)

  These functions return the product of :samp:`{a}` and :samp:`{b}`.

.. function:: _Decimal32 __dpd_divsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal32 __bid_divsd3 (_Decimal32 a, _Decimal32 b)
              _Decimal64 __dpd_divdd3 (_Decimal64 a, _Decimal64 b)
              _Decimal64 __bid_divdd3 (_Decimal64 a, _Decimal64 b)
              _Decimal128 __dpd_divtd3 (_Decimal128 a, _Decimal128 b)
              _Decimal128 __bid_divtd3 (_Decimal128 a, _Decimal128 b)

  These functions return the quotient of :samp:`{a}` and :samp:`{b}` ; that is,
  :samp:`{a}` / :samp:`{b}`.

.. function:: _Decimal32 __dpd_negsd2 (_Decimal32 a)
              _Decimal32 __bid_negsd2 (_Decimal32 a)
              _Decimal64 __dpd_negdd2 (_Decimal64 a)
              _Decimal64 __bid_negdd2 (_Decimal64 a)
              _Decimal128 __dpd_negtd2 (_Decimal128 a)
              _Decimal128 __bid_negtd2 (_Decimal128 a)

  These functions return the negation of :samp:`{a}`.  They simply flip the
  sign bit, so they can produce negative zero and negative NaN.

Conversion functions
^^^^^^^^^^^^^^^^^^^^

.. function:: _Decimal64 __dpd_extendsddd2 (_Decimal32 a)
              _Decimal64 __bid_extendsddd2 (_Decimal32 a)
              _Decimal128 __dpd_extendsdtd2 (_Decimal32 a)
              _Decimal128 __bid_extendsdtd2 (_Decimal32 a)
              _Decimal128 __dpd_extendddtd2 (_Decimal64 a)
              _Decimal128 __bid_extendddtd2 (_Decimal64 a)
              _Decimal32 __dpd_truncddsd2 (_Decimal64 a)
              _Decimal32 __bid_truncddsd2 (_Decimal64 a)
              _Decimal32 __dpd_trunctdsd2 (_Decimal128 a)
              _Decimal32 __bid_trunctdsd2 (_Decimal128 a)
              _Decimal64 __dpd_trunctddd2 (_Decimal128 a)
              _Decimal64 __bid_trunctddd2 (_Decimal128 a)

  These functions convert the value :samp:`{a}` from one decimal floating type
  to another.

.. function:: _Decimal64 __dpd_extendsfdd (float a)
              _Decimal64 __bid_extendsfdd (float a)
              _Decimal128 __dpd_extendsftd (float a)
              _Decimal128 __bid_extendsftd (float a)
              _Decimal128 __dpd_extenddftd (double a)
              _Decimal128 __bid_extenddftd (double a)
              _Decimal128 __dpd_extendxftd (long double a)
              _Decimal128 __bid_extendxftd (long double a)
              _Decimal32 __dpd_truncdfsd (double a)
              _Decimal32 __bid_truncdfsd (double a)
              _Decimal32 __dpd_truncxfsd (long double a)
              _Decimal32 __bid_truncxfsd (long double a)
              _Decimal32 __dpd_trunctfsd (long double a)
              _Decimal32 __bid_trunctfsd (long double a)
              _Decimal64 __dpd_truncxfdd (long double a)
              _Decimal64 __bid_truncxfdd (long double a)
              _Decimal64 __dpd_trunctfdd (long double a)
              _Decimal64 __bid_trunctfdd (long double a)

  These functions convert the value of :samp:`{a}` from a binary floating type
  to a decimal floating type of a different size.

.. function:: float __dpd_truncddsf (_Decimal64 a)
              float __bid_truncddsf (_Decimal64 a)
              float __dpd_trunctdsf (_Decimal128 a)
              float __bid_trunctdsf (_Decimal128 a)
              double __dpd_extendsddf (_Decimal32 a)
              double __bid_extendsddf (_Decimal32 a)
              double __dpd_trunctddf (_Decimal128 a)
              double __bid_trunctddf (_Decimal128 a)
              long double __dpd_extendsdxf (_Decimal32 a)
              long double __bid_extendsdxf (_Decimal32 a)
              long double __dpd_extendddxf (_Decimal64 a)
              long double __bid_extendddxf (_Decimal64 a)
              long double __dpd_trunctdxf (_Decimal128 a)
              long double __bid_trunctdxf (_Decimal128 a)
              long double __dpd_extendsdtf (_Decimal32 a)
              long double __bid_extendsdtf (_Decimal32 a)
              long double __dpd_extendddtf (_Decimal64 a)
              long double __bid_extendddtf (_Decimal64 a)

  These functions convert the value of :samp:`{a}` from a decimal floating type
  to a binary floating type of a different size.

.. function:: _Decimal32 __dpd_extendsfsd (float a)
              _Decimal32 __bid_extendsfsd (float a)
              _Decimal64 __dpd_extenddfdd (double a)
              _Decimal64 __bid_extenddfdd (double a)
              _Decimal128 __dpd_extendtftd (long double a)
              _Decimal128 __bid_extendtftd (long double a)
              float __dpd_truncsdsf (_Decimal32 a)
              float __bid_truncsdsf (_Decimal32 a)
              double __dpd_truncdddf (_Decimal64 a)
              double __bid_truncdddf (_Decimal64 a)
              long double __dpd_trunctdtf (_Decimal128 a)
              long double __bid_trunctdtf (_Decimal128 a)

  These functions convert the value of :samp:`{a}` between decimal and
  binary floating types of the same size.

.. function:: int __dpd_fixsdsi (_Decimal32 a)
              int __bid_fixsdsi (_Decimal32 a)
              int __dpd_fixddsi (_Decimal64 a)
              int __bid_fixddsi (_Decimal64 a)
              int __dpd_fixtdsi (_Decimal128 a)
              int __bid_fixtdsi (_Decimal128 a)

  These functions convert :samp:`{a}` to a signed integer.

.. function:: long __dpd_fixsddi (_Decimal32 a)
              long __bid_fixsddi (_Decimal32 a)
              long __dpd_fixdddi (_Decimal64 a)
              long __bid_fixdddi (_Decimal64 a)
              long __dpd_fixtddi (_Decimal128 a)
              long __bid_fixtddi (_Decimal128 a)

  These functions convert :samp:`{a}` to a signed long.

.. function:: unsigned int __dpd_fixunssdsi (_Decimal32 a)
              unsigned int __bid_fixunssdsi (_Decimal32 a)
              unsigned int __dpd_fixunsddsi (_Decimal64 a)
              unsigned int __bid_fixunsddsi (_Decimal64 a)
              unsigned int __dpd_fixunstdsi (_Decimal128 a)
              unsigned int __bid_fixunstdsi (_Decimal128 a)

  These functions convert :samp:`{a}` to an unsigned integer.  Negative values all become zero.

.. function:: unsigned long __dpd_fixunssddi (_Decimal32 a)
              unsigned long __bid_fixunssddi (_Decimal32 a)
              unsigned long __dpd_fixunsdddi (_Decimal64 a)
              unsigned long __bid_fixunsdddi (_Decimal64 a)
              unsigned long __dpd_fixunstddi (_Decimal128 a)
              unsigned long __bid_fixunstddi (_Decimal128 a)

  These functions convert :samp:`{a}` to an unsigned long.  Negative values
  all become zero.

.. function:: _Decimal32 __dpd_floatsisd (int i)
              _Decimal32 __bid_floatsisd (int i)
              _Decimal64 __dpd_floatsidd (int i)
              _Decimal64 __bid_floatsidd (int i)
              _Decimal128 __dpd_floatsitd (int i)
              _Decimal128 __bid_floatsitd (int i)

  These functions convert :samp:`{i}`, a signed integer, to decimal floating point.

.. function:: _Decimal32 __dpd_floatdisd (long i)
              _Decimal32 __bid_floatdisd (long i)
              _Decimal64 __dpd_floatdidd (long i)
              _Decimal64 __bid_floatdidd (long i)
              _Decimal128 __dpd_floatditd (long i)
              _Decimal128 __bid_floatditd (long i)

  These functions convert :samp:`{i}`, a signed long, to decimal floating point.

.. function:: _Decimal32 __dpd_floatunssisd (unsigned int i)
              _Decimal32 __bid_floatunssisd (unsigned int i)
              _Decimal64 __dpd_floatunssidd (unsigned int i)
              _Decimal64 __bid_floatunssidd (unsigned int i)
              _Decimal128 __dpd_floatunssitd (unsigned int i)
              _Decimal128 __bid_floatunssitd (unsigned int i)

  These functions convert :samp:`{i}`, an unsigned integer, to decimal floating point.

.. function:: _Decimal32 __dpd_floatunsdisd (unsigned long i)
              _Decimal32 __bid_floatunsdisd (unsigned long i)
              _Decimal64 __dpd_floatunsdidd (unsigned long i)
              _Decimal64 __bid_floatunsdidd (unsigned long i)
              _Decimal128 __dpd_floatunsditd (unsigned long i)
              _Decimal128 __bid_floatunsditd (unsigned long i)

  These functions convert :samp:`{i}`, an unsigned long, to decimal floating point.

Comparison functions
^^^^^^^^^^^^^^^^^^^^

.. function:: int __dpd_unordsd2 (_Decimal32 a, _Decimal32 b)
              int __bid_unordsd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_unorddd2 (_Decimal64 a, _Decimal64 b)
              int __bid_unorddd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_unordtd2 (_Decimal128 a, _Decimal128 b)
              int __bid_unordtd2 (_Decimal128 a, _Decimal128 b)

  These functions return a nonzero value if either argument is NaN, otherwise 0.

There is also a complete group of higher level functions which
correspond directly to comparison operators.  They implement the ISO C
semantics for floating-point comparisons, taking NaN into account.
Pay careful attention to the return values defined for each set.
Under the hood, all of these routines are implemented as

.. code-block:: c++

    if (__bid_unordXd2 (a, b))
      return E;
    return __bid_cmpXd2 (a, b);

where :samp:`{E}` is a constant chosen to give the proper behavior for
NaN.  Thus, the meaning of the return value is different for each set.
Do not rely on this implementation; only the semantics documented
below are guaranteed.

.. function:: int __dpd_eqsd2 (_Decimal32 a, _Decimal32 b)
              int __bid_eqsd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_eqdd2 (_Decimal64 a, _Decimal64 b)
              int __bid_eqdd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_eqtd2 (_Decimal128 a, _Decimal128 b)
              int __bid_eqtd2 (_Decimal128 a, _Decimal128 b)

  These functions return zero if neither argument is NaN, and :samp:`{a}` and
  :samp:`{b}` are equal.

.. function:: int __dpd_nesd2 (_Decimal32 a, _Decimal32 b)
              int __bid_nesd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_nedd2 (_Decimal64 a, _Decimal64 b)
              int __bid_nedd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_netd2 (_Decimal128 a, _Decimal128 b)
              int __bid_netd2 (_Decimal128 a, _Decimal128 b)

  These functions return a nonzero value if either argument is NaN, or
  if :samp:`{a}` and :samp:`{b}` are unequal.

.. function:: int __dpd_gesd2 (_Decimal32 a, _Decimal32 b)
              int __bid_gesd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_gedd2 (_Decimal64 a, _Decimal64 b)
              int __bid_gedd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_getd2 (_Decimal128 a, _Decimal128 b)
              int __bid_getd2 (_Decimal128 a, _Decimal128 b)

  These functions return a value greater than or equal to zero if
  neither argument is NaN, and :samp:`{a}` is greater than or equal to
  :samp:`{b}`.

.. function:: int __dpd_ltsd2 (_Decimal32 a, _Decimal32 b)
              int __bid_ltsd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_ltdd2 (_Decimal64 a, _Decimal64 b)
              int __bid_ltdd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_lttd2 (_Decimal128 a, _Decimal128 b)
              int __bid_lttd2 (_Decimal128 a, _Decimal128 b)

  These functions return a value less than zero if neither argument is
  NaN, and :samp:`{a}` is strictly less than :samp:`{b}`.

.. function:: int __dpd_lesd2 (_Decimal32 a, _Decimal32 b)
              int __bid_lesd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_ledd2 (_Decimal64 a, _Decimal64 b)
              int __bid_ledd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_letd2 (_Decimal128 a, _Decimal128 b)
              int __bid_letd2 (_Decimal128 a, _Decimal128 b)

  These functions return a value less than or equal to zero if neither
  argument is NaN, and :samp:`{a}` is less than or equal to :samp:`{b}`.

.. function:: int __dpd_gtsd2 (_Decimal32 a, _Decimal32 b)
              int __bid_gtsd2 (_Decimal32 a, _Decimal32 b)
              int __dpd_gtdd2 (_Decimal64 a, _Decimal64 b)
              int __bid_gtdd2 (_Decimal64 a, _Decimal64 b)
              int __dpd_gttd2 (_Decimal128 a, _Decimal128 b)
              int __bid_gttd2 (_Decimal128 a, _Decimal128 b)

  These functions return a value greater than zero if neither argument
  is NaN, and :samp:`{a}` is strictly greater than :samp:`{b}`.
