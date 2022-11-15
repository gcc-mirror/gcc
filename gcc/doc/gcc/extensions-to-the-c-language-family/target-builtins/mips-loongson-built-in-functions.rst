..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _mips-loongson-built-in-functions:

MIPS Loongson Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access the SIMD instructions provided by the
ST Microelectronics Loongson-2E and -2F processors.  These intrinsics,
available after inclusion of the ``loongson.h`` header file,
operate on the following 64-bit vector types:

* ``uint8x8_t``, a vector of eight unsigned 8-bit integers;

* ``uint16x4_t``, a vector of four unsigned 16-bit integers;

* ``uint32x2_t``, a vector of two unsigned 32-bit integers;

* ``int8x8_t``, a vector of eight signed 8-bit integers;

* ``int16x4_t``, a vector of four signed 16-bit integers;

* ``int32x2_t``, a vector of two signed 32-bit integers.

The intrinsics provided are listed below; each is named after the
machine instruction to which it corresponds, with suffixes added as
appropriate to distinguish intrinsics that expand to the same machine
instruction yet have different argument types.  Refer to the architecture
documentation for a description of the functionality of each
instruction.

.. code-block:: c++

  int16x4_t packsswh (int32x2_t s, int32x2_t t);
  int8x8_t packsshb (int16x4_t s, int16x4_t t);
  uint8x8_t packushb (uint16x4_t s, uint16x4_t t);
  uint32x2_t paddw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t paddh_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t paddb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t paddw_s (int32x2_t s, int32x2_t t);
  int16x4_t paddh_s (int16x4_t s, int16x4_t t);
  int8x8_t paddb_s (int8x8_t s, int8x8_t t);
  uint64_t paddd_u (uint64_t s, uint64_t t);
  int64_t paddd_s (int64_t s, int64_t t);
  int16x4_t paddsh (int16x4_t s, int16x4_t t);
  int8x8_t paddsb (int8x8_t s, int8x8_t t);
  uint16x4_t paddush (uint16x4_t s, uint16x4_t t);
  uint8x8_t paddusb (uint8x8_t s, uint8x8_t t);
  uint64_t pandn_ud (uint64_t s, uint64_t t);
  uint32x2_t pandn_uw (uint32x2_t s, uint32x2_t t);
  uint16x4_t pandn_uh (uint16x4_t s, uint16x4_t t);
  uint8x8_t pandn_ub (uint8x8_t s, uint8x8_t t);
  int64_t pandn_sd (int64_t s, int64_t t);
  int32x2_t pandn_sw (int32x2_t s, int32x2_t t);
  int16x4_t pandn_sh (int16x4_t s, int16x4_t t);
  int8x8_t pandn_sb (int8x8_t s, int8x8_t t);
  uint16x4_t pavgh (uint16x4_t s, uint16x4_t t);
  uint8x8_t pavgb (uint8x8_t s, uint8x8_t t);
  uint32x2_t pcmpeqw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t pcmpeqh_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t pcmpeqb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t pcmpeqw_s (int32x2_t s, int32x2_t t);
  int16x4_t pcmpeqh_s (int16x4_t s, int16x4_t t);
  int8x8_t pcmpeqb_s (int8x8_t s, int8x8_t t);
  uint32x2_t pcmpgtw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t pcmpgth_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t pcmpgtb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t pcmpgtw_s (int32x2_t s, int32x2_t t);
  int16x4_t pcmpgth_s (int16x4_t s, int16x4_t t);
  int8x8_t pcmpgtb_s (int8x8_t s, int8x8_t t);
  uint16x4_t pextrh_u (uint16x4_t s, int field);
  int16x4_t pextrh_s (int16x4_t s, int field);
  uint16x4_t pinsrh_0_u (uint16x4_t s, uint16x4_t t);
  uint16x4_t pinsrh_1_u (uint16x4_t s, uint16x4_t t);
  uint16x4_t pinsrh_2_u (uint16x4_t s, uint16x4_t t);
  uint16x4_t pinsrh_3_u (uint16x4_t s, uint16x4_t t);
  int16x4_t pinsrh_0_s (int16x4_t s, int16x4_t t);
  int16x4_t pinsrh_1_s (int16x4_t s, int16x4_t t);
  int16x4_t pinsrh_2_s (int16x4_t s, int16x4_t t);
  int16x4_t pinsrh_3_s (int16x4_t s, int16x4_t t);
  int32x2_t pmaddhw (int16x4_t s, int16x4_t t);
  int16x4_t pmaxsh (int16x4_t s, int16x4_t t);
  uint8x8_t pmaxub (uint8x8_t s, uint8x8_t t);
  int16x4_t pminsh (int16x4_t s, int16x4_t t);
  uint8x8_t pminub (uint8x8_t s, uint8x8_t t);
  uint8x8_t pmovmskb_u (uint8x8_t s);
  int8x8_t pmovmskb_s (int8x8_t s);
  uint16x4_t pmulhuh (uint16x4_t s, uint16x4_t t);
  int16x4_t pmulhh (int16x4_t s, int16x4_t t);
  int16x4_t pmullh (int16x4_t s, int16x4_t t);
  int64_t pmuluw (uint32x2_t s, uint32x2_t t);
  uint8x8_t pasubub (uint8x8_t s, uint8x8_t t);
  uint16x4_t biadd (uint8x8_t s);
  uint16x4_t psadbh (uint8x8_t s, uint8x8_t t);
  uint16x4_t pshufh_u (uint16x4_t dest, uint16x4_t s, uint8_t order);
  int16x4_t pshufh_s (int16x4_t dest, int16x4_t s, uint8_t order);
  uint16x4_t psllh_u (uint16x4_t s, uint8_t amount);
  int16x4_t psllh_s (int16x4_t s, uint8_t amount);
  uint32x2_t psllw_u (uint32x2_t s, uint8_t amount);
  int32x2_t psllw_s (int32x2_t s, uint8_t amount);
  uint16x4_t psrlh_u (uint16x4_t s, uint8_t amount);
  int16x4_t psrlh_s (int16x4_t s, uint8_t amount);
  uint32x2_t psrlw_u (uint32x2_t s, uint8_t amount);
  int32x2_t psrlw_s (int32x2_t s, uint8_t amount);
  uint16x4_t psrah_u (uint16x4_t s, uint8_t amount);
  int16x4_t psrah_s (int16x4_t s, uint8_t amount);
  uint32x2_t psraw_u (uint32x2_t s, uint8_t amount);
  int32x2_t psraw_s (int32x2_t s, uint8_t amount);
  uint32x2_t psubw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t psubh_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t psubb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t psubw_s (int32x2_t s, int32x2_t t);
  int16x4_t psubh_s (int16x4_t s, int16x4_t t);
  int8x8_t psubb_s (int8x8_t s, int8x8_t t);
  uint64_t psubd_u (uint64_t s, uint64_t t);
  int64_t psubd_s (int64_t s, int64_t t);
  int16x4_t psubsh (int16x4_t s, int16x4_t t);
  int8x8_t psubsb (int8x8_t s, int8x8_t t);
  uint16x4_t psubush (uint16x4_t s, uint16x4_t t);
  uint8x8_t psubusb (uint8x8_t s, uint8x8_t t);
  uint32x2_t punpckhwd_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t punpckhhw_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t punpckhbh_u (uint8x8_t s, uint8x8_t t);
  int32x2_t punpckhwd_s (int32x2_t s, int32x2_t t);
  int16x4_t punpckhhw_s (int16x4_t s, int16x4_t t);
  int8x8_t punpckhbh_s (int8x8_t s, int8x8_t t);
  uint32x2_t punpcklwd_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t punpcklhw_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t punpcklbh_u (uint8x8_t s, uint8x8_t t);
  int32x2_t punpcklwd_s (int32x2_t s, int32x2_t t);
  int16x4_t punpcklhw_s (int16x4_t s, int16x4_t t);
  int8x8_t punpcklbh_s (int8x8_t s, int8x8_t t);

.. toctree::
  :maxdepth: 2


.. _paired-single-arithmetic:

Paired-Single Arithmetic
~~~~~~~~~~~~~~~~~~~~~~~~

The table below lists the ``v2sf`` operations for which hardware
support exists.  ``a``, ``b`` and ``c`` are ``v2sf``
values and ``x`` is an integral value.

.. list-table::
   :header-rows: 1

   * - C code
     - MIPS instruction

   * - ``a + b``
     - ``add.ps``
   * - ``a - b``
     - ``sub.ps``
   * - ``-a``
     - ``neg.ps``
   * - ``a * b``
     - ``mul.ps``
   * - ``a * b + c``
     - ``madd.ps``
   * - ``a * b - c``
     - ``msub.ps``
   * - ``-(a * b + c)``
     - ``nmadd.ps``
   * - ``-(a * b - c)``
     - ``nmsub.ps``
   * - ``x ? a : b``
     - ``movn.ps`` / ``movz.ps``

Note that the multiply-accumulate instructions can be disabled
using the command-line option ``-mno-fused-madd``.

.. _paired-single-built-in-functions:

Paired-Single Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following paired-single functions map directly to a particular
MIPS instruction.  Please refer to the architecture specification
for details on what each instruction does.

.. function:: v2sf __builtin_mips_pll_ps (v2sf, v2sf)

  Pair lower lower (``pll.ps``).

.. function:: v2sf __builtin_mips_pul_ps (v2sf, v2sf)

  Pair upper lower (``pul.ps``).

.. function:: v2sf __builtin_mips_plu_ps (v2sf, v2sf)

  Pair lower upper (``plu.ps``).

.. function:: v2sf __builtin_mips_puu_ps (v2sf, v2sf)

  Pair upper upper (``puu.ps``).

.. function:: v2sf __builtin_mips_cvt_ps_s (float, float)

  Convert pair to paired single (``cvt.ps.s``).

.. function:: float __builtin_mips_cvt_s_pl (v2sf)

  Convert pair lower to single (``cvt.s.pl``).

.. function:: float __builtin_mips_cvt_s_pu (v2sf)

  Convert pair upper to single (``cvt.s.pu``).

.. function:: v2sf __builtin_mips_abs_ps (v2sf)

  Absolute value (``abs.ps``).

.. function:: v2sf __builtin_mips_alnv_ps (v2sf, v2sf, int)

  Align variable (``alnv.ps``).

  .. note::

    The value of the third parameter must be 0 or 4
    modulo 8, otherwise the result is unpredictable.  Please read the
    instruction description for details.

The following multi-instruction functions are also available.
In each case, :samp:`{cond}` can be any of the 16 floating-point conditions:
``f``, ``un``, ``eq``, ``ueq``, ``olt``, ``ult``,
``ole``, ``ule``, ``sf``, ``ngle``, ``seq``, ``ngl``,
``lt``, ``nge``, ``le`` or ``ngt``.

.. function:: v2sf __builtin_mips_movt_c_cond_ps (v2sf a, v2sf b, v2sf c, v2sf d)
.. function:: v2sf __builtin_mips_movf_c_cond_ps (v2sf a, v2sf b, v2sf c, v2sf d)

  Conditional move based on floating-point comparison (``c.cond.ps``,
  ``movt.ps`` / ``movf.ps``).

  The ``movt`` functions return the value :samp:`{x}` computed by:

  .. code-block:: c++

    c.cond.ps cc,a,b
    mov.ps x,c
    movt.ps x,d,cc

  The ``movf`` functions are similar but use ``movf.ps`` instead
  of ``movt.ps``.

.. function:: int __builtin_mips_upper_c_cond_ps (v2sf a, v2sf b)
.. function:: int __builtin_mips_lower_c_cond_ps (v2sf a, v2sf b)

  Comparison of two paired-single values (``c.cond.ps``,
  ``bc1t`` / ``bc1f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``c.cond.ps``
  and return either the upper or lower half of the result.  For example:

  .. code-block:: c++

    v2sf a, b;
    if (__builtin_mips_upper_c_eq_ps (a, b))
      upper_halves_are_equal ();
    else
      upper_halves_are_unequal ();

    if (__builtin_mips_lower_c_eq_ps (a, b))
      lower_halves_are_equal ();
    else
      lower_halves_are_unequal ();

.. _mips-3d-built-in-functions:

MIPS-3D Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~

The MIPS-3D Application-Specific Extension (ASE) includes additional
paired-single instructions that are designed to improve the performance
of 3D graphics operations.  Support for these instructions is controlled
by the :option:`-mips3d` command-line option.

The functions listed below map directly to a particular MIPS-3D
instruction.  Please refer to the architecture specification for
more details on what each instruction does.

.. function:: v2sf __builtin_mips_addr_ps (v2sf, v2sf)

  Reduction add (``addr.ps``).

.. function:: v2sf __builtin_mips_mulr_ps (v2sf, v2sf)

  Reduction multiply (``mulr.ps``).

.. function:: v2sf __builtin_mips_cvt_pw_ps (v2sf)

  Convert paired single to paired word (``cvt.pw.ps``).

.. function:: v2sf __builtin_mips_cvt_ps_pw (v2sf)

  Convert paired word to paired single (``cvt.ps.pw``).

.. function:: float __builtin_mips_recip1_s (float)
.. function:: double __builtin_mips_recip1_d (double)
.. function:: v2sf __builtin_mips_recip1_ps (v2sf)

  Reduced-precision reciprocal (sequence step 1) (``recip1.fmt``).

.. function:: float __builtin_mips_recip2_s (float, float)
.. function:: double __builtin_mips_recip2_d (double, double)
.. function:: v2sf __builtin_mips_recip2_ps (v2sf, v2sf)

  Reduced-precision reciprocal (sequence step 2) (``recip2.fmt``).

.. function:: float __builtin_mips_rsqrt1_s (float)
.. function:: double __builtin_mips_rsqrt1_d (double)
.. function:: v2sf __builtin_mips_rsqrt1_ps (v2sf)

  Reduced-precision reciprocal square root (sequence step 1)
  (``rsqrt1.fmt``).

.. function:: float __builtin_mips_rsqrt2_s (float, float)
.. function:: double __builtin_mips_rsqrt2_d (double, double)
.. function:: v2sf __builtin_mips_rsqrt2_ps (v2sf, v2sf)

  Reduced-precision reciprocal square root (sequence step 2)
  (``rsqrt2.fmt``).

The following multi-instruction functions are also available.
In each case, :samp:`{cond}` can be any of the 16 floating-point conditions:

``f``, ``un``, ``eq``, ``ueq``, ``olt``, ``ult``,
``ole``, ``ule``, ``sf``, ``ngle``, ``seq``,
``ngl``, ``lt``, ``nge``, ``le`` or ``ngt``.

.. function:: int __builtin_mips_cabs_cond_s (float a, float b)
.. function:: int __builtin_mips_cabs_cond_d (double a, double b)

  Absolute comparison of two scalar values (``cabs.cond.fmt``,
  ``bc1t`` / ``bc1f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``cabs.cond.s``
  or ``cabs.cond.d`` and return the result as a boolean value.
  For example:

  .. code-block:: c++

    float a, b;
    if (__builtin_mips_cabs_eq_s (a, b))
      true ();
    else
      false ();

.. function:: int __builtin_mips_upper_cabs_cond_ps (v2sf a, v2sf b)
.. function:: int __builtin_mips_lower_cabs_cond_ps (v2sf a, v2sf b)

  Absolute comparison of two paired-single values (``cabs.cond.ps``,
  ``bc1t`` / ``bc1f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``cabs.cond.ps``
  and return either the upper or lower half of the result.  For example:

  .. code-block:: c++

    v2sf a, b;
    if (__builtin_mips_upper_cabs_eq_ps (a, b))
      upper_halves_are_equal ();
    else
      upper_halves_are_unequal ();

    if (__builtin_mips_lower_cabs_eq_ps (a, b))
      lower_halves_are_equal ();
    else
      lower_halves_are_unequal ();

.. function:: v2sf __builtin_mips_movt_cabs_cond_ps (v2sf a, v2sf b, v2sf c, v2sf d)
.. function:: v2sf __builtin_mips_movf_cabs_cond_ps (v2sf a, v2sf b, v2sf c, v2sf d)

  Conditional move based on absolute comparison (``cabs.cond.ps``,
  ``movt.ps`` / ``movf.ps``).

  The ``movt`` functions return the value :samp:`{x}` computed by:

  .. code-block:: c++

    cabs.cond.ps cc,a,b
    mov.ps x,c
    movt.ps x,d,cc

  The ``movf`` functions are similar but use ``movf.ps`` instead
  of ``movt.ps``.

.. function:: int __builtin_mips_any_c_cond_ps (v2sf a, v2sf b)
.. function:: int __builtin_mips_all_c_cond_ps (v2sf a, v2sf b)
.. function:: int __builtin_mips_any_cabs_cond_ps (v2sf a, v2sf b)
.. function:: int __builtin_mips_all_cabs_cond_ps (v2sf a, v2sf b)

  Comparison of two paired-single values
  (``c.cond.ps`` / ``cabs.cond.ps``,
  ``bc1any2t`` / ``bc1any2f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``c.cond.ps``
  or ``cabs.cond.ps``.  The ``any`` forms return ``true`` if either
  result is ``true`` and the ``all`` forms return ``true`` if both results are ``true``.
  For example:

  .. code-block:: c++

    v2sf a, b;
    if (__builtin_mips_any_c_eq_ps (a, b))
      one_is_true ();
    else
      both_are_false ();

    if (__builtin_mips_all_c_eq_ps (a, b))
      both_are_true ();
    else
      one_is_false ();

.. function:: int __builtin_mips_any_c_cond_4s (v2sf a, v2sf b, v2sf c, v2sf d)
.. function:: int __builtin_mips_all_c_cond_4s (v2sf a, v2sf b, v2sf c, v2sf d)
.. function:: int __builtin_mips_any_cabs_cond_4s (v2sf a, v2sf b, v2sf c, v2sf d)
.. function:: int __builtin_mips_all_cabs_cond_4s (v2sf a, v2sf b, v2sf c, v2sf d)

  Comparison of four paired-single values
  (``c.cond.ps`` / ``cabs.cond.ps``,
  ``bc1any4t`` / ``bc1any4f``).

  These functions use ``c.cond.ps`` or ``cabs.cond.ps``
  to compare :samp:`{a}` with :samp:`{b}` and to compare :samp:`{c}` with :samp:`{d}`.
  The ``any`` forms return ``true`` if any of the four results are ``true``
  and the ``all`` forms return ``true`` if all four results are ``true``.
  For example:

  .. code-block:: c++

    v2sf a, b, c, d;
    if (__builtin_mips_any_c_eq_4s (a, b, c, d))
      some_are_true ();
    else
      all_are_false ();

    if (__builtin_mips_all_c_eq_4s (a, b, c, d))
      all_are_true ();
    else
      some_are_false ();
