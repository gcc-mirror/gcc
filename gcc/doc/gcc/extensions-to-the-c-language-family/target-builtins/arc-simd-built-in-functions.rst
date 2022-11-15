..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arc-simd-built-in-functions:

ARC SIMD Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

SIMD builtins provided by the compiler can be used to generate the
vector instructions.  This section describes the available builtins
and their usage in programs.  With the :option:`-msimd` option, the
compiler provides 128-bit vector types, which can be specified using
the ``vector_size`` attribute.  The header file :samp:`arc-simd.h`
can be included to use the following predefined types:

.. code-block:: c++

  typedef int __v4si   __attribute__((vector_size(16)));
  typedef short __v8hi __attribute__((vector_size(16)));

These types can be used to define 128-bit variables.  The built-in
functions listed in the following section can be used on these
variables to generate the vector operations.

For all builtins, ``__builtin_arc_someinsn``, the header file
:samp:`arc-simd.h` also provides equivalent macros called
``_someinsn`` that can be used for programming ease and
improved readability.  The following macros for DMA control are also
provided:

.. code-block:: c++

  #define _setup_dma_in_channel_reg _vdiwr
  #define _setup_dma_out_channel_reg _vdowr

The following is a complete list of all the SIMD built-ins provided
for ARC, grouped by calling signature.

The following take two ``__v8hi`` arguments and return a
``__v8hi`` result:

.. code-block:: c++

  __v8hi __builtin_arc_vaddaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vaddw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vand (__v8hi, __v8hi);
  __v8hi __builtin_arc_vandaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vavb (__v8hi, __v8hi);
  __v8hi __builtin_arc_vavrb (__v8hi, __v8hi);
  __v8hi __builtin_arc_vbic (__v8hi, __v8hi);
  __v8hi __builtin_arc_vbicaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vdifaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vdifw (__v8hi, __v8hi);
  __v8hi __builtin_arc_veqw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vh264f (__v8hi, __v8hi);
  __v8hi __builtin_arc_vh264ft (__v8hi, __v8hi);
  __v8hi __builtin_arc_vh264fw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vlew (__v8hi, __v8hi);
  __v8hi __builtin_arc_vltw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmaxaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmaxw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vminaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vminw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr1aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr1w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr2aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr2w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr3aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr3w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr4aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr4w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr5aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr5w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr6aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr6w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr7aw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmr7w (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmrb (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmulaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmulfaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmulfw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vmulw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vnew (__v8hi, __v8hi);
  __v8hi __builtin_arc_vor (__v8hi, __v8hi);
  __v8hi __builtin_arc_vsubaw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vsubw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vsummw (__v8hi, __v8hi);
  __v8hi __builtin_arc_vvc1f (__v8hi, __v8hi);
  __v8hi __builtin_arc_vvc1ft (__v8hi, __v8hi);
  __v8hi __builtin_arc_vxor (__v8hi, __v8hi);
  __v8hi __builtin_arc_vxoraw (__v8hi, __v8hi);

The following take one ``__v8hi`` and one ``int`` argument and return a
``__v8hi`` result:

.. code-block:: c++

  __v8hi __builtin_arc_vbaddw (__v8hi, int);
  __v8hi __builtin_arc_vbmaxw (__v8hi, int);
  __v8hi __builtin_arc_vbminw (__v8hi, int);
  __v8hi __builtin_arc_vbmulaw (__v8hi, int);
  __v8hi __builtin_arc_vbmulfw (__v8hi, int);
  __v8hi __builtin_arc_vbmulw (__v8hi, int);
  __v8hi __builtin_arc_vbrsubw (__v8hi, int);
  __v8hi __builtin_arc_vbsubw (__v8hi, int);

The following take one ``__v8hi`` argument and one ``int`` argument which
must be a 3-bit compile time constant indicating a register number
I0-I7.  They return a ``__v8hi`` result.

.. code-block:: c++

  __v8hi __builtin_arc_vasrw (__v8hi, const int);
  __v8hi __builtin_arc_vsr8 (__v8hi, const int);
  __v8hi __builtin_arc_vsr8aw (__v8hi, const int);

The following take one ``__v8hi`` argument and one ``int``
argument which must be a 6-bit compile time constant.  They return a
``__v8hi`` result.

.. code-block:: c++

  __v8hi __builtin_arc_vasrpwbi (__v8hi, const int);
  __v8hi __builtin_arc_vasrrpwbi (__v8hi, const int);
  __v8hi __builtin_arc_vasrrwi (__v8hi, const int);
  __v8hi __builtin_arc_vasrsrwi (__v8hi, const int);
  __v8hi __builtin_arc_vasrwi (__v8hi, const int);
  __v8hi __builtin_arc_vsr8awi (__v8hi, const int);
  __v8hi __builtin_arc_vsr8i (__v8hi, const int);

The following take one ``__v8hi`` argument and one ``int`` argument which
must be a 8-bit compile time constant.  They return a ``__v8hi``
result.

.. code-block:: c++

  __v8hi __builtin_arc_vd6tapf (__v8hi, const int);
  __v8hi __builtin_arc_vmvaw (__v8hi, const int);
  __v8hi __builtin_arc_vmvw (__v8hi, const int);
  __v8hi __builtin_arc_vmvzw (__v8hi, const int);

The following take two ``int`` arguments, the second of which which
must be a 8-bit compile time constant.  They return a ``__v8hi``
result:

.. code-block:: c++

  __v8hi __builtin_arc_vmovaw (int, const int);
  __v8hi __builtin_arc_vmovw (int, const int);
  __v8hi __builtin_arc_vmovzw (int, const int);

The following take a single ``__v8hi`` argument and return a
``__v8hi`` result:

.. code-block:: c++

  __v8hi __builtin_arc_vabsaw (__v8hi);
  __v8hi __builtin_arc_vabsw (__v8hi);
  __v8hi __builtin_arc_vaddsuw (__v8hi);
  __v8hi __builtin_arc_vexch1 (__v8hi);
  __v8hi __builtin_arc_vexch2 (__v8hi);
  __v8hi __builtin_arc_vexch4 (__v8hi);
  __v8hi __builtin_arc_vsignw (__v8hi);
  __v8hi __builtin_arc_vupbaw (__v8hi);
  __v8hi __builtin_arc_vupbw (__v8hi);
  __v8hi __builtin_arc_vupsbaw (__v8hi);
  __v8hi __builtin_arc_vupsbw (__v8hi);

The following take two ``int`` arguments and return no result:

.. code-block:: c++

  void __builtin_arc_vdirun (int, int);
  void __builtin_arc_vdorun (int, int);

The following take two ``int`` arguments and return no result.  The
first argument must a 3-bit compile time constant indicating one of
the DR0-DR7 DMA setup channels:

.. code-block:: c++

  void __builtin_arc_vdiwr (const int, int);
  void __builtin_arc_vdowr (const int, int);

The following take an ``int`` argument and return no result:

.. code-block:: c++

  void __builtin_arc_vendrec (int);
  void __builtin_arc_vrec (int);
  void __builtin_arc_vrecrun (int);
  void __builtin_arc_vrun (int);

The following take a ``__v8hi`` argument and two ``int``
arguments and return a ``__v8hi`` result.  The second argument must
be a 3-bit compile time constants, indicating one the registers I0-I7,
and the third argument must be an 8-bit compile time constant.

.. note::

  Although the equivalent hardware instructions do not take
  an SIMD register as an operand, these builtins overwrite the relevant
  bits of the ``__v8hi`` register provided as the first argument with
  the value loaded from the ``[Ib, u8]`` location in the SDM.

.. code-block:: c++

  __v8hi __builtin_arc_vld32 (__v8hi, const int, const int);
  __v8hi __builtin_arc_vld32wh (__v8hi, const int, const int);
  __v8hi __builtin_arc_vld32wl (__v8hi, const int, const int);
  __v8hi __builtin_arc_vld64 (__v8hi, const int, const int);

The following take two ``int`` arguments and return a ``__v8hi``
result.  The first argument must be a 3-bit compile time constants,
indicating one the registers I0-I7, and the second argument must be an
8-bit compile time constant.

.. code-block:: c++

  __v8hi __builtin_arc_vld128 (const int, const int);
  __v8hi __builtin_arc_vld64w (const int, const int);

The following take a ``__v8hi`` argument and two ``int``
arguments and return no result.  The second argument must be a 3-bit
compile time constants, indicating one the registers I0-I7, and the
third argument must be an 8-bit compile time constant.

.. code-block:: c++

  void __builtin_arc_vst128 (__v8hi, const int, const int);
  void __builtin_arc_vst64 (__v8hi, const int, const int);

The following take a ``__v8hi`` argument and three ``int``
arguments and return no result.  The second argument must be a 3-bit
compile-time constant, identifying the 16-bit sub-register to be
stored, the third argument must be a 3-bit compile time constants,
indicating one the registers I0-I7, and the fourth argument must be an
8-bit compile time constant.

.. code-block:: c++

  void __builtin_arc_vst16_n (__v8hi, const int, const int, const int);
  void __builtin_arc_vst32_n (__v8hi, const int, const int, const int);
