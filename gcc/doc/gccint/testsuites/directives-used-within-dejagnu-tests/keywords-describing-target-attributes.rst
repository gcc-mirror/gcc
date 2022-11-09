..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _effective-target-keywords:

Keywords describing target attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Effective-target keywords identify sets of targets that support
particular functionality.  They are used to limit tests to be run only
for particular targets, or to specify that particular sets of targets
are expected to fail some tests.

Effective-target keywords are defined in :samp:`lib/target-supports.exp` in
the GCC testsuite, with the exception of those that are documented as
being local to a particular test directory.

The :samp:`effective target` takes into account all of the compiler options
with which the test will be compiled, including the multilib options.
By convention, keywords ending in ``_nocache`` can also include options
specified for the particular test in an earlier ``dg-options`` or
``dg-add-options`` directive.

Endianness
~~~~~~~~~~

``be``
  Target uses big-endian memory order for multi-byte and multi-word data.

``le``
  Target uses little-endian memory order for multi-byte and multi-word data.

Data type sizes
~~~~~~~~~~~~~~~

``ilp32``
  Target has 32-bit ``int``, ``long``, and pointers.

``lp64``
  Target has 32-bit ``int``, 64-bit ``long`` and pointers.

``llp64``
  Target has 32-bit ``int`` and ``long``, 64-bit ``long long``
  and pointers.

``double64``
  Target has 64-bit ``double``.

``double64plus``
  Target has ``double`` that is 64 bits or longer.

``longdouble128``
  Target has 128-bit ``long double``.

``int32plus``
  Target has ``int`` that is at 32 bits or longer.

``int16``
  Target has ``int`` that is 16 bits or shorter.

``longlong64``
  Target has 64-bit ``long long``.

``long_neq_int``
  Target has ``int`` and ``long`` with different sizes.

``short_eq_int``
  Target has ``short`` and ``int`` with the same size.

``ptr_eq_short``
  Target has pointers (``void *``) and ``short`` with the same size.

``int_eq_float``
  Target has ``int`` and ``float`` with the same size.

``ptr_eq_long``
  Target has pointers (``void *``) and ``long`` with the same size.

``large_double``
  Target supports ``double`` that is longer than ``float``.

``large_long_double``
  Target supports ``long double`` that is longer than ``double``.

``ptr32plus``
  Target has pointers that are 32 bits or longer.

``size20plus``
  Target has a 20-bit or larger address space, so supports at least
  16-bit array and structure sizes.

``size24plus``
  Target has a 24-bit or larger address space, so supports at least
  20-bit array and structure sizes.

``size32plus``
  Target has a 32-bit or larger address space, so supports at least
  24-bit array and structure sizes.

``4byte_wchar_t``
  Target has ``wchar_t`` that is at least 4 bytes.

:samp:`float{n}`
  Target has the ``_Floatn`` type.

:samp:`float{n}x`
  Target has the ``_Floatnx`` type.

:samp:`float{n}_runtime`
  Target has the ``_Floatn`` type, including runtime support
  for any options added with ``dg-add-options``.

:samp:`float{n}x_runtime`
  Target has the ``_Floatnx`` type, including runtime support
  for any options added with ``dg-add-options``.

``floatn_nx_runtime``
  Target has runtime support for any options added with
  ``dg-add-options`` for any ``_Floatn`` or
  ``_Floatnx`` type.

``inf``
  Target supports floating point infinite (``inf``) for type
  ``double``.

``inff``
  Target supports floating point infinite (``inf``) for type
  ``float``.

Fortran-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``fortran_integer_16``
  Target supports Fortran ``integer`` that is 16 bytes or longer.

``fortran_real_10``
  Target supports Fortran ``real`` that is 10 bytes or longer.

``fortran_real_16``
  Target supports Fortran ``real`` that is 16 bytes or longer.

``fortran_large_int``
  Target supports Fortran ``integer`` kinds larger than ``integer(8)``.

``fortran_large_real``
  Target supports Fortran ``real`` kinds larger than ``real(8)``.

Vector-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~

``vect_align_stack_vars``
  The target's ABI allows stack variables to be aligned to the preferred
  vector alignment.

``vect_avg_qi``
  Target supports both signed and unsigned averaging operations on vectors
  of bytes.

``vect_mulhrs_hi``
  Target supports both signed and unsigned multiply-high-with-round-and-scale
  operations on vectors of half-words.

``vect_sdiv_pow2_si``
  Target supports signed division by constant power-of-2 operations
  on vectors of 4-byte integers.

``vect_condition``
  Target supports vector conditional operations.

``vect_cond_mixed``
  Target supports vector conditional operations where comparison operands
  have different type from the value operands.

``vect_double``
  Target supports hardware vectors of ``double``.

``vect_double_cond_arith``
  Target supports conditional addition, subtraction, multiplication,
  division, minimum and maximum on vectors of ``double``, via the
  ``cond_`` optabs.

``vect_element_align_preferred``
  The target's preferred vector alignment is the same as the element
  alignment.

``vect_float``
  Target supports hardware vectors of ``float`` when
  :option:`-funsafe-math-optimizations` is in effect.

``vect_float_strict``
  Target supports hardware vectors of ``float`` when
  :option:`-funsafe-math-optimizations` is not in effect.
  This implies ``vect_float``.

``vect_int``
  Target supports hardware vectors of ``int``.

``vect_long``
  Target supports hardware vectors of ``long``.

``vect_long_long``
  Target supports hardware vectors of ``long long``.

``vect_check_ptrs``
  Target supports the ``check_raw_ptrs`` and ``check_war_ptrs``
  optabs on vectors.

``vect_fully_masked``
  Target supports fully-masked (also known as fully-predicated) loops,
  so that vector loops can handle partial as well as full vectors.

``vect_masked_load``
  Target supports vector masked loads.

``vect_masked_store``
  Target supports vector masked stores.

``vect_gather_load_ifn``
  Target supports vector gather loads using internal functions
  (rather than via built-in functions or emulation).

``vect_scatter_store``
  Target supports vector scatter stores.

``vect_aligned_arrays``
  Target aligns arrays to vector alignment boundary.

``vect_hw_misalign``
  Target supports a vector misalign access.

``vect_no_align``
  Target does not support a vector alignment mechanism.

``vect_peeling_profitable``
  Target might require to peel loops for alignment purposes.

``vect_no_int_min_max``
  Target does not support a vector min and max instruction on ``int``.

``vect_no_int_add``
  Target does not support a vector add instruction on ``int``.

``vect_no_bitwise``
  Target does not support vector bitwise instructions.

``vect_bool_cmp``
  Target supports comparison of ``bool`` vectors for at least one
  vector length.

``vect_char_add``
  Target supports addition of ``char`` vectors for at least one
  vector length.

``vect_char_mult``
  Target supports ``vector char`` multiplication.

``vect_short_mult``
  Target supports ``vector short`` multiplication.

``vect_int_mult``
  Target supports ``vector int`` multiplication.

``vect_long_mult``
  Target supports 64 bit ``vector long`` multiplication.

``vect_extract_even_odd``
  Target supports vector even/odd element extraction.

``vect_extract_even_odd_wide``
  Target supports vector even/odd element extraction of vectors with elements
  ``SImode`` or larger.

``vect_interleave``
  Target supports vector interleaving.

``vect_strided``
  Target supports vector interleaving and extract even/odd.

``vect_strided_wide``
  Target supports vector interleaving and extract even/odd for wide
  element types.

``vect_perm``
  Target supports vector permutation.

``vect_perm_byte``
  Target supports permutation of vectors with 8-bit elements.

``vect_perm_short``
  Target supports permutation of vectors with 16-bit elements.

``vect_perm3_byte``
  Target supports permutation of vectors with 8-bit elements, and for the
  default vector length it is possible to permute:

  .. code-block:: c++

    { a0, a1, a2, b0, b1, b2, ... }

  to:

  .. code-block:: c++

    { a0, a0, a0, b0, b0, b0, ... }
    { a1, a1, a1, b1, b1, b1, ... }
    { a2, a2, a2, b2, b2, b2, ... }

  using only two-vector permutes, regardless of how long the sequence is.

``vect_perm3_int``
  Like ``vect_perm3_byte``, but for 32-bit elements.

``vect_perm3_short``
  Like ``vect_perm3_byte``, but for 16-bit elements.

``vect_shift``
  Target supports a hardware vector shift operation.

``vect_unaligned_possible``
  Target prefers vectors to have an alignment greater than element
  alignment, but also allows unaligned vector accesses in some
  circumstances.

``vect_variable_length``
  Target has variable-length vectors.

``vect64``
  Target supports vectors of 64 bits.

``vect32``
  Target supports vectors of 32 bits.

``vect_widen_sum_hi_to_si``
  Target supports a vector widening summation of ``short`` operands
  into ``int`` results, or can promote (unpack) from ``short``
  to ``int``.

``vect_widen_sum_qi_to_hi``
  Target supports a vector widening summation of ``char`` operands
  into ``short`` results, or can promote (unpack) from ``char``
  to ``short``.

``vect_widen_sum_qi_to_si``
  Target supports a vector widening summation of ``char`` operands
  into ``int`` results.

``vect_widen_mult_qi_to_hi``
  Target supports a vector widening multiplication of ``char`` operands
  into ``short`` results, or can promote (unpack) from ``char`` to
  ``short`` and perform non-widening multiplication of ``short``.

``vect_widen_mult_hi_to_si``
  Target supports a vector widening multiplication of ``short`` operands
  into ``int`` results, or can promote (unpack) from ``short`` to
  ``int`` and perform non-widening multiplication of ``int``.

``vect_widen_mult_si_to_di_pattern``
  Target supports a vector widening multiplication of ``int`` operands
  into ``long`` results.

``vect_sdot_qi``
  Target supports a vector dot-product of ``signed char``.

``vect_udot_qi``
  Target supports a vector dot-product of ``unsigned char``.

``vect_usdot_qi``
  Target supports a vector dot-product where one operand of the multiply is
  ``signed char`` and the other of ``unsigned char``.

``vect_sdot_hi``
  Target supports a vector dot-product of ``signed short``.

``vect_udot_hi``
  Target supports a vector dot-product of ``unsigned short``.

``vect_pack_trunc``
  Target supports a vector demotion (packing) of ``short`` to ``char``
  and from ``int`` to ``short`` using modulo arithmetic.

``vect_unpack``
  Target supports a vector promotion (unpacking) of ``char`` to ``short``
  and from ``char`` to ``int``.

``vect_intfloat_cvt``
  Target supports conversion from ``signed int`` to ``float``.

``vect_uintfloat_cvt``
  Target supports conversion from ``unsigned int`` to ``float``.

``vect_floatint_cvt``
  Target supports conversion from ``float`` to ``signed int``.

``vect_floatuint_cvt``
  Target supports conversion from ``float`` to ``unsigned int``.

``vect_intdouble_cvt``
  Target supports conversion from ``signed int`` to ``double``.

``vect_doubleint_cvt``
  Target supports conversion from ``double`` to ``signed int``.

``vect_max_reduc``
  Target supports max reduction for vectors.

``vect_sizes_16B_8B``
  Target supports 16- and 8-bytes vectors.

``vect_sizes_32B_16B``
  Target supports 32- and 16-bytes vectors.

``vect_logical_reduc``
  Target supports AND, IOR and XOR reduction on vectors.

``vect_fold_extract_last``
  Target supports the ``fold_extract_last`` optab.

``vect_len_load_store``
  Target supports the ``len_load`` and ``len_store`` optabs.

``vect_partial_vectors_usage_1``
  Target supports loop vectorization with partial vectors and
  ``vect-partial-vector-usage`` is set to 1.

``vect_partial_vectors_usage_2``
  Target supports loop vectorization with partial vectors and
  ``vect-partial-vector-usage`` is set to 2.

``vect_partial_vectors``
  Target supports loop vectorization with partial vectors and
  ``vect-partial-vector-usage`` is nonzero.

``vect_slp_v2qi_store_align``
  Target supports vectorization of 2-byte char stores with 2-byte aligned
  address at plain :option:`-O2`.

``vect_slp_v4qi_store_align``
  Target supports vectorization of 4-byte char stores with 4-byte aligned
  address at plain :option:`-O2`.

``vect_slp_v4qi_store_unalign``
  Target supports vectorization of 4-byte char stores with unaligned address
  at plain :option:`-O2`.

``struct_4char_block_move``
  Target supports block move for 8-byte aligned 4-byte size struct initialization.

``vect_slp_v4qi_store_unalign_1``
  Target supports vectorization of 4-byte char stores with unaligned address
  or store them with constant pool at plain :option:`-O2`.

``struct_8char_block_move``
  Target supports block move for 8-byte aligned 8-byte size struct initialization.

``vect_slp_v8qi_store_unalign_1``
  Target supports vectorization of 8-byte char stores with unaligned address
  or store them with constant pool at plain :option:`-O2`.

``struct_16char_block_move``
  Target supports block move for 8-byte aligned 16-byte size struct
  initialization.

``vect_slp_v16qi_store_unalign_1``
  Target supports vectorization of 16-byte char stores with unaligned address
  or store them with constant pool at plain :option:`-O2`.

``vect_slp_v2hi_store_align``
  Target supports vectorization of 4-byte short stores with 4-byte aligned
  addressat plain :option:`-O2`.

``vect_slp_v2hi_store_unalign``
  Target supports vectorization of 4-byte short stores with unaligned address
  at plain :option:`-O2`.

``vect_slp_v4hi_store_unalign``
  Target supports vectorization of 8-byte short stores with unaligned address
  at plain :option:`-O2`.

``vect_slp_v2si_store_align``
  Target supports vectorization of 8-byte int stores with 8-byte aligned address
  at plain :option:`-O2`.

``vect_slp_v4si_store_unalign``
  Target supports vectorization of 16-byte int stores with unaligned address
  at plain :option:`-O2`.

Thread Local Storage attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``tls``
  Target supports thread-local storage.

``tls_native``
  Target supports native (rather than emulated) thread-local storage.

``tls_runtime``
  Test system supports executing TLS executables.

Decimal floating point attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``dfp``
  Targets supports compiling decimal floating point extension to C.

``dfp_nocache``
  Including the options used to compile this particular test, the
  target supports compiling decimal floating point extension to C.

``dfprt``
  Test system can execute decimal floating point tests.

``dfprt_nocache``
  Including the options used to compile this particular test, the
  test system can execute decimal floating point tests.

``hard_dfp``
  Target generates decimal floating point instructions with current options.

``dfp_bid``
  Target uses the BID format for decimal floating point.

ARM-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~

``arm32``
  ARM target generates 32-bit code.

``arm_little_endian``
  ARM target that generates little-endian code.

``arm_eabi``
  ARM target adheres to the ABI for the ARM Architecture.

.. _arm_fp_ok:

``arm_fp_ok``
  ARM target defines ``__ARM_FP`` using ``-mfloat-abi=softfp`` or
  equivalent options.  Some multilibs may be incompatible with these
  options.

.. _arm_fp_dp_ok:

``arm_fp_dp_ok``
  ARM target defines ``__ARM_FP`` with double-precision support using
  ``-mfloat-abi=softfp`` or equivalent options.  Some multilibs may
  be incompatible with these options.

``arm_hf_eabi``
  ARM target adheres to the VFP and Advanced SIMD Register Arguments
  variant of the ABI for the ARM Architecture (as selected with
  ``-mfloat-abi=hard``).

``arm_softfloat``
  ARM target uses emulated floating point operations.

``arm_hard_vfp_ok``
  ARM target supports ``-mfpu=vfp -mfloat-abi=hard``.
  Some multilibs may be incompatible with these options.

``arm_iwmmxt_ok``
  ARM target supports ``-mcpu=iwmmxt``.
  Some multilibs may be incompatible with this option.

``arm_neon``
  ARM target supports generating NEON instructions.

``arm_tune_string_ops_prefer_neon``
  Test CPU tune supports inlining string operations with NEON instructions.

``arm_neon_hw``
  Test system supports executing NEON instructions.

``arm_neonv2_hw``
  Test system supports executing NEON v2 instructions.

.. _arm_neon_ok:

``arm_neon_ok``
  ARM Target supports ``-mfpu=neon -mfloat-abi=softfp`` or compatible
  options.  Some multilibs may be incompatible with these options.

``arm_neon_ok_no_float_abi``
  ARM Target supports NEON with ``-mfpu=neon``, but without any
  -mfloat-abi= option.  Some multilibs may be incompatible with this
  option.

``arm_neonv2_ok``
  ARM Target supports ``-mfpu=neon-vfpv4 -mfloat-abi=softfp`` or compatible
  options.  Some multilibs may be incompatible with these options.

.. _arm_fp16_ok:

``arm_fp16_ok``
  Target supports options to generate VFP half-precision floating-point
  instructions.  Some multilibs may be incompatible with these
  options.  This test is valid for ARM only.

``arm_fp16_hw``
  Target supports executing VFP half-precision floating-point
  instructions.  This test is valid for ARM only.

.. _arm_neon_fp16_ok:

``arm_neon_fp16_ok``
  ARM Target supports ``-mfpu=neon-fp16 -mfloat-abi=softfp`` or compatible
  options, including ``-mfp16-format=ieee`` if necessary to obtain the
  ``__fp16`` type.  Some multilibs may be incompatible with these options.

``arm_neon_fp16_hw``
  Test system supports executing Neon half-precision float instructions.
  (Implies previous.)

``arm_fp16_alternative_ok``
  ARM target supports the ARM FP16 alternative format.  Some multilibs
  may be incompatible with the options needed.

``arm_fp16_none_ok``
  ARM target supports specifying none as the ARM FP16 format.

``arm_thumb1_ok``
  ARM target generates Thumb-1 code for ``-mthumb``.

``arm_thumb2_ok``
  ARM target generates Thumb-2 code for ``-mthumb``.

``arm_nothumb``
  ARM target that is not using Thumb.

``arm_vfp_ok``
  ARM target supports ``-mfpu=vfp -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

.. _arm_vfp3_ok:

``arm_vfp3_ok``
  ARM target supports ``-mfpu=vfp3 -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

.. _arm_arch_v8a_hard_ok:

``arm_arch_v8a_hard_ok``
  The compiler is targeting ``arm*-*-*`` and can compile and assemble code
  using the options ``-march=armv8-a -mfpu=neon-fp-armv8 -mfloat-abi=hard``.
  This is not enough to guarantee that linking works.

``arm_arch_v8a_hard_multilib``
  The compiler is targeting ``arm*-*-*`` and can build programs using
  the options ``-march=armv8-a -mfpu=neon-fp-armv8 -mfloat-abi=hard``.
  The target can also run the resulting binaries.

``arm_v8_vfp_ok``
  ARM target supports ``-mfpu=fp-armv8 -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

``arm_v8_neon_ok``
  ARM target supports ``-mfpu=neon-fp-armv8 -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

.. _arm_v8_1a_neon_ok:

``arm_v8_1a_neon_ok``
  ARM target supports options to generate ARMv8.1-A Adv.SIMD instructions.
  Some multilibs may be incompatible with these options.

``arm_v8_1a_neon_hw``
  ARM target supports executing ARMv8.1-A Adv.SIMD instructions.  Some
  multilibs may be incompatible with the options needed.  Implies
  arm_v8_1a_neon_ok.

``arm_acq_rel``
  ARM target supports acquire-release instructions.

.. _arm_v8_2a_fp16_scalar_ok:

``arm_v8_2a_fp16_scalar_ok``
  ARM target supports options to generate instructions for ARMv8.2-A and
  scalar instructions from the FP16 extension.  Some multilibs may be
  incompatible with these options.

``arm_v8_2a_fp16_scalar_hw``
  ARM target supports executing instructions for ARMv8.2-A and scalar
  instructions from the FP16 extension.  Some multilibs may be
  incompatible with these options.  Implies arm_v8_2a_fp16_neon_ok.

.. _arm_v8_2a_fp16_neon_ok:

``arm_v8_2a_fp16_neon_ok``
  ARM target supports options to generate instructions from ARMv8.2-A with
  the FP16 extension.  Some multilibs may be incompatible with these
  options.  Implies arm_v8_2a_fp16_scalar_ok.

``arm_v8_2a_fp16_neon_hw``
  ARM target supports executing instructions from ARMv8.2-A with the FP16
  extension.  Some multilibs may be incompatible with these options.
  Implies arm_v8_2a_fp16_neon_ok and arm_v8_2a_fp16_scalar_hw.

.. _arm_v8_2a_dotprod_neon_ok:

``arm_v8_2a_dotprod_neon_ok``
  ARM target supports options to generate instructions from ARMv8.2-A with
  the Dot Product extension. Some multilibs may be incompatible with these
  options.

``arm_v8_2a_dotprod_neon_hw``
  ARM target supports executing instructions from ARMv8.2-A with the Dot
  Product extension. Some multilibs may be incompatible with these options.
  Implies arm_v8_2a_dotprod_neon_ok.

``arm_v8_2a_i8mm_neon_hw``
  ARM target supports executing instructions from ARMv8.2-A with the 8-bit
  Matrix Multiply extension.  Some multilibs may be incompatible with these
  options.  Implies arm_v8_2a_i8mm_ok.

.. _arm_fp16fml_neon_ok:

``arm_fp16fml_neon_ok``
  ARM target supports extensions to generate the ``VFMAL`` and ``VFMLS``
  half-precision floating-point instructions available from ARMv8.2-A and
  onwards.  Some multilibs may be incompatible with these options.

``arm_v8_2a_bf16_neon_ok``
  ARM target supports options to generate instructions from ARMv8.2-A with
  the BFloat16 extension (bf16). Some multilibs may be incompatible with these
  options.

``arm_v8_2a_i8mm_ok``
  ARM target supports options to generate instructions from ARMv8.2-A with
  the 8-Bit Integer Matrix Multiply extension (i8mm). Some multilibs may be
  incompatible with these options.

``arm_v8_1m_mve_ok``
  ARM target supports options to generate instructions from ARMv8.1-M with
  the M-Profile Vector Extension (MVE). Some multilibs may be incompatible
  with these options.

``arm_v8_1m_mve_fp_ok``
  ARM target supports options to generate instructions from ARMv8.1-M with
  the Half-precision floating-point instructions (HP), Floating-point Extension
  (FP) along with M-Profile Vector Extension (MVE). Some multilibs may be
  incompatible with these options.

``arm_mve_hw``
  Test system supports executing MVE instructions.

``arm_v8m_main_cde``
  ARM target supports options to generate instructions from ARMv8-M with
  the Custom Datapath Extension (CDE). Some multilibs may be incompatible
  with these options.

``arm_v8m_main_cde_fp``
  ARM target supports options to generate instructions from ARMv8-M with
  the Custom Datapath Extension (CDE) and floating-point (VFP).
  Some multilibs may be incompatible with these options.

``arm_v8_1m_main_cde_mve``
  ARM target supports options to generate instructions from ARMv8.1-M with
  the Custom Datapath Extension (CDE) and M-Profile Vector Extension (MVE).
  Some multilibs may be incompatible with these options.

``arm_prefer_ldrd_strd``
  ARM target prefers ``LDRD`` and ``STRD`` instructions over
  ``LDM`` and ``STM`` instructions.

``arm_thumb1_movt_ok``
  ARM target generates Thumb-1 code for ``-mthumb`` with ``MOVW``
  and ``MOVT`` instructions available.

``arm_thumb1_cbz_ok``
  ARM target generates Thumb-1 code for ``-mthumb`` with
  ``CBZ`` and ``CBNZ`` instructions available.

``arm_divmod_simode``
  ARM target for which divmod transform is disabled, if it supports hardware
  div instruction.

``arm_cmse_ok``
  ARM target supports ARMv8-M Security Extensions, enabled by the ``-mcmse``
  option.

``arm_cmse_hw``
  Test system supports executing CMSE instructions.

.. _arm_coproc1_ok:

``arm_coproc1_ok``
  ARM target supports the following coprocessor instructions: ``CDP``,
  ``LDC``, ``STC``, ``MCR`` and ``MRC``.

.. _arm_coproc2_ok:

``arm_coproc2_ok``
  ARM target supports all the coprocessor instructions also listed as supported
  in :ref:`_arm_coproc1_ok <arm_coproc1_ok>` in addition to the following: ``CDP2``, ``LDC2``,
  ``LDC2l``, ``STC2``, ``STC2l``, ``MCR2`` and ``MRC2``.

.. _arm_coproc3_ok:

``arm_coproc3_ok``
  ARM target supports all the coprocessor instructions also listed as supported
  in :ref:`arm_coproc2_ok <arm_coproc2_ok>` in addition the following: ``MCRR`` and ``MRRC``.

``arm_coproc4_ok``
  ARM target supports all the coprocessor instructions also listed as supported
  in :ref:`arm_coproc3_ok <arm_coproc3_ok>` in addition the following: ``MCRR2`` and ``MRRC2``.

``arm_simd32_ok``
  ARM Target supports options suitable for accessing the SIMD32 intrinsics from
  ``arm_acle.h``.
  Some multilibs may be incompatible with these options.

``arm_sat_ok``
  ARM Target supports options suitable for accessing the saturation
  intrinsics from ``arm_acle.h``.
  Some multilibs may be incompatible with these options.

.. _arm_dsp_ok:

``arm_dsp_ok``
  ARM Target supports options suitable for accessing the DSP intrinsics
  from ``arm_acle.h``.
  Some multilibs may be incompatible with these options.

``arm_softfp_ok``
  ARM target supports the ``-mfloat-abi=softfp`` option.

``arm_hard_ok``
  ARM target supports the ``-mfloat-abi=hard`` option.


.. _arm_mve:

``arm_mve``
  ARM target supports generating MVE instructions.

``arm_v8_1_lob_ok``
  ARM Target supports executing the Armv8.1-M Mainline Low Overhead Loop
  instructions ``DLS`` and ``LE``.
  Some multilibs may be incompatible with these options.

``arm_thumb2_no_arm_v8_1_lob``
  ARM target where Thumb-2 is used without options but does not support
  executing the Armv8.1-M Mainline Low Overhead Loop instructions
  ``DLS`` and ``LE``.

``arm_thumb2_ok_no_arm_v8_1_lob``
  ARM target generates Thumb-2 code for ``-mthumb`` but does not
  support executing the Armv8.1-M Mainline Low Overhead Loop
  instructions ``DLS`` and ``LE``.

AArch64-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``aarch64_asm_<ext>_ok``
  AArch64 assembler supports the architecture extension ``ext`` via the
  ``.arch_extension`` pseudo-op.

``aarch64_tiny``
  AArch64 target which generates instruction sequences for tiny memory model.

``aarch64_small``
  AArch64 target which generates instruction sequences for small memory model.

``aarch64_large``
  AArch64 target which generates instruction sequences for large memory model.

``aarch64_little_endian``
  AArch64 target which generates instruction sequences for little endian.

``aarch64_big_endian``
  AArch64 target which generates instruction sequences for big endian.

``aarch64_small_fpic``
  Binutils installed on test system supports relocation types required by -fpic
  for AArch64 small memory model.

``aarch64_sve_hw``
  AArch64 target that is able to generate and execute SVE code (regardless of
  whether it does so by default).

``aarch64_sve128_hw`` ``aarch64_sve256_hw`` ``aarch64_sve512_hw`` ``aarch64_sve1024_hw`` ``aarch64_sve2048_hw``
  Like ``aarch64_sve_hw``, but also test for an exact hardware vector length.

``aarch64_fjcvtzs_hw``
  AArch64 target that is able to generate and execute armv8.3-a FJCVTZS
  instruction.

MIPS-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~

``mips64``
  MIPS target supports 64-bit instructions.

``nomips16``
  MIPS target does not produce MIPS16 code.

``mips16_attribute``
  MIPS target can generate MIPS16 code.

``mips_loongson``
  MIPS target is a Loongson-2E or -2F target using an ABI that supports
  the Loongson vector modes.

``mips_msa``
  MIPS target supports ``-mmsa``, MIPS SIMD Architecture (MSA).

``mips_newabi_large_long_double``
  MIPS target supports ``long double`` larger than ``double``
  when using the new ABI.

``mpaired_single``
  MIPS target supports ``-mpaired-single``.

MSP430-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~

``msp430_small``
  MSP430 target has the small memory model enabled (``-msmall``).

``msp430_large``
  MSP430 target has the large memory model enabled (``-mlarge``).

PowerPC-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``dfp_hw``
  PowerPC target supports executing hardware DFP instructions.

``p8vector_hw``
  PowerPC target supports executing VSX instructions (ISA 2.07).

``powerpc64``
  Test system supports executing 64-bit instructions.

``powerpc_altivec``
  PowerPC target supports AltiVec.

``powerpc_altivec_ok``
  PowerPC target supports ``-maltivec``.

``powerpc_eabi_ok``
  PowerPC target supports ``-meabi``.

``powerpc_elfv2``
  PowerPC target supports ``-mabi=elfv2``.

``powerpc_fprs``
  PowerPC target supports floating-point registers.

``powerpc_hard_double``
  PowerPC target supports hardware double-precision floating-point.

``powerpc_htm_ok``
  PowerPC target supports ``-mhtm``

``powerpc_p8vector_ok``
  PowerPC target supports ``-mpower8-vector``

``powerpc_popcntb_ok``
  PowerPC target supports the ``popcntb`` instruction, indicating
  that this target supports ``-mcpu=power5``.

``powerpc_ppu_ok``
  PowerPC target supports ``-mcpu=cell``.

``powerpc_spe``
  PowerPC target supports PowerPC SPE.

``powerpc_spe_nocache``
  Including the options used to compile this particular test, the
  PowerPC target supports PowerPC SPE.

``powerpc_spu``
  PowerPC target supports PowerPC SPU.

``powerpc_vsx_ok``
  PowerPC target supports ``-mvsx``.

``powerpc_405_nocache``
  Including the options used to compile this particular test, the
  PowerPC target supports PowerPC 405.

``ppc_recip_hw``
  PowerPC target supports executing reciprocal estimate instructions.

``vmx_hw``
  PowerPC target supports executing AltiVec instructions.

``vsx_hw``
  PowerPC target supports executing VSX instructions (ISA 2.06).

``has_arch_pwr5``
  PowerPC target pre-defines macro _ARCH_PWR5 which means the ``-mcpu``
  setting is Power5 or later.

``has_arch_pwr6``
  PowerPC target pre-defines macro _ARCH_PWR6 which means the ``-mcpu``
  setting is Power6 or later.

``has_arch_pwr7``
  PowerPC target pre-defines macro _ARCH_PWR7 which means the ``-mcpu``
  setting is Power7 or later.

``has_arch_pwr8``
  PowerPC target pre-defines macro _ARCH_PWR8 which means the ``-mcpu``
  setting is Power8 or later.

``has_arch_pwr9``
  PowerPC target pre-defines macro _ARCH_PWR9 which means the ``-mcpu``
  setting is Power9 or later.

RISC-V specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~

``rv32``
  Test system has an integer register width of 32 bits.

``rv64``
  Test system has an integer register width of 64 bits.

Other hardware attributes
~~~~~~~~~~~~~~~~~~~~~~~~~

.. Please keep this table sorted alphabetically.

``autoincdec``
  Target supports autoincrement/decrement addressing.

``avx``
  Target supports compiling ``avx`` instructions.

``avx_runtime``
  Target supports the execution of ``avx`` instructions.

``avx2``
  Target supports compiling ``avx2`` instructions.

``avx2_runtime``
  Target supports the execution of ``avx2`` instructions.

``avxvnni``
  Target supports the execution of ``avxvnni`` instructions.

``avx512f``
  Target supports compiling ``avx512f`` instructions.

``avx512f_runtime``
  Target supports the execution of ``avx512f`` instructions.

``avx512vp2intersect``
  Target supports the execution of ``avx512vp2intersect`` instructions.

``avxifma``
  Target supports the execution of ``avxifma`` instructions.

``avxneconvert``
  Target supports the execution of ``avxneconvert`` instructions.

``avxvnniint8``
  Target supports the execution of ``avxvnniint8`` instructions.

``amx_tile``
  Target supports the execution of ``amx-tile`` instructions.

``amx_int8``
  Target supports the execution of ``amx-int8`` instructions.

``amx_bf16``
  Target supports the execution of ``amx-bf16`` instructions.

``amx_fp16``
  Target supports the execution of ``amx-fp16`` instructions.

``cell_hw``
  Test system can execute AltiVec and Cell PPU instructions.

``cmpccxadd``
  Target supports the execution of ``cmpccxadd`` instructions.

``coldfire_fpu``
  Target uses a ColdFire FPU.

``divmod``
  Target supporting hardware divmod insn or divmod libcall.

``divmod_simode``
  Target supporting hardware divmod insn or divmod libcall for SImode.

``hard_float``
  Target supports FPU instructions.

``non_strict_align``
  Target does not require strict alignment.

``pie_copyreloc``
  The x86-64 target linker supports PIE with copy reloc.

``prefetchi``
  Target supports the execution of ``prefetchi`` instructions.

``raoint``
  Target supports the execution of ``raoint`` instructions.

``rdrand``
  Target supports x86 ``rdrand`` instruction.

``sqrt_insn``
  Target has a square root instruction that the compiler can generate.

``sse``
  Target supports compiling ``sse`` instructions.

``sse_runtime``
  Target supports the execution of ``sse`` instructions.

``sse2``
  Target supports compiling ``sse2`` instructions.

``sse2_runtime``
  Target supports the execution of ``sse2`` instructions.

``sync_char_short``
  Target supports atomic operations on ``char`` and ``short``.

``sync_int_long``
  Target supports atomic operations on ``int`` and ``long``.

``ultrasparc_hw``
  Test environment appears to run executables on a simulator that
  accepts only ``EM_SPARC`` executables and chokes on ``EM_SPARC32PLUS``
  or ``EM_SPARCV9`` executables.

``vect_cmdline_needed``
  Target requires a command line argument to enable a SIMD instruction set.

``xorsign``
  Target supports the xorsign optab expansion.

Environment attributes
~~~~~~~~~~~~~~~~~~~~~~

``c``
  The language for the compiler under test is C.

``c++``
  The language for the compiler under test is C++.

``c99_runtime``
  Target provides a full C99 runtime.

``correct_iso_cpp_string_wchar_protos``
  Target ``string.h`` and ``wchar.h`` headers provide C++ required
  overloads for ``strchr`` etc. functions.

``d_runtime``
  Target provides the D runtime.

``d_runtime_has_std_library``
  Target provides the D standard library (Phobos).

``dummy_wcsftime``
  Target uses a dummy ``wcsftime`` function that always returns zero.

``fd_truncate``
  Target can truncate a file from a file descriptor, as used by
  :samp:`libgfortran/io/unix.c:fd_truncate`; i.e. ``ftruncate`` or
  ``chsize``.

``fenv``
  Target provides :samp:`fenv.h` include file.

``fenv_exceptions``
  Target supports :samp:`fenv.h` with all the standard IEEE exceptions
  and floating-point exceptions are raised by arithmetic operations.

``fenv_exceptions_dfp``
  Target supports :samp:`fenv.h` with all the standard IEEE exceptions
  and floating-point exceptions are raised by arithmetic operations for
  decimal floating point.

``fileio``
  Target offers such file I/O library functions as ``fopen``,
  ``fclose``, ``tmpnam``, and ``remove``.  This is a link-time
  requirement for the presence of the functions in the library; even if
  they fail at runtime, the requirement is still regarded as satisfied.

``freestanding``
  Target is :samp:`freestanding` as defined in section 4 of the C99 standard.
  Effectively, it is a target which supports no extra headers or libraries
  other than what is considered essential.

``gettimeofday``
  Target supports ``gettimeofday``.

``init_priority``
  Target supports constructors with initialization priority arguments.

``inttypes_types``
  Target has the basic signed and unsigned types in ``inttypes.h``.
  This is for tests that GCC's notions of these types agree with those
  in the header, as some systems have only ``inttypes.h``.

``lax_strtofp``
  Target might have errors of a few ULP in string to floating-point
  conversion functions and overflow is not always detected correctly by
  those functions.

``mempcpy``
  Target provides ``mempcpy`` function.

``mmap``
  Target supports ``mmap``.

``newlib``
  Target supports Newlib.

``newlib_nano_io``
  GCC was configured with ``--enable-newlib-nano-formatted-io``, which reduces
  the code size of Newlib formatted I/O functions.

``pow10``
  Target provides ``pow10`` function.

``pthread``
  Target can compile using ``pthread.h`` with no errors or warnings.

``pthread_h``
  Target has ``pthread.h``.

``run_expensive_tests``
  Expensive testcases (usually those that consume excessive amounts of CPU
  time) should be run on this target.  This can be enabled by setting the
  :envvar:`GCC_TEST_RUN_EXPENSIVE` environment variable to a non-empty string.

``simulator``
  Test system runs executables on a simulator (i.e. slowly) rather than
  hardware (i.e. fast).

``signal``
  Target has ``signal.h``.

``stabs``
  Target supports the stabs debugging format.

``stdint_types``
  Target has the basic signed and unsigned C types in ``stdint.h``.
  This will be obsolete when GCC ensures a working ``stdint.h`` for
  all targets.

``stdint_types_mbig_endian``
  Target accepts the option :option:`-mbig-endian` and ``stdint.h``
  can be included without error when :option:`-mbig-endian` is passed.

``stpcpy``
  Target provides ``stpcpy`` function.

``sysconf``
  Target supports ``sysconf``.

``trampolines``
  Target supports trampolines.

``two_plus_gigs``
  Target supports linking programs with 2+GiB of data.

``uclibc``
  Target supports uClibc.

``unwrapped``
  Target does not use a status wrapper.

``vxworks_kernel``
  Target is a VxWorks kernel.

``vxworks_rtp``
  Target is a VxWorks RTP.

``wchar``
  Target supports wide characters.

Other attributes
~~~~~~~~~~~~~~~~

``R_flag_in_section``
  Target supports the 'R' flag in .section directive in assembly inputs.

``automatic_stack_alignment``
  Target supports automatic stack alignment.

``branch_cost``
  Target supports :option:`-branch-cost=N`.

``cxa_atexit``
  Target uses ``__cxa_atexit``.

.. _default_packed:

``default_packed``
  Target has packed layout of structure members by default.

``exceptions``
  Target supports exceptions.

``exceptions_enabled``
  Target supports exceptions and they are enabled in the current
  testing configuration.

``fgraphite``
  Target supports Graphite optimizations.

``fixed_point``
  Target supports fixed-point extension to C.

``fopenacc``
  Target supports OpenACC via :option:`-fopenacc`.

``fopenmp``
  Target supports OpenMP via :option:`-fopenmp`.

``fpic``
  Target supports :option:`-fpic` and :option:`-fPIC`.

``freorder``
  Target supports :option:`-freorder-blocks-and-partition`.

``fstack_protector``
  Target supports :option:`-fstack-protector`.

``gas``
  Target uses GNU :command:`as`.

``gc_sections``
  Target supports :option:`--gc-sections`.

``gld``
  Target uses GNU :command:`ld`.

``keeps_null_pointer_checks``
  Target keeps null pointer checks, either due to the use of
  :option:`-fno-delete-null-pointer-checks` or hardwired into the target.

``llvm_binutils``
  Target is using an LLVM assembler and/or linker, instead of GNU Binutils.

``lra``
  Target supports local register allocator (LRA).

``lto``
  Compiler has been configured to support link-time optimization (LTO).

``lto_incremental``
  Compiler and linker support link-time optimization relocatable linking
  with :option:`-r` and :option:`-flto` options.

``naked_functions``
  Target supports the ``naked`` function attribute.

``named_sections``
  Target supports named sections.

``natural_alignment_32``
  Target uses natural alignment (aligned to type size) for types of
  32 bits or less.

``target_natural_alignment_64``
  Target uses natural alignment (aligned to type size) for types of
  64 bits or less.

``no_alignment_constraints``
  Target defines __BIGGEST_ALIGNMENT__=1.  Hence target imposes
  no alignment constraints.  This is similar, but not necessarily
  the same as :ref:`default_packed`.  Although ``BIGGEST_FIELD_ALIGNMENT``
  defaults to ``BIGGEST_ALIGNMENT`` for most targets, it is possible
  for a target to set those two with different values and have different
  alignment constraints for aggregate and non-aggregate types.

``noinit``
  Target supports the ``noinit`` variable attribute.

``nonpic``
  Target does not generate PIC by default.

``o_flag_in_section``
  Target supports the 'o' flag in .section directive in assembly inputs.

``offload_gcn``
  Target has been configured for OpenACC/OpenMP offloading on AMD GCN.

``persistent``
  Target supports the ``persistent`` variable attribute.

``pie_enabled``
  Target generates PIE by default.

``pcc_bitfield_type_matters``
  Target defines ``PCC_BITFIELD_TYPE_MATTERS``.

``pe_aligned_commons``
  Target supports :option:`-mpe-aligned-commons`.

``pie``
  Target supports :option:`-pie`, :option:`-fpie` and :option:`-fPIE`.

``rdynamic``
  Target supports :option:`-rdynamic`.

``scalar_all_fma``
  Target supports all four fused multiply-add optabs for both ``float``
  and ``double``.  These optabs are: ``fma_optab``, ``fms_optab``,
  ``fnma_optab`` and ``fnms_optab``.

``section_anchors``
  Target supports section anchors.

``short_enums``
  Target defaults to short enums.

.. _stack_size_et:

``stack_size``
  Target has limited stack size.  The stack size limit can be obtained using the
  STACK_SIZE macro defined by :ref:`stack_size_ao`.

``static``
  Target supports :option:`-static`.

``static_libgfortran``
  Target supports statically linking :samp:`libgfortran`.

``string_merging``
  Target supports merging string constants at link time.

``ucn``
  Target supports compiling and assembling UCN.

``ucn_nocache``
  Including the options used to compile this particular test, the
  target supports compiling and assembling UCN.

``unaligned_stack``
  Target does not guarantee that its ``STACK_BOUNDARY`` is greater than
  or equal to the required vector alignment.

``vector_alignment_reachable``
  Vector alignment is reachable for types of 32 bits or less.

``vector_alignment_reachable_for_64bit``
  Vector alignment is reachable for types of 64 bits or less.

``vma_equals_lma``
  Target generates executable with VMA equal to LMA for .data section.

``wchar_t_char16_t_compatible``
  Target supports ``wchar_t`` that is compatible with ``char16_t``.

``wchar_t_char32_t_compatible``
  Target supports ``wchar_t`` that is compatible with ``char32_t``.

``comdat_group``
  Target uses comdat groups.

``indirect_calls``
  Target supports indirect calls, i.e. calls where the target is not
  constant.

``lgccjit``
  Target supports -lgccjit, i.e. libgccjit.so can be linked into jit tests.

``__OPTIMIZE__``
  Optimizations are enabled (``__OPTIMIZE__``) per the current
  compiler flags.

Local to tests in gcc.target/i386
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``3dnow``
  Target supports compiling ``3dnow`` instructions.

``aes``
  Target supports compiling ``aes`` instructions.

``fma4``
  Target supports compiling ``fma4`` instructions.

``mfentry``
  Target supports the ``-mfentry`` option that alters the
  position of profiling calls such that they precede the prologue.

``ms_hook_prologue``
  Target supports attribute ``ms_hook_prologue``.

``pclmul``
  Target supports compiling ``pclmul`` instructions.

``sse3``
  Target supports compiling ``sse3`` instructions.

``sse4``
  Target supports compiling ``sse4`` instructions.

``sse4a``
  Target supports compiling ``sse4a`` instructions.

``ssse3``
  Target supports compiling ``ssse3`` instructions.

``vaes``
  Target supports compiling ``vaes`` instructions.

``vpclmul``
  Target supports compiling ``vpclmul`` instructions.

``xop``
  Target supports compiling ``xop`` instructions.

Local to tests in gcc.test-framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``no``
  Always returns 0.

``yes``
  Always returns 1.
