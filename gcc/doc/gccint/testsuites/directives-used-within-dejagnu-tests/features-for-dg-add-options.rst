..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _add-options:

Features for dg-add-options
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The supported values of :samp:`{feature}` for directive ``dg-add-options``
are:

``arm_fp``
  ``__ARM_FP`` definition.  Only ARM targets support this feature, and only then
  in certain modes; see the :ref:`arm_fp_ok <arm_fp_ok>`.

``arm_fp_dp``
  ``__ARM_FP`` definition with double-precision support.  Only ARM
  targets support this feature, and only then in certain modes; see the
  :ref:`arm_fp_dp_ok <arm_fp_dp_ok>`.

``arm_neon``
  NEON support.  Only ARM targets support this feature, and only then
  in certain modes; see the :ref:`arm_neon_ok <arm_neon_ok>`.

``arm_fp16``
  VFP half-precision floating point support.  This does not select the
  FP16 format; for that, use :ref:`arm_fp16_ieee <arm_fp16_ieee>` or
  :ref:`arm_fp16_alternative <arm_fp16_alternative>` instead.  This
  feature is only supported by ARM targets and then only in certain
  modes; see the :ref:`arm_fp16_ok <arm_fp16_ok>`.

.. _arm_fp16_ieee:

``arm_fp16_ieee``
  ARM IEEE 754-2008 format VFP half-precision floating point support.
  This feature is only supported by ARM targets and then only in certain
  modes; see the :ref:`arm_fp16_ok <arm_fp16_ok>`.

.. _arm_fp16_alternative:

``arm_fp16_alternative``
  ARM Alternative format VFP half-precision floating point support.
  This feature is only supported by ARM targets and then only in certain
  modes; see the :ref:`arm_fp16_ok <arm_fp16_ok>`.

``arm_neon_fp16``
  NEON and half-precision floating point support.  Only ARM targets
  support this feature, and only then in certain modes; see
  the :ref:`arm_neon_fp16_ok <arm_neon_fp16_ok>`.

``arm_vfp3``
  arm vfp3 floating point support; see
  the :ref:`arm_vfp3_ok <arm_vfp3_ok>`.

``arm_arch_v8a_hard``
  Add options for ARMv8-A and the hard-float variant of the AAPCS,
  if this is supported by the compiler; see the
  :ref:`arm_arch_v8a_hard_ok <arm_arch_v8a_hard_ok>` effective target keyword.

``arm_v8_1a_neon``
  Add options for ARMv8.1-A with Adv.SIMD support, if this is supported
  by the target; see the :ref:`arm_v8_1a_neon_ok <arm_v8_1a_neon_ok>`
  effective target keyword.

``arm_v8_2a_fp16_scalar``
  Add options for ARMv8.2-A with scalar FP16 support, if this is
  supported by the target; see the
  :ref:`arm_v8_2a_fp16_scalar_ok <arm_v8_2a_fp16_scalar_ok>` effective
  target keyword.

``arm_v8_2a_fp16_neon``
  Add options for ARMv8.2-A with Adv.SIMD FP16 support, if this is
  supported by the target; see the
  :ref:`arm_v8_2a_fp16_neon_ok <arm_v8_2a_fp16_neon_ok>` effective target
  keyword.

``arm_v8_2a_dotprod_neon``
  Add options for ARMv8.2-A with Adv.SIMD Dot Product support, if this is
  supported by the target; see the
  :ref:`arm_v8_2a_dotprod_neon_ok <arm_v8_2a_dotprod_neon_ok>` effective target keyword.

``arm_fp16fml_neon``
  Add options to enable generation of the ``VFMAL`` and ``VFMSL``
  instructions, if this is supported by the target; see the
  :ref:`arm_fp16fml_neon_ok <arm_fp16fml_neon_ok>` effective target keyword.

``arm_dsp``
  Add options for ARM DSP intrinsics support, if this is supported by
  the target; see the :ref:`arm_dsp_ok <arm_dsp_ok>`.

``bind_pic_locally``
  Add the target-specific flags needed to enable functions to bind
  locally when using pic/PIC passes in the testsuite.

:samp:`float{n}`
  Add the target-specific flags needed to use the ``_Floatn`` type.

:samp:`float{n}x`
  Add the target-specific flags needed to use the ``_Floatnx`` type.

``ieee``
  Add the target-specific flags needed to enable full IEEE
  compliance mode.

``mips16_attribute``
  ``mips16`` function attributes.
  Only MIPS targets support this feature, and only then in certain modes.

.. _stack_size_ao:

``stack_size``
  Add the flags needed to define macro STACK_SIZE and set it to the stack size
  limit associated with the :ref:`stack_size_et <stack_size_et>`.

``sqrt_insn``
  Add the target-specific flags needed to enable hardware square root
  instructions, if any.

``tls``
  Add the target-specific flags needed to use thread-local storage.
