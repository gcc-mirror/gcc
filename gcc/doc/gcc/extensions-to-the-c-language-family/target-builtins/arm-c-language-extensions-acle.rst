..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arm-c-language-extensions-(acle):

ARM C Language Extensions (ACLE)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC implements extensions for C as described in the ARM C Language
Extensions (ACLE) specification, which can be found at
https://developer.arm.com/documentation/ihi0053/latest/.

As a part of ACLE, GCC implements extensions for Advanced SIMD as described in
the ARM C Language Extensions Specification.  The complete list of Advanced SIMD
intrinsics can be found at
https://developer.arm.com/documentation/ihi0073/latest/.
The built-in intrinsics for the Advanced SIMD extension are available when
NEON is enabled.

Currently, ARM and AArch64 back ends do not support ACLE 2.0 fully.  Both
back ends support CRC32 intrinsics and the ARM back end supports the
Coprocessor intrinsics, all from :samp:`arm_acle.h`.  The ARM back end's 16-bit
floating-point Advanced SIMD intrinsics currently comply to ACLE v1.1.
AArch64's back end does not have support for 16-bit floating point Advanced SIMD
intrinsics yet.

See :ref:`arm-options` and :ref:`aarch64-options` for more information on the
availability of extensions.