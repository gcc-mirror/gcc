..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: half-precision floating point, __fp16 data type, __Float16 data type

.. _half-precision:

Half-Precision Floating Point
*****************************

On ARM and AArch64 targets, GCC supports half-precision (16-bit) floating
point via the ``__fp16`` type defined in the ARM C Language Extensions.
On ARM systems, you must enable this type explicitly with the
:option:`-mfp16-format` command-line option in order to use it.
On x86 targets with SSE2 enabled, GCC supports half-precision (16-bit)
floating point via the ``_Float16`` type. For C++, x86 provides a builtin
type named ``_Float16`` which contains same data format as C.

ARM targets support two incompatible representations for half-precision
floating-point values.  You must choose one of the representations and
use it consistently in your program.

Specifying :option:`-mfp16-format=ieee` selects the IEEE 754-2008 format.
This format can represent normalized values in the range of 2^{-14} to 65504.
There are 11 bits of significand precision, approximately 3
decimal digits.

Specifying :option:`-mfp16-format=alternative` selects the ARM
alternative format.  This representation is similar to the IEEE
format, but does not support infinities or NaNs.  Instead, the range
of exponents is extended, so that this format can represent normalized
values in the range of 2^{-14} to 131008.

The GCC port for AArch64 only supports the IEEE 754-2008 format, and does
not require use of the :option:`-mfp16-format` command-line option.

The ``__fp16`` type may only be used as an argument to intrinsics defined
in ``<arm_fp16.h>``, or as a storage format.  For purposes of
arithmetic and other operations, ``__fp16`` values in C or C++
expressions are automatically promoted to ``float``.

The ARM target provides hardware support for conversions between
``__fp16`` and ``float`` values
as an extension to VFP and NEON (Advanced SIMD), and from ARMv8-A provides
hardware support for conversions between ``__fp16`` and ``double``
values.  GCC generates code using these hardware instructions if you
compile with options to select an FPU that provides them;
for example, :option:`-mfpu=neon-fp16 -mfloat-abi=softfp`,
in addition to the :option:`-mfp16-format` option to select
a half-precision format.

Language-level support for the ``__fp16`` data type is
independent of whether GCC generates code using hardware floating-point
instructions.  In cases where hardware support is not specified, GCC
implements conversions between ``__fp16`` and other types as library
calls.

It is recommended that portable code use the ``_Float16`` type defined
by ISO/IEC TS 18661-3:2015.  See :ref:`floating-types`.

On x86 targets with SSE2 enabled, without :option:`-mavx512fp16`,
all operations will be emulated by software emulation and the ``float``
instructions. The default behavior for ``FLT_EVAL_METHOD`` is to keep the
intermediate result of the operation as 32-bit precision. This may lead to
inconsistent behavior between software emulation and AVX512-FP16 instructions.
Using :option:`-fexcess-precision=16` will force round back after each operation.

Using :option:`-mavx512fp16` will generate AVX512-FP16 instructions instead of
software emulation. The default behavior of ``FLT_EVAL_METHOD`` is to round
after each operation. The same is true with :option:`-fexcess-precision=standard`
and :option:`-mfpmath=sse`. If there is no :option:`-mfpmath=sse`,
:option:`-fexcess-precision=standard` alone does the same thing as before,
It is useful for code that does not have ``_Float16`` and runs on the x87
FPU.
