..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _other-mips-built-in-functions:

Other MIPS Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides other MIPS-specific built-in functions:

.. function:: void __builtin_mips_cache (int op, const volatile void *addr)

  Insert a :samp:`cache` instruction with operands :samp:`{op}` and :samp:`{addr}`.
  GCC defines the preprocessor macro ``___GCC_HAVE_BUILTIN_MIPS_CACHE``
  when this function is available.

.. function:: unsigned int __builtin_mips_get_fcsr (void)
.. function:: void __builtin_mips_set_fcsr (unsigned int value)

  Get and set the contents of the floating-point control and status register
  (FPU control register 31).  These functions are only available in hard-float
  code but can be called in both MIPS16 and non-MIPS16 contexts.

  ``__builtin_mips_set_fcsr`` can be used to change any bit of the
  register except the condition codes, which GCC assumes are preserved.
