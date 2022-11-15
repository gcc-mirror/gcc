..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _aarch64-built-in-functions:

AArch64 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the AArch64 family of
processors.

.. code-block:: c++

  unsigned int __builtin_aarch64_get_fpcr ();
  void __builtin_aarch64_set_fpcr (unsigned int);
  unsigned int __builtin_aarch64_get_fpsr ();
  void __builtin_aarch64_set_fpsr (unsigned int);

  unsigned long long __builtin_aarch64_get_fpcr64 ();
  void __builtin_aarch64_set_fpcr64 (unsigned long long);
  unsigned long long __builtin_aarch64_get_fpsr64 ();
  void __builtin_aarch64_set_fpsr64 (unsigned long long);
