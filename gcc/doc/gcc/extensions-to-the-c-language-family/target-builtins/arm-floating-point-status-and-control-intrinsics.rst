..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arm-floating-point-status-and-control-intrinsics:

ARM Floating Point Status and Control Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the ARM family of
processors with floating-point unit.

.. code-block:: c++

  unsigned int __builtin_arm_get_fpscr ();
  void __builtin_arm_set_fpscr (unsigned int);