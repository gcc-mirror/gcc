..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _sh-built-in-functions:

SH Built-in Functions
^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are supported on the SH1, SH2, SH3 and SH4
families of processors:

.. function:: void __builtin_set_thread_pointer (void *ptr)

  Sets the :samp:`GBR` register to the specified value :samp:`{ptr}`.  This is usually
  used by system code that manages threads and execution contexts.  The compiler
  normally does not generate code that modifies the contents of :samp:`GBR` and
  thus the value is preserved across function calls.  Changing the :samp:`GBR`
  value in user code must be done with caution, since the compiler might use
  :samp:`GBR` in order to access thread local variables.

.. function:: void * __builtin_thread_pointer (void)

  Returns the value that is currently set in the :samp:`GBR` register.
  Memory loads and stores that use the thread pointer as a base address are
  turned into :samp:`GBR` based displacement loads and stores, if possible.
  For example:

  .. code-block:: c++

    struct my_tcb
    {
       int a, b, c, d, e;
    };

    int get_tcb_value (void)
    {
      // Generate mov.l @(8,gbr),r0 instruction
      return ((my_tcb*)__builtin_thread_pointer ())->c;
    }

.. function:: unsigned int __builtin_sh_get_fpscr (void)

  Returns the value that is currently set in the :samp:`FPSCR` register.

.. function:: void __builtin_sh_set_fpscr (unsigned int val)

  Sets the :samp:`FPSCR` register to the specified value :samp:`{val}`, while
  preserving the current values of the FR, SZ and PR bits.
