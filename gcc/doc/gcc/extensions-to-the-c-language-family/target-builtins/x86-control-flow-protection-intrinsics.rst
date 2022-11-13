..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _x86-control-flow-protection-intrinsics:

x86 Control-Flow Protection Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: ret_type _get_ssp (void)

  Get the current value of shadow stack pointer if shadow stack support
  from Intel CET is enabled in the hardware or ``0`` otherwise.
  The ``ret_type`` is ``unsigned long long`` for 64-bit targets
  and ``unsigned int`` for 32-bit targets.

.. function:: void _inc_ssp (unsigned int)

  Increment the current shadow stack pointer by the size specified by the
  function argument.  The argument is masked to a byte value for security
  reasons, so to increment by more than 255 bytes you must call the function
  multiple times.

The shadow stack unwind code looks like:

.. code-block:: c++

  #include <immintrin.h>

  /* Unwind the shadow stack for EH.  */
  #define _Unwind_Frames_Extra(x)       \
    do                                  \
      {                                \
        _Unwind_Word ssp = _get_ssp (); \
        if (ssp != 0)                   \
          {                            \
            _Unwind_Word tmp = (x);     \
            while (tmp > 255)           \
              {                        \
                _inc_ssp (tmp);         \
                tmp -= 255;             \
              }                        \
            _inc_ssp (tmp);             \
          }                            \
      }                                \
      while (0)

This code runs unconditionally on all 64-bit processors.  For 32-bit
processors the code runs on those that support multi-byte NOP instructions.