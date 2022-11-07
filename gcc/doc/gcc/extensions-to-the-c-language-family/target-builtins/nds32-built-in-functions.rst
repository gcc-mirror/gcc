..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _nds32-built-in-functions:

NDS32 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the NDS32 target:

.. function:: void __builtin_nds32_isync (int *addr)

  Insert an ISYNC instruction into the instruction stream where
  :samp:`{addr}` is an instruction address for serialization.

.. function:: void __builtin_nds32_isb (void)

  Insert an ISB instruction into the instruction stream.

.. function:: int __builtin_nds32_mfsr (int sr)

  Return the content of a system register which is mapped by :samp:`{sr}`.

.. function:: int __builtin_nds32_mfusr (int usr)

  Return the content of a user space register which is mapped by :samp:`{usr}`.

.. function:: void __builtin_nds32_mtsr (int value, int sr)

  Move the :samp:`{value}` to a system register which is mapped by :samp:`{sr}`.

.. function:: void __builtin_nds32_mtusr (int value, int usr)

  Move the :samp:`{value}` to a user space register which is mapped by :samp:`{usr}`.

.. function:: void __builtin_nds32_setgie_en (void)

  Enable global interrupt.

.. function:: void __builtin_nds32_setgie_dis (void)

  Disable global interrupt.