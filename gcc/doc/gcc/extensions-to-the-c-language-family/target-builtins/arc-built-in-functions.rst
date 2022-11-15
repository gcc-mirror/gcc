..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arc-built-in-functions:

ARC Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are provided for ARC targets.  The
built-ins generate the corresponding assembly instructions.  In the
examples given below, the generated code often requires an operand or
result to be in a register.  Where necessary further code will be
generated to ensure this is true, but for brevity this is not
described in each case.

.. note::
  Using a built-in to generate an instruction not supported
  by a target may cause problems. At present the compiler is not
  guaranteed to detect such misuse, and as a result an internal compiler
  error may be generated.

.. function:: int __builtin_arc_aligned (void *val, int alignval)

  Return 1 if :samp:`{val}` is known to have the byte alignment given
  by :samp:`{alignval}`, otherwise return 0.
  Note that this is different from

  .. code-block:: c++

    __alignof__(*(char *)val) >= alignval

  because __alignof__ sees only the type of the dereference, whereas
  __builtin_arc_align uses alignment information from the pointer
  as well as from the pointed-to type.
  The information available will depend on optimization level.

.. function:: void __builtin_arc_brk (void)

  Generates

  .. code-block:: c++

    brk

.. function:: unsigned int __builtin_arc_core_read (unsigned int regno)

  The operand is the number of a register to be read.  Generates:

  .. code-block:: c++

    mov  dest, rregno

  where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_core_write (unsigned int regno, unsigned int val)

  The first operand is the number of a register to be written, the
  second operand is a compile time constant to write into that
  register.  Generates:

  .. code-block:: c++

    mov  rregno, val

.. function:: int __builtin_arc_divaw (int a, int b)

  Only available if either :option:`-mcpu=ARC700` or :option:`-meA` is set.
  Generates:

  .. code-block:: c++

    divaw  dest, a, b

  where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_flag (unsigned int a)

  Generates

  .. code-block:: c++

    flag  a

.. function:: unsigned int __builtin_arc_lr (unsigned int auxr)

  The operand, :samp:`{auxv}`, is the address of an auxiliary register and
  must be a compile time constant.  Generates:

  .. code-block:: c++

    lr  dest, [auxr]

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_mul64 (int a, int b)

  Only available with :option:`-mmul64`.  Generates:

  .. code-block:: c++

    mul64  a, b

.. function:: void __builtin_arc_mulu64 (unsigned int a, unsigned int b)

  Only available with :option:`-mmul64`.  Generates:

  .. code-block:: c++

    mulu64  a, b

.. function:: void __builtin_arc_nop (void)

  Generates:

  .. code-block:: c++

    nop

.. function:: int __builtin_arc_norm (int src)

  Only valid if the :samp:`norm` instruction is available through the
  :option:`-mnorm` option or by default with :option:`-mcpu=ARC700`.
  Generates:

  .. code-block:: c++

    norm  dest, src

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: short int __builtin_arc_normw (short int src)

  Only valid if the :samp:`normw` instruction is available through the
  :option:`-mnorm` option or by default with :option:`-mcpu=ARC700`.
  Generates:

  .. code-block:: c++

    normw  dest, src

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_rtie (void)

  Generates:

  .. code-block:: c++

    rtie

.. function:: void __builtin_arc_sleep (int a)

  Generates:

  .. code-block:: c++

    sleep  a

.. function:: void __builtin_arc_sr (unsigned int val, unsigned int auxr)

  The first argument, :samp:`{val}`, is a compile time constant to be
  written to the register, the second argument, :samp:`{auxr}`, is the
  address of an auxiliary register.  Generates:

  .. code-block:: c++

    sr  val, [auxr]

.. function:: int __builtin_arc_swap (int src)

  Only valid with :option:`-mswap`.  Generates:

  .. code-block:: c++

    swap  dest, src

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_swi (void)

  Generates:

  .. code-block:: c++

    swi

.. function:: void __builtin_arc_sync (void)

  Only available with :option:`-mcpu=ARC700`.  Generates:

  .. code-block:: c++

    sync

.. function:: void __builtin_arc_trap_s (unsigned int c)

  Only available with :option:`-mcpu=ARC700`.  Generates:

  .. code-block:: c++

    trap_s  c

.. function:: void __builtin_arc_unimp_s (void)

  Only available with :option:`-mcpu=ARC700`.  Generates:

  .. code-block:: c++

    unimp_s

The instructions generated by the following builtins are not
considered as candidates for scheduling.  They are not moved around by
the compiler during scheduling, and thus can be expected to appear
where they are put in the C code:

.. code-block:: c++

  __builtin_arc_brk()
  __builtin_arc_core_read()
  __builtin_arc_core_write()
  __builtin_arc_flag()
  __builtin_arc_lr()
  __builtin_arc_sleep()
  __builtin_arc_sr()
  __builtin_arc_swi()
