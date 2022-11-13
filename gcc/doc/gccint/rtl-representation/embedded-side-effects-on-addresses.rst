..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL preincrement, RTL postincrement, RTL predecrement, RTL postdecrement

.. _incdec:

Embedded Side-Effects on Addresses
**********************************

Six special side-effect expression codes appear as memory addresses.

.. index:: pre_dec

:samp:`(pre_dec:{m} {x})`
  Represents the side effect of decrementing :samp:`{x}` by a standard
  amount and represents also the value that :samp:`{x}` has after being
  decremented.  :samp:`{x}` must be a ``reg`` or ``mem``, but most
  machines allow only a ``reg``.  :samp:`{m}` must be the machine mode
  for pointers on the machine in use.  The amount :samp:`{x}` is decremented
  by is the length in bytes of the machine mode of the containing memory
  reference of which this expression serves as the address.  Here is an
  example of its use:

  .. code-block:: c++

    (mem:DF (pre_dec:SI (reg:SI 39)))

  This says to decrement pseudo register 39 by the length of a ``DFmode``
  value and use the result to address a ``DFmode`` value.

  .. index:: pre_inc

:samp:`(pre_inc:{m} {x})`
  Similar, but specifies incrementing :samp:`{x}` instead of decrementing it.

  .. index:: post_dec

:samp:`(post_dec:{m} {x})`
  Represents the same side effect as ``pre_dec`` but a different
  value.  The value represented here is the value :samp:`{x}` has before
  being decremented.

  .. index:: post_inc

:samp:`(post_inc:{m} {x})`
  Similar, but specifies incrementing :samp:`{x}` instead of decrementing it.

  .. index:: post_modify

:samp:`(post_modify:{m} {x} {y})`
  Represents the side effect of setting :samp:`{x}` to :samp:`{y}` and
  represents :samp:`{x}` before :samp:`{x}` is modified.  :samp:`{x}` must be a
  ``reg`` or ``mem``, but most machines allow only a ``reg``.
  :samp:`{m}` must be the machine mode for pointers on the machine in use.

  The expression :samp:`{y}` must be one of three forms:
  ``(plus:mxz)``,
  ``(minus:mxz)``, or
  ``(plus:mxi)``,
  where :samp:`{z}` is an index register and :samp:`{i}` is a constant.

  Here is an example of its use:

  .. code-block:: c++

    (mem:SF (post_modify:SI (reg:SI 42) (plus (reg:SI 42)
                                              (reg:SI 48))))

  This says to modify pseudo register 42 by adding the contents of pseudo
  register 48 to it, after the use of what ever 42 points to.

  .. index:: pre_modify

:samp:`(pre_modify:{m} {x} {expr})`
  Similar except side effects happen before the use.

These embedded side effect expressions must be used with care.  Instruction
patterns may not use them.  Until the :samp:`flow` pass of the compiler,
they may occur only to represent pushes onto the stack.  The :samp:`flow`
pass finds cases where registers are incremented or decremented in one
instruction and used as an address shortly before or after; these cases are
then transformed to use pre- or post-increment or -decrement.

If a register used as the operand of these expressions is used in
another address in an insn, the original value of the register is used.
Uses of the register outside of an address are not permitted within the
same insn as a use in an embedded side effect expression because such
insns behave differently on different machines and hence must be treated
as ambiguous and disallowed.

An instruction that can be represented with an embedded side effect
could also be represented using ``parallel`` containing an additional
``set`` to describe how the address register is altered.  This is not
done because machines that allow these operations at all typically
allow them wherever a memory address is called for.  Describing them as
additional parallel stores would require doubling the number of entries
in the machine description.