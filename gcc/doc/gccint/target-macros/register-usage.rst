..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: register usage

.. _registers:

Register Usage
**************

This section explains how to describe what registers the target machine
has, and how (in general) they can be used.

The description of which registers a specific instruction can use is
done with register classes; see :ref:`register-classes`.  For information
on using registers to access a stack frame, see :ref:`frame-registers`.
For passing values in registers, see :ref:`register-arguments`.
For returning values in registers, see :ref:`scalar-return`.

.. toctree::
  :maxdepth: 2


.. _register-basics:

Basic Characteristics of Registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Registers have various characteristics.

.. c:macro:: FIRST_PSEUDO_REGISTER

  Number of hardware registers known to the compiler.  They receive
  numbers 0 through ``FIRST_PSEUDO_REGISTER-1`` ; thus, the first
  pseudo register's number really is assigned the number
  ``FIRST_PSEUDO_REGISTER``.

.. c:macro:: FIXED_REGISTERS

  .. index:: fixed register

  An initializer that says which registers are used for fixed purposes
  all throughout the compiled code and are therefore not available for
  general allocation.  These would include the stack pointer, the frame
  pointer (except on machines where that can be used as a general
  register when no frame pointer is needed), the program counter on
  machines where that is considered one of the addressable registers,
  and any other numbered register with a standard use.

  This information is expressed as a sequence of numbers, separated by
  commas and surrounded by braces.  The :samp:`{n}` th number is 1 if
  register :samp:`{n}` is fixed, 0 otherwise.

  The table initialized from this macro, and the table initialized by
  the following one, may be overridden at run time either automatically,
  by the actions of the macro ``CONDITIONAL_REGISTER_USAGE``, or by
  the user with the command options :option:`-ffixed-reg`,
  :option:`-fcall-used-reg` and :option:`-fcall-saved-reg`.

.. c:macro:: CALL_USED_REGISTERS

  .. index:: call-used register, call-clobbered register, call-saved register

  Like ``FIXED_REGISTERS`` but has 1 for each register that is
  clobbered (in general) by function calls as well as for fixed
  registers.  This macro therefore identifies the registers that are not
  available for general allocation of values that must live across
  function calls.

  If a register has 0 in ``CALL_USED_REGISTERS``, the compiler
  automatically saves it on function entry and restores it on function
  exit, if the register is used within the function.

  Exactly one of ``CALL_USED_REGISTERS`` and ``CALL_REALLY_USED_REGISTERS``
  must be defined.  Modern ports should define ``CALL_REALLY_USED_REGISTERS``.

.. c:macro:: CALL_REALLY_USED_REGISTERS

  .. index:: call-used register, call-clobbered register, call-saved register

  Like ``CALL_USED_REGISTERS`` except this macro doesn't require
  that the entire set of ``FIXED_REGISTERS`` be included.
  (``CALL_USED_REGISTERS`` must be a superset of ``FIXED_REGISTERS``).

  Exactly one of ``CALL_USED_REGISTERS`` and ``CALL_REALLY_USED_REGISTERS``
  must be defined.  Modern ports should define ``CALL_REALLY_USED_REGISTERS``.

.. index:: call-used register, call-clobbered register, call-saved register

.. include:: tm.rst.in
  :start-after: [TARGET_FNTYPE_ABI]
  :end-before: [TARGET_FNTYPE_ABI]


.. include:: tm.rst.in
  :start-after: [TARGET_INSN_CALLEE_ABI]
  :end-before: [TARGET_INSN_CALLEE_ABI]


.. index:: call-used register, call-clobbered register, call-saved register

.. include:: tm.rst.in
  :start-after: [TARGET_HARD_REGNO_CALL_PART_CLOBBERED]
  :end-before: [TARGET_HARD_REGNO_CALL_PART_CLOBBERED]


.. include:: tm.rst.in
  :start-after: [TARGET_GET_MULTILIB_ABI_NAME]
  :end-before: [TARGET_GET_MULTILIB_ABI_NAME]


.. index:: fixed_regs, call_used_regs, global_regs, reg_names, reg_class_contents

.. include:: tm.rst.in
  :start-after: [TARGET_CONDITIONAL_REGISTER_USAGE]
  :end-before: [TARGET_CONDITIONAL_REGISTER_USAGE]


.. c:macro:: INCOMING_REGNO (out)

  Define this macro if the target machine has register windows.  This C
  expression returns the register number as seen by the called function
  corresponding to the register number :samp:`{out}` as seen by the calling
  function.  Return :samp:`{out}` if register number :samp:`{out}` is not an
  outbound register.

.. c:macro:: OUTGOING_REGNO (in)

  Define this macro if the target machine has register windows.  This C
  expression returns the register number as seen by the calling function
  corresponding to the register number :samp:`{in}` as seen by the called
  function.  Return :samp:`{in}` if register number :samp:`{in}` is not an inbound
  register.

.. c:macro:: LOCAL_REGNO (regno)

  Define this macro if the target machine has register windows.  This C
  expression returns true if the register is call-saved but is in the
  register window.  Unlike most call-saved registers, such registers
  need not be explicitly restored on function exit or during non-local
  gotos.

.. c:macro:: PC_REGNUM

  If the program counter has a register number, define this as that
  register number.  Otherwise, do not define it.

.. index:: order of register allocation, register allocation order

.. _allocation-order:

Order of Allocation of Registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Registers are allocated in order.

.. c:macro:: REG_ALLOC_ORDER

  If defined, an initializer for a vector of integers, containing the
  numbers of hard registers in the order in which GCC should prefer
  to use them (from most preferred to least).

  If this macro is not defined, registers are used lowest numbered first
  (all else being equal).

  One use of this macro is on machines where the highest numbered
  registers must always be saved and the save-multiple-registers
  instruction supports only sequences of consecutive registers.  On such
  machines, define ``REG_ALLOC_ORDER`` to be an initializer that lists
  the highest numbered allocable register first.

.. c:macro:: ADJUST_REG_ALLOC_ORDER

  A C statement (sans semicolon) to choose the order in which to allocate
  hard registers for pseudo-registers local to a basic block.

  Store the desired register order in the array ``reg_alloc_order``.
  Element 0 should be the register to allocate first; element 1, the next
  register; and so on.

  The macro body should not assume anything about the contents of
  ``reg_alloc_order`` before execution of the macro.

  On most machines, it is not necessary to define this macro.

.. c:macro:: HONOR_REG_ALLOC_ORDER

  Normally, IRA tries to estimate the costs for saving a register in the
  prologue and restoring it in the epilogue.  This discourages it from
  using call-saved registers.  If a machine wants to ensure that IRA
  allocates registers in the order given by REG_ALLOC_ORDER even if some
  call-saved registers appear earlier than call-used ones, then define this
  macro as a C expression to nonzero. Default is 0.

.. c:macro:: IRA_HARD_REGNO_ADD_COST_MULTIPLIER (regno)

  In some case register allocation order is not enough for the
  Integrated Register Allocator (IRA) to generate a good code.
  If this macro is defined, it should return a floating point value
  based on :samp:`{regno}`.  The cost of using :samp:`{regno}` for a pseudo will
  be increased by approximately the pseudo's usage frequency times the
  value returned by this macro.  Not defining this macro is equivalent
  to having it always return ``0.0``.

  On most machines, it is not necessary to define this macro.

.. _values-in-registers:

How Values Fit in Registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section discusses the macros that describe which kinds of values
(specifically, which machine modes) each register can hold, and how many
consecutive registers are needed for a given mode.

.. include:: tm.rst.in
  :start-after: [TARGET_HARD_REGNO_NREGS]
  :end-before: [TARGET_HARD_REGNO_NREGS]


.. c:macro:: HARD_REGNO_NREGS_HAS_PADDING (regno, mode)

  A C expression that is nonzero if a value of mode :samp:`{mode}`, stored
  in memory, ends with padding that causes it to take up more space than
  in registers starting at register number :samp:`{regno}` (as determined by
  multiplying GCC's notion of the size of the register when containing
  this mode by the number of registers returned by
  ``TARGET_HARD_REGNO_NREGS``).  By default this is zero.

  For example, if a floating-point value is stored in three 32-bit
  registers but takes up 128 bits in memory, then this would be
  nonzero.

  This macros only needs to be defined if there are cases where
  ``subreg_get_info``
  would otherwise wrongly determine that a ``subreg`` can be
  represented by an offset to the register number, when in fact such a
  ``subreg`` would contain some of the padding not stored in
  registers and so not be representable.

.. c:macro:: HARD_REGNO_NREGS_WITH_PADDING (regno, mode)

  For values of :samp:`{regno}` and :samp:`{mode}` for which
  ``HARD_REGNO_NREGS_HAS_PADDING`` returns nonzero, a C expression
  returning the greater number of registers required to hold the value
  including any padding.  In the example above, the value would be four.

.. c:macro:: REGMODE_NATURAL_SIZE (mode)

  Define this macro if the natural size of registers that hold values
  of mode :samp:`{mode}` is not the word size.  It is a C expression that
  should give the natural size in bytes for the specified mode.  It is
  used by the register allocator to try to optimize its results.  This
  happens for example on SPARC 64-bit where the natural size of
  floating-point registers is still 32-bit.

.. include:: tm.rst.in
  :start-after: [TARGET_HARD_REGNO_MODE_OK]
  :end-before: [TARGET_HARD_REGNO_MODE_OK]


.. c:macro:: HARD_REGNO_RENAME_OK (from, to)

  A C expression that is nonzero if it is OK to rename a hard register
  :samp:`{from}` to another hard register :samp:`{to}`.

  One common use of this macro is to prevent renaming of a register to
  another register that is not saved by a prologue in an interrupt
  handler.

  The default is always nonzero.

.. include:: tm.rst.in
  :start-after: [TARGET_MODES_TIEABLE_P]
  :end-before: [TARGET_MODES_TIEABLE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_HARD_REGNO_SCRATCH_OK]
  :end-before: [TARGET_HARD_REGNO_SCRATCH_OK]


.. c:macro:: AVOID_CCMODE_COPIES

  Define this macro if the compiler should avoid copies to/from ``CCmode``
  registers.  You should only define this macro if support for copying to/from
  ``CCmode`` is incomplete.

.. index:: leaf functions, functions, leaf

.. _leaf-functions:

Handling Leaf Functions
^^^^^^^^^^^^^^^^^^^^^^^

On some machines, a leaf function (i.e., one which makes no calls) can run
more efficiently if it does not make its own register window.  Often this
means it is required to receive its arguments in the registers where they
are passed by the caller, instead of the registers where they would
normally arrive.

The special treatment for leaf functions generally applies only when
other conditions are met; for example, often they may use only those
registers for its own variables and temporaries.  We use the term 'leaf
function' to mean a function that is suitable for this special
handling, so that functions with no calls are not necessarily 'leaf
functions'.

GCC assigns register numbers before it knows whether the function is
suitable for leaf function treatment.  So it needs to renumber the
registers in order to output a leaf function.  The following macros
accomplish this.

.. c:macro:: LEAF_REGISTERS

  Name of a char vector, indexed by hard register number, which
  contains 1 for a register that is allowable in a candidate for leaf
  function treatment.

  If leaf function treatment involves renumbering the registers, then the
  registers marked here should be the ones before renumbering---those that
  GCC would ordinarily allocate.  The registers which will actually be
  used in the assembler code, after renumbering, should not be marked with 1
  in this vector.

  Define this macro only if the target machine offers a way to optimize
  the treatment of leaf functions.

.. c:macro:: LEAF_REG_REMAP (regno)

  A C expression whose value is the register number to which :samp:`{regno}`
  should be renumbered, when a function is treated as a leaf function.

  If :samp:`{regno}` is a register number which should not appear in a leaf
  function before renumbering, then the expression should yield -1, which
  will cause the compiler to abort.

  Define this macro only if the target machine offers a way to optimize the
  treatment of leaf functions, and registers need to be renumbered to do
  this.

.. index:: current_function_is_leaf, current_function_uses_only_leaf_regs

``TARGET_ASM_FUNCTION_PROLOGUE`` and
``TARGET_ASM_FUNCTION_EPILOGUE`` must usually treat leaf functions
specially.  They can test the C variable ``current_function_is_leaf``
which is nonzero for leaf functions.  ``current_function_is_leaf`` is
set prior to local register allocation and is valid for the remaining
compiler passes.  They can also test the C variable
``current_function_uses_only_leaf_regs`` which is nonzero for leaf
functions which only use leaf registers.
``current_function_uses_only_leaf_regs`` is valid after all passes
that modify the instructions have been run and is only useful if
``LEAF_REGISTERS`` is defined.

.. changed this to fix overfull.  ALSO:  why the "it" at the beginning

.. of the next paragraph?!  -mew 2feb93

.. _stack-registers:

Registers That Form a Stack
^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are special features to handle computers where some of the
'registers' form a stack.  Stack registers are normally written by
pushing onto the stack, and are numbered relative to the top of the
stack.

Currently, GCC can only handle one group of stack-like registers, and
they must be consecutively numbered.  Furthermore, the existing
support for stack-like registers is specific to the 80387 floating
point coprocessor.  If you have a new architecture that uses
stack-like registers, you will need to do substantial work on
:samp:`reg-stack.cc` and write your machine description to cooperate
with it, as well as defining these macros.

.. c:macro:: STACK_REGS

  Define this if the machine has any stack-like registers.

.. c:macro:: STACK_REG_COVER_CLASS

  This is a cover class containing the stack registers.  Define this if
  the machine has any stack-like registers.

.. c:macro:: FIRST_STACK_REG

  The number of the first stack-like register.  This one is the top
  of the stack.

.. c:macro:: LAST_STACK_REG

  The number of the last stack-like register.  This one is the bottom of
  the stack.
