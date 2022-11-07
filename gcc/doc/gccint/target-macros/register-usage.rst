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

.. function:: const predefined_function_abi & TARGET_FNTYPE_ABI (const_tree type)

  .. hook-start:TARGET_FNTYPE_ABI

  Return the ABI used by a function with type :samp:`{type}` ; see the
  definition of ``predefined_function_abi`` for details of the ABI
  descriptor.  Targets only need to define this hook if they support
  interoperability between several ABIs in the same translation unit.

.. hook-end

.. function:: const predefined_function_abi & TARGET_INSN_CALLEE_ABI (const rtx_insn *insn)

  .. hook-start:TARGET_INSN_CALLEE_ABI

  This hook returns a description of the ABI used by the target of
  call instruction :samp:`{insn}` ; see the definition of
  ``predefined_function_abi`` for details of the ABI descriptor.
  Only the global function ``insn_callee_abi`` should call this hook
  directly.

  Targets only need to define this hook if they support
  interoperability between several ABIs in the same translation unit.

.. hook-end

.. index:: call-used register, call-clobbered register, call-saved register

.. function:: bool TARGET_HARD_REGNO_CALL_PART_CLOBBERED (unsigned int abi_id, unsigned int regno, machine_mode mode)

  .. hook-start:TARGET_HARD_REGNO_CALL_PART_CLOBBERED

  ABIs usually specify that calls must preserve the full contents
  of a particular register, or that calls can alter any part of a
  particular register.  This information is captured by the target macro
  ``CALL_REALLY_USED_REGISTERS``.  However, some ABIs specify that calls
  must preserve certain bits of a particular register but can alter others.
  This hook should return true if this applies to at least one of the
  registers in :samp:`(reg:{mode}{regno})`, and if as a result the
  call would alter part of the :samp:`{mode}` value.  For example, if a call
  preserves the low 32 bits of a 64-bit hard register :samp:`{regno}` but can
  clobber the upper 32 bits, this hook should return true for a 64-bit mode
  but false for a 32-bit mode.

  The value of :samp:`{abi_id}` comes from the ``predefined_function_abi``
  structure that describes the ABI of the call; see the definition of the
  structure for more details.  If (as is usual) the target uses the same ABI
  for all functions in a translation unit, :samp:`{abi_id}` is always 0.

  The default implementation returns false, which is correct
  for targets that don't have partly call-clobbered registers.

.. hook-end

.. function:: const char * TARGET_GET_MULTILIB_ABI_NAME (void)

  .. hook-start:TARGET_GET_MULTILIB_ABI_NAME

  This hook returns name of multilib ABI name.

.. hook-end

.. index:: fixed_regs, call_used_regs, global_regs, reg_names, reg_class_contents

.. function:: void TARGET_CONDITIONAL_REGISTER_USAGE (void)

  .. hook-start:TARGET_CONDITIONAL_REGISTER_USAGE

  This hook may conditionally modify five variables
  ``fixed_regs``, ``call_used_regs``, ``global_regs``,
  ``reg_names``, and ``reg_class_contents``, to take into account
  any dependence of these register sets on target flags.  The first three
  of these are of type ``char []`` (interpreted as boolean vectors).
  ``global_regs`` is a ``const char *[]``, and
  ``reg_class_contents`` is a ``HARD_REG_SET``.  Before the macro is
  called, ``fixed_regs``, ``call_used_regs``,
  ``reg_class_contents``, and ``reg_names`` have been initialized
  from ``FIXED_REGISTERS``, ``CALL_USED_REGISTERS``,
  ``REG_CLASS_CONTENTS``, and ``REGISTER_NAMES``, respectively.
  ``global_regs`` has been cleared, and any :option:`-ffixed-reg`,
  :option:`-fcall-used-reg` and :option:`-fcall-saved-reg`
  command options have been applied.

  .. index:: disabling certain registers, controlling register usage

  If the usage of an entire class of registers depends on the target
  flags, you may indicate this to GCC by using this macro to modify
  ``fixed_regs`` and ``call_used_regs`` to 1 for each of the
  registers in the classes which should not be used by GCC.  Also make
  ``define_register_constraint`` s return ``NO_REGS`` for constraints
  that shouldn't be used.

  (However, if this class is not included in ``GENERAL_REGS`` and all
  of the insn patterns whose constraints permit this class are
  controlled by target switches, then GCC will automatically avoid using
  these registers when the target switches are opposed to them.)

.. hook-end

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

.. function:: unsigned int TARGET_HARD_REGNO_NREGS (unsigned int regno, machine_mode mode)

  .. hook-start:TARGET_HARD_REGNO_NREGS

  This hook returns the number of consecutive hard registers, starting
  at register number :samp:`{regno}`, required to hold a value of mode
  :samp:`{mode}`.  This hook must never return zero, even if a register
  cannot hold the requested mode - indicate that with
  ``TARGET_HARD_REGNO_MODE_OK`` and/or
  ``TARGET_CAN_CHANGE_MODE_CLASS`` instead.

  The default definition returns the number of words in :samp:`{mode}`.

.. hook-end

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

.. function:: bool TARGET_HARD_REGNO_MODE_OK (unsigned int regno, machine_mode mode)

  .. hook-start:TARGET_HARD_REGNO_MODE_OK

  This hook returns true if it is permissible to store a value
  of mode :samp:`{mode}` in hard register number :samp:`{regno}` (or in several
  registers starting with that one).  The default definition returns true
  unconditionally.

  You need not include code to check for the numbers of fixed registers,
  because the allocation mechanism considers them to be always occupied.

  .. index:: register pairs

  On some machines, double-precision values must be kept in even/odd
  register pairs.  You can implement that by defining this hook to reject
  odd register numbers for such modes.

  The minimum requirement for a mode to be OK in a register is that the
  :samp:`mov{mode}` instruction pattern support moves between the
  register and other hard register in the same class and that moving a
  value into the register and back out not alter it.

  Since the same instruction used to move ``word_mode`` will work for
  all narrower integer modes, it is not necessary on any machine for
  this hook to distinguish between these modes, provided you define
  patterns :samp:`movhi`, etc., to take advantage of this.  This is
  useful because of the interaction between ``TARGET_HARD_REGNO_MODE_OK``
  and ``TARGET_MODES_TIEABLE_P`` ; it is very desirable for all integer
  modes to be tieable.

  Many machines have special registers for floating point arithmetic.
  Often people assume that floating point machine modes are allowed only
  in floating point registers.  This is not true.  Any registers that
  can hold integers can safely *hold* a floating point machine
  mode, whether or not floating arithmetic can be done on it in those
  registers.  Integer move instructions can be used to move the values.

  On some machines, though, the converse is true: fixed-point machine
  modes may not go in floating registers.  This is true if the floating
  registers normalize any value stored in them, because storing a
  non-floating value there would garble it.  In this case,
  ``TARGET_HARD_REGNO_MODE_OK`` should reject fixed-point machine modes in
  floating registers.  But if the floating registers do not automatically
  normalize, if you can store any bit pattern in one and retrieve it
  unchanged without a trap, then any machine mode may go in a floating
  register, so you can define this hook to say so.

  The primary significance of special floating registers is rather that
  they are the registers acceptable in floating point arithmetic
  instructions.  However, this is of no concern to
  ``TARGET_HARD_REGNO_MODE_OK``.  You handle it by writing the proper
  constraints for those instructions.

  On some machines, the floating registers are especially slow to access,
  so that it is better to store a value in a stack frame than in such a
  register if floating point arithmetic is not being done.  As long as the
  floating registers are not in class ``GENERAL_REGS``, they will not
  be used unless some pattern's constraint asks for one.

.. hook-end

.. c:macro:: HARD_REGNO_RENAME_OK (from, to)

  A C expression that is nonzero if it is OK to rename a hard register
  :samp:`{from}` to another hard register :samp:`{to}`.

  One common use of this macro is to prevent renaming of a register to
  another register that is not saved by a prologue in an interrupt
  handler.

  The default is always nonzero.

.. function:: bool TARGET_MODES_TIEABLE_P (machine_mode mode1, machine_mode mode2)

  .. hook-start:TARGET_MODES_TIEABLE_P

  This hook returns true if a value of mode :samp:`{mode1}` is accessible
  in mode :samp:`{mode2}` without copying.

  If ``TARGET_HARD_REGNO_MODE_OK (r, mode1)`` and
  ``TARGET_HARD_REGNO_MODE_OK (r, mode2)`` are always
  the same for any :samp:`{r}`, then
  ``TARGET_MODES_TIEABLE_P (mode1, mode2)``
  should be true.  If they differ for any :samp:`{r}`, you should define
  this hook to return false unless some other mechanism ensures the
  accessibility of the value in a narrower mode.

  You should define this hook to return true in as many cases as
  possible since doing so will allow GCC to perform better register
  allocation.  The default definition returns true unconditionally.

.. hook-end

.. function:: bool TARGET_HARD_REGNO_SCRATCH_OK (unsigned int regno)

  .. hook-start:TARGET_HARD_REGNO_SCRATCH_OK

  This target hook should return ``true`` if it is OK to use a hard register
  :samp:`{regno}` as scratch reg in peephole2.

  One common use of this macro is to prevent using of a register that
  is not saved by a prologue in an interrupt handler.

  The default version of this hook always returns ``true``.

.. hook-end

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