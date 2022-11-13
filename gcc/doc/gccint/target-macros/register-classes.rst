..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: register class definitions, class definitions, register

.. _register-classes:

Register Classes
****************

On many machines, the numbered registers are not all equivalent.
For example, certain registers may not be allowed for indexed addressing;
certain registers may not be allowed in some instructions.  These machine
restrictions are described to the compiler using :dfn:`register classes`.

You define a number of register classes, giving each one a name and saying
which of the registers belong to it.  Then you can specify register classes
that are allowed as operands to particular instruction patterns.

.. index:: ALL_REGS, NO_REGS

In general, each register will belong to several classes.  In fact, one
class must be named ``ALL_REGS`` and contain all the registers.  Another
class must be named ``NO_REGS`` and contain no registers.  Often the
union of two classes will be another class; however, this is not required.

.. index:: GENERAL_REGS

One of the classes must be named ``GENERAL_REGS``.  There is nothing
terribly special about the name, but the operand constraint letters
:samp:`r` and :samp:`g` specify this class.  If ``GENERAL_REGS`` is
the same as ``ALL_REGS``, just define it as a macro which expands
to ``ALL_REGS``.

Order the classes so that if class :samp:`{x}` is contained in class :samp:`{y}`
then :samp:`{x}` has a lower class number than :samp:`{y}`.

The way classes other than ``GENERAL_REGS`` are specified in operand
constraints is through machine-dependent operand constraint letters.
You can define such letters to correspond to various classes, then use
them in operand constraints.

You must define the narrowest register classes for allocatable
registers, so that each class either has no subclasses, or that for
some mode, the move cost between registers within the class is
cheaper than moving a register in the class to or from memory
(see :ref:`costs`).

You should define a class for the union of two classes whenever some
instruction allows both classes.  For example, if an instruction allows
either a floating point (coprocessor) register or a general register for a
certain operand, you should define a class ``FLOAT_OR_GENERAL_REGS``
which includes both of them.  Otherwise you will get suboptimal code,
or even internal compiler errors when reload cannot find a register in the
class computed via ``reg_class_subunion``.

You must also specify certain redundant information about the register
classes: for each class, which classes contain it and which ones are
contained in it; for each pair of classes, the largest class contained
in their union.

When a value occupying several consecutive registers is expected in a
certain class, all the registers used must belong to that class.
Therefore, register classes cannot be used to enforce a requirement for
a register pair to start with an even-numbered register.  The way to
specify this requirement is with ``TARGET_HARD_REGNO_MODE_OK``.

Register classes used for input-operands of bitwise-and or shift
instructions have a special requirement: each such class must have, for
each fixed-point machine mode, a subclass whose registers can transfer that
mode to or from memory.  For example, on some machines, the operations for
single-byte values (``QImode``) are limited to certain registers.  When
this is so, each register class that is used in a bitwise-and or shift
instruction must have a subclass consisting of registers from which
single-byte values can be loaded or stored.  This is so that
``PREFERRED_RELOAD_CLASS`` can always have a possible value to return.

.. index:: enum reg_class

Data type enum reg_classAn enumerated type that must be defined with all the register class names
as enumerated values.  ``NO_REGS`` must be first.  ``ALL_REGS``
must be the last register class, followed by one more enumerated value,
``LIM_REG_CLASSES``, which is not a register class but rather
tells how many classes there are.

Each register class has a number, which is the value of casting
the class name to type ``int``.  The number serves as an index
in many of the tables described below.

.. c:macro:: N_REG_CLASSES

  The number of distinct register classes, defined as follows:

  .. code-block:: c++

    #define N_REG_CLASSES (int) LIM_REG_CLASSES

.. c:macro:: REG_CLASS_NAMES

  An initializer containing the names of the register classes as C string
  constants.  These names are used in writing some of the debugging dumps.

.. c:macro:: REG_CLASS_CONTENTS

  An initializer containing the contents of the register classes, as integers
  which are bit masks.  The :samp:`{n}` th integer specifies the contents of class
  :samp:`{n}`.  The way the integer :samp:`{mask}` is interpreted is that
  register :samp:`{r}` is in the class if ``mask & (1 << r)`` is 1.

  When the machine has more than 32 registers, an integer does not suffice.
  Then the integers are replaced by sub-initializers, braced groupings containing
  several integers.  Each sub-initializer must be suitable as an initializer
  for the type ``HARD_REG_SET`` which is defined in :samp:`hard-reg-set.h`.
  In this situation, the first integer in each sub-initializer corresponds to
  registers 0 through 31, the second integer to registers 32 through 63, and
  so on.

.. c:macro:: REGNO_REG_CLASS (regno)

  A C expression whose value is a register class containing hard register
  :samp:`{regno}`.  In general there is more than one such class; choose a class
  which is :dfn:`minimal`, meaning that no smaller class also contains the
  register.

.. c:macro:: BASE_REG_CLASS

  A macro whose definition is the name of the class to which a valid
  base register must belong.  A base register is one used in an address
  which is the register value plus a displacement.

.. c:macro:: MODE_BASE_REG_CLASS (mode)

  This is a variation of the ``BASE_REG_CLASS`` macro which allows
  the selection of a base register in a mode dependent manner.  If
  :samp:`{mode}` is VOIDmode then it should return the same value as
  ``BASE_REG_CLASS``.

.. c:macro:: MODE_BASE_REG_REG_CLASS (mode)

  A C expression whose value is the register class to which a valid
  base register must belong in order to be used in a base plus index
  register address.  You should define this macro if base plus index
  addresses have different requirements than other base register uses.

.. c:macro:: MODE_CODE_BASE_REG_CLASS (mode, address_space, outer_code, index_code)

  A C expression whose value is the register class to which a valid
  base register for a memory reference in mode :samp:`{mode}` to address
  space :samp:`{address_space}` must belong.  :samp:`{outer_code}` and :samp:`{index_code}`
  define the context in which the base register occurs.  :samp:`{outer_code}` is
  the code of the immediately enclosing expression (``MEM`` for the top level
  of an address, ``ADDRESS`` for something that occurs in an
  ``address_operand``).  :samp:`{index_code}` is the code of the corresponding
  index expression if :samp:`{outer_code}` is ``PLUS`` ; ``SCRATCH`` otherwise.

.. c:macro:: INDEX_REG_CLASS

  A macro whose definition is the name of the class to which a valid
  index register must belong.  An index register is one used in an
  address where its value is either multiplied by a scale factor or
  added to another register (as well as added to a displacement).

.. c:macro:: REGNO_OK_FOR_BASE_P (num)

  A C expression which is nonzero if register number :samp:`{num}` is
  suitable for use as a base register in operand addresses.

.. c:macro:: REGNO_MODE_OK_FOR_BASE_P (num, mode)

  A C expression that is just like ``REGNO_OK_FOR_BASE_P``, except that
  that expression may examine the mode of the memory reference in
  :samp:`{mode}`.  You should define this macro if the mode of the memory
  reference affects whether a register may be used as a base register.  If
  you define this macro, the compiler will use it instead of
  ``REGNO_OK_FOR_BASE_P``.  The mode may be ``VOIDmode`` for
  addresses that appear outside a ``MEM``, i.e., as an
  ``address_operand``.

.. c:macro:: REGNO_MODE_OK_FOR_REG_BASE_P (num, mode)

  A C expression which is nonzero if register number :samp:`{num}` is suitable for
  use as a base register in base plus index operand addresses, accessing
  memory in mode :samp:`{mode}`.  It may be either a suitable hard register or a
  pseudo register that has been allocated such a hard register.  You should
  define this macro if base plus index addresses have different requirements
  than other base register uses.

  Use of this macro is deprecated; please use the more general
  ``REGNO_MODE_CODE_OK_FOR_BASE_P``.

.. c:macro:: REGNO_MODE_CODE_OK_FOR_BASE_P (num, mode, address_space, outer_code, index_code)

  A C expression which is nonzero if register number :samp:`{num}` is
  suitable for use as a base register in operand addresses, accessing
  memory in mode :samp:`{mode}` in address space :samp:`{address_space}`.
  This is similar to ``REGNO_MODE_OK_FOR_BASE_P``, except
  that that expression may examine the context in which the register
  appears in the memory reference.  :samp:`{outer_code}` is the code of the
  immediately enclosing expression (``MEM`` if at the top level of the
  address, ``ADDRESS`` for something that occurs in an
  ``address_operand``).  :samp:`{index_code}` is the code of the
  corresponding index expression if :samp:`{outer_code}` is ``PLUS`` ;
  ``SCRATCH`` otherwise.  The mode may be ``VOIDmode`` for addresses
  that appear outside a ``MEM``, i.e., as an ``address_operand``.

.. c:macro:: REGNO_OK_FOR_INDEX_P (num)

  A C expression which is nonzero if register number :samp:`{num}` is
  suitable for use as an index register in operand addresses.  It may be
  either a suitable hard register or a pseudo register that has been
  allocated such a hard register.

  The difference between an index register and a base register is that
  the index register may be scaled.  If an address involves the sum of
  two registers, neither one of them scaled, then either one may be
  labeled the 'base' and the other the 'index'; but whichever
  labeling is used must fit the machine's constraints of which registers
  may serve in each capacity.  The compiler will try both labelings,
  looking for one that is valid, and will reload one or both registers
  only if neither labeling works.

.. function:: reg_class_t TARGET_PREFERRED_RENAME_CLASS (reg_class_t rclass)

  .. hook-start:TARGET_PREFERRED_RENAME_CLASS

  A target hook that places additional preference on the register
  class to use when it is necessary to rename a register in class
  :samp:`{rclass}` to another class, or perhaps :samp:`{NO_REGS}`, if no
  preferred register class is found or hook ``preferred_rename_class``
  is not implemented.
  Sometimes returning a more restrictive class makes better code.  For
  example, on ARM, thumb-2 instructions using ``LO_REGS`` may be
  smaller than instructions using ``GENERIC_REGS``.  By returning
  ``LO_REGS`` from ``preferred_rename_class``, code size can
  be reduced.

.. hook-end

.. function:: reg_class_t TARGET_PREFERRED_RELOAD_CLASS (rtx x, reg_class_t rclass)

  .. hook-start:TARGET_PREFERRED_RELOAD_CLASS

  A target hook that places additional restrictions on the register class
  to use when it is necessary to copy value :samp:`{x}` into a register in class
  :samp:`{rclass}`.  The value is a register class; perhaps :samp:`{rclass}`, or perhaps
  another, smaller class.

  The default version of this hook always returns value of ``rclass`` argument.

  Sometimes returning a more restrictive class makes better code.  For
  example, on the 68000, when :samp:`{x}` is an integer constant that is in range
  for a :samp:`moveq` instruction, the value of this macro is always
  ``DATA_REGS`` as long as :samp:`{rclass}` includes the data registers.
  Requiring a data register guarantees that a :samp:`moveq` will be used.

  One case where ``TARGET_PREFERRED_RELOAD_CLASS`` must not return
  :samp:`{rclass}` is if :samp:`{x}` is a legitimate constant which cannot be
  loaded into some register class.  By returning ``NO_REGS`` you can
  force :samp:`{x}` into a memory location.  For example, rs6000 can load
  immediate values into general-purpose registers, but does not have an
  instruction for loading an immediate value into a floating-point
  register, so ``TARGET_PREFERRED_RELOAD_CLASS`` returns ``NO_REGS`` when
  :samp:`{x}` is a floating-point constant.  If the constant can't be loaded
  into any kind of register, code generation will be better if
  ``TARGET_LEGITIMATE_CONSTANT_P`` makes the constant illegitimate instead
  of using ``TARGET_PREFERRED_RELOAD_CLASS``.

  If an insn has pseudos in it after register allocation, reload will go
  through the alternatives and call repeatedly ``TARGET_PREFERRED_RELOAD_CLASS``
  to find the best one.  Returning ``NO_REGS``, in this case, makes
  reload add a ``!`` in front of the constraint: the x86 back-end uses
  this feature to discourage usage of 387 registers when math is done in
  the SSE registers (and vice versa).

.. hook-end

.. c:macro:: PREFERRED_RELOAD_CLASS (x, class)

  A C expression that places additional restrictions on the register class
  to use when it is necessary to copy value :samp:`{x}` into a register in class
  :samp:`{class}`.  The value is a register class; perhaps :samp:`{class}`, or perhaps
  another, smaller class.  On many machines, the following definition is
  safe:

  .. code-block:: c++

    #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

  Sometimes returning a more restrictive class makes better code.  For
  example, on the 68000, when :samp:`{x}` is an integer constant that is in range
  for a :samp:`moveq` instruction, the value of this macro is always
  ``DATA_REGS`` as long as :samp:`{class}` includes the data registers.
  Requiring a data register guarantees that a :samp:`moveq` will be used.

  One case where ``PREFERRED_RELOAD_CLASS`` must not return
  :samp:`{class}` is if :samp:`{x}` is a legitimate constant which cannot be
  loaded into some register class.  By returning ``NO_REGS`` you can
  force :samp:`{x}` into a memory location.  For example, rs6000 can load
  immediate values into general-purpose registers, but does not have an
  instruction for loading an immediate value into a floating-point
  register, so ``PREFERRED_RELOAD_CLASS`` returns ``NO_REGS`` when
  :samp:`{x}` is a floating-point constant.  If the constant cannot be loaded
  into any kind of register, code generation will be better if
  ``TARGET_LEGITIMATE_CONSTANT_P`` makes the constant illegitimate instead
  of using ``TARGET_PREFERRED_RELOAD_CLASS``.

  If an insn has pseudos in it after register allocation, reload will go
  through the alternatives and call repeatedly ``PREFERRED_RELOAD_CLASS``
  to find the best one.  Returning ``NO_REGS``, in this case, makes
  reload add a ``!`` in front of the constraint: the x86 back-end uses
  this feature to discourage usage of 387 registers when math is done in
  the SSE registers (and vice versa).

.. function:: reg_class_t TARGET_PREFERRED_OUTPUT_RELOAD_CLASS (rtx x, reg_class_t rclass)

  .. hook-start:TARGET_PREFERRED_OUTPUT_RELOAD_CLASS

  Like ``TARGET_PREFERRED_RELOAD_CLASS``, but for output reloads instead of
  input reloads.

  The default version of this hook always returns value of ``rclass``
  argument.

  You can also use ``TARGET_PREFERRED_OUTPUT_RELOAD_CLASS`` to discourage
  reload from using some alternatives, like ``TARGET_PREFERRED_RELOAD_CLASS``.

.. hook-end

.. c:macro:: LIMIT_RELOAD_CLASS (mode, class)

  A C expression that places additional restrictions on the register class
  to use when it is necessary to be able to hold a value of mode
  :samp:`{mode}` in a reload register for which class :samp:`{class}` would
  ordinarily be used.

  Unlike ``PREFERRED_RELOAD_CLASS``, this macro should be used when
  there are certain modes that simply cannot go in certain reload classes.

  The value is a register class; perhaps :samp:`{class}`, or perhaps another,
  smaller class.

  Don't define this macro unless the target machine has limitations which
  require the macro to do something nontrivial.

.. function:: reg_class_t TARGET_SECONDARY_RELOAD (bool in_p, rtx x, reg_class_t reload_class, machine_mode reload_mode, secondary_reload_info *sri)

  .. hook-start:TARGET_SECONDARY_RELOAD

  Many machines have some registers that cannot be copied directly to or
  from memory or even from other types of registers.  An example is the
  :samp:`MQ` register, which on most machines, can only be copied to or
  from general registers, but not memory.  Below, we shall be using the
  term 'intermediate register' when a move operation cannot be performed
  directly, but has to be done by copying the source into the intermediate
  register first, and then copying the intermediate register to the
  destination.  An intermediate register always has the same mode as
  source and destination.  Since it holds the actual value being copied,
  reload might apply optimizations to re-use an intermediate register
  and eliding the copy from the source when it can determine that the
  intermediate register still holds the required value.

  Another kind of secondary reload is required on some machines which
  allow copying all registers to and from memory, but require a scratch
  register for stores to some memory locations (e.g., those with symbolic
  address on the RT, and those with certain symbolic address on the SPARC
  when compiling PIC).  Scratch registers need not have the same mode
  as the value being copied, and usually hold a different value than
  that being copied.  Special patterns in the md file are needed to
  describe how the copy is performed with the help of the scratch register;
  these patterns also describe the number, register class(es) and mode(s)
  of the scratch register(s).

  In some cases, both an intermediate and a scratch register are required.

  For input reloads, this target hook is called with nonzero :samp:`{in_p}`,
  and :samp:`{x}` is an rtx that needs to be copied to a register of class
  :samp:`{reload_class}` in :samp:`{reload_mode}`.  For output reloads, this target
  hook is called with zero :samp:`{in_p}`, and a register of class :samp:`{reload_class}`
  needs to be copied to rtx :samp:`{x}` in :samp:`{reload_mode}`.

  If copying a register of :samp:`{reload_class}` from/to :samp:`{x}` requires
  an intermediate register, the hook ``secondary_reload`` should
  return the register class required for this intermediate register.
  If no intermediate register is required, it should return NO_REGS.
  If more than one intermediate register is required, describe the one
  that is closest in the copy chain to the reload register.

  If scratch registers are needed, you also have to describe how to
  perform the copy from/to the reload register to/from this
  closest intermediate register.  Or if no intermediate register is
  required, but still a scratch register is needed, describe the
  copy  from/to the reload register to/from the reload operand :samp:`{x}`.

  You do this by setting ``sri->icode`` to the instruction code of a pattern
  in the md file which performs the move.  Operands 0 and 1 are the output
  and input of this copy, respectively.  Operands from operand 2 onward are
  for scratch operands.  These scratch operands must have a mode, and a
  single-register-class

  .. [later: or memory]

  output constraint.

  When an intermediate register is used, the ``secondary_reload``
  hook will be called again to determine how to copy the intermediate
  register to/from the reload operand :samp:`{x}`, so your hook must also
  have code to handle the register class of the intermediate operand.

  .. [For later: maybe we'll allow multi-alternative reload patterns -

  ..   the port maintainer could name a mov<mode> pattern that has clobbers -

  ..   and match the constraints of input and output to determine the required

  ..   alternative.  A restriction would be that constraints used to match

  ..   against reloads registers would have to be written as register class

  ..   constraints, or we need a new target macro / hook that tells us if an

  ..   arbitrary constraint can match an unknown register of a given class.

  ..   Such a macro / hook would also be useful in other places.]

  :samp:`{x}` might be a pseudo-register or a ``subreg`` of a
  pseudo-register, which could either be in a hard register or in memory.
  Use ``true_regnum`` to find out; it will return -1 if the pseudo is
  in memory and the hard register number if it is in a register.

  Scratch operands in memory (constraint ``"=m"`` / ``"=&m"``) are
  currently not supported.  For the time being, you will have to continue
  to use ``TARGET_SECONDARY_MEMORY_NEEDED`` for that purpose.

  ``copy_cost`` also uses this target hook to find out how values are
  copied.  If you want it to include some extra cost for the need to allocate
  (a) scratch register(s), set ``sri->extra_cost`` to the additional cost.
  Or if two dependent moves are supposed to have a lower cost than the sum
  of the individual moves due to expected fortuitous scheduling and/or special
  forwarding logic, you can set ``sri->extra_cost`` to a negative amount.

.. hook-end

.. c:macro:: SECONDARY_RELOAD_CLASS (class, mode, x)
             SECONDARY_INPUT_RELOAD_CLASS (class, mode, x)
             SECONDARY_OUTPUT_RELOAD_CLASS (class, mode, x)

  These macros are obsolete, new ports should use the target hook
  ``TARGET_SECONDARY_RELOAD`` instead.

  These are obsolete macros, replaced by the ``TARGET_SECONDARY_RELOAD``
  target hook.  Older ports still define these macros to indicate to the
  reload phase that it may
  need to allocate at least one register for a reload in addition to the
  register to contain the data.  Specifically, if copying :samp:`{x}` to a
  register :samp:`{class}` in :samp:`{mode}` requires an intermediate register,
  you were supposed to define ``SECONDARY_INPUT_RELOAD_CLASS`` to return the
  largest register class all of whose registers can be used as
  intermediate registers or scratch registers.

  If copying a register :samp:`{class}` in :samp:`{mode}` to :samp:`{x}` requires an
  intermediate or scratch register, ``SECONDARY_OUTPUT_RELOAD_CLASS``
  was supposed to be defined to return the largest register
  class required.  If the
  requirements for input and output reloads were the same, the macro
  ``SECONDARY_RELOAD_CLASS`` should have been used instead of defining both
  macros identically.

  The values returned by these macros are often ``GENERAL_REGS``.
  Return ``NO_REGS`` if no spare register is needed; i.e., if :samp:`{x}`
  can be directly copied to or from a register of :samp:`{class}` in
  :samp:`{mode}` without requiring a scratch register.  Do not define this
  macro if it would always return ``NO_REGS``.

  If a scratch register is required (either with or without an
  intermediate register), you were supposed to define patterns for
  :samp:`reload_in{m}` or :samp:`reload_out{m}`, as required
  (see :ref:`standard-names`.  These patterns, which were normally
  implemented with a ``define_expand``, should be similar to the
  :samp:`mov{m}` patterns, except that operand 2 is the scratch
  register.

  These patterns need constraints for the reload register and scratch
  register that
  contain a single register class.  If the original reload register (whose
  class is :samp:`{class}`) can meet the constraint given in the pattern, the
  value returned by these macros is used for the class of the scratch
  register.  Otherwise, two additional reload registers are required.
  Their classes are obtained from the constraints in the insn pattern.

  :samp:`{x}` might be a pseudo-register or a ``subreg`` of a
  pseudo-register, which could either be in a hard register or in memory.
  Use ``true_regnum`` to find out; it will return -1 if the pseudo is
  in memory and the hard register number if it is in a register.

  These macros should not be used in the case where a particular class of
  registers can only be copied to memory and not to another class of
  registers.  In that case, secondary reload registers are not needed and
  would not be helpful.  Instead, a stack location must be used to perform
  the copy and the ``movm`` pattern should use memory as an
  intermediate storage.  This case often occurs between floating-point and
  general registers.

.. function:: bool TARGET_SECONDARY_MEMORY_NEEDED (machine_mode mode, reg_class_t class1, reg_class_t class2)

  .. hook-start:TARGET_SECONDARY_MEMORY_NEEDED

  Certain machines have the property that some registers cannot be copied
  to some other registers without using memory.  Define this hook on
  those machines to return true if objects of mode :samp:`{m}` in registers
  of :samp:`{class1}` can only be copied to registers of class :samp:`{class2}` by
  storing a register of :samp:`{class1}` into memory and loading that memory
  location into a register of :samp:`{class2}`.  The default definition returns
  false for all inputs.

.. hook-end

.. c:macro:: SECONDARY_MEMORY_NEEDED_RTX (mode)

  Normally when ``TARGET_SECONDARY_MEMORY_NEEDED`` is defined, the compiler
  allocates a stack slot for a memory location needed for register copies.
  If this macro is defined, the compiler instead uses the memory location
  defined by this macro.

  Do not define this macro if you do not define
  ``TARGET_SECONDARY_MEMORY_NEEDED``.

.. function:: machine_mode TARGET_SECONDARY_MEMORY_NEEDED_MODE (machine_mode mode)

  .. hook-start:TARGET_SECONDARY_MEMORY_NEEDED_MODE

  If ``TARGET_SECONDARY_MEMORY_NEEDED`` tells the compiler to use memory
  when moving between two particular registers of mode :samp:`{mode}`,
  this hook specifies the mode that the memory should have.

  The default depends on ``TARGET_LRA_P``.  Without LRA, the default
  is to use a word-sized mode for integral modes that are smaller than a
  a word.  This is right thing to do on most machines because it ensures
  that all bits of the register are copied and prevents accesses to the
  registers in a narrower mode, which some machines prohibit for
  floating-point registers.

  However, this default behavior is not correct on some machines, such as
  the DEC Alpha, that store short integers in floating-point registers
  differently than in integer registers.  On those machines, the default
  widening will not work correctly and you must define this hook to
  suppress that widening in some cases.  See the file :samp:`alpha.cc` for
  details.

  With LRA, the default is to use :samp:`{mode}` unmodified.

.. hook-end

.. function:: void TARGET_SELECT_EARLY_REMAT_MODES (sbitmap modes)

  .. hook-start:TARGET_SELECT_EARLY_REMAT_MODES

  On some targets, certain modes cannot be held in registers around a
  standard ABI call and are relatively expensive to spill to the stack.
  The early rematerialization pass can help in such cases by aggressively
  recomputing values after calls, so that they don't need to be spilled.

  This hook returns the set of such modes by setting the associated bits
  in :samp:`{modes}`.  The default implementation selects no modes, which has
  the effect of disabling the early rematerialization pass.

.. hook-end

.. function:: bool TARGET_CLASS_LIKELY_SPILLED_P (reg_class_t rclass)

  .. hook-start:TARGET_CLASS_LIKELY_SPILLED_P

  A target hook which returns ``true`` if pseudos that have been assigned
  to registers of class :samp:`{rclass}` would likely be spilled because
  registers of :samp:`{rclass}` are needed for spill registers.

  The default version of this target hook returns ``true`` if :samp:`{rclass}`
  has exactly one register and ``false`` otherwise.  On most machines, this
  default should be used.  For generally register-starved machines, such as
  i386, or machines with right register constraints, such as SH, this hook
  can be used to avoid excessive spilling.

  This hook is also used by some of the global intra-procedural code
  transformations to throtle code motion, to avoid increasing register
  pressure.

.. hook-end

.. function:: unsigned char TARGET_CLASS_MAX_NREGS (reg_class_t rclass, machine_mode mode)

  .. hook-start:TARGET_CLASS_MAX_NREGS

  A target hook returns the maximum number of consecutive registers
  of class :samp:`{rclass}` needed to hold a value of mode :samp:`{mode}`.

  This is closely related to the macro ``TARGET_HARD_REGNO_NREGS``.
  In fact, the value returned by ``TARGET_CLASS_MAX_NREGS (rclass,
  mode)`` target hook should be the maximum value of
  ``TARGET_HARD_REGNO_NREGS (regno, mode)`` for all :samp:`{regno}`
  values in the class :samp:`{rclass}`.

  This target hook helps control the handling of multiple-word values
  in the reload pass.

  The default version of this target hook returns the size of :samp:`{mode}`
  in words.

.. hook-end

.. c:macro:: CLASS_MAX_NREGS (class, mode)

  A C expression for the maximum number of consecutive registers
  of class :samp:`{class}` needed to hold a value of mode :samp:`{mode}`.

  This is closely related to the macro ``TARGET_HARD_REGNO_NREGS``.  In fact,
  the value of the macro ``CLASS_MAX_NREGS (class, mode)``
  should be the maximum value of ``TARGET_HARD_REGNO_NREGS (regno,
  mode)`` for all :samp:`{regno}` values in the class :samp:`{class}`.

  This macro helps control the handling of multiple-word values
  in the reload pass.

.. function:: bool TARGET_CAN_CHANGE_MODE_CLASS (machine_mode from, machine_mode to, reg_class_t rclass)

  .. hook-start:TARGET_CAN_CHANGE_MODE_CLASS

  This hook returns true if it is possible to bitcast values held in
  registers of class :samp:`{rclass}` from mode :samp:`{from}` to mode :samp:`{to}`
  and if doing so preserves the low-order bits that are common to both modes.
  The result is only meaningful if :samp:`{rclass}` has registers that can hold
  both ``from`` and ``to``.  The default implementation returns true.

  As an example of when such bitcasting is invalid, loading 32-bit integer or
  floating-point objects into floating-point registers on Alpha extends them
  to 64 bits.  Therefore loading a 64-bit object and then storing it as a
  32-bit object does not store the low-order 32 bits, as would be the case
  for a normal register.  Therefore, :samp:`alpha.h` defines
  ``TARGET_CAN_CHANGE_MODE_CLASS`` to return:

  .. code-block:: c++

    (GET_MODE_SIZE (from) == GET_MODE_SIZE (to)
     || !reg_classes_intersect_p (FLOAT_REGS, rclass))

  Even if storing from a register in mode :samp:`{to}` would be valid,
  if both :samp:`{from}` and ``raw_reg_mode`` for :samp:`{rclass}` are wider
  than ``word_mode``, then we must prevent :samp:`{to}` narrowing the
  mode.  This happens when the middle-end assumes that it can load
  or store pieces of an :samp:`{N}` -word pseudo, and that the pseudo will
  eventually be allocated to :samp:`{N}` ``word_mode`` hard registers.
  Failure to prevent this kind of mode change will result in the
  entire ``raw_reg_mode`` being modified instead of the partial
  value that the middle-end intended.

.. hook-end

.. function:: reg_class_t TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS (int, reg_class_t, reg_class_t)

  .. hook-start:TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS

  A target hook which can change allocno class for given pseudo from
  allocno and best class calculated by IRA.

  The default version of this target hook always returns given class.

.. hook-end

.. function:: bool TARGET_LRA_P (void)

  .. hook-start:TARGET_LRA_P

  A target hook which returns true if we use LRA instead of reload pass.

  The default version of this target hook returns true.  New ports
  should use LRA, and existing ports are encouraged to convert.

.. hook-end

.. function:: int TARGET_REGISTER_PRIORITY (int)

  .. hook-start:TARGET_REGISTER_PRIORITY

  A target hook which returns the register priority number to which the
  register :samp:`{hard_regno}` belongs to.  The bigger the number, the
  more preferable the hard register usage (when all other conditions are
  the same).  This hook can be used to prefer some hard register over
  others in LRA.  For example, some x86-64 register usage needs
  additional prefix which makes instructions longer.  The hook can
  return lower priority number for such registers make them less favorable
  and as result making the generated code smaller.

  The default version of this target hook returns always zero.

.. hook-end

.. function:: bool TARGET_REGISTER_USAGE_LEVELING_P (void)

  .. hook-start:TARGET_REGISTER_USAGE_LEVELING_P

  A target hook which returns true if we need register usage leveling.
  That means if a few hard registers are equally good for the
  assignment, we choose the least used hard register.  The register
  usage leveling may be profitable for some targets.  Don't use the
  usage leveling for targets with conditional execution or targets
  with big register files as it hurts if-conversion and cross-jumping
  optimizations.

  The default version of this target hook returns always false.

.. hook-end

.. function:: bool TARGET_DIFFERENT_ADDR_DISPLACEMENT_P (void)

  .. hook-start:TARGET_DIFFERENT_ADDR_DISPLACEMENT_P

  A target hook which returns true if an address with the same structure
  can have different maximal legitimate displacement.  For example, the
  displacement can depend on memory mode or on operand combinations in
  the insn.

  The default version of this target hook returns always false.

.. hook-end

.. function:: bool TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P (rtx subst)

  .. hook-start:TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P

  A target hook which returns ``true`` if :samp:`{subst}` can't
  substitute safely pseudos with equivalent memory values during
  register allocation.
  The default version of this target hook returns ``false``.
  On most machines, this default should be used.  For generally
  machines with non orthogonal register usage for addressing, such
  as SH, this hook can be used to avoid excessive spilling.

.. hook-end

.. function:: bool TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT (rtx *offset1, rtx *offset2, poly_int64 orig_offset, machine_mode mode)

  .. hook-start:TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT

  This hook tries to split address offset :samp:`{orig_offset}` into
  two parts: one that should be added to the base address to create
  a local anchor point, and an additional offset that can be applied
  to the anchor to address a value of mode :samp:`{mode}`.  The idea is that
  the local anchor could be shared by other accesses to nearby locations.

  The hook returns true if it succeeds, storing the offset of the
  anchor from the base in :samp:`{offset1}` and the offset of the final address
  from the anchor in :samp:`{offset2}`.  The default implementation returns false.

.. hook-end

.. function:: reg_class_t TARGET_SPILL_CLASS (reg_class_t, machine_mode)

  .. hook-start:TARGET_SPILL_CLASS

  This hook defines a class of registers which could be used for spilling
  pseudos of the given mode and class, or ``NO_REGS`` if only memory
  should be used.  Not defining this hook is equivalent to returning
  ``NO_REGS`` for all inputs.

.. hook-end

.. function:: bool TARGET_ADDITIONAL_ALLOCNO_CLASS_P (reg_class_t)

  .. hook-start:TARGET_ADDITIONAL_ALLOCNO_CLASS_P

  This hook should return ``true`` if given class of registers should
  be an allocno class in any way.  Usually RA uses only one register
  class from all classes containing the same register set.  In some
  complicated cases, you need to have two or more such classes as
  allocno ones for RA correct work.  Not defining this hook is
  equivalent to returning ``false`` for all inputs.

.. hook-end

.. function:: scalar_int_mode TARGET_CSTORE_MODE (enum insn_code icode)

  .. hook-start:TARGET_CSTORE_MODE

  This hook defines the machine mode to use for the boolean result of
  conditional store patterns.  The ICODE argument is the instruction code
  for the cstore being performed.  Not definiting this hook is the same
  as accepting the mode encoded into operand 0 of the cstore expander
  patterns.

.. hook-end

.. function:: int TARGET_COMPUTE_PRESSURE_CLASSES (enum reg_class *pressure_classes)

  .. hook-start:TARGET_COMPUTE_PRESSURE_CLASSES

  A target hook which lets a backend compute the set of pressure classes to
  be used by those optimization passes which take register pressure into
  account, as opposed to letting IRA compute them.  It returns the number of
  register classes stored in the array :samp:`{pressure_classes}`.

.. hook-end