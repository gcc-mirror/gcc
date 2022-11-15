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

.. include:: tm.rst.in
  :start-after: [TARGET_PREFERRED_RENAME_CLASS]
  :end-before: [TARGET_PREFERRED_RENAME_CLASS]


.. include:: tm.rst.in
  :start-after: [TARGET_PREFERRED_RELOAD_CLASS]
  :end-before: [TARGET_PREFERRED_RELOAD_CLASS]


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

.. include:: tm.rst.in
  :start-after: [TARGET_PREFERRED_OUTPUT_RELOAD_CLASS]
  :end-before: [TARGET_PREFERRED_OUTPUT_RELOAD_CLASS]


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

.. include:: tm.rst.in
  :start-after: [TARGET_SECONDARY_RELOAD]
  :end-before: [TARGET_SECONDARY_RELOAD]


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

.. include:: tm.rst.in
  :start-after: [TARGET_SECONDARY_MEMORY_NEEDED]
  :end-before: [TARGET_SECONDARY_MEMORY_NEEDED]


.. c:macro:: SECONDARY_MEMORY_NEEDED_RTX (mode)

  Normally when ``TARGET_SECONDARY_MEMORY_NEEDED`` is defined, the compiler
  allocates a stack slot for a memory location needed for register copies.
  If this macro is defined, the compiler instead uses the memory location
  defined by this macro.

  Do not define this macro if you do not define
  ``TARGET_SECONDARY_MEMORY_NEEDED``.

.. include:: tm.rst.in
  :start-after: [TARGET_SECONDARY_MEMORY_NEEDED_MODE]
  :end-before: [TARGET_SECONDARY_MEMORY_NEEDED_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_SELECT_EARLY_REMAT_MODES]
  :end-before: [TARGET_SELECT_EARLY_REMAT_MODES]


.. include:: tm.rst.in
  :start-after: [TARGET_CLASS_LIKELY_SPILLED_P]
  :end-before: [TARGET_CLASS_LIKELY_SPILLED_P]


.. include:: tm.rst.in
  :start-after: [TARGET_CLASS_MAX_NREGS]
  :end-before: [TARGET_CLASS_MAX_NREGS]


.. c:macro:: CLASS_MAX_NREGS (class, mode)

  A C expression for the maximum number of consecutive registers
  of class :samp:`{class}` needed to hold a value of mode :samp:`{mode}`.

  This is closely related to the macro ``TARGET_HARD_REGNO_NREGS``.  In fact,
  the value of the macro ``CLASS_MAX_NREGS (class, mode)``
  should be the maximum value of ``TARGET_HARD_REGNO_NREGS (regno,
  mode)`` for all :samp:`{regno}` values in the class :samp:`{class}`.

  This macro helps control the handling of multiple-word values
  in the reload pass.

.. include:: tm.rst.in
  :start-after: [TARGET_CAN_CHANGE_MODE_CLASS]
  :end-before: [TARGET_CAN_CHANGE_MODE_CLASS]


.. include:: tm.rst.in
  :start-after: [TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS]
  :end-before: [TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS]


.. include:: tm.rst.in
  :start-after: [TARGET_LRA_P]
  :end-before: [TARGET_LRA_P]


.. include:: tm.rst.in
  :start-after: [TARGET_REGISTER_PRIORITY]
  :end-before: [TARGET_REGISTER_PRIORITY]


.. include:: tm.rst.in
  :start-after: [TARGET_REGISTER_USAGE_LEVELING_P]
  :end-before: [TARGET_REGISTER_USAGE_LEVELING_P]


.. include:: tm.rst.in
  :start-after: [TARGET_DIFFERENT_ADDR_DISPLACEMENT_P]
  :end-before: [TARGET_DIFFERENT_ADDR_DISPLACEMENT_P]


.. include:: tm.rst.in
  :start-after: [TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P]
  :end-before: [TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P]


.. include:: tm.rst.in
  :start-after: [TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT]
  :end-before: [TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT]


.. include:: tm.rst.in
  :start-after: [TARGET_SPILL_CLASS]
  :end-before: [TARGET_SPILL_CLASS]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDITIONAL_ALLOCNO_CLASS_P]
  :end-before: [TARGET_ADDITIONAL_ALLOCNO_CLASS_P]


.. include:: tm.rst.in
  :start-after: [TARGET_CSTORE_MODE]
  :end-before: [TARGET_CSTORE_MODE]


.. include:: tm.rst.in
  :start-after: [TARGET_COMPUTE_PRESSURE_CLASSES]
  :end-before: [TARGET_COMPUTE_PRESSURE_CLASSES]
