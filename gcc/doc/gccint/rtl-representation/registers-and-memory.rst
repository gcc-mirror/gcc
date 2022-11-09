..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL register expressions, RTL memory expressions

.. _regs-and-memory:

Registers and Memory
********************

Here are the RTL expression types for describing access to machine
registers and to main memory.

.. index:: reg, hard registers, pseudo registers

:samp:`(reg:{m} {n})`
  For small values of the integer :samp:`{n}` (those that are less than
  ``FIRST_PSEUDO_REGISTER``), this stands for a reference to machine
  register number :samp:`{n}` : a :dfn:`hard register`.  For larger values of
  :samp:`{n}`, it stands for a temporary value or :dfn:`pseudo register`.
  The compiler's strategy is to generate code assuming an unlimited
  number of such pseudo registers, and later convert them into hard
  registers or into memory references.

  :samp:`{m}` is the machine mode of the reference.  It is necessary because
  machines can generally refer to each register in more than one mode.
  For example, a register may contain a full word but there may be
  instructions to refer to it as a half word or as a single byte, as
  well as instructions to refer to it as a floating point number of
  various precisions.

  Even for a register that the machine can access in only one mode,
  the mode must always be specified.

  The symbol ``FIRST_PSEUDO_REGISTER`` is defined by the machine
  description, since the number of hard registers on the machine is an
  invariant characteristic of the machine.  Note, however, that not
  all of the machine registers must be general registers.  All the
  machine registers that can be used for storage of data are given
  hard register numbers, even those that can be used only in certain
  instructions or can hold only certain types of data.

  A hard register may be accessed in various modes throughout one
  function, but each pseudo register is given a natural mode
  and is accessed only in that mode.  When it is necessary to describe
  an access to a pseudo register using a nonnatural mode, a ``subreg``
  expression is used.

  A ``reg`` expression with a machine mode that specifies more than
  one word of data may actually stand for several consecutive registers.
  If in addition the register number specifies a hardware register, then
  it actually represents several consecutive hardware registers starting
  with the specified one.

  Each pseudo register number used in a function's RTL code is
  represented by a unique ``reg`` expression.

  .. index:: FIRST_VIRTUAL_REGISTER, LAST_VIRTUAL_REGISTER

  Some pseudo register numbers, those within the range of
  ``FIRST_VIRTUAL_REGISTER`` to ``LAST_VIRTUAL_REGISTER`` only
  appear during the RTL generation phase and are eliminated before the
  optimization phases.  These represent locations in the stack frame that
  cannot be determined until RTL generation for the function has been
  completed.  The following virtual register numbers are defined:

  .. index:: VIRTUAL_INCOMING_ARGS_REGNUM

  .. envvar:: VIRTUAL_INCOMING_ARGS_REGNUM

    This points to the first word of the incoming arguments passed on the
    stack.  Normally these arguments are placed there by the caller, but the
    callee may have pushed some arguments that were previously passed in
    registers.

    .. index:: FIRST_PARM_OFFSET and virtual registers, ARG_POINTER_REGNUM and virtual registers

    When RTL generation is complete, this virtual register is replaced
    by the sum of the register given by ``ARG_POINTER_REGNUM`` and the
    value of ``FIRST_PARM_OFFSET``.

    .. index:: FRAME_GROWS_DOWNWARD and virtual registers

  .. envvar:: VIRTUAL_STACK_VARS_REGNUM

    If ``FRAME_GROWS_DOWNWARD`` is defined to a nonzero value, this points
    to immediately above the first variable on the stack.  Otherwise, it points
    to the first variable on the stack.

    .. index:: TARGET_STARTING_FRAME_OFFSET and virtual registers, FRAME_POINTER_REGNUM and virtual registers

    ``VIRTUAL_STACK_VARS_REGNUM`` is replaced with the sum of the
    register given by ``FRAME_POINTER_REGNUM`` and the value
    ``TARGET_STARTING_FRAME_OFFSET``.

  .. envvar:: VIRTUAL_STACK_DYNAMIC_REGNUM

    This points to the location of dynamically allocated memory on the stack
    immediately after the stack pointer has been adjusted by the amount of
    memory desired.

    .. index:: STACK_DYNAMIC_OFFSET and virtual registers, STACK_POINTER_REGNUM and virtual registers

    This virtual register is replaced by the sum of the register given by
    ``STACK_POINTER_REGNUM`` and the value ``STACK_DYNAMIC_OFFSET``.

  .. envvar:: VIRTUAL_OUTGOING_ARGS_REGNUM

    This points to the location in the stack at which outgoing arguments
    should be written when the stack is pre-pushed (arguments pushed using
    push insns should always use ``STACK_POINTER_REGNUM``).

    .. index:: STACK_POINTER_OFFSET and virtual registers

    This virtual register is replaced by the sum of the register given by
    ``STACK_POINTER_REGNUM`` and the value ``STACK_POINTER_OFFSET``.

  .. index:: subreg

:samp:`(subreg:{m1} {reg:m2} {bytenum})`
  ``subreg`` expressions are used to refer to a register in a machine
  mode other than its natural one, or to refer to one register of
  a multi-part ``reg`` that actually refers to several registers.

  Each pseudo register has a natural mode.  If it is necessary to
  operate on it in a different mode, the register must be
  enclosed in a ``subreg``.

  There are currently three supported types for the first operand of a
  ``subreg`` :

  * pseudo registers
    This is the most common case.  Most ``subreg`` s have pseudo
    ``reg`` s as their first operand.

  * mem
    ``subreg`` s of ``mem`` were common in earlier versions of GCC and
    are still supported.  During the reload pass these are replaced by plain
    ``mem`` s.  On machines that do not do instruction scheduling, use of
    ``subreg`` s of ``mem`` are still used, but this is no longer
    recommended.  Such ``subreg`` s are considered to be
    ``register_operand`` s rather than ``memory_operand`` s before and
    during reload.  Because of this, the scheduling passes cannot properly
    schedule instructions with ``subreg`` s of ``mem``, so for machines
    that do scheduling, ``subreg`` s of ``mem`` should never be used.
    To support this, the combine and recog passes have explicit code to
    inhibit the creation of ``subreg`` s of ``mem`` when
    ``INSN_SCHEDULING`` is defined.

    The use of ``subreg`` s of ``mem`` after the reload pass is an area
    that is not well understood and should be avoided.  There is still some
    code in the compiler to support this, but this code has possibly rotted.
    This use of ``subreg`` s is discouraged and will most likely not be
    supported in the future.

  * hard registers
    It is seldom necessary to wrap hard registers in ``subreg`` s; such
    registers would normally reduce to a single ``reg`` rtx.  This use of
    ``subreg`` s is discouraged and may not be supported in the future.

  ``subreg`` s of ``subreg`` s are not supported.  Using
  ``simplify_gen_subreg`` is the recommended way to avoid this problem.

  ``subreg`` s come in two distinct flavors, each having its own
  usage and rules:

  Paradoxical subregs
    When :samp:`{m1}` is strictly wider than :samp:`{m2}`, the ``subreg``
    expression is called :dfn:`paradoxical`.  The canonical test for this
    class of ``subreg`` is:

    .. code-block:: c++

      paradoxical_subreg_p (m1, m2)

    Paradoxical ``subreg`` s can be used as both lvalues and rvalues.
    When used as an lvalue, the low-order bits of the source value
    are stored in :samp:`{reg}` and the high-order bits are discarded.
    When used as an rvalue, the low-order bits of the ``subreg`` are
    taken from :samp:`{reg}` while the high-order bits may or may not be
    defined.

    The high-order bits of rvalues are defined in the following circumstances:

    * ``subreg`` s of ``mem``
      When :samp:`{m2}` is smaller than a word, the macro ``LOAD_EXTEND_OP``,
      can control how the high-order bits are defined.

    * ``subreg`` of ``reg`` s
      The upper bits are defined when ``SUBREG_PROMOTED_VAR_P`` is true.
      ``SUBREG_PROMOTED_UNSIGNED_P`` describes what the upper bits hold.
      Such subregs usually represent local variables, register variables
      and parameter pseudo variables that have been promoted to a wider mode.

    :samp:`{bytenum}` is always zero for a paradoxical ``subreg``, even on
    big-endian targets.

    For example, the paradoxical ``subreg`` :

    .. code-block:: c++

      (set (subreg:SI (reg:HI x) 0) y)

    stores the lower 2 bytes of :samp:`{y}` in :samp:`{x}` and discards the upper
    2 bytes.  A subsequent:

    .. code-block:: c++

      (set z (subreg:SI (reg:HI x) 0))

    would set the lower two bytes of :samp:`{z}` to :samp:`{y}` and set the upper
    two bytes to an unknown value assuming ``SUBREG_PROMOTED_VAR_P`` is
    false.

  Normal subregs
    When :samp:`{m1}` is at least as narrow as :samp:`{m2}` the ``subreg``
    expression is called :dfn:`normal`.

    .. index:: REGMODE_NATURAL_SIZE

    Normal ``subreg`` s restrict consideration to certain bits of
    :samp:`{reg}`.  For this purpose, :samp:`{reg}` is divided into
    individually-addressable blocks in which each block has:

    .. code-block:: c++

      REGMODE_NATURAL_SIZE (m2)

    bytes.  Usually the value is ``UNITS_PER_WORD`` ; that is,
    most targets usually treat each word of a register as being
    independently addressable.

    There are two types of normal ``subreg``.  If :samp:`{m1}` is known
    to be no bigger than a block, the ``subreg`` refers to the
    least-significant part (or :dfn:`lowpart`) of one block of :samp:`{reg}`.
    If :samp:`{m1}` is known to be larger than a block, the ``subreg`` refers
    to two or more complete blocks.

    When used as an lvalue, ``subreg`` is a block-based accessor.
    Storing to a ``subreg`` modifies all the blocks of :samp:`{reg}` that
    overlap the ``subreg``, but it leaves the other blocks of :samp:`{reg}`
    alone.

    When storing to a normal ``subreg`` that is smaller than a block,
    the other bits of the referenced block are usually left in an undefined
    state.  This laxity makes it easier to generate efficient code for
    such instructions.  To represent an instruction that preserves all the
    bits outside of those in the ``subreg``, use ``strict_low_part``
    or ``zero_extract`` around the ``subreg``.

    :samp:`{bytenum}` must identify the offset of the first byte of the
    ``subreg`` from the start of :samp:`{reg}`, assuming that :samp:`{reg}` is
    laid out in memory order.  The memory order of bytes is defined by
    two target macros, ``WORDS_BIG_ENDIAN`` and ``BYTES_BIG_ENDIAN`` :

    .. index:: WORDS_BIG_ENDIAN, effect on subreg

    * ``WORDS_BIG_ENDIAN``, if set to 1, says that byte number zero is
      part of the most significant word; otherwise, it is part of the least
      significant word.

    .. index:: BYTES_BIG_ENDIAN, effect on subreg

    * ``BYTES_BIG_ENDIAN``, if set to 1, says that byte number zero is
      the most significant byte within a word; otherwise, it is the least
      significant byte within a word.

    .. index:: FLOAT_WORDS_BIG_ENDIAN, (lack of) effect on subreg

    On a few targets, ``FLOAT_WORDS_BIG_ENDIAN`` disagrees with
    ``WORDS_BIG_ENDIAN``.  However, most parts of the compiler treat
    floating point values as if they had the same endianness as integer
    values.  This works because they handle them solely as a collection of
    integer values, with no particular numerical value.  Only real.cc and
    the runtime libraries care about ``FLOAT_WORDS_BIG_ENDIAN``.

    Thus,

    .. code-block:: c++

      (subreg:HI (reg:SI x) 2)

    on a ``BYTES_BIG_ENDIAN``, :samp:`UNITS_PER_WORD == 4` target is the same as

    .. code-block:: c++

      (subreg:HI (reg:SI x) 0)

    on a little-endian, :samp:`UNITS_PER_WORD == 4` target.  Both
    ``subreg`` s access the lower two bytes of register :samp:`{x}`.

    Note that the byte offset is a polynomial integer; it may not be a
    compile-time constant on targets with variable-sized modes.  However,
    the restrictions above mean that there are only a certain set of
    acceptable offsets for a given combination of :samp:`{m1}` and :samp:`{m2}`.
    The compiler can always tell which blocks a valid subreg occupies, and
    whether the subreg is a lowpart of a block.

  A ``MODE_PARTIAL_INT`` mode behaves as if it were as wide as the
  corresponding ``MODE_INT`` mode, except that it has a number of
  undefined bits, which are determined by the precision of the
  mode.

  For example, on a little-endian target which defines ``PSImode``
  to have a precision of 20 bits:

  .. code-block:: c++

    (subreg:PSI (reg:SI 0) 0)

  accesses the low 20 bits of :samp:`(reg:SI 0)`.

  .. index:: REGMODE_NATURAL_SIZE

  Continuing with a ``PSImode`` precision of 20 bits, if we assume
  :samp:`REGMODE_NATURAL_SIZE (DImode) <= 4`,
  then the following two ``subreg`` s:

  .. code-block:: c++

    (subreg:PSI (reg:DI 0) 0)
    (subreg:PSI (reg:DI 0) 4)

  represent accesses to the low 20 bits of the two halves of
  :samp:`(reg:DI 0)`.

  If :samp:`REGMODE_NATURAL_SIZE (PSImode) <= 2` then these two ``subreg`` s:

  .. code-block:: c++

    (subreg:HI (reg:PSI 0) 0)
    (subreg:HI (reg:PSI 0) 2)

  represent independent 2-byte accesses that together span the whole
  of :samp:`(reg:PSI 0)`.  Storing to the first ``subreg`` does not
  affect the value of the second, and vice versa, so the assignment:

  .. code-block:: c++

    (set (subreg:HI (reg:PSI 0) 0) (reg:HI 4))

  sets the low 16 bits of :samp:`(reg:PSI 0)` to :samp:`(reg:HI 4)`, and
  the high 4 defined bits of :samp:`(reg:PSI 0)` retain their
  original value.  The behavior here is the same as for
  normal ``subreg`` s, when there are no
  ``MODE_PARTIAL_INT`` modes involved.

  .. index:: TARGET_CAN_CHANGE_MODE_CLASS and subreg semantics

  The rules above apply to both pseudo :samp:`{reg}` s and hard :samp:`{reg}` s.
  If the semantics are not correct for particular combinations of
  :samp:`{m1}`, :samp:`{m2}` and hard :samp:`{reg}`, the target-specific code
  must ensure that those combinations are never used.  For example:

  .. code-block:: c++

    TARGET_CAN_CHANGE_MODE_CLASS (m2, m1, class)

  must be false for every class :samp:`{class}` that includes :samp:`{reg}`.

  GCC must be able to determine at compile time whether a subreg is
  paradoxical, whether it occupies a whole number of blocks, or whether
  it is a lowpart of a block.  This means that certain combinations of
  variable-sized mode are not permitted.  For example, if :samp:`{m2}`
  holds :samp:`{n}` ``SI`` values, where :samp:`{n}` is greater than zero,
  it is not possible to form a ``DI`` ``subreg`` of it; such a
  ``subreg`` would be paradoxical when :samp:`{n}` is 1 but not when
  :samp:`{n}` is greater than 1.

  .. index:: SUBREG_REG, SUBREG_BYTE

  The first operand of a ``subreg`` expression is customarily accessed
  with the ``SUBREG_REG`` macro and the second operand is customarily
  accessed with the ``SUBREG_BYTE`` macro.

  It has been several years since a platform in which
  ``BYTES_BIG_ENDIAN`` not equal to ``WORDS_BIG_ENDIAN`` has
  been tested.  Anyone wishing to support such a platform in the future
  may be confronted with code rot.

  .. index:: scratch, scratch operands

:samp:`(scratch:{m})`
  This represents a scratch register that will be required for the
  execution of a single instruction and not used subsequently.  It is
  converted into a ``reg`` by either the local register allocator or
  the reload pass.

  ``scratch`` is usually present inside a ``clobber`` operation
  (see :ref:`side-effects`).

  On some machines, the condition code register is given a register number
  and a ``reg`` is used.
  Other machines store condition codes in general
  registers; in such cases a pseudo register should be used.

  Some machines, such as the SPARC and RS/6000, have two sets of
  arithmetic instructions, one that sets and one that does not set the
  condition code.  This is best handled by normally generating the
  instruction that does not set the condition code, and making a pattern
  that both performs the arithmetic and sets the condition code register.
  For examples, search for :samp:`addcc` and :samp:`andcc` in :samp:`sparc.md`.

  .. index:: pc

``(pc)``

  .. index:: program counter

  This represents the machine's program counter.  It has no operands and
  may not have a machine mode.  ``(pc)`` may be validly used only in
  certain specific contexts in jump instructions.

  .. index:: pc_rtx

  There is only one expression object of code ``pc`` ; it is the value
  of the variable ``pc_rtx``.  Any attempt to create an expression of
  code ``pc`` will return ``pc_rtx``.

  All instructions that do not jump alter the program counter implicitly
  by incrementing it, but there is no need to mention this in the RTL.

  .. index:: mem

:samp:`(mem:{m} {addr} {alias})`
  This RTX represents a reference to main memory at an address
  represented by the expression :samp:`{addr}`.  :samp:`{m}` specifies how large
  a unit of memory is accessed.  :samp:`{alias}` specifies an alias set for the
  reference.  In general two items are in different alias sets if they cannot
  reference the same memory address.

  The construct ``(mem:BLK (scratch))`` is considered to alias all
  other memories.  Thus it may be used as a memory barrier in epilogue
  stack deallocation patterns.

  .. index:: concat

:samp:`(concat{m} {rtx} {rtx})`
  This RTX represents the concatenation of two other RTXs.  This is used
  for complex values.  It should only appear in the RTL attached to
  declarations and during RTL generation.  It should not appear in the
  ordinary insn chain.

  .. index:: concatn

:samp:`(concatn{m} [{rtx} ...])`
  This RTX represents the concatenation of all the :samp:`{rtx}` to make a
  single value.  Like ``concat``, this should only appear in
  declarations, and not in the insn chain.
