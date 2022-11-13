..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL side effect expressions

.. _side-effects:

Side Effect Expressions
***********************

The expression codes described so far represent values, not actions.
But machine instructions never produce values; they are meaningful
only for their side effects on the state of the machine.  Special
expression codes are used to represent side effects.

The body of an instruction is always one of these side effect codes;
the codes described above, which represent values, appear only as
the operands of these.

.. index:: set

:samp:`(set {lval} {x})`
  Represents the action of storing the value of :samp:`{x}` into the place
  represented by :samp:`{lval}`.  :samp:`{lval}` must be an expression
  representing a place that can be stored in: ``reg`` (or ``subreg``,
  ``strict_low_part`` or ``zero_extract``), ``mem``, ``pc``,
  or ``parallel``.

  If :samp:`{lval}` is a ``reg``, ``subreg`` or ``mem``, it has a
  machine mode; then :samp:`{x}` must be valid for that mode.

  If :samp:`{lval}` is a ``reg`` whose machine mode is less than the full
  width of the register, then it means that the part of the register
  specified by the machine mode is given the specified value and the
  rest of the register receives an undefined value.  Likewise, if
  :samp:`{lval}` is a ``subreg`` whose machine mode is narrower than
  the mode of the register, the rest of the register can be changed in
  an undefined way.

  If :samp:`{lval}` is a ``strict_low_part`` of a subreg, then the part
  of the register specified by the machine mode of the ``subreg`` is
  given the value :samp:`{x}` and the rest of the register is not changed.

  If :samp:`{lval}` is a ``zero_extract``, then the referenced part of
  the bit-field (a memory or register reference) specified by the
  ``zero_extract`` is given the value :samp:`{x}` and the rest of the
  bit-field is not changed.  Note that ``sign_extract`` cannot
  appear in :samp:`{lval}`.

  If :samp:`{lval}` is a ``parallel``, it is used to represent the case of
  a function returning a structure in multiple registers.  Each element
  of the ``parallel`` is an ``expr_list`` whose first operand is a
  ``reg`` and whose second operand is a ``const_int`` representing the
  offset (in bytes) into the structure at which the data in that register
  corresponds.  The first element may be null to indicate that the structure
  is also passed partly in memory.

  .. index:: jump instructions and set, if_then_else usage

  If :samp:`{lval}` is ``(pc)``, we have a jump instruction, and the
  possibilities for :samp:`{x}` are very limited.  It may be a
  ``label_ref`` expression (unconditional jump).  It may be an
  ``if_then_else`` (conditional jump), in which case either the
  second or the third operand must be ``(pc)`` (for the case which
  does not jump) and the other of the two must be a ``label_ref``
  (for the case which does jump).  :samp:`{x}` may also be a ``mem`` or
  ``(plus:SI (pc) y)``, where :samp:`{y}` may be a ``reg`` or a
  ``mem`` ; these unusual patterns are used to represent jumps through
  branch tables.

  If :samp:`{lval}` is not ``(pc)``, the mode of
  :samp:`{lval}` must not be ``VOIDmode`` and the mode of :samp:`{x}` must be
  valid for the mode of :samp:`{lval}`.

  .. index:: SET_DEST, SET_SRC

  :samp:`{lval}` is customarily accessed with the ``SET_DEST`` macro and
  :samp:`{x}` with the ``SET_SRC`` macro.

  .. index:: return

``(return)``
  As the sole expression in a pattern, represents a return from the
  current function, on machines where this can be done with one
  instruction, such as VAXen.  On machines where a multi-instruction
  'epilogue' must be executed in order to return from the function,
  returning is done by jumping to a label which precedes the epilogue, and
  the ``return`` expression code is never used.

  Inside an ``if_then_else`` expression, represents the value to be
  placed in ``pc`` to return to the caller.

  Note that an insn pattern of ``(return)`` is logically equivalent to
  ``(set (pc) (return))``, but the latter form is never used.

  .. index:: simple_return

``(simple_return)``
  Like ``(return)``, but truly represents only a function return, while
  ``(return)`` may represent an insn that also performs other functions
  of the function epilogue.  Like ``(return)``, this may also occur in
  conditional jumps.

  .. index:: call

:samp:`(call {function} {nargs})`
  Represents a function call.  :samp:`{function}` is a ``mem`` expression
  whose address is the address of the function to be called.
  :samp:`{nargs}` is an expression which can be used for two purposes: on
  some machines it represents the number of bytes of stack argument; on
  others, it represents the number of argument registers.

  Each machine has a standard machine mode which :samp:`{function}` must
  have.  The machine description defines macro ``FUNCTION_MODE`` to
  expand into the requisite mode name.  The purpose of this mode is to
  specify what kind of addressing is allowed, on machines where the
  allowed kinds of addressing depend on the machine mode being
  addressed.

  .. index:: clobber

:samp:`(clobber {x})`
  Represents the storing or possible storing of an unpredictable,
  undescribed value into :samp:`{x}`, which must be a ``reg``,
  ``scratch``, ``parallel`` or ``mem`` expression.

  One place this is used is in string instructions that store standard
  values into particular hard registers.  It may not be worth the
  trouble to describe the values that are stored, but it is essential to
  inform the compiler that the registers will be altered, lest it
  attempt to keep data in them across the string instruction.

  If :samp:`{x}` is ``(mem:BLK (const_int 0))`` or
  ``(mem:BLK (scratch))``, it means that all memory
  locations must be presumed clobbered.  If :samp:`{x}` is a ``parallel``,
  it has the same meaning as a ``parallel`` in a ``set`` expression.

  Note that the machine description classifies certain hard registers as
  'call-clobbered'.  All function call instructions are assumed by
  default to clobber these registers, so there is no need to use
  ``clobber`` expressions to indicate this fact.  Also, each function
  call is assumed to have the potential to alter any memory location,
  unless the function is declared ``const``.

  If the last group of expressions in a ``parallel`` are each a
  ``clobber`` expression whose arguments are ``reg`` or
  ``match_scratch`` (see :ref:`rtl-template`) expressions, the combiner
  phase can add the appropriate ``clobber`` expressions to an insn it
  has constructed when doing so will cause a pattern to be matched.

  This feature can be used, for example, on a machine that whose multiply
  and add instructions don't use an MQ register but which has an
  add-accumulate instruction that does clobber the MQ register.  Similarly,
  a combined instruction might require a temporary register while the
  constituent instructions might not.

  When a ``clobber`` expression for a register appears inside a
  ``parallel`` with other side effects, the register allocator
  guarantees that the register is unoccupied both before and after that
  insn if it is a hard register clobber.  For pseudo-register clobber,
  the register allocator and the reload pass do not assign the same hard
  register to the clobber and the input operands if there is an insn
  alternative containing the :samp:`&` constraint (see :ref:`modifiers`) for
  the clobber and the hard register is in register classes of the
  clobber in the alternative.  You can clobber either a specific hard
  register, a pseudo register, or a ``scratch`` expression; in the
  latter two cases, GCC will allocate a hard register that is available
  there for use as a temporary.

  For instructions that require a temporary register, you should use
  ``scratch`` instead of a pseudo-register because this will allow the
  combiner phase to add the ``clobber`` when required.  You do this by
  coding (``clobber`` (``match_scratch`` ...)).  If you do
  clobber a pseudo register, use one which appears nowhere else---generate
  a new one each time.  Otherwise, you may confuse CSE.

  There is one other known use for clobbering a pseudo register in a
  ``parallel`` : when one of the input operands of the insn is also
  clobbered by the insn.  In this case, using the same pseudo register in
  the clobber and elsewhere in the insn produces the expected results.

  .. index:: use

:samp:`(use {x})`
  Represents the use of the value of :samp:`{x}`.  It indicates that the
  value in :samp:`{x}` at this point in the program is needed, even though
  it may not be apparent why this is so.  Therefore, the compiler will
  not attempt to delete previous instructions whose only effect is to
  store a value in :samp:`{x}`.  :samp:`{x}` must be a ``reg`` expression.

  In some situations, it may be tempting to add a ``use`` of a
  register in a ``parallel`` to describe a situation where the value
  of a special register will modify the behavior of the instruction.
  A hypothetical example might be a pattern for an addition that can
  either wrap around or use saturating addition depending on the value
  of a special control register:

  .. code-block:: c++

    (parallel [(set (reg:SI 2) (unspec:SI [(reg:SI 3)
                                           (reg:SI 4)] 0))
               (use (reg:SI 1))])

  This will not work, several of the optimizers only look at expressions
  locally; it is very likely that if you have multiple insns with
  identical inputs to the ``unspec``, they will be optimized away even
  if register 1 changes in between.

  This means that ``use`` can *only* be used to describe
  that the register is live.  You should think twice before adding
  ``use`` statements, more often you will want to use ``unspec``
  instead.  The ``use`` RTX is most commonly useful to describe that
  a fixed register is implicitly used in an insn.  It is also safe to use
  in patterns where the compiler knows for other reasons that the result
  of the whole pattern is variable, such as :samp:`cpymem{m}` or
  :samp:`call` patterns.

  During the reload phase, an insn that has a ``use`` as pattern
  can carry a reg_equal note.  These ``use`` insns will be deleted
  before the reload phase exits.

  During the delayed branch scheduling phase, :samp:`{x}` may be an insn.
  This indicates that :samp:`{x}` previously was located at this place in the
  code and its data dependencies need to be taken into account.  These
  ``use`` insns will be deleted before the delayed branch scheduling
  phase exits.

  .. index:: parallel

:samp:`(parallel [{x0} {x1} ...])`
  Represents several side effects performed in parallel.  The square
  brackets stand for a vector; the operand of ``parallel`` is a
  vector of expressions.  :samp:`{x0}`, :samp:`{x1}` and so on are individual
  side effect expressions---expressions of code ``set``, ``call``,
  ``return``, ``simple_return``, ``clobber`` or ``use``.

  'In parallel' means that first all the values used in the individual
  side-effects are computed, and second all the actual side-effects are
  performed.  For example,

  .. code-block:: c++

    (parallel [(set (reg:SI 1) (mem:SI (reg:SI 1)))
               (set (mem:SI (reg:SI 1)) (reg:SI 1))])

  says unambiguously that the values of hard register 1 and the memory
  location addressed by it are interchanged.  In both places where
  ``(reg:SI 1)`` appears as a memory address it refers to the value
  in register 1 *before* the execution of the insn.

  It follows that it is *incorrect* to use ``parallel`` and
  expect the result of one ``set`` to be available for the next one.
  For example, people sometimes attempt to represent a jump-if-zero
  instruction this way:

  .. code-block:: c++

    (parallel [(set (reg:CC CC_REG) (reg:SI 34))
               (set (pc) (if_then_else
                            (eq (reg:CC CC_REG) (const_int 0))
                            (label_ref ...)
                            (pc)))])

  But this is incorrect, because it says that the jump condition depends
  on the condition code value *before* this instruction, not on the
  new value that is set by this instruction.

  .. index:: peephole optimization, RTL representation

  Peephole optimization, which takes place together with final assembly
  code output, can produce insns whose patterns consist of a ``parallel``
  whose elements are the operands needed to output the resulting
  assembler code---often ``reg``, ``mem`` or constant expressions.
  This would not be well-formed RTL at any other stage in compilation,
  but it is OK then because no further optimization remains to be done.

  .. index:: cond_exec

:samp:`(cond_exec [{cond} {expr}])`
  Represents a conditionally executed expression.  The :samp:`{expr}` is
  executed only if the :samp:`{cond}` is nonzero.  The :samp:`{cond}` expression
  must not have side-effects, but the :samp:`{expr}` may very well have
  side-effects.

  .. index:: sequence

:samp:`(sequence [{insns} ...])`
  Represents a sequence of insns.  If a ``sequence`` appears in the
  chain of insns, then each of the :samp:`{insns}` that appears in the sequence
  must be suitable for appearing in the chain of insns, i.e. must satisfy
  the ``INSN_P`` predicate.

  After delay-slot scheduling is completed, an insn and all the insns that
  reside in its delay slots are grouped together into a ``sequence``.
  The insn requiring the delay slot is the first insn in the vector;
  subsequent insns are to be placed in the delay slot.

  ``INSN_ANNULLED_BRANCH_P`` is set on an insn in a delay slot to
  indicate that a branch insn should be used that will conditionally annul
  the effect of the insns in the delay slots.  In such a case,
  ``INSN_FROM_TARGET_P`` indicates that the insn is from the target of
  the branch and should be executed only if the branch is taken; otherwise
  the insn should be executed only if the branch is not taken.
  See :ref:`delay-slots`.

  Some back ends also use ``sequence`` objects for purposes other than
  delay-slot groups.  This is not supported in the common parts of the
  compiler, which treat such sequences as delay-slot groups.

  DWARF2 Call Frame Address (CFA) adjustments are sometimes also expressed
  using ``sequence`` objects as the value of a ``RTX_FRAME_RELATED_P``
  note.  This only happens if the CFA adjustments cannot be easily derived
  from the pattern of the instruction to which the note is attached.  In
  such cases, the value of the note is used instead of best-guesing the
  semantics of the instruction.  The back end can attach notes containing
  a ``sequence`` of ``set`` patterns that express the effect of the
  parent instruction.

  These expression codes appear in place of a side effect, as the body of
  an insn, though strictly speaking they do not always describe side
  effects as such:

.. index:: asm_input

:samp:`(asm_input {s})`
  Represents literal assembler code as described by the string :samp:`{s}`.

  .. index:: unspec, unspec_volatile

:samp:`(unspec [{operands} ...] {index})` :samp:`(unspec_volatile [{operands} ...] {index})`
  Represents a machine-specific operation on :samp:`{operands}`.  :samp:`{index}`
  selects between multiple machine-specific operations.
  ``unspec_volatile`` is used for volatile operations and operations
  that may trap; ``unspec`` is used for other operations.

  These codes may appear inside a ``pattern`` of an
  insn, inside a ``parallel``, or inside an expression.

  .. index:: addr_vec

:samp:`(addr_vec:{m} [{lr0} {lr1} ...])`
  Represents a table of jump addresses.  The vector elements :samp:`{lr0}`,
  etc., are ``label_ref`` expressions.  The mode :samp:`{m}` specifies
  how much space is given to each address; normally :samp:`{m}` would be
  ``Pmode``.

  .. index:: addr_diff_vec

:samp:`(addr_diff_vec:{m} {base} [{lr0} {lr1} ...] {min} {max} {flags})`
  Represents a table of jump addresses expressed as offsets from
  :samp:`{base}`.  The vector elements :samp:`{lr0}`, etc., are ``label_ref``
  expressions and so is :samp:`{base}`.  The mode :samp:`{m}` specifies how much
  space is given to each address-difference.  :samp:`{min}` and :samp:`{max}`
  are set up by branch shortening and hold a label with a minimum and a
  maximum address, respectively.  :samp:`{flags}` indicates the relative
  position of :samp:`{base}`, :samp:`{min}` and :samp:`{max}` to the containing insn
  and of :samp:`{min}` and :samp:`{max}` to :samp:`{base}`.  See rtl.def for details.

  .. index:: prefetch

:samp:`(prefetch:{m} {addr} {rw} {locality})`
  Represents prefetch of memory at address :samp:`{addr}`.
  Operand :samp:`{rw}` is 1 if the prefetch is for data to be written, 0 otherwise;
  targets that do not support write prefetches should treat this as a normal
  prefetch.
  Operand :samp:`{locality}` specifies the amount of temporal locality; 0 if there
  is none or 1, 2, or 3 for increasing levels of temporal locality;
  targets that do not support locality hints should ignore this.

  This insn is used to minimize cache-miss latency by moving data into a
  cache before it is accessed.  It should use only non-faulting data prefetch
  instructions.