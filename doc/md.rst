..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: simple constraints

.. _simple-constraints:

Simple Constraints
^^^^^^^^^^^^^^^^^^

The simplest kind of constraint is a string full of letters, each of
which describes one kind of operand that is permitted.  Here are
the letters that are allowed:

whitespace
  Whitespace characters are ignored and can be inserted at any position
  except the first.  This enables each alternative for different operands to
  be visually aligned in the machine description even if they have different
  number of constraints and modifiers.

  .. index:: m in constraint, memory references in constraints

m
  A memory operand is allowed, with any kind of address that the machine
  supports in general.
  Note that the letter used for the general memory constraint can be
  re-defined by a back end using the ``TARGET_MEM_CONSTRAINT`` macro.

  .. index:: offsettable address, o in constraint

o
  A memory operand is allowed, but only if the address is
  :dfn:`offsettable`.  This means that adding a small integer (actually,
  the width in bytes of the operand, as determined by its machine mode)
  may be added to the address and the result is also a valid memory
  address.

  .. index:: autoincrement/decrement addressing

  For example, an address which is constant is offsettable; so is an
  address that is the sum of a register and a constant (as long as a
  slightly larger constant is also within the range of address-offsets
  supported by the machine); but an autoincrement or autodecrement
  address is not offsettable.  More complicated indirect/indexed
  addresses may or may not be offsettable depending on the other
  addressing modes that the machine supports.

  Note that in an output operand which can be matched by another
  operand, the constraint letter :samp:`o` is valid only when accompanied
  by both :samp:`<` (if the target machine has predecrement addressing)
  and :samp:`>` (if the target machine has preincrement addressing).

  .. index:: V in constraint

V
  A memory operand that is not offsettable.  In other words, anything that
  would fit the :samp:`m` constraint but not the :samp:`o` constraint.

  .. index:: < in constraint

<
  A memory operand with autodecrement addressing (either predecrement or
  postdecrement) is allowed.  In inline ``asm`` this constraint is only
  allowed if the operand is used exactly once in an instruction that can
  handle the side effects.  Not using an operand with :samp:`<` in constraint
  string in the inline ``asm`` pattern at all or using it in multiple
  instructions isn't valid, because the side effects wouldn't be performed
  or would be performed more than once.  Furthermore, on some targets
  the operand with :samp:`<` in constraint string must be accompanied by
  special instruction suffixes like ``%U0`` instruction suffix on PowerPC
  or ``%P0`` on IA-64.

  .. index:: > in constraint

>
  A memory operand with autoincrement addressing (either preincrement or
  postincrement) is allowed.  In inline ``asm`` the same restrictions
  as for :samp:`<` apply.

  .. index:: r in constraint, registers in constraints

r
  A register operand is allowed provided that it is in a general
  register.

  .. index:: constants in constraints, i in constraint

i
  An immediate integer operand (one with constant value) is allowed.
  This includes symbolic constants whose values will be known only at
  assembly time or later.

  .. index:: n in constraint

n
  An immediate integer operand with a known numeric value is allowed.
  Many systems cannot support assembly-time constants for operands less
  than a word wide.  Constraints for these operands should use :samp:`n`
  rather than :samp:`i`.

  .. index:: I in constraint

:samp:`{I}, {J}, {K}, ... {P}`
  Other letters in the range :samp:`I` through :samp:`P` may be defined in
  a machine-dependent fashion to permit immediate integer operands with
  explicit integer values in specified ranges.  For example, on the
  68000, :samp:`I` is defined to stand for the range of values 1 to 8.
  This is the range permitted as a shift count in the shift
  instructions.

  .. index:: E in constraint

E
  An immediate floating operand (expression code ``const_double``) is
  allowed, but only if the target floating point format is the same as
  that of the host machine (on which the compiler is running).

  .. index:: F in constraint

F
  An immediate floating operand (expression code ``const_double`` or
  ``const_vector``) is allowed.

  .. index:: G in constraint, H in constraint

:samp:`{G}, {H}`
  :samp:`G` and :samp:`H` may be defined in a machine-dependent fashion to
  permit immediate floating operands in particular ranges of values.

  .. index:: s in constraint

s
  An immediate integer operand whose value is not an explicit integer is
  allowed.

  This might appear strange; if an insn allows a constant operand with a
  value not known at compile time, it certainly must allow any known
  value.  So why use :samp:`s` instead of :samp:`i`?  Sometimes it allows
  better code to be generated.

  For example, on the 68000 in a fullword instruction it is possible to
  use an immediate operand; but if the immediate value is between -128
  and 127, better code results from loading the value into a register and
  using the register.  This is because the load into the register can be
  done with a :samp:`moveq` instruction.  We arrange for this to happen
  by defining the letter :samp:`K` to mean 'any integer outside the
  range -128 to 127', and then specifying :samp:`Ks` in the operand
  constraints.

  .. index:: g in constraint

g
  Any register, memory or immediate integer operand is allowed, except for
  registers that are not general registers.

  .. index:: X in constraint

X

  .. only:: gccint

    Any operand whatsoever is allowed, even if it does not satisfy
    ``general_operand``.  This is normally used in the constraint of
    a ``match_scratch`` when certain alternatives will not actually
    require a scratch register.

  .. only:: not gccint

    Any operand whatsoever is allowed.

  .. index:: 0 in constraint, digits in constraint

:samp:`{0}, {1}, {2}, ... {9}`
  An operand that matches the specified operand number is allowed.  If a
  digit is used together with letters within the same alternative, the
  digit should come last.

  This number is allowed to be more than a single digit.  If multiple
  digits are encountered consecutively, they are interpreted as a single
  decimal integer.  There is scant chance for ambiguity, since to-date
  it has never been desirable that :samp:`10` be interpreted as matching
  either operand 1 *or* operand 0.  Should this be desired, one
  can use multiple alternatives instead.

  .. index:: matching constraint, constraint, matching

  This is called a :dfn:`matching constraint` and what it really means is
  that the assembler has only a single operand that fills two roles

  .. only:: gccint

    considered separate in the RTL insn.  For example, an add insn has two
    input operands and one output operand in the RTL, but on most CISC

  .. only:: not gccint

    which ``asm`` distinguishes.  For example, an add instruction uses
    two input operands and an output operand, but on most CISC

  machines an add instruction really has only two operands, one of them an
  input-output operand:

  .. code-block::

    addl #35,r12

  Matching constraints are used in these circumstances.
  More precisely, the two operands that match must include one input-only
  operand and one output-only operand.  Moreover, the digit must be a
  smaller number than the number of the operand that uses it in the
  constraint.

  .. only:: gccint

    For operands to match in a particular case usually means that they
    are identical-looking RTL expressions.  But in a few special cases
    specific kinds of dissimilarity are allowed.  For example, ``*x``
    as an input operand will match ``*x++`` as an output operand.
    For proper results in such cases, the output template should always
    use the output-operand's number when printing the operand.

  .. index:: load address instruction, push address instruction, address constraints, p in constraint

p
  An operand that is a valid memory address is allowed.  This is
  for 'load address' and 'push address' instructions.

  .. index:: address_operand

  :samp:`p` in the constraint must be accompanied by ``address_operand``
  as the predicate in the ``match_operand``.  This predicate interprets
  the mode specified in the ``match_operand`` as the mode of the memory
  reference for which the address would be valid.

  .. index:: other register constraints, extensible constraints

other-letters
  Other letters can be defined in machine-dependent fashion to stand for
  particular classes of registers or other arbitrary operand types.
  :samp:`d`, :samp:`a` and :samp:`f` are defined on the 68000/68020 to stand
  for data, address and floating point registers.

.. only:: gccint

  In order to have valid assembler code, each operand must satisfy
  its constraint.  But a failure to do so does not prevent the pattern
  from applying to an insn.  Instead, it directs the compiler to modify
  the code so that the constraint will be satisfied.  Usually this is
  done by copying an operand into a register.

  Contrast, therefore, the two instruction patterns that follow:

  .. code-block:: c++

    (define_insn ""
      [(set (match_operand:SI 0 "general_operand" "=r")
            (plus:SI (match_dup 0)
                     (match_operand:SI 1 "general_operand" "r")))]
      ""
      "...")

  which has two operands, one of which must appear in two places, and

  .. code-block:: c++

    (define_insn ""
      [(set (match_operand:SI 0 "general_operand" "=r")
            (plus:SI (match_operand:SI 1 "general_operand" "0")
                     (match_operand:SI 2 "general_operand" "r")))]
      ""
      "...")

  which has three operands, two of which are required by a constraint to be
  identical.  If we are considering an insn of the form

  .. code-block:: c++

    (insn n prev next
      (set (reg:SI 3)
           (plus:SI (reg:SI 6) (reg:SI 109)))
      ...)

  the first pattern would not apply at all, because this insn does not
  contain two identical subexpressions in the right place.  The pattern would
  say, 'That does not look like an add instruction; try other patterns'.
  The second pattern would say, 'Yes, that's an add instruction, but there
  is something wrong with it'.  It would direct the reload pass of the
  compiler to generate additional insns to make the constraint true.  The
  results might look like this:

  .. code-block:: c++

    (insn n2 prev n
      (set (reg:SI 3) (reg:SI 6))
      ...)

    (insn n n2 next
      (set (reg:SI 3)
           (plus:SI (reg:SI 3) (reg:SI 109)))
      ...)

  It is up to you to make sure that each operand, in each pattern, has
  constraints that can handle any RTL expression that could be present for
  that operand.  (When multiple alternatives are in use, each pattern must,
  for each possible combination of operand expressions, have at least one
  alternative which can handle that combination of operands.)  The
  constraints don't need to *allow* any possible operand---when this is
  the case, they do not constrain---but they must at least point the way to
  reloading any possible operand so that it will fit.

  * If the constraint accepts whatever operands the predicate permits,
    there is no problem: reloading is never necessary for this operand.

    For example, an operand whose constraints permit everything except
    registers is safe provided its predicate rejects registers.

    An operand whose predicate accepts only constant values is safe
    provided its constraints include the letter :samp:`i`.  If any possible
    constant value is accepted, then nothing less than :samp:`i` will do;
    if the predicate is more selective, then the constraints may also be
    more selective.

  * Any operand expression can be reloaded by copying it into a register.
    So if an operand's constraints allow some kind of register, it is
    certain to be safe.  It need not permit all classes of registers; the
    compiler knows how to copy a register into another register of the
    proper class in order to make an instruction valid.

    .. index:: nonoffsettable memory reference, memory reference, nonoffsettable

  * A nonoffsettable memory reference can be reloaded by copying the
    address into a register.  So if the constraint uses the letter
    :samp:`o`, all memory references are taken care of.

  * A constant operand can be reloaded by allocating space in memory to
    hold it as preinitialized data.  Then the memory reference can be used
    in place of the constant.  So if the constraint uses the letters
    :samp:`o` or :samp:`m`, constant operands are not a problem.

  * If the constraint permits a constant and a pseudo register used in an insn
    was not allocated to a hard register and is equivalent to a constant,
    the register will be replaced with the constant.  If the predicate does
    not permit a constant and the insn is re-recognized for some reason, the
    compiler will crash.  Thus the predicate must always recognize any
    objects allowed by the constraint.

  If the operand's predicate can recognize registers, but the constraint does
  not permit them, it can make the compiler crash.  When this operand happens
  to be a register, the reload pass will be stymied, because it does not know
  how to copy a register temporarily into memory.

  If the predicate accepts a unary operator, the constraint applies to the
  operand.  For example, the MIPS processor at ISA level 3 supports an
  instruction which adds two registers in ``SImode`` to produce a
  ``DImode`` result, but only if the registers are correctly sign
  extended.  This predicate for the input operands accepts a
  ``sign_extend`` of an ``SImode`` register.  Write the constraint
  to indicate the type of register that is required for the operand of the
  ``sign_extend``.

.. only:: not gccint

  So the first alternative for the 68000's logical-or could be written as
  ``"+m" (output) : "ir" (input)``.  The second could be ``"+r"
  (output): "irm" (input)``.  However, the fact that two memory locations
  cannot be used in a single instruction prevents simply using ``"+rm"
  (output) : "irm" (input)``.  Using multi-alternatives, this might be
  written as ``"+m,r" (output) : "ir,irm" (input)``.  This describes
  all the available alternatives to the compiler, allowing it to choose
  the most efficient one for the current conditions.

  There is no way within the template to determine which alternative was
  chosen.  However you may be able to wrap your ``asm`` statements with
  builtins such as ``__builtin_constant_p`` to achieve the desired results.

.. index:: multiple alternative constraints

.. _multi-alternative:

Multiple Alternative Constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sometimes a single instruction has multiple alternative sets of possible
operands.  For example, on the 68000, a logical-or instruction can combine
register or an immediate value into memory, or it can combine any kind of
operand into a register; but it cannot combine one memory location into
another.

These constraints are represented as multiple alternatives.  An alternative
can be described by a series of letters for each operand.  The overall
constraint for an operand is made from the letters for this operand
from the first alternative, a comma, the letters for this operand from
the second alternative, a comma, and so on until the last alternative.
All operands for a single instruction must have the same number of
alternatives.

.. only:: gccint

  Here is how it is done for fullword logical-or on the 68000:

  .. code-block:: c++

    (define_insn "iorsi3"
      [(set (match_operand:SI 0 "general_operand" "=m,d")
            (ior:SI (match_operand:SI 1 "general_operand" "%0,0")
                    (match_operand:SI 2 "general_operand" "dKs,dmKs")))]
      ...)

  The first alternative has :samp:`m` (memory) for operand 0, :samp:`0` for
  operand 1 (meaning it must match operand 0), and :samp:`dKs` for operand
  2.  The second alternative has :samp:`d` (data register) for operand 0,
  :samp:`0` for operand 1, and :samp:`dmKs` for operand 2.  The :samp:`=` and
  :samp:`%` in the constraints apply to all the alternatives; their
  meaning is explained in the next section (see :ref:`class-preferences`).

  If all the operands fit any one alternative, the instruction is valid.
  Otherwise, for each alternative, the compiler counts how many instructions
  must be added to copy the operands so that that alternative applies.
  The alternative requiring the least copying is chosen.  If two alternatives
  need the same amount of copying, the one that comes first is chosen.
  These choices can be altered with the :samp:`?` and :samp:`!` characters:

  .. index:: ? in constraint, question mark

  ``?``
    Disparage slightly the alternative that the :samp:`?` appears in,
    as a choice when no alternative applies exactly.  The compiler regards
    this alternative as one unit more costly for each :samp:`?` that appears
    in it.

    .. index:: ! in constraint, exclamation point

  ``!``
    Disparage severely the alternative that the :samp:`!` appears in.
    This alternative can still be used if it fits without reloading,
    but if reloading is needed, some other alternative will be used.

    .. index:: ^ in constraint, caret

  ``^``
    This constraint is analogous to :samp:`?` but it disparages slightly
    the alternative only if the operand with the :samp:`^` needs a reload.

    .. index:: $ in constraint, dollar sign

  ``$``
    This constraint is analogous to :samp:`!` but it disparages severely
    the alternative only if the operand with the :samp:`$` needs a reload.

  When an insn pattern has multiple alternatives in its constraints, often
  the appearance of the assembler code is determined mostly by which
  alternative was matched.  When this is so, the C code for writing the
  assembler code can use the variable ``which_alternative``, which is
  the ordinal number of the alternative that was actually satisfied (0 for
  the first, 1 for the second alternative, etc.).  See :ref:`output-statement`.

.. _class-preferences:

Register Class Preferences
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. only:: gccint

  .. index:: class preference constraints, register class preference constraints, voting between constraint alternatives

  The operand constraints have another function: they enable the compiler
  to decide which kind of hardware register a pseudo register is best
  allocated to.  The compiler examines the constraints that apply to the
  insns that use the pseudo register, looking for the machine-dependent
  letters such as :samp:`d` and :samp:`a` that specify classes of registers.
  The pseudo register is put in whichever class gets the most 'votes'.
  The constraint letters :samp:`g` and :samp:`r` also vote: they vote in
  favor of a general register.  The machine description says which registers
  are considered general.

  Of course, on some machines all registers are equivalent, and no register
  classes are defined.  Then none of this complexity is relevant.

.. index:: modifiers in constraints, constraint modifier characters

.. _modifiers:

Constraint Modifier Characters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are constraint modifier characters.

.. index:: = in constraint

:samp:`=`
  Means that this operand is written to by this instruction:
  the previous value is discarded and replaced by new data.

  .. index:: + in constraint

:samp:`+`
  Means that this operand is both read and written by the instruction.

  When the compiler fixes up the operands to satisfy the constraints,
  it needs to know which operands are read by the instruction and
  which are written by it.  :samp:`=` identifies an operand which is only
  written; :samp:`+` identifies an operand that is both read and written; all
  other operands are assumed to only be read.

  If you specify :samp:`=` or :samp:`+` in a constraint, you put it in the
  first character of the constraint string.

  .. index:: & in constraint, earlyclobber operand

:samp:`&`
  Means (in a particular alternative) that this operand is an
  :dfn:`earlyclobber` operand, which is written before the instruction is
  finished using the input operands.  Therefore, this operand may not lie
  in a register that is read by the instruction or as part of any memory
  address.

  :samp:`&` applies only to the alternative in which it is written.  In
  constraints with multiple alternatives, sometimes one alternative
  requires :samp:`&` while others do not.  See, for example, the
  :samp:`movdf` insn of the 68000.

  An operand which is read by the instruction can be tied to an earlyclobber
  operand if its only use as an input occurs before the early result is
  written.  Adding alternatives of this form often allows GCC to produce
  better code when only some of the read operands can be affected by the
  earlyclobber. See, for example, the :samp:`mulsi3` insn of the ARM.

  Furthermore, if the :dfn:`earlyclobber` operand is also a read/write
  operand, then that operand is written only after it's used.

  :samp:`&` does not obviate the need to write :samp:`=` or :samp:`+`.  As
  :dfn:`earlyclobber` operands are always written, a read-only
  :dfn:`earlyclobber` operand is ill-formed and will be rejected by the
  compiler.

  .. index:: % in constraint

:samp:`%`
  Declares the instruction to be commutative for this operand and the
  following operand.  This means that the compiler may interchange the
  two operands if that is the cheapest way to make all operands fit the
  constraints.  :samp:`%` applies to all alternatives and must appear as
  the first character in the constraint.  Only read-only operands can use
  :samp:`%`.

  .. only:: gccint

    This is often used in patterns for addition instructions
    that really have only two operands: the result must go in one of the
    arguments.  Here for example, is how the 68000 halfword-add
    instruction is defined:

    .. code-block:: c++

      (define_insn "addhi3"
        [(set (match_operand:HI 0 "general_operand" "=m,r")
           (plus:HI (match_operand:HI 1 "general_operand" "%0,0")
                    (match_operand:HI 2 "general_operand" "di,g")))]
        ...)

  GCC can only handle one commutative pair in an asm; if you use more,
  the compiler may fail.  Note that you need not use the modifier if
  the two alternatives are strictly identical; this would only waste
  time in the reload pass.

  .. only:: gccint

    The modifier is not operational after
    register allocation, so the result of ``define_peephole2``
    and ``define_split`` s performed after reload cannot rely on
    :samp:`%` to make the intended insn match.

    .. index:: # in constraint

  :samp:`#`
    Says that all following characters, up to the next comma, are to be
    ignored as a constraint.  They are significant only for choosing
    register preferences.

    .. index:: * in constraint

  :samp:`*`
    Says that the following character should be ignored when choosing
    register preferences.  :samp:`*` has no effect on the meaning of the
    constraint as a constraint, and no effect on reloading.  For LRA
    :samp:`*` additionally disparages slightly the alternative if the
    following character matches the operand.

    Here is an example: the 68000 has an instruction to sign-extend a
    halfword in a data register, and can also sign-extend a value by
    copying it into an address register.  While either kind of register is
    acceptable, the constraints on an address-register destination are
    less strict, so it is best if register allocation makes an address
    register its goal.  Therefore, :samp:`*` is used so that the :samp:`d`
    constraint letter (for data register) is ignored when computing
    register preferences.

    .. code-block:: c++

      (define_insn "extendhisi2"
        [(set (match_operand:SI 0 "general_operand" "=*d,a")
              (sign_extend:SI
               (match_operand:HI 1 "general_operand" "0,g")))]
        ...)

.. index:: machine specific constraints, constraints, machine specific

.. _machine-constraints:

Constraints for Particular Machines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Whenever possible, you should use the general-purpose constraint letters
in ``asm`` arguments, since they will convey meaning more readily to
people reading your code.  Failing that, use the constraint letters
that usually have very similar meanings across architectures.  The most
commonly used constraints are :samp:`m` and :samp:`r` (for memory and
general-purpose registers respectively; see :ref:`simple-constraints`), and
:samp:`I`, usually the letter indicating the most common
immediate-constant format.

Each architecture defines additional constraints.  These constraints
are used by the compiler itself for instruction generation, as well as
for ``asm`` statements; therefore, some of the constraints are not
particularly useful for ``asm``.  Here is a summary of some of the
machine-dependent constraints available on some particular machines;
it includes both constraints that are useful for ``asm`` and
constraints that aren't.  The compiler source file mentioned in the
table heading for each architecture is the definitive reference for
the meanings of that architecture's constraints.

.. Please keep this table alphabetized by target!

AArch64 family---:samp:`{config/aarch64/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``k``
  The stack pointer register (``SP``)

``w``
  Floating point register, Advanced SIMD vector register or SVE vector register

``x``
  Like ``w``, but restricted to registers 0 to 15 inclusive.

``y``
  Like ``w``, but restricted to registers 0 to 7 inclusive.

``Upl``
  One of the low eight SVE predicate registers (``P0`` to ``P7``)

``Upa``
  Any of the SVE predicate registers (``P0`` to ``P15``)

``I``
  Integer constant that is valid as an immediate operand in an ``ADD``
  instruction

``J``
  Integer constant that is valid as an immediate operand in a ``SUB``
  instruction (once negated)

``K``
  Integer constant that can be used with a 32-bit logical instruction

``L``
  Integer constant that can be used with a 64-bit logical instruction

``M``
  Integer constant that is valid as an immediate operand in a 32-bit ``MOV``
  pseudo instruction. The ``MOV`` may be assembled to one of several different
  machine instructions depending on the value

``N``
  Integer constant that is valid as an immediate operand in a 64-bit ``MOV``
  pseudo instruction

``S``
  An absolute symbolic address or a label reference

``Y``
  Floating point constant zero

``Z``
  Integer constant zero

``Ush``
  The high part (bits 12 and upwards) of the pc-relative address of a symbol
  within 4GB of the instruction

``Q``
  A memory address which uses a single base register with no offset

``Ump``
  A memory address suitable for a load/store pair instruction in SI, DI, SF and
  DF modes

AMD GCN ---:samp:`{config/gcn/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``I``
  Immediate integer in the range -16 to 64

``J``
  Immediate 16-bit signed integer

``Kf``
  Immediate constant -1

``L``
  Immediate 15-bit unsigned integer

``A``
  Immediate constant that can be inlined in an instruction encoding: integer
  -16..64, or float 0.0, +/-0.5, +/-1.0, +/-2.0,
  +/-4.0, 1.0/(2.0\*PI)

``B``
  Immediate 32-bit signed integer that can be attached to an instruction encoding

``C``
  Immediate 32-bit integer in range -16..4294967295 (i.e. 32-bit unsigned
  integer or :samp:`A` constraint)

``DA``
  Immediate 64-bit constant that can be split into two :samp:`A` constants

``DB``
  Immediate 64-bit constant that can be split into two :samp:`B` constants

``U``
  Any ``unspec``

``Y``
  Any ``symbol_ref`` or ``label_ref``

``v``
  VGPR register

``Sg``
  SGPR register

``SD``
  SGPR registers valid for instruction destinations, including VCC, M0 and EXEC

``SS``
  SGPR registers valid for instruction sources, including VCC, M0, EXEC and SCC

``Sm``
  SGPR registers valid as a source for scalar memory instructions (excludes M0
  and EXEC)

``Sv``
  SGPR registers valid as a source or destination for vector instructions
  (excludes EXEC)

``ca``
  All condition registers: SCC, VCCZ, EXECZ

``cs``
  Scalar condition register: SCC

``cV``
  Vector condition register: VCC, VCC_LO, VCC_HI

``e``
  EXEC register (EXEC_LO and EXEC_HI)

``RB``
  Memory operand with address space suitable for ``buffer_*`` instructions

``RF``
  Memory operand with address space suitable for ``flat_*`` instructions

``RS``
  Memory operand with address space suitable for ``s_*`` instructions

``RL``
  Memory operand with address space suitable for ``ds_*`` LDS instructions

``RG``
  Memory operand with address space suitable for ``ds_*`` GDS instructions

``RD``
  Memory operand with address space suitable for any ``ds_*`` instructions

``RM``
  Memory operand with address space suitable for ``global_*`` instructions

ARC ---:samp:`{config/arc/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``q``
  Registers usable in ARCompact 16-bit instructions: ``r0`` - ``r3``,
  ``r12`` - ``r15``.  This constraint can only match when the :option:`-mq`
  option is in effect.

``e``
  Registers usable as base-regs of memory addresses in ARCompact 16-bit memory
  instructions: ``r0`` - ``r3``, ``r12`` - ``r15``, ``sp``.
  This constraint can only match when the :option:`-mq`
  option is in effect.

``D``
  ARC FPX (dpfp) 64-bit registers. ``D0``, ``D1``.

``I``
  A signed 12-bit integer constant.

``Cal``
  constant for arithmetic/logical operations.  This might be any constant
  that can be put into a long immediate by the assmbler or linker without
  involving a PIC relocation.

``K``
  A 3-bit unsigned integer constant.

``L``
  A 6-bit unsigned integer constant.

``CnL``
  One's complement of a 6-bit unsigned integer constant.

``CmL``
  Two's complement of a 6-bit unsigned integer constant.

``M``
  A 5-bit unsigned integer constant.

``O``
  A 7-bit unsigned integer constant.

``P``
  A 8-bit unsigned integer constant.

``H``
  Any const_double value.

ARM family---:samp:`{config/arm/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``h``
  In Thumb state, the core registers ``r8`` - ``r15``.

``k``
  The stack pointer register.

``l``
  In Thumb State the core registers ``r0`` - ``r7``.  In ARM state this
  is an alias for the ``r`` constraint.

``t``
  VFP floating-point registers ``s0`` - ``s31``.  Used for 32 bit values.

``w``
  VFP floating-point registers ``d0`` - ``d31`` and the appropriate
  subset ``d0`` - ``d15`` based on command line options.
  Used for 64 bit values only.  Not valid for Thumb1.

``y``
  The iWMMX co-processor registers.

``z``
  The iWMMX GR registers.

``G``
  The floating-point constant 0.0

``I``
  Integer that is valid as an immediate operand in a data processing
  instruction.  That is, an integer in the range 0 to 255 rotated by a
  multiple of 2

``J``
  Integer in the range -4095 to 4095

``K``
  Integer that satisfies constraint :samp:`I` when inverted (ones complement)

``L``
  Integer that satisfies constraint :samp:`I` when negated (twos complement)

``M``
  Integer in the range 0 to 32

``Q``
  A memory reference where the exact address is in a single register
  (':samp:`m`' is preferable for ``asm`` statements)

``R``
  An item in the constant pool

``S``
  A symbol in the text segment of the current file

``Uv``
  A memory reference suitable for VFP load/store insns (reg+constant offset)

``Uy``
  A memory reference suitable for iWMMXt load/store instructions.

``Uq``
  A memory reference suitable for the ARMv4 ldrsb instruction.

AVR family---:samp:`{config/avr/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``l``
  Registers from r0 to r15

``a``
  Registers from r16 to r23

``d``
  Registers from r16 to r31

``w``
  Registers from r24 to r31.  These registers can be used in :samp:`adiw` command

``e``
  Pointer register (r26--r31)

``b``
  Base pointer register (r28--r31)

``q``
  Stack pointer register (SPH:SPL)

``t``
  Temporary register r0

``x``
  Register pair X (r27:r26)

``y``
  Register pair Y (r29:r28)

``z``
  Register pair Z (r31:r30)

``I``
  Constant greater than -1, less than 64

``J``
  Constant greater than -64, less than 1

``K``
  Constant integer 2

``L``
  Constant integer 0

``M``
  Constant that fits in 8 bits

``N``
  Constant integer -1

``O``
  Constant integer 8, 16, or 24

``P``
  Constant integer 1

``G``
  A floating point constant 0.0

``Q``
  A memory address based on Y or Z pointer with displacement.

Blackfin family---:samp:`{config/bfin/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  P register

``d``
  D register

``z``
  A call clobbered P register.

:samp:`q{n}`
  A single register.  If :samp:`{n}` is in the range 0 to 7, the corresponding D
  register.  If it is ``A``, then the register P0.

``D``
  Even-numbered D register

``W``
  Odd-numbered D register

``e``
  Accumulator register.

``A``
  Even-numbered accumulator register.

``B``
  Odd-numbered accumulator register.

``b``
  I register

``v``
  B register

``f``
  M register

``c``
  Registers used for circular buffering, i.e. I, B, or L registers.

``C``
  The CC register.

``t``
  LT0 or LT1.

``k``
  LC0 or LC1.

``u``
  LB0 or LB1.

``x``
  Any D, P, B, M, I or L register.

``y``
  Additional registers typically used only in prologues and epilogues: RETS,
  RETN, RETI, RETX, RETE, ASTAT, SEQSTAT and USP.

``w``
  Any register except accumulators or CC.

``Ksh``
  Signed 16 bit integer (in the range -32768 to 32767)

``Kuh``
  Unsigned 16 bit integer (in the range 0 to 65535)

``Ks7``
  Signed 7 bit integer (in the range -64 to 63)

``Ku7``
  Unsigned 7 bit integer (in the range 0 to 127)

``Ku5``
  Unsigned 5 bit integer (in the range 0 to 31)

``Ks4``
  Signed 4 bit integer (in the range -8 to 7)

``Ks3``
  Signed 3 bit integer (in the range -3 to 4)

``Ku3``
  Unsigned 3 bit integer (in the range 0 to 7)

:samp:`P{n}`
  Constant :samp:`{n}`, where :samp:`{n}` is a single-digit constant in the range 0 to 4.

``PA``
  An integer equal to one of the MACFLAG_XXX constants that is suitable for
  use with either accumulator.

``PB``
  An integer equal to one of the MACFLAG_XXX constants that is suitable for
  use only with accumulator A1.

``M1``
  Constant 255.

``M2``
  Constant 65535.

``J``
  An integer constant with exactly a single bit set.

``L``
  An integer constant with all bits set except exactly one.

``H``, ``Q``

  Any SYMBOL_REF.

C-SKY---:samp:`{config/csky/constraints.md}`

``a``
  The mini registers r0 - r7.

``b``
  The low registers r0 - r15.

``c``
  C register.

``y``
  HI and LO registers.

``l``
  LO register.

``h``
  HI register.

``v``
  Vector registers.

``z``
  Stack pointer register (SP).

``Q``
  A memory address which uses a base register with a short offset
  or with a index register with its scale.

``W``
  A memory address which uses a base register with a index register
  with its scale.

.. only:: gccint

  The C-SKY back end supports a large set of additional constraints
  that are only useful for instruction selection or splitting rather
  than inline asm, such as constraints representing constant integer
  ranges accepted by particular instruction encodings.
  Refer to the source code for details.

Epiphany---:samp:`{config/epiphany/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``U16``
  An unsigned 16-bit constant.

``K``
  An unsigned 5-bit constant.

``L``
  A signed 11-bit constant.

``Cm1``
  A signed 11-bit constant added to -1.
  Can only match when the :option:`-m1reg-reg` option is active.

``Cl1``
  Left-shift of -1, i.e., a bit mask with a block of leading ones, the rest
  being a block of trailing zeroes.
  Can only match when the :option:`-m1reg-reg` option is active.

``Cr1``
  Right-shift of -1, i.e., a bit mask with a trailing block of ones, the
  rest being zeroes.  Or to put it another way, one less than a power of two.
  Can only match when the :option:`-m1reg-reg` option is active.

``Cal``
  Constant for arithmetic/logical operations.
  This is like ``i``, except that for position independent code,
  no symbols / expressions needing relocations are allowed.

``Csy``
  Symbolic constant for call/jump instruction.

``Rcs``
  The register class usable in short insns.  This is a register class
  constraint, and can thus drive register allocation.
  This constraint won't match unless :option:`-mprefer-short-insn-regs` is
  in effect.

``Rsc``
  The register class of registers that can be used to hold a
  sibcall call address.  I.e., a caller-saved register.

``Rct``
  Core control register class.

``Rgs``
  The register group usable in short insns.
  This constraint does not use a register class, so that it only
  passively matches suitable registers, and doesn't drive register allocation.

.. only:: gccint

  ``Car``
    Constant suitable for the addsi3_r pattern.  This is a valid offset
    For byte, halfword, or word addressing.

``Rra``
  Matches the return address if it can be replaced with the link register.

``Rcc``
  Matches the integer condition code register.

``Sra``
  Matches the return address if it is in a stack slot.

``Cfm``
  Matches control register values to switch fp mode, which are encapsulated in
  ``UNSPEC_FP_MODE``.

FRV---:samp:`{config/frv/frv.h}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  Register in the class ``ACC_REGS`` (``acc0`` to ``acc7``).

``b``
  Register in the class ``EVEN_ACC_REGS`` (``acc0`` to ``acc7``).

``c``
  Register in the class ``CC_REGS`` (``fcc0`` to ``fcc3`` and
  ``icc0`` to ``icc3``).

``d``
  Register in the class ``GPR_REGS`` (``gr0`` to ``gr63``).

``e``
  Register in the class ``EVEN_REGS`` (``gr0`` to ``gr63``).
  Odd registers are excluded not in the class but through the use of a machine
  mode larger than 4 bytes.

``f``
  Register in the class ``FPR_REGS`` (``fr0`` to ``fr63``).

``h``
  Register in the class ``FEVEN_REGS`` (``fr0`` to ``fr63``).
  Odd registers are excluded not in the class but through the use of a machine
  mode larger than 4 bytes.

``l``
  Register in the class ``LR_REG`` (the ``lr`` register).

``q``
  Register in the class ``QUAD_REGS`` (``gr2`` to ``gr63``).
  Register numbers not divisible by 4 are excluded not in the class but through
  the use of a machine mode larger than 8 bytes.

``t``
  Register in the class ``ICC_REGS`` (``icc0`` to ``icc3``).

``u``
  Register in the class ``FCC_REGS`` (``fcc0`` to ``fcc3``).

``v``
  Register in the class ``ICR_REGS`` (``cc4`` to ``cc7``).

``w``
  Register in the class ``FCR_REGS`` (``cc0`` to ``cc3``).

``x``
  Register in the class ``QUAD_FPR_REGS`` (``fr0`` to ``fr63``).
  Register numbers not divisible by 4 are excluded not in the class but through
  the use of a machine mode larger than 8 bytes.

``z``
  Register in the class ``SPR_REGS`` (``lcr`` and ``lr``).

``A``
  Register in the class ``QUAD_ACC_REGS`` (``acc0`` to ``acc7``).

``B``
  Register in the class ``ACCG_REGS`` (``accg0`` to ``accg7``).

``C``
  Register in the class ``CR_REGS`` (``cc0`` to ``cc7``).

``G``
  Floating point constant zero

``I``
  6-bit signed integer constant

``J``
  10-bit signed integer constant

``L``
  16-bit signed integer constant

``M``
  16-bit unsigned integer constant

``N``
  12-bit signed integer constant that is negative---i.e. in the
  range of -2048 to -1

``O``
  Constant zero

``P``
  12-bit signed integer constant that is greater than zero---i.e. in the
  range of 1 to 2047.

FT32---:samp:`{config/ft32/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``A``
  An absolute address

``B``
  An offset address

``W``
  A register indirect memory operand

``e``
  An offset address.

``f``
  An offset address.

``O``
  The constant zero or one

``I``
  A 16-bit signed constant (-32768 ... 32767)

``w``
  A bitfield mask suitable for bext or bins

``x``
  An inverted bitfield mask suitable for bext or bins

``L``
  A 16-bit unsigned constant, multiple of 4 (0 ... 65532)

``S``
  A 20-bit signed constant (-524288 ... 524287)

``b``
  A constant for a bitfield width (1 ... 16)

``KA``
  A 10-bit signed constant (-512 ... 511)

Hewlett-Packard PA-RISC---:samp:`{config/pa/pa.h}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  General register 1

``f``
  Floating point register

``q``
  Shift amount register

``x``
  Floating point register (deprecated)

``y``
  Upper floating point register (32-bit), floating point register (64-bit)

``Z``
  Any register

``I``
  Signed 11-bit integer constant

``J``
  Signed 14-bit integer constant

``K``
  Integer constant that can be deposited with a ``zdepi`` instruction

``L``
  Signed 5-bit integer constant

``M``
  Integer constant 0

``N``
  Integer constant that can be loaded with a ``ldil`` instruction

``O``
  Integer constant whose value plus one is a power of 2

``P``
  Integer constant that can be used for ``and`` operations in ``depi``
  and ``extru`` instructions

``S``
  Integer constant 31

``U``
  Integer constant 63

``G``
  Floating-point constant 0.0

``A``
  A ``lo_sum`` data-linkage-table memory operand

``Q``
  A memory operand that can be used as the destination operand of an
  integer store instruction

``R``
  A scaled or unscaled indexed memory operand

``T``
  A memory operand for floating-point loads and stores

``W``
  A register indirect memory operand

Intel IA-64---:samp:`{config/ia64/ia64.h}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  General register ``r0`` to ``r3`` for ``addl`` instruction

``b``
  Branch register

``c``
  Predicate register (:samp:`c` as in 'conditional')

``d``
  Application register residing in M-unit

``e``
  Application register residing in I-unit

``f``
  Floating-point register

``m``
  Memory operand.  If used together with :samp:`<` or :samp:`>`,
  the operand can have postincrement and postdecrement which
  require printing with :samp:`%Pn` on IA-64.

``G``
  Floating-point constant 0.0 or 1.0

``I``
  14-bit signed integer constant

``J``
  22-bit signed integer constant

``K``
  8-bit signed integer constant for logical instructions

``L``
  8-bit adjusted signed integer constant for compare pseudo-ops

``M``
  6-bit unsigned integer constant for shift counts

``N``
  9-bit signed integer constant for load and store postincrements

``O``
  The constant zero

``P``
  0 or -1 for ``dep`` instruction

``Q``
  Non-volatile memory for floating-point loads and stores

``R``
  Integer constant in the range 1 to 4 for ``shladd`` instruction

``S``
  Memory operand except postincrement and postdecrement.  This is
  now roughly the same as :samp:`m` when not used together with :samp:`<`
  or :samp:`>`.

M32C---:samp:`{config/m32c/m32c.cc}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``Rsp`` ``Rfb`` ``Rsb``
  :samp:`$sp`, :samp:`$fb`, :samp:`$sb`.

``Rcr``
  Any control register, when they're 16 bits wide (nothing if control
  registers are 24 bits wide)

``Rcl``
  Any control register, when they're 24 bits wide.

``R0w`` ``R1w`` ``R2w`` ``R3w``
  $r0, $r1, $r2, $r3.

``R02``
  $r0 or $r2, or $r2r0 for 32 bit values.

``R13``
  $r1 or $r3, or $r3r1 for 32 bit values.

``Rdi``
  A register that can hold a 64 bit value.

``Rhl``
  $r0 or $r1 (registers with addressable high/low bytes)

``R23``
  $r2 or $r3

``Raa``
  Address registers

``Raw``
  Address registers when they're 16 bits wide.

``Ral``
  Address registers when they're 24 bits wide.

``Rqi``
  Registers that can hold QI values.

``Rad``
  Registers that can be used with displacements ($a0, $a1, $sb).

``Rsi``
  Registers that can hold 32 bit values.

``Rhi``
  Registers that can hold 16 bit values.

``Rhc``
  Registers chat can hold 16 bit values, including all control
  registers.

``Rra``
  $r0 through R1, plus $a0 and $a1.

``Rfl``
  The flags register.

``Rmm``
  The memory-based pseudo-registers $mem0 through $mem15.

``Rpi``
  Registers that can hold pointers (16 bit registers for r8c, m16c; 24
  bit registers for m32cm, m32c).

``Rpa``
  Matches multiple registers in a PARALLEL to form a larger register.
  Used to match function return values.

``Is3``
  -8 ... 7

``IS1``
  -128 ... 127

``IS2``
  -32768 ... 32767

``IU2``
  0 ... 65535

``In4``
  -8 ... -1 or 1 ... 8

``In5``
  -16 ... -1 or 1 ... 16

``In6``
  -32 ... -1 or 1 ... 32

``IM2``
  -65536 ... -1

``Ilb``
  An 8 bit value with exactly one bit set.

``Ilw``
  A 16 bit value with exactly one bit set.

``Sd``
  The common src/dest memory addressing modes.

``Sa``
  Memory addressed using $a0 or $a1.

``Si``
  Memory addressed with immediate addresses.

``Ss``
  Memory addressed using the stack pointer ($sp).

``Sf``
  Memory addressed using the frame base register ($fb).

``Ss``
  Memory addressed using the small base register ($sb).

``S1``
  $r1h

LoongArch---:samp:`{config/loongarch/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``f``
  A floating-point register (if available).

``k``
  A memory operand whose address is formed by a base register and
  (optionally scaled) index register.

``l``
  A signed 16-bit constant.

``m``
  A memory operand whose address is formed by a base register and offset
  that is suitable for use in instructions with the same addressing mode
  as ``st.w`` and ``ld.w``.

``I``
  A signed 12-bit constant (for arithmetic instructions).

``K``
  An unsigned 12-bit constant (for logic instructions).

``ZB``
  An address that is held in a general-purpose register.
  The offset is zero.

``ZC``
  A memory operand whose address is formed by a base register and offset
  that is suitable for use in instructions with the same addressing mode
  as ``ll.w`` and ``sc.w``.

MicroBlaze---:samp:`{config/microblaze/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``d``
  A general register (``r0`` to ``r31``).

``z``
  A status register (``rmsr``, ``$fcc1`` to ``$fcc7``).

MIPS---:samp:`{config/mips/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``d``
  A general-purpose register.  This is equivalent to ``r`` unless
  generating MIPS16 code, in which case the MIPS16 register set is used.

``f``
  A floating-point register (if available).

``h``
  Formerly the ``hi`` register.  This constraint is no longer supported.

``l``
  The ``lo`` register.  Use this register to store values that are
  no bigger than a word.

``x``
  The concatenated ``hi`` and ``lo`` registers.  Use this register
  to store doubleword values.

``c``
  A register suitable for use in an indirect jump.  This will always be
  ``$25`` for :option:`-mabicalls`.

``v``
  Register ``$3``.  Do not use this constraint in new code;
  it is retained only for compatibility with glibc.

``y``
  Equivalent to ``r`` ; retained for backwards compatibility.

``z``
  A floating-point condition code register.

``I``
  A signed 16-bit constant (for arithmetic instructions).

``J``
  Integer zero.

``K``
  An unsigned 16-bit constant (for logic instructions).

``L``
  A signed 32-bit constant in which the lower 16 bits are zero.
  Such constants can be loaded using ``lui``.

``M``
  A constant that cannot be loaded using ``lui``, ``addiu``
  or ``ori``.

``N``
  A constant in the range -65535 to -1 (inclusive).

``O``
  A signed 15-bit constant.

``P``
  A constant in the range 1 to 65535 (inclusive).

``G``
  Floating-point zero.

``R``
  An address that can be used in a non-macro load or store.

``ZC``
  A memory operand whose address is formed by a base register and offset
  that is suitable for use in instructions with the same addressing mode
  as ``ll`` and ``sc``.

``ZD``
  An address suitable for a ``prefetch`` instruction, or for any other
  instruction with the same addressing mode as ``prefetch``.

Motorola 680x0---:samp:`{config/m68k/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  Address register

``d``
  Data register

``f``
  68881 floating-point register, if available

``I``
  Integer in the range 1 to 8

``J``
  16-bit signed number

``K``
  Signed number whose magnitude is greater than 0x80

``L``
  Integer in the range -8 to -1

``M``
  Signed number whose magnitude is greater than 0x100

``N``
  Range 24 to 31, rotatert:SI 8 to 1 expressed as rotate

``O``
  16 (for rotate using swap)

``P``
  Range 8 to 15, rotatert:HI 8 to 1 expressed as rotate

``R``
  Numbers that mov3q can handle

``G``
  Floating point constant that is not a 68881 constant

``S``
  Operands that satisfy 'm' when -mpcrel is in effect

``T``
  Operands that satisfy 's' when -mpcrel is not in effect

``Q``
  Address register indirect addressing mode

``U``
  Register offset addressing

``W``
  const_call_operand

``Cs``
  symbol_ref or const

``Ci``
  const_int

``C0``
  const_int 0

``Cj``
  Range of signed numbers that don't fit in 16 bits

``Cmvq``
  Integers valid for mvq

``Capsw``
  Integers valid for a moveq followed by a swap

``Cmvz``
  Integers valid for mvz

``Cmvs``
  Integers valid for mvs

``Ap``
  push_operand

``Ac``
  Non-register operands allowed in clr

Moxie---:samp:`{config/moxie/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``A``
  An absolute address

``B``
  An offset address

``W``
  A register indirect memory operand

``I``
  A constant in the range of 0 to 255.

``N``
  A constant in the range of 0 to -255.

MSP430---:samp:`{config/msp430/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``R12``
  Register R12.

``R13``
  Register R13.

``K``
  Integer constant 1.

``L``
  Integer constant -1^20..1^19.

``M``
  Integer constant 1-4.

``Ya``
  Memory references which do not require an extended MOVX instruction.

``Yl``
  Memory reference, labels only.

``Ys``
  Memory reference, stack only.

NDS32---:samp:`{config/nds32/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``w``
  LOW register class $r0 to $r7 constraint for V3/V3M ISA.

``l``
  LOW register class $r0 to $r7.

``d``
  MIDDLE register class $r0 to $r11, $r16 to $r19.

``h``
  HIGH register class $r12 to $r14, $r20 to $r31.

``t``
  Temporary assist register $ta (i.e. $r15).

``k``
  Stack register $sp.

``Iu03``
  Unsigned immediate 3-bit value.

``In03``
  Negative immediate 3-bit value in the range of -7--0.

``Iu04``
  Unsigned immediate 4-bit value.

``Is05``
  Signed immediate 5-bit value.

``Iu05``
  Unsigned immediate 5-bit value.

``In05``
  Negative immediate 5-bit value in the range of -31--0.

``Ip05``
  Unsigned immediate 5-bit value for movpi45 instruction with range 16--47.

``Iu06``
  Unsigned immediate 6-bit value constraint for addri36.sp instruction.

``Iu08``
  Unsigned immediate 8-bit value.

``Iu09``
  Unsigned immediate 9-bit value.

``Is10``
  Signed immediate 10-bit value.

``Is11``
  Signed immediate 11-bit value.

``Is15``
  Signed immediate 15-bit value.

``Iu15``
  Unsigned immediate 15-bit value.

``Ic15``
  A constant which is not in the range of imm15u but ok for bclr instruction.

``Ie15``
  A constant which is not in the range of imm15u but ok for bset instruction.

``It15``
  A constant which is not in the range of imm15u but ok for btgl instruction.

``Ii15``
  A constant whose compliment value is in the range of imm15u
  and ok for bitci instruction.

``Is16``
  Signed immediate 16-bit value.

``Is17``
  Signed immediate 17-bit value.

``Is19``
  Signed immediate 19-bit value.

``Is20``
  Signed immediate 20-bit value.

``Ihig``
  The immediate value that can be simply set high 20-bit.

``Izeb``
  The immediate value 0xff.

``Izeh``
  The immediate value 0xffff.

``Ixls``
  The immediate value 0x01.

``Ix11``
  The immediate value 0x7ff.

``Ibms``
  The immediate value with power of 2.

``Ifex``
  The immediate value with power of 2 minus 1.

``U33``
  Memory constraint for 333 format.

``U45``
  Memory constraint for 45 format.

``U37``
  Memory constraint for 37 format.

Nios II family---:samp:`{config/nios2/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``I``
  Integer that is valid as an immediate operand in an
  instruction taking a signed 16-bit number. Range
  -32768 to 32767.

``J``
  Integer that is valid as an immediate operand in an
  instruction taking an unsigned 16-bit number. Range
  0 to 65535.

``K``
  Integer that is valid as an immediate operand in an
  instruction taking only the upper 16-bits of a
  32-bit number. Range 32-bit numbers with the lower
  16-bits being 0.

``L``
  Integer that is valid as an immediate operand for a
  shift instruction. Range 0 to 31.

``M``
  Integer that is valid as an immediate operand for
  only the value 0. Can be used in conjunction with
  the format modifier ``z`` to use ``r0``
  instead of ``0`` in the assembly output.

``N``
  Integer that is valid as an immediate operand for
  a custom instruction opcode. Range 0 to 255.

``P``
  An immediate operand for R2 andchi/andci instructions.

``S``
  Matches immediates which are addresses in the small
  data section and therefore can be added to ``gp``
  as a 16-bit immediate to re-create their 32-bit value.

``U``
  Matches constants suitable as an operand for the rdprs and
  cache instructions.

``v``
  A memory operand suitable for Nios II R2 load/store
  exclusive instructions.

``w``
  A memory operand suitable for load/store IO and cache
  instructions.

.. only:: gccint

  ``T``
    A ``const`` wrapped ``UNSPEC`` expression,
    representing a supported PIC or TLS relocation.

OpenRISC---:samp:`{config/or1k/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``I``
  Integer that is valid as an immediate operand in an
  instruction taking a signed 16-bit number. Range
  -32768 to 32767.

``K``
  Integer that is valid as an immediate operand in an
  instruction taking an unsigned 16-bit number. Range
  0 to 65535.

``M``
  Signed 16-bit constant shifted left 16 bits. (Used with ``l.movhi``)

``O``
  Zero

.. only:: gccint

  ``c``
    Register usable for sibcalls.

PDP-11---:samp:`{config/pdp11/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  Floating point registers AC0 through AC3.  These can be loaded from/to
  memory with a single instruction.

``d``
  Odd numbered general registers (R1, R3, R5).  These are used for
  16-bit multiply operations.

``D``
  A memory reference that is encoded within the opcode, but not
  auto-increment or auto-decrement.

``f``
  Any of the floating point registers (AC0 through AC5).

``G``
  Floating point constant 0.

``h``
  Floating point registers AC4 and AC5.  These cannot be loaded from/to
  memory with a single instruction.

``I``
  An integer constant that fits in 16 bits.

``J``
  An integer constant whose low order 16 bits are zero.

``K``
  An integer constant that does not meet the constraints for codes
  :samp:`I` or :samp:`J`.

``L``
  The integer constant 1.

``M``
  The integer constant -1.

``N``
  The integer constant 0.

``O``
  Integer constants 0 through 3; shifts by these
  amounts are handled as multiple single-bit shifts rather than a single
  variable-length shift.

``Q``
  A memory reference which requires an additional word (address or
  offset) after the opcode.

``R``
  A memory reference that is encoded within the opcode.

PowerPC and IBM RS6000---:samp:`{config/rs6000/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``r``
  A general purpose register (GPR), ``r0``... ``r31``.

``b``
  A base register.  Like ``r``, but ``r0`` is not allowed, so
  ``r1``... ``r31``.

``f``
  A floating point register (FPR), ``f0``... ``f31``.

``d``
  A floating point register.  This is the same as ``f`` nowadays;
  historically ``f`` was for single-precision and ``d`` was for
  double-precision floating point.

``v``
  An Altivec vector register (VR), ``v0``... ``v31``.

``wa``
  A VSX register (VSR), ``vs0``... ``vs63``.  This is either an
  FPR (``vs0``... ``vs31`` are ``f0``... ``f31``) or a VR
  (``vs32``... ``vs63`` are ``v0``... ``v31``).

  When using ``wa``, you should use the ``%x`` output modifier, so that
  the correct register number is printed.  For example:

  .. code-block:: c++

    asm ("xvadddp %x0,%x1,%x2"
         : "=wa" (v1)
         : "wa" (v2), "wa" (v3));

  You should not use ``%x`` for ``v`` operands:

  .. code-block:: c++

    asm ("xsaddqp %0,%1,%2"
         : "=v" (v1)
         : "v" (v2), "v" (v3));

.. only:: gccint

  ``h``
    A special register (``vrsave``, ``ctr``, or ``lr``).

``c``
  The count register, ``ctr``.

``l``
  The link register, ``lr``.

``x``
  Condition register field 0, ``cr0``.

``y``
  Any condition register field, ``cr0``... ``cr7``.

.. only:: gccint

  ``z``
    The carry bit, ``XER[CA]``.

  ``we``
    Like ``wa``, if :option:`-mpower9-vector` and :option:`-m64` are used;
    otherwise, ``NO_REGS``.

  ``wn``
    No register (``NO_REGS``).

  ``wr``
    Like ``r``, if :option:`-mpowerpc64` is used; otherwise, ``NO_REGS``.

  ``wx``
    Like ``d``, if :option:`-mpowerpc-gfxopt` is used; otherwise, ``NO_REGS``.

  ``wA``
    Like ``b``, if :option:`-mpowerpc64` is used; otherwise, ``NO_REGS``.

  ``wB``
    Signed 5-bit constant integer that can be loaded into an Altivec register.

  ``wE``
    Vector constant that can be loaded with the XXSPLTIB instruction.

  ``wF``
    Memory operand suitable for power8 GPR load fusion.

  ``wL``
    Int constant that is the element number mfvsrld accesses in a vector.

  ``wM``
    Match vector constant with all 1's if the XXLORC instruction is available.

  ``wO``
    Memory operand suitable for the ISA 3.0 vector d-form instructions.

  ``wQ``
    Memory operand suitable for the load/store quad instructions.

  ``wS``
    Vector constant that can be loaded with XXSPLTIB & sign extension.

  ``wY``
    A memory operand for a DS-form instruction.

  ``wZ``
    An indexed or indirect memory operand, ignoring the bottom 4 bits.

``I``
  A signed 16-bit constant.

``J``
  An unsigned 16-bit constant shifted left 16 bits (use ``L`` instead
  for ``SImode`` constants).

``K``
  An unsigned 16-bit constant.

``L``
  A signed 16-bit constant shifted left 16 bits.

.. only:: gccint

  ``M``
    An integer constant greater than 31.

  ``N``
    An exact power of 2.

  ``O``
    The integer constant zero.

  ``P``
    A constant whose negation is a signed 16-bit constant.

``eI``
  A signed 34-bit integer constant if prefixed instructions are supported.

``eQ``
  An IEEE 128-bit constant that can be loaded into a VSX register with
  the ``lxvkq`` instruction.

.. only:: gccint

  ``G``
    A floating point constant that can be loaded into a register with one
    instruction per word.

  ``H``
    A floating point constant that can be loaded into a register using
    three instructions.

``m``
  A memory operand.
  Normally, ``m`` does not allow addresses that update the base register.
  If the ``<`` or ``>`` constraint is also used, they are allowed and
  therefore on PowerPC targets in that case it is only safe
  to use ``m<>`` in an ``asm`` statement if that ``asm`` statement
  accesses the operand exactly once.  The ``asm`` statement must also
  use ``%U<opno>`` as a placeholder for the 'update' flag in the
  corresponding load or store instruction.  For example:

  .. code-block:: c++

    asm ("st%U0 %1,%0" : "=m<>" (mem) : "r" (val));

  is correct but:

  .. code-block:: c++

    asm ("st %1,%0" : "=m<>" (mem) : "r" (val));

  is not.

.. only:: gccint

  ``es``
    A 'stable' memory operand; that is, one which does not include any
    automodification of the base register.  This used to be useful when
    ``m`` allowed automodification of the base register, but as those
    are now only allowed when ``<`` or ``>`` is used, ``es`` is
    basically the same as ``m`` without ``<`` and ``>``.

``Q``
  A memory operand addressed by just a base register.

.. only:: gccint

  ``Y``
    A memory operand for a DQ-form instruction.

``Z``
  A memory operand accessed with indexed or indirect addressing.

.. only:: gccint

  ``R``
    An AIX TOC entry.

``a``
  An indexed or indirect address.

.. only:: gccint

  ``U``
    A V.4 small data reference.

  ``W``
    A vector constant that does not require memory.

  ``j``
    The zero vector constant.

PRU---:samp:`{config/pru/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``I``
  An unsigned 8-bit integer constant.

``J``
  An unsigned 16-bit integer constant.

``L``
  An unsigned 5-bit integer constant (for shift counts).

``T``
  A text segment (program memory) constant label.

``Z``
  Integer constant zero.

RL78---:samp:`{config/rl78/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``Int3``
  An integer constant in the range 1 ... 7.

``Int8``
  An integer constant in the range 0 ... 255.

``J``
  An integer constant in the range -255 ... 0

``K``
  The integer constant 1.

``L``
  The integer constant -1.

``M``
  The integer constant 0.

``N``
  The integer constant 2.

``O``
  The integer constant -2.

``P``
  An integer constant in the range 1 ... 15.

``Qbi``
  The built-in compare types--eq, ne, gtu, ltu, geu, and leu.

``Qsc``
  The synthetic compare types--gt, lt, ge, and le.

``Wab``
  A memory reference with an absolute address.

``Wbc``
  A memory reference using ``BC`` as a base register, with an optional offset.

``Wca``
  A memory reference using ``AX``, ``BC``, ``DE``, or ``HL`` for the address, for calls.

``Wcv``
  A memory reference using any 16-bit register pair for the address, for calls.

``Wd2``
  A memory reference using ``DE`` as a base register, with an optional offset.

``Wde``
  A memory reference using ``DE`` as a base register, without any offset.

``Wfr``
  Any memory reference to an address in the far address space.

``Wh1``
  A memory reference using ``HL`` as a base register, with an optional one-byte offset.

``Whb``
  A memory reference using ``HL`` as a base register, with ``B`` or ``C`` as the index register.

``Whl``
  A memory reference using ``HL`` as a base register, without any offset.

``Ws1``
  A memory reference using ``SP`` as a base register, with an optional one-byte offset.

``Y``
  Any memory reference to an address in the near address space.

``A``
  The ``AX`` register.

``B``
  The ``BC`` register.

``D``
  The ``DE`` register.

``R``
  ``A`` through ``L`` registers.

``S``
  The ``SP`` register.

``T``
  The ``HL`` register.

``Z08W``
  The 16-bit ``R8`` register.

``Z10W``
  The 16-bit ``R10`` register.

``Zint``
  The registers reserved for interrupts (``R24`` to ``R31``).

``a``
  The ``A`` register.

``b``
  The ``B`` register.

``c``
  The ``C`` register.

``d``
  The ``D`` register.

``e``
  The ``E`` register.

``h``
  The ``H`` register.

``l``
  The ``L`` register.

``v``
  The virtual registers.

``w``
  The ``PSW`` register.

``x``
  The ``X`` register.

RISC-V---:samp:`{config/riscv/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``f``
  A floating-point register (if available).

``I``
  An I-type 12-bit signed immediate.

``J``
  Integer zero.

``K``
  A 5-bit unsigned immediate for CSR access instructions.

``A``
  An address that is held in a general-purpose register.

``S``
  A constraint that matches an absolute symbolic address.

RX---:samp:`{config/rx/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``Q``
  An address which does not involve register indirect addressing or
  pre/post increment/decrement addressing.

``Symbol``
  A symbol reference.

``Int08``
  A constant in the range -256 to 255, inclusive.

``Sint08``
  A constant in the range -128 to 127, inclusive.

``Sint16``
  A constant in the range -32768 to 32767, inclusive.

``Sint24``
  A constant in the range -8388608 to 8388607, inclusive.

``Uint04``
  A constant in the range 0 to 15, inclusive.

S/390 and zSeries---:samp:`{config/s390/s390.h}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  Address register (general purpose register except r0)

``c``
  Condition code register

``d``
  Data register (arbitrary general purpose register)

``f``
  Floating-point register

``I``
  Unsigned 8-bit constant (0--255)

``J``
  Unsigned 12-bit constant (0--4095)

``K``
  Signed 16-bit constant (-32768--32767)

``L``
  Value appropriate as displacement.

  ``(0..4095)``
    for short displacement

  ``(-524288..524287)``
    for long displacement

``M``
  Constant integer with a value of 0x7fffffff.

``N``
  Multiple letter constraint followed by 4 parameter letters.

  ``0..9:``
    number of the part counting from most to least significant

  ``H,Q:``
    mode of the part

  ``D,S,H:``
    mode of the containing operand

  ``0,F:``
    value of the other parts (F---all bits set)

  The constraint matches if the specified part of a constant
  has a value different from its other parts.

``Q``
  Memory reference without index register and with short displacement.

``R``
  Memory reference with index register and short displacement.

``S``
  Memory reference without index register but with long displacement.

``T``
  Memory reference with index register and long displacement.

``U``
  Pointer with short displacement.

``W``
  Pointer with long displacement.

``Y``
  Shift count operand.

SPARC---:samp:`{config/sparc/sparc.h}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``f``
  Floating-point register on the SPARC-V8 architecture and
  lower floating-point register on the SPARC-V9 architecture.

``e``
  Floating-point register.  It is equivalent to :samp:`f` on the
  SPARC-V8 architecture and contains both lower and upper
  floating-point registers on the SPARC-V9 architecture.

``c``
  Floating-point condition code register.

``d``
  Lower floating-point register.  It is only valid on the SPARC-V9
  architecture when the Visual Instruction Set is available.

``b``
  Floating-point register.  It is only valid on the SPARC-V9 architecture
  when the Visual Instruction Set is available.

``h``
  64-bit global or out register for the SPARC-V8+ architecture.

``C``
  The constant all-ones, for floating-point.

``A``
  Signed 5-bit constant

``D``
  A vector constant

``I``
  Signed 13-bit constant

``J``
  Zero

``K``
  32-bit constant with the low 12 bits clear (a constant that can be
  loaded with the ``sethi`` instruction)

``L``
  A constant in the range supported by ``movcc`` instructions (11-bit
  signed immediate)

``M``
  A constant in the range supported by ``movrcc`` instructions (10-bit
  signed immediate)

``N``
  Same as :samp:`K`, except that it verifies that bits that are not in the
  lower 32-bit range are all zero.  Must be used instead of :samp:`K` for
  modes wider than ``SImode``

``O``
  The constant 4096

``G``
  Floating-point zero

``H``
  Signed 13-bit constant, sign-extended to 32 or 64 bits

``P``
  The constant -1

``Q``
  Floating-point constant whose integral representation can
  be moved into an integer register using a single sethi
  instruction

``R``
  Floating-point constant whose integral representation can
  be moved into an integer register using a single mov
  instruction

``S``
  Floating-point constant whose integral representation can
  be moved into an integer register using a high/lo_sum
  instruction sequence

``T``
  Memory address aligned to an 8-byte boundary

``U``
  Even register

``W``
  Memory address for :samp:`e` constraint registers

``w``
  Memory address with only a base register

``Y``
  Vector zero

TI C6X family---:samp:`{config/c6x/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  Register file A (A0--A31).

``b``
  Register file B (B0--B31).

``A``
  Predicate registers in register file A (A0--A2 on C64X and
  higher, A1 and A2 otherwise).

``B``
  Predicate registers in register file B (B0--B2).

``C``
  A call-used register in register file B (B0--B9, B16--B31).

``Da``
  Register file A, excluding predicate registers (A3--A31,
  plus A0 if not C64X or higher).

``Db``
  Register file B, excluding predicate registers (B3--B31).

``Iu4``
  Integer constant in the range 0 ... 15.

``Iu5``
  Integer constant in the range 0 ... 31.

``In5``
  Integer constant in the range -31 ... 0.

``Is5``
  Integer constant in the range -16 ... 15.

``I5x``
  Integer constant that can be the operand of an ADDA or a SUBA insn.

``IuB``
  Integer constant in the range 0 ... 65535.

``IsB``
  Integer constant in the range -32768 ... 32767.

``IsC``
  Integer constant in the range -2^{20} ... 2^{20} - 1.

``Jc``
  Integer constant that is a valid mask for the clr instruction.

``Js``
  Integer constant that is a valid mask for the set instruction.

``Q``
  Memory location with A base register.

``R``
  Memory location with B base register.

.. only:: gccint

  ``S0``
    On C64x+ targets, a GP-relative small data reference.

  ``S1``
    Any kind of ``SYMBOL_REF``, for use in a call address.

  ``Si``
    Any kind of immediate operand, unless it matches the S0 constraint.

  ``T``
    Memory location with B base register, but not using a long offset.

  ``W``
    A memory operand with an address that cannot be used in an unaligned access.

``Z``
  Register B14 (aka DP).

Visium---:samp:`{config/visium/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``b``
  EAM register ``mdb``

``c``
  EAM register ``mdc``

``f``
  Floating point register

.. only:: gccint

  ``k``
    Register for sibcall optimization

``l``
  General register, but not ``r29``, ``r30`` and ``r31``

``t``
  Register ``r1``

``u``
  Register ``r2``

``v``
  Register ``r3``

``G``
  Floating-point constant 0.0

``J``
  Integer constant in the range 0 .. 65535 (16-bit immediate)

``K``
  Integer constant in the range 1 .. 31 (5-bit immediate)

``L``
  Integer constant in the range -65535 .. -1 (16-bit negative immediate)

``M``
  Integer constant -1

``O``
  Integer constant 0

``P``
  Integer constant 32

x86 family---:samp:`{config/i386/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``R``
  Legacy register---the eight integer registers available on all
  i386 processors (``a``, ``b``, ``c``, ``d``,
  ``si``, ``di``, ``bp``, ``sp``).

``q``
  Any register accessible as ``rl``.  In 32-bit mode, ``a``,
  ``b``, ``c``, and ``d`` ; in 64-bit mode, any integer register.

``Q``
  Any register accessible as ``rh`` : ``a``, ``b``,
  ``c``, and ``d``.

.. only:: gccint

  ``l``
    Any register that can be used as the index in a base+index memory
    access: that is, any general register except the stack pointer.

``a``
  The ``a`` register.

``b``
  The ``b`` register.

``c``
  The ``c`` register.

``d``
  The ``d`` register.

``S``
  The ``si`` register.

``D``
  The ``di`` register.

``A``
  The ``a`` and ``d`` registers.  This class is used for instructions
  that return double word results in the ``ax:dx`` register pair.  Single
  word values will be allocated either in ``ax`` or ``dx``.
  For example on i386 the following implements ``rdtsc`` :

  .. code-block:: c++

    unsigned long long rdtsc (void)
    {
      unsigned long long tick;
      __asm__ __volatile__("rdtsc":"=A"(tick));
      return tick;
    }

  This is not correct on x86-64 as it would allocate tick in either ``ax``
  or ``dx``.  You have to use the following variant instead:

  .. code-block:: c++

    unsigned long long rdtsc (void)
    {
      unsigned int tickl, tickh;
      __asm__ __volatile__("rdtsc":"=a"(tickl),"=d"(tickh));
      return ((unsigned long long)tickh << 32)|tickl;
    }

``U``
  The call-clobbered integer registers.

``f``
  Any 80387 floating-point (stack) register.

``t``
  Top of 80387 floating-point stack (``%st(0)``).

``u``
  Second from top of 80387 floating-point stack (``%st(1)``).

.. only:: gccint

  ``Yk``
    Any mask register that can be used as a predicate, i.e. ``k1-k7``.

  ``k``
    Any mask register.

``y``
  Any MMX register.

``x``
  Any SSE register.

``v``
  Any EVEX encodable SSE register (``%xmm0-%xmm31``).

.. only:: gccint

  ``w``
    Any bound register.

``Yz``
  First SSE register (``%xmm0``).

.. only:: gccint

  ``Yi``
    Any SSE register, when SSE2 and inter-unit moves are enabled.

  ``Yj``
    Any SSE register, when SSE2 and inter-unit moves from vector registers are enabled.

  ``Ym``
    Any MMX register, when inter-unit moves are enabled.

  ``Yn``
    Any MMX register, when inter-unit moves from vector registers are enabled.

  ``Yp``
    Any integer register when ``TARGET_PARTIAL_REG_STALL`` is disabled.

  ``Ya``
    Any integer register when zero extensions with ``AND`` are disabled.

  ``Yb``
    Any register that can be used as the GOT base when calling

    ``___tls_get_addr`` : that is, any general register except ``a``
    and ``sp`` registers, for :option:`-fno-plt` if linker supports it.
    Otherwise, ``b`` register.

  ``Yf``
    Any x87 register when 80387 floating-point arithmetic is enabled.

  ``Yr``
    Lower SSE register when avoiding REX prefix and all SSE registers otherwise.

  ``Yv``
    For AVX512VL, any EVEX-encodable SSE register (``%xmm0-%xmm31``),
    otherwise any SSE register.

  ``Yh``
    Any EVEX-encodable SSE register, that has number factor of four.

  ``Bf``
    Flags register operand.

  ``Bg``
    GOT memory operand.

  ``Bm``
    Vector memory operand.

  ``Bc``
    Constant memory operand.

``Bn``
  Memory operand without REX prefix.

``Bs``
  Sibcall memory operand.

``Bw``
  Call memory operand.

``Bz``
  Constant call address operand.

``BC``
  SSE constant -1 operand.

``I``
  Integer constant in the range 0 ... 31, for 32-bit shifts.

``J``
  Integer constant in the range 0 ... 63, for 64-bit shifts.

``K``
  Signed 8-bit integer constant.

``L``
  ``0xFF`` or ``0xFFFF``, for andsi as a zero-extending move.

``M``
  0, 1, 2, or 3 (shifts for the ``lea`` instruction).

``N``
  Unsigned 8-bit integer constant (for ``in`` and ``out``
  instructions).

.. only:: gccint

  ``O``
    Integer constant in the range 0 ... 127, for 128-bit shifts.

``G``
  Standard 80387 floating point constant.

``C``
  SSE constant zero operand.

``e``
  32-bit signed integer constant, or a symbolic reference known
  to fit that range (for immediate operands in sign-extending x86-64
  instructions).

``We``
  32-bit signed integer constant, or a symbolic reference known
  to fit that range (for sign-extending conversion operations that
  require non- ``VOIDmode`` immediate operands).

``Wz``
  32-bit unsigned integer constant, or a symbolic reference known
  to fit that range (for zero-extending conversion operations that
  require non- ``VOIDmode`` immediate operands).

``Wd``
  128-bit integer constant where both the high and low 64-bit word
  satisfy the ``e`` constraint.

``Z``
  32-bit unsigned integer constant, or a symbolic reference known
  to fit that range (for immediate operands in zero-extending x86-64
  instructions).

``Tv``
  VSIB address operand.

``Ts``
  Address operand without segment register.

Xstormy16---:samp:`{config/stormy16/stormy16.h}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  Register r0.

``b``
  Register r1.

``c``
  Register r2.

``d``
  Register r8.

``e``
  Registers r0 through r7.

``t``
  Registers r0 and r1.

``y``
  The carry register.

``z``
  Registers r8 and r9.

``I``
  A constant between 0 and 3 inclusive.

``J``
  A constant that has exactly one bit set.

``K``
  A constant that has exactly one bit clear.

``L``
  A constant between 0 and 255 inclusive.

``M``
  A constant between -255 and 0 inclusive.

``N``
  A constant between -3 and 0 inclusive.

``O``
  A constant between 1 and 4 inclusive.

``P``
  A constant between -4 and -1 inclusive.

``Q``
  A memory reference that is a stack push.

``R``
  A memory reference that is a stack pop.

``S``
  A memory reference that refers to a constant address of known value.

``T``
  The register indicated by Rx (not implemented yet).

``U``
  A constant that is not between 2 and 15 inclusive.

``Z``
  The constant 0.

Xtensa---:samp:`{config/xtensa/constraints.md}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``a``
  General-purpose 32-bit register

``b``
  One-bit boolean register

``A``
  MAC16 40-bit accumulator register

``I``
  Signed 12-bit integer constant, for use in MOVI instructions

``J``
  Signed 8-bit integer constant, for use in ADDI instructions

``K``
  Integer constant valid for BccI instructions

``L``
  Unsigned constant valid for BccUI instructions