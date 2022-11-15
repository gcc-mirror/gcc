..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: insns

.. _insns:

Insns
*****

The RTL representation of the code for a function is a doubly-linked
chain of objects called :dfn:`insns`.  Insns are expressions with
special codes that are used for no other purpose.  Some insns are
actual instructions; others represent dispatch tables for ``switch``
statements; others represent labels to jump to or various sorts of
declarative information.

In addition to its own specific data, each insn must have a unique
id-number that distinguishes it from all other insns in the current
function (after delayed branch scheduling, copies of an insn with the
same id-number may be present in multiple places in a function, but
these copies will always be identical and will only appear inside a
``sequence``), and chain pointers to the preceding and following
insns.  These three fields occupy the same position in every insn,
independent of the expression code of the insn.  They could be accessed
with ``XEXP`` and ``XINT``, but instead three special macros are
always used:

.. index:: INSN_UID

:samp:`INSN_UID ({i})`
  Accesses the unique id of insn :samp:`{i}`.

  .. index:: PREV_INSN

:samp:`PREV_INSN ({i})`
  Accesses the chain pointer to the insn preceding :samp:`{i}`.
  If :samp:`{i}` is the first insn, this is a null pointer.

  .. index:: NEXT_INSN

:samp:`NEXT_INSN ({i})`
  Accesses the chain pointer to the insn following :samp:`{i}`.
  If :samp:`{i}` is the last insn, this is a null pointer.

.. index:: get_insns, get_last_insn

The first insn in the chain is obtained by calling ``get_insns`` ; the
last insn is the result of calling ``get_last_insn``.  Within the
chain delimited by these insns, the ``NEXT_INSN`` and
``PREV_INSN`` pointers must always correspond: if :samp:`{insn}` is not
the first insn,

.. code-block:: c++

  NEXT_INSN (PREV_INSN (insn)) == insn

is always true and if :samp:`{insn}` is not the last insn,

.. code-block:: c++

  PREV_INSN (NEXT_INSN (insn)) == insn

is always true.

After delay slot scheduling, some of the insns in the chain might be
``sequence`` expressions, which contain a vector of insns.  The value
of ``NEXT_INSN`` in all but the last of these insns is the next insn
in the vector; the value of ``NEXT_INSN`` of the last insn in the vector
is the same as the value of ``NEXT_INSN`` for the ``sequence`` in
which it is contained.  Similar rules apply for ``PREV_INSN``.

This means that the above invariants are not necessarily true for insns
inside ``sequence`` expressions.  Specifically, if :samp:`{insn}` is the
first insn in a ``sequence``, ``NEXT_INSN (PREV_INSN (insn))``
is the insn containing the ``sequence`` expression, as is the value
of ``PREV_INSN (NEXT_INSN (insn))`` if :samp:`{insn}` is the last
insn in the ``sequence`` expression.  You can use these expressions
to find the containing ``sequence`` expression.

Every insn has one of the following expression codes:

.. index:: insn

``insn``
  The expression code ``insn`` is used for instructions that do not jump
  and do not do function calls.  ``sequence`` expressions are always
  contained in insns with code ``insn`` even if one of those insns
  should jump or do function calls.

  Insns with code ``insn`` have four additional fields beyond the three
  mandatory ones listed above.  These four are described in a table below.

  .. index:: jump_insn

``jump_insn``
  The expression code ``jump_insn`` is used for instructions that may
  jump (or, more generally, may contain ``label_ref`` expressions to
  which ``pc`` can be set in that instruction).  If there is an
  instruction to return from the current function, it is recorded as a
  ``jump_insn``.

  .. index:: JUMP_LABEL

  ``jump_insn`` insns have the same extra fields as ``insn`` insns,
  accessed in the same way and in addition contain a field
  ``JUMP_LABEL`` which is defined once jump optimization has completed.

  For simple conditional and unconditional jumps, this field contains
  the ``code_label`` to which this insn will (possibly conditionally)
  branch.  In a more complex jump, ``JUMP_LABEL`` records one of the
  labels that the insn refers to; other jump target labels are recorded
  as ``REG_LABEL_TARGET`` notes.  The exception is ``addr_vec``
  and ``addr_diff_vec``, where ``JUMP_LABEL`` is ``NULL_RTX``
  and the only way to find the labels is to scan the entire body of the
  insn.

  Return insns count as jumps, but their ``JUMP_LABEL`` is ``RETURN``
  or ``SIMPLE_RETURN``.

  .. index:: call_insn

``call_insn``
  The expression code ``call_insn`` is used for instructions that may do
  function calls.  It is important to distinguish these instructions because
  they imply that certain registers and memory locations may be altered
  unpredictably.

  .. index:: CALL_INSN_FUNCTION_USAGE

  ``call_insn`` insns have the same extra fields as ``insn`` insns,
  accessed in the same way and in addition contain a field
  ``CALL_INSN_FUNCTION_USAGE``, which contains a list (chain of
  ``expr_list`` expressions) containing ``use``, ``clobber`` and
  sometimes ``set`` expressions that denote hard registers and
  ``mem`` s used or clobbered by the called function.

  A ``mem`` generally points to a stack slot in which arguments passed
  to the libcall by reference (see :ref:`register-arguments`) are stored.  If the argument is
  caller-copied (see :ref:`register-arguments`),
  the stack slot will be mentioned in ``clobber`` and ``use``
  entries; if it's callee-copied, only a ``use`` will appear, and the
  ``mem`` may point to addresses that are not stack slots.

  Registers occurring inside a ``clobber`` in this list augment
  registers specified in ``CALL_USED_REGISTERS`` (see :ref:`register-basics`).

  If the list contains a ``set`` involving two registers, it indicates
  that the function returns one of its arguments.  Such a ``set`` may
  look like a no-op if the same register holds the argument and the return
  value.

  .. index:: code_label, CODE_LABEL_NUMBER

``code_label``
  A ``code_label`` insn represents a label that a jump insn can jump
  to.  It contains two special fields of data in addition to the three
  standard ones.  ``CODE_LABEL_NUMBER`` is used to hold the :dfn:`label
  number`, a number that identifies this label uniquely among all the
  labels in the compilation (not just in the current function).
  Ultimately, the label is represented in the assembler output as an
  assembler label, usually of the form :samp:`L{n}` where :samp:`{n}` is
  the label number.

  When a ``code_label`` appears in an RTL expression, it normally
  appears within a ``label_ref`` which represents the address of
  the label, as a number.

  Besides as a ``code_label``, a label can also be represented as a
  ``note`` of type ``NOTE_INSN_DELETED_LABEL``.

  .. index:: LABEL_NUSES

  The field ``LABEL_NUSES`` is only defined once the jump optimization
  phase is completed.  It contains the number of times this label is
  referenced in the current function.

  .. index:: LABEL_KIND, SET_LABEL_KIND, LABEL_ALT_ENTRY_P, alternate entry points

  The field ``LABEL_KIND`` differentiates four different types of
  labels: ``LABEL_NORMAL``, ``LABEL_STATIC_ENTRY``,
  ``LABEL_GLOBAL_ENTRY``, and ``LABEL_WEAK_ENTRY``.  The only labels
  that do not have type ``LABEL_NORMAL`` are :dfn:`alternate entry
  points` to the current function.  These may be static (visible only in
  the containing translation unit), global (exposed to all translation
  units), or weak (global, but can be overridden by another symbol with the
  same name).

  Much of the compiler treats all four kinds of label identically.  Some
  of it needs to know whether or not a label is an alternate entry point;
  for this purpose, the macro ``LABEL_ALT_ENTRY_P`` is provided.  It is
  equivalent to testing whether :samp:`LABEL_KIND (label) == LABEL_NORMAL`.
  The only place that cares about the distinction between static, global,
  and weak alternate entry points, besides the front-end code that creates
  them, is the function ``output_alternate_entry_point``, in
  :samp:`final.cc`.

  To set the kind of a label, use the ``SET_LABEL_KIND`` macro.

  .. index:: jump_table_data

``jump_table_data``
  A ``jump_table_data`` insn is a placeholder for the jump-table data
  of a ``casesi`` or ``tablejump`` insn.  They are placed after
  a ``tablejump_p`` insn.  A ``jump_table_data`` insn is not part o
  a basic blockm but it is associated with the basic block that ends with
  the ``tablejump_p`` insn.  The ``PATTERN`` of a ``jump_table_data``
  is always either an ``addr_vec`` or an ``addr_diff_vec``, and a
  ``jump_table_data`` insn is always preceded by a ``code_label``.
  The ``tablejump_p`` insn refers to that ``code_label`` via its
  ``JUMP_LABEL``.

  .. index:: barrier

``barrier``
  Barriers are placed in the instruction stream when control cannot flow
  past them.  They are placed after unconditional jump instructions to
  indicate that the jumps are unconditional and after calls to
  ``volatile`` functions, which do not return (e.g., ``exit``).
  They contain no information beyond the three standard fields.

  .. index:: note, NOTE_LINE_NUMBER, NOTE_SOURCE_FILE

``note``
  ``note`` insns are used to represent additional debugging and
  declarative information.  They contain two nonstandard fields, an
  integer which is accessed with the macro ``NOTE_LINE_NUMBER`` and a
  string accessed with ``NOTE_SOURCE_FILE``.

  If ``NOTE_LINE_NUMBER`` is positive, the note represents the
  position of a source line and ``NOTE_SOURCE_FILE`` is the source file name
  that the line came from.  These notes control generation of line
  number data in the assembler output.

  Otherwise, ``NOTE_LINE_NUMBER`` is not really a line number but a
  code with one of the following values (and ``NOTE_SOURCE_FILE``
  must contain a null pointer):

  .. index:: NOTE_INSN_DELETED

  .. envvar:: NOTE_INSN_DELETED

    Such a note is completely ignorable.  Some passes of the compiler
    delete insns by altering them into notes of this kind.

  .. envvar:: NOTE_INSN_DELETED_LABEL

    This marks what used to be a ``code_label``, but was not used for other
    purposes than taking its address and was transformed to mark that no
    code jumps to it.

  .. envvar:: NOTE_INSN_BLOCK_BEG

    These types of notes indicate the position of the beginning and end
    of a level of scoping of variable names.  They control the output
    of debugging information.

  .. envvar:: NOTE_INSN_EH_REGION_BEG

    These types of notes indicate the position of the beginning and end of a
    level of scoping for exception handling.  ``NOTE_EH_HANDLER``
    identifies which region is associated with these notes.

  .. envvar:: NOTE_INSN_FUNCTION_BEG

    Appears at the start of the function body, after the function
    prologue.

  .. envvar:: NOTE_INSN_VAR_LOCATION

    This note is used to generate variable location debugging information.
    It indicates that the user variable in its ``VAR_LOCATION`` operand
    is at the location given in the RTL expression, or holds a value that
    can be computed by evaluating the RTL expression from that static
    point in the program up to the next such note for the same user
    variable.

  .. envvar:: NOTE_INSN_BEGIN_STMT

    This note is used to generate ``is_stmt`` markers in line number
    debugging information.  It indicates the beginning of a user
    statement.

  .. envvar:: NOTE_INSN_INLINE_ENTRY

    This note is used to generate ``entry_pc`` for inlined subroutines in
    debugging information.  It indicates an inspection point at which all
    arguments for the inlined function have been bound, and before its first
    statement.

  These codes are printed symbolically when they appear in debugging dumps.

  .. index:: debug_insn, INSN_VAR_LOCATION

``debug_insn``
  The expression code ``debug_insn`` is used for pseudo-instructions
  that hold debugging information for variable tracking at assignments
  (see :option:`-fvar-tracking-assignments` option).  They are the RTL
  representation of ``GIMPLE_DEBUG`` statements
  (:ref:`GIMPLE_DEBUG`), with a ``VAR_LOCATION`` operand that
  binds a user variable tree to an RTL representation of the
  ``value`` in the corresponding statement.  A ``DEBUG_EXPR`` in
  it stands for the value bound to the corresponding
  ``DEBUG_EXPR_DECL``.

  ``GIMPLE_DEBUG_BEGIN_STMT`` and ``GIMPLE_DEBUG_INLINE_ENTRY`` are
  expanded to RTL as a ``DEBUG_INSN`` with a ``DEBUG_MARKER``
  ``PATTERN`` ; the difference is the RTL mode: the former's
  ``DEBUG_MARKER`` is ``VOIDmode``, whereas the latter is
  ``BLKmode`` ; information about the inlined function can be taken from
  the lexical block encoded in the ``INSN_LOCATION``.  These
  ``DEBUG_INSN`` s, that do not carry ``VAR_LOCATION`` information,
  just ``DEBUG_MARKER`` s, can be detected by testing
  ``DEBUG_MARKER_INSN_P``, whereas those that do can be recognized as
  ``DEBUG_BIND_INSN_P``.

  Throughout optimization passes, ``DEBUG_INSN`` s are not reordered
  with respect to each other, particularly during scheduling.  Binding
  information is kept in pseudo-instruction form, so that, unlike notes,
  it gets the same treatment and adjustments that regular instructions
  would.  It is the variable tracking pass that turns these
  pseudo-instructions into ``NOTE_INSN_VAR_LOCATION``,
  ``NOTE_INSN_BEGIN_STMT`` and ``NOTE_INSN_INLINE_ENTRY`` notes,
  analyzing control flow, value equivalences and changes to registers and
  memory referenced in value expressions, propagating the values of debug
  temporaries and determining expressions that can be used to compute the
  value of each user variable at as many points (ranges, actually) in the
  program as possible.

  Unlike ``NOTE_INSN_VAR_LOCATION``, the value expression in an
  ``INSN_VAR_LOCATION`` denotes a value at that specific point in the
  program, rather than an expression that can be evaluated at any later
  point before an overriding ``VAR_LOCATION`` is encountered.  E.g.,
  if a user variable is bound to a ``REG`` and then a subsequent insn
  modifies the ``REG``, the note location would keep mapping the user
  variable to the register across the insn, whereas the insn location
  would keep the variable bound to the value, so that the variable
  tracking pass would emit another location note for the variable at the
  point in which the register is modified.

.. index:: TImode, in insn, HImode, in insn, QImode, in insn

The machine mode of an insn is normally ``VOIDmode``, but some
phases use the mode for various purposes.

The common subexpression elimination pass sets the mode of an insn to
``QImode`` when it is the first insn in a block that has already
been processed.

The second Haifa scheduling pass, for targets that can multiple issue,
sets the mode of an insn to ``TImode`` when it is believed that the
instruction begins an issue group.  That is, when the instruction
cannot issue simultaneously with the previous.  This may be relied on
by later passes, in particular machine-dependent reorg.

Here is a table of the extra fields of ``insn``, ``jump_insn``
and ``call_insn`` insns:

.. index:: PATTERN

:samp:`PATTERN ({i})`
  An expression for the side effect performed by this insn.  This must
  be one of the following codes: ``set``, ``call``, ``use``,
  ``clobber``, ``return``, ``simple_return``, ``asm_input``,
  ``asm_output``, ``addr_vec``, ``addr_diff_vec``,
  ``trap_if``, ``unspec``, ``unspec_volatile``,
  ``parallel``, ``cond_exec``, or ``sequence``.  If it is a
  ``parallel``, each element of the ``parallel`` must be one these
  codes, except that ``parallel`` expressions cannot be nested and
  ``addr_vec`` and ``addr_diff_vec`` are not permitted inside a
  ``parallel`` expression.

  .. index:: INSN_CODE

:samp:`INSN_CODE ({i})`
  An integer that says which pattern in the machine description matches
  this insn, or -1 if the matching has not yet been attempted.

  Such matching is never attempted and this field remains -1 on an insn
  whose pattern consists of a single ``use``, ``clobber``,
  ``asm_input``, ``addr_vec`` or ``addr_diff_vec`` expression.

  .. index:: asm_noperands

  Matching is also never attempted on insns that result from an ``asm``
  statement.  These contain at least one ``asm_operands`` expression.
  The function ``asm_noperands`` returns a non-negative value for
  such insns.

  In the debugging output, this field is printed as a number followed by
  a symbolic representation that locates the pattern in the :samp:`md`
  file as some small positive or negative offset from a named pattern.

  .. index:: REG_NOTES

:samp:`REG_NOTES ({i})`
  A list (chain of ``expr_list``, ``insn_list`` and ``int_list``
  expressions) giving miscellaneous information about the insn.  It is often
  information pertaining to the registers used in this insn.

The ``REG_NOTES`` field of an insn is a chain that includes
``expr_list`` and ``int_list`` expressions as well as ``insn_list``
expressions.  There are several
kinds of register notes, which are distinguished by the machine mode, which
in a register note is really understood as being an ``enum reg_note``.
The first operand :samp:`{op}` of the note is data whose meaning depends on
the kind of note.

.. index:: REG_NOTE_KIND, PUT_REG_NOTE_KIND

The macro ``REG_NOTE_KIND (x)`` returns the kind of
register note.  Its counterpart, the macro ``PUT_REG_NOTE_KIND
(x, newkind)`` sets the register note type of :samp:`{x}` to be
:samp:`{newkind}`.

Register notes are of three classes: They may say something about an
input to an insn, they may say something about an output of an insn, or
they may create a linkage between two insns.

These register notes annotate inputs to an insn:

.. index:: REG_DEAD

.. envvar:: REG_DEAD

  The value in :samp:`{op}` dies in this insn; that is to say, altering the
  value immediately after this insn would not affect the future behavior
  of the program.

  It does not follow that the register :samp:`{op}` has no useful value after
  this insn since :samp:`{op}` is not necessarily modified by this insn.
  Rather, no subsequent instruction uses the contents of :samp:`{op}`.

.. envvar:: REG_UNUSED

  The register :samp:`{op}` being set by this insn will not be used in a
  subsequent insn.  This differs from a ``REG_DEAD`` note, which
  indicates that the value in an input will not be used subsequently.
  These two notes are independent; both may be present for the same
  register.

.. envvar:: REG_INC

  The register :samp:`{op}` is incremented (or decremented; at this level
  there is no distinction) by an embedded side effect inside this insn.
  This means it appears in a ``post_inc``, ``pre_inc``,
  ``post_dec`` or ``pre_dec`` expression.

.. envvar:: REG_NONNEG

  The register :samp:`{op}` is known to have a nonnegative value when this
  insn is reached.  This is used by special looping instructions
  that terminate when the register goes negative.

  The ``REG_NONNEG`` note is added only to :samp:`doloop_end`
  insns, if its pattern uses a ``ge`` condition.

.. envvar:: REG_LABEL_OPERAND

  This insn uses :samp:`{op}`, a ``code_label`` or a ``note`` of type
  ``NOTE_INSN_DELETED_LABEL``, but is not a ``jump_insn``, or it
  is a ``jump_insn`` that refers to the operand as an ordinary
  operand.  The label may still eventually be a jump target, but if so
  in an indirect jump in a subsequent insn.  The presence of this note
  allows jump optimization to be aware that :samp:`{op}` is, in fact, being
  used, and flow optimization to build an accurate flow graph.

.. envvar:: REG_LABEL_TARGET

  This insn is a ``jump_insn`` but not an ``addr_vec`` or
  ``addr_diff_vec``.  It uses :samp:`{op}`, a ``code_label`` as a
  direct or indirect jump target.  Its purpose is similar to that of
  ``REG_LABEL_OPERAND``.  This note is only present if the insn has
  multiple targets; the last label in the insn (in the highest numbered
  insn-field) goes into the ``JUMP_LABEL`` field and does not have a
  ``REG_LABEL_TARGET`` note.  See :ref:`insns`.

.. envvar:: REG_SETJMP

  Appears attached to each ``CALL_INSN`` to ``setjmp`` or a
  related function.

The following notes describe attributes of outputs of an insn:

.. index:: REG_EQUIV, REG_EQUAL

.. envvar:: REG_EQUIV

  This note is only valid on an insn that sets only one register and
  indicates that that register will be equal to :samp:`{op}` at run time; the
  scope of this equivalence differs between the two types of notes.  The
  value which the insn explicitly copies into the register may look
  different from :samp:`{op}`, but they will be equal at run time.  If the
  output of the single ``set`` is a ``strict_low_part`` or
  ``zero_extract`` expression, the note refers to the register that
  is contained in its first operand.

  For ``REG_EQUIV``, the register is equivalent to :samp:`{op}` throughout
  the entire function, and could validly be replaced in all its
  occurrences by :samp:`{op}`.  ('Validly' here refers to the data flow of
  the program; simple replacement may make some insns invalid.)  For
  example, when a constant is loaded into a register that is never
  assigned any other value, this kind of note is used.

  When a parameter is copied into a pseudo-register at entry to a function,
  a note of this kind records that the register is equivalent to the stack
  slot where the parameter was passed.  Although in this case the register
  may be set by other insns, it is still valid to replace the register
  by the stack slot throughout the function.

  A ``REG_EQUIV`` note is also used on an instruction which copies a
  register parameter into a pseudo-register at entry to a function, if
  there is a stack slot where that parameter could be stored.  Although
  other insns may set the pseudo-register, it is valid for the compiler to
  replace the pseudo-register by stack slot throughout the function,
  provided the compiler ensures that the stack slot is properly
  initialized by making the replacement in the initial copy instruction as
  well.  This is used on machines for which the calling convention
  allocates stack space for register parameters.  See
  ``REG_PARM_STACK_SPACE`` in :ref:`stack-arguments`.

  In the case of ``REG_EQUAL``, the register that is set by this insn
  will be equal to :samp:`{op}` at run time at the end of this insn but not
  necessarily elsewhere in the function.  In this case, :samp:`{op}`
  is typically an arithmetic expression.  For example, when a sequence of
  insns such as a library call is used to perform an arithmetic operation,
  this kind of note is attached to the insn that produces or copies the
  final value.

  These two notes are used in different ways by the compiler passes.
  ``REG_EQUAL`` is used by passes prior to register allocation (such as
  common subexpression elimination and loop optimization) to tell them how
  to think of that value.  ``REG_EQUIV`` notes are used by register
  allocation to indicate that there is an available substitute expression
  (either a constant or a ``mem`` expression for the location of a
  parameter on the stack) that may be used in place of a register if
  insufficient registers are available.

  Except for stack homes for parameters, which are indicated by a
  ``REG_EQUIV`` note and are not useful to the early optimization
  passes and pseudo registers that are equivalent to a memory location
  throughout their entire life, which is not detected until later in
  the compilation, all equivalences are initially indicated by an attached
  ``REG_EQUAL`` note.  In the early stages of register allocation, a
  ``REG_EQUAL`` note is changed into a ``REG_EQUIV`` note if
  :samp:`{op}` is a constant and the insn represents the only set of its
  destination register.

  Thus, compiler passes prior to register allocation need only check for
  ``REG_EQUAL`` notes and passes subsequent to register allocation
  need only check for ``REG_EQUIV`` notes.

These notes describe linkages between insns.  They occur in pairs: one
insn has one of a pair of notes that points to a second insn, which has
the inverse note pointing back to the first insn.

.. index:: REG_DEP_TRUE

.. envvar:: REG_DEP_TRUE

  This indicates a true dependence (a read after write dependence).

.. envvar:: REG_DEP_OUTPUT

  This indicates an output dependence (a write after write dependence).

.. envvar:: REG_DEP_ANTI

  This indicates an anti dependence (a write after read dependence).

These notes describe information gathered from gcov profile data.  They
are stored in the ``REG_NOTES`` field of an insn.

.. index:: REG_BR_PROB

.. envvar:: REG_BR_PROB

  This is used to specify the ratio of branches to non-branches of a
  branch insn according to the profile data.  The note is represented
  as an ``int_list`` expression whose integer value is an encoding
  of ``profile_probability`` type.  ``profile_probability`` provide
  member function ``from_reg_br_prob_note`` and ``to_reg_br_prob_note``
  to extract and store the probability into the RTL encoding.

.. envvar:: REG_BR_PRED

  These notes are found in JUMP insns after delayed branch scheduling
  has taken place.  They indicate both the direction and the likelihood
  of the JUMP.  The format is a bitmask of ATTR_FLAG\_\* values.

.. envvar:: REG_FRAME_RELATED_EXPR

  This is used on an RTX_FRAME_RELATED_P insn wherein the attached expression
  is used in place of the actual insn pattern.  This is done in cases where
  the pattern is either complex or misleading.

The note ``REG_CALL_NOCF_CHECK`` is used in conjunction with the
:option:`-fcf-protection=branch` option.  The note is set if a
``nocf_check`` attribute is specified for a function type or a
pointer to function type.  The note is stored in the ``REG_NOTES``
field of an insn.

.. index:: REG_CALL_NOCF_CHECK

.. envvar:: REG_CALL_NOCF_CHECK

  Users have control through the ``nocf_check`` attribute to identify
  which calls to a function should be skipped from control-flow instrumentation
  when the option :option:`-fcf-protection=branch` is specified.  The compiler
  puts a ``REG_CALL_NOCF_CHECK`` note on each ``CALL_INSN`` instruction
  that has a function type marked with a ``nocf_check`` attribute.

For convenience, the machine mode in an ``insn_list`` or
``expr_list`` is printed using these symbolic codes in debugging dumps.

.. index:: insn_list, expr_list

The only difference between the expression codes ``insn_list`` and
``expr_list`` is that the first operand of an ``insn_list`` is
assumed to be an insn and is printed in debugging dumps as the insn's
unique id; the first operand of an ``expr_list`` is printed in the
ordinary way as an expression.
