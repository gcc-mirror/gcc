..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. Most of this node appears by itself (in a different place) even
   when the INTERNALS flag is clear.  Passages that require the internals
   manual's context are conditionalized to appear only in the internals manual.

.. index:: operand constraints, constraints

.. _constraints:

Operand Constraints
*******************

Each ``match_operand`` in an instruction pattern can specify
constraints for the operands allowed.  The constraints allow you to
fine-tune matching within the set of operands allowed by the
predicate.

Constraints can say whether
an operand may be in a register, and which kinds of register; whether the
operand can be a memory reference, and which kinds of address; whether the
operand may be an immediate constant, and which possible values it may
have.  Constraints can also require two operands to match.
Side-effects aren't allowed in operands of inline ``asm``, unless
:samp:`<` or :samp:`>` constraints are used, because there is no guarantee
that the side effects will happen exactly once in an instruction that can update
the addressing register.

.. toctree::
  :maxdepth: 2


.. include:: ../../../../doc/md.rst


.. index:: enabled

.. _disable-insn-alternatives:

Disable insn alternatives using the enabled attribute
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are three insn attributes that may be used to selectively disable
instruction alternatives:

``enabled``
  Says whether an alternative is available on the current subtarget.

``preferred_for_size``
  Says whether an enabled alternative should be used in code that is
  optimized for size.

``preferred_for_speed``
  Says whether an enabled alternative should be used in code that is
  optimized for speed.

All these attributes should use ``(const_int 1)`` to allow an alternative
or ``(const_int 0)`` to disallow it.  The attributes must be a static
property of the subtarget; they cannot for example depend on the
current operands, on the current optimization level, on the location
of the insn within the body of a loop, on whether register allocation
has finished, or on the current compiler pass.

The ``enabled`` attribute is a correctness property.  It tells GCC to act
as though the disabled alternatives were never defined in the first place.
This is useful when adding new instructions to an existing pattern in
cases where the new instructions are only available for certain cpu
architecture levels (typically mapped to the ``-march=`` command-line
option).

In contrast, the ``preferred_for_size`` and ``preferred_for_speed``
attributes are strong optimization hints rather than correctness properties.
``preferred_for_size`` tells GCC which alternatives to consider when
adding or modifying an instruction that GCC wants to optimize for size.
``preferred_for_speed`` does the same thing for speed.  Note that things
like code motion can lead to cases where code optimized for size uses
alternatives that are not preferred for size, and similarly for speed.

Although ``define_insn`` s can in principle specify the ``enabled``
attribute directly, it is often clearer to have subsiduary attributes
for each architectural feature of interest.  The ``define_insn`` s
can then use these subsiduary attributes to say which alternatives
require which features.  The example below does this for ``cpu_facility``.

E.g. the following two patterns could easily be merged using the ``enabled``
attribute:

.. code-block::

  (define_insn "*movdi_old"
    [(set (match_operand:DI 0 "register_operand" "=d")
          (match_operand:DI 1 "register_operand" " d"))]
    "!TARGET_NEW"
    "lgr %0,%1")

  (define_insn "*movdi_new"
    [(set (match_operand:DI 0 "register_operand" "=d,f,d")
          (match_operand:DI 1 "register_operand" " d,d,f"))]
    "TARGET_NEW"
    "@
     lgr  %0,%1
     ldgr %0,%1
     lgdr %0,%1")

to:

.. code-block::

  (define_insn "*movdi_combined"
    [(set (match_operand:DI 0 "register_operand" "=d,f,d")
          (match_operand:DI 1 "register_operand" " d,d,f"))]
    ""
    "@
     lgr  %0,%1
     ldgr %0,%1
     lgdr %0,%1"
    [(set_attr "cpu_facility" "*,new,new")])

with the ``enabled`` attribute defined like this:

.. code-block::

  (define_attr "cpu_facility" "standard,new" (const_string "standard"))

  (define_attr "enabled" ""
    (cond [(eq_attr "cpu_facility" "standard") (const_int 1)
           (and (eq_attr "cpu_facility" "new")
                (ne (symbol_ref "TARGET_NEW") (const_int 0)))
           (const_int 1)]
          (const_int 0)))

.. index:: defining constraints, constraints, defining

.. _define-constraints:

Defining Machine-Specific Constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Machine-specific constraints fall into two categories: register and
non-register constraints.  Within the latter category, constraints
which allow subsets of all possible memory or address operands should
be specially marked, to give ``reload`` more information.

Machine-specific constraints can be given names of arbitrary length,
but they must be entirely composed of letters, digits, underscores
(:samp:`_`), and angle brackets (:samp:`< >`).  Like C identifiers, they
must begin with a letter or underscore.

In order to avoid ambiguity in operand constraint strings, no
constraint can have a name that begins with any other constraint's
name.  For example, if ``x`` is defined as a constraint name,
``xy`` may not be, and vice versa.  As a consequence of this rule,
no constraint may begin with one of the generic constraint letters:
:samp:`E F V X g i m n o p r s`.

Register constraints correspond directly to register classes.
See :ref:`register-classes`.  There is thus not much flexibility in their
definitions.

.. index:: define_register_constraint

MD Expression define_register_constraint name regclass docstringAll three arguments are string constants.
:samp:`{name}` is the name of the constraint, as it will appear in
``match_operand`` expressions.  If :samp:`{name}` is a multi-letter
constraint its length shall be the same for all constraints starting
with the same letter.  :samp:`{regclass}` can be either the
name of the corresponding register class (see :ref:`register-classes`),
or a C expression which evaluates to the appropriate register class.
If it is an expression, it must have no side effects, and it cannot
look at the operand.  The usual use of expressions is to map some
register constraints to ``NO_REGS`` when the register class
is not available on a given subarchitecture.

:samp:`{docstring}` is a sentence documenting the meaning of the
constraint.  Docstrings are explained further below.

Non-register constraints are more like predicates: the constraint
definition gives a boolean expression which indicates whether the
constraint matches.

.. index:: define_constraint

MD Expression define_constraint name docstring expThe :samp:`{name}` and :samp:`{docstring}` arguments are the same as for
``define_register_constraint``, but note that the docstring comes
immediately after the name for these expressions.  :samp:`{exp}` is an RTL
expression, obeying the same rules as the RTL expressions in predicate
definitions.  See :ref:`defining-predicates`, for details.  If it
evaluates true, the constraint matches; if it evaluates false, it
doesn't. Constraint expressions should indicate which RTL codes they
might match, just like predicate expressions.

``match_test`` C expressions have access to the
following variables:

:samp:`{op}`
  The RTL object defining the operand.

:samp:`{mode}`
  The machine mode of :samp:`{op}`.

:samp:`{ival}`
  :samp:`INTVAL ({op})`, if :samp:`{op}` is a ``const_int``.

:samp:`{hval}`
  :samp:`CONST_DOUBLE_HIGH ({op})`, if :samp:`{op}` is an integer
  ``const_double``.

:samp:`{lval}`
  :samp:`CONST_DOUBLE_LOW ({op})`, if :samp:`{op}` is an integer
  ``const_double``.

:samp:`{rval}`
  :samp:`CONST_DOUBLE_REAL_VALUE ({op})`, if :samp:`{op}` is a floating-point
  ``const_double``.

The :samp:`{*val}` variables should only be used once another piece of the
expression has verified that :samp:`{op}` is the appropriate kind of RTL
object.

Most non-register constraints should be defined with
``define_constraint``.  The remaining two definition expressions
are only appropriate for constraints that should be handled specially
by ``reload`` if they fail to match.

.. index:: define_memory_constraint

MD Expression define_memory_constraint name docstring expUse this expression for constraints that match a subset of all memory
operands: that is, ``reload`` can make them match by converting the
operand to the form :samp:`(mem (reg {X} ))`, where :samp:`{X}` is a
base register (from the register class specified by
``BASE_REG_CLASS``, see :ref:`register-classes`).

For example, on the S/390, some instructions do not accept arbitrary
memory references, but only those that do not make use of an index
register.  The constraint letter :samp:`Q` is defined to represent a
memory address of this type.  If :samp:`Q` is defined with
``define_memory_constraint``, a :samp:`Q` constraint can handle any
memory operand, because ``reload`` knows it can simply copy the
memory address into a base register if required.  This is analogous to
the way an :samp:`o` constraint can handle any memory operand.

The syntax and semantics are otherwise identical to
``define_constraint``.

.. index:: define_special_memory_constraint

MD Expression define_special_memory_constraint name docstring expUse this expression for constraints that match a subset of all memory
operands: that is, ``reload`` cannot make them match by reloading
the address as it is described for ``define_memory_constraint`` or
such address reload is undesirable with the performance point of view.

For example, ``define_special_memory_constraint`` can be useful if
specifically aligned memory is necessary or desirable for some insn
operand.

The syntax and semantics are otherwise identical to
``define_memory_constraint``.

.. index:: define_relaxed_memory_constraint

MD Expression define_relaxed_memory_constraint name docstring expThe test expression in a ``define_memory_constraint`` can assume
that ``TARGET_LEGITIMATE_ADDRESS_P`` holds for the address inside
a ``mem`` rtx and so it does not need to test this condition itself.
In other words, a ``define_memory_constraint`` test of the form:

.. code-block:: c++

  (match_test "mem")

is enough to test whether an rtx is a ``mem`` *and* whether
its address satisfies ``TARGET_MEM_CONSTRAINT`` (which is usually
:samp:`'m'`).  Thus the conditions imposed by a ``define_memory_constraint``
always apply on top of the conditions imposed by ``TARGET_MEM_CONSTRAINT``.

However, it is sometimes useful to define memory constraints that allow
addresses beyond those accepted by ``TARGET_LEGITIMATE_ADDRESS_P``.
``define_relaxed_memory_constraint`` exists for this case.
The test expression in a ``define_relaxed_memory_constraint`` is
applied with no preconditions, so that the expression can determine
'from scratch' exactly which addresses are valid and which are not.

The syntax and semantics are otherwise identical to
``define_memory_constraint``.

.. index:: define_address_constraint

MD Expression define_address_constraint name docstring expUse this expression for constraints that match a subset of all address
operands: that is, ``reload`` can make the constraint match by
converting the operand to the form :samp:`(reg {X} )`, again
with :samp:`{X}` a base register.

Constraints defined with ``define_address_constraint`` can only be
used with the ``address_operand`` predicate, or machine-specific
predicates that work the same way.  They are treated analogously to
the generic :samp:`p` constraint.

The syntax and semantics are otherwise identical to
``define_constraint``.

For historical reasons, names beginning with the letters :samp:`G H`
are reserved for constraints that match only ``const_double`` s, and
names beginning with the letters :samp:`I J K L M N O P` are reserved
for constraints that match only ``const_int`` s.  This may change in
the future.  For the time being, constraints with these names must be
written in a stylized form, so that ``genpreds`` can tell you did
it correctly:

.. code-block::

  (define_constraint "[GHIJKLMNOP]..."
    "doc..."
    (and (match_code "const_int")  ; const_double for G/H
         condition...))            ; usually a match_test

.. the semicolons line up in the formatted manual

It is fine to use names beginning with other letters for constraints
that match ``const_double`` s or ``const_int`` s.

Each docstring in a constraint definition should be one or more complete
sentences, marked up in Texinfo format.  *They are currently unused.*
In the future they will be copied into the GCC manual, in :ref:`machine-constraints`, replacing the hand-maintained tables currently found in
that section.  Also, in the future the compiler may use this to give
more helpful diagnostics when poor choice of ``asm`` constraints
causes a reload failure.

If you put the pseudo-Texinfo directive :samp:`@internal` at the
beginning of a docstring, then (in the future) it will appear only in
the internals manual's version of the machine-specific constraint tables.
Use this for constraints that should not appear in ``asm`` statements.

.. index:: testing constraints, constraints, testing

.. _c-constraint-interface:

Testing constraints from C
^^^^^^^^^^^^^^^^^^^^^^^^^^

It is occasionally useful to test a constraint from C code rather than
implicitly via the constraint string in a ``match_operand``.  The
generated file :samp:`tm_p.h` declares a few interfaces for working
with constraints.  At present these are defined for all constraints
except ``g`` (which is equivalent to ``general_operand``).

Some valid constraint names are not valid C identifiers, so there is a
mangling scheme for referring to them from C.  Constraint names that
do not contain angle brackets or underscores are left unchanged.
Underscores are doubled, each :samp:`<` is replaced with :samp:`_l`, and
each :samp:`>` with :samp:`_g`.  Here are some examples:

.. the @c's prevent double blank lines in the printed manual.

.. list-table::

   * - **Original**
     - **Mangled** ..  c
   * - ``x``
     - ``x`` ..  c
   * - ``P42x``
     - ``P42x`` ..  c
   * - ``P4_x``
     - ``P4__x`` ..  c
   * - ``P4>x``
     - ``P4_gx`` ..  c
   * - ``P4>>``
     - ``P4_g_g`` ..  c
   * - ``P4_g>``
     - ``P4__g_g`` ..  c

Throughout this section, the variable :samp:`{c}` is either a constraint
in the abstract sense, or a constant from ``enum constraint_num`` ;
the variable :samp:`{m}` is a mangled constraint name (usually as part of
a larger identifier).

.. index:: constraint_num

Enum constraint_numFor each constraint except ``g``, there is a corresponding
enumeration constant: :samp:`CONSTRAINT_` plus the mangled name of the
constraint.  Functions that take an ``enum constraint_num`` as an
argument expect one of these constants.

.. function:: inline bool satisfies_constraint_m (rtx exp)

  For each non-register constraint :samp:`{m}` except ``g``, there is
  one of these functions; it returns ``true`` if :samp:`{exp}` satisfies the
  constraint.  These functions are only visible if :samp:`rtl.h` was included
  before :samp:`tm_p.h`.

.. function:: bool constraint_satisfied_p (rtx exp, enum constraint_num c)

  Like the ``satisfies_constraint_m`` functions, but the
  constraint to test is given as an argument, :samp:`{c}`.  If :samp:`{c}`
  specifies a register constraint, this function will always return
  ``false``.

.. function:: enum reg_class reg_class_for_constraint (enum constraint_num c)

  Returns the register class associated with :samp:`{c}`.  If :samp:`{c}` is not
  a register constraint, or those registers are not available for the
  currently selected subtarget, returns ``NO_REGS``.

Here is an example use of ``satisfies_constraint_m``.  In
peephole optimizations (see :ref:`peephole-definitions`), operand
constraint strings are ignored, so if there are relevant constraints,
they must be tested in the C condition.  In the example, the
optimization is applied if operand 2 does *not* satisfy the
:samp:`K` constraint.  (This is a simplified version of a peephole
definition from the i386 machine description.)

.. code-block::

  (define_peephole2
    [(match_scratch:SI 3 "r")
     (set (match_operand:SI 0 "register_operand" "")
          (mult:SI (match_operand:SI 1 "memory_operand" "")
                   (match_operand:SI 2 "immediate_operand" "")))]

    "!satisfies_constraint_K (operands[2])"

    [(set (match_dup 3) (match_dup 1))
     (set (match_dup 0) (mult:SI (match_dup 3) (match_dup 2)))]

    "")