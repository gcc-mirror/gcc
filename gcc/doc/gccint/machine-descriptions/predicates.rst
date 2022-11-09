..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: predicates, operand predicates, operator predicates

.. _predicates:

Predicates
**********

A predicate determines whether a ``match_operand`` or
``match_operator`` expression matches, and therefore whether the
surrounding instruction pattern will be used for that combination of
operands.  GCC has a number of machine-independent predicates, and you
can define machine-specific predicates as needed.  By convention,
predicates used with ``match_operand`` have names that end in
:samp:`_operand`, and those used with ``match_operator`` have names
that end in :samp:`_operator`.

All predicates are boolean functions (in the mathematical sense) of
two arguments: the RTL expression that is being considered at that
position in the instruction pattern, and the machine mode that the
``match_operand`` or ``match_operator`` specifies.  In this
section, the first argument is called :samp:`{op}` and the second argument
:samp:`{mode}`.  Predicates can be called from C as ordinary two-argument
functions; this can be useful in output templates or other
machine-specific code.

Operand predicates can allow operands that are not actually acceptable
to the hardware, as long as the constraints give reload the ability to
fix them up (see :ref:`constraints`).  However, GCC will usually generate
better code if the predicates specify the requirements of the machine
instructions as closely as possible.  Reload cannot fix up operands
that must be constants ('immediate operands'); you must use a
predicate that allows only constants, or else enforce the requirement
in the extra condition.

.. index:: predicates and machine modes, normal predicates, special predicates

Most predicates handle their :samp:`{mode}` argument in a uniform manner.
If :samp:`{mode}` is ``VOIDmode`` (unspecified), then :samp:`{op}` can have
any mode.  If :samp:`{mode}` is anything else, then :samp:`{op}` must have the
same mode, unless :samp:`{op}` is a ``CONST_INT`` or integer
``CONST_DOUBLE``.  These RTL expressions always have
``VOIDmode``, so it would be counterproductive to check that their
mode matches.  Instead, predicates that accept ``CONST_INT`` and/or
integer ``CONST_DOUBLE`` check that the value stored in the
constant will fit in the requested mode.

Predicates with this behavior are called :dfn:`normal`.
:command:`genrecog` can optimize the instruction recognizer based on
knowledge of how normal predicates treat modes.  It can also diagnose
certain kinds of common errors in the use of normal predicates; for
instance, it is almost always an error to use a normal predicate
without specifying a mode.

Predicates that do something different with their :samp:`{mode}` argument
are called :dfn:`special`.  The generic predicates
``address_operand`` and ``pmode_register_operand`` are special
predicates.  :command:`genrecog` does not do any optimizations or
diagnosis when special predicates are used.

.. toctree::
  :maxdepth: 2


.. index:: machine-independent predicates, generic predicates

.. _machine-independent-predicates:

Machine-Independent Predicates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These are the generic predicates available to all back ends.  They are
defined in :samp:`recog.cc`.  The first category of predicates allow
only constant, or :dfn:`immediate`, operands.

.. index:: immediate_operand

Function immediate_operandThis predicate allows any sort of constant that fits in :samp:`{mode}`.
It is an appropriate choice for instructions that take operands that
must be constant.

.. index:: const_int_operand

Function const_int_operandThis predicate allows any ``CONST_INT`` expression that fits in
:samp:`{mode}`.  It is an appropriate choice for an immediate operand that
does not allow a symbol or label.

.. index:: const_double_operand

Function const_double_operandThis predicate accepts any ``CONST_DOUBLE`` expression that has
exactly :samp:`{mode}`.  If :samp:`{mode}` is ``VOIDmode``, it will also
accept ``CONST_INT``.  It is intended for immediate floating point
constants.

The second category of predicates allow only some kind of machine
register.

.. index:: register_operand

Function register_operandThis predicate allows any ``REG`` or ``SUBREG`` expression that
is valid for :samp:`{mode}`.  It is often suitable for arithmetic
instruction operands on a RISC machine.

.. index:: pmode_register_operand

Function pmode_register_operandThis is a slight variant on ``register_operand`` which works around
a limitation in the machine-description reader.

.. code-block:: c++

  (match_operand n "pmode_register_operand" constraint)

means exactly what

.. code-block:: c++

  (match_operand:P n "register_operand" constraint)

would mean, if the machine-description reader accepted :samp:`:P`
mode suffixes.  Unfortunately, it cannot, because ``Pmode`` is an
alias for some other mode, and might vary with machine-specific
options.  See :ref:`misc`.

.. index:: scratch_operand

Function scratch_operandThis predicate allows hard registers and ``SCRATCH`` expressions,
but not pseudo-registers.  It is used internally by ``match_scratch`` ;
it should not be used directly.

The third category of predicates allow only some kind of memory reference.

.. index:: memory_operand

Function memory_operandThis predicate allows any valid reference to a quantity of mode
:samp:`{mode}` in memory, as determined by the weak form of
``GO_IF_LEGITIMATE_ADDRESS`` (see :ref:`addressing-modes`).

.. index:: address_operand

Function address_operandThis predicate is a little unusual; it allows any operand that is a
valid expression for the *address* of a quantity of mode
:samp:`{mode}`, again determined by the weak form of
``GO_IF_LEGITIMATE_ADDRESS``.  To first order, if
:samp:`(mem: {mode} ( {exp ))` is acceptable to
``memory_operand``, then :samp:`{exp}` is acceptable to
``address_operand``.  Note that :samp:`{exp}` does not necessarily have
the mode :samp:`{mode}`.

.. index:: indirect_operand

Function indirect_operandThis is a stricter form of ``memory_operand`` which allows only
memory references with a ``general_operand`` as the address
expression.  New uses of this predicate are discouraged, because
``general_operand`` is very permissive, so it's hard to tell what
an ``indirect_operand`` does or does not allow.  If a target has
different requirements for memory operands for different instructions,
it is better to define target-specific predicates which enforce the
hardware's requirements explicitly.

.. index:: push_operand

Function push_operandThis predicate allows a memory reference suitable for pushing a value
onto the stack.  This will be a ``MEM`` which refers to
``stack_pointer_rtx``, with a side effect in its address expression
(see :ref:`incdec`); which one is determined by the
``STACK_PUSH_CODE`` macro (see :ref:`frame-layout`).

.. index:: pop_operand

Function pop_operandThis predicate allows a memory reference suitable for popping a value
off the stack.  Again, this will be a ``MEM`` referring to
``stack_pointer_rtx``, with a side effect in its address
expression.  However, this time ``STACK_POP_CODE`` is expected.

The fourth category of predicates allow some combination of the above
operands.

.. index:: nonmemory_operand

Function nonmemory_operandThis predicate allows any immediate or register operand valid for :samp:`{mode}`.

.. index:: nonimmediate_operand

Function nonimmediate_operandThis predicate allows any register or memory operand valid for :samp:`{mode}`.

.. index:: general_operand

Function general_operandThis predicate allows any immediate, register, or memory operand
valid for :samp:`{mode}`.

Finally, there are two generic operator predicates.

.. index:: comparison_operator

Function comparison_operatorThis predicate matches any expression which performs an arithmetic
comparison in :samp:`{mode}` ; that is, ``COMPARISON_P`` is true for the
expression code.

.. index:: ordered_comparison_operator

Function ordered_comparison_operatorThis predicate matches any expression which performs an arithmetic
comparison in :samp:`{mode}` and whose expression code is valid for integer
modes; that is, the expression code will be one of ``eq``, ``ne``,
``lt``, ``ltu``, ``le``, ``leu``, ``gt``, ``gtu``,
``ge``, ``geu``.

.. index:: defining predicates, define_predicate, define_special_predicate

.. _defining-predicates:

Defining Machine-Specific Predicates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many machines have requirements for their operands that cannot be
expressed precisely using the generic predicates.  You can define
additional predicates using ``define_predicate`` and
``define_special_predicate`` expressions.  These expressions have
three operands:

* The name of the predicate, as it will be referred to in
  ``match_operand`` or ``match_operator`` expressions.

* An RTL expression which evaluates to true if the predicate allows the
  operand :samp:`{op}`, false if it does not.  This expression can only use
  the following RTL codes:

  .. envvar:: MATCH_OPERAND

    When written inside a predicate expression, a ``MATCH_OPERAND``
    expression evaluates to true if the predicate it names would allow
    :samp:`{op}`.  The operand number and constraint are ignored.  Due to
    limitations in :command:`genrecog`, you can only refer to generic
    predicates and predicates that have already been defined.

  .. envvar:: MATCH_CODE

    This expression evaluates to true if :samp:`{op}` or a specified
    subexpression of :samp:`{op}` has one of a given list of RTX codes.

    The first operand of this expression is a string constant containing a
    comma-separated list of RTX code names (in lower case).  These are the
    codes for which the ``MATCH_CODE`` will be true.

    The second operand is a string constant which indicates what
    subexpression of :samp:`{op}` to examine.  If it is absent or the empty
    string, :samp:`{op}` itself is examined.  Otherwise, the string constant
    must be a sequence of digits and/or lowercase letters.  Each character
    indicates a subexpression to extract from the current expression; for
    the first character this is :samp:`{op}`, for the second and subsequent
    characters it is the result of the previous character.  A digit
    :samp:`{n}` extracts :samp:`XEXP ({e}, {n})`; a letter :samp:`{l}`
    extracts :samp:`XVECEXP ({e}, 0, {n})` where :samp:`{n}` is the
    alphabetic ordinal of :samp:`{l}` (0 for 'a', 1 for 'b', and so on).  The
    ``MATCH_CODE`` then examines the RTX code of the subexpression
    extracted by the complete string.  It is not possible to extract
    components of an ``rtvec`` that is not at position 0 within its RTX
    object.

  .. envvar:: MATCH_TEST

    This expression has one operand, a string constant containing a C
    expression.  The predicate's arguments, :samp:`{op}` and :samp:`{mode}`, are
    available with those names in the C expression.  The ``MATCH_TEST``
    evaluates to true if the C expression evaluates to a nonzero value.
    ``MATCH_TEST`` expressions must not have side effects.

  ``AND`` ``IOR`` ``NOT`` ``IF_THEN_ELSE``
    The basic :samp:`MATCH_` expressions can be combined using these
    logical operators, which have the semantics of the C operators
    :samp:`&&`, :samp:`||`, :samp:`!`, and :samp:`? :` respectively.  As
    in Common Lisp, you may give an ``AND`` or ``IOR`` expression an
    arbitrary number of arguments; this has exactly the same effect as
    writing a chain of two-argument ``AND`` or ``IOR`` expressions.

* An optional block of C code, which should execute
  :samp:`return true` if the predicate is found to match and
  :samp:`return false` if it does not.  It must not have any side
  effects.  The predicate arguments, :samp:`{op}` and :samp:`{mode}`, are
  available with those names.

  If a code block is present in a predicate definition, then the RTL
  expression must evaluate to true *and* the code block must
  execute :samp:`return true` for the predicate to allow the operand.
  The RTL expression is evaluated first; do not re-check anything in the
  code block that was checked in the RTL expression.

The program :command:`genrecog` scans ``define_predicate`` and
``define_special_predicate`` expressions to determine which RTX
codes are possibly allowed.  You should always make this explicit in
the RTL predicate expression, using ``MATCH_OPERAND`` and
``MATCH_CODE``.

Here is an example of a simple predicate definition, from the IA64
machine description:

.. code-block:: c++

  ;; True if op is a SYMBOL_REF which refers to the sdata section.
  (define_predicate "small_addr_symbolic_operand"
    (and (match_code "symbol_ref")
         (match_test "SYMBOL_REF_SMALL_ADDR_P (op)")))

And here is another, showing the use of the C block.

.. code-block:: c++

  ;; True if op is a register operand that is (or could be) a GR reg.
  (define_predicate "gr_register_operand"
    (match_operand 0 "register_operand")
  {
    unsigned int regno;
    if (GET_CODE (op) == SUBREG)
      op = SUBREG_REG (op);

    regno = REGNO (op);
    return (regno >= FIRST_PSEUDO_REGISTER || GENERAL_REGNO_P (regno));
  })

Predicates written with ``define_predicate`` automatically include
a test that :samp:`{mode}` is ``VOIDmode``, or :samp:`{op}` has the same
mode as :samp:`{mode}`, or :samp:`{op}` is a ``CONST_INT`` or
``CONST_DOUBLE``.  They do *not* check specifically for
integer ``CONST_DOUBLE``, nor do they test that the value of either
kind of constant fits in the requested mode.  This is because
target-specific predicates that take constants usually have to do more
stringent value checks anyway.  If you need the exact same treatment
of ``CONST_INT`` or ``CONST_DOUBLE`` that the generic predicates
provide, use a ``MATCH_OPERAND`` subexpression to call
``const_int_operand``, ``const_double_operand``, or
``immediate_operand``.

Predicates written with ``define_special_predicate`` do not get any
automatic mode checks, and are treated as having special mode handling
by :command:`genrecog`.

The program :command:`genpreds` is responsible for generating code to
test predicates.  It also writes a header file containing function
declarations for all machine-specific predicates.  It is not necessary
to declare these predicates in :samp:`{cpu}-protos.h`.
