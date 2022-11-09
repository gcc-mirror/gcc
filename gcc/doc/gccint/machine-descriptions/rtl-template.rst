..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RTL insn template, generating insns, insns, generating, recognizing insns, insns, recognizing

.. _rtl-template:

RTL Template
************

The RTL template is used to define which insns match the particular pattern
and how to find their operands.  For named patterns, the RTL template also
says how to construct an insn from specified operands.

Construction involves substituting specified operands into a copy of the
template.  Matching involves determining the values that serve as the
operands in the insn being matched.  Both of these activities are
controlled by special expression types that direct matching and
substitution of the operands.

.. index:: match_operand

:samp:`(match_operand:{m} {n} {predicate} {constraint})`
  This expression is a placeholder for operand number :samp:`{n}` of
  the insn.  When constructing an insn, operand number :samp:`{n}`
  will be substituted at this point.  When matching an insn, whatever
  appears at this position in the insn will be taken as operand
  number :samp:`{n}` ; but it must satisfy :samp:`{predicate}` or this instruction
  pattern will not match at all.

  Operand numbers must be chosen consecutively counting from zero in
  each instruction pattern.  There may be only one ``match_operand``
  expression in the pattern for each operand number.  Usually operands
  are numbered in the order of appearance in ``match_operand``
  expressions.  In the case of a ``define_expand``, any operand numbers
  used only in ``match_dup`` expressions have higher values than all
  other operand numbers.

  :samp:`{predicate}` is a string that is the name of a function that
  accepts two arguments, an expression and a machine mode.
  See :ref:`predicates`.  During matching, the function will be called with
  the putative operand as the expression and :samp:`{m}` as the mode
  argument (if :samp:`{m}` is not specified, ``VOIDmode`` will be used,
  which normally causes :samp:`{predicate}` to accept any mode).  If it
  returns zero, this instruction pattern fails to match.
  :samp:`{predicate}` may be an empty string; then it means no test is to be
  done on the operand, so anything which occurs in this position is
  valid.

  Most of the time, :samp:`{predicate}` will reject modes other than :samp:`{m}` ---but
  not always.  For example, the predicate ``address_operand`` uses
  :samp:`{m}` as the mode of memory ref that the address should be valid for.
  Many predicates accept ``const_int`` nodes even though their mode is
  ``VOIDmode``.

  :samp:`{constraint}` controls reloading and the choice of the best register
  class to use for a value, as explained later (see :ref:`constraints`).
  If the constraint would be an empty string, it can be omitted.

  People are often unclear on the difference between the constraint and the
  predicate.  The predicate helps decide whether a given insn matches the
  pattern.  The constraint plays no role in this decision; instead, it
  controls various decisions in the case of an insn which does match.

  .. index:: match_scratch

:samp:`(match_scratch:{m} {n} {constraint})`
  This expression is also a placeholder for operand number :samp:`{n}`
  and indicates that operand must be a ``scratch`` or ``reg``
  expression.

  When matching patterns, this is equivalent to

  .. code-block::

    (match_operand:m n "scratch_operand" constraint)

  but, when generating RTL, it produces a (``scratch`` : :samp:`{m}`)
  expression.

  If the last few expressions in a ``parallel`` are ``clobber``
  expressions whose operands are either a hard register or
  ``match_scratch``, the combiner can add or delete them when
  necessary.  See :ref:`side-effects`.

  .. index:: match_dup

:samp:`(match_dup {n})`
  This expression is also a placeholder for operand number :samp:`{n}`.
  It is used when the operand needs to appear more than once in the
  insn.

  In construction, ``match_dup`` acts just like ``match_operand`` :
  the operand is substituted into the insn being constructed.  But in
  matching, ``match_dup`` behaves differently.  It assumes that operand
  number :samp:`{n}` has already been determined by a ``match_operand``
  appearing earlier in the recognition template, and it matches only an
  identical-looking expression.

  Note that ``match_dup`` should not be used to tell the compiler that
  a particular register is being used for two operands (example:
  ``add`` that adds one register to another; the second register is
  both an input operand and the output operand).  Use a matching
  constraint (see :ref:`simple-constraints`) for those.  ``match_dup`` is for the cases where one
  operand is used in two places in the template, such as an instruction
  that computes both a quotient and a remainder, where the opcode takes
  two input operands but the RTL template has to refer to each of those
  twice; once for the quotient pattern and once for the remainder pattern.

  .. index:: match_operator

:samp:`(match_operator:{m} {n} {predicate} [{operands}...])`
  This pattern is a kind of placeholder for a variable RTL expression
  code.

  When constructing an insn, it stands for an RTL expression whose
  expression code is taken from that of operand :samp:`{n}`, and whose
  operands are constructed from the patterns :samp:`{operands}`.

  When matching an expression, it matches an expression if the function
  :samp:`{predicate}` returns nonzero on that expression *and* the
  patterns :samp:`{operands}` match the operands of the expression.

  Suppose that the function ``commutative_operator`` is defined as
  follows, to match any expression whose operator is one of the
  commutative arithmetic operators of RTL and whose mode is :samp:`{mode}` :

  .. code-block::

    int
    commutative_integer_operator (x, mode)
         rtx x;
         machine_mode mode;
    {
      enum rtx_code code = GET_CODE (x);
      if (GET_MODE (x) != mode)
        return 0;
      return (GET_RTX_CLASS (code) == RTX_COMM_ARITH
              || code == EQ || code == NE);
    }

  Then the following pattern will match any RTL expression consisting
  of a commutative operator applied to two general operands:

  .. code-block::

    (match_operator:SI 3 "commutative_operator"
      [(match_operand:SI 1 "general_operand" "g")
       (match_operand:SI 2 "general_operand" "g")])

  Here the vector ``[operands...]`` contains two patterns
  because the expressions to be matched all contain two operands.

  When this pattern does match, the two operands of the commutative
  operator are recorded as operands 1 and 2 of the insn.  (This is done
  by the two instances of ``match_operand``.)  Operand 3 of the insn
  will be the entire commutative expression: use ``GET_CODE
  (operands[3])`` to see which commutative operator was used.

  The machine mode :samp:`{m}` of ``match_operator`` works like that of
  ``match_operand`` : it is passed as the second argument to the
  predicate function, and that function is solely responsible for
  deciding whether the expression to be matched 'has' that mode.

  When constructing an insn, argument 3 of the gen-function will specify
  the operation (i.e. the expression code) for the expression to be
  made.  It should be an RTL expression, whose expression code is copied
  into a new expression whose operands are arguments 1 and 2 of the
  gen-function.  The subexpressions of argument 3 are not used;
  only its expression code matters.

  When ``match_operator`` is used in a pattern for matching an insn,
  it usually best if the operand number of the ``match_operator``
  is higher than that of the actual operands of the insn.  This improves
  register allocation because the register allocator often looks at
  operands 1 and 2 of insns to see if it can do register tying.

  There is no way to specify constraints in ``match_operator``.  The
  operand of the insn which corresponds to the ``match_operator``
  never has any constraints because it is never reloaded as a whole.
  However, if parts of its :samp:`{operands}` are matched by
  ``match_operand`` patterns, those parts may have constraints of
  their own.

  .. index:: match_op_dup

:samp:`(match_op_dup:{m} {n}[{operands}...])`
  Like ``match_dup``, except that it applies to operators instead of
  operands.  When constructing an insn, operand number :samp:`{n}` will be
  substituted at this point.  But in matching, ``match_op_dup`` behaves
  differently.  It assumes that operand number :samp:`{n}` has already been
  determined by a ``match_operator`` appearing earlier in the
  recognition template, and it matches only an identical-looking
  expression.

  .. index:: match_parallel

:samp:`(match_parallel {n} {predicate} [{subpat}...])`
  This pattern is a placeholder for an insn that consists of a
  ``parallel`` expression with a variable number of elements.  This
  expression should only appear at the top level of an insn pattern.

  When constructing an insn, operand number :samp:`{n}` will be substituted at
  this point.  When matching an insn, it matches if the body of the insn
  is a ``parallel`` expression with at least as many elements as the
  vector of :samp:`{subpat}` expressions in the ``match_parallel``, if each
  :samp:`{subpat}` matches the corresponding element of the ``parallel``,
  *and* the function :samp:`{predicate}` returns nonzero on the
  ``parallel`` that is the body of the insn.  It is the responsibility
  of the predicate to validate elements of the ``parallel`` beyond
  those listed in the ``match_parallel``.

  A typical use of ``match_parallel`` is to match load and store
  multiple expressions, which can contain a variable number of elements
  in a ``parallel``.  For example,

  .. code-block::

    (define_insn ""
      [(match_parallel 0 "load_multiple_operation"
         [(set (match_operand:SI 1 "gpc_reg_operand" "=r")
               (match_operand:SI 2 "memory_operand" "m"))
          (use (reg:SI 179))
          (clobber (reg:SI 179))])]
      ""
      "loadm 0,0,%1,%2")

  This example comes from :samp:`a29k.md`.  The function
  ``load_multiple_operation`` is defined in :samp:`a29k.c` and checks
  that subsequent elements in the ``parallel`` are the same as the
  ``set`` in the pattern, except that they are referencing subsequent
  registers and memory locations.

  An insn that matches this pattern might look like:

  .. code-block::

    (parallel
     [(set (reg:SI 20) (mem:SI (reg:SI 100)))
      (use (reg:SI 179))
      (clobber (reg:SI 179))
      (set (reg:SI 21)
           (mem:SI (plus:SI (reg:SI 100)
                            (const_int 4))))
      (set (reg:SI 22)
           (mem:SI (plus:SI (reg:SI 100)
                            (const_int 8))))])

  .. index:: match_par_dup

:samp:`(match_par_dup {n} [{subpat}...])`
  Like ``match_op_dup``, but for ``match_parallel`` instead of
  ``match_operator``.
