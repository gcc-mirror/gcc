..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: canonicalization of instructions, insn canonicalization

.. _insn-canonicalizations:

Canonicalization of Instructions
********************************

There are often cases where multiple RTL expressions could represent an
operation performed by a single machine instruction.  This situation is
most commonly encountered with logical, branch, and multiply-accumulate
instructions.  In such cases, the compiler attempts to convert these
multiple RTL expressions into a single canonical form to reduce the
number of insn patterns required.

In addition to algebraic simplifications, following canonicalizations
are performed:

* For commutative and comparison operators, a constant is always made the
  second operand.  If a machine only supports a constant as the second
  operand, only patterns that match a constant in the second operand need
  be supplied.

* For associative operators, a sequence of operators will always chain
  to the left; for instance, only the left operand of an integer ``plus``
  can itself be a ``plus``.  ``and``, ``ior``, ``xor``,
  ``plus``, ``mult``, ``smin``, ``smax``, ``umin``, and
  ``umax`` are associative when applied to integers, and sometimes to
  floating-point.

.. index:: neg, canonicalization of, not, canonicalization of, mult, canonicalization of, plus, canonicalization of, minus, canonicalization of

* For these operators, if only one operand is a ``neg``, ``not``,
  ``mult``, ``plus``, or ``minus`` expression, it will be the
  first operand.

* In combinations of ``neg``, ``mult``, ``plus``, and
  ``minus``, the ``neg`` operations (if any) will be moved inside
  the operations as far as possible.  For instance,
  ``(neg (mult A B))`` is canonicalized as ``(mult (neg A) B)``, but
  ``(plus (mult (neg B) C) A)`` is canonicalized as
  ``(minus A (mult B C))``.

  .. index:: compare, canonicalization of

* For the ``compare`` operator, a constant is always the second operand
  if the first argument is a condition code register.

* For instructions that inherently set a condition code register, the
  ``compare`` operator is always written as the first RTL expression of
  the ``parallel`` instruction pattern.  For example,

  .. code-block::

    (define_insn ""
      [(set (reg:CCZ FLAGS_REG)
    	(compare:CCZ
    	  (plus:SI
    	    (match_operand:SI 1 "register_operand" "%r")
    	    (match_operand:SI 2 "register_operand" "r"))
    	  (const_int 0)))
       (set (match_operand:SI 0 "register_operand" "=r")
    	(plus:SI (match_dup 1) (match_dup 2)))]
      ""
      "addl %0, %1, %2")

* An operand of ``neg``, ``not``, ``mult``, ``plus``, or
  ``minus`` is made the first operand under the same conditions as
  above.

* ``(ltu (plus ab) b)`` is converted to
  ``(ltu (plus ab) a)``. Likewise with ``geu`` instead
  of ``ltu``.

* ``(minus x (const_int n))`` is converted to
  ``(plus x (const_int -n))``.

* Within address computations (i.e., inside ``mem``), a left shift is
  converted into the appropriate multiplication by a power of two.

  .. index:: ior, canonicalization of, and, canonicalization of, De Morgan's law

* De Morgan's Law is used to move bitwise negation inside a bitwise
  logical-and or logical-or operation.  If this results in only one
  operand being a ``not`` expression, it will be the first one.

  A machine that has an instruction that performs a bitwise logical-and of one
  operand with the bitwise negation of the other should specify the pattern
  for that instruction as

  .. code-block::

    (define_insn ""
      [(set (match_operand:m 0 ...)
            (and:m (not:m (match_operand:m 1 ...))
                         (match_operand:m 2 ...)))]
      "..."
      "...")

  Similarly, a pattern for a 'NAND' instruction should be written

  .. code-block::

    (define_insn ""
      [(set (match_operand:m 0 ...)
            (ior:m (not:m (match_operand:m 1 ...))
                         (not:m (match_operand:m 2 ...))))]
      "..."
      "...")

  In both cases, it is not necessary to include patterns for the many
  logically equivalent RTL expressions.

  .. index:: xor, canonicalization of

* The only possible RTL expressions involving both bitwise exclusive-or
  and bitwise negation are ``(xor:mxy)``
  and ``(not:m (xor:mxy))``.

* The sum of three items, one of which is a constant, will only appear in
  the form

  .. code-block:: c++

    (plus:m (plus:m x y) constant)

  .. index:: zero_extract, canonicalization of, sign_extract, canonicalization of

* Equality comparisons of a group of bits (usually a single bit) with zero
  will be written using ``zero_extract`` rather than the equivalent
  ``and`` or ``sign_extract`` operations.

  .. index:: mult, canonicalization of

* ``(sign_extend:m1 (mult:m2 (sign_extend:m2x)
  (sign_extend:m2y)))`` is converted to ``(mult:m1
  (sign_extend:m1x) (sign_extend:m1y))``, and likewise
  for ``zero_extend``.

* ``(sign_extend:m1 (mult:m2 (ashiftrt:m2xs) (sign_extend:m2y)))`` is converted
  to ``(mult:m1 (sign_extend:m1 (ashiftrt:m2xs)) (sign_extend:m1y))``, and likewise for
  patterns using ``zero_extend`` and ``lshiftrt``.  If the second
  operand of ``mult`` is also a shift, then that is extended also.
  This transformation is only applied when it can be proven that the
  original operation had sufficient precision to prevent overflow.

Further canonicalization rules are defined in the function
``commutative_operand_precedence`` in :samp:`gcc/rtlanal.cc`.