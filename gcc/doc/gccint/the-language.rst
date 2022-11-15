..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: The Language

.. _the-language:

The Language
************

The language in which to write expression simplifications resembles
other domain-specific languages GCC uses.  Thus it is lispy.  Let's
start with an example from the match.pd file:

.. code-block::

  (simplify
    (bit_and @0 integer_all_onesp)
    @0)

This example contains all required parts of an expression simplification.
A simplification is wrapped inside a ``(simplify ...)`` expression.
That contains at least two operands - an expression that is matched
with the GIMPLE or GENERIC IL and a replacement expression that is
returned if the match was successful.

Expressions have an operator ID, ``bit_and`` in this case.  Expressions can
be lower-case tree codes with ``_expr`` stripped off or builtin
function code names in all-caps, like ``BUILT_IN_SQRT``.

``@n`` denotes a so-called capture.  It captures the operand and lets
you refer to it in other places of the match-and-simplify.  In the
above example it is referred to in the replacement expression.  Captures
are ``@`` followed by a number or an identifier.

.. code-block::

  (simplify
    (bit_xor @0 @0)
    { build_zero_cst (type); })

In this example ``@0`` is mentioned twice which constrains the matched
expression to have two equal operands.  Usually matches are constrained
to equal types.  If operands may be constants and conversions are involved,
matching by value might be preferred in which case use ``@@0`` to
denote a by-value match and the specific operand you want to refer to
in the result part.  This example also introduces
operands written in C code.  These can be used in the expression
replacements and are supposed to evaluate to a tree node which has to
be a valid GIMPLE operand (so you cannot generate expressions in C code).

.. code-block::

  (simplify
    (trunc_mod integer_zerop@0 @1)
    (if (!integer_zerop (@1))
     @0))

Here ``@0`` captures the first operand of the trunc_mod expression
which is also predicated with ``integer_zerop``.  Expression operands
may be either expressions, predicates or captures.  Captures
can be unconstrained or capture expressions or predicates.

This example introduces an optional operand of simplify,
the if-expression.  This condition is evaluated after the
expression matched in the IL and is required to evaluate to true
to enable the replacement expression in the second operand
position.  The expression operand of the ``if`` is a standard C
expression which may contain references to captures.  The ``if``
has an optional third operand which may contain the replacement
expression that is enabled when the condition evaluates to false.

A ``if`` expression can be used to specify a common condition
for multiple simplify patterns, avoiding the need
to repeat that multiple times:

.. code-block::

  (if (!TYPE_SATURATING (type)
       && !FLOAT_TYPE_P (type) && !FIXED_POINT_TYPE_P (type))
    (simplify
      (minus (plus @0 @1) @0)
      @1)
    (simplify
      (minus (minus @0 @1) @0)
      (negate @1)))

Note that ``if`` s in outer position do not have the optional
else clause but instead have multiple then clauses.

Ifs can be nested.

There exists a ``switch`` expression which can be used to
chain conditions avoiding nesting ``if`` s too much:

.. code-block::

  (simplify
   (simple_comparison @0 REAL_CST@1)
   (switch
    /* a CMP (-0) -> a CMP 0  */
    (if (REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (@1)))
     (cmp @0 { build_real (TREE_TYPE (@1), dconst0); }))
    /* x != NaN is always true, other ops are always false.  */
    (if (REAL_VALUE_ISNAN (TREE_REAL_CST (@1))
         && ! HONOR_SNANS (@1))
     { constant_boolean_node (cmp == NE_EXPR, type); })))

Is equal to

.. code-block::

  (simplify
   (simple_comparison @0 REAL_CST@1)
   (switch
    /* a CMP (-0) -> a CMP 0  */
    (if (REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (@1)))
     (cmp @0 { build_real (TREE_TYPE (@1), dconst0); })
     /* x != NaN is always true, other ops are always false.  */
     (if (REAL_VALUE_ISNAN (TREE_REAL_CST (@1))
          && ! HONOR_SNANS (@1))
      { constant_boolean_node (cmp == NE_EXPR, type); }))))

which has the second ``if`` in the else operand of the first.
The ``switch`` expression takes ``if`` expressions as
operands (which may not have else clauses) and as a last operand
a replacement expression which should be enabled by default if
no other condition evaluated to true.

Captures can also be used for capturing results of sub-expressions.

.. code-block::

  #if GIMPLE
  (simplify
    (pointer_plus (addr@2 @0) INTEGER_CST_P@1)
    (if (is_gimple_min_invariant (@2)))
    {
      poly_int64 off;
      tree base = get_addr_base_and_unit_offset (@0, &off);
      off += tree_to_uhwi (@1);
      /* Now with that we should be able to simply write
         (addr (mem_ref (addr @base) (plus @off @1)))  */
      build1 (ADDR_EXPR, type,
              build2 (MEM_REF, TREE_TYPE (TREE_TYPE (@2)),
                      build_fold_addr_expr (base),
                      build_int_cst (ptr_type_node, off)));
    })
  #endif

In the above example, ``@2`` captures the result of the expression
``(addr @0)``.  For the outermost expression only its type can be
captured, and the keyword ``type`` is reserved for this purpose.  The
above example also gives a way to conditionalize patterns to only apply
to ``GIMPLE`` or ``GENERIC`` by means of using the pre-defined
preprocessor macros ``GIMPLE`` and ``GENERIC`` and using
preprocessor directives.

.. code-block::

  (simplify
    (bit_and:c integral_op_p@0 (bit_ior:c (bit_not @0) @1))
    (bit_and @1 @0))

Here we introduce flags on match expressions.  The flag used
above, ``c``, denotes that the expression should
be also matched commutated.  Thus the above match expression
is really the following four match expressions:

.. code-block::

    (bit_and integral_op_p@0 (bit_ior (bit_not @0) @1))
    (bit_and (bit_ior (bit_not @0) @1) integral_op_p@0)
    (bit_and integral_op_p@0 (bit_ior @1 (bit_not @0)))
    (bit_and (bit_ior @1 (bit_not @0)) integral_op_p@0)

Usual canonicalizations you know from GENERIC expressions are
applied before matching, so for example constant operands always
come second in commutative expressions.

The second supported flag is ``s`` which tells the code
generator to fail the pattern if the expression marked with
``s`` does have more than one use and the simplification
results in an expression with more than one operator.
For example in

.. code-block::

  (simplify
    (pointer_plus (pointer_plus:s @0 @1) @3)
    (pointer_plus @0 (plus @1 @3)))

this avoids the association if ``(pointer_plus @0 @1)`` is
used outside of the matched expression and thus it would stay
live and not trivially removed by dead code elimination.
Now consider ``((x + 3) + -3)`` with the temporary
holding ``(x + 3)`` used elsewhere.  This simplifies down
to ``x`` which is desirable and thus flagging with ``s``
does not prevent the transform.  Now consider ``((x + 3) + 1)``
which simplifies to ``(x + 4)``.  Despite being flagged with
``s`` the simplification will be performed.  The
simplification of ``((x + a) + 1)`` to ``(x + (a + 1))`` will
not performed in this case though.

More features exist to avoid too much repetition.

.. code-block::

  (for op (plus pointer_plus minus bit_ior bit_xor)
    (simplify
      (op @0 integer_zerop)
      @0))

A ``for`` expression can be used to repeat a pattern for each
operator specified, substituting ``op``.  ``for`` can be
nested and a ``for`` can have multiple operators to iterate.

.. code-block::

  (for opa (plus minus)
       opb (minus plus)
    (for opc (plus minus)
      (simplify...

In this example the pattern will be repeated four times with
``opa, opb, opc`` being ``plus, minus, plus`` ;
``plus, minus, minus`` ; ``minus, plus, plus`` ;
``minus, plus, minus``.

To avoid repeating operator lists in ``for`` you can name
them via

.. code-block:: c++

  (define_operator_list pmm plus minus mult)

and use them in ``for`` operator lists where they get expanded.

.. code-block:: c++

  (for opa (pmm trunc_div)
   (simplify...

So this example iterates over ``plus``, ``minus``, ``mult``
and ``trunc_div``.

Using operator lists can also remove the need to explicitly write
a ``for``.  All operator list uses that appear in a ``simplify``
or ``match`` pattern in operator positions will implicitly
be added to a new ``for``.  For example

.. code-block::

  (define_operator_list SQRT BUILT_IN_SQRTF BUILT_IN_SQRT BUILT_IN_SQRTL)
  (define_operator_list POW BUILT_IN_POWF BUILT_IN_POW BUILT_IN_POWL)
  (simplify
   (SQRT (POW @0 @1))
   (POW (abs @0) (mult @1 { built_real (TREE_TYPE (@1), dconsthalf); })))

is the same as

.. code-block::

  (for SQRT (BUILT_IN_SQRTF BUILT_IN_SQRT BUILT_IN_SQRTL)
       POW (BUILT_IN_POWF BUILT_IN_POW BUILT_IN_POWL)
   (simplify
    (SQRT (POW @0 @1))
    (POW (abs @0) (mult @1 { built_real (TREE_TYPE (@1), dconsthalf); }))))

``for`` s and operator lists can include the special identifier
``null`` that matches nothing and can never be generated.  This can
be used to pad an operator list so that it has a standard form,
even if there isn't a suitable operator for every form.

Another building block are ``with`` expressions in the
result expression which nest the generated code in a new C block
followed by its argument:

.. code-block::

  (simplify
   (convert (mult @0 @1))
   (with { tree utype = unsigned_type_for (type); }
    (convert (mult (convert:utype @0) (convert:utype @1)))))

This allows code nested in the ``with`` to refer to the declared
variables.  In the above case we use the feature to specify the
type of a generated expression with the ``:type`` syntax where
``type`` needs to be an identifier that refers to the desired type.
Usually the types of the generated result expressions are
determined from the context, but sometimes like in the above case
it is required that you specify them explicitly.

Another modifier for generated expressions is ``!`` which
tells the machinery to only consider the simplification in case
the marked expression simplified to a simple operand.  Consider
for example

.. code-block::

  (simplify
    (plus (vec_cond:s @0 @1 @2) @3)
    (vec_cond @0 (plus! @1 @3) (plus! @2 @3)))

which moves the outer ``plus`` operation to the inner arms
of the ``vec_cond`` expression but only if the actual plus
operations both simplify.  Note that on ``GENERIC`` a simple
operand means that the result satisfies ``!EXPR_P`` which
can be limiting if the operation itself simplifies but the
remaining operand is an (unrelated) expression.

As intermediate conversions are often optional there is a way to
avoid the need to repeat patterns both with and without such
conversions.  Namely you can mark a conversion as being optional
with a ``?`` :

.. code-block::

  (simplify
   (eq (convert@0 @1) (convert? @2))
   (eq @1 (convert @2)))

which will match both ``(eq (convert @1) (convert @2))`` and
``(eq (convert @1) @2)``.  The optional converts are supposed
to be all either present or not, thus
``(eq (convert? @1) (convert? @2))`` will result in two
patterns only.  If you want to match all four combinations you
have access to two additional conditional converts as in
``(eq (convert1? @1) (convert2? @2))``.

The support for ``?`` marking extends to all unary operations
including predicates you declare yourself with ``match``.

Predicates available from the GCC middle-end need to be made
available explicitly via ``define_predicates`` :

.. code-block::

  (define_predicates
   integer_onep integer_zerop integer_all_onesp)

You can also define predicates using the pattern matching language
and the ``match`` form:

.. code-block::

  (match negate_expr_p
   INTEGER_CST
   (if (TYPE_OVERFLOW_WRAPS (type)
        || may_negate_without_overflow_p (t))))
  (match negate_expr_p
   (negate @0))

This shows that for ``match`` expressions there is ``t``
available which captures the outermost expression (something
not possible in the ``simplify`` context).  As you can see
``match`` has an identifier as first operand which is how
you refer to the predicate in patterns.  Multiple ``match``
for the same identifier add additional cases where the predicate
matches.

Predicates can also match an expression in which case you need
to provide a template specifying the identifier and where to
get its operands from:

.. code-block::

  (match (logical_inverted_value @0)
   (eq @0 integer_zerop))
  (match (logical_inverted_value @0)
   (bit_not truth_valued_p@0))

You can use the above predicate like

.. code-block::

  (simplify
   (bit_and @0 (logical_inverted_value @0))
   { build_zero_cst (type); })

Which will match a bitwise and of an operand with its logical
inverted value.
