..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: expression, TREE_TYPE, TREE_OPERAND

.. _expression-trees:

Expressions
***********

The internal representation for expressions is for the most part quite
straightforward.  However, there are a few facts that one must bear in
mind.  In particular, the expression 'tree' is actually a directed
acyclic graph.  (For example there may be many references to the integer
constant zero throughout the source program; many of these will be
represented by the same expression node.)  You should not rely on
certain kinds of node being shared, nor should you rely on certain kinds of
nodes being unshared.

The following macros can be used with all expression nodes:

.. envvar:: TREE_TYPE

  Returns the type of the expression.  This value may not be precisely the
  same type that would be given the expression in the original program.

In what follows, some nodes that one might expect to always have type
``bool`` are documented to have either integral or boolean type.  At
some point in the future, the C front end may also make use of this same
intermediate representation, and at this point these nodes will
certainly have integral type.  The previous sentence is not meant to
imply that the C++ front end does not or will not give these nodes
integral type.

Below, we list the various kinds of expression nodes.  Except where
noted otherwise, the operands to an expression are accessed using the
``TREE_OPERAND`` macro.  For example, to access the first operand to
a binary plus expression ``expr``, use:

.. code-block:: c++

  TREE_OPERAND (expr, 0)

As this example indicates, the operands are zero-indexed.

.. toctree::
  :maxdepth: 2


.. _constant-expressions:

Constant expressions
^^^^^^^^^^^^^^^^^^^^

.. index:: INTEGER_CST, tree_int_cst_lt, tree_int_cst_equal, tree_fits_uhwi_p, tree_fits_shwi_p, tree_to_uhwi, tree_to_shwi, TREE_INT_CST_NUNITS, TREE_INT_CST_ELT, TREE_INT_CST_LOW, REAL_CST, FIXED_CST, COMPLEX_CST, VECTOR_CST, STRING_CST, POLY_INT_CST, TREE_STRING_LENGTH, TREE_STRING_POINTER

The table below begins with constants, moves on to unary expressions,
then proceeds to binary expressions, and concludes with various other
kinds of expressions:

.. envvar:: INTEGER_CST

  These nodes represent integer constants.  Note that the type of these
  constants is obtained with ``TREE_TYPE`` ; they are not always of type
  ``int``.  In particular, ``char`` constants are represented with
  ``INTEGER_CST`` nodes.  The value of the integer constant ``e`` is
  represented in an array of HOST_WIDE_INT.   There are enough elements
  in the array to represent the value without taking extra elements for
  redundant 0s or -1.  The number of elements used to represent ``e``
  is available via ``TREE_INT_CST_NUNITS``. Element ``i`` can be
  extracted by using ``TREE_INT_CST_ELT (e, i)``.
  ``TREE_INT_CST_LOW`` is a shorthand for ``TREE_INT_CST_ELT (e, 0)``.

  The functions ``tree_fits_shwi_p`` and ``tree_fits_uhwi_p``
  can be used to tell if the value is small enough to fit in a
  signed HOST_WIDE_INT or an unsigned HOST_WIDE_INT respectively.
  The value can then be extracted using ``tree_to_shwi`` and
  ``tree_to_uhwi``.

.. envvar:: REAL_CST

  .. todo:: Talk about how to obtain representations of this constant, do
    comparisons, and so forth.

.. envvar:: FIXED_CST

  These nodes represent fixed-point constants.  The type of these constants
  is obtained with ``TREE_TYPE``.  ``TREE_FIXED_CST_PTR`` points to
  a ``struct fixed_value`` ;  ``TREE_FIXED_CST`` returns the structure
  itself.  ``struct fixed_value`` contains ``data`` with the size of two
  ``HOST_BITS_PER_WIDE_INT`` and ``mode`` as the associated fixed-point
  machine mode for ``data``.

.. envvar:: COMPLEX_CST

  These nodes are used to represent complex number constants, that is a
  ``__complex__`` whose parts are constant nodes.  The
  ``TREE_REALPART`` and ``TREE_IMAGPART`` return the real and the
  imaginary parts respectively.

.. envvar:: VECTOR_CST

  These nodes are used to represent vector constants.  Each vector
  constant :samp:`{v}` is treated as a specific instance of an arbitrary-length
  sequence that itself contains :samp:`VECTOR_CST_NPATTERNS ({v})`
  interleaved patterns.  Each pattern has the form:

  .. code-block:: c++

    { base0, base1, base1 + step, base1 + step * 2, ... }

  The first three elements in each pattern are enough to determine the
  values of the other elements.  However, if all :samp:`{step}` s are zero,
  only the first two elements are needed.  If in addition each :samp:`{base1}`
  is equal to the corresponding :samp:`{base0}`, only the first element in
  each pattern is needed.  The number of encoded elements per pattern
  is given by :samp:`VECTOR_CST_NELTS_PER_PATTERN ({v})`.

  For example, the constant:

  .. code-block:: c++

    { 0, 1, 2, 6, 3, 8, 4, 10, 5, 12, 6, 14, 7, 16, 8, 18 }

  is interpreted as an interleaving of the sequences:

  .. code-block:: c++

    { 0, 2, 3, 4, 5, 6, 7, 8 }
    { 1, 6, 8, 10, 12, 14, 16, 18 }

  where the sequences are represented by the following patterns:

  .. code-block:: c++

    base0 == 0, base1 == 2, step == 1
    base0 == 1, base1 == 6, step == 2

  In this case:

  .. code-block:: c++

    VECTOR_CST_NPATTERNS (v) == 2
    VECTOR_CST_NELTS_PER_PATTERN (v) == 3

  The vector is therefore encoded using the first 6 elements
  (:samp:`{ 0, 1, 2, 6, 3, 8 }`), with the remaining 10 elements
  being implicit extensions of them.

  Sometimes this scheme can create two possible encodings of the same
  vector.  For example { 0, 1 } could be seen as two patterns with
  one element each or one pattern with two elements (:samp:`{base0}` and
  :samp:`{base1}`).  The canonical encoding is always the one with the
  fewest patterns or (if both encodings have the same number of
  petterns) the one with the fewest encoded elements.

  :samp:`vector_cst_encoding_nelts ({v})` gives the total number of
  encoded elements in :samp:`{v}`, which is 6 in the example above.
  ``VECTOR_CST_ENCODED_ELTS (v)`` gives a pointer to the elements
  encoded in :samp:`{v}` and ``VECTOR_CST_ENCODED_ELT (v, i)``
  accesses the value of encoded element :samp:`{i}`.

  :samp:`VECTOR_CST_DUPLICATE_P ({v})` is true if :samp:`{v}` simply contains
  repeated instances of :samp:`VECTOR_CST_NPATTERNS ({v})` values.  This is
  a shorthand for testing :samp:`VECTOR_CST_NELTS_PER_PATTERN ({v}) == 1`.

  :samp:`VECTOR_CST_STEPPED_P ({v})` is true if at least one
  pattern in :samp:`{v}` has a nonzero step.  This is a shorthand for
  testing :samp:`VECTOR_CST_NELTS_PER_PATTERN ({v}) == 3`.

  The utility function ``vector_cst_elt`` gives the value of an
  arbitrary index as a ``tree``.  ``vector_cst_int_elt`` gives
  the same value as a ``wide_int``.

.. envvar:: STRING_CST

  These nodes represent string-constants.  The ``TREE_STRING_LENGTH``
  returns the length of the string, as an ``int``.  The
  ``TREE_STRING_POINTER`` is a ``char*`` containing the string
  itself.  The string may not be ``NUL`` -terminated, and it may contain
  embedded ``NUL`` characters.  Therefore, the
  ``TREE_STRING_LENGTH`` includes the trailing ``NUL`` if it is
  present.

  For wide string constants, the ``TREE_STRING_LENGTH`` is the number
  of bytes in the string, and the ``TREE_STRING_POINTER``
  points to an array of the bytes of the string, as represented on the
  target system (that is, as integers in the target endianness).  Wide and
  non-wide string constants are distinguished only by the ``TREE_TYPE``
  of the ``STRING_CST``.

  .. todo:: The formats of string constants are not well-defined when the
    target system bytes are not the same width as host system bytes.

.. envvar:: POLY_INT_CST

  These nodes represent invariants that depend on some target-specific
  runtime parameters.  They consist of ``NUM_POLY_INT_COEFFS``
  coefficients, with the first coefficient being the constant term and
  the others being multipliers that are applied to the runtime parameters.

  ``POLY_INT_CST_ELT (x, i)`` references coefficient number
  :samp:`{i}` of ``POLY_INT_CST`` node :samp:`{x}`.  Each coefficient is an
  ``INTEGER_CST``.

.. _storage-references:

References to storage
^^^^^^^^^^^^^^^^^^^^^

.. index:: ADDR_EXPR, INDIRECT_REF, MEM_REF, ARRAY_REF, ARRAY_RANGE_REF, TARGET_MEM_REF, COMPONENT_REF

.. envvar:: ARRAY_REF

  These nodes represent array accesses.  The first operand is the array;
  the second is the index.  To calculate the address of the memory
  accessed, you must scale the index by the size of the type of the array
  elements.  The type of these expressions must be the type of a component of
  the array.  The third and fourth operands are used after gimplification
  to represent the lower bound and component size but should not be used
  directly; call ``array_ref_low_bound`` and ``array_ref_element_size``
  instead.

.. envvar:: ARRAY_RANGE_REF

  These nodes represent access to a range (or 'slice') of an array.  The
  operands are the same as that for ``ARRAY_REF`` and have the same
  meanings.  The type of these expressions must be an array whose component
  type is the same as that of the first operand.  The range of that array
  type determines the amount of data these expressions access.

.. envvar:: COMPONENT_REF

  These nodes represent non-static data member accesses.  The first
  operand is the object (rather than a pointer to it); the second operand
  is the ``FIELD_DECL`` for the data member.  The third operand represents
  the byte offset of the field, but should not be used directly; call
  ``component_ref_field_offset`` instead.

.. envvar:: ADDR_EXPR

  These nodes are used to represent the address of an object.  (These
  expressions will always have pointer or reference type.)  The operand may
  be another expression, or it may be a declaration.

  As an extension, GCC allows users to take the address of a label.  In
  this case, the operand of the ``ADDR_EXPR`` will be a
  ``LABEL_DECL``.  The type of such an expression is ``void*``.

  If the object addressed is not an lvalue, a temporary is created, and
  the address of the temporary is used.

.. envvar:: INDIRECT_REF

  These nodes are used to represent the object pointed to by a pointer.
  The operand is the pointer being dereferenced; it will always have
  pointer or reference type.

.. envvar:: MEM_REF

  These nodes are used to represent the object pointed to by a pointer
  offset by a constant.
  The first operand is the pointer being dereferenced; it will always have
  pointer or reference type.  The second operand is a pointer constant
  serving as constant offset applied to the pointer being dereferenced
  with its type specifying the type to be used for type-based alias analysis.
  The type of the node specifies the alignment of the access.

.. envvar:: TARGET_MEM_REF

  These nodes represent memory accesses whose address directly map to
  an addressing mode of the target architecture.  The first argument
  is ``TMR_BASE`` and is a pointer to the object being accessed.
  The second argument is ``TMR_OFFSET`` which is a pointer constant
  with dual purpose serving both as constant offset and holder of
  the type used for type-based alias analysis.  The first two operands
  have exactly the same semantics as ``MEM_REF``.  The third
  and fourth operand are ``TMR_INDEX`` and ``TMR_STEP`` where
  the former is an integer and the latter an integer constant.  The
  fifth and last operand is ``TMR_INDEX2`` which is an alternate
  non-constant offset.  Any of the third to last operands may be
  ``NULL`` if the corresponding component does not appear in
  the address, but ``TMR_INDEX`` and ``TMR_STEP`` shall be
  always supplied in pair.  The Address of the ``TARGET_MEM_REF``
  is determined in the following way.

  .. code-block:: c++

    TMR_BASE + TMR_OFFSET + TMR_INDEX * TMR_STEP + TMR_INDEX2

  The type of the node specifies the alignment of the access.

.. _unary-and-binary-expressions:

Unary and Binary Expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: NEGATE_EXPR, ABS_EXPR, ABSU_EXPR, BIT_NOT_EXPR, TRUTH_NOT_EXPR, PREDECREMENT_EXPR, PREINCREMENT_EXPR, POSTDECREMENT_EXPR, POSTINCREMENT_EXPR, FIX_TRUNC_EXPR, FLOAT_EXPR, COMPLEX_EXPR, CONJ_EXPR, REALPART_EXPR, IMAGPART_EXPR, NON_LVALUE_EXPR, NOP_EXPR, CONVERT_EXPR, FIXED_CONVERT_EXPR, THROW_EXPR, LSHIFT_EXPR, RSHIFT_EXPR, BIT_IOR_EXPR, BIT_XOR_EXPR, BIT_AND_EXPR, TRUTH_ANDIF_EXPR, TRUTH_ORIF_EXPR, TRUTH_AND_EXPR, TRUTH_OR_EXPR, TRUTH_XOR_EXPR, POINTER_PLUS_EXPR, POINTER_DIFF_EXPR, PLUS_EXPR, MINUS_EXPR, MULT_EXPR, WIDEN_MULT_EXPR, MULT_HIGHPART_EXPR, RDIV_EXPR, TRUNC_DIV_EXPR, FLOOR_DIV_EXPR, CEIL_DIV_EXPR, ROUND_DIV_EXPR, TRUNC_MOD_EXPR, FLOOR_MOD_EXPR, CEIL_MOD_EXPR, ROUND_MOD_EXPR, EXACT_DIV_EXPR, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EQ_EXPR, NE_EXPR, ORDERED_EXPR, UNORDERED_EXPR, UNLT_EXPR, UNLE_EXPR, UNGT_EXPR, UNGE_EXPR, UNEQ_EXPR, LTGT_EXPR, MODIFY_EXPR, INIT_EXPR, COMPOUND_EXPR, COND_EXPR, CALL_EXPR, STMT_EXPR, BIND_EXPR, LOOP_EXPR, EXIT_EXPR, CLEANUP_POINT_EXPR, CONSTRUCTOR, COMPOUND_LITERAL_EXPR, SAVE_EXPR, TARGET_EXPR, VA_ARG_EXPR, ANNOTATE_EXPR

.. envvar:: NEGATE_EXPR

  These nodes represent unary negation of the single operand, for both
  integer and floating-point types.  The type of negation can be
  determined by looking at the type of the expression.

  The behavior of this operation on signed arithmetic overflow is
  controlled by the ``flag_wrapv`` and ``flag_trapv`` variables.

.. envvar:: ABS_EXPR

  These nodes represent the absolute value of the single operand, for
  both integer and floating-point types.  This is typically used to
  implement the ``abs``, ``labs`` and ``llabs`` builtins for
  integer types, and the ``fabs``, ``fabsf`` and ``fabsl``
  builtins for floating point types.  The type of abs operation can
  be determined by looking at the type of the expression.

  This node is not used for complex types.  To represent the modulus
  or complex abs of a complex value, use the ``BUILT_IN_CABS``,
  ``BUILT_IN_CABSF`` or ``BUILT_IN_CABSL`` builtins, as used
  to implement the C99 ``cabs``, ``cabsf`` and ``cabsl``
  built-in functions.

.. envvar:: ABSU_EXPR

  These nodes represent the absolute value of the single operand in
  equivalent unsigned type such that ``ABSU_EXPR`` of ``TYPE_MIN``
  is well defined.

.. envvar:: BIT_NOT_EXPR

  These nodes represent bitwise complement, and will always have integral
  type.  The only operand is the value to be complemented.

.. envvar:: TRUTH_NOT_EXPR

  These nodes represent logical negation, and will always have integral
  (or boolean) type.  The operand is the value being negated.  The type
  of the operand and that of the result are always of ``BOOLEAN_TYPE``
  or ``INTEGER_TYPE``.

.. envvar:: PREDECREMENT_EXPR

  These nodes represent increment and decrement expressions.  The value of
  the single operand is computed, and the operand incremented or
  decremented.  In the case of ``PREDECREMENT_EXPR`` and
  ``PREINCREMENT_EXPR``, the value of the expression is the value
  resulting after the increment or decrement; in the case of
  ``POSTDECREMENT_EXPR`` and ``POSTINCREMENT_EXPR`` is the value
  before the increment or decrement occurs.  The type of the operand, like
  that of the result, will be either integral, boolean, or floating-point.

.. envvar:: FIX_TRUNC_EXPR

  These nodes represent conversion of a floating-point value to an
  integer.  The single operand will have a floating-point type, while
  the complete expression will have an integral (or boolean) type.  The
  operand is rounded towards zero.

.. envvar:: FLOAT_EXPR

  These nodes represent conversion of an integral (or boolean) value to a
  floating-point value.  The single operand will have integral type, while
  the complete expression will have a floating-point type.

  .. todo:: How is the operand supposed to be rounded?  Is this dependent on
    :option:`-mieee` ?

.. envvar:: COMPLEX_EXPR

  These nodes are used to represent complex numbers constructed from two
  expressions of the same (integer or real) type.  The first operand is the
  real part and the second operand is the imaginary part.

.. envvar:: CONJ_EXPR

  These nodes represent the conjugate of their operand.

.. envvar:: REALPART_EXPR

  These nodes represent respectively the real and the imaginary parts
  of complex numbers (their sole argument).

.. envvar:: NON_LVALUE_EXPR

  These nodes indicate that their one and only operand is not an lvalue.
  A back end can treat these identically to the single operand.

.. envvar:: NOP_EXPR

  These nodes are used to represent conversions that do not require any
  code-generation.  For example, conversion of a ``char*`` to an
  ``int*`` does not require any code be generated; such a conversion is
  represented by a ``NOP_EXPR``.  The single operand is the expression
  to be converted.  The conversion from a pointer to a reference is also
  represented with a ``NOP_EXPR``.

.. envvar:: CONVERT_EXPR

  These nodes are similar to ``NOP_EXPR`` s, but are used in those
  situations where code may need to be generated.  For example, if an
  ``int*`` is converted to an ``int`` code may need to be generated
  on some platforms.  These nodes are never used for C++-specific
  conversions, like conversions between pointers to different classes in
  an inheritance hierarchy.  Any adjustments that need to be made in such
  cases are always indicated explicitly.  Similarly, a user-defined
  conversion is never represented by a ``CONVERT_EXPR`` ; instead, the
  function calls are made explicit.

.. envvar:: FIXED_CONVERT_EXPR

  These nodes are used to represent conversions that involve fixed-point
  values.  For example, from a fixed-point value to another fixed-point value,
  from an integer to a fixed-point value, from a fixed-point value to an
  integer, from a floating-point value to a fixed-point value, or from
  a fixed-point value to a floating-point value.

.. envvar:: LSHIFT_EXPR

  These nodes represent left and right shifts, respectively.  The first
  operand is the value to shift; it will always be of integral type.  The
  second operand is an expression for the number of bits by which to
  shift.  Right shift should be treated as arithmetic, i.e., the
  high-order bits should be zero-filled when the expression has unsigned
  type and filled with the sign bit when the expression has signed type.
  Note that the result is undefined if the second operand is larger
  than or equal to the first operand's type size. Unlike most nodes, these
  can have a vector as first operand and a scalar as second operand.

.. envvar:: BIT_IOR_EXPR

  These nodes represent bitwise inclusive or, bitwise exclusive or, and
  bitwise and, respectively.  Both operands will always have integral
  type.

.. envvar:: TRUTH_ANDIF_EXPR

  These nodes represent logical 'and' and logical 'or', respectively.
  These operators are not strict; i.e., the second operand is evaluated
  only if the value of the expression is not determined by evaluation of
  the first operand.  The type of the operands and that of the result are
  always of ``BOOLEAN_TYPE`` or ``INTEGER_TYPE``.

.. envvar:: TRUTH_AND_EXPR

  These nodes represent logical and, logical or, and logical exclusive or.
  They are strict; both arguments are always evaluated.  There are no
  corresponding operators in C or C++, but the front end will sometimes
  generate these expressions anyhow, if it can tell that strictness does
  not matter.  The type of the operands and that of the result are
  always of ``BOOLEAN_TYPE`` or ``INTEGER_TYPE``.

.. envvar:: POINTER_PLUS_EXPR

  This node represents pointer arithmetic.  The first operand is always
  a pointer/reference type.  The second operand is always an unsigned
  integer type compatible with sizetype.  This and POINTER_DIFF_EXPR are
  the only binary arithmetic operators that can operate on pointer types.

.. envvar:: POINTER_DIFF_EXPR

  This node represents pointer subtraction.  The two operands always
  have pointer/reference type.  It returns a signed integer of the same
  precision as the pointers.  The behavior is undefined if the difference
  of the two pointers, seen as infinite precision non-negative integers,
  does not fit in the result type.  The result does not depend on the
  pointer type, it is not divided by the size of the pointed-to type.

.. envvar:: PLUS_EXPR

  These nodes represent various binary arithmetic operations.
  Respectively, these operations are addition, subtraction (of the second
  operand from the first) and multiplication.  Their operands may have
  either integral or floating type, but there will never be case in which
  one operand is of floating type and the other is of integral type.

  The behavior of these operations on signed arithmetic overflow is
  controlled by the ``flag_wrapv`` and ``flag_trapv`` variables.

.. envvar:: WIDEN_MULT_EXPR

  This node represents a widening multiplication.  The operands have
  integral types with same :samp:`{b}` bits of precision, producing an
  integral type result with at least 2 :samp:`{b}` bits of precision.
  The behaviour is equivalent to extending both operands, possibly of
  different signedness, to the result type, then multiplying them.

.. envvar:: MULT_HIGHPART_EXPR

  This node represents the 'high-part' of a widening multiplication.
  For an integral type with :samp:`{b}` bits of precision, the result is
  the most significant :samp:`{b}` bits of the full 2 :samp:`{b}` product.
  Both operands must have the same precision and same signedness.

.. envvar:: RDIV_EXPR

  This node represents a floating point division operation.

.. envvar:: TRUNC_DIV_EXPR

  These nodes represent integer division operations that return an integer
  result.  ``TRUNC_DIV_EXPR`` rounds towards zero, ``FLOOR_DIV_EXPR``
  rounds towards negative infinity, ``CEIL_DIV_EXPR`` rounds towards
  positive infinity and ``ROUND_DIV_EXPR`` rounds to the closest integer.
  Integer division in C and C++ is truncating, i.e. ``TRUNC_DIV_EXPR``.

  The behavior of these operations on signed arithmetic overflow, when
  dividing the minimum signed integer by minus one, is controlled by the
  ``flag_wrapv`` and ``flag_trapv`` variables.

.. envvar:: TRUNC_MOD_EXPR

  These nodes represent the integer remainder or modulus operation.
  The integer modulus of two operands ``a`` and ``b`` is
  defined as ``a - (a/b)*b`` where the division calculated using
  the corresponding division operator.  Hence for ``TRUNC_MOD_EXPR``
  this definition assumes division using truncation towards zero, i.e.
  ``TRUNC_DIV_EXPR``.  Integer remainder in C and C++ uses truncating
  division, i.e. ``TRUNC_MOD_EXPR``.

.. envvar:: EXACT_DIV_EXPR

  The ``EXACT_DIV_EXPR`` code is used to represent integer divisions where
  the numerator is known to be an exact multiple of the denominator.  This
  allows the backend to choose between the faster of ``TRUNC_DIV_EXPR``,
  ``CEIL_DIV_EXPR`` and ``FLOOR_DIV_EXPR`` for the current target.

.. envvar:: LT_EXPR

  These nodes represent the less than, less than or equal to, greater than,
  greater than or equal to, less or greater than, equal, and not equal
  comparison operators.  The first and second operands will either be both
  of integral type, both of floating type or both of vector type, except for
  LTGT_EXPR where they will only be both of floating type.  The result type
  of these expressions will always be of integral, boolean or signed integral
  vector type.  These operations return the result type's zero value for false,
  the result type's one value for true, and a vector whose elements are zero
  (false) or minus one (true) for vectors.

  For floating point comparisons, if we honor IEEE NaNs and either operand
  is NaN, then ``NE_EXPR`` always returns true and the remaining operators
  always return false.  On some targets, comparisons against an IEEE NaN,
  other than equality and inequality, may generate a floating-point exception.

.. envvar:: ORDERED_EXPR

  These nodes represent non-trapping ordered and unordered comparison
  operators.  These operations take two floating point operands and
  determine whether they are ordered or unordered relative to each other.
  If either operand is an IEEE NaN, their comparison is defined to be
  unordered, otherwise the comparison is defined to be ordered.  The
  result type of these expressions will always be of integral or boolean
  type.  These operations return the result type's zero value for false,
  and the result type's one value for true.

.. envvar:: UNLT_EXPR

  These nodes represent the unordered comparison operators.
  These operations take two floating point operands and determine whether
  the operands are unordered or are less than, less than or equal to,
  greater than, greater than or equal to, or equal respectively.  For
  example, ``UNLT_EXPR`` returns true if either operand is an IEEE
  NaN or the first operand is less than the second.  All these operations
  are guaranteed not to generate a floating point exception.  The result
  type of these expressions will always be of integral or boolean type.
  These operations return the result type's zero value for false,
  and the result type's one value for true.

.. envvar:: MODIFY_EXPR

  These nodes represent assignment.  The left-hand side is the first
  operand; the right-hand side is the second operand.  The left-hand side
  will be a ``VAR_DECL``, ``INDIRECT_REF``, ``COMPONENT_REF``, or
  other lvalue.

  These nodes are used to represent not only assignment with :samp:`=` but
  also compound assignments (like :samp:`+=`), by reduction to :samp:`=`
  assignment.  In other words, the representation for :samp:`i += 3` looks
  just like that for :samp:`i = i + 3`.

.. envvar:: INIT_EXPR

  These nodes are just like ``MODIFY_EXPR``, but are used only when a
  variable is initialized, rather than assigned to subsequently.  This
  means that we can assume that the target of the initialization is not
  used in computing its own value; any reference to the lhs in computing
  the rhs is undefined.

.. envvar:: COMPOUND_EXPR

  These nodes represent comma-expressions.  The first operand is an
  expression whose value is computed and thrown away prior to the
  evaluation of the second operand.  The value of the entire expression is
  the value of the second operand.

.. envvar:: COND_EXPR

  These nodes represent ``?:`` expressions.  The first operand
  is of boolean or integral type.  If it evaluates to a nonzero value,
  the second operand should be evaluated, and returned as the value of the
  expression.  Otherwise, the third operand is evaluated, and returned as
  the value of the expression.

  The second operand must have the same type as the entire expression,
  unless it unconditionally throws an exception or calls a noreturn
  function, in which case it should have void type.  The same constraints
  apply to the third operand.  This allows array bounds checks to be
  represented conveniently as ``(i >= 0 && i < 10) ? i : abort()``.

  As a GNU extension, the C language front-ends allow the second
  operand of the ``?:`` operator may be omitted in the source.
  For example, ``x ? : 3`` is equivalent to ``x ? x : 3``,
  assuming that ``x`` is an expression without side effects.
  In the tree representation, however, the second operand is always
  present, possibly protected by ``SAVE_EXPR`` if the first
  argument does cause side effects.

.. envvar:: CALL_EXPR

  These nodes are used to represent calls to functions, including
  non-static member functions.  ``CALL_EXPR`` s are implemented as
  expression nodes with a variable number of operands.  Rather than using
  ``TREE_OPERAND`` to extract them, it is preferable to use the
  specialized accessor macros and functions that operate specifically on
  ``CALL_EXPR`` nodes.

  ``CALL_EXPR_FN`` returns a pointer to the
  function to call; it is always an expression whose type is a
  ``POINTER_TYPE``.

  The number of arguments to the call is returned by ``call_expr_nargs``,
  while the arguments themselves can be accessed with the ``CALL_EXPR_ARG``
  macro.  The arguments are zero-indexed and numbered left-to-right.
  You can iterate over the arguments using ``FOR_EACH_CALL_EXPR_ARG``, as in:

  .. code-block:: c++

    tree call, arg;
    call_expr_arg_iterator iter;
    FOR_EACH_CALL_EXPR_ARG (arg, iter, call)
      /* arg is bound to successive arguments of call.  */
      ...;

  For non-static
  member functions, there will be an operand corresponding to the
  ``this`` pointer.  There will always be expressions corresponding to
  all of the arguments, even if the function is declared with default
  arguments and some arguments are not explicitly provided at the call
  sites.

  ``CALL_EXPR`` s also have a ``CALL_EXPR_STATIC_CHAIN`` operand that
  is used to implement nested functions.  This operand is otherwise null.

.. envvar:: CLEANUP_POINT_EXPR

  These nodes represent full-expressions.  The single operand is an
  expression to evaluate.  Any destructor calls engendered by the creation
  of temporaries during the evaluation of that expression should be
  performed immediately after the expression is evaluated.

.. envvar:: CONSTRUCTOR

  These nodes represent the brace-enclosed initializers for a structure or an
  array.  They contain a sequence of component values made out of a vector of
  constructor_elt, which is a (``INDEX``, ``VALUE``) pair.

  If the ``TREE_TYPE`` of the ``CONSTRUCTOR`` is a ``RECORD_TYPE``,
  ``UNION_TYPE`` or ``QUAL_UNION_TYPE`` then the ``INDEX`` of each
  node in the sequence will be a ``FIELD_DECL`` and the ``VALUE`` will
  be the expression used to initialize that field.

  If the ``TREE_TYPE`` of the ``CONSTRUCTOR`` is an ``ARRAY_TYPE``,
  then the ``INDEX`` of each node in the sequence will be an
  ``INTEGER_CST`` or a ``RANGE_EXPR`` of two ``INTEGER_CST`` s.
  A single ``INTEGER_CST`` indicates which element of the array is being
  assigned to.  A ``RANGE_EXPR`` indicates an inclusive range of elements
  to initialize.  In both cases the ``VALUE`` is the corresponding
  initializer.  It is re-evaluated for each element of a
  ``RANGE_EXPR``.  If the ``INDEX`` is ``NULL_TREE``, then
  the initializer is for the next available array element.

  In the front end, you should not depend on the fields appearing in any
  particular order.  However, in the middle end, fields must appear in
  declaration order.  You should not assume that all fields will be
  represented.  Unrepresented fields will be cleared (zeroed), unless the
  CONSTRUCTOR_NO_CLEARING flag is set, in which case their value becomes
  undefined.

.. envvar:: COMPOUND_LITERAL_EXPR

  These nodes represent ISO C99 compound literals.  The
  ``COMPOUND_LITERAL_EXPR_DECL_EXPR`` is a ``DECL_EXPR``
  containing an anonymous ``VAR_DECL`` for
  the unnamed object represented by the compound literal; the
  ``DECL_INITIAL`` of that ``VAR_DECL`` is a ``CONSTRUCTOR``
  representing the brace-enclosed list of initializers in the compound
  literal.  That anonymous ``VAR_DECL`` can also be accessed directly
  by the ``COMPOUND_LITERAL_EXPR_DECL`` macro.

.. envvar:: SAVE_EXPR

  A ``SAVE_EXPR`` represents an expression (possibly involving
  side effects) that is used more than once.  The side effects should
  occur only the first time the expression is evaluated.  Subsequent uses
  should just reuse the computed value.  The first operand to the
  ``SAVE_EXPR`` is the expression to evaluate.  The side effects should
  be executed where the ``SAVE_EXPR`` is first encountered in a
  depth-first preorder traversal of the expression tree.

.. envvar:: TARGET_EXPR

  A ``TARGET_EXPR`` represents a temporary object.  The first operand
  is a ``VAR_DECL`` for the temporary variable.  The second operand is
  the initializer for the temporary.  The initializer is evaluated and,
  if non-void, copied (bitwise) into the temporary.  If the initializer
  is void, that means that it will perform the initialization itself.

  Often, a ``TARGET_EXPR`` occurs on the right-hand side of an
  assignment, or as the second operand to a comma-expression which is
  itself the right-hand side of an assignment, etc.  In this case, we say
  that the ``TARGET_EXPR`` is 'normal'; otherwise, we say it is
  'orphaned'.  For a normal ``TARGET_EXPR`` the temporary variable
  should be treated as an alias for the left-hand side of the assignment,
  rather than as a new temporary variable.

  The third operand to the ``TARGET_EXPR``, if present, is a
  cleanup-expression (i.e., destructor call) for the temporary.  If this
  expression is orphaned, then this expression must be executed when the
  statement containing this expression is complete.  These cleanups must
  always be executed in the order opposite to that in which they were
  encountered.  Note that if a temporary is created on one branch of a
  conditional operator (i.e., in the second or third operand to a
  ``COND_EXPR``), the cleanup must be run only if that branch is
  actually executed.

.. envvar:: VA_ARG_EXPR

  This node is used to implement support for the C/C++ variable argument-list
  mechanism.  It represents expressions like ``va_arg (ap, type)``.
  Its ``TREE_TYPE`` yields the tree representation for ``type`` and
  its sole argument yields the representation for ``ap``.

.. envvar:: ANNOTATE_EXPR

  This node is used to attach markers to an expression. The first operand
  is the annotated expression, the second is an ``INTEGER_CST`` with
  a value from ``enum annot_expr_kind``, the third is an ``INTEGER_CST``.

.. _vectors:

Vectors
^^^^^^^

.. index:: VEC_DUPLICATE_EXPR, VEC_SERIES_EXPR, VEC_LSHIFT_EXPR, VEC_RSHIFT_EXPR, VEC_WIDEN_MULT_HI_EXPR, VEC_WIDEN_MULT_LO_EXPR, VEC_WIDEN_PLUS_HI_EXPR, VEC_WIDEN_PLUS_LO_EXPR, VEC_WIDEN_MINUS_HI_EXPR, VEC_WIDEN_MINUS_LO_EXPR, VEC_UNPACK_HI_EXPR, VEC_UNPACK_LO_EXPR, VEC_UNPACK_FLOAT_HI_EXPR, VEC_UNPACK_FLOAT_LO_EXPR, VEC_UNPACK_FIX_TRUNC_HI_EXPR, VEC_UNPACK_FIX_TRUNC_LO_EXPR, VEC_PACK_TRUNC_EXPR, VEC_PACK_SAT_EXPR, VEC_PACK_FIX_TRUNC_EXPR, VEC_PACK_FLOAT_EXPR, VEC_COND_EXPR, SAD_EXPR

.. envvar:: VEC_DUPLICATE_EXPR

  This node has a single operand and represents a vector in which every
  element is equal to that operand.

.. envvar:: VEC_SERIES_EXPR

  This node represents a vector formed from a scalar base and step,
  given as the first and second operands respectively.  Element :samp:`{i}`
  of the result is equal to :samp:`{base} + {i}*{step}`.

  This node is restricted to integral types, in order to avoid
  specifying the rounding behavior for floating-point types.

.. envvar:: VEC_LSHIFT_EXPR

  These nodes represent whole vector left and right shifts, respectively.
  The first operand is the vector to shift; it will always be of vector type.
  The second operand is an expression for the number of bits by which to
  shift.  Note that the result is undefined if the second operand is larger
  than or equal to the first operand's type size.

.. envvar:: VEC_WIDEN_MULT_HI_EXPR

  These nodes represent widening vector multiplication of the high and low
  parts of the two input vectors, respectively.  Their operands are vectors
  that contain the same number of elements (``N``) of the same integral type.
  The result is a vector that contains half as many elements, of an integral type
  whose size is twice as wide.  In the case of ``VEC_WIDEN_MULT_HI_EXPR`` the
  high ``N/2`` elements of the two vector are multiplied to produce the
  vector of ``N/2`` products. In the case of ``VEC_WIDEN_MULT_LO_EXPR`` the
  low ``N/2`` elements of the two vector are multiplied to produce the
  vector of ``N/2`` products.

.. envvar:: VEC_WIDEN_PLUS_HI_EXPR

  These nodes represent widening vector addition of the high and low parts of
  the two input vectors, respectively.  Their operands are vectors that contain
  the same number of elements (``N``) of the same integral type. The result
  is a vector that contains half as many elements, of an integral type whose size
  is twice as wide.  In the case of ``VEC_WIDEN_PLUS_HI_EXPR`` the high
  ``N/2`` elements of the two vectors are added to produce the vector of
  ``N/2`` products.  In the case of ``VEC_WIDEN_PLUS_LO_EXPR`` the low
  ``N/2`` elements of the two vectors are added to produce the vector of
  ``N/2`` products.

.. envvar:: VEC_WIDEN_MINUS_HI_EXPR

  These nodes represent widening vector subtraction of the high and low parts of
  the two input vectors, respectively.  Their operands are vectors that contain
  the same number of elements (``N``) of the same integral type. The high/low
  elements of the second vector are subtracted from the high/low elements of the
  first. The result is a vector that contains half as many elements, of an
  integral type whose size is twice as wide.  In the case of
  ``VEC_WIDEN_MINUS_HI_EXPR`` the high ``N/2`` elements of the second
  vector are subtracted from the high ``N/2`` of the first to produce the
  vector of ``N/2`` products.  In the case of
  ``VEC_WIDEN_MINUS_LO_EXPR`` the low ``N/2`` elements of the second
  vector are subtracted from the low ``N/2`` of the first to produce the
  vector of ``N/2`` products.

.. envvar:: VEC_UNPACK_HI_EXPR

  These nodes represent unpacking of the high and low parts of the input vector,
  respectively.  The single operand is a vector that contains ``N`` elements
  of the same integral or floating point type.  The result is a vector
  that contains half as many elements, of an integral or floating point type
  whose size is twice as wide.  In the case of ``VEC_UNPACK_HI_EXPR`` the
  high ``N/2`` elements of the vector are extracted and widened (promoted).
  In the case of ``VEC_UNPACK_LO_EXPR`` the low ``N/2`` elements of the
  vector are extracted and widened (promoted).

.. envvar:: VEC_UNPACK_FLOAT_HI_EXPR

  These nodes represent unpacking of the high and low parts of the input vector,
  where the values are converted from fixed point to floating point.  The
  single operand is a vector that contains ``N`` elements of the same
  integral type.  The result is a vector that contains half as many elements
  of a floating point type whose size is twice as wide.  In the case of
  ``VEC_UNPACK_FLOAT_HI_EXPR`` the high ``N/2`` elements of the vector are
  extracted, converted and widened.  In the case of ``VEC_UNPACK_FLOAT_LO_EXPR``
  the low ``N/2`` elements of the vector are extracted, converted and widened.

.. envvar:: VEC_UNPACK_FIX_TRUNC_HI_EXPR

  These nodes represent unpacking of the high and low parts of the input vector,
  where the values are truncated from floating point to fixed point.  The
  single operand is a vector that contains ``N`` elements of the same
  floating point type.  The result is a vector that contains half as many
  elements of an integral type whose size is twice as wide.  In the case of
  ``VEC_UNPACK_FIX_TRUNC_HI_EXPR`` the high ``N/2`` elements of the
  vector are extracted and converted with truncation.  In the case of
  ``VEC_UNPACK_FIX_TRUNC_LO_EXPR`` the low ``N/2`` elements of the
  vector are extracted and converted with truncation.

.. envvar:: VEC_PACK_TRUNC_EXPR

  This node represents packing of truncated elements of the two input vectors
  into the output vector.  Input operands are vectors that contain the same
  number of elements of the same integral or floating point type.  The result
  is a vector that contains twice as many elements of an integral or floating
  point type whose size is half as wide. The elements of the two vectors are
  demoted and merged (concatenated) to form the output vector.

.. envvar:: VEC_PACK_SAT_EXPR

  This node represents packing of elements of the two input vectors into the
  output vector using saturation.  Input operands are vectors that contain
  the same number of elements of the same integral type.  The result is a
  vector that contains twice as many elements of an integral type whose size
  is half as wide.  The elements of the two vectors are demoted and merged
  (concatenated) to form the output vector.

.. envvar:: VEC_PACK_FIX_TRUNC_EXPR

  This node represents packing of elements of the two input vectors into the
  output vector, where the values are converted from floating point
  to fixed point.  Input operands are vectors that contain the same number
  of elements of a floating point type.  The result is a vector that contains
  twice as many elements of an integral type whose size is half as wide.  The
  elements of the two vectors are merged (concatenated) to form the output
  vector.

.. envvar:: VEC_PACK_FLOAT_EXPR

  This node represents packing of elements of the two input vectors into the
  output vector, where the values are converted from fixed point to floating
  point.  Input operands are vectors that contain the same number of elements
  of an integral type.  The result is a vector that contains twice as many
  elements of floating point type whose size is half as wide.  The elements of
  the two vectors are merged (concatenated) to form the output vector.

.. envvar:: VEC_COND_EXPR

  These nodes represent ``?:`` expressions.  The three operands must be
  vectors of the same size and number of elements.  The second and third
  operands must have the same type as the entire expression.  The first
  operand is of signed integral vector type.  If an element of the first
  operand evaluates to a zero value, the corresponding element of the
  result is taken from the third operand. If it evaluates to a minus one
  value, it is taken from the second operand. It should never evaluate to
  any other value currently, but optimizations should not rely on that
  property. In contrast with a ``COND_EXPR``, all operands are always
  evaluated.

.. envvar:: SAD_EXPR

  This node represents the Sum of Absolute Differences operation.  The three
  operands must be vectors of integral types.  The first and second operand
  must have the same type.  The size of the vector element of the third
  operand must be at lease twice of the size of the vector element of the
  first and second one.  The SAD is calculated between the first and second
  operands, added to the third operand, and returned.
