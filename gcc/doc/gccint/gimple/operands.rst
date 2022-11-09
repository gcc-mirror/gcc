..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Operands

.. _operands:

Operands
********

In general, expressions in GIMPLE consist of an operation and the
appropriate number of simple operands; these operands must either be a
GIMPLE rvalue (``is_gimple_val``), i.e. a constant or a register
variable.  More complex operands are factored out into temporaries, so
that

.. code-block:: c++

  a = b + c + d

becomes

.. code-block:: c++

  T1 = b + c;
  a = T1 + d;

The same rule holds for arguments to a ``GIMPLE_CALL``.

The target of an assignment is usually a variable, but can also be a
``MEM_REF`` or a compound lvalue as described below.

.. toctree::
  :maxdepth: 2


.. index:: Compound Expressions

.. _compound-expressions:

Compound Expressions
^^^^^^^^^^^^^^^^^^^^

The left-hand side of a C comma expression is simply moved into a separate
statement.

.. index:: Compound Lvalues

.. _compound-lvalues:

Compound Lvalues
^^^^^^^^^^^^^^^^

Currently compound lvalues involving array and structure field references
are not broken down; an expression like ``a.b[2] = 42`` is not reduced
any further (though complex array subscripts are).  This restriction is a
workaround for limitations in later optimizers; if we were to convert this
to

.. code-block:: c++

  T1 = &a.b;
  T1[2] = 42;

alias analysis would not remember that the reference to ``T1[2]`` came
by way of ``a.b``, so it would think that the assignment could alias
another member of ``a`` ; this broke ``struct-alias-1.c``.  Future
optimizer improvements may make this limitation unnecessary.

.. index:: Conditional Expressions

.. _conditional-expressions:

Conditional Expressions
^^^^^^^^^^^^^^^^^^^^^^^

A C ``?:`` expression is converted into an ``if`` statement with
each branch assigning to the same temporary.  So,

.. code-block:: c++

  a = b ? c : d;

becomes

.. code-block:: c++

  if (b == 1)
    T1 = c;
  else
    T1 = d;
  a = T1;

The GIMPLE level if-conversion pass re-introduces ``?:``
expression, if appropriate. It is used to vectorize loops with
conditions using vector conditional operations.

Note that in GIMPLE, ``if`` statements are represented using
``GIMPLE_COND``, as described below.

.. index:: Logical Operators

.. _logical-operators:

Logical Operators
^^^^^^^^^^^^^^^^^

Except when they appear in the condition operand of a
``GIMPLE_COND``, logical 'and' and 'or' operators are simplified
as follows: ``a = b && c`` becomes

.. code-block:: c++

  T1 = (bool)b;
  if (T1 == true)
    T1 = (bool)c;
  a = T1;

Note that ``T1`` in this example cannot be an expression temporary,
because it has two different assignments.

Manipulating operands
^^^^^^^^^^^^^^^^^^^^^

All gimple operands are of type ``tree``.  But only certain
types of trees are allowed to be used as operand tuples.  Basic
validation is controlled by the function
``get_gimple_rhs_class``, which given a tree code, returns an
``enum`` with the following values of type ``enum
gimple_rhs_class``

* ``GIMPLE_INVALID_RHS``
  The tree cannot be used as a GIMPLE operand.

* ``GIMPLE_TERNARY_RHS``
  The tree is a valid GIMPLE ternary operation.

* ``GIMPLE_BINARY_RHS``
  The tree is a valid GIMPLE binary operation.

* ``GIMPLE_UNARY_RHS``
  The tree is a valid GIMPLE unary operation.

* ``GIMPLE_SINGLE_RHS``
  The tree is a single object, that cannot be split into simpler
  operands (for instance, ``SSA_NAME``, ``VAR_DECL``, ``COMPONENT_REF``, etc).

  This operand class also acts as an escape hatch for tree nodes
  that may be flattened out into the operand vector, but would need
  more than two slots on the RHS.  For instance, a ``COND_EXPR``
  expression of the form ``(a op b) ? x : y`` could be flattened
  out on the operand vector using 4 slots, but it would also
  require additional processing to distinguish ``c = a op b``
  from ``c = a op b ? x : y``.  Something similar occurs with
  ``ASSERT_EXPR``.   In time, these special case tree
  expressions should be flattened into the operand vector.

For tree nodes in the categories ``GIMPLE_TERNARY_RHS``,
``GIMPLE_BINARY_RHS`` and ``GIMPLE_UNARY_RHS``, they cannot be
stored inside tuples directly.  They first need to be flattened and
separated into individual components.  For instance, given the GENERIC
expression

.. code-block:: c++

  a = b + c

its tree representation is:

.. code-block:: c++

  MODIFY_EXPR <VAR_DECL  <a>, PLUS_EXPR <VAR_DECL <b>, VAR_DECL <c>>>

In this case, the GIMPLE form for this statement is logically
identical to its GENERIC form but in GIMPLE, the ``PLUS_EXPR``
on the RHS of the assignment is not represented as a tree,
instead the two operands are taken out of the ``PLUS_EXPR`` sub-tree
and flattened into the GIMPLE tuple as follows:

.. code-block:: c++

  GIMPLE_ASSIGN <PLUS_EXPR, VAR_DECL <a>, VAR_DECL <b>, VAR_DECL <c>>

Operand vector allocation
^^^^^^^^^^^^^^^^^^^^^^^^^

The operand vector is stored at the bottom of the three tuple
structures that accept operands. This means, that depending on
the code of a given statement, its operand vector will be at
different offsets from the base of the structure.  To access
tuple operands use the following accessors

.. function:: unsigned gimple_num_ops (gimple g)

  Returns the number of operands in statement G.

.. function:: tree gimple_op (gimple g, unsigned i)

  Returns operand ``I`` from statement ``G``.

.. function:: tree * gimple_ops (gimple g)

  Returns a pointer into the operand vector for statement ``G``.  This
  is computed using an internal table called ``gimple_ops_offset_`` [].
  This table is indexed by the gimple code of ``G``.

  When the compiler is built, this table is filled-in using the
  sizes of the structures used by each statement code defined in
  gimple.def.  Since the operand vector is at the bottom of the
  structure, for a gimple code ``C`` the offset is computed as sizeof
  (struct-of ``C``) - sizeof (tree).

  This mechanism adds one memory indirection to every access when
  using ``gimple_op`` (), if this becomes a bottleneck, a pass can
  choose to memoize the result from ``gimple_ops`` () and use that to
  access the operands.

Operand validation
^^^^^^^^^^^^^^^^^^

When adding a new operand to a gimple statement, the operand will
be validated according to what each tuple accepts in its operand
vector.  These predicates are called by the
``gimple_name_set_...()``.  Each tuple will use one of the
following predicates (Note, this list is not exhaustive):

.. function:: bool is_gimple_val (tree t)

  Returns true if t is a "GIMPLE value", which are all the
  non-addressable stack variables (variables for which
  ``is_gimple_reg`` returns true) and constants (expressions for which
  ``is_gimple_min_invariant`` returns true).

.. function:: bool is_gimple_addressable (tree t)

  Returns true if t is a symbol or memory reference whose address
  can be taken.

.. function:: bool is_gimple_asm_val (tree t)

  Similar to ``is_gimple_val`` but it also accepts hard registers.

.. function:: bool is_gimple_call_addr (tree t)

  Return true if t is a valid expression to use as the function
  called by a ``GIMPLE_CALL``.

.. function:: bool is_gimple_mem_ref_addr (tree t)

  Return true if t is a valid expression to use as first operand
  of a ``MEM_REF`` expression.

.. function:: bool is_gimple_constant (tree t)

  Return true if t is a valid gimple constant.

.. function:: bool is_gimple_min_invariant (tree t)

  Return true if t is a valid minimal invariant.  This is different
  from constants, in that the specific value of t may not be known
  at compile time, but it is known that it doesn't change (e.g.,
  the address of a function local variable).

.. function:: bool is_gimple_ip_invariant (tree t)

  Return true if t is an interprocedural invariant.  This means that t
  is a valid invariant in all functions (e.g. it can be an address of a
  global variable but not of a local one).

.. function:: bool is_gimple_ip_invariant_address (tree t)

  Return true if t is an ``ADDR_EXPR`` that does not change once the
  program is running (and which is valid in all functions).

Statement validation
^^^^^^^^^^^^^^^^^^^^

.. function:: bool is_gimple_assign (gimple g)

  Return true if the code of g is ``GIMPLE_ASSIGN``.

.. function:: bool is_gimple_call (gimple g)

  Return true if the code of g is ``GIMPLE_CALL``.

.. function:: bool is_gimple_debug (gimple g)

  Return true if the code of g is ``GIMPLE_DEBUG``.

.. function:: bool gimple_assign_cast_p (const_gimple g)

  Return true if g is a ``GIMPLE_ASSIGN`` that performs a type cast
  operation.

.. function:: bool gimple_debug_bind_p (gimple g)

  Return true if g is a ``GIMPLE_DEBUG`` that binds the value of an
  expression to a variable.

.. function:: bool is_gimple_omp (gimple g)

  Return true if g is any of the OpenMP codes.

.. function:: bool gimple_debug_begin_stmt_p (gimple g)

  Return true if g is a ``GIMPLE_DEBUG`` that marks the beginning of
  a source statement.

.. function:: bool gimple_debug_inline_entry_p (gimple g)

  Return true if g is a ``GIMPLE_DEBUG`` that marks the entry
  point of an inlined function.

.. function:: bool gimple_debug_nonbind_marker_p (gimple g)

  Return true if g is a ``GIMPLE_DEBUG`` that marks a program location,
  without any variable binding.
