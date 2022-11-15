..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _basic-data-structures:

Basic data structures
*********************

Gfortran creates GENERIC as an intermediate language for the
middle-end. Details about GENERIC can be found in the GCC manual.

The basic data structure of GENERIC is a ``tree``. Everything in
GENERIC is a ``tree``, including types and statements.  Fortunately
for the gfortran programmer, ``tree`` variables are
garbage-collected, so doing memory management for them is not
necessary.

``tree`` expressions are built using functions such as, for
example, ``fold_build2_loc``.  For two tree variables ``a`` and
``b``, both of which have the type ``gfc_arry_index_type``,
calculation ``c = a * b`` would be done by

.. code-block:: c++

  c = fold_build2_loc (input_location, MULT_EXPR,
                       gfc_array_index_type, a, b);

The types have to agree, otherwise internal compiler errors will occur
at a later stage.  Expressions can be converted to a different type
using ``fold_convert``.

Accessing individual members in the ``tree`` structures should not
be done. Rather, access should be done via macros.

One basic data structure is the ``stmtblock_t`` struct. This is
used for holding a list of statements, expressed as ``tree``
expressions.  If a block is created using ``gfc_start_block``, it
has its own scope for variables; if it is created using
``gfc_init_block``, it does not have its own scope.

It is possible to

* Add an expression to the end of a block using ``gfc_add_expr_to_block``

* Add an expression to the beginning of a block using ``void gfc_prepend_expr_to_block``

* Make a block into a single ``tree`` using
  ``gfc_finish_block``.  For example, this is needed to put the
  contents of a block into the ``if`` or ``else`` branch of
  a ``COND_EXPR``.

Variables are also ``tree`` expressions, they can be created using
``gfc_create_var``. Assigning to a variable can be done with
``gfc_add_modify``.

An example: Creating a default integer type variable in the current
scope with the prefix 'everything' in the ``stmt_block``
``block`` and assigning the value 42 would be

.. code-block:: c++

  tree var, *block;
  /* Initialize block somewhere here.  */
  var = gfc_create_var (integer_type_node, "everything");
  gfc_add_modify (block, var, build_int_cst (integer_type_node, 42));
