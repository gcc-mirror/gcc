..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gfc_expr:

gfc_expr
********

.. index:: gfc_expr, struct gfc_expr

Expressions and 'values', including constants, variable-, array- and
component-references as well as complex expressions consisting of operators and
function calls are internally represented as one or a whole tree of
``gfc_expr`` objects.  The member ``expr_type`` specifies the overall
type of an expression (for instance, ``EXPR_CONSTANT`` for constants or
``EXPR_VARIABLE`` for variable references).  The members ``ts`` and
``rank`` as well as ``shape``, which can be ``NULL``, specify
the type, rank and, if applicable, shape of the whole expression or expression
tree of which the current structure is the root.  ``where`` is the locus of
this expression in the source code.

Depending on the flavor of the expression being described by the object
(that is, the value of its ``expr_type`` member), the corresponding structure
in the ``value`` union will usually contain additional data describing the
expression's value in a type-specific manner.  The ``ref`` member is used to
build chains of (array-, component- and substring-) references if the expression
in question contains such references, see below for details.

Constants
^^^^^^^^^

Scalar constants are represented by ``gfc_expr`` nodes with their
``expr_type`` set to ``EXPR_CONSTANT``.  The constant's value shall
already be known at compile-time and is stored in the ``logical``,
``integer``, ``real``, ``complex`` or ``character`` struct inside
``value``, depending on the constant's type specification.

Operators
^^^^^^^^^

Operator-expressions are expressions that are the result of the execution of
some operator on one or two operands.  The expressions have an ``expr_type``
of ``EXPR_OP``.  Their ``value.op`` structure contains additional data.

``op1`` and optionally ``op2`` if the operator is binary point to the
two operands, and ``operator`` or ``uop`` describe the operator that
should be evaluated on these operands, where ``uop`` describes a user-defined
operator.

Function Calls
^^^^^^^^^^^^^^

If the expression is the return value of a function-call, its ``expr_type``
is set to ``EXPR_FUNCTION``, and ``symtree`` must point to the symtree
identifying the function to be called.  ``value.function.actual`` holds the
actual arguments given to the function as a linked list of
``gfc_actual_arglist`` nodes.

The other members of ``value.function`` describe the function being called
in more detail, containing a link to the intrinsic symbol or user-defined
function symbol if the call is to an intrinsic or external function,
respectively.  These values are determined during resolution-phase from the
structure's ``symtree`` member.

A special case of function calls are 'component calls' to type-bound
procedures; those have the ``expr_type`` ``EXPR_COMPCALL`` with
``value.compcall`` containing the argument list and the procedure called,
while ``symtree`` and ``ref`` describe the object on which the procedure
was called in the same way as a ``EXPR_VARIABLE`` expression would.
See :ref:`type-bound-procedures`.

Array- and Structure-Constructors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Array- and structure-constructors (one could probably call them 'array-' and
'derived-type constants') are ``gfc_expr`` structures with their
``expr_type`` member set to ``EXPR_ARRAY`` or ``EXPR_STRUCTURE``,
respectively.  For structure constructors, ``symtree`` points to the
derived-type symbol for the type being constructed.

The values for initializing each array element or structure component are
stored as linked-list of ``gfc_constructor`` nodes in the
``value.constructor`` member.

Null
^^^^

``NULL`` is a special value for pointers; it can be of different base types.
Such a ``NULL`` value is represented in the internal tree by a
``gfc_expr`` node with ``expr_type`` ``EXPR_NULL``.  If the base type
of the ``NULL`` expression is known, it is stored in ``ts`` (that's for
instance the case for default-initializers of ``ALLOCATABLE`` components),
but this member can also be set to ``BT_UNKNOWN`` if the information is not
available (for instance, when the expression is a pointer-initializer
``NULL()``).

Variables and Reference Expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Variable references are ``gfc_expr`` structures with their ``expr_type``
set to ``EXPR_VARIABLE`` ; their ``symtree`` should point to the variable
that is referenced.

For this type of expression, it's also possible to chain array-, component-
or substring-references to the original expression to get something like
:samp:`struct%component(2:5)`, where ``component`` is either an array or
a ``CHARACTER`` member of ``struct`` that is of some derived-type.  Such a
chain of references is achieved by a linked list headed by ``ref`` of the
``gfc_expr`` node.  For the example above it would be (:samp:`==|` is the
last ``NULL`` pointer):

.. code-block:: c++

  EXPR_VARIABLE(struct) ==> REF_COMPONENT(component) ==> REF_ARRAY(2:5) ==|

If ``component`` is a string rather than an array, the last element would be
a ``REF_SUBSTRING`` reference, of course.  If the variable itself or some
component referenced is an array and the expression should reference the whole
array rather than being followed by an array-element or -section reference, a
``REF_ARRAY`` reference must be built as the last element in the chain with
an array-reference type of ``AR_FULL``. Consider this example code:

.. code-block:: fortran

  TYPE :: mytype
    INTEGER :: array(42)
  END TYPE mytype

  TYPE(mytype) :: variable
  INTEGER :: local_array(5)

  CALL do_something (variable%array, local_array)

The ``gfc_expr`` nodes representing the arguments to the :samp:`do_something`
call will have a reference-chain like this:

.. code-block:: c++

  EXPR_VARIABLE(variable) ==> REF_COMPONENT(array) ==> REF_ARRAY(FULL) ==|
  EXPR_VARIABLE(local_array) ==> REF_ARRAY(FULL) ==|

Constant Substring References
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``EXPR_SUBSTRING`` is a special type of expression that encodes a substring
reference of a constant string, as in the following code snippet:

.. code-block:: c++

  x = "abcde"(1:2)

In this case, ``value.character`` contains the full string's data as if it
was a string constant, but the ``ref`` member is also set and points to a
substring reference as described in the subsection above.
