.. Copyright (C) 2014-2024 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <https://www.gnu.org/licenses/>.

.. default-domain:: c

Expressions
===========

Rvalues
-------
.. type:: gcc_jit_rvalue

A :c:type:`gcc_jit_rvalue` is an expression that can be computed.

It can be simple, e.g.:

  * an integer value e.g. `0` or `42`
  * a string literal e.g. `"Hello world"`
  * a variable e.g. `i`.  These are also lvalues (see below).

or compound e.g.:

  * a unary expression e.g. `!cond`
  * a binary expression e.g. `(a + b)`
  * a function call e.g. `get_distance (&player_ship, &target)`
  * etc.

Every rvalue has an associated type, and the API will check to ensure
that types match up correctly (otherwise the context will emit an error).

.. function:: gcc_jit_type *gcc_jit_rvalue_get_type (gcc_jit_rvalue *rvalue)

  Get the type of this rvalue.

.. function:: gcc_jit_object *gcc_jit_rvalue_as_object (gcc_jit_rvalue *rvalue)

  Upcast the given rvalue to be an object.


Simple expressions
******************

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_rvalue_from_int (gcc_jit_context *ctxt, \
                                                   gcc_jit_type *numeric_type, \
                                                   int value)

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :expr:`int` value.

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_rvalue_from_long (gcc_jit_context *ctxt, \
                                                    gcc_jit_type *numeric_type, \
                                                    long value)

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :expr:`long` value.

.. function::  gcc_jit_rvalue *gcc_jit_context_zero (gcc_jit_context *ctxt, \
                                                     gcc_jit_type *numeric_type)

   Given a numeric type (integer or floating point), get the rvalue for
   zero.  Essentially this is just a shortcut for:

   .. code-block:: c

      gcc_jit_context_new_rvalue_from_int (ctxt, numeric_type, 0)

.. function::  gcc_jit_rvalue *gcc_jit_context_one (gcc_jit_context *ctxt, \
                                                    gcc_jit_type *numeric_type)

   Given a numeric type (integer or floating point), get the rvalue for
   one.  Essentially this is just a shortcut for:

   .. code-block:: c

      gcc_jit_context_new_rvalue_from_int (ctxt, numeric_type, 1)

.. function::  gcc_jit_rvalue *\
               gcc_jit_context_new_rvalue_from_double (gcc_jit_context *ctxt, \
                                                       gcc_jit_type *numeric_type, \
                                                       double value)

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :expr:`double` value.

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_rvalue_from_ptr (gcc_jit_context *ctxt, \
                                                   gcc_jit_type *pointer_type, \
                                                   void *value)

   Given a pointer type, build an rvalue for the given address.

.. function:: gcc_jit_rvalue *gcc_jit_context_null (gcc_jit_context *ctxt, \
                                                    gcc_jit_type *pointer_type)

   Given a pointer type, build an rvalue for ``NULL``.  Essentially this
   is just a shortcut for:

   .. code-block:: c

      gcc_jit_context_new_rvalue_from_ptr (ctxt, pointer_type, NULL)

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_string_literal (gcc_jit_context *ctxt, \
                                                  const char *value)

   Generate an rvalue for the given NIL-terminated string, of type
   :c:data:`GCC_JIT_TYPE_CONST_CHAR_PTR`.

   The parameter ``value`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_sizeof (gcc_jit_context *ctxt, \
                                          gcc_jit_type *type)

   Generate an rvalue that is equal to the size of ``type``.

   The parameter ``type`` must be non-NULL.

   This is equivalent to this C code:

   .. code-block:: c

     sizeof (type)

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_alignof (gcc_jit_context *ctxt, \
                                           gcc_jit_type *type)

   Generate an rvalue that is equal to the alignment of ``type``.

   The parameter ``type`` must be non-NULL.

   This is equivalent to this C code:

   .. code-block:: c

     _Alignof (type)

Constructor expressions
***********************

   The following functions make constructors for array, struct and union
   types.

   The constructor rvalue can be used for assignment to locals.
   It can be used to initialize global variables with
   :func:`gcc_jit_global_set_initializer_rvalue`. It can also be used as a
   temporary value for function calls and return values, but its address
   can't be taken.

   Note that arrays in libgccjit do not collapse to pointers like in
   C. I.e. if an array constructor is used as e.g. a return value, the whole
   array would be returned by value - array constructors can be assigned to
   array variables.

   The constructor can contain nested constructors.

   Note that a string literal rvalue can't be used to construct a char array;
   the latter needs one rvalue for each char.

   These entrypoints were added in :ref:`LIBGCCJIT_ABI_19`; you can test for
   their presence using:

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_CTORS

.. function:: gcc_jit_rvalue *\
	      gcc_jit_context_new_array_constructor (gcc_jit_context *ctxt,\
						     gcc_jit_location *loc,\
						     gcc_jit_type *type,\
						     size_t num_values,\
						     gcc_jit_rvalue **values)

   Create a constructor for an array as an rvalue.

   Returns NULL on error. ``values`` are copied and
   do not have to outlive the context.

   ``type`` specifies what the constructor will build and has to be
   an array.

   ``num_values`` specifies the number of elements in ``values`` and
   it can't have more elements than the array type.

   Each value in ``values`` sets the corresponding value in the array.
   If the array type itself has more elements than ``values``, the
   left-over elements will be zeroed.

   Each value in ``values`` need to be the same unqualified type as the
   array type's element type.

   If ``num_values`` is 0, the ``values`` parameter will be
   ignored and zero initialization will be used.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_19`; you can test for its
   presence using:

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_CTORS

.. function:: gcc_jit_rvalue *\
	      gcc_jit_context_new_struct_constructor (gcc_jit_context *ctxt,\
						      gcc_jit_location *loc,\
						      gcc_jit_type *type,\
						      size_t num_values,\
						      gcc_jit_field **fields,\
						      gcc_jit_rvalue **values)


   Create a constructor for a struct as an rvalue.

   Returns NULL on error. The two parameter arrays are copied and
   do not have to outlive the context.

   ``type`` specifies what the constructor will build and has to be
   a struct.

   ``num_values`` specifies the number of elements in ``values``.

   ``fields`` need to have the same length as ``values``, or be NULL.

   If ``fields`` is null, the values are applied in definition order.

   Otherwise, each field in ``fields`` specifies which field in the struct to
   set to the corresponding value in ``values``. ``fields`` and ``values``
   are paired by index.

   The fields in ``fields`` have to be in definition order, but there
   can be gaps. Any field in the struct that is not specified in
   ``fields`` will be zeroed.

   The fields in ``fields`` need to be the same objects that were used
   to create the struct.

   Each value has to have the same unqualified type as the field
   it is applied to.

   A NULL value element  in ``values`` is a shorthand for zero initialization
   of the corresponding field.

   If ``num_values`` is 0, the array parameters will be
   ignored and zero initialization will be used.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_19`; you can test for its
   presence using:

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_CTORS

.. function:: gcc_jit_rvalue *\
	      gcc_jit_context_new_union_constructor (gcc_jit_context *ctxt,\
						     gcc_jit_location *loc,\
						     gcc_jit_type *type,\
						     gcc_jit_field *field,\
						     gcc_jit_rvalue *value)

   Create a constructor for a union as an rvalue.

   Returns NULL on error.

   ``type`` specifies what the constructor will build and has to be
   an union.

   ``field`` specifies which field to set. If it is NULL, the first
   field in the union will be set.``field`` need to be the same object
   that were used to create the union.

   ``value`` specifies what value to set the corresponding field to.
   If ``value`` is NULL, zero initialization will be used.

   Each value has to have have the same unqualified type as the field
   it is applied to.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_19`; you can test for its
   presence using:

   .. code-block:: c

     #ifdef LIBGCCJIT_HAVE_CTORS

Vector expressions
******************

.. function:: gcc_jit_rvalue * \
              gcc_jit_context_new_rvalue_from_vector (gcc_jit_context *ctxt, \
                                                      gcc_jit_location *loc, \
                                                      gcc_jit_type *vec_type, \
                                                      size_t num_elements, \
                                                      gcc_jit_rvalue **elements)

   Build a vector rvalue from an array of elements.

   "vec_type" should be a vector type, created using
   :func:`gcc_jit_type_get_vector`.

   "num_elements" should match that of the vector type.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_10`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_rvalue_from_vector

.. function:: gcc_jit_rvalue * \
              gcc_jit_context_new_rvalue_vector_perm (gcc_jit_context *ctxt, \
                                                      gcc_jit_location *loc, \
                                                      gcc_jit_rvalue *elements1, \
                                                      gcc_jit_rvalue *elements2, \
                                                      gcc_jit_rvalue *mask);

   Build a permutation of two vectors.

   "elements1" and "elements2" should have the same type.
   The length of "mask" and "elements1" should be the same.
   The element type of "mask" should be integral.
   The size of the element type of "mask" and "elements1" should be the same.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_31`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_VECTOR_OPERATIONS

    Analogous to:

    .. code-block:: c

       __builtin_shuffle (elements1, elements2, mask)

    in C.

Unary Operations
****************

.. function:: gcc_jit_rvalue * \
              gcc_jit_context_new_unary_op (gcc_jit_context *ctxt, \
                                            gcc_jit_location *loc, \
                                            enum gcc_jit_unary_op op, \
                                            gcc_jit_type *result_type, \
                                            gcc_jit_rvalue *rvalue)

   Build a unary operation out of an input rvalue.

   The parameter ``result_type`` must be a numeric type.

.. enum:: gcc_jit_unary_op

The available unary operations are:

.. list-table::
   :header-rows: 1

   * - Unary Operation
     - C equivalent

   * - :c:macro:`GCC_JIT_UNARY_OP_MINUS`
     - `-(EXPR)`
   * - :c:macro:`GCC_JIT_UNARY_OP_BITWISE_NEGATE`
     - `~(EXPR)`
   * - :c:macro:`GCC_JIT_UNARY_OP_LOGICAL_NEGATE`
     - `!(EXPR)`
   * - :c:macro:`GCC_JIT_UNARY_OP_ABS`
     - `abs (EXPR)`

.. c:macro:: GCC_JIT_UNARY_OP_MINUS

    Negate an arithmetic value; analogous to:

    .. code-block:: c

       -(EXPR)

    in C.

.. c:macro:: GCC_JIT_UNARY_OP_BITWISE_NEGATE

    Bitwise negation of an integer value (one's complement); analogous
    to:

    .. code-block:: c

       ~(EXPR)

    in C.

.. c:macro:: GCC_JIT_UNARY_OP_LOGICAL_NEGATE

    Logical negation of an arithmetic or pointer value; analogous to:

    .. code-block:: c

       !(EXPR)

    in C.

.. c:macro:: GCC_JIT_UNARY_OP_ABS

    Absolute value of an arithmetic expression; analogous to:

    .. code-block:: c

        abs (EXPR)

    in C.

Binary Operations
*****************

.. function:: gcc_jit_rvalue *gcc_jit_context_new_binary_op (gcc_jit_context *ctxt, \
                                                             gcc_jit_location *loc, \
                                                             enum gcc_jit_binary_op op, \
                                                             gcc_jit_type *result_type, \
                                                             gcc_jit_rvalue *a, gcc_jit_rvalue *b)

   Build a binary operation out of two constituent rvalues.

   The parameter ``result_type`` must be a numeric type.

.. enum:: gcc_jit_binary_op

The available binary operations are:

.. list-table::
   :header-rows: 1

   * - Binary Operation
     - C equivalent

   * - :c:macro:`GCC_JIT_BINARY_OP_PLUS`
     - `x + y`
   * - :c:macro:`GCC_JIT_BINARY_OP_MINUS`
     - `x - y`
   * - :c:macro:`GCC_JIT_BINARY_OP_MULT`
     - `x * y`
   * - :c:macro:`GCC_JIT_BINARY_OP_DIVIDE`
     - `x / y`
   * - :c:macro:`GCC_JIT_BINARY_OP_MODULO`
     - `x % y`
   * - :c:macro:`GCC_JIT_BINARY_OP_BITWISE_AND`
     - `x & y`
   * - :c:macro:`GCC_JIT_BINARY_OP_BITWISE_XOR`
     - `x ^ y`
   * - :c:macro:`GCC_JIT_BINARY_OP_BITWISE_OR`
     - `x | y`
   * - :c:macro:`GCC_JIT_BINARY_OP_LOGICAL_AND`
     - `x && y`
   * - :c:macro:`GCC_JIT_BINARY_OP_LOGICAL_OR`
     - `x || y`
   * - :c:macro:`GCC_JIT_BINARY_OP_LSHIFT`
     - `x << y`
   * - :c:macro:`GCC_JIT_BINARY_OP_RSHIFT`
     - `x >> y`

.. c:macro:: GCC_JIT_BINARY_OP_PLUS

   Addition of arithmetic values; analogous to:

   .. code-block:: c

     (EXPR_A) + (EXPR_B)

   in C.

   For pointer addition, use :c:func:`gcc_jit_context_new_array_access`.

.. c:macro:: GCC_JIT_BINARY_OP_MINUS

   Subtraction of arithmetic values; analogous to:

   .. code-block:: c

     (EXPR_A) - (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_MULT

   Multiplication of a pair of arithmetic values; analogous to:

   .. code-block:: c

     (EXPR_A) * (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_DIVIDE

   Quotient of division of arithmetic values; analogous to:

   .. code-block:: c

     (EXPR_A) / (EXPR_B)

   in C.

   The result type affects the kind of division: if the result type is
   integer-based, then the result is truncated towards zero, whereas
   a floating-point result type indicates floating-point division.

.. c:macro:: GCC_JIT_BINARY_OP_MODULO

   Remainder of division of arithmetic values; analogous to:

   .. code-block:: c

     (EXPR_A) % (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_BITWISE_AND

   Bitwise AND; analogous to:

   .. code-block:: c

     (EXPR_A) & (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_BITWISE_XOR

   Bitwise exclusive OR; analogous to:

   .. code-block:: c

      (EXPR_A) ^ (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_BITWISE_OR

   Bitwise inclusive OR; analogous to:

   .. code-block:: c

     (EXPR_A) | (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_LOGICAL_AND

   Logical AND; analogous to:

   .. code-block:: c

     (EXPR_A) && (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_LOGICAL_OR

   Logical OR; analogous to:

   .. code-block:: c

     (EXPR_A) || (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_LSHIFT

   Left shift; analogous to:

   .. code-block:: c

     (EXPR_A) << (EXPR_B)

   in C.

.. c:macro:: GCC_JIT_BINARY_OP_RSHIFT

   Right shift; analogous to:

   .. code-block:: c

     (EXPR_A) >> (EXPR_B)

   in C.

Comparisons
***********

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_comparison (gcc_jit_context *ctxt,\
                                              gcc_jit_location *loc,\
                                              enum gcc_jit_comparison op,\
                                              gcc_jit_rvalue *a, gcc_jit_rvalue *b)

   Build a boolean rvalue out of the comparison of two other rvalues.

.. enum:: gcc_jit_comparison

.. list-table::
   :header-rows: 1

   * - Comparison
     - C equivalent

   * - :c:macro:`GCC_JIT_COMPARISON_EQ`
     - `x == y`
   * - :c:macro:`GCC_JIT_COMPARISON_NE`
     - `x != y`
   * - :c:macro:`GCC_JIT_COMPARISON_LT`
     - `x < y`
   * - :c:macro:`GCC_JIT_COMPARISON_LE`
     - `x <= y`
   * - :c:macro:`GCC_JIT_COMPARISON_GT`
     - `x > y`
   * - :c:macro:`GCC_JIT_COMPARISON_GE`
     - `x >= y`

Function calls
**************
.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_call (gcc_jit_context *ctxt,\
                                        gcc_jit_location *loc,\
                                        gcc_jit_function *func,\
                                        int numargs , gcc_jit_rvalue **args)

   Given a function and the given table of argument rvalues, construct a
   call to the function, with the result as an rvalue.

   .. note::

      :c:func:`gcc_jit_context_new_call` merely builds a
      :c:type:`gcc_jit_rvalue` i.e. an expression that can be evaluated,
      perhaps as part of a more complicated expression.
      The call *won't* happen unless you add a statement to a function
      that evaluates the expression.

      For example, if you want to call a function and discard the result
      (or to call a function with ``void`` return type), use
      :c:func:`gcc_jit_block_add_eval`:

      .. code-block:: c

         /* Add "(void)printf (arg0, arg1);".  */
         gcc_jit_block_add_eval (
           block, NULL,
           gcc_jit_context_new_call (
             ctxt,
             NULL,
             printf_func,
             2, args));

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_call_through_ptr (gcc_jit_context *ctxt,\
                                                    gcc_jit_location *loc,\
                                                    gcc_jit_rvalue *fn_ptr,\
                                                    int numargs, \
                                                    gcc_jit_rvalue **args)

   Given an rvalue of function pointer type (e.g. from
   :c:func:`gcc_jit_context_new_function_ptr_type`), and the given table of
   argument rvalues, construct a call to the function pointer, with the
   result as an rvalue.

   .. note::

      The same caveat as for :c:func:`gcc_jit_context_new_call` applies.

.. function:: void\
              gcc_jit_rvalue_set_bool_require_tail_call (gcc_jit_rvalue *call,\
                                                         int require_tail_call)

   Given an :c:type:`gcc_jit_rvalue` for a call created through
   :c:func:`gcc_jit_context_new_call` or
   :c:func:`gcc_jit_context_new_call_through_ptr`, mark/clear the
   call as needing tail-call optimization.  The optimizer will
   attempt to optimize the call into a jump instruction; if it is
   unable to do do, an error will be emitted.

   This may be useful when implementing functions that use the
   continuation-passing style (e.g. for functional programming
   languages), in which every function "returns" by calling a
   "continuation" function pointer.  This call must be
   guaranteed to be implemented as a jump, otherwise the program
   could consume an arbitrary amount of stack space as it executed.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_6`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_rvalue_set_bool_require_tail_call

Function pointers
*****************

Function pointers can be obtained:

  * from a :c:type:`gcc_jit_function` using
    :c:func:`gcc_jit_function_get_address`, or

  * from an existing function using
    :c:func:`gcc_jit_context_new_rvalue_from_ptr`,
    using a function pointer type obtained using
    :c:func:`gcc_jit_context_new_function_ptr_type`.

Type-coercion
*************

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_cast (gcc_jit_context *ctxt,\
                                        gcc_jit_location *loc,\
                                        gcc_jit_rvalue *rvalue,\
                                        gcc_jit_type *type)

   Given an rvalue of T, construct another rvalue of another type.

   Currently only a limited set of conversions are possible:

     * int <-> float
     * int <-> bool
     * P*  <-> Q*, for pointer types P and Q

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_bitcast (gcc_jit_context *ctxt,\
                                           gcc_jit_location *loc,\
                                           gcc_jit_rvalue *rvalue,\
                                           gcc_jit_type *type)

   Given an rvalue of T, bitcast it to another type, meaning that this will
   generate a new rvalue by interpreting the bits of ``rvalue`` to the layout
   of ``type``.

   The type of rvalue must be the same size as the size of ``type``.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_21`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitcast

.. function:: gcc_jit_rvalue *
              gcc_jit_context_convert_vector (gcc_jit_context *ctxt, \
                                              gcc_jit_location *loc, \
                                              gcc_jit_rvalue *vector, \
                                              gcc_jit_type *type)

   Given a vector rvalue, cast it to the type ``type``, doing an element-wise
   conversion.

   The number of elements in ``vector`` and ``type`` must match.
   The ``type`` must be a vector type.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_30`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_convert_vector

Lvalues
-------

.. type:: gcc_jit_lvalue

An lvalue is something that can of the *left*-hand side of an assignment:
a storage area (such as a variable).  It is also usable as an rvalue,
where the rvalue is computed by reading from the storage area.

.. function:: gcc_jit_object *\
              gcc_jit_lvalue_as_object (gcc_jit_lvalue *lvalue)

   Upcast an lvalue to be an object.

.. function:: gcc_jit_rvalue *\
              gcc_jit_lvalue_as_rvalue (gcc_jit_lvalue *lvalue)

   Upcast an lvalue to be an rvalue.

.. function:: gcc_jit_rvalue *\
              gcc_jit_lvalue_get_address (gcc_jit_lvalue *lvalue,\
                                          gcc_jit_location *loc)

   Take the address of an lvalue; analogous to:

   .. code-block:: c

     &(EXPR)

   in C.

.. function:: void\
              gcc_jit_lvalue_set_tls_model (gcc_jit_lvalue *lvalue,\
                                            enum gcc_jit_tls_model model)

   Make a variable a thread-local variable.

   The "model" parameter determines the thread-local storage model of the "lvalue":

   .. enum:: gcc_jit_tls_model

   .. c:macro:: GCC_JIT_TLS_MODEL_NONE

      Don't set the TLS model.

   .. c:macro:: GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC

   .. c:macro:: GCC_JIT_TLS_MODEL_LOCAL_DYNAMIC

   .. c:macro:: GCC_JIT_TLS_MODEL_INITIAL_EXEC

   .. c:macro:: GCC_JIT_TLS_MODEL_LOCAL_EXEC

   This is analogous to:

   .. code-block:: c

     _Thread_local int foo __attribute__ ((tls_model("MODEL")));

   in C.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_17`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_tls_model

.. function:: void\
              gcc_jit_lvalue_set_link_section (gcc_jit_lvalue *lvalue,\
                                               const char *section_name)

   Set the link section of a variable.
   The parameter ``section_name`` must be non-NULL and must contain the
   leading dot. Analogous to:

   .. code-block:: c

     int variable __attribute__((section(".section")));

   in C.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_18`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_link_section

.. function:: void\
              gcc_jit_lvalue_set_register_name (gcc_jit_lvalue *lvalue,\
                                                const char *reg_name);

   Set the register name of a variable.
   The parameter ``reg_name`` must be non-NULL. Analogous to:

   .. code-block:: c

     register int variable asm ("r12");

   in C.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_22`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_lvalue_set_register_name

.. function:: void\
              gcc_jit_lvalue_set_alignment (gcc_jit_lvalue *lvalue,\
                                            unsigned bytes)

   Set the alignment of a variable, in bytes.
   Analogous to:

   .. code-block:: c

     int variable __attribute__((aligned (16)));

   in C.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_24`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_ALIGNMENT

.. function:: unsigned\
              gcc_jit_lvalue_get_alignment (gcc_jit_lvalue *lvalue)

   Return the alignment of a variable set by ``gcc_jit_lvalue_set_alignment``.
   Return 0 if the alignment was not set. Analogous to:

   .. code-block:: c

     _Alignof (variable)

   in C.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_24`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_ALIGNMENT

Global variables
****************

.. function:: gcc_jit_lvalue *\
              gcc_jit_context_new_global (gcc_jit_context *ctxt,\
                                          gcc_jit_location *loc,\
                                          enum gcc_jit_global_kind kind,\
                                          gcc_jit_type *type,\
                                          const char *name)

   Add a new global variable of the given type and name to the context.

   The parameter ``type`` must be non-`void`.

   The parameter ``name`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

   The "kind" parameter determines the visibility of the "global" outside
   of the :c:type:`gcc_jit_result`:

   .. enum:: gcc_jit_global_kind

   .. c:macro:: GCC_JIT_GLOBAL_EXPORTED

      Global is defined by the client code and is visible
      by name outside of this JIT context via
      :c:func:`gcc_jit_result_get_global` (and this value is required for
      the global to be accessible via that entrypoint).

   .. c:macro:: GCC_JIT_GLOBAL_INTERNAL

      Global is defined by the client code, but is invisible
      outside of it.  Analogous to a "static" global within a .c file.
      Specifically, the variable will only be visible within this
      context and within child contexts.

   .. c:macro:: GCC_JIT_GLOBAL_IMPORTED

      Global is not defined by the client code; we're merely
      referring to it.  Analogous to using an "extern" global from a
      header file.

.. function:: gcc_jit_lvalue *\
              gcc_jit_global_set_initializer (gcc_jit_lvalue *global,\
                                              const void *blob,\
                                              size_t num_bytes)

   Set an initializer for ``global`` using the memory content pointed
   by ``blob`` for ``num_bytes``.  ``global`` must be an array of an
   integral type.  Return the global itself.

   The parameter ``blob`` must be non-NULL. The call copies the memory
   pointed by ``blob`` for ``num_bytes`` bytes, so it is valid to pass
   in a pointer to an on-stack buffer.  The content will be stored in
   the compilation unit and used as initialization value of the array.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_14`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_global_set_initializer

.. function:: gcc_jit_lvalue *\
	      gcc_jit_global_set_initializer_rvalue (gcc_jit_lvalue *global,\
	                                             gcc_jit_rvalue *init_value)

   Set the initial value of a global with an rvalue.

   The rvalue needs to be a constant expression, e.g. no function calls.

   The global can't have the ``kind`` :c:macro:`GCC_JIT_GLOBAL_IMPORTED`.

   As a non-comprehensive example it is OK to do the equivalent of:

   .. code-block:: c

       int foo = 3 * 2; /* rvalue from gcc_jit_context_new_binary_op.  */
       int arr[] = {1,2,3,4}; /* rvalue from gcc_jit_context_new_constructor.  */
       int *bar = &arr[2] + 1; /* rvalue from nested "get address" of "array access".  */
       const int baz = 3; /* rvalue from gcc_jit_context_rvalue_from_int.  */
       int boz = baz; /* rvalue from gcc_jit_lvalue_as_rvalue.  */

   Use together with :c:func:`gcc_jit_context_new_struct_constructor`,
   :c:func:`gcc_jit_context_new_union_constructor`, :c:func:`gcc_jit_context_new_array_constructor`
   to initialize structs, unions and arrays.

   On success, returns the ``global`` parameter unchanged. Otherwise, ``NULL``.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_19`; you can test for its
   presence using:

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_CTORS

Variables
*********

.. function::  void\
               gcc_jit_lvalue_add_string_attribute (gcc_jit_lvalue *variable,
                                                    enum gcc_jit_variable_attribute attribute,
                                                    const char *value)

     Add an attribute ``attribute`` with value ``value`` to a variable ``variable``.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_26`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_ATTRIBUTES

.. function:: void\
              gcc_jit_global_set_readonly (gcc_jit_lvalue *global)

   Set the global variable as read-only, meaning you cannot assign to this variable.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_29`; you can test for its
   presence using:

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_global_set_readonly

Working with pointers, structs and unions
-----------------------------------------

.. function:: gcc_jit_lvalue *\
              gcc_jit_rvalue_dereference (gcc_jit_rvalue *rvalue,\
                                          gcc_jit_location *loc)

   Given an rvalue of pointer type ``T *``, dereferencing the pointer,
   getting an lvalue of type ``T``.  Analogous to:

   .. code-block:: c

     *(EXPR)

   in C.

Field access is provided separately for both lvalues and rvalues.

.. function:: gcc_jit_lvalue *\
              gcc_jit_lvalue_access_field (gcc_jit_lvalue *struct_,\
                                           gcc_jit_location *loc,\
                                           gcc_jit_field *field)

   Given an lvalue of struct or union type, access the given field,
   getting an lvalue of the field's type.  Analogous to:

   .. code-block:: c

      (EXPR).field = ...;

   in C.

.. function:: gcc_jit_rvalue *\
              gcc_jit_rvalue_access_field (gcc_jit_rvalue *struct_,\
                                           gcc_jit_location *loc,\
                                           gcc_jit_field *field)

   Given an rvalue of struct or union type, access the given field
   as an rvalue.  Analogous to:

   .. code-block:: c

      (EXPR).field

   in C.

.. function:: gcc_jit_lvalue *\
              gcc_jit_rvalue_dereference_field (gcc_jit_rvalue *ptr,\
                                                gcc_jit_location *loc,\
                                                gcc_jit_field *field)

   Given an rvalue of pointer type ``T *`` where T is of struct or union
   type, access the given field as an lvalue.  Analogous to:

   .. code-block:: c

      (EXPR)->field

   in C, itself equivalent to ``(*EXPR).FIELD``.

.. function:: gcc_jit_lvalue *\
              gcc_jit_context_new_array_access (gcc_jit_context *ctxt,\
                                                gcc_jit_location *loc,\
                                                gcc_jit_rvalue *ptr,\
                                                gcc_jit_rvalue *index)

   Given an rvalue of pointer type ``T *``, get at the element `T` at
   the given index, using standard C array indexing rules i.e. each
   increment of ``index`` corresponds to ``sizeof(T)`` bytes.
   Analogous to:

   .. code-block:: c

      PTR[INDEX]

   in C (or, indeed, to ``PTR + INDEX``).

.. function:: gcc_jit_lvalue *\
              gcc_jit_context_new_vector_access (gcc_jit_context *ctxt,\
                                                 gcc_jit_location *loc,\
                                                 gcc_jit_rvalue *vector,\
                                                 gcc_jit_rvalue *index)

   Given an rvalue of vector type ``T __attribute__ ((__vector_size__ (SIZE)))``,
   get the element `T` at the given index.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_31`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_VECTOR_OPERATIONS

   Analogous to:

   .. code-block:: c

      VECTOR[INDEX]

   in C.
