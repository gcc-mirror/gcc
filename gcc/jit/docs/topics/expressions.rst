.. Copyright (C) 2014-2021 Free Software Foundation, Inc.
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
   <http://www.gnu.org/licenses/>.

.. default-domain:: c

Expressions
===========

Rvalues
-------
.. type:: gcc_jit_rvalue

A :c:type:`gcc_jit_rvalue *` is an expression that can be computed.

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
   the given constant :c:type:`int` value.

.. function:: gcc_jit_rvalue *\
              gcc_jit_context_new_rvalue_from_long (gcc_jit_context *ctxt, \
                                                    gcc_jit_type *numeric_type, \
                                                    long value)

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :c:type:`long` value.

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
   the given constant :c:type:`double` value.

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

.. type:: enum gcc_jit_unary_op

The available unary operations are:

==========================================  ============
Unary Operation                             C equivalent
==========================================  ============
:c:macro:`GCC_JIT_UNARY_OP_MINUS`           `-(EXPR)`
:c:macro:`GCC_JIT_UNARY_OP_BITWISE_NEGATE`  `~(EXPR)`
:c:macro:`GCC_JIT_UNARY_OP_LOGICAL_NEGATE`  `!(EXPR)`
:c:macro:`GCC_JIT_UNARY_OP_ABS`             `abs (EXPR)`
==========================================  ============

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

.. type:: enum gcc_jit_binary_op

The available binary operations are:

========================================  ============
Binary Operation                          C equivalent
========================================  ============
:c:macro:`GCC_JIT_BINARY_OP_PLUS`         `x + y`
:c:macro:`GCC_JIT_BINARY_OP_MINUS`        `x - y`
:c:macro:`GCC_JIT_BINARY_OP_MULT`         `x * y`
:c:macro:`GCC_JIT_BINARY_OP_DIVIDE`       `x / y`
:c:macro:`GCC_JIT_BINARY_OP_MODULO`       `x % y`
:c:macro:`GCC_JIT_BINARY_OP_BITWISE_AND`  `x & y`
:c:macro:`GCC_JIT_BINARY_OP_BITWISE_XOR`  `x ^ y`
:c:macro:`GCC_JIT_BINARY_OP_BITWISE_OR`   `x | y`
:c:macro:`GCC_JIT_BINARY_OP_LOGICAL_AND`  `x && y`
:c:macro:`GCC_JIT_BINARY_OP_LOGICAL_OR`   `x || y`
:c:macro:`GCC_JIT_BINARY_OP_LSHIFT`       `x << y`
:c:macro:`GCC_JIT_BINARY_OP_RSHIFT`       `x >> y`
========================================  ============

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

.. type:: enum gcc_jit_comparison

=======================================  ============
Comparison                               C equivalent
=======================================  ============
:c:macro:`GCC_JIT_COMPARISON_EQ`         `x == y`
:c:macro:`GCC_JIT_COMPARISON_NE`         `x != y`
:c:macro:`GCC_JIT_COMPARISON_LT`         `x < y`
:c:macro:`GCC_JIT_COMPARISON_LE`         `x <= y`
:c:macro:`GCC_JIT_COMPARISON_GT`         `x > y`
:c:macro:`GCC_JIT_COMPARISON_GE`         `x >= y`
=======================================  ============


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

   Given an :c:type:`gcc_jit_rvalue *` for a call created through
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

   .. type:: enum gcc_jit_global_kind

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
