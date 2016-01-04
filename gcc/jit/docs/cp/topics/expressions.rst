.. Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

.. default-domain:: cpp

Expressions
===========

Rvalues
-------
.. class:: gccjit::rvalue

A :class:`gccjit::rvalue` is an expression that can be computed.  It is a
subclass of :class:`gccjit::object`, and is a thin wrapper around
:c:type:`gcc_jit_rvalue *` from the C API.

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

.. function:: gccjit::type gccjit::rvalue::get_type ()

  Get the type of this rvalue.


Simple expressions
******************

.. function:: gccjit::rvalue \
              gccjit::context::new_rvalue (gccjit::type numeric_type, \
                                           int value) const

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :c:type:`int` value.

.. function:: gccjit::rvalue \
              gccjit::context::new_rvalue (gccjit::type numeric_type, \
                                           long value) const

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :c:type:`long` value.

.. function::  gccjit::rvalue \
               gccjit::context::zero (gccjit::type numeric_type) const

   Given a numeric type (integer or floating point), get the rvalue for
   zero.  Essentially this is just a shortcut for:

   .. code-block:: c++

      ctxt.new_rvalue (numeric_type, 0)

.. function::  gccjit::rvalue \
               gccjit::context::one (gccjit::type numeric_type) const

   Given a numeric type (integer or floating point), get the rvalue for
   one.  Essentially this is just a shortcut for:

   .. code-block:: c++

      ctxt.new_rvalue (numeric_type, 1)

.. function::  gccjit::rvalue \
               gccjit::context::new_rvalue (gccjit::type numeric_type, \
                                            double value) const

   Given a numeric type (integer or floating point), build an rvalue for
   the given constant :c:type:`double` value.

.. function:: gccjit::rvalue \
              gccjit::context::new_rvalue (gccjit::type pointer_type, \
                                           void *value) const

   Given a pointer type, build an rvalue for the given address.

.. function:: gccjit::rvalue \
              gccjit::context::new_rvalue (const std::string &value) const

   Generate an rvalue of type :c:data:`GCC_JIT_TYPE_CONST_CHAR_PTR` for
   the given string.  This is akin to a string literal.


Unary Operations
****************

.. function:: gccjit::rvalue  \
              gccjit::context::new_unary_op (enum gcc_jit_unary_op, \
                                             gccjit::type result_type, \
                                             gccjit::rvalue rvalue, \
                                             gccjit::location loc)

   Build a unary operation out of an input rvalue.

   Parameter ``loc`` is optional.

   This is a thin wrapper around the C API's
   :c:func:`gcc_jit_context_new_unary_op` and the available unary
   operations are documented there.

There are shorter ways to spell the various specific kinds of unary
operation:

.. function:: gccjit::rvalue \
              gccjit::context::new_minus (gccjit::type result_type, \
                                          gccjit::rvalue a, \
                                          gccjit::location loc)

   Negate an arithmetic value; for example:

   .. code-block:: c++

      gccjit::rvalue negpi = ctxt.new_minus (t_double, pi);

   builds the equivalent of this C expression:

   .. code-block:: c

      -pi

.. function:: gccjit::rvalue \
              new_bitwise_negate (gccjit::type result_type, \
                                  gccjit::rvalue a, \
                                  gccjit::location loc)

   Bitwise negation of an integer value (one's complement); for example:

   .. code-block:: c++

      gccjit::rvalue mask = ctxt.new_bitwise_negate (t_int, a);

   builds the equivalent of this C expression:

   .. code-block:: c

      ~a

.. function:: gccjit::rvalue \
              new_logical_negate (gccjit::type result_type, \
                                  gccjit::rvalue a, \
                                  gccjit::location loc)

   Logical negation of an arithmetic or pointer value; for example:

   .. code-block:: c++

      gccjit::rvalue guard = ctxt.new_logical_negate (t_bool, cond);

   builds the equivalent of this C expression:

   .. code-block:: c

      !cond


The most concise way to spell them is with overloaded operators:

.. function:: gccjit::rvalue operator- (gccjit::rvalue a)

   .. code-block:: c++

     gccjit::rvalue negpi = -pi;


.. function:: gccjit::rvalue operator~ (gccjit::rvalue a)

   .. code-block:: c++

      gccjit::rvalue mask = ~a;

.. function:: gccjit::rvalue operator! (gccjit::rvalue a)

   .. code-block:: c++

      gccjit::rvalue guard = !cond;


Binary Operations
*****************

.. function:: gccjit::rvalue\
              gccjit::context::new_binary_op (enum gcc_jit_binary_op, \
                                              gccjit::type result_type, \
                                              gccjit::rvalue a, \
                                              gccjit::rvalue b, \
                                              gccjit::location loc)

   Build a binary operation out of two constituent rvalues.

   Parameter ``loc`` is optional.

   This is a thin wrapper around the C API's
   :c:func:`gcc_jit_context_new_binary_op` and the available binary
   operations are documented there.

There are shorter ways to spell the various specific kinds of binary
operation:

.. function:: gccjit::rvalue \
              gccjit::context::new_plus (gccjit::type result_type, \
                                         gccjit::rvalue a, gccjit::rvalue b, \
                                         gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_minus (gccjit::type result_type, \
                                          gccjit::rvalue a, gccjit::rvalue b, \
                                          gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_mult (gccjit::type result_type, \
                                         gccjit::rvalue a, gccjit::rvalue b, \
                                         gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_divide (gccjit::type result_type, \
                                           gccjit::rvalue a, gccjit::rvalue b, \
                                           gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_modulo (gccjit::type result_type, \
                                           gccjit::rvalue a, gccjit::rvalue b, \
                                           gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_bitwise_and (gccjit::type result_type, \
                                                gccjit::rvalue a, gccjit::rvalue b, \
                                                gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_bitwise_xor (gccjit::type result_type, \
                                                gccjit::rvalue a, gccjit::rvalue b, \
                                                gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_bitwise_or (gccjit::type result_type, \
                                               gccjit::rvalue a, gccjit::rvalue b, \
                                               gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_logical_and (gccjit::type result_type, \
                                                gccjit::rvalue a, gccjit::rvalue b, \
                                                gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_logical_or (gccjit::type result_type, \
                                               gccjit::rvalue a, gccjit::rvalue b, \
                                               gccjit::location loc)

The most concise way to spell them is with overloaded operators:

.. function:: gccjit::rvalue operator+ (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue sum = a + b;

.. function:: gccjit::rvalue operator- (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue diff = a - b;

.. function:: gccjit::rvalue operator* (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue prod = a * b;

.. function:: gccjit::rvalue operator/ (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue result = a / b;

.. function:: gccjit::rvalue operator% (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue mod = a % b;

.. function:: gccjit::rvalue operator& (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue x = a & b;

.. function:: gccjit::rvalue operator^ (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue x = a ^ b;

.. function:: gccjit::rvalue operator| (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue x = a | b;

.. function:: gccjit::rvalue operator&& (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = a && b;

.. function:: gccjit::rvalue operator|| (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = a || b;

These can of course be combined, giving a terse way to build compound
expressions:

   .. code-block:: c++

      gccjit::rvalue discriminant = (b * b) - (four * a * c);


Comparisons
***********

.. function:: gccjit::rvalue \
              gccjit::context::new_comparison (enum gcc_jit_comparison,\
                                               gccjit::rvalue a, \
                                               gccjit::rvalue b, \
                                               gccjit::location loc)

   Build a boolean rvalue out of the comparison of two other rvalues.

   Parameter ``loc`` is optional.

   This is a thin wrapper around the C API's
   :c:func:`gcc_jit_context_new_comparison` and the available kinds
   of comparison are documented there.

There are shorter ways to spell the various specific kinds of binary
operation:

.. function:: gccjit::rvalue \
              gccjit::context::new_eq (gccjit::rvalue a, gccjit::rvalue b, \
                                       gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_ne (gccjit::rvalue a, gccjit::rvalue b, \
                                       gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_lt (gccjit::rvalue a, gccjit::rvalue b, \
                                       gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_le (gccjit::rvalue a, gccjit::rvalue b, \
                                       gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_gt (gccjit::rvalue a, gccjit::rvalue b, \
                                       gccjit::location loc)

.. function:: gccjit::rvalue \
              gccjit::context::new_ge (gccjit::rvalue a, gccjit::rvalue b, \
                                       gccjit::location loc)

The most concise way to spell them is with overloaded operators:

.. function:: gccjit::rvalue \
              operator== (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = (a == ctxt.zero (t_int));

.. function:: gccjit::rvalue \
              operator!= (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = (i != j);

.. function:: gccjit::rvalue \
              operator< (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = i < n;

.. function:: gccjit::rvalue \
              operator<= (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = i <= n;

.. function:: gccjit::rvalue \
              operator> (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = (ch > limit);

.. function:: gccjit::rvalue \
              operator>= (gccjit::rvalue a, gccjit::rvalue b)

   .. code-block:: c++

      gccjit::rvalue cond = (score >= ctxt.new_rvalue (t_int, 100));

.. TODO: beyond this point

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

      :func:`gccjit::context::new_call` merely builds a
      :class:`gccjit::rvalue` i.e. an expression that can be evaluated,
      perhaps as part of a more complicated expression.
      The call *won't* happen unless you add a statement to a function
      that evaluates the expression.

      For example, if you want to call a function and discard the result
      (or to call a function with ``void`` return type), use
      :func:`gccjit::block::add_eval`:

      .. code-block:: c++

         /* Add "(void)printf (arg0, arg1);".  */
         block.add_eval (ctxt.new_call (printf_func, arg0, arg1));

Type-coercion
*************

.. function:: gccjit::rvalue \
              gccjit::context::new_cast (gccjit::rvalue rvalue,\
                                         gccjit::type type, \
                                         gccjit::location loc)

   Given an rvalue of T, construct another rvalue of another type.

   Currently only a limited set of conversions are possible:

     * int <-> float
     * int <-> bool
     * P*  <-> Q*, for pointer types P and Q

Lvalues
-------

.. class:: gccjit::lvalue

An lvalue is something that can of the *left*-hand side of an assignment:
a storage area (such as a variable).  It is a subclass of
:class:`gccjit::rvalue`, where the rvalue is computed by reading from the
storage area.

It iss a thin wrapper around :c:type:`gcc_jit_lvalue *` from the C API.

.. function:: gccjit::rvalue \
              gccjit::lvalue::get_address (gccjit::location loc)

   Take the address of an lvalue; analogous to:

   .. code-block:: c

     &(EXPR)

   in C.

   Parameter "loc" is optional.

Global variables
****************

.. function:: gccjit::lvalue \
              gccjit::context::new_global (enum gcc_jit_global_kind,\
                                           gccjit::type type, \
                                           const char *name, \
                                           gccjit::location loc)

   Add a new global variable of the given type and name to the context.

   This is a thin wrapper around :c:func:`gcc_jit_context_new_global` from
   the C API; the "kind" parameter has the same meaning as there.

Working with pointers, structs and unions
-----------------------------------------

.. function:: gccjit::lvalue \
              gccjit::rvalue::dereference (gccjit::location loc)

   Given an rvalue of pointer type ``T *``, dereferencing the pointer,
   getting an lvalue of type ``T``.  Analogous to:

   .. code-block:: c++

     *(EXPR)

   in C.

   Parameter "loc" is optional.

If you don't need to specify the location, this can also be expressed using
an overloaded operator:

.. function:: gccjit::lvalue \
              gccjit::rvalue::operator* ()

   .. code-block:: c++

      gccjit::lvalue content = *ptr;

Field access is provided separately for both lvalues and rvalues:

.. function:: gccjit::lvalue \
              gccjit::lvalue::access_field (gccjit::field field, \
                                            gccjit::location loc)

   Given an lvalue of struct or union type, access the given field,
   getting an lvalue of the field's type.  Analogous to:

   .. code-block:: c++

      (EXPR).field = ...;

   in C.

.. function:: gccjit::rvalue \
              gccjit::rvalue::access_field (gccjit::field field, \
                                            gccjit::location loc)

   Given an rvalue of struct or union type, access the given field
   as an rvalue.  Analogous to:

   .. code-block:: c++

      (EXPR).field

   in C.

.. function:: gccjit::lvalue \
              gccjit::rvalue::dereference_field (gccjit::field field, \
                                                 gccjit::location loc)

   Given an rvalue of pointer type ``T *`` where T is of struct or union
   type, access the given field as an lvalue.  Analogous to:

   .. code-block:: c++

      (EXPR)->field

   in C, itself equivalent to ``(*EXPR).FIELD``.

.. function:: gccjit::lvalue \
              gccjit::context::new_array_access (gccjit::rvalue ptr, \
                                                 gccjit::rvalue index, \
                                                 gccjit::location loc)

   Given an rvalue of pointer type ``T *``, get at the element `T` at
   the given index, using standard C array indexing rules i.e. each
   increment of ``index`` corresponds to ``sizeof(T)`` bytes.
   Analogous to:

   .. code-block:: c++

      PTR[INDEX]

   in C (or, indeed, to ``PTR + INDEX``).

   Parameter "loc" is optional.

For array accesses where you don't need to specify a :class:`gccjit::location`,
two overloaded operators are available:

    gccjit::lvalue gccjit::rvalue::operator[] (gccjit::rvalue index)

    .. code-block:: c++

       gccjit::lvalue element = array[idx];

    gccjit::lvalue gccjit::rvalue::operator[] (int index)

    .. code-block:: c++

       gccjit::lvalue element = array[0];
