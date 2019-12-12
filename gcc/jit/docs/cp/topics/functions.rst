.. Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

Creating and using functions
============================

Params
------
.. class:: gccjit::param

   A `gccjit::param` represents a parameter to a function.

.. function:: gccjit::param \
              gccjit::context::new_param (gccjit::type type,\
                                          const char *name, \
                                          gccjit::location loc)

   In preparation for creating a function, create a new parameter of the
   given type and name.

:class:`gccjit::param` is a subclass of :class:`gccjit::lvalue` (and thus
of :class:`gccjit::rvalue` and :class:`gccjit::object`).  It is a thin
wrapper around the C API's :c:type:`gcc_jit_param *`.

Functions
---------

.. class:: gccjit::function

   A `gccjit::function` represents a function - either one that we're
   creating ourselves, or one that we're referencing.

.. function::  gccjit::function \
               gccjit::context::new_function (enum gcc_jit_function_kind,\
                                              gccjit::type return_type, \
                                              const char *name, \
                                              std::vector<param> &params, \
                                              int is_variadic, \
                                              gccjit::location loc)

   Create a gcc_jit_function with the given name and parameters.

   Parameters "is_variadic" and "loc" are optional.

   This is a wrapper around the C API's :c:func:`gcc_jit_context_new_function`.

.. function::  gccjit::function \
               gccjit::context::get_builtin_function (const char *name)

   This is a wrapper around the C API's
   :c:func:`gcc_jit_context_get_builtin_function`.

.. function::  gccjit::param \
               gccjit::function::get_param (int index) const

   Get the param of the given index (0-based).

.. function::  void \
               gccjit::function::dump_to_dot (const char *path)

   Emit the function in graphviz format to the given path.

.. function:: gccjit::lvalue \
              gccjit::function::new_local (gccjit::type type,\
                                           const char *name, \
                                           gccjit::location loc)

   Create a new local variable within the function, of the given type and
   name.

Blocks
------
.. class:: gccjit::block

   A `gccjit::block` represents a basic block within a function  i.e. a
   sequence of statements with a single entry point and a single exit
   point.

   :class:`gccjit::block` is a subclass of :class:`gccjit::object`.

   The first basic block that you create within a function will
   be the entrypoint.

   Each basic block that you create within a function must be
   terminated, either with a conditional, a jump, a return, or
   a switch.

   It's legal to have multiple basic blocks that return within
   one function.

.. function::  gccjit::block \
               gccjit::function::new_block (const char *name)

   Create a basic block of the given name.  The name may be NULL, but
   providing meaningful names is often helpful when debugging: it may
   show up in dumps of the internal representation, and in error
   messages.

Statements
----------

.. function:: void\
              gccjit::block::add_eval (gccjit::rvalue rvalue, \
                                       gccjit::location loc)

   Add evaluation of an rvalue, discarding the result
   (e.g. a function call that "returns" void).

   This is equivalent to this C code:

   .. code-block:: c

     (void)expression;

.. function:: void\
              gccjit::block::add_assignment (gccjit::lvalue lvalue, \
                                             gccjit::rvalue rvalue, \
                                             gccjit::location loc)

   Add evaluation of an rvalue, assigning the result to the given
   lvalue.

   This is roughly equivalent to this C code:

   .. code-block:: c

     lvalue = rvalue;

.. function:: void\
              gccjit::block::add_assignment_op (gccjit::lvalue lvalue, \
                                                enum gcc_jit_binary_op, \
                                                gccjit::rvalue rvalue, \
                                                gccjit::location loc)

   Add evaluation of an rvalue, using the result to modify an
   lvalue.

   This is analogous to "+=" and friends:

   .. code-block:: c

     lvalue += rvalue;
     lvalue *= rvalue;
     lvalue /= rvalue;

   etc.  For example:

   .. code-block:: c

     /* "i++" */
     loop_body.add_assignment_op (
       i,
       GCC_JIT_BINARY_OP_PLUS,
       ctxt.one (int_type));

.. function:: void\
              gccjit::block::add_comment (const char *text, \
	                                  gccjit::location loc)

   Add a no-op textual comment to the internal representation of the
   code.  It will be optimized away, but will be visible in the dumps
   seen via :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE`
   and :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE`,
   and thus may be of use when debugging how your project's internal
   representation gets converted to the libgccjit IR.

   Parameter "loc" is optional.

.. function:: void\
              gccjit::block::end_with_conditional (gccjit::rvalue boolval,\
                                                   gccjit::block on_true,\
                                                   gccjit::block on_false, \
                                                   gccjit::location loc)

   Terminate a block by adding evaluation of an rvalue, branching on the
   result to the appropriate successor block.

   This is roughly equivalent to this C code:

   .. code-block:: c

     if (boolval)
       goto on_true;
     else
       goto on_false;

   block, boolval, on_true, and on_false must be non-NULL.

.. function:: void\
              gccjit::block::end_with_jump (gccjit::block target, \
                                            gccjit::location loc)

   Terminate a block by adding a jump to the given target block.

   This is roughly equivalent to this C code:

   .. code-block:: c

      goto target;

.. function:: void\
              gccjit::block::end_with_return (gccjit::rvalue rvalue, \
                                              gccjit::location loc)

   Terminate a block.

   Both params are optional.

   An rvalue must be provided for a function returning non-void, and
   must not be provided by a function "returning" `void`.

   If an rvalue is provided, the block is terminated by evaluating the
   rvalue and returning the value.

   This is roughly equivalent to this C code:

   .. code-block:: c

      return expression;

   If an rvalue is not provided, the block is terminated by adding a
   valueless return, for use within a function with "void" return type.

   This is equivalent to this C code:

   .. code-block:: c

      return;

.. function:: void\
              gccjit::block::end_with_switch (gccjit::rvalue expr,\
                                              gccjit::block default_block,\
                                              std::vector <gccjit::case_> cases,\
                                              gccjit::location loc)

   Terminate a block by adding evalation of an rvalue, then performing
   a multiway branch.

   This is roughly equivalent to this C code:

   .. code-block:: c

     switch (expr)
       {
       default:
         goto default_block;

       case C0.min_value ... C0.max_value:
         goto C0.dest_block;

       case C1.min_value ... C1.max_value:
         goto C1.dest_block;

       ...etc...

       case C[N - 1].min_value ... C[N - 1].max_value:
         goto C[N - 1].dest_block;
     }

   ``expr`` must be of the same integer type as all of the ``min_value``
   and ``max_value`` within the cases.

   The ranges of the cases must not overlap (or have duplicate
   values).

   The API entrypoints relating to switch statements and cases:

      * :func:`gccjit::block::end_with_switch`

      * :func:`gccjit::context::new_case`

   were added in :ref:`LIBGCCJIT_ABI_3`; you can test for their presence
   using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS

   .. class:: gccjit::case_

   A `gccjit::case_` represents a case within a switch statement, and
   is created within a particular :class:`gccjit::context` using
   :func:`gccjit::context::new_case`.  It is a subclass of
   :class:`gccjit::object`.

   Each case expresses a multivalued range of integer values.  You
   can express single-valued cases by passing in the same value for
   both `min_value` and `max_value`.

   .. function:: gccjit::case_ *\
                 gccjit::context::new_case (gccjit::rvalue min_value,\
                                            gccjit::rvalue max_value,\
                                            gccjit::block dest_block)

      Create a new gccjit::case for use in a switch statement.
      `min_value` and `max_value` must be constants of an integer type,
      which must match that of the expression of the switch statement.

      `dest_block` must be within the same function as the switch
      statement.

   Here's an example of creating a switch statement:

     .. literalinclude:: ../../../../testsuite/jit.dg/test-switch.cc
       :start-after: /* Quote from here in docs/cp/topics/functions.rst.  */
       :end-before: /* Quote up to here in docs/cp/topics/functions.rst.  */
       :language: c++
