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

Creating and using functions
============================

Params
------
.. type:: gcc_jit_param

   A `gcc_jit_param` represents a parameter to a function.

.. function:: gcc_jit_param *\
              gcc_jit_context_new_param (gcc_jit_context *ctxt,\
                                         gcc_jit_location *loc,\
                                         gcc_jit_type *type,\
                                         const char *name)

   In preparation for creating a function, create a new parameter of the
   given type and name.

   The parameter ``type`` must be non-`void`.

   The parameter ``name`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

Parameters are lvalues, and thus are also rvalues (and objects), so the
following upcasts are available:

.. function::  gcc_jit_lvalue *\
               gcc_jit_param_as_lvalue (gcc_jit_param *param)

   Upcasting from param to lvalue.

.. function::  gcc_jit_rvalue *\
               gcc_jit_param_as_rvalue (gcc_jit_param *param)

   Upcasting from param to rvalue.

.. function::  gcc_jit_object *\
               gcc_jit_param_as_object (gcc_jit_param *param)

   Upcasting from param to object.


Functions
---------

.. type:: gcc_jit_function

   A `gcc_jit_function` represents a function - either one that we're
   creating ourselves, or one that we're referencing.

.. function::  gcc_jit_function *\
               gcc_jit_context_new_function (gcc_jit_context *ctxt,\
                                             gcc_jit_location *loc,\
                                             enum gcc_jit_function_kind kind,\
                                             gcc_jit_type *return_type,\
                                             const char *name,\
                                             int num_params,\
                                             gcc_jit_param **params,\
                                             int is_variadic)

   Create a gcc_jit_function with the given name and parameters.

   .. type:: enum gcc_jit_function_kind

   This enum controls the kind of function created, and has the following
   values:

      .. macro:: GCC_JIT_FUNCTION_EXPORTED

         Function is defined by the client code and visible
         by name outside of the JIT.

         This value is required if you want to extract machine code
         for this function from a :type:`gcc_jit_result` via
         :func:`gcc_jit_result_get_code`.

      .. macro::   GCC_JIT_FUNCTION_INTERNAL

         Function is defined by the client code, but is invisible
         outside of the JIT.  Analogous to a "static" function.

      .. macro::   GCC_JIT_FUNCTION_IMPORTED

         Function is not defined by the client code; we're merely
         referring to it.  Analogous to using an "extern" function from a
         header file.

      .. macro::   GCC_JIT_FUNCTION_ALWAYS_INLINE

         Function is only ever inlined into other functions, and is
         invisible outside of the JIT.

         Analogous to prefixing with ``inline`` and adding
         ``__attribute__((always_inline))``

         Inlining will only occur when the optimization level is
         above 0; when optimization is off, this is essentially the
         same as GCC_JIT_FUNCTION_INTERNAL.

   The parameter ``name`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

.. function::  gcc_jit_function *\
               gcc_jit_context_get_builtin_function (gcc_jit_context *ctxt,\
                                                     const char *name)

   Get the :type:`gcc_jit_function` for the built-in function with the
   given name.  For example:

   .. code-block:: c

      gcc_jit_function *fn
        = gcc_jit_context_get_builtin_function (ctxt, "__builtin_memcpy");

   .. note:: Due to technical limitations with how libgccjit interacts with
      the insides of GCC, not all built-in functions are supported.  More
      precisely, not all types are supported for parameters of built-in
      functions from libgccjit.  Attempts to get a built-in function that
      uses such a parameter will lead to an error being emitted within
      the context.

.. function::  gcc_jit_object *\
               gcc_jit_function_as_object (gcc_jit_function *func)

    Upcasting from function to object.

.. function::  gcc_jit_param *\
               gcc_jit_function_get_param (gcc_jit_function *func, int index)

   Get the param of the given index (0-based).

.. function::  void \
               gcc_jit_function_dump_to_dot (gcc_jit_function *func,\
                                             const char *path)

   Emit the function in graphviz format to the given path.

.. function:: gcc_jit_lvalue *\
              gcc_jit_function_new_local (gcc_jit_function *func,\
                                          gcc_jit_location *loc,\
                                          gcc_jit_type *type,\
                                          const char *name)

   Create a new local variable within the function, of the given type and
   name.

   The parameter ``type`` must be non-`void`.

   The parameter ``name`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

Blocks
------
.. type:: gcc_jit_block

   A `gcc_jit_block` represents a basic block within a function  i.e. a
   sequence of statements with a single entry point and a single exit
   point.

   The first basic block that you create within a function will
   be the entrypoint.

   Each basic block that you create within a function must be
   terminated, either with a conditional, a jump, a return, or a
   switch.

   It's legal to have multiple basic blocks that return within
   one function.

.. function::  gcc_jit_block *\
               gcc_jit_function_new_block (gcc_jit_function *func,\
                                           const char *name)

   Create a basic block of the given name.  The name may be NULL, but
   providing meaningful names is often helpful when debugging: it may
   show up in dumps of the internal representation, and in error
   messages.  It is copied, so the input buffer does not need to outlive
   the call; you can pass in a pointer to an on-stack buffer, e.g.:

   .. code-block:: c

     for (pc = 0; pc < fn->fn_num_ops; pc++)
      {
        char buf[16];
        sprintf (buf, "instr%i", pc);
        state.op_blocks[pc] = gcc_jit_function_new_block (state.fn, buf);
      }

.. function::  gcc_jit_object *\
               gcc_jit_block_as_object (gcc_jit_block *block)

   Upcast from block to object.

.. function::  gcc_jit_function *\
               gcc_jit_block_get_function (gcc_jit_block *block)

   Which function is this block within?


Statements
----------

.. function:: void\
              gcc_jit_block_add_eval (gcc_jit_block *block,\
                                      gcc_jit_location *loc,\
                                      gcc_jit_rvalue *rvalue)

   Add evaluation of an rvalue, discarding the result
   (e.g. a function call that "returns" void).

   This is equivalent to this C code:

   .. code-block:: c

     (void)expression;

.. function:: void\
              gcc_jit_block_add_assignment (gcc_jit_block *block,\
                                            gcc_jit_location *loc,\
                                            gcc_jit_lvalue *lvalue,\
                                            gcc_jit_rvalue *rvalue)

   Add evaluation of an rvalue, assigning the result to the given
   lvalue.

   This is roughly equivalent to this C code:

   .. code-block:: c

     lvalue = rvalue;

.. function:: void\
              gcc_jit_block_add_assignment_op (gcc_jit_block *block,\
                                 gcc_jit_location *loc,\
                                 gcc_jit_lvalue *lvalue,\
                                 enum gcc_jit_binary_op op,\
                                 gcc_jit_rvalue *rvalue)

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
     gcc_jit_block_add_assignment_op (
       loop_body, NULL,
       i,
       GCC_JIT_BINARY_OP_PLUS,
       gcc_jit_context_one (ctxt, int_type));

.. function:: void\
              gcc_jit_block_add_comment (gcc_jit_block *block,\
                                         gcc_jit_location *loc,\
                                         const char *text)

   Add a no-op textual comment to the internal representation of the
   code.  It will be optimized away, but will be visible in the dumps
   seen via :macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE`
   and :macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE`,
   and thus may be of use when debugging how your project's internal
   representation gets converted to the libgccjit IR.

   The parameter ``text`` must be non-NULL.  It is copied, so the input
   buffer does not need to outlive the call.  For example:

   .. code-block:: c

     char buf[100];
     snprintf (buf, sizeof (buf),
               "op%i: %s",
               pc, opcode_names[op->op_opcode]);
     gcc_jit_block_add_comment (block, loc, buf);

.. function:: void\
              gcc_jit_block_end_with_conditional (gcc_jit_block *block,\
                                                  gcc_jit_location *loc,\
                                                  gcc_jit_rvalue *boolval,\
                                                  gcc_jit_block *on_true,\
                                                  gcc_jit_block *on_false)

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
              gcc_jit_block_end_with_jump (gcc_jit_block *block,\
                                           gcc_jit_location *loc,\
                                           gcc_jit_block *target)


   Terminate a block by adding a jump to the given target block.

   This is roughly equivalent to this C code:

   .. code-block:: c

      goto target;

.. function:: void\
              gcc_jit_block_end_with_return (gcc_jit_block *block,\
                                             gcc_jit_location *loc,\
                                             gcc_jit_rvalue *rvalue)


   Terminate a block by adding evaluation of an rvalue, returning the value.

   This is roughly equivalent to this C code:

   .. code-block:: c

      return expression;

.. function:: void\
              gcc_jit_block_end_with_void_return (gcc_jit_block *block,\
                                                  gcc_jit_location *loc)


   Terminate a block by adding a valueless return, for use within a function
   with "void" return type.

   This is equivalent to this C code:

   .. code-block:: c

      return;

.. function:: void\
              gcc_jit_block_end_with_switch (gcc_jit_block *block,\
                                             gcc_jit_location *loc,\
                                             gcc_jit_rvalue *expr,\
                                             gcc_jit_block *default_block,\
                                             int num_cases,\
                                             gcc_jit_case **cases)

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

   ``block``, ``expr``, ``default_block`` and ``cases`` must all be
   non-NULL.

   ``expr`` must be of the same integer type as all of the ``min_value``
   and ``max_value`` within the cases.

   ``num_cases`` must be >= 0.

   The ranges of the cases must not overlap (or have duplicate
   values).

   The API entrypoints relating to switch statements and cases:

      * :c:func:`gcc_jit_block_end_with_switch`

      * :c:func:`gcc_jit_case_as_object`

      * :c:func:`gcc_jit_context_new_case`

   were added in :ref:`LIBGCCJIT_ABI_3`; you can test for their presence
   using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS

   .. type:: gcc_jit_case

   A `gcc_jit_case` represents a case within a switch statement, and
   is created within a particular :c:type:`gcc_jit_context` using
   :c:func:`gcc_jit_context_new_case`.

   Each case expresses a multivalued range of integer values.  You
   can express single-valued cases by passing in the same value for
   both `min_value` and `max_value`.

   .. function:: gcc_jit_case *\
                 gcc_jit_context_new_case (gcc_jit_context *ctxt,\
                                           gcc_jit_rvalue *min_value,\
                                           gcc_jit_rvalue *max_value,\
                                           gcc_jit_block *dest_block)

      Create a new gcc_jit_case instance for use in a switch statement.
      `min_value` and `max_value` must be constants of an integer type,
      which must match that of the expression of the switch statement.

      `dest_block` must be within the same function as the switch
      statement.

   .. function:: gcc_jit_object *\
                 gcc_jit_case_as_object (gcc_jit_case *case_)

      Upcast from a case to an object.

   Here's an example of creating a switch statement:

     .. literalinclude:: ../../../testsuite/jit.dg/test-switch.c
       :start-after: /* Quote from here in docs/topics/functions.rst.  */
       :end-before: /* Quote up to here in docs/topics/functions.rst.  */
       :language: c

See also :type:`gcc_jit_extended_asm` for entrypoints for adding inline
assembler statements to a function.
