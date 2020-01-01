.. Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

Tutorial part 3: Loops and variables
------------------------------------
Consider this C function:

 .. code-block:: c

  int loop_test (int n)
  {
    int sum = 0;
    for (int i = 0; i < n; i++)
      sum += i * i;
    return sum;
  }

This example demonstrates some more features of libgccjit, with local
variables and a loop.

To break this down into libgccjit terms, it's usually easier to reword
the `for` loop as a `while` loop, giving:

 .. code-block:: c

  int loop_test (int n)
  {
    int sum = 0;
    int i = 0;
    while (i < n)
    {
      sum += i * i;
      i++;
    }
    return sum;
  }

Here's what the final control flow graph will look like:

    .. figure:: sum-of-squares.png
      :alt: image of a control flow graph

As before, we include the libgccjit header and make a
:c:type:`gcc_jit_context *`.

.. code-block:: c

  #include <libgccjit.h>

  void test (void)
  {
    gcc_jit_context *ctxt;
    ctxt = gcc_jit_context_acquire ();

The function works with the C `int` type:

.. code-block:: c

  gcc_jit_type *the_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *return_type = the_type;

though we could equally well make it work on, say, `double`:

.. code-block:: c

  gcc_jit_type *the_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);

Let's build the function:

.. code-block:: c

  gcc_jit_param *n =
    gcc_jit_context_new_param (ctxt, NULL, the_type, "n");
  gcc_jit_param *params[1] = {n};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "loop_test",
				  1, params, 0);

Expressions: lvalues and rvalues
********************************

The base class of expression is the :c:type:`gcc_jit_rvalue *`,
representing an expression that can be on the *right*-hand side of
an assignment: a value that can be computed somehow, and assigned
*to* a storage area (such as a variable).  It has a specific
:c:type:`gcc_jit_type *`.

Anothe important class is :c:type:`gcc_jit_lvalue *`.
A :c:type:`gcc_jit_lvalue *`. is something that can of the *left*-hand
side of an assignment: a storage area (such as a variable).

In other words, every assignment can be thought of as:

.. code-block:: c

   LVALUE = RVALUE;

Note that :c:type:`gcc_jit_lvalue *` is a subclass of
:c:type:`gcc_jit_rvalue *`, where in an assignment of the form:

.. code-block:: c

   LVALUE_A = LVALUE_B;

the `LVALUE_B` implies reading the current value of that storage
area, assigning it into the `LVALUE_A`.

So far the only expressions we've seen are `i * i`:

.. code-block:: c

   gcc_jit_rvalue *expr =
     gcc_jit_context_new_binary_op (
       ctxt, NULL,
       GCC_JIT_BINARY_OP_MULT, int_type,
       gcc_jit_param_as_rvalue (param_i),
       gcc_jit_param_as_rvalue (param_i));

which is a :c:type:`gcc_jit_rvalue *`, and the various function
parameters: `param_i` and `param_n`, instances of
:c:type:`gcc_jit_param *`, which is a subclass of
:c:type:`gcc_jit_lvalue *` (and, in turn, of :c:type:`gcc_jit_rvalue *`):
we can both read from and write to function parameters within the
body of a function.

Our new example has a couple of local variables.  We create them by
calling :c:func:`gcc_jit_function_new_local`, supplying a type and a
name:

.. code-block:: c

  /* Build locals:  */
  gcc_jit_lvalue *i =
    gcc_jit_function_new_local (func, NULL, the_type, "i");
  gcc_jit_lvalue *sum =
    gcc_jit_function_new_local (func, NULL, the_type, "sum");

These are instances of :c:type:`gcc_jit_lvalue *` - they can be read from
and written to.

Note that there is no precanned way to create *and* initialize a variable
like in C:

.. code-block:: c

   int i = 0;

Instead, having added the local to the function, we have to separately add
an assignment of `0` to `local_i` at the beginning of the function.

Control flow
************

This function has a loop, so we need to build some basic blocks to
handle the control flow.  In this case, we need 4 blocks:

1. before the loop (initializing the locals)
2. the conditional at the top of the loop (comparing `i < n`)
3. the body of the loop
4. after the loop terminates (`return sum`)

so we create these as :c:type:`gcc_jit_block *` instances within the
:c:type:`gcc_jit_function *`:

.. code-block:: c

  gcc_jit_block *b_initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *b_loop_cond =
    gcc_jit_function_new_block (func, "loop_cond");
  gcc_jit_block *b_loop_body =
    gcc_jit_function_new_block (func, "loop_body");
  gcc_jit_block *b_after_loop =
    gcc_jit_function_new_block (func, "after_loop");

We now populate each block with statements.

The entry block `b_initial` consists of initializations followed by a jump
to the conditional.  We assign `0` to `i` and to `sum`, using
:c:func:`gcc_jit_block_add_assignment` to add
an assignment statement, and using :c:func:`gcc_jit_context_zero` to get
the constant value `0` for the relevant type for the right-hand side of
the assignment:

.. code-block:: c

  /* sum = 0; */
  gcc_jit_block_add_assignment (
    b_initial, NULL,
    sum,
    gcc_jit_context_zero (ctxt, the_type));

  /* i = 0; */
  gcc_jit_block_add_assignment (
    b_initial, NULL,
    i,
    gcc_jit_context_zero (ctxt, the_type));

We can then terminate the entry block by jumping to the conditional:

.. code-block:: c

  gcc_jit_block_end_with_jump (b_initial, NULL, b_loop_cond);

The conditional block is equivalent to the line `while (i < n)` from our
C example. It contains a single statement: a conditional, which jumps to
one of two destination blocks depending on a boolean
:c:type:`gcc_jit_rvalue *`, in this case the comparison of `i` and `n`.
We build the comparison using :c:func:`gcc_jit_context_new_comparison`:

.. code-block:: c

  /* (i >= n) */
   gcc_jit_rvalue *guard =
     gcc_jit_context_new_comparison (
       ctxt, NULL,
       GCC_JIT_COMPARISON_GE,
       gcc_jit_lvalue_as_rvalue (i),
       gcc_jit_param_as_rvalue (n));

and can then use this to add `b_loop_cond`'s sole statement, via
:c:func:`gcc_jit_block_end_with_conditional`:

.. code-block:: c

  /* Equivalent to:
       if (guard)
         goto after_loop;
       else
         goto loop_body;  */
  gcc_jit_block_end_with_conditional (
    b_loop_cond, NULL,
    guard,
    b_after_loop, /* on_true */
    b_loop_body); /* on_false */

Next, we populate the body of the loop.

The C statement `sum += i * i;` is an assignment operation, where an
lvalue is modified "in-place".  We use
:c:func:`gcc_jit_block_add_assignment_op` to handle these operations:

.. code-block:: c

  /* sum += i * i */
  gcc_jit_block_add_assignment_op (
    b_loop_body, NULL,
    sum,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT, the_type,
      gcc_jit_lvalue_as_rvalue (i),
      gcc_jit_lvalue_as_rvalue (i)));

The `i++` can be thought of as `i += 1`, and can thus be handled in
a similar way.  We use :c:func:`gcc_jit_context_one` to get the constant
value `1` (for the relevant type) for the right-hand side
of the assignment.

.. code-block:: c

  /* i++ */
  gcc_jit_block_add_assignment_op (
    b_loop_body, NULL,
    i,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, the_type));

.. note::

  For numeric constants other than 0 or 1, we could use
  :c:func:`gcc_jit_context_new_rvalue_from_int` and
  :c:func:`gcc_jit_context_new_rvalue_from_double`.

The loop body completes by jumping back to the conditional:

.. code-block:: c

  gcc_jit_block_end_with_jump (b_loop_body, NULL, b_loop_cond);

Finally, we populate the `b_after_loop` block, reached when the loop
conditional is false.  We want to generate the equivalent of:

.. code-block:: c

   return sum;

so the block is just one statement:

.. code-block:: c

  /* return sum */
  gcc_jit_block_end_with_return (
    b_after_loop,
    NULL,
    gcc_jit_lvalue_as_rvalue (sum));

.. note::

   You can intermingle block creation with statement creation,
   but given that the terminator statements generally include references
   to other blocks, I find it's clearer to create all the blocks,
   *then* all the statements.

We've finished populating the function.  As before, we can now compile it
to machine code:

.. code-block:: c

   gcc_jit_result *result;
   result = gcc_jit_context_compile (ctxt);

   typedef int (*loop_test_fn_type) (int);
   loop_test_fn_type loop_test =
    (loop_test_fn_type)gcc_jit_result_get_code (result, "loop_test");
   if (!loop_test)
     goto error;
   printf ("result: %d", loop_test (10));

.. code-block:: bash

   result: 285


Visualizing the control flow graph
**********************************

You can see the control flow graph of a function using
:c:func:`gcc_jit_function_dump_to_dot`:

.. code-block:: c

  gcc_jit_function_dump_to_dot (func, "/tmp/sum-of-squares.dot");

giving a .dot file in GraphViz format.

You can convert this to an image using `dot`:

.. code-block:: bash

   $ dot -Tpng /tmp/sum-of-squares.dot -o /tmp/sum-of-squares.png

or use a viewer (my preferred one is xdot.py; see
https://github.com/jrfonseca/xdot.py; on Fedora you can
install it with `yum install python-xdot`):

    .. figure:: sum-of-squares.png
      :alt: image of a control flow graph

Full example
************

   .. literalinclude:: ../examples/tut03-sum-of-squares.c
    :lines: 1-
    :language: c

Building and running it:

.. code-block:: console

  $ gcc \
      tut03-sum-of-squares.c \
      -o tut03-sum-of-squares \
      -lgccjit

  # Run the built program:
  $ ./tut03-sum-of-squares
  loop_test returned: 285
