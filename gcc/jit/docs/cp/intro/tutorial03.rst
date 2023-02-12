.. Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

.. default-domain:: cpp

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

    .. figure:: ../../intro/sum-of-squares.png
      :alt: image of a control flow graph

As before, we include the libgccjit++ header and make a
:type:`gccjit::context`.

.. code-block:: c++

  #include <libgccjit++.h>

  void test (void)
  {
    gccjit::context ctxt;
    ctxt = gccjit::context::acquire ();

The function works with the C `int` type.

In the previous tutorial we acquired this via

.. code-block:: c++

  gccjit::type the_type = ctxt.get_type (ctxt, GCC_JIT_TYPE_INT);

though we could equally well make it work on, say, `double`:

.. code-block:: c++

  gccjit::type the_type = ctxt.get_type (ctxt, GCC_JIT_TYPE_DOUBLE);

For integer types we can use :func:`gccjit::context::get_int_type<T>`
to directly bind a specific type:

.. code-block:: c++

  gccjit::type the_type = ctxt.get_int_type <int> ();

Let's build the function:

.. code-block:: c++

  gcc_jit_param n = ctxt.new_param (the_type, "n");
  std::vector<gccjit::param> params;
  params.push_back (n);
  gccjit::function func =
    ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                       return_type,
                       "loop_test",
                       params, 0);

Expressions: lvalues and rvalues
********************************

The base class of expression is the :type:`gccjit::rvalue`,
representing an expression that can be on the *right*-hand side of
an assignment: a value that can be computed somehow, and assigned
*to* a storage area (such as a variable).  It has a specific
:type:`gccjit::type`.

Anothe important class is :type:`gccjit::lvalue`.
A :type:`gccjit::lvalue`. is something that can of the *left*-hand
side of an assignment: a storage area (such as a variable).

In other words, every assignment can be thought of as:

.. code-block:: c

   LVALUE = RVALUE;

Note that :type:`gccjit::lvalue` is a subclass of
:type:`gccjit::rvalue`, where in an assignment of the form:

.. code-block:: c

   LVALUE_A = LVALUE_B;

the `LVALUE_B` implies reading the current value of that storage
area, assigning it into the `LVALUE_A`.

So far the only expressions we've seen are from the previous tutorial:

1. the multiplication `i * i`:

  .. code-block:: c++

     gccjit::rvalue expr =
       ctxt.new_binary_op (
         GCC_JIT_BINARY_OP_MULT, int_type,
         param_i, param_i);

     /* Alternatively, using operator-overloading: */
     gccjit::rvalue expr = param_i * param_i;

  which is a :type:`gccjit::rvalue`, and

2. the various function parameters: `param_i` and `param_n`, instances of
   :type:`gccjit::param`, which is a subclass of :type:`gccjit::lvalue`
   (and, in turn, of :type:`gccjit::rvalue`):
   we can both read from and write to function parameters within the
   body of a function.

Our new example has a new kind of expression: we have two local
variables.  We create them by calling
:func:`gccjit::function::new_local`, supplying a type and a name:

.. code-block:: c++

  /* Build locals:  */
  gccjit::lvalue i = func.new_local (the_type, "i");
  gccjit::lvalue sum = func.new_local (the_type, "sum");

These are instances of :type:`gccjit::lvalue` - they can be read from
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

so we create these as :type:`gccjit::block` instances within the
:type:`gccjit::function`:

.. code-block:: c++

  gccjit::block b_initial = func.new_block ("initial");
  gccjit::block b_loop_cond = func.new_block ("loop_cond");
  gccjit::block b_loop_body = func.new_block ("loop_body");
  gccjit::block b_after_loop = func.new_block ("after_loop");

We now populate each block with statements.

The entry block `b_initial` consists of initializations followed by a jump
to the conditional.  We assign `0` to `i` and to `sum`, using
:func:`gccjit::block::add_assignment` to add
an assignment statement, and using :func:`gccjit::context::zero` to get
the constant value `0` for the relevant type for the right-hand side of
the assignment:

.. code-block:: c++

  /* sum = 0; */
  b_initial.add_assignment (sum, ctxt.zero (the_type));

  /* i = 0; */
  b_initial.add_assignment (i, ctxt.zero (the_type));

We can then terminate the entry block by jumping to the conditional:

.. code-block:: c++

  b_initial.end_with_jump (b_loop_cond);

The conditional block is equivalent to the line `while (i < n)` from our
C example. It contains a single statement: a conditional, which jumps to
one of two destination blocks depending on a boolean
:type:`gccjit::rvalue`, in this case the comparison of `i` and `n`.

We could build the comparison using :func:`gccjit::context::new_comparison`:

.. code-block:: c++

   gccjit::rvalue guard =
     ctxt.new_comparison (GCC_JIT_COMPARISON_GE,
                          i, n);

and can then use this to add `b_loop_cond`'s sole statement, via
:func:`gccjit::block::end_with_conditional`:

.. code-block:: c++

  b_loop_cond.end_with_conditional (guard,
                                    b_after_loop, // on_true
                                    b_loop_body); // on_false

However :type:`gccjit::rvalue` has overloaded operators for this, so we
express the conditional as

.. code-block:: c++

   gccjit::rvalue guard = (i >= n);

and hence we can write the block more concisely as:

.. code-block:: c++

  b_loop_cond.end_with_conditional (
    i >= n,
    b_after_loop, // on_true
    b_loop_body); // on_false

Next, we populate the body of the loop.

The C statement `sum += i * i;` is an assignment operation, where an
lvalue is modified "in-place".  We use
:func:`gccjit::block::add_assignment_op` to handle these operations:

.. code-block:: c++

  /* sum += i * i */
  b_loop_body.add_assignment_op (sum,
                                 GCC_JIT_BINARY_OP_PLUS,
                                 i * i);

The `i++` can be thought of as `i += 1`, and can thus be handled in
a similar way.  We use :c:func:`gcc_jit_context_one` to get the constant
value `1` (for the relevant type) for the right-hand side
of the assignment.

.. code-block:: c++

  /* i++ */
  b_loop_body.add_assignment_op (i,
                                 GCC_JIT_BINARY_OP_PLUS,
                                 ctxt.one (the_type));

.. note::

  For numeric constants other than 0 or 1, we could use
  :func:`gccjit::context::new_rvalue`, which has overloads
  for both ``int`` and ``double``.

The loop body completes by jumping back to the conditional:

.. code-block:: c++

  b_loop_body.end_with_jump (b_loop_cond);

Finally, we populate the `b_after_loop` block, reached when the loop
conditional is false.  We want to generate the equivalent of:

.. code-block:: c++

   return sum;

so the block is just one statement:

.. code-block:: c++

  /* return sum */
  b_after_loop.end_with_return (sum);

.. note::

   You can intermingle block creation with statement creation,
   but given that the terminator statements generally include references
   to other blocks, I find it's clearer to create all the blocks,
   *then* all the statements.

We've finished populating the function.  As before, we can now compile it
to machine code:

.. code-block:: c++

   gcc_jit_result *result;
   result = ctxt.compile ();

   ctxt.release ();

   if (!result)
     {
       fprintf (stderr, "NULL result");
       return 1;
     }

   typedef int (*loop_test_fn_type) (int);
   loop_test_fn_type loop_test =
    (loop_test_fn_type)gcc_jit_result_get_code (result, "loop_test");
   if (!loop_test)
     {
       fprintf (stderr, "NULL loop_test");
       gcc_jit_result_release (result);
       return 1;
     }
   printf ("result: %d", loop_test (10));

.. code-block:: bash

   result: 285


Visualizing the control flow graph
**********************************

You can see the control flow graph of a function using
:func:`gccjit::function::dump_to_dot`:

.. code-block:: c++

  func.dump_to_dot ("/tmp/sum-of-squares.dot");

giving a .dot file in GraphViz format.

You can convert this to an image using `dot`:

.. code-block:: bash

   $ dot -Tpng /tmp/sum-of-squares.dot -o /tmp/sum-of-squares.png

or use a viewer (my preferred one is xdot.py; see
https://github.com/jrfonseca/xdot.py; on Fedora you can
install it with `yum install python-xdot`):

    .. figure:: ../../intro/sum-of-squares.png
      :alt: image of a control flow graph

Full example
************

   .. literalinclude:: ../../examples/tut03-sum-of-squares.cc
    :lines: 1-
    :language: c++

Building and running it:

.. code-block:: console

  $ gcc \
      tut03-sum-of-squares.cc \
      -o tut03-sum-of-squares \
      -lgccjit

  # Run the built program:
  $ ./tut03-sum-of-squares
  loop_test returned: 285
