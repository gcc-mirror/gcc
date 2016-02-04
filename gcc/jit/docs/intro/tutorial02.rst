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

.. default-domain:: c

Tutorial part 2: Creating a trivial machine code function
---------------------------------------------------------

Consider this C function:

.. code-block:: c

   int square (int i)
   {
     return i * i;
   }

How can we construct this at run-time using libgccjit?

First we need to include the relevant header:

.. code-block:: c

  #include <libgccjit.h>

All state associated with compilation is associated with a
:c:type:`gcc_jit_context *`.

Create one using :c:func:`gcc_jit_context_acquire`:

.. code-block:: c

  gcc_jit_context *ctxt;
  ctxt = gcc_jit_context_acquire ();

The JIT library has a system of types.  It is statically-typed: every
expression is of a specific type, fixed at compile-time.  In our example,
all of the expressions are of the C `int` type, so let's obtain this from
the context, as a :c:type:`gcc_jit_type *`, using
:c:func:`gcc_jit_context_get_type`:

.. code-block:: c

  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

:c:type:`gcc_jit_type *` is an example of a "contextual" object: every
entity in the API is associated with a :c:type:`gcc_jit_context *`.

Memory management is easy: all such "contextual" objects are automatically
cleaned up for you when the context is released, using
:c:func:`gcc_jit_context_release`:

.. code-block:: c

  gcc_jit_context_release (ctxt);

so you don't need to manually track and cleanup all objects, just the
contexts.

Although the API is C-based, there is a form of class hierarchy, which
looks like this::

  +- gcc_jit_object
      +- gcc_jit_location
      +- gcc_jit_type
         +- gcc_jit_struct
      +- gcc_jit_field
      +- gcc_jit_function
      +- gcc_jit_block
      +- gcc_jit_rvalue
          +- gcc_jit_lvalue
             +- gcc_jit_param

There are casting methods for upcasting from subclasses to parent classes.
For example, :c:func:`gcc_jit_type_as_object`:

.. code-block:: c

   gcc_jit_object *obj = gcc_jit_type_as_object (int_type);

One thing you can do with a :c:type:`gcc_jit_object *` is
to ask it for a human-readable description, using
:c:func:`gcc_jit_object_get_debug_string`:

.. code-block:: c

   printf ("obj: %s\n", gcc_jit_object_get_debug_string (obj));

giving this text on stdout:

.. code-block:: bash

   obj: int

This is invaluable when debugging.

Let's create the function.  To do so, we first need to construct
its single parameter, specifying its type and giving it a name,
using :c:func:`gcc_jit_context_new_param`:

.. code-block:: c

  gcc_jit_param *param_i =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");

Now we can create the function, using
:c:func:`gcc_jit_context_new_function`:

.. code-block:: c

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "square",
                                  1, &param_i,
                                  0);

To define the code within the function, we must create basic blocks
containing statements.

Every basic block contains a list of statements, eventually terminated
by a statement that either returns, or jumps to another basic block.

Our function has no control-flow, so we just need one basic block:

.. code-block:: c

  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

Our basic block is relatively simple: it immediately terminates by
returning the value of an expression.

We can build the expression using :c:func:`gcc_jit_context_new_binary_op`:

.. code-block:: c

   gcc_jit_rvalue *expr =
     gcc_jit_context_new_binary_op (
       ctxt, NULL,
       GCC_JIT_BINARY_OP_MULT, int_type,
       gcc_jit_param_as_rvalue (param_i),
       gcc_jit_param_as_rvalue (param_i));

A :c:type:`gcc_jit_rvalue *` is another example of a
:c:type:`gcc_jit_object *` subclass.  We can upcast it using
:c:func:`gcc_jit_rvalue_as_object` and as before print it with
:c:func:`gcc_jit_object_get_debug_string`.

.. code-block:: c

   printf ("expr: %s\n",
           gcc_jit_object_get_debug_string (
             gcc_jit_rvalue_as_object (expr)));

giving this output:

.. code-block:: bash

   expr: i * i

Creating the expression in itself doesn't do anything; we have to add
this expression to a statement within the block.  In this case, we use it
to build a return statement, which terminates the basic block:

.. code-block:: c

  gcc_jit_block_end_with_return (block, NULL, expr);

OK, we've populated the context.  We can now compile it using
:c:func:`gcc_jit_context_compile`:

.. code-block:: c

   gcc_jit_result *result;
   result = gcc_jit_context_compile (ctxt);

and get a :c:type:`gcc_jit_result *`.

At this point we're done with the context; we can release it:

.. code-block:: c

   gcc_jit_context_release (ctxt);

We can now use :c:func:`gcc_jit_result_get_code` to look up a specific
machine code routine within the result, in this case, the function we
created above.

.. code-block:: c

   void *fn_ptr = gcc_jit_result_get_code (result, "square");
   if (!fn_ptr)
     {
       fprintf (stderr, "NULL fn_ptr");
       goto error;
     }

We can now cast the pointer to an appropriate function pointer type, and
then call it:

.. code-block:: c

  typedef int (*fn_type) (int);
  fn_type square = (fn_type)fn_ptr;
  printf ("result: %d", square (5));

.. code-block:: bash

  result: 25

Once we're done with the code, we can release the result:

.. code-block:: c

   gcc_jit_result_release (result);

We can't call ``square`` anymore once we've released ``result``.


Error-handling
**************
Various kinds of errors are possible when using the API, such as
mismatched types in an assignment.  You can only compile and get code
from a context if no errors occur.

Errors are printed on stderr; they typically contain the name of the API
entrypoint where the error occurred, and pertinent information on the
problem:

.. code-block:: console

  ./buggy-program: error: gcc_jit_block_add_assignment: mismatching types: assignment to i (type: int) from "hello world" (type: const char *)

The API is designed to cope with errors without crashing, so you can get
away with having a single error-handling check in your code:

.. code-block:: c

   void *fn_ptr = gcc_jit_result_get_code (result, "square");
   if (!fn_ptr)
     {
       fprintf (stderr, "NULL fn_ptr");
       goto error;
     }

For more information, see the :ref:`error-handling guide <error-handling>`
within the Topic eference.


Options
*******

To get more information on what's going on, you can set debugging flags
on the context using :c:func:`gcc_jit_context_set_bool_option`.

.. (I'm deliberately not mentioning
    :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE` here since I think
    it's probably more of use to implementors than to users)

Setting :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE` will dump a
C-like representation to stderr when you compile (GCC's "GIMPLE"
representation):

.. code-block:: c

   gcc_jit_context_set_bool_option (
     ctxt,
     GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,
     1);
   result = gcc_jit_context_compile (ctxt);

.. code-block:: c

  square (signed int i)
  {
    signed int D.260;

    entry:
    D.260 = i * i;
    return D.260;
  }

We can see the generated machine code in assembler form (on stderr) by
setting :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE` on the context
before compiling:

.. code-block:: c

  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,
    1);
  result = gcc_jit_context_compile (ctxt);

.. code-block:: gas

        .file   "fake.c"
        .text
        .globl  square
        .type   square, @function
  square:
  .LFB6:
        .cfi_startproc
        pushq   %rbp
        .cfi_def_cfa_offset 16
        .cfi_offset 6, -16
        movq    %rsp, %rbp
        .cfi_def_cfa_register 6
        movl    %edi, -4(%rbp)
  .L14:
        movl    -4(%rbp), %eax
        imull   -4(%rbp), %eax
        popq    %rbp
        .cfi_def_cfa 7, 8
        ret
        .cfi_endproc
  .LFE6:
        .size   square, .-square
        .ident  "GCC: (GNU) 4.9.0 20131023 (Red Hat 0.2-0.5.1920c315ff984892399893b380305ab36e07b455.fc20)"
        .section       .note.GNU-stack,"",@progbits

By default, no optimizations are performed, the equivalent of GCC's
`-O0` option.  We can turn things up to e.g. `-O3` by calling
:c:func:`gcc_jit_context_set_int_option` with
:c:macro:`GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL`:

.. code-block:: c

  gcc_jit_context_set_int_option (
    ctxt,
    GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
    3);

.. code-block:: gas

        .file   "fake.c"
        .text
        .p2align 4,,15
        .globl  square
        .type   square, @function
  square:
  .LFB7:
        .cfi_startproc
  .L16:
        movl    %edi, %eax
        imull   %edi, %eax
        ret
        .cfi_endproc
  .LFE7:
        .size   square, .-square
        .ident  "GCC: (GNU) 4.9.0 20131023 (Red Hat 0.2-0.5.1920c315ff984892399893b380305ab36e07b455.fc20)"
        .section        .note.GNU-stack,"",@progbits

Naturally this has only a small effect on such a trivial function.


Full example
************

Here's what the above looks like as a complete program:

   .. literalinclude:: ../examples/tut02-square.c
    :lines: 1-
    :language: c

Building and running it:

.. code-block:: console

  $ gcc \
      tut02-square.c \
      -o tut02-square \
      -lgccjit

  # Run the built program:
  $ ./tut02-square
  result: 25
