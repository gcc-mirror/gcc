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

Tutorial part 2: Creating a trivial machine code function
---------------------------------------------------------

Consider this C function:

.. code-block:: c

   int square (int i)
   {
     return i * i;
   }

How can we construct this at run-time using libgccjit's C++ API?

First we need to include the relevant header:

.. code-block:: c++

  #include <libgccjit++.h>

All state associated with compilation is associated with a
:type:`gccjit::context`, which is a thin C++ wrapper around the C API's
:c:type:`gcc_jit_context *`.

Create one using :func:`gccjit::context::acquire`:

.. code-block:: c++

  gccjit::context ctxt;
  ctxt = gccjit::context::acquire ();

The JIT library has a system of types.  It is statically-typed: every
expression is of a specific type, fixed at compile-time.  In our example,
all of the expressions are of the C `int` type, so let's obtain this from
the context, as a :type:`gccjit::type`, using
:func:`gccjit::context::get_type`:

.. code-block:: c++

  gccjit::type int_type = ctxt.get_type (GCC_JIT_TYPE_INT);

:type:`gccjit::type` is an example of a "contextual" object: every
entity in the API is associated with a :type:`gccjit::context`.

Memory management is easy: all such "contextual" objects are automatically
cleaned up for you when the context is released, using
:func:`gccjit::context::release`:

.. code-block:: c++

  ctxt.release ();

so you don't need to manually track and cleanup all objects, just the
contexts.

All of the C++ classes in the API are thin wrappers around pointers to
types in the C API.

The C++ class hierarchy within the ``gccjit`` namespace looks like this::

  +- object
      +- location
      +- type
         +- struct
      +- field
      +- function
      +- block
      +- rvalue
          +- lvalue
             +- param

One thing you can do with a :type:`gccjit::object` is
to ask it for a human-readable description as a :type:`std::string`, using
:func:`gccjit::object::get_debug_string`:

.. code-block:: c++

   printf ("obj: %s\n", obj.get_debug_string ().c_str ());

giving this text on stdout:

.. code-block:: bash

   obj: int

This is invaluable when debugging.

Let's create the function.  To do so, we first need to construct
its single parameter, specifying its type and giving it a name,
using :func:`gccjit::context::new_param`:

.. code-block:: c++

  gccjit::param param_i = ctxt.new_param (int_type, "i");

and we can then make a vector of all of the params of the function,
in this case just one:

.. code-block:: c++

  std::vector<gccjit::param> params;
  params.push_back (param_i);

Now we can create the function, using
:c:func:`gccjit::context::new_function`:

.. code-block:: c++

  gccjit::function func =
    ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                       int_type,
                       "square",
                       params,
                       0);

To define the code within the function, we must create basic blocks
containing statements.

Every basic block contains a list of statements, eventually terminated
by a statement that either returns, or jumps to another basic block.

Our function has no control-flow, so we just need one basic block:

.. code-block:: c++

  gccjit::block block = func.new_block ();

Our basic block is relatively simple: it immediately terminates by
returning the value of an expression.

We can build the expression using :func:`gccjit::context::new_binary_op`:

.. code-block:: c++

   gccjit::rvalue expr =
     ctxt.new_binary_op (
       GCC_JIT_BINARY_OP_MULT, int_type,
       param_i, param_i);

A :type:`gccjit::rvalue` is another example of a
:type:`gccjit::object` subclass.  As before, we can print it with
:func:`gccjit::object::get_debug_string`.

.. code-block:: c++

   printf ("expr: %s\n", expr.get_debug_string ().c_str ());

giving this output:

.. code-block:: bash

   expr: i * i

Note that :type:`gccjit::rvalue` provides numerous overloaded operators
which can be used to dramatically reduce the amount of typing needed.
We can build the above binary operation more directly with this one-liner:

.. code-block:: c++

   gccjit::rvalue expr = param_i * param_i;

Creating the expression in itself doesn't do anything; we have to add
this expression to a statement within the block.  In this case, we use it
to build a return statement, which terminates the basic block:

.. code-block:: c++

  block.end_with_return (expr);

OK, we've populated the context.  We can now compile it using
:func:`gccjit::context::compile`:

.. code-block:: c++

   gcc_jit_result *result;
   result = ctxt.compile ();

and get a :c:type:`gcc_jit_result *`.

We can now use :c:func:`gcc_jit_result_get_code` to look up a specific
machine code routine within the result, in this case, the function we
created above.

.. code-block:: c++

   void *fn_ptr = gcc_jit_result_get_code (result, "square");
   if (!fn_ptr)
     {
       fprintf (stderr, "NULL fn_ptr");
       goto error;
     }

We can now cast the pointer to an appropriate function pointer type, and
then call it:

.. code-block:: c++

  typedef int (*fn_type) (int);
  fn_type square = (fn_type)fn_ptr;
  printf ("result: %d", square (5));

.. code-block:: bash

  result: 25


Options
*******

To get more information on what's going on, you can set debugging flags
on the context using :func:`gccjit::context::set_bool_option`.

.. (I'm deliberately not mentioning
    :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE` here since I think
    it's probably more of use to implementors than to users)

Setting :c:macro:`GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE` will dump a
C-like representation to stderr when you compile (GCC's "GIMPLE"
representation):

.. code-block:: c++

   ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, 1);
   result = ctxt.compile ();

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

.. code-block:: c++

  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE, 1);
  result = ctxt.compile ();

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
:func:`gccjit::context::set_int_option` with
:c:macro:`GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL`:

.. code-block:: c++

  ctxt.set_int_option (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 3);

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

   .. literalinclude:: ../../examples/tut02-square.cc
    :lines: 1-
    :language: c++

Building and running it:

.. code-block:: console

  $ gcc \
      tut02-square.cc \
      -o tut02-square \
      -lgccjit

  # Run the built program:
  $ ./tut02-square
  result: 25
