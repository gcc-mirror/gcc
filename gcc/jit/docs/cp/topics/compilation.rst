.. Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

Compiling a context
===================

Once populated, a :class:`gccjit::context` can be compiled to
machine code, either in-memory via :func:`gccjit::context::compile` or
to disk via :func:`gccjit::context::compile_to_file`.

You can compile a context multiple times (using either form of
compilation), although any errors that occur on the context will
prevent any future compilation of that context.

In-memory compilation
*********************

.. function:: gcc_jit_result *\
              gccjit::context::compile ()

   This calls into GCC and builds the code, returning a
   `gcc_jit_result *`.

   This is a thin wrapper around the
   :c:func:`gcc_jit_context_compile` API entrypoint.

Ahead-of-time compilation
*************************

Although libgccjit is primarily aimed at just-in-time compilation, it
can also be used for implementing more traditional ahead-of-time
compilers, via the :func:`gccjit::context::compile_to_file` method.

.. function:: void \
              gccjit::context::compile_to_file (enum gcc_jit_output_kind,\
                                                const char *output_path)

   Compile the :class:`gccjit::context` to a file of the given
   kind.

   This is a thin wrapper around the
   :c:func:`gcc_jit_context_compile_to_file` API entrypoint.
