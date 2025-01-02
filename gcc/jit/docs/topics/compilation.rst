.. Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

Compiling a context
===================

Once populated, a :c:expr:`gcc_jit_context *` can be compiled to
machine code, either in-memory via :c:func:`gcc_jit_context_compile` or
to disk via :c:func:`gcc_jit_context_compile_to_file`.

You can compile a context multiple times (using either form of
compilation), although any errors that occur on the context will
prevent any future compilation of that context.

In-memory compilation
*********************

.. function:: gcc_jit_result *\
              gcc_jit_context_compile (gcc_jit_context *ctxt)

   This calls into GCC and builds the code, returning a
   `gcc_jit_result *`.

   If the result is non-NULL, the caller becomes responsible for
   calling :func:`gcc_jit_result_release` on it once they're done
   with it.

.. type:: gcc_jit_result

  A `gcc_jit_result` encapsulates the result of compiling a context
  in-memory, and the lifetimes of any machine code functions or globals
  that are within the result.

.. function:: void *\
              gcc_jit_result_get_code (gcc_jit_result *result,\
                                       const char *funcname)

   Locate a given function within the built machine code.

   Functions are looked up by name.  For this to succeed, a function
   with a name matching `funcname` must have been created on
   `result`'s context (or a parent context) via a call to
   :func:`gcc_jit_context_new_function` with `kind`
   :macro:`GCC_JIT_FUNCTION_EXPORTED`:

   .. code-block:: c

     gcc_jit_context_new_function (ctxt,
                                   any_location, /* or NULL */
                                   /* Required for func to be visible to
                                      gcc_jit_result_get_code: */
                                   GCC_JIT_FUNCTION_EXPORTED,
                                   any_return_type,
                                   /* Must string-compare equal: */
                                   funcname,
                                   /* etc */);

   If such a function is not found (or `result` or `funcname` are
   ``NULL``), an error message will be emitted on stderr and
   ``NULL`` will be returned.

   If the function is found, the result will need to be cast to a
   function pointer of the correct type before it can be called.

   Note that the resulting machine code becomes invalid after
   :func:`gcc_jit_result_release` is called on the
   :expr:`gcc_jit_result *`; attempting to call it after that may lead
   to a segmentation fault.

.. function:: void *\
              gcc_jit_result_get_global (gcc_jit_result *result,\
                                         const char *name)

   Locate a given global within the built machine code.

   Globals are looked up by name.  For this to succeed, a global
   with a name matching `name` must have been created on
   `result`'s context (or a parent context) via a call to
   :func:`gcc_jit_context_new_global` with `kind`
   :macro:`GCC_JIT_GLOBAL_EXPORTED`.

   If the global is found, the result will need to be cast to a
   pointer of the correct type before it can be called.

   This is a *pointer* to the global, so e.g. for an :expr:`int` this is
   an :expr:`int *`.

   For example, given an ``int foo;`` created this way:

   .. code-block:: c

     gcc_jit_lvalue *exported_global =
       gcc_jit_context_new_global (ctxt,
       any_location, /* or NULL */
       GCC_JIT_GLOBAL_EXPORTED,
       int_type,
       "foo");

   we can access it like this:

   .. code-block:: c

      int *ptr_to_foo =
        (int *)gcc_jit_result_get_global (result, "foo");

   If such a global is not found (or `result` or `name` are
   ``NULL``), an error message will be emitted on stderr and
   ``NULL`` will be returned.

   Note that the resulting address becomes invalid after
   :func:`gcc_jit_result_release` is called on the
   :expr:`gcc_jit_result *`; attempting to use it after that may lead
   to a segmentation fault.

.. function:: void\
              gcc_jit_result_release (gcc_jit_result *result)

   Once we're done with the code, this unloads the built .so file.
   This cleans up the result; after calling this, it's no longer
   valid to use the result, or any code or globals that were obtained
   by calling :func:`gcc_jit_result_get_code` or
   :func:`gcc_jit_result_get_global` on it.


Ahead-of-time compilation
*************************

Although libgccjit is primarily aimed at just-in-time compilation, it
can also be used for implementing more traditional ahead-of-time
compilers, via the :c:func:`gcc_jit_context_compile_to_file`
API entrypoint.

For linking in object files, use :c:func:`gcc_jit_context_add_driver_option`.

.. function:: void \
              gcc_jit_context_compile_to_file (gcc_jit_context *ctxt, \
                                               enum gcc_jit_output_kind output_kind,\
                                               const char *output_path)

   Compile the :c:expr:`gcc_jit_context *` to a file of the given
   kind.

:c:func:`gcc_jit_context_compile_to_file` ignores the suffix of
``output_path``, and insteads uses the given
:c:enum:`gcc_jit_output_kind` to decide what to do.

.. note::

   This is different from the ``gcc`` program, which does make use of the
   suffix of the output file when determining what to do.

.. enum:: gcc_jit_output_kind

The available kinds of output are:

.. list-table::
   :header-rows: 1

   * - Output kind
     - Typical suffix

   * - :c:macro:`GCC_JIT_OUTPUT_KIND_ASSEMBLER`
     - .s
   * - :c:macro:`GCC_JIT_OUTPUT_KIND_OBJECT_FILE`
     - .o
   * - :c:macro:`GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY`
     - .so or .dll
   * - :c:macro:`GCC_JIT_OUTPUT_KIND_EXECUTABLE`
     - None, or .exe

.. c:macro:: GCC_JIT_OUTPUT_KIND_ASSEMBLER

   Compile the context to an assembler file.

.. c:macro:: GCC_JIT_OUTPUT_KIND_OBJECT_FILE

   Compile the context to an object file.

.. c:macro:: GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY

   Compile the context to a dynamic library.

.. c:macro:: GCC_JIT_OUTPUT_KIND_EXECUTABLE

   Compile the context to an executable.
