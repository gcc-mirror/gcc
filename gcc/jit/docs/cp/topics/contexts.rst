.. Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

Compilation contexts
====================

.. class:: gccjit::context

The top-level of the C++ API is the :class:`gccjit::context` type.

A :class:`gccjit::context` instance encapsulates the state of a
compilation.

You can set up options on it, and add types, functions and code.
Invoking :func:`gccjit::context::compile` on it gives you a
:c:expr:`gcc_jit_result *`.

It is a thin wrapper around the C API's :c:expr:`gcc_jit_context *`.

Lifetime-management
-------------------
Contexts are the unit of lifetime-management within the API: objects
have their lifetime bounded by the context they are created within, and
cleanup of such objects is done for you when the context is released.

.. function:: gccjit::context gccjit::context::acquire ()

  This function acquires a new :class:`gccjit::context` instance,
  which is independent of any others that may be present within this
  process.

.. function:: void gccjit::context::release ()

  This function releases all resources associated with the given context.
  Both the context itself and all of its :expr:`gccjit::object *`
  instances are cleaned up.  It should be called exactly once on a given
  context.

  It is invalid to use the context or any of its "contextual" objects
  after calling this.

  .. code-block:: c++

    ctxt.release ();

.. function:: gccjit::context \
              gccjit::context::new_child_context ()

   Given an existing JIT context, create a child context.

   The child inherits a copy of all option-settings from the parent.

   The child can reference objects created within the parent, but not
   vice-versa.

   The lifetime of the child context must be bounded by that of the
   parent: you should release a child context before releasing the parent
   context.

   If you use a function from a parent context within a child context,
   you have to compile the parent context before you can compile the
   child context, and the gccjit::result of the parent context must
   outlive the gccjit::result of the child context.

   This allows caching of shared initializations.  For example, you could
   create types and declarations of global functions in a parent context
   once within a process, and then create child contexts whenever a
   function or loop becomes hot. Each such child context can be used for
   JIT-compiling just one function or loop, but can reference types
   and helper functions created within the parent context.

   Contexts can be arbitrarily nested, provided the above rules are
   followed, but it's probably not worth going above 2 or 3 levels, and
   there will likely be a performance hit for such nesting.


Thread-safety
-------------
Instances of :class:`gccjit::context` created via
:func:`gccjit::context::acquire` are independent from each other:
only one thread may use a given context at once, but multiple threads
could each have their own contexts without needing locks.

Contexts created via :func:`gccjit::context::new_child_context` are
related to their parent context.  They can be partitioned by their
ultimate ancestor into independent "family trees".   Only one thread
within a process may use a given "family tree" of such contexts at once,
and if you're using multiple threads you should provide your own locking
around entire such context partitions.


Error-handling
--------------
.. FIXME: How does error-handling work for C++ API?

You can only compile and get code from a context if no errors occur.

In general, if an error occurs when using an API entrypoint, it returns
NULL.  You don't have to check everywhere for NULL results, since the
API gracefully handles a NULL being passed in for any argument.

Errors are printed on stderr and can be queried using
:func:`gccjit::context::get_first_error`.

.. function:: const char *\
              gccjit::context::get_first_error (gccjit::context *ctxt)

   Returns the first error message that occurred on the context.

   The returned string is valid for the rest of the lifetime of the
   context.

   If no errors occurred, this will be NULL.

Debugging
---------

.. function:: void\
              gccjit::context::dump_to_file (const std::string &path, \
                                             int update_locations)

   To help with debugging: dump a C-like representation to the given path,
   describing what's been set up on the context.

   If "update_locations" is true, then also set up :class:`gccjit::location`
   information throughout the context, pointing at the dump file as if it
   were a source file.  This may be of use in conjunction with
   :c:macro:`GCC_JIT_BOOL_OPTION_DEBUGINFO` to allow stepping through the
   code in a debugger.

.. function:: void\
              gccjit::context::dump_reproducer_to_file (gcc_jit_context *ctxt,\
                                                        const char *path)

   This is a thin wrapper around the C API
   :c:func:`gcc_jit_context_dump_reproducer_to_file`, and hence works the
   same way.

   Note that the generated source is C code, not C++; this might be of use
   for seeing what the C++ bindings are doing at the C level.

Options
-------

String Options
**************

.. function:: void \
              gccjit::context::set_str_option (enum gcc_jit_str_option, \
                                               const char *value)

   Set a string option of the context.

   This is a thin wrapper around the C API
   :c:func:`gcc_jit_context_set_str_option`; the options have the same
   meaning.

Boolean options
***************

.. function:: void \
              gccjit::context::set_bool_option(enum gcc_jit_bool_option, \
                                               int value)

  Set a boolean option of the context.

  This is a thin wrapper around the C API
  :c:func:`gcc_jit_context_set_bool_option`; the options have the same
  meaning.

.. function:: void \
              gccjit::context::set_bool_allow_unreachable_blocks (int bool_value)

   By default, libgccjit will issue an error about unreachable blocks
   within a function.

   This entrypoint can be used to disable that error; it is a thin wrapper
   around the C API
   :c:func:`gcc_jit_context_set_bool_allow_unreachable_blocks`.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_2`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_allow_unreachable_blocks

.. function:: void \
              gccjit::context::set_bool_use_external_driver (int bool_value)

   libgccjit internally generates assembler, and uses "driver" code
   for converting it to other formats (e.g. shared libraries).

   By default, libgccjit will use an embedded copy of the driver
   code.

   This option can be used to instead invoke an external driver executable
   as a subprocess; it is a thin wrapper around the C API
   :c:func:`gcc_jit_context_set_bool_use_external_driver`.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_5`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_set_bool_use_external_driver

Integer options
***************

.. function:: void \
              gccjit::context::set_int_option (enum gcc_jit_int_option, \
                                               int value)

  Set an integer option of the context.

  This is a thin wrapper around the C API
  :c:func:`gcc_jit_context_set_int_option`; the options have the same
  meaning.

Additional command-line options
*******************************

.. function:: void \
              gccjit::context::add_command_line_option (const char *optname)

   Add an arbitrary gcc command-line option to the context for use
   when compiling.

   This is a thin wrapper around the C API
   :c:func:`gcc_jit_context_add_command_line_option`.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_1`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
