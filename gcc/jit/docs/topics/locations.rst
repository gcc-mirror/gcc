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

.. default-domain:: c

Source Locations
================

.. type:: gcc_jit_location

   A `gcc_jit_location` encapsulates a source code location, so that
   you can (optionally) associate locations in your language with
   statements in the JIT-compiled code, allowing the debugger to
   single-step through your language.

   `gcc_jit_location` instances are optional: you can always pass NULL to
   any API entrypoint accepting one.

   You can construct them using :c:func:`gcc_jit_context_new_location`.

   You need to enable :c:macro:`GCC_JIT_BOOL_OPTION_DEBUGINFO` on the
   :c:type:`gcc_jit_context` for these locations to actually be usable by
   the debugger:

   .. code-block:: c

     gcc_jit_context_set_bool_option (
       ctxt,
       GCC_JIT_BOOL_OPTION_DEBUGINFO,
       1);

.. function:: gcc_jit_location *\
              gcc_jit_context_new_location (gcc_jit_context *ctxt,\
                                            const char *filename,\
                                            int line,\
                                            int column)

   Create a `gcc_jit_location` instance representing the given source
   location.

   The parameter ``filename`` must be non-NULL.  The call takes a copy of
   the underlying string, so it is valid to pass in a pointer to an
   on-stack buffer.

Faking it
---------
If you don't have source code for your internal representation, but need
to debug, you can generate a C-like representation of the functions in
your context using :c:func:`gcc_jit_context_dump_to_file()`:

.. code-block:: c

  gcc_jit_context_dump_to_file (ctxt, "/tmp/something.c",
                                1 /* update_locations */);

This will dump C-like code to the given path.  If the `update_locations`
argument is true, this will also set up `gcc_jit_location` information
throughout the context, pointing at the dump file as if it were a source
file, giving you *something* you can step through in the debugger.
