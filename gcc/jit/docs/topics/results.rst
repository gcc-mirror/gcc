.. Copyright (C) 2014 Free Software Foundation, Inc.
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

Compilation results
===================

.. type:: gcc_jit_result

  A `gcc_jit_result` encapsulates the result of compiling a context,
  and the lifetimes of any machine code functions that are
  returned.

.. function:: gcc_jit_result *\
              gcc_jit_context_compile (gcc_jit_context *ctxt)

   This calls into GCC and builds the code, returning a
   `gcc_jit_result *`.


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
   `gcc_jit_result *`; attempting to call it after that may lead
   to a segmentation fault.


.. function:: void\
              gcc_jit_result_release (gcc_jit_result *result)

   Once we're done with the code, this unloads the built .so file.
   This cleans up the result; after calling this, it's no longer
   valid to use the result, or any code that was obtained by calling
   :func:`gcc_jit_result_get_code` on it.
