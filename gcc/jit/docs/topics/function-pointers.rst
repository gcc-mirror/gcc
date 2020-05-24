.. Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

Function pointers
=================

You can generate calls that use a function pointer via
:c:func:`gcc_jit_context_new_call_through_ptr`.

To do requires a :c:type:`gcc_jit_rvalue` of the correct function pointer type.

Function pointers for a :c:type:`gcc_jit_function` can be obtained
via :c:func:`gcc_jit_function_get_address`.

.. function:: gcc_jit_rvalue *\
	      gcc_jit_function_get_address (gcc_jit_function *fn,\
                                            gcc_jit_location *loc)

   Get the address of a function as an rvalue, of function pointer
   type.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_9`; you can test
   for its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_function_get_address

Alternatively, given an existing function, you can obtain a pointer
to it in :c:type:`gcc_jit_rvalue` form using
:c:func:`gcc_jit_context_new_rvalue_from_ptr`, using a function pointer
type obtained using :c:func:`gcc_jit_context_new_function_ptr_type`.

Here's an example of creating a function pointer type corresponding to C's
:c:type:`void (*) (int, int, int)`:

.. code-block:: c

  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the function ptr type.  */
  gcc_jit_type *param_types[3];
  param_types[0] = int_type;
  param_types[1] = int_type;
  param_types[2] = int_type;

  gcc_jit_type *fn_ptr_type =
    gcc_jit_context_new_function_ptr_type (ctxt, NULL,
					   void_type,
					   3, param_types, 0);

.. function:: gcc_jit_type *\
	      gcc_jit_context_new_function_ptr_type (gcc_jit_context *ctxt,\
				       gcc_jit_location *loc,\
				       gcc_jit_type *return_type,\
				       int num_params,\
				       gcc_jit_type **param_types,\
				       int is_variadic)

   Generate a :c:type:`gcc_jit_type` for a function pointer with the
   given return type and parameters.

   Each of `param_types` must be non-`void`; `return_type` may be `void`.
