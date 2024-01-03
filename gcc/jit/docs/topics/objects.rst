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

.. default-domain:: c

Objects
=======

.. type:: gcc_jit_object

Almost every entity in the API (with the exception of
:c:expr:`gcc_jit_context *` and :c:expr:`gcc_jit_result *`) is a
"contextual" object, a :c:expr:`gcc_jit_object *`

A JIT object:

  * is associated with a :c:expr:`gcc_jit_context *`.

  * is automatically cleaned up for you when its context is released so
    you don't need to manually track and cleanup all objects, just the
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
      +- gcc_jit_case
      +- gcc_jit_extended_asm

There are casting methods for upcasting from subclasses to parent classes.
For example, :c:func:`gcc_jit_type_as_object`:

.. code-block:: c

   gcc_jit_object *obj = gcc_jit_type_as_object (int_type);

The object "base class" has the following operations:

.. function:: gcc_jit_context *gcc_jit_object_get_context (gcc_jit_object *obj)

  Which context is "obj" within?


.. function:: const char *gcc_jit_object_get_debug_string (gcc_jit_object *obj)

  Generate a human-readable description for the given object.

  For example,

  .. code-block:: c

     printf ("obj: %s\n", gcc_jit_object_get_debug_string (obj));

  might give this text on stdout:

  .. code-block:: bash

     obj: 4.0 * (float)i

  .. note::

     If you call this on an object, the `const char *` buffer is allocated
     and generated on the first call for that object, and the buffer will
     have the same lifetime as the object  i.e. it will exist until the
     object's context is released.
