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

Objects
=======

.. class:: gccjit::object

Almost every entity in the API (with the exception of
:class:`gccjit::context` and :c:expr:`gcc_jit_result *`) is a
"contextual" object, a :class:`gccjit::object`.

A JIT object:

  * is associated with a :class:`gccjit::context`.

  * is automatically cleaned up for you when its context is released so
    you don't need to manually track and cleanup all objects, just the
    contexts.

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
      +- case_

The :class:`gccjit::object` base class has the following operations:

.. function:: gccjit::context gccjit::object::get_context () const

  Which context is the obj within?

.. function:: std::string gccjit::object::get_debug_string () const

  Generate a human-readable description for the given object.

  For example,

  .. code-block:: c++

     printf ("obj: %s\n", obj.get_debug_string ().c_str ());

  might give this text on stdout:

  .. code-block:: bash

     obj: 4.0 * (float)i
