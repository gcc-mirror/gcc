.. Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

Types
=====

.. class:: gccjit::type

   gccjit::type represents a type within the library.  It is a subclass
   of :class:`gccjit::object`.

Types can be created in several ways:

* fundamental types can be accessed using
  :func:`gccjit::context::get_type`:

  .. code-block:: c++

      gccjit::type int_type = ctxt.get_type (GCC_JIT_TYPE_INT);

  or using the :func:`gccjit::context::get_int_type<T>` template:

  .. code-block:: c++

      gccjit::type t = ctxt.get_int_type <unsigned short> ();

  See :c:func:`gcc_jit_context_get_type` for the available types.

* derived types can be accessed by using functions such as
  :func:`gccjit::type::get_pointer` and :func:`gccjit::type::get_const`:

  .. code-block:: c++

    gccjit::type const_int_star = int_type.get_const ().get_pointer ();
    gccjit::type int_const_star = int_type.get_pointer ().get_const ();

* by creating structures (see below).

Standard types
--------------

.. function:: gccjit::type gccjit::context::get_type (enum gcc_jit_types)

   Access a specific type.  This is a thin wrapper around
   :c:func:`gcc_jit_context_get_type`; the parameter has the same meaning.

.. function:: gccjit::type \
              gccjit::context::get_int_type (size_t num_bytes, int is_signed)

   Access the integer type of the given size.

.. function:: gccjit::type \
              gccjit::context::get_int_type <T> ()

   Access the given integer type.  For example, you could map the
   ``unsigned short`` type into a gccjit::type via:

   .. code-block:: c++

      gccjit::type t = ctxt.get_int_type <unsigned short> ();

Pointers, `const`, and `volatile`
---------------------------------

.. function::  gccjit::type gccjit::type::get_pointer ()

   Given type "T", get type "T*".

.. function::  gccjit::type gccjit::type::get_const ()

   Given type "T", get type "const T".

.. function::  gccjit::type gccjit::type::get_volatile ()

   Given type "T", get type "volatile T".

.. function::  gccjit::type gccjit::type::get_aligned (size_t alignment_in_bytes)

   Given type "T", get type:

   .. code-block:: c

      T __attribute__ ((aligned (ALIGNMENT_IN_BYTES)))

   The alignment must be a power of two.

.. function::  gccjit::type \
               gccjit::context::new_array_type (gccjit::type element_type, \
                                                int num_elements, \
			                        gccjit::location loc)

   Given type "T", get type "T[N]" (for a constant N).
   Param "loc" is optional.


Vector types
------------

.. function::  gccjit::type gccjit::type::get_vector (size_t num_units)

   Given type "T", get type:

   .. code-block:: c

      T  __attribute__ ((vector_size (sizeof(T) * num_units))

   T must be integral or floating point; num_units must be a power of two.


Structures and unions
---------------------

.. class:: gccjit::struct_

A compound type analagous to a C `struct`.

:class:`gccjit::struct_` is a subclass of :class:`gccjit::type` (and thus
of :class:`gccjit::object` in turn).

.. class:: gccjit::field

A field within a :class:`gccjit::struct_`.

:class:`gccjit::field` is a subclass of :class:`gccjit::object`.

You can model C `struct` types by creating :class:`gccjit::struct_` and
:class:`gccjit::field` instances, in either order:

* by creating the fields, then the structure.  For example, to model:

  .. code-block:: c

    struct coord {double x; double y; };

  you could call:

  .. code-block:: c++

    gccjit::field field_x = ctxt.new_field (double_type, "x");
    gccjit::field field_y = ctxt.new_field (double_type, "y");
    std::vector fields;
    fields.push_back (field_x);
    fields.push_back (field_y);
    gccjit::struct_ coord = ctxt.new_struct_type ("coord", fields);

* by creating the structure, then populating it with fields, typically
  to allow modelling self-referential structs such as:

  .. code-block:: c

    struct node { int m_hash; struct node *m_next; };

  like this:

  .. code-block:: c++

    gccjit::struct_ node = ctxt.new_opaque_struct_type ("node");
    gccjit::type node_ptr = node.get_pointer ();
    gccjit::field field_hash = ctxt.new_field (int_type, "m_hash");
    gccjit::field field_next = ctxt.new_field (node_ptr, "m_next");
    std::vector fields;
    fields.push_back (field_hash);
    fields.push_back (field_next);
    node.set_fields (fields);

.. FIXME: the above API doesn't seem to exist yet

.. function:: gccjit::field \
              gccjit::context::new_field (gccjit::type type,\
                                          const char *name, \
                                          gccjit::location loc)

   Construct a new field, with the given type and name.

.. function:: gccjit::struct_ \
   gccjit::context::new_struct_type (const std::string &name,\
                                     std::vector<field> &fields,\
                                     gccjit::location loc)

     Construct a new struct type, with the given name and fields.

.. function:: gccjit::struct_ \
              gccjit::context::new_opaque_struct (const std::string &name, \
                                                  gccjit::location loc)

     Construct a new struct type, with the given name, but without
     specifying the fields.   The fields can be omitted (in which case the
     size of the struct is not known), or later specified using
     :c:func:`gcc_jit_struct_set_fields`.
