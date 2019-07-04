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

Types
=====

.. c:type:: gcc_jit_type

   gcc_jit_type represents a type within the library.

.. function:: gcc_jit_object *gcc_jit_type_as_object (gcc_jit_type *type)

   Upcast a type to an object.

Types can be created in several ways:

* fundamental types can be accessed using
  :func:`gcc_jit_context_get_type`:

  .. code-block:: c

      gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  See :func:`gcc_jit_context_get_type` for the available types.

* derived types can be accessed by using functions such as
  :func:`gcc_jit_type_get_pointer` and :func:`gcc_jit_type_get_const`:

  .. code-block:: c

    gcc_jit_type *const_int_star = gcc_jit_type_get_pointer (gcc_jit_type_get_const (int_type));
    gcc_jit_type *int_const_star = gcc_jit_type_get_const (gcc_jit_type_get_pointer (int_type));

* by creating structures (see below).

Standard types
--------------

.. function:: gcc_jit_type *gcc_jit_context_get_type (gcc_jit_context *ctxt, \
                                                      enum gcc_jit_types type_)

   Access a specific type.  The available types are:

   ==========================================  ================================
   `enum gcc_jit_types` value                  Meaning
   ==========================================  ================================
   :c:data:`GCC_JIT_TYPE_VOID`                 C's ``void`` type.
   :c:data:`GCC_JIT_TYPE_VOID_PTR`             C's ``void *``.
   :c:data:`GCC_JIT_TYPE_BOOL`                 C++'s ``bool`` type; also C99's
                                               ``_Bool`` type, aka ``bool`` if
                                               using stdbool.h.
   :c:data:`GCC_JIT_TYPE_CHAR`                 C's ``char`` (of some signedness)
   :c:data:`GCC_JIT_TYPE_SIGNED_CHAR`          C's ``signed char``
   :c:data:`GCC_JIT_TYPE_UNSIGNED_CHAR`        C's ``unsigned char``
   :c:data:`GCC_JIT_TYPE_SHORT`                C's ``short`` (signed)
   :c:data:`GCC_JIT_TYPE_UNSIGNED_SHORT`       C's ``unsigned short``
   :c:data:`GCC_JIT_TYPE_INT`                  C's ``int`` (signed)
   :c:data:`GCC_JIT_TYPE_UNSIGNED_INT`         C's ``unsigned int``
   :c:data:`GCC_JIT_TYPE_LONG`                 C's ``long`` (signed)
   :c:data:`GCC_JIT_TYPE_UNSIGNED_LONG`        C's ``unsigned long``
   :c:data:`GCC_JIT_TYPE_LONG_LONG`            C99's ``long long`` (signed)
   :c:data:`GCC_JIT_TYPE_UNSIGNED_LONG_LONG`   C99's ``unsigned long long``
   :c:data:`GCC_JIT_TYPE_FLOAT`
   :c:data:`GCC_JIT_TYPE_DOUBLE`
   :c:data:`GCC_JIT_TYPE_LONG_DOUBLE`
   :c:data:`GCC_JIT_TYPE_CONST_CHAR_PTR`       C type: ``(const char *)``
   :c:data:`GCC_JIT_TYPE_SIZE_T`               C's ``size_t`` type
   :c:data:`GCC_JIT_TYPE_FILE_PTR`             C type: ``(FILE *)``
   :c:data:`GCC_JIT_TYPE_COMPLEX_FLOAT`        C99's ``_Complex float``
   :c:data:`GCC_JIT_TYPE_COMPLEX_DOUBLE`       C99's ``_Complex double``
   :c:data:`GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE`  C99's ``_Complex long double``
   ==========================================  ================================

.. function:: gcc_jit_type *\
              gcc_jit_context_get_int_type (gcc_jit_context *ctxt, \
                                            int num_bytes, int is_signed)

   Access the integer type of the given size.


Pointers, `const`, and `volatile`
---------------------------------

.. function::  gcc_jit_type *gcc_jit_type_get_pointer (gcc_jit_type *type)

   Given type "T", get type "T*".

.. function::  gcc_jit_type *gcc_jit_type_get_const (gcc_jit_type *type)

   Given type "T", get type "const T".

.. function::  gcc_jit_type *gcc_jit_type_get_volatile (gcc_jit_type *type)

   Given type "T", get type "volatile T".

.. function::  gcc_jit_type *\
               gcc_jit_context_new_array_type (gcc_jit_context *ctxt, \
                                               gcc_jit_location *loc, \
                                               gcc_jit_type *element_type, \
                                               int num_elements)

   Given type "T", get type "T[N]" (for a constant N).

.. function::  gcc_jit_type *\
               gcc_jit_type_get_aligned (gcc_jit_type *type, \
                                         size_t alignment_in_bytes)

   Given type "T", get type:

   .. code-block:: c

      T __attribute__ ((aligned (ALIGNMENT_IN_BYTES)))

   The alignment must be a power of two.

   This entrypoint was added in :ref:`LIBGCCJIT_ABI_7`; you can test for
   its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_aligned

Vector types
------------

.. function::  gcc_jit_type *\
               gcc_jit_type_get_vector (gcc_jit_type *type, \
                                        size_t num_units)

   Given type "T", get type:

   .. code-block:: c

      T  __attribute__ ((vector_size (sizeof(T) * num_units))

   T must be integral or floating point; num_units must be a power of two.

   This can be used to construct a vector type in which operations
   are applied element-wise.  The compiler will automatically
   use SIMD instructions where possible.  See:
   https://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html

   For example, assuming 4-byte ``ints``, then:

   .. code-block:: c

      typedef int v4si __attribute__ ((vector_size (16)));

   can be obtained using:

   .. code-block:: c

      gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt,
                                                         GCC_JIT_TYPE_INT);
      gcc_jit_type *v4si_type = gcc_jit_type_get_vector (int_type, 4);

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_8`; you can test
   for its presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_type_get_vector

   Vector rvalues can be generated using
   :func:`gcc_jit_context_new_rvalue_from_vector`.


Structures and unions
---------------------

.. c:type:: gcc_jit_struct

A compound type analagous to a C `struct`.

.. c:type:: gcc_jit_field

A field within a :c:type:`gcc_jit_struct`.

You can model C `struct` types by creating :c:type:`gcc_jit_struct *` and
:c:type:`gcc_jit_field` instances, in either order:

* by creating the fields, then the structure.  For example, to model:

  .. code-block:: c

    struct coord {double x; double y; };

  you could call:

  .. code-block:: c

    gcc_jit_field *field_x =
      gcc_jit_context_new_field (ctxt, NULL, double_type, "x");
    gcc_jit_field *field_y =
      gcc_jit_context_new_field (ctxt, NULL, double_type, "y");
    gcc_jit_field *fields[2] = {field_x, field_y};
    gcc_jit_struct *coord =
      gcc_jit_context_new_struct_type (ctxt, NULL, "coord", 2, fields);

* by creating the structure, then populating it with fields, typically
  to allow modelling self-referential structs such as:

  .. code-block:: c

    struct node { int m_hash; struct node *m_next; };

  like this:

  .. code-block:: c

    gcc_jit_type *node =
      gcc_jit_context_new_opaque_struct (ctxt, NULL, "node");
    gcc_jit_type *node_ptr =
      gcc_jit_type_get_pointer (node);
    gcc_jit_field *field_hash =
      gcc_jit_context_new_field (ctxt, NULL, int_type, "m_hash");
    gcc_jit_field *field_next =
      gcc_jit_context_new_field (ctxt, NULL, node_ptr, "m_next");
    gcc_jit_field *fields[2] = {field_hash, field_next};
    gcc_jit_struct_set_fields (node, NULL, 2, fields);

.. function:: gcc_jit_field *\
              gcc_jit_context_new_field (gcc_jit_context *ctxt,\
                                         gcc_jit_location *loc,\
                                         gcc_jit_type *type,\
                                         const char *name)

   Construct a new field, with the given type and name.

   The parameter ``name`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

.. function:: gcc_jit_field *\
              gcc_jit_context_new_bitfield (gcc_jit_context *ctxt,\
                                            gcc_jit_location *loc,\
                                            gcc_jit_type *type,\
                                            int width,\
                                            const char *name)

   Construct a new bit field, with the given type width and name.

   The parameter ``name`` must be non-NULL.  The call takes a copy of the
   underlying string, so it is valid to pass in a pointer to an on-stack
   buffer.

   The parameter ``type`` must be an integer type.

   The parameter ``width`` must be a positive integer that does not exceed the
   size of ``type``.

   This API entrypoint was added in :ref:`LIBGCCJIT_ABI_12`; you can test
   for its presence using
   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_gcc_jit_context_new_bitfield

.. function:: gcc_jit_object *\
              gcc_jit_field_as_object (gcc_jit_field *field)

   Upcast from field to object.

.. function:: gcc_jit_struct *\
   gcc_jit_context_new_struct_type (gcc_jit_context *ctxt,\
                                    gcc_jit_location *loc,\
                                    const char *name,\
                                    int num_fields,\
                                    gcc_jit_field **fields)

     Construct a new struct type, with the given name and fields.

     The parameter ``name`` must be non-NULL.  The call takes a copy of
     the underlying string, so it is valid to pass in a pointer to an
     on-stack buffer.

.. function:: gcc_jit_struct *\
              gcc_jit_context_new_opaque_struct (gcc_jit_context *ctxt,\
                                                 gcc_jit_location *loc,\
                                                 const char *name)

     Construct a new struct type, with the given name, but without
     specifying the fields.   The fields can be omitted (in which case the
     size of the struct is not known), or later specified using
     :c:func:`gcc_jit_struct_set_fields`.

     The parameter ``name`` must be non-NULL.  The call takes a copy of
     the underlying string, so it is valid to pass in a pointer to an
     on-stack buffer.

.. function:: gcc_jit_type *\
              gcc_jit_struct_as_type (gcc_jit_struct *struct_type)

   Upcast from struct to type.

.. function:: void\
              gcc_jit_struct_set_fields (gcc_jit_struct *struct_type,\
                                         gcc_jit_location *loc,\
                                         int num_fields,\
                                         gcc_jit_field **fields)

   Populate the fields of a formerly-opaque struct type.

   This can only be called once on a given struct type.

.. function:: gcc_jit_type *\
              gcc_jit_context_new_union_type (gcc_jit_context *ctxt,\
                                              gcc_jit_location *loc,\
                                              const char *name,\
                                              int num_fields,\
                                              gcc_jit_field **fields)

     Construct a new union type, with the given name and fields.

     The parameter ``name`` must be non-NULL.  It is copied, so the input
     buffer does not need to outlive the call.

     Example of use:

     .. literalinclude:: ../../../testsuite/jit.dg/test-accessing-union.c
       :start-after: /* Quote from here in docs/topics/types.rst.  */
       :end-before: /* Quote up to here in docs/topics/types.rst.  */
       :language: c

Function pointer types
----------------------

Function pointer types can be created using
:c:func:`gcc_jit_context_new_function_ptr_type`.
