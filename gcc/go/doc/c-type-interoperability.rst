..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c-type-interoperability:

C Type Interoperability
***********************

Basic types map directly: an ``int`` in Go is an ``int`` in C,
etc.  Go ``byte`` is equivalent to C ``unsigned char``.
Pointers in Go are pointers in C.  A Go ``struct`` is the same as C
``struct`` with the same field names and types.

.. index:: string in C

The Go ``string`` type is currently defined as a two-element
structure:

.. code-block:: c++

  struct __go_string {
    const unsigned char *__data;
    int __length;
  };

You can't pass arrays between C and Go.  However, a pointer to an
array in Go is equivalent to a C pointer to the equivalent of the
element type.  For example, Go ``*[10]int`` is equivalent to C
``int*``, assuming that the C pointer does point to 10 elements.

.. index:: slice in C

A slice in Go is a structure.  The current definition is:

.. code-block:: c++

  struct __go_slice {
    void *__values;
    int __count;
    int __capacity;
  };

The type of a Go function with no receiver is equivalent to a C
function whose parameter types are equivalent.  When a Go function
returns more than one value, the C function returns a struct.  For
example, these functions have equivalent types:

.. code-block:: c++

  func GoFunction(int) (int, float)
  struct { int i; float f; } CFunction(int)

A pointer to a Go function is equivalent to a pointer to a C function
when the functions have equivalent types.

Go ``interface``, ``channel``, and ``map`` types have no
corresponding C type (``interface`` is a two-element struct and
``channel`` and ``map`` are pointers to structs in C, but the
structs are deliberately undocumented).  C ``enum`` types
correspond to some integer type, but precisely which one is difficult
to predict in general; use a cast.  C ``union`` types have no
corresponding Go type.  C ``struct`` types containing bitfields
have no corresponding Go type.  C++ ``class`` types have no
corresponding Go type.

Memory allocation is completely different between C and Go, as Go uses
garbage collection.  The exact guidelines in this area are
undetermined, but it is likely that it will be permitted to pass a
pointer to allocated memory from C to Go.  The responsibility of
eventually freeing the pointer will remain with C side, and of course
if the C side frees the pointer while the Go side still has a copy the
program will fail.  When passing a pointer from Go to C, the Go
function must retain a visible copy of it in some Go variable.
Otherwise the Go garbage collector may delete the pointer while the C
function is still using it.
