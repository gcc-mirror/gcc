..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: vague linkage

.. _vague-linkage:

Vague Linkage
*************

There are several constructs in C++ that require space in the object
file but are not clearly tied to a single translation unit.  We say that
these constructs have 'vague linkage'.  Typically such constructs are
emitted wherever they are needed, though sometimes we can be more
clever.

Inline Functions
  Inline functions are typically defined in a header file which can be
  included in many different compilations.  Hopefully they can usually be
  inlined, but sometimes an out-of-line copy is necessary, if the address
  of the function is taken or if inlining fails.  In general, we emit an
  out-of-line copy in all translation units where one is needed.  As an
  exception, we only emit inline virtual functions with the vtable, since
  it always requires a copy.

  Local static variables and string constants used in an inline function
  are also considered to have vague linkage, since they must be shared
  between all inlined and out-of-line instances of the function.

VTables
  C++ virtual functions are implemented in most compilers using a lookup
  table, known as a vtable.  The vtable contains pointers to the virtual
  functions provided by a class, and each object of the class contains a
  pointer to its vtable (or vtables, in some multiple-inheritance
  situations).  If the class declares any non-inline, non-pure virtual
  functions, the first one is chosen as the 'key method' for the class,
  and the vtable is only emitted in the translation unit where the key
  method is defined.

  .. note::

    If the chosen key method is later defined as inline, the
    vtable is still emitted in every translation unit that defines it.
    Make sure that any inline virtuals are declared inline in the class
    body, even if they are not defined there.

:samp:`{type_info} objects`
  C++ requires information about types to be written out in order to
  implement :samp:`dynamic_cast`, :samp:`typeid` and exception handling.
  For polymorphic classes (classes with virtual functions), the :samp:`type_info`
  object is written out along with the vtable so that :samp:`dynamic_cast`
  can determine the dynamic type of a class object at run time.  For all
  other types, we write out the :samp:`type_info` object when it is used: when
  applying :samp:`typeid` to an expression, throwing an object, or
  referring to a type in a catch clause or exception specification.

Template Instantiations
  Most everything in this section also applies to template instantiations,
  but there are other options as well.
  See :ref:`template-instantiation`.

When used with GNU ld version 2.8 or later on an ELF system such as
GNU/Linux or Solaris 2, or on Microsoft Windows, duplicate copies of
these constructs will be discarded at link time.  This is known as
COMDAT support.

On targets that don't support COMDAT, but do support weak symbols, GCC
uses them.  This way one copy overrides all the others, but
the unused copies still take up space in the executable.

For targets that do not support either COMDAT or weak symbols,
most entities with vague linkage are emitted as local symbols to
avoid duplicate definition errors from the linker.  This does not happen
for local statics in inlines, however, as having multiple copies
almost certainly breaks things.

See :ref:`c++-interface`, for
another way to control placement of these constructs.
