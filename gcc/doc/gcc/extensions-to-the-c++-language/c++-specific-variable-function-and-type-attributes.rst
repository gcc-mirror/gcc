..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c++-attributes:

C++-Specific Variable, Function, and Type Attributes
****************************************************

Some attributes only make sense for C++ programs.

.. index:: abi_tag function attribute, abi_tag variable attribute, abi_tag type attribute

.. gcc-attr:: abi_tag ("tag", ...)

  The ``abi_tag`` attribute can be applied to a function, variable, or class
  declaration.  It modifies the mangled name of the entity to
  incorporate the tag name, in order to distinguish the function or
  class from an earlier version with a different ABI; perhaps the class
  has changed size, or the function has a different return type that is
  not encoded in the mangled name.

  The attribute can also be applied to an inline namespace, but does not
  affect the mangled name of the namespace; in this case it is only used
  for :option:`-Wabi-tag` warnings and automatic tagging of functions and
  variables.  Tagging inline namespaces is generally preferable to
  tagging individual declarations, but the latter is sometimes
  necessary, such as when only certain members of a class need to be
  tagged.

  The argument can be a list of strings of arbitrary length.  The
  strings are sorted on output, so the order of the list is
  unimportant.

  A redeclaration of an entity must not add new ABI tags,
  since doing so would change the mangled name.

  The ABI tags apply to a name, so all instantiations and
  specializations of a template have the same tags.  The attribute will
  be ignored if applied to an explicit specialization or instantiation.

  The :option:`-Wabi-tag` flag enables a warning about a class which does
  not have all the ABI tags used by its subobjects and virtual functions; for users with code
  that needs to coexist with an earlier ABI, using this option can help
  to find all affected types that need to be tagged.

  When a type involving an ABI tag is used as the type of a variable or
  return type of a function where that tag is not already present in the
  signature of the function, the tag is automatically applied to the
  variable or function.  :option:`-Wabi-tag` also warns about this
  situation; this warning can be avoided by explicitly tagging the
  variable or function or moving it into a tagged inline namespace.

.. index:: init_priority variable attribute

.. gcc-attr:: init_priority (priority)

  In Standard C++, objects defined at namespace scope are guaranteed to be
  initialized in an order in strict accordance with that of their definitions
  *in a given translation unit*.  No guarantee is made for initializations
  across translation units.  However, GNU C++ allows users to control the
  order of initialization of objects defined at namespace scope with the
  ``init_priority`` attribute by specifying a relative :samp:`{priority}`,
  a constant integral expression currently bounded between 101 and 65535
  inclusive.  Lower numbers indicate a higher priority.

  In the following example, ``A`` would normally be created before
  ``B``, but the ``init_priority`` attribute reverses that order:

  .. code-block:: c++

    Some_Class  A  __attribute__ ((init_priority (2000)));
    Some_Class  B  __attribute__ ((init_priority (543)));

  Note that the particular values of :samp:`{priority}` do not matter; only their
  relative ordering.

.. index:: warn_unused type attribute

.. gcc-attr:: warn_unused

  For C++ types with non-trivial constructors and/or destructors it is
  impossible for the compiler to determine whether a variable of this
  type is truly unused if it is not referenced. This type attribute
  informs the compiler that variables of this type should be warned
  about if they appear to be unused, just like variables of fundamental
  types.

  This attribute is appropriate for types which just represent a value,
  such as ``std::string`` ; it is not appropriate for types which
  control a resource, such as ``std::lock_guard``.

  This attribute is also accepted in C, but it is unnecessary because C
  does not have constructors or destructors.
