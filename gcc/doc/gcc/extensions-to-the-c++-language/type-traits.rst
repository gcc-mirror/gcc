..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _type-traits:

Type Traits
***********

The C++ front end implements syntactic extensions that allow
compile-time determination of
various characteristics of a type (or of a
pair of types).

``__has_nothrow_assign (type)``
  If ``type`` is ``const`` -qualified or is a reference type then
  the trait is ``false``.  Otherwise if ``__has_trivial_assign (type)``
  is ``true`` then the trait is ``true``, else if ``type`` is
  a cv-qualified class or union type with copy assignment operators that are
  known not to throw an exception then the trait is ``true``, else it is
  ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__has_nothrow_copy (type)``
  If ``__has_trivial_copy (type)`` is ``true`` then the trait is
  ``true``, else if ``type`` is a cv-qualified class or union type
  with copy constructors that are known not to throw an exception then
  the trait is ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__has_nothrow_constructor (type)``
  If ``__has_trivial_constructor (type)`` is ``true`` then the trait
  is ``true``, else if ``type`` is a cv class or union type (or array
  thereof) with a default constructor that is known not to throw an
  exception then the trait is ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__has_trivial_assign (type)``
  If ``type`` is ``const`` - qualified or is a reference type then
  the trait is ``false``.  Otherwise if ``__is_trivial (type)`` is
  ``true`` then the trait is ``true``, else if ``type`` is
  a cv-qualified class or union type with a trivial copy assignment
  ([class.copy]) then the trait is ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__has_trivial_copy (type)``
  If ``__is_trivial (type)`` is ``true`` or ``type`` is a reference
  type then the trait is ``true``, else if ``type`` is a cv class
  or union type with a trivial copy constructor ([class.copy]) then the trait
  is ``true``, else it is ``false``.  Requires: ``type`` shall be
  a complete type, (possibly cv-qualified) ``void``, or an array of unknown
  bound.

``__has_trivial_constructor (type)``
  If ``__is_trivial (type)`` is ``true`` then the trait is ``true``,
  else if ``type`` is a cv-qualified class or union type (or array thereof)
  with a trivial default constructor ([class.ctor]) then the trait is ``true``,
  else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__has_trivial_destructor (type)``
  If ``__is_trivial (type)`` is ``true`` or ``type`` is a reference type
  then the trait is ``true``, else if ``type`` is a cv class or union
  type (or array thereof) with a trivial destructor ([class.dtor]) then
  the trait is ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__has_virtual_destructor (type)``
  If ``type`` is a class type with a virtual destructor
  ([class.dtor]) then the trait is ``true``, else it is ``false``.
  Requires: If ``type`` is a non-union class type, it shall be a complete type.

``__is_abstract (type)``
  If ``type`` is an abstract class ([class.abstract]) then the trait
  is ``true``, else it is ``false``.
  Requires: If ``type`` is a non-union class type, it shall be a complete type.

``__is_aggregate (type)``
  If ``type`` is an aggregate type ([dcl.init.aggr]) the trait is
  ``true``, else it is ``false``.
  Requires: If ``type`` is a class type, it shall be a complete type.

``__is_base_of (base_type, derived_type)``
  If ``base_type`` is a base class of ``derived_type``
  ([class.derived]) then the trait is ``true``, otherwise it is ``false``.
  Top-level cv-qualifications of ``base_type`` and
  ``derived_type`` are ignored.  For the purposes of this trait, a
  class type is considered is own base.
  Requires: if ``__is_class (base_type)`` and ``__is_class (derived_type)``
  are ``true`` and ``base_type`` and ``derived_type`` are not the same
  type (disregarding cv-qualifiers), ``derived_type`` shall be a complete
  type.  A diagnostic is produced if this requirement is not met.

``__is_class (type)``
  If ``type`` is a cv-qualified class type, and not a union type
  ([basic.compound]) the trait is ``true``, else it is ``false``.

``__is_empty (type)``
  If ``__is_class (type)`` is ``false`` then the trait is ``false``.
  Otherwise ``type`` is considered empty if and only if: ``type``
  has no non-static data members, or all non-static data members, if
  any, are bit-fields of length 0, and ``type`` has no virtual
  members, and ``type`` has no virtual base classes, and ``type``
  has no base classes ``base_type`` for which
  ``__is_empty (base_type)`` is ``false``.
  Requires: If ``type`` is a non-union class type, it shall be a complete type.

``__is_enum (type)``
  If ``type`` is a cv enumeration type ([basic.compound]) the trait is
  ``true``, else it is ``false``.

``__is_final (type)``
  If ``type`` is a class or union type marked ``final``, then the trait
  is ``true``, else it is ``false``.
  Requires: If ``type`` is a class type, it shall be a complete type.

``__is_literal_type (type)``
  If ``type`` is a literal type ([basic.types]) the trait is
  ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__is_pod (type)``
  If ``type`` is a cv POD type ([basic.types]) then the trait is ``true``,
  else it is ``false``.
  Requires: ``type`` shall be a complete type, (possibly cv-qualified)
  ``void``, or an array of unknown bound.

``__is_polymorphic (type)``
  If ``type`` is a polymorphic class ([class.virtual]) then the trait
  is ``true``, else it is ``false``.
  Requires: If ``type`` is a non-union class type, it shall be a complete type.

``__is_standard_layout (type)``
  If ``type`` is a standard-layout type ([basic.types]) the trait is
  ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, an array of complete types,
  or (possibly cv-qualified) ``void``.

``__is_trivial (type)``
  If ``type`` is a trivial type ([basic.types]) the trait is
  ``true``, else it is ``false``.
  Requires: ``type`` shall be a complete type, an array of complete types,
  or (possibly cv-qualified) ``void``.

``__is_union (type)``
  If ``type`` is a cv union type ([basic.compound]) the trait is
  ``true``, else it is ``false``.

``__underlying_type (type)``
  The underlying type of ``type``.
  Requires: ``type`` shall be an enumeration type ([dcl.enum]).

``__integer_pack (length)``
  When used as the pattern of a pack expansion within a template
  definition, expands to a template argument pack containing integers
  from ``0`` to ``length-1``.  This is provided for efficient
  implementation of ``std::make_integer_sequence``.
