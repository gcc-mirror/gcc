..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: type, pointer, reference, fundamental type, array

.. _types:

Types
*****

.. index:: VOID_TYPE, INTEGER_TYPE, TYPE_MIN_VALUE, TYPE_MAX_VALUE, REAL_TYPE, FIXED_POINT_TYPE, COMPLEX_TYPE, ENUMERAL_TYPE, BOOLEAN_TYPE, POINTER_TYPE, REFERENCE_TYPE, FUNCTION_TYPE, METHOD_TYPE, ARRAY_TYPE, RECORD_TYPE, UNION_TYPE, OPAQUE_TYPE, UNKNOWN_TYPE, OFFSET_TYPE, TYPE_UNQUALIFIED, TYPE_QUAL_CONST, TYPE_QUAL_VOLATILE, TYPE_QUAL_RESTRICT, TYPE_MAIN_VARIANT, qualified type, TYPE_SIZE, TYPE_ALIGN, TYPE_PRECISION, TYPE_ARG_TYPES, TYPE_METHOD_BASETYPE, TYPE_OFFSET_BASETYPE, TREE_TYPE, TYPE_CONTEXT, TYPE_NAME, TYPENAME_TYPE_FULLNAME, TYPE_FIELDS, TYPE_CANONICAL, TYPE_STRUCTURAL_EQUALITY_P, SET_TYPE_STRUCTURAL_EQUALITY

All types have corresponding tree nodes.  However, you should not assume
that there is exactly one tree node corresponding to each type.  There
are often multiple nodes corresponding to the same type.

For the most part, different kinds of types have different tree codes.
(For example, pointer types use a ``POINTER_TYPE`` code while arrays
use an ``ARRAY_TYPE`` code.)  However, pointers to member functions
use the ``RECORD_TYPE`` code.  Therefore, when writing a
``switch`` statement that depends on the code associated with a
particular type, you should take care to handle pointers to member
functions under the ``RECORD_TYPE`` case label.

The following functions and macros deal with cv-qualification of types:

.. envvar:: TYPE_MAIN_VARIANT

  This macro returns the unqualified version of a type.  It may be applied
  to an unqualified type, but it is not always the identity function in
  that case.

A few other macros and functions are usable with all types:

.. envvar:: TYPE_SIZE

  The number of bits required to represent the type, represented as an
  ``INTEGER_CST``.  For an incomplete type, ``TYPE_SIZE`` will be
  ``NULL_TREE``.

.. envvar:: TYPE_ALIGN

  The alignment of the type, in bits, represented as an ``int``.

.. envvar:: TYPE_NAME

  This macro returns a declaration (in the form of a ``TYPE_DECL``) for
  the type.  (Note this macro does *not* return an
  ``IDENTIFIER_NODE``, as you might expect, given its name!)  You can
  look at the ``DECL_NAME`` of the ``TYPE_DECL`` to obtain the
  actual name of the type.  The ``TYPE_NAME`` will be ``NULL_TREE``
  for a type that is not a built-in type, the result of a typedef, or a
  named class type.

.. envvar:: TYPE_CANONICAL

  This macro returns the 'canonical' type for the given type
  node. Canonical types are used to improve performance in the C++ and
  Objective-C++ front ends by allowing efficient comparison between two
  type nodes in ``same_type_p`` : if the ``TYPE_CANONICAL`` values
  of the types are equal, the types are equivalent; otherwise, the types
  are not equivalent. The notion of equivalence for canonical types is
  the same as the notion of type equivalence in the language itself. For
  instance,

  When ``TYPE_CANONICAL`` is ``NULL_TREE``, there is no canonical
  type for the given type node. In this case, comparison between this
  type and any other type requires the compiler to perform a deep,
  'structural' comparison to see if the two type nodes have the same
  form and properties.

  The canonical type for a node is always the most fundamental type in
  the equivalence class of types. For instance, ``int`` is its own
  canonical type. A typedef ``I`` of ``int`` will have ``int``
  as its canonical type. Similarly, ``I*``and a typedef ``IP``(defined to ``I*``) will has ``int*`` as their canonical
  type. When building a new type node, be sure to set
  ``TYPE_CANONICAL`` to the appropriate canonical type. If the new
  type is a compound type (built from other types), and any of those
  other types require structural equality, use
  ``SET_TYPE_STRUCTURAL_EQUALITY`` to ensure that the new type also
  requires structural equality. Finally, if for some reason you cannot
  guarantee that ``TYPE_CANONICAL`` will point to the canonical type,
  use ``SET_TYPE_STRUCTURAL_EQUALITY`` to make sure that the new
  type--and any type constructed based on it--requires structural
  equality. If you suspect that the canonical type system is
  miscomparing types, pass :option:`--param` :gcc-param:`verify-canonical-types`:samp:`=1` to
  the compiler or configure with ``--enable-checking`` to force the
  compiler to verify its canonical-type comparisons against the
  structural comparisons; the compiler will then print any warnings if
  the canonical types miscompare.

.. envvar:: TYPE_STRUCTURAL_EQUALITY_P

  This predicate holds when the node requires structural equality
  checks, e.g., when ``TYPE_CANONICAL`` is ``NULL_TREE``.

.. envvar:: SET_TYPE_STRUCTURAL_EQUALITY

  This macro states that the type node it is given requires structural
  equality checks, e.g., it sets ``TYPE_CANONICAL`` to
  ``NULL_TREE``.

``same_type_p``
  This predicate takes two types as input, and holds if they are the same
  type.  For example, if one type is a ``typedef`` for the other, or
  both are ``typedef`` s for the same type.  This predicate also holds if
  the two trees given as input are simply copies of one another; i.e.,
  there is no difference between them at the source level, but, for
  whatever reason, a duplicate has been made in the representation.  You
  should never use ``==`` (pointer equality) to compare types; always
  use ``same_type_p`` instead.

  Detailed below are the various kinds of types, and the macros that can
  be used to access them.  Although other kinds of types are used
  elsewhere in G++, the types described here are the only ones that you
  will encounter while examining the intermediate representation.

.. envvar:: VOID_TYPE

  Used to represent the ``void`` type.

.. envvar:: INTEGER_TYPE

  Used to represent the various integral types, including ``char``,
  ``short``, ``int``, ``long``, and ``long long``.  This code
  is not used for enumeration types, nor for the ``bool`` type.
  The ``TYPE_PRECISION`` is the number of bits used in
  the representation, represented as an ``unsigned int``.  (Note that
  in the general case this is not the same value as ``TYPE_SIZE`` ;
  suppose that there were a 24-bit integer type, but that alignment
  requirements for the ABI required 32-bit alignment.  Then,
  ``TYPE_SIZE`` would be an ``INTEGER_CST`` for 32, while
  ``TYPE_PRECISION`` would be 24.)  The integer type is unsigned if
  ``TYPE_UNSIGNED`` holds; otherwise, it is signed.

  The ``TYPE_MIN_VALUE`` is an ``INTEGER_CST`` for the smallest
  integer that may be represented by this type.  Similarly, the
  ``TYPE_MAX_VALUE`` is an ``INTEGER_CST`` for the largest integer
  that may be represented by this type.

.. envvar:: REAL_TYPE

  Used to represent the ``float``, ``double``, and ``long
  double`` types.  The number of bits in the floating-point representation
  is given by ``TYPE_PRECISION``, as in the ``INTEGER_TYPE`` case.

.. envvar:: FIXED_POINT_TYPE

  Used to represent the ``short _Fract``, ``_Fract``, ``long
  _Fract``, ``long long _Fract``, ``short _Accum``, ``_Accum``,
  ``long _Accum``, and ``long long _Accum`` types.  The number of bits
  in the fixed-point representation is given by ``TYPE_PRECISION``,
  as in the ``INTEGER_TYPE`` case.  There may be padding bits, fractional
  bits and integral bits.  The number of fractional bits is given by
  ``TYPE_FBIT``, and the number of integral bits is given by ``TYPE_IBIT``.
  The fixed-point type is unsigned if ``TYPE_UNSIGNED`` holds; otherwise,
  it is signed.
  The fixed-point type is saturating if ``TYPE_SATURATING`` holds; otherwise,
  it is not saturating.

.. envvar:: COMPLEX_TYPE

  Used to represent GCC built-in ``__complex__`` data types.  The
  ``TREE_TYPE`` is the type of the real and imaginary parts.

.. envvar:: ENUMERAL_TYPE

  Used to represent an enumeration type.  The ``TYPE_PRECISION`` gives
  (as an ``int``), the number of bits used to represent the type.  If
  there are no negative enumeration constants, ``TYPE_UNSIGNED`` will
  hold.  The minimum and maximum enumeration constants may be obtained
  with ``TYPE_MIN_VALUE`` and ``TYPE_MAX_VALUE``, respectively; each
  of these macros returns an ``INTEGER_CST``.

  The actual enumeration constants themselves may be obtained by looking
  at the ``TYPE_VALUES``.  This macro will return a ``TREE_LIST``,
  containing the constants.  The ``TREE_PURPOSE`` of each node will be
  an ``IDENTIFIER_NODE`` giving the name of the constant; the
  ``TREE_VALUE`` will be an ``INTEGER_CST`` giving the value
  assigned to that constant.  These constants will appear in the order in
  which they were declared.  The ``TREE_TYPE`` of each of these
  constants will be the type of enumeration type itself.

.. envvar:: OPAQUE_TYPE

  Used for things that have a ``MODE_OPAQUE`` mode class in the
  backend. Opaque types have a size and precision, and can be held in
  memory or registers. They are used when we do not want the compiler to
  make assumptions about the availability of other operations as would
  happen with integer types.

.. envvar:: BOOLEAN_TYPE

  Used to represent the ``bool`` type.

.. envvar:: POINTER_TYPE

  Used to represent pointer types, and pointer to data member types.  The
  ``TREE_TYPE`` gives the type to which this type points.

.. envvar:: REFERENCE_TYPE

  Used to represent reference types.  The ``TREE_TYPE`` gives the type
  to which this type refers.

.. envvar:: FUNCTION_TYPE

  Used to represent the type of non-member functions and of static member
  functions.  The ``TREE_TYPE`` gives the return type of the function.
  The ``TYPE_ARG_TYPES`` are a ``TREE_LIST`` of the argument types.
  The ``TREE_VALUE`` of each node in this list is the type of the
  corresponding argument; the ``TREE_PURPOSE`` is an expression for the
  default argument value, if any.  If the last node in the list is
  ``void_list_node`` (a ``TREE_LIST`` node whose ``TREE_VALUE``
  is the ``void_type_node``), then functions of this type do not take
  variable arguments.  Otherwise, they do take a variable number of
  arguments.

  Note that in C (but not in C++) a function declared like ``void f()``
  is an unprototyped function taking a variable number of arguments; the
  ``TYPE_ARG_TYPES`` of such a function will be ``NULL``.

.. envvar:: METHOD_TYPE

  Used to represent the type of a non-static member function.  Like a
  ``FUNCTION_TYPE``, the return type is given by the ``TREE_TYPE``.
  The type of ``*this``, i.e., the class of which functions of this
  type are a member, is given by the ``TYPE_METHOD_BASETYPE``.  The
  ``TYPE_ARG_TYPES`` is the parameter list, as for a
  ``FUNCTION_TYPE``, and includes the ``this`` argument.

.. envvar:: ARRAY_TYPE

  Used to represent array types.  The ``TREE_TYPE`` gives the type of
  the elements in the array.  If the array-bound is present in the type,
  the ``TYPE_DOMAIN`` is an ``INTEGER_TYPE`` whose
  ``TYPE_MIN_VALUE`` and ``TYPE_MAX_VALUE`` will be the lower and
  upper bounds of the array, respectively.  The ``TYPE_MIN_VALUE`` will
  always be an ``INTEGER_CST`` for zero, while the
  ``TYPE_MAX_VALUE`` will be one less than the number of elements in
  the array, i.e., the highest value which may be used to index an element
  in the array.

.. envvar:: RECORD_TYPE

  Used to represent ``struct`` and ``class`` types, as well as
  pointers to member functions and similar constructs in other languages.
  ``TYPE_FIELDS`` contains the items contained in this type, each of
  which can be a ``FIELD_DECL``, ``VAR_DECL``, ``CONST_DECL``, or
  ``TYPE_DECL``.  You may not make any assumptions about the ordering
  of the fields in the type or whether one or more of them overlap.

.. envvar:: UNION_TYPE

  Used to represent ``union`` types.  Similar to ``RECORD_TYPE``
  except that all ``FIELD_DECL`` nodes in ``TYPE_FIELD`` start at
  bit position zero.

.. envvar:: QUAL_UNION_TYPE

  Used to represent part of a variant record in Ada.  Similar to
  ``UNION_TYPE`` except that each ``FIELD_DECL`` has a
  ``DECL_QUALIFIER`` field, which contains a boolean expression that
  indicates whether the field is present in the object.  The type will only
  have one field, so each field's ``DECL_QUALIFIER`` is only evaluated
  if none of the expressions in the previous fields in ``TYPE_FIELDS``
  are nonzero.  Normally these expressions will reference a field in the
  outer object using a ``PLACEHOLDER_EXPR``.

.. envvar:: LANG_TYPE

  This node is used to represent a language-specific type.  The front
  end must handle it.

.. envvar:: OFFSET_TYPE

  This node is used to represent a pointer-to-data member.  For a data
  member ``X::m`` the ``TYPE_OFFSET_BASETYPE`` is ``X`` and the
  ``TREE_TYPE`` is the type of ``m``.

There are variables whose values represent some of the basic types.
These include:

``void_type_node``
  A node for ``void``.

``integer_type_node``
  A node for ``int``.

``unsigned_type_node.``
  A node for ``unsigned int``.

``char_type_node.``
  A node for ``char``.

  It may sometimes be useful to compare one of these variables with a type
  in hand, using ``same_type_p``.
