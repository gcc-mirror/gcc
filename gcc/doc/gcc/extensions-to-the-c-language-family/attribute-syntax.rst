..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: attribute syntax

.. _attribute-syntax:

Attribute Syntax
****************

This section describes the syntax with which ``__attribute__`` may be
used, and the constructs to which attribute specifiers bind, for the C
language.  Some details may vary for C++ and Objective-C.  Because of
infelicities in the grammar for attributes, some forms described here
may not be successfully parsed in all cases.

There are some problems with the semantics of attributes in C++.  For
example, there are no manglings for attributes, although they may affect
code generation, so problems may arise when attributed types are used in
conjunction with templates or overloading.  Similarly, ``typeid``
does not distinguish between types with different attributes.  Support
for attributes in C++ may be restricted in future to attributes on
declarations only, but not on nested declarators.

See :ref:`function-attributes`, for details of the semantics of attributes
applying to functions.  See :ref:`variable-attributes`, for details of the
semantics of attributes applying to variables.  See :ref:`type-attributes`,
for details of the semantics of attributes applying to structure, union
and enumerated types.
See :ref:`label-attributes`, for details of the semantics of attributes
applying to labels.
See :ref:`enumerator-attributes`, for details of the semantics of attributes
applying to enumerators.
See :ref:`statement-attributes`, for details of the semantics of attributes
applying to statements.

An :dfn:`attribute specifier` is of the form
``__attribute__ ((attribute-list))``.  An :dfn:`attribute list`
is a possibly empty comma-separated sequence of :dfn:`attributes`, where
each attribute is one of the following:

* Empty.  Empty attributes are ignored.

* An attribute name
  (which may be an identifier such as :fn-attr:`unused`, or a reserved
  word such as :fn-attr:`const`).

* An attribute name followed by a parenthesized list of
  parameters for the attribute.
  These parameters take one of the following forms:

  * An identifier.  For example, ``mode`` attributes use this form.

  * An identifier followed by a comma and a non-empty comma-separated list
    of expressions.  For example, ``format`` attributes use this form.

  * A possibly empty comma-separated list of expressions.  For example,
    ``format_arg`` attributes use this form with the list being a single
    integer constant expression, and ``alias`` attributes use this form
    with the list being a single string constant.

An :dfn:`attribute specifier list` is a sequence of one or more attribute
specifiers, not separated by any other tokens.

You may optionally specify attribute names with :samp:`__`
preceding and following the name.
This allows you to use them in header files without
being concerned about a possible macro of the same name.  For example,
you may use the attribute name ``__noreturn__`` instead of :fn-attr:`noreturn`.

Label Attributes
^^^^^^^^^^^^^^^^

In GNU C, an attribute specifier list may appear after the colon following a
label, other than a ``case`` or ``default`` label.  GNU C++ only permits
attributes on labels if the attribute specifier is immediately
followed by a semicolon (i.e., the label applies to an empty
statement).  If the semicolon is missing, C++ label attributes are
ambiguous, as it is permissible for a declaration, which could begin
with an attribute list, to be labelled in C++.  Declarations cannot be
labelled in C90 or C99, so the ambiguity does not arise there.

Enumerator Attributes
^^^^^^^^^^^^^^^^^^^^^

In GNU C, an attribute specifier list may appear as part of an enumerator.
The attribute goes after the enumeration constant, before ``=``, if
present.  The optional attribute in the enumerator appertains to the
enumeration constant.  It is not possible to place the attribute after
the constant expression, if present.

Statement Attributes
^^^^^^^^^^^^^^^^^^^^

In GNU C, an attribute specifier list may appear as part of a null
statement.  The attribute goes before the semicolon.

Type Attributes
^^^^^^^^^^^^^^^

An attribute specifier list may appear as part of a ``struct``,
``union`` or ``enum`` specifier.  It may go either immediately
after the ``struct``, ``union`` or ``enum`` keyword, or after
the closing brace.  The former syntax is preferred.
Where attribute specifiers follow the closing brace, they are considered
to relate to the structure, union or enumerated type defined, not to any
enclosing declaration the type specifier appears in, and the type
defined is not complete until after the attribute specifiers.

.. Otherwise, there would be the following problems: a shift/reduce

.. conflict between attributes binding the struct/union/enum and

.. binding to the list of specifiers/qualifiers; and "aligned"

.. attributes could use sizeof for the structure, but the size could be

.. changed later by "packed" attributes.

All other attributes
^^^^^^^^^^^^^^^^^^^^

Otherwise, an attribute specifier appears as part of a declaration,
counting declarations of unnamed parameters and type names, and relates
to that declaration (which may be nested in another declaration, for
example in the case of a parameter declaration), or to a particular declarator
within a declaration.  Where an
attribute specifier is applied to a parameter declared as a function or
an array, it should apply to the function or array rather than the
pointer to which the parameter is implicitly converted, but this is not
yet correctly implemented.

Any list of specifiers and qualifiers at the start of a declaration may
contain attribute specifiers, whether or not such a list may in that
context contain storage class specifiers.  (Some attributes, however,
are essentially in the nature of storage class specifiers, and only make
sense where storage class specifiers may be used; for example,
``section``.)  There is one necessary limitation to this syntax: the
first old-style parameter declaration in a function definition cannot
begin with an attribute specifier, because such an attribute applies to
the function instead by syntax described below (which, however, is not
yet implemented in this case).  In some other cases, attribute
specifiers are permitted by this grammar but not yet supported by the
compiler.  All attribute specifiers in this place relate to the
declaration as a whole.  In the obsolescent usage where a type of
``int`` is implied by the absence of type specifiers, such a list of
specifiers and qualifiers may be an attribute specifier list with no
other specifiers or qualifiers.

At present, the first parameter in a function prototype must have some
type specifier that is not an attribute specifier; this resolves an
ambiguity in the interpretation of ``void f(int
(__attribute__((foo)) x))``, but is subject to change.  At present, if
the parentheses of a function declarator contain only attributes then
those attributes are ignored, rather than yielding an error or warning
or implying a single parameter of type int, but this is subject to
change.

An attribute specifier list may appear immediately before a declarator
(other than the first) in a comma-separated list of declarators in a
declaration of more than one identifier using a single list of
specifiers and qualifiers.  Such attribute specifiers apply
only to the identifier before whose declarator they appear.  For
example, in

.. code-block:: c++

  __attribute__((noreturn)) void d0 (void),
      __attribute__((format(printf, 1, 2))) d1 (const char *, ...),
       d2 (void);

the :fn-attr:`noreturn` attribute applies to all the functions
declared; the ``format`` attribute only applies to ``d1``.

An attribute specifier list may appear immediately before the comma,
``=`` or semicolon terminating the declaration of an identifier other
than a function definition.  Such attribute specifiers apply
to the declared object or function.  Where an
assembler name for an object or function is specified (see :ref:`asm-labels`), the attribute must follow the ``asm``
specification.

An attribute specifier list may, in future, be permitted to appear after
the declarator in a function definition (before any old-style parameter
declarations or the function body).

Attribute specifiers may be mixed with type qualifiers appearing inside
the ``[]`` of a parameter array declarator, in the C99 construct by
which such qualifiers are applied to the pointer to which the array is
implicitly converted.  Such attribute specifiers apply to the pointer,
not to the array, but at present this is not implemented and they are
ignored.

An attribute specifier list may appear at the start of a nested
declarator.  At present, there are some limitations in this usage: the
attributes correctly apply to the declarator, but for most individual
attributes the semantics this implies are not implemented.
When attribute specifiers follow the ``*`` of a pointer
declarator, they may be mixed with any type qualifiers present.
The following describes the formal semantics of this syntax.  It makes the
most sense if you are familiar with the formal specification of
declarators in the ISO C standard.

Consider (as in C99 subclause 6.7.5 paragraph 4) a declaration ``T
D1``, where ``T`` contains declaration specifiers that specify a type
:samp:`{Type}` (such as ``int``) and ``D1`` is a declarator that
contains an identifier :samp:`{ident}`.  The type specified for :samp:`{ident}`
for derived declarators whose type does not include an attribute
specifier is as in the ISO C standard.

If ``D1`` has the form ``( attribute-specifier-list D )``,
and the declaration ``T D`` specifies the type
' :samp:`{derived-declarator-type-list}` :samp:`{Type}` ' for :samp:`{ident}`, then
``T D1`` specifies the type ' :samp:`{derived-declarator-type-list}`
:samp:`{attribute-specifier-list}` :samp:`{Type}` ' for :samp:`{ident}`.

If ``D1`` has the form ``*
type-qualifier-and-attribute-specifier-list D``, and the
declaration ``T D`` specifies the type
' :samp:`{derived-declarator-type-list}` :samp:`{Type}` ' for :samp:`{ident}`, then
``T D1`` specifies the type ' :samp:`{derived-declarator-type-list}`
:samp:`{type-qualifier-and-attribute-specifier-list}` pointer to :samp:`{Type}` ' for
:samp:`{ident}`.

For example,

.. code-block:: c++

  void (__attribute__((noreturn)) ****f) (void);

specifies the type 'pointer to pointer to pointer to pointer to
non-returning function returning ``void`` '.  As another example,

.. code-block:: c++

  char *__attribute__((aligned(8))) *f;

specifies the type 'pointer to 8-byte-aligned pointer to ``char`` '.
Note again that this does not work with most attributes; for example,
the usage of :samp:`aligned` and :samp:`noreturn` attributes given above
is not yet supported.

For compatibility with existing code written for compiler versions that
did not implement attributes on nested declarators, some laxity is
allowed in the placing of attributes.  If an attribute that only applies
to types is applied to a declaration, it is treated as applying to
the type of that declaration.  If an attribute that only applies to
declarations is applied to the type of a declaration, it is treated
as applying to that declaration; and, for compatibility with code
placing the attributes immediately before the identifier declared, such
an attribute applied to a function return type is treated as
applying to the function type, and such an attribute applied to an array
element type is treated as applying to the array type.  If an
attribute that only applies to function types is applied to a
pointer-to-function type, it is treated as applying to the pointer
target type; if such an attribute is applied to a function return type
that is not a pointer-to-function type, it is treated as applying
to the function type.
