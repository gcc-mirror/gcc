..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _c-and-c++-trees:

C and C++ Trees
***************

This section documents the internal representation used by GCC to
represent C and C++ source programs.  When presented with a C or C++
source program, GCC parses the program, performs semantic analysis
(including the generation of error messages), and then produces the
internal representation described here.  This representation contains a
complete representation for the entire translation unit provided as
input to the front end.  This representation is then typically processed
by a code-generator in order to produce machine code, but could also be
used in the creation of source browsers, intelligent editors, automatic
documentation generators, interpreters, and any other programs needing
the ability to process C or C++ code.

This section explains the internal representation.  In particular, it
documents the internal representation for C and C++ source
constructs, and the macros, functions, and variables that can be used to
access these constructs.  The C++ representation is largely a superset
of the representation used in the C front end.  There is only one
construct used in C that does not appear in the C++ front end and that
is the GNU 'nested function' extension.  Many of the macros documented
here do not apply in C because the corresponding language constructs do
not appear in C.

The C and C++ front ends generate a mix of GENERIC trees and ones
specific to C and C++.  These language-specific trees are higher-level
constructs than the ones in GENERIC to make the parser's job easier.
This section describes those trees that aren't part of GENERIC as well
as aspects of GENERIC trees that are treated in a language-specific
manner.

If you are developing a 'back end', be it is a code-generator or some
other tool, that uses this representation, you may occasionally find
that you need to ask questions not easily answered by the functions and
macros available here.  If that situation occurs, it is quite likely
that GCC already supports the functionality you desire, but that the
interface is simply not documented here.  In that case, you should ask
the GCC maintainers (via mail to gcc@gcc.gnu.org) about
documenting the functionality you require.  Similarly, if you find
yourself writing functions that do not deal directly with your back end,
but instead might be useful to other people using the GCC front end, you
should submit your patches for inclusion in GCC.

.. toctree::
  :maxdepth: 2


.. _types-for-c++:

Types for C++
^^^^^^^^^^^^^

.. index:: UNKNOWN_TYPE, TYPENAME_TYPE, TYPEOF_TYPE, cp_type_quals, TYPE_UNQUALIFIED, TYPE_QUAL_CONST, TYPE_QUAL_VOLATILE, TYPE_QUAL_RESTRICT, TYPE_MAIN_VARIANT, qualified type, TYPE_SIZE, TYPE_ALIGN, TYPE_PRECISION, TYPE_ARG_TYPES, TYPE_METHOD_BASETYPE, TYPE_PTRDATAMEM_P, TYPE_OFFSET_BASETYPE, TREE_TYPE, TYPE_CONTEXT, TYPE_NAME, TYPENAME_TYPE_FULLNAME, TYPE_FIELDS, TYPE_PTROBV_P

In C++, an array type is not qualified; rather the type of the array
elements is qualified.  This situation is reflected in the intermediate
representation.  The macros described here will always examine the
qualification of the underlying element type when applied to an array
type.  (If the element type is itself an array, then the recursion
continues until a non-array type is found, and the qualification of this
type is examined.)  So, for example, ``CP_TYPE_CONST_P`` will hold of
the type ``const int ()[7]``, denoting an array of seven ``int`` s.

The following functions and macros deal with cv-qualification of types:

``cp_type_quals``
  This function returns the set of type qualifiers applied to this type.
  This value is ``TYPE_UNQUALIFIED`` if no qualifiers have been
  applied.  The ``TYPE_QUAL_CONST`` bit is set if the type is
  ``const`` -qualified.  The ``TYPE_QUAL_VOLATILE`` bit is set if the
  type is ``volatile`` -qualified.  The ``TYPE_QUAL_RESTRICT`` bit is
  set if the type is ``restrict`` -qualified.

.. envvar:: CP_TYPE_CONST_P

  This macro holds if the type is ``const`` -qualified.

.. envvar:: CP_TYPE_VOLATILE_P

  This macro holds if the type is ``volatile`` -qualified.

.. envvar:: CP_TYPE_RESTRICT_P

  This macro holds if the type is ``restrict`` -qualified.

.. envvar:: CP_TYPE_CONST_NON_VOLATILE_P

  This predicate holds for a type that is ``const`` -qualified, but
  *not* ``volatile`` -qualified; other cv-qualifiers are ignored as
  well: only the ``const`` -ness is tested.

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

.. envvar:: CP_INTEGRAL_TYPE

  This predicate holds if the type is an integral type.  Notice that in
  C++, enumerations are *not* integral types.

.. envvar:: ARITHMETIC_TYPE_P

  This predicate holds if the type is an integral type (in the C++ sense)
  or a floating point type.

.. envvar:: CLASS_TYPE_P

  This predicate holds for a class-type.

.. envvar:: TYPE_BUILT_IN

  This predicate holds for a built-in type.

.. envvar:: TYPE_PTRDATAMEM_P

  This predicate holds if the type is a pointer to data member.

.. envvar:: TYPE_PTR_P

  This predicate holds if the type is a pointer type, and the pointee is
  not a data member.

.. envvar:: TYPE_PTRFN_P

  This predicate holds for a pointer to function type.

.. envvar:: TYPE_PTROB_P

  This predicate holds for a pointer to object type.  Note however that it
  does not hold for the generic pointer to object type ``void *``.  You
  may use ``TYPE_PTROBV_P`` to test for a pointer to object type as
  well as ``void *``.

The table below describes types specific to C and C++ as well as
language-dependent info about GENERIC types.

.. envvar:: POINTER_TYPE

  Used to represent pointer types, and pointer to data member types.  If
  ``TREE_TYPE``
  is a pointer to data member type, then ``TYPE_PTRDATAMEM_P`` will hold.
  For a pointer to data member type of the form :samp:`T X::*`,
  ``TYPE_PTRMEM_CLASS_TYPE`` will be the type ``X``, while
  ``TYPE_PTRMEM_POINTED_TO_TYPE`` will be the type ``T``.

.. envvar:: RECORD_TYPE

  Used to represent ``struct`` and ``class`` types in C and C++.  If
  ``TYPE_PTRMEMFUNC_P`` holds, then this type is a pointer-to-member
  type.  In that case, the ``TYPE_PTRMEMFUNC_FN_TYPE`` is a
  ``POINTER_TYPE`` pointing to a ``METHOD_TYPE``.  The
  ``METHOD_TYPE`` is the type of a function pointed to by the
  pointer-to-member function.  If ``TYPE_PTRMEMFUNC_P`` does not hold,
  this type is a class type.  For more information, see :ref:`classes`.

.. envvar:: UNKNOWN_TYPE

  This node is used to represent a type the knowledge of which is
  insufficient for a sound processing.

.. envvar:: TYPENAME_TYPE

  Used to represent a construct of the form ``typename T::A``.  The
  ``TYPE_CONTEXT`` is ``T`` ; the ``TYPE_NAME`` is an
  ``IDENTIFIER_NODE`` for ``A``.  If the type is specified via a
  template-id, then ``TYPENAME_TYPE_FULLNAME`` yields a
  ``TEMPLATE_ID_EXPR``.  The ``TREE_TYPE`` is non- ``NULL`` if the
  node is implicitly generated in support for the implicit typename
  extension; in which case the ``TREE_TYPE`` is a type node for the
  base-class.

.. envvar:: TYPEOF_TYPE

  Used to represent the ``__typeof__`` extension.  The
  ``TYPE_FIELDS`` is the expression the type of which is being
  represented.

.. -
   Namespaces
   -

.. index:: namespace, scope

.. _namespaces:

Namespaces
^^^^^^^^^^

.. index:: NAMESPACE_DECL

The root of the entire intermediate representation is the variable
``global_namespace``.  This is the namespace specified with ``::``
in C++ source code.  All other namespaces, types, variables, functions,
and so forth can be found starting with this namespace.

However, except for the fact that it is distinguished as the root of the
representation, the global namespace is no different from any other
namespace.  Thus, in what follows, we describe namespaces generally,
rather than the global namespace in particular.

A namespace is represented by a ``NAMESPACE_DECL`` node.

The following macros and functions can be used on a ``NAMESPACE_DECL`` :

.. envvar:: DECL_NAME

  This macro is used to obtain the ``IDENTIFIER_NODE`` corresponding to
  the unqualified name of the name of the namespace (see :ref:`identifiers`).
  The name of the global namespace is :samp:`::`, even though in C++ the
  global namespace is unnamed.  However, you should use comparison with
  ``global_namespace``, rather than ``DECL_NAME`` to determine
  whether or not a namespace is the global one.  An unnamed namespace
  will have a ``DECL_NAME`` equal to ``anonymous_namespace_name``.
  Within a single translation unit, all unnamed namespaces will have the
  same name.

.. envvar:: DECL_CONTEXT

  This macro returns the enclosing namespace.  The ``DECL_CONTEXT`` for
  the ``global_namespace`` is ``NULL_TREE``.

.. envvar:: DECL_NAMESPACE_ALIAS

  If this declaration is for a namespace alias, then
  ``DECL_NAMESPACE_ALIAS`` is the namespace for which this one is an
  alias.

  Do not attempt to use ``cp_namespace_decls`` for a namespace which is
  an alias.  Instead, follow ``DECL_NAMESPACE_ALIAS`` links until you
  reach an ordinary, non-alias, namespace, and call
  ``cp_namespace_decls`` there.

.. envvar:: DECL_NAMESPACE_STD_P

  This predicate holds if the namespace is the special ``::std``
  namespace.

``cp_namespace_decls``
  This function will return the declarations contained in the namespace,
  including types, overloaded functions, other namespaces, and so forth.
  If there are no declarations, this function will return
  ``NULL_TREE``.  The declarations are connected through their
  ``TREE_CHAIN`` fields.

  Although most entries on this list will be declarations,
  ``TREE_LIST`` nodes may also appear.  In this case, the
  ``TREE_VALUE`` will be an ``OVERLOAD``.  The value of the
  ``TREE_PURPOSE`` is unspecified; back ends should ignore this value.
  As with the other kinds of declarations returned by
  ``cp_namespace_decls``, the ``TREE_CHAIN`` will point to the next
  declaration in this list.

  For more information on the kinds of declarations that can occur on this
  list, See :ref:`declarations`.  Some declarations will not appear on this
  list.  In particular, no ``FIELD_DECL``, ``LABEL_DECL``, or
  ``PARM_DECL`` nodes will appear here.

  This function cannot be used with namespaces that have
  ``DECL_NAMESPACE_ALIAS`` set.

.. -
   Classes
   -

.. index:: class, scope

.. _classes:

Classes
^^^^^^^

.. index:: RECORD_TYPE, UNION_TYPE, CLASSTYPE_DECLARED_CLASS, TYPE_BINFO, BINFO_TYPE, TYPE_FIELDS, TYPE_VFIELD

Besides namespaces, the other high-level scoping construct in C++ is the
class.  (Throughout this manual the term :dfn:`class` is used to mean the
types referred to in the ANSI/ISO C++ Standard as classes; these include
types defined with the ``class``, ``struct``, and ``union``
keywords.)

A class type is represented by either a ``RECORD_TYPE`` or a
``UNION_TYPE``.  A class declared with the ``union`` tag is
represented by a ``UNION_TYPE``, while classes declared with either
the ``struct`` or the ``class`` tag are represented by
``RECORD_TYPE`` s.  You can use the ``CLASSTYPE_DECLARED_CLASS``
macro to discern whether or not a particular type is a ``class`` as
opposed to a ``struct``.  This macro will be true only for classes
declared with the ``class`` tag.

Almost all members are available on the ``TYPE_FIELDS``
list.  Given one member, the next can be found by following the
``TREE_CHAIN``.  You should not depend in any way on the order in
which fields appear on this list.  All nodes on this list will be
:samp:`DECL` nodes.  A ``FIELD_DECL`` is used to represent a non-static
data member, a ``VAR_DECL`` is used to represent a static data
member, and a ``TYPE_DECL`` is used to represent a type.  Note that
the ``CONST_DECL`` for an enumeration constant will appear on this
list, if the enumeration type was declared in the class.  (Of course,
the ``TYPE_DECL`` for the enumeration type will appear here as well.)
There are no entries for base classes on this list.  In particular,
there is no ``FIELD_DECL`` for the 'base-class portion' of an
object.  If a function member is overloaded, each of the overloaded
functions appears; no ``OVERLOAD`` nodes appear on the ``TYPE_FIELDS``
list.  Implicitly declared functions (including default constructors,
copy constructors, assignment operators, and destructors) will appear on
this list as well.

The ``TYPE_VFIELD`` is a compiler-generated field used to point to
virtual function tables.  It may or may not appear on the
``TYPE_FIELDS`` list.  However, back ends should handle the
``TYPE_VFIELD`` just like all the entries on the ``TYPE_FIELDS``
list.

Every class has an associated :dfn:`binfo`, which can be obtained with
``TYPE_BINFO``.  Binfos are used to represent base-classes.  The
binfo given by ``TYPE_BINFO`` is the degenerate case, whereby every
class is considered to be its own base-class.  The base binfos for a
particular binfo are held in a vector, whose length is obtained with
``BINFO_N_BASE_BINFOS``.  The base binfos themselves are obtained
with ``BINFO_BASE_BINFO`` and ``BINFO_BASE_ITERATE``.  To add a
new binfo, use ``BINFO_BASE_APPEND``.  The vector of base binfos can
be obtained with ``BINFO_BASE_BINFOS``, but normally you do not need
to use that.  The class type associated with a binfo is given by
``BINFO_TYPE``.  It is not always the case that ``BINFO_TYPE
(TYPE_BINFO (x))``, because of typedefs and qualified types.  Neither is
it the case that ``TYPE_BINFO (BINFO_TYPE (y))`` is the same binfo as
``y``.  The reason is that if ``y`` is a binfo representing a
base-class ``B`` of a derived class ``D``, then ``BINFO_TYPE
(y)`` will be ``B``, and ``TYPE_BINFO (BINFO_TYPE (y))`` will be
``B`` as its own base-class, rather than as a base-class of ``D``.

The access to a base type can be found with ``BINFO_BASE_ACCESS``.
This will produce ``access_public_node``, ``access_private_node``
or ``access_protected_node``.  If bases are always public,
``BINFO_BASE_ACCESSES`` may be ``NULL``.

``BINFO_VIRTUAL_P`` is used to specify whether the binfo is inherited
virtually or not.  The other flags, ``BINFO_FLAG_0`` to
``BINFO_FLAG_6``, can be used for language specific use.

The following macros can be used on a tree node representing a class-type.

.. envvar:: LOCAL_CLASS_P

  This predicate holds if the class is local class *i.e.* declared
  inside a function body.

.. envvar:: TYPE_POLYMORPHIC_P

  This predicate holds if the class has at least one virtual function
  (declared or inherited).

.. envvar:: TYPE_HAS_DEFAULT_CONSTRUCTOR

  This predicate holds whenever its argument represents a class-type with
  default constructor.

.. envvar:: CLASSTYPE_HAS_MUTABLE

  These predicates hold for a class-type having a mutable data member.

.. envvar:: CLASSTYPE_NON_POD_P

  This predicate holds only for class-types that are not PODs.

.. envvar:: TYPE_HAS_NEW_OPERATOR

  This predicate holds for a class-type that defines
  ``operator new``.

.. envvar:: TYPE_HAS_ARRAY_NEW_OPERATOR

  This predicate holds for a class-type for which
  ``operator new[]`` is defined.

.. envvar:: TYPE_OVERLOADS_CALL_EXPR

  This predicate holds for class-type for which the function call
  ``operator()`` is overloaded.

.. envvar:: TYPE_OVERLOADS_ARRAY_REF

  This predicate holds for a class-type that overloads
  ``operator[]``

.. envvar:: TYPE_OVERLOADS_ARROW

  This predicate holds for a class-type for which ``operator->`` is
  overloaded.

.. index:: function

.. _functions-for-c++:

Functions for C++
^^^^^^^^^^^^^^^^^

.. index:: FUNCTION_DECL, OVERLOAD, OVL_CURRENT, OVL_NEXT

A function is represented by a ``FUNCTION_DECL`` node.  A set of
overloaded functions is sometimes represented by an ``OVERLOAD`` node.

An ``OVERLOAD`` node is not a declaration, so none of the
:samp:`DECL_` macros should be used on an ``OVERLOAD``.  An
``OVERLOAD`` node is similar to a ``TREE_LIST``.  Use
``OVL_CURRENT`` to get the function associated with an
``OVERLOAD`` node; use ``OVL_NEXT`` to get the next
``OVERLOAD`` node in the list of overloaded functions.  The macros
``OVL_CURRENT`` and ``OVL_NEXT`` are actually polymorphic; you can
use them to work with ``FUNCTION_DECL`` nodes as well as with
overloads.  In the case of a ``FUNCTION_DECL``, ``OVL_CURRENT``
will always return the function itself, and ``OVL_NEXT`` will always
be ``NULL_TREE``.

To determine the scope of a function, you can use the
``DECL_CONTEXT`` macro.  This macro will return the class
(either a ``RECORD_TYPE`` or a ``UNION_TYPE``) or namespace (a
``NAMESPACE_DECL``) of which the function is a member.  For a virtual
function, this macro returns the class in which the function was
actually defined, not the base class in which the virtual declaration
occurred.

If a friend function is defined in a class scope, the
``DECL_FRIEND_CONTEXT`` macro can be used to determine the class in
which it was defined.  For example, in

.. code-block:: c++

  class C { friend void f() {} };

the ``DECL_CONTEXT`` for ``f`` will be the
``global_namespace``, but the ``DECL_FRIEND_CONTEXT`` will be the
``RECORD_TYPE`` for ``C``.

The following macros and functions can be used on a ``FUNCTION_DECL`` :

.. envvar:: DECL_MAIN_P

  This predicate holds for a function that is the program entry point
  ``::code``.

.. envvar:: DECL_LOCAL_FUNCTION_P

  This predicate holds if the function was declared at block scope, even
  though it has a global scope.

.. envvar:: DECL_ANTICIPATED

  This predicate holds if the function is a built-in function but its
  prototype is not yet explicitly declared.

.. envvar:: DECL_EXTERN_C_FUNCTION_P

  This predicate holds if the function is declared as an
  ' ``extern "C"`` ' function.

.. envvar:: DECL_LINKONCE_P

  This macro holds if multiple copies of this function may be emitted in
  various translation units.  It is the responsibility of the linker to
  merge the various copies.  Template instantiations are the most common
  example of functions for which ``DECL_LINKONCE_P`` holds; G++
  instantiates needed templates in all translation units which require them,
  and then relies on the linker to remove duplicate instantiations.

  .. todo:: This macro is not yet implemented.

.. envvar:: DECL_FUNCTION_MEMBER_P

  This macro holds if the function is a member of a class, rather than a
  member of a namespace.

.. envvar:: DECL_STATIC_FUNCTION_P

  This predicate holds if the function a static member function.

.. envvar:: DECL_NONSTATIC_MEMBER_FUNCTION_P

  This macro holds for a non-static member function.

.. envvar:: DECL_CONST_MEMFUNC_P

  This predicate holds for a ``const`` -member function.

.. envvar:: DECL_VOLATILE_MEMFUNC_P

  This predicate holds for a ``volatile`` -member function.

.. envvar:: DECL_CONSTRUCTOR_P

  This macro holds if the function is a constructor.

.. envvar:: DECL_NONCONVERTING_P

  This predicate holds if the constructor is a non-converting constructor.

.. envvar:: DECL_COMPLETE_CONSTRUCTOR_P

  This predicate holds for a function which is a constructor for an object
  of a complete type.

.. envvar:: DECL_BASE_CONSTRUCTOR_P

  This predicate holds for a function which is a constructor for a base
  class sub-object.

.. envvar:: DECL_COPY_CONSTRUCTOR_P

  This predicate holds for a function which is a copy-constructor.

.. envvar:: DECL_DESTRUCTOR_P

  This macro holds if the function is a destructor.

.. envvar:: DECL_COMPLETE_DESTRUCTOR_P

  This predicate holds if the function is the destructor for an object a
  complete type.

.. envvar:: DECL_OVERLOADED_OPERATOR_P

  This macro holds if the function is an overloaded operator.

.. envvar:: DECL_CONV_FN_P

  This macro holds if the function is a type-conversion operator.

.. envvar:: DECL_GLOBAL_CTOR_P

  This predicate holds if the function is a file-scope initialization
  function.

.. envvar:: DECL_GLOBAL_DTOR_P

  This predicate holds if the function is a file-scope finalization
  function.

.. envvar:: DECL_THUNK_P

  This predicate holds if the function is a thunk.

  These functions represent stub code that adjusts the ``this`` pointer
  and then jumps to another function.  When the jumped-to function
  returns, control is transferred directly to the caller, without
  returning to the thunk.  The first parameter to the thunk is always the
  ``this`` pointer; the thunk should add ``THUNK_DELTA`` to this
  value.  (The ``THUNK_DELTA`` is an ``int``, not an
  ``INTEGER_CST``.)

  Then, if ``THUNK_VCALL_OFFSET`` (an ``INTEGER_CST``) is nonzero
  the adjusted ``this`` pointer must be adjusted again.  The complete
  calculation is given by the following pseudo-code:

  .. code-block:: c++

    this += THUNK_DELTA
    if (THUNK_VCALL_OFFSET)
      this += (*((ptrdiff_t **) this))[THUNK_VCALL_OFFSET]

  Finally, the thunk should jump to the location given
  by ``DECL_INITIAL`` ; this will always be an expression for the
  address of a function.

.. envvar:: DECL_NON_THUNK_FUNCTION_P

  This predicate holds if the function is *not* a thunk function.

.. envvar:: GLOBAL_INIT_PRIORITY

  If either ``DECL_GLOBAL_CTOR_P`` or ``DECL_GLOBAL_DTOR_P`` holds,
  then this gives the initialization priority for the function.  The
  linker will arrange that all functions for which
  ``DECL_GLOBAL_CTOR_P`` holds are run in increasing order of priority
  before ``main`` is called.  When the program exits, all functions for
  which ``DECL_GLOBAL_DTOR_P`` holds are run in the reverse order.

.. envvar:: TYPE_RAISES_EXCEPTIONS

  This macro returns the list of exceptions that a (member-)function can
  raise.  The returned list, if non ``NULL``, is comprised of nodes
  whose ``TREE_VALUE`` represents a type.

.. envvar:: TYPE_NOTHROW_P

  This predicate holds when the exception-specification of its arguments
  is of the form ' ``()`` '.

.. envvar:: DECL_ARRAY_DELETE_OPERATOR_P

  This predicate holds if the function an overloaded
  ``operator delete[]``.

.. -
   Function Bodies
   -

.. index:: statements

.. _statements-for-c-and-c++:

Statements for C and C++
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: BREAK_STMT, CLEANUP_STMT, CLEANUP_DECL, CLEANUP_EXPR, CONTINUE_STMT, DECL_STMT, DECL_STMT_DECL, DO_STMT, DO_BODY, DO_COND, EMPTY_CLASS_EXPR, EXPR_STMT, EXPR_STMT_EXPR, FOR_STMT, FOR_INIT_STMT, FOR_COND, FOR_EXPR, FOR_BODY, HANDLER, IF_STMT, IF_COND, THEN_CLAUSE, ELSE_CLAUSE, RETURN_STMT, RETURN_EXPR, SUBOBJECT, SUBOBJECT_CLEANUP, SWITCH_STMT, SWITCH_COND, SWITCH_BODY, TRY_BLOCK, TRY_STMTS, TRY_HANDLERS, HANDLER_PARMS, HANDLER_BODY, USING_STMT, WHILE_STMT, WHILE_BODY, WHILE_COND

A function that has a definition in the current translation unit has
a non- ``NULL`` ``DECL_INITIAL``.  However, back ends should not make
use of the particular value given by ``DECL_INITIAL``.

The ``DECL_SAVED_TREE`` gives the complete body of the
function.

There are tree nodes corresponding to all of the source-level
statement constructs, used within the C and C++ frontends.  These are
enumerated here, together with a list of the various macros that can
be used to obtain information about them.  There are a few macros that
can be used with all statements:

.. envvar:: STMT_IS_FULL_EXPR_P

  In C++, statements normally constitute 'full expressions'; temporaries
  created during a statement are destroyed when the statement is complete.
  However, G++ sometimes represents expressions by statements; these
  statements will not have ``STMT_IS_FULL_EXPR_P`` set.  Temporaries
  created during such statements should be destroyed when the innermost
  enclosing statement with ``STMT_IS_FULL_EXPR_P`` set is exited.

Here is the list of the various statement nodes, and the macros used to
access them.  This documentation describes the use of these nodes in
non-template functions (including instantiations of template functions).
In template functions, the same nodes are used, but sometimes in
slightly different ways.

Many of the statements have substatements.  For example, a ``while``
loop has a body, which is itself a statement.  If the substatement
is ``NULL_TREE``, it is considered equivalent to a statement
consisting of a single ``;``, i.e., an expression statement in which
the expression has been omitted.  A substatement may in fact be a list
of statements, connected via their ``TREE_CHAIN`` s.  So, you should
always process the statement tree by looping over substatements, like
this:

.. code-block:: c++

  void process_stmt (stmt)
       tree stmt;
  {
    while (stmt)
      {
        switch (TREE_CODE (stmt))
          {
          case IF_STMT:
            process_stmt (THEN_CLAUSE (stmt));
            /* More processing here.  */
            break;

          ...
          }

        stmt = TREE_CHAIN (stmt);
      }
  }

In other words, while the ``then`` clause of an ``if`` statement
in C++ can be only one statement (although that one statement may be a
compound statement), the intermediate representation sometimes uses
several statements chained together.

.. envvar:: BREAK_STMT

  Used to represent a ``break`` statement.  There are no additional
  fields.

.. envvar:: CLEANUP_STMT

  Used to represent an action that should take place upon exit from the
  enclosing scope.  Typically, these actions are calls to destructors for
  local objects, but back ends cannot rely on this fact.  If these nodes
  are in fact representing such destructors, ``CLEANUP_DECL`` will be
  the ``VAR_DECL`` destroyed.  Otherwise, ``CLEANUP_DECL`` will be
  ``NULL_TREE``.  In any case, the ``CLEANUP_EXPR`` is the
  expression to execute.  The cleanups executed on exit from a scope
  should be run in the reverse order of the order in which the associated
  ``CLEANUP_STMT`` s were encountered.

.. envvar:: CONTINUE_STMT

  Used to represent a ``continue`` statement.  There are no additional
  fields.

.. envvar:: CTOR_STMT

  Used to mark the beginning (if ``CTOR_BEGIN_P`` holds) or end (if
  ``CTOR_END_P`` holds of the main body of a constructor.  See also
  ``SUBOBJECT`` for more information on how to use these nodes.

.. envvar:: DO_STMT

  Used to represent a ``do`` loop.  The body of the loop is given by
  ``DO_BODY`` while the termination condition for the loop is given by
  ``DO_COND``.  The condition for a ``do`` -statement is always an
  expression.

.. envvar:: EMPTY_CLASS_EXPR

  Used to represent a temporary object of a class with no data whose
  address is never taken.  (All such objects are interchangeable.)  The
  ``TREE_TYPE`` represents the type of the object.

.. envvar:: EXPR_STMT

  Used to represent an expression statement.  Use ``EXPR_STMT_EXPR`` to
  obtain the expression.

.. envvar:: FOR_STMT

  Used to represent a ``for`` statement.  The ``FOR_INIT_STMT`` is
  the initialization statement for the loop.  The ``FOR_COND`` is the
  termination condition.  The ``FOR_EXPR`` is the expression executed
  right before the ``FOR_COND`` on each loop iteration; often, this
  expression increments a counter.  The body of the loop is given by
  ``FOR_BODY``.  ``FOR_SCOPE`` holds the scope of the ``for``
  statement (used in the C++ front end only).  Note that
  ``FOR_INIT_STMT`` and ``FOR_BODY`` return statements, while
  ``FOR_COND`` and ``FOR_EXPR`` return expressions.

.. envvar:: HANDLER

  Used to represent a C++ ``catch`` block.  The ``HANDLER_TYPE``
  is the type of exception that will be caught by this handler; it is
  equal (by pointer equality) to ``NULL`` if this handler is for all
  types.  ``HANDLER_PARMS`` is the ``DECL_STMT`` for the catch
  parameter, and ``HANDLER_BODY`` is the code for the block itself.

.. envvar:: IF_STMT

  Used to represent an ``if`` statement.  The ``IF_COND`` is the
  expression.

  If the condition is a ``TREE_LIST``, then the ``TREE_PURPOSE`` is
  a statement (usually a ``DECL_STMT``).  Each time the condition is
  evaluated, the statement should be executed.  Then, the
  ``TREE_VALUE`` should be used as the conditional expression itself.
  This representation is used to handle C++ code like this:

  .. code-block:: c++

    if (int i = 7) ...

  where there is a new local variable (or variables) declared within the
  condition.

  The ``THEN_CLAUSE`` represents the statement given by the ``then``
  condition, while the ``ELSE_CLAUSE`` represents the statement given
  by the ``else`` condition.

  C++ distinguishes between this and ``COND_EXPR`` for handling templates.

.. envvar:: SUBOBJECT

  In a constructor, these nodes are used to mark the point at which a
  subobject of ``this`` is fully constructed.  If, after this point, an
  exception is thrown before a ``CTOR_STMT`` with ``CTOR_END_P`` set
  is encountered, the ``SUBOBJECT_CLEANUP`` must be executed.  The
  cleanups must be executed in the reverse order in which they appear.

.. envvar:: SWITCH_STMT

  Used to represent a ``switch`` statement.  The ``SWITCH_STMT_COND``
  is the expression on which the switch is occurring.  See the documentation
  for an ``IF_STMT`` for more information on the representation used
  for the condition.  The ``SWITCH_STMT_BODY`` is the body of the switch
  statement.   The ``SWITCH_STMT_TYPE`` is the original type of switch
  expression as given in the source, before any compiler conversions.
  The ``SWITCH_STMT_SCOPE`` is the statement scope (used in the
  C++ front end only).

  There are also two boolean flags used with ``SWITCH_STMT``.
  ``SWITCH_STMT_ALL_CASES_P`` is true if the switch includes a default label
  or the case label ranges cover all possible values of the condition
  expression.  ``SWITCH_STMT_NO_BREAK_P`` is true if there are no
  ``break`` statements in the switch.

.. envvar:: TRY_BLOCK

  Used to represent a ``try`` block.  The body of the try block is
  given by ``TRY_STMTS``.  Each of the catch blocks is a ``HANDLER``
  node.  The first handler is given by ``TRY_HANDLERS``.  Subsequent
  handlers are obtained by following the ``TREE_CHAIN`` link from one
  handler to the next.  The body of the handler is given by
  ``HANDLER_BODY``.

  If ``CLEANUP_P`` holds of the ``TRY_BLOCK``, then the
  ``TRY_HANDLERS`` will not be a ``HANDLER`` node.  Instead, it will
  be an expression that should be executed if an exception is thrown in
  the try block.  It must rethrow the exception after executing that code.
  And, if an exception is thrown while the expression is executing,
  ``terminate`` must be called.

.. envvar:: USING_STMT

  Used to represent a ``using`` directive.  The namespace is given by
  ``USING_STMT_NAMESPACE``, which will be a NAMESPACE_DECL.  This node
  is needed inside template functions, to implement using directives
  during instantiation.

.. envvar:: WHILE_STMT

  Used to represent a ``while`` loop.  The ``WHILE_COND`` is the
  termination condition for the loop.  See the documentation for an
  ``IF_STMT`` for more information on the representation used for the
  condition.

  The ``WHILE_BODY`` is the body of the loop.

.. _c++-expressions:

C++ Expressions
^^^^^^^^^^^^^^^

This section describes expressions specific to the C and C++ front
ends.

.. envvar:: TYPEID_EXPR

  Used to represent a ``typeid`` expression.

.. envvar:: NEW_EXPR

  Used to represent a call to ``new`` and ``new[]`` respectively.

.. envvar:: DELETE_EXPR

  Used to represent a call to ``delete`` and ``delete[]`` respectively.

.. envvar:: MEMBER_REF

  Represents a reference to a member of a class.

.. envvar:: THROW_EXPR

  Represents an instance of ``throw`` in the program.  Operand 0,
  which is the expression to throw, may be ``NULL_TREE``.

.. envvar:: AGGR_INIT_EXPR

  An ``AGGR_INIT_EXPR`` represents the initialization as the return
  value of a function call, or as the result of a constructor.  An
  ``AGGR_INIT_EXPR`` will only appear as a full-expression, or as the
  second operand of a ``TARGET_EXPR``.  ``AGGR_INIT_EXPR`` s have
  a representation similar to that of ``CALL_EXPR`` s.  You can use
  the ``AGGR_INIT_EXPR_FN`` and ``AGGR_INIT_EXPR_ARG`` macros to access
  the function to call and the arguments to pass.

  If ``AGGR_INIT_VIA_CTOR_P`` holds of the ``AGGR_INIT_EXPR``, then
  the initialization is via a constructor call.  The address of the
  ``AGGR_INIT_EXPR_SLOT`` operand, which is always a ``VAR_DECL``,
  is taken, and this value replaces the first argument in the argument
  list.

  In either case, the expression is void.
