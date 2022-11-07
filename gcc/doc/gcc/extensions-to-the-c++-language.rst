..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: extensions, C++ language, C++ language extensions

.. _c++-extensions:

Extensions to the C++ Language
------------------------------

The GNU compiler provides these extensions to the C++ language (and you
can also use most of the C language extensions in your C++ programs).  If you
want to write code that checks whether these features are available, you can
test for the GNU compiler the same way as for C programs: check for a
predefined macro ``__GNUC__``.  You can also use ``__GNUG__`` to
test specifically for GNU C++ (see :ref:`cpp:common-predefined-macros`).

.. toctree::
  :maxdepth: 2

  extensions-to-the-c++-language/vague-linkage
  extensions-to-the-c++-language/function-multiversioning
  extensions-to-the-c++-language/type-traits
  extensions-to-the-c++-language/c++-concepts
  extensions-to-the-c++-language/deprecated-features
  extensions-to-the-c++-language/backwards-compatibility
  extensions-to-the-c++-language/when-is-a-volatile-c++-object-accessed
  extensions-to-the-c++-language/restricting-pointer-aliasing
  extensions-to-the-c++-language/c++-interface-and-implementation-pragmas
  extensions-to-the-c++-language/wheres-the-template
  extensions-to-the-c++-language/extracting-the-function-pointer-from-a-bound-pointer-to-member-function
  extensions-to-the-c++-language/c++-specific-variable-function-and-type-attributes