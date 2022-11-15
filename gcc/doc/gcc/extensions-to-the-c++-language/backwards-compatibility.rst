..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Backwards Compatibility, ARM [Annotated C++ Reference Manual]

.. _backwards-compatibility:

Backwards Compatibility
***********************

Now that there is a definitive ISO standard C++, G++ has a specification
to adhere to.  The C++ language evolved over time, and features that
used to be acceptable in previous drafts of the standard, such as the ARM
[Annotated C++ Reference Manual], are no longer accepted.  In order to allow
compilation of C++ written to such drafts, G++ contains some backwards
compatibilities.  *All such backwards compatibility features are
liable to disappear in future versions of G++.* They should be considered
deprecated.   See :ref:`deprecated-features`.

``Implicit C language``
  Old C system header files did not contain an ``extern "C" {...}``
  scope to set the language.  On such systems, all system header files are
  implicitly scoped inside a C language scope.  Such headers must
  correctly prototype function argument types, there is no leeway for
  ``()`` to indicate an unspecified set of arguments.
