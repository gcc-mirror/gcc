..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _deprecated-features:

Deprecated Features
*******************

In the past, the GNU C++ compiler was extended to experiment with new
features, at a time when the C++ language was still evolving.  Now that
the C++ standard is complete, some of those features are superseded by
superior alternatives.  Using the old features might cause a warning in
some cases that the feature will be dropped in the future.  In other
cases, the feature might be gone already.

G++ allows a virtual function returning :samp:`void *` to be overridden
by one returning a different pointer type.  This extension to the
covariant return type rules is now deprecated and will be removed from a
future version.

The use of default arguments in function pointers, function typedefs
and other places where they are not permitted by the standard is
deprecated and will be removed from a future version of G++.

G++ allows floating-point literals to appear in integral constant expressions,
e.g. :samp:`enum E { e = int(2.2 * 3.7) }`
This extension is deprecated and will be removed from a future version.

G++ allows static data members of const floating-point type to be declared
with an initializer in a class definition. The standard only allows
initializers for static members of const integral types and const
enumeration types so this extension has been deprecated and will be removed
from a future version.

G++ allows attributes to follow a parenthesized direct initializer,
e.g. :samp:`int f (0) __attribute__ ((something));` This extension
has been ignored since G++ 3.3 and is deprecated.

G++ allows anonymous structs and unions to have members that are not
public non-static data members (i.e. fields).  These extensions are
deprecated.
