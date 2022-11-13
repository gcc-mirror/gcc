..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Objective-C, Objective-C++

Objective-C and Objective-C++ Languages
***************************************

GCC supports 'traditional' Objective-C (also known as 'Objective-C
1.0') and contains support for the Objective-C exception and
synchronization syntax.  It has also support for a number of
'Objective-C 2.0' language extensions, including properties, fast
enumeration (only for Objective-C), method attributes and the
@optional and @required keywords in protocols.  GCC supports
Objective-C++ and features available in Objective-C are also available
in Objective-C++.

GCC by default uses the GNU Objective-C runtime library, which is part
of GCC and is not the same as the Apple/NeXT Objective-C runtime
library used on Apple systems.  There are a number of differences
documented in this manual.  The options :option:`-fgnu-runtime` and
:option:`-fnext-runtime` allow you to switch between producing output
that works with the GNU Objective-C runtime library and output that
works with the Apple/NeXT Objective-C runtime library.

There is no formal written standard for Objective-C or Objective-C++.
The authoritative manual on traditional Objective-C (1.0) is
'Object-Oriented Programming and the Objective-C Language':
http://www.gnustep.org/resources/documentation/ObjectivCBook.pdf
is the original NeXTstep document.

The Objective-C exception and synchronization syntax (that is, the
keywords ``@try``, ``@throw``, ``@catch``,
``@finally`` and ``@synchronized``) is
supported by GCC and is enabled with the option
:option:`-fobjc-exceptions`.  The syntax is briefly documented in this
manual and in the Objective-C 2.0 manuals from Apple.

The Objective-C 2.0 language extensions and features are automatically
enabled; they include properties (via the ``@property``,
``@synthesize`` and
``@dynamic keywords``), fast enumeration (not available in
Objective-C++), attributes for methods (such as :fn-attr:`deprecated`,
:fn-attr:`noreturn`, :fn-attr:`sentinel`, ``format``),
the :fn-attr:`unused` attribute for method arguments, the
``@package`` keyword for instance variables and the ``@optional`` and
``@required`` keywords in protocols.  You can disable all these
Objective-C 2.0 language extensions with the option
:option:`-fobjc-std=objc1`, which causes the compiler to recognize the
same Objective-C language syntax recognized by GCC 4.0, and to produce
an error if one of the new features is used.

GCC has currently no support for non-fragile instance variables.

The authoritative manual on Objective-C 2.0 is available from Apple:

* https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html

For more information concerning the history of Objective-C that is
available online, see https://gcc.gnu.org/readings.html