..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: compiler options, Objective-C and Objective-C++, Objective-C and Objective-C++ options, command-line, options, Objective-C and Objective-C++

.. _objective-c-and-objective-c++-dialect-options:

Options Controlling Objective-C and Objective-C++ Dialects
**********************************************************

.. note::

  This manual does not describe the Objective-C and Objective-C++
  languages themselves.  See :ref:`standards`, for references.

This section describes the command-line options that are only meaningful
for Objective-C and Objective-C++ programs.  You can also use most of
the language-independent GNU compiler options.
For example, you might compile a file :samp:`some_class.m` like this:

.. code-block:: shell

  gcc -g -fgnu-runtime -O -c some_class.m

In this example, :option:`-fgnu-runtime` is an option meant only for
Objective-C and Objective-C++ programs; you can use the other options with
any language supported by GCC.

Note that since Objective-C is an extension of the C language, Objective-C
compilations may also use options specific to the C front-end (e.g.,
:option:`-Wtraditional`).  Similarly, Objective-C++ compilations may use
C++-specific options (e.g., :option:`-Wabi`).

Here is a list of options that are *only* for compiling Objective-C
and Objective-C++ programs:

.. option:: -fconstant-string-class={class-name}

  Use :samp:`{class-name}` as the name of the class to instantiate for each
  literal string specified with the syntax ``@"..."``.  The default
  class name is ``NXConstantString`` if the GNU runtime is being used, and
  ``NSConstantString`` if the NeXT runtime is being used (see below).  The
  :option:`-fconstant-cfstrings` option, if also present, overrides the
  :option:`-fconstant-string-class` setting and cause ``@"..."`` literals
  to be laid out as constant CoreFoundation strings.

.. option:: -fgnu-runtime

  Generate object code compatible with the standard GNU Objective-C
  runtime.  This is the default for most types of systems.

.. option:: -fnext-runtime

  Generate output compatible with the NeXT runtime.  This is the default
  for NeXT-based systems, including Darwin and Mac OS X.  The macro
  ``__NEXT_RUNTIME__`` is predefined if (and only if) this option is
  used.

.. option:: -fno-nil-receivers

  Assume that all Objective-C message dispatches (``[receiver
  message:arg]``) in this translation unit ensure that the receiver is
  not ``nil``.  This allows for more efficient entry points in the
  runtime to be used.  This option is only available in conjunction with
  the NeXT runtime and ABI version 0 or 1.

.. option:: -fnil-receivers

  Default setting; overrides :option:`-fno-nil-receivers`.

.. option:: -fobjc-abi-version={n}

  Use version :samp:`{n}` of the Objective-C ABI for the selected runtime.
  This option is currently supported only for the NeXT runtime.  In that
  case, Version 0 is the traditional (32-bit) ABI without support for
  properties and other Objective-C 2.0 additions.  Version 1 is the
  traditional (32-bit) ABI with support for properties and other
  Objective-C 2.0 additions.  Version 2 is the modern (64-bit) ABI.  If
  nothing is specified, the default is Version 0 on 32-bit target
  machines, and Version 2 on 64-bit target machines.

.. option:: -fobjc-call-cxx-cdtors

  For each Objective-C class, check if any of its instance variables is a
  C++ object with a non-trivial default constructor.  If so, synthesize a
  special ``- (id) .cxx_construct`` instance method which runs
  non-trivial default constructors on any such instance variables, in order,
  and then return ``self``.  Similarly, check if any instance variable
  is a C++ object with a non-trivial destructor, and if so, synthesize a
  special ``- (void) .cxx_destruct`` method which runs
  all such default destructors, in reverse order.

  The ``- (id) .cxx_construct`` and ``- (void) .cxx_destruct``
  methods thusly generated only operate on instance variables
  declared in the current Objective-C class, and not those inherited
  from superclasses.  It is the responsibility of the Objective-C
  runtime to invoke all such methods in an object's inheritance
  hierarchy.  The ``- (id) .cxx_construct`` methods are invoked
  by the runtime immediately after a new object instance is allocated;
  the ``- (void) .cxx_destruct`` methods are invoked immediately
  before the runtime deallocates an object instance.

  As of this writing, only the NeXT runtime on Mac OS X 10.4 and later has
  support for invoking the ``- (id) .cxx_construct`` and
  ``- (void) .cxx_destruct`` methods.

.. option:: -fobjc-direct-dispatch

  Allow fast jumps to the message dispatcher.  On Darwin this is
  accomplished via the comm page.

.. option:: -fobjc-exceptions

  Enable syntactic support for structured exception handling in
  Objective-C, similar to what is offered by C++.  This option
  is required to use the Objective-C keywords ``@try``,
  ``@throw``, ``@catch``, ``@finally`` and
  ``@synchronized``.  This option is available with both the GNU
  runtime and the NeXT runtime (but not available in conjunction with
  the NeXT runtime on Mac OS X 10.2 and earlier).

.. option:: -fobjc-gc

  Enable garbage collection (GC) in Objective-C and Objective-C++
  programs.  This option is only available with the NeXT runtime; the
  GNU runtime has a different garbage collection implementation that
  does not require special compiler flags.

.. option:: -fobjc-nilcheck

  For the NeXT runtime with version 2 of the ABI, check for a nil
  receiver in method invocations before doing the actual method call.
  This is the default and can be disabled using
  :option:`-fno-objc-nilcheck`.  Class methods and super calls are never
  checked for nil in this way no matter what this flag is set to.
  Currently this flag does nothing when the GNU runtime, or an older
  version of the NeXT runtime ABI, is used.

.. option:: -fobjc-std=objc1

  Conform to the language syntax of Objective-C 1.0, the language
  recognized by GCC 4.0.  This only affects the Objective-C additions to
  the C/C++ language; it does not affect conformance to C/C++ standards,
  which is controlled by the separate C/C++ dialect option flags.  When
  this option is used with the Objective-C or Objective-C++ compiler,
  any Objective-C syntax that is not recognized by GCC 4.0 is rejected.
  This is useful if you need to make sure that your Objective-C code can
  be compiled with older versions of GCC.

.. option:: -freplace-objc-classes

  Emit a special marker instructing :command:`ld(1)` not to statically link in
  the resulting object file, and allow :command:`dyld(1)` to load it in at
  run time instead.  This is used in conjunction with the Fix-and-Continue
  debugging mode, where the object file in question may be recompiled and
  dynamically reloaded in the course of program execution, without the need
  to restart the program itself.  Currently, Fix-and-Continue functionality
  is only available in conjunction with the NeXT runtime on Mac OS X 10.3
  and later.

.. option:: -fzero-link

  When compiling for the NeXT runtime, the compiler ordinarily replaces calls
  to ``objc_getClass("...")`` (when the name of the class is known at
  compile time) with static class references that get initialized at load time,
  which improves run-time performance.  Specifying the :option:`-fzero-link` flag
  suppresses this behavior and causes calls to ``objc_getClass("...")``
  to be retained.  This is useful in Zero-Link debugging mode, since it allows
  for individual class implementations to be modified during program execution.
  The GNU runtime currently always retains calls to ``objc_get_class("...")``
  regardless of command-line options.

.. option:: -fno-local-ivars

  By default instance variables in Objective-C can be accessed as if
  they were local variables from within the methods of the class they're
  declared in.  This can lead to shadowing between instance variables
  and other variables declared either locally inside a class method or
  globally with the same name.  Specifying the :option:`-fno-local-ivars`
  flag disables this behavior thus avoiding variable shadowing issues.

.. option:: -flocal-ivars

  Default setting; overrides :option:`-fno-local-ivars`.

.. option:: -fivar-visibility=[public|protected|private|package]

  Set the default instance variable visibility to the specified option
  so that instance variables declared outside the scope of any access
  modifier directives default to the specified visibility.

.. option:: -gen-decls

  Dump interface declarations for all classes seen in the source file to a
  file named :samp:`{sourcename}.decl`.

.. option:: -Wassign-intercept

  .. note::

    Objective-C and Objective-C++ only

  Warn whenever an Objective-C assignment is being intercepted by the
  garbage collector.

.. option:: -Wno-assign-intercept

  Default setting; overrides :option:`-Wassign-intercept`.

.. option:: -Wno-property-assign-default

  .. note::

    Objective-C and Objective-C++ only

  Do not warn if a property for an Objective-C object has no assign
  semantics specified.

.. option:: -Wproperty-assign-default

  Default setting; overrides :option:`-Wno-property-assign-default`.

.. option:: -Wno-protocol

  .. note::

    Objective-C and Objective-C++ only

  If a class is declared to implement a protocol, a warning is issued for
  every method in the protocol that is not implemented by the class.  The
  default behavior is to issue a warning for every method not explicitly
  implemented in the class, even if a method implementation is inherited
  from the superclass.  If you use the :option:`-Wno-protocol` option, then
  methods inherited from the superclass are considered to be implemented,
  and no warning is issued for them.

.. option:: -Wprotocol

  Default setting; overrides :option:`-Wno-protocol`.

.. option:: -Wobjc-root-class

  .. note::

    Objective-C and Objective-C++ only

  Warn if a class interface lacks a superclass. Most classes will inherit
  from ``NSObject`` (or ``Object``) for example.  When declaring
  classes intended to be root classes, the warning can be suppressed by
  marking their interfaces with ``__attribute__((objc_root_class))``.

.. option:: -Wselector

  .. note::

    Objective-C and Objective-C++ only

  Warn if multiple methods of different types for the same selector are
  found during compilation.  The check is performed on the list of methods
  in the final stage of compilation.  Additionally, a check is performed
  for each selector appearing in a ``@selector(...)``
  expression, and a corresponding method for that selector has been found
  during compilation.  Because these checks scan the method table only at
  the end of compilation, these warnings are not produced if the final
  stage of compilation is not reached, for example because an error is
  found during compilation, or because the :option:`-fsyntax-only` option is
  being used.

.. option:: -Wno-selector

  Default setting; overrides :option:`-Wselector`.

.. option:: -Wstrict-selector-match

  .. note::

    Objective-C and Objective-C++ only

  Warn if multiple methods with differing argument and/or return types are
  found for a given selector when attempting to send a message using this
  selector to a receiver of type ``id`` or ``Class``.  When this flag
  is off (which is the default behavior), the compiler omits such warnings
  if any differences found are confined to types that share the same size
  and alignment.

.. option:: -Wno-strict-selector-match

  Default setting; overrides :option:`-Wstrict-selector-match`.

.. option:: -Wundeclared-selector

  .. note::

    Objective-C and Objective-C++ only

  Warn if a ``@selector(...)`` expression referring to an
  undeclared selector is found.  A selector is considered undeclared if no
  method with that name has been declared before the
  ``@selector(...)`` expression, either explicitly in an
  ``@interface`` or ``@protocol`` declaration, or implicitly in
  an ``@implementation`` section.  This option always performs its
  checks as soon as a ``@selector(...)`` expression is found,
  while :option:`-Wselector` only performs its checks in the final stage of
  compilation.  This also enforces the coding style convention
  that methods and selectors must be declared before being used.

.. option:: -Wno-undeclared-selector

  Default setting; overrides :option:`-Wundeclared-selector`.

.. option:: -print-objc-runtime-info

  Generate C header describing the largest structure that is passed by
  value, if any.
