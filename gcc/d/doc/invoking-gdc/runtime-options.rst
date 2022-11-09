..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options, runtime

.. _runtime-options:

Runtime Options
***************

These options affect the runtime behavior of programs compiled with
:command:`gdc`.

.. option:: -fall-instantiations

  .. index:: -fall-instantiations, -fno-all-instantiations

  Generate code for all template instantiations.  The default template emission
  strategy is to not generate code for declarations that were either
  instantiated speculatively, such as from ``__traits(compiles, ...)``, or
  that come from an imported module not being compiled.

.. option:: -fno-assert

  .. index:: -fassert, -fno-assert

  Turn off code generation for ``assert`` contracts.

.. option:: -fno-bounds-check

  .. index:: -fbounds-check, -fno-bounds-check

  Turns off array bounds checking for all functions, which can improve
  performance for code that uses arrays extensively.  Note that this
  can result in unpredictable behavior if the code in question actually
  does violate array bounds constraints.  It is safe to use this option
  if you are sure that your code never throws a ``RangeError``.

.. option:: -fbounds-check=value

  .. index:: -fbounds-check=

  An alternative to :option:`-fbounds-check` that allows more control
  as to where bounds checking is turned on or off.  The following values
  are supported:

  :samp:`on`
    Turns on array bounds checking for all functions.

  :samp:`safeonly`
    Turns on array bounds checking only for ``@safe`` functions.

  :samp:`off`
    Turns off array bounds checking completely.

.. option:: -fno-builtin

  .. index:: -fbuiltin, -fno-builtin

  Don't recognize built-in functions unless they begin with the prefix
  :samp:`__builtin_`.  By default, the compiler will recognize when a
  function in the ``core.stdc`` package is a built-in function.

.. option:: -fcheckaction

  This option controls what code is generated on an assertion, bounds check, or
  final switch failure.  The following values are supported:

  :samp:`context`
    Throw an ``AssertError`` with extra context information.

  :samp:`halt`
    Halt the program execution.

  :samp:`throw`
    Throw an ``AssertError`` (the default).

.. option:: -fdebug=value

  .. index:: -fno-debug

  Turn on compilation of conditional ``debug`` code into the program.
  The :option:`-fdebug` option itself sets the debug level to ``1``,
  while :option:`-fdebug=` enables ``debug`` code that are identified
  by any of the following values:

  :samp:`level`
    Sets the debug level to :samp:`{level}`, any ``debug`` code <= :samp:`{level}`
    is compiled into the program.

  :samp:`ident`
    Turns on compilation of any ``debug`` code identified by :samp:`{ident}`.

.. option:: -fno-druntime

  .. index:: -fdruntime, -fno-druntime

  Implements https://dlang.org/spec/betterc.html.  Assumes that
  compilation targets an environment without a D runtime library.

  This is equivalent to compiling with the following options:

  .. code-block:: c++

    gdc -nophoboslib -fno-exceptions -fno-moduleinfo -fno-rtti

.. option:: -fextern-std=standard

  Sets the C++ name mangling compatibility to the version identified by
  :samp:`{standard}`.  The following values are supported:

  :samp:`c++98`, :samp:`c++03`
    Sets ``__traits(getTargetInfo, "cppStd")`` to ``199711``.

  :samp:`c++11`
    Sets ``__traits(getTargetInfo, "cppStd")`` to ``201103``.

  :samp:`c++14`
    Sets ``__traits(getTargetInfo, "cppStd")`` to ``201402``.

  :samp:`c++17`
    Sets ``__traits(getTargetInfo, "cppStd")`` to ``201703``.
    This is the default.

  :samp:`c++20`
    Sets ``__traits(getTargetInfo, "cppStd")`` to ``202002``.

.. option:: -fno-invariants

  .. index:: -finvariants, -fno-invariants

  Turns off code generation for class ``invariant`` contracts.

.. option:: -fmain

  Generates a default ``main()`` function when compiling.  This is useful when
  unittesting a library, as it enables running the unittests in a library without
  having to manually define an entry-point function.  This option does nothing
  when ``main`` is already defined in user code.

.. option:: -fno-moduleinfo

  Turns off generation of the ``ModuleInfo`` and related functions
  that would become unreferenced without it, which may allow linking
  to programs not written in D.  Functions that are not be generated
  include module constructors and destructors (``static this`` and
  ``static ~this``), ``unittest`` code, and ``DSO`` registry
  functions for dynamically linked code.

.. option:: -fonly=filename

  .. index:: -fonly

  Tells the compiler to parse and run semantic analysis on all modules
  on the command line, but only generate code for the module specified
  by :samp:`{filename}`.

.. option:: -fno-postconditions

  .. index:: -fpostconditions, -fno-postconditions

  Turns off code generation for postcondition ``out`` contracts.

.. option:: -fno-preconditions

  .. index:: -fpreconditions, -fno-preconditions

  Turns off code generation for precondition ``in`` contracts.

.. option:: -fpreview=id

  .. index:: -fpreview

  Turns on an upcoming D language change identified by :samp:`{id}`.  The following
  values are supported:

  :samp:`all`
    Turns on all upcoming D language features.

  :samp:`dip1000`
    Implements https://github.com/dlang/DIPs/blob/master/DIPs/other/DIP1000.md
    (Scoped pointers).

  :samp:`dip1008`
    Implements https://github.com/dlang/DIPs/blob/master/DIPs/other/DIP1008.md
    (Allow exceptions in ``@nogc`` code).

  :samp:`dip1021`
    Implements https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1021.md
    (Mutable function arguments).

  :samp:`dip25`
    Implements https://github.com/dlang/DIPs/blob/master/DIPs/archive/DIP25.md
    (Sealed references).

  :samp:`dtorfields`
    Turns on generation for destructing fields of partially constructed objects.

  :samp:`fieldwise`
    Turns on generation of struct equality to use field-wise comparisons.

  :samp:`fixaliasthis`
    Implements new lookup rules that check the current scope for ``alias this``
    before searching in upper scopes.

  :samp:`fiximmutableconv`
    Disallows unsound immutable conversions that were formerly incorrectly
    permitted.

  :samp:`in`
    Implements ``in`` parameters to mean ``scope const [ref]`` and accepts
    rvalues.

  :samp:`inclusiveincontracts`
    Implements ``in`` contracts of overridden methods to be a superset of parent
    contract.

  :samp:`intpromote`
    Implements C-style integral promotion for unary ``+``, ``-`` and ``~``
    expressions.

  :samp:`nosharedaccess`
    Turns off and disallows all access to shared memory objects.

  :samp:`rvaluerefparam`
    Implements rvalue arguments to ``ref`` parameters.

  :samp:`systemvariables`
    Disables access to variables marked ``@system`` from ``@safe`` code.

.. option:: -frelease

  .. index:: -fno-release

  Turns on compiling in release mode, which means not emitting runtime
  checks for contracts and asserts.  Array bounds checking is not done
  for ``@system`` and ``@trusted`` functions, and assertion
  failures are undefined behavior.

  This is equivalent to compiling with the following options:

  .. code-block:: c++

    gdc -fno-assert -fbounds-check=safe -fno-invariants \
        -fno-postconditions -fno-preconditions -fno-switch-errors

.. option:: -frevert=

  .. index:: -frevert

  Turns off a D language feature identified by :samp:`{id}`.  The following values
  are supported:

  :samp:`all`
    Turns off all revertable D language features.

  :samp:`dip25`
    Reverts https://github.com/dlang/DIPs/blob/master/DIPs/archive/DIP25.md
    (Sealed references).

  :samp:`dtorfields`
    Turns off generation for destructing fields of partially constructed objects.

  :samp:`markdown`
    Turns off Markdown replacements in Ddoc comments.

.. option:: -fno-rtti

  .. index:: -frtti, -fno-rtti

  Turns off generation of run-time type information for all user defined types.
  Any code that uses features of the language that require access to this
  information will result in an error.

.. option:: -fno-switch-errors

  .. index:: -fswitch-errors, -fno-switch-errors

  This option controls what code is generated when no case is matched
  in a ``final switch`` statement.  The default run time behavior
  is to throw a ``SwitchError``.  Turning off :option:`-fswitch-errors`
  means that instead the execution of the program is immediately halted.

.. option:: -funittest

  .. index:: -funittest, -fno-unittest

  Turns on compilation of ``unittest`` code, and turns on the
  ``version(unittest)`` identifier.  This implies :option:`-fassert`.

.. option:: -fversion=value

  .. index:: -fversion

  Turns on compilation of conditional ``version`` code into the program
  identified by any of the following values:

  :samp:`level`
    Sets the version level to :samp:`{level}`, any ``version`` code >= :samp:`{level}`
    is compiled into the program.

  :samp:`ident`
    Turns on compilation of ``version`` code identified by :samp:`{ident}`.

.. option:: -fno-weak-templates

  .. index:: -fweak-templates, -fno-weak-templates

  Turns off emission of declarations that can be defined in multiple objects as
  weak symbols.  The default is to emit all public symbols as weak, unless the
  target lacks support for weak symbols.  Disabling this option means that common
  symbols are instead put in COMDAT or become private.
