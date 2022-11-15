..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: pragmas, #pragma

.. _pragmas:

Pragmas Accepted by GCC
***********************

GCC supports several types of pragmas, primarily in order to compile
code originally written for other compilers.  Note that in general
we do not recommend the use of pragmas; See :ref:`function-attributes`,
for further explanation.

The GNU C preprocessor recognizes several pragmas in addition to the
compiler pragmas documented here.  Refer to the CPP manual for more
information.

.. toctree::
  :maxdepth: 2


.. _aarch64-pragmas:

AArch64 Pragmas
^^^^^^^^^^^^^^^

The pragmas defined by the AArch64 target correspond to the AArch64
target function attributes.  They can be specified as below:

.. code-block:: c++

  #pragma GCC target("string")

where ``string`` can be any string accepted as an AArch64 target
attribute.  See :ref:`aarch64-function-attributes`, for more details
on the permissible values of ``string``.

.. _arm-pragmas:

ARM Pragmas
^^^^^^^^^^^

The ARM target defines pragmas for controlling the default addition of
:arm-fn-attr:`long_call` and ``short_call`` attributes to functions.
See :ref:`function-attributes`, for information about the effects of these
attributes.

``long_calls``

  .. index:: pragma, long_calls

  Set all subsequent functions to have the :arm-fn-attr:`long_call` attribute.

``no_long_calls``

  .. index:: pragma, no_long_calls

  Set all subsequent functions to have the ``short_call`` attribute.

``long_calls_off``

  .. index:: pragma, long_calls_off

  Do not affect the :arm-fn-attr:`long_call` or ``short_call`` attributes of
  subsequent functions.

.. _m32c-pragmas:

M32C Pragmas
^^^^^^^^^^^^

:samp:`GCC memregs {number}`

  .. index:: pragma, memregs

  Overrides the command-line option ``-memregs=`` for the current
  file.  Use with care!  This pragma must be before any function in the
  file, and mixing different memregs values in different objects may
  make them incompatible.  This pragma is useful when a
  performance-critical function uses a memreg for temporary values,
  as it may allow you to reduce the number of memregs used.

:samp:`ADDRESS {name}{address}`

  .. index:: pragma, address

  For any declared symbols matching :samp:`{name}`, this does three things
  to that symbol: it forces the symbol to be located at the given
  address (a number), it forces the symbol to be volatile, and it
  changes the symbol's scope to be static.  This pragma exists for
  compatibility with other compilers, but note that the common
  ``1234H`` numeric syntax is not supported (use ``0x1234``
  instead).  Example:

  .. code-block:: c++

    #pragma ADDRESS port3 0x103
    char port3;

.. _mep-pragmas:

MeP Pragmas
^^^^^^^^^^^

``custom io_volatile (on|off)``

  .. index:: pragma, custom io_volatile

  Overrides the command-line option ``-mio-volatile`` for the current
  file.  Note that for compatibility with future GCC releases, this
  option should only be used once before any :mep-var-attr:`io` variables in each
  file.

:samp:`GCC coprocessor available {registers}`

  .. index:: pragma, coprocessor available

  Specifies which coprocessor registers are available to the register
  allocator.  :samp:`{registers}` may be a single register, register range
  separated by ellipses, or comma-separated list of those.  Example:

  .. code-block:: c++

    #pragma GCC coprocessor available $c0...$c10, $c28

:samp:`GCC coprocessor call_saved {registers}`

  .. index:: pragma, coprocessor call_saved

  Specifies which coprocessor registers are to be saved and restored by
  any function using them.  :samp:`{registers}` may be a single register,
  register range separated by ellipses, or comma-separated list of
  those.  Example:

  .. code-block:: c++

    #pragma GCC coprocessor call_saved $c4...$c6, $c31

:samp:`GCC coprocessor subclass '(A|B|C|D)' = {registers}`

  .. index:: pragma, coprocessor subclass

  Creates and defines a register class.  These register classes can be
  used by inline ``asm`` constructs.  :samp:`{registers}` may be a single
  register, register range separated by ellipses, or comma-separated
  list of those.  Example:

  .. code-block:: c++

    #pragma GCC coprocessor subclass 'B' = $c2, $c4, $c6

    asm ("cpfoo %0" : "=B" (x));

:samp:`GCC disinterrupt {name} , {name} ...`

  .. index:: pragma, disinterrupt

  For the named functions, the compiler adds code to disable interrupts
  for the duration of those functions.  If any functions so named
  are not encountered in the source, a warning is emitted that the pragma is
  not used.  Examples:

  .. code-block:: c++

    #pragma disinterrupt foo
    #pragma disinterrupt bar, grill
    int foo () { ... }

:samp:`GCC call {name} , {name} ...`

  .. index:: pragma, call

  For the named functions, the compiler always uses a register-indirect
  call model when calling the named functions.  Examples:

  .. code-block:: c++

    extern int foo ();
    #pragma call foo

.. _pru-pragmas:

PRU Pragmas
^^^^^^^^^^^

:samp:`ctable_entry {index}{constant_address}`

  .. index:: pragma, ctable_entry

  Specifies that the PRU CTABLE entry given by :samp:`{index}` has the value
  :samp:`{constant_address}`.  This enables GCC to emit LBCO/SBCO instructions
  when the load/store address is known and can be addressed with some CTABLE
  entry.  For example:

  .. code-block:: c++

    /* will compile to "sbco Rx, 2, 0x10, 4" */
    #pragma ctable_entry 2 0x4802a000
    *(unsigned int *)0x4802a010 = val;

.. _rs-6000-and-powerpc-pragmas:

RS/6000 and PowerPC Pragmas
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The RS/6000 and PowerPC targets define one pragma for controlling
whether or not the :powerpc-fn-attr:`longcall` attribute is added to function
declarations by default.  This pragma overrides the :option:`-mlongcall`
option, but not the :powerpc-fn-attr:`longcall` and ``shortcall`` attributes.
See :ref:`rs-6000-and-powerpc-options`, for more information about when long
calls are and are not necessary.

``longcall (1)``

  .. index:: pragma, longcall

  Apply the :powerpc-fn-attr:`longcall` attribute to all subsequent function
  declarations.

``longcall (0)``
  Do not apply the :powerpc-fn-attr:`longcall` attribute to subsequent function
  declarations.

.. Describe h8300 pragmas here.
   Describe sh pragmas here.
   Describe v850 pragmas here.

.. _s-390-pragmas:

S/390 Pragmas
^^^^^^^^^^^^^

The pragmas defined by the S/390 target correspond to the S/390
target function attributes and some the additional options:

:samp:`zvector`
:samp:`no-zvector`

Note that options of the pragma, unlike options of the target
attribute, do change the value of preprocessor macros like
``__VEC__``.  They can be specified as below:

.. code-block:: c++

  #pragma GCC target("string[,string]...")
  #pragma GCC target("string"[,"string"]...)

.. _darwin-pragmas:

Darwin Pragmas
^^^^^^^^^^^^^^

The following pragmas are available for all architectures running the
Darwin operating system.  These are useful for compatibility with other
Mac OS compilers.

:samp:`mark {tokens}...`

  .. index:: pragma, mark

  This pragma is accepted, but has no effect.

:samp:`options align={alignment}`

  .. index:: pragma, options align

  This pragma sets the alignment of fields in structures.  The values of
  :samp:`{alignment}` may be ``mac68k``, to emulate m68k alignment, or
  ``power``, to emulate PowerPC alignment.  Uses of this pragma nest
  properly; to restore the previous setting, use ``reset`` for the
  :samp:`{alignment}`.

:samp:`segment {tokens}...`

  .. index:: pragma, segment

  This pragma is accepted, but has no effect.

:samp:`unused ({var} [, {var}]...)`

  .. index:: pragma, unused

  This pragma declares variables to be possibly unused.  GCC does not
  produce warnings for the listed variables.  The effect is similar to
  that of the :var-attr:`unused` attribute, except that this pragma may appear
  anywhere within the variables' scopes.

.. _solaris-pragmas:

Solaris Pragmas
^^^^^^^^^^^^^^^

The Solaris target supports ``#pragma redefine_extname``
(see :ref:`symbol-renaming-pragmas`).  It also supports additional
``#pragma`` directives for compatibility with the system compiler.

:samp:`align {alignment} ({variable} [, {variable}]...)`

  .. index:: pragma, align

  Increase the minimum alignment of each :samp:`{variable}` to :samp:`{alignment}`.
  This is the same as GCC's :var-attr:`aligned` attribute see :ref:`variable-attributes`).  Macro expansion occurs on the arguments to this pragma
  when compiling C and Objective-C.  It does not currently occur when
  compiling C++, but this is a bug which may be fixed in a future
  release.

:samp:`fini ({function} [, {function}]...)`

  .. index:: pragma, fini

  This pragma causes each listed :samp:`{function}` to be called after
  main, or during shared module unloading, by adding a call to the
  ``.fini`` section.

:samp:`init ({function} [, {function}]...)`

  .. index:: pragma, init

  This pragma causes each listed :samp:`{function}` to be called during
  initialization (before ``main``) or during shared module loading, by
  adding a call to the ``.init`` section.

.. _symbol-renaming-pragmas:

Symbol-Renaming Pragmas
^^^^^^^^^^^^^^^^^^^^^^^

GCC supports a ``#pragma`` directive that changes the name used in
assembly for a given declaration. While this pragma is supported on all
platforms, it is intended primarily to provide compatibility with the
Solaris system headers. This effect can also be achieved using the asm
labels extension (see :ref:`asm-labels`).

:samp:`redefine_extname {oldname}{newname}`

  .. index:: pragma, redefine_extname

  This pragma gives the C function :samp:`{oldname}` the assembly symbol
  :samp:`{newname}`.  The preprocessor macro ``__PRAGMA_REDEFINE_EXTNAME``
  is defined if this pragma is available (currently on all platforms).

This pragma and the ``asm`` labels extension interact in a complicated
manner.  Here are some corner cases you may want to be aware of:

* This pragma silently applies only to declarations with external
  linkage.  The ``asm`` label feature does not have this restriction.

* In C++, this pragma silently applies only to declarations with
  'C' linkage.  Again, ``asm`` labels do not have this restriction.

* If either of the ways of changing the assembly name of a
  declaration are applied to a declaration whose assembly name has
  already been determined (either by a previous use of one of these
  features, or because the compiler needed the assembly name in order to
  generate code), and the new name is different, a warning issues and
  the name does not change.

* The :samp:`{oldname}` used by ``#pragma redefine_extname`` is
  always the C-language name.

.. _structure-layout-pragmas:

Structure-Layout Pragmas
^^^^^^^^^^^^^^^^^^^^^^^^

For compatibility with Microsoft Windows compilers, GCC supports a
set of ``#pragma`` directives that change the maximum alignment of
members of structures (other than zero-width bit-fields), unions, and
classes subsequently defined. The :samp:`{n}` value below always is required
to be a small power of two and specifies the new alignment in bytes.

* ``#pragma pack(n)`` simply sets the new alignment.

* ``#pragma pack()`` sets the alignment to the one that was in
  effect when compilation started (see also command-line option
  :option:`-fpack-struct[=n]` see :ref:`code-gen-options`).

* ``#pragma pack(push[,n])`` pushes the current alignment
  setting on an internal stack and then optionally sets the new alignment.

* ``#pragma pack(pop)`` restores the alignment setting to the one
  saved at the top of the internal stack (and removes that stack entry).
  Note that ``#pragma pack([n])`` does not influence this internal
  stack; thus it is possible to have ``#pragma pack(push)`` followed by
  multiple ``#pragma pack(n)`` instances and finalized by a single
  ``#pragma pack(pop)``.

Some targets, e.g. x86 and PowerPC, support the ``#pragma ms_struct``
directive which lays out structures and unions subsequently defined as the
documented ``__attribute__ ((ms_struct))``.

* ``#pragma ms_struct on`` turns on the Microsoft layout.

* ``#pragma ms_struct off`` turns off the Microsoft layout.

* ``#pragma ms_struct reset`` goes back to the default layout.

Most targets also support the ``#pragma scalar_storage_order`` directive
which lays out structures and unions subsequently defined as the documented
``__attribute__ ((scalar_storage_order))``.

* ``#pragma scalar_storage_order big-endian`` sets the storage order
  of the scalar fields to big-endian.

* ``#pragma scalar_storage_order little-endian`` sets the storage order
  of the scalar fields to little-endian.

* ``#pragma scalar_storage_order default`` goes back to the endianness
  that was in effect when compilation started (see also command-line option
  :option:`-fsso-struct=endianness` see :ref:`c-dialect-options`).

.. _weak-pragmas:

Weak Pragmas
^^^^^^^^^^^^

For compatibility with SVR4, GCC supports a set of ``#pragma``
directives for declaring symbols to be weak, and defining weak
aliases.

:samp:`#pragma weak {symbol}`
  This pragma declares :samp:`{symbol}` to be weak, as if the declaration
  had the attribute of the same name.  The pragma may appear before
  or after the declaration of :samp:`{symbol}`.  It is not an error for
  :samp:`{symbol}` to never be defined at all.

:samp:`#pragma weak {symbol1} = {symbol2}`
  This pragma declares :samp:`{symbol1}` to be a weak alias of :samp:`{symbol2}`.
  It is an error if :samp:`{symbol2}` is not defined in the current
  translation unit.

.. _diagnostic-pragmas:

Diagnostic Pragmas
^^^^^^^^^^^^^^^^^^

GCC allows the user to selectively enable or disable certain types of
diagnostics, and change the kind of the diagnostic.  For example, a
project's policy might require that all sources compile with
:option:`-Werror` but certain files might have exceptions allowing
specific types of warnings.  Or, a project might selectively enable
diagnostics and treat them as errors depending on which preprocessor
macros are defined.

:samp:`#pragma GCC diagnostic {kind}{option}`
  Modifies the disposition of a diagnostic.  Note that not all
  diagnostics are modifiable; at the moment only warnings (normally
  controlled by :samp:`-W...`) can be controlled, and not all of them.
  Use :option:`-fdiagnostics-show-option` to determine which diagnostics
  are controllable and which option controls them.

  :samp:`{kind}` is :samp:`error` to treat this diagnostic as an error,
  :samp:`warning` to treat it like a warning (even if :option:`-Werror` is
  in effect), or :samp:`ignored` if the diagnostic is to be ignored.
  :samp:`{option}` is a double quoted string that matches the command-line
  option.

  .. code-block:: c++

    #pragma GCC diagnostic warning "-Wformat"
    #pragma GCC diagnostic error "-Wformat"
    #pragma GCC diagnostic ignored "-Wformat"

  Note that these pragmas override any command-line options.  GCC keeps
  track of the location of each pragma, and issues diagnostics according
  to the state as of that point in the source file.  Thus, pragmas occurring
  after a line do not affect diagnostics caused by that line.

``#pragma GCC diagnostic push``, ``#pragma GCC diagnostic pop``
  Causes GCC to remember the state of the diagnostics as of each
  ``push``, and restore to that point at each ``pop``.  If a
  ``pop`` has no matching ``push``, the command-line options are
  restored.

  .. code-block:: c++

    #pragma GCC diagnostic error "-Wuninitialized"
      foo(a);                       /* error is given for this one */
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wuninitialized"
      foo(b);                       /* no diagnostic for this one */
    #pragma GCC diagnostic pop
      foo(c);                       /* error is given for this one */
    #pragma GCC diagnostic pop
      foo(d);                       /* depends on command-line options */

``#pragma GCC diagnostic ignored_attributes``
  Similarly to :option:`-Wno-attributes=`, this pragma allows users to suppress
  warnings about unknown scoped attributes (in C++11 and C2X).  For example,
  ``#pragma GCC diagnostic ignored_attributes "vendor::attr"`` disables
  warning about the following declaration:

  .. code-block:: c++

    [[vendor::attr]] void f();

  whereas ``#pragma GCC diagnostic ignored_attributes "vendor::"`` prevents
  warning about both of these declarations:

  .. code-block:: c++

    [[vendor::safe]] void f();
    [[vendor::unsafe]] void f2();

GCC also offers a simple mechanism for printing messages during compilation.

:samp:`#pragma message {string}`

  .. index:: pragma, diagnostic

  Prints :samp:`{string}` as a compiler message on compilation.  The message
  is informational only, and is neither a compilation warning nor an
  error.  Newlines can be included in the string by using the :samp:`\\n`
  escape sequence.

  .. code-block:: c++

    #pragma message "Compiling " __FILE__ "..."

  :samp:`{string}` may be parenthesized, and is printed with location
  information.  For example,

  .. code-block:: fortran

    #define DO_PRAGMA(x) _Pragma (#x)
    #define TODO(x) DO_PRAGMA(message ("TODO - " #x))

    TODO(Remember to fix this)

  prints :samp:`/tmp/file.c:4: note: #pragma message:
  TODO - Remember to fix this`.

:samp:`#pragma GCC error {message}`

  .. index:: pragma, diagnostic

  Generates an error message.  This pragma *is* considered to
  indicate an error in the compilation, and it will be treated as such.

  Newlines can be included in the string by using the :samp:`\\n`
  escape sequence.  They will be displayed as newlines even if the
  :option:`-fmessage-length` option is set to zero.

  The error is only generated if the pragma is present in the code after
  pre-processing has been completed.  It does not matter however if the
  code containing the pragma is unreachable:

  .. code-block:: c++

    #if 0
    #pragma GCC error "this error is not seen"
    #endif
    void foo (void)
    {
      return;
    #pragma GCC error "this error is seen"
    }

:samp:`#pragma GCC warning {message}`

  .. index:: pragma, diagnostic

  This is just like :samp:`pragma GCC error` except that a warning
  message is issued instead of an error message.  Unless
  :option:`-Werror` is in effect, in which case this pragma will generate
  an error as well.

.. _visibility-pragmas:

Visibility Pragmas
^^^^^^^^^^^^^^^^^^

:samp:`#pragma GCC visibility push({visibility})`, ``#pragma GCC visibility pop``

  .. index:: pragma, visibility

  This pragma allows the user to set the visibility for multiple
  declarations without having to give each a visibility attribute
  (see :ref:`function-attributes`).

  In C++, :samp:`#pragma GCC visibility` affects only namespace-scope
  declarations.  Class members and template specializations are not
  affected; if you want to override the visibility for a particular
  member or instantiation, you must use an attribute.

.. _push-pop-macro-pragmas:

Push/Pop Macro Pragmas
^^^^^^^^^^^^^^^^^^^^^^

For compatibility with Microsoft Windows compilers, GCC supports
:samp:`#pragma push_macro({"macro_name"})`
and :samp:`#pragma pop_macro({"macro_name"})`.

:samp:`#pragma push_macro({"macro_name"})`

  .. index:: pragma, push_macro

  This pragma saves the value of the macro named as :samp:`{macro_name}` to
  the top of the stack for this macro.

:samp:`#pragma pop_macro({"macro_name"})`

  .. index:: pragma, pop_macro

  This pragma sets the value of the macro named as :samp:`{macro_name}` to
  the value on top of the stack for this macro. If the stack for
  :samp:`{macro_name}` is empty, the value of the macro remains unchanged.

  For example:

.. code-block:: c++

  #define X  1
  #pragma push_macro("X")
  #undef X
  #define X -1
  #pragma pop_macro("X")
  int x [X];

In this example, the definition of X as 1 is saved by ``#pragma
push_macro`` and restored by ``#pragma pop_macro``.

.. _function-specific-option-pragmas:

Function Specific Option Pragmas
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:samp:`#pragma GCC target ({string}, ...)`

  .. index:: pragma GCC target

  This pragma allows you to set target-specific options for functions
  defined later in the source file.  One or more strings can be
  specified.  Each function that is defined after this point is treated
  as if it had been declared with one  ``target(``:samp:`{string}` ``)``
  attribute for each :samp:`{string}` argument.  The parentheses around
  the strings in the pragma are optional.  See :ref:`function-attributes`,
  for more information about the ``target`` attribute and the attribute
  syntax.

  The ``#pragma GCC target`` pragma is presently implemented for
  x86, ARM, AArch64, PowerPC, S/390, and Nios II targets only.

:samp:`#pragma GCC optimize ({string}, ...)`

  .. index:: pragma GCC optimize

  This pragma allows you to set global optimization options for functions
  defined later in the source file.  One or more strings can be
  specified.  Each function that is defined after this point is treated
  as if it had been declared with one  ``optimize(``:samp:`{string}` ``)``
  attribute for each :samp:`{string}` argument.  The parentheses around
  the strings in the pragma are optional.  See :ref:`function-attributes`,
  for more information about the ``optimize`` attribute and the attribute
  syntax.

``#pragma GCC push_options`` ``#pragma GCC pop_options``

  .. index:: pragma GCC push_options, pragma GCC pop_options

  These pragmas maintain a stack of the current target and optimization
  options.  It is intended for include files where you temporarily want
  to switch to using a different :samp:`#pragma GCC target` or
  :samp:`#pragma GCC optimize` and then to pop back to the previous
  options.

``#pragma GCC reset_options``

  .. index:: pragma GCC reset_options

  This pragma clears the current ``#pragma GCC target`` and
  ``#pragma GCC optimize`` to use the default switches as specified
  on the command line.

.. _loop-specific-pragmas:

Loop-Specific Pragmas
^^^^^^^^^^^^^^^^^^^^^

``#pragma GCC ivdep``

  .. index:: pragma GCC ivdep

  With this pragma, the programmer asserts that there are no loop-carried
  dependencies which would prevent consecutive iterations of
  the following loop from executing concurrently with SIMD
  (single instruction multiple data) instructions.

  For example, the compiler can only unconditionally vectorize the following
  loop with the pragma:

  .. code-block:: c++

    void foo (int n, int *a, int *b, int *c)
    {
      int i, j;
    #pragma GCC ivdep
      for (i = 0; i < n; ++i)
        a[i] = b[i] + c[i];
    }

  In this example, using the ``restrict`` qualifier had the same
  effect. In the following example, that would not be possible. Assume
  k < -m or k >= m. Only with the pragma, the compiler knows
  that it can unconditionally vectorize the following loop:

  .. code-block:: c++

    void ignore_vec_dep (int *a, int k, int c, int m)
    {
    #pragma GCC ivdep
      for (int i = 0; i < m; i++)
        a[i] = a[i + k] * c;
    }

:samp:`#pragma GCC unroll {n}`

  .. index:: pragma GCC unroll n

  You can use this pragma to control how many times a loop should be unrolled.
  It must be placed immediately before a ``for``, ``while`` or ``do``
  loop or a ``#pragma GCC ivdep``, and applies only to the loop that follows.
  :samp:`{n}` is an integer constant expression specifying the unrolling factor.
  The values of 0 and 1 block any unrolling of the loop.
