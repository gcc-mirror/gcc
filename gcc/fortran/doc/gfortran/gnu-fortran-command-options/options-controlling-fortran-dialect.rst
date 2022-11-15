..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: dialect options, language, dialect options, options, dialect

.. _fortran-dialect-options:

Options controlling Fortran dialect
***********************************

The following options control the details of the Fortran dialect
accepted by the compiler:

.. index:: ffree-form, ffixed-form, options, Fortran dialect, file format, free, file format, fixed

.. option:: -ffree-form, -ffixed-form

  Specify the layout used by the source file.  The free form layout
  was introduced in Fortran 90.  Fixed form was traditionally used in
  older Fortran programs.  When neither option is specified, the source
  form is determined by the file extension.

.. index:: fall-intrinsics

.. option:: -fall-intrinsics

  This option causes all intrinsic procedures (including the GNU-specific
  extensions) to be accepted.  This can be useful with :option:`-std=` to
  force standard-compliance but get access to the full range of intrinsics
  available with :command:`gfortran`.  As a consequence, :option:`-Wintrinsics-std`
  will be ignored and no user-defined procedure with the same name as any
  intrinsic will be called except when it is explicitly declared ``EXTERNAL``.

.. index:: fallow-argument-mismatch

.. option:: -fallow-argument-mismatch

  Some code contains calls to external procedures with mismatches
  between the calls and the procedure definition, or with mismatches
  between different calls. Such code is non-conforming, and will usually
  be flagged with an error.  This options degrades the error to a
  warning, which can only be disabled by disabling all warnings via
  :option:`-w`.  Only a single occurrence per argument is flagged by this
  warning.  :option:`-fallow-argument-mismatch` is implied by
  :option:`-std=legacy`.

  Using this option is *strongly* discouraged.  It is possible to
  provide standard-conforming code which allows different types of
  arguments by using an explicit interface and ``TYPE(*)``.

.. index:: allow-invalid-boz

.. option:: -fallow-invalid-boz

  A BOZ literal constant can occur in a limited number of contexts in
  standard conforming Fortran.  This option degrades an error condition
  to a warning, and allows a BOZ literal constant to appear where the
  Fortran standard would otherwise prohibit its use.

.. index:: fd-lines-as-code, fd-lines-as-comments

.. option:: -fd-lines-as-code, -fd-lines-as-comments

  Enable special treatment for lines beginning with ``d`` or ``D``
  in fixed form sources.  If the :option:`-fd-lines-as-code` option is
  given they are treated as if the first column contained a blank.  If the
  :option:`-fd-lines-as-comments` option is given, they are treated as
  comment lines.

.. index:: fdec

.. option:: -fdec

  DEC compatibility mode. Enables extensions and other features that mimic
  the default behavior of older compilers (such as DEC).
  These features are non-standard and should be avoided at all costs.
  For details on GNU Fortran's implementation of these extensions see the
  full documentation.

  Other flags enabled by this switch are:

  :option:`-fdollar-ok` :option:`-fcray-pointer` :option:`-fdec-char-conversions` |gol|
  :option:`-fdec-structure` :option:`-fdec-intrinsic-ints` :option:`-fdec-static` |gol|
  :option:`-fdec-math` :option:`-fdec-include` :option:`-fdec-blank-format-item` |gol|
  :option:`-fdec-format-defaults`

  If :option:`-fd-lines-as-code` / :option:`-fd-lines-as-comments` are unset, then
  :option:`-fdec` also sets :option:`-fd-lines-as-comments`.

.. index:: fdec-char-conversions

.. option:: -fdec-char-conversions

  Enable the use of character literals in assignments and ``DATA`` statements
  for non-character variables.

.. index:: fdec-structure

.. option:: -fdec-structure

  Enable DEC ``STRUCTURE`` and ``RECORD`` as well as ``UNION``,
  ``MAP``, and dot ('.') as a member separator (in addition to '%'). This is
  provided for compatibility only; Fortran 90 derived types should be used
  instead where possible.

.. index:: fdec-intrinsic-ints

.. option:: -fdec-intrinsic-ints

  Enable B/I/J/K kind variants of existing integer functions (e.g. BIAND, IIAND,
  JIAND, etc...). For a complete list of intrinsics see the full documentation.

.. index:: fdec-math

.. option:: -fdec-math

  Enable legacy math intrinsics such as COTAN and degree-valued trigonometric
  functions (e.g. TAND, ATAND, etc...) for compatability with older code.

.. index:: fdec-static

.. option:: -fdec-static

  Enable DEC-style STATIC and AUTOMATIC attributes to explicitly specify
  the storage of variables and other objects.

.. index:: fdec-include

.. option:: -fdec-include

  Enable parsing of INCLUDE as a statement in addition to parsing it as
  INCLUDE line.  When parsed as INCLUDE statement, INCLUDE does not have to
  be on a single line and can use line continuations.

.. index:: fdec-format-defaults

.. option:: -fdec-format-defaults

  Enable format specifiers F, G and I to be used without width specifiers,
  default widths will be used instead.

.. index:: fdec-blank-format-item

.. option:: -fdec-blank-format-item

  Enable a blank format item at the end of a format specification i.e. nothing
  following the final comma.

.. index:: fdollar-ok, $, symbol names, character set

.. option:: -fdollar-ok

  Allow :samp:`$` as a valid non-first character in a symbol name. Symbols
  that start with :samp:`$` are rejected since it is unclear which rules to
  apply to implicit typing as different vendors implement different rules.
  Using :samp:`$` in ``IMPLICIT`` statements is also rejected.

.. index:: backslash, backslash, escape characters

.. option:: -fbackslash

  Change the interpretation of backslashes in string literals from a single
  backslash character to 'C-style' escape characters. The following
  combinations are expanded ``\a``, ``\b``, ``\f``, ``\n``,
  ``\r``, ``\t``, ``\v``, ``\\``, and ``\0`` to the ASCII
  characters alert, backspace, form feed, newline, carriage return,
  horizontal tab, vertical tab, backslash, and NUL, respectively.
  Additionally,  ``\x``:samp:`{nn}`,  ``\u``:samp:`{nnnn}` and
  ``\U``:samp:`{nnnnnnnn}` (where each :samp:`{n}` is a hexadecimal digit) are
  translated into the Unicode characters corresponding to the specified code
  points. All other combinations of a character preceded by \ are
  unexpanded.

.. index:: fmodule-private, module entities, private

.. option:: -fmodule-private

  Set the default accessibility of module entities to ``PRIVATE``.
  Use-associated entities will not be accessible unless they are explicitly
  declared as ``PUBLIC``.

.. index:: ffixed-line-length-n, file format, fixed

.. option:: -ffixed-line-length-n
            -ffixed-line-length-none
            -ffixed-line-length-0

  Set column after which characters are ignored in typical fixed-form
  lines in the source file, and, unless ``-fno-pad-source``, through which
  spaces are assumed (as if padded to that length) after the ends of short
  fixed-form lines.

  Popular values for :samp:`{n}` include 72 (the
  standard and the default), 80 (card image), and 132 (corresponding
  to 'extended-source' options in some popular compilers).
  :samp:`{n}` may also be :samp:`none`, meaning that the entire line is meaningful
  and that continued character constants never have implicit spaces appended
  to them to fill out the line.
  :option:`-ffixed-line-length-0` means the same thing as
  :option:`-ffixed-line-length-none`.

.. index:: fpad-source

.. option:: -fno-pad-source

  By default fixed-form lines have spaces assumed (as if padded to that length)
  after the ends of short fixed-form lines.  This is not done either if
  :option:`-ffixed-line-length-0`, :option:`-ffixed-line-length-none` or
  if :option:`-fno-pad-source` option is used.  With any of those options
  continued character constants never have implicit spaces appended
  to them to fill out the line.

.. index:: ffree-line-length-n, file format, free

.. option:: -ffree-line-length-n
            -ffree-line-length-none
            -ffree-line-length-0

  Set column after which characters are ignored in typical free-form
  lines in the source file. The default value is 132.
  :samp:`{n}` may be :samp:`none`, meaning that the entire line is meaningful.
  :option:`-ffree-line-length-0` means the same thing as
  :option:`-ffree-line-length-none`.

.. index:: fmax-identifier-length=n

.. option:: -fmax-identifier-length={n}

  Specify the maximum allowed identifier length. Typical values are
  31 (Fortran 95) and 63 (Fortran 2003 and later).

.. index:: fimplicit-none

.. option:: -fimplicit-none

  Specify that no implicit typing is allowed, unless overridden by explicit
  ``IMPLICIT`` statements.  This is the equivalent of adding
  ``implicit none`` to the start of every procedure.

.. index:: fcray-pointer

.. option:: -fcray-pointer

  Enable the Cray pointer extension, which provides C-like pointer
  functionality.

.. index:: fopenacc, OpenACC

.. option:: -fopenacc

  Enable the OpenACC extensions.  This includes OpenACC ``!$acc``
  directives in free form and ``c$acc``, ``*$acc`` and
  ``!$acc`` directives in fixed form, ``!$`` conditional
  compilation sentinels in free form and ``c$``, ``*$`` and
  ``!$`` sentinels in fixed form, and when linking arranges for the
  OpenACC runtime library to be linked in.

.. index:: fopenmp, OpenMP

.. option:: -fopenmp

  Enable the OpenMP extensions.  This includes OpenMP ``!$omp`` directives
  in free form
  and ``c$omp``, ``*$omp`` and ``!$omp`` directives in fixed form,
  ``!$`` conditional compilation sentinels in free form
  and ``c$``, ``*$`` and ``!$`` sentinels in fixed form,
  and when linking arranges for the OpenMP runtime library to be linked
  in.  The option :option:`-fopenmp` implies :option:`-frecursive`.

.. index:: frange-check

.. option:: -fno-range-check

  Disable range checking on results of simplification of constant
  expressions during compilation.  For example, GNU Fortran will give
  an error at compile time when simplifying ``a = 1. / 0``.
  With this option, no error will be given and ``a`` will be assigned
  the value ``+Infinity``.  If an expression evaluates to a value
  outside of the relevant range of [ ``-HUGE()`` : ``HUGE()`` ],
  then the expression will be replaced by ``-Inf`` or ``+Inf``
  as appropriate.
  Similarly, ``DATA i/Z'FFFFFFFF'/`` will result in an integer overflow
  on most systems, but with :option:`-fno-range-check` the value will
  'wrap around' and ``i`` will be initialized to -1 instead.

.. index:: fdefault-integer-8

.. option:: -fdefault-integer-8

  Set the default integer and logical types to an 8 byte wide type.  This option
  also affects the kind of integer constants like ``42``. Unlike
  :option:`-finteger-4-integer-8`, it does not promote variables with explicit
  kind declaration.

.. index:: fdefault-real-8

.. option:: -fdefault-real-8

  Set the default real type to an 8 byte wide type.  This option also affects
  the kind of non-double real constants like ``1.0``.  This option promotes
  the default width of ``DOUBLE PRECISION`` and double real constants
  like ``1.d0`` to 16 bytes if possible.  If ``-fdefault-double-8``
  is given along with ``fdefault-real-8``, ``DOUBLE PRECISION``
  and double real constants are not promoted.  Unlike :option:`-freal-4-real-8`,
  ``fdefault-real-8`` does not promote variables with explicit kind
  declarations.

.. index:: fdefault-real-10

.. option:: -fdefault-real-10

  Set the default real type to an 10 byte wide type.  This option also affects
  the kind of non-double real constants like ``1.0``.  This option promotes
  the default width of ``DOUBLE PRECISION`` and double real constants
  like ``1.d0`` to 16 bytes if possible.  If ``-fdefault-double-8``
  is given along with ``fdefault-real-10``, ``DOUBLE PRECISION``
  and double real constants are not promoted.  Unlike :option:`-freal-4-real-10`,
  ``fdefault-real-10`` does not promote variables with explicit kind
  declarations.

.. index:: fdefault-real-16

.. option:: -fdefault-real-16

  Set the default real type to an 16 byte wide type.  This option also affects
  the kind of non-double real constants like ``1.0``.  This option promotes
  the default width of ``DOUBLE PRECISION`` and double real constants
  like ``1.d0`` to 16 bytes if possible.  If ``-fdefault-double-8``
  is given along with ``fdefault-real-16``, ``DOUBLE PRECISION``
  and double real constants are not promoted.  Unlike :option:`-freal-4-real-16`,
  ``fdefault-real-16`` does not promote variables with explicit kind
  declarations.

.. index:: fdefault-double-8

.. option:: -fdefault-double-8

  Set the ``DOUBLE PRECISION`` type and double real constants
  like ``1.d0`` to an 8 byte wide type.  Do nothing if this
  is already the default.  This option prevents :option:`-fdefault-real-8`,
  :option:`-fdefault-real-10`, and :option:`-fdefault-real-16`,
  from promoting ``DOUBLE PRECISION`` and double real constants like
  ``1.d0`` to 16 bytes.

.. index:: finteger-4-integer-8

.. option:: -finteger-4-integer-8

  Promote all ``INTEGER(KIND=4)`` entities to an ``INTEGER(KIND=8)``
  entities.  If ``KIND=8`` is unavailable, then an error will be issued.
  This option should be used with care and may not be suitable for your codes.
  Areas of possible concern include calls to external procedures,
  alignment in ``EQUIVALENCE`` and/or ``COMMON``, generic interfaces,
  BOZ literal constant conversion, and I/O.  Inspection of the intermediate
  representation of the translated Fortran code, produced by
  :option:`-fdump-tree-original`, is suggested.

.. index:: freal-4-real-8, freal-4-real-10, freal-4-real-16, freal-8-real-4, freal-8-real-10, freal-8-real-16, options, real kind type promotion

.. option:: -freal-4-real-8, -freal-4-real-10, -freal-4-real-16, -freal-8-real-4, -freal-8-real-10, -freal-8-real-16

  Promote all ``REAL(KIND=M)`` entities to ``REAL(KIND=N)`` entities.
  If ``REAL(KIND=N)`` is unavailable, then an error will be issued.
  The ``-freal-4-`` flags also affect the default real kind and the
  ``-freal-8-`` flags also the double-precision real kind.  All other
  real-kind types are unaffected by this option.  The promotion is also
  applied to real literal constants of default and double-precision kind
  and a specified kind number of 4 or 8, respectively.
  However, ``-fdefault-real-8``, ``-fdefault-real-10``,
  ``-fdefault-real-10``, and ``-fdefault-double-8`` take precedence
  for the default and double-precision real kinds, both for real literal
  constants and for declarations without a kind number.
  Note that for ``REAL(KIND=KIND(1.0))`` the literal may get promoted and
  then the result may get promoted again.
  These options should be used with care and may not be suitable for your
  codes.  Areas of possible concern include calls to external procedures,
  alignment in ``EQUIVALENCE`` and/or ``COMMON``, generic interfaces,
  BOZ literal constant conversion, and I/O and calls to intrinsic procedures
  when passing a value to the ``kind=`` dummy argument.  Inspection of the
  intermediate representation of the translated Fortran code, produced by
  :option:`-fdump-fortran-original` or :option:`-fdump-tree-original`, is suggested.

.. index:: std=std option

.. option:: -std={std}

  Specify the standard to which the program is expected to conform,
  which may be one of :samp:`f95`, :samp:`f2003`, :samp:`f2008`,
  :samp:`f2018`, :samp:`gnu`, or :samp:`legacy`.  The default value for
  :samp:`{std}` is :samp:`gnu`, which specifies a superset of the latest
  Fortran standard that includes all of the extensions supported by GNU
  Fortran, although warnings will be given for obsolete extensions not
  recommended for use in new code.  The :samp:`legacy` value is
  equivalent but without the warnings for obsolete extensions, and may
  be useful for old non-standard programs.  The :samp:`f95`,
  :samp:`f2003`, :samp:`f2008`, and :samp:`f2018` values specify strict
  conformance to the Fortran 95, Fortran 2003, Fortran 2008 and Fortran
  2018 standards, respectively; errors are given for all extensions
  beyond the relevant language standard, and warnings are given for the
  Fortran 77 features that are permitted but obsolescent in later
  standards. The deprecated option :samp:`-std=f2008ts` acts as an alias for
  :samp:`-std=f2018`. It is only present for backwards compatibility with
  earlier gfortran versions and should not be used any more.

.. index:: ftest-forall-temp

.. option:: -ftest-forall-temp

  Enhance test coverage by forcing most forall assignments to use temporary.
