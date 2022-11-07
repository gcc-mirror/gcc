..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: dialect options, language dialect options, options, dialect

.. _c-dialect-options:

Options Controlling C Dialect
*****************************

The following options control the dialect of C (or languages derived
from C, such as C++, Objective-C and Objective-C++) that the compiler
accepts:

.. index:: ANSI support, ISO support

.. option:: -ansi

  In C mode, this is equivalent to :option:`-std=c90`. In C++ mode, it is
  equivalent to :option:`-std=c++98`.

  This turns off certain features of GCC that are incompatible with ISO
  C90 (when compiling C code), or of standard C++ (when compiling C++ code),
  such as the ``asm`` and ``typeof`` keywords, and
  predefined macros such as ``unix`` and ``vax`` that identify the
  type of system you are using.  It also enables the undesirable and
  rarely used ISO trigraph feature.  For the C compiler,
  it disables recognition of C++ style :samp:`//` comments as well as
  the ``inline`` keyword.

  The alternate keywords ``__asm__``, ``__extension__``,
  ``__inline__`` and ``__typeof__`` continue to work despite
  :option:`-ansi`.  You would not want to use them in an ISO C program, of
  course, but it is useful to put them in header files that might be included
  in compilations done with :option:`-ansi`.  Alternate predefined macros
  such as ``__unix__`` and ``__vax__`` are also available, with or
  without :option:`-ansi`.

  The :option:`-ansi` option does not cause non-ISO programs to be
  rejected gratuitously.  For that, :option:`-Wpedantic` is required in
  addition to :option:`-ansi`.  See :ref:`warning-options`.

  The macro ``__STRICT_ANSI__`` is predefined when the :option:`-ansi`
  option is used.  Some header files may notice this macro and refrain
  from declaring certain functions or defining certain macros that the
  ISO standard doesn't call for; this is to avoid interfering with any
  programs that might use these names for other things.

  Functions that are normally built in but do not have semantics
  defined by ISO C (such as ``alloca`` and ``ffs``) are not built-in
  functions when :option:`-ansi` is used.  See :ref:`other-builtins`, for details of the functions
  affected.

.. option:: -std=

  Determine the language standard. See :ref:`standards`, for details of these standard versions.  This option
  is currently only supported when compiling C or C++.

  The compiler can accept several base standards, such as :samp:`c90` or
  :samp:`c++98`, and GNU dialects of those standards, such as
  :samp:`gnu90` or :samp:`gnu++98`.  When a base standard is specified, the
  compiler accepts all programs following that standard plus those
  using GNU extensions that do not contradict it.  For example,
  :option:`-std=c90` turns off certain features of GCC that are
  incompatible with ISO C90, such as the ``asm`` and ``typeof``
  keywords, but not other GNU extensions that do not have a meaning in
  ISO C90, such as omitting the middle term of a ``?:``
  expression. On the other hand, when a GNU dialect of a standard is
  specified, all features supported by the compiler are enabled, even when
  those features change the meaning of the base standard.  As a result, some
  strict-conforming programs may be rejected.  The particular standard
  is used by :option:`-Wpedantic` to identify which features are GNU
  extensions given that version of the standard. For example
  :option:`-std=gnu90 -Wpedantic` warns about C++ style :samp:`//`
  comments, while :option:`-std=gnu99 -Wpedantic` does not.

  A value for this option must be provided; possible values are

  :samp:`c90` :samp:`c89` :samp:`iso9899:1990`
    Support all ISO C90 programs (certain GNU extensions that conflict
    with ISO C90 are disabled). Same as :option:`-ansi` for C code.

  :samp:`iso9899:199409`
    ISO C90 as modified in amendment 1.

  :samp:`c99` :samp:`c9x` :samp:`iso9899:1999` :samp:`iso9899:199x`
    ISO C99.  This standard is substantially completely supported, modulo
    bugs and floating-point issues
    (mainly but not entirely relating to optional C99 features from
    Annexes F and G).  See
    https://gcc.gnu.org/c99status.html for more information.  The
    names :samp:`c9x` and :samp:`iso9899:199x` are deprecated.

  :samp:`c11` :samp:`c1x` :samp:`iso9899:2011`
    ISO C11, the 2011 revision of the ISO C standard.  This standard is
    substantially completely supported, modulo bugs, floating-point issues
    (mainly but not entirely relating to optional C11 features from
    Annexes F and G) and the optional Annexes K (Bounds-checking
    interfaces) and L (Analyzability).  The name :samp:`c1x` is deprecated.

  :samp:`c17` :samp:`c18` :samp:`iso9899:2017` :samp:`iso9899:2018`
    ISO C17, the 2017 revision of the ISO C standard
    (published in 2018).  This standard is
    same as C11 except for corrections of defects (all of which are also
    applied with :option:`-std=c11`) and a new value of
    ``__STDC_VERSION__``, and so is supported to the same extent as C11.

  :samp:`c2x`
    The next version of the ISO C standard, still under development.  The
    support for this version is experimental and incomplete.

  :samp:`gnu90` :samp:`gnu89`
    GNU dialect of ISO C90 (including some C99 features).

  :samp:`gnu99` :samp:`gnu9x`
    GNU dialect of ISO C99.  The name :samp:`gnu9x` is deprecated.

  :samp:`gnu11` :samp:`gnu1x`
    GNU dialect of ISO C11.
    The name :samp:`gnu1x` is deprecated.

  :samp:`gnu17` :samp:`gnu18`
    GNU dialect of ISO C17.  This is the default for C code.

  :samp:`gnu2x`
    The next version of the ISO C standard, still under development, plus
    GNU extensions.  The support for this version is experimental and
    incomplete.

  :samp:`c++98` :samp:`c++03`
    The 1998 ISO C++ standard plus the 2003 technical corrigendum and some
    additional defect reports. Same as :option:`-ansi` for C++ code.

  :samp:`gnu++98` :samp:`gnu++03`
    GNU dialect of :option:`-std=c++98`.

  :samp:`c++11` :samp:`c++0x`
    The 2011 ISO C++ standard plus amendments.
    The name :samp:`c++0x` is deprecated.

  :samp:`gnu++11` :samp:`gnu++0x`
    GNU dialect of :option:`-std=c++11`.
    The name :samp:`gnu++0x` is deprecated.

  :samp:`c++14` :samp:`c++1y`
    The 2014 ISO C++ standard plus amendments.
    The name :samp:`c++1y` is deprecated.

  :samp:`gnu++14` :samp:`gnu++1y`
    GNU dialect of :option:`-std=c++14`.
    The name :samp:`gnu++1y` is deprecated.

  :samp:`c++17` :samp:`c++1z`
    The 2017 ISO C++ standard plus amendments.
    The name :samp:`c++1z` is deprecated.

  :samp:`gnu++17` :samp:`gnu++1z`
    GNU dialect of :option:`-std=c++17`.
    This is the default for C++ code.
    The name :samp:`gnu++1z` is deprecated.

  :samp:`c++20` :samp:`c++2a`
    The 2020 ISO C++ standard plus amendments.
    Support is experimental, and could change in incompatible ways in
    future releases.
    The name :samp:`c++2a` is deprecated.

  :samp:`gnu++20` :samp:`gnu++2a`
    GNU dialect of :option:`-std=c++20`.
    Support is experimental, and could change in incompatible ways in
    future releases.
    The name :samp:`gnu++2a` is deprecated.

  :samp:`c++2b` :samp:`c++23`
    The next revision of the ISO C++ standard, planned for
    2023.  Support is highly experimental, and will almost certainly
    change in incompatible ways in future releases.

  :samp:`gnu++2b` :samp:`gnu++23`
    GNU dialect of :option:`-std=c++2b`.  Support is highly experimental,
    and will almost certainly change in incompatible ways in future
    releases.

.. option:: -aux-info {filename}

  Output to the given filename prototyped declarations for all functions
  declared and/or defined in a translation unit, including those in header
  files.  This option is silently ignored in any language other than C.

  Besides declarations, the file indicates, in comments, the origin of
  each declaration (source file and line), whether the declaration was
  implicit, prototyped or unprototyped (:samp:`I`, :samp:`N` for new or
  :samp:`O` for old, respectively, in the first character after the line
  number and the colon), and whether it came from a declaration or a
  definition (:samp:`C` or :samp:`F`, respectively, in the following
  character).  In the case of function definitions, a K&R-style list of
  arguments followed by their declarations is also provided, inside
  comments, after the declaration.

.. option:: -fno-asm

  Do not recognize ``asm``, ``inline`` or ``typeof`` as a
  keyword, so that code can use these words as identifiers.  You can use
  the keywords ``__asm__``, ``__inline__`` and ``__typeof__``
  instead.  In C, :option:`-ansi` implies :option:`-fno-asm`.

  In C++, ``inline`` is a standard keyword and is not affected by
  this switch.  You may want to use the :option:`-fno-gnu-keywords` flag
  instead, which disables ``typeof`` but not ``asm`` and
  ``inline``.  In C99 mode (:option:`-std=c99` or :option:`-std=gnu99`),
  this switch only affects the ``asm`` and ``typeof`` keywords,
  since ``inline`` is a standard keyword in ISO C99.  In C2X mode
  (:option:`-std=c2x` or :option:`-std=gnu2x`), this switch only affects
  the ``asm`` keyword, since ``typeof`` is a standard keyword in
  ISO C2X.

.. option:: -fasm

  Default setting; overrides :option:`-fno-asm`.

.. index:: built-in functions

.. option:: -fno-builtin, -fno-builtin-function

  Don't recognize built-in functions that do not begin with
  :samp:`__builtin_` as prefix.  See :ref:`other-builtins`, for details of the functions affected,
  including those which are not built-in functions when :option:`-ansi` or
  :option:`-std` options for strict ISO C conformance are used because they
  do not have an ISO standard meaning.

  GCC normally generates special code to handle certain built-in functions
  more efficiently; for instance, calls to ``alloca`` may become single
  instructions which adjust the stack directly, and calls to ``memcpy``
  may become inline copy loops.  The resulting code is often both smaller
  and faster, but since the function calls no longer appear as such, you
  cannot set a breakpoint on those calls, nor can you change the behavior
  of the functions by linking with a different library.  In addition,
  when a function is recognized as a built-in function, GCC may use
  information about that function to warn about problems with calls to
  that function, or to generate more efficient code, even if the
  resulting code still contains calls to that function.  For example,
  warnings are given with :option:`-Wformat` for bad calls to
  ``printf`` when ``printf`` is built in and ``strlen`` is
  known not to modify global memory.

  With the :option:`-fno-builtin-function` option
  only the built-in function :samp:`{function}` is
  disabled.  :samp:`{function}` must not begin with :samp:`__builtin_`.  If a
  function is named that is not built-in in this version of GCC, this
  option is ignored.  There is no corresponding
  :option:`-fbuiltin-function` option; if you wish to enable
  built-in functions selectively when using :option:`-fno-builtin` or
  :option:`-ffreestanding`, you may define macros such as:

  .. code-block:: c++

    #define abs(n)          __builtin_abs ((n))
    #define strcpy(d, s)    __builtin_strcpy ((d), (s))

.. option:: -fbuiltin

  Default setting; overrides :option:`-fno-builtin`.

.. option:: -fcond-mismatch

  Allow conditional expressions with mismatched types in the second and
  third arguments.  The value of such an expression is void.  This option
  is not supported for C++.

.. index:: hosted environment

.. option:: -ffreestanding

  Assert that compilation targets a freestanding environment.  This
  implies :option:`-fno-builtin`.  A freestanding environment
  is one in which the standard library may not exist, and program startup may
  not necessarily be at ``main``.  The most obvious example is an OS kernel.
  This is equivalent to :option:`-fno-hosted`.

  See :ref:`standards`, for details of
  freestanding and hosted environments.

.. option:: -fgimple

  Enable parsing of function definitions marked with ``__GIMPLE``.
  This is an experimental feature that allows unit testing of GIMPLE
  passes.

.. option:: -fgnu-tm

  When the option :option:`-fgnu-tm` is specified, the compiler
  generates code for the Linux variant of Intel's current Transactional
  Memory ABI specification document (Revision 1.1, May 6 2009).  This is
  an experimental feature whose interface may change in future versions
  of GCC, as the official specification changes.  Please note that not
  all architectures are supported for this feature.

  For more information on GCC's support for transactional memory,
  see :ref:`libitm:enabling-libitm`.

  Note that the transactional memory feature is not supported with
  non-call exceptions (:option:`-fnon-call-exceptions`).

.. option:: -fgnu89-inline

  The option :option:`-fgnu89-inline` tells GCC to use the traditional
  GNU semantics for ``inline`` functions when in C99 mode.
  See :ref:`inline`.
  Using this option is roughly equivalent to adding the
  :fn-attr:`gnu_inline` function attribute to all inline functions
  (see :ref:`function-attributes`).

  The option :option:`-fno-gnu89-inline` explicitly tells GCC to use the
  C99 semantics for ``inline`` when in C99 or gnu99 mode (i.e., it
  specifies the default behavior).
  This option is not supported in :option:`-std=c90` or
  :option:`-std=gnu90` mode.

  The preprocessor macros ``__GNUC_GNU_INLINE__`` and
  ``__GNUC_STDC_INLINE__`` may be used to check which semantics are
  in effect for ``inline`` functions.  See :ref:`cpp:common-predefined-macros`.

.. index:: hosted environment

.. option:: -fhosted

  Assert that compilation targets a hosted environment.  This implies
  :option:`-fbuiltin`.  A hosted environment is one in which the
  entire standard library is available, and in which ``main`` has a return
  type of ``int``.  Examples are nearly everything except a kernel.
  This is equivalent to :option:`-fno-freestanding`.

.. option:: -flax-vector-conversions

  Allow implicit conversions between vectors with differing numbers of
  elements and/or incompatible element types.  This option should not be
  used for new code.

.. option:: -fms-extensions

  Accept some non-standard constructs used in Microsoft header files.

  In C++ code, this allows member names in structures to be similar
  to previous types declarations.

  .. code-block:: c++

    typedef int UOW;
    struct ABC {
      UOW UOW;
    };

  Some cases of unnamed fields in structures and unions are only
  accepted with this option.  See :ref:`unnamed-fields`, for details.

  Note that this option is off for all targets except for x86
  targets using ms-abi.

.. index:: Offloading targets, OpenACC offloading targets, OpenMP offloading targets

.. option:: -foffload=disable

  Specify for which OpenMP and OpenACC offload targets code should be generated.
  The default behavior, equivalent to :option:`-foffload=default`, is to generate
  code for all supported offload targets.  The :option:`-foffload=disable` form
  generates code only for the host fallback, while
  :option:`-foffload=target-list` generates code only for the specified
  comma-separated list of offload targets.

  Offload targets are specified in GCC's internal target-triplet format. You can
  run the compiler with :option:`-v` to show the list of configured offload targets
  under ``OFFLOAD_TARGET_NAMES``.

.. index:: Offloading options, OpenACC offloading options, OpenMP offloading options

.. option:: -foffload-options={options}

  With :option:`-foffload-options=options`, GCC passes the specified
  :samp:`{options}` to the compilers for all enabled offloading targets.  You can
  specify options that apply only to a specific target or targets by using
  the :option:`-foffload-options=target-list=options` form.  The
  :samp:`{target-list}` is a comma-separated list in the same format as for the
  :option:`-foffload=` option.

  Typical command lines are

  :option:`-foffload-options=-lgfortran` :option:`-foffload-options=-lm`
  :option:`-foffload-options="-lgfortran-lm` :option:`-lm"` :option:`-foffload-options=nvptx-none=-latomic`
  :option:`-foffload-options=amdgcn-amdhsa=-march=gfx906` :option:`-foffload-options=-lm`

.. index:: OpenACC accelerator programming

.. option:: -fopenacc

  Enable handling of OpenACC directives ``#pragma acc`` in C/C++ and
  ``!$acc`` in Fortran.  When :option:`-fopenacc` is specified, the
  compiler generates accelerated code according to the OpenACC Application
  Programming Interface v2.6 https://www.openacc.org.  This option
  implies :option:`-pthread`, and thus is only supported on targets that
  have support for :option:`-pthread`.

.. index:: OpenACC accelerator programming

.. option:: -fopenacc-dim={geom}

  Specify default compute dimensions for parallel offload regions that do
  not explicitly specify.  The :samp:`{geom}` value is a triple of
  ':'-separated sizes, in order 'gang', 'worker' and, 'vector'.  A size
  can be omitted, to use a target-specific default value.

.. index:: OpenMP parallel

.. option:: -fopenmp

  Enable handling of OpenMP directives ``#pragma omp`` in C/C++,
  ``[[omp::directive(...)]]`` and ``[[omp::sequence(...)]]`` in C++ and
  ``!$omp`` in Fortran.  When :option:`-fopenmp` is specified, the
  compiler generates parallel code according to the OpenMP Application
  Program Interface v4.5 https://www.openmp.org.  This option
  implies :option:`-pthread`, and thus is only supported on targets that
  have support for :option:`-pthread`. :option:`-fopenmp` implies
  :option:`-fopenmp-simd`.

.. index:: OpenMP SIMD, SIMD

.. option:: -fopenmp-simd

  Enable handling of OpenMP's :gcc-attr:`simd`, ``declare simd``,
  ``declare reduction``, ``assume``, ``ordered``, ``scan``,
  ``loop`` directives and combined or composite directives with
  :gcc-attr:`simd` as constituent with ``#pragma omp`` in C/C++,
  ``[[omp::directive(...)]]`` and ``[[omp::sequence(...)]]`` in C++
  and ``!$omp`` in Fortran.  Other OpenMP directives are ignored.

.. option:: -fpermitted-flt-eval-methods={style}

  ISO/IEC TS 18661-3 defines new permissible values for
  ``FLT_EVAL_METHOD`` that indicate that operations and constants with
  a semantic type that is an interchange or extended format should be
  evaluated to the precision and range of that type.  These new values are
  a superset of those permitted under C99/C11, which does not specify the
  meaning of other positive values of ``FLT_EVAL_METHOD``.  As such, code
  conforming to C11 may not have been written expecting the possibility of
  the new values.

  :option:`-fpermitted-flt-eval-methods` specifies whether the compiler
  should allow only the values of ``FLT_EVAL_METHOD`` specified in C99/C11,
  or the extended set of values specified in ISO/IEC TS 18661-3.

  :samp:`{style}` is either ``c11`` or ``ts-18661-3`` as appropriate.

  The default when in a standards compliant mode (:option:`-std=c11` or similar)
  is :option:`-fpermitted-flt-eval-methods=c11`.  The default when in a GNU
  dialect (:option:`-std=gnu11` or similar) is
  :option:`-fpermitted-flt-eval-methods=ts-18661-3`.

.. option:: -fplan9-extensions

  Accept some non-standard constructs used in Plan 9 code.

  This enables :option:`-fms-extensions`, permits passing pointers to
  structures with anonymous fields to functions that expect pointers to
  elements of the type of the field, and permits referring to anonymous
  fields declared using a typedef.  See :ref:`unnamed-fields`, for details.  This is only
  supported for C, not C++.

.. option:: -fsigned-bitfields, -funsigned-bitfields, -fno-signed-bitfields, -fno-unsigned-bitfields

  These options control whether a bit-field is signed or unsigned, when the
  declaration does not use either ``signed`` or ``unsigned``.  By
  default, such a bit-field is signed, because this is consistent: the
  basic integer types such as ``int`` are signed types.

.. option:: -fsigned-char

  Let the type ``char`` be signed, like ``signed char``.

  Note that this is equivalent to :option:`-fno-unsigned-char`, which is
  the negative form of :option:`-funsigned-char`.  Likewise, the option
  :option:`-fno-signed-char` is equivalent to :option:`-funsigned-char`.

.. option:: -funsigned-char

  Let the type ``char`` be unsigned, like ``unsigned char``.

  Each kind of machine has a default for what ``char`` should
  be.  It is either like ``unsigned char`` by default or like
  ``signed char`` by default.

  Ideally, a portable program should always use ``signed char`` or
  ``unsigned char`` when it depends on the signedness of an object.
  But many programs have been written to use plain ``char`` and
  expect it to be signed, or expect it to be unsigned, depending on the
  machines they were written for.  This option, and its inverse, let you
  make such a program work with the opposite default.

  The type ``char`` is always a distinct type from each of
  ``signed char`` or ``unsigned char``, even though its behavior
  is always just like one of those two.

.. option:: -fstrict-flex-arrays

  Control when to treat the trailing array of a structure as a flexible array
  member for the purpose of accessing the elements of such an array.
  The positive form is equivalent to :option:`-fstrict-flex-arrays=3`, which is the
  strictest.  A trailing array is treated as a flexible array member only when it
  is declared as a flexible array member per C99 standard onwards.
  The negative form is equivalent to :option:`-fstrict-flex-arrays=0`, which is the
  least strict.  All trailing arrays of structures are treated as flexible array
  members.

.. option:: -fno-strict-flex-arrays

  Default setting; overrides :option:`-fstrict-flex-arrays`.

.. index:: fstrict-flex-arrays=level

.. option:: -fstrict-flex-arrays={level}

  Control when to treat the trailing array of a structure as a flexible array
  member for the purpose of accessing the elements of such an array.  The value
  of :samp:`{level}` controls the level of strictness.

  The possible values of :samp:`{level}` are the same as for the
  ``strict_flex_array`` attribute (see :ref:`variable-attributes`).

  You can control this behavior for a specific trailing array field of a
  structure by using the variable attribute ``strict_flex_array`` attribute
  (see :ref:`variable-attributes`).

.. option:: -fsso-struct={endianness}

  Set the default scalar storage order of structures and unions to the
  specified endianness.  The accepted values are :samp:`big-endian`,
  :samp:`little-endian` and :samp:`native` for the native endianness of
  the target (the default).  This option is not supported for C++.

  .. warning::

    The :option:`-fsso-struct` switch causes GCC to generate
    code that is not binary compatible with code generated without it if the
    specified endianness is not the native endianness of the target.