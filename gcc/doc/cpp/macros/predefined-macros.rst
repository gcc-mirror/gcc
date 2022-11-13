..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: predefined macros

.. _predefined-macros:

Predefined Macros
*****************

Several object-like macros are predefined; you use them without
supplying their definitions.  They fall into three classes: standard,
common, and system-specific.

In C++, there is a fourth category, the named operators.  They act like
predefined macros, but you cannot undefine them.

.. toctree::
  :maxdepth: 2


.. index:: standard predefined macros.

.. _standard-predefined-macros:

Standard Predefined Macros
^^^^^^^^^^^^^^^^^^^^^^^^^^

The standard predefined macros are specified by the relevant
language standards, so they are available with all compilers that
implement those standards.  Older compilers may not provide all of
them.  Their names all start with double underscores.

.. c:macro:: __FILE__

  This macro expands to the name of the current input file, in the form of
  a C string constant.  This is the path by which the preprocessor opened
  the file, not the short name specified in :samp:`#include` or as the
  input file name argument.  For example,
  ``"/usr/local/include/myheader.h"`` is a possible expansion of this
  macro.

.. c:macro:: __LINE__

  This macro expands to the current input line number, in the form of a
  decimal integer constant.  While we call it a predefined macro, it's
  a pretty strange macro, since its 'definition' changes with each
  new line of source code.

  ``__FILE__`` and ``__LINE__`` are useful in generating an error
  message to report an inconsistency detected by the program; the message
  can state the source line at which the inconsistency was detected.  For
  example,

.. code-block:: c++

  fprintf (stderr, "Internal error: "
                   "negative string length "
                   "%d at %s, line %d.",
           length, __FILE__, __LINE__);

An :samp:`#include` directive changes the expansions of ``__FILE__``
and ``__LINE__`` to correspond to the included file.  At the end of
that file, when processing resumes on the input file that contained
the :samp:`#include` directive, the expansions of ``__FILE__`` and
``__LINE__`` revert to the values they had before the
:samp:`#include` (but ``__LINE__`` is then incremented by one as
processing moves to the line after the :samp:`#include`).

A :samp:`#line` directive changes ``__LINE__``, and may change
``__FILE__`` as well.  See :ref:`line-control`.

C99 introduced ``__func__``, and GCC has provided ``__FUNCTION__``
for a long time.  Both of these are strings containing the name of the
current function (there are slight semantic differences; see the GCC
manual).  Neither of them is a macro; the preprocessor does not know the
name of the current function.  They tend to be useful in conjunction
with ``__FILE__`` and ``__LINE__``, though.

.. c:macro:: __DATE__

  This macro expands to a string constant that describes the date on which
  the preprocessor is being run.  The string constant contains eleven
  characters and looks like ``"Feb 12 1996"``.  If the day of the
  month is less than 10, it is padded with a space on the left.

  If GCC cannot determine the current date, it will emit a warning message
  (once per compilation) and ``__DATE__`` will expand to
  ``"??? ?? ????"``.

.. c:macro:: __TIME__

  This macro expands to a string constant that describes the time at
  which the preprocessor is being run.  The string constant contains
  eight characters and looks like ``"23:59:01"``.

  If GCC cannot determine the current time, it will emit a warning message
  (once per compilation) and ``__TIME__`` will expand to
  ``"??:??:??"``.

.. c:macro:: __STDC__

  In normal operation, this macro expands to the constant 1, to signify
  that this compiler conforms to ISO Standard C.  If GNU CPP is used with
  a compiler other than GCC, this is not necessarily true; however, the
  preprocessor always conforms to the standard unless the
  :option:`-traditional-cpp` option is used.

  This macro is not defined if the :option:`-traditional-cpp` option is used.

  On some hosts, the system compiler uses a different convention, where
  ``__STDC__`` is normally 0, but is 1 if the user specifies strict
  conformance to the C Standard.  CPP follows the host convention when
  processing system header files, but when processing user files
  ``__STDC__`` is always 1.  This has been reported to cause problems;
  for instance, some versions of Solaris provide X Windows headers that
  expect ``__STDC__`` to be either undefined or 1.  See :ref:`invocation`.

.. c:macro:: __STDC_VERSION__

  This macro expands to the C Standard's version number, a long integer
  constant of the form ``yyyymmL`` where :samp:`{yyyy}` and
  :samp:`{mm}` are the year and month of the Standard version.  This signifies
  which version of the C Standard the compiler conforms to.  Like
  ``__STDC__``, this is not necessarily accurate for the entire
  implementation, unless GNU CPP is being used with GCC.

  The value ``199409L`` signifies the 1989 C standard as amended in
  1994, which is the current default; the value ``199901L`` signifies
  the 1999 revision of the C standard; the value ``201112L``
  signifies the 2011 revision of the C standard; the value
  ``201710L`` signifies the 2017 revision of the C standard (which is
  otherwise identical to the 2011 version apart from correction of
  defects).  An unspecified value larger than ``201710L`` is used for
  the experimental :option:`-std=c2x` and :option:`-std=gnu2x` modes.

  This macro is not defined if the :option:`-traditional-cpp` option is
  used, nor when compiling C++ or Objective-C.

.. c:macro:: __STDC_HOSTED__

  This macro is defined, with value 1, if the compiler's target is a
  :dfn:`hosted environment`.  A hosted environment has the complete
  facilities of the standard C library available.

.. c:macro:: __cplusplus

  This macro is defined when the C++ compiler is in use.  You can use
  ``__cplusplus`` to test whether a header is compiled by a C compiler
  or a C++ compiler.  This macro is similar to ``__STDC_VERSION__``, in
  that it expands to a version number.  Depending on the language standard
  selected, the value of the macro is
  ``199711L`` for the 1998 C++ standard,
  ``201103L`` for the 2011 C++ standard,
  ``201402L`` for the 2014 C++ standard,
  ``201703L`` for the 2017 C++ standard,
  ``202002L`` for the 2020 C++ standard,
  or an unspecified value strictly larger than ``202002L`` for the
  experimental languages enabled by :option:`-std=c++23` and
  :option:`-std=gnu++23`.

.. c:macro:: __OBJC__

  This macro is defined, with value 1, when the Objective-C compiler is in
  use.  You can use ``__OBJC__`` to test whether a header is compiled
  by a C compiler or an Objective-C compiler.

.. c:macro:: __ASSEMBLER__

  This macro is defined with value 1 when preprocessing assembly
  language.

.. index:: common predefined macros

.. _common-predefined-macros:

Common Predefined Macros
^^^^^^^^^^^^^^^^^^^^^^^^

The common predefined macros are GNU C extensions.  They are available
with the same meanings regardless of the machine or operating system on
which you are using GNU C or GNU Fortran.  Their names all start with
double underscores.

.. c:macro:: __COUNTER__

  This macro expands to sequential integral values starting from 0.  In
  conjunction with the ``##`` operator, this provides a convenient means to
  generate unique identifiers.  Care must be taken to ensure that
  ``__COUNTER__`` is not expanded prior to inclusion of precompiled headers
  which use it.  Otherwise, the precompiled headers will not be used.

.. c:macro:: __GFORTRAN__

  The GNU Fortran compiler defines this.

.. c:macro:: __GNUC__
             __GNUC_MINOR__
             __GNUC_PATCHLEVEL__

  These macros are defined by all GNU compilers that use the C
  preprocessor: C, C++, Objective-C and Fortran.  Their values are the major
  version, minor version, and patch level of the compiler, as integer
  constants.  For example, GCC version :samp:`{x}`. :samp:`{y}`. :samp:`{z}`
  defines ``__GNUC__`` to :samp:`{x}`, ``__GNUC_MINOR__`` to :samp:`{y}`,
  and ``__GNUC_PATCHLEVEL__`` to :samp:`{z}`.  These
  macros are also defined if you invoke the preprocessor directly.

  If all you need to know is whether or not your program is being compiled
  by GCC, or a non-GCC compiler that claims to accept the GNU C dialects,
  you can simply test ``__GNUC__``.  If you need to write code
  which depends on a specific version, you must be more careful.  Each
  time the minor version is increased, the patch level is reset to zero;
  each time the major version is increased, the
  minor version and patch level are reset.  If you wish to use the
  predefined macros directly in the conditional, you will need to write it
  like this:

  .. code-block:: c++

    /* Test for GCC > 3.2.0 */
    #if __GNUC__ > 3 || \
        (__GNUC__ == 3 && (__GNUC_MINOR__ > 2 || \
                           (__GNUC_MINOR__ == 2 && \
                            __GNUC_PATCHLEVEL__ > 0))

  Another approach is to use the predefined macros to
  calculate a single number, then compare that against a threshold:

  .. code-block:: c++

    #define GCC_VERSION (__GNUC__ * 10000 \
                         + __GNUC_MINOR__ * 100 \
                         + __GNUC_PATCHLEVEL__)
    ...
    /* Test for GCC > 3.2.0 */
    #if GCC_VERSION > 30200

  Many people find this form easier to understand.

.. c:macro:: __GNUG__

  The GNU C++ compiler defines this.  Testing it is equivalent to
  testing ``(__GNUC__ && __cplusplus)``.

.. c:macro:: __STRICT_ANSI__

  GCC defines this macro if and only if the :option:`-ansi` switch, or a
  :option:`-std` switch specifying strict conformance to some version of ISO C
  or ISO C++, was specified when GCC was invoked.  It is defined to :samp:`1`.
  This macro exists primarily to direct GNU libc's header files to use only
  definitions found in standard C.

.. c:macro:: __BASE_FILE__

  This macro expands to the name of the main input file, in the form
  of a C string constant.  This is the source file that was specified
  on the command line of the preprocessor or C compiler.

.. c:macro:: __FILE_NAME__

  This macro expands to the basename of the current input file, in the
  form of a C string constant.  This is the last path component by which
  the preprocessor opened the file.  For example, processing
  ``"/usr/local/include/myheader.h"`` would set this
  macro to ``"myheader.h"``.

.. c:macro:: __INCLUDE_LEVEL__

  This macro expands to a decimal integer constant that represents the
  depth of nesting in include files.  The value of this macro is
  incremented on every :samp:`#include` directive and decremented at the
  end of every included file.  It starts out at 0, its value within the
  base file specified on the command line.

.. c:macro:: __ELF__

  This macro is defined if the target uses the ELF object format.

.. c:macro:: __VERSION__

  This macro expands to a string constant which describes the version of
  the compiler in use.  You should not rely on its contents having any
  particular form, but it can be counted on to contain at least the
  release number.

.. c:macro:: __OPTIMIZE__
             __OPTIMIZE_SIZE__
             __NO_INLINE__

  These macros describe the compilation mode.  ``__OPTIMIZE__`` is
  defined in all optimizing compilations.  ``__OPTIMIZE_SIZE__`` is
  defined if the compiler is optimizing for size, not speed.
  ``__NO_INLINE__`` is defined if no functions will be inlined into
  their callers (when not optimizing, or when inlining has been
  specifically disabled by :option:`-fno-inline`).

  These macros cause certain GNU header files to provide optimized
  definitions, using macros or inline functions, of system library
  functions.  You should not use these macros in any way unless you make
  sure that programs will execute with the same effect whether or not they
  are defined.  If they are defined, their value is 1.

.. c:macro:: __GNUC_GNU_INLINE__

  GCC defines this macro if functions declared ``inline`` will be
  handled in GCC's traditional gnu90 mode.  Object files will contain
  externally visible definitions of all functions declared ``inline``
  without ``extern`` or ``static``.  They will not contain any
  definitions of any functions declared ``extern inline``.

.. c:macro:: __GNUC_STDC_INLINE__

  GCC defines this macro if functions declared ``inline`` will be
  handled according to the ISO C99 or later standards.  Object files will contain
  externally visible definitions of all functions declared ``extern
  inline``.  They will not contain definitions of any functions declared
  ``inline`` without ``extern``.

  If this macro is defined, GCC supports the ``gnu_inline`` function
  attribute as a way to always get the gnu90 behavior.

.. c:macro:: __CHAR_UNSIGNED__

  GCC defines this macro if and only if the data type ``char`` is
  unsigned on the target machine.  It exists to cause the standard header
  file :samp:`limits.h` to work correctly.  You should not use this macro
  yourself; instead, refer to the standard macros defined in :samp:`limits.h`.

.. c:macro:: __WCHAR_UNSIGNED__

  Like ``__CHAR_UNSIGNED__``, this macro is defined if and only if the
  data type ``wchar_t`` is unsigned and the front-end is in C++ mode.

.. c:macro:: __REGISTER_PREFIX__

  This macro expands to a single token (not a string constant) which is
  the prefix applied to CPU register names in assembly language for this
  target.  You can use it to write assembly that is usable in multiple
  environments.  For example, in the ``m68k-aout`` environment it
  expands to nothing, but in the ``m68k-coff`` environment it expands
  to a single :samp:`%`.

.. c:macro:: __USER_LABEL_PREFIX__

  This macro expands to a single token which is the prefix applied to
  user labels (symbols visible to C code) in assembly.  For example, in
  the ``m68k-aout`` environment it expands to an :samp:`_`, but in the
  ``m68k-coff`` environment it expands to nothing.

  This macro will have the correct definition even if
  :option:`-f(no-)underscores` is in use, but it will not be correct if
  target-specific options that adjust this prefix are used (e.g. the
  OSF/rose :option:`-mno-underscores` option).

.. c:macro:: __SIZE_TYPE__
             __PTRDIFF_TYPE__
             __WCHAR_TYPE__
             __WINT_TYPE__
             __INTMAX_TYPE__
             __UINTMAX_TYPE__
             __SIG_ATOMIC_TYPE__
             __INT8_TYPE__
             __INT16_TYPE__
             __INT32_TYPE__
             __INT64_TYPE__
             __UINT8_TYPE__
             __UINT16_TYPE__
             __UINT32_TYPE__
             __UINT64_TYPE__
             __INT_LEAST8_TYPE__
             __INT_LEAST16_TYPE__
             __INT_LEAST32_TYPE__
             __INT_LEAST64_TYPE__
             __UINT_LEAST8_TYPE__
             __UINT_LEAST16_TYPE__
             __UINT_LEAST32_TYPE__
             __UINT_LEAST64_TYPE__
             __INT_FAST8_TYPE__
             __INT_FAST16_TYPE__
             __INT_FAST32_TYPE__
             __INT_FAST64_TYPE__
             __UINT_FAST8_TYPE__
             __UINT_FAST16_TYPE__
             __UINT_FAST32_TYPE__
             __UINT_FAST64_TYPE__
             __INTPTR_TYPE__
             __UINTPTR_TYPE__

  These macros are defined to the correct underlying types for the
  ``size_t``, ``ptrdiff_t``, ``wchar_t``, ``wint_t``,
  ``intmax_t``, ``uintmax_t``, ``sig_atomic_t``, ``int8_t``,
  ``int16_t``, ``int32_t``, ``int64_t``, ``uint8_t``,
  ``uint16_t``, ``uint32_t``, ``uint64_t``,
  ``int_least8_t``, ``int_least16_t``, ``int_least32_t``,
  ``int_least64_t``, ``uint_least8_t``, ``uint_least16_t``,
  ``uint_least32_t``, ``uint_least64_t``, ``int_fast8_t``,
  ``int_fast16_t``, ``int_fast32_t``, ``int_fast64_t``,
  ``uint_fast8_t``, ``uint_fast16_t``, ``uint_fast32_t``,
  ``uint_fast64_t``, ``intptr_t``, and ``uintptr_t`` typedefs,
  respectively.  They exist to make the standard header files
  :samp:`stddef.h`, :samp:`stdint.h`, and :samp:`wchar.h` work correctly.
  You should not use these macros directly; instead, include the
  appropriate headers and use the typedefs.  Some of these macros may
  not be defined on particular systems if GCC does not provide a
  :samp:`stdint.h` header on those systems.

.. c:macro:: __CHAR_BIT__

  Defined to the number of bits used in the representation of the
  ``char`` data type.  It exists to make the standard header given
  numerical limits work correctly.  You should not use
  this macro directly; instead, include the appropriate headers.

.. c:macro:: __SCHAR_MAX__
             __WCHAR_MAX__
             __SHRT_MAX__
             __INT_MAX__
             __LONG_MAX__
             __LONG_LONG_MAX__
             __WINT_MAX__
             __SIZE_MAX__
             __PTRDIFF_MAX__
             __INTMAX_MAX__
             __UINTMAX_MAX__
             __SIG_ATOMIC_MAX__
             __INT8_MAX__
             __INT16_MAX__
             __INT32_MAX__
             __INT64_MAX__
             __UINT8_MAX__
             __UINT16_MAX__
             __UINT32_MAX__
             __UINT64_MAX__
             __INT_LEAST8_MAX__
             __INT_LEAST16_MAX__
             __INT_LEAST32_MAX__
             __INT_LEAST64_MAX__
             __UINT_LEAST8_MAX__
             __UINT_LEAST16_MAX__
             __UINT_LEAST32_MAX__
             __UINT_LEAST64_MAX__
             __INT_FAST8_MAX__
             __INT_FAST16_MAX__
             __INT_FAST32_MAX__
             __INT_FAST64_MAX__
             __UINT_FAST8_MAX__
             __UINT_FAST16_MAX__
             __UINT_FAST32_MAX__
             __UINT_FAST64_MAX__
             __INTPTR_MAX__
             __UINTPTR_MAX__
             __WCHAR_MIN__
             __WINT_MIN__
             __SIG_ATOMIC_MIN__

  Defined to the maximum value of the ``signed char``, ``wchar_t``,
  ``signed short``,
  ``signed int``, ``signed long``, ``signed long long``,
  ``wint_t``, ``size_t``, ``ptrdiff_t``,
  ``intmax_t``, ``uintmax_t``, ``sig_atomic_t``, ``int8_t``,
  ``int16_t``, ``int32_t``, ``int64_t``, ``uint8_t``,
  ``uint16_t``, ``uint32_t``, ``uint64_t``,
  ``int_least8_t``, ``int_least16_t``, ``int_least32_t``,
  ``int_least64_t``, ``uint_least8_t``, ``uint_least16_t``,
  ``uint_least32_t``, ``uint_least64_t``, ``int_fast8_t``,
  ``int_fast16_t``, ``int_fast32_t``, ``int_fast64_t``,
  ``uint_fast8_t``, ``uint_fast16_t``, ``uint_fast32_t``,
  ``uint_fast64_t``, ``intptr_t``, and ``uintptr_t`` types and
  to the minimum value of the ``wchar_t``, ``wint_t``, and
  ``sig_atomic_t`` types respectively.  They exist to make the
  standard header given numerical limits work correctly.  You should not
  use these macros directly; instead, include the appropriate headers.
  Some of these macros may not be defined on particular systems if GCC
  does not provide a :samp:`stdint.h` header on those systems.

.. c:macro:: __INT8_C
             __INT16_C
             __INT32_C
             __INT64_C
             __UINT8_C
             __UINT16_C
             __UINT32_C
             __UINT64_C
             __INTMAX_C
             __UINTMAX_C

  Defined to implementations of the standard :samp:`stdint.h` macros with
  the same names without the leading ``__``.  They exist the make the
  implementation of that header work correctly.  You should not use
  these macros directly; instead, include the appropriate headers.  Some
  of these macros may not be defined on particular systems if GCC does
  not provide a :samp:`stdint.h` header on those systems.

.. c:macro:: __SCHAR_WIDTH__
             __SHRT_WIDTH__
             __INT_WIDTH__
             __LONG_WIDTH__
             __LONG_LONG_WIDTH__
             __PTRDIFF_WIDTH__
             __SIG_ATOMIC_WIDTH__
             __SIZE_WIDTH__
             __WCHAR_WIDTH__
             __WINT_WIDTH__
             __INT_LEAST8_WIDTH__
             __INT_LEAST16_WIDTH__
             __INT_LEAST32_WIDTH__
             __INT_LEAST64_WIDTH__
             __INT_FAST8_WIDTH__
             __INT_FAST16_WIDTH__
             __INT_FAST32_WIDTH__
             __INT_FAST64_WIDTH__
             __INTPTR_WIDTH__
             __INTMAX_WIDTH__

  Defined to the bit widths of the corresponding types.  They exist to
  make the implementations of :samp:`limits.h` and :samp:`stdint.h` behave
  correctly.  You should not use these macros directly; instead, include
  the appropriate headers.  Some of these macros may not be defined on
  particular systems if GCC does not provide a :samp:`stdint.h` header on
  those systems.

.. c:macro:: __SIZEOF_INT__
             __SIZEOF_LONG__
             __SIZEOF_LONG_LONG__
             __SIZEOF_SHORT__
             __SIZEOF_POINTER__
             __SIZEOF_FLOAT__
             __SIZEOF_DOUBLE__
             __SIZEOF_LONG_DOUBLE__
             __SIZEOF_SIZE_T__
             __SIZEOF_WCHAR_T__
             __SIZEOF_WINT_T__
             __SIZEOF_PTRDIFF_T__

  Defined to the number of bytes of the C standard data types: ``int``,
  ``long``, ``long long``, ``short``, ``void *``, ``float``,
  ``double``, ``long double``, ``size_t``, ``wchar_t``, ``wint_t``
  and ``ptrdiff_t``.

.. c:macro:: __BYTE_ORDER__
             __ORDER_LITTLE_ENDIAN__
             __ORDER_BIG_ENDIAN__
             __ORDER_PDP_ENDIAN__

  ``__BYTE_ORDER__`` is defined to one of the values
  ``__ORDER_LITTLE_ENDIAN__``, ``__ORDER_BIG_ENDIAN__``, or
  ``__ORDER_PDP_ENDIAN__`` to reflect the layout of multi-byte and
  multi-word quantities in memory.  If ``__BYTE_ORDER__`` is equal to
  ``__ORDER_LITTLE_ENDIAN__`` or ``__ORDER_BIG_ENDIAN__``, then
  multi-byte and multi-word quantities are laid out identically: the
  byte (word) at the lowest address is the least significant or most
  significant byte (word) of the quantity, respectively.  If
  ``__BYTE_ORDER__`` is equal to ``__ORDER_PDP_ENDIAN__``, then
  bytes in 16-bit words are laid out in a little-endian fashion, whereas
  the 16-bit subwords of a 32-bit quantity are laid out in big-endian
  fashion.

  You should use these macros for testing like this:

  .. code-block:: c++

    /* Test for a little-endian machine */
    #if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__

.. c:macro:: __FLOAT_WORD_ORDER__

  ``__FLOAT_WORD_ORDER__`` is defined to one of the values
  ``__ORDER_LITTLE_ENDIAN__`` or ``__ORDER_BIG_ENDIAN__`` to reflect
  the layout of the words of multi-word floating-point quantities.

.. c:macro:: __DEPRECATED

  This macro is defined, with value 1, when compiling a C++ source file
  with warnings about deprecated constructs enabled.  These warnings are
  enabled by default, but can be disabled with :option:`-Wno-deprecated`.

.. c:macro:: __EXCEPTIONS

  This macro is defined, with value 1, when compiling a C++ source file
  with exceptions enabled.  If :option:`-fno-exceptions` is used when
  compiling the file, then this macro is not defined.

.. c:macro:: __GXX_RTTI

  This macro is defined, with value 1, when compiling a C++ source file
  with runtime type identification enabled.  If :option:`-fno-rtti` is
  used when compiling the file, then this macro is not defined.

.. c:macro:: __USING_SJLJ_EXCEPTIONS__

  This macro is defined, with value 1, if the compiler uses the old
  mechanism based on ``setjmp`` and ``longjmp`` for exception
  handling.

.. c:macro:: __GXX_EXPERIMENTAL_CXX0X__

  This macro is defined when compiling a C++ source file with C++11 features
  enabled, i.e., for all C++ language dialects except :option:`-std=c++98`
  and :option:`-std=gnu++98`. This macro is obsolete, but can be used to
  detect experimental C++0x features in very old versions of GCC. Since
  GCC 4.7.0 the ``__cplusplus`` macro is defined correctly, so most
  code should test ``__cplusplus >= 201103L`` instead of using this
  macro.

.. c:macro:: __GXX_WEAK__

  This macro is defined when compiling a C++ source file.  It has the
  value 1 if the compiler will use weak symbols, COMDAT sections, or
  other similar techniques to collapse symbols with 'vague linkage'
  that are defined in multiple translation units.  If the compiler will
  not collapse such symbols, this macro is defined with value 0.  In
  general, user code should not need to make use of this macro; the
  purpose of this macro is to ease implementation of the C++ runtime
  library provided with G++.

.. c:macro:: __NEXT_RUNTIME__

  This macro is defined, with value 1, if (and only if) the NeXT runtime
  (as in :option:`-fnext-runtime`) is in use for Objective-C.  If the GNU
  runtime is used, this macro is not defined, so that you can use this
  macro to determine which runtime (NeXT or GNU) is being used.

.. c:macro:: __LP64__
             _LP64

  These macros are defined, with value 1, if (and only if) the compilation
  is for a target where ``long int`` and pointer both use 64-bits and
  ``int`` uses 32-bit.

.. c:macro:: __SSP__

  This macro is defined, with value 1, when :option:`-fstack-protector` is in
  use.

.. c:macro:: __SSP_ALL__

  This macro is defined, with value 2, when :option:`-fstack-protector-all` is
  in use.

.. c:macro:: __SSP_STRONG__

  This macro is defined, with value 3, when :option:`-fstack-protector-strong` is
  in use.

.. c:macro:: __SSP_EXPLICIT__

  This macro is defined, with value 4, when :option:`-fstack-protector-explicit` is
  in use.

``__SANITIZE_ADDRESS__``
  This macro is defined, with value 1, when :option:`-fsanitize=address`
  or :option:`-fsanitize=kernel-address` are in use.

``__SANITIZE_THREAD__``
  This macro is defined, with value 1, when :option:`-fsanitize=thread` is in use.

.. c:macro:: __TIMESTAMP__

  This macro expands to a string constant that describes the date and time
  of the last modification of the current source file. The string constant
  contains abbreviated day of the week, month, day of the month, time in
  hh:mm:ss form, year and looks like ``"Sun Sep 16 01:03:52 1973"``.
  If the day of the month is less than 10, it is padded with a space on the left.

  If GCC cannot determine the current date, it will emit a warning message
  (once per compilation) and ``__TIMESTAMP__`` will expand to
  ``"??? ??? ?? ??:??:?? ????"``.

.. c:macro:: __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1
             __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
             __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
             __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8
             __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16

  These macros are defined when the target processor supports atomic compare
  and swap operations on operands 1, 2, 4, 8 or 16 bytes in length, respectively.

.. c:macro:: __HAVE_SPECULATION_SAFE_VALUE

  This macro is defined with the value 1 to show that this version of GCC
  supports ``__builtin_speculation_safe_value``.

.. c:macro:: __GCC_HAVE_DWARF2_CFI_ASM

  This macro is defined when the compiler is emitting DWARF CFI directives
  to the assembler.  When this is defined, it is possible to emit those same
  directives in inline assembly.

.. c:macro:: __FP_FAST_FMA
             __FP_FAST_FMAF
             __FP_FAST_FMAL

  These macros are defined with value 1 if the backend supports the
  ``fma``, ``fmaf``, and ``fmal`` builtin functions, so that
  the include file :samp:`math.h` can define the macros
  ``FP_FAST_FMA``, ``FP_FAST_FMAF``, and ``FP_FAST_FMAL``
  for compatibility with the 1999 C standard.

.. c:macro:: __FP_FAST_FMAF16
             __FP_FAST_FMAF32
             __FP_FAST_FMAF64
             __FP_FAST_FMAF128
             __FP_FAST_FMAF32X
             __FP_FAST_FMAF64X
             __FP_FAST_FMAF128X

  These macros are defined with the value 1 if the backend supports the
  ``fma`` functions using the additional ``_Floatn`` and
  ``_Floatnx`` types that are defined in ISO/IEC TS
  18661-3:2015.  The include file :samp:`math.h` can define the
  ``FP_FAST_FMAFn`` and ``FP_FAST_FMAFnx`` macros if
  the user defined ``__STDC_WANT_IEC_60559_TYPES_EXT__`` before
  including :samp:`math.h`.

.. c:macro:: __GCC_IEC_559

  This macro is defined to indicate the intended level of support for
  IEEE 754 (IEC 60559) floating-point arithmetic.  It expands to a
  nonnegative integer value.  If 0, it indicates that the combination of
  the compiler configuration and the command-line options is not
  intended to support IEEE 754 arithmetic for ``float`` and
  ``double`` as defined in C99 and C11 Annex F (for example, that the
  standard rounding modes and exceptions are not supported, or that
  optimizations are enabled that conflict with IEEE 754 semantics).  If
  1, it indicates that IEEE 754 arithmetic is intended to be supported;
  this does not mean that all relevant language features are supported
  by GCC.  If 2 or more, it additionally indicates support for IEEE
  754-2008 (in particular, that the binary encodings for quiet and
  signaling NaNs are as specified in IEEE 754-2008).

  This macro does not indicate the default state of command-line options
  that control optimizations that C99 and C11 permit to be controlled by
  standard pragmas, where those standards do not require a particular
  default state.  It does not indicate whether optimizations respect
  signaling NaN semantics (the macro for that is
  ``__SUPPORT_SNAN__``).  It does not indicate support for decimal
  floating point or the IEEE 754 binary16 and binary128 types.

.. c:macro:: __GCC_IEC_559_COMPLEX

  This macro is defined to indicate the intended level of support for
  IEEE 754 (IEC 60559) floating-point arithmetic for complex numbers, as
  defined in C99 and C11 Annex G.  It expands to a nonnegative integer
  value.  If 0, it indicates that the combination of the compiler
  configuration and the command-line options is not intended to support
  Annex G requirements (for example, because :option:`-fcx-limited-range`
  was used).  If 1 or more, it indicates that it is intended to support
  those requirements; this does not mean that all relevant language
  features are supported by GCC.

.. c:macro:: __NO_MATH_ERRNO__

  This macro is defined if :option:`-fno-math-errno` is used, or enabled
  by another option such as :option:`-ffast-math` or by default.

.. c:macro:: __RECIPROCAL_MATH__

  This macro is defined if :option:`-freciprocal-math` is used, or enabled
  by another option such as :option:`-ffast-math` or by default.

.. c:macro:: __NO_SIGNED_ZEROS__

  This macro is defined if :option:`-fno-signed-zeros` is used, or enabled
  by another option such as :option:`-ffast-math` or by default.

.. c:macro:: __NO_TRAPPING_MATH__

  This macro is defined if :option:`-fno-trapping-math` is used.

.. c:macro:: __ASSOCIATIVE_MATH__

  This macro is defined if :option:`-fassociative-math` is used, or enabled
  by another option such as :option:`-ffast-math` or by default.

.. c:macro:: __ROUNDING_MATH__

  This macro is defined if :option:`-frounding-math` is used.

.. c:macro:: __GNUC_EXECUTION_CHARSET_NAME
             __GNUC_WIDE_EXECUTION_CHARSET_NAME

  These macros are defined to expand to a narrow string literal of
  the name of the narrow and wide compile-time execution character
  set used.  It directly reflects the name passed to the options
  :option:`-fexec-charset` and :option:`-fwide-exec-charset`, or the defaults
  documented for those options (that is, it can expand to something like
  ``"UTF-8"``).  See :ref:`invocation`.

.. index:: system-specific predefined macros, predefined macros, system-specific, reserved namespace

.. _system-specific-predefined-macros:

System-specific Predefined Macros
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The C preprocessor normally predefines several macros that indicate what
type of system and machine is in use.  They are obviously different on
each target supported by GCC.  This manual, being for all systems and
machines, cannot tell you what their names are, but you can use
:command:`cpp -dM` to see them all.  See :ref:`invocation`.  All system-specific
predefined macros expand to a constant value, so you can test them with
either :samp:`#ifdef` or :samp:`#if`.

The C standard requires that all system-specific macros be part of the
:dfn:`reserved namespace`.  All names which begin with two underscores,
or an underscore and a capital letter, are reserved for the compiler and
library to use as they wish.  However, historically system-specific
macros have had names with no special prefix; for instance, it is common
to find ``unix`` defined on Unix systems.  For all such macros, GCC
provides a parallel macro with two underscores added at the beginning
and the end.  If ``unix`` is defined, ``__unix__`` will be defined
too.  There will never be more than two underscores; the parallel of
``_mips`` is ``__mips__``.

When the :option:`-ansi` option, or any :option:`-std` option that
requests strict conformance, is given to the compiler, all the
system-specific predefined macros outside the reserved namespace are
suppressed.  The parallel macros, inside the reserved namespace, remain
defined.

We are slowly phasing out all predefined macros which are outside the
reserved namespace.  You should never use them in new programs, and we
encourage you to correct older code to use the parallel macros whenever
you find it.  We don't recommend you use the system-specific macros that
are in the reserved namespace, either.  It is better in the long run to
check specifically for features you need, using a tool such as
:command:`autoconf`.

.. index:: named operators, C++ named operators, iso646.h

.. _c++-named-operators:

C++ Named Operators
^^^^^^^^^^^^^^^^^^^

In C++, there are eleven keywords which are simply alternate spellings
of operators normally written with punctuation.  These keywords are
treated as such even in the preprocessor.  They function as operators in
:samp:`#if`, and they cannot be defined as macros or poisoned.  In C, you
can request that those keywords take their C++ meaning by including
:samp:`iso646.h`.  That header defines each one as a normal object-like
macro expanding to the appropriate punctuator.

These are the named operators and their corresponding punctuators:

.. list-table::

   * - Named Operator
     - Punctuator
   * - ``and``
     - ``&&``
   * - ``and_eq``
     - ``&=``
   * - ``bitand``
     - ``&``
   * - ``bitor``
     - ``|``
   * - ``compl``
     - ``~``
   * - ``not``
     - ``!``
   * - ``not_eq``
     - ``!=``
   * - ``or``
     - ``||``
   * - ``or_eq``
     - ``|=``
   * - ``xor``
     - ``^``
   * - ``xor_eq``
     - ``^=``