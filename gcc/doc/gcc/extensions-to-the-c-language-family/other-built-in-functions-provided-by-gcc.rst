..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: built-in functions, __builtin_alloca, __builtin_alloca_with_align, __builtin_alloca_with_align_and_max, __builtin_call_with_static_chain, __builtin_extend_pointer, __builtin_fpclassify, __builtin_has_attribute, __builtin_isfinite, __builtin_isnormal, __builtin_isgreater, __builtin_isgreaterequal, __builtin_isinf_sign, __builtin_isless, __builtin_islessequal, __builtin_islessgreater, __builtin_isunordered, __builtin_object_size, __builtin_powi, __builtin_powif, __builtin_powil, __builtin_speculation_safe_value, _Exit, _exit, abort, abs, acos, acosf, acosh, acoshf, acoshl, acosl, alloca, asin, asinf, asinh, asinhf, asinhl, asinl, atan, atan2, atan2f, atan2l, atanf, atanh, atanhf, atanhl, atanl, bcmp, bzero, cabs, cabsf, cabsl, cacos, cacosf, cacosh, cacoshf, cacoshl, cacosl, calloc, carg, cargf, cargl, casin, casinf, casinh, casinhf, casinhl, casinl, catan, catanf, catanh, catanhf, catanhl, catanl, cbrt, cbrtf, cbrtl, ccos, ccosf, ccosh, ccoshf, ccoshl, ccosl, ceil, ceilf, ceill, cexp, cexpf, cexpl, cimag, cimagf, cimagl, clog, clogf, clogl, clog10, clog10f, clog10l, conj, conjf, conjl, copysign, copysignf, copysignl, cos, cosf, cosh, coshf, coshl, cosl, cpow, cpowf, cpowl, cproj, cprojf, cprojl, creal, crealf, creall, csin, csinf, csinh, csinhf, csinhl, csinl, csqrt, csqrtf, csqrtl, ctan, ctanf, ctanh, ctanhf, ctanhl, ctanl, dcgettext, dgettext, drem, dremf, dreml, erf, erfc, erfcf, erfcl, erff, erfl, exit, exp, exp10, exp10f, exp10l, exp2, exp2f, exp2l, expf, expl, expm1, expm1f, expm1l, fabs, fabsf, fabsl, fdim, fdimf, fdiml, ffs, floor, floorf, floorl, fma, fmaf, fmal, fmax, fmaxf, fmaxl, fmin, fminf, fminl, fmod, fmodf, fmodl, fprintf, fprintf_unlocked, fputs, fputs_unlocked, free, frexp, frexpf, frexpl, fscanf, gamma, gammaf, gammal, gamma_r, gammaf_r, gammal_r, gettext, hypot, hypotf, hypotl, ilogb, ilogbf, ilogbl, imaxabs, index, isalnum, isalpha, isascii, isblank, iscntrl, isdigit, isgraph, islower, isprint, ispunct, isspace, isupper, iswalnum, iswalpha, iswblank, iswcntrl, iswdigit, iswgraph, iswlower, iswprint, iswpunct, iswspace, iswupper, iswxdigit, isxdigit, j0, j0f, j0l, j1, j1f, j1l, jn, jnf, jnl, labs, ldexp, ldexpf, ldexpl, lgamma, lgammaf, lgammal, lgamma_r, lgammaf_r, lgammal_r, llabs, llrint, llrintf, llrintl, llround, llroundf, llroundl, log, log10, log10f, log10l, log1p, log1pf, log1pl, log2, log2f, log2l, logb, logbf, logbl, logf, logl, lrint, lrintf, lrintl, lround, lroundf, lroundl, malloc, memchr, memcmp, memcpy, mempcpy, memset, modf, modff, modfl, nearbyint, nearbyintf, nearbyintl, nextafter, nextafterf, nextafterl, nexttoward, nexttowardf, nexttowardl, pow, pow10, pow10f, pow10l, powf, powl, printf, printf_unlocked, putchar, puts, realloc, remainder, remainderf, remainderl, remquo, remquof, remquol, rindex, rint, rintf, rintl, round, roundf, roundl, scalb, scalbf, scalbl, scalbln, scalblnf, scalblnf, scalbn, scalbnf, scanfnl, signbit, signbitf, signbitl, signbitd32, signbitd64, signbitd128, significand, significandf, significandl, sin, sincos, sincosf, sincosl, sinf, sinh, sinhf, sinhl, sinl, snprintf, sprintf, sqrt, sqrtf, sqrtl, sscanf, stpcpy, stpncpy, strcasecmp, strcat, strchr, strcmp, strcpy, strcspn, strdup, strfmon, strftime, strlen, strncasecmp, strncat, strncmp, strncpy, strndup, strnlen, strpbrk, strrchr, strspn, strstr, tan, tanf, tanh, tanhf, tanhl, tanl, tgamma, tgammaf, tgammal, toascii, tolower, toupper, towlower, towupper, trunc, truncf, truncl, vfprintf, vfscanf, vprintf, vscanf, vsnprintf, vsprintf, vsscanf, y0, y0f, y0l, y1, y1f, y1l, yn, ynf, ynl

.. _other-builtins:

Other Built-in Functions Provided by GCC
****************************************

GCC provides a large number of built-in functions other than the ones
mentioned above.  Some of these are for internal use in the processing
of exceptions or variable-length argument lists and are not
documented here because they may change from time to time; we do not
recommend general use of these functions.

The remaining functions are provided for optimization purposes.

With the exception of built-ins that have library equivalents such as
the standard C library functions discussed below, or that expand to
library calls, GCC built-in functions are always expanded inline and
thus do not have corresponding entry points and their address cannot
be obtained.  Attempting to use them in an expression other than
a function call results in a compile-time error.

.. index:: fno-builtin

GCC includes built-in versions of many of the functions in the standard
C library.  These functions come in two forms: one whose names start with
the ``__builtin_`` prefix, and the other without.  Both forms have the
same type (including prototype), the same address (when their address is
taken), and the same meaning as the C library functions even if you specify
the :option:`-fno-builtin` option see :ref:`c-dialect-options`).  Many of these
functions are only optimized in certain cases; if they are not optimized in
a particular case, a call to the library function is emitted.

.. index:: ansi, std

Outside strict ISO C mode (:option:`-ansi`, :option:`-std=c90`,
:option:`-std=c99` or :option:`-std=c11`), the functions
``_exit``, ``alloca``, ``bcmp``, ``bzero``,
``dcgettext``, ``dgettext``, ``dremf``, ``dreml``,
``drem``, ``exp10f``, ``exp10l``, ``exp10``, ``ffsll``,
``ffsl``, ``ffs``, ``fprintf_unlocked``,
``fputs_unlocked``, ``gammaf``, ``gammal``, ``gamma``,
``gammaf_r``, ``gammal_r``, ``gamma_r``, ``gettext``,
``index``, ``isascii``, ``j0f``, ``j0l``, ``j0``,
``j1f``, ``j1l``, ``j1``, ``jnf``, ``jnl``, ``jn``,
``lgammaf_r``, ``lgammal_r``, ``lgamma_r``, ``mempcpy``,
``pow10f``, ``pow10l``, ``pow10``, ``printf_unlocked``,
``rindex``, ``roundeven``, ``roundevenf``, ``roundevenl``,
``scalbf``, ``scalbl``, ``scalb``,
``signbit``, ``signbitf``, ``signbitl``, ``signbitd32``,
``signbitd64``, ``signbitd128``, ``significandf``,
``significandl``, ``significand``, ``sincosf``,
``sincosl``, ``sincos``, ``stpcpy``, ``stpncpy``,
``strcasecmp``, ``strdup``, ``strfmon``, ``strncasecmp``,
``strndup``, ``strnlen``, ``toascii``, ``y0f``, ``y0l``,
``y0``, ``y1f``, ``y1l``, ``y1``, ``ynf``, ``ynl`` and
``yn``
may be handled as built-in functions.
All these functions have corresponding versions
prefixed with ``__builtin_``, which may be used even in strict C90
mode.

The ISO C99 functions
``_Exit``, ``acoshf``, ``acoshl``, ``acosh``, ``asinhf``,
``asinhl``, ``asinh``, ``atanhf``, ``atanhl``, ``atanh``,
``cabsf``, ``cabsl``, ``cabs``, ``cacosf``, ``cacoshf``,
``cacoshl``, ``cacosh``, ``cacosl``, ``cacos``,
``cargf``, ``cargl``, ``carg``, ``casinf``, ``casinhf``,
``casinhl``, ``casinh``, ``casinl``, ``casin``,
``catanf``, ``catanhf``, ``catanhl``, ``catanh``,
``catanl``, ``catan``, ``cbrtf``, ``cbrtl``, ``cbrt``,
``ccosf``, ``ccoshf``, ``ccoshl``, ``ccosh``, ``ccosl``,
``ccos``, ``cexpf``, ``cexpl``, ``cexp``, ``cimagf``,
``cimagl``, ``cimag``, ``clogf``, ``clogl``, ``clog``,
``conjf``, ``conjl``, ``conj``, ``copysignf``, ``copysignl``,
``copysign``, ``cpowf``, ``cpowl``, ``cpow``, ``cprojf``,
``cprojl``, ``cproj``, ``crealf``, ``creall``, ``creal``,
``csinf``, ``csinhf``, ``csinhl``, ``csinh``, ``csinl``,
``csin``, ``csqrtf``, ``csqrtl``, ``csqrt``, ``ctanf``,
``ctanhf``, ``ctanhl``, ``ctanh``, ``ctanl``, ``ctan``,
``erfcf``, ``erfcl``, ``erfc``, ``erff``, ``erfl``,
``erf``, ``exp2f``, ``exp2l``, ``exp2``, ``expm1f``,
``expm1l``, ``expm1``, ``fdimf``, ``fdiml``, ``fdim``,
``fmaf``, ``fmal``, ``fmaxf``, ``fmaxl``, ``fmax``,
``fma``, ``fminf``, ``fminl``, ``fmin``, ``hypotf``,
``hypotl``, ``hypot``, ``ilogbf``, ``ilogbl``, ``ilogb``,
``imaxabs``, ``isblank``, ``iswblank``, ``lgammaf``,
``lgammal``, ``lgamma``, ``llabs``, ``llrintf``, ``llrintl``,
``llrint``, ``llroundf``, ``llroundl``, ``llround``,
``log1pf``, ``log1pl``, ``log1p``, ``log2f``, ``log2l``,
``log2``, ``logbf``, ``logbl``, ``logb``, ``lrintf``,
``lrintl``, ``lrint``, ``lroundf``, ``lroundl``,
``lround``, ``nearbyintf``, ``nearbyintl``, ``nearbyint``,
``nextafterf``, ``nextafterl``, ``nextafter``,
``nexttowardf``, ``nexttowardl``, ``nexttoward``,
``remainderf``, ``remainderl``, ``remainder``, ``remquof``,
``remquol``, ``remquo``, ``rintf``, ``rintl``, ``rint``,
``roundf``, ``roundl``, ``round``, ``scalblnf``,
``scalblnl``, ``scalbln``, ``scalbnf``, ``scalbnl``,
``scalbn``, ``snprintf``, ``tgammaf``, ``tgammal``,
``tgamma``, ``truncf``, ``truncl``, ``trunc``,
``vfscanf``, ``vscanf``, ``vsnprintf`` and ``vsscanf``
are handled as built-in functions
except in strict ISO C90 mode (:option:`-ansi` or :option:`-std=c90`).

There are also built-in versions of the ISO C99 functions
``acosf``, ``acosl``, ``asinf``, ``asinl``, ``atan2f``,
``atan2l``, ``atanf``, ``atanl``, ``ceilf``, ``ceill``,
``cosf``, ``coshf``, ``coshl``, ``cosl``, ``expf``,
``expl``, ``fabsf``, ``fabsl``, ``floorf``, ``floorl``,
``fmodf``, ``fmodl``, ``frexpf``, ``frexpl``, ``ldexpf``,
``ldexpl``, ``log10f``, ``log10l``, ``logf``, ``logl``,
``modfl``, ``modff``, ``powf``, ``powl``, ``sinf``,
``sinhf``, ``sinhl``, ``sinl``, ``sqrtf``, ``sqrtl``,
``tanf``, ``tanhf``, ``tanhl`` and ``tanl``
that are recognized in any mode since ISO C90 reserves these names for
the purpose to which ISO C99 puts them.  All these functions have
corresponding versions prefixed with ``__builtin_``.

There are also built-in functions ``__builtin_fabsfn``,
``__builtin_fabsfnx``, ``__builtin_copysignfn`` and
``__builtin_copysignfnx``, corresponding to the TS 18661-3
functions ``fabsfn``, ``fabsfnx``,
``copysignfn`` and ``copysignfnx``, for supported
types ``_Floatn`` and ``_Floatnx``.

There are also GNU extension functions ``clog10``, ``clog10f`` and
``clog10l`` which names are reserved by ISO C99 for future use.
All these functions have versions prefixed with ``__builtin_``.

The ISO C94 functions
``iswalnum``, ``iswalpha``, ``iswcntrl``, ``iswdigit``,
``iswgraph``, ``iswlower``, ``iswprint``, ``iswpunct``,
``iswspace``, ``iswupper``, ``iswxdigit``, ``towlower`` and
``towupper``
are handled as built-in functions
except in strict ISO C90 mode (:option:`-ansi` or :option:`-std=c90`).

The ISO C90 functions
``abort``, ``abs``, ``acos``, ``asin``, ``atan2``,
``atan``, ``calloc``, ``ceil``, ``cosh``, ``cos``,
``exit``, ``exp``, ``fabs``, ``floor``, ``fmod``,
``fprintf``, ``fputs``, ``free``, ``frexp``, ``fscanf``,
``isalnum``, ``isalpha``, ``iscntrl``, ``isdigit``,
``isgraph``, ``islower``, ``isprint``, ``ispunct``,
``isspace``, ``isupper``, ``isxdigit``, ``tolower``,
``toupper``, ``labs``, ``ldexp``, ``log10``, ``log``,
``malloc``, ``memchr``, ``memcmp``, ``memcpy``,
``memset``, ``modf``, ``pow``, ``printf``, ``putchar``,
``puts``, ``realloc``, ``scanf``, ``sinh``, ``sin``,
``snprintf``, ``sprintf``, ``sqrt``, ``sscanf``, ``strcat``,
``strchr``, ``strcmp``, ``strcpy``, ``strcspn``,
``strlen``, ``strncat``, ``strncmp``, ``strncpy``,
``strpbrk``, ``strrchr``, ``strspn``, ``strstr``,
``tanh``, ``tan``, ``vfprintf``, ``vprintf`` and ``vsprintf``
are all recognized as built-in functions unless
:option:`-fno-builtin` is specified (or :option:`-fno-builtin-function`
is specified for an individual function).  All of these functions have
corresponding versions prefixed with ``__builtin_``.

GCC provides built-in versions of the ISO C99 floating-point comparison
macros that avoid raising exceptions for unordered operands.  They have
the same names as the standard macros ( ``isgreater``,
``isgreaterequal``, ``isless``, ``islessequal``,
``islessgreater``, and ``isunordered``) , with ``__builtin_``
prefixed.  We intend for a library implementor to be able to simply
``#define`` each standard macro to its built-in equivalent.
In the same fashion, GCC provides ``fpclassify``, ``isfinite``,
``isinf_sign``, ``isnormal`` and ``signbit`` built-ins used with
``__builtin_`` prefixed.  The ``isinf`` and ``isnan``
built-in functions appear both with and without the ``__builtin_`` prefix.
With ``-ffinite-math-only`` option the ``isinf`` and ``isnan``
built-in functions will always return 0.

GCC provides built-in versions of the ISO C99 floating-point rounding and
exceptions handling functions ``fegetround``, ``feclearexcept`` and
``feraiseexcept``.  They may not be available for all targets, and because
they need close interaction with libc internal values, they may not be available
for all target libcs, but in all cases they will gracefully fallback to libc
calls.  These built-in functions appear both with and without the
``__builtin_`` prefix.

.. function:: void *__builtin_alloca (size_t size)

  The ``__builtin_alloca`` function must be called at block scope.
  The function allocates an object :samp:`{size}` bytes large on the stack
  of the calling function.  The object is aligned on the default stack
  alignment boundary for the target determined by the
  ``__BIGGEST_ALIGNMENT__`` macro.  The ``__builtin_alloca``
  function returns a pointer to the first byte of the allocated object.
  The lifetime of the allocated object ends just before the calling
  function returns to its caller.   This is so even when
  ``__builtin_alloca`` is called within a nested block.

  For example, the following function allocates eight objects of ``n``
  bytes each on the stack, storing a pointer to each in consecutive elements
  of the array ``a``.  It then passes the array to function ``g``
  which can safely use the storage pointed to by each of the array elements.

  .. code-block:: c++

    void f (unsigned n)
    {
      void *a [8];
      for (int i = 0; i != 8; ++i)
        a [i] = __builtin_alloca (n);

      g (a, n);   // safe
    }

  Since the ``__builtin_alloca`` function doesn't validate its argument
  it is the responsibility of its caller to make sure the argument doesn't
  cause it to exceed the stack size limit.
  The ``__builtin_alloca`` function is provided to make it possible to
  allocate on the stack arrays of bytes with an upper bound that may be
  computed at run time.  Since C99 Variable Length Arrays offer
  similar functionality under a portable, more convenient, and safer
  interface they are recommended instead, in both C99 and C++ programs
  where GCC provides them as an extension.
  See :ref:`variable-length`, for details.

.. function:: void *__builtin_alloca_with_align (size_t size, size_t alignment)

  The ``__builtin_alloca_with_align`` function must be called at block
  scope.  The function allocates an object :samp:`{size}` bytes large on
  the stack of the calling function.  The allocated object is aligned on
  the boundary specified by the argument :samp:`{alignment}` whose unit is given
  in bits (not bytes).  The :samp:`{size}` argument must be positive and not
  exceed the stack size limit.  The :samp:`{alignment}` argument must be a constant
  integer expression that evaluates to a power of 2 greater than or equal to
  ``CHAR_BIT`` and less than some unspecified maximum.  Invocations
  with other values are rejected with an error indicating the valid bounds.
  The function returns a pointer to the first byte of the allocated object.
  The lifetime of the allocated object ends at the end of the block in which
  the function was called.  The allocated storage is released no later than
  just before the calling function returns to its caller, but may be released
  at the end of the block in which the function was called.

  For example, in the following function the call to ``g`` is unsafe
  because when ``overalign`` is non-zero, the space allocated by
  ``__builtin_alloca_with_align`` may have been released at the end
  of the ``if`` statement in which it was called.

  .. code-block:: c++

    void f (unsigned n, bool overalign)
    {
      void *p;
      if (overalign)
        p = __builtin_alloca_with_align (n, 64 /* bits */);
      else
        p = __builtin_alloc (n);

      g (p, n);   // unsafe
    }

  Since the ``__builtin_alloca_with_align`` function doesn't validate its
  :samp:`{size}` argument it is the responsibility of its caller to make sure
  the argument doesn't cause it to exceed the stack size limit.
  The ``__builtin_alloca_with_align`` function is provided to make
  it possible to allocate on the stack overaligned arrays of bytes with
  an upper bound that may be computed at run time.  Since C99
  Variable Length Arrays offer the same functionality under
  a portable, more convenient, and safer interface they are recommended
  instead, in both C99 and C++ programs where GCC provides them as
  an extension.  See :ref:`variable-length`, for details.

.. function:: void *__builtin_alloca_with_align_and_max (size_t size, size_t alignment, size_t max_size)

  Similar to ``__builtin_alloca_with_align`` but takes an extra argument
  specifying an upper bound for :samp:`{size}` in case its value cannot be computed
  at compile time, for use by :option:`-fstack-usage`, :option:`-Wstack-usage`
  and :option:`-Walloca-larger-than`.  :samp:`{max_size}` must be a constant integer
  expression, it has no effect on code generation and no attempt is made to
  check its compatibility with :samp:`{size}`.

.. function:: bool __builtin_has_attribute (type_or_expression, attribute)

  The ``__builtin_has_attribute`` function evaluates to an integer constant
  expression equal to ``true`` if the symbol or type referenced by
  the :samp:`{type_or_expression}` argument has been declared with
  the :samp:`{attribute}` referenced by the second argument.  For
  an :samp:`{type_or_expression}` argument that does not reference a symbol,
  since attributes do not apply to expressions the built-in consider
  the type of the argument.  Neither argument is evaluated.
  The :samp:`{type_or_expression}` argument is subject to the same
  restrictions as the argument to ``typeof`` (see :ref:`typeof`).  The
  :samp:`{attribute}` argument is an attribute name optionally followed by
  a comma-separated list of arguments enclosed in parentheses.  Both forms
  of attribute names---with and without double leading and trailing
  underscores---are recognized.  See :ref:`attribute-syntax`, for details.
  When no attribute arguments are specified for an attribute that expects
  one or more arguments the function returns ``true`` if
  :samp:`{type_or_expression}` has been declared with the attribute regardless
  of the attribute argument values.  Arguments provided for an attribute
  that expects some are validated and matched up to the provided number.
  The function returns ``true`` if all provided arguments match.  For
  example, the first call to the function below evaluates to ``true``
  because ``x`` is declared with the :type-attr:`aligned` attribute but
  the second call evaluates to ``false`` because ``x`` is declared
  ``aligned (8)`` and not ``aligned (4)``.

  .. code-block:: c++

    __attribute__ ((aligned (8))) int x;
    _Static_assert (__builtin_has_attribute (x, aligned), "aligned");
    _Static_assert (!__builtin_has_attribute (x, aligned (4)), "aligned (4)");

  Due to a limitation the ``__builtin_has_attribute`` function returns
  ``false`` for the ``mode`` attribute even if the type or variable
  referenced by the :samp:`{type_or_expression}` argument was declared with one.
  The function is also not supported with labels, and in C with enumerators.

  Note that unlike the ``__has_attribute`` preprocessor operator which
  is suitable for use in ``#if`` preprocessing directives
  ``__builtin_has_attribute`` is an intrinsic function that is not
  recognized in such contexts.

.. function:: type __builtin_speculation_safe_value (type val, type failval)

  This built-in function can be used to help mitigate against unsafe
  speculative execution.  :samp:`{type}` may be any integral type or any
  pointer type.

  * If the CPU is not speculatively executing the code, then :samp:`{val}`
    is returned.

  * If the CPU is executing speculatively then either:

    * The function may cause execution to pause until it is known that the
      code is no-longer being executed speculatively (in which case
      :samp:`{val}` can be returned, as above); or

    * The function may use target-dependent speculation tracking state to cause
      :samp:`{failval}` to be returned when it is known that speculative
      execution has incorrectly predicted a conditional branch operation.

  The second argument, :samp:`{failval}`, is optional and defaults to zero
  if omitted.

  GCC defines the preprocessor macro
  ``__HAVE_BUILTIN_SPECULATION_SAFE_VALUE`` for targets that have been
  updated to support this builtin.

  The built-in function can be used where a variable appears to be used in a
  safe way, but the CPU, due to speculative execution may temporarily ignore
  the bounds checks.  Consider, for example, the following function:

  .. code-block:: c++

    int array[500];
    int f (unsigned untrusted_index)
    {
      if (untrusted_index < 500)
        return array[untrusted_index];
      return 0;
    }

  If the function is called repeatedly with ``untrusted_index`` less
  than the limit of 500, then a branch predictor will learn that the
  block of code that returns a value stored in ``array`` will be
  executed.  If the function is subsequently called with an
  out-of-range value it will still try to execute that block of code
  first until the CPU determines that the prediction was incorrect
  (the CPU will unwind any incorrect operations at that point).
  However, depending on how the result of the function is used, it might be
  possible to leave traces in the cache that can reveal what was stored
  at the out-of-bounds location.  The built-in function can be used to
  provide some protection against leaking data in this way by changing
  the code to:

  .. code-block:: c++

    int array[500];
    int f (unsigned untrusted_index)
    {
      if (untrusted_index < 500)
        return array[__builtin_speculation_safe_value (untrusted_index)];
      return 0;
    }

  The built-in function will either cause execution to stall until the
  conditional branch has been fully resolved, or it may permit
  speculative execution to continue, but using 0 instead of
  ``untrusted_value`` if that exceeds the limit.

  If accessing any memory location is potentially unsafe when speculative
  execution is incorrect, then the code can be rewritten as

  .. code-block:: c++

    int array[500];
    int f (unsigned untrusted_index)
    {
      if (untrusted_index < 500)
        return *__builtin_speculation_safe_value (&array[untrusted_index], NULL);
      return 0;
    }

  which will cause a ``NULL`` pointer to be used for the unsafe case.

.. function:: int __builtin_types_compatible_p (type1, type2)

  You can use the built-in function ``__builtin_types_compatible_p`` to
  determine whether two types are the same.

  This built-in function returns 1 if the unqualified versions of the
  types :samp:`{type1}` and :samp:`{type2}` (which are types, not expressions) are
  compatible, 0 otherwise.  The result of this built-in function can be
  used in integer constant expressions.

  This built-in function ignores top level qualifiers (e.g., ``const``,
  ``volatile``).  For example, ``int`` is equivalent to ``const
  int``.

  The type ``int[]`` and ``int[5]`` are compatible.  On the other
  hand, ``int`` and ``char *`` are not compatible, even if the size
  of their types, on the particular architecture are the same.  Also, the
  amount of pointer indirection is taken into account when determining
  similarity.  Consequently, ``short *`` is not similar to
  ``short **``.  Furthermore, two types that are typedefed are
  considered compatible if their underlying types are compatible.

  An ``enum`` type is not considered to be compatible with another
  ``enum`` type even if both are compatible with the same integer
  type; this is what the C standard specifies.
  For example, ``enum {foo, bar}`` is not similar to
  ``enum {hot, dog}``.

  You typically use this function in code whose execution varies
  depending on the arguments' types.  For example:

  .. code-block:: c++

    #define foo(x)                                                  \
      ({                                                           \
        typeof (x) tmp = (x);                                       \
        if (__builtin_types_compatible_p (typeof (x), long double)) \
          tmp = foo_long_double (tmp);                              \
        else if (__builtin_types_compatible_p (typeof (x), double)) \
          tmp = foo_double (tmp);                                   \
        else if (__builtin_types_compatible_p (typeof (x), float))  \
          tmp = foo_float (tmp);                                    \
        else                                                        \
          abort ();                                                 \
        tmp;                                                        \
      })

  .. note::

    This construct is only available for C.

.. function:: type __builtin_call_with_static_chain (call_exp, pointer_exp)

  The :samp:`{call_exp}` expression must be a function call, and the
  :samp:`{pointer_exp}` expression must be a pointer.  The :samp:`{pointer_exp}`
  is passed to the function call in the target's static chain location.
  The result of builtin is the result of the function call.

  .. note::

    This builtin is only available for C.

  This builtin can be used to call Go closures from C.

.. function:: type __builtin_choose_expr (const_exp, exp1, exp2)

  You can use the built-in function ``__builtin_choose_expr`` to
  evaluate code depending on the value of a constant expression.  This
  built-in function returns :samp:`{exp1}` if :samp:`{const_exp}`, which is an
  integer constant expression, is nonzero.  Otherwise it returns :samp:`{exp2}`.

  This built-in function is analogous to the :samp:`? :` operator in C,
  except that the expression returned has its type unaltered by promotion
  rules.  Also, the built-in function does not evaluate the expression
  that is not chosen.  For example, if :samp:`{const_exp}` evaluates to ``true``,
  :samp:`{exp2}` is not evaluated even if it has side effects.

  This built-in function can return an lvalue if the chosen argument is an
  lvalue.

  If :samp:`{exp1}` is returned, the return type is the same as :samp:`{exp1}` 's
  type.  Similarly, if :samp:`{exp2}` is returned, its return type is the same
  as :samp:`{exp2}`.

  Example:

  .. code-block:: c++

    #define foo(x)                                                    \
      __builtin_choose_expr (                                         \
        __builtin_types_compatible_p (typeof (x), double),            \
        foo_double (x),                                               \
        __builtin_choose_expr (                                       \
          __builtin_types_compatible_p (typeof (x), float),           \
          foo_float (x),                                              \
          /* The void expression results in a compile-time error  \
             when assigning the result to something.  */          \
          (void)0))

  .. note::

    This construct is only available for C.  Furthermore, the
    unused expression (:samp:`{exp1}` or :samp:`{exp2}` depending on the value of
    :samp:`{const_exp}`) may still generate syntax errors.  This may change in
    future revisions.

.. function:: type __builtin_tgmath (functions, arguments)

  The built-in function ``__builtin_tgmath``, available only for C
  and Objective-C, calls a function determined according to the rules of
  ``<tgmath.h>`` macros.  It is intended to be used in
  implementations of that header, so that expansions of macros from that
  header only expand each of their arguments once, to avoid problems
  when calls to such macros are nested inside the arguments of other
  calls to such macros; in addition, it results in better diagnostics
  for invalid calls to ``<tgmath.h>`` macros than implementations
  using other GNU C language features.  For example, the ``pow``
  type-generic macro might be defined as:

  .. code-block:: c++

    #define pow(a, b) __builtin_tgmath (powf, pow, powl, \
                                        cpowf, cpow, cpowl, a, b)

  The arguments to ``__builtin_tgmath`` are at least two pointers to
  functions, followed by the arguments to the type-generic macro (which
  will be passed as arguments to the selected function).  All the
  pointers to functions must be pointers to prototyped functions, none
  of which may have variable arguments, and all of which must have the
  same number of parameters; the number of parameters of the first
  function determines how many arguments to ``__builtin_tgmath`` are
  interpreted as function pointers, and how many as the arguments to the
  called function.

  The types of the specified functions must all be different, but
  related to each other in the same way as a set of functions that may
  be selected between by a macro in ``<tgmath.h>``.  This means that
  the functions are parameterized by a floating-point type :samp:`{t}`,
  different for each such function.  The function return types may all
  be the same type, or they may be :samp:`{t}` for each function, or they
  may be the real type corresponding to :samp:`{t}` for each function (if
  some of the types :samp:`{t}` are complex).  Likewise, for each parameter
  position, the type of the parameter in that position may always be the
  same type, or may be :samp:`{t}` for each function (this case must apply
  for at least one parameter position), or may be the real type
  corresponding to :samp:`{t}` for each function.

  The standard rules for ``<tgmath.h>`` macros are used to find a
  common type :samp:`{u}` from the types of the arguments for parameters
  whose types vary between the functions; complex integer types (a GNU
  extension) are treated like ``_Complex double`` for this purpose
  (or ``_Complex _Float64`` if all the function return types are the
  same ``_Floatn`` or ``_Floatnx`` type).
  If the function return types vary, or are all the same integer type,
  the function called is the one for which :samp:`{t}` is :samp:`{u}`, and it is
  an error if there is no such function.  If the function return types
  are all the same floating-point type, the type-generic macro is taken
  to be one of those from TS 18661 that rounds the result to a narrower
  type; if there is a function for which :samp:`{t}` is :samp:`{u}`, it is
  called, and otherwise the first function, if any, for which :samp:`{t}`
  has at least the range and precision of :samp:`{u}` is called, and it is
  an error if there is no such function.

.. function:: int __builtin_constant_p (exp)

  You can use the built-in function ``__builtin_constant_p`` to
  determine if a value is known to be constant at compile time and hence
  that GCC can perform constant-folding on expressions involving that
  value.  The argument of the function is the value to test.  The function
  returns the integer 1 if the argument is known to be a compile-time
  constant and 0 if it is not known to be a compile-time constant.  A
  return of 0 does not indicate that the value is *not* a constant,
  but merely that GCC cannot prove it is a constant with the specified
  value of the :option:`-O` option.

  You typically use this function in an embedded application where
  memory is a critical resource.  If you have some complex calculation,
  you may want it to be folded if it involves constants, but need to call
  a function if it does not.  For example:

  .. code-block:: c++

    #define Scale_Value(X)      \
      (__builtin_constant_p (X) \
      ? ((X) * SCALE + OFFSET) : Scale (X))

  You may use this built-in function in either a macro or an inline
  function.  However, if you use it in an inlined function and pass an
  argument of the function as the argument to the built-in, GCC
  never returns 1 when you call the inline function with a string constant
  or compound literal (see :ref:`compound-literals`) and does not return 1
  when you pass a constant numeric value to the inline function unless you
  specify the :option:`-O` option.

  You may also use ``__builtin_constant_p`` in initializers for static
  data.  For instance, you can write

  .. code-block:: c++

    static const int table[] = {
       __builtin_constant_p (EXPRESSION) ? (EXPRESSION) : -1,
       /* ... */
    };

  This is an acceptable initializer even if :samp:`{EXPRESSION}` is not a
  constant expression, including the case where
  ``__builtin_constant_p`` returns 1 because :samp:`{EXPRESSION}` can be
  folded to a constant but :samp:`{EXPRESSION}` contains operands that are
  not otherwise permitted in a static initializer (for example,
  ``0 && foo ()``).  GCC must be more conservative about evaluating the
  built-in in this case, because it has no opportunity to perform
  optimization.

.. function:: bool __builtin_is_constant_evaluated (void)

  The ``__builtin_is_constant_evaluated`` function is available only
  in C++.  The built-in is intended to be used by implementations of
  the ``std::is_constant_evaluated`` C++ function.  Programs should make
  use of the latter function rather than invoking the built-in directly.

  The main use case of the built-in is to determine whether a ``constexpr``
  function is being called in a ``constexpr`` context.  A call to
  the function evaluates to a core constant expression with the value
  ``true`` if and only if it occurs within the evaluation of an expression
  or conversion that is manifestly constant-evaluated as defined in the C++
  standard.  Manifestly constant-evaluated contexts include constant-expressions,
  the conditions of ``constexpr if`` statements, constraint-expressions, and
  initializers of variables usable in constant expressions.   For more details
  refer to the latest revision of the C++ standard.

.. function:: void __builtin_clear_padding (ptr)

  The built-in function ``__builtin_clear_padding`` function clears
  padding bits inside of the object representation of object pointed by
  :samp:`{ptr}`, which has to be a pointer.  The value representation of the
  object is not affected.  The type of the object is assumed to be the type
  the pointer points to.  Inside of a union, the only cleared bits are
  bits that are padding bits for all the union members.

  This built-in-function is useful if the padding bits of an object might
  have intederminate values and the object representation needs to be
  bitwise compared to some other object, for example for atomic operations.

  For C++, :samp:`{ptr}` argument type should be pointer to trivially-copyable
  type, unless the argument is address of a variable or parameter, because
  otherwise it isn't known if the type isn't just a base class whose padding
  bits are reused or laid out differently in a derived class.

.. function:: type __builtin_bit_cast (type, arg)

  The ``__builtin_bit_cast`` function is available only
  in C++.  The built-in is intended to be used by implementations of
  the ``std::bit_cast`` C++ template function.  Programs should make
  use of the latter function rather than invoking the built-in directly.

  This built-in function allows reinterpreting the bits of the :samp:`{arg}`
  argument as if it had type :samp:`{type}`.  :samp:`{type}` and the type of the
  :samp:`{arg}` argument need to be trivially copyable types with the same size.
  When manifestly constant-evaluated, it performs extra diagnostics required
  for ``std::bit_cast`` and returns a constant expression if :samp:`{arg}`
  is a constant expression.  For more details
  refer to the latest revision of the C++ standard.

.. function:: long __builtin_expect (long exp, long c)

  .. index:: fprofile-arcs

  You may use ``__builtin_expect`` to provide the compiler with
  branch prediction information.  In general, you should prefer to
  use actual profile feedback for this (:option:`-fprofile-arcs`), as
  programmers are notoriously bad at predicting how their programs
  actually perform.  However, there are applications in which this
  data is hard to collect.

  The return value is the value of :samp:`{exp}`, which should be an integral
  expression.  The semantics of the built-in are that it is expected that
  :samp:`{exp}` == :samp:`{c}`.  For example:

  .. code-block:: c++

    if (__builtin_expect (x, 0))
      foo ();

  indicates that we do not expect to call ``foo``, since
  we expect ``x`` to be zero.  Since you are limited to integral
  expressions for :samp:`{exp}`, you should use constructions such as

  .. code-block:: c++

    if (__builtin_expect (ptr != NULL, 1))
      foo (*ptr);

  when testing pointer or floating-point values.

  For the purposes of branch prediction optimizations, the probability that
  a ``__builtin_expect`` expression is ``true`` is controlled by GCC's
  ``builtin-expect-probability`` parameter, which defaults to 90%.

  You can also use ``__builtin_expect_with_probability`` to explicitly
  assign a probability value to individual expressions.  If the built-in
  is used in a loop construct, the provided probability will influence
  the expected number of iterations made by loop optimizations.

.. function:: long __builtin_expect_with_probability (long exp, long c, double probability)

  This function has the same semantics as ``__builtin_expect``,
  but the caller provides the expected probability that :samp:`{exp}` == :samp:`{c}`.
  The last argument, :samp:`{probability}`, is a floating-point value in the
  range 0.0 to 1.0, inclusive.  The :samp:`{probability}` argument must be
  constant floating-point expression.

.. function:: void __builtin_trap (void)

  This function causes the program to exit abnormally.  GCC implements
  this function by using a target-dependent mechanism (such as
  intentionally executing an illegal instruction) or by calling
  ``abort``.  The mechanism used may vary from release to release so
  you should not rely on any particular implementation.

.. function:: void __builtin_unreachable (void)

  If control flow reaches the point of the ``__builtin_unreachable``,
  the program is undefined.  It is useful in situations where the
  compiler cannot deduce the unreachability of the code.

  One such case is immediately following an ``asm`` statement that
  either never terminates, or one that transfers control elsewhere
  and never returns.  In this example, without the
  ``__builtin_unreachable``, GCC issues a warning that control
  reaches the end of a non-void function.  It also generates code
  to return after the ``asm``.

  .. code-block:: c++

    int f (int c, int v)
    {
      if (c)
        {
          return v;
        }
      else
        {
          asm("jmp error_handler");
          __builtin_unreachable ();
        }
    }

  Because the ``asm`` statement unconditionally transfers control out
  of the function, control never reaches the end of the function
  body.  The ``__builtin_unreachable`` is in fact unreachable and
  communicates this fact to the compiler.

  Another use for ``__builtin_unreachable`` is following a call a
  function that never returns but that is not declared
  ``__attribute__((noreturn))``, as in this example:

  .. code-block:: c++

    void function_that_never_returns (void);

    int g (int c)
    {
      if (c)
        {
          return 1;
        }
      else
        {
          function_that_never_returns ();
          __builtin_unreachable ();
        }
    }

.. function:: type __builtin_assoc_barrier (type expr)

  This built-in inhibits re-association of the floating-point expression
  :samp:`{expr}` with expressions consuming the return value of the built-in. The
  expression :samp:`{expr}` itself can be reordered, and the whole expression
  :samp:`{expr}` can be reordered with operands after the barrier. The barrier is
  only relevant when ``-fassociative-math`` is active, since otherwise
  floating-point is not treated as associative.

  .. code-block:: c++

    float x0 = a + b - b;
    float x1 = __builtin_assoc_barrier(a + b) - b;

  means that, with ``-fassociative-math``, ``x0`` can be optimized to
  ``x0 = a`` but ``x1`` cannot.

.. function:: void * __builtin_assume_aligned (const void *exp, size_t align, ...)

  This function returns its first argument, and allows the compiler
  to assume that the returned pointer is at least :samp:`{align}` bytes
  aligned.  This built-in can have either two or three arguments,
  if it has three, the third argument should have integer type, and
  if it is nonzero means misalignment offset.  For example:

  .. code-block:: c++

    void *x = __builtin_assume_aligned (arg, 16);

  means that the compiler can assume ``x``, set to ``arg``, is at least
  16-byte aligned, while:

  .. code-block:: c++

    void *x = __builtin_assume_aligned (arg, 32, 8);

  means that the compiler can assume for ``x``, set to ``arg``, that
  ``(char *) x - 8`` is 32-byte aligned.

.. function:: int __builtin_LINE ()

  This function is the equivalent of the preprocessor ``__LINE__``
  macro and returns a constant integer expression that evaluates to
  the line number of the invocation of the built-in.  When used as a C++
  default argument for a function :samp:`{F}`, it returns the line number
  of the call to :samp:`{F}`.

.. function:: const char * __builtin_FUNCTION ()

  This function is the equivalent of the ``__FUNCTION__`` symbol
  and returns an address constant pointing to the name of the function
  from which the built-in was invoked, or the empty string if
  the invocation is not at function scope.  When used as a C++ default
  argument for a function :samp:`{F}`, it returns the name of :samp:`{F}` 's
  caller or the empty string if the call was not made at function
  scope.

.. function:: const char * __builtin_FILE ()

  This function is the equivalent of the preprocessor ``__FILE__``
  macro and returns an address constant pointing to the file name
  containing the invocation of the built-in, or the empty string if
  the invocation is not at function scope.  When used as a C++ default
  argument for a function :samp:`{F}`, it returns the file name of the call
  to :samp:`{F}` or the empty string if the call was not made at function
  scope.

  For example, in the following, each call to function ``foo`` will
  print a line similar to ``"file.c:123: foo: message"`` with the name
  of the file and the line number of the ``printf`` call, the name of
  the function ``foo``, followed by the word ``message``.

  .. code-block:: c++

    const char*
    function (const char *func = __builtin_FUNCTION ())
    {
      return func;
    }

    void foo (void)
    {
      printf ("%s:%i: %s: message\n", file (), line (), function ());
    }

.. function:: void __builtin___clear_cache (void *begin, void *end)

  This function is used to flush the processor's instruction cache for
  the region of memory between :samp:`{begin}` inclusive and :samp:`{end}`
  exclusive.  Some targets require that the instruction cache be
  flushed, after modifying memory containing code, in order to obtain
  deterministic behavior.

  If the target does not require instruction cache flushes,
  ``__builtin___clear_cache`` has no effect.  Otherwise either
  instructions are emitted in-line to clear the instruction cache or a
  call to the ``__clear_cache`` function in libgcc is made.

.. function:: void __builtin_prefetch (const void *addr, ...)

  This function is used to minimize cache-miss latency by moving data into
  a cache before it is accessed.
  You can insert calls to ``__builtin_prefetch`` into code for which
  you know addresses of data in memory that is likely to be accessed soon.
  If the target supports them, data prefetch instructions are generated.
  If the prefetch is done early enough before the access then the data will
  be in the cache by the time it is accessed.

  The value of :samp:`{addr}` is the address of the memory to prefetch.
  There are two optional arguments, :samp:`{rw}` and :samp:`{locality}`.
  The value of :samp:`{rw}` is a compile-time constant one or zero; one
  means that the prefetch is preparing for a write to the memory address
  and zero, the default, means that the prefetch is preparing for a read.
  The value :samp:`{locality}` must be a compile-time constant integer between
  zero and three.  A value of zero means that the data has no temporal
  locality, so it need not be left in the cache after the access.  A value
  of three means that the data has a high degree of temporal locality and
  should be left in all levels of cache possible.  Values of one and two
  mean, respectively, a low or moderate degree of temporal locality.  The
  default is three.

  .. code-block:: c++

    for (i = 0; i < n; i++)
      {
        a[i] = a[i] + b[i];
        __builtin_prefetch (&a[i+j], 1, 1);
        __builtin_prefetch (&b[i+j], 0, 1);
        /* ... */
      }

  Data prefetch does not generate faults if :samp:`{addr}` is invalid, but
  the address expression itself must be valid.  For example, a prefetch
  of ``p->next`` does not fault if ``p->next`` is not a valid
  address, but evaluation faults if ``p`` is not a valid address.

  If the target does not support data prefetch, the address expression
  is evaluated if it includes side effects but no other code is generated
  and GCC does not issue a warning.

.. function:: size_t __builtin_object_size (const void * ptr, int type)

  Returns the size of an object pointed to by :samp:`{ptr}`.  See :ref:`object-size-checking`, for a detailed description of the function.

.. function:: double __builtin_huge_val (void)

  Returns a positive infinity, if supported by the floating-point format,
  else ``DBL_MAX``.  This function is suitable for implementing the
  ISO C macro ``HUGE_VAL``.

.. function:: float __builtin_huge_valf (void)

  Similar to ``__builtin_huge_val``, except the return type is ``float``.

.. function:: long double __builtin_huge_vall (void)

  Similar to ``__builtin_huge_val``, except the return
  type is ``long double``.

.. function:: _Floatn __builtin_huge_valfn (void)

  Similar to ``__builtin_huge_val``, except the return type is
  ``_Floatn``.

.. function:: _Floatnx __builtin_huge_valfnx (void)

  Similar to ``__builtin_huge_val``, except the return type is
  ``_Floatnx``.

.. function:: int __builtin_fpclassify (int, int, int, int, int, ...)

  This built-in implements the C99 fpclassify functionality.  The first
  five int arguments should be the target library's notion of the
  possible FP classes and are used for return values.  They must be
  constant values and they must appear in this order: ``FP_NAN``,
  ``FP_INFINITE``, ``FP_NORMAL``, ``FP_SUBNORMAL`` and
  ``FP_ZERO``.  The ellipsis is for exactly one floating-point value
  to classify.  GCC treats the last argument as type-generic, which
  means it does not do default promotion from float to double.

.. function:: double __builtin_inf (void)

  Similar to ``__builtin_huge_val``, except a warning is generated
  if the target floating-point format does not support infinities.

.. function:: _Decimal32 __builtin_infd32 (void)

  Similar to ``__builtin_inf``, except the return type is ``_Decimal32``.

.. function:: _Decimal64 __builtin_infd64 (void)

  Similar to ``__builtin_inf``, except the return type is ``_Decimal64``.

.. function:: _Decimal128 __builtin_infd128 (void)

  Similar to ``__builtin_inf``, except the return type is ``_Decimal128``.

.. function:: float __builtin_inff (void)

  Similar to ``__builtin_inf``, except the return type is ``float``.
  This function is suitable for implementing the ISO C99 macro ``INFINITY``.

.. function:: long double __builtin_infl (void)

  Similar to ``__builtin_inf``, except the return
  type is ``long double``.

.. function:: _Floatn __builtin_inffn (void)

  Similar to ``__builtin_inf``, except the return
  type is ``_Floatn``.

.. function:: _Floatn __builtin_inffnx (void)

  Similar to ``__builtin_inf``, except the return
  type is ``_Floatnx``.

.. function:: int __builtin_isinf_sign (...)

  Similar to ``isinf``, except the return value is -1 for
  an argument of ``-Inf`` and 1 for an argument of ``+Inf``.
  Note while the parameter list is an
  ellipsis, this function only accepts exactly one floating-point
  argument.  GCC treats this parameter as type-generic, which means it
  does not do default promotion from float to double.

.. function:: double __builtin_nan (const char *str)

  This is an implementation of the ISO C99 function ``nan``.

  Since ISO C99 defines this function in terms of ``strtod``, which we
  do not implement, a description of the parsing is in order.  The string
  is parsed as by ``strtol`` ; that is, the base is recognized by
  leading :samp:`0` or :samp:`0x` prefixes.  The number parsed is placed
  in the significand such that the least significant bit of the number
  is at the least significant bit of the significand.  The number is
  truncated to fit the significand field provided.  The significand is
  forced to be a quiet NaN.

  This function, if given a string literal all of which would have been
  consumed by ``strtol``, is evaluated early enough that it is considered a
  compile-time constant.

.. function:: _Decimal32 __builtin_nand32 (const char *str)

  Similar to ``__builtin_nan``, except the return type is ``_Decimal32``.

.. function:: _Decimal64 __builtin_nand64 (const char *str)

  Similar to ``__builtin_nan``, except the return type is ``_Decimal64``.

.. function:: _Decimal128 __builtin_nand128 (const char *str)

  Similar to ``__builtin_nan``, except the return type is ``_Decimal128``.

.. function:: float __builtin_nanf (const char *str)

  Similar to ``__builtin_nan``, except the return type is ``float``.

.. function:: long double __builtin_nanl (const char *str)

  Similar to ``__builtin_nan``, except the return type is ``long double``.

.. function:: _Floatn __builtin_nanfn (const char *str)

  Similar to ``__builtin_nan``, except the return type is
  ``_Floatn``.

.. function:: _Floatnx __builtin_nanfnx (const char *str)

  Similar to ``__builtin_nan``, except the return type is
  ``_Floatnx``.

.. function:: double __builtin_nans (const char *str)

  Similar to ``__builtin_nan``, except the significand is forced
  to be a signaling NaN.  The ``nans`` function is proposed by
  `WG14 N965 <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n965.htm>`_.

.. function:: _Decimal32 __builtin_nansd32 (const char *str)

  Similar to ``__builtin_nans``, except the return type is ``_Decimal32``.

.. function:: _Decimal64 __builtin_nansd64 (const char *str)

  Similar to ``__builtin_nans``, except the return type is ``_Decimal64``.

.. function:: _Decimal128 __builtin_nansd128 (const char *str)

  Similar to ``__builtin_nans``, except the return type is ``_Decimal128``.

.. function:: float __builtin_nansf (const char *str)

  Similar to ``__builtin_nans``, except the return type is ``float``.

.. function:: long double __builtin_nansl (const char *str)

  Similar to ``__builtin_nans``, except the return type is ``long double``.

.. function:: _Floatn __builtin_nansfn (const char *str)

  Similar to ``__builtin_nans``, except the return type is
  ``_Floatn``.

.. function:: _Floatnx __builtin_nansfnx (const char *str)

  Similar to ``__builtin_nans``, except the return type is
  ``_Floatnx``.

.. function:: int __builtin_issignaling (...)

  Return non-zero if the argument is a signaling NaN and zero otherwise.
  Note while the parameter list is an
  ellipsis, this function only accepts exactly one floating-point
  argument.  GCC treats this parameter as type-generic, which means it
  does not do default promotion from float to double.
  This built-in function can work even without the non-default
  ``-fsignaling-nans`` option, although if a signaling NaN is computed,
  stored or passed as argument to some function other than this built-in
  in the current translation unit, it is safer to use ``-fsignaling-nans``.
  With ``-ffinite-math-only`` option this built-in function will always
  return 0.

.. function:: int __builtin_ffs (int x)

  Returns one plus the index of the least significant 1-bit of :samp:`{x}`, or
  if :samp:`{x}` is zero, returns zero.

.. function:: int __builtin_clz (unsigned int x)

  Returns the number of leading 0-bits in :samp:`{x}`, starting at the most
  significant bit position.  If :samp:`{x}` is 0, the result is undefined.

.. function:: int __builtin_ctz (unsigned int x)

  Returns the number of trailing 0-bits in :samp:`{x}`, starting at the least
  significant bit position.  If :samp:`{x}` is 0, the result is undefined.

.. function:: int __builtin_clrsb (int x)

  Returns the number of leading redundant sign bits in :samp:`{x}`, i.e. the
  number of bits following the most significant bit that are identical
  to it.  There are no special cases for 0 or other values.

.. function:: int __builtin_popcount (unsigned int x)

  Returns the number of 1-bits in :samp:`{x}`.

.. function:: int __builtin_parity (unsigned int x)

  Returns the parity of :samp:`{x}`, i.e. the number of 1-bits in :samp:`{x}`
  modulo 2.

.. function:: int __builtin_ffsl (long)

  Similar to ``__builtin_ffs``, except the argument type is
  ``long``.

.. function:: int __builtin_clzl (unsigned long)

  Similar to ``__builtin_clz``, except the argument type is
  ``unsigned long``.

.. function:: int __builtin_ctzl (unsigned long)

  Similar to ``__builtin_ctz``, except the argument type is
  ``unsigned long``.

.. function:: int __builtin_clrsbl (long)

  Similar to ``__builtin_clrsb``, except the argument type is
  ``long``.

.. function:: int __builtin_popcountl (unsigned long)

  Similar to ``__builtin_popcount``, except the argument type is
  ``unsigned long``.

.. function:: int __builtin_parityl (unsigned long)

  Similar to ``__builtin_parity``, except the argument type is
  ``unsigned long``.

.. function:: int __builtin_ffsll (long long)

  Similar to ``__builtin_ffs``, except the argument type is
  ``long long``.

.. function:: int __builtin_clzll (unsigned long long)

  Similar to ``__builtin_clz``, except the argument type is
  ``unsigned long long``.

.. function:: int __builtin_ctzll (unsigned long long)

  Similar to ``__builtin_ctz``, except the argument type is
  ``unsigned long long``.

.. function:: int __builtin_clrsbll (long long)

  Similar to ``__builtin_clrsb``, except the argument type is
  ``long long``.

.. function:: int __builtin_popcountll (unsigned long long)

  Similar to ``__builtin_popcount``, except the argument type is
  ``unsigned long long``.

.. function:: int __builtin_parityll (unsigned long long)

  Similar to ``__builtin_parity``, except the argument type is
  ``unsigned long long``.

.. function:: double __builtin_powi (double, int)

  Returns the first argument raised to the power of the second.  Unlike the
  ``pow`` function no guarantees about precision and rounding are made.

.. function:: float __builtin_powif (float, int)

  Similar to ``__builtin_powi``, except the argument and return types
  are ``float``.

.. function:: long double __builtin_powil (long double, int)

  Similar to ``__builtin_powi``, except the argument and return types
  are ``long double``.

.. function:: uint16_t __builtin_bswap16 (uint16_t x)

  Returns :samp:`{x}` with the order of the bytes reversed; for example,
  ``0xaabb`` becomes ``0xbbaa``.  Byte here always means
  exactly 8 bits.

.. function:: uint32_t __builtin_bswap32 (uint32_t x)

  Similar to ``__builtin_bswap16``, except the argument and return types
  are 32-bit.

.. function:: uint64_t __builtin_bswap64 (uint64_t x)

  Similar to ``__builtin_bswap32``, except the argument and return types
  are 64-bit.

.. function:: uint128_t __builtin_bswap128 (uint128_t x)

  Similar to ``__builtin_bswap64``, except the argument and return types
  are 128-bit.  Only supported on targets when 128-bit types are supported.

.. function:: Pmode __builtin_extend_pointer (void * x)

  On targets where the user visible pointer size is smaller than the size
  of an actual hardware address this function returns the extended user
  pointer.  Targets where this is true included ILP32 mode on x86_64 or
  Aarch64.  This function is mainly useful when writing inline assembly
  code.

.. function:: int __builtin_goacc_parlevel_id (int x)

  Returns the openacc gang, worker or vector id depending on whether :samp:`{x}` is
  0, 1 or 2.

.. function:: int __builtin_goacc_parlevel_size (int x)

  Returns the openacc gang, worker or vector size depending on whether :samp:`{x}` is
  0, 1 or 2.