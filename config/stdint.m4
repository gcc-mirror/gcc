AC_DEFUN([GCC_STDINT_TYPES],
[AC_REQUIRE([AC_TYPE_INT8_T])
AC_REQUIRE([AC_TYPE_INT16_T])
AC_REQUIRE([AC_TYPE_INT32_T])
AC_REQUIRE([AC_TYPE_INT64_T])
AC_REQUIRE([AC_TYPE_INTMAX_T])
AC_REQUIRE([AC_TYPE_INTPTR_T])
AC_REQUIRE([AC_TYPE_UINT8_T])
AC_REQUIRE([AC_TYPE_UINT16_T])
AC_REQUIRE([AC_TYPE_UINT32_T])
AC_REQUIRE([AC_TYPE_UINT64_T])
AC_REQUIRE([AC_TYPE_UINTMAX_T])
AC_REQUIRE([AC_TYPE_UINTPTR_T])])

AC_DEFUN([GCC_HEADER_STDINT],
[AC_REQUIRE([GCC_STDINT_TYPES])
AC_CHECK_TYPES([int_least32_t, int_fast32_t],,,[#include <sys/types.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif])

# ----------------- Summarize what we found so far

m4_define([_GCC_STDINT_H], m4_ifval($1, $1, _stdint.h))
m4_if(m4_bmatch(m4_quote(/_GCC_STDINT_H),
                /stdint\.h$, bad,
                /inttypes\.h$, bad, ok), bad,
      [m4_fatal([cannot overwrite ]m4_quote(_GCC_STDINT_H))])

# ----------------- done all checks, emit header -------------
AC_CONFIG_COMMANDS(_GCC_STDINT_H, [
cat >> tmp-stdint.h <<EOF
#ifndef GCC_GENERATED_STDINT_H
#define GCC_GENERATED_STDINT_H 1

#include "config.h"
#include <sys/types.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
EOF

if test "$ac_cv_type_int_least32_t" != yes; then
  sed 's/^ *//' >> tmp-stdint.h <<EOF

    /* Define int_least types */
    typedef int8_t     int_least8_t;
    typedef int16_t    int_least16_t;
    typedef int32_t    int_least32_t;
    typedef int64_t    int_least64_t;

    typedef uint8_t    uint_least8_t;
    typedef uint16_t   uint_least16_t;
    typedef uint32_t   uint_least32_t;
    typedef uint64_t   uint_least64_t;
EOF
fi

if test "$ac_cv_type_int_fast32_t" != yes; then
  dnl NOTE: The following code assumes that sizeof (int) > 1.
  dnl Fix when strange machines are reported.
  sed 's/^ *//' >> tmp-stdint.h <<EOF

    /* Define int_fast types.  short is often slow */
    typedef int8_t       int_fast8_t;
    typedef int          int_fast16_t;
    typedef int32_t      int_fast32_t;
    typedef int64_t      int_fast64_t;

    typedef uint8_t      uint_fast8_t;
    typedef unsigned int uint_fast16_t;
    typedef uint32_t     uint_fast32_t;
    typedef uint64_t     uint_fast64_t;
EOF
fi

echo '#endif /* GCC_GENERATED_STDINT_H */' >> tmp-stdint.h

if test -r ]_GCC_STDINT_H[ && cmp -s tmp-stdint.h ]_GCC_STDINT_H[; then
  rm -f tmp-stdint.h
else
  mv -f tmp-stdint.h ]_GCC_STDINT_H[
fi

], [
ac_cv_type_int_least32_t="$ac_cv_type_int_least32_t"
ac_cv_type_int_fast32_t="$ac_cv_type_int_fast32_t"
])

])
