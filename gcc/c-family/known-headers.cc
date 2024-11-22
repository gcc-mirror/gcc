/* Support for suggestions about missing #include directives.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "c-family/c-common.h"
#include "c-family/name-hint.h"
#include "c-family/known-headers.h"
#include "gcc-rich-location.h"

/* An enum for distinguishing between the C and C++ stdlibs.  */

enum stdlib
{
  STDLIB_C,
  STDLIB_CPLUSPLUS,

  NUM_STDLIBS
};

/* A struct for associating names in a standard library with the header
   that should be included to locate them, for each of the C and C++ stdlibs
   (or NULL, for names that aren't in a header for a particular stdlib).  */

struct stdlib_hint
{
  const char *name;
  const char *header[NUM_STDLIBS];
};

/* Given non-NULL NAME, return the header name defining it (as literal
   string) within either the standard library (with '<' and '>'), or
   NULL.

   Only handle string macros, so that this can be used for
   get_stdlib_header_for_name and
   get_c_stdlib_header_for_string_macro_name.  */

static const char *
get_string_macro_hint (const char *name, enum stdlib lib)
{
  /* <inttypes.h> and <cinttypes>.  */
  static const char *c99_cxx11_macros[] =
    { "PRId8", "PRId16", "PRId32", "PRId64",
      "PRIi8", "PRIi16", "PRIi32", "PRIi64",
      "PRIo8", "PRIo16", "PRIo32", "PRIo64",
      "PRIu8", "PRIu16", "PRIu32", "PRIu64",
      "PRIx8", "PRIx16", "PRIx32", "PRIx64",
      "PRIX8", "PRIX16", "PRIX32", "PRIX64",

      "PRIdPTR", "PRIiPTR", "PRIoPTR", "PRIuPTR", "PRIxPTR", "PRIXPTR",

      "SCNd8", "SCNd16", "SCNd32", "SCNd64",
      "SCNi8", "SCNi16", "SCNi32", "SCNi64",
      "SCNo8", "SCNo16", "SCNo32", "SCNo64",
      "SCNu8", "SCNu16", "SCNu32", "SCNu64",
      "SCNx8", "SCNx16", "SCNx32", "SCNx64",

      "SCNdPTR", "SCNiPTR", "SCNoPTR", "SCNuPTR", "SCNxPTR" };

  if ((lib == STDLIB_C && flag_isoc99)
      || (lib == STDLIB_CPLUSPLUS && cxx_dialect >= cxx11 ))
    {
      const size_t num_c99_cxx11_macros = ARRAY_SIZE (c99_cxx11_macros);
      for (size_t i = 0; i < num_c99_cxx11_macros; i++)
	if (strcmp (name, c99_cxx11_macros[i]) == 0)
	  return lib == STDLIB_C ? "<inttypes.h>" : "<cinttypes>";
    }

  return NULL;
}

/* Given non-NULL NAME, return the header name defining it within either
   the standard library (with '<' and '>'), or NULL.
   Only handles a subset of the most common names within the stdlibs.  */

static const char *
get_stdlib_header_for_name (const char *name, enum stdlib lib)
{
  gcc_assert (name);
  gcc_assert (lib < NUM_STDLIBS);

  static const stdlib_hint hints[] = {
    /* <assert.h> and <cassert>.  */
    {"assert", {"<assert.h>",  "<cassert>"} },

    /* <errno.h> and <cerrno>.  */
    {"errno", {"<errno.h>", "<cerrno>"} },

    /* <limits.h> and <climits>.  */
    {"CHAR_BIT", {"<limits.h>", "<climits>"} },
    {"CHAR_MAX", {"<limits.h>", "<climits>"} },
    {"CHAR_MIN", {"<limits.h>", "<climits>"} },
    {"INT_MAX", {"<limits.h>", "<climits>"} },
    {"INT_MIN", {"<limits.h>", "<climits>"} },
    {"LLONG_MAX", {"<limits.h>", "<climits>"} },
    {"LLONG_MIN", {"<limits.h>", "<climits>"} },
    {"LONG_MAX", {"<limits.h>", "<climits>"} },
    {"LONG_MIN", {"<limits.h>", "<climits>"} },
    {"MB_LEN_MAX", {"<limits.h>", "<climits>"} },
    {"SCHAR_MAX", {"<limits.h>", "<climits>"} },
    {"SCHAR_MIN", {"<limits.h>", "<climits>"} },
    {"SHRT_MAX", {"<limits.h>", "<climits>"} },
    {"SHRT_MIN", {"<limits.h>", "<climits>"} },
    {"UCHAR_MAX", {"<limits.h>", "<climits>"} },
    {"UINT_MAX", {"<limits.h>", "<climits>"} },
    {"ULLONG_MAX", {"<limits.h>", "<climits>"} },
    {"ULONG_MAX", {"<limits.h>", "<climits>"} },
    {"USHRT_MAX", {"<limits.h>", "<climits>"} },

    /* <float.h> and <cfloat>.  */
    {"DBL_MAX", {"<float.h>", "<cfloat>"} },
    {"DBL_MIN", {"<float.h>", "<cfloat>"} },
    {"FLT_MAX", {"<float.h>", "<cfloat>"} },
    {"FLT_MIN", {"<float.h>", "<cfloat>"} },
    {"LDBL_MAX", {"<float.h>", "<cfloat>"} },
    {"LDBL_MIN", {"<float.h>", "<cfloat>"} },

    /* <stdarg.h> and <cstdarg>.  */
    {"va_list", {"<stdarg.h>", "<cstdarg>"} },

    /* <stddef.h> and <cstddef>.  */
    {"NULL", {"<stddef.h>", "<cstddef>"} },
    {"nullptr_t", {NULL, "<cstddef>"} },
    {"offsetof", {"<stddef.h>", "<cstddef>"} },
    {"ptrdiff_t", {"<stddef.h>", "<cstddef>"} },
    {"size_t", {"<stddef.h>", "<cstddef>"} },
    {"wchar_t", {"<stddef.h>", NULL /* a keyword in C++ */} },

    /* <stdio.h> and <cstdio>.  */
    {"BUFSIZ", {"<stdio.h>", "<cstdio>"} },
    {"EOF", {"<stdio.h>", "<cstdio>"} },
    {"FILE", {"<stdio.h>", "<cstdio>"} },
    {"FILENAME_MAX", {"<stdio.h>", "<cstdio>"} },
    {"fopen", {"<stdio.h>", "<cstdio>"} },
    {"fpos_t", {"<stdio.h>", "<cstdio>"} },
    {"getchar", {"<stdio.h>", "<cstdio>"} },
    {"printf", {"<stdio.h>", "<cstdio>"} },
    {"snprintf", {"<stdio.h>", "<cstdio>"} },
    {"sprintf", {"<stdio.h>", "<cstdio>"} },
    {"stderr", {"<stdio.h>", "<cstdio>"} },
    {"stdin", {"<stdio.h>", "<cstdio>"} },
    {"stdout", {"<stdio.h>", "<cstdio>"} },

    /* <stdlib.h> and <cstdlib>.  */
    {"EXIT_FAILURE", {"<stdlib.h>", "<cstdlib>"} },
    {"EXIT_SUCCESS", {"<stdlib.h>", "<cstdlib>"} },
    {"abort", {"<stdlib.h>", "<cstdlib>"} },
    {"atexit", {"<stdlib.h>", "<cstdlib>"} },
    {"calloc", {"<stdlib.h>", "<cstdlib>"} },
    {"exit", {"<stdlib.h>", "<cstdlib>"} },
    {"free", {"<stdlib.h>", "<cstdlib>"} },
    {"getenv", {"<stdlib.h>", "<cstdlib>"} },
    {"malloc", {"<stdlib.h>", "<cstdlib>"} },
    {"realloc", {"<stdlib.h>", "<cstdlib>"} },

    /* <string.h> and <cstring>.  */
    {"memchr", {"<string.h>", "<cstring>"} },
    {"memcmp", {"<string.h>", "<cstring>"} },
    {"memcpy", {"<string.h>", "<cstring>"} },
    {"memmove", {"<string.h>", "<cstring>"} },
    {"memset", {"<string.h>", "<cstring>"} },
    {"strcat", {"<string.h>", "<cstring>"} },
    {"strchr", {"<string.h>", "<cstring>"} },
    {"strcmp", {"<string.h>", "<cstring>"} },
    {"strcpy", {"<string.h>", "<cstring>"} },
    {"strerror", {"<string.h>", "<cstring>"} },
    {"strlen", {"<string.h>", "<cstring>"} },
    {"strncat", {"<string.h>", "<cstring>"} },
    {"strncmp", {"<string.h>", "<cstring>"} },
    {"strncpy", {"<string.h>", "<cstring>"} },
    {"strrchr", {"<string.h>", "<cstring>"} },
    {"strspn", {"<string.h>", "<cstring>"} },
    {"strstr", {"<string.h>", "<cstring>"} },

    /* <stdint.h>.  */
    {"PTRDIFF_MAX", {"<stdint.h>", "<cstdint>"} },
    {"PTRDIFF_MIN", {"<stdint.h>", "<cstdint>"} },
    {"SIG_ATOMIC_MAX", {"<stdint.h>", "<cstdint>"} },
    {"SIG_ATOMIC_MIN", {"<stdint.h>", "<cstdint>"} },
    {"SIZE_MAX", {"<stdint.h>", "<cstdint>"} },
    {"WINT_MAX", {"<stdint.h>", "<cstdint>"} },
    {"WINT_MIN", {"<stdint.h>", "<cstdint>"} },

    /* <time.h>.  */
    {"asctime", {"<time.h>", "<ctime>"} },
    {"clock", {"<time.h>", "<ctime>"} },
    {"clock_t", {"<time.h>", "<ctime>"} },
    {"ctime", {"<time.h>", "<ctime>"} },
    {"difftime", {"<time.h>", "<ctime>"} },
    {"gmtime", {"<time.h>", "<ctime>"} },
    {"localtime", {"<time.h>", "<ctime>"} },
    {"mktime", {"<time.h>", "<ctime>"} },
    {"strftime", {"<time.h>", "<ctime>"} },
    {"time", {"<time.h>", "<ctime>"} },
    {"time_t", {"<time.h>", "<ctime>"} },
    {"tm", {"<time.h>", "<ctime>"} },

    /* <wchar.h>.  */
    {"WCHAR_MAX", {"<wchar.h>", "<cwchar>"} },
    {"WCHAR_MIN", {"<wchar.h>", "<cwchar>"} }
  };
  const size_t num_hints = ARRAY_SIZE (hints);
  for (size_t i = 0; i < num_hints; i++)
    if (strcmp (name, hints[i].name) == 0)
      return hints[i].header[lib];

  static const stdlib_hint c99_cxx11_hints[] = {
    /* <stdbool.h>.  Defined natively in C++.  */
    {"bool", {"<stdbool.h>", NULL} },
    {"true", {"<stdbool.h>", NULL} },
    {"false", {"<stdbool.h>", NULL} },

    /* <stdint.h> and <cstdint>.  */
    {"int8_t", {"<stdint.h>", "<cstdint>"} },
    {"uint8_t", {"<stdint.h>", "<cstdint>"} },
    {"int16_t", {"<stdint.h>", "<cstdint>"} },
    {"uint16_t", {"<stdint.h>", "<cstdint>"} },
    {"int32_t", {"<stdint.h>", "<cstdint>"} },
    {"uint32_t", {"<stdint.h>", "<cstdint>"} },
    {"int64_t", {"<stdint.h>", "<cstdint>"} },
    {"uint64_t", {"<stdint.h>", "<cstdint>"} },
    {"intptr_t", {"<stdint.h>", "<cstdint>"} },
    {"uintptr_t", {"<stdint.h>", "<cstdint>"} },
    {"INT8_MAX", {"<stdint.h>", "<cstdint>"} },
    {"INT16_MAX", {"<stdint.h>", "<cstdint>"} },
    {"INT32_MAX", {"<stdint.h>", "<cstdint>"} },
    {"INT64_MAX", {"<stdint.h>", "<cstdint>"} },
    {"UINT8_MAX", {"<stdint.h>", "<cstdint>"} },
    {"UINT16_MAX", {"<stdint.h>", "<cstdint>"} },
    {"UINT32_MAX", {"<stdint.h>", "<cstdint>"} },
    {"UINT64_MAX", {"<stdint.h>", "<cstdint>"} },
    {"INTPTR_MAX", {"<stdint.h>", "<cstdint>"} },
    {"UINTPTR_MAX", {"<stdint.h>", "<cstdint>"} }
  };

  const size_t num_c99_cxx11_hints = sizeof (c99_cxx11_hints)
					     / sizeof (c99_cxx11_hints[0]);
  if ((lib == STDLIB_C && flag_isoc99)
      || (lib == STDLIB_CPLUSPLUS && cxx_dialect >= cxx11 ))
    for (size_t i = 0; i < num_c99_cxx11_hints; i++)
      if (strcmp (name, c99_cxx11_hints[i].name) == 0)
	return c99_cxx11_hints[i].header[lib];

  return get_string_macro_hint (name, lib);
}

/* Given non-NULL NAME, return the header name defining it within the C
   standard library (with '<' and '>'), or NULL.  */

const char *
get_c_stdlib_header_for_name (const char *name)
{
  return get_stdlib_header_for_name (name, STDLIB_C);
}

/* Given non-NULL NAME, return the header name defining it within the C++
   standard library (with '<' and '>'), or NULL.  */

const char *
get_cp_stdlib_header_for_name (const char *name)
{
  return get_stdlib_header_for_name (name, STDLIB_CPLUSPLUS);
}

/* Given non-NULL NAME, return the header name defining a string macro
   within the C standard library (with '<' and '>'), or NULL.  */
const char *
get_c_stdlib_header_for_string_macro_name (const char *name)
{
  return get_string_macro_hint (name, STDLIB_C);
}

/* Given non-NULL NAME, return the header name defining a string macro
   within the C++ standard library (with '<' and '>'), or NULL.  */
const char *
get_cp_stdlib_header_for_string_macro_name (const char *name)
{
  return get_string_macro_hint (name, STDLIB_CPLUSPLUS);
}

/* Implementation of class suggest_missing_header.  */

/* suggest_missing_header's ctor.  */

suggest_missing_header::suggest_missing_header (location_t loc,
						const char *name,
						const char *header_hint)
: deferred_diagnostic (loc), m_name_str (name), m_header_hint (header_hint)
{
  gcc_assert (name);
  gcc_assert (header_hint);
}

/* suggest_missing_header's dtor.  */

suggest_missing_header::~suggest_missing_header ()
{
  if (is_suppressed_p ())
    return;

  gcc_rich_location richloc (get_location ());
  maybe_add_include_fixit (&richloc, m_header_hint, true);
  inform (&richloc,
	  "%qs is defined in header %qs;"
	  " this is probably fixable by adding %<#include %s%>",
	  m_name_str, m_header_hint, m_header_hint);
}
