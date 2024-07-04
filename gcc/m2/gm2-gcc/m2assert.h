/* m2assert.h header file for m2assert.cc and assertion macros.

Copyright (C) 2012-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#if !defined(m2assert_h)
#define m2assert_h
#if defined(m2assert_c)
#define EXTERN
#else /* !m2assert_c.  */
#define EXTERN extern
#endif /* !m2assert_c.  */

#if !defined(ASSERT)
#define ASSERT(X, Y)                                                          \
  {                                                                           \
    if (!(X))                                                                 \
      {                                                                       \
        debug_tree (Y);                                                       \
        internal_error ("%s:%d:condition %s failed", __FILE__, __LINE__,      \
                        #X);                                                  \
      }                                                                       \
  }
#endif

#if !defined(ASSERT_BOOL)
#define ASSERT_BOOL(X)                                                        \
  {                                                                           \
    if ((X != 0) && (X != 1))                                                 \
      {                                                                       \
        internal_error (                                                      \
            "%s:%d:the value %s is not a BOOLEAN as the value is %d",         \
            __FILE__, __LINE__, #X, X);                                       \
      }                                                                       \
  }
#endif

#if !defined(ASSERT_CONDITION)
#define ASSERT_CONDITION(X)                                                   \
  {                                                                           \
    if (!(X))                                                                 \
      {                                                                       \
        internal_error ("%s:%d:condition %s failed", __FILE__, __LINE__,      \
                        #X);                                                  \
      }                                                                       \
  }
#endif

EXTERN void m2assert_AssertLocation (location_t location);

#undef EXTERN
#endif  /* m2assert_h.  */
