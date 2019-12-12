/* d-system.h -- DMD frontend inclusion of gcc header files.
 * Copyright (C) 2018-2019 Free Software Foundation, Inc.
 *
 * GCC is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GCC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GCC; see the file COPYING3.  If not see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef GCC_D_SYSTEM_H
#define GCC_D_SYSTEM_H

#include "config.h"
#include "system.h"

/* Used by the dmd front-end to determine if we have POSIX-style IO.  */
#define POSIX (__linux__ || __GLIBC__ || __gnu_hurd__ || __APPLE__ \
	       || __FreeBSD__ || __NetBSD__ || __OpenBSD__ || __DragonFly__ \
	       || __sun || __unix__)

/* Forward assert invariants to gcc_assert.  */
#undef assert
#define assert(EXPR) gcc_assert(EXPR)

/* Use libiberty's lrealpath to avoid portability problems.  */
#undef realpath
#define realpath(a, b) lrealpath((a))

/* Forward ctype.h macros used by the dmd front-end to safe-ctype.h.  */
#undef isalpha
#define isalpha(c) ISALPHA(c)
#undef isalnum
#define isalnum(c) ISALNUM(c)
#undef isdigit
#define isdigit(c) ISDIGIT(c)
#undef islower
#define islower(c) ISLOWER(c)
#undef isprint
#define isprint(c) ISPRINT(c)
#undef isspace
#define isspace(c) ISSPACE(c)
#undef isupper
#define isupper(c) ISUPPER(c)
#undef isxdigit
#define isxdigit(c) ISXDIGIT(c)
#undef tolower
#define tolower(c) TOLOWER(c)

/* Forward _mkdir on MinGW to mkdir in system.h.  */
#ifdef _WIN32
#undef _mkdir
#define _mkdir(p) mkdir(p, 0)
#endif

/* Define any missing _MAX and _MIN macros.  */
#ifndef INT32_MAX
# define INT32_MAX INTTYPE_MAXIMUM (int32_t)
#endif
#ifndef INT32_MIN
# define INT32_MIN INTTYPE_MINIMUM (int32_t)
#endif
#ifndef INT64_MIN
# define INT64_MIN INTTYPE_MINIMUM (int64_t)
#endif
#ifndef UINT32_MAX
# define UINT32_MAX INTTYPE_MAXIMUM (uint32_t)
#endif
#ifndef UINT64_MAX
# define UINT64_MAX INTTYPE_MAXIMUM (uint64_t)
#endif

#endif  /* GCC_D_SYSTEM_H  */
