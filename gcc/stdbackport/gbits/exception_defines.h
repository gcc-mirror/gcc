// -fno-exceptions Support -*- C++ -*-

// Copyright (C) 2001-2026 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef GCC_BACKPORT_EXCEPTION_DEFINES_H
#define GCC_BACKPORT_EXCEPTION_DEFINES_H 1

#include "assert.h"

#if !defined(GCC_BACKPORT_WANT_EXCEPTIONS) || !__cpp_exceptions
// Iff exceptions unwanted, transform error handling code to work without it.
# define GCC_BACKPORT_HAS_EXCEPTIONS 0
# define g_try      if (true)
# define g_catch(X) if (false)
# define g_throw_exception_again
// Note that when exceptions are disabled, EXC is neither evaluated *nor used*.
# define g_throw_or_abort(exc) gccbackport_assert (!#exc)
#else
// Else proceed normally.
# define GCC_BACKPORT_HAS_EXCEPTIONS 1
# define g_try      try
# define g_catch(X) catch(X)
# define g_throw_exception_again throw
// Note that when exceptions are disabled, EXC is neither evaluated *nor used*.
# define g_throw_or_abort(exc) throw (exc)
#endif

#endif
