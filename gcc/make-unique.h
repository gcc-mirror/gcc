/* Minimal implementation of make_unique for C++11 compatibility.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

#ifndef GCC_MAKE_UNIQUE
#define GCC_MAKE_UNIQUE

/* This header uses std::unique_ptr, but <memory> can't be directly
   included due to issues with macros.  Hence <memory> must be included
   from system.h by defining INCLUDE_MEMORY in any source file using
   make-unique.h.  */

#ifndef INCLUDE_MEMORY
# error "You must define INCLUDE_MEMORY before including system.h to use make-unique.h"
#endif

#include <type_traits>

/* Minimal implementation of make_unique for C++11 compatibility
   (std::make_unique is C++14).  */

template<typename T, typename... Args>
inline typename std::enable_if<!std::is_array<T>::value, std::unique_ptr<T>>::type
make_unique(Args&&... args)
{
  return std::unique_ptr<T> (new T (std::forward<Args> (args)...));
}

#endif /* ! GCC_MAKE_UNIQUE */
