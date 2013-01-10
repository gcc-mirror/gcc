// go-system.h -- Go frontend inclusion of gcc header files   -*- C++ -*-
// Copyright (C) 2009-2013 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef GO_SYSTEM_H
#define GO_SYSTEM_H

#include "config.h"

// These must be included before the #poison declarations in system.h.

#include <algorithm>
#include <string>
#include <list>
#include <map>
#include <set>
#include <vector>

#if defined(HAVE_UNORDERED_MAP)

# include <unordered_map>
# include <unordered_set>

# define Unordered_map(KEYTYPE, VALTYPE) \
	std::unordered_map<KEYTYPE, VALTYPE>

# define Unordered_map_hash(KEYTYPE, VALTYPE, HASHFN, EQFN) \
	std::unordered_map<KEYTYPE, VALTYPE, HASHFN, EQFN>

# define Unordered_set(KEYTYPE) \
	std::unordered_set<KEYTYPE>

# define Unordered_set_hash(KEYTYPE, HASHFN, EQFN) \
	std::unordered_set<KEYTYPE, HASHFN, EQFN>

#elif defined(HAVE_TR1_UNORDERED_MAP)

# include <tr1/unordered_map>
# include <tr1/unordered_set>

# define Unordered_map(KEYTYPE, VALTYPE) \
	std::tr1::unordered_map<KEYTYPE, VALTYPE>

# define Unordered_map_hash(KEYTYPE, VALTYPE, HASHFN, EQFN) \
	std::tr1::unordered_map<KEYTYPE, VALTYPE, HASHFN, EQFN>

# define Unordered_set(KEYTYPE) \
	std::tr1::unordered_set<KEYTYPE>

# define Unordered_set_hash(KEYTYPE, HASHFN, EQFN) \
	std::tr1::unordered_set<KEYTYPE, HASHFN, EQFN>

#elif defined(HAVE_EXT_HASH_MAP)

# include <ext/hash_map>
# include <ext/hash_set>

# define Unordered_map(KEYTYPE, VALTYPE) \
	__gnu_cxx::hash_map<KEYTYPE, VALTYPE>

# define Unordered_map_hash(KEYTYPE, VALTYPE, HASHFN, EQFN) \
	__gnu_cxx::hash_map<KEYTYPE, VALTYPE, HASHFN, EQFN>

# define Unordered_set(KEYTYPE) \
	__gnu_cxx::hash_set<KEYTYPE>

# define Unordered_set_hash(KEYTYPE, HASHFN, EQFN) \
	__gnu_cxx::hash_set<KEYTYPE, HASHFN, EQFN>

// Provide hash functions for strings and pointers.

namespace __gnu_cxx
{

template<>
struct hash<std::string>
{
  size_t
  operator()(std::string s) const
  { return __stl_hash_string(s.c_str()); }
};

template<typename T>
struct hash<T*>
{
  size_t
  operator()(T* p) const
  { return reinterpret_cast<size_t>(p); }
};

}

#else

# define Unordered_map(KEYTYPE, VALTYPE) \
	std::map<KEYTYPE, VALTYPE>

# define Unordered_set(KEYTYPE) \
	std::set<KEYTYPE>

// We could make this work by writing an adapter class which
// implemented operator< in terms of the hash function.
# error "requires hash table type"

#endif

// We don't really need iostream, but some versions of gmp.h include
// it when compiled with C++, which means that we need to include it
// before the macro magic of safe-ctype.h, which is included by
// system.h.
#include <iostream>

#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"

#include "diagnostic-core.h"	/* For error_at and friends.  */
#include "input.h"		/* For source_location.  */
#include "intl.h"		/* For _().  */

// When using gcc, go_assert is just gcc_assert.
#define go_assert(EXPR) gcc_assert(EXPR)

// When using gcc, go_unreachable is just gcc_unreachable.
#define go_unreachable() gcc_unreachable()

#endif // !defined(GO_SYSTEM_H)
