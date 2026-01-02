/* jit-target.h -- Data structure definitions for target-specific jit behavior.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_JIT_TARGET_H
#define GCC_JIT_TARGET_H

#define DEFHOOKPOD(NAME, DOC, TYPE, INIT) TYPE NAME;
#define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) TYPE (* NAME) PARAMS;
#define DEFHOOK_UNDOC DEFHOOK
#define HOOKSTRUCT(FRAGMENT) FRAGMENT

#include "jit-target.def"
#include "libgccjit.h"

#include <unordered_map>
#include <unordered_set>

static size_t hash_cstr (const char *s)
{
  const size_t seed = 0;
  return std::_Hash_bytes (s, std::strlen (s), seed);
}

struct CStringHash {
  size_t operator () (const char* const &string) const {
    auto res = hash_cstr (string);
    return res;
  }
};

struct CStringEqual {
  bool
  operator () (const char *const &string1, const char *const &string2) const
  {
    return strcmp (string1, string2) == 0;
  }
};

struct target_info {
  public:
    bool has_target_value (const char *key, const char *value);

    std::unordered_map<const char *,
	std::unordered_set<const char *, CStringHash, CStringEqual>,
	CStringHash, CStringEqual>
	m_info;
    std::string m_arch;
    std::unordered_set<enum gcc_jit_types> m_supported_target_dependent_types;
};

/* Each target can provide their own.  */
extern struct gcc_targetjitm targetjitm;

extern void jit_target_init ();
extern void jit_target_set_arch (std::string const& arch);
extern void
jit_target_add_supported_target_dependent_type (enum gcc_jit_types type_);
extern void jit_add_target_info (const char *key, const char *value);
extern target_info * jit_get_target_info ();

#endif /* GCC_JIT_TARGET_H  */
