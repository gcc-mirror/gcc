/* Feature dependency helpers for AArch64.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef AARCH64_FEATURE_DEPS_H
#define AARCH64_FEATURE_DEPS_H 1

namespace {
namespace feature_deps {

/* Together, these definitions of get_flags take a list of
   feature names (representing functions that are defined below)
   and return the set of associated flags.  */
constexpr aarch64_feature_flags get_flags () { return 0; }

template<typename T1, typename ...Ts>
constexpr aarch64_feature_flags
get_flags (T1 i, Ts... args)
{
  return i ().flag | get_flags (args...);
}

/* Like get_flags, but return the transitive closure of those features
   and the ones that they rely on.  */
constexpr aarch64_feature_flags get_enable () { return 0; }

template<typename T1, typename ...Ts>
constexpr aarch64_feature_flags
get_enable (T1 i, Ts... args)
{
  return i ().enable | get_enable (args...);
}

/* Define info<FEATURE> such that it has the following static constant
   variables:

   - flag: the aarch64_feature_flags bit associated with FEATURE

   - enable: the transitive closure of the features that FEATURE requires,
     plus FLAG itself

   - explicit_on: the transitive closure of the features that an
     explicit +FEATURE enables, including FLAG itself.  This is
     always a superset of ENABLE

   Also define a function FEATURE () that returns an info<FEATURE>
   (which is an empty structure, since all members are static).

   Building up the list feature-by-feature ensures that the definition
   files are in topological order.  */
template<aarch64_feature> struct info;

#define HANDLE(IDENT, REQUIRES, EXPLICIT_ON)				\
  template<> struct info<aarch64_feature::IDENT> {			\
    static constexpr auto flag = AARCH64_FL_##IDENT;			\
    static constexpr auto enable = flag | get_enable REQUIRES;		\
    static constexpr auto explicit_on = enable | get_enable EXPLICIT_ON; \
  };									\
  constexpr aarch64_feature_flags info<aarch64_feature::IDENT>::flag;	\
  constexpr aarch64_feature_flags info<aarch64_feature::IDENT>::enable;	\
  constexpr aarch64_feature_flags info<aarch64_feature::IDENT>::explicit_on; \
  constexpr info<aarch64_feature::IDENT> IDENT ()			\
  {									\
    return info<aarch64_feature::IDENT> ();				\
  }
#define AARCH64_OPT_EXTENSION(A, IDENT, REQUIRES, EXPLICIT_ON, E, F) \
  HANDLE (IDENT, REQUIRES, EXPLICIT_ON)
#define AARCH64_ARCH(A, B, IDENT, D, REQUIRES) HANDLE (IDENT, REQUIRES, ())
#include "config/aarch64/aarch64-option-extensions.def"
#include "config/aarch64/aarch64-arches.def"
#undef HANDLE

/* Return the set of all features that would need to be disabled if
   the features in MASK are disabled.

   Note that the size of the expression varies linearly with the number
   of features, which means that invoking this function once per feature
   is quadratic in the number of features.  However, collecting the same
   information at compiler start-up is likely to be quadratic too, so
   we're better off paying the cost once per compiler build rather than
   once per compiler run.  */
constexpr aarch64_feature_flags
get_flags_off (aarch64_feature_flags mask)
{
  return (aarch64_feature_flags (0)
#define AARCH64_OPT_EXTENSION(A, IDENT, C, D, E, F) \
	  | (feature_deps::IDENT ().enable & mask ? AARCH64_FL_##IDENT \
						  : aarch64_feature_flags (0))
#include "config/aarch64/aarch64-option-extensions.def"
	  );
}

/* Define root_off_<IDENT> variables for each feature, giving the set of
   features that must be turned off by +noIDENT.  This set is not transitively
   closed; use get_flags_off to complete the closure.  */
#define AARCH64_OPT_EXTENSION(A, IDENT, C, D, EXPLICIT_OFF, F) \
  constexpr auto root_off_##IDENT \
    = AARCH64_FL_##IDENT | get_flags EXPLICIT_OFF;
#include "config/aarch64/aarch64-option-extensions.def"

/* Define cpu_<NAME> variables for each CPU, giving the transitive
   closure of all the features that the CPU supports.  */
#define AARCH64_CORE(A, CORE_IDENT, C, ARCH_IDENT, FEATURES, F, G, H, I) \
  constexpr auto cpu_##CORE_IDENT = ARCH_IDENT ().enable | get_enable FEATURES;
#include "config/aarch64/aarch64-cores.def"

/* Define fmv_deps_<NAME> variables for each FMV feature, giving the transitive
   closure of all the features that the FMV feature enables.  */
#define AARCH64_FMV_FEATURE(A, FEAT_NAME, OPT_FLAGS) \
  constexpr auto fmv_deps_##FEAT_NAME = get_enable OPT_FLAGS;
#include "config/aarch64/aarch64-option-extensions.def"


}
}

#endif
