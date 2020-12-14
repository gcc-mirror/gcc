/* C++ modules.  Experimental!
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

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

/* This file intentionally left empty of all but barest minium.  */

/* In expermental (trunk) sources, MODULE_VERSION is a #define passed
   in from the Makefile.  It records the modification date of the
   source directory -- that's the only way to stay sane.  In release
   sources, we (plan to) use the compiler's major.minor versioning.
   While the format might not change between at minor versions, it
   seems simplest to tie the two together.  There's no concept of
   inter-version compatibility.  */
#define IS_EXPERIMENTAL(V) ((V) >= (1U << 20))
#define MODULE_MAJOR(V) ((V) / 10000)
#define MODULE_MINOR(V) ((V) % 10000)
#define EXPERIMENT(A,B) (IS_EXPERIMENTAL (MODULE_VERSION) ? (A) : (B))
#ifndef MODULE_VERSION
// Be sure you're ready!  Remove #error this before release!
#error "Shtopp! What are you doing? This is not ready yet."
#include "bversion.h"
#define MODULE_VERSION (BUILDING_GCC_MAJOR * 10000U + BUILDING_GCC_MINOR)
#elif !IS_EXPERIMENTAL (MODULE_VERSION)
#error "This is not the version I was looking for."
#endif

#define _DEFAULT_SOURCE 1 /* To get TZ field of struct tm, if available.  */
#include "config.h"

#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "dumpfile.h"
#include "bitmap.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "cpplib.h"
#include "mkdeps.h"
#include "incpath.h"
#include "libiberty.h"
#include "stor-layout.h"
#include "version.h"
#include "tree-diagnostic.h"
#include "toplev.h"
#include "opts.h"
#include "attribs.h"
#include "intl.h"
#include "langhooks.h"

/* During duplicate detection we need to tell some comparators that
   these are equivalent.  */
tree map_context_from;
tree map_context_to;

/* Id for dumping module information.  */
int module_dump_id;

/* What the current TU is.  */
unsigned module_kind;

module_state *
get_module (tree, module_state *, bool)
{
  return nullptr;
}

const char *
module_name (unsigned, bool)
{
  return nullptr;
}

bitmap
get_import_bitmap ()
{
  return nullptr;
}

void
mangle_module (int, bool)
{
}

void
mangle_module_fini ()
{
}

int
module_initializer_kind ()
{
  return 0;
}

void
module_add_import_initializers ()
{
}

int
get_originating_module (tree, bool)
{
  return 0;
}

unsigned
get_importing_module (tree, bool)
{
  return 0;
}

bool
module_may_redeclare (tree)
{
  return true;
}

void
set_instantiating_module (tree)
{
}

void
set_defining_module (tree)
{
}

tree
get_originating_module_decl (tree decl)
{
  return decl;
}

void
set_originating_module (tree, bool)
{
}

void
maybe_attach_decl (tree, tree)
{
}

void
lazy_load_binding (unsigned, tree, tree, binding_slot *)
{
}

void
lazy_load_specializations (tree)
{
}

void
lazy_load_members (tree)
{
}

bool
lazy_specializations_p (unsigned, bool, bool)
{
  return false;
}

bitmap
visible_instantiation_path (bitmap *)
{
  return nullptr;
}

void
import_module (module_state *, location_t, bool, tree, cpp_reader *)
{
}

void
declare_module (module_state *, location_t, bool, tree, cpp_reader *)
{
}

module_state *
preprocess_module (module_state *, unsigned, bool, bool, bool, cpp_reader *)
{
  return nullptr;
}

void
preprocessed_module (cpp_reader *)
{
}

void
module_begin_main_file (cpp_reader *, line_maps *, const line_map_ordinary *)
{
}

void
init_modules (cpp_reader *)
{
  /* Do not turn on yet.  */
  if (modules_p ())
    fatal_error (input_location,
		 "Shtopp! What are you doing? This is not ready yet.");
}

void
maybe_check_all_macros (cpp_reader *)
{
}

void
finish_module_processing (cpp_reader *)
{
}

void
fini_modules ()
{
}

bool
handle_module_option (unsigned, const char *, int)
{
  return false;
}

void
module_preprocess_options (cpp_reader *)
{
}
