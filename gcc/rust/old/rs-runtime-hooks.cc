/* This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>. */

#include "rust.h"

#define nitems(_a) (sizeof (_a)) / sizeof ((_a)[0])
#define LANG_HOOK(name_) "__GRUST_"name_, name_

struct rust_runtime {
    const char * mangled_name;
    const char * rust_symbol;
    const size_t nargs;
    const tree ** paramater_types;
};

static struct rust_runtime hooks [] =  {
    /* sentinal */
    { NULL, NULL, 0, NULL }
};

void rs_fill_runtime_decls (std::map<std::string, tree> * dict)
{
    struct rust_runtime * hk;
    for (hk = hooks; hk->mangled_name != NULL; ++hk)
    {
	tree * args = XALLOCAVEC (tree, hk->nargs);
	size_t i;
	for (i = 0; i < hk->nargs; ++i)
	    args[i] = *(hk->paramater_types [i]);

	tree fntype = build_function_type_array (void_type_node, hk->nargs, args);
	tree fndecl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
				  get_identifier (hk->mangled_name), fntype);
	tree restype = TREE_TYPE (fndecl);
	tree resdecl = build_decl (BUILTINS_LOCATION, RESULT_DECL,
				   NULL_TREE, restype);
	DECL_CONTEXT (resdecl) = fndecl;
	DECL_RESULT (fndecl) = resdecl;
	DECL_EXTERNAL (fndecl) = 1;
	TREE_PUBLIC (fndecl) = 1;

	(*dict)[std::string (hk->rust_symbol)] = fndecl;
    }
}
