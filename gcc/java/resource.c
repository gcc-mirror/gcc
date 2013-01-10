/* Functions related to building resource files.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

This file is part of GCC.

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
<http://www.gnu.org/licenses/>.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "java-tree.h"
#include "jcf.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "parse.h"
#include "function.h"
#include "ggc.h"
#include "tree-iterator.h"
#include "cgraph.h"

/* A list of all the resources files.  */
static GTY(()) vec<tree, va_gc> *resources;

void
compile_resource_data (const char *name, const char *buffer, int length)
{
  tree rtype, field = NULL_TREE, data_type, rinit, data, decl;
  vec<constructor_elt, va_gc> *v = NULL;

  data_type = build_prim_array_type (unsigned_byte_type_node,
				     strlen (name) + length);
  rtype = make_node (RECORD_TYPE);
  PUSH_FIELD (input_location,
	      rtype, field, "name_length", unsigned_int_type_node);
  PUSH_FIELD (input_location,
	      rtype, field, "resource_length", unsigned_int_type_node);
  PUSH_FIELD (input_location, rtype, field, "data", data_type);
  FINISH_RECORD (rtype);
  START_RECORD_CONSTRUCTOR (v, rtype);
  PUSH_FIELD_VALUE (v, "name_length", 
		    build_int_cst (NULL_TREE, strlen (name)));
  PUSH_FIELD_VALUE (v, "resource_length", 
		    build_int_cst (NULL_TREE, length));
  data = build_string (strlen(name) + length, buffer);
  TREE_TYPE (data) = data_type;
  PUSH_FIELD_VALUE (v, "data", data);
  FINISH_RECORD_CONSTRUCTOR (rinit, v, rtype);
  TREE_CONSTANT (rinit) = 1;

  decl = build_decl (input_location,
		     VAR_DECL, java_mangle_resource_name (name), rtype);
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  java_hide_decl (decl);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_THIS_VOLATILE (decl) = 0;
  DECL_INITIAL (decl) = rinit;
  layout_decl (decl, 0);
  pushdecl (decl);
  rest_of_decl_compilation (decl, global_bindings_p (), 0);
  varpool_finalize_decl (decl);

  vec_safe_push (resources, decl);
}

void
write_resource_constructor (tree *list_p)
{
  tree decl, t, register_resource_fn;
  unsigned ix;

  if (resources == NULL)
    return;

  t = build_function_type_list (void_type_node, ptr_type_node, NULL);
  t = build_decl (input_location,
		  FUNCTION_DECL, get_identifier ("_Jv_RegisterResource"), t);
  TREE_PUBLIC (t) = 1;
  DECL_EXTERNAL (t) = 1;
  register_resource_fn = t;

  /* Write out entries in the same order in which they were defined.  */
  FOR_EACH_VEC_ELT (*resources, ix, decl)
    {
      t = build_fold_addr_expr (decl);
      t = build_call_expr (register_resource_fn, 1, t);
      append_to_statement_list (t, list_p);
    }
}

/* Generate a byte array representing the contents of FILENAME.  The
   array is assigned a unique local symbol.  The array represents a
   compiled Java resource, which is accessed by the runtime using
   NAME.  */
void
compile_resource_file (const char *name, const char *filename)
{
  struct stat stat_buf;
  int fd;
  char *buffer;

  fd = open (filename, O_RDONLY | O_BINARY);
  if (fd < 0)
    {
      perror ("Failed to read resource file");
      return;
    }
  if (fstat (fd, &stat_buf) != 0
      || ! S_ISREG (stat_buf.st_mode))
    {
      perror ("Could not figure length of resource file");
      return;
    }
  buffer = XNEWVEC (char, strlen (name) + stat_buf.st_size);
  strcpy (buffer, name);
  read (fd, buffer + strlen (name), stat_buf.st_size);
  close (fd);

  compile_resource_data (name, buffer, stat_buf.st_size);
}

#include "gt-java-resource.h"
