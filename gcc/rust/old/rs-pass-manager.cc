/* This file is part of GCC.

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

#include "rust.h"

tree cstring_type_node;
static bool empty = true;
#define MAYBE_BOMB_OUT                          \
  do                                            \
    if (seen_error ()) return;                  \
  while (0);

static vec<rdot,va_gc> * rust_decls;
typedef vec<rdot,va_gc> * (*dot_pass)(vec<rdot,va_gc> *);
static dot_pass dot_pass_mngr[] =
  {
    &dot_pass_PrettyPrint,  /* pretty print if -fdump-dot pre and post infereance */
    &dot_pass_inferTypes,   /* This ensures there are no longer any D_MAYBE_TYPES */
    &dot_pass_PrettyPrint,
    NULL                         /* sentinal */
  };

/* Pushes each decl from the parser onto the current translation unit */
void dot_pass_pushDecl (rdot decl)
{
  if (empty)
    empty = false;

  if (RDOT_TYPE (decl) == D_STRUCT_TYPE)
    {
      // look for duplicate fields
      std::map<std::string, bool> layout;
      rdot next;
      for (next = RDOT_rhs_TT (decl); next != NULL_DOT;
           next = RDOT_CHAIN (next))
        {
          const char * pid = RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (next));
          if (layout.count (pid) > 0)
              error ("structure [%s] already contains element [%s]",
                     RDOT_IDENTIFIER_POINTER (RDOT_lhs_TT (decl)), pid);
          else
            layout [pid] = true;
        }
    }
  vec_safe_push (rust_decls, decl);
}

/* Function to run over the pass manager hooks and
   generate the generic code to pass to gcc middle-end
*/
void dot_pass_WriteGlobals (void)
{
  // if the rdot translation unit is empty there is nothing to compile..
  if (empty == true)
    return;

  tree cptr = build_pointer_type (char_type_node);
  cstring_type_node = build_qualified_type (cptr, TYPE_QUAL_CONST);

  dot_pass *p = NULL;
  vec<rdot,va_gc> * dot_decls = rust_decls;

  /* walk the passes */
  for (p = dot_pass_mngr; *p != NULL; ++p)
    {
      MAYBE_BOMB_OUT;
      dot_decls = (*p)(dot_decls);
    }
    
  /* check errors */
  MAYBE_BOMB_OUT;

  /* lower the decls from DOT -> GENERIC */
  vec<tree,va_gc> * globals = dot_pass_Genericify (dot_decls);

  int global_vec_len = vec_safe_length (globals);
  tree * global_vec = new tree[global_vec_len];
  tree itx = NULL_TREE;
  int idx, idy = 0;
  /*
    Lets make sure to dump the Translation Unit this isn't that
    useful to read over but can help to make sure certain tree's
    are being generated...
      
    We also fill up the vector of tree's to be passed to the middle-end
  */
  FILE * tu_stream = dump_begin (TDI_tu, NULL);
  for (idx = 0; vec_safe_iterate (globals, idx, &itx); ++idx)
    {
      if (tu_stream)
        dump_node (itx, 0, tu_stream);
      global_vec [idy] = itx;
      idy++;
    }
  if (tu_stream)
    dump_end(TDI_tu, tu_stream);
  
  /* Passing control to GCC middle-end */
  wrapup_global_declarations (global_vec, global_vec_len);
  finalize_compilation_unit ();
  check_global_declarations (global_vec, global_vec_len);
  emit_debug_global_declarations (global_vec, global_vec_len);

  delete [] global_vec;
}
