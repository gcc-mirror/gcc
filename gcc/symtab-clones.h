/* Representation of adjustment made to virtual clones in the symbol table.
   Copyright (C) 2003-2021 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

#ifndef GCC_SYMTAB_CLONES_H
#define GCC_SYMTAB_CLONES_H

struct GTY(()) clone_info
{
  /* Constructor.  */
  clone_info ()
    : tree_map (NULL),
      param_adjustments (NULL),
      performed_splits (NULL)
  {
  }
  /* Constants discovered by IPA-CP, i.e. which parameter should be replaced
     with what.  */
  vec<ipa_replace_map *, va_gc> *tree_map;
  /* Parameter modification that IPA-SRA decided to perform.  */
  ipa_param_adjustments *param_adjustments;
  /* Lists of dummy-decl and offset pairs representing split formal parameters
     in the caller.  Offsets of all new replacements are enumerated, those
     coming from the same original parameter have the same dummy decl stored
     along with them.

     Dummy decls sit in call statement arguments followed by new parameter
     decls (or their SSA names) in between (caller) clone materialization and
     call redirection.  Redirection then recognizes the dummy variable and
     together with the stored offsets can reconstruct what exactly the new
     parameter decls represent and can leave in place only those that the
     callee expects.  */
  vec<ipa_param_performed_split, va_gc> *performed_splits;

  /* Return clone_info, if available.  */
  static clone_info *get (cgraph_node *node);

  /* Return clone_info possibly creating new one.  */
  static clone_info *get_create (cgraph_node *node);

  /* Remove clone_info.  */
  static void remove (cgraph_node *node);

  /* Release all clone_infos.  */
  static void release (void);
};

/* Return clone_info, if available.  */
inline clone_info *
clone_info::get (cgraph_node *node)
{
  if (!symtab->m_clones)
    return NULL;
  return symtab->m_clones->get (node);
}


/* Remove clone_info association for NODE.  */
inline void
clone_info::remove (cgraph_node *node)
{
  symtab->m_clones->remove (node);
}

/* Free clone info summaries.  */
inline void
clone_info::release ()
{
  if (symtab->m_clones)
    ggc_delete (symtab->m_clones);
  symtab->m_clones = NULL;
}

#endif  /* GCC_SYMTAB_CLONES_H  */
