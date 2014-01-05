/* c-upc-lang.h: UPC language-specific functions.
   Copyright (C) 2012
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

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
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_C_UPC_LANG_H
#define GCC_C_UPC_LANG_H 1
extern void upc_pts_struct_init_type (void);
extern void upc_build_init_func (tree);
extern void upc_toggle_keywords (bool);
extern bool upc_lang_layout_decl_p (tree, tree);
extern void upc_lang_layout_decl (tree, tree);
extern void upc_lang_init (void);
#endif /* !GCC_C_UPC_LANG_H */
