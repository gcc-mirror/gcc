/* Mudflap: narrow-pointer bounds-checking by tree rewriting.
   Copyright (C) 2001, 2002, 2003, 2005 Free Software Foundation, Inc.
   Contributed by Frank Ch. Eigler <fche@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef TREE_MUDFLAP_H
#define TREE_MUDFLAP_H

/* Instrumentation.  */
extern void mudflap_init (void);
extern void mudflap_enqueue_decl (tree);
extern void mudflap_enqueue_constant (tree);
extern void mudflap_finish_file (void);

/* Tree node marking.  */
extern int mf_marked_p (tree);
extern tree mf_mark (tree);

#endif /* TREE_MUDFLAP_H */
