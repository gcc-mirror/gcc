/* strub (stack scrubbing) infrastructure.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>.

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

/* Return TRUE if CALLEE can be inlined into CALLER, as far as stack scrubbing
   constraints are concerned.  CALLEE doesn't have to be called directly by
   CALLER, but the returned value says nothing about intervening functions.  */
extern bool strub_inlinable_to_p (cgraph_node *callee, cgraph_node *caller);

/* Return FALSE if NODE is a strub context, and TRUE otherwise.  */
extern bool strub_splittable_p (cgraph_node *node);

/* Locate and return the watermark_ptr parameter for FNDECL.  If FNDECL is not a
   strub context, return NULL.  */
extern tree strub_watermark_parm (tree fndecl);

/* Make a function type or declaration callable.  */
extern void strub_make_callable (tree fndecl);

/* Return zero iff ID is NOT an acceptable parameter for a user-supplied strub
   attribute for a function.  Otherwise, return >0 if it enables strub, <0 if it
   does not.  Return +/-1 if the attribute-modified type is compatible with the
   type without the attribute, or +/-2 if it is not compatible.  */
extern int strub_validate_fn_attr_parm (tree id);

/* Like comptypes, return 0 if t1 and t2 are not compatible, 1 if they are
   compatible, and 2 if they are nearly compatible.  Same strub mode is
   compatible, interface-compatible strub modes are nearly compatible.  */
extern int strub_comptypes (tree t1, tree t2);
