/* General-purpose hooks.
   Copyright (C) 2002 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

/* This file contains generic hooks that can be used as defaults for
   target or language-dependent hook initializers.  */

#include "config.h"
#include "system.h"
#include "hooks.h"

/* Generic hook that does absolutely zappo.  */
void
hook_void_void ()
{
}

/* Generic hook that takes no arguments and returns false.  */
bool
hook_void_bool_false ()
{
  return false;
}

/* Generic hook that takes (tree) and returns false.  */
bool
hook_tree_bool_false (a)
     tree a ATTRIBUTE_UNUSED;
{
  return false;
}
