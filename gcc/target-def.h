/* Default initializers for a generic GCC target.
   Copyright (C) 2001 Free Software Foundation, Inc.

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

/* See target.h for a desciption of what this file contains and how to
   use it.  */

/* Both in tree.c.  */
#define TARGET_MERGE_DECL_ATTRIBUTES merge_decl_attributes
#define TARGET_MERGE_TYPE_ATTRIBUTES merge_type_attributes
#define TARGET_VALID_DECL_ATTRIBUTE 0
#define TARGET_VALID_TYPE_ATTRIBUTE 0

/* The whole shebang.  */
#define TARGET_INITIALIZER			\
{						\
  TARGET_MERGE_DECL_ATTRIBUTES,			\
  TARGET_MERGE_TYPE_ATTRIBUTES,			\
  TARGET_VALID_DECL_ATTRIBUTE,			\
  TARGET_VALID_TYPE_ATTRIBUTE			\
}
