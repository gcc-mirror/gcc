/* header file for GCC for a Symbian OS targeted SH backend.
   Copyright (C) 2004, 2005, 2007, 2009 Free Software Foundation, Inc.
   Contributed by RedHat.
   Most of this code is stolen from i386/winnt.c.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* A unique character to encode declspec encoded objects.  */
#define SH_SYMBIAN_FLAG_CHAR "$"

/* Unique strings to prefix exported and imported objects.  */
#define DLL_IMPORT_PREFIX SH_SYMBIAN_FLAG_CHAR "i."
#define DLL_EXPORT_PREFIX SH_SYMBIAN_FLAG_CHAR "e."

/* Select the level of debugging information to display.
   0 for no debugging.
   1 for informative messages about decisions to add attributes
   2 for verbose information about what is being done.  */
#define SYMBIAN_DEBUG 0
/* #define SYMBIAN_DEBUG 1 */
/* #define SYMBIAN_DEBUG 2 */

/* Functions exported from symbian-base.c.  */
extern tree sh_symbian_associated_type (tree);

/* Functions exported from symbian-[c|c++].c.  */
extern bool sh_symbian_is_dllimported (tree);

