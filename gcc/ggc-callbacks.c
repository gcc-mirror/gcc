/* Empty GC callbacks to be used by languages that don't support GC.
   Copyright (C) 1999 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "ggc.h"

int ggc_p = 0;

void ATTRIBUTE_NORETURN
lang_mark_tree (t)
     union tree_node *t ATTRIBUTE_UNUSED;
{
  /* If this function is called, we are doing GC.  But, this file is
     only included in compilers for languages that don't support GC.  */
  abort ();
}

void ATTRIBUTE_NORETURN
lang_mark_false_label_stack (l)
     struct label_node *l ATTRIBUTE_UNUSED;
{
  /* If this function is called, we are doing GC.  But, this file is
     only included in compilers for languages that don't support GC.  */
  abort ();
}
