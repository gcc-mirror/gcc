/* Null garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

/* This version is used by the gen* programs, where we don't really
   need GC at all.  This prevents problems with pulling in all the
   tree stuff.  */

/* We are used by gengenrtl, before genrtl.h exists.  But we don't 
   need it either.  */
#define NO_GENRTL_H

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"

/* For now, keep using the old obstack scheme in the gen* programs.  */
int ggc_p = 0;

void *
ggc_alloc_obj (size, zero)
     size_t size;
     int zero;
{
  void *p = xmalloc (size);
  if (zero)
    memset (p, 0, size);
  return p;
}
