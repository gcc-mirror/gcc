/* Implements unwind table entry lookup for AIX (cf. fde-glibc.c). 
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
   Contributed by Timothy Wall <twall@redhat.com>

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

#include "tconfig.h"
#include "tsystem.h"
#include "unwind.h"
#include "unwind-ia64.h"

#include <dlfcn.h>
#include <link.h>
#include <sys/mman.h>

static struct unw_table_entry *
find_fde_for_dso (Elf64_Addr pc, rt_link_map *map,
                  unsigned long* pseg_base, unsigned long* pgp)
{
  rt_segment *seg;
  Elf64_Addr seg_base;
  struct unw_table_entry *f_base;
  size_t lo, hi;
  
  /* See if PC falls into one of the loaded segments.  */
  for (seg = map->l_segments; seg; seg = (rt_segment *)seg->s_next) 
    {
      if (pc >= seg->s_map_addr && pc < seg->s_map_addr + seg->s_mapsz)
        break;
    }
  if (!seg) 
    return NULL;
  
  /* Search for the entry within the unwind table.  */
  f_base = (struct unw_table_entry *) (map->l_unwind_table);
  seg_base = (Elf64_Addr) seg->s_map_addr;
  lo = 0;
  hi = map->l_unwind_sz / sizeof (struct unw_table_entry);

  while (lo < hi)
    {
      size_t mid = (lo + hi) / 2;
      struct unw_table_entry *f = f_base + mid;

      if (pc < f->start_offset + seg_base)
        hi = mid;
      else if (pc >= f->end_offset + seg_base)
        lo = mid + 1;
      else {
        /* AIX executables are *always* dynamic.  Look up GP for this
           object.  */ 
        Elf64_Dyn *dyn = map->l_ld;
        *pgp = 0;
        for (; dyn->d_tag != DT_NULL ; dyn++) 
          {
            if (dyn->d_tag == DT_PLTGOT)
              {
                *pgp = dyn->d_un.d_ptr;
                break;
              }
          }
        *pseg_base = seg_base;
        return f;
      }
    }
  return NULL;
}

/* Return a pointer to the unwind table entry for the function containing
   PC.  */  
struct unw_table_entry *
_Unwind_FindTableEntry (void *pc, unsigned long *pseg_base, unsigned long *pgp)
{
  extern rt_r_debug _r_debug;
  struct unw_table_entry *ret;
  rt_link_map *map = _r_debug.r_map; /* address of link map */

  /* Check the main application first, hoping that most of the user's
     code is there instead of in some library.  */
  ret = find_fde_for_dso ((Elf64_Addr)pc, map, pseg_base, pgp);
  if (ret) 
    {
      /* If we're in the main application, use the current GP value.  */
      register unsigned long gp __asm__("gp");
      *pgp = gp;
      return ret;
    }

  /* FIXME need a DSO lock mechanism for AIX here, to ensure shared
     libraries aren't changed while we're examining them.  */

  for (map = _r_debug.r_map; map; map = map->l_next)
    {
      /* Skip the main application's entry.  */
      if (!map->l_name)
      continue;
      ret = find_fde_for_dso ((Elf64_Addr)pc, map, pseg_base, pgp);
      if (ret)
      break;
    }

  /* FIXME need a DSO unlock mechanism for AIX here.  */

  return ret;
}
