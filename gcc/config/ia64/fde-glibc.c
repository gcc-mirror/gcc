/* Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@cygnus.com>.

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

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* Locate the FDE entry for a given address, using glibc ld.so routines
   to avoid register/deregister calls at DSO load/unload.  */

#include <stdlib.h>
#include <link.h>
#include <bits/libc-lock.h>
#include "unwind-ia64.h"


/* Initialized by crtbegin from the main application.  */
extern Elf64_Ehdr *__ia64_app_header;

/* ??? A redeclaration of the lock in ld.so.  Perhaps this should
   appear in <link.h> in a new glibc version.  */
__libc_lock_define (extern, _dl_load_lock)

/* This always exists, even in a static application.  */
extern struct link_map *_dl_loaded;

static struct unw_table_entry *
find_fde_for_dso (Elf64_Addr pc, Elf64_Ehdr *ehdr,
		  unsigned long *pseg_base, unsigned long *pgp)
{
  Elf64_Phdr *phdr, *p_unwind, *p_dynamic;
  long n, match;
  Elf64_Addr load_base, seg_base;
  struct unw_table_entry *f_base, *f;
  size_t lo, hi;

  /* Verify that we are looking at an ELF header.  */
  if (ehdr->e_ident[0] != 0x7f
      || ehdr->e_ident[1] != 'E'
      || ehdr->e_ident[2] != 'L'
      || ehdr->e_ident[3] != 'F'
      || ehdr->e_ident[EI_CLASS] != ELFCLASS64
      || ehdr->e_ident[EI_DATA] != ELFDATA2LSB
      || ehdr->e_machine != EM_IA_64)
    abort ();

  match = 0;
  phdr = (Elf64_Phdr *)((char *)ehdr + ehdr->e_phoff);
  load_base = (ehdr->e_type == ET_DYN ? (Elf64_Addr)ehdr : 0);
  p_unwind = NULL;
  p_dynamic = NULL;

  /* See if PC falls into one of the loaded segments.  Find the unwind
     segment at the same time.  */
  for (n = ehdr->e_phnum; --n >= 0; phdr++)
    {
      if (phdr->p_type == PT_LOAD)
	{
	  Elf64_Addr vaddr = phdr->p_vaddr + load_base;
	  if (pc >= vaddr && pc < vaddr + phdr->p_memsz)
	    match = 1;
	}
      else if (phdr->p_type == PT_IA_64_UNWIND)
	p_unwind = phdr;
      else if (phdr->p_type == PT_DYNAMIC)
	p_dynamic = phdr;
    }
  if (!match || !p_unwind)
    return NULL;

  /* Search for the FDE within the unwind segment.  */

  f_base = (struct unw_table_entry *) (p_unwind->p_vaddr + load_base);
  seg_base = (Elf64_Addr) ehdr;
  lo = 0;
  hi = p_unwind->p_memsz / sizeof (struct unw_table_entry);

  while (lo < hi)
    {
      size_t mid = (lo + hi) / 2;

      f = f_base + mid;
      if (pc < f->start_offset + seg_base)
	hi = mid;
      else if (pc >= f->end_offset + seg_base)
	lo = mid + 1;
      else
        goto found;
    }
  return NULL;

 found:
  *pseg_base = seg_base;
  *pgp = 0;

  if (p_dynamic)
    {
      /* For dynamicly linked executables and shared libraries,
	 DT_PLTGOT is the gp value for that object.  */
      Elf64_Dyn *dyn = (Elf64_Dyn *)(p_dynamic->p_vaddr + load_base);
      for (; dyn->d_tag != DT_NULL ; dyn++)
	if (dyn->d_tag == DT_PLTGOT)
	  {
	    /* ??? Glibc seems to have relocated this already.  */
	    *pgp = dyn->d_un.d_ptr;
	    break;
	  }
    }
  else
    {
      /* Otherwise this is a static executable with no _DYNAMIC.
	 The gp is constant program-wide.  */
      register unsigned long gp __asm__("gp");
      *pgp = gp;
    }

  return f;
}

/* Return a pointer to the unwind table entry for the function
   containing PC.  */

struct unw_table_entry *
_Unwind_FindTableEntry (void *pc, unsigned long *segment_base,
                        unsigned long *gp)
{
  struct unw_table_entry *ret;
  struct link_map *map;

  /* Check the main application first, hoping that most of the user's
     code is there instead of in some library.  */
  ret = find_fde_for_dso ((Elf64_Addr)pc, __ia64_app_header,
			  segment_base, gp);
  if (ret)
    return ret;

  /* Glibc is probably unique in that we can (with certain restrictions)
     dynamicly load libraries into staticly linked applications.  Thus
     we _always_ check _dl_loaded.  */

  __libc_lock_lock (_dl_load_lock);

  for (map = _dl_loaded; map ; map = map->l_next)
    {
      /* Skip the main application's entry.  */
      if (map->l_name[0] == 0)
	continue;
      ret = find_fde_for_dso ((Elf64_Addr)pc, (Elf64_Ehdr *)map->l_addr,
			      segment_base, gp);
      if (ret)
	break;
    }

  __libc_lock_unlock (_dl_load_lock);

  return ret;
}
