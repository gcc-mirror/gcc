/* Tru64 UNIX host-specific hook definitions.
   Copyright (C) 2011 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include <sys/mman.h>
/* Inhibit inclusion of <sys/mount.h>, unnecessary and errors out due to
   use of poisoned bcmp, bcopy.  */
#define _SYS_MOUNT_H_
#include <sys/procfs.h>
#include "hosthooks.h"
#include "hosthooks-def.h"


#undef HOST_HOOKS_GT_PCH_GET_ADDRESS
#define HOST_HOOKS_GT_PCH_GET_ADDRESS osf_gt_pch_get_address
#undef HOST_HOOKS_GT_PCH_USE_ADDRESS
#define HOST_HOOKS_GT_PCH_USE_ADDRESS osf_gt_pch_use_address

/* The mmap ADDR parameter may be ignored without MAP_FIXED set.  Before we
   give up, check existing mappings with ioctl(PIOCMAP) to see if the space
   is really free.  */

static void *
mmap_fixed (void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
  void *base;

  base = mmap ((caddr_t) addr, len, prot, flags, fd, off);
  
  if (base != addr)
    {
      /* PID_MAX is SHRT_MAX on Tru64 UNIX V4.0, but INT_MAX on V5.1.
	 Allow for both.  "/proc/" + INT_MAX + '\0'.  */
      char pname[6+10+1];
      int procfd, nmap;
      prmap_t *pmap;
      int i, overlap = 0;

      if (base != (void *) MAP_FAILED)
	munmap ((caddr_t) base, len);

      /* Check if there's any mapping overlapping [addr, addr+len).  */

      snprintf (pname, sizeof (pname), "/proc/%d", getpid ());
      procfd = open (pname, O_RDONLY);
      if (procfd == -1)
	return ((void *) MAP_FAILED);
      if (ioctl (procfd, PIOCNMAP, &nmap) == -1)
	return ((void *) MAP_FAILED);
      pmap = (prmap_t *) xmalloc (sizeof (*pmap) * (nmap+1));
      if (ioctl (procfd, PIOCMAP, pmap) == -1)
	return ((void *) MAP_FAILED);

      /* It seems like pmap[] is sorted by address, but can we rely on
	 that?  */
      for (i = 0; i < nmap; i++)
	{
	  unsigned long map_start = (unsigned long) pmap[i].pr_vaddr;
	  unsigned long map_end = map_start + pmap[i].pr_size;

	  if ((unsigned long) addr < map_end
	      && (unsigned long) addr+len > map_start)
	    {
	      overlap = 1;
	      break;
	    }
	}
      free (pmap);
      close (procfd);

      if (!overlap)
	base = mmap ((caddr_t) addr, len, prot, flags | MAP_FIXED, fd, off);
      else
	base = mmap ((caddr_t) addr, len, prot, flags, fd, off);
    }

  return base;
}

/* For various ports, try to guess a fixed spot in the vm space that's
   probably free.  Take the middle between start of text segment and
   dynamic loader space.  See <sys/machine/addrconf.h> and Tru64 UNIX
   Assembly Language Programmer's Guide, p.6-18, Figure 6-3: Default Layout
   of Memory (User Program View).  */
#define TRY_EMPTY_VM_SPACE	0x20050000000

/* Determine a location where we might be able to reliably allocate
   SIZE bytes.  FD is the PCH file, though we should return with the
   file unmapped.  */

static void *
osf_gt_pch_get_address (size_t size, int fd)
{
  void *addr;

  addr = mmap_fixed ((caddr_t) TRY_EMPTY_VM_SPACE, size,
		     PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

  /* If we failed the map, that means there's *no* free space.  */
  if (addr == (void *) MAP_FAILED)
    return NULL;
  /* Unmap the area before returning.  */
  munmap ((caddr_t) addr, size);

  return addr;
}

/* Map SIZE bytes of FD+OFFSET at BASE.  Return 1 if we succeeded at 
   mapping the data at BASE, -1 if we couldn't.  */

static int
osf_gt_pch_use_address (void *base, size_t size, int fd, size_t offset)
{
  void *addr;

  /* We're called with size == 0 if we're not planning to load a PCH
     file at all.  This allows the hook to free any static space that
     we might have allocated at link time.  */
  if (size == 0)
    return -1;

  addr = mmap_fixed ((caddr_t) base, size,
		     PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, offset);

  return addr == base ? 1 : -1;
}


const struct host_hooks host_hooks = HOST_HOOKS_INITIALIZER;
