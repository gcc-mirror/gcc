/* Linux host-specific hook definitions.
   Copyright (C) 2004 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the
   Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include <sys/mman.h>
#include "hosthooks.h"
#include "hosthooks-def.h"


/* Linux has a feature called exec-shield-randomize that perturbs the
   address of non-fixed mapped segments by a (relatively) small amount.
   The feature is intended to make it harder to attack the system with
   buffer overflow attacks, since every invocation of a program will
   have its libraries and data segments at slightly different addresses.

   This feature causes us problems with PCH because it makes it that
   much harder to acquire a stable location at which to map our PCH
   data file.

   [ The feature causes other points of non-determinism within the
     compiler as well, so we'd *really* like to be able to have the
     driver disable exec-shield-randomize for the process group, but
     that isn't possible at present.  ]

   We're going to try several things:

      * Select an architecture specific address as "likely" and see
	if that's free.  For our 64-bit hosts, we can easily choose
	an address in Never Never Land.

      * If exec-shield-randomize is disabled, then just use the
	address chosen by mmap in step one.

      * If exec-shield-randomize is enabled, then temporarily allocate
	32M of memory as a buffer, then allocate PCH memory, then
	free the buffer.  The theory here is that the perturbation is
	no more than 16M, and so by allocating our buffer larger than
	that we make it considerably more likely that the address will
	be free when we want to load the data back.
*/

#undef HOST_HOOKS_GT_PCH_GET_ADDRESS
#define HOST_HOOKS_GT_PCH_GET_ADDRESS linux_gt_pch_get_address

/* For various ports, try to guess a fixed spot in the vm space
   that's probably free.  */
#if defined(__alpha)
# define TRY_EMPTY_VM_SPACE	0x10000000000
#elif defined(__ia64)
# define TRY_EMPTY_VM_SPACE	0x2000000100000000
#elif defined(__x86_64)
# define TRY_EMPTY_VM_SPACE	0x1000000000
#elif defined(__i386)
# define TRY_EMPTY_VM_SPACE	0x60000000
#elif defined(__powerpc__)
# define TRY_EMPTY_VM_SPACE	0x60000000
#elif defined(__s390x__)
# define TRY_EMPTY_VM_SPACE	0x8000000000
#elif defined(__s390__)
# define TRY_EMPTY_VM_SPACE	0x60000000
#else
# define TRY_EMPTY_VM_SPACE	0
#endif

/* Determine a location where we might be able to reliably allocate SIZE
   bytes.  FD is the PCH file, though we should return with the file 
   unmapped.  */

static void *
linux_gt_pch_get_address (size_t size, int fd)
{
  size_t buffer_size = 32 * 1024 * 1024;
  void *addr, *buffer;
  FILE *f;
  bool randomize_on;

  addr = mmap ((void *)TRY_EMPTY_VM_SPACE, size, PROT_READ | PROT_WRITE,
	       MAP_PRIVATE, fd, 0);

  /* If we failed the map, that means there's *no* free space.  */
  if (addr == (void *) MAP_FAILED)
    return NULL;
  /* Unmap the area before returning.  */
  munmap (addr, size);

  /* If we got the exact area we requested, then that's great.  */
  if (TRY_EMPTY_VM_SPACE && addr == (void *) TRY_EMPTY_VM_SPACE)
    return addr;

  /* If we didn't, then we need to look to see if randomization is on.  */
  f = fopen ("/proc/sys/kernel/exec-shield-randomize", "r");
  randomize_on = false;
  if (f != NULL)
    {
      char buf[100];
      size_t c;

      c = fread (buf, 1, sizeof buf - 1, f);
      if (c > 0)
	{
	  buf[c] = '\0';
	  randomize_on = (atoi (buf) > 0);
	}
      fclose (f);
    }

  /* If it isn't, then accept the address that mmap selected as fine.  */
  if (!randomize_on)
    return addr;

  /* Otherwise, we need to try again with buffer space.  */
  buffer = mmap (0, buffer_size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
  addr = mmap (0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
  if (buffer != (void *) MAP_FAILED)
    munmap (buffer, buffer_size);
  if (addr == (void *) MAP_FAILED)
    return NULL;
  munmap (addr, size);

  return addr;
}


const struct host_hooks host_hooks = HOST_HOOKS_INITIALIZER;
