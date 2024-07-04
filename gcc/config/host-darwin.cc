/* Darwin host-specific hook definitions.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.

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
#include "options.h"
#include "diagnostic-core.h"
#include "config/host-darwin.h"
#include <errno.h>

/* For Darwin (macOS only) platforms, without ASLR (PIE) enabled on the
   binaries, the following VM addresses are expected to be available.
   NOTE, that for aarch64, ASLR is always enabled - but the VM address
   mentioned below is available (at least on Darwin20).

   The spaces should all have 512Mb available c.f. PCH files for large
   C++ or Objective-C in the range of 150Mb for 64b hosts.

   We also try to steer clear of places already used for sanitizers.

   If the allocation fails at the 'ideal' address, we go with what the
   kernel provides (there is more likelihood that we will need to relocate
   on read in).  */

#define PAGE_SZ 4096
#if defined(__x86_64) && defined(__LP64__)
# define TRY_EMPTY_VM_SPACE	0x180000000000ULL
#elif defined(__x86_64)
# define TRY_EMPTY_VM_SPACE	0x00006fe00000ULL
#elif defined(__i386)
# define TRY_EMPTY_VM_SPACE	0x00006fe00000ULL
#elif defined(__POWERPC__) && defined(__LP64__)
# define TRY_EMPTY_VM_SPACE	0x180000000000ULL
#elif defined(__POWERPC__)
# define TRY_EMPTY_VM_SPACE	0x00006fe00000ULL
#elif defined(__aarch64__)
# undef PAGE_SZ
# define PAGE_SZ 16384
# define TRY_EMPTY_VM_SPACE	0x180000000000ULL
#else
# error "unknown Darwin target"
#endif

/* Try to map a known position in the VM.  The current PCH implementation
   can adjust values at write-time, but not at read-time thus we need to
   pick up the same position when reading as we got at write-time.  */

void *
darwin_gt_pch_get_address (size_t sz, int fd)
{
  /* First try with the constraint that we really want this address...  */
  void *addr = mmap ((void *)TRY_EMPTY_VM_SPACE, sz, PROT_READ | PROT_WRITE,
		     MAP_PRIVATE | MAP_FIXED, fd, 0);

  if (addr != (void *) MAP_FAILED)
    munmap (addr, sz);

  /* This ought to be the only alternative to failure, but there are comments
     that suggest some versions of mmap can be buggy and return a different
     value.  */
  if (addr == (void *) TRY_EMPTY_VM_SPACE)
    return addr;

  /* OK try to find a space without the constraint.  */
  addr = mmap ((void *) TRY_EMPTY_VM_SPACE, sz, PROT_READ | PROT_WRITE,
	       MAP_PRIVATE, fd, 0);

  /* We return whatever the kernel gave us.  */
  if (addr != (void *) MAP_FAILED)
    {
      /* Unmap the area before returning.  */
      munmap (addr, sz);
      return addr;
    }

  /* Otherwise, try again but put some arbitrary buffer space first.  */
  size_t buffer_size = 64 * 1024 * 1024;
  void *buffer = mmap (0, buffer_size, PROT_NONE,
		       MAP_PRIVATE | MAP_ANON, -1, 0);
  addr = mmap ((void *)TRY_EMPTY_VM_SPACE, sz, PROT_READ | PROT_WRITE,
		MAP_PRIVATE, fd, 0);

  if (buffer != (void *) MAP_FAILED)
    munmap (buffer, buffer_size);

  /* If we failed this time, that means there is *no* large enough free
     space.  */
  if (addr == (void *) MAP_FAILED)
    {
      error ("PCH memory is not available: %m");
      return NULL;
    }

  munmap (addr, sz);
  return addr;
}

/* Try to mmap the PCH file at ADDR for SZ bytes at OFF offset in the file.
   If we succeed return 1, if we cannot mmap the desired address, then we
   fail with -1.  */

int
darwin_gt_pch_use_address (void *&addr, size_t sz, int fd, size_t off)
{
  void *mapped_addr;

  /* We're called with size == 0 if we're not planning to load a PCH
     file at all.  This allows the hook to free any static space that
     we might have allocated at link time.  */
  if (sz == 0)
    return -1;

  gcc_checking_assert (!(off % PAGE_SZ));

  /* Try to map the file with MAP_PRIVATE and FIXED.  */
  mapped_addr = mmap (addr, sz, PROT_READ | PROT_WRITE,
		      MAP_PRIVATE | MAP_FIXED, fd, (off_t) off);

  /* Hopefully, we succeed.  */
  if (mapped_addr == addr)
    return 1;

  /* In theory, the only alternative to success for MAP_FIXED should be FAILED
     however, there are some buggy earlier implementations that could return
     an address.  */
  if (mapped_addr != (void *) MAP_FAILED)
    munmap (mapped_addr, sz);

  /* Try to map the file with MAP_PRIVATE but let the kernel move it.  */
  mapped_addr = mmap (addr, sz, PROT_READ | PROT_WRITE,
		      MAP_PRIVATE, fd, (off_t) off);

  /* Hopefully, we succeed.  */
  if (mapped_addr != (void *) MAP_FAILED)
    {
      addr = mapped_addr;
      return 1;
    }

  /* Try to make an anonymous private mmap at the desired location in case
     the problem is in mapping the file.  */
  mapped_addr = mmap (addr, sz, PROT_READ | PROT_WRITE,
		      MAP_PRIVATE | MAP_ANON, -1, (off_t)0);

  /* If this fails, we are out of ideas (and maybe memory).  */
  if (mapped_addr == (void *) MAP_FAILED)
    return -1;

  addr = mapped_addr;

  if (lseek (fd, off, SEEK_SET) == (off_t) -1)
    return -1;

  while (sz)
    {
      ssize_t nbytes;

      nbytes = read (fd, mapped_addr, MIN (sz, (size_t) -1 >> 1));
      if (nbytes <= 0)
	return -1;
      mapped_addr = (char *) mapped_addr + nbytes;
      sz -= nbytes;
    }

  return 1;
}
