/* Generic atexit()
   Copyright (C) 1996 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* Rather than come up with some ugly hack to make mcrt1 work, it is
   better to just go ahead and provide atexit().  */


#include <stdlib.h>


void exit(int) __attribute__((noreturn));
void _exit(int) __attribute__((noreturn));
void _cleanup(void);


#define FNS_PER_BLOCK	32

struct atexit_fn_block
{
  struct atexit_fn_block *next;
  void (*fns[FNS_PER_BLOCK])(void);
  short used;
};


/* staticly allocate the first block */
static struct atexit_fn_block atexit_fns;
static struct atexit_fn_block *current_block = &atexit_fns;


int atexit(void (*fn)(void))
{
  if (current_block->used >= FNS_PER_BLOCK)
    {
      struct atexit_fn_block *new_block = 
	(struct atexit_fn_block *)malloc(sizeof(struct atexit_fn_block));
      if (new_block == NULL)
	return -1;

      new_block->used = 0;
      new_block->next = current_block;
      current_block = new_block;
    }

  current_block->fns[current_block->used++] = fn;

  return 0;
}


void exit(int status)
{
  struct atexit_fn_block *block = current_block, *old_block;
  short i;

  while (1)
    {
      for (i = block->used; --i >= 0 ;)
	(*block->fns[i])();
      if (block == &atexit_fns)
	break;
      /* I know what you are thinking -- we are about to exit, why free?
	 Because it is friendly to memory leak detectors, that's why. */
      old_block = block;
      block = block->next;
      free(old_block);
    }

  _exit(status);
}
