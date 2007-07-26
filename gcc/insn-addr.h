/* Macros to support INSN_ADDRESSES
   Copyright (C) 2000, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_INSN_ADDR_H
#define GCC_INSN_ADDR_H

#include "vecprim.h"

extern VEC(int,heap) *insn_addresses_;
extern int insn_current_address;

#define INSN_ADDRESSES(id) (*&(VEC_address (int, insn_addresses_) [id]))
#define INSN_ADDRESSES_ALLOC(size)			\
  do							\
    {							\
      insn_addresses_ = VEC_alloc (int, heap, size);	\
      VEC_safe_grow (int, heap, insn_addresses_, size);	\
      memset (VEC_address (int, insn_addresses_),	\
	      0, sizeof (int) * size);			\
    }							\
  while (0)
#define INSN_ADDRESSES_FREE() (VEC_free (int, heap, insn_addresses_))
#define INSN_ADDRESSES_SET_P() (insn_addresses_ != 0)
#define INSN_ADDRESSES_SIZE() (VEC_length (int, insn_addresses_))

static inline void
insn_addresses_new (rtx insn, int insn_addr)
{
  unsigned insn_uid = INSN_UID ((insn));

  if (INSN_ADDRESSES_SET_P ())
    {
      size_t size = INSN_ADDRESSES_SIZE ();
      if (size <= insn_uid)
	{
	  int *p;
	  VEC_safe_grow (int, heap, insn_addresses_, insn_uid + 1);
	  p = VEC_address (int, insn_addresses_);
	  memset (&p[size],
		  0, sizeof (int) * (insn_uid + 1 - size));
	}
      INSN_ADDRESSES (insn_uid) = insn_addr;
    }
}

#define INSN_ADDRESSES_NEW(insn, addr)		\
  (insn_addresses_new (insn, addr))

#endif /* ! GCC_INSN_ADDR_H */
