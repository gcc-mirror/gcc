/* OSF/rose half-pic support functions.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* The OSF/rose half-pic model assumes that the non-library code does
   not need to have full PIC (position independent code), but rather,
   that pointers to external references are put into the data section
   and derefenced as normal pointers.  References to static data does
   not need to be PIC-ized.

   Another optimization is to have the compiler know what symbols are
   in the shared libraries, and to only lay down the pointers to
   things which in the library proper.  */

#include "config.h"

#ifdef HALF_PIC_INIT

#include "tree.h"
#include "rtl.h"
#include <stdio.h>

extern rtx eliminate_constant_term ();

int flag_half_pic;		/* Global half-pic flag.  */


/* Do any half-pic initializations.  */

void
half_pic_init ()
{
  flag_half_pic = TRUE;
}


/* Encode in a declaration whether or not it is half-pic.  */

void
half_pic_encode (decl)
     tree decl;
{
#if 0
  fprintf (stderr, "\n========== Half_pic_encode\n");
  debug_tree (decl);
#endif
}


/* Return whether an address is half-pic.  */

int
half_pic_address_p (addr)
     rtx addr;
{
  char *name;

  switch (GET_CODE (addr))
    {
    case CONST:
      rtx offset = const0_rtx;
      addr = eliminate_constant_term (addr, &offset);
      if (GET_CODE (addr) != SYMBOL_REF)
	return FALSE;
	
      /* fall through */

    case SYMBOL_REF:
      name = XSTR (addr, 0);

      /* If this is a label, it will have a '*' in front of it.  */
      if (name[0] == '*')
	return FALSE;
    }

  return FALSE;
}

#endif /* HALF_PIC_INIT */
