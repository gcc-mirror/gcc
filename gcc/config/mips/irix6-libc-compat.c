/* Compensate for inconsistent structure passing conventions on IRIX 6.  */
/* Compile this one with gcc.  */
/* Copyright (C) 2001  Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* GCC doesn't correctly implement the structure and union passing and return
   conventions of the N32 and N64 ABIs on IRIX 6, as described in the
   MIPSpro N32 ABI Handbook, ch. 2, Calling Convention Implementations, p.7.
   The ABI requires that structures (or trailing parts of structures) smaller
   than 8 bytes (a 64-bit register) are left-justified, whereas GCC
   right-justifies them.

   While GCC is internally consistent, calling routines compiled with a
   compiler that does implement the documented ABI (like SGIs MIPSpro C
   compiler) doesn't work.  This is primarily an issue for system libraries
   like libc.  Fortunately, there exist only very few routines that take
   structure value arguments or return structures by value, so until the
   underlying bug is fixed, it is possible to work around it by providing
   wrapper functions for the few affected routines that compensate for the
   inconsistent alignment.

   These wrappers rely on the fact that e.g. libc contains weak versions of
   those routines, and the real implementation is provided by _-prefixed
   variants.  So we can provide our own versions, which will only be linked
   if the application uses any of the affected functions, calling the private
   variants after shifting the arguments or results as required.

   This is a rewrite of code created by Andy Polyakov.  */

#include "config.h"
#include "system.h"

/* This must only be used for the N32 and N64 ABIs.  O32 is correct.  */

#if _MIPS_SIM == _ABIN32 || _MIPS_SIM == _ABI64

/* The affected arguments need to be shifted by

	BITS_PER_WORD - (sizeof (arg) * BITS_PER_UNIT).

   Since only 32-bit args and results are involved, the shift count is
   always 32.  */
#define SHIFT_BITS	32

extern machreg_t inet_ntoa	PARAMS ((machreg_t));
extern machreg_t inet_lnaof	PARAMS ((machreg_t));
extern machreg_t inet_netof	PARAMS ((machreg_t));
extern machreg_t inet_makeaddr	PARAMS ((machreg_t, machreg_t));

extern machreg_t _inet_ntoa	PARAMS ((machreg_t));
extern machreg_t _inet_lnaof	PARAMS ((machreg_t));
extern machreg_t _inet_netof	PARAMS ((machreg_t));
extern machreg_t _inet_makeaddr	PARAMS ((machreg_t, machreg_t));

/* <arpa/inet.h> has

	char *inet_ntoa (struct in_addr);

   on both IRIX 6.2 and 6.5, with struct in_addr containing a 32-bit int.  */

machreg_t
inet_ntoa (machreg_t in)
{
  return _inet_ntoa (in << SHIFT_BITS);
}

/* <arpa/inet.h> has

	unsigned long inet_lnaof (struct in_addr);		(IRIX 6.2)
	in_addr_t inet_lnaof (struct in_addr);			(IRIX 6.5)

   in_addr_t is a 32-bit int.  */

machreg_t
inet_lnaof (machreg_t in)
{
  return _inet_lnaof (in << SHIFT_BITS);
}

/* <arpa/inet.h> has

	unsigned long inet_netof (struct in_addr);		(IRIX 6.2)
	in_addr_t inet_netof (struct in_addr);			(IRIX 6.5)  */

machreg_t
inet_netof (machreg_t in)
{
  return _inet_netof (in << SHIFT_BITS);
}

/* <arpa/inet.h> has

	struct in_addr inet_makeaddr (int, int);		(IRIX 6.2)
	struct in_addr inet_makeaddr (in_addr_t, in_addr_t);	(IRIX 6.5)  */

machreg_t
inet_makeaddr (machreg_t net, machreg_t lna)
{
  return _inet_makeaddr (net, lna) >> SHIFT_BITS;
}

#if _MIPS_SIM == _ABIN32
extern machreg_t semctl		PARAMS ((machreg_t, machreg_t, machreg_t, machreg_t));
extern machreg_t _semctl	PARAMS ((machreg_t, machreg_t, machreg_t, machreg_t));

/* <sys/sem.h> has

	int semctl (int, int, int, ...);

   where the variadic argument is union semun if used.  union semun contains
   an int and two pointers, so the union is already 64 bits wide under the
   N64 ABI and alignment is not an issue.  */

machreg_t
semctl (machreg_t semid, machreg_t semnum, machreg_t cmd, machreg_t arg)
{
  return _semctl(semid, semnum, cmd, arg << SHIFT_BITS);
}
#endif /* _ABIN32 */

#endif /* _ABIN32 || _ABI64 */
