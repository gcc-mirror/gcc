/* Define constants and variables for communication with the parser.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)
   and by Brendan Kehoe (brendan@cygnus.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */


#ifndef GCC_CP_LEX_H
#define GCC_CP_LEX_H

#if 0
/* Formerly, the RID_* values used as mask bits did not fit into a
   single 32-bit word.  Now they do, but let's preserve the old logic
   in case they ever stop fitting again.  -zw, 8 Aug 2000 */

/* The type that can represent all values of RIDBIT.  */
/* We assume that we can stick in at least 32 bits into this.  */
typedef struct { unsigned long idata[2]; }
     RID_BIT_TYPE;

/* Be careful, all these modify N twice.  */
#define RIDBIT_SETP(N, V) (((unsigned long)1 << (int) ((N)%32))		      \
			    & (V).idata[(N)/32])
#define RIDBIT_NOTSETP(NN, VV) (! RIDBIT_SETP (NN, VV))
#define RIDBIT_SET(N, V) do {						      \
				(V).idata[(N)/32]			      \
				  |= ((unsigned long)1 << (int) ((N)%32));    \
			      } while (0)
#define RIDBIT_RESET(N, V) do {						      \
				  (V).idata[(N)/32]			      \
				    &= ~((unsigned long)1 << (int) ((N)%32)); \
				} while (0)
#define RIDBIT_RESET_ALL(V) do {					      \
				   (V).idata[0] = 0;     		      \
				   (V).idata[1] = 0;			      \
				 } while (0)
#define RIDBIT_ANY_SET(V) ((V).idata[0] || (V).idata[1])
#else
typedef unsigned long RID_BIT_TYPE;	/* assumed at least 32 bits */
#define RIDBIT_OF(R) ((unsigned long)1 << (int) (R))

#define RIDBIT_SETP(N, V) ((V) & RIDBIT_OF (N))
#define RIDBIT_NOTSETP(N, V) (! ((V) & RIDBIT_OF (N)))
#define RIDBIT_ANY_SET(V) (V)

#define RIDBIT_SET(N, V) do { (V) |= RIDBIT_OF (N); } while (0)
#define RIDBIT_RESET(N, V) do { (V) &= ~RIDBIT_OF (N); } while (0)
#define RIDBIT_RESET_ALL(V) do { (V) = 0; } while (0)
#endif

#endif /* ! GCC_CP_LEX_H */
