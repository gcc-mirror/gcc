/* Define constants and variables for communication with parse.y.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   2000 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)
   and by Brendan Kehoe (brendan@cygnus.com).

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

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
