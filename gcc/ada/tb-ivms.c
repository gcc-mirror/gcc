/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                 T R A C E B A C K - I t a n i u m  / V M S               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2007-2011, AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Itanium Open/VMS implementation of backtrace.  Use ICB (Invocation
   Context Block) routines.  */
#include <stdlib.h>
#include <vms/libicb.h>

/* Declare libicb routines.  */
extern INVO_CONTEXT_BLK *LIB$I64_CREATE_INVO_CONTEXT (void *(*)(size_t),
						      void (*)(void *),
						      int);
extern void LIB$I64_FREE_INVO_CONTEXT (INVO_CONTEXT_BLK *);
extern int LIB$I64_GET_CURR_INVO_CONTEXT(INVO_CONTEXT_BLK *);
extern int LIB$I64_GET_PREV_INVO_CONTEXT(INVO_CONTEXT_BLK *);

/* Gcc internal headers poison malloc.  So use xmalloc() when building the
   compiler.  */
#ifdef IN_RTS
#define BT_MALLOC malloc
#else
#define BT_MALLOC xmalloc
#endif

int
__gnat_backtrace (void **array, int size,
                  void *exclude_min, void *exclude_max, int skip_frames)
{
  INVO_CONTEXT_BLK *ctxt;
  int res = 0;
  int n = 0;

  /* Create the context.  */
  ctxt = LIB$I64_CREATE_INVO_CONTEXT (BT_MALLOC, free, 0);
  if (ctxt == NULL)
    return 0;

  LIB$I64_GET_CURR_INVO_CONTEXT (ctxt);

  while (1)
    {
      void *pc = (void *)ctxt->libicb$ih_pc;
      if (pc == (void *)0)
	break;
      if (ctxt->libicb$v_bottom_of_stack)
	break;
      if (n >= skip_frames && (pc < exclude_min || pc > exclude_max))
	{
	  array[res++] = (void *)(ctxt->libicb$ih_pc);
	  if (res == size)
	    break;
	}
      n++;
      LIB$I64_GET_PREV_INVO_CONTEXT (ctxt);
    }

  /* Free the context.  */
  LIB$I64_FREE_INVO_CONTEXT (ctxt);
  return res;
}
