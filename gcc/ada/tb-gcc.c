/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                   T R A C E B A C K - G C C t a b l e s                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2004-2007, AdaCore                     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is an implementation of the __gnat_backtrace routine using the
   underlying GCC unwinding support associated with the exception handling
   infrastructure.  This will only work for ZCX based applications.  */

#include <unwind.h>

/* The implementation boils down to a call to _Unwind_Backtrace with a
   tailored callback and carried-on datastructure to keep track of the
   input parameters we got as well as of the basic processing state.  */

/******************
 * trace_callback *
 ******************/

#if !defined (__USING_SJLJ_EXCEPTIONS__)

typedef struct {
  void ** traceback;
  int max_len;
  void * exclude_min;
  void * exclude_max;
  int  n_frames_to_skip;
  int  n_frames_skipped;
  int  n_entries_filled;
} uw_data_t;

#if defined (__ia64__) && defined (__hpux__)
#include <uwx.h>
#endif

static _Unwind_Reason_Code
trace_callback (struct _Unwind_Context * uw_context, uw_data_t * uw_data)
{
  void * pc;

#if defined (__ia64__) && defined (__hpux__)
  /* Work around problem with _Unwind_GetIP on ia64 HP-UX. */
  uwx_get_reg ((struct uwx_env *) uw_context, UWX_REG_IP, (uint64_t *) &pc);
#else
  pc = (void *) _Unwind_GetIP (uw_context);
#endif

  if (uw_data->n_frames_skipped < uw_data->n_frames_to_skip)
    {
      uw_data->n_frames_skipped ++;
      return _URC_NO_REASON;
    }

  if (uw_data->n_entries_filled >= uw_data->max_len)
    return _URC_NORMAL_STOP;

  if (pc < uw_data->exclude_min || pc > uw_data->exclude_max)
    uw_data->traceback [uw_data->n_entries_filled ++] = pc + PC_ADJUST;

  return _URC_NO_REASON;
}

#endif

/********************
 * __gnat_backtrace *
 ********************/

int
__gnat_backtrace (void ** traceback, int max_len,
		  void * exclude_min, void * exclude_max,
		  int  skip_frames)
{
#if defined (__USING_SJLJ_EXCEPTIONS__)
  /* We have no unwind material (tables) at hand with sjlj eh, and no
     way to retrieve complete and accurate call chain information from
     the context stack we maintain.  */
  return 0;
#else
  uw_data_t uw_data;
  /* State carried over during the whole unwinding process.  */

  uw_data.traceback   = traceback;
  uw_data.max_len     = max_len;
  uw_data.exclude_min = exclude_min;
  uw_data.exclude_max = exclude_max;

  uw_data.n_frames_to_skip = skip_frames;

  uw_data.n_frames_skipped = 0;
  uw_data.n_entries_filled = 0;

  _Unwind_Backtrace ((_Unwind_Trace_Fn)trace_callback, &uw_data);

  return uw_data.n_entries_filled;
#endif
}
