/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                           I N I T - V X S I M                            *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2015, Free Software Foundation, Inc.         *
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

/* This file is an addition to init.c that must be compiled with the CPU
   specified for running under vxsim for x86-vxworks6, as the signal context
   structure is different for vxsim vs. real hardware.  */

#undef CPU
#define CPU __VXSIM_CPU__

#include "vxWorks.h"
#include "tconfig.h"

#include <signal.h>
#include <taskLib.h>

#ifndef __RTP__
#include <intLib.h>
#include <iv.h>
#endif

extern void
__gnat_map_signal (int sig, siginfo_t *si ATTRIBUTE_UNUSED,
		   void *sc ATTRIBUTE_UNUSED);

/* Process the vxsim signal context.  */
void
__gnat_vxsim_error_handler (int sig, siginfo_t *si, void *sc)
{
  #include "sigtramp.h"

  __gnat_sigtramp_vxsim (sig, (void *)si, (void *)sc,
		   (__sigtramphandler_t *)&__gnat_map_signal);
}
