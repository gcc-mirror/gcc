/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            T R A C E B A C K                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 2000-2004 Ada Core Technologies, Inc.            *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
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

/* This file contains low level support for stack unwinding using GCC intrinsic
   functions.
   It has been tested on the following configurations:
   PowerPC/AiX
   PowerPC/VxWorks
   Sparc/Solaris
   i386/GNU/Linux
   i386/Solaris
   i386/NT
   i386/OS2
   i386/LynxOS
   Alpha/VxWorks
   Alpha/VMS
*/

#ifdef __alpha_vxworks
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#define POSIX
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

extern int __gnat_backtrace (void **, int, void *, void *, int);

/* The point is to provide an implementation of the __gnat_backtrace function
   above, called by the default implementation of the System.Traceback package.

   We first have a series of target specific implementations, each included
   from a separate C file for readability purposes.

   Then come two flavors of a generic implementation: one relying on static
   assumptions about the frame layout, and the other one using the GCC EH
   infrastructure.  The former uses a whole set of macros and structures which
   may be tailored on a per target basis, and is activated as soon as
   USE_GENERIC_UNWINDER is defined.  The latter uses a small subset of the
   macro definitions and is activated when USE_GCC_UNWINDER is defined. It is
   only available post GCC 3.3.

   Finally, there is a default dummy implementation, necessary to make the
   linker happy on platforms where the feature is not supported, but where the
   function is still referenced by the default System.Traceback.  */

#define Lock_Task system__soft_links__lock_task
extern void (*Lock_Task) (void);

#define Unlock_Task system__soft_links__unlock_task
extern void (*Unlock_Task) (void);

/*-------------------------------------*
 *-- Target specific implementations --*
 *-------------------------------------*/

#if defined (__alpha_vxworks)

#include "tb-alvxw.c"

#elif defined (__ALPHA) && defined (__VMS__)

#include "tb-alvms.c"

#else
/* No target specific implementation.  */

/*----------------------------------------------------------------*
 *-- Target specific definitions for the generic implementation --*
 *----------------------------------------------------------------*/

/* The stack layout is specified by the target ABI. The "generic" scheme is
   based on the following assumption:

     The stack layout from some frame pointer is such that the information
     required to compute the backtrace is available at static offsets.

   For a given frame, the information we are interested in is the saved return
   address (somewhere after the call instruction in the caller) and a pointer
   to the caller's frame. The former is the base of the call chain information
   we store in the tracebacks array. The latter allows us to loop over the
   successive frames in the chain.

   To initiate the process, we retrieve an initial frame pointer using the
   appropriate GCC builtin (__builtin_frame_address).

   This scheme is unfortunately not applicable on every target because the
   stack layout is not necessarily regular (static) enough. On targets where
   this scheme applies, the implementation relies on the following items:

   o struct layout, describing the expected stack data layout relevant to the
     information we are interested in,

   o FRAME_OFFSET, the offset, from a given frame pointer, at which this
     layout will be found,

   o FRAME_LEVEL, controls how many frames up we get at to start with,
     from the initial frame pointer we compute by way of the GCC builtin,

     0 is most often the appropriate value. 1 may be necessary on targets
     where return addresses are saved by a function in it's caller's frame
     (e.g. PPC).

   o PC_ADJUST, to account for the difference between a call point (address
     of a call instruction), which is what we want in the output array, and
     the associated return address, which is what we retrieve from the stack.

   o STOP_FRAME, to decide wether we reached the top of the call chain, and
     thus if the process shall stop.

	   :
	   :                   stack
	   |             +----------------+
	   |   +-------->|       :        |
	   |   |         | (FRAME_OFFSET) |
	   |   |         |       :        |  (PC_ADJUST)
	   |   |  layout:| return_address ----------------+
	   |   |         |     ....       |               |
	   +---------------  next_frame   |               |
	       |         |     ....       |               |
	       |         |                |               |
	       |         +----------------+               |  +-----+
	       |         |       :        |<- Base fp     |  |  :  |
	       |         | (FRAME_OFFSET) | (FRAME_LEVEL) |  |  :  |
	       |         |       :        |               +--->    | [1]
	       |  layout:| return_address -------------------->    | [0]
	       |         |       ...      |  (PC_ADJUST)     +-----+
	       +----------   next_frame   |                 traceback[]
		         |       ...      |
		         |                |
		         +----------------+

   o BASE_SKIP,

   Since we inherently deal with return addresses, there is an implicit shift
   by at least one for the initial point we are able to observe in the chain.

   On some targets (e.g. sparc-solaris), the first return address we can
   easily get without special code is even our caller's return address, so
   there is a initial shift of two.

   BASE_SKIP represents this initial shift, which is the minimal "skip_frames"
   value we support. We could add special code for the skip_frames < BASE_SKIP
   cases. This is not done currently because there is virtually no situation
   in which this would be useful.

   Finally, to account for some ABI specificities, a target may (but does
   not have to) define:

   o FORCE_CALL, to force a call to a dummy function at the very beginning
     of the computation. See the PPC AIX target for an example where this
     is useful.

   o FETCH_UP_FRAME, to force an invocation of __builtin_frame_address with a
     positive argument right after a possibly forced call even if FRAME_LEVEL
     is 0. See the Sparc Solaris case for an example where this is useful.

  */

/*------------------------------ PPC AIX -------------------------------*/

#if defined (_AIX)

#define USE_GENERIC_UNWINDER

struct layout
{
  struct layout *next;
  void *pad;
  void *return_address;
};

#define FRAME_OFFSET 0
#define PC_ADJUST -4
#define STOP_FRAME(CURRENT, TOP_STACK) ((void *) (CURRENT) < (TOP_STACK))

/* The PPC ABI has an interesting specificity: the return address saved by a
   function is located in it's caller's frame, and the save operation only
   takes place if the function performs a call.

   To have __gnat_backtrace retrieve it's own return address, we then
   define ... */

#define FORCE_CALL
#define FRAME_LEVEL 1

#define BASE_SKIP 1

/*---------------------------- PPC VxWorks------------------------------*/

#elif defined (_ARCH_PPC) && defined (__vxworks)

#define USE_GENERIC_UNWINDER

struct layout
{
  struct layout *next;
  void *return_address;
};

#define FORCE_CALL
#define FRAME_LEVEL 1
/* See the PPC AIX case for an explanation of these values.  */

#define FRAME_OFFSET 0
#define PC_ADJUST -4
#define STOP_FRAME(CURRENT, TOP_STACK) ((CURRENT)->next == 0)

#define BASE_SKIP 1

/*-------------------------- Sparc Solaris -----------------------------*/

#elif defined (sun) && defined (sparc)

#define USE_GENERIC_UNWINDER

/* These definitions are inspired from the Appendix D (Software
   Considerations) of the SPARC V8 architecture manual.  */

struct layout
{
  struct layout *next;
  void *return_address;
};

#define FRAME_LEVEL 0
#define FRAME_OFFSET (14 * (sizeof (void*)))
#define PC_ADJUST 0
#define STOP_FRAME(CURRENT, TOP_STACK) \
  ((CURRENT)->return_address == 0|| (CURRENT)->next == 0 \
   || (void *) (CURRENT) < (TOP_STACK))

/* The sparc register windows need to be flushed before we may access them
   from the stack. This is achieved by way of builtin_frame_address only
   when the "count" argument is positive, so force at least one such call.  */
#define FETCH_UP_FRAME_ADDRESS

#define BASE_SKIP 2
/* From the frame pointer of frame N, we are accessing the flushed register
   window of frame N-1 (positive offset from fp), in which we retrieve the
   saved return address. We then end up with our caller's return address.  */

/*------------------------------- x86 ----------------------------------*/

#elif defined (i386)

#ifdef __WIN32
#include <windows.h>
#define IS_BAD_PTR(ptr) (IsBadCodePtr((void *)ptr))
#else
#define IS_BAD_PTR(ptr) 0
#endif

#define USE_GENERIC_UNWINDER

struct layout
{
  struct layout *next;
  void *return_address;
};

#define LOWEST_ADDR 0
#define FRAME_LEVEL 0
#define FRAME_OFFSET 0
#define PC_ADJUST -2
#define STOP_FRAME(CURRENT, TOP_STACK) \
  (IS_BAD_PTR((long)(CURRENT)->return_address) \
   || (unsigned int)(CURRENT)->return_address < LOWEST_ADDR \
   || (CURRENT)->return_address == 0|| (CURRENT)->next == 0  \
   || (void *) (CURRENT) < (TOP_STACK))

#define BASE_SKIP 1

/* On i386 architecture we check that at the call point we really have a call
   insn. Possible call instructions are:

   call  addr16        E8 xx xx xx xx
   call  reg           FF Dx
   call  off(reg)      FF xx xx
   lcall addr seg      9A xx xx xx xx xx xx

   This check will not catch all cases but it will increase the backtrace
   reliability on this architecture.
*/

#define VALID_STACK_FRAME(ptr) \
   (!IS_BAD_PTR(ptr) \
    && (((*((ptr) - 3) & 0xff) == 0xe8) \
        || ((*((ptr) - 5) & 0xff) == 0x9a) \
        || ((*((ptr) - 1) & 0xff) == 0xff) \
        || (((*(ptr) & 0xd0ff) == 0xd0ff))))

/*------------------------------- mips-irix -------------------------------*/

#elif defined (__mips) && defined (__sgi)

#define USE_GCC_UNWINDER
#define PC_ADJUST -8

#endif

/*---------------------------------------------------------------------*
 *--      The post GCC 3.3 infrastructure based implementation       --*
 *---------------------------------------------------------------------*/

#if defined (USE_GCC_UNWINDER) && (__GNUC__ * 10 + __GNUC_MINOR__ > 33)

/* Conditioning the inclusion on the GCC version is useful to avoid bootstrap
   path problems, since the included file refers to post 3.3 functions in
   libgcc, and the stage1 compiler is unlikely to be linked against a post 3.3
   library.  It actually disables the support for backtraces in this compiler
   for targets defining USE_GCC_UNWINDER, which is OK since we don't use the
   traceback capablity in the compiler anyway.

   The condition is expressed the way above because we cannot reliably rely on
   any other macro from the base compiler when compiling stage1.  */

#include "tb-gcc.c"

/*------------------------------------------------------------------*
 *-- The generic implementation based on frame layout assumptions --*
 *------------------------------------------------------------------*/

#elif defined (USE_GENERIC_UNWINDER)

#ifndef CURRENT_STACK_FRAME
# define CURRENT_STACK_FRAME  ({ char __csf; &__csf; })
#endif

#ifndef VALID_STACK_FRAME
#define VALID_STACK_FRAME(ptr) 1
#endif

#ifndef MAX
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#endif

/* Define a dummy function to call if FORCE_CALL is defined.  Don't
   define it otherwise, as this could lead to "defined but not used"
   warnings.  */
#if defined (FORCE_CALL)
static void forced_callee () {}
#endif

int
__gnat_backtrace (void **array,
                  int size,
                  void *exclude_min,
                  void *exclude_max,
                  int skip_frames)
{
  struct layout *current;
  void *top_frame;
  void *top_stack;
  int cnt = 0;

  /* Honor FORCE_CALL when defined.  */
#if defined (FORCE_CALL)
  forced_callee ();
#endif

  /* Force a call to builtin_frame_address with a positive argument
     if required. This is necessary e.g. on sparc to have the register
     windows flushed before we attempt to access them on the stack.  */
#if defined (FETCH_UP_FRAME_ADDRESS) && (FRAME_LEVEL == 0)
  __builtin_frame_address (1);
#endif

  top_frame = __builtin_frame_address (FRAME_LEVEL);
  top_stack = CURRENT_STACK_FRAME;
  current = (struct layout *) ((size_t) top_frame + FRAME_OFFSET);

  /* Skip the number of calls we have been requested to skip, accounting for
     the BASE_SKIP parameter.

     FRAME_LEVEL is meaningless for the count adjustment. It impacts where we
     start retrieving data from, but how many frames "up" we start at is in
     BASE_SKIP by definition.  */

  skip_frames = MAX (0, skip_frames - BASE_SKIP);

  while (cnt < skip_frames)
    {
      current = (struct layout *) ((size_t) current->next + FRAME_OFFSET);
      cnt++;
    }

  cnt = 0;
  while (cnt < size)
    {
      if (STOP_FRAME (current, top_stack) ||
	  !VALID_STACK_FRAME((char *)(current->return_address + PC_ADJUST)))
        break;

      if (current->return_address < exclude_min
	  || current->return_address > exclude_max)
        array[cnt++] = current->return_address + PC_ADJUST;

      current = (struct layout *) ((size_t) current->next + FRAME_OFFSET);
    }

  return cnt;
}

#else

/* No target specific implementation and neither USE_GCC_UNWINDER not
   USE_GCC_UNWINDER defined.  */

/*------------------------------*
 *-- The dummy implementation --*
 *------------------------------*/

int
__gnat_backtrace (array, size, exclude_min, exclude_max, skip_frames)
     void **array ATTRIBUTE_UNUSED;
     int size ATTRIBUTE_UNUSED;
     void *exclude_min ATTRIBUTE_UNUSED;
     void *exclude_max ATTRIBUTE_UNUSED;
     int skip_frames ATTRIBUTE_UNUSED;
{
  return 0;
}

#endif

#endif
