/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                            T R A C E B A C K                             *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2000-2025, Free Software Foundation, Inc.       *
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

/* This file contains low level support for stack unwinding using GCC intrinsic
   functions.
   It has been tested on the following configurations:
   PowerPC/AiX
   PowerPC/Darwin
   PowerPC/VxWorks
   PowerPC/LynxOS-178
   SPARC/Solaris
   i386/GNU/Linux
   i386/Solaris
   i386/NT
   i386/OS2
   i386/LynxOS
   Alpha/VxWorks
   Alpha/VMS
*/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef IN_RTS
#define POSIX
#include "runtime.h"
#include <stddef.h>
#else
#include "config.h"
#include "system.h"
/* We don't want fancy_abort here.  */
#undef abort
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

#if defined (_WIN64) && defined (__SEH__)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define IS_BAD_PTR(ptr) (IsBadCodePtr((FARPROC)ptr))

int
__gnat_backtrace (void **array,
                  int size,
                  void *exclude_min,
                  void *exclude_max,
                  int skip_frames)
{
  CONTEXT context;
  UNWIND_HISTORY_TABLE history;
  int i;

  /* Get the context.  */
  RtlCaptureContext (&context);

  /* Setup unwind history table (a cached to speed-up unwinding).  */
  memset (&history, 0, sizeof (history));

  i = 0;
  while (1)
    {
      PRUNTIME_FUNCTION RuntimeFunction;
      KNONVOLATILE_CONTEXT_POINTERS NvContext;
      ULONG64 ImageBase;
      VOID *HandlerData;
      ULONG64 EstablisherFrame;

      /* Get function metadata.  */
      RuntimeFunction = RtlLookupFunctionEntry
	(context.Rip, &ImageBase, &history);

      if (!RuntimeFunction)
	{
	  /* In case of failure, assume this is a leaf function.  */
	  context.Rip = *(ULONG64 *) context.Rsp;
	  context.Rsp += 8;
	}
      else
	{
	  /* If the last unwinding step failed somehow, stop here.  */
	  if (IS_BAD_PTR(context.Rip))
	    break;

	  /* Unwind.  */
	  memset (&NvContext, 0, sizeof (KNONVOLATILE_CONTEXT_POINTERS));
	  RtlVirtualUnwind (0, ImageBase, context.Rip, RuntimeFunction,
			    &context, &HandlerData, &EstablisherFrame,
			    &NvContext);
	}

      /* 0 means bottom of the stack.  */
      if (context.Rip == 0)
	break;

      /* Skip frames.  */
      if (skip_frames > 1)
	{
	  skip_frames--;
	  continue;
	}
      /* Excluded frames.  */
      if ((void *)context.Rip >= exclude_min
	  && (void *)context.Rip <= exclude_max)
	continue;

      array[i++] = (void *)(context.Rip - 2);
      if (i >= size)
	break;
    }
  return i;
}
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

   To initiate the process, we retrieve an initial frame address using the
   appropriate GCC builtin (__builtin_frame_address).

   This scheme is unfortunately not applicable on every target because the
   stack layout is not necessarily regular (static) enough. On targets where
   this scheme applies, the implementation relies on the following items:

   o struct layout, describing the expected stack data layout relevant to the
     information we are interested in,

   o FRAME_OFFSET, the offset, from a given frame address or frame pointer
     value, at which this layout will be found,

   o FRAME_LEVEL, controls how many frames up we get at to start with,
     from the initial frame pointer we compute by way of the GCC builtin,

     0 is most often the appropriate value. 1 may be necessary on targets
     where return addresses are saved by a function in it's caller's frame
     (e.g. PPC).

   o PC_ADJUST, to account for the difference between a call point (address
     of a call instruction), which is what we want in the output array, and
     the associated return address, which is what we retrieve from the stack.

   o STOP_FRAME, to decide whether we reached the top of the call chain, and
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
     is 0. See the SPARC Solaris case for an example where this is useful.

  */

/*------------------- Darwin 8 (OSX 10.4) or newer ----------------------*/
#if defined (__APPLE__) \
    && defined (__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) \
    && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1040

#define USE_GCC_UNWINDER

#if defined (__i386__) || defined (__x86_64__)
#define PC_ADJUST -2
#elif defined (__ppc__) || defined (__ppc64__)
#define PC_ADJUST -4
#elif defined (__arm__)
#define PC_ADJUST -2
#elif defined (__arm64__)
#define PC_ADJUST -4
#else
#error Unhandled darwin architecture.
#endif

/*---------------------------- x86 *BSD --------------------------------*/

#elif defined (__i386__) &&   \
    ( defined (__NetBSD__) || defined (__FreeBSD__) || defined (__OpenBSD__) )

#define USE_GCC_UNWINDER
/* The generic unwinder is not used for this target because the default
   implementation doesn't unwind on the BSD platforms.  AMD64 targets use the
   gcc unwinder for all platforms, so let's keep i386 consistent with that.
*/

#define PC_ADJUST -2
/* The minimum size of call instructions on this architecture is 2 bytes */

/*---------------------- ARM VxWorks ------------------------------------*/
#elif (defined (ARMEL) && defined (__vxworks))

#include "vxWorks.h"
#include "version.h"

#define USE_GCC_UNWINDER
#define PC_ADJUST -2

#if ((_WRS_VXWORKS_MAJOR >= 7) && (_VX_CPU != ARMARCH8A))
#define USING_ARM_UNWINDING 1
#endif

/*---------------------- ARM Linux ------------------------------------ -*/
#elif (defined (__ARMEL__) && defined (__linux))

#define USE_GCC_UNWINDER
#define PC_ADJUST -2
#define USING_ARM_UNWINDING 1

/*---------------------- ARM RTEMS ------------------------------------ -*/
#elif (defined (__arm__) && defined (__rtems__))

#define USE_GCC_UNWINDER
#define PC_ADJUST -2
#define USING_ARM_UNWINDING 1

/*---------------------- PPC AIX/PPC Lynx 178/Older Darwin --------------*/
#elif ((defined (_POWER) && defined (_AIX)) || \
       (defined (__powerpc__) && defined (__Lynx__) && !defined(__ELF__)) || \
       (defined (__ppc__) && defined (__APPLE__)))

#define USE_GENERIC_UNWINDER

struct layout
{
  struct layout *next;
  void *pad;
  void *return_address;
};

#define FRAME_OFFSET(FP) 0
#define PC_ADJUST -4

/* Eventhough the base PPC ABI states that a toplevel frame entry
   should to feature a null backchain, AIX might expose a null return
   address instead.  */

/* Then LynxOS-178 features yet another variation, with return_address
   == &<entrypoint>, with two possible entry points (one for the main
   process and one for threads). Beware that &bla returns the address
   of a descriptor when "bla" is a function.  Getting the code address
   requires an extra dereference.  */

#if defined (__Lynx__)
extern void __start();  /* process entry point.  */
extern void __runnit(); /* thread entry point.  */
#define EXTRA_STOP_CONDITION(CURRENT)                 \
  ((CURRENT)->return_address == *(void**)&__start     \
   || (CURRENT)->return_address == *(void**)&__runnit)
#else
#define EXTRA_STOP_CONDITION(CURRENT) (0)
#endif

#define STOP_FRAME(CURRENT, TOP_STACK) \
  (((void *) (CURRENT) < (TOP_STACK)) \
   || (CURRENT)->return_address == NULL \
   || EXTRA_STOP_CONDITION(CURRENT))

/* The PPC ABI has an interesting specificity: the return address saved by a
   function is located in it's caller's frame, and the save operation only
   takes place if the function performs a call.

   To have __gnat_backtrace retrieve its own return address, we then
   define ... */

#define FORCE_CALL 1
#define FRAME_LEVEL 1

#define BASE_SKIP 1

/*----------- PPC ELF (GNU/Linux & VxWorks & Lynx178e & RTEMS ) ----------*/

#elif (defined (_ARCH_PPC) && defined (__vxworks)) ||  \
  (defined (__powerpc__) && defined (__Lynx__) && defined(__ELF__)) || \
  (defined (__linux__) && defined (__powerpc__)) || \
  (defined (__powerpc__) && defined (__rtems__))

#if defined (_ARCH_PPC64) && !defined (__USING_SJLJ_EXCEPTIONS__)
#define USE_GCC_UNWINDER
#else
#define USE_GENERIC_UNWINDER
#endif

struct layout
{
  struct layout *next;
  void *return_address;
};

#define FORCE_CALL 1
#define FRAME_LEVEL 1
/* See the PPC AIX case for an explanation of these values.  */

#define FRAME_OFFSET(FP) 0
#define PC_ADJUST -4

/* According to the base PPC ABI, a toplevel frame entry should feature
   a null backchain.  What happens at signal handler frontiers isn't so
   well specified, so we add a safety guard on top.  */

#define STOP_FRAME(CURRENT, TOP_STACK) \
 ((CURRENT)->next == 0 || ((long)(CURRENT)->next % __alignof__(void*)) != 0)

#define BASE_SKIP 1

/*-------------------------- SPARC Solaris or RTEMS --------------------*/

#elif (defined (__sun__) || defined (__rtems__)) && defined (__sparc__)

#define USE_GENERIC_UNWINDER

/* These definitions are inspired from the Appendix D (Software
   Considerations) of the SPARC V8 architecture manual.  */

struct layout
{
  struct layout *next;
  void *return_address;
};

#ifdef __arch64__
#define STACK_BIAS 2047 /* V9 ABI */
#else
#define STACK_BIAS 0    /* V8 ABI */
#endif

#define FRAME_LEVEL 0
#define FRAME_OFFSET(FP) (14 * sizeof (void*) + (FP ? STACK_BIAS : 0))
#define PC_ADJUST 0
#define STOP_FRAME(CURRENT, TOP_STACK) \
  ((CURRENT)->return_address == 0|| (CURRENT)->next == 0 \
   || (void *) (CURRENT) < (TOP_STACK))

/* The SPARC register windows need to be flushed before we may access them
   from the stack. This is achieved by way of builtin_frame_address only
   when the "count" argument is positive, so force at least one such call.  */
#define FETCH_UP_FRAME_ADDRESS

#define BASE_SKIP 2
/* From the frame pointer of frame N, we are accessing the flushed register
   window of frame N-1 (positive offset from fp), in which we retrieve the
   saved return address. We then end up with our caller's return address.  */

/*---------------------------- x86 & x86_64 ---------------------------------*/

#elif defined (__i386__) || defined (__x86_64__)

#if defined (__WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define IS_BAD_PTR(ptr) (IsBadCodePtr((FARPROC)ptr))
#elif defined (__sun__)
#define IS_BAD_PTR(ptr) ((unsigned long)ptr == -1UL)
#else
#define IS_BAD_PTR(ptr) 0
#endif

/* Use the dwarf2 unwinder when we expect to have dwarf2 tables at
   hand. Backtraces will reliably stop on frames missing such tables,
   but our only alternative is the generic unwinder which requires
   compilation forcing a frame pointer to be reliable.  */

#if (defined (__x86_64__) || defined (__linux__)) && !defined (__USING_SJLJ_EXCEPTIONS__)
#define USE_GCC_UNWINDER
#else
#define USE_GENERIC_UNWINDER
#endif

struct layout
{
  struct layout *next;
  void *return_address;
};

#define FRAME_LEVEL 1
/* builtin_frame_address (1) is expected to work on this family of targets,
   and (0) might return the soft stack pointer, which does not designate a
   location where a backchain and a return address might be found.  */

#define FRAME_OFFSET(FP) 0
#define PC_ADJUST -2
#define STOP_FRAME(CURRENT, TOP_STACK) \
  (IS_BAD_PTR((long)(CURRENT)) \
   || (void *) (CURRENT) < (TOP_STACK) \
   || IS_BAD_PTR((long)(CURRENT)->return_address) \
   || (CURRENT)->return_address == 0 \
   || (void *) ((CURRENT)->next) < (TOP_STACK)  \
   || EXTRA_STOP_CONDITION(CURRENT))

#define BASE_SKIP (1+FRAME_LEVEL)

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

#if defined (__vxworks) && defined (__RTP__)

/* For VxWorks following backchains past the "main" frame gets us into the
   kernel space, where it can't be dereferenced. So lets stop at the main
   symbol.  */
extern void main();

static int
is_return_from(void *symbol_addr, void *ret_addr)
{
  int ret = 0;
  char *ptr = (char *)ret_addr;

  if ((*(ptr - 5) & 0xff) == 0xe8)
    {
      /* call addr16  E8 xx xx xx xx  */
      int32_t offset = *(int32_t *)(ptr - 4);
      ret = (ptr + offset) == symbol_addr;
    }

  /* Others not implemented yet...  But it is very likely that call addr16
     is used here.  */
  return ret;
}

#define EXTRA_STOP_CONDITION(CURRENT) \
  (is_return_from(&main, (CURRENT)->return_address))
#else /* not (defined (__vxworks) && defined (__RTP__)) */
#define EXTRA_STOP_CONDITION(CURRENT) (0)
#endif /* not (defined (__vxworks) && defined (__RTP__)) */

/*----------------------------- qnx ----------------------------------*/

#elif defined (__QNX__)

#define USE_GCC_UNWINDER

#if defined (__aarch64__)
#define PC_ADJUST -4
#elif defined (__ARMEL__)
#define PC_ADJUST -2
#define USING_ARM_UNWINDING 1
#else
#error Unhandled QNX architecture.
#endif

/*------------------- aarch64 FreeBSD, Linux, RTEMS -----------------*/

#elif (defined (__aarch64__) && (defined (__FreeBSD__) || \
       defined (__linux__) || defined (__rtems__)))

#define USE_GCC_UNWINDER
#define PC_ADJUST -4

/*----------------------------- ia64 ---------------------------------*/

#elif defined (__ia64__) && (defined (__linux__) || defined (__hpux__))

#define USE_GCC_UNWINDER
/* Use _Unwind_Backtrace driven exceptions on ia64 HP-UX and ia64
   GNU/Linux, where _Unwind_Backtrace is provided by the system unwind
   library. On HP-UX 11.23 this requires patch PHSS_33352, which adds
   _Unwind_Backtrace to the system unwind library. */

#define PC_ADJUST -4


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
   traceback capability in the compiler anyway.

   The condition is expressed the way above because we cannot reliably rely on
   any other macro from the base compiler when compiling stage1.  */

#ifdef USING_ARM_UNWINDING
/* This value is not part of the enumerated reason codes defined in unwind.h
   for ARM style unwinding, but is used in the included "C" code, so we
   define it to a reasonable value to avoid a compilation error.  */
#define _URC_NORMAL_STOP 0
#endif

/* This is an implementation of the __gnat_backtrace routine using the
   underlying GCC unwinding support associated with the exception handling
   infrastructure.  This will only work for ZCX based applications.  */

#include <unwind.h>

/* The implementation boils down to a call to _Unwind_Backtrace with a
   tailored callback and carried-on data structure to keep track of the
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
  char * pc;

#if defined (__ia64__) && defined (__hpux__) && defined (USE_LIBUNWIND_EXCEPTIONS)
  /* Work around problem with _Unwind_GetIP on ia64 HP-UX. */
  uwx_get_reg ((struct uwx_env *) uw_context, UWX_REG_IP, (uint64_t *) &pc);
#else
  pc = (char *) _Unwind_GetIP (uw_context);
#endif

  if (uw_data->n_frames_skipped < uw_data->n_frames_to_skip)
    {
      uw_data->n_frames_skipped ++;
      return _URC_NO_REASON;
    }

  if (uw_data->n_entries_filled >= uw_data->max_len)
    return _URC_NORMAL_STOP;

  if (pc < (char *)uw_data->exclude_min || pc > (char *)uw_data->exclude_max)
    uw_data->traceback [uw_data->n_entries_filled ++] = pc + PC_ADJUST;

  return _URC_NO_REASON;
}

#endif

/********************
 * __gnat_backtrace *
 ********************/

int
__gnat_backtrace (void ** traceback __attribute__((unused)),
		  int max_len __attribute__((unused)),
		  void * exclude_min __attribute__((unused)),
		  void * exclude_max __attribute__((unused)),
		  int skip_frames __attribute__((unused)))
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

/*------------------------------------------------------------------*
 *-- The generic implementation based on frame layout assumptions --*
 *------------------------------------------------------------------*/

#elif defined (USE_GENERIC_UNWINDER)

/* No warning since the cases where FRAME_LEVEL > 0 are known to work.  */
#pragma GCC diagnostic ignored "-Wframe-address"

#ifndef CURRENT_STACK_FRAME
# define CURRENT_STACK_FRAME  ({ char __csf; &__csf; })
#endif

#ifndef VALID_STACK_FRAME
#define VALID_STACK_FRAME(ptr) 1
#endif

#ifndef MAX
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#endif

#ifndef FORCE_CALL
#define FORCE_CALL 0
#endif

/* Make sure the function is not inlined.  */
static void forced_callee (void) __attribute__ ((noinline));

static void forced_callee (void)
{
  /* Make sure the function is not pure.  */
  volatile int i __attribute__ ((unused)) = 0;
}

int
__gnat_backtrace (void **array,
                  int size,
                  void *exclude_min,
                  void *exclude_max,
                  int skip_frames)
{
  struct layout *current;
  void *top_frame;
  void *top_stack ATTRIBUTE_UNUSED;
  int cnt = 0;

  if (FORCE_CALL)
    forced_callee ();

  /* Force a call to builtin_frame_address with a positive argument
     if required. This is necessary e.g. on SPARC to have the register
     windows flushed before we attempt to access them on the stack.  */
#if defined (FETCH_UP_FRAME_ADDRESS) && (FRAME_LEVEL == 0)
  __builtin_frame_address (1);
#endif

  top_frame = __builtin_frame_address (FRAME_LEVEL);
  top_stack = CURRENT_STACK_FRAME;
  current = (struct layout *) ((size_t) top_frame + FRAME_OFFSET (0));

  /* Skip the number of calls we have been requested to skip, accounting for
     the BASE_SKIP parameter.

     FRAME_LEVEL is meaningless for the count adjustment. It impacts where we
     start retrieving data from, but how many frames "up" we start at is in
     BASE_SKIP by definition.  */

  skip_frames = MAX (0, skip_frames - BASE_SKIP);

  while (cnt < skip_frames)
    {
      current = (struct layout *) ((size_t) current->next + FRAME_OFFSET (1));
      cnt++;
    }

  cnt = 0;
  while (cnt < size)
    {
      if (STOP_FRAME (current, top_stack) ||
	  !VALID_STACK_FRAME(((char *) current->return_address) + PC_ADJUST))
        break;

      if (current->return_address < exclude_min
	  || current->return_address > exclude_max)
        array[cnt++] = ((char *) current->return_address) + PC_ADJUST;

      current = (struct layout *) ((size_t) current->next + FRAME_OFFSET (1));
    }

  return cnt;
}

#else

/* No target specific implementation and neither USE_GCC_UNWINDER nor
   USE_GENERIC_UNWINDER defined.  */

/*------------------------------*
 *-- The dummy implementation --*
 *------------------------------*/

int
__gnat_backtrace (void **array ATTRIBUTE_UNUSED,
                  int size ATTRIBUTE_UNUSED,
                  void *exclude_min ATTRIBUTE_UNUSED,
                  void *exclude_max ATTRIBUTE_UNUSED,
                  int skip_frames ATTRIBUTE_UNUSED)
{
  return 0;
}

#endif

#endif

#ifdef __cplusplus
}
#endif
