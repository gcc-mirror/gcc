/* Configuration for an i386 running Mach as the target machine.  */

/* We do want to add an underscore to the front of each user symbol.
   i386gas.h checks this.  */
#define YES_UNDERSCORES

#include "i386gstabs.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -DMACH"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"

/* Make stddef.h agree with types.h.  */
#define SIZE_TYPE "long int"

/* Defines to be able to build libgcc.a with GCC.  */

/* It might seem that these are not important, since gcc 2 will never
   call libgcc for these functions.  But programs might be linked with
   code compiled by gcc 1, and then these will be used.  */

#define perform_udivsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = a;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b), "d" (dx));	\
  return ax;								\
}

#define perform_divsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  ax = a;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b));	\
  return ax;								\
}

#define perform_umodsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = a;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b), "d" (dx));	\
  return dx;								\
}

#define perform_modsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  ax = a;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b));	\
  return dx;								\
}

#define perform_fixdfsi(a)						\
{									\
  auto unsigned short ostatus;						\
  auto unsigned short nstatus;						\
  auto int ret;								\
  auto double tmp;							\
									\
  &ostatus;			/* guarantee these land in memory */	\
  &nstatus;								\
  &ret;									\
  &tmp;									\
									\
  asm volatile ("fnstcw %0" : "=m" (ostatus));				\
  nstatus = ostatus | 0x0c00;						\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (nstatus));		\
  tmp = a;								\
  asm volatile ("fldl %0" : /* no outputs */ : "m" (tmp));		\
  asm volatile ("fistpl %0" : "=m" (ret));				\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (ostatus));		\
									\
  return ret;								\
}
