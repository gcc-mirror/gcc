/* Kernel-side additional module for the VxWorks threading support
   logic for GCC.  Written 2002 by Zack Weinberg.

   This file is distributed with GCC, but it is not part of GCC.
   The contents of this file are in the public domain.  */

/* If you are using the Tornado IDE, copy this file to
   $WIND_BASE/target/config/comps/src/gthread_supp.c.  Then create a
   file named 10comp_gthread_supp.cdf in target/config/comps/vxWorks
   with the following contents:

   Component INCLUDE_GCC_GTHREAD {
      NAME                GCC 3.x gthread support (required by C++)
      CONFIGLETTES        gthread_supp.c
      REQUIRES            INCLUDE_CPLUS
      INCLUDE_WHEN        INCLUDE_CPLUS
      _FOLDER             FOLDER_CPLUS
   }

   If you are using command line builds, instead copy this file to
   $WIND_BASE/target/src/config/gthread_supp.c, and add the following
   block to target/src/config/usrExtra.c:

   #ifdef INCLUDE_CPLUS
   #include "../../src/config/gthread_supp.c"
   #endif

   You should now be able to rebuild your application using GCC 3.x.  */

#include <vxWorks.h>
#include <taskLib.h>

/* This file provides these routines:  */
extern void *__gthread_get_tsd_data (WIND_TCB *tcb);
extern void __gthread_set_tsd_data (WIND_TCB *tcb, void *data);

extern void __gthread_enter_tsd_dtor_context (WIND_TCB *tcb);
extern void __gthread_leave_tsd_dtor_context (WIND_TCB *tcb);

/* Set and retrieve the TSD data block for the task TCB.

   Possible choices for TSD_SLOT are:
     reserved1
     reserved2
     spare1
     spare2
     spare3
     spare4
   (these are all fields of the TCB structure; all have type 'int').

   If you find that the slot chosen by default is already used for
   something else, simply change the #define below and recompile this
   file.  No other file should reference TSD_SLOT directly.  */

/* WARNING: This code is not 64-bit clean (it assumes that a pointer
   can be held in an 'int' without truncation).  As much of the rest
   of VxWorks also makes this assumption, we can't really avoid it.  */

#define TSD_SLOT reserved1

void *
__gthread_get_tsd_data (WIND_TCB *tcb)
{
  return (void *) (tcb->TSD_SLOT);
}

void
__gthread_set_tsd_data (WIND_TCB *tcb, void *data)
{
  tcb->TSD_SLOT = (int) data;
}

/* Enter and leave "TSD destructor context".  This is defined as a
   state in which it is safe to call free() from a task delete hook
   on a memory block allocated by the task being deleted.
   For VxWorks 5.x, nothing needs to be done.  */

#if __GNUC__ >= 2
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

void
__gthread_enter_tsd_dtor_context (WIND_TCB *tcb UNUSED)
{
}

void
__gthread_leave_tsd_dtor_context (WIND_TCB *tcb UNUSED)
{
}
