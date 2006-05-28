/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                           I N I T I A L I Z E                            *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2005, Free Software Foundation, Inc.         *
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

/*  This unit provides default implementation for __gnat_initialize ()
    which is called before the elaboration of the partition. It is provided
    in a separate file/object so that users can replace it easily.
    The default implementation should be null on most targets. */

/* The following include is here to meet the published VxWorks requirement
   that the __vxworks header appear before any other include. */
#ifdef __vxworks
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

#include "raise.h"

/******************************************/
/* __gnat_initialize (NT-mingw32 Version) */
/******************************************/

#if defined (__MINGW32__)
#include <windows.h>

extern void __gnat_init_float (void);
extern void __gnat_plist_init (void);
extern void __gnat_install_SEH_handler (void *);

void
__gnat_initialize (void *eh)
{
   /* Initialize floating-point coprocessor. This call is needed because
      the MS libraries default to 64-bit precision instead of 80-bit
      precision, and we require the full precision for proper operation,
      given that we have set Max_Digits etc with this in mind */
   __gnat_init_float ();

   /* Initialize a lock for a process handle list - see adaint.c for the
      implementation of __gnat_portable_no_block_spawn, __gnat_portable_wait */
   __gnat_plist_init();

   /* Note that we do not activate this for the compiler itself to avoid a
      bootstrap path problem.  Older version of gnatbind will generate a call
      to __gnat_initialize() without argument. Therefore we cannot use eh in
      this case.  It will be possible to remove the following #ifdef at some
      point.  */
#ifdef IN_RTS
   /* Install the Structured Exception handler.  */
   if (eh)
     __gnat_install_SEH_handler (eh);
#endif
}

/******************************************/
/* __gnat_initialize (init_float version) */
/******************************************/

#elif defined (__INTERIX) || defined (__Lynx__) || \
      defined (__FreeBSD__) || defined(__NetBSD__)

extern void __gnat_init_float (void);

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
   __gnat_init_float ();
}

/***************************************/
/* __gnat_initialize (VxWorks Version) */
/***************************************/

#elif defined(__vxworks)

extern void __gnat_init_float (void);

void
__gnat_initialize (void *eh)
{
  __gnat_init_float ();

  /* On targets where we might be using the ZCX scheme, we need to register
     the frame tables.

     For applications loaded as a set of "modules", the crtstuff objects
     linked in (crtbegin/end) are tailored to provide this service a-la C++
     constructor fashion, typically triggered by the VxWorks loader.  This is
     achieved by way of a special variable declaration in the crt object, the
     name of which has been deduced by analyzing the output of the "munching"
     step documented for C++.  The de-registration is handled symmetrically,
     a-la C++ destructor fashion and typically triggered by the dynamic
     unloader.  Note that since the tables shall be registered against a
     common datastructure, libgcc should be one of the modules (vs being
     partially linked against all the others at build time) and shall be
     loaded first.

     For applications linked with the kernel, the scheme above would lead to
     duplicated symbols because the VxWorks kernel build "munches" by default.
     To prevent those conflicts, we link against crtbegin/endS objects that
     don't include the special variable and directly call the appropriate
     function here. We'll never unload that, so there is no de-registration to
     worry about.

     For whole applications loaded as a single module, we may use one scheme
     or the other, except for the mixed Ada/C++ case in which the first scheme
     would fail for the same reason as in the linked-with-kernel situation.

     We can differentiate by looking at the __module_has_ctors value provided
     by each class of crt objects. As of today, selecting the crt set with the
     ctors/dtors capabilities (first scheme above) is triggered by adding
     "-dynamic" to the gcc *link* command line options. Selecting the other
     set of crt objects is achieved by "-static" instead.

     This is a first approach, tightly synchronized with a number of GCC
     configuration and crtstuff changes. We need to ensure that those changes
     are there to activate this circuitry.  */

#if (__GNUC__ >= 3) && (defined (_ARCH_PPC) || defined (__ppc))
 {
   /* The scheme described above is only useful for the actual ZCX case, and
      we don't want any reference to the crt provided symbols otherwise.  We
      may not link with any of the crt objects in the non-ZCX case, e.g. from
      documented procedures instructing the use of -nostdlib, and references
      to the ctors symbols here would just remain unsatisfied.

      We have no way to avoid those references in the right conditions in this
      C module, because we have nothing like a IN_ZCX_RTS macro.  This aspect
      is then deferred to an Ada routine, which can do that based on a test
      against a constant System flag value.  */

   extern void __gnat_vxw_setup_for_eh (void);
   __gnat_vxw_setup_for_eh ();
 }
#endif
}

#elif defined(_T_HPUX10) || (!defined(IN_RTS) && defined(_X_HPUX10))

/************************************************/
/* __gnat_initialize (PA-RISC HP-UX 10 Version) */
/************************************************/

extern void __main (void);

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
  __main ();
}

#else

/* For all other versions of GNAT, the initialize routine and handler
   installation do nothing */

/***************************************/
/* __gnat_initialize (Default Version) */
/***************************************/

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
}

#endif
