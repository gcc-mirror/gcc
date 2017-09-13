/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                           I N I T I A L I Z E                            *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2017, Free Software Foundation, Inc.         *
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

/*  This unit provides default implementation for __gnat_initialize ()
    which is called before the elaboration of the partition. It is provided
    in a separate file/object so that users can replace it easily.
    The default implementation should be null on most targets.  */

/* The following include is here to meet the published VxWorks requirement
   that the __vxworks header appear before any other include.  */
#ifdef __vxworks
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)
#define xrealloc(V,S) realloc (V,S)
#else
#include "config.h"
#include "system.h"
#endif

#include "raise.h"
#include <fcntl.h>

#ifdef __cplusplus
extern "C" {
#endif

/******************************************/
/* __gnat_initialize (NT-mingw32 Version) */
/******************************************/

#if defined (__MINGW32__)

extern void __gnat_install_SEH_handler (void *);

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
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

#elif defined (__Lynx__) || defined (__FreeBSD__) || defined(__NetBSD__) \
  || defined (__OpenBSD__) || defined (__DragonFly__)

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
}

/***************************************/
/* __gnat_initialize (VxWorks Version) */
/***************************************/

#elif defined(__vxworks)

void
__gnat_initialize (void *eh)
{
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

#ifdef __cplusplus
}
#endif
