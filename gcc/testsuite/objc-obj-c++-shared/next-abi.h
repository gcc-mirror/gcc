/* Check which version of the API and ABI are appropriate for the target.
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.

   Contributed by Iain Sandoe <iains@gcc.gnu.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _OBJC_NEXT_ABI_H_
#define _OBJC_NEXT_ABI_H_
/* Produce a define that allows us to figure out what facilities are
   available for this gcc and OS combination.
*/

/* By default we do nothing - therefore ifdef NEXT_OBJC_USE_NEW_INTERFACE
 * is reliable for detecting versions of the target that require either
 * API=2, or both API & ABI = 2 (m64 code).
 *
 * This applies for versions of OSX >= 10.5 (darwin9).
 *
 * A compiler capable of producing ObjC V2 ABI should define __OBJC2__
*/

#undef NEXT_OBJC_ABI_VERSION
#undef NEXT_OBJC_USE_NEW_INTERFACE

#ifdef __NEXT_RUNTIME__
#  if (__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050 || __OBJC2__)
    /* We have to use an updated interface for 32bit NeXT to avoid
     * 'deprecated' warnings. 
     * For 64bit NeXT the ABI is different (and the interfaces 'deprecated'
     * for 32bit have been removed).
    */
#    define NEXT_OBJC_USE_NEW_INTERFACE 1
#    if __OBJC2__ || __LP64__
       /* We have OBJC v2 ABI compiler, 
          (or, at least, the available NeXT runtime requires one) */
#      define NEXT_OBJC_ABI_VERSION 2
#    else
       /* We leave it open to define ABI 1 if and when we implement those 
        * extensions.
       */
#      define NEXT_OBJC_ABI_VERSION 0
#    endif
#  else
      /* Pre-OSX 10.5 all is ABI 0.  */
#     define NEXT_OBJC_ABI_VERSION 0
#  endif /* MAC_OS_X_VERSION_MIN_REQUIRED > 10.5 or OBJC2 */
#endif /* __NEXT_RUNTIME__ */

#endif /* _OBJC_NEXT_ABI_H_ */
