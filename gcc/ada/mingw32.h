/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               M I N G W 3 2                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 2002, Free Software Foundation, Inc.            *
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

/*  This file provides some macros used for the MINGW32 platform. The main
    goal is to be able to build GNAT with a standard MINGW32 C header set */

#ifndef _MINGW32_H
#define _MINGW32_H

#include <stdlib.h>

/* STD_MINGW: standard if MINGW32 version > 1.3, we have switched to this
   version instead of the previous enhanced version to ease building GNAT on
   Windows platforms. By using STD_MINGW or OLD_MINGW it is possible to build
   GNAT using both MingW include files (Old MingW + ACT changes and standard
   MingW starting with version 1.3. */
#define STD_MINGW ((__MINGW32_MAJOR_VERSION == 1 \
		   && __MINGW32_MINOR_VERSION >= 3) \
     || (__MINGW32_MAJOR_VERSION >= 2))

#define OLD_MINGW (!(STD_MINGW))

#define MAXPATHLEN MAX_PATH

#endif /* _MINGW32_H */
