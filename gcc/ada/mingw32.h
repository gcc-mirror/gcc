/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               M I N G W 3 2                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 2002-2006, Free Software Foundation, Inc.         *
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

/*  This file provides some macros used for the MINGW32 platform. The main
    goal is to be able to build GNAT with a standard MINGW32 C header set */

#ifndef _MINGW32_H
#define _MINGW32_H



/* Uncomment to activate the GNAT Unicode support. */
/*#define GNAT_UNICODE_SUPPORT */

#ifdef GNAT_UNICODE_SUPPORT
#define _UNICODE /* For C runtime */
#define UNICODE  /* For Win32 API */
#endif

#include <tchar.h>

/* After including this file it is possible to use the character t as prefix
   to routines. If GNAT_UNICODE_SUPPORT is defined then the unicode enabled
   versions will be used. */

/* Copy to/from wide-string, if GNAT_UNICODE_SUPPORT activated this will do
   the proper translations using the UTF-8 encoding.  */

#ifdef GNAT_UNICODE_SUPPORT
#define S2WS(wstr,str,len) \
   MultiByteToWideChar (CP_UTF8,0,str,-1,wstr,len);
#define WS2S(str,wstr,len) \
   WideCharToMultiByte (CP_UTF8,0,wstr,-1,str,len,NULL,NULL);
#else
#define S2WS(wstr,str,len) strncpy(wstr,str,len);
#define WS2S(str,wstr,len) strncpy(str,wstr,len);
#endif

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

#ifndef MAXPATHLEN
#define MAXPATHLEN MAX_PATH
#endif

#endif /* _MINGW32_H */
