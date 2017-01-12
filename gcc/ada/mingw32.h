/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               M I N G W 3 2                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 2002-2016, Free Software Foundation, Inc.         *
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

/*  This file provides some macros used for the MINGW32 platform. The main
    goal is to be able to build GNAT with a standard MINGW32 C header
    set. This files contains also the circuitry for the unicode support.   */

#ifndef _MINGW32_H
#define _MINGW32_H

#include <_mingw.h>

#ifndef RTX
#define GNAT_UNICODE_SUPPORT
#define _UNICODE /* For C runtime */
#define UNICODE  /* For Win32 API */
#endif

/* We need functionality available only starting with Windows XP */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#ifndef __CYGWIN__
#include <tchar.h>
#endif
#if defined (__CYGWIN__) && !defined (__CYGWIN32__) && !defined (IN_RTS)
/* Note: windows.h on cygwin-64 includes x86intrin.h which uses malloc.
   That fails to compile, if malloc is poisoned, i.e. if !IN_RTS.  */
#define _X86INTRIN_H_INCLUDED
#endif
#include <windows.h>

#ifndef _O_U8TEXT
#define _O_U8TEXT _O_TEXT
#endif
#ifndef _O_U16TEXT
#define _O_U16TEXT _O_TEXT
#endif
#ifndef _O_WTEXT
#define _O_WTEXT _O_TEXT
#endif

/* After including this file it is possible to use the character t as prefix
   to routines. If GNAT_UNICODE_SUPPORT is defined then the unicode enabled
   versions will be used.  */

/* Copy to/from wide-string, if GNAT_UNICODE_SUPPORT activated this will do
   the proper translations using the UTF-8 encoding.  */

#ifdef GNAT_UNICODE_SUPPORT

extern UINT __gnat_current_codepage;
extern UINT __gnat_current_ccs_encoding;

/*  Macros to convert to/from the code page specified in
    __gnat_current_codepage.  */
#define S2WSC(wstr,str,len) \
   MultiByteToWideChar (__gnat_current_codepage,0,str,-1,wstr,len)
#define WS2SC(str,wstr,len) \
   WideCharToMultiByte (__gnat_current_codepage,0,wstr,-1,str,len,NULL,NULL)

/*  Macros to convert to/from UTF-8 code page.  */
#define S2WSU(wstr,str,len) \
   MultiByteToWideChar (CP_UTF8,0,str,-1,wstr,len)
#define WS2SU(str,wstr,len) \
   WideCharToMultiByte (CP_UTF8,0,wstr,-1,str,len,NULL,NULL)

/*  Macros to convert to/from Windows default code page.  */
#define S2WS(wstr,str,len) \
   MultiByteToWideChar (CP_ACP,0,str,-1,wstr,len)
#define WS2S(str,wstr,len) \
   WideCharToMultiByte (CP_ACP,0,wstr,-1,str,len,NULL,NULL)
#else
#define S2WSC(wstr,str,len) strncpy(wstr,str,len)
#define WS2SC(str,wstr,len) strncpy(str,wstr,len)
#define S2WSU(wstr,str,len) strncpy(wstr,str,len)
#define WS2SU(str,wstr,len) strncpy(str,wstr,len)
#define S2WS(wstr,str,len) strncpy(wstr,str,len)
#define WS2S(str,wstr,len) strncpy(str,wstr,len)
#endif

#include <stdlib.h>

/* STD_MINGW: standard if MINGW32 version > 1.3, we have switched to this
   version instead of the previous enhanced version to ease building GNAT on
   Windows platforms. By using STD_MINGW or OLD_MINGW it is possible to build
   GNAT using both MingW include files (Old MingW + ACT changes and standard
   MingW starting with version 1.3.
   For w64 Mingw the define STD_MINGW is always set to value 1, because
   there is no old header set present.  */
#ifdef _WIN64
#define STD_MINGW 1
#else
#define STD_MINGW ((__MINGW32_MAJOR_VERSION == 1 \
		   && __MINGW32_MINOR_VERSION >= 3) \
     || (__MINGW32_MAJOR_VERSION >= 2))
#endif

#define OLD_MINGW (!(STD_MINGW))

#ifndef MAXPATHLEN
#define MAXPATHLEN MAX_PATH
#endif

#endif /* _MINGW32_H */
