/* mbchar.h - Various declarations for functions found in mbchar.c
   Copyright (C) 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef __GCC_MBCHAR_H__
#define __GCC_MBCHAR_H__

#ifdef MULTIBYTE_CHARS
/* escape character used for JIS encoding */
#define JIS_ESC_CHAR 0x1b

#define ISSJIS1(c)   ((c) >= 0x81 && (c) <= 0x9f || (c) >= 0xe0 && (c) <= 0xef)
#define ISSJIS2(c)   ((c) >= 0x40 && (c) <= 0x7e || (c) >= 0x80 && (c) <= 0xfc)
#define ISEUCJP(c)   ((c) >= 0xa1 && (c) <= 0xfe)
#define ISJIS(c)     ((c) >= 0x21 && (c) <= 0x7e)

int local_mbtowc     PROTO ((wchar_t *, const char *, size_t));
int local_mblen      PROTO ((const char *, size_t));
int local_mb_cur_max PROTO ((void));

/* The locale being used for multibyte characters in string/char literals.  */
extern char *literal_codeset;
#endif /* MULTIBYTE_CHARS */

#endif /* __GCC_MBCHAR_H__ */
