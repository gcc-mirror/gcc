/* mbchar.h - Various declarations for functions found in mbchar.c
   Copyright (C) 1998 Free Software Foundation, Inc.
 */

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
