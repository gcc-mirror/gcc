/* C Compatible Compiler Preprocessor (CCCP)
Copyright (C) 1986, 1987, 1989, 2000 Free Software Foundation, Inc.
                    Written by Paul Rubin, June 1986
		    Adapted to ANSI C, Richard Stallman, Jan 1987
		    Dusted off, polished, and adapted for use as traditional
		    preprocessor only, Zack Weinberg, Jul 2000

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef _TRADCPP_H_
#define _TRADCPP_H_

extern void error PARAMS ((const char *msgid, ...)) ATTRIBUTE_PRINTF_1;
extern void warning PARAMS ((const char *msgid, ...)) ATTRIBUTE_PRINTF_1;
extern void fatal PARAMS ((const char *msgid, ...)) ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void error_with_line PARAMS ((int, const char *msgid, ...)) ATTRIBUTE_PRINTF_2;
extern void error_from_errno PARAMS ((const char *msgid));

extern void perror_with_name PARAMS ((const char *msgid));
extern void pfatal_with_name PARAMS ((const char *msgid)) ATTRIBUTE_NORETURN;
extern void fancy_abort PARAMS ((int, const char *)) ATTRIBUTE_NORETURN;

extern struct hashnode *lookup PARAMS ((const unsigned char *, int, int));
extern int parse_c_expression PARAMS ((const char *));  /* in tradcif.y */

/* some external tables of character types */
extern unsigned char is_idstart[], is_idchar[];

#endif /* ! _TRADCPP_H_ */
