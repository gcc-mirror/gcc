/* mcLexBuf.h provides a C interface to the mcLexBuf module.

Copyright (C) 2015-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef mcLexBufH
#define mcLexBufH


#include "mcReserved.h"

extern mcReserved_toktype mcLexBuf_currenttoken;
extern void *mcLexBuf_currentstring;
extern unsigned int mcLexBuf_currentcolumn;
extern int mcLexBuf_currentinteger;
extern void *mcLexBuf_currentcomment;


/*
   openSource - Attempts to open the source file, s.
                The success of the operation is returned.
*/

extern unsigned int mcLexBuf_openSource (void *s);


/*
   closeSource - closes the current open file.
*/

extern void mcLexBuf_closeSource(void);


/*
   reInitialize - re-initialize the all the data structures.
*/

extern void mcLexBuf_reInitialize(void);


/*
   resetForNewPass - reset the buffer pointers to the beginning ready for
                     a new pass
*/

extern void mcLexBuf_resetForNewPass(void);


/*
   getToken - gets the next token into currenttoken.
*/

extern void mcLexBuf_getToken(void);


/*
   insertToken - inserts a symbol, token, infront of the current token
                 ready for the next pass.
*/

extern void mcLexBuf_insertToken(mcReserved_toktype token);


/*
   insertTokenAndRewind - inserts a symbol, token, infront of the current token
                          and then moves the token stream back onto the inserted token.
*/

extern void mcLexBuf_insertTokenAndRewind(mcReserved_toktype token);


/*
   getPreviousTokenLineNo - returns the line number of the previous token.
*/

extern unsigned int mcLexBuf_getPreviousTokenLineNo(void);


/*
   getLineNo - returns the current line number where the symbol occurs in
               the source file.
*/

extern unsigned int mcLexBuf_getLineNo(void);


/*
   getTokenNo - returns the current token number.
*/

extern unsigned int mcLexBuf_getTokenNo(void);


/*
   tokenToLineNo - returns the line number of the current file for the
                   TokenNo. The depth refers to the include depth.
                   A depth of 0 is the current file, depth of 1 is the file
                   which included the current file. Zero is returned if the
                   depth exceeds the file nesting level.
*/

extern unsigned int mcLexBuf_tokenToLineNo(unsigned int TokenNo,
					   unsigned int depth);


/*
   getColumnNo - returns the current column where the symbol occurs in
                 the source file.
*/

extern unsigned int mcLexBuf_getColumnNo(void);


/*
   tokenToColumnNo - returns the column number of the current file for the
                     TokenNo. The depth refers to the include depth.
                     A depth of 0 is the current file, depth of 1 is the file
                     which included the current file. Zero is returned if the
                     depth exceeds the file nesting level.
*/

extern unsigned int mcLexBuf_tokenToColumnNo(unsigned int TokenNo,
					     unsigned int depth);


/*
   tokenToLocation - returns the location_t corresponding to, TokenNo.
*/

extern int mcLexBuf_tokenToLocation(unsigned int TokenNo);


/*
   findFileNameFromToken - returns the complete FileName for the appropriate
                           source file yields the token number, TokenNo.
                           The, Depth, indicates the include level: 0..n
                           Level 0 is the current. NIL is returned if n+1
                           is requested.
*/

extern void *mcLexBuf_findFileNameFromToken(unsigned int TokenNo,
					    unsigned int depth);


/*
   getFileName - returns a String defining the current file.
*/

extern void *mcLexBuf_getFileName(void);


/* ***********************************************************************
 *
 * These functions allow m2.lex to deliver tokens into the buffer
 *
 ************************************************************************* */

/*
   addTok - adds a token to the buffer.
*/

extern void mcLexBuf_addTok(mcReserved_toktype t);


/*
   addTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*/

extern void mcLexBuf_addTokCharStar (mcReserved_toktype t, void *s);


/*
   addTokInteger - adds a token and an integer to the buffer.
*/

extern void mcLexBuf_addTokInteger (mcReserved_toktype t, int i);


/*
   addTokComment - adds a token to the buffer and a comment descriptor, com.
*/

extern void mcLexBuf_addTokComment (mcReserved_toktype t, void *com);


/*
   setFile - sets the current filename to, filename.
*/

extern void mcLexBuf_setFile(void *filename);


/*
   pushFile - indicates that, filename, has just been included.
*/

extern void mcLexBuf_pushFile(void *filename);


/*
   popFile - indicates that we are returning to, filename, having finished
             an include.
*/

extern void mcLexBuf_popFile(void *filename);

#endif
