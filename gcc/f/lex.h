/* lex.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Owning Modules:
      lex.c

   Modifications:
      22-Aug-89	 JCB  1.1
	 Change for new ffewhere interface.
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_lex
#define _H_f_lex

/* Simple definitions and enumerations. */

typedef enum
  {
    FFELEX_typeNONE,
    FFELEX_typeCOMMENT,
    FFELEX_typeEOS,
    FFELEX_typeEOF,
    FFELEX_typeERROR,
    FFELEX_typeRAW,
    FFELEX_typeQUOTE,
    FFELEX_typeDOLLAR,
    FFELEX_typeHASH,
    FFELEX_typePERCENT,
    FFELEX_typeAMPERSAND,
    FFELEX_typeAPOSTROPHE,
    FFELEX_typeOPEN_PAREN,
    FFELEX_typeCLOSE_PAREN,
    FFELEX_typeASTERISK,
    FFELEX_typePLUS,
    FFELEX_typeMINUS,
    FFELEX_typePERIOD,
    FFELEX_typeSLASH,
    FFELEX_typeNUMBER,		/* Grep: [0-9][0-9]*. */
    FFELEX_typeOPEN_ANGLE,
    FFELEX_typeEQUALS,
    FFELEX_typeCLOSE_ANGLE,
    FFELEX_typeNAME,		/* Grep: [A-Za-z][A-Za-z0-9_]*. */
    FFELEX_typeCOMMA,
    FFELEX_typePOWER,		/* "**". */
    FFELEX_typeCONCAT,		/* "//". */
    FFELEX_typeDEBUG,
    FFELEX_typeNAMES,		/* Same as FFELEX_typeNAME in initial
				   context. */
    FFELEX_typeHOLLERITH,	/* <text> part of <nn>H<text>. */
    FFELEX_typeCHARACTER,	/* <text> part of '<text>' or "<text>". */
    FFELEX_typeCOLON,
    FFELEX_typeSEMICOLON,
    FFELEX_typeUNDERSCORE,
    FFELEX_typeQUESTION,
    FFELEX_typeOPEN_ARRAY,	/* "(/". */
    FFELEX_typeCLOSE_ARRAY,	/* "/)". */
    FFELEX_typeCOLONCOLON,	/* "::". */
    FFELEX_typeREL_LE,		/* "<=". */
    FFELEX_typeREL_NE,		/* "<>". */
    FFELEX_typeREL_EQ,		/* "==". */
    FFELEX_typePOINTS,		/* "=>". */
    FFELEX_typeREL_GE,		/* ">=". */
    FFELEX_type
  } ffelexType;

/* Typedefs. */

typedef struct _lextoken_ *ffelexToken;
typedef void *lex_sigh_;
typedef lex_sigh_ (*lex_sigh__) (ffelexToken);
typedef lex_sigh__ (*ffelexHandler) (ffelexToken);

/* Include files needed by this one. */

#include "top.h"
#include "where.h"

/* Structure definitions. */

struct _lextoken_
  {
    long int id_;		/* DEBUG ONLY. */
    ffeTokenLength size;
    ffeTokenLength length;
    unsigned short uses;
    char *text;
    ffelexType type;
    ffewhereLine where_line;
    ffewhereColumn where_col;
    ffewhereLine currentnames_line;	/* For tracking NAMES tokens. */
    ffewhereColumn currentnames_col;	/* For tracking NAMES tokens. */
    ffewhereTrack wheretrack;	/* For tracking NAMES tokens. */
  };

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffelex_display_token (ffelexToken t);
bool ffelex_expecting_character (void);
ffelexHandler ffelex_file_fixed (ffewhereFile wf, FILE *f);
ffelexHandler ffelex_file_free (ffewhereFile wf, FILE *f);
void ffelex_hash_kludge (FILE *f);
void ffelex_init_1 (void);
bool ffelex_is_names_expected (void);
char *ffelex_line (void);
ffewhereColumnNumber ffelex_line_length (void);
ffewhereLineNumber ffelex_line_number (void);
void ffelex_set_expecting_hollerith (long length, char which,
				     ffewhereLine line,
				     ffewhereColumn column);
void ffelex_set_handler (ffelexHandler first);
void ffelex_set_hexnum (bool on);
void ffelex_set_include (ffewhereFile wf, bool free_form, FILE *fi);
void ffelex_set_names (bool on);
void ffelex_set_names_pure (bool on);
ffelexHandler ffelex_splice_tokens (ffelexHandler first, ffelexToken master,
				    ffeTokenLength start);
ffelexHandler ffelex_swallow_tokens (ffelexToken t, ffelexHandler handler);
ffelexToken ffelex_token_dollar_from_names (ffelexToken t,
					    ffeTokenLength start);
void ffelex_token_kill (ffelexToken t);
ffelexToken ffelex_token_name_from_names (ffelexToken t,
					  ffeTokenLength start,
					  ffeTokenLength len);
ffelexToken ffelex_token_names_from_names (ffelexToken t,
					   ffeTokenLength start,
					   ffeTokenLength len);
ffelexToken ffelex_token_new (void);
ffelexToken ffelex_token_new_character (const char *s, ffewhereLine l,
					ffewhereColumn c);
ffelexToken ffelex_token_new_eof (void);
ffelexToken ffelex_token_new_name (const char *s, ffewhereLine l,
				   ffewhereColumn c);
ffelexToken ffelex_token_new_names (const char *s, ffewhereLine l,
				    ffewhereColumn c);
ffelexToken ffelex_token_new_number (const char *s, ffewhereLine l,
				     ffewhereColumn c);
ffelexToken ffelex_token_new_simple_ (ffelexType type, ffewhereLine l,
				      ffewhereColumn c);
ffelexToken ffelex_token_number_from_names (ffelexToken t,
					    ffeTokenLength start);
ffelexToken ffelex_token_uscore_from_names (ffelexToken t,
					    ffeTokenLength start);
ffelexToken ffelex_token_use (ffelexToken t);

/* Define macros. */

#define ffelex_init_0()
#define ffelex_init_2()
#define ffelex_init_3()
#define ffelex_init_4()
#define ffelex_is_firstnamechar(c) \
  (ISALPHA ((c)) || ((c) == '_'))
#define ffelex_terminate_0()
#define ffelex_terminate_1()
#define ffelex_terminate_2()
#define ffelex_terminate_3()
#define ffelex_terminate_4()
#define ffelex_token_length(t) ((t)->length)
#define ffelex_token_new_eos(l,c) \
  ffelex_token_new_simple_ (FFELEX_typeEOS, (l), (c))
#define ffelex_token_new_period(l,c) \
  ffelex_token_new_simple_ (FFELEX_typePERIOD, (l), (c))
#define ffelex_token_strcmp(t1,t2) strcmp ((t1)->text, (t2)->text)
#define ffelex_token_text(t) ((t)->text)
#define ffelex_token_type(t) ((t)->type)
#define ffelex_token_where_column(t) ((t)->where_col)
#define ffelex_token_where_filename(t) \
  ffewhere_line_filename ((t)->where_line)
#define ffelex_token_where_filelinenum(t) \
  ffewhere_line_filelinenum((t)->where_line)
#define ffelex_token_where_line(t) ((t)->where_line)
#define ffelex_token_where_line_number(t) \
  ffewhere_line_number ((t)->where_line)
#define ffelex_token_wheretrack(t) ((t)->wheretrack)

/* End of #include file. */

#endif
