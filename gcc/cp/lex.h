/* Define constants and variables for communication with parse.y.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   2000 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)
   and by Brendan Kehoe (brendan@cygnus.com).

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */



enum rid
{
  RID_UNUSED,
  RID_INT,
  RID_BOOL,
  RID_CHAR,
  RID_WCHAR,
  RID_FLOAT,
  RID_DOUBLE,
  RID_VOID,

  /* C++ extension */
  RID_CLASS,
  RID_RECORD,
  RID_UNION,
  RID_ENUM,
  RID_LONGLONG,

  /* This is where grokdeclarator starts its search when setting the specbits.
     The first seven are in the order of most frequently used, as found
     building libg++.  */
  RID_FIRST_MODIFIER,

  RID_EXTERN = RID_FIRST_MODIFIER,
  RID_CONST,
  RID_LONG,
  RID_TYPEDEF,
  RID_UNSIGNED,
  RID_SHORT,
  RID_INLINE,

  RID_STATIC,

  RID_REGISTER,
  RID_VOLATILE,
  RID_FRIEND,
  RID_VIRTUAL,
  RID_EXPLICIT,
  RID_EXPORT,
  RID_SIGNED,
  RID_AUTO,
  RID_MUTABLE,
  RID_COMPLEX,
  RID_RESTRICT,

  RID_LAST_MODIFIER = RID_RESTRICT,
  /* This is where grokdeclarator ends its search when setting the
     specbits.  */

  RID_PUBLIC,
  RID_PRIVATE,
  RID_PROTECTED,
  RID_EXCEPTION,
  RID_TEMPLATE,
  RID_NULL,
  /* Before adding enough to get up to 64, the RIDBIT_* macros
     will have to be changed a little.  */
  RID_MAX
};

/* The type that can represent all values of RIDBIT.  */
/* We assume that we can stick in at least 32 bits into this.  */
typedef struct { unsigned long idata[2]; }
     RID_BIT_TYPE;

/* Be careful, all these modify N twice.  */
#define RIDBIT_SETP(N, V) (((unsigned long)1 << (int) ((N)%32))		      \
			    & (V).idata[(N)/32])
#define RIDBIT_NOTSETP(NN, VV) (! RIDBIT_SETP (NN, VV))
#define RIDBIT_SET(N, V) do {						      \
				(V).idata[(N)/32]			      \
				  |= ((unsigned long)1 << (int) ((N)%32));    \
			      } while (0)
#define RIDBIT_RESET(N, V) do {						      \
				  (V).idata[(N)/32]			      \
				    &= ~((unsigned long)1 << (int) ((N)%32)); \
				} while (0)
#define RIDBIT_RESET_ALL(V) do {					      \
				   (V).idata[0] = 0;     		      \
				   (V).idata[1] = 0;			      \
				 } while (0)
#define RIDBIT_ANY_SET(V) ((V).idata[0] || (V).idata[1])

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
extern tree ridpointers[(int) RID_MAX];

/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
extern tree lastiddecl;

extern char *token_buffer;	/* Pointer to token buffer.  */

/* Back-door communication channel to the lexer.  */
extern int looking_for_typename;
extern int looking_for_template;

/* Tell the lexer where to look for names.  */
extern tree got_scope;
extern tree got_object;

/* Pending language change.
   Positive is push count, negative is pop count.  */
extern int pending_lang_change;

extern int yylex PARAMS ((void));

extern struct lang_decl *free_lang_decl_chain;
