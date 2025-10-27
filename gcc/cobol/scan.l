/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
%top{
#include "config.h"
}
%{
#include <fstream>  // Before cobol-system because it uses poisoned functions
#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#undef yy_flex_debug

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "parse.h"
#include "cdf.h"
#include "copybook.h"
#include "scan_ante.h"
#include "lexio.h"
#include "../../libgcobol/exceptl.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wunused-function"

%}

    /* C99 floating point constant, per flex(1) appendix "common patterns" */
dseq      ([[:digit:]]+)
dseq_opt  ([[:digit:]]*)
Afrac      (({dseq_opt}"."{dseq})|{dseq}".") /* American */
frac      (({dseq_opt}[.,]{dseq})|{dseq}[.,])
exp       ([eE][+-]?{dseq})
exp_opt   ({exp}?)
fsuff     [flFL]
fsuff_opt ({fsuff}?)
hpref     (0[xX])
hdseq     ([[:xdigit:]]+)
hdseq_opt ([[:xdigit:]]*)
hfrac     (({hdseq_opt}"."{hdseq})|({hdseq}"."))
bexp      ([pP][+-]?{dseq})
dfc       (({frac}{exp_opt}{fsuff_opt})|({dseq}{exp}{fsuff_opt}))
hfc       (({hpref}{hfrac}{bexp}{fsuff_opt})|({hpref}{hdseq}{bexp}{fsuff_opt}))

boolseq   (([''][01]+[''])|([""][01]+[""]))
hexseq    ((['']{hdseq}[''])|([""]{hdseq}[""]))
nonseq    (([''][[:alnum:]]+][''])|([""][[:alnum:]]+[""]))

INTEGER   0*[1-9][[:digit:]]*
INTEGERZ  [[:digit:]]+

NONWORD   [^[:alnum:]$_-]+

SPC  [[:space:]]+
OSPC [[:space:]]*
BLANK  [[:blank:]]+
OBLANK [[:blank:]]*
EOL  \r?\n
BLANK_EOL   [[:blank:]]*{EOL}
BLANK_OEOL  [[:blank:]]*{EOL}?

PICTURE   [^[:space:]]+

DOTSEP    [.]+[[:space:]]
DOTEOL    [[:blank:]]*[.]{BLANK_EOL}

SKIP	  [[:blank:]]*SKIP[123][[:blank:]]*[.]?{BLANK_EOL}
TITLE	  [[:blank:]]*TITLE($|[.]|[^\n]*)

COUNT     [(][[:digit:]]+[)]
N9        9+|(9{COUNT})
NP        P+|(P{COUNT})

UNSIGNED  [[:space:]]+UNSIGNED
SIGNED    [[:space:]]+SIGNED

ALNUM     [AX9]+

AX       [AX]{COUNT}?
B0       [B0/]{COUNT}?
ALPHEDREQ   ({N9}*{AX}+{N9}*{B0}+{N9}*)|({N9}*({B0}|[.])+{N9}*{AX}+{N9}*)
ALPHED   {ALPHEDREQ}([AX9B0/]{COUNT}?)*

			/* Must contain at least one 0, B, /, Z, *, +,
                         * (comma), ., –, CR, DB, or cs. Can contain
                         * Ps, 9s, and one V. Must describe 1 to 31
                         * digit positions, which can be represented
                         * by 9s, zero suppression symbols (Z, *), and
                         * floating insertion symbols (+, –, cs).
                         * Cannot end with '.'.  // BPVZ90/,.+- CR DB * cs
			 */
NUMEDCHAR  [BPVZ90/,]+{COUNT}?
NUMEDCHARS {NUMEDCHAR}([.]?{NUMEDCHAR})*
NUMED     ([+-]{NUMEDCHARS}+)|({NUMEDCHARS}+[+-])
CURRENCY [A-Zfhijklmoqtuwy\x80-\xFF]{-}[ABCDEGNPRSVXZ]
NUMEDCUR (([.]?[-$0B/Z*+,P9()V+–]|{CURRENCY}+|{COUNT})+([.][$0B/Z*+P9()V+\–])*)+

NUMEDITED         {NUMED}|{NUMEDCUR}
EDITED   {ALPHED}|{NUMED}|{NUMEDCUR}

DATE_FMT_B (YYYYMMDD)|(YYYYDDD)|(YYYYWwwD)
DATE_FMT_E (YYYY-MM-DD)|(YYYY-DDD)|(YYYY-Www-D)
DATE_FMT   {DATE_FMT_B}|{DATE_FMT_E}

TIME_FMT1 hhmmss([.,]s+)?
TIME_FMT3 hhmmss([.,]s+)?Z
TIME_FMT5 hhmmss([.,]s+)?[+]hhmm
TIME_FMT2 hh:mm:ss([.,]s+)?
TIME_FMT4 hh:mm:ss([.,]s+)?Z
TIME_FMT6 hh:mm:ss([.,]s+)?[+]hh:mm

TIME_FMT_B {TIME_FMT1}|{TIME_FMT3}|{TIME_FMT5}
TIME_FMT_E {TIME_FMT2}|{TIME_FMT4}|{TIME_FMT6}
TIME_FMT   {TIME_FMT_B}|{TIME_FMT_E}

DATETIME_FMT ({DATE_FMT_B}T{TIME_FMT_B})|({DATE_FMT_E}T{TIME_FMT_E})

NAME      [[:alnum:]]+([_-]+[[:alnum:]]+)*
SUBELEMS  {NAME}({SPC}{NAME})*

EOP (EOP|END-OF-PAGE)

PARENS [(]{OSPC}[)]
SUBSCRIPT [(]{OSPC}{SUBELEMS}{OSPC}[)]
NAMEQUAL  OF{SPC}{NAME}
NAMEQUALS {NAMEQUAL}({SPC}{NAMEQUAL})*

STRING   [^\r\n""]+
STRING1  [^\r\n'']+
			/* comma & semicolon must be followed by a space */
COMMA    [,;][[:blank:]]*

ISNT	 (IS{SPC})?NOT


COMMENTARY AUTHOR|DATE-COMPILED|DATE-WRITTEN|INSTALLATION|SECURITY

SORT_MERGE    SORT(-MERGE)?

LESS_THAN (IS{SPC})?LESS({SPC}THAN)?
GREATER_THAN (IS{SPC})?GREATER({SPC}THAN)?
OR_EQUAL  OR{SPC}EQUALS?({SPC}TO)?

			/* for reasons unclear, flex refuses {SPC} here */
SIZE_ERROR (ON[[[:space:]]+)?SIZE[[:space:]]+ERROR

VARTYPE  NUMERIC|ALPHABETIC|ALPHABETIC_LOWER|ALPHABETIC_UPPER|DBCS|KANJI
NAMTYP   {NAME}|{VARTYPE}

NL       [[:blank:]]*{EOL}[[:blank:]]*

PUSH_FILE \f?[#]FILE{SPC}PUSH{SPC}[^\f]+\f
POP_FILE  \f?[#]FILE{SPC}POP\f
LINE_DIRECTIVE ^[#]line{SPC}[[:alnum:]]+{SPC}[""''].+\n

%x procedure_div ident_state addr_of function classify
%x program_id_state comment_entries
%x date_state field_level field_state dot_state
%x numeric_state name_state
%x quoted1 quoted2 quoteq
%x picture picture_count integer_count
%x basis copy_state sort_state
%x cdf_state bool_state hex_state subscripts numstr_state exception
%x datetime_fmt raising partial_name cobol_words

%option debug noyywrap stack yylineno case-insensitive
%%
   /* CDF */
<bool_state>{
  [''""]/[01]
  [01]+/[''""]		{ if( copy_state == YY_START ) {
			    ydflval.boolean = ((*yytext == 1) ^ is_not);
			    return YDF_BOOL;
			  }
			  yylval.numstr.radix = boolean_e;
			  yylval.numstr.string = xstrdup(yytext);
			  if( ! original_number(yylval.numstr.string) ) {
			    error_msg(yylloc, "input inconceivably long");
			    return NO_CONDITION;
			  }
			  static int nwarn;
			  if( !nwarn++ )
			  not_implemented("Boolean literals are "
					     "not expected to work correctly");
			  return NUMSTR;
			}
  [''""]		{ yy_pop_state(); }
}
<hex_state>{
  [''""]/{hdseq}
  {hdseq}/[''""]	{ if( copy_state == YY_START ) {
			    ydflval.number = integer_of(yytext, true);
			    return YDF_NUMBER;
			  }
			  if( 0 == yyleng % 2 ) {
                            yylval.literal.set_data( yyleng/2, hex_decode(yytext) );
			    update_location_col(yytext, -3);
			    return LITERAL;
			  }
			  dbgmsg( "hex literal '%s' "
				  "has an odd number (%d) of characters",
				  yytext, yyleng );
			  return '@'; // invalid token
			}
  [''""] 		{ yy_pop_state(); }
}

    /* Initial start condition only.  */

WORKING-STORAGE{SPC}SECTION {
                yy_push_state(field_state);
                return WORKING_STORAGE_SECT; }
LOCAL-STORAGE{SPC}SECTION {
                  yy_push_state(field_state);
                  return LOCAL_STORAGE_SECT; }
WORKING-STORAGE	{ return WORKING_STORAGE; }
LOCAL-STORAGE 	{ return LOCAL_STORAGE; }
SCREEN 		{ return SCREEN; }

LINKAGE{SPC}SECTION {
                yy_push_state(field_state);
                return LINKAGE_SECT; }

FUNCTION-ID{OSPC}{DOTSEP}?	{ yy_push_state(ident_state);
                	  	  yy_push_state(program_id_state);
                  		  yy_push_state(name_state); return FUNCTION; }

PROGRAM-ID{OSPC}{DOTSEP}?	{ yy_push_state(ident_state);
	                  	  yy_push_state(program_id_state);
        	          	  yy_push_state(name_state); return PROGRAM_ID; }

PROCEDURE{SPC}DIVISION	{ yy_push_state(procedure_div);
                                  return PROCEDURE_DIV; }
<comment_entries>{
  (ENVIRONMENT|DATA|PROCEDURE){SPC}DIVISION  { myless(0); yy_pop_state(); }
  {BLANK_EOL}
  [^[:space:]]{1,512}{BLANK_OEOL}  // about 1/2 KB at a time
}

<ident_state>{
  {BLANK_OEOL}
  ID(ENTIFICATION)?{SPC}DIVISION	{ myless(0); yy_pop_state(); }
  (ENVIRONMENT|DATA|PROCEDURE){SPC}DIVISION {
					  myless(0); yy_pop_state(); }
  OPTIONS				{ myless(0); yy_pop_state(); }

  AS{SPC}[""] 	{ yy_push_state(quoted2); return AS; }
  AS{SPC}[''] 	{ yy_push_state(quoted1); return AS; }
  IS		{ pop_return IS; }

  {COMMENTARY} { BEGIN(comment_entries); }
}

<INITIAL>{
  COBOL		{ return COBOL; }
  CODE-SET	{ return CODESET; }
  FUNCTION    	{ return FUNCTION; }
  GLOBAL	{ return GLOBAL; }

  ^[[:blank:]]*0?1/[[:space:]] { /* If in File Section parse record */
                            yy_push_state(field_state);
                            yy_set_bol(1);
                            myless(0); }

  END{SPC}PROGRAM        { yy_push_state(name_state);
                                  return program_level() > 1?
                                         END_SUBPROGRAM : END_PROGRAM; }

  END{SPC}FUNCTION        { yy_push_state(name_state);
                                  return program_level() > 1?
                                         END_SUBPROGRAM /*invalid*/ :
                                         END_FUNCTION; }
}

<INITIAL,procedure_div,cdf_state>{

	/* unused Context Words */
ARITHMETIC			{ return ARITHMETIC; }
ATTRIBUTE			{ return ATTRIBUTE; }
AUTO				{ return AUTO; }
AUTOMATIC			{ return AUTOMATIC; }
AWAY-FROM-ZERO			{ return AWAY_FROM_ZERO; }
BACKGROUND-COLOR		{ return BACKGROUND_COLOR; }
BELL				{ return BELL; }
BINARY-ENCODING			{ return BINARY_ENCODING; }
BLINK				{ return BLINK; }
CAPACITY			{ return CAPACITY; }

CENTER		{
		  if( ! dialect_ibm() ) return CENTER;
		  yylval.string = xstrdup(yytext);
		  return typed_name(yytext);
		}

	/* figurative constants that are otherwise matched as names */

ZEROE?S?/{OSPC}{DOTSEP}		{ return ZERO; }
SPACES?/{OSPC}{DOTSEP}		{ yylval.string = NULL; return SPACES; }
QUOTES?/{OSPC}{DOTSEP}		{ return QUOTES; }
NULLS?/{OSPC}{DOTSEP}		{ return NULLS; }
LOW-VALUES?/{OSPC}{DOTSEP}	{ return LOW_VALUES; }
HIGH-VALUES?/{OSPC}{DOTSEP}	{ return HIGH_VALUES; }

BINARY				{ return BINARY; }
CLASSIFICATION			{ return CLASSIFICATION; }
CYCLE				{ return CYCLE; }
DECIMAL-ENCODING		{ return DECIMAL_ENCODING; }
ENTRY-CONVENTION		{ return ENTRY_CONVENTION; }
EOL				{ return EOL; }
EOS				{ return EOS; }
ERASE				{ return ERASE; }
EXPANDS				{ return EXPANDS; }
FLOAT-BINARY			{ return FLOAT_BINARY; }
FLOAT-DECIMAL			{ return FLOAT_DECIMAL; }
FOREGROUND-COLOR		{ return FOREGROUND_COLOR; }
FOREVER				{ return FOREVER; }
FULL				{ return FULL; }
HIGHLIGHT			{ return HIGHLIGHT; }
HIGH-ORDER-LEFT			{ return HIGH_ORDER_LEFT; }
HIGH-ORDER-RIGHT		{ return HIGH_ORDER_RIGHT; }
IGNORING			{ return IGNORING; }
IMPLEMENTS			{ return IMPLEMENTS; }
INITIALIZED			{ return INITIALIZED; }
INTERMEDIATE			{ return INTERMEDIATE; }
LC_ALL				{ return LC_ALL_kw; }
LC_COLLATE			{ return LC_COLLATE_kw; }
LC_CTYPE			{ return LC_CTYPE_kw; }
LC_MESSAGES			{ return LC_MESSAGES_kw; }
LC_MONETARY			{ return LC_MONETARY_kw; }
LC_NUMERIC			{ return LC_NUMERIC_kw; }
LC_TIME				{ return LC_TIME_kw; }
LENGTH				{ return LENGTH; }
LENGTH{SPC}OF			{ return LENGTH_OF; }
LOCALE				{ return LOCALE; }
LOWLIGHT			{ return LOWLIGHT; }
NEAREST-AWAY-FROM-ZERO		{ return NEAREST_AWAY_FROM_ZERO; }
NEAREST-EVEN			{ return NEAREST_EVEN; }
NEAREST-TOWARD-ZERO		{ return NEAREST_TOWARD_ZERO; }
NONE				{ return NONE; }
NORMAL				{ return NORMAL; }
NUMBERS				{ return NUMBERS; }
PREFIXED			{ return PREFIXED; }
PREVIOUS			{ return PREVIOUS; }
PROTOTYPE			{ return PROTOTYPE; }
PROHIBITED			{ return PROHIBITED; }
RAISING{SPC}/LAST[[:space:]]	{ yy_push_state(raising); return RAISING; }
RELATION			{ return RELATION; }
REQUIRED			{ return REQUIRED; }
REVERSE-VIDEO			{ return REVERSE_VIDEO; }
ROUNDING			{ return ROUNDING; }
SECONDS				{ return SECONDS; }
SECURE				{ return SECURE; }
SHORT				{ return SHORT; }
SIGNED				{ return SIGNED_kw; }
STANDARD-BINARY			{ return STANDARD_BINARY; }
STANDARD-DECIMAL		{ return STANDARD_DECIMAL; }
STATEMENT			{ return STATEMENT; }
STEP				{ return STEP; }
STRONG				{ return STRONG; }
STRUCTURE			{ return STRUCTURE; }

TALLY				{ // Use TALLY register for IBM, else it's just a name.
				  static const char tally[] = "_TALLY";
				  auto p = dialect_ibm()? tally : tally + 1;
				  yylval.string = xstrdup(p);
				  return NAME;
				}

TOWARD-GREATER			{ return TOWARD_GREATER; }
TOWARD-LESSER			{ return TOWARD_LESSER; }
TRUNCATION			{ return TRUNCATION; }
UCS-4				{ return UCS_4; }
UNDERLINE			{ return UNDERLINE; }
UNSIGNED			{ return UNSIGNED_kw; }
UTF-16				{ return UTF_16; }
UTF-8				{ return UTF_8; }

SYSIN		{ return SYSIN; }
SYSIPT		{ return SYSIPT; }
SYSOUT		{ return SYSOUT; }
SYSLIST		{ return SYSLIST; }
SYSLST		{ return SYSLST; }
SYSPUNCH	{ return SYSPUNCH; }
SYSPCH		{ return SYSPCH; }
CONSOLE		{ return CONSOLE; }
C01		{ return C01; }
C02		{ return C02; }
C03		{ return C03; }
C04		{ return C04; }
C05		{ return C05; }
C06		{ return C06; }
C07		{ return C07; }
C08		{ return C08; }
C09		{ return C09; }
C10		{ return C10; }
C11		{ return C11; }
C12		{ return C12; }
CSP		{ return CSP; }
S01		{ return S01; }
S02		{ return S02; }
S03		{ return S03; }
S04		{ return S04; }
S05		{ return S05; }
AFP-5A		{ return AFP_5A; }
STDIN		{ return STDIN; }
STDOUT		{ return STDOUT; }
STDERR		{ return STDERR; }
SYSERR		{ return STDERR; }

ARGUMENT-NUMBER  	{ return ARGUMENT_NUMBER; }
ARGUMENT-VALUE   	{ return ARGUMENT_VALUE; }
ENVIRONMENT-NAME 	{ return ENVIRONMENT_NAME; }
ENVIRONMENT-VALUE	{ return ENVIRONMENT_VALUE; }

CANCEL		{ return CANCEL; }
COMMIT		{ return COMMIT; }
COMMON		{ return COMMON; }
CONTINUE	{ return CONTINUE; }

COPY		{
                  yy_push_state(copy_state);
                  myless(0);
                }

EXTEND		{ return EXTEND;}
INITIALIZE	{ return INITIALIZE; }
INSPECT		{ return INSPECT; }
INVOKE		{ return INVOKE; }
INTRINSIC	{ return INTRINSIC; }
MERGE		{ return MERGE; }
UNSTRING	{ return UNSTRING; }
XML		{ return XML; }
XMLGENERATE	{ return XMLGENERATE; }
XMLPARSE	{ return XMLPARSE; }

ZEROE?S?	{ return ZERO; }

WRITE		{ return WRITE; }

WITH{SPC}NO/[[:^alnum:]_-]		{ return NO; }

WITH		{ return WITH; }

WHEN	        { return WHEN; }
ALSO	        { return ALSO; }

VARYING		{ return VARYING; }
VALUE		{ return VALUE; }
UTILITY		{ return UTILITY; }
USING		{ return USING; }
USE{SPC}(AFTER{SPC})?/(EC|EXCEPTION)		{ return USE; }
USE		{ return USE; }

UPON		{ return UPON; }
UP		{ return UP; }
UPSI		{ return UPSI; }
UNTIL		{ return UNTIL; }
UNITS		{ return UNITS; }
UNIT-RECORD	{ return UNIT_RECORD; }
UNIT		{ return UNIT; }
TYPE		{ return TYPE; }
TRY		{ return TRY; }
FALSE		{ return FALSE_kw; }
TRUE		{ return TRUE_kw; }
TRANSFORM	{ return TRANSFORM; }
TRACKS		{ return TRACKS; }
TRACK-AREA	{ return TRACK_AREA; }
TRACE		{ return TRACE; }
TOP		{ return TOP; }
TO		{ return TO; }
TIMES		{ return TIMES; }
THRU|THROUGH    { return THRU; }
THEN		{ return THEN; }
THAN		{ return THAN; }
TEST		{ return TEST; }
TERMINATE	{ return TERMINATE; }
TALLYING	{ return TALLYING; }
TALLY		{ return TALLY; }
SYSPUNCH	{ return SYSPUNCH; }
SYSOUT		{ return SYSOUT; }
SYSIN		{ return SYSIN; }
SYMBOLIC	{ return SYMBOLIC; }
SYMBOL		{ return SYMBOL; }
SUM		{ return SUM; }
SUBTRACT	{ return SUBTRACT; }
STOP 		{ return STOP ; }
START 		{ return START ; }
STATUS 		{ return STATUS ; }
STANDARD 	{ return STANDARD ; }
STANDARD-[12] 	{ return STANDARD_ALPHABET; }
STANDARD 	{ return STANDARD ; }
SPECIAL-NAMES 	{ return SPECIAL_NAMES ; }
SPACES?		{ yylval.string = NULL; return SPACES; }
SOURCE-COMPUTER	{ return SOURCE_COMPUTER; }
SOURCE		{ return SOURCE; }
{SORT_MERGE}	{ return SORT; }
SIZE		{ return SIZE; }
SIGN		{ return SIGN; }
SET		{ return SET; }
SHARING		{ return SHARING; }
SEQUENCE	{ return SEQUENCE; }

SEQUENTIAL	{ return SEQUENTIAL; }
SENTENCE	{ return SENTENCE; }
SELECT		{ return SELECT; }
SECURITY	{ return SECURITY; }

SECTION{SPC}[+-]?{INTEGERZ}/{OSPC}{DOTSEP}	{
				  auto eotext = yytext + yyleng;
				  auto p = std::find_if(yytext, eotext, fisspace);
				  p = std::find_if(p, eotext, nonspace);
				  yylval.string = xstrdup(p);
				  return SECTION;
				}

SECTION{OSPC}{DOTSEP}/USE[[:space:]]	{ yylval.string = NULL; return SECTION; }
SECTION					{ yylval.string = NULL; return SECTION; }

PARAGRAPH	{ return PARAGRAPH; }
SEARCH		{ return SEARCH; }

SAME		{ return SAME; }
RUN		{ return RUN; }
ROUNDED		{ return ROUNDED; }
RIGHT		{ return RIGHT; }
RH		{ return RH; }
RF		{ return RF; }
REWRITE		{ return REWRITE; }
REWIND		{ return REWIND; }
REVERSED	{ return REVERSED; }
RETURN		{ return RETURN; }
RESTRICTED	{ return RESTRICTED; }

RESUME		{
		  if( ! dialect_ibm() ) return RESUME;
		  yylval.string = xstrdup(yytext);
		  return typed_name(yytext);
		}

RESET		{ return RESET; }
RESERVE		{ return RESERVE; }
RERUN		{ return RERUN; }

REPOSITORY	{ return REPOSITORY; }

REPORTS		{ return REPORTS; }
REPORTING	{ return REPORTING; }
REPORT		{ return REPORT; }
REPLACING	{ return REPLACING; }
REPLACE		{ return REPLACE; }
RENAMES		{ return RENAMES; }
REMAINDER	{ return REMAINDER; }
REMARKS		{ return REMARKS; }
RELEASE		{ return RELEASE; }

RELATIVE{SPC}(KEY{SPC})?(IS{SPC})?{NAME} {
		  // RELATIVE ... NAME  returns KEY
		  // RELATIVE ... token returns RELATIVE
		  std::reverse_iterator<char *>
			p(yytext), pend(yytext + yyleng);
		  p = std::find_if(pend, p, fisspace);
		  char *name = p.base();
		  assert(ISALNUM(name[0]));
		  assert(ISSPACE(name[-1]));
		  int token = keyword_tok(name)? RELATIVE : KEY;
		  myless( name - yytext );
		  return token;
		}
RELATIVE	{ return RELATIVE; }

REEL		{ return REEL; }
RECORDING	{ return RECORDING; }
RECORD		{ return RECORD; }
RECORD{SPC}(IS) 		{ return RECORD; }
RECORDS{SPC}(ARE)	{ return RECORDS; }
RECORDS		{ return RECORDS; }
READY		{ return READY; }
READ		{ return READ; }
RD		{ return RD; }
RANDOM		{ return RANDOM; }
RAISE		{ return RAISE; }
QUOTES		{ return QUOTES; }
QUOTE		{ return QUOTES; }

PROGRAM         { return PROGRAM_kw; }
PROCESS		{ return PROCESS; }
PROCEED		{ return PROCEED; }
PROCEDURE	{ return PROCEDURE; }
PROCEDURES	{ return PROCEDURES; }

PRINT-SWITCH	{ return PRINT_SWITCH; }
POSITIVE	{ return POSITIVE; }
PLUS		{ return PLUS; }
PICTURE         { return PICTURE; }
PH		{ return PH; }
PF		{ return PF; }
PERFORM		  { yylval.boolean = false; return PERFORM; }
PERFORM{SPC}CYCLE { yylval.boolean = true;  return PERFORM; }

PAGE-COUNTER	{ return PAGE_COUNTER; }
PAGE		{ return PAGE; }
PADDING		{ return PADDING; }
OUTPUT		{ return OUTPUT; }
OTHERWISE	{ return OTHERWISE; }
OTHER    	{ return OTHER; }
ORGANI[SZ]ATION	{ return ORGANIZATION; }
ORDER		{ return ORDER; }

OPTIONS{SPC}?[.]	{ return OPTIONS; }
OPTIONAL	{ return OPTIONAL; }
OPEN		{ return OPEN; }
ON		{ return ON; }
OMITTED		{ return OMITTED; }
OFF		{ return OFF; }
OF		{ return OF; }

OBJECT-COMPUTER	{ return OBJECT_COMPUTER; }

MEMORY{SPC}(SIZE{SPC})?[0-9]+{SPC}(WORDS|CHARACTERS|MODULES) {/*ignore*/}

NUMERIC			{ return NUMERIC; }
NUMERIC-EDITED		{ return NUMERIC_EDITED; }

NULLS?		{ return NULLS; }

NOTE		{ return NOTE; }
NOT		{ return NOT; }
NO		{ return NO; }
NEXT		{ return NEXT; }
NEGATIVE	{ return NEGATIVE; }
NATIVE		{ return NATIVE; }
NAMED		{ return NAMED; }
NAT		{ return NAT; }
NATIONAL		{ return NATIONAL; }
NATIONAL-EDITED		{ return NATIONAL_EDITED; }
MULTIPLY	{ return MULTIPLY; }
MOVE		{ return MOVE; }
MODE		{ return MODE; }
LOW-VALUES?	{ return LOW_VALUES; }
LOCK{SPC}ON	{ return LOCK_ON; }
LOCK		{ return LOCK; }
LINKAGE		{ return LINKAGE; }
LINES		{ return LINES; }
LINE-COUNTER	{ return LINE_COUNTER; }
LINAGE		{ return LINAGE; }
LINE		{ return LINE; }
LIMITS		{ return LIMITS; }
LIMIT		{ return LIMIT; }

LEADING		{ return LEADING; }
LAST		{ return LAST; }
LABEL		{ return LABEL; }
TRAILING        { return TRAILING; }

KEY({SPC}IS)?		{ return KEY; }
KANJI		{ return KANJI; }

JUSTIFIED	{ return JUSTIFIED; }

IS		{ return IS; }

INTO		{ return INTO; }
  /* INSTALLATION	{ return INSTALLATION; } */

INPUT-OUTPUT{SPC}SECTION	{ return INPUT_OUTPUT_SECT; }

INPUT		{ return INPUT; }
INITIATE	{ return INITIATE; }
INITIALIZE	{ return INITIALIZE; }
INITIAL		{ return INITIAL_kw; }
INDICATE	{ return INDICATE; }
INDEXED		{ return INDEXED; }
INCLUDE		{ return INCLUDE; }
IN		{ return IN; }
IF		{ return IF; }

ID(ENTIFICATION)?{SPC}DIVISION	{ BEGIN(0); return IDENTIFICATION_DIV; }

IBM-360		{ return IBM_360; }
I-O-CONTROL	{ return IO_CONTROL; }
I-O		{ return IO; }
HOLD		{ return HOLD; }
HIGH-VALUES?	{ return HIGH_VALUES; }
HEX		{ return HEX; }
HEADING		{ return HEADING; }
GROUP		{ return GROUP; }

GOBACK		{ return GOBACK; }
BEAT-FEET	{ return GOBACK; }

GO({SPC}TO)?	{ return GOTO; }

GLOBAL		{ return GLOBAL; }
GIVING		{ return GIVING; }
GENERATE	{ return GENERATE; }

FROM/[[:space:]]+(DATE|DAY|TIME)[[:space:]] { yy_push_state(date_state); return FROM; }
FROM		{ return FROM; }
FREE		{ return FREE; }

FORM-OVERFLOW	{ return FORM_OVERFLOW; }
FOR		{ return FOR; }
FOOTING		{ return FOOTING; }
FIRST		{ return FIRST; }
FINAL		{ return FINAL; }
FILE-LIMIT	{ return FILE_LIMIT; }
FILE-CONTROL	{ return FILE_CONTROL; }

FILE{SPC}SECTION { return FILE_SECT; }

FILE		{ return FILE_KW; }

FD		{ return FD; }
SD		{ return SD; }

EXTERNAL	{ return EXTERNAL; }
EXIT		{ return EXIT; }
EXHIBIT		{ return EXHIBIT; }
EXAMINE		{ return EXAMINE; }
EVERY		{ return EVERY; }
ERROR		{ return ERROR; }
EVALUATE	{ return EVALUATE; }

EQUALS?         { return '='; }
ENVIRONMENT[[:blank:]]+DIVISION	{ return ENVIRONMENT_DIV; }

ENTRY		{ return ENTRY; }
ENTER		{ return ENTER; }
END-WRITE	{ return END_WRITE; }
END-UNSTRING	{ return END_UNSTRING; }
END-SUBTRACT	{ return END_SUBTRACT; }
END-STRING	{ return END_STRING; }
END-START	{ return END_START ; }

END-SEARCH	{ return END_SEARCH; }
END-REWRITE	{ return END_REWRITE; }
END-RETURN	{ return END_RETURN; }
END-READ	{ return END_READ; }
END-PERFORM	{ return END_PERFORM; }
END-MULTIPLY	{ return END_MULTIPLY; }

END-IF		{ return END_IF; }
END-EVALUATE	{ return END_EVALUATE; }
END-DIVIDE	{ return END_DIVIDE; }
END-DISPLAY     { return END_DISPLAY; }
END-DELETE	{ return END_DELETE; }
END-COMPUTE	{ return END_COMPUTE; }
END-CALL	{ return END_CALL; }
END-ADD		{ return END_ADD; }
END-ACCEPT	{ return END_ACCEPT; }
END		{ yylval.number = END; return END; }

ELSE		{ return ELSE; }

EC				{ return EC; }
EXCEPTION{SPC}CONDITION		{ return EC; }

EBCDIC		{ return EBCDIC; }

DYNAMIC		{ return DYNAMIC; }
DUPLICATES	{ return DUPLICATES; }
DOWN		{ return DOWN; }
DIVIDE		{ return DIVIDE; }

DISPLAY		{ return DISPLAY; }

DIRECT-ACCESS	{ return DIRECT_ACCESS; }
DIRECT		{ return DIRECT; }
DETAIL		{ return DETAIL; }
DESCENDING	{ return DESCENDING; }
DEPENDING	{ return DEPENDING; }

DELIMITER	{ return DELIMITER; }
DELETE		{ return DELETE; }
DEFAULT		{ return DEFAULT; }
DECLARATIVES	{ return DECLARATIVES; }
DECIMAL-POINT	{ return DECIMAL_POINT; }
DEBUGGING	{ return DEBUGGING; }
DE		{ return DE; }
EGCS		{ return EGCS; }
DBCS		{ return DBCS; }
DATE-WRITTEN	{ return DATE_WRITTEN; }
DATE-COMPILED	{ return DATE_COMPILED; }
DAY-OF-WEEK	{ return DAY_OF_WEEK; }

DATA{SPC}DIVISION{DOTEOL}		{ return DATA_DIV; }
DATA		{ return DATA; }

CURRENCY	{ return CURRENCY; }
COUNT		{ return COUNT; }

CORR(ESPONDING)?	{ return CORRESPONDING; }

CONVERTING	{ return CONVERTING; }
CONTROLS	{ return CONTROLS; }
CONTROL		{ return CONTROL; }

CONSOLE		{ return CONSOLE; }

CONTAINS		{ return CONTAINS; }
CONFIGURATION{SPC}SECTION	{ return CONFIGURATION_SECT; }

COMPUTE		{ return COMPUTE; }
COMMA		{ return COMMA; }

COLUMN		{ return COLUMN; }
COLLATING	{ return COLLATING; }
CODE		{ return CODE; }
CLASS		{ return CLASS; }
CLOSE		{ return CLOSE; }

CHARACTERS	{ return CHARACTERS; }
CHARACTER	{ return CHARACTER; }
CHANGED		{ return CHANGED; }
CH		{ return CH; }
CF		{ return CF; }
CALL		{ return CALL; }

BY		{ return BY; }
BOTTOM		{ return BOTTOM; }
BEFORE		{ return BEFORE; }
BLOCK		{ return BLOCK_kw; }
BACKWARD	{ return BACKWARD; }

AT		{ return AT; }
ASSIGN		{ return ASSIGN; }
ASCENDING	{ return ASCENDING; }
AREAS		{ return AREAS; }
AREA		{ return AREA; }
ARE		{ return ARE; }
APPLY		{ return APPLY; }
ANYCASE		{ return ANYCASE; }
ANY		{ return ANY; }
ANUM            { return ANUM; }

ALTERNATE	{ return ALTERNATE; }
ALTER		{ return ALTER; }
ALSO		{ return ALSO; }

ALPHABET		{ return ALPHABET; }
ALPHABETIC		{ return ALPHABETIC; }
ALPHABETIC-LOWER	{ return ALPHABETIC_LOWER; }
ALPHABETIC-UPPER	{ return ALPHABETIC_UPPER; }
ALPHANUMERIC            { return ALPHANUMERIC; }
ALPHANUMERIC-EDITED     { return ALPHANUMERIC_EDITED; }

ALLOCATE	{ return ALLOCATE; }
ALL		{ return ALL; }
AFTER		{ return AFTER; }
ADVANCING	{ return ADVANCING; }
ADDRESS		{ return ADDRESS; }
ADD		{ return ADD; }
ACTUAL		{ return ACTUAL; }
ACCESS		{ return ACCESS; }
ACCEPT		{ return ACCEPT; }

DELETE		{ return DELETE; }
EJECT{DOTEOL}? {
		  if( ! dialect_ibm() ) {
		    dialect_error(yylloc, "EJECT is not ISO syntax,", "ibm");
		  }
		}
INSERTT		{ return INSERTT; }
LABEL		{ return LABEL; }
PROCESS		{ return PROCESS; }
SERVICE[[:blank:]]+RELOAD		{ return SERVICE_RELOAD; }
TITLE		{ return TITLE; }
USE({SPC}FOR)?		{ return USE; }

}

<field_level>{
  66/{SPC}(\f#)?{NAME} { yy_pop_state();
                          if( !parsing.on() ) orig_picture[0] = '\0';
                          if( level_needed() ) {
                            level_found();
                            yylval.number = level_of(yytext); return LEVEL66;
                          } else {
                            return numstr_of(yytext);
                          }
                        }
  78/{SPC}(\f#)?{NAME} { yy_pop_state();
                          if( !parsing.on() ) orig_picture[0] = '\0';
                          if( level_needed() ) {
                            level_found();
                            yylval.number = level_of(yytext); return LEVEL78;
                          } else {
                            return numstr_of(yytext);
                          }
                        }
  88/{SPC}(\f#)?{NAME} { yy_pop_state();
                          if( !parsing.on() ) orig_picture[0] = '\0';
                          if( level_needed() ) {
                            level_found();
                            yylval.number = level_of(yytext); return LEVEL88;
                          } else {
                            return numstr_of(yytext);
                          }
                        }
  [[:digit:]]{1,2}/[[:space:]] { yy_pop_state();
                          if( !parsing.on() ) orig_picture[0] = '\0';
                          if( level_needed() ) {
                            level_found();
                            yylval.number = level_of(yytext); return LEVEL;
                          } else {
                            return numstr_of(yytext);
                          }
                        }

  . { cbl_errx( "failed to parse field level on line %d", yylineno); }
}

<field_state>{
  ^[[:blank:]]*[[:digit:]]{1,2}{OSPC}/[.] {
                              if( !parsing.on() ) orig_picture[0] = '\0';
                              level_found();
                              yylval.number = level_of(yytext);
                              return LEVEL;
                            }

  ^[[:blank:]]+             {}
               ^[[:digit:]]{1,2}[[:space:]]              { yy_push_state(field_level); }
   [[:blank:]]*/[[:digit:]]{1,2}{SPC}(\f#)?{NAME} { yy_push_state(field_level); }

  [+-]?{INTEGERZ}           { return numstr_of(yytext); }
  [+-]?{dfc}                { char *s = xstrdup(yytext);
                              // "The decimal point can appear anywhere within
                              //  the literal except as the rightmost character."
                              size_t len = strlen(s);
                              assert(len);
                              if( s[--len] == '.' ) {
                                s[len] = '\0';
                                myless(len);
                              }
                              numstr_of(s); free(s);
                              return NUMSTR;
                           }

  PIC(TURE)?({SPC}IS)?{SPC}{PICTURE} {
				  auto pos = validate_picture();
				  myless(pos);
				  yy_push_state(picture); return PIC; }

  ANY				{ return ANY; }
  LENGTH			{ return LENGTH; }
  LENGTH{SPC}OF			{ return LENGTH_OF; }
  BASED				{ return BASED; }
  USAGE				{ return USAGE; }
  UNBOUNDED			{ return UNBOUNDED; }
				  /* use coded capacity 255 to indicate comp-x */
  COMP(UTATIONAL)?-X		{ return ucomputable(FldNumericBin5, 0xFF); }
  COMP(UTATIONAL)?-6		{ return ucomputable(FldPacked, 0); }
  COMP(UTATIONAL)?-5		{ return ucomputable(FldNumericBin5, 0); }
  COMP(UTATIONAL)?-4		{ return scomputable(FldNumericBinary, 0); }
  COMP(UTATIONAL)?-3		{ return PACKED_DECIMAL; }
  COMP(UTATIONAL)?-2		{ return ucomputable(FldFloat, 8); }
  COMP(UTATIONAL)?-1		{ return ucomputable(FldFloat, 4); }
  COMP(UTATIONAL)?		{ return ucomputable(FldNumericBinary, 0); }
  BINARY            		{ return scomputable(FldNumericBinary, 0); }

  BINARY-CHAR	   		{ return bcomputable(FldNumericBin5, 1); }
  BINARY-SHORT	  		{ return bcomputable(FldNumericBin5, 2); }
  BINARY-LONG	   		{ return bcomputable(FldNumericBin5, 4); }
  BINARY-DOUBLE			{ return bcomputable(FldNumericBin5, 8); }
  BINARY-LONG-LONG		{ if( ! dialect_mf() ) {
				    dialect_error(yylloc, yytext, "mf");
				  }
				  return bcomputable(FldNumericBin5, 8);
				}

  BIT				{ not_implemented("USAGE type: BIT");
				  return BIT; }

  FLOAT-BINARY-32		{ return ucomputable(FldFloat, 4); }
  FLOAT-BINARY-64		{ return ucomputable(FldFloat, 8); }
  FLOAT-BINARY-128		{ return ucomputable(FldFloat, 16); }
  FLOAT-DECIMAL-(16|34)		{ not_implemented("USAGE type: %<FLOAT_DECIMAL%>");
				  return FLOAT_DECIMAL; // causes syntax error
				}
  /* 21) The representation and length of a data item described with USAGE
     BINARY-CHAR, BINARY-SHORT, BINARY-LONG, BINARY-DOUBLE, FLOAT-SHORT,
     FLOAT-LONG, or FLOAT-EXTENDED is implementor-defined. */
  FLOAT-EXTENDED  		{ return ucomputable(FldFloat, 16); }
  FLOAT-LONG	  		{ return ucomputable(FldFloat, 8); }
  FLOAT-SHORT    		{ return ucomputable(FldFloat, 4); }

  SIGNED			{ return SIGNED_kw; }
  UNSIGNED			{ return UNSIGNED_kw; }

  INDEX				{ return INDEX; }
  MESSAGE-TAG			{ not_implemented("USAGE type: MESSAGE-TAG"); }
  NATIONAL			{ not_implemented("USAGE type: NATIONAL");
				  return NATIONAL; }
  OBJECT{SPC}REFERENCE		{ not_implemented("USAGE type: OBJECT REFERENCE"); }

  PACKED-DECIMAL		{ return PACKED_DECIMAL; }

  FUNCTION-POINTER     	|
  PROGRAM-POINTER      	{ yylval.field_attr = prog_ptr_e; return POINTER; }
  POINTER           	{ yylval.field_attr = none_e;     return POINTER; }

  PROCEDURE-POINTER    	{ if( dialect_gcc() ) {
			    dialect_error(yylloc, yytext, "ibm or mf");
			  }
			  yylval.field_attr = prog_ptr_e;
			  return POINTER;  // return it anyway
			}

  ZEROE?S?	        { return ZERO; }
  SPACES?		{ yylval.string = NULL; return SPACES; }
  LOW-VALUES?	        { return LOW_VALUES; }
  HIGH-VALUES?	        { return HIGH_VALUES; }
  QUOTES?		{ return QUOTES; }
  NULLS?		{ return NULLS; }

  OF		           { return OF; }
  VALUE({SPC}IS)?   { return VALUE; }
  VALUES({SPC}ARE)? { return VALUE; }
  THRU|THROUGH             { return THRU; }

  VALUES?({SPC}(IS|ARE))?{SPC}NULLS? {    return NULLPTR; }
  VALUES?({SPC}(IS|ARE))?/{SPC}[+-]?{dfc} {
                          yy_push_state(numeric_state); return VALUE; }

 (THRU|THROUGH)/{SPC}[[:digit:].,+-] {
                          yy_push_state(numeric_state); return THRU; }

  ALL			{ return ALL; }
  AS			{ return AS; }
  ASCENDING		{ return ASCENDING; }
  BLANK    		{ return BLANK; }
  BLOCK			{ return BLOCK_kw; }
  BY			{ return BY; }
  BYTE-LENGTH		{ return BYTE_LENGTH; }
  CHARACTER		{ return CHARACTER; }
  CHARACTERS		{ return CHARACTERS; }
  CODE-SET		{ return CODESET; }
  CONSTANT		{ return CONSTANT; }
  CONTAINS		{ return CONTAINS; }
  DATA			{ return DATA; }
  DEPENDING		{ return DEPENDING; }
  DESCENDING		{ return DESCENDING; }
  DISPLAY		{ return DISPLAY; }
  EJECT{DOTEOL}?  {
		  if( ! dialect_ibm() ) {
		    dialect_error(yylloc, "EJECT is not ISO syntax,", "ibm");
		  }
                  auto len = yyleng - 1;
                  if( yytext[len] == '\f' ) myless(--len);
		}
  EXTERNAL		{ return EXTERNAL; }
  FALSE			{ return FALSE_kw; }
  FROM			{ return FROM; }
  GLOBAL		{ return GLOBAL; }
  IN			{ return IN; }
  INDEXED		{ return INDEXED; }
  IS			{ return IS; }
  JUST(IFIED)?({SPC}RIGHT)?		{ return JUSTIFIED; }
  KEY			{ return KEY; }
  LABEL			{ return LABEL; }
  LEADING		{ return LEADING; }
  LEFT    		{ return LEFT; }
  MODE			{ return MODE; }
  OCCURS/{SPC}{NAME} 	{ return OCCURS; }
  OCCURS		{ yy_push_state(integer_count);  return OCCURS; }
  OF			{ return OF; }
  OMITTED		{ return OMITTED; }
  ON			{ return ON; }
  RECORD		{ return RECORD; }
  RECORDING		{ return RECORDING; }
  RECORDS		{ return RECORDS; }
  RECORDS{SPC}ARE	{ return RECORDS; }
  RECORD{SPC}IS		{ return RECORD; }
  REDEFINES		{ return REDEFINES; }
  RENAMES		{ return RENAMES; }
  RIGHT    		{ return RIGHT; }
  SEPARATE		{ return SEPARATE; }
  SET			{ return SET; }
  SAME			{ return SAME; }
  SIGN			{ return SIGN; }
  SIZE			{ return SIZE; }
  STANDARD		{ return STANDARD; }
  STRONG		{ return STRONG; }
  SYNC(HRONIZED)?	{ return SYNCHRONIZED; }
  TIMES			{ return TIMES; }
  TIMES[[:space::]]+DEPENDING		{ return DEPENDING; }
  TO			{ return TO; }
  TRAILING		{ return TRAILING; }
  TRUE			{ return TRUE_kw; }
  TYPE			{ return TYPE; }
  TYPEDEF		{ return TYPEDEF; }
  VARYING		{ return VARYING; }
  VOLATILE    		{ return VOLATILE; }
  WHEN    		{ return WHEN; }

  COPY			{
                	  yy_push_state(copy_state);
                	  myless(0);
                	}

  FD/[[:blank:]]+       { parsing.need_level(false); return FD; }
  SD/[[:blank:]]+       { parsing.need_level(false); return SD; }

  {NAME}                { // NAME here might be a token alias
		   	  int token;
		          if( 0 != (token = binary_integer_usage(yytext)) ) {
			    return token;
			  }
                          if( is_integer_token() ) return numstr_of(yytext);
			  ydflval.string = yylval.string = xstrdup(yytext);
			  token = typed_name(yytext);
			  return token == NAME88? NAME : token;
 			}

  Z?['']                { yylval.literal.set_prefix(yytext, yyleng-1);
			  yy_push_state(quoted1); }
  Z?[""]                { yylval.literal.set_prefix(yytext, yyleng-1);
			  yy_push_state(quoted2); }
  N?X/{hexseq}		{ yylval.literal.set_prefix(yytext, yyleng);
	    		  yy_push_state(hex_state); }
  N?X{nonseq}		{ dbgmsg("invalid hexadecimal value: %s", yytext);
			  return NO_CONDITION; }
  [[:blank:]]*{EOL}     {}

  WORKING-STORAGE{SPC}SECTION { return WORKING_STORAGE_SECT; }
  LOCAL-STORAGE{SPC}SECTION   { return LOCAL_STORAGE_SECT; }
  LINKAGE{SPC}SECTION         { return LINKAGE_SECT; }
  SCREEN{SPC}/SECTION	      { return SCREEN; }
  SECTION{OSPC}/{DOTSEP}      { yylval.string = NULL; return SECTION; }

  PROCEDURE{SPC}DIVISION	{ BEGIN(procedure_div); return PROCEDURE_DIV; }

  [*]>.*$ // ignore inline comment
}

<numstr_state>{
  [''""]/{hdseq}
  {hdseq}/[''""]	{
			  switch( yylval.numstr.radix ) {
			  case boolean_e:
			    if( 1 != yyleng ) {
			      error_msg(yylloc,  "syntax error: Boolean literal '%s' "
			    	      "has too many (%d) characters",
			    	      yytext, yyleng );
			      return NEG; // invalid token
			    }
			    return numstr_of(yytext, yylval.numstr.radix);
			  case hexadecimal_e:
			    if( 0 != yyleng % 2 ) {
			      error_msg(yylloc,  "syntax error: hex literal '%s' "
			    	      "has an odd number (%d) of characters",
			    	      yytext, yyleng );
			      return NEG; // invalid token
			    }
			    return numstr_of(yytext, yylval.numstr.radix);
			  default:
			    return NEG;
			  }
			}
  [''""] 		{ yy_pop_state(); }
}

   /*
    * dot dot dot: sayeth the standard:
    * 01 xxx PIC 999. VALUE something.  is a syntax error.
    * 01 xxx PIC 999.  is just three nines, and will be NumericDisplay.
    * 01 xxx PIC 999.. is three nines and a dot, and is NumericEdited.
    *
    * On entry, we might have found a newline.  If so, we accept any leading
    * blanks, and ignore blank lines.  This sets up recognizing SKIP2 etc.
    *
    * Any blank or separator period ends terminates the picture.
    */
<picture>{
  ^[[:blank:]]+
  ^{BLANK_EOL}

  {COMMA} 			|
  [[:blank:]]*{EOL}		|
  [[:blank:]]+{EOL}?		{ yy_pop_state(); /* embedded/trailing blank */ }
  {DOTSEP}[[:blank:].]+$	{ yy_pop_state(); return '.'; }
  {DOTSEP}			{ yy_pop_state(); return '.'; }


  [[:blank:]]+[-+]/{EDITED} 	{ return picset(yytext[yyleng-1]); }

  S/({N9}|{NP}|V)+ { return picset('S'); }
  V?{NP}/{N9}      { yylval.number = ndigit(yyleng);     return picset(PIC_P); }
  {N9}/{N9}*{NP}V? { yylval.number = ndigit(yyleng);     return picset(NINES); }
  {NP}V?/[,.]?     { yylval.number = ndigit(yyleng);     return picset(PIC_P); }
  {N9}*V/{N9}*     { yylval.number = ndigit(yyleng - 1); return picset(NINEV); }
  {N9}/{N9}*[,.]?  { yylval.number = ndigit(yyleng);     return picset(NINES); }
  P+/[,.]?{EOL}    { yylval.number = yyleng;             return picset(PIC_P); }

  1{1,31}/({COUNT}|[(]{NAME}[)]) {
                         yy_push_state(picture_count);
                         yylval.string = xstrdup(yytext); return picset(ONES); }
  1{1,31} {              yylval.string = xstrdup(yytext); return picset(ONES); }

  {ALNUM}/{COUNT}({ALNUM}{COUNT}?)+ {
                         yy_push_state(picture_count);
                         yylval.string = xstrdup(yytext); return picset(ALNUM); }
  {ALNUM}/{COUNT}      { yy_push_state(picture_count);
                         yylval.string = xstrdup(yytext); return picset(ALNUM); }
  {ALNUM}/[(]{NAME}[)] { yy_push_state(picture_count);
                         yylval.string = xstrdup(yytext); return picset(ALNUM); }
  {ALNUM}              { yylval.string = xstrdup(yytext); return picset(ALNUM); }

  {ALPHED}             { yylval.string = xstrdup(yytext); return picset(ALPHED); }
  {NUMEDITED}          { yylval.string = xstrdup(yytext); return picset(NUMED); }
  {NUMEDITED}[.]?CR    { yylval.string = xstrdup(yytext); return picset(NUMED_CR); }
  {NUMEDITED}[.]?DB    { yylval.string = xstrdup(yytext); return picset(NUMED_DB); }
  {NUMEDITED}[.]/{DOTEOL} {
                         yylval.string = xstrdup(yytext); return picset(NUMED); }

  [^[:space:].,;]+([.,;][^[:space:].,;]+)* {
                         yylval.string = xstrdup(yytext); return picset(ALPHED); }

  .     { dbgmsg("unrecognized character '%c' (0x%x) in PICTURE",
                    *yytext, *yytext ); return NO_CONDITION; }

}
<picture_count>{
  [(] 				{ return picset(*yytext); }
  [)]                           { pop_return picset(*yytext); }
  {INTEGER}			{ return picset(numstr_of(yytext)); }
  {NAME}			{ yylval.string = xstrdup(yytext);
				  return picset(NAME); }
}

<integer_count>{
  {SPC}/{INTEGER}
  {INTEGERZ}          { yy_pop_state();
                        return numstr_of(yytext); }
}

<copy_state>{
  BY            { return BY; }
  IN|OF         { return IN; }
  SUPPRESS      { return SUPPRESS; }
  REPLACING     { return REPLACING; }
  COPY          { return COPY; }
  {DOTSEP}[[:blank:].]+$	{ pop_return *yytext; }
  {DOTSEP}      		{ pop_return *yytext; }

  [(][^().]*[)]	{ ydflval.string = xstrdup(yytext);
		  return SUBSCRIPT;
		}
  [(][^().]*/[(] {ydflval.string = xstrdup(yytext);
		  return LSUB;
		}
  [^().]*[)]	{ ydflval.string = xstrdup(yytext);
		  return RSUB;
		}

  {NAME}	{
		  ydflval.string = xstrdup(yytext);
		  return NAME;
		}

  /* CDF REPLACING needs quotes to distinquish strings from identifiers. */
  Z?['']{STRING1}['']   { auto *s = xstrdup(yytext);
			  std::replace(s, s + strlen(s), '\'', '"');
			  ydflval.string = s;
			  update_location_col(s);
			  return LITERAL; }
  Z?[""]{STRING}[""]    { ydflval.string = xstrdup(yytext);
			  update_location_col(yytext);
			  return LITERAL; }
  [=]{4}                { static char nullstring[] = "";
                          ydflval.string = nullstring; return PSEUDOTEXT; }
  [=]{2}                { yy_push_state(quoteq); }
}

<quoteq>{
  [^=]+[=]/[^=]       { tmpstring_append(yyleng); }
  [^=]+/[=]{2}        { yylval.string = xstrdup(tmpstring_append(yyleng));
                        ydflval.string = yylval.string;
			update_location_col(yylval.string);
                        return PSEUDOTEXT; }
  [=]{2}              { tmpstring = NULL; yy_pop_state(); }
}

<quoted2>{
  {STRING}$           { tmpstring_append(yyleng); }
  ^-[ ]{4,}[""]/.+    /* ignore continuation mark */
  {STRING}?[""]{2}    { tmpstring_append(yyleng - 1); }
  {STRING}            { tmpstring_append(yyleng); }
  [""]{SPC}[&]{SPC}[""''] {
		        if( yytext[yyleng - 1] == '\'' ) BEGIN(quoted1);
		      }
  [""]-{OSPC}({EOL}{OSPC})+[""] /* continue ...  */
  [""]                {
      			char *s = xstrdup(tmpstring? tmpstring : "\0");
                        yylval.literal.set_data(strlen(s), s);
                        ydflval.string = yylval.literal.data;
			update_location_col(yylval.literal.data, -2);
                        tmpstring = NULL; pop_return LITERAL; }
}

<quoted1>{
  {STRING1}$          { tmpstring_append(yyleng); }
  ^-[ ]{4,}['']/.+    /* ignore continuation mark */
  {STRING1}?['']{2}   { tmpstring_append(yyleng - 1); }
  {STRING1}           { tmpstring_append(yyleng); }
  ['']{SPC}[&]{SPC}[""''] {
		        if( yytext[yyleng - 1] == '"' ) BEGIN(quoted2);
		      }
  ['']-{OSPC}({EOL}{OSPC})+[''] /* continue ...  */
  ['']                {
      			char *s = xstrdup(tmpstring? tmpstring : "\0");
                        yylval.literal.set_data(strlen(s), s);
                        ydflval.string = yylval.literal.data;
			update_location_col(yylval.literal.data, -2);
                        tmpstring = NULL; pop_return LITERAL; }
}

<*>{
  AS		{ return AS; }
  CONSTANT	{ return CONSTANT; }
  (IS{SPC})?DEFINED	{ ydflval.boolean = true;  return DEFINED; }
  {ISNT}{SPC}DEFINED	{ ydflval.boolean = false; return DEFINED; }
  OFF		{ return OFF; }
}

<cdf_state>{
  [+-]?{INTEGERZ}       { int value;
			  if( is_integer_token(&value) ) {
			    ydflval.number = value;
			    return YDF_NUMBER;
			  }
			  dbgmsg("%s not an integer = %d",
				   yytext, value);
			  return NO_CONDITION;
			}

  {NAME}{SPC}AS 	{ char *s = xstrdup(yytext);
			  char *p = strchr(s, 0x20);
			  gcc_assert(p); // just found via regex
			  *p = '\0';
			  ydflval.string = yylval.string = s;
			  return NAME;
			}
  {NAME}	 	{ ydflval.string = yylval.string = xstrdup(yytext);
			  return NAME;
			}
  %EBCDIC-MODE		{ ydflval.number = feature_internal_ebcdic_e;
			  return FEATURE; }
  %64-BIT-POINTER	{ ydflval.number = feature_embiggen_e;
			  return FEATURE; }
  [[:blank:]]+
  {BLANK_EOL}
  . 			{ myless(0); yy_pop_state(); } // not a CDF token
}

<program_id_state>{
  {BLANK_OEOL}
  (IS)?[[:space:]]
  AS/{SPC} 	{ myless(0); yy_pop_state(); } /* => ident_state */

  INITIAL	{ return INITIAL_kw; }
  COMMON	{ return COMMON; }
  RECURSIVE	{ return RECURSIVE; }
  PROGRAM 	{ return PROGRAM_kw; }

  {DOTSEP}	{ pop_return '.'; }
}

<name_state>{           /* Either pop from here, or let the quoted state pop */
  {BLANK_OEOL}

  {NAME}	        { yy_pop_state();
			  yylval.string = xstrdup(yytext);
			  return NAME; 
			}
  Z?['']                { yylval.literal.set_prefix(yytext, yyleng-1);
			  BEGIN(quoted1); }
  Z?[""]                { yylval.literal.set_prefix(yytext, yyleng-1);
			  BEGIN(quoted2); }

  .             	{ myless(0); yy_pop_state();
			  /* Should not happen for valid inputs. */ }
}
<dot_state>{
  [[:blank:]]*[.][[:blank:].]+{EOL} 	{ pop_return '.'; }
  [[:blank:]]*[.]+			{ pop_return '.'; }
}

<date_state>{
  ^[[:blank:]]+
  {BLANK_EOL}

  DATE                { pop_return DATE; }
  DAY                 { pop_return DAY; }
  DATE/[[:blank:]]+Y  {                 return DATE; }
  DAY/[[:blank:]]+Y   {                 return DAY; }
  TIME                { pop_return TIME; }

  YYYYMMDD            { yy_pop_state();
                        yylval.string = xstrdup(yytext); return YYYYMMDD; }
  YYYYDDD             { yy_pop_state();
                        yylval.string = xstrdup(yytext); return YYYYDDD; }
  DAY-OF-WEEK         { yy_pop_state();
                        yylval.string = xstrdup(yytext); return DAY_OF_WEEK; }
}

<INITIAL,procedure_div,copy_state>{
  NOT{SPC}B/{boolseq}	{ is_not = true;  yy_push_state(bool_state); }
    B/{boolseq}			{ is_not = false; yy_push_state(bool_state); }
  N?X/{hexseq}			{ yylval.literal.set_prefix(yytext, yyleng);
	    			  yy_push_state(hex_state); }
  N?X{nonseq}		{ dbgmsg("invalid hexadecimal value: %s", yytext);
			  return NO_CONDITION; }

  BX/{hexseq}			{ yylval.numstr.radix = hexadecimal_e;
			  	  yy_push_state(numstr_state); }

  Z?['']                   { yylval.literal.set_prefix(yytext, yyleng-1);
			     yy_push_state(quoted1); }
  Z?[""]                   { yylval.literal.set_prefix(yytext, yyleng-1);
			     yy_push_state(quoted2); }
  Z?[""]/{STRING}[""]      { yylval.literal.set_prefix(yytext, yyleng-1);
			     yy_push_state(quoted2); }

  {INTEGERZ}/[[:punct:]][[:space:]]{BLANK_OEOL} { return numstr_of(yytext); }
  {dfc}/[[:blank:][:punct:]]         		{ return numstr_of(yytext); }

  [+-]?({dfc}|{dseq})([.,][[:digit:]])* { auto eotext = yytext + yyleng - 1;
				       if( *eotext == '.' ) {
				         myless(yyleng - 1);
				         *eotext = '\0';
				       }
				       return numstr_of(yytext); }

  UPSI-[0-7]                 { char *p = yytext + yyleng - 1;
                               ydflval.string = yylval.string = xstrdup(p);
                               return UPSI; }
}

    /*
     * "The decimal point can appear anywhere within the literal except as the
     * rightmost character."
     */
<numeric_state>{
  [[:blank:]]+
  {BLANK_EOL}

  [+-]?{INTEGERZ} {            pop_return numstr_of(yytext); }
  [+-]?{dfc}([.][[:digit:]])*  {
                               char *s = xstrdup(yytext);
                               char *p = strchr(s, '.');
                               if( p && strlen(p) == 1 ) {
                                 *p = '\0';
                                 myless(p - s);
                               }
                               numstr_of(s); free(s);
                               pop_return NUMSTR;
			     }
}

<cdf_state,procedure_div>{
  (IS{SPC})?"<"         { return '<'; }
  (IS{SPC})?"<="        { return LE;  }
  (IS{SPC})?"="         { return '='; }
  (IS{SPC})?"<>"        { return NE;  }
  (IS{SPC})?">="        { return GE;  }
  (IS{SPC})?">"         { return '>'; }

  {LESS_THAN} 				{ return '<'; }
  {LESS_THAN}{SPC}{OR_EQUAL}/[[:space:]]	{ return LE; }
  (IS{SPC})?EQUALS?({SPC}TO)?/[[:space:]]		{ return '='; }
  {GREATER_THAN}{SPC}{OR_EQUAL}/[[:space:]]	{ return GE; }
  {GREATER_THAN}			{ return '>'; }

  {ISNT}{OSPC}">="        { verify_ws(yytext[yyleng - 3]); return '<'; }
  {ISNT}{OSPC}">"         { verify_ws(yytext[yyleng - 2]); return LE;  }
  {ISNT}{OSPC}"="         { verify_ws(yytext[yyleng - 2]); return NE;  }
  {ISNT}{OSPC}"<"         { verify_ws(yytext[yyleng - 2]); return GE;  }
  {ISNT}{OSPC}"<="        { verify_ws(yytext[yyleng - 3]); return '>'; }

  {ISNT}{SPC}GREATER{SPC}(THAN)?{SPC}{OR_EQUAL}/[[:space:]] { return '<'; }
  {ISNT}{SPC}GREATER{SPC}(THAN)?	{ return LE; }
  {ISNT}{SPC}EQUALS?{SPC}(TO)?		{ return NE; }
  {ISNT}{SPC}LESS{SPC}(THAN)?		{ return GE; }
  {ISNT}{SPC}LESS{SPC}(THAN)?{SPC}{OR_EQUAL}/[[:space:]] { return '>'; }

  [*]{2}	{ return POW; }

  /* 
   * "A boolean operator specifies the type of boolean operation to be performed
   *  on one or two operands, for a unary operator or binary operator,
   *  respectively."
   * Binary boolean operators
   *  B-AND B-OR B-XOR
   * Unary boolean operator
   *  B-NOT
   * Boolean shift operators
   *  B-SHIFT-L B-SHIFT-LC B-SHIFT-R B-SHIFT-RC
   */

B-AND
B-OR
B-XOR
B-NOT
B-SHIFT-L
B-SHIFT-LC
B-SHIFT-R
B-SHIFT-RC

}

<procedure_div>{
  (ID|IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE){SPC}DIVISION  {
			          myless(0); BEGIN(INITIAL); }
  PROGRAM-ID{OSPC}{DOTSEP}	{ myless(0); BEGIN(INITIAL); }

  EXIT{SPC}/(PROGRAM|SECTION|PARAGRAPH|PERFORM) {
			  return EXIT; }
  EXIT{OSPC}/{DOTSEP}	{ return SIMPLE_EXIT; }
  EXIT 			{ return EXIT; } // (PROGRAM|SECTION|PARAGRAPH|PERFORM)
  RETURNING             { return RETURNING; }

  ACTIVATING            { return ACTIVATING; }
  CURRENT               { return CURRENT; }
  NESTED                { return NESTED; }
  STACK                 { return STACK; }
  TOP-LEVEL             { return TOP_LEVEL; }

  {NAME}/{SPC}SECTION{OSPC}{DOTSEP} {
				    yylval.string = xstrdup(yytext);
				    return NAME; }

  (IS{SPC})?POSITIVE/[[:space:]]  { yylval.number =  IS; return POSITIVE; }
  (IS{SPC})?NEGATIVE/[[:space:]]  { yylval.number =  IS; return NEGATIVE; }
  (IS{SPC})?ZERO/[[:space:]]      { yylval.number =  IS; return ZERO; }

  {ISNT}{SPC}POSITIVE/[[:space:]] { yylval.number = NOT; return POSITIVE; }
  {ISNT}{SPC}NEGATIVE/[[:space:]] { yylval.number = NOT; return NEGATIVE; }
  {ISNT}{SPC}ZERO/[[:space:]]     { yylval.number = NOT; return ZERO; }

  [(:)] 			{ return *yytext; }
  [(]/[^(:)""'']*[:][^)]*[)] 	{ return LPAREN; /* parentheses around a colon */ }

  FILLER			{ return FILLER_kw; }
  INVALID			{ yylval.number = INVALID; return INVALID; }
  NOT{SPC}INVALID	{ yylval.number = NOT;     return INVALID; }

  ON{SPC}SIZE		{ return SIZE; }

 (ON{SPC})?EXCEPTION     { yylval.number = EXCEPTION; return EXCEPTION; }
  NOT{SPC}(ON{SPC})?EXCEPTION {
                                  yylval.number = NOT;       return EXCEPTION; }

 (ON{SPC})?OVERFLOW      { yylval.number = OVERFLOW_kw; return OVERFLOW_kw; }
  NOT{SPC}(ON{SPC})?OVERFLOW {
                                  yylval.number = NOT;      return OVERFLOW_kw; }

 (AT{SPC})?END/[[:space:]]                { yylval.number = END;
                                                   return END; }
  NOT{SPC}(AT{SPC})?END/[[:space:]] { yylval.number = NOT;
                                                   return END; }

 (AT{SPC})?{EOP}/[[:space:]]                { yylval.number = EOP;
                                                   return EOP; }
  NOT{SPC}(AT{SPC})?{EOP}/[[:space:]] { yylval.number = NOT;
                                                   return EOP; }

  {SIZE_ERROR}     		{ yylval.number = ERROR; return SIZE_ERROR; }
  NOT{SPC}{SIZE_ERROR} 		{ yylval.number = NOT;   return SIZE_ERROR; }

  STRING                        { return STRING_kw; }
  UNSTRING                      { return UNSTRING; }
  POINTER                       { return POINTER; }
  REFERENCE                     { return REFERENCE; }
  COMMAND-LINE                  { return COMMAND_LINE; }
  COMMAND-LINE-COUNT            { return COMMAND_LINE_COUNT; }
  CONTENT                       { return CONTENT; }
  DELIMITED                     { return DELIMITED; }
  DELIMITER                     { return DELIMITER; }
  ENVIRONMENT                   { return ENVIRONMENT; }

			/* After name state, pop out of procedure_div state. */
  END{SPC}PROGRAM        { yy_push_state(name_state);
                                  return program_level() > 1?
                                         END_SUBPROGRAM : END_PROGRAM; }

  END{SPC}FUNCTION        { yy_push_state(name_state);
                                  return program_level() > 1?
                                         END_SUBPROGRAM /*invalid*/ :
                                         END_FUNCTION; }

  {ISNT}{SPC}{VARTYPE}   { yylval.number = NOT;
                                  yy_push_state(classify);
                                  myless(0);
                                  return MIGHT_BE;
                                }
  IS{SPC}{VARTYPE}       { yylval.number = IS;
                                  yy_push_state(classify);
                                  myless(0);
                                  return MIGHT_BE;
                                }

  {SORT_MERGE}{SPC}(\f#)?/{NAME}  { yy_push_state(sort_state); return SORT; }

  ADDRESS{SPC}(OF{SPC})?/FUNCTION { yy_push_state(addr_of); return ADDRESS; }

  FUNCTION 			{ yy_push_state(function); return FUNCTION; }

  SECTION{OSPC}[.]+{SPC}/USE[[:space:]]	{ yylval.string = NULL; return SECTION; }

  [.]+({SPC}(EJECT|SKIP[123]))*{SPC}EXIT{OSPC}/{DOTSEP} {
                   // EXIT format-1 is a "continue" statement
                 }
  {NAME}/{OSPC}{DOTSEP} {
		   assert(YY_START == procedure_div);
		   int token;
		   if( 0 != (token = binary_integer_usage(yytext)) ) return token;
                   if( 0 != (token = keyword_tok(yytext)) ) return token;
                   if( is_integer_token() ) return numstr_of(yytext);

                   ydflval.string = yylval.string = xstrdup(yytext);
                   return typed_name(yytext);
                 }
  LENGTH{SPC}OF/{SPC}{NAME}	 { return LENGTH_OF; }
  {NAME}/{SPC}(IN|OF){SPC}{NAME}{SPC}(IN|OF)[[:space:]] {
                    int token = keyword_tok(yytext);
                    if( token ) return token;
                    if( is_integer_token() ) return numstr_of(yytext);
		    myless(0);
	            yy_push_state(partial_name);
		    tee_up_empty();
		}
  {NAME}/{SPC}(IN|OF){SPC}{NAME} {
                    int token = keyword_tok(yytext);
                    if( token ) return token;
                    if( is_integer_token() ) return numstr_of(yytext);
		    // if the 2nd name is a filename, return NAME for normal processing
		    // skip {SPC}(IN|OF){SPC}
		    char *p = yytext + yyleng + 1;
		    while( ISSPACE(*p) ) p++;
		    assert(TOUPPER(p[0]) == 'I' || TOUPPER(p[0]) == 'O' );
		    assert(TOUPPER(p[1]) == 'N' || TOUPPER(p[1]) == 'F' );
		    p += 2;
		    while( ISSPACE(*p) ) p++;
		    cbl_name_t name2;
		    std::transform( p, p + sizeof(name2), name2,
				    []( char ch ) {
				      switch(ch) {
				      case '-':
				      case '_': return ch;
				      default:
					if( ISALNUM(ch) ) return ch;
				      }
				      return '\0';
				    } );
		    symbol_elem_t *e = symbol_file(PROGRAM, name2);
		    /*
		     * For NAME IN FILENAME, we want the parser to handle it.
		     * For NAME IN NAME (of filename), the scanner handles it.
		     */
		    if( e ) { // e is an FD, but name2 could be its 01
		      cbl_namelist_t names = {name2, yytext};
		      auto p = symbol_find(PROGRAM, names);
		      if( !p.second ) {
		        ydflval.string = yylval.string = xstrdup(yytext);;
		        return NAME;
		      }
		    }
		    myless(0);
	            yy_push_state(partial_name);
		    tee_up_empty();
		}
}

<partial_name>{
  {NAME}/{SPC}(IN|OF)[[:space:]] {
    tee_up_name(yylloc, xstrdup(yytext));
  }
  {SPC}(IN|OF){SPC}
  {NAME} { yy_pop_state();
    auto name = xstrdup(yytext);
    auto names = teed_up_names();
    names.push_front(name);
    auto found = symbol_find( PROGRAM, names);

    ydflval.string = yylval.string = name;
    if( found.first && found.second ) { // unique
      symbol_elem_t *e = found.first;
      if( e->type == SymField ) {
	auto f( cbl_field_of(e) );
	if( f->level == 88 ) return NAME88;
      }
    }
    return NAME;
  }
  {NAME}{OSPC}/[(] { BEGIN(subscripts);
    auto name = xstrdup(yytext);
    char *eoname = name + strlen(name);
    auto p = std::find_if(name, eoname, fisspace); // stop at blank, if any
    if( p < eoname )  *p = '\0';

    auto names = teed_up_names();
    names.push_front(name);
    auto found = symbol_find( PROGRAM, names);

    ydflval.string = yylval.string = name;
    if( found.first && found.second ) { // unique
      symbol_elem_t *e = found.first;
      if( e->type == SymField ) {
	auto f( cbl_field_of(e) );
	if( f->level == 88 ) return NAME88;
      }
    }
    return NAME;
  }
}

<addr_of>FUNCTION { pop_return FUNCTION; }

<classify>{
  {ISNT}/{SPC}{NAMTYP}  { yy_pop_state(); }
      IS/{SPC}{NAMTYP}  { yy_pop_state(); }
}

<sort_state>{
  {NAME} 	{ yylval.string = xstrdup(yytext);
		  pop_return symbol_file(PROGRAM, yytext)? FILENAME : NAME;
		}
}

<datetime_fmt>{
  [(] 				{ return *yytext; }

  ['']{DATETIME_FMT}['']	|
  [""]{DATETIME_FMT}[""]	{ yylval.string = xstrdup(yytext + 1);
				  yylval.string[yyleng-2] = '\0';
				  pop_return DATETIME_FMT; }

  ['']{DATE_FMT}['']		|
  [""]{DATE_FMT}[""]		{ yylval.string = xstrdup(yytext + 1);
				  yylval.string[yyleng-2] = '\0';
				  pop_return DATE_FMT; }

  ['']{TIME_FMT}['']		|
  [""]{TIME_FMT}[""]		{ yylval.string = xstrdup(yytext + 1);
				  yylval.string[yyleng-2] = '\0';
				  pop_return TIME_FMT; }

  {SPC}				// ignore
  {NAME}			{
				  int token = NAME;
				  char type = 0;
				  auto elem = symbol_field(PROGRAM, 0, yytext);

				  if( elem->type == SymField ) {
				    auto f = cbl_field_of(elem);
				    if( f->type == FldLiteralA && f->has_attr(constant_e) ) {
				      type = date_time_fmt(f->data.initial);
				      yylval.string = xstrdup(f->data.initial);
				    }
				  } else {
				    yylval.string = xstrdup(yytext);
				  }
				  switch(type) {
				  case 'D': token = DATETIME_FMT; break;
				  case 'd': token = DATE_FMT; break;
				  case 't': token = TIME_FMT; break;
				  default:
				    dbgmsg("format must be literal");
				    pop_return token;
				    break;
				  }
				  pop_return token;
				}

  . 				{ myless(0); yy_pop_state(); }
}

<function>{
  ABS/{NONWORD}				{ pop_return ABS; }
  ACOS/{NONWORD}			{ pop_return ACOS; }
  ANNUITY/{NONWORD}			{ pop_return ANNUITY; }
  ASIN/{NONWORD}			{ pop_return ASIN; }
  ATAN/{NONWORD}			{ pop_return ATAN; }
  BASECONVERT/{NONWORD}			{ pop_return BASECONVERT; }
  BIT-OF/{NONWORD}			{ pop_return BIT_OF; }
  BIT-TO-CHAR/{NONWORD}			{ pop_return BIT_TO_CHAR; }
  BOOLEAN-OF-INTEGER/{NONWORD}		{ pop_return BOOLEAN_OF_INTEGER; }
  BYTE-LENGTH/{NONWORD}			{ pop_return BYTE_LENGTH; }
  CHAR-NATIONAL/{NONWORD}		{ pop_return CHAR_NATIONAL; }
  CHAR/{NONWORD}			{ pop_return CHAR; }
  COMBINED-DATETIME/{NONWORD}		{ pop_return COMBINED_DATETIME; }
  CONCAT/{NONWORD}			{ pop_return CONCAT; }
  CONTENT-LENGTH/{NONWORD}		{ pop_return NO_CONDITION; /* GNU only*/ }
  CONTENT-OF/{NONWORD}			{ pop_return NO_CONDITION; /* GNU only*/ }
  CONVERT/{NONWORD}			{ pop_return CONVERT; }
  COS/{NONWORD}				{ pop_return COS; }
  CURRENCY-SYBOL/{NONWORD}		{ pop_return NO_CONDITION; /* GNU only*/ }
  CURRENT-DATE/{NONWORD}		{ pop_return CURRENT_DATE; }
  DATE-OF-INTEGER/{NONWORD}		{ pop_return DATE_OF_INTEGER; }
  DATE-TO-YYYYMMDD/{NONWORD}		{ pop_return DATE_TO_YYYYMMDD; }
  DAY-OF-INTEGER/{NONWORD}		{ pop_return DAY_OF_INTEGER; }
  DAY-TO-YYYYDDD/{NONWORD}		{ pop_return DAY_TO_YYYYDDD; }
  DISPLAY-OF/{NONWORD}			{ pop_return DISPLAY_OF; }
  E/{NONWORD}				{ pop_return E; }

  EXCEPTION-FILE-N/{NONWORD}		{ pop_return EXCEPTION_FILE_N; }
  EXCEPTION-FILE/{NONWORD}		{ pop_return EXCEPTION_FILE; }
  EXCEPTION-LOCATION-N/{NONWORD}	{ pop_return EXCEPTION_LOCATION_N; }
  EXCEPTION-LOCATION/{NONWORD}		{ pop_return EXCEPTION_LOCATION; }
  EXCEPTION-STATEMENT/{NONWORD}		{ pop_return EXCEPTION_STATEMENT; }
  EXCEPTION-STATUS/{NONWORD}		{ pop_return EXCEPTION_STATUS; }

  EXP/{NONWORD}				{ pop_return EXP; }
  EXP10/{NONWORD}			{ pop_return EXP10; }
  FACTORIAL/{NONWORD}			{ pop_return FACTORIAL; }
  FIND-STRING/{NONWORD}			{ pop_return FIND_STRING; }

  FORMATTED-CURRENT-DATE/{NONWORD}	{ BEGIN(datetime_fmt);
					  return FORMATTED_CURRENT_DATE; }
  FORMATTED-DATE/{NONWORD}		{ BEGIN(datetime_fmt); return FORMATTED_DATE; }
  FORMATTED-DATETIME/{NONWORD}		{ BEGIN(datetime_fmt); return FORMATTED_DATETIME; }
  FORMATTED-TIME/{NONWORD}		{ BEGIN(datetime_fmt); return FORMATTED_TIME; }
  FRACTION-PART/{NONWORD}		{ pop_return FRACTION_PART; }

  HEX-OF/{NONWORD}			{ pop_return HEX_OF; }
  HEX-TO-CHAR/{NONWORD}			{ pop_return HEX_TO_CHAR; }
  HIGHEST-ALGEBRAIC/{NONWORD}		{ pop_return HIGHEST_ALGEBRAIC; }

  INTEGER/{NONWORD}			{ pop_return INTEGER; }
  INTEGER-OF-BOOLEAN/{NONWORD}		{ pop_return INTEGER_OF_BOOLEAN; }
  INTEGER-OF-DATE/{NONWORD}		{ pop_return INTEGER_OF_DATE; }
  INTEGER-OF-DAY/{NONWORD}		{ pop_return INTEGER_OF_DAY; }
  INTEGER-OF-FORMATTED-DATE/{NONWORD}	{ BEGIN(datetime_fmt);
					  return INTEGER_OF_FORMATTED_DATE; }
  INTEGER-PART/{NONWORD}		{ pop_return INTEGER_PART; }
  LENGTH/{NONWORD}			{ pop_return LENGTH; }
  LOCALE-COMPARE/{NONWORD}		{ pop_return LOCALE_COMPARE; }
  LOCALE-DATE/{NONWORD}			{ pop_return LOCALE_DATE; }
  LOCALE-TIME/{NONWORD}			{ pop_return LOCALE_TIME; }
  LOCALE-TIME-FROM-SECONDS/{NONWORD}	{ pop_return LOCALE_TIME_FROM_SECONDS; }
  LOG/{NONWORD}				{ pop_return LOG; }
  LOG10/{NONWORD}			{ pop_return LOG10; }
  LOWER-CASE/{NONWORD}			{ pop_return LOWER_CASE; }
  LOWEST-ALGEBRAIC/{NONWORD}		{ pop_return LOWEST_ALGEBRAIC; }
  MAX/{NONWORD}				{ pop_return MAXX; }
  MEAN/{NONWORD}			{ pop_return MEAN; }
  MEDIAN/{NONWORD}			{ pop_return MEDIAN; }
  MIDRANGE/{NONWORD}			{ pop_return MIDRANGE; }
  MIN/{NONWORD}				{ pop_return MINN; }
  MOD/{NONWORD}				{ pop_return MOD; }
  MODULE-NAME/{NONWORD}			{ pop_return MODULE_NAME; }
  NATIONAL-OF/{NONWORD}			{ pop_return NATIONAL_OF; }
  NUMVAL/{NONWORD}			{ pop_return NUMVAL; }
  NUMVAL-C/{NONWORD}			{ pop_return NUMVAL_C; }
  NUMVAL-F/{NONWORD}			{ pop_return NUMVAL_F; }
  ORD/{NONWORD}				{ pop_return ORD; }
  ORD-MAX/{NONWORD}			{ pop_return ORD_MAX; }
  ORD-MIN/{NONWORD}			{ pop_return ORD_MIN; }
  PI/{NONWORD}				{ pop_return PI; }
  PRESENT-VALUE/{NONWORD}		{ pop_return PRESENT_VALUE; }

  RANDOM{OSPC}{PARENS}			{ pop_return RANDOM; }
  RANDOM{OSPC}[(]		 	{ pop_return RANDOM_SEED; }
  RANDOM			 	{ pop_return RANDOM; }

  RANGE/{NONWORD}			{ pop_return RANGE; }
  REM/{NONWORD}				{ pop_return REM; }
  REVERSE/{NONWORD}			{ pop_return REVERSE; }
  SECONDS-FROM-FORMATTED-TIME/{NONWORD} { BEGIN(datetime_fmt);
					   return SECONDS_FROM_FORMATTED_TIME; }
  SECONDS-PAST-MIDNIGHT/{NONWORD}	{ pop_return SECONDS_PAST_MIDNIGHT; }
  SIGN/{NONWORD}			{ pop_return SIGN; }
  SIN/{NONWORD}				{ pop_return SIN; }
  SMALLEST-ALGEBRAIC/{NONWORD}		{ pop_return SMALLEST_ALGEBRAIC; }
  SQRT/{NONWORD}			{ pop_return SQRT; }
  STANDARD-COMPARE/{NONWORD}		{ pop_return STANDARD_COMPARE; }
  STANDARD-DEVIATION/{NONWORD}		{ pop_return STANDARD_DEVIATION; }
  SUBSTITUTE/{NONWORD}			{ pop_return SUBSTITUTE; }
  SUM/{NONWORD}				{ pop_return SUM; }
  TAN/{NONWORD}				{ pop_return TAN; }
  TEST-DATE-YYYYMMDD/{NONWORD}		{ pop_return TEST_DATE_YYYYMMDD; }
  TEST-DAY-YYYYDDD/{NONWORD}		{ pop_return TEST_DAY_YYYYDDD; }
  TEST-FORMATTED-DATETIME/{NONWORD}	{ BEGIN(datetime_fmt); return TEST_FORMATTED_DATETIME; }
  TEST-NUMVAL/{NONWORD}			{ pop_return TEST_NUMVAL; }
  TEST-NUMVAL-C/{NONWORD}		{ pop_return TEST_NUMVAL_C; }
  TEST-NUMVAL-F/{NONWORD}		{ pop_return TEST_NUMVAL_F; }
  TRIM/{NONWORD}			{ pop_return TRIM; }
  ULENGTH/{NONWORD}			{ pop_return ULENGTH; }
  UPOS/{NONWORD}			{ pop_return UPOS; }
  UPPER-CASE/{NONWORD}			{ pop_return UPPER_CASE; }
  USUBSTR/{NONWORD}			{ pop_return USUBSTR; }
  USUPPLEMENTARY/{NONWORD}		{ pop_return USUPPLEMENTARY; }
  UUID4/{NONWORD}			{ pop_return UUID4; }
  UVALID/{NONWORD}			{ pop_return UVALID; }
  UWIDTH/{NONWORD}			{ pop_return UWIDTH; }
  VARIANCE/{NONWORD}			{ pop_return VARIANCE; }
  WHEN-COMPILED/{NONWORD}		{ pop_return WHEN_COMPILED; }
  YEAR-TO-YYYY/{NONWORD}		{ pop_return YEAR_TO_YYYY; }

  /* Matches above include NONWORD because the NAME tests below are otherwise longer, */

  {NAME}{OSPC}/[(] { /* If /{OSPC}, "dangerous trailing context" "*/
                  auto name = null_trim(xstrdup(yytext));
  		  if( 0 != (yylval.number = symbol_function_token(name)) ) {
  		    pop_return FUNCTION_UDF;
  		  }
		  yylval.string = name;
		  pop_return NAME;
		}

  {NAME}({OSPC}{PARENS})? {
                  auto name = null_trim(xstrdup(yytext));
		  auto p = strchr(name, '(');
		  if( p ) *p = '\0';
		  if( 0 != (yylval.number = symbol_function_token(name)) ) {
  		    pop_return FUNCTION_UDF_0;
  		  }
		  yylval.string = name;
		  pop_return NAME;
	 	}
}

		/*
		 * CDF: Compiler-directing Facility
		 */

[*]CBL		{ return STAR_CBL; }
[*]CONTROL	{ return STAR_CBL; }

^[ ]*[*](PROCESS\b|CBL\b).*$ {
		  auto p = std::find(yytext, yytext + yyleng, '*');
		  not_implemented("CDF '%s' was ignored", p);
		}
^[ ]*[@]OPTIONS.+$ {
		  auto p = std::find(yytext, yytext + yyleng, '@');
		  not_implemented("CDF '%s' was ignored", p);
		}

BASIS		{ yy_push_state(basis); return BASIS; }

<basis>{
  [[:blank:]]+
  {BLANK_EOL}

  {STRING}      { yy_pop_state();
                  yypush_buffer_state( yy_create_buffer(yyin, YY_BUF_SIZE) );
                  if( (yyin = cdftext::lex_open(yytext)) == NULL ) {
                    yywarn("could not open BASIS file '%s'", yytext);
                    yyterminate();
                  }
                }
}

<subscripts>{
  [(]		{ pop_return LPAREN; }
}

<procedure_div>{
   EQUALS?{OSPC}/[(] { return '='; }

   {NAME}{OSPC}/[(] { /* If /{OSPC}, "dangerous trailing context" "*/
                  if( is_integer_token() ) return numstr_of(yytext);
                  ydflval.string = yylval.string = xstrdup(yytext);

                  int token = keyword_tok(null_trim(yylval.string), true);

                  if( token && ! symbol_field(PROGRAM, 0, yylval.string) ) {
                    // If token is an intrinsic, and not in Repository, pretend
                    // it's a name and let the parser sort it out.
                    auto name = intrinsic_function_name(token);
                    if( ! name ) return token; // valid keyword, like IF
                    if( token == repository_function_tok(name) ) {
                      return token; // intrinsic and in repository
                    }
                    error_msg(yylloc, "'FUNCTION %s' required because %s "
			     "is not mentioned in REPOSITORY paragraph",
			     name, name);
                  }

		  if( 0 != (token = repository_function_tok(yylval.string)) ) {
		    auto e = symbol_function(0, yylval.string);
		    assert(e);
		    yylval.number = symbol_index(e);
		    return token;
		  }
                  token = typed_name(yylval.string);
                  switch(token) {
                  case NAME:
                  case NUME:
                  case NAME88:
                     yy_push_state(subscripts);
                  }
                  return token;
                }
  [.]+[[:blank:].]+  	{ return '.'; }
}

<exception>{
  CHECKING	{ return CHECKING; }
  ON		{ return ON; }
  OFF		{ return OFF; }
  WITH		{ return WITH; }
  LOCATION	{ return LOCATION; }

  {NAME} 	{
		  auto ec = ec_type_of(yytext);
		  if( ec != ec_none_e ) {
		    ydflval.number = ec;
		    return EXCEPTION_NAME;
		  }
		  ydflval.string = xstrdup(yytext);
		  return symbol_file(PROGRAM, yytext)? FILENAME : NAME;
		}
  [[:blank:]]+
  {EOL}		{ yy_pop_state(); }
}

<raising>{
  LAST({SPC}EXCEPTION)?		{ yy_pop_state(); return LAST; }
  .				{ yy_pop_state(); return RAISING; } // invalid syntax
}
                /*
                 * Catch-all
                 */


<*>{
  ^[ ]{6}D.*\n 	{
		  if( !is_fixed_format() ) {
		    myless(6);
		  } else {
		    // If WITH DEBUGGING MODE, drop the D, else drop the line.
		    if( include_debug() ) myless(7);
		  }
		}
  ^[ ]*>>{OBLANK}IF		{ yy_push_state(cdf_state); return CDF_IF; }
  ^[ ]*>>{OBLANK}ELSE	 	{ return CDF_ELSE; }
  ^[ ]*>>{OBLANK}END-IF	 	{ return CDF_END_IF; }

  ^[ ]*[$]{OBLANK}IF		{ if( ! dialect_mf() )  {
				    dialect_error(yylloc,  yytext, "mf");
				  }
				  yy_push_state(cdf_state); return CDF_IF; }
  ^[ ]*[$]{OBLANK}ELSE	 	{ if( ! dialect_mf() )  {
				    dialect_error(yylloc,  yytext, "mf");
				  }
				  return CDF_ELSE; }
  ^[ ]*[$]{OBLANK}END	 	{ if( ! dialect_mf() ) {
				    dialect_error(yylloc,  yytext, "mf");
				  }
				  return CDF_END_IF; }

  ^[ ]*[$]{OBLANK}SET({SPC}CONSTANT)? {
				  if( ! dialect_mf() ) dialect_error(yylloc, yytext, "mf");
				  yy_push_state(cdf_state); return CDF_DEFINE; }

  ^[ ]*>>{OBLANK}EVALUATE		{ return CDF_EVALUATE; }
  ^[ ]*>>{OBLANK}WHEN		{ return CDF_WHEN; }
  ^[ ]*>>{OBLANK}END-EVALUATE	{ return CDF_END_EVALUATE; }

  ^[ ]*>>{OBLANK}CALL-CONVENTION{BLANK}C		{ return CALL_VERBATIM; }
  ^[ ]*>>{OBLANK}CALL-CONVENTION{BLANK}COBOL	{ return CALL_COBOL; }
  ^[ ]*>>{OBLANK}CALL-CONVENTION{BLANK}VERBATIM	{ return CALL_VERBATIM; }

  ^[ ]*>>{OBLANK}DEFINE	{ yy_push_state(cdf_state); return CDF_DEFINE; }
  ^[ ]*>>{OBLANK}DISPLAY 	{ return CDF_DISPLAY; }
  ^[ ]*>>{OBLANK}TURN 	{ yy_push_state(exception); return TURN; }
  ^[ ]*>>{OBLANK}COBOL-WORDS	{ yy_push_state(cobol_words); return COBOL_WORDS; }

  ^[ ]*>>{OBLANK}SOURCE{BLANK}FORMAT	{ return SOURCE_FORMAT; }

  ^[ ]*>>{OBLANK}PUSH 		{ return CDF_PUSH; }
  ^[ ]*>>{OBLANK}POP 		{ return CDF_POP; }

  ^[ ]*>>{OBLANK}{NAME} 	{
			  error_msg(yylloc, "unknown CDF token: %s", yytext);
			}


  OTHER		{ return OTHER; }
  OVERRIDE	{ return OVERRIDE; }
  PARAMETER	{ return PARAMETER_kw; }
  THRU		{ return THRU; }
  TRUE		{ return TRUE_kw; }

  ALL			{ return ALL; }
  CALL-CONVENTION	{ return CALL_CONVENTION; }
  COBOL-WORDS		{ return COBOL_WORDS; }
  DEFINE		{ return CDF_DEFINE; }
  SOURCE{BLANK}FORMAT	{ return SOURCE_FORMAT; }

}

<cobol_words>{
  EQUATE	{ return EQUATE; }
  UNDEFINE	{ return UNDEFINE; }
  SUBSTITUTE	{ return SUBSTITUTE; }
  RESERVE	{ return RESERVE; }
  {NAME}	{
                  ydflval.string = yylval.string = xstrdup(yytext);
		  pop_return NAME;
		}
}

<*>{
  {PUSH_FILE}		{
			  yy_set_bol(true);
			  auto top_file = cobol_lineno(yylineno);
			  if( top_file ) {
			    if( yy_flex_debug ) dbgmsg("  saving line %4d of %s",
			  			       yylineno, top_file);
			  }
			  // "\f#file push <name>": name starts at offset 13.
			  char *filename = xstrdup(yytext);
			  filename[yyleng - 1] = '\0'; // kill the trailing formfeed
			  filename += 12;
			  if( yytext[0] != '\f' ) {
			    dbgmsg("logic warning: filename was adjusted to %s",
				   --filename);
			  }
			  input_file_status.enter(filename);
			  yylineno = 1;
			  reset_location();
  			}

  {POP_FILE}	 	{
			  yy_set_bol(true);
			  input_file_status.leave();
			  yylineno = cobol_lineno();
			}

  {LINE_DIRECTIVE} 	{
      			  yylineno = cobol_fileline_set(yytext);
			  reset_location();
			}
}


<*>OR 				{ return OR; }
<*>AND				{ return AND; }
<*>{DOTSEP}			{ return '.'; }
<*>[().=*/+&-]			{ return *yytext; }
<*>[[:blank:]]+
<*>{EOL}

<*>{
  {COMMA}
  ^{SKIP}
  ^{TITLE}
}

<*>{
  ACCEPT	{ return ACCEPT; }
  ACCESS	{ return ACCESS; }
  ADD		{ return ADD; }
  ADDRESS	{ return ADDRESS; }
  ADVANCING	{ return ADVANCING; }
  AFTER		{ return AFTER; }
  ALL		{ return ALL; }
  ALLOCATE	{ return ALLOCATE; }
  ALPHABET	{ return ALPHABET; }
  ALPHABETIC	{ return ALPHABETIC; }
  ALPHABETIC-LOWER	{ return ALPHABETIC_LOWER; }
  ALPHABETIC-UPPER	{ return ALPHABETIC_UPPER; }
  ALPHANUMERIC		{ return ALPHANUMERIC; }
  ALPHANUMERIC-EDITED	{ return ALPHANUMERIC_EDITED; }
  ALSO		{ return ALSO; }
  ALTERNATE	{ return ALTERNATE; }
  AND		{ return AND; }
  ANY		{ return ANY; }
  ANYCASE	{ return ANYCASE; }
  ARE		{ return ARE; }
  AREA		{ return AREA; }
  AREAS		{ return AREAS; }
  AS		{ return AS; }
  ASCENDING	{ return ASCENDING; }
  ASSIGN		{ return ASSIGN; }
  AT		{ return AT; }
  BASED		{ return BASED; }
  BEFORE	{ return BEFORE; }
  BINARY	{ return BINARY; }
  BIT		{ return BIT; }
  BLANK		{ return BLANK; }
  BLOCK		{ return BLOCK_kw; }
  BOTTOM	{ return BOTTOM; }
  BY		{ return BY; }
  CALL		{ return CALL; }
  CANCEL	{ return CANCEL; }
  CF		{ return CF; }
  CH		{ return CH; }
  CHARACTER	{ return CHARACTER; }
  CHARACTERS	{ return CHARACTERS; }
  CLASS		{ return CLASS; }
  CLOSE		{ return CLOSE; }
  CODE		{ return CODE; }
  COMMA		{ return COMMA; }
  COMMIT	{ return COMMIT; }
  COMMON	{ return COMMON; }
  CONDITION	{ return CONDITION; }
  CONSTANT	{ return CONSTANT; }
  CONTAINS	{ return CONTAINS; }
  CONTENT	{ return CONTENT; }
  CONTINUE	{ return CONTINUE; }
  CONTROL	{ return CONTROL; }
  CONTROLS	{ return CONTROLS; }
  CONVERTING	{ return CONVERTING; }
  COPY		{ return COPY; }
  COUNT		{ return COUNT; }
  CURRENCY	{ return CURRENCY; }
  DATA		{ return DATA; }
  DATE		{ return DATE; }
  DAY		{ return DAY; }
  DAY-OF-WEEK	{ return DAY_OF_WEEK; }
  DE		{ return DE; }
  DECIMAL-POINT	{ return DECIMAL_POINT; }
  DECLARATIVES	{ return DECLARATIVES; }
  DEFAULT	{ return DEFAULT; }
  DELETE	{ return DELETE; }
  DELIMITED	{ return DELIMITED; }
  DELIMITER	{ return DELIMITER; }
  DEPENDING	{ return DEPENDING; }
  DESCENDING	{ return DESCENDING; }
  DETAIL	{ return DETAIL; }
  DISPLAY	{ return DISPLAY; }
  DIVIDE	{ return DIVIDE; }
  DOWN		{ return DOWN; }
  DUPLICATES	{ return DUPLICATES; }
  DYNAMIC	{ return DYNAMIC; }
  EC		{ return EC; }
  ELSE		{ return ELSE; }
  END		{ return END; }
  END-ACCEPT	{ return END_ACCEPT; }
  END-ADD	{ return END_ADD; }
  END-CALL	{ return END_CALL; }
  END-DELETE	{ return END_DELETE; }
  END-DISPLAY	{ return END_DISPLAY; }
  END-DIVIDE	{ return END_DIVIDE; }
  END-EVALUATE	{ return END_EVALUATE; }
  END-IF	{ return END_IF; }
  END-MULTIPLY	{ return END_MULTIPLY; }
  END-PERFORM	{ return END_PERFORM; }
  END-READ	{ return END_READ; }
  END-RETURN	{ return END_RETURN; }
  END-REWRITE	{ return END_REWRITE; }
  END-SEARCH	{ return END_SEARCH; }
  END-SUBTRACT	{ return END_SUBTRACT; }
  END-WRITE	{ return END_WRITE; }
  ENVIRONMENT	{ return ENVIRONMENT; }
  EQUAL		{ return EQUAL; }
  ERROR		{ return ERROR; }
  EVALUATE	{ return EVALUATE; }
  EXCEPTION	{ return EXCEPTION; }
  EXIT		{ return EXIT; }
  EXTEND	{ return EXTEND; }
  EXTERNAL	{ return EXTERNAL; }

  FD		{ return FD; }
  FINAL		{ return FINAL; }
  FINALLY	{ return FINALLY; }
  FIRST		{ return FIRST; }
  FOOTING	{ return FOOTING; }
  FOR		{ return FOR; }
  FREE		{ return FREE; }
  FROM		{ return FROM; }
  FUNCTION	{ return FUNCTION; }
  GENERATE	{ return GENERATE; }
  GIVING	{ return GIVING; }
  GLOBAL	{ return GLOBAL; }
  GO		{ return GO; }
  GOBACK	{ return GOBACK; }
  GROUP		{ return GROUP; }
  HEADING	{ return HEADING; }
  IF		{ return IF; }
  IN		{ return IN; }
  INDEX		{ return INDEX; }
  INDEXED	{ return INDEXED; }
  INDICATE	{ return INDICATE; }
  INITIAL	{ return INITIAL; }
  INITIALIZE	{ return INITIALIZE; }
  INITIATE	{ return INITIATE; }
  INPUT		{ return INPUT; }
  INSPECT	{ return INSPECT; }
  INTERFACE	{ return INTERFACE; }
  INTO		{ return INTO; }
  INVOKE	{ return INVOKE; }
  IS		{ return IS; }
  KEY		{ return KEY; }
  LAST		{ return LAST; }
  LEADING	{ return LEADING; }
  LEFT		{ return LEFT; }
  LENGTH	{ return LENGTH; }
  LIMIT		{ return LIMIT; }
  LIMITS	{ return LIMITS; }
  LINAGE	{ return LINAGE; }
  LINE		{ return LINE; }
  LINE-COUNTER	{ return LINE_COUNTER; }
  LINES		{ return LINES; }
  LINKAGE	{ return LINKAGE; }
  LOCAL-STORAGE	{ return LOCAL_STORAGE; }
  LOCALE	{ return LOCALE; }
  LOCATION	{ return LOCATION; }
  LOCK		{ return LOCK; }
  MERGE		{ return MERGE; }
  MODE		{ return MODE; }
  MOVE		{ return MOVE; }
  MULTIPLY	{ return MULTIPLY; }
  NATIONAL	{ return NATIONAL; }
  NATIONAL-EDITED	{ return NATIONAL_EDITED; }
  NATIVE	{ return NATIVE; }
  NEGATIVE	{ return NEGATIVE; }
  NESTED	{ return NESTED; }
  NEXT		{ return NEXT; }
  NO		{ return NO; }
  NOT		{ return NOT; }
  NUMBER	{ return NUMBER; }
  NUMERIC	{ return NUMERIC; }
  NUMERIC-EDITED	{ return NUMERIC_EDITED; }
  OCCURS	{ return OCCURS; }
  OF		{ return OF; }
  OFF		{ return OFF; }
  OMITTED	{ return OMITTED; }
  ON		{ return ON; }
  OPEN		{ return OPEN; }
  OPTIONAL	{ return OPTIONAL; }
  OPTIONS	{ return OPTIONS; }
  OR		{ return OR; }
  ORDER		{ return ORDER; }
  ORGANI[SZ]ATION	{ return ORGANIZATION; }
  OTHER		{ return OTHER; }
  OUTPUT	{ return OUTPUT; }
  OVERFLOW	{ return OVERFLOW_kw; }
  OVERRIDE	{ return OVERRIDE; }
  PACKED-DECIMAL	{ return PACKED_DECIMAL; }
  PAGE		{ return PAGE; }
  PAGE-COUNTER	{ return PAGE_COUNTER; }
  PERFORM	{ return PERFORM; }
  PF		{ return PF; }
  PH		{ return PH; }
  PIC		{ return PIC; }
  PICTURE	{ return PICTURE; }
  PLUS		{ return PLUS; }
  POINTER	{ return POINTER; }
  POSITIVE	{ return POSITIVE; }
  PROCEDURE	{ return PROCEDURE; }
  PROGRAM	{ return PROGRAM_kw; }
  PROGRAM-ID	{ return PROGRAM_ID; }
  PROPERTY	{ return PROPERTY; }
  PROTOTYPE	{ return PROTOTYPE; }
  QUOTES	{ return QUOTES; }
  RAISE		{ return RAISE; }
  RAISING	{ return RAISING; }
  RANDOM	{ return RANDOM; }
  RD		{ return RD; }
  READ		{ return READ; }
  RECORD	{ return RECORD; }
  RECORDS	{ return RECORDS; }
  REDEFINES	{ return REDEFINES; }
  REEL		{ return REEL; }
  REFERENCE	{ return REFERENCE; }
  RELATIVE	{ return RELATIVE; }
  RELEASE	{ return RELEASE; }
  REMAINDER	{ return REMAINDER; }
  REMOVAL	{ return REMOVAL; }
  RENAMES	{ return RENAMES; }
  REPLACE	{ return REPLACE; }
  REPLACING	{ return REPLACING; }
  REPORT	{ return REPORT; }
  REPORTING	{ return REPORTING; }
  REPORTS	{ return REPORTS; }
  REPOSITORY	{ return REPOSITORY; }
  RESERVE	{ return RESERVE; }
  RESET		{ return RESET; }
  RESUME	{ return RESUME; }
  RETURN	{ return RETURN; }
  RETURNING	{ return RETURNING; }
  REWIND	{ return REWIND; }
  REWRITE	{ return REWRITE; }
  RF		{ return RF; }
  RH		{ return RH; }
  RIGHT		{ return RIGHT; }
  ROUNDED	{ return ROUNDED; }
  RUN		{ return RUN; }
  SAME		{ return SAME; }
  SCREEN	{ return SCREEN; }
  SD		{ return SD; }
  SEARCH	{ return SEARCH; }
  SECTION	{ yylval.string = NULL; return SECTION; }
  SELECT	{ return SELECT; }
  SENTENCE	{ return SENTENCE; }
  SEPARATE	{ return SEPARATE; }
  SEQUENCE	{ return SEQUENCE; }
  SEQUENTIAL	{ return SEQUENTIAL; }
  SET		{ return SET; }
  SHARING	{ return SHARING; }
  SIGN		{ return SIGN; }
  SIZE		{ return SIZE; }
  SORT		{ return SORT; }
  SORT-MERGE	{ return SORT_MERGE; }
  SOURCE	{ return SOURCE; }
  SPACE		{ return SPACE; }
  SPACES	{ return SPACES; }
  SPECIAL-NAMES	{ return SPECIAL_NAMES; }
  STANDARD	{ return STANDARD; }
  STANDARD-1	{ return STANDARD_1; }
  START		{ return START; }
  STATUS	{ return STATUS; }
  STOP		{ return STOP; }
  SUBTRACT	{ return SUBTRACT; }
  SUM		{ return SUM; }
  SUPPRESS	{ return SUPPRESS; }
  SYMBOLIC	{ return SYMBOLIC; }
  TALLYING	{ return TALLYING; }
  TERMINATE	{ return TERMINATE; }
  TEST		{ return TEST; }
  THAN		{ return THAN; }
  THEN		{ return THEN; }
  THRU		{ return THRU; }
  TIME		{ return TIME; }
  TIMES		{ return TIMES; }
  TO		{ return TO; }
  TOP		{ return TOP; }
  TRAILING	{ return TRAILING; }

  TYPE		{ return TYPE; }
  TYPEDEF	{ return TYPEDEF; }
  UNIT		{ return UNIT; }
  UNTIL		{ return UNTIL; }
  UP		{ return UP; }
  UPON		{ return UPON; }
  USAGE		{ return USAGE; }
  USE		{ return USE; }
  USING		{ return USING; }
  VALUE		{ return VALUE; }
  VARYING	{ return VARYING; }
  WHEN		{ return WHEN; }
  WITH		{ return WITH; }
  WORKING-STORAGE	{ return WORKING_STORAGE; }
  WRITE		{ return WRITE; }

  ZERO	|
  ZEROES	|
  ZEROS		{ return ZERO; }
}

<*>{
  %EBCDIC-MODE		{ ydflval.number = feature_internal_ebcdic_e;
			  return FEATURE; }
  %64-BIT-POINTER	{ ydflval.number = feature_embiggen_e;
			  return FEATURE; }
}

<*>{
   {NAME}       {
                  int token = keyword_tok(yytext);
                  if( token ) {
		    if(yy_flex_debug && YY_START) {
                       dbgmsg("missed token %s in start condition %d",
			      yytext, YY_START);
		    }
		    // Do not return "token" because it may have been excluded
		    // by a start condition.  For example, REM might be a name,
		    // but is the name of an intrinsic function, which would
		    // appear only after FUNCTION.
		  }
                  if( is_integer_token() ) return numstr_of(yytext);
                  ydflval.string = yylval.string = xstrdup(yytext);
                  return typed_name(yytext);
                }
}

<*>.		{
		  auto state = start_condition_is();
		  dbgmsg("scanner error: "
		         "%sstart condition %s (0x%02x): scanner default rule",
			 YY_AT_BOL()? "(bol) " : "", state, *yytext );
		  return NO_CONDITION;
		}

<quoted1,quoted2>{
  <<EOF>> 	{
		    error_msg(yylloc, "syntax error: unterminated string %<%s%>",
			     tmpstring);
		    return NO_CONDITION;
		}
}

%%

#pragma GCC diagnostic pop

#include "scan_post.h"
