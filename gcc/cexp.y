/* Parse C expressions for CCCP.
   Copyright (C) 1987, 92, 94-99, 2000 Free Software Foundation.

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
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!

 Adapted from expread.y of GDB by Paul Rubin, July 1986.  */

/* Parse a C expression from text in a string  */
   
%{
#include "config.h"

#include "system.h"
#include "intl.h"
#include <setjmp.h>
/* #define YYDEBUG 1 */

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif /* MULTIBYTE_CHARS */

typedef unsigned char U_CHAR;

/* This is used for communicating lists of keywords with cccp.c.  */
struct arglist {
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
};

HOST_WIDEST_INT parse_c_expression PARAMS ((char *, int));

static int yylex PARAMS ((void));
static void yyerror PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
static HOST_WIDEST_INT expression_value;
#ifdef TEST_EXP_READER
static int expression_signedp;
#endif

static jmp_buf parse_return_error;

/* Nonzero means count most punctuation as part of a name.  */
static int keyword_parsing = 0;

/* Nonzero means do not evaluate this expression.
   This is a count, since unevaluated expressions can nest.  */
static int skip_evaluation;

/* Nonzero means warn if undefined identifiers are evaluated.  */
static int warn_undef;

/* some external tables of character types */
extern unsigned char is_idstart[], is_idchar[], is_space[];

/* Flag for -pedantic.  */
extern int pedantic;

/* Flag for -traditional.  */
extern int traditional;

/* Flag for -lang-c89.  */
extern int c89;

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef MAX_CHAR_TYPE_SIZE
#define MAX_CHAR_TYPE_SIZE CHAR_TYPE_SIZE
#endif

#ifndef MAX_INT_TYPE_SIZE
#define MAX_INT_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef MAX_LONG_TYPE_SIZE
#define MAX_LONG_TYPE_SIZE LONG_TYPE_SIZE
#endif

#ifndef MAX_WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE WCHAR_TYPE_SIZE
#endif

#define MAX_CHAR_TYPE_MASK (MAX_CHAR_TYPE_SIZE < HOST_BITS_PER_WIDEST_INT \
			    ? (~ (~ (HOST_WIDEST_INT) 0 << MAX_CHAR_TYPE_SIZE)) \
			    : ~ (HOST_WIDEST_INT) 0)

#define MAX_WCHAR_TYPE_MASK (MAX_WCHAR_TYPE_SIZE < HOST_BITS_PER_WIDEST_INT \
			     ? ~ (~ (HOST_WIDEST_INT) 0 << MAX_WCHAR_TYPE_SIZE) \
			     : ~ (HOST_WIDEST_INT) 0)

/* Suppose A1 + B1 = SUM1, using 2's complement arithmetic ignoring overflow.
   Suppose A, B and SUM have the same respective signs as A1, B1, and SUM1.
   Suppose SIGNEDP is negative if the result is signed, zero if unsigned.
   Then this yields nonzero if overflow occurred during the addition.
   Overflow occurs if A and B have the same sign, but A and SUM differ in sign,
   and SIGNEDP is negative.
   Use `^' to test whether signs differ, and `< 0' to isolate the sign.  */
#define overflow_sum_sign(a, b, sum, signedp) \
	((~((a) ^ (b)) & ((a) ^ (sum)) & (signedp)) < 0)

struct constant;

HOST_WIDEST_INT parse_escape PARAMS ((char **, HOST_WIDEST_INT));
int check_assertion PARAMS ((U_CHAR *, int, int, struct arglist *));
struct hashnode *lookup PARAMS ((U_CHAR *, int, int));
void error PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
void verror PARAMS ((const char *, va_list));
void pedwarn PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
void warning PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;

static int parse_number PARAMS ((int));
static HOST_WIDEST_INT left_shift PARAMS ((struct constant *, unsigned HOST_WIDEST_INT));
static HOST_WIDEST_INT right_shift PARAMS ((struct constant *, unsigned HOST_WIDEST_INT));
static void integer_overflow PARAMS ((void));

/* `signedp' values */
#define SIGNED (~0)
#define UNSIGNED 0
%}

%union {
  struct constant {HOST_WIDEST_INT value; int signedp;} integer;
  struct name {U_CHAR *address; int length;} name;
  struct arglist *keywords;
}

%type <integer> exp exp1 start
%type <keywords> keywords
%token <integer> INT CHAR
%token <name> NAME
%token <integer> ERROR

%right '?' ':'
%left ','
%left OR
%left AND
%left '|'
%left '^'
%left '&'
%left EQUAL NOTEQUAL
%left '<' '>' LEQ GEQ
%left LSH RSH
%left '+' '-'
%left '*' '/' '%'
%right UNARY

/* %expect 40 */

%%

start   :	exp1
		{
		  expression_value = $1.value;
#ifdef TEST_EXP_READER
		  expression_signedp = $1.signedp;
#endif
		}
	;

/* Expressions, including the comma operator.  */
exp1	:	exp
	|	exp1 ',' exp
			{ if (pedantic)
			    pedwarn ("comma operator in operand of `#if'");
			  $$ = $3; }
	;

/* Expressions, not including the comma operator.  */
exp	:	'-' exp    %prec UNARY
			{ $$.value = - $2.value;
			  $$.signedp = $2.signedp;
			  if (($$.value & $2.value & $$.signedp) < 0)
			    integer_overflow (); }
	|	'!' exp    %prec UNARY
			{ $$.value = ! $2.value;
			  $$.signedp = SIGNED; }
	|	'+' exp    %prec UNARY
			{ $$ = $2; }
	|	'~' exp    %prec UNARY
			{ $$.value = ~ $2.value;
			  $$.signedp = $2.signedp; }
	|	'#' NAME
  			{ $$.value = check_assertion ($2.address, $2.length,
						      0, NULL_PTR);
			  $$.signedp = SIGNED; }
	|	'#' NAME
			{ keyword_parsing = 1; }
		'(' keywords ')'
  			{ $$.value = check_assertion ($2.address, $2.length,
						      1, $5);
			  keyword_parsing = 0;
			  $$.signedp = SIGNED; }
	|	'(' exp1 ')'
			{ $$ = $2; }
	;

/* Binary operators in order of decreasing precedence.  */
exp	:	exp '*' exp
			{ $$.signedp = $1.signedp & $3.signedp;
			  if ($$.signedp)
			    {
			      $$.value = $1.value * $3.value;
			      if ($1.value
				  && ($$.value / $1.value != $3.value
				      || ($$.value & $1.value & $3.value) < 0))
				integer_overflow ();
			    }
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					* $3.value); }
	|	exp '/' exp
			{ if ($3.value == 0)
			    {
			      if (!skip_evaluation)
				error ("division by zero in #if");
			      $3.value = 1;
			    }
			  $$.signedp = $1.signedp & $3.signedp;
			  if ($$.signedp)
			    {
			      $$.value = $1.value / $3.value;
			      if (($$.value & $1.value & $3.value) < 0)
				integer_overflow ();
			    }
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					/ $3.value); }
	|	exp '%' exp
			{ if ($3.value == 0)
			    {
			      if (!skip_evaluation)
				error ("division by zero in #if");
			      $3.value = 1;
			    }
			  $$.signedp = $1.signedp & $3.signedp;
			  if ($$.signedp)
			    $$.value = $1.value % $3.value;
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					% $3.value); }
	|	exp '+' exp
			{ $$.value = $1.value + $3.value;
			  $$.signedp = $1.signedp & $3.signedp;
			  if (overflow_sum_sign ($1.value, $3.value,
						 $$.value, $$.signedp))
			    integer_overflow (); }
	|	exp '-' exp
			{ $$.value = $1.value - $3.value;
			  $$.signedp = $1.signedp & $3.signedp;
			  if (overflow_sum_sign ($$.value, $3.value,
						 $1.value, $$.signedp))
			    integer_overflow (); }
	|	exp LSH exp
			{ $$.signedp = $1.signedp;
			  if (($3.value & $3.signedp) < 0)
			    $$.value = right_shift (&$1, -$3.value);
			  else
			    $$.value = left_shift (&$1, $3.value); }
	|	exp RSH exp
			{ $$.signedp = $1.signedp;
			  if (($3.value & $3.signedp) < 0)
			    $$.value = left_shift (&$1, -$3.value);
			  else
			    $$.value = right_shift (&$1, $3.value); }
	|	exp EQUAL exp
			{ $$.value = ($1.value == $3.value);
			  $$.signedp = SIGNED; }
	|	exp NOTEQUAL exp
			{ $$.value = ($1.value != $3.value);
			  $$.signedp = SIGNED; }
	|	exp LEQ exp
			{ $$.signedp = SIGNED;
			  if ($1.signedp & $3.signedp)
			    $$.value = $1.value <= $3.value;
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					<= $3.value); }
	|	exp GEQ exp
			{ $$.signedp = SIGNED;
			  if ($1.signedp & $3.signedp)
			    $$.value = $1.value >= $3.value;
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					>= $3.value); }
	|	exp '<' exp
			{ $$.signedp = SIGNED;
			  if ($1.signedp & $3.signedp)
			    $$.value = $1.value < $3.value;
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					< $3.value); }
	|	exp '>' exp
			{ $$.signedp = SIGNED;
			  if ($1.signedp & $3.signedp)
			    $$.value = $1.value > $3.value;
			  else
			    $$.value = ((unsigned HOST_WIDEST_INT) $1.value
					> $3.value); }
	|	exp '&' exp
			{ $$.value = $1.value & $3.value;
			  $$.signedp = $1.signedp & $3.signedp; }
	|	exp '^' exp
			{ $$.value = $1.value ^ $3.value;
			  $$.signedp = $1.signedp & $3.signedp; }
	|	exp '|' exp
			{ $$.value = $1.value | $3.value;
			  $$.signedp = $1.signedp & $3.signedp; }
	|	exp AND
			{ skip_evaluation += !$1.value; }
		exp
			{ skip_evaluation -= !$1.value;
			  $$.value = ($1.value && $4.value);
			  $$.signedp = SIGNED; }
	|	exp OR
			{ skip_evaluation += !!$1.value; }
		exp
			{ skip_evaluation -= !!$1.value;
			  $$.value = ($1.value || $4.value);
			  $$.signedp = SIGNED; }
	|	exp '?'
			{ skip_evaluation += !$1.value; }
	        exp ':'
			{ skip_evaluation += !!$1.value - !$1.value; }
		exp
			{ skip_evaluation -= !!$1.value;
			  $$.value = $1.value ? $4.value : $7.value;
			  $$.signedp = $4.signedp & $7.signedp; }
	|	INT
			{ $$ = yylval.integer; }
	|	CHAR
			{ $$ = yylval.integer; }
	|	NAME
			{ if (warn_undef && !skip_evaluation)
			    warning ("`%.*s' is not defined",
				     $1.length, $1.address);
			  $$.value = 0;
			  $$.signedp = SIGNED; }
	;

keywords :
			{ $$ = 0; } 
	|	'(' keywords ')' keywords
			{ struct arglist *temp;
			  $$ = (struct arglist *) xmalloc (sizeof (struct arglist));
			  $$->next = $2;
			  $$->name = (U_CHAR *) "(";
			  $$->length = 1;
			  temp = $$;
			  while (temp != 0 && temp->next != 0)
			    temp = temp->next;
			  temp->next = (struct arglist *) xmalloc (sizeof (struct arglist));
			  temp->next->next = $4;
			  temp->next->name = (U_CHAR *) ")";
			  temp->next->length = 1; }
	|	NAME keywords
			{ $$ = (struct arglist *) xmalloc (sizeof (struct arglist));
			  $$->name = $1.address;
			  $$->length = $1.length;
			  $$->next = $2; } 
	;
%%

/* During parsing of a C expression, the pointer to the next character
   is in this variable.  */

static char *lexptr;

/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/* maybe needs to actually deal with floating point numbers */

static int
parse_number (olen)
     int olen;
{
  register char *p = lexptr;
  register int c;
  register unsigned HOST_WIDEST_INT n = 0, nd, max_over_base;
  register int base = 10;
  register int len = olen;
  register int overflow = 0;
  register int digit, largest_digit = 0;
  int spec_long = 0;

  yylval.integer.signedp = SIGNED;

  if (*p == '0') {
    base = 8;
    if (len >= 3 && (p[1] == 'x' || p[1] == 'X')) {
      p += 2;
      base = 16;
      len -= 2;
    }
  }

  max_over_base = (unsigned HOST_WIDEST_INT) -1 / base;

  for (; len > 0; len--) {
    c = *p++;

    if (c >= '0' && c <= '9')
      digit = c - '0';
    else if (base == 16 && c >= 'a' && c <= 'f')
      digit = c - 'a' + 10;
    else if (base == 16 && c >= 'A' && c <= 'F')
      digit = c - 'A' + 10;
    else {
      /* `l' means long, and `u' means unsigned.  */
      while (1) {
	if (c == 'l' || c == 'L')
	  {
	    if (!pedantic < spec_long)
	      yyerror ("too many `l's in integer constant");
	    spec_long++;
	  }
	else if (c == 'u' || c == 'U')
	  {
	    if (! yylval.integer.signedp)
	      yyerror ("two `u's in integer constant");
	    yylval.integer.signedp = UNSIGNED;
	  }
	else {
	  if (c == '.' || c == 'e' || c == 'E' || c == 'p' || c == 'P')
	    yyerror ("Floating point numbers not allowed in #if expressions");
	  else
	    yyerror ("missing white space after number `%.*s'",
		     (int) (p - lexptr - 1), lexptr);
	}

	if (--len == 0)
	  break;
	c = *p++;
      }
      /* Don't look for any more digits after the suffixes.  */
      break;
    }
    if (largest_digit < digit)
      largest_digit = digit;
    nd = n * base + digit;
    overflow |= (max_over_base < n) | (nd < n);
    n = nd;
  }

  if (base <= largest_digit)
    pedwarn ("integer constant contains digits beyond the radix");

  if (overflow)
    pedwarn ("integer constant out of range");

  /* If too big to be signed, consider it unsigned.  */
  if (((HOST_WIDEST_INT) n & yylval.integer.signedp) < 0)
    {
      if (base == 10)
	warning ("integer constant is so large that it is unsigned");
      yylval.integer.signedp = UNSIGNED;
    }

  lexptr = p;
  yylval.integer.value = n;
  return INT;
}

struct token {
  const char *operator;
  int token;
};

static struct token tokentab2[] = {
  {"&&", AND},
  {"||", OR},
  {"<<", LSH},
  {">>", RSH},
  {"==", EQUAL},
  {"!=", NOTEQUAL},
  {"<=", LEQ},
  {">=", GEQ},
  {"++", ERROR},
  {"--", ERROR},
  {NULL, ERROR}
};

/* Read one token, getting characters through lexptr.  */

static int
yylex ()
{
  register int c;
  register int namelen;
  register unsigned char *tokstart;
  register struct token *toktab;
  int wide_flag;
  HOST_WIDEST_INT mask;

 retry:

  tokstart = (unsigned char *) lexptr;
  c = *tokstart;
  /* See if it is a special token of length 2.  */
  if (! keyword_parsing)
    for (toktab = tokentab2; toktab->operator != NULL; toktab++)
      if (c == *toktab->operator && tokstart[1] == toktab->operator[1]) {
	lexptr += 2;
	if (toktab->token == ERROR)
	  yyerror ("`%s' not allowed in operand of `#if'", toktab->operator);
	return toktab->token;
      }

  switch (c) {
  case '\n':
    return 0;
    
  case ' ':
  case '\t':
  case '\r':
    lexptr++;
    goto retry;
    
  case 'L':
    /* Capital L may start a wide-string or wide-character constant.  */
    if (lexptr[1] == '\'')
      {
	lexptr++;
	wide_flag = 1;
	mask = MAX_WCHAR_TYPE_MASK;
	goto char_constant;
      }
    if (lexptr[1] == '"')
      {
	lexptr++;
	wide_flag = 1;
	mask = MAX_WCHAR_TYPE_MASK;
	goto string_constant;
      }
    break;

  case '\'':
    wide_flag = 0;
    mask = MAX_CHAR_TYPE_MASK;
  char_constant:
    lexptr++;
    if (keyword_parsing) {
      char *start_ptr = lexptr - 1;
      while (1) {
	c = *lexptr++;
	if (c == '\\')
	  c = parse_escape (&lexptr, mask);
	else if (c == '\'')
	  break;
      }
      yylval.name.address = tokstart;
      yylval.name.length = lexptr - start_ptr;
      return NAME;
    }

    /* This code for reading a character constant
       handles multicharacter constants and wide characters.
       It is mostly copied from c-lex.c.  */
    {
      register HOST_WIDEST_INT result = 0;
      register int num_chars = 0;
      int chars_seen = 0;
      unsigned width = MAX_CHAR_TYPE_SIZE;
      int max_chars;
#ifdef MULTIBYTE_CHARS
      int longest_char = local_mb_cur_max ();
      char *token_buffer = (char *) alloca (longest_char);
      (void) local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif

      max_chars = MAX_LONG_TYPE_SIZE / width;
      if (wide_flag)
	width = MAX_WCHAR_TYPE_SIZE;

      while (1)
	{
	  c = *lexptr++;

	  if (c == '\'' || c == EOF)
	    break;

	  ++chars_seen;
	  if (c == '\\')
	    {
	      c = parse_escape (&lexptr, mask);
	    }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      wchar_t wc;
	      int i;
	      int char_len = -1;
	      for (i = 1; i <= longest_char; ++i)
		{
		  token_buffer[i - 1] = c;
		  char_len = local_mbtowc (& wc, token_buffer, i);
		  if (char_len != -1)
		    break;
		  c = *lexptr++;
		}
	      if (char_len > 1)
		{
		  /* mbtowc sometimes needs an extra char before accepting */
		  if (char_len < i)
		    lexptr--;
		  if (! wide_flag)
		    {
		      /* Merge character into result; ignore excess chars.  */
		      for (i = 1; i <= char_len; ++i)
			{
			  if (i > max_chars)
			    break;
			  if (width < HOST_BITS_PER_INT)
			    result = (result << width)
			      | (token_buffer[i - 1]
				 & ((1 << width) - 1));
			  else
			    result = token_buffer[i - 1];
			}
		      num_chars += char_len;
		      continue;
		    }
		}
	      else
		{
		  if (char_len == -1)
		    warning ("Ignoring invalid multibyte character");
		}
	      if (wide_flag)
		c = wc;
#endif /* ! MULTIBYTE_CHARS */
	    }

	  if (wide_flag)
	    {
	      if (chars_seen == 1) /* only keep the first one */
		result = c;
	      continue;
	    }

	  /* Merge character into result; ignore excess chars.  */
	  num_chars++;
	  if (num_chars <= max_chars)
	    {
	      if (width < HOST_BITS_PER_INT)
		result = (result << width) | (c & ((1 << width) - 1));
	      else
		result = c;
	    }
	}

      if (c != '\'')
	error ("malformatted character constant");
      else if (chars_seen == 0)
	error ("empty character constant");
      else if (num_chars > max_chars)
	{
	  num_chars = max_chars;
	  error ("character constant too long");
	}
      else if (chars_seen != 1 && ! traditional)
	warning ("multi-character character constant");

      /* If char type is signed, sign-extend the constant.  */
      if (! wide_flag)
	{
	  int num_bits = num_chars * width;
	  if (num_bits == 0)
	    /* We already got an error; avoid invalid shift.  */
	    yylval.integer.value = 0;
	  else if (lookup ((U_CHAR *) "__CHAR_UNSIGNED__",
		      sizeof ("__CHAR_UNSIGNED__") - 1, -1)
	      || ((result >> (num_bits - 1)) & 1) == 0)
	    yylval.integer.value
	      = result & (~ (unsigned HOST_WIDEST_INT) 0
			  >> (HOST_BITS_PER_WIDEST_INT - num_bits));
	  else
	    yylval.integer.value
	      = result | ~(~ (unsigned HOST_WIDEST_INT) 0
			   >> (HOST_BITS_PER_WIDEST_INT - num_bits));
	}
      else
	{
	  yylval.integer.value = result;
	}
    }

    /* This is always a signed type.  */
    yylval.integer.signedp = SIGNED;
    
    return CHAR;

    /* some of these chars are invalid in constant expressions;
       maybe do something about them later */
  case '/':
  case '+':
  case '-':
  case '*':
  case '%':
  case '|':
  case '&':
  case '^':
  case '~':
  case '!':
  case '@':
  case '<':
  case '>':
  case '[':
  case ']':
  case '.':
  case '?':
  case ':':
  case '=':
  case '{':
  case '}':
  case ',':
  case '#':
    if (keyword_parsing)
      break;
  case '(':
  case ')':
    lexptr++;
    return c;

  case '"':
    mask = MAX_CHAR_TYPE_MASK;
  string_constant:
    if (keyword_parsing) {
      char *start_ptr = lexptr;
      lexptr++;
      while (1) {
	c = *lexptr++;
	if (c == '\\')
	  c = parse_escape (&lexptr, mask);
	else if (c == '"')
	  break;
      }
      yylval.name.address = tokstart;
      yylval.name.length = lexptr - start_ptr;
      return NAME;
    }
    yyerror ("string constants not allowed in #if expressions");
    return ERROR;
  }

  if (c >= '0' && c <= '9' && !keyword_parsing) {
    /* It's a number */
    for (namelen = 1; ; namelen++) {
      int d = tokstart[namelen];
      if (! ((is_idchar[d] || d == '.')
	     || ((d == '-' || d == '+')
		 && (c == 'e' || c == 'E'
		     || ((c == 'p' || c == 'P') && ! c89))
		 && ! traditional)))
	break;
      c = d;
    }
    return parse_number (namelen);
  }

  /* It is a name.  See how long it is.  */

  if (keyword_parsing) {
    for (namelen = 0;; namelen++) {
      if (is_space[tokstart[namelen]])
	break;
      if (tokstart[namelen] == '(' || tokstart[namelen] == ')')
	break;
      if (tokstart[namelen] == '"' || tokstart[namelen] == '\'')
	break;
    }
  } else {
    if (!is_idstart[c]) {
      yyerror ("Invalid token in expression");
      return ERROR;
    }

    for (namelen = 0; is_idchar[tokstart[namelen]]; namelen++)
      ;
  }
  
  lexptr += namelen;
  yylval.name.address = tokstart;
  yylval.name.length = namelen;
  return NAME;
}


/* Parse a C escape sequence.  STRING_PTR points to a variable
   containing a pointer to the string to parse.  That pointer
   is updated past the characters we use.  The value of the
   escape sequence is returned.

   RESULT_MASK is used to mask out the result;
   an error is reported if bits are lost thereby.

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

HOST_WIDEST_INT
parse_escape (string_ptr, result_mask)
     char **string_ptr;
     HOST_WIDEST_INT result_mask;
{
  register int c = *(*string_ptr)++;
  switch (c)
    {
    case 'a':
      return TARGET_BELL;
    case 'b':
      return TARGET_BS;
    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
      return TARGET_ESC;
    case 'f':
      return TARGET_FF;
    case 'n':
      return TARGET_NEWLINE;
    case 'r':
      return TARGET_CR;
    case 't':
      return TARGET_TAB;
    case 'v':
      return TARGET_VT;
    case '\n':
      return -2;
    case 0:
      (*string_ptr)--;
      return 0;
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register HOST_WIDEST_INT i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '7')
	      i = (i << 3) + c - '0';
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	  }
	if (i != (i & result_mask))
	  {
	    i &= result_mask;
	    pedwarn ("octal escape sequence out of range");
	  }
	return i;
      }
    case 'x':
      {
	register unsigned HOST_WIDEST_INT i = 0, overflow = 0;
	register int digits_found = 0, digit;
	for (;;)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '9')
	      digit = c - '0';
	    else if (c >= 'a' && c <= 'f')
	      digit = c - 'a' + 10;
	    else if (c >= 'A' && c <= 'F')
	      digit = c - 'A' + 10;
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	    overflow |= i ^ (i << 4 >> 4);
	    i = (i << 4) + digit;
	    digits_found = 1;
	  }
	if (!digits_found)
	  yyerror ("\\x used with no following hex digits");
	if (overflow | (i != (i & result_mask)))
	  {
	    i &= result_mask;
	    pedwarn ("hex escape sequence out of range");
	  }
	return i;
      }
    default:
      return c;
    }
}

static void
integer_overflow ()
{
  if (!skip_evaluation && pedantic)
    pedwarn ("integer overflow in preprocessor expression");
}

static HOST_WIDEST_INT
left_shift (a, b)
     struct constant *a;
     unsigned HOST_WIDEST_INT b;
{
   /* It's unclear from the C standard whether shifts can overflow.
      The following code ignores overflow; perhaps a C standard
      interpretation ruling is needed.  */
  if (b >= HOST_BITS_PER_WIDEST_INT)
    return 0;
  else
    return (unsigned HOST_WIDEST_INT) a->value << b;
}

static HOST_WIDEST_INT
right_shift (a, b)
     struct constant *a;
     unsigned HOST_WIDEST_INT b;
{
  if (b >= HOST_BITS_PER_WIDEST_INT)
    return a->signedp ? a->value >> (HOST_BITS_PER_WIDEST_INT - 1) : 0;
  else if (a->signedp)
    return a->value >> b;
  else
    return (unsigned HOST_WIDEST_INT) a->value >> b;
}

/* This page contains the entry point to this file.  */

/* Parse STRING as an expression, and complain if this fails
   to use up all of the contents of STRING.
   STRING may contain '\0' bytes; it is terminated by the first '\n'
   outside a string constant, so that we can diagnose '\0' properly.
   If WARN_UNDEFINED is nonzero, warn if undefined identifiers are evaluated.
   We do not support C comments.  They should be removed before
   this function is called.  */

HOST_WIDEST_INT
parse_c_expression (string, warn_undefined)
     char *string;
     int warn_undefined;
{
  lexptr = string;
  warn_undef = warn_undefined;

  /* if there is some sort of scanning error, just return 0 and assume
     the parsing routine has printed an error message somewhere.
     there is surely a better thing to do than this.     */
  if (setjmp (parse_return_error))
    return 0;

  if (yyparse () != 0)
    abort ();

  if (*lexptr != '\n')
    error ("Junk after end of expression.");

  return expression_value;	/* set by yyparse () */
}

static void
yyerror VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif

  verror (msgid, args);
  va_end (args);
  skip_evaluation = 0;
  longjmp (parse_return_error, 1);
}


#ifdef TEST_EXP_READER

#if YYDEBUG
extern int yydebug;
#endif

int pedantic;
int traditional;
int c89;

int main PARAMS ((int, char **));
static void initialize_random_junk PARAMS ((void));
static void print_unsigned_host_widest_int PARAMS ((unsigned HOST_WIDEST_INT));

/* Main program for testing purposes.  */
int
main (argc, argv)
     int argc;
     char **argv;
{
  int n, c;
  char buf[1024];
  unsigned HOST_WIDEST_INT u;

  pedantic = 1 < argc;
  traditional = 2 < argc;
  c89 = 3 < argc;
#if YYDEBUG
  yydebug = 4 < argc;
#endif
  initialize_random_junk ();

  for (;;) {
    printf ("enter expression: ");
    n = 0;
    while ((buf[n] = c = getchar ()) != '\n' && c != EOF)
      n++;
    if (c == EOF)
      break;
    parse_c_expression (buf, 1);
    printf ("parser returned ");
    u = (unsigned HOST_WIDEST_INT) expression_value;
    if (expression_value < 0 && expression_signedp) {
      u = -u;
      printf ("-");
    }
    if (u == 0)
      printf ("0");
    else
      print_unsigned_host_widest_int (u);
    if (! expression_signedp)
      printf("u");
    printf ("\n");
  }

  return 0;
}

static void
print_unsigned_host_widest_int (u)
     unsigned HOST_WIDEST_INT u;
{
  if (u) {
    print_unsigned_host_widest_int (u / 10);
    putchar ('0' + (int) (u % 10));
  }
}

/* table to tell if char can be part of a C identifier. */
unsigned char is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
unsigned char is_idstart[256];
/* table to tell if c is horizontal or vertical space.  */
unsigned char is_space[256];

/*
 * initialize random junk in the hash table and maybe other places
 */
static void
initialize_random_junk ()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha (c) || c == '_'), etc.
   * Must do set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    ++is_idchar[TOUPPER(i)];
    ++is_idchar[i];
    ++is_idstart[TOUPPER(i)];
    ++is_idstart[i];
  }
  for (i = '0'; i <= '9'; i++)
    ++is_idchar[i];
  ++is_idchar['_'];
  ++is_idstart['_'];
  ++is_idchar['$'];
  ++is_idstart['$'];

  ++is_space[' '];
  ++is_space['\t'];
  ++is_space['\v'];
  ++is_space['\f'];
  ++is_space['\n'];
  ++is_space['\r'];
}

void
error VPARAMS ((char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, char *);
#endif

  fprintf (stderr, "error: ");
  vfprintf (stderr, _(msgid), args);
  fprintf (stderr, "\n");
  va_end (args);
}

void
pedwarn VPARAMS ((char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, char *);
#endif

  fprintf (stderr, "pedwarn: ");
  vfprintf (stderr, _(msgid), args);
  fprintf (stderr, "\n");
  va_end (args);
}

void
warning VPARAMS ((char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, char *);
#endif

  fprintf (stderr, "warning: ");
  vfprintf (stderr, _(msgid), args);
  fprintf (stderr, "\n");
  va_end (args);
}


int
check_assertion (name, sym_length, tokens_specified, tokens)
     U_CHAR *name;
     int sym_length;
     int tokens_specified;
     struct arglist *tokens;
{
  return 0;
}

struct hashnode *
lookup (name, len, hash)
     U_CHAR *name;
     int len;
     int hash;
{
  return (DEFAULT_SIGNED_CHAR) ? 0 : ((struct hashnode *) -1);
}

PTR
xmalloc (size)
  size_t size;
{
  return (PTR) malloc (size);
}
#endif
