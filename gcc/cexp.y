/* Parse C expressions for CCCP.
   Copyright (C) 1987, 1992, 1994 Free Software Foundation.

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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!

 Adapted from expread.y of GDB by Paul Rubin, July 1986.  */

/* Parse a C expression from text in a string  */
   
%{
#include "config.h"
#include <setjmp.h>
/* #define YYDEBUG 1 */

#ifdef MULTIBYTE_CHARS
#include <stdlib.h>
#include <locale.h>
#endif

#include <stdio.h>

typedef unsigned char U_CHAR;

/* This is used for communicating lists of keywords with cccp.c.  */
struct arglist {
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
};

/* Define a generic NULL if one hasn't already been defined.  */

#ifndef NULL
#define NULL 0
#endif

#ifndef GENERIC_PTR
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define GENERIC_PTR void *
#else
#define GENERIC_PTR char *
#endif
#endif

/* Find the largest host integer type and set its size and type.  */

#ifndef HOST_BITS_PER_WIDE_INT

#if HOST_BITS_PER_LONG > HOST_BITS_PER_INT
#define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_LONG
#define HOST_WIDE_INT long
#else
#define HOST_BITS_PER_WIDE_INT HOST_BITS_PER_INT
#define HOST_WIDE_INT int
#endif

#endif

#ifndef NULL_PTR
#define NULL_PTR ((GENERIC_PTR)0)
#endif

int yylex ();
void yyerror ();
HOST_WIDE_INT expression_value;

static jmp_buf parse_return_error;

/* Nonzero means count most punctuation as part of a name.  */
static int keyword_parsing = 0;

/* some external tables of character types */
extern unsigned char is_idstart[], is_idchar[], is_hor_space[];

extern char *xmalloc ();

/* Flag for -pedantic.  */
extern int pedantic;

/* Flag for -traditional.  */
extern int traditional;

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

/* Yield nonzero if adding two numbers with A's and B's signs can yield a
   number with SUM's sign, where A, B, and SUM are all C integers.  */
#define possible_sum_sign(a, b, sum) ((((a) ^ (b)) | ~ ((a) ^ (sum))) < 0)

static void integer_overflow ();
static long left_shift ();
static long right_shift ();
%}

%union {
  struct constant {long value; int unsignedp;} integer;
  struct name {U_CHAR *address; int length;} name;
  struct arglist *keywords;
  int voidval;
  char *sval;
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
		{ expression_value = $1.value; }
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
			  if (($$.value & $2.value) < 0 && ! $2.unsignedp)
			    integer_overflow ();
			  $$.unsignedp = $2.unsignedp; }
	|	'!' exp    %prec UNARY
			{ $$.value = ! $2.value;
			  $$.unsignedp = 0; }
	|	'+' exp    %prec UNARY
			{ $$ = $2; }
	|	'~' exp    %prec UNARY
			{ $$.value = ~ $2.value;
			  $$.unsignedp = $2.unsignedp; }
	|	'#' NAME
  			{ $$.value = check_assertion ($2.address, $2.length,
						      0, NULL_PTR);
			  $$.unsignedp = 0; }
	|	'#' NAME
			{ keyword_parsing = 1; }
		'(' keywords ')'
  			{ $$.value = check_assertion ($2.address, $2.length,
						      1, $5);
			  keyword_parsing = 0;
			  $$.unsignedp = 0; }
	|	'(' exp1 ')'
			{ $$ = $2; }
	;

/* Binary operators in order of decreasing precedence.  */
exp	:	exp '*' exp
			{ $$.unsignedp = $1.unsignedp || $3.unsignedp;
			  if ($$.unsignedp)
			    $$.value = (unsigned long) $1.value * $3.value;
			  else
			    {
			      $$.value = $1.value * $3.value;
			      if ($1.value
				  && ($$.value / $1.value != $3.value
				      || ($$.value & $1.value & $3.value) < 0))
				integer_overflow ();
			    } }
	|	exp '/' exp
			{ if ($3.value == 0)
			    {
			      error ("division by zero in #if");
			      $3.value = 1;
			    }
			  $$.unsignedp = $1.unsignedp || $3.unsignedp;
			  if ($$.unsignedp)
			    $$.value = (unsigned long) $1.value / $3.value;
			  else
			    {
			      $$.value = $1.value / $3.value;
			      if (($$.value & $1.value & $3.value) < 0)
				integer_overflow ();
			    } }
	|	exp '%' exp
			{ if ($3.value == 0)
			    {
			      error ("division by zero in #if");
			      $3.value = 1;
			    }
			  $$.unsignedp = $1.unsignedp || $3.unsignedp;
			  if ($$.unsignedp)
			    $$.value = (unsigned long) $1.value % $3.value;
			  else
			    $$.value = $1.value % $3.value; }
	|	exp '+' exp
			{ $$.value = $1.value + $3.value;
			  $$.unsignedp = $1.unsignedp || $3.unsignedp;
			  if (! $$.unsignedp
			      && ! possible_sum_sign ($1.value, $3.value,
						      $$.value))
			    integer_overflow (); }
	|	exp '-' exp
			{ $$.value = $1.value - $3.value;
			  $$.unsignedp = $1.unsignedp || $3.unsignedp;
			  if (! $$.unsignedp
			      && ! possible_sum_sign ($$.value, $3.value,
						      $1.value))
			    integer_overflow (); }
	|	exp LSH exp
			{ $$.unsignedp = $1.unsignedp;
			  if ($3.value < 0 && ! $3.unsignedp)
			    $$.value = right_shift (&$1, -$3.value);
			  else
			    $$.value = left_shift (&$1, $3.value); }
	|	exp RSH exp
			{ $$.unsignedp = $1.unsignedp;
			  if ($3.value < 0 && ! $3.unsignedp)
			    $$.value = left_shift (&$1, -$3.value);
			  else
			    $$.value = right_shift (&$1, $3.value); }
	|	exp EQUAL exp
			{ $$.value = ($1.value == $3.value);
			  $$.unsignedp = 0; }
	|	exp NOTEQUAL exp
			{ $$.value = ($1.value != $3.value);
			  $$.unsignedp = 0; }
	|	exp LEQ exp
			{ $$.unsignedp = 0;
			  if ($1.unsignedp || $3.unsignedp)
			    $$.value = (unsigned long) $1.value <= $3.value;
			  else
			    $$.value = $1.value <= $3.value; }
	|	exp GEQ exp
			{ $$.unsignedp = 0;
			  if ($1.unsignedp || $3.unsignedp)
			    $$.value = (unsigned long) $1.value >= $3.value;
			  else
			    $$.value = $1.value >= $3.value; }
	|	exp '<' exp
			{ $$.unsignedp = 0;
			  if ($1.unsignedp || $3.unsignedp)
			    $$.value = (unsigned long) $1.value < $3.value;
			  else
			    $$.value = $1.value < $3.value; }
	|	exp '>' exp
			{ $$.unsignedp = 0;
			  if ($1.unsignedp || $3.unsignedp)
			    $$.value = (unsigned long) $1.value > $3.value;
			  else
			    $$.value = $1.value > $3.value; }
	|	exp '&' exp
			{ $$.value = $1.value & $3.value;
			  $$.unsignedp = $1.unsignedp || $3.unsignedp; }
	|	exp '^' exp
			{ $$.value = $1.value ^ $3.value;
			  $$.unsignedp = $1.unsignedp || $3.unsignedp; }
	|	exp '|' exp
			{ $$.value = $1.value | $3.value;
			  $$.unsignedp = $1.unsignedp || $3.unsignedp; }
	|	exp AND exp
			{ $$.value = ($1.value && $3.value);
			  $$.unsignedp = 0; }
	|	exp OR exp
			{ $$.value = ($1.value || $3.value);
			  $$.unsignedp = 0; }
	|	exp '?' exp ':' exp
			{ $$.value = $1.value ? $3.value : $5.value;
			  $$.unsignedp = $3.unsignedp || $5.unsignedp; }
	|	INT
			{ $$ = yylval.integer; }
	|	CHAR
			{ $$ = yylval.integer; }
	|	NAME
			{ $$.value = 0;
			  $$.unsignedp = 0; }
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

int
parse_number (olen)
     int olen;
{
  register char *p = lexptr;
  register int c;
  register unsigned long n = 0, nd, ULONG_MAX_over_base;
  register int base = 10;
  register int len = olen;
  register int overflow = 0;
  register int digit, largest_digit = 0;
  int spec_long = 0;

  for (c = 0; c < len; c++)
    if (p[c] == '.') {
      /* It's a float since it contains a point.  */
      yyerror ("floating point numbers not allowed in #if expressions");
      return ERROR;
    }

  yylval.integer.unsignedp = 0;

  if (len >= 3 && (!strncmp (p, "0x", 2) || !strncmp (p, "0X", 2))) {
    p += 2;
    base = 16;
    len -= 2;
  }
  else if (*p == '0')
    base = 8;

  ULONG_MAX_over_base = (unsigned long) -1 / base;

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
	    if (spec_long)
	      yyerror ("two `l's in integer constant");
	    spec_long = 1;
	  }
	else if (c == 'u' || c == 'U')
	  {
	    if (yylval.integer.unsignedp)
	      yyerror ("two `u's in integer constant");
	    yylval.integer.unsignedp = 1;
	  }
	else
	  break;

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
    overflow |= ULONG_MAX_over_base < n | nd < n;
    n = nd;
  }

  if (len != 0) {
    yyerror ("Invalid number in #if expression");
    return ERROR;
  }

  if (base <= largest_digit)
    warning ("integer constant contains digits beyond the radix");

  if (overflow)
    warning ("integer constant out of range");

  /* If too big to be signed, consider it unsigned.  */
  if ((long) n < 0 && ! yylval.integer.unsignedp)
    {
      if (base == 10)
	warning ("integer constant is so large that it is unsigned");
      yylval.integer.unsignedp = 1;
    }

  lexptr = p;
  yylval.integer.value = n;
  return INT;
}

struct token {
  char *operator;
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

int
yylex ()
{
  register int c;
  register int namelen;
  register unsigned char *tokstart;
  register struct token *toktab;
  int wide_flag;

 retry:

  tokstart = (unsigned char *) lexptr;
  c = *tokstart;
  /* See if it is a special token of length 2.  */
  if (! keyword_parsing)
    for (toktab = tokentab2; toktab->operator != NULL; toktab++)
      if (c == *toktab->operator && tokstart[1] == toktab->operator[1]) {
	lexptr += 2;
	if (toktab->token == ERROR)
	  {
	    char *buf = (char *) alloca (40);
	    sprintf (buf, "`%s' not allowed in operand of `#if'", toktab->operator);
	    yyerror (buf);
	  }
	return toktab->token;
      }

  switch (c) {
  case 0:
    return 0;
    
  case ' ':
  case '\t':
  case '\r':
  case '\n':
    lexptr++;
    goto retry;
    
  case 'L':
    /* Capital L may start a wide-string or wide-character constant.  */
    if (lexptr[1] == '\'')
      {
	lexptr++;
	wide_flag = 1;
	goto char_constant;
      }
    if (lexptr[1] == '"')
      {
	lexptr++;
	wide_flag = 1;
	goto string_constant;
      }
    break;

  case '\'':
    wide_flag = 0;
  char_constant:
    lexptr++;
    if (keyword_parsing) {
      char *start_ptr = lexptr - 1;
      while (1) {
	c = *lexptr++;
	if (c == '\\')
	  c = parse_escape (&lexptr);
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
      register int result = 0;
      register num_chars = 0;
      unsigned width = MAX_CHAR_TYPE_SIZE;
      int max_chars;
      char *token_buffer;

      if (wide_flag)
	{
	  width = MAX_WCHAR_TYPE_SIZE;
#ifdef MULTIBYTE_CHARS
	  max_chars = MB_CUR_MAX;
#else
	  max_chars = 1;
#endif
	}
      else
	max_chars = MAX_LONG_TYPE_SIZE / width;

      token_buffer = (char *) alloca (max_chars + 1);

      while (1)
	{
	  c = *lexptr++;

	  if (c == '\'' || c == EOF)
	    break;

	  if (c == '\\')
	    {
	      c = parse_escape (&lexptr);
	      if (width < HOST_BITS_PER_INT
		  && (unsigned) c >= (1 << width))
		pedwarn ("escape sequence out of range for character");
	    }

	  num_chars++;

	  /* Merge character into result; ignore excess chars.  */
	  if (num_chars < max_chars + 1)
	    {
	      if (width < HOST_BITS_PER_INT)
		result = (result << width) | (c & ((1 << width) - 1));
	      else
		result = c;
	      token_buffer[num_chars - 1] = c;
	    }
	}

      token_buffer[num_chars] = 0;

      if (c != '\'')
	error ("malformatted character constant");
      else if (num_chars == 0)
	error ("empty character constant");
      else if (num_chars > max_chars)
	{
	  num_chars = max_chars;
	  error ("character constant too long");
	}
      else if (num_chars != 1 && ! traditional)
	warning ("multi-character character constant");

      /* If char type is signed, sign-extend the constant.  */
      if (! wide_flag)
	{
	  int num_bits = num_chars * width;

	  if (lookup ("__CHAR_UNSIGNED__", sizeof ("__CHAR_UNSIGNED__")-1, -1)
	      || ((result >> (num_bits - 1)) & 1) == 0)
	    yylval.integer.value
	      = result & ((unsigned long) ~0 >> (HOST_BITS_PER_LONG - num_bits));
	  else
	    yylval.integer.value
	      = result | ~((unsigned long) ~0 >> (HOST_BITS_PER_LONG - num_bits));
	}
      else
	{
#ifdef MULTIBYTE_CHARS
	  /* Set the initial shift state and convert the next sequence.  */
	  result = 0;
	  /* In all locales L'\0' is zero and mbtowc will return zero,
	     so don't use it.  */
	  if (num_chars > 1
	      || (num_chars == 1 && token_buffer[0] != '\0'))
	    {
	      wchar_t wc;
	      (void) mbtowc (NULL_PTR, NULL_PTR, 0);
	      if (mbtowc (& wc, token_buffer, num_chars) == num_chars)
		result = wc;
	      else
		warning ("Ignoring invalid multibyte character");
	    }
#endif
	  yylval.integer.value = result;
	}
    }

    /* This is always a signed type.  */
    yylval.integer.unsignedp = 0;
    
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
  string_constant:
    if (keyword_parsing) {
      char *start_ptr = lexptr;
      lexptr++;
      while (1) {
	c = *lexptr++;
	if (c == '\\')
	  c = parse_escape (&lexptr);
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
    for (namelen = 0;
	 c = tokstart[namelen], is_idchar[c] || c == '.'; 
	 namelen++)
      ;
    return parse_number (namelen);
  }

  /* It is a name.  See how long it is.  */

  if (keyword_parsing) {
    for (namelen = 0;; namelen++) {
      if (is_hor_space[tokstart[namelen]])
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

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

int
parse_escape (string_ptr)
     char **string_ptr;
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
      return 033;
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
	register int i = c - '0';
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
	if ((i & ~((1 << MAX_CHAR_TYPE_SIZE) - 1)) != 0)
	  {
	    i &= (1 << MAX_CHAR_TYPE_SIZE) - 1;
	    warning ("octal character constant does not fit in a byte");
	  }
	return i;
      }
    case 'x':
      {
	register unsigned i = 0, overflow = 0, digits_found = 0, digit;
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
	if (overflow | (i & ~((1 << BITS_PER_UNIT) - 1)))
	  {
	    i &= (1 << BITS_PER_UNIT) - 1;
	    warning ("hex character constant does not fit in a byte");
	  }
	return i;
      }
    default:
      return c;
    }
}

void
yyerror (s)
     char *s;
{
  error (s);
  longjmp (parse_return_error, 1);
}

static void
integer_overflow ()
{
  if (pedantic)
    pedwarn ("integer overflow in preprocessor expression");
}

static long
left_shift (a, b)
     struct constant *a;
     unsigned long b;
{
  if (b >= HOST_BITS_PER_LONG)
    {
      if (! a->unsignedp && a->value != 0)
	integer_overflow ();
      return 0;
    }
  else if (a->unsignedp)
    return (unsigned long) a->value << b;
  else
    {
      long l = a->value << b;
      if (l >> b != a->value)
	integer_overflow ();
      return l;
    }
}

static long
right_shift (a, b)
     struct constant *a;
     unsigned long b;
{
  if (b >= HOST_BITS_PER_LONG)
    return a->unsignedp ? 0 : a->value >> (HOST_BITS_PER_LONG - 1);
  else if (a->unsignedp)
    return (unsigned long) a->value >> b;
  else
    return a->value >> b;
}

/* This page contains the entry point to this file.  */

/* Parse STRING as an expression, and complain if this fails
   to use up all of the contents of STRING.  */
/* We do not support C comments.  They should be removed before
   this function is called.  */

HOST_WIDE_INT
parse_c_expression (string)
     char *string;
{
  lexptr = string;
  
  if (lexptr == 0 || *lexptr == 0) {
    error ("empty #if expression");
    return 0;			/* don't include the #if group */
  }

  /* if there is some sort of scanning error, just return 0 and assume
     the parsing routine has printed an error message somewhere.
     there is surely a better thing to do than this.     */
  if (setjmp (parse_return_error))
    return 0;

  if (yyparse ())
    return 0;			/* actually this is never reached
				   the way things stand. */
  if (*lexptr)
    error ("Junk after end of expression.");

  return expression_value;	/* set by yyparse () */
}

#ifdef TEST_EXP_READER
extern int yydebug;

/* Main program for testing purposes.  */
int
main ()
{
  int n, c;
  char buf[1024];

/*
  yydebug = 1;
*/
  initialize_random_junk ();

  for (;;) {
    printf ("enter expression: ");
    n = 0;
    while ((buf[n] = getchar ()) != '\n' && buf[n] != EOF)
      n++;
    if (buf[n] == EOF)
      break;
    buf[n] = '\0';
    printf ("parser returned %ld\n", parse_c_expression (buf));
  }

  return 0;
}

/* table to tell if char can be part of a C identifier. */
unsigned char is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
unsigned char is_idstart[256];
/* table to tell if c is horizontal space.  isspace () thinks that
   newline is space; this is not a good idea for this program. */
char is_hor_space[256];

/*
 * initialize random junk in the hash table and maybe other places
 */
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
    ++is_idchar[i - 'a' + 'A'];
    ++is_idchar[i];
    ++is_idstart[i - 'a' + 'A'];
    ++is_idstart[i];
  }
  for (i = '0'; i <= '9'; i++)
    ++is_idchar[i];
  ++is_idchar['_'];
  ++is_idstart['_'];
#if DOLLARS_IN_IDENTIFIERS
  ++is_idchar['$'];
  ++is_idstart['$'];
#endif

  /* horizontal space table */
  ++is_hor_space[' '];
  ++is_hor_space['\t'];
}

error (msg)
{
  printf ("error: %s\n", msg);
}

warning (msg)
{
  printf ("warning: %s\n", msg);
}

struct hashnode *
lookup (name, len, hash)
     char *name;
     int len;
     int hash;
{
  return (DEFAULT_SIGNED_CHAR) ? 0 : ((struct hashnode *) -1);
}
#endif
