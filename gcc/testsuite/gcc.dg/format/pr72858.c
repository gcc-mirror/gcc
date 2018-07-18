/* { dg-options "-Wformat -fdiagnostics-show-caret" } */

#include "format.h"

/* Various format tests, some containing type mismatches.  Verify that for
   the type mismatch cases that we offer "good" suggestions.  Specifically,
   any suggestions should preserve flags characters, field width and precision,
   and, if possible, the conversion specifier character, whilst giving a
   corrected length modifier appropriate to the argument type.  */

/* Tests of "x" without a length modifier, with various param types.
   Suggestions should preserve the "x" for integer arguments.  */

void
test_x (char *d,
	int iexpr, unsigned int uiexpr,
	long lexpr, unsigned long ulexpr,
	long long llexpr, unsigned long long ullexpr,
	float fexpr, double dexpr, long double ldexpr,
	void *ptr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8x ", iexpr);
  sprintf (d, " %-8x ", uiexpr);

  sprintf (d, " %-8x ", lexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'long int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", lexpr);
                 ~~~^    ~~~~~
                 %-8lx
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8x ", ulexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'long unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", ulexpr);
                 ~~~^    ~~~~~~
                 %-8lx
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8x ", llexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'long long int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", llexpr);
                 ~~~^    ~~~~~~
                 %-8llx
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8x ", ullexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'long long unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", ullexpr);
                 ~~~^    ~~~~~~~
                 %-8llx
   { dg-end-multiline-output "" } */

  /* Floating-point arguments.  */

  sprintf (d, " %-8x ", fexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", fexpr);
                 ~~~^    ~~~~~
                 %-8f
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8x ", dexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", dexpr);
                 ~~~^    ~~~~~
                 %-8f
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8x ", ldexpr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'long double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", ldexpr);
                 ~~~^    ~~~~~~
                 %-8Lf
   { dg-end-multiline-output "" } */

  /* Pointer.  */
  sprintf (d, " %-8x ", ptr); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'void \\*'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", ptr);
                 ~~~^    ~~~
                 %-8p
   { dg-end-multiline-output "" } */

  /* Something unrecognized.  */
  struct s { int i; };
  struct s s;
  sprintf (d, " %-8x ", s); /* { dg-warning "20: format '%x' expects argument of type 'unsigned int', but argument 3 has type 'struct s'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8x ", s);
                 ~~~^    ~
   { dg-end-multiline-output "" } */
}

/* Tests of "x" with "l", with various param types.
   Suggestions should preserve the "x" for integer arguments.  */

void
test_lx (char *d,
	 int iexpr, unsigned int uiexpr,
	 long lexpr, unsigned long ulexpr,
	 long long llexpr, unsigned long long ullexpr,
	 float fexpr, double dexpr, long double ldexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8lx ", iexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", iexpr);
                 ~~~~^    ~~~~~
                 %-8x
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8lx ", uiexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", uiexpr);
                 ~~~~^    ~~~~~~
                 %-8x
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8lx ", lexpr);
  sprintf (d, " %-8lx ", ulexpr);

  sprintf (d, " %-8lx ", llexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'long long int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", llexpr);
                 ~~~~^    ~~~~~~
                 %-8llx
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8lx ", ullexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'long long unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", ullexpr);
                 ~~~~^    ~~~~~~~
                 %-8llx
   { dg-end-multiline-output "" } */

  /* Floating-point arguments.  */

  sprintf (d, " %-8lx ", fexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", fexpr);
                 ~~~~^    ~~~~~
                 %-8f
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8lx ", dexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", dexpr);
                 ~~~~^    ~~~~~
                 %-8f
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8lx ", ldexpr); /* { dg-warning "21: format '%lx' expects argument of type 'long unsigned int', but argument 3 has type 'long double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lx ", ldexpr);
                 ~~~~^    ~~~~~~
                 %-8Lf
   { dg-end-multiline-output "" } */
}

/* Tests of "o" without a length modifier, with various param types.
   Suggestions should preserve the "o" for integer arguments.  */

void
test_o (char *d,
	int iexpr, unsigned int uiexpr,
	long lexpr, unsigned long ulexpr,
	long long llexpr, unsigned long long ullexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8o ", iexpr);
  sprintf (d, " %-8o ", uiexpr);

  sprintf (d, " %-8o ", lexpr); /* { dg-warning "20: format '%o' expects argument of type 'unsigned int', but argument 3 has type 'long int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8o ", lexpr);
                 ~~~^    ~~~~~
                 %-8lo
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8o ", ulexpr); /* { dg-warning "20: format '%o' expects argument of type 'unsigned int', but argument 3 has type 'long unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8o ", ulexpr);
                 ~~~^    ~~~~~~
                 %-8lo
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8o ", llexpr); /* { dg-warning "20: format '%o' expects argument of type 'unsigned int', but argument 3 has type 'long long int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8o ", llexpr);
                 ~~~^    ~~~~~~
                 %-8llo
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8o ", ullexpr); /* { dg-warning "20: format '%o' expects argument of type 'unsigned int', but argument 3 has type 'long long unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8o ", ullexpr);
                 ~~~^    ~~~~~~~
                 %-8llo
   { dg-end-multiline-output "" } */
}

/* Tests of "o" with "l", with various param types.
   Suggestions should preserve the "o" for integer arguments.  */

void
test_lo (char *d,
	int iexpr, unsigned int uiexpr,
	long lexpr, unsigned long ulexpr,
	long long llexpr, unsigned long long ullexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8lo ", iexpr); /* { dg-warning "21: format '%lo' expects argument of type 'long unsigned int', but argument 3 has type 'int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lo ", iexpr);
                 ~~~~^    ~~~~~
                 %-8o
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8lo ", uiexpr); /* { dg-warning "21: format '%lo' expects argument of type 'long unsigned int', but argument 3 has type 'unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lo ", uiexpr);
                 ~~~~^    ~~~~~~
                 %-8o
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8lo ", lexpr);
  sprintf (d, " %-8lo ", ulexpr);

  sprintf (d, " %-8lo ", llexpr); /* { dg-warning "21: format '%lo' expects argument of type 'long unsigned int', but argument 3 has type 'long long int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lo ", llexpr);
                 ~~~~^    ~~~~~~
                 %-8llo
   { dg-end-multiline-output "" } */
  sprintf (d, " %-8lo ", ullexpr); /* { dg-warning "21: format '%lo' expects argument of type 'long unsigned int', but argument 3 has type 'long long unsigned int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8lo ", ullexpr);
                 ~~~~^    ~~~~~~~
                 %-8llo
   { dg-end-multiline-output "" } */
}

/* Tests of "e" without a length modifier, with various param types.
   Suggestions should preserve the "e" for float  arguments.  */

void
test_e (char *d, int iexpr, float fexpr, double dexpr, long double ldexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8e ", iexpr); /* { dg-warning "20: format '%e' expects argument of type 'double', but argument 3 has type 'int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8e ", iexpr);
                 ~~~^    ~~~~~
                 %-8d
   { dg-end-multiline-output "" } */

  /* Floating-point arguments.  */

  sprintf (d, " %-8e ", fexpr);
  sprintf (d, " %-8e ", dexpr);
  sprintf (d, " %-8e ", ldexpr); /* { dg-warning "20: format '%e' expects argument of type 'double', but argument 3 has type 'long double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8e ", ldexpr);
                 ~~~^    ~~~~~~
                 %-8Le
   { dg-end-multiline-output "" } */
}

/* Tests of "e" with "L", with various param types.
   Suggestions should preserve the "e" for float  arguments.  */

void
test_Le (char *d, int iexpr, float fexpr, double dexpr, long double ldexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8Le ", iexpr); /* { dg-warning "21: format '%Le' expects argument of type 'long double', but argument 3 has type 'int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8Le ", iexpr);
                 ~~~~^    ~~~~~
                 %-8d
   { dg-end-multiline-output "" } */

  /* Floating-point arguments.  */

  sprintf (d, " %-8Le ", fexpr); /* { dg-warning "21: format '%Le' expects argument of type 'long double', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8Le ", fexpr);
                 ~~~~^    ~~~~~
                 %-8e
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8Le ", dexpr); /* { dg-warning "21: format '%Le' expects argument of type 'long double', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8Le ", dexpr);
                 ~~~~^    ~~~~~
                 %-8e
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8Le ", ldexpr);
}

/* Tests of "E" without a length modifier, with various param types.
   Suggestions should preserve the "E" for floating-point arguments.  */

void
test_E (char *d, int iexpr, float fexpr, double dexpr, long double ldexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8E ", iexpr); /* { dg-warning "20: format '%E' expects argument of type 'double', but argument 3 has type 'int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8E ", iexpr);
                 ~~~^    ~~~~~
                 %-8d
   { dg-end-multiline-output "" } */

  /* Floating-point arguments.  */

  sprintf (d, " %-8E ", fexpr);
  sprintf (d, " %-8E ", dexpr);
  sprintf (d, " %-8E ", ldexpr); /* { dg-warning "20: format '%E' expects argument of type 'double', but argument 3 has type 'long double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8E ", ldexpr);
                 ~~~^    ~~~~~~
                 %-8LE
   { dg-end-multiline-output "" } */
}

/* Tests of "E" with "L", with various param types.
   Suggestions should preserve the "E" for floating-point arguments.  */

void
test_LE (char *d, int iexpr, float fexpr, double dexpr, long double ldexpr)
{
  /* Integer arguments.  */

  sprintf (d, " %-8LE ", iexpr); /* { dg-warning "21: format '%LE' expects argument of type 'long double', but argument 3 has type 'int'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8LE ", iexpr);
                 ~~~~^    ~~~~~
                 %-8d
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8LE ", fexpr); /* { dg-warning "21: format '%LE' expects argument of type 'long double', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8LE ", fexpr);
                 ~~~~^    ~~~~~
                 %-8E
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8LE ", dexpr); /* { dg-warning "21: format '%LE' expects argument of type 'long double', but argument 3 has type 'double'" } */
/* { dg-begin-multiline-output "" }
   sprintf (d, " %-8LE ", dexpr);
                 ~~~~^    ~~~~~
                 %-8E
   { dg-end-multiline-output "" } */

  sprintf (d, " %-8LE ", ldexpr);
}

/* Test of a suggestion for a conversion specification containing
   all features (flags, width, precision, length modifier), where
   all the other arguments have mismatching types.  */

void
test_everything (char *d, long lexpr)
{
  sprintf (d, "before %-+*.*lld after", lexpr, lexpr, lexpr); /* { dg-line test_everything_sprintf } */

  /* { dg-warning "26: field width specifier '\\*' expects argument of type 'int', but argument 3 has type 'long int'" "" { target *-*-* } test_everything_sprintf } */
  /* { dg-begin-multiline-output "" }
   sprintf (d, "before %-+*.*lld after", lexpr, lexpr, lexpr);
                       ~~~^~~~~~         ~~~~~
   { dg-end-multiline-output "" } */

  /* { dg-warning "28: field precision specifier '\\.\\*' expects argument of type 'int', but argument 4 has type 'long int'" "" { target *-*-* } test_everything_sprintf } */
  /* { dg-begin-multiline-output "" }
   sprintf (d, "before %-+*.*lld after", lexpr, lexpr, lexpr);
                       ~~~~~^~~~                ~~~~~
   { dg-end-multiline-output "" } */

  /* { dg-warning "31: format '%lld' expects argument of type 'long long int', but argument 5 has type 'long int'" "" { target *-*-* } test_everything_sprintf } */
  /* { dg-begin-multiline-output "" }
   sprintf (d, "before %-+*.*lld after", lexpr, lexpr, lexpr);
                       ~~~~~~~~^                       ~~~~~
                       %-+*.*ld
   { dg-end-multiline-output "" } */
}
