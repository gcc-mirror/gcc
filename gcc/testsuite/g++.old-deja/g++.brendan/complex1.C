// Special g++ Options:

// This test makes sure that the stuff in lex.c (real_yylex) is
// set up to handle real and imag numbers correctly.  This test is against
// a bug where the compiler was not converting the integer `90' to a
// complex number, unless you did `90.0'.  Fixed 10/1/1997.

extern "C" {
int printf (const char *, ...);
void exit (int);
void abort (void);
};

__complex__ double cd;

int one = 1;

int
main()
{
  cd = 1.0+90i;
  cd *= one;

  if (__real__ cd != 1 || __imag__ cd != 90)
    abort ();

  exit (0);
}
