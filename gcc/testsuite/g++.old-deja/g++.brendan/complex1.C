// Special g++ Options:

// This test makes sure that the stuff in lex.c (real_yylex) is
// set up to handle real and imag numbers correctly.  This test is against
// a bug where the compiler was not converting the integer `90' to a
// complex number, unless you did `90.0'.  Fixed 10/1/1997.

extern "C" int printf (const char *, ...);

__complex__ double cd;

int
main(int argc, char *argv[])
{
  cd = 1.0+90i;
  cd *= argc;

  if (__real__ cd != 1 || __imag__ cd != 90)
    exit (1);

  exit (0);
}
