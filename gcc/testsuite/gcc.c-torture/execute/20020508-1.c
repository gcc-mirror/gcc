/* This tests the rotate patterns that some machines support.  */

#include <limits.h>

#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif

#define ROR(a,b) (((a) >> (b)) | ((a) << ((sizeof (a) * CHAR_BIT) - (b))))
#define ROL(a,b) (((a) << (b)) | ((a) >> ((sizeof (a) * CHAR_BIT) - (b))))

#define CHAR_VALUE ((unsigned char)0xf234U)
#define SHORT_VALUE ((unsigned short)0xf234U)
#define INT_VALUE 0xf234U
#define LONG_VALUE 0xf2345678LU
#define LL_VALUE 0xf2345678abcdef0LLU

#define SHIFT1 4
#define SHIFT2 ((sizeof (long long) * CHAR_BIT) - SHIFT1)

unsigned char uc = CHAR_VALUE;
unsigned short us = SHORT_VALUE;
unsigned int ui = INT_VALUE;
unsigned long ul = LONG_VALUE;
unsigned long long ull = LL_VALUE;
int shift1 = SHIFT1;
int shift2 = SHIFT2;

main ()
{
  if (ROR (uc, shift1) != ROR (CHAR_VALUE, SHIFT1))
    abort ();

  if (ROR (uc, SHIFT1) != ROR (CHAR_VALUE, SHIFT1))
    abort ();

  if (ROR (us, shift1) != ROR (SHORT_VALUE, SHIFT1))
    abort ();

  if (ROR (us, SHIFT1) != ROR (SHORT_VALUE, SHIFT1))
    abort ();

  if (ROR (ui, shift1) != ROR (INT_VALUE, SHIFT1))
    abort ();

  if (ROR (ui, SHIFT1) != ROR (INT_VALUE, SHIFT1))
    abort ();

  if (ROR (ul, shift1) != ROR (LONG_VALUE, SHIFT1))
    abort ();

  if (ROR (ul, SHIFT1) != ROR (LONG_VALUE, SHIFT1))
    abort ();

  if (ROR (ull, shift1) != ROR (LL_VALUE, SHIFT1))
    abort ();

  if (ROR (ull, SHIFT1) != ROR (LL_VALUE, SHIFT1))
    abort ();

  if (ROR (ull, shift2) != ROR (LL_VALUE, SHIFT2))
    abort ();

  if (ROR (ull, SHIFT2) != ROR (LL_VALUE, SHIFT2))
    abort ();

  if (ROL (uc, shift1) != ROL (CHAR_VALUE, SHIFT1))
    abort ();

  if (ROL (uc, SHIFT1) != ROL (CHAR_VALUE, SHIFT1))
    abort ();

  if (ROL (us, shift1) != ROL (SHORT_VALUE, SHIFT1))
    abort ();

  if (ROL (us, SHIFT1) != ROL (SHORT_VALUE, SHIFT1))
    abort ();

  if (ROL (ui, shift1) != ROL (INT_VALUE, SHIFT1))
    abort ();

  if (ROL (ui, SHIFT1) != ROL (INT_VALUE, SHIFT1))
    abort ();

  if (ROL (ul, shift1) != ROL (LONG_VALUE, SHIFT1))
    abort ();

  if (ROL (ul, SHIFT1) != ROL (LONG_VALUE, SHIFT1))
    abort ();

  if (ROL (ull, shift1) != ROL (LL_VALUE, SHIFT1))
    abort ();

  if (ROL (ull, SHIFT1) != ROL (LL_VALUE, SHIFT1))
    abort ();

  if (ROL (ull, shift2) != ROL (LL_VALUE, SHIFT2))
    abort ();

  if (ROL (ull, SHIFT2) != ROL (LL_VALUE, SHIFT2))
    abort ();

  exit (0);
}
