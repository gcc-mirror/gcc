/* Test C23 digit separators.  Valid usages.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_Static_assert (123'45'6 == 123456);
_Static_assert (0'123 == 0123);
_Static_assert (0x1'23 == 0x123);
_Static_assert (0b1'01 == 0b101);

#define m(x) 0

_Static_assert (m(1'2)+(3'4) == 34);

_Static_assert (0x0'e-0xe == 0);

#define a0 '.' -
#define acat(x) a ## x
_Static_assert (acat (0'.') == 0);

#define c0(x) 0
#define b0 c0 (
#define bcat(x) b ## x
_Static_assert (bcat (0'\u00c0')) == 0);

extern void exit (int);
extern void abort (void);

int
main (void)
{
  if (314'159e-0'5f != 3.14159f)
    abort ();
  exit (0);
}

#line 0'123
_Static_assert (__LINE__ == 123);

#line 4'56'7'8'9
_Static_assert (__LINE__ == 456789);
