/* Test floating-point conversions.  __float128 type with TImode: bug
   53317.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  volatile unsigned long long a = 0x1000000000000ULL;
  volatile unsigned long long b = 0xffffffffffffffffULL;
  unsigned __int128 c = (((unsigned __int128) a) << 64) | b;
  __float128 d = c;
  if (d != 0x1.000000000000ffffffffffffffffp112q)
    abort ();
  exit (0);
}
