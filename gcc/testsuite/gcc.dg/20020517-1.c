/* This testcase caused ICE in do_SUBST on IA-32, because 0xf6 constant
   was not sign-extended for QImode.  */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=i686" { target i?86-*-* } } */

#include <limits.h>

void abort (void);
void exit (int);

void foo (void)
{
  int i;
  char *p;

  p = (char *) &i;
  *p = -10;
  if (* (unsigned char *) p != 0x100 - 10)
    abort ();
}

int main (void)
{
  if (UCHAR_MAX == 255)
    foo ();
  exit (0);
}
