/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-g -fselective-scheduling2 -O2 -fvar-tracking-assignments -w" } */

int *a;
int b, c;
void
d ()
{
  for (int e; c; e++)
    switch (e)
      {
      case 0:
       a[e] = 1;
      case 1:
       b = 2;
       break;
      default:
       a[e] = 3;
      }
}
