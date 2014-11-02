/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os -fselective-scheduling2" } */

long
foo (int x, long *y)
{
  long a = 0;
  switch (x)
    {
    case 0:
      a = *y;
      break;
    case 1:
      a = *y;
      break;
    case 2:
      a = *y;
      break;
    case 3:
      a = *y;
      break;
    case 4:
      a = *y;
      break;
    }
  return a;
}
