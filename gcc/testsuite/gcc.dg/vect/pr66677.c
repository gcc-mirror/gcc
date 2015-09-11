/* { dg-do compile } */
/* { dg-additional-options "-msse4" { target x86_64-*-* i?86-*-* } } */

int *a, *b;
void fn1(char *p1)
{
  int x;
  for (; x; x += 2)
    {
      a[x] = p1[0];
      a[x + 1] = 0;
      b[x] = b[x + 1] = p1[1];
      p1 += 4;
    }
}
