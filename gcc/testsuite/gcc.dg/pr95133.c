/* { dg-do compile } */
/* { dg-options "-O3" } */

extern int a[16];
void f (int *ip, int x)
{
  int *xp = a;
  for (int i=0; i<8; ++i)
  {
    base: if (x) return;
  }
  *xp++ = *ip;
  goto *(&&base + *ip);
}
