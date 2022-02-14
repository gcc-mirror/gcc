/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx -mno-mmx" { target x86_64-*-* i?86-*-* } } */

signed char a;
signed char f (int i, int j)
{
  signed char c;
  while (i != 0)
  {
    a ^= j;
    ++c;
    ++i;
  }
  return c;
}
