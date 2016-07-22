/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -Wno-implicit-function-declaration" } */

void f (char *s1, char *s2)
{
  int z = 5;

  struct { char a[z]; } x;

  s1[0] = s2[0];

  foo (x, x);
}
