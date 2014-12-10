/* { dg-do compile } */
/* { dg-require-effective-target arm_prefer_ldrd_strd } */
/* { dg-options "-O2 -mthumb" } */

struct
{
  int x;
  int y;
  char c;
  int d;
}a;

int foo(int x, int y)
{
  int c;
  a.x = x;
  c = a.x;
  a.d = c;
  a.y = y;

  return 0;
}
/* { dg-final { scan-assembler "strd\t" { target { arm_thumb2_ok } } } } */
