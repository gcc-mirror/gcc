/* { dg-do compile } */
/* { dg-options "-O2" } */

void ext(int x, int y);

void foo(int x, int y)
{
  int t1 = x ^ y;
  int t2 = t1 ^ x;
  int t3 = t1 ^ y;
  ext(t2,t3);
}

/* { dg-final { scan-assembler "swpw r3,r2" } } */
