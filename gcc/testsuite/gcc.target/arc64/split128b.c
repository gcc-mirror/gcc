/* { dg-do compile } */
/* { dg-require-effective-target hs6x } */
/* { dg-options "-O2 -m128 -fpic" } */

struct tst {
  long a;
  long b;
}  static var;

void bar (long, struct tst);

void foo (void)
{
  bar (0, var);
}

/* { dg-final { scan-assembler "ldl\\s+r1,\\\[pcl,@var@pcl\\\]" } } */
/* { dg-final { scan-assembler "ldl\\s+r2,\\\[pcl,@var@pcl\\\+8\\\]" } } */
