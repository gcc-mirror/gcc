/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10 -O3 -dp" } */

long addadd0(long a, long b, long c)
{
  return a+b+c;
}
long addadd1(long a, long b, long c, long *t)
{
  long r=a+b+c;
  *t = b;
  return r;
}
long addadd2(long s, long a, long b, long c)
{
  return b+c+a;
}

typedef vector long vlong;
vlong vaddadd(vlong a, vlong b, vlong c)
{
  return a+b+c;
}
vlong vaddadd1(vlong a, vlong b, vlong c, vlong *t)
{
  vlong r=a+b+c;
  *t = b;
  return r;
}
vlong vaddadd2(vlong s, vlong a, vlong b, vlong c)
{
  return a+b+c;
}

/* { dg-final { scan-assembler-times {\mfuse_add_add\M/}                3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vaddudm_vaddudm\M/}        3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_add\M/}                3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vaddudm_vaddudm\M/}        0 { target ilp32 } } } */
