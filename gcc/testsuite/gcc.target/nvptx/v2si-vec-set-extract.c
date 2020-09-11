/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

typedef int __v2si __attribute__((__vector_size__(8)));

int
foo (__v2si arg)
{
  return arg[0] + arg[1];
}

__v2si
foo2 (unsigned int a, unsigned int b)
{
  __v2si res;
  res[0] = a;
  res[1] = b;
  return res;
}

/* { dg-final { scan-assembler "mov.u32.*\\.x;" } } */
/* { dg-final { scan-assembler "mov.u32.*\\.y;" } } *

/* { dg-final { scan-assembler "mov.u32\[\t\]%r\[0-9\]\[0-9\]*\\.x, " } } */
/* { dg-final { scan-assembler "mov.u32\[\t\]%r\[0-9\]\[0-9\]*\\.y, " } } */
