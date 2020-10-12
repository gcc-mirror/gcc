/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=i386 -mtune=generic" } */

#define min(a,b) (((a) < (b))? (a) : (b))

int foo(int x)
{
  return min(x,0);
}

signed char bar(signed char x)
{
  return min(x,0);
}

/* { dg-final { scan-assembler "cltd" } } */
/* { dg-final { scan-assembler "sarb" } } */
