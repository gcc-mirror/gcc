/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Os" } */

#define max(a,b) (((a) > (b))? (a) : (b))
#define min(a,b) (((a) < (b))? (a) : (b))

int foo(int x)
{
  return max(x,0);
}

int bar(int x)
{
  return min(x,0);
}

unsigned int baz(unsigned int x)
{
  return min(x,1);
}

/* { dg-final { scan-assembler-times "xor" 3 } } */
/* { dg-final { scan-assembler-times "test" 3 } } */
