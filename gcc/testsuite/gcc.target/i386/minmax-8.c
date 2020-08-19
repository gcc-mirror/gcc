/* { dg-do compile } */
/* { dg-options "-Os" } */

#define max(a,b) (((a) > (b))? (a) : (b))
#define min(a,b) (((a) < (b))? (a) : (b))

int foo(int x)
{
  return max(x,12345);
}

int bar(int x)
{
  return min(x,87654);
}

/* { dg-final { scan-assembler-times "12345" 1 } } */
/* { dg-final { scan-assembler-times "87654" 1 } } */
