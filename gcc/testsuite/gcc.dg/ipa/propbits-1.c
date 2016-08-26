/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fdump-ipa-cp" } */

__attribute__((noinline)) 
static int f(int x)
{
  int some_op(int);
  return some_op (x);
}

int main(void)
{
  int a = f(1);
  int b = f(2);
  int c = f(4);
  return a + b + c;
}

/* { dg-final { scan-ipa-dump "Adjusting mask for param 0 to 0x7" "cp" } } */
