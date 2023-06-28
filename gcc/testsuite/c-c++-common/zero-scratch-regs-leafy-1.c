/* { dg-do run } */
/* { dg-options "-O2 -fzero-call-used-regs=leafy" } */

volatile int result = 0;
int 
__attribute__((noipa))
foo (int x)
{
  return x;
}
int main()
{
  result = foo (2);
  return 0;
}
