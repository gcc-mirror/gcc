/* { dg-do run } */
/* { dg-options "-O2" } */
int test;

int
main ()
{
  int x = test;
  asm ("movl $1,test");
  if (x + test != 1)
    __builtin_trap ();
  return 0;
}
