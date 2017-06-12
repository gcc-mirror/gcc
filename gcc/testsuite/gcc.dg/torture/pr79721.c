/* { dg-do run }  */
/* { dg-require-effective-target int32plus } */
/* We use -ftrapv so that when SCEV final value replacement introduces
   undefined overflow we trap.  UBSAN inhibits final value replacement.  */
/* { dg-additional-options "-ftrapv" } */

int __attribute__((noclone,noinline))
foo(int a, int b)
{
  int sum = 0;
  for (int i = 0; i < 60000; i++)
    sum += a + i * b;
  return sum;
}

int main(int argc, char **argv)
{
  if (foo (-30000, 2) != 1799940000)
    __builtin_abort ();
  return 0;
}
