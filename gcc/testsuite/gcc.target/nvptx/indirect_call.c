/* { dg-options "-O2 -msoft-stack" } */
/* { dg-do run } */

int
f1 (int a)
{
  return a + 1;
}
  
int (*f2)(int) = f1;

int
main ()
{
  if (f2 (100) != 101)
    __builtin_abort();

  return 0;
}
