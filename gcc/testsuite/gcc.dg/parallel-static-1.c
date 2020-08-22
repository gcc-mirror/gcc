/* { dg-do compile } */
/* { dg-options "-fparallel-jobs=2 --param=balance-partitions=0" } */

static int global_var;

int foo1(void)
{
  global_var = 1;
}

int foo2(void)
{
  global_var = 2;
}

int main ()
{
  foo1 ();
  foo2 ();
  return 0;
}
