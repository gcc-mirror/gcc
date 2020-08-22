/* { dg-do compile } */
/* { dg-options "-fparallel-jobs=2 --param=balance-partitions=0" } */

int foo1(void)
{
  static int var;
  var = 1;
}

int foo2(void)
{
  static int var;
  var = 2;
}

int main ()
{
  foo1 ();
  foo2 ();
  return 0;
}
