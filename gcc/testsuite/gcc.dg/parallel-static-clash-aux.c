/* { dg-do compile } */
/* { dg-options "-fparallel-jobs=2 --param=balance-partitions=0" } */

static int __attribute__ ((noinline))
private ()
{
  return -42;
}

int
file2_c ()
{
  return private ();
}
