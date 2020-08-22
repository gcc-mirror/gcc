/* { dg-do run } */
/* { dg-options "-fparallel-jobs=2 --param=balance-partitions=0 --param=promote-statics=1" } */
/* { dg-additional-sources "parallel-static-clash-aux.c" } */

int file2_c ();

static int __attribute__ ((noinline))
private ()
{
  return 42;
}

int
file1_c ()
{
  return private ();
}

int
main ()
{
  return file1_c () + file2_c ();
}
