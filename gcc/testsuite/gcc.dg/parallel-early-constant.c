/* { dg-do compile } */
/* { dg-options "-fparallel-jobs=2 --param=balance-partitions=0" } */

#define A "This is a long test that tests the structure initialization"
#define B A,A
#define C B,B,B,B
#define D C,C,C,C

const char *foo1 ()
{
  return A;
}

int foo2 ()
{
  return 42;
}

int main()
{
  char *subs[]={ D, D, D, D, D, D, D, D, D, D, D, D, D, D, D};
}
