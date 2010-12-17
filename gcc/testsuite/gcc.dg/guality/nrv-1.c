/* { dg-do run } */
/* { dg-options "-g -fno-tree-sra" } */

void abort (void);

struct A
{
  int i[100];
};

struct A a1, a3;

__attribute__((noinline)) struct A
f ()
{
  struct A a2;
  a2.i[0] = 42;
  if (a3.i[0] != 0)
    abort ();
  a2.i[4] = 7;	/* { dg-final { gdb-test 20 "a2.i\[0\]" "42" } } */
  return a2;
}

int
main ()
{
  a1 = f ();
  return 0;
}
