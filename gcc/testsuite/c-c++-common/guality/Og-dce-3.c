/* { dg-do run } */
/* { dg-options "-g" } */

volatile int amount = 10;

void __attribute__((noipa))
do_something (int *ptr)
{
  *ptr += 10;
}

int __attribute__((noipa))
foo (int count)
{
  int x = 1;
  for (int i = 0; i < count; ++i)
    do_something (&x); /* { dg-final { gdb-test . "x" "1" } } */
  int res = x; /* { dg-final { gdb-test . "x" "101" } } */
  x = res + 1;
  return res; /* { dg-final { gdb-test . "x" "102" } } */
  
}

int
main (void)
{
  foo (10);
  return 0;
}
