/* PR debug/36728 */
/* { dg-do run } */
/* { dg-options "-g" } */

int __attribute__((noinline))
foo (int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7)
{
  char *x = __builtin_alloca (arg7);
  int __attribute__ ((aligned(32))) y;

  y = 2;
  asm volatile ("" : "=m" (y) : "m" (y));
  x[0] = 25;
  asm volatile ("" : "=m" (x[0]) : "m" (x[0]));
  return y;
}

/* { dg-final { gdb-test 12 "arg1" "1" } } */
/* { dg-final { gdb-test 12 "arg2" "2" } } */
/* { dg-final { gdb-test 12 "arg3" "3" } } */
/* { dg-final { gdb-test 12 "arg4" "4" } } */
/* { dg-final { gdb-test 12 "arg5" "5" } } */
/* { dg-final { gdb-test 12 "arg6" "6" } } */
/* { dg-final { gdb-test 12 "arg7" "30" } } */
/* { dg-final { gdb-test 12 "y" "2" } } */
/* { dg-final { gdb-test 14 "arg1" "1" } } */
/* { dg-final { gdb-test 14 "arg2" "2" } } */
/* { dg-final { gdb-test 14 "arg3" "3" } } */
/* { dg-final { gdb-test 14 "arg4" "4" } } */
/* { dg-final { gdb-test 14 "arg5" "5" } } */
/* { dg-final { gdb-test 14 "arg6" "6" } } */
/* { dg-final { gdb-test 14 "arg7" "30" } } */
/* { dg-final { gdb-test 14 "*x" "(char) 25" } } */
/* { dg-final { gdb-test 14 "y" "2" } } */

int
main ()
{
  int l = 0;
  asm volatile ("" : "=r" (l) : "0" (l));
  foo (l + 1, l + 2, l + 3, l + 4, l + 5, l + 6, l + 30);
  return 0;
}
