/* PR sanitizer/87837 */
/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */

int
foo (int n)
{
  return n + __INT_MAX__ < n;
}

int
main ()
{
  volatile int a = foo (1);
  return 0;
}

/* { dg-output "signed integer overflow: 1 \\+ 2147483647 cannot be represented in type 'int'" } */
