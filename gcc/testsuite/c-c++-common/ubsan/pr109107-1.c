/* PR sanitizer/109107 */
/* { dg-do run { target int32 } } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

#define INT_MIN (-__INT_MAX__ - 1)
int a = INT_MIN;
const int b = 676540;

__attribute__((noipa)) int
foo ()
{
  int c = a + 1 - (int) (short) b;
  return c;
}

int
main ()
{
  foo ();
  return 0;
}

/* { dg-output "signed integer overflow: -2147483647 - 21180 cannot be represented in type 'int'" } */
