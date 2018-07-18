/* PR sanitizer/79757 */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */

int
main (void)
{
  int
  div (int n)
  {
    __auto_type i = 5 / n;
    return i;
  }

  int
  shift (int n)
  {
    __auto_type i = 5 << n;
    return i;
  }

  int j = shift (100);
  int i = div (0);
  return 0;
}

/* { dg-output "shift exponent 100 is too large for \[^\n\r]*-bit type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*division by zero" } */
