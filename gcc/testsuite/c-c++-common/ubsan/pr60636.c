/* PR sanitizer/60636 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

volatile long long int a;

int
main ()
{
  long long int u = -__LONG_LONG_MAX__ - 1;
  a = u > 0 ? u : -u;
  return 0;
}

/* { dg-output "negation of -9223372036854775808 cannot be represented in type 'long long int'" } */
