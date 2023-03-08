/* PR sanitizer/109050 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds -fno-sanitize-recover=all" } */
/* { dg-shouldfail "ubsan" } */

long a;
int b;
int
main ()
{
  int c[4] = {0, 1, 2, 3};
  a = 0;
  c[a - 9806816] |= b;
}

/* { dg-output "index -9806816 out of bounds for type 'int \\\[4\\\]'" } */
