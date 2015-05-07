/* PR sanitizer/65280 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

void
foo (int n, int (*b)[n])
{
  (*b)[n] = 1;
}

int
main ()
{
  int a[20];
  foo (3, (int (*)[3]) &a);
}

/* { dg-output "index 3 out of bounds for type 'int \\\[\\\*\\\]'" } */
