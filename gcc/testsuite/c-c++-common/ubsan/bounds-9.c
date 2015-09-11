/* PR sanitizer/65280 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* Origin: Martin Uecker <uecker@eecs.berkeley.edu> */

void
foo (volatile int (*a)[3])
{
  (*a)[3] = 1;	// error
  a[0][0] = 1;	// ok
  a[1][0] = 1;	// ok
  a[1][4] = 1;	// error
}

int
main ()
{
  volatile int a[20];
  foo ((int (*)[3]) &a);
  return 0;
}

/* { dg-output "index 3 out of bounds for type 'int \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 4 out of bounds for type 'int \\\[3\\\]'" } */
