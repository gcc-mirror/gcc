/* PR sanitizer/65280 */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

int
main (void)
{
  int *t = (int *) __builtin_malloc (sizeof (int) * 10);
  int (*a)[1] = (int (*)[1]) t;
  (*a)[2] = 1;
}

/* { dg-output "index 2 out of bounds for type 'int \\\[1\\\]'" } */
