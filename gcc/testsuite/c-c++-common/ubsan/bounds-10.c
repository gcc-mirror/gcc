/* { dg-do run } */
/* { dg-options "-fsanitize=bounds-strict" } */

struct V { int l; int a[1]; };

int
main (void)
{
  /* For strict, do instrument last array in a struct.  */
  struct V *v = (struct V *) __builtin_malloc (sizeof (struct V) + 10);
  v->a[1] = 1;

  return 0;
}

/* { dg-output "index 1 out of bounds for type 'int \\\[1\\\]'" } */
