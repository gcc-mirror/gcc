/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=2 halt_on_error=1" } */
/* { dg-options "-fsanitize=address,pointer-subtract" } */

volatile __PTRDIFF_TYPE__ v;

void
bar (char *p, char *q)
{
  v = q - p;
  v = p - q;
}

char global[10000] = {};

int
main ()
{
  /* Heap allocated memory.  */
  char *p = (char *)__builtin_malloc (42);
  bar (p, p + 20);
  __builtin_free (p);

  /* Global variable.  */
  bar (&global[0], &global[100]);
  bar (&global[1000], &global[9000]);
  bar (&global[500], &global[10]);
  bar (&global[0], &global[10000]);

  /* Stack variable.  */
  char stack[10000];
  bar (&stack[0], &stack[100]);
  bar (&stack[1000], &stack[9000]);
  bar (&stack[500], &stack[10]);

  return 0;
}
