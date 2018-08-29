/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=2 halt_on_error=1" } */
/* { dg-options "-fsanitize=address,pointer-compare" } */

volatile int v;

int
foo (char *p)
{
  char *p2 = p + 20;
  v = p > p2;
  return v;
}

void
bar (char *p, char *q)
{
  v = p <= q;
}

void
baz (char *p, char *q)
{
  v = (p != 0 && p < q);
}

char global[8192] = {};
char small_global[7] = {};

int
main ()
{
  /* Heap allocated memory.  */
  char *p = (char *)__builtin_malloc (42);
  int r = foo (p);
  __builtin_free (p);

  p = (char *)__builtin_malloc (1024);
  bar (p, p + 1024);
  bar (p + 1024, p + 1023);
  bar (p + 1, p + 1023);
  __builtin_free (p);

  p = (char *)__builtin_malloc (4096);
  bar (p, p + 4096);
  bar (p + 10, p + 100);
  bar (p + 1024, p + 4096);
  bar (p + 4095, p + 4096);
  bar (p + 4095, p + 4094);
  bar (p + 100, p + 4096);
  bar (p + 100, p + 4094);
  __builtin_free (p);

  /* Global variable.  */
  bar (&global[0], &global[1]);
  bar (&global[1], &global[2]);
  bar (&global[2], &global[1]);
  bar (&global[0], &global[100]);
  bar (&global[1000], &global[7000]);
  bar (&global[500], &global[10]);
  p = &global[0];
  bar (p, p + 8192);
  p = &global[8000];
  bar (p, p + 192);

  p = &small_global[0];
  bar (p, p + 1);
  bar (p, p + 7);
  bar (p + 7, p + 1);
  bar (p + 6, p + 7);
  bar (p + 7, p + 7);

  /* Stack variable.  */
  char stack[10000];
  bar (&stack[0], &stack[100]);
  bar (&stack[1000], &stack[9000]);
  bar (&stack[500], &stack[10]);

  baz (0, &stack[10]);

  return 0;
}
