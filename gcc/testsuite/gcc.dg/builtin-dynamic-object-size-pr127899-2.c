/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;
static char pool[64];
static size_t used;

/* Hand out consecutive pieces of POOL.  */
__attribute__ ((alloc_size (1), noinline)) char *
pool_alloc (size_t n)
{
  char *p = pool + used;

  used += n;
  return p;
}

int
main (int argc, char **argv)
{
  (void) argv;
  size_t n = argc + 7;
  char *a = pool_alloc (8);
  char *b = pool_alloc (n);
  /* A again: B - 8 is &pool[0].  */
  char *before = b - 8;

  __builtin_printf ("%d %zu\n", before == a, __builtin_dynamic_object_size (before, 2));
  return 0;
}
