/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef struct A { int i; double d; } A;

void f1 (const char *c)
{
  A *s = (A*) __builtin_malloc (sizeof (A));
  double *p = &s->d;
  s->i = 42;
  __builtin_memcpy (p, c, sizeof (double));
  int j = s->i;
  if (j != 42) __builtin_abort();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

