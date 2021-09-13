/* { dg-additional-options "-Wno-analyzer-too-complex" } */
/* TODO: remove the need for this option (PR analyzer/93695).  */

#define NELEMS 10
#define ARRAY_SIZE(a) (sizeof (a) / sizeof (a[0]))

void
test_1 (void)
{
  int *p[NELEMS];
  int i;

  for (i = 0; i < ARRAY_SIZE (p); ++i)
    p[i] = __builtin_malloc (sizeof (i));

  for (i = 0; i < ARRAY_SIZE (p); ++i)
    __builtin_free (p [i]);
}

void
test_2 (int n)
{
  int **p;
  int i;

  p = (int **)__builtin_malloc (sizeof (int *) * n);
  if (!p)
    return;

  for (i = 0; i < n; ++i)
    p[i] = __builtin_malloc (sizeof (i));

  for (i = 0; i < n; ++i)
    __builtin_free (p [i]);

  __builtin_free (p);
}

void
test_3 (int **p, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    p[i] = __builtin_malloc (sizeof (i));
}

void
test_4 (void **p, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    __builtin_free (p[i]);
}
