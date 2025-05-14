/* PR c/78668 - aligned_alloc, realloc, et al. missing attribute alloc_size
   Test to verify that memory allocation built-ins are decorated with
   attribute alloc_size that __builtin_object_size can make use of (or
   are treated as if they were for that purpose)..
   { dg-do compile }
   { dg-additional-options "-O2 -fdump-tree-optimized" } */

void sink (void*);

static unsigned size (unsigned n)
{
  return n;
}

void test_aligned_alloc (unsigned a)
{
  unsigned n = size (7);

  void *p = __builtin_aligned_alloc (a, n);
  if (__builtin_object_size (p, 0) != n)
    __builtin_abort ();
  sink (p);
}

void test_alloca (void)
{
  unsigned n = size (13);

  void *p = __builtin_alloca (n);

  /* Also verify that alloca is declared with attribute returns_nonnull
     (or treated as it were as the case may be).  */
  if (!p)
    __builtin_abort ();

  if (__builtin_object_size (p, 0) != n)
    __builtin_abort ();
  sink (p);
}

void test_calloc (void)
{
  unsigned m = size (19);
  unsigned n = size (23);

  void *p = __builtin_calloc (m, n);
  if (__builtin_object_size (p, 0) != m * n)
    __builtin_abort ();
  sink (p);
}

void test_malloc (void)
{
  unsigned n = size (17);

  void *p = __builtin_malloc (n);
  if (__builtin_object_size (p, 0) != n)
    __builtin_abort ();
  sink (p);
}

void test_realloc (void *p)
{
  unsigned n = size (31);

  p = __builtin_realloc (p, n);
  if (__builtin_object_size (p, 0) != n)
    __builtin_abort ();
  sink (p);
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
