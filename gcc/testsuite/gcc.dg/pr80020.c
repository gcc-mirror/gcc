/* PR middle-end/80020 - gcc confused about aligned_alloc argument order
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

void sink (void*);

void foo (void)
{
  enum {
    Align = 32,
    Size = 123
  };

  void *p = __builtin_aligned_alloc (Align, Size);
  unsigned n = __builtin_object_size (p, 0);

  if (n != Size)
    __builtin_abort ();

  __builtin___memset_chk (p, 0, Size, n);

  sink (p);
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } }
   { dg-final { scan-tree-dump-not "memset_chk" "optimized" } } */
