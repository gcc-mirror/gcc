/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void f1 (char *p, __SIZE_TYPE__ sz)
{
  char *q = __builtin_memchr (p, 0, sz);
  __PTRDIFF_TYPE__ n = q - p;

  if (n >= __PTRDIFF_MAX__)
    __builtin_abort ();
}

void f2 (unsigned char *p, __SIZE_TYPE__ sz)
{
  unsigned char *q = __builtin_memchr (p, 0, sz);
  __PTRDIFF_TYPE__ n = q - p;

  if (n >= __PTRDIFF_MAX__)
    __builtin_abort ();
}

void f3 (signed char *p, __SIZE_TYPE__ sz)
{
  signed char *q = __builtin_memchr (p, 0, sz);
  __PTRDIFF_TYPE__ n = q - p;

  if (n >= __PTRDIFF_MAX__)
    __builtin_abort ();
}


/* { dg-final { scan-tree-dump-not "memchr" "optimized" } } */
