/* PR c/78284 - warn on malloc with very large arguments
   Test verifying that the built-in allocation functions are declared
   with attribute malloc.  This means that the pointer they return
   can be assumed not to alias any other valid pointer.  */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void sink (void*);

extern int x;

#define TEST(call)				\
  do {						\
    p = call;					\
    x = 123;					\
    *(int*)p = 456;				\
    (x == 123) ? (void)0 : __builtin_abort ();	\
    sink (p);					\
  } while (0)

void test (void *p, unsigned n)
{
  TEST (__builtin_aligned_alloc (8, n));
  TEST (__builtin_alloca (n));
  TEST (__builtin_calloc (4, n));
  TEST (__builtin_malloc (n));
  TEST (__builtin_realloc (p, n + 1));
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
