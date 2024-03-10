/* PR middle-end/98166: bogus -Wmismatched-dealloc on user-defined allocator
   and inlining
   Verify that the allocator can be declared inline without a warning when
   it's associated with a standard deallocator.  Associating an inline
   deallocator with an allocator would cause false positives when the former
   calls a deallocation function the allocator isn't associated with, so
   that triggers a warning on declaration.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

__attribute__ ((malloc (__builtin_free)))
inline int*
alloc_int (int n)
{
  return (int*)__builtin_malloc (n + sizeof (int));
}

void test_nowarn_int (int n)
{
  {
    int *p = alloc_int (n);
    __builtin_free (p);
  }

  {
    int *p = alloc_int (n);
    __builtin_free (p + 1);   // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
}


inline void
dealloc_long (long *p)
{
  __builtin_free (p);         // { dg-warning "'(__builtin_free|void __builtin_free\\(void\\*\\))' called on pointer '(p|<unknown>)' with nonzero offset" }
}

__attribute__ ((malloc (dealloc_long)))
long* alloc_long (int);       // { dg-warning "'malloc \\\(dealloc_long\\\)' attribute ignored with deallocation functions declared 'inline'" }

void test_nowarn_long (int n)
{
  {
    long *p = alloc_long (n);
    dealloc_long (p);
  }

  {
    long *p = alloc_long (n);
    dealloc_long (p + 1);
  }
}
