/* PR middle-end/98166: bogus -Wmismatched-dealloc on user-defined allocator
   and inlining
   Verify that without inlining, both the allocator and the deallocator
   can be declared inline without a warning and that mismatched calls are
   detected, but that declaring them always_inline does trigger a warning.
   { dg-do compile }
   { dg-options "-Wall" } */

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
    __builtin_free (p + 1);   // { dg-warning "'__builtin_free|void __builtin_free\\(void\\*\\)' called on pointer 'p|<unknown>' with nonzero offset" }
  }
}


inline void
dealloc_long (long *p) { __builtin_free (p); }

__attribute__ ((malloc (dealloc_long)))
long* alloc_long (int);

void test_nowarn_long (int n)
{
  {
    long *p = alloc_long (n);
    dealloc_long (p);
  }

  {
    long *p = alloc_long (n);
    dealloc_long (p + 1);     // { dg-warning "'dealloc_long' called on pointer 'p|<unknown>' with nonzero offset" }
  }
}


inline __attribute__ ((always_inline)) void
dealloc_float (float *p)      // { dg-message "deallocation function declared here" }
{
  __builtin_free (p);         // { dg-warning "'__builtin_free|void __builtin_free\\(void\\*\\)' called on pointer 'p|<unknown>' with nonzero offset" }
}

__attribute__ ((malloc (dealloc_float)))
float* alloc_float (int);     // { dg-warning "'malloc \\(dealloc_float\\)' attribute ignored with deallocation functions declared 'inline'" }

void test_nowarn_float (int n)
{
  {
    float *p = alloc_float (n);
    dealloc_float (p);
  }

  {
    float *p = alloc_float (n);
    dealloc_float (p + 2);
  }
}
