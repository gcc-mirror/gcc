/* Verify that casts between pointers and integers don't trigger false
   positives.  Test derived from Glibc's _dl_allocate_tls_storage() in
   dl-tls.c.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

typedef __SIZE_TYPE__ size_t;
typedef __UINTPTR_TYPE__ uintptr_t;

size_t a;
size_t s;

void* _dl_allocate_tls_storage (void)
{
  void *p = __builtin_malloc (s + a + sizeof (void *));

  char *q = (char *)(__builtin_constant_p (a) && (((a - 1) & a) == 0)
             ? ((((uintptr_t)p) + a - 1) & ~(a - 1))
             : (((((uintptr_t)p) + (a - 1)) / a) * a));

  char *r = q + s - sizeof (int[4]);
  __builtin_memset (r, '\0', sizeof (int[4]));
  return r;
}
