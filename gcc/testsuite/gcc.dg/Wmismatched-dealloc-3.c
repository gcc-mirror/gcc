/* Verify that Glibc <stdlib.h> declarations are handled correctly
   { dg-do compile }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((malloc (__VA_ARGS__), noipa))

typedef __SIZE_TYPE__ size_t;

/* All functions with the same standard deallocator are associated
   with each other.  */
void free (void*);
void* calloc (size_t, size_t);
void* malloc (size_t);
void* realloc (void*, size_t);

A (__builtin_free) void* aligned_alloc (size_t, size_t);

/* Like realloc(), reallocarray() is both an allocator and a deallocator.
   It must be associated with both free() and with itself, but nothing
   else.  */
A (__builtin_free) void* reallocarray (void*, size_t, size_t);
A (reallocarray) void* reallocarray (void*, size_t, size_t);

A (__builtin_free) extern char *canonicalize_file_name (const char*);


void dealloc (void*);
A (dealloc) void* alloc (size_t);


void sink (void*);
void* source (void);


void test_builtin_aligned_alloc (void *p)
{
  {
    void *q = __builtin_aligned_alloc (1, 2);
    sink (q);
    __builtin_free (q);
  }

  {
    void *q = __builtin_aligned_alloc (1, 2);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_aligned_alloc (1, 2);
    q = __builtin_realloc (q, 3);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_aligned_alloc (1, 2);
    q = realloc (q, 3);
    sink (q);
    free (q);
  }

  {
    void *q;
    q = __builtin_aligned_alloc (1, 2); // { dg-message "returned from '__builtin_aligned_alloc'" }
    sink (q);
    dealloc (q);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }
}


void test_aligned_alloc (void *p)
{
  {
    void *q = aligned_alloc (1, 2);
    sink (q);
    __builtin_free (q);
  }

  {
    void *q = aligned_alloc (1, 2);
    sink (q);
    free (q);
  }

  {
    void *q = aligned_alloc (1, 2);
    q = __builtin_realloc (q, 3);
    sink (q);
    free (q);
  }

  {
    void *q = aligned_alloc (1, 2);
    q = realloc (q, 3);
    sink (q);
    free (q);
  }

  {
    void *q = aligned_alloc (1, 2);     // { dg-message "returned from 'aligned_alloc'" }
    sink (q);
    dealloc (q);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }
}


void test_reallocarray (void *p)
{
  {
    void *q = __builtin_aligned_alloc (1, 2);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = aligned_alloc (1, 2);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_calloc (1, 2);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = calloc (1, 2);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_malloc (1);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = malloc (1);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_realloc (p, 1);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = realloc (p, 1);
    q = reallocarray (q, 2, 3);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_strdup ("abc");
    q = reallocarray (q, 3, 4);
    sink (q);
    free (q);
  }

  {
    void *q = __builtin_strndup ("abcd", 3);
    q = reallocarray (q, 4, 5);
    sink (q);
    free (q);
  }

  {
    void *q = source ();
    q = reallocarray (q, 5, 6);
    sink (q);
    free (q);
  }

  {
    void *q = alloc (1);                // { dg-message "returned from 'alloc'" }
    q = reallocarray (q, 6, 7);         // { dg-warning "'reallocarray' called on pointer returned from a mismatched allocation function" }
    sink (q);
    free (q);
  }

  {
    void *q = reallocarray (p, 7, 8);
    q = __builtin_realloc (q, 9);
    sink (q);
    free (q);
  }

  {
    void *q = reallocarray (p, 7, 8);
    q = realloc (q, 9);
    sink (q);
    free (q);
  }

  {
    void *q = reallocarray (p, 8, 9);
    q = reallocarray (q, 3, 4);
    sink (q);
    free (q);
  }

  {
    void *q = reallocarray (p, 9, 10);
    q = reallocarray (q, 3, 4);
    sink (q);
    dealloc (q);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }
}


void test_canonicalize_filename (void *p)
{
  {
    void *q = canonicalize_file_name ("a");
    sink (q);
    __builtin_free (q);
  }

  {
    void *q = canonicalize_file_name ("b");
    sink (q);
    free (q);
  }

  {
    void *q = canonicalize_file_name ("c");
    q = __builtin_realloc (q, 2);
    sink (q);
    free (q);
  }

  {
    void *q = canonicalize_file_name ("d");
    q = realloc (q, 3);
    sink (q);
    free (q);
  }

  {
    void *q = canonicalize_file_name ("e");
    q = reallocarray (q, 4, 5);
    sink (q);
    free (q);
  }

  {
    void *q;
    q = canonicalize_file_name ("f");   // { dg-message "returned from 'canonicalize_file_name'" }
    sink (q);
    dealloc (q);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }
}
