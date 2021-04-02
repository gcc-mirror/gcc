/* PR middle-end/94527 - Add an attribute that marks a function as freeing
   an object
   Verify that attribute malloc with one or two arguments has the expected
   effect on diagnostics.
   { dg-options "-Wall -ftrack-macro-expansion=0" } */

#define A(...) __attribute__ ((malloc (__VA_ARGS__), noipa))

typedef __SIZE_TYPE__ size_t;
typedef struct A A;
typedef struct B B;

/* A pointer returned by any of the four functions must be deallocated
   either by dealloc() or by realloc_{A,B}().  */
A (__builtin_free) A* alloc_A (int);
A (__builtin_free) B* alloc_B (int);
A (__builtin_free) A* realloc_A (A *p, int n) { return p; }
A (__builtin_free) B* realloc_B (B *p, int n) { return p; }

A (realloc_A) A* alloc_A (int);
A (realloc_B) B* alloc_B (int);
A (realloc_A) A* realloc_A (A*, int);
A (realloc_B) B* realloc_B (B*, int);

void dealloc (void*);
A (dealloc) void* alloc (int);

void sink (void*);

void test_alloc_A (void)
{
  {
    void *p = alloc_A (1);
    p = realloc_A (p, 2);
    __builtin_free (p);
  }

  {
    void *p = alloc_A (1);
    /* Verify that calling realloc doesn't trigger a warning even though
       alloc_A is not directly associated with it.  */
    p = __builtin_realloc (p, 2);
    sink (p);
  }

  {
    void *p = alloc_A (1);              // { dg-message "returned from 'alloc_A'" }
    dealloc (p);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }

  {
    /* Because alloc_A() and realloc_B() share free() as a deallocator
       they must also be valid as each other's deallocators.  */
    void *p = alloc_A (1);
    p = realloc_B ((B*)p, 2);
    __builtin_free (p);
  }

  {
    void *p = alloc_A (1);
    p = realloc_A (p, 2);
    p = __builtin_realloc (p, 3);
    __builtin_free (p);
  }
}


void test_realloc_A (void *ptr)
{
  {
    void *p = realloc_A (0, 1);
    p = realloc_A (p, 2);
    __builtin_free (p);
  }

  {
    void *p = realloc_A (ptr, 2);
    p = realloc_A (p, 2);
    __builtin_free (p);
  }

  {
    void *p = realloc_A (0, 3);
    p = __builtin_realloc (p, 2);
    sink (p);
  }

  {
    void *p = realloc_A (0, 4);         // { dg-message "returned from 'realloc_A'" }
    dealloc (p);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }

  {
    /* Because realloc_A() and realloc_B() share free() as a deallocator
       they must also be valid as each other's deallocators.  */
    void *p = realloc_A (0, 5);
    p = realloc_B ((B*)p, 2);
    __builtin_free (p);
  }

  {
    void *p = realloc_A (0, 6);
    p = realloc_A ((A*)p, 2);
    p = __builtin_realloc (p, 3);
    __builtin_free (p);
  }
}


void test_realloc (void *ptr)
{
  extern void free (void*);
  extern void* realloc (void*, size_t);

  {
    void *p = realloc (ptr, 1);
    p = realloc_A (p, 2);
    __builtin_free (p);
  }

  {
    void *p = realloc (ptr, 2);
    p = realloc_A (p, 2);
    free (p);
  }

  {
    void *p = realloc (ptr, 3);
    free (p);
  }

  {
    void *p = realloc (ptr, 4);
    __builtin_free (p);
  }

  {
    void *p = realloc (ptr, 5);         // { dg-message "returned from 'realloc'" }
    dealloc (p);                        // { dg-warning "'dealloc' called on pointer returned from a mismatched allocation function" }
  }
}
