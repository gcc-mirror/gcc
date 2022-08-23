/* { dg-additional-options "-Wno-free-nonheap-object" } */

typedef __SIZE_TYPE__ size_t;

#define NULL ((void *)0)

extern void *malloc (size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__malloc__))
  __attribute__ ((__alloc_size__ (1)));
extern void *realloc (void *__ptr, size_t __size)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__warn_unused_result__))
  __attribute__ ((__alloc_size__ (2)));
extern void free (void *__ptr)
  __attribute__ ((__nothrow__ , __leaf__));

void *test_1 (void *ptr)
{
  return realloc (ptr, 1024);
}

void *test_2 (void *ptr)
{
  void *p = malloc (1024); /* { dg-message "allocated here" } */
  p = realloc (p, 4096); /* { dg-message "when 'realloc' fails" } */
  free (p);
} /* { dg-warning "leak of 'p'" } */ // ideally this would be on the realloc stmt

void *test_3 (void *ptr)
{
  void *p = malloc (1024);
  void *q = realloc (p, 4096);
  if (q)
    free (q);
  else
    free (p);
}

void *test_4 (void)
{
  return realloc (NULL, 1024);
}

int *test_5 (int *p)
{
  *p = 42;
  int *q = realloc (p, sizeof(int) * 4); /* { dg-message "when 'realloc' fails" } */
  *q = 43; /* { dg-warning "dereference of NULL 'q'" } */
  return q;
}

void test_6 (size_t sz)
{
  void *p = realloc (NULL, sz);
} /* { dg-warning "leak of 'p'" } */

/* The analyzer should complain about realloc of non-heap.  */

void *test_7 (size_t sz)
{
  char buf[100]; /* { dg-message "region created on stack here" } */
  void *p = realloc (&buf, sz); /* { dg-warning "'realloc' of '&buf' which points to memory on the stack" } */
  return p;  
}

/* Mismatched allocator.  */

struct foo
{
  int m_int;
};

extern void foo_release (struct foo *);
extern struct foo *foo_acquire (void)
  __attribute__ ((malloc (foo_release)));

void test_8 (void)
{
  struct foo *p = foo_acquire ();
  void *q = realloc (p, 1024); /* { dg-warning "'p' should have been deallocated with 'foo_release' but was deallocated with 'realloc'" } */
}

/* We should complain about realloc on a freed pointer.  */

void test_9 (void *p)
{
  free (p);
  void *q = realloc (p, 1024); /* { dg-warning "double-'free' of 'p'" } */
}

void test_10 (char *s, int n)
{
  __builtin_realloc(s, n); /* { dg-warning "ignoring return value of '__builtin_realloc' declared with attribute 'warn_unused_result'" } */
} /* { dg-warning "leak" } */
