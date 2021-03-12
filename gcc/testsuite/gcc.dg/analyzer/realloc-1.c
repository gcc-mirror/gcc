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
  void *p = malloc (1024);
  p = realloc (p, 4096);
  /* TODO: should warn about the leak when the above call fails (PR analyzer/99260).  */
  free (p);
}

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
  int *q = realloc (p, sizeof(int) * 4);
  *q = 43; /* { dg-warning "possibly-NULL 'q'" "PR analyzer/99260" { xfail *-*-* } } */
  return q;
}

void test_6 (size_t sz)
{
  void *p = realloc (NULL, sz);
} /* { dg-warning "leak of 'p'" } */
