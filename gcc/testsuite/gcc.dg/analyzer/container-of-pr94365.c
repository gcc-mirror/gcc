/* Regression test for leak false positive (PR analyzer/94365).  */

#define NULL ((void*)0)

struct a_struct {
  int a;
};

struct container {
  void *a;
  struct a_struct b;
};

struct a_struct *
foo()
{
  struct container *a = __builtin_malloc (sizeof(*a));
  if (!a)
    return NULL;
  return &a->b; /* { dg-bogus "leak of 'a'" "PR analyzer/94365" } */
}

#define container_of(ptr, type, member) \
    (type *)((char *)(ptr) - (char *) &((type *)0)->member)

int
test (void)
{
  struct a_struct *b = foo();
  struct container *a;

  if (!b)
    return -1;

  __builtin_free (container_of (b, struct container, b));

  return 0;
}
