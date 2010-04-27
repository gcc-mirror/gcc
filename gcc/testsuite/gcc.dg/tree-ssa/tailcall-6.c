/* PR tree-optimization/43904.  */
/* { dg-do run } */
/* { dg-options "-O1 -foptimize-sibling-calls" } */

typedef __SIZE_TYPE__ size_t;
extern void abort(void);

void *memcpy(void *dest, const void *src, size_t n);

void
buggy_init(void *ptr, size_t size)
{
  const char *str = "Hello world!";
  memcpy(ptr, &str, size);
}

void
expose_bug(void *ptr, size_t size)
{
  const char *str;
  memcpy(&str, ptr, size);
  if (*str != 'H')
    abort ();
}

int
main()
{
  const char *ptr;
  buggy_init(&ptr, sizeof(ptr));
  expose_bug(&ptr, sizeof(ptr));
  return 0;
}
