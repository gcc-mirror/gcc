/* { dg-do run { target { posix_memalign } } } */

typedef __SIZE_TYPE__ size_t;
extern int posix_memalign(void **memptr, size_t alignment, size_t size);
extern void abort(void);
int
main (void)
{
  void *p;
  int ret;

  p = (void *)&ret;
  ret = posix_memalign (&p, sizeof (void *), -1);
  if (p != (void *)&ret)
    abort ();
  return 0;
}
