/* { dg-do compile } */

struct X { };

static inline void *
bar (void *dst, void *src)
{
  return __builtin___memcpy_chk (dst, src, sizeof (struct X),
				 __builtin_object_size (dst, 0));
}

struct X
foo (struct X *x)
{
  struct X any;
  bar (&any, x);
  return any;
}
