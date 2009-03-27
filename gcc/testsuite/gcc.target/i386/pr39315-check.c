typedef float __m128 __attribute__ ((__vector_size__ (16)));

extern void foo (__m128 *);
extern void abort (void);

__m128 y = { 0.0, 1.0, 2.0, 3.0 };

void
bar (__m128 *x, int align)
{
  if ((((__PTRDIFF_TYPE__) x) & (align - 1)) != 0)
    abort ();
  if (__builtin_memcmp (x, &y, sizeof (y)) != 0)
    abort ();
}

int
main ()
{
  foo (&y);
  return 0;
}
