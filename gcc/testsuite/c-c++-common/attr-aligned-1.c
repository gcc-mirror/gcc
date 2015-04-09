/* PR c++/65690 */
/* { dg-do run } */

typedef double T[4][4] __attribute__((aligned (2 * __alignof__ (double))));
void foo (const T);
struct S { T s; };

int
main ()
{
  if (__alignof__ (struct S) != 2 * __alignof__ (double)
      || __alignof__ (T) != 2 * __alignof__ (double)
      || __alignof__ (const struct S) != 2 * __alignof__ (double)
      || __alignof__ (const T) != 2 * __alignof__ (double))
    __builtin_abort ();
  return 0;
}

#if defined(__cplusplus) && __cplusplus >= 201103L
static_assert (alignof (S) == 2 * alignof (double), "alignment of S");
static_assert (alignof (T) == 2 * alignof (double), "alignment of T");
static_assert (alignof (const S) == 2 * alignof (double), "alignment of const S");
static_assert (alignof (const T) == 2 * alignof (double), "alignment of const T");
#endif
