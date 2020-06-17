/* PR tree-optimization/93210 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return \[0-9]\[0-9a-fA-FxX]*;" 31 "optimized" } } */

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 L;
#else
typedef unsigned long long L;
#endif
struct S { signed char a, b; unsigned char c; };
struct T { signed char d; struct S e[25]; signed char f; };
union U { struct T g; L h[10]; };
const union U u = { { 1, { { 2, 3, 4 }, { 5, 6, 7 }, { 8, 9, 10 },
                           { 12, 13, 14 }, { 15, 16, 17 }, { 18, 19, 20 },
                           { 22, 23, 24 }, { 25, 26, 27 }, { 28, 29, 30 },
                           { 32, 33, 34 }, { 35, 36, 37 }, { 38, 39, 40 },
                           { 42, 43, 44 }, { 45, 46, 47 }, { 48, 49, 50 },
                           { 52, 53, 54 }, { 55, 56, 57 }, { 58, 59, 60 },
                           { 62, 63, 64 }, { 65, 66, 67 }, { 68, 69, 70 },
                           { 72, 73, 74 }, { 75, 76, 77 }, { 78, 79, 80 },
                           { 82, 83, 84 } }, 85 } };
const union U v = { { 1, { { 2, 3, 4 }, [1 ... 23] = { 5, 6, 7 },
			   { 8, 9, 10 } }, 86 } };
struct A { char a[5]; char b[16]; char c[7]; };
union V { struct A d; unsigned int e[10]; };
const union V w = { { "abcde", "ijkl", "mnopqr" } };
#define N(n) __attribute__((noipa)) L foo##n (void) { return u.h[n]; }
#define M N(0) N(1) N(2) N(3) N(4) N(5) N(6) N(7) N(8) N(9)
M
#undef N
#define N(n) __attribute__((noipa)) L bar##n (void) { return v.h[n]; }
M
#undef N
#define N(n) __attribute__((noipa)) L baz##n (void) { return w.e[n]; }
M

typedef L (*F) (void);
F arr[30] = {
#undef N
#define N(n) foo##n,
M
#undef N
#define N(n) bar##n,
M
#undef N
#define N(n) baz##n,
M
};

int
main ()
{
  const union U *p = &u;
  const union U *q = &v;
  const union V *r = &w;
  __asm ("" : "+g" (p));
  __asm ("" : "+g" (q));
  __asm ("" : "+g" (r));
  for (int i = 0; i < 10; i++)
    if (arr[i] () != p->h[i]
	|| arr[i + 10] () != q->h[i]
	|| arr[i + 20] () != r->e[i])
      __builtin_abort ();
  return 0;
}
