/* PR middle-end/91582 - missing heap overflow detection for strcpy
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#include "range.h"

#define INT_MAX     __INT_MAX__
#define INT_MIN     (-INT_MAX - 1)

#define ATTR(...)   __attribute__ ((__VA_ARGS__))
#define NOIPA       ATTR (noipa)

extern void* alloca (size_t);
extern void* calloc (size_t, size_t);
extern void* malloc (size_t);

extern ATTR (alloc_size (1), malloc) void*
  alloc1 (size_t, int);
extern ATTR (alloc_size (2), malloc) void*
  alloc2 (int, size_t);
extern ATTR (alloc_size (2, 4), malloc) void*
  alloc2_4 (int, size_t, int, size_t);

extern char* strcpy (char*, const char*);

void sink (void*);

#define S36 "0123456789abcdefghijklmnopqrstuvwxyz"
#define S(N) (S36 + sizeof S36 - N - 1)

#define T(src, alloc) do {			\
    char *s = src;				\
    char *d = alloc;				\
    strcpy (d, s);				\
    sink (d);					\
  } while (0)


NOIPA void test_strcpy_alloca (size_t n)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

  T (S (0), alloca (r_0_1));
  T (S (1), alloca (r_0_1));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloca (r_1_2));
  T (S (1), alloca (r_1_2));
  T (S (2), alloca (r_1_2));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloca (r_2_3));
  T (S (2), alloca (r_2_3));
  T (S (3), alloca (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), alloca (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }

  size_t r_2_smax = UR (2, SIZE_MAX);
  T (S (0), alloca (r_2_smax));
  T (S (1), alloca (r_2_smax));
  T (S (2), alloca (r_2_smax));
  T (S (3), alloca (r_2_smax * 2));
  T (S (4), alloca (r_2_smax * 2 + 1));

  T (S (1), alloca (n));
  T (S (2), alloca (n + 1));
  T (S (9), alloca (n * 2 + 1));

  int r_imin_imax = SR (INT_MIN, INT_MAX);
  T (S (1), alloca (r_imin_imax));
  T (S (2), alloca (r_imin_imax + 1));
  T (S (9), alloca (r_imin_imax * 2 + 1));

  int r_0_imax = SR (0, INT_MAX);
  T (S (1), alloca (r_0_imax));
  T (S (2), alloca (r_0_imax + 1));
  T (S (9), alloca (r_0_imax * 2 + 1));

  int r_1_imax = SR (1, INT_MAX);
  T (S (1), alloca (r_1_imax));
  T (S (2), alloca (r_1_imax + 1));
  T (S (9), alloca (r_1_imax * 2 + 1));

  ptrdiff_t r_dmin_dmax = SR (DIFF_MIN, DIFF_MAX);
  T (S (1), alloca (r_dmin_dmax));
  T (S (2), alloca (r_dmin_dmax + 1));
  T (S (9), alloca (r_dmin_dmax * 2 + 1));
}

NOIPA void test_strcpy_calloc (void)
{
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

  T (S (0), calloc (r_1_2, 1));
  T (S (1), calloc (r_1_2, 1));
  T (S (2), calloc (r_1_2, 1));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (2), calloc (r_2_3, 1));
  T (S (3), calloc (r_2_3, 1));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), calloc (1, r_1_2));
  T (S (1), calloc (1, r_1_2));
  T (S (2), calloc (1, r_1_2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (2), calloc (1, r_2_3));
  T (S (3), calloc (1, r_2_3));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), calloc (r_1_2, 2));
  T (S (1), calloc (r_1_2, 2));
  T (S (2), calloc (r_1_2, 2));
  T (S (3), calloc (r_1_2, 2));
  T (S (4), calloc (r_1_2, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), calloc (r_2_3, 2));
  T (S (1), calloc (r_2_3, 2));
  T (S (2), calloc (r_2_3, 2));
  T (S (5), calloc (r_2_3, 2));
  T (S (6), calloc (r_2_3, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), calloc (r_1_2, 2));
  T (S (1), calloc (r_1_2, 2));
  T (S (2), calloc (r_1_2, 2));
  T (S (3), calloc (r_1_2, 2));
  T (S (4), calloc (r_1_2, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), calloc (r_2_3, 2));
  T (S (1), calloc (r_2_3, 2));
  T (S (2), calloc (r_2_3, 2));
  T (S (5), calloc (r_2_3, 2));
  T (S (6), calloc (r_2_3, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), calloc (r_1_2, r_2_3));
  T (S (1), calloc (r_1_2, r_2_3));
  T (S (2), calloc (r_1_2, r_2_3));
  T (S (3), calloc (r_1_2, r_2_3));
  T (S (4), calloc (r_1_2, r_2_3));
  T (S (5), calloc (r_1_2, r_2_3));
  T (S (6), calloc (r_1_2, r_2_3));   // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), calloc (r_1_2, r_2_3));   // { dg-warning "\\\[-Wstringop-overflow" }

  size_t r_2_dmax = UR (2, DIFF_MAX);
  T (S (0), calloc (0, r_2_dmax));   // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (0), calloc (1, r_2_dmax));
  T (S (9), calloc (2, r_2_dmax));

  T (S (0), calloc (r_2_dmax, r_2_dmax));
  T (S (9), calloc (r_2_dmax, r_2_dmax));

  size_t r_2_smax = UR (2, SIZE_MAX);
  T (S (0), calloc (r_2_smax, 1));
  T (S (9), calloc (r_2_smax, 2));

  T (S (0), calloc (r_2_smax, r_2_smax));
  T (S (9), calloc (r_2_smax, r_2_smax));
}


NOIPA void test_strcpy_malloc (void)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

  T (S (0), malloc (r_0_1));
  T (S (1), malloc (r_0_1));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), malloc (r_1_2));
  T (S (1), malloc (r_1_2));
  T (S (2), malloc (r_1_2));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), malloc (r_2_3));
  T (S (2), malloc (r_2_3));
  T (S (3), malloc (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), malloc (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
}


NOIPA void test_strcpy_alloc1 (void)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

#define alloc1(n) alloc1 (n, 1)

  T (S (0), alloc1 (r_0_1));
  T (S (1), alloc1 (r_0_1));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc1 (r_1_2));
  T (S (1), alloc1 (r_1_2));
  T (S (2), alloc1 (r_1_2));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc1 (r_2_3));
  T (S (2), alloc1 (r_2_3));
  T (S (3), alloc1 (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), alloc1 (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
}

NOIPA void test_strcpy_alloc2 (void)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

#define alloc2(n) alloc2 (1, n)

  T (S (0), alloc1 (r_0_1));
  T (S (1), alloc1 (r_0_1));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc1 (r_1_2));
  T (S (1), alloc1 (r_1_2));
  T (S (2), alloc1 (r_1_2));      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc1 (r_2_3));
  T (S (2), alloc1 (r_2_3));
  T (S (3), alloc1 (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), alloc1 (r_2_3));      // { dg-warning "\\\[-Wstringop-overflow" }
}


NOIPA void test_strcpy_alloc2_4 (void)
{
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

#define alloc2_4(n1, n2) alloc2_4 (1, n1, 2, n2)

  T (S (0), alloc2_4 (r_1_2, 1));
  T (S (1), alloc2_4 (r_1_2, 1));
  T (S (2), alloc2_4 (r_1_2, 1));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (2), alloc2_4 (r_2_3, 1));
  T (S (3), alloc2_4 (r_2_3, 1));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc2_4 (1, r_1_2));
  T (S (1), alloc2_4 (1, r_1_2));
  T (S (2), alloc2_4 (1, r_1_2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (2), alloc2_4 (1, r_2_3));
  T (S (3), alloc2_4 (1, r_2_3));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc2_4 (r_1_2, 2));
  T (S (1), alloc2_4 (r_1_2, 2));
  T (S (2), alloc2_4 (r_1_2, 2));
  T (S (3), alloc2_4 (r_1_2, 2));
  T (S (4), alloc2_4 (r_1_2, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc2_4 (r_2_3, 2));
  T (S (1), alloc2_4 (r_2_3, 2));
  T (S (2), alloc2_4 (r_2_3, 2));
  T (S (5), alloc2_4 (r_2_3, 2));
  T (S (6), alloc2_4 (r_2_3, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc2_4 (r_1_2, 2));
  T (S (1), alloc2_4 (r_1_2, 2));
  T (S (2), alloc2_4 (r_1_2, 2));
  T (S (3), alloc2_4 (r_1_2, 2));
  T (S (4), alloc2_4 (r_1_2, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc2_4 (r_2_3, 2));
  T (S (1), alloc2_4 (r_2_3, 2));
  T (S (2), alloc2_4 (r_2_3, 2));
  T (S (5), alloc2_4 (r_2_3, 2));
  T (S (6), alloc2_4 (r_2_3, 2));   // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), alloc2_4 (r_1_2, r_2_3));
  T (S (1), alloc2_4 (r_1_2, r_2_3));
  T (S (2), alloc2_4 (r_1_2, r_2_3));
  T (S (3), alloc2_4 (r_1_2, r_2_3));
  T (S (4), alloc2_4 (r_1_2, r_2_3));
  T (S (5), alloc2_4 (r_1_2, r_2_3));
  T (S (6), alloc2_4 (r_1_2, r_2_3));   // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), alloc2_4 (r_1_2, r_2_3));   // { dg-warning "\\\[-Wstringop-overflow" }

  size_t r_2_dmax = UR (2, DIFF_MAX);
  T (S (0), alloc2_4 (r_2_dmax, r_2_dmax));
  T (S (9), alloc2_4 (r_2_dmax, r_2_dmax));

  size_t r_2_smax = UR (2, SIZE_MAX);
  T (S (0), alloc2_4 (r_2_smax, r_2_smax));
  T (S (9), alloc2_4 (r_2_smax, r_2_smax));
}

#undef T
#define T(T, src, n) do {			\
    char *s = src;				\
    T vla[n];					\
    char *d = (char*)vla;			\
    strcpy (d, s);				\
    sink (vla);					\
  } while (0)

NOIPA void test_strcpy_vla (void)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

  T (char, S (0), r_0_1);
  T (char, S (1), r_0_1);       // { dg-warning "\\\[-Wstringop-overflow" }

  T (char, S (0), r_1_2);
  T (char, S (1), r_1_2);
  T (char, S (2), r_1_2);       // { dg-warning "\\\[-Wstringop-overflow" }

  T (char, S (0), r_2_3);
  T (char, S (2), r_2_3);
  T (char, S (3), r_2_3);       // { dg-warning "\\\[-Wstringop-overflow" }
  T (char, S (9), r_2_3);       // { dg-warning "\\\[-Wstringop-overflow" }

#ifdef __INT16_TYPE__
  typedef __INT16_TYPE__ int16_t;

  T (int16_t, S (0), r_1_2);
  T (int16_t, S (2), r_1_2);
  T (int16_t, S (3), r_1_2);
  T (int16_t, S (4), r_1_2);    // { dg-warning "\\\[-Wstringop-overflow" }
  T (int16_t, S (5), r_1_2);    // { dg-warning "\\\[-Wstringop-overflow" }
  T (int16_t, S (9), r_1_2);    // { dg-warning "\\\[-Wstringop-overflow" }

  T (int16_t, S (0), r_2_3);
  T (int16_t, S (2), r_2_3);
  T (int16_t, S (3), r_2_3);
  T (int16_t, S (4), r_2_3);
  T (int16_t, S (5), r_2_3);
  T (int16_t, S (6), r_2_3);    // { dg-warning "\\\[-Wstringop-overflow" }
#endif

#ifdef __INT32_TYPE__
  typedef __INT32_TYPE__ int32_t;

  T (int32_t, S ( 0), r_2_3);
  T (int32_t, S ( 2), r_2_3);
  T (int32_t, S ( 3), r_2_3);
  T (int32_t, S ( 4), r_2_3);
  T (int32_t, S ( 5), r_2_3);
  T (int32_t, S ( 6), r_2_3);
  T (int32_t, S (11), r_2_3);
  T (int32_t, S (12), r_2_3);    // { dg-warning "\\\[-Wstringop-overflow" }
  T (int32_t, S (36), r_2_3);    // { dg-warning "\\\[-Wstringop-overflow" }
#endif
}


struct Flex
{
  char n, ax[];
};

#undef T
#define T(T, src, n) do {			\
    char *s = src;				\
    typedef struct { T n, ax[]; } Flex;		\
    Flex *p = (Flex*)malloc (sizeof *p + n);	\
    char *d = (char*)p->ax;			\
    strcpy (d, s);				\
    sink (p);					\
  } while (0)

NOIPA void test_strcpy_malloc_flexarray (void)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

  T (char, S (0), r_0_1);
  T (char, S (1), r_0_1);       // { dg-warning "\\\[-Wstringop-overflow" "pr92814" { xfail *-*-* } }

  T (char, S (0), r_1_2);
  T (char, S (1), r_1_2);
  T (char, S (2), r_1_2);       // { dg-warning "\\\[-Wstringop-overflow" "pr92814" { xfail *-*-* } }

  T (char, S (0), r_2_3);
  T (char, S (2), r_2_3);
  T (char, S (3), r_2_3);       // { dg-warning "\\\[-Wstringop-overflow" "pr92814" { xfail *-*-* } }
  T (char, S (9), r_2_3);       // { dg-warning "\\\[-Wstringop-overflow" "pr92814" { xfail *-*-* } }
}
