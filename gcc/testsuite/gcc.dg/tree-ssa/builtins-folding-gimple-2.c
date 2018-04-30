/* PR 81908 - FAIL: gfortran.dg/alloc_comp_auto_array_2.f90 -O3 -g -m32
   Test to verify that calls to memcpy et al. where the size is in a range
   with just one valid value -- zero -- are eliminated.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#define INT_MAX    __INT_MAX__
#define SHRT_MAX   __SHRT_MAX__
#define SIZE_MAX   __SIZE_MAX__
#define SSIZE_MAX  (SIZE_MAX / 2)

typedef __PTRDIFF_TYPE__ ssize_t;
typedef __SIZE_TYPE__    size_t;

#define UNIQUE_FUNCNAME(func, line) test_ ## func ## _ ## line
#define FUNCNAME(func, line)        UNIQUE_FUNCNAME (func, line)

#define AR(func, type, min, max, val)					\
  void __attribute__ ((noclone, noinline))				\
  FUNCNAME (func, __LINE__) (char *d, const char *s, type n)		\
  {									\
    if ((type)min <= n && n <= (type)max)				\
      n = val;								\
    __builtin_ ## func (d, s, n);					\
  } typedef void DummyType

AR (memcpy, short, 1, SHRT_MAX, 0);
AR (memcpy, int,   1, INT_MAX, 0);
AR (memcpy, size_t,  1, SSIZE_MAX, 0);
AR (memcpy, ssize_t, 1, SSIZE_MAX, 0);

AR (memmove, short, 1, SHRT_MAX, 0);
AR (memmove, int,   1, INT_MAX, 0);
AR (memmove, ssize_t, 1, SSIZE_MAX, 0);
AR (memmove, ssize_t, 1, SSIZE_MAX, 0);

AR (mempcpy, short, 1, SHRT_MAX, 0);
AR (mempcpy, int,   1, INT_MAX, 0);
AR (mempcpy, size_t,  1, SSIZE_MAX, 0);
AR (mempcpy, ssize_t, 1, SSIZE_MAX, 0);

/* { dg-final { scan-tree-dump-not "builtin_memcpy" "optimized" } }
   { dg-final { scan-tree-dump-not "builtin_memmove" "optimized" } }
   { dg-final { scan-tree-dump-not "builtin_mempcpy" "optimized" } }  */
