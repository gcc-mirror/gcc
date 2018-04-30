/* PR 81908 - FAIL: gfortran.dg/alloc_comp_auto_array_2.f90 -O3 -g -m32
   Test to verify that calls to memcpy et al. where the size is in a range
   with more than one valid value are not eliminated (this test complements
   builtins-folding-gimple-2.c).
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#define SHRT_MAX   __SHRT_MAX__
#define SHRT_MIN   (-SHRT_MAX - 1)
#define INT_MAX    __INT_MAX__
#define INT_MIN    (-INT_MAX - 1)

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

AR (memcpy, short, SHRT_MIN, 0, 1);
AR (memcpy, short, SHRT_MIN, 1, 2);
AR (memcpy, short, 2, SHRT_MAX, 1);

AR (memcpy, int, INT_MIN, 0, 1);
AR (memcpy, int, INT_MIN, 1, 2);
AR (memcpy, int, INT_MIN, 2, 3);
AR (memcpy, int, 2, INT_MAX, 1);
AR (memcpy, int, 2, INT_MAX, 1);

AR (memmove, short, 2, SHRT_MAX, 1);
AR (memmove, int,   2, INT_MAX, 1);

AR (mempcpy, short, 2, SHRT_MAX, 1);
AR (mempcpy, int,   2, INT_MAX, 1);

/* { dg-final { scan-tree-dump-times "builtin_memcpy" 8 "optimized" } }
   { dg-final { scan-tree-dump-times "builtin_memmove" 2 "optimized" } }
   { dg-final { scan-tree-dump-times "builtin_mempcpy" 2 "optimized" } }  */
