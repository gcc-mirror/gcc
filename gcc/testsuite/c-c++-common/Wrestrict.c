/* PR 35503 - Warn about restricted pointers
   { dg-do compile }
   { dg-options "-O2 -Wrestrict -ftrack-macro-expansion=0" } */

#include "../gcc.dg/range.h"

#if !defined LINE
# define LINE 0
#endif

#if __cplusplus
#  define restrict __restrict
extern "C" {
#endif

extern void* memcpy (void* restrict, const void* restrict, size_t);
extern void* mempcpy (void* restrict, const void* restrict, size_t);
extern void* memmove (void*, const void*, size_t);

extern char* stpcpy (char* restrict, const char* restrict);

extern char* strcat (char* restrict, const char* restrict);
extern char* strcpy (char* restrict, const char* restrict);
extern char* strncpy (char* restrict, const char* restrict, size_t);

#if __cplusplus
}   /* extern "C" */
#endif

void sink (void*, ...);

struct MemArrays
{
  char a8[8];
  char a16[16];
  char ax[];
};

/* Exercise memcpy with constant or known arguments.  */

void test_memcpy_cst (void *d, const void *s)
{
#undef T
#define T(dst, src, n) do {				\
    if (!LINE || LINE == __LINE__) {			\
      char a[9] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };	\
      void *pd = (dst);					\
      const void *ps = (src);				\
      memcpy (pd, ps, (n));				\
      sink (a, pd, ps);					\
    }							\
  } while (0)

  T (a, a, 0);

  /* This isn't detected because memcpy calls with size of 1 are
     intentionally folded into safe copies equivalent to memmove,
     regardless of the target (larger power-of-2 copies may or
     may not be folded depending on the target -- see non_strict_align
     below, for example).
     It's marked xfail only because there is value in detecting such
     invalid calls for portability, and as a reminder of why it isn't
     diagnosed.  */
  T (a, a + 1, 1);           /* { dg-warning "\\\[-Wrestrict" "memcpy with a small power of 2 size" { xfail *-*-* } } */
  T (a, a + 3, 3);
  T (a, a + 3, 5);           /* { dg-warning "\\\[-Wrestrict" "memcpy" } */

  {
    char a[3][7];
    sink (a);

    void *d = a[0];
    const void *s = a[1];
    memcpy (d, s, sizeof a[0]);
    sink (&a);

    d = a[0];
    s = a[1];
    /* The following is only diagnosed for sizes that aren't small
       powers of 2.  */
    memcpy (d, s, sizeof a[0] + 2); /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
    sink (&a);

    d = a[0] + 1;
    s = a[1] + 1;
    memcpy (d, s, sizeof a[0]);
    sink (&a);

    d = a[0] + 1;
    s = a[1] + 1;
    memcpy (d, s, sizeof a[0] + 2); /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
    sink (&a);
  }

  {
    struct {
      char a[7];
      char b[7];
      char c[7];
    } x;
    sink (&x);

    void *d = x.a;
    const void *s = x.b;
    memcpy (d, s, sizeof x.a);
    sink (&x);

    d = x.a + 4;
    s = x.b;
    memcpy (d, s, sizeof x.a);    /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
    sink (&x);

    d = x.a + 6;
    s = x.b;
    memcpy (d, s, 1);
    sink (&x);

    d = x.a + 7;
    s = x.b;
    memcpy (d, s, 3);             /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
    sink (&x);

    d = x.a + 7;
    s = x.b + 1;
    memcpy (d, s, 1);
    sink (&x);

    d = x.b;
    s = x.a;
    memcpy (d, s, 1);
    sink (&x);

    d = x.b;
    s = x.a;
    memcpy (d, s, sizeof x.b);
    sink (&x);

    d = x.b + 2;
    s = x.a + 1;
    memcpy (d, s, sizeof x.b);
    sink (&x);

    d = x.b + 2;
    s = x.a + 2;
    memcpy (d, s, sizeof x.b);
    sink (&x);

    d = x.b + 2;
    s = x.a + 3;
    memcpy (d, s, sizeof x.b);    /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
    sink (&x);
  }

  {
#undef T
#define T(dst, src, n) do {				\
    if (!LINE || LINE == __LINE__) {			\
      char a[9] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };	\
      memcpy ((dst), (src), (n));			\
      sink (a);						\
    }							\
    } while (0)

    /* Verify the offset of the overlap is the same regardless of whether
       the destination is at lower or higher offset than the source.  */
    T (a, a + 1, 5);             /* { dg-warning "accessing 5 bytes at offsets 0 and 1 overlaps 4 bytes at offset 1" "memcpy" } */
    T (a, a + 2, 5);             /* { dg-warning "accessing 5 bytes at offsets 0 and 2 overlaps 3 bytes at offset 2" "memcpy" } */
    T (a, a + 3, 5);             /* { dg-warning "accessing 5 bytes at offsets 0 and 3 overlaps 2 bytes at offset 3" "memcpy" } */

    T (a + 1, a, 5);             /* { dg-warning "accessing 5 bytes at offsets 1 and 0 overlaps 4 bytes at offset 1" "memcpy" } */
    T (a + 2, a, 5);             /* { dg-warning "accessing 5 bytes at offsets 2 and 0 overlaps 3 bytes at offset 2" "memcpy" } */
    T (a + 3, a, 5);             /* { dg-warning "accessing 5 bytes at offsets 3 and 0 overlaps 2 bytes at offset 3" "memcpy" } */
  }
}

/* Exercise memcpy with destination or source offset or size in
   a determinate range.  */

void test_memcpy_range (char *d, size_t sz)
{
#undef T
#define T(dst, src, n) do {			\
    if (!LINE || LINE == __LINE__) {		\
      char a[9] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };	\
      void *pd = (dst);					\
      const void *ps = (src);				\
      memcpy (pd, ps, (n));				\
      sink (a, pd, ps);					\
    }							\
  } while (0)

  ptrdiff_t ir = SR (2, 3);
  T (a + ir, a, 0);
  T (a + ir, a, 1);
  T (a + ir, a, 2);
  T (a + ir, a, 3);
  /* The following fails because the size is a small power of 2.  */
  T (a + ir, a, 4);               /* { dg-warning "accessing 4 bytes at offsets \\\[2, 3] and 0 overlaps between 1 and 2 bytes at offset \(\\\[3, 2]|\\\[2, 3]\)" "pr79220" { xfail non_strict_align } } */
  T (a + ir, a, 5);               /* { dg-warning "accessing 5 bytes at offsets \\\[2, 3] and 0 overlaps between 2 and 3 bytes at offset \\\[2, 3]" "memcpy" } */

  T (d + ir, d, 0);
  T (d + ir, d, 1);
  T (d + ir, d, 2);
  T (d + ir, d, 3);
  T (d + ir, d, 4);               /* { dg-warning "accessing 4 bytes at offsets \\\[2, 3] and 0 overlaps between 1 and 2 bytes at offset \\\[2, 3]" "pr79220" { xfail non_strict_align } } */
  T (d + ir, d, 5);               /* { dg-warning "accessing 5 bytes at offsets \\\[2, 3] and 0 overlaps between 2 and 3 bytes at offset \\\[2, 3]" "memcpy" } */

  /* Because the size is constant and a power of 2 the following is
     folded too early to detect the overlap.  */
  T (d + ir, d, 4);               /* { dg-warning "accessing 4 bytes at offsets \\\[2, 3] and 0 overlaps between 1 and 2 bytes at offset \\\[2, 3]" "pr79220" { xfail non_strict_align } } */
  T (d + ir, d, 5);               /* { dg-warning "accessing 5 bytes at offsets \\\[2, 3] and 0 overlaps between 2 and 3 bytes at offset \\\[2, 3]" "memcpy" } */

  /* Exercise the full range of size_t.  */
  T (d + sz, d, 0);
  T (d + sz, d, 1);
  T (d + sz, d, 9);

  T (a, a + 1, SR (0, 1));
  T (a, a + 1, SR (0, 2));
  T (a, a + 1, SR (1, 2));
  T (a, a + 1, SR (2, 3));         /* { dg-warning "accessing between 2 and 3 bytes at offsets 0 and 1 overlaps between 1 and 2 bytes at offset 1" "memcpy" } */
  T (a, a + 1, UR (2, DIFF_MAX + (size_t)1));  /* { dg-warning "accessing 2 or more bytes at offsets 0 and 1 overlaps 1 or more bytes at offset 1" "memcpy" } */
  T (a, a + 1, UR (2, SIZE_MAX - 1));        /* { dg-warning "accessing 2 or more bytes at offsets 0 and 1 overlaps 1 or more bytes at offset 1" "memcpy" } */
  T (a, a + 2, SR (2, 3));
  T (a, a + 2, SR (3, 4));         /* { dg-warning "accessing between 3 and 4 bytes at offsets 0 and 2 overlaps between 1 and 2 bytes at offset 2" "memcpy" } */
  T (a, a + 3, SR (3, 4));
  T (a, a + 3, SR (4, 5));         /* { dg-warning "accessing between 4 and 5 bytes at offsets 0 and 3 overlaps between 1 and 2 bytes at offset 3" "memcpy" } */
  T (a, a + 3, SR (5, 6));         /* { dg-warning "accessing between 5 and 6 bytes at offsets 0 and 3 overlaps between 2 and 3 bytes at offset 3" "memcpy" } */

  T (a + 1, a, SR (0, 1));
  T (a + 1, a, SR (0, 2));
  T (a + 1, a, SR (1, 2));
  T (a + 1, a, SR (2, 3));         /* { dg-warning "accessing between 2 and 3 bytes at offsets 1 and 0 overlaps between 1 and 2 bytes at offset 1" "memcpy" } */
  T (a + 1, a, UR (2, DIFF_MAX + (size_t)1)); /* { dg-warning "accessing 2 or more bytes at offsets 1 and 0 overlaps 1 or more bytes at offset 1" "memcpy" } */
  T (a + 1, a, UR (2, SIZE_MAX - 1)); /* { dg-warning "accessing 2 or more bytes at offsets 1 and 0 overlaps 1 or more bytes at offset 1" "memcpy" } */
  T (a + 2, a, SR (2, 3));
  T (a + 2, a, SR (3, 4));         /* { dg-warning "accessing between 3 and 4 bytes at offsets 2 and 0 overlaps between 1 and 2 bytes at offset 2" "memcpy" } */
  T (a + 3, a, SR (3, 4));
  T (a + 3, a, SR (4, 5));         /* { dg-warning "accessing between 4 and 5 bytes at offsets 3 and 0 overlaps between 1 and 2 bytes at offset 3" "memcpy" } */
  T (a + 3, a, SR (5, 6));         /* { dg-warning "accessing between 5 and 6 bytes at offsets 3 and 0 overlaps between 2 and 3 bytes at offset 3" "memcpy" } */

  ir = SR (2, 5);
  T (a, a + ir, 4);
  T (a, a + ir, 5);                /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[2, 5] overlaps between 1 and 3 bytes at offset \\\[2, 4]" "memcpy" } */
  T (a, a + ir, 6);                /* { dg-warning "accessing 6 bytes at offsets 0 and \\\[2, 5] overlaps between 3 and 4 bytes at offset \\\[2, 3]" "memcpy" } */

  /* Below, there are two possible regions for the source of the copy:
       1) one just before the high end of the address space that's 3
          bytes large close to the lower end of the offset range, and
       2) another in the [DIFF_MIN, -8] range from D and so at least
          8 bytes in size
     A copy from (1) overlaps but one from (2) does not.  Verify that
     the copy is not diagnosed.  (This test case was reduced from
     the Linux kernel.) */
  T (d, d + UR (DIFF_MAX - 3, SIZE_MAX - 7), 5);
  T (d, d + UR (DIFF_MAX - 3, SIZE_MAX - 7), 6);
  T (d, d + UR (DIFF_MAX - 3, SIZE_MAX - 7), 7);
  T (d, d + UR (DIFF_MAX - 3, SIZE_MAX - 7), 9);

  T (d + UR (DIFF_MAX - 3, SIZE_MAX - 7), d, 5);
  T (d + UR (DIFF_MAX - 3, SIZE_MAX - 7), d, 6);
  T (d + UR (DIFF_MAX - 3, SIZE_MAX - 7), d, 7);
  T (d + UR (DIFF_MAX - 3, SIZE_MAX - 7), d, 9);

  {
    /* Create an offset in the range [0, -1].  */
    size_t o = sz << 1;
#if __SIZEOF_SIZE_T__ < 4
    T (d, d + o, 1234);
    T (d + o, d, 2345);
#else
    T (d, d + o, 12345);
    T (d + o, d, 23456);
#endif
  }

  /* Exercise memcpy with both destination and source pointer offsets
     in some known range.  */
  size_t n = UR (3, 4);
  T (a + SR (1, 3), a + SR (1, 3), n);  /* { dg-warning "accessing between 3 and 4 bytes at offsets \\\[1, 3] and \\\[1, 3] overlaps between 1 and 4 bytes at offset \\\[1, 3]" "memcpy" } */

  /* This is an interesting case:
       memcpy (a + i, a + j, n) with i in [1, 3], j in [2, 3], and n in [3, 4]
     we have the following possibilities ('^' denotesthe destination offset,
     '*' marks the overlap, and '?' is the possible overlap for large n):
       i j | a = 012345678   SIZ  OFF (size and offset of the overlap)
       1 2 |      ^**?       2-3   2
       1 3 |      ^ *?       1-2   3
       2 2 |       ***?      3-4   2
       2 3 |       ^**?      2-3   3
       3 3 |        ***?     3-4   3
     There are two ways to present the results:
     1) overlaps between 1 and 4 bytes at offset [2, 3]
     2) overlaps between 1 and 4 bytes at offset 2.  */
  T (a + SR (1, 3), a + SR (2, 3), n);  /* { dg-warning "accessing between 3 and 4 bytes at offsets \\\[1, 3] and \\\[2, 3] overlaps between 1 and 4 bytes at offset \\\[2, 3]" "memcpy" } */
  T (a + SR (1, 3), a + SR (3, 4), n);

  T (a + SR (2, 3), a + SR (3, 4), n);  /* { dg-warning "accessing between 3 and 4 bytes at offsets \\\[2, 3] and \\\[3, 4] overlaps between 1 and 4 bytes at offset \\\[3, 4]" "memcpy" } */

  T (a + SR (1, 3), a + SR (4, 5), n);
  T (a + SR (2, 3), a + SR (4, 5), n);
  T (a + SR (3, 4), a + SR (4, 5), n);  /* { dg-warning "accessing between 3 and 4 bytes at offsets \\\[3, 4] and \\\[4, 5] overlaps between 1 and 4 bytes at offset \\\[4, 5]" "memcpy" } */

  /* Exercise the full range of size_t.  */
  T (d, d + sz, 0);
  T (d, d + sz, 1);
  T (d, d + sz, 9);
}

/* Exercise memcpy with offset and/or size in a determinate anti-range.  */

void test_memcpy_anti_range (char *d, const char *s)
{
  T (d, d + SAR (0, 3), 1);
  T (d, d + SAR (0, 3), 2);
  T (d, d + SAR (0, 3), 3);
  T (d, d + SAR (0, 3), DIFF_MAX - 2);   /* { dg-warning "overlaps \[0-9\]+ bytes at offset 2" "memcpy" } */
  T (d, d + SAR (0, 3), DIFF_MAX - 1);   /* { dg-warning "overlaps \[0-9\]+ bytes at offset 1" "memcpy" } */
  T (d, d + SAR (0, 3), DIFF_MAX);       /* { dg-warning "overlaps \[0-9\]+ bytes at offset 0" "memcpy" } */

  T (d, d + SAR (0, 3), UR (DIFF_MAX - 2, DIFF_MAX));               /* { dg-warning "accessing \[0-9\]+ or more bytes at offsets 0 and \\\[-?\[0-9\]+, -?\[0-9\]+] overlaps \[0-9\]+ bytes at offset 2" "memcpy" } */

  /* Verify that a size in an anti-range ~[1, N] where N >= PTRDIFF_MAX - 2
     doesn't trigger a warning.
     With ~[1, PTRDIFF_MAX - 1] the difference between the just-past-the-end
     pointer to A and A for char A[PTRDIFF_MAX] wouldn't be representable in
     ptrdiff_t.  Since such a large object cannot exist, so the size of
     the region must be zero.  */
  T (d, s, UAR (1, DIFF_MAX / 2 - 1));
  T (d, s, UAR (1, DIFF_MAX - 1));
  T (d, s, UAR (1, DIFF_MAX));
  T (d, s, UAR (1, SIZE_MAX - 1));
  T (d, s, UAR (1, SIZE_MAX));
}

/* Verify calls to memcpy() where the combination of offsets in some
   range and size is such that either overlap is unavoidable or one
   or both offsets would exceed the maximum size of an object
   (DIFF_MAX).  */

void test_memcpy_range_exceed (char *d, const char *s)
{
  /* Verify offset and size both in some range.  The memcpy checking
     is less strict than that of string functions like strncpy and
     doesn't trigger unless the overlap is certain.  The following
     overlaps for (r == 3 && n > 3) but not, for example, for
     (r == 4 && n == 4), and so it's not diagnosed.  */
  ptrdiff_t i = SR (3, 5);
  size_t n = UR (4, 6);

  T (a, a + i, n);
  T (a + i, a, n);
  /* Ditto for objects of unknown sizes.  */
  T (d, d + i, n);
  T (d + i, d, n);

  /* Verify that a warning is issued for a copy between two regions
     whose aggregate size would exceed DIFF_MAX if it were to not
     overlap.  */
  T (d, s, DIFF_MAX / 2);
  T (d, s, DIFF_MAX / 2 + 1);   /* { dg-warning "overlaps 1 byte" "memcpy" } */
  T (d, s, DIFF_MAX / 2 + 2);   /* { dg-warning "overlaps 3 bytes" "memcpy" } */
  T (d, s, DIFF_MAX / 2 + 3);   /* { dg-warning "overlaps 5 bytes" "memcpy" } */

  i = SR (DIFF_MAX - 2, DIFF_MAX);

  /* Verify a warning for an out-of-bounds offset range and constant
     size addition.  */
  T (d, d + i, 3);   /* { dg-warning "accessing 3 bytes at offsets 0 and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 byte" "memcpy" } */
  T (d + i, d, 3);   /* { dg-warning "accessing 3 bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and 0 overlaps 1 byte" "memcpy" } */

  T (d + 1, d + i, 3);   /* { dg-warning "accessing 3 bytes at offsets 1 and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 byte" "memcpy" } */
  T (d + i, d + 1, 3);   /* { dg-warning "accessing 3 bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and 1 overlaps 1 byte" "memcpy" } */

  /* Verify that the warnings above are independent of whether the source
     and destination are known to be based on the same object.  */
  T (d, s + i, 3);   /* { dg-warning "accessing 3 bytes at offsets 0 and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 byte" "memcpy" } */
  T (d + i, s, 3);   /* { dg-warning "accessing 3 bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and 0 overlaps 1 byte" "memcpy" } */

  T (d + 1, s + i, 3);   /* { dg-warning "accessing 3 bytes at offsets 1 and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 byte" "memcpy" } */
  T (d + i, s + 1, 3);   /* { dg-warning "accessing 3 bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and 1 overlaps 1 byte" "memcpy" } */

#if __SIZEOF_SIZE_T__ == 8
  /* Verify the offset and size computation is correct.  The overlap
     offset mentioned in the warning plus the size of the access must
     not exceed DIFF_MAX.  */
  T (d, d + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[9223372036854775805, 9223372036854775807] overlaps 3 bytes at offset 9223372036854775802" "LP64" { target { lp64 || llp64 } } } */
  T (d + i, d, 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[9223372036854775805, 9223372036854775807] and 0 overlaps 3 bytes at offset 9223372036854775802" "LP64" { target { lp64 || llp64 } } } */

  T (d, s + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[9223372036854775805, 9223372036854775807] overlaps 3 bytes at offset 9223372036854775802" "LP64" { target { lp64 || llp64 } } } */
  T (d + i, s, 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[9223372036854775805, 9223372036854775807] and 0 overlaps 3 bytes at offset 9223372036854775802" "LP64" { target { lp64 || llp64 } } } */
#elif __SIZEOF_SIZE_T__ == 4
  T (d, d + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[2147483645, 2147483647] overlaps 3 bytes at offset 2147483642" "ILP32" { target ilp32 } } */
  T (d + i, d, 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[2147483645, 2147483647] and 0 overlaps 3 bytes at offset 2147483642" "ILP32" { target ilp32 } } */

  T (d, s + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[2147483645, 2147483647] overlaps 3 bytes at offset 2147483642" "ILP32" { target ilp32 } } */
  T (d + i, s, 5);   /* { dg-warning "accessing 5 bytes at offsets \\\[2147483645, 2147483647] and 0 overlaps 3 bytes at offset 2147483642" "ILP32" { target ilp32} } */
#endif

  ptrdiff_t j = SR (DIFF_MAX - 9, DIFF_MAX - 1);
  i = SR (DIFF_MAX - 5, DIFF_MAX - 1);
  n = UR (4, 5);
  T (d + i, d + j, n);

  n = UR (4, DIFF_MAX - 1);
  T (d + i, d + j, n);

  n = UR (4, SIZE_MAX - 1);
  T (d + i, d + j, n);

  j = SR (DIFF_MAX - 8, DIFF_MAX - 1);
  T (d + i, d + j, n);

  j = SR (DIFF_MAX - 7, DIFF_MAX - 1);
  T (d + i, d + j, n);   /* { dg-warning "accessing 4( or more)? bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and \\\[\[0-9\]+, \[0-9\]+] overlaps" "memcpy" } */

  j = SR (DIFF_MAX - 6, DIFF_MAX - 1);
  T (d + i, d + j, n);   /* { dg-warning "accessing 4( or more)? bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and \\\[\[0-9\]+, \[0-9\]+] overlaps" "memcpy" } */

  n = UR (3, DIFF_MAX);
  T (d + i, d + j, n);

  j = SR (DIFF_MAX - 6, DIFF_MAX - 1);
  T (d + i, d + j, n);

  j = SR (DIFF_MAX - 5, DIFF_MAX - 1);
  T (d + i, d + j, n);   /* { dg-warning "accessing 3 or more bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 or more bytes" "memcpy" } */

  j = SR (DIFF_MAX - 4, DIFF_MAX - 1);
  T (d + i, d + j, n);   /* { dg-warning "accessing 3 or more bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 or more bytes" "memcpy" } */

  j = SR (DIFF_MAX - 2, DIFF_MAX - 1);
  T (d + i, d + j, n);   /* { dg-warning "accessing 3 or more bytes at offsets \\\[\[0-9\]+, \[0-9\]+] and \\\[\[0-9\]+, \[0-9\]+] overlaps 1 or more bytes" "memcpy" } */
}

/* Exercise memcpy with destination and source of unknown size.  */

void test_memcpy_var (char *d, const char *s)
{
  size_t n = unsigned_value ();

  /* Since no copying takes place no warning should be issued.  */
  memcpy (d, d, 0);
  sink (d);

  /* The following overlaps if n is greater than 1.  */
  s = d + 1;
  memcpy (d, s, n);
  sink (d);

  s = d + n;
  memcpy (d, s, n);
  sink (d);

  s = d + signed_value ();
  memcpy (d, s, unsigned_value ());
  sink (d);

  s = d + 3;
  n = 1;
  memcpy (d, s, n);
  sink (d);

  s = d + 3;
  n = 2;
  memcpy (d, s, n);
  sink (d);

  s = d + 3;
  n = 3;
  memcpy (d, s, n);
  sink (d);

  s = d + 3;
  n = 4;
  memcpy (d, s, n);               /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
  sink (d);

  s = d + 5;
  n = 7;
  memcpy (d, s, n);               /* { dg-warning "\\\[-Wrestrict" "memcpy" } */
}


void test_memcpy_memarrray (struct MemArrays *p)
{
#undef T
#define T(dst, src, n) do {			\
    if (!LINE || LINE == __LINE__) {		\
      void *pd = (dst);				\
      const void *ps = (src);			\
      memcpy (pd, ps, (n));			\
      sink (pd, ps);				\
    }						\
  } while (0)

  T (p->a8, p->a8, 0);
  T (p->a8, p->a8 + 1, 1);
  T (p->a8, p->a8 + 2, 2);
  T (p->a8, p->a8 + 8, 1);

  T (p->a8, p->a8 + 2, 3);        /* { dg-warning "accessing 3 bytes at offsets 0 and 2 overlaps 1 byte at offset 2" "memcpy" } */
}

/* Exercise the absence of warnings with memmove.  */

void test_memmove (void)
{
  {
    char d[7];
    sink (d);

    const void *s = d;
    memmove (d, s, 7);
    sink (d);

    s = d + 1;
    memmove (d, s, 6);
    sink (d);

    s = d + 2;
    memmove (d + 1, s, 5);
    sink (d);
  }
}

/* Exercise strcat with constant or known arguments.  */

void test_strcat_cst (const char *s)
{
#undef T
#define T(init, dst, src) do {				\
    if (!LINE || LINE == __LINE__) {			\
      char a[9] = init;					\
      char *pd = (dst);					\
      const char *ps = (src);				\
      strcat (pd, ps);					\
      sink (a, pd, ps);					\
    }							\
  } while (0)

  T ("0",   a, a);                /* { dg-warning "source argument is the same as destination" "strcat" } */
  T ("01",  a, a);                /* { dg-warning "source argument is the same as destination" "strcat" } */
  T ("012", a, a);                /* { dg-warning "source argument is the same as destination" "strcat" } */
  /* The 3 bytes "12\0" being appended to "012" overwrite the final NUL.  */
  T ("012", a, a + 1);            /* { dg-warning "accessing 3 bytes at offsets 0 and 1 overlaps 1 byte at offset 3" "strcat" } */
  T ("012", a, a + 2);            /* { dg-warning "accessing 2 bytes at offsets 0 and 2 overlaps 1 byte at offset 3" "strcat" } */
  /* The nul copied from a[3] to a[3] overwrites itself so this is
     diagnosed.  */
  T ("012", a, a + 3);            /* { dg-warning "accessing 1 byte at offsets 0 and 3 overlaps 1 byte at offset 3" "strcat" } */

  T ("012", a, a + 4);
  T ("012", a, a + 5);
  T ("012", a, a + 6);
  T ("012", a, a + 7);
  T ("012", a, a + 8);

  T ("0",   a + 1, a);            /* { dg-warning "accessing 2 bytes at offsets 1 and 0 overlaps 1 byte at offset 1" "strcat" } */
  T ("0",   a + 2, a);

  /* The first of the two offsets in the diagnostic for strcat is that
     of the first write into the destination, not that of the initial
     read from it to compute its length.  */
  T ("01",  a + 1, a);            /* { dg-warning "accessing 3 bytes at offsets 1 and 0 overlaps 1 byte at offset 2" "strcat" } */
  T ("01",  a + 2, a);            /* { dg-warning "accessing 3 bytes at offsets 2 and 0 overlaps 1 byte at offset 2" "strcat" } */
  T ("01",  a + 3, a);

  T ("012", a + 1, a);            /* { dg-warning "accessing 4 bytes at offsets 1 and 0 overlaps 1 byte at offset 3" "strcat" } */
  T ("012", a + 2, a);            /* { dg-warning "accessing 4 bytes at offsets 2 and 0 overlaps 1 byte at offset 3" "strcat" } */
  T ("012", a + 3, a);            /* { dg-warning "accessing 4 bytes at offsets 3 and 0 overlaps 1 byte at offset 3 " "strcat" } */
  T ("012", a + 4, a);
  T ("012", a + 5, a);

  /* Verify that the obviously benign cases below aren't diagnosed.  */
  T ("012",      a, "012");
  T ("012",      a, s);
  T ("01234567", a, s);
}

/* Exercise strcat with destination and source of unknown length.  */

void test_strcat_var (char *d, const char *s)
{
#undef T
#define T(dst, src) do {				\
    if (!LINE || LINE == __LINE__) {			\
      char *pd = (dst);					\
      const char *ps = (src);				\
      strcat (pd, ps);					\
      sink (pd, ps);					\
    }							\
  } while (0)

  T (d, d);                       /* { dg-warning "source argument is the same as destination" "strcat" } */
  T (d, d + 1);                   /* { dg-warning "accessing 2 or more bytes at offsets 0 and 1 may overlap 1 byte" "strcat" } */
  T (d, d + 2);                   /* { dg-warning "accessing 3 or more bytes at offsets 0 and 2 may overlap 1 byte at offset 2" "strcat" } */
  T (d, d + 999);                 /* { dg-warning "accessing 1000 or more bytes at offsets 0 and 999 may overlap 1 byte at offset 999" "strcat" } */

  /* The source string must be at least 100 bytes in length for the copy
     below to overlap.  */
  T (d, d + -99);                 /* { dg-warning "accessing 100 or more bytes at offsets 0 and -99 may overlap 1 byte" "strcat" } */

  size_t n = unsigned_value ();

  T (d + n, d + n);                       /* { dg-warning "\\\[-Wrestrict" "strcat" } */

  /* Verify that the obviously benign cases below aren't diagnosed.  */
  T (d, "012");
  T (d + 1, "0123");
  T (d + n, "01234");
  T (d, s);
  T (d + 1, s);
  T (d + n, s);

  /* Since the offset is unknown the overlap in the call below, while
     possible, is certainly not inevitable.  Conservatively, it should
     not be diagnosed.  For safety, an argument for diagnosing can be
     made.  It's a judgment call, partly determined by the effort and
     complexity of treating this case differently from other similar
     to it.   */
  T (d, d + n); /* { dg-warning "may overlap" "strcat" } */
}

/* Exercise strcpy with constant or known arguments.  */

void test_strcpy_cst (ptrdiff_t i)
{
#undef T
#define T(init, dst, src) do {				\
    if (!LINE || LINE == __LINE__) {			\
      char a[8] = init;					\
      char *pd = (dst);					\
      const char *ps = (src);				\
      strcpy (pd, ps);					\
      sink (a, pd, ps);					\
    }							\
  } while (0)

  T ("012", a, a);                /* { dg-warning "source argument is the same as destination" "strcpy" } */
  T ("012", a, a + 1);            /* { dg-warning "accessing 3 bytes at offsets 0 and 1 overlaps 2 bytes at offset 1" "strcpy" } */
  T ("012", a, a + 2);
  T ("012", a, a + 3);
  T ("012", a, a + sizeof a);     /* { dg-warning "\\\[-Wstringop-overread" "pr81437" } */

  /* The terminating nul written to d[2] overwrites s[0].  */
  T ("0123", a, a + 2);           /* { dg-warning "accessing 3 bytes at offsets 0 and 2 overlaps 1 byte at offset 2" } */

  /* The '5' copied from s[2] to d[2] overwrites s[0].  */
  T ("01234", a, a + 2);          /* { dg-warning "accessing 4 bytes at offsets 0 and 2 overlaps 2 bytes at offset 2" } */

  /* This happens to be safe in GCC but it's still wrong.  */
  T ("012", a, a);                /* { dg-warning "source argument is the same as destination" "strcpy" } */

  T ("012", a + 1, a);            /* { dg-warning "accessing 4 bytes at offsets 1 and 0 overlaps 3 bytes at offset 1" "strcpy" } */
  T ("012", a + 2, a);            /* { dg-warning "accessing 4 bytes at offsets 2 and 0 overlaps 2 bytes at offset 2" "strcpy" } */
  T ("012", a + 3, a);            /* { dg-warning "accessing 4 bytes at offsets 3 and 0 overlaps 1 byte at offset 3" "strcpy" } */
  T ("012", a + 4, a);
  /* The following doesn't overlap but it triggers -Wstringop-overflow
     for writing past the end.  */
  T ("012", a + sizeof a, a);     /* { dg-warning "\\\[-Wstringop-overflow" } */
}

/* Exercise strcpy with constant or known arguments offset by a range.
   The tests verify the use of the lower bound of the range which is
   more restrictive than using the upper bound for positive values.  */

void test_strcpy_range (void)
{
#undef T
#define T(N, init, dst, src)			\
  do {						\
    if (!LINE || LINE == __LINE__) {		\
      char a[N] = init;				\
      char *pd = (dst);				\
      const char *ps = (src);			\
      strcpy (pd, ps);				\
      sink (a, pd, ps);				\
    }						\
  } while (0)

  ptrdiff_t r;

  r = SR (0, 1);
  T (8, "0", a + r, a);   /* { dg-warning "accessing 2 bytes at offsets \\\[0, 1] and 0 overlaps between 1 and 2 bytes at offset \\\[0, 1]" "strcpy" } */

  r = SR (2, 5);
  T (8, "01",  a + r, a);            /* { dg-warning "accessing 3 bytes at offsets \\\[2, 5] and 0 may overlap 1 byte at offset 2" } */
  T (8, "012", a + r, a);            /* { dg-warning "accessing 4 bytes at offsets \\\[2, 5] and 0 may overlap up to 2 bytes at offset \\\[2, 3]" "strcpy" } */

  /* The highest offset to which to copy without overflowing the 8-byte
     destination is 3 and that overlaps 2 bytes.  */
  T (8, "0123", a + r, a);           /* { dg-warning "accessing 5 bytes at offsets \\\[2, 5] and 0 overlaps between 2 and 3 bytes at offset \\\[2, 3]" "strcpy" } */

  /* With a 9-byte destination the highest offset is 4 and that still
     overlaps 1 byte (the final NUL).  */
  T (9, "0123", a + r, a);           /* { dg-warning "accessing 5 bytes at offsets \\\[2, 5] and 0 overlaps between 1 and 3 bytes at offset \\\[2, 4]" "strcpy" } */

  /* With a 10-byte buffer it's possible to copy all 5 bytes without
     overlap at (a + 5).  Copying at offsets 2 through 4 overflows
     between 3 and 1 bytes, respectively.  */
  T (10, "0123", a + r, a);          /* { dg-warning "accessing 5 bytes at offsets \\\[2, 5] and 0 may overlap up to 3 bytes at offset \\\[2, 4]" "strcpy" } */


  r  = SR (3, 4);
  T (8, "01",  a + r, a);
  T (8, "012", a + r, a);            /* { dg-warning "accessing 4 bytes at offsets \\\[3, 4] and 0 may overlap 1 byte at offset 3" "strcpy" } */

  /* The highest offset to which to copy without overflowing the 8-byte
     destination is 3 and that overlaps 2 bytes.  */
  T (8, "0123", a + r, a);           /* { dg-warning "accessing 5 bytes at offsets \\\[3, 4] and 0 overlaps 2 bytes at offset 3" "strcpy" } */

  /* With a 9-byte destination the highest offset is 4 and that still
     overlaps 1 byte (the final NUL).  */
  T (9, "0123", a + r, a);           /* { dg-warning "accessing 5 bytes at offsets \\\[3, 4] and 0 overlaps between 1 and 2 bytes at offset \\\[3, 4]" "strcpy" } */

  /* With a 10-byte buffer it's possible to copy all 5 bytes without
     overlap at (a + 5).  Copying at offsets 2 through 4 overflows
     between 3 and 1 bytes, respectively.  */
  T (10, "0123", a + r, a);          /* { dg-warning "accessing 5 bytes at offsets \\\[3, 4] and 0 overlaps between 1 and 2 bytes at offset \\\[3, 4]" "strcpy" } */

  T (8, "01",     a, a + r);
  T (8, "012",    a, a + r);
  T (8, "0123",   a, a + r);
  T (8, "01234",  a, a + r);

  /* With the smaller offset of 3 the final NUL definitely overlaps
     the '4' at a[3], but with the larger offset of 4 there is no
     overlap, so the warning is a "may overlap" and the size of
     the overlap is 1 byte.  */
  T (8, "012345", a, a + r);         /* { dg-warning "accessing between 3 and 4 bytes at offsets 0 and \\\[3, 4] may overlap 1 byte at offset 3" "strcpy" } */
  T (8, "0123456", a, a + r);        /* { dg-warning "accessing between 4 and 5 bytes at offsets 0 and \\\[3, 4] may overlap up to 2 bytes at offset \\\[3, 4]" "strcpy" } */

  r = SR (3, DIFF_MAX - 3);
  T (8, "01",  a + r, a);

  /* The accesses below might trigger either
       -Wrestrict: accessing 4 bytes at offsets [3, \[0-9\]+] and 0 may overlap 1 byte at offset 3
     or
       -Wstringop-overflow: writing 4 bytes into a region of size 0
     Either of the two is appropriate.  */
  T (8, "012", a + r, a);            /* { dg-warning "\\\[-Wrestrict|-Wstringop-overflow" } */

  r = SR (DIFF_MAX - 2, DIFF_MAX - 1);
  T (8, "012", a + r, a);            /* { dg-warning "\\\[-Wrestrict|-Wstringop-overflow" } */

  /* Exercise the full range of ptrdiff_t.  */
  r = signed_value ();

  /* The overlap in the cases below isn't inevitable but it is diagnosed
     because it is possible and so the code is considered unsafe.  */
  T (8, "", a, a + r);               /* { dg-warning "accessing 1 byte at offsets 0 and \\\[0, 8] may overlap 1 byte" "strcpy" } */
  T (8, "0", a + r, a);              /* { dg-warning "accessing 2 bytes at offsets \\\[0, 8] and 0 may overlap up to 2 bytes" "strcpy" } */
  T (8, "012", a + r, a);            /* { dg-warning "accessing 4 bytes at offsets \\\[0, 8] and 0 may overlap up to 4 bytes" "strcpy" } */

  T (8, "", a, a + r);               /* { dg-warning "accessing 1 byte at offsets 0 and \\\[0, 8] may overlap" "strcpy" } */
  T (8, "0", a, a + r);              /* { dg-warning "accessing between 1 and 2 bytes at offsets 0 and \\\[0, 8] may overlap up to 2 bytes" "strcpy" } */
  T (8, "012", a, a + r);            /* { dg-warning "accessing between 1 and 4 bytes at offsets 0 and \\\[0, 8] may overlap up to 4 bytes" "strcpy" } */
}

/* Exercise strcpy with destination and/or source of unknown lengthu.  */

void test_strcpy_var (char *d, const char *s)
{
#undef T
#define T(dst, src) do {			\
    if (!LINE || LINE == __LINE__) {			\
      char *pd = (dst);					\
      const char *ps = (src);				\
      strcpy (pd, ps);					\
      sink (pd, ps);					\
    }							\
  } while (0)

  T (d, s);

  T (d, &d[0]);                   /* { dg-warning "source argument is the same as destination" "strcpy" } */
  T (&d[0], d);                   /* { dg-warning "source argument is the same as destination" "strcpy" } */

  s = d;
  T (d, s);                       /* { dg-warning "source argument is the same as destination" "strcpy" } */

  /* The following overlaps if *s is not nul.  It arguably should be
     diagnosed.  */
  T (d, d + 1);

  /* The following overlaps if strlen (d) is greater than 1.  Like
     the above, it possibly should be diagnosed too.  */
  int r = SR (2, 3);
  T (d, d + r);

  /* The following overlaps only if strlen (s + n) >= n so it's not
     diagnosed.  */
  s = d + signed_value ();
  T (d, s);
}

/* Exercise strncpy with constant or known arguments.  */

void test_strncpy_cst (void)
{
#undef T
#define T(init, dst, src, size) do {			\
    if (!LINE || LINE == __LINE__) {			\
      char a[9] = init;					\
      char *pd = (dst);					\
      const char *ps = (src);				\
      strncpy (pd, ps, (size));				\
      sink (a, pd, ps);					\
    }							\
  } while (0)

  T ("012", a, a, 0);
  T ("012", a, a, 1);             /* { dg-warning "source argument is the same as destination " "strncpy" } */

  T ("012", a, a + 1, 1);
  T ("012", a, a + 1, 2);         /* { dg-warning "accessing 2 bytes at offsets 0 and 1 overlaps 1 byte at offset 1" "strncpy" } */
  T ("012", a, a + 1, 3);         /* { dg-warning "accessing 3 bytes at offsets 0 and 1 overlaps 2 bytes at offset 1" "strncpy" } */
  T ("012", a, a + 1, 4);         /* { dg-warning "accessing 4 bytes at offsets 0 and 1 overlaps 3 bytes at offset 1" "strncpy" } */
  T ("012", a, a + 1, 5);         /* { dg-warning "accessing 5 bytes at offsets 0 and 1 overlaps 3 bytes at offset 1" "strncpy" } */
  T ("012", a, a + 1, 6);         /* { dg-warning "accessing 6 bytes at offsets 0 and 1 overlaps 3 bytes at offset 1" "strncpy" } */

  T ("012", a, a + 2, 1);
  T ("012", a, a + 2, 2);
  /* The third written byte (nul) overwrites a[2].  */
  T ("012", a, a + 2, 3);         /* { dg-warning "accessing 3 bytes at offsets 0 and 2 overlaps 1 byte at offset 2" "strncpy" } */
  T ("012", a, a + 2, 4);         /* { dg-warning "accessing 4 bytes at offsets 0 and 2 overlaps 2 bytes at offset 2" "strncpy" } */
  T ("012", a, a + 2, 5);         /* { dg-warning "accessing 5 bytes at offsets 0 and 2 overlaps 2 bytes at offset 2" "strncpy" } */

  T ("0123", a, a + 2, 1);
  T ("0123", a, a + 2, 2);
  /* The terminating nul written to a[2] overwrites s[0].  */
  T ("0123", a, a + 2, 3);        /* { dg-warning "accessing 3 bytes at offsets 0 and 2 overlaps 1 byte at offset 2" "strncpy" } */
  T ("0123", a, a + 2, 4);        /* { dg-warning "accessing 4 bytes at offsets 0 and 2 overlaps 2 bytes at offset 2" "strncpy" } */
  T ("0123", a, a + 2, 5);        /* { dg-warning "accessing 5 bytes at offsets 0 and 2 overlaps 3 bytes at offset 2" "strncpy" } */
  T ("0123", a, a + 2, 6);        /* { dg-warning "accessing 6 bytes at offsets 0 and 2 overlaps 3 bytes at offset 2" "strncpy" } */

  T ("01234", a, a + 2, 1);
  T ("01234", a, a + 2, 2);
  T ("01234", a, a + 2, 3);       /* { dg-warning "accessing 3 bytes at offsets 0 and 2 overlaps 1 byte at offset 2" "strncpy" } */
  /* The '5' copied from s[2] to d[2] overwrites s[0].  */
  T ("01234", a, a + 2, 4);       /* { dg-warning "accessing 4 bytes at offsets 0 and 2 overlaps 2 bytes at offset 2" "strncpy" } */
  T ("01234", a, a + 2, 5);       /* { dg-warning "accessing 5 bytes at offsets 0 and 2 overlaps 3 bytes at offset 2" "strncpy" } */
}


/* Exercise strncpy with one or more arguments in a determinate range.  */

void test_strncpy_range (char *d, size_t n)
{
#undef T
#define T(init, dst, src, size) do {			\
    if (!LINE || LINE == __LINE__) {			\
      char a[9] = init;					\
      strncpy ((dst), (src), (size));			\
      sink (a, (dst), (src));				\
    }							\
  } while (0)

  ptrdiff_t i;

  i = SR (0, 1);
  T ("0123", a, a + i, 0);
  T ("0123", a, a + i, 1); /* { dg-warning "accessing 1 byte at offsets 0 and \\\[0, 1] may overlap 1 byte at offset 0" } */
  /* When i == 1 the following overlaps at least 1 byte: the nul at a[1]
     (if a + 1 is the empty string).  If a + 1 is not empty then it overlaps
     it plus as many non-nul characters after it, up to the total of 2.  */
  T ("0123", a, a + i, 2);   /* { dg-warning "accessing 2 bytes at offsets 0 and \\\[0, 1] overlaps between 1 and 2 bytes at offset \\\[0, 1]" "strncpy" } */

  i = SR (1, 5);
  T ("0123", a, a + i, 0);
  T ("0123", a, a + i, 1);
  T ("0123", a, a + i, 2);   /* { dg-warning "accessing 2 bytes at offsets 0 and \\\[1, 5] may overlap 1 byte at offset 1" "strncpy" } */
  T ("0123", a, a + i, 3);   /* { dg-warning "accessing 3 bytes at offsets 0 and \\\[1, 5] may overlap up to 2 bytes at offset \\\[1, 2]" "strncpy" } */
  T ("0123", a, a + i, 4);   /* { dg-warning "accessing 4 bytes at offsets 0 and \\\[1, 5] may overlap up to 3 bytes at offset \\\[1, 3]" "strncpy" } */
  T ("0123", a, a + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[1, 5] may overlap up to 4 bytes at offset \\\[1, 4]" "strncpy" } */

  i = SR (2, 5);
  T ("0123", a, a + i, 0);
  T ("0123", a, a + i, 1);
  T ("0123", a, a + i, 2);
  T ("0123", a, a + i, 3);   /* { dg-warning "accessing 3 bytes at offsets 0 and \\\[2, 5] may overlap 1 byte at offset 2" "strncpy" } */
  T ("0123", a, a + i, 4);   /* { dg-warning "accessing 4 bytes at offsets 0 and \\\[2, 5] may overlap up to 2 bytes at offset \\\[2, 3]" "strncpy" } */
  T ("0123", a, a + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[2, 5] may overlap up to 3 bytes at offset \\\[2, 4]" "strncpy" } */
  /* When i == 5 the following overlaps at least 1 byte: the nul at a[5]
     (if a + 5 is the empty string).  If a + 5 is not empty then it overlaps
     it plus as many non-nul characters after it, up to the total of 6.  */
  T ("0123", a, a + i, 6);   /* { dg-warning "accessing 6 bytes at offsets 0 and \\\[2, 5] overlaps between 1 and 3 bytes at offset \\\[2, 5]" "strncpy" } */

  i = SR (3, 5);
  T ("0123", a, a + i, 0);
  T ("0123", a, a + i, 1);
  T ("0123", a, a + i, 2);
  T ("0123", a, a + i, 3);
  T ("0123", a, a + i, 4);   /* { dg-warning "accessing 4 bytes at offsets 0 and \\\[3, 5] may overlap 1 byte at offset 3" "strncpy" } */
  T ("0123", a, a + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[3, 5] may overlap up to 2 bytes at offset \\\[3, 4]" "strncpy" } */

  /* The following copy overlaps at most 2 bytes.  When i == 3 it overlaps
     the 2 bytes at "3", when i == 4 just the final nul.  When i == 5 it
     also overlaps 1 byte, the nul at a[5].  Although the overlap offset
     range suggests the overlap is up to three bytes, it correctly reflects
     the union of the two cases.  */
  T ("0123", a, a + i, 6);   /* { dg-warning "accessing 6 bytes at offsets 0 and \\\[3, 5] overlaps between 1 and 2 bytes at offset \\\[3, 5]" "strncpy" } */

  i = SR (4, 5);
  T ("0123", a, a + i, 0);
  T ("0123", a, a + i, 1);
  T ("0123", a, a + i, 2);
  T ("0123", a, a + i, 3);
  T ("0123", a, a + i, 4);
  T ("0123", a, a + i, 5);   /* { dg-warning "accessing 5 bytes at offsets 0 and \\\[4, 5] may overlap 1 byte at offset 4" "strncpy" } */
  /* Regardless of the value of i, the following overlaps exactlty
     one byte: the nul at a[4].  There is no overlap at a[5] because
     the source is not read past the nul so the offset below isn't
     entirely correct.  */
  T ("0123", a, a + i, 6);   /* { dg-warning "accessing 6 bytes at offsets 0 and \\\[4, 5] overlaps 1 byte at offset \\\[4, 5]" "strncpy" } */

  /* Verify offset and size both in some range.  The strncpy checking
     is more strict than that of memcpy and triggers even when the
     overlap is possible but not inevitable.  The following overlaps
     like so ('*' denotes the terminating NUL, '.' the appended NUL
     that's not copied from the source):
        a:        01234567*  (also indicates offset)
        i = 4:    4567       none
                  4567*      overlaps 1 at offset 4
                  4567*.     overlaps 2 at offset 4
        i = 5:    567*       none
                  567*.      none
                  567*..     overlaps 1 at offset 5  */
  T ("01234567", a, a + i, UR (4, 6));   /* { dg-warning "accessing between 4 and 6 bytes at offsets 0 and \\\[4, 5] may overlap up to 2 bytes at offset \\\[4, 5]" "strncpy" } */

  /* Ditto for objects of unknown sizes.  */
  T ("01234567", d, d + i, UR (4, 6));  /* { dg-warning "accessing between 4 and 6 bytes at offsets 0 and \\\[4, 5] may overlap up to 2 bytes at offset \\\[4, 5]" "strncpy" } */

  T ("01234567", a, a + i, UR (6, 7));  /* { dg-warning "accessing between 6 and 7 bytes at offsets 0 and \\\[4, 5] overlaps between 1 and 3 bytes at offset \\\[4, 5]" "strncpy" } */

  /* The following overlaps except in the unlikely case that value ()
     is zero, so it's diagnosed.  */
  T ("012", a, a, n);             /* { dg-warning "\\\[-Wrestrict]" "strncpy" } */
}


/* Exercise strncpy with destination and source of unknown length.  */

void test_strncpy_var (char *d, const char *s, size_t n)
{
#undef T
#define T(dst, src, size) do {			\
    if (!LINE || LINE == __LINE__) {		\
      char *pd = (dst);				\
      const char *ps = (src);			\
      strncpy (pd, ps, (size));			\
      sink (pd, ps);				\
    }						\
  } while (0)

  T (d, s, 1);
  T (d, s, n);

  T (d, d, 1);                    /* { dg-warning "\\\[-Wrestrict" "strncpy" } */
  T (d, d, n);                    /* { dg-warning "\\\[-Wrestrict" "strncpy" } */

  T (d,     d + 1, 1);
  T (d,     d + 1, 2);            /* { dg-warning "\\\[-Wrestrict" "strncpy" } */
  T (d + 1, d,     1);
  T (d + 1, d,     2);            /* { dg-warning "\\\[-Wrestrict" "strncpy" } */
}

struct MemberArrays
{
  char a[7];
  char b[8];
  char c[9];
};

void test_strncpy_strcpy_var (struct MemberArrays *ar, const char *s)
{
  /* The following is safe and should not trigger a warning.  */
  strncpy (ar->b, s, sizeof ar->b - 1);
  ar->b[sizeof ar->b - 1] = '\0';
  strcpy (ar->a, ar->b);
  sink (ar);

  /* The following is not as safe (it might overflow ar->a) but there
     is no overlap so it also shouldn't trigger -Wrestrict.  */
  strncpy (ar->c, s, sizeof ar->c - 1);
  ar->c[sizeof ar->c - 1] = '\0';
  strcpy (ar->a, ar->c);
  sink (ar);
}
