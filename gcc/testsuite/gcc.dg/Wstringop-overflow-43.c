/* PR 96903 - bogus warning on memcpy at negative offset from array end
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#include "range.h"

#define INT_MAX    __INT_MAX__
#define INT_MIN    -(INT_MAX - 1)
#define UINT_MAX   (2U * INT_MAX + 1)

typedef __SIZE_TYPE__ size_t;

void* memset (void *, int, size_t);

void sink (void*, ...);

extern char a11[11];
struct S { char a11[11], b; };
extern struct S sa11;

#define T2(dst, off1, off2, n) do {		\
    char *_p0 = dst;				\
    char *_p1 = _p0 + (off1);			\
    char *_p2 = _p1 + (off2);			\
    memset (_p2, 0, n);				\
    sink (dst, _p0, _p1, _p2);			\
  } while (0);

#define T1(dst, off, n) T2 (dst, off, 0, n)


void nowarn_memset_array_cst (void)
{
  char *p = &a11[11];

  T1 (p, -11, 11);
  T1 (p, -10, 10);
  T1 (p,  -9,  9);
  T1 (p,  -8,  8);
  T1 (p,  -3,  3);
  T1 (p,  -2,  2);
  T1 (p,  -1,  1);
  T1 (p,   0,  0);

  T2 (p, -6, -5, 11);
  T2 (p, -6, -4, 10);
  T2 (p, -6, -3,  9);
  T2 (p, -6, -2,  8);
  T2 (p, -6, -1,  7);
  T2 (p, -5, -6, 11);
  T2 (p, -5, -5, 10);
}

void nowarn_memset_array_rng_int (void)
{
  char *p = &a11[11];

  int i11 = SR (11, INT_MAX);
  int i10 = SR (10, INT_MAX);
  int i9  = SR ( 9, INT_MAX);
  int i3  = SR ( 3, INT_MAX);
  int i2  = SR ( 2, INT_MAX);
  int i1  = SR ( 1, INT_MAX);
  int i0  = SR ( 0, INT_MAX);

  int m11 = SR (INT_MIN, -11);
  int m10 = SR (INT_MIN, -10);
  int m9  = SR (INT_MIN,  -9);
  int m3  = SR (INT_MIN,  -3);
  int m2  = SR (INT_MIN,  -2);
  int m1  = SR (INT_MIN,  -1);
  int m0  = SR (INT_MIN,  -0);

  T1 (p, m11, i11);
  T1 (p, m10, i10);
  T1 (p,  m9,  i9);
  T1 (p,  m3,  i3);
  T1 (p,  m2,  i2);
  T1 (p,  m1,  i1);
  T1 (p,  m0,  i0);

  T1 (p, m11, i11);
  T1 (p, m10, i10);
  T1 (p,  m9,  i9);
  T1 (p,  m3,  i3);
  T1 (p,  m2,  i2);
  T1 (p,  m1,  i1);
  T1 (p,  m0,  i0);
}


void nowarn_memset_array_rng (void)
{
  char *p = &a11[11];

  T2 (p, SR (-11, -10), SR ( -2,  -1), UR (11, 12));
  T2 (p, SR (-10,  -9), SR ( -1,   0), UR (11, 13));
  T2 (p, SR ( -9,  -8), SR ( -2,  -1), UR (11, 14));
  T2 (p, SR ( -8,  -7), SR ( -3,  -2), UR (11, 15));
  T2 (p, SR ( -7,  -6), SR ( -4,  -3), UR (11, 16));
  T2 (p, SR ( -6,  -5), SR ( -5,  -4), UR (11, 17));
  T2 (p, SR ( -5,  -4), SR ( -6,  -5), UR (11, 18));
  T2 (p, SR ( -4,  -3), SR ( -7,  -6), UR (11, 19));
  T2 (p, SR ( -3,  -2), SR ( -8,  -7), UR (11, INT_MAX));
  T2 (p, SR ( -2,  -1), SR ( -9,  -8), UR (11, UINT_MAX));
  T2 (p, SR ( -1,   0), SR (-10,  -9), UR (11, DIFF_MAX));
  T2 (p, SR (  0,   1), SR (-11, -10), UR (11, SIZE_MAX));

  T2 (p, SR (DIFF_MIN, -10), SR (DIFF_MIN, -1), UR (10, 12));

  T2 (p, SR (-11, -10), SR ( -3,  -1), UR (10, 12))
  T2 (p, SR (-11, -10), SR ( -3,  -1), UR (10, 12))
}


void warn_memset_array_rng (void)
{
  char *p = &a11[11];
  size_t n11_12 = UR (11, 12);
  size_t n10_12 = UR (10, 12);

  T2 (p, SR (-11, -10), SR ( -3,  -2), n11_12);    // { dg-warning "writing between 11 and 12 bytes into a region of size 0" }
  T2 (p, SR (-11, -10), SR ( -3,  -2), n10_12);    // { dg-warning "writing between 10 and 12 bytes into a region of size 0" }
}


void nowarn_memset_anti_range (void)
{
  size_t n11 = UR (11, SIZE_MAX);

  char *p = &a11[11];

  T1 (p, (int)SAR (INT_MIN,      -12), n11);
  T1 (p, (int)SAR (    -13,      -13), n11);
  T1 (p, (int)SAR (    -13,      -12), n11);
  T1 (p, (int)SAR (    -10,        1), n11);
  T1 (p, (int)SAR (    -10,       11), n11);
  T1 (p, (int)SAR (    -10,  INT_MAX), n11);
  T1 (p, (int)SAR (     -1,       -1), n11);
  T1 (p, (int)SAR (     -1,        0), n11);
  T1 (p, (int)SAR (     -1,       11), n11);
  T1 (p, (int)SAR (     -1,  INT_MAX), n11);

  T1 (p, SAR (DIFF_MIN,       -12), n11);
  T1 (p, SAR (     -13,       -13), n11);
  T1 (p, SAR (     -13,       -12), n11);
  T1 (p, SAR (     -10,         1), n11);   // { dg-bogus "-Wstringop-overflow" }
  T1 (p, SAR (     -10,        11), n11);   // { dg-bogus "-Wstringop-overflow" }
  T1 (p, SAR (     -10,  DIFF_MAX), n11);
  T1 (p, SAR (     -1,         -1), n11);   // { dg-bogus "-Wstringop-overflow" }
  T1 (p, SAR (     -1,          0), n11);   // { dg-bogus "-Wstringop-overflow" }
  T1 (p, SAR (     -1,         11), n11);   // { dg-bogus "-Wstringop-overflow" }
  T1 (p, SAR (     -1,   DIFF_MAX), n11);
}

void warn_memset_reversed_range (void)
{
  size_t n11 = UR (11, SIZE_MAX);

  char *p = &a11[11];

  /* Since the offset is excessive, either starting before &a11[0]
     ot just past &a[11], the region size in the warning should
     probably be zero, but accept other sizes too.

     The problem isn't detected anymore because the offset is in
     the anti-range ~[INT_MIN, -11] which isn't handled.  */
  T1 (p, SAR (INT_MIN, -11), n11);      // { dg-warning "writing 11 or more bytes into a region of size \\d+" "" { xfail *-*-* } }

  /* The following are represented as ordinary ranges with reversed bounds
     and those are handled. */
  T1 (p, SAR (INT_MIN,  11), n11);      // { dg-warning "writing 11 or more bytes into a region of size 0" }
  T1 (p, SAR (INT_MIN,   1), n11);      // { dg-warning "writing 11 or more bytes into a region of size 0" }
  T1 (p, SAR (INT_MIN,   0), n11);      // { dg-warning "writing 11 or more bytes into a region of size 0" }
  /* Also represented as a true anti-range.  */
  T1 (p, SAR (    -12, -11), n11);      // { dg-warning "writing 11 or more bytes into a region of size \\d+" "" { xfail *-*-* } }
  T1 (p, SAR (    -12,  -1), n11);      // { dg-warning "writing 11 or more bytes into a region of size 0" }
  T1 (p, SAR (    -11,   0), n11);      // { dg-warning "writing 11 or more bytes into a region of size 0" }
  T1 (p, SAR (    -11,  11), n11);      // { dg-warning "writing 11 or more bytes into a region of size 0" }
}
