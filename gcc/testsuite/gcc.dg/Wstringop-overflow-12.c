/* PR tree-optimization/89350 - Wrong -Wstringop-overflow warning
   on a variable offset from the end of an array
   Test exercising -Wstringop-truncation alone, with -Warray-bounds
   explicitly disabled.
   { dg-do compile }
   { dg-options "-O2 -Wno-array-bounds -Wstringop-overflow -ftrack-macro-expansion=0" }  */

#include "range.h"

extern void* memcpy (void*, const void*, size_t);
extern void* memset (void*, int, size_t);

void sink (void*);

extern char ga7[7];


#define T(d, n) (memcpy ((d), s, (n)), sink (d))

void test_memcpy_array_cst_range_off (const void *s)
{
  char *d = ga7 + 1;

  T (d + UR (1, 2), 1);
  T (d + UR (1, 2), 5);

  T (d + UR (0, 1), 6);
  T (d + UR (0, 1), 7);       /* { dg-warning ".memcpy. writing 6 bytes into a region of size 5 overflows the destination" "pr89428" { xfail *-*-* } } */
  T (d + UR (1, 2), 6);       /* { dg-warning ".memcpy. writing 6 bytes into a region of size 5 overflows the destination" } */
  T (d + UR (1, 2), 7);       /* { dg-warning "writing 7 bytes into a region of size 5 " } */

  T (d + SR (-3, -2), 1);     /* { dg-warning "writing 1 byte into a region of size 0 " } */
  T (d + SR (-2, -1), 1);
  T (d + SR (-2, -1), 2);     /* { dg-warning "writing 2 bytes into a region of size 7 " "pr89428" { xfail *-*-* } } */
  T (d + SR (-2, -1), 9);     /* { dg-warning "writing 9 bytes into a region of size 7 " } */

  d = ga7 + 7;
  T (d + SR (-7, -6), 1);
  T (d + SR (-7, -1), 1);
  T (d + SR (-2, -1), 3);     /* { dg-warning "writing 3 bytes into a region of size 2 " } */

  T (d + UR (1, 2), 1);       /* { dg-warning "writing 1 byte into a region of size 0 " } */
}


void test_memcpy_array_range_range_off (const void *s)
{
  char *d = ga7 + UR (0, 1);
  T (d + SR (-1, 0), 1);
  T (d + SR (-1, 0), 7);
  T (d + SR (-1, 0), 9);       /* { dg-warning "writing 1 byte into a region of size 0 " "pr89350" { xfail *-*-* } } */
}


#undef T
#define T(d, n) (memset ((d), 0, (n)), sink (d))

void test_memset_array_unsigned_off (void)
{
  char *d = ga7 + 1;

  T (d + UR (1, 2), 1);
  T (d + UR (1, 2), 5);

  T (d + UR (0, 1), 6);
  T (d + UR (0, 1), 7);       /* { dg-warning ".memset. writing 6 bytes into a region of size 5 overflows the destination" "pr89428" { xfail *-*-* } } */
  T (d + UR (1, 2), 6);       /* { dg-warning ".memset. writing 6 bytes into a region of size 5 overflows the destination" } */
  T (d + UR (1, 2), 7);       /* { dg-warning "writing 7 bytes into a region of size 5 " } */

  T (d + SR (-3, -2), 1);     /* { dg-warning "writing 1 byte into a region of size 0 " } */
  T (d + SR (-2, -1), 1);
  T (d + SR (-2, -1), 2);     /* { dg-warning "writing 2 bytes into a region of size 7 " "pr89428" { xfail *-*-* } } */
  T (d + SR (-2, -1), 9);     /* { dg-warning "writing 9 bytes into a region of size 7 " } */

  d = ga7 + 7;
  T (d + SR (-7, -6), 1);
  T (d + SR (-7, -1), 1);
  T (d + SR (-2, -1), 3);     /* { dg-warning "writing 3 bytes into a region of size 2 " } */

  T (d + UR (1, 2), 1);       /* { dg-warning "writing 1 byte into a region of size 0 " } */
}



struct MemArray { char a7[7], a3[3], c; };

extern struct MemArray gma;

void test_memset_memarray (void)
{
  char *d = gma.a7 + 1;

  T (d + UR (1, 2), 1);
  T (d + UR (1, 2), 5);

  T (d + UR (0, 1), 6);
  T (d + UR (0, 1), 7);       /* { dg-warning ".memset. writing 6 bytes into a region of size 5 overflows the destination" "pr89428" { xfail *-*-* } } */
  T (d + UR (1, 2), 6);       /* { dg-warning ".memset. writing 6 bytes into a region of size 5 overflows the destination" "pr89350" { xfail *-*-* } } */
  T (d + UR (1, 2), 7);       /* { dg-warning "writing 7 bytes into a region of size 5 " "pr85350" { xfail *-*-* } } */

}


#undef T
#define T(d, n) (memcpy ((d), s, (n)), sink (d))

void test_memcpy_array_signed_off (const void *s)
{
  char *d = ga7 + 1;

  T (d + SR (-7, 7), 7);
  T (d + SR (-1, 1), 7);
  T (d + SR (-1, 1), 9);      /* { dg-warning "writing 9 bytes into a region of size " } */
  T (d + SR (-1, 2), 9);      /* { dg-warning "writing 9 bytes into a region of size " } */
  T (d + SR (1, 2), 1);
  T (d + SR (1, 2), 5);

  T (d + SR (0, 1), 6);
  T (d + UR (1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}
