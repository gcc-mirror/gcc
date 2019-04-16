/* PR tree-optimization/89350 - Wrong -Wstringop-overflow warning
   on a variable offset from the end of an array
   Test exercising -Wstringop-truncation with -Wall.
   -Wstringop-truncation is disabled to avoid warnings for strncpy
   calls whose bound matches the size of the destination getting
   in the way of -Wstringop-overflow.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-truncation -ftrack-macro-expansion=0" }  */

#include "range.h"

extern void* memcpy (void*, const void*, size_t);
extern void* memset (void*, int, size_t);
extern char* strcpy (char*, const char*);
extern char* strncpy (char*, const char*, size_t);

void sink (void*);

#define CAT(pfx, line) pfx ## line
#define CONCAT(pfx, line) CAT (pfx, line)
#define UNIQ_NAME(pfx) CONCAT (pfx, __LINE__)

/* Exercise a call to memset with a distinct destination object each
   time to prevent GCC from reusing the destination pointer in later
   tests.  */
#define T(off1, off2, n)			\
  do {						\
    extern char UNIQ_NAME (ga)[7];		\
    char *d = UNIQ_NAME (ga) + off1;		\
    d += off2;					\
    memset (d, 0, n);				\
    sink (d);					\
  } while (0)


/* Exercise calls to memset with a destination pointer pointing to
   an array plus constant offset plus variable offset, in that order.  */

void test_memset_array_cst_range_off (void)
{
  T (1, SR (-7, 7), 7);
  T (1, SR (-1, 1), 7);
  T (1, SR (-1, 1), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (1, SR ( 1, 2), 1);
  T (1, SR ( 1, 2), 5);

  T (1, SR ( 0, 1), 6);
  T (1, UR ( 1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR (-7, 7), 7);
  T (2, SR (-2, 7), 7);
  T (2, SR (-1, 1), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR (-1, 1), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR ( 1, 2), 1);
  T (2, SR ( 1, 2), 3);
  T (2, SR ( 1, 2), 4);
  T (2, SR ( 1, 2), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR ( 0, 1), 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-* } } */
  T (2, UR ( 1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-7, 0), 7);
  T (7, UR (-7, 0), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-3, 2), 3);
  T (7, UR (-2, 2), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


/* Exercise calls to memset with a destination pointer pointing to
   an array plus variable offset plus constant offset.  */

void test_memset_array_range_cst_off (void)
{
  T (SR (-7, 7), 1, 7);
  T (SR (-1, 1), 1, 7);
  T (SR (-1, 1), 1, 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-*} } */
  T (SR ( 1, 2), 1, 1);
  T (SR ( 1, 2), 1, 5);

  T (SR ( 0, 1), 1, 6);
  T (UR ( 1, 2), 1, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR (-7, 7), 2, 7);
  T (SR (-1, 1), 2, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR (-1, 1), 2, 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR ( 1, 2), 2, 1);
  T (SR ( 1, 2), 2, 3);
  T (SR ( 1, 2), 2, 4);
  T (SR ( 1, 2), 2, 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR ( 0, 1), 2, 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (UR ( 1, 2), 2, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


void test_memset_array_range_range_off (void)
{
  T (UR (0, 1), UR (0, 1), 7);
  T (UR (3, 5), UR (2, 7), 1);
  T (UR (3, 7), UR (2, 9), 2);
  T (UR (3, 9), UR (2, 9), 3);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (UR (0, 1), UR (1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


#undef T
#define T(off1, off2, n)			\
  do {						\
    extern char UNIQ_NAME (ga)[7];		\
    char *d = UNIQ_NAME (ga) + off1;		\
    d += off2;					\
    memcpy (d, s, n);				\
    sink (d);					\
  } while (0)


void test_memcpy_array_cst_range_off (const void *s)
{
  T (1, SR (-7, 7), 7);
  T (1, SR (-1, 1), 7);
  T (1, SR (-1, 1), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (1, SR ( 1, 2), 1);
  T (1, SR ( 1, 2), 5);

  T (1, SR ( 0, 1), 6);
  T (1, UR ( 1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR (-7, 7), 7);
  T (2, SR (-2, 7), 7);
  T (2, SR (-1, 1), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR (-1, 1), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR ( 1, 2), 1);
  T (2, SR ( 1, 2), 3);
  T (2, SR ( 1, 2), 4);
  T (2, SR ( 1, 2), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR ( 0, 1), 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-* } } */
  T (2, UR ( 1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-7, 0), 7);
  T (7, UR (-7, 0), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-3, 2), 3);
  T (7, UR (-2, 2), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


void test_memcpy_array_range_cst_off (const void *s)
{
  T (SR (-7, 7), 1, 7);
  T (SR (-1, 1), 1, 7);
  T (SR (-1, 1), 1, 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-*} } */
  T (SR ( 1, 2), 1, 1);
  T (SR ( 1, 2), 1, 5);

  T (SR ( 0, 1), 1, 6);
  T (UR ( 1, 2), 1, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR (-7, 7), 2, 7);
  T (SR (-1, 1), 2, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR (-1, 1), 2, 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR ( 1, 2), 2, 1);
  T (SR ( 1, 2), 2, 3);
  T (SR ( 1, 2), 2, 4);
  T (SR ( 1, 2), 2, 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR ( 0, 1), 2, 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (UR ( 1, 2), 2, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


void test_memcpy_array_range_range_off (const void *s)
{
  T (UR (0, 1), UR (0, 1), 7);
  T (UR (3, 5), UR (2, 7), 1);
  T (UR (3, 7), UR (2, 9), 2);
  T (UR (3, 9), UR (2, 9), 3);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (UR (0, 1), UR (1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


#undef T
#define T(off1, off2, n)			\
  do {						\
    extern char UNIQ_NAME (ga)[7];		\
    char *d = UNIQ_NAME (ga) + off1;		\
    d += off2;					\
    const char str[] = "0123456789";		\
    const char *s = str + sizeof str - 1 - n;   \
    strcpy (d, s);				\
    sink (d);					\
  } while (0)


void test_strcpy_array_cst_range_off (void)
{
  T (1, SR (-7, 7), 6);
  T (1, SR (-1, 1), 6);
  T (1, SR (-1, 1), 8);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (1, SR ( 1, 2), 0);
  T (1, SR ( 1, 2), 4);

  T (1, SR ( 0, 1), 5);
  T (1, UR ( 1, 2), 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR (-7, 7), 6);
  T (2, SR (-2, 7), 6);
  T (2, SR (-1, 1), 6);      /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR (-1, 1), 8);      /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR ( 1, 2), 0);
  T (2, SR ( 1, 2), 2);
  T (2, SR ( 1, 2), 3);
  T (2, SR ( 1, 2), 4);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR ( 0, 1), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-* } } */
  T (2, UR ( 1, 2), 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-7, 0), 6);
  T (7, UR (-7, 0), 8);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-3, 2), 2);
  T (7, UR (-2, 2), 4);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


void test_strcpy_array_range_cst_off (const char *s)
{
  T (SR (-7, 7), 1, 6);
  T (SR (-1, 1), 1, 6);
  T (SR (-1, 1), 1, 8);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-*} } */
  T (SR ( 1, 2), 1, 0);
  T (SR ( 1, 2), 1, 1);
  T (SR ( 1, 2), 1, 4);

  T (SR ( 0, 1), 1, 5);
  T (UR ( 1, 2), 1, 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR (-7, 7), 2, 6);
  T (SR (-1, 1), 2, 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR (-1, 1), 2, 8);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR ( 1, 2), 2, 0);
  T (SR ( 1, 2), 2, 1);
  T (SR ( 1, 2), 2, 2);
  T (SR ( 1, 2), 2, 3);
  T (SR ( 1, 2), 2, 4);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR ( 0, 1), 2, 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (UR ( 1, 2), 2, 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


#undef T
#define T(off1, off2, n)			\
  do {						\
    extern char UNIQ_NAME (ga)[7];		\
    char *d = UNIQ_NAME (ga) + off1;		\
    d += off2;					\
    strncpy (d, s, n);				\
    sink (d);					\
  } while (0)


void test_strncpy_array_cst_range_off (const char *s)
{
  T (1, SR (-7, 7), 7);
  T (1, SR (-1, 1), 7);
  T (1, SR (-1, 1), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (1, SR ( 1, 2), 1);
  T (1, SR ( 1, 2), 5);

  T (1, SR ( 0, 1), 6);
  T (1, UR ( 1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR ( -7, 7), 7);
  T (2, SR ( -1, 1), 7);      /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR ( -1, 1), 9);      /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (2, SR ( 1, 2), 1);
  T (2, SR ( 1, 2), 3);
  T (2, SR ( 1, 2), 4);
  T (2, SR ( 1, 2), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (2, SR ( 0, 1), 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-* } } */
  T (2, UR ( 1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-7, 0), 7);
  T (7, UR (-7, 0), 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (7, UR (-3, 2), 3);
  T (7, UR (-2, 2), 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


void test_strncpy_array_range_cst_off (const char *s)
{
  T (SR (-7, 7), 1, 7);
  T (SR (-1, 1), 1, 7);
  T (SR (-1, 1), 1, 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "pr89428" { xfail *-*-*} } */
  T (SR ( 1, 2), 1, 1);
  T (SR ( 1, 2), 1, 5);

  T (SR ( 0, 1), 1, 6);
  T (UR ( 1, 2), 1, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR (-7, 7), 2, 7);
  T (SR (-1, 1), 2, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR (-1, 1), 2, 9);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (SR ( 1, 2), 2, 1);
  T (SR ( 1, 2), 2, 3);
  T (SR ( 1, 2), 2, 4);
  T (SR ( 1, 2), 2, 5);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (SR ( 0, 1), 2, 6);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */

  T (UR ( 1, 2), 2, 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}


void test_strncpy_array_range_range_off (const char *s)
{
  T (UR (0, 1), UR (0, 1), 7);
  T (UR (3, 5), UR (2, 7), 1);
  T (UR (3, 7), UR (2, 9), 2);
  T (UR (3, 9), UR (2, 9), 3);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
  T (UR (0, 1), UR (1, 2), 7);       /* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" } */
}
