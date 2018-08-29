/* PR tree-optimization/85753 - missing -Wrestrict on memcpy into a member
   array
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#define memcpy   __builtin_memcpy

char a[16];

struct { char a[16]; } x;

/* Exercise aggregate types.  */

void test_aggr_idx_nowarn (int i, int j)
{
  memcpy (&a[i], &a[j], 7);
  memcpy (&x.a[i], &x.a[j], 7);
}

void test_aggr_idx_warn (int i, int j)
{
  memcpy (&a[i], &a[j], 9);       /* { dg-warning "\\\[-Wrestrict" } */
  memcpy (&x.a[i], &x.a[j], 9);   /* { dg-warning "\\\[-Wrestrict" } */
}

void test_aggr_off_nowarn (int i, int j)
{
  memcpy (a + i, a + j, 5);
  memcpy (x.a + i, x.a + j, 5);
}

void test_aggr_off_warn (int i, int j)
{
  memcpy (a + i, a + j, 9);       /* { dg-warning "\\\[-Wrestrict" } */
  memcpy (x.a + i, x.a + j, 9);   /* { dg-warning "\\\[-Wrestrict" } */
}


void sink (void*);

#define T(call) sink (call)


/* Also exercise basic types.  */

#ifdef __UINT32_TYPE__

__UINT32_TYPE__ i32;

void test_basic_32 (int i, int j)
{
  char *p = (char*)&i32;

  T (memcpy (&p[i], &p[j], 1));
  T (memcpy (&p[i], &p[j], 2));
  T (memcpy (&p[i], &p[j], 3));   /* { dg-warning "\\\[-Wrestrict" } */

  T (memcpy (p + i, p + j, 1));
  T (memcpy (p + i, p + j, 2));
  T (memcpy (p + i, p + j, 3));   /* { dg-warning "\\\[-Wrestrict" } */
}

#endif

#ifdef __UINT64_TYPE__

__UINT64_TYPE__ i64;

void test_basic_64 (int i, int j)
{
  char *p = (char*)&i64;

  T (memcpy (&p[i], &p[j], 1));
  T (memcpy (&p[i], &p[j], 2));
  T (memcpy (&p[i], &p[j], 3));
  T (memcpy (&p[i], &p[j], 5));   /* { dg-warning "\\\[-Wrestrict" } */
  T (memcpy (&p[i], &p[j], 6));   /* { dg-warning "\\\[-Wrestrict" } */
  T (memcpy (&p[i], &p[j], 7));   /* { dg-warning "\\\[-Wrestrict" } */

  T (memcpy (p + i, p + j, 1));
  T (memcpy (p + i, p + j, 2));
  T (memcpy (p + i, p + j, 3));
  T (memcpy (p + i, p + j, 5));   /* { dg-warning "\\\[-Wrestrict" } */
  T (memcpy (p + i, p + j, 6));   /* { dg-warning "\\\[-Wrestrict" } */
  T (memcpy (p + i, p + j, 7));   /* { dg-warning "\\\[-Wrestrict" } */
}

#endif
