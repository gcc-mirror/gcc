/* PR tree-optimization/101397 - spurious warning writing to the result
   of stpcpy minus 1
   Verify warnings for indexing into a pointer returned from stpncpy.
   The call stpncpy(S1, S2, N) returns the address of the copy of
   the first NUL is it exists or &S1[N] otherwise.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-truncation" } */

typedef __SIZE_TYPE__ size_t;

__attribute__ ((alloc_size (1))) const void* alloc (size_t);

void* memchr (const void*, int, size_t);

void sink (int, ...);

extern char ax[], a3[3], a5[5], a7[7], a9[9];

volatile int x;

/* Verify warnings for indexing into the result of memchr.  */

void test_memchr (int i, int n, int n3_5, int n3_9)
{
  {
    /* Because memchr never returns a past-the-end pointer the result
       below is in [ax, ax + 4].  */
    const char *p = memchr (ax, x, 5);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-4];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }

  {
    // The returned pointer is in [ax, ax + n].
    const char *p = memchr (ax, x, n);
    sink (p[-99], p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[99]);
  }


  {
    // The returned pointer is in [a5, a5 + 2].
    const char *p = memchr (a5, x, 3);
    x = p[-3];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is a5 + 4.
    const char *p = memchr (a5, x, 4);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is in [a5, a5 + 4].
    const char *p = memchr (a5, x, n);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (n3_5 < 3 || 5 < n3_5)
      n3_5 = 3;

    // The returned pointer is in [a7, a7 + 4].
    const char *p = memchr (a7, x, n3_5);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (n3_9 < 3 || 9 < n3_9)
      n3_9 = 3;

    // The returned pointer is in [a5, a5 + 4].
    const char *p = memchr (a5, x, n3_9);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = memchr (a5, x, 4);

    if (i > -1) i = -1;
    x = p[i];

    if (i > -2) i = -2;
    x = p[i];

    if (i > -3) i = -3;
    x = p[i];

    if (i > -4) i = -4;
    x = p[i];                           // { dg-warning "\\\[-Warray-bounds" }
  }
}


void test_memchr_in_allocated (int i, int n, int n5_7, int n3_9)
{
  if (n5_7 < 5 || 7 < n5_7)
    n5_7 = 5;

  {
    const char *s = alloc (n5_7);
    const char *p = memchr (s, x, n);
    x = p[-7];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-6], p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *s = alloc (n5_7);
    const char *p = memchr (s, x, n);
    x = p[-7];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *s = alloc (n5_7);
    const char *p = memchr (s, x, n);
    x = p[-7];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *s = alloc (n5_7);
    const char *p = memchr (s, x, n);
    x = p[-7];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-6], p[-5], p[-3], p[-4], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *s = alloc (n5_7);
    const char *p = memchr (s, x, n3_9);
    x = p[-7];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *s = alloc (n5_7);
    const char *p = memchr (s, x, n3_9);
    x = p[-7];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-6], p[-5], p[-4], p[-4], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

}
