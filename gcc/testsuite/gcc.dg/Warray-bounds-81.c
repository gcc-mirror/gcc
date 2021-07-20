/* PR tree-optimization/101397 - spurious warning writing to the result
   of stpcpy minus 1
   Verify warnings for indexing into a pointer returned from stpncpy.
   The call stpncpy(S1, S2, N) returns the address of the copy of
   the first NUL is it exists or &S1[N] otherwise.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-truncation" } */

typedef __SIZE_TYPE__ size_t;

void* calloc (size_t, size_t);
char* stpncpy (char*, const char*, size_t);

void sink (int, ...);

extern char ax[], a3[3], a5[5], a7[7], a9[9], *s;

volatile int x;

/* Verify warnings for indexing into the result of stpncpy with a source
   pointing to an array of unknown bound.  */

void test_stpncpy_from_ptr (int i, int n)
{
  {
    // P is in [ax, ax + 5].
    char *p = stpncpy (ax, s, 5);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-5], p[-1], p[0], p[9]);
  }

  {
    // P is in [a5, a5 + 3].
    char *p = stpncpy (a5, s, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[ 1], p[ 2], p[ 3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // P is in [ax, ax + 4].
    char *p = stpncpy (a5, s, 4);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[ 1], p[ 2], p[ 3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // P is in [ax, ax + 5].
    char *p = stpncpy (a5, s, n);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[ 1], p[ 2], p[ 3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // P is in [ax, ax + 4].
    char *p = stpncpy (a5, s, 4);

    if (i > -1) i = -1;
    x = p[i];

    if (i > -2) i = -2;
    x = p[i];

    if (i > -3) i = -3;
    x = p[i];

    if (i > -4) i = -4;
    x = p[i];

    if (i > -5) i = -5;
    x = p[i];                           // { dg-warning "\\\[-Warray-bounds" }
  }
}

/* Verify warnings for indexing into the result of stpncpy with a source
   an array of size 5.  */

void test_stpncpy_from_a5 (int i, int n, int n3_9)
{
  {
    // The returned pointer is in [ax, ax + 3].
    char *p = stpncpy (ax, a5, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0], p[1], p[99]);
  }

  {
    // The returned pointer is in [ax, ax + 5].
    char *p = stpncpy (ax, a5, 5);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }

  {
    //The returned pointer is in [ax, ax + 5] even though n is not known.
    char *p = stpncpy (ax, a5, n);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-5], p[-4], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[9], p[99]);
  }


  {
    // The returned pointer is in [a3, a3 + 3].
    char *p = stpncpy (a3, a5, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[ 1], p[ 2]);
    x = p[ 3];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is in [a3, a3 + 3].
    char *p = stpncpy (a3, a5, n);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[ 1], p[ 2]);
    x = p[ 3];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (n3_9 < 3 || 9 < n3_9)
      n3_9 = 3;

    // The returned pointer is in [a3, a3 + 3].
    char *p = stpncpy (a3, a5, n3_9);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[ 1], p[ 2]);
    x = p[ 3];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = stpncpy (a3, a5, 3);

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


/* Verify warnings for indexing into the result of stpncpy with a source
   an array of size 7.  */

void test_stpncpy_from_a7 (int i, int n, int n3_9)
{
  {
    // The returned pointer is ax + 5.
    char *p = stpncpy (ax, a7, 5);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }

  {
    //The returned pointer is in [ax, ax + 7] even though n is not known.
    char *p = stpncpy (ax, a7, n);
    x = p[-8];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-7], p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9]);
  }


  {
    // The returned pointer is in [a5, a5 + 3].
    char *p = stpncpy (a5, a7, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is a5 + 4.
    char *p = stpncpy (a5, a7, 4);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is in [a5, a5 + 5].
    char *p = stpncpy (a5, a7, n);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (n3_9 < 3 || 9 < n3_9)
      n3_9 = 3;

    // The returned pointer is in [a5, a5 + 5].
    char *p = stpncpy (a5, a7, n3_9);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = stpncpy (a5, a7, 4);

    if (i > -1) i = -1;
    x = p[i];

    if (i > -2) i = -2;
    x = p[i];

    if (i > -3) i = -3;
    x = p[i];

    if (i > -4) i = -4;
    x = p[i];

    if (i > -5) i = -5;
    x = p[i];                           // { dg-warning "\\\[-Warray-bounds" }
  }
}


void test_stpncpy_from_a5_to_allocated (int i, int n, int n5_7, int n3_9)
{
  if (n5_7 < 5 || 7 < n5_7)
    n5_7 = 5;

  {
    char *d = calloc (n5_7, 1);
    char *p = stpncpy (d, s, n);
    x = p[-8];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-7], p[-6], p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *d = calloc (n5_7, 1);
    char *p = stpncpy (d, a3, n);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *d = calloc (n5_7, 1);
    char *p = stpncpy (d, a5, n);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-5], p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *d = calloc (n5_7, 1);
    char *p = stpncpy (d, a9, n);
    x = p[-8];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-7], p[-6], p[-5], p[-3], p[-4], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *d = calloc (n5_7, 1);
    char *p = stpncpy (d, a3, n3_9);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *d = calloc (n5_7, 1);
    char *p = stpncpy (d, a9, n3_9);
    x = p[-8];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-7], p[-6], p[-5], p[-4], p[-4], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4], p[5], p[6]);
    x = p[7];                           // { dg-warning "\\\[-Warray-bounds" }
  }

}
