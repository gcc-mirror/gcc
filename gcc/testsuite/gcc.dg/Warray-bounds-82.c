/* PR tree-optimization/101397 - spurious warning writing to the result
   of stpcpy minus 1
   Verify warnings for indexing into a pointer returned from mempcpy.
   The call mempcpy(S1, S2, N) returns &S1[N].
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

void* mempcpy (void*, const void*, size_t);

extern char ax[], a3[3], a5[5], a7[7], *s;

volatile int x;

/* Verify warnings for indexing into the result of mempcpy with a source
   pointing to an array of unknown bound.  */

void test_mempcpy_from_ptr (int i)
{
  {
    char *p = mempcpy (ax, s, 5);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }

  {
    char *p = mempcpy (a5, s, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];
    x = p[ 1];
    x = p[ 2];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = mempcpy (a5, s, 4);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-4];
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];
    x = p[ 1];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = mempcpy (a5, s, 4);

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

/* Verify warnings for indexing into the result of mempcpy with a source
   an array of size 5.  */

void test_mempcpy_from_a5 (int i, int n, int n3_9)
{
  {
    // The returned pointer is ax + 3 as specified by the bound.
    char *p = mempcpy (ax, a5, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-3];
    x = p[-2];
    x = p[ 0];
    x = p[ 1];
    x = p[ 2];
  }

  {
    // The returned pointer is ax + 5.
    char *p = mempcpy (ax, a5, 5);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }

  {
    //The returned pointer is in [ax, ax + 5] even though n is not known.
    char *p = mempcpy (ax, a5, n);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }


  {
    // The returned pointer is a3 + 3.
    char *p = mempcpy (a3, a5, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-3];
    x = p[-1];
    x = p[ 0];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[ 1];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is in [a3, a3 + 3].
    char *p = mempcpy (a3, a5, n);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];
    x = p[ 2];
    x = p[ 3];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (n3_9 < 3 || 9 < n3_9)
      n3_9 = 3;

    // The returned pointer is a3.
    char *p = mempcpy (a3, a5, n3_9);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = mempcpy (a3, a5, 3);

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


/* Verify warnings for indexing into the result of mempcpy with a source
   an array of size 7.  */

void test_mempcpy_from_a7 (int i, int n, int n3_9)
{
  {
    // The returned pointer is ax + 5.
    char *p = mempcpy (ax, a7, 5);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }

  {
    //The returned pointer is in [ax, ax + 7] even though n is not known.
    char *p = mempcpy (ax, a7, n);
    x = p[-8];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-7];
    x = p[-1];
    x = p[ 0];
    x = p[ 9];
  }


  {
    // The returned pointer is a5 + 3 as specified by the bound.
    char *p = mempcpy (a5, a7, 3);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-3];
    x = p[-2];
    x = p[ 0];
    x = p[ 1];
    x = p[ 2];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is a5 + 4.
    char *p = mempcpy (a5, a7, 4);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-4];
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];
    x = p[ 1];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    // The returned pointer is in [a5, a5 + 5].
    char *p = mempcpy (a5, a7, n);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];
    x = p[ 4];
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (n3_9 < 3 || 9 < n3_9)
      n3_9 = 3;

    // The returned pointer is in [a5 + 3, a5 + 5].
    char *p = mempcpy (a5, a7, n3_9);
    x = p[-6];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[-5];
    x = p[-3];
    x = p[-2];
    x = p[-1];
    x = p[ 0];
    x = p[ 1];
    x = p[ 2];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = mempcpy (a5, a7, 4);

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
