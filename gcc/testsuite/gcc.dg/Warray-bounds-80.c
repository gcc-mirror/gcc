/* PR tree-optimization/101397 - spurious warning writing to the result
   of stpcpy minus 1
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char* stpcpy (char*, const char*);

void sink (int, ...);

extern char ax[], a3[3], a5[5], *s;

volatile int x;

void test_stpcpy (int i)
{
  {
    char *p = stpcpy (ax, s);
    x = p[-9];                          // { dg-bogus "\\\[-Warray-bounds" }
    x = p[-1];                          // { dg-bogus "\\\[-Warray-bounds" }
    x = p[ 0];
    x = p[+9];
  }

  {
    char *p = stpcpy (a3, s);
    x = p[-2];                          // { dg-bogus "\\\[-Warray-bounds" }
    x = p[-1];                          // { dg-bogus "\\\[-Warray-bounds" }
  }

  {
    char *p = stpcpy (a3, s);
    x = p[-3];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-2], p[-1], p[0], p[1], p[2]);
    x = p[ 3];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    /* Stpcpy always returns a pointer to the copied nul (which must
       exist) and never a past-the-end pointer.  As a result, P below
       is in [a5, a5 + 4].  */
    char *p = stpcpy (a5, s);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = stpcpy (a5 + 1, s);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2], p[3]);
    x = p[ 4];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = stpcpy (a5 + 2, s);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1], p[2]);
    x = p[ 3];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = stpcpy (a5 + 3, s);
    x = p[-5];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-4], p[-3], p[-2], p[-1], p[0]);
    sink (p[1]);
    x = p[ 2];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    /* Because strlen (a3) is at most 2, the stpcpy call must return
       a pointer in the range [ax, ax + 2], and so -3 is necessarily
       out of bounds.  */
    char *p = stpcpy (ax, a3);
    p[-3] = 1;                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    if (i >= 0)
      i = -1;

    char *p = stpcpy (a3, s);
    x = p[i];                           // { dg-bogus "\\\[-Warray-bounds" }
  }

  {
    if (i >= -3)
      i = -3;

    char *p = stpcpy (a3, s);
    p[i] = 1;                           // { dg-warning "\\\[-Warray-bounds" }
  }

}
