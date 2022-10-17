/* PR middle-end/100137 - -Warray-bounds false positive on varying offset
   plus negative
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern char ax[], a1[1], a2[2], a3[3], a4[4], a5[5];

int* ptr;
#define X (*ptr++)


__attribute__ ((noipa)) void
array_plus_var_minus_cstint (int i, int j)
{
  {
    const char *p = ax;
    p += i;
    X = p[-1];
    X = p[-123];
  }

  {
    const char *p = a1;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a2;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a3;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-5];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a4;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-5];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-6];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a5;
    p += i;
    p += j;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-5];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-6];      // { dg-warning "\\\[-Warray-bounds" }
  }
}


__attribute__ ((noipa)) void
array_plus_var_minus_cstlong (long i, long j)
{
  {
    const char *p = ax;
    p += i;
    X = p[-1];
    X = p[-123];
  }

  {
    const char *p = a1;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a2;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a3;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-5];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a4;
    p += i;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-5];      // { dg-warning "\\\[-Warray-bounds" }
    X = p[-6];      // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    const char *p = a5;
    p += i;
    p += j;
    X = p[-1];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-2];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-3];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-4];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-5];      // { dg-bogus "\\\[-Warray-bounds" }
    X = p[-6];      // { dg-warning "\\\[-Warray-bounds" }
  }
}
