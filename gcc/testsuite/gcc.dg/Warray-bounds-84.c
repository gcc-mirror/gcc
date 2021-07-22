/* PR tree-optimization/101397 - spurious warning writing to the result
   of stpcpy minus 1
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

char* strcpy (char*, const char*);

void sink (int, ...);

extern char ax[], a3[3], a5[5], *s;

volatile int x;

void test_strcpy (int i)
{
  {
    char *p = strcpy (ax, s);
    x = p[-1];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[ 0];
    x = p[+9];
  }

  {
    char *p = strcpy (a3, s);
    x = p[-1];                          // { dg-warning "\\\[-Warray-bounds" }
    x = p[0];
    x = p[1];
    x = p[2];
    x = p[3];                           // { dg-warning "\\\[-Warray-bounds" }
 }

  {
    char *p = strcpy (a5, s);
    x = p[-1];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[0], p[1], p[2], p[3], p[4]);
    x = p[ 5];                          // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = strcpy (a5 + 1, s);
    x = p[-2];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-1], p[0], p[1], p[2], p[3]);
    x = p[4];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = strcpy (a5 + 2, s);
    x = p[-3];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-2], p[-1], p[0], p[1], p[2]);
    x = p[3];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = strcpy (a5 + 3, s);
    x = p[-4];                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[-3], p[-2], p[-1], p[0], p[1]);
    x = p[2];                           // { dg-warning "\\\[-Warray-bounds" }
  }

  {
    char *p = strcpy (ax, a3);
    p[-1] = 1;                          // { dg-warning "\\\[-Warray-bounds" }
    sink (p[0], p[1], p[2], p[9], p[99]);
  }
}
