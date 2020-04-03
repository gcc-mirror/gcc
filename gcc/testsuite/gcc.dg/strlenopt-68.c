/* PR tree-optimization/83431 - Verify that snprintf (0, 0, "%s",
   with an argument that's a conditional expression evaluates to
   the expected result regardless of the order of the expression
   operands.
   { dg-do run }
   { dg-skip-if "UNIX 2003 return behavior not supported" { hppa*-*-hpux* } }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))

const char gs0[] = "";
const char gs3[] = "123";

char gc;
char ga5[7];

struct S { char n, ma7[7], max[]; };


__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs0_gs3_ga5_m1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs0 : 0 < i ? gs3 : ga5;

  A (snprintf (0, 0, "%s", p) == 0);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs0_gs3_ga5_0 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs0 : 0 < i ? gs3 : ga5;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs0_gs3_ga5_p1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs0 : 0 < i ? gs3 : ga5;

  A (snprintf (0, 0, "%s", p) == 3);
}


__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs0_ga5_gs3_m1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs0 : 0 < i ? ga5 : gs3;

  A (snprintf (0, 0, "%s", p) == 0);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs0_ga5_gs3_0 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs0 : 0 < i ? ga5 : gs3;

  A (snprintf (0, 0, "%s", p) == 3);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs0_ga5_gs3_p1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs0 : 0 < i ? ga5 : gs3;

  A (snprintf (0, 0, "%s", p) == 4);
}


__attribute__ ((noclone, noinline, noipa)) void
equal_4_ga5_gs0_gs3_m1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? ga5 : 0 < i ? gs0 : gs3;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_ga5_gs0_gs3_0 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? ga5 : 0 < i ? gs0 : gs3;

  A (snprintf (0, 0, "%s", p) == 3);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_ga5_gs0_gs3_p1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? ga5 : 0 < i ? gs0 : gs3;

  A (snprintf (0, 0, "%s", p) == 0);
}


__attribute__ ((noclone, noinline, noipa)) void
equal_4_ga5_gs3_gs0_m1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? ga5 : 0 < i ? gs3 : gs0;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_ga5_gs3_gs0_0 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? ga5 : 0 < i ? gs3 : gs0;

  A (snprintf (0, 0, "%s", p) == 0);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_ga5_gs3_gs0_p1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? ga5 : 0 < i ? gs3 : gs0;

  A (snprintf (0, 0, "%s", p) == 3);
}


__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs3_gs0_ga5_m1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs3 : 0 < i ? gs0 : ga5;

  A (snprintf (0, 0, "%s", p) == 3);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs3_gs0_ga5_0 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs3 : 0 < i ? gs0 : ga5;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
equal_4_gs3_gs0_ga5_p1 (int i)
{
  strcpy (ga5, "1234");
  const char *p = i < 0 ? gs3 : 0 < i ? gs0 : ga5;

  A (snprintf (0, 0, "%s", p) == 0);
}


/* Similar to the above but with memcpy creating a string at least
   four characters long, and the address of the NUL character.  */

__attribute__ ((noclone, noinline, noipa)) void
min_4_gc_gs3_ga5_m1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? &gc : 0 < i ? gs3 : ga5;

  A (snprintf (0, 0, "%s", p) == 0);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_gc_gs3_ga5_0 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? &gc : 0 < i ? gs3 : ga5;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_gc_gs3_ga5_p1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? &gc : 0 < i ? gs3 : ga5;

  A (snprintf (0, 0, "%s", p) == 3);
}


__attribute__ ((noclone, noinline, noipa)) void
min_4_gc_ga5_gs3_m1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? &gc : 0 < i ? ga5 : gs3;

  A (snprintf (0, 0, "%s", p) == 0);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_gc_ga5_gs3_0 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? &gc : 0 < i ? ga5 : gs3;

  A (snprintf (0, 0, "%s", p) == 3);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_gc_ga5_gs3_p1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? &gc : 0 < i ? ga5 : gs3;

  A (snprintf (0, 0, "%s", p) == 4);
}


__attribute__ ((noclone, noinline, noipa)) void
min_4_ga5_gc_gs3_m1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? ga5 : 0 < i ? &gc : gs3;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_ga5_gc_gs3_0 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? ga5 : 0 < i ? &gc : gs3;

  A (snprintf (0, 0, "%s", p) == 3);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_ga5_gc_gs3_p1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? ga5 : 0 < i ? &gc : gs3;

  A (snprintf (0, 0, "%s", p) == 0);
}


__attribute__ ((noclone, noinline, noipa)) void
min_4_ga5_gs3_gc_m1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? ga5 : 0 < i ? gs3 : &gc;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_ga5_gs3_gc_0 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? ga5 : 0 < i ? gs3 : &gc;

  A (snprintf (0, 0, "%s", p) == 0);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_ga5_gs3_gc_p1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? ga5 : 0 < i ? gs3 : &gc;

  A (snprintf (0, 0, "%s", p) == 3);
}


__attribute__ ((noclone, noinline, noipa)) void
min_4_gs3_gc_ga5_m1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? gs3 : 0 < i ? &gc : ga5;

  A (snprintf (0, 0, "%s", p) == 3);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_gs3_gc_ga5_0 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? gs3 : 0 < i ? &gc : ga5;

  A (snprintf (0, 0, "%s", p) == 4);
}

__attribute__ ((noclone, noinline, noipa)) void
min_4_gs3_gc_ga5_p1 (int i)
{
  gc = 0;
  memcpy (ga5, "1234", 4);
  const char *p = i < 0 ? gs3 : 0 < i ? &gc : ga5;

  A (snprintf (0, 0, "%s", p) == 0);
}


int main (void)
{
  equal_4_gs0_gs3_ga5_m1 (-1);
  equal_4_gs0_gs3_ga5_0  ( 0);
  equal_4_gs0_gs3_ga5_p1 (+1);

  equal_4_gs0_ga5_gs3_m1 (-1);
  equal_4_gs0_ga5_gs3_0  ( 0);
  equal_4_gs0_ga5_gs3_p1 (+1);

  equal_4_ga5_gs0_gs3_m1 (-1);
  equal_4_ga5_gs0_gs3_0  ( 0);
  equal_4_ga5_gs0_gs3_p1 (+1);

  equal_4_ga5_gs3_gs0_m1 (-1);
  equal_4_ga5_gs3_gs0_0  ( 0);
  equal_4_ga5_gs3_gs0_p1 (+1);

  equal_4_gs3_gs0_ga5_m1 (-1);
  equal_4_gs3_gs0_ga5_0  ( 0);
  equal_4_gs3_gs0_ga5_p1 (+1);

  /* Same as aabove but with memcpy creating a string at least four
     characters long.  */
  memset (ga5, 0, sizeof ga5);
  min_4_gc_gs3_ga5_m1 (-1);
  memset (ga5, 0, sizeof ga5);
  min_4_gc_gs3_ga5_0  ( 0);
  memset (ga5, 0, sizeof ga5);
  min_4_gc_gs3_ga5_p1 (+1);

  memset (ga5, 0, sizeof ga5);
  min_4_gc_ga5_gs3_m1 (-1);
  memset (ga5, 0, sizeof ga5);
  min_4_gc_ga5_gs3_0  ( 0);
  memset (ga5, 0, sizeof ga5);
  min_4_gc_ga5_gs3_p1 (+1);

  memset (ga5, 0, sizeof ga5);
  min_4_ga5_gc_gs3_m1 (-1);
  memset (ga5, 0, sizeof ga5);
  min_4_ga5_gc_gs3_0  ( 0);
  memset (ga5, 0, sizeof ga5);
  min_4_ga5_gc_gs3_p1 (+1);

  memset (ga5, 0, sizeof ga5);
  min_4_ga5_gs3_gc_m1 (-1);
  memset (ga5, 0, sizeof ga5);
  min_4_ga5_gs3_gc_0  ( 0);
  memset (ga5, 0, sizeof ga5);
  min_4_ga5_gs3_gc_p1 (+1);

  memset (ga5, 0, sizeof ga5);
  min_4_gs3_gc_ga5_m1 (-1);
  memset (ga5, 0, sizeof ga5);
  min_4_gs3_gc_ga5_0  ( 0);
  memset (ga5, 0, sizeof ga5);
  min_4_gs3_gc_ga5_p1 (+1);
}
