/* PR tree-optimization/90662 - strlen of a string in a vla plus offset
   not folded
   Verify that strlen of pointers to wide character arrays (emulated
   by int16_t) are computed correctly (whether folded or not).
   { dg-do run }
   { dg-options "-O2 -Wall -Wno-incompatible-pointer-types" } */

#include "strlenopt.h"

typedef __INT16_TYPE__ int16_t;

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))

typedef int16_t A5[5];

A5 a5[5];
A5* p[5] = { &a5[4], &a5[3], &a5[2], &a5[1], &a5[0] };

__attribute__ ((noclone, noinline, noipa))
void deref_deref (void)
{
  strcpy (**p, "12345");
  A (strlen (**p) == 5);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_0 (void)
{
  strcpy (*p[0], "");
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_1 (void)
{
  strcpy (*p[1], "12");
  A (strlen (*p[1]) == 2);
  A (strlen (&(*p[1])[1]) == 0);

  A (strlen ((char*)*p[1] + 1) == 1);
  A (strlen ((char*)*p[1] + 2) == 0);
  A (strlen ((char*)*p[1] + 3) == 0);

  A (strlen ((char*)&(*p[1])[1] + 1) == 0);

  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_2 (void)
{
  strcpy (*p[2], "1234");
  A (strlen (*p[2]) == 4);
  A (strlen (&(*p[2])[1]) == 2);
  A (strlen (&(*p[2])[2]) == 0);

  A (strlen ((char*)*p[2] + 1) == 3);
  A (strlen ((char*)*p[2] + 2) == 2);
  A (strlen ((char*)*p[2] + 3) == 1);
  A (strlen ((char*)*p[2] + 4) == 0);
  A (strlen ((char*)*p[2] + 5) == 0);

  A (strlen ((char*)&(*p[2])[1] + 1) == 1);
  A (strlen ((char*)&(*p[2])[1] + 2) == 0);

  A (strlen (*p[1]) == 2);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_3 (void)
{
  strcpy (*p[3], "123456");
  A (strlen (*p[3]) == 6);
  A (strlen (&(*p[3])[1]) == 4);
  A (strlen (&(*p[3])[2]) == 2);
  A (strlen (&(*p[3])[3]) == 0);

  A (strlen ((char*)*p[3] + 1) == 5);
  A (strlen ((char*)*p[3] + 2) == 4);
  A (strlen ((char*)*p[3] + 3) == 3);
  A (strlen ((char*)*p[3] + 4) == 2);
  A (strlen ((char*)*p[3] + 5) == 1);
  A (strlen ((char*)*p[3] + 6) == 0);

  A (strlen (*p[2]) == 4);
  A (strlen (*p[1]) == 2);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_4 (void)
{
  strcpy (*p[4], "12345678");
  A (strlen (*p[4]) == 8);
  A (strlen (&(*p[4])[1]) == 6);
  A (strlen (&(*p[4])[2]) == 4);
  A (strlen (&(*p[4])[3]) == 2);
  A (strlen (&(*p[4])[4]) == 0);

  A (strlen (*p[3]) == 6);
  A (strlen (*p[2]) == 4);
  A (strlen (*p[1]) == 2);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_4_x (void)
{
  strcpy (*p[4], "");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 6);
  A (strlen (*p[2]) == 4);
  A (strlen (*p[1]) == 2);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_3_x (void)
{
  strcpy (&(*p[3])[0], "1");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 1);
  A (strlen (*p[2]) == 4);
  A (strlen (*p[1]) == 2);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_2_x (void)
{
  strcpy (*p[2], "12");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 1);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 2);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_1_x (void)
{
  strcpy (*p[1], "123");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 1);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 3);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_0_x (void)
{
  strcpy (*p[0], "1234");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 1);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 3);
  A (strlen (*p[0]) == 4);
}

int main (void)
{
  deref_deref ();

  deref_idx_0 ();
  deref_idx_1 ();
  deref_idx_2 ();
  deref_idx_3 ();
  deref_idx_4 ();

  deref_idx_4_x ();
  deref_idx_3_x ();
  deref_idx_2_x ();
  deref_idx_1_x ();
  deref_idx_0_x ();
}
