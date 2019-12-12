/* PR tree-optimization/90662 - strlen of a string in a vla plus offset
   not folded
   Verify that strlen of pointers to char arrays are computed correctly
   (whether folded or not).
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define A(expr)                                                 \
  ((expr)                                                       \
   ? (void)0                                                    \
   : (__builtin_printf ("assertion failed on line %i: %s\n",    \
                        __LINE__, #expr),                       \
      __builtin_abort ()))

typedef char A5[5];

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
  strcpy (*p[1], "1");
  A (strlen (*p[1]) == 1);
  A (strlen (&(*p[1])[1]) == 0);

  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_2 (void)
{
  strcpy (*p[2], "12");
  A (strlen (*p[2]) == 2);
  A (strlen (&(*p[2])[1]) == 1);
  A (strlen (&(*p[2])[2]) == 0);

  A (strlen (*p[1]) == 1);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_3 (void)
{
  strcpy (*p[3], "123");
  A (strlen (*p[3]) == 3);
  A (strlen (&(*p[3])[1]) == 2);
  A (strlen (&(*p[3])[2]) == 1);
  A (strlen (&(*p[3])[3]) == 0);

  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 1);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_4 (void)
{
  strcpy (*p[4], "1234");
  A (strlen (*p[4]) == 4);
  A (strlen (&(*p[4])[1]) == 3);
  A (strlen (&(*p[4])[2]) == 2);
  A (strlen (&(*p[4])[3]) == 1);
  A (strlen (&(*p[4])[4]) == 0);

  A (strlen (*p[3]) == 3);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 1);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_4_x (void)
{
  strcpy (*p[4], "");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 3);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 1);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_3_x (void)
{
  strcpy (&(*p[3])[0], "1");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 1);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 1);
  A (strlen (*p[0]) == 0);
}

__attribute__ ((noclone, noinline, noipa))
void deref_idx_2_x (void)
{
  strcpy (*p[2], "12");
  A (strlen (*p[4]) == 0);
  A (strlen (*p[3]) == 1);
  A (strlen (*p[2]) == 2);
  A (strlen (*p[1]) == 1);
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
