/* Test floating-point contraction occurs only within source language
   expressions.  */
/* { dg-do run } */
/* { dg-options "-std=c99 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

volatile float a = 1 + 0x1p-23f, b = 1 - 0x1p-23f, c = -1;

int
main (void)
{
  float av = a, bv = b, cv = c;
  float p = av * bv;
  float r = p + cv;
  if (r == 0)
    exit (0);
  else
    abort ();
}
