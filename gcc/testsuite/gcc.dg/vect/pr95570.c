/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve -msve-vector-bits=256 -mstrict-align -fwrapv" { target aarch64*-*-* } } */

int x[8][32];

void
foo (int start)
{
  for (int i = start; i < start + 16; i++)
    x[start][i] = i;
}
