/* { dg-do compile } */
/* { dg-options "-save-temps -O2" } */

int
foo (int a, int b)
{
  return ((a & (1 << 25)) ? 5 : 4);
}

/* { dg-final { scan-assembler "ubfx\t\[xw\]\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-not "csel\tw\[0-9\]*.*" } } */
