/* { dg-do assemble } */
/* { dg-additional-options "-march=armv8.2-a+sve -mcpu=cortex-a72 -O1 -w -###" } */

int main ()
{
  return 0;
}

/* { dg-message "-march=armv8-a\+crc" "no arch from cpu" { xfail *-*-* } 0 } */
/* { dg-message "-march=armv8\\.2-a\\+sve" "using only sve" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
