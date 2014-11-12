/* Check that -mno-odd-spreg is not supported with -mabi=64.  */
/* { dg-options "-mabi=64 -mno-odd-spreg -mhard-float" } */
/* { dg-error "unsupported combination" "" { target *-*-* } 0 } */

float a;
float
foo ()
{
  float b = a + 1.0f;
  return b;
}
