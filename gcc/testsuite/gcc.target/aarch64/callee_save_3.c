/* { dg-options "-O2" } */

float test ();
float g;

float foo (float x, float y) {
  g = x + test ();
  return (x + test ()) * y;
}

/* { dg-final { scan-assembler {\tstp\td14, d15, \[sp,} } } */
/* { dg-final { scan-assembler {\tldp\td14, d15, \[sp,} } } */
