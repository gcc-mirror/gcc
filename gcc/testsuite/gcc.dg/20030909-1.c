/* Verify that ands are combined.  */
/* { dg-do compile { target arm*-*-* strongarm*-*-* xscale*-*-* } } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "#255.*#255" } } */
int f(int a, int b) { return ((a & 0xff) + (b & 0xff)) & 0xff; }
