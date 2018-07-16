/* { dg-do compile } */
/* { dg-options "-m31 -mesa -march=g5 -fPIC -pg -mnop-mcount -Wno-deprecated" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler "NOPs for -mnop-mcount \\(16 halfwords\\)\n.*bc\t0,0\n.*bc\t0,0\n.*bc\t0,0\n.*bc\t0,0\n.*bc\t0,0\n.*bc\t0,0\n.*bc\t0,0\n.*bc\t0,0" } } */
}
