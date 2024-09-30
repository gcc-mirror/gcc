/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int
sgeu (unsigned int x, unsigned int y)
{
  return x >= y;
}

/* { dg-final { scan-assembler "sltu\\sa0,a0,a1" } } */
/* { dg-final { scan-assembler-not "andi|sext\\.w" } } */
