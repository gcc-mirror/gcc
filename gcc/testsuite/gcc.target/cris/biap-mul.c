/* Make sure ADDI is used for trivial multiplications too.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\taddi" 2 } } */
/* { dg-final { scan-assembler-not "\tlsl|\tmul|\tmove|\tadd\[^i\]" } } */

int xyzzy (int r10)
{
  return r10 * 5;
}

int plugh (int r10)
{
  return r10 * 3;
}
