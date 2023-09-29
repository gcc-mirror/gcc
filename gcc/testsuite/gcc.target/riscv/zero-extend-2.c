/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
void
sub (unsigned int wc, unsigned long step, unsigned char *start)
{
  do
    {
      start[--step] = wc;
      wc >>= 6;
    }
  while (step > 1);
}
/* { dg-final { scan-assembler-times {\msext\.w\M} 0 } } */
