/* { dg-do compile { target "mips*-*-*" } } */
/* { dg-options "-O2 -mips4" } */
/* { dg-final { scan-assembler "movz" } } */
/* { dg-final { scan-assembler "movn" } } */
/* { dg-final { scan-assembler "movf" } } */

void ext_long (long);

#if __mips < 4
asm ("# movz movn");
#else
long
sub4 (long i, long j, long k)
{
  ext_long (k ? i : j);
}

long
sub5 (long i, long j, int k)
{
  ext_long (!k ? i : j);
}
#endif

#if __mips < 4 || __mips_soft_float
asm ("# movf");
#else
long
sub6 (long i, long j, float f)
{
  ext_long (!f ? i : j);
}
#endif
