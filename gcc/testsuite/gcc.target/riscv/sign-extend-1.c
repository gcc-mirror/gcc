/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

signed long
foo1 (int i)
{
  return i >> 31;
}
/* { dg-final { scan-assembler "sraiw\ta\[0-9\],a\[0-9\],31" } } */

signed char
sub2 (long i)
{
  return i >> 24;
}
/* { dg-final { scan-assembler "sraiw\ta\[0-9\],a\[0-9\],24" } } */

signed short
sub3 (long i)
{
  return i >> 16;
}
/* { dg-final { scan-assembler "sraiw\ta\[0-9\],a\[0-9\],16" } } */

/* { dg-final { scan-assembler-not "srai\t" } } */
/* { dg-final { scan-assembler-not "srli\t" } } */
/* { dg-final { scan-assembler-not "srliw\t" } } */
