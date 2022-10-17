/* Verify t0 is saved before use.  */
/* { dg-do compile } */
/* { dg-options "-fomit-frame-pointer" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */
void __attribute__ ((interrupt))
foo (void)
{
  char array[4096];
}
/* { dg-final { scan-assembler "s\[wd\]\tt0" } } */
