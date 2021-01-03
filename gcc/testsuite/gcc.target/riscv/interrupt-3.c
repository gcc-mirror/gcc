/* Verify t0 is saved before use.  */
/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */
void __attribute__ ((interrupt))
foo (void)
{
  char array[4096];
}
/* { dg-final { scan-assembler "s\[wd\]\tt0" } } */
