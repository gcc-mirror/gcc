/* { dg-do compile } */
/* { dg-options "-O0 -std=c99" } */

__fp16
foo ()
{
  /* { dg-final { scan-assembler "movi\tv\[0-9\]+\.8b" } } */
  return 0x1.544p5;
}
