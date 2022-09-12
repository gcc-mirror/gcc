/* { dg-do link } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -mdejagnu-cpu=power10" } */

/* Verify there is no ICE in LTO mode.  */

int main ()
{
  float *b;
  const __vector_quad c;
  __builtin_mma_disassemble_acc (b, &c);
  return 0;
}
