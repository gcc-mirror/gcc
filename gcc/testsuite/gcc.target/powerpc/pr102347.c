/* { dg-do link } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -mdejagnu-cpu=power9" } */

/* Verify there are no error messages in LTO mode.  */

#pragma GCC target "cpu=power10"
int main ()
{
  float *b;
  __vector_quad c;
  __builtin_mma_disassemble_acc (b, &c);
  return 0;
}
