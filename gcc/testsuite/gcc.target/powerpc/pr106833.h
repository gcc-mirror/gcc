/* Header file for pr106833.c test - contains test functions only */

/* Verify there is no ICE in LTO mode.  */

int main ()
{
  float *b;
  const __vector_quad c;
  __builtin_mma_disassemble_acc (b, &c);
  return 0;
}
