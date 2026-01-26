/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2" } */

typedef int v2si __attribute__ ((vector_size (8)));
int test (int accumulator, int dummy, v2si v1, v2si v2, v2si v3, v2si v4)
{
  accumulator &= v4[0] & v4[1];
  return accumulator;
}

/* v4 should be passed on the stack.  */
/* { dg-final { scan-assembler "vle32.v\tv\[0-9\]+,0\\(sp\\)" } } */
