/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

typedef int v4si __attribute__ ((vector_size (16)));
int test (int accumulator, int dummy, v4si v1, v4si v2, v4si v3, v4si v4)
{
  accumulator &= v4[0] & v4[1] & v4[2] & v4[3];
  return accumulator;
}

/* v4 should be passed on the stack.  */
/* { dg-final { scan-assembler "vle32.v\tv\[0-9\]+,0\\(sp\\)" } } */
