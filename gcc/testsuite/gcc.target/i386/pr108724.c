/* { dg-do compile } */
/* { dg-options "-O3 -mno-sse" } */

int a[16], b[16], c[16];
void foo()
{
  for (int i = 0; i < 16; i++) {
    a[i] = b[i] + c[i];
  }
}

/* When this is vectorized this shouldn't be expanded piecewise again
   which will result in spilling for the upper half access.  */

/* { dg-final { scan-assembler-not "\\\[er\\\]sp" } } */
