/* PR tree-optimization/127039 */
/* { dg-do compile { target { aarch64*-*-* || riscv64*-*-* } } } */
/* { dg-options "-O3 -fdump-tree-vect-details" } */
/* { dg-additional-options "-march=armv8.2-a+sve --param=vect-partial-vector-usage=0" { target aarch64*-*-* } } */
/* { dg-additional-options "-march=rv64im_zve64f -mabi=lp64" { target riscv64*-*-* } } */
/* { dg-timeout 10 } */

char a;
int b;
unsigned c;
short d;

void
foo (void)
{
  for (short f = 0; f < c; f += 3)
    {
      a ^= d;
      b = b < 0 ? b : 0;
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
