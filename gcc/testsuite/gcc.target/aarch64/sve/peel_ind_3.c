/* { dg-do compile } */
/* Pick an arbitrary target for which unaligned accesses are more
   expensive.  */
/* { dg-options "-O3 -msve-vector-bits=256 -mtune=thunderx" } */

#define N 32
#define MAX_START 8
#define COUNT 16

int x[MAX_START][N] __attribute__((aligned(32)));

void __attribute__((noinline, noclone))
foo (int start)
{
  for (int i = start; i < start + COUNT; ++i)
    x[start][i] = i;
}

/* We should operate on aligned vectors.  */
/* { dg-final { scan-assembler {\t(adrp|adr)\tx[0-9]+, x\n} } } */
/* { dg-final { scan-assembler {\tubfx\t} } } */
