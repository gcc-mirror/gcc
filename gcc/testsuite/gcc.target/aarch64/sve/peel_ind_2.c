/* { dg-do compile } */
/* Pick an arbitrary target for which unaligned accesses are more
   expensive.  */
/* { dg-options "-O3 -msve-vector-bits=256 -mtune=thunderx" } */

#define N 512
#define START 7
#define END 22

int x[N] __attribute__((aligned(32)));

void __attribute__((noinline, noclone))
foo (void)
{
  for (unsigned int i = START; i < END; ++i)
    x[i] = i;
}

/* We should operate on aligned vectors.  */
/* { dg-final { scan-assembler {\t(adrp|adr)\tx[0-9]+, x\n} } } */
/* We should unroll the loop three times.  */
/* { dg-final { scan-assembler-times "\tst1w\t" 3 } } */
