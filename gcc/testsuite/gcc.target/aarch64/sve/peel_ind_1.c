/* { dg-do compile } */
/* Pick an arbitrary target for which unaligned accesses are more
   expensive.  */
/* { dg-options "-O3 -msve-vector-bits=256 -mtune=thunderx" } */

#define N 512
#define START 1
#define END 505

int x[N] __attribute__((aligned(32)));

void __attribute__((noinline, noclone))
foo (void)
{
  unsigned int v = 0;
  for (unsigned int i = START; i < END; ++i)
    {
      x[i] = v;
      v += 5;
    }
}

/* We should operate on aligned vectors.  */
/* { dg-final { scan-assembler {\t(adrp|adr)\tx[0-9]+, x\n} } } */
/* We should use an induction that starts at -5, with only the last
   7 elements of the first iteration being active.  */
/* { dg-final { scan-assembler {\tindex\tz[0-9]+\.s, #-5, #5\n} } } */
