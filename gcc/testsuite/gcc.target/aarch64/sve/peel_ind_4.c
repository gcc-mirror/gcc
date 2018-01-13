/* { dg-do compile } */
/* Pick an arbitrary target for which unaligned accesses are more
   expensive.  */
/* { dg-options "-Ofast -msve-vector-bits=256 -mtune=thunderx -fno-vect-cost-model" } */

#define START 1
#define END 505

void __attribute__((noinline, noclone))
foo (double *x)
{
  double v = 10.0;
  for (unsigned int i = START; i < END; ++i)
    {
      x[i] = v;
      v += 5.0;
    }
}

/* We should operate on aligned vectors.  */
/* { dg-final { scan-assembler {\tubfx\t} } } */
