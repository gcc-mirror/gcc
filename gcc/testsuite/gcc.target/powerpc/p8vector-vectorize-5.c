/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

#ifndef SIZE
#define SIZE 1024
#endif

#ifndef ALIGN
#define ALIGN 32
#endif

#ifndef ATTR_ALIGN
#define ATTR_ALIGN __attribute__((__aligned__(ALIGN)))
#endif

#ifndef TYPE
#define TYPE unsigned int
#endif

TYPE in1  [SIZE] ATTR_ALIGN;
TYPE in2  [SIZE] ATTR_ALIGN;
TYPE eqv  [SIZE] ATTR_ALIGN;
TYPE nand1[SIZE] ATTR_ALIGN;
TYPE nand2[SIZE] ATTR_ALIGN;
TYPE orc1 [SIZE] ATTR_ALIGN;
TYPE orc2 [SIZE] ATTR_ALIGN;

void
do_eqv (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    {
      eqv[i] = ~(in1[i] ^ in2[i]);
    }
}

void
do_nand1 (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    {
      nand1[i] = ~(in1[i] & in2[i]);
    }
}

void
do_nand2 (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    {
      nand2[i] = (~in1[i]) | (~in2[i]);
    }
}

void
do_orc1 (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    {
      orc1[i] = (~in1[i]) | in2[i];
    }
}

void
do_orc2 (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    {
      orc1[i] = in1[i] | (~in2[i]);
    }
}

/* { dg-final { scan-assembler-times "xxleqv"  1 } } */
/* { dg-final { scan-assembler-times "xxlnand" 2 } } */
/* { dg-final { scan-assembler-times "xxlorc"  2 } } */
