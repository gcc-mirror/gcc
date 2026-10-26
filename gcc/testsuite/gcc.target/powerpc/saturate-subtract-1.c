/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */
/* { dg-require-effective-target powerpc_future_assemble_ok } */

/* Check that saturating subtract (subfus) is generated.  Check that all
   combinations of >, >=, <, and <= are optimized.  */

#ifndef TYPE
#define TYPE	unsigned int
#endif

void
saturated_subtract_gt (TYPE a, TYPE b, TYPE *p)
{
  *p = (a > b) ? a - b : 0;
}

void
saturated_subtract_ge (TYPE a, TYPE b, TYPE *p)
{
  *p = (a >= b) ? a - b : 0;
}

void
saturated_subtract_lt (TYPE a, TYPE b, TYPE *p)
{
  *p = (a < b) ? 0 : a - b;
}

void
saturated_subtract_le (TYPE a, TYPE b, TYPE *p)
{
  *p = (a <= b) ? 0 : a - b;
}

/* { dg-final { scan-assembler-times {\msubwus\M} 4 } } */
/* { dg-final { scan-assembler-not   {\mcmplw\M}    } } */
/* { dg-final { scan-assembler-not   {\misel\M}     } } */
/* { dg-final { scan-assembler-not   {\msubf\M}     } } */
