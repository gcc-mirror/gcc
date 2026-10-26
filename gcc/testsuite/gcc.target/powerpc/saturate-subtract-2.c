/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_assemble_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Check that saturating subtract (subfus) is generated.  Check that all
   combinations of >, >=, <, and <= are optimized.  */

#ifndef TYPE
#define TYPE	unsigned long long
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

/* { dg-final { scan-assembler-times {\msubdus\M} 4 } } */
/* { dg-final { scan-assembler-not   {\mcmpld\M}    } } */
/* { dg-final { scan-assembler-not   {\misel\M}     } } */
/* { dg-final { scan-assembler-not   {\msubf\M}     } } */
