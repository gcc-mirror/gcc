/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 16 

const unsigned int in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
unsigned int out[N];

__attribute__ ((noinline)) int
main1 (void)
{
  const unsigned int *pin = &in[1];
  unsigned int *pout = &out[0];

  /* Misaligned load.  */
  *pout++ = *pin++;
  *pout++ = *pin++;
  *pout++ = *pin++;
  *pout++ = *pin++;

  return 0;
}

/* Verify that the assembly contains vector instructions alone
   with no word loads (lw, lwu, lwz, lwzu, or their indexed forms)
   or word stores (stw, stwu, stwx, stwux, or their indexed forms).  */

/* { dg-final { scan-assembler "\t(lvx|lxv|lvsr|stxv)" } } */
/* { dg-final { scan-assembler-not "\tlwz?u?x? " { xfail { powerpc-ibm-aix* } } } } */
/* { dg-final { scan-assembler-not "\tstwu?x? " } } */
