/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include "harness.h"

static unsigned long long svul[2] __attribute__ ((aligned (16)));
static double svd[2] __attribute__ ((aligned (16)));

static void init ()
{
  unsigned int i;
  for (i = 0; i < 2; ++i)
    {
      svul[i] = i;
      svd[i] = i * 1.0;
    }
}

static void test ()
{
  vector unsigned long long evul = {0,1};
  vector double evd = {0.0,1.0};

  vector unsigned long long vul;
  vector double vd;
  unsigned i;

  init ();

  vul = vec_ld (0, (vector unsigned long long *)svul);
  vd  = vec_ld (0, (vector double *)svd);

  for (i = 0; i < 2; ++i)
    {
      check (vul[i] == evul[i], "vul");
      check (vd[i]  == evd[i],  "vd" );
    }
}
