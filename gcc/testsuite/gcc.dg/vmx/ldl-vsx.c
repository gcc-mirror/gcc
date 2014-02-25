/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx" } */

#include "harness.h"

static unsigned long svul[2] __attribute__ ((aligned (16)));
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
  vector unsigned long evul = {0,1};
  vector double evd = {0.0,1.0};

  vector unsigned long vul;
  vector double vd;

  init ();

  vul = vec_ldl (0, (vector unsigned long *)svul);
  vd  = vec_ldl (0, (vector double *)svd);

  check (vec_all_eq (vul, evul), "vul");
  check (vec_all_eq (vd,  evd ), "vd" );
}
