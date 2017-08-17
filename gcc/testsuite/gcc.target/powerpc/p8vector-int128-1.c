/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3" } */

#include <altivec.h>

#ifndef TYPE
#define TYPE vector __int128_t
#endif

TYPE
do_addcuq (TYPE p, TYPE q)
{
  return __builtin_vec_vaddcuq (p, q);
}

TYPE
do_adduqm (TYPE p, TYPE q)
{
  return __builtin_vec_add (p, q);
}

TYPE
do_addeuqm (TYPE p, TYPE q, TYPE r)
{
  return __builtin_vec_vaddeuqm (p, q, r);
}

TYPE
do_addecuq (TYPE p, TYPE q, TYPE r)
{
  return __builtin_vec_vaddecuq (p, q, r);
}

TYPE
do_subeuqm (TYPE p, TYPE q, TYPE r)
{
  return __builtin_vec_vsubeuqm (p, q, r);
}

TYPE
do_subecuq (TYPE p, TYPE q, TYPE r)
{
  return __builtin_vec_vsubecuq (p, q, r);
}

TYPE
do_subcuq (TYPE p, TYPE q)
{
  return __builtin_vec_vsubcuq (p, q);
}

TYPE
do_subuqm (TYPE p, TYPE q)
{
  return __builtin_vec_vsubuqm (p, q);
}

TYPE
do_zero (void)
{
  return (TYPE) { 0 };
}

TYPE
do_minus_one (void)
{
  return (TYPE) { -1 };
}

/* { dg-final { scan-assembler	   "vaddcuq"   } } */
/* { dg-final { scan-assembler	   "vadduqm"   } } */
/* { dg-final { scan-assembler	   "vaddecuq"  } } */
/* { dg-final { scan-assembler	   "vaddeuqm"  } } */
/* { dg-final { scan-assembler	   "vsubecuq"  } } */
/* { dg-final { scan-assembler	   "vsubeuqm"  } } */
/* { dg-final { scan-assembler	   "vsubcuq"   } } */
/* { dg-final { scan-assembler	   "vsubuqm"   } } */
/* { dg-final { scan-assembler-not "mtvsrd"    } } */
/* { dg-final { scan-assembler-not "mfvsrd"    } } */
/* { dg-final { scan-assembler-not "ori 2,2,0" } } */
/* { dg-final { scan-assembler-not "xxpermdi"  } } */
/* { dg-final { scan-assembler-not "stxvd2x"   } } */
/* { dg-final { scan-assembler-not "stxvw4x"   } } */
