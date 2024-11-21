/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target int128 } */

#ifndef TYPE
#define TYPE vector __int128_t
#endif

TYPE
do_adduqm (TYPE p, TYPE q)
{
  return p + q;
}

TYPE
do_subuqm (TYPE p, TYPE q)
{
  return p - q;
}

/* { dg-final { scan-assembler-times "vadduqm" 1 } } */
/* { dg-final { scan-assembler-times "vsubuqm" 1 } } */
