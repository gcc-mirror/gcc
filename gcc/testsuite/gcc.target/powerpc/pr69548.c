/* { dg-do assemble { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -Os -mbig" } */

__int128
quad_exchange (__int128 *ptr, __int128 newval)
{
  return __atomic_exchange_n (ptr, newval, __ATOMIC_RELAXED);
}
