/* { dg-do assemble { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -Os -mbig" } */

__int128
quad_exchange (__int128 *ptr, __int128 newval)
{
  return __atomic_exchange_n (ptr, newval, __ATOMIC_RELAXED);
}
