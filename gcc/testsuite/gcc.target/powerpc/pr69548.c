/* { dg-do assemble } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power8 -Os -mbig" } */

__int128
quad_exchange (__int128 *ptr, __int128 newval)
{
  return __atomic_exchange_n (ptr, newval, __ATOMIC_RELAXED);
}
