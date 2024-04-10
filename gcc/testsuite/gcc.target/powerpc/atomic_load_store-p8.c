/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-final { scan-assembler-times "lq" 1 } } */
/* { dg-final { scan-assembler-times "stq" 1 } } */
/* { dg-final { scan-assembler-not "bl __atomic" } } */
/* { dg-final { scan-assembler-not "lqarx" } } */
/* { dg-final { scan-assembler-not "stqcx" } } */

__int128
atomic_load_128_relaxed (__int128 *ptr)
{
	return __atomic_load_n (ptr, __ATOMIC_RELAXED);
}

void
atomic_store_128_relaxed (__int128 *ptr, __int128 val)
{
	__atomic_store_n (ptr, val, __ATOMIC_RELAXED);
}

