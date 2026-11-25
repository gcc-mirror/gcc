/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */

/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

bool
word_exchange_qi (signed char *ptr, signed char *expected, signed char *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_uqi (unsigned char *ptr, unsigned char *expected,
                   unsigned char *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_hi (short *ptr, short *expected, short *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_shi (signed short *ptr, signed short *expected,
                   signed short *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_uhi (unsigned short *ptr, unsigned short *expected,
                   unsigned short *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_si (int *ptr, int *expected, int *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_ssi (signed int *ptr, signed int *expected, signed int *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_usi (unsigned int *ptr, unsigned int *expected,
                   unsigned int *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}

/* { dg-final { scan-assembler-times {\mlbarx +[0-9]+,[0-9]+,[0-9]+,1} 2 } } */
/* { dg-final { scan-assembler-times {\mlharx +[0-9]+,[0-9]+,[0-9]+,1} 3 } } */
/* { dg-final { scan-assembler-times {\mlwarx +[0-9]+,[0-9]+,[0-9]+,1} 3 } } */
