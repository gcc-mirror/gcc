/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */
/* { dg-final { scan-assembler-not "lbarx" } } */
/* { dg-final { scan-assembler-not "lharx" } } */
/* { dg-final { scan-assembler-times "lwarx" 18 } } */
/* { dg-final { scan-assembler-times "ldarx" 6 } } */
/* { dg-final { scan-assembler-not "lqarx" } } */
/* { dg-final { scan-assembler-not "stbcx" } } */
/* { dg-final { scan-assembler-not "sthcx" } } */
/* { dg-final { scan-assembler-times "stwcx" 18 } } */
/* { dg-final { scan-assembler-times "stdcx" 6 } } */
/* { dg-final { scan-assembler-not "stqcx" } } */
/* { dg-final { scan-assembler-times "bl __atomic" 6 } } */
/* As since PR59448 GCC promotes consume to acquire, the expected isync count
   is 16 rather than 12.  */
/* { dg-final { scan-assembler-times "isync" 16 } } */
/* { dg-final { scan-assembler-times "lwsync" 8 } } */
/* { dg-final { scan-assembler-not "mtvsrd" } } */
/* { dg-final { scan-assembler-not "mtvsrwa" } } */
/* { dg-final { scan-assembler-not "mtvsrwz" } } */
/* { dg-final { scan-assembler-not "mfvsrd" } } */
/* { dg-final { scan-assembler-not "mfvsrwz" } } */

/* Test for the byte atomic operations on power8 using lbarx/stbcx.  */
char
char_fetch_add_relaxed (char *ptr, int value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}

char
char_fetch_sub_consume (char *ptr, int value)
{
  return __atomic_fetch_sub (ptr, value, __ATOMIC_CONSUME);
}

char
char_fetch_and_acquire (char *ptr, int value)
{
  return __atomic_fetch_and (ptr, value, __ATOMIC_ACQUIRE);
}

char
char_fetch_ior_release (char *ptr, int value)
{
  return __atomic_fetch_or (ptr, value, __ATOMIC_RELEASE);
}

char
char_fetch_xor_acq_rel (char *ptr, int value)
{
  return __atomic_fetch_xor (ptr, value, __ATOMIC_ACQ_REL);
}

char
char_fetch_nand_seq_cst (char *ptr, int value)
{
  return __atomic_fetch_nand (ptr, value, __ATOMIC_SEQ_CST);
}

/* Test for the half word atomic operations on power8 using lharx/sthcx.  */
short
short_fetch_add_relaxed (short *ptr, int value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}

short
short_fetch_sub_consume (short *ptr, int value)
{
  return __atomic_fetch_sub (ptr, value, __ATOMIC_CONSUME);
}

short
short_fetch_and_acquire (short *ptr, int value)
{
  return __atomic_fetch_and (ptr, value, __ATOMIC_ACQUIRE);
}

short
short_fetch_ior_release (short *ptr, int value)
{
  return __atomic_fetch_or (ptr, value, __ATOMIC_RELEASE);
}

short
short_fetch_xor_acq_rel (short *ptr, int value)
{
  return __atomic_fetch_xor (ptr, value, __ATOMIC_ACQ_REL);
}

short
short_fetch_nand_seq_cst (short *ptr, int value)
{
  return __atomic_fetch_nand (ptr, value, __ATOMIC_SEQ_CST);
}

/* Test for the word atomic operations on power8 using lwarx/stwcx.  */
int
int_fetch_add_relaxed (int *ptr, int value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}

int
int_fetch_sub_consume (int *ptr, int value)
{
  return __atomic_fetch_sub (ptr, value, __ATOMIC_CONSUME);
}

int
int_fetch_and_acquire (int *ptr, int value)
{
  return __atomic_fetch_and (ptr, value, __ATOMIC_ACQUIRE);
}

int
int_fetch_ior_release (int *ptr, int value)
{
  return __atomic_fetch_or (ptr, value, __ATOMIC_RELEASE);
}

int
int_fetch_xor_acq_rel (int *ptr, int value)
{
  return __atomic_fetch_xor (ptr, value, __ATOMIC_ACQ_REL);
}

int
int_fetch_nand_seq_cst (int *ptr, int value)
{
  return __atomic_fetch_nand (ptr, value, __ATOMIC_SEQ_CST);
}

/* Test for the double word atomic operations on power8 using ldarx/stdcx.  */
long
long_fetch_add_relaxed (long *ptr, long value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}

long
long_fetch_sub_consume (long *ptr, long value)
{
  return __atomic_fetch_sub (ptr, value, __ATOMIC_CONSUME);
}

long
long_fetch_and_acquire (long *ptr, long value)
{
  return __atomic_fetch_and (ptr, value, __ATOMIC_ACQUIRE);
}

long
long_fetch_ior_release (long *ptr, long value)
{
  return __atomic_fetch_or (ptr, value, __ATOMIC_RELEASE);
}

long
long_fetch_xor_acq_rel (long *ptr, long value)
{
  return __atomic_fetch_xor (ptr, value, __ATOMIC_ACQ_REL);
}

long
long_fetch_nand_seq_cst (long *ptr, long value)
{
  return __atomic_fetch_nand (ptr, value, __ATOMIC_SEQ_CST);
}

/* Test for the quad word atomic operations on power8 using ldarx/stdcx.  */
__int128_t
quad_fetch_add_relaxed (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}

__int128_t
quad_fetch_sub_consume (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_sub (ptr, value, __ATOMIC_CONSUME);
}

__int128_t
quad_fetch_and_acquire (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_and (ptr, value, __ATOMIC_ACQUIRE);
}

__int128_t
quad_fetch_ior_release (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_or (ptr, value, __ATOMIC_RELEASE);
}

__int128_t
quad_fetch_xor_acq_rel (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_xor (ptr, value, __ATOMIC_ACQ_REL);
}

__int128_t
quad_fetch_nand_seq_cst (__int128_t *ptr, __int128_t value)
{
  return __atomic_fetch_nand (ptr, value, __ATOMIC_SEQ_CST);
}
