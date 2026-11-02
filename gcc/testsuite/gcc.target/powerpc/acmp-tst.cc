/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */

// Need power8 for l<b,h,q>arx
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

typedef struct udt_1
{
  char *a;
} udt_1t;
typedef struct udt_2
{
  char a;
  char b;
} udt_2t;
typedef struct udt_4
{
  short a;
  short b;
} udt_4t;
typedef struct udt_8
{
  int a;
  int b;
} udt_8t;
typedef struct udt_16
{
  long long a;
  long long b;
} udt_16t;
bool
word_exchange_nqi (char *ptr, char *expected, char *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
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
bool
word_exchange_di (long long *ptr, long long *expected, long long *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_sdi (signed long long *ptr, signed long long *expected,
                   signed long long *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udi (unsigned long long *ptr, unsigned long long *expected,
                   unsigned long long *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_sti (signed __int128 *ptr, signed __int128 *expected,
                   signed __int128 *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_uti (unsigned __int128 *ptr, unsigned __int128 *expected,
                   unsigned __int128 *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_f32 (float *ptr, float *expected, float *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_f64 (double *ptr, double *expected, double *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_f128 (__ieee128 *ptr, __ieee128 *expected, __ieee128 *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_1 (udt_1t *ptr, udt_1t *expected, udt_1t *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_2 (udt_2t *ptr, udt_2t *expected, udt_2t *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_4 (udt_4t *ptr, udt_4t *expected, udt_4t *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_8 (udt_8t *ptr, udt_8t *expected, udt_8t *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_16 (udt_16t *ptr, udt_16t *expected, udt_16t *desired)
{
  return __builtin_ppc_atomic_cas_local (ptr, expected, desired, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}

/* { dg-final { scan-assembler-times {\mlbarx +[0-9]+,[0-9]+,[0-9]+,1} 3 } } */
/* { dg-final { scan-assembler-times {\mlharx +[0-9]+,[0-9]+,[0-9]+,1} 4 } } */
/* { dg-final { scan-assembler-times {\mlwarx +[0-9]+,[0-9]+,[0-9]+,1} 5 } } */
/* { dg-final { scan-assembler-times {\mldarx +[0-9]+,[0-9]+,[0-9]+,1} 6 } } */
/* { dg-final { scan-assembler-times {\mlqarx +[0-9]+,[0-9]+,[0-9]+,1} 4 } } */
