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
bool
word_exchange_nqi (char *ptr, char *expected, char *desired,
		   unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_qi (signed char *ptr, signed char *expected, signed char *desired,
		  unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_uqi (unsigned char *ptr, unsigned char *expected,
                   unsigned char *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_hi (short *ptr, short *expected, short *desired,
		  unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_shi (signed short *ptr, signed short *expected,
                   signed short *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_uhi (unsigned short *ptr, unsigned short *expected,
                   unsigned short *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_si (int *ptr, int *expected, int *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_ssi (signed int *ptr, signed int *expected, signed int *desired,
		   unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_usi (unsigned int *ptr, unsigned int *expected,
                   unsigned int *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_di (long long *ptr, long long *expected, long long *desired,
		  unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_sdi (signed long long *ptr, signed long long *expected,
                   signed long long *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udi (unsigned long long *ptr, unsigned long long *expected,
                   unsigned long long *desired, unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_f32 (float *ptr, float *expected, float *desired,
		   unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_f64 (double *ptr, double *expected, double *desired,
		   unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_1 (udt_1t *ptr, udt_1t *expected, udt_1t *desired,
		     unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_2 (udt_2t *ptr, udt_2t *expected, udt_2t *desired,
		     unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_4 (udt_4t *ptr, udt_4t *expected, udt_4t *desired,
		     unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}
bool
word_exchange_udt_8 (udt_8t *ptr, udt_8t *expected, udt_8t *desired,
		     unsigned long long n)
{
  return __builtin_ppc_atomic_cas_local (ptr+n, expected+n, desired+n, 0,
					 __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
}

/* Test if indexed mode addresses are used, r6 must be used which corresponds
   to the argument n in all functions.  */

/* { dg-final { scan-assembler-times {\mlbarx +[0-9]+,[0-9]+,6,1} 3 } } */
/* { dg-final { scan-assembler-times {\mstbcx. +[0-9]+,[0-9]+,6} 3 } } */

/* { dg-final { scan-assembler-times {\mlharx +[0-9]+,[0-9]+,6,1} 4 } } */
/* { dg-final { scan-assembler-times {\msthcx. +[0-9]+,[0-9]+,6} 4 } } */

/* { dg-final { scan-assembler-times {\mlwarx +[0-9]+,[0-9]+,6,1} 5 } } */
/* { dg-final { scan-assembler-times {\mstwcx. +[0-9]+,[0-9]+,6} 5 } } */

/* { dg-final { scan-assembler-times {\mldarx +[0-9]+,[0-9]+,6,1} 6 } } */
/* { dg-final { scan-assembler-times {\mstdcx. +[0-9]+,[0-9]+,6} 6 } } */
