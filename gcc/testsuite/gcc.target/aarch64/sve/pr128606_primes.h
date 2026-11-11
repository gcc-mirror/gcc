
#ifndef ELEM_TYPE
#error "No element type defined"
#endif

#include <arm_sve.h>

#define JOIN2(A, B) A##B
#define JOIN(A, B) JOIN2 (A, B)
#define VEC_TYPE JOIN (sv, ELEM_TYPE)
#define LIT _Literal (typeof ((ELEM_TYPE) __COUNTER__))

#if (NUM_PRIMES < 1) || (NUM_PRIMES > 64) || (NUM_PRIMES != (NUM_PRIMES & ~(NUM_PRIMES - 1)))
#error "Unsupported value of NUM_PRIMES"
#endif

enum
{
  start = __COUNTER__
};

VEC_TYPE
__GIMPLE (startwith ("expand")) foo (void)
{
  VEC_TYPE v;

  v = _Literal (VEC_TYPE){LIT 2
#if NUM_PRIMES > 1
			  , LIT 3
#endif
#if NUM_PRIMES > 2

			  , LIT 5,  LIT 7
#endif
#if NUM_PRIMES > 4

			  , LIT 11, LIT 13, LIT 17, LIT 19
#endif
#if NUM_PRIMES > 8
			  , LIT 23, LIT 29, LIT 31, LIT 37, LIT 41, LIT 43, LIT 47, LIT 53
#endif
#if NUM_PRIMES > 16
			  , LIT 59, LIT 61, LIT 67, LIT 71, LIT 73, LIT 79, LIT 83, LIT 89,
			    LIT 97, LIT 101, LIT 103, LIT 107, LIT 109, LIT 113, LIT 127, LIT 131
#endif
#if NUM_PRIMES > 32
			  , LIT 137, LIT 139, LIT 149, LIT 151, LIT 157, LIT 163, LIT 167, LIT 173,
			    LIT 179, LIT 181, LIT 191, LIT 193, LIT 197, LIT 199, LIT 211, LIT 223,
			    LIT 227, LIT 229, LIT 233, LIT 239, LIT 241, LIT 251, LIT 241, LIT 239,
			    LIT 233, LIT 229, LIT 227, LIT 223, LIT 211, LIT 199, LIT 197, LIT 193
#endif
  };

  return v;
}

_Static_assert (__COUNTER__ - 1 - start == NUM_PRIMES,
		"Incorrect number of initialisers");

#undef JOIN
#undef JOIN2
#undef LIT
#undef ELEM_TYPE
#undef VEC_TYPE
