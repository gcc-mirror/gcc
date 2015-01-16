/* Check reciprocal estimate functions for accuracy.  */

#ifdef __LP64__
typedef unsigned long uns64_t;
#define UNUM64(x) x ## L

#else
typedef unsigned long long uns64_t;
#define UNUM64(x) x ## LL
#endif

typedef unsigned int uns32_t;

#define TNAME2(x) #x
#define TNAME(x) TNAME2(x)

/*
 * Float functions.
 */

#define TYPE float
#define NAME(PREFIX) PREFIX ## _float
#define UNS_TYPE uns32_t
#define UNS_ABS __builtin_abs
#define EXP_SIZE 8
#define MAN_SIZE 23
#define FABS __builtin_fabsf
#define FMAX __builtin_fmaxf
#define FMIN __builtin_fminf
#define SQRT __builtin_sqrtf
#define RMIN 1.0e-10
#define RMAX 1.0e+10
#define BDIV 1
#define BRSQRT 2
#define ASMDIV "fdivs"
#define ASMSQRT "fsqrts"

#define INIT_DIV							\
{									\
  { 0x4fffffff },	/* 8589934080 */				\
  { 0x4effffff },	/* 2147483520 */				\
  { 0x40ffffff },	/* 7.99999952316284 */				\
  { 0x3fffffff },	/* 1.99999988079071 */				\
  { 0x417fffff },	/* 15.9999990463257 */				\
  { 0x42ffffff },	/* 127.999992370605 */				\
  { 0x3dffffff },	/* 0.124999992549419 */				\
  { 0x3effffff },	/* 0.499999970197678 */				\
}

#define INIT_RSQRT							\
{									\
  { 0x457ffffe },	/* 4096 - small amount */			\
  { 0x4c7fffff },	/* 6.71089e+07 */				\
  { 0x3d7fffff },	/* 0.0625 - small amount */			\
  { 0x307ffffe },	/* 9.31322e-10 */				\
  { 0x4c7ffffe },	/* 6.71089e+07 */				\
  { 0x397ffffe },	/* 0.000244141 */				\
  { 0x2e7fffff },	/* 5.82077e-11 */				\
  { 0x2f7fffff },	/* 2.32831e-10 */				\
}


#include "recip-test2.h"

/*
 * Double functions.
 */

#undef TYPE
#undef NAME
#undef UNS_TYPE
#undef UNS_ABS
#undef EXP_SIZE
#undef MAN_SIZE
#undef FABS
#undef FMAX
#undef FMIN
#undef SQRT
#undef RMIN
#undef RMAX
#undef BDIV
#undef BRSQRT
#undef ASMDIV
#undef ASMSQRT
#undef INIT_DIV
#undef INIT_RSQRT

#define TYPE double
#define NAME(PREFIX) PREFIX ## _double
#define UNS_TYPE uns64_t
#define UNS_ABS __builtin_imaxabs
#define EXP_SIZE 11
#define MAN_SIZE 52
#define FABS __builtin_fabs
#define FMAX __builtin_fmax
#define FMIN __builtin_fmin
#define SQRT __builtin_sqrt
#define RMIN 1.0e-100
#define RMAX 1.0e+100
#define BDIV 1
#define BRSQRT 2
#define ASMDIV "fdiv"
#define ASMSQRT "fsqrt"

#define INIT_DIV							\
{									\
  { UNUM64 (0x2b57be53f2a2f3a0) },	/* 6.78462e-100 */		\
  { UNUM64 (0x2b35f8e8ea553e52) },	/* 1.56963e-100 */		\
  { UNUM64 (0x2b5b9d861d2fe4fb) },	/* 7.89099e-100 */		\
  { UNUM64 (0x2b45dc44a084e682) },	/* 3.12327e-100 */		\
  { UNUM64 (0x2b424ce16945d777) },	/* 2.61463e-100 */		\
  { UNUM64 (0x2b20b5023d496b50) },	/* 5.96749e-101 */		\
  { UNUM64 (0x2b61170547f57caa) },	/* 9.76678e-100 */		\
  { UNUM64 (0x2b543b9d498aac37) },	/* 5.78148e-100 */		\
}

#define INIT_RSQRT							\
{									\
  { UNUM64 (0x2b616f2d8cbbc646) },	/* 9.96359e-100 */		\
  { UNUM64 (0x2b5c4db2da0a011d) },	/* 8.08764e-100 */		\
  { UNUM64 (0x2b55a82d5735b262) },	/* 6.1884e-100 */		\
  { UNUM64 (0x2b50b52908258cb8) },	/* 4.77416e-100 */		\
  { UNUM64 (0x2b363989a4fb29af) },	/* 1.58766e-100 */		\
  { UNUM64 (0x2b508b9f6f4180a9) },	/* 4.7278e-100 */		\
  { UNUM64 (0x2b4f7a1d48accb40) },	/* 4.49723e-100 */		\
  { UNUM64 (0x2b1146a37372a81f) },	/* 3.08534e-101 */		\
  { UNUM64 (0x2b33f876a8c48050) },	/* 1.42663e-100 */		\
}

#include "recip-test2.h"

int
main (int argc __attribute__((__unused__)),
      char *argv[] __attribute__((__unused__)))
{
  srand48 (1);
  run_float ();

#ifdef VERBOSE
  printf ("\n");
#endif

  run_double ();

  if (error_count_float != 0 || error_count_double != 0)
    abort ();

  return 0;
}
