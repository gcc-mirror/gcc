/* Test __builtin_{add,sub}_overflow_p on {,un}signed long long int.  */
/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */

typedef signed long long int S;
typedef unsigned long long int U;
#define COND 1
#define SHIFT ((__SIZEOF_LONG_LONG__ - 1) * __CHAR_BIT__)
#define S_MAX __LONG_LONG_MAX__
#define S_MIN (-__LONG_LONG_MAX__ - 1)
#if __SIZEOF_INT128__ > __SIZEOF_LONG_LONG__
typedef __int128 W;
#else
#undef COND
#define COND 0
#endif
#include "builtin-arith-overflow-p-7.c"
