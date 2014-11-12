/* Test __builtin_{add,sub}_overflow on {,un}signed int.  */
/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */

typedef signed int S;
typedef unsigned int U;
#define COND 1
#define SHIFT ((__SIZEOF_INT__ - 1) * __CHAR_BIT__)
#define S_MAX __INT_MAX__
#define S_MIN (-__INT_MAX__ - 1)
#if __SIZEOF_LONG__ > __SIZEOF_INT__
typedef long int W;
#elif __SIZEOF_LONG_LONG__ > __SIZEOF_INT__
typedef long long int W;
#elif __SIZEOF_INT128__ > __SIZEOF_INT__
typedef __int128 W;
#else
#undef COND
#define COND 0
#endif
#include "builtin-arith-overflow-7.c"
