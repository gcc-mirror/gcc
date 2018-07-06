/* { dg-do compile } */
/* { dg-options "-O2 -fno-sched-critical-path-heuristic -fno-sched-rank-heuristic --param=max-sched-extend-regions-iters=5 --param sched-pressure-algorithm=2" } */

#ifdef __SIZEOF_INT128__
typedef __int128 largeint;
#else
typedef long long largeint;
#endif

largeint a;
int b;

largeint
foo (char d, short e, int f)
{
  b = __builtin_sub_overflow_p (b, 1, (unsigned long)0);
  return a + f;
}
