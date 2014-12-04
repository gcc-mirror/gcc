/* Test __builtin_{add,sub}_overflow on {,un}signed char.  */
/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */

#define UCHAR_MAX ((unsigned char) ~0)
#ifndef SHIFT
typedef signed char S;
typedef unsigned char U;
typedef int W;
#define SHIFT 0
#define S_MAX __SCHAR_MAX__
#define S_MIN (-__SCHAR_MAX__ - 1)
#define COND (__SIZEOF_INT__ > 1)
#endif

#define F(n, t1, t2, tr, b) \
__attribute__((noinline, noclone)) tr		\
n (t1 x, t2 y, int *ovf)			\
{						\
  tr res;					\
  *ovf = __builtin_##b##_overflow (x, y, &res);	\
  return res;					\
}

F (spses, S, S, S, add)
F (upueu, U, U, U, add)
F (spseu, S, S, U, add)
F (upues, U, U, S, add)
F (spues, S, U, S, add)
F (upses, U, S, S, add)
F (spueu, S, U, U, add)
F (upseu, U, S, U, add)
F (ssses, S, S, S, sub)
F (usueu, U, U, U, sub)
F (ssseu, S, S, U, sub)
F (usues, U, U, S, sub)
F (ssues, S, U, S, sub)
F (usses, U, S, S, sub)
F (ssueu, S, U, U, sub)
F (usseu, U, S, U, sub)

int
main ()
{
#if COND
  int i, j;
  for (i = 0; i < UCHAR_MAX; i++)
    for (j = 0; j < UCHAR_MAX; j++)
      {
	S s1 = ((W) i << SHIFT) + S_MIN;
	U u1 = ((W) i << SHIFT);
	S s2 = ((W) j << SHIFT) + S_MIN;
	U u2 = ((W) j << SHIFT);
	W w;
	int ovf;
#define T(n, t1, t2, tr, op) \
	w = ((W) t1##1) op ((W) t2##2);		\
	if (n (t1##1, t2##2, &ovf) != (tr) w	\
	    || ovf != (w != (tr) w))		\
	  __builtin_abort ();
	T (spses, s, s, S, +)
	T (upueu, u, u, U, +)
	T (spseu, s, s, U, +)
	T (upues, u, u, S, +)
	T (spues, s, u, S, +)
	T (upses, u, s, S, +)
	T (spueu, s, u, U, +)
	T (upseu, u, s, U, +)
	T (ssses, s, s, S, -)
	T (usueu, u, u, U, -)
	T (ssseu, s, s, U, -)
	T (usues, u, u, S, -)
	T (ssues, s, u, S, -)
	T (usses, u, s, S, -)
	T (ssueu, s, u, U, -)
	T (usseu, u, s, U, -)
      }
#endif
  return 0;
}
