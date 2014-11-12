#define SCHAR_MAX __SCHAR_MAX__
#define SCHAR_MIN (-__SCHAR_MAX__ - 1)
#define SHRT_MAX __SHRT_MAX__
#define SHRT_MIN (-__SHRT_MAX__ - 1)
#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)
#define LONG_MAX __LONG_MAX__
#define LONG_MIN (-__LONG_MAX__ - 1)
#define LLONG_MAX __LONG_LONG_MAX__
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1)

int v;

__attribute__((noinline, noclone)) void
bar (void)
{
  v++;
}

#define T(n, t1, t2, tr, v1, v2, vr, b, o) \
__attribute__((noinline, noclone)) tr		\
t##n##_1##b (t1 x, t2 y)			\
{						\
  tr r;						\
  if (__builtin_##b##_overflow (x, y, &r))	\
    bar ();					\
  return r;					\
}						\
						\
__attribute__((noinline, noclone)) tr		\
t##n##_2##b (t2 y)				\
{						\
  t1 x = (v1);					\
  tr r;						\
  if (__builtin_##b##_overflow (x, y, &r))	\
    bar ();					\
  return r;					\
}						\
						\
__attribute__((noinline, noclone)) tr		\
t##n##_3##b (t2 y)				\
{						\
  tr r;						\
  if (__builtin_##b##_overflow ((t1) (v1), y,	\
				&r))		\
    bar ();					\
  return r;					\
}						\
						\
__attribute__((noinline, noclone)) tr		\
t##n##_4##b (t1 x)				\
{						\
  t2 y = (v2);					\
  tr r;						\
  if (__builtin_##b##_overflow (x, y, &r))	\
    bar ();					\
  return r;					\
}						\
						\
__attribute__((noinline, noclone)) tr		\
t##n##_5##b (t1 x)				\
{						\
  tr r;						\
  if (__builtin_##b##_overflow (x, (t2) (v2),	\
				&r))		\
    bar ();					\
  return r;					\
}						\
						\
__attribute__((noinline, noclone)) void		\
t##n##b (void)					\
{						\
  t1 x = (v1);					\
  t2 y = (v2);					\
  tr r1, r2;					\
  v = 0;					\
  if (t##n##_1##b (x, y) != (tr) (vr)		\
      || t##n##_2##b (y) != (tr) (vr)		\
      || t##n##_3##b (y) != (tr) (vr)		\
      || t##n##_4##b (x) != (tr) (vr)		\
      || t##n##_5##b (x) != (tr) (vr))		\
    __builtin_abort ();				\
  if (__builtin_##b##_overflow (x, y, &r1))	\
    bar ();					\
  if (r1 != (tr) (vr))				\
    __builtin_abort ();				\
  if (__builtin_##b##_overflow ((t1) (v1),	\
				(t2) (v2), &r2))\
    bar ();					\
  if (r2 != (tr) (vr) || v != 7 * o)		\
    __builtin_abort ();				\
}
#define ST(n, t, v1, v2, vr, b, o) \
T (n, t, t, t, v1, v2, vr, b, o)
