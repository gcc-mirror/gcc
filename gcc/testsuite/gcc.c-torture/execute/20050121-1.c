/* PR middle-end/19551 */

extern void abort ();

#define T(type, name) \
__attribute__((pure)) _Complex type		\
foo_##name (int x)				\
{						\
  _Complex type r;				\
  __real r = x + 1;				\
  __imag r = x - 1;				\
  return r;					\
}						\
						\
void						\
bar_##name (type *x)				\
{						\
  *x = __real foo_##name (5);			\
}						\
						\
void						\
baz_##name (type *x)				\
{						\
  *x = __imag foo_##name (5);			\
}

typedef long double ldouble_t;
typedef long long llong;

T (float, float)
T (double, double)
T (long double, ldouble_t)
T (char, char)
T (short, short)
T (int, int)
T (long, long)
T (long long, llong)
#undef T

int
main (void)
{
#define T(type, name) \
  {						\
    type var = 0;				\
    bar_##name (&var);				\
    if (var != 6)				\
      abort ();					\
    var = 0;					\
    baz_##name (&var);				\
    if (var != 4)				\
      abort ();					\
  }
  T (float, float)
  T (double, double)
  T (long double, ldouble_t)
  T (char, char)
  T (short, short)
  T (int, int)
  T (long, long)
  T (long long, llong)
  return 0;
}
