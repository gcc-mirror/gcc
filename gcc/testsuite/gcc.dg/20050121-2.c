/* PR middle-end/19551 */
/* { dg-options "-O2" } */
/* { dg-do link } */

extern void abort ();

#define T(type, name) \
extern __attribute__((pure)) _Complex type	\
foo_c##name (int x);				\
						\
void						\
bar_c##name (type *x)				\
{						\
  type f = __real foo_c##name (5);		\
  if (0) *x = f;				\
}						\
						\
void						\
baz_c##name (type *x)				\
{						\
  type f = __imag foo_c##name (5);		\
  if (0) *x = f;				\
}						\
						\
extern __attribute__((pure)) type		\
foo_##name (int x);				\
						\
void						\
bar_##name (type *x)				\
{						\
  type f = foo_##name (5);			\
  if (0) *x = f;				\
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
    bar_c##name (&var);				\
    baz_c##name (&var);				\
    bar_##name (&var);				\
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
