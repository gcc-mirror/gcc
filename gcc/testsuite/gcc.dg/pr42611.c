/* PR other/42611 */
/* { dg-do compile } */
/* { dg-options "" } */

#define L \
  (sizeof (__SIZE_TYPE__) == 1 ? __SCHAR_MAX__				\
  : sizeof (__SIZE_TYPE__) == sizeof (short) ? __SHRT_MAX__		\
  : sizeof (__SIZE_TYPE__) == sizeof (int) ? __INT_MAX__		\
  : sizeof (__SIZE_TYPE__) == sizeof (long) ? __LONG_MAX__		\
  : sizeof (__SIZE_TYPE__) == sizeof (long long) ? __LONG_LONG_MAX__	\
  : __INTMAX_MAX__)
struct S { int a; char b[L]; };

void
foo (void)
{
  struct S s;				/* { dg-error "is too large" } */
  asm volatile ("" : : "r" (&s));
}
