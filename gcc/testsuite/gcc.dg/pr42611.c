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
struct S { int a; char b[L]; };	/* { dg-error "type .struct S. is too large" } */

void
foo (void)
{
  struct S s;   /* { dg-warning "size of .s. \[0-9\]+ bytes exceeds maximum object size \[0-9\]+" } */
  asm volatile ("" : : "r" (&s));
}
