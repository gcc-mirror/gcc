
/* { dg-do run  } */
/* { dg-options "-O0" } */

#include <limits.h>
#define ABS(x)	(((x) >= 0) ? (x) : -(x))

#define DEF_TEST(TYPE)	\
void foo_##TYPE (signed TYPE x, unsigned TYPE y){	\
    TYPE t = ABS (x);				\
    if (t != y)					\
 	__builtin_abort ();			\
}						\

DEF_TEST (char);
DEF_TEST (short);
DEF_TEST (int);
DEF_TEST (long);

int main ()
{
  foo_char (SCHAR_MIN + 1, SCHAR_MAX);
  foo_char (0, 0);
  foo_char (-1, 1);
  foo_char (1, 1);
  foo_char (SCHAR_MAX, SCHAR_MAX);

  foo_int (-1, 1);
  foo_int (0, 0);
  foo_int (INT_MAX, INT_MAX);
  foo_int (INT_MIN + 1, INT_MAX);

  foo_short (-1, 1);
  foo_short (0, 0);
  foo_short (SHRT_MAX, SHRT_MAX);
  foo_short (SHRT_MIN + 1, SHRT_MAX);

  foo_long (-1, 1);
  foo_long (0, 0);
  foo_long (LONG_MAX, LONG_MAX);
  foo_long (LONG_MIN + 1, LONG_MAX);

  return 0;
}

