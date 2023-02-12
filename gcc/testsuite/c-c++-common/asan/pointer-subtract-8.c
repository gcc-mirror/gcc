/* PR middle-end/108543 */
/* { dg-do compile  } */
/* { dg-options "-fno-sanitize=address -fsanitize=kernel-address -fsanitize=pointer-subtract" } */

struct S {
  long _M_p;
};

typedef struct S S;

__PTRDIFF_TYPE__
f (S __x, S __y)
{
  return &__x._M_p - &__y._M_p;
}
