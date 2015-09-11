/* PR sanitizer/66908 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=shift,bounds -O2 -Werror=maybe-uninitialized" } */
/* { dg-additional-options "-std=gnu90" { target c } } */

struct S { int a[22]; };
static int const e[22] = { };

void
foo (struct S const *s, unsigned int m, unsigned int *res)
{
  unsigned int i;
  for (i = 0; i < 22; ++i)
    res[i] = ((s->a[i] + e[i]) << m);
}
