/* PR middle-end/80163 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O0" } */

typedef __INTPTR_TYPE__ intptr_t;
void bar (void);

__int128_t *
foo (void)
{
a:
  bar ();
b:;
  static __int128_t d = (intptr_t) &&a - (intptr_t) &&b;	/* { dg-error "initializer element is not computable at load time" } */
  return &d;
}

__int128_t *
baz (void)
{
  static __int128_t d = (long) (3 * 4);
  return &d;
}
