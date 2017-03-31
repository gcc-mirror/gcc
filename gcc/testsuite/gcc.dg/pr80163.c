/* PR middle-end/80163 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O0" } */

void bar (void);

__int128_t *
foo (void)
{
a:
  bar ();
b:;
  static __int128_t d = (long) &&a - (long) &&b;	/* { dg-error "initializer element is not computable at load time" } */
  return &d;
}

__int128_t *
baz (void)
{
  static __int128_t d = (long) (3 * 4);
  return &d;
}
