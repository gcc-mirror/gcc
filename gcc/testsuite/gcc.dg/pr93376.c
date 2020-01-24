/* PR target/93376 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -finline-functions-called-once" } */

unsigned a, b, c;

int
bar (int x)
{
  short s = __builtin_sub_overflow (~x, 0, &b);
  a = __builtin_ffsll (~x);
  return __builtin_add_overflow_p (-(unsigned __int128) a, s,
				   (unsigned __int128) 0);
}

void
foo (void)
{
  c = bar (0);
}
