/* PR target/126447 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O0" } */

void foo (int);
[[gnu::returns_twice]] void bar ();

unsigned _BitInt(575)
baz ()
{
  unsigned _BitInt(575) w = 1;
  bar ();
  w = __builtin_bitreverseg (w);
  foo (3);
  return w;
}

unsigned _BitInt(512)
qux ()
{
  unsigned _BitInt(512) w = 1;
  bar ();
  w = __builtin_bswapg (w);
  foo (3);
  return w;
}
