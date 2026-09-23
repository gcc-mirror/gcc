/* PR middle-end/126447 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

void foo (int);
[[gnu::returns_twice]] void bar ();

_BitInt(575)
baz ()
{
  _BitInt(575) w = 1;
  bar ();
  w *= 3;
  foo (3);
  return w;
}

_BitInt(575)
qux ()
{
  _BitInt(575) w = 1;
  bar ();
  w += 3;
  foo (3);
  return w;
}

_BitInt(575)
fred (_BitInt(575) x)
{
  _BitInt(575) w = 1;
  bar ();
  w -= x;
  foo (3);
  return w;
}
