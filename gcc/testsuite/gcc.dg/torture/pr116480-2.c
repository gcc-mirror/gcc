/* { dg-do compile { target bitint } } */

int
foo(unsigned _BitInt(127) b)
{
  return __builtin_popcountg(b) == 1;
}

