/* { dg-do compile { target int128 } } */

int
foo(unsigned __int128 b)
{
  return __builtin_popcountg(b) == 1;
}

