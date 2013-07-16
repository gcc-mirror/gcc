/* { dg-do compile } */

int
foo ()
{
  char s0[5] = {0};
  char s1[5] = {1};

  return __builtin_memcmp (s0, s1, 2);
}
