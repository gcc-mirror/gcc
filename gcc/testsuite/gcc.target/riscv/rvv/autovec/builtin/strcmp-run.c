/* { dg-do run } */
/* { dg-additional-options "-O3 -minline-strcmp" } */

#include <string.h>

int
__attribute__ ((noipa))
foo (const char *s, const char *t)
{
  return __builtin_strcmp (s, t);
}

int
__attribute__ ((noipa, optimize ("0")))
foo2 (const char *s, const char *t)
{
  return strcmp (s, t);
}

#define SZ 10

int main ()
{
  const char *s[SZ]
    = {"",  "asdf", "0", "\0", "!@#$%***m1123fdnmoi43",
       "a", "z",    "1", "9",  "12345678901234567889012345678901234567890"};

  for (int i = 0; i < SZ; i++)
    for (int j = 0; j < SZ; j++)
      if (foo (s[i], s[j]) != foo2 (s[i], s[j]))
        __builtin_abort ();
}
