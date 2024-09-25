// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

struct S {
  int x;
  int *ptr;
};

int
main (int argc, char *argv[])
{
  S s;
  int S::* xp = &S::x;
  int* S::* ptrp = &S::ptr;

  s.ptr = new int[64];

  s.*xp = 6;
  for (int i = 0; i < 64; i++)
    (s.*ptrp)[i] = i;

#pragma omp target map(s.*xp, s.*ptrp, (s.*ptrp)[:64])
  /* { dg-message {sorry, unimplemented: pointer-to-member mapping '\*\(\*\(\(\(int\*\*\)\(& s\)\) \+ \(\(sizetype\)ptrp\)\)\)' not supported} "" { target *-*-* } .-1 } */
  /* { dg-message {sorry, unimplemented: pointer-to-member mapping '\*\(\(\(int\*\*\)\(& s\)\) \+ \(\(sizetype\)ptrp\)\)' not supported} "" { target *-*-* } .-2 } */
  /* { dg-message {sorry, unimplemented: pointer-to-member mapping '\*\(\(\(int\*\)\(& s\)\) \+ \(\(sizetype\)xp\)\)' not supported} "" { target *-*-* } .-3 } */
#pragma omp teams distribute parallel for
  for (int i = 0; i < 64; i++)
    {
      (s.*xp)++;
      (s.*ptrp)[i]++;
    }

  assert (s.*xp == 70);
  for (int i = 0; i < 64; i++)
    assert ((s.*ptrp)[i] == i + 1);

  return 0;
}
