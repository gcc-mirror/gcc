// { dg-do assemble  }
// { dg-options "-W -Wall" }
// Don't warn about these comparisons.

struct A {
  unsigned int b : 14;
};

int f (int i, unsigned char u, A a, unsigned long ul)
{
  if ((u & 0x10) == 0)
    return 1;
  if (i == 0U)
    return 1;
  if (a.b > ul)
    return 1;

  return 0;
}
