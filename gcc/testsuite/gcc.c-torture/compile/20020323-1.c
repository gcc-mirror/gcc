/* This testcase caused ICE on powerpc at -O3, because regrename did
   not handle match_dup of match_operator if the RTLs were not shared.  */

struct A
{
  unsigned char *a0, *a1;
  int a2;
};

void bar (struct A *);

unsigned int
foo (int x)
{
  struct A a;
  unsigned int b;

  if (x < -128 || x > 255 || x == -1)
    return 26;

  a.a0 = (unsigned char *) &b;
  a.a1 = a.a0 + sizeof (unsigned int);
  a.a2 = 0;
  bar (&a);
  return b;
}
