/* PR middle-end/48335 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-sra" } */

typedef short U __attribute__((may_alias, aligned (1)));

struct S
{
  double d;
};

void bar (struct S);

void
f1 (int x)
{
  struct S s = { .d = 0.0 };
  ((U *)((char *) &s.d + 1))[0] = x;
  __real__ s.d *= 7.0;
  bar (s);
}

void
f2 (int x)
{
  struct S s = { .d = 0.0 };
  ((U *)((char *) &s.d + 1))[1] = x;
  __real__ s.d *= 7.0;
  bar (s);
}

void
f3 (int x)
{
  struct S s = { .d = 0.0 };
  ((U *)((char *) &s.d + 1))[2] = x;
  __real__ s.d *= 7.0;
  bar (s);
}

void
f4 (int x)
{
  struct S s = { .d = 0.0 };
  ((U *)((char *) &s.d + 1))[3] = x;
  __real__ s.d *= 7.0;
  bar (s);
}
