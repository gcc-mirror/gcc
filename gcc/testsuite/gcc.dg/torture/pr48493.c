/* { dg-do compile } */

typedef long long T __attribute__((may_alias, aligned (1)));

struct S
{
  _Complex float d __attribute__((aligned (8)));
};

void bar (struct S);

void
f1 (T x)
{
  struct S s;
  *(T *) ((char *) &s.d + 1) = x;
  bar (s);
}
