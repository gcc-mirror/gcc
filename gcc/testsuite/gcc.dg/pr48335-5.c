/* PR middle-end/48335 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-sra" } */

typedef long long T __attribute__((may_alias));

struct S
{
  _Complex float d __attribute__((aligned (8)));
};

int
f1 (struct S x)
{
  struct S s = x;
  return *(T *) &s.d;
}

int
f2 (struct S x)
{
  struct S s = x;
  return *(char *) &s.d;
}

int
f3 (struct S x)
{
  struct S s = x;
  return ((char *) &s.d)[2];
}

int
f4 (struct S x, int y)
{
  struct S s = x;
  return ((char *) &s.d)[y];
}
