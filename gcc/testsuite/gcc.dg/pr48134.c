/* { dg-do compile } */
/* { dg-options "-O2 -fstack-check=specific -fno-tree-dse -fno-tree-fre -fno-tree-loop-optimize -g" } */

struct S
{
  int w, z;
};
struct T
{
  struct S s;
};

int i;

static inline struct S
bar (struct S x)
{
  i++;
  return x;
}

int
foo (struct T t, struct S s)
{
  struct S *c = &s;
  if (i)
    c = &t.s;
  t.s.w = 3;
  s = bar (*c);
  return t.s.w;
}
