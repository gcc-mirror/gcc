// PR debug/100852
// { dg-do compile }
// { dg-options "-Og -fif-conversion -fno-tree-ccp -fno-tree-copy-prop -fcompare-debug" }

static inline int
min (unsigned a, int b)
{
  return a < b ? a : b;
}

struct S { S (char); };

static inline S
foo (unsigned x)
{
  int h;
  h += min (x * 4, h);
  return h;
}

void
bar ()
{
  foo (0);
}
