// PR debug/100469
// { dg-do compile }
// { dg-options "-O2 -fcompare-debug -fno-tree-dse -fno-tree-forwprop -fno-tree-tail-merge --param=sccvn-max-alias-queries-per-access=0" }

struct S
{
  long m;
  S (const S &s)
  {
    m = s.m;
  }
  S (long l)
  {
    m = l;
  }
  bool operatorX (const S &s)
  {
    return m >= s.m;
  }
};

static inline S
bar (S a, S b)
{
  return a.operatorX (b) ? a : b;
}

S
foo (S s)
{
  return bar (s, (S) 0);
}
