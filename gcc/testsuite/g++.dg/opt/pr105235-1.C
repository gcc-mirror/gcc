// PR tree-optimization/105235
// { dg-do compile }
// { dg-options "-O -fno-tree-dominator-opts -fexceptions -fnon-call-exceptions -fno-tree-fre" }

struct S { ~S (); };

double
foo (double d)
{
  S s;
  return __builtin_ilogbl (d) + __builtin_sinl (d);
}
