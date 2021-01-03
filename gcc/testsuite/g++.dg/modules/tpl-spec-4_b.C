// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

import TPL;

int main ()
{
  X<int> q;

  q.m = 5;

  X<float> p;
  p.f = 4.0f;

  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending specializations keyed to TPL\[0\] '::template X@TPL:.'} module } }
