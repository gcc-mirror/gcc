// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias} }

import TPL;

int main ()
{
  X<int,2> q;

  q.ary[0] = 5;

  X<float,1> p;
  p.scalar = 4.0f;

  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending specializations keyed to '::X@TPL:.<T,#unnamed#>'} module } }
// { dg-final { scan-lang-dump {Adding partial specialization '::X@TPL:.<T,0x1>' to '::X@TPL:.<T,#unnamed#>'} module } }
