// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias-uid} }
import foo;

int main ()
{
  TPL<int> x;
  if (user (1) != 1)
    return 1;
  return 0;
}

// { dg-final { scan-lang-dump {Reading 2 pending specializations keyed} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]* new mergeable specialization type_decl:'::TPL@foo:2'} module } }
// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::TPL@foo:2<int>'} module } }
