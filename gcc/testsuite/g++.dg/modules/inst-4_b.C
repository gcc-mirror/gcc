// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias-uid} }
import foo;

int main ()
{
  TPL<int> x;
  if (user (1) != 1)
    return 1;
  return 0;
}

// { dg-final { scan-lang-dump {Read:-10 matched mergeable specialization type_decl:'::TPL<int>'} module } }
// { dg-final { scan-lang-dump {Deduping '::TPL<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::TPL<int>'} module } }
