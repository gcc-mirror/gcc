// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias-uid} }
import foo;

int main ()
{
  if (user (1) != 1)
    return 1;
  TPL<int> x;
  TPL<float> y;
  return 0;
}

// { dg-final { scan-lang-dump {Read:-10 new mergeable specialization type_decl:'::TPL'} module } }
// { dg-final { scan-lang-dump {Reading mergeable:-10 type_decl} module } }
