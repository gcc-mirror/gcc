// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias} }
import foo;

int main ()
{
  TPL<int> x;
  if (user (1) != 1)
    return 1;
  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending entities keyed to '::TPL'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s type spec merge key \(new\) type_decl:'::TPL'} module } }
