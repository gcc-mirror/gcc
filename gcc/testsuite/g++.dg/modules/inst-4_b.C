// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias-uid} }
import foo;

int main ()
{
  TPL<int> x;
  if (user (1) != 1)
    return 1;
  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending specializations keyed to foo\[.\] '::template TPL@foo:.'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s type spec merge key \(new\) type_decl:'::TPL'} module } }
