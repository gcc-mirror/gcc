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

// { dg-final { scan-lang-dump {Read:-[0-9]*'s specialization merge key \(new\) type_decl:'::TPL@foo:2'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s specialization merge key \(new\) type_decl:'::TPL@foo:2<T>::TPL'} module } }
