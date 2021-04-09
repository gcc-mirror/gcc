// { dg-additional-options {-fmodules-ts -fdump-lang-module-uid-alias} }
import foo;

int main ()
{
  user ();
  foo ('a'); // new inst
  foo (1);  // find foo's inst
  return 0;
}

// { dg-final { scan-lang-dump {Reading definition function_decl '::foo@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Reading definition function_decl '::foo@foo:.<float>'} module } }

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(unique\) function_decl:'::baz'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(unique\) function_decl:'::baz'} module } }
