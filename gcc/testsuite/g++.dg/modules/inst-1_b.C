// { dg-additional-options {-fmodules-ts -fdump-lang-module-uid-alias} }
import foo;

int main ()
{
  user ();
  foo ('a'); // new inst
  foo (1);  // find foo's inst
  return 0;
}

// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::foo@foo:2<int>'} module } }
// { dg-final { scan-lang-dump {Reading definition function_decl '::foo@foo:2<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 function_decl:'::foo@foo:2<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort decl:1 \[1\] '::foo@foo:2<float>'} module } }
// { dg-final { scan-lang-dump {Reading definition function_decl '::foo@foo:2<float>'} module } }
// { dg-final { scan-lang-dump {Inserted:-2 horcrux:1@0 function_decl:'::foo@foo:2<float>'} module } }

// { dg-final { scan-lang-dump {Read:-[0-9]* unique mergeable decl function_decl:'::baz@foo:2'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]* unique mergeable decl function_decl:'::baz@foo:2'} module } }
