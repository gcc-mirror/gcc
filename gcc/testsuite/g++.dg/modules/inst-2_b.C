// { dg-additional-options {-fmodules-ts -fdump-lang-module-uid-alias} }
import foo;

int main ()
{
  foo (1);  // read pending inst
  user (); // 
  foo (1);  // reuse inst
  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending specializations} module } }
// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::foo@foo:2<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 function_decl:'::foo@foo:2<int>'} module } }

// { dg-final { scan-lang-dump {Read:-[0-9]*'s specialization merge key \(new\) function_decl:'::foo@foo:2'} module } }
