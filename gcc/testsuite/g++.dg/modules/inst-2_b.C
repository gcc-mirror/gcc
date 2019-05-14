// { dg-additional-options {-fmodules-ts -fdump-lang-module-uid-alias} }
import foo;

int main ()
{
  foo (1);  // new inst
  user (); // merge inst
  foo (1);  // reuse inst
  return 0;
}

// { dg-final { scan-lang-dump {Reading merged:-5 function_decl} module } }
// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::foo<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 function_decl:'::foo<int>'} module } }

// { dg-final { scan-lang-dump {Read:-5 matched mergeable specialization function_decl:'::foo<int>'} module } }
// { dg-final { scan-lang-dump {Deduping '::foo<int>'} module } }
// { dg-final { scan-lang-dump {Recording new skippable function_decl:'::foo<int>'} module } }
