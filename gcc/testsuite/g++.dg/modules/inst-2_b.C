// { dg-additional-options {-fmodules-ts -fdump-lang-module-uid-alias} }
import foo;

int main ()
{
  foo (1);  // new inst
  user (); // merge inst
  foo (1);  // reuse inst
  return 0;
}

// { dg-final { scan-lang-dump {Reading mergeable:-4 function_decl} module } }
// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\] '::foo<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 '::foo<int>'} module } }

// { dg-final { scan-lang-dump {Read:-4 matched mergeable specialization function_decl:'::foo<int>'} module } }
// { dg-final { scan-lang-dump {Reading mergeable:-4 function_decl} module } }
// { dg-final { scan-lang-dump {Deduping '::foo<int>'} module } }
// { dg-final { scan-lang-dump {Recording new skippable function_decl:'::foo<int>'} module } }
