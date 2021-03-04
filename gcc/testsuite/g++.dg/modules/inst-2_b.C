// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias} }
import foo;

int main ()
{
  foo (1);  // read pending inst
  user (); // 
  foo (1);  // reuse inst
  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending entities keyed to '::foo'} module } }

// { dg-final { scan-lang-dump {Read:-[0-9]*'s decl spec merge key \(new\) function_decl:'::foo'} module } }
