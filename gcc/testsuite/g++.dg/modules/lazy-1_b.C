// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

// Check some lazy loading

import foo;

int main ()
{
  bar ();

  baz ();

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar'@'foo' section} "module" } }
// { dg-final { scan-lang-dump {Lazily binding '::baz'@'foo' section} "module" } }
// quux is not referenced, so never loaded
// { dg-final { scan-lang-dump {Bindings '::quux' section} "module" } }
// { dg-final { scan-lang-dump-not {Lazily binding '::quux'@'foo' section} "module" } }
// { dg-final { scan-lang-dump-not {Read -[0-9]* function_decl:'::quux'} "module" } }
