
// Check some lazy loading
// { dg-additional-options -fdump-lang-module }

import foo;

int main ()
{
  bar ();

  baz ();

  return 0;
}

// { dg-final { scan-lang-dump {Lazily loading '::bar'@'foo' section} "module" } }
// { dg-final { scan-lang-dump {Lazily loading '::baz'@'foo' section} "module" } }
// quux is not referenced, so never loaded
// { dg-final { scan-lang-dump {Bindings '::quux' section} "module" } }
// { dg-final { scan-lang-dump-not {Lazily loading '::quux'@'foo' section} "module" } }
// { dg-final { scan-lang-dump-not {Read -[0-9]* function_decl:'::quux'} "module" } }
