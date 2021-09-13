// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import "stdio-1_a.H";

int main ()
{
  printf ("hello world!\n");
  return 0;
}

// { dg-final { scan-lang-dump {Bindings '::printf' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::printf'@'[^\n]*stdio-1_a.H' section:} module } }
// Make sure we don't load spurious stuff
// { dg-final { scan-lang-dump-not {binding '::scanf'@'[^\n]*'} module } }
