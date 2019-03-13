// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import <stdio.h>;

int main ()
{
  printf ("hello world!\n");
  return 0;
}

// { dg-final { scan-lang-dump {Bindings '::printf' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::printf'@'<stdio.h>' section:} module } }
// Make sure we don't load spurious stuff
// { dg-final { scan-lang-dump-not {binding '::scanf'@'<stdio.h>'} module } }
