// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import hello;
int main (void)
{
  greeter ("world");
  return 0;
}

// { dg-final { scan-lang-dump {Reading definition of '::template basic_string_view@hello:1'} module } }
// { dg-final { scan-lang-dump {Read declaration of '::basic_string_view@hello:1<char>'} module } }
// { dg-final { scan-lang-dump {Read declaration of '::greeter@hello:1'} module } }
