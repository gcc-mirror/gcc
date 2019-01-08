// { dg-additional-options "-fmodules-ts --param lazy-modules=1 -fdump-lang-module" }

import bob;
import stuart;
import kevin;

int main ()
{
  stuart ();
  bob ();
  kevin ();

  return 0;
}

// { dg-final { scan-lang-dump {Freezing 'bob.[^']*'} module } }
// { dg-final { scan-lang-dump {Freezing 'stuart.[^']*'} module } }
// { dg-final { scan-lang-dump {Freezing 'kevin.[^']*'} module } }
// { dg-final { scan-lang-dump {Defrosting 'bob.[^']*'} module } }
// { dg-final { scan-lang-dump {Defrosting 'stuart.[^']*'} module } }
// { dg-final { scan-lang-dump {Defrosting 'kevin.[^']*'} module } }
