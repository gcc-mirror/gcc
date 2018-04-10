// { dg-additional-options "--param lazy-module-files=1 -fdump-lang-mdule" }

import bob;
import stuart;
import kevin;

int main ()
{
  bob ();
  stuart ();
  kevin ();
  return 0;
}

// { dg-final scan-lang-dump {Freezing bob.nms} module } }
// { dg-final scan-lang-dump {Freezing stuart.nms} module } }
// { dg-final scan-lang-dump {Freezing kevin.nms} module } }
// { dg-final scan-lang-dump {Defrosting bob.nms} module } }
// { dg-final scan-lang-dump {Defrosting stuart.nms} module } }
// { dg-final scan-lang-dump {Defrosting kevin.nms} module } }
