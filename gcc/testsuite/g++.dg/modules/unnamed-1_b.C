// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import PiL;

// Until the linkage is promoted, this won't link.

int main ()
{
  int i = get ();
  if (i)
    return 1;
  int h = hwm ();
  if (h != 1)
    return 2;
  return 0;
}

// { dg-final { scan-lang-dump {> Bindings '::counter'} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::counter'@'PiL' section:1} module } }
