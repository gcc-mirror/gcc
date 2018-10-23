// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

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

// { dg-final { scan-lang-dump {Unnamed 0 section:1} module } }
// { dg-final { scan-lang-dump {> Voldemort decl:0 '::counter'} module } }
// { dg-final { scan-lang-dump {Inserted horcrux:-1 '::counter'} module } }
// { dg-final { scan-lang-dump {Read backref:-1 found var_decl:'::counter'} module } }
// { dg-final { scan-lang-dump {>Lazily binding ''@'PiL' section:1} module } }
