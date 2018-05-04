// { dg-additional-options "-fdump-lang-module" }
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

// { dg-final { scan-lang-dump {Unnamed 0 section:2} module } }
// { dg-final { scan-lang-dump {>Lazily loading ''@'PiL' section:2} module } }
// { dg-final { scan-lang-dump {Voldemort:-11 var_decl:'::counter'@PiL} module } }
