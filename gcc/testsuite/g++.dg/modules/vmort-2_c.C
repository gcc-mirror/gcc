// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

import malfoy;

void interpose ()
{
  // Force renumber of anon vars
  auto lambda0 = [] () {};
  auto lambda1 = [] () {};
  auto lambda2 = [] () {};
}

int main ()
{
  auto widget = conduit (2);

  return !(widget (8) == 10);
}

// { dg-final { scan-lang-dump {Loading entity voldy\[1\] section:1} module } }
// { dg-final { scan-lang-dump {Indirect:-[0-9]* decl's type record_type:'::frobber@voldy:.::._anon_3@voldy:.'} module } }
