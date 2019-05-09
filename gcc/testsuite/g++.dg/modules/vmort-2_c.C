// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

import malfoy;

int main ()
{
  auto widget = conduit (2);

  return !(widget (8) == 10);
}

// { dg-final { scan-lang-dump {Lazily binding '#null##null#'@'voldy' section:1} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@2 type_decl:'::frobber@voldy:2::._0'} module } }
// { dg-final { scan-lang-dump {Inserted:-2 type record_type:'::frobber@voldy:2::._0'} module } }
