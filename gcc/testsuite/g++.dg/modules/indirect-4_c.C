// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid-alias" }
import bar;

int main ()
{
  if (bar::quux () != 3)
    return 1;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::TPL'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL@foo:2'@foo} module } }

// { dg-final { scan-lang-dump {Reading definition function_decl '::foo::TPL@bar:3<0x1>::frob@bar:3<0x2>'} module } }
// { dg-final { scan-lang-dump {Reading definition type_decl '::foo::TPL@bar:3<0x1>'} module } }
