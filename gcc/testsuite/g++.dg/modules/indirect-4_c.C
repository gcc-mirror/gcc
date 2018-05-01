// { dg-additional-options -fdump-lang-module }
import bar;

int main ()
{
  if (bar::quux () != 3)
    return 1;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily loading '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::TPL'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL::frob'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* type_decl:'::foo::TPL'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* function_decl:'::foo::TPL::frob'@foo} module } }
