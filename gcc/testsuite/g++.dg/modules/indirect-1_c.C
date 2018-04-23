// { dg-additional-options -fdump-lang-module }
import bar;


int main ()
{
  if ( bar::frob ())
    return 1;
  if ( bar::quux ())
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily loading '::bar::frob'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* namespace_decl:'::foo'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::X'@foo} module } }
