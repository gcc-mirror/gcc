// { dg-additional-options -fdump-lang-module }
import bar;

int main ()
{

  if (bar::frob ())
    return 1;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily loading '::bar::frob'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::frob'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* function_decl:'::foo::frob'@foo} module } }
