// { dg-additional-options -fdump-lang-module }
import bar;


int main ()
{
  return bar::frob ();
}

// { dg-final { scan-lang-dump {Lazily loading '::bar::frob'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* namespace_decl:'::foo'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::frob'@foo} module } }
