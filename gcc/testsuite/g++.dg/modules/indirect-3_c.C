// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import bar;

int main ()
{

  if (bar::quux ())
    return 1;

  if (bar::toto ())
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar@bar:.::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[5\] section:2} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo@foo:.::X@foo:.::template frob@foo:.'@foo} module } }
// { dg-final { scan-lang-dump-not {Instantiation:-[0-9]* function_decl:'::foo::X@foo:.::frob@.()<0x0>'} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar@bar:.::toto'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[.\] section:1} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo@foo:.::template TPL@foo:.'@foo} module } }
// { dg-final { scan-lang-dump {Reading definition type_decl '::foo@foo:.::TPL@bar:.<0x0>'} module } }
