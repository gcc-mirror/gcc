// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import bar;

int main ()
{

  if (bar::frob ())
    return 1;

  if (bar::quux ())
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar::frob'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::frob@foo:2'@foo} module } }
// { dg-final { scan-lang-dump-not {Wrote mergeable} module } }


// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::X@foo:2'@foo} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]* function_decl:'::foo::frob@bar:3<0x0>'} module } }
// { dg-final { scan-lang-dump {Voldemort decl:0 \[0\]* '::foo::frob@bar:3<0x0>'} module } }
