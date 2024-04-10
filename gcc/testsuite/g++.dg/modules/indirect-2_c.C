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
// { dg-final { scan-lang-dump {>Loading entity foo\[4\] section:2} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::template frob@foo:.'@foo} module } }
// { dg-final { scan-lang-dump-not {Wrote mergeable} module } }


// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[1\] section:1} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::template X@foo:.'@foo} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]* function_decl:'::foo::frob<0x0>'} module } }
