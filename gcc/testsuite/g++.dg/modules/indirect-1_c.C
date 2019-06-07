// { dg-additional-options "-fmodules-ts -fdump-lang-module-eh-uid" }
import bar;


int main ()
{
  if (bar::frob ()) // { dg-bogus "during lazy loading" }
    return 1;
  if (bar::quux ()) // { dg-bogus "during lazy loading" }
    return 2;

  if (bar::Z (1, 2).frob () != 3) // { dg-bogus "during lazy loading" }
    return 3;

  static_assert (bar::Plain_One (true) == 1);
  static_assert (bar::Plain_One (false) == 2);
  static_assert (int (bar::Scoped_One (true)) == 1);
  static_assert (int (bar::Scoped_One (false)) == 2);

  static_assert (bar::Plain_Const_Three == 3);
  static_assert (int (bar::Scoped_Const_Three) == 3);

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar::frob'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Namespace:-[0-9]* namespace_decl:'::foo'@.none.} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::frob@foo:2'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::X@foo:2'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::X@foo:2::__ct_comp '@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::Z'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::Y'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::Y@foo:2'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* field_decl:'::foo::Y@foo:2::_vptr.Y'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::Y@foo:2::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::Plain_One'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::Plain'@'foo' section} module } }
// { dg-final { scan-lang-dump {Lazily binding '::bar::Scoped_One'@'bar' section} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::Scoped'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily binding '::foo::[ABC]'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily binding '::foo::Scoped@\(foo\)::[ABC]'@'foo' section:} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::Plain_Const_Three'@'bar' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::bar::Scoped_Const_Three'@'bar' section} module } }

// { dg-final { scan-lang-dump {Mapper request:[^\n]*IMPORT bar} module } }
// { dg-final { scan-lang-dump-not {Mapper request:IMPORT foo} module } }
