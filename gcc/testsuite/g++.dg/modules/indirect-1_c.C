// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import bar;


int main ()
{
  if (bar::frob ())
    return 1;
  if (bar::quux ())
    return 2;

  if (bar::Z (1, 2).frob () != 3)
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
// { dg-final { scan-lang-dump {>Loading entity foo\[14\] section:4} module } }
// { dg-final { scan-lang-dump {Named:-[0-9]* namespace_decl:'::foo'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::frob@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[1\] section:1} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::X@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::Z'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[8\] section:2} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::Y@foo:.'@foo} module } }
// { dg-final { scan-lang-dump {Read member:-[0-9]* field_decl:'::foo::Y@foo:.::_vptr.Y'} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::Y@foo:.::frob@foo:.'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::Plain_One'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[13\] section:3} module } }
// { dg-final { scan-lang-dump {Lazily binding '::bar::Scoped_One'@'bar' section} module } }
// { dg-final { scan-lang-dump {>Loading entity foo\[15\] section:5} module } }
// { dg-final { scan-lang-dump-not {Lazily binding '::foo::[ABC]'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily binding '::foo::Scoped@\(foo\)::[ABC]'@'foo' section:} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::Plain_Const_Three'@'bar' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::bar::Scoped_Const_Three'@'bar' section} module } }
