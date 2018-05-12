// { dg-additional-options -fdump-lang-module }
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

// { dg-final { scan-lang-dump {Lazily loading '::bar::frob'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::frob'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* namespace_decl:'::foo'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::X'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::X'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::X::__ct_comp '@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::bar::Z'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::Y'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* type_decl:'::foo::Y'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* field_decl:'::foo::Y::_vptr.Y'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::Y::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::bar::Plain_One'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::Plain'@'foo' section} module } }
// { dg-final { scan-lang-dump {Lazily loading '::bar::Scoped_One'@'bar' section} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::Scoped'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily loading '::foo::[ABC]'@'foo' section:} module } }
// { dg-final { scan-lang-dump-not {Lazily loading '::foo::Scoped::[ABC]'@'foo' section:} module } }

// { dg-final { scan-lang-dump {Lazily loading '::bar::Plain_Const_Three'@'bar' section:} module } }
// { dg-final { scan-lang-dump {Lazily loading '::bar::Scoped_Const_Three'@'bar' section} module } }

// { dg-final { scan-lang-dump {Querying server for 'bar'} module } }
// { dg-final { scan-lang-dump-not {Querying server for 'foo'} module } }
