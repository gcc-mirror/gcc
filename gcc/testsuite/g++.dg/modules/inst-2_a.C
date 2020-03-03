// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-blocks-uid-alias} }

export module foo;
// { dg-module-cmi foo }

inline int baz (int i)
{
  return i;
}

export template <typename T>
inline int foo (T t)
{
  return baz (t);
}

export inline void user ()
{
  foo (1);
}

// { dg-final { scan-lang-dump {\[0\]=specialization definition '::foo<int>'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s named merge key \(decl\) function_decl:'::baz'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s decl spec merge key \(specialization\) function_decl:'::foo<int>'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]*'s decl spec merge key \(specialization\) function_decl:'::foo<int>'} module } }
