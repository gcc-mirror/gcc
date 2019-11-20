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

// { dg-final { scan-lang-dump {\[0\]=specialization definition '::foo@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Writing named key for mergeable decl function_decl:'::baz@foo:.'} module } }
// { dg-final { scan-lang-dump {Unnamed 0 '::foo@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Writing decl spec key for mergeable specialization function_decl:'::foo@foo:.<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort:0 '::foo@foo:.<int>'} module } }

// { dg-final { scan-lang-dump {Writing decl spec key for mergeable specialization function_decl:'::foo@foo:.<int>'} module } }
