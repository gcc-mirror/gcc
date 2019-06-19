// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-blocks-uid-alias} }

export module foo;
// { dg-module-bmi foo }

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

// { dg-final { scan-lang-dump {\[0\]=specialization definition '::foo@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Writing key for mergeable decl function_decl:'::baz@foo:1'} module } }
// { dg-final { scan-lang-dump {Unnamed 0 '::foo@foo:1<int>'} module } }
// { dg-final { scan-lang-dump-times {Ordered 1 mergeables} 4 module } }
// { dg-final { scan-lang-dump {Writing key for mergeable specialization function_decl:'::foo@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Voldemort:0 '::foo@foo:1<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::foo@foo:1<int>'} module } }

// { dg-final { scan-lang-dump {Writing key for mergeable specialization function_decl:'::foo@foo:1<int>'} module } }
