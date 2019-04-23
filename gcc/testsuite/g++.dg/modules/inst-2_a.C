// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-blocks-uid-alias} }

export module foo;
// { dg-module-bmi foo }

inline int baz (int i)
{
  return i;
}

export template <typename T>
int foo (T t)
{
  return baz (t);
}

export inline void user ()
{
  foo (1);
}

// { dg-final { scan-lang-dump {\[0\]=specialization definition '::foo@1\(foo\)<int>'} module } }
// { dg-final { scan-lang-dump {Wrote mergeable:-2 function_decl:'::baz@1\(foo\)'} module } }
// { dg-final { scan-lang-dump {Unnamed 0 '::foo@1\(foo\)<int>'} module } }
// { dg-final { scan-lang-dump-times {Seeded 1 mergeables} 4 module } }
// { dg-final { scan-lang-dump {Wrote mergeable:-4 function_decl:'::foo@1\(foo\)<int>'\(_Z3fooIiEiT_\)} module } }
// { dg-final { scan-lang-dump {Voldemort:0 '::foo@1\(foo\)<int>'} module } }
// { dg-final { scan-lang-dump {Inserted:-1 horcrux:0@0 for '::foo@1\(foo\)<int>'} module } }

// { dg-final { scan-lang-dump {Wrote:-4 global specialization function_decl:'::foo@1\(foo\)<int>'} module } }
