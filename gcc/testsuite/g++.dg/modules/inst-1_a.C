// { dg-module-do run }
// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module foo;
// { dg-module-cmi foo }

int i_baz (int i)
{
  return i;
}

inline int baz (int i)
{
  return i_baz (i);
}

int f_baz (float f)
{
  return int (f);
}

inline int baz (float f)
{
  return f_baz (f);
}

export template <typename T>
int foo (T t)
{
  return baz (t);
}

export inline void user ()
{
  foo (1);
  foo (1.0f);
}

// { dg-final { scan-lang-dump {Depending definition function_decl:'::foo<float>'} module } }
// { dg-final { scan-lang-dump {Depending definition function_decl:'::foo<int>'} module } }
// { dg-final { scan-lang-dump {\[0\]=specialization definition '::foo<float>'} module } }
// { dg-final { scan-lang-dump {\[0\]=specialization definition '::foo<int>'} module } }
