// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }

export module foo;
// { dg-module-cmi foo }

export import :bar;
export import :baz;

export inline int frob (int x)
{
  return foo (x);
}

// { dg-final { scan-lang-dump {Read:-[0-9]'s named merge key \(new\) function_decl:'::foo'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]'s named merge key \(matched\) function_decl:'::foo'} module } }
// { dg-final { scan-lang-dump {Cluster sections are \[1,3\)} module } }
