// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }
// bogus errors until we properly dedup partition entities
export module foo; // { dg-bogus "not writing" "" }
// { dg-module-bmi foo }

export import :bar;
export import :baz;

export inline int frob (int x)
{
  return foo (x); // { dg-bogus "ambiguous" "" }
}

// { dg-final { scan-lang-dump {Read:-[0-9] new mergeable named function_decl:'::foo@foo:bar:2'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9] matched mergeable named function_decl:'::foo@foo:baz:3'} module } }
// { dg-final { scan-lang-dump {Recording new skippable function_decl:'::foo@foo:bar:2'} module } }
// { dg-final { scan-lang-dump {Declaration sections are \[1,3\)} module } }
