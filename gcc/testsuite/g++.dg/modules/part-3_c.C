// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }
// bogus errors until we properly dedup partition entities
export module foo; // { dg-bogus "not writing" "" { xfail *-*-* } }
// { dg-module-bmi foo { xfail *-*-* } }

export import :bar;
export import :baz;

export inline int frob (int x)
{
  return foo (x); // { dg-bogus "ambiguous" "" { xfail *-*-* } }
}

// { dg-final { scan-lang-dump {Read:-[0-9] new mergeable decl function_decl:'::foo'} module { xfail *-*-* } } }
// { dg-final { scan-lang-dump {Read:-[0-9] matched mergeable decl function_decl:'::foo'} module { xfail *-*-* } } }
// { dg-final { scan-lang-dump {Recording new skippable function_decl:'::foo'} module { xfail *-*-* } } }
// { dg-final { scan-lang-dump {Declaration sections are \[1,3\)} module { xfail *-*-* } } }
