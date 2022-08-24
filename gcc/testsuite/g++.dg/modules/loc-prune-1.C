//  { dg-additional-options {-fmodules-ts -fdump-lang-module-lineno} }

export module foo;
// { dg-module-cmi foo }
#define NOT 1
#define YES 1
#define AGAIN_NO (1 + 2)
#if NOT
int foo (int = YES)
{
  return AGAIN_NO;
}
#endif

// { dg-final { scan-lang-dump { Macro maps:1} module } }
// { dg-final { scan-lang-dump { Macro:0 YES 1/1.2 locations } module } }
// { dg-final { scan-lang-dump { Macro maps:1 locs:1} module } }
// { dg-final { scan-lang-dump-not {Macro:. NOT } module } }
// { dg-final { scan-lang-dump-not {Macro:. AGAIN_NO } module } }
