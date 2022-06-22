//  { dg-additional-options {-fmodules-ts -fdump-lang-module-lineno} }

export module Eve;
// { dg-module-cmi Eve }

#define BEGIN_NAMESPACE(X) inline namespace X {
#define END_NAMESPACE(X) }

BEGIN_NAMESPACE (BOB)
void Alice ();
END_NAMESPACE (BOB)

// { dg-final { scan-lang-dump { Macro maps:1} module } }
// { dg-final { scan-lang-dump { Macro:0 BEGIN_NAMESPACE 5/6.2 locations } module } }
