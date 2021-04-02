// Check SCC regions are atomic
// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }

export module foo;
// { dg-module-cmi foo }

export int bar (int);
export int baz (int = bar (1));
export int bar (int = baz (1));

// The ordering depends on hash table iteration, which is address-dependent

// { dg-final { scan-lang-dump { Writing named:-[0-9]* function_decl:'::bar'} "module" } }
// { dg-final { scan-lang-dump { Writing named:-[0-9]* function_decl:'::baz'} "module" } }
// { dg-final { scan-lang-dump {  \.[0-9]*\.  Wrote backref:-[0-9]* function_decl:'::ba[rz]'} "module" } }
// { dg-final { scan-lang-dump { Wrote backref:-[0-9]* function_decl:'::ba[rz]'} "module" } }
