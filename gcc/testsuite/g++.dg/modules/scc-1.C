// Check SCC regions are atomic
// { dg-additional-options -fdump-lang-module }

export module foo;
// { dg-module-bmi foo }

export int bar (int);
export int baz (int = bar (1));
export int bar (int = baz (1));

// The ordering depends on hash table iteration, which is address-dependent

// { dg-final { scan-lang-dump {  Writing:-1 function_decl:'::ba[rz]'} "module" } }
// { dg-final { scan-lang-dump {  *\.\.* Writing:-[0-9]* function_decl:'::ba[rz]'} "module" } }
// { dg-final { scan-lang-dump {  *\.\.* Wrote backref:-1 function_decl:'::ba[rz]'} "module" } }
// { dg-final { scan-lang-dump {  Wrote backref:-[0-9]* function_decl:'::ba[rz]'} "module" } }
