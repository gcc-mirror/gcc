// { dg-additional-options -fdump-lang-module}

export module foo;
// { dg-module-bmi foo }

export int defarg ();

export inline int frob (int i = defarg ())
{
  return i;
}

// { dg-final { scan-lang-dump {Need deferrable definition of} module } }

// The defarg dependency is on the declaration, not the definition.
// { dg-final { scan-lang-dump {Declaration '::frob'\n   ... Dependency on declaration '::defarg' added} module } }
// { dg-final { scan-lang-dump-not {Definition '::frob'\n   ... Dependency on declaration '::defarg' added} module } }
