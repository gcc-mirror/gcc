// { dg-additional-options -fdump-lang-module }

export module foo;
// { dg-module-bmi foo }

namespace not_export 
{
  // contains nothing exported
}

namespace explicit_export 
{
  // this partition does not cause export
}

namespace implicit_export 
{
  export int frob ();
}

export namespace explicit_export
{
  // but this one does
  namespace also_exported
  {
    // inside export region
  }
}

// { dg-final { scan-lang-dump "Writable bindings at '::explicit_export'" "module" } }
// { dg-final { scan-lang-dump "Writable bindings at '::explicit_export::also_exported'" "module" } }
// { dg-final { scan-lang-dump-not "Writable bindings at '::implicit_export'" "module" } }
// { dg-final { scan-lang-dump-not "Writable bindings at '::not_exported'" "module" } }
// { dg-final { scan-lang-dump "Writing namespace '::implicit_export' 0::." "module" } }
// { dg-final { scan-lang-dump "Writing namespace '::explicit_export' 0::." "module" } }
// { dg-final { scan-lang-dump "Writing namespace '::explicit_export::also_exported' .::3" "module" } }
// { dg-final { scan-lang-dump-not "Writing namespace '::not_exported' ." "module" } }
// { dg-final { scan-lang-dump-not "Writing namespace '::std' ." "module" } }
