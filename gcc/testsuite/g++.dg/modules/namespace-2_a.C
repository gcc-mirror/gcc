// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

module;

namespace not_exported
{
  // not in purview
}

export module foo;
// { dg-module-bmi foo }

namespace explicit_export
{
}

namespace implicit_export 
{
  export int bob ();
}

namespace also_not_exported 
{
  int bob ();
}

export namespace explicit_export
{
  namespace also_exported
  {
  }
}

// { dg-final { scan-lang-dump-not {Writable bindings at '::not_exported'} "module" } }
// { dg-final { scan-lang-dump {Writing namespace [0-9] '::implicit_export', export, public, parent:0} "module" } }
// { dg-final { scan-lang-dump {Writing namespace [0-9] '::explicit_export', export, public, parent:0} "module" } }
// { dg-final { scan-lang-dump {Writing namespace [0-9] '::also_not_exported', public, parent:0} "module" } }
// { dg-final { scan-lang-dump {Writing namespace [0-9] '::explicit_export::also_exported', export, public, } "module" } }
// { dg-final { scan-lang-dump-not {Writing namespace [0-9] '::not_exported'} "module" } }
// { dg-final { scan-lang-dump-not {Writing namespace [0-9] '::std'} "module" } }
