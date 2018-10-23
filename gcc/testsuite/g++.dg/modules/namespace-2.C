// { dg-options "-fmodules-ts -fdump-lang-module" }

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
}

export namespace explicit_export
{
  namespace also_exported
  {
  }
}

// { dg-final { scan-lang-dump-not {Writable bindings at '::not_exported'} "module" } }
// { dg-final { scan-lang-dump {Writing namespace [0-9] '::implicit_export', export, public, owner=foo, parent:0} "module" } }
// { dg-final { scan-lang-dump {Writing namespace [0-9] '::explicit_export', export, public, owner=foo, parent:0} "module" } }
// { dg-final { scan-lang-dump {Writing namespace 3 '::explicit_export::also_exported', export, public, owner=foo, parent:1} "module" } }
// { dg-final { scan-lang-dump-not {Writing namespace [0-9] '::not_exported',} "module" } }
// { dg-final { scan-lang-dump-not {Writing namespace [0-9] '::std',} "module" } }
