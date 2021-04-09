// { dg-additional-options "-fmodules-ts -fdump-lang-module -Wno-pedantic" }

module;
# 5 "gmf" 1
namespace not_exported
{
  // not in purview
}
# 10 "" 2
export module foo;
// { dg-module-cmi foo }

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
// { dg-final { scan-lang-dump {Writing namespace:[0-9] '::also_not_exported', public, purview\n} "module" } }
// { dg-final { scan-lang-dump {Writing namespace:[0-9] '::implicit_export', public, purview, export\n} "module" } }
// { dg-final { scan-lang-dump {Writing namespace:[0-9] '::explicit_export', public, purview, export\n} "module" } }
// { dg-final { scan-lang-dump {Writing namespace:[0-9] '::explicit_export::also_exported', public, purview, export\n} "module" } }
// { dg-final { scan-lang-dump-not {Writing namespace:[0-9] '::not_exported'} "module" } }
// { dg-final { scan-lang-dump-not {Writing namespace:[0-9] '::std'} "module" } }
