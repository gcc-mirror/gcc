// { dg-additional-options "-fmodules -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;
namespace test {
  struct S {};
  void go(S);
}
export module M;

// Ideally we don't emit any namespaces that only have discarded entries
// { dg-final { scan-lang-dump-not {Writing namespace:[0-9]* '::test'} module } }
