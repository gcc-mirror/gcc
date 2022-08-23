// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module SPEC;

export import TPL;

// Body is emitted in module-unit itself
template <> int foo<int> (int y)
{
  return 0;
}

// { dg-final { scan-lang-dump {Dependencies of specialization function_decl:'::foo<int>'} module } }
// { dg-final { scan-lang-dump-not {Depending definition function_decl:'::foo<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization declaration '::foo<int>'} module } }
// { dg-final { scan-lang-dump {Pending specialization '::foo<int>' entity:[0-9]* section:. keyed to '::foo'} module } }

// { dg-final { scan-assembler {_ZW3TPL3fooIiEiT_:} } }
