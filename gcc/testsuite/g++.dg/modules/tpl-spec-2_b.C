// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module SPEC;

export import TPL;

// Body is emitted in module-unit itself
template <> int foo<int> (int y)
{
  return 0;
}

// { dg-final { scan-lang-dump {Dependencies of specialization function_decl:'::foo@SPEC:1<int>'} module } }
// { dg-final { scan-lang-dump-not {Depending definition function_decl:'::foo@SPEC:1<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization declaration '::foo@SPEC:1<int>'} module } }
// { dg-final { scan-lang-dump {Specialization '::foo@SPEC:1<int>' section:1 keyed to '::foo@TPL:2<T>' \(2\)} module } }

// { dg-final { scan-assembler {_Z3fooIiEiT_:} } }
