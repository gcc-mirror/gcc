// { dg-additional-options {-fmodules-ts -fdump-lang-module-graph-blocks} }

export module TPL;
// { dg-module-cmi TPL }

export struct frob 
{
  int i;

  template<typename T> void store (T i_)
  {
    i = int (i_);
  }
};

template <> void frob::store (int i_)
{
  i = -i_;
}

// { dg-final { scan-lang-dump {Dependencies of specialization function_decl:'::frob@TPL:1::store@TPL:1<int>'} module } }
// { dg-final { scan-lang-dump-not {Depending definition function_decl:'::frob@TPL:1::store@TPL:1<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization declaration '::frob@TPL:1::store@TPL:1<int>'} module } }
// { dg-final { scan-lang-dump {Specialization '::frob@TPL:1::store@TPL:1<int>' section:2 keyed to '::frob@TPL:1' \(2\)} module } }

// { dg-final { scan-assembler {_ZN4frob5storeIiEEvT_:} } }
