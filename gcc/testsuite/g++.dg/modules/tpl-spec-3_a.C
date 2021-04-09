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

// Not inline!
template <> void frob::store (int i_)
{
  i = -i_;
}

// { dg-final { scan-lang-dump {Dependencies of specialization function_decl:'::frob::store<int>'} module } }
// { dg-final { scan-lang-dump-not {Depending definition function_decl:'::frob::store<int>'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=specialization declaration '::frob::store<int>'} module } }
// { dg-final { scan-lang-dump {Pending specialization '::frob::store<int>' entity:[0-9]* section:. keyed to '::frob'} module } }

// { dg-final { scan-assembler {_ZN4frob5storeIiEEvT_:} } }
