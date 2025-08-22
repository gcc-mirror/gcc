// PR c++/120499
// { dg-additional-options "-fmodules -fdump-lang-module-blocks" }
// { dg-module-cmi B }

export module B;
import A;

struct Coll {
  vector<int> vals;
};

export Coll createColl() {
  return Coll{};
}

// But the definition of _Vector_impl::~_Vector_impl has been synthesized here
// { dg-final { scan-lang-dump-times {\[0\]=decl definition '::vector@A:1<int>::_Vector_impl@A:1<int>::__dt '} 1 module } }
