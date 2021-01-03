// { dg-additional-options {-fmodules-ts -fdump-lang-module-blocks} }

module foo:bob;
// { dg-module-cmi foo:bob }

class frob
{
public:
  int field;
};

template<int J>
class FROB
{
public:
  static constexpr int val = J;
};

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::frob'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::template FROB'} module } }
