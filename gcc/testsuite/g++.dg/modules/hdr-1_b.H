// { dg-additional-options {-fmodule-header -fdump-lang-module-blocks} }

// { dg-module-cmi {} }

import "hdr-1_a.H";

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
