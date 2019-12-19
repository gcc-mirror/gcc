// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
// A more complete imp-member test
export module A;
// { dg-module-cmi A }

struct M
{
  M (){}
  M (M const &){}
  M (M &&){}
  ~M (){}
  M &operator=(M const &){ return *this;}
  M &operator=(M &&){ return *this;}
};

export struct C 
{
  M m;
  // lazy implicit ctors, dtors, assop
};

// C doesn't contain a lot
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl definition '::C'\n  \[1\]=decl declaration '::C::C'\n  \[2\]=binding '::C'\n} module } }

// particularly not ...
// { dg-final { scan-lang-dump-not {'::C::__ct '} module } }
// { dg-final { scan-lang-dump-not {'::C::__dt '} module } }
// { dg-final { scan-lang-dump-not {'::C::operator= '} module } }
