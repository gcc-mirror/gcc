// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }

export module foo;
// { dg-module-cmi foo }

export namespace __gnu_cxx
{
enum _Lock_policy { _S_single};
}

export namespace std
{
  using __gnu_cxx::_S_single;
}

// { dg-final { scan-lang-dump {Writing section:1 4 depsets\n Cluster members:\n  \[0\]=decl definition '::__gnu_cxx::_Lock_policy'\n  \[1\]=using declaration '::__gnu_cxx::_Lock_policy::_S_single'\n  \[2\]=binding '::__gnu_cxx::_[A-Za-z_]*'\n  \[3\]=binding '::__gnu_cxx::_[A-Za-z_]*'\n} module } }
// { dg-final { scan-lang-dump {Writing section:2 2 depsets\n Cluster members:\n  \[0\]=using declaration '::__gnu_cxx::_Lock_policy::_S_single'\n  \[1\]=binding '::std::_S_single'\n} module } }
