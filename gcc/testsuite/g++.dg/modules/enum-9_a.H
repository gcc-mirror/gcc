// PR c++/106848
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _T1>
struct pair {
  using type = void(*)(const _T1&);
};

struct _ScannerBase {
  enum _TokenT { _S_token_anychar };
  pair<_TokenT> _M_token_tbl;
};
