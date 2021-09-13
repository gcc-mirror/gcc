// PR 99283 part 3, ICE with template alias as default template parm
// of member template
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _Traits>
struct _Insert
{
  using size_type = int;
  
  template<typename _Pair>
  using _IFconsp = bool;

  template<typename _Pair, typename = _IFconsp<_Pair>>
  int insert (_Pair&& __v);
};

void Foo (typename _Insert<int>::size_type);


