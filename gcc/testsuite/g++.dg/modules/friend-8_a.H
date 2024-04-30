// PR c++/114889
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template<class, class>
struct _Hashtable;

template<class _Key, class _Val>
struct _Map_base {
  void f() {
    _Hashtable<_Key, _Val> __h;
    __h._M_hash_code(0);
  }
};

template<class _Key, class _Value>
struct _Hashtable {
  template<class, class> friend struct _Map_base;
protected:
  void _M_hash_code(int);
};

inline _Hashtable<int, int> m;
