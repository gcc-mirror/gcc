// PR c++/120499
// { dg-additional-options "-fmodules -fdump-lang-module-blocks" }
// { dg-module-cmi A }

export module A;

struct allocator {
  ~allocator() {}
};

export template <typename _Tp>
struct vector {
  struct _Vector_impl : public allocator {};
  _Vector_impl _M_impl;
  vector() = default;
};

template <typename T>
struct regex_token_iterator {
  vector<int> _M_subs;
};
template struct regex_token_iterator<const char*>;

// No definition of _Vector_impl::~_Vector_impl here (not synthesized)
// { dg-final { scan-lang-dump-not {'::vector@A:1<int>::_Vector_impl@A:1<int>::__dt '} module } }
