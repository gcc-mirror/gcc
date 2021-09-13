// { dg-additional-options "-Wno-analyzer-use-of-uninitialized-value" }

template <typename> class allocator {
public:
  allocator(const allocator &);
  allocator();
};

template <typename> struct allocator_traits;
template <typename _Tp> struct allocator_traits<allocator<_Tp> > {
  static allocator<_Tp> select_on_container_copy_construction() {
    return allocator<_Tp>();
  }
  static allocator<_Tp> _S_select_on_copy() {
    return select_on_container_copy_construction();
  }
};

class basic_string {
  struct _Alloc_hider {
    _Alloc_hider(allocator<char>);
  } _M_dataplus;

public:
  basic_string(basic_string &)
    : _M_dataplus(allocator_traits<allocator<char> >::_S_select_on_copy()) {}
} xxx(xxx);
