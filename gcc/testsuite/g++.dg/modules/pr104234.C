// PR c++/104234
// { dg-additional-options "-fmodules-ts" }

template <typename> struct _Node_handle_common {
  template <typename> friend class _Rb_tree;
};
struct _Hashtable {
  using node_type = _Node_handle_common<int>;
  node_type __trans_tmp_1;
};
template <typename> class _Rb_tree {
  struct _Rb_tree_impl {
    _Rb_tree_impl();
  } _M_impl;
};
_Rb_tree<int> _M_tmap_;
