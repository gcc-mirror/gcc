// PR tree-optimization/97609
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fno-tree-fre -fnon-call-exceptions" }

struct _Fwd_list_node_base {
  _Fwd_list_node_base *_M_next;
  void _M_transfer_after() { _Fwd_list_node_base *__keep = _M_next = __keep; }
};
struct _Fwd_list_const_iterator {
  _Fwd_list_const_iterator(_Fwd_list_node_base *__n) : _M_node(__n) {}
  _Fwd_list_const_iterator(int);
  _Fwd_list_node_base *_M_node;
};
template <typename, typename> struct forward_list {
  _Fwd_list_node_base _M_head;
  template <typename _InputIterator>
  forward_list(_InputIterator, _InputIterator);
  forward_list(int);
  _Fwd_list_const_iterator cbefore_begin() { return &_M_head; }
  void splice_after(_Fwd_list_const_iterator) noexcept;
  void splice_after(_Fwd_list_const_iterator __pos, forward_list &) {
    splice_after(__pos, 0);
  }
  using __remove_return_type = void;
  __remove_return_type unique() { unique(0); }
  template <typename _BinPred> __remove_return_type unique(_BinPred);
};
template <typename _Tp, typename _Alloc>
void forward_list<_Tp, _Alloc>::splice_after(_Fwd_list_const_iterator __pos)
  noexcept {
  __pos._M_node->_M_transfer_after();
}
template <typename _Tp, typename _Alloc>
template <typename _BinPred>
auto forward_list<_Tp, _Alloc>::unique(_BinPred) -> __remove_return_type {
  forward_list __to_destroy(0);
  splice_after(__to_destroy.cbefore_begin());
}

void
foo ()
{
  forward_list<int, int> c1 (0, 0);
  c1.unique ();
}

