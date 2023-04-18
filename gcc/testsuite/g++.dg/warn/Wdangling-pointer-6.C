// PR c++/109514
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O2 -Werror=dangling-pointer" }

struct _Rb_tree_node_base {
  _Rb_tree_node_base *_M_parent;
};
struct _Rb_tree_header {
  _Rb_tree_node_base _M_header;
  void _M_move_data() { _M_header._M_parent->_M_parent = &_M_header; }
};
struct _Rb_tree {
  _Rb_tree_header _M_impl;
  _Rb_tree_node_base *&_M_root() { return _M_impl._M_header._M_parent; }
  _Rb_tree();
  _Rb_tree &operator=(_Rb_tree &&);
};
_Rb_tree &_Rb_tree::operator=(_Rb_tree &&) {
  if (_M_root())
    _M_impl._M_move_data();
  return *this;
}
struct set {
  _Rb_tree _M_t;
};
set FilterRs();
void f() {
  set rs;
  rs = FilterRs();
};
