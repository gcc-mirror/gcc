// PR rtl-optimization/100254
// { dg-do compile }
// { dg-options "-O2 -fno-guess-branch-probability -fipa-pta -fnon-call-exceptions -fcompare-debug" }
// { dg-additional-options "-mtune=goldmont" { target i?86-*-* x86_64-*-* } }

struct _Rb_tree_node_base {
  typedef _Rb_tree_node_base *_Base_ptr;
  typedef _Rb_tree_node_base *_Const_Base_ptr;
  _Base_ptr _M_left;
};
template <typename _Key_compare> struct _Rb_tree_key_compare {
  _Key_compare _M_key_compare;
};
struct _Rb_tree_header {
  _Rb_tree_node_base _M_header;
};
struct _Rb_tree_iterator {
  _Rb_tree_iterator(_Rb_tree_node_base::_Base_ptr);
  friend bool operator==(_Rb_tree_iterator, _Rb_tree_iterator);
};
template <typename _Tp> struct _Rb_tree_const_iterator {
  typedef _Rb_tree_const_iterator _Self;
  _Rb_tree_const_iterator(_Rb_tree_node_base::_Const_Base_ptr) {}
  _Tp operator*();
  template <typename _Up>
  friend bool operator!=(_Rb_tree_const_iterator<_Up>, _Rb_tree_const_iterator<_Up>);
};
template <typename _Key, typename _Val, typename _Compare> struct _Rb_tree {
  template <typename _Key_compare>
  struct _Rb_tree_impl : _Rb_tree_key_compare<_Key_compare>, _Rb_tree_header {};
  _Rb_tree_impl<_Compare> _M_impl;
  _Key _S_key();
  typedef _Rb_tree_const_iterator<_Val> const_iterator;
  const_iterator begin() { return _M_impl._M_header._M_left; }
  _Rb_tree_iterator find(const _Key &);
};
template <typename _Key, typename _Val, typename _Compare>
_Rb_tree_iterator _Rb_tree<_Key, _Val, _Compare>::find(const _Key &__k) {
  _Rb_tree_iterator __j = 0;
  return __j == 0 || _M_impl._M_key_compare(__k, _S_key()) ? 0 : __j;
}
template <typename _Key, typename _Compare = _Key> struct set {
  typedef _Key key_type;
  typedef _Rb_tree<key_type, _Key, _Compare> _Rep_type;
  _Rep_type _M_t;
  typedef typename _Rep_type::const_iterator iterator;
  iterator begin() { return _M_t.begin(); }
  iterator end();
  void find(key_type __x) { _M_t.find(__x); }
};
struct WindowDesc {
  WindowDesc(short);
} _station_view_desc(0);
struct Window {
  void IsWidgetLowered();
  virtual void OnClick(int, int, int);
};
int AllocateWindowDescFront_window_number;
template <typename Wcls> void AllocateWindowDescFront(WindowDesc *desc, bool) {
  Wcls(desc, AllocateWindowDescFront_window_number);
}
class CargoDataEntry;
struct CargoSorter {
  bool operator()(const CargoDataEntry *, const CargoDataEntry *) const;
};
struct CargoDataEntry {
  ~CargoDataEntry();
  char Retrieve_cargo;
  void Retrieve() {
    CargoDataEntry t(Retrieve_cargo);
    children->find(&t);
  }
  CargoDataEntry(char);
  set<CargoDataEntry *, CargoSorter> *children;
};
CargoDataEntry::CargoDataEntry(char) : children() {}
CargoDataEntry::~CargoDataEntry() {
  if (children)
    for (set<CargoDataEntry *>::iterator i = children->begin();
         i != children->end();)
      delete *i;
}
bool CargoSorter::operator()(const CargoDataEntry *,
                             const CargoDataEntry *) const { return false; }
struct StationViewWindow : Window {
  StationViewWindow(WindowDesc *, int);
  CargoDataEntry HandleCargoWaitingClick_filter;
  void OnClick(int, int widget, int) {
    switch (widget) {
    case 0:
      HandleCargoWaitingClick_filter.Retrieve();
      HandleCargoWaitingClick_filter.Retrieve();
    case 1:
      IsWidgetLowered();
    }
  }
};
void ShowStationViewWindow_station() {
  AllocateWindowDescFront<StationViewWindow>(&_station_view_desc,
                                             ShowStationViewWindow_station);
}
