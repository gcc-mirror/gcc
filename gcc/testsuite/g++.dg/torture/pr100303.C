/* { dg-additional-options "-w -fcompare-debug -fno-dce -ftracer" } */

template < typename _T1 > struct pair
{
  _T1 first;
  int second;
};
struct __aligned_membuf
{
  void _M_ptr ();
};
struct _Rb_tree_node_base
{
  typedef _Rb_tree_node_base *_Base_ptr;
};
struct _Rb_tree_node:_Rb_tree_node_base
{
  __aligned_membuf _M_storage;
  void _M_valptr ()
  {
    _M_storage._M_ptr ();
  }
};
struct _Rb_tree_iterator
{
  typedef _Rb_tree_node_base::_Base_ptr _Base_ptr;
    _Rb_tree_iterator (_Base_ptr __x):_M_node (__x)
  {
  }
  void operator* ()
  {
    static_cast < _Rb_tree_node * >(_M_node)->_M_valptr ();
  }
  friend bool operator== (_Rb_tree_iterator __x, _Rb_tree_iterator)
  {
    return __x._M_node;
  }
  _Base_ptr _M_node;
};

template < typename, typename, typename, typename, typename =
  int >class _Rb_tree
{
  typedef _Rb_tree_node_base *_Base_ptr;
public:
    pair < _Base_ptr > _M_get_insert_hint_unique_pos (int);
  void _M_insert_node (_Base_ptr, int);
    template < typename ... _Args >
    _Rb_tree_iterator _M_emplace_hint_unique (_Args && ...);
  _Rb_tree_iterator lower_bound ()
  {
    _Rb_tree_node_base __trans_tmp_2;
      return &__trans_tmp_2;
  }
};
template < typename _Key, typename _Val, typename _KeyOfValue,
  typename _Compare,
  typename _Alloc > template < typename ... _Args >
  _Rb_tree_iterator _Rb_tree < _Key, _Val, _KeyOfValue, _Compare,
  _Alloc >::_M_emplace_hint_unique (_Args && ...)
{
  int __z;
  try
  {
    auto __res = _M_get_insert_hint_unique_pos (0);
    _Rb_tree_node_base *__res_1;
    if (__res_1)
      _M_insert_node (__res.first, __z);
    return __res.first;
  }
  catch ( ...)
  {
  }
}

class map
{
  _Rb_tree < int, int, int, int >_M_t;
public:
    _Rb_tree_iterator end ();
  void operator[] (int)
  {
    _Rb_tree_iterator __i = lower_bound ();
    if (__i == end ())
      __i = _M_t._M_emplace_hint_unique (__i);
    *__i;
  }
  _Rb_tree_iterator lower_bound ()
  {
    return _M_t.lower_bound ();
  }
};

class FlowStat
{
public:
  int FlowStat_flow;
    FlowStat ()
  {
    shares[FlowStat_flow];
  }
  map shares;
};

class LinkGraphJob
{
  ~LinkGraphJob ();
};
LinkGraphJob::~LinkGraphJob ()
{
  FlowStat ();
}
