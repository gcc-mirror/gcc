// PR tree-optimization/22444
// When creating SFT's, we shouldn't add the original variable
// to the addressable vars list, because this may cause false aliasing
// with the subvars leading to the subvars not being renamed when they should
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-Wno-return-type" }
__extension__ typedef __PTRDIFF_TYPE__ ptrdiff_t;
__extension__ typedef __SIZE_TYPE__ size_t;
namespace std
{
  template<class _T1, class _T2> struct pair
  {
    _T1 first;
    _T2 second;
    pair(const _T1& __a, const _T2& __b) : first(__a), second(__b) { }
  };
}
namespace __gnu_internal
{
  typedef char __one;
  template<typename _Tp> __one __test_type(int _Tp::*);
}
namespace std
{
  template<typename _Tp> struct ___is_pod
  {
    enum { __value = (sizeof(__gnu_internal::__test_type<_Tp>(0))!= sizeof(__gnu_internal::__one)) };
  };
  template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t, typename _Pointer = _Tp*, typename _Reference = _Tp&> struct iterator
  { };
  template<typename _Iterator> struct iterator_traits
  {
    typedef typename _Iterator::difference_type difference_type;
  };
  template<typename _Iterator> class reverse_iterator : public iterator<typename iterator_traits<_Iterator>::iterator_category, typename iterator_traits<_Iterator>::value_type, typename iterator_traits<_Iterator>::difference_type, typename iterator_traits<_Iterator>::pointer, typename iterator_traits<_Iterator>::reference>
  {
    typedef _Iterator iterator_type;
    typedef typename iterator_traits<_Iterator>::difference_type difference_type;
    typedef typename iterator_traits<_Iterator>::reference reference;
    reverse_iterator operator+(difference_type __n) const {}
    reverse_iterator& operator+=(difference_type __n) { }
    reference operator[](difference_type __n) const { }
  };
}
namespace __gnu_cxx
{
  template<bool _Thread> class __pool;
  template<template <bool> class _PoolTp, bool _Thread> struct __common_pool_policy;
  template<typename _Tp> class __mt_alloc_base
  {
    typedef ptrdiff_t difference_type;
    typedef _Tp* pointer;
  };
  template<typename _Tp, typename _Poolp = __common_pool_policy<__pool, true> > class __mt_alloc : public __mt_alloc_base<_Tp>
  {
    typedef size_t size_type;
  };
}
namespace std
{
  template<typename _Tp> struct allocator:public __gnu_cxx::__mt_alloc<_Tp>
  {
    template<typename _Tp1> struct rebind
    {
      typedef allocator<_Tp1> other;
    };
  };
  template <class _Arg, class _Result> struct unary_function  { };
  template <class _Arg1, class _Arg2, class _Result> struct binary_function
  {
    typedef _Arg2 second_argument_type;
  };
  template <class _Tp> struct less : public binary_function<_Tp, _Tp, bool>
  {
    bool operator()(const _Tp& __x, const _Tp& __y) const { }
  };
  template <class _Tp> struct _Identity : public unary_function<_Tp,_Tp> { };
  struct _Rb_tree_node_base
  {
    typedef _Rb_tree_node_base* _Base_ptr;
    typedef const _Rb_tree_node_base* _Const_Base_ptr;
    _Base_ptr _M_right;
    static _Base_ptr _S_minimum(_Base_ptr __x) { }
    static _Base_ptr _S_maximum(_Base_ptr __x) { }
  };
  template<typename _Val> struct _Rb_tree_node { };
  template<typename _Tp> struct _Rb_tree_iterator
  {
    typedef _Tp* pointer;
    typedef _Rb_tree_iterator<_Tp> _Self;
    typedef _Rb_tree_node_base::_Base_ptr _Base_ptr;
    pointer operator->() const { }
    _Self operator++(int) { }
    _Base_ptr _M_node;
  };
  template<typename _Tp> struct _Rb_tree_const_iterator
  {
    typedef const _Tp* pointer;
    typedef _Rb_tree_iterator<_Tp> iterator;
    typedef _Rb_tree_node_base::_Const_Base_ptr _Base_ptr;
    _Rb_tree_const_iterator(const iterator& __it) : _M_node(__it._M_node) { }
    _Base_ptr _M_node;
  };
  template<typename _Key, typename _Val, typename _KeyOfValue, typename _Compare, typename _Alloc = allocator<_Val> > struct _Rb_tree
  {
    typedef typename _Alloc::template rebind<std::_Rb_tree_node<_Val> >::other _Node_allocator;
    typedef _Rb_tree_node_base* _Base_ptr;
    typedef const _Rb_tree_node_base* _Const_Base_ptr;
    typedef std::_Rb_tree_node<_Val> _Rb_tree_node;
    typedef _Key key_type;
    typedef _Val value_type;
    typedef value_type* pointer;
    typedef _Rb_tree_node* _Link_type;
    template<typename _Key_compare, bool _Is_pod_comparator = std::___is_pod<_Key_compare>::__value> struct _Rb_tree_impl
      : _Node_allocator
    {
      _Rb_tree_node_base _M_header;
    };
    _Rb_tree_impl<_Compare> _M_impl;
    typedef _Rb_tree_iterator<value_type> iterator;
    typedef _Rb_tree_const_iterator<value_type> const_iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;
    pair<iterator,bool> insert_unique(const value_type& __x);
  };
  template<class _Key, class _Compare, class _Alloc> class set
  {
    typedef _Key key_type;
    typedef _Key value_type;
    typedef _Compare key_compare;
    typedef typename _Alloc::template rebind<_Key>::other _Key_alloc_type;
    typedef _Rb_tree<_Key, value_type, _Identity<value_type>, key_compare, _Key_alloc_type> _Rep_type;
    _Rep_type _M_t;
    typedef typename _Rep_type::const_iterator iterator;
    std::pair<iterator,bool> insert(const value_type& __x)
    {
      std::pair<typename _Rep_type::iterator, bool> __p = _M_t.insert_unique(__x);
      return std::pair<iterator, bool>(__p.first, __p.second);
    }
  };
}
template class std::set<int, std::less<int>, std::allocator<char> >;
