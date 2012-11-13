/* { dg-do compile } */
/* { dg-options "-c -O -fgcse-after-reload -fnon-call-exceptions" } */
template < typename _Tp > class new_allocator
{
public:
  typedef _Tp pointer;
  template < typename _Tp1 > struct rebind
  {
    typedef new_allocator < _Tp1 > other;
  };

};

template < typename > class allocator;

template < typename _Alloc > struct __alloc_traits
{
  typedef typename _Alloc::pointer pointer;
    template < typename _Tp > struct rebind
  {
    typedef typename _Alloc::template rebind < _Tp >::other other;
  };

};

template < typename _Tp, typename _Alloc > struct _Vector_base
{
  typedef
    typename
    __alloc_traits < _Alloc >::template rebind < _Tp >::other _Tp_alloc_type;
  typedef typename __alloc_traits < _Tp_alloc_type >::pointer pointer;
  struct _Vector_impl
  {
    pointer _M_start;
    pointer _M_end_of_storage;
  };

   ~_Vector_base ();
  _Vector_impl _M_impl;
};

template < typename _Tp, typename _Alloc = allocator < _Tp > >class vector:
_Vector_base < _Tp, _Alloc >
{
  typedef _Vector_base < _Tp, _Alloc > _Base;
public:
  typedef typename _Base::pointer pointer;
vector ():
  _Base ()
  {
    _M_erase_at_end (this->_M_impl._M_start);
  }
  void _M_erase_at_end (pointer)
  {
  }
};

template < typename T > class clear_alloc:
public new_allocator < T >
{
};

void
foo ()
{
  new vector < int, clear_alloc < int > >;
}

