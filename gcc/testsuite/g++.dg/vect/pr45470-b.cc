/* { dg-do compile } */
/* { dg-additional-options "-O1 -fnon-call-exceptions" } */

template < typename _Tp > struct new_allocator
{
  typedef _Tp * pointer;
    template < typename > struct rebind
  {
    typedef new_allocator other;
  };

};

template < typename _Tp > struct allocator:public new_allocator < _Tp >
{};

template < typename _Tp, typename _Alloc > struct _Vector_base
{
  typedef typename _Alloc::template rebind < _Tp >::other _Tp_alloc_type;
  struct
  {
    typename _Tp_alloc_type::pointer _M_start;
    typename _Tp_alloc_type::pointer _M_finish;
    typename _Tp_alloc_type::pointer _M_end_of_storage;
  };

};

template
  <
  typename
  _Tp,
  typename
  _Alloc = allocator < _Tp > >struct vector:_Vector_base < _Tp, _Alloc >
{
  typedef _Vector_base < _Tp, _Alloc > _Base;
    vector ():_Base ()
  {}
   ~vector ();
}
;
struct LoadGraph
{
  LoadGraph (int);
    vector < struct _GdkColor >colors;
    vector < float >data_block;
};

LoadGraph::LoadGraph (int)
{}

