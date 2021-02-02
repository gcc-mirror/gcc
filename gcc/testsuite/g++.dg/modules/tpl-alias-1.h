template<typename _Ptr> struct pointer_traits;

template<typename _Tp>
struct pointer_traits<_Tp*>
{
  template<typename _Up> using rebind = _Up*;
};

template<typename _Ptr, typename _Tp>
using __ptr_rebind = typename pointer_traits<_Ptr>::template rebind<_Tp>;

template<typename _Tp>
struct allocator
{
  typedef _Tp value_type;
  typedef _Tp* pointer;
};

template<typename _Alloc> struct allocator_traits;

template<typename _Tp>
struct allocator_traits<allocator<_Tp>>
{
  using pointer = _Tp*;
  template<typename _Up>
  using rebind_alloc = allocator<_Up>;
};

template<typename _Alloc, typename = typename _Alloc::value_type>
struct __alloc_traits
  : allocator_traits<_Alloc>
{
  typedef _Alloc allocator_type;
  typedef allocator_traits<_Alloc> _Base_type;
  template<typename _Tp>
  struct rebind
  {
    typedef typename _Base_type::template rebind_alloc<_Tp> other;
  };
};

template<typename _Ref, typename _Ptr>
struct _Deque_iterator
{
  template<typename _CvTp>
  using __iter = _Deque_iterator<_CvTp&, __ptr_rebind<_Ptr, _CvTp>>;
  
  typedef __ptr_rebind<_Ptr, long> _Elt_pointer;
  typedef __ptr_rebind<_Ptr, _Elt_pointer> _Map_pointer;
};

template<typename _Alloc>
struct _Deque_base
{
  typedef typename __alloc_traits<_Alloc>::template rebind<long>::other _Tp_alloc_type;

  typedef __alloc_traits<_Tp_alloc_type> _Alloc_traits;

  typedef typename _Alloc_traits::pointer _Ptr;

  typedef _Deque_iterator<long&, _Ptr> iterator;
  
  typedef typename iterator::_Map_pointer _Map_pointer;
};


inline void stack ()
{
  _Deque_base<allocator<long>> c;
}
