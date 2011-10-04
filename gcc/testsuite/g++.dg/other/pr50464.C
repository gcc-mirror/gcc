// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O3 -mxop" }

typedef long unsigned int size_t;
typedef unsigned long ulong_t;
typedef signed long slong_t;

  template<typename _Iterator>
    struct iterator_traits
    {
      typedef typename _Iterator::reference reference;
    };

  template<typename _Tp>
    struct iterator_traits<_Tp*>
    {
      typedef _Tp& reference;
    };

  template<typename _Iterator, typename _Container>
    class __normal_iterator
    {
    protected:
      _Iterator _M_current;
      typedef iterator_traits<_Iterator> __traits_type;

    public:
      typedef typename __traits_type::reference reference;

      explicit
      __normal_iterator(const _Iterator& __i) : _M_current(__i) { }

      reference
      operator*() const
      { return *_M_current; }

      __normal_iterator&
      operator++()
      {
         ++_M_current;
         return *this;
      }

      const _Iterator&
      base() const
      { return _M_current; }
    };

  template<typename _Iterator, typename _Container>
    inline bool
    operator!=(const __normal_iterator<_Iterator, _Container>& __lhs,
        const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() != __rhs.base(); }

  template<typename _Tp>
    class allocator
    {
    public:
      typedef _Tp* pointer;
      typedef _Tp value_type;

      template<typename _Tp1>
        struct rebind
        { typedef allocator<_Tp1> other; };

       pointer allocate(size_t __n, const void* = 0)
       {
          return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
       }
    };

  template<typename _Tp, typename _Alloc>
    struct _Vector_base
    {
      typedef typename _Alloc::template rebind<_Tp>::other _Tp_alloc_type;

      struct _Vector_impl
      : public _Tp_alloc_type
      {
        typename _Tp_alloc_type::pointer _M_start;
        typename _Tp_alloc_type::pointer _M_finish;
        typename _Tp_alloc_type::pointer _M_end_of_storage;

        _Vector_impl(_Tp_alloc_type const& __a) { }
      };

    public:
      typedef _Alloc allocator_type;

      _Vector_base(size_t __n, const allocator_type& __a)
      : _M_impl(__a)
      {
        this->_M_impl._M_start = this->_M_allocate(__n);
        this->_M_impl._M_finish = this->_M_impl._M_start;
        this->_M_impl._M_end_of_storage = this->_M_impl._M_start + __n;
      }

    public:
      _Vector_impl _M_impl;

      typename _Tp_alloc_type::pointer
      _M_allocate(size_t __n)
      { return __n != 0 ? _M_impl.allocate(__n) : 0; }

    };

  template<typename _Tp, typename _Alloc = allocator<_Tp> >
    class vector : protected _Vector_base<_Tp, _Alloc>
    {
      typedef _Vector_base<_Tp, _Alloc> _Base;
      typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;

    public:
      typedef _Tp value_type;
      typedef typename _Tp_alloc_type::pointer pointer;
      typedef __normal_iterator<pointer, vector> iterator;
      typedef _Alloc allocator_type;

    protected:
      using _Base::_M_allocate;
      using _Base::_M_impl;

    public:

      explicit
      vector(size_t __n, const value_type& __value = value_type(),
      const allocator_type& __a = allocator_type())
      : _Base(__n, __a)
      { _M_fill_initialize(__n, __value); }

      iterator begin()
      { return iterator(this->_M_impl._M_start); }

      iterator end()
      { return iterator(this->_M_impl._M_finish); }

    protected:
      void
      _M_fill_initialize(size_t __n, const value_type& __value)
      {
         this->_M_impl._M_finish = this->_M_impl._M_end_of_storage;
      }
    };

  template<typename _InputIterator, typename _OutputIterator, typename _Tp>
    _OutputIterator
    replace_copy(_InputIterator __first, _InputIterator __last,
   _OutputIterator __result,
   const _Tp& __old_value, const _Tp& __new_value)
    {
      ;
      for (; __first != __last; ++__first, ++__result)
         if (*__first == __old_value)
            *__result = __new_value;
         else
            *__result = *__first;
      return __result;
    }

extern size_t shape_rank;

void createDataspaceIdentifier()
{
  vector< ulong_t > dataspaceDims( shape_rank );
  vector< ulong_t > maxDataspaceDims( shape_rank );

  replace_copy(
    dataspaceDims.begin(), dataspaceDims.end(),
    maxDataspaceDims.begin(), ulong_t( 0 ), ((ulong_t)(slong_t)(-1)) );
}
