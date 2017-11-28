/* { dg-do assemble } */
/* { dg-additional-options "-Wno-return-type" } */

/* Minimized from the testcase in PR c++/44473; mangling of decimal types
   did not include CV qualifiers. */

namespace std
{
  namespace decimal
  {
    class decimal32
    {
    public:
      typedef float __decfloat32 __attribute__ ((mode (SD)));
      explicit decimal32 (float __r):__val (__r) {}
    private:
      __decfloat32 __val;
    };
  };

  template <typename _BI1, typename _BI2>
  _BI2 copy_backward (_BI1 __first, _BI2 __result);
}

namespace __gnu_cxx
{
  template <typename _Iterator, typename _Container>
  class __normal_iterator
  {
  public:
    explicit __normal_iterator (const _Iterator & __i) {}
    const _Iterator & base () const {}
  };

  template <typename _IteratorL, typename _IteratorR, typename _Container>
  bool operator== (const __normal_iterator <_IteratorL, _Container> &__lhs,
		   const __normal_iterator <_IteratorR, _Container> &__rhs)
  {
    return __lhs.base () == __rhs.base ();
  }

  template <typename _Tp>
  class new_allocator
  {
  public:
    typedef _Tp *pointer;
    typedef const _Tp *const_pointer;
    template <typename _Tp1>
    struct rebind
    {
      typedef new_allocator <_Tp1> other;
    };
  };
}

namespace std
{
  template <typename _Tp>
  class allocator:public __gnu_cxx::new_allocator <_Tp> {};

  template <typename _Tp, typename _Alloc>
  struct _Vector_base
  {
    typedef typename _Alloc::template rebind <_Tp>::other _Tp_alloc_type;
    struct _Vector_impl:public _Tp_alloc_type
    {
      typename _Tp_alloc_type::pointer _M_finish;
    };
  public:  _Vector_impl _M_impl;
  };

  template <typename _Tp, typename _Alloc = std::allocator <_Tp> >
  class vector:protected _Vector_base <_Tp, _Alloc>
  {
    typedef _Vector_base <_Tp, _Alloc> _Base;
    typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  public:
    typedef _Tp value_type;
    typedef typename _Tp_alloc_type::pointer pointer;
    typedef typename _Tp_alloc_type::const_pointer const_pointer;
    typedef __gnu_cxx::__normal_iterator <pointer, vector> iterator;
    typedef __gnu_cxx::__normal_iterator <const_pointer, vector>
      const_iterator;
    const_iterator begin () const;
    iterator end ()
    {
      return iterator (this->_M_impl._M_finish);
    }
    const_iterator end () const
    {
      return const_iterator (this->_M_impl._M_finish);
    }
    bool empty () const
    {
      return begin () == end ();
    }
    void push_back (const value_type & __x)
    {
      _M_insert_aux (end ());
    }
    void _M_insert_aux (iterator __position);
  };

  template <typename _Tp, typename _Alloc>
  void vector <_Tp, _Alloc>::_M_insert_aux (iterator __position)
  {
    std::copy_backward (__position.base (), this->_M_impl._M_finish - 1);
  }
}

std::vector <std::decimal::decimal32> vec;

int
foo ()
{
  return (vec.empty ()) ? 1 : 0;
}

bool
bar ()
{
  vec.push_back (std::decimal::decimal32 (0));
  return true;
}
