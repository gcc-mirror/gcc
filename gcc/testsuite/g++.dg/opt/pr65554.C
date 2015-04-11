// PR middle-end/65554
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

namespace std
{
  struct B { enum { __value }; };
  template <typename _Iterator> struct C
  {
    static _Iterator _S_base (_Iterator p1) { return p1; }
  };
  template <typename> using _RequireInputIter = int;
  template <typename _Iterator> _Iterator __niter_base (_Iterator p1)
  {
    return std::C <_Iterator>::_S_base (p1);
  }
  template <typename _Iterator> _Iterator __miter_base (_Iterator p1)
  {
    return std::C <_Iterator>::_S_base (p1);
  }
  struct D
  {
    template <typename _Tp> static _Tp *__copy_m (_Tp * p1, _Tp * p2, _Tp *)
    {
      int _Num = p2 - p1;
      __builtin_memmove (0, p1, sizeof (_Tp) * _Num);
    }
  };
  template <int, typename _II, typename _OI> void __copy_move_a (_II p1, _II p2, _OI p3)
  {
    D::__copy_m (p1, p2, p3);
  }
  template <int, typename _II, typename _OI> void __copy_move_a2 (_II p1, _II p2, _OI p3)
  {
    __copy_move_a <0> (std::__niter_base (p1), std::__niter_base (p2), std::__niter_base (p3));
  }
  template <typename _II, typename _OI> void copy (_II p1, _II p2, _OI p3)
  {
    __copy_move_a2 <B::__value> (std::__miter_base (p1), std::__miter_base (p2), p3);
  }
}
template <typename _Tp> struct F { typedef _Tp *pointer; };
namespace std
{
  template <typename _Tp> using __allocator_base = F <_Tp>;
  template <typename _Tp> struct allocator:__allocator_base <_Tp> {};
  template <class _E> struct initializer_list
  {
    typedef _E *const_iterator;
    _E *_M_array;
    __SIZE_TYPE__ _M_len;
    const_iterator begin () { return _M_array; }
    const_iterator end () { return begin () + 1; }
  };
  template <typename _Alloc> struct allocator_traits
  {
    template <typename _Tp> static typename _Tp::pointer _S_pointer_helper (_Tp *);
    typedef decltype (_S_pointer_helper ((_Alloc *) 0)) __pointer;
    typedef __pointer pointer;
  };
}
template <typename> struct __alloc_traits:
std::allocator_traits <std::allocator <const char *>> {};
namespace std
{
  struct G
  {
    template <typename _InputIterator, typename _ForwardIterator> static _ForwardIterator __uninit_copy (_InputIterator p1, _InputIterator p2, _ForwardIterator p3)
    {
      copy (p1, p2, p3);
    }
  };
  template <typename _InputIterator, typename _ForwardIterator> void
  uninitialized_copy (_InputIterator p1, _InputIterator p2, _ForwardIterator p3)
  {
    G::__uninit_copy (p1, p2, p3);
  }
  template <typename _InputIterator, typename _ForwardIterator, typename _Tp> void __uninitialized_copy_a (_InputIterator p1, _InputIterator p2, _ForwardIterator p3, allocator <_Tp> &)
  {
    uninitialized_copy (p1, p2, p3);
  }
  struct H
  {
    typedef std::allocator <int *> _Tp_alloc_type;
    typedef __alloc_traits <_Tp_alloc_type>::pointer pointer;
    _Tp_alloc_type & _M_get_Tp_allocator ();
  };
  template <typename _Tp, typename = std::allocator <_Tp>> struct J: H
  {
    void operator= (initializer_list <_Tp> p1)
    {
      this->assign (p1.begin (), p1.end ());
    }
    template <typename _InputIterator, typename = std::_RequireInputIter <_InputIterator>> void assign (_InputIterator p1, _InputIterator p2)
    {
      _M_assign_dispatch (p1, p2, 0);
    }
    pointer _M_allocate_and_copy___result;
    template <typename _ForwardIterator> void _M_allocate_and_copy (int, _ForwardIterator p2, _ForwardIterator p3)
    {
      __uninitialized_copy_a (p2, p3, _M_allocate_and_copy___result, _M_get_Tp_allocator ());
    }
    template <typename _InputIterator> void _M_assign_dispatch (_InputIterator p1, _InputIterator p2, int)
    {
      _M_assign_aux (p1, p2, 0);
    }
    template <typename _ForwardIterator> void _M_assign_aux (_ForwardIterator, _ForwardIterator, int);
  };
  template <typename _Tp, typename _Alloc>
    template <typename _ForwardIterator> void J <_Tp, _Alloc>::_M_assign_aux (_ForwardIterator p1, _ForwardIterator p2, int)
  {
    _M_allocate_and_copy (0, p1, p2);
  }
  class I
  {
    void tabCompletion (std::J <int>&) const;
  };
  void
  I::tabCompletion (J <int>&) const
  {
    J <const char *> extra;
    extra = { "foo" };
  }
}
