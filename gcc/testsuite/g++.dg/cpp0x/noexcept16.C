// PR c++/50972
// { dg-do compile { target c++11 } }
// Ignore all errors, we're just testing that this doesn't ICE
// { dg-prune-output "error" }

namespace std
typedef long unsigned int size_t;
template<typename...>
struct __and_;
template<typename _Tp>
struct is_nothrow_move_constructible
{
};
template<typename _Tp>
struct is_nothrow_move_assignable
struct __add_rvalue_reference_helper<_Tp, true>
{ typedef _Tp&& type; };
template<typename _Tp>
struct add_rvalue_reference
  : public __add_rvalue_reference_helper<_Tp>
{
};
template<typename _Tp>
inline typename add_rvalue_reference<_Tp>::type
declval() noexcept
{
}
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
template<typename _Tp>
class new_allocator
{
};
}
namespace std __attribute__ ((__visibility__ ("default")))
class allocator: public __gnu_cxx::new_allocator<_Tp>
{
  template<typename _Tp1>
  struct rebind
  { typedef allocator<_Tp1> other; };
};
}
namespace std __attribute__ ((__visibility__ ("default")))
template<typename _Alloc, typename _Tp>
struct __alloctr_rebind<_Alloc, _Tp, true>
{
  typedef typename _Alloc::template rebind<_Tp>::other __type;
};
template<typename _Alloc>
struct allocator_traits
{
  template<typename _Tp>
  struct __rebind_alloc
  {
    typedef typename __alloctr_rebind<_Alloc, _Tp>::__type __type;
  };
}
  }
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
template<typename _Alloc>
struct __alloc_traits
{
  typedef std::allocator_traits<_Alloc> _Base_type;
  static constexpr bool _S_nothrow_swap()
  {
    return !_S_propagate_on_swap()
      || noexcept(swap(std::declval<_Alloc&>(), std::declval<_Alloc&>()));
  }
  template<typename _Tp>
  struct rebind
  { typedef typename _Base_type::template __rebind_alloc<_Tp>::__type other; };
};
}
namespace std __attribute__ ((__visibility__ ("default")))
template<typename _Tp, typename _Alloc>
struct _Vector_base
{
  typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template
  rebind<_Tp>::other _Tp_alloc_type;
};
template<typename _Tp, typename _Alloc = std::allocator<_Tp> >
class vector : protected _Vector_base<_Tp, _Alloc>
{
  typedef _Vector_base<_Tp, _Alloc> _Base;
  typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
  typedef __gnu_cxx::__alloc_traits<_Tp_alloc_type> _Alloc_traits;
  swap(vector& __x)
  noexcept(_Alloc_traits::_S_nothrow_swap());
};
}
namespace lexertl
namespace detail
}
namespace detail
template<typename id_type>
struct basic_internals
{
  typedef std::vector<id_type> id_type_vector;
};
};
template<typename char_type, typename id_type = std::size_t>
class basic_state_machine
{
  typedef detail::basic_internals<id_type> internals;
  void minimise ()
  {
    minimise_dfa (dfa_alphabet_, *dfa_, size_);
  }
  typedef typename internals::id_type_vector id_type_vector;
  void minimise_dfa (const id_type dfa_alphabet_,
		     id_type_vector &dfa_, std::size_t size_)
  {
    id_type_vector new_dfa_ (front_, front_ + dfa_alphabet_);
    dfa_.swap (new_dfa_);
  }
}
  }
namespace std __attribute__ ((__visibility__ ("default")))
template<typename _Tp>
void
swap(_Tp&, _Tp&)
  noexcept(__and_<is_nothrow_move_constructible<_Tp>,
	   is_nothrow_move_assignable<_Tp>>::value)
  ;
typedef lexertl::basic_state_machine<char32_t> lexstate;
lexstate m_state_machine;
void GenerateLexer()
{
  m_state_machine.minimise();
}
