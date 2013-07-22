/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-m32 -O2 -fno-omit-frame-pointer -fPIC -std=gnu++11" } */

typedef int int32;
typedef long long int64;
typedef unsigned int uint32;
typedef unsigned long long uint64;
namespace std {
  typedef unsigned int size_t;
  template<class _CharT>
  struct char_traits;
  template<typename _Tp>
  inline _Tp* __addressof(_Tp& __r) noexcept {
    return reinterpret_cast<_Tp*> (&const_cast<char&>(reinterpret_cast<const volatile char&>(__r)));
  }
  template<typename _Tp>
  struct remove_reference {
    typedef _Tp type;
  };
  template<typename _Tp>
  constexpr _Tp&& forward(typename std::remove_reference<_Tp>::type& __t) noexcept {
    return static_cast<_Tp&&>(__t);
  }
}
typedef unsigned int size_t;
extern "C++" {
  inline void* operator new(std::size_t, void* __p) noexcept {
    return __p;
  }
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp>
    class new_allocator {
  public:
    typedef size_t size_type;
    typedef _Tp* pointer;
  };
}
namespace std {
  template<typename _Tp>
  using __allocator_base = __gnu_cxx::new_allocator<_Tp>;
  template<typename _Tp>
  class allocator
    : public __allocator_base<_Tp> {
  public:
    typedef size_t size_type;
    template<typename _Tp1>
    struct rebind {
      typedef allocator<_Tp1> other;
    };
  };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _CharT, typename _Traits, typename _Alloc>
    class __sso_string_base;
  template<typename _CharT, typename _Traits = std::char_traits<_CharT>, typename _Alloc = std::allocator<_CharT>, template <typename, typename, typename> class _Base = __sso_string_base>
    class __versa_string;
  template<typename _CharT, typename _Traits, typename _Alloc>
    struct __vstring_utility {
    typedef typename _Alloc::template rebind<_CharT>::other _CharT_alloc_type;
    template<typename _Alloc1>
    struct _Alloc_hider
      : public _Alloc1 {
      _Alloc_hider(const _Alloc1& __a, _CharT* __ptr)
  : _Alloc1(__a), _M_p(__ptr) {
      }
      _CharT* _M_p;
    };
  };
  template<typename _CharT, typename _Traits, typename _Alloc>
    class __sso_string_base
      : protected __vstring_utility<_CharT, _Traits, _Alloc> {
    typedef __vstring_utility<_CharT, _Traits, _Alloc> _Util_Base;
    typedef typename _Util_Base::_CharT_alloc_type _CharT_alloc_type;
    typedef typename _CharT_alloc_type::size_type size_type;
  private:
    typename _Util_Base::template _Alloc_hider<_CharT_alloc_type>
    _M_dataplus;
    size_type _M_string_length;
    enum {
      _S_local_capacity = 15 };
    union {
      _CharT _M_local_data[_S_local_capacity + 1];
    };
    template<typename _InIterator>
    void _M_construct(_InIterator __beg, _InIterator __end);
  public:
    size_type _M_max_size() const;
    _CharT* _M_data() const {
      return _M_dataplus._M_p;
    }
    size_type _M_length() const {
      return _M_string_length;
    }
    __sso_string_base(const __sso_string_base& __rcs);
    const _CharT_alloc_type& _M_get_allocator() const {
    }
  };
  template<typename _CharT, typename _Traits, typename _Alloc>
    __sso_string_base<_CharT, _Traits, _Alloc>:: __sso_string_base(const __sso_string_base& __rcs)
    : _M_dataplus(__rcs._M_get_allocator(), _M_local_data) {
    _M_construct(__rcs._M_data(), __rcs._M_data() + __rcs._M_length());
  }
  template<typename _CharT, typename _Traits, typename _Alloc, template <typename, typename, typename> class _Base>
    class __versa_string
      : private _Base<_CharT, _Traits, _Alloc> {
  };
}
template<typename _CharT, typename _Traits = std::char_traits<_CharT>, typename _Alloc = std::allocator<_CharT> >
class basic_string
  : public __gnu_cxx::__versa_string<_CharT, _Traits, _Alloc> {
};
typedef basic_string<char> string;
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Alloc, typename _Tp>
    class __alloctr_rebind_helper {
  public:
    static const bool __value = true;
  };
  template<typename _Alloc, typename _Tp, bool = __alloctr_rebind_helper<_Alloc, _Tp>::__value>
    struct __alloctr_rebind;
  template<typename _Alloc, typename _Tp> struct __alloctr_rebind<_Alloc, _Tp, true>
  {
    typedef typename _Alloc::template rebind<_Tp>::other __type;
  };
  template<typename _Alloc>
    struct allocator_traits {
  private:
    template<typename _Tp>
    static typename _Tp::pointer _S_pointer_helper(_Tp*);
    typedef decltype(_S_pointer_helper((_Alloc*)0)) __pointer;
  public:
    typedef __pointer pointer;
    template<typename _Tp>
    using rebind_alloc = typename __alloctr_rebind<_Alloc, _Tp>::__type;
  };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _Alloc> struct __alloc_traits
    : std::allocator_traits<_Alloc>
  {
    typedef std::allocator_traits<_Alloc> _Base_type;
    template<typename _Tp>
    struct rebind {
      typedef typename _Base_type::template rebind_alloc<_Tp>
      other;
    };
  };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _T1, typename... _Args>
    inline void _Construct(_T1* __p, _Args&&... __args) {
    ::new(static_cast<void*>(__p)) _T1(std::forward<_Args>(__args)...);
  }
  template<typename _Tp, typename _Alloc>
    struct _Vector_base {
    typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template rebind<_Tp>::other _Tp_alloc_type;
    typedef typename __gnu_cxx::__alloc_traits<_Tp_alloc_type>::pointer pointer;
    struct _Vector_impl
      : public _Tp_alloc_type {
      pointer _M_start;
      pointer _M_finish;
    };
  public:
    _Vector_impl _M_impl;
  };
  template<typename _Tp, typename _Alloc = std::allocator<_Tp> >
    class vector
      : protected _Vector_base<_Tp, _Alloc> {
    typedef _Vector_base<_Tp, _Alloc> _Base;
  public:
    typedef _Tp value_type;
    typedef typename _Base::pointer pointer;
    typedef size_t size_type;
    size_type size() const;
    void push_back(const value_type& __x) {
      _M_emplace_back_aux(__x);
    }
    template<typename... _Args>
    void _M_emplace_back_aux(_Args&&... __args);
    size_type _M_check_len();
  };
  template<typename _Tp, typename _Alloc> template<typename... _Args>
    void vector<_Tp, _Alloc>:: _M_emplace_back_aux(_Args&&... __args) {
    const size_type __len = _M_check_len();
    pointer __new_start(static_cast<pointer>(::operator new(__len * sizeof(_Tp))));
    pointer __new_temp(__new_start + size());
    ::new((void *)__new_temp) _Tp(std::forward<_Args>(__args)...);
    pointer __cur = __new_start;
    pointer __first = this->_M_impl._M_start;
    pointer __last = this->_M_impl._M_finish;
    for (;
	 __first != __last;
	 ++__first, ++__cur) std::_Construct(std::__addressof(*__cur), *__first);
  }
}
using std::vector;
class DL {
public:
  struct ChunkId {
    int64 disk_id;
    uint64 handle;
    uint64 version;
    string capability;
    ChunkId();
  };
  struct ChunkInfo {
    ChunkId id;
    uint64 mtime;
    uint32 length;
    int32 space_used;
  };
};
class FDB {
  void CollectChunk(const DL::ChunkInfo& chunk, const int& location);
private:
  struct ChunkData {
    int location;
    DL::ChunkInfo chunk_info;
  };
  vector<ChunkData> chunk_data_;
};
void FDB::CollectChunk(const DL::ChunkInfo& chunk, const int& location) {
  ChunkData chunk_data;
  chunk_data_.push_back( chunk_data);
}
