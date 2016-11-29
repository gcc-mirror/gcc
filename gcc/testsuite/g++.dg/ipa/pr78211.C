// PR lto/78211
// { dg-do compile { target { lto && c++11 } } }
// { dg-options "-fcompare-debug -fno-printf-return-value -flto -fno-use-linker-plugin -O3" }

namespace std {
  typedef __SIZE_TYPE__ size_t;
  inline namespace __cxx11 { }
  template<typename...> using __void_t = void;
  template<class _E>
  class initializer_list {
    typedef size_t size_type;
    typedef const _E* iterator;
    iterator _M_array;
    size_type _M_len;
  };
}
extern "C++" {
  namespace std {
    template<typename _Tp> struct __is_char { enum { __value = 1 }; };
  }
  namespace __gnu_cxx {
    template<bool, typename> struct __enable_if { };
    template<typename _Tp> struct __enable_if<true, _Tp> { typedef _Tp __type; };
  }
}
namespace std {
  template<typename _Iterator, typename = __void_t<>> struct __iterator_traits { };
  template<typename _Iterator> struct iterator_traits : public __iterator_traits<_Iterator> { };
  template<typename _Tp> struct iterator_traits<_Tp*> { typedef _Tp& reference; };
}
namespace __gnu_cxx {
  using std::iterator_traits;
  template<typename _Iterator, typename _Container> class __normal_iterator {
    typedef iterator_traits<_Iterator> __traits_type;
   public:
    typedef typename __traits_type::reference reference;
    reference operator*() const noexcept { }
  };
  template<typename _IteratorL, typename _IteratorR, typename _Container>
  inline bool operator!=(const __normal_iterator<_IteratorL, _Container>& __lhs, const __normal_iterator<_IteratorR, _Container>& __rhs) noexcept { }
}
namespace std {
  template<typename _CharT> struct char_traits;
  template<typename> class allocator;
  template<typename _Alloc> struct allocator_traits { };
  template<typename _Tp> struct allocator_traits<allocator<_Tp>> {
    using const_pointer = const _Tp*;
    template<typename _Up> using rebind_alloc = allocator<_Up>;
  };
}
namespace __gnu_cxx {
  template<typename _Alloc>   struct __alloc_traits : std::allocator_traits<_Alloc>   {
    typedef std::allocator_traits<_Alloc> _Base_type;
    template<typename _Tp> struct rebind {
      typedef typename _Base_type::template rebind_alloc<_Tp> other;
    };
  };
}
namespace std {
  namespace __cxx11 {
    template<typename _CharT, typename _Traits = char_traits<_CharT>, typename _Alloc = allocator<_CharT> > class basic_string;
    typedef basic_string<char> string;
  }
  template<typename _CharT> inline typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value, bool>::__type
  operator==(const basic_string<_CharT>& __lhs, const basic_string<_CharT>& __rhs) noexcept { }
  template<typename _Tp, typename _Alloc> struct _Vector_base {
    typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template rebind<_Tp>::other _Tp_alloc_type;
  };
  template<typename _Tp, typename _Alloc = std::allocator<_Tp> > class vector {
    typedef _Vector_base<_Tp, _Alloc> _Base;
    typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
    typedef __gnu_cxx::__alloc_traits<_Tp_alloc_type> _Alloc_traits;
   public:
    typedef typename _Alloc_traits::const_pointer const_pointer;
    typedef __gnu_cxx::__normal_iterator<const_pointer, vector> const_iterator;
    const_iterator end() const noexcept { }
  };
}
class VwViewPlane;
class VwViewer {
  std::vector<VwViewPlane*> mViewPlaneList;
  VwViewPlane* FindViewPlane (const std::string& name);
  const VwViewPlane* FindViewPlane (const std::string& name) const;
};
class VwAssimilatorStickyBox;
class VwViewer_2D final {
  VwAssimilatorStickyBox* mp_stickyAssimilator;
  void drawStickyBox();
  void undrawStickyBox();
};
struct VwViewPlane {
  const std::string& GetName() const { }
};
struct VwAssimilator_2D {
  virtual int DrawNext() = 0;
};
class VwAssimilator_2D_Geometry : public VwAssimilator_2D { };
class VwAssimilatorStickyBox final : public VwAssimilator_2D_Geometry { };
VwViewPlane* VwViewer::FindViewPlane (const std::string& name) {
  VwViewPlane* p_result = __null;
  std::vector<VwViewPlane*>::const_iterator it;
  while (it != mViewPlaneList.end()) {
    if ((*it) -> GetName() == name ) break;
  }
  return p_result;
}
const VwViewPlane* VwViewer::FindViewPlane (const std::string& name) const {
  VwViewPlane* p_result = __null;
  std::vector<VwViewPlane*>::const_iterator it;
  while (it != mViewPlaneList.end()) {
    if ((*it) -> GetName() == name ) break;
  }
  return p_result;
}
void VwViewer_2D::drawStickyBox() {
  mp_stickyAssimilator->DrawNext();
}
void VwViewer_2D::undrawStickyBox() {
  mp_stickyAssimilator->DrawNext();
}
