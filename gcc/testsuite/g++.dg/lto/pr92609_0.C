// PR lto/92609
// { dg-lto-do link }
// { dg-lto-options { { -fPIC -flto } } }
// { dg-require-effective-target shared }
// { dg-require-effective-target fpic }
// { dg-extra-ld-options "-shared" }

namespace std {
inline namespace __cxx11 {}
template < typename _Default > struct __detector { using type = _Default; };
template < typename _Default, template < typename > class >
using __detected_or = __detector< _Default >;
template < typename _Default, template < typename > class _Op >
using __detected_or_t = typename __detected_or< _Default, _Op >::type;
template < typename > class allocator;
template < class > struct char_traits;
namespace __cxx11 {
template < typename _CharT, typename = char_traits< _CharT >,
           typename = allocator< _CharT > >
class basic_string;
}
struct __allocator_traits_base {
  template < typename _Tp > using __pointer = typename _Tp::pointer;
};
struct allocator_traits : __allocator_traits_base {
  using pointer = __detected_or_t< char *, __pointer >;
};
} // std
struct rebind {
  typedef std::allocator_traits other;
};
namespace std {
namespace __cxx11 {
template < typename, typename, typename > class basic_string {
  struct _Alloc_hider {
    rebind::other::pointer _M_p;
  } _M_dataplus;
  unsigned long _M_string_length;
  enum { _S_local_capacity = 15 };
  union {
    char _M_local_buf[_S_local_capacity + 1];
    unsigned long _M_allocated_capacity;
  };
};
} // __cxx11
template < typename _Tp > class __uniq_ptr_impl {
  template < typename _Up > struct _Ptr { using type = _Up *; };

public:
  using pointer = typename _Ptr< _Tp >::type;
};
template < typename _Tp > class unique_ptr {
public:
  using pointer = typename __uniq_ptr_impl< _Tp >::pointer;
  unique_ptr(pointer);
};
} // std
class wxRefCounter;
class wxObject {
  virtual wxRefCounter CreateRefData();
  wxRefCounter *m_refData;
};
class wxGDIObject : wxObject {};
class wxFontBase : wxGDIObject {};
class wxFont : wxFontBase {};
class VisualTool {
protected:
  VisualTool(int *, int *);
};
class OpenGLText;
class VisualToolCross : VisualTool {
  std::unique_ptr< OpenGLText > gl_text;
  VisualToolCross();
};
class OpenGLText { // { dg-lto-warning "7: type 'struct OpenGLText' violates the C\\+\\+ One Definition Rule" }
  float r, g, b, a;
  int fontSize;
  bool fontBold;
  bool fontItalics;
  std::basic_string< char > fontFace;
  wxFont font;
  int glyphs;
};
int VisualToolCross_parent;
int VisualToolCross_context;
VisualToolCross::VisualToolCross()
    : VisualTool(&VisualToolCross_parent, &VisualToolCross_context),
      gl_text(0) {}
