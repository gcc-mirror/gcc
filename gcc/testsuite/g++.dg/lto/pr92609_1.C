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
namespace __cxx11 {
template < typename, typename, typename > class basic_string {
  struct _Alloc_hider {
    allocator_traits::pointer _M_p;
  } _M_dataplus;
  unsigned long _M_string_length;
  enum { _S_local_capacity = 15 };
  union {
    char _M_local_buf[_S_local_capacity + 1];
    unsigned long _M_allocated_capacity;
  };
};
} // __cxx11
} // std
class wxRefCounter;
class wxObject {
  virtual int GetClassInfo();
  wxRefCounter *m_refData;
};
class wxGDIObject : wxObject {};
class wxFontBase : wxGDIObject {};
class wxFont : wxFontBase {};
template < class > class map {};
namespace {
struct OpenGLTextGlyph;
}
typedef map< OpenGLTextGlyph > glyphMap;
class OpenGLText {
  float r, g, b, a;
  int fontSize;
  bool fontBold;
  bool fontItalics;
  std::basic_string< char > fontFace;
  wxFont font;
  glyphMap glyphs;
  OpenGLText();
};
OpenGLText::OpenGLText() {}
