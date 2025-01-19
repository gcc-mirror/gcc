// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi !{} }
// external linkage variables or functions in header units must
// not have non-inline definitions

struct S { int x; };

int x_err;  // { dg-error "external linkage definition" }
int y_err = 123;  // { dg-error "external linkage definition" }
auto [d_err] = S{};  // { dg-error "external linkage definition" }
void f_err() {}  // { dg-error "external linkage definition" }

struct Err {
  Err();
  void m();
  static void s();
  static int x;
  static int y;
};
Err::Err() = default;  // { dg-error "external linkage definition" }
void Err::m() {}  // { dg-error "external linkage definition" }
void Err::s() {}  // { dg-error "external linkage definition" }
int Err::x;  // { dg-error "external linkage definition" }
int Err::y = 123;  // { dg-error "external linkage definition" }

// No definition, OK
extern int y_decl;
void f_decl();

template <typename> struct DeductionGuide {};
DeductionGuide() -> DeductionGuide<int>;

struct NoDefStatics {
  enum E { V };
  static const int x = 123;
  static const E e = V;
};

// But these have definitions again (though the error locations aren't great)
struct YesDefStatics {
  enum E { V };
  static const int x = 123;  // { dg-error "external linkage definition" }
  static const E e = V;  // { dg-error "external linkage definition" }
};
const int YesDefStatics::x;
const YesDefStatics::E YesDefStatics::e;

// Inline decls are OK
inline int x_inl;
inline int y_inl = 123;
inline void f_inl() {}
constexpr void g_inl() {}
void h_inl() = delete;

struct Inl {
  void m() {}
  static void s() {}
  static inline int x;
  static inline int y = 123;
};

// Internal linkage decls are OK
static int x_internal;
static int y_internal = 123;
namespace { auto [d_internal] = S{}; }
static void f_internal() {}

namespace {
  struct Internal {
    void m();
    static void s();
    static int x;
    static int y;
  };
  void Internal::m() {}
  void Internal::s() {}
  int Internal::x;
  int Internal::y = 123;
}

// Function-scope entities are OK
inline void f_static() {
  static int x_static;
  static int y_static = 123;
  thread_local int x_thread_local;
  thread_local int y_thread_local = 123;

#if __cplusplus >= 202002L
  static auto [d_static] = S{};
  thread_local auto [d_thread_local] = S{};
#endif

  x_static = y_static;
  x_thread_local = y_thread_local;
}

// Templates (not functions or variables) are OK
template <typename> int x_tpl;
template <typename> int y_tpl = 123;
template <typename> void f_tpl() {}

struct Template_Body {
  template <typename> void m();
  template <typename> static void s();
  template <typename> static int x;
  template <typename> static int y;
};
template <typename> void Template_Body::m() {}
template <typename> void Template_Body::s() {}
template <typename> int Template_Body::x;
template <typename> int Template_Body::y = 123;

template <typename> struct Template_Type {
  void m();
  static void s();
  static int x;
  static int y;
};
template <typename T> void Template_Type<T>::m() {}
template <typename T> void Template_Type<T>::s() {}
template <typename T> int Template_Type<T>::x;
template <typename T> int Template_Type<T>::y = 123;

// Implicit instantiations are OK
inline void instantiate_tmpls() {
  x_tpl<int> = y_tpl<int>;
  f_tpl<int>();

  Template_Body{}.m<int>();
  Template_Body::s<int>();
  Template_Body::x<int> = Template_Body::y<int>;

  using TT = Template_Type<int>;
  TT{}.m();
  TT::s();
  TT::x = TT::y;
}

// Explicit instantiations are also OK (extern or otherwise)
template int x_tpl<char>;
template int y_tpl<char>;
template void f_tpl<char>();

template void Template_Body::m<char>();
template void Template_Body::s<char>();
template int Template_Body::x<char>;
template int Template_Body::y<char>;

template void Template_Type<char>::m();
template void Template_Type<char>::s();
template int Template_Type<char>::x;
template int Template_Type<char>::y;

extern template int x_tpl<double>;
extern template int y_tpl<double>;
extern template void f_tpl<double>();

extern template void Template_Body::m<double>();
extern template void Template_Body::s<double>();
extern template int Template_Body::x<double>;
extern template int Template_Body::y<double>;

extern template void Template_Type<double>::m();
extern template void Template_Type<double>::s();
extern template int Template_Type<double>::x;
extern template int Template_Type<double>::y;

// But explicit specialisations are not (though note [temp.expl.spec] p13)
template <> int x_tpl<short>;  // { dg-error "inline" }
template <> int y_tpl<short> = 123;  // { dg-error "inline" }
template <> void f_tpl<short>() {}  // { dg-error "inline" }

template <> void Template_Body::m<short>() {}  // { dg-error "inline" }
template <> void Template_Body::s<short>() {}  // { dg-error "inline" }
template <> int Template_Body::x<short>;  // { dg-bogus "inline" "not a definition" }
template <> int Template_Body::y<short> = 123;  // { dg-error "inline" }

template <> void Template_Type<short>::m() {}  // { dg-error "inline" }
template <> void Template_Type<short>::s() {}  // { dg-error "inline" }
template <> int Template_Type<short>::x;  // { dg-bogus "inline" "not a definition" }
template <> int Template_Type<short>::y = 123;  // { dg-error "inline" }
