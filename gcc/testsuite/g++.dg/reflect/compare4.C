// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test comparison of reflections.

using info = decltype(^^void);

int x;
void fn();
struct S {
  int x;
  static constexpr int s_x = 11;
  void fn();
  static void s_fn();
};
enum Enum { A, B, C };
enum class EnumCls { A, B, C };

static_assert(&[:^^x:] == &x);
static_assert([:^^fn:] == fn);

static_assert(&[:^^S::x:] == &S::x);
static_assert(&[:^^S::s_x:] == &S::s_x);
static_assert(&[:^^S::fn:] == &S::fn);
static_assert([:^^S::s_fn:] == S::s_fn);

static_assert([:^^Enum::B:] == Enum::B);
static_assert([:^^EnumCls::B:] == EnumCls::B);
