// PR c++/122253
// { dg-additional-options "-fmodules -Wtemplate-names-tu-local" }

export module M;

template <int> struct ic {};
struct S {
  constexpr operator int() const { return 5; }
  constexpr int operator&() const { return 8; }
};

template <typename T> inline void a(T) {
  T a;
  static T b;
  ic<a>{};
  ic<b>{};
  ic<&a>{};
  ic<&b>{};
}

template <typename T> inline auto b(T x) {
  return [&](auto y) {
    return [=](auto z) {
      return ic<(int)x + (int)&y + (int)z>{};
    };
  };
}

template void a(S);
ic<5 + 8 + 5> x = b(S{})(S{})(S{});
