// PR c++/119755
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename _Out> void format(_Out) {
  constexpr int __term = 1;
  [&] { __term; };
  [&] { const int outer = __term; { __term; } };
  [&]() noexcept { __term; };
  [&]() noexcept { const int outer = __term; { __term; } };
  [&](auto) { int n[__term]; }(0);
  [&](auto) noexcept { int n[__term]; }(0);
}

inline void vformat() {
  format(0);
}
