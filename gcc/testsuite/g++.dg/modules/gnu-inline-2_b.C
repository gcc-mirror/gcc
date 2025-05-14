// PR c++/119154
// { dg-additional-options "-fmodules" }

template <typename> struct char_traits {
  void assign();
};

void foo(char_traits<wchar_t> s) {
  s.assign();
}

import xstd;

// Lazy loading at EOF of a gnu_inline declaration should not ICE.
