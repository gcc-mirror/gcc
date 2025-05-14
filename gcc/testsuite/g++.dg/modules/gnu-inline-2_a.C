// PR c++/119154
// { dg-additional-options "-fmodules" }
// { dg-module-cmi xstd }

export module xstd;

inline __attribute__((__gnu_inline__)) void wmemset() {}

extern "C++" template <class> struct char_traits {
  void assign() { wmemset(); }
};
