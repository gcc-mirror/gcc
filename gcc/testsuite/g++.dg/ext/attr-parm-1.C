// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// PR 94946
class a {
  template <typename b> a(b (*)());
  template <typename b> a(b(__attribute__((fastcall)) *c)());
};
