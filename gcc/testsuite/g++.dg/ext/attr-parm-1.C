// { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ia32 || lp64 } } } }
// PR 94946
class a {
#ifdef __LP64__
  template <typename b> a(b(__attribute__((sysv_abi)) *c)());
  template <typename b> a(b(__attribute__((ms_abi)) *c)());
#else
  template <typename b> a(b (*)());
  template <typename b> a(b(__attribute__((fastcall)) *c)());
#endif
};
