// PR c++/84314
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }

struct a {
  void b(long() __attribute__((fastcall))) {}
};
