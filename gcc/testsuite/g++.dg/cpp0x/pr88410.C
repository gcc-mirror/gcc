// PR c++/88410
// { dg-do compile { target c++11 } }

typedef __UINTPTR_TYPE__ uintptr_t;
const uintptr_t a = 32;
struct C { int b; int c; };
uintptr_t d { uintptr_t (&reinterpret_cast<C *>(a)->c) - a };
