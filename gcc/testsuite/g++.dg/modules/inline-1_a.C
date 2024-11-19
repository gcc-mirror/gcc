// { dg-additional-options -fmodules }
// { dg-module-do run }
export module M;

inline int b = 42;
struct A
{
  static inline int a = 4200;
};

export inline int f() { return b+A::a; }
