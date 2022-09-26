// Test for conversion from stateless lambda to function pointer.

// { dg-do compile { target c++11_only } }
// { dg-final { scan-assembler "weak\[^\n\r\]*_?_ZZ1fvENKUlvE_cvPFvvEEv" { target { ! { *-*-darwin* *-*-mingw* *-*-cygwin } } } } }

inline void f()
{
  void (*pfn)() = []{};
}

int main()
{
  f();
}
