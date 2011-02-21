// Test for conversion from stateless lambda to function pointer.

// { dg-options -std=c++0x }
// { dg-final { scan-assembler "weak\[^\n\r\]*_?_ZZ1fvENKUlvE_cvPFvvEEv" { target { ! { *-*-darwin* *-*-mingw* *-*-cygwin *-*-hpux10* } } } } }

inline void f()
{
  void (*pfn)() = []{};
}

int main()
{
  f();
}
