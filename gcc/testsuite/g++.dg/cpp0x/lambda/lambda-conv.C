// Test for conversion from stateless lambda to function pointer, which is
// not yet part of the draft but hopefully will be after the March 2010
// meeting.

// { dg-options -std=c++0x }
// { dg-final { scan-assembler "weak\[^\n\r\]*_?_ZZ1fvENUlvE_cvPFvvEEv" { target { ! { *-*-darwin* *-*-mingw* *-*-cygwin } } } } }

inline void f()
{
  void (*pfn)() = []{};
}

int main()
{
  f();
}
