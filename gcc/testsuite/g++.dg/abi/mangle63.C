// DR 850 makes this valid
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

template<class T> struct A
{
  int mem;
  template<class U> decltype(U()+mem) f();
};
int i = A<int>().f<int>();

// { dg-final { scan-assembler "_ZN1AIiE1fIiEEDTplcvT__E3memEv" } }
