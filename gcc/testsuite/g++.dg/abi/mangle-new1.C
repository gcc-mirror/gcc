// PR c++/119316
// { dg-do compile { target c++11 } }

template <unsigned> struct A { };
template<typename T>
auto foo(unsigned n) -> A<sizeof(new T[n])>
{ return {}; }
int main() { foo<int>(5); }

// { dg-final { scan-assembler {_Z3fooIiE1AIXszna_Afp__T_EEEj} } }
