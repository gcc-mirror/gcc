// PR c++/120503
// Implement P2115r0 "merging definitions of unnamed unscoped enums"
// { dg-do compile { target c++17 } }

template<auto V> int Frob () { return int (V); }

enum { A = (unsigned int)12345, B = 0 };
template int Frob<A> ();
template int Frob<B> ();
// { dg-final { scan-assembler {_Z4FrobITnDaLUej1A12345EEiv:} { target { c++20 && { ! short_enums } } } } }
// { dg-final { scan-assembler {_Z4FrobITnDaL8._anon_012345EEiv:} { target c++17_down } } }
// { dg-final { scan-assembler {_Z4FrobITnDaLUej1A0EEiv:} { target { c++20 && { ! short_enums } } } } }
// { dg-final { scan-assembler {_Z4FrobITnDaL8._anon_00EEiv:} { target c++17_down } } }

enum { C = 5 } typedef X;
template int Frob<C> ();
// typedef name 'X' should take precedence
// { dg-final { scan-assembler {_Z4FrobITnDaL1X5EEiv:} } }

typedef enum : long long { D = 8 }* Y;
template int Frob<D> ();
// but 'Y' is not a typedef name here
// { dg-final { scan-assembler {_Z4FrobITnDaLUex1D8EEiv:} { target c++20 } } }
// { dg-final { scan-assembler {_Z4FrobITnDaL8._anon_28EEiv:} { target c++17_down } } }

enum : int { E };
void foo(decltype(E), decltype(E)) {}
// { dg-final { scan-assembler {_Z3fooUei1ES_:} { target c++20 } } }
// { dg-final { scan-assembler {_Z3foo8._anon_3S_:} { target c++17_down } } }
