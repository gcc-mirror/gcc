// { dg-do compile { target c++14 } }
// PR 94426 ICE mangling lambda

template <bool> using Void = void;

template <typename U> bool Init (U) {return true;}
template <typename> bool VAR = Init ([] {});

template <typename T>
Void<false && VAR<T>> Foo (T)
{}

void q ()
{
  Foo ([] {});
}

// The instantiation of VAR becomes local
// { dg-final { scan-assembler {.local	_Z3VARIZ1qvEUlvE_E} { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler {.comm	_Z3VARIZ1qvEUlvE_E,1,1} { target { i?86-*-* x86_64-*-* } } } }
