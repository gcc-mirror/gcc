// { dg-do compile { target c++14 } }
// PR 94426 ICE mangling lambda
// { dg-options {-flto -O2} }
// { dg-require-effective-target lto }

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
