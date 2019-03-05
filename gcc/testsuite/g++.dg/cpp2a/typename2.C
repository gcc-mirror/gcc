// P0634R3
// { dg-do compile { target c++2a } }

template<class T> typename T::R f();

template <class T> struct A;
template <class T> using B = typename A<T>::U;

template<typename T>
struct PtrTraits { typedef int Ptr; };

template<class T> struct S {
  using Ptr = typename PtrTraits<T>::Ptr;

  auto g() -> typename S<T*>::Ptr;

  typename T::R
  f (typename T::P p)
  {
    return static_cast<typename T::R>(p);
  }
};
