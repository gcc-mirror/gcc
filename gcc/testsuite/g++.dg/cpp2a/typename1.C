// P0634R3
// { dg-do compile { target c++20 } }

// OK, return type of a function declaration at global scope.
template<class T> T::R f();

// Ill-formed (no diagnostic required), attempt to declare
// a void variable template
template<class T> void f(T::R); // { dg-error "declared void" }

template <class T> struct A;
template <class T> using B = A<T>::U;

template<typename T>
struct PtrTraits { typedef int Ptr; };

template<class T> struct S {
  // OK, in a defining-type-id.
  using Ptr = PtrTraits<T>::Ptr;

  // OK, trailing-return-type.
  auto g() -> S<T*>::Ptr;

  // OK, class scope
  T::R
  f (T::P p)
  {
    // OK, type-id of a static_cast
    return static_cast<T::R>(p);
  }
};

template<typename T>
void f ()
{
  // Variable pf of type void* initialized with T::X
  void (*pf)(T::X);

  // Error: T::X at block scope does not denote a type
  // (attempt to declare a void variable)
  void g(T::X); // { dg-error "declared void" }
}
