// P0634R3
// { dg-do compile { target c++17_only } }

// (5.2.1) simple-declaration or a function-definition in namespace scope

template<typename T>
T::X fn1 (int); // { dg-error "need .typename." }

template<typename T>
T::X fn1 (int) // { dg-error "need .typename." }
{
  return 42;
}

template<typename T>
T::X v1; // { dg-error "need .typename." }

namespace N {
  template<typename T>
  T::X v2; // { dg-error "need .typename." }
}

// (5.2.2) member-declaration

template<typename T>
struct S {
  [[noreturn]] T::X fn2 (); // { dg-error "need .typename." }
  T::X fn3 (); // { dg-error "need .typename." }
  T::X fn4 () { return 5; } // { dg-error "need .typename." }
  T::X fn5 () final; // { dg-error "need .typename." }
  T::X fn6 () = 0; // { dg-error "need .typename." }
  T::X fn8 () override; // { dg-error "need .typename." }
  T::X v3; // { dg-error "need .typename." }
  T::X *v4; // { dg-error "need .typename." }
  T::X v5[5]; // { dg-error "need .typename." }
  T::X v6 = 0; // { dg-error "need .typename." }
  T::X v7{0}; // { dg-error "need .typename.|;" }
  T::X v8 : 16; // { dg-error "need .typename." }
  static constexpr T::X v9 = 0; // { dg-error "need .typename." }
  typedef T::X T2; // { dg-error "need .typename." }
  friend T::X fn7<int> (); // { dg-error "need .typename." }
  static inline T::X v10; // { dg-error "need .typename." }
};

// (5.2.3) parameter-declaration in a member-declaration,
// unless that parameter-declaration appears in a default argument

template<typename T>
struct S2 {
  friend int fn1<T::X> (); // { dg-error "" }
  int fn2 (T::X p); // { dg-error "not a type" }
  int fn5 (int = T::X);
};

// (5.2.4) parameter-declaration in a declarator of a function or function
// template declaration whose declarator-id is qualified,
// unless that parameter-declaration appears in a default argument
template<typename T>
int fn3 (T::X);

template<typename T>
int fn4 (T::X p) { return p; } // { dg-error "" }

// (5.2.6) parameter-declaration of a (non-type) template-parameter

template<typename T, T::X N> // { dg-error "not a type" }
struct S3 { };

// default argument of a type-parameter of a template
template<typename T, typename U = T::X> // { dg-error "need .typename." }
struct S4 { };

// type-id of a static_cast, const_cast, reinterpret_cast, or dynamic_cast
template<typename T>
struct S5 {
  void fn6 (T::X p) // { dg-error "not a type" }
  {
    int i = static_cast<T::Y>(p); // { dg-error "need .typename." }
    i = dynamic_cast<T::Y>(p); // { dg-error "need .typename." }
    i = reinterpret_cast<T::Y>(p); // { dg-error "need .typename." }
    i = const_cast<T::Y>(p); // { dg-error "need .typename." }
  }
};

template<typename T>
void fn7 (T::X p) // { dg-error "" }
{
  int i = static_cast<T::Y>(p);
  i = dynamic_cast<T::Y>(p);
  i = reinterpret_cast<T::Y>(p);
  i = const_cast<T::Y>(p);
}

// new-type-id
template<typename T>
void
fn8 ()
{
  new T::X[10]; // { dg-error "need .typename." }
}

// defining-type-id

template<typename T>
struct W { typedef int M; };

template<typename T>
struct S6 {
  using TT = T::X; // { dg-error "need .typename." }
  using TT2 = W<T>::M; // { dg-error "need .typename." }
};

// trailing-return-type
template<typename T>
struct S7 {
  auto fn9() -> W<T>::M; // { dg-error "need .typename." }
};
