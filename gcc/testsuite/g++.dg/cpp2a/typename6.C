// P0634R3
// { dg-do compile { target c++20 } }

// (5.2.1) simple-declaration or a function-definition in namespace scope

template<typename T>
T::X fn1 (int);

template<typename T>
T::X fn1 (int)
{
  return 42;
}

template<typename T>
T::X v1;

namespace N {
  template<typename T>
  T::X v2;
}

// (5.2.2) member-declaration

template<typename T>
struct S {
  [[noreturn]] T::X fn2 ();
  T::X fn3 ();
  T::X fn4 () { return 5; }
  T::X fn5 () final;
  T::X fn6 () = 0;
  T::X fn8 () override;
  T::X v3;
  T::X *v4;
  T::X v5[5];
  T::X v6 = 0;
  T::X v7{0};
  T::X v8 : 16;
  static constexpr T::X v9 = 0;
  typedef T::X T2;
  friend T::X fn7<int> ();
  static inline T::X v10;
};

// (5.2.3) parameter-declaration in a member-declaration,
// unless that parameter-declaration appears in a default argument

template<typename T>
struct S2 {
  friend int fn1<T::X> ();
  int fn2 (T::X p);
  int fn5 (int = T::X);
};

// (5.2.4) parameter-declaration in a declarator of a function or function
// template declaration whose declarator-id is qualified,
// unless that parameter-declaration appears in a default argument

struct M {
  template<typename T>
  int fn (T::X);
};

template<typename T>
int M::fn (T::X p) { return p; }

// (5.2.5) parameter-declaration in a lambda-declarator,
// unless that parameter-declaration appears in a default argument

void
fn5 ()
{
  auto j = []<typename T>(T::X t, int i) { return i; };
}

// (5.2.6) parameter-declaration of a (non-type) template-parameter

template<typename T, T::X N>
struct S3 { };

// default argument of a type-parameter of a template
template<typename T, typename U = T::X>
struct S4 { };

// type-id of a static_cast, const_cast, reinterpret_cast, or dynamic_cast
template<typename T>
struct S5 {
  void fn6 (T::X p)
  {
    int i = static_cast<T::Y>(p);
    i = dynamic_cast<T::Y>(p);
    i = reinterpret_cast<T::Y>(p);
    i = const_cast<T::Y>(p);
  }
};

template<typename T>
void fn7 (typename T::X p)
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
  new T::X[10];
}

// defining-type-id

template<typename T>
struct W { typedef int M; };

template<typename T>
struct S6 {
  using TT = T::X;
  using TT2 = W<T>::M;
};

// trailing-return-type
template<typename T>
struct S7 {
  auto fn9() -> W<T>::M;
};
