// { dg-do compile { target c++20 } }

template<typename T>
concept Class = __is_class(T);

template<typename T>
concept Union = __is_union(T);

template<typename T>
concept One = sizeof(T) >= 4;

template<typename T>
concept Two = One<T> && sizeof(T) >= 8;

// Basic checks
template<typename T> requires true struct ok { };
template<typename T> requires false struct err { };

ok<int> ok1;
err<int> err1; // { dg-error "template constraint failure" }
err<int>* err2; // { dg-error "template constraint failure" }

// Redeclarations
template<typename T>
  requires Class<T>
struct S1;

template<Class T> // { dg-error "template parameter | different constraints" }
struct S1 { };

template<typename T>
  requires Class<T>
struct S2;

template<typename T>
  requires Union<T>
struct S2; // { dg-error "redeclaration | different constraints" }


// Check non-overlapping specializations
template<typename T>
struct S3 { static const int value = 0; };

template<typename T>
  requires Class<T>
struct S3<T> { static const int value = 1; };

template<typename T>
  requires Union<T>
struct S3<T> { static const int value = 2; };

struct S { };
union U { };

static_assert(S3<int>::value == 0, "");
static_assert(S3<S>::value == 1, "");
static_assert(S3<U>::value == 2, "");

// Check ordering of partial specializations
template<typename T>
struct S4 { static const int value = 0;  };

template<typename T>
  requires One<T>
struct S4<T> { static const int value = 1; };

template<typename T>
  requires Two<T>
struct S4<T> { static const int value = 2; };

struct one_type { char x[4]; };
struct two_type { char x[8]; };

static_assert(S4<char>::value == 0, "");
static_assert(S4<one_type>::value == 1, "");
static_assert(S4<two_type>::value == 2, "");

// Specializations are more specialized.
template<typename T> requires Two<T> struct S5 { };
template<typename T> requires One<T> struct S5<T> { }; // { dg-error "does not specialize" }

// Constraints are checked even when decls are not instantiatied.
S5<one_type>* x4b; // { dg-error "constraint|invalid" }

// Deduction guides
template <class T>
concept IsInt = __is_same_as(T,int);

template<typename T>
struct A
{
  int i;
  A(...);
};

template<typename I>
  requires IsInt<I>
A(I) -> A<I>;

A a(1);
A a2(1.0);      // { dg-error "class template argument deduction | no matching function for call" }


template<typename T>
struct S6
{
  template<typename U>
    requires true
  struct Inner;
};

template<typename T>
template<typename U>
struct S6<T>::Inner { }; // { dg-error "does not match" }

