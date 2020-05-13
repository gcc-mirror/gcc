// { dg-do compile { target c++20 } }

// Test the types of atomic constraints

// req5.C
struct fool {
  constexpr fool operator&&(fool) const { return {}; }
  constexpr fool operator||(fool) const { return {}; }
};

template<typename T> constexpr fool p1() { return {}; }
template<typename T> constexpr fool p2() { return {}; }

template<typename T>
concept Bad = p1<T>() && p2<T>(); // { dg-error "bool" }

template<typename T> requires Bad<T> void bad(T x) { }

void driver_2()
{
  bad(0); // { dg-message "" }
}

// req6.C
struct X { };
int operator==(X, X) { return 0; }

template<typename T>
concept C1 = (X()); // { dg-error "bool" }

template<typename T>
concept C2 = (X() == X()); // { dg-error "bool" }

template<typename T>
  requires C1<T>
void h1(T) { }

template<typename T>
  requires C2<T>
void h2(T);

void driver_3()
{
  h1(0); // { dg-message "" }
  h2(0); // { dg-message "" }
}

// req7.C
template<bool B>
struct boolean_constant
{
  constexpr operator bool() const { return B; }
};

using true_type = boolean_constant<true>;
using false_type = boolean_constant<false>;

template<typename T>
struct dependent_true : true_type { };

template<typename T>
struct dependent_false : false_type { };

template<typename T>
  requires (dependent_true<T>{}) // { dg-message "bool" }
struct S5 { };

template<typename T>
  requires (dependent_false<T>{}) // { dg-message "bool" }
struct S6 { };

S5<int> x5; // { dg-error "template constraint failure" }
S6<int> x6; // { dg-error "template constraint failure" }

