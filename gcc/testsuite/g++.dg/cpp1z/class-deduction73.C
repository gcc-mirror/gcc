// PR c++/90210
// { dg-do compile { target c++17 } }

template<typename T> struct tuple { tuple(T); };
template<typename T> explicit tuple(T t) -> tuple<T>;
tuple t = { 1 }; // { dg-error "explicit deduction guide selected" }
tuple t1 = tuple{ 1 };
tuple t2{ 1 };

template<typename T> struct A { A(T, T); };
template<typename T> explicit A(T, T) -> A<int>;
A a = {1, 1}; // { dg-error "explicit deduction guide selected" }
A a1 = A{1, 1};
A a2{1, 1};

template<typename T, typename U>
struct B {
  B(T, U);
};
template<typename T, typename U>
B(T, U) -> B<T, typename U::type>; // SFINAEd-out
B b = { 1, 2 }; // OK
B b1 = B{ 1, 2 }; // OK
B b2{ 1, 2 }; // OK

// Overriden implicit default constructor deduction guide:
template<typename T>
struct C { };
explicit C() -> C<int>;
C c = {}; // { dg-error "explicit deduction guide selected" }
C c1 = C{};
C c2{};

// Overriden copy guide:
template<typename T>
struct D { };
template<typename T> explicit D(D<T>) -> D<T>;
D<int> d;
D d1 = {d}; // { dg-error "explicit deduction guide selected" }
D d2 = D{d};
D d3{d};
