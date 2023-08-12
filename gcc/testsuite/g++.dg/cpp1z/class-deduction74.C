// PR c++/97099
// { dg-do compile { target c++17 } }
// [temp.deduct.guide]p3: Two deduction guide declarations in the same
// translation unit for the same class template shall not have equivalent
// parameter-declaration-clauses.

template<typename> struct S { };
template<typename> struct X { };

S() -> S<int>; // { dg-message "previously defined here|old declaration" }
S() -> S<int>; // { dg-error "redefinition" }
X() -> X<int>;
S() -> S<float>; // { dg-error "ambiguating new declaration of" }

S(bool) -> S<int>; // { dg-message "previously defined here" }
explicit S(bool) -> S<int>; // { dg-error "redefinition" }

explicit S(char) -> S<int>; // { dg-message "previously defined here" }
S(char) -> S<int>; // { dg-error "redefinition" }

template<typename T> S(T, T) -> S<int>; // { dg-message "previously declared here" }
template<typename T> X(T, T) -> X<int>;
template<typename T> S(T, T) -> S<int>; // { dg-error "redefinition" }

// OK: Use SFINAE.
template<typename T> S(T) -> S<typename T::foo>;
template<typename T> S(T) -> S<typename T::bar>;

// OK: Non-template wins.
S(int) -> S<int>;
template<typename T = int> S(int) -> S<char>;
