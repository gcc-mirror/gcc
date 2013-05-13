// PR c++/56871
// { dg-options "-std=c++11" }

template<typename T> constexpr int foo(T);
template<> int foo(int);
template<> int foo(int);            // { dg-error "previous" }
template<> constexpr int foo(int);  // { dg-error "redeclaration" }

template<typename T> int bar(T);
template<> constexpr int bar(int);
template<> constexpr int bar(int);  // { dg-error "previous" }
template<> int bar(int);            // { dg-error "redeclaration" }
