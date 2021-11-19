// PR c++/56871
// { dg-do compile { target c++11 } }

template<typename T> constexpr int foo(T);
template<> int foo(int);
template<> int foo(int);            // { dg-message "previous declaration 'int foo" "" { target { ! implicit_constexpr } } }
template<> constexpr int foo(int);  // { dg-error "redeclaration 'constexpr int foo" "" { target { ! implicit_constexpr } } }

template<typename T> int bar(T);
template<> constexpr int bar(int);
template<> constexpr int bar(int);  // { dg-message "previous declaration 'constexpr int bar" "" { target { ! implicit_constexpr } } }
template<> int bar(int);            // { dg-error "redeclaration 'int bar" "" { target { ! implicit_constexpr } } }
