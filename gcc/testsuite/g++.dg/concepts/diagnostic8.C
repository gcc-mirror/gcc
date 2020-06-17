// PR c++/94306
// { dg-do compile { target c++2a } }

template<typename T> struct S { };
template<typename T> requires { typename T::type; } struct S<T> { };
// { dg-error "missing additional .requires." "" { target *-*-* } .-1 }
