// PR c++/99678
// { dg-do compile { target c++20 } }

template<class T>
void f1() requires undeclared_identifier; // { dg-error "not declared" }

template<class T>
void f2() requires true && undeclared_identifier; // { dg-error "not declared" }

template<class T>
void f3() requires false || undeclared_identifier; // { dg-error "not declared" }

template<class T>
void f4() requires undeclared_identifier(T{}); // { dg-error "must be enclosed in parentheses" }
