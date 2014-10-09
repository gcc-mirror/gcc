// PR c++/34938

template <class T> struct A { };
struct B { };

A<void()const>* p1 = 42;           // { dg-error "void\\(\\) const" }
A<void(B::*)()const>* p2 = 42;     // { dg-error "void \\(B::\\*\\)\\(\\) const" }

A<void()volatile>* p3 = 42;        // { dg-error "void\\(\\) volatile" }
A<void(B::*)()volatile>* p4 = 42;  // { dg-error "void \\(B::\\*\\)\\(\\) volatile" }
