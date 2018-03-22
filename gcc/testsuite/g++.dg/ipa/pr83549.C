// PR ipa/83549
// { dg-do compile }
// { dg-options "-O2" }

struct A { virtual ~A (); };
struct B { virtual void foo (...); };
struct C : A, B { void foo (...) {} };
C c;
