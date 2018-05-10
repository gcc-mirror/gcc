// CWG 2267
// { dg-do compile { target c++11 } }

struct A {} a; 
struct B { explicit B(const A&); }; 
B b1(a); // #1, ok 
const B &b2{a}; // { dg-error "" }
const B &b3(a); // { dg-error "" }

struct D { D(); }; 
struct C { explicit operator D(); } c; 
D d1(c); // ok 
const D &d2{c}; // { dg-error "" }
const D &d3(c); // { dg-error "" }
