// { dg-do assemble  }

struct A { A() { a = 1; } int a; }; // { dg-message "" } 
struct A { A() { a = 2; } int a; }; // { dg-error "" } 
A aavv;
