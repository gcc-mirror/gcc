// PR 12486

struct A { int ma; }; 
struct B { }; 
 
void foo() 
{ 
  B *b; 
  b->A::ma=0; // { dg-error "" }
} 
