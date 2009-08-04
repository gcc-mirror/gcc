// PR 16696 Strange message when operator++ not found
// { dg-do compile } 
// { dg-options "-fdiagnostics-show-option" } 


struct X { void operator++(); }; 
struct Y { };

int main () { 
  X x; 
  Y y;
  x++; // { dg-bogus "trying prefix operator" } 
  // { dg-error "fpermissive" "" { target *-*-* } 12 }
  y++; // { dg-bogus "trying prefix operator" } 
  // { dg-error "fpermissive" "" { target *-*-* } 14 }
} 

