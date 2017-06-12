// PR 16696 Strange message when operator++ not found
// { dg-do compile } 
// { dg-options "-fdiagnostics-show-option -fpermissive" } 


struct X { void operator++(); }; 
struct Y { };

int main () { 
  X x; 
  Y y;
  x++; // { dg-warning "trying prefix operator" } 

  y++; // { dg-warning "trying prefix operator" } 
  // { dg-error "no match" "" { target *-*-* } .-1 }
} 

