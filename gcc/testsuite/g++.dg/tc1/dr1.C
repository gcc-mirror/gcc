// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR1: What if two using-declarations refer to the same function but the 
//  declarations introduce different default-arguments? 

namespace A { 
  extern "C" void f(int = 5); 
} 
namespace B { 
  extern "C" void f(int = 5); 
} 
using A::f; 
using B::f; 

void use() { 
  f(3);       
  f();        // { dg-error "" "" { xfail *-*-* } }
} 
