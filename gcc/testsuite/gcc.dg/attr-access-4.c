/* PR middle-end/97861 - ICE on an invalid redeclaration of a function
   with attribute access
   { dg-do compile }
   { dg-options "-Wall" } */

__attribute__ ((access (read_only, 2)))
void f (int, int*);
void f (int a) { }  // { dg-error "conflicting types for 'f'" }
