/* PR 97955 - ICE in build_array_type_1 on invalid redeclaration of function
   with VLA parameter
   { dg-do compile }
   { dg-options "-Wall" } */

void f (int n, int a[n]);
void f (int *b) { }           // { dg-error "conflicting types" }
