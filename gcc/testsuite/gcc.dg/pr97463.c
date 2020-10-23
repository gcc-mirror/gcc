/* PR c/97463 - ICE in warn_parm_ptrarray_mismatch on an incompatible
   function redeclaration
   { dg-do compile }
   { dg-options "-Wall" } */

void f (void**);
void f (int n) { }      // { dg-error "conflicting types" }
