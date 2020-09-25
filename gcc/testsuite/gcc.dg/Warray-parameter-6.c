/* PR c/97131 - ICE: Segmentation fault in warn_parm_ptrarray_mismatch
   { dg-do compile }
   { dg-options "-Wall" } */

struct bm { };

void ms (struct bm (*at)[1]) { }

void ms (int f1) { }          // { dg-error "conflicting types for 'ms'" }
