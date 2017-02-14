/* { dg-do compile } */
/* { dg-options "-W -fdiagnostics-show-caret" } */

double val;

_Atomic double val0() {return val;} /* { dg-warning "qualifiers ignored" } */
/* { dg-begin-multiline-output "" }
 _Atomic double val0() {return val;}
 ^~~~~~~
{ dg-end-multiline-output "" } */

