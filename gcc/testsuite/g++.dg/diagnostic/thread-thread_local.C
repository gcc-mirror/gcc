// { dg-options "-fdiagnostics-show-caret" }
// { dg-do compile { target c++11 } }

thread_local __thread int a;  // { dg-error "14:both .__thread. and .thread_local. specified" }
/* { dg-begin-multiline-output "" }
 thread_local __thread int a;
 ~~~~~~~~~~~~ ^~~~~~~~
   { dg-end-multiline-output "" } */
__thread thread_local int b;  // { dg-error "10:both .__thread. and .thread_local. specified" }
/* { dg-begin-multiline-output "" }
 __thread thread_local int b;
 ~~~~~~~~ ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */
