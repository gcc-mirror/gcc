/* Test that visibility of function parameters propagates to an undecorated
   function. */
/* { dg-require-visibility "" }
/* { dg-final { scan-hidden "_Z3fooP8a_struct" } } */

struct __attribute__((visibility("hidden"))) a_struct;

void foo(a_struct * p) 
{ }
