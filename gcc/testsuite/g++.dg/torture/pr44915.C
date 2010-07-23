/* { dg-do compile }  */
/* { dg-options "-findirect-inlining" } */

struct A;

typedef void (A::*f_ptr) ();

void dummy (f_ptr) { }

void call_dummy (f_ptr cb)
{
  dummy (cb);
}
