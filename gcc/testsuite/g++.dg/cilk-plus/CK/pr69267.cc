/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

struct A {};

void f (A) {}

void g () {
  _Cilk_spawn f (A ());
}
