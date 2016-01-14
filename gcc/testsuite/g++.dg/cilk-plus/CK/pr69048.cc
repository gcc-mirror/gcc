/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

struct A {
  ~A () {}
};

A f () {
  return A ();
}

void t1 () {
  _Cilk_spawn f ();
}
