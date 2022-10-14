// PR c++/106304
// { dg-additional-options -fmodules-ts }

module pr106304;

void f(A& a) {
  as_b(a);
}
