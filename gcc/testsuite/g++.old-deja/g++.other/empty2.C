// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct E {};

void f () {
  E e1, e2;
  e1 = e2; // We should not warn about this statement, even though no
           // code is generated for it.
}
