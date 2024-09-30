/* { dg-additional-options "-fanalyzer-show-events-in-system-headers" } */
/* { dg-skip-if "no shared_ptr in C++98" { c++98_only }  } */
/* { dg-skip-if "requires hosted libstdc++ for memory shared_ptr" { ! hostedlib } } */

#include <memory>

struct A {int x; int y;};

int main () { /* { dg-message "\\(1\\) entry to 'main'" "telltale event that we are going within a deeper frame than 'main'" } */
  std::shared_ptr<A> a;
  a->x = 4; /* { dg-line deref_a } */ 
  /* { dg-warning "dereference of NULL" "" { target *-*-* } deref_a } */

  return 0;
}
