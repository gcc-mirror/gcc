/* { dg-skip-if "no shared_ptr in C++98" { c++98_only }  } */

#include <memory>

struct A {int x; int y;};

int main () {
  std::shared_ptr<A> a; /* { dg-line declare_a } */
  a->x = 4; /* { dg-line deref_a } */ 
  /* { dg-warning "dereference of NULL" "" { target *-*-* } deref_a } */

  return 0;
}

/* { dg-note "\\(1\\) 'a\\.std::.+::_M_ptr' is NULL" "" { target c++14_down } declare_a } */
/* { dg-note "dereference of NULL 'a\\.std::.+::operator->\\(\\)'" "" { target *-*-* } deref_a } */
/* { dg-note "calling 'std::.+::operator->' from 'main'" "" { target *-*-* } deref_a } */
/* { dg-note "returning to 'main' from 'std::.+::operator->'" "" { target *-*-* } deref_a } */
