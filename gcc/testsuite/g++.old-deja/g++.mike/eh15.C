// { dg-do assemble { target c++14_down } }
// { dg-options "-fexceptions" }

struct A {
  A() throw (int);	// { dg-warning "deprecated" "" { target c++11 } }
};
