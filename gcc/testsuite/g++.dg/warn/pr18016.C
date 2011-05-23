/* { dg-do compile } */
/* { dg-options "-Wuninitialized -Winit-self" } */

class X {
  int i;
  X() : i(i) { }   // { dg-warning "initialized with itself" }
  X(int i) : i(i) { }
  X(const X& x) : i(x.i) { }
};

// { dg-prune-output "In constructor" }
