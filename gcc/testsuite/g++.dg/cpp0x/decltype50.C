// PR c++/52597
// { dg-require-effective-target c++11 }

struct A {
   int zip();

   decltype(zip) bar0; // { dg-error "invalid use of non-static member function" }
   void bar1() {
     typedef decltype(this->A::zip) x; // { dg-error "invalid use of non-static member function" }
   }
   void bar2() {
     typedef decltype(A::zip) x; // { dg-error "invalid use of non-static member function" }
   }
};

typedef decltype(A().zip) x; // { dg-error "invalid use of non-static member function" }

// { dg-prune-output "invalid type in declaration" }
