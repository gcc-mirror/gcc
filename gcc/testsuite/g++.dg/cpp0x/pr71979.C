// PR c++/71979
// { dg-do compile { target c++11 } }

struct A
{ 
  A & operator= (A &);
};

struct B : A {};   // { dg-error "invalid initialization" }

void foo ()
{ 
  B b;
  b = B ();  // { dg-error "use of deleted" }
}
