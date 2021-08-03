// PR c++/96636
// { dg-options "" }

typedef class {
  class a {};
  class : virtual a {};		// { dg-error "anonymous struct with base" }
} b;
void foo(){ b();}

