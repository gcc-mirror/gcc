// PR c++/96636
// { dg-options "-Wno-non-c-typedef-for-linkage" }

typedef class {
  class a {};
  class : virtual a {};		// { dg-error "anonymous struct with base" }
} b;
void foo(){ b();}

