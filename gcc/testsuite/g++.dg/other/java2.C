// PR c++/???
// { dg-do compile }

extern "Java"
{
  struct A {};
}

typedef void* jclass;

A* p = new A;  // { dg-error "class\\$" }
