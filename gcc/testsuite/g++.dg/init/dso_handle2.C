// PR c++/58846

extern "C" { char* __dso_handle; }

struct A
{
  ~A();
};

A a;  // { dg-error "conflicting declaration" }
