// PR c++/22508
// ICE on invalid operator new
// Origin: Flash Sheridan  <flash@pobox.com>
// { dg-do compile }

struct A
{
  void* operator new(__SIZE_TYPE__) throw(X);  // { dg-error "" }
};

A* p = new A;                                  // { dg-error "no suitable" }
