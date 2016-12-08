// PR c++/22508
// ICE on invalid operator new
// Origin: Flash Sheridan  <flash@pobox.com>
// { dg-do compile }

struct A
{
  void* operator new(__SIZE_TYPE__) throw(X);  // { dg-error "expected|type" }
};					       // { dg-error "dynamic exception specification" "" { target c++1z } .-1 }
					       // { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } .-2 }
A* p = new A;
