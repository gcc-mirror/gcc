// PR c++/96462
// { dg-do compile { target c++11 } }

enum E {};
using E::~E; // { dg-error "names destructor" }
