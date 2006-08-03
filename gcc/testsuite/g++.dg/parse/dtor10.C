// PR c++/27508
// { dg-do compile }

namespace N
{
    struct A { ~A(); };
}

N::~A () {}  // { dg-error "not a class-name" }
