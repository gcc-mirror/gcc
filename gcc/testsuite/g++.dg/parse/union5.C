// PR c++/93809
// { dg-do compile { target c++11 } }

union U {};
auto var = new (typename ::U);
