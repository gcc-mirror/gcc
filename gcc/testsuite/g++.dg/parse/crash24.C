// PR c++/20461
// { dg-do compile }

class C; // { dg-error "forward declaration" }

C::C() : f() {} // { dg-error "invalid use|does not have" }
