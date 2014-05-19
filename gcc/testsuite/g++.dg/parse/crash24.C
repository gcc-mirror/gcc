// PR c++/20461
// { dg-do compile }

class C; // { dg-message "forward declaration" }

C::C() : f() {} // { dg-error "invalid use|does not have" }
