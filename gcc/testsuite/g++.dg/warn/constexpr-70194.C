// PR c++/70194
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

int i;

const bool b0 = &i == 0; // { dg-warning "the address of .i. will never be NULL" }
constexpr int *p = &i;
const bool b1 = p == 0; // { dg-warning "the address of .i. will never be NULL" }
const bool b2 = 0 == p; // { dg-warning "the address of .i. will never be NULL" }
const bool b3 = p != 0; // { dg-warning "the address of .i. will never be NULL" }
const bool b4 = 0 != p; // { dg-warning "the address of .i. will never be NULL" }
