// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test cast to int

#define unsigned
__extension__ typedef __SIZE_TYPE__ ssize_t;
#undef unsigned

const int n4 = static_cast<const int>(nullptr); // { dg-error "invalid static_cast " }
const short int n5 = reinterpret_cast<short int>(nullptr); // { dg-error "loses precision" }
const ssize_t n6 = reinterpret_cast<ssize_t>(nullptr);
const ssize_t n7 = (ssize_t)nullptr;
