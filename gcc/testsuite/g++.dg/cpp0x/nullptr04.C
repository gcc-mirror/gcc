// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test cast to int

const int n4 = static_cast<const int>(nullptr); // { dg-error "invalid static_cast " }
const short int n5 = reinterpret_cast<short int>(nullptr); // { dg-error "loses precision" }
const long int n6 = reinterpret_cast<long int>(nullptr);
const long int n7 = (long int)nullptr;
