// PR c++/16301

// { dg-do compile }

namespace NS2 
{ 
  using namespace NS1 __attribute__ ((strong));  // { dg-error "" }
}
