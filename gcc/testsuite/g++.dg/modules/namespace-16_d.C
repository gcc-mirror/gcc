// PR c++/122915
// { dg-additional-options "-fmodules" }

module tests;

namespace ns { using T = int; };
T x = 123;

namespace abc { using U = double; };
part::U y = 3.14;
