// PR c++/121724
// { dg-additional-options "-fmodules" }

import foo;
using T = A::B::S;
using T = A::X::B::S;
