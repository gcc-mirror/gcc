// PR c++/127207
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }

import "friend-15_a.H";

Foo<int> x = createFoo<int>();
