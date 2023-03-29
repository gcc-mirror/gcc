// { dg-additional-options {-std=c++20 -fmodules-ts} }

import "100616_a.H";
import pr100616_b;
import pr100616_c;

C<A{}> c0;
using type = decltype(c0);
using type = decltype(c1);
using type = decltype(c2); // bogus error: types of c1 and c2 don't match
