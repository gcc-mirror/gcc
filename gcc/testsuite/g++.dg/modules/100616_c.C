// { dg-additional-options {-std=c++20 -fmodules-ts} }

export module pr100616_c;
// { dg-module-cmi pr100616_c }

import "100616_a.H";
export C<A{}> c2;
