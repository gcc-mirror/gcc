// { dg-additional-options "-fmodules" }

import M;

auto imported_c = make_c ();
using C = decltype (imported_c);
enum C::E : int { e };

int after;
