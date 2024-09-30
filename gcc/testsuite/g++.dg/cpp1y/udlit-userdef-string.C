// { dg-do compile { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for complex in complex_literals.h" { ! hostedlib } }

#include "complex_literals.h"

auto cx = 1.1if;

auto cn = 123if;
