// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for deque in xtreme-header-3.h" { ! hostedlib } }

#include "xtreme-header-3.h"
import "xtreme-header-3_a.H";
