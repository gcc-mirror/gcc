// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for cassert in xtreme-header-1.h" { ! hostedlib } }

#include "xtreme-header-1.h"
import "xtreme-header-1_a.H";
