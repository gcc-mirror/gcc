// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for execution in xtreme-header-2.h" { ! hostedlib } }

#include "xtreme-header-2.h"
import "xtreme-header-2_a.H";
