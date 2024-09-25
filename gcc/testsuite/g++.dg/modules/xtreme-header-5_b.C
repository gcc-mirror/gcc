// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for any in xtreme-header-5.h" { ! hostedlib } }

#include "xtreme-header-5.h"
import "xtreme-header-5_a.H";
