// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for charconv in xtreme-header-4.h" { ! hostedlib } }

#include "xtreme-header-4.h"
import "xtreme-header-4_a.H";
