// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for barrier in xtreme-header-6.h" { ! hostedlib } }

#include "xtreme-header-6.h"
import "xtreme-header-6_a.H";
