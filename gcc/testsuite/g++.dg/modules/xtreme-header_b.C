// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for any in xtreme-header.h" { ! hostedlib } }

#include "xtreme-header.h"
import "xtreme-header_a.H";
