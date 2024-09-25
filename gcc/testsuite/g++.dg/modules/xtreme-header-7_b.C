// A version of xtreme-header_b.C that doesn't use -fno-module-lazy.
// { dg-additional-options -fmodules-ts }
// { dg-skip-if "requires hosted libstdc++ for any in xtreme-header.h" { ! hostedlib } }

#include "xtreme-header.h"
import "xtreme-header-7_a.H";
