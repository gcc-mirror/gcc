// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// { dg-skip-if "requires hosted libstdc++ for tr1/functional in xtreme-tr1.h" { ! hostedlib } }

#include "xtreme-tr1.h"
import "xtreme-tr1_a.H";
