// { dg-additional-options "-fmodules" }
// { dg-module-cmi Y }

module;
#include "pr114630.h"
export module Y;
import X;
formatter<int> b;
