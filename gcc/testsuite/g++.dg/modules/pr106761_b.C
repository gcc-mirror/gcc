// PR c++/106761
// { dg-additional-options -fmodules-ts }

#include "pr106761.h"
import "pr106761_a.H";

tuple<int, int> u = t;
