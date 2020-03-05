// PR c++/90884
// { dg-options "-Wctor-dtor-privacy" }
// { dg-prune-output "In file included from" }

#include "ctor-dtor-privacy-4.h"  // { dg-bogus "is public" }
