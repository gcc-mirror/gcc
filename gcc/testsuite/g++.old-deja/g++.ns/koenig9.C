// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Theodore.Papadopoulo 23 Jun 2000 <Theodore.Papadopoulo@sophia.inria.fr>

#include <algorithm>

void foo(const char*,...);

inline void
bar() {
  foo("",count);    //  ERROR - multiple overloaded count functions
}
