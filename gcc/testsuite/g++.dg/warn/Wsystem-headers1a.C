// { dg-do compile { target *-*-*gnu } }
// { dg-options "-Wsystem-headers" }
// { dg-skip-if "requires hosted libstdc++ for stdlib atof" { ! hostedlib } }

#include <stdlib.h>

extern double atof (const char *); // { dg-warning "different exception spec" }
