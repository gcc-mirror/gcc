// { dg-do compile { target *-*-*gnu } }
// { dg-options "-Wsystem-headers" }

#include <stdlib.h>

extern double atof (const char *); // { dg-warning "different exception spec" }
