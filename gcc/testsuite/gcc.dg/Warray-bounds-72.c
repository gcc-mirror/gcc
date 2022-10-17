/* Verify that -Warray-bounds suppression via #pragma GCC diagnostic
   works at any call site in an inlining stack
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define IGNORE '2'
#include "Warray-bounds-71.h"
