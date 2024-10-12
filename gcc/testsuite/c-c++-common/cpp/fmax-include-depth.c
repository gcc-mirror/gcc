/* { dg-do preprocess } */
/* { dg-options "-fmax-include-depth=1" } */

#include "fmax-include-depth-1b.h" /* { dg-error "'#include' nested depth 1 exceeds maximum of 1 \\\(use '-fmax-include-depth=DEPTH' to increase the maximum\\\)" } */
