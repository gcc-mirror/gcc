/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* Test vector constructions with char/short run successfully on Power8.  */

#include <stdlib.h>
#include "pr96933.h"
#include "pr96933-run.h"

