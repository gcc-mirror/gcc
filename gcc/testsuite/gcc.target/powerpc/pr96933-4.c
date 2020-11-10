/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

/* Test vector constructions with char/short run successfully on Power9.  */

#include <stdlib.h>
#include "pr96933.h"
#include "pr96933-run.h"

