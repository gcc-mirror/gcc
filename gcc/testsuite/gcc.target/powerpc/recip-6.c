/* { dg-do run { target { powerpc*-*-linux* powerpc*-ibm-aix* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mdejagnu-cpu=power7 -O3 -ftree-vectorize -ffast-math -mrecip=all -mrecip-precision" } */

/* Check reciprocal estimate functions for accuracy.  */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <float.h>
#include <string.h>

#include "recip-test.h"
