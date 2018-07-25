/* { dg-require-effective-target vect_int } */

#define SIGNEDNESS signed
#define BIAS 2

#include "vect-avg-5.c"

/* { dg-final { scan-tree-dump-not "vect_recog_average_pattern: detected" "vect" } } */
