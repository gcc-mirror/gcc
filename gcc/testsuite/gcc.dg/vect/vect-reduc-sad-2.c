/* { dg-require-effective-target vect_int } */

#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 signed

#include "vect-reduc-sad-1.c"

/* { dg-final { scan-tree-dump "vect_recog_sad_pattern: detected" "vect" } } */

