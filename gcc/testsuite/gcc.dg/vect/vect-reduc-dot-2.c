/* { dg-require-effective-target vect_int } */

#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 signed

#include "vect-reduc-dot-1.c"

/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */

