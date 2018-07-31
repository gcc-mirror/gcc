/* { dg-require-effective-target vect_int } */

#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 signed
#define SIGNEDNESS_3 unsigned

#include "vect-reduc-dot-1.c"

/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */

