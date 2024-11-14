/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=gcs" } */
/* { dg-final { scan-assembler-times "hint\\t40 // chkfeat x16" 2 } } */
/* { dg-final { scan-assembler-times "mrs\\tx\[0-9\]+, s3_3_c2_c5_1 // gcspr_el0" 2 } } */
/* { dg-final { scan-assembler-times "sysl\\txzr, #3, c7, c7, #1 // gcspopm" 1 } } */

#include "gcs-nonlocal-2.h"
