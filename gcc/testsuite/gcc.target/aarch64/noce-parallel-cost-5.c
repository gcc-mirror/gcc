/* { dg-do compile } */
/* { dg-options "-Oz -mcpu=olympus -fdump-rtl-ce1" } */

#include "noce-parallel-cost-1.c"

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
